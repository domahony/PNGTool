#lang racket

(require "png_filter.rkt")

(provide 
  (combine-out 
    (prefix-out PNG:
		bstr->int)
    (prefix-out PNG:
		verify-signature)

    (prefix-out PNG:
		(struct-out chunk))
    (prefix-out PNG:
		read-chunk)

    (prefix-out PNG:
		(struct-out ihdr))
    (prefix-out PNG:
		(struct-out text))
    (prefix-out PNG:
		parse-ihdr)

    (prefix-out PNG:
		png%)))

(define signature (bytes 137 80 78 71 13 10 26 10))

(define (verify-signature in)
	(define insig (read-bytes (bytes-length signature) in))

	(if (not (bytes=? insig signature))
	  (error 'verify-signature 
		 "Expected ~s\nRecieved ~s\n" signature insig)
	  #t))

(struct chunk (
		   size
		   type
		   data
		   crc))

(define (read-chunk in)
  (define len (bstr->int (read-bytes 4 in)))
  (define type (read-bytes 4 in))
  (define data (read-bytes len in))
  (define crc (bstr->int (read-bytes 4 in)))
  (chunk len type data crc))

(struct ihdr (
		  width
		  height
		  bit_depth
		  color_type
		  compression_method
		  filter_method
		  interlace_method))

(define (parse-ihdr chunk)
  (define data (chunk-data chunk))
  (ihdr
    (bstr->int (subbytes data 0 4)) 
    (bstr->int (subbytes data 4 8)) 
    (bytes-ref data 8) 
    (bytes-ref data 9) 
    (bytes-ref data 10) 
    (bytes-ref data 11) 
    (bytes-ref data 12)))

(struct gama (
	      gamma))

(define (parse-gama chunk)
  (define data (chunk-data chunk))
  (gama (bstr->int (subbytes data 0 4))))

(struct srgb (
	     rendering_intent))

(define (parse-srgb chunk)
  (srgb (chunk-data chunk)))

(struct chrm (
	     white_point_x white_point_y
	     red_x red_y
	     green_x green_y
	     blue_x blue_y))

(struct time (
	      time))

(define (parse-time chunk)
  (printf "ZZZZ TIME ~a\n" (chunk-data chunk)) 
  (time (chunk-data chunk)))

(struct sbit (
	      sbit))

(define (parse-sbit chunk)
  (printf "ZZZZ SBIT ~a\n" (chunk-data chunk)) 
  (sbit (chunk-data chunk)))

(define (parse-chrm chunk)

  (define data (chunk-data chunk))

  (define s 0)
  (define e (+ s 4))
  (define wpx (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define wpy (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define rx (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define ry (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define gx (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define gy (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define bx (bstr->int (subbytes data s e))) 

  (set! s e)
  (set! e (+ s 4))
  (define by (bstr->int (subbytes data s e))) 

  (chrm 
	 wpx wpy
	 rx ry
	 gx gy
	 bx by))

(struct bkgd (
	      greyscale 
	      red green blue 
	      palette_index))

(define (parse-bkgd chunk h)
  (define data (chunk-data chunk))
  (define color_type (ihdr-color_type h))
  (case color_type
	  ['0 (bkgd (bstr->short (subbytes data 0 2)) 0 0 0 0)]
	  ['4 (bkgd (bstr->short (subbytes data 0 2)) 0 0 0 0)]
	  ['2 (bkgd 
		0
		(bstr->short (subbytes data 0 2)) 
		(bstr->short (subbytes data 2 4)) 
		(bstr->short (subbytes data 4 6)) 
		0)]
	  ['6 (bkgd 
		0
		(bstr->short (subbytes data 0 2)) 
		(bstr->short (subbytes data 2 4)) 
		(bstr->short (subbytes data 4 6)) 
		0)]
	  ['3 (bkgd 0 0 0 0 (subbytes data 0 1))]))

;
; Physical Pixel Size (pHYs)
;
(struct phys (
	      ppu_x
	      ppu_y
	      unit))

(define (parse-phys chunk) 
  (define data (chunk-data chunk))
  (phys 
    (bstr->int (subbytes data 0 4))
    (bstr->int (subbytes data 4 8))
    (bytes-ref data 8)))

;
; Text
;
(struct text (
	      keyword
	      text_string
	      ))

(define (parse-text chunk)
  (define data (chunk-data chunk))
  (define nullidx 0)

  (for ([i (in-range 0 79)]
       #:break (= (bytes-ref data i) 0))
       (set! nullidx i))
  (text (subbytes data 0 (+ nullidx 1)) (subbytes data (+ nullidx 2))))

;
; Main PNG object
;
(define png%
  (class object%
	 (super-new)

	 (init-field ihdr)

	 (field [plte (void)])

	 (field [trns (void)]) 

	 (field [chrm (void)]) (field [gama (void)])
	 (field [iccp (void)]) (field [sbit (void)])
	 (field [srgb (void)]) 

	 (field [bkgd (void)]) (field [hIST (void)])
	 (field [phys (void)]) (field [splt '()])

	 (field [time (void)]) 
	 (field [data (void)]) 

	 (field [itxt '()])
	 (field [text '()])
	 (field [ztxt '()])
	 (field [scanlines '()])

	 (define/public (add-splt p)
			(set! splt (append splt (list p))))
	 (define/public (add-itxt p)
			(set! itxt (append itxt (list p))))
	 (define/public (add-text p)
			(set! text (append text (list (parse-text p)))))
	 (define/public (add-ztxt p)
			(set! ztxt (append ztxt (list p)))) 
	 (define/public (set-gama c) 
			(set! gama (parse-gama c)))
	 (define/public (set-srgb c) 
			(set! srgb (parse-srgb c)))
	 (define/public (set-chrm c) 
			(set! chrm (parse-chrm c)))
	 (define/public (set-time c) 
			(set! time (parse-time c)))
	 (define/public (set-bkgd c) 
			(set! bkgd (parse-bkgd c ihdr)))
	 (define/public (set-phys c) 
			(set! phys (parse-phys c)))
	 (define/public (set-sbit c) 
			(set! sbit (parse-sbit c)))
	 (define/public (set-data buf) 
			(define w 
			  (+ 1 (* (bytes-per-pixel) (ihdr-width ihdr))))

			(for ([i (in-range 0 (ihdr-height ihdr))]) 
			      (define start (* i w)) 
			      (set! scanlines (append scanlines 
				      (list (PNG:scan 
					      (bytes-ref buf start) 
					      (subbytes buf 
							(+ 1 start) 
							(+ start w)))))))) 
	 (define/public (decode f) 
			(define out (open-output-file f
					  #:mode 'binary
					  #:exists 'replace))

			(define bytespp (bytes-per-pixel))
			(define prev void) 

			(for ([s scanlines])

			  (PNG:unfilter bytespp prev s)
			  ;(scale-out out 16 7 s)
			  (fprintf out "~a" (PNG:scan-data s))
			  (set! prev s))

			(close-output-port out))

	 (define/public (raw-size)
			(define w (ihdr-width ihdr)) 
			(define h (ihdr-height ihdr))
			(define bd (ihdr-bit_depth ihdr))
			(define samples 0)
			(define bpp 
			  (case (ihdr-color_type ihdr) 
			    ['0 (set! samples 1) (* bd samples)]
			    ['2 (set! samples 3) (* bd samples)]
			    ['3 (set! samples 1) (*  8 samples)]
			    ['4 (set! samples 2) (* bd samples)]
			    ['6 (set! samples 4) (* bd samples)]))
			(printf "h: ~s w: ~s bpp: ~s samples: ~s total: ~s\n" 
				w h bpp samples (* w h (/ bpp 8))) 
			(/ (* (+ w 1) h bpp) 8))

	 (define (bytes-per-pixel)
	   (define bd (ihdr-bit_depth ihdr)) 
	   (define samples 0) 
	   (define bpp 
	     (case (ihdr-color_type ihdr) 
	       ['0 (set! samples 1) (* bd samples)] 
	       ['2 (set! samples 3) (* bd samples)] 
	       ['3 (set! samples 1) (*  8 samples)] 
	       ['4 (set! samples 2) (* bd samples)] 
	       ['6 (set! samples 4) (* bd samples)]))
	   (/ bpp 8))
	 ))

(define (bstr->int bstr)
  (define b0 (bytes-ref bstr 3))
  (define b1 (bytes-ref bstr 2))
  (define b2 (bytes-ref bstr 1))
  (define b3 (bytes-ref bstr 0))
  ( + ( * b0 (expt 2 0)) ( * b1 (expt 2 8)) ( * b2 (expt 2 16)) ( * b3 (expt 2 24))))

(define (bstr->short bstr)
  (define b0 (bytes-ref bstr 1))
  (define b1 (bytes-ref bstr 0))
  ( + ( * b0 (expt 2 0)) ( * b1 (expt 2 8)) ))

(define (scale s in out)
  (exact-floor (+ 0.5 (* (/ s (- (expt 2 in) 1)) (- (expt 2 out) 1)))))

(define (scale-out out is os s)
  (define data (PNG:scan-data s))
  (for ([i (in-range 0 (bytes-length data) 2)]) 
    (define b (scale (bstr->short (subbytes data i)) is os))
    (write-byte b out)))
