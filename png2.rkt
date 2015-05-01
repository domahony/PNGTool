#lang racket

(require (planet soegaard/gzip:2:2))
(require "png_types.rkt")

(define (main)

  (define argv (current-command-line-arguments))

  (define inpath (vector-ref argv 0))
  (define outpath (vector-ref argv 1))

  (define in (open-input-file inpath #:mode 'binary))

  (PNG:verify-signature in)

  (define header (PNG:read-chunk in))

  (define png (new PNG:png% [ihdr (PNG:parse-ihdr header)]))

  (define buf (bytes-append))
  (define (r chunk)
    (define cont 
      (case (PNG:chunk-type chunk) 
	['#"gAMA" (send png set-gama chunk) #t] 
	['#"sRGB" (send png set-srgb chunk) #t] 
	['#"cHRM" (send png set-chrm chunk) #t] 
	['#"bKGD" (send png set-bkgd chunk) #t] 
	['#"pHYs" (send png set-phys chunk) #t] 
	['#"tEXt" (send png add-text chunk) #t] 
	['#"tIME" (send png set-time chunk) #t]
	['#"sBIT" (send png set-sbit chunk) #t]
	['#"IDAT" (set! buf (bytes-append buf (PNG:chunk-data chunk))) #t] 
	['#"IEND" #f] 
	[else (printf "~a\n" (PNG:chunk-type chunk)) #t]))

      (if cont
	(r (PNG:read-chunk in)) 
	#f))

  (r (PNG:read-chunk in))


  (printf "In ~a\n" inpath)
  (printf "Out ~a\n" outpath))

(main)

