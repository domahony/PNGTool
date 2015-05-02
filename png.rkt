#lang racket

(require (planet soegaard/gzip:2:2))
(require "png_types.rkt")

(define (main)

  (define argv (current-command-line-arguments))
  (define in (open-input-file (vector-ref argv 0) #:mode 'binary))

  (define outpath (vector-ref argv 1))

  (PNG:verify-signature in)

  (define header (PNG:read-chunk in))

  (define png (new PNG:png% [ihdr (PNG:parse-ihdr header)]))

  (define compressed-buf (bytes-append))

  (define (process-chunks chunk)
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
	['#"IDAT" (set! compressed-buf 
		    (bytes-append compressed-buf (PNG:chunk-data chunk))) #t] 
	['#"IEND" #f] 
	[else (printf "~a\n" (PNG:chunk-type chunk)) #t]))

      (if cont
	(process-chunks (PNG:read-chunk in)) 
	#f))

  (process-chunks (PNG:read-chunk in))

  (define uncompressed-buf 
    (uncompress-bytes compressed-buf (send png raw-size)))

  (send png set-data uncompressed-buf)
  (send png decode outpath)

  (printf "Out ~a\n" outpath))

(main)

