#lang racket

(require "png_types.rkt")

(define (main)

  (define argv (current-command-line-arguments))
  (define in (open-input-file (vector-ref argv 0) #:mode 'binary))

  (define outpath (vector-ref argv 1))

  (PNG:verify-signature in)

  (define header (PNG:read-chunk in))
  (define png (new PNG:png% 
		   [ihdr (PNG:parse-ihdr header)]
		   [output outpath]))

  (define (process-chunks chunk) 
    (if (send png add-chunk chunk) 
      (process-chunks (PNG:read-chunk in)) 
      #f))

  (process-chunks (PNG:read-chunk in))

  ;(send png decode outpath)

  (printf "Out ~a\n" outpath))

(main)

