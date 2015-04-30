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

  (define png (new PNG:png% [ihdr header]))

  (define (r chunk) 
    (printf "~a\n" (PNG:chunk-type chunk))
    (r (PNG:read-chunk in)))

  (r (PNG:read-chunk in))


  (printf "In ~a\n" inpath)
  (printf "Out ~a\n" outpath))

(main)

