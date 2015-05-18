#lang racket

(require "png_zlib.rkt")

(define ZI (new PNG:zlib_inflater%))
(define INFILE (open-input-file "op2" #:mode 'binary))

(define (doit) 
  (define data (read-bytes 66 INFILE)) 
  (define ret void)

  (if (eq? data eof)
    (void)
    (begin 
      (set! ret (send ZI do_inflate data)) 
      (if (not (eq? eof ret)) 
	(begin 
	  (printf "~a" ret) 
	  (doit)) 
	(void)))))

(doit)
