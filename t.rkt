#lang racket

(require ffi/unsafe
	 ffi/unsafe/define
	 ffi/cvector)

(define-ffi-definer define-z (ffi-lib "libz"))

(define-cstruct _z_stream_s ([next_in _pointer]
			   [avail_in _uint]
			   [total_in _ulong]
			   [next_out _pointer]
			   [avail_out _uint]
			   [total_out _ulong]
			   [msg _bytes]
			   [state _pointer]
			   [zalloc _pointer]
			   [zfree _pointer]
			   [opaque _pointer]
			   [data_type _int]
			   [adler _ulong]
			   [reserved _ulong]))

(define-z deflateInit_ (_fun _z_stream_s-pointer _int _string _int -> _int))
(define-z deflate (_fun _z_stream_s-pointer _int -> _int))

(define z (make-z_stream_s (make-bytes 255) 0 0 #f 0 0 #f 
			 #f #f #f #f 0 0 0))

(deflateInit_ z 9 "1.2.8" (ctype-sizeof _z_stream_s))

(define out (make-cvector _byte 255))
(define infile (open-input-file "png_types.rkt" #:mode 'binary))
;(define infile (open-input-file "tt" #:mode 'binary))
(define op (open-output-file "op" #:mode 'binary #:exists 'replace))

(define have 0)

(define (do_chunk data) 
  (if (eq? data eof) 
    #f 
    (begin
    (set-z_stream_s-next_in! z data) 
     (set-z_stream_s-avail_in! z (bytes-length data)) 
     (set-z_stream_s-avail_out! z 255) 
     (set-z_stream_s-next_out! z (cvector-ptr out)) 
     (deflate z 1) 
     (set! have (- 255 (z_stream_s-avail_out z))) 
     (printf "~s\n" have) 
     (fprintf op "~a" (subbytes (list->bytes (cvector->list out)) 0 have)) 
     (do_chunk (read-bytes 255 infile)))))

(do_chunk (read-bytes 255 infile))

(close-output-port op)
(close-input-port infile)
