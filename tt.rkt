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

(define SIZE 255)
(define-z deflateInit_ (_fun _z_stream_s-pointer _int _string _int -> _int))
(define-z deflate (_fun _z_stream_s-pointer _int -> _int))
(define-z deflateEnd (_fun _z_stream_s-pointer -> _int))
(define-z inflateInit_ (_fun _z_stream_s-pointer _string _int -> _int))
(define-z inflate (_fun _z_stream_s-pointer _int -> _int))
(define-z inflateEnd (_fun _z_stream_s-pointer -> _int))

(define z (make-z_stream_s (make-bytes SIZE) 0 0 #f 0 0 #f 
			 #f #f #f #f 0 0 0))

;(deflateInit_ z 9 "1.2.8" (ctype-sizeof _z_stream_s))
(inflateInit_ z "1.2.8" (ctype-sizeof _z_stream_s))

(define out (make-cvector _byte SIZE))
(define infile (open-input-file "op" #:mode 'binary))
(define op (open-output-file "op-exploded" #:mode 'binary #:exists 'replace))

(define have 0)

(define (do_chunk data) 
  (if (not (eof-object? data)) 
    (begin
    (set-z_stream_s-next_in! z data) 
     (set-z_stream_s-avail_in! z (bytes-length data)) 
     (set-z_stream_s-avail_out! z SIZE) 
     (set-z_stream_s-next_out! z (cvector-ptr out)) 
     (deflate z 1) 
     (set! have (- SIZE (z_stream_s-avail_out z))) 
     (printf "Have: ~s\n" have) 
     (printf "~a\n" out)
     (fprintf op "~a" (subbytes (list->bytes (cvector->list out)) 0 have)) 
     (do_chunk (read-bytes SIZE infile)))
    #f))

;(do_chunk (read-bytes SIZE infile))

(define (process)
      (set-z_stream_s-avail_out! z SIZE)
      (set-z_stream_s-next_out! z (cvector-ptr out)) 
      (inflate z 0)
      (set! have (- SIZE (z_stream_s-avail_out z)))
      (printf "Have: ~s\n" have) 
      (fprintf op "~a" (subbytes (list->bytes (cvector->list out)) 0 have)) 
      (if (eq? have SIZE)
	(process)
	#f))

(define (do_inflate data)
  (if (not (eof-object? data))
    (begin
      (set-z_stream_s-next_in! z data)
      (set-z_stream_s-avail_in! z (bytes-length data))
      (process)
      (do_inflate (read-bytes SIZE infile)))
    #f))

(do_inflate (read-bytes SIZE infile))

;(deflateEnd z)
(inflateEnd z)

(close-output-port op)
(close-input-port infile)

;(define (inflate in out)
