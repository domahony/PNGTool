#lang racket

(require ffi/unsafe 
	 ffi/unsafe/define)

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

(define SIZE (* 1024 1024))
(define-z deflateInit_ (_fun _z_stream_s-pointer _int _string _int -> _int))
(define-z deflate (_fun _z_stream_s-pointer _int -> _int))
(define-z deflateEnd (_fun _z_stream_s-pointer -> _int))
(define-z inflateInit_ (_fun _z_stream_s-pointer _string _int -> _int))
(define-z inflate (_fun _z_stream_s-pointer _int -> _int))
(define-z inflateEnd (_fun _z_stream_s-pointer -> _int))

;(define z (make-z_stream_s (make-bytes SIZE) 0 0 #f 0 0 #f 
(define z (make-z_stream_s #f 0 0 #f 0 0 #f 
			 #f #f #f #f 0 0 0))

(define out (malloc _byte 'raw SIZE)) 

(define (do_deflate iport oport) 
  (define data (read-bytes SIZE iport))
  (define have 0) 
  (if (not (eof-object? data)) 
    (begin
    (set-z_stream_s-next_in! z data) 
     (set-z_stream_s-avail_in! z (bytes-length data)) 
     (set-z_stream_s-avail_out! z SIZE) 
     (set-z_stream_s-next_out! z out)
     (deflate z 1) 
     (set! have (- SIZE (z_stream_s-avail_out z))) 
     (printf "Have: ~s\n" have) 
     (fprintf op "~a" (make-sized-byte-string out have)) 
     (do_deflate iport oport))
    #f))

(define (process oport) 
  (define have 0) 
  (set-z_stream_s-avail_out! z SIZE) 
  (set-z_stream_s-next_out! z out)
  (printf "Ret: ~s\n" (inflate z 0)) 
  (set! have (- SIZE (z_stream_s-avail_out z))) 
  (printf "Have: ~s\n" have) 
  (fprintf op "~a" (make-sized-byte-string out have)) 
  (if (eq? have SIZE) 
    (process oport)
    #f))

(define (do_inflate iport oport)
  (define data (read-bytes SIZE iport))
  (if (not (eof-object? data))
    (begin
      (set-z_stream_s-next_in! z data)
      (set-z_stream_s-avail_in! z (bytes-length data))
      (process oport)
      (do_inflate iport oport))
    #f))

(define (DEF iport oport) 
  (deflateInit_ z -1 "1.2.8" (ctype-sizeof _z_stream_s))
  (do_deflate iport oport)
  (deflateEnd z))

(define (INF iport oport)
  (inflateInit_ z "1.2.8" (ctype-sizeof _z_stream_s)) 
  (do_inflate iport oport)
  (inflateEnd z))

(define infile (open-input-file "op" #:mode 'binary))
(define op (open-output-file "op-exploded" #:mode 'binary #:exists 'replace))
(INF infile op)

;(define infile (open-input-file "blah.zero" #:mode 'binary))
;(define op (open-output-file "op" #:mode 'binary #:exists 'replace))
;(DEF infile op)

(close-output-port op)
(close-input-port infile)
(free out)

