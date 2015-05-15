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

(define out (make-cvector _byte 255))

(define data  #"David O'Mahony")

(set-z_stream_s-next_in! z data)
(set-z_stream_s-avail_in! z (bytes-length data))
(set-z_stream_s-avail_out! z 255)
(set-z_stream_s-next_out! z (cvector-ptr out))

(deflateInit_ z 9 "1.2.8" (ctype-sizeof _z_stream_s))
(deflate z 1)

(define have (- 255 (z_stream_s-avail_out z)))
(printf "~s\n" have) 
(define op (open-output-file "op" #:mode 'binary #:exists 'replace))
(fprintf op "~a" (subbytes (list->bytes (cvector->list out)) 0 have))

(close-output-port op)
