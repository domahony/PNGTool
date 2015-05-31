#lang racket

(require ffi/unsafe 
	 ffi/unsafe/define)

(provide 
  (prefix-out PNG:
	      zlib_inflater%))

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

(define Z_FINISH 4)
(define Z_NO_FLUSH 0)
(define Z_OK 0)
(define Z_STREAM_END 1)
(define Z_STREAM_ERROR -2)
(define Z_DATA_ERROR -3)
(define Z_MEM_ERROR -4)
(define Z_BUF_ERROR -5)

(define SIZE (* 128 128))
(define-z deflateInit_ (_fun _z_stream_s-pointer _int _string _int -> _int))
(define-z deflate (_fun _z_stream_s-pointer _int -> _int))
(define-z deflateEnd (_fun _z_stream_s-pointer -> _int))
(define-z inflateInit_ (_fun _z_stream_s-pointer _string _int -> _int))
(define-z inflate (_fun _z_stream_s-pointer _int -> _int))
(define-z inflateEnd (_fun _z_stream_s-pointer -> _int))

(define zlib_inflater%
  (class object% 
	 (super-new)

	 (field [z (make-z_stream_s 
		     #f 0 0 #f 0 0 #f
		     #f #f #f #f 0 0 0)])

	 (field [out (malloc _byte 'raw SIZE)])
	 (field [input (void)]) 
	 (field [done #f])
	 (define (process ret) 
	   (define have 0) 
	   (set-z_stream_s-avail_out! z SIZE) 
	   (set-z_stream_s-next_out! z out) 

	   (case (inflate z 0) 
	     ['Z_OK void]
	     ['Z_STREAM_END (set! done #t) (inflateEnd z)]
	     ['Z_DATA_ERROR void]
	     ['Z_STREAM_ERROR void]
	     ['Z_MEM_ERROR void]
	     ['Z_BUF_ERROR void]
	     )

	   (set! have (- SIZE (z_stream_s-avail_out z))) 
	   (set! ret (bytes-append ret (make-sized-byte-string out have))) 
	   (if (eq? have SIZE) 
	     (process ret) 
	     ret))

	 (define/public (do_inflate data) 
			(set-z_stream_s-next_in! z data) 
			(set-z_stream_s-avail_in! z (bytes-length data))
			(define ret (bytes-append))
			(if done 
			  (eof) 
			  (process ret)))

	 (define/public (do_end)
			(if (not done) 
			  (inflateEnd z) 
			  (void)))

  	(inflateInit_ z "1.2.8" (ctype-sizeof _z_stream_s))))
