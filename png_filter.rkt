#lang racket

(provide 
  (combine-out 

   (prefix-out PNG:
		(struct-out scan))))

(struct scan (
	      filter_method
	      data))
(provide
  (combine-out
    (prefix-out PNG:
		unfilter)))

(define (unfilter bpp prev cur)
  (case (scan-filter_method cur)
    ['0 (printf "Filter None\n") (unfilter-none bpp prev cur)]
    ['1 (printf "Filter Sub\n") (unfilter-sub bpp prev cur)]
    ['2 (printf "Filter Up\n") (unfilter-up bpp prev cur)]
    ['3 (printf "Filter Average\n") (unfilter-average bpp prev cur)]
    ['4 (printf "Filter Paeth\n") (unfilter-paeth bpp prev cur)]))

(define (unfilter-none bpp prev cur)
  (printf "None\n"))

(define (unfilter-sub bpp prev cur)

  (for ([p (in-range 0 (bytes-length (scan-data cur)))])

    (define a (get-a p bpp (scan-data cur)))
    (define x (bytes-ref (scan-data cur) p))

    (printf "~a ~a ~a ~a\n" p a x (modulo (+ x a) 256))
    (bytes-set! (scan-data cur) p (modulo (+ x a) 256)))) 

(define (unfilter-up bpp prev cur)

  (for ([p (in-range 0 (bytes-length (scan-data cur)))])
    (define b (get-b p (scan-data cur) (scan-data prev)))
    (define x (bytes-ref (scan-data cur) p))
    (bytes-set! (scan-data cur) p (modulo (+ x b) 256)))) 

(define (unfilter-average bpp prev cur)
  (printf "Average\n"))

(define (unfilter-paeth bpp prev cur)
  (for ([p (in-range 0 (bytes-length (scan-data cur)))])
    (define a (get-a p bpp (scan-data cur)))
    (define b (get-b p (scan-data cur) (scan-data prev)))
    (define c (get-c p bpp (scan-data cur) (scan-data prev)))
    (define x (bytes-ref (scan-data cur) p))

    (define pp (- (+ a b) c))
    (define pa (abs (- pp a)))
    (define pb (abs (- pp b)))
    (define pc (abs (- pp c)))
    (define pr (cond
		 [(and (<= pa pb) (<= pa pc)) a]
		 [(<= pb pc) b]
		 [c]))

    (bytes-set! (scan-data cur) p (modulo (+ x pr) 256)))) 

(define (get-a i bpp buf) 
  (if (< i bpp) 
    0 
    (bytes-ref buf (- i bpp))))

(define (get-b i cbuf pbuf) 
  (if (eq? pbuf void) 
    0 
    (bytes-ref pbuf i)))

(define (get-c i bpp cbuf pbuf)
  (if (eq? pbuf void) 
    0
    (get-a i bpp pbuf)))

