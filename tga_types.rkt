#lang racket

(provide 
  (combine-out 
    (prefix-out TGA:
		tga%)))

(struct header (
		IDLength
		ColorMapType
		ImageType
		CMapStart
		CMapLength
		XOffset
		YOffset
		Width
		Height
		PixelDepth
		ImageDescriptor
		))

; Main TGA object
;
(define tga%
  (class object%
	 (super-new)
	 (init-field h1 [h1-i (init-header h1)])
	 (define/public (write-header f) 
			(printf "~a\n" h1-i))
	 (define/private (init-header png)
			 (printf "~a\n" png)
			 'some-value)))
