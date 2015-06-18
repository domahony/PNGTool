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
	 (field [header (void)])
	 (define/public (write-header f)
			)))
