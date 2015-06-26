#lang racket

(require "png_types.rkt")

(provide 
  (combine-out 
    (prefix-out TGA:
		tga%)))

(struct header (
		IDLength ColorMapType ImageType
		CMapStart CMapLength
		XOffset YOffset
		Width Height
		PixelDepth ImageDescriptor
		))

; Main TGA object
;
(define tga%
  (class object%
	 (super-new)
	 (init-field png [hdr (init-header png)])
	 (define/public (write-header f) 
			(printf "~a"
				(bytes (arithmetic-shift (header-Width hdr) -8)))
			(printf "~a"
				(bytes (bitwise-and (header-Width hdr) 255))))
	 (define/private (init-header _png)
			 (printf "~a\n" (PNG:ihdr-width (get-field ihdr _png)))
			 (printf "~a\n" (PNG:ihdr-height (get-field ihdr _png)))
			 (header 
			   0 0 2
			   0 0
			   0 0
			 (PNG:ihdr-width (get-field ihdr _png))
			 (PNG:ihdr-height (get-field ihdr _png))
			   0 0))))
