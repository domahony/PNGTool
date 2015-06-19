#lang racket

(require "tga_types.rkt")

(define (main)
  (define tga (new TGA:tga% [h1 "arg"]))
  (printf "~a" "blah\n") 
  (send tga write-header 'blah4))


(main)

