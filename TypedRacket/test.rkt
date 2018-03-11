#lang racket

 (require "type.rkt")

((lambda ([x : int][y : int]) (* 1 (+ 4 x))) 2 3)                       ; 6
((lambda ([x : string][y : string]) (string-append x y)) "dfd" "dfdf")  ; "dfddfdf"
(+ 1 2)                                                                 ; 3
(print-type: 3)                                                         ; int
(print-type: (not #t))                                                  ; bool
(print-type: (lambda ([x : int] [y : string]) y))                       ; (-> int string string)
(print-type: (lambda ([x : int]) (lambda ([y : int]) x)))               ; (-> int (-> int int))
(print-type: (lambda ([x : int][y : int]) x))                           ; (-> int int int)
(print-type: +)                                                         ; (-> int int int)