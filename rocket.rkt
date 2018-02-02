;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define-struct line (beginning end color))
;; A line is a structure:
;;         (make-line p1 p2 s)
;; where p1 and p2 are posns and s is a (color) symbol.

;; A list-of-lines is either
;; 1. empty, or
;; 2. (cons l lol) where l is a line and lol is a list of lines.
;; example:
;; 1.empty
;; 2.(cons (make-line (make-posn 75 75) (make-posn 225 225) 'yellow)
;;         empty)
;; 3.(cons (make-line (make-posn 50 50) (make-posn 10 10) 'orange)
;;         (cons (make-line (make-posn 75 75) (make-posn 225 225) 'yellow)
;;               empty))

(define ROCKET (cons (make-line (make-posn 50 440) (make-posn 40 450) 'red)
                     (cons (make-line (make-posn 40 450) (make-posn 40 470) 'red)
                           (cons (make-line (make-posn 40 470) (make-posn 30 480) 'red)
                                 (cons (make-line (make-posn 30 480) (make-posn 40 500) 'red)
                                       (cons (make-line (make-posn 40 500) (make-posn 50 480) 'red)
                                             (cons (make-line (make-posn 50 480) (make-posn 60 500) 'red)
                                                   (cons (make-line (make-posn 60 500) (make-posn 70 480) 'red)
                                                         (cons (make-line (make-posn 70 480) (make-posn 60 470) 'red)
                                                               (cons (make-line (make-posn 60 470) (make-posn 60 450) 'red)
                                                                     (cons (make-line (make-posn 60 450) (make-posn 50 440) 'red)
                                                                           empty)))))))))))

;; draw-a-line : line -> boolean
;; to draw a-line
(define (draw-a-line a-line)
  (draw-solid-line (line-beginning a-line)
                   (line-end a-line)
                   (line-color a-line)))

;; draw-lol : list-of-lines -> boolean
;; to draw alol
(define (draw-lol alol)
  (cond
    [(empty? alol) true]
    [else (and (draw-a-line (first alol))
               (draw-lol (rest alol)))]))

;; clear-a-line : line -> boolean
;; to clear a-line
(define (clear-a-line a-line)
  (clear-solid-line (line-beginning a-line)
                    (line-end a-line)))

;; clear-lol : list-of-lines -> boolean
;; to clear alol
(define (clear-lol alol)
  (cond
    [(empty? alol) true]
    [else (and (clear-a-line (first alol))
               (clear-lol (rest alol)))]))

;; draw-and-clear-lol : list-of-lines -> boolean
;; to draw alol, wait for a short time, and then to clear alol
(define (draw-and-clear-lol alol)
  (and (and (draw-lol alol)
            (sleep-for-a-while 1))
       (clear-lol alol)))

;; translate-line : line number -> line
;; to create a line from a-line which is similar to it
;; except it has been translated in the y direction by delta pixels
;; examples:
;; 1.(translate-line (make-line (make-posn 15 30) (make-posn 50 100) 'yellow) 15)
;;   should produce
;;   (make-line (make-posn 15 45) (make-posn 50 115) 'red)
;; 2.(translate-line (make-line (make-posn 75 100) (make-posn 90 180) 'orange) 10)
;;   should produce
;;   (make-line (make-posn 75 110) (make-posn 90 190) 'orange)
(define (translate-line a-line delta)
  (make-line (make-posn (posn-x (line-beginning a-line))
                        (+ (posn-y (line-beginning a-line))
                           delta))
             (make-posn (posn-x (line-end a-line))
                        (+ (posn-y (line-end a-line))
                           delta))
             (line-color a-line)))

;; translate-lol : list-of-lines number -> list-of-lines
;; to create a list of lines from alol which is similar to it
;; except every item in the list has been translated in the -y direction by delta pixels
;; examples:
;; 1.(translate-lol empty 50)
;;   should produce
;;   empty
;; 2.(translate-lol (cons (make-line (make-posn 75 75) (make-posn 225 225) 'yellow)
;;                        (cons (make-line (make-posn 225 75) (make-posn 75 225) 'orange)
;;                              empty)) 10)
;;   should produce
;;   (cons (make-line (make-posn 75 85) (make-posn 225 235) 'yellow)
;;         (cons (make-line (make-posn 225 85) (make-posn 75 235) 'orange)
;;               empty))
(define (translate-lol alol delta)
  (cond
    [(empty? alol) empty]
    [else (cons (translate-line (first alol) delta)
                (translate-lol (rest alol) delta))]))

;; move-lol : number list-of-lines -> ???
;; to draw alol, wait for some time, to clear alol,
;; and then produce a list of lines which are similar to alol
;; except every item in the list has been translated in the -y direction by delta pixels
(define (move-lol delta alol)
  (cond
    [(draw-and-clear-lol alol) (translate-lol alol delta)]))