#lang racket

(require racket/control)


; This is the implementation of -< by the end of Lecture 14.

(define choices (box '()))
(define (add-choice! val) (set-box! choices (cons val (unbox choices))))
(define (get-choice!) (let* ([unboxed (unbox choices)]
                             [val (car unboxed)])
                        (begin (set-box! choices (cdr unboxed))
                               val)))

(define-syntax -<
  (syntax-rules ()
    [(-< expr) ; "when there's only candidate, there's only one choice"
     expr]

    ; Multiple choices: return the first one and store the amb
    ; that produces all the others in choices.
    [(-< expr1 expr2 ...)
     (shift k
            (begin (add-choice! (thunk (k (-< expr2 ...))))
                   (k expr1)))]))

(define (next!)
  (if (empty? (unbox choices))
      (shift k 'done)
      (reset ((get-choice!)))))

(define (backtrack!)
  (shift k (next!)))

(define (?- pred expr)
  (if (pred expr)
      expr
      (backtrack!)))

(define (sudoku4 grid)
  (?- (λ (x) (sudoku-matching x)) (map-grid grid '())))

(define (sudoku-matching grid)
  (define (all-unique-rows? grid)
    (match grid
      ['() #t]
      [(cons row rest) (and (all-unique? row) (all-unique-rows? rest))]))

  (define (all-unique? array)
    (equal? (remove-duplicates array) array))

  (define (all-unique-cols? grid)
    (match grid
      ['(() () () ()) #t]
      [(list first-row second-row third-row fourth-row)
       (and
        (all-unique? (list (car (first grid)) (car (second grid)) (car (third grid)) (car (fourth grid))))
        (all-unique-cols? (list (cdr (first grid)) (cdr (second grid)) (cdr (third grid)) (cdr (fourth grid)))))]))

  (define (quarters? grid)
    (let ([first-quarter (takes-first-quarter grid)]
          [second-quarter (takes-second-quarter grid)]
          [third-quarter (takes-third-quarter grid)]
          [fourth-quarter (takes-fourth-quarter grid)])
          (and
           (all-unique? first-quarter)
           (all-unique? second-quarter)
           (all-unique? third-quarter)
           (all-unique? fourth-quarter))))

  (define (takes-first-quarter grid)
    (list (car(first grid)) (car(second grid)) (second(first grid)) (second(second grid))))

  (define (takes-second-quarter grid)
    (list (car(third grid)) (car(fourth grid)) (second(third grid)) (second(fourth grid))))

  (define (takes-third-quarter grid)
    (list (third(first grid)) (third(second grid)) (fourth(first grid)) (fourth(second grid))))

  (define (takes-fourth-quarter grid)
    (list (third(third grid)) (third(fourth grid)) (fourth(third grid)) (fourth(fourth grid))))
  
  (and
      (quarters? grid)
      (all-unique-cols? grid)
      (all-unique-rows? grid)
      ))

(define (map-grid grid acc)
  (match grid
    ['() acc]
    [(cons row rest-rows) (map-grid rest-rows (append acc (list (map-row row '()))))]))

(define (map-row row acc)
  (match row
    ['() acc]
    [(cons x xs) (map-row xs (append acc (list (check-double-quote x))))]))

(define (check-double-quote x)
  (match x
    ["" (-< 1 2 3 4)]
    [_ x]))