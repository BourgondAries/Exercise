#! /usr/bin/env racket
; Find a single-letter transition path from one word to another.
; Example: (iddfs "sick" "well") -> ("sick" "silk" "sill" "sell" "well")
#lang racket

(provide iddfs)

(require lens levenshtein threading)

(define all-words
  (with-input-from-file "/usr/share/dict/words"
    (lambda ()
      (let loop ([line (read-line)]
                 [total '()])
        (if (not (eof-object? line))
          (loop (read-line) (cons line total))
          total)))))

(define (find-single-distance-words dictionary word)
  (~>>
    (filter (lambda (x) (= (string-length x) (string-length word))) dictionary)
    (map (lambda (x) (list x (string-levenshtein x word))))
    (filter (lambda (x) (= (second x) 1)))
    (map first)))

(define (find-new-single-distance-words seen word)
  (let* ([expanded (remove* seen (find-single-distance-words all-words word))]
         [seen* (remove-duplicates (append seen expanded))])
    (values expanded seen*)))

(define (iddfs* i n seen from to)
  (if (>= i n)
    (values seen #f)
    (let-values ([(e* s*) (find-new-single-distance-words seen from)])
      (if (member to e*)
        (values (list from to) #t)
        (let-values ([(s t) (for/fold ([s s*] [stop #f])
                                      ([e e*])
                                      #:break stop
                                      (iddfs* (add1 i) n s e to))])
          (values (cons from s) t))))))

(define (iddfs from to)
  (for/fold ([s '()] [stop #f])
            ([n (in-naturals)])
            #:break stop
    (iddfs* 0 (add1 n) '() from to)))

(time (iddfs "sick" "well"))
