#! /usr/bin/env racket
; Find a single-letter transition path from one word to another.
; Example: (iddfs "sick" "well") -> ("sick" "silk" "sill" "sell" "well")
#lang racket

(provide iddfs)

(require "logger.rkt" lens levenshtein threading)

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
    ; (filter (lambda (x) (= (string-length x) (string-length word))) dictionary)
    (map (lambda (x) (list x (string-levenshtein x word))) dictionary)
    (filter (lambda (x) (= (second x) 1)))
    (map first)))

(define (find-new-single-distance-words seen word dictionary)
  (let* ([expanded (remove* seen (find-single-distance-words dictionary word))]
         [seen* (remove-duplicates (append seen expanded))])
    (values expanded seen*)))

(define (iddfs* i n seen from to dictionary)
  (if (>= i n)
    (values seen #f)
    (let-values ([(e* s*) (find-new-single-distance-words seen from dictionary)])
      (if (member to e*)
        (values (list from to) #t)
        (let-values ([(s t) (for/fold ([s s*] [stop #f])
                                      ([e e*])
                                      #:break stop
                                      (iddfs* (add1 i) n s e to dictionary))])
          (values (cons from s) t))))))

(define (iddfs from to)
  (if (not (= (string-length from) (string-length to)))
    #f
    (let ([dictionary (filter (lambda (x) (= (string-length x) (string-length from))) all-words)])
      (if (or (not (member from dictionary)) (not (member to dictionary)))
        "Not in dict"
        (for/fold ([s '()] [stop #f])
                  ([n (in-naturals (string-levenshtein from to))])
                  #:break stop
          (info n)
          (iddfs* 0 (add1 n) '() from to dictionary))))))

(time (iddfs "clean" "words"))
(time (iddfs "clean" "deals"))
