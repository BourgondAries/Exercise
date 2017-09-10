#! /usr/bin/env racket
#lang racket

(require "logger.rkt" lens threading)

;; A track is a list of x-sorted coordinates
;; The height is found by interpolating the two nearest
;; points
(define example-track '((0 1) (2 3) (8 -1)))

(define (find-surrounding-points track x)
  (let ([l (findf (lambda (p) (< (first p) x)) (reverse track))]
        [r (findf (lambda (p) (>= (first p) x)) track)])
    (values l r)))

(define (interpolate l r x)
  (let ([dy/dx (/ (- (second r) (second l)) (- (first r) (first l)))])
    (* dy/dx (- x (first l)))))

(define (track-y track x)
  (if (empty? track)
    0
    (let-values ([(l r) (find-surrounding-points track x)])
      (cond
        ([false? l] (second r))
        ([false? r] (second l))
        (else (interpolate l r x))))))

(track-y example-track 1)
