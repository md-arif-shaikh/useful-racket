#lang racket
(require plot plot/utils racket/draw colormaps/utils colormaps/tol)
(require data-frame)
(require "x-separated.rkt")
(require "plot-settings.rkt")

(define mm22 (df-read/xsv "./22_mm_f0_2_e_inj_0.005_q_inj_10_m_100_d_1_det_ET_sample_rate_2048_flow_5_e_min_0.0001000_e_max_0.0794328_e_num_30_l_max_inj_4_l_max_rec_2.dat" #:sep #\space))

(define plot-1 (parameterize ([plot-y-transform log-transform]
                              [plot-x-transform log-transform]
                              [plot-x-ticks      (log-ticks)])
                 (plot-pict (for/list ([q (map number->string (range 5.0 6.0 0.5))])
                              (list (points (df-select* mm22 "e" q)))))))

(save-pict-as-pdf plot-1 #:adjust-width 55 #:adjust-height 40 #:file-name "mismatch.pdf")

(define mass-ratios (map number->string (range 5.0 15.0 0.5)))
(define eccentricities (vector->list (df-select mm22 "e")))
(define mismatches (flatten (for/list ([q mass-ratios])
                              (vector->list (df-select mm22 q)))))
(define qs (flatten (for/list ([q mass-ratios])
                      (make-list (length eccentricities) (string->number q)))))
(define eccs (flatten (for/list ([q mass-ratios])
                        eccentricities)))

(define (color-map->list-of-colors cm)
  (parameterize ([plot-pen-color-map cm])
    (for/list ([c (in-range (color-map-size cm))])
      (match-define (list r g b) (->pen-color c))
      (make-object color% r g b))))

(define (get-levels min-val max-val color-count)
  (let* ([intervals-count (sub1 color-count)]
         [step (/ (- max-val min-val) intervals-count)])
    (inclusive-range min-val (+ step max-val) step)))

(define (get-color-index val min-val max-val color-count)
  (let* ([intervals (get-levels min-val max-val color-count)])
    (index-of intervals val >=)))

(define color-list (color-map->list-of-colors 'tol-sd))
(define (get-color val min-val max-val color-list)
  (let* ((color-count (length color-list))
         (color-index (get-color-index val min-val max-val color-count)))
    (last (take color-list (add1 color-index)))))

;(pp-color-map 'tol-sd)
(define min-mm (apply min mismatches))
(define max-mm (apply max mismatches))
;(get-color min-mm min-mm max-mm color-list)

(define scatter (parameterize ([plot-y-transform log-transform]
                               [plot-x-transform log-transform]
                               [plot-x-ticks      (log-ticks)])
                  (plot-pict 
                   (for/list ([ecc eccs]
                              [q qs]
                              [mm mismatches])
                     (points (map vector (list ecc) (list q)) #:color (get-color mm min-mm max-mm color-list) #:sym 'fullcircle1)))))

(require pict)

(define (get-color-picts list-of-colors)
  (for/list ([c list-of-colors])
    (filled-rectangle 30 15 #:draw-border? #f #:color c)))

(define (get-label-picts min-val max-val color-count)
  (for/list ([l (in-list (get-levels min-val max-val color-count))])
    (cc-superimpose
     (ghost (rectangle 30 15))
     (text (real->decimal-string l 3) null (plot-font-size)))))
(define label-picts (get-label-picts min-mm max-mm (length color-list)))
(define color-picts (get-color-picts color-list))
(define colorbar (hc-append (apply vl-append (reverse color-picts))
                            (apply vl-append (reverse label-picts))))
(hc-append 10 scatter colorbar)