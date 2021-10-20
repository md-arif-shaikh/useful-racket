#lang racket
(include "plot-settings.rkt")
(require plot)
(require pict)
(require racket/draw)
(require data-frame)
(define eob (df-read/csv "./eob.csv" #:quoted-numbers? #f))
(define target (df-read/csv "./target.csv" #:quoted-numbers? #t))
(define the-plot
  (hc-append
   (plot-pict (list (lines (df-select* target "time" "amp") #:width 1.5 #:color 3 #:label "target")
                    (lines (df-select* eob "time" "amp") #:style 'short-dash #:width 1.5 #:color "orange" #:label "eob"))
              #:x-min -500
              #:x-label "time"
              #:y-label "h_22"
              #:width 300
              #:height 200)
   (plot-pict (list (lines (df-select* target "time" "amp") #:width 1.5 #:color 3)
                    (lines (df-select* eob "time" "amp") #:style 'short-dash #:width 1.5 #:color "orange"))
              #:x-min -100
              #:x-max 100
              #:y-min 0.1
              #:y-max 0.15
              #:width 300
              #:height 200)))

(save-pict-as-pdf the-plot
                  #:adjust-width 120
                  #:adjust-height 50
                  #:file-name "the-plot.pdf")
