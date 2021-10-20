#lang racket
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

(define (save-pict-as-pdf pict inset adjust-width adjust-height file-name)
  (let* ((w (+ inset inset (- (pict-width pict) adjust-width)))
         (h (+ inset inset (- (pict-height pict) adjust-height)))
         (dc (new pdf-dc% [width w] [height h]
                  [interactive #f]
                  [output file-name])))
    (send dc start-doc "export to file")
    (send dc start-page)
    (draw-pict pict dc inset inset)
    (send dc end-page)
    (send dc end-doc)))

(save-pict-as-pdf the-plot 2 120 50 "the-plot.pdf")
