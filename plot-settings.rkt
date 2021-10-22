#lang racket
(require plot)
(require pict)
(require racket/draw)

(line-width 1.5)
(plot-width 300)
(plot-height 200)
(plot-font-size 10)
(plot-tick-size 7)
(plot-x-far-ticks  no-ticks)
(plot-y-far-ticks  no-ticks)
(plot-pen-color-map 'tab10)
(point-size 5)

(define (save-pict-as-pdf pict #:inset [inset 2] #:adjust-width [adjust-width 100] #:adjust-height [adjust-height 50] #:file-name file-name)
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

(provide save-pict-as-pdf)
