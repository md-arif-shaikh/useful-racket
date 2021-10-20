(require plot)
(require pict)
(require racket/draw)

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
