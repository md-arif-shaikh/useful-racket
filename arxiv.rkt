#lang racket
(require racket/cmdline) ;; for command line parsing
(require html-parsing) ;; for parsing html to sxml
(require net/url) ;; for getting html source from url
(require sxml sxml/sxpath) ;; for parsing xml
(require gregor) ;; for date arithmatics

(define to-date (make-parameter (~t (today/utc) "yyyy-MM-dd")))
(define from-date (make-parameter (~t (-weeks (today/utc) 1) "yyyy-MM-dd")))

(define parser
  (command-line
   #:usage-help
   "Take date to search from and date to search upto"
   "or number of weeks."

   #:once-each
   [("-f" "--from-date") SEARCH-FROM-DATE
    "Search paper from SEARCH-FROM-DATE ['YYYY-MM-DD']"
    (from-date SEARCH-FROM-DATE)]
   [("-t" "--to-date") SEARCH-TO-DATE
    "Search paper from SEARCH-TO-DATE ['YYYY-MM-DD']"
    (to-date SEARCH-TO-DATE)]
   [("-w" "--weeks") SEARCH-WEEKS
    "Search paper from last SEARCH-WEEKS [Number]"
    (from-date (~t (-weeks (today/utc) (string->number SEARCH-WEEKS)) "yyyy-MM-dd"))]

   #:args () (void)))

(printf "--- Searching papers from ~a to ~a ---\n"
        (~t (iso8601->date (from-date)) "E, MMMM d, y")
        (~t (iso8601->date (to-date)) "E, MMMM d, y"))

(define arxiv-url (format (string-append "https://arxiv.org/search/advanced?advanced=&terms"
                                         "-0-operator=AND&terms-0-term=gravitational-wave&terms"
                                         "-0-field=abstract&terms-1-operator=OR&terms-1-term="
                                         "%22gravitational+wave%22&terms-1-field=abstract&"
                                         "classification-physics=y&classification-physics_archives"
                                         "=astro-ph&classification-include_cross_list=include&date"
                                         "-year=&date-filter_by=date_range&date-from_date=~a&date-"
                                         "to_date=~a&date-date_type=submitted_date_first&abstracts"
                                         "=show&size=50&order=-announced_date_first")
                          (from-date) (to-date)))

(printf "--- parsing arxiv html ---\n")
(define arxiv-xml (html->xexp
                   (get-pure-port (string->url arxiv-url))))

(printf "--- Looking for titles ---\n")

(define (get-title doc)
  (define title ((sxpath "//p[contains(@class, 'title is-5 mathjax')]/text()")
                 doc))
  (define formatted-title (remove* (list "\n" "      \n" "    ") title))
  (string-trim (list-ref formatted-title  0))
  )

(define (get-authors doc)
  (define authors ((sxpath "//p[contains(@class, 'authors')]/a/text()") doc))
  (string-join authors ", "
               #:before-last " and ")
  )

(define (get-link doc)
  (define arxiv-id ((sxpath "//p[contains(@class, 'list-title is-inline-block')]/a/text()") doc))
  (define arxiv-link (string-join (list "https://arxiv.org/abs/" (substring (string-trim (list-ref arxiv-id 0)) 6)) ""))
  arxiv-link)

(define results ((sxpath "//li[contains(@class, 'arxiv-result')]")
                 arxiv-xml))

(if (> (length results) 0)
    (begin
      (printf "--- Found ~a results ---\n" (length results))
      (for ([idx (build-list (length results) values)]
            [result results]
            )
        (define title (get-title result))
        (define authors (get-authors result))
        (define link (get-link result))
        (if (< idx 9)
            (printf "0~a. Title: ~a\n" (+ 1 idx) title)
            (printf "~a. Title: ~a\n" (+ 1 idx) title)
            )
        (printf "    Authors: ~a\n" authors)
        (printf "    Link: ~a\n" link)
        ))
    (printf "--- No titles found ---"))
