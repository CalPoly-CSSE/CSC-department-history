#lang racket

;; please dear lord someone take the keyboard away from
;; me before I spend another 10 hours writing a parser for
;; fun instead of doing work that is useful to students or
;; coworkers.

(require markdown
         racket/runtime-path
         threading
         scramble/regexp)

(define-runtime-path here ".")


(define parsed
  (parse-markdown
   (file->string (build-path here "../docs/faculty-staff-listing.md"))))

(define faculty-lines
  (match parsed
    [`((h2 . ,rest1)
       (p . ,rest2)
       (ul . ,rest3)
       (p . ,rest4)
       (ul . ,rest6)
       . ,rest7
       ;,th
       ;. ,rest
       )
     rest6]
  
    ))

(define-RE dot (inject "."))

(define (line-strs l)
  (match l
    ['() #f]
    [(list-rest 'li '() los) los]
    [other (list 'fail other)]))

(define (stringify s)
  (match s
    [(? string? s) s]
    ['ndash (string #\u2013)]
    ['ldquo (string #\u201c)]
    ['rdquo (string #\u201d)]
    ['lsquo (string #\u2018)]
    ['rsquo (string #\u2019)]
    [other (error 'stringify "unexpected element to stringify: ~e" s)]))

(define (glue-line los)
  (apply string-append (map stringify los)))

(define-RE year (report (repeat (chars digit) 4)))

;; warning: can match an empty string!
(define-RE year-or-similar
  (report (or (cat (? (repeat (chars digit) 4)) (? "?")))))

;; given a string such as "1971-1984" or "?-1989?", return a list
;; containing a start and finish string
(define (date-parse datepiece)
  (match (string-trim datepiece)
    [(regexp (px ^ year-or-similar (or "–" "-") year-or-similar $)
             (list _ start finish))
     (list (cleanup-start-year start) (cleanup-end-year finish))]
    [other (list 'date-range-fail other)]))

;; given the part of a year range, return a parsed version
(define (cleanup-start-year str)
  (match str
    ["?" 'unknown]
    [(regexp (px ^ year $)) (string->number str)]
    [(regexp (px ^ year "?" $) (list _ year-str))
     (list 'about (string->number year-str))]
    [other (list 'failed-cleanup str)]))

(require rackunit)
(check-equal? (cleanup-start-year "1971?") '(about 1971))

;; same as cleanup-start-year, but an empty string is used to indicate
;; "still here"
(define (cleanup-end-year str)
  (match str
    ["" 'still-here]
    [other (cleanup-start-year str)]))

(define (parse-strs str)
  (match str
    ;; millie is the only one with parens in her name, sigh
    #;["Emilia E. “Millie” Buckalew (Villarreal), (1994–2000?), TT, Csc, Ph.D., University of Texas"
     (list "Emilia E. “Millie” Buckalew (Villarreal)" 1994 2000)]
    [(regexp (px ^ (report (+ (chars [#\A #\Z] [#\a #\z] " .-’“”()"))) ;; name
                 "," (* " ") "("
                 (report (* (chars (complement ")"))))  ")"
                 (report (* dot)) $)
             (list _ name date b))
     (define parsed-date
       (map date-parse (regexp-split (px ",") date)))
     
     (list name parsed-date b)]
    #;[(list-rest (regexp (px ^ (report (+ (chars [#\A #\Z] [#\a #\z] " .-")))
                            (report (* dot)) $)
                        (list _ name b))
                r)
     (match b
       [(regexp (px ^ (? ",") (* " ") "(" (* " ") (report (+ (chars digit)))
                    (report (* dot)) $)
                (list _ year r2))
        (list (string-trim name) year r2 r)
        ]
       [other (list 'fail2 name b r)])
     ]))

(~> faculty-lines
    (map line-strs _)
    (filter (λ (x) x) _)
    (map glue-line _)
    (map parse-strs _))

