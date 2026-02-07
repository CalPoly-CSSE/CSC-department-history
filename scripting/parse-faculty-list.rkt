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
       . ,rest4
       ;,th
       ;. ,rest
       )
     rest3]
  
    ))

(define-RE dot (inject "."))

;; okay, stopped for now. next step is to have a "de-entity" step, where
;; things like rdquo and ndash get turned back into strings, to enable regexp-based parsing.

(define (line-strs l)
  (match l
    ['() #f]
    [(list-rest 'li '() los) los]
    [other (list 'fail other)]))

(define (parse-strs los)
  (match los
    ['() (error 'parse-strs "empty los")]
    [(list-rest (regexp (px ^ (report (+ (chars [#\A #\Z] [#\a #\z] " .-")))
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
    (map parse-strs _))

