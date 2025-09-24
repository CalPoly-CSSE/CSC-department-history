#lang racket

;; please dear lord someone take the keyboard away from
;; me before I spend another 10 hours writing a parser for
;; fun instead of doing work that is useful to students or
;; coworkers.

(require markdown)

(define parsed
  (parse-markdown
   (file->string "/Users/clements/CSC-department-history/docs/faculty-staff-listing.md")))

(match parsed
  [`((h2 . ,rest1)
     (p . ,rest2)
     (ul . ,rest3)
     . ,rest4
     ;,th
     ;. ,rest
     )
   rest3]
  
  )