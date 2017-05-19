#lang heresy

(:> '(1 2 3 4)
     (l> map (fn (x) (* x x)))
     (f> left 2)
     (l> append '(a b))
     (f> append '(a b)))

(-> '(1 2 3 4)
    (left 2)
    (append '(a b)))

(->> '(1 2 3 4)
     (map (fn (x) (* x x)))
     (append '(a b)))
