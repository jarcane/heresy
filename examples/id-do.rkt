#lang heresy

(id-do
 (x = 5)
 (y = 4)
 (z = (+ x y))
 (print (format$ "#_ + #_ = #_" x y z)))