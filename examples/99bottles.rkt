#lang heresy

(def bot 99)

(def fn bottles (n)
  (for (x in (range n to 1 step -1))
    (? (format$ "#_ bottles of beer on the wall, #_ bottles of beer," x x))
    (? "Take one down pass it around,")
    (if (zero? (dec x)) then
        (? "No more bottles of beer on the wall.")
        else
        (do 
          (print & (dec x))
          (? " bottles of beer on the wall.")))))

(bottles bot)