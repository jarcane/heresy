#lang heresy

(def fn celebrate ()
  (for (x in (range 0 to 9) with "!")
    (do
      (print (& "Heresy is 1 year old" cry))
      (carry (& "!" cry)))))

(celebrate)