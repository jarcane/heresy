#lang heresy

(def cards
  (for (suit in '(♠ ♣ ♥ ♦))
    (carry (append (for (x in (append (range 2 to 10) '(J Q K A)))
                     (carry (join `(,x ,suit) cry)))
                   cry))))

(def fn rand-card (cards)
  (let ((n (int (inc (* (len cards) (rnd))))))
    (index n cards)))

(def shuffled
  (for (x in (range 1 to (len cards)) with (thing (new '())
                                                  (old cards)))
    (def pick (rand-card (cry 'old)))
    (def old (filter (fn (x) (not (equal? x pick))) (cry 'old)))
    (def new (join pick (cry 'new)))
    (carry (cry `(,new ,old)))))

(print (left (shuffled 'new) 5))