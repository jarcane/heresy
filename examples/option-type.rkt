#lang heresy

(def >>= maybe-bind)

(def fn // (x y)
  (>>= x
       (fn (a)
         (>>= y
              (fn (b)
                (if (= b 0) then None else (some (/ a b))))))))

(def fn //- (x y)
  (maybe-do
   (a <- x)
   (b <- y)
   (yield (if (zero? b) then None else (/ a b)))))


(-> (//- (some 4) (some 2))
    (get-some))

(maybe-do
 (a <- (some 4))
 (c = (* a 4))
 (yield c))

(maybe-do
 (a <- (some 5))
 (b <- (some 4))
 (c = (+ a b))
 (* a c))