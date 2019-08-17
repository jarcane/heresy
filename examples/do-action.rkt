#lang heresy

(describe Identity (state Null))

(def fn id (v)
  (Identity (list v)))

(def fn >>= (act fn)
  (fn (act 'state)))

(def macroset do> (return :=)
  ((_ (return exp))
   (id exp))
  ((_ (exp ...)) (exp ...))
  ((_ (name := val) exp ...)
   (>>= (id val) (fn (name) (do> exp ...))))
  ((_ (exp0 ...) exp1 ...)
   (>>= (id (exp0 ...)) (fn (_) (do> exp1 ...)))))

(rem (do>
 (a := (input stx))
 (b := (input stx))
 (print (format$ "#_ * #_ = " a b))
 (c := (* a b))
 (a := 100)
 (print c)))

(do>
 (x := 5)
 (print (format$ "Value was #_" x))
 (x := (+ x 5))
 (print (format$ "But now it's #_" x))
 (x := "Behold, a monad ... -ish.")
 (return x))