#lang heresy

(import rkt racket)

;; Machine state object
(describe Machine
          (data-ptr 0)
          (instr-ptr 0)
          (cells (for (x in (range 1 to 30000))
                   (carry (join 0 cry)))))

;; helper functions
(def fn constantly (val)
  (fn args* val))

(def fn update (nth fun lst)
  (select
   ((> nth (len lst)) (error 'update "out of index"))
   ((zero? nth) (join (fun (head lst))
                      (tail lst)))
   (else (join (head lst)
               (update (dec nth)
                       fun
                       (tail lst))))))

(def fn assoc-v (tgt lst)
  (:> (assoc tgt lst) tail))

(def fn update-alst (tgt fun lst)
  (let ([orig (assoc-v tgt lst)])
    (subst tgt (fun orig) lst)))

;; Instruction implementations

; >
(def fn inc-dptr (Machine)
  (let ([old (Machine 'data-ptr)])
    (Machine `(,(inc old) * *))))

; <
(def fn dec-dptr (Machine)
  (let ([old (Machine 'data-ptr)])
    (Machine `(,(dec old) * *))))

; +
(def fn inc-byte (Machine)
  (let ([old (Machine 'cells)]
        [ptr (Machine 'data-ptr)])
    (Machine `(* * ,(update ptr inc old)))))

; -
(def fn dec-byte (Machine)
  (let ([old (Machine 'cells)]
        [ptr (Machine 'data-ptr)])
    (Machine `(* * ,(update ptr dec old)))))

; .
(def fn out-byte (Machine)
  (let ([byte (index (inc (Machine 'data-ptr)) (Machine 'cells))])
    (rkt:write-byte byte (rkt:current-output-port))
    Machine))

; ,
(def fn in-byte (Machine)
  (let ([byte (rkt:read-byte (rkt:current-input-port))]
        [old (Machine 'cells)]
        [ptr (Machine 'data-ptr)])
    (Machine `(* * ,(update ptr (constantly byte) old)))))

; [
(def fn jmp-r (Machine prg)
  (let ([init-ptr (Machine 'instr-ptr)])
    (for (x in (list$ (slice$ prg (+ 2 init-ptr))) with `((lefts . 0)
                                                          (count . ,(inc init-ptr))))
      (select case x
              (("]") (if (zero? (assoc-v 'lefts cry)) then
                         (break (Machine `(* ,(inc (assoc-v 'count cry)) *))) else
                         (carry (update-alst 'lefts dec (update-alst 'count inc cry)))))
              (("[") (carry (update-alst 'lefts inc (update-alst 'count inc cry))))
              (else (carry (update-alst 'count inc cry)))))))

; ]
(def fn jmp-l (Machine prg)
  (let ([init-ptr (Machine 'instr-ptr)])
    (for (x in (reverse (list$ (slice$ prg 1 init-ptr))) with `((rights . 0)
                                                                (count . ,init-ptr)))
      (select case x
              (("[") (if (zero? (assoc-v 'rights cry)) then
                         (break (Machine `(* ,(assoc-v 'count cry) *))) else
                         (carry (update-alst 'rights dec (update-alst 'count dec cry)))))
              (("]") (carry (update-alst 'rights inc (update-alst 'count dec cry))))
              (else (carry (update-alst 'count dec cry)))))))

;; Runtime
(def fn inc-iptr (Machine)
  (let ([old (Machine 'instr-ptr)])
    (Machine `(* ,(inc old) *))))

(def fn eval-instr (Machine instr)
  (select case instr
          ((">") (inc-iptr (inc-dptr Machine)))
          (("<") (inc-iptr (dec-dptr Machine)))
          (("+") (inc-iptr (inc-byte Machine)))
          (("-") (inc-iptr (dec-byte Machine)))
          ((".") (inc-iptr (out-byte Machine)))
          ((",") (inc-iptr (in-byte  Machine)))
          (("[") (if (zero? ((Machine 'cells) (Machine 'data-ptr))) then
                     (jmp-r Machine) else
                     (inc-iptr Machine)))
          (("]") (if (not (zero? ((Machine 'cells) (Machine 'data-ptr)))) then
                     (jmp-l Machine) else
                     (inc-iptr Machine)))
          (else (inc-iptr Machine))))

(def fn start (prg)
  (do loop with Machine
    (if (> (cry 'instr-ptr) (dec (len$ prg))) then
        (break cry) else
        (carry (eval-instr cry (index (inc (Machine 'instr-ptr)) (list$ prg)))))))
