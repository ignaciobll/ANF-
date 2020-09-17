(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)

(assert (xor a b c))
(assert (xor b c))
(assert (xor a c))

(check-sat)
