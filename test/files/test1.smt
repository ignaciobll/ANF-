(declare-const a Bool)
(declare-const b Bool)
(declare-const c Bool)
(declare-const d Bool)
(declare-const e Bool)

(assert (xor a b c))
(assert (xor b c))
(assert (xor d c))
(assert (=> (and a b) c))
(assert (or b c a))
(assert (and e d))
(assert (=> a d))
(assert (xor a c))
(assert (xor a d))
(assert (xor a e))
(assert (xor d e))
(assert (and b d))

(check-sat)
