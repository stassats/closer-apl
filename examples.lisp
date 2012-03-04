(in-package #:closer-apl)
(named-readtables:in-readtable :apl)

|-3| ;=> 3
⌊1/2⌋ ;=> 0
⌈1/2⌉ ;=> 1
√9 ;=> 3
¬∅ ;=> T

(≠ 2 3) ;=> T

∃ n ∈ (1 2 3 4 5): (evenp n) ;=> T
∀ n ∈ (2 4 6 8): (evenp n) ;=> T
{n ∈ ℕ : (< √n 3)} ;=> T
{n ∈ (1 2 3 4): (oddp n)} ;=> (1 3)

∑ k = 1 → 20 (/ k 2) ;=> 105
∏ k = 1 → 6 k ;=> 720