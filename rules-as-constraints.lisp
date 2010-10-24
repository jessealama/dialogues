;; particle rules as constraints

;; strategy:
;;
;; * as the game proceeds, more constraints are added.  Consider,
;; e.g., rule D10: "Proponent may assert an atomic formula only after
;; Opponent has asserted it."  We might express this as:
;;
;; (=> (atomic-formula? formula)
;;     (equal-formulas? (formula-at 0) formula))
;;
;; But we *know* what (FORMULA-AT 0) is; so after each turn we can add
;; a new constrait:
;;
;;  (=> (atomic-formula? formula)
;;      (equal-formulas? <formula at move k> formula))
;;
;;  This could be advantageous because EQUAL-FORMULAS? is being called
;;  with an argument of a known CLOS type.  We might even consider
;;  recompiling the rulset occassionally.
;;
;; What about D11?