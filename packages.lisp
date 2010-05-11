
(in-package :cl-user)

(defpackage :dialogues
  (:use :cl)
  (:export ; utils
           "EMPTY-STRING?" ; funny: this has nothing to do with dialogues
	   ; named formulas
	   "peirce-formula"
	   "excluded-middle"
	   "dummett-formula"
	   "markov-formula"
	   "double-negation-intro"
	   "double-negation-elimination"
	   "k-formula"
	   "b-formula"
	   "c-formula"
	   "w-formula"
	   "weak-excluded-middle"
	   "scott-formula"
	   "smetanich-formula"))
