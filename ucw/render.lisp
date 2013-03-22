
(in-package :dialogues-ucw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UCW functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render ((sa (eql *attack-left-conjunct*)))
  (<:as-is "&and;")
  (<:sub "L"))

(defmethod render ((sa (eql *attack-right-conjunct*)))
  (<:as-is "&and;")
  (<:sub "R"))

(defmethod render ((sa (eql *which-instance?*)))
  (<:as-is "?"))

(defmethod render ((sa (eql *which-disjunct?*)))
  (<:as-is "?"))

(defmethod render-fancily ((sa (eql *attack-left-conjunct*)))
  "∧<sub>L</sub>")

(defmethod render-fancily ((sa (eql *attack-right-conjunct*)))
  "∧<sub>R</sub>")

(defmethod render-fancily ((sa (eql *which-instance?*)))
  "?")

(defmethod render-fancily ((sa (eql *which-disjunct?*)))
  "?")

;; HTML representation of strategies and strategy nodes

(defmethod render ((move move))
  (with-slots (player statement stance reference)
      move
    (<:strong (<:as-is player))
    (<:as-is " ")
    (render statement)
    (when (and stance reference)
      (<:as-is " " (format nil "[~a,~d]" stance reference)))))

(defun render-as-table-row (move)
  (with-slots (player statement stance reference)
      move
    (<:tr
     (<:td (<:strong (<:as-is player)))
     (<:td (render statement)))
    (if (and stance reference)
	(<:td (<:format "[~a,~d]" stance reference))
	(<:td))))
