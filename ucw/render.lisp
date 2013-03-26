
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

(defgeneric render-plainly (statement))

(defgeneric render-fancily (statement))

(defmethod render-plainly ((statement term))
  (let ((func-sym (function-symbol statement))
(args (arguments statement)))
    (if (null args)
(format nil "~A" func-sym)
(if (null (cdr args))
(format nil "~A(~A)"
func-sym
(render-plainly (car args)))
(funcall #'concat-strings
(format nil "~A" func-sym)
"("
(render-plainly (car args))
(apply #'concat-strings
(mapcar #'(lambda (arg)
(format nil ",~A" (render-plainly arg)))
(cdr args)))
")")))))

(defmethod render-fancily ((statement term))
  (render-plainly statement))

(defmethod render-plainly :around ((formula unary-connective-formula))
  (let ((body (call-next-method)))
    (concatenate 'string body (render-plainly (argument formula)))))

(defmethod render-fancily :around ((formula unary-connective-formula))
  (format nil "~a~a" (call-next-method) (render-fancily (argument formula))))

(defmethod render-plainly ((neg negation))
  "~")

(defmethod render-fancily ((neg negation))
  "¬")

(defmethod render-plainly :around ((formula binary-connective-formula))
  (concatenate 'string
"("
(render-plainly (lhs formula))
" "
(call-next-method)
" "
(render-plainly (rhs formula))
")"))

(defmethod render-fancily :around ((formula binary-connective-formula))
  (format nil "(~a ~a ~a)"
(render-fancily (lhs formula))
(call-next-method)
(render-fancily (rhs formula))))

(defmethod render-plainly :around ((gen generalization))
  (concatenate 'string
(call-next-method)
(render-plainly (bound-variable gen))
"["
(render-plainly (matrix gen))
"]"))

(defmethod render-fancily :around ((gen generalization))
  (format nil "~a~a[~a]"
(call-next-method)
(render-fancily (bound-variable gen))
(render-fancily (matrix gen))))

(defmethod render-fancily ((formula atomic-formula))
  (format nil "<i>~a</i>" (render-plainly formula)))

(defmethod render-fancily ((sa (eql *which-instance?*)))
  "?")

(defmethod render-fancily ((sa (eql *which-disjunct?*)))
  "?")

(defmethod render-plainly ((formula binary-conjunction))
  "&")

(defmethod render-fancily ((formula binary-conjunction))
  "∧")

(defmethod render-plainly ((formula binary-disjunction))
  "v")

(defmethod render-fancily ((formula binary-disjunction))
  "∨")

(defmethod render-plainly ((formula implication))
  "-->")

(defmethod render-fancily ((formula implication))
  "→")

(defmethod render-plainly ((formula equivalence))
  "<-->")

(defmethod render-fancily ((formula equivalence))
  "↔")

(defmethod render-plainly ((formula universal-generalization))
  "forall")

(defmethod render-fancily ((formula universal-generalization))
  "∀")

(defmethod render-plainly ((formula existential-generalization))
  "exists")

(defmethod render-fancily ((formula existential-generalization))
  "∃")

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
