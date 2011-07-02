;;; about.lisp

(in-package :dialogues)

(defcomponent about-component ()
  ())

(defmethod render ((self about-component))
  (<:h1 "About dialogue games and dialogical logic")
  (<:p "Lorenzen dialogues are, originaly, a formalism for capturing
intuitionistic validity using games.  Since their invention and
development in the late 1950s and 1960s, they have been extended from
intuitionistic first-order logic so that they apply to different
notions of validity, such as those of classical logic, modal logics,
linear logic, etc.  For more information,
consult " (<:a :href "http://plato.stanford.edu/entries/logic-dialogical/" "the
entry on dialogue games") " in the " (<:em "Stanford Encyclopedia of
Philosophy") ".")
  (<:p "Dialogue games come in various flavors.  The particular style and notation of dialogue game employed on this site is due to W. Felscher, " (<:as-is "&ldquo;") (<:a :href "http://dx.doi.org/10.1016/0168-0072(85)90016-8"
"Dialogues, strategies, and intuitionistic provability" ) (<:as-is "&rdquo;") ", " (<:i "Annals of Pure and Applied Logic") " " (<:b "28") ", pp. 217" (<:as-is "&ndash;") "254.")
  (<:p "There's also " (<:a :href "http://dialogue-games.blogspot.com/" :title "TONIGHT: Proponent v. Opponent" "a blog") " about dialogue games and dialogical logic, to which the maintainer of this web site contributes.")
  (<:p "The " (<:a :href "http://www-ls.informatik.uni-tuebingen.de/difos/" :title "Dialogical Foundations of Semantics" "homepage for the research project for which this site was designed") " also contains some valuable information.")
  (<:hr)
  (<:address
   (<:a :href "mailto:jesse.alama@gmail.com"
	"Contact the site maintainer")))

(defcomponent about-this-site-component ()
  ())

(defmethod render ((self about-this-site-component))
  (<:h1 "About this site")
  (<:h2 "History and motivation")
  (<:p "This site was created as an attempt to understand dialogue
games and dialogical logic.")
  (<:p "Playing these games by hand, with pencil and paper, often led
to pitfalls because I would often fail to see some possible moves that
could be made in a game, owing to the awkward nature of some of the
rules that are commonly used.  I thus wrote some software for playing
dialogue games that helped me to play them and make statements with
confidence about their properties.")
  (<:p "A web application on top of what I had made seemed valuable to me for various reasons:")
  (<:ul
   (<:li (<:b "disseminating dialogical games") " in the world of
mathematical and philosophical logic.  If some student somewhere plays
just one or two dialogue games and gets hooked, this site will have
served this purpose;")
   (<:li (<:b "enabling collaboration") " between researchers working
on dialogical logic.  The idea is to be able to play concrete games or
search for winning strategies and share the results with other
researchers.  Different researchers would have a common, reliable,
objective presentation of the games, so as to learn from or resolve
disputes about them;"))
  (<:p "(Of course, as a hacker, I also wanted to
just " (<:em "make") " this thing.  But perhaps that goes without
saying.)")
  (<:h2 "Implementation")
  (<:p "This site was written in Common Lisp using
the " (<:a :href "http://common-lisp.net/project/ucw/" :title "UnCommon
Web" "UnCommon Web") " system.  The Common Lisp implementation is " (<:a :href "http://www.sbcl.org" :title "Steel Bank Common Lisp" "SBCL") ".  You can follow the development on " (<:a :href "http://github.com/jessealama/dialogues" :title "dialogues on github" "github") "."))

;;; about.lisp ends here
