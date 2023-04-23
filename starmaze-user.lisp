;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.starmaze-user
  (:nicknames #:starmaze-user)
  (:use #:common-lisp)
  (:import-from #:starmaze
		#:play #:make-starmaze
		#:list-games #:clear-games
		#:list-mazes #:get-maze
		#:list-axes #:get-axes
		#:list-keys #:get-keys
		#:print-star-grid #:print-star-chart)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.STARMAZE-USER

For playtesting starmaze."))

(in-package #:org.wobh.common-lisp.games.starmaze-user)
