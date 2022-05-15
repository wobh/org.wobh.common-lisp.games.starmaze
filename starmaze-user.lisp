;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.starmaze-user
  (:use #:common-lisp)
  (:nicknames #:starmaze-user)
  (:local-nicknames (#:starmaze #:org.wobh.common-lisp.games.starmaze))
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
