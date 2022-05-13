;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.starmaze-test
  (:use #:common-lisp)
  (:local-nicknames (#:starmaze #:org.wobh.common-lisp.games.starmaze))
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.STARMAZE-TEST
"))

(in-package #:org.wobh.common-lisp.games.starmaze-test)

(assert (null (starmaze:list-games)))

(let ((subject (starmaze:list-mazes)))
  (setf (getf subject :foo)
	'(1 2 3 4 5 6 7 8 9))
  (assert (null (getf (starmaze:list-mazes) :foo))
	  ()
	  "Mazes-db should be immutable through `starmaze:list-mazes'."))

;; TODO: come up with better tests.
