;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.starmaze-test
  (:nicknames #:starmaze-test)
  (:use #:common-lisp)
  (:export #:test-all)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.STARMAZE-TEST
"))

(in-package #:org.wobh.common-lisp.games.starmaze-test)

;; (assert (null (starmaze:list-games)))

(defun test-list-mazes
    (&optional (subject (starmaze:list-mazes)))
  (setf (getf subject :foo)
	'(1 2 3 4 5 6 7 8 9))
  (assert (null (getf (starmaze:list-mazes) :foo))
	  ()
	  "Mazes-db should be immutable through `starmaze:list-mazes'."))

(defun test-all ()
  (loop
    with tests = '(test-list-mazes)
    for test in tests
    do (funcall test)
    finally (return t)))

;; TODO: come up with better tests.
