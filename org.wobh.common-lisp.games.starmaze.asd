;; -*- mode: lisp -*-

(defsystem "org.wobh.common-lisp.games.starmaze"
  :description "Common Lisp implementation of _Star Maze_"
  :version "0.0.1"
  :author "William Clifford <will@wobh.org>"
  :in-order-to ((test-op (test-op "org.wobh.common-lisp.games.starmaze/test")))
  :components ((:file "starmaze")
               (:file "starmaze-user"
                      :depends-on ("starmaze"))))

(defsystem "org.wobh.common-lisp.games.starmaze/test"
  :description "Tests for _Star Maze_"
  :depends-on ("org.wobh.common-lisp.games.starmaze")
  :perform (test-op (o c) (symbol-call 'starmaze-test
                                       'test-all))
  :components ((:file "starmaze-test")))
