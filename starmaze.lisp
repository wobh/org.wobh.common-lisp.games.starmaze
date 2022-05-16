;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.games.starmaze
  (:use #:common-lisp)
  (:nicknames #:starmaze)
  (:export #:play #:make-starmaze
           #:list-games #:clear-games
           #:list-mazes #:get-maze
           #:list-axes #:get-axes
           #:list-keys #:get-keys
           #:print-star-grid #:print-star-chart)
  (:documentation "ORG.WOBH.COMMON-LISP.GAMES.STARMAZE

DESCRIPTION

Based on the game described by John Cartan at:
http://www.cartania.com/starmaze/intro.html

AUTHORS

William H. Clifford <wobh@wobh.org>

NOTES

Locations in the starmaze are treated as numbers and this code
uses octal numbers to represent them because they easier to
visualize the constellation expected.

(print-star-grid t #o421 'star)

 *    
   *  
     *

By default shangri-la #o777 lies out-of-reach.

Type 'help' for help.
"))

(in-package #:org.wobh.common-lisp.games.starmaze)

;;; Starmaze report functions (many based loosely on Emacs Org-mode format)

(defun print-section (stream level sect-head &key (headspace 0))
  "Make a section heading."
  (format stream "~V&~A ~A~%"
          headspace
          (make-string level :initial-element #\*)
          sect-head))

(defun print-defn-alist (stream defn-alist
                         &key
                         (indent 0)
                         (term-fmt "- ~A ")
                         (defn-fmt ":: ~A"))
  "Print an org-mode-like definition list from a lisp association list."
  (format stream
          (with-output-to-string (defn-list)
            (loop
               for (term . defn) in defn-alist
               do (format defn-list "~&~V,0T~@?~@?"
                          indent
                          term-fmt term
                          defn-fmt defn)))))

(defun print-table-row (stream col-formats col-widths col-data)
  "Print an org-mode-like table row from formatted row data."
  (format stream "~&|~{~:}|" " ~@? ~^|"
          (loop
             for format in col-formats
             for width  in col-widths
             for data   in col-data
             append (list format width data))))

(defun print-table-div (stream col-widths)
  "Print an org-mode-like divider row for table."
  (format stream "~&|~{~:}|" "-~@A-~^+"
          (mapcar (lambda (w) (make-string w :initial-element #\-))
                  col-widths)))

(defun make-table (head-frmts body-frmts col-widths head-data body-data)
  "Make an org-mode-like table with all trimmings."
  (with-output-to-string (table-str)
    (print-table-row table-str head-frmts col-widths head-data)
    (print-table-div table-str col-widths)
    (loop
       for row in body-data
       do (print-table-row table-str body-frmts col-widths row))))


;;; Starmaze data

(defparameter *games* '()
  "List of games played.")

(defun list-games ()
  "List of games played."
  *games*)

(defun clear-games ()
  "Clear list of games played."
  (setf *games* '()))

(defparameter *mazes-db*
  (list
   :classical
   (list #o660 #o700 #o330
         #o444 #o272 #o111
         #o066 #o007 #o033)
   :aborigine
   (list #o640 #o720 #o310
         #o464 #o525 #o131
         #o046 #o027 #o013)
   :moonscape
   (list #o524 #o250 #o521
         #o242 #o272 #o212
         #o425 #o052 #o125)
   :pitchfork
   (list #o504 #o270 #o501
         #o262 #o525 #o232
         #o405 #o072 #o105)
   :supernova
   (list #o432 #o205 #o162
         #o141 #o272 #o414
         #o234 #o502 #o261)
   :jellyfish
   (list #o412 #o225 #o142
         #o161 #o525 #o434
         #o214 #o522 #o241)
   ;; Hypercube coordinate mappings
   :hypercube
   (list #o764 #o255 #o731
         #o343 #o272 #o616
         #o467 #o552 #o137)
   )
  "List of mazes (assumes major axes for which each octal digit
represents each printed row top-to-bottom).")

(defun list-mazes ()
  "List of mazes (assumes major axes for which each octal digit
represents each printed row top-to-bottom)."
  *mazes-db*)

(defun get-maze (maze-name)
  "Get a maze from the list of mazes."
  (getf *mazes-db* (intern (symbol-name maze-name) 'keyword)))
;; NOTE: I originally accepted keywords for arguments but that seemed
;; like a bad idea, so I switched to symbols, but now it does either.

(defun change-maze (locus maze-ref)
  "Convert location from old-maze to new-maze."
  (assert (typep locus '(integer 0 #o777)))
  (let ((new-maze
         (etypecase maze-ref
           (symbol (get-maze maze-ref))
           (list maze-ref))))
    (reduce #'logxor
            (remove-if (complement
                        (lambda (i)
                          (logbitp (- 8 (position i new-maze)) locus)))
                       new-maze))))


(defparameter *axes-db*
  (list
   :major ; (loop for x from 8 downto 0 collect (expt 2 x))
   (list #o400 #o200 #o100
         #o040 #o020 #o010
         #o004 #o002 #o001)
   :loshu
   (list #o010 #o400 #o002
         #o004 #o020 #o100
         #o200 #o001 #o040)
   :ulam
   (list #o020 #o010 #o004
         #o040 #o001 #o002
         #o100 #o200 #o400)
   :minor ; (loop for x from 0 to 8 collect (expt 2 x))
   (list #o001 #o002 #o004
         #o010 #o020 #o040
         #o100 #o200 #o400)
   )
  "List of axes. System uses major internally. Use the others for
conversion.")

(defun list-axes ()
  "List of axes. System uses major internally. Use the others for
conversion."
  *axes-db*)

(defun get-axes (axes-name)
  "Get axes from the list of axes."
  (getf *axes-db* (intern (symbol-name axes-name) 'keyword)))

(defun get-maze-axis (axis axes-ref maze-ref)
  "Get the maze in mazes corresponding to the axis in axes."  
  (elt (etypecase maze-ref
         (symbol (get-maze maze-ref))
         (list maze-ref))
       (position axis (etypecase axes-ref
                        (symbol (get-axes axes-ref))
                        (list axes-ref)))))

(defun change-axes (number old-order-ref new-order-ref)
  "Convert number from old-order to new-order of powers"
  (let ((old-order (etypecase old-order-ref
                     (symbol (get-axes old-order-ref))
                     (list   old-order-ref)))
        (new-order (etypecase new-order-ref
                     (symbol (get-axes new-order-ref))
                     (list   new-order-ref))))
    (reduce #'+
            (mapcar
             #'(lambda (power)
                 (if (logtest number power)
                     (elt new-order
                          (position power old-order))
                     0))
             old-order))))


(defparameter *keys-db*
  '(:number-row  (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
    :number-pad  (#\7 #\8 #\9 #\4 #\5 #\6 #\1 #\2 #\3)
    :qwerty-left (#\q #\w #\e #\a #\s #\d #\z #\x #\c))
  "List of keys.")

(defun list-keys ()
  "List of keys."
  *keys-db*)

(defun get-keys (keys-name)
  "Get keys from the list of keys."
  (getf *keys-db* (intern (symbol-name keys-name) 'keyword)))

(defun get-axis-key (key keys-ref axes-ref)
  "Get the axis in axes corresponding to the key in keys."
  (elt (etypecase axes-ref
         (symbol (get-axes axes-ref))
         (list axes-ref))
       (position key (etypecase keys-ref
                       (symbol (get-keys keys-ref))
                       (list keys-ref)) :test 'equal)))

(defun get-maze-key (key keys-ref axes-ref maze-ref)
  "Get the maze corresponding to the axis corresponding to the key."
  (get-maze-axis (get-axis-key key keys-ref axes-ref) axes-ref maze-ref))

(defparameter *asterices*
  '((mazestart #o020 "Entrance")
    (blackhole #o000 "Black Hole")
    (leavemaze #o757 "Home Base")
    (shangrila #o777 "Shangri-La"))
  "List of constellations.")

(defun get-asterism (star-ref &optional aster-type)
  "Get asterism from reference."
  (let ((aster
         (apply #'find star-ref *asterices*
                (etypecase star-ref
                  (symbol (list :key 'first))
                  (number (list :key 'second))
                  (string (list :key 'third :test 'equal))))))
    (if aster-type
        (ecase aster-type
          (list aster)
          (symbol (first aster))
          (number (second aster))
          (string (third aster)))
        aster)))

;; TODO: It should be possible for a starmaze to have different starts
;; and ends, as well as other unreachable places. May need a starmaze
;; graph to find these.

(defparameter *figures*
  (list
   :symbols (list 'void 'star)
   :star    (list #\space #\*)
   :from    (list #\space #\+)
   :near    (list #\space #\o)
   :yinyang (list "(~A)" "[~A]"))
  "List of figures for lists and strings.")

(defun get-figures (figure-ref)
  "Get axes from the list of axes."
  (getf *figures* (intern (symbol-name figure-ref) 'keyword)))

(defparameter *legend*
  '((void . #\space)
    (star . #\*) ; next
    (from . #\+)
    (near . #\o)
    ;;(yin  "(~A)")
    ;;(yang "[~A]")
    )
  "List of symbols for maps, charts and legends.")

(defun get-legend-sign (legend-ref &optional legend-type)
  "Get legend-sign from reference."
  (let ((sign
         (funcall (etypecase legend-ref
                    (symbol        #'assoc)
                    (character     #'rassoc)
                    ((integer 0 1) #'nth)
                    ;; (boolean #'(lambda (b) (ecase b (t 1) ((nil) 0))))
                    ) legend-ref *legend*)))
    (if legend-type
        (ecase legend-type
          (list sign)
          (symbol (car sign))
          (character (cdr sign))
          (string (string (cdr sign))))
        sign)))

;; FIXME: At this point the *legend* and get-legend-sign seem
;; redundant, but I used to do fancy things with them that *figures*
;; and get-figures do more simply.


;;; Starmaze output functions

(defun make-maze-figures (locus &optional figures)
  "Create a list of figures for representing a maze locus.

Returns a list of binary digits in big-endian order. If figures are given, maps the digits to the figures."
  (assert (typep locus '(integer 0 #o777)))
  (let ((nlist (loop
                  for power from 8 downto 0 
                  collect 
                    (multiple-value-bind (quotient remainder)
                        (floor locus (expt 2 power))
                      (setf locus remainder)
                      quotient))))
    (if figures
        (map 'list
             (lambda (n)
               (elt (etypecase figures
                      (symbol (get-figures figures))
                      (list figures)) n))
             nlist)
        nlist)))

(defun make-cartan-maze-figures (locus)
  "Create list of cartan figures."
  (assert (typep locus '(integer 0 #o777)))
  (loop
       for astr in (make-maze-figures locus (get-figures 'star))
       for cart in (make-maze-figures
                    (change-maze locus 'hypercube)
                    (get-figures 'yinyang))
       collect (format nil cart astr)))

(defun make-key-figures (locus keys)
  "Create a list of available keys at maze locus."
  (assert (typep locus '(integer 0 #o777)))
  (loop
     for d in (make-maze-figures locus)
     ;; for d across (format nil "~9,'0B" n)
     for k in keys
     collect (if (zerop d) ;; (zerop (char-digit-p d))
                 (get-legend-sign 'void 'string)
                 k)))

(defun print-grid (stream figures)
  "Print a 3x3 grid of figures."
  (assert (= 9 (list-length figures)))
  (format stream "~2&~{~:}" "~&~3@{ ~A~}~%" figures))

(defun print-star-grid (stream locus &optional (figures (get-figures 'star)))
  "Print constellation to stream from sector-scan."
  (print-grid stream (make-maze-figures locus figures)))

(defun print-key-grid (stream locus keys)
  "Print keys to stream from sector-scan."
  (print-grid stream (make-key-figures locus keys)))

(defun print-chart (stream figure-lists)
  "Print 3x(3x3) chart of figure grids."
  (assert (= 9 (list-length figure-lists)))
  (format stream
          (with-output-to-string (chart)
            (loop
              initially
                 (format chart "~2&|-------+-------+-------|")
              for i from 0 to 6 by 3
              for j from 3 to 9 by 3
              do
                 (loop
                   for k from 0 to 6 by 3
                   for l from 3 to 9 by 3
                   do
                      (format chart "~&|")
                      (loop
                        for figures in (subseq figure-lists i j)
                        do
                           (format chart "~{ ~A~} |" (subseq figures k l))))
                 (format chart "~&|-------+-------+-------|"))))
  ;; FIXME: Give i, j, k, l more sensible names.
  ;; FIXME: Generalize? Use table making functions?
  ;;   (let ((div (print-table-div nil (list 5 5 5))))
  ;;     do (print-table-row chart ...)
  ;;     (print-table-div chart (list 5 5 5))
  )

(defun print-star-chart (stream loci &optional (figures (get-figures 'star)))
  (flet ((make-maze-figures (locus)
           (make-maze-figures locus figures)))
    (print-chart stream (mapcar #'make-maze-figures loci))))


;;; Starmaze fundamentals

(defun looker (here axis)
  "Starmaze look function"
  (logtest here axis))

(defun walker (here path)
  "Starmaze step function"
  (logxor here path))


;;; Starmaze environment

(defstruct (starmaze (:conc-name sm-))
  "The star maze game environment."
  (maze              (get-maze 'classical))
  (axes              (get-axes 'major))
  (keys              (get-keys 'number-row))
  (look              #'looker) ; #'logtest
  (walk              #'walker) ; #'logxor
  (path              (list (get-asterism 'mazestart 'number)))
  (path-max          nil) ; when (eq max nil) record without limit
  (orders            '())
  (orders-max        64)  ; when (eq max nil) record without limit
  (bad-input-count   0)
  (bad-input-max     6)   ; when (eq max nil) record without limit
  ;;(book *asterices*)
  ;;(signs *legend*)
  (info              '(loshu ulam octal #|cartn gray8|#))
  (turn-info         nil)
  (turn-keys         nil)
  (explore           nil))

(defun sm-here (env)
  "Where are we in the starmaze?"
  (first (sm-path env)))

(defun sm-prev-order (env)
  "What was the last order given?"
 (first (first (sm-orders env))))

(defun sm-walker (env key)
  "Walk down a starmaze path."
  (with-accessors ((keys sm-keys) (maze sm-maze) (axes sm-axes)
                   (look sm-look) (walk sm-walk)
                   (path sm-path) (path-max sm-path-max)) env
    (let ((here (sm-here env)))
      (when (funcall look here (get-axis-key key keys axes))
        (push (funcall walk here (get-maze-key key keys axes maze)) path)
        (etypecase path-max
          ((integer 1)
           (when (< path-max (length path))
             ;; (delete (first (last path)) path :from-end t :count 1)
             (setf path
                   (butlast path
                            (- (length path) path-max)))))
          (Null (first path)))))))

(defun sm-sys-info (env)
  "Make sys-info alist."
  (let ((here (sm-here env))
        (systems
         '((ulam  "Ulam  " "~6,4,'0R")
           (loshu "Lo Shu" " ~3,'0D")
           (octal "Octal " " ~3,'0O")
           #|(cartn "Cartan" " ~3,'0D")|#
           #|(grey8 "Grey  " " ~3,'0D")|#))
        (sys-info '()))
    (loop
       for sys in (sm-info env)
       do
         (let* ((sys-lst (assoc sys systems))
                (sys-txt (second sys-lst))
                (sys-fmt (third  sys-lst)))
           (setf sys-info
                 (acons sys-txt
                        (format nil sys-fmt
                                (cond ((eq sys 'octal) here)
                                      (t (change-axes here 'major sys))))
                        sys-info))))
    sys-info))

(defun sm-print (message &key (stream *standard-output*) (headspace 1))
  "Print a message to player."
  ;;(terpri stream)
  (format stream "~V&~A" headspace message)
  (force-output stream))


;;; Starmaze game functions

(defun show-here (env &key (level 1))
  "Show sky where we are."
  (with-output-to-string (report)
    (print-section report level "We are here:" :headspace 2)
    (print-star-grid report (sm-here env))
    (terpri report)))

(defun show-info (env &key (level 1))
  "Show info about where we are."
  (with-output-to-string (report)
    (print-section report level "System info")
    (print-defn-alist report (sm-sys-info env) :indent (+ 2 level))
    (format report "~%")))

(defun show-keys (env &key (level 1))
  "Report available keys."
  (with-output-to-string (report)
    (print-section report level "Available keys")
    (print-key-grid report (sm-here env) (sm-keys env))))

(defun show-turn (env)
  "Show standard turn message and prompts."
  (with-output-to-string (turn-text)
    (unless (member (sm-prev-order env) '(eval-keys show-here))
      (sm-print (show-here env :level 1) :stream turn-text))
    (when (sm-turn-keys env)
      (sm-print (show-keys env :level 2) :stream turn-text))
    (when (sm-turn-info env)
      (sm-print (show-info env :level 2) :stream turn-text))
    (sm-print "starmaze> " :stream turn-text :headspace 2)))

(defun show-help (env &key (level 1))
  "Report commands and keys."
  (let ((help-data (list
                    '(help . "report commands and keys")
                    '(keys . "report available keys")
                    '(info . "report system information")
                    '(here . "report where we are")
                    '(near . "report nearby systems")
                    '(maps . "report named systems")
                    '(quit . "quit game"))))
    (with-output-to-string (report)
      (print-section report level "Commands")
      (print-defn-alist report help-data :indent 2)
      (print-section report (1+ level) "Keys" :headspace 2)
      (print-key-grid report #o777 (sm-keys env))
      (format report "~%"))))

(defun scan-near (env)
  "Scan all paths and make a list of all nearby systems."
  (with-accessors ((axes sm-axes) (maze sm-maze) (path sm-path)
                   (walk sm-walk) (look sm-look)) env
    (loop
       for axis in axes
       for turn in maze
       collect
         (make-maze-figures (funcall walk (first path) turn)
                            (cond
                              ((equal (second path)
                                      (funcall walk (first path) turn))
                               'from)
                              ((funcall look (first path) axis) 'star)
                              (t 'near))))))

(defun show-near (env &key (level 1))
  "Report chart of nearby systems."
  (let ((chart (print-chart nil (scan-near env))))
    (with-output-to-string (report)
      (print-section report level "Chart of nearby systems")
      (format report chart)
      ;; (format report "~%")
      (print-section report (1+ level) "Legend" :headspace 2)
      (print-defn-alist report *legend*
                        :indent (+ 2 level)
                        :term-fmt "- ~(~A~) " :defn-fmt ":: [~A]")
      (format report "~%"))));FIXME

(defun show-maps (&key (level 1))
  "Report maps of named systems."
  (with-output-to-string (report)
    (print-section report level "Named Systems")
    (print-section report (1+ level) "Legend" :headspace 2)
    (print-defn-alist report (subseq *legend* 0 2)
                      :indent (+ 2 level)
                      :term-fmt "- ~(~A~) " :defn-fmt ":: [~A]")
    (format report "~%")
    (loop for aster in *asterices*
       do
         (print-section report (1+ level) (third aster) :headspace 2)
         (print-star-grid report (second aster)))))


;;; Game elements

(defun end-game (env end)
  "Decided end game message, conditions."
  (with-accessors ((orders sm-orders)) env
    (cond ((member end '(quit-game too-many-bad-inputs))
           (when (y-or-n-p "~&Do you wish to keep playing?")
             (when (eq end 'too-many-bad-inputs)
               (setf (sm-bad-input-count env) 0))
             (pop orders)))
          ((equal end 'leave-maze)
           (cond ((y-or-n-p
                   "~2&We may leave or continue to explore the maze.~
                    ~2&Shall we keep exploring?")
                  (setf (sm-explore env) t))
                 (t (push '(end-game leave-maze) orders) t)))
          ((equal end 'shangri-la) nil) ;; not implemented
          ((equal end 'black-hole) nil) ;; not implemented
          (t (error "End-game ~A not recognized." end)))))


;; Game outcomes

;; |           | player-quits | game-stops |
;; |-----------+--------------+------------|
;; | discovery | home-base    | shangri-la |
;; | reversal  | quit-game    | black-hole |

;; MAYBE: Blackhole doesn't end the loop and the player has to quit.

;; MAYBE: Shangri-La begs you to stay?

(defun eval-here (env)
  "Evaluate location."
  (with-accessors ((here sm-here)
                   (path sm-path)) env
    (let ((asterism (get-asterism here 'symbol)))
      (cond ((eq asterism 'shangrila)
             (sm-print "We have found Shangri-La!")
             (end-game env 'shangri-la))
            ((eq asterism 'blackhole)
             (sm-print "We have fallen into a black hole!")
             (end-game env 'black-hole))
            ((eq asterism 'leavemaze)
             (unless (sm-explore env)
               (sm-print "We have found our home base!")
               (end-game env 'leave-maze)))
            ((eq asterism 'mazestart)
             (when (= (length path) 1)
               (sm-print "We have entered the star maze!")))
            (t nil)))))

(defun eval-keys (env kbd)
  "Evaluate key input."
  (cond ((sm-walker env kbd)
         (sm-print (show-here env :level 1))
         (eval-here env))
        (t (sm-print "No star that way."))))

(defun play-read (&key (stream *query-io*))
  "Read input from player."
  (force-output stream)
  (clear-input stream)
  (let* ((input-line (read-line stream))
	 (input-word (subseq input-line
			     0
			     (position #\space
				       input-line))))
    (if (= 1 (length input-word))
	(char input-word 0)
	(values (intern (symbol-name (read-from-string input-word))
			:keyword)))))


(defun play-apply (order env &rest args)
  "Carry out orders, update order history."
  (cond ((eq order 'bad-input)
         (let ((mesg (format nil "Bad input, '~A'. Type 'help' for help."
                             (first args))))
           (with-accessors ((bad-max sm-bad-input-max)
                            (bad-num sm-bad-input-count)) env
             (etypecase bad-max
               ((integer 1)
                (cond ((< bad-max bad-num)
                       (play-apply 'end-game env
                                   'too-many-bad-inputs))
                      (t  (incf bad-num) mesg)))
               (null mesg)))))
        (t
         (with-accessors ((orders  sm-orders)
                          (ord-max sm-orders-max)) env
           (push (list* order args) orders)
           (etypecase ord-max
             ((integer 0) (when (< ord-max (length orders))
                            ;; (delete (first (last orders)) orders :from-end t :count 1)))
                            (setf orders
                                  (butlast orders
                                           (- (length orders) ord-max)))))
             (null nil)))
         (cond ((eq order 'show-maps)
                (apply (symbol-function order) args))
               (t
                (apply (symbol-function order) env args))))))

(defun play-eval (env kbd)
  "Evaluate player input."
  (if (find kbd (sm-keys env))
      (play-apply 'eval-keys env kbd)
      (case kbd
	(:help     (play-apply 'show-help env))
	(:maps     (play-apply 'show-maps env))
	(:info     (play-apply 'show-info env))
	(:keys     (play-apply 'show-keys env))
	(:here     (play-apply 'show-here env))
	(:near     (play-apply 'show-near env))
	(:quit     (play-apply 'end-game  env 'quit-game))
	(otherwise (play-apply 'bad-input env kbd)))))

(defun play-print (message &key (stream *standard-output*))
  "Print message to player."
  (when (stringp message)
    (sm-print message :stream stream)))

(defparameter *launch-message*
  "
             *** Welcome to the Star Maze! ***

You have found your way to a cluster of stars connected by a
tangle of one-way wormholes. As you travel through the maze
you will see the constellations change. You may find your
way Home, you might fall into the Black Hole, you might even
discover the fabled Shangri-La of the Star Maze.

Good luck, pilgrim ...

Type 'help' for help.
")

;;; Main function

(defun end-game-p (env)
  "Has an end-game order been given?"
  (eq (sm-prev-order env) 'end-game))

(defun play (&optional starmaze)
  "Play a game of Starmaze."
  (sm-print *launch-message*)
  (loop
     with env = (or starmaze (make-starmaze))
     initially (eval-here env)
     do
       (play-print (show-turn env))
       (play-print (play-eval env (play-read)))
     until (end-game-p env)
     finally (push env *games*)))

;;; TODO: Write this whole game with format
