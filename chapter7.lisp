;;; Dotlist
;; list do not finish with nil
(cons 1 (cons 2 3)); => (1 2 . 3)
'(1 . (2 . (3 . nil))); => (1 2 3)

;;; Versus
;; used when contain two things in one cons cell
(cons 2 3); => (2 . 3)

;;; Circulation list
(setf *print-circle* t); to avoid infinite loop
(defparameter foo (list 1 2 3))
(setf (cdddr foo) foo)
foo; => #1=(1 2 3 . #1#)

;;; Associated list a.k.a alist
;; pair of keys and values
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
(assoc 'lisa *drink-order*); => (lisa . small-drip-coffee)

; push append one pair to head of associated list
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
(assoc 'lisa *drink-order*); => (lisa . large-mocha-with-whipped-cream)
;;; alist is not so efficient data structure

;;;; Wood structure
(defparameter *house* '((walls (mortar (cement)
                                (water)
                                (sand))
                         (bricks))
                        (windows (glass)
                         (frame)
                         (curtains))
                        (roof (shingles)
                         (chimney))))

;;;; Graph structure
;; to visualize graph structure is difficult
;; using two alist and Graphviz is one of simple way to visualize
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                        threre is a well in front of you.))
                               (attic (you are in the attic. threre
                                       is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

;;; Graphviz in a open source software to visualize graphs
;; dotfile accept only alphabet, number, underscore
;; so convert other characters to underscores
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   (concatenate 'string fname)
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk)))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))


(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
