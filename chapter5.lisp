;;;; define nodes
;;; *nodes* is association list. (key: data)
(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in the beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;;;; define edges
;;; *edges* connect *nodes*
(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;; describe all edges in a place
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;;;; define objects in this world
;;; *objects* is list of objects
;;; *object-locations* is alist of object and location
(defparameter *objects* '(whisky bucket frog chain))
(defparameter *object-locations* '((whisky living-room)
                                   (bucket living-room)
                                   (frog garden)
                                   (chain garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                   (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))
;; describe all objects in the place
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))


;;;; 'look' describe where are you and what you can see
;; where are you now is stored in *location*
;; COUTION: the function look is not functional programming style
(defparameter *location* 'living-room)
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;;; player use these commands to take action in this world
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

;; new state of the object is pushed to head of *object-location*
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

;;;; check what does player have now
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
