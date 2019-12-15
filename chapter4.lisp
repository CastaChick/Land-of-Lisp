(if '()
    'list-has-stuff-in-it
    'list-is-empty)

(if '(1)
    'list-has-stuff-in-it
    'list-is-empty)

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(eq '() nil); => T
(eq '() ()); => T
(eq '() 'nil); => T

(if (= (+ 1 2) 3)
    'yup
    'nope)

(if (oddp 5)
    'odd-number
    'even-number)

(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
                 'odd-number)
    'even-number)

(when (oddp 5)
  (setf *number-was-odd* t)
  'odd-number)

(unless (oddp 4)
  (setf *number-was-odd* nil)
  'even-number)

(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnny))
        (t '(why you eat my pudding stranger ?))))

(defun pudding-eater-2 (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
     '(couse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger ?))))

(and (oddp 5) (oddp 7) (oddp 9))
(or (oddp 4) (oddp 7) (oddp 8))

(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t))

(member 1 '(3 4 1 5)); => (1 5)
(member 1 (cons 3 (cons 4 (cons 1 (cons 5 nil))))); => (cons 1 (cons 5 nil))

(find-if #'oddp '(2 4 5 6)); => 5
;;Coution
(find-if #'null '(2 4 nil 6)); => NIL

;; eq is used only when compare symbols
(defparameter *fruit* 'apple)
(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))

;; equal is used in many ways
(equal 'apple 'apple); => T
(equal (list 1 2 3) (list 1 2 3)); => T
(equal '(1 2 3) (cons 1 (cons 2 (cons 3 nil)))); => T
(equal 5 5); => T
(equal 2.5 2.5); => T
(equal "foo" "foo"); => T
(equal #\a #\a); => T

;; eql resembles eq, but can be used when compare numbers or character
(eql 'foo 'foo); => T
(eql 3.4 3.4); => T
(eql #\a #\a); => T

;; equalp resembles equal, but it can handle complex tasks
(equalp "Bob Smith" "bob smith"); => T
(equalp 0 0.0); => T

;; = is used when compare numbers
;; string-equal used when compare strings
;; char-equal used when compare characters
(= 4 4)
(string-equal "Bob" "Bob")
(char-equal #\a #\a)
