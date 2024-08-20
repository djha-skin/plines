;;;; main.lisp -- Reference Implementation for Pennant Lines in Common Lisp
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

#+(or)

(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
           (asdf:load-system "alexandria")
           (asdf:load-system "fset"))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.plines/wbtrees (:use #:cl)
  (:documentation
    "
    Pennant Lines
    ")
  (:export
    size
    by-index
    ins-min
    ins-max
    ins
    rm-min
    rm-max
    rm
    retrieve
    unglue
    glue
    foldl
    foldr))

(in-package #:com.djhaskin.plines/wbtrees)

(defstruct node
  value
  size
  left
  right)

(declaim (inline size))
(defun size (tree)
  (if (null tree)
      0
      (node-size tree)))

(declaim (inline glue))
(defun glue (val left right)
  (make-node
    :value val
    :size (+ 1
             (size left)
             (size right))
    :left left
    :right right))

(declaim (inline unglue))
(defun unglue
    (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
      (values tval tleft tright)))

(declaim (inline by-index))
(defun by-index (candidate resident index size)
  (declare (ignore candidate)
           (ignore resident))
  (- index size))

(declaim (inline single-rotation-p))
(defun single-rotation-p (a b)
  (<
    (1+ (size a))
    (ash (1+ (size b)) 1)))

(declaim (inline balanced-p))
(defun balanced-p (a b)
  (let ((aweight (1+ (size a))))
    (>=
      (+ (ash aweight 1) aweight)
      (1+ (size b)))))


(define-condition rotation-error ()
  ((direction :initform (error "Need a direction for rotate error"))
   (tree :initform (error "Need a tree for rotate error"))))

(defun single-rotate-left (val left right)
  (with-slots ((rval value)
               (rleft left)
               (rright right))
      right
    (glue rval
             (glue val left rleft)
             rright)))

(defun double-rotate-left
    (val left right)
  (with-slots ((rval value)
               (rleft left)
               (rright right))
      right
    (with-slots ((rlval value)
                 (rlleft left)
                 (rlright right))
        rleft
      (glue rlval
               (glue val left rlleft)
               (glue rval rlright rright)))))

(defun rotate-left
    (val
      left
      right)
  (with-slots ((rval value)
               (rsize size)
               (rleft left)
               (rright right))
      right
    (if (single-rotation-p rleft rright)
        (single-rotate-left val left right)
        (double-rotate-left val left right))))

(defun balance-left
    (value
      left
      right)
  (if (balanced-p left right)
      (glue value left right)
      (rotate-left value left right)))

(defun single-rotate-right (val left right)
  (with-slots ((lval value)
               (lleft left)
               (lright right))
      left
    (glue lval
             lleft
             (glue val lright right))))

(defun double-rotate-right
    (val left right)
  (with-slots ((lval value)
               (lleft left)
               (lright right))
      left
    (with-slots ((lrval value)
                 (lrleft left)
                 (lrright right))
        lright
      (glue lrval
               (glue lval lleft lrleft)
               (glue val lrright right)))))

(defun rotate-right
    (val
      left
      right)
  (with-slots ((lval value)
               (lsize size)
               (lleft left)
               (lright right))
      left
    (if (single-rotation-p lright lleft)
        (single-rotate-right val left right)
        (double-rotate-right val left right))))

(defun balance-right
    (value
      left
      right)
  (if (balanced-p right left)
      (glue value left right)
      (rotate-right value left right)))

(defun ins-min
    (val tree)
  (if (null tree)
      (glue val nil nil)
      (with-slots ((tval value)
                   (tleft left)
                   (tright right))
          tree
        (balance-right tval (ins-min val tleft) tright))))

(defun ins-max
    (val tree)
  (if (null tree)
      (glue val nil nil)
      (with-slots ((tval value)
                   (tleft left)
                   (tright right))
          tree
        (balance-left tval tleft (ins-max val tright)))))

(defun ins
    (val
      tree
      &key
      (index 0)
      (cmp #'by-index)
      (allow-duplicates t))
  (labels
      ((insrec (rectree recindex dups)
         (if (null rectree)
             (glue
               val
               nil
               nil)
             (with-slots ((tval value)
                          (tsize size)
                          (tleft left)
                          (tright right))
                 rectree
               (let* ((lsize (size tleft))
                      (result (funcall cmp val tval recindex lsize)))
                 (cond
                   ((< result 0)
                    (balance-right
                      tval
                      (insrec tleft recindex dups)
                      tright))
                   ((> result 0)
                    (balance-left
                      tval
                      tleft
                      (insrec tright (- recindex lsize 1) dups)))
                   (dups
                    (balance-left
                      val
                      tleft
                      (ins-min tval
                               tright)))
                      (:else 
                       (return-from ins tree))))))))
    (insrec tree index allow-duplicates)))

(defun rm-min (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
    (if (null tleft)
        (values tright tval)
        (multiple-value-bind (newleft removed)
            (rm-min tleft)
          (values (balance-left
                    tval newleft tright) removed)))))

(defun rm-max (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
    (if (null tright)
        (values tleft tval)
        (multiple-value-bind (newright removed)
            (rm-max tright)
          (values (balance-right
                    tval tleft newright) removed)))))

(defun rm
    (tree
      &key
      (val nil)
      (index 0)
      (cmp #'by-index)
      (allow-duplicates t)
      (sentinel nil))
  (if (null tree)
      (values nil sentinel)
      (with-slots ((tval value)
                   (tsize size)
                   (tleft left)
                   (tright right))
          tree
        (let* ((lsize (size tleft))
               (result (funcall cmp val tval index lsize)))
          (cond
            ((< result 0)
             (multiple-value-bind
                 (delleft removed)
                 (rm tleft
                     :val val
                     :index index
                     :cmp cmp
                     :allow-duplicates allow-duplicates
                     :sentinel sentinel)
               (values
                 (balance-left
                   tval
                   delleft
                   tright)
                 removed)))
            ((> result 0)
             (multiple-value-bind
                 (delright removed)
                 (rm tright
                     :val val
                     :index (- index lsize 1)
                     :cmp cmp
                     :allow-duplicates allow-duplicates
                     :sentinel sentinel)
               (values
                 (balance-right
                   tval
                   tleft
                   delright)
                 removed)))
            ((and (null tright)
                  (null tleft))
             (values nil tval))
            ((null tright)
             (multiple-value-bind (newleft newroot)
                 (rm-max tleft)
               (values
                 (glue newroot newleft tright)
                 tval)))
            (:else
             (multiple-value-bind (newright newroot)
                 (rm-min tright)
               (values
                 (glue newroot tleft newright)
                 tval))))))))

(defun retrieve
    (tree
      &key
      (val nil)
      (index 0)
      (cmp #'by-index)
      (sentinel nil))
  (if (null tree)
      sentinel
      (with-slots ((tval value)
                   (tsize size)
                   (tleft left)
                   (tright right))
          tree
        (let* ((lsize (size tleft))
               (result (funcall cmp val tval index lsize)))
          (cond
            ((< result 0)
             (retrieve
               tleft
               :val val
               :index index
               :cmp cmp
               :sentinel sentinel))
            ((> result 0)
             (retrieve
               tright
               :val val
               :index (- index lsize 1)
               :cmp cmp
               :sentinel sentinel))
            (:else
             tval))))))

(defun foldl
    (tree
      &optional
      (init nil)
      (reducer #'cons))
  (if (null tree)
      init
      (with-slots
          ((tval value)
           (tleft left)
           (tright right))
          tree
        (foldl
          tright
          (funcall
            reducer
            tval
            (foldl tleft init reducer))
          reducer))))

(defun foldr
    (tree
      &optional
      (init nil)
      (reducer #'cons))
  (if (null tree)
      init
      (with-slots
          ((tval value)
           (tleft left)
           (tright right))
          tree
        (foldr
          tleft
          (funcall
            reducer
            tval
            (foldr tright init reducer))
          reducer))))
