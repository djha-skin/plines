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
    (:import-from #:alexandria)
  (:export
    size
    by-index
    insert-min
    insert-max
    insert
    delete-min
    delete-max
    delete
    access
    unglue
    glue))



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

(declaim (inline newnode))
(defun newnode (val left right)
  (make-node
    :value val
    :size (+ 1
             (size left)
             (size right))
    :left left
    :right right))

(declaim (inline by-index))
(defun by-index (candidate resident index size)
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
    (newnode rval
             (newnode val left rleft)
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
      (newnode rlval
               (newnode val left rlleft)
               (newnode rval rlright rright)))))

(defun rotate-left
    (value
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
      (newnode value left right)
      (rotate-left value left right)))

(defun single-rotate-right (val left right)
  (with-slots ((lval value)
               (lleft left)
               (lright right))
      left
    (newnode lval
             lleft
             (newnode val lright right))))

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
      (newnode lrval
               (newnode lval lleft lrleft)
               (newnode val lrright right)))))

(defun rotate-right
    (value
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

(defun insert-min
    (val tree)
  (if (null tree)
      (newnode val nil nil)
      (with-slots ((tval value)
                   (tleft left)
                   (tright right))
          tree
        (balance-right tval (insert-min val tleft) tright))))

(defun insert-max
    (val tree)
  (if (null tree)
      (newnode val nil nil)
      (with-slots ((tval value)
                   (tleft left)
                   (tright right))
          tree
        (balance-left tval tleft (insert-max val tright)))))

(defun insert
    (val
      tree
      &key
      (index 0)
      (cmp by-index)
      (allow-duplicates t))
  (if (null tree)
      (newnode
        value
        nil
        nil)
      (with-slots ((tval value)
                   (tsize size)
                   (tleft left)
                   (tright right))
          tree
        (let ((result (funcall cmp val tval index tsize)))
          (cond
            ((< result 0)
             (balance-right
               tval
               (insert val
                       tleft
                       :index index
                       :cmp cmp
                       :allow-duplicates allow-duplicates)
               tright))
            ((> result 0)
             (balance-left
               tval
               tleft
               (insert val
                       tright
                       :index (- index size)
                       :cmp cmp
                       :allow-duplicates allow-duplicates)))
            (:else
             (if allow-duplicates
                 (balance-left
                   value
                   tleft
                   (insert-min tval
                           tright))
                 tree)))))))


(defun delete-min (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
    (if (null tleft)
        (values tright value)
        (multiple-value-bind (newleft deleted)
            (delete-min tleft)
          (values (balance-left
                    tval newleft right) deleted)))))

(defun delete-max (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
    (if (null tright)
        (values tleft value)
        (multiple-value-bind (newright deleted)
            (delete-max tright)
          (values (balance-right
                    tval left newright) deleted)))))

(defun delete
    (val
      tree
      &key
      (index 0)
      (cmp by-index)
      (allow-duplicates t))
  (if (null tree)
      (values nil nil)
      (with-slots ((tval value)
                   (tsize size)
                   (tleft left)
                   (tright right))
          tree
        (let ((result (funcall cmp val tval index tsize)))
          (cond
            ((< result 0)
             (multiple-value-bind
                 (delleft deleted)
                 (delete val
                         tleft
                         :index index
                         :cmp cmp
                         :allow-duplicates allow-duplicates)
               (values
                 (balance-left
                   tval
                   delleft
                   tright)
                 deleted)))
            ((> result 0)
             (multiple-value-bind
                 (delright deleted)
                 (delete val
                         tright
                         :index (- index size)
                         :cmp cmp
                         :allow-duplicates allow-duplicates)
               (values
                 (balance-right
                   tval
                   tleft
                   delright)
                 deleted)))
            (:else
             (cond ((and (null tright)
                         (null tleft))
                    (values nil tval))
                   ((null tright)
                    (multiple-value-bind (newleft newroot)
                        (delete-max tleft)
                      (values
                        (newnode newroot newleft tright)
                        tval)
                        ))
                   (:else
                    (multiple-value-bind (newright newroot)
                        (delete-min tright)
                      (values
                      (newnode newroot tleft newright)
                      tval))))))))))

(defun access
    (val
      tree
      &key
      (index 0)
      (cmp by-index)
      (sentinel nil))
  (if (null tree)
      sentinel
      (with-slots ((tval value)
                   (tsize size)
                   (tleft left)
                   (tright right))
          tree
        (let ((result (funcall cmp val tval index tsize)))
          (cond
            ((< result 0)
             (access
               val
               tleft
               :index index
               :cmp cmp
               :sentinel sentinel)
            ((> result 0)
             (access
               val
               tright
               :index index
               :cmp cmp
               :sentinel sentinel))
            (:else
             tval)))))))

(defun unglue
    (tree)
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      (values tval tleft tright)))

(defun glue
    (pivot left right)
  (newnode pivot left right))
