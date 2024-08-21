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
    Pennant Lines' Weight-Balanced Trees
    ")
  (:export
    size
    tree
    by-index
    ins-min
    ins-max
    ins
    rm-min
    rm-max
    rm
    balanced-p
    retrieve
    unglue
    glue
    foldl
    foldr))

(in-package #:com.djhaskin.plines/wbtrees)

(defparameter undefined 'undefined)

(defstruct node
  value
  size
  left
  right)

(deftype tree ()
  '(or node null))

(declaim (inline size))
(defun size (tree)
  (declare (type tree tree))
  (if (null tree)
      0
      (node-size tree)))

(declaim (inline glue))
(defun glue (val left right)
  (declare (type tree left)
           (type tree right))
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
  (declare (type tree tree))
  (with-slots
      ((tval value)
       (tleft left)
       (tright right))
      tree
      (values tval tleft tright)))

(declaim (inline by-index))
(defun by-index (candidate resident index size)
  (declare (type integer index)
           (type integer size)
           (ignore candidate)
           (ignore resident))
  (- index size))

(declaim (inline single-rotation-p))
(defun single-rotation-p (a b)
  (declare (type tree a)
           (type tree b))
  (<
    (1+ (size a))
    (ash (1+ (size b)) 1)))

(declaim (inline balanced-against))
(defun balanced-against (a b)
  (declare (type tree a)
           (type tree b))
  (let ((aweight (1+ (size a))))
    (>=
      (+ (ash aweight 1) aweight)
      (1+ (size b)))))

(declaim (inline balanced-p))
(defun balanced-p (tree)
  (declare (type tree tree))
  (with-slots (left right)
      tree
    (and
      (balanced-against left right)
      (balanced-against right left))))

(defun single-rotate-left (val left right)
  (declare (type tree left)
           (type tree right))
  (with-slots ((rval value)
               (rleft left)
               (rright right))
      right
    (glue rval
          (glue val left rleft)
          rright)))

(defun double-rotate-left
    (val left right)
  (declare (type tree left)
           (type tree right))
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
  (declare (type tree left)
           (type tree right))
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
  (declare (type tree left)
           (type tree right))
  (if (balanced-against left right)
      (glue value left right)
      (rotate-left value left right)))

(defun single-rotate-right (val left right)
  (declare (type tree left)
           (type tree right))
  (with-slots ((lval value)
               (lleft left)
               (lright right))
      left
    (glue lval
             lleft
             (glue val lright right))))

(defun double-rotate-right
    (val left right)
  (declare (type tree left)
           (type tree right))
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
  (declare (type tree left)
           (type tree right))
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
  (declare (type tree left)
           (type tree right))
  (if (balanced-against right left)
      (glue value left right)
      (rotate-right value left right)))

(defun ins-min
    (val tree)
  (declare (type tree tree))
  (if (null tree)
      (glue val nil nil)
      (with-slots ((tval value)
                   (tleft left)
                   (tright right))
          tree
        (balance-right tval (ins-min val tleft) tright))))

(defun ins-max
    (val tree)
  (declare (type tree tree))
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
  (declare (type integer index)
           (type function cmp)
           (type boolean allow-duplicates))
  (labels
      ((insrec (rectree recindex)
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
                      (insrec tleft recindex)
                      tright))
                   ((> result 0)
                    (balance-left
                      tval
                      tleft
                      (insrec tright (- recindex lsize 1))))
                   (allow-duplicates
                    (balance-left
                      val
                      tleft
                      (ins-min tval
                               tright)))
                      (:else
                       (return-from ins tree))))))))
    (insrec tree index)))

(defun rm-min (tree
                &key
                sentinel)
  (declare (type tree tree))
  (if (null tree)
      (values tree sentinel)
      (let ((removed-value sentinel))
        (labels ((rmmin-rec (rectree)
                   (with-slots
                       ((tval value)
                        (tleft left)
                        (tright right))
                       rectree
                     (if (null tleft)
                         (progn
                           (setf removed-value tval)
                           tright)
                         (balance-left
                           tval
                           (rmmin-rec tleft)
                           tright)))))
          (values (rmmin-rec tree) removed-value)))))

(defun rm-max (tree
                &key
                sentinel)
  (declare (type tree tree))
  (if (null tree)
      (values tree sentinel)

      (let ((removed-value sentinel))
        (labels ((rmmax-rec (rectree)
                   (with-slots
                       ((tval value)
                        (tleft left)
                        (tright right))
                       rectree
                     (if (null tright)
                         (progn
                           (setf removed-value tval)
                           tleft)
                         (balance-right
                           tval
                           tleft
                           (rmmax-rec tright))))))
          (values (rmmax-rec tree) removed-value)))))

(defun rm
    (tree
      &key
      (val nil)
      (index 0)
      (cmp #'by-index)
      (sentinel nil))
  (declare (type tree tree)
           (type integer index)
           (type function cmp))
  (let ((removed-value sentinel))
    (labels
        ((rmrec (rectree recindex)
           (if (null rectree)
               (return-from rm
                            (values tree sentinel))
               (with-slots ((tval value)
                            (tsize size)
                            (tleft left)
                            (tright right))
                   rectree
                 (let* ((lsize (size tleft))
                        (result (funcall cmp val tval recindex lsize)))
                   (cond
                     ((< result 0)
                      (balance-left
                        tval
                        (rmrec tleft recindex)
                        tright))
                     ((> result 0)
                      (balance-right
                        tval
                        tleft
                        (rmrec
                          tright
                          (- recindex lsize 1))))
                     ((and (null tright)
                           (null tleft))
                      (setf removed-value tval)
                      nil)
                     ((null tright)
                      (multiple-value-bind (newleft newroot)
                          (rm-max tleft :sentinel sentinel)
                        (setf removed-value tval)
                        (balance-left
                          newroot
                          newleft
                          tright)))
                     (:else
                      (multiple-value-bind (newright newroot)
                          (rm-min tright :sentinel sentinel)
                        (setf removed-value tval)
                        (balance-right
                          newroot
                          tleft
                          newright)))))))))
      (values (rmrec tree index) removed-value))))

(defun retrieve
    (tree
      &key
      (val nil)
      (index 0)
      (cmp #'by-index)
      (sentinel nil))
  (declare (type tree tree)
           (type integer index)
           (type function cmp))
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
  (declare (type tree tree)
           (type function reducer))
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
  (declare (type tree tree)
           (type function reducer))
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

(defun join-with
    (a b pivot)
  (declare (type tree a)
           (type tree b))
  (let ((a-not-too-big (balanced-against b a))
        (b-not-too-big (balanced-against a b)))
    (cond ((and
             a-not-too-big
             b-not-too-big)
           (glue pivot a b))
          (a-not-too-big
           (with-slots ((bval value)
                        (bleft left)
                        (bright right))
             (balance-right
               bval
                 (join-with
                   a
                   bleft
                   pivot)
                 bright)))
          (b-not-too-big
           (with-slots ((aval value)
                        (aleft left)
                        (aright right))
               (balance-left
                 aval
                 aleft
                 (join-with
                   aright
                   b
                   pivot))))
          (:else
           (error "Unhandled condition")))))

(defun join (a b)
  (declare (type tree a)
           (type tree b))
  (cond ((and (zerop (size a))
              (zerop (size b)))
         nil)
        ((zerop (size a)) b)
        ((zerop (size b)) a)
        ((> (size a) (size b))
         (multiple-value-bind (newa newpivot)
             (rm-max a)
           (join-with newa newpivot b)))
        (:else
         (multiple-value-bind (newb newpivot)
             (rm-min b)
           (join-with a newpivot newb)))))

(defun split-at
    (tree
      &key
      index
      val
      (cmp #'by-int)
      sentinel)
  (declare (type tree a)
           (type tree b))
  (let ((effective-index (if (null index)
                             (truncate (size tree) 2)
                             index)))
    (labels ((splitrec (rectree recindex)
               (if (null rectree)
                   (values
                     sentinel
                     nil
                     nil)
                   (with-slots ((tval value)
                                (tleft left)
                                (tright right))
                       rectree
        (let* ((lsize (size tleft))
               (result (funcall cmp val tval recindex lsize)))
          (cond ((< result 0)
                 (multiple-value-bind (sat sleft sright)
                     (splitrec tleft recindex)
                   (values
                     sat
                     sleft
                   (join-with
                     tval
                     sright
                     tright))))
                ((> result 0)
                 (multiple-value-bind (sat sleft sright)
                     (splitrec tright (- recindex lsize 1))
                   (values
                     sat
                   (join-with
                     tval
                     tleft
                     sleft)
                   sright)))
                (:else
                 (values
                   tval
                   tleft
                   tright))))))))
      (splitrec tree index))))


;;; https://en.wikipedia.org/wiki/Weight-balanced_tree
(defun set-union (a b cmp)
  (declare (type tree a)
           (type tree b))
  (cond ((null a) b)
        ((null b) a)
        (:else
         (with-slots ((bval value)
                      (bleft left)
                      (bright right))
             b
           (multiple-value-bind (pivot sleft sright)
               (split-at a
                         :value bval
                         :cmp cmp)
             (declare (ignore pivot))
             (join-with
               (set-union
                 sleft
                 bleft
                 cmp)
               (set-union
                 sright
                 bright
                 cmp)
                 (join-with newleft newright bval)))))))

(defun set-intersection (a b cmp)
  (declare (type tree a)
           (type tree b))
  (cond ((null a) a)
        ((null b) b)
        (:else
         (with-slots ((bval value)
                      (bleft left)
                      (bright right))
             b
           (multiple-value-bind (pivot sleft sright)
               (split-at a
                         :value bval
                         :cmp cmp
                         :sentinel undefined)
             (let ((newleft (set-intersection
                 sleft
                 bleft
                 cmp))
                   (newright (set-intersection
                 sright
                 bright
                 cmp)))
             (if (eq pivot undefined)
                 (join newleft newright)
                 (join-with newleft newright bval))))))))

(defun set-subtract (a b cmp)
  (declare (type tree a)
           (type tree b))
  (cond ((null a) a)
        ((null b) a)
        (:else
         (with-slots ((bval value)
                      (bleft left)
                      (bright right))
             b
           (multiple-value-bind (pivot sleft sright)
               (split-at a
                         :value bval
                         :cmp cmp)
             (declare (ignore pivot))
             (let ((newleft (set-subtract
                 sleft
                 bleft
                 cmp))
                   (newright (set-subtract
                 sright
                 bright
                 cmp)))
                 (join newleft newright)))))))
