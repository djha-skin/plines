;;;; main.lisp -- Reference Implementation for Pennant Lines in Common Lisp
;;;;
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

#+(or)
(progn
  (declaim (optimize (speed 3) (safety 1) (debug 0))))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.plines/wbtrees (:use #:cl)
  (:documentation
    "My Doof!")
  (:export
    reverse-tree
    seq-tree
    size
    tree
    by-index
    strcmp
    symbolcmp
    numcmp
    kvcmp
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
    foldr
    split-at
    join-with
    join
    set-intersection
    set-subtract
    set-union))

(in-package #:com.djhaskin.plines/wbtrees)

(defparameter undefined 'undefined)

(defstruct bseq ()
  size
  items)

(defstruct bset (bseq)
  min-element)


(declaim (inline size))
(defgeneric size (tree))
(defgeneric
  (bseq-size tree))


(defgeneric compute-size (item))

(defmethod compute-size ((item t))
  1)

(defmethod compute-size ((item bseq))
  (loop for child across (bseq-items item)
        sum (size child)))




;; (declaim (inline by-index))
;; (defun by-index (candidate resident prevpos next-size)
;;   (declare (ignore candidate)
;;            (ignore resident)
;;            (type (integer 0) prevpos)
;;            (type (integer 0) next-size))
;;   (- prevpos next--size 1))
;; 
;; (declaim (inline strcmp))
;; (defun strcmp (candidate resident prevpos left-size)
;;   (declare (type string candidate)
;;            (type string resident)
;;            (type (integer 0) prevpos)
;;            (type (integer 0) left-size)
;;            (ignore prevpos)
;;            (ignore left-size))
;;   (loop for c across candidate
;;         for r across resident
;;         for diff = (- (char-code c) (char-code r))
;;         while (zerop diff)
;;         finally
;;         (return diff)))
;; 
;; (declaim (inline symbolcmp))
;; (defun symbolcmp (candidate resident prevpos left-size)
;;   (declare (type symbol candidate)
;;            (type symbol resident)
;;            (type (integer 0) prevpos)
;;            (type (integer 0) left-size))
;;   (let ((pkgcmp
;;           (strcmp
;;             (package-name (symbol-package candidate))
;;             (package-name (symbol-package resident))
;;             prevpos
;;             left-size)))
;;     (if (zerop pkgcmp)
;;         (strcmp (symbol-name candidate)
;;                 (symbol-name resident)
;;                 prevpos
;;                 left-size)
;;         pkgcmp)))
;; 
;; (declaim (inline numcmp))
;; (defun numcmp (candidate resident prevpos left-size)
;;   (declare (type number resident)
;;            (type number candidate)
;;            (type (integer 0) left-size)
;;            (type (integer 0) prevpos)
;;            (ignore left-size)
;;            (ignore prevpos))
;;   (- candidate resident))
;; 
;; (declaim (inline kvcmp))
;; (defun kvcmp (cmp)
;;   (lambda (candidate resident prevpos left-size)
;;     (declare (type (integer 0) prevpos)
;;              (type (integer 0) left-size)
;;              (type cons candidate)
;;              (type cons resident))
;;     (funcall cmp (car candidate) (car resident) prevpos left-size)))


(defun vector-insert (elem vec index)
  (loop with result = (make-array (1+ (length vec)))
        with place = 0
        for i from 0 below (length result)
        do
        (if (= i index)
            (setf (svref result i) elem)
            (progn
              (setf (svref result i) (svref vec place))
              (incf place)))
        finally
        (return result)))

(defun vector-delete (vec index)
  (loop with result = (make-array (1- (length vec)))
        with place = 0
        for i from 0 below (length vec)
        when (/= i index)
        do
        (setf (svref result place) (svref vec i))
        (incf place)
        finally
        (return result)))

(defparameter *b* 16)

(defun size-by-children (tree)
  (loop for child across (bseq-items tree)
        sum (size 

(defun split (tree)
  (

(defgeneric ins-min (val thing))
(defmethod ins-min ((val t) (thing t))
  (values 1 val 1 thing))

(defmethod ins-min ((val t) (tree bseq))
  (multiple-value-bind (lsize left rsize right)
      (ins-min val (svref (bseq-items tree) 0))
      (if (zerop rsize)
          dots
          (if (< (1+ (length (bseq-items tree)))
                      (ash *b* 1))
            (progn
              (setf (bseq-items tree)
                      (vector-insert left (bseq-items tree) 0))
                (incf (bseq-size tree)))
            (make-bseq






              (vector-inert
              

          (vector-insert 


      



(defun ins-min
    (val tree)
  (declare (type tree bseq))
  (
  (make-array  *b* :fill-pointer 0)



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
      (cmp #'by-index)
      (index 0)
      (allow-duplicates t))
  (declare (type integer index)
           (type function cmp)
           (type boolean allow-duplicates))
  (labels
      ((insrec (rectree recprev)
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
                      (result (funcall cmp val tval recprev lsize)))
                 (cond
                   ((< result 0)
                    (balance-right
                      tval
                      (insrec tleft recprev)
                      tright))
                   ((> result 0)
                    (balance-left
                      tval
                      tleft
                      (insrec tright result)))
                   (allow-duplicates
                    (balance-left
                      val
                      tleft
                      (ins-min tval
                               tright)))
                   (:else
                    (return-from ins tree))))))))
    (insrec tree (1+ index))))

(defun seq-tree
    (seq &key
         (cmp #'by-index))
  (reduce
    (lambda (c v)
      (ins c v :cmp cmp))
    seq
    :initial-value nil
    :from-end t))

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
        ((rmrec (rectree recprev)
           (if (null rectree)
               (return-from rm
                            (values tree sentinel))
               (with-slots ((tval value)
                            (tsize size)
                            (tleft left)
                            (tright right))
                   rectree
                 (let* ((lsize (size tleft))
                        (result (funcall cmp val tval recprev lsize)))
                   (cond
                     ((< result 0)
                      (balance-left
                        tval
                        (rmrec tleft recprev)
                        tright))
                     ((> result 0)
                      (balance-right
                        tval
                        tleft
                        (rmrec
                          tright
                          result)))
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
      (values (rmrec tree (1+ index)) removed-value))))

(defun retrieve
    (tree
      &key
      (cmp #'by-index)
      (val nil)
      (index 0)
      (sentinel nil))
  (declare (type tree tree)
           (type integer index)
           (type function cmp))
  (labels ((retrec (rectree recprev)
             (if (null rectree)
                 sentinel
                 (with-slots ((tval value)
                              (tsize size)
                              (tleft left)
                              (tright right))
                     rectree
                   (let* ((lsize (size tleft))
                          (result (funcall cmp val tval recprev lsize)))
                     (cond
                       ((< result 0)
                        (retrec
                          tleft
                          recprev))
                       ((> result 0)
                        (retrec
                          tright
                          result))
                       (:else
                        tval)))))))
    (retrec tree (1+ index))))

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

(defun reverse-tree
    (tree)
  (when (not (null tree))
    (multiple-value-bind (val tleft tright)
        (unglue tree)
      (glue val (reverse-tree tright) (reverse-tree tleft)))))

(defun join-with
    (pivot a b)
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
               b
             (balance-right
               bval
                 (join-with
                   pivot
                   a
                   bleft)
                 bright)))
          (b-not-too-big
           (with-slots ((aval value)
                        (aleft left)
                        (aright right))
               a
               (balance-left
                 aval
                 aleft
                 (join-with
                   pivot
                   aright
                   b))))
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
           (join-with newpivot newa b)))
        (:else
         (multiple-value-bind (newb newpivot)
             (rm-min b)
           (join-with newpivot a newb)))))

(defun split-at
    (tree
      &key
      index
      val
      (cmp #'by-index)
      sentinel)
  (declare (type tree tree)
           (type (or null (integer 0)) index)
           (type function cmp))
  (let ((effective-index (if (null index)
                             (size (node-left tree))
                             index)))
    (labels ((splitrec (rectree recprev)
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
               (result (funcall cmp val tval recprev lsize)))
          (cond ((< result 0)
                 (multiple-value-bind (sat sleft sright)
                     (splitrec tleft recprev)
                   (values
                     sat
                     sleft
                   (join-with
                     tval
                     sright
                     tright))))
                ((> result 0)
                 (multiple-value-bind (sat sleft sright)
                     (splitrec tright result)
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
      (splitrec tree (1+ effective-index)))))

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
                         :val bval
                         :cmp cmp)
             (declare (ignore pivot))
             (join-with
               bval
               (set-union
                 sleft
                 bleft
                 cmp)
               (set-union
                 sright
                 bright
                 cmp)))))))

(defun set-intersection (a b cmp)
  (declare (type tree a)
           (type tree b))
  ;; There are other, better algorithms, but like, whatever
  (cond ((null a) a)
        ((null b) b)
        (:else
         (with-slots ((bval value)
                      (bleft left)
                      (bright right))
             b
           (multiple-value-bind (pivot sleft sright)
               (split-at a
                         :val bval
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
                 (join-with bval newleft newright))))))))

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
                         :val bval
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
