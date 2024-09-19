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
    pline-wbtree
    wbtree-pline
    ;; seq-pline
    size
    ins-min
    ins-max
    ins
    rm-min
    rm-max
    rm
    balanced-p
    balance
    retrieve
    foldl
    foldr
    split-at
    join-with
    join)
  (:local-nicknames
    (#:wbtrees #:com.djhaskin.plines/wbtrees)))

(in-package #:com.djhaskin.plines/wbtrees)


(defun wbtree-pline (wbtree)
  (labels ((wprec (rectree building)
             (if (null rectree)
                 building
               (multiple-value-bind (pivot left right)
                   (wbtrees:unglue rectree)
                 (wprec left (cons (cons pivot right) building))))))
    (wprec wbtree nil)))

(defun pline-wbtree (pline)
  (labels ((pwrec (recline building))
           (if (null recline)
             building
             (destructuring-bind ((mval . mtree) . rst)
                 recline
               (pwrec rst (wbtrees:glue mval building mtree)))))
    (pwrec pline nil)))


(deftype tree ()
  '(or node null))

(declaim (inline balanced-against))
(defun balanced-against (pline b)
  (let ((aweight (1+ (size a))))
    (>=
      (+ (ash aweight 1) aweight)
      (1+ (size b)))))


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
(defun by-index (candidate resident prevpos left-size)
  (declare (ignore candidate)
           (ignore resident)
           (type (integer 0) prevpos)
           (type (integer 0) left-size))
  (- prevpos left-size 1))

(declaim (inline strcmp))
(defun strcmp (candidate resident prevpos left-size)
  (declare (type string candidate)
           (type string resident)
           (type (integer 0) prevpos)
           (type (integer 0) left-size)
           (ignore prevpos)
           (ignore left-size))
  (loop for c across candidate
        for r across resident
        for diff = (- (char-code c) (char-code r))
        while (zerop diff)
        finally
        (return diff)))

(declaim (inline symbolcmp))
(defun symbolcmp (candidate resident prevpos left-size)
  (declare (type symbol candidate)
           (type symbol resident)
           (type (integer 0) prevpos)
           (type (integer 0) left-size))
  (let ((pkgcmp
          (strcmp
            (package-name (symbol-package candidate))
            (package-name (symbol-package resident))
            prevpos
            left-size)))
    (if (zerop pkgcmp)
        (strcmp (symbol-name candidate)
                (symbol-name resident)
                prevpos
                left-size)
        pkgcmp)))

(declaim (inline numcmp))
(defun numcmp (candidate resident prevpos left-size)
  (declare (type number resident)
           (type number candidate)
           (type (integer 0) left-size)
           (type (integer 0) prevpos)
           (ignore left-size)
           (ignore prevpos))
  (- candidate resident))

(declaim (inline kvcmp))
(defun kvcmp (cmp)
  (lambda (candidate resident prevpos left-size)
    (declare (type (integer 0) prevpos)
             (type (integer 0) left-size)
             (type cons candidate)
             (type cons resident))
    (funcall cmp (car candidate) (car resident) prevpos left-size)))

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
  (if (null tree)
      t
      (with-slots (left right)
          tree
        (and
          (balanced-against left right)
          (balanced-against right left)))))

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
