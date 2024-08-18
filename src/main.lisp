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
  #:com.djhaskin.plines (:use #:cl)
  (:documentation
    "
    Pennant Lines
    ")
    (:import-from #:alexandria))

(in-package #:com.djhaskin.plines)

