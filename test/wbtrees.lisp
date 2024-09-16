;;;; main.lisp -- Reference Implementation Parser Tests for NRDL in Common Lisp
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT


#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))

  (progn
    (asdf:load-system "parachute")
    (asdf:load-system "com.djhaskin.plines")
    (asdf:test-system "com.djhaskin.plines")
    )
  )

(in-package #:cl-user)

(defpackage #:com.djhaskin.plines/tests/wbtrees
  (:use #:cl)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from
    #:com.djhaskin.plines/wbtrees)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:wbtrees #:com.djhaskin.plines/wbtrees)))

(in-package #:com.djhaskin.plines/tests/wbtrees)

(define-test basic-functions)

(define-test "weight-balanced trees: basic functions: size"
  :parent basic-functions
  (is eql
      0
      (wbtrees:size nil))
  (is eql
      1
      (wbtrees:size
        (wbtrees::make-node :value 1
                          :size 1
                          :left nil
                          :right nil)))
  (is eql
      17
      (wbtrees:size
        (wbtrees::make-node :value 1
                            :size 17
                            :left nil
                            :right 23))))

(defparameter a (wbtrees:glue 15 nil nil))
(defparameter b (wbtrees:glue 7 nil nil))
(defparameter c (wbtrees:glue 87 nil nil))
(defparameter d (wbtrees:glue 81 nil nil))
(defparameter e (wbtrees:glue 10 c d))
(defparameter f (wbtrees:glue 08 a b))
(defparameter g (wbtrees:glue 5 e f))

(define-test "weight-balanced trees: glue, unglue, and size"
  :parent basic-functions
    (is eql
        (wbtrees:size nil)
        0)
    (is eql
        (wbtrees:size a)
        1)
    (is eql
        (wbtrees:size e)
        (wbtrees:size f))
    (is eql
        (wbtrees:size g)
        7)
    (is-values (wbtrees:unglue a)
               (= 15)
               (eq nil)
               (eq nil))
    (is-values (wbtrees:unglue g)
               (= 5)
               (eq e)
               (eq f)))

(define-test "weight-balanced trees: basic functions: by-index"
  :parent basic-functions
  (true (< (wbtrees:by-index nil "foo" 0 3) 0))
  (true (> (wbtrees:by-index nil "foo" 3 0) 0))
  (true (= (wbtrees:by-index 'debate 'action 4 3) 0)))

(define-test rotation-functions)

(define-test "weight-balanced trees: rotation-functions: single-rotation-p"
  :parent rotation-functions
  (true
      (wbtrees::single-rotation-p nil nil))
  (false
    (wbtrees::single-rotation-p
      (wbtrees:glue "Deli Market" nil nil)
      nil)))


(define-test "weight-balanced trees: rotation functions: balanced-against"
  (true
    (wbtrees::balanced-against nil nil))
  (true
    (wbtrees::balanced-against
      nil
      (wbtrees::make-node
        :value "Deli Market"
        :size 1
        :left nil
        :right nil)))
  (false
    (wbtrees::balanced-against
      nil
      (wbtrees::make-node
        :value "Deli Market"
        :size 3
        :left (wbtrees::make-node
                :value "Meat Section"
                :size 1
                :left nil
                :right nil)
        :right (wbtrees::make-node
                 :value "Salads"
                 :size 1
                 :left nil
                 :right nil)))))


(defparameter one-element (wbtrees:ins 5 nil))

(defparameter eight-element
                  (wbtrees:ins
                    8
                    (wbtrees:ins
                      7
                      (wbtrees:ins
                        6
                        (wbtrees:ins
                          4
                          (wbtrees:ins
                            3
                            (wbtrees:ins
                              2
                              (wbtrees:ins
                                1
                                (wbtrees:ins
                                  5
                                  nil)))))))))

(defparameter eight-element-result
  (wbtrees:glue
      3
      (wbtrees:glue
                6
                (wbtrees:glue
                          8
                          nil
                          (wbtrees:glue
                                    7
                                    nil
                                    nil))
                (wbtrees:glue 4 nil nil))
      (wbtrees:glue
                 1
                 (wbtrees:glue 2 nil nil)
                 (wbtrees:glue 5 nil nil))))


(defparameter random-insert
              (wbtrees:ins
                9
                eight-element
                :index 5))

(defparameter random-insert-result
  (wbtrees:glue
      3
      (wbtrees:glue
                6
                (wbtrees:glue
                          8
                          nil
                          (wbtrees:glue
                                    7
                                    nil
                                    nil))
                (wbtrees:glue 4 nil nil))
      (wbtrees:glue
                 1
                 (wbtrees:glue
                           9
                           nil
                           (wbtrees:glue
                                      2
                                      nil
                                      nil))
                 (wbtrees:glue 5 nil nil))))

(defparameter
  eight-max
  (wbtrees:ins-max
    5
    (wbtrees:ins-max
      1
      (wbtrees:ins-max
        2
        (wbtrees:ins-max
          3
          (wbtrees:ins-max
            4
            (wbtrees:ins-max
              6
              (wbtrees:ins-max
                7
                (wbtrees:ins-max
                  8
                  nil)))))))))

(defparameter
  eight-max-result
  (wbtrees:glue
      4
      (wbtrees:glue
                7
                (wbtrees:glue 8 nil nil)
                (wbtrees:glue 6 nil nil))
      (wbtrees:glue
                 2
                 (wbtrees:glue 3 nil nil)
                 (wbtrees:glue
                            1
                            nil
                            (wbtrees:glue
                                       5
                                       nil
                                       nil)))))

(define-test fold-functions)

(define-test "weight-balanced trees: fold functions"
  :parent fold-functions
  (is
    equal
    '(5 1 2 3 4 6 7 8)
    (wbtrees:foldl eight-max-result nil #'cons))
  (is
    equal
    '(8 7 6 4 3 2 1 5)
    (wbtrees:foldr eight-max-result nil #'cons))
  (is
    equal
    (wbtrees:foldl eight-max-result)
    (wbtrees:foldl eight-element nil #'cons)))

(define-test insert-functions)

(define-test "weight-balanced trees: insert-functions: basic tests"
  :parent insert-functions
  (is
    eql
    (wbtrees:size one-element)
    1)
  (is
    equalp
    eight-element
    eight-element-result)
  (is
    equalp
    (wbtrees:foldr eight-element)
    (wbtrees:foldr (wbtrees:ins-min
      8
      (wbtrees:ins-min
        7
        (wbtrees:ins-min
          6
          (wbtrees:ins-min
            4
            (wbtrees:ins-min
              3
              (wbtrees:ins-min
                2
                (wbtrees:ins-min
                  1
                  (wbtrees:ins-min
                    5
                    nil))))))))))
  (is
    equalp
    random-insert-result
    random-insert)
  (is
    equalp
    (wbtrees:ins "four" random-insert-result :index 18)
    (wbtrees:glue
        3
        (wbtrees:glue
                  6
                  (wbtrees:glue
                            8
                            nil
                            (wbtrees:glue
                                      7
                                      nil
                                      nil))
                  (wbtrees:glue 4 nil nil))
        (wbtrees:glue
                   1
                   (wbtrees:glue
                             9
                             nil
                             (wbtrees:glue
                                        2
                                        nil
                                        nil))
                   (wbtrees:glue
                              5
                              nil
                              (wbtrees:glue
                                         "four"
                                         nil
                                         nil)))))
  (is
    equalp
    (wbtrees:ins nil random-insert-result :index 18)
    (wbtrees:glue
        3
        (wbtrees:glue
                  6
                  (wbtrees:glue
                            8
                            nil
                            (wbtrees:glue
                                      7
                                      nil
                                      nil))
                  (wbtrees:glue 4 nil nil))
        (wbtrees:glue
                   1
                   (wbtrees:glue
                             9
                             nil
                             (wbtrees:glue
                                        2
                                        nil
                                        nil))
                   (wbtrees:glue
                              5
                              nil
                              (wbtrees:glue
                                         nil
                                         nil
                                         nil))))))

(defparameter ins-by-int
  (wbtrees:ins
    1
    (wbtrees:ins
      4
      (wbtrees:ins
        2
        (wbtrees:ins
          5
          (wbtrees:ins
            3
            (wbtrees:ins
              6
              (wbtrees:ins
                8
                (wbtrees:ins
                  -1
                  nil)
                :cmp #'wbtrees:numcmp)
              :cmp #'wbtrees:numcmp)
            :cmp #'wbtrees:numcmp)
          :cmp #'wbtrees:numcmp)
        :cmp #'wbtrees:numcmp)
      :cmp #'wbtrees:numcmp)
    :cmp #'wbtrees:numcmp))

(define-test
  "weight-balanced trees: insert-functions: sorted mode"
  (is
    equalp
    ins-by-int
    (wbtrees:glue
        3
        (wbtrees:glue
            -1
            nil
            (wbtrees:glue
                       2
                       (wbtrees:glue
                                 1
                                 nil
                                 nil)
                       nil))
        (wbtrees:glue
            6
            (wbtrees:glue
                      5
                      (wbtrees:glue
                                4
                                nil
                                nil)
                      nil)
            (wbtrees:glue 8 nil nil))))
  (is equalp
      (wbtrees:ins 5 ins-by-int :cmp #'by-int)
      (wbtrees:glue
          3
          (wbtrees:glue
                    -1
                    nil
                    (wbtrees:glue
                               2
                               (wbtrees:glue
                                         1
                                         nil
                                         nil)
                               nil))
          (wbtrees:glue
                     6
                     (wbtrees:glue
                               5
                               (wbtrees:glue
                                         4
                                         nil
                                         nil)
                               (wbtrees:glue
                                          5
                                          nil
                                          nil))
                     (wbtrees:glue 8 nil nil))))
  (is
    eq
    (wbtrees:ins 5 ins-by-int :cmp #'by-int :allow-duplicates nil)
    ins-by-int))

(define-test retrieve-functions)

(define-test "weight-balanced trees: retrieve functions"
  :parent retrieve-functions
  (is eql
      (wbtrees:retrieve eight-max-result
                        :index 5)
      2)
  (is eql
      (wbtrees:retrieve nil :index 25 :sentinel :not-found)
      :not-found)
  (is eql
      (wbtrees:retrieve eight-max-result
                        :index 25 :sentinel :not-found)
      :not-found))

(define-test removal-functions)

(define-test "weight-balanced trees: removal-functions: basic tests"
  :parent removal-functions
  (is-values
    (wbtrees:rm nil :sentinel :not-found)
    (eq nil)
    (eql :not-found))
  (is-values
    (wbtrees:rm-max nil :sentinel :not-found)
    (eq nil)
    (eql :not-found))
  (is-values
    (wbtrees:rm-max nil :sentinel :not-found)
    (eq nil)
    (eql :not-found))
  (is-values
    (wbtrees:rm one-element :index 25 :sentinel :not-found)
    ;; TODO: make `eq` work
    (equalp one-element)
    (eql :not-found))
  (is-values
    (wbtrees:rm eight-element :index 4)
    (equalp (wbtrees:glue
                2
                (wbtrees:glue
                          6
                          (wbtrees:glue
                                    8
                                    nil
                                    (wbtrees:glue
                                              7
                                              nil
                                              nil))
                          (wbtrees:glue 4 nil nil))
                (wbtrees:glue
                           1
                           nil
                           (wbtrees:glue 5 nil nil))))
    (eql 3))
  (is-values
    (wbtrees:rm-max (wbtrees:rm-max (wbtrees:rm-max eight-element)))
    (equalp (wbtrees:glue
                6
                (wbtrees:glue
                          8
                          nil
                          (wbtrees:glue 7 nil nil))
                (wbtrees:glue
                           3
                           (wbtrees:glue 4 nil nil)
                           nil)))
    (eql 2))
  (is-values (wbtrees:rm-min eight-element)
             (equalp
               (wbtrees:glue
                   3
                   (wbtrees:glue
                             6
                             (wbtrees:glue 7 nil nil)
                             (wbtrees:glue 4 nil nil))
                   (wbtrees:glue
                              1
                              (wbtrees:glue 2 nil nil)
                              (wbtrees:glue 5 nil nil))))
             (eql 8))
  (is-values (wbtrees:rm-min one-element)
             (eq nil)
             (eql 5))
  (is-values (wbtrees:rm-max one-element)
             (eq nil)
             (eql 5)))

(defparameter right-dangerous
  (wbtrees:glue
      3
      (wbtrees:glue
                6
                (wbtrees:glue
                          7
                          (wbtrees:glue
                                    8
                                    nil
                                    nil)
                          (wbtrees:glue
                                     81
                                     nil
                                     nil))
                (wbtrees:glue 4 nil nil))
      (wbtrees:glue
                 1
                 nil
                 nil)))

(define-test "weight-balanced trees: removal-functions: right-dangerous tests"
  (is-values
    (wbtrees:rm-max right-dangerous)
    (equalp
      (wbtrees:glue
          6
          (wbtrees:glue
                    7
                    (wbtrees:glue 8 nil nil)
                    (wbtrees:glue 81 nil nil))
          (wbtrees:glue
                     3
                     (wbtrees:glue 4 nil nil)
                     nil)))
    (eql 1))
  (is-values
    (wbtrees:rm-min right-dangerous)
    (equalp
      (wbtrees:glue
          3
          (wbtrees:glue
                    6
                    (wbtrees:glue
                              7
                              nil
                              (wbtrees:glue
                                         81
                                         nil
                                         nil))
                    (wbtrees:glue 4 nil nil))
          (wbtrees:glue 1 nil nil)))
    (eql 8))
  (is-values (wbtrees:rm right-dangerous :index 3)
             (equalp
               (wbtrees:glue
                   3
                   (wbtrees:glue
                             7
                             (wbtrees:glue 8 nil nil)
                             (wbtrees:glue
                                        4
                                        (wbtrees:glue
                                                  81
                                                  nil
                                                  nil)
                                        nil))
                   (wbtrees:glue 1 nil nil)))
             (eql 6))
  (is-values (wbtrees:rm right-dangerous :index 5)
             (equalp
               (wbtrees:glue
                   6
                   (wbtrees:glue
                             7
                             (wbtrees:glue 8 nil nil)
                             (wbtrees:glue 81 nil nil))
                   (wbtrees:glue
                              1
                              (wbtrees:glue 4 nil nil)
                              nil)))
             (eql 3)))

(define-test "weight-balanced trees: removal-functions: set tests"
  :parent removal-functions
  (is-values (wbtrees:rm ins-by-int
                         :val 5
                         :cmp #'wbtrees:numcmp)
             (equalp
               (wbtrees:glue
                   3
                   (wbtrees:glue
                             -1
                             nil
                             (wbtrees:glue
                                        2
                                        (wbtrees:glue
                                                  1
                                                  nil
                                                  nil)
                                        nil))
                   (wbtrees:glue
                              6
                              (wbtrees:glue 4 nil nil)
                              (wbtrees:glue 8 nil nil))))
             (eql 5)))
