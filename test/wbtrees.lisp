;;;; main.lisp -- Reference Implementation Parser Tests for NRDL in Common Lisp
;;;;
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

(define-test "weight-balanced trees: basic functions: from-left"
  :parent basic-functions
  (true (< (wbtrees:from-left nil "foo" 0 3 3) 0))
  (true (> (wbtrees:from-left nil "foo" 3 0 3) 0))
  (true (= (wbtrees:from-left 'debate 'action 4 3 0) 0)))

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
      (wbtrees:ins 5 ins-by-int :cmp #'wbtrees:numcmp)
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
    (wbtrees:ins 5 ins-by-int :cmp #'wbtrees:numcmp :allow-duplicates nil)
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

(defparameter NATO-alphabet
  '(
    "echo"
    "x-ray"
    "foxtrot"
    "delta"
    "alpha"
    "romeo"
    "india"
    "juliet"
    "Victor"
    "yankee"
    "kilo"
    "lima"
    "charlie"
    "victor"
    "quebec"
    "golf"
    "oscar"
    "papa"
    "sierra"
    "tango"
    "michael"
    "bravo"
    "hotel"
    "zulu"
    )
  )

(defparameter ins-nato
  (loop
    with building = nil
    for na in NATO-alphabet
    do
    (setf building (wbtrees:ins na building :cmp #'wbtrees:strcmp))
    finally
    (return building)))

(define-test comparison-functions)

(define-test "nato alphabet strcmp function"
  :parent comparison-functions
  (is
    equal
    (wbtrees:foldl ins-nato)
    '("zulu" "yankee" "x-ray" "victor" "tango" "sierra" "romeo" "quebec" "papa"
     "oscar" "michael" "lima" "kilo" "juliet" "india" "hotel" "golf" "foxtrot"
     "echo" "delta" "charlie" "bravo" "alpha" "Victor"))
  (is
    equal
    (wbtrees:foldr ins-nato)
    '("Victor" "alpha" "bravo" "charlie" "delta" "echo" "foxtrot" "golf" "hotel"
     "india" "juliet" "kilo" "lima" "michael" "oscar" "papa" "quebec" "romeo"
     "sierra" "tango" "victor" "x-ray" "yankee" "zulu")))


(defparameter NATO-alphabet-symbol
  '(
    "echo"
    "x-ray"
    "foxtrot"
    "delta"
    "alpha"
    "romeo"
    "india"
    "juliet"
    "Victor"
    "yankee"
    "kilo"
    "lima"
    "charlie"
    "victor"
    "quebec"
    "golf"
    "oscar"
    "papa"
    "sierra"
    "tango"
    "michael"
    "bravo"
    "hotel"
    "zulu"
    )
  )

(define-test "symbolcmp functions"
  :parent comparison-functions
  (true
      (zerop (wbtrees:symbolcmp :x :x 0 0 0)))
  (true
      (zerop (wbtrees:symbolcmp :x :x 15 38 4)))
  (true (< (wbtrees:symbolcmp 'cl:+ ':+ 0 0 0) 0)))


(defparameter an-alist
  '(
    ("echo" . 1)
    ("x-ray" . 2)
    ("foxtrot" . 3)
    ("delta" . 4)
    ("alpha" . 5)
    ("romeo" . 6)
    ("india" . 7)
    ("juliet" . 8)
    ("Victor" . 9)
    ("yankee" . 10)
    ("kilo" . 11)
    ("lima" . 12)
    ("charlie" . 13)
    ("victor" . 14)
    ("quebec" . 15)
    ("golf" . 16)
    ("oscar" . 17)
    ("papa" . 18)
    ("sierra" . 19)
    ("tango" . 20)
    ("michael" . 21)
    ("bravo" . 22)
    ("hotel" . 23)
    ("zulu" . 24)
  ))

(defparameter ins-alist
  (loop
    with building = nil
    with cmpfun = (wbtrees:kvcmp #'wbtrees:strcmp)
    for na in an-alist
    do
    (setf building (wbtrees:ins na building :cmp cmpfun))
    finally
    (return building)))

(define-test "kvcmp"
  :parent comparison-functions
  (is
    equal
    (wbtrees:foldr ins-alist)
    '(("Victor" . 9) ("alpha" . 5) ("bravo" . 22) ("charlie" . 13) ("delta" . 4)
                    ("echo" . 1) ("foxtrot" . 3) ("golf" . 16) ("hotel" . 23) ("india" . 7)
                    ("juliet" . 8) ("kilo" . 11) ("lima" . 12) ("michael" . 21) ("oscar" . 17)
                    ("papa" . 18) ("quebec" . 15) ("romeo" . 6) ("sierra" . 19) ("tango" . 20)
                    ("victor" . 14) ("x-ray" . 2) ("yankee" . 10) ("zulu" . 24)))
  (is
    equal
    (wbtrees:foldl ins-alist)
    '(("zulu" . 24) ("yankee" . 10) ("x-ray" . 2) ("victor" . 14) ("tango" . 20)
                   ("sierra" . 19) ("romeo" . 6) ("quebec" . 15) ("papa" . 18) ("oscar" . 17)
                   ("michael" . 21) ("lima" . 12) ("kilo" . 11) ("juliet" . 8) ("india" . 7)
                   ("hotel" . 23) ("golf" . 16) ("foxtrot" . 3) ("echo" . 1) ("delta" . 4)
                   ("charlie" . 13) ("bravo" . 22) ("alpha" . 5) ("Victor" . 9))))
