;;;; srfi-60.lisp

(cl:in-package :srfi-60.internal)

;;;; "logical.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'arithmetic-shift)
        #'cl:ash))

(define-symbol-macro |LOGICAL:BOOLE-XOR|
 '#(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    #(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14)
    #(2 3 0 1 6 7 4 5 10 11 8 9 14 15 12 13)
    #(3 2 1 0 7 6 5 4 11 10 9 8 15 14 13 12)
    #(4 5 6 7 0 1 2 3 12 13 14 15 8 9 10 11)
    #(5 4 7 6 1 0 3 2 13 12 15 14 9 8 11 10)
    #(6 7 4 5 2 3 0 1 14 15 12 13 10 11 8 9)
    #(7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8)
    #(8 9 10 11 12 13 14 15 0 1 2 3 4 5 6 7)
    #(9 8 11 10 13 12 15 14 1 0 3 2 5 4 7 6)
    #(10 11 8 9 14 15 12 13 2 3 0 1 6 7 4 5)
    #(11 10 9 8 15 14 13 12 3 2 1 0 7 6 5 4)
    #(12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3)
    #(13 12 15 14 9 8 11 10 5 4 7 6 1 0 3 2)
    #(14 15 12 13 10 11 8 9 6 7 4 5 2 3 0 1)
    #(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))

(define-symbol-macro |LOGICAL:BOOLE-AND|
 '#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
    #(0 0 2 2 0 0 2 2 0 0 2 2 0 0 2 2)
    #(0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)
    #(0 0 0 0 4 4 4 4 0 0 0 0 4 4 4 4)
    #(0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5)
    #(0 0 2 2 4 4 6 6 0 0 2 2 4 4 6 6)
    #(0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7)
    #(0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8)
    #(0 1 0 1 0 1 0 1 8 9 8 9 8 9 8 9)
    #(0 0 2 2 0 0 2 2 8 8 10 10 8 8 10 10)
    #(0 1 2 3 0 1 2 3 8 9 10 11 8 9 10 11)
    #(0 0 0 0 4 4 4 4 8 8 8 8 12 12 12 12)
    #(0 1 0 1 4 5 4 5 8 9 8 9 12 13 12 13)
    #(0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14)
    #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

(defun |LOGICAL:ASH-4| (x)
  (if (minusp x)
      (+ -1 (truncate (+ 1 x) 16))
      (truncate x 16)))

(declaim (ftype (function (function integer) function)
                |LOGICAL:REDUCE|))

(defun |LOGICAL:REDUCE| (op4 ident)
  (declare (optimize (safety 0) (speed 3) (space 3)))
  (lambda (&rest args)
    (do ((res ident (funcall op4 res (car rgs) 1 0))
	 (rgs args (cdr rgs)))
	((null rgs) res))))

;@
#+sbcl
(defun logand (&rest args)
  (declare (optimize (space 3))
           (inline |LOGICAL:REDUCE|))
  (labels ((lgand (n2 n1 scl acc)
             (declare (integer n2 n1))
             (cond ((= n1 n2) (+ acc (* scl n1)))
                   ((zerop n2) acc)
                   ((zerop n1) acc)
                   (:else (lgand (|LOGICAL:ASH-4| n2)
                                 (|LOGICAL:ASH-4| n1)
                                 (* 16 scl)
                                 (+ (* (svref (svref |LOGICAL:BOOLE-AND|
                                                     (mod n1 16) )
                                              (mod n2 16) )
                                       scl )
                                    acc ))))))
    (apply (|LOGICAL:REDUCE| #'lgand -1)
           args) ))

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'logand) #'cl:logand))

;@
#+sbcl
(defun logior (&rest args)
  (declare (inline |LOGICAL:REDUCE|))
  (labels ((lgior (n2 n1 scl acc)
             (cond ((= n1 n2) (+ acc (* scl n1)))
                   ((zerop n2) (+ acc (* scl n1)))
                   ((zerop n1) (+ acc (* scl n2)))
                   (:else (lgior (|LOGICAL:ASH-4| n2)
                                 (|LOGICAL:ASH-4| n1)
                                 (* 16 scl)
                                 (+ (* (- 15 (svref
                                              (svref |LOGICAL:BOOLE-AND|
                                                          (- 15 (mod n1 16)))
                                              (- 15 (mod n2 16))))
                                       scl)
                                    acc))))))
    (apply (|LOGICAL:REDUCE| #'lgior 0)
           args )))

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'logior) #'cl:logior))

;@
#+sbcl
(defun logxor (&rest args)
  (labels ((lgxor (n2 n1 scl acc)
             (cond ((= n1 n2) acc)
                   ((zerop n2) (+ acc (* scl n1)))
                   ((zerop n1) (+ acc (* scl n2)))
                   (:else (lgxor (|LOGICAL:ASH-4| n2)
                                 (|LOGICAL:ASH-4| n1)
                                 (* 16 scl)
                                 (+ (* (svref (svref |LOGICAL:BOOLE-XOR|
                                                     (mod n1 16) )
                                              (mod n2 16) )
                                       scl )
                                    acc ))))))
    (apply (|LOGICAL:REDUCE| #'lgxor 0)
           args)) )

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function 'logxor) #'cl:logxor))

;@
(defun lognot (n) (- -1 n))
;@
(defun logtest (n1 n2)
  (not (zerop (logand n1 n2))))
;@
(defun logbit? (index n)
  (logtest (expt 2 index) n))
;@
(defun copy-bit (index to bool)
  (if bool
      (logior to (arithmetic-shift 1 index))
      (logand to (lognot (arithmetic-shift 1 index)))))
;@
(defun bitwise-if (mask n0 n1)
  (logior (logand mask n0)
	  (logand (lognot mask) n1)))
;@
(defun bit-field (n start end)
  (logand (lognot (arithmetic-shift -1 (- end start)))
	  (arithmetic-shift n (- start))))
;@
(defun copy-bit-field (to from start end)
  (bitwise-if (arithmetic-shift (lognot (arithmetic-shift -1 (- end start))) start)
	      (arithmetic-shift from start)
	      to))
;@
(defun rotate-bit-field (n count start end)
  (let ((width (- end start)))
    (setq count (mod count width))
    (let ((mask (lognot (arithmetic-shift -1 width))))
      (let ((zn (logand mask (arithmetic-shift n (- start)))))
        (logior (arithmetic-shift
                 (logior (logand mask (arithmetic-shift zn count))
                         (arithmetic-shift zn (- count width)) )
                 start )
                (logand (lognot (arithmetic-shift mask start)) n) )))))
;@
(defun ash (n count)
  (if (minusp count)
      (let ((k (expt 2 (- count))))
	(if (minusp n)
	    (+ -1 (floor (+ 1 n) k))
	    (floor n k)))
      (* (expt 2 count) n)))
;@
(defun integer-length (n)
  (labels ((intlen (n tot)
             (case n
               ((0 -1) (+ 0 tot))
               ((1 -2) (+ 1 tot))
               ((2 3 -3 -4) (+ 2 tot))
               ((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
               (otherwise (intlen (|LOGICAL:ASH-4| n) (+ 4 tot))))))
    (intlen n 0)))
;@
(defun bitwise-bit-count (n)
  (labels ((logcnt (n tot)
             (if (zerop n)
                 tot
                 (logcnt (floor n 16)
                         (+ (svref
                             '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                             (mod n 16))
                            tot)))))
    (cond ((minusp n) (lognot (logcnt (lognot n) 0)))
          ((plusp n) (logcnt n 0))
          (:else 0))))
;@
(defun logcount (n)
  (cond ((minusp n) (bitwise-bit-count (lognot n)))
	(:else (bitwise-bit-count n))))
;@
(defun log2-binary-factors (n)
  (+ -1 (integer-length (logand n (- n)))))

(defun bit-reverse (k n)
  (do ((m (if (minusp n) (lognot n) n) (ash m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (logior (ash rvs 1) (logand 1 m))))
      ((minusp k) (if (minusp n) (lognot rvs) rvs))))
;@
(defun reverse-bit-field (n start end)
  (let ((width (- end start)))
    (let ((mask (lognot (ash -1 width))))
      (let ((zn (logand mask (ash n (- start)))))
        (logior (ash (bit-reverse width zn) start)
                (logand (lognot (ash mask start)) n) )))))
;@
(defun integer->list (k &rest len)
  (if (null len)
      (do ((k k (ash k -1))
	   (lst '() (cons (oddp k) lst)) )
	  ((<= k 0) lst) )
      (do ((idx (+ -1 (car len)) (+ -1 idx))
	   (k k (ash k -1))
	   (lst '() (cons (oddp k) lst)) )
	  ((minusp idx) lst) )))
;@
(defun list->integer (bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null bs) acc)))
(defun booleans->integer (&rest bools)
  (list->integer bools))

;;;;@ SRFI-60 aliases
(macrolet ((define (new old)
             `(setf (symbol-function ',new)
                    (function ,old))))
  (define arithmetic-shift ash)
  (define bitwise-ior logior)
  (define bitwise-xor logxor)
  (define bitwise-and logand)
  (define bitwise-not lognot)
  (define bit-count logcount)
  (define bit-set?   logbit?)
  (define any-bits-set? logtest)
  (define first-set-bit log2-binary-factors)
  (define bitwise-merge bitwise-if))

;;; Legacy
;;(define (logical:rotate k count len) (rotate-bit-field k count 0 len))
;;(define (logical:ones deg) (lognot (ash -1 deg)))
;;(define integer-expt expt)		; legacy name


