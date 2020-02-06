(cl:in-package "https://github.com/g000001/srfi-60#internals")

(def-suite srfi-60)

(in-suite srfi-60)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun number->string (n base)
    (format nil "~VR" base n) ))

(macrolet ((dotests (&body body)
             (do ((tests (reverse body)
                         (cdddr tests) )
                  (es '()
                      (destructuring-bind (test => res &rest ignore)
                                          tests
                        (declare (ignore => ignore))
                        (cons `(TEST ,(gensym)
                                 (IS (EQUAL ,test ,res )))
                              es ))))
                 ((endp tests)
                  `(progn ,@es)) )))
  (dotests
   (number->string (logand #b1100 #b1010) 2) => "1000"
   (number->string (logior #b1100 #b1010) 2) => "1110"
   (number->string (logxor #b1100 #b1010) 2) => "110"
   (number->string (lognot #b10000000) 2) => "-10000001"
   (number->string (lognot #b0) 2) => "-1"
   (logtest #b0100 #b1011) => nil
   (logtest #b0100 #b0111) => t
   (logcount #b10101010) => 4
   (logcount 0) => 0
   (logcount -2) => 1
   (integer-length #b10101010) => 8
   (integer-length 0) => 0
   (integer-length #b1111) => 4
   (log2-binary-factors  0) => -1    (log2-binary-factors 0) => -1
   (log2-binary-factors -1) => 0     (log2-binary-factors 1) => 0
   (log2-binary-factors -2) => 1    (log2-binary-factors 2) => 1
   (log2-binary-factors -3) => 0     (log2-binary-factors 3) => 0
   (log2-binary-factors -4) => 2   (log2-binary-factors 4) => 2
   (log2-binary-factors -5) => 0   (log2-binary-factors 5) => 0
   (log2-binary-factors -6) => 1   (log2-binary-factors 6) => 1
   (log2-binary-factors -7) => 0   (log2-binary-factors 7) => 0
   (log2-binary-factors -8) => 3   (log2-binary-factors 8) => 3
   (log2-binary-factors -9) => 0   (log2-binary-factors 9) => 0
   (log2-binary-factors -10) => 1    (log2-binary-factors 10) => 1
   (log2-binary-factors -11) => 0    (log2-binary-factors 11) => 0
   (log2-binary-factors -12) => 2    (log2-binary-factors 12) => 2
   (log2-binary-factors -13) => 0    (log2-binary-factors 13) => 0
   (log2-binary-factors -14) => 1    (log2-binary-factors 14) => 1
   (log2-binary-factors -15) => 0    (log2-binary-factors 15) => 0
   (log2-binary-factors -16) => 4    (log2-binary-factors 16) => 4
   (logbit? 0 #b1101) => T
   (logbit? 1 #b1101) => nil
   (logbit? 2 #b1101) => T
   (logbit? 3 #b1101) => T
   (logbit? 4 #b1101) => nil
   (number->string (copy-bit 0 0 T) 2)       => "1"
   (number->string (copy-bit 2 0 T) 2)       => "100"
   (number->string (copy-bit 2 #b1111 nil) 2)  => "1011"
   (number->string (bit-field #b1101101010 0 4) 2) => "1010"
   (number->string (bit-field #b1101101010 4 9) 2) => "10110"
   (number->string (copy-bit-field #b1101101010 0 0 4) 2) => "1101100000"
   (number->string (copy-bit-field #b1101101010 -1 0 4) 2) => "1101101111"
   (number->string (copy-bit-field #b110100100010000 -1 5 9) 2)
   => "110100111110000"
   (number->string (ash #b1 3) 2) => "1000"
   (number->string (ash #b1010 -1) 2) => "101"
   (number->string (rotate-bit-field #b0100 3 0 4) 2) => "10"
   (number->string (rotate-bit-field #b0100 -1 0 4) 2) => "10"
   (number->string (rotate-bit-field #b110100100010000 -1 5 9) 2)
   => "110100010010000"
   (number->string (rotate-bit-field #b110100100010000 1 5 9) 2)
   => "110100000110000"
   (number->string (reverse-bit-field #xa7 0 8) 16) => "E5"
   (list->integer (integer->list 3)) => 3
   (booleans->integer t) => 1
   (booleans->integer nil 1) => 1
   (booleans->integer t t t t) => 15
   ))

;;; eof
