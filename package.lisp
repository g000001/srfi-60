;;;; package.lisp

(cl:in-package :cl-user)

(defpackage "https://github.com/g000001/srfi-60"
  (:use)
  (:export
   logand bitwise-and logior bitwise-ior logxor
   bitwise-xor lognot bitwise-not bitwise-if bitwise-merge
   logtest any-bits-set? logcount bit-count integer-length
   log2-binary-factors first-set-bit logbit? bit-set? copy-bit
   bit-field copy-bit-field ash arithmetic-shift rotate-bit-field
   reverse-bit-field integer->list integer->list list->integer
   booleans->integer))

(defpackage "https://github.com/g000001/srfi-60#internals"
  (:use "https://github.com/g000001/srfi-60"
        :cl
        :fiveam)
  (:shadowing-import-from "https://github.com/g000001/srfi-60"
                          logand
                          logior
                          logxor
                          lognot
                          logtest
                          ash
                          integer-length
                          logcount ))

