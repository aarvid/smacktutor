(in-package :smacktutor)

(defparameter *lesson-newbie*
  (define-tutorial "newbie"
      (:question "Welcome to the Newbie tutorial.
This tutorial should introduce you to the basic lisp concepts,
types and syntax.  Evaluate (next) to continue."
       :hints nil
       :answer "(next)"
       :recap "")
    (:question "Lisp has numbers for everyone.
There are integers like 0, 1 and 42. Evaluate 42"
     :hints '("42")
     :answer "42"
     :recap "Notice that a number evaluates to itself.")
    (:question "Integers can be negative. Evaluate -666."
     :hints '("-666")
     :answer "-666"
     :recap "")
    (:question "Integers can also be really really big.
Evaluate the mass of the earth in kg:
5972000000000000000000000"
     :hints '("5972000000000000000000000" "(* 5972 (expt 10 21))")
     :answer "5972000000000000000000000"
     :recap "")
    (:question "Lisp also has floating point or real numbers.
3.14159 is a floating point with single precision.
Evaluate it."
     :hints '("3.14159")
     :answer "3.14159"
     :recap "")
    (:question "Floating points can also have exponents.
Evaluate Avogadro constant 6.02214129e23"
     :hints '("6.02214129e23")
     :answer "6.02214129e23"
     :recap "Notice that lisp automatically altered the result to the
nearest single precision float.")
    (:question "When printing with an exponent, it will
also always print the result in standard scientific notation.
Evaluate 602214129e15"
     :hints '("602214129e15")
     :answer "602214129e15"
     :recap "")
    (:question "PI is a constant with double precision.
Evaluate it."
     :hints '("PI")
     :answer "PI"
     :recap "Notice the \"d0\".
The \"d\" means double precision.
The \"0\" means it has exponent zero.
Ergo, \"d\" functions like \"e\" for single precision
but also is used to indicate double precision. ")
    (:question "Exponents can be negative. Evaluate 3141592653589793d-15" 
     :hints '("3141592653589793d-15")
     :answer "3141592653589793d-15"
     :recap "Notice that this was PI in a non-standard format
but the REPL will always print in standard format. ")))

#|(:question ""
 :hints '("")
 :answer "")|#



