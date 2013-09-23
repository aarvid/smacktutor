(in-package :smacktutor)

(defparameter *lesson-repl*
  (define-tutorial "repl"
      (:question "Welcome to the repl tutorial.
Repl stands for read, evaluate, print, loop.
This basically describes what the interface does.
Read an expression the user inputs.  Evaluates the expression.
Prints out the result of the evaluation.
Then loops back to receive more input.

Try evaluating the number 42"
       :hints '("42")
       :answer "42"
       :recap "")
    (:question "Notice that almost everything in lisp is an expression.
So the repl is a natural place to evaluate these expressions.
Constants are expressions.  

Try evaluating the lisp expression PI"
     :hints '("pi")
     :answer "pi"
     :recap "")
    (:question "Lisp uses prefix notation so (+ 1 2) is an expression.
Evaluate this please."
     :hints '("(+ 1 2)")
     :answer "(+ 1 2"
     :recap "")
    (:question "The REPL has several special variables
referring to previous evaluations.
* is a variable and holds the last value returned in the repl.
To be clear, * is both a variable and a function (multiplication).
Evaluate (* 2 3) again. Then evaluate *"
     :hints '("*")
     :answer "*"
     :recap "")
    (:question "** is a variable and holds the second to last value returned.
Evaluate (* 2 3), then evaluate (+ 3 4).  finally evaluate **."
     :hints '("**")
     :answer "**"
     :recap "")
    (:question "*** is a variable and holds the third to last value returned.
Evaluate (* 1 2), then evaluate (* 3 4), then evaluate (* 5 6).
Finally multiply the last three returned values."
     :hints '("(* * ** ***)")
     :answer "(* * ** ***)"
     :recap "")
    (:question "+ is a variable that holds the last expression after it has
been read by the REPL reader.
Note: like *, + is both a variable and a function.
Evaluate (+ 1 2 3 4). then evaluate + "
     :hints '("+")
     :answer "+"
     :recap "")
    (:question
     "Note that + does not hold what you typed, but holds the
reader's parsing into an s-expression of what you typed before
it is evaluated.
Evaluate (+ 1
            2
            3)
Then evaluate +. "
     :hints '("+")
     :answer "+"
     :recap "")
    (:question "++ holds the second to last expression.  Try it now."
     :hints '("++")
     :answer "++"
     :recap "")
    (:question "+++ holds the third to last expression.  Try it now."
     :hints '("+++")
     :answer "+++"
     :recap "")
    (:question "- is a variable that holds the current expression.
Try it now."
     :hints '("-")
     :answer "-"
     :recap "")
    (:question "- by itself is not very interesting.  But you can use
- to create self referential expressions. for example, try (first -)
and (second -)"
     :hints '("(first -)" "(second -)")
     :answer "(first -)"
     :recap "")
    (:question "Here is a more complicated self-referential expression:
 (and (print -) (- 4 3 2 1))
Note that this also shows that - is both a function and a variable.
Try it now."
     :hints '("(and (print -) (- 4 3 2 1))")
     :answer "(and (print -) (- 4 3 2 1))"
     :recap "")
    (:question "For the next three variables, you will need to understand
that a Lisp function or expression can return multiple values.
VALUES is a function that returns each argument as a value.
Try (values 1 2 3)"
     :hints '("(values 1 2 3)")
     :answer "(values 1 2 3)"
     :recap "")
    (:question "Now the variable / returns a list of the multiple values
returned.
Evaluate (values 1 2 3), then /, then **.
Notice the difference."
     :hints '("/")
     :answer "/"
     :recap "")
    (:question "As with * and +, / has sister variables // and ///.
// holds the list of values returned by the second to last expression.
Evaluate (floor pi), then *, then //"
     :hints '("//")
     :answer "//"
     :recap "")
    (:question "/// holds the list of values returned by the third to last expression.
Evaluate (floor pi), then pi, then 42 then ///"
     :hints '("///")
     :answer "///"
     :recap "")
    ))


#|(:question ""
 :hints '("")
 :answer "")|#

