(in-package :smacktutor)

(defparameter *lesson-tutorial*
  (define-tutorial "tutorial" 
    (:question "Welcome to the tutorial tutorial.
The tutorial interface uses Lisp functions to navigate the
tutorial and to interface with it.
Call the function NEXT by typing (next)."
     :hints '("(next)" "NEXT inside of a pair of matching parentheses")
     :answer "(next)"
     :recap "")
    (:question "Congratulations you made it to the second step.
To return to the first step, call the function PREV.
Have as much fun as you want moving between the first and second
steps, then go on to the third step."
     :hints '("(prev)" "PREV inside of a pair of matching parentheses")
     :answer "(prev)"
     :recap "")
    (:question "You can also jump to a particular step by using the function GOTO.
GOTO takes one argument which is the step number.
Please use this function to go to step 4."
     :hints '("(goto 4)" "(let ((x 4)) (goto x))")
     :answer "(goto 4)"
     :recap "")
    (:question "The function CURRENT displays the name of the current tutorial
and the current step number. Try it now."
     :hints '("(current)" "(progn (current) (next))")
     :answer "(current)"
     :recap "")
    (:question "The function QUESTION re-displays the text of the current step
in the lesson. Try it now."
     :hints '("(question)")
     :answer "(question)"
     :recap "")
    (:question "The function ANSWER displays the answer to the current step
in the lesson. Try it now."
     :hints '("(answer)")
     :answer "(answer)"
     :recap "")
    (:question "The function HINT displays a random hint for the current step
in the lesson. Try it now."
     :hints '("(hint)" "(HINT)" "(HiNt)")
     :answer "(hint)"
     :recap "")
    (:question "The function CHECK should check if the previously entered
expression matches the  answer to the current step in the lesson.
Unfortunately, this is not yet implemented but you should try it now."
     :hints '("(check)")
     :answer "(check)"
     :recap "")
    (:question "The function LIST-TUTORIALS returns a list of the names of
the available tutorials. Tutorial names are strings.
Strings in Lisp use double quotes and not single quotes as delimiters.
Note that this is the only function of the tutorial interface that
returns something other than T or NIL. Try it now."
     :hints '("(list-tutorials)")
     :answer "(list-tutorials)"
     :recap "")
    (:question "The function LOAD-TUTORIAL loads a tutorial.
LOAD-TUTORIAL takes a single parameter which is the name of the
tutorial. Tutorial names are strings. Strings in Lisp use
double quotes and not single quotes as delimiters.
Try it now by loading the \"repl\" tutorial."
     :hints '("(load-tutorial \"tutorial-name-here\")"
              "(load-tutorial \"repl\")")
     :answer "(load-tutorial \"repl\")"
     :recap "")))
