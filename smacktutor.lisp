
;;;; smacktutor.lisp

(in-package #:smacktutor)

(defparameter *tutor-functions* nil)


(defun create-lisp-environment ()
  (let* ((env (make-hash-table))
         (smacklisp:*smack-symbols* env))
    (smacklisp:init-smack-interp)
    (mapc #'smacklisp::link-smack-cl-function *tutor-functions*)
    env))

(defvar *tutorials* nil)

(defclass tutorial ()
  ((name :initarg :name :accessor tutorial-name)
   (steps :initform nil :accessor tutorial-steps)))

(defclass tutorial-step ()
  ((question :initarg :question :accessor step-question)
   (answer :initarg :answer :accessor step-answer)
   (hints :initarg :hints :accessor step-hints)
   (recap :initarg :recap :accessor step-recap))
  (:default-initargs
   :question ""
   :answer ""
   :hints  nil
   :recap  ""))


(defclass tutor ()
  (
   (tutorial :initarg :tutorial :accessor tutorial)
   (current-step :initform 0 :accessor current-step)
   (display-step :initform nil :accessor display-step)
   (lisp-env  :initform (create-lisp-environment) :accessor lisp-env))
  )

(defun get-global-tutorial (name)
  (find name *tutorials*
        :key #'tutorial-name
        :test #'string-equal))

(defgeneric tutorial-step-exists (tutorial step)
  (:method ((tutorial tutorial) step)
    (and (<= 0 step)
         (< step (length (tutorial-steps tutorial))))))

(defun print-step-missing (step &optional (destination t))
  (format destination  "Current step ~a does not exist in tutorial~%" (1+ step)))

(defun print-step-number (step &optional (destination t))
  (format destination "Step #~a: " (1+ step)))


(defgeneric display-problem (tutor)
  (:method ((tutor tutor))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (if (tutorial-step-exists tt step)
          (progn (fresh-line)
                 (print-step-number step)
                 (terpri)
                 (princ (step-question (nth step (tutorial-steps tt))))
                 (setf (display-step tutor) step)
                 t)
          (progn (print-step-missing step)
                 nil)))))

(defgeneric display-recap (tutor)
  (:method ((tutor tutor))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (when (equal (display-step tutor) step)
       (if (tutorial-step-exists tt step)
           (when-let (recap (step-recap (nth step (tutorial-steps tt))))
             (terpri)
             (princ recap)
             (terpri)
             t)
           (progn (print-step-missing step)
                  nil))))))

(defgeneric check-problem (tutor)
  (:method ((tutor tutor))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (if (tutorial-step-exists tt step)
          (progn (princ "Check not yet implemented")
                 t)
          (progn (print-step-missing step)
                 nil)))))

(defgeneric answer-problem (tutor)
  (:method ((tutor tutor))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (if (tutorial-step-exists tt step)
          (progn (princ (step-answer (nth step (tutorial-steps tt))))
                 t)
          (progn (print-step-missing step)
                 nil)))))

(defgeneric show-a-hint (tutor)
  (:method ((tutor tutor))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (if (tutorial-step-exists tt step)
          (let ((hints (step-hints (nth step (tutorial-steps tt)))))
            (princ (if hints
                       (nth (random (length hints)) hints)
                       "No hints available."))
            t)
          (progn (print-step-missing step)
                 nil)))))

(defgeneric current-problem (tutor &optional destination)
  (:method ((tutor tutor) &optional (destination t))
    (let ((tt (tutorial tutor))
          (step (current-step tutor)))
      (format destination
              "~a~%~a"
              (tutorial-name tt)
              (print-step-number step nil))
      t)))

(defgeneric goto-step (tutor step not-exists-message)
  (:method ((tutor tutor) step not-exists-message)
    (if (slot-boundp tutor 'tutorial)
        (let ((tut (tutorial tutor)))
          (when (tutorial-step-exists tut (current-step tutor))
              (display-recap tutor))
          (if (tutorial-step-exists tut step)
              (progn 
                     (setf (current-step tutor) step)
                     (display-problem tutor))
              (progn (princ not-exists-message)
                     nil)))
        (progn (princ "Load a tutorial, please.")
               nil))))

(defgeneric next-step (tutor)
  (:method ((tutor tutor))
    (goto-step tutor (1+ (current-step tutor)) "No next step, at tutorial end." )))

(defgeneric prev-step (tutor)
  (:method ((tutor tutor))
    (goto-step tutor (1- (current-step tutor))
               "No previous step, at tutorial beginning." )))


(defgeneric load-tutorial (tutor tutorial-name)
  (:method ((tutor tutor) tutorial-name)
    (when-let ((tut (find tutorial-name *tutorials*
                        :key #'tutorial-name
                        :test #'string-equal)))
      (setf (tutorial tutor) tut
            (current-step tutor) 0)
      t)))

(defparameter *active-tutor* nil)

(defgeneric tutor-repl (tutor))
(defmethod tutor-repl ((tutor tutor))
  (let ((*active-tutor* tutor)
        (smacklisp:*smack-symbols* (lisp-env tutor)))
    (smacklisp:smack)))


(defun api-next ()
  (next-step *active-tutor*))

(defun api-prev ()
  (prev-step *active-tutor*))

(defun api-question ()
  (display-problem *active-tutor*))

(defun api-recap ()
  (display-recap *active-tutor*))

(defun api-check ()
  (check-problem *active-tutor* ))

(defun api-answer ()
  (answer-problem *active-tutor*))

(defun api-hint ()
  (show-a-hint *active-tutor*))

(defun api-current ()
  (current-problem *active-tutor*))

(defun api-goto (index)
  (goto-step *active-tutor* (1- index) "Step does not exist in tutorial"))

(defun api-list-tutorials ()
  (mapcar #'tutorial-name *tutorials*))

(defun api-load-tutorial (tutorial-name)
  (if (load-tutorial *active-tutor* tutorial-name)
    (progn
      (display-problem *active-tutor*)
      t)
    (progn (print "Unknown tutorial") nil)))

(defparameter *tutor-functions*
  '(
    (smacklisp::next api-next)    
    (smacklisp::prev api-prev)
    (smacklisp::question api-question)
    (smacklisp::recap api-recap)
    (smacklisp::check api-check)
    (smacklisp::answer api-answer)
    (smacklisp::hint api-hint)
    (smacklisp::current api-current)
    (smacklisp::goto api-goto)        
    (smacklisp::list-tutorials api-list-tutorials)
    (smacklisp::load-tutorial api-load-tutorial)))





(defparameter tut (make-instance 'tutor))

(defun make-global-tutorial (name)
  (let ((tut (or (get-global-tutorial name)
                 (car (push
                       (make-instance 'tutorial :name name)
                       *tutorials*)))))
    (setf (tutorial-steps tut) nil)
    tut))

(defmacro define-tutorial (name &rest steps)
  (with-gensyms (tut)
    `(let ((,tut (make-global-tutorial,name)))
       ,@(mapcar (lambda (arg)
                   `(appendf (tutorial-steps ,tut)
                             (list (make-instance 'tutorial-step ,@arg))))
                 steps)
       ,tut)))

