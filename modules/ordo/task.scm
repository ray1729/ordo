(define-module (ordo task)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-task task? task-name task-prerequisite-data task-want-skip task-action task-seq))

;; Task
;; name - a descriptive name for the task
;; prerequisite-data - list of prerequisite data (local data
;;                     that must be copied to the remote host
;;                     in order for the task to run)
;; want-skip - function of no args that should return #t if the
;;             the task should be skipped
;; action    - function of no args that runs the task
(define-record-type <task>
  (make-task name prerequisite-data want-skip action)
  task?
  (want-skip task-want-skip)
  (name task-name)
  (prerequisite-data task-prerequisite-data)
  (action task-action))

(define (combine-prerequisite-data tasks)
  ;; TODO: work out what the equality operator should be, which
  ;; will depend on how we represent prerequisite data
  (apply lset-union = (map task-prerequisite-data tasks)))

;; Combine the want-skips functions from a sequence of tasks.
;; If any task has no want-skip function, the combined task cannot
;; be skipped, so simply return #f. Otherwise, return a function that
;; will only return #t if every task's want-skip function returns true.
;; TODO: With this approach, if the top-level want-skip funciton returns
;; #f (so the task action sequence runs), some of the tests will be repeated.
;; Is it preferable always to have the top-level return #f and simply run
;; the subtasks?
(define (combine-want-skips tasks)
  (let ((skips (map task-want-skip tasks)))
    (if (every identity skips)
        (lambda () (every identity (map (lambda (f) (f)) skips)))
        #f)))

;; Return a function that will apply each of the task actions
;; in order.
;; TODO: would it be better to store the list of actions and
;; implement a task runner that would run either a single task
;; or a sequence of tasks with appropriate logging?
;; TODO: the implementation below does not handle skipping
;; tasks in the sequence, this would be handled by a task runner.
(define (combine-actions tasks)
  (let ((actions (map task-action tasks)))
    (lambda ()
      (for-each (lambda (f) (f)) actions))))

;; Return a task consists of a sequence of other tasks.
(define (task-seq name task . task*)
  (let ((tasks (cons task task*)))
    (make-task
     name
     (combine-prerequisite-data tasks)
     (combine-want-skips tasks)
     (combine-actions tasks))))
