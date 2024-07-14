(define-module (ordo task)
  #:use-module (srfi srfi-9)
  #:export (make-task task? task-name task-prerequisite-data set-task-prerequisite-data! task-action set-task-action!))

(define-record-type <task>
  (make-task name prerequisite-data action)
  task?
  (name task-name)
  (prerequisite-data task-prerequisite-data set-task-prerequisite-data!)
  (action task-action set-task-action!))
