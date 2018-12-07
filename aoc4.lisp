;;;; day 4 answer.

;;; As much as I'm enjoying Lisp for this, I feel like I could have written
;;; this in Perl in like a third of the time.  Part of the problem is that I'm
;;; more likely to be careful with Lisp, and just hack the hell out of Perl.

;;; This isn't that nice, either, of course.  It is both over- and
;;; under-engineered.  In particular, it assumes that the guard will always be
;;; awake at 00:00 and by 00:59, a nice property of the data.  Of course, I
;;; spent time looking into the time library before I just did the subtraction
;;; carelessly.

;;; The more I use the loop macro the less I like it.  It wouldn't be so bad if
;;; it had consistent syntax, but hash tables, lists, and arrays all have
;;; different syntaxes, and the one for the hash table is not memorable.

(defpackage :aoc4
  (:use :common-lisp :cl-ppcre)
  (:export :read-input :ppcre-register-groups-bind))

(in-package :aoc4)

(defvar *input-file* "/home/tjs/git/aoc2018/4.input")

(defun load-sort-input-lines ()
  (sort (ioutil:snarf-file *input-file*) #'string<))


(defstruct event
  (month nil :type integer :read-only t)
  (day nil :type integer :read-only t)
  (hour nil :type integer :read-only t)
  (minute nil :type integer :read-only t)
  (what nil :type string  :read-only t))

(defconstant +line-scanner+
  (cl-ppcre:create-scanner
   "\\[(\\d{4})-(\\d\\d)-(\\d\\d) (\\d\\d):(\\d\\d).*\\] (.*)"))

(defconstant +guard-scanner+
  (cl-ppcre:create-scanner
   "Guard #(\\d+) begins shift"))

(defun scan-line (s)
  (ppcre:register-groups-bind
      (y (#'parse-integer m d h mm) (#'parse-string what))
      (+line-scanner+ s :sharedp t)
    (check-type y string)
    (make-event :month m
                   :day d
                   :hour h
                :minute mm
                :what what)))

(defun guard-id-from-what (w)
  (ppcre:register-groups-bind
      ((#'parse-integer id))
      (+guard-scanner+ w :sharedp t)
    id))

(defun parse-lines (lines)
  (mapcar #'scan-line lines))

(defvar *loaded-events*
  (parse-lines (load-sort-input-lines)))

(defun sleep-event-p (e)
  (string= (event-what e) "falls asleep"))

(defun wake-event-p (e)
  (string= (event-what e) "wakes up"))

(defun events-to-minutes-hash (events)
  (let ((id-to-minutes (make-hash-table)))
    (loop :with guard-id = nil
          :with asleep-at = nil
          :for i = 0
          :for e :in events :do
            (cond
              ((let ((gid (guard-id-from-what (event-what e))))
                 (when gid
                   (assert (not asleep-at))
                   (setf guard-id gid)))
               t)
              ((sleep-event-p e)
               (progn (assert (not asleep-at))
                      (setf asleep-at (event-minute e))))
              ((wake-event-p e)
               (progn (assert asleep-at)
                      (incf (gethash guard-id id-to-minutes 0)
                            (- (event-minute e) asleep-at))
                      (format t "guard ~a asleep ~a..~a, ttl ~a~%"
                              guard-id
                              asleep-at
                              (event-minute e)
                              (gethash guard-id id-to-minutes))
                      (setf asleep-at nil)))
              (t (assert nil))))
    id-to-minutes))

(defun worst-guard (h)
  "Worst guard (most likely to be asleep) from everyone in the hash-table h."
  (check-type h hash-table)
  (let ((worst-guard-id nil)
        (most-minutes 0))
    (loop :for id :being :the :hash-keys :of h
            :using (hash-value minutes)
          :do (format t "guard ~a asleep ~a~%"
                      id minutes)
          :do (when (> minutes most-minutes)
                (setf worst-guard-id id)
                (setf most-minutes minutes)))
    (format t "worst guard ~a~%" worst-guard-id)
    (format t "minutes ~a~%" most-minutes)
    worst-guard-id))

(defun select-worst-minute (minutes)
  (check-type minutes (array *))
  (let ((best-minute -1))
    (loop 
      :with best-hits := 0
      :for i :from 0 :to (1- (length minutes)) :do
        (when (> (aref minutes i) best-hits)
          (setf best-minute i)
          (setf best-hits (aref minutes i))))
    (format t "minutes=~a~%" minutes)
    best-minute))
  
;; this returns the answer to part one
(defun worst-minute (id events)
  (let ((minutes (make-array 60 :initial-element 0)))
    (loop :with looking-at-guard-id = nil
          :with asleep-at = nil
          :for e :in events :do
            (cond
              ((let ((gid (guard-id-from-what (event-what e))))
                 (when gid
                   (format t "guard ~a~%" gid)
                   (assert (not asleep-at))
                   (setf looking-at-guard-id gid)))
               t)
              ((and (sleep-event-p e))
               (when (= id looking-at-guard-id) 
                 (assert (not asleep-at))
                 (format t "guard ~a is asleep at ~a ~%" id e)
                 (setf asleep-at (event-minute e))))
              ((and (wake-event-p e))
               (when (= id looking-at-guard-id)
                 (assert asleep-at)
                 (format t "guard ~a is awake at ~a ~%" id e)
                 (loop :for i :from asleep-at
                         :to (1- (event-minute e)) :do
                           (incf (aref minutes i)))
                 (setf asleep-at nil)))
              (t (assert nil))))
    (select-worst-minute minutes)))

(defun events-to-part2-answer (events)
  (let ((id-to-minutes-array (make-hash-table)))
    (loop :for e :in events :do
      (let ((gid (guard-id-from-what (event-what e))))
        (when gid
          (setf (gethash gid id-to-minutes-array)
                (make-array 60 :initial-element 0)))))

    (loop :with guard-id = nil
          :with asleep-at = nil
          :for i = 0
          :for e :in events :do
            (cond
              ((let ((gid (guard-id-from-what (event-what e))))
                 (when gid
                   (assert (not asleep-at))
                   (setf guard-id gid)))
               t)
              ((sleep-event-p e)
               (progn (assert (not asleep-at))
                      (setf asleep-at (event-minute e))))
              ((wake-event-p e)
               (progn
                 (assert asleep-at)
                 (loop
                   :with a = (gethash guard-id id-to-minutes-array)
                   :for i :from asleep-at :to (1- (event-minute e)) :do
                     (incf (aref a i))
                   :do (setf asleep-at nil))))))

    (let ((sleepyhead -1)
          (sleepyminute -1)
          (amt -1))

      (maphash (lambda (guard-id a)
                 (loop
                   :for i from 0
                   :for zz :across a :do
                     (when (> zz amt)
                       (setf amt zz)
                       (setf sleepyhead guard-id)
                       (setf sleepyminute i)))
                 id-to-minutes-array))

      (format t "sleepyhead=~a sleepyminute=~a amt=~a~%"
              sleepyhead sleepyminute amt)
      (* sleepyhead sleepyminute))))
