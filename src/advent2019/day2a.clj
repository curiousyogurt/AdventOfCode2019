;;;;
;;;; Advent of Code 2019: Day 2, Part 1
;;;; https://adventofcode.com/2019/day/2
;;;;
;;;; 1202 Program Alarm
;;;;
(ns day2a.n)

;;;
;;; Load in the data for the problem as a vector
;;;
(def input-data
  (mapv read-string
        (re-seq #"[\d.]+" (slurp "resources/day2.txt"))))

;;;
;;; Given a sequence of 4 integers, return the required operation as a function.
;;; Opcode 1 is addition; opcode 2 is multiplication.  Any other opcode returns
;;; nil.
;;;
(defn op
  [intcode position]
  (let [opcode (get intcode position)]
    (cond
      (= opcode 1) +
      (= opcode 2) *
      :else nil)))

;;;
;;; Pull an individual instruction (4 integers) from the intcode, starting at
;;; position.  The pattern is:
;;;
;;;   1: opcode (submit to op to get a function)
;;;   2: position in intcode of first value
;;;   3: position in intcode of second value
;;;   4: position for output
;;;
(defn execute
  [intcode position]
  (let [operation (op intcode position)
        val1 (get intcode (get intcode (+ position 1)))
        val2 (get intcode (get intcode (+ position 2)))
        out (get intcode (+ position 3))]
    (when-not (nil? operation)
      (assoc intcode out (operation val1 val2)))))

;;;
;;; Run the intcode.  Do this by starting at specified position.  If we get nil
;;; as a result, either we have hit opcode 99 (halt), or we have an unknown
;;; opcode; halt in either case.  Step through intcode 4 positions at a time.
;;;
(defn run
  [intcode position]
  (loop [intcode intcode
         position position]
    (let [result (execute intcode position)]
      (if (nil? result)
        intcode
        (recur result (+ position 4))))))

;;;
;;; Prep the intcode by replacing position 1 with the value 12, and position
;;; 2 with the value 2.
;;;
(defn prep [intcode]
  (assoc (assoc intcode 1 12) 2 2))

;;;
;;; Run the intcode starting at position 0, and return the value at position 0.
;;;
(first (run (prep input-data) 0))
