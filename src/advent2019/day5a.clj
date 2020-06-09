;;;;
;;;; Advent of Code 2019: Day 5, Part 1
;;;; https://adventofcode.com/2019/day/5
;;;;
;;;; Sunny with a Chance of Asteroids
;;;;

;;;
;;; Load in the data for the problem as a vector
;;;
(def input-data
  (mapv read-string
        (re-seq #"[\d.]+" (slurp "resources/day5.txt"))))

(def input 1)
(def output 1)

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
      (= opcode 3) input
      (= opcode 4) output
      :else nil)))

;;;
;;; Given an integer, break it into its constituent digits.  Convert to a
;;; string with `(str number)`, then use a nifty trick of ASCII codes (I think
;;; credit for this trick goes to whoever wrote a certain program in the
;;; user manual for the Commodore VIC-20; there was a similar trick there
;;; using ASCII math: subtract the ASCII value of the number in question
;;; from the ASCII value for \0 (which is 48), and you get the digit itself.
;;; For example, \5 has an ASCII value of 53; so when we calculate 53 minus
;;; 48, we get 5.
;;;
(defn breakout
  [number]
  (mapv #(- (int %) (int \0))
       (str number)))

;;;
;;; If `parameter-mode` is nil, set to 0; otherwise, leave it
;;;
(defn coerce
  [parameter-mode]
  (if (nil? parameter-mode) 0 parameter-mode))

;;;
;;; Return the number of parameters requried for a given opcode
;;;
(defn parameters
  [opcode]
    (cond
      (= opcode 1) 3
      (= opcode 2) 3
      (= opcode 3) 1
      (= opcode 4) 1
      :else nil))

;;;
;;; Given the intcode, a position, and the number of parameters required,
;;; return a map of the first/second/third parameters.
;;;
(defn get-parameters
  [intcode position parameters]
  (cond
    (= parameters 1) {:first-parameter (nth intcode (+ position 1))
                      :second-parameter nil
                      :third-parametr nil}
    (= parameters 2) {:first-parameter (nth intcode (+ position 1))
                      :second-parameter (nth intcode (+ position 2))
                      :third-parameter nil}
    (= parameters 3) {:first-parameter (nth intcode (+ position 1))
                      :second-parameter (nth intcode (+ position 2))
                      :third-parameter (nth intcode (+ position 3))}))

;;;
;;; Given the intcode and a position, return a map of the opcode, the number
;;; of parameters requried, and the mode for the first/second/third parameters,
;;; where 0 is position mode, and 1 is immediate mode.
;;;
(defn operation
  [intcode position]
  (let [op (breakout (nth intcode position))
        opcode (+ (* (last (butlast op)) 10) (last op))]
    {:opcode opcode
     :parameters (parameters opcode)
     :first-parameter-mode (coerce (last (butlast (butlast op))))
     :second-parameter-mode (coerce (last (butlast (butlast (butlast op)))))
     :third-parameter-mode (coerce (last (butlast (butlast (butlast (butlast op))))))}))

;;;
;;; Given the intcode and a position, return a map of the operation (opcode,
;;; number of parameters requried, mode for first/second/third parameters and
;;; the first/second/third parameters.
;;;
(defn instruction
  [intcode position]
  (let [op (operation intcode position)
        parameters (get-parameters intcode position (:parameters op))]
    (into op parameters)))

(instruction [1002 4 3 4 33] 0)

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
