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

;;;
;;; Arbitrary assicgnment of input and output, according to puzzle.
;;;
(def input 1)
(def output 1)

;;;
;;; Given the opcode, return the operation as a function.  These are arbitrary,
;;; so `cond` is appropriate.
;;;
(defn op
  [opcode]
  (cond
    (= opcode 1) +
    (= opcode 2) *
    (= opcode 3) (fn [n] input)
    (= opcode 4) (fn [n] output)
    :else nil))

;;;
;;; Return the number of parameters requried for a given opcode.  These are
;;; arbitrary, and so `cond` is appropriate.
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
;;; Given the intcode and a position, return a map of the opcode, the number
;;; of parameters requried, and the mode for the first/second/third parameters,
;;; where 0 is position mode, and 1 is immediate mode.  The following let
;;; assignments are of interest:
;;;
;;; `op`: split the operation into a vector of individual digits
;;; `last-op`: pick out the last two digits of op
;;; `opcode`: convert last-op to an integer
;;; `coerce`: (fn) corverts a nil parameter to 0
;;;
;;; The output of this function is a map with the parsed operation.  use this
;;; together with `parse-parameters` to generate a map of the instruction.
;;;
(defn parse-operation
  [operation]
  (let [op (mapv #(- (int %) (int \0)) (str operation))
        last-op (str (last (butlast op)) (last op))
        opcode (Integer/parseInt (first (re-find #"(\d){1,2}$" last-op)))
        coerce #(if (nil? %) 0 %)]
    {:opcode opcode
     :parameters (parameters opcode)
     :first-parameter-mode (coerce (last (butlast (butlast op))))
     :second-parameter-mode (coerce (last (butlast (butlast (butlast op)))))
     :third-parameter-mode (coerce (last (butlast (butlast (butlast (butlast op))))))}))

;;;
;;; Given the parameters, create a map.  Use this together with `parse-
;;; operation` to generate a map of the instruction.
;;;
(defn parse-parameters
  [parameters]
  {:first-parameter (first parameters)
   :second-parameter (first (next parameters))
   :third-parameter (first (next (next parameters)))})

;;;
;;; Given the intcode and a position, return a map of the instruction (opcode,
;;; number of parameters, mode for first/second/third parameters and the first/
;;; second/third parameters.
;;;
(defn instruction
  [intcode position]
  (let [operation (parse-operation (nth intcode position))
        parameters (parse-parameters (subvec intcode (inc position) (+ (inc position) (:parameters operation))))]
    (into operation parameters)))

;(instruction [1101,100,-1,4,0] 0)

;;; TODO: Finish Day 5.  Below is from Day 2.

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
