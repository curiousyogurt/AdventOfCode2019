;;;; Advent of Code 2019: Day 5, Part 1
;;;; https://adventofcode.com/2019/day/5
;;;;
;;;; Sunny with a Chance of Asteroids
;;;;
(ns day5a.ns)

;;;
;;; Load in the data for the problem as a vector
;;;
(def input-data
  (mapv read-string
        (re-seq #"-?[\d.]+" (slurp "resources/day5.txt"))))

;;;
;;; Arbitrary assignment of input value, according to puzzle description.
;;;
(def input 1)

;;;
;;; Below are the implementations of the opcodes defined for this puzzle.
;;; Each returns a map with three values:
;;;   intcode: The intcode after the opertaion
;;;   position: The position of the pointer after the operation
;;;   halt: Whether to halt or not (as a boolean)
;;;

;; "Opcode 1 adds together numbers read from two positions and stores the result
;; in a third position."
(defn opcode-1
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:third-parameter instruction)
                   (+ (:first-value instruction) (:second-value instruction)))
   :position (+ position 4)
   :halt false})

;; "Opcode 2 works exactly like opcode 1, except it multiplies the two inputs
;; instead of adding them."
(defn opcode-2
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:third-parameter instruction)
                   (* (:first-value instruction) (:second-value instruction)))
   :position (+ position 4)
   :halt false})

;; "Opcode 3 takes a single integer as input and saves it to the position given
;; by its only parameter."
(defn opcode-3
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:first-parameter instruction)
                   input)
   :position (+ position 2)
   :halt false})

;; "Opcode 4 outputs the value of its only parameter."
(defn opcode-4
  [intcode position instruction]
  (println (:first-value instruction))
  {:intcode intcode
   :position (+ position 2)
   :halt false})

;; "[Opcode] 99 means that the program is finished and should immediately halt."
(defn opcode-99
  [intcode position]
  {:intcode intcode
   :position position
   :halt true})

;;;
;;; Return the number of parameters expected for a given opcode.
;;;
(defn parameters
  [opcode]
  (cond
    (= opcode 1) 3
    (= opcode 2) 3
    (= opcode 3) 1
    (= opcode 4) 1
    (= opcode 99) 0
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
;;; Given the incode, operation, and parameters, pull in the values for each
;;; paramater, as appropriate, depending on whether the parameter mode is
;;; immediate (0) or position (1)
;;;
(defn get-values
  [intcode operation parameters]
  {:first-value (if (zero? (:first-parameter-mode operation))
                  (get intcode (:first-parameter parameters))
                  (:first-parameter parameters))
   :second-value (if (zero? (:second-parameter-mode operation))
                   (get intcode (:second-parameter parameters))
                   (:second-parameter parameters))
   :third-value (if (zero? (:third-parameter-mode operation))
                  (get intcode (:third-parameter parameters))
                  (:third-parameter parameters))})

;;;
;;; Given the intcode and a position, return a map of the instruction (opcode,
;;; number of parameters, mode for first/second/third parameters and the first/
;;; second/third parameters.
;;;
(defn parse-instruction
  [intcode position]
  (let [operation (parse-operation (nth intcode position))
        parameters (parse-parameters
                     (subvec intcode (inc position)
                             (+ (inc position) (:parameters operation))))
        values (get-values intcode operation parameters)]
    (into operation (into parameters values))))

;;;
;;; Execute a single instruction by calling the corresponding opcode-x function.
;;;
(defn execute-instruction
  [intcode position]
  (let [instruction (parse-instruction intcode position)
        result (cond
                 (= (:opcode instruction) 1) (opcode-1 intcode position instruction)
                 (= (:opcode instruction) 2) (opcode-2 intcode position instruction)
                 (= (:opcode instruction) 3) (opcode-3 intcode position instruction)
                 (= (:opcode instruction) 4) (opcode-4 intcode position instruction)
                 (= (:opcode instruction) 99) (opcode-99 intcode position))]
    {:intcode (:intcode result)
     :position (inc (+ position (:parameters instruction)))
     :halt (:halt result)}))

;;;
;;; Begininng at position 0, step through the intcode, one instruction at a time.
;;;
(defn run
  [intcode]
  (loop [intcode intcode
         position 0]
    (let [result (execute-instruction intcode position)]
      (if-not (:halt result)
        (recur (:intcode result) (:position result))
        'halt))))

(run input-data)
