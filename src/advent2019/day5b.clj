;;;;
;;;; Advent of Code 2019: Day 5, Part 2
;;;; https://adventofcode.com/2019/day/5
;;;;
;;;; Sunny with a Chance of Asteroids
;;;;
(ns day5b.ns)

;;;
;;; Load in the data for the problem as a vector
;;;
(def input-data
  (mapv read-string
        (re-seq #"-?[\d.]+" (slurp "resources/day5.txt"))))

;;;
;;; Arbitrary assignment of input value, according to puzzle description.
;;;
(def input 5)

;;;
;;; Opcodes and Abstractions
;;; ------------------------
;;;
;;; Below are the implementations of the opcodes defined for this puzzle.
;;; Each returns a map with three values:
;;;   intcode: The intcode after the opertaion
;;;   position: The position of the pointer after the operation
;;;   halt: Whether to halt or not (as a boolean)
;;;
;;; Here is a description of the opcodes, directly from the puzzle
;;; descriptions:
;;;
;;;   "Opcode 1 adds together numbers read from two positions and stores the
;;;   result in a third position."
;;;
;;;   "Opcode 2 works exactly like opcode 1, except it multiplies the two inputs
;;;   instead of adding them."
;;;
;;;   "Opcode 3 takes a single integer as input and saves it to the position
;;;   given by its only parameter."
;;;
;;;   "Opcode 4 outputs the value of its only parameter."
;;;
;;;   "Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the
;;;   instruction pointer to the value from the second parameter. Otherwise, it
;;;   does nothing."
;;;
;;;   "Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
;;;   instruction pointer to the value from the second parameter. Otherwise, it
;;;   does nothing."
;;;
;;;   "Opcode 7 is less than: if the first parameter is less than the second
;;;   parameter, it stores 1 in the position given by the third parameter.
;;;   Otherwise, it stores 0."
;;;
;;;   "Opcode 8 is equals: if the first parameter is equal to the second
;;;   parameter, it stores 1 in the position given by the third parameter.
;;;   Otherwise, it stores 0."
;;;
;;;   "[Opcode] 99 means that the program is finished and should immediately
;;;   halt."
;;;
;;; The opcodes can be abstracted into types, according to the forms of the
;;; functions they implement, as follows:
;;;
;;; Opcode-Type Opcode Parameters Description
;;; ----------- ------ ---------- ------------------------------
;;;      A       1,2       3      arithmetic function evaluation
;;;      B       3         1      input
;;;      C       4         1      output
;;;      D       5,6       2      conditional jump
;;;      E       7,8       3      predicate evaluation
;;;      Z       99        0      halt
;;;

(defn opcode-type-A
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:third-parameter instruction)
                   ((:operation-function instruction)
                    (:first-value instruction) (:second-value instruction)))
   :position (+ position (inc (:parameters instruction)))
   :halt false})

(defn opcode-type-B
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:first-parameter instruction)
                   input)
   :position (+ position (inc (:parameters instruction)))
   :halt false})

(defn opcode-type-C
  [intcode position instruction]
  (println (:first-value instruction))
  {:intcode intcode
   :position (+ position (inc (:parameters instruction)))
   :halt false})

(defn opcode-type-D
  [intcode position instruction]
  {:intcode intcode
   :position (if ((:operation-function instruction)
                  (:first-value instruction))
               (:second-value instruction)
               (+ position (inc (:parameters instruction))))
   :halt false})

(defn opcode-type-E
  [intcode position instruction]
  {:intcode (assoc intcode
                   (:third-parameter instruction)
                   (if ((:operation-function instruction)
                        (:first-value instruction)
                        (:second-value instruction))
                        1 0))
   :position (+ position (inc (:parameters instruction)))
   :halt false})

(defn opcode-type-Z
  [intcode position _]
  {:intcode intcode
   :position position
   :halt true})

;;;
;;; Set the opcode-type (to be called later by `execute instruction`), the
;;; number of parameters, and the operation-function that gets supplied to
;;; the opcode-type-* function.
;;;
(defn opcode-function
  [opcode]
  (cond
    (= opcode 1)
    {:function opcode-type-A :parameters 3 :operation-function +}
    (= opcode 2)
    {:function opcode-type-A :parameters 3 :operation-function *}
    (= opcode 3)
    {:function opcode-type-B :parameters 1}
    (= opcode 4)
    {:function opcode-type-C :parameters 1}
    (= opcode 5)
    {:function opcode-type-D :parameters 2 :operation-function #(not (zero? %))}
    (= opcode 6)
    {:function opcode-type-D :parameters 2 :operation-function zero?}
    (= opcode 7)
    {:function opcode-type-E :parameters 3 :operation-function <}
    (= opcode 8)
    {:function opcode-type-E :parameters 3 :operation-function =}
    (= opcode 99)
    {:function opcode-type-Z :parameters 0}
    :else nil))

;;;
;;; Given the intcode and a position, return a map of the opcode, the number
;;; of parameters requried, and the mode for the first/second/third parameters,
;;; where 0 is position mode, and 1 is immediate mode.  The following let
;;; assignments are of interest:
;;;
;;; `op`: split the operation into a vector of individual digits
;;; `last-op`: pick out the last two digits of op
;;; `function`: sets function that corresponds to op
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
        function (opcode-function opcode)
        coerce #(if (nil? %) 0 %)]
    {:function (:function function)
     :parameters (:parameters function)
     :operation-function (:operation-function function)
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
;;; second/third parameters.  Importantly, this function calls parse-operation,
;;; parse-parameters, and get-values.
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
;;; To generate result, call (:function instruction), which is the function we
;;; need to call previously assigned by opcode-function.  This relieves us of
;;; the requirement to embed a condition that explicitly calls different
;;; functions.
;;;
(defn execute-instruction
  [intcode position]
  (let [instruction (parse-instruction intcode position)
        result ((:function instruction) intcode position instruction)]
    {:intcode (:intcode result)
     :position (:position result)
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

;;;
;;; Run the intcode program.  Any outputs are the result of opcode-4.
;;;
(run input-data)
