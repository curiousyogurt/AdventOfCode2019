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
        (re-seq #"[\d.]+" (slurp "resources/day5.txt"))))

;;;
;;; Arbitrary assicgnment of input and output, according to puzzle.
;;;
(defn input [] 1)
(defn output [] 1)

;;;
;;; Given the opcode, return the operation as a function.  These are arbitrary,
;;; so `cond` is appropriate.
;;;
(defn op
  [opcode]
  (cond
    (= opcode 1) +
    (= opcode 2) *
    (= opcode 3) input
    (= opcode 4) output
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
(defn instruction
  [intcode position]
  (let [operation (parse-operation (nth intcode position))
        parameters (parse-parameters
                     (subvec intcode (inc position) (+ (inc position) (:parameters operation))))
        values (get-values intcode operation parameters)]
    (into operation (into parameters values))))

(:parameters (instruction [1001,4,3,4,33] 0))

(defn execute
  [intcode position]
  (let [instruction (instruction intcode position)] 
    (cond
      (= (:opcode instruction) 1)
      (assoc intcode (:third-parameter instruction)
             ((op (:opcode instruction)) (:first-value instruction) (:second-value instruction)))
      (= (:opcode instruction) 2)
      (assoc intcode (:third-parameter instruction)
             ((op (:opcode instruction)) (:first-value instruction) (:second-value instruction)))
      (= (:opcode instruction) 3)
      (assoc intcode (:first-parameter instruction)
             1)
      (= (:opcode instruction) 4)
      (println (nth intcode (:first-parameter instruction)))
      (= (:opcode instruction) 99)
      (println "halt"))))

(execute [1101,100,-1,4,0] 0)
(execute [1002,4,3,4,33] 0)
(execute [4,0,4,0,99] 0)

;;;
;;; Run the intcode.  Do this by starting at specified position.  If we get nil
;;; as a result, either we have hit opcode 99 (halt), or we have an unknown
;;; opcode; halt in either case.  Step through intcode 4 positions at a time.
;;;
(defn run
  [intcode position]
  (loop [intcode intcode
         position position]
    (let [result (execute intcode position)
          jump (inc (:parameters (instruction intcode position)))]
      (if (nil? result)
        intcode
        (recur result (+ position jump))))))

(println (run input-data 0))

;;;
;;; Prep the intcode by replacing position 1 with the value 12, and position
;;; 2 with the value 2.
;;;
;(defn prep [intcode]
  ;(assoc (assoc intcode 1 12) 2 2))

;;;
;;; Run the intcode starting at position 0, and return the value at position 0.
;;;
;(first (run (prep input-data) 0))
