;;;;
;;;; Advent of Code 2019: Day 3, Part 2
;;;; https://adventofcode.com/2019/day/3
;;;;
;;;; Crossed Wires
;;;;
(ns day3b.ns
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

;;;
;;; Load in the data for the problem from file as puzzle-wires.
;;;
(defn get-file
  [source]
  (str/split-lines (slurp source)))

(defn parse [move]
  (vector (re-find #"[RLUD]" move) (read-string (re-find #"\d+" move))))

(defn wires-from-file
  [source]
  (let [file (get-file source)]
  (vector (mapv parse (re-seq #"[\w\d.]+" (first file)))
          (mapv parse (re-seq #"[\w\d.]+" (last file))))))

(def puzzle-wires (wires-from-file "resources/day3.txt"))

;;;
;;; Load in examples from problem statement (given as strings).
;;;
(defn wire-from-string
  [string]
  (mapv parse (re-seq #"[\w\d.]+" string)))

;;;
;;; Example wires from the problem statement
;;;
(def example-1a (wire-from-string "R8,U5,L5,D3"))
(def example-1b (wire-from-string "U7,R6,D4,L4"))
(def example-2a (wire-from-string "R75,D30,R83,U83,L12,D49,R71,U7,L72"))
(def example-2b (wire-from-string "U62,R66,U55,R34,D71,R55,D58,R83"))
(def example-3a (wire-from-string
                 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"))
(def example-3b (wire-from-string "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

;;;
;;; Given origin (as an ordered pair) and a direction (RLDU), calculate
;;; the destination coordinate.
;;;
;;; Example: (step [0 0] "R") ==> [1 0
;;;
(defn step
  [origin direction]
  (cond
    (= direction "R") (vector (inc (first origin)) (second origin))
    (= direction "L") (vector (dec (first origin)) (second origin))
    (= direction "U") (vector (first origin) (inc (second origin)))
    (= direction "D") (vector (first origin) (dec (second origin)))
    :else :error))

;;;
;;; Produce a trail of breadcrums for one move.  Define move as a list with two
;;; elements: a direction, and a distance.  This is analogous to the convention
;;; in thet data.  So ["R" 8] means move right 8 units.
;;;
;;; The breadcrumbs are the sequence of coordinates that allsowm us to realise
;;; the path.  For example, [[2 0] [1 0] [0 0]] means that we started at [0 0]
;;; and took 2 steps to the right (along the x axis).
;;;
;;; We can then call step to create a list of breadcrumbs recursively for the
;;; move in question.  So to extend the trail of breadcrumbs with crumbs
;;; [[0 0]] and move ["R" 2], we do a single move with [0 0] as our origin and
;;; "R" as our direction.  Step will return [1 0].  Then we recur, consing
;;; [1 0] to the breadcrumbs, and making our move ["R" 1].
;;;
;;; Example: (breadcrumbs [[0 0]] ["U" 2] ==> '([0 2] [0 1] [0 0]).
;;;
(defn breadcrumbs
  [crumbs move]
  (loop [crumbs crumbs
         move move]
    (if (zero? (second move))
      crumbs
      (recur (cons (step (first crumbs) (first move)) crumbs)
             (list (first move) (dec (second move)))))))

;;;
;;; Follow a wire, stepping recursively through the list of moves.  The output
;;; is a complete list of coordinates (the breadcrumbs).  Submit moves to
;;; breadcrumbs one at a time.
;;;
(defn follow
  [wire]
  (reduce (fn [path move]
          (into [] (breadcrumbs path move)))
          [[0 0]] wire))

;;;
;;; Instead of intersection-distances (from Part One), we just need the
;;; intersections.  Given a pair of wires, calculate path1 (for wire1) and
;;; path2 (for wire 2).  This will give us every coördinate for the wires,
;;; in order.  Since both start at [0 0], eliminate the last element of
;;; both lists, and convert to sets.  This allows us to use the intersection
;;; function to create a set of only overlapping coördinates.
;;;
(defn intersections
  [wires]
  (let [path1 (into #{} (butlast (follow (first wires))))
        path2 (into #{} (butlast (follow (second wires))))]
    (set/intersection path1 path2)))

;;;
;;; This was my first implementation of the steps function (see below).  It
;;; did the job, but was impossibly slow.  What we want to do is get the index
;;; of a particular element.  It is easy enough to do this with a single line,
;;; but my first attempt was to recur the path until the coordinates were
;;; found.  I include step-slow for reference only.
;;;
;;; (defn steps-slow
;;;  [path coordinate]
;;;  (loop [path path
;;;         steps 1]
;;;    (if (= (last path) coordinate)
;;;      steps
;;;      (recur (butlast path) (inc steps)))))

;;;
;;; In order to find the position in the path of a particular pair of
;;; coördinates, drop-while all the element that are not those coördinates, and
;;; count the size of the resulting vector.
;;;
(defn steps
  [path coordinate]
  (count (drop-while #(not= % coordinate) path)))

;;;
;;; This function does the heavy lifting for the problem.  Given a pair of
;;; wires, calculate their paths as path1 and path2.  Then, calculate the sum
;;; of steps to each of the intersections.  Conj the results, so that when the
;;; results are sorted, the element in first place will be the fewest number
;;; of steps.
;;;
(defn intersection-paths
  [wires]
  (let [path1 (into [] (butlast (follow (first wires))))
        path2 (into [] (butlast (follow (second wires))))]
    (reduce (fn [steps-for-intersections intersection]
              (conj steps-for-intersections
                    (+ (steps path1 intersection)
                       (steps path2 intersection))))
            [] (intersections wires))))

;;;
;;; Calculate the result and pick out the first value after sorting.  This will
;;; be the fewest combined steps a pair of wires takes to reach an
;;; intersection.
;;;
(first (sort (intersection-paths puzzle-wires)))
