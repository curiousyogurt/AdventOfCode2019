;;;;
;;;; Advent of Code 2019: Day 1, Part 2
;;;; https://adventofcode.com/2019/day/1
;;;;
(ns day1b.ns
  (:require [clojure.string :as str]))

;;;;
;;;; This solution differs from Part 1 in that total calls module-fuel
;;;; instead of fuel, so that we calculate fuel for fuel.
;;;;

;;;
;;; Load in the data for the problem: the mass of each module
;;;
(def input-data
  (->> "resources/day1.txt"
    slurp str/split-lines
    (mapv read-string)))

;;;
;;; Calculate the fuel for a single module from mass
;;;
(defn fuel [mass]
  (- (quot mass 3) 2))

;;;
;;; Recursively calculate the total fuel required to launch, since
;;; fuel itself has mass and therefore requires fuel (fuel for fuel).
;;;
(defn module-fuel
  [mass]
  (loop [total 0
         extra (fuel mass)]
    (if (> 0 extra)
      total
      (recur (+ extra total) (fuel extra)))))

;;;
;;; Calculate the total fuel for the Fuel Counter-Upper.  Given a list of
;;; module masses, add required fuel for each module to a running total.
;;;
(defn total
  [masses]
  (reduce (fn [total mass] (+ total (module-fuel mass)))
          0 masses))

;;;
;;; Calculate the result
;;;
(total input-data)
