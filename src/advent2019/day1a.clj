;;;;
;;;; Advent of Code 2019: Day 1, Part 1
;;;; https://adventofcode.com/2019/day/1
;;;;
(ns day1a.ns
  (:require [clojure.string :as str]))

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
;;; Calculate the total fuel for the Fuel Counter-Upper.  Given a list of
;;; module masses, add required fuel for each module to a running total.
;;;
(defn total [masses]
  (reduce (fn [total mass] (+ total (fuel mass)))
          0 masses))

;;;
;;; Calculate the result
;;;
(total input-data)
