;;;;
;;;; Advent of Code 2019: Day 6, Part 1
;;;; https://adventofcode.com/2019/day/6
;;;;
;;;; Universal Orbit Map
;;;;
(ns day6a.ns)

;;;
;;; Load in the data for the problem as vectors of keywords
;;;
(def input-data (mapv #(vector (keyword (re-find #"^.+(?=\))" %))
                               (keyword (re-find #"(?<=\)).*$" %)))
                      (re-seq #".*\).*" (slurp "resources/day6.txt"))))

