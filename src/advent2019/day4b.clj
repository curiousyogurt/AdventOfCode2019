;;;;
;;;; Advent of Code 2019: Day 4, Part 2
;;;; https://adventofcode.com/2019/day/4
;;;;
;;;; Secure Container
;;;;
(ns day4b.ns
  (:require [clojure.string :as str]))

(def puzzle-input [178416 676461])

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
  (map #(- (int %) (int \0))
       (str number)))

;;;
;;; Step through digits of `number` and determine whether they meet the
;;; increase-or-stay-the-same criterion.
;;;
(defn increase?
  [number]
  (let [n (breakout number)]
    (<= (first n)
        (first (next n))
        (first (next (next n)))
        (first (next (next (next n))))
        (first (next (next (next (next n)))))
        (first (next (next (next (next (next n)))))))))

;;;
;;; Step through the digits of `number` and detremine hethre they meet the
;;; at-least-two-the-same criterior.  This is the old version of `adjacents?`
;;; which we leave here for reference purposes.
;;;
;;;(defn adjacents?
;;;  [number]
;;;  (let [n (breakout number)]
;;;    (or (= (first n)
;;;           (first (next n)))
;;;        (= (first (next n))
;;;           (first (next (next n))))
;;;        (= (first (next (next n)))
;;;           (first (next (next (next n)))))
;;;        (= (first (next (next (next n))))
;;;           (first (next (next (next (next n))))))
;;;        (= (first (next (next (next (next n)))))
;;;           (first (next (next (next (next (next n))))))))))

;;;
;;; Return `true` if number has at least one doubled digit; otherwise, return
;;; `false` (=`nil` on `re-find`).
;;;
(defn adjacents?
  [number]
  (boolean (re-find #"(\d)\1" (str number))))

;;;
;;; Recurse through all possible passwords, storing all potentially correct
;;; passwords in a vector.  This is the old version of the `passwords`
;;; function, below.  It turns out to be much easier to `filter` based on a
;;; predicate than recurse through a range with `reduce`.  We leave this 
;;; version here for reference purposes.
;;;
;;;(defn passwords
;;;  [lower upper]
;;;  (reduce (fn [passwords candidate]
;;;            (if (and (increase? candidate) (adjacents? candidate))
;;;              (conj passwords candidate)
;;;              passwords))
;;;          [] (range lower upper)))

;;;
;;; Given a range as a vector, filter based on meeting both the increase? and
;;; the adjacents? predicates.
;;;
(defn passwords
  [lower upper]
  (filter #(and (increase? %) (adjacents? %))
          (range lower upper)))

;;;
;;; The additional criterion for Part 2 is that there be two adjacent
;;; matching digits that are not part of a larger group of matching digits.
;;; The strategy we use to deal with this is to take the results from
;;; Part 1 (since Part 2 meets all the constraints of Part 1, but imposes
;;; an additional constraint), and then do the following: (i) convert all
;;; digits in a larger group of matching digits to "-" using a regex, so
;;; larger groups are removed from consideration (see `converted` below); (ii)
;;; delete all pairs of digits (see `deleted` below).  We then compare the
;;; length of strings of opertaion (i) and opertaion (ii); if they not the same
;;; length, there must have been a pair of digits that is not pat of a larger
;;; group.
;;; 
(defn thin
  [passwords]
  (filter (fn [candidate]
            (let [n (str candidate)
                  converted (str/replace n #"([0-9])\1\1+" "-")
                  deleted (str/replace converted #"(\d)\1" "")]
              (not (= (count converted)
                      (count deleted)))))
          (map str passwords)))

;;;
;;; Perform the calculation by count the number of potentially correct
;;; passwords.
;;;
(count (thin (passwords (first puzzle-input) (second puzzle-input))))
