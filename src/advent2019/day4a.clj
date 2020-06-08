;;;;
;;;; Advent of Code 2019: Day 4, Part 1
;;;; https://adventofcode.com/2019/day/4
;;;;
;;;; Secure Container
;;;;
(ns day4a.ns)

(def puzzle-input [178416 676461])

;;;
;;; Given an integer, break it into its constituent digits.  Convert to a
;;; string with `(str number)`, then use a nifty trick of ASCII codes (I think
;;; credit for this trick goes to whomever wrote a certain program in the
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

(defn increase?
  [number]
  (let [n (breakout number)]
    (<= (first n)
        (first (next n))
        (first (next (next n)))
        (first (next (next (next n))))
        (first (next (next (next (next n)))))
        (first (next (next (next (next (next n)))))))))

(defn adjacents?
  [number]
  (let [n (breakout number)]
    (or (= (first n)
           (first (next n)))
        (= (first (next n))
           (first (next (next n))))
        (= (first (next (next n)))
           (first (next (next (next n)))))
        (= (first (next (next (next n))))
           (first (next (next (next (next n))))))
        (= (first (next (next (next (next n)))))
           (first (next (next (next (next (next n))))))))))

(defn passwords
  [lower upper]
  (reduce (fn [passwords candidate]
            (if (and (increase? candidate) (adjacents? candidate))
              (conj passwords candidate)
              passwords))
          [] (range lower upper)))

(count (passwords (first puzzle-input) (second puzzle-input)))
