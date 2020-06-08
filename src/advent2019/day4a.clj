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
;;; at-least-two-the-same criterior.
;;;
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

;;;
;;; Recurse through all possible passwords, storing all potentially correct
;;; passwords in a vector.
;;;
(defn passwords
  [lower upper]
  (reduce (fn [passwords candidate]
            (if (and (increase? candidate) (adjacents? candidate))
              (conj passwords candidate)
              passwords))
          [] (range lower upper)))

;;;
;;; Perform the calculation by count the number of potentially correct
;;; passwords.
;;;
(count (passwords (first puzzle-input) (second puzzle-input)))
