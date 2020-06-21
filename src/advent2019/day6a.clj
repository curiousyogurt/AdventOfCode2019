;;;;
;;;; Advent of Code 2019: Day 6, Part 1
;;;; https://adventofcode.com/2019/day/6
;;;;
;;;; Universal Orbit Map
;;;;
(ns day6a.ns)

;;;
;;; Load in data as a set of vectors of keywords
;;;
(defn groom-data 
  [data]
  (mapv #(vector (keyword (re-find #"^.+(?=\))" %))
                 (keyword (re-find #"(?<=\)).*$" %)))
        (re-seq #".*\).*" data)))

(def input-data (groom-data (slurp "resources/day6.txt")))

;;;
;;;        G - H       J - K - L
;;;        /           /
;;; COM - B - C - D - E - F
;;;                \
;;;                 I
;;;
(def example1 (groom-data
               "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"))

;;;
;;;   C - D - E
;;;  /
;;; COM - B
;;;  \
;;;   F - G
;;;
(def example2 (groom-data "COM)B\nCOM)C\nC)D\nD)E\nCOM)F\nF)G"))

;;;
;;; COM - B - C - D
;;;
(def example3 (groom-data "COM)B\nB)C\nC)D"))

;;;
;;;         C
;;;        /
;;; COM - B
;;;        \
;;;         D
;;;
(def example4 (groom-data "COM)B\nB)C\nB)D"))


;;;
;;; Create a list of immediate descendants for a given node.  If there are no
;;; descendants, return `nil`.
;;;
(defn descend
  [col node]
  (let [filtered (filterv #(= (first %) node) col)]
    (if (nil? (first filtered))
      nil
      (map second filtered))))

;;;
;;; We may represent a tree using lists, where each list has a root node
;;; followed by branches as lists.  For example, suppose we have this tree:
;;;
;;;
;;;   C - D - E
;;;  /
;;; A - B
;;;  \
;;;   F - G
;;;
;;; A is the root, and it has three desdendants: B, C-D-E, and F-G.  So we may
;;; represent this as (A (B) (C-D-E) (F-G)).  But in this list, (C-D-E) and
;;; (F-G) are not represneted correctly.  So for (C-D-E), we should have
;;; (C (D-E)), and (F (G)).  And then for (D-E), we should have (D (E)).  So
;;; the final representation is: (A (B) (C (D (E))) (F (G))).
;;;
;;; In order to create thsi data structure, we may write a function that builds
;;; these element up recurnsiely.  Starting with a `col` and a (root) `node`,
;;; we get all the immediate children for `col`, and then call our function on
;;; each child (branch) in turn, execpt if that child is a leaf (that is, has no
;;; children of its own.)  As a result, we recurse through all the children
;;; (children, grandchildren, great grandchildren, and so on) of the root node.
;;; 
;;; The following subfunctions are defined:
;;;
;;; subtree: call `tree` on `col` and a given node (%)
;;; branch: cons a node with a list for a branch
;;; leaf: create a list wit ha single element (%) for a leaf
;;;
;;; A (perhaps more perspicuous) option for the function below would be to use
;;; `partial` for `subtree`.  For example: `(let [subtree (partial tree col)]`;
;;; and then call branch with: `branch node (map subree children)`.  When  we
;;; call `subtree` as part of `branch`, `partial` then supplies `col` as the
;;; first parameter.  The code would look like this:
;;;
;;;  (let [subtree (partial tree col)
;;;        branch (fn [m n] (cons m n))
;;;        leaf #(list %)]
;;;    (if-let [children (descend col node)]
;;;      (branch node (map subtree children))
;;;      (leaf node)))
;;;
(defn tree
  [col node]
  (let [subtree #(tree col %)
        branch (fn [m n] (cons m n))
        leaf #(list %)]
    (if-let [children (descend col node)]
      (branch node (map subtree children))
      (leaf node))))

;;;
;;; Given a tree, returns a list of all the proper subtrees.  "Proper" because
;;; the original tree is not included in the list.  We do this by maintaining
;;; a list of trees.  Everytime we generate a subtree, we copy that subtree
;;; into `results`; and if that subtree is not just a list of leaves, we also
;;; add the subtree to the list of trees to be processed.  Thus, we end up with
;;; a recursively generated list of subtrees in `results`.
;;;
;;; Two functions are used to make this work:
;;;   subtree - Given a list, if the first element is a keyword (indicating a
;;;             branch instead of a list of leaves), return everything after
;;;             the keyword; otherwise, return nil.
;;;   leaf?   — This function returns true if the given subtree is a list of one
;;;             or more leaves; otherwise it returns false.
;;;
(defn subtrees
  [tree]
  (loop [trees tree
         results '()]
    (if (empty? trees)
      results
      (let [subtree #(if (keyword? (first %)) (rest %) nil)
            leaf? #(and (list %) (keyword? (first %)) (= (count %) 1))
            sub (subtree (first trees))]
        (if (every? leaf? sub)
          (recur (rest trees) (into results sub))
          (recur (into (rest trees) sub) (into results sub)))))))

;;;
;;; Some example trees to play with (substitute for "input" below).
;;;
(def t1 (tree example1 :COM))
(def t2 (tree example2 :COM))
(def t3 (tree example3 :COM))
(def t4 (tree example4 :COM))
(def c1 (tree (groom-data "A)B\nA)C") :A))
(def c2 (tree (groom-data "D)E") :D))
(def c3 (tree (groom-data "G)H\nH)I") :G))
(def c4 (tree (groom-data "J)K\nJ)M\nK)L\nM)N") :J))

;;;
;;; Calculate the results.  Do this is three steps.  First, create a tree from
;;; an association list.  Second, generate all the subtrees for the generated
;;; tree.  Third, flatten and count the elements.  This will be equal to the
;;; number of direct and indirect orbits.
;;;
(def input (tree input-data :COM))
(count (flatten (subtrees (list input))))
