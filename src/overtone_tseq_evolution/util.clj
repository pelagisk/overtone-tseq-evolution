(ns overtone-tseq-evolution.util)

(defn fn-pow
  "Compose f n times and apply to x"
  [f x n]
  (if (> n 0)
    (recur f (f x) (dec n))
    x))

(defn dot-product
  "The dot product of two vectors"
  [x y]
  (reduce + (map * x y)))

(defn transpose
  "returns the transposition of a `coll` of vectors"
  [coll]
  (apply map vector coll))

(defn matrix-mult
  "Matrix multiplication representing linear operators"
  [mat1 mat2]
  (let [row-mult (fn [mat row]
                   (map (partial dot-product row)
                        (transpose mat)))]
    (map (partial row-mult mat2) mat1)))

(defn midi-number-seq-to-time-seq
  "Tool to convert a midi seq to a time seq:

  [m m' ...]

  ((t [m]) (t' [m']) ...)

  TODO: allow for a longer period"
  [seq]
  (let [n (count seq)]
    (map #(identity [(/ %1 n) [(nth seq %1)]]) (range n))))
