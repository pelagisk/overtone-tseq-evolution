(ns overtone-tseq-evolution.sieve
  (:use [overtone-tseq-evolution.util]
        ;; [overtone-tseq-evolution.tseq]))

(defn residual
  "Sieve of residual class k, modulo n of integers"
  [n k]
  #(= k (mod %1 n)))

(defn union
  "Union of sieves"
  [& sieves]
  #(some identity ((apply juxt sieves) %1)))

(defn disjunction
  "Disjunction of sieves"
  [& sieves]
  #(every? identity ((apply juxt sieves) %1)))

(defn negation
  "Negation of sieve"
  [sieve]
  #(not (sieve %1)))

;; ;; Major scale filter
;; (def major-scale
;;   (union
;;    (disjunction
;;     (negation (residual 3 2))
;;     (residual 4 0))
;;    (disjunction
;;     (negation (residual 3 1))
;;     (residual 4 1))
;;    (disjunction
;;     (residual 3 2)
;;     (residual 4 2))
;;    (disjunction
;;     (negation (residual 3 0))
;;     (residual 4 3))))

(defn transpose
  "Transpose by n integers of sieve"
  [sieve n]
  #(sieve (+ %1 n)))

;; Scale representation is isomorphic with the whole numbers. One seq
;; of objects ordered by an index.

(defn scale-op
  [op i scale]
  (identity (scale (op i)))

;; (defn scale-op
;;   "Perform op repeatedly until a group element is reached. Works for
;;   groups built on integers, like scales"
;;   [op i s]
;;   (let [i (op i)]
;;     (if (s i)
;;       i
;;       (scale-op op i s))))

;; (defn scale-inc
;;   "Increase in scale-like group"
;;   [i s]
;;   (scale-op inc i s))

;; (defn scale-dec
;;   "Decrease in scale-like group"
;;   [i s]
;;   (scale-op dec i s))

(defn g-inc
  "Increase in group"
  [i group]
  (nth (group i) 1))

(defn group-dec
  "Decrease in group"
  [i group]
  (nth (group i) 0))

(defn group-disp
  "Displaces i by d in group g"
  [d g i]
  (if (>= d 0)
    (fn-pow #(g-inc %1 g) i d)
    (fn-pow #(g-dec %1 g) i (- d))))

(defn group-disp-seq
  "Takes a seq of displacements on a group and returns a seq of
  displaced group elements, starting from i"
  [disps g coll i]
  (let [f (first disps)]
    (if f
      (let [j (group-disp f g i)]
        (group-disp-seq (rest disps) g (conj coll j) j))
      coll)))

;; Then finally, work with a set of displacements and play each of the notes
;; (def disp-1 [1 2 -3 -2 4 -1 -1])
;; (def disp-2 [1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1 -1])

;; (def seqs-3 [
;;              [(midi-number-seq-to-time-seq
;;                (play-disps disp-2 test-ladder [] 60)) beep-note]])

;; (play-inst-seqs-loop-1 metro40 1 seqs-3)

;; (def seqs-major [
;;                  [(midi-number-seq-to-time-seq
;;                    (play-disps disp-2 (s-to-g major-scale) [] 60)) beep-note]])

;; (play-inst-seqs-loop-1 metro40 1 seqs-major)
