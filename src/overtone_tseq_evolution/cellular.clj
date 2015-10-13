(ns overtone-tseq-evolution.cellular
  (:use [overtone.live]))

(definst beep [freq 440]
  (let [src (sin-osc freq)
        env (env-gen (perc 0.01 1.0) :action FREE)]
    (* src env)))

(defn beep-note [n]
  (beep (midi->hz n)))

(defn rule1 [i pattern]
  (let [l (count pattern)
        a (nth pattern (mod (dec (dec i)) l))
        b (nth pattern (mod (dec i) l))
        c (nth pattern i)
        d (nth pattern (mod (inc i) l))
        e (nth pattern (mod (inc (inc i)) l))
        a? (first a)
        b? (first b)
        c? (first c)
        d? (first d)
        e? (first e)]
    (if c?
      (if (and b? d?)
        (if (and a? e?)
          (merge c b? a? e?)
          (if (or a? e?)
            e
            (merge c b? d?)))
        (if (or b? d?)
          e
          []))
      (if (and b? d?)
        (if (and a? e?)
          d
          (if (or a? e?)
            e
            []))
        (if (or b? d?)
          (if (and a? e?)
            (merge c e?)
            (if (or a? e?)
              c
              [])))))))

(defn rule2 [i pattern]
  (let [l (count pattern)
        a (nth pattern (mod (dec i) l))
        b (nth pattern i)
        c (nth pattern (mod (inc i) l))]
    (if b
      (if (< b 2)
        (+ b 8)
        (if (> b 100)
          (- b 8)
          (if (and a c)
            nil
            (if a
              (inc (inc (int (/ (+ b a) 2))))
              (if c
                (dec (dec (int (/ (+ c b) 2))))
                nil)))))
      (if (and a c)
        (int (/ (+ a c) 2))
        (if a
          (inc a)
          (if b
            (inc b)
            80))))))

(defn rule3 [i pattern]
  (let [l (count pattern)
        a (nth pattern (mod (dec i) l))
        b (nth pattern i)
        c (nth pattern (mod (inc i) l))
        avg (/ (+ a c) 2)]
    (if (< b 2)
      (+ b 8)
      (if (> b 100)
        22
        (if (= b (int avg))
          (- b 8)
          (if (> b avg)
            (+ b 2)
            (- b 3)))))))

(defn evolve-automaton [pattern rule]
  (let [l (count pattern)]
    (for [i (range l)]
      (rule i pattern))))

(defn automaton [cur-t sep-t pattern rule synth]
  (let [pattern (evolve-automaton pattern rule)]
    (at cur-t (doseq [note pattern] (synth (max note (- note)))))
    (let [new-t (+ cur-t sep-t)]
      (apply-by new-t #'automaton [new-t sep-t pattern rule synth]))))

(def pattern '(80 100 46 94 67 48 46 67 65 60 71 60 74 81))
(def pattern2 '(10 42 20 95))

;; (automaton (now) 200 pattern2 rule3 beep-note)

;; TODO: a 2d lattice

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn play-cellular [cur-t sep-t pattern synth]
  (let [new-pattern (step pattern)]
    (at cur-t (doseq [[semi oct] new-pattern]
                (synth (+ (* 12 (mod (+ oct 3) 8)) (mod semi 12)))))
    (let [new-t (+ cur-t sep-t)]
      (apply-by new-t #'play-cellular [new-t sep-t new-pattern synth]))))

(def blinker #{[1 0] [1 1] [1 2]})
(def glider #{[2 0] [2 1] [2 2] [1 2] [0 1]})
(def light-spaceship #{[2 0] [4 0] [1 1] [1 2] [1 3] [4 3] [1 4] [2 4] [3 4]})
(def herschel #{[-1 -2] [-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [1 1]})
(def multum-in-parvo #{[0 -2] [1 -2] [2 -2] [-1 -1] [2 -1] [-2 0] [-3 1]})
(def x66 #{[-24 -5] [-26 -4] [-25 -4] [-26 -3] [-23 -3] [-22 -3] [-21 -3] [-18 -3] [-26 -2] [-21 -2] [-20 -2] [-19 -2] [-25 -1] [-24 -1] [-23 -1] [-20 -1] [-19 -1] [-25 1] [-24 1] [-23 1] [-20 1] [-19 1] [-26 2] [-21 2] [-20 2] [-19 2] [-26 3] [-23 3] [-22 3] [-21 3] [-18 3] [-26 4] [-25 4] [-24 5]})

;; (beep 600)

;; (play-cellular (now) 200 x66 beep-note)

;; (stop)
