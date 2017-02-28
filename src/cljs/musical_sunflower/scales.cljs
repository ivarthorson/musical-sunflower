(ns musical-sunflower.scales)

;; For creating musical scales (or perhaps I should say, tunings)

;; Data structures and their names:
;;;--------------------------------
;; Fracs:  [numerator denomenator] pairs
;; Ratios: 1.0203, meaning 2% higher than the root.
;; Pitches: 330.0 hz, i.e. a frequency

(defn gcd ;;   TODO: Do this again with a better algorithm!
  "Returns the greatest common denominator of a and b."
  [a b]
  (if-let [z (filter #(let [x (/ a %)
                            y (/ b %)]
                        (and (= x (int x))
                             (= y (int y))))
                     (range 1 (min a b)))]
    (or (last z) 1)
    1))

(defn in-octave
  "Maps a ratio n/d back into an octave range (1/1 to 2/1)."
  [[n d]]
  (let [a (- n (* d (dec (quot n d))))
        g (gcd a d)]
    [(/ a g) (/ d g)]))

(defn fracs->ratios
  "Converts the scale fraction numbers into ratios"
  [fracs]
  (map (fn [[n d]] (* n (/ 1 d))) fracs))

(defn fracs->intervals
  "Returns the intervals created by the chord or scale. Only returns
  the 'first differences', so if you have ABC, you only get back AB
  and BC; getting AC is your own responsibility."
  [scale]
  (loop [s scale
         r []]
    (if (< 1 (count s))
      (let [[an ad] (first s)
            [bn bd] (second s)
            n (* bn ad)
            d (* an bd)]
        (recur (next s)             
               (conj r (in-octave [n d]))))
      r)))

(def phi (+ -1 (/ (+ 1 (Math/sqrt 5)) 2)))
(def sunflower-seq (reductions + (repeat phi)))

(def trt    (Math/pow 2 (/ 1 12))) ;; twelfth root of two
(def low-c  (* 220.0 trt trt trt))

(defn equitemper [base steps indexes] (map #(Math/pow base (/ % steps)) indexes))

(def et12-names ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A"])
(def et12 (equitemper 2 12 (range 13)))
(def et23 (equitemper 2 23 (range 24)))

;; No octave! 
(def exotic-1 (equitemper 3 23 (range 23)))

(def piano-entire     (equitemper 2 12 (map #(+ 12 % -39) (range 88)))) ;; Assumes A = 440.0 is 0 index
(def piano-white-keys (equitemper 2 12 [0 2 4 5 7 9 11]))
(def piano-black-keys (equitemper 2 12 [1 3 6 8 10]))

(def major (equitemper 2 12 [0 2 4 5 7 9 11 12]))
(def minor (equitemper 2 12 [0 2 3 5 7 9 10 12]))

(def pythagorean-inf-fracs  (map in-octave (iterate (fn [[n d]] [(* 3 n) (* 2 d)]) [1 1])))
(def pythagorean-fracs [[1 1] [9 8] [81 64] [4 3] [3 2] [27 16] [243 128] [2 1]])
(def pythagorean (fracs->ratios pythagorean-fracs))
(def pythagorean-dorian (concat (rest pythagorean) (map #(* 2 %) (take 1 (rest pythagorean)))))

(def pentatonic-fracs [[1 1] [9 8] [4 3] [3 2] [27 16] [2 1]])
(def pentatonic (fracs->ratios pentatonic-fracs))

(def triad-major (equitemper 2 12 [0 4 7]))
(def triad-minor (equitemper 2 12 [0 3 7]))
