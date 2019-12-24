(ns sunflower.string
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [sunflower.common :refer [timeout]]
            [sunflower.sound :as sound]
            [sunflower.svg :as svg]))

;; For plotting a vibrating string widget

(def num-strings 11)
;(def harmonic-names ["1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11"  ])
(def harmonic-names ["A" "A" "G" "A" "C" "G" "N" "C" "D" "N" "N"])

(def vibration-time   (atom 0.0))
(def vibrating?       (atom (vec (repeat num-strings false ))))
(def vibration-thread (go (while true
                            (swap! vibration-time + 0.01)
                            (<! (timeout 30)))))

(defn vibration-on!  [n] (swap! vibrating? assoc n true))
(defn vibration-off! [n] (swap! vibrating? assoc n false))

(defn zarathustra [_] 
  (let [freq 110.0]
    (go (vibration-on! 1)
        (<! (timeout 2000)) (vibration-on! 2)
        (<! (timeout 2000)) (vibration-on! 3)
        (<! (timeout 2000)) (vibration-on! 4)
        (<! (timeout 2000)) (vibration-on! 5)
        (<! (timeout 2000)) (reset! vibrating? (vec (repeat num-strings false)))) 

    (sound/play! [{:pitch freq
                   :harmonics [1]
                   :instrument sound/organ
                   :duration 10.0
                   :time 0.01
                   :color "blue"}
                  {:pitch (* 2 freq)
                   :harmonics [1]
                   :instrument sound/organ
                   :duration 8.0
                   :time 2.0
                   :color "blue"}
                  {:pitch (* 3 freq)
                   :harmonics [1]
                   :instrument sound/organ
                   :duration 6.0
                   :time 4.0
                   :color "blue"}
                  {:pitch (* 4 freq)
                   :harmonics [1]
                   :instrument sound/organ
                   :duration 4.0
                   :time 6.0
                   :color "blue"}
                  {:pitch (* 5 freq)
                   :harmonics [1]
                   :instrument sound/organ
                   :duration 2.0
                   :time 8.0
                   :color "blue"}])))

(defn vibrating-string
  "Amplitude A, freq f, time T"
  [harmonic x0 x1 y0]
  (let [wave  (fn [a t x] (vector x (+ y0 (* a
                                             (Math/cos (* 1 Math/PI harmonic t))
                                             (Math/sin (* 1 Math/PI harmonic x))))))
        times (range x0 x1 (* 0.02 (- x1 x0)))
        freq (* 110.0 harmonic)
        t  (if (get @vibrating? harmonic) @vibration-time 0)
        a 0.025
        pos    (map (partial wave    a  0) times)
        moving (map (partial wave    a  t) times)
        neg    (map (partial wave (- a) 0) times)]
    (binding [svg/*line-stroke-width* 0.001]
      [:g 
       [svg/text 0 y0 0.025 (nth harmonic-names (dec harmonic)) 
        {:text-anchor "end"
         :on-click (fn [_]
                     (go (vibration-on!  harmonic)
                         (<! (timeout 8000))
                         (vibration-off! harmonic)) 
                     (sound/play! [{:pitch freq
                                    :harmonics [1]
                                    :instrument sound/organ
                                    :duration 8.0
                                    :time 0.01
                                    :color "blue"}]))}]
       (svg/line-seq pos)      
       (svg/line-seq moving)      
       (svg/line-seq neg)
       [svg/text 1.0 y0 0.025 (str freq "Hz")
        {}]])))

(defn vibrating-string-svg
  "An SVG of vibrating strings that can be clicked on to cause sounds to occur."
  []
  (let [w 900
        h 400]
     [svg/xy-graph [w h] [-0.2 1.1 0 0.6]
      (into [:g] (for [harmonic (range 1 (inc num-strings))]
                   (let [y0 (* 0.05 harmonic)]
                     [vibrating-string harmonic 0 1 y0])))]))
