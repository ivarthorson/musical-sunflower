(ns musical-sunflower.harmonics
  (:require [reagent.core :as reagent :refer [atom]]
            [musical-sunflower.model :as model]
            [musical-sunflower.notes :as notes]
            [musical-sunflower.svg :as svg]))

;; For generating or displaying harmonics on a frequency plot

(defn frequency-plot
  "Shows the present frequencies being played. F is the transformation
  function. "
  [& [f]]
  (let [w 600
        h 125
        f (or f identity)
        fmin (f 20)
        fmax (f 20000)
        linewidth (/ (- fmax fmin) w)]
    [:span
     "20Hz" [svg/xy-graph [w h] [fmin fmax -0.1 1.1]     
         (binding [svg/*line-stroke-width* 0.01]
           [:g
            (svg/x-axis fmin fmax)
            (svg/y-axis 0 1)
                                        ;(svg/labeled-xtics (range 0 fmax 1000))
                                        ;(svg/labeled-ytics (range 0 1 0.2))
            ])
         (into [:g]
               (for [note @model/notes-playing]
                 (let [freqs (notes/note->frequencies note)
                       color (or (:color note)  "red")]
                   (into [:g]
                         (for [[p a] freqs]
                           (binding [svg/*line-stroke-width* (* 2 linewidth)]
                             (svg/line (f p) 0 (f p) a {:opacity 0.5
                                                        :stroke color})))))))]
     "20kHz"]))

(defn log-frequency-plot
  []
  (frequency-plot (fn [x] (Math/log x))))
