(ns musical-sunflower.piano
  (:require [musical-sunflower.model :as model]
            [musical-sunflower.scales :as scales]
            [musical-sunflower.sound :as sound]
            [musical-sunflower.svg :as svg]) )

(defn half-steps-apart
  "Returns the number of half-steps apart two notes are."
  [f0 f1]
  (* 12 (Math/log (/ f1 f0)) (/ 1 (Math/log 2))))

(defn nearby-active-notes
  "Returns a color mixture of the colors, weighted according to their strengths.
  The opacity calculation is : R = w*R_fg + (1-x)*R_bg"  
  [freq mainrgb]
  (let [apart   (map vector 
                     (map (partial half-steps-apart freq) (map :pitch @model/notes-playing))
                     (map #(condp = (:color %)
                             "red"   [1 0 0]
                             "green" [0 1 0]
                             "blue"  [0 0 1]
                             "magenta" [1 1 0]) @model/notes-playing)
                     ;(repeat [1 0 0])    ;
                     )
        nears   (filter #(> 1 (Math/abs (first %))) apart)
        weights (map #(- 1 (Math/abs (first %))) nears)
        rgbs    (map second nears)
        opac    (fn [rgb1 [w rgb2]]
                  (vec (map #(+ (* w %2) (* (- 1 w) %1)) rgb1 rgb2)))]
    (if (empty? nears)
      (svg/rgb2color mainrgb)
      (svg/rgb2color (reduce opac mainrgb (map vector weights rgbs))))))

(defn draw-piano-key
  [w h n f c & [x0]]
  (let [fill (nearby-active-notes f c)]
    [:rect {:class "pianokey"
            :fill fill
            :stroke "black" 
            :x (or x0 (* w n))
            :y 0
            :width w
            :height h
            :on-mouse-down (fn [_] (sound/quick-chord! [f]))}]))

(defn piano-octave
  [rootnote width height x y]
  (let [width 161
        white-height height
        black-height (* 0.66666 height)
        white-width (/ width 7)  
        black-width (/ width 12) 
        cde-width  (- white-width (* 0.66666 black-width))
        abfg-width (- white-width (* 0.75 black-width))
        freqs (map #(* rootnote (Math/pow 2 (/ % 12.0))) (range 12))]
    [:g {:transform (str "translate(" x "," y ")")}
     (into [:g]
           (for [[n f] (map vector (range 7) (map #(nth freqs %) [0 2 4 5 7 9 11]))] ;; white notes              
             [draw-piano-key white-width white-height n f [1 1 1]]))
     (into [:g]
           (for [[n f] (map vector [1 3] (map #(nth freqs %) [1 3]))] ;; black notes CDE    
             [draw-piano-key black-width black-height n f [0.8 0.8 0.8]]))
     (into [:g] (for [[n f] (map vector [1 3 5] (map #(nth freqs %) [6 8 10]))] ;; black notes ABFG
                  [draw-piano-key black-width black-height n f [0.8 0.8 0.8] (+ (* white-width 3) (* n abfg-width))]))]))

(defn piano-keyboard
  []
  (let [w 161
        h 120]
    [:svg {:width (* 3 w)
           :height h
           :xmlns "http://www.w3.org/2000/svg"}
     [piano-octave (* 1 scales/low-c) w h (* w 0) 0]
     [piano-octave (* 2 scales/low-c) w h (* w 1) 0]
     [piano-octave (* 4 scales/low-c) w h (* w 2) 0]]))
