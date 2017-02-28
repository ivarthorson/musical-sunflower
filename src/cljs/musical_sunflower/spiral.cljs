(ns musical-sunflower.spiral
  (:require [reagent.core :as reagent :refer [atom]]
            [musical-sunflower.math  :as math]
            [musical-sunflower.model :as model]
            [musical-sunflower.notes :as notes]
            [musical-sunflower.scales :as scales]
            [musical-sunflower.svg   :as svg]))

;; For the spiral viewer

;; TODO: Package all of these into a single atom

(def IR           (atom 0.25))
(def OR           (atom 1.00))
(def spiral?      (atom true))
(def spiral-pitch (atom 0.05))
(def divisions    (atom nil))
(def divisions?   (atom true))
(def poly?        (atom true))
(def polygons     (atom [])) ;; each number in the vector draws a complete polygon
(def spokes?      (atom true))
(def spokes       (atom [])) ;; each number in the vector draws a single spoke, in units of a turn
(def sunflower-terms (atom {:terms 0}))


(defn freq->angle  [freq] (* freq (/ math/tau @model/root-note)))
(defn freq->radius [freq] (+ @IR (- (* @spiral-pitch (/ freq (* @model/root-note))) @spiral-pitch)))

(defn draw-spiral []
  (let [ang-inc (* math/d2r 5)
        freqs  (take-while #(< (freq->radius %) @OR) (iterate #(+ (* @model/root-note (/ 1 72.0)) %)
                                                               @model/root-note))
        angles  (map freq->angle freqs)
        radii   (map freq->radius freqs)
        coords  (map math/polar->cartesian radii angles)]    
    [svg/line-seq coords {:stroke-width "0.005"}]))

(defn draw-polygons []
  (into [:g] (for [poly @polygons]
               (let [angles (map #(* % (/ math/tau poly)) (range 0 (inc poly)))
                     coords (map (partial math/polar->cartesian 1.0) angles)]
                 (svg/line-seq coords {:stroke-width 0.01})))))

(defn draw-spokes []  
  (into [:g] (for [spoke @spokes]
               (let [[x1 y1] (math/polar->cartesian @OR (* spoke math/tau))]
                 [:g 
                  [svg/line 0 0 x1 y1 {:stroke-width 0.01}]
                  ;;[svg/text x1 y1 (str spoke)]
                  ]))))

(defn draw-divisions []
  (let [ir @OR
        or (* 1.05 @OR)]
   [:g
    [svg/circle 0 0 ir {:stroke-width 0.01}] 
    [svg/circle 0 0 or {:stroke-width 0.01}] 
    (into [:g]
          (let [divs (if (nil? @divisions) 0 @divisions)]
            (for [d (range 0 divs 2)]
              (let [d_ang   (float (/ math/tau divs))
                    ang     (float (* d d_ang))
                    [x1 y1] (math/rotate ang [ir 0])
                    [x2 y2] (math/rotate ang [or 0])
                    [x3 y3] (math/rotate (+ d_ang ang) [or 0])
                    [x4 y4] (math/rotate (+ d_ang ang) [ir 0])]                 
                [:path {:d (apply str (interpose 
                                       " "
                                       ["M" x1 y1 "L" x2 y2 "A" or or 0 0 0 x3 y3 "L" x4 y4 "A" ir ir 0 0 1 x1 y1] ))
                        :stroke "black"
                        :fill "grey"
                        :stroke-width "0.01"}]))))]))  

(defn draw-note-dot
  [x y opac color tooltip]
  [:g
   [svg/circle x y 0.05 {:stroke-width 0
                         :fill color
                         :opacity opac}]
   #_ [svg/text x y 0.1 tooltip {:opacity 1.0
                              :class "tooltip"}]])

(defn draw-note 
  "Draws a single note on the spiral graph."
  [{:keys [color pitch harmonics] :as note}]
  (into [:g]
        (map (fn [[freq strength]]
               (let [a (freq->angle freq)
                     r (freq->radius freq)
                     [x y] (math/polar->cartesian r a)]                 
                 [draw-note-dot x y strength color (str freq "Hz")]))
             (notes/note->frequencies note))))

(defn draw-notes
  "Draws the notes and their harmonics. "
  []
  (into [:g]
        (for [note @model/notes-playing]
          [draw-note note])))

(defn draw-sunflower
  []
  (->> (take (js/Number (:terms @sunflower-terms)) scales/sunflower-seq)
       (map #(*  % @model/root-note))
       (map (fn [freq]
              (let [a (freq->angle freq)
                    r (+ @IR (- (* 0.25 @spiral-pitch (/ freq @model/root-note)) @spiral-pitch))
                    [x y] (math/polar->cartesian r a)]                 
                [draw-note-dot x y 1.0 "black" (str freq "Hz")])))
       (into [:g])))

(defn spiral-plot
  []
  [svg/xy-graph [600 600] [-1.1 1.1 -1.1 1.1]     
   [draw-spiral]
   [draw-divisions]
   [draw-polygons]
   [draw-spokes]
   [draw-notes]
   [draw-sunflower]])

;; (defn draw-tuning
;;   ""
;;   [diam root freqs names #_ notes? #_ harmonics?]
;;   [:g (for [[f n] (map (fn [a b] [a b]) freqs (range))]
;;         (let [a (freq->angle f)]
;;           ^{:key (str a)}
;;           [draw-spoke diam a 0.5 "#ccc" (nth names n)]))])

;; (defn draw-updown
;;   "Up/Down arrows for atom A using functions FUP and FDOWN."
;;   [a fup fdown]
;;   [:g 
;;    [:path {:d (str "M 0 0 L 5 0 L 2.5 -4")
;;            :fill "#000"
;;            :on-click (fn [_] (swap! a fup))}]
;;    [:path {:d (str "M 0 1 L 5 1 L 2.5 5")
;;            :fill "#000"
;;            :on-click (fn [_] (swap! a fdown))}]])

;; (defn draw-controls-menu
;;   []
;;   [:g {:transform "translate(-95 -90)"}
;;    [:g {:transform "translate(0 0)"} 
;;     [:g {:transform "translate(50 0)"}
;;      (draw-poly 4 @poly-sides 90)]
;;     [text 6 2 (str @poly-sides " sided polygon")]
;;     (draw-updown poly-sides #(min 20 (inc %)) #(max 2 (dec %)))]
;;    [:g {:transform "translate(0 10)"} 
;;     [text 6 2 (str @divisions " divisions")]
;;     (draw-updown divisions #(min 50 (inc %)) #(max 0 (dec %)))]
;;    [:g {:transform "translate(0 20)"} 
;;     [text 6 2 (str @harmonics " harmonics")]
;;     (draw-updown harmonics #(min 50 (inc %)) #(max 0 (dec %)))]
;;    [:g {:transform "translate(0 30)"} 
;;     [text 6 2 (str @sunflower-skip-left " SkipLeft")]
;;     (draw-updown sunflower-skip-left #(min 30 (inc %)) #(max 0 (dec %)))]
;;    [:g {:transform "translate(0 40)"} 
;;     [text 6 2 (str @sunflower-skip-right " SkipRight")]
;;     (draw-updown sunflower-skip-right #(min 30 (inc %)) #(max 0 (dec %)))]
;;    [:g {:transform "translate(0 50)"} 
;;     [text 6 2 (str @sunflower-points " FlowerPoints")]
;;     (draw-updown sunflower-points #(min 200 (inc %)) #(max 0 (dec %)))]
;;    [:rect {:x 0 :y 60 :width 10 :height 10 :fill "blue"  :on-click (fn [_] (play-notes!))}]
;;    [:rect {:x 0 :y 70 :width 10 :height 10 :fill "black" :on-click (fn [_] (cycle-root!))}]])

