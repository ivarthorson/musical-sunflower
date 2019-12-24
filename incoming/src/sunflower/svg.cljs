(ns sunflower.svg
  (:require [sunflower.common :refer [d2s]]))

;; For plotting svgs with mathematical-style coordinate systems

(def ^:dynamic *tic-scale* 0.1)
(def ^:dynamic *tic-label-scale* 0.1)
(def ^:dynamic *line-stroke-width* 0.1)

(defn tohex [x] 
  (condp = (int (* 16 x))
    0 "0"
    1 "1"
    2 "2"
    3 "3"
    4 "4"
    5 "5"
    6 "6"   
    7 "7"
    8 "8"
    9 "9"
    10 "A"
    11 "B"
    12 "C"
    13 "D"
    14 "E"
    15 "F"
    "F"))

(defn rgb2color [[r g b]]
  (let [mag (Math/sqrt (+ (* r r) (* g g) (* b b)))]
   (str "#" (tohex r) (tohex g) (tohex b))))

(defn opacity
  "Wrap this around part of an SVG to control the opacity of objects OBJS."
  [val & objs]
  [:g {:stroke-opacity val
       :fill-opacity val}
   (into [:g] objs)])

(defn text
  "Shorthand for rendering svg text."
  [x y font-size s & [options]] 
  [:g {:transform (str "translate(" x "," y ")")}
   [:g {:transform "scale(1,-1)"}
    [:text (merge {:font-size font-size} options) s]]])

(defn line
  "Shorthand for rendering svg line. Not performant for many lines."
  [x0 y0 x1 y1 & [options]]
  [:path (merge {:d (str "M " x0 " " y0 " L " x1 " " y1)
                 :stroke "#000"
                 :fill "none"
                 :stroke-width *line-stroke-width*}
                options)])

(defn line-seq
  "Shorthand for plotting a sequence of lines."
  [seq & [options]]
  (when-not (empty? seq)
    [:path (merge {:d (apply str (concat ["M " (first (first seq)) 
                                          " " (second (first seq)) " "]
                                         (apply concat (for [[x y] seq]
                                                         ["L " x " " y " "]))))
                   :stroke "#000"
                   :fill "none"
                   :stroke-width *line-stroke-width*}
                  options)]))

(defn arc
  "Draw an arc centered at point C with radius R that starts at
  S and ends at E"
  [c r s e & [color]]
  (let [scale *plot-scale*
        [xc yc] c
        [xs ys] s
        [xe ye] e]
   [:path {:d (apply str ["M "
                          (* scale xs) " "
                          (* scale ys) " "
                          "A " (*  scale r) " " (* scale r) " " 0 " " ;; circular arc, not elliptical, and no rotation
                          0 " " 0 " " ;; small arc flag, sweep flag
                          (* scale xe) " "
                          (* scale ye)])
           :stroke (or color "black")
           :fill "none"
           :stroke-width *line-stroke-width*}]))

(defn circle
  "Shorthand for drawing an SVG circle."
  [xc yc r & [options]]
  [:circle (merge {:cx xc
                   :cy yc
                   :r  r
                   :stroke "#000"
                   :fill "none"
                   :stroke-width *line-stroke-width*}
                  options)])

(defn xy-graph
  "For plotting xy graphs using mathematical coordinates instead of SVG ones.
   [w h]                  Width and height of svg in pixels
   [xmin xmax ymin ymax]  Display range of what is displayed of x and y axes"
  [[w h] [xmin xmax ymin ymax] & elements]
  (let [xrange (- xmax xmin)
        yrange (- ymax ymin)]
     [:svg {:width  (str w)
            :height (str h)
            :xmlns  "http://www.w3.org/2000/svg"
            :viewBox (apply str (interpose " " [0 0 w h]))}
      [:g {:transform (str "translate(" 
                           (+ 0 (/ (* w (- xmin)) xrange)) ","  
                           (- h (/ (* h (- ymin)) yrange)) ")")}
       [:g {:transform (str "scale(" (/ w xrange) ", " (/ (- h) yrange) ")")}
        (into [:g] elements)]]]))

(defn xtic
  "Draws a vertical tic on the x-axis at position X."
  [x]
  (binding [*line-stroke-width* 0.02]
    (line x 0 x (- *tic-scale*))))

(defn ytic
  "Draws a horizontal tic on the y-axis at height Y."
  [y]
  (binding [*line-stroke-width* 0.02]
    (line 0 y (- *tic-scale*) y)))

(defn labeled-xtic
  "Plots a labeled x-tic at NUM/DEN, where NUM
  is the numerator and DEN is the (optional) denominator."
  [[num den]]  
  (let [x (/ num (or den 1))]
    [:g 
     [xtic x]
     (text x (+ (- *tic-scale*) (* -1 *tic-label-scale*)) 
           *tic-label-scale*
           (d2s num)
           {:text-anchor "middle"})
     (when den
       (text x  (+ (- *tic-scale*) (* -2 *tic-label-scale*))
             *tic-label-scale*
             (d2s den)
             {:text-decoration "overline"
              :text-anchor "middle"}))]))

(defn labeled-ytic
  "Plots a labeled x-tic at NUM/DEN, where NUM
  is the numerator and DEN is the (optional) denominator."
  [[num den]]  
  (let [y (/ num (or den 1))]
    [:g 
     [ytic y]
     (text (- *tic-label-scale*) 
           y 
           *tic-label-scale*
           (d2s num)
           {:text-anchor "end"
            :alignment-baseline "middle"})
     (when den
       (text (- *tic-label-scale*) 
             y
             *tic-label-scale*
             (d2s den)
             {:text-decoration "overline"
              :alignment-baseline "middle"
              :text-anchor "end"}))]))

(defn labeled-xtics
  [xs]
  (into [:g] (map #(labeled-xtic (if (vector? %)
                                   %
                                   (vector %))) 
                  xs)))

(defn labeled-ytics
  [ys]
  (into [:g] (map #(labeled-ytic (if (vector? %)
                                   %
                                   (vector %))) 
                  ys)))

(defn x-axis [xmin xmax] (line xmin 0 xmax 0)) 
(defn y-axis [ymin ymax] (line 0 ymin 0 ymax)) 

(defn axes
  "Draws the axes."
  [xmin xmax ymin ymax]
   [:g 
    (x-axis xmin xmax)
    (y-axis ymin ymax)])
