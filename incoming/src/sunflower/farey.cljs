(ns sunflower.farey
  (:require [reagent.core :as reagent :refer [atom]]
            [sunflower.svg :as svg]))

;; For drawing farey squares and circles

(def INTEGER_MAX_VALUE 2000000)
(def INTEGER_MIN_VALUE -2000000)

(def farey-terms           (atom 4))
(def farey-terms-max-denom (atom 20))

(def ^:dynamic farey-highlight-color "blue")

(defn farey-to-angle
  "Converts a farey fraction into an angle around the unit circle."
  [[n d]]
  (* 2 Math/PI (/ n d)))

(defn point-on-unit-circle
  [theta]
  (let [x (Math/cos theta)
        y (Math/sin theta)]
    [x y]))

(defn midpoint
  "The midpoint rule for the x-coordinate of trapezoids."
  [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn farey-next
  "Given seq of farey points S, return a new seq
  with the next iteration of farey points interposed. Example:
  (farey-next [[0 1] [1 1]]) ; => [[0 1] [1 2] [1 1]]"
  [s]
  (loop [a (first s)
         b (second s)
         r (rest s)
         z []]    
    (if b
      (recur (first r)
             (second r)
             (rest r)
             (conj (conj z a) (midpoint a b)))
      (conj z a))))

(defn filter-farey 
  "Removes terms with denominators less than or equal to farey-terms-max-denom."
  [s]
  (filter (fn [[n d]] (<= d @farey-terms-max-denom)) s)) 

(defn farey-seq [] (map filter-farey (iterate farey-next [[0 1] [1 1]])))

(defn log-farey-seq [] (map (fn [s] (map (fn [[n d]] [[n d] (/ (Math/log (+ 1 (/ n d))) (Math/log 2))]) s))
                            (map filter-farey (iterate farey-next [[0 1] [1 1]]))))

(defn text-line
  "For drawing text on the bottom line of a farey square."
  [[n d]]
  (let [x (/ n d)
        y1 (- 0.05)
        y2 (- 0.10)] 
    [:g {:transform "scale(1,-1)"}
     [:text {:x x :y y1 :font-size 0.05 :text-anchor "middle" :style {:text-decoration "underline"}} (str n )]
     [:text {:x x :y y2 :font-size 0.05 :text-anchor "middle"} (str d)]]))

(defn text-circle
  "For drawing text around the unit circle."
  [[n d]]
  (let [theta (farey-to-angle [n d]) 
        [x y] (point-on-unit-circle (- (* 2 Math/PI) theta))
        s 1.05 ] 
    [:g {:transform "scale(1, -1)"}
      [:text {:x (* s x) :y (- (* s y) 0.05) :font-size 0.05 :text-anchor "middle" :style {:text-decoration "underline"}} (str n )]
      [:text {:x (* s x) :y (+ (* s y) 0.05) :font-size 0.05 :text-anchor "middle"} (str d)]]))

(defn farey-text-line-seq [] (map #(map text-line %) (farey-seq)))
(defn farey-text-circle-seq [] (map #(map text-circle %)  (farey-seq)))

(defn intersection
  "Return the intersection point of two lines of form y = Ax + b"
  [[A1 b1] [A2 b2]] 
  (let [x (/ (- b2 b1) (- A1 A2))
        y (+ (* A1 x) b1)]
    [x y]))

(defn distance
  "Returns the euclidian distance between two points A and B."
  [[x1 y1] [x2 y2]]
  (let [xd (- x1 x2)
        yd (- y1 y2)]
    (Math/sqrt (+ (* xd xd) (* yd yd)))))

(defn tangent
  "Returns the equation of a line, tangent to the unit circle,
  that contacts the point on the unit circle at angle THETA."
  [theta]
  (let [[x y] (point-on-unit-circle theta)
        A (if (= y 0)
            (if (< x 0) INTEGER_MAX_VALUE INTEGER_MIN_VALUE)
            (/ (- x) y)) 
        b (if (= y 0)
            (if (> x 0) INTEGER_MAX_VALUE INTEGER_MIN_VALUE)
            (+ y (/ (* x x) y)))]
      [A b]))


(defn farey-arc
  "Draws a farey arc between two angles on the unit circle."
  [[n1 d1] [n2 d2]]
  (let [theta1 (farey-to-angle [n1 d1])
        theta2 (farey-to-angle [n2 d2])
        start  (point-on-unit-circle theta1)
        end    (point-on-unit-circle theta2)
        center (intersection (tangent (+ 0.001 theta1))  ;; The +0.001 is for numerical reasons at 0 & pi radians
                             (tangent theta2))
        radius (distance center start)]
    (svg/arc center radius start end)))

(defn farey-seq-to-arcs [s]
  (map farey-arc s (next s)))

(defn farey-arcs-seq [] (map farey-seq-to-arcs (rest (farey-seq))))

(defn farey-lines
  "Returns [L1,L2,L3], wich are the three Farey lines connecting
  rationals r1 and r2 in a trapezoid.
  BUG: Why does 2/5ths have a yc that is wrong?"
  [[n1 d1] [n2 d2] & [color]]
  (let [x1 (/ n1 d1)
        x2 (/ n2 d2)
        [n d] (midpoint [n1 d1] [n2 d2])
        y1 (/ 1 d1)
        y2 (/ 1 d2)
        xc (/ n d)
        yc (/ 1 d)]
    [:g
     (svg/line [x1 y1] [xc 0] color)
     (svg/line [x2 y2] [xc 0] color)
     (svg/line [xc yc] [xc 0] color)]))

(defn farey-seq-to-lines [s]
  (map farey-lines s (next s)))

(defn farey-lines-seq [] (map farey-seq-to-lines (farey-seq)))

(defn farey-halfarc
  "Draws a half-arc between the two points."
  [[n1 d1] [n2 d2] & [color]]
  (let [x1 (/ n1 d1)
        x2 (/ n2 d2)
        r  (* 0.5 (- x2 x1))
        xc (* 0.5 (+ x1 x2))]
    (svg/arc [xc 0] r [x1 0] [x2 0] color)))

(defn farey-seq-to-halfarcs [s]
  (map farey-halfarc s (next s)))

(defn farey-halfarcs-seq [] (map farey-seq-to-halfarcs (farey-seq)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Move elsewhere

(defn box
  [[x1 y1] [x2 y2]]
  [:g 
   (svg/line [x1 y1] [x1 y2])
   (svg/line [x1 y1] [x2 y1])
   (svg/line [x2 y1] [x2 y2])
   (svg/line [x1 y2] [x2 y2])
   (svg/line [x1 y1] [x2 y2])
   (svg/line [x1 y2] [x2 y1])])

;; ;; Basic idea for rendering
;; ;; 0. Always, a slider affects number of farey terms displayed. 0-7 I think is a reasonable limit
;; ;;    You can also "hide" terms with denominators below some limit. This is accomplished by filtering
;; ;;    The "new" terms should be light gray
;; ;; 1. Number line
;; ;;    Slider affects number of farey terms displayed
;; ;; 2. Pull from several sequences
;; ;;    farey-seq, which you plot on the number line as numbers and tics
;; ;;    farey-lines-seq, which plots a box
;; ;;    farey-linearcs-seq, which plots arcs on the farey box
;; ;; 3. An interpolation function for the circles and arcs
;; ;;    Interpolate the number line itself, wrapping into a circle
;; ;;    Interpolate the tics, which follow the line
;; ;; 4. Then expose the farey-arcs-seq, which plots in a circle.
;; ;;    (May want to play around with wrapping the arcs too, but optional)
;; ;; 5. Now relate the complexity of the path to the point with repeated fractions

(defn farey-square []
  [svg/xy-graph [600 600] [-0.1 1.1 -0.1 1.1]
   [box [0 0] [1 1]]
   (nth (farey-text-line-seq) @farey-terms)
   (apply concat (take @farey-terms (farey-lines-seq)))
   (apply concat (take @farey-terms (farey-halfarcs-seq)))])

(defn farey-circle []
  [svg/xy-graph [600 600] [-1.1 1.1 -1.1 1.1]
   [svg/circle [0 0] 1]
   (butlast (nth (farey-text-circle-seq) @farey-terms))
   (apply concat (take @farey-terms (farey-arcs-seq)))])

(defn farey-buttons [data-atom]
  [:div 
   [:input {:type "button" :value "0" :on-click (fn [e] (reset! data-atom 0))}]
   [:input {:type "button" :value "1" :on-click (fn [e] (reset! data-atom 1))}]
   [:input {:type "button" :value "2" :on-click (fn [e] (reset! data-atom 2))}]
   [:input {:type "button" :value "3" :on-click (fn [e] (reset! data-atom 3))}]
   [:input {:type "button" :value "4" :on-click (fn [e] (reset! data-atom 4))}]
   [:input {:type "button" :value "5" :on-click (fn [e] (reset! data-atom 5))}]
   [:input {:type "button" :value "6" :on-click (fn [e] (reset! data-atom 6))}]
   [:input {:type "button" :value "7" :on-click (fn [e] (reset! data-atom 7))}]
   [:input {:type "button" :value "8" :on-click (fn [e] (reset! data-atom 8))}]
   [:input {:type "button" :value "9" :on-click (fn [e] (reset! data-atom 9))}]
   [:input {:type "button" :value "10" :on-click (fn [e] (reset! data-atom 10))}]])

(defn farey-widgets
  []
  [:div 
   [farey-square]
   [farey-buttons farey-terms]
   [farey-buttons farey-terms-max-denom]])


