(ns sunflower.math)

(def pi   Math/PI)
(def tau (* 2.0 Math/PI))
(def d2r (/ pi 180.0))

 (defn mean [s] (* (/ 1 (count s)) (reduce + s)))

(defn polar->cartesian
  [r theta]
  [(* r (Math/cos theta)) (* r (Math/sin theta))])

(defn rotate
  "Rotates coordinates (x,y) CCW about the origin by theta radians."
  [theta [x y]]
  [(+ (*  1.0 x (Math/cos theta))
      (*  1.0 y (Math/sin theta)))
   (+ (*  -1.0 x (Math/sin theta))
      (*  -1.0 y (Math/cos theta)))])
