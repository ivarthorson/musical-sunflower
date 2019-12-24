(ns sunflower.common
  (:require [cljs.core.async :refer [chan close!]]
            [goog.string :as gstring]))

(defn timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn d2s 
  "Converts a decimal into a string with 3 digits after the decimal place."
  [d]
  (let [s (str d)]
    (apply str (if (= \- (first s))
                 (take 6 s)
                 (take 5 s))))
  ;; (gstring/format "%3.3f" d)
  )
