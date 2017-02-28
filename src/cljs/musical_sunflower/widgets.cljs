(ns musical-sunflower.widgets)

;; Shorthand for common UI widgets

(defn button
  [name callback] 
  [:input.mybutton
   {
    :type "button" 
    :value name
    :on-click callback}])

(defn slider
  "Creates a slider for a specific key K under an atom A."
  [a k min max vstep]
  [:input {:type "range" 
           :step vstep
           :defaultValue min
           :min min 
           :max max
           :style {:width "95%"}
           :on-change (fn [e] (swap! a assoc k (.-target.value e)))}])

(defn checkbox
  "A checkbox for an atom"
  [a k label]
  [:label [:input {:type "checkbox"
                   :checked (get @a k)
                   :on-click (fn [e]
                               (swap! a assoc k (.-target.checked e)))}]
   label])
