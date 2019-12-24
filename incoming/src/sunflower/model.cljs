(ns sunflower.model
  (:require [reagent.core :as reagent :refer [atom]]
            [sunflower.scales :refer [low-c] ]))

(def notes-playing (atom #{}))
(def root-note  (atom (* 2 low-c))) ;; TODO: Rename root-freq
