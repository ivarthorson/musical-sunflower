(ns musical-sunflower.notes
  (:require [musical-sunflower.sound :as snd]))

;; For creating notes, chords, and sequences of notes

(defn note->frequencies
  "Converts the note's pitch and harmonics into plottable frequency/amplitude pairs."
  [{:keys [pitch harmonics] :as note}] 
  (let [harmonics (or harmonics [1])]    
    (map-indexed (fn [i harmonic] [(* pitch (inc i)) harmonic])
                 harmonics)))

(def chord-colors ["red" "blue" "green" "magenta" "orange" "brown"])

(defn chord
  "Returns a chord of the notes."
  [instrument duration pitches harmonics]
  (map-indexed (fn [i p]
                 {:pitch p
                  :harmonics harmonics
                  :duration duration
                  :time 0.01
                  :instrument instrument
                  :color (nth chord-colors i)})
               pitches))

(defn arpeggio
  "Returns an arpeggio of notes."
  [instrument duration-per-note pitches harmonics]
  (map-indexed (fn [i p]
                 {:pitch p
                  :harmonics harmonics
                  :duration duration-per-note
                  :time (* duration-per-note i)
                  :instrument instrument
                  :color "red"})
               pitches))
