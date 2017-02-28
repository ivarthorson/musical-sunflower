(ns musical-sunflower.sound
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :refer [chan close!]]
            [cljs-bach.synthesis :refer 
             [sine triangle connect-> adsr adshr gain add
              low-pass sawtooth run-with destination
              constant connect percussive current-time] :as syn]
            [musical-sunflower.common :refer [timeout]]
            [musical-sunflower.model :as model]))

(defn human-audible? [hz] (< hz 20000))

;; Harmonic strengths relative to the fundamental
(def square-wave   (map #(/ 1.0 (- (* 2 (inc %)) 1)) (range)))
(def triangle-wave (map #(* % %) square-wave))
(def sawtooth-wave (map #(/ 1.0 (inc %)) (range)))

;; For playing sounds (notes) in a browser
;; 
;; A note is a hashmap with four keys:
;;    :pitch       frequency [hz]
;;    :time        when the note should start [s]
;;    :duration    note duration [s]
;;    :instrument  (fn [{:keys [pitch]}] ...) or one of the instruments below.

(defonce audiocontext  (syn/audio-context))

(defonce user-instrument (atom {:attack 0.02
                                :decay 0.1
                                :sustain 0.8
                                :release 0.2
                                :harmonics [1.0 0.6 0.4 0.3 0.2]}))

(defn make-instrument 
  "Makes an instrument function with relative strength of harmonics and
  with the overall volume modulated by an ADSR envelope."
  [attack-decay-sustain-release harmonics]
  (fn [{:keys [pitch]}]
    (connect-> (apply add (map (fn [harmonic-number proportion]
                                 (connect-> (sine (* pitch harmonic-number))
                                            ;;(percussive 0.01 proportion)
                                            (gain (* 0.1 proportion))))
                               (rest (range))
                               harmonics))
               (apply adsr attack-decay-sustain-release))))

(defn user-defined-instrument
  "Makes a new instrument when called."
  []
  (let [{:keys [attack sustain decay release harmonics]} @user-instrument]
   (make-instrument [attack sustain decay release] 
                    harmonics)))

(def fake-bell (make-instrument [0.02 0.2 0.8 0.3] 
                                [1.0 0.6 0.4 0.3 0.2]))

(defn bell []
  (let [pitch 440.0
        f (fn [n proportion]
            (connect-> 
             (sine (* n pitch))        ; Each harmonic is a sine wave.
             (percussive 0.01 proportion) ; The attack and decay of each note.
             (gain (* proportion 0.05))))] ; Multiply the volume of each harmonic by 0.5.
    (apply add (map f
                    [1.0 2.0 3.0 4.1 5.2] ; Each harmonic is a multiple of the base frequency.
                    [1.0 0.6 0.4 0.3 0.2] ; Higher harmonics are weaker.
                    ))))

(defn marimba [{:keys [pitch]}]
  (connect->
    (add (sine pitch)
         (sine (inc pitch)) 
         (sine (* 2 pitch)))
    (adshr 0.01 0.2 0.2 0.2 0.3)  
    (gain 0.1)))

(defn organ [{:keys [pitch]}]
  (connect->
    (add (sine (* 0.5 pitch))
         (triangle pitch))
;    (low-pass (* 4 pitch) (connect-> (sine 3) (gain 3)))
    (adsr 0.1 0 1 0.3)
    (gain 0.1)))

(defn wah [{:keys [pitch]}]
  (connect->
    (sawtooth pitch)
    (low-pass
      (connect->
        (constant (* 4 pitch))
        (adsr 0.1 0.2 0.4 0.3)) 5)
    (adsr 0.3 0.5 0.8 0.3)
    (gain 0.3)))

(defn play!
  "Take a sequence of notes or chords and play them!"
  [notes]
  (doseq [{:keys [time duration instrument pitch color harmonics] :as note} notes]
    (let [at (+ time (.-currentTime audiocontext))
          connected-instance (connect (instrument note) destination)
          nn (assoc 
                 (dissoc note :time :duration :instrument)
               :uuid (rand 1000000))]
      ;; Fire off fake threads to manage each note's display
      (go (<! (timeout (* 1000 time)))
          (swap! model/notes-playing conj nn)
          (<! (timeout (* 1000 duration)))
          (swap! model/notes-playing disj nn))
      ;; Now add it to the actual sound device
      (connected-instance audiocontext at duration))))

(defn quick-chord!
  [fs]
  (play! (vec (for [f fs]
                {:pitch f
                 :harmonics (take 10 sawtooth-wave)
                 :instrument organ
                 :duration 3.0
                 :time 0.01
                 :color "blue"}))))
