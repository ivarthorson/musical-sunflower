(ns sunflower.core
  (:require [reagent.core :as reagent :refer [atom]]
            ;; [reagent.session :as session]
            [cljs.core.async :refer [chan close!]] 
            [sunflower.common :refer [timeout d2s]]
            [sunflower.farey :as farey]
            [sunflower.harmonics :as harmonics]
            [sunflower.model :as model]
            [sunflower.notes :as notes]
            [sunflower.piano :as piano]
            [sunflower.scales :as scales]
            [sunflower.sound :as sound]         
            [sunflower.spiral :as spiral]           
            [sunflower.string :as string]       
            [sunflower.svg :as svg]            
            [sunflower.widgets :as widgets])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def defaults (atom {:instrument sound/organ
                     :scale-speed 0.5
                     :chord-hold 4.0
                     :root (* 2 scales/low-c)
                     :harmonics (take 20 (iterate #(* 0.9 %) 1.0))}))

(defn scale-button
  "Makes a button that plays scale RATIOS."
  [name ratios & [speed]]
  (widgets/button name
                  (fn [_]    
                    (let [{:keys [instrument scale-speed root harmonics]} @defaults]
                      (sound/play! (notes/arpeggio instrument
                                                 (or speed scale-speed) 
                                                 (map (partial * root) ratios)
                                                 harmonics))))))

(defn chord-button
  "Chords."
  [name ratios & [extra-fn]]
  (widgets/button name
                  (fn [_]    
                    (let [{:keys [instrument chord-hold root harmonics]} @defaults]
                      (when extra-fn (extra-fn))
                      (sound/play! (notes/chord instrument
                                                chord-hold
                                                (map (partial * root) ratios)
                                                harmonics))))))


(defn interval-table
  [rows]
  (let [mkrow (fn [title labels x nom denom]
                (let [frac (/ nom denom)
                      set-spiral-hints (fn []
                                         (reset! spiral/polygons [denom])
                                         (reset! spiral/spokes (vec (map #(/ % denom) (range denom)))))]
                  [:tr [:td title] 
                   [:td (apply str (interpose "+" labels))]
                   [:td (chord-button (d2s x) [1 x] set-spiral-hints)]
                   [:td (chord-button (str nom "/" denom) [1 frac] set-spiral-hints)]
                   [:td (str (d2s (/ (- frac x) frac 0.01))  "%")]]))] 
    (into [:table {:style {:font-size "70%"}}
           [:tr.head [:td "Chord"] [:td "Ex"] [:td "Actual"] [:td "Near"] [:td "Err"]]]
           (for [r rows]
             (apply mkrow r))))) 


(defn chromatic-intervals-table
  [rows]
  (let [s scales/et12] 
    (interval-table
     [["Unison"        ["C" "C"]   (nth s 0)  1 1]
      ["Minor Second"  ["C" "C#"]  (nth s 1)  18 17]
      ["Second"        ["C" "D"]   (nth s 2)  9 8]
      ["Minor Third"   ["C" "D#"]  (nth s 3)  6 5]
      ["Third"         ["C" "E"]   (nth s 4)  5 4]
      ["Fourth"        ["C" "F"]   (nth s 5)  4 3]
      ["Tritone"       ["C" "F#"]  (nth s 6)  17 12]
      ["Fifth"         ["C" "G"]   (nth s 7)  3 2]
      ["Minor Sixth"   ["C" "G#"]  (nth s 8)  11 7]
      ["Sixth"         ["C" "A"]   (nth s 9)  5 3]
      ["Minor Seventh" ["C" "A#"]  (nth s 10) 16 9]
      ["Seventh"       ["C" "B"]   (nth s 11) 17 9]
      ["Octave"        ["C" "C"]   (nth s 12) 2 1]
      ])))

;; ;; (defn et23-intervals-table
;; ;;   [rows]
;; ;;   (let [vars (map #(+ -1 (Math/pow 2 (/ % 23.0))) (range 23))] 
;; ;;     (interval-table
;; ;;      [["Unison"        ["C" "C"]   i1  0 1]
;; ;;       ["Minor Second"  ["C" "C#"]  i2  1 17]
;; ;;       ["Second"        ["C" "D"]   i3  1 8]
;; ;;       ["Minor Third"   ["C" "D#"]  i4  3 16]
;; ;;       ["Third"         ["C" "E"]   i5  1 4]
;; ;;       ["Fourth"        ["C" "F"]   i6  1 3]
;; ;;       ["Tritone"       ["C" "F#"]  i7  5 12]
;; ;;       ["Fifth"         ["C" "G"]   i8  1 2]
;; ;;       ["Minor Sixth"   ["C" "G#"]  i9  4 7]
;; ;;       ["Sixth"         ["C" "A"]   i10 2 3]
;; ;;       ["Minor Seventh" ["C" "A#"]  i11 1 1]
;; ;;       ["Seventh"       ["C" "B"]   i12 7 8]
;; ;;       ["Octave"        ["C" "C"]   i13 1 1]])))

;; (defn dissonant-intervals-table
;;   [rows]
;;   (let [phi scales/phi
;;         r2 (+ -1 (Math/sqrt 2))
;;         r3 (+ -1 (Math/sqrt 3))] 
;;     (interval-table
;;      [["Phi"       ["" ""] phi  0  1]     
;;       ["sqrt(2)"   ["C" "F#"]  r2  5 12]
;;       ["sqrt(3)"   ["" ""]  r3  1  8]
;;       ["Pi"        ["" ""] 0.14159 1 1]  
;;       ;;["sqrt(5)"   []  r5  3 16]
;;       ])))

;; (defn interval-table-variant
;;   [rows]
;;   (let [r @plot/root-note
;;         mkrow (fn [title x nom denom]
;;                 (let [xd (mod x 1)
;;                       fi (* r (+ 1 x))
;;                       fr (* r (+ 1 (/ nom denom)))
;;                       fd (* r (+ 1 xd))]
;;                  [:tr 
;;                   [:td title] 
;;                   [:td (chord-button (d2s x)  [r fi] (d2s x))]
;;                   [:td (chord-button (d2s xd) [r fd] (d2s xd))]
;;                   [:td (chord-button (str nom "/" denom) [r fr] [(str (d2s r) "Hz") (str (d2s fr) "Hz")])]
;;                   [:td (str (d2s (/ (- fd fr) fr 0.01))  "%")]]))] 
;;     [:table 
;;      [:tr#head [:td "Note"] [:td "Unscaled"] [:td "Scaled"] [:td "Ratio"] [:td "Error"]] 
;;      (for [r rows]
;;        (apply mkrow r))]))

;; (defn sunflower-table
;;   [rows]
;;   (let [phi-notes (map #(- % 1) (take 100 scales/sunflower-ratios))] 
;;     (interval-table-variant
;;      [["0"   (nth phi-notes 0)  5  8]     
;;       ["1"   (nth phi-notes 1)  1  4]     
;;       ["2"   (nth phi-notes 2)  6  7]     
;;       ["3"   (nth phi-notes 3)  1  2]     
;;       ["4"   (nth phi-notes 4)  1  11]     
;;       ["5"   (nth phi-notes 5)  5  7]    
;;       ["6"   (nth phi-notes 6)  1  3]     
;;       ["7"   (nth phi-notes 7)  19 20]     
;;       ["8"   (nth phi-notes 8)  11  20]     
;;       ["9"   (nth phi-notes 9)  1  6]     
;;       ["10"  (nth phi-notes 10) 4  5]     
  
;;       ["11"  (nth phi-notes 11) 2  5]     
;;       ["12"  (nth phi-notes 12) 1 29]     
;;       ["13"  (nth phi-notes 13) 2  3]     
;;       ["14"  (nth phi-notes 14) 1  4]     
;;       ["15"  (nth phi-notes 15) 8  9]     
;;       ["16"  (nth phi-notes 16) 1  2]     
;;       ["17"  (nth phi-notes 17) 1  8]     

;;       ["18"  (nth phi-notes 18) 3  4]     
;;       ["19"  (nth phi-notes 19) 3  8]     
;;       ["20"  (nth phi-notes 20) 32 33]     
;;       ["21"  (nth phi-notes 21) 3  5]     
;;       ["22"  (nth phi-notes 22) 1  5]     
;;       ["23"  (nth phi-notes 23) 5  6]     

;;       ["24"  (nth phi-notes 24) 9  20]     
;;       ["25"  (nth phi-notes 25) 1  15]     
;;       ["26"  (nth phi-notes 26) 2  3]     


;;       ["27"  (nth phi-notes 27) 2  3]     
;;       ["28"  (nth phi-notes 28) 2  3]     
;;       ["29"  (nth phi-notes 29) 2  3]     
;; ;      ["30"  (nth phi-notes 30) 1  6]     
;; ;      ["31"  (nth phi-notes 31) 1  6]     
;; ;      ["32"  (nth phi-notes 32) 1  6]     
      
;;       ;["sqrt(5)"   []  r5  3 16]
;;       ])))

;; (defn rearranged-sunflower-table
;;   []
;;   (let [phi-notes (map #(- % 1) (take 100 scales/sunflower-ratios))] 
;;     (interval-table-variant
;;      [["0"   (nth phi-notes 0)  5  8]    
;;       ["16"  (nth phi-notes 16) 1  2]     

;;       ["6"   (nth phi-notes 6)  1  3]     
;;       ["13"  (nth phi-notes 13) 2  3]  

;;       ["1"   (nth phi-notes 1)  1  4]     

;;       ["18"  (nth phi-notes 18) 3  4]     

;;       ["22"  (nth phi-notes 22) 1  5]     
;;       ["11"  (nth phi-notes 11) 2  5]     
;;       ["21"  (nth phi-notes 21) 3  5]     
;;       ["10"  (nth phi-notes 10) 4  5]

;;       ["9"   (nth phi-notes 9)  1  6]
;;       ["23"  (nth phi-notes 23) 5  6]

;;       ["14"  (nth phi-notes 14) 2  7]                     
;;       ["8"   (nth phi-notes 8)  4  7]     
;;       ["5"   (nth phi-notes 5)  5  7]
;;       ["2"   (nth phi-notes 2)  6  7]


;;       ["17"  (nth phi-notes 17) 1  8]
;;       ["19"  (nth phi-notes 19) 3  8]

;;       ["15"  (nth phi-notes 15) 8  9]

                                      
;;       ;; Less consonant
;;       ["3"   (nth phi-notes 3)  1  2] 

;;       ["4"   (nth phi-notes 4)  1  11]     
;;       ["12"  (nth phi-notes 12) 1 29]      

;;       ["7"   (nth phi-notes 7)  19 20]     
 

;;       ["20"  (nth phi-notes 20) 32 33]     


      
;;       ;["sqrt(5)"   []  r5  3 16]
;;       ])))


;; (defn flower-button
;;   [tex l r]
;;   [:input 
;;    {:type "button" 
;;     :value tex
;;     :on-click (fn [_]
;;                 (reset! plot/sunflower-skip-left l)
;;                 (reset! plot/sunflower-skip-right r))}])

;; (defn flower-table
;;   [rows]
;;   (let [mkrow (fn [title l r]
;;                  [:tr 
;;                   [:td title] 
;;                   [:td (flower-button title l r)]])]
;;     [:table 
;;      [:tr#head [:td "Description"] [:td "Skip Pattern"]] 
;;      (for [r rows]
;;        (apply mkrow r))]))

;; (defn preset-flower-table
;;   []
;;   (flower-table [["Rose (2,3)" 2 3]
;;                  ["Budding Rose (3,5)" 3 5]
;;                  ["Cactus (5,8)" 5 8]
;;                  ["Pinwheel (5,13)" 5 13]
;;                  ["Sunflower (8,13)" 8 13]
;;                  ["Dandelion (4,17)" 4 17]]) )

;; (defn preset-chords
;;   []
;;   [:g
;;    [:h3 "Diatonic Intervals"]
;;    [diatonic-intervals-table]
;;    [:h3 "Chromatic Intervals"]
;;    [chromatic-intervals-table]
;;    [:p "Note how the number of harmonic spokes hints at the denominator!"] 
;;    [:h3 "Most Dissonant Intervals"]
;;    [dissonant-intervals-table]
;;    [:h3 "Sunflower Scale"]
;;    ;; (str scales/sunflower-ratios)
;;    [rearranged-sunflower-table]
;;    [sunflower-table]
;;    [preset-flower-table]

;;    [:p (str (nth (farey/log-farey-seq) 5))]

;;    [:h3 "Next TODOs:"]
;;    [:ol    
;;     [:li "Make play, cycle root buttons pretty"]
;;     [:li "Num Available Notes Button"]
;;     [:li "Clear all chords button (right of piano)"]
;;     [:li "Select root note button (right of piano"]

;;     [:li "Draw spokes for ET12 tuning. (or other arbitrary tuning. Accept data in format: freq-ratios, labels)"]
;;     [:li "Wave Widget: sine, triangle, square wave selector"]
;;     [:li "Arpeggio Widget: Arpeggio y/n before chord"]
;;     [:li "Freq widget: Spinner can numerically select 1 frequency"]
;;     [:li "Write repeated fractions for dissonant things, and consonant things"]
;;     [:li "Hovering could do more stuff"]

;;     [:li "Text: Log and Linear FFT Plot with note tics"]
;;     [:li "Text: Sine Waveform viewer!"]
;;     [:li "Text: beautiful math eqns"]
;;     [:li "Text: Redo harmonics limit, other widget controls"]
;;     [:li "Text: Enable perceptually identical cone tracks cursor"]
;;     [:li "Extra: Make an introductory animation"]
   
;;     ]]) 

 
;; [{:pitch 440.0
;;   :duration 1.0
;;   :time 0.01
;;   :instrument (sound/user-defined-instrument)}
;;  #_                                   {:pitch 550.0
;;                                        :duration 1.0
;;                                        :time 1.0
;;                                        :instrument sound/bell}
;;  #_                                   {:pitch 660.0
;;                                        :duration 1.0
;;                                        :time 2.0
;;                                        :instrument sound/organ}
;;  #_                                   {:pitch 770.0
;;                                        :duration 1.0
;;                                        :time 3.0
;;                                        :instrument sound/wah}
;;  #_                                   {:pitch 880.0
;;                                        :duration 1.0
;;                                        :time 4.0
;;                                        :instrument sound/fake-bell}]

                       ;(sound/chime [440.0] 0.1)
                       ;(println scales/pythagorean)
                       ;; #_(go
                       ;;   (doseq [[n d] s]
                       ;;     (sound/chime [(* 440.0 n (/ 1.0 d))])
                       ;;     (<! (timeout 1000))))


(defn pretty-frac [[n d]]
  [:span.frac [:sup n] [:span "/"] [:sub d]])

(defn hspace [w]
  [:span {:style {:display "inline-block" :width w}}])

(defn fracs->string
  "Makes them a printable string"
  [fracs]
  (into [:span] (map (fn [[n d]] [:span (pretty-frac [n d]) ", " [hspace "40px"]]) fracs)))

(defn home-page []
  [:div
   [:ul.rmenu
;    [presentation]

    ;; (chord-button "Play1" [1000 1101 1300 1400] ["A" "b" "c" "d"])
    ;; (chord-button "Play2" [1300 1500] ["q" "r"]) 
    ;; (str @plot/active-notes)


    [scale-button "Major" scales/major]
    [scale-button "Minor" scales/minor]
    [chord-button "Major" scales/triad-major]
    [:br]

    [:br]
      [:br]
    ; [:div#sec1 [:h3 "s1"] (txt/lorem-ipsum 4)]
    
    ;; [:div#farey
    ;;  [farey/farey-widgets]]    
    ;; [:div#stringy
    ;;  [string/string-widget 0.999]]
    ;; [:div#presets 
    ;;  [preset-chords]]
    ;; (txt/main-text)
    
    ]

   [:div.centerpane
    ;[:p (str @model/notes-playing)] 
    [harmonics/frequency-plot]  [:br]
    [harmonics/log-frequency-plot] [:br]
    [spiral/spiral-plot]  [:br]   
    [piano/piano-keyboard]
    ]])

(comment 

     [:section [:h2 "Vibrating String"]
      [string/vibrating-string-svg] [:br]
      [:span     
       [:span.fragment "110Hz, 220Hz, 330Hz, ..."] 
       [:span.fragment "all at once!"]
       [:span.fragment "(Timbre)"]
       [:span.fragment [widgets/button "First 5 Harmonics" string/zarathustra]]] [:br]
      [harmonics/frequency-plot] [:br]]

     [:section [:h2 "Simple Ratios are Consonant"]
      ; [:p "(Pythagoras found this millenia ago)"]
      [:center
       [:table 
        [:tr [:td [chord-button "Play" [1 2]]]    [:td "2:1"] [:td "C-C"] [:td "\"Octave\""] [:td "Most consonant"]]
        [:tr [:td [chord-button "Play" [1 1.5]]] [:td "3:2"] [:td "C-G"] [:td "\"Fifth\""]  [:td ""]]
        [:tr [:td [chord-button "Play" [1 (/ 4 3)]]] [:td "4:3"] [:td "C-F"] [:td "\"Fourth\""] [:td ""]]
        [:tr [:td [chord-button "Play" [1 1.25]]] [:td "5:4"] [:td "C-E"] [:td "\"Third\""]  [:td ""]]
        [:tr [:td [chord-button "Play" [1 1.2]]] [:td "6:5"] [:td "C-Eb"] [:td "\"Minor Third\""] [:td ""]]
        [:tr [:td [chord-button "Play" [1 1.125]]] [:td "9:8"] [:td "C-D"] [:td "\"Second\""]      [:td ""]]
        [:tr [:td [chord-button "Play" [1 (/ 18 17)]]] [:td "18:17"] [:td "C-C#"] [:td "\"Half-Step\""]      [:td "Dissonant"]]]
       [harmonics/frequency-plot]  [:br]
       [piano/piano-keyboard]]]

     [:section [:h2 "Pythagorean Tuning"]      
      (let [pf pretty-frac]
        [:span
         [:ul       
          [:li "Based on:" [:br] [:center  (pf [4 3]) " * " (pf [3 2]) " = " (pf [2 1])]  [:br]]
          [:li "Pitches relative to the \"root\" note:" [:br] 
           [:center (fracs->string scales/pythagorean-fracs)] [:br]]
          [:li "Neighboring note intervals:" [:br]
           [:center (fracs->string (scales/fracs->intervals scales/pythagorean-fracs))] [:br]]
          [:li "What does it sound like?" [:br]
           [:center [scale-button "Pythagorean Scale" scales/pythagorean] [:br]]]
          [:li.fragment "Problem: transposition requires re-tuning instruments!" [:br]
           [:center [scale-button "Pythagorean Scale" scales/pythagorean-dorian] [:br]]]]


         ;[scale-button "Pythagorean Mode" scales/pythagorean-mode]
         ])]

     [:section [:h2 "Linear and Log Scales"]
      [:span "Lin:" [harmonics/frequency-plot]]  [:br]
      [:span "Log:" [harmonics/log-frequency-plot]] [:br]
      [piano/piano-keyboard] [:br]
      [:ul
       [:li.fragment "Linear view is useful for seeing ratios, consonance"]
       [:li.fragment "Logarithmic view is closer to our perceptual sensitivity"
        [:br] [:center "(100 vs 101Hz is easy, 10,000 vs 10,001Hz is impossible)"]]
       [:li.fragment "Scales: "
        [scale-button "Pythagorean" scales/pythagorean]
        [scale-button "White Keys" scales/piano-white-keys]
        [scale-button "Black Keys" scales/piano-black-keys]
        [scale-button "ET12" scales/et12]
        [scale-button "Entire Piano" scales/piano-entire 0.1]]
       (let [c (fn [[n d]] [chord-button (str n ":" d) [1 (/ n d)]])]
         (into [:li.fragment "Chords: "] (map c [[2 1] [3 2] [4 3] [5 4]])))
       [:li.fragment "Anyone hear the difference between pythagorean and white keys?"]]]

     [:section [:h2 "Modern Equitemper-12 Tuning"]
      (let [p2 (fn [e] [:span "2" [:sup (str e "/12")] [hspace "40px"]])] 
        [:span
         [:ul
          [:li "Pitches relative to root note:" [:br]
           (into [:center] (concat (map p2 [0 1 2 3 4 5])
                                   ["..."]))
           [:br]]
          [:li "Neighboring note intervals: " [:br]
           (into [:center] [[p2 1] [p2 1] [p2 1] [p2 1] [p2 1]]) [:br]]
          [:li.fragment "Transposition is very easy!"]
          [:li.fragment "...but what about consonance?"]]])]
     
     [:section [:h2 "Modern Equitemper-12 Imperfections"]
      [:center
       [piano/piano-keyboard] [:br]
       [chromatic-intervals-table]]]

     [:section [:h2 "Recap of Musical History"]
      [:ul
       [:li.fragment "All natural sounds have many harmonics"]
       [:li.fragment "Relative harmonic volumes determine timbre"]
       [:li.fragment "Simple ratios sound consonant"]
       [:li.fragment "Simple ratios result in harmonics \"lining up\""]
       [:li.fragment "The modern musical system is harmonically imperfect" [:br]
        [:center.fragment "...but it is hugely convenient for transposition"]]
       [:li.fragment "We can't hear very small errors (<0.5%? <0.3%?)"
        [:br] [:center.fragment "...but what if future people or aliens could?"]]
       [:li.fragment "Can we visualize consonance that we can't hear?"]]]

     [:section [:h2 "Linear Spiral Plot"]      
      [:div
       [:ul.rmenu
        [piano/piano-keyboard]
        [chromatic-intervals-table]]
       [:div.centerpane
        [spiral/spiral-plot] 
        [:p "Each turn represents 440Hz;"]
        "Chords:"
        [chord-button "ET12 Major" scales/triad-major (fn []
                                                        (reset! spiral/spokes [0 0.25 0.5 0.75])
                                                        (reset! spiral/polygons [4]))]
        [chord-button "ET12 Minor" scales/triad-minor (fn []
                                                        (reset! spiral/spokes [0 0.2 0.4 0.6 0.8])
                                                        (reset! spiral/polygons [5]))]
        [:br]
        [chord-button "Rat. Major" [1 1.25 1.5] (fn []
                                                   (reset! spiral/spokes [0 0.25 0.5 0.75])
                                                   (reset! spiral/polygons [4]))]
        [chord-button "Rat. Minor" [1 1.2 1.5] (fn []
                                        (reset! spiral/spokes [0 0.2 0.4 0.6 0.8])
                                        (reset! spiral/polygons [5]))]]]]
     
     [:section [:h2 "What's the most dissonant dyad possible?"]
      [:p.fragment "Some \"irrational\" number whose harmonics never match?"]
      [:p.fragment "Repeated fractions:"]
      [:center 
       [:table.fragment
        [:tr [:td "pi"] [:td " = 3.1415..."] [:td " = [3; 7, 15, 1, 292, 1, ...]"] [:td ""]]
        [:tr [:td "sqrt(2)"] [:td " = 1.4142..."] [:td " =  [1; 2, 2, 2, 2, ...]"] [:td "Tritone, Wolf tone"]]
        [:tr [:td "phi"] [:td " = 1.6180..."] [:td " = [1; 1, 1, 1, 1, ...]"] [:td "Nature's Number"]]]]
      [:p.fragment
       [chord-button "pi" [1 (/ 3.14159 1)]]
       [chord-button "pi/2" [1 (/ 3.14159 2)]]
       [chord-button "sqrt(2)" [1 1.41421]]
       [chord-button "phi" [1 1.61803]]
       [:br]
       [harmonics/frequency-plot]]
      ]
     
     #_[:section [:h2 "Farey Sequences"]
      [:center
       [farey/farey-widgets]]]

     [:section [:h2 "The Worst Possible Tuning System"]
      [:p "What if we put a new note every phi-1=0.618 rotations?"]
      [spiral/spiral-plot]
      [widgets/slider spiral/sunflower-terms :terms 0 100 1]
      ;[:p (str @spiral/sunflower-terms)]
      ]
     
     [:section [:h2 "Sunflowers"]
      [:img {:src "img/sunflower.jpg" :width "60%"}]]

     [:section [:h2 "The End"]
      [:p "Thank you for listening!"]]
     
     )

(defn slideshow-page  []
  (fn []
     [:div.reveal
      [:div.slides
       [:section 
        [:h2 "How Sunflowers Lead to Another Musical Scale"]
        [:center
         [:h4 "Jan 31, 2017"]
         [:h4 "Ivar Thorson"]]]

       [:section [:h2 "What is this about?"]      
        [:p.fragment "Music? " [:span.fragment "Math? "] [:span.fragment "History? "] [:span.fragment "Psychophysics? "] [:span.fragment "Kooky numerology?"]]
        [:ol 
         [:li.fragment "What patterns occur in music?"]
         [:li.fragment "What does everybody like about music?"]
         [:li.fragment "What will future/alien music sound like?"]]
        [:br]
        [:br]
        [:p.fragment "Consonance, Dissonance, and Sunflowers."]
        [:aside.notes "Musically, things have changed a lot in the past 1000 years; there is much more sophisticated polyphony, rhythms, and even more notes available in scale. Over the last 100 years, we have seen an explosion of genres. Can we somehow peer into the future and glimpse what lies ahead? How will musical scales antd polyphony be extended in complexity in the future?
More broadly, what won't change even in the future? What does non-human or alien music sound like, if it exists somewhere in the universe? Is there any part of music that is universal, like math?"]]
       ]]))

;; [:li "MATHjax test: " [:script
;;                        {:type "text/x-mathjax-config"}
;;                        "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});"]]

;; ----------------------------------------------------------------------------
;; Initialize app

(defn init-reveal
  []
  (println "init-reveal!")
  ;;this is just to re-trigger the Reveal code during development!
  (go (<! (timeout 1000))
      (.configure js/Reveal (js-obj {:controls false
                                     :progress false
                                     :history true
                                     :center false                                     
                                     :width "100%"
                                     :height "100%"
                                     :margin 0
                                     :minscale 1
                                     :maxscale 1
                                     }))
      (println "Done setting config: " (.getConfig js/Reveal))))


(defn fake-page []
  (fn []
    [:div.slides
     [:section 
      [:h2 "How Sunflowers Lead to Another Musical Scale"]
      [:center
       [:h4 "Jan 31, 2017"]
       [:h4 "Ivar Thorson"]]]
     
     [:section [:h2 "What is this about?"]      
      [:p.fragment "Music? " [:span.fragment "Math? "] [:span.fragment "History? "] [:span.fragment "Psychophysics? "] [:span.fragment "Kooky numerology?"]]
      [:ol 
       [:li.fragment "What patterns occur in music?"]
       [:li.fragment "What does everybody like about music?"]
       [:li.fragment "What will future/alien music sound like?"]]
      [:br]
      [:br]
      [:p.fragment "Consonance, Dissonance, and Sunflowers."]
      [:aside.notes "Musically, things have changed a lot in the past 1000 years; there is much more sophisticated polyphony, rhythms, and even more notes available in scale. Over the last 100 years, we have seen an explosion of genres. Can we somehow peer into the future and glimpse what lies ahead? How will musical scales antd polyphony be extended in complexity in the future?
More broadly, what won't change even in the future? What does non-human or alien music sound like, if it exists somewhere in the universe? Is there any part of music that is universal, like math?"]]

     ]))

(defn init []
  (println "Running init:") 
  ;;(reagent/render [slideshow-page] (.getElementById js/document "app"))
  (reagent/render [fake-page] (.getElementById js/document "app"))
  (init-reveal)
  )

(defn on-js-reload []

  (println "Reloading-on-js")  
  (init)
  )
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

