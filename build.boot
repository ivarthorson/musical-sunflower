(set-env! :source-paths   #{"src/cljs"}
          :resource-paths #{"resources/public/"} 
          :dependencies '[[adzerk/boot-cljs "2.1.5" :scope "test"]
                          [adzerk/boot-cljs-repl "0.4.0" :scope "test"]
                          [adzerk/boot-reload "0.6.0" :scope "test"]
                          [pandeiro/boot-http "0.8.3"  :scope "test"]                          
                          [cider/piggieback "0.3.9" :scope "test"]
                          [nrepl "0.4.5" :scope "test"]
                          [weasel "0.7.0" :scope "test"]
                          [org.clojure/tools.nrepl "0.2.13" :scope "test"]
                          [org.clojure/clojurescript "1.10.439"]
                          [org.clojure/core.async "0.4.490"]
                          [reagent "0.8.1"]
                          [cljs-bach "0.3.0"]])

(require '[adzerk.boot-cljs      :refer [cljs]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[adzerk.boot-reload    :refer [reload]]
         '[pandeiro.boot-http    :refer [serve]])

(deftask build []
  (comp (speak)        
        (cljs)))

(deftask run []
  (comp (serve)
        (watch)
        (cljs-repl)       
        (reload)
        (build)))

(deftask production []
  (task-options! cljs {:optimizations :advanced})
  identity)

(deftask development []
  (task-options! cljs   {:optimizations :none}
                 reload {:on-jsload 'musical-sunflower.core/init})
  identity)

(deftask dev
  "Simple alias to run application in development mode"
  []
  (comp (development)
        (run)))


