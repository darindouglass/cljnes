(ns snes.repl
  (:require [pge.core :as pge]))

(defn on-update [pge _]
  (prn _)
  (pge/fill-rect pge
                 (rand-int 100)
                 (rand-int 100)
                 (rand-int 25)
                 (rand-int 25)
                 (rand-nth pge/colors)))

(comment
  (def pge (-> {}
               (pge/construct 800 800 160 160 {:id "canvas"})
               #_(pge/start on-update)))
  )
