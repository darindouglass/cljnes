(ns snes.core
  (:require [pge.core :as pge]
            [snes.bus :as bus]
            [snes.cpu :as cpu]))

(defn tick [state elapsed-ms]
  (-> state
      #_(pge/clear)
      (pge/draw-string 0 0 (str "darin douglass - " (rand-int 10) " - " elapsed-ms) "0xFFF")))

(defn start! []
  (-> {}
      (cpu/init!)
      (bus/init!)
      (pge/construct 680 480 2 2 {:id "canvas"})
      (pge/start tick)))

(defn -main [& _]
  (start!))
