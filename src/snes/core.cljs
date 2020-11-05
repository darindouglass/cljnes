(ns snes.core
  (:require [pge.core :as pge]
            [snes.bus :as bus]
            [snes.cpu :as cpu]))

(defn tick [state elapsed-ms]
  (prn state))

(defn start! []
  (-> {}
      (cpu/init!)
      (bus/init!)
      (pge/construct 680 480 2 2 {:id "canvas"})
      (pge/start tick)))

(defn -main [& _]
  (start!))
