(ns snes.cpu.impl
  (:require [snes.binary :as b]
            [snes.bus :as bus]))

;; Base functionality
(def flags
  {:carry (b/<< 1 0)
   :zero (b/<< 1 1)
   :disable-interrupt (b/<< 1 2)
   :decimal-mode (b/<< 1 3)
   :break (b/<< 1 4)
   :_ (b/<< 1 5)
   :overflow (b/<< 1 6)
   :negative (b/<< 1 7)})

(defn inc-program-counter [nes]
  (update-in nes [:cpu :program-counter] inc))

(defn read-register [nes register]
  (get-in nes [:cpu :registers register]))

(defn set-register [nes register value]
  (assoc-in nes [:cpu :registers register] value))

(defn update-register [nes register fn args]
  (apply update-in nes [:cpu :registers register] fn args))

(defn get-flag [nes flag]
  (let [status (read-register nes :status)]
    (b/truthy? (b/and status (flags flag)))))

(defn set-flag! [nes flag state]
  (let [flag (flags flag)]
    (if state
      (update-register nes :status b/or flag)
      (update-register nes :status b/and (b/not flag)))))

(defn read [nes address]
  (bus/read nes address))

(defn write [nes address data]
  (bus/write nes address data))

(defn reset [nes])

(defn interrupt [nes])

(defn non-maskable-interrupt [nes])

(defn mark-extra-cycle [nes]
  (update-in nes [:cpu :cycles] + 0.5))
