(ns snes.bus)

(defn init! [state]
  (assoc-in state
            [:bus :devices :fake-ram]
            (into [] (repeat (* 64 1024) 0x0))))

(defn device-key [address]
  (cond
    (<= 0x0000 address 0xFFFF) :fake-ram))

(defn read [nes address]
  (get-in nes [:bus (device-key address) address]))

(defn write [nes address data]
  (assoc-in nes [:bus (device-key address) address] data))
