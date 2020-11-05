(ns snes.cpu.opcode.utils
  (:require [snes.binary :as b]
            [snes.cpu.impl :as cpu]))

(defn branch
  [nes relative-address flag & {:keys [invert?]}]
  (let [flagged? (cond-> (cpu/get-flag nes flag)
                   (= :not invert?) (not))]
    (if flagged?
      (let [program-counter (get-in nes [:cpu :program-counter])
            address (+ program-counter relative-address)
            new-page? (!= (b/and address 0xFF00)
                          (b/and program-counter 0xFF00))]
        (-> nes
            (update :cycles inc)
            (assoc :program-counter address)
            (cond-> new-page? (update :cycles inc))))
      nes)))

(defn load-register [nes address register]
  (let [data (cpu/read nes address)]
    (-> nes
        (cpu/set-register register data)
        (cpu/set-flag :zero (zero? data))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 data)))
        (cpu/mark-extra-cycle))))
