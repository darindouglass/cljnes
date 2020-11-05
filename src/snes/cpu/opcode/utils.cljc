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
