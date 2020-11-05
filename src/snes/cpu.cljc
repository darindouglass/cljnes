(ns snes.cpu
  (:require [snes.binary :as b]
            [snes.bus :as bus]
            [snes.cpu.impl :as cpu]
            [snes.cpu.opcode :as opcode]))

(defn init! [state]
  (assoc state :cpu {:stack-pointer 0x00
                     :program-counter 0x00
                     :cycles 0
                     :registers {:accumulator 0x00
                                 :status 0x00
                                 :x 0x00
                                 :y 0x00}}))

(defn clock [{{:keys [cycles]} :cpu :as nes}]
  (if (zero? cycles)
    (let [code (get-in nes [:cpu :program-counter])]
      (-> nes
          (cpu/inc-program-counter)
          (opcode/run code)))
    (update-in nes [:cpu :cycles] dec)))
