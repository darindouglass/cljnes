(ns snes.cpu.opcode
  (:require [snes.binary :as b]
            [snes.cpu.address-mode :as address-mode]
            [snes.cpu.impl :as cpu])
  #?(:cljs (:require-macros [snes.cpu.opcode :refer [defopcode]])))

;; For debugging, going from an opcode to a name is super handy.
;; If we didn't the above for debugging, `defmethod` would be the ideal way to do this.
;; However because we would key off the actual opcode and because there's no way to add metadata
;; to a `defmethod`, we'd lose the ability to.
;;
;; As such, we need an actual fn with an actual symbol/name and a lookup
;; atom to facilitate the backwards lookup.
(def opcodes (atom {}))

#?(:clj
   (defmacro defopcode
     [symbol & rest]
     (let [[opcode-data [[nes-symbol address-symbol] & body]] (split-with vector? rest)
           opcode-data (partition 4 opcode-data)]
       `(doseq [[opcode# address-mode# _# cycle-count#] ~opcode-data]
          (swap! opcodes assoc opcode#
                 {:name ~(name symbol)
                  :op-fn (fn [~nes-symbol]
                           (let [program-counter# (get-in ~nes-symbol [:cpu :program-counter])
                                 [~nes-symbol ~address-symbol] (-> ~nes-symbol
                                                                   (assoc-in [:cpu :cycles] cycle-count#)
                                                                   (address-mode# program-counter#))]
                             ~@body))})))))

(defopcode brk
  0x00 address-mode/immediate :cycles 7
  [nes _]
  (let [pointer (inc (get-in nes [:cpu :program-counter]))
        stack (get-in nes [:cpu :stack-pointer])
        hi (b/and (b/>> pointer 8) 0x00FF)
        lo (b/and pointer 0x00FF)]
    (-> nes
        (cpu/set-flag :interrupt true)
        (cpu/write (+ 0x0100 stack) hi)
        (cpu/write (+ 0x0100 (- stack 1)) lo)
        (cpu/set-flag :block true)
        (as-> $ (cpu/write $ (+ 0x0100 (- stack 2)) (cpu/read-register $ :status)))
        (cpu/set-flag :block false)
        (assoc-in [:cpu :stack-pointer] (- stack 3))
        (as-> $ (assoc-in $ [:cpu :program-counter]
                          (b/or (cpu/read $ 0xFFFE)
                                (b/<< (cpu/read $ 0xFFFF) 8)))))))

(defopcode ora {}
  0x01 address-mode/indirect-x  :cycles 6
  0x05 address-mode/zero-page   :cycles 3
  0x09 address-mode/immediate   :cycles 2
  0x0D address-mode/absolute    :cycles 4
  0x11 address-mode/indirect-y  :cycles 5
  0x15 address-mode/zero-page-x :cycles 4
  0x18 address-mode/absolute-y  :cycles 4
  0x1D address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        a (b/or (cpu/read-register nes :accumulator) data)]
    (-> nes
        (cpu/set-register :accumulator a)
        (cpu/set-flag :zero (zero? a))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 a)))
        (cpu/mark-extra-cycle))))

(defopcode asl {}
  0x06 address-mode/zero-page   :cycles 5
  0x0A address-mode/accumulator :cycles 2
  0x0E address-mode/absolute    :cycles 6
  0x16 address-mode/zero-page-x :cycles 6
  0x1E address-mode/absolute-x  :cycles 7
  [nes address]
  ;; TODO: implied vs accumulaor mode returning `nil`.
  (let [implied? (nil? address)
        data (-> nes
                 (cpu/read (or address (cpu/read-register nes :accumulator)))
                 (b/<< 1))
        lo (b/and data 0x00FF)
        nes (-> nes
                (cpu/set-flag :carry (b/truthy? (b/and data 0xFF00)))
                (cpu/set-flag :zero (zero? lo))
                (cpu/set-flag :negative (b/truthy? (b/and 0x80 data))))]
    (if implied?
      (cpu/set-register nes :accumulator lo)
      (cpu/write nes address lo))))

(defopcode php
  0x08 address-mode/implied :cycles 3
  [{:keys [stack-pointer] :as nes} _]
  (let [address (+ 0x0100 stack-pointer)
        data (b/or (cpu/read-register nes :status)
                   (cpu/get-flag nes :break)
                   (cpu/get-flag nes :_))]
    (-> nes
        (cpu/write address data)
        (cpu/set-flag :break false)
        (cpu/set-flag :_ false)
        (update :stack-pointer dec))))

(defopcode bpl
  0x10 address-mode/relative :cycles 2
  [nes relative-address]
  (if-not (cpu/get-flag nes :negative)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc :program-counter address)
          (cond-> new-page? (update :cycles inc))))
    nes))

(defopcode clc
  0x18 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :carry false))

(defopcode jsr
  0x20 address-mode/absolute :cycles 6
  [nes address]
  (let [program-counter (dec (get-in nes [:cpu :program-counter]))
        stack (get-in nes [:cpu :stack-pointer])
        hi (b/and (b/>> program-counter 8) 0x00FF)
        lo (b/and program-counter 0x00FF)]
    (-> nes
        (cpu/write (+ 0x0100 stack) lo)
        (cpu/write (+ 0x0100 (dec stack)) hi)
        (update-in [:cpu :stack-pointer] - 2)
        (assoc-in [:cpu :program-counter] address))))

(defopcode and
  0x21 address-mode/indirect-x  :cycles 6
  0x25 address-mode/zero-page   :cycles 3
  0x29 address-mode/immediate   :cycles 2
  0x2D address-mode/absolute    :cycles 4
  0x31 address-mode/indirect-y  :cycles 5
  0x35 address-mode/zero-page-y :cycles 4
  0x39 address-mode/absolute-y  :cycles 4
  0x3D address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        a (b/and (cpu/read-register nes :accumulator) data)]
    (-> nes
        (cpu/set-register :accumulator a)
        (cpu/set-flag :zero (zero? a))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 a)))
        (cpu/mark-extra-cycle))))

(defopcode bit
  0x24 address-mode/zero-page :cycles 3
  0x2C address-mode/absolute  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        anded (b/and (cpu/read-register nes :accumulator) data)]
    (-> nes
        (cpu/set-flag :zero (zero? (b/and anded 0x00FF)))
        (cpu/set-flag :negative (b/truthy? (b/and data (b/<< 1 7))))
        (cpu/set-flag :overflow (b/truthy? (b/and data (b/<< 1 6)))))))

(defopcode rol
  0x26 address-mode/zero-page   :cycles 5
  0x2A address-mode/accumulator :cycles 2
  0x2E address-mode/absolute    :cycles 6
  0x36 address-mode/zero-page-x :cycles 6
  0x3E address-mode/absolute-x  :cycles 7
  [nes address]
  (let [data (b/or (b/<< (cpu/read nes address) 1)
                   (cpu/get-flag nes :carray))
        accumulator-mode? (= address (cpu/read-register nes :accumulator))
        nes (-> nes
                (cpu/set-flag :carry (b/truthy? (b/and data 0xFF00)))
                (cpu/set-flag :zero (zero? (b/and data 0x00FF)))
                (cpu/set-flag :negative (b/truthy? (b/and data 0x80))))]
    (if accumulator-mode?
      (cpu/set-register nes :accumulator (b/and data 0x00FF))
      (cpu/write nes address (b/and data 0x00FF)))))

(defopcode plp
  0x28 address-mode/implied :cycles 4
  [nes _]
  (as-> nes $
    (update $ :stack-pointer inc)
    (cpu/set-register $ :status (cpu/read $ (+ 0x0100 (:stack-pointer $))))
    (cpu/set-flag $ :_ true)))

(defopcode bmi
  0x30 address-mode/relative :cycles 2
  [nes address]
  (if (cpu/get-flag nes :negative)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc-in [:cpu :program-counter] address)
          (cond-> new-page? (update-in [:cpu :cycles] inc))))
    nes))

(defopcode sec
  0x38 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :carry true))

(defopcode rti
  0x40 address-mode/implied :cycles 6
  [nes _]
  (let [stack (get-in nes [:cpu :stack-pointer])
        break (cpu/get-flag nes :break)
        unused (cpu/get-flag nes :_)
        new-status (->> stack
                        (+ 0x0100)
                        (cpu/read nes)
                        (b/and (b/not break))
                        (b/and (b/not unused)))
        lo (cpu/read nes (+ 0x0100 (+ stack 2)))
        hi (cpu/read nes (+ 0x0100 (+ stack 3)))]
    (-> nes
        (assoc-in [:cpu :program-counter (b/->16-bit hi lo)])
        (cpu/set-register :status new-status))))

(defopcode eor
  0x41 address-mode/indirect-y  :cycles 5
  0x45 address-mode/zero-page   :cycles 3
  0x49 address-mode/immediate   :cycles 2
  0x4D address-mode/absolute    :cycles 4
  0x51 address-mode/indirect-y  :cycles 5
  0x55 address-mode/zero-page-x :cycles 4
  0x59 address-mode/absolute-y  :cycles 4
  0x5D address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        a (b/xor (cpu/read-register nes :accumulator) data)]
    (-> nes
        (cpu/set-register :accumulator a)
        (cpu/set-flag :zero (zero? a))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 a)))
        (cpu/mark-extra-cycle))))

(defopcode lsr
  0x46 address-mode/zero-page   :cycles 5
  0x4A address-mode/accumulator :cycles 5
  0x4E address-mode/absolute    :cycles 6
  0x56 address-mode/zero-page-x :cycles 6
  0x5E address-mode/absolute-x  :cycles 7
  [nes address]
  ;; TODO: implied vs accumulaor mode returning `nil`.
  (let [implied? (nil? address)
        data (cpu/read nes address)
        shifted (b/>> data 1)
        lo (b/and shifted 0x00FF)
        nes (-> nes
                (cpu/set-flag :carry (b/truthy? (b/and data 0x0001)))
                (cpu/set-flag :zero (zero? lo))
                (cpu/set-flag :negative (b/truthy? (b/and shifted 0x0080))))]
    (if implied?
      (cpu/set-register nes :accumulator lo)
      (cpu/write nes address lo))))

(defopcode pha
  0x48 address-mode/implied :cycles 3
  [nes _]
  (let [stack (get-in nes [:cpu :stack-pointer])
        a (cpu/read-register nes :accumulator)]
    (-> nes
        (cpu/write (+ 0x0100 stack) a)
        (update-in [:cpu :stack-pointer] dec))))

(defopcode jmp
  0x4C address-mode/absolute :cycles 3
  0x6C address-mode/indirect :cycles 5
  [nes address]
  (assoc-in nes [:cpu :program-counter] address))

(defopcode bvc
  0x50 address-mode/relative :cycles 2
  [nes relative-address]
  (if-not (cpu/get-flag nes :overflow)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update-in [:cpu :cycles] inc)
          (assoc-in [:cpu :program-counter] address)
          (cond-> new-page? (update-in [:cpu :cycles] inc))))
    nes))

(defopcode cli
  0x58 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-register nes :disable-interrupt false))

(defopcode rts
  0x60 address-mode/implied :cycles 6
  [nes _]
  (let [stack (get-in nes [:cpu :stack-pointer])
        lo (cpu/read nes (+ 0x0100 stack))
        hi (cpu/read nes (+ 0x0100 (inc stack)))
        program-counter (b/->16-bit hi lo)]
    (-> nes
        (update-in [:cpu :stack-pointer] + 2)
        (assoc-in [:cpu :program-counter] program-counter))))

(defopcode adc
  0x61 address-mode/indirect-x  :cycles 6
  0x65 address-mode/zero-page   :cycles 3
  0x69 address-mode/immediate   :cycles 2
  0x6D address-mode/absolute    :cycles 4
  0x71 address-mode/indirect-y  :cycles 5
  0x75 address-mode/zero-page-x :cycles 4
  0x79 address-mode/absolute-y  :cycles 4
  0x7D address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        a (cpu/read-register nes :accumulator)
        value (+ a data (cpu/get-flag nes :carry))
        lo (b/and value 0x00FF)
        overflowed? (b/and 0x0080
                           (b/not (b/xor a data))
                           (b/xor a value))]
    (-> nes
        (cpu/set-flag :carry (> value 255))
        (cpu/set-flag :zero (zero? lo))
        (cpu/set-flag :negative (b/truthy? (b/and value 0x80)))
        (cpu/set-flag :overflow overflowed?)
        (cpu/set-register :accumulator lo)
        (cpu/mark-extra-cycles))))

(defopcode ror
  0x66 address-mode/zero-page   :cycles 5
  0x6A address-mode/accumulator :cycles 2
  0x6E address-mode/absolute    :cycles 6
  0x76 address-mode/zero-page-x :cycles 6
  0x7E address-mode/absolute-x  :cycles 7
  [nes address]
  ;; TODO: implied vs accumulaor mode returning `nil`.
  (let [implied? (nil? address)
        data (cpu/read nes address)
        shifted (-> nes
                    (cpu/get-flag :carry)
                    (b/<< 7)
                    (b/or (b/>> data 1)))
        lo (b/and shifted 0x00FF)
        nes (-> nes
                (cpu/set-flag :carry (b/truthy? (b/and data 0x01)))
                (cpu/set-flag :zero (zero? lo))
                (cpu/set-flag :negative (b/truthy? (b/and shifted 0x0080))))]
    (if implied?
      (cpu/set-register nes :accumulator lo)
      (cpu/write nes address lo))))

(defopcode pla
  0x68 address-mode/implied :cycles 4
  [nes _]
  (let [stack (inc (get-in nes [:cpu :stack-pointer]))
        a (cpu/read nes stack)]
    (-> nes
        (cpu/set-register :accumulator a)
        (cpu/set-flag :zero (zero? a))
        (cpu/set-flag :negative (b/truthy? (b/and a 0x80)))
        (update-in [:cpu :stack-pointer] inc))))

(defopcode bvs
  0x70 address-mode/relative :cycles 2
  [nes relative-address]
  (if-not (cpu/get-flag nes :overflow)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update-in [:cpu :cycles] inc)
          (assoc-in [:cpu :program-counter] address)
          (cond-> new-page? (update-in [:cpu :cycles] inc))))
    nes))

(defopcode sei
  0x78 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :disable-interrupt true))

(defopcode sta
  0x81 address-mode/indirect-x  :cycles 6
  0x85 address-mode/zero-page   :cycles 3
  0x8D address-mode/absolute    :cycles 4
  0x91 address-mode/indirect-x  :cycles 6
  0x95 address-mode/zero-page-x :cycles 4
  0x99 address-mode/absolute-y  :cycles 5
  0x9D address-mode/absolute-x  :cycles 5
  [nes address]
  (cpu/write nes address (cpu/read-register nes :accumulator)))

(defopcode sty
  0x84 address-mode/zero-page   :cycles 3
  0x8C address-mode/absolute    :cycles 4
  0x94 address-mode/zero-page-x :cycles 4
  [nes address]
  (cpu/write nes address (cpu/read-register nes :y)))

(defopcode stx
  0x86 address-mode/zero-page   :cycles 6
  0x8E address-mode/absolute    :cycles 4
  0x96 address-mode/zero-page-y :cycles 4
  [nes address]
  (cpu/write nes address (cpu/read-register nes :x)))

(defopcode dey
  0x86 address-mode/implied :cycles 2
  [nes _]
  (let [value (dec (cpu/read-register nes :y))]
    (-> nes
        (cpu/set-register :y value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode txa
  0x8A address-mode/implied :cycles 2
  [nes _]
  (let [value (cpu/read-register nes :x)]
    (-> nes
        (cpu/set-register :accumulator value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode bcc
  0x90 address-mode/relative :cycles 2
  [nes relative-address]
  (if-not (cpu/get-flag nes :carry)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc :program-counter address)
          (cond-> new-page? (update :cycles inc))))
    nes))

(defopcode tya
  0x98 address-mode/implied :cycles 2
  [nes _]
  (let [value (cpu/read-register nes :y)]
    (-> nes
        (cpu/set-register :accumulator value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode txs
  0x9A address-mode/implied :cycles 2
  [nes _]
  (let [value (cpu/read-register nes :x)]
    (assoc-in nes [:cpu :stack-pointer] value)))

(defopcode ldy
  0xA0 address-mode/immediate   :cycles 2
  0xA4 address-mode/zero-page   :cycles 3
  0xAC address-mode/absolute    :cycles 4
  0xB4 address-mode/zero-page-x :cycles 4
  0xBC address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)]
    (-> nes
        (cpu/set-register :y data)
        (cpu/set-flag :zero (zero? data))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 data)))
        (cpu/mark-extra-cycle))))

(defopcode lda
  0xA1 address-mode/indirect-x  :cycles 6
  0xA5 address-mode/zero-page   :cycles 3
  0xA9 address-mode/immediate   :cycles 2
  0xAD address-mode/absolute    :cycles 4
  0x91 address-mode/indirect-y  :cycles 5
  0xB5 address-mode/zero-page-x :cycles 4
  0xB9 address-mode/absolute-y  :cycles 4
  0xBD address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)]
    (-> nes
        (cpu/set-register :x data)
        (cpu/set-flag :zero (zero? data))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 data)))
        (cpu/mark-extra-cycle))))

(defopcode ldx
  0xA2 address-mode/immediate   :cycles 2
  0xA6 address-mode/zero-page   :cycles 3
  0xAE address-mode/absolute    :cycles 4
  0xB6 address-mode/zero-page-y :cycles 4
  0xBE address-mode/absolute-y  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)]
    (-> nes
        (cpu/set-register :x data)
        (cpu/set-flag :zero (zero? data))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 data)))
        (cpu/mark-extra-cycle))))

(defopcode tay
  0xA8 address-mode/implied :cycles 2
  [nes _]
  (let [value (cpu/read-register nes :accumulator)]
    (-> nes
        (cpu/set-register :y value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode tax
  0xAA address-mode/implied :cycles 2
  [nes _]
  (let [value (cpu/read-register nes :accumulator)]
    (-> nes
        (cpu/set-register :x value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode bcs
  0xB0 address-mode/relative :cycles 2
  [nes relative-address]
  (if (cpu/get-flag nes :carry)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc :program-counter address)
          (cond-> new-page? (update :cycles inc))))
    nes))

(defopcode clv
  0xB8 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :overflow false))

(defopcode tsx
  0xBA address-mode/implied :cycles 2
  [nes _]
  (let [value (get-in nes [:cpu :stack-pointer])]
    (-> nes
        (cpu/set-register :x value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode cpy
  0xC0 address-mode/immediate :cycles 2
  0xC4 address-mode/zero-page :cycles 3
  0xCC address-mode/absolute  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        y (cpu/read-register nes :y)
        comparison (- y data)]
    (-> nes
        (cpu/set-flag :carry (>= y data))
        (cpu/set-flag :zero (zero? (b/and comparison 0x00FF)))
        (cpu/set-flag :negative (b/truthy? (b/and comparison 0x0080))))))

(defopcode cmp
  0xC1 address-mode/indirect-x  :cycles 6
  0xC5 address-mode/zero-page   :cycles 3
  0xC9 address-mode/immediate   :cycles 2
  0xCD address-mode/absolute    :cycles 4
  0xD1 address-mode/indirect-y  :cycles 5
  0xD5 address-mode/zero-page-x :cycles 5
  0xD9 address-mode/absolute-y  :cycles 4
  0xDD address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        a (cpu/read-register nes :accumulator)
        comparison (- a data)]
    (-> nes
        (cpu/set-flag :carry (>= a data))
        (cpu/set-flag :zero (zero? (b/and comparison 0x00FF)))
        (cpu/set-flag :negative (b/truthy? (b/and comparison 0x0080)))
        (cpu/mark-extra-cycle))))

(defopcode dec
  0xC6 address-mode/zero-page   :cycles 5
  0xCE address-mode/absolute    :cycles 6
  0xD6 address-mode/zero-page-x :cycles 6
  0xDE address-mode/absolute-x  :cycles 7
  [nes address]
  (let [data (dec (cpu/read nes address))]
    (-> nes
        (cpu/write address (b/and data 0x00FF))
        (cpu/set-register :zero (zero? (b/and data 0x00FF)))
        (cpu/set-register :negative (b/truthy? (b/and data 0x0080))))))

(defopcode iny
  0xC8 address-mode/implied :cycles 2
  [nes _]
  (let [value (inc (cpu/read-register nes :y))]
    (-> nes
        (cpu/set-register :y value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and value 0x80))))))

(defopcode dex
  0xCA address-mode/implied :cycles 2
  [nes _]
  (let [value (dec (cpu/read-register nes :x))]
    (-> nes
        (cpu/set-register :y value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode bne
  0xD0 address-mode/relative :cycles 2
  [nes relative-address]
  (if-not (cpu/get-flag nes :zero)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc :program-counter address)
          (cond-> new-page? (update :cycles inc))))
    nes))

(defopcode cld
  0xD8 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :decimal false))

(defopcode cpx
  0xE0 address-mode/immediate :cycles 2
  0xE4 address-mode/zero-page :cycles 3
  0xEC address-mode/absolute  :cycles 4
  [nes address]
  (let [data (cpu/read nes address)
        x (cpu/read-register nes :x)
        comparison (- x data)]
    (-> nes
        (cpu/set-flag :carry (>= x data))
        (cpu/set-flag :zero (zero? (b/and comparison 0x00FF)))
        (cpu/set-flag :negative (b/truthy? (b/and comparison 0x0080))))))

;; TODO: addition is just substraction with a slight inversion.
(defopcode sbc
  0xE1 address-mode/indirect-x  :cycles 6
  0xE5 address-mode/zero-page   :cycles 3
  0xE9 address-mode/immediate   :cycles 2
  0xED address-mode/absolute    :cycles 4
  0xF1 address-mode/indirect-y  :cycles 5
  0xF5 address-mode/zero-page-x :cycles 5
  0xF9 address-mode/absolute-y  :cycles 4
  0xFD address-mode/absolute-x  :cycles 4
  [nes address]
  (let [data (b/xor (cpu/read nes address) 0x00FF)
        a (cpu/read-register nes :accumulator)
        value (+ a data (cpu/get-flag nes :carry))
        lo (b/and value 0x00FF)
        overflowed? (b/and 0x0080
                           (b/not (b/xor a data))
                           (b/xor a value))]
    (-> nes
        (cpu/set-flag :carry (> value 255))
        (cpu/set-flag :zero (zero? lo))
        (cpu/set-flag :negative (b/truthy? (b/and value 0x80)))
        (cpu/set-flag :overflow overflowed?)
        (cpu/set-register :accumulator lo)
        (cpu/mark-extra-cycles))))

(defopcode inc
  0xE6 address-mode/zero-page   :cycles 5
  0xEE address-mode/absolute    :cycles 6
  0xF6 address-mode/zero-page-x :cycles 6
  0xFE address-mode/absolute-x  :cycles 7
  [nes address]
  (let [data (inc (cpu/read nes address))]
    (-> nes
        (cpu/write address (b/and data 0x00FF))
        (cpu/set-register :zero (zero? (b/and data 0x00FF)))
        (cpu/set-register :negative (b/truthy? (b/and data 0x0080))))))

(defopcode inx
  0xE8 address-mode/implied :cycles 2
  [nes _]
  (let [value (inc (cpu/read-register nes :x))]
    (-> nes
        (cpu/set-register :y value)
        (cpu/set-flag :zero (zero? value))
        (cpu/set-flag :negative (b/truthy? (b/and 0x80 value))))))

(defopcode nop
  0xEA address-mode/implied :cycles 2
  [nes _]
  nes)

(defopcode beq
  0xF0 address-mode/relative :cycles 2
  [nes relative-address]
  (if (cpu/get-flag nes :zero)
    (let [program-counter (get-in nes [:cpu :program-counter])
          address (+ program-counter relative-address)
          new-page? (!= (b/and address 0xFF00)
                        (b/and program-counter 0xFF00))]
      (-> nes
          (update :cycles inc)
          (assoc :program-counter address)
          (cond-> new-page? (update :cycles inc))))
    nes))

(defopcode sed
  0xF8 address-mode/implied :cycles 2
  [nes _]
  (cpu/set-flag nes :decimal true))

(defn run [nes program-counter]
  (let [{:keys [op-fn]} (get @opcodes program-counter)]
    (-> nes
        (op-fn)
        ;; Address mode/opcode fns will mark the cycle count
        ;; if they may need another cycle.
        ;;
        ;; After everything has finished, it'll either be exactly
        ;; 1 cycle more (if both address mode + opcode needed a cycle)
        ;; or .5 more if only one of them needed it. `int` will remove the
        ;; updates from the latter case.
        (update-in [:cpu :cycles] int))))
