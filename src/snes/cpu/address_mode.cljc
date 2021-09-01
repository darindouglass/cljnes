(ns snes.cpu.address-mode
  "All address-mode fn's are provided the NES state
  and the byte immediately after the opcode byte for convenience.

  They return a vector of new NES state and the address they've concocted."
  (:require [snes.binary :as b]
            [snes.cpu.impl :as cpu]))

(defn implied
  "The no-op of address-modes. Address detection is handled by the opcode."
  [nes _]
  [nes nil])

(defn accumulator
  "Load address from the accumulator."
  [nes _]
  [nes (cpu/read-register nes :accumulator)])

(defn immediate
  "Load the bytes at the current program counter."
  [nes program-counter]
  [(cpu/inc-program-counter nes) program-counter])

(defn zero-page
  "Addresses an offset in the zeroth page."
  [nes program-counter]
  (let [address (b/and (cpu/read nes program-counter) 0x00FF)]
    [(cpu/inc-program-counter nes) address]))

(defn zero-page-x
  "Addresses an offset in the zeroth page after adding the value in the `x` as an offset."
  [nes program-counter]
  (let [address (->> program-counter
                     (cpu/read nes)
                     (+ (cpu/read-register nes :x))
                     (b/and 0x00FF))]
    [(cpu/inc-program-counter nes) address]))

(defn zero-page-y
  "Addresses an offset in the zeroth page after adding the value in the `x` as an offset."
  [nes program-counter]
  (let [address (->> program-counter
                     (cpu/read nes)
                     (+ (cpu/read-register nes :y))
                     (b/and 0x00FF))]
    [(cpu/inc-program-counter nes) address]))

(defn relative
  "Used by branching instructions, must be within -128,+127 bytes of the instruction."
  [nes program-counter]
  (let [address (cpu/read nes program-counter)
        relative-address (if (b/truthy? (b/and address 0x80))
                           (b/and address 0xFF00)
                           address)]
    [(cpu/inc-program-counter nes) relative-address]))

(defn absolute
  "Reads a full 16-byte address."
  [nes program-counter]
  (let [lo (cpu/read nes program-counter)
        hi (cpu/read nes (inc program-counter))]
    [nes (b/->16-bit hi lo)]))

(defn absolute-x
  "Reads a full 16-byte address adding the value in the `x` register."
  [nes program-counter]
  (let [lo (cpu/read nes program-counter)
        hi (cpu/read nes (inc program-counter))
        address (+ (b/or (b/<< hi 8) lo)
                   (cpu/read-register nes :x))
        new-page? (= (b/and address 0xFF00)
                     (b/<< hi 8))]
    [(-> nes
         ;; +1 inc for our extra read
         (cpu/inc-program-counter)
         (cpu/inc-program-counter)
         (cond-> new-page? (cpu/mark-extra-cycle)))
     address]))

(defn absolute-y
  "Reads a full 16-byte address adding the value in the `y` register."
  [nes program-counter]
  (let [lo (cpu/read nes program-counter)
        hi (cpu/read nes (inc program-counter))
        address (+ (b/or (b/<< hi 8) lo)
                   (cpu/read-register nes :y))
        new-page? (= (b/and address 0xFF00)
                     (b/<< hi 8))]
    [(-> nes
         ;; +1 inc for our extra read
         (cpu/inc-program-counter)
         (cpu/inc-program-counter)
         (cond-> new-page? (cpu/mark-extra-cycle)))
     address]))

(defn indirect
  "The read bytes act as a pointer. There's a hardware bug that we need to account for."
  [nes program-counter]
  (let [lo (cpu/read nes program-counter)
        hi (cpu/read nes (inc program-counter))
        pointer (b/->16-bit hi lo)
        real-lo (cpu/read nes pointer)
        ;; simulate a hardware bug
        real-hi (if (= lo 0x00FF)
                  (cpu/read nes (b/and pointer 0xFF00))
                  (cpu/read nes (inc pointer)))]
    [(-> nes
         ;; +1 inc for our extra read
         (cpu/inc-program-counter)
         (cpu/inc-program-counter))
     (b/->16-bit real-hi real-lo)]))

(defn indirect-x
  "The read byte + `x` register act as a pointer."
  [nes program-counter]
  (let [pointer (+ (cpu/read nes program-counter)
                   (cpu/read-register nes :x))
        lo (cpu/read nes (b/and pointer 0x00FF))
        hi (cpu/read nes (b/and (inc pointer) 0x00FF))]
    [(cpu/inc-program-counter nes) (b/->16-bit hi lo)]))

(defn indirect-y
  "The read a full 16-bit address from the pointer, adding the value from the `y`
  register."
  [nes program-counter]
  (let [pointer (cpu/read nes program-counter)
        lo (cpu/read nes (b/and pointer 0x00FF))
        hi (cpu/read nes (b/and (inc pointer) 0x00FF))
        address (+ (b/->16-bit hi lo)
                   (cpu/read-register nes :y))
        new-page? (= (b/and address 0xFF00) (b/<< hi 8))]
    [(-> nes
         (cpu/inc-program-counter)
         (cond-> new-page? (cpu/mark-extra-cycle)))
     address]))
