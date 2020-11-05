(ns snes.binary
  (:refer-clojure :exclude [and or pr-str not]))

(defn and [& bytes]
  (apply bit-and bytes))

(defn or [& bytes]
  (apply bit-or bytes))

(defn xor [& bytes]
  (apply bit-xor bytes))

(defn not [byte]
  (bit-not byte))

(defn << [bytes amount]
  (bit-shift-left bytes amount))

(defn >> [bytes amount]
  (bit-shift-right bytes amount))

(defn pr-str [num]
  (.toString num 2))

(defn truthy? [num]
  (pos? num))

(defn ->16-bit [hi lo]
  (or (<< hi 8) lo))
