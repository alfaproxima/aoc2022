(ns aoc2022.day6
  (:require 
    [clojure.string :as str]))

(def sample "bvwbjplbgvbhsrlpgdmjqwftvncz")

(def data (slurp "src/day6/data"))

(defn find-marker [n data]
  (some 
    #(let [s (subs data % (+ n %))]
      (when (apply distinct? (seq s))
        (+ % n)))
    (range 0 (- (count data) n))))

(find-marker 4 sample)
(find-marker 4 data)

(find-marker 14 sample)
(find-marker 14 data)