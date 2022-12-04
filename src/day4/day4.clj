(ns aoc2022.day4
  (:require 
    [clojure.string :as str]))

(def sample "2-4,6-8
  2-3,4-5
  5-7,7-9
  2-8,3-7
  6-6,4-6
  2-6,4-8")

(def data (slurp "src/day4/data"))

(defn parse [data]
  (map
    #(partition 2 
      (mapv parse-long (re-seq #"\d+" %)))
    (str/split-lines data)))

(defn check [[lf ll] [rf rl]]
  (or
    (and
      (<= lf rf)
      (>= ll rl))
    (and
      (<= rf lf)
      (>= rl ll))))

(defn part1 [data]
  (count
    (filter
      #(apply check %) data)))

(part1 (parse data))

(defn check2 [[lf ll] [rf rl]]
  (>= ll rf))

; (defn part2 [data]
;   (count
;     (filter #(apply check2 %)
;       (mapv #(sort-by first %) data))))

(defn part2 [data]
    (->>
      data
      (mapv #(sort-by first %))
      (filter #(apply check2 %))
      (count)))

(part2 (parse data))