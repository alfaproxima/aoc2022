(ns aoc2022.day3
 (:require 
  [clojure.string :as str]
  [clojure.set :as set]))

(def sample 
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def data (slurp "src/day3/data"))

(def alphabet
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def priorities
  (zipmap (str/split alphabet #"") (range 1 53)))

(defn parse [data]
  (map 
    (fn [s] 
      (map #(set %) 
          (partition
            (/ (count s) 2) 
            (str/split s #""))))
    (str/split-lines data)))

(defn get-intersect [[f l]]
  (set/intersection f l))

(defn part1 [data]
  (reduce + 0
    (for [sym (map get-intersect data)]
      (priorities (first sym)))))

(part1 (parse data))

(defn parse-part2 [data]
  (partition 3 (str/split-lines data)))

(defn get-common-badge [list]
  (apply set/intersection (map set list)))

(defn part2 [data]
  (reduce + 0
    (for [sym (map #(get-common-badge %) data)]
      (priorities (str (first sym))))))

(part2 (parse-part2 data))


