(ns aoc2022.day1
  (:require 
    [clojure.string :as str]
    [clojure.set :as set]))

(def testData
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn parse [data]
  (->>
    (str/split data #"\r\n\r\n")
    (mapv #(->> (str/split % #"\r\n") (mapv parse-long)))))

(defn naiveParse [data]
  (mapv (fn [n] 
    (mapv parse-long (str/split n #"\n"))) 
  (str/split data #"\n\n")))

(def data (slurp "src/day1/data"))

(defn findMax [data]
  (apply max 
    (map (fn [arr] (reduce + 0 arr)) 
      data)))

(findMax (naiveParse testData))
(findMax (parse data))

(defn findThreeBiggest [data]
  (reduce + 0
    (take 3
      (reverse
        (sort
          (map
            (fn [arr] (reduce + 0 arr)) 
            data))))))

(findThreeBiggest (naiveParse testData))
(findThreeBiggest (parse data))

