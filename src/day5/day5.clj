(ns aoc2022.day4
  (:require 
    [clojure.string :as str]))

(def sample
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse [data]
  (str/split-lines data))

(defn find-index-line [data]
  (loop [n 0 line (nth data n)]
  (if (< n (- (count data) 1))
    (if
      (number?? (str/replace line #"\s" ""))
        {:index n  :line line}
        (recur (inc n) (nth data n))))))

(defn number?? [string]
  (boolean (re-matches #"\d+" (str string))))

;     [D]    
; [N] [C]    
; [Z] [M] [P]
;  1   2   3 -> line separator (every digit at index of a letter)
;  ^   ^   ^-> take all indicies (9) to array 3
;  |   |->take all indexes (5) to array 2
;  -> take all indexes (2) to array 1

(defn parse-stacks [data]
  (let [index-line (find-index-line data)
        string (index-line :line)
        stacks (take (- (index-line :index) 1) data)]
    (loop [n 0
           letter (.charAt string n)
           result {}]
      (if (< n (count string))
        (if (number?? letter)
          (let [res (conj result {(str letter) (fill-stack stacks (- n 1))})]
            (recur (inc n) (.charAt string n) res))
          (recur (inc n) (.charAt string n) result))
        result))))

(defn parse-commands [data]
  (let [start (:index (find-index-line data))
        commands (subvec data (+ start 1))]
    (->>
      commands
      (mapv #(re-seq #"\d+" %))
      (mapv #(zipmap [:crate :from :to] %)))))

(defn fill-stack [stacks index]
  (mapv #(str (.charAt % index)) stacks))

(defn part1 [data]
  (let [stacks (parse-stacks data)
        commands (parse-commands data)])
    ; loop on commands
    ; move letters in stacks
    ; take first chars from each stack
)