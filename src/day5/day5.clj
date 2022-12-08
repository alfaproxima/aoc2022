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

(def data (slurp "src/day5/data"))

(defn parse [data]
  (str/split-lines data))

;     [D]    
; [N] [C]    
; [Z] [M] [P]
;  1   2   3 -> line separator, iterate trough it
;  ^   ^   ^-> take all indicies (9) and push to the array 3
;  |   |->take all indexes (5) and push to the array 2
;  -> take all indexes (2) and push to the array 1

(defn parse-stacks [data]
  (let [index-line (find-index-line data)
        string (index-line :line)
        stacks (take (- (index-line :index) 1) data)]
    (loop [n 0
           letter (.charAt string n)
           result []]
      (if (< n (count string))
        (if (digit? letter)
          (let [res (conj result (fill-stack stacks (- n 1)))]
            (recur (inc n) (.charAt string n) res))
          (recur (inc n) (.charAt string n) result))
        result))))

(defn find-index-line [data]
  (loop [n 0 line (nth data n)]
  (if (< n (- (count data) 1))
    (if
      (digit? (str/replace line #"\s" ""))
        {:index n  :line line}
        (recur (inc n) (nth data n))))))

(defn digit? [string]
  (boolean (re-matches #"\d+" (str string))))

(defn parse-commands [data]
  (let [start (:index (find-index-line data))
        commands (subvec data (+ start 1))]
    (->>
      commands
      (mapv #(re-seq #"\d+" %))
      (mapv #(zipmap [:amount :from :to] %)))))

(defn fill-stack [stacks index]
  (vec
    (filter #(re-matches #"\w+" %)
      (mapv #(str (.charAt % index)) stacks))))

(defn run-commands [data movefn]
  (let [stacks (parse-stacks data)
        commands (parse-commands data)]
    (loop [n 0
           {:keys [amount from to]} (nth commands n)
           result stacks]
      (if (< n (count commands))
          (recur 
            (inc n)
            (nth commands (+ n 1) [])
            (movefn result (parse-long amount) (parse-long from) (parse-long to)))
          result)
)))

(defn one-from-stack [stacks from to]
  (let [crate (first (nth stacks (- from 1)))
        upd (update stacks (- from 1) #(subvec % 1))]
    (update upd (- to 1) #(vec (concat [crate] %)))))

(defn move-crates [stacks amount from to]
  (reduce
    (fn [stack acc]
      (one-from-stack stack from to))
    stacks
    (range amount)))

(defn move-crates2 [stacks amount from to]
  (let [crates (subvec (nth stacks (- from 1)) 0 amount)
        upd (update stacks (- from 1) #(subvec % amount))]
    (update upd (- to 1) #(vec (concat crates %)))))
 
(defn part1 [data]
  (str/join "" 
      (mapv first (run-commands data move-crates))))

(part1 (parse sample))
(part1 (parse data))

(defn part2 [data]
  (str/join "" 
      (mapv first (run-commands data move-crates2))))

(part2 (parse sample))
(part2 (parse data))