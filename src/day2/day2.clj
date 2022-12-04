(ns aoc2022.day2
 (:require 
  [clojure.string :as str]
  [clojure.set :as set]))

; A - rock,  B - paper,    C - scissors
; Y - paper, Z - scissors, X - rock
(def sample
  "A Y
B X
C Z")

(def data (slurp "src/day2/data"))

(defn parse [data]
  (partition 2
    (re-seq #"\w" data)))

(defn parseShapes [[left right]]
  [({"A" :rock "B" :paper "C" :scissors} left)
   ({"X" :rock "Y" :paper "Z" :scissors} right)])

; get score for a shape
; get score for a outcome (win, draw, loose)
(defn score [[left right]]
   (+ ({:rock 1 :paper 2 :scissors 3} right)
    (cond
      (= left right) 3
      (= [:rock :paper] [left right]) 6
      (= [:paper :scissors] [left right]) 6
      (= [:scissors :rock] [left right]) 6
      :else 0)))

(defn part1 [data]
  (reduce + 0
    (map score
      (map parseShapes data))))

(part1 (parse sample))
(part1 (parse data))

(defn parseShapesAndOutcome [[left right]]
  [({"A" :rock "B" :paper "C" :scissors} left)
   ({"X" :loose "Y" :draw "Z" :win} right)])

(defn calcRound [[l r]]
  (case r
    :win (score [l ({:rock :paper
                     :paper :scissors
                     :scissors :rock} l)])
    :loose (score [l ({:rock :scissors
                       :paper :rock
                       :scissors :paper} l)])
    :draw (score [l l])))

(defn part2 [data]
  (reduce + 0
    (map calcRound
      (map parseShapesAndOutcome data))))

(part2 (parse sample))
(part2 (parse data))