(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce (fn [acc row] (conj acc (get row col)))
            #{}
            board)))

(defn coord-pairs
  ([coords]
   (for [rows coords
         cols coords]
     [rows cols]))
  ([[first-row first-col] block-width]
   (for [row (range first-row (+ first-row block-width))
         col (range first-col (+ first-col block-width))]
     [row col])))

(defn block-corner [[row col]]
  (let [lowerbound (fn [n] (cond
                             (< n 3) 0
                             (< n 6) 3
                             :else 6))
        lrow (lowerbound row)
        lcol (lowerbound col)]
    [lrow lcol]))

(defn block-values [board coord]
  (let [[row col] (block-corner coord)
        coords (coord-pairs [row col] 3)]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [complimentary-values (set/union (row-values board coord)
                                  (col-values board coord)
                                  (block-values board coord))]
      (set/difference all-values complimentary-values))))

(defn board-values [board]
  (map #(value-at board %) (coord-pairs (range 0 9))))

(defn filled? [board]
  (not (contains? (set (board-values board)) 0)))

(defn rows [board]
  (reduce (fn [acc row] (conj acc (set row))) [] board))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (let [col-tops (map #(conj [0] %) (range 0 9))]
    (reduce (fn [acc col] (conj acc (col-values board col))) [] col-tops)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (let [block-centers [[1 1] [1 4] [1 7]
                       [4 1] [4 4] [4 7]
                       [7 1] [7 4] [7 7]]]
    (reduce (fn [acc block-center] (conj acc (block-values board block-center))) [] block-centers)))

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
