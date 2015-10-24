(ns sudoku
  (:require [clojure.set :as set]))

(def sample-board [[5 3 0 0 7 0 0 0 0]
                   [6 0 0 1 9 5 0 0 0]
                   [0 9 8 0 0 0 0 6 0]
                   [8 0 0 0 6 0 0 0 3]
                   [4 0 0 8 0 3 0 0 1]
                   [7 0 0 0 2 0 0 0 6]
                   [0 6 0 0 0 0 2 8 0]
                   [0 0 0 4 1 9 0 0 5]
                   [0 0 0 0 8 0 0 7 9]])

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

(defn block-sig-num
  "Number of blocks in a row, in a column, and the block width"
  [board]
  (int (Math/sqrt (count board))))

(defn block-edges [sig-num]
  (vec (map #(* sig-num %) (range sig-num))))

(defn block-corner [board [row col]]
  (let [sig-num (block-sig-num board)
        bounds (block-edges sig-num)
        row-block (int (/ row sig-num))
        col-block (int (/ col sig-num))
        lowerbound (fn [n] (get bounds n))
        lrow (lowerbound row-block)
        lcol (lowerbound col-block)]
    [lrow lcol]))

(defn block-values [board coord]
  (let [[row col] (block-corner board coord)
        coords (coord-pairs [row col] (block-sig-num board))]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [complimentary-values (set/union (row-values board coord)
                                  (col-values board coord)
                                  (block-values board coord))]
      (set/difference all-values complimentary-values))))

(defn board-values [board]
  (map #(value-at board %) (coord-pairs (range 0 (count board)))))

(defn filled? [board]
  (not (contains? (set (board-values board)) 0)))

(defn rows [board]
  (reduce (fn [acc row] (conj acc (set row))) [] board))

(defn eq-all-values? [coll]
  (= all-values (set coll)))

(defn valid-rows? [board]
  (every? eq-all-values? (rows board)))

(defn cols [board]
  (let [col-tops (map #(conj [0] %) (range 0 (count board)))]
    (reduce (fn [acc col] (conj acc (col-values board col))) [] col-tops)))

(defn valid-cols? [board]
  (every? eq-all-values? (cols board)))

(defn blocks [board]
  (let [sig-num (block-sig-num board)
        block-corners (vec (for [rows (block-edges sig-num)
                                 cols (block-edges sig-num)]
                             [rows cols]))]
    (reduce (fn [acc block-corner] (conj acc (block-values board block-corner))) [] block-corners)))

(defn valid-blocks? [board]
  (every? eq-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (coord-pairs (range 0 (count board))))))

(defn solve-helper [current-board]
  (if (valid-solution? current-board)
    current-board
    (let [next-point (find-empty-point current-board)
          possible-values (valid-values-for current-board next-point)]
      (for [value possible-values
            solution (solve-helper (set-value-at current-board next-point value))]
        solution))))

(defn solve [board]
  (solve-helper board))
