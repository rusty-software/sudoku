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

(defn coord-pairs [coords]
  (for [rows coords
        cols coords]
    [rows cols]))

(defn block-corner [[row col]]
  (let [lowerbound (fn [n] (cond
                             (< n 3) 0
                       (< n 6) 3
                       :else 6))
        lrow (lowerbound row)
        lcol (lowerbound col)]
    [lrow lcol]))

(defn block-values [board coord]
  (let [lcorner (block-corner coord)
        l (first lcorner)
        coords (coord-pairs [l (+ 1 l) (+ 2 l)])]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [complimentary-values (set/union (row-values board coord)
                                  (col-values board coord)
                                  (block-values board coord))]
      (set/difference all-values complimentary-values))))

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

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
