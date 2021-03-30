(ns seating-system
  (:require [clojure.java.io :as io]
            [lambdaisland.deep-diff2 :as ddiff]
            [clojure.string :as str]))

(def symbol->space
  {"." :floor
   "L" :empty-seat
   "#" :occupied-seat})

(defn spaces-chart-file->spaces-grid
  [file]
  (->> file
    io/resource
    slurp
    str/split-lines
    (mapv (fn [a-row-of-seating] (str/split a-row-of-seating #"")))
    (reduce
      (fn [grid row]
        (conj grid
          (mapv (fn [symbol]
                  (symbol->space symbol)
                  ) row)))
      [])))

(defn loc->adjacent-locs
  [[row-idx col-idx]]
  #{[(inc row-idx) col-idx]
    [(dec row-idx) col-idx]
    [row-idx (inc col-idx)]
    [row-idx (dec col-idx)]
    [(inc row-idx) (inc col-idx)]
    [(dec row-idx) (dec col-idx)]
    [(inc row-idx) (dec col-idx)]
    [(dec row-idx) (inc col-idx)]})

(defn loc+grid->spaces
  [loc grid]
  {:space           (get-in grid loc)
   :adjacent-spaces (map #(get-in grid %) (loc->adjacent-locs loc))})

;; --------------- SEATING RULES START ---------------------
;;If a seat (space) is empty and there are no occupied seats adjacent to it, the seat becomes occupied.
(defn is-empty-and-adjacent-empty?
  [{:keys [space adjacent-spaces]}]
  (let [is-space-empty? (fn
                          [space]
                          (or
                            (= :floor space)
                            ;; checking nil, because we access index's out of bounds and they have to count as empty.
                            (nil? space)
                            (= :empty-seat space)))]
    (every? is-space-empty?  (conj adjacent-spaces space))))

;; If a seat (space) is occupied and four or more seats adjacent to it are also occupied, the seat becomes empty.
(defn is-occupied-and-four-occupied?
  [{:keys [space adjacent-spaces]}]
  (and
    (= :occupied-seat space)
    (-> adjacent-spaces
      frequencies
      (get :occupied-seat 0)
      (>= 4))))
;; --------------- SEATING RULES END ---------------------

(defn loc+grid->new-space
  [{:keys [space] :as spaces}]
  (cond
    (= :floor space)                        :floor
    (is-empty-and-adjacent-empty? spaces)   :occupied-seat
    (is-occupied-and-four-occupied? spaces) :empty-seat
    :else                                   space))

(defn grid->next-grid
  [grid]
  (reduce-kv
    (fn [new-grid row-idx row]
      (conj new-grid
        (reduce-kv
          (fn [new-row col-idx _]
            (conj new-row (->> grid
                            (loc+grid->spaces [row-idx col-idx])
                            loc+grid->new-space)))
          []
          row)))
    []
    grid))

(defn grid->grid-log
  [log]
  (let [grid         (last log)
        next-grid    (grid->next-grid grid)
        new-grid-log (conj log next-grid)]
    (if (= next-grid grid)
      log
      (grid->grid-log new-grid-log))))

(defn grid->final-grid
  [grid]
  (->> [grid]
    grid->grid-log
    last))

(defn grid->occupied-count
  [grid]
  (->> grid
    flatten
    frequencies
    :occupied-seat))

(comment

  (let [expected-grid-log
        (->> "seating-chart-output.txt"
          io/resource
          slurp
          str/split-lines
          (mapv (fn [a-row-of-seating] (str/split a-row-of-seating #"")))
          (partition-by #(= [""] %))
          (remove #(= [[""]] %))
          (map (fn [symbols]
                 (reduce
                   (fn [grid row]
                     (conj grid
                       (mapv (fn [symbol]
                               (symbol->space symbol)
                               ) row)))
                   []
                   symbols))))

        ;; ---------- START HERE! ---------
        ;; Goal is to find the number of occupied spaces in a seating-chart, which we call a grid.
        ;; To do this a were given an `example-init-grid`

        example-init-grid (spaces-chart-file->spaces-grid "seating-chart-example.txt")

        ;; and we create a grid and then get the occupied count.
        example-occupied-count (->> example-init-grid
                                 grid->final-grid
                                 grid->occupied-count)]


    ;; which in the example should be  equal to 37
    (= example-occupied-count 37)



    ;; ----------- HELPFUL DEBUG INFO ---------------------
    ;; 1. `seating-chart-output` is the grid log output that should be produced.
    ;; 2. diff library to inspect differences in deeply nested data structures


    ;; a way to inspect how the structures diff, check repl
    #_(ddiff/pretty-print (ddiff/diff expected-grid-log example-grid-log))
    )

  )
