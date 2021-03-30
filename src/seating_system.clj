(ns seating-system
  (:require [clojure.java.io :as io]
            [lambdaisland.deep-diff2 :as ddiff]
            [clojure.string :as str]))



;; The seat layout fits neatly on a grid. Each position is either floor (.), an empty seat (L), or an occupied seat (#). For example, the initial seat layout might look like this:
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

(defn reverse-hash-map
  [m]
  (into {} (map (fn [[k v]] [v k]) m)))

(defn is-space-empty?
  [space]
  (or
    ;;TODO resolve is a floor seat next to you considered empty
    (= :floor space)
    ;; because we access index's out of bounds
    (nil? space)
    (= :empty-seat space)))

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

;;If a seat is empty and there are no occupied seats adjacent to it, the seat becomes occupied.
(defn is-empty-and-adjacent-empty?
  [loc grid]
  (let [adjacent-locs   (loc->adjacent-locs loc)
        space           (get-in grid loc)
        adjacent-spaces (map #(get-in grid %) adjacent-locs)]
    (every? is-space-empty?  (conj adjacent-spaces space))))

;; If a seat is occupied and four or more seats adjacent to it are also occupied, the seat becomes empty.
(defn is-occupied-and-four-occupied?
  [loc grid]
  (let [adjacent-locs   (loc->adjacent-locs loc)
        space           (get-in grid loc)
        adjacent-spaces (map #(get-in grid %) adjacent-locs)]
    (and
      (= :occupied-seat space)
      (-> adjacent-spaces
        frequencies
        (get :occupied-seat 0)
        (>= 4)))))


(defn loc->adjacent-spaces
  [loc grid]
  (map #(get-in grid %) (loc->adjacent-locs loc)))

(defn loc+grid->new-space
  [loc grid]
  (cond
    (= :floor (get-in grid loc))              :floor
    (is-empty-and-adjacent-empty? loc grid)   :occupied-seat
    (is-occupied-and-four-occupied? loc grid) :empty-seat
    :else                                     (get-in grid loc)))

(defn grid->next-grid
  [grid]
  (reduce-kv
    (fn [new-grid row-idx row]
      (conj new-grid
        (reduce-kv
          (fn [new-row col-idx _]
            (conj new-row (loc+grid->new-space [row-idx col-idx] grid)))
          []
          row)))
    []
    grid))

(defn grid->grid-log
  [log]
  (let [grid         (last log)
        next-grid    (grid->next-grid grid)
        new-grid-log (conj log next-grid)]
    (if (=  next-grid grid)
      log
      (grid->grid-log new-grid-log))))

(
 comment

 (def example-output-grids 
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
              symbols)))))

 (def init-grid (spaces-chart-file->spaces-grid "seating-chart-example.txt"))

 (def current-output (grid->grid-log [init-grid]) )

 (let [loc             [0 2]
       grid            (nth current-output 1)
       space           (get-in grid loc)
       adjacent-spaces (loc->adjacent-spaces loc grid)]
   (loc+grid->new-space loc grid)) 

 (ddiff/pretty-print (ddiff/diff (nth current-output 2) (nth example-output-grids 2)  ))

 (= current-output example-output-grids  );; => true


 (defn log->occupied-count
   [log]
   (->> log
     last
     flatten
     frequencies
     :occupied-seat));; => 37


 (log->occupied-count current-output);; => 37

 (vector [])

 (->> "seating-chart-input.txt"
   spaces-chart-file->spaces-grid
   vector
   grid->grid-log
   log->occupied-count);; => 2238




 )


;; At this point, something interesting happens: the chaos stabilizes and further applications of these rules cause no seats to change state! Once people stop moving around, you count 37 occupied seats.

;; Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many seats end up occupied?
