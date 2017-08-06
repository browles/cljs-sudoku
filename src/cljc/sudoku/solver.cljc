(ns sudoku.solver
  (:require [clojure.core.reducers :as r]))

(defn sqrt [n]
  (#? (:clj Math/sqrt :cljs js/Math.sqrt) n))

(defn bit-mask [nums]
  (reduce (fn [mask num]
            (if (zero? num)
              mask
              (bit-set mask (dec num)))) 0 nums))

(defn to-nums [mask]
  (loop [m mask i 1 nums []]
    (if (zero? m)
      nums
      (recur (bit-shift-right m 1)
             (inc i)
             (if (pos? (bit-and m 1)) (conj nums i) nums)))))

(defn difference [a & args]
  (- a (bit-and a (reduce bit-or args))))

(defn row [board i]
  (bit-mask (get board i)))

(defn col [board j]
  (bit-mask (map #(get % j) board)))

(defn cell [size board i j]
  (let [a (* size (int (/ i size)))
        b (* size (int (/ j size)))]
    (->> board
         (drop a)
         (take size)
         (map #(->> %
                    (drop b)
                    (take size)))
         flatten
         bit-mask)))

(defn board [n]
  (->> (repeat 0)
       (take n)
       repeat
       (map shuffle)
       (take n)
       (mapv vec)
       (#(with-meta % {:n n :size (int (sqrt n))}))))

(defn all-nums [n]
  (->> (range)
       (take n)
       (map inc)
       bit-mask))

(defn sparse-board [n num-items]
  (let [size (sqrt n)
        nums (all-nums n)
        indexes (for [i (range n) j (range n)] [i j])
        placement-indexes (->> indexes
                               shuffle
                               (take num-items))]
    (reduce (fn [board [i j]]
              (assoc-in board
                        [i j]
                        (rand-nth (or (seq (to-nums (difference nums
                                                                (row board i)
                                                                (col board j)
                                                                (cell size board i j))))
                                      [0]))))
            (board n)
            placement-indexes)))

(defn validate-board [board]
  (let [n (:n (meta board))
        size (:size (meta board))
        nums (all-nums n)]
    (every? true? (for [i (range n) j (range n)]
                    (and (= nums (row board i))
                         (= nums (col board j))
                         (= nums (cell size board i j)))))))

(defn print-board [_ board]
  (dorun (map (fn [row]
                (println (map #(let [s (str %)]
                                 (if (= 1 (count s)) (str " " s) s))
                              row)))
              board))
  (println))

(defn solve
  [board]
  (let [n (:n (meta board))
        size (:size (meta board))
        nums (all-nums n)
        placements (for [i (range n) j (range n)
                         :let [free (to-nums (difference nums
                                                         (row board i)
                                                         (col board j)
                                                         (cell size board i j)))]
                         :when (zero? (get-in board [i j]))]
                     {:ij [i j]
                      :free free
                      :n (count free)})
        best (when (seq placements) (apply min-key :n (shuffle placements)))
        branch-points (map #(assoc-in board (:ij best) %) (:free best))
        branches (map #(lazy-seq (cons % (solve %))) branch-points)]
    (if (= 1 (count placements))
      (map #(vary-meta % assoc :complete (validate-board %)) branch-points)
      (apply concat branches)))) ; dfs

(defn solvable-board [n num-items]
  (let [solved (first (filter (comp :complete meta) (solve (board n))))
        indexes (for [i (range n) j (range n)] [i j])]
    (reduce
      #(assoc-in %1 %2 (get-in solved %2))
      (board n)
      (take num-items (shuffle indexes)))))
