(ns sudoku.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! close! chan <! >! timeout onto-chan]]
            [reagent.core :as reagent]
            [sudoku.solver :as solver]))

(defn cell [props item]
  (let [state (reagent/atom item)
        class (reagent/atom nil)]
    (fn [props item]
      (let [changed (not= item @state)
            green (and changed (pos? item))
            red (and changed (zero? item))
            cls (cond
                  green "green"
                  red "red"
                  :else nil)]
        (when cls (reset! class cls))
        (reset! state item)
        [:div.cell (merge {:className @class}
                          props) @state]))))

(defn border [i j size]
  (->> [(zero? (mod (inc i) size)) {:borderBottom "1px solid grey"}
        (zero? (mod i size)) {:borderTop "1px solid grey"}
        (zero? (mod (inc j) size)) {:borderRight "1px solid grey"}
        (zero? (mod j size)) {:borderLeft "1px solid grey"}]
       (partition-all 2)
       (filter first)
       (map second)
       (apply merge)
       (hash-map :style)))

(defn grid-view
  ([rows]
   (grid-view nil rows))
  ([props rows]
   (let [{:keys [n size]} (meta rows)]
     [:div props
      (map-indexed
        (fn [i row]
          ^{:key i}
          [:div.row
           (map-indexed
             (fn [j item]
               ^{:key (str i "-" j)}
               [cell
                (border i j size)
                item])
             row)])
        rows)])))

(defonce board (reagent/atom nil))
(defonce solver (atom nil))

(defn main []
  [grid-view @board])

(defn run-solver []
  (let [boards (solver/solve @board)
        c (onto-chan (chan 1) boards)]
    (go-loop [b (<! c)]
      (when b (reset! board b))
      (when-not (:complete (meta b))
        (<! (timeout 10))
        (recur (<! c))))))

(defn new-board
  ([n num-items]
   (new-board n num-items false))
  ([n num-items force-solvable?]
   (let [f (if force-solvable? solver/solvable-board solver/sparse-board)
         b (f n num-items)]
     (reset! board b))))

(defn start []
  (reset! solver (run-solver)))

(defn mount-root []
  (reagent/render [main] (.getElementById js/document "app")))

(defn init! []
  (mount-root)
  (new-board 16 0)
  (start))
