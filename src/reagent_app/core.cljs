(ns reagent_app.core
    (:require [reagent.core :as reagent :refer [atom]]))


(js/console.log "hello from clojurescript")

(def grid-width 10)
(def grid-height 10)
(def num-mines 20)
(def init-val "-")

(defonce app-state 
  (atom {:text "Minesweeper"
         :grid (->> init-val
                    (repeat grid-width)
                    vec
                    (repeat grid-height)
                    vec)
         :display-grid (->> "."
                    (repeat grid-width)
                    vec
                    (repeat grid-height)
                    vec)
         :buttons ["true", "false", "1", "0"]}))

(defn init-grid
  "randomly placing mines to the grid"
  [num-mines]
  (let [random-mines (as-> (* grid-width grid-height) $ 
                           (range 0 $)
                           (shuffle $)
                           (subvec $ 0 num-mines))
        mines-loc (map #(list (quot % grid-height) (mod % grid-width)) random-mines)]
    (doseq [loc mines-loc]
      (swap! app-state assoc-in (concat [:grid] loc) "X"))))
  
(defn find-neighbors
  [x y]
  (let [get-arr (fn [curr max-num] 
                  (let [first-num (max 0 (dec curr))
                        last-num (min (inc curr) (dec max-num))]
                    (-> [first-num curr last-num] set vec)))
        x-arr (get-arr x grid-width)
        y-arr (get-arr y grid-height)] 
    (->> y-arr 
         (mapv (fn [y-num] (mapv #(vector % y-num) x-arr))) 
         (reduce into []))))

(defn cal-hint
  "calculate hint"
  [x y]
  (let [neighbors (find-neighbors x y) 
        hint-count (->> neighbors 
                        (map #(get-in (:grid @app-state) %)) 
                        (filter #(= % "X")) 
                        count)]
    #_(swap! app-state assoc-in [:grid x y] hint-count)
    #_(if (zero? hint-count) 
      (doseq [[neighbor-x neighbor-y] neighbors]
        (if (or (not= x neighbor-x) (not= y neighbor-y))
          #_(button-click neighbor-x neighbor-y)
          (swap! app-state assoc-in [:grid neighbor-x neighbor-y] cal-hint neighbor-x neighbor-y)))
        )
    hint-count))

(defn button-click
  "click on the bomb"
  [x y]
  (if (= (get-in @app-state [:grid x y]) "X")
    (do
        (swap! app-state assoc :text "You Lost.")
        (swap! app-state assoc-in [:display-grid x y] "X"))
    (swap! app-state assoc-in [:display-grid x y] (cal-hint x y))))

(defn display-button
  "display a single button"
  [row col content]
  [:button {:key (str "row-" row col)
            :style {:width "40px" 
                    :height "40px" 
                    :font-size "15px"
                    :font-weight (if (= content "X") "bold" "normal")
                    :border-color (if (= content "X") "red")
                    :color (if (= content "X") "red")
                    :margin-top 2
                    :margin-left 2
                    :background-color (if (= content "X") "lightgray" "white")}
            :on-click #(button-click row col)} 
   content])

(defn display-row
  "display a row of buttons"
  [index row]
  [:div {:class "row" :key (str "row-" index)}
   (map-indexed (fn [idx x] (display-button index idx x)) row)])



(defn app []
  [:div {:class "main-app"
         :style {:text-align "center"}}
   [:h1 (:text @app-state)]
   (map-indexed display-row (:display-grid @app-state))])

;; =================  Main  ====================

(init-grid num-mines)

(reagent/render-component [app]
                          (. js/document (getElementById "app")))

