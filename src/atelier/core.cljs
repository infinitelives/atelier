(ns atelier.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]

            [atelier.canvas.canvas :as canvas]
            [atelier.canvas.devcards :as devcards]
            [atelier.code :as code]
            [atelier.partition :as partition]

            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as events]
            [cljs.core.async :refer [<! chan put! alts! timeout]]
            )
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]
   [reagent.ratom :refer [reaction]]
   )
  )

(enable-console-print!)

(defonce state
  (reagent/atom
   (let [width (.-innerWidth js/window)
         height (.-innerHeight js/window)
         initial-x (int (* 0.60 width))]
     {
      :editor {
               :value "{
  :foo {:size [8 8] :pos [8 8]}
  :bar {
    :size [16 16]
    :pos [24 24]
  }
}"
               :cursor {
                        :line 1
                        :ch 0
                        }
               :width initial-x
               :height height
               }

      :canvas {
               :url "https://retrogradeorbit.github.io/moonhenge/img/sprites.png"
               :highlights [
                            {:pos [9 12]
                             :size [1 10]}
                            ]
               :scale 3
               :offset [0 0]
               :width initial-x
               :height height

               }

      :partition {:x initial-x}})))

(defn print-state []
  (log (str @state)))

(defn update-atoms! [x]
  (let [height (.-innerHeight js/window)]
    (swap! state
           #(-> %
                (assoc-in [:partition :x] x)
                (assoc-in [:editor :width] x)
                (assoc-in [:editor :height] height)
                (assoc-in [:canvas :width] x)
                (assoc-in [:canvas :height] height)))))

;; propagate watch events into the cursors
;; https://github.com/reagent-project/reagent/issues/244
(defn cursor [src path]
  (let [watch-key (->> path
                       (map name)
                       (string/join "-")
                       (str "propagate-")
                       keyword)
        curs (reagent/cursor src path)]
    (add-watch src watch-key
               (fn [k a o n]
                 (when
                     (not=
                      (get-in o path)
                      (get-in n path))
                   (deref curs))))
    curs))

(defn simple-component []
  (let [height (.-innerHeight js/window)
        width (.-innerWidth js/window)
        pos (-> state deref :partition :x)
        canvas-cursor (cursor state [:canvas])
        editor-cursor (cursor state [:editor])
        partition-cursor (cursor state [:partition])]

    [:div
     [:div#main-canvas {:style {:position "absolute"}}
      [canvas/image-canvas canvas-cursor
       ;; initial position
;       :width 20
;       :height 20
       ]]
     [partition/partitioner
      partition-cursor
      update-atoms!]
     [:div#code-editor
      [code/editor editor-cursor
       ;; initial position
       :width pos :height height
       ]]]))

(defn render-simple []
  (reagent/render-component
   [simple-component]
   (.getElementById js/document "app")))

(render-simple)

(defonce resize-thread
  (go (let [c (events/new-resize-chan)]
        (while true
          (<! c)
          (update-atoms! (-> state deref :partition :x))))))
