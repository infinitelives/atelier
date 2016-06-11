(ns atelier.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]

            [atelier.canvas.core :as canvas]
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
             :width 200
             :height 100
             }

    :canvas {
             :url "https://retrogradeorbit.github.io/moonhenge/img/sprites.png"
             :highlights [
                          {:pos [9 12]
                           :size [1 10]}
                          ]
             :scale 3
             :offset [0 0]
             :width 200
             :height 400

             }

    :partition {:x 200}}))

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
  (let [y (.-innerHeight js/window)
        canvas-cursor (cursor state [:canvas])
        editor-cursor (cursor state [:editor])
        partition-cursor (cursor state [:partition])]

    [:div
     [:div#main-canvas {:style {:position "absolute"}}
      [canvas/image-canvas
       canvas-cursor]]
     [partition/partitioner
      partition-cursor
      update-atoms!]
     [:div#code-editor
      (code/editor
       editor-cursor)]]))

(defn render-simple []
  (reagent/render-component
   [simple-component]
   (.getElementById js/document "app")))

(render-simple)

(update-atoms! (int (* (.-innerWidth js/window) 0.75)))

(defonce resize-thread
  (go (let [c (events/new-resize-chan)]
        (while true
          (<! c)
          (update-atoms! 500)))))

;; hacky bugfix
#_ (go (<! (timeout 2000))
    (update-atoms! (int (* (.-innerWidth js/window) 0.7))))

;; test image load
#_ (go (<! (timeout 4000))
    (swap! state assoc-in [:canvas :url] "https://retrogradeorbit.github.io/biscuit-switch/img/sprites.png"))
