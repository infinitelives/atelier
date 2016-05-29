(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]
            [atelier.partition :as partition]

            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as events]
            [cljs.core.async :refer [<! chan put! alts! timeout]]
            )
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]])
  )

(enable-console-print!)


(defn on-click [ratom]
  (swap! ratom update-in [:count] inc))

(defonce editor-state (reagent/atom {:value "{
  :foo {:size [8 8] :pos [8 8]}
  :bar {
    :size [16 16]
    :pos [24 24]
  }
}"
                                     :cursor {:line 1 :ch 0}}))

(defonce canvas-state (reagent/atom {
   :highlights
   [
    {:pos [9 12]
     :size [1 10]}
    ]

   :scale 3

   :offset [0 0]
   :width 200
   :height 400
   }))

(defonce screen-state
  (reagent/atom
   {
    :split-x 200
    }))

(defn update-atoms! [x]
  (let [height (.-innerHeight js/window)]
    (swap! screen-state assoc :split-x x)
    (swap! editor-state assoc :width x :height height)
    (swap! canvas-state assoc :width x)))

(defn simple-component []
  (let [y (.-innerHeight js/window)]
    [:div
     [:div#main-canvas {:style {:position "absolute"}}
      [canvas/image-canvas canvas-state]]
     [partition/partitioner screen-state update-atoms!]
     [:div#code-editor
      (code/editor editor-state)]]))

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
          (update-atoms! (:split-x @screen-state))))))

;; hacky bugfix
(go (<! (timeout 2000))
    (update-atoms! (int (* (.-innerWidth js/window) 0.7))))
