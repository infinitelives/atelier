(ns atelier.canvas.devcards
  (:require [reagent.core :as reagent]
            [atelier.canvas.canvas :as canvas]
            [atelier.canvas.events :as events]

            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as e]
            [infinitelives.utils.gamepad :as g]
            )
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]))
;;
;; Devcards
;;
(defcard-doc
  "
## Canvas
Note: This widget is for representing infinitelives textures
")

(defcard card-component-canvas
  "A basic pixi canvas with different shape."
  (reagent/as-element [canvas/image-canvas (atom {:width 100 :height 100})]))


(defcard card-component-canvas
  "A basic pixi canvas with changable scale and highlight box."
  (fn [data-atom owner]
    (reagent/as-element
     [:div
      [canvas/image-canvas data-atom]
      [:p "scale: "
       [:button {:on-click #(swap! data-atom update :scale dec)} "-"]
       [:button {:on-click #(swap! data-atom update :scale inc)} "+"]]
      [:p "offset: "
       [:button {:on-click #(swap! data-atom update-in [:offset 0] - 5)} "left"]
       [:button {:on-click #(swap! data-atom update-in [:offset 0] + 5)} "right"]
       [:button {:on-click #(swap! data-atom update-in [:offset 1] - 5)} "up"]
       [:button {:on-click #(swap! data-atom update-in [:offset 1] + 5)} "down"]]
      [:p "size: "
       [:button {:on-click #(swap! data-atom update :width + 5)} "wider"]
       [:button {:on-click #(swap! data-atom update :width - 5)} "narrower"]
       [:button {:on-click #(swap! data-atom update :height + 5)} "taller"]
       [:button {:on-click #(swap! data-atom update :height - 5)} "shorter"]]
      [:p "image: "
       [:select
        {:on-change #(let [selection (.-target.value %)
                           url-map {"bunny" "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
                                    "moonhenge" "https://raw.githubusercontent.com/retrogradeorbit/moonhenge/master/resources/public/img/sprites.png"}
                           ]
                       (swap! data-atom assoc :url (url-map selection))
                       )}
        [:option "bunny"]
        [:option "moonhenge"]]
       ]
      [:p
       [:button
        {:on-click
         #(swap! data-atom update-in [:highlights 0]
                 assoc
                 :pos [(int (* 24 (rand)))
                       (int (* 24 (rand)))]
                 :size [(inc (int (* 10 (rand))))
                        (inc (int (* 10 (rand))))])}
        "highlight"]]]))
  {
   :highlights
   [{:pos [9 12]
     :size [1 10]}]
   :scale 3
   :offset [0 0]
   :width 200 :height 200
   :url "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
   }
  {:inspect-data true
   :history true
   })

(defcard card-component-canvas-draggable
  "A basic pixi canvas with changable scale and highlight box. Right click
and drag to reposition canvas. mouse wheel to zoom. Left click and drag to select.

"
  (fn [data-atom owner]
    (reagent/as-element
     [:div
      [canvas/image-canvas data-atom :ui-control-fn events/canvas-control]]))
  {
   :highlights
   [{
     :pos [9 12]
     :size [1 10]}]
   :scale 3
   :offset [0 0]
   :width 300 :height 300
   :url "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
   }
  {:inspect-data true})
