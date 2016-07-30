(ns atelier.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]

            [atelier.canvas.canvas :as canvas]
            [atelier.canvas.events :as e]
            [atelier.canvas.devcards :as devcards]
            [atelier.code :as code]
            [atelier.partition :as partition]
            [atelier.file :as file]

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
      ;; display about popup?
      :about? false

      ;; selected dropdown
      :selected "moonhenge"

      ;; images for drop down entries
      :images
      {"moonhenge"
       "https://retrogradeorbit.github.io/moonhenge/img/sprites.png"

       "biscuit switch"
       "https://retrogradeorbit.github.io/biscuit-switch/img/sprites.png"

       "bunnies"
       "http://www.goodboydigital.com/pixijs/bunnymark/bunnys.png"}

      :editor-contents
      {
       "moonhenge"
       {:value "{}" :cursor {:line 0 :ch 0}}

       "biscuit switch"
       {:value "{}" :cursor {:line 0 :ch 0}}

       "bunnies"
       {:value "{}" :cursor {:line 0 :ch 0}}
       }

      :canvas-contents
      {
       "moonhenge"
       {:scale 1 :offset [0 0]}

       "biscuit switch"
       {:scale 1 :offset [0 0]}

       "bunnies"
       {:scale 1 :offset [0 0]}
       }

      ;; widgets
      :editor {
               :value ""
               :cursor {
                        :line 0
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
               :scale 1
               :offset [0 0]
               :width initial-x
               :height height

               }

      :partition {:x initial-x}})))

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
        curs (reagent/cursor src path)
        ]
    (add-watch src watch-key
               (fn [k a o n]
                 (when
                     (not=
                      (get-in o path)
                      (get-in n path))
                   (deref curs))))
    curs))


(defn cursor-fn [data]
  (swap! state assoc-in [:canvas :highlights 0]
         {:pos (:pos data)
          :size (:size data)}))

(defn simple-component []
  (let [height (.-innerHeight js/window)
        width (.-innerWidth js/window)
        pos (-> state deref :partition :x)
        canvas-cursor (cursor state [:canvas])
        editor-cursor (cursor state [:editor])
        partition-cursor (cursor state [:partition])]
    [:div
     [:div#toolbar
      {:style {:background "#3f3f3f"
               :padding "1px"
               :border-bottom "2px solid #333"}}
      [:select
       {:on-change
        (fn [ev]
          (let [v (.-target.value ev)]
            (swap! state
                   #(-> %
                        (assoc :selected v)
                        (assoc-in [:canvas :url]
                                  ((:images @state) v))))))
        :value (:selected @state)}
       (for [[label url] (:images @state)]
         ^{:key label} [:option {:value label} label])]
      [file/file-selection
       (fn [filename data-url]
         (swap! state
                #(-> %
                     (assoc-in [:canvas :url] data-url)
                     (assoc-in [:images filename] data-url)
                     (assoc :selected filename))))]
      [:button {:style {:float "right"}
                :on-click #(swap! state update-in [:about?] not)} "About"]
      ]
     [:div {:style {:position "relative"}}
      [:div#main-canvas {:style {:position "absolute"}}
       [canvas/image-canvas canvas-cursor
        :ui-control-fn e/canvas-control

        ;; initial position
                                        ;       :width 20
                                        ;       :height 20
        ]]
      [partition/partitioner
       partition-cursor
       update-atoms!]
      [:div#code-editor
       [code/editor editor-cursor
        ;; removing this breaks the code div resize?!!?
        :width pos :height height
        :cursor-fn cursor-fn
        ]]]

     (when (:about? @state)
       [:div.about {:style {:position "absolute"
                            :width "500px"
                            :height "500px"
                            :left "300px"
                            :top "100px"
                            :background "#eee"
                            :padding "16px"
                            :z-index 10 }}
        [:div {:on-click #(swap! state assoc :about? false)
               :style {:float "right"}} "❌"]
        [:h1 "About Atelier"]
        [:p "Version 0.1"]
        [:p "Copyright © 2016, Crispin Wellington <retrogradeorbit@gmail.com>"]
        [:p "This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version."]
        [:p "This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details."]
        [:p "You should have received a copy of the GNU General Public License
    along with this program.  If not, see "
         [:a {:href "http://www.gnu.org/licenses/"} "http://www.gnu.org/licenses/"]]
        [:p "The source code of the project can be found here: "
         [:a {:href "https://github.com/infinitelives/atelier"} "https://github.com/infinitelives/atelier"]]
        ])

     ]
    ))

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
