(ns atelier.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]

            [atelier.canvas.canvas :as canvas]
            [atelier.canvas.events :as e]
            [atelier.canvas.devcards :as devcards]
            [atelier.code :as code]
            [atelier.partition :as partition]
            [atelier.file :as file]
            [atelier.about :as about]

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

(defn cursor-fn [data]
  (swap! state assoc-in [:canvas :highlights 0]
         {:pos (:pos data)
          :size (:size data)}))

(defn on-selection-change [ev]
  (let [v (.-target.value ev)]
    (swap! state
           #(-> %
                ;; which is selected
                (assoc :selected v)

                ;; save current editor state
                (assoc-in [:editor-contents (:selected %)]
                          {:value (get-in % [:editor :value])
                           :cursor (get-in % [:editor :cursor])})

                ;; save current canvas state
                (assoc-in [:canvas-contents (:selected %)]
                          {:scale (get-in % [:canvas :scale])
                           :offset (get-in % [:canvas :offset])})

                ;; new editor state
                (assoc-in [:editor :value]
                          (get-in % [:editor-contents v :value]))
                (assoc-in [:editor :cursor]
                          (get-in % [:editor-contents v :cursor]))

                ;; new canvas state
                (assoc-in [:canvas :scale]
                          (get-in % [:canvas-contents v :scale]))
                (assoc-in [:canvas :offset]
                          (get-in % [:canvas-contents v :offset]))

                (assoc-in [:canvas :url]
                          ((:images @state) v))))))

(defn simple-component []
  (let [height (.-innerHeight js/window)
        width (.-innerWidth js/window)
        pos (-> state deref :partition :x)
        canvas-cursor (reagent/cursor state [:canvas])
        editor-cursor (reagent/cursor state [:editor])
        partition-cursor (reagent/cursor state [:partition])]
    [:div
     [:div#toolbar
      {:style {:background "#3f3f3f"
               :padding "1px"
               :border-bottom "2px solid #333"}}
      [:select
       {:on-change on-selection-change
        :value (:selected @state)}
       (for [[label url] (:images @state)]
         ^{:key label} [:option {:value label} label])]
      [file/file-selection
       (fn [filename data-url]
         (swap! state
                #(-> %
                     (assoc-in [:canvas :url] data-url)
                     (assoc-in [:images filename] data-url)
                     (assoc-in [:editor-contents filename] {:value "{\n}\n" :cursor {:line 0 :ch 0}})
                     (assoc-in [:canvas-contents filename] {:scale 2 :offset [0 0]})
                     (assoc :selected filename)
                     )))]
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
        ;; everything we deref here triggers a repaint
        :width pos :height height
        :cursor-fn cursor-fn

        ;; we need to refresh code widget when selection changes
        :selected (:selected @state)
        ]]]

     (when (:about? @state)
       (about/about-dialog #(swap! state assoc :about? false)))]))

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
