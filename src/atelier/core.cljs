(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]

            )
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]])
  )

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(defn on-click [ratom]
  (swap! ratom update-in [:count] inc))

(defonce editor-state (reagent/atom {:value "{
  :foo
  {
    :size [8 8]
    :pos [8 8]
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
   }))

(defn simple-component []
  [:div

   [:div {:style {:position "absolute"
                  :display "block"
                  :width "100%"
                  :visibility "visible"
                  :overflow "visible"

                  :margin "0px"
                  :left "0px"
                  :right "auto"
                  :top "0px"
                  }}
    (canvas/image-canvas canvas-state :width 1190 :height 900)]

    [:div {:style {:position "absolute"
                  :display "block"
                  ;:height "739px"
                  :width "10px"
                  :visibility "visible"
                  :overflow "visible"
                  :border "1px solid black"
                  :background-color "#aaa"

                  :margin "0px"
                  :left "1190px"
                  :right "0px"
                  :top "0px"
                  :bottom "0px"
                  }}]

   [:div {:style {:position "absolute"
                  :display "block"
                  ;:height "739px"
                  :width "*"
                  :visibility "visible"
                  :overflow "visible"

                  :margin "0px"
                  :left "1202px"
                  :right "0px"
                  :top "0px"
                  ;:bottom "0px"
                  }}
    (code/editor editor-state :width 705 :height 900)]
   ])

(defn render-simple []
  (reagent/render-component
   [simple-component]
   (.getElementById js/document "app")))

(render-simple)
