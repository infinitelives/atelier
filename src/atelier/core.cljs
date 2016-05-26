(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]

            [infinitelives.utils.console :refer [log]]
            [cljs.core.async :refer [<! chan put! alts!]]
            )
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]])
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

(defonce screen-state
  (reagent/atom
   {
    :split-x 200
    }))

(defn partitioner-thread [el mouse-down mouse-up mouse-move]
  (go
    (loop []
      (log ".")
      (let [[data c] (alts! [mouse-up mouse-down mouse-move])]
        (when-not (nil? data)
          (let [[ev ox oy] data]
            (cond
              (= c mouse-down)
              ;; mouse down. drag
              (do
                (log "!" ox oy)
                (loop []
                  (let [[data2 c2] (alts! [mouse-up mouse-down mouse-move])]
                    (when-not (nil? data2)
                      (let [[ev x2 y2] data2]
                        (cond
                          (= c2 mouse-move)
                          (let [inner (.-innerWidth js/window)]
                            (log "partition drag" x2 y2)

                            (swap! screen-state assoc :split-x (- x2 10))
                            (recur))

                          (= c2 mouse-down)
                          (recur)

                          (= c2 mouse-up)
                          nil)))))
                (recur))

              :default
              (recur))))))))


(defn partitioner-control [el data-atom]
  (log "pc")
  (let [mouse-down (chan 1)
        mouse-up (chan 1)
        mouse-move (chan 1)]
    (.addEventListener el "mousedown"
                       #(do
                          (put! mouse-down [% (.-clientX %) (.-clientY %)])
                          (.preventDefault %)))
    (.addEventListener el "mouseup"
                       #(do
                          (put! mouse-up [% (.-clientX %) (.-clientY %)])
                          (.preventDefault %)))
    (.addEventListener el "mousemove"
                       #(do
                          (put! mouse-move [% (.-clientX %) (.-clientY %)])))

    (partitioner-thread el mouse-down mouse-up mouse-move)))

(defn partitioner-did-mount [data-atom]
  (fn [this]
    (partitioner-control (reagent/dom-node this) data-atom)))

(defn partitioner [data-atom]
  [(with-meta
     (fn [] [:div {:style {:position "absolute"
                           :display "block"
                                        ;:height "739px"
                           :width "20px"
                           :visibility "visible"
                           :overflow "visible"
                           :border "1px solid black"
                           :background-color "#aaa"

                           :margin "0px"
                           :left (str (:split-x @data-atom) "px")
                           :right "0px"
                           :top "0px"
                           :bottom "0px"
                           }}])
     {:component-did-mount (partitioner-did-mount data-atom)} )])

(defn simple-component []
  (let [y (.-innerHeight js/window)]
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
      (canvas/image-canvas canvas-state :width (:split-x @screen-state) :height y)]

     [partitioner screen-state]

     [:div {:style {:position "absolute"
                    :display "block"
                     ;                   :height "739px"
                    :width "*"
                    :height (str y "px")
                    :visibility "visible"
                    :overflow "visible"

                    :margin "0px"
                    :left (str (+ 2 20 (:split-x @screen-state)) "px")
                    :right "0px"
                    :top "0px"
                                        ;:bottom "0px"
                    }}
      (code/editor editor-state :width 200 :height y)]
     ]))

(defn render-simple []
  (reagent/render-component
   [simple-component]
   (.getElementById js/document "app")))

(render-simple)
