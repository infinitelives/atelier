(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]

            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as events]
            [cljs.core.async :refer [<! chan put! alts! timeout]]
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
                          (let [inner (.-innerWidth js/window)
                                height (.-innerHeight js/window)]
                            (log "partition drag" x2 y2)

                            (update-atoms! (- x2 10))

                            ;; this is the wrong place... just to
                            ;; test if the appearing space was scrolling
                            (.scrollTo js/window 0 0)

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
    (.addEventListener js/window "mouseup"
                       #(do
                          (put! mouse-up [% (.-clientX %) (.-clientY %)])
                          (.preventDefault %)))
    (.addEventListener js/window "mousemove"
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
                           :width "12px"
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
                    ;:display "block"
                    ;:width "100%"
                    :visibility "visible"
                    :overflow "visible"

                    :margin "0px"
                    :left "0px"
                    ;:right "auto"
                    :top "0px"
                    }}
      [canvas/image-canvas canvas-state
       ]]

     [partitioner screen-state]

     [:div #_ {:style {:position "absolute"
                    ;:display "block"
                     ;                   :height "739px"
                    :width "*"
                    :height (str y "px")
                    :visibility "visible"
                    :overflow "visible"

                    :margin "0px"
                    ;:left (str (+ 2 12 (:split-x @screen-state)) "px")
                    :right "0px"
                    :top "0px"
                                        ;:bottom "0px"
                    }}
      (code/editor editor-state)]
     ]))

(defn render-simple []
  (reagent/render-component
   [simple-component]
   (.getElementById js/document "app")))

(render-simple)

(update-atoms! (int (* (.-innerWidth js/window) 0.75)))

(go (let [c (events/new-resize-chan)]
      (while true
        (<! c)
        (update-atoms! (:split-x @screen-state))
        )
      ))

;; hacky bugfix
(go (<! (timeout 2000))
    (update-atoms! (int (* (.-innerWidth js/window) 0.7))))
