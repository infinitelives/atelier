(ns atelier.partition
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

(defn control-thread [el mouse-down mouse-up mouse-move update-fn]
  (go
    (loop []
      (let [[data c] (alts! [mouse-up mouse-down mouse-move])]
        (when-not (nil? data)
          (let [[ev ox oy] data]
            (cond
              (= c mouse-down)
              ;; mouse down. drag
              (do
                (loop []
                  (let [[data2 c2] (alts! [mouse-up mouse-down mouse-move])]
                    (when-not (nil? data2)
                      (let [[ev x2 y2] data2]
                        (cond
                          (= c2 mouse-move)
                          (do
                            (update-fn (- x2 10))
                            (recur))

                          (= c2 mouse-down)
                          (recur)

                          (= c2 mouse-up)
                          nil)))))
                (recur))

              :default
              (recur))))))))

(defn partitioner-control [el data-atom update-fn]
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

    (control-thread el mouse-down mouse-up mouse-move update-fn)))

(defn partitioner-did-mount [data-atom update-fn]
  (fn [this]
    (partitioner-control (reagent/dom-node this) data-atom update-fn)))

(defn partitioner [data-atom update-fn]
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
     {:component-did-mount (partitioner-did-mount data-atom update-fn)} )])
