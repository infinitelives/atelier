(ns atelier.partition
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]

            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as events]
            [cljs.core.async :refer [<! chan put! alts! timeout]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go alt!]]))


(defn control-thread [el mouse-down mouse-up mouse-move update-fn]
  (go
    (loop []
      (alt!
        mouse-down (do
                     (loop []
                       (alt!
                         mouse-move ([[ev x2 y2]]
                                     (update-fn (- x2 10))
                                     (recur))
                         mouse-down (recur)
                         mouse-up nil))
                     (recur))
        mouse-up (recur)
        mouse-move (recur)))))

(defn- make-channel-processing-fn [ch & arg-list]
  (let [flags (set arg-list)]
    (fn [ev]
      (put! ch [ev (.-clientX ev) (.-clientY ev)])
      (when (:prevent-default flags) (.preventDefault ev)))))

(defn partitioner-control [el update-fn]
  (let [mouse-down (chan 1)
        mouse-up (chan 1)
        mouse-move (chan 1)]
    (.addEventListener el "mousedown"
      (make-channel-processing-fn mouse-down :prevent-default))
    (.addEventListener js/window "mouseup"
      (make-channel-processing-fn mouse-up :prevent-default))
    (.addEventListener js/window "mousemove"
      (make-channel-processing-fn mouse-move))
    (control-thread el mouse-down mouse-up mouse-move update-fn)))

(defn partitioner-did-mount [update-fn]
  (fn [this]
    (partitioner-control (reagent/dom-node this) update-fn)))

(defn partitioner [data-atom update-fn]
  [(with-meta
     (fn []
       [:div
        {:style
         {:position "absolute"
          :display "block"
          :visibility "visible"
          :overflow "visible"

          :border "1px solid black"
          :background-color "#aaa"
          :margin "0px"
          :width "12px"

          :left (str (:x @data-atom) "px")
          :top "0px"
          :bottom "0px"}}])
     {:component-did-mount (partitioner-did-mount update-fn)} )])
