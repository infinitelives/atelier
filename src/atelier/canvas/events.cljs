(ns atelier.canvas.events
  (:require [infinitelives.utils.math :as math]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.events :as events]
            [atelier.canvas.coord :as coord]
            [cljs.core.async :refer [<! chan put! alts!]])
  (:require-macros
   [cljs.core.async.macros :refer [go alt!]]))

;; pass key handler defaults through so codemirror
;; handlers continue to work
(events/allow-key-defaults)

(defn selection-drag [canvas->local-fn sethighlight-fn
                      mouse-up mouse-move mouse-wheel]
  (go
    (loop [x 0 y 0]
      (alt!
        mouse-move ([[ev x2 y2]]
                    (let [e (canvas->local-fn [x2 y2])]
                      (sethighlight-fn e)
                      (recur x2 y2)))
        mouse-wheel (recur x y)
        mouse-up nil))))


(defn drag-canvas [canvas->local-fn setpos-fn zoom-fn
                   mouse-up mouse-move mouse-wheel]
  (go
    (loop [x 0 y 0]
      (alt!
        mouse-move ([[ev x2 y2]]
                    (setpos-fn x2 y2)
                    (recur x2 y2))
        mouse-wheel ([[ev x2 y2]]
                     (let [local-offset (canvas->local-fn [x y])]
                       (zoom-fn (Math/sign x2))
                       (recur x y)))
        mouse-up nil))))


(defn zoom-canvas [canvas x y
                   getpos-fn getscale-fn setpos-fn zoom-fn zoom-canvas
                   texture-width texture-height ox]
  (let [bounds (.getBoundingClientRect canvas)
        top (.-top bounds)
        left (.-left bounds)

        md-x (- x left)
        md-y (- y top)

        [start-x start-y] (getpos-fn)
        scale (getscale-fn)

        co-ord (vec2/vec2 md-x md-y)

        ;; this pixel has to stay at the cursor position after zoom
        local-offset (coord/canvas->local canvas [x y]
                                    (getpos-fn)
                                    [texture-width texture-height]
                                    (getscale-fn))

        ;; calculate the new canvas offset to keep this pixel there on the canvas
        new-scale (+ (Math/sign ox) scale)
        new-offset (coord/local->canvas canvas local-offset co-ord
                                  [start-x start-y]
                                  [texture-width texture-height]
                                  new-scale)]
    ;; this pixel has to stay at the cursor position after zoom
    (if (>= new-scale 1)
      (do
        (setpos-fn
         (vec2/get-x new-offset)
         (vec2/get-y new-offset))
        (zoom-fn (Math/sign ox))))))


(defn control-thread [canvas texture-width texture-height
                      mouse-down mouse-up mouse-move mouse-wheel
                      getpos-fn setpos-fn zoom-fn sethighlight-fn getscale-fn]
  (go
    (loop [x 0 y 0]
      (let [[data c] (alts! [mouse-move
                             mouse-up
                             mouse-down
                             mouse-wheel])]
        (when-not (nil? data)
          (let [[ev ox oy] data]
            (cond
              (and (= c mouse-down)
                   (= 2 (.-button ev)))

              ;; mouse RMB down. drag canvas
              (let [[start-x start-y] (getpos-fn)]
                (<! (drag-canvas
                     #(coord/canvas->local
                       canvas % (getpos-fn)
                       [texture-width texture-height]
                       (getscale-fn))
                     #(setpos-fn
                      (+ start-x (- %1 ox))
                      (+ start-y (- %2 oy)))
                     zoom-fn
                     mouse-up mouse-move mouse-wheel))
                (recur x y))

              (and (= c mouse-down)
                   (= 0 (.-button ev)))

              ;; mouse LMB down. make selection
              (let [bounds (.getBoundingClientRect canvas)
                    top (.-top bounds)
                    left (.-left bounds)
                    local-offset (coord/canvas->local canvas [ox oy]
                                                (getpos-fn)
                                                [texture-width texture-height]
                                                (getscale-fn))]
                (<! (selection-drag
                     #(coord/canvas->local
                       canvas % (getpos-fn)
                       [texture-width texture-height]
                       (getscale-fn))
                     #(sethighlight-fn
                       (int (vec2/get-x local-offset))
                       (int (vec2/get-y local-offset))
                       (int (vec2/get-x %))
                       (int (vec2/get-y %)))
                     mouse-up mouse-move mouse-wheel))
                (recur x y))


              (= c mouse-wheel)
              ;; mouse wheel movement
              (do (zoom-canvas canvas
                               x y
                               getpos-fn getscale-fn setpos-fn zoom-fn zoom-canvas
                               texture-width texture-height ox)
                  (recur x y))

              (= c mouse-move)
              (recur ox oy)

              ;; default
              :default
              (recur x y))))))))

(defn- make-channel-processing-fn [ch & arg-list]
  (let [flags (set arg-list)]
    (fn [ev]
      (put! ch
            (if (:wheel-delta flags)
              [ev (.-wheelDelta ev)]
              [ev (.-clientX ev) (.-clientY ev)]))
      (when (:prevent-default flags) (.preventDefault ev)))))

(defn canvas-control [el data-atom texture-width texture-height]
  (let [mouse-down (chan 1)
        mouse-up (chan 1)
        mouse-move (chan 1)
        mouse-wheel (chan 1)]
    (.addEventListener el "mousedown"
                       (make-channel-processing-fn mouse-down :prevent-default))
    (.addEventListener js/window "mouseup"
                       (make-channel-processing-fn mouse-up :prevent-default))
    (.addEventListener js/window "mousemove"
                       (make-channel-processing-fn mouse-move))
    (.addEventListener el "mousewheel"
                       (make-channel-processing-fn mouse-wheel :wheel-delta :prevent-default))

    (control-thread el texture-width texture-height
                    mouse-down mouse-up mouse-move mouse-wheel
                    (fn [] (:offset @data-atom))
                    (fn [x y] (swap! data-atom assoc :offset [x y]))
                    (fn [z] (swap! data-atom update :scale + z))
                    (fn [x y x2 y2] (swap! data-atom update-in
                                           [:highlights 0]
                                           assoc
                                           :pos [x y]
                                           :size [(- x2 x) (- y2 y)]))
                    (fn [] (:scale @data-atom)))))
