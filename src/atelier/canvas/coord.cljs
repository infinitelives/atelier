(ns atelier.canvas.coord
  (:require [infinitelives.utils.vec2 :as vec2]))

(defn- half-canvas
  "return a vecs from the top left corner of the canvas to the middle
  of the canvas"
  [canvas]
  (vec2/vec2
   (/ (.-width canvas) 2)
   (/ (.-height canvas) 2)))

(defn canvas->local [canvas [x y] [start-x start-y] [texture-width texture-height] scale]
  (let [bounds (.getBoundingClientRect canvas)
        top (.-top bounds)
        left (.-left bounds)

        md-x (- x left)
        md-y (- y top)

        co-ord (vec2/vec2 md-x md-y)
        canvas-offset (vec2/vec2 start-x start-y)
        half-texture (vec2/scale
                      (vec2/vec2 (/ texture-width -2)
                                 (/ texture-height -2))
                      scale) ;; negative half loaded image size

        local-offset (-> co-ord
                         (vec2/sub (half-canvas canvas))
                         (vec2/sub canvas-offset)
                         (vec2/sub half-texture)
                         (vec2/scale (/ 1 scale)))]
    local-offset))


(defn local->canvas
  "convert a local, texture image co-ordinate, into a coordinate in
  the canvas. Useful for finding out where on the screen a particular
  pixel x,y is."
  [canvas local-offset co-ord [start-x start-y]
   [texture-width texture-height]
   new-scale]
  (let [canvas-offset (vec2/vec2 start-x start-y)
        half-texture (vec2/vec2 (/ texture-width -2)
                                (/ texture-height -2))

        ;; calculate the new canvas offset to keep this pixel there on the canvas
        new-offset (-> local-offset
                       (vec2/scale new-scale)
                       (vec2/add (half-canvas canvas))
                       (vec2/add (vec2/scale half-texture new-scale))
                       (vec2/sub co-ord)
                       (vec2/scale -1))]
    new-offset))
