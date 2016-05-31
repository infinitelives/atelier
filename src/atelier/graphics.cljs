(ns atelier.graphics)

(defn draw-chessboard
  [gfx & {:keys [start-x start-y width height
                 colour-1 colour-2
                 tile-width tile-height]
          :or {start-x -50
               start-y -50
               width 100
               height 100
               colour-1 0x808080
               colour-2 0xa0a0a0
               tile-width 16
               tile-height 16}}]
  (doto gfx
    (.beginFill colour-1)
    (.lineStyle 0 0x000000)
    (.drawRect start-x start-y width height)
    .endFill)

  (doall
   (for [cx (range (/ width tile-width))
         cy (range (/ height tile-height))
         :when (odd? (+ cx cy))]
     (let [left (+ start-x (* tile-width cx))
           top (+ start-y (* tile-height cy))
           right (min
                  (+ start-x (* tile-width (inc cx)))
                  (+ start-x width))
           bottom (min
                   (+ start-y (* tile-height (inc cy)))
                   (+ start-y height))]
       (doto gfx
         (.beginFill colour-2)
         (.lineStyle 0 0x000000)
         (.drawRect left top (- right left) (- bottom top))
         .endFill)))))

(defn draw-foreground-rectangle
  [gfx scale [x y] [width height] [image-width image-height]]
  (let [left (* scale -0.5 image-width)
        top (* scale -0.5 image-height)
        scale-x (* scale x)
        scale-y (* scale y)
        scale-w (* scale width)
        scale-h (* scale height)
        start-x (+ scale-x left)
        start-y (+ scale-y top)]
    (doto gfx
      (.beginFill 0x000000 0.0)
      (.lineStyle 1 0x000000 1)
      (.drawRect (dec start-x) (dec start-y)
                 (inc scale-w) (inc scale-h))
      (.lineStyle 1 0xffffff 1)
      (.drawRect (- start-x 2) (- start-y 2)
                 (+ 3 scale-w) (+ 3 scale-h))
      (.lineStyle 1 0x000000 1)
      (.drawRect (- start-x 3) (- start-y 3)
                 (+ 5 scale-w) (+ 5 scale-h)))))
