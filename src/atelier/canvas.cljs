(ns atelier.canvas
  (:require [reagent.core :as reagent]
            [cljs.tools.reader :refer [read-string]]
            [cljsjs.codemirror]
            [cljsjs.codemirror.mode.clojure]
            [cljsjs.codemirror.keymap.emacs]

            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.events :as e]
            [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.pixi.pixelfont :as pf]
            [infinitelives.utils.math :as math]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound]
            [infinitelives.utils.console :refer [log]]

            [cljs.core.async :refer [<! chan put! alts!]]

)

  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go]]
   [infinitelives.pixi.macros :as m]
   )
  )

;; pass key handler defaults through so codemirror
;; handlers continue to work
(events/allow-key-defaults)

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

  ;; define your app data so that it doesn't get over-written on reload


(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(defcard-doc
  "
## Canvas
Note: This widget is for representing infinitelives textures
")

(defn canvas-did-mount [data-atom]
  (fn [this]
    (c/init
     {:layers [:bg :fg]
      :background 0xa0a0a0
      :canvas (reagent/dom-node this)})
    ))

(defn canvas [data-atom]
  [(with-meta
     (fn [] [:canvas {:style {:width "100px" :height "100px"}}])
     {:component-did-mount (canvas-did-mount data-atom)})])

(defcard card-component-canvas
  "A basic pixi canvas"
  (reagent/as-element [canvas (atom nil)])
  )

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
       ;(.log js/console cx cy)
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


(defn control-thread [canvas
                      mouse-down mouse-up mouse-move mouse-wheel mouse-out mouse-over
                      getpos-fn setpos-fn zoom-fn]
  (go
    (loop []
      (let [[data c] (alts! [mouse-move mouse-up mouse-down mouse-wheel mouse-out mouse-over])]
        ;(log "data" data "c" c)
        (when-not (nil? data)
          (let [[ev ox oy] data]
            (cond
              (and (= c mouse-down)
                   (= 2 (.-button ev)))
              ;; mouse down
              (let [[start-x start-y] (getpos-fn)]

                (log "mouse-down" ox oy (getpos-fn))
                (loop [x 0 y 0]
                  (let [[data2 c2] (alts! [mouse-up mouse-move mouse-wheel mouse-out mouse-over])]
                    (log "!!!" data2)
                    (when-not (nil? data2)
                      (let [[ev x2 y2] data2]
                        (log "->" ev x2 y2)
                        (cond
                          (= c2 mouse-up)
                          (log "mouse-up")

                          (= c2 mouse-out)
                          (log "mouse-out")

                          (= c2 mouse-over)
                          (log "mouse-over")

                          (= c2 mouse-move)
                          (do (log "mouse-move" x2 y2)
                              (setpos-fn
                               (+ start-x (- x2 ox))
                               (+ start-y (- y2 oy)))
                              (recur x2 y2))

                          (= c2 mouse-wheel)
                          (do
                            (log "mouse-wheel 2" x2 x y)
                            (zoom-fn (Math/sign x2))
                            (recur x y))
                          )))
                    ))
                (recur))

              (and (= c mouse-down)
                   (= 0 (.-button ev)))
              (do
                (let [bounds (.getBoundingClientRect canvas)
                      top (.-top bounds)
                      left (.-left bounds)
                      ]
                  (log "mouse-down left:" ox oy (- ox left) (- oy top)))
                (recur))

              (= c mouse-wheel)
              ;; mouse wheel movement
              (do
                (log "mousewheel:" (Math/sign ox))
                (zoom-fn (Math/sign ox))
                (recur))

              ;; default
              :default
              (recur))))
        )))
  )


(defn canvas-control [el data-atom]
  (let [mouse-down (chan 1)
        mouse-up (chan 1)
        mouse-move (chan 1)
        mouse-wheel (chan 1)
        mouse-out (chan 1)
        mouse-over (chan 1)]
    (.addEventListener el "mousedown"
                       #(do
                                        ;(log "MD")
                          (put! mouse-down [% (.-clientX %) (.-clientY %)])
                          (.preventDefault %)))
    (.addEventListener el "mouseup"
                       #(do             ;(log "MU")
                          (put! mouse-up [% (.-clientX %) (.-clientY %)])
                          (.preventDefault %)))
    (.addEventListener el "mousemove"
                       #(do             ;(log "MM")
                          (put! mouse-move [% (.-clientX %) (.-clientY %)])))
    (.addEventListener el "mousewheel"
                       #(do (log "MW")
                            (put! mouse-wheel [% (.-wheelDelta %)])
                            (.preventDefault %)))
    (.addEventListener el "mouseout"
                       #(do (log "MOut")
                            (put! mouse-out [% (.-clientX %) (.-clientY %)])
                            (.preventDefault %)))
    (.addEventListener el "mouseover"
                       #(do (log "MOver")
                            (put! mouse-over [% (.-clientX %) (.-clientY %)])
                            (.preventDefault %)))


    (control-thread el
                    mouse-down mouse-up mouse-move mouse-wheel mouse-out mouse-over
                    (fn [] (:offset @data-atom))
                    (fn [x y] (swap! data-atom assoc :offset [x y]))
                    (fn [z] (swap! data-atom update :scale + z)))
    )
  )


(defn image-canvas-did-mount [data-atom]
  (fn [this]
    (let [canv (c/init
                {:layers [:bg :image :fg]
                 :background 0x404040

                 ;; canvas width and geight from the dom node are wrong at this point
                 ;; so to keep aspect ratio correct we pass them in
                 :width 640 :height 200
                 :canvas (reagent/dom-node this)})
          bg-url "/img/peasanttiles01.png"
          ;"http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
          ]
      (go
        (<! (r/load-resources canv :fg [bg-url]))

        (let [rabbit-texture (r/get-texture :peasanttiles01 :nearest)
              _ (log "RABBIT-TEXTURE" rabbit-texture)
              texture-width (.-width rabbit-texture)
              texture-height (.-height rabbit-texture)
              empty-colour 0x800000
              border-colour 0xffffff
              highlight-colour 0xff00ff
              bunny-width texture-width
              bunny-height texture-height
              scale (get @data-atom :scale 3)
              full-colour 0x0000ff
              ]
          (t/set-texture! :rabbit rabbit-texture)

          (m/with-sprite canv :image
            [rabbit (s/make-sprite :rabbit :scale scale)]
            (let [image-background (js/PIXI.Graphics.)
                  image-foreground (js/PIXI.Graphics.)]
              (set! (.-interactive image-background) true)
              (set! (.-mousedown image-background) #(.log js/console "bg:" %))

              (set! (.-oncontextmenu (:canvas canv))
                      (fn [e] (.preventDefault e)))
              (canvas-control (:canvas canv) data-atom)

              (doto image-background
                (.beginFill empty-colour 0.0)
                (.lineStyle 1 border-colour)
                (.drawRect (dec (* scale -0.5 bunny-width))
                           (dec (* scale -0.5 bunny-height))
                           (inc (* scale bunny-width))
                           (inc (* scale bunny-height))))

              (draw-foreground-rectangle
               image-foreground scale
               [9 10] [1 10] [texture-width texture-height])

              (draw-chessboard
               image-background
               :start-x (* scale -0.5 bunny-width)
               :start-y (* scale -0.5 bunny-height)
               :width (* scale bunny-width)
               :height (* scale bunny-height)
               :colour-1 0x8080ff
               :colour-2 0xa0a0ff
               :tile-width 24
               :tile-height 24)

              (.addChild (m/get-layer canv :bg) image-background)
              (.addChild (m/get-layer canv :fg) image-foreground)

              (let [position [0 0]]
                (apply s/set-pivot! (m/get-layer canv :bg) position)
                (apply s/set-pivot! (m/get-layer canv :image) position)
                (apply s/set-pivot! (m/get-layer canv :fg) position))

              ;; add watcher? These will bunch
              ;; up on load unless we remove them
              (add-watch data-atom :dummy
                         (fn [key atom old-state
                              {:keys [scale highlights offset]}]
                           (s/set-scale! rabbit scale)

                           (let [[x y] offset
                                 position [(- x) (- y)]]
                             (apply s/set-pivot! (m/get-layer canv :bg) position)
                             (apply s/set-pivot! (m/get-layer canv :image) position)
                             (apply s/set-pivot! (m/get-layer canv :fg) position))

                           (.clear image-background)
                           (.clear image-foreground)

                           (draw-chessboard
                            image-background
                            :start-x (* scale -0.5 bunny-width)
                            :start-y (* scale -0.5 bunny-height)
                            :width (* scale bunny-width)
                            :height (* scale bunny-height)
                            :colour-1 0x8080ff
                            :colour-2 0xa0a0ff
                            :tile-width 24
                            :tile-height 24)

                           (doto image-background
                             (.beginFill empty-colour 0.0)
                             (.lineStyle 1 border-colour)
                             (.drawRect (dec (* scale -0.5 bunny-width))
                                        (dec (* scale -0.5 bunny-height))
                                        (inc (* scale bunny-width))
                                        (inc (* scale bunny-height))))

                           (loop [[{:keys [pos size]} & t] highlights]
                             (.log js/console "pos" pos "size" size)
                             (draw-foreground-rectangle
                              image-foreground scale
                              pos size [texture-width texture-height])
                             (when (seq t)
                               (recur t)))


                           ))

              (loop [f 0]
                (<! (e/next-frame))
                (recur (inc f))))))))))

(defn image-canvas [data-atom]
  [(with-meta
     (fn [] [:canvas {:style {:width "640px" :height "200px"}}])
     {:component-did-mount (image-canvas-did-mount data-atom)})]
  )

(defcard card-component-canvas
  "A basic pixi canvas with different shape."
  (reagent/as-element [image-canvas (atom nil)])
  )


(defcard card-component-canvas
  "A basic pixi canvas with changable scale and highlight box."
  (fn [data-atom owner]
    (reagent/as-element
     [:div
      [image-canvas data-atom]
      [:p "scale: "
       [:button {:on-click #(swap! data-atom update :scale dec)} "-"]
       [:button {:on-click #(swap! data-atom update :scale inc)} "+"]]
      [:p "offset: "
       [:button {:on-click #(swap! data-atom update-in [:offset 0] - 5)} "left"]
       [:button {:on-click #(swap! data-atom update-in [:offset 0] + 5)} "right"]
       [:button {:on-click #(swap! data-atom update-in [:offset 1] - 5)} "up"]
       [:button {:on-click #(swap! data-atom update-in [:offset 1] + 5)} "down"]]
      [:p
       [:button
        {:on-click
         #(swap! data-atom update-in [:highlights 0]
                 assoc
                 :pos [(int (* 24 (rand)))
                       (int (* 24 (rand)))]
                 :size [(inc (int (* 10 (rand))))
                        (inc (int (* 10 (rand))))])}
        "highlight"]]]))

  {
   :highlights
   [
    {:pos [9 12]
     :size [1 10]}
    ]

   :scale 3

   :offset [0 0]
   }

  {:inspect-data true
   :history true
   })


(defcard card-component-canvas-draggable
  "A basic pixi canvas with changable scale and highlight box. Right click
and drag to reposition canvas. mouse wheel to zoom."
  (fn [data-atom owner]
    (reagent/as-element
     [:div
      [image-canvas data-atom]]))

  {
   :highlights
   [
    {:pos [9 12]
     :size [1 10]}
    ]

   :scale 3

   :offset [0 0]
   }

  {:inspect-data true
   ;:history true
   })
