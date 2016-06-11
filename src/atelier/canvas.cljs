(ns atelier.canvas
  (:require [reagent.core :as reagent]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.events :as e]
            [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.pixi.pixelfont :as pf]
            [infinitelives.utils.math :as math]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound]
            [infinitelives.utils.string :refer [url-keyword]]
            [infinitelives.utils.console :refer [log]]

            [cljs.core.async :refer [<! chan put! alts!]]

            [atelier.graphics :as graphics])

  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go alt!]]
   [infinitelives.pixi.macros :as m])
  )

;; pass key handler defaults through so codemirror
;; handlers continue to work
(events/allow-key-defaults)

(enable-console-print!)

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
        local-offset (canvas->local canvas [x y]
                                    (getpos-fn)
                                    [texture-width texture-height]
                                    (getscale-fn))

        ;; calculate the new canvas offset to keep this pixel there on the canvas
        new-scale (+ (Math/sign ox) scale)
        new-offset (local->canvas canvas local-offset co-ord
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
                     #(canvas->local
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
                    local-offset (canvas->local canvas [ox oy]
                                                (getpos-fn)
                                                [texture-width texture-height]
                                                (getscale-fn))]
                (<! (selection-drag
                     #(canvas->local
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

(defn- draw-image-background [image-background scale
                              empty-colour border-colour
                              document-width document-height]
  (.clear image-background)
  (graphics/draw-chessboard
   image-background
   :start-x (* scale -0.5 document-width)
   :start-y (* scale -0.5 document-height)
   :width (* scale document-width)
   :height (* scale document-height)
   :colour-1 0x8080ff
   :colour-2 0xa0a0ff
   :tile-width 24
   :tile-height 24)
  (doto image-background
    (.beginFill empty-colour 0.0)
    (.lineStyle 1 border-colour)
    (.drawRect (dec (* scale -0.5 document-width))
               (dec (* scale -0.5 document-height))
               (inc (* scale document-width))
               (inc (* scale document-height)))))

(defn set-canvas-pos! [canv pos]
  (apply s/set-pivot! (m/get-layer canv :bg) pos)
  (apply s/set-pivot! (m/get-layer canv :image) pos)
  (apply s/set-pivot! (m/get-layer canv :fg) pos))

(defn draw-all-highlight-rectangles! [sprite document-size scale [{:keys [pos size]} & tail]]
  (graphics/draw-foreground-rectangle sprite scale pos size document-size)
  (when (seq tail)
    (recur sprite document-size scale tail)))

(defn set-canvas-size! [canv [width height]]
  (set! (.-style.width (:canvas canv)) (str width))
  (set! (.-style.height (:canvas canv)) (str height))
  ((:resize-fn canv) width height))

(defn set-canvas-offset! [canv [x y]]
  (set-canvas-pos! canv [(- x) (- y)]))

(defn set-canvas-scale! [canv scale
                         document [document-width document-height]
                         image-background {:keys [empty-colour border-colour]}]
  (s/set-scale! document scale)
  (draw-image-background image-background scale
                         empty-colour border-colour
                         document-width document-height))

(defn set-canvas-highlights! [canv image-foreground document-size scale highlights]
  (.clear image-foreground)
  (draw-all-highlight-rectangles! image-foreground document-size
                                  scale highlights))


(defn make-atom-watch-fn [canv document image-foreground image-background
                          document-width document-height empty-colour border-colour
                          texture-width texture-height]
  (fn [key atom old-state
       {:keys [scale highlights offset width height url]}]

    ;; canvas widget size
    (when (and width height)
      (set-canvas-size! canv [width height]))

    ;; offset changed
    (when-not (= (:offset old-state) offset)
      (set-canvas-offset! canv offset))

    ;; scale changed
    (when-not (= (:scale old-state) scale)
      (set-canvas-scale! canv scale document [document-width document-height]
                         image-background {:empty-colour empty-colour
                                           :border-colour border-colour}))

    ;; highlights changed
    (when-not (= (:highlights old-state) highlights)
      (set-canvas-highlights! canv image-foreground [document-width document-height]
                              scale highlights))))


(defn setup-canvas-image [url document scale image-background image-foreground
                          {:keys [empty-colour border-colour
                                  highlight-colour full-colour]
                           :or {empty-colour 0x800000
                                border-colour 0xffffff
                                highlight-colour 0xff00ff
                                full-colour 0x0000ff}}]
  (let [document-texture (r/get-texture
                          (url-keyword url)
                          :nearest)
        document-width (.-width document-texture)
        document-height (.-height document-texture)]
    (t/set-texture! :spritesheet document-texture)
    (s/set-texture! document document-texture)

    (graphics/draw-foreground-rectangle
     image-foreground scale
     [9 10] [1 10] [document-width document-height])

    (draw-image-background image-background scale
                           empty-colour border-colour
                           document-width document-height)

    document-texture))

(defn image-canvas-did-mount
  [data-atom ui-control-fn]
  (fn [this]
    (log "component-did-mount")
    (let [url (:url @data-atom)
          width (:width @data-atom)
          height (:height @data-atom)
          canv (c/init
                {:layers [:bg :image :fg]
                 :background 0x404040
                 :width width :height height
                 :canvas (reagent/dom-node this)})]
      (go
        (loop []
          (if url
            (do
              (<! (r/load-resources canv :fg [url]))

              (let [document-texture (setup-canvas-image
                                      url document scale image-background image-foreground
                                      {})
                    document-width (.-width document-texture)
                    document-height (.-height document-texture)]
                (t/set-texture! :spritesheet document-texture)

                (m/with-sprite canv :image
                  [document (s/make-sprite :spritesheet :scale scale)]
                  (let [image-background (js/PIXI.Graphics.)
                        image-foreground (js/PIXI.Graphics.)]
                    (set! (.-interactive image-background) true)
                    (set! (.-mousedown image-background) #(.log js/console "bg:" %))

                    (set! (.-oncontextmenu (:canvas canv))
                          (fn [e] (.preventDefault e)))

                    (ui-control-fn
                     (:canvas canv) data-atom
                     texture-width texture-height)

                    (setup-canvas-image
                           url document (:scale @data-atom) image-background image-foreground
                           {})

                    (.addChild (m/get-layer canv :bg) image-background)
                    (.addChild (m/get-layer canv :fg) image-foreground)

                    (set-canvas-pos! canv [0 0])

                    (add-watch
                     data-atom :dummy
                     (make-atom-watch-fn
                      canv document image-foreground image-background
                      document-width document-height empty-colour border-colour
                      texture-width texture-height))

                    (loop [f 0 url url]

                      (<! (e/next-frame))

                      (when (not= url (:url @data-atom))
                        (log "changed from:" url "to:" (:url @data-atom))

                        (let [url (:url @data-atom)]
                          (log "URL changed" url)

                          ;; load new image
                          (<! (r/load-resources canv :fg [url] :fade-in 0.01 :fade-out 0.01))

                          (setup-canvas-image
                           url document (:scale @data-atom) image-background image-foreground
                           {})))

                      (recur (inc f) (:url @data-atom)))))))

                                        ;no url. leave canvas blank
            (do
              (<! (e/next-frame))
              (recur))
            ))))))

(defn image-canvas [data-atom & {:keys [ui-control-fn]
                                 :or {ui-control-fn (fn [c a w h] nil)}}]
  [(with-meta
     (fn [] [:canvas])
     {:component-did-mount (image-canvas-did-mount data-atom ui-control-fn)})])

;;
;; Devcards
;;
(defcard-doc
  "
## Canvas
Note: This widget is for representing infinitelives textures
")

(defcard card-component-canvas
  "A basic pixi canvas with different shape."
  (reagent/as-element [image-canvas (atom {:width 100 :height 100})]))


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
      [:p "size: "
       [:button {:on-click #(swap! data-atom update :width + 5)} "wider"]
       [:button {:on-click #(swap! data-atom update :width - 5)} "narrower"]
       [:button {:on-click #(swap! data-atom update :height + 5)} "taller"]
       [:button {:on-click #(swap! data-atom update :height - 5)} "shorter"]]
      [:p "image: "
       [:select
        {:on-change #(let [selection (.-target.value %)
                           url-map {"bunny" "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
                                    "moonhenge" "https://raw.githubusercontent.com/retrogradeorbit/moonhenge/master/resources/public/img/sprites.png"}
                           ]
                       (swap! data-atom assoc :url (url-map selection))
                       )}
        [:option "bunny"]
        [:option "moonhenge"]]
       ]
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
   [{:pos [9 12]
     :size [1 10]}]
   :scale 3
   :offset [0 0]
   :width 200 :height 200
   :url "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
   }
  {:inspect-data true
   :history true
   })

(defcard card-component-canvas-draggable
  "A basic pixi canvas with changable scale and highlight box. Right click
and drag to reposition canvas. mouse wheel to zoom. Left click and drag to select.

"
  (fn [data-atom owner]
    (reagent/as-element
     [:div
      [image-canvas data-atom :ui-control-fn canvas-control]]))
  {
   :highlights
   [{
     :pos [9 12]
     :size [1 10]}]
   :scale 3
   :offset [0 0]
   :width 300 :height 300
   :url "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"
   }
  {:inspect-data true})
