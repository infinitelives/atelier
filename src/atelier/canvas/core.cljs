(ns atelier.canvas.core
  (:require [reagent.core :as reagent]
           [infinitelives.pixi.canvas :as c]
           [infinitelives.pixi.events :as e]
           [infinitelives.pixi.resources :as r]
           [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.sprite :as s]
;            [infinitelives.pixi.pixelfont :as pf]
;            [infinitelives.utils.math :as math]
;            [infinitelives.utils.vec2 :as vec2]
;            [infinitelives.utils.events :as events]
;            [infinitelives.utils.sound :as sound]
            [infinitelives.utils.string :refer [url-keyword]]
            [infinitelives.utils.console :refer [log]]

            [cljs.core.async :refer [<! chan put! alts!]]

            [atelier.canvas.devcards :as devcards]
            [atelier.graphics :as graphics]
            )

  (:require-macros
;   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
   [cljs.core.async.macros :refer [go alt!]]
   [infinitelives.pixi.macros :as m]
   )
  )


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
                          document-width document-height]
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

(def foreground-drawing-options
  {:empty-colour 0x800000
   :border-colour 0xffffff
   :highlight-colour 0xff00ff
   :full-colour 0x0000ff})

(defn image-canvas-did-mount
  [data-atom ui-control-fn]
  (fn [this]
    (log "component-did-mount")
    (let [{:keys [url width height scale]} @data-atom
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

              (m/with-sprite canv :image
                [document (s/make-sprite :spritesheet :scale scale)]
                (let [image-background (js/PIXI.Graphics.)
                      image-foreground (js/PIXI.Graphics.)
                      document-texture (setup-canvas-image
                                        url document scale image-background image-foreground
                                        foreground-drawing-options)
                      document-width (.-width document-texture)
                      document-height (.-height document-texture)
                      ]
                  (t/set-texture! :spritesheet document-texture)
                  (set! (.-interactive image-background) true)
                  (set! (.-mousedown image-background) #(.log js/console "bg:" %))

                  (set! (.-oncontextmenu (:canvas canv))
                        (fn [e] (.preventDefault e)))

                  (ui-control-fn
                   (:canvas canv) data-atom
                   document-width document-height)

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
                    document-width document-height
                    (:empty-colour foreground-drawing-options)
                    (:border-colour foreground-drawing-options)
                    document-width document-height))

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

                    (recur (inc f) (:url @data-atom))))))

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
