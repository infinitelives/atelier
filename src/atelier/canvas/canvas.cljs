(ns atelier.canvas.canvas
  (:require [reagent.core :as reagent]
            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.events :as e]
            [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.utils.string :refer [url-keyword]]
            [infinitelives.utils.console :refer [log]]

            [cljs.core.async :refer [<! chan put! alts!]]
            [atelier.graphics :as graphics])
  (:require-macros
   [cljs.core.async.macros :refer [go alt!]]
   [infinitelives.pixi.macros :as m]))


(defn- get-document-size [document]
  (let [texture (.-texture document)]
    [(.-width texture) (.-height texture)]))

(defn- draw-image-background [image-background document
                              {:keys [scale]}
                              {:keys [empty-colour border-colour]}]
  (let [[document-width document-height] (get-document-size document)]
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
                 (inc (* scale document-height))))))

(defn set-canvas-pos! [canv pos]
  (apply s/set-pivot! (m/get-layer canv :bg) pos)
  (apply s/set-pivot! (m/get-layer canv :image) pos)
  (apply s/set-pivot! (m/get-layer canv :fg) pos))

(defn draw-all-highlight-rectangles! [sprite document-size {:keys [scale highlights]}]
  (loop [[{:keys [pos size]} & tail] highlights]
    (graphics/draw-foreground-rectangle sprite scale pos size document-size)
    (when (seq tail)
      (recur tail))))

(defn set-canvas-size! [canv {:keys [width height]}]
  (set! (.-style.width (:canvas canv)) (str width))
  (set! (.-style.height (:canvas canv)) (str height))
  ((:resize-fn canv) width height))

(defn set-canvas-offset! [canv {[x y] :offset}]
  (set-canvas-pos! canv [(- x) (- y)]))

(defn set-canvas-highlights! [canv image-foreground document-size data]
  (.clear image-foreground)
  (draw-all-highlight-rectangles! image-foreground document-size data))

(defn set-canvas-scale! [canv
                         {:keys [document image-background image-foreground]}
                         data background-options]
  (s/set-scale! document (:scale data))
  (draw-image-background image-background document data background-options)
  (set-canvas-highlights! canv image-foreground (get-document-size document) data))

(defn make-atom-watch-fn [canv
                          {:keys [document image-foreground image-background] :as layers}
                          {:keys [empty-colour border-colour] :as foreground-drawing-options}]
  (fn [key atom old-state
       {:keys [scale highlights offset width height url]
        :as new-state}]
    ;; canvas widget size
    (when (and width height)
      (set-canvas-size! canv new-state))

    ;; offset changed
    (when-not (= (:offset old-state) offset)
      (set-canvas-offset! canv new-state))

    ;; scale changed
    (when-not (= (:scale old-state) scale)
      (set-canvas-scale! canv layers new-state foreground-drawing-options))

    ;; highlights changed
    (when-not (= (:highlights old-state) highlights)
      (set-canvas-highlights! canv image-foreground (get-document-size document) new-state))))


(defn setup-canvas-image [canv
                          {:keys [document image-foreground image-background]}
                          {:keys [url scale highlights] :as data}
                          {:keys [empty-colour border-colour
                                  highlight-colour full-colour]
                           :as foreground-drawing-options
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

    (draw-image-background image-background document data foreground-drawing-options)
    (set-canvas-highlights! canv image-foreground
                            [document-width document-height]
                            data)

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
    (let [{:keys [url width height scale highlights]} @data-atom
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
                      layers {:image-foreground image-foreground
                              :document document
                              :image-background image-background}
                      document-texture (setup-canvas-image canv layers @data-atom foreground-drawing-options)]
                  (t/set-texture! :spritesheet document-texture)
                  (set! (.-interactive image-background) true)
                  (set! (.-oncontextmenu (:canvas canv))
                        (fn [e] (.preventDefault e)))

                  (ui-control-fn (:canvas canv) data-atom (.-width document-texture) (.-height document-texture))

                  (setup-canvas-image canv layers @data-atom foreground-drawing-options)

                  (.addChild (m/get-layer canv :bg) image-background)
                  (.addChild (m/get-layer canv :fg) image-foreground)

                  (set-canvas-pos! canv [0 0])

                  (add-watch data-atom :dummy
                   (make-atom-watch-fn canv layers foreground-drawing-options))

                  (loop [f 0 url url]

                    (<! (e/next-frame))

                    (when (not= url (:url @data-atom))
                      (log "changed from:" url "to:" (:url @data-atom))

                      (let [url (:url @data-atom)]
                        (log "URL changed" url)

                        ;; load new image
                        (<! (r/load-resources canv :fg [url] :fade-in 0.01 :fade-out 0.01))

                        (setup-canvas-image canv layers @data-atom foreground-drawing-options)))

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
