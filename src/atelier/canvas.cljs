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

(defn draw-chessboard [gfx & {:keys [start-x start-y width height
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
     (do
       (.log js/console cx cy)
       (doto gfx
         (.beginFill colour-2)
         (.lineStyle 0 0x000000)
         (.drawRect (+ start-x (* tile-width cx))
                    (+ start-y (* tile-height cy))

                    (-
                     (min
                      (+ start-x (* tile-width (inc cx)))
                      (+ start-x width))
                     (+ start-x (* tile-width cx)))

                    (-
                     (min
                      (+ start-y (* tile-height (inc cy)))
                      (+ start-y height))
                     (+ start-y (* tile-height cy)))
                    )
         .endFill
         ))

     ))



  (
   )
  )

(defn image-canvas-did-mount [data-atom]
  (fn [this]
    (let [canv (c/init
                {:layers [:bg :fg]
                 :background 0x404040

                 ;; canvas width and geight from the dom node are wrong at this point
                 ;; so to keep aspect ratio correct we pass them in
                 :width 640 :height 200
                 :canvas (reagent/dom-node this)})
          bg-url "http://www.goodboydigital.com/pixijs/examples/1/bunny.png"]
      (go
        (<! (r/load-resources canv :fg [bg-url]))

        (t/set-texture! :rabbit (r/get-texture :bunny :nearest))

        (m/with-sprite canv :fg
          [rabbit (s/make-sprite :rabbit :scale 3)]
          (let [box (js/PIXI.Graphics.)
                empty-colour 0x800000
                border-colour 0xffffff
                bunny-width 26
                bunny-height 37
                scale 3
                full-colour 0x0000ff

                ]
            (doto box
              (.beginFill empty-colour 0.0)
              (.lineStyle 1 border-colour)
              (.drawRect (dec (* scale -0.5 bunny-width)) (dec (* scale -0.5 bunny-height))
                         (inc (* scale bunny-width)) (inc (* scale bunny-height)))
              )


            (draw-chessboard
             box
             :start-x (* scale -0.5 bunny-width)
             :start-y (* scale -0.5 bunny-height)
             :width (* scale bunny-width)
             :height (* scale bunny-height))

            (.addChild (m/get-layer canv :bg) box)

            (loop [f 0]
              (<! (e/next-frame))
              (recur (inc f)))))))))

(defn image-canvas [data-atom]
  [(with-meta
     (fn [] [:canvas {:style {:width "640px" :height "200px"}}])
     {:component-did-mount (image-canvas-did-mount data-atom)})]
)

(defcard card-component-canvas
  "A basic pixi canvas with different shape. Need to fix the aspect ratio bug (when specifying custom canvas div."
  (reagent/as-element [image-canvas (atom nil)])
  )
