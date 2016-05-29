(ns atelier.code
  (:require [reagent.core :as reagent]
            [cljs.tools.reader :refer [read-string]]
            [cljsjs.codemirror]
            [cljsjs.codemirror.mode.clojure]
            [cljsjs.codemirror.keymap.emacs]
            [infinitelives.utils.console :refer [log]]
)
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]])
  )

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
## Code Box
Note: This widget is for representing clojure literals as source code
")

(defn highlight-code [html-node]
  (let [nodes (.querySelectorAll html-node "pre code")]
    (loop [i (.-length nodes)]
      (when-not (neg? i)
        (when-let [item (.item nodes i)]
          (.highlightBlock js/hljs item))
        (recur (dec i))))))

(defn highlight-component [content]
  [(with-meta
     (fn []
       [:pre [:code
              {
               ;:contentEditable "true"
               :style {:background "#3f3f3f"}
               :class "clojure"
               :dangerouslySetInnerHTML
               {:__html content}}]])
     {:component-did-mount
      (fn [this]
        (let [node (reagent/dom-node this)]
          (highlight-code node)))})])

(defn component-static-code-display []
  (highlight-component
   "{
  :foo
  {
    :size [8 8]
    :pos [124 100]
  }
}"))


(defcard card-component-static-code-display
  "a static highlight/js code display"
  (reagent/as-element [component-static-code-display]))

(defn editor-did-mount [data-atom & {:keys [width height]}]
  (fn [this]
    (let [node (reagent/dom-node this)
          cm (js/CodeMirror
              node
              #js {:mode "clojure"
                   :lineNumbers true
                   :value (or (:value @data-atom) "")
                   :theme "zenburn"
                   :keyMap "emacs"})]
      (.setSize cm width "100%")
      (add-watch data-atom nil
                 (fn [key atom old-state new-state]
                   (.log js/console "fn" key atom (str old-state) (str new-state))
                   (when (not= old-state new-state)

                     (when (and (:width new-state) (:height new-state))
                       (.log js/console "set size")
                       (.setSize cm
                                 (- (.-innerWidth js/window) (:width new-state) 18)
                                 (- (:height new-state) 2))
                       (set! (.-display.wrapper.style.left cm)
                             (str (+ 2 12 (:width new-state)) "px"))
                       )

                                        ;(+ 2 12 (:split-x @screen-state))

                     (when (not= (:value new-state) (:value old-state))
                       (.log js/console "setValue")
                       ;; this setValue needs to NOT trigger an onchange
                       (.setValue cm (:value new-state)))

                     (when (not= (:cursor new-state) (:cursor old-state))
                       (.log js/console "setCursor")
                       (.setCursor cm (clj->js (:cursor new-state)))))))
      (.on cm "change"
           (fn [from to text removed origin]
             (.log js/console "onChange")
             (.log js/console "change:" (.getValue from) )
             (.log js/console "C:" from to text removed origin)
             (if (not= (.-origin to) "setValue")
               (do
                 (swap! data-atom assoc
                        :value (.getValue from)
                        :cursor (let [curs (.getCursor cm "head")]
                                  (.log js/console "curs" curs)
                                  {:line (.-line curs)
                                   :ch (.-ch curs)}))
                 (let [parsed (read-string (.getValue from))]
                   ;; successful parse. TODO: catch error
                   (swap! data-atom assoc :parsed parsed)
                   ))
               (.log js/console "skipped"))
             ))

      ;; this gets triggered on add-watch .setValue
      ;; and this screws up devcards 'redo'
      ;; TODO: filter out the unneeded events like in on change above
      #_ (.on cm "cursorActivity"
              #(do
                 (.log js/console "cursorActivity")
                 (swap! data-atom assoc
                        :cursor (let [curs (.getCursor cm "head")]
                                  (.log js/console "curs" curs)
                                  {:line (.-line curs)
                                   :ch (.-ch curs)})
                        )
                 ))
      )))



(defn editor [data-atom & {:keys [width height]
                           :or {width 640
                                height 480}}]
  [(with-meta
     (fn [] [:div {:style {:width (str width "px")
                           :height (str height "px")
                           }}])
     {:component-did-mount (editor-did-mount data-atom :width width :height height)
      :component-will-mount #(log "code-component-will-mount")
      :component-will-update #(log "code-component-will-update")
      :component-did-update #(log "code-component-did-update")
      :component-will-unmount #(log "code-component-will-unmount")

})])

(defcard card-component-editable-display
  "reloadable, editable code entry"
  (reagent/as-element [editor (atom nil)]))


(defcard card-component-changable-editor-with-reader
  "as you change the code, the reader is invoked and the data structure dumped. Undo and redo should work."
  (fn [data-atom owner]
    (reagent/as-element [editor data-atom]))
                                        ;(reagent/as-element [editor (atom nil)])
  {:value "{}\n"
   :cursor {:line 1 :ch 0}}
  {:inspect-data true
   :history true}
  ;(reagent/as-element [editor (atom nil)])
)