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

(defn- make-editor-change-fn [data-atom codemirror]
  (fn [from to text removed origin]
    (when (not= (.-origin to) "setValue")
      ;; a change from the user, not from the data atom
      (do
        (swap! data-atom assoc
               :value (.getValue from)
               :cursor (let [curs (.getCursor codemirror "head")]
                         {:line (.-line curs)
                          :ch (.-ch curs)}))
        (let [parsed (read-string (.getValue from))]
          ;; successful parse. TODO: catch error
          (swap! data-atom assoc :parsed parsed))))))

(defn- set-codemirror-placement [codemirror width height]
  (.setSize codemirror
            (- (.-innerWidth js/window) width 18)
            (- height 2))
  (set! (.-display.wrapper.style.left codemirror)
        (str (+ 2 12 width) "px")))

(defn- make-watcher [codemirror]
  (fn [key atom old-state new-state]
    (when (not= old-state new-state)
      (when (and (:width new-state) (:height new-state))
        (set-codemirror-placement codemirror (:width new-state) (:height new-state)))

      (when (not= (:value new-state) (:value old-state))
        ;; this setValue needs to NOT trigger an onchange
        ;; so we exclude this change with the swap! in
        ;; on "change"
        (.setValue codemirror (:value new-state)))

      (when (not= (:cursor new-state) (:cursor old-state))
        (.setCursor codemirror (clj->js (:cursor new-state)))))))

(defn search-backwards [codemirror start-char end-char line curs]
  (let [str (.getLine codemirror line)
        substring (subs str 0 curs)
        end-index (.indexOf substring end-char)
        start-index (.indexOf substring start-char)
        end-found? (not= -1 end-index)
        start-found? (not= -1 start-index)
        ]
    ;; have we found an end-char? if so abort
    (cond
      (and end-found? start-found? (> end-index start-index))
      nil

      start-found?
      [line start-index]

      :default
      (recur codemirror
             start-char end-char
             (dec line) (.-length (.getLine codemirror (dec line))))
      )))
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
      (set-codemirror-placement cm width height)
      (add-watch data-atom :code-editor (make-watcher cm))
      (.on cm "change" (make-editor-change-fn data-atom cm)))))

(defn editor [data-atom & {:keys [width height]
                           :or {width 300
                                height 280}}]
  [(with-meta
     (fn [] [:div {:style {:width (str width "px")
                           :height (str height "px")
                           }}])
     {:component-did-mount (editor-did-mount data-atom :width width :height height)})])

(defcard card-component-editable-display
  "reloadable, editable code entry"
  (reagent/as-element [editor (atom nil)]))

(defcard card-component-changable-editor-with-reader
  "as you change the code, the reader is invoked and the data structure dumped. Undo and redo should work."
  (fn [data-atom owner]
    (reagent/as-element [editor data-atom]))
  {:value "{}\n"
   :cursor {:line 1 :ch 0}}
  {:inspect-data true
   :history true})
