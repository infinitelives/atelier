(ns atelier.code
  (:require [atelier.search :as search]
            [reagent.core :as reagent]
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

(defn- make-editor-change-fn
  "Make the on-change function for a codemirror instance.
  Once registered, the function returned from this is called by
  codemirror on every change.

  https://codemirror.net/doc/manual.html#events says:

  Fires every time the content of the editor is changed. The changeObj
  is a {from, to, text, removed, origin} object containing information
  about the changes that occurred as second argument. from and to are
  the positions (in the pre-change coordinate system) where the change
  started and ended (for example, it might be {ch:0, line:18} if the
  position is at the beginning of line #19). text is an array of
  strings representing the text that replaced the changed range (split
  by line). removed is the text that used to be between from and to,
  which is overwritten by this change. This event is fired before the
  end of an operation, before the DOM updates happen."
  [data-atom codemirror]
  (fn [from to text removed origin]
    (when (not= (.-origin to) "setValue")
      ;; a change from the user, not from the data atom
      (let [parsed (try
                     (read-string (.getValue from))
                     (catch :default e
                       (:parsed @data-atom)))]
        (swap! data-atom assoc
               :value (.getValue from)
               :cursor (let [curs (.getCursor codemirror "head")]
                         {:line (.-line curs)
                          :ch (.-ch curs)})
               :parsed parsed)))))

(defn- set-codemirror-placement [codemirror width height]
  (.setSize codemirror
            (- (.-innerWidth js/window) width 12)
            (- height 2))
  (set! (.-display.wrapper.style.left codemirror)
        (str (+ 2 6 width) "px")))

(defn make-watcher [codemirror]
  (fn [key atom old-state new-state]
    (when (not= (dissoc old-state :parsed) (dissoc new-state :parsed))
      (when (or
             (not= (:width new-state) (:width old-state))
             (not= (:height new-state) (:height old-state)))
        (set-codemirror-placement codemirror (:width new-state) (:height new-state)))

      (when (not= (:value new-state) (:value old-state))
        ;; this setValue needs to NOT trigger an onchange
        ;; so we exclude this change with the swap! in
        ;; on "change"
        (.setValue codemirror (:value new-state)))

      (when (not= (:cursor new-state) (:cursor old-state))
        (.setCursor codemirror (clj->js (:cursor new-state)))))))

(defn editor-did-mount [data-atom & {:keys [width height cursor-fn]}]
  (fn [this]
    (let [node (reagent/dom-node this)
          cm (js/CodeMirror
              node
              #js {:mode "clojure"
                   :lineNumbers true
                   :value (or (:value @data-atom) "")
                   :theme "zenburn"
                   :keyMap "emacs"})]
      (.setSize cm (:width data-atom) "100%")
      (set-codemirror-placement cm (:width @data-atom) (:height @data-atom))
      (add-watch data-atom :code-editor (make-watcher cm))
      (.on cm "change" (make-editor-change-fn data-atom cm))
      (.on cm "cursorActivity"
           (fn [cm]
             ;(log "cursorActivity:" cm)
             (let [curs (.getCursor cm)
                   ch (.-ch curs)
                   line (.-line curs)
                   get-line #(.getLine cm %)
                   back (search/reverse-search get-line "{" "}" line ch)
                   forward (search/forward-search get-line "{" "}" line ch)]
               (when (and back forward)
                 (let [data (read-string (search/from-to get-line back forward))]
                   (cursor-fn data)))))))))

(defn editor [data-atom & {:keys [width height cursor-fn]
                           :or {width 300
                                height 280}}]
  [(with-meta
     (fn [] [:div {:style {:width (str width "px")
                           :height (str height "px")
                           }}])
     {:component-did-mount (editor-did-mount data-atom :cursor-fn cursor-fn)})])

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
