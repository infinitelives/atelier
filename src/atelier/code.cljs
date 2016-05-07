(ns atelier.code
  (:require [reagent.core :as reagent])
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
               :contenteditable "true"
               :style {:background "#3f3f3f"}
               :class "clojure"
               :dangerouslySetInnerHTML
               {:__html content}}]])
     {:component-did-mount
      (fn [this]
        (let [node (reagent/dom-node this)]
          (highlight-code node)))})])

(defn component-code []
  (highlight-component
   "{
  :foo
  {
    :size [8 8]
    :pos [124 100]
  }
}"))

(defcard card-component-code
  (reagent/as-element [component-code]))

(defcard card-)
