(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.canvas :as canvas]
            [atelier.code :as code]

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


(defn on-click [ratom]
  (swap! ratom update-in [:count] inc))
