(ns atelier.core
  (:require [reagent.core :as reagent]
            [atelier.code :as code])
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

(defcard-doc
  "
## Rendering Reagent components
Note: The following examples assume a namespace that looks like this:
```clojure
(ns xxx
    (:require [devcards.core]
              [reagent.core :as reagent])
    (:require-macros [devcards.core :as dc
                                    :refer [defcard defcard-rg]]))
```
")

(defcard
    "## Devcard Reagent basics
First of all, you don't need to use any of Devcards helpers to use Reagent.
You can use the `reagent.core/as-element` function to turn your
reagent code into a ReactElement.
The following works great:
```clojure
(defcard reagent-no-help
  (reagent/as-element [:h1 \"Reagent example\"]))
```
You can see the example rendered below:
")

(defcard reagent-no-help

  (reagent/as-element [:h1 "Reagent example"]))

(defcard
  "This will also work with Reagent components
```clojure
(defn reagent-component-example []
  [:div \"hey there\"])
(defcard reagent-no-help
  (reagent/as-element [reagent-component-example]))
```
And you can see this rendered below:
")

(defn reagent-component-example []
  [:div "hey there"])

(defcard reagent-component
  (reagent/as-element [reagent-component-example]))
