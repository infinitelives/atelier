(ns atelier.test
  (:require [cljs.test :refer-macros [run-all-tests]]
            [atelier.search-test]))

(enable-console-print!)

(defn ^:export run
  []
  (run-all-tests #"atelier.*-test"))
