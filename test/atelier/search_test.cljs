(ns atelier.search-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as string]
            [cljs.tools.reader :refer [read-string]]
            [atelier.search :as s]))

(def src "{
  :foo {:size [10 10]
        :pos [20 20]}
  :bar {:pos [0 0] :size [16 16]}
  :baz {
    :size [2 2]
    :pos [2 2]
  }
}")

(def src-lines (string/split src #"\n"))

(deftest search
  (is
   (= (s/reverse-search src-lines "{" "}" 2 0)
      [1 7]))
  (is
   (= (s/forward-search src-lines "{" "}" 2 0)
      [2 20])))

(deftest from-to
  (is
   (= (read-string (s/from-to src-lines [1 7] [2 20]))
      {:pos [20 20] :size [10 10]})))


(deftest nested-search
    (is
     (= (s/reverse-search src-lines "{" "}" 3 0)
        [0 0]))
    (is
     (= (s/forward-search src-lines "{" "}" 1 0)
        [8 0])))

(def src2 "{
  :foo {
    :bar {
      :baz [1 2]
    }
  }
}")

(def src2-lines (string/split src2 #"\n"))

(deftest expand-hashmap
  (is
   (= (s/expand-hashmap src2-lines {:start [3 10] :end [3 10]})
      {:start [2 9] :end [4 4]}))
  (is
   (= (s/expand-hashmap src2-lines {:start [2 9] :end [4 4]})
      {:start [1 7] :end [5 2]}))
  (is
   (= (s/expand-hashmap src2-lines {:start [1 7] :end [5 2]})
      {:start [0 0] :end [6 0]})))

(deftest forward-search-string
  (is (= (s/forward-search-string "foo bar baz" 0 "bar") 4))
  (is (= (s/forward-search-string "foo bar baz" 4 "bar") 4))
  (is (= (s/forward-search-string "foo bar baz" 5 "bar") nil))
  (is (= (s/forward-search-string "foo bar baz foo" 0 "foo") 0))
  (is (= (s/forward-search-string "foo bar baz foo" 1 "foo") 12)))

(deftest reverse-search-string
  (is (= (s/reverse-search-string "foo bar baz" 10 "baz") 8))
  (is (= (s/reverse-search-string "foo bar baz" 8 "baz") 8))
  (is (= (s/reverse-search-string "foo bar baz" 7 "baz") nil))
  (is (= (s/reverse-search-string "foo bar baz foo" 10 "foo") 0))
  (is (= (s/reverse-search-string "foo bar baz foo" 0 "foo") 0)))
