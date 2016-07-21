(ns atelier.search)

(defn search-backwards [codemirror start-char end-char line curs]
  (when-let [str (.getLine codemirror line)]
    (let [
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

        (> line 0)
        (recur codemirror
               start-char end-char
               (dec line) (.-length (.getLine codemirror (dec line))))

        :default nil
        ))))

(defn search-forwards [codemirror start-char end-char line curs]
  (when-let [str (.getLine codemirror line)]
    (let [substring (subs str curs)
          end-index (.indexOf substring end-char)
          start-index (.indexOf substring start-char)
          end-found? (not= -1 end-index)
          start-found? (not= -1 start-index)
          ]
      (cond
        (and start-found? end-found? (> end-index start-index))
        nil

        end-found?
        [line (+ curs end-index)]

        :default
        (recur codemirror
               start-char end-char
               (inc line) 0)))))

(defn from-to [codemirror [ls cs] [le ce]]
  (let [string (.getLine codemirror ls)]
    (cond
      (= le ls)
      (subs string cs (inc ce))

      (> le ls)
      (str (subs string cs) (from-to codemirror [(inc ls) 0] [le ce]))

      :default
      nil)))
