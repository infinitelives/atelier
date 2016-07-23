(ns atelier.search)

(def !-1 (partial not= -1))

(defn reverse-search [get-line start-char end-char line curs]
  (when-let [str (get-line line)]
    (let [
          substring (subs str 0 curs)
          end-index (.indexOf substring end-char)
          start-index (.indexOf substring start-char)
          end-found? (!-1 end-index)
          start-found? (!-1 start-index)
          ]
      ;; have we found an end-char? if so abort
      (cond
        (and end-found? start-found? (> end-index start-index))
        nil

        start-found?
        [line start-index]

        (> line 0)
        (recur get-line
               start-char end-char
               (dec line) (.-length (get-line (dec line))))

        :default nil
        ))))

(defn forward-search [get-line start-char end-char line curs]
  (when-let [str (get-line line)]
    (let [substring (subs str curs)
          end-index (.indexOf substring end-char)
          start-index (.indexOf substring start-char)
          end-found? (!-1 end-index)
          start-found? (!-1 start-index)
          ]
      (cond
        (and start-found? end-found? (> end-index start-index))
        nil

        end-found?
        [line (+ curs end-index)]

        :default
        (recur get-line
               start-char end-char
               (inc line) 0)))))

(defn from-to [get-line [ls cs] [le ce]]
  (let [string (get-line ls)]
    (cond
      (= le ls)
      (subs string cs (inc ce))

      (> le ls)
      (str (subs string cs) (from-to get-line [(inc ls) 0] [le ce]))

      :default
      nil)))
