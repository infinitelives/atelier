(ns atelier.search)

(def !-1 (partial not= -1))

(defn forward-search-string
  "search a `string` from position `pos` forwards until `sub` is
  found. returns nil if the substring isn't found"
  [string pos char]
  (let [found (.indexOf (subs string pos) char)]
    (when (!-1 found)
      (+ pos found))))

(defn reverse-search-string
  "search a `string` from position `pos` backwards until `sub` is
  found. returns nil if the substring isn't found"
  [string pos char]
  (let [found (.lastIndexOf (subs string 0 (+ pos (count char))) char)]
    (when (!-1 found)
      found)))

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
        (or
         (and end-found? start-found? (> end-index start-index))
         (and end-found? (not start-found?)))
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
        (or
         (and start-found? end-found? (> end-index start-index))
         (and start-found? (not end-found?)))
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
