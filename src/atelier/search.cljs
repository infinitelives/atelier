(ns atelier.search)

(def !-1 (partial not= -1))

(defn forward-search-string
  "search a `string` from position `pos` forwards until `sub` is
  found. returns nil if the substring isn't found"
  [string pos sub]
  (let [found (.indexOf (subs string pos) sub)]
    (when (!-1 found)
      (+ pos found))))

(defn reverse-search-string
  "search a `string` from position `pos` backwards until `sub` is
  found. returns nil if the substring isn't found"
  [string pos sub]
  (let [found (.lastIndexOf (subs string 0 (+ pos (count sub))) sub)]
    (when (!-1 found)
      found)))

(defn reverse-search [get-line start-char end-char line curs]
  (when-let [str (get-line line)]
    (let [start (reverse-search-string str curs start-char)
          end (reverse-search-string str curs end-char)]
      (cond
        (and end start (> end start))
        nil

        (and end (not start))
        nil

        start
        [line start]

        (> line 0)
        (recur get-line
               start-char end-char
               (dec line) (.-length (get-line (dec line))))

        :default nil
        ))))

(defn forward-search [get-line start-char end-char line curs]
  (when-let [str (get-line line)]
    (let [start (forward-search-string str curs start-char)
          end (forward-search-string str curs end-char)]
      (cond
        (and start end (> end start))
        nil

        (and start (not end))
        nil

        end
        [line (+ curs end)]

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
