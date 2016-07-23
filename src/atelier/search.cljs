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

(defn reverse-search
  ([get-line start-char end-char line curs]
   (reverse-search get-line start-char end-char line curs 0))
  ([get-line start-char end-char line curs nested-count]
   (when-let [str (get-line line)]
     (let [start (reverse-search-string str curs start-char)
           end (reverse-search-string str curs end-char)]
       (cond
         ;; ran off the start of the line. continue on next line
         (neg? curs)
         (recur get-line
                start-char end-char
                (dec line) (.-length (get-line (dec line))) nested-count)

         ;; start and end appear together, we recur, same level of nesting
         (and end start (> end start))
         (recur get-line start-char end-char
                line (dec start) nested-count)

         ;; only end appears. we recur, but nested one deeper
         (and end (not start))
         (recur get-line start-char end-char
                line (dec end) (inc nested-count))

         ;; start appears, but we are nested. continue nested one shallower
         (and start (pos? nested-count))
         (recur get-line start-char end-char
                line (dec start) (dec nested-count))

         ;; start appears and we are outside any nests. The answer!
         (and start (zero? nested-count))
         [line start]

         ;; this line is searched. next line
         (> line 0)
         (recur get-line
                start-char end-char
                (dec line) (.-length (get-line (dec line))) nested-count)

         ;; broken syntax. mismatched start/ends
         :default nil
         )))))

(defn forward-search
  ([get-line start-char end-char line curs]
   (forward-search get-line start-char end-char line curs 0))
  ([get-line start-char end-char line curs nested-count]
   (when-let [str (get-line line)]
     (let [start (forward-search-string str curs start-char)
           end (forward-search-string str curs end-char)]
       (cond
         ;; ran off the start of the line. continue on next line
         (neg? curs)
         (recur get-line
                start-char end-char
                (inc line) 0 nested-count)

         ;; start and end appear together, we recur, same level of nesting
         (and end start (> end start))
         (recur get-line start-char end-char
                line (inc end) nested-count)

         ;; only start appears. we recur, but nested one deeper
         (and start (not end))
         (recur get-line start-char end-char
                line (inc start) (inc nested-count))

         ;; end appears, but we are nested. continue nested one shallower
         (and end (pos? nested-count))
         (recur get-line start-char end-char
                line (inc end) (dec nested-count))

         ;; start appears and we are outside any nests. The answer!
         (and end (zero? nested-count))
         [line end]

         ;; this line is searched. next line
         :default
         (recur get-line
                start-char end-char
                (inc line) 0 nested-count))))))

(defn from-to [get-line [ls cs] [le ce]]
  (let [string (get-line ls)]
    (cond
      (= le ls)
      (subs string cs (inc ce))

      (> le ls)
      (str (subs string cs) (from-to get-line [(inc ls) 0] [le ce]))

      :default
      nil)))
