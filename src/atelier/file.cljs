(ns atelier.file
  (:require [infinitelives.utils.console :refer [log]]
            [infinitelives.pixi.resources :as resources]
            [cljs.core.async :refer [<! chan put! alts! timeout]]
            )
  (:require-macros [cljs.core.async.macros :refer [go]])
  )

;; HTML5 File API code

(defn filelist->seq [filelist]
  (for [n (range (.-length filelist))] (aget filelist n)))

(defn read-file [file & [type]]
  (let [c (chan)
        reader (js/FileReader.)]
    (set! (.-onload reader) #(put! c (.-target.result %)))
    (case type
      :binary-string (.readAsBinaryString reader file)
      :text (.readAsText reader file)
      :array-buffer (.readAsArrayBuffer reader file)
      (:data-url nil) (.readAsDataURL reader file))
    c))

(defn handle-file-select [set-url-fn event]
  (let [files (filelist->seq (.-target.files event))
        file (first files)]
    (go
      (log "recieved:"
           (-> file
               (read-file :data-url)
               <!
               set-url-fn
               ;; resources/load
               ;; <!
               ;; second
               ;; :nearest
               )))))

(defn file-selection [set-url-fn]
  [:input#files
   {:type "file" :name "files" :multiple true
    :on-change (partial handle-file-select set-url-fn)}])
