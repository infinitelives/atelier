(ns atelier.about)

(defn about-dialog [close-fn]
  [:div.about {:style {:position "absolute"
                       :width "500px"
                       :height "500px"
                       :left "300px"
                       :top "100px"
                       :background "#eee"
                       :padding "16px"
                       :z-index 10 }}
   [:div {:on-click close-fn
          :style {:float "right"}} "❌"]
   [:h1 "About Atelier"]
   [:p "Version 0.1"]
   [:p "Copyright © 2016, Crispin Wellington <retrogradeorbit@gmail.com>"]
   [:p "This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version."]
   [:p "This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details."]
   [:p "You should have received a copy of the GNU General Public License
    along with this program.  If not, see "
    [:a {:href "http://www.gnu.org/licenses/"} "http://www.gnu.org/licenses/"]]
   [:p "The source code of the project can be found here: "
    [:a {:href "https://github.com/infinitelives/atelier"} "https://github.com/infinitelives/atelier"]]
   ])
