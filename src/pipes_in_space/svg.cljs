(ns pipes-in-space.svg)

(defn inlet-arrows [inlets]
  (map-indexed (fn [i ord]
                 ^{:key i}
                 [:g {:stroke "#222"
                      :stroke-width 2
                      :transform (let [deg (case ord
                                             :n 0
                                             :e 90
                                             :s 180
                                             :w 270)]
                                   (str "rotate(" deg ", 25, 25)"))}
                  [:polyline {:points "21 5, 25 10, 29 5"}]
                  [:polyline {:points "21 10, 25 15, 29 10"}]])
               inlets))

(defmulti piece-svg :type)

(defmethod piece-svg :empty [_] nil)
(defmethod piece-svg nil [_] nil)

(defmethod piece-svg :block [_]
  [:svg {:viewBox [0 0 50 50]}
   [:rect {:x 0 :y 0 :width 50 :height 50 :stroke :none :fill #_ "#6f6f6f" "#000"}]])

(defmethod piece-svg :start [{:keys [direction]}]
  [:svg {:viewBox [0 0 50 50]}
   [:g {:transform (let [deg (case direction
                               :n 0
                               :e 90
                               :s 180
                               :w 270)]
                     (str "rotate(" deg ", 25, 25)"))}
    [:line {:x1 25 :y1 25 :x2 25 :y2 0 :stroke-width 15 :stroke-linecap :round}]
    [:line {:x1 15 :y1 0 :x2 35 :y2 0 :stroke-width 5}]]])

(defmethod piece-svg :straight [{:keys [axis inlets]}]
  [:svg {:viewBox [0 0 50 50]}
   [:g (when (= axis :h) {:transform "rotate(90, 25, 25)"})
    [:line {:x1 25 :y1 0 :x2 25 :y2 50 :stroke-width 15}]
    [:line {:x1 15 :y1 0 :x2 35 :y2 0 :stroke-width 5}]
    [:line {:x1 15 :y1 50 :x2 35 :y2 50 :stroke-width 5}]]
   (inlet-arrows inlets)])

(defmethod piece-svg :cross [{:keys [inlets]}]
  [:svg {:viewBox [0 0 50 50]}
   [:g
    [:line {:x1 25 :y1 0 :x2 25 :y2 50 :stroke-width 15}]
    [:line {:x1 15 :y1 0 :x2 35 :y2 0 :stroke-width 5}]
    [:line {:x1 15 :y1 50 :x2 35 :y2 50 :stroke-width 5}]]
   [:g {:transform "rotate(90, 25, 25)"}
    [:line {:x1 25 :y1 0 :x2 25 :y2 50 :stroke-width 15}]
    [:line {:x1 15 :y1 0 :x2 35 :y2 0 :stroke-width 5}]
    [:line {:x1 15 :y1 50 :x2 35 :y2 50 :stroke-width 5}]]
   (inlet-arrows inlets)])

(defmethod piece-svg :angle [{:keys [corner inlets]}]
  [:svg {:viewBox [0 0 50 50]}
   [:g {:transform (let [deg (case corner
                               :ne 0
                               :se 90
                               :sw 180
                               :nw 270)]
                     (str "rotate(" deg ", 25, 25)"))}
    [:polyline {:points "25 0, 25 25, 50 25" :stroke-width 15 :stroke-linejoin :round}]
    [:line {:x1 15 :y1 0 :x2 35 :y2 0 :stroke-width 5}]
    [:line {:x1 50 :y1 15 :x2 50 :y2 35 :stroke-width 5}]]
   (inlet-arrows inlets)])

(defmethod piece-svg :default [_] "?")

(defmulti goo-svg (fn [piece _ _ _] (:type piece)))

(defmethod goo-svg :start [_ _ outlet pct]
  [:svg {:class :goo :viewBox [0 0 50 50]}
   [:g {:transform (let [deg (case outlet
                               :s 0
                               :w 90
                               :n 180
                               :e 270)]
                     (str "rotate(" deg ", 25, 25)"))}
    [:mask {:id "start-goo-mask"} [:rect {:x 0 :y 0 :width 50 :height (* pct 50) :fill :white}]]
    ; masks don't work with lines, so we use a circle and a rectangle
    ; <https://stackoverflow.com/a/53476700/15204>
    [:g {:fill "#6b5eb3" :mask "url(#start-goo-mask)"}
     [:circle {:cx 25 :cy 25 :r 5}]
     [:rect {:x 20 :y 25 :width 10 :height 25}]]]])

(defmethod goo-svg :straight [_ inlet _ pct]
  [:svg {:class :goo :viewBox [0 0 50 50]}
   [:g {:transform (let [deg (case inlet
                               :n 0
                               :e 90
                               :s 180
                               :w 270)]
                     (str "rotate(" deg ", 25, 25)"))}
    [:line {:x1 25 :y1 0 :x2 25 :y2 (* pct 50) :stroke-width 10 :stroke "#6b5eb3"}]]])

(defmethod goo-svg :cross [_ inlet _ pct]
  [:svg {:class :goo :viewBox [0 0 50 50]}
   [:g {:transform (let [deg (case inlet
                               :n 0
                               :e 90
                               :s 180
                               :w 270)]
                     (str "rotate(" deg ", 25, 25)"))}
    [:line {:x1 25 :y1 0 :x2 25 :y2 (* pct 50) :stroke-width 10 :stroke "#6b5eb3"}]]])

(defmethod goo-svg :angle [_ inlet outlet pct]
  [:svg {:class :goo :viewBox [0 0 50 50]}
   [:g {:transform (let [[deg flip] (case [inlet outlet]
                                      [:n :e] [0 false]
                                      [:e :s] [90 false]
                                      [:s :w] [180 false]
                                      [:w :n] [270 false]
                                      [:s :e] [0 true]
                                      [:w :s] [90 true]
                                      [:n :w] [180 true]
                                      [:e :n] [270 true])]
                     (str "rotate(" deg ", 25, 25)" (when flip "scale(1, -1) translate(0, -50)")))}
    [:polyline {:points (condp >= pct
                          .5 (str "25 0, 25 " (* pct 2 25))
                          1 (str "25 0, 25 25, " (+ 25 (* (- pct .5) 2 25)) " 25"))
                :stroke-width 10
                :stroke-linejoin :round
                :stroke "#6b5eb3"}]]])

(defmethod goo-svg :default [_ _ _ _] nil)
