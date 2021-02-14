(ns ^:figwheel-hooks pipes-in-space.core
  (:require
   [pipes-in-space.svg :as svg]
   [clojure.core.async :as a]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]))

(set! *print-length* 20)
(enable-console-print!)


;;; basics

(def dimensions [7 10])

(def empty-board
  (let [[h w] dimensions]
    (vec (repeat h (vec (repeat w {:type :empty}))))))

(def start-state {:board empty-board
                  :game-state :idle
                  :score 0
                  :won-last-level false})

(defonce state-atom (atom start-state))

(def queue-pieces [{:type :cross}
                   {:type :straight :axis :h}
                   {:type :straight :axis :v}
                   {:type :angle :corner :ne}
                   {:type :angle :corner :nw}
                   {:type :angle :corner :se}
                   {:type :angle :corner :sw}])

(def unidirectional-queue-pieces [{:type :straight :axis :h :inlets #{:e}}
                                  {:type :straight :axis :h :inlets #{:w}}
                                  {:type :straight :axis :v :inlets #{:n}}
                                  {:type :straight :axis :v :inlets #{:s}}
                                  {:type :angle :corner :ne :inlets #{:n}}
                                  {:type :angle :corner :ne :inlets #{:e}}
                                  {:type :angle :corner :nw :inlets #{:n}}
                                  {:type :angle :corner :nw :inlets #{:w}}
                                  {:type :angle :corner :se :inlets #{:s}}
                                  {:type :angle :corner :se :inlets #{:e}}
                                  {:type :angle :corner :sw :inlets #{:s}}
                                  {:type :angle :corner :sw :inlets #{:w}}])

(defn level-params [level-num]
  (if (= (mod level-num 4) 3)
    (merge (level-params (- level-num 2)) {:has-exit true})
    {:goo-delay (- 17 (/ level-num 3))
     :goo-speed (* (+ level-num 3) 0.06)
     :has-warp (> level-num 1)
     :min-pipeline-length (+ 14 level-num)
     :num-blocks (max 0 (- level-num 1))
     :queue-pieces (if (> level-num 3)
                     (apply concat unidirectional-queue-pieces (repeat 10 queue-pieces))
                     queue-pieces)}))

(defn floor [x]
  (.floor js/Math x))

(defn ceil [x]
  (.ceil js/Math x))


;;; board

(defn neighbor-at-ord [[y x] ord]
  (let [[h w] dimensions]
    (case ord
      :n (if (pos? y) [(- y 1) x] nil)
      :s (if (< y (- h 1)) [(+ y 1) x] nil)
      :w (if (pos? x) [y (- x 1)] nil)
      :e (if (< x (- w 1)) [y (+ x 1)] nil))))

(def opp-ord {:e :w :w :e :n :s :s :n})

(def boundaries
  (let [[h w] dimensions]
    (concat
      (map #(-> [[0 %] :n]) (range w))
      (map #(-> [[(- h 1) %] :s]) (range w))
      (map #(-> [[% (- w 1)] :e]) (range h))
      (map #(-> [[% 0] :w]) (range h)))))

(defn opp-point-ord [[[y x] ord]]
  (let [[h w] dimensions]
    (case ord
      (:n :s) [[(- h y 1) x] (opp-ord ord)]
      (:e :w) [[y (- w x 1)] (opp-ord ord)])))

(defn boundary? [[y x]]
  (let [[h w] dimensions]
    (or (= y 0) (= y (- h 1)) (= x 0) (= x (- w 1)))))

(defn boundary-ords [[y x]]
  (let [[h w] dimensions]
    (cond
      (= [y x] [0 0]) [:w :n]
      (= [y x] [0 (- w 1)]) [:e :n]
      (= [y x] [(- h 1) 0]) [:w :s]
      (= [y x] [(- h 1) (- w 1)]) [:e :s]
      (= y 0) [:n]
      (= y (- h 1)) [:s]
      (= x 0) [:w]
      (= x (- w 1)) [:e]
      :else [])))


;;; pieces

(defmulti pipes
  "Return a map from inlets to outlets for the given piece.
  Not all pipes are necessarily open. Use `open-pipes` for that."
  :type)

(defmethod pipes :empty [_] {})
(defmethod pipes :block [_] {})
(defmethod pipes :start [_] {})

(defmethod pipes :cross [_]
  {:n :s :s :n :e :w :w :e})

(defmethod pipes :straight [{:keys [axis]}]
  (case axis
    :h {:e :w :w :e}
    :v {:n :s :s :n}))

(defmethod pipes :angle [{:keys [corner]}]
  (let [[ord1 ord2] (map keyword (name corner))]
    {ord1 ord2 ord2 ord1}))

(defmethod pipes :reservoir [{:keys [axis]}]
  (case axis
    :h {:e :w :w :e}
    :v {:n :s :s :n}))

(defn open-pipes
  "Return a map from *open* inlets to outlets for the given piece."
  [piece]
  (let [ps (pipes piece)]
    (if (:inlets piece)
      (select-keys ps (:inlets piece))
      ps)))

(defmulti pipe-volume :type)
(defmethod pipe-volume :reservoir [_] 2)
(defmethod pipe-volume :default [_] 1)


;;; pipeline

(defn trace-pipeline
  "Return a vector describing the pipeline from the given point.
  Each element of the vector is a pair of [[y x] [inlet outlet]], where inlet
  and outlet are ordinal directions (except for the first element, where the
  inlet is :start)."
  ([board warps point]
   (trace-pipeline board warps point :start))
  ([board warps point inlet]
   (vec
     (when point
       (let [piece (get-in board point)]
         (when-let [outlet (if (= inlet :start)
                             (:direction piece)
                             ((open-pipes piece) inlet))]
           (let [[next-point next-inlet] (or (warps [point outlet])
                                             [(neighbor-at-ord point outlet) (opp-ord outlet)])]
             ; we use self-call recursion here, but the recursion depth is
             ; bounded so i think it's ok.
             (cons [point [inlet outlet]] (trace-pipeline board warps next-point next-inlet)))))))))

(defn flow-map
  "Return a map from point to flows, where flows is a sequence of
  [inlet outlet percent-full] vectors."
  [pipeline goo-distance]
  (first (reduce (fn [[m gd] [point [inlet outlet]]]
                   [(merge-with concat m {point [[inlet outlet (min gd 1)]]})
                    (max 0 (- gd 1))])
                 [{} goo-distance]
                 pipeline)))

(defn unused-pieces
  "Return a sequence of non-fixed pieces on the board that are not part of the pipeline."
  [{:keys [board pipeline]}]
  (let [[h w] dimensions
        pipeline-points (into #{} (map first pipeline))]
    (for [y (range h)
          x (range w)
          :when (let [piece (get-in board [y x])]
                  (and (not (:fixed piece))
                       (not= (:type piece) :empty)
                       (not (contains? pipeline-points [y x]))))]
      [y x])))

(defn add-piece
  "Take a piece from the queue and add it to the board at the given location.
  Update the pipeline."
  [{:keys [board queue start-point warps] :as state} [y x]]
  (let [board (assoc-in board [y x] (first queue))]
    (assoc state
           :board board
           :queue (drop 1 queue)
           :pipeline (trace-pipeline board warps start-point))))

(defn goo-distance
  "Return a [distance overflow] pair, where distance is the distance (in pieces)
  of the goo currently in the system, and overflow is a flag indicating whether
  the goo volume exceeds the pipeline volume."
  [board pipeline goo-volume]
  (loop [gv goo-volume
         pl pipeline
         gd 0]
    (if-let [point (ffirst pl)]
      (let [piece (get-in board point)
            piece-volume (pipe-volume piece)]
        (if (> gv piece-volume)
          (recur (- gv piece-volume) (next pl) (inc gd))
          [(+ gd (/ gv piece-volume)) false]))
      [gd true])))

(defn contains-goo? [board pipeline goo-volume [y x]]
  (let [gd (ceil (first (goo-distance board pipeline goo-volume)))]
    (boolean (some #{[y x]} (take gd (map first pipeline))))))


;;; queue

(defn weighted-range
  "Return a range up to n where 0 is repeated n times, 1 is repeated n-1 times,
  and so on."
  [n]
  (mapcat #(repeat (- n %) %) (range n)))

(defn randish-seq
  "Return a lazy sequence of randomly selected values from the given vector.
  Favor less recently returned values."
  [xs]
  (let [poss-indexes (vec (weighted-range (count xs)))]
    (lazy-seq
      (let [i (rand-nth poss-indexes)
            x (nth xs i)
            xs (conj (into (subvec xs 0 i) (subvec xs (inc i))) x)]
        (cons x (randish-seq xs))))))


;;; board setup

(defn choose-start [open-points]
  (let [start-point (rand-nth (filter (complement boundary?) open-points))
        start-ord (rand-nth [:n :s :e :w])
        start-piece {:type :start :direction start-ord}
        next-point (neighbor-at-ord start-point start-ord)
        open-points (disj open-points start-point next-point)]
    [open-points start-point start-piece]))

(defn choose-warps [open-points n]
  (loop [open-points open-points
         n n
         warps {}]
    (if (pos? n)
      (let [warp-point-a (rand-nth (filter boundary? open-points))
            warp-ord-a (rand-nth (boundary-ords warp-point-a))
            warp-a [warp-point-a warp-ord-a]
            [warp-point-b _ :as warp-b] (opp-point-ord warp-a)
            open-points (disj open-points warp-point-a warp-point-b)
            warps (assoc warps warp-a warp-b warp-b warp-a)]
        (recur open-points (dec n) warps))
      [open-points warps])))

(defn choose-exit [open-points]
  (let [exit-point (rand-nth (filter boundary? open-points))
        exit-ord (rand-nth (boundary-ords exit-point))
        open-points (disj open-points exit-point)]
    [open-points [exit-point exit-ord]]))

(defn choose-blocks [open-points board n]
  (loop [open-points open-points
         board board
         n n]
    (if (pos? n)
      (let [block-point (rand-nth (seq open-points))
            board (assoc-in board block-point {:type :block :fixed true})
            open-points (disj open-points block-point)]
        (recur open-points board (dec n)))
      [open-points board])))


;;; gameplay

(defn result?
  "Return :won if the game is won, :lost if lost, and nil otherwise."
  [{:keys [board exit goo-volume min-pipeline-length pipeline]}]
  (let [[_ overflow] (goo-distance board pipeline goo-volume)]
    (when overflow
      (let [[end-point [_ end-ord]] (peek pipeline)]
        (if (or (< (count pipeline) min-pipeline-length)
                (and exit (not= exit [end-point end-ord])))
          :lost
          :won)))))

(defn new-level [state level-num timestamp]
  (let [level (level-params level-num)
        open-points (set (for [y (range 7) x (range 10)] [y x]))
        [open-points start-point start-piece] (choose-start open-points)
        [open-points warps] (if (:has-warp level)
                              (choose-warps open-points 1)
                              [open-points {}])
        [open-points exit] (if (:has-exit level)
                             (choose-exit open-points)
                             [open-points nil])
        board (assoc-in empty-board start-point start-piece)
        [_ board] (choose-blocks open-points board (:num-blocks level))]
    (-> state
        (assoc :board board
               :start-point start-point
               :pipeline (trace-pipeline board warps start-point)
               :goo-volume 0
               :elapsed 0
               :level level-num
               :queue (randish-seq (shuffle (:queue-pieces level)))
               :fast-forward false
               :destroying nil
               :last-timestamp timestamp
               :exit exit
               :warps warps
               :game-state :playing)
        (merge level))))

(defn score-new-piece' [board pipeline goo-volume min-pipeline-length fast-forward]
  (let [gd (floor (first (goo-distance board pipeline goo-volume)))
        full-pipeline (take gd pipeline)
        base-score (if (>= gd min-pipeline-length) 100 50)
        [point _ _] (last full-pipeline)
        prev-pipeline (butlast full-pipeline)
        piece (get-in board point)
        self-cross (and (= (:type piece) :cross)
                        (some #{point} (map first prev-pipeline)))
        prev-self-crosses (- (count prev-pipeline) (count (distinct (map first prev-pipeline))))]
    (cond-> base-score
       self-cross (+ 500)
       (and self-cross (= prev-self-crosses 4)) (+ 10000)
       fast-forward (* 2))))

(defn score-new-piece
  "Return the score that should be added to the total as a result of the goo
  passing through the most recently filled piece."
  [{:keys [board fast-forward goo-volume min-pipeline-length pipeline]}]
  (score-new-piece' board pipeline goo-volume min-pipeline-length fast-forward))

(defn advance-time [{:keys [board elapsed fast-forward game-state goo-delay goo-speed goo-volume last-timestamp pipeline] :as state} timestamp]
  (if (= game-state :playing)
    (let [time-delta (/ (- timestamp last-timestamp) 1000)
          elapsed (+ elapsed time-delta)
          gv-delta (if (or fast-forward (> elapsed goo-delay)) (* time-delta goo-speed) 0)
          goo-volume' (+ goo-volume gv-delta)
          gd (floor (first (goo-distance board pipeline goo-volume)))
          gd' (floor (first (goo-distance board pipeline goo-volume')))]
      (as-> state s
        (assoc s :elapsed elapsed :goo-volume goo-volume' :last-timestamp timestamp)
        (if (> gd' gd)
          (update s :score (partial + (score-new-piece s)))
          s)
        (case (result? s)
          :won (assoc s :game-state :cleanup :won-last-level true)
          :lost (assoc s :game-state :cleanup :won-last-level false)
          s)))
    (assoc state :last-timestamp timestamp)))

(defn clickable? [{:keys [destroying game-state]}]
  (and (= game-state :playing) (not destroying)))

(defn toggle-pause [{:keys [game-state] :as state}]
  (case game-state
    :playing (assoc state :game-state :paused)
    :paused (assoc state :game-state :playing)
    state))

(defn enable-fast-forward [state]
  (assoc state
         :fast-forward true
         :goo-speed 3))

(defn handle-click! [[y x]]
  (let [{:keys [board goo-volume pipeline] :as state} @state-atom
        piece (get-in board [y x])]
    (when (and (clickable? state)
               (not (contains-goo? board pipeline goo-volume [y x]))
               (not (:fixed piece))
               (not= (:type piece) :start))
      (if (not= (:type piece) :empty)
        (do
          (swap! state-atom (fn [s] (-> s
                                        (assoc :destroying [y x])
                                        (update :score #(max 0 (- % 50))))))
          (.setTimeout js/window
                      (fn [] (swap! state-atom #(-> %
                                                    (assoc :destroying nil)
                                                    (add-piece [y x])))) 500))
        (swap! state-atom #(add-piece % [y x]))))))

(defn destroy-unused! []
  (a/go-loop [unused (unused-pieces @state-atom)]
    (when-let [[y x] (first unused)]
      (swap! state-atom (fn [s] (-> s
                                    (assoc :destroying [y x])
                                    (update :score #(max 0 (- % 50))))))
      (a/<! (a/timeout 200))
      (swap! state-atom (fn [s] (-> s
                                    (assoc-in [:board y x] {:type :empty})
                                    (assoc :destroying nil))))
      (recur (next unused)))))

(defn time-loop! [timestamp]
  (let [new-state (swap! state-atom #(advance-time % timestamp))]
    (if (= (:game-state new-state) :cleanup)
      (a/go
        (a/<! (destroy-unused!))
        (swap! state-atom #(assoc % :game-state :idle)))
      (.setTimeout
        js/window
        #(.requestAnimationFrame js/window time-loop!)
        30))))

(defn start-level! [level-num]
  (.requestAnimationFrame
    js/window
    (fn [t]
      (swap! state-atom #(new-level % level-num t))
      (time-loop! t))))

(defn new-game! []
  (swap! state-atom #(assoc % :score 0))
  (start-level! 0))

(defn next-level! []
  (start-level! (inc (:level @state-atom))))


;;; components

(defn piece-cp [piece]
  [:div.piece
   {:class (:type piece)}
   (svg/piece-svg piece)])

(defn goo-cp [piece inlet outlet pct]
   (svg/goo-svg piece inlet outlet pct))

(defn cell-cp [& {:keys [piece flows on-click exit-ord warp-ord destroying]}]
  [:div.cell {:class [(when exit-ord (str "exit-" (name exit-ord)))
                      (when warp-ord (str "warp-" (name warp-ord)))
                      (when destroying :destroying)]
              :on-click on-click}
   [piece-cp piece]
   (map-indexed (fn [i [inlet outlet pct]]
                  ^{:key i}
                  [goo-cp piece inlet outlet pct]) flows)])

(defn board-cp []
  (let [{:keys [board destroying exit game-state goo-volume pipeline warps]} @state-atom
        [h w] dimensions
        gd (first (goo-distance board pipeline goo-volume))
        warp-points (into (hash-map) (keys warps))
        fm (flow-map pipeline gd)]
    [:div.board
     (for [y (range h)]
       ^{:key y}
       [:div.row
        (for [x (range w)]
          (if (= game-state :paused)
            ^{:key x} [cell-cp]
            (let [click #(handle-click! [y x])
                  flows (fm [y x])
                  exit-ord (when (= (first exit) [y x]) (second exit))
                  warp-ord (warp-points [y x])]
              ^{:key x}
              [cell-cp
               :piece (get-in board [y x])
               :flows flows
               :on-click click
               :exit-ord exit-ord
               :warp-ord warp-ord
               :destroying (= destroying [y x])])))])]))

(defn level-cp []
  (let [{:keys [level]} @state-atom]
    [:div.level
     [:span (str "Level: " (inc level))]]))

(defn countdown-cp []
  (let [{:keys [board goo-volume min-pipeline-length pipeline]} @state-atom
        gd (floor (first (goo-distance board pipeline goo-volume)))]
    [:div.countdown
     [:span (str "Distance to Goal: " (max 0 (- min-pipeline-length gd)))]]))

(defn score-cp []
  (let [{:keys [score]} @state-atom]
    [:div.score
     [:span (str "Score: " score)]]))

(defn header-cp []
  [:div.header
   [level-cp]
   [countdown-cp]
   [score-cp]])

(defn button-cp []
  (let [{:keys [fast-forward game-state level won-last-level]} @state-atom]
    (case game-state
      :playing (if (not fast-forward)
                 [:button {:on-click #(swap! state-atom enable-fast-forward)} ">>"]
                 [:div.placeholder])
      :idle (if won-last-level
              [:button.play.won {:on-click next-level!} "Next Level"]
              [:button.play {:class (when level :lost)
                             :on-click new-game!} "New Game"])
      [:div.placeholder])))

(defn queue-cp []
  (let [{:keys [game-state queue]} @state-atom]
    [:div.row.queue
     (when (not= game-state :paused)
       (map-indexed (fn [i p]
                      ^{:key i}
                      [:div.cell (piece-cp p)]) (take 5 queue)))]))

(defn footer-cp []
  [:div.footer
   [button-cp]
   [queue-cp]])

(defn app-cp []
  [:div#container {:on-key-up (fn [e]
                                (case (.-keyCode e)
                                  113 (new-game!)
                                  115 (swap! state-atom toggle-pause)
                                  nil))
                   :tab-index 0}
   [header-cp]
   [board-cp]
   [:div.separator]
   [footer-cp]])


(rdom/render [app-cp] (.getElementById js/document "app"))
(.focus (.getElementById js/document "container"))
(reset! state-atom @state-atom)
