(ns pipes-in-space.core-test
  (:require
    [pipes-in-space.core :as pd]
    [cljs.test :refer-macros [deftest is testing]]))

(deftest test-neighbor-at-ord
  (is (= (pd/neighbor-at-ord [0 0] :s) [1 0]))
  (is (= (pd/neighbor-at-ord [5 2] :w) [5 1]))
  (is (= (pd/neighbor-at-ord [0 0] :n) nil)))

(deftest test-opp-point-ord
  (is (= (pd/opp-point-ord [[0 4] :n]) [[6 4] :s]))
  (is (= (pd/opp-point-ord [[0 4] :e]) [[0 5] :w])))

(deftest test-goo-distance
  (let [board (-> pd/empty-board
                  (assoc-in [2 2] {:type :start :direction :e})
                  (assoc-in [2 3] {:type :angle :corner :sw})
                  (assoc-in [3 3] {:type :reservoir :axis :v}))
        pipeline [[[2 2] [:start :e]] [[2 3] [:w :s]] [[3 3] [:n :s]]]]
    (is (= (pd/goo-distance board pipeline 0) [0 false]))
    (is (= (pd/goo-distance board pipeline 0.6) [0.6 false]))
    (is (= (pd/goo-distance board pipeline 1) [1 false]))
    (is (= (pd/goo-distance board pipeline 1.2) [1.2 false]))
    (is (= (pd/goo-distance board pipeline 2) [2 false]))
    (is (= (pd/goo-distance board pipeline 2.5) [2.25 false]))
    (is (= (pd/goo-distance board pipeline 4) [3 false]))
    (is (= (pd/goo-distance board pipeline 5) [3 true]))))

(deftest test-contains-goo?
  (let [board (-> pd/empty-board
                  (assoc-in [2 2] {:type :start :direction :e})
                  (assoc-in [2 3] {:type :angle :corner :sw})
                  (assoc-in [3 3] {:type :reservoir :axis :v}))
        pipeline [[[2 2] [:start :e]] [[2 3] [:w :s]] [[3 3] [:n :s]]]]
    (is (false? (pd/contains-goo? board pipeline 0 [2 2])))
    (is (true? (pd/contains-goo? board pipeline 0.1 [2 2])))
    (is (false? (pd/contains-goo? board pipeline 0.1 [2 3])))
    (is (false? (pd/contains-goo? board pipeline 1 [2 3])))))

(deftest test-trace-pipeline
  (testing "just the start"
    (let [board (assoc-in pd/empty-board [2 2] {:type :start :direction :e})]
      (is (= (pd/trace-pipeline board {} [2 2]) [[[2 2] [:start :e]]]))))
  (testing "start, then bad piece"
    (let [board (-> pd/empty-board
                    (assoc-in [2 2] {:type :start :direction :e})
                    (assoc-in [2 3] {:type :straight :axis :v}))]
      (is (= (pd/trace-pipeline board {} [2 2]) [[[2 2] [:start :e]]]))))
  (testing "start, then good piece"
    (let [board (-> pd/empty-board
                    (assoc-in [2 2] {:type :start :direction :e})
                    (assoc-in [2 3] {:type :angle :corner :sw}))]
      (is (= (pd/trace-pipeline board {} [2 2]) [[[2 2] [:start :e]]
                                                 [[2 3] [:w :s]]]))))
  (testing "self-cross"
    (let [board (-> pd/empty-board
                    (assoc-in [2 2] {:type :start :direction :e})
                    (assoc-in [2 3] {:type :cross})
                    (assoc-in [2 4] {:type :angle :corner :nw})
                    (assoc-in [1 4] {:type :angle :corner :sw})
                    (assoc-in [1 3] {:type :angle :corner :se})
                    (assoc-in [3 3] {:type :straight :axis :v}))]
      (is (= (pd/trace-pipeline board {} [2 2]) [[[2 2] [:start :e]]
                                                 [[2 3] [:w :e]]
                                                 [[2 4] [:w :n]]
                                                 [[1 4] [:s :w]]
                                                 [[1 3] [:e :s]]
                                                 [[2 3] [:n :s]]
                                                 [[3 3] [:n :s]]]))))
  (testing "warp"
    (let [board (-> pd/empty-board
                    (assoc-in [2 2] {:type :start :direction :n})
                    (assoc-in [1 2] {:type :cross})
                    (assoc-in [0 2] {:type :straight :axis :v})
                    (assoc-in [6 2] {:type :angle :corner :sw})
                    (assoc-in [6 1] {:type :straight :axis :h}))]
      (is (= (pd/trace-pipeline board {[[0 2] :n] [[6 2] :s]} [2 2]) [[[2 2] [:start :n]]
                                                                      [[1 2] [:s :n]]
                                                                      [[0 2] [:s :n]]
                                                                      [[6 2] [:s :w]]
                                                                      [[6 1] [:e :w]]])))))

(deftest test-score-new-piece'
  (let [board (-> pd/empty-board
                  (assoc-in [1 0] {:type :start :direction :e})
                  (assoc-in [1 1] {:type :cross})
                  (assoc-in [1 2] {:type :cross})
                  (assoc-in [1 3] {:type :cross})
                  (assoc-in [1 4] {:type :cross})
                  (assoc-in [1 5] {:type :cross})
                  (assoc-in [1 6] {:type :angle :corner :nw})
                  (assoc-in [0 6] {:type :angle :corner :sw})
                  (assoc-in [0 5] {:type :angle :corner :se})
                  (assoc-in [2 5] {:type :angle :corner :nw})
                  (assoc-in [2 4] {:type :angle :corner :ne})
                  (assoc-in [0 4] {:type :angle :corner :sw})
                  (assoc-in [0 3] {:type :angle :corner :se})
                  (assoc-in [2 3] {:type :angle :corner :nw})
                  (assoc-in [2 2] {:type :angle :corner :ne})
                  (assoc-in [0 2] {:type :angle :corner :sw})
                  (assoc-in [0 1] {:type :angle :corner :se})
                  (assoc-in [2 1] {:type :straight :axis :v}))
        pipeline (pd/trace-pipeline board {} [1 0])]
    (testing "normal piece"
      (is (= (pd/score-new-piece' board pipeline 1 15 false) 50)))
    (testing "fast-forward"
      (is (= (pd/score-new-piece' board pipeline 1 15 true) 100)))
    (testing "normal piece after min-pipeline-length has been met"
      (is (= (pd/score-new-piece' board pipeline 3 2 false) 100)))
    (testing "fast forward after min-pipeline-length has been met"
      (is (= (pd/score-new-piece' board pipeline 3 2 true) 200)))
    (testing "self-cross"
      (is (= (pd/score-new-piece' board pipeline 10 15 false) 550)))
    (testing "5 self-crosses"
      (is (= (pd/score-new-piece' board pipeline 22 50 false) 10550)))))
