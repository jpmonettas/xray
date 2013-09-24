(ns xray.core-test
  (:require [clojure.test :refer :all]
            [xray.core :refer :all]))

(xray println
      (defn defn-test [[a b] j] 5))
(defn-test [5 6] 7)

(xray println
      (defn let-test []
        (let [j 40
              ?w 30]
          (+ j w))))

(let-test)

(xray println
      (defn if-test [a b c]
          (if (= a b)
            5)))

(if-test 11 5 6)

(xray println
      (defn cond-test [a b c]
        (cond (= a 4) 6
              (= ?a 3) 8
              :else 9)))

(cond-test 3 4 5)

