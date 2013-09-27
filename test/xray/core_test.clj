(ns xray.core-test
  (:require [clojure.test :refer :all]
            [xray.core :refer :all]))

(xray println
      (defn defn-test [[?a b] j] a))
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


(xray println
      (defn test [?a b]
        (let [w 5
              ?x (+ w a)]
          (if (> ?x ?b)
            5
            x))))

(test 4 7)



(defn max-test [a b]
  (if (> a b)
    a
    b))


(defn max-test [a b]
  (if (> a b)
    (do
      (println "(> a b) was TRUE")
      a)
    (do
      (println "(> a b) was FALSE")
      b)))

(if some-test
    (do


(defmacro if? [test then-form else-form]
  `(if ~test
     (do 
       (print (str (quote ~test) "was TRUE"))
       ~then-form)
     (do 
       (print (str (quote ~test) "was FALSE"))
       ~else-form)))


(defn test [a b]
  (let [w 5
        x (+ w a)]
    (if (> x b)
      5
      x)))
