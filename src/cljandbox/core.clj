(ns cljandbox.core
  (:require fn)
  (:use [clojure.walk :only [postwalk-replace]]
        [clojure.contrib.def :only [defalias]]))


;; http://github.com/zahardzhan/fn

(defalias fn-and fn/and)
(defalias fn-or fn/or)
(defalias fn-not fn/not)

(defn <- [coll]
  (filter identity coll))

(defmacro ->_
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta `(~@(postwalk-replace {'_ x} form)) (meta form))
              (list form x)))
  ([x form & more] `(->_ (->_ ~x ~form) ~@more)))
