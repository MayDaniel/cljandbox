(ns ^{:author "Daniel May (MayDaniel)", :doc "A collection of Clojure utilities."}
  cljandbox
  (:require fn)
  (:use [clojure.walk :only [postwalk-replace]]
        [clojure.contrib.def :only [defalias defnk]]))

;; http://github.com/zahardzhan/fn

(defalias fn-and fn/and)
(defalias fn-or fn/or)
(defalias fn-not fn/not)

(defn <-
  "Returns a lazy sequence of the items in coll that are boolean true."
  [coll]
  (filter identity coll))

(defmacro defunk
  "Similar to clojure.contrib.def/defnk, but accepts :arglists meta data."
  [fn-name & fn-tail]
  (let [arglists (some :arglists fn-tail)]
    `(do (defnk ~fn-name ~@fn-tail)
         (when ~arglists (alter-meta! (var ~fn-name) assoc :arglists ~arglists))
         (var ~fn-name))))

(defmacro ->_
  "Threads the forms, replacing underscores with the result of the last expression."
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta (postwalk-replace {'_ x} form) (meta form))
              (list form x)))
  ([x form & more] `(->_ (->_ ~x ~form) ~@more)))
