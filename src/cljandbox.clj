(ns ^{:author "Daniel May (MayDaniel)", :doc "A collection of Clojure utilities."}
  cljandbox
  (:use [clojure.walk :only [postwalk-replace]]
        [clojure.contrib.def :only [defnk]]))

(defn compose-and
  "(compose-and f g ...)
   => (fn [& xs] (and (apply f xs) (apply g xs) ... true))

  ((compose-and coll? empty?) []) yielding true."
  [& predicates]
  (fn [& xs] (every? #(apply % xs) predicates)))

(defn compose-or
  "(compose-or f g ...)
   => (fn [& xs] (boolean (or (apply f xs) (apply g xs) ...)))

  ((compose-or number? string?) 2.5) yielding true."
  [& predicates]
  (fn [& xs] (boolean (some #(apply % xs) predicates))))

(defn compose-not
  "(compose-not f g ...)
   => (fn [& xs] (and (apply (complement f) xs) (apply (complement g) xs) ...))

  ((compose-not integer? string?) {}) yielding true."
  [& predicates]
  (fn [& xs] (apply (apply compose-and (map complement predicates)) xs)))

(defn <-
  "Returns a lazy sequence of the items in coll that are boolean true."
  [coll]
  (filter identity coll))

(defmacro defunk
  "Similar to clojure.contrib.def/defnk, but accepts :arglists meta-data."
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
