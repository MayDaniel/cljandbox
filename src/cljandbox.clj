(ns ^{:author "Daniel May (MayDaniel)", :doc "A collection of Clojure utilities."}
  cljandbox
  (:use [clojure.walk :only [postwalk-replace]]
        [clojure.contrib.def :only [defnk]]))

(defmacro ->_
  "Threads the forms, replacing underscores with the result of the last expression."
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta (postwalk-replace {'_ x} form) (meta form))
              (list form x)))
  ([x form & more] `(->_ (->_ ~x ~form) ~@more)))

(defmacro defsource
  "Similar to clojure.core/defn, but saves the function's definition in the var's
   :source metadata."
  {:arglists (:arglists (meta (var defn)))}
  [fn-name & defn-stuff]
  `(do (defn ~fn-name ~@defn-stuff)
       (alter-meta! (var ~fn-name) assoc :source (quote ~&form))
       (var ~fn-name)))

(defmacro defunk
  "Similar to clojure.contrib.def/defnk, but accepts :arglists metadata."
  [fn-name & fn-tail]
  (let [arglists (some :arglists fn-tail)]
    `(do (defnk ~fn-name ~@fn-tail)
         (when ~arglists (alter-meta! (var ~fn-name) assoc :arglists ~arglists))
         (var ~fn-name))))

(defn <-
  "Returns a lazy sequence of the items in coll that are boolean true."
  [coll]
  (filter identity coll))

(defn of-all?
  "(of-all? f g ...)
   => (fn [& xs] (and (apply f xs) (apply g xs) ... true))

  ((of-all? coll? empty?) []) will yield true."
  [& predicates]
  (fn [& xs] (every? #(apply % xs) predicates)))

(defn of-any?
  "(of-any? f g ...)
   => (fn [& xs] (boolean (or (apply f xs) (apply g xs) ...)))

  ((of-any? number? string?) 2.5) will yield true."
  [& predicates]
  (fn [& xs] (boolean (some #(apply % xs) predicates))))

(defn of-none?
  "(of-none? f g ...)
   => (fn [& xs] (and (apply (complement f) xs) (apply (complement g) xs) ...))

  ((of-none? integer? string?) {}) will yield true."
  [& predicates]
  (fn [& xs] (not-any? #(apply % xs) predicates)))
