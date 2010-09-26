(ns ^{:author "Daniel May (MayDaniel)"
      :doc "A collection of Clojure utilities."}
  cljandbox
  (:use [clojure.walk        :only [postwalk-replace]]
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

(defmacro do-when
  "Executes the then form when test is truthy. For use with side-effects."
  {:arglists '([test then & more])}
  [& clauses]
  (if-not (even? (count clauses))
    (throw (IllegalArgumentException. "do-when requires an even number of clauses."))
    (cons 'do (map (partial 'when) (partition 2 clauses)))))

(defn <-
  "Returns a lazy sequence of the items in coll that are boolean true."
  [coll]
  (filter identity coll))

(defn all-of?
  "((all-of? :a :b) {:a 1 :b 2}) ;; => true

   ((all-of? :a :b) {:a 3 :c 4}) ;; => false"
  [& predicates]
  (fn [& xs] (every? #(apply % xs) predicates)))

(defn any-of?
  "((any-of? integer? string?) 5) ;; => true

   ((any-of integer? string?) [1 2 3]) ;; => false"
  [& predicates]
  (fn [& xs] (boolean (some #(apply % xs) predicates))))

(defn none-of?
  "((none-of? map? vector?) '(1 2 3)) ;; => true

   ((none-of? map? vector?) [1 2 3]) ;; => false"
  [& predicates]
  (fn [& xs] (not-any? #(apply % xs) predicates)))
