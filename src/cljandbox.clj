(ns ^{:author "Daniel May (MayDaniel)"
      :doc    "A collection of Clojure utilities."}
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
  (if (odd? (count clauses))
    (throw (IllegalArgumentException. "do-when requires an even number of clauses."))
    (cons 'do (map (partial cons 'when)
                   (partition 2 clauses)))))

(defmacro cond-pred
  "(cond-pred 10
     string? \"String!\"
     vector? \"Vector!\"
             \"Default!\")
  ;; => \"Default\""
  [x & clauses]
  (letfn [(? [pred x] (pred x))]
    `(condp ~? ~x ~@clauses)))

(defn partialr
  "((partial / 5.0) 2)  ;; => 0.2
   ((partial / 5 5) 10) ;; => 0.4"
  [f & r-args]
  (fn [& l-args] (apply f (concat l-args r-args))))

(defn id<-
  "Returns a lazy sequence of the items in coll that are boolean true."
  [coll]
  (filter identity coll))

(defn all-of?
  "((all-of? :a :b) {:a 1 :b 2}) ;; => true
   ((all-of? :a :b) {:a 3 :c 4}) ;; => false"
  [& preds]
  (fn [x] (every? #(% x) preds)))

(defn any-of?
  "((any-of? integer? string?) 5)      ;; => true
   ((any-of integer? string?) [1 2 3]) ;; => false"
  [& preds]
  (fn [x] (boolean (some #(% x) preds))))

(defn none-of?
  "((none-of? map? vector?) '(1 2 3)) ;; => true
   ((none-of? map? vector?) [1 2 3])  ;; => false"
  [& preds]
  (fn [x] (not-any? #(% x) preds)))

(defn dup-in
  "(take 7 (dup-in 2 (range 1000)))
   ;; => (0 0 1 1 2 2 3)"
  [n coll]
  (mapcat (partial repeat n) coll))

(defn tails
  "A lazy sequence of the tails of the collection. (seq coll) inclusive.
   (tails [1 2 3]) ;; => ((1 2 3) (2 3) (3))"
  [coll]
  (->> (seq coll)
       (iterate next)
       (take-while seq)))
