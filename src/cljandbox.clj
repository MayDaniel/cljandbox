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

(defmacro defpartial
  "Defines a function that accepts a fixed number of arguments, and
   automagically partially applies its arguments if too few are passed.
   (defpartial foo [{:keys [a b]} c] (+ a b c))
   (foo {:a 1 :b 2} 3)   ;; => 6
   ((foo {:a 1 :b 2}) 3) ;; => 6"
  [name args & body]
  `(defn ~name {:arglists (quote ([~@args]))} [~'& ~'args]
    (let [count#  ~(count args)
          actual#  (count ~'args)]
      (cond (> actual# count#) ;; too many arguments
            (throw (IllegalArgumentException.))
            (< actual# count#) ;; too few arguments
            (apply partial ~name ~'args)
            (= count# actual#) ;; correct number of arguments
            (let [[~@args] ~'args] ~@body)))))

(defmacro do-when
  "Executes the then form when test is truthy. For use with side-effects."
  {:arglists '([test then & more])}
  [& clauses]
  (assert (even? (count clauses)))
  (cons 'do (map (partial cons 'when)
                 (partition 2 clauses))))

(defmacro cond-pred
  "(cond-pred user-map
    (comp not :valid?) \"Invalid user.\"
    (comp not :in)     \"User is logged out.\")"
  [x & clauses]
  (assert (even? (count clauses)))
  (when (not-empty clauses)
    (let [[pred then & more] clauses]
      `(if (~pred ~x) ~then (cond-pred ~x ~@more)))))

(defmacro check-let
  "(check-let [username (re-find #\"$[a-z]+^\" name) \"Invalid username.\"
               password (re-find #\"$[a-z]+^\" pass) \"Invalid password.\"]
     (register-user username password))"
  [bindings & body]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  (if (not-empty bindings)
    (let [[binding [else & more]] (split-at 2 bindings)]
      `(if-let [~@binding] (check-let [~@more] ~@body) ~else))
    `(do ~@body)))

(defn partialr
  "((partialr / 10) 2)  ;; => 0.2
   ((partialr / 5 5) 10) ;; => 0.4"
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

(defn starts-with?
  "Returns true if a starts with b, otherwise false.
   (starts-with? [1 2 3] [1 2 3 4]) ;; => true
   (starts-with? \"foo\" \"fo\")    ;; => true"
  [a b]
  (every? true? (map = a b)))

(defn tails
  "A lazy sequence of the tails of the collection. (seq coll) inclusive.
   (tails [1 2 3]) ;; => ((1 2 3) (2 3) (3))"
  [coll]
  (->> (seq coll)
       (iterate next)
       (take-while seq)))

(defn heads
  "A lazy sequence of the heads of the collection.
   (heads [1 2 3]) ;; => ([1] [1 2] [1 2 3])"
  [coll]
  (rest (reductions conj [] coll)))

(defn take-until
  "(take-until #{3} (range 6))
   ;; => (0 1 2)"
  [pred coll]
  (take-while (complement pred) coll))

(defn drop-until
  "(drop-until #{3} (range 6))
   ;; => (3 4 5)"
  [pred coll]
  (drop-while (complement pred) coll))

(defn mapmap
  "(mapmap (partial * 10) [1 2 3 4]) ;; => {1 10, 2 20, 3 30, 4 40}"
  [f coll]
  (persistent!
   (reduce #(assoc! %1 %2 (f %2)) (transient {}) coll)))

(defn mapcachemap
  "Similar to mapmap, but checks if (f ele) has already been executed,
   running and associating (f ele) only if it hasn't."
  [f coll]
  (reduce #(if (contains? %1 %2) %1 (assoc %1 %2 (f %2))) {} coll))

(defn mapmultimap
  "Similar to mapmap, but allows multiple collections, throwing f's
   arguments into a vector.
   (mapmultimap + [1 2 3] [4 5 6]) ;; => {[1 4] 5, [2 5] 7, [3 6] 9}"
  [f & colls]
  (into {} (apply map (juxt vector f) colls)))
