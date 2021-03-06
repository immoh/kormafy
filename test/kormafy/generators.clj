(ns kormafy.generators
  (:refer-clojure :exclude [group-by])
  (:require [clojure.string]
            [clojure.test.check.generators :as gen]))

(def identifier (gen/fmap (fn [[a b]] (clojure.string/join (cons a b)))
                              (gen/tuple gen/char-alpha (gen/vector gen/char-alpha-numeric))))

(defn prefixed-identifier [prefix]
  (gen/fmap (partial str prefix ".") identifier))

(defn aliasable
  ([g] (aliasable nil g))
  ([prefix g]
    (gen/fmap (fn [[name alias]]
                {:name (if prefix (str prefix "." name) name)
                 :alias alias})
              (gen/tuple g (gen/frequency [[1 (gen/return nil)] [1 identifier]])))))

(defn function [table]
  (gen/fmap (fn [[fn-name args]]
              (format "%s(%s)" (clojure.string/upper-case fn-name) (clojure.string/join ", " args)))
            (gen/tuple identifier (gen/vector (prefixed-identifier table)))))

(defn columns [table]
  (gen/frequency [[9 (gen/not-empty (gen/vector (gen/one-of [(aliasable table identifier) (aliasable (function table))])))]
                  [1 (gen/return [{:name (str table ".*")}])]]))

(def modifier (gen/frequency [[1 (gen/return "DISTINCT")] [9 (gen/return nil)]]))

(defn order-by [table]
  (gen/vector (gen/tuple (prefixed-identifier table) (gen/elements ["ASC" "DESC"]))))

(defn group-by [table]
  (gen/vector (prefixed-identifier table)))

(def nil-or-pos-int (gen/one-of [gen/pos-int (gen/return nil)]))

(defn where [table]
  (gen/frequency [[1 (gen/return nil)] [9 (prefixed-identifier table)]]))


(defn format-columns [modifier columns]
  (str
    (when modifier (str modifier " "))
    (clojure.string/join ", " (map (fn [{:keys [name alias]}]
                                     (str
                                       name
                                       (when alias (str " AS " alias))))
                                   columns))))

(defn format-table [{:keys [name alias]}]
  (if alias
    (str name " AS " alias)
    name))

(defn format-order-by [order-by-columns]
  (clojure.string/join ", " (map (fn [[column dir]] (format "%s %s" column dir)) order-by-columns)))

(def sql (gen/fmap (fn [[table modifier columns where group-by order-by limit offset]]
                     (str
                       (format "SELECT %s FROM %s" (format-columns modifier columns) (format-table table))
                       (when where (str " WHERE " where " = ?"))
                       (when (seq group-by) (str " GROUP BY " (clojure.string/join ", " group-by)))
                       (when (seq order-by) (str " ORDER BY " (format-order-by order-by)))
                       (when limit (str " LIMIT " limit))
                       (when offset (str " OFFSET " offset))))
                   (gen/bind (aliasable identifier)
                             (fn [{:keys [name alias] :as table}]
                               (let [prefix (or alias name)]
                                 (gen/tuple (gen/return table) modifier (columns prefix) (where prefix) (group-by prefix) (order-by prefix) nil-or-pos-int nil-or-pos-int))))))
