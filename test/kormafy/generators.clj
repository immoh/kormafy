(ns kormafy.generators
  (:require [clojure.string]
            [clojure.test.check.generators :as gen]))

(def identifier (gen/fmap (fn [[a b]] (clojure.string/join (cons a b)))
                              (gen/tuple gen/char-alpha (gen/vector gen/char-alpha-numeric))))

(def aliasable (gen/fmap (fn [[name alias]] {:name name :alias alias})
                         (gen/tuple identifier (gen/frequency [[1 (gen/return nil)] [1 identifier]]))))

(def columns (gen/frequency [[9 (gen/not-empty (gen/vector aliasable))] [1 (gen/return [{:name "*"}])]]))

(def modifier (gen/frequency [[1 (gen/return "DISTINCT")] [9 (gen/return nil)]]))

(defn format-columns [{table-name :name table-alias :alias} modifier columns]
  (str
    (when modifier (str modifier " "))
    (clojure.string/join ", " (map (fn [{:keys [name alias]}]
                                     (str
                                       (if table-alias table-alias table-name) "." name
                                       (when alias (str " AS " alias))))
                                   columns))))

(defn format-table [{:keys [name alias]}]
  (if alias
    (str name " AS " alias)
    name))

(def sql (gen/fmap (fn [[table modifier columns]]
                     (format "SELECT %s FROM %s" (format-columns table modifier columns) (format-table table)))
                   (gen/tuple aliasable modifier columns)))
