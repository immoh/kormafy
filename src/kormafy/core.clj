(ns kormafy.core
  (:require [clojure.string]
            [instaparse.core :as insta]))

(def sql-parser (insta/parser "<sql>        = select <whitespace> from
                               whitespace   = #'\\s+'
                               select       = <'select'> <whitespace> columns
                               <columns>    = column (<whitespace>? <','> <whitespace>? column)*
                               column       = column-name [alias]
                               <column-name>  = [identifier <'.'>] (identifier | '*')
                               alias        = <whitespace> <'as'> <whitespace> identifier
                               from         = <'from'> <whitespace> identifier
                               <identifier> = #'[A-Za-z][A-Za-z0-9]*'"
                              :string-ci true))

(defn- sql-map->korma [{:keys [from fields]}]
  (list 'select from (list* 'fields fields)))

(defmulti ^:private transform-sql-node first)

(defmethod transform-sql-node :select [[_ & columns]]
  {:fields (map transform-sql-node columns)})

(defmethod transform-sql-node :from [[_ table]]
  {:from (keyword table)})

(defn- parse-alias [v]
  (when (vector? v)
    (let [[tag value] v]
      (when (= :alias tag)
        value))))

(defmethod transform-sql-node :column [[_ & parts]]
  (let [alias (keyword (parse-alias (last parts)))
        field (keyword (clojure.string/join "." (if alias (butlast parts) parts)))]
    (if alias
      [field alias]
      field)))

(defn sql->korma [sql-string]
  (let [parsed (sql-parser sql-string)]
    (if (insta/failure? parsed)
      parsed
      (sql-map->korma (apply merge (map transform-sql-node parsed))))))
