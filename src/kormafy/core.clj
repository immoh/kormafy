(ns kormafy.core
  (:require [clojure.string]
            [instaparse.core :as insta]))

(def sql-parser (insta/parser "<sql>        = select <whitespace> from
                               whitespace   = #'\\s+'
                               select       = <'select'> <whitespace> [modifier <whitespace>] columns
                               modifier     = #'(?i)distinct'
                               <columns>    = column (<whitespace>? <','> <whitespace>? column)*
                               column       = column-name [alias]
                               <column-name>  = [identifier <'.'>] (identifier | '*')
                               alias        = <whitespace> <'as'> <whitespace> identifier
                               from         = <'from'> <whitespace> table
                               table        = identifier [alias]
                               <identifier> = #'[A-Za-z][A-Za-z0-9]*'"
                              :string-ci true))

(defn- sql-map->korma [{:keys [from fields modifier]}]
  (list* 'select from (filter identity [(list* 'fields fields)
                                        (when modifier (list 'modifier modifier))])))

(defn- parse-tag [tag v]
  (when (vector? v)
    (let [[tag' value] v]
      (when (= tag tag')
        value))))

(defmulti ^:private transform-sql-node first)

(defmethod transform-sql-node :select [[_ & columns]]
  (let [modifier (parse-tag :modifier (first columns))
        columns (if modifier (rest columns) columns)]
    {:modifier modifier
     :fields   (map transform-sql-node columns)}))

(defmethod transform-sql-node :from [[_ table]]
  {:from (transform-sql-node table)})

(defn- aliasable [parts]
  (let [alias (keyword (parse-tag :alias (last parts)))
        field (keyword (clojure.string/join "." (if alias (butlast parts) parts)))]
    (if alias
      [field alias]
      field)))

(defmethod transform-sql-node :table [[_ & parts]]
  (aliasable parts))

(defmethod transform-sql-node :column [[_ & parts]]
  (aliasable parts))

(defn sql->korma [sql-string]
  (let [parsed (sql-parser sql-string)]
    (if (insta/failure? parsed)
      parsed
      (sql-map->korma (apply merge (map transform-sql-node parsed))))))
