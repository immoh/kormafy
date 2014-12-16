(ns kormafy.core
  (:require [clojure.string]
            [instaparse.core :as insta]))

(def sql-parser (insta/parser "<sql>           = select <whitespace> from [<whitespace> order-by]
                               whitespace      = #'\\s+'
                               select          = <'select'> <whitespace> [modifier <whitespace>] columns
                               modifier        = #'(?i)distinct'
                               <columns>       = column (<whitespace>? <','> <whitespace>? column)*
                               column          = column-name [alias]
                               <column-name>   = [identifier <'.'>] (identifier | '*')
                               alias           = <whitespace> <'as'> <whitespace> identifier
                               from            = <'from'> <whitespace> table
                               table           = identifier [alias]
                               <identifier>    = #'[A-Za-z][A-Za-z0-9]*'
                               order-by        = <'order by'> <whitespace> order-columns
                               <order-columns> = order-column (<whitespace>? <','> <whitespace>? order-column)*
                               order-column    = column-name [<whitespace> order-dir]
                               order-dir       = 'ASC' | 'DESC'"
                              :string-ci true))

(defn- sql-map->korma [{:keys [from fields modifier order-by]}]
  (list* 'select from (filter identity (concat [(list* 'fields fields)
                                                (when modifier (list 'modifier modifier))]
                                               (when order-by (map (partial list* 'order) order-by))))))

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

(defmethod transform-sql-node :order-by [[_ & columns]]
  {:order-by (map transform-sql-node columns)})

(defmethod transform-sql-node :order-column [[_ & parts]]
  (let [dir (parse-tag :order-dir (last parts))
        parts (if dir (butlast parts) parts)]
    [(keyword (clojure.string/join "." parts))
     (or (keyword dir) :ASC)]))

(defn sql->korma [sql-string]
  (let [parsed (sql-parser sql-string)]
    (if (insta/failure? parsed)
      parsed
      (sql-map->korma (apply merge (map transform-sql-node parsed))))))
