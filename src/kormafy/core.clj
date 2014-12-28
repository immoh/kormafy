(ns kormafy.core
  (:require [clojure.string]
            [instaparse.core :as insta]))

(def sql-parser (insta/parser "<sql>            = select <whitespace>
                                                  from
                                                  [<whitespace> where]
                                                  [<whitespace> group-by]
                                                  [<whitespace> order-by]
                                                  [<whitespace> limit]
                                                  [<whitespace> offset]
                               whitespace       = #'\\s+'
                               separator        = <whitespace>? <','> <whitespace>?
                               select           = <'select'> <whitespace> [modifier <whitespace>] select-fields
                               modifier         = #'(?i)distinct'
                               <select-fields>  = select-field (<separator> select-field)*
                               select-field     = (column [alias]) | (function [alias]) | all-columns
                               all-columns      = (identifier <'.'> <'*'>) | <'*'>
                               column           = [identifier <'.'>] identifier
                               alias            = <whitespace> <'as'> <whitespace> identifier
                               select-function  = function [alias]
                               function         = identifier <'('> function-arg? (<separator> function-arg)* <')'>
                               <function-arg>   = column | function | all-columns
                               from             = <'from'> <whitespace> table
                               table            = identifier [alias]
                               <identifier>     = #'[A-Za-z][A-Za-z0-9]*'
                               order-by         = <'order by'> <whitespace> order-columns
                               <order-columns>  = order-column (<separator> order-column)*
                               order-column     = column [<whitespace> order-dir]
                               <order-dir>      = 'ASC' | 'DESC'
                               limit            = <'limit'> <whitespace> number
                               offset           = <'offset'> <whitespace> number
                               <number>         = #'[0-9]+'
                               group-by         = <'group by'> <whitespace> column (<separator> column)*
                               where            = <'where'> <whitespace> condition
                               condition        = column <whitespace> operator <whitespace> value
                               operator         = '='
                               value            = '?'"
                              :string-ci true))

(defn- sql-map->korma [{:keys [from fields modifier where group-by order-by limit offset]}]
  (list* 'select from (filter identity (concat [(list* 'fields fields)
                                                (when modifier (list 'modifier modifier))
                                                (when where (list 'where where))
                                                (when group-by (list* 'group group-by))]
                                               (when order-by (map (partial list* 'order) order-by))
                                               [(when limit (list 'limit limit))
                                                (when offset (list 'offset offset))]))))

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

(defmethod transform-sql-node :table [[_ name alias]]
  (if alias
    [(keyword name) (transform-sql-node alias)]
    (keyword name)))

(defmethod transform-sql-node :select-field [[_ field alias]]
  (if alias
    [(transform-sql-node field) (transform-sql-node alias)]
    (transform-sql-node field)))

(defmethod transform-sql-node :column [[_ & parts]]
  (keyword (clojure.string/join "." parts)))

(defmethod transform-sql-node :alias [[_ alias]]
  (keyword alias))

(defmethod transform-sql-node :function [[_ name & args]]
  (list* 'sqlfn (symbol name) (map transform-sql-node args)))

(defmethod transform-sql-node :all-columns [[_ & parts]]
  (keyword (clojure.string/join "." (concat parts ["*"]))))

(defmethod transform-sql-node :order-by [[_ & columns]]
  {:order-by (map transform-sql-node columns)})

(defmethod transform-sql-node :order-column [[_ column dir]]
  [(transform-sql-node column) (or (keyword dir) :ASC)])

(defmethod transform-sql-node :limit [[_ n]]
  {:limit (Integer/parseInt n)})

(defmethod transform-sql-node :offset [[_ n]]
  {:offset (Integer/parseInt n)})

(defmethod transform-sql-node :group-by [[_ & columns]]
  {:group-by (map transform-sql-node columns)})

(defmethod transform-sql-node :where [[_ condition]]
  {:where (transform-sql-node condition)})

(defmethod transform-sql-node :condition [[_ x op y]]
  (list* (map transform-sql-node [op x y])))

(def ^:private op->clj {"=" '=})

(defmethod transform-sql-node :operator [[_ op]]
  (op->clj op))

(defmethod transform-sql-node :value [_]
  "?")

(defn sql->korma [sql-string]
  (let [parsed (sql-parser sql-string)]
    (if (insta/failure? parsed)
      parsed
      (sql-map->korma (apply merge (map transform-sql-node parsed))))))
