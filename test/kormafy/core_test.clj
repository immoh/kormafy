(ns kormafy.core-test
  (:require [kormafy.core :refer :all]
            [kormafy.generators :as gen]
            [korma.core :refer :all]
            [korma.config]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]))

(use-fixtures :once (fn [t]
                      (require '[korma.core :refer :all])
                      (korma.config/set-delimiters "")
                      (t)))

(deftest korma-dsl-generation
  (testing "select * from table"
    (is (= '(select :foo (fields :*))
           (sql->korma "select * from foo"))))
  (testing "case-insensitivity"
    (is (= '(select :DUAL (fields :*))
           (sql->korma "SELECT * FROM DUAL"))))
  (testing "qualified *"
    (is (= '(select :foo (fields :foo.*))
           (sql->korma "select foo.* from foo"))))
  (testing "single column"
    (is (= '(select :foo (fields :bar))
           (sql->korma "select bar from foo"))))
  (testing "single qualified column"
    (is (= '(select :foo (fields :foo.bar))
           (sql->korma "select foo.bar from foo"))))
  (testing "multiple columns"
    (is (= '(select :foo (fields :a :b :c))
           (sql->korma "select a,b,c from foo"))))
  (testing "column alias"
    (is (= '(select :foo (fields [:bar :b]))
           (sql->korma "select bar as b from foo"))))
  (testing "table alias"
    (is (= '(select [:foo :f] (fields :f.a))
           (sql->korma "select f.a from foo as f"))))
  (testing "distinct"
    (is (= '(select :foo (fields :a) (modifier "distinct"))
           (sql->korma "select distinct a from foo"))))
  (testing "order by"
    (is (= '(select :foo (fields :*) (order :a :ASC) (order :b :DESC))
           (sql->korma "select * from foo order by a, b desc")))))

(defspec generated-dsl-generates-same-sql 100
  (prop/for-all [sql gen/sql]
    (= sql
       (sql-only (eval (sql->korma sql))))))
