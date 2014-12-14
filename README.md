# Kormafy

Transform SQL to [Korma](https://github.com/korma/Korma) DSL

## Usage

Don't.

Seriously, why would you programmatically first transform SQL to Clojure DSL and then back to SQL?

If you still insist:

```clojure
(require '[kormafy.core :refer [sql->korma]])

(sql->korma "SELECT foo FROM bar")
=> (select :bar (fields :foo))
```

Currently supported SQL constructs can be found from the
[tests](https://github.com/immoh/kormafy/blob/master/test/kormafy/core_test.clj).

## Why?

Because it can be done.

Because I wanted to to play with [Instaparse](https://github.com/Engelberg/instaparse) and
[test.check](https://github.com/clojure/test.check).
