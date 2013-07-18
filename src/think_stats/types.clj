(ns think-stats.types)

(derive clojure.lang.PersistentTreeMap :types/map)
(derive clojure.lang.PersistentHashMap :types/map)
(derive clojure.lang.PersistentArrayMap :types/map)

(derive clojure.lang.PersistentVector :types/seq)
(derive clojure.lang.PersistentList :types/seq)
(derive clojure.lang.LazySeq :types/seq)
