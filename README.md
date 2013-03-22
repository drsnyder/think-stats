# think-stats

[Think Stats](http://www.greenteapress.com/thinkstats/) in Clojure.


## Usage

Get the data files from [here](http://www.greenteapress.com/thinkstats/) and
put them into the ./tmp directory.

    (require '[thinkstats.survey :as s])
    (require '[thinkstats.util :as util])
    (require '[thinkstats.pregnancy :as preg])

    (def preg (util/read-file "tmp/2002FemPreg.dat.gz" :gunzip true))
    (def db (map (partial s/line->fields preg/fields) preg))

    (def first-born (for [r db :when (= (get r "birthord") 1)] r))
    (def rest-born (for [r db :when (not= (get r "birthord") 1)] r))


## License

Copyright © 2013 Damon Snyder 

Distributed under the Eclipse Public License, the same as Clojure.
