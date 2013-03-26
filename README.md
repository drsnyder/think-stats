# think-stats

[Think Stats](http://www.greenteapress.com/thinkstats/) in Clojure.


## Usage

Get the data files from [here](http://www.greenteapress.com/thinkstats/) and
put them into the ./tmp directory.

    (require '[think-stats.survey :as s])
    (require '[think-stats.util :as util])
    (require '[think-stats.pregnancy :as preg])
    (require '[think-stats.stats :as stats])

    (def preg (util/read-file "tmp/2002FemPreg.dat.gz" :gunzip true))
    (def db (map (partial s/line->fields preg/fields) preg))

    (def first-born (for [r db :when (= (get r "birthord") 1)] r))
    (def rest-born (for [r db :when (not= (get r "birthord") 1)] r))

    (def fb-prglength (for [r first-born] (get r "prglength")))
    (def rb-prglength (for [r rest-born] (get r "prglength")))


## License

Copyright © 2013 Damon Snyder 

Distributed under the Eclipse Public License, the same as Clojure.
