# think-stats

[Think Stats](http://www.greenteapress.com/thinkstats/) in Clojure.


## Usage

Get the data files from [here](http://www.greenteapress.com/thinkstats/) and
put them into the ./tmp directory.

    (require '[think-stats.survey :as s])
    (require '[think-stats.util :as util])
    (require '[think-stats.pregnancy :as preg])
    (require '[think-stats.stats :as stats])

    ; looking at the data
    (def preg (util/read-file "tmp/2002FemPreg.dat.gz" :gunzip true))
    (def db (map (partial s/line->fields preg/fields) preg))

    (def first-born (for [r db :when (= (get r "birthord") 1)] r))
    (def rest-born (for [r db :when (not= (get r "birthord") 1)] r))

    (def fb-prglength (for [r first-born] (get r "prglength")))
    (def rb-prglength (for [r rest-born] (get r "prglength")))

    ; ploting the hist of prglength by birthord
    (preg/plot-length-hist "tmp/2002FemPreg.dat.gz")  
    (preg/plot-diff-hist "tmp/2002FemPreg.dat.gz")  

## Chapter 3

    (def sample-r1m (take 1000000 (repeatedly #(inc (rand 99)))))

    (time (d/percentile-s sample-r1m 50))
    "Elapsed time: 1180.638 msecs"
    50.460172154741564

    (time (d/percentile-c sample-r1m 50))
    "Elapsed time: 815.941 msecs"
    50.460172154741564

    (time (d/percentile sample-r1m 50))
    "Elapsed time: 632.59 msecs"
    50.460172154741564



    

## License

Copyright © 2013 Damon Snyder 

Distributed under the Eclipse Public License, the same as Clojure.
