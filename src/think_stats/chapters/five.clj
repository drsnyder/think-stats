(ns think-stats.chapters.five
  (:require (think-stats
              [stats :as stats]
              [distributions :as d]
              [util :as util]
              [probability :as p]
              [random :as random])))

(def number-doors 3)

(def starting-sim-map {:trial 1 :wins 0})

(defn create-doors
  []
  (-> (vec (take number-doors (cycle [:goat])))
      (assoc (rand-int number-doors) :car)))

(defn select-door
  "The user uses this to select a door"
  [doors num-doors]
  (rand-int num-doors))

(defn remove-door
  "Monty uses this to remove a door"
  [door doors]
  (assert (>= (count doors) 3) "remove-door assumes 3 or more doors")
  (condp = door
    0 (subvec doors 1 number-doors)
    number-doors (subvec doors 0 (dec number-doors))
    (vec (concat (subvec doors 0 door) (subvec doors (inc door) number-doors)))))

(defn montys-pick-leaves
  "Pick the door that doesn't have the car."
  [remaining]
  (cond
    (= (first remaining) :car) :car
    (= (second remaining) :car) :car
    :else :goat))

(defn make-selections
  "Make the selections.
  (make-selections doors)      ; picks a random door
  (make-selections doors user-selected-door) ; uses door
  "
  ([doors user-selected-door]
   (let [user-prize (nth doors user-selected-door)
         other-door (montys-pick-leaves (remove-door user-selected-door doors))]
     [user-prize other-door]))
  ([doors]
   (make-selections doors (select-door doors number-doors))))


(defn update-sim-map
  "Update the sim-map based on the selections made by the user and Monty and whether the
  user decided to stay or switch."
  [sim-map selections stay?]
  (let [[user-prize other-door] selections]
    (cond-> (update-in sim-map [:trial] inc)
            (and stay? (= user-prize :car)) (update-in [:wins] inc)
            (and (not stay?) (= other-door :car)) (update-in [:wins] inc))))


(defn monty-hall-trial
  "Runs a single trial of the Monty Hall problem."
  [sim-map doors stay?]
  (update-sim-map sim-map (make-selections doors) stay?))


(defn simulate-monty-hall
  "Simulates the Monty Hall problem out to the given horizon using the strategy specified by stay?."
  [horizon stay?]
  (take horizon
        (iterate (fn [sim-map]
                   (monty-hall-trial sim-map (create-doors) stay?)) starting-sim-map)))


(defn summarize-monty-hall-simulation
  "Summarize a Monty Hall simulation."
  [horizon stay?]
  (let [final (last (simulate-monty-hall horizon stay?))
        wins (:wins final)
        trials (:trial final)]
   {:trials trials
    :wins wins
    :win-rate (float (/ wins horizon))}))



; Questions:
; 5.3
; 1: (p/independent-and 1/2 1/2) => 1/4
; 2: (p/dependent-and 1/2 1/2)   => 1/2
; 3: (p/dependent-and 1/2 1/2)   => 1/2
; 4: (p/dependent-and 1/2 1/2)   => 1/2


(defn baker-trial
  [mean stddev n sims]
  (repeatedly sims
              (fn []
                (apply max
                       (repeatedly n
                                   #(random/normalvariate mean stddev))))))

(defn compare-baker-and-poincare
  "5.6: Compare a sampling of the baker's bread with what he is giving Poincaré."
  [mean stddev n sims]
  (let [r-script "plots/bakery.R"
        csv-out "plots/bakery.csv"
        csv-raw-out "plots/bakery-raw.csv"
        p-loafs (map int (baker-trial mean stddev n sims))
        pcdf (d/cdf p-loafs :to-float true)
        bakery (map int (repeatedly sims #(random/normalvariate (stats/mean p-loafs) (stats/stddev p-loafs))))
        bcdf (d/cdf bakery :to-float true)]
    (util/write-to-csv csv-out (conj
                               (map vector
                                    (keys bcdf)
                                    (vals bcdf)
                                    (keys pcdf)
                                    (vals pcdf))
                               ["bweights" "Baker" "pweights" "Poincare"]))
    (util/write-to-csv csv-raw-out (conj (map vector bakery p-loafs) ["baker" "poincare"]))
    (let [ret (util/shell-exec (format "Rscript %s %s %s" r-script csv-out csv-raw-out))]
      (if (not= (:exit ret) 0)
        (println "Error: " (:err ret))
        (println (:out ret)))
      [bakery p-loafs])))


(defn dance-party
  "5.7: In the BRFSS (see Section 4.5), the distribution of heights is roughly normal with
  parameters μ=178cm and σ2=59.4cm for men, and μ=163cm and σ2 = 52.8 cm for women.

  One way to determine this probability is to simulate pairing p partners and count the number of occurences where
  the height of the woman and greater than the height of the male. For more accuracy we could simulate this n times
  and take the mean.

  I wanted to confirm this result analytically, but I wasn't sure how. I later found this:
  http://stats.stackexchange.com/questions/24693/probability-that-random-variable-b-is-greater-than-random-variable-a
  which confirmed the simulated result.
  "
  []
  (let [horizon 1000
        sims 100
        men-mean 178
        men-var 59.4
        men-sigma (Math/sqrt men-var)
        women-mean 163
        woman-var 52.8
        women-sigma (Math/sqrt woman-var)
        sim (repeatedly sims (fn []
                               (/ (count (filter
                                           #(> (second %) (first %))
                                           (map vector
                                                (random/sample horizon #(random/normalvariate men-mean men-sigma))
                                                (random/sample horizon #(random/normalvariate women-mean women-sigma)))))
                                  horizon)))]
    {:men-mean men-mean
     :men-sigma men-sigma
     :women-mean women-mean
     :women-sigma women-sigma
     :prob (stats/z->p-value (p/stress-strength-prob men-mean men-var women-mean woman-var))
     :sim (float (stats/mean sim))}))

(defn streak-sim
  [shots make p trials]
  (let [event (p/bernoulli-event p)
        game #(util/contains-streak? (repeatedly shots event) 1 make)
        sim (repeatedly trials game)
        streaks (filter true? sim)]
    (float 
      (/ (count streaks) 
         trials))))
