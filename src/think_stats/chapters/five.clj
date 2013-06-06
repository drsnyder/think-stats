(ns think-stats.chapters.five
  (:require (think-stats
              [stats :as stats]
              [util :as util]
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
  [out-file mean stddev n sims]
  (let [bakery (map int (repeatedly sims #(random/normalvariate mean stddev)))
        bpmf (stats/pmf bakery)
        p-loafs (map int (baker-trial mean stddev n sims))
        ppmf (stats/pmf p-loafs)]
    (util/write-to-csv out-file (conj
                               (map vector
                                    (keys bpmf)
                                    (vals bpmf)
                                    (keys ppmf)
                                    (vals ppmf))
                               ["bweights" "bfreq" "pweights" "pfreq"]))
    [(stats/summary bakery) (stats/summary p-loafs)]))



