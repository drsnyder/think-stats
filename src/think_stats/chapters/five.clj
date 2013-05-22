(ns think-stats.chapters.five)

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

(defn monty-hall-trial
  "Runs a single trial of the Monty Hall problem."
  [sim-map stay?]
  (let [doors (create-doors)
        user-door (select-door doors number-doors)
        user-prize (nth doors user-door)
        other-door (montys-pick-leaves (remove-door user-door doors))]
    (cond-> (update-in sim-map [:trial] inc)
            (and stay? (= user-prize :car)) (update-in [:wins] inc)
            (and (not stay?) (= other-door :car)) (update-in [:wins] inc))))

(defn simulate-monty-hall
  "Simulates the Monty Hall problem out to the given horizon using the strategy specified by stay?."
  [horizon stay?]
  (let [final (last 
                (take horizon 
                      (iterate #(monty-hall-trial % stay?) starting-sim-map)))
        wins (:wins final)
        trials (:trial final)]
   {:trials trials
    :wins wins
    :win-rate (float (/ wins horizon))}))


