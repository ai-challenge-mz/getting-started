(ns upgrade-bot
  (:require [clojure.string :as string]))

(defrecord Planet [planet-id x y growth-rate owner num-ships])

(def ^:const neutral-id 0)

(defn read-world []
  (loop [world {:planets [] :player-id -1}]
    (let [line (string/trim (read-line))]
      (cond
        (= "." line) world
        (re-find #"^P" line)
        (let [values (->> (rest (string/split line #"\s+"))
                          (map #(Integer/parseInt %)))
              planet (apply ->Planet values)]
          (recur (update-in world [:planets] conj planet)))
        (re-find #"^Y" line)
        (let [[player-id] (->> (rest (string/split line #"\s+"))
                               (map #(Integer/parseInt %)))]
          (recur (assoc world :player-id player-id)))
        :else (recur world)))))

(defn write-turns [turns]
  (doseq [turn turns]
    (println turn))
  (println ".")
  (flush))

(defn- planets-by-owner [world player-id]
  (->> (:planets world)
       (filter #(= player-id (:owner %)))))

(defn try-boost [world]
  (when-first [target (->> (planets-by-owner world (:player-id world))
                           (filter #(<= (:growth-rate %) 6))
                           (filter #(<= (bit-shift-left 1 (:growth-rate %)) (:num-ships %))))]
    [(str "B " (:planet-id target))]))

(defn try-force [world]
  (when-first [[src dst] (for [assault (planets-by-owner world (:player-id world))
                               neutral (planets-by-owner world neutral-id)
                               :when (< (:num-ships neutral) (:num-ships assault))]
                           [assault neutral])]
    [(str "F " (:planet-id src) " " (:planet-id dst) " " (inc (:num-ships dst)))]))

(while true
  (let [world (read-world)
        turns (some #(% world) [try-force try-boost])]
    (write-turns turns)))
