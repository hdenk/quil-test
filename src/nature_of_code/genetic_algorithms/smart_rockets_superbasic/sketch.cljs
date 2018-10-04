(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch
  "Rockets adapt behavior to environment by applying genetic algorithm
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [nature-of-code.math.vector :as mv]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.spec :as spec]))

(def config 
  ^{:doc "DataStructure representing Params to customize the app"}
  {:background 255
   :frame-rate 30
   :lifetime 200
   :fitness-fn :distance
   :mutation-rate 0.05
   :max-force 1.0
   :mating-factor 100
   :target-r 24
   :rocket-count 50
   :rocket-r 4
   :rocket-color 127
   :thrusters-color 0})

;;
;; forces (genes)
;;

(defn random-force
  "Gen random force (2D)"
  [force-limit]
  (let [angle (rand (* Math/PI 2))
        gene [(Math/cos angle) (Math/sin angle)]]
    (mv/multiply gene force-limit)))

(defn gen-forces
  "Gen lifetime * random-force as vector"
  [lifetime]
  (let [forces (vec (repeatedly lifetime
                                #(random-force (rand (config :max-force)))))]
    forces))

(defn crossover-forces
  "Split two vectors and concat halves to a resulting vector"
  ([forces-1 forces-2 split-idx] ; to support testing
   (let [resulting-forces (vec (concat ; @see https://stuartsierra.com/2015/04/26/clojure-donts-concat
                                (first (split-at split-idx forces-1))
                                (second (split-at split-idx forces-2))))]
     resulting-forces))
  ([forces-1 forces-2] ; called by app
   (crossover-forces forces-1 forces-2 (inc (rand-int (dec (count forces-1))))))) ; [1..n-1]

(defn mutate-forces 
  "Replace forces (by chance) with random-force"
  [forces mutation-rate]
  (let [next-forces (map (fn [force] (if (< (rand) mutation-rate)
                                       (random-force (rand (config :max-force)))
                                       force))
                         forces)]
    next-forces))

;;
;; Rocket
;;

(defn gen-rocket 
  "Gen rocket, merge with default-attributes"
  [m]
  (merge {:id nil
          :mass 1.0
          :location  nil
          :velocity [0.0 0.0]
          :forces nil
          :force-index 0
          :rocket-r (config :rocket-r)
          :fitness 0.0
          :hit-target false}
         m))

(defn gen-rocket-population 
  "Gen a population of rockets"
  [location forces rocket-count]
  (mapv (fn [nr] (gen-rocket {:id (str "r" nr)
                              :location location
                              :forces (gen-forces (config :lifetime))}))
        (range rocket-count)))

(defn move-rocket 
  "Progress rocket to next motion-state"
  [rocket]
  (let [force (nth (:forces rocket) (:force-index rocket))
        acceleration (mv/divide force (float (:mass rocket)))
        next-location (mv/add (:location rocket) (:velocity rocket))
        next-velocity (mv/add (:velocity rocket) acceleration)
        next-force-index (inc (:force-index rocket))]
    (assoc rocket :location next-location :velocity next-velocity :force-index next-force-index)))

(defn check-rocket-hit-target
  "Check if rocket hit target, sets attribute :hit-target" 
  [rocket target]
  (let [d (mv/distance (:location rocket) (:location target))
        next-hit-target (< d (:target-r target))]
    (assoc rocket :hit-target next-hit-target)))

(defn move-and-check-population [rockets target]
  (let [next-rockets (mapv
                      (fn [rocket] (if-not (:hit-target rocket)
                                     (-> rocket
                                         (move-rocket)
                                         (check-rocket-hit-target target))
                                     rocket))
                      rockets)]
    next-rockets))

(defmulti fitness
  "Fitness-Function is important, there maybe alternatives" 
  (fn [key rocket & other] key))

(defmethod fitness :steps [key rocket & _]
  "Fitness-Kriterium is steps to target, the fewer steps the fitter. Minimal fitness is 1"
  (Math/pow (max 1 (- (config :lifetime) (:force-index rocket))) 2))

(defmethod fitness :distance [key rocket & [target]]
  "Fitness-Kriterium is distance to target, the nearer the fitter. Minimal fittnes is 1"
  (/ 1 (Math/pow (max 1 (mv/distance (:location rocket) (:location target))) 2)))

(defn update-population-fitness 
  "Update fitness for all rockets"
  [rockets fitness-fn]
  (let [next-rockets (mapv
                      (fn [rocket]
                        (let [next-fitness (fitness-fn rocket)]
                          (assoc rocket :fitness next-fitness)))
                      rockets)]
    next-rockets))

(defn map-range
  "Transform a value from one range to another" 
  [value from-min from-max to-min to-max]
  (+ to-min (* (- to-max to-min) (/ (- value from-min) (- from-max from-min)))))

(defn reproduce-forces
  "Produce 1 to :mating-factor forces (clone)" 
  [rocket max-fitness]
  (let [norm-fitness (map-range (:fitness rocket) 0 max-fitness 0 1)
        n (max 1 (int (* norm-fitness (config :mating-factor))))]
    ;(js/console.log (str "fitness:" (:fitness rocket) ", max-fitness:" max-fitness ", n:" n ", norm:" norm-fitness))
    (repeat n (:forces rocket))))

(defn gen-mating-pool
  "Gen a pool of forces (criterium is fitness)" 
  [rockets]
  (let [max-fitness (:fitness (apply max-key :fitness rockets))]
    (vec (mapcat (fn [rocket]
                   (reproduce-forces rocket max-fitness))
                 rockets))))

(defn select-next-population 
  "Gen next population by 1. selection (random from mating-pool), 2. crossover, 3. mutation"
  [rockets location]
  (let [mating-pool (gen-mating-pool rockets)
        pool-size (count mating-pool)]
    (mapv (fn [nr]
            (let [forces-1 (nth mating-pool (rand-int pool-size))
                  forces-2 (nth mating-pool (rand-int pool-size))
                  x-forces (crossover-forces forces-1 forces-2)
                  forces (mutate-forces x-forces (config :mutation-rate))]
              (gen-rocket {:id (str "r" nr)
                           :location location
                           :forces forces})))
          (range (count rockets)))))

(defn draw-rocket 
  "Draw one rocket"
  [rocket]
  (q/fill 200 100)
  (q/stroke 0)
  (q/rect-mode :center)

  (q/push-matrix)
  ; Translate to rocket location
  (let [[location-x location-y] (:location rocket)]
    (q/translate location-x location-y))
  ; Draw a triangle rotated in the direction of velocity
  (let [theta (+ (mv/heading-2d (:velocity rocket)) (/ Math/PI 2))]
    (q/rotate theta))

  (let [r (:rocket-r rocket)
        rh (/ r 2)
        r2 (* r 2)]
    ; Thrusters
    (q/fill (config :thrusters-color))
    (q/rect (* rh -1) r2 rh r)
    (q/rect rh r2 rh r)

    ; Rocket body
    (q/fill (config :rocket-color))
    (q/begin-shape :triangles)
    (q/vertex 0 (* r2 -1))
    (q/vertex (* r -1) r2)
    (q/vertex r r2)
    (q/end-shape :close)) ; processing.core.PConstants/CLOSE
  (q/pop-matrix))

(defn draw-target 
  "Draw target"
  [target]
  (q/fill 0)
  (let [[location-x location-y] (:location target)
        target-r (:target-r target)]
    (q/ellipse location-x location-y target-r target-r)))

;;
;; Sketch
;;

(def sketch-model  
  ^{:doc "Contains important data"}
  (atom nil))

(defn initial-rocket-location [[size-x size-y :as sketch-size]]
  (vector (/ size-x 2) (- size-y 20)))

(defn initial-target-location [[size-x size-y :as sketch-size]]
  (vector (/ size-x 2) (config :target-r)))

(defn init-sketch-model
  "Initialize sketch-model, called by quil's setup"
  [m-atom]
  (let [sketch-size (vector (q/width) (q/height))
        rocket-location (initial-rocket-location sketch-size)
        rocket-forces (gen-forces (config :lifetime))
        rockets (gen-rocket-population rocket-location rocket-forces (config :rocket-count))
        target-location (initial-target-location sketch-size)
        target {:location target-location :target-r (config :target-r)}]
    (swap! m-atom (fn [_] {:sketch-size sketch-size
                           :rockets rockets
                           :target target
                           :generation-count 0
                           :life-count 0}))))

(defn setup-sketch 
  "Setup sketch, Callback-Fn that gets called by quil, must be registered"
  []
  (js/console.log (str "setup-sketch " (q/width) " " (q/height)))
  (q/frame-rate (config :frame-rate))
  (q/smooth)
  (init-sketch-model sketch-model))

(defn draw-sketch 
  "Draw sketch, Callback-Fn that gets called by quil, must be registered"
  []
  ; draw Background
  (q/no-stroke)
  (q/fill 255)
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  (let [sketch-size (:sketch-size @sketch-model)
        rockets (:rockets @sketch-model)
        target (:target @sketch-model)
        life-count (:life-count @sketch-model)
        generation-count (:generation-count @sketch-model)]

    (draw-target target)

    ; draw rockets 
    (run! draw-rocket rockets) ; @see https://clojuredocs.org/clojure.core/run! and also...
                                ; https://stuartsierra.com/2015/08/25/clojure-donts-lazy-effects        
    ; state-progression 
    (if (< life-count (config :lifetime))
      (let [next-rockets (move-and-check-population rockets target) ; either next motion-step 
            next-life-count (inc life-count)]
        (swap! sketch-model (fn [m] (assoc m :rockets next-rockets :life-count next-life-count))))
      (let [fitness-fn (fn [rocket] (fitness (config :fitness-fn) rocket target))
            next-rockets (-> rockets                                ; or next generation 
                             (update-population-fitness fitness-fn)
                             (select-next-population (initial-rocket-location sketch-size)))
            next-generation-count (inc generation-count)]
        (swap! sketch-model (fn [m] (assoc m :rockets next-rockets :life-count 0 :generation-count next-generation-count))))))

    ; Display some info
  (q/fill 0)
  (q/text (str "Generation #: " (:generation-count @sketch-model)) 10 18)
  (q/text (str "Cycles left: " (- (config :lifetime) (:life-count @sketch-model))) 10 36))

(defn mouse-pressed 
  "Mouse-Handler, Callback-Fn that gets called by quil, must be registered"
  []
  (swap! sketch-model (fn [m] (assoc m :target {:location [(q/mouse-x) (q/mouse-y)] :target-r (config :target-r)}))))
