(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch
  "Rockets adapt behavior to environment by applying genetic algorithm
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [nature-of-code.math.vector :as mv]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.spec :as spec]))

(def config ^{:doc "DataStructure representing Params to customize the app"}
  {:background 255
   :frame-rate 30
   :lifetime 200 
   :mutation-rate 0.01
   :max-force 1.0
   :target-r 24
   :rocket-count 30
   :rocket-r 4
   :rocket-color 127
   :thrusters-color 0})

;;
;; forces (= genes)
;;

(defn random-force
  "gen random force"
  [force-limit]
  (let [angle (rand (* Math/PI 2))
        gene [(Math/cos angle) (Math/sin angle)]]
    (mv/multiply gene force-limit)))

(defn gen-forces
  "lifetime * random-force as vector)"
  [lifetime]
  (let [forces (vec (repeatedly
                     lifetime
                     #(random-force (rand (config :max-force)))))]
    forces))

(defn crossover
  "split two vectors and concat halves to a resulting vector"
  ([forces1 forces2 split-idx] ; to support testing
   (let [resulting-forces (vec (concat ; @see https://stuartsierra.com/2015/04/26/clojure-donts-concat
                                (first (split-at split-idx forces1))
                                (second (split-at split-idx forces2))))]
     resulting-forces))
  ([forces1 forces2] ; called by app
   (crossover forces1 forces2 (inc (rand-int (dec (count forces1))))))) ; [1..n-1]

(defn mutate [forces mutation-rate]
  (let [next-forces (map (fn [force] (if (< (rand) mutation-rate)
                                       (random-force (rand (config :max-force)))
                                       force))
                         forces)]
    next-forces))

;;
;; Rocket
;;

(defn gen-rocket
  [& {:keys [id mass location velocity forces force-index rocket-r fitness hit-target]
      :or {id "rx" mass 1.0 location [0 0] velocity [0 0] forces [] force-index 0
           rocket-r (config :rocket-r) fitness 0 hit-target false}}]
  {:id id :mass mass :location location :velocity velocity :forces forces :force-index force-index
   :rocket-r rocket-r :fitness fitness :hit-target hit-target})

(defn gen-rockets [location rocket-count]
  (mapv (fn [nr] (gen-rocket :id (str "r" nr)
                             :location location
                             :forces  (gen-forces (config :lifetime))))
        (range rocket-count)))

(defn next-motion-state [rocket]
  ;(js/console.log (str rocket))
  (let [force (nth (:forces rocket) (:force-index rocket))
        acceleration (mv/divide force (float (:mass rocket)))
        next-location (mv/add (:location rocket) (:velocity rocket))
        next-velocity (mv/add (:velocity rocket) acceleration)
        next-force-index (mod (inc (:force-index rocket)) (count (:forces rocket)))]
    (assoc rocket :location next-location :velocity next-velocity :force-index next-force-index)))

(defn check-hit-target [rocket target]
  (let [d (mv/distance (:location rocket) target)
        next-hit-target (< d (config :target-r))]
    (assoc rocket :hit-target next-hit-target)))

(defn fitness [rocket]
    ; hit-target -> fitness-criterium = how-fast
    (let [next-fitness (Math/pow (- (config :lifetime) (:force-index rocket)) 2)]
      (assoc rocket :fitness next-fitness)))

(defn draw-rocket [rocket]
  (q/fill 200 100)
  (q/stroke 0)
  (q/rect-mode :center)

  (q/push-matrix)
  (q/translate (first (:location rocket)) (second (:location rocket)))
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

;;
;; Population
;;

(defn gen-population
  [& {:keys [mutation-rate rockets mating-pool generation-count]
      :or {mutation-rate 0.0 rockets [] mating-pool [] generation-count 0}}]
  {:mutation-rate mutation-rate :rockets rockets :mating-pool mating-pool :generation-count generation-count})

(defn move-and-check-rockets [population target]
  (let [next-rockets (mapv
                            #(-> % (next-motion-state) (check-hit-target target))
                            (:rockets population))]
    (assoc population :rockets next-rockets)))

(defn calc-rocket-fitness [population target]
  (let [next-rockets (into []
                           (map
                            #(fitness %)
                            (:rockets population)))]
    (assoc population :rockets next-rockets)))

(defn dup-rockets [rocket max-fitness]
  (let [norm-fitness (q/map-range (:fitness rocket) 0 max-fitness 0 1)
        n (int (* norm-fitness 100))]
    (repeat n rocket)))

(defn gen-mating-pool [rockets max-fitness]
  (vec
   (apply
    concat
    (map
     #(dup-rockets % max-fitness) rockets))))

(defn populate-mating-pool [population]
  (let [rockets (:rockets population)
        max-fitness (:fitness (apply max-key :fitness rockets))
        next-mating-pool (gen-mating-pool rockets max-fitness)]
    ;(dbg max-fitness)
    (assoc population :mating-pool next-mating-pool)))

(defn combine-two-rockets [rocket-index rocket1 rocket2]
  (let [dna1 (:dna rocket1)
        dna2 (:dna rocket2)
        new-dna (crossover dna1 dna2)]
    (gen-rocket :id (str "r" rocket-index)
                :location [(/ (q/width) 2) (- (q/height) 20)]
                :dna new-dna)))

(defn mutate-rocket [rocket mutation-rate]
  (let [next-dna (mutate (:dna rocket) mutation-rate)]
    (assoc rocket :dna next-dna)))

(defn reproduce-rocket [rocket-index mating-pool mutation-rate]
  (let [pool-size (count mating-pool)
        rocket1-idx (rand-int pool-size)
        rocket2-idx (rand-int pool-size)
        rocket1 (get mating-pool rocket1-idx)
        rocket2 (get mating-pool rocket2-idx)
        new-rocket (combine-two-rockets rocket-index rocket1 rocket2)]
    (mutate-rocket new-rocket mutation-rate)))

(defn reproduce-rockets [rockets-count mating-pool mutation-rate]
  (into []
        (map
         #(reproduce-rocket % mating-pool mutation-rate)
         (range rockets-count))))

(defn next-generation [population]
  (let [mutation-rate (:mutation-rate population)
        mating-pool (:mating-pool population)
        rockets-count (count (:rockets population))
        next-rockets (reproduce-rockets rockets-count mating-pool mutation-rate)
        next-generation-count (inc (:generation-count population))]
    (assoc population :rockets next-rockets :generation-count next-generation-count)))

(defn draw-population [population]
  (dorun (map draw-rocket (:rockets population))))

;;
;; World
;;

(defn gen-world
  [& {:keys [population target life-count]
      :or {population nil target [0 0] life-count 0}}]
  {:population population :target target :life-count life-count})

(def world (atom {}))

;;
;; Sketch
;;

(defn setup-sketch []
  (js/console.log (str "setup-sketch " (q/width) " " (q/height)))

  (q/frame-rate (config :frame-rate))
  (q/smooth)

  ; initialize world
  (let [initial-rocket-location (vector (/ (q/width) 2) (- (q/height) 20))
        rockets (gen-rockets initial-rocket-location (config :rocket-count))
        mutation-rate (config :mutation-rate)
        population (gen-population :mutation-rate mutation-rate :rockets rockets)
        target [(/ (q/width) 2) (config :target-r)]]
    (swap! world (constantly (gen-world :population population :target target :life-count 0)))))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255)
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  (let [population (:population @world)
        target (:target @world)
        life-count (:life-count @world)]
    ; draw target
    (q/fill 0)
    (q/ellipse (first target) (second target) (config :target-r) (config :target-r))

    ; draw rockets 
    (draw-population (:population @world))

    ; state-progression 
    (if (< life-count (config :lifetime))
      ; next step in current populations life
      (let [next-population (move-and-check-rockets population target)
            next-life-count (inc life-count)]
        (swap! world assoc :population next-population :life-count next-life-count))
      ; next generation
      (let [next-population (-> population
                                (calc-rocket-fitness target)
                                (populate-mating-pool)
                                (next-generation))]
        (swap! world assoc :population next-population :life-count 0)))

    ; Display some info
    (q/fill 0)
    (q/text (str "Generation #: " (:generation-count population)) 10 18)
    (q/text (str "Cycles left: " (- (config :lifetime) life-count)) 10 36)))

(defn mouse-pressed []
  (swap! world assoc :target [(q/mouse-x) (q/mouse-y)]))
