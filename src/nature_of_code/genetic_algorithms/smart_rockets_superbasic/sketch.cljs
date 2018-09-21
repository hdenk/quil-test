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
   :rocket-count 50 
   :rocket-r 4
   :rocket-color 127
   :thrusters-color 0}) 

;;
;; Abstractions
;;

(defprotocol Mobile
  (move [this] "moves the mobile Object"))

(defprotocol Massive
  (apply-force [this force] "apply force to the massive object"))

(defprotocol Drawable
  (draw [this] "draw the drawable object to an output-device"))

(defprotocol Genetic ; TODO find better names for this abstraction(s)
  (crossover [this partner] "produce new DNA by mixing genes of two individuals")
  (mutate [this mutation-rate] "mutate based on probability"))

;;
;; DNA
;;

; TODO spec-check ?
(defn check-dna [m]
  m) 

(defn random-gene [max-force]
  (let [angle (rand (* Math/PI 2))
        rand-gene [(Math/cos angle) (Math/sin angle)]]
    (mv/multiply rand-gene (rand max-force))
    rand-gene))

(defn random-dna [lifetime]
  (let [force (rand (config :max-force))
        genes (vec (repeatedly lifetime #(random-gene force)))]
    (check-dna {:max-force (config :max-force) 
                :genes genes})))

(defn crossover [dna partner-dna]
  (let [split-idx (rand-int (count (:genes dna)))
        child-genes (into [] (concat 
                        (first (split-at split-idx (:genes dna))) 
                        (second (split-at split-idx (:genes partner-dna)))))]
    (assoc dna :genes child-genes)))

(defn mutate [dna mutation-rate]
  (let [mutated-genes (into []
                            (map    
                              #(if (< (rand) mutation-rate)
                                 (random-gene (config :max-force))
                                 %)
                              (:genes dna)))]
    (assoc dna :genes mutated-genes)))

;;
;; Rocket
;;

(defn next-motion-state [rocket]
  (let [next-location (mv/add (:location rocket) (:velocity rocket))
        next-velocity (mv/add (:velocity rocket) (:acceleration rocket))
        next-acceleration (mv/multiply (:acceleration rocket) (float 0))]
    (assoc rocket :location next-location :velocity next-velocity :acceleration next-acceleration)))

(defrecord Rocket [id mass location velocity acceleration r fitness dna gene-index min-d hit-target]
  Mobile 
  (move [rocket]
    (if-not (:hit-target rocket)
      (let [dna (:dna rocket)
            genes (:genes dna)
            gene-index (:gene-index rocket)
            force (get genes gene-index)
            next-gene-index (mod (inc gene-index) (count genes))]
        (-> rocket 
            (apply-force force)
            (next-motion-state)
            (assoc :gene-index next-gene-index)))
      rocket))

  Massive
  (apply-force [rocket force]
    (let [mf (mv/divide force (float (:mass rocket)))
          next-acceleration (mv/add (:acceleration rocket) mf)]
      (assoc rocket :acceleration next-acceleration)))

  Drawable
  (draw [rocket]
    (q/fill 200 100)
    (q/stroke 0)
    (q/rect-mode :center)

    (q/push-matrix)
    (q/translate (first (:location rocket)) (second (:location rocket)))
    ; Draw a triangle rotated in the direction of velocity
    (let [theta (+ (mv/heading-2d (:velocity rocket)) (/ Math/PI 2))]
      (q/rotate theta))

    (let [r (:r rocket)
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
    (q/pop-matrix)))

(defn gen-rocket
  [& {:keys [id mass location velocity acceleration r fitness dna gene-counter min-d hit-target] 
      :or {id "rx" mass 1.0 location [0 0] velocity [0 0] acceleration [0 0] 
           r (config :rocket-r) fitness 0 dna [] gene-counter 0 min-d js/Number.MAX_SAFE_INTEGER hit-target false}}] 
  (Rocket. id mass location velocity acceleration r fitness dna gene-counter min-d hit-target))

; TODO fitness-funktion zurück nach einfach, min-d entfernen ?
(defn fitness [rocket target]
  (if (:hit-target rocket)
    ; hit-target -> fitness-criterium = how-fast
    (let [how-fast (Math/pow (- (config :lifetime) (:gene-index rocket)) 2)] 
      ;(dbg how-fast)
      (assoc rocket :fitness how-fast)) 
    ; didn't hit-target -> fitness-criterium = how-near
    (let [d (q/dist (first (:location rocket)) (second (:location rocket)) (first target) (second target))
          min-d (min (:min-d rocket) d)
          how-near (Math/pow (/ 1 min-d) 2)]
      ;(dbg how-near)
      (assoc rocket :fitness how-near))))

(defn check-target [rocket target]
  (let [d (q/dist (first (:location rocket)) (second (:location rocket)) (first target) (second target))
        next-hit-target (< d (config :target-r))
        next-min-d (min (:min-d rocket d))]
    (assoc rocket :hit-target next-hit-target :min-d next-min-d)))

;;
;; Population
;;

(defrecord Population [mutation-rate rockets mating-pool generation-count])

(defn gen-population
  [& {:keys [mutation-rate rockets mating-pool generation-count] 
      :or {mutation-rate 0.0 rockets [] mating-pool [] generation-count 0}}] 
  (Population. mutation-rate rockets mating-pool generation-count))

(defn gen-random-rockets [rocket-count]
  (into [] 
        (map
          #(gen-rocket
             :id (str "r" %)
             :location [(/ (q/width) 2) (- (q/height) 20)]
             :dna  (random-dna (config :lifetime)))
          (range rocket-count))))

(defn move-and-check-rockets [population target]
  (let [next-rockets (into []
                           (map 
                             #(-> % (move) (check-target target)) 
                             (:rockets population)))] 
    (assoc population :rockets next-rockets)))

(defn calc-rocket-fitness [population target]
  (let [next-rockets (into []
                           (map
                             #(fitness % target) 
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
  (dorun (map draw (:rockets population))))

;;
;; World
;;

(defrecord World [population target life-count])

(defn gen-world 
  [& {:keys [population target life-count] 
      :or {population nil target [0 0] life-count 0}}] 
  (World. population target life-count)) 

(def world (atom {}))  

;;
;; Sketch
;;

(defn setup-sketch []
  (q/frame-rate (config :frame-rate))
  (q/smooth)

  ; initialize world
  (let [rockets (gen-random-rockets (config :rocket-count))
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
