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

(defn crossover-forces
  "split two vectors and concat halves to a resulting vector"
  ([forces1 forces2 split-idx] ; to support testing
   (let [resulting-forces (vec (concat ; @see https://stuartsierra.com/2015/04/26/clojure-donts-concat
                                (first (split-at split-idx forces1))
                                (second (split-at split-idx forces2))))]
     resulting-forces))
  ([forces1 forces2] ; called by app
   (crossover-forces forces1 forces2 (inc (rand-int (dec (count forces1))))))) ; [1..n-1]

(defn mutate-forces [forces mutation-rate]
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

(defn move-rocket-one-step [rocket]
  ;(js/console.log (str rocket))
  (let [force (nth (:forces rocket) (:force-index rocket))
        acceleration (mv/divide force (float (:mass rocket)))
        next-location (mv/add (:location rocket) (:velocity rocket))
        next-velocity (mv/add (:velocity rocket) acceleration)
        next-force-index (mod (inc (:force-index rocket)) (count (:forces rocket)))]
    (assoc rocket :location next-location :velocity next-velocity :force-index next-force-index)))

(defn check-rocket-hit-target [rocket target]
  (let [d (mv/distance (:location rocket) target)
        next-hit-target (< d (config :target-r))]
    (assoc rocket :hit-target next-hit-target)))

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

(defn draw-target [target]
    (q/fill 0)
    (q/ellipse (first target) (second target) (config :target-r) (config :target-r)))

(defn move-all-rockets (rockets target)
  (let [next-rockets (mapv (fn [rocket] 
                             (if-not (:hit-target rocket)
                               (-> rocket 
                                   (move-rocket-one-step) 
                                   (check-rocket-hit-target target))
                               rocket))
                           rockets)]
    next-rockets))

(defn calc-rockets-fitness (rockets)
  (let [next-rockets (mapv (fn [rocket]
                              (let [next-fitness (Math/pow (- (config :lifetime) (:force-index rocket)) 2)]
                                (assoc rocket :fitness next-fitness)))
                           rockets)]
    next-rockets))

(map-range [value from-min from-max to-min to-max]
  (+ to-main (* (- to-max to-min) (/ (- value from-min) (- from-max from-min))))

(defn reproduce-forces [rocket max-fitness]
  (let [norm-fitness (map-range (:fitness rocket) 0 max-fitness 0 1)
        n (int (* norm-fitness 100))]
    (repeat n (:forces rocket))))

(defn gen-mating-pool [rockets]
  (let [max-fitness (:fitness (apply max-key :fitness rockets))]
    (vec (mapcat (fn [rocket]
                     (reproduce-forces rocket max-fitness)) 
                 rockets))))

(defn reproduce-rockets [rockets]
  (let [mating-pool (gen-mating-pool rockets)
        pool-size (count mating-pool)]
    (mapv (fn [id] 
              (let [forces-1 (nth mating-pool (random-int pool-size))
                    forces-2 (nth mating-pool (random-int pool-size))
                    xforces (crossover-forces forces-1 forces-2)
                    forces (mutate-forces xforces (config :mutation-rate))]
                (gen-rocket :id (str "r" id) ; TODO gen-rocket ?
                            :location [(/ (q/width) 2) (- (q/height) 20)]
                            :forces forces)))
          (range (count rockets)))))

;;
;; Sketch
;;

(def sketch-model (atom nil))

(defn init-sketch-model [m-atom]
  (let [initial-rocket-location (vector (/ (q/width) 2) (- (q/height) 20))
        rockets (gen-rockets initial-rocket-location (config :rocket-count))
        initial-target-location (vector (/ (q/width) 2) (config :target-r))
        target initial-target-location]
    (swap! m-atom (constantly {:rockets rockets :target target :generation-count 0 :life-count 0}))))

(defn setup-sketch []
  (js/console.log (str "setup-sketch " (q/width) " " (q/height)))

  (q/frame-rate (config :frame-rate))
  (q/smooth)

  (init-sketch-model sketch-model))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255)
  (q/rect-mode :corner)
  (q/rect 0 0 (q/width) (q/height))

  (let [rockets (:rockets @sketch-model)
        target (:target @sketch-model)
        life-count (:life-count @sketch-model)
        generation-count (:generation-count @sketch-model)]
    
    (draw-target target)

    ; draw rockets 
    (run! draw-rockets rockets) ; @see https://clojuredocs.org/clojure.core/run! and also...
                                ; https://stuartsierra.com/2015/08/25/clojure-donts-lazy-effects        
    ; state-progression 
    (if (< life-count (config :lifetime))
      ; either next motion-step
      (let [next-rockets (move-rockets-one-step rockets target)
            next-life-count (inc life-count)]
        (swap! sketch-model (fn [m] (assoc m :rockets next-rockets :life-count next-life-count))))
      ; or next generation
      (let [next-rockets (-> rockets 
                                (calc-rockets-fitness)
                                (reproduce-rockets))
            next-generation-count (inc generation-count)]
        (swap! sketch-model (fn [m] (assoc m :rockets next-rockets :life-count 0 :generation-count next-generation-count)))

    ; Display some info
    (q/fill 0)
    (q/text (str "Generation #: " (:generation-count population)) 10 18)
    (q/text (str "Cycles left: " (- (config :lifetime) life-count)) 10 36)))))

(defn mouse-pressed []
  (swap! sketch-model (fn [m] assoc m :target [(q/mouse-x) (q/mouse-y)])))
