(ns nature-of-code.systems.particlesystem-forces.sketch
  "Particle-System produces Particles that experience Gravity
   Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]
            [nature-of-code.systems.particlesystem-forces.spec :as spec]))
;;
;; config
;;

(defn check-config [m]
  (when (m :check-spec)
    (spec/check-config m))
  m)

(def config (check-config {:background 255
                           :frame-rate 30
                           :gravity [0.0 0.1]
                           :lifespan 255
                           :lifespan-dec-rate 1.5
                           :circle-r 16
                           :square-l 16
                           :particle-color 127
                           :particle-outline-thickness 2
                           :particle-shapes [:circle :rotating-square]
                           :check-spec true}))

;;
;; particle
;;

(defn check-particle [m]
  (when (config :check-spec)
    (spec/check-particle m))
  m)

(defn apply-force-to-particle [{:keys [mass acceleration] :as particle} force]
  (let [mf (mv/divide force (float mass))
        next-acceleration (mv/add acceleration mf)]
    (assoc particle :acceleration next-acceleration)))

(defn move-particle [{:keys [mass location velocity acceleration lifespan] :as particle}]
  (let [next-location (mv/add location velocity)
        next-velocity (mv/add velocity acceleration)
        next-acceleration (mv/multiply acceleration 0)
        next-lifespan (- lifespan (config :lifespan-dec-rate))]
    (assoc particle :location next-location :velocity next-velocity :acceleration next-acceleration :lifespan next-lifespan)))

(defn next-particle [particle force]
  (-> particle
      (apply-force-to-particle force)
      (move-particle)))

(defn is-expired-particle? [{:keys [lifespan] :as particle}]
  (< lifespan 0))

(defmulti draw-particle (fn [particle] (:shape particle)))

(defmethod draw-particle :circle [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (config :particle-outline-thickness))
  (q/fill (config :particle-color) lifespan)
  (q/ellipse (first location) (second location) (config :circle-r) (config :circle-r))
  particle)

(defmethod draw-particle :square [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (config :particle-outline-thickness))
  (q/fill (config :particle-color) lifespan)
  (q/rect-mode :center)
  (q/rect (first location) (second location) (config :square-l) (config :square-l))
  (q/rect-mode :corner) ; TODO? get current rect-mode from graphic object
  particle)

(defmethod draw-particle :rotating-square [{:keys [location lifespan] :as particle}]
  (q/stroke 0 lifespan)
  (q/stroke-weight (config :particle-outline-thickness))
  (q/fill (config :particle-color) lifespan)
  (q/push-matrix)
  (q/translate (first location) (second location))
  (let [theta (q/map-range (first location) 0 (q/width) 0 (* Math/PI 2))]
    (q/rotate theta))
  (q/rect-mode :center)
  (q/rect 0 0 (config :square-l) (config :square-l))
  (q/rect-mode :corner) ; TODO? get current rect-mode from graphic object
  (q/pop-matrix)
  particle)

;;
;; particle-system
;;

(defn check-particle-system [m]
  (when (config :check-spec)
    (spec/check-particle-system m))
  m)

(defn next-particles [{:keys [gravity particles] :as particle-system}]
  (let [next-particles (into [] (map (fn [particle] (next-particle particle gravity))
                                     particles))]
    (assoc particle-system :particles next-particles)))

(defn random-shape []
  (rand-nth (config :particle-shapes)))

(defn add-new-particle [{:keys [origin particles] :as particle-system}]
  (let [next-particles (conj particles
                             (check-particle {:id (count particles)
                                              :shape (random-shape)
                                              :mass 1.0
                                              :location origin
                                              :velocity [(q/random -2.0 2.0) (q/random -2.0 0.0)]
                                              :acceleration [0 0]
                                              :lifespan (config :lifespan)}))]
    (assoc particle-system :particles next-particles)))

(defn remove-expired-particles [{:keys [particles] :as particle-system}]
  (let [next-particles (remove is-expired-particle? particles)]
    (assoc particle-system :particles next-particles)))

(defn next-particle-system [particle-system]
  (-> particle-system
      (next-particles)
      (add-new-particle)
      (remove-expired-particles)))

(defn draw-particle-system [{:keys [particles] :as particle-system}]
  (dorun (map (fn [particle] (draw-particle particle)) particles))
  particle-system)

;;
;; Sketch
;; 

(def particle-system (atom nil))

(defn setup-sketch []
  (js/console.log (str "setup-sketch " (q/width) " " (q/height)))

  (swap! particle-system (fn [_] (check-particle-system {:origin [(/ (q/width) 2) (- (q/height) (* (q/height) 0.75))]
                                                         :gravity (config :gravity)
                                                         :particles []})))

  (q/frame-rate (config :frame-rate))
  (q/smooth)
  (q/rect-mode :corner))

(defn draw-sketch []
  ; draw Background
  (q/no-stroke)
  (q/fill 255)
  (q/rect 0 0 (q/width) (q/height))

  ; draw particle-system
  (draw-particle-system @particle-system)

  ; update ParticleSystem to next-state
  (swap! particle-system next-particle-system))
