(ns nature-of-code.forces.fluidresistance.sketch
  "Demonstration of multiple force acting on bodies (Mover class)
  Bodies experience gravity continuously
  Bodies experience fluid resistance when in water
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as qc :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]))

(def config
  {:background 255
   :frame-rate 30
   :mover-count 5
   :mass-classes 3
   :r-factor 16
   :re-bouncing-factor -0.6
   :initial-speed-x 0
   :initial-speed-y 0
   :initial-acceleration-x 0
   :initial-acceleration-y 0
   :drag-coefficient 0.2
   :mover-color 127
   :fluid-color 211})

;;;
;;; Fluid
;;;

(defn gen-fluid []
  {:id "fluid1"
   :x 0 :y (* (qc/height) 0.75) :width (qc/width) :height (qc/height)
   :color (config :fluid-color) :drag-coefficient (config :drag-coefficient)})

(defn contains-mover?
  "takes a fluid and a mover and returns true, if the mover is inside the fluid"
  [{:keys [x y width height] :as fluid} {:keys [location] :as mover}]
  (let [[mover-x mover-y] location]
    (if (and
         (>= mover-x x) (<= mover-x (+ x width))
         (>= mover-y y) (<= mover-y (+ y height)))
      true
      false)))

(defn drag-force
  "takes a fluid and a mover and returns drag-force"
  [{:keys [drag-coefficient] :as fluid} {:keys [velocity] :as mover}]
  (if (contains-mover? fluid mover)
    (let [speed (mv/magnitude velocity)
          drag-magnitude (* drag-coefficient speed speed)
          drag-force (mv/multiply velocity (float -1))]
      (-> (mv/normalize drag-force)
          (mv/multiply (float drag-magnitude))))
    [0 0]))

(defn draw-fluid
  [{:keys [x y width height color] :as fluid}]
  (qc/no-stroke)
  (qc/fill color)
  (qc/rect x y width height)
  fluid)

;;;
;;; Mover
;;;

(defn gen-movers []
  (map
   (fn [id]
     {:id (str "mover" id)
      :mass (inc (rand-int (config :mass-classes)))
      :location [(rand-int (qc/width)) (/ (rand-int (qc/height)) 2)]
      :velocity [(config :initial-speed-x) (config :initial-speed-y)]
      :acceleration [(config :initial-acceleration-x) (config :initial-acceleration-y)]
      :color (config :mover-color)})
   (range (config :mover-count))))

(defn apply-force [{:keys [acceleration mass] :as mover} force]
  "takes a mover and a force, applies the force and returns a mover with changed acceleration"
  ; Newton's 2nd law: F = M * A
  ; or A = F / M"
  (let [f (mv/divide force (float mass))
        next-acceleration (mv/add acceleration f)]
    (assoc mover :acceleration next-acceleration)))

(defn apply-gravity [{:keys [mass] :as mover}]
  ; Gravity is scaled by mass here!
  (let [gravity [0 (* 0.1 mass)]]
    (apply-force mover gravity)))

(defn next-motion-state
  "takes a mover and force returns a mover with updated motion-state"
  [{:keys [location velocity acceleration] :as mover}]
  (let [next-location (mv/add location velocity)
        next-velocity (mv/add velocity acceleration)
        next-acceleration (mv/multiply acceleration (float 0))]
    (assoc mover :location next-location :velocity next-velocity :acceleration next-acceleration)))

(defn check-edges [{:keys [location velocity mass] :as mover}]
  (let [[x y] location
        rh (/ (* (config :r-factor) mass) 2)
        y-max (- (qc/height) rh)]
    (if (> y y-max)
      (assoc mover :location [x y-max] :velocity (mv/multiply velocity (float (config :re-bouncing-factor))))
      mover)))

(defn next-mover
  "takes a mover and force returns a mover with updated motion-state and applied force"
  [mover fluid]
  (let [drag-force (drag-force fluid mover)]
    (-> (apply-gravity mover)
        (apply-force drag-force)
        (next-motion-state)
        (check-edges))))

(defn draw-mover
  [{:keys [location mass color] :as mover}]
  (qc/stroke 0)
  (qc/stroke-weight 2)
  (qc/fill color, 200)
  (let [[x y] location]
    (qc/ellipse x y (* mass (config :r-factor)) (* mass (config :r-factor))))
  mover)

;;;
;;; Main
;;;

(def sketch-model
  (atom
   {:fluid nil
    :movers nil}))

(defn init-sketch-model [m-atom]
  (swap!
   m-atom
   (fn [m]
     (-> (assoc-in m [:fluid] (gen-fluid))
         (assoc-in [:movers] (gen-movers))))))

(defn next-movers [fluid movers]
  (map #(next-mover % fluid) movers))

(defn setup-sketch []
  (qc/frame-rate (config :frame-rate))
  (qc/smooth) ; anti aliasing on
  (init-sketch-model sketch-model))

(defn draw-sketch []
  ; draw Background
  (qc/background (config :background))

  ; draw fluid
  (draw-fluid (@sketch-model :fluid))

  ; draw movers
  (dorun (map #(draw-mover %) (@sketch-model :movers)))

  ; draw hint(s)
  (qc/fill 0)
  (qc/text "click mouse to reset" 10 30)

  ; update sketch-model to next state
  (swap!
   sketch-model
   #(update-in
     %
     [:movers]
     (partial next-movers (@sketch-model :fluid)))))

(defn mouse-pressed []
  (swap!
   sketch-model
   #(update-in
     %
     [:movers]
     (constantly (gen-movers)))))
