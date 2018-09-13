(ns nature-of-code.vectors.bouncingball-vectors.sketch
  "Example 1-2: Bouncing Ball, with PVector!
	 Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [nature-of-code.math.vector :as mv]
            [nature-of-code.vectors.bouncingball-vectors.spec :as spec]))

;;
;; config
;;

(defn gen-config [m]
  (when (m :check-spec)
    (spec/check-config m))
  m)

(def config (gen-config {:size [200 200]
                         :background 255
                         :frame-rate 30
                         :ball-x 100
                         :ball-y 100
                         :ball-r 16
                         :speed-x 2.5
                         :speed-y 5
                         :damping-factor -0.9
                         :check-spec true}))

;;
;; ball
;;

(defn gen-ball [m]
  (when (config :check-spec)
    (spec/check-ball m))
  m)

(defn check-edges [{:keys [location velocity] :as ball}]
  (let [location-x (first location)
        location-y (second location)
        next-velocity (-> velocity
                          (#(let [velocity-x (first %)]
                              (if (or
                                   (and (> location-x (q/width)) (> velocity-x 0))
                                   (and (< location-x 0) (< velocity-x 0)))
                                (assoc velocity 0 (* velocity-x (config :damping-factor)))
                                %)))
                          (#(let [velocity-y (second %)]
                              (if (or
                                   (and (> location-y (q/height)) (> velocity-y 0))
                                   (and (< location-y 0) (< velocity-y 0)))
                                (assoc velocity 1 (* velocity-y (config :damping-factor)))
                                %))))]
    (assoc ball :velocity next-velocity)))

(defn move [{:keys [location velocity] :as ball}]
  (let [next-location (mv/add location velocity)]
    (assoc ball :location next-location)))

;;
;; sketch
;;

(def ball
  (let [location [(config :ball-x) (config :ball-y)]
        velocity [(config :speed-x) (config :speed-y)]]
    (atom (gen-ball {:location location :velocity velocity}))))

(defn setup-sketch []
  (js/console.log "setup-sketch")
  (q/frame-rate (config :frame-rate))
  (q/background (config :background))
  (q/smooth))

(defn draw-sketch []
  (q/no-stroke)
  (q/fill 255 10)
  (q/rect 0 0 (q/width) (q/height))

  ; Update ball
  (let [next-ball (-> @ball
                      (check-edges)
                      (move))]
    (swap! ball (fn [b n] n) next-ball))

  ; Display circle at ball location
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (first (:location @ball)) (second (:location @ball)) (config :ball-r) (config :ball-r)))

; This sketch uses functional-mode middleware.
; Check quil wiki for more info about middlewares and particularly
; fun-mode.
(q/defsketch bouncing-ball
  :host "sketch"
  :size (config :size)
  :setup setup-sketch
  :draw draw-sketch
  :middleware [m/fun-mode])
