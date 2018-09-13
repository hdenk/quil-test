(ns nature-of-code.systems.particlesystem-forces.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::2d-vector (s/tuple float? float?))

;;
;; config
;;

(s/def ::background int?)
(s/def ::frame-rate int?)
(s/def ::gravity ::2d-vector)
(s/def ::lifespan int?)
(s/def ::lifespan-dec-rate float?)
(s/def ::circle-r int?)
(s/def ::square-l int?)
(s/def ::particle-color int?)
(s/def ::particle-outline-thickness int?)
(s/def ::particle-shapes (s/* keyword?))
(s/def ::check-spec boolean?)

(s/def ::config (s/keys :req-un [::background ::frame-rate ::gravity ::lifespan ::lifespan-dec-rate ::circle-r ::square-l ::particle-color ::particle-outline-thickness ::particle-shapes ::check-spec]))

(defn check-config [config]
  (when-not (s/valid? ::config config)
    (throw (js/Error. (s/explain-str ::config config))))
  config)

;;
;; particle
;;

(s/def ::id int?)
(s/def ::shape keyword?)
(s/def ::mass float?)
(s/def ::location ::2d-vector)
(s/def ::velocity ::2d-vector)
(s/def ::acceleration ::2d-vector)
(s/def ::lifespan int?)

(s/def ::particle (s/keys :req-un [::id ::shape ::mass ::location ::velocity ::acceleration ::lifespan]))

(defn check-particle [particle]
  (when-not (s/valid? ::particle particle)
    (throw (js/Error. (s/explain-str ::particle particle))))
  particle)

;;
;; particle-system
;;

(s/def ::origin ::2d-vector)
(s/def ::gravity ::2d-vector)
(s/def ::particles (s/* ::particle))

(s/def ::particle-system (s/keys :req-un [::origin ::gravity ::particles]))

(defn check-particle-system [particle-system]
  (when-not (s/valid? ::particle-system particle-system)
    (throw (js/Error. (s/explain-str ::particle-system particle-system))))
  particle-system)
