(ns nature-of-code.vectors.bouncingball-vectors.spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::2d-vector (s/tuple float? float?))

;;
;; config
;;

(s/def ::size (s/tuple int? int?))
(s/def ::background int?)
(s/def ::frame-rate int?)
(s/def ::ball-x int?)
(s/def ::ball-y int?)
(s/def ::ball-r int?)
(s/def ::speed-x float?)
(s/def ::speed-y float?)
(s/def ::damping-factor float?)
(s/def ::check-spec boolean?)

(s/def ::config (s/keys :req-un [::size ::background ::frame-rate ::ball-x ::ball-y ::ball-r ::speed-x ::speed-y ::damping-factor ::check-spec]))

(defn check-config [config]
    (when-not (s/valid? ::config config)
          (throw (js/Error. (s/explain-str ::config config))))
    config)

;;
;; ball
;;

(s/def ::location ::2d-vector)
(s/def ::velocity ::2d-vector)

(s/def ::ball (s/keys :req-un [::location ::velocity]))

(defn check-ball [ball]
    (when-not (s/valid? ::ball ball)
          (throw (js/Error. (s/explain-str ::ball ball))))
    ball)
