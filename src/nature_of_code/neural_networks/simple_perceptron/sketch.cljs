(ns nature-of-code.neural-networks.simple-perceptron.sketch
  "Neural Networking - A Simple Perceptron learns to classify 2D-Points
  according to a linear Function
  Based on the Nature of Code by Daniel Shiffman http://natureofcode.com"
  (:require [quil.core :as q :include-macros true]
            [goog.string :as gstring]
            [goog.string.format :as gformat]))

;;
;; config
;;

(def config
  {:background 255
   :frame-rate 30
   :x-min -400
   :x-max 400
   :y-min -100
   :y-max 100
   :point-r 8
   :inputs-per-neuron 3
   :learning-rate 0.00001
   :training-record-count 2000
   :f-line-color 127
   :n-line-color 0})

(def reciprocal (partial / 1))

(def epow (partial Math.pow Math/E))

(def sigmoid (comp reciprocal inc epow -))

;;
;; Perceptron
;;

(defn gen-perceptron
  [& {:keys [weights learning-rate] :as m}]
  m)

(defn f [x]
  "The function that describes a line"
  (+ (* 0.4 x) 1))

(defn random [min max]
  (+ min (rand (- max min))))

(defn random-weights [weights-count]
  (into [] (take weights-count (repeatedly #(random -1 1)))))

; Aktivierungsfunktion als Mutimethod -> Mehrere zur Auswahl
(defmulti activate (fn [key sum] key))

(defmethod activate :simple [key sum]
  "simple activate-function"  
  (if (> sum  0)
    1
    -1))

(defmethod activate :sigmoid [key sum]
  "sigmoid activate-function (logistic function)"
  (- 0.5 (sigmoid sum)))

(defn feed-forward [perceptron inputs]
  (let [sum (reduce
             +
             (map * inputs (:weights perceptron)))]
    (activate :simple sum)))

(defn train [perceptron inputs desired]
  ; guess the result (0, -2, or 2) and mult by learning-rate
  (let [guess (feed-forward perceptron inputs)
        error (- desired guess)
        next-weights (into []
                           (map
                            (fn [weight input] (+ weight (* input error (config :learning-rate))))
                            (:weights perceptron)
                            inputs))]
    ;;(js/console.log (str "error: " error " weights: " next-weights))
    (assoc perceptron :weights next-weights)))

;;
;; TrainingRecord
;;

(defn gen-training-record
  [answer & inputs]
  {:inputs (vec inputs) :answer answer})

(defn gen-training-data [training-record-count]
  (into []
        (take training-record-count
              (repeatedly
               #(let [x (random (config :x-min) (config :x-max))
                      y (random (config :x-min) (config :x-max))
                      answer (if (< y (f x)) -1 1)]
                  (gen-training-record answer x y 1))))))

;;
;; Sketch
;;

(def sketch-model
  (atom {:training-data nil
         :training-index 0
         :perceptron nil}))

(defn init-sketch-model [m-atom]
  (swap! m-atom #(assoc % :training-data (gen-training-data (config :training-record-count))))
  (let [weights (random-weights (config :inputs-per-neuron))]
    (swap! m-atom #(assoc % :perceptron (gen-perceptron :weights weights :learning-rate (config :learning-rate))))))

(defn setup-sketch []
  (js/console.log (str "setup-sketch " (q/width) " " (q/height)))
  (q/frame-rate (config :frame-rate))
  (q/smooth)
  (init-sketch-model sketch-model))

(defn draw-sketch []
  ; draw Background
  (q/background (config :background))
  (q/translate (/ (q/width) 2) (/ (q/height) 2))

  ; Draw the line based on f(x)
  (q/stroke (config :n-line-color))
  (q/stroke-weight 4)
  (let [x1 (config :x-min)
        y1 (f  x1)
        x2 (config :x-max)
        y2 (f x2)]
    (q/line x1 y1 x2 y2))

  ; Draw the line based on the current weights
  ; Formula is weights[0]*x + weights[1]*y + weights[2] = 0
  (q/stroke (config :n-line-color))
  (q/stroke-weight 1)
  (let [perceptron (:perceptron @sketch-model)
        weights (:weights perceptron)
        x1 (config :x-min)
        y1 (- (- (nth weights 2)) (/ (* (nth weights 0) x1) (nth weights 1)))
        x2 (config :x-max)
        y2 (- (- (nth weights 2)) (/ (* (nth weights 0) x2) (nth weights 1)))]
    (q/line x1 y1 x2 y2))

  (let [perceptron (:perceptron @sketch-model)
        training-data (:training-data @sketch-model)
        training-index (:training-index @sketch-model)]
    ; Train the Perceptron with one "training" point at a time
    (let [inputs (:inputs (nth training-data training-index))
          answer (:answer (nth training-data training-index))
          next-perceptron (train perceptron inputs answer)
          next-training-index (mod (inc training-index) (count training-data))]
      (swap! sketch-model #(assoc % :perceptron next-perceptron :training-index next-training-index)))

    ; Draw all the points based on what the Perceptron would "guess"
    ; Does not use the "known" correct answer
    (loop [index 0]
      (q/stroke 0)
      (q/stroke-weight 1)
      (q/fill 0)

      (let [training-record (nth training-data index)
            inputs (:inputs training-record)
            x (nth inputs 0)
            y (nth inputs 1)
            guess (feed-forward perceptron inputs)]
        ;;(js/console.log (str "guess: " guess))
        (when (> guess 0)
          (q/no-fill))
        (q/ellipse x y (config :point-r) (config :point-r)))
      (when (< index training-index)
        (recur (inc index))))

    ; Display some info (text)
    (q/translate (/ (- (q/width)) 2) (/ (+ (q/height)) 2))
    (q/fill 127)
    (let [[w0 w1 w2] (:weights perceptron)
          x (/ w0 w1)
          c (/ w2 w1)]
      ;;(q/text (str training-index x c) 10 -18))))
      (q/text (str training-index (gstring/format " y = %.3f * x + %.3f" x c)) 10 -18))))
