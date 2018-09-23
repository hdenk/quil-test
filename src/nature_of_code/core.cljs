(ns nature-of-code.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]
    [reagent.core :as r]
    [nature-of-code.vectors.bouncingball-vectors.sketch :as bouncingball-vectors]
    [nature-of-code.forces.fluidresistance.sketch :as fluidresistance]
    [nature-of-code.neural-networks.simple-perceptron.sketch :as simple-perceptron]
    [nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch :as smart-rockets-superbasic]
    [nature-of-code.systems.particlesystem-forces.sketch :as particlesystem-forces]))

(defn canvas [params]
  (r/create-class
    {:component-did-mount
     (fn [component]
       (let [node (r/dom-node component)
             width (.-width node)
             height (.-height node)]
         (q/sketch
           :host node
           :size [width height]
           :setup (params :setup) 
           :draw (params :draw)
           :mouse-pressed (params :mouse-pressed)
           :middleware [m/fun-mode])))
     :render
     (fn []
       [:canvas {:width  (/ (.-innerWidth js/window) 2)
                 :height (/ (.-innerHeight js/window) 2)}])}))

(defn home-page []
  [:div
    (r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Smart Rockets"]
        [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup smart-rockets-superbasic/setup-sketch
                  :draw smart-rockets-superbasic/draw-sketch
                  :mouse-pressed smart-rockets-superbasic/mouse-pressed}])])
    #_(r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Simple Perceptron"]
        [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup simple-perceptron/setup-sketch
                  :draw simple-perceptron/draw-sketch}])])
    #_(r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Bouncingball"]
        [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup bouncingball-vectors/setup-sketch
                  :draw bouncingball-vectors/draw-sketch}])])
    #_(r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Fluid-Resistance"]
       [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup fluidresistance/setup-sketch
                  :draw fluidresistance/draw-sketch}])])
    #_(r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Particle-System"]
       [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup particlesystem-forces/setup-sketch
                  :draw particlesystem-forces/draw-sketch}])])])

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
