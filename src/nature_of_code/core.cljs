(ns nature-of-code.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]
    [reagent.core :as r]
    [nature-of-code.vectors.bouncingball-vectors.sketch :as bouncingball-vectors.sketch]
    [nature-of-code.forces.fluidresistance.sketch :as fluidresistance.sketch]
    [nature-of-code.systems.particlesystem-forces.sketch :as particlesystem-forces.sketch]))

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
           :middleware [m/fun-mode])))
     :render
     (fn []
       [:canvas {:width  (/ (.-innerWidth js/window) 2)
                 :height (/ (.-innerHeight js/window) 2)}])}))

(defn home-page []
  [:div
    (r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Bouncingball"]
        [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup bouncingball-vectors.sketch/setup-sketch
                  :draw bouncingball-vectors.sketch/draw-sketch}])])
    (r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Fluid-Resistance"]
       [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup fluidresistance.sketch/setup-sketch
                  :draw fluidresistance.sketch/draw-sketch}])])
    (r/with-let [running? (r/atom false)]
      [:div
       [:h3 "Particle-System"]
       [:div>button
        {:on-click #(swap! running? not)}
        (if @running? "stop" "start")]
       (when @running?
         [canvas {:setup particlesystem-forces.sketch/setup-sketch
                  :draw particlesystem-forces.sketch/draw-sketch}])])])

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
