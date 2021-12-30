(ns default.core
  (:require [quil.core :as q])
  (:require [default.d :as d]))

(declare default)
(q/defsketch default
  :title "Inauguration"
  :size [1600 900]

  :setup d/setup
  :draw d/draw
  ;:features [:keep-on-top]
  )

(defn refresh []
  (use :reload 'default.d)
  (.loop default))
