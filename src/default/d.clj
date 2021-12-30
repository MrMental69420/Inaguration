(ns default.d
  (:require [quil.core :as q])
  (:require [incanter.interpolation :as interpol]))

(defn setup []
  (q/background 0))

(defn ema3 [c a]
  (reduce (fn [res ct]
            (conj
             res
             (+ (* a ct)
                (* (- 1 a) (peek res)))))
          [(first c)]
          (rest c)))

(defn gen-shifts2 [max-shift count]
  (let [raw-shifts (for [_ (range count)]
                     (q/map-range (q/random-gaussian) -3 3 (* -1 max-shift) max-shift))
        mean-shifts (ema3 raw-shifts 0.3)]
    mean-shifts))

(defn generate-row [y min-x max-x cols max-shift]
  (let [width (- max-x min-x)
        x-step (/ width (dec cols))
        shifts (gen-shifts2 max-shift cols)]
    (for [i (range cols)]
      (list (+ (* i x-step) (nth shifts i)) y))))

(defn generate-rows [min-x max-x min-y max-y cols rows max-shift]
  (let [height (- max-y min-y)
        y-step (/ height (dec rows))]
    (for [i (range rows)]
      (generate-row (* i y-step) min-x max-x cols max-shift))))

(defn transpose [grid]
  (let [cols (count (first grid))
        rows (count grid)]
    (for [j (range cols)]
      (for [i (range rows)] 
        (nth (nth grid i) j)))))

(defn draw []
  (q/ellipse-mode :center)
  (q/rect-mode :center)
  (q/stroke 255)
  (q/stroke-weight 5)
  (let [grid (transpose (generate-rows 0 (q/width) 0 (q/height) 40 4 400))
        grid-fn (for [column grid]
                  (list (interpol/interpolate (for [row column] (list (last row) (first row))) :cubic)
                        (last (first column))
                        (last (last column))))
        pallete-1 [[126 156 167]
                   [193 184 169]
                   [222 212 197]
                   [7 35 73]
                   [69 74 76]]
        pallete-2 [[44 54 70]
                   [124 132 145]
                   [214 225 229]
                   [245 226 208]
                   [155 136 124]]
        pallete-3 [[127 91 90]
                   [169 100 108]
                   [219 181 187]
                   [231 200 186]
                   [149 137 71]]
        pallete-4 [[219 176 160]
                 [224 194 192]
                 [234 181 134]
                 [178 72 27]
                 [87 57 73]]
        pallete-5 [[153 153 142]
                 [203 183 150]
                 [248 167 63]
                 [219 92 1]
                 [26 22 18]]
        pallete-6 [[228 234 234]
                 [198 178 155]
                 [174 98 31]
                 [58 54 8]
                 [19 26 19]]
        pallete [[68 65 59]
                 [173 154 142]
                 [217 204 193]
                 [235 198 134]
                 [253 147 52]]
        steps 20]
    (apply q/background (rand-nth pallete))
    (dotimes [_ 4]
      (let [radius (q/map-range (rand) 0 1 200 600)
            x (rand-int (q/width))
            y (rand-int (q/height))]
        (doseq [rad (range radius (/ radius 3) -10)]
          (q/fill 0 0 0 0)
          (apply q/stroke (rand-nth pallete))
          (q/ellipse x y rad rad))))
    (q/stroke-weight 2)
    (doseq [[x-fn min-y max-y] grid-fn]
      (if (> (rand) 0.6)
        (doseq [i (range 0 (q/height) steps)]
          (q/with-translation [(x-fn i) i]
            (q/with-rotation [(+ (q/radians 90) (q/atan2 (* 2 steps) (- (x-fn (+ steps i)) (x-fn (- i steps)))))]
              (dotimes [_ (q/map-range (q/random-gaussian) -3 3 1 6)]
                (apply q/fill (concat (rand-nth pallete) (list (q/map-range (q/random-gaussian) -3 3 100 255))))
                (apply q/stroke (rand-nth pallete))
                (q/rect (q/map-range (rand) 0 1 -20 20)
                        (q/map-range (rand) 0 1 -20 20) 15 (q/map-range (q/random-gaussian) -3 3 30 60))))))

        (q/with-fill [0 0 0 0] (q/begin-shape)
          (doseq [i (range 0 (+ 10 (q/height)) steps)]
            (q/vertex (x-fn i) i))
          (q/end-shape)))))

  ;(q/no-loop)
  (q/save (str "gen/inaguration-" (q/frame-count) ".png"))
  (println (q/frame-count)))