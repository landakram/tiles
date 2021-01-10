(ns tiles.core
  (:require [quil.core :refer :all]
            #_[plotter-utils.grid :refer :all]
            [plotter-utils.quil :as pl]
            [data.grid2d :as grid]
            [quil.helpers.calc :refer [mul-add]]
            [clojure.pprint :refer [pprint]]
            [quil.middleware :as m]))

(def println pprint)

(defn make-grid [origin width height tile-width tile-height x-start y-start]
  (let [columns (/ width tile-width)
        rows (/ height tile-height)]
    {:g (grid/create-grid columns rows
                          (fn [[x y]]
                            {}))
     :width width
     :height height
     :tile-width tile-width
     :tile-height tile-height
     :origin origin}))

(defn initial-state []
  {})

(defn setup []
  (frame-rate 1)
  (color-mode :hsb)
  (initial-state))

(defn update-state [state]
  state)


(defn random-point-in [start-x start-y end-x end-y]
  [(random start-x end-x)
   (random start-y end-y)])

(defn -x [point]
  (first point))

(defn -y [point]
  (second point))

(def debug? (atom false))
(def type (atom :bezier))

(def bg (atom [0 150 200]))
(def color-1 (atom [200 50 200]))
(def color-2 (atom @color-1))

(defn draw-shape [side-1 side-2 [start-x start-y end-x end-y]]
  (let [c1 (random-point-in start-x start-y end-x end-y)
        c2 (random-point-in start-x start-y end-x end-y)]
    (begin-shape)

    (let [[p2 p1] side-1]
      (vertex (-x p1) (-y p1))
      (vertex (-x p2) (-y p2))
      )

    (when @debug?
      (stroke 0 0 0))

    (let [[p1 _] side-1
          [_ p2] side-2
          ;; c1 (random-point-in start-x start-y end-x end-y)
          ;; c2 (random-point-in start-x start-y end-x end-y)
          ]

      (condp = @type
        :lines
        (vertex (-x p1) (-y p1)
                (-x p2) (-y p2))

        :curve
        (do 
          (curve-vertex (-x p1) (-y p1))
          (curve-vertex (-x p1) (-y p1))
          (curve-vertex (-x c1) (-y c1))
          (curve-vertex (-x c2) (-y c2))
          (curve-vertex (-x p2) (-y p2)))

        :bezier
        (bezier-vertex 
         (-x c1) (-y c1)
         (-x c2) (-y c2)
         (-x p2) (-y p2))))

    (let [[p2 p1] side-2]
      (vertex (-x p1) (-y p1))
      (vertex (-x p2) (-y p2)))

    (let [[s1p1 s1p2] side-1
          [s2p1 s2p2] side-2
          p1 s2p1
          p2 s1p2
          mx1 (/ (+ (-x s1p1) (-x s2p2)) 2)
          my1 (/ (+ (-y s1p1) (-y s2p2)) 2)

          mx2 (/ (+ (-x s1p2) (-x s2p1)) 2)
          my2 (/ (+ (-y s1p2) (-y s2p1)) 2)
          
          c1 [(+ (-x c1)
                 (- mx2  mx1))
              (+ (-y c1)
                 (- my2 my1))]
          c2 [(+ (-x c2)
                 (- mx2 mx1))
              (+ (-y c2)
                 (- my2 my1))]
          ;; c1 (random-point-in start-x start-y end-x end-y)
          ;; c2 (random-point-in start-x start-y end-x end-y)
          ]

      (condp = @type
        :lines
        (vertex (-x p1) (-y p1)
                (-x p2) (-y p2))

        :curve
        (do
          (curve-vertex (-x p1) (-y p1))
          (curve-vertex (-x p1) (-y p1))
          (curve-vertex (-x c2) (-y c2))
          (curve-vertex (-x c1) (-y c1))
          (curve-vertex (-x p2) (-y p2)))

        :bezier
        (bezier-vertex 
         (-x c2) (-y c2)
         (-x c1) (-y c1)
         (-x p2) (-y p2)))

      (end-shape)

      (when @debug?
        (fill 0 0 0)

        (text "m1" mx1 my1)
        (text "m2" mx2 my2)

        (text "side-2" (-x (first side-2)) (-y (first side-2)))

        (text "new c1" (-x c1) (-y c1))
        (ellipse (-x c1) (-y c1) 8 8)
        (text "new c2" (-x c2) (-y c2))
        (ellipse (-x c2) (-y c2) 8 8))
      )

    (when @debug?
      (text "c1" (-x c1) (-y c1))
      (text "c2" (-x c2) (-y c2))
      (ellipse (-x c1) (-y c1) 8 8)
      (ellipse (-x c2) (-y c2) 8 8))

      ))

(defn draw-tile [start-x start-y end-x end-y]
  (let [tile-width (- end-x start-x)
        tile-height (- end-y start-y)

        one-third-w (* tile-width (/ 1 3))
        two-third-w (* tile-width (/ 2 3))
        one-third-h (* tile-height (/ 1 3))
        two-third-h (* tile-height (/ 2 3))

        one-third-x (+ start-x one-third-w)
        two-third-x (+ start-x two-third-w)
        one-third-y (+ start-y one-third-h)
        two-third-y (+ start-y two-third-h)

        point-1 [one-third-x start-y]
        point-2 [two-third-x start-y]
        point-3 [end-x one-third-y]
        point-4 [end-x two-third-y]
        point-5 [two-third-x end-y]
        point-6 [one-third-x end-y]
        point-7 [start-x two-third-y]
        point-8 [start-x one-third-y]

        top-side [point-1 point-2]
        right-side [point-3 point-4]
        bottom-side [point-5 point-6]
        left-side [point-7 point-8]

        sides [top-side right-side bottom-side left-side]
        ]
    ;; (no-fill)
    ;; (no-stroke)
    ;; (rect start-x start-y tile-width tile-height)

    (let [[side-1 side-2 side-3 side-4] (shuffle sides)
          bounds [start-x start-y end-x end-y]
          color-1 @color-1
          color-2 @color-2
          ]
      (apply fill color-1)
      (apply stroke color-1)
      (draw-shape side-1 side-2 bounds)

      (apply fill color-2)
      (apply stroke color-2)
      (draw-shape side-3 side-4 bounds)
      )))

(defn draw-state [state]
  ;; (background 31 31 20)
  (apply background @bg)

  (let [tiles-in-row 25
        tiles-in-col 25
        start-x 0
        start-y 0
        end-x 1200
        end-y 1200
        width (- end-x start-x)
        height (- end-y start-y)
        tile-width (/ width tiles-in-row)
        tile-height (/ height tiles-in-col)

        grid (grid/create-grid tiles-in-row tiles-in-col (fn [[x y]]
                                                           [(+ start-x (* x tile-width))
                                                            (+ start-y (* y tile-height))]))
        ]

    (doseq [[x y] (grid/cells grid)]
      (draw-tile x y (+ x tile-width) (+ y tile-height))

      ;; (stroke 0 100 100)
      ;; (no-fill)
      ;; (rect x y tile-width tile-height)
      )


    (no-loop)))

#_(reset! type :lines)
#_(reset! type :curve)
(reset! type :bezier)
(defsketch tiles
  :title "tiles"
  :renderer :svg
  :output-file "generated/tiles.svg"
  :settings
  (fn []
    (smooth 2))
  :size [600 600]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
