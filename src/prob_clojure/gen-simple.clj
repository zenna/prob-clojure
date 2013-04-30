(defn conv-cart-polar
  "Convert a point to polar coordinates around a pole"
  [pole point]
  (let [trans-point (vec-f - point pole)
        x (first trans-point)
        y (second trans-point)
        r (Math/sqrt (+ (* x x) (* y y)))
        theta (Math/atan2 y x)]
    [r theta]))

(defn rosetta-compare [s1 s2]
  (let [len1 (count s1) len2 (count s2)]
    (if (= len1 len2)
      (compare (.toLowerCase s1) (.toLowerCase s2))
      (- len2 len1))))

(def c-comp (comparator b-comp))

(defn extract
  "For list of maps, extract a key"
  [coll keyfn]
  (map (fn [m] (keyfn m)) coll))

(defn convex-hull-gf
  "Find convex hull of points with gift-wrapping algorithm"
  [points]
  (cond
    (<= (count points) 3)
    points

    :else
    (let [bottom-left-comp (comparator (fn [a b]
                                        (let [xa (first a)
                                              ya (second a)
                                              xb (first b)
                                              yb (second b)]
                                        (cond
                                          (< ya yb) true
                                          (and (= ya yb) (< xa xb)) true
                                          :else false))))
          sorted-points (sort bottom-left-comp points)
          pvar (println "Sorted:" sorted-points)
          initial-point (first sorted-points) ; start with bottom leftmost point


          ; There are two criteria for selecting a new point, before we reach the apex
          smallest-pos-val (fn [polar-coords] (let [pos-points (filter #(pos? (second %)) polar-coords)]
                                                (if (empty? pos-points)
                                                    nil
                                                    (first (sort-by second > pos-points)))))
          ; And after reaching the apex
          largest-neg-val (fn [polar-coords] (let [neg-points (filter #(neg? (second %)) polar-coords)]
                                                (if (empty? neg-points)
                                                    nil
                                                    (first (sort-by second < neg-points)))))]

      (loop [point initial-point convex-set [initial-point] find-point smallest-pos-val]
        (let [cart-to-polar (reduce merge
                                    (map (fn [point] {(conv-cart-polar initial-point point) point})
                                         (remove #(= initial-point %) sorted-points)))
              pvar (println "cart-to-polar:" cart-to-polar)
              polar-angles (extract cart-to-polar key)
              pvar (println "Polar Angles:" polar-angles)
              candidate-next-point (find-point polar-angles)
              pvar (println "Candidate Next Point:" candidate-next-point)
              find-point (if (nil? candidate-next-point) largest-neg-val smallest-pos-val)
              next-point-polar (if (nil? candidate-next-point) (find-point polar-angles) candidate-next-point)
              next-point (cart-to-polar next-point-polar)
              pvar (println "Actual Next Point:" next-point)]
          (cond
            (= initial-point next-point) convex-set

            :else
            (recur next-point (conj convex-set next-point) find-point)))))))
 
(defn sample-wo-replacement
  "Select n unique points"
  {:pre [(>= (count coll) n)]}
  [coll n])

(defn vec-f
    "Like merge-with but for vectors"
  [f v1 v2]
  (loop [index 0 merged-vec []]
    (if (= index (count v1))
      merged-vec
      (recur (inc index) (conj merged-vec (f (nth v1 index) (nth v2 index)))))))

(defn winding-num
  "Return winding number of polygon
  see Alciatore "
  [poly point]
        ; translate poly such that point is at origin
  (let [translated-poly (map #(vec-f - % point) poly)]
    ; w is wind-num
    (loop [vertices translated-poly w 0]
      (cond
        (= (count vertices) 1)
        w

        :else
        (let [x1 (first (first vertices))
              x2 (first (second vertices))
              y1 (second (first vertices))
              y2 (second (second vertices))]
          (cond 
            (and (< (* y1 y2) 0)
                 (> (+ x1 (/ (* y1 (- x2 x1))
                         (- y1 y2)))
                    0))
            (if (< y1 0)
                (recur (rest vertices) (inc w))
                (recur (rest vertices) (dec w)))

            (and (zero? y1)
                 (> x1 0))
            (if (> y2 0)
                (recur (rest vertices) (+ w 0.5))
                (recur (rest vertices) (- w 0.5)))

            (and (zero? y2)
                 (> x2 0))
            (if (< y1 0)
                 (recur (rest vertices) (+ w 0.5))
                 (recur (rest vertices) (- w 0.5)))

            :else
            (recur (rest vertices) w)))))))

(defn point-out-poly?
  "is the point in the poly?
  True when the winding num is zero"
  [poly point]
  (zero? (winding-num poly point)))

(defn point-in-poly?
  "is the point outside the polygon?"
  [poly point]
  (not (point-out-poly poly point)))

; What about
(defn set-/)

(defn try-until
  "Keep doing f until pred f"
  [f pred])

; TODO IMPORT THIS FROM HELPERS
(defn gen-until [f p]
  (let [x (f)]
    (if (p x) x (recur f p))))

(fn [poly] (every? #(outside? (convex-hull poly) %)
                                                    (set- points poly))

(defn gen-simple-poly
  "Generate simple polygon in n-dims dimensions"
  [n-dims]
  (let [n-points (rand-int 10)
        ; TODO: test for colinearity
        points (repeatedly n-points #(repeatedly n-dims rand))
        initial-poly (gen-until (fn [poly] (every? #(point-out-poly? (convex-hull poly) %)
                                                    (set- points poly)))
                                #(uniform-select-n points 3))]
  (loop [poly initial-poly]
    (cond
      (= (count poly) n-points) poly ;no more points to add
      
      :else
      (let [ss (filter (fn [point] (every? #(point-out-poly? (convex-hull (set-u poly point)) %)
                                                      (set- points (set-u poly point))))
                       (set- points poly))
            s (random-nth s)
            ;2. Find completely visible edge
            ]
        (recur exp-poly))))))