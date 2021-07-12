(ns icfpc2021
  (:require [clojure.data.json :as json]
            [clj-http.client :as http])
  (:import [javax.swing JFrame JPanel]
           [java.awt.event MouseMotionListener KeyListener KeyEvent MouseWheelListener]
           (java.awt Graphics RenderingHints Color Font)))

(def api-key (or (System/getenv "API_TOKEN") (throw (Exception. "Missing API_TOKEN"))))

(def scale (atom 5))
(def translate-offset 40)
(def font (Font. "Consolas" Font/TRUETYPE_FONT 16))
(defn extract-mouse-pos [evt]
  (let [x (-> evt .getPoint .-x (- translate-offset) (/ @scale) (double) (Math/round))
        y (-> evt .getPoint .-y (- translate-offset) (/ @scale) (double) (Math/round))]
    [x y]))



(defn fetch-problem [id]
  (println "Loading problem ID" id)
  (->
    (str "https://poses.live/api/problems/" id)
    (http/get {:headers          {:authorization (str "Bearer " api-key)}
               :throw-exceptions true})
    :body
    (json/read-str :key-fn keyword)))

(defn upload-solution [id solution]
  (->
    (str "https://poses.live/api/problems/" id "/solutions")
    (http/post {:headers          {:authorization (str "Bearer " api-key)}
                :body             (json/write-str {"vertices" solution})
                :throw-exceptions false})
    :body
    (json/read-str :key-fn keyword)
    println))

(defn midpoint [x1 x2]
  (int (+ x1 (/ (- x2 x1) 2))))

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))

(defn dislikes [hole pose]
  (apply + (map (fn [h] (apply min (map (fn [v] (distance h v)) pose))) hole)))

(defn canvas [problem-id]
  (let [problem (fetch-problem problem-id)
        *mouse (atom [0 0])
        *solution (atom (-> problem :figure :vertices))
        *selected-vertex-index (atom nil)
        panel (proxy [JPanel MouseMotionListener] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (.translate g translate-offset translate-offset)
                  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                  (.setFont g font)
                  (let [lines (atom 0)
                        draw-text (fn [^String s] (.drawString g s -20 (* @lines 20)) (swap! lines inc))]

                    ;; Hole
                    (doseq [len [(-> problem :hole count)]
                            idx1 (range len)
                            idx2 [(-> idx1 inc (mod len))]]
                      (let [[x1 y1] (-> problem :hole (nth idx1))
                            [x2 y2] (-> problem :hole (nth idx2))]
                        (.drawLine g (* @scale x1) (* @scale y1) (* @scale x2) (* @scale y2))))

                    ;; Figure
                    (.setColor g (Color. 0x11000000 true))
                    (doseq [[idx1 idx2] (-> problem :figure :edges)]
                      (let [[x1 y1] (-> problem :figure :vertices (nth idx1))
                            [x2 y2] (-> problem :figure :vertices (nth idx2))]
                        (.drawLine g (* @scale x1) (* @scale y1) (* @scale x2) (* @scale y2))))

                    ;; Solution
                    (.setColor g Color/RED)
                    (doseq [[idx1 idx2] (-> problem :figure :edges)]
                      (let [[x1 y1] (-> problem :figure :vertices (nth idx1))
                            [x2 y2] (-> problem :figure :vertices (nth idx2))
                            [x1' y1'] (-> @*solution (nth idx1))
                            [x2' y2'] (-> @*solution (nth idx2))
                            dist (distance [x1 y1] [x2 y2])
                            dist' (distance [x1' y1'] [x2' y2'])
                            ratio (- (/ dist' dist) 1)
                            valid? (<= (Math/abs ratio) (/ (:epsilon problem) 1000000))]
                        (.setColor g (if valid? Color/GREEN Color/RED))
                        (.drawLine g (* @scale x1') (* @scale y1') (* @scale x2') (* @scale y2'))
                        (if (not valid?)
                          (.drawString g (str (Math/round (* 100 ratio)) "%") (* @scale (midpoint x1' x2')) (* @scale (midpoint y1' y2'))))))

                    ;; Selection
                    (.setColor g (Color. 0x66FF0000 true))
                    (let [index @*selected-vertex-index]
                      (when index
                        (let [[vx vy] (nth @*solution index)]
                          (.fillOval g (- (* @scale vx) 5) (- (* @scale vy) 5) 10 10))
                        (.setColor g (Color. 0x3333AA66 true))
                        (doseq [neighbor (->> problem :figure :edges (filter #(some #{index} %)) flatten (remove #{index}))]
                          (let [[vx vy] (get-in @*solution [neighbor])
                                [ox oy] (get-in problem [:figure :vertices index])
                                [nx ny] (get-in problem [:figure :vertices neighbor])
                                radius (Math/sqrt (distance [ox oy] [nx ny]))]
                            (.drawOval g (* @scale (- vx radius)) (* @scale (- vy radius)) (* @scale radius 2) (* @scale radius 2)))
                          )))

                    ;; Info
                    (.setColor g (Color/BLACK))
                    (draw-text (str "Eps. " (/ (:epsilon problem) 1000000.0)))
                    (draw-text (str "Dislikes: " (dislikes (:hole problem) @*solution))))
                  ))]

    (.addMouseMotionListener
      panel
      (reify MouseMotionListener
        (mouseMoved [this evt]
          (let [[x y] (extract-mouse-pos evt)
                distance (fn [[index [vx vy]]] (+ (Math/pow (- x vx) 2) (Math/pow (- y vy) 2)))
                closest (apply min-key distance (map-indexed (fn [idx itm] [idx itm]) @*solution))]
            (reset! *mouse [x y])
            (reset! *selected-vertex-index (if (< (distance closest) 30) (first closest))))
          (.repaint panel))

        (mouseDragged [this evt]
          (let [[x y] (extract-mouse-pos evt)
                shift? (.isShiftDown evt)
                index @*selected-vertex-index]
            (cond
              shift? (do
                       (reset! *solution (into [] (map (fn [[vx vy]] (let [[mx my] @*mouse
                                                                           [dx dy] [(- x mx) (- y my)]]
                                                                       [(+ vx dx) (+ vy dy)])) @*solution)))
                       (.repaint panel))
              index (do
                      (swap! *solution assoc index [x y])
                      (.repaint panel)))
            (reset! *mouse [x y])))))

    (.addMouseWheelListener
      panel
      (reify MouseWheelListener
        (mouseWheelMoved [this evt]
          (let [delta (.getWheelRotation evt)]
            (swap! scale #(+ % delta))
            (.repaint panel)))))

    (.setFocusable panel true)
    (.requestFocusInWindow panel)
    (.addKeyListener
      panel
      (reify KeyListener
        (keyReleased [this evt]
          (let [code (.getKeyCode evt)]
            (cond
              (= code (KeyEvent/VK_U)) (upload-solution problem-id @*solution)
              (= code (KeyEvent/VK_LEFT)) (println "previous"))))
        (keyPressed [this evt])
        (keyTyped [this evt])))

    panel))

(defn -main [& args]
  (let [frame (JFrame. "ICFPC EDITOR")]
    (doto frame
      (.add (canvas 22))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setBounds 100 100 600 600)
      (.setVisible true))))
