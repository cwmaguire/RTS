(ns rts.core
  (:use [clojure.contrib.math :only [abs]])
  (:import (javax.swing JFrame JPanel JButton SwingUtilities JTextArea JComponent JScrollPane)
           (java.awt Dimension BorderLayout Color Rectangle)
           (java.awt.geom Point2D$Float Rectangle2D$Float Line2D$Float Ellipse2D$Float)
           (java.awt.event MouseAdapter MouseEvent ActionListener MouseMotionAdapter)
           (java.util.concurrent Executors TimeUnit)))

(def units (atom []))
(def selected-units (atom []))
(def selection (atom nil))
(def unit-moves (atom nil))

(def square-size 6)
(def unit-size (* 3 square-size))
(def draw-grid? true)

(def text-area (new JTextArea))

(defn debug [& strs] (.setText text-area (str (.getText text-area) (apply str strs) "\n")))

;read the comments bottom to top
; I should re-write this to use ->, or even just use more let statements
(defn translate-n [n-source n-dest dist]
  (if (= n-source n-dest)
    n-source
    (+ n-source ; add the neg. or pos. distance to n1
      (* ; multiply the min distance by 1 or -1 to handle converging on n2 from any direction
        (/ ; divide the distance between n-dest and n-source by the abs of itself to get 1 or -1
          (- n-dest n-source)
          (abs (- n-dest n-source)))
        (min ; get the minimum of the distance or 5
          (abs (- n-dest n-source))
          dist)))))

(defn move-unit [unit-move]
  (let [{:keys [unit move] :as unit-move} unit-move
        shape (:shape @unit)
        x-orig (.getX shape)
        y-orig (.getY shape)
        h (.getHeight shape)
        w (.getWidth shape)
        x-move (:x move)
        y-move (:y move)
        new-shape (Rectangle2D$Float. (translate-n x-orig x-move 5) (translate-n y-orig y-move 5) w h)
        x-dest (.getX new-shape)
        y-dest (.getY new-shape)]
    (debug "Moving unit from [" x-orig "," y-orig "] to [" x-dest "," y-dest "]")
    (swap! unit assoc :shape new-shape)
    ; return clipping range to redraw any area where a unit has moved
    {:x (min x-orig x-dest) :y (min y-orig y-dest) :w (+ unit-size (- (max x-orig x-dest) (min x-orig x-dest))) :h (+ unit-size (- (max y-orig y-dest) (min y-orig y-dest)))}
    ))

(defn do-moves []
  "Run each unit move and return a list of the clip regions to repaint"
  (map move-unit @unit-moves))

(defn unit-selected? [unit]
  (some #(= % unit) @selected-units))

(defn draw-unit [g2d unit]
  (let [selected (unit-selected? unit) color (if selected Color/RED Color/BLACK)]
    (.setColor g2d color)
    ;(write-text "Color is" color)
    (.draw g2d (:shape @unit))))


(defn selection-rectangle []
  (let [{:keys [start end]} @selection]
    (if (and start end)
      (let [start-x (.getX start)
            start-y (.getY start)
            end-x (.getX end)
            end-y (.getY end)]
        (Rectangle2D$Float. (min start-x end-x) (min start-y end-y) (abs (- start-x end-x)) (abs (- start-y end-y)))
        ))))

(defn draw-selection [g2d]
    (if-let [rect (selection-rectangle)]
        (do (.setColor g2d Color/BLUE)
            (.draw g2d rect))))

(defn draw-grid [panel g2d]
  (let [clip (.getClip g2d)
        x (.getX clip)
        y (.getY clip)
        w (.getWidth clip)
        h (.getHeight clip)
        max-x (+ x w)
        max-y (+ y h)
        prev-x (- x (mod x square-size))
        next-x (- (+ max-x square-size) (mod (+ max-x square-size) square-size))
        prev-y (- y (mod y square-size))
        next-y (- (+ max-y square-size) (mod (+ max-y square-size) square-size))]
    ; blank it out
    (.setColor g2d Color/WHITE)
    (.fill g2d clip)

    (.setColor g2d Color/GRAY)
    (doseq [line (map #(Line2D$Float. % %2 % %3) (range prev-x next-x square-size) (repeat y) (repeat max-y))]
      (.draw g2d line))
    (doseq [line (map #(Line2D$Float. %2 % %3 %) (range prev-y next-y square-size) (repeat x) (repeat max-x))]
      (.draw g2d line))))

(defn draw-panel-paint [panel g2d]
      (debug "draw-panel-paint with clip: " (.getX (.getClip g2d)) " " (.getY (.getClip g2d)) " " (.getWidth (.getClip g2d)) " " (.getHeight (.getClip g2d)))
      (if draw-grid? (draw-grid panel g2d))

      (.draw g2d (Ellipse2D$Float. (.getX (.getClip g2d)) (.getX (.getClip g2d)) 10 10))

      ;(debug "paint")
      (let [units @units selection @selection sel-start (:start selection) sel-end (:end selection)]
        ;(debug "p[" (count units) "s]")
        (if units
          (doall (map (fn [unit] (draw-unit g2d unit)) units))
        )
        (draw-selection g2d)))

(def draw-panel
  (doto (proxy (JPanel) [] (paintComponent [g2d] (draw-panel-paint this g2d)))
    (.setOpaque true)
    (.setLayout (new BorderLayout))))

(def draw-frame (doto (new JFrame)
  (.setLayout (new BorderLayout))
  (.setPreferredSize (Dimension. 500 500))
  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))

(def debug-panel
  (doto (JPanel.)
    (.setOpaque true)
    (.setLayout (new BorderLayout))
    (.add (JScrollPane. text-area) BorderLayout/CENTER)))

(def debug-frame (doto (new JFrame)
  (.setLayout (new BorderLayout))
  (.setPreferredSize (Dimension. 500 500))))

(defn select-unit [unit ctrl?]
  (if (not ctrl?)
    (swap! selected-units empty))
  (if (unit-selected? unit)
    (swap! selected-units (partial remove #(= unit %)))
    (swap! selected-units conj unit)))

(defn create-unit [mouse-event ctrl?]
  (let [new-unit (atom {:shape (Rectangle. (.getX mouse-event) (.getY mouse-event) unit-size unit-size)})]
    (swap! units conj new-unit)
    (select-unit new-unit ctrl?)))

(defn sel-contains-unit [sel-rect unit]
  (let [unit-rect (.getBounds2D (:shape @unit))]
    (.contains sel-rect unit-rect)))

(defn recalc-selection [ctrl?]
  ; check for Ctrl before clearing selection
  (let [units @units sel-rect (selection-rectangle)]
    (if sel-rect
      (do
        (if (not ctrl?)
          (swap! selected-units empty))
        (swap! selected-units concat (filter (partial sel-contains-unit (selection-rectangle)) units))))))


(defn get-unit [mouse-event]
  (let [p (Point2D$Float. (.getX mouse-event) (.getY mouse-event))]
    (debug (.getButton mouse-event))
    (first (filter #(. (:shape (deref %)) contains p) @units))))

(defn left-mouse [mouse-event]
  (if-let [unit (get-unit mouse-event)]
    (select-unit unit (.isControlDown mouse-event)) ; add check for Ctrl key to add selection instead of replace it
    (create-unit mouse-event (.isControlDown mouse-event)))

  (swap! selection assoc :start nil :end nil))

(defn resolve-to-square [n] (- n (mod n square-size)))

(defn right-mouse [mouse-event]
  ;(debug "resolving: x " (resolve-to-square (.getX mouse-event)) " y " (resolve-to-square (.getY mouse-event)))
  (swap! unit-moves concat (map (fn [unit] {:unit unit :move {:x (resolve-to-square (.getX mouse-event)) :y (resolve-to-square (.getY mouse-event))}}) @selected-units))
  )

(defn xywh
  ([point-one point-two]
  (let [x1 (.getX point-one) x2 (.getX point-two) y1 (.getY point-one) y2 (.getY point-two)]
    [(min x1 x2) (min y1 y2) (- (max x1 x2) (min x1 x2)) (- (max y1 y2) (min y1 y2))]))
  ([selection]
    (xywh (:start selection) (:end selection))))

(def mouse-adapter
  ; switch to reify
  (proxy (MouseAdapter) []
    (mouseClicked [mouse-event]
      (if (= MouseEvent/BUTTON1 (.getButton mouse-event))
        (left-mouse mouse-event)
        (right-mouse mouse-event))
      )

    (mouseReleased [mouse-event]
      (let [old-selection @selection]
      (swap! selection assoc :start nil :end nil)
      (apply #(.repaint draw-frame % %2 %3 %4) (xywh old-selection))))))

(def mouse-motion-adapter
  (proxy (MouseMotionAdapter) []
    (mouseDragged [mouse-event]
      ; store the end of the selection
      (swap! selection assoc :end (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))

      (let [start (:start @selection)]
        (if (nil? start)
          (swap! selection assoc :start (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))))

      (recalc-selection (.isControlDown mouse-event))
      (.repaint draw-frame (.getX (:start @selection)) (.getY (:start @selection)) (.getX (:end @selection)) (.getY (:end @selection))))))

;
; !!! - I don't have any code that intercepts a clipped repaint, so it doesn't do anything to call repaint
;
; I need to override the method and re-draw whatever rooms are in the clipping region
;
(defn draw-moves []
  ;(debug "drawing moves")
  ;(prn "ack")
  (doseq [{:keys [x y w h]} (do-moves)] (debug "redraw clip x: " x " y: " y " w: " w " h: " h)(.repaint draw-panel x y (+ w 1) (+ h 1)) )
  ;remove finished moves
  (swap! unit-moves empty))

(defn run []
  (SwingUtilities/invokeLater (fn [] (do
    (.addMouseListener draw-panel mouse-adapter)
    (.addMouseMotionListener draw-panel mouse-motion-adapter)
    (doto draw-frame
      (.add draw-panel BorderLayout/CENTER)
      .pack
      (.setVisible true))
    (doto debug-frame
      (.add debug-panel BorderLayout/CENTER)
      .pack
      (.setVisible true)))))

    ; wrapping draw moves in an anonymous function lets me update draw-moves on the fly since the thread holds a reference
    ; to the anonymous function, not draw-moves
    ; I'm guessing the anonymous function is a closure and calls the real draw-moves even after I re-def it
    (-> (Executors/newScheduledThreadPool 1) (.scheduleWithFixedDelay (fn [] (draw-moves)) 100 100 TimeUnit/MILLISECONDS))
  )