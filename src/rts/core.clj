(ns rts.core
  (:use [clojure.contrib.math :only [abs]])
  (:import (javax.swing JFrame JPanel JButton SwingUtilities JTextField JComponent)
           (java.awt Dimension BorderLayout Color Rectangle)
           (java.awt.geom Point2D$Float Rectangle2D$Float)
           (java.awt.event MouseAdapter MouseEvent ActionListener MouseMotionAdapter)))


(def units (atom []))

(def selected-units (atom []))

(def selection (atom nil))

(def text-field (new JTextField))

(defn write-text [& strs] (.setText text-field (str (.getText text-field) (apply str strs))))

(defn draw-unit [g2d unit]
  (let [selected (some #(= % unit) @selected-units) color (if selected Color/RED Color/BLACK)]
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

(def panel (doto
  (proxy (JPanel) []
    (paintComponent [g2d]
      (let [units @units selection @selection sel-start (:start selection) sel-end (:end selection)]
        (write-text "p[" (count units) "s]")
        (if units
          (doall (map (fn [unit] (draw-unit g2d unit)) units))
        )
        (draw-selection g2d)
        )))

  (.setOpaque true)
  (.setLayout (new BorderLayout))
  (.add text-field BorderLayout/NORTH)))

(def frame (doto (new JFrame)
  (.setLayout (new BorderLayout))
  (.setPreferredSize (Dimension. 500 500))
  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))

(defn select-unit [unit ctrl?]
  (if (not ctrl?)
    (swap! selected-units empty))
  (swap! selected-units conj unit))

(defn create-unit [mouse-event ctrl?]
  (let [new-unit (atom {:shape (Rectangle. (.getX mouse-event) (.getY mouse-event) 10 10)})]
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
    (first (filter #(. (:shape (deref %)) contains p) @units))))

(def mouse-adapter
  (proxy (MouseAdapter) []
    (mouseClicked [mouse-event]
      (if-let [unit (get-unit mouse-event)]
        (select-unit unit (.isControlDown mouse-event)) ; add check for Ctrl key to add selection instead of replace it
        (create-unit mouse-event (.isControlDown mouse-event)))

      (swap! selection assoc :start nil :end nil)

      ; repaint source instead of needing reference to frame?
      (.repaint frame))

    (mouseReleased [mouse-event]
      (swap! selection assoc :start nil :end nil)
      (.repaint frame))))

(def mouse-motion-adapter
  (proxy (MouseMotionAdapter) []
    (mouseDragged [mouse-event]
      ; store the end of the selection
      (swap! selection assoc :end (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))

      (let [start (:start @selection)]
        (if (nil? start)
          (swap! selection assoc :start (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))))

      (recalc-selection (.isControlDown mouse-event))
      (.repaint frame))))

(defn run []
(SwingUtilities/invokeLater (fn [] (do
  (.addMouseListener panel mouse-adapter)
  (.addMouseMotionListener panel mouse-motion-adapter)
  (doto frame
    .pack
    (.setVisible true)
    (.add panel BorderLayout/CENTER))))))