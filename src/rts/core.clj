(ns rts.core
  (:use [clojure.contrib.math :only [abs]])
  (:import (javax.swing JFrame JPanel JButton SwingUtilities JTextArea JComponent JScrollPane)
           (java.awt Dimension BorderLayout Color Rectangle)
           (java.awt.geom Point2D$Float Rectangle2D$Float)
           (java.awt.event MouseAdapter MouseEvent ActionListener MouseMotionAdapter)
           (java.util.concurrent Executors TimeUnit)))

(def units (atom []))
(def selected-units (atom []))
(def selection (atom nil))
(def unit-moves (atom nil))

(def text-area (new JTextArea))

(defn debug [& strs] (.setText text-area (str (.getText text-area) (apply str strs "\n"))))

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
        x1 (.getX shape)
        y1 (.getY shape)
        h (.getHeight shape)
        w (.getWidth shape)
        x2 (:x move)
        y2 (:y move)]
    (debug "Moving unit from [" x1 "," y1 "] to [" x2 "," y2 "]")
    (swap! unit assoc :shape (Rectangle2D$Float. (translate-n x1 x2 5) (translate-n y1 y2 5) w h))
    ))

(defn do-moves []
  ;(debug "doing" (count @unit-moves) "moves")
  (doseq [unit-move @unit-moves] (move-unit unit-move))
  (swap! unit-moves (fn [xs] []))
  ; remove moves that are no longer valid
  )

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

(def draw-panel (doto
  (proxy (JPanel) []
    (paintComponent [g2d]
      (let [units @units selection @selection sel-start (:start selection) sel-end (:end selection)]
        ;(debug "p[" (count units) "s]")
        (if units
          (doall (map (fn [unit] (draw-unit g2d unit)) units))
        )
        (draw-selection g2d)
        )))

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
    (debug (.getButton mouse-event))
    (first (filter #(. (:shape (deref %)) contains p) @units))))

(defn left-mouse [mouse-event]
  (if-let [unit (get-unit mouse-event)]
              (select-unit unit (.isControlDown mouse-event)) ; add check for Ctrl key to add selection instead of replace it
              (create-unit mouse-event (.isControlDown mouse-event)))

              (swap! selection assoc :start nil :end nil))

(defn right-mouse [mouse-event]
  (swap! unit-moves concat (map (fn [unit] {:unit unit :move {:x (.getX mouse-event) :y (.getY mouse-event)}}) @selected-units))
  )

(def mouse-adapter
  ; switch to reify
  (proxy (MouseAdapter) []
    (mouseClicked [mouse-event]
      (if (= MouseEvent/BUTTON1 (.getButton mouse-event))
        (left-mouse mouse-event)
        (right-mouse mouse-event))
      )

    (mouseReleased [mouse-event]
      (swap! selection assoc :start nil :end nil)
      (.repaint draw-frame))))

(def mouse-motion-adapter
  (proxy (MouseMotionAdapter) []
    (mouseDragged [mouse-event]
      ; store the end of the selection
      (swap! selection assoc :end (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))

      (let [start (:start @selection)]
        (if (nil? start)
          (swap! selection assoc :start (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))))

      (recalc-selection (.isControlDown mouse-event))
      (.repaint draw-frame))))

(defn run []
  (SwingUtilities/invokeLater (fn [] (do
    (.addMouseListener draw-panel mouse-adapter)
    (.addMouseMotionListener draw-panel mouse-motion-adapter)
    (doto draw-frame
      .pack
      (.setVisible true)
      (.add draw-panel BorderLayout/CENTER)))
    (doto debug-frame
      .pack
      (.setVisible true)
      (.add debug-panel BorderLayout/CENTER))))
    
    (-> (Executors/newScheduledThreadPool 1) (.scheduleWithFixedDelay (fn [] (do-moves) (.repaint draw-frame)) 100 100 TimeUnit/MILLISECONDS))
  )