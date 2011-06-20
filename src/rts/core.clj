(ns rts.core
  (:use [clojure.contrib.math :only [abs]])
  (:use [clojure.contrib.swing-utils :only [do-swing do-swing*]])
  (:import (javax.swing JFrame JPanel JButton SwingUtilities JTextArea JComponent JScrollPane)
           (java.awt Dimension BorderLayout Color)
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
(defn translate-n
  "For a pair of x or y start and end points, calculate the point dist pixels closer to the end point, or
   return the end point if it is closer than 5 pixels away"
  [n-source n-dest dist]
  (if (= n-source n-dest)
    n-source
    (+ n-source ; add the neg. or pos. distance to n1
      (* ; multiply the min distance by 1 or -1 to handle converging on n2 from any direction
        (/ ; divide the distance between n-dest and n-source by the abs of itself to get 1 or -1
          (- n-dest n-source)
          (abs (- n-dest n-source)))
        (min ; get the minimum of the distance between dest and source or dist
          (abs (- n-dest n-source))
          dist)))))

(defn move-unit
  "Work in progress. Move a unit. "
  [unit-move]
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
    ;
    ; !! might be able to use a watch to do this
    {:x (min x-orig x-dest) :y (min y-orig y-dest) :w (+ unit-size (- (max x-orig x-dest) (min x-orig x-dest))) :h (+ unit-size (- (max y-orig y-dest) (min y-orig y-dest)))}
    ))

(defn do-moves []
  "Run each unit move and return a list of the clip regions to repaint"
  (map move-unit @unit-moves))

(defn unit-selected?
  "Check if a unit is in the selected units collection"
  [unit]
  (some #(= % unit) @selected-units))

(defn draw-unit
  "Draw a unit on the draw panel"
  [g2d unit]
  (let [selected (unit-selected? unit) color (if selected Color/RED Color/BLACK)]
    (.setColor g2d color)
    (.draw g2d (:shape @unit))))

(defn selection-rectangle
  "Given two, create a rectangle the encompasses both points. (Might be a dupe of xywh)"
  []
  (let [{:keys [start end]} @selection]
    (if (and start end)
      (let [start-x (.getX start)
            start-y (.getY start)
            end-x (.getX end)
            end-y (.getY end)]
        (Rectangle2D$Float. (min start-x end-x) (min start-y end-y) (abs (- start-x end-x)) (abs (- start-y end-y)))
        ))))

(defn draw-selection
  "Draw a selection rectangle"
  [g2d]
    (if-let [rect (selection-rectangle)]
        (do (.setColor g2d Color/BLUE)
            (.draw g2d rect))))

(defn draw-grid
  "Draw a grid on the draw-panel; this is for working on path finding and would be in the final version"
  [panel g2d]
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

    ; blank out clipped region
    (.setColor g2d Color/WHITE)
    (.fill g2d clip)

    (.setColor g2d Color/GRAY)
    (doseq [line (map #(Line2D$Float. % %2 % %3) (range prev-x next-x square-size) (repeat y) (repeat max-y))]
      (.draw g2d line))
    (doseq [line (map #(Line2D$Float. %2 % %3 %) (range prev-y next-y square-size) (repeat x) (repeat max-x))]
      (.draw g2d line))))

(defn in-clip?
  "Returns if a unit is in a clipping region shape. (Might be dupe of sel-contains-unit?)"
  [clip unit]
  (.intersects (:shape @unit) (.getX clip) (.getY clip) (.getWidth clip) (.getHeight clip)))

(defn draw-panel-paint
  "Function to repaint the dirty regions of the draw panel"
  [panel g2d]
      (if draw-grid? (draw-grid panel g2d))

      (let [units @units selection @selection sel-start (:start selection) sel-end (:end selection)]
        (if units
          (doall (map (fn [unit] (draw-unit g2d unit)) (filter (partial in-clip? (.getClip g2d)) units))))
        
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

(defn select-unit
  "Create a new selection with this unit or add it to the existing selection based on the Ctrl key flag"
  [unit ctrl?]
  (if (not ctrl?)
    (swap! selected-units empty))
  (if (unit-selected? unit)
    (swap! selected-units (partial remove #(= unit %)))
    (swap! selected-units conj unit)))

(defn create-unit
  "Given a mouse event and a ctrl flag (for the Ctrl keyboard key), create a new room and either select it or add it
   to the existing room selections"
  [mouse-event ctrl?]
  (let [new-unit (atom {:shape (Rectangle2D$Float. (.getX mouse-event) (.getY mouse-event) unit-size unit-size)})]
    (swap! units conj new-unit)
    (select-unit new-unit ctrl?)))

(defn sel-contains-unit? [sel-rect unit]
  (let [unit-rect (.getBounds2D (:shape @unit))]
    (.contains sel-rect unit-rect)))

(defn recalc-selection
  "Update the collection of selected units when the selection box is changed"
  [ctrl?]
  ; check for Ctrl before clearing selection
  (let [units @units sel-rect (selection-rectangle)]
    (if sel-rect
      (do
        (if (not ctrl?)
          (swap! selected-units empty))
        (swap! selected-units concat (filter (partial sel-contains-unit? (selection-rectangle)) units))))))


(defn get-unit
  "Given a mouse event, return any unit at the mouse event x,y"
  [mouse-event]
  (let [p (Point2D$Float. (.getX mouse-event) (.getY mouse-event))]
    (debug (.getButton mouse-event))
    (first (filter #(. (:shape (deref %)) contains p) @units))))

(defn left-mouse [mouse-event]
  (if-let [unit (get-unit mouse-event)]
    (select-unit unit (.isControlDown mouse-event)) ; add check for Ctrl key to add selection instead of replace it
    (create-unit mouse-event (.isControlDown mouse-event)))

  (swap! selection assoc :start nil :end nil))

; !! FIX ME - doesn't line up to grid
(defn resolve-to-square
  "Supposed to take a number and return it adjusted to align with a multiple of square-size (i.e. to align with a grid). Doesn't work well."
  [n] (- n (mod n square-size)))

(defn right-mouse [mouse-event]
  ;(debug "resolving: x " (resolve-to-square (.getX mouse-event)) " y " (resolve-to-square (.getY mouse-event)))
  (swap! unit-moves concat (map (fn [unit] {:unit unit :move {:x (resolve-to-square (.getX mouse-event)) :y (resolve-to-square (.getY mouse-event))}}) @selected-units))
  )

(defn xywh
  "convert two points, or a selection containing two points, to a list containing the
   x, y, width and height of a rectangle formed by the points"
  ([point-one point-two]
  (let [x1 (.getX point-one) x2 (.getX point-two) y1 (.getY point-one) y2 (.getY point-two)]
    [(min x1 x2) (min y1 y2) (- (max x1 x2) (min x1 x2)) (- (max y1 y2) (min y1 y2))]))
  ([selection]
    (xywh (:start selection) (:end selection))))

(defn mouse-clicked [mouse-event]
  (if (= MouseEvent/BUTTON1 (.getButton mouse-event))
        (left-mouse mouse-event)
        (right-mouse mouse-event)))

(defn mouse-released [mouse-event] (swap! selection assoc :start nil :end nil))

(def mouse-adapter
  ; switch to reify
  (proxy [MouseAdapter] []
    (mouseClicked [mouse-event] (mouse-clicked mouse-event))
    (mouseReleased [mouse-event] (mouse-released mouse-event))))


(defn mouse-dragged
  "Handles updating room selections on mouse drag"
  [mouse-event] (
    ; store the end of the selection
    (swap! selection assoc :end (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))

    ; store the start of the selection if we haven't already
    (let [start (:start @selection)]
      (if (nil? start)
        (swap! selection assoc :start (Point2D$Float. (.getX mouse-event) (.getY mouse-event)))))

    (recalc-selection (.isControlDown mouse-event))))

(def mouse-motion-adapter
  (proxy [MouseMotionAdapter] [] (mouseDragged [mouse-event] (mouse-dragged mouse-event))))

; !! use a watch to do the repainting
(defn draw-moves
  "Performs all moves and then repaints resulting clips"
  []
  (doseq [{:keys [x y w h]} (do-moves)] (debug "redraw clip x: " x " y: " y " w: " w " h: " h)(.repaint draw-panel x y (+ w 1) (+ h 1)) )

  ;!! remove finished moves
  (swap! unit-moves empty))

(defn union-selections
  "takes two selections or four points and returns a rectangle that encompasses both/all of them;
   handles an empty new selection (i.e. selection is cancelled)"
  ([sel-old sel-new]
    (cond
      (:start sel-new) (union-selections (:start sel-new) (:end sel-new) (:start sel-old) (:end sel-old))
      (:start sel-old) (xywh sel-old)
      :default nil))

  ([p1 p2 p3 p4]
    (let [[[x1 y1] [x2 y2] [x3 y3] [x4 y4]] (map (fn [p] [(.getX p) (.getY p)]) [p1 p2 p3 p4])
          min-x (min x1 x2 x3 x4)
          min-y (min y1 y2 y3 y4)
          max-x (max x1 x2 x3 x4)
          max-y (max y1 y2 y3 y4)]
      [min-x min-y (- max-x min-x) (- max-y min-y)])))

; not sure why this works even though we're not deref'ing the future
(add-watch selection "selection-watch" (fn [key ref old-state new-state] (future (apply #(.repaint draw-panel % %2 (+ 1 %3) (+ 1 %4)) (union-selections old-state new-state)))))

(defn unique
  "given two collections, return a set of elements unique to both collections; e.g. [1 2 3][3 4 5] -> [1 2 4 5]"
  [xs ys]
  ;could also use the frequencies function to look for elements with more than 1 occurrence
  (apply disj (set (concat xs ys)) (clojure.set/intersection (set xs) (set ys))))

(defn repaint-draw-panel
  "Wrapper function for .repaint to allow (apply) to be used"
  [x y w h]
  (.repaint draw-panel x y w h))

(defn unit-ref-shape-xywh
  "Given unit ref, return the x, y, w, h of the unit's shape"
  ([unit-ref] (unit-ref-shape-xywh unit-ref 0))
  ([unit-ref padding] (let [shape (:shape @unit-ref)] [(.getX shape) (.getY shape) (+ padding (.getWidth shape)) (+ padding (.getHeight shape))])))

(defn units-changed
  "calls repaint for units that are added to or removed from the reference (e.g. selected units, all units)"
  [key ref old-state new-state]
    (-> (Thread. #(doseq [xywh (map unit-ref-shape-xywh (unique old-state new-state) (repeat 1))] (apply repaint-draw-panel xywh))) (.start)))

(add-watch units ::units-watch units-changed)
(add-watch selected-units :selected-units-watch units-changed)

(defn run
  "Creates mouse listeners, debug frame, draw frame; shows frames; schedules
   move function"
  []
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

    ;; !! switch to do-moves as a watcher can do the drawing
    (-> (Executors/newScheduledThreadPool 1) (.scheduleWithFixedDelay (fn [] (draw-moves)) 100 100 TimeUnit/MILLISECONDS))
  )