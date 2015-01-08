;; Pseudo - A pseudo 3d multiplayer roguelike
;; Copyright (C) 2013 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; map.lisp
(in-package #:pseudo)

(defparameter *map-max-room-size* 2)
(defparameter *map-min-size* 21)
(defparameter *map-max-size* 50)
(defparameter *stair-range-threshold* 4)
(defparameter *maps* (make-array 3 :adjustable t :fill-pointer 0))
(defparameter *stairs* (make-hash-table))
(defparameter *seed* (make-random-state t))

(defmacro map-size (map &rest rest)
  "Quickly set up the base to iterate over map data"
  `(let* ((dim (array-dimensions ,map))
          (y-size (car dim))
          (x-size (cadr dim))) ,@rest))

(defun map-create (&optional map-max-size)
  "Create a map of random sizes"
  (let* ((map-max-size (or map-max-size (1+ (random *map-max-size* *seed*))))
         (y-size (+ *map-min-size* map-max-size))
         (x-size (+ *map-min-size* map-max-size)))
    (make-array (list y-size x-size) :initial-element 1)))

(defun map-grow-room (map y x grow)
  "Loop over a map and grow some rooms"
  (map-size
   map (loop for ny from (- y grow) to (+ y grow) do
            (loop for nx from (- x grow) to (+ x grow) do
                 (when (and (< ny (1- y-size)) (> ny 1)
                            (< nx (1- x-size)) (> nx 1))
                   (setf (aref map ny nx) 0))))))

(defun map-connect-room (map sy sx dy dx)
  "Take two points and connect them by setting each traversed point to 0"
  (map-size
   map
   (let ((x sx)
         (y sy))
     (if (> (random 2 *seed*) 0)
         (cond ((> sy dy) (decf y))
               ((< sy dy) (incf y)))
         (cond ((> sx dx) (decf x))
               ((< sx dx) (incf x))))
     (if (and (= dx x) (= dy y))
         map
         (progn (setf (aref map y x) (if (and (< y (1- y-size))
                                              (< x (1- x-size))) 0 1))
                (map-connect-room map y x dy dx))))))

(defun map-extra-textures (map)
  "Run across map and changes the 1's to different texture walls"
  (map-size map (loop for y from 0 below y-size do
                     (loop for x from 0 below x-size
                        when (eq (aref map y x) 1)
                        do (setf (aref map y x) (1+ (random 4 *seed*)))))
            map))

(defun map-gen-stairs (map)
  "Run across map and choose some stair coordinates"
  (let ((up (list :x 0 :y 0))
        (down (list :x 0 :y 0)))
    (map-size map (loop until
                       (and (eq (aref map (getf up :y) (getf up :x)) 0)
                            (eq (aref map (getf down :y) (getf down :x)) 0))
                     do (setf up
                              (list :dir "up"
                                    :type 0
                                    :x (1+ (random (1- x-size)))
                                    :y (1+ (random (1- y-size))))
                              down
                              (list :dir "down"
                                    :type 1
                                    :x (1+ (random (1- x-size)))
                                    :y (1+ (random (1- y-size)))))))
    (list up down)))

(defun map-set-stairs (map-id)
  "Given a map ID, add some stairs into the hash entry for the map"
  (let ((map (map-lookup map-id)))
    (setf (gethash map-id *stairs*) (map-gen-stairs map))))

(defun map-set-items (map-id)
  "Pick some stationary items to spawn (currently trees)"
  (let ((map (map-lookup map-id)))
    (map-size
     map
     (let ((times (random (+ x-size y-size) *seed*)))
       (dotimes (i times)
         (let ((x (max 2 (1- (random x-size *seed*))))
               (y (max 2 (1- (random y-size *seed*)))))
           (when (eq (aref map y x) 0)
             (item-spawn (+ 2 (random 2 *seed*))
                         map-id :x x :y y))))))))

(defun map-set-npcs (map-id)
  "Similar to set stairs, but slightly different - make use
of the random coordinates from map-gen-stairs however"
  (dotimes (i (1+ map-id)) ;; Spawn mobs equal to map id +1
    (let* ((coords (car (map-gen-stairs (map-lookup map-id))))
           (x (getf coords :x))
           (y (getf coords :y)))
      (create-npc
       (random (length *jobs*)) ;; job id
       :map-index map-id
       :sx x
       :sy y
       :level map-id))))

(defun map-randomize (map)
  "Create a usable map by setting up some rooms"
  (map-size map (let ((sy *map-min-size*)
                      (sx *map-min-size*))
                  (dotimes (i (round (/ (+ x-size y-size) 3)) map)
                    (let ((y (1+ (random (- y-size 2) *seed*)))
                          (x (1+ (random (- x-size 2) *seed*)))
                          (size (1+ (random *map-max-room-size* *seed*))))
                      (setf (aref map y x) size)
                      (map-connect-room map sy sx y x)
                      (map-grow-room map y x size)))
                  (map-grow-room map *map-min-size* *map-min-size* 2)) map))

(defun map-lookup (id)
  "Grab a map based on its ID, or create if it doesn't exist yet"
  (if (and (array-in-bounds-p *maps* id)
           (arrayp (aref *maps* id)))
      (aref *maps* id)
      (progn ;; else
        (let ((base-map (if (eq id 0) (map-create 1) (map-create))))
          (vector-push-extend (map-extra-textures
                               (map-randomize base-map)) *maps*)
          (map-set-stairs id)
          (map-set-npcs id)
          (map-set-items id)
          (map-lookup id)))))

(defun map-to-list (map)
  "Helper function to change array map to a list"
  (map-size map (loop for y from 0 below y-size collect
                     (loop for x from 0 below x-size collect
                          (aref map y x)))))

(defun map-to-json (map-id)
  "Convert the map to our json string, also do a lookup on next map to
prepare it."
  (let ((map-data (map-to-list (map-lookup map-id))))
    (map-lookup (1+ map-id)) ;; TODO - Pinpoint the real bottleneck, remove stopgap measure
    (list (cons :map-data map-data)
          (cons :junk "yes"))))

(defun request-map (json client)
  "Request a map for the users"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (map-id (slot-value (get-from *units* 'id unit-id) 'map_index)))
    (emit "map-init" (map-to-json map-id) client)))

(ƒ keywords-to-strings
   "Run over list and change keywords into strings for json parsing"
   (keywordp α)    → (string α)
   (not (listp α)) → α
   α               → (mapcar #'keywords-to-strings α))

(defun stairs-to-json (map-id)
  "Put the map stair data in a json format"
  (gethash map-id *stairs*))

(defun request-stairs (json client)
  "Send out the relevant stair data"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (map-id (slot-value (get-from *units* 'id unit-id) 'map_index)))
    (emit "stair-init"
          (mapcar #'plist-alist (stairs-to-json map-id))
          client)))

(defun zone-change (unit-id direction)
  "Move up or down a zone"
  (let ((unit (get-from *units* 'id unit-id)))
    (when unit
      (with-slots (x y map_index) unit
        (loop for stair in (gethash map_index *stairs*)
           when (and (in-range x y (getf stair :x) (getf stair :y)
                               *stair-range-threshold*)
                     (equal (getf stair :dir) direction))
           do (progn (setf x *map-min-size* y *map-min-size*)
                     (funcall (λ "up" → (decf map_index)
                                 "down" → (incf map_index)) direction)))
        map_index))))

(defun request-zone-change (json client)
  "Change zones based on json input"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (direction (cdr (assoc :direction json)))
         (map-id (zone-change unit-id direction)))
    (emit "map-reload"
          (map-to-json map-id)
          client)))

(defun map-clients-by-id (map-id)
  "Gather the available clients that are present on a given map id"
  (loop for unit in (get-matches *units* 'map_index map-id)
     unless (eq 0 (unit-owner unit))
     collect (unit-client (unit-id unit))))

(defun map-js ()
  (ps

    (defun map-controller (event data)
      "Needing to eval - something is off here TODO come back to this"
      (let ((json data)) ;; (eval (+ "(" data ")"))))
        (funcall (λλ
                  "reload" → (map-reload)
                  "init"   → (map-init (@ json map-data))
                  "stairs" → (map-stair-init json)) event)))

    (defvar *last-map-index* 0)
    (set-interval map-retexture 500)
    (defun map-retexture ()
      "On slow connections this may not hit fast enough"
      (unless (= *last-map-index* (@ player map_index))
        (setf *last-map-index* (@ player map_index))
        (if (= 0 (@ player map_index)) ;; first level has grass
            (setf ceiling-color "blue"
                  floor-color "lime"
                  wall-textures (array
                                 "/grass.png"
                                 "/grass.png"
                                 "/grass.png"
                                 "/grass.png"
                                 ))
            ;; else
            (setf ceiling-color "#999"
                  floor-color "red"
                  wall-textures (array
                                 "tiles/stonefloor_32cols_256.png"
                                 "tiles/stonefloor_32cols_256.png"
                                 "tiles/stonefloor_32cols_256.png"
                                 "tiles/stonefloor_32cols_256.png"
                                 )))
        (chain ($ "#ceiling") (css (create background ceiling-color)))
        (chain ($ "#floor") (css (create background floor-color)))))

    (defun map-init (data)
      "Update a map as needed"
      (chain ($ "#loading") (show))
      (setf pause-render-p t)
      (sock-emit "request-stairs" (create "unit_id" *my-id*))
      (sock-emit "request-item-resync" (create "unit_id" *my-id*))
      (setf *map* data)
      (set-timeout init 1000)
      (setf (@ player x) 20
            (@ player y) 20) ;; lazy hack - eventually calc safest nonclip wall
      (dialog nil))

    (defun map-reload ()
      "Reload the map for the user"
      (sock-emit "request-map" (create unit_id *my-id*)))

    (defvar *stair-data* [])
    (defun map-stair-init (data)
      (setf *stair-data* data
            map-items (append *stair-data* *item-data*)))

    (defvar *zone-change-direction* "down")
    (defun zone-change (direction)
      (dialog (+ "Do you wish to venture " direction "wards?")
              (λλ α → (progn
                        (chain ($ "#loading") (show))
                        (dialog nil)
                        (setf *zone-change-direction* direction)
                        (set-timeout
                         (λλ α →
                             (sock-emit "request-zone-change"
                                        (create "unit_id" *my-id*
                                                "direction" *zone-change-direction*)))
                         500)))))

    (defun dialog-add-prompts (msg)
      (+ msg "<a href='#' class='yes'>yes (quick key 'y')</a>"
         "<a href='#' class='no'>no (quick key 'n')</a>"))

    (defvar dialog-up nil)
    (defvar dialog-last-message nil)
    (defun dialog (msg fn)
      (when (eq msg nil)
        (setf dialog-up nil)
        (chain ($ "#dialog") (html ""))
        (chain ($ "#dialog") (hide))
        (return-from dialog))
      (let ((msg (if fn (dialog-add-prompts msg) msg)))
        (unless (and dialog-up (equal msg dialog-last-message))
          (setf dialog-up t)
          (chain ($ "#dialog")
                 (html msg)
                 (show))
          (setf dialog-last-message msg)
          (chain ($ ".no") (click (λλ α → (dialog nil))))
          (chain ($ ".yes") (click fn)))))

    nil))
