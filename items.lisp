
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

;;;; units.lisp
(in-package #:pseudo)

(defparameter *owned-items* (make-hash-table))
(defparameter *item-spawn-counter* 0)
(defparameter *last-item-spawn-time* 0)
(defparameter *item-spawn-interval* (* 4 3600)) ;; One item per 4 hours
(defparameter *item-types* '())

(defun item-set-types ()
  "Set the various item types we have in the game"
  (setf *item-types*
        '("up arrow"
          "down arrow"
          "plant"
          "lamp"
          "chest"
          "helm"
          "boots"
          "legs"
          "gloves"
          "arms"
          "sword")))

(item-set-types) ;; Calling this after defining it

(defun item-create-name (type map-id)
  "Generate an item name based on the type of item sent in,
as well as the map-id for relative item power"
  (let ((prefix (nth type *item-types*))
        (suffix (funcall
                 (λ 0 → "beginner"
                    1 → "novice"
                    2 → "adventurer"
                    3 → "warrior"
                    4 → "veteran"
                    5 → "destroyer"
                    6 → "ultimate"
                    α → "godlike") map-id)))
    (format nil "~a of the ~a"
            (string-capitalize prefix)
            (string-capitalize suffix))))

(defun item-pickup (unit-id item-id)
  "Given a unit-id and item-id, give an item to a player"
  (let ((item (get-from *items* 'id item-id)))
    (with-slots (owner map_index) item
      (when (eq 0 owner) ;; if no one owns it, pick it up
        (setf map_index -1
              owner unit-id)
        (broadcast "item-update" (item-to-json item))))))

(defun request-item-pickup (json client)
  "Request loot rights on an item (add to player)"
  (let ((unit-id (cdr (assoc :unit--id json)))
        (item-id (cdr (assoc :item--id json))))
    (item-pickup unit-id item-id)
    (emit "request-item-owned" json client)
    (broadcast "item-update"
               (item-to-json (get-from *items* 'id item-id)))))

(defun item-filtered ()
  "Show all items that are still visible on a map"
  (mapcar #'item-to-json
          (remove-if (λ α → (< (slot-value α 'map_index) 0))
                     *items*)))

(defun request-all-items (json client)
  "Pull out list of all items in system for Node to hold"
  (declare (ignore json))
  (json-response "blub" (item-filtered)
                 "dub" (item-filtered)))

(defun item-resync (map-index &optional client)
  "Use a map index to pull out all items on the map - send out an emit
signal to each user on the map."
  (let ((items (get-matches *items* 'map_index map-index))
        (clients (if client (list client) (map-clients-by-id map-index))))
    (mapcar (lambda (client)
              (emit "item-resync"
                    (mapcar #'item-to-json items)
                    client)) clients)))

(defun request-item-resync (json client)
  "Have a unit request items on their map"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (map-index (slot-value (get-from *units* 'id unit-id)
                                'map_index)))
    (item-resync map-index client)))

(defun item-owned (unit-id client)
  "Grab all items owned by a unit"
  (let ((items (mapcar #'item-to-json
                       (get-matches *items* 'owner unit-id))))
    (emit "item-owned" items client)))

(defun request-item-owned (json client)
  "Have a unit request the items that they currently own"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (item-owned unit-id client)))

(defun item-stat-mod (unit item &key (fn #'+))
  "Given a unit and item, return the modified unit data after equipping it
Only succeed on equip if owner matches unit and it is not worn"
  (with-slots (worn owner) item
    (with-slots (id hp mp atk def mag mdef) unit
      (when (and (eq owner id)                   ;; make sure owner matches item owner
                 (eq worn (if (eq #'+ fn) 0 1))) ;; wear it if it isn't worn yet
        (setf worn (if (eq #'+ fn) 1 0)
              hp (funcall fn hp (slot-value item 'hp))
              mp (funcall fn mp (slot-value item 'mp))
              atk (funcall fn atk (slot-value item 'atk))
              def (funcall fn def (slot-value item 'def))
              mag (funcall fn mag (slot-value item 'mag))
              mdef (funcall fn mdef (slot-value item 'mdef)))
        unit))))

(defun item-unequip-similar-types (unit type)
  "Run across items and unequip similar types so only one of each type may be worn,
such as one pair of boots/gloves, not infinite.
Unit is the unit, while type is the specific type to unequip"
  (mapcar (λ α → (item-stat-mod unit α :fn #'-))
          (get-matches (get-matches *items* 'owner (slot-value unit 'id)) 'type type)))

(defun item-equip (unit-id item-id &optional unequip)
  "Equip/unequip an item given a unit-id and item-id"
  (let ((unit (get-from *units* 'id unit-id))
        (item (get-from *items* 'id item-id)))
    (item-unequip-similar-types unit (slot-value item 'type))
    (item-stat-mod unit item :fn (if unequip #'- #'+))))

(defun item-unequip (unit-id item-id)
  "Short cut function to unequip an item"
  (item-equip unit-id item-id t))

(defun request-item-equip (json client)
  (let ((unit-id (cdr (assoc :unit--id json)))
        (item-id (cdr (assoc :item--id json))))
    (item-equip unit-id item-id)
    (item-owned unit-id client)
    (emit "update-unit" (get-from *units* 'id unit-id) client)))

(defun request-item-unequip (json client)
  (let ((unit-id (cdr (assoc :unit--id json)))
        (item-id (cdr (assoc :item--id json))))
    (item-unequip unit-id item-id)
    (item-owned unit-id client)
    (emit "update-unit" (get-from *units* 'id unit-id) client)))

(defun item-update-to-same-map-units (map-id item-id)
  "Send an item update to all users on the same map"
  (let ((clients (map-clients-by-id map-id))
        (item (item-to-json (get-from *items* 'id item-id))))
    (mapcar (λ α → (emit "item-update" item α)) clients)))

(defun item-spawn (type map-id &key (x 20) (y 20))
  "Given an item type and map-id, spawn an item there of that type.
Items scaled based on map id as the max value for a stat - ie venture
further to get stronger gear (or at least the chance of some)."
  (let* ((max-stat (1+ map-id))
         (hp (random max-stat))
         (mp (random max-stat))
         (atk (random max-stat))
         (def (random max-stat))
         (mag (random max-stat))
         (mdef (random max-stat))
         (item-id (incf *item-spawn-counter*)))
    (add-item (list item-id
                    (item-create-name type map-id)
                    x
                    y
                    map-id
                    type
                    0     ;; owner
                    hp mp atk def mag mdef
                    0     ;; special power
                    "Lore goes here"
                    0))  ;; worn
    (item-update-to-same-map-units map-id item-id)))

(defun item-spawner ()
  "Run across the available maps, spawn item in a random location"
  (let ((time (get-universal-time)))
    (when (> (- time *last-item-spawn-time*) *item-spawn-interval*)
      (setf *last-item-spawn-time* time)
      (loop for m from 0 to (length *maps*) ;; TODO Make sure to spawn where its reachable
         do (item-spawn (+ 4 (random 7)) m ;; The random is the upper bound on item available
                        :x (random *map-max-size*)
                        :y (random *map-max-size*))) *items*)))

(defun item-js ()
  "The item JS"
  (ps

    (defvar item-types
      (array (create img "sprites/up.png" block false)
             (create img "sprites/down.png" block false)
             (create img "sprites/plantgreen.png" block false)
             (create img "sprites/lamp.png" block false)
             (create img "sprites/items/chest.png" block false)
             (create img "sprites/items/helm.png" block false)
             (create img "sprites/items/boots.png" block false)
             (create img "sprites/items/legs.png" block false)
             (create img "sprites/items/gloves.png" block false)
             (create img "sprites/items/arms.png" block false)
             (create img "sprites/items/weapon.png" block false)))

    (defvar map-items
      (array (create type 3 x 3 y 0)
             (create type 3 x 4 y 1)
             (create type 3 x 5 y 2)
             (create type 3 x 6 y 3)
             (create type 3 x 7 y 4)
             (create type 3 x 8 y 5)
             (create type 3 x 9 y 7)
             (create type 3 x 3 y 6)))

    (defvar dialog-pop-p t)

    (defun player-in-object-range ()
      "See if a player is in object/item range"
      (let ((x (ash (@ player x) 0))
            (y (ash (@ player y) 0)))
        (loop for item in map-items
           when (in-range x y
                          (ash (@ item x) 0)
                          (ash (@ item y) 0)
                          2)
           do (progn
                (object-in-range-controller item)
                (return-from player-in-object-range))))
      (dialog nil))

    (defun item-pickup (item)
      "Pick up an item and add to inventory"
      (chat-add-to-log (+ "You picked up a " (@ item name) "!"))
      (dialog nil)
      (sock-emit "request-item-pickup" (create unit_id *my-id*
                                               item_id (@ item id)))
      (set-timeout (λλ α → (sock-emit "request-item-owned" (create unit_id *my-id*))) 500))

    (defun object-in-range-controller (item)
      "Choose which action to assign to an in range item"
      (funcall (λλ
                0 → (if (eq (@ player map_index) 0)
                        (dialog "This does absolutely nothing currently!")
                        (zone-change "up"))
                1 → (zone-change "down")
                2 → (dialog "A pretty green plant")
                3 → (dialog "A pretty tree")
                4 → (dialog "A piece of chest armor - pick it up?"
                            (λλ α → (item-pickup item)))
                5 → (dialog "Some type of helmet - pick it up?"
                            (λλ α → (item-pickup item)))
                6 → (dialog "It appears to be some old boots"
                            (λλ α → (item-pickup item)))
                7 → (dialog "Someone else's pants - you sure you want them?"
                            (λλ α → (item-pickup item)))
                8 → (dialog "Gloves - I think -- these would be great for picking things up"
                            (λλ α → (item-pickup item)))
                9 → (dialog "Some tattered sleeves - where's the rest of the shirt?"
                            (λλ α → (item-pickup item)))
                10 → (dialog "Is that Jim's sword?"
                             (λλ α → (item-pickup item)))
                α → (dialog "What to do...?"))
               (@ item type)))

    (set-interval player-in-object-range 1000)

    (defun item-controller (&optional event data)
      "Handle item controller events"
      (funcall (λλ
                "resync" → (item-resync data)
                "update" → (item-add-or-update data)
                "owned"  → (item-owned data)
                α        → (chain console (log "item-controller called")))
               event))

    (defvar *item-update-received-p* nil)
    (defvar *item-data* [])

    (ƒƒ item-resync
        α → (progn
              (setf *item-data* α
                    map-items (append *stair-data* *item-data*))
              (setf *item-update-received-p* t)))

    (ƒƒ item-owned
        α → (progn
              (chain ($ "#item-list") (html ""))
              (loop for item in α
                 do (let ((worn (if (@ item worn) "(e)" ""))
                          (fn (if (@ item worn)
                                  "itemRequestUnequip"
                                  "itemRequestEquip")))
                      (chain ($ "#item-list")
                             (append
                              (+ "<div class='item' onclick='" fn "("
                                 (@ item id)
                                 ");'><img src='"
                                 (@ (aref item-types (@ item type)) img)
                                 "'>"
                                 "<b>" (@ item name) worn "</b>"
                                 "<div class='item-stats'>"
                                 "<b>hp</b>" (@ item hp)
                                 "<b>mp</b>" (@ item mp)
                                 "<b>atk</b>" (@ item atk)
                                 "<b>def</b>" (@ item def)
                                 "<b>mag</b>" (@ item mag)
                                 "<b>mdef</b>" (@ item mdef)
                                 "</div>"
                                 "</div>")))))))

    (ƒƒ item-request-unequip
        α → (sock-emit "request-item-unequip"
                       (create unit_id *my-id*
                               item_id α)))

    (ƒƒ item-request-equip
        α → (sock-emit "request-item-equip"
                       (create unit_id *my-id*
                               item_id α)))

    (ƒƒ item-filter
        α → (setf map-items
                  (append *stair-data*
                          (loop for item in *item-data*
                             when (eq (@ item map_index) (@ player map_index))
                             collect item))))

    (ƒƒ item-add-or-update
        α → (progn
              (when (equal (typeof (@ α id)) "undefined")
                (setf α (aref α 0)))
              (loop for item in *item-data*
                 when (eq (@ α id) (@ item id)) ;; If we find item, update it
                 do (progn (with-slots (map_index x y owner) item
                             (setf map_index (@ α map_index)
                                   x (@ α x)
                                   y (@ α y)
                                   owner (@ α owner)))
                           (setf *item-update-received-p* t)
                           (return-from item-add-or-update)))
              (setf *item-update-received-p* t)
              (chain *item-data* (push α)) ;; Otherwise, add to the item arrays
              (chain map-items (push α))))

    (ƒƒ item-reinitializer
        (not *item-update-received-p*) → (return-from item-reinitializer)
        α → (progn
              (setf *item-update-received-p* nil)
              (item-filter)
              (set-timeout draw-mini-map 1000)
              (set-timeout init-sprites 1000)))

    (set-interval item-reinitializer 1000)

    (ƒƒ items-show-pane
        α → (progn
              (chain ($ "#item-list, #item-helper") (show))
              (chain ($ "#items") (css (create width "500px"
                                               height "300px")))))

    (ƒƒ items-hide-pane
        α → (progn
              (chain ($ "#item-list, #item-helper") (hide))
              (chain ($ "#items") (css (create width "50px"
                                               height "50px")))))

    nil))
