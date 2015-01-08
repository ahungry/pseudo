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

(defparameter *event-logs* (make-hash-table))

;; DATA OBJECTS - ideally match to db table struct
;; ACCOUNTS
(game-dataset account id user pass ip banned uid)

;; UNITS
(game-dataset unit id name hp mp atk def active mag mdef
              owner job_id level xp jp speed x y img map_index)

;; CARDS
(game-dataset card id name min_range max_range
              atk cost phy mag effect)

;; JOBS
(game-dataset job id name hp mp atk def mag mdef xp img reqs
              speed spoiler)

;; DECKS
(game-dataset deck unit-id cards)

;; HANDS
(game-dataset hand unit-id cards last-draw-time)

;; ITEMS
(game-dataset item id name x y map_index type owner
              hp mp atk def mag mdef special lore worn)

;; None of this is probably going to be used, but leave here for
;; a reference in case I want to set up copy methods later on
(defgeneric copy-class (type instance))
(defmethod copy-class (type instance)
  nil)
(defmethod copy-class ((type (eql :unit)) instance)
  (with-slots (id name hp mp atk def active mag mdef
                  owner job_id level xp jp speed x y img map_index)
      instance
    (make-instance 'unit
                   :id id
                   :name name
                   :hp hp :mp mp :atk atk :def def
                   :active active :mag mag :mdef mdef
                   :owner owner :job_id job_id :level level
                   :xp xp :jp jp :speed speed :x x :y y
                   :img img :map_index map_index)))

(defun get-matches (dataset key search)
  "Seek out all matching items from what we want"
  (loop for item in dataset when
       (equal search (slot-value item key)) collect item))

(defun get-from (dataset key search)
  "Search out a data item by any key we want"
  (when dataset
    (find search dataset
          :test #'equal
          :key (lambda (data)
                 (when data
                   (slot-value data key))))))

(defun get-db-accounts()
  "Grab all accounts"
  (setf *accounts* nil)
  (loop for account in (1query "select * from account where banned = 0") do
       (add-account account)) *accounts*)

;; Pull the units from the db into your variable
(defun get-db-units ()
  "Pull all active units out of our tactics db"
  (setf *units* nil)
  (loop for unit in (1query "select unit.*, job.img from unit left join job on unit.job_id = job.id") do
       (add-unit unit)) *units*)

(defun get-db-cards ()
  "Get all the cards out of the db"
  (setf *cards* nil)
  (loop for card in (1query "select * from card") do
       (add-card card)) *cards*)

(defun get-db-jobs ()
  "Get all the jobs out of the db"
  (setf *jobs* nil)
  (loop for job in (1query "select * from job") do
       (add-job job)) *jobs*)

(defun reformat-deck-cards (unit-id)
  "Turn our whitespace separated deck into a real list"
  (let ((deck (get-from *decks* 'unit-id unit-id)))
    (when deck
      (with-slots (cards) deck
        (setf cards
              (loop for card-id in
                   (all-matches-as-strings "\\d+ |\\d+$" cards)
                 collect (parse-integer card-id)))))))

(defun get-db-decks ()
  "Get all the decks out of the db"
  (setf *decks* nil)
  (loop for deck in (1query "select * from deck") do
       (progn (add-deck deck)
              (reformat-deck-cards (first deck)))))

(defun draw-card (unit-id)
  "Draw a random card from the unit deck"
  (let ((deck (get-from *decks* 'unit-id unit-id)))
    (when deck
      (let ((size (length (slot-value deck 'cards))))
        (when (> size 0)
          (with-slots (cards) deck
            (nth (random size (make-random-state t)) cards)))))))

(defun draw-card-controller (unit-id &optional draw-timer)
  "Only draw cards for user if they are under limit and timer"
  (let ((hand (get-from *hands* 'unit-id unit-id))
        (time (get-universal-time))
        (draw-timer (or draw-timer *draw-card-interval*)))
    (when (not hand)
      (add-hand (list unit-id (list) 0))
      (setf hand (get-from *hands* 'unit-id unit-id)))
    (with-slots (unit-id cards last-draw-time) hand
      (if (>= (length cards) *max-hand-size*)
          (no-json-response)
          (if (< (- time last-draw-time) draw-timer)
              (no-json-response)
              (let ((card-id (draw-card unit-id)))
                (setf last-draw-time time)
                (when card-id (push card-id cards))))))))

(defun give-deck (unit-id)
  "Build an initial deck for a unit/user"
  (let ((deck (get-from *decks* 'unit-id unit-id)))
    (when (not deck)
      (add-deck (list unit-id ())) (setf deck (get-from *decks* 'unit-id unit-id)))
    (with-slots (cards) deck
      (let ((needed-cards (- *starter-deck-size* (length cards))))
        (when (> needed-cards 0)
          (setf cards (append cards (loop for x from 1 to needed-cards
                                       for y = (+ 1 (random 5 (make-random-state t)))
                                       collect y))))))))

(defun same-map-units-to-json (unit-id)
  "Grab only units on the same map as the player"
  (let* ((player (get-from *units* 'id unit-id))
         (map-index (slot-value player 'map_index)))
    (loop for u in *units* when (eq (slot-value u 'map_index) map-index)
       collect u)))

(defun resync (unit-id &optional client)
  "Simply send out a json response to refresh units"
  (when client
    (emit "resync" (if unit-id
                       (mapcar #'unit-to-json (same-map-units-to-json unit-id))
                       (units-to-json))
          client))
  (broadcast "update-unit"
             (unit-to-json (get-from *units* 'id unit-id))))

(defun request-all-units (json client)
  "Pull out list of all units in system for Node to hold"
  (declare (ignore json))
  (json-response "blub" (units-to-json)
                 "dub" (units-to-json)))

(defun request-resync (json client)
  "Request a resync event for everyone but the requester"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (resync unit-id client)))

;; Different potential actions
;; Have a unit join the game - prioritize existing over db
(defun join-game (json client)
  "A unit has joined the game, alert everyone else"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (or (get-from *decks* 'unit-id unit-id) (give-deck unit-id))
    (draw-card-controller unit-id 0)
    (resync unit-id client)))

(defun move (unit-id dx dy)
  "Move unit to x y"
  (let ((unit (get-from *units* 'id unit-id)))
    (with-slots (id x y name) unit
      (setf x dx y dy))
    (resync unit-id)))
;; Temporarily taking out while I work on a fix for pushing AI out
;;    (json-response "update-unit" (unit-to-json unit)
;;         "update-unit" (unit-to-json unit))))

(defun level-up (unit-id)
  "On a level up apply certain bonuses based on job"
  (let* ((unit (get-from *units* 'id unit-id))
         (job (get-from *jobs* 'id (slot-value unit 'job_id))))
    (with-slots (hp mp atk def mag mdef speed xp level) unit
      (incf hp (+ 5 (slot-value job 'hp)))
      (incf mp (slot-value job 'mp))
      (incf atk (slot-value job 'atk))
      (incf def (slot-value job 'def))
      (incf mag (slot-value job 'mag))
      (incf mdef (slot-value job 'mdef))
      (incf speed (slot-value job 'speed))
      (incf level)
      (setf xp 0))))

(defun level-up-formula (level xp-required)
  "See if we meet requirement to level"
  (when (<= xp-required 0) (setf xp-required 1))
  (* level xp-required))

(defun unit-level-ups ()
  "Run across all units, see who gets a level up"
  (mapcar
   (lambda (unit)
     (with-slots (id job_id level xp name owner) unit
       (unless (eq owner 0) ;; for now dont let npc level up
         (let ((job (get-from *jobs* 'id job_id)))
           (when job
             (let ((needed-xp (level-up-formula level (slot-value job 'xp))))
               (when (>= xp needed-xp)
                 (level-up id)
                 (format t "Level up for ~a~%" name)))))))) *units*))

(defun client-error (&optional message)
  "Spit out a client error message to the user"
  (let ((message (or message "Something bad!")))
    (json-response "event-log" message "event-log" message)))

(defun damage-formula (atk def base)
  "Formula for damage calculation"
  (let ((modifier (- atk def)))
    (when (minusp modifier) (setf modifier 0))
    (round (if (plusp base)
               (+ base modifier)
               (- base modifier)))))

(defun change-job (unit-id job-id)
  "Change a unit job via unit id and job id"
  (let ((unit (get-from *units* 'id unit-id))
        (job-img (slot-value (get-from *jobs* 'id job-id) 'img)))
    (with-slots (job_id img) unit
      (setf job_id job-id img job-img))))

(defun kill-unit (unit-id killer-id)
  "Delete unit if NPC, if player change to skeleton
This causes big issues if using NPCs when list items suddenly
vanish - just set the unit to skeleton and clean out dead guys
through a separate function"
  (let ((unit (get-from *units* 'id unit-id))
        (lost-card-id (car (slot-value (get-from *decks* 'unit-id unit-id) 'cards))))
    (when lost-card-id (move-card *decks* unit-id killer-id lost-card-id))
    (with-slots (owner x y id level hp) unit
      (when (eq owner 0) (setf x 0 y 0 hp (* *regen-hp* -5)))
      (change-job id 1))))

(defun revive-unit (unit-id)
  "if not NPC, revive player to slime"
  (change-job unit-id 2))

(defun do-damage (unit hit card)
  "Do damage to a unit"
  (let ((damage (slot-value card 'atk))
        (physical (slot-value card 'phy))
        (magical (slot-value card 'mag))
        (atk (slot-value unit 'atk))
        (mag (slot-value unit 'mag))
        (def (slot-value hit 'def))
        (mdef (slot-value hit 'mdef)))
    (with-slots (hp id x y) hit
      (unless (and (eq x 0) (eq y 0))
        (let ((original-hp hp))
          (when (eq physical 1) ;; do physical damage
            (setf hp (- hp (damage-formula atk def damage))))
          (when (eq magical 1) ;; do magical damage
            (setf hp (- hp (damage-formula mag mdef damage))))
          (cond ((and (> original-hp 0) (< hp 1)) (kill-unit id (slot-value unit 'id)))
                ((and (< original-hp 1) (> hp 0)) (revive-unit id))
                ((< original-hp 0) (setf hp -1))))))))

(defun get-cards-from (object unit-id)
  "Grab all the cards in an object"
  (let ((hand (get-from object 'unit-id unit-id)))
    (when hand
      (let ((cards (slot-value hand 'cards)))
        cards))))

(defun get-card-from (object unit-id card-id)
  "Seek a card out of unit active hand"
  (let ((cards (get-cards-from object unit-id)))
    (when cards
      (let ((card-id-found (find card-id cards)))
        (when card-id-found
          (get-from *cards* 'id card-id))))))

(defun remove-card-helper (object unit-id card-id &optional card-slot)
  "Create a new hand list object"
  (let ((excluded nil)
        (hand (get-cards-from object unit-id))
        (i 0)
        (new-hand '()))
    (when (find card-id hand)
      (mapcar (lambda (card)
                (if (and
                     (eq card-id card)
                     (not excluded)
                     (>= i (or card-slot 0)))
                    (setf excluded t) ;; Only exclude 1 card
                    (push card new-hand))
                (setf i (incf i))) hand)
      (if excluded (nreverse new-hand)
          (remove-card-helper object unit-id card-id)))))

(defun remove-card-from (object unit-id card-id &optional card-slot)
  "Remove card from an object such as hand or deck"
  (let ((hand (get-from object 'unit-id unit-id))
        (new-cards (remove-card-helper object unit-id card-id card-slot)))
    (when new-cards
      (with-slots (cards) hand
        (setf cards new-cards)))))

(defun xp-formula (player-level opponent-level)
  "Standard formula for xp gain"
  (let ((min-xp 1)
        (xp (* 2 (- (+ 2 opponent-level) player-level))))
    (if (> xp min-xp) xp min-xp)))

(defun play-card (unit-id card-id dx dy &optional card-slot)
  "Play card on a location"
  (let* ((unit (get-from *units* 'id unit-id))
         (card (get-card-from *hands* unit-id card-id)))
    (if (not card) (client-error "You do not have that card, sorry!")
        (let ((hit-units (get-matches
                          (get-matches *units* 'x dx) 'y dy))
              (mp-remaining (- (slot-value unit 'mp)
                               (slot-value card 'cost))))
          (if (not (and unit (>= mp-remaining 0)))
              (client-error "Play card failed (not enough mp)!") ;; no matches
              (progn
                (with-slots (xp jp mp level) unit
                  (setf mp mp-remaining) ;; set units new mp
                  (mapcar (lambda (hit) ;; do damage on each hit
                            (let ((hit-lvl (slot-value hit 'level))) ;; give xp/jp
                              (setf xp (incf xp (xp-formula level hit-lvl)))
                              (setf jp (incf jp (xp-formula level hit-lvl))))
                            (do-damage unit hit card)) hit-units)) ;; hit units
                (remove-card-from *hands* unit-id card-id card-slot)
                (resync unit-id)))))))

(defun set-action (json client)
  "Parse the input action from the json format"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (card-id (cdr (assoc :card--id json)))
         (card-slot (cdr (assoc :card--slot json)))
         (x (cdr (assoc :x json)))
         (y (cdr (assoc :y json))))
    (if (eq card-id 0) (move unit-id x y)
        (play-card unit-id card-id x y card-slot))))

(defun slot-or-not (object slot-name &optional default)
  "Returns a slot value or a default"
  (if (not object) default
      (if (slot-boundp object slot-name)
          (slot-value object slot-name) default)))

(defun unit-regen ()
  "Compare current time with last regen time"
  (let ((time (get-universal-time)))
    (when (> (- time *last-regen-time*) *regen-interval*)
      (setf *last-regen-time* time)
      (mapcar (lambda (unit)
                (with-slots (hp mp job_id level owner) unit
                  (when (and (> hp 0)
                             (> owner 0))
                    (let* ((job (get-from *jobs* 'id job_id))
                           (job-mp (slot-or-not job 'mp 1))
                           (job-hp (slot-or-not job 'hp 1)))
                      (when (< mp (+ 10 (* job-mp level)))
                        (setf mp (incf mp *regen-mp*)))
                      (when (< hp (+ 10 (* job-hp level)))
                        (setf hp (incf hp *regen-hp*)))))))
              *units*))))

(defun add-card-to (object unit-id card-id)
  "Add a card to some place such as a hand or deck"
  (let ((item (get-from object 'unit-id unit-id)))
    (when (get-from *cards* 'id card-id)
      (with-slots (cards) item
        (setf cards (push card-id cards))))))

(defun unit-knockback (attacker defender)
  "When a knockback occurs, have the defender sent backwards"
  (declare (ignore attacker))
  (with-slots (x y) defender
    (let ((map (map-lookup (unit-map_index defender))))
      (dotimes (i 3)
        (when (eq 0 (aref map (unit-y defender) (unit-x defender)))
          (decf x)
          (decf y)))))
  (broadcast "update-unit" (unit-to-json defender)))

(defun request-sword-swing (json client)
  "Swing the sword and knock back anyone in range"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (player (get-from *units* 'id unit-id))
         (range 4)
         (hit-units (remove-if-not
                     (lambda (unit)
                       (and
                        (not (eq (unit-id unit) (unit-id player)))
                        (in-range (unit-x player) (unit-y player)
                                  (unit-x unit) (unit-y unit) range)))
                     (get-matches *units* 'map_index (unit-map_index player))))
         (msg (if hit-units
                  (format nil "You swing your sword, hitting ~{~a, ~}~%" (mapcar #'unit-name hit-units))
                  (format nil "You swing your sword, hitting no one!~%"))))
    (emit "event-log" (list msg) client)
    (mapcar (lambda (unit)
              (unit-physical-attack unit-id (unit-id unit))
              (unit-knockback player unit)) hit-units)))

(defun request-cards (json client-input)
  (json-response "request-cards" (cards-to-json)
                 "nil" (new-js ("nil" "nil"))))

(defun request-hand (json client)
  (let ((unit-id (cdr (assoc :unit--id json))))
    (draw-card-controller unit-id) ;; see if we can get a new card first
    (let ((hand (get-from *hands* 'unit-id unit-id)))
      (if (not hand) (no-json-response)
          (emit "request-hand" (hand-to-json hand) client)))))

(defun request-deck (json client)
  "Send in a json request to receive units deck"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (let ((deck (get-from *decks* 'unit-id unit-id)))
      (if (not deck) (no-json-response)
          (emit "request-deck" (deck-to-json deck) client)))))

(defun change-unit-name (unit-id new-name)
  "Update name of a unit"
  (let ((unit (get-from *units* 'id unit-id)))
    (when unit
      (with-slots (name) unit
        (setf name new-name)))))

(defun chat-command (unit-id command)
  "Attempt to match a command based on /command target"
  (let* ((matches (nth-value 1 (cl-ppcre:scan-to-strings "^\/(.*)\\s(.*)" command)))
         (action (aref matches 0))
         (target (aref matches 1)))
    (cond ((equal action "nick") (change-unit-name unit-id target))))
  (resync unit-id))

(defun request-chat (json client)
  "Send in a json request to receive units chat"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (chat (cdr (assoc :chat json)))
         (chatter (slot-value (get-from *units* 'id unit-id) 'name))
         (msg (format nil "~a said, '~a'" chatter chat)))
    (if (cl-ppcre:scan "^\/" chat) (chat-command unit-id chat)
        (if (and (> (length chat) 0) (not (cl-ppcre:scan "[^A-Za-z0-9? _\\-!.,/'\"+]" chat)))
            (broadcast "request-chat"
                       (list (cons :unit--id unit-id)
                             (cons :chat msg)))
            (no-json-response)))))

(defun save-units-to-db ()
  "Write a query of data into the db"
  (dolist (unit *units*)
    (with-slots (id hp mp atk def mag mdef job_id level xp jp speed x y) unit
      (1query (format nil "update unit set hp='~a', mp='~a', atk='~a', def='~a', mag='~a', mdef='~a', job_id='~a', level='~a', xp='~a', jp='~a', speed='~a', x='~a', y='~a' where id=~a" hp mp atk def mag mdef job_id level xp jp speed x y id)))))

(defun do-db-snapshot ()
  "Compare current time with last db time"
  (let ((time (get-universal-time)))
    (when (> (- time *last-db-snapshot-time*) *db-snapshot-interval*)
      (setf *last-db-snapshot-time* time)
      (save-units-to-db))))

(defun get-next-id (dataset)
  "Get the next highest id on a dataset"
  (let ((id 0))
    (loop for item in dataset when (> (slot-value item 'id) id)
       do (setf id (slot-value item 'id)))
    (incf id)))

(defun create-new-unit (account-id &optional name)
  "Creates a new unit to be tied to an account"
  (let ((id (get-next-id *units*))
        (name (or name (concatenate 'string "tester-" (prin1-to-string account-id))))
        (hp 10)
        (mp 10)
        (atk 1)
        (def 1)
        (active 1)
        (mag 1)
        (mdef 1)
        (owner account-id)
        (job_id 3)
        (level 1)
        (xp 0)
        (jp 0)
        (speed 1)
        (x *map-min-size*)
        (y *map-min-size*)
        (img "witch-front.png"))
    (add-unit (list id name hp mp atk def active mag mdef owner job_id
                    level xp jp speed x y img)) id))

(defun create-new-account (client-ip)
  "Create a new account for player that is tied to IP address"
  (unless (get-from *accounts* 'ip client-ip)
    (let* ((id (get-next-id *accounts*))
           (user (concatenate 'string "tester-" (prin1-to-string id)))
           (pass "tester")
           (ip client-ip)
           (banned 0)
           (uid (create-new-unit id)))
      (give-deck uid)
      (add-account (list id user pass ip banned uid)) id)))

(defun request-signin (json client)
  "For right now just bind accounts to IP"
  (declare (ignore json))
  (let ((client-ip (client-host client)))
    (let ((account (get-from *accounts* 'ip client-ip)))
      (if (not account) (progn (create-new-account client-ip)
                               (request-signin client-ip client))
          (let ((unit-id (slot-value account 'uid)))
            (emit "start-game"
                  (unit-to-json (get-from *units* 'id unit-id))
                  client))))))

(defun create-npc (job-id &key map-index level npc-name sx sy)
  "Create a non player character (as known by owner 0) and set name, job and level"
  (let* ((job (nth job-id *jobs*)) ;; (get-from *jobs* 'id job-id))
         (job-id (slot-value job 'id))
         (level (or level 0))
         (sx (or sx *map-min-size*))
         (sy (or sy *map-min-size*))
         (map-index (or map-index 0)))
    (if (not job) "Job not found!"
        (let* ((unit-id (create-new-unit 0 npc-name))
               (unit (get-from *units* 'id unit-id)))
          (with-slots (name job_id img x y map_index) unit
            (when (not npc-name) (setf name (concatenate 'string "npc-" (prin1-to-string unit-id))))
            (setf ;; set the appropriate job related fields
             x sx
             y sy
             img (slot-value job 'img)
             map_index map-index
             job_id job-id))
          (dotimes (x level) (level-up unit-id)) ;; level up the unit
          (give-deck unit-id)
          (dotimes (x *max-hand-size*) (draw-card-controller unit-id 0))
          unit))))

(defun request-change-job (json client)
  "Json request to change unit job"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (job-id (cdr (assoc :job--id json))))
    (change-job unit-id job-id)
    (resync unit-id)))

(defun job-stat-checker (unit comparison stat value)
  "Checks something similar to a bash style check"
  (let ((unit-value (slot-value unit (intern (string-upcase stat) :ahungry-tactics))))
    (when (not (integerp value)) (setf value (parse-integer value)))
    (cond ((equal "gt" comparison) (> unit-value value))
          ((equal "ge" comparison) (>= unit-value value))
          ((equal "lt" comparison) (< unit-value value))
          ((equal "le" comparison) (<= unit-value value))
          ((equal "eq" comparison) (= unit-value value))
          ((equal "ne" comparison) (not (= unit-value value)))
          (t t))))

(defun job-eligibility (unit job)
  "Make check on a unit and a job reqs"
  (when (and unit job)
    (let ((requirements (split-sequence:split-sequence #\, (slot-value job 'reqs)))
          (eligible t))
      (dolist (requirement requirements)
        (let* ((reqs (split-sequence:split-sequence #\: requirement))
               (stat (first reqs))
               (scanned (nth-value 1 (cl-ppcre:scan-to-strings "(\\w\\w)(.*)" (second reqs))))
               (comparison (aref scanned 0))
               (value (aref scanned 1)))
          (when (not (job-stat-checker unit comparison stat value))
            (setf eligible nil))))
      (when eligible t))))

(defun get-eligible-jobs (unit-id)
  "Parse the special reqs format and filter
returned jobs based on eligibilty"
  (let ((unit (get-from *units* 'id unit-id)))
    (loop for job in *jobs* when (job-eligibility unit job)
       collect job)))

(defun request-jobs (json client)
  "Return a listing of jobs to the user"
  (let* ((unit-id (cdr (assoc :unit--id json))))
    (emit "request-jobs"
          (mapcar #'job-to-json (get-eligible-jobs unit-id))
          client)))

(defun move-card (object unit-id giving-to card-id)
  "Take a card from one location and give to another location"
  (when (and (remove-card-from object unit-id card-id)
             (> giving-to 0))
    (add-card-to object giving-to card-id)))

(defun request-give-card (json client)
  "Attempt to send a card from one person to another"
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (card-id (cdr (assoc :card--id json)))
         (giving-to (cdr (assoc :giving--to json))))
    (move-card *decks* unit-id giving-to card-id)
    (json-response "deck-updated" (new-js ("unit_id" unit-id) ("card_name" "bandage"))
                   "deck-updated" (new-js ("unit_id" giving-to) ("card_name" "bandage")))))

(defun in-range (sx sy dx dy max-range &optional min-range)
  "Perform a range check using x/y values"
  (let ((min-range (or min-range 0))
        (distance-x (abs (- sx dx)))
        (distance-y (abs (- sy dy))))
    (<= min-range (+ distance-x distance-y) max-range)))

(defun ai-movement (sx sy dx dy range)
  "Find the best place to have the unit move given available max range
Offset the panel slightly to avoid huge overlap"
  (let* ((x-offset (random 2))
         (y-offset (random 2))
         (dx (if (evenp x-offset) (+ dx x-offset) (- dx x-offset)))
         (dy (if (evenp y-offset) (+ dy y-offset) (- dy y-offset))))
    (dotimes (x range)
      (cond ((> sx dx) (decf sx))
            ((< sx dx) (incf sx))
            ((> sy dy) (decf sy))
            ((< sy dy) (incf sy)))) (list sx sy)))

(defun ai-choose-a-card (unit-id target-id)
  "Have the unit pick which card they will use"
  (let* ((unit (get-from *units* 'id unit-id))
         (target (get-from *units* 'id target-id))
         (sx (slot-value unit 'x))
         (sy (slot-value unit 'y))
         (dx (slot-value target 'x))
         (dy (slot-value target 'y))
         (hand (slot-value (get-from *hands* 'unit-id (slot-value unit 'id)) 'cards)))
    (remove nil (loop for card in hand
                   collect (with-slots (min_range max_range atk) (get-from *cards* 'id card)
                             (when (and (in-range sx sy dx dy max_range min_range)
                                        (> atk 0)) card))))))

(defun ai-choose-target (unit-id &optional removee)
  "Find the nearest player or enemy to attack - Returns id to attack
This should also avoid selecting units with 0 hp as its not good
to target a despawned unit for any reason"
  (let ((unit (get-from *units* 'id unit-id))
        (removee (or removee 0)))
    (with-slots (x y map_index) unit
      (remove removee (sort ;; Remove owner 0 (npc) from list by default
                       (loop for target in *units* ;; Sort by closest unit
                          when (and (eq (slot-value target 'map_index)
                                        map_index)
                                    (> (slot-value target 'hp) 0))
                          collect (list :id (slot-value target 'id)
                                        :distance (+ (abs (- x (slot-value target 'x)))
                                                     (abs (- y (slot-value target 'y))))
                                        :x (slot-value target 'x)
                                        :y (slot-value target 'y)
                                        :owner (slot-value target 'owner)))
                       #'< :key #'cadddr) :key (lambda (list) (getf list :owner))))))

(defun ai-controller ()
  "Run through our NPC's and choose if they should attack or move"
  (mapcar
   (lambda (npc)
     (with-slots (id x y hp) npc
       (when (> hp 0) ;; If the NPC is dead, don't do anything
         (let ((targets (ai-choose-target id)))
           (when targets
             (let ((target (nth (random (length targets)) targets)))
               (apply #'move (cons id (ai-movement x y
                                                   (getf target :x)
                                                   (getf target :y)
                                                   *max-move*)))))))))
   (get-matches *units* 'owner 0)))

(defun ai-time-check ()
  "Keep the time for AI moves reasonable to avoid insanity"
  (let ((time (get-universal-time)))
    (when (> (- time *last-ai-time*) *ai-interval*)
      (setf *last-ai-time* time)
      (ai-controller))))

(defun request-move (json client)
  "Let everyone know where someone has moved"
  (declare (ignore client))
  (let* ((unit-id (cdr (assoc :unit--id json)))
         (nx (cdr (assoc :x json)))
         (ny (cdr (assoc :y json)))
         (unit (get-from *units* 'id unit-id)))
    (when unit (with-slots (x y) unit
                 (setf x nx y ny)))
    (broadcast "update-unit" (unit-to-json unit))))

(defun unit-base-damage (attacker defender)
  "Check the owner (0 npc, >0 player) to see which to apply"
  (if (and (not (eq attacker 0))
           (eq defender 0))
      3 1))

(defun unit-physical-attack (attacker-id defender-id)
  "Have one person hit another"
  (let* ((attacker (get-from *units* 'id attacker-id))
         (defender (get-from *units* 'id defender-id))
         (atk (slot-value attacker 'atk))
         (def (slot-value defender 'def))
         (base (unit-base-damage (unit-owner attacker) (unit-owner defender)))
         (damage-done (damage-formula atk def base)))
    (with-slots (hp) defender
      (with-slots (owner xp jp) attacker
        (decf hp damage-done)
        (when (and (> owner 0) (< hp 1)) ;; XP only awarded on a kill
          (incf xp (xp-formula (slot-value attacker 'level)
                               (slot-value defender 'level))))
        (incf jp)))
    (emit "event-log"
          (list (format nil "You hit ~a for ~d damage!~%"
                        (slot-value defender 'name) damage-done))
          (unit-client attacker-id))
    (when (< (slot-value defender 'hp) 1)
      ;; Currently items go from 4 to 10
      (item-spawn (+ 4 (random 7)) ;; When something dies, spawn a random item
                  (slot-value defender 'map_index)
                  :x (slot-value defender 'x)
                  :y (slot-value defender 'y))
      (emit "event-log"
            (list (format nil "You defeated ~a!~%" (slot-value defender 'name)))
            (unit-client attacker-id)))
    (emit "event-log" (list (format nil "~a hit you for ~d damage!~%"
                                    (slot-value attacker 'name) damage-done))
          (unit-client defender-id))
    (when (< (slot-value defender 'hp) 1)
      (emit "event-log"
            (list (format nil "~a has defeated you! You are defeated!~%" (slot-value attacker 'name)))
            (unit-client defender-id)))
    (broadcast "update-unit" (unit-to-json defender))
    (broadcast "update-unit" (unit-to-json attacker))))

(defun unit-damage-cycle ()
  "Loop through all units and if any are in range of each other,
begin hitting and doing damage among the units if they are
NPCs (players can use the button to swing weapon etc. to do damage)"
  (when (> (length *units*) 1)
    (loop for u in *units* do
         (let* ((owner (slot-value u 'owner))
                (id (slot-value u 'id))
                (target-id (getf (car (ai-choose-target id owner)) :id))
                (target (get-from *units* 'id target-id)))
           (when (and (> (slot-value u 'map_index) 0) ;; are we on a map other than intro?
                      (> (slot-value u 'hp) 0)        ;; is attacker alive?
                      (eq owner 0)                    ;; only NPCs get auto attack
                      target                          ;; do we have someone to attack?
                      (> (slot-value target 'hp) 0)   ;; is target alive?
                      (eq (slot-value u 'map_index)   ;; are they on same map?
                          (slot-value target 'map_index))
                      (in-range (slot-value u 'x)     ;; are they in range?
                                (slot-value u 'y)
                                (slot-value target 'x)
                                (slot-value target 'y)
                                *combat-range*))
             (unit-physical-attack id target-id))))))

(defun unit-reset (unit-id)
  "Reset a player unit when a player requests it"
  (let ((unit (get-from *units* 'id unit-id)))
    (with-slots (id level hp mp map_index atk def mag mdef) unit
      (setf level 1 hp 10 mp 10 map_index 0 atk 1 def 1 mag 1 mdef 1)
      (mapcar (λ α → (setf (slot-value α 'worn) 0)) ;; Also unequip all items
              (get-matches *items* 'owner id)))
    unit))

(defun request-reset (json client)
  "Someone died, so let them restart the game"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (unit-reset unit-id))
  (emit "force-reload" json client))

(defun unit-baseline-hp (level job-id)
  "Determine the baseline/max hp for a unit based on job"
  (let* ((job (get-from *jobs* 'id job-id))
         (jhp (slot-value job 'hp)))
    (+ 10 (* level 5) (* jhp level))))

(defun unit-defeated-check ()
  "For npcs that are defeated, have a small chance to respawn them"
  (loop for npc in (get-matches *units* 'owner 0)
     do (with-slots (hp level job_id) npc
          (when
              (and (<= hp 0) ;; negative hp
                   (>= 5 (random 100))) ;; a 5% chance to come alive
            (setf hp (unit-baseline-hp level
                                       job_id)))))) ;; give them some hp

(defun event-log-send-and-clear (id)
  "Grab an event log and wipe it out as it sends to user"
  (let ((out (gethash id *event-logs*)))
    (setf (gethash id *event-logs*) (list (print-game-time)))
    (nreverse out)))

(defun request-event-log (json client)
  "Send out the event log to those who request it.
There is an issue with event-log failing when unit-id does not come through
for some reason, if this happens, handle gracefully"
  (let ((unit-id (cdr (assoc :unit--id json))))
    (when unit-id
      (emit "event-log" (event-log-send-and-clear unit-id) client))))

(defun unit-by-ip-address (ip)
  "Grab a unit by lookup via ip"
  (let ((account (get-from *accounts* 'ip ip)))
    (when account
      (get-from *units* 'id (slot-value account 'uid)))))

(defun unit-js ()
  "The unit JS"
  (ps

    (defvar enemy-types ;; corresponds to job_id basically - animation stuff
      (array
       (create img "units/witch-front.png" ;; id 0 can be blank
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/skeleton-front.png" ;; skeleton
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/slime-front.png" ;; slime
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; peasant
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/mermaid-front.png" ;; mermaid
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (8)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (9)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (10)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (11)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/robot-front.png" ;; robot (12)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; witch (13)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/blonde-front.png" ;; amazon (14)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (15)
               move-speed 0.05 rot-speed 3 total-states 4)
       (create img "units/witch-front.png" ;; blank (16)
               move-speed 0.05 rot-speed 3 total-states 4)))

    (defvar map-enemies ;; our actual enemies - overwritten when socket starts
      (array (create job_id 1 x 20 y 20 unit_id 0)
             (create job_id 1 x 20 y 20 unit_id 0)))

    (defvar enemy-shadows [])

    (defvar player (create          ;; our player - overwritten when socket starts
                    name "Self"
                    job_id 1
                    x 20            ;; current x, y position of player
                    y 20
                    dir 0           ;; current direction, either -1 for left or 1 for right
                    rot-deg 225
                    rot 180         ;; current angle of rotation
                    speed 0         ;; the player forward = 1 or backwards = -1
                    move-speed 0.15 ;; how far (map units) the player moves each step
                    rot-speed 4))   ;; how much rotation each step

    (defvar *my-id*)                 ;; Store the user ID here for future reference
    (defvar shadow (create x 0 y 0)) ;; Store a copy of user shadow location for animation

    (defun unit-controller (&optional event data)
      "Handle unit controller events"
      (funcall (λλ
                "resync"      → (unit-resync data)
                "update-unit" → (unit-add-or-update data)
                α             → (chain console (log "unit-controller called")))
               event))

    (defun unit-add-or-update (unit)
      ;; This implies it was sent from node and is stuck in an extra array
      (when (equal (typeof (@ unit id)) "undefined")
        (setf unit (aref unit 0)))
      ;; If the unit id belongs to self, update self accordingly
      (if (eq (@ unit id) *my-id*)
          (unit-update-self unit)
          ;; Verify unit is on the same map index as the player
          (when (eq (@ unit map_index) (@ player map_index))
            ;;(c "Map check success")
            ;; Check if there is existing data
            (let ((existing (loop for enemy in enemies
                               when (eq (@ enemy id) (@ unit id))
                               collect unit)))
              (if (> (length existing) 0)
                  (unit-update unit)
                  (unit-add-new unit))))))

    (ƒƒ unit-request-give-coords
        α → (progn
              (when (or (not (= (@ shadow x) (@ player x)))
                        (not (= (@ shadow y) (@ player y))))
                (sock-emit "request-move"
                           (create unit_id *my-id*
                                   x (ash (@ player x) 0)
                                   y (ash (@ player y) 0)))
                (with-slots (x y) shadow
                  (setf x (@ player x) y (@ player y))))
              (set-timeout unit-request-give-coords 1000)))

    (defvar set-coords-update-p nil)
    (ƒƒ unit-update-self
        α → (with-slots (hp mp map_index x y name) α
              (setf (@ player hp) hp
                    (@ player mp) mp
                    (@ player map_index) map_index
                    (@ player name) name)
              (unit-update-self-stats α)
              (unless set-coords-update-p
                (progn (setf set-coords-update-p t)
                       (unit-request-give-coords)))))

    (ƒƒ unit-update-self-stats
        α → (with-slots (name hp mp atk def mag mdef map_index level xp jp) α
              (chain ($ "#p-name") (html name))
              (chain ($ "#p-hp") (html hp))
              (chain ($ "#p-mp") (html mp))
              (chain ($ "#p-atk") (html atk))
              (chain ($ "#p-def") (html def))
              (chain ($ "#p-mag") (html mag))
              (chain ($ "#p-mdef") (html mdef))
              (chain ($ "#p-level") (html level))
              (chain ($ "#p-xp") (html xp))
              (chain ($ "#p-jp") (html jp))
              (chain ($ "#p-map") (html map_index))))

    (ƒƒ unit-resync
        α → (progn (chain ($ ".enemy") (remove))
                   (setf map-enemies
                         (loop for unit in α
                            if (not (eq (@ unit id) *my-id*))
                            collect unit
                            else do (unit-update-self unit)))
                   (init-enemies)))

    (ƒƒ unit-update
        (eq (@ α id) *my-id*) → (unit-update-self α)
        α → (progn
              (loop for unit in enemies
                 when (eq (@ unit id) (@ α id))
                 do (with-slots (id x y hp img) unit
                      (if (< (@ α hp) 1)
                          (progn
                            (setf (aref enemy-shadows id)
                                  (create dx -1
                                          dy -1))
                            (setf x -1
                                  y -1
                                  (@ img display) "none"))
                          (progn ;; else
                            (setf (aref enemy-shadows id) ;; Update for movement animation
                                  (create dx (@ α x)
                                          dy (@ α y)))
                            (setf hp (@ α hp)))))))) ;; Set the HP of unit

    (ƒƒ unit-add-new
        α → (sock-emit "request-resync"
                       (create unit_id *my-id*)))

    (ƒƒ reset-game
        α → (sock-emit "request-reset" (create unit_id *my-id*)))

    (ƒƒ game-over
        α → (progn (chain ($ "#loading") (css (create background "maroon")))
                   (set-timeout reset-game 10000)
                   (chain ($ "#loading") (html "You have been defeated!
Click <a href='#' onclick='resetGame()'>here</a> to restart!"))
                   (chain ($ "#loading") (fade-in 5000))))

    ;;(set-interval (λλ α → (when (and *my-id* socket-clws)
    ;;                       (sock-emit "request-event-log"
    ;;                                 (create unit_id *my-id*)))) 1000)

    (defun in-range (sx sy dx dy max-range &optional min-range)
      "Perform a range check using x/y values"
      (let ((min-range (or min-range 0))
            (distance-x (abs (- sx dx)))
            (distance-y (abs (- sy dy))))
        (<= min-range (+ distance-x distance-y) max-range)))

    (defun game-over-check ()
      (if (< (@ player hp) 1)
          (game-over)
          (set-timeout game-over-check 1000)))

    (set-timeout game-over-check 3000)

    nil))
