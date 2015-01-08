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

;;;; pseudo.lisp

(in-package #:pseudo)

;;; "pseudo" goes here. Hacks and glory await!

(setf *js-string-delimiter* #\")
(setf *ps-print-pretty* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro+ps style-setter (val default &optional extra)
    "Set up to create the if blocks that set oldStyles and style"
    `(when (not (equal (@ old-styles ,val) ,default))
       (setf (@ style ,val) ,(if extra `(+ ,default ,extra) default)
             (@ old-styles ,val) ,default))))

(defun web-home ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Pseudo")
      (:link :type "text/css" :rel "stylesheet/less" :href "css/main.less")
      (:script :src "js/jquery-1.10.2.min.js")
      (:script :src "js/less-1.4.1.min.js")
      (:script (ps (setf less (create env "development"))))
      (:script
       :type "text/javascript"
       (str (ps
              (defvar *ok* "Welcome")
              (defvar holder-state 0)
              (defun loading-animation ()
                "Show the loading screen animation"
                (unless (= holder-state -1)
                  (let ((total-states 4)
                        (speed 1000))
                    (incf holder-state)
                    (when (>= holder-state total-states)
                      (setf holder-state 0))
                    (chain ($ ".holder img") (css (create "opacity" 1
                                                          "margin-left"
                                                          (+ (* -1 64 holder-state) "px"))))
                    (set-timeout loading-animation (/ speed total-states)))))

              (set-timeout loading-animation 200)

              )))
      (:body
       (:div :id "content"
             (:h1 "Welcome to Pseudo!")
             (:div :id "loading"
                   (:br)
                   (:div :class "holder" (:img :src "units/witch-front.png"))
                   (:div :class "holder" (:img :src "units/mermaid-front.png"))
                   (:div :class "holder" (:img :src "units/blonde-front.png"))
                   (:div :class "holder" (:img :src "units/skeleton-front.png"))
                   (:div :class "holder" (:img :src "units/slime-front.png"))
                   (:div :class "holder" (:img :src "units/robot-front.png")))
             (:a :id "play-game" :href "/pseudo.html" "Play the game!")
             (:h2 "What is Pseudo?")
             (:p "Pseudo is a 'roguelike' game (think rogue, dungeon
crawl: stone soup, pixel dungeon, diablo).  The objective is to make
it as far as you can without dying (isn't that the point in every
game?).")
             (:img :src "/img/pseudo1.png" :class "screenie")
             (:p "However, unlike your traditional game, you are
likely to (and expected) to die quite frequently.  Dying will reset
your level back to the base level - however if you can avoid dying,
you will be able to level up, gain new powers and items, and
eventually conquer the dungeon.")
             (:img :src "/img/pseudo2.png" :class "screenie")
             (:h2 "How does it work?")
             (:p "Pseudo is played in the browser with other players,
using HTML5 websockets.")
             (:p "All the code is open source (AGPLv3) and available
on my Github at")
             (:a :href "https://github.com/ahungry/pseudo"
                 "https://github.com/ahungry/pseudo")
             (:img :src "/img/pseudo3.png" :class "screenie")
             (:h2 "Can I read more about it?")
             (:p "You sure can, check out some of the blog articles I've been
discussing it in at:")
             (:ul
              (:li (:a :href "http://ahungry.com/blog/2013-11-29-New-Sprites-for-Pseudo.html"
                       "New Sprites for Pseudo [2013-11-29]"))
              (:li (:a :href "http://ahungry.com/blog/2013-11-06-Pseudo-an-almost-3d-roguelike.html"
                       "Pseudo, an almost 3d roguelike [2013-11-06]"))
              )
             (:img :src "/img/anim.gif")
             ))))))

(defjs blub ()
  (alert "BLUB!"))

(defun web-assets ()
  (let ((player (unit-by-ip-address (remote-addr*))))
    (with-html-output-to-string (s)
      (:html
       (:head
        (:title "Pseudo")
        (:link :type "text/css" :rel "stylesheet/less" :href "css/pseudo.less")
        (:script :src "js/jquery-1.10.2.min.js")
        (:script :src "js/less-1.4.1.min.js")
        (:script :src (lisp *defjs-url*))
        (:script (defjs:loader))
        (:script (ps (setf less (create env "development"))))
        (:script
         (str
          (ps
            ;; helper functions
            (defvar $$ (λλ α → (chain document (get-element-by-id α))))
            (defvar dc (λλ α → (chain document (create-element α)))))))
        (:script (str (socket-js))) ;; socket functions
        (:script (str (ps (defvar *map*))))
        (:script (str (map-js)))    ;; map functions
        (:script (str (unit-js)))   ;; unit functions
        (:script (str (item-js)))   ;; item functions
        (:script (str (chat-js)))   ;; chat functions
        (:script (str (funcall (λ 0 → (ps
                                        (defvar ceiling-color "blue")
                                        (defvar floor-color "#000")
                                        (defvar +floor-texture+ "/walls.png")
                                        (defvar wall-textures
                                          (array
                                           "tiles/wallvines_32cols_256.png"
                                           "/grass.png"
                                           "tiles/stonefloor_32cols_256.png"
                                           "tiles/wallvines_32cols_256.png"
                                           )))
                                  α → (ps
                                        (defvar ceiling-color "#999")
                                        (defvar floor-color "red")
                                        (defvar +floor-texture+ "/tiles/wallvines_32cols_256.png")
                                        (defvar wall-textures
                                          (array
                                           "tiles/wallvines_32cols_256.png"
                                           "/grass.png"
                                           "tiles/stonefloor_32cols_256.png"
                                           "tiles/stonefloor_32cols_256.png"
                                           ))))
                               (if player (slot-value (unit-by-ip-address (remote-addr*)) 'map_index) 0))))
        (:script
         (str
          (ps

            (defvar *combat-range* 2)
            (defvar map-width 0)
            (defvar map-height 0)
            (defvar mini-map-scale 10) ;; more like the block size
            (defvar screen-width (* 0.99 (@ window inner-width)))
            (defvar screen-height (* 0.99 (@ window inner-height)))
            (defvar show-overlay true)
            (defvar strip-width 4)
            (defvar strip-height 8)
            (defvar fov (* 75 (/ pi 180)))
            (defvar num-rays (ceiling (/ screen-width strip-width)))
            (defvar fov-half (/ fov 2))
            (defvar view-dist (/ (/ screen-width 2) (tan (/ fov 2))))
            (defvar two-PI (* pi 2))
            (defvar num-textures 4)
            (defvar user-agent (chain navigator user-agent (to-lower-case)))
            (defvar is-gecko (and (not (equal (chain user-agent (index-of "gecko")) -1))
                                  (equal (chain user-agent (index-of "safari")) -1)))
            (defvar use-single-texture); is-gecko)
            (defvar screen-strips ([]))
            (defvar floor-strips ([]))
            (defvar overlay)
            (defvar fps 0)
            (defvar overlay-text "")
            (defvar pause-render-p t)
            (defvar debug-floor-cast "")

            (defvar once-init-p nil)
            (defun once-init ()
              "Only do these things once per game"
              (unless once-init-p
                (setf once-init-p t)
                (init-screen)
                (bind-keys)
                (game-cycle)
                ;; (day-time)
                (chain ($ "#right-pane")
                       (hover (lambda () (chain ($ this) (css (create opacity 1))))
                              (lambda () (chain ($ this) (css (create opacity .5))))))
                (chain ($ "#items")
                       (hover (lambda () (items-show-pane))
                              (lambda () (items-hide-pane))))
                (render-cycle)))

            (defun init ()
              "Initialize"
              (when (not *map*) (set-timeout init 1000)
                    (return-from init))
              (once-init)
              (setf (@ ($$ "screen") style width) screen-width
                    (@ ($$ "screen") style height) screen-height)
              (setf map-width (chain (aref *map* 0) length))
              (setf map-height (@ *map* length))
              (init-sprites)
              (init-enemies)
              (chain ($ "#ceiling") (css (create background ceiling-color)))
              (chain ($ "#floor, #shade1") (css (create background floor-color)))
              (draw-mini-map)
              (setf pause-render-p nil)
              (chain ($ "#loading") (fade-out))
              (setf holder-state -1))

            (defun day-time ()
              (chain ($ ".lighting") (fade-out 5000))
              (set-timeout night-time 15000))

            (defun night-time ()
              (chain ($ ".lighting") (fade-in 5000))
              (set-timeout day-time 15000))

            (defvar enemies [])

            (defun init-enemies ()
              (chain ($ ".enemy") (remove))
              (setf enemies []) ;; enemies needs to be cleared before pushing more in
              (defvar screen ($$ "screen"))
              (loop for i from 0 below (@ map-enemies length) do
                   (progn
                     (defvar enemy (aref map-enemies i))
                     (when (> (@ enemy hp) 0)
                       (defvar type (aref enemy-types (@ enemy job_id)))
                       (defvar img (dc "img"))
                       (setf (@ img src) (@ type img)
                             (@ img style display) "none"
                             (@ img class-name) "enemy"
                             (@ img style position) "absolute")
                       (with-slots (state rot rot-deg dir speed move-speed
                                          rot-speed total-states old-styles
                                          img) enemy
                         (setf state 0
                               rot 0
                               rot-deg 0
                               dir 0
                               speed 0
                               move-speed (@ type move-speed)
                               rot-speed (@ type rot-speed)
                               total-states (@ type total-states)
                               old-styles (create
                                           left 0
                                           top 0
                                           width 0
                                           height 0
                                           clip ""
                                           display "none"
                                           z-index 0)))
                       (setf (@ enemy img) img)
                       (chain enemies (push enemy))
                       (chain screen (append-child img))))))

            (defvar sprite-map ([]))
            (defvar visible-sprites ([]))
            (defvar old-visible-sprites ([]))

            (defvar *init-sprites-p* nil)

            (defun init-sprites ()
              "If map isn't initialized, try again in a little bit"
              (when (or (not *map*)
                        (not item-types)
                        *init-sprites-p*)
                (set-timeout init-sprites 100)
                (return-from init-sprites))

              (setf *init-sprites-p* t)
              (chain ($ ".sprite") (remove))
              (setf visible-sprites [])
              (setf sprite-map [])
              (loop for y from 0 below (@ *map* length) do
                   (setf (aref sprite-map y) ([])))
              (defvar screen ($$ "screen"))
              (loop for i from 0 below (@ map-items length) do
                   (progn
                     (defvar sprite (aref map-items i))
                     (defvar item-type (aref item-types (@ sprite type)))
                     (defvar img (dc "img"))
                     (setf (@ img src) (@ item-type img)
                           (@ img style display) "none"
                           (@ img style position) "absolute"
                           (@ img class-name) "sprite")
                     (setf (@ sprite visible) false
                           (@ sprite block) (@ item-type block)
                           (@ sprite img) img)
                     (when (and sprite-map
                                (aref sprite-map (@ sprite y)))
                       (setf (aref sprite-map (@ sprite y) (@ sprite x)) sprite))
                     (chain screen (append-child img))))
              (setf *init-sprites-p* nil))

            (defvar last-game-cycle-time 0)
            (defvar game-cycle-delay (/ 1000 30))

            (defun game-cycle ()
              "The default game cycle / animation sequence"
              (defvar now (new (chain (-date) (get-time))))
              (defvar time-delta (- now last-game-cycle-time)) ;; Since last game logic
              (move player time-delta)
              (ai time-delta)
              (defvar cycle-delay game-cycle-delay) ;; timer will not happen that fast
              (when (> time-delta cycle-delay)
                (setf cycle-delay (max 1 (- cycle-delay (- time-delta cycle-delay)))))
              (set-timeout game-cycle cycle-delay)
              (setf last-game-cycle-time now))

            (defun ai (time-delta)
              "For now disable on client side - will come soon on other end"
              (loop for i from 0 below (@ enemies length) do
                   (progn
                     (defvar enemy (aref enemies i))
                     (when (not (aref enemy-shadows (@ enemy id))) (return-from ai))
                     (defvar dx (- (@ (aref enemy-shadows (@ enemy id)) dx) (@ enemy x)))
                     (defvar dy (- (@ (aref enemy-shadows (@ enemy id)) dy) (@ enemy y)))
                     (defvar dist (sqrt (+ (* dx dx) (* dy dy))))
                     (with-slots (rot-deg rot speed img total-states state job_id) enemy
                       (let ((sprite-src (@ (aref enemy-types job_id) img))
                             (sprite-states (@ (aref enemy-types job_id) total-states)))
                         (if (> dist .5)
                             (progn
                               (defvar angle (atan dy dx))
                               (setf rot-deg (* angle (/ 180 pi))
                                     rot angle
                                     speed 1)
                               (when (not (= total-states sprite-states))
                                 (setf (@ img src) sprite-src
                                       total-states sprite-states
                                       state 0))
                               (defvar walk-cycle-time 1500)
                               (defvar num-walk-sprites (1- sprite-states))
                               (setf (@ enemy state)
                                     (1+ (floor (/ (rem (new (-date)) walk-cycle-time)
                                                   (/ walk-cycle-time num-walk-sprites))))))
                             (progn
                               (when (not (= total-states sprite-states))
                                 (setf (@ img src) sprite-src
                                       speed 0
                                       total-states sprite-states
                                       state 0))
                               (defvar walk-cycle-time 3000)
                               (defvar num-walk-sprites (1- sprite-states))
                               (setf (@ enemy state)
                                     (1+ (floor (/ (rem (new (-date)) walk-cycle-time)
                                                   (/ walk-cycle-time num-walk-sprites)))))))))
                     (move (aref enemies i) time-delta))))

            (defvar last-render-cycle-time 0)

            (defun render-cycle ()
              (unless pause-render-p
                (update-mini-map)
                (clear-sprites)
                (cast-rays)
                (render-sprites)
                (render-enemies))
              ;; time since last render
              (defvar now (new (chain (-date) (get-time))))
              (defvar time-delta (- now last-render-cycle-time))
              (defvar cycle-delay (/ 1000 30))
              (when (> time-delta cycle-delay)
                (setf cycle-delay (max 1 (- cycle-delay (- time-delta cycle-delay)))))
              (setf last-render-cycle-time now)
              (set-timeout render-cycle cycle-delay)
              (setf fps (/ 1000 time-delta))
              (when show-overlay (update-overlay)))

            (defun clear-sprites ()
              "clear visible sprites array but keep a copy for later in old-visible-sprites
also mark all sprites as not visible so they can be added during raycasting"
              (setf old-visible-sprites ([]))
              (loop for i from 0 below (@ visible-sprites length) do
                   (progn
                     (defvar sprite (aref visible-sprites i))
                     (setf (aref old-visible-sprites i) sprite
                           (@ sprite visible) false)))
              (setf visible-sprites ([])))

            (defun render-sprites ()
              (loop for i from 0 below (@ visible-sprites length) do
                   (progn
                     (defvar sprite (aref visible-sprites i))
                     (defvar img (@ sprite img))
                     (setf (@ img style display) "block")
                     ;; translate position to viewer space
                     (defvar dx (- (+ (@ sprite x) 0.5) (@ player x)))
                     (defvar dy (- (+ (@ sprite y) 0.5) (@ player y)))
                     ;; distance to sprite
                     (defvar dist (sqrt (+ (* dx dx) (* dy dy))))
                     ;; angle relative to viewing angle
                     (defvar sprite-angle (- (atan dy dx) (@ player rot)))
                     ;; size of sprite
                     (defvar size (/ view-dist (* (cos sprite-angle) dist)))
                     (when (<= size 0) (@ continue))
                     ;; x position on screen
                     (defvar x (* (tan sprite-angle) view-dist))
                     (setf (@ img style left) (+ (- (+ (/ screen-width 2) x) (/ size 2)) "px"))
                     ;; y is constant since all sprites keep height and vert pos
                     (setf (@ img style top) (+ (/ (- screen-height size) 2) "px")
                           (@ img style width) (+ size "px")
                           (@ img style height) (+ size "px"))
                     (defvar dbx (- (@ sprite x) (@ player x)))
                     (defvar dby (- (@ sprite y) (@ player y)))
                     (defvar block-dist (+ (* dbx dbx) (* dby dby)))
                     (setf (@ img style z-index) (* -1 (floor (* block-dist 1000))))))
              ;; hide sprites no longer visible
              (loop for i from 0 below (@ old-visible-sprites length) do
                   (progn
                     (defvar sprite (aref old-visible-sprites i))
                     (when (< (chain visible-sprites (index-of sprite)) 0)
                       (setf (@ sprite visible) false
                             (@ sprite img style display) "none")))))

            (defun render-enemies ()
              (loop for i from 0 below (@ enemies length) do
                   (progn
                     (defvar enemy (aref enemies i))
                     (when (> (@ enemy hp) 0)
                       (defvar img (@ enemy img))
                       (defvar dx (- (@ enemy x) (@ player x)))
                       (defvar dy (- (@ enemy y) (@ player y)))
                       (defvar angle (- (atan dy dx) (@ player rot)))
                       (when (< angle (* -1 pi)) (incf angle (* 2 pi)))
                       (when (>= angle pi) (decf angle (* 2 pi)))
                       ;; is enemy in front of player? use fov instead
                       (if (and (> angle (* (* -1 pi) 0.5)) (< angle (* pi 0.5)))
                           (progn
                             (defvar dist-squared (+ (* dx dx) (* dy dy)))
                             (defvar dist (sqrt dist-squared))
                             (defvar size (/ view-dist (* (cos angle) dist)))
                             (when (<= size 0) (@ continue))
                             (defvar x (* (tan angle) view-dist))
                             (defvar style (@ img style))
                             (defvar old-styles (@ enemy old-styles))
                             ;; if height is equal to sprite size
                             (unless (equal size (@ old-styles height))
                               (setf (@ style height) (+ size "px")
                                     (@ old-styles height) size))
                             ;; width equal to sprite size times total num states
                             (defvar style-width (* size (@ enemy total-states)))
                             (unless (equal style-width (@ old-styles width))
                               (setf (@ style width) (+ style-width "px")
                                     (@ old-styles width) style-width))
                             ;; top pos is halfway down screen minus a half sprite height
                             (defvar style-top (/ (- screen-height size) 2))
                             (unless (equal style-top (@ old-styles top))
                               (setf (@ style top) (+ style-top "px")
                                     (@ old-styles top) style-top))
                             ;; place at x pos, adjust for sprite size and current sprite state
                             (defvar style-left (- (- (+ (/ screen-width 2) x) (/ size 2)) (* size (@ enemy state))))
                             (unless (equal style-left (@ old-styles left))
                               (setf (@ style left) (+ style-left "px")
                                     (@ old-styles left) style-left))
                             (defvar style-z-index (ash (* -1 (* dist-squared 1000)) 0))
                             (unless (equal style-z-index (@ old-styles z-index))
                               (setf (@ style z-index) style-z-index
                                     (@ old-styles z-index) style-z-index))
                             (defvar style-display "block")
                             (unless (equal style-display (@ old-styles display))
                               (setf (@ style display) style-display
                                     (@ old-styles display) style-display))
                             (defvar style-clip (+ "rect(0, " (* size (1+ (@ enemy state)))
                                                   ", " size ", " (* size (@ enemy state)) ")"))
                             (unless (equal style-clip (@ old-styles clip))
                               (setf (@ style clip) style-clip
                                     (@ old-styles clip) style-clip)))
                           (progn
                             (defvar style-display "none")
                             (unless (equal style-display (@ enemy old-styles display))
                               (setf (@ img style display) style-display
                                     (@ enemy old-styles display) style-display))))))))

            (defun update-overlay ()
              (setf (@ overlay inner-H-T-M-L) (+ "FPS: " (chain fps (to-fixed 1)) "<br>" overlay-text)
                    overlay-text ""))

            (defun init-screen ()
              (defvar screen ($$ "screen"))
              (loop for i from 0 below screen-width by strip-width do
                   (progn
                     (defvar strip (dc "img"))
                     (with-slots (position height left top) (@ strip style)
                       (setf position "absolute"
                             height "0px"
                             left "0px"
                             top "0px"))
                     (when use-single-texture
                       (setf (@ strip src) (if (@ window opera)
                                               "/tiles/wallvines_32cols_256.png"
                                               "/tiles/wallvines_32cols_256.png")))
                     (setf (@ strip old-styles)
                           (create left 0 top 0 width 0 height 0 clip "" src ""))
                     (chain screen-strips (push strip))
                     (chain screen (append-child strip))
                     ))
              (loop for x from 0 below screen-width by strip-width do
                   (@ continue)
                   (progn
                     (let ((row-y []))
                       (loop for y from 0 below screen-height by strip-height do
                            (progn
                              (defvar floor-strip (dc "img"))
                              (with-slots (position height left top) (@ floor-strip style)
                                (setf position "absolute"
                                      height "0px"
                                      left "0px"
                                      top "0px"))
                              (setf (@ floor-strip src) +floor-texture+)
                              (setf (@ floor-strip old-styles)
                                    (create left 0 top 0 width 0 height 0 clip "" src ""))
                              (chain row-y (push floor-strip))
                              (chain screen (append-child floor-strip))))
                       (chain floor-strips (push row-y)))))
              ;; overlay div for adding text
              (setf overlay (dc "div")
                    (@ overlay id) "overlay"
                    (@ overlay style display) (if show-overlay "block" "none"))
              (chain screen (append-child overlay)))

            (defun cast-rays ()
              (defvar strip-idx 0)
              (loop for i from 0 below num-rays do
                   (progn
                     (defvar ray-screen-pos
                       (* (+ (/ (* -1 num-rays) 2) i) strip-width)) ;; Where does ray go through?
                     (defvar ray-view-dist
                       (sqrt (+ (* ray-screen-pos ray-screen-pos) (* view-dist view-dist))))
                     ;; the angle of the ray, relative to the viewing direction.
                     ;; right triangle: a = sin(A) * c
                     (defvar ray-angle (asin (/ ray-screen-pos ray-view-dist)))
                     (cast-single-ray (+ (@ player rot) ray-angle) strip-idx)
                     (incf strip-idx))))

            (defun cast-single-ray (ray-angle strip-idx)
              "make sure angle is between 0 and 360"
              (setf ray-angle (rem ray-angle two-PI))
              (when (< ray-angle 0) (incf ray-angle two-PI))
              ;; moving right/left? up/down? find the quadrant angle is in
              (defvar right (or (> ray-angle (* two-PI 0.75))
                                (< ray-angle (* two-PI 0.25))))
              (defvar up (or (< ray-angle 0) (> ray-angle pi)))
              (defvar wall-type 0)
              ;; only do once
              (defvar angle-sin (sin ray-angle))
              (defvar angle-cos (cos ray-angle))

              (defvar dist 0) ;; distance to block we hit
              (defvar x-hit 0) ;; coords of hit block
              (defvar y-hit 0)
              (defvar x-wall-hit 0)
              (defvar y-wall-hit 0)

              (defvar texture-x) ;; x - coord on texture of the block
              (defvar wall-x)
              (defvar wall-y)
              (defvar wall-is-shaded false)
              (defvar wall-is-horizontal false)

              ;; first check against the vertical map/wall lines
              ;; we do this by moving to the right or left edge of the block we're standing in
              ;; and then moving in 1 map unit steps horizontally. The amount we have to move vertically
              ;; is determined by the slope of the ray, which is simply defined as sin(angle) / cos(angle).

              (defvar slope (/ angle-sin angle-cos))
              (defvar d-x-ver (if right 1 -1))
              (defvar d-y-ver (* d-x-ver slope))
              (defvar x (if right (ceiling (@ player x)) (floor (@ player x))))
              (defvar y (+ (@ player y) (* (- x (@ player x)) slope)))

              (while (and (>= x 0) (< x map-width) (>= y 0) (< y map-height))
                (defvar wall-x (ash (+ x (if right 0 -1)) 0))
                (defvar wall-y (ash y 0))
                (when (and (aref sprite-map wall-y)
                           (aref sprite-map wall-y wall-x)
                           (not (chain (aref sprite-map wall-y wall-x) visible)))
                  (setf (chain (aref sprite-map wall-y wall-x) visible) true)
                  (chain visible-sprites (push (aref sprite-map wall-y wall-x))))
                ;; is this point in a wall block?
                (when (> (aref *map* wall-y wall-x) 0)
                  (defvar dist-x (- x (@ player x)))
                  (defvar dist-y (- y (@ player y)))
                  (setf dist (+ (* dist-x dist-x) (* dist-y dist-y)) ;; distance player to point squared
                        wall-type (aref *map* wall-y wall-x) ;; remember for later
                        texture-x (rem y 1)) ;; where are we on wall
                  (when (not right) (setf texture-x (- 1 texture-x))) ;; on left of map, reverse texture
                  (setf x-hit x y-hit y
                        x-wall-hit wall-x y-wall-hit wall-y
                        wall-is-shaded true wall-is-horizontal true)
                  (@ break))
                (setf x (+ x d-x-ver))
                (setf y (+ y d-y-ver)))
              ;; now check against horizontal lines. It's basically the same, just "turned around".
              ;; the only difference here is that once we hit a map block,
              ;; we check if there we also found one in the earlier, vertical run. We'll know that if dist != 0.
              ;; If so, we only register this hit if this distance is smaller.
              (defvar slope (/ angle-cos angle-sin))
              (defvar d-y-hor (if up -1 1))
              (defvar d-x-hor (* d-y-hor slope))
              (defvar y (if up (floor (@ player y)) (ceiling (@ player y))))
              (defvar x (+ (@ player x) (* (- y (@ player y)) slope)))

              (while (and (>= x 0) (< x map-width) (>= y 0) (< y map-height))
                (defvar wall-y (ash (+ y (if up -1 0)) 0))
                (defvar wall-x (ash x 0))
                (when (and (aref sprite-map wall-y)
                           (aref sprite-map wall-y wall-x)
                           (not (chain (aref sprite-map wall-y wall-x) visible)))
                  (setf (chain (aref sprite-map wall-y wall-x) visible) true)
                  (chain visible-sprites (push (aref sprite-map wall-y wall-x))))
                (when (and (aref *map* wall-y)
                           (> (aref *map* wall-y wall-x) 0))
                  (defvar dist-x (- x (@ player x)))
                  (defvar dist-y (- y (@ player y)))
                  (defvar block-dist (+ (* dist-x dist-x) (* dist-y dist-y)))
                  (when (or (not dist) (< block-dist dist))
                    (setf dist block-dist
                          x-hit x y-hit y
                          x-wall-hit wall-x y-wall-hit wall-y
                          wall-type (aref *map* wall-y wall-x)
                          texture-x (rem x 1)
                          wall-is-shaded false)
                    (when up (setf texture-x (- 1 texture-x))))
                  (@ break))
                (incf x d-x-hor)
                (incf y d-y-hor))
              (when dist
                (defvar strip (aref screen-strips strip-idx))
                ;; use perpendicular distance to adjust for fish eye
                ;; distorted_dist = correct_dist / cos(relative_angle_of_ray)
                (setf dist (sqrt dist)
                      dist (* dist (cos (- (@ player rot) ray-angle))))
                ;; calc position, height and width of strip
                (defvar height (round (/ view-dist dist)))
                (defvar width (* height strip-width))
                (defvar top (round (/ (- screen-height height) 2)))
                (defvar img-top 0)
                (defvar style (@ strip style))
                (defvar old-styles (@ strip old-styles))
                (defvar style-height)
                (if use-single-texture
                    (progn
                      (setf img-top (ash (* height (- wall-type 1)) 0))
                      (defvar style-height (ash (* height num-textures) 0)))
                    (progn
                      (defvar style-src (aref wall-textures (- wall-type 1)))
                      (when (not (eq (@ old-styles src) style-src))
                        (setf (@ strip src) style-src
                              (@ old-styles src) style-src))
                      (defvar style-height (ash height 0))))
                (style-setter height style-height "px")
                (defvar tex-x (round (* texture-x width)))
                (when (> tex-x (- width strip-width))
                  (setf tex-x (- width strip-width)))
                (incf tex-x (if wall-is-shaded width 0))
                (defvar style-width (ash (* width 2) 0))
                (style-setter width style-width "px")
                (defvar style-top (- top img-top))
                (style-setter top style-top "px")
                (defvar style-left (- (* strip-idx strip-width) tex-x))
                (style-setter left style-left "px")
                (defvar style-clip (+ "rect(" img-top ", " (+ tex-x strip-width) ", "
                                      (+ img-top height) ", " tex-x ")"))
                (style-setter clip style-clip)
                (defvar dwx (- x-wall-hit (@ player x)))
                (defvar dwy (- y-wall-hit (@ player y)))
                (defvar wall-dist (+ (* dwx dwx) (* dwy dwy)))
                (defvar style-z-index (ash (* -1 (* wall-dist 1000)) 0))
                (when (not (equal style-z-index (@ old-styles z-index)))
                  (setf (@ strip style z-index) style-z-index
                        (@ old-styles z-index) style-z-index))
                ;; Start some floor casting (maybe)
                ;; Notice a lot of the vars from strip are being redefined here
                ;; Amazing tutorial on floor casting here:
                ;; http://lodev.org/cgtutor/raycasting2.html
                (defvar floor-x-wall x) ;; TODO (cond check)
                (defvar floor-y-wall y) ;; TODO (cond check)
                (defvar floor-dist-player 0)
                (defvar floor-draw-end (+ style-height style-top))
                (defvar floor-current-pos (* (+ (* floor-weight dwy) (- 1 floor-weight)) (@ player y)))
                (defvar floor-height (- screen-height (+ style-top style-height)))
                (loop for fy from 0 below floor-height by strip-height do
                     (return-from cast-single-ray)
                     (progn
                       (defvar floor-current-dist (/ height (- (* 2 fy) height))) ;; Lookup table instead?
                       (defvar floor-weight (/ (- floor-current-dist floor-dist-player)
                                               (- wall-dist floor-dist-player)))

                       (defvar floor-current-x (+ (* floor-weight floor-x-wall)
                                                  (* (- 1.0 - floor-weight) (@ player x))))
                       (defvar floor-current-y (+ (* floor-weight floor-y-wall)
                                                  (* (- 1.0 - floor-weight) (@ player y))))

                       (defvar floor-tex-x (rem (* floor-current-x strip-width) strip-width))
                       (defvar floor-tex-y (rem (* floor-current-y strip-height) strip-height))

                       ;; buffer[x][y] = (texture[3][texWidth * floorTexY + floorTexX] >> 1) & 8355711;
                       (unless (and floor-strips
                                    (aref floor-strips strip-idx)
                                    (aref floor-strips strip-idx fy))
                         (return-from cast-single-ray))
                       (defvar floor-strip (aref floor-strips strip-idx fy))
                       (defvar style (@ floor-strip style))
                       (defvar old-styles (@ floor-strip old-styles))
                       ;;(defvar floor-top (+ (* fy strip-height) floor-tex-y
                       ;;                     (- screen-height (+ style-top img-top))))
                       (defvar floor-top (+ style-top style-height (* fy strip-height)))
                       (defvar floor-left style-left)
                       ;;(defvar floor-width (/ view-dist dist))
                       (defvar floor-height (* (/ strip-height strip-width)
                                               (+ floor-tex-y (/ view-dist dist))))
                       (defvar floor-width (+ floor-tex-x (* floor-height strip-width)))
                       (style-setter top floor-top "px")
                       (style-setter left floor-left "px")
                       (style-setter width floor-width "px")
                       (style-setter height floor-height "px")
                       (style-setter z-index (- style-z-index 10000))
                       (style-setter opacity ".3")
                       ;;(defvar style-clip (+ "rect(" img-top ", " (+ tex-x strip-width) ", "
                       ;;                     (+ img-top height) ", " tex-x ")"))
                       (defvar floor-clip (+ "rect(" img-top ", " (+ tex-x strip-width) ", "
                                             floor-height ", " tex-x ")"))
                       ;; Fun rect(top, right, bottom, left)
                                        ;(defvar floor-clip (+ "rect(" (+ floor-top (* fy strip-height)) ", " (* fy (+ strip-width tex-x)) ", "
                                        ;                      screen-height ", " (* fy tex-x) ")"))
                       (defvar floor-clip (+ "rect(" 0 ", " (+ floor-tex-x (* floor-height strip-width)) ", "
                                             (* (/ strip-height strip-width)
                                                (+ floor-top floor-tex-y strip-height)) ", " 0 ")"))
                                        ;(defvar floor-clip "rect(0,10,20,0)")
                       (style-setter clip floor-clip)
                       ))
                ))

            (defun draw-ray (ray-x ray-y)
              (defvar mini-map-objects ($$ "minimapobjects"))
              (defvar object-ctx (chain mini-map-objects (get-context "2d")))
              (setf (@ object-ctx stroke-style) "rgba(0,100,0,0,.3)"
                    (@ object-ctx line-width) 0.5)
              (chain object-ctx (begin-path))
              (chain object-ctx (move-to (* (@ player x) mini-map-scale)
                                         (* (@ player y) mini-map-scale)))
              (chain object-ctx (line-to (* ray-x mini-map-scale)
                                         (* ray-y mini-map-scale)))
              (chain object-ctx (close-path))
              (chain object-ctx (stroke)))

            (defun move (entity time-delta)
              (defvar mul (/ time-delta game-cycle-delay))
              (defvar move-step (* mul (@ entity speed) (@ entity move-speed)))
              (incf (@ entity rot-deg) (* mul (@ entity dir) (@ entity rot-speed)))
              (setf (@ entity rot-deg) (rem (@ entity rot-deg) 360))
              (when (< (@ entity rot-deg) -180) (incf (@ entity rot-deg) 360))
              (when (>= (@ entity rot-deg) 180) (decf (@ entity rot-deg) 360))
              (defvar snap (rem (+ (@ entity rot-deg) 360) 90))
              (when (or (< snap 2) (> snap 88))
                (setf (@ entity rot-deg) (* (round (/ (@ entity rot-deg) 90)) 90)))
              (setf (@ entity rot) (* (@ entity rot-deg) (/ pi 180)))
              (defvar new-x (+ (@ entity x) (* (cos (@ entity rot)) move-step)))
              (defvar new-y (+ (@ entity y) (* (sin (@ entity rot)) move-step)))
              (if (= (@ entity owner) 0) ;; Only collision detect on client end for players
                  (defvar pos (create x new-x y new-y radius 0.35))
                  (defvar pos (check-collision (@ entity x) (@ entity y) new-x new-y 0.35)))
              (setf (@ entity x) (@ pos x)
                    (@ entity y) (@ pos y)))

            (defun check-collision (from-x from-y to-x to-y radius)
              (defvar pos (create x from-x y from-y))
              (when (or (< to-y 0) (>= to-y map-height) (< to-x 0) (>= to-x map-width))
                (return-from check-collision pos))
              (defvar block-x (floor to-x))
              (defvar block-y (floor to-y))
              (when (is-blocking block-x block-y)
                (return-from check-collision pos))
              (setf (@ pos x) to-x
                    (@ pos y) to-y)
              (defvar block-top (is-blocking block-x (1- block-y)))
              (defvar block-bottom (is-blocking block-x (1+ block-y)))
              (defvar block-left (is-blocking (1- block-x) block-y))
              (defvar block-right (is-blocking (1+ block-x) block-y))
              (when (and (not (equal block-top 0))
                         (< (- to-y block-y) radius))
                (setf to-y (+ block-y radius)
                      (@ pos y) to-y))
              (when (and (not (equal block-bottom 0))
                         (< (- (1+ block-y) to-y) radius))
                (setf to-y (- (1+ block-y) radius)
                      (@ pos y) to-y))
              (when (and (not (equal block-left 0))
                         (< (- to-x block-x) radius))
                (setf to-x (+ block-x radius)
                      (@ pos x) to-x))
              (when (and (not (equal block-right 0))
                         (< (- (1+ block-x) to-x) radius))
                (setf to-x (- (1+ block-x) radius)
                      (@ pos x) to-x))
              ;; is tile to the top left a wall?
              (when (and (not (equal (is-blocking (1- block-x) (1- block-y)) 0))
                         (not (and (not (equal block-top 0)) (not (equal block-left 0)))))
                (defvar dx (- to-x block-x))
                (defvar dy (- to-y block-y))
                (when (< (+ (* dx dx) (* dy dy)) (* radius radius))
                  (if (> (* dx dx) (* dy dy))
                      (setf to-x (+ block-x radius)
                            (@ pos x) to-x)
                      (setf to-y (+ block-y radius)
                            (@ pos y) to-y))))
              ;; is tile to top right a wall?
              (when (and (not (equal (is-blocking (1+ block-x) (1- block-y)) 0))
                         (not (and (not (equal block-top 0)) (not (equal block-right 0)))))
                (defvar dx (- to-x (1+ block-x)))
                (defvar dy (- to-y block-y))
                (when (< (+ (* dx dx) (* dy dy)) (* radius radius))
                  (if (> (* dx dx) (* dy dy))
                      (setf to-x (- (1+ block-x) radius)
                            (@ pos x) to-x)
                      (setf to-y (+ block-y radius)
                            (@ pos y) to-y))))
              ;; is tile to bottom-left a wall? - sure about bottom-left? seems bottom-right?
              (when (and (not (equal (is-blocking (1- block-x) (1+ block-y)) 0))
                         (not (and (not (equal block-bottom 0)) (not (equal block-left 0)))))
                (defvar dx (- to-x block-x))
                (defvar dy (- to-y (1+ block-y)))
                (when (< (+ (* dx dx) (* dy dy)) (* radius radius))
                  (if (> (* dx dx) (* dy dy))
                      (setf to-x (+ block-x radius)
                            (@ pos x) to-x)
                      (setf to-y (- (1+ block-y) radius)
                            (@ pos y) to-y))))
              ;; is tile to bottom right a wall? - again seems like this is bottom-left
              (when (and (not (equal (is-blocking (1+ block-x) (1+ block-y)) 0))
                         (not (and (not (equal block-bottom 0)) (not (equal block-right 0)))))
                (defvar dx (- to-x (1+ block-x)))
                (defvar dy (- to-y (1+ block-y)))
                (when (< (+ (* dx dx) (* dy dy)) (* radius radius))
                  (if (> (* dx dx) (* dy dy))
                      (setf to-x (+ block-x radius)
                            (@ pos x) to-x)
                      (setf to-y (- (1+ block-y) radius)
                            (@ pos y) to-y))))
              pos)

            (defun is-blocking (x y)
              ;; first make sure we cannot move outside level bounds
              (when (or (< y 0) (>= y map-height) (< x 0) (>= x map-width))
                (return-from is-blocking true))
              (defvar ix (floor x))
              (defvar iy (floor y))
              ;; return true if map block is not 0, aka blocking wall
              (when (not (equal (aref *map* iy ix) 0)) (return-from is-blocking true))
              (when (and (aref sprite-map iy ix) (@ (aref sprite-map iy ix) block))
                (return-from is-blocking true))
              false)

            (defun player-in-player-range ()
              "If a player is in range of another, alert combat mode"
              (let ((px (ash (@ player x) 0))
                    (py (ash (@ player y) 0))
                    (show-combat nil))
                (when (> (@ player map_index) 0)
                  (loop for enemy in enemies
                     do (with-slots (x y) enemy
                          (when (in-range px py (ash x 0) (ash y 0) *combat-range*)
                            (setf show-combat t)))))
                (if show-combat
                    (chain ($ "#combat") (show))
                    (chain ($ "#combat") (hide)))))

            (set-interval player-in-player-range 1000)

            (defun update-mini-map ()
              (defvar mini-map ($$ "minimap"))
              (defvar mini-map-objects ($$ "minimapobjects"))
              (defvar object-ctx (chain mini-map-objects (get-context "2d")))
              (setf (@ mini-map-objects width) (@ mini-map-objects width)
                    (@ object-ctx fill-style) "red")
              (setf (@ object-ctx stroke-style) "red")
              (setf (@ ($$ "minimapcontainer") style margin-left)
                    (+ 50 (* -1 (@ player x) mini-map-scale)))
              (setf (@ ($$ "minimapcontainer") style margin-top)
                    (+ 50 (* -1 (@ player y) mini-map-scale)))
              (chain object-ctx (fill-text "@"
                                           (- (* (@ player x) mini-map-scale) 2)
                                           (- (* (@ player y) mini-map-scale) 2)))
              (chain object-ctx (begin-path))
              (chain object-ctx (move-to (* (@ player x) mini-map-scale)
                                         (* (@ player y) mini-map-scale)))
              (chain object-ctx (line-to
                                 (* (+ (@ player x) (* (cos (@ player rot)) 4)) mini-map-scale)
                                 (* (+ (@ player y) (* (sin (@ player rot)) 4)) mini-map-scale)))
              (chain object-ctx (close-path))
              (chain object-ctx (stroke))
              (loop for i from 0 below (@ enemies length) do
                   (progn
                     (defvar enemy (aref enemies i))
                     (when (and (> (@ enemy hp) 0)
                                (> (@ enemy y) 0)
                                (> (@ enemy x) 0))
                       (let* ((wall-type (aref *map*
                                              (ash (@ enemy y) 0)
                                              (ash (@ enemy x) 0))))
                         (when (eq 0 wall-type))
                         (setf (@ object-ctx fill-style) "red")
                         (chain object-ctx (fill-rect
                                            (- (* (@ enemy x) mini-map-scale) 2)
                                            (- (* (@ enemy y) mini-map-scale) 2)
                                            4 4)))))))

            (defun draw-mini-map ()
              "Draw the topdown view minimap"
              (unless *map*
                (set-timeout draw-mini-map 100)
                (return-from draw-mini-map))
              (defvar mini-map ($$ "minimap"))
              (defvar mini-map-ctr ($$ "minimapcontainer"))
              (defvar mini-map-objects ($$ "minimapobjects"))

              (setf (@ mini-map width) (* map-width mini-map-scale))
              (setf (@ mini-map height) (* map-height mini-map-scale))
              (setf (@ mini-map-objects width) (@ mini-map width))
              (setf (@ mini-map-objects height) (@ mini-map height))
              (defvar w (+ (* map-width mini-map-scale) "px"))
              (defvar h (+ (* map-height mini-map-scale) "px"))
              (setf (@ mini-map style width) w)
              (setf (@ mini-map-objects style width) w)
              (setf (@ mini-map-ctr style width) w)
              (setf (@ mini-map style height) h)
              (setf (@ mini-map-objects style height) h)
              (setf (@ mini-map-ctr style height) h)
              (defvar ctx (chain mini-map (get-context "2d")))
              (setf (@ ctx fill-style) "white")
              (chain ctx (fill-rect 0 0 (@ mini-map width) (@ mini-map height)))
              ;; loop over map blocks
              (loop for y from 0 below map-height do
                   (loop for x from 0 below map-width do
                        (progn
                          (when (and *map*
                                     (aref *map* y)
                                     (aref *map* y x))
                            (defvar wall (aref *map* y x))
                            (when (> wall 0) ;; if a wall is here
                              (setf (@ ctx fill-style) "rgb(200,200,200)")
                              (chain ctx (fill-rect
                                          (* x mini-map-scale)
                                          (* y mini-map-scale)
                                          mini-map-scale mini-map-scale))))
                          (when (and (aref sprite-map y x)
                                     (or (eq 1 (@ (aref sprite-map y x) type))
                                         (eq 0 (@ (aref sprite-map y x) type))))
                            (setf (@ ctx fill-style) "rgb(100,200,100)")
                            (chain ctx (fill-rect
                                        (+ (* x mini-map-scale) (* mini-map-scale 0.25))
                                        (+ (* y mini-map-scale) (* mini-map-scale 0.25))
                                        (* mini-map-scale 0.5) (* mini-map-scale 0.5)))))))
              (update-mini-map))

            (defvar sword-state 0)
            (defun swing-sword ()
              "Show the sword swinging"
              (let ((total-states 7)
                    (sword-speed 600))
                (incf sword-state)
                (if (>= sword-state total-states)
                    (progn
                      (setf sword-state 0)
                      (chain ($ "#sword") (css (create "opacity" .3
                                                       "margin-left"
                                                       (+ (* -1 640 sword-state) "px")))))
                    (progn
                      (chain ($ "#sword") (css (create "opacity" 1
                                                       "margin-left"
                                                       (+ (* -1 640 sword-state) "px"))))
                      (set-timeout swing-sword (/ sword-speed total-states))))))

            (defun sword-attack ()
              "Do a sword attack to knock enemies backwards"
              (swing-sword)
              (sock-emit "request-sword-swing" (create unit_id *my-id*)))

            (defvar holder-state 0)
            (defun loading-animation ()
              "Show the loading screen animation"
              (unless (= holder-state -1)
                (let ((total-states 4)
                      (speed 1000))
                  (incf holder-state)
                  (when (>= holder-state total-states)
                    (setf holder-state 0))
                  (chain ($ ".holder img") (css (create "opacity" 1
                                                        "margin-left"
                                                        (+ (* -1 64 holder-state) "px"))))
                  (set-timeout loading-animation (/ speed total-states)))))

            (set-timeout loading-animation 200)

            (defun bind-keys ()
              "Grab the key event code"
              (setf (@ document onkeydown)
                    (lambda (e)
                      (let ((e (or e (@ window event))))
                        (down-keys (@ e key-code)))))
              (setf (@ document onkeyup)
                    (lambda (e)
                      (let ((e (or e (@ window event))))
                        (up-keys (@ e key-code))))))

            (ƒƒ down-keys
                "Swap based on keybind pressed"
                13 → (chat-controller)
                (eq chat-prompt-state t) → (return-from down-keys)
                89 → (chain ($ ".yes") (click)) ;; yes prompt
                78 → (chain ($ ".no") (click))  ;; no prompt
                38 → (setf (@ player speed) 1)  ;; up
                87 → (setf (@ player speed) 1)  ;; up w
                75 → (setf (@ player speed) 1)  ;; up k
                40 → (setf (@ player speed) -1) ;; down
                83 → (setf (@ player speed) -1) ;; down s
                74 → (setf (@ player speed) -1) ;; down j
                37 → (setf (@ player dir) -1)   ;; left
                65 → (setf (@ player dir) -1)   ;; left a
                72 → (setf (@ player dir) -1)   ;; left h
                39 → (setf (@ player dir) 1)    ;; right
                68 → (setf (@ player dir) 1)    ;; right d
                76 → (setf (@ player dir) 1)    ;; right l
                32 → (sword-attack))            ;; spacebar

            (ƒƒ up-keys
                "Swap based on key raised"
                (eq chat-prompt-state t) → (return-from up-keys)
                38 → (setf (@ player speed) 0)
                87 → (setf (@ player speed) 0)
                75 → (setf (@ player speed) 0)
                40 → (setf (@ player speed) 0)
                83 → (setf (@ player speed) 0)
                74 → (setf (@ player speed) 0)
                37 → (setf (@ player dir) 0)
                65 → (setf (@ player dir) 0)
                72 → (setf (@ player dir) 0)
                39 → (setf (@ player dir) 0)
                68 → (setf (@ player dir) 0)
                76 → (setf (@ player dir) 0))

            (setf (@ window onload) (set-timeout (λλ α → (sock-emit "request-signin" {})) 10))))))
        (:body
         (:div :id "loading" "LOADING..."
               (:br)
               (:div :class "holder" (:img :src "units/witch-front.png"))
               (:div :class "holder" (:img :src "units/mermaid-front.png"))
               (:div :class "holder" (:img :src "units/blonde-front.png"))
               (:div :class "holder" (:img :src "units/skeleton-front.png"))
               (:div :class "holder" (:img :src "units/slime-front.png"))
               (:div :class "holder" (:img :src "units/robot-front.png"))
               (:br)
               "If this takes a long time (more than 10 seconds),
the server may be down :("
               (:br)(:br)
               "There is currently an issue with loading new maps, try at least one F5 if
initially loading the page to fix the situation (temporarily)"
               (:br)(:br)
               "While you wait, check out the source at "
               (:a :href "https://github.com/ahungry/pseudo" "https://github.com/ahungry/pseudo!"))
         (:div :id "battle"
               (:img :src "/img/busts/witch-bust.png")
               (:canvas :id "battle-canvas"))
         (:div :id "attack-animation" (:img :id "sword" :src "units/sword-swing.png"))
         (:div :id "screen"
               (:div :id "shade4" :class "lighting")
               (:div :id "shade3" :class "lighting")
               (:div :id "shade2" :class "lighting")
               (:div :id "shade1" :class "lighting")
               (:div :id "floor-shade")
               (:div :id "floor")
               (:div :id "ceiling-shade")
               (:div :id "ceiling"))
         (:div :id "right-pane"
               (:div :id "fog"
                     (:div :id "minimapcontainer"
                           (:canvas :id "minimap")
                           (:canvas :id "minimapobjects")))
               (:div :id "info"
                     (:div :id "stats"
                           (:span :id "p-name" "Your Name")
                           " the level "
                           (:span :id "p-level" "1")
                           " "
                           (:span :id "p-job" "Peasant")
                           (:div :id "portrait"
                                 (:img :src "/units/witch-front.png"))
                           (dotimes (x 2) (htm (:br)))
                           (:div "hp" (:span :id "p-hp" "1"))
                           (:div "mp" (:span :id "p-mp" "1"))
                           (dotimes (x 2) (htm (:br)))
                           (:div "atk" (:span :id "p-atk" "1"))
                           (:div "def" (:span :id "p-def" "1"))
                           (:div "mag" (:span :id "p-mag" "1"))
                           (:div "mdef" (:span :id "p-mdef" "1"))
                           (dotimes (x 2) (htm (:br)))
                           (:div "xp" (:span :id "p-xp" "1"))
                           (:div "jp" (:span :id "p-jp" "1"))
                           (:div "map" (:span :id "p-map" 0))
                           (:br))))
         (:br :style "clear : both;height : 1px;")
         (:div :id "dialog" "HERE")
         (:div :id "chat-prompt" "Type text here"
               (:input :type "text" :id "chatter"))
         (:pre :id "chat" "Press ENTER to Chat, use /nick YOURNAME to set your name<br>")
         (:div :id "combat" "FIGHTING!! [press SPACE to attack nearest enemy]")
         (:div :id "items" "Your items<br>"
               (:div :id "item-helper" "Click an item to equip or unequip it<br>
Find items by exploring the map and/or defeating enemies and players!<br>
Equipped items will show an (e) next to the item name and increase your stats<br>")
               (:div :id "item-list"))
         (:div :id "debug"))))))

;; Clean up problemating terpris from input
(defun cleaner (string)
  (string-trim '(#\newline #\return) string))

(defun default-tcp-handler (stream) ; null
  (declare (type stream stream))
  (unwind-protect
       (let* ((action (format nil "pseudo:~a" (cleaner (read-line stream))))
              (reply (funcall (read-from-string action) (cleaner (read-line stream)))))
         (format nil "sending out: ~a~%" reply)
         (format stream "~a~%" reply))))

;; Start the server
(usocket:socket-server "localhost" 7159 'default-tcp-handler ()
                       :in-new-thread t
                       :reuse-address t
                       :multi-threading t)

(defun print-game-time ()
  (multiple-value-bind
        (seconds minutes hours)
      (get-decoded-time)
    (format nil "~%[~a:~a:~a]~%" hours minutes seconds)))

(defun cyclic-events ()
  "Put all the things we will run on a timer / game interval in here
and drop in it's own thread"
  (sleep *cycle-interval*)
  (maphash (lambda (k v)
             (declare (ignore v))
             (setf (gethash k *event-logs*)
                                        ;(list)))
                   (list (print-game-time))))
           *event-logs*)
  (unit-damage-cycle) ;; have units apply damage to each other if in range
  (unit-regen)
  (unit-defeated-check)
  (unit-level-ups)
  (item-spawner)
  (ai-controller) ;; have npc move closer to player if possible
  (cyclic-events))

;; Start it up stuff
(defun main ()
  "Fill up our game with db data and set appropriate start up values.
Don't forget to generate an initial map or a few to avoid slow down
on subsequent zoning."
  (get-db-jobs)
  (get-db-cards)
  (item-set-types)
  (dotimes (i 2)
    (map-lookup i)))

(defun reset-game ()
  (setf *decks* nil *cards* nil *accounts* nil *units* nil *hands* nil)
  (main))

;; Call the functions to start the game
(main)
(defjs:main)
(make-thread #'cyclic-events :name "cyclic-events")

(in-package #:cl-user)

(defparameter *toot* (make-instance 'hunchentoot:easy-acceptor
                                    :port 4242
                                    :message-log-destination nil
                                    :access-log-destination nil))
(setf (hunchentoot:acceptor-document-root *toot*) #P"~/src/lisp/pseudo/www/")
(hunchentoot:start *toot*)

(hunchentoot:define-easy-handler (pseudo-home :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (pseudo:web-home))

(hunchentoot:define-easy-handler (pseudo :uri "/pseudo.html") ()
  (setf (hunchentoot:content-type*) "text/html")
  (pseudo:web-assets))
