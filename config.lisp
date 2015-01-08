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

;;;; settings.lisp

(in-package #:pseudo)

;; Set some game parameters
(defparameter *websocket-port* 7155)
(defparameter *node-socket-url* "http://localhost:7158")
(defparameter *websocket-url* "ws://localhost:7155")
(defparameter *defjs-url* "http://localhost:39998/defjs.js")
(defparameter *cycle-interval* 3)
(defparameter *regen-interval* 6)
(defparameter *ai-interval* 10)
(defparameter *regen-hp* 1)
(defparameter *regen-mp* 1)
(defparameter *max-hand-size* 4)
(defparameter *draw-card-interval* 4)
(defparameter *starter-deck-size* 5)
(defparameter *db-snapshot-interval* 30)
(defparameter *max-move* 6)
(defparameter *combat-range* 2)

;; You likely don't want to modify these...
(defparameter *last-regen-time* 0)
(defparameter *last-ai-time* 0)
(defparameter *last-db-snapshot-time* 0)

(load "~/.pseudorc" :if-does-not-exist nil) ;; load user rc file
