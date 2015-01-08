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

;;;; package.lisp

(defpackage #:pseudo
  (:use #:cl #:glyphs #:parenscript #:hunchentoot #:cl-fad #:cl-who #:clws
	#:alexandria #:cl-json #:jsown #:cl-ppcre #:bordeaux-threads)
  (:shadowing-import-from #:alexandria #:copy-stream #:copy-file)
  (:shadowing-import-from #:parenscript #:prototype #:switch)
  (:import-from #:defjs #:defjs #:dojs #:loader)
  (:export #:web-assets #:join-game #:set-action #:request-cards #:request-signin
	   #:request-map #:request-move #:request-event-log #:request-stairs
	   #:request-change-job #:request-hand #:request-chat #:request-chat
	   #:request-give-card #:request-jobs #:request-deck #:zone-change
	   #:request-resync #:request-reset #:request-zone-change
           #:request-all-items #:request-item-resync #:request-item-pickup
           #:request-item-equip #:request-item-unequip #:request-item-owned
           #:request-sword-swing #:web-home
	   #:request-all-units))
