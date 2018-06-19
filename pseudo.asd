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

;;;; pseudo.asd

(asdf:defsystem #:pseudo
  :serial t
  :description "A pseudo 3d multiplayer roguelike"
  :author "Matthew Carter <m@ahungry.com>"
  :license "AGPLv3"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:usocket
               #:usocket-server
               #:jsown
               #:clsql
               #:cl-ppcre
               #:split-sequence
               #:trivial-shell
               #:hunchentoot
               #:cl-fad
               #:cl-who
               #:parenscript
               #:cl-json
               #:clws
               #:glyphs
               #:defjs)
  :components ((:file "package")
               (:file "config")
               (:file "centos")
               (:file "db-connection")
               (:file "macros")
               (:file "socket")
               (:file "map")
               (:file "units")
               (:file "items")
               (:file "chat")
               (:file "pseudo")))
