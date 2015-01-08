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

;; MACRO Definitions
(in-package #:pseudo)

(defmacro class-to-json (name slots &body body)
  "Send a class data set to json"
  `(with-slots ,slots ,name
     (new-js
       ,@body)))

(defmacro thing-to-json (fn-name name &rest varlist)
  "Create a function such as card-to-json to pull out
   a card from a place object passed in and serialize as json"
  `(defun ,fn-name (,name)
     (class-to-json ,name (,@varlist)
       ,@(nreverse (loop for var in varlist
	      collect `((string-downcase
			 (string ',var)) ,var))))))

(defmacro things-to-json (fn-name helper name place)
  `(defun ,fn-name ()
     (loop for ,name in ,place collect
	  (,helper ,name))))

(defmacro mad-adder (fn-name name place &rest varlist)
  "Create an (add-classname function for fast adding of new
   class instances to the main place object"
  `(defun ,fn-name (obj)
     (let ((,name (make-instance ',name)))
       (with-slots (,@varlist) ,name
	 ,@(loop for var in varlist
	       collect `(setf ,var (if obj (pop obj) 0)))
	 (push ,name ,place)))))

;; Lifted from http://stackoverflow.com/questions/3868658/defmacro-with-defclass
(defun build-var (classname var)
  "Helper function for defobject"
  (list var
	:initform nil
	:accessor (intern (concatenate 'string (string classname) "-" (string var)))
	:initarg (intern (string var) :keyword)))

(defun build-varlist (classname varlist)
  "Helper function for defobject"
  (loop for var in varlist
       collect (build-var classname var)))

(defmacro defobject (name &rest varlist)
  "Define a class with a set of behaviors,
   Variables are accessed by name-varname.
   (defobject classname v1 v2 v3)"
  `(defclass ,name ()
     ,(build-varlist name varlist)))

(defmacro json-response (emit-action emit-data
			 broadcast-action broadcast-data)
  "Standard output format for the game's json responses,
   it sends out as emit and broadcast for local and global
   scope on the user's receiving end"
  `(to-json
    (new-js
      ("emit"
       (new-js ("event" ,emit-action)
	       ("data" ,emit-data)))
      ("broadcast"
       (new-js ("event" ,broadcast-action)
	       ("data" ,broadcast-data))))))

(defun no-json-response ()
  (json-response "nil" "nil" "nil" "nil"))

(defmacro game-dataset (name &rest fields)
  `(progn
     (defparameter
	 ,(intern (concatenate 'string "*" (string name) "S*"))
       nil)
     (defobject ,name ,@fields)
     (mad-adder
      ,(intern (concatenate 'string "ADD-" (string name)))
      ,name
      ,(intern (concatenate 'string "*" (string name) "S*"))
      ,@fields)
     (thing-to-json
      ,(intern (concatenate 'string (string name) "-TO-JSON"))
      ,name
      ,@fields)
     (things-to-json
      ,(intern (concatenate 'string (string name) "S-TO-JSON"))
      ,(intern (concatenate 'string (string name) "-TO-JSON"))
      ,name
      ,(intern (concatenate 'string "*" (string name) "S*")))))

(defmacro pretty-json (json-object)
  `(cl-ppcre:regex-replace-all ",|}" (string ,json-object)
		      (string #\Newline)))
