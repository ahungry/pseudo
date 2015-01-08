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

;;;; socket.lisp
(in-package #:pseudo)

(defparameter *clients* (make-hash-table :test 'equal))

(make-thread (λ α → (run-server *websocket-port*))
             :name "Websocket Server")

(defclass ws-controller (ws-resource)
  ())

(defmethod resource-client-connected ((res ws-controller) client)
  "Save our client in connection pool"
  (setf (gethash (client-host client) *clients*) client)
  (format t "Got connection from ~s: ~s~%"
          (client-host client)
          (client-port client))
  (write-to-client-text client "{\"event\":\"Welcome\"}")
  t)

(defmethod resource-client-disconnected ((resource ws-controller) client)
  "TODO - Disable their in game character at this point"
  (format t "Client disconnected from resource ~s: ~s~%" resource client)
  t)

(defun ws-clients ()
  "Get the list of the clients"
  (loop for v being the hash-values in *clients*
     collect v))

(defmethod resource-received-text ((res ws-controller) client message)
  "This should more or less be a drop in replacement for the existing
way things are piped into the general socket listener and spit back
out at the client."
  (let* ((json (cl-json:decode-json-from-string message))
         (event (format nil "pseudo::~a" (cdr (assoc :event json))))
         (data-in (cdr (assoc :data json))))
    (funcall (read-from-string event) data-in client)))

(defmethod resource-received-binary ((res ws-controller) client message)
  (format t "Client ~s sent binary frame ~s~%" client message)
  (write-to-client-binary client message))

(register-global-resource "/controller/"
                          (make-instance 'ws-controller)
                          (origin-prefix
                           "http://127.0.0.1"
                           "http://pseudo.ahungry.com"
                           "http://ahungry.com"
                           "http://pro"
                           "http://localhost"))

(make-thread (λ α → (run-resource-listener
                     (find-global-resource "/controller/")))
             :name "Resource listener for /controller/")

(ƒ emit-broadcast-data-formatter
   (and (listp α) (eq :obj (car α))) → (cdr α)
   (and (listp α)
        (listp (car α))
        (eq :obj (car (car α)))) → (mapcar #'cdr α)
   (stringp α) → (decode-json-from-string α)
   α → α)

(defun broadcast (event data)
  "Receive a list of data items and send to all connected clients"
  (let ((json-string (encode-json-to-string
                      (list (cons :event event)
                            (cons :data (emit-broadcast-data-formatter data))))))
  (write-to-clients-text (ws-clients) json-string)))

(defun emit (event data client)
  "Send data out to just one individual client"
  (let ((json-string (encode-json-to-string
                      (list (cons :event event)
                            (cons :data (emit-broadcast-data-formatter data))))))
    (when client
      (write-to-client-text client json-string))
    json-string))

(defun unit-client (unit-id)
  "Given a unit-id, figure out which client corresponds"
  (let ((account (get-from *accounts* 'uid unit-id)))
    (when account
      (gethash (account-ip account)
               *clients*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro+ps c (&rest data)
    `(chain console (log ,@data))))

(defun socket-js ()
  (ps
    ;;(defvar *socket-url* (lisp *node-socket-url*))
    (defvar *websocket-url* (+ (lisp *websocket-url*) "/controller/"))
    (defvar socket)
    (defvar socket-clws)
    (defvar *sock-responses* [])

    (defun sock-on (event fn)
      "Bind a function to the sock-responses"
      (setf (aref *sock-responses* event) fn))

    (defun sock-emit (event data)
      "Safely send data into the websocket, relink if fail"
      (if (and socket-clws
               (eq (@ socket-clws ready-state) 1))
          (progn
            (let ((json (chain -j-s-o-n (stringify
                                         (create "event" event
                                                 "data" data)))))
              (c event json)
              (chain socket-clws (send json))))
          (progn
            (socket-start)
            (set-timeout (λλ α → (sock-emit event data)) 1000))))

    (defun socket-start ()
      ;;(setf socket (chain io (connect *socket-url*)))
      (setf socket-clws (new (-Web-Socket *websocket-url*)))
      ;; Response mappings on open
      (setf (@ socket-clws onopen)
            (lambda (event)
              (setf (@ socket-clws onmessage)
                    (λλ α → (progn
                              (let* ((data (chain -j-s-o-n (parse (@ α data))))
                                     (event (aref *sock-responses* (@ data event))))
                                (when event (chain event (call nil (@ data data))))))))))

      ;; Essentially a Firefox fix for not gracefully closing sockets
      (setf (@ window onbeforeunload)
            (λλ α → (progn (setf (@ socket-clws onclose)
                                 (lambda () nil))
                           (chain socket-clws (close))))))

    (sock-on "start-game"
             (λλ α → (progn
                       (setf *my-id* (@ α id))
                       (init)
                       (sock-emit "request-map" (create unit_id *my-id*))
                       (sock-emit "request-item-resync" (create unit_id *my-id*))
                       (sock-emit "request-item-owned" (create unit_id *my-id*))
                       (sock-emit "join-game" (create unit_id *my-id*)))))

    (sock-on "resync"
             (λλ α → (unit-controller "resync" α)))

    (sock-on "request-chat"
             (λλ α → (chat-controller "received" α)))

    (sock-on "map-init"
             (λλ α → (progn (map-controller "init" α))))

    (sock-on "map-reload"
             (λλ α → (progn (map-controller "reload" α))))

    (sock-on "stair-init"
             (λλ α → (progn (map-controller "stairs" α))))

    (sock-on "update-unit"
             (λλ α → (unit-controller "update-unit" α)))

    (sock-on "force-reload"
             (λλ α → (chain window location (reload))))

    (sock-on "event-log"
             (λλ α → (chain ($ "#chat") (append α))))

    (sock-on "item-resync"
             (λλ α → (item-controller "resync" α)))

    (sock-on "item-owned"
             (λλ α → (item-controller "owned" α)))

    (sock-on "item-update"
             (λλ α → (item-controller "update" α)))

    (sock-on "eval"
             (λλ α → (progn
                       (eval (+ (@ α code))))))

    nil))
