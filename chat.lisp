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

;;;; chat.lisp
(in-package #:pseudo)

(defun chat-js ()
  (ps

    (defvar chat-prompt-state nil)

    (defun chat-controller (&optional event data)
      (funcall (λλ
                "received" → (chat-add-to-log (@ data chat))
                α → (chat-toggler chat-prompt-state)) event))

    (ƒƒ chat-add-to-log
        α → (chain ($ "#chat") (append (+ α "<br>"))))

    ;; Keep the chat box panned down to the bottom
    (set-interval chat-scroller 500)

    ;; TODO - Only scroll if the chat isn't being hovered
    (ƒƒ chat-scroller
        α → (setf (chain ($$ "chat") scroll-top)
                  (chain ($$ "chat") scroll-height)))

    (ƒƒ chat-val
        α → (chain ($ "#chat-prompt input") (val)))

    (ƒƒ chat-toggler
        nil → (progn
                (setf chat-prompt-state t)
                (chain ($ "#chat-prompt") (show))
                (chain ($ "#chat-prompt input") (val ""))
                (chain ($ "#chat-prompt input") (focus)))
        t   → (progn
                (setf chat-prompt-state nil)
                (sock-emit "request-chat"
                           (create chat (chat-val) unit_id *my-id*))
                (chain ($ "#chat-prompt") (blur))
                (chain ($ "#chat-prompt") (hide))))

    nil))
