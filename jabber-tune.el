;;; jabber-tune.el --- XEP-0118 implementation

;; Copyright (C) 2009  mad

;; Author: mad  <owner.mad.epa@gmail.com>
;; Keywords: jabber

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Installing:

;; if you use emms then add to your configure file this line:

;; (add-hook 'emms-player-started-hook
;;           '(lambda ()
;;              (let* ((emms-current-track (emms-playlist-current-selected-track)))
;;                (run-with-timer 10 nil 'jabber-event-tune-send
;;                                juick-bot-jid
;;                                (emms-track-get emms-current-track 'info-artist)
;;                                (number-to-string (or (emms-track-get emms-current-track 'info-playing-time) 0))
;;                                "0" ;;; use rating ?
;;                                (emms-track-get emms-current-track 'info-album)
;;                                (emms-track-get emms-current-track 'info-title)
;;                                (emms-track-get emms-current-track 'info-tracknumber)
;;                                ""))))

;; or other player use for example this:

;; #! /bin/bash
;;
;; ARTIST="AC/DC"
;; LENGTH="251"
;; RATING="8"
;; SOURCE="TNT"
;; TITLE="Rock'N'Roll Singer"
;; TRACK="2"
;; URI=""
;;
;; emacsclient --eval "(jabber-pep-tune-send \"$ARTIST\" \
;;                                           \"$LENGTH\" \
;;                                           \"$RATING\" \
;;                                           \"$SOURCE\" \
;;                                           \"$TITLE\"  \
;;                                           \"$TRACK\"  \
;;                                           \"$URI\")"

;; info of artist getting through dbus or other tech.

;;; Note:

;; If your server do not support PEP, use `jabber-event-tune-send' instead
;; `jabber-pep-tune-send'

;;; Code:

(defun jabber-pep-tune-send (artist length rating source title track uri)
  (unless (memq jabber-buffer-connection jabber-connections)
    (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
      (if new-jc
	  (setq jabber-buffer-connection new-jc)
	(setq jabber-buffer-connection (jabber-read-account)))))
  (let* ((id (apply 'format "emacs-msg-%d.%d.%d" (current-time)))
	 (stanza-to-send `(iq
			   ((from . ,(jabber-connection-bare-jid jabber-buffer-connection))
			    (id . ,id)
			    (type . "set"))
			   (pubsub ((xmlns . "http://jabber.org/protocol/pubsub"))
				   (publish ((node . "http://jabber.org/protocol/tune"))
					    (item nil
						  ((tune ((xmlns . "http://jabber.org/protocol/tune"))
							 ,(when (and artist title)
							    `((artist nil ,artist)
							      (length nil ,length)
							      (rating nil ,rating)
							      (source nil ,source)
							      (title nil ,title)
							      (track nil ,track)
							      (uri nil ,uri)))
							 ))))))))
    (jabber-send-sexp jabber-buffer-connection stanza-to-send)))

(defun jabber-event-tune-send (to artist length rating source title track uri)
  (when (and artist title)
    (unless (memq jabber-buffer-connection jabber-connections)
      (let ((new-jc (jabber-find-active-connection jabber-buffer-connection)))
        (if new-jc
            (setq jabber-buffer-connection new-jc)
          (setq jabber-buffer-connection (jabber-read-account)))))
    (let* ((gid (format "%08x-%04x-%04x-%04x-%012x"
                        (random #xfffffff7)
                        (random #xfff7) (random #xfff7) (random #xfff7)
                        (random #xfffffff7)))
           (stanza-to-send `(message
                             ((from . ,(jabber-connection-bare-jid jabber-buffer-connection))
                              (to . ,to))
                             (event ((xmlns . "http://jabber.org/protocol/pubsub#event"))
                                    (items ((node . "http://jabber.org/protocol/tune"))
                                           (item ((id . ,gid))
                                                 ((tune ((xmlns . "http://jabber.org/protocol/tune"))
                                                        (artist nil ,artist)
                                                        (length nil ,length)
                                                        (rating nil ,rating)
                                                        (source nil ,source)
                                                        (title nil ,title)
                                                        (track nil ,track)
                                                        (uri nil ,uri)))))))))
      (jabber-send-sexp jabber-buffer-connection stanza-to-send))))

(provide 'jabber-tune)
;;; jabber-tune.el ends here
