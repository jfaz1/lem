(defpackage :lem-mlem-mode
  (:use :cl :lem)
  (:export :mlem-mode
           :mlem-delete-forward
           :mlem-delete-backward))

(in-package :lem-mlem-mode)

(define-minor-mode mlem-mode
    (:name "mlem"
     :description "Hungry deletion: deleting whitespace will delete all whitespace until the next non-whitespace character."
     :keymap *mlem-mode-keymap*
     :global t))

(defvar *mlem-mode-keymap* (make-keymap)
  "Keymap for mlem-minor-mode.")

(defvar *mlem-chars-to-skip* '(#\Space #\Tab #\Newline #\Return #\Page #\Vt)
  "Characters to be considered as whitespace for mlem-mode.")

(defun mlem-skip-ws-forward (point)
  "Skip over any whitespace following point."
  (skip-chars-forward point *mlem-chars-to-skip*))

(defun mlem-skip-ws-backward (point)
  "Skip over any whitespace preceding point."
  (skip-chars-backward point *mlem-chars-to-skip*))
 
(define-command mlem-delete-forward () ()
  (with-point ((origin (current-point) :right-inserting))
    (if (member (character-at origin) *mlem-chars-to-skip*)
        (progn
          (mlem-skip-ws-forward origin)
          (delete-between-points (current-point) origin))
        (delete-next-char 1))))

(define-command mlem-delete-backward () ()
  (with-point ((origin (current-point) :left-inserting))
    (if (member (character-at origin -1) *mlem-chars-to-skip*)
        (progn
          (mlem-skip-ws-backward origin)
          (delete-between-points origin (current-point)))
        (delete-previous-char 1))))

(define-key *mlem-mode-keymap* 'delete-next-char 'mlem-delete-forward)
(define-key *mlem-mode-keymap* 'delete-previous-char 'mlem-delete-backward)