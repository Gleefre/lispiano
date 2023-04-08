(defpackage #:lispiano
  (:use #:cl #:sketch #:sketch-fit)
  (:export #:start))

(in-package #:lispiano)

(defparameter *unit* 70)
(defparameter *keys* "q2w3er5t6y7ui9o0p[=]azsxcfvgbnjmk,l./'")
(defparameter *char-keycode*
  '((#\q . 20) (#\2 . 31) (#\w . 26) (#\3 . 32) (#\e . 8) (#\r . 21) (#\5 . 34)
    (#\t . 23) (#\6 . 35) (#\y . 28) (#\7 . 36) (#\u . 24) (#\i . 12) (#\9 . 38)
    (#\o . 18) (#\0 . 39) (#\p . 19) (#\[ . 47) (#\= . 46) (#\] . 48) (#\a . 4)
    (#\z . 29) (#\s . 22) (#\x . 27) (#\c . 6) (#\f . 9) (#\v . 25) (#\g . 10)
    (#\b . 5) (#\n . 17) (#\j . 13) (#\m . 16) (#\k . 14) (#\, . 54) (#\l . 15)
    (#\. . 55) (#\/ . 56) (#\' . 52)))

(defparameter *note-shift* -24)
(defparameter *notes-folder*
  (asdf:system-relative-pathname :lispiano "./piano_c4/"))

(defun note-filename (note-index)
  (format nil "~a~a.wav" *notes-folder* (+ *note-shift* note-index)))

(defun make-note (note-index)
  (let* ((filename (note-filename note-index)))
    (cons note-index (sdl2-mixer:load-wav filename))))

(defun play-note (note)
  (sdl2-mixer:play-channel (car note) (cdr note) 0))

(defun stop-note (note)
  (sdl2-mixer:halt-channel (car note)))

(defun make-notes ()
  (loop for (char . keycode) in *char-keycode*
        for i from 0
        collect (cons keycode (make-note i))))

(defun close-notes (notes)
  (loop for (keycode . (i . chunk)) in notes
        do (sdl2-mixer:free-chunk chunk)))

;;; App

(defsketch key-piano ((width (* 12 *unit*))
                      (height (* 6 *unit*))
                      (pressed (make-hash-table :size 50))
                      (pressed-notes ())
                      (notes (make-notes)))
  (fit (* 12 *unit*) (* 6 *unit*) width height)
  (loop for char across *keys*
        for i from 1
        for black = (member (mod i 12)
                            '(2 4 7 9 11))
        for x = 0 then (if (equal char #\a) (- (/ *unit* 4))
                           (if black x
                               (+ *unit* x)))
        for y = 0 then (if (equal char #\a) (* 2 *unit*) y)
        do (let ((color (if black +black+ +white+))
                 (x (if black (+ x (/ *unit* 2)) x))
                 (y (if black y (+ y *unit*))))
             (when (gethash (cdr (assoc char *char-keycode*)) pressed)
               (setf color (gray (if black 0.2 0.8))))
             (with-pen (make-pen :fill color)
               (rect (+ 2 x) (+ 2 y) (- *unit* 4) (- *unit* 4)))
             (with-font (make-font :color (if (= i 13) +blue+ +red+) :size (/ *unit* 2) :align :left)
               (text (format nil "~a" char) (+ (/ *unit* 4) x) (+ (/ *unit* 4) y)))))
  (with-pen (make-pen :fill (gray 0.9))
    (rect (* *unit* 3) (+ 2 (* *unit* 4))
          (* *unit* 6) (- (* 2 *unit*) 4)))
  (with-font (make-font :color (gray 0.1) :size (/ *unit* 4) :align :left)
    (text (format nil "~a" pressed-notes)
          (+ 5 (* *unit* 3))
          (+ 5 (* *unit* 4)))))

(defmethod kit.sdl2:keyboard-event ((app key-piano) state ts repeat? keysym)
  (unless (eq :scancode-escape (sdl2:scancode keysym))
    (unless repeat?
      (let ((note (cdr (assoc (sdl2:scancode-value keysym) (slot-value app 'notes)))))
	(when note
	  (if (eq state :keydown)
	      (play-note note)
              (stop-note note)))))
    (setf (gethash (sdl2:scancode-value keysym) (slot-value app 'pressed))
	  (eq state :keydown))
    (let ((note (sdl2:scancode keysym)))
      (if (eq state :keydown)
	  (pushnew note (slot-value app 'pressed-notes))
          (setf (slot-value app 'pressed-notes)
		(remove note (slot-value app 'pressed-notes) :test #'equal))))
    (kit.sdl2:render app)))

(define-start-function (start) key-piano (:resizable t)
  (:on-close (app)
    (close-notes (key-piano-notes app)))
  (:start
    (sdl2-mixer:init :wave)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:allocate-channels 100))
  (:quit
    (sdl2-mixer:halt-channel -1)
    (sdl2-mixer:close-audio)
    (sdl2-mixer:quit)
    (print 'bye!)))
