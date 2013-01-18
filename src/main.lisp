;;;
;;; This file is part of common-sense project
;;;
;;; This project is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option any
;;; later version.
;;;
;;; This project is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; this project. If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Copyright 2013 Gordon Quad
;;;

(in-package :common-sense)

(qt:enable-syntax)

(defvar *qapp*)

(defclass browser-main-window ()
  ((webview :initform nil
            :accessor webview))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance browser-main-window)
                                       &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((wv (#_new QWebView instance))
        (layout (#_new QVBoxLayout))
        (url (#_new QUrl "http://google.com")))
    (setf (webview instance) wv)
    (#_addWidget layout (webview instance))
    (#_setLayout instance layout)
    (#_load (webview instance) url)))

(defun main ()
  (make-thread (lambda ()
                   (setf *qapp* (make-qapplication))
                   (#_connect "QObject"
                              *qapp*
                              (QSIGNAL "lastWindowClosed()")
                              *qapp*
                              (QSLOT "quit()"))
                   (let ((window (make-instance 'browser-main-window)))
                     (#_show window)
                     (unwind-protect
                          (#_exec *qapp*)
                       (#_hide window))))))

