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
(in-readtable :qt)

(defvar *network-access-manager-class* 'network-access-manager)

(defclass network-access-manager ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QNetworkAccessManager"))

(defmethod initialize-instance :after ((instance network-access-manager)
                                       &key parent)
  (if parent
      (new instance parent)
      (new instance)))
