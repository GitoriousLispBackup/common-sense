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

(enable-syntax)

(defclass url ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QUrl"))

(defmethod initialize-instance :after ((instance url)
                                       &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(defgeneric supported-content-types (ww)
  (:documentation "")
  (:method ((ww web-widget))
    (#_supportedContentTypes (page ww))))

(defgeneric error-string (url)
  (:documentation "")
  (:method ((url url))
    (#_errorString url)))

errorString
fromUserInput
fragment/setFragment
hasFragment
hasQuery
hasQueryItem
host/setHost
isEmpty
isLocalFile
isParentOf
isValid
isRelative
password/setPassword
path/setPath
port/setPort
scheme/setScheme
queryItemValue
removeAllQueryItems
resolved
authory/setAuthory
userinfo/setUserinfo
userName/setUserName
url/setUrl