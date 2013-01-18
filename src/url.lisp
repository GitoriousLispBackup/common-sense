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
                                       &key value user-input)
  (if value
      (if user-input
          (new instance value)
          (new instance value))
      (new instance)))

(defgeneric error-string (url)
  (:documentation "")
  (:method ((url url))
    (#_errorString url)))

(defgeneric empty-p (url)
  (:documentation "")
  (:method ((url url))
    (#_isEmpty url)))

(defgeneric local-file-p (url)
  (:documentation "")
  (:method ((url url))
    (#_isLocalFile url)))

(defgeneric parent-p (url child-url)
  (:documentation "")
  (:method ((url url) child-url)
    (#_isParentOf url child-url)))

(defgeneric valid-p (url)
  (:documentation "")
  (:method ((url url))
    (#_isValid url)))

(defgeneric relative-p (url)
  (:documentation "")
  (:method ((url url))
    (#_isRelative url)))

(defgeneric has-fragment-p (url)
  (:documentation "")
  (:method ((url url))
    (#_hasFragment url)))

(defgeneric has-query-p (url)
  (:documentation "")
  (:method ((url url))
    (#_hasQuery url)))

(defgeneric has-query-item-p (url item)
  (:documentation "")
  (:method ((url url) item)
    (#_hasQueryItem url item)))

(defgeneric resolved (url)
  (:documentation "")
  (:method ((url url))
    (make-instance 'url :pointer (qt::qobject-pointer (#_resolved url)))))

;; queryItems waiting for QPair marshalling
;; Encoded

(defgeneric remove-all-query-items (url item &key encoded)
  (:documentation "")
  (:method ((url url) item &key encoded)
    (if encoded
        (#_removeEncodedAllQueryItems url item)
        (#_removeAllQueryItems url item))))

(defgeneric query-item-value (url item &key encoded)
  (:documentation "")
  (:method ((url url) item &key encoded)
    (if encoded
        (#_data (#_encodedQueryItemValue url item))
        (#_queryItemValue url item))))

(defgeneric encoded-query (url)
  (:documentation "")
  (:method ((url url))
    (#_data (#_encodedQuery url))))

(defgeneric host (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedHost url))
        (#_host url))))

(defgeneric (setf host) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedHost url value)
        (#_setHost url value))))

(defgeneric fragment (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedFragment url))
        (#_fragment url))))

(defgeneric (setf fragment) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedFragment url value)
        (#_setFragment url value))))

(defgeneric password (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedPassword url))
        (#_password url))))

(defgeneric (setf password) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedPassword url value)
        (#_setPassword url value))))

(defgeneric path (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedPath url))
        (#_path url))))

(defgeneric (setf path) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedPath url value)
        (#_setPath url value))))

(defgeneric port (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedPort url))
        (#_port url))))

(defgeneric (setf port) (value url)
  (:documentation "")
  (:method (value (url url))
    (#_setPort url value)))

(defgeneric scheme (url)
  (:documentation "")
  (:method ((url url))
    (#_scheme url)))

(defgeneric (setf scheme) (value url)
  (:documentation "")
  (:method (value (url url))
    (#_setScheme url value)))

(defgeneric authority (url)
  (:documentation "")
  (:method ((url url))
    (#_authority url)))

(defgeneric (setf authority) (value url)
  (:documentation "")
  (:method (value (url url))
    (#_setAuthority url value)))

(defgeneric user-info (url)
  (:documentation "")
  (:method ((url url))
    (#_userInfo url)))

(defgeneric (setf user-info) (value url)
  (:documentation "")
  (:method (value (url url))
    (#_setUserInfo url value)))

(defgeneric user-name (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_encodedUserName url))
        (#_userName url))))

(defgeneric (setf user-name) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedUserName url value)
        (#_setUserName url value))))

(defgeneric url (url &key encoded)
  (:documentation "")
  (:method ((url url) &key encoded)
    (if encoded
        (#_data (#_toEncoded url))
        (#_toString url))))

(defgeneric (setf url) (value url &key encoded)
  (:documentation "")
  (:method (value (url url) &key encoded)
    (if encoded
        (#_setEncodedUrl url value)
        (#_setUrl url value))))

