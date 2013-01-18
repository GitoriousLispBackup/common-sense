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

(defclass web-widget ()
  ()
  (:documentation "This class represents webkit widget with web page")
  (:metaclass qt-class)
  (:qt-superclass "QWebView"))

(defmethod initialize-instance :after ((instance web-widget)
                                       &key parent
                                       (network-access-manager-class *network-access-manager-class*)
                                       (web-page-class nil)
                                       (proxy-factory-class *proxy-factory-class*))
  (if parent
      (new instance parent)
      (new instance))
  (when web-page-class
    (#_setPage instance (make-instance web-page-class)))
  (when network-access-manager-class
    (#_setNetworkAccessManager (page instance)
                               (make-instance network-access-manager-class)))
  (when proxy-factory-class
    (#_setProxyFactory (network-access-manager instance)
                       (make-instance proxy-factory-class))))

;; Hail boilerplate!

(defgeneric page (ww)
  (:documentation "Get page object from web-widget")
  (:method ((ww web-widget))
    (#_page ww)))

(defgeneric network-access-manager (ww)
  (:documentation "Get network access manager object")
  (:method ((ww web-widget))
    (#_networkAccessManager (page ww))))

(defgeneric load-url (ww url)
  (:documentation "Load url")
  (:method ((ww web-widget) (url url))
    (#_load ww url))
  (:method ((ww web-widget) (url string))
    (#_load ww (make-instance 'url url))))

(defgeneric (setf url) (url ww)
  (:documentation "Set url")
  (:method ((url url) (ww web-widget))
    (#_setUrl ww url))
  (:method (url (ww web-widget))
    (#_setUrl ww (make-instance 'url url))))

(defgeneric url (ww)
  (:documentation "Get url")
  (:method ((ww web-widget))
    (#_url ww)))

(defgeneric icon (ww)
  (:documentation "Get favicon")
  (:method ((ww web-widget))
    (#_icon ww)))

(defgeneric zoom-factor (ww)
  (:documentation "Get zoom factor")
  (:method ((ww web-widget))
    (#_zoomFactor ww)))

(defgeneric (setf zoom-factor) (zf ww)
  (:documentation "Set zoom factor")
  (:method (zf (ww web-widget))
    (#_setZoomFactor ww zf)))

(defgeneric has-selection (ww)
  (:documentation "Is there a selection in web view")
  (:method ((ww web-widget))
    (#_hasSelection ww)))

(defgeneric selected-text (ww)
  (:documentation "Get selected text")
  (:method ((ww web-widget))
    (#_selectedText ww)))

(defgeneric title (ww)
  (:documentation "Get title text")
  (:method ((ww web-widget))
    (#_title ww)))

(defgeneric text-size-multiplier (ww)
  (:documentation "Get text size factor")
  (:method ((ww web-widget))
    (#_textSizeMultipliler ww)))

(defgeneric (setf text-size-multiplier) (tsm ww)
  (:documentation "Set text size factor")
  (:method (tsm (ww web-widget))
    (#_setTextSizeMultiplier ww tsm)))

(defgeneric focus-next-prev-child (ww &key next-child)
  (:documentation "Get text size factor")
  (:method ((ww web-widget) &key next-child)
    (#_focusNextPrevChild (page ww) next-child)))

(defgeneric feature-permission (ww &key feature permission)
  (:documentation "Get text size factor")
  (:method ((ww web-widget) &key feature permission)
    (if permission
        (#_setFeaturePermission (page ww) (qt-value feature)
                                (#_QWebView::PermissionGrantedByUser))
        (#_setFeaturePermission (page ww) (qt-value feature)
                                (#_QWebView::PermissionDenienByUser)))))

(defgeneric supported-content-types (ww)
  (:documentation "Get supported content types")
  (:method ((ww web-widget))
    (#_supportedContentTypes (page ww))))

(defgeneric supports-content-type (ww ct)
  (:documentation "Is this content-type supported")
  (:method ((ww web-widget) ct)
    (#_supportsContentType (page ww) ct)))

(defgeneric supports-extension (ww ext)
  (:documentation "Is this extension supported")
  (:method ((ww web-widget) ext)
    (#_supportsExtension(page ww) ext)))

(defgeneric total-bytes (ww)
  (:documentation "Get total bytes of loaded content")
  (:method ((ww web-widget))
    (#_totalBytes (page ww))))

(defgeneric evaluate-javascript (ww js)
  (:documentation "Evaluate javascript")
  (:method ((ww web-widget) js)
    (#_evaluateJavascript (#_mainFrame (page ww)))))

(defgeneric web-attribute (ww attr)
  (:documentation "Get value of web attribute")
  (:method ((ww web-widget) attr)
    (#_testAttribute (#_settings (page ww)) (qt-value attr))))

(defgeneric (setf web-attribute) (value ww attr)
  (:documentation "Set value if web attribute")
  (:method (value (ww web-widget) attr)
    (#_setAttribute (#_settings (page ww)) (qt-value attr) value)))

(defgeneric reset-web-attribute (ww attr)
  (:documentation "Resets value of web attribute to default")
  (:method ((ww web-widget) attr)
    (#_resetAttribute (#_settings (page ww)) (qt-value attr))))

(defgeneric default-text-encoding (ww)
  (:documentation "Get value of default text encoding")
  (:method ((ww web-widget))
    (#_defaultTextEncoding (#_settings (page ww)))))

(defgeneric (setf default-text-encoding) (value ww)
  (:documentation "Set value of default text encoding")
  (:method (value (ww web-widget))
    (#_setDefaultTextEncoding (#_settings (page ww)) value)))

;; TODO:
;; findText
;; pageAction
;; QWebPage:
;; currentFrame - after DOM
;; extension - ?
;; history - QList
;; mainFrame - after DOM
;; preferredContentSize - wtf is that
;; triggerAction ; possible same with pageAction
;; undoStack
;; viewportSize
;; shouldInterruptJavascript ; for override
;; QWebSettings:
;; fontFamily
;; localStoragePath
