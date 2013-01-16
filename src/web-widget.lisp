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
(ensure-smoke "qtwebkit")

(defvar *network-access-manager-class* nil)
(defvar *web-page-class* nil)
(defvar *proxy-factory-class* nil)

(defclass web-widget ()
  ()
  (:documentation "This class represents webkit widget with web page")
  (:metaclass qt-class)
  (:qt-superclass "QWebView"))

(defmethod initialize-instance :after ((instance web-widget)
                                       &key parent
                                       (network-access-manager-class *network-access-manager-class*)
                                       (web-page-class *web-page-class*)
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

(defgeneric page ((ww web-widget))
  (:documentation "Get page object from web-widget")
  (:method ((ww web-widget))
    (#_page ww)))

(defgeneric network-access-manager (ww)
  (:documentation "Get network access manager object from web-widget")
  (:method ((ww web-widget))
    (#_networkAccessManager (page ww))))

(defgeneric load-url (ww url)
  (:documentation "Load url into web-widget")
  (:method ((ww web-widget) url)
    (#_load ww (#_new QUrl url))))

QWebView:
icon
url
zoomFactor
hasSelection
findText
selectedText
title
textSizeMultiplier
pageAction

QWebPage:
currentFrame ; after DOM
extension
focusNextPreviousChild
history
mainFrame ; after DOM
preferredContentSize
setFeaturePermission
supportedContentTypes
supportsContentType
supportsExtension
totalBytes
triggerAction ; possible same with pageAction
undoStack
viewportSize
shouldInterruptJavascript ; for override

QFrame:
evaluateJavaScript

QWebSettings:
attibute
defaultTextEncoding
localStoragePath
testAttribute
fontFamily

(defgeneric )

