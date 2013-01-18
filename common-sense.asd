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

(asdf:defsystem common-sense
  :version "0.1.0"
  :licence "GPLv3"
  :author "Gordon Quad <gordon@niflheim.info"
  :depends-on (:qt
               :alexandria
               :iterate
               :bordeaux-threads)
  :components ((:module src
                        :pathname #p"src/"
                        :components
                        ((:file "package")
                         (:file "enum-converter" :depends-on ("package"))
                         (:file "url" :depends-on ("package" "enum-converter"))
                         (:file "network-access-manager" :depends-on ("package" "enum-converter"))
                         (:file "proxy-factory" :depends-on ("package" "enum-converter"))
                         (:file "web-widget" :depends-on ("package"
                                                          "enum-converter"
                                                          "url"
                                                          "network-access-manager"
                                                          "proxy-factory"))))))
