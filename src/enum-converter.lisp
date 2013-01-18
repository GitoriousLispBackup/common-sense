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

(defvar *enum-value-hash* (alexandria:plist-hash-table
                           '( ;; Web Attributes
                             :attribute-auto-load-images (#_QWebSettings::AutoLoadImages)
                             :attribute-dns-prefetch-enabled (#_QWebSettings::DnsPrefetchEnabled)
                             :attribute-javascript-enabled (#_QWebSettings::JavascriptEnabled)
                             :attribute-java-enabled (#_QWebSettings::JavaEnabled)
                             :attribute-plugins-enabled (#_QWebSettings::PluginsEnabled)
                             :attribute-private-browsing-enabled (#_QWebSettings::PrivateBrowsingEnabled)
                             :attribute-javascript-can-open-windows (#_QWebSettings::JavascriptCanOpenWindows)
                             :attribute-javascript-can-close-windows (#_QWebSettings::JavascriptCanCloseWindows)
                             :attribute-javascript-can-access-clipboard (#_QWebSettings::JavascriptCanAccessClipboard)
                             :attribute-developer-extras-enabled (#_QWebSettings::DeveloperExtrasEnabled)
                             :attribute-spatial-navigation-enabled (#_QWebSettings::SpatialNavigationEnabled)
                             :attribute-links-included-in-focus-chain (#_QWebSettings::LinksIncludedInFocusChain)
                             :attribute-zoom-text-only (#_QWebSettings::ZoomTextOnly)
                             :attribute-print-element-backgrounds (#_QWebSettings::PrintElementBackgrounds)
                             :attribute-offline-storage-database-enabled (#_QWebSettings::OfflineStorageDatabaseEnabled)
                             :attribute-offline-web-application-cache-enabled (#_QWebSettings::OfflineWebApplicationCacheEnabled)
                             :attribute-local-storage-enabled (#_QWebSettings::LocalStorageEnabled)
                             :attribute-local-storage-database-enabled (#_QWebSettings::LocalStorageDatabaseEnabled)
                             :attribute-local-content-can-access-remote-urls (#_QWebSettings::LocalContentCanAccessRemoteUrls)
                             :attribute-local-content-can-access-file-urls (#_QWebSettings::LocalContentCanAccessFileUrls)
                             :attribute-xss-auditing-enabled (#_QWebSettings::XSSAuditingEnabled)
                             :attribute-accelerated-compositing-enabled (#_QWebSettings::AcceleratedCompositingEnabled)
                             :attribute-tiled-backing-store-enabled (#_QWebSettings::TiledBackingStoreEnabled)
                             :attribute-frame-flattening-enabled (#_QWebSettings::FrameFlatteningEnabled)
                             :attribute-site-specific-quirks-enabled (#_QWebSettings::SiteSpecificQuirksEnabled)

                             ;; Web Features
                             :feature-notifications (#_QWebPage::Notifications)
                             :feature-geolocation (#_QWebPage::Geolocation))))

(defun qt-value (symb)
  (gethash symb *enum-value-hash* -1))

