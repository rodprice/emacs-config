;;; Package --- Init file for graphene setup for emacs
;;; Commentary:
;; Stop flycheck from whining about a commentary section
;;; Code:

;; Make sure the emacs server is running
(server-start)

;; Set up proxies on Raytheon machines (Aurora site)
(defvar url-proxy-services
      '(("no_proxy" . "ray.com")
        ("http" . "proxy.ext.ray.com:80")
        ("https" . "proxy.ext.ray.com:80")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'graphene)

(load-theme 'hc-zenburn t)

(provide 'init)
;;; init.el ends here
