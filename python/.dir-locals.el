;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; local virtualenv (e.g. via poetry) needs to have
;; - python-lsp-server[rope]
;; - pylsp-mypy
;; - pylsp-rope
;; - python-lsp-ruff

((nil . ((fill-column . 88)))
 (python-mode . ((lsp-enabled-clients . (pylsp))
                 (lsp-pylsp-plugins-flake8-enabled . nil)
                 (lsp-ruff-lsp-show-notifications . onError)
                 )))
