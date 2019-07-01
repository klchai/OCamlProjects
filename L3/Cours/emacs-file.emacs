
(add-to-list 'load-path "~/tuareg-2.0.4") 

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t) 


; Surlignage apparant
(setq-default transient-mark-mode t)


(global-font-lock-mode 1)

; Revert buffer automatique
(global-auto-revert-mode 1)
(turn-on-auto-fill)

(global-set-key [(control delete)] 'kill-this-buffer)
(global-set-key [(meta g)] 'goto-line)
; (global-set-key [(meta c)] 'goto-char)


(if (>= emacs-major-version 21) (tool-bar-mode 0))

(global-set-key "\C-z"  '(lambda () (interactive) (scroll-up 1)))
(global-set-key "\ez"  '(lambda () (interactive) (scroll-down 1)))


(global-set-key [f4]   'goto-line)
(global-set-key [f5]   'compile)
(global-set-key [f6]   'recompile)
(global-set-key [f7]   'next-error)
(global-set-key [f8]   'normal-mode)

(setq compilation-scroll-output t)
(setq-default tuareg-in-indent 0)

;; Affichage des parenthèse (ou accolades) correspondantes
;; sur fond coloré.
(require 'paren)
(show-paren-mode 1)


;----------
; mode CAML
;----------

(setq auto-mode-alist 
      (cons '("\\.ml[iylp]?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(autoload 'camldebug "camldebug" "Run the Caml debugger." t)


(if (and (boundp 'window-system) window-system)
    (require 'font-lock))

(add-hook 
 'tuareg-mode-hook 
 '(lambda ()
    (define-key tuareg-mode-map [f2] 'tuareg-eval-phrase)
    (define-key tuareg-mode-map [S-f2] 'tuareg-eval-until-point)
    (define-key tuareg-mode-map [f4] 'caml-types-show-type)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-netscape-arguments nil)
 '(browse-url-netscape-program "firefox")
 '(browse-url-netscape-startup-arguments (quote ("-remote")))
 '(browse-url-netscape-version 4)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(user-mail-address "Sylvain.Conchon@lri.fr")
 '(vm-delete-after-saving t))


