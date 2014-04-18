;; helper methods and key binding for scheme mode


;; jump back and forth from current buffer to repl
;;
;; Key binding: M-o
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "M-o") 'switch-to-previous-buffer)


;; eval region or whole buffer and goto repl
;;
;; Key bidning: M-.
(defun scheme-send-region-or-buffer (beg end)
  "In scheme mode, eval the current region(if region is given)
or the whole buffer and go the repl"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (deactivate-mark)
  (scheme-send-region-and-go beg end))

(define-key scheme-mode-map (kbd "M-.") 'scheme-send-region-or-buffer)
