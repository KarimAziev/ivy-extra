;;; ivy-extra.el --- Configure extra -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-extra
;; Keywords: lisp
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (ivy "0.14.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with extra

;; Commands

;; M-x `ivy-extra-read' (prompt collection &rest ivy-args)
;;      Configure and call `ivy-read' with PROMPT, COLLECTION and IVY-ARGS.
;;      IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
;;      excluding caller.

;; M-x `ivy-extra-read-multi' (prompt collection &rest ivy-args)
;;     Read COLLECTION with PROMPT and return list with selected candidates.
;;     IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
;;     excluding:

;;     - :action
;;     - :multi-action
;;     - :caller

;;     but accepting additional:

;;     - :persistent-action
;;     - :premarked

;; Persistent action will be called with current candidate without exiting
;; completion.

;; Premarked is candidates from COLLECTION which should be initially marked
;;; Code:

(require 'ivy)

(defvar ivy-extra-configure-keywords
  '(:parent :initial-input :height :occur
            :update-fn :init-fn :unwind-fn
            :index-fn :sort-fn :sort-matches-fn
            :format-fn :display-fn :display-transformer-fn
            :alt-done-fn :more-chars :grep-p :exit-codes))

(defvar ivy-extra-ivy-read-keywords
  '(:predicate :require-match :initial-input
               :history :preselect
               :def :keymap :update-fn :sort
               :unwind :re-builder :matcher
               :dynamic-collection
               :extra-props
               :action :multi-action))

(defun ivy-extra-plist-pick (plist keywords)
  "Pick KEYWORDS from PLIST."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when (memq keyword plist)
        (let ((value (plist-get plist keyword)))
          (setq result (nconc result
                              (list keyword
                                    value))))))
    result))

(defun ivy-extra-plist-omit (plist keywords)
  "Omit KEYWORDS with it's values from PLIST."
  (if (seq-find (lambda (it) (memq it plist)) keywords)
      (let ((result))
        (while plist
          (let* ((key (pop plist))
                 (val (pop plist)))
            (unless (memq key keywords)
              (push (list key val) result))))
        (reverse result))
    plist))

(defun ivy-extra-switch-buffer-action-no-record (buffer)
  "Switch to BUFFER without record.
BUFFER may be a string or nil."
  (let ((buff (if (zerop (length buffer))
                  (get-buffer ivy-text)
                (get-buffer buffer)))
        (virtual)
        (view))
    (cond ((and buff
                (buffer-live-p buff))
           (pop-to-buffer-same-window buff))
          ((setq virtual (assoc buffer ivy--virtual-buffers))
           (find-file (cdr virtual)))
          ((setq view (assoc buffer ivy-views))
           (delete-other-windows)
           (let (;; silence "Directory has changed on disk"
                 (inhibit-message t))
             (ivy-set-view-recur (cadr view))))
          (t
           (switch-to-buffer buffer t 'force-same-window)))))

;;;###autoload
(defun ivy-extra-read (prompt collection &rest ivy-args)
  "Configure and call `ivy-read' with PROMPT, COLLECTION and IVY-ARGS.
IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
excluding caller."
  (interactive)
  (dolist (alist-sym '(ivy--parents-alist
                       ivy-initial-inputs-alist
                       ivy-height-alist
                       ivy-update-fns-alist
                       ivy-unwind-fns-alist
                       ivy-init-fns-alist
                       ivy-index-functions-alist
                       ivy-sort-functions-alist
                       ivy-sort-matches-functions-alist
                       ivy-format-functions-alist
                       ivy-display-functions-alist
                       ivy--display-transformers-alist
                       ivy-alt-done-functions-alist
                       ivy-more-chars-alist))
    (ivy--alist-set alist-sym 'ivy-extra-read nil))
  (when (and (boundp 'counsel--async-exit-code-plist)
             (plist-get counsel--async-exit-code-plist 'ivy-extra-read))
    (setq counsel--async-exit-code-plist
          (ivy-extra-plist-omit counsel--async-exit-code-plist
                                '(ivy-extra-read))))
  (let ((args (append
               (list prompt
                     collection
                     :caller 'ivy-extra-read)
               (ivy-extra-plist-pick
                ivy-args
                ivy-extra-ivy-read-keywords)))
        (configure-args (ivy-extra-plist-pick
                         ivy-args
                         ivy-extra-configure-keywords)))
    (when configure-args
      (push 'ivy-extra-read configure-args)
      (apply #'ivy-configure configure-args))
    (apply #'ivy-read args)))

(defun ivy-extra-mark-candidates (candidates)
  "Mark CANDIDATES from ivy collection."
  (dolist (cand (ivy-state-collection
                 ivy-last))
    (when (member cand
                  candidates)
      (let ((marked-cand (concat
                          ivy-mark-prefix
                          cand)))
        (setq ivy--old-cands
              ivy--all-candidates)
        (setcar
         (member cand
                 ivy--all-candidates)
         (setcar
          (member cand
                  ivy--old-cands)
          marked-cand))
        (setq ivy-marked-candidates
              (append
               ivy-marked-candidates
               (list
                marked-cand)))))))

;;;###autoload
(defun ivy-extra-read-multi (prompt collection &rest ivy-args)
  "Read COLLECTION with PROMPT and return list with selected candidates.
IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
excluding:

- :action
- :multi-action
- :caller

but accepting:

- :persistent-action
- :premarked

Persistent action will be called with current candidate without exiting
completion.

Premarked is candidates from COLLECTION which should be initially marked."
  (interactive)
  (dolist (alist-sym '(ivy--parents-alist
                       ivy-initial-inputs-alist
                       ivy-height-alist
                       ivy-update-fns-alist
                       ivy-unwind-fns-alist
                       ivy-init-fns-alist
                       ivy-index-functions-alist
                       ivy-sort-functions-alist
                       ivy-sort-matches-functions-alist
                       ivy-format-functions-alist
                       ivy-display-functions-alist
                       ivy--display-transformers-alist
                       ivy-alt-done-functions-alist
                       ivy-more-chars-alist))
    (ivy--alist-set alist-sym 'ivy-extra-read-multi nil))
  (when (and (boundp 'counsel--async-exit-code-plist)
             (plist-get counsel--async-exit-code-plist
                        'ivy-extra-read-multi))
    (setq counsel--async-exit-code-plist
          (ivy-extra-plist-omit counsel--async-exit-code-plist
                                '(ivy-extra-read-multi))))
  (let ((marked)
        (persistent-action (plist-get ivy-args :persistent-action))
        (premarked-candidates (plist-get ivy-args :premarked)))
    (let ((args (append
                 (list prompt
                       collection
                       :caller 'ivy-extra-read-multi
                       :action (lambda (item)
                                 (when (and persistent-action
                                            (null ivy-exit))
                                   (funcall persistent-action item))
                                 item)
                       :multi-action (lambda (children)
                                       (setq marked children)))
                 (ivy-extra-plist-pick
                  ivy-args
                  (seq-difference ivy-extra-ivy-read-keywords
                                  '(:multi-action
                                    :action)))))
          (configure-args (ivy-extra-plist-pick
                           ivy-args
                           ivy-extra-configure-keywords))
          (item))
      (when configure-args
        (push 'ivy-extra-read-multi configure-args)
        (apply #'ivy-configure configure-args))
      (setq item (if premarked-candidates
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (when (active-minibuffer-window)
                             (ivy-extra-mark-candidates premarked-candidates)))
                       (apply #'ivy-read args))
                   (apply #'ivy-read args)))
      (or marked
          (when item (list item))))))

;;;###autoload
(defun ivy-extra-update (candidates &optional prompt)
  "Update ivy collection to CANDIDATES without exiting minibuffer.
With optional argument PROMPT also update `ivy--prompt'."
  (ivy-update-candidates candidates)
  (let ((input ivy-text)
        (pos
         (when-let ((wind (active-minibuffer-window)))
           (with-selected-window wind
             (point))))
        (prompt-end (minibuffer-prompt-end))
        (diff))
    (delete-minibuffer-contents)
    (setq diff (- pos prompt-end))
    (setf (ivy-state-collection ivy-last)
          ivy--all-candidates)
    (setf (ivy-state-preselect ivy-last)
          ivy--index)
    (ivy--reset-state ivy-last)
    (when prompt
      (setq ivy--prompt prompt))
    (when-let ((wind (active-minibuffer-window)))
      (with-selected-window wind
        (insert input)
        (goto-char (minibuffer-prompt-end))
        (forward-char diff)))))

(defcustom ivy-extra-incompatible-ivy-read-modes '(fido-mode
                                                   icomplete-mode
                                                   vertico-mode
                                                   ido-mode
                                                   minibuffer-auto-mode
                                                   fido-vertical-mode)
  "List of modes to disable before `ivy-read' calling."
  :type '(repeat symbol)
  :group 'ivy)

(defcustom ivy-extra-extra-default-actions '((describe-keymap . describe-keymap)
                                             (describe-variable . describe-variable)
                                             (describe-function . describe-function)
                                             (cl-describe-type . cl-describe-type)
                                             (describe-face . describe-face)
                                             (describe-symbol . describe-symbol)
                                             (describe-icon . describe-icon)
                                             (describe-command . describe-command))
  "Alist of extra commands and default ivy actions."
  :type '(alist
          :key-type (symbol :tag "Command")
          :value-type (symbol :tag "Action"))
  :group 'ivy-extra)




(defun ivy-extra-disable-incompatible-modes (fn &rest args)
  "Disable incompatible modes for FN, invoke FN with ARGS and then restore modes.
Incompatible modes are stored in `ivy-extra-incompatible-ivy-read-modes'.
Usage:
\\=(advice-add \\='ivy-read :around #\\='ivy-extra-disable-incompatible-modes)."
  (let ((active-modes
         (seq-filter
          (lambda (mode)
            (and
             (boundp mode)
             (symbol-value mode)))
          ivy-extra-incompatible-ivy-read-modes)))
    (if active-modes
        (unwind-protect
            (progn
              (dolist (mode active-modes)
                (when (and
                       (boundp mode)
                       (symbol-value mode))
                  (funcall mode -1)))
              (apply fn args))
          (dolist (mode active-modes)
            (funcall mode 1)))
      (apply fn args))))


(defun ivy-extra-add-extra-actions ()
  "Add default actions to commands based on `ivy-extra-extra-default-actions'."
  (pcase-dolist (`(,command . ,action) ivy-extra-extra-default-actions)
    (ivy-add-actions command
                     `(("o" .
                        ,(cond ((commandp action)
                                `((lambda (sym)
                                    (funcall-interactively
                                     #',action
                                     (intern
                                      sym)))
                                  "default"))
                               (t `((lambda (sym)
                                      (funcall #',action (intern sym)))
                                    "default"))))))))




(provide 'ivy-extra)
;;; ivy-extra.el ends here