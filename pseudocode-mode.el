;;; pseudocode-mode.el --- A major mode for pseudocode -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/pseudocode-mode
;; Version: 0.1.0
;; Package-Requires: ((ht "2.3"))
;; Keywords: pseudocode
;; prefix: pseudocode-

;; This file is not a part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Example pseudocode syntax:

;; Function doX(a, b)
;; Input a and b
;; Output does x
;; a <- 1
;; if b = 0 then
;;   doSomething(a, b)
;; return a * b

;; This program has a minor mode in which it matches blocks like this:
;;
;; /*
;;  * Function doX(a, b)
;;  * Input a and b
;;  * Output does x
;;  * a <- 1
;;  * if b = 0 then
;;  *   doSomething(a, b)
;;  * return a * b
;;  */
;; That is, C style comments beginning with Function.

;;; Code:
(require 'ht)
(require 'cl-lib)


(defconst pseudocode-keyword-list
  '(Function
    Input
    Output
    <--
    <-
    if
    then
    else
    NOT
    AND
    while
    repeat
    return))

(defconst pseudocode-before-keyword
  "\\(\\(?:^\\|[[:space:]]\\)\\("
  "The things that can come before a keyword")

(defconst pseudocode-after-keyword
  "\\)\\(?:$\\|[[:space:]]\\)\\)"
  "The things that can come after a keyword")

(defconst pseudocode-keywords
  (let* ((b pseudocode-before-keyword)
         (a pseudocode-after-keyword)
         (rest (cdr pseudocode-keyword-list))
         (first (car pseudocode-keyword-list))
         (first (format "%s%s%s" b first a)))
    (concat first (cl-loop for item in rest
                           concat (format "\\|%s%s%s" b item a))))
  "Keywords in psuedocode.")

(defconst pseudocode-match-function-name
  "\\(Function \\)\\(.+\\)\\(?: ?(\\)"
  "Matches the function name of the psuedocode")

(defconst pseudocode-match-function-variable-declaration
  "\\(Function [^(]+(\\)\\([^,]\\(, ?\\)\\)+\\([^)]\\))"
  "Matches variable names in the algoirhtm definition")

(defconst pseudocode-match-variable-declaration
  "\\(?:[^$_a-zA-Z]\\)\\([$_a-zA-Z][$_0-9a-zA-Z]*?\\) <--? .+$"
  "Matches a variable declaration elsewhere")

(defvar pseudocode-mode-highlights nil
  "Highlighted keywords in pseudocode mode.")

(setq pseudocode-mode-highlights
      `((,pseudocode-keywords 0 font-lock-keyword-face)
        (,pseudocode-match-function-name 2 font-lock-function-name-face)
        (,pseudocode-match-function-variable-declaration 2 font-lock-variable-name-face)
        (,pseudocode-match-function-variable-declaration 4 font-lock-variable-name-face)
        (,pseudocode-match-variable-declaration 1 font-lock-variable-name-face)))

;;;###autoload
(define-derived-mode pseudocode-mode prog-mode "psuedocode"
  "A mode for editing and viewing psuedocode."
  (setq font-lock-defaults '(pseudocode-mode-highlights)))

;; code for finding comments and highlighting functions in them

(defconst pseudocode-function-comment-matcher
  "/\\*[ \n\\*]*Function [$_a-zA-Z][$_a-zA-Z0-9]*?(\\([$_a-zA-Z][$_a-zA-Z0-9]*,? ?\\)*)\\(.\\|\n\\)+?\\*/"
  "Matches comments with functions")

(defun pseudocode-re-noerr (regexp &optional bound count)
  (re-search-forward regexp bound t count))

(defun pseudocode-overlay-one-comment ()
  (with-silent-modifications
    (let ((case-fold-search nil))
      (when (pseudocode-re-noerr pseudocode-function-comment-matcher nil)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (varlist (ht-create)))
          (remove-overlays beg end 'pseudocode t)
          (goto-char beg)
          (while (pseudocode-re-noerr pseudocode-keywords end)
            (let ((o (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put o 'pseudocode t)
              (overlay-put o 'face 'font-lock-keyword-face)))
          (goto-char beg)
          (when (pseudocode-re-noerr pseudocode-match-function-name end)
            (let ((o (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put o 'pseudocode t)
              (overlay-put o 'face font-lock-function-name-face)))
          (goto-char beg)
          (pseudocode-re-noerr "Function [^(]+?(" end)
          (let ((keep-going t))
            (while keep-going
              (let* ((param-list-beg (point))
                     (param-list-end param-list-beg))
                (while (not (or (= (char-after) ?,) (= (char-after) ?\))))
                  (forward-char)
                  (cl-incf param-list-end))
                (when (or (= (char-after) ?\)))
                  (setq keep-going nil))
                (forward-char)
                (when (not (equal param-list-beg param-list-end))
                  (let ((o (make-overlay param-list-beg param-list-end)))
                    (overlay-put o 'pseudocode t)
                    (overlay-put o 'face 'font-lock-variable-name-face))
                  (ht-set! varlist (buffer-substring-no-properties param-list-beg param-list-end) t)))))
          (goto-char beg)
          (while (pseudocode-re-noerr pseudocode-match-variable-declaration end)
            (let* ((var-beg (match-beginning 1))
                   (var-end (match-end 1))
                   (s (buffer-substring-no-properties var-beg var-end)))
              (when (not (ht-get varlist s))
                (ht-set! varlist s t)
                (let ((o (make-overlay (match-beginning 1) (match-end 1))))
                  (overlay-put o 'pseudocode t)
                  (overlay-put o 'face 'font-lock-variable-name-face)))))
          (goto-char end))
        t))))

(defun pseudocode--region (start end)
  ;; FIXME: This code goes over the whole buffer no matter what.
  ;; I couldn't figure out another way to do it.
  (setq start (point-min)
        end (point-max))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (pseudocode-overlay-one-comment))
      t)))

;;;###autoload
(define-minor-mode pseudocode-comment-mode
  "A minor mode for highlighting functions in c style comments"
  nil
  "pseudocode-comments"
  nil
  (jit-lock-unregister #'pseudocode--region)
  (remove-overlays (point-min) (point-max) 'pseudocode t)
  (when pseudocode-comment-mode
    (jit-lock-register #'pseudocode--region t)
    (pseudocode--region (point-min) (point-max))))

(provide 'pseudocode-mode)
;;; pseudocode-mode.el ends here
