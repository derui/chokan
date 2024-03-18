;;; chokan.el --- cho-tto Kanzen - re-implementation Kanzen -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "29.1"))
;; Keywords: imput method

;;; Commentary:
;;
;; chokan.el provides functionalities to input japanese text Input Method with Kanzen method.
;; Kanzen is originally developed by Takeuchi Ikuo.
;;
;; https://www.nue.org/nue/tao/kanzen/wdj.html
;;

;;: Customization:

;;; Code:

(require 'chokan-roman-table)

(defgroup chokan nil
  "chokan - cho-tto Kanzen"
  :group 'input-method
  :prefix "chokan-")

(defcustom chokan-katakana-cursor-type 'hollow
  "カタカナ入力モードの際のカーソルの形状。デフォルトでは下線"
  :type 'symbol
  :group 'chokan)

(defcustom chokan-ascii-cursor-type 'bar
  "asciiモードの際のカーソルの形状。デフォルトではbar"
  :type 'symbol
  :group 'chokan)

(defcustom chokan-ja-cursor-type '(hbar . 2)
  "日本語入力モードの際のカーソルの形状。デフォルトではhollow box"
  :type 'symbol
  :group 'chokan)

;; global variable

(defvar chokan-mode-map (make-sparse-keymap)
  "Keymap for `chokan-mode'. This keymap is empty by default.
You should call `chokan-mode-setup' to setup keymap for `chokan-mode'.
 ")

(defvar chokan-ascii-mode-map (make-sparse-keymap)
  "Keymap for `chokan-ascii-mode'.
 ")

(defvar chokan-ja-mode-map (make-sparse-keymap)
  "Keymap for `chokan-ja-mode'. ")

;; buffer-local variable

(defvar chokan--internal-mode 'hiragana
  "chokanの現時点で入力しているモード。モードとしては以下が存在する。この変数はバッファローカルである。

chokanが起動された時点では、自動的に `hiragana' に設定される。

- 'ascii' : アルファベットをそのまま入力する
- 'hiragana' : ひらがなを入力する。変換を起動することができる
- 'katakana' : カタカナを入力する。変換を起動することはできない
")

(defvar chokan--default-cursor-type nil
  "chokanが終了したときに戻すためのcursorの形状。この変数はバッファローカルである")

;; faces

(defface chokan-kana-roman
  '((t (:foreground "darkgoldenrod")))
  "ひらがな・カタカナを入力している際に確定されていないアルファベットに対して適用されるface。"
  :group 'chokan)

;; internal functions

(defsubst chokan--ascii-p ()
  "現在chokanがascii modeであるかどうかを返す"
  (and chokan-mode (eq chokan--internal-mode 'ascii)))

(defsubst chokan--ja-p ()
  "現在chokanが日本語入力モードであるかどうかを返す"
  (and chokan-mode (not (eq chokan--internal-mode 'ascii))))

(defsubst chokan--ja-katakana-p ()
  "現在chokanがカタカナ入力モードであるかどうかを返す"
  (and (chokan--ja-p) (eq chokan--internal-mode 'katakana)))

(defun chokan--roman-to-kana (alphabet mode)
  "modeに従って `alphabet' をかなに変換する。

'alphabet' はローマ字のalphabetであることを前提としている。 'mode' は 'hiragana' または 'katakana' のいずれかである。

返却される値は、以下のいずれかである。

* '(<かな> . <rest of input>)' : 変換に成功した場合。rest of inputは、再度変換対象として利用する必要がある
* <input string> : 将来的に変換可能な組み合わせが存在するが、確定できない場合
* nil : 対応する組み合わせがない場合
"

  (let* ((kana (chokan-roman-table-roman-to-kana alphabet)))
    (cond
     ((eq 'not-found (car kana)) nil)
     ((eq 'ambiguous (car kana)) alphabet)
     ((eq 'found (car kana))
      (let ((kana (cadr kana))
            (rest (cddr kana)))
        (if (eq mode 'hiragana)
            (cons kana rest)
          (cons (chokan-roman-table-hira-to-kata kana) rest))))
     )
    )
  )

(defun chokan--insert-kana-if-possible (start end mode)
  "chokanのローマ字変換において、確定できていない文字がある場合に、それを確定するための関数。

`mode' は `hiragana' または `takakana' のいずれかである。

対象の文字かどうかの判断は、text-propertyにある `chokan-alphabet' が `t' であるかどうかで行われる。
"
  (let ((start-candidate (or (save-excursion
                               (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
                                 (if prop (prop-match-beginning prop) nil)))
                             start)))
    ;; 開始地点は、現時点を含んで同じpropertyを持つ領域全体である
    (let* ((target-string (buffer-substring-no-properties start-candidate end))
           (ret (chokan--roman-to-kana target-string mode)))
      (cond
       ((null ret) (delete-region start-candidate (1- end)))
       ((stringp ret) nil)
       (t (progn
            (delete-region start-candidate end)
            (insert (car ret))
            (insert (cdr ret))
            (let* (
                   (start (- (point) (length (cdr ret))))
                   (end (point)))
              (put-text-property start end 'face 'chokan-kana-roman)
              (put-text-property start end 'chokan-alphabet t))))))))

(defun chokan--ja-self-insert (start end str)
  "日本語入力モードでのself-insertの処理をおこなう"
  (when (and (chokan--ja-p)
             (s-matches-p "[a-z]" str))
    (progn
      (put-text-property start end 'face 'chokan-kana-roman)
      (put-text-property start end 'chokan-alphabet t)

      (chokan--insert-kana-if-possible start end chokan--internal-mode)
      )
    )
  )

(defun chokan--self-insert (start end length)
  "chokanに関する入力を判定するための処理。

`start' は入力の開始位置、 `end' は同終了位置、 `length' は入力された文字列の長さを表す。

この関数では、モードによってまず大きく分岐する。alphabetモードの場合は何も行わない。
日本語入力モードの場合は、入力文字と、今残っているpropertyがついた文字について判定する。
"

  (let ((str (buffer-substring-no-properties start end)))
    (cond
     ((chokan--ascii-p) nil)
     ((chokan--ja-p) (chokan--ja-self-insert start end str))
     (t nil))))

(defun chokan--after-change (start end length)
  "chokanに関する入力を判定するための処理。

直前に行われたコマンドと、現時点のmode、そしてそのコマンドによって変更された範囲を元に、chokanに関する入力を判定する。
"
  (when chokan-mode
    (let ((cmd this-command))
      (condition-case-unless-debug nil
          (cond
           ((and (eq cmd 'self-insert-command)) (chokan--self-insert start end length))
           (t
            ;; self-insert-commandではない変更が行われた場合は、確定できていない文字を削除する
            ;; 変換中の文字は、あくまで途中の文字でしか無いので、確定しない限りは、self-insert以外では削除する
            (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
              (if prop
                  (delete-region (prop-match-beginning prop) (prop-match-end prop))
                nil))
            (message "do not touch in %s %d" cmd length)))
        (error nil)))))

;; command definition
(defun chokan-ascii ()
  "chokanをasciiモードに変更する"
  (interactive)
  (chokan-ja-mode -1)
  (chokan-ascii-mode +1)
  (setq cursor-type chokan-ascii-cursor-type))

(defun chokan-ja ()
  "chokanを日本語入力モードに変更する"
  (interactive)
  (chokan-ascii-mode -1)
  (chokan-ja-mode +1)
  (setq cursor-type chokan-ja-cursor-type))

(defun chokan-toggle-katakana ()
  "chokanの内部モードをカタカナ入力に変更する"
  (interactive)
  (when (chokan--ja-p)
    (if (not (chokan--ja-katakana-p))
        (progn 
          (setq chokan--internal-mode 'katakana)
          (setq cursor-type chokan-katakana-cursor-type))
      (setq chokan--internal-mode 'hiragana)
      (setq cursor-type chokan-ja-cursor-type))))

;; mode definition

(define-minor-mode chokan-ascii-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

This mode only handle to keymap for changing mode to `chokan-mode' and `chokan-ja-mode'.
"
  :keymap chokan-ascii-mode-map
  :after-hook (progn
                (setq chokan--internal-mode 'ascii)
                ))

(define-minor-mode chokan-ja-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

This mode only handle to keymap for changing mode to `chokan-mode' and `chokan-ascii-mode'.
"
  :keymap chokan-ja-mode-map
  :after-hook (progn
                (setq chokan--internal-mode 'hiragana)
                ))

(define-minor-mode chokan-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

`chokan' has some functions to input japanese in Emacs, one of `ASCII' mode for input alphabet, and `JA' mode for input japanese text.

When called interactively, toggle `chokan-mode'.  With prefix ARG, enable `chokan-mode' if ARG is positive, and disable it otherwise.
"
  :keymap chokan-mode-map
  :after-hook (progn
                (make-variable-buffer-local 'after-change-functions)
                (add-to-list 'after-change-functions #'chokan--after-change)
                (if chokan-mode
                    (progn
                      (setq-local chokan--default-cursor-type cursor-type)
                      (chokan-ascii-mode))
                  (setq cursor-type chokan--default-cursor-type)
                  (chokan-ja-mode -1)
                  (chokan-ascii-mode -1)))
  )

;; setup initial keymap
(define-key chokan-ascii-mode-map (kbd "C-j") #'chokan-ja)
(define-key chokan-ja-mode-map (kbd "M-c") #'chokan-ascii)
(define-key chokan-ja-mode-map (kbd "*") #'chokan-toggle-katakana)

(provide 'chokan)
