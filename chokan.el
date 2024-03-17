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

;; global variable

(defvar chokan-mode-map (make-sparse-keymap)
  "Keymap for `chokan-mode'. This keymap is empty by default.
You should call `chokan-mode-setup' to setup keymap for `chokan-mode'.
 ")

;; buffer-local variable

(defvar chokan--internal-mode 'hiragana
  "chokanの現時点で入力しているモード。モードとしては以下が存在する。この変数はバッファローカルである。

chokanが起動された時点では、自動的に `hiragana' に設定される。

- 'ascii' : アルファベットをそのまま入力する
- 'hiragana' : ひらがなを入力する。変換を起動することができる
- 'katakana' : カタカナを入力する。変換を起動することはできない
")

(defvar chokan--roman-not-finalized-char-positions nil
  "chokanのローマ字変換において、確定できていない文字の位置を記録するための変数。")

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

(defun chokan--roman-to-kana (alphabet mode)
  "modeに従って `alphabet' をかなに変換する。

'alphabet' はローマ字のalphabetであることを前提としている。 'mode' は 'hiragana' または 'katakana' のいずれかである。
"

  (let ((kana (chokan-table-roman-to-kana alphabet)))
    (if (eq mode 'katakana)
        (chokan-table-hiragana-to-katakana kana)
      kana))
  )

(defun chokan--insert-kana-if-possible (start end mode)
  "chokanのローマ字変換において、確定できていない文字がある場合に、それを確定するための関数。

`mode' は `hiragana' または `takakana' のいずれかである。

対象の文字かどうかの判断は、text-propertyにある `chokan-alphabet' が `t' であるかどうかで行われる。
"
  (let (start-candidate)
    (setq start-candidate (or (save-excursion
                                (let ((prop (text-property-search-backward 'chokan-alphabet t t t)))
                                  (if prop (prop-match-beginning prop) nil)))
                              start)
          )
    (let ((str (buffer-substring-no-properties start-candidate end)))
      (if-let ((kana (chokan--roman-to-kana str mode)))
          (progn
            (delete-region start-candidate end)
            (insert kana))
        )
      )
    ))

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
      (cond
       ((and (eq cmd 'self-insert-command)) (chokan--self-insert start end length))
       (t
        ;; self-insert-commandではない変更が行われた場合は、確定できていない文字を削除する
        ;; 変換中の文字は、あくまで途中の文字でしか無いので、確定しない限りは、self-insert以外では削除する
        (message "do not touch in %s %d" cmd length)))))
  )

;; command definition
(defun chokan-ascii ()
  "chokanをasciiモードに変更する"
  (interactive)
  (setq chokan--internal-mode 'ascii))

(defun chokan-ja ()
  "chokanを日本語入力モードに変更する"
  (interactive)
  (setq chokan--internal-mode 'hiragana))

;; mode definition

(define-minor-mode chokan-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

`chokan' has some functions to input japanese in Emacs, one of `ASCII' mode for input alphabet, and `JA' mode for input japanese text.

When called interactively, toggle `chokan-mode'.  With prefix ARG, enable `chokan-mode' if ARG is positive, and disable it otherwise.
"
  :after-hook (progn
                (make-variable-buffer-local 'after-change-functions)
                (add-to-list 'after-change-functions #'chokan--after-change)
                )
  )

(provide 'chokan)
