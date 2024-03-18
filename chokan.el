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

(defface chokan-conversion-start
  '((t (:underline t)))
  "下線部に対して適用されるface"
  :group 'chokan)

(defface chokan-inverse
  '((t))
  "反転部に適用されるface"
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

(defun chokan--post-command ()
  "chokanに関する入力を判定するための処理。

直前に行われたコマンドと、現時点のmode、そしてそのコマンドによって変更された範囲を元に、chokanに関する入力を判定する。
"
  (when chokan-mode
    (let ((cmd this-command))
      (condition-case-unless-debug nil
          (cond
           ((and (eq cmd 'chokan-insert-normal-alphabet)) nil)
           (t
            ;; self-insert-commandではない変更が行われた場合は、確定できていない文字を削除する
            ;; 変換中の文字は、あくまで途中の文字でしか無いので、確定しない限りは、self-insert以外では削除する
            (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
              (if prop
                  (delete-region (prop-match-beginning prop) (prop-match-end prop))
                nil))
            (message "do not touch in %s %d" cmd length)))
        (error nil)))))

(defun chokan--roman-to-kana (alphabet)
  "現在のmodeに従って `alphabet' をかなに変換する。"
  (let* ((kana (chokan-roman-table-roman-to-kana alphabet)))
    (cond
     ((null kana) nil)
     ((eq 'ambiguous (car kana)) `(not-detect . ,alphabet))
     (t
      (cons 'detect . (if (eq chokan--internal-mode 'hiragana)
                          kana
                        (chokan-roman-table-hira-to-kata kana))))))
  )

(defun chokan--convert-roman-to-kana-if-possible (key start)
  "chokanのローマ字変換において、確定できていない文字がある場合に、それを確定するための関数。

対象の文字かどうかの判断は、text-propertyにある `chokan-alphabet' が `t' であるかどうかで行われる。

- 'key' は入力されたキー
- 'start' は入力された地点を表す
"
  ;; 変換する領域は、現時点を含んで同じpropertyを持つ領域全体である
  (unless (string= key "")
    (let* ((region (save-excursion
                     (goto-char start)
                     (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
                       (if prop (list (prop-match-beginning prop)
                                      (prop-match-end prop))
                         nil))))
           (ret (if previous-region
                    (chokan--roman-to-kana (buffer-substring (car region) (cadr region)))
                  nil))
           (cond
            ;; 領域がない場合は何もしない
            ((null previous-region) nil)
            ((null ret)
             (delete-region (car region) (cadr region))
             (pcase (chokan--roman-to-kana key)
               ((pred null) (chokan--insert-with-type key 'not-finalized))
               ((pred (lambda (v) (eq (car v) 'not-detect))) (chokan--insert-with-type key 'not-finalized))
               ((pred (lambda (v) (eq (car v) 'detect))) (chokan--insert-with-type key 'normal))))
            ((eq (car ret) 'not-detect))
            ((eq (car ret) 'detect)
             (delete-region (car region) (cadr region))
             (chokan--insert-with-type key 'normal)))))))

(defun chokan--insert-with-type (str type)
  "指定した種別に対応するtext propertyを付与して文字をinsertする。

'type' は以下のいずれかを取る。

- 'not-finalized' :: 未確定のローマ字
- 'normal' :: 通常の文字
- 'conversion-start' :: 変換の起点。下線部表記になる
- 'inverse' :: 反転部。かな漢字変換をしている場所になる
"
  (insert str)
  (let* ((end (point))
         (start (- start (length str))))
    (put-text-property start end 'face 'chokan-kana-roman)
    (put-text-property start end 'chokan-alphabet t)
    )
  (cond
   ((eq type 'not-finalized)
    (put-text-property start end 'face 'chokan-kana-roman)
    (put-text-property start end 'chokan-alphabet t))
   ;; 下線部
   ((eq type 'conversion-start)
    (put-text-property start end 'face 'chokan-conversion-start)
    (put-text-property start end 'chokan-conversion-start t))
   ;; 反転部
   ((eq type 'inverse)
    (put-text-property start end 'face 'chokan-inverse)
    (put-text-property start end 'chokan-inverse t))))

(defun chokan--self-insert (key underscore char-type)
  "chokanでキーに対応する文字を入力するための関数。

'key' は入力されたキー、 'underscore' は入力した文字が下線部になることを示す。
'char-type' は、 'alphabet' 'symbols' のいずれかのsymbolである。 'char-type' が 'alphabet' の場合のみ、ローマ字かな変換を行う。

"
  (cond
   ((eq char-type 'alphabet)
    (chokan--insert-with-type key 'not-finalized)
    (chokan--convert-roman-to-kana-if-possible key (point))
    )
   ((eq char-type 'symbols)
    (insert key))
   ))

(defun chokan--insert (convert-launchable underscore char-type)
  "chokanにおける各文字を入力するためのエントリーポイントとなる関数。特殊な記号による入力はこの関数以外で実行すること。

'convert-launchable' が 'non-nil' の場合、起動したコマンドのキーが変換起動可能であることを表す。
'underscore' が 'non-nil' の場合、入力した文字が下線部になる。
'char-type' は、 'alphabet' 'symbols' のいずれかのsymbolである。

この関数では以下を実行する。

1. 現在のmodeがカタカナであるかどうか
2. 反転部の確定（ 'convert-launchable' が non-nil である場合）
3. 下線部のかな漢字変換起動（ 'convert-launchable' が non-nil である場合）
4. 下線部の設定（ 'underscore' が non-nil である場合）
5. 自己挿入し、必要ならローマ字かな変換を行う
"
  (let* ((type (if underscore 'conversion-start 'normal))
         (key (this-command-keys)))
    (cond
     ((eq char-type 'alphabet)
      (chokan--self-insert key underscore char-type))
     ((eq char-type 'symbols)
      (insert key))
     )
    )
  )

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

(defun chokan-insert-normal-alphabet ()
  "変換起動をしないで文字を入力する"
  (interactive)
  (chokan--insert nil nil 'alphabet))

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

(defun chokan-mode--activate ()
  "chokan-modeが起動するときに実行する処理をまとめた*関数"

  (set-face-attribute 'chokan-inverse nil :foreground (face-attribute 'default :background))
  (set-face-attribute 'chokan-inverse nil :background (face-attribute 'default :foreground))

  (setq-local chokan--default-cursor-type cursor-type)
  (chokan-ascii-mode)
  )

(define-minor-mode chokan-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

`chokan' has some functions to input japanese in Emacs, one of `ASCII' mode for input alphabet, and `JA' mode for input japanese text.

When called interactively, toggle `chokan-mode'.  With prefix ARG, enable `chokan-mode' if ARG is positive, and disable it otherwise.
"
  :keymap chokan-mode-map
  :after-hook (progn
                (make-variable-buffer-local 'after-change-functions)
                (add-hook 'post-command-hook #'chokan--after-change nil t)
                (if chokan-mode
                    (chokan-mode--activate)
                  (setq cursor-type chokan--default-cursor-type)
                  (chokan-ja-mode -1)
                  (chokan-ascii-mode -1)))
  )

;; setup initial keymap
(define-key chokan-ascii-mode-map (kbd "C-j") #'chokan-ja)
(define-key chokan-ja-mode-map (kbd "M-c") #'chokan-ascii)
(define-key chokan-ja-mode-map (kbd "*") #'chokan-toggle-katakana)

(dolist (k '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-normal-alphabet))

(provide 'chokan)
