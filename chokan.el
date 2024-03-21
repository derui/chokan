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
(require 'chokan-conversion)

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

(defface chokan-conversion-start-roman
  '((t (:underline t :foreground "darkgoldenrod")))
  "下線部かつ確定されていないアルファベットに対して適用されるface"
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
           ((or (eq cmd 'chokan-insert-normal-alphabet)
                (eq cmd 'chokan-insert-conversion-start-key)) nil)
           (t
            ;; self-insert-commandではない変更が行われた場合は、確定できていない文字を削除する
            ;; 変換中の文字は、あくまで途中の文字でしか無いので、確定しない限りは、self-insert以外では削除する
            (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
              (if prop
                  (delete-region (prop-match-beginning prop) (prop-match-end prop))
                nil))
            (message "do not touch in %s" cmd)))
        (error nil)))))

(defun chokan--roman-to-kana (alphabet)
  "現在のmodeに従って `alphabet' をかなに変換する。"
  (let* ((kana (chokan-roman-table-roman-to-kana alphabet)))
    (pcase kana
      (`() nil)
      ((and v (pred stringp))
       (if (eq chokan--internal-mode 'hiragana)
           v
         (chokan-roman-table-hira-to-kata v))))))

(defun chokan--get-roman-region ()
  "現在のpointを含むローマ字の未確定領域を取得する。
未確定領域がない場合は 'NIL' を返す。

仕様上、未確定領域は現在のポイントから前にしか存在しない。"
  (save-excursion
    (if-let* ((prop (text-property-search-backward 'chokan-alphabet t t)))
        (cons (prop-match-beginning prop) (prop-match-end prop))
      nil)))

(defun chokan--get-inverse-region ()
  "現在のpointを含む反転部の領域を取得する。
反転部がない場合は 'NIL' を返す。

仕様上、未確定領域は現在のポイントから前にしか存在しない。"
  (save-excursion
    (if-let* ((prop (text-property-search-backward 'chokan-inverse t t nil)))
        (cons (prop-match-beginning prop) (prop-match-end prop))
      nil)))

(defun chokan--convert-roman-to-kana-if-possible (region)
  "chokanのローマ字変換において、確定できていない文字がある場合に、それを変換する。 'REGION'として指定した領域のみで判定する。

結果として、変換できた場合は変換した文字を、出来なかった場合は'NIL'を返す
"
  ;; 変換する領域は、現時点を含んで同じpropertyを持つ領域全体である
  (let* ((current-buffer-string (if region (buffer-substring (car region) (cdr region)) nil))
         (ret (if current-buffer-string
                  (chokan--roman-to-kana current-buffer-string)
                nil)))
    (cond
     ;; 領域がない場合は何もしない
     ((or (null region)
          (null ret)) nil)
     (t
      ret))))

(defun chokan--insert-with-type (str char-props)
  "指定した種別に対応するtext propertyを付与して文字をinsertする。

ここではあくまでtext propertyの設定のみであり、faceは設定しない。

'char-props' は以下のassocである。いずれも `nil' または 't' を取る。

`((roman . nil)
  (conversion-start . nil)
  (inverse . nil))
'

- 'roman' :: ローマ字変換の対象
- 'conversion-start' :: 変換の起点。下線部表記になる
- 'inverse' :: 反転部。かな漢字変換をしている場所になる
"
  (insert str)
  
  (let* ((roman (cdr (assoc 'roman char-props)))
         (conversion-start (cdr (assoc 'conversion-start char-props)))
         (inverse (cdr (assoc 'inverse char-props)))
         (end (point))
         (start (- end (length str))))
    
    (pcase (list roman conversion-start inverse)
      (`(t nil ,_)
       (put-text-property start end 'chokan-alphabet t))
      ;; 大文字のアルファベットのとき
      (`(t t ,_)
       (put-text-property start end 'chokan-alphabet t)
       (put-text-property start end 'chokan-conversion-start t))
      ;; 下線部
      (`(nil t ,_)
       (put-text-property start end 'chokan-conversion-start t))
      ;; 反転部
      (`(nil nil t)
       (put-text-property start end 'chokan-inverse t)))))

(defun chokan--get-face (text-props)
  "text propertiesから、対応するfaceに対応するproperty listを返す"
  (flatten-list (list (if (plist-get text-props 'chokan-conversion-start) '(:underline t) )
                      (if (plist-get text-props 'chokan-inverse) (list
                                                                  :foreground (face-attribute 'default :background)
                                                                  :background (face-attribute 'default :foreground)
                                                                  ))
                      (if (plist-get text-props 'chokan-alphabet) '(:foreground "darkgoldenrod")))))


(defun chokan--propertize-string (converted region)
  "'REGION'の文字列からtext-propertyを判定し、結果としてface設定がされた文字列を返す。

この処理は、前提として下線部またはローマ字部分に限る。反転部については、この関数では処理する必要がないため行うことはない。

'REGION' が 'NIL' の場合はnilを返す。'CONVERTED' のみが 'NIL' の場合は、regionの文字列に対してfaceを設定する
"
  (let* ((original (if region (buffer-substring (car region) (cdr region)) nil))
         (kana-finalized (not (null converted)))
         (converted (or converted original))
         ret
         (current-point 1))
    (with-temp-buffer
      (insert original)
      
      (seq-do (lambda (c)
                (when-let* ((props (text-properties-at current-point))
                            (props (if kana-finalized (plist-put props 'chokan-alphabet nil) props)))
                  (setq ret (concat ret (propertize (string c)
                                                    'face (chokan--get-face props)
                                                    ;; 決定された場合はpropertyを設定しない
                                                    'chokan-alphabet (plist-get props 'chokan-alphabet)
                                                    'chokan-conversion-start (plist-get props 'chokan-conversion-start)
                                                    'chokan-inverse (plist-get props 'chokan-inverse))))
                  (cl-incf current-point)))
              converted))
    ret))

(defun chokan--self-insert (key char-type char-props)
  "chokanでキーに対応する文字を入力するための関数。

'key' は入力されたキー、 'underscore' は入力した文字が下線部になることを示す。
'char-type' は、 'alphabet' 'symbols' のいずれかのsymbolである。 'char-type' が 'alphabet' の場合のみ、ローマ字かな変換を行う。

"
  (cond
   ((eq char-type 'alphabet)
    (chokan--insert-with-type key char-props)

    ;; ローマ字である場合は変換も行う
    (when (cdr (assoc 'roman char-props))
      (let* ((region (chokan--get-roman-region))
             (converted (chokan--convert-roman-to-kana-if-possible region))
             (propertized (chokan--propertize-string converted region)))
        (when propertized
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (insert propertized)))))
   ((eq char-type 'symbols)
    (insert key))))

(defun chokan--insert-candidate (region candidate)
  "指定されたregionに対して 'CANDIDATE'を挿入し、反転部とする。 "
  (let* ((start (car region))
         (end (cdr region))
         (current (point)))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert candidate)
      (add-text-properties start (+ start (length candidate))
                           `(face (:foreground ,(face-attribute 'default :background)
                                               :background ,(face-attribute 'default :foreground))
                                  chokan-inverse t)))
    (when (= end current)
      (goto-char (+ 1 start (length candidate))))))

(defun chokan--launch-conversion-if-possible (convert-launchable)
  "必要なら変換処理を起動し、反転部を作成する。 "

  (when convert-launchable
    (chokan-conversion-launch (lambda (start end candidate)
                                (when candidate
                                  (chokan--insert-candidate (cons start end) candidate))))))

(defun chokan--finalize-inverse-if-possible (finalizable)
  "反転部を確定できる場合は確定する"

  (when-let* (finalizable
              (region (chokan--get-inverse-region)))
    (remove-text-properties (car region) (cdr region) '(chokan-inverse t face nil))))


(defun chokan--insert (convert-launchable underscore char-type)
  "chokanにおける各文字を入力するためのエントリーポイントとなる関数。特殊な記号による入力はこの関数以外で実行すること。

'CONVERT-LAUNCHABLE' が 'non-nil' の場合、起動したコマンドのキーが変換起動可能であることを表す。
'UNDERSCORE' が 'non-nil' の場合、入力した文字が下線部になる。
'CHAR-TYPE' は、 'alphabet' 'symbols' のいずれかのsymbolである。

この関数では以下を実行する。

1. 現在のmodeがカタカナであるかどうか
2. 反転部の確定（ 'convert-launchable' が non-nil である場合）
3. 下線部のかな漢字変換起動（ 'convert-launchable' が non-nil である場合）
4. 下線部の設定（ 'underscore' が non-nil である場合）
5. 自己挿入し、必要ならローマ字かな変換を行う
"
  (let* ((key (this-command-keys)))
    (chokan--finalize-inverse-if-possible convert-launchable)
    (chokan--launch-conversion-if-possible convert-launchable)
    (chokan--self-insert key char-type `((roman . ,(eq char-type 'alphabet))
                                         (conversion-start . ,convert-launchable)
                                         (inverse . nil)))))

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

(defun chokan-insert-conversion-start-key ()
  "変換起動をして文字を入力する"
  (interactive)
  (chokan--insert t t 'alphabet))

(defun chokan-next-candidate ()
  "現在の反転部に対する次の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region)))
        (when-let* ((candidate (chokan-conversion-next-candidate)))
          (chokan--insert-candidate region candidate))
      (let* ((chokan-ja-mode nil)
             (old-func (key-binding current-key)))
        (call-interactively old-func)))))

(defun chokan-previous-candidate ()
  "現在の反転部に対する前の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region)))
        (when-let ((candidate (chokan-conversion-previous-candidate)))
          (chokan--insert-candidate region candidate))
      (let* ((chokan-ja-mode nil)
             (old-func (key-binding current-key)))
        (call-interactively old-func)))))

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
                (add-hook 'post-command-hook #'chokan--post-command nil t)
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
(define-key chokan-ja-mode-map (kbd "C-h") #'chokan-next-candidate)
(define-key chokan-ja-mode-map (kbd "C-g") #'chokan-previous-candidate)

(dolist (k '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-normal-alphabet))
(dolist (k '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-conversion-start-key))

(provide 'chokan)
