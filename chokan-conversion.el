;;; chokan-conversion.el --- conversion support of chokan -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ime

;;; Commentary:
;;
;;: Customization:

;;; Code:

(defvar chokan-conversion-function
  nil
  "変換起動した文字列から、実際に候補を取得する関数。

関数は、引数として変換対象となる文字列と、下線部の直前にあったcontextを受け取る。contextは、 (<type symbol> string) の形式で渡される。
contextが存在しない場合はnilを渡す。
'type symbol'は、'foreign'か'numeric'のいずれかである。

実行した結果として、候補のリストを返す。候補がない場合は'NIL'を返す。候補のリストは、文字列のリストである。
")

(defvar chokan-conversion--candidates nil
  "変換候補のリスト。変換起動が行われるたびに初期化される。")
(defvar chokan-conversion--candidate-pos 0
  "現在選択している候補の位置を 0オリジンで保持する。")

(defconst chokan-conversion--target-character-regexp
  "[a-zA-Z0-9あ-ん]+"
  "変換対象とする文字を検索するための正規表現")

(defun chokan-conversion--get-conversion-region ()
  "現在の下線部があれば、その周辺で変換対象のregionを取得する。
下線部が存在しない場合は 'NIL' を返す。
"
  (let* ((current (point)))
    (save-excursion
      (when-let* ((prop-match (text-property-search-backward 'chokan-conversion-start t t nil))
                  (region (cons (prop-match-beginning prop-match) (prop-match-end prop-match)))
                  ;; ひらがな・アルファベット・数字以外、またはカーソル位置を対象にする
                  (end (re-search-forward chokan-conversion--target-character-regexp current t)))
        (cons (car region) end)))))

(defun chokan-conversion-launch (callback)
  "現在のポイントから変換起動を試みる。変換起動が出来ない場合は、何も行わない。

変換起動が出来た場合は、'CALLBACK'に対象のregionの開始位置と終了位置、最初の変換候補を渡して実行する。変換候補がない場合は、変換候補をnilが設定される。
"

  (when-let* ((region (chokan-conversion--get-conversion-region))
              (start (car region))
              (end (cdr region))
              (str (buffer-substring-no-properties start end)))
    (if chokan-conversion-function
        (progn
          (setq chokan-conversion--candidate-pos 0)
          (setq chokan-conversion--candidates (funcall chokan-conversion-function str nil))

          (let* ((candidate (and chokan-conversion--candidates
                                 (car chokan-conversion--candidates))))
            (funcall callback start end candidate)))
      (funcall callback start end nil)))
  )

(defun chokan-conversion-next-candidate ()
  "次の候補があれば取得する。存在していなければnilを返す"
  (when-let* ((next (nth (1+ chokan-conversion--candidate-pos) chokan-conversion--candidates)))
    (setq chokan-conversion--candidate-pos (1+ chokan-conversion--candidate-pos))
    next))

(defun chokan-conversion-previous-candidate ()
  "前の候補があれば取得する。存在していなければnilを返す。 "
  (if (zerop chokan-conversion--candidate-pos)
      nil
    (when-let* ((prev (nth (1- chokan-conversion--candidate-pos) chokan-conversion--candidates)))
      (setq chokan-conversion--candidate-pos (1- chokan-conversion--candidate-pos))
      prev)))

(provide 'chokan-conversion)
