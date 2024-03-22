;;; chokan-symbol.el --- Symbol handling for chokan -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
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

(defconst chokan-symbol-conversion-table
  '(("-" . "ー")
    ("." . "。")
    ("," . "、")
    ("<" . "＜")
    (">" . "＞")
    ("(" . "（")
    (")" . "）")
    ("[" . "［")
    ("]" . "］")
    ("{" . "｛")
    ("}" . "｝")
    ("'" . "’")
    ("\"" . "”")
    ("`" . "‘")
    ("~" . "～")
    ("!" . "！")
    ("?" . "？")
    (";" . "；")
    (":" . "：")
    ("/" . "・")
    ("\\" . "＼")
    ("|" . "｜")
    ("@" . "＠")
    ("#" . "＃")
    ("$" . "＄")
    ("%" . "％")
    ("&" . "＆")
    ("*" . "＊")
    ("+" . "＋")
    ("=" . "＝")
    ("^" . "＾")
    ("_" . "＿")
    )
  "記号類を日本語における記号に変換するテーブル")


(defun chokan-symbol-convert-to-ja (symbol)
  "SYMBOLを日本語における記号に変換する。変換できない場合はnilを返す"
  (let* ((converted (assoc symbol chokan-symbol-conversion-table)))
    (if converted
        (cdr converted)
      nil)))

(provide 'chokan-symbol)
