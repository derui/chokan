;;; chokan-roman-table.el --- Hepburn romanization conversion table -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
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

(defconst chokan--roman-table
  '(
    ("a" . "あ")
    ("i" . "い")
    ("u" . "う")
    ("e" . "え")
    ("o" . "お")
    ("o" . "お")
    ("ka" . "か")
    ("ki" . "き")
    ("ku" . "く")
    ("ke" . "け")
    ("ko" . "こ")
    
    ("sa" . "さ")
    ("si" . "し")
    ("shi" . "し")
    ("su" . "す")
    ("se" . "せ")
    ("so" . "そ")
    
    ("ta" . "た")
    ("ti" . "ち")
    ("chi" . "ち")
    ("tu" . "つ")
    ("tsu" . "つ")
    ("te" . "て")
    ("to" . "と")

    ("na" . "な")
    ("ni" . "に")
    ("nu" . "ぬ")
    ("ne" . "ね")
    ("no" . "の")

    ("ha" . "は")
    ("hi" . "ひ")
    ("hu" . "ふ")
    ("fu" . "ふ")
    ("he" . "へ")
    ("ho" . "ほ")

    ("ma" . "ま")
    ("mi" . "み")
    ("mu" . "む")
    ("me" . "め")
    ("mo" . "も")

    ("ya" . "や")
    ("yu" . "ゆ")
    ("yo" . "よ")

    ("wa" . "わ")
    ("wi" . "ゐ")
    ("wo" . "を")
    ("we" . "ゑ")
    ("nn" . "ん")
    ;; 濁音・半濁音
    ("ga" . "が")
    ("gi" . "ぎ")
    ("gu" . "ぐ")
    ("ge" . "げ")
    ("go" . "ご")

    ("za" . "ざ")
    ("zi" . "じ")
    ("ji" . "じ")
    ("zu" . "ず")
    ("ze" . "ぜ")
    ("zo" . "ぞ")

    ("da" . "だ")
    ("di" . "ぢ")
    ;; jiが両方に利用するのは、ローマ字変換では区別できないので、「ぢ」については定義しない
    ("du" . "づ")
    ("de" . "で")
    ("do" . "ど")

    ("ba" . "ば")
    ("bi" . "び")
    ("bu" . "ぶ")
    ("be" . "べ")
    ("bo" . "ぼ")

    ("pa" . "ぱ")
    ("pi" . "ぴ")
    ("pu" . "ぷ")
    ("pe" . "ぺ")
    ("po" . "ぽ")

    ;; 拗音
    ("kya" . "きゃ")
    ("kyu" . "きゅ")
    ("kyo" . "きょ")

    ("sha" . "しゃ")
    ("shu" . "しゅ")
    ("sho" . "しょ")

    ("cha" . "ちゃ")
    ("chu" . "ちゅ")
    ("cho" . "ちょ")

    ("nya" . "にゃ")
    ("nyu" . "にゅ")
    ("nyo" . "にょ")

    ("hya" . "ひゃ")
    ("hyu" . "ひゅ")
    ("hyo" . "ひょ")

    ("mya" . "みゃ")
    ("myu" . "みゅ")
    ("myo" . "みょ")

    ("rya" . "りゃ")
    ("ryu" . "りゅ")
    ("ryo" . "りょ")

    ("gya" . "ぎゃ")
    ("gyu" . "ぎゅ")
    ("gyo" . "ぎょ")

    ("ja" . "じゃ")
    ("ju" . "じゅ")
    ("jo" . "じょ")

    ("bya" . "びゃ")
    ("byu" . "びゅ")
    ("byo" . "びょ")

    ("pya" . "ぴゃ")
    ("pyu" . "ぴゅ")
    ("pyo" . "ぴょ")

    ;; 小書き
    ("xa" . "ぁ")
    ("xi" . "ぃ")
    ("xu" . "ぅ")
    ("xe" . "ぇ")
    ("xo" . "ぉ")
    
    ("xya" . "ゃ")
    ("xyu" . "ゅ")
    ("xyo" . "ょ")
    ("xtu" . "っ")
    ("xtsu" . "っ")
    ("xwa" . "ゎ")
    )
  "chokanで利用するローマ字変換表。但し、歴史的事情であったり入力負荷が高いような綴りについては、
広く利用されている形式も利用できるようにしている。
"
  )

(provide ''chokan-roman-table)
