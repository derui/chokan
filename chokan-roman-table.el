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

    ("ra" . "ら")
    ("ri" . "り")
    ("ru" . "る")
    ("re" . "れ")
    ("ro" . "ろ")

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

    ;; 外来語
    ("fa" . "ふぁ")
    ("fi" . "ふぃ")
    ("fe" . "ふぇ")
    ("fo" . "ふぉ")

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

(defvar chokan-talbe--katakana-table
  '(
    ("あ" . "ア")
    ("い" . "イ")
    ("う" . "ウ")
    ("え" . "エ")
    ("お" . "オ")
    ;; 
    ("か" . "カ")
    ("き" . "キ")
    ("く" . "ク")
    ("け" . "ケ")
    ("こ" . "コ")
    ;; 
    ("さ" . "サ")
    ("し" . "シ")
    ("す" . "ス")
    ("せ" . "セ")
    ("そ" . "ソ")
    ;; 
    ("た" . "タ")
    ("ち" . "チ")
    ("つ" . "ツ")
    ("て" . "テ")
    ("と" . "ト")
    ;; 
    ("な" . "ナ")
    ("に" . "ニ")
    ("ぬ" . "ヌ")
    ("ね" . "ネ")
    ("の" . "ノ")
    ;; 
    ("は" . "ハ")
    ("ひ" . "ヒ")
    ("ふ" . "フ")
    ("へ" . "ヘ")
    ("ほ" . "ホ")
    ;; 
    ("ま" . "マ")
    ("み" . "ミ")
    ("む" . "ム")
    ("め" . "メ")
    ("も" . "モ")
    ;; 
    ("や" . "ヤ")
    ("ゆ" . "ユ")
    ("よ" . "ヨ")
    ;; 
    ("ら" . "ラ")
    ("り" . "リ")
    ("る" . "ル")
    ("れ" . "レ")
    ("ろ" . "ロ")
    ;; 
    ("わ" . "ワ")
    ("を" . "ヲ")
    ("ゐ" . "ヰ")
    ("ゑ" . "ヱ")
    ("を" . "ヲ")
    ("ん" . "ン")
    ;; 
    ("が" . "ガ")
    ("ぎ" . "ギ")
    ("ぐ" . "グ")
    ("げ" . "ゲ")
    ("ご" . "ゴ")
    ;; 
    ("ざ" . "ザ")
    ("じ" . "ジ")
    ("ず" . "ズ")
    ("ぜ" . "ゼ")
    ("ぞ" . "ゾ")
    ;; 
    ("だ" . "ダ")
    ("ぢ" . "ヂ")
    ("づ" . "ヅ")
    ("で" . "デ")
    ("ど" . "ド")
    ;;
    ("ば" . "バ")
    ("び" . "ビ")
    ("ぶ" . "ブ")
    ("べ" . "ベ")
    ("ぼ" . "ボ")
    ;;
    ("ぱ" . "パ")
    ("ぴ" . "ピ")
    ("ぷ" . "プ")
    ("ぺ" . "ペ")
    ("ぽ" . "ポ")
    ;;
    ("ゃ" . "ャ")
    ("ゅ" . "ュ")
    ("ょ" . "ョ")
    ("っ" . "ッ")
    ("ゎ" . "ヮ")
    ("ぁ" . "ァ")
    ("ぃ" . "ィ")
    ("ぅ" . "ゥ")
    ("ぇ" . "ェ")
    ("ぉ" . "ォ")
    )
  "ひらがなからカタカナへの変換テーブル")

(defun chokan-roman-table-roman-to-kana (input) 
  "ローマ字を仮名に変換する。

変換結果によって、以下のいずれかの結果を返す。

- '(not-found)' : 対応する候補が見つからない場合
- '(ambiguous . (list of candidates))' : 対応する候補が複数見つかり、確定できない場合
- '(found . (<kana> <rest of input>)' : 対応する候補が一つ見つかり、確定できる場合。
                                        inputを全部消費しない場合、残りのinputを返す
"
  (let ((result (seq-filter (lambda (v) (string-prefix-p input (car v)))
                            chokan--roman-table)))
    (cond
     ;; 全体の組み合わせで見つからない場合は促音の可能性を考慮しつつ、not foundを返す
     ((null result)
      (if (= 1 (length input))
          '(not-found)
        (let* ((first-char (substring input 0 1))
               (second-char (substring input 1 2))
               (rest (substring input 1))
               (rest-result (chokan-roman-table-roman-to-kana rest))
               )
          (if (string= first-char second-char)
              (cons 'found (cons "っ" (cond
                                       ((eq (car rest-result) 'found) (cdar rest-result))
                                       (t rest))))
            (cond
             ((eq (car rest-result) 'found) (cdar rest-result))
             (t '(not-found)))))))
     
     ((= 1 (length result))
      (let* ((kana (cdr (car result)))
             (rest (substring input (length (car (car result))))))
        (if (string= rest "")
            (cons 'found (cons kana ""))
          (const 'found (cons kana rest))))
      )
     (t `(ambiguous . ,(mapcar (lambda (v) (cdr v)) result)))
     )))

(defun chokan-roman-table-hira-to-kata (hira)
  "ひらがなをカタカナに変換する。変換できない場合は `nil' を返す"
  (let ((result (assoc hira chokan-talbe--katakana-table)))
    (if result
        (cdr result)
      nil)))

(provide 'chokan-roman-table)
