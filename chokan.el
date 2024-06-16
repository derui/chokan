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
(require 'text-property-search)

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

(defvar chokan-conversion-functions
  nil
  "変換起動した文字列から、実際に候補を取得する関数のマッピング。

マッピングのキーとしては、次が利用可能である。

- `normal' : 通常の変換を行う場合の関数
- `tankan' : 単漢字変換を行う場合
- `proper' : 固有名詞優先変換を行う場合
- `update-frequency' : かな漢字変換の頻度を更新する
- `register-word' : 単語を登録する
- `alphabet' : アルファベット変換を行う

関数は、`normal', `tankan' `proper' は、 引数として変換対象となる文字列と、下線部の直前にあったcontextを受け取る。contextは、 (<type symbol> string) の形式で渡される。
contextが存在しない場合はnilを渡す。
'type symbol'は、`foreign'か`numeric'のいずれかである。

実行した結果として、以下の形式で候補のリストを返す。
`(:id session-id :candidates ((id . candidate)))'

`update-frequency'は、その変換におけるsession idとcandidate idが渡される。

`register-word' は、選択された漢字、その読みと品詞が渡される。
  品詞が指定されていない場合は `GUESS' のシンボルが渡される。

`alphabet'は、引数として変換対象となる文字列のみが渡される
実行した結果として、以下の形式で候補のリストを返す。
`(:candidates ((id . candidate)))'
")

;; buffer-local variable

(defvar chokan--internal-mode 'hiragana
  "chokanの現時点で入力しているモード。モードとしては以下が存在する。この変数はバッファローカルである。

chokanが起動された時点では、自動的に `hiragana' に設定される。

- `ascii' : アルファベットをそのまま入力する
- `hiragana' : ひらがなを入力する。変換を起動することができる
- `katakana' : カタカナを入力する。変換を起動することはできない
")

(defvar chokan--default-cursor-type nil
  "chokanが終了したときに戻すためのcursorの形状。この変数はバッファローカルである")

(defvar chokan--sticky nil
  "次に入力するキーを下線部が対応するものにする。対象のキーはalphabetのみである")

(defvar chokan--conversion-candidates nil
  "変換候補のリスト。変換起動が行われるたびに初期化される。

candidateは、それぞれ `(:id id :candidate-id candidate-id :candidate value)'
 というplistで保持される。idは、候補の識別子であり、candidateは、候補の文字列である。
")
(defvar chokan--conversion-candidate-pos 0
  "現在選択している候補の位置を 0オリジンで保持する。")

(defvar chokan--target-character-regexp
  "[a-zA-Z0-9あ-ん]+"
  "変換対象とする文字を検索するための正規表現")

(defvar chokan--numeral-context-regexp
  "[0-9０-９]+"
  "数字のcontextとして利用する文字列の正規表現")

(defvar chokan--foreign-word-context-regexp
  "[ア-ンー]+"
  "外来語のcontextとして利用する文字列の正規表現")


(defvar chokan--candidate-overlay nil
  "候補を表現するためのoverlay")
(defvar chokan--conversion-overlay nil
  "変換起動部分を表現するためのoverlay")

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

(defconst chokan--roman-table
  '(
    ("a" . "あ")
    ("i" . "い")
    ("u" . "う")
    ("e" . "え")
    ("o" . "お")
    ("A" . "あ")
    ("I" . "い")
    ("U" . "う")
    ("E" . "え")
    ("O" . "お")
    
    ("ka" . "か")
    ("ki" . "き")
    ("ku" . "く")
    ("ke" . "け")
    ("ko" . "こ")
    ("Ka" . "か")
    ("Ki" . "き")
    ("Ku" . "く")
    ("Ke" . "け")
    ("Ko" . "こ")
    
    ("sa" . "さ")
    ("si" . "し")
    ("shi" . "し")
    ("su" . "す")
    ("se" . "せ")
    ("so" . "そ")
    ("Sa" . "さ")
    ("Si" . "し")
    ("Shi" . "し")
    ("Su" . "す")
    ("Se" . "せ")
    ("So" . "そ")
    
    ("ta" . "た")
    ("ti" . "ち")
    ("chi" . "ち")
    ("tu" . "つ")
    ("tsu" . "つ")
    ("te" . "て")
    ("to" . "と")
    ("Ta" . "た")
    ("Ti" . "ち")
    ("Chi" . "ち")
    ("Tu" . "つ")
    ("Tsu" . "つ")
    ("Te" . "て")
    ("To" . "と")

    ("na" . "な")
    ("ni" . "に")
    ("nu" . "ぬ")
    ("ne" . "ね")
    ("no" . "の")
    ("Na" . "な")
    ("Ni" . "に")
    ("Nu" . "ぬ")
    ("Ne" . "ね")
    ("No" . "の")

    ("ha" . "は")
    ("hi" . "ひ")
    ("hu" . "ふ")
    ("fu" . "ふ")
    ("he" . "へ")
    ("ho" . "ほ")
    ("Ha" . "は")
    ("Hi" . "ひ")
    ("Hu" . "ふ")
    ("Fu" . "ふ")
    ("He" . "へ")
    ("Ho" . "ほ")
    
    ("ma" . "ま")
    ("mi" . "み")
    ("mu" . "む")
    ("me" . "め")
    ("mo" . "も")
    ("Ma" . "ま")
    ("Mi" . "み")
    ("Mu" . "む")
    ("Me" . "め")
    ("Mo" . "も")

    ("ra" . "ら")
    ("ri" . "り")
    ("ru" . "る")
    ("re" . "れ")
    ("ro" . "ろ")
    ("Ra" . "ら")
    ("Ri" . "り")
    ("Ru" . "る")
    ("Re" . "れ")
    ("Ro" . "ろ")

    ("ya" . "や")
    ("yu" . "ゆ")
    ("yo" . "よ")
    ("Ya" . "や")
    ("Yu" . "ゆ")
    ("Yo" . "よ")

    ("wa" . "わ")
    ("wi" . "ゐ")
    ("wo" . "を")
    ("we" . "ゑ")
    ("nn" . "ん")
    ("Wa" . "わ")
    ("Wi" . "ゐ")
    ("Wo" . "を")
    ("We" . "ゑ")
    ("Nn" . "ん")

    ;; 濁音・半濁音
    ("ga" . "が")
    ("gi" . "ぎ")
    ("gu" . "ぐ")
    ("ge" . "げ")
    ("go" . "ご")
    ("Ga" . "が")
    ("Gi" . "ぎ")
    ("Gu" . "ぐ")
    ("Ge" . "げ")
    ("Go" . "ご")

    ("za" . "ざ")
    ("zi" . "じ")
    ("ji" . "じ")
    ("zu" . "ず")
    ("ze" . "ぜ")
    ("zo" . "ぞ")
    ("Za" . "ざ")
    ("Zi" . "じ")
    ("Ji" . "じ")
    ("Zu" . "ず")
    ("Ze" . "ぜ")
    ("Zo" . "ぞ")

    ;; jiが両方に利用するのは、ローマ字変換では区別できないので、「ぢ」については定義しない
    ("da" . "だ")
    ("di" . "ぢ")
    ("du" . "づ")
    ("de" . "で")
    ("do" . "ど")
    ("Da" . "だ")
    ("Di" . "ぢ")
    ("Du" . "づ")
    ("De" . "で")
    ("Do" . "ど")

    ("ba" . "ば")
    ("bi" . "び")
    ("bu" . "ぶ")
    ("be" . "べ")
    ("bo" . "ぼ")
    ("Ba" . "ば")
    ("Bi" . "び")
    ("Bu" . "ぶ")
    ("Be" . "べ")
    ("Bo" . "ぼ")

    ("pa" . "ぱ")
    ("pi" . "ぴ")
    ("pu" . "ぷ")
    ("pe" . "ぺ")
    ("po" . "ぽ")
    ("Pa" . "ぱ")
    ("Pi" . "ぴ")
    ("Pu" . "ぷ")
    ("Pe" . "ぺ")
    ("Po" . "ぽ")

    ;; 拗音
    ("kya" . "きゃ")
    ("kyu" . "きゅ")
    ("kyo" . "きょ")
    ("Kya" . "きゃ")
    ("Kyu" . "きゅ")
    ("Kyo" . "きょ")

    ("sha" . "しゃ")
    ("shu" . "しゅ")
    ("sho" . "しょ")
    ("sya" . "しゃ")
    ("syu" . "しゅ")
    ("syo" . "しょ")
    ("Sha" . "しゃ")
    ("Shu" . "しゅ")
    ("Sho" . "しょ")
    ("Sya" . "しゃ")
    ("Syu" . "しゅ")
    ("Syo" . "しょ")

    ("cha" . "ちゃ")
    ("chu" . "ちゅ")
    ("cho" . "ちょ")
    ("Cha" . "ちゃ")
    ("Chu" . "ちゅ")
    ("Cho" . "ちょ")
    ("tya" . "ちゃ")
    ("tyu" . "ちゅ")
    ("tyo" . "ちょ")
    ("Tya" . "ちゃ")
    ("Tyu" . "ちゅ")
    ("Tyo" . "ちょ")

    ("nya" . "にゃ")
    ("nyu" . "にゅ")
    ("nyo" . "にょ")
    ("Nya" . "にゃ")
    ("Nyu" . "にゅ")
    ("Nyo" . "にょ")

    ("hya" . "ひゃ")
    ("hyu" . "ひゅ")
    ("hyo" . "ひょ")
    ("Hya" . "ひゃ")
    ("Hyu" . "ひゅ")
    ("Hyo" . "ひょ")

    ("mya" . "みゃ")
    ("myu" . "みゅ")
    ("myo" . "みょ")
    ("Mya" . "みゃ")
    ("Myu" . "みゅ")
    ("Myo" . "みょ")

    ("rya" . "りゃ")
    ("ryu" . "りゅ")
    ("ryo" . "りょ")
    ("Rya" . "りゃ")
    ("Ryu" . "りゅ")
    ("Ryo" . "りょ")

    ("gya" . "ぎゃ")
    ("gyu" . "ぎゅ")
    ("gyo" . "ぎょ")
    ("Gya" . "ぎゃ")
    ("Gyu" . "ぎゅ")
    ("Gyo" . "ぎょ")

    ("ja" . "じゃ")
    ("ju" . "じゅ")
    ("jo" . "じょ")
    ("Ja" . "じゃ")
    ("Ju" . "じゅ")
    ("Jo" . "じょ")
    ("zya" . "じゃ")
    ("zyu" . "じゅ")
    ("zyo" . "じょ")
    ("Zya" . "じゃ")
    ("Zyu" . "じゅ")
    ("Zyo" . "じょ")

    ("bya" . "びゃ")
    ("byu" . "びゅ")
    ("byo" . "びょ")
    ("Bya" . "びゃ")
    ("Byu" . "びゅ")
    ("Byo" . "びょ")

    ("pya" . "ぴゃ")
    ("pyu" . "ぴゅ")
    ("pyo" . "ぴょ")
    ("Pya" . "ぴゃ")
    ("Pyu" . "ぴゅ")
    ("Pyo" . "ぴょ")

    ;; 外来語
    ("fa" . "ふぁ")
    ("fi" . "ふぃ")
    ("fe" . "ふぇ")
    ("fo" . "ふぉ")
    ("Fa" . "ふぁ")
    ("Fi" . "ふぃ")
    ("Fe" . "ふぇ")
    ("Fo" . "ふぉ")

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

先頭が大文字であるものが定義されているのは、下線部の起動時と挙動を一致させるためである。
"
  )

(defvar chokan--katakana-table
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

(defconst chokan--symbol-table
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
    ("$" . "＄")
    ("%" . "％")
    ("&" . "＆")
    ("+" . "＋")
    ("=" . "＝")
    ("^" . "＾")
    ("_" . "＿")
    ([return] . "\n")
    ;; いくつかの記号は、変換起動などで利用するために一旦定義しない
    )
  "記号類を日本語における記号に変換するテーブル")

;;; roman internal functions

(defun chokan--roman-sokuon-p (input)
  "inputが促音を含むかどうかを判定する。

ここでの促音は、 `tt' のように子音を重ねたもののみを判定する。
"
  (let ((consonants '(?t ?b ?j ?f ?h ?s ?w ?r ?y ?p ?k ?g ?z ?c ?v)))
    (and (>= (length input) 2)
         (equal (aref input 0) (aref input 1))
         (member (aref input 0) consonants)
         (member (aref input 1) consonants))))

(defun chokan--roman-to-hiragana (input) 
  "ローマ字をひらがなに変換する。

組み合わせ上変換できない文字は、順次スキップされ、最終的にはすべての文字が変換される。
"
  (let* ((ret "")
         (input input)
         (max-table-key-size (seq-reduce (lambda (acc it)
                                           (let ((l (length (car it))))
                                             (max acc l)))
                                         chokan--roman-table
                                         0)))
    (while (not (seq-empty-p input))
      (while (chokan--roman-sokuon-p input)
        (setq ret (seq-concatenate 'string ret "っ"))
        (setq input (substring input 1)))
      ;; 1-(roman-tableで最大)文字で順次探す。
      (let* ((found (cl-dotimes (len max-table-key-size)
                      (let* ((subst (substring input 0 (min (seq-length input) len))))
                        (pcase (assoc subst chokan--roman-table)
                          ;; 見つかった場合はそのまま返す
                          (`(,_ . ,ret)
                           (setq input (substring input len))
                           (cl-return ret)))))))
        (if found
            (progn
              (setq ret (seq-concatenate 'string ret found)))
          (setq ret (seq-concatenate 'string ret (substring input 0 1)))
          (setq input (substring input 1)))))
    ret))

(defun chokan--roman-hira-to-kata (hira)
  "ひらがなをカタカナに変換する。変換できない文字はそのままで返す"
  (let ((result '()))
    (seq-doseq (c hira)
      (if-let ((kata (assoc (string c) chokan--katakana-table)))
          (push (cdr kata) result)
        (push (string c) result)))
    (string-join (seq-reverse result) "")))

(defun chokan--symbol-convert-to-ja (symbol)
  "SYMBOLを日本語における記号に変換する。変換できない場合はnilを返す"
  (let* ((converted (assoc symbol chokan--symbol-table)))
    (if converted
        (cdr converted)
      nil)))

;;; chokan-conversion internal function
(defun chokan--same-type-string-backward (regexp)
  "同一のregexpにマッチする連続した文字列を返す"
  (save-excursion
    (let* ((current (point))
           (char (buffer-substring-no-properties (max 1 (1- current)) current))
           ret)
      (while (string-match-p regexp char)
        (backward-char)
        (setq ret (concat (or ret "") char))
        (setq char (buffer-substring-no-properties (1- (point)) (point))))
      (seq-reverse ret))))

(defun chokan--get-previous-context (start)
  "`START'のpointより前のcontextを取得する。contextは consの形式で返却され、carにはcontextの種別、cdrにはcontextの文字列が格納される。

contextは、以下のいずれかである。

- 通常の文字列 :: type = `normal'
- 連続した数字 :: type = `numeral'
- 連続したアルファベット :: type = `foreign-word'
"
  (save-excursion
    (goto-char start)
    (let* ((context-fw (chokan--same-type-string-backward chokan--foreign-word-context-regexp))
           (context-number (chokan--same-type-string-backward chokan--numeral-context-regexp)))
      (pcase (list context-fw context-number)
        (`(,(pred (not null)) ,_) (cons 'foreign-word context-fw))
        (`(,_ ,(pred (not null))) (cons 'numeral context-number))
        (_ '(normal))))))


(defun chokan--get-conversion-region ()
  "現在の下線部があれば、その周辺で変換対象のregionと種別、さらにcontextを取得する。
下線部が存在しない場合は `NIL' を返す。
"
  (let* ((current (point)))
    (save-excursion
      (when-let* ((props (text-property-search-backward 'chokan-conversion-start t t))
                  (start (prop-match-beginning props))
                  ;; ひらがな・アルファベット・数字以外、またはカーソル位置を対象にする
                  (end (progn
                         (goto-char start)
                         (re-search-forward chokan--target-character-regexp current t)))
                  (detail (get-text-property start 'chokan-conversion-detail))
                  (context (chokan--get-previous-context start)))
        (list start end detail context)))))

(defun chokan--conversion-launch (callback override-conversion)
  "現在のポイントから変換起動を試みる。変換起動が出来ない場合は、何も行わない。

変換起動が出来た場合は、'CALLBACK'に対象のregionの開始位置と終了位置、最初の変換候補を渡して実行する。変換候補がない場合は、変換候補をnilが設定される。
"

  (when-let* ((region (chokan--get-conversion-region))
              (start (car region))
              (end (cadr region))
              (type (or override-conversion (caddr region)))
              (context (cadddr region))
              (str (buffer-substring-no-properties start end)))
    (let ((func (assoc type chokan-conversion-functions)))
      ;; 下線部は存在していることがここで確定しているので、overlayを削除する
      (delete-overlay chokan--conversion-overlay)
      (if func
          (progn
            (setq chokan--conversion-candidate-pos 0)
            (setq chokan--conversion-candidates (funcall (cdr func) str context))

            (let* ((candidate (and chokan--conversion-candidates
                                   (car (plist-get chokan--conversion-candidates :candidates)))))
              (funcall callback start end candidate)))
        (funcall callback start end nil)))))


;;; chokan-core internal functions

(defsubst chokan--ascii-p ()
  "現在chokanがascii modeであるかどうかを返す"
  (and chokan-mode (eq chokan--internal-mode 'ascii)))

(defsubst chokan--ja-p ()
  "現在chokanが日本語入力モードであるかどうかを返す"
  (and chokan-mode (not (eq chokan--internal-mode 'ascii))))

(defsubst chokan--ja-katakana-p ()
  "現在chokanがカタカナ入力モードであるかどうかを返す"
  (and (chokan--ja-p) (eq chokan--internal-mode 'katakana)))

(defsubst chokan--sticky-p ()
  "sticky 状態かどうかを返す"
  chokan--sticky)

(defsubst chokan--sticky-activate ()
  "sticky 状態にする"
  (setq chokan--sticky t))

(defsubst chokan--sticky-deactivate ()
  "sticky 状態を解除する"
  (setq chokan--sticky nil))

(defun chokan--post-command ()
  "chokanに関する入力を判定するための処理。

直前に行われたコマンドと、現時点のmode、そしてそのコマンドによって変更された範囲を元に、chokanに関する入力を判定する。
"
  (when chokan-mode
    (let ((cmd this-command))
      (condition-case-unless-debug nil
          (cond
           ((or (eq cmd 'chokan-insert-normal-alphabet)
                (eq cmd 'chokan-insert-conversion-start-key)
                (eq cmd 'chokan-insert-symbol-key)
                (eq cmd 'chokan-insert-tankan-start-key)
                (eq cmd 'chokan-insert-alphabet-start-key)
                (eq cmd 'chokan-force-finalize)
                (eq cmd 'chokan-toggle-katakana)
                (eq cmd 'chokan-next-candidate)
                (eq cmd 'chokan-previous-candidate)
                (eq cmd 'chokan-through-key))
            nil)
           (t
            ;; self-insert-commandではない変更が行われた場合は、確定できていない文字を削除する
            ;; 変換中の文字は、あくまで途中の文字でしか無いので、確定しない限りは、self-insert以外では削除する
            (let ((prop (text-property-search-backward 'chokan-alphabet t t)))
              (if prop
                  (delete-region (prop-match-beginning prop) (prop-match-end prop))
                nil))

            ;; 反転部が存在する場合に、カーソルの位置に応じて反転部の確定を実行する
            (when-let* ((region (chokan--get-inverse-region))
                        (start (car region))
                        (end (cdr region)))
              (when (<= start (point) end)
                (chokan--finalize-inverse-if-possible t region)))))
        (error nil)))))

(defun chokan--roman-to-kana (alphabet)
  "現在のmodeに従って `alphabet' をかなまたはカナに変換する。"
  (let* ((kana (chokan--roman-to-hiragana alphabet)))
    (if (not (chokan--ja-katakana-p))
        kana
      (chokan--roman-hira-to-kata kana))))

(defun chokan--get-roman-content ()
  "現在のpointを含むローマ字の未確定文字列とregionを取得する。返却する文字列にはtext propertyが含まれる。
未確定領域がない場合は `NIL' を返す。

返却する形式は `((START . END) . CONTENT)' というconsである。

仕様上、未確定領域は現在のポイントから前にしか存在しない。"
  )

(defun chokan--get-inverse-region ()
  "現在のpointを含む反転部の領域を取得する。
反転部がない場合は `NIL' を返す。

仕様上、未確定領域は現在のポイントから前にしか存在しない。"
  (save-excursion
    (if-let* ((overlay chokan--candidate-overlay)
              (start (overlay-start overlay))
              (end (overlay-end overlay)))
        (cons start end)
      nil)))

(defun chokan--convert-roman-to-kana-if-possible (current-point)
  "chokanのローマ字変換において、確定できていない文字がある場合に、それを変換する。 `CURRENT-POINT' は、
実行時点で入力された文字の位置である。
"
  ;; 変換する領域は、現時点を含んで同じpropertyを持つ領域全体である
  
  (when-let* ((prop (text-property-search-backward 'chokan-alphabet t t))
              (begin (prop-match-beginning prop))
              (end (prop-match-end prop))
              (content (buffer-substring begin end))
              (ret (chokan--roman-to-kana content)))
    ;; 下線部を含む場合は、先頭に限り、text propertyもcopyする
    (when (< 0 (seq-length ret))
      (if (get-text-property begin 'chokan-conversion-start)
          (let* ((conversion-start (get-text-property begin 'chokan-conversion-start))
                 (alphabet (get-text-property 0 'chokan-alphabet ret))
                 (conversion-detail (get-text-property begin 'chokan-conversion-detail)))
            (put-text-property 0 1 'chokan-conversion-start conversion-start ret)
            (put-text-property 0 1 'chokan-conversion-detail conversion-detail ret)

            ;; faceを再設定する
            (let ((face (if font-lock-mode 'font-lock-face 'face)))
              (remove-text-properties 0 1 `(,face nil) ret)
              (if alphabet
                  (put-text-property 0 1 face 'chokan-conversion-start-roman ret)
                (put-text-property 0 1 face 'chokan-conversion-start ret)))
            
            (delete-region begin end)
            (goto-char begin)
            (insert ret))
        (delete-region begin end)
        (goto-char begin)
        (insert ret)))))

(defun chokan--insert-with-props (str char-props)
  "指定した種別に対応するtext propertyを付与して文字をinsertする。

ここではあくまでtext propertyの設定のみであり、faceは設定しない。

'char-props' は以下のassocである。いずれも `nil' または固有の値または `t' である。

`((roman . nil)
  (conversion-start . nil)
  (inverse . nil))
'

- 'roman' :: ローマ字変換の対象
- 'conversion-start' :: 変換の起点。下線部表記になる。 'normal' `'tankan', `alphabet' のいずれかを設定する
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
      (`(t ,(pred (not (null))) ,_)
       (put-text-property start end 'chokan-alphabet t)
       (put-text-property start end 'chokan-conversion-start t)
       (put-text-property start end 'chokan-conversion-detail conversion-start))
      ;; 下線部
      (`(nil ,(pred (not (null))) ,_)
       (put-text-property start end 'chokan-conversion-start t)
       (put-text-property start end 'chokan-conversion-detail conversion-start))
      ;; 反転部
      (`(nil nil t)
       (put-text-property start end 'chokan-inverse t)))))

(defun chokan--self-insert (key char-type char-props)
  "chokanでキーに対応する文字を入力するための関数。

`key' は入力されたキー、 `char-props' は入力した文字に対するpropertyをいれたassociation listである。
`char-type' は、 `alphabet' `symbols' のいずれかのsymbolである。 `char-type' が `alphabet' の場合のみ、ローマ字かな変換を行う。

"
  (cond
   ((eq char-type 'alphabet)
    (chokan--insert-with-props key char-props)
    (put-text-property (1- (point)) (point) (if font-lock-mode 'font-lock-face 'face) 'chokan-kana-roman)
    (chokan--convert-roman-to-kana-if-possible (point)))
   ((eq char-type 'symbols)
    (let ((key (or (chokan--symbol-convert-to-ja key) key)))
      (chokan--insert-with-props key char-props)))))

(defun chokan--insert-candidate (region candidate)
  "指定されたregionに対して `CANDIDATE'を挿入し、反転部とする。

ここではoverlayの構築が行われる。"
  (let* ((start (car region))
         (end (cdr region))
         (overlay (or chokan--candidate-overlay
                      (setq chokan--candidate-overlay (make-overlay start end)))))
    (save-excursion
      (move-overlay chokan--candidate-overlay start end (current-buffer))
      (put-text-property start end 'chokan-inverse t)
      (overlay-put overlay 'face `(t :inverse-video t))
      (overlay-put overlay 'display candidate))))

(defun chokan--conversion-callback (start end candidate)
  "変換起動のコールバック関数。挿入する候補が 'CANDIDATE'である。'START', 'END' は対応する範囲をあらわす。"
  (let* ((candidate (or (cdr candidate) (buffer-substring-no-properties start end))))
    (chokan--insert-candidate (cons start end) candidate)))

(defun chokan--launch-conversion-if-possible (convert-launchable &optional override-conversion)
  "必要なら変換処理を起動し、反転部を作成する。

`CONVERT-LAUNCHABLE' が `non-nil' の場合、変換処理を起動する。`OVERRIDE-CONVERSION' が `non-nil' の場合、指定された変換処理で上書きする。
"
  (when convert-launchable
    (chokan--conversion-launch #'chokan--conversion-callback override-conversion)))

(defun chokan--insert-conversion-start-if-possible (conversion-startable)
  "`CONVERSION-STARTABLE' が `non-nil' の場合、 `KEY' を下線部として挿入する。

`ALPHABET' が `non-nil' である場合、対象のkeyは未確定のローマ字であることを示す。
"

  (when conversion-startable
    (let* ((face 'chokan-conversion-start-roman)
           (current (point)))
      (move-overlay chokan--conversion-overlay current current (current-buffer)))))

(defun chokan--finalize-inverse-if-possible (finalizable &optional inverted-region)
  "反転部を確定できる場合は確定する。

'INVERTED-REGION' にconsが渡されている場合はそれが利用される"

  (when-let* (finalizable
              (region (or (and (consp inverted-region) inverted-region)
                          (chokan--get-inverse-region)))
              (session-id (plist-get chokan--conversion-candidates :id))
              (candidate (nth chokan--conversion-candidate-pos (plist-get chokan--conversion-candidates :candidates))))
    (remove-text-properties (car region) (cdr region) '(chokan-inverse t face nil))
    (delete-overlay chokan--candidate-overlay)

    (let* ((current (point))
           (point-contains-region (<= (car region) current (cdr region))))
      (save-excursion
        (delete-region (car region) (cdr region))
        (goto-char (car region))
        (insert (cdr candidate)))
      ;; region内部にpointが含まれている場合、ここでずらさないと、変換起動した直後にまた反転部が挿入されてしまう
      (when point-contains-region
        (forward-char (length (cdr candidate)))))

    (when-let* ((func (assoc 'update-frequency chokan-conversion-functions)))
      (funcall (cdr func) session-id (car candidate)))))

(defun chokan--insert (convert-launchable conversion-detail char-type)
  "chokanにおける各文字を入力するためのエントリーポイントとなる関数。特殊な記号による入力はこの関数以外で実行すること。

`CONVERT-LAUNCHABLE' が `non-nil' の場合、起動したコマンドのキーが変換起動可能であることを表す。
`CONVERSION-DETAIL' が `non-nil' の場合、入力した文字が下線部になる。指定したsymbolに対応する特殊変換がトグルされる
`CHAR-TYPE' は、 `alphabet' `symbols' のいずれかのsymbolである。

この関数では以下を実行する。

1. 現在のmodeがカタカナであるかどうか
2. 反転部の確定（ `convert-launchable' が non-nil である場合）
3. 下線部のかな漢字変換起動（ `convert-launchable' が non-nil である場合）
4. 下線部の設定（ `conversion-detail' が non-nil である場合）
5. 自己挿入し、必要ならローマ字かな変換を行う
"
  (let* ((key (this-command-keys)))
    (when (and conversion-detail convert-launchable)
      (chokan--hiragana-enable))
    (chokan--finalize-inverse-if-possible convert-launchable)
    (chokan--launch-conversion-if-possible convert-launchable)
    (chokan--insert-conversion-start-if-possible conversion-detail)
    (chokan--self-insert key char-type `((roman . ,(eq char-type 'alphabet))
                                         (conversion-start . ,conversion-detail)
                                         (inverse . nil)))
    ;; stickyはあらゆる入力で解除される
    (chokan--sticky-deactivate)))

(defun chokan--katakana-enable ()
  "カタカナモードに遷移する"
  (setq chokan--internal-mode 'katakana)
  (setq cursor-type chokan-katakana-cursor-type))

(defun chokan--hiragana-enable ()
  "ひらがなモードに遷移する"
  (setq chokan--internal-mode 'hiragana)
  (setq cursor-type chokan-ja-cursor-type))

;; command definition
(defun chokan-ascii ()
  "chokanをasciiモードに変更する。

asciiモードに遷移すると、強制的に変換起動される"
  (interactive)
  (chokan--finalize-inverse-if-possible t)
  (chokan--launch-conversion-if-possible t)
  (chokan-ja-mode -1)
  (chokan-ascii-mode +1))

(defun chokan-ja ()
  "chokanを日本語入力モードに変更する"
  (interactive)
  (chokan-ascii-mode -1)
  (chokan-ja-mode +1)
  (chokan--hiragana-enable))

(defun chokan-toggle-katakana ()
  "chokanの内部モードをカタカナ入力に変更する。

カタカナモードに入ったタイミングで、変換起動される。
"
  (interactive)
  (chokan--finalize-inverse-if-possible t)
  (chokan--launch-conversion-if-possible t)
  
  (when (chokan--ja-p)
    (if (not (chokan--ja-katakana-p))
        (chokan--katakana-enable)
      (chokan--hiragana-enable))))

(defun chokan-insert-normal-alphabet ()
  "変換起動をしないで文字を入力する"
  (interactive)
  (let ((conversion-start (and (chokan--sticky-p) 'normal)))
    (chokan--insert (not (null conversion-start)) conversion-start 'alphabet)))

(defun chokan-insert-conversion-start-key ()
  "変換起動をして文字を入力する"
  (interactive)
  
  ;; 下線部を追加する場合は、カタカナモードからは強制的に離脱する
  (when  (chokan--ja-katakana-p)
    (chokan--hiragana-enable))
  (chokan--insert t 'normal 'alphabet))

(defun chokan-insert-symbol-key ()
  "各種記号を入力する。記号は原則として変換起動するが、自分自身は下線部ではない。"
  (interactive)
  (chokan--insert t nil 'symbols))

(defun chokan-through-key ()
  "元々割り当てられているcommandを実行するが、変換起動は実行する"
  (interactive)
  (let* ((chokan-ja-mode nil)
         (old-func (key-binding (this-command-keys))))
    
    (chokan--finalize-inverse-if-possible t)
    (chokan--launch-conversion-if-possible t)
    (when (not (eq old-func 'chokan-through-key))
      (call-interactively old-func))))

(defun chokan-insert-tankan-start-key ()
  "単漢字変換を起動して文字を入力する"
  (interactive)
  (chokan--insert t 'tankan 'symbols))

(defun chokan-insert-alphabet-start-key ()
  "単漢字変換を起動して文字を入力する"
  (interactive)
  (let* ((chokan-ja-mode nil)
         (old-func (key-binding (this-command-keys))))
    (chokan--finalize-inverse-if-possible t)
    (chokan--launch-conversion-if-possible t 'alphabet)
    (when (not (eq old-func 'chokan-insert-alphabet-start-key))
      (call-interactively old-func))))

(defun chokan-insert-proper-start-key ()
  "固有名詞を優先する変換を起動して文字を入力する"
  (interactive)
  (chokan--insert t 'proper 'symbols))

(defun chokan-next-candidate ()
  "現在の反転部に対する次の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region))
              (candidates (plist-get chokan--conversion-candidates :candidates)))
        (when-let* ((candidate (when-let* ((next (nth (1+ chokan--conversion-candidate-pos) candidates)))
                                 (setq chokan--conversion-candidate-pos (1+ chokan--conversion-candidate-pos))
                                 next)))
          (chokan--insert-candidate region (cdr candidate)))
      (let* ((chokan-ja-mode nil)
             (chokan-ascii-mode nil)
             (chokan-mode nil)
             (old-func (key-binding current-key)))
        (when (not (eq old-func 'chokan-next-candidate))
          (call-interactively old-func))))))

(defun chokan-previous-candidate ()
  "現在の反転部に対する前の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region))
              (candidates (plist-get chokan--conversion-candidates :candidates)))
        (when-let ((candidate (if (zerop chokan--conversion-candidate-pos)
                                  nil
                                (when-let* ((prev (nth (1- chokan--conversion-candidate-pos) candidates)))
                                  (setq chokan--conversion-candidate-pos (1- chokan--conversion-candidate-pos))
                                  prev))))
          (chokan--insert-candidate region (cdr candidate)))
      (let* ((chokan-ja-mode nil)
             (chokan-ascii-mode nil)
             (chokan-mode nil)
             (old-func (key-binding current-key)))
        (when (not (eq old-func 'chokan-previous-candidate))
          (call-interactively old-func))))))

(defun chokan-force-finalize ()
  "強制的に反転部を確定させる"
  (interactive)
  (chokan--finalize-inverse-if-possible t)
  (chokan--launch-conversion-if-possible t)
  (chokan--hiragana-enable))

(defun chokan-sticky ()
  "次に入力するアルファベットを、大文字のアルファベットと同等にする"
  (interactive)
  (chokan--sticky-activate))

(defun chokan-register-word (s e)
  "指定した範囲を単語として登録する。

追加の引数が追加された場合は、品詞を追加で設定する。
"
  (interactive "r")
  (when-let* ((func (assoc 'register-word chokan-conversion-functions)))
    (let* ((word (buffer-substring-no-properties s e))
           (reading (read-string (format "[辞書登録] <%s> : " word))))
      (when (and (< 0 (seq-length word)) (< 0 (seq-length reading)))
        (funcall (cdr func) word reading 'guess)
        (deactivate-mark)))))

;; mode definition

;;;###autoload
(define-minor-mode chokan-ascii-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

This mode only handle to keymap for changing mode to `chokan-mode' and `chokan-ja-mode'.
"
  :keymap chokan-ascii-mode-map
  :after-hook (progn 
                (setq chokan--internal-mode 'ascii)
                (setq cursor-type chokan-ascii-cursor-type)))

;;;###autoload
(define-minor-mode chokan-ja-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

This mode only handle to keymap for changing mode to `chokan-mode' and `chokan-ascii-mode'.
"
  :keymap chokan-ja-mode-map
  :after-hook (chokan--hiragana-enable))

(defun chokan-mode--activate ()
  "chokan-modeが起動するときに実行する処理をまとめた関数"

  (setq-local chokan--default-cursor-type cursor-type)
  (setq-local chokan--sticky nil)
  (setq-local chokan--conversion-candidates nil)
  (setq-local chokan--conversion-candidate-pos 0)
  (setq-local chokan--conversion-overlay (make-overlay 1 1))
  (overlay-put chokan--conversion-overlay 'face 'chokan-conversion-start)
  ;; これをやっておかないと、余計なfaceが反映されてしまう
  (delete-overlay chokan--conversion-overlay)
  (chokan-ja-mode)
  )

;;;###autoload
(define-minor-mode chokan-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

`chokan' has some functions to input japanese in Emacs, one of `ASCII' mode
for input alphabet, and `JA' mode for input japanese text.

When called interactively, toggle `chokan-mode'.  With prefix ARG,
enable `chokan-mode' if ARG is positive, and disable it otherwise.
"
  :keymap chokan-mode-map
  :after-hook (progn
                (add-hook 'post-command-hook #'chokan--post-command nil t)
                (if chokan-mode
                    (chokan-mode--activate)
                  (setq cursor-type chokan--default-cursor-type)
                  (chokan-ja-mode -1)
                  (chokan-ascii-mode -1)))
  )

;; setup initial keymap
(define-key chokan-mode-map (kbd "C-n") #'chokan-next-candidate)
(define-key chokan-mode-map (kbd "C-p") #'chokan-previous-candidate)
(define-key chokan-ascii-mode-map (kbd "C-j") #'chokan-ja)
(define-key chokan-ja-mode-map (kbd "C-j") #'chokan-force-finalize)
(define-key chokan-ja-mode-map (kbd "C-l") #'chokan-ascii)
(define-key chokan-ja-mode-map (kbd "*") #'chokan-toggle-katakana)
(define-key chokan-ja-mode-map (kbd ";") #'chokan-sticky)
(define-key chokan-ja-mode-map (kbd "@") #'chokan-insert-tankan-start-key)
(define-key chokan-ja-mode-map (kbd "$") #'chokan-insert-proper-start-key)

(dolist (k '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-normal-alphabet))
(dolist (k '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-conversion-start-key))
(dolist (k '("-" "." "," "=" "+" "_" "|" "%" "&" "^" "~" "!" "?" "\"" "`" "(" ")" "[" "]" "{" "}" "<" ">" "/"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-symbol-key))

(define-key chokan-ja-mode-map (kbd "RET") #'chokan-through-key)
(define-key chokan-ja-mode-map (kbd "SPC") #'chokan-insert-alphabet-start-key)

;; register input method

;;;###autoload
(defun chokan-activate (&optional name)
  "chokanをleim経由で起動する"
  (setq deactivate-current-input-method-function #'chokan-deactivate)
  (chokan-mode +1)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook #'chokan-leim-exit-from-minibuffer)))

;;;###autoload
(defun chokan-deactivate ()
  "chokanをleim経由で終了する"
  (chokan-mode -1))

;;;###autoload
(defun chokan-leim-exit-from-minibuffer ()
  "chokanがminiubuffer内で起動していて、終了するときに呼び出される"
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook #'chokan-leim-exit-from-minibuffer)))

;;;###autoload
(register-input-method 'japanese-chokan "Japanese" #'chokan-activate "chokan" "Cho-tto Kan-zen")

(provide 'chokan)
