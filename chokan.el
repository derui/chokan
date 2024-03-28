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
  (list
   '(normal . chokan-websocket-get-candidates)
   '(tankan . chokan-websocket-get-tankan-candidates)
   )
  "変換起動した文字列から、実際に候補を取得する関数のマッピング。

マッピングのキーとしては、次が利用可能である。

- 'normal' : 通常の変換を行う場合の関数
- 'tankan' : 単漢字変換を行う場合

関数は、引数として変換対象となる文字列と、下線部の直前にあったcontextを受け取る。contextは、 (<type symbol> string) の形式で渡される。
contextが存在しない場合はnilを渡す。
'type symbol'は、'foreign'か'numeric'のいずれかである。

実行した結果として、候補のリストを返す。候補がない場合は'NIL'を返す。候補のリストは、文字列のリストである。
")

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

(defvar chokan--sticky nil
  "次に入力するキーを下線部が対応するものにする。対象のキーはalphabetのみである")

(defvar chokan--conversion-candidates nil
  "変換候補のリスト。変換起動が行われるたびに初期化される。

candidateは、それぞれ '(id . candidate)' というconsで保持される。idは、候補の識別子であり、candidateは、候補の文字列である。
")
(defvar chokan--conversion-candidate-pos 0
  "現在選択している候補の位置を 0オリジンで保持する。")

(defvar chokan--target-character-regexp
  "[a-zA-Z0-9あ-ん]+"
  "変換対象とする文字を検索するための正規表現")

(defvar chokan--numeral-context-regexp
  "[0-9０-９]"
  "数字のcontextとして利用する文字列の正規表現")

(defvar chokan--foreign-word-context-regexp
  "[a-zA-Z]"
  "外来語のcontextとして利用する文字列の正規表現")



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

ここでの促音は、 'tt' のように子音を重ねたもののみを判定する。
"
  (let ((consonants '(?t ?b ?j ?f ?h ?s ?w ?r ?y ?p ?k ?g ?z ?c ?v)))
    (and (>= (length input) 2)
         (equal (aref input 0) (aref input 1))
         (member (aref input 0) consonants)
         (member (aref input 1) consonants))))

(defun chokan--roman-to-hiragana (input) 
  "ローマ字をひらがなに変換する。
w
変換結果によって、以下のいずれかの結果を返す。

- 'nil' : 対応する候補が見つからない場合
- '\"result\"' : 対応するひらがな
"
  (let* ((sokuon "")
         (input (downcase input)))
    (while (chokan--roman-sokuon-p input)
      (setq sokuon (concat sokuon "っ"))
      (setq input (substring input 1)))
    (pcase (assoc input chokan--roman-table)
      ;; 全体の組み合わせで見つからない場合はnil
      (`() nil)
      ;; 見つかった場合はそのまま返す
      (`(,_ . ,ret)
       (concat sokuon ret)))))

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
           (char (buffer-substring-no-properties (1- current) current))
           ret)
      (while (string-match-p regexp char)
        (backward-char)
        (setq ret (concat (or ret "") char))
        (setq char (buffer-substring-no-properties (1- (point)) (point))))
      (seq-reverse ret))))

(defun chokan--get-previous-context ()
  "現在位置より前のcontextを取得する。contextは consの形式で返却され、carにはcontextの種別、cdrにはcontextの文字列が格納される。

contextは、以下のいずれかである。

- 通常の文字列 :: type = 'normal'
- 連続した数字 :: type = 'numeral'
- 連続したアルファベット :: type = 'foreign-word'
"
  (save-excursion
    (let* ((current (point))
           (test-char (buffer-substring-no-properties (1- current) current))
           (context-fw (chokan--same-type-string-backward chokan--foreign-word-context-regexp))
           (context-number (chokan--same-type-string-backward chokan--numeral-context-regexp)))
      (pcase (list context-fw context-number)
        (`(,(pred numberp) ,_) (cons 'foreign-word context-fw))
        (`(,_ ,(pred numberp)) (cons 'numeral context-number))
        (_ '(normal))))))

(defun chokan--get-conversion-region ()
  "現在の下線部があれば、その周辺で変換対象のregionと種別、さらにcontextを取得する。
下線部が存在しない場合は 'NIL' を返す。
"
  (let* ((current (point)))
    (save-excursion
      (when-let* ((prop-match (text-property-search-backward 'chokan-conversion-start t t nil))
                  (region (cons (prop-match-beginning prop-match) (prop-match-end prop-match)))
                  ;; ひらがな・アルファベット・数字以外、またはカーソル位置を対象にする
                  (end (re-search-forward chokan--target-character-regexp current t))
                  (detail (get-text-property (prop-match-beginning prop-match) 'chokan-conversion-detail))
                  (context (chokan--get-previous-context)))
        (list (car region) end detail context)))))

;;;###autoload
(defun chokan--conversion-launch (callback)
  "現在のポイントから変換起動を試みる。変換起動が出来ない場合は、何も行わない。

変換起動が出来た場合は、'CALLBACK'に対象のregionの開始位置と終了位置、最初の変換候補を渡して実行する。変換候補がない場合は、変換候補をnilが設定される。
"

  (when-let* ((region (chokan--get-conversion-region))
              (start (car region))
              (end (cadr region))
              (type (caddr region))
              (context (cadddr region))
              (str (buffer-substring-no-properties start end)))
    (let ((func (assoc type chokan-conversion-functions)))
      (if func
          (progn
            (setq chokan--conversion-candidate-pos 0)
            (setq chokan--conversion-candidates (funcall (cdr func) str context))

            (let* ((candidate (and chokan--conversion-candidates
                                   (car chokan--conversion-candidates))))
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
                (eq cmd 'chokan-force-finalize))
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
  "現在のmodeに従って `alphabet' をかなに変換する。"
  (let* ((kana (chokan--roman-to-hiragana alphabet)))
    (pcase kana
      (`() nil)
      ((and v (pred stringp))
       (if (eq chokan--internal-mode 'hiragana)
           v
         (chokan--roman-hira-to-kata v))))))

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
    (if-let* ((backward-prop (text-property-search-backward 'chokan-inverse t t))
              (forward-prop (text-property-search-forward 'chokan-inverse t t)))
        (cons (prop-match-beginning backward-prop) (prop-match-end forward-prop))
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

'char-props' は以下のassocである。いずれも `nil' または固有の値または `t' である。

`((roman . nil)
  (conversion-start . nil)
  (inverse . nil))
'

- 'roman' :: ローマ字変換の対象
- 'conversion-start' :: 変換の起点。下線部表記になる。 'normal' `'tankan'のいずれかを設定する
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
                                                    'chokan-conversion-detail (plist-get props 'chokan-conversion-detail)
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
    (let ((key (or (chokan--symbol-convert-to-ja key) key)))
      (chokan--insert-with-type key char-props)
      ;; 対象の部分に下線部が追加されたりするのでその対応をする
      (let* ((props (text-properties-at (- (point) (length key)))))
        (put-text-property (- (point) (length key)) (point) 'face (chokan--get-face props)))))))

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
    (chokan--conversion-launch (lambda (start end candidate)
                                 (let* ((candidate (or (cdr candidate) (buffer-substring-no-properties start end))))
                                   (chokan--insert-candidate (cons start end) candidate))))))

(defun chokan--finalize-inverse-if-possible (finalizable &optional inverted-region)
  "反転部を確定できる場合は確定する。

'INVERTED-REGION' にconsが渡されている場合はそれが利用される"

  (when-let* (finalizable
              (region (or (and (consp inverted-region) inverted-region)
                          (chokan--get-inverse-region))))
    (remove-text-properties (car region) (cdr region) '(chokan-inverse t face nil))))

(defun chokan--insert (convert-launchable underscore char-type)
  "chokanにおける各文字を入力するためのエントリーポイントとなる関数。特殊な記号による入力はこの関数以外で実行すること。

'CONVERT-LAUNCHABLE' が 'non-nil' の場合、起動したコマンドのキーが変換起動可能であることを表す。
'UNDERSCORE' が 'non-nil' の場合、入力した文字が下線部になる。指定したsymbolに対応する特殊変換がトグルされる
'CHAR-TYPE' は、 'alphabet' 'symbols' のいずれかのsymbolである。

この関数では以下を実行する。

1. 現在のmodeがカタカナであるかどうか
2. 反転部の確定（ 'convert-launchable' が non-nil である場合）
3. 下線部のかな漢字変換起動（ 'convert-launchable' が non-nil である場合）
4. 下線部の設定（ 'underscore' が non-nil である場合）
5. 自己挿入し、必要ならローマ字かな変換を行う
"
  (let* ((key (this-command-keys)))
    ;; 下線部を追加する場合は、カタカナモードからは強制的に離脱する
    (when (and (chokan--ja-katakana-p)
               (not (null underscore)))
      (setq chokan--internal-mode 'hiragana))
    (chokan--finalize-inverse-if-possible convert-launchable)
    (chokan--launch-conversion-if-possible convert-launchable)
    (chokan--self-insert key char-type `((roman . ,(eq char-type 'alphabet))
                                         (conversion-start . ,underscore)
                                         (inverse . nil)))
    ;; stickyはあらゆる入力で解除される
    (chokan--sticky-deactivate)))

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
  (let ((conversion-start (and (chokan--sticky-p) 'normal)))
    (chokan--insert (not (null conversion-start)) conversion-start 'alphabet)))

(defun chokan-insert-conversion-start-key ()
  "変換起動をして文字を入力する"
  (interactive)
  (chokan--insert t 'normal 'alphabet))

(defun chokan-insert-symbol-key ()
  "各種記号を入力する。記号は原則として変換起動するが、自分自身は下線部ではない。"
  (interactive)
  (chokan--insert t nil 'symbols))

(defun chokan-insert-tankan-start-key ()
  "単漢字変換を起動して文字を入力する"
  (interactive)
  (chokan--insert t 'tankan 'symbols))

(defun chokan-next-candidate ()
  "現在の反転部に対する次の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region)))
        (when-let* ((candidate (when-let* ((next (nth (1+ chokan--conversion-candidate-pos) chokan--conversion-candidates)))
                                 (setq chokan--conversion-candidate-pos (1+ chokan--conversion-candidate-pos))
                                 next)))
          (chokan--insert-candidate region (cdr candidate)))
      (let* ((chokan-ja-mode nil)
             (old-func (key-binding current-key)))
        (call-interactively old-func)))))

(defun chokan-previous-candidate ()
  "現在の反転部に対する前の候補を表示する

反転部がない場合は、もともとのキーバインドにフォールバックする。"
  (interactive)
  (let ((current-key (this-command-keys)))
    (if-let* ((region (chokan--get-inverse-region)))
        (when-let ((candidate (if (zerop chokan--conversion-candidate-pos)
                                  nil
                                (when-let* ((prev (nth (1- chokan--conversion-candidate-pos) chokan--conversion-candidates)))
                                  (setq chokan--conversion-candidate-pos (1- chokan--conversion-candidate-pos))
                                  prev))))
          (chokan--insert-candidate region (cdr candidate)))
      (let* ((chokan-ja-mode nil)
             (old-func (key-binding current-key)))
        (call-interactively old-func)))))

(defun chokan-force-finalize ()
  "強制的に反転部を確定させる"
  (interactive)
  (chokan--finalize-inverse-if-possible t)
  (chokan--launch-conversion-if-possible t))

(defun chokan-sticky ()
  "次に入力するアルファベットを、大文字のアルファベットと同等にする"
  (interactive)
  (chokan--sticky-activate))

;; mode definition

;;;###autoload
(define-minor-mode chokan-ascii-mode
  "Toggle minor mode to enable Input Method `chokan' in this buffer.

This mode only handle to keymap for changing mode to `chokan-mode' and `chokan-ja-mode'.
"
  :keymap chokan-ascii-mode-map
  :after-hook (progn
                (setq chokan--internal-mode 'ascii)
                ))

;;;###autoload
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
  (setq-local chokan--sticky nil)
  (setq-local chokan--conversion-candidates nil)
  (setq-local chokan--conversion-candidate-pos 0)
  (chokan-ja-mode)
  )

;;;###autoload
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
(define-key chokan-ja-mode-map (kbd "C-j") #'chokan-force-finalize)
(define-key chokan-ja-mode-map (kbd "M-c") #'chokan-ascii)
(define-key chokan-ja-mode-map (kbd "*") #'chokan-toggle-katakana)
(define-key chokan-ja-mode-map (kbd "C-h") #'chokan-next-candidate)
(define-key chokan-ja-mode-map (kbd "C-g") #'chokan-previous-candidate)
(define-key chokan-ja-mode-map (kbd "'") #'chokan-sticky)
(define-key chokan-ja-mode-map (kbd "@") #'chokan-insert-tankan-start-key)

(dolist (k '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-normal-alphabet))
(dolist (k '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-conversion-start-key))
(dolist (k '("-" "." "," "=" "+" "_" "|" "$" "%" "&" "^" "~" "!" "?" "\"" "`" "(" ")" "[" "]" "{" "}" "<" ">" " " "<return>"))
  (define-key chokan-ja-mode-map (kbd k) #'chokan-insert-symbol-key))

(provide 'chokan)
