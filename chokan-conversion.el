;;; chokan-conversion.el --- conversion support of chokan -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "27.1") (websocket))
;; Keywords: ime

;;; Commentary:
;;
;;: Customization:

;;; Code:

(require 'url-parse)
(require 'jsonrpc)
(require 'chokan-variable)
(require 'websocket)

(defvar chokan-conversion-function
  #'chokan-conversion--get-candidates
  "変換起動した文字列から、実際に候補を取得する関数。

関数は、引数として変換対象となる文字列と、下線部の直前にあったcontextを受け取る。contextは、 (<type symbol> string) の形式で渡される。
contextが存在しない場合はnilを渡す。
'type symbol'は、'foreign'か'numeric'のいずれかである。

実行した結果として、候補のリストを返す。候補がない場合は'NIL'を返す。候補のリストは、文字列のリストである。
")

(defvar chokan-conversion--candidates nil
  "変換候補のリスト。変換起動が行われるたびに初期化される。

candidateは、それぞれ '(id . candidate)' というconsで保持される。idは、候補の識別子であり、candidateは、候補の文字列である。
")
(defvar chokan-conversion--candidate-pos 0
  "現在選択している候補の位置を 0オリジンで保持する。")

(defvar chokan-conversion--connection-cache nil
  "変換サーバーへのconnectionを保持する。全バッファで共有される。")
(defvar chokan-conversion--websocket nil
  "変換サーバーへのwebsocket connectionを保持する。全バッファで共有される。")

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

;; websocket用のjsonrpc-connectionを定義する
(defclass chokan-conversion--connection (jsonrpc-connection)
  ((-websocket
    :initarg :socket :accessor chokan-conversion--websocket
    :documentation "The websocket process."))
  :documentation "JSON-RPC connection over websocket")

(cl-defmethod jsonrpc-connection-send ((conn chokan-conversion--connection)
                                       &rest args
                                       &key
                                       _id
                                       method
                                       _params
                                       _result
                                       _error
                                       _partial)
  "Send a JSON-RPC REQUEST over the websocket connection."
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method)))))
  (let* ((message `(;; CDP isn't technically JSONRPC, so don't send
                    ;; the `:jsonrpc' "2.0" version identifier which
                    ;; trips up node's server, for example.
                    :jsonrpc "2.0"
                    ,@args))
         (json (jsonrpc--json-encode message)))
    (with-slots (-websocket) conn
      (websocket-send-text -websocket json))))

(cl-defmethod jsonrpc-running-p ((conn chokan-conversion--connection))
  "websocketコネクションが生存しているかどうかを返す"
  (with-slots (-websocket) conn
    (and -websocket
         (websocket-openp -websocket)))
  )

(cl-defmethod jsonrpc-shutdown ((conn chokan-conversion--connection))
  "websocketコネクションをシャットダウンする"
  (with-slots (-websocket) conn
    (when (websocket-openp -websocket)
      (websocket-close -websocket))))

(defun chokan-conversion--on-websocket-message (ws frame)
  "websocketからのmessageを、JSON RPCとして解釈する。"
  (jsonrpc-connection-receive
   ;; client自体のデータとしてconnectionが入っている。
   (websocket-client-data ws)
   (json-parse-string
    (websocket-frame-text frame)
    :object-type 'plist
    :null-object nil
    :false-object :json-false)))

(defun chokan-conversion--current-connection ()
  "JSON RPCで利用するconnection objectを返す。

実行時にconnection objectが存在していない場合は作成を試みる"
  (or chokan-conversion--connection-cache
      (progn
        (setq chokan-conversion--websocket (websocket-open
                                            (format "ws://%s:%s" chokan-server-address chokan-server-port)
                                            :on-message 'chokan-conversion--on-websocket-message
                                            :on-close (lambda (_)
                                                        (setq chokan-conversion--connection-cache nil))
                                            :on-error (lambda (_)
                                                        (setq chokan-conversion--connection-cache nil))))
        (setq chokan-conversion--connection-cache
              (make-instance 'chokan-conversion--connection
                             :socket chokan-conversion--websocket))
        (setf (websocket-client-data chokan-conversion--websocket) chokan-conversion--connection-cache)
        chokan-conversion--connection-cache)))

(defun chokan-conversion--get-candidates (input ctx)
  "変換候補を取得する。

事前に対応するserverが起動している必要がある。サーバーのアドレスは `chokan-server-address' で設定する。"
  (let* ((conn (chokan-conversion--current-connection))
         (res (jsonrpc-request conn :GetCandidates `(:input ,input)))
         (candidates (plist-get res :candidates))
         (candidates (seq-map (lambda (c) (cons (plist-get c :id) (plist-get c :candidate))) candidates)))
    candidates))

;; public functions

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

;; package private functions
(defun chokan-conversion--setup ()
  "変換に関する処理を行うためのセットアップを行う。

ここでは変数のバッファローカル化を行う。"
  (setq-local chokan-conversion--candidates nil)
  (setq-local chokan-conversion--candidate-pos 0))

(provide 'chokan-conversion)
