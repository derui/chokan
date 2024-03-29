;;; chokan-websocket.el --- conversion method with websocket -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "27.1") (websocket) (jsonrpc))
;; Keywords: ime

;;; Commentary:
;;
;;: Customization:

;;; Code:

(require 'jsonrpc)
(require 'websocket)

(defcustom chokan-websocket-address "127.0.0.1"
  "chokan-serverの起動アドレス"
  :type 'string
  :group 'chokan)

(defcustom chokan-websocket-port "8876"
  "chokan-serverが起動しているポート"
  :type 'string
  :group 'chokan)

(defvar chokan-websocket--connection-cache nil
  "変換サーバーへのconnectionを保持する。全バッファで共有される。")
(defvar chokan-websocket--websocket nil
  "変換サーバーへのwebsocket connectionを保持する。全バッファで共有される。")

;; websocket用のjsonrpc-connectionを定義する
(defclass chokan-websocket--connection (jsonrpc-connection)
  ((-websocket
    :initarg :socket :accessor chokan-websocket--websocket
    :documentation "The websocket process."))
  :documentation "JSON-RPC connection over websocket")

(cl-defmethod jsonrpc-connection-send ((conn chokan-websocket--connection)
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

(cl-defmethod jsonrpc-running-p ((conn chokan-websocket--connection))
  "websocketコネクションが生存しているかどうかを返す"
  (with-slots (-websocket) conn
    (and -websocket
         (websocket-openp -websocket)))
  )

(cl-defmethod jsonrpc-shutdown ((conn chokan-websocket--connection))
  "websocketコネクションをシャットダウンする"
  (with-slots (-websocket) conn
    (when (websocket-openp -websocket)
      (websocket-close -websocket))))

(defun chokan-websocket--on-websocket-message (ws frame)
  "websocketからのmessageを、JSON RPCとして解釈する。"
  (jsonrpc-connection-receive
   ;; client自体のデータとしてconnectionが入っている。
   (websocket-client-data ws)
   (json-parse-string
    (websocket-frame-text frame)
    :object-type 'plist
    :null-object nil
    :false-object :json-false)))

(defun chokan-websocket--current-connection ()
  "JSON RPCで利用するconnection objectを返す。

実行時にconnection objectが存在していない場合は作成を試みる"
  (or chokan-websocket--connection-cache
      (progn
        (setq chokan-websocket--websocket (websocket-open
                                           (format "ws://%s:%s" chokan-websocket-address chokan-websocket-port)
                                           :on-message 'chokan-websocket--on-websocket-message
                                           :on-close (lambda (_)
                                                       (setq chokan-websocket--connection-cache nil))
                                           :on-error (lambda (_)
                                                       (setq chokan-websocket--connection-cache nil))))
        (setq chokan-websocket--connection-cache
              (make-instance 'chokan-websocket--connection
                             :socket chokan-websocket--websocket))
        (setf (websocket-client-data chokan-websocket--websocket) chokan-websocket--connection-cache)
        chokan-websocket--connection-cache)))

(defsubst chokan-websocket--context-to-server (name)
  "contextをserver向けに変換する"
  (cond
   ((eq name 'normal) "Normal")
   ((eq name 'foreign-word) "BorrowedWord")
   ((eq name 'numeral) "Numeral")))

(defun chokan-websocket-get-candidates (input ctx)
  "変換候補を取得する。

事前に対応するserverが起動している必要がある。サーバーのアドレスは `chokan-websocket-address' で設定する。"
  (let* ((conn (chokan-websocket--current-connection))
         (res (jsonrpc-request conn :GetCandidates `(:input ,input :context (:type ,(chokan-websocket--context-to-server (car ctx)) :value ,(cdr ctx)))))
         (candidates (plist-get res :candidates))
         (candidates (seq-map (lambda (c) (cons (plist-get c :id) (plist-get c :candidate))) candidates)))
    candidates))

(defun chokan-websocket-get-tankan-candidates (input ctx)
  "単漢字変換の変換候補を取得する。

事前に対応するserverが起動している必要がある。サーバーのアドレスは `chokan-websocket-address' で設定する。"
  (let* ((input (substring input 1))
         (conn (chokan-websocket--current-connection))
         (res (jsonrpc-request conn :GetTankanCandidates `(:input ,input)))
         (candidates (plist-get res :candidates))
         (candidates (seq-map (lambda (c) (cons (plist-get c :id) (plist-get c :candidate))) candidates)))
    candidates))

;; public functions

(provide 'chokan-websocket)
