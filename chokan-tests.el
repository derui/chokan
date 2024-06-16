;;; chokan-tests.el --- tests for chokan frontend -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: input-method

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'chokan)

;; define tests

(ert-deftest alphabet-to-kana ()
  (should (equal "lx" (chokan--roman-to-hiragana-2 "lx")))
  (should (equal "l" (chokan--roman-to-hiragana-2 "l")))
  (should (equal "し" (chokan--roman-to-hiragana-2 "si")))
  (should (equal "しじ" (chokan--roman-to-hiragana-2 "sizi")))
  (should (equal "してん" (chokan--roman-to-hiragana-2 "sitenn")))
  (should (equal "z"
                 (chokan--roman-to-hiragana-2 "z")))
  (should (equal "あ"
                 (chokan--roman-to-hiragana-2 "a")))
  (should (equal "qい"
                 (chokan--roman-to-hiragana-2 "qi")))
  )

(ert-deftest sokuon-to-kana ()
  (should (equal "っt"
                 (chokan--roman-to-hiragana-2 "tt")))
  (should (equal "っす"
                 (chokan--roman-to-hiragana-2 "ssu")))
  (should (equal "っった"
                 (chokan--roman-to-hiragana-2 "ttta")))
  (should (equal "かったら"
                 (chokan--roman-to-hiragana-2 "kattara")))
  )

(ert-deftest kana-and-alphabet ()
  (should (equal "pろgらm"
                 (chokan--roman-to-hiragana-2 "pろgらm")))
  (should (equal "とちゅう"
                 (chokan--roman-to-hiragana-2 "とtyuu")))
  )

(ert-deftest hira-to-kata ()
  (should (equal "カ" (chokan--roman-hira-to-kata "か")))
  (should (equal "ッt" (chokan--roman-hira-to-kata "っt")))
  )

(ert-deftest en-symbol-to-ja-symbol ()
  (should (equal "。" (chokan--symbol-convert-to-ja ".")))
  (should (equal "、" (chokan--symbol-convert-to-ja ",")))
  (should (equal "ー" (chokan--symbol-convert-to-ja "-"))))
(ert t)
