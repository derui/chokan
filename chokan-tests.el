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
  (should (equal nil (chokan--roman-to-hiragana "lx")))
  (should (equal nil (chokan--roman-to-hiragana "l")))
  (should (equal "し" (chokan--roman-to-hiragana "si")))
  (should (equal nil
                 (chokan--roman-to-hiragana "z")))
  (should (equal "あ"
                 (chokan--roman-to-hiragana "a")))
  (should (equal nil
                 (chokan--roman-to-hiragana "qi")))
  )

(ert-deftest sokuon-to-kana ()
  (should (equal nil
                 (chokan--roman-to-hiragana "tt")))
  (should (equal "っす"
                 (chokan--roman-to-hiragana "ssu")))
  (should (equal "っった"
                 (chokan--roman-to-hiragana "ttta")))
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
