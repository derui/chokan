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
(require 'chokan-roman-table)
(require 'chokan)

;; define tests

(ert-deftest alphabet-to-kana ()
  (should (equal nil (chokan-roman-table-roman-to-kana "lx")))
  (should (equal nil (chokan-roman-table-roman-to-kana "l")))
  (should (equal "z"
                 (chokan-roman-table-roman-to-kana "z")))
  (should (equal '("あ" . "")
                 (chokan-roman-table-roman-to-kana "a")))
  (should (equal nil
                 (chokan-roman-table-roman-to-kana "qi")))
  )

(ert-deftest sokuon-to-kana ()
  (should (equal '("っ" . "t")
                 (chokan-roman-table-roman-to-kana "tt")))
  (should (equal '("っ" . "s")
                 (chokan-roman-table-roman-to-kana "ss")))
  )

(ert-deftest hira-to-kata ()
  (should (equal "カ" (chokan-roman-table-hira-to-kata "か")))
  (should (equal "ッt" (chokan-roman-table-hira-to-kata "っt")))
  )
