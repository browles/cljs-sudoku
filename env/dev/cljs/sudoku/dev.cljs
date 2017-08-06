(ns ^:figwheel-no-load sudoku.dev
  (:require
    [sudoku.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
