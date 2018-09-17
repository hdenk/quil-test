(ns ^:figwheel-no-load nature_of_code.dev
  (:require
    [nature_of_code.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
