(ns lab1-clustering.io
  (require [clojure.java.io :as io]))

(defn print_lines [path]
  (with-open [reader (io/reader path)]
    (doseq [line (line-seq reader)]
      (println line))))
