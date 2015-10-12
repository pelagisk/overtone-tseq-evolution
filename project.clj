(defproject overtone-tseq-evolution "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [overtone "0.9.1"]]
  :main ^:skip-aot overtone-tseq-evolution.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
