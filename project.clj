(defproject clj-miscutil "0.5.0"
  :description "Miscellaneous utility functions/macros in Clojure"
  :url "https://github.com/kumarshantanu/clj-miscutil"
  :mailing-list {:name "Bitumen Framework discussion group"
                 :archive "https://groups.google.com/group/bitumenframework"
                 :other-archives ["https://groups.google.com/group/clojure"]
                 :post "bitumenframework@googlegroups.com"}
  :profiles {:dev {:dependencies [[simple-jndi "0.11.4"]
                                  [org.clojure/tools.nrepl "0.2.5"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}}
  :aliases {"dev" ["with-profile" "dev,1.6"]
            "all" ["with-profile" "dev,1.3:dev,1.4:dev,1.5:dev,1.6:dev,1.7"]}
  :min-lein-version "2.0.0"
  :global-vars {*warn-on-reflection* true})
