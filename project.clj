(defproject clj-miscutil "0.4.1"
  :description "Miscellaneous utility functions/macros in Clojure"
  :url "https://github.com/kumarshantanu/clj-miscutil"
  :mailing-list {:name "Bitumen Framework discussion group"
                 :archive "https://groups.google.com/group/bitumenframework"
                 :other-archives ["https://groups.google.com/group/clojure"]
                 :post "bitumenframework@googlegroups.com"}
  :profiles {:dev {:dependencies [[simple-jndi "0.11.4"]]}
             :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.0-alpha4"]]}}
  :aliases {"dev" ["with-profile" "dev,1.4"]
            "all" ["with-profile" "dev,1.2:dev,1.3:dev,1.4:dev,1.5"]}
  :warn-on-reflection true
  :min-lein-version "2.0.0")
