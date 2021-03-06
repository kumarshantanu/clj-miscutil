(ns clj-miscutil.test.core
  (:import
    (java.io File ByteArrayOutputStream PrintStream))
  (:use clj-miscutil.core)
  (:use clojure.test))


(defn is=
  "Apply (is (= (f k) v)) on all entries of a map"
  [f m]
  (doall
    (map #(is (= (f (first %)) (last %)))
      (seq m))))


(deftest test-str
  (testing "sb-str"
    (is (= "45" (sb-str 45)))
    (is (= "foobar" (sb-str "foo" "bar")))
    (is (= "foobar45" (sb-str "foo" "bar" 45)))))


(deftest test-maps
  (testing "map-keys"
    (is (= (array-map 2 2 4 4 6 6) (map-keys inc (array-map 1 2 3 4 5 6)))))
  (testing "map-vals"
    (is (= (array-map 1 1 3 3 5 5) (map-vals dec (array-map 1 2 3 4 5 6))))))


(deftest test-random
  (testing "random-number"
    (is (not= (random-number) (random-number) (random-number)))
    (let [ms 10 mx 20 r (random-number ms mx)]
      (is (>= r ms))
      (is (< r mx))))
  (testing "random-charseq"
    (is (not= (take 5 (random-charseq)) (take 5 (random-charseq))
          (take 5 (random-charseq))))
    (is (seq? (random-charseq)))
    (let [x (random-charseq 10)
          y (random-charseq 20)
          z (random-charseq 30)]
      (is (= 10 (count x))) (is (seq? x))
      (is (= 20 (count y))) (is (seq? y))
      (is (= 30 (count z))) (is (seq? z))))
  (testing "random-string"
    (is (not= (random-string) (random-string) (random-string)))
    (let [x (random-string 10)
          y (random-string 20)
          z (random-string 30)]
      (is (= 10 (count x)))
      (is (= 20 (count y)))
      (is (= 30 (count z)))
      (is (not= x y z)))))


(deftest test-type-check
  (testing "boolean?"
    (is (boolean? false)))
  (testing "not-boolean?"
    (is (not-boolean? 55)))
  (testing "date?"
    (is (date? (java.util.Date.)) "Identify Date type correctly")
    (is (not (date? "Hello")) "Return false for not a Date type"))
  (testing "not-date?"
    (is (not-date? 1000) "Detect a non-Date value")
    (is (not (not-date? (java.util.Date.))))))


(deftest test-pretty-printing
  (testing "with-stringwriter"
    (is (= "Hello World!" (with-stringwriter s
                       (.append s "Hello World!")))))
  (testing "with-err-str"
    (is (= "Hello" (with-err-str (binding [*out* *err*] (print "Hello")))))
    (let [txt (with-err-str
                (maybe-val (! (/ 10 0))))
          ltx (.substring ^String txt 0 45)]
      (is (= ltx "java.lang.ArithmeticException: Divide by zero"))))
  (testing "pprint-str"
    (let [x (take 12 (iterate inc 0))]
      (is (= (with-out-str (clojure.pprint/pprint x)) (pprint-str x)))))
  (testing "comma-sep-str"
    (is (= "1, 2, 3" (comma-sep-str [1 2 3]))))
  (testing "echo"
    (let [t "hello"
          r (echo t)
          s (with-out-str (echo t))]
      (is (= r t))
      (is (= s (format "\"%s\"\n" t))))))


(deftest test-var-metadata
  (testing "var-name"
    (is (= "map?" (var-name map?)))
    (is (= "some" (var-name some))))
  (testing "var-body"
    (is (let [b (var-body map?)]
          (and (string? b) (not-empty? b))))
    (is (= "Source not found\n" (var-body #(do 44))))))


(deftest test-exception-throwing
  (testing "val-dump"
    (is (= "(class clojure.lang.Keyword) :foo\n" (val-dump :foo)))
    (is (= "<nil>\n" (val-dump nil))))
  (testing "illegal-arg"
    (is (thrown? IllegalArgumentException (illegal-arg "reason"))))
  (testing "illegal-arg-wrap"
    (is (thrown? IllegalArgumentException (illegal-arg-wrap
                                            (NullPointerException.) "Null value"))))
  (testing "illegal-argval"
    (is (thrown? IllegalArgumentException (illegal-argval "reason"))))
  (testing "illegal-state"
    (is (thrown? IllegalStateException    (illegal-state "reason"))))
  (testing "illegal-state-wrap"
    (is (thrown? IllegalStateException    (illegal-state-wrap
                                            (NullPointerException.) "Null value"))))
  (testing "unsupported-op"
    (is (thrown? UnsupportedOperationException (unsupported-op "reason"))))
  (testing "unsupported-op-wrap"
    (is (thrown? UnsupportedOperationException (unsupported-op-wrap
                                                 (NullPointerException.) "Null value")))))


(deftest test-exception-catching
  (testing "maybe"
    (is (= [10 nil] (maybe 10)))
    (let [[_ e] (maybe (throw (NullPointerException.)))]
      (is (instance? NullPointerException e))))
  (testing "maybe-val"
    (is (= 10  (maybe-val 10)))
    (is (= nil (maybe-val (throw (NullPointerException.))))))
  (testing "maybe-ex"
    (is (= nil (maybe-ex 10)))
    (is (instance? NullPointerException
          (maybe-ex (throw (NullPointerException.))))))
  (testing "filter-exception"
    (is (= 10 (filter-exception (constantly true) 10)))
    (is (= nil (filter-exception #(instance? IllegalStateException %)
                 (throw (IllegalArgumentException.)))))
    (is (thrown? IllegalArgumentException
          (filter-exception #(instance? IllegalArgumentException %)
            (throw (IllegalArgumentException.))))))
  (testing "with-exceptions"
    (is (thrown? IllegalArgumentException
          (with-exceptions [IllegalArgumentException] [Exception]
            (throw (IllegalArgumentException.)))))
    (is (nil? (with-exceptions [IllegalArgumentException] [Exception]
                10
                (throw (NullPointerException.)))))
    (is (= 10 (with-exceptions [] []
                10))))
  (testing "repeat-exec"
    (is (= [5 5] (repeat-exec 2 (constantly 5))))
    (is (thrown? NullPointerException (repeat-exec 20
                                        (throw (NullPointerException.))))))
  (testing "try-while"
    (let [a (atom 0)
          b #(do (swap! a inc)
               (if (< @a 5) (throw (NullPointerException.))
                 (%)))
          f #(b (fn [] (+ 10 @a))) ; returns number
          g #(b (fn [] false))]    ; returns logical false
      (is (= 15 (try-while (fn [^Throwable e] (< @a 7)) (f))))
      (is (= 5 @a))))
  (testing "try-times"
    (let [a (atom 0)
          b #(do (swap! a inc)
               (if (< @a 5) (throw (NullPointerException.))
                 (%)))
          f #(b (fn [] (+ 10 @a))) ; returns number
          g #(b (fn [] false))]    ; returns logical false
      (is (= 15 (try-times 7 (f))))
      (is (= 5 @a))
      (reset! a 0)
      (is (thrown? NullPointerException (try-times 3 (f))))
      (reset! a 0)
      (is (= false (try-times 7 (g))))
      (is (= 5 @a))
      (reset! a 0)
      (is (thrown? NullPointerException (try-times 3 (g))))))
  (testing "with-safe-open"
    (let [close-ex (proxy [java.io.Closeable] []
                     (close [] (throw (IllegalStateException.))))]
      (is (thrown? IllegalStateException
            (with-open [c close-ex]
              10)))
      (is (= 10 (with-safe-open [c close-ex]
                  10))))))


(deftest test-type-conversion
  (let [tester  (fn [converter]
                  #(is (= (converter (first %)) (last %))))
        testall (fn [converter av-map]
                  (doall (map (tester converter) (seq av-map))))]
    (testing "as-string"
      (testall as-string
        {10 "10", nil "", "hello" "hello", :kw "kw"}))
    (testing "java-filepath"
      (is (= "C:/path/to/file.txt" (java-filepath "C:\\path\\to\\file.txt"))))
    (testing "local-filepath"
      (let [d (File/separator)]
        (is (= (format "C:%spath%sto%sfile.txt" d d d)
              (local-filepath "C:/path\\to/file.txt")))))
    (testing "split-filepath"
      (is (= ["C:/path/to" "file.txt"] (split-filepath "C:\\path\\to\\file.txt")))
      (is (= ["/path/to" "file.txt"]   (split-filepath "/path/to/file.txt")))
      (is (= ["" "file.txt"]           (split-filepath "file.txt"))))
    (testing "pick-filedir"
      (is (= "C:/path/to" (pick-filedir "C:\\path\\to\\file.txt")))
      (is (= ""           (pick-filedir "file.txt"))))
    (testing "pick-filename"
      (is (= "file.txt"   (pick-filename "C:\\path\\to\\file.txt")))
      (is (= "file.txt"   (pick-filename "file.txt"))))
    (testing "split-filename"
      (is (= ["file" "txt"]      (split-filename "C:\\path\\to\\file.txt")))
      (is (= ["file" "txt"]      (split-filename "/path/to/file.txt")))
      (is (= ["file" "txt"]      (split-filename "file.txt")))
      (is (= ["file.exe"  "txt"] (split-filename "file.exe.txt")))
      (is (= [".file.exe" "txt"] (split-filename ".file.exe.txt")))
      (is (= [".txt" ""]         (split-filename ".txt")))
      (is (= ["file" ""]         (split-filename "file"))))
    (testing "pick-filename-only"
      (is (= "file" (pick-filename-only "C:\\path\\to\\file.txt")))
      (is (= "file" (pick-filename-only "file.txt")))
      (is (= ".txt" (pick-filename-only ".txt"))))
    (testing "pick-filename-ext"
      (is (= "txt"  (pick-filename-ext "C:\\path\\to\\file.txt")))
      (is (= "txt"  (pick-filename-ext "file.txt")))
      (is (= "txt"  (pick-filename-ext "file.exe.txt")))
      (is (= ""     (pick-filename-ext ".txt")))
      (is (= ""     (pick-filename-ext "file"))))
    (testing "as-vstr"
      (testall as-vstr
        {10 "10", nil "<nil>", "hello" "hello", :kw "kw"}))
    (testing "as-keys"
      (testall (comp set as-keys)
               {{:a 10 :b 20} #{:a :b}, [:b :c] #{:b :c}})
      (is (= 30 (as-keys [:a 20] 30))))
    (testing "as-vals"
      (testall (comp set as-vals)
               {{:a 10 :b 20} #{10 20}, [:b :c] #{:b :c}})
      (is (= 33 (as-vals [:a 10] 33))))
    (testing "as-vector"
      (testall as-vector
        {10 [10], [20] [20], nil [], '(30) [30], {:k 40} [40]}))
    (testing "as-set"
      (testall as-set
        {10 #{10}, [20] #{20}, nil #{}, '(30) #{30}, {:k 40} #{40}}))
    (testing "as-map"
      (is (= (as-map [10 20 30 40]) {10 20 30 40}))
      (is (= (as-map {10 20}) {10 20}))
      (is (thrown? IllegalArgumentException
            (as-map 10))))
    (testing "as-boolean"
      (testall as-boolean
        {"true" true "yes" false 45 false nil false (java.util.Date.) false}))
    (testing "as-short"
      (testall as-short
        {"10" (short 10) 20 (short 20) nil nil "ab" nil [29] (short 29) {:v 93} (short 93)}))
    (testing "as-integer"
      (testall as-integer
        {"10" 10 20 20 nil nil "ab" nil [29] 29 {:v 93} 93}))
    (testing "as-long"
      (testall as-long
        {"10" 10 20 20 nil nil "ab" nil [29] 29 {:v 93} 93}))
    (testing "as-float"
      (testall as-float
        {"10" 10.0 "45.44" (float 45.44) 20 20.0 nil nil "ab" nil [29] 29.0 {:v 93} 93.0}))
    (testing "as-double"
      (testall as-double
        {"10" 10.0 20 20.0 "12.34" 12.34 nil nil "ab" nil [29] 29.0 {:v 93} 93.0}))))


(deftest test-not-prefixed
  (testing "not-nil?"
    (doseq [i [10 "hello" false [4 5 6]]]
      (is (= (not (nil? i)) (not-nil? i))))
    (is (= (not (nil? nil)) (not-nil? nil))))
  (testing "not-empty?"
    (doseq [i [10 "hello" false [4 5 6]]]
      (is (= (not (empty? (as-vector i))) (not-empty? (as-vector i)))))
    (is (= (not (empty? [])) (not-empty? [])))))


(deftest test-arrays
  (testing "array-type"
    (is (= nil (array-type nil)))
    (is (= (array-type (into-array ["Hello" "World"])) String)))
  (testing "array?"
    (is (not (array? nil)))
    (is (not (array? ["Hello" "World"])))
    (is (array? (into-array ["Hello" "World"])))
    (is (array? (into-array ["Hello" "World"]) String)))
  (testing "not-array?"
    (is (not-array? nil))
    (is (not-array? []))
    (is (not (not-array? (into-array []))))))


(deftest test-contains-val
  (testing "contains-val?"
    (is (contains-val? [91 92 93 94 95] 91))
    (is (not (contains-val? nil "Hello")))
    (is (contains-val? {:dog "animal" :papaya "fruit"} "fruit")))
  (testing "not-contains-val?"
    (is (not-contains-val? nil "Hello"))
    (is (not-contains-val? [91 92 93 94 95] 99))
    (is (not-contains-val? {:dog "animal" :papaya "fruit"} :papaya))))


(deftest test-stacktrace-printing
  (testing "print-exception-stacktrace"
    (let [st (with-err-str (print-exception-stacktrace (NullPointerException.)))]
      (is (and (string? st) (not-empty? st)))))
  (testing "!"
    (let [st (with-err-str (maybe-val (! (/ 10 0))))]
      (is (and (string? st) (not-empty? st))))))


(deftest test-assertion-helpers
  (testing "verify-arg"
    (is (= true (verify-arg (empty? []))))
    (is (thrown? IllegalArgumentException (verify-arg (empty? [10])))))
  (testing "verify-cond"
    (is (= true (verify-cond (empty? []))))
    (is (thrown? IllegalStateException (verify-cond (empty? [10])))))
  (testing "verify-opt"
    (is (thrown? IllegalArgumentException (verify-opt [:a :b] [:c :d])))
    (is (true? (verify-opt [:a :b] [:a]))))
  (testing "verify-type"
    (is (= true (verify-type String "aa")))
    (is (thrown? IllegalArgumentException (verify-type String true))))
  (testing "echo"
    (let [m {:a 10 :b 20}
          r (echo m)
          s (with-out-str (echo m))]
      (is (= r m) "Return value should be same as the argument")
      (is (or (= s "{:a 10, :b 20}\n")
              (= s "{:b 20, :a 10}\n")) "Output of 'pprint'"))))


(deftest test-type-annotation
  (testing "obj?"
    (is (obj? {}))
    (is (not (obj? 10))))
  (testing "not-obj?"
    (is (not (not-obj? {})))
    (is (not-obj? 10)))
  (testing "typed"
    (is (typed? (typed {} :abc) :abc))
    (is (typed? (typed {} :abc :def) :abc))
    (is (typed? (typed {} :abc :def) :def))
    (let [t (typed (typed {} :abc) :def :ghi)]
      (is (typed? t :abc))
      (is (typed? t :def))
      (is (typed? t :ghi))))
  (testing "ftyped"
    (is (typed? (ftyped 1055 :abc) :abc))
    (is (typed? (ftyped 1055 :abc :def) :abc))
    (is (typed? (ftyped (ftyped 1055 :abc) :def :ghi) :abc))
    (let [t (ftyped (ftyped {} :abc) :def :ghi)]
      (is (typed? t :abc))
      (is (typed? t :def))
      (is (typed? t :ghi))))
  (testing "untyped"
    (is (not-contains? (untyped (typed {} :abc :def)) :type)))
  (testing "type-meta"
    (is (= (type-meta (typed {} :abc)) :abc)))
  (testing "hierarchy-test for typed?"
    (let [e (make-hierarchy)
          g (derive e ::employee ::salaried)
          h (derive g ::salaried ::person)]
      (is (every? #(typed? h (typed {} ::employee) %) [::salaried ::person]))
      (is (some #(typed? h (typed {} ::employee) %) [::freelancer ::person])))
    (derive ::employee ::salaried)
    (derive ::salaried ::person)
    (is (every? #(typed? (typed {} ::employee) %) [::salaried ::person]))
    (is (some #(typed? (typed {} ::employee) %) [::freelancer ::person]))))


(deftest test-keyword-string-conversion
  (testing "k-to-camelstr"
    (is= k-to-camelstr {:-            ""
                        :to-do        "toDo"       ; dash triggers upper-case
                        :to_do        "to_do"      ; underscore stays intact
                        :to-do-       "toDo"       ; trailing dash is ignored
                        :-from-here   "fromHere"   ; leading dash is ignored too
                        :hello--there "helloThere" ; consecutive dashes are one
                        :toDo         "todo"       ; ignore uppercase in keyword
                        }))
  (testing "camelstr-to-k"
    (is= camelstr-to-k {""     (keyword "")
                        "toDo" :to-do
                        "toDO" :to-d-o
                        "ToDo" :to-do
                        "TODO" :t-o-d-o
                        }))
  (testing "k-to-methodname"
    (is= k-to-methodname {:to-do "toDo"
                          :todo  "todo"
                          :doAll "doAll"
                          })
    (is= #(k-to-methodname % ["do"]) {:execute     "doExecute"
                                      :run-forever "doRunForever"
                                      :do-run      "doRun"
                                      :doAll       "doAll"
                                      }))
  (testing "k-to-setter"
    (is= k-to-setter {:price      "setPrice"
                      :unit-price "setUnitPrice"
                      :unitPrice  "setUnitPrice"
                      :set-price  "setPrice"     ; detect "set" prefix
                      }))
  (testing "k-to-getter"
    (is= k-to-getter {:price "getPrice"
                      :unit-price "getUnitPrice"
                      :unitPrice  "getUnitPrice"
                      :get-price  "getPrice"     ; detect "get" prefix
                      :is-valid   "isValid"      ; detect "is"  prefix
                      :isValid    "isValid"
                      })))


(deftest test-reflection
  (testing "call-specs"
    (is (= (call-specs "Hello" [:char-at 0] :to-string)
          [["Hello" :char-at 0] ["Hello" :to-string]])))
  (testing "method"
    (is (= (method (StringBuilder.) :set-length 0) nil)) ; returns void
    (is (= (method "Hello" :char-at 0) \H))       ; returns primitive char
    (is (= (method "Hello" :substring 3 4) "l"))  ; returns string
    (is (= (method "Hello" :to-string) "Hello"))  ; no-arg method
    ;; combined call
    (is (= (method (into [[(StringBuilder.) :set-length 0]]
                     (call-specs "Hello"
                       [:char-at 0] [:substring 3 4] [:to-string])))
          [nil \H "l" "Hello"])))
  (testing
    "static-method"
    (let [bs (ByteArrayOutputStream.)
          ps (PrintStream. bs)
          out System/out]
      (try
        (static-method System :set-out ps)
        (.print System/out "hello")
        (is (= "hello" (.toString bs)))
        (finally
          (System/setOut out)))))
  (testing "pojo-fn"
    (is (= (doall (map (pojo-fn "Hello") [[:char-at 0]     ; returns primitive char
                                          [:substring 3 4] ; returns string
                                          [:to-string]     ; no-arg method
                                          ]))
          [\H "l" "Hello"]))
    (is (= (doall (map (pojo-fn "Hello" :char-at) [0 1 2]))
          [\H \e \l]))
    (is (= (doall (map #((apply pojo-fn "Hello" :char-at %)) [[0] [1] [2]]))
          [\H \e \l])))
  (testing
    "class-fn"
    (let [bs (ByteArrayOutputStream.)
          ps (PrintStream. bs)
          out System/out]
      (try
        ((class-fn System) [:set-out ps])
        (.print System/out "hello")
        (is (= "hello" (.toString bs)))
        (finally
          (System/setOut out)))))
  (testing "setter"
    (is (= (setter (StringBuilder.) :length 0) ; .setLength(0) - returns void
          nil))
    (let [sb (StringBuilder. "Hello")]
      (is (= (setter (call-specs sb
                       [:length 4]     ; .setLength(4)      - returns void
                       [:char-at 0 \C] ; .setCharAt(0, 'C') - returns void
                       ))
            [nil nil]))
      (is (= (.toString sb) "Cell"))))
  (testing
    "static-setter"
    (let [bs (ByteArrayOutputStream.)
          ps (PrintStream. bs)
          out System/out]
      (try
        (static-setter System :out ps)
        (.print System/out "hello")
        (is (= "hello" (.toString bs)))
        (finally
          (System/setOut out)))))
  (testing "setter-fn"
    (let [sb (StringBuilder. "Hello")]
      (is (= (doall (map (setter-fn sb)
                      [[:length 4]     ; .setLength(4)      - returns void
                       [:char-at 0 \C] ; .setCharAt(0, 'C') - returns void
                       ]))
            [nil nil]))
      (is (= (doall (map #((apply setter-fn sb :char-at %))
                      [[0 \C] ; .setCharAt(0, 'C') - returns void
                       [1 \o] ; .setCharAt(0, 'o') - returns void
                       [2 \o] ; .setCharAt(0, 'o') - returns void
                       ]))
            [nil nil nil]))
      (is (= (.toString sb) "Cool"))
      (is (= (doall (map (setter-fn sb :char-at 0)
                      [\A ; .setCharAt(0, 'A') - returns void
                       \B ; .setCharAt(0, 'B') - returns void
                       ]))
            [nil nil]))
      (is (= (.toString sb) "Bool"))))
  (testing
    "static-setter-fn"
    (let [bs (ByteArrayOutputStream.)
          ps (PrintStream. bs)
          out System/out]
      (try
        ((static-setter-fn System) [:out ps])
        (.print System/out "hello")
        (is (= "hello" (.toString bs)))
        (finally
          (System/setOut out)))))
  (let [lst (java.util.LinkedList.)
        _   (.add lst 1)
        _   (.add lst 2)]
    (testing "getter"
      (is (= (getter lst :first) ; .getFirst()
            1))
      (is (= (getter (call-specs lst :first ; .getFirst() - returns 1
                                     :last  ; .getLast()  - returns 2
                                     ))
            [1 2])))
    (testing
      "static-getter"
      (static-setter System :property "foo" "bar")
      (= "bar" (static-getter System :property "foo")))
    (testing "getter-fn"
      (is (= (doall (map (getter-fn lst) [:first ; .getFirst() - returns 1
                                          :last  ; .getLast()  - returns 2
                                          ]))
            [1 2]))
      (is (= (doall (map #((apply getter-fn lst :first %))
                      [[] ; .getFirst() - returns 1
                       [] ; .getFirst() - returns 1
                       ]))
            [1 1])))
    (testing
      "static-getter-fn"
      (static-setter System :property "foo" "bar")
      (= "bar" ((static-getter-fn System) [:property "foo"])))
    (testing "coll-as-string"
      (is (= ["a" "b" "10"] (coll-as-string [:a "b" 10]))))
    (testing "coll-as-keys"
      (is (= [:a :b :c] (coll-as-keys ["a" "b" :c]))))
    (testing "keys-to-str"
      (is (= {"a" 10 "b" 20 "c" 30} (keys-to-str {:a 10 :b 20 :c 30}))))
    (testing "str-to-keys"
      (is (= {:a 10 :b 20 :c false} (str-to-keys {"a" 10 "b" 20 "c" false}))))))


(deftest test-properties
  (let [;;ps (read-properties "src/test/conf/dbconfig.properties")
        ps (java.util.Properties.)
        _  (setter (call-specs ps
                     [:property "a" "10"]
                     [:property "b" "20"]
                     [:property "c" "true"]))
        pm (property-map ps)
        km (str-to-keys pm)]
    (testing "property-map"
      (is (= pm {"a" "10"
                 "b" "20"
                 "c" "true"})))
    (testing "str-to-keys"
      (is (= km {:a "10"
                 :b "20"
                 :c "true"})))
    (testing "is-true?"
      (is (is-true? (:c km)))
      (is (is-true? :True))
      (is (is-true? 10)))))


(deftest test-jndi
  "See also (not used): http://commons.apache.org/dbcp/guide/jndi-howto.html"
  (testing "print-jndi-tree"
    (with-root-context (javax.naming.InitialContext.)
      (print-jndi-tree)))
  (testing "Parallel print-jndi-tree"
    (doall (pmap (fn [_] (with-root-context (javax.naming.InitialContext.)
                           (print-jndi-tree))) (take 5 (repeat true)))))
  (let [sc (find-jndi-subcontext (javax.naming.InitialContext.)
             "java:comp")]
    (is (not-nil? sc))
    (print-jndi-tree sc))
  (let [ds (jndi-lookup "java:comp/env/myDataSource")]
    (is (not-nil? ds))
    (is (instance? javax.sql.DataSource ds))
    (println ds)))


(defn test-ns-hook []
  (test-str)
  (test-maps)
  (test-random)
  (test-type-check)
  (test-pretty-printing)
  (test-var-metadata)
  (test-exception-throwing)
  (test-exception-catching)
  (test-type-conversion)
  (test-not-prefixed)
  (test-arrays)
  (test-contains-val)
  (test-stacktrace-printing)
  (test-assertion-helpers)
  (test-type-annotation)
  (test-keyword-string-conversion)
  (test-reflection)
  (test-properties)
  (test-jndi))
