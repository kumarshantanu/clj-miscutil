(ns org.bituf.test-clj-miscutil
  (:use org.bituf.clj-miscutil)
  (:use org.bituf.clj-miscutil.internal)
  (:use clojure.test))


(defn is=
  "Apply (is (= (f k) v)) on all entries of a map"
  [f m]
  (doall
    (map #(is (= (f (first %)) (last %)))
      (seq m))))


(deftest test-illegal-arg
  (testing "illegal-arg"
    (is (thrown? IllegalArgumentException
          (illegal-arg "reason")))))


(deftest test-type-conversion
  (let [tester  (fn [converter]
                  #(is (= (converter (first %)) (last %))))
        testall (fn [converter av-map]
                  (doall (map (tester converter) (seq av-map))))]
    (testing "as-str"
      (testall as-str
        {10 "10", nil "", "hello" "hello", :kw "kw"}))
    (testing "as-vstr"
      (testall as-vstr
        {10 "10", nil "<nil>", "hello" "hello", :kw "kw"}))
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
            (as-map 10))))))


(deftest test-not-prefixed
  (testing "not-nil?"
    (doseq [i [10 "hello" false [4 5 6]]]
      (is (= (not (nil? i)) (not-nil? i))))
    (is (= (not (nil? nil)) (not-nil? nil))))
  (testing "not-empty?"
    (doseq [i [10 "hello" false [4 5 6]]]
      (is (= (not (empty? (as-vector i))) (not-empty? (as-vector i)))))
    (is (= (not (empty? [])) (not-empty? [])))))


(deftest test-condition-assertion
  (let [throw-excp #(when-assert-cond
                      (illegal-arg "reason"))
        when-true  #(is (thrown? IllegalArgumentException
                          (throw-excp)))
        when-false #(is (= nil
                          (throw-excp)))]
    (testing "if-assert-cond"
      (binding [*assert-cond* true]
        (when-true))
      (binding [*assert-cond* false]
        (when-false)))
    (testing "with-assert-cond"
      (with-assert-cond true
        (when-true))
      (with-assert-cond false
        (when-false)))
    (testing "assert-cond-true"
      (assert-cond-true
        (when-true)))
    (testing "assert-cond-false"
      (assert-cond-false
        (when-false)))))


(deftest test-assertion-helpers
  (testing "verify"
    (is (= true (verify empty? [])))
    (is (thrown? IllegalArgumentException (verify empty? [10]))))
  (testing "assert-type"
    (is (= nil (assert-type "aa" String)))
    (is (thrown? AssertionError (assert-type true String)))))


(deftest test-keyword-string-conversion
  (testing "k-to-colname"
    (is= k-to-colname {:a "a" :a-b "a_b"}))
  (testing "colname-to-k"
    (is= colname-to-k {"a" :a "a_b" :a-b}))
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
  (testing "pojo-fn"
    (is (= (doall (map (pojo-fn "Hello") [[:char-at 0]     ; returns primitive char
                                          [:substring 3 4] ; returns string
                                          [:to-string]     ; no-arg method
                                          ]))
          [\H "l" "Hello"])))
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
  (testing "setter-fn"
    (let [sb (StringBuilder. "Hello")]
      (is (= (doall (map (setter-fn sb)
                      [[:length 4]     ; .setLength(4)      - returns void
                       [:char-at 0 \C] ; .setCharAt(0, 'C') - returns void
                       ]))
            [nil nil]))
      (is (= (.toString sb) "Cell"))))
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
    (testing "getter-fn"
      (is (= (doall (map (getter-fn lst) [:first ; .getFirst() - returns 1
                                          :last  ; .getLast()  - returns 2
                                          ]))
            [1 2])))))


(deftest test-properties
  (let [;;ps (read-properties "src/test/conf/dbconfig.properties")
        ps (java.util.Properties.)
        _  (setter (call-specs ps
                     [:property "a" "10"]
                     [:property "b" "20"]
                     [:property "c" "true"]))
        pm (property-map ps)
        km (strkey-to-keyword pm)]
    (testing "property-map"
      (is (= pm {"a" "10"
                 "b" "20"
                 "c" "true"})))
    (testing "strkey-to-keyword"
      (is (= km {:a "10"
                 :b "20"
                 :c "true"})))
    (testing "is-true?"
      (is (is-true? (:c km))))))


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
  (test-illegal-arg)
  (test-type-conversion)
  (test-not-prefixed)
  (test-condition-assertion)
  (test-assertion-helpers)
  (test-keyword-string-conversion)
  (test-reflection)
  (test-properties)
  (test-jndi))
