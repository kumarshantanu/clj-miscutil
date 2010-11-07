(ns org.bituf.clj-miscutil
  "Assortment of functions for carrying out miscellaneous activities."
  (:import (javax.naming Binding Context InitialContext))
  (:import (javax.naming NameClassPair NamingEnumeration))
  (:import (clojure.lang Reflector))
  (:use clojure.repl)
  (:use org.bituf.clj-miscutil.internal))


;; ===== Boolean values ======

(defn boolean?
  "Tell whether given value is a boolean"
  [x]
  (instance? Boolean x))


(defn not-boolean?
  "Tell whether given value is not a boolean"
  [x]
  (not (boolean? x)))


;; ===== Print tables =====

(def *pt-column-delim* " | ")

(def *pt-min-cols-width* (repeat 2))

(def *pt-max-cols-width* (repeat 200))


(defn calc-width
  "Calculate width for a collection of data rows"
  [data-rows]
  (let [col-count  (count (first data-rows))
        cols-width (atom
                     (into []
                       (take col-count (repeat 0))))]
    (doseq [each-row data-rows]
      (let [each-cols-width (map #(count (xlat-np-chars (str %))) each-row)]
        (reset! cols-width
          (->> each-cols-width
            (map max @cols-width)         ; find max real width
            (map min *pt-max-cols-width*) ; limit min/max col width
            (map max *pt-min-cols-width*)))))
    (into [] @cols-width)))


(defn fixed-width-str
  "Give fixed width string based on passed value and width. Alignment can
  either be specified as :left, :right or :center/:centre, or it is inferred
  from the given value."
  ([value width alignment]
    (let [xlt (xlat-np-chars (str value)) ; translated text
          len (count xlt)
          lhs (case alignment
                :right  (- width len)
                :centre (/ (- width len) 2)
                :center (/ (- width len) 2)
                0)
          rhs (case alignment
                :left   (- width len)
                :centre (/ (- width len) 2)
                :center (/ (- width len) 2)
                0)
          spc #(apply str (repeat % \ ))]
      (str (spc lhs) xlt (spc rhs))))
  ([value width]
    (let [alignment (cond
                      (number?  value)  :right
                      (boolean? value) :center
                      :else :left)]
      (fixed-width-str value width alignment))))


(defn print-cols
  "Print column values like a row in a table.
  Example:
    (print-cols [\"Nachos\" 1001 true] [10 5 5])"
  [cols cols-width]
  (let [fixed-width-tokens (map fixed-width-str cols cols-width)]
    (println (apply str (interpose *pt-column-delim* fixed-width-tokens)))))


(defn print-table-from-coll
  "Print data table from a collection of rows. Expected input is a collection
  of equi-count collections.
  Example:
    (print-table-from-coll [[1 2 3]
                            [4 5 6]])
    (print-table-from-coll [[\"James\" 2 3]
                            [\"Henry\" 5 6]] [10 4 4])"
  ([data-rows]
    (print-table-from-coll data-rows (calc-width data-rows)))
  ([data-rows cols-width]
    (doseq [each-row data-rows]
      (print-cols each-row cols-width))))


(defn print-table-with-header
  "Print table with header columns (i.e. column titles).
  Example:
    (print-table-from-coll [[1 2 3]
                            [4 5 6]] [\"Q1\" \"Q2\" \"Q3\"])
    (print-table-from-coll [[\"James\" 2 3]
                            [\"Henry\" 5 6]]
                           [\"Name\" \"Days#\" \"Score\"] [10 4 4])"
  ([data-rows header-cols]
    (let [all-rows  (cons header-cols data-rows)
          all-width (calc-width all-rows)]
      (print-table-with-header
        data-rows header-cols all-width)))
  ([data-rows header-cols cols-width]
    (let [sep-cols (map #(apply str (take % (repeat \-))) cols-width)
          all-rows (into [header-cols sep-cols] data-rows)]
      (print-table-from-coll all-rows cols-width))))


(defn print-table-from-maps
  "Print a collection of maps as a table; you must ensure all maps contain the
  same keys.
  Example:
    (print-table-from-maps [{:name \"John\"  :age 34 :regular true}
                            {:name \"Filip\" :age 23 :regular false}])
    (print-table-from-maps [{:name \"John\"  :age 34 :regular true}
                            {:name \"Filip\" :age 23 :regular false}]
                           [20 5 10])"
  ([data-map-rows]
    (let [header-cols (keys (first data-map-rows))
          find-vals   (fn [m] (map #(m %) header-cols))
          data-rows   (map find-vals data-map-rows)]
      (print-table-with-header data-rows header-cols)))
  ([data-map-rows cols-width]
    (let [header-cols (keys (first data-map-rows))
          find-vals   (fn [m] (map #(m %) header-cols))
          data-rows   (map find-vals data-map-rows)]
      (print-table-with-header data-rows header-cols cols-width))))


(defn print-table
  "Print a table from collections of rows or maps."
  ([data-rows]
    (if (map? (first data-rows)) (print-table-from-maps data-rows)
      (print-table-from-coll data-rows)))
  ([data-rows cols-width]
    (if (map? (first data-rows)) (print-table-from-maps data-rows cols-width)
      (print-table-from-coll data-rows cols-width))))


;; ===== Var metadata  =====

(defmacro var-name
  "Return name of the var"
  [v]
  `(or
     (try (:name (meta (resolve (quote ~v))))
       (catch Exception _# nil))
     (try (:name (meta (var ~v)))
       (catch Exception _# nil))
     (str ~v)))


(defn var-body
  "Return the body of the var"
  [v]
  (or
    (source v)
    (try (quote v)
      (catch Exception _# nil))))


;; ===== Exceptions =====

(defn illegal-arg
  "Throw IllegalArgumentException with specified arguments. Use this when you
   encounter bad/invalid parameters."
  [reason & more]
  (if (instance? Throwable reason)
    ;; then
    (if (empty? more)
      (throw (IllegalArgumentException. reason))
      (throw (IllegalArgumentException. (apply str more) reason)))
    ;; else
    (throw (IllegalArgumentException. (apply str reason more)))))


(defn illegal-state
  "Throw IllegalStateException with specified arguments. Use this when you
   encounter bad/invalid state while running the program."
  [reason & more]
  (if (instance? Throwable reason)
    ;; then
    (if (empty? more)
      (throw (IllegalStateException. reason))
      (throw (IllegalStateException. (apply str more) reason)))
    ;; else
    (throw (IllegalStateException. (apply str reason more)))))


(defn unsupported-op
  "Throw UnsupportedOperationException with specified arguments. Use this when
  you do not support an operation."
  [reason & more]
  (if (instance? Throwable reason)
    ;; then
    (if (empty? more)
      (throw (UnsupportedOperationException. reason))
      (throw (UnsupportedOperationException. (apply str more) reason)))
    ;; else
    (throw (UnsupportedOperationException. (apply str reason more)))))


;; ===== Type conversion =====

(defn as-string
  "Convert given argument to string.
  Example:
    (str       \":one\") ; returns \":one\"
    (as-string \":one\") ; returns \"one\"
  See also: as-vstr"
  [x]
  (if (or (keyword? x) (symbol? x)) (name x)
    (str x)))


(defn as-vstr
  "Convert to verbose string - useful for diagnostics and error messages. Like
  as-string, but distinguishes nil as \"<nil>\". 
  Example:
    (as-string  nil) ; returns \"\"
    (as-vstr    nil) ; returns \"<nil>\"
  See also: as-string"
  [x]
  (if-let [y x] (as-string y) "<nil>"))


(defn as-vector
  "Convert/wrap given argument as a vector"
  [anything]
  (if (vector? anything) anything
    (if (or (seq? anything) (set? anything)) (into [] anything)
      (if (map? anything) (into [] (vals anything))
        (if (nil? anything) []
          [anything])))))


(defn as-set
  "Convert/wrap given argument as a set"
  [anything]
  (if (set? anything) anything
    (if (or (seq? anything) (vector? anything)) (into #{} anything)
      (if (map? anything) (into #{} (vals anything))
        (if (nil? anything) #{}
          #{anything})))))


(defn as-map
  "Convert given collection as a map"
  [coll]
  (if (map? coll) coll
    (if (and (coll? coll) (even? (count coll))) (apply array-map coll)
      (if (nil? coll) {}
        (illegal-arg
          (str "Expected collection with size a multiple of 2, but found "
            (type coll) ": " (as-vstr coll)))))))


;; ===== "not" stripped functions =====

(defn any?                  [x] (not (not-any?          x)))


;; ===== "not" prefixed functions =====

(defn not-associative?      [x] (not (associative?      x)))
(defn not-bound?            [x] (not (bound?            x)))
(defn not-char?             [x] (not (char?             x)))
(defn not-chunked-seq?      [x] (not (chunked-seq?      x)))
(defn not-class?            [x] (not (class?            x)))
(defn not-coll?             [x] (not (coll?             x)))
(defn not-contains?         [x] (not (contains?         x)))
(defn not-counted?          [x] (not (counted?          x)))
(defn not-decimal?          [x] (not (decimal?          x)))
(defn not-delay?            [x] (not (delay?            x)))
(defn not-distinct?         [x] (not (distinct?         x)))
(defn not-empty?            [x] (not (empty?            x)))
(defn not-even?             [x] (not (even?             x)))
(defn not-extends?          [x] (not (extends?          x)))
(defn not-false?            [x] (not (false?            x)))
(defn not-float?            [x] (not (float?            x)))
(defn not-fn?               [x] (not (fn?               x)))
(defn not-future-cancelled? [x] (not (future-cancelled? x)))
(defn not-future-done?      [x] (not (future-done?      x)))
(defn not-future?           [x] (not (future?           x)))
(defn not-identical?        [x] (not (identical?        x)))
(defn not-ifn?              [x] (not (ifn?              x)))
(defn not-instance?         [x] (not (instance?         x)))
(defn not-integer?          [x] (not (integer?          x)))
(defn not-isa?              [x] (not (isa?              x)))
(defn not-keyword?          [x] (not (keyword?          x)))
(defn not-list?             [x] (not (list?             x)))
(defn not-map?              [x] (not (map?              x)))
(defn not-neg?              [x] (not (neg?              x)))
(defn not-nil?              [x] (not (nil?              x)))
(defn not-number?           [x] (not (number?           x)))
(defn not-odd?              [x] (not (odd?              x)))
(defn not-pos?              [x] (not (pos?              x)))
(defn not-ratio?            [x] (not (ratio?            x)))
(defn not-rational?         [x] (not (rational?         x)))
(defn not-reversible?       [x] (not (reversible?       x)))
(defn not-satisfies?        [x] (not (satisfies?        x)))
(defn not-seq?              [x] (not (seq?              x)))
(defn not-sequential?       [x] (not (sequential?       x)))
(defn not-set?              [x] (not (set?              x)))
(defn not-sorted?           [x] (not (sorted?           x)))
(defn not-special-symbol?   [x] (not (special-symbol?   x)))
(defn not-string?           [x] (not (string?           x)))
(defn not-symbol?           [x] (not (symbol?           x)))
(defn not-thread-bound?     [x] (not (thread-bound?     x)))
(defn not-true?             [x] (not (true?             x)))
(defn not-var?              [x] (not (var?              x)))
(defn not-vector?           [x] (not (vector?           x)))
(defn not-zero?             [x] (not (zero?             x)))


;; ===== Array types =====

(defn array-type
  "Return array type"
  [obj]
  (if (nil? obj) nil
    (.getComponentType (class obj))))


(defn array?
  "Tell whether given object is an array"
  ([obj]
    (if (nil? obj) false
      (not-nil? (array-type obj))))
  ([obj elem-type]
    (if (or (nil? obj) (nil? elem-type)) false
      (= (as-string elem-type) (as-string (array-type obj))))))


;; ===== includes? -- replacement for contains? =====

(defn includes?
  "Like 'contains?', but works for indexed collections (e.g. vectors, arrays)
  too"
  [coll needle]
  (let [indexless-coll (if (or (vector? coll) (array? coll)) (as-set coll)
                         coll)]
    (contains? indexless-coll needle)))


;; ===== Argument/condition assertion =====

(defmacro when-assert-cond
  "Execute body if *assert-cond* flag is true. Function implemention may
  use this macro to conditionally assert args or return value or any condition
  within the code. Function consumers can instead use either of:
  with-assert-cond
  assert-cond-true
  assert-cond-false"
  [& body]
  `(when *assert-cond*
    ~@body))


(defmacro with-assert-cond
  "Execute body of code in a given assert-cond context (true/false). You may
  like to use 'false' in production mode:
    (with-assert-cond false
      ..)"
  [bool & body]
  `(binding [*assert-cond* ~bool]
    ~@body))


(defmacro assert-cond-false
  "Short for
    (with-assert-cond false
      ..)
  See also: with-assert-cond"
  [& body]
  `(with-assert-cond false
    ~@body))


(defmacro assert-cond-true
  "Short for
    (with-assert-cond true
      ..)
  See also: with-assert-cond"
  [& body]
  `(with-assert-cond true
    ~@body))


;; ===== Stack trace and Exceptions =====


(defn stacktrace-rows
  "Given an array or collection of StackTraceElement objects, return a vector
  of [file-name line-number class-name method-name] objects."
  ([stack-trace]
    (map #(let [class-name  (or (.getClassName  ^StackTraceElement %) "")
                method-name (or (.getMethodName ^StackTraceElement %) "")
                file-name   (or (.getFileName   ^StackTraceElement %) "")
                line-number (.getLineNumber ^StackTraceElement %)]
            [file-name line-number class-name method-name
             (str "at " class-name "(" file-name ":" line-number ")")])
      (into [] stack-trace)))
  ([]
    (stacktrace-rows (.getStackTrace (Thread/currentThread)))))


(defn clj-stacktrace-row?
  "Tell (true/false) whether given stack trace row is Clojure-specific"
  ([[file-name line-number class-name method-name]
    classname-begin-tokens classname-not-begin-tokens]
    (and (.contains file-name ".clj")
      (or (empty? classname-begin-tokens)
        (some #(.startsWith class-name %)
          classname-begin-tokens))
      (every? #(not (.startsWith class-name %))
        classname-not-begin-tokens)))
  ([row]
    (clj-stacktrace-row? row [] [])))


(defn app-stacktrace-row?
  "Tell (true or false) whether given stack trace row is app-specific"
  ([row]
    (clj-stacktrace-row? row [] ["clojure."]))
  ([row classname-begin-tokens classname-not-begin-tokens]
    (clj-stacktrace-row? row classname-begin-tokens
      classname-not-begin-tokens)))


(defn print-first-nonempty-stacktrace
  [& stacktrace-rows]
  (print-table-with-header
    (first (filter not-empty? stacktrace-rows))
    ["File" "Line#" "Class" "Method" "IDE Reference"]))


(defn print-stacktrace
  "Print first non-empty stack trace from the following:
  1. Application specific
  2. Clojure specific
  3. Entire stack trace"
  ([stack-trace classname-begin-tokens classname-not-begin-tokens]
    (let [all-stacktrace (stacktrace-rows stack-trace)
          clj-stacktrace (filter clj-stacktrace-row? all-stacktrace)
          app-stacktrace (filter #(app-stacktrace-row? % classname-begin-tokens
                                    classname-not-begin-tokens)
                           clj-stacktrace)]
      (print-first-nonempty-stacktrace
        app-stacktrace
        clj-stacktrace
        all-stacktrace)))
  ([stack-trace]
    (let [all-stacktrace (stacktrace-rows stack-trace)
          clj-stacktrace (filter clj-stacktrace-row? all-stacktrace)
          app-stacktrace (filter app-stacktrace-row? clj-stacktrace)]
      (print-first-nonempty-stacktrace
        app-stacktrace
        clj-stacktrace
        all-stacktrace)))
  ([]
    (print-stacktrace (stacktrace-rows))))


(defn print-exception-stacktrace
  "Print stack trace for a given exception"
  [^Throwable t]
  (binding [*out* *err*]
    (println (str t))
    (loop [th t]
      (print-stacktrace (.getStackTrace th))
      (let [cause (.getCause th)]
        (when (not-nil? cause)
          (println "Caused by: " (str cause))
          (recur cause))))))


(defmacro !
  "Execute a body of code in try-catch block, and report friendly stack trace"
  [& body]
  `(try ~@body
     (catch Exception e#
       (print-exception-stacktrace e#)
       (.printStackTrace e#))))


;; ===== Assertion helpers =====

(defn csv
  "Command separated values"
  [coll]
  (interpose ", " coll))


;(defn verify2
;  "Apply f? (must return Boolean) to args - return true when asserted true,
;  throw exception otherwise."
;  [f? & args]
;  (do
;    (assert (fn? f?))
;    (if (apply f? args) true
;      (illegal-arg
;        "Invalid argument(s) " (apply str (csv (map as-vstr args)))
;        " (Expected: " (or
;                         (try (source f?)
;                           (catch Exception _# nil))
;                         (try (:name (meta (resolve (quote f?))))
;                           (catch Exception _# nil))
;                         (meta f?))
;        ", Found: " (apply str (csv (map #(as-vstr (type %)) args))) ")"
;        ;(Thread/dumpStack)
;        (print-stacktrace)))))


(defmacro verify
  "Apply f? (must return Boolean) to args - return true when asserted true,
  throw exception otherwise."
  [f? & many-args]
  `(let [args# (vector ~@many-args)]
    (assert (fn? ~f?))
    (if (apply ~f? args#) true
      (illegal-arg
        "Invalid argument " (apply str (csv (map as-vstr args#)))
        " (Expected: " (or
                         (try (source ~f?)
                           (catch Exception _# nil))
                         (try (:name (meta (resolve (quote ~f?))))
                           (catch Exception _# nil))
                         (meta ~f?))
        ", Found: " (apply str (csv (map #(as-vstr (type %)) args#))) ")"))))


(defn assert-type
  [item expected-type]
  (assert (not-nil? item))
  (assert (instance? Class expected-type))
  (try
    (assert (isa? (type item) expected-type))
    (catch AssertionError e
      (throw (AssertionError. (str "Expected " expected-type " but found "
                                (type item)))))))


;; ===== Type annotation =====

(defn implied-types
  "Get implied types for a given object type.
  Example:
    user=> (implied-types :employee)
    [:salaried :person]
  See also: with-implied-types"
  [obj-type]
  (let [types (*implied-types* obj-type)]
    types))


(defmacro with-implied-types
  "Run a body of code in the context of a given implied types map.
  Example:
    user=> (with-implied-types {:employee [:salaried :person]
                                :salaried [:person]}
             (let [ravi (typed {:name \"Ravi\" :empid 7784}
                          :employee)]
               (typed? ravi :person)))
    true"
  [all-implied-types & body]
  `(binding [*implied-types* ~all-implied-types]
     ~@body))


(defn obj?
  "Tell whether Clojure object (IObj)"
  [obj]
  (instance? clojure.lang.IObj obj))


(defn- mdata-types
  "Return meta data (map) and types (set) the object belongs to"
  [obj]
  (let [mdata (or (meta obj) {})
        types (types-keyword mdata)]
    [mdata types]))


(defn type-meta
  "Return set of types the given object belongs to"
  [obj]
  (let [[_ types] (mdata-types obj)]
    (or types #{})))


(defn typed
  "Annotate given object with specified types"
  [obj type-1 & more]
  (let [of-types (into [type-1] more)]
    (when-assert-cond
      (verify obj? obj)
      (doseq [each-type of-types]
        (verify keyword? each-type)))
    (let [[mdata types] (mdata-types obj)]
      (with-meta obj
        (into mdata
          {types-keyword (or (and types (apply conj types of-types))
                           (into #{} of-types))} )))))


(defn typed?
  "Tell whether a given object is of a certain type.
  See also: typed"
  ([f obj type-1 & more]
    (let [of-types (into [type-1] more)]
      (when-assert-cond
        (verify fn? f)
        (doseq [each-type of-types] (verify keyword? each-type)))
      (and (obj? obj)
        (let [[mdata types] (mdata-types obj)
              all-types     (into types
                              (flatten (map #(implied-types %) types)))]
          (f #(contains? all-types %) of-types)))))
  ([obj type-1]
    (typed? every? obj type-1)))


(defn typed-every?
  "Call typed? on obj for every type -- (every? #(typed? obj %) types)"
  [obj type-1 & more]
  (apply typed? every? obj (into [type-1] more)))


(defn typed-some?
  "Call typed? on obj for at least one type -- (some #(typed? obj %) types)"
  [obj type-1 & more]
  (apply typed? some obj (into [type-1] more)))


(defn wrap
  "Given anything (such as numeric, string, keyword etc. -- presumably that
  cannot be annotated with meta data), return a no-arg function that when
  executed returns the original value.
  Example:
    (let [x (wrap 10)]
      (x))"
  [v]
  (fn [& xs] (or (and xs (apply v xs)) v)))


;; ===== Keyword to/from string conversion =====

(defn k-to-colname
  "Convert a keyword to database column name (string) and replace dash with
  underscore."
  [k]
  (let [n (name k)
        s (.replace n "-" "_")]
    s))


(defn colname-to-k
  "Convert database column name (string) to keyword after replacing underscore
  with dash."
  [s]
  (let [n (.replace s "_" "-")]
    (keyword n)))


(defn k-to-camelstr
  "Convert keyword to camel-case string and treat dash as case-changer.
  :-            --> \"\"
  :to-do        --> \"toDo\"       ; dash triggers upper-case
  :to_do        --> \"to_do\"      ; underscore stays intact
  :to-do-       --> \"toDo\"       ; trailing dash is ignored
  :-from-here   --> \"fromHere\"   ; leading dash is ignored too
  :hello--there --> \"helloThere\" ; consecutive dashes are treated as one"
  [k]
  (let [s (name k)
        tokens (filter not-empty? (into [] (.split s "-")))
        ;; ucase1 converts first character to upper case
        ucase1 #(str (Character/toUpperCase (first %))
                  (apply str (rest %)))
        lcase  #(if (not-nil? %) (.toLowerCase %))
        cctoks (map ucase1 tokens)]
    (apply str (lcase (first cctoks)) (rest cctoks))))


(defn camelstr-to-k
  "Given a camel-case string, convert it into a dash-delimited keyword. Upper
  case character triggers insertion of the dash."
  [cs]
  (let [b (StringBuilder.)
        f #(do
             (if (and (Character/isUpperCase %) (not (empty? b)))
               (.append b \-))
             (.append b (Character/toLowerCase %)))
        _ (doall (map f cs))
        a (filter not-empty? (into [] (.split (.toString b) "-")))
        s (apply str (interpose \- a))]
    (keyword s)))


(defn k-to-methodname
  "Given a keyword and a bunch of method-name prefixes (collection of string),
  construct the method name (string). When called with only a keyword as an
  argument, k-to-methodname behaves like k-to-camelstr.
  See also: k-to-camelstr, camelstr-to-k"
  ([k prefixes]
    (let [s (name (camelstr-to-k (name k)))
          n (if (some #(.startsWith s (str % \-)) prefixes) s
              (str (first prefixes) \- s))]
      (k-to-camelstr (keyword n))))
  ([k]
    (k-to-methodname k [""])))


(defn k-to-setter [k] (k-to-methodname k ["set"]))
(defn k-to-getter [k] (k-to-methodname k ["get" "is"]))


;; ===== Reflection (not recommended for performance-critical code) =====


(defn call-specs
  "Accept a common target object, one or more method specs and turn them into
  call specs. A call spec looks like: [target-object method-spec] and a method
  spec looks like: [method-key & args]
  Example:
    (call-specs \"Hello\" [:char-at 0] [:substring 3 4])
    ;; returns
    [[\"Hello\" :char-at 0]
     [\"Hello\" :substring 3 4]]
    ;; another example - no need to wrap no-arg methods into a vector
    (call-specs \"Hello\" [:char-at 0] :to-string)
    ;; returns
    [[\"Hello\" :char-at 0]
     [\"Hello\" :to-string]]"
  [target method-spec & more-method-specs]
  (let [method-specs (into [method-spec] more-method-specs)]
    (into [] (map #(into [] (cons target (as-vector %))) method-specs))))


(defn method
  "Call instance method on the target object. Wrapper for
  Reflector/invokeInstanceMethod (see link):
  http://github.com/richhickey/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
  Short link: http://j.mp/a2Kd9R
  Examples:
    (call-method \"Hello\" :char-at 0)     ; returns \\H
    (call-method \"Hello\" :substring 3 4) ; returns \"l\"
    ;; the call below returns [\\H \"l\"]
    (call-method [[\"Hello\" :char-at 0]
                  [\"Hello\" :substring 3 4]])
    ;; same call as above expressed using target-method-specs
    (call-method (target-method-specs \"Hello\"
                   [:char-at 0]
                   [:substring 3 4]))"
  ([target method-name & args]
    (Reflector/invokeInstanceMethod
      ;; Object target, String methodName, Object[] args
      target
      (if (keyword? method-name) (k-to-methodname method-name)
        (as-string method-name))
      (into-array Object args)))
  ([call-specs]
    (into [] (map #(apply method %) call-specs))))


(defn pojo-fn
  "Wrap a Plain Old Java Object (POJO) into a function that accepts a
  method spec and invokes the method upon execution."
  ([pojo]
    (fn [method-spec]
      (let [[method-name & args] (as-vector method-spec)]
        (apply method pojo method-name args))))
  ([pojo method-name & args]
    (fn [& more-args]
      (apply method pojo method-name (into [] (concat args more-args))))))


(defn setter
  "Call setter method on a target object using args. 'setter' is either a
  keyword or a string.
  Examples:
    (call-setter obj :price 67.88)   ; .setPrice(67.88)   - returns 67.88
    (call-setter obj :min-max 30 75) ; .setMinMax(30, 75) - returns void
    (call-setter obj :auto-commit)   ; .setAutoCommit()   - returns true
    ;; same stuff in a single call - returns [67.88 nil true]
    (call-setter [[obj :price 67.88]
                  [obj :min-max 30 75]
                  [obj :auto-commit]])
    ;; same stuff without repeating the target object - returns [67.88 nil true]
         (call-setter (call-specs obj [[:price 67.88]
                                       [:min-max 30 75]
                                       [:auto-commit]]))"
  ([target setter-name & args]
    (apply method
      target (if (keyword? setter-name) (k-to-setter setter-name)
               setter-name) args))
  ([setter-specs]
    (into [] (map #(apply setter %) setter-specs))))


(defn setter-fn
  "Wrap a Plain Old Java Object (POJO) into a function that accepts a setter
  method spec and invokes the method upon execution."
  ([pojo]
    (fn [method-spec]
      (let [[method-name & args] (as-vector method-spec)]
        (apply method pojo (k-to-setter method-name) args))))
  ([pojo method-name & args]
    (fn [& more-args]
      (apply method pojo (k-to-setter method-name)
        (into [] (concat args more-args))))))


(defn getter
  "Call getter method on a target object. 'getter-name' is either a keyword
  or a string.
  Example:
    (getter obj :price)        ; .getPrice()    - returns 566.89
    (getter obj :item-code)    ; .getItemCode() - returns 634
    (getter obj :is-available) ; .isAvailable() - returns true
    ;; same calls merged into one
    (getter [[obj :price]
             [obj :item-code]
             [obj :is-available]])
    ;; or a shortcut - returns [566.89 634 true]
    (getter (call-specs obj [:price] [:item-code] [:is-available]))
    ;; even shorter
    (getter (call-specs obj :price :item-code :is-available))"
  ([target getter-name & args]
    (apply method
      target (if (keyword? getter-name) (k-to-getter getter-name)
               getter-name) args))
  ([getter-specs]
    (into [] (map #(apply getter %) getter-specs))))


(defn getter-fn
  "Wrap a Plain Old Java Object (POJO) into a function that accepts a getter
  method spec and invokes the method upon execution.
  Example:
    ;; assuming a Person class having getters getName, getAddress and getEmail
    (map (getter-fn person) [:name :address :email])
"
  ([pojo]
    (fn [method-spec]
      (let [[method-name & args] (as-vector method-spec)]
        (apply method pojo (k-to-getter method-name) args))))
  ([pojo method-name & args]
    (fn [& more-args]
      (apply method pojo (k-to-getter method-name)
        (into [] (concat args more-args))))))


;; ===== Properties handling =====

(defn property-map
  "Transform a given Properties instance to a map."
  [^java.util.Properties properties]
  (let [ks (into [] (.stringPropertyNames properties))
        vs (into [] (map #(.getProperty properties %) ks))]
    (zipmap ks vs)))


(defn strkey-to-keyword
  "Given a map with every key a string, convert keys to keywords.
  Input: {\"a\" 10 \"b\" \"20\"}
  Returns: {:a 10 :b \"20\"}"
  [m]
  (assert (map? m))
  (into {} (map #(let [k (first %)]
                   (assert (string? k))
                   [(keyword k) (last %)]) (seq m))))


(defn is-true?
  "Tell whether a given value is equivalent to true."
  [any]
  (if (string? any)
    (let [v (.toLowerCase any)]
      (or
        (= "true" v)
        (= "yes"  v)
        (= "on"   v)))
    (if (number? any)
      (> any 0)
      (true? any))))


;; ===== JNDI functions (tree-printing not recommended for production use) =====

(defmacro with-root-context
  [root-context & body]
  `(do
    (assert (not (nil? ~root-context)))
    (assert (instance? Context ~root-context))
    (binding [*root-context* ~root-context]
      ~@body)))


(defn- increase-indent []
  (swap! *indent* #(+ % 4)))


(defn- decrease-indent []
  (swap! *indent* #(- % 4)))


(defn- print-entry
  [^NameClassPair next-elem]
  (let [indent-str (apply str
                     (take @*indent* (repeat " ")))]
    (if (nil? next-elem) (println indent-str "--> <nil>")
      (println indent-str "-->"
        (.getName next-elem)
        " (" (type next-elem) "->" (.getClassName next-elem) ")"))))


(declare do-print-jndi-tree)


(defn- print-ne
  [^NamingEnumeration ne ^String parent-ctx]
  (loop []
    (when (.hasMoreElements ne)
      (let [next-elem (.nextElement ne)]
        (print-entry next-elem)
        (increase-indent)
        (if (or (instance? Context next-elem)
              (and (instance? NameClassPair next-elem)
                (instance? Context (.getObject next-elem))))
          (do-print-jndi-tree
            (if (zero? (.length parent-ctx))
              (.getName next-elem)
              (str parent-ctx "/" (.getName next-elem))))
          (println "** Not drilling "
            (type (.getObject next-elem))))
        (decrease-indent))
      (recur))))


(defn- do-print-jndi-tree
  [^String ct]
  (assert (not (nil? ct)))
  (if (instance? Context *root-context*)
    (print-ne (.list *root-context* ct) ct)
    (print-entry *root-context*)))


(defn print-jndi-tree
  ([^String ct]
    (binding [*indent* (atom 0)]
      (do-print-jndi-tree ct)))
  ([]
   (print-jndi-tree "")))


(defn jndi-lookup
  ([^Context context k]
    (.lookup context k))
  ([k]
    (jndi-lookup (InitialContext.) k)))


(defn find-jndi-subcontext
  "Find subcontext in a given JNDI context.
  context  JNDI context
  args     string keys"
  [^Context context & args]
  (assert (not (nil? args)))
  (assert (not (some nil? args)))
  (let [lookup-fn (fn [ctx k] (.lookup ctx k))
        new-ctx (reduce lookup-fn context args)]
    new-ctx))
