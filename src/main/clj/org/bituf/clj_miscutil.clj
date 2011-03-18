(ns org.bituf.clj-miscutil
  "Assortment of functions for carrying out miscellaneous activities."
  (:import
    (java.io      File PrintWriter StringWriter InputStream Reader)
    (java.net     URL)
    (java.util    Collection List Map Properties)
    (javax.naming Binding Context InitialContext
                  NameClassPair NamingEnumeration)
    (clojure.lang Reflector))
  (:require
    [clojure.string :as sr]
    [clojure.pprint :as pp]
    [clojure.repl   :as repl]
    [org.bituf.clj-miscutil.internal :as in]))


;; ===== Declarations ======


(declare as-vstr)


;; ===== Random values ======


(defn random-number
  "Generate a random number. If a min-value/max-value range is specified a
  random double-precision number from that range is returned. Note that
  min-value is inclusive and max-value is exclusive."
  ([]
    (Math/abs (.nextLong in/random-seed)))
  ([min-value max-value] {:post [(number? %)]
                          :pre  [(number? min-value)
                                 (number? max-value)
                                 (>= max-value min-value)]}
    (let [diff (- max-value min-value)]
      (+ min-value (* (Math/random) diff)))))


(declare random-string)


(defn random-charseq
  "Return a lazy (infinite!, or length n if supplied) sequence of random
  alphanumeric characters."
  ([] {:post [(seq? %)]}
    (let [f (fn thisfn []
              (cons (seq (random-string)) (lazy-seq (thisfn))))]
      (flatten (f))))
  ([n] {:pre [pos? n]}
    (take n (random-charseq))))


(defn random-string
  "Generate a random string. Return a string of len characters if specified."
  ([] {:post [(string? %)]}
    (Long/toString (random-number) 36))
  ([len] {:post [(string? %)]
          :pre  [(pos? len)]}
    (apply str (random-charseq len))))


;; ===== Type check =====


(defn boolean?
  "Return true if given value is a boolean, false otherwise."
  [x]
  (instance? Boolean x))


(defn not-boolean?
  "Return true if given value is not a boolean, false otherwise."
  [x]
  (not (boolean? x)))


(defn date?
  "Return true if given value is a java.util.Date, false otherwise."
  [d]
  (instance? java.util.Date d))


(defn not-date?
  "Return true if given value is not a java.util.Date, false otherwise."
  [d]
  (not (date? d)))


;; ===== Pretty printing =====


(defmacro with-stringwriter
  "Create a StringWriter and make it available in a let binding; execute body
  of code in the let context and finally return the string from the writer."
  [s & body]
  `(let [~s (StringWriter.)]
     (do ~@body)
     (.toString ~s)))


(defmacro printwriter-str
  "Capture the output to specified device by executing body of code and return
  as a string."
  [^PrintWriter out & body]
  `(let [sw# (StringWriter.)
         pw# (PrintWriter. sw#)]
     (binding [~out pw#]
       (do ~@body)
       (.toString sw#))))


(defmacro with-err-str
  "Capture the output to *err* by executing body of code and return as a string."
  [& body]
  `(printwriter-str *err* ~@body))


(defn pprint-str
  "Pretty-print anything"
  [whatever]
  (with-stringwriter w
    (pp/pprint whatever w)))


(defn comma-sep-str
  "Return comma separated string for a given collection of values."
  [coll]
  (apply str (interpose ", " coll)))


(defn pprint
  "Invoke clojure.pprint/pprint on all args one by one."
  [x & more]
  (doseq [each (into [x] more)]
    (pp/pprint each)))


(defn echo
  "Print argument using 'pprint' and then return it."
  [x]
  (pp/pprint x)
  x)


;; ===== Numbers detection =====


(defn zeronum?
  "Safe version of zero? - return true if n is zero, false otherwise."
  [n]
  (and (number? n) (zero? n)))


(defn posnum?
  "Safe version of pos? - return true if n is a positive number, false otherwise."
  [n]
  (and (number? n) (pos? n)))


(defn negnum?
  "Safe version of neg? - return true if n is a negative number, false otherwise."
  [n]
  (and (number? n) (neg? n)))


;; ===== Print tables =====

(def ^{:doc "Delimiter string to be printed between columns"
       :dynamic true}
      *pt-column-delim* " | ")

(def ^{:doc "Minimum width for each column; effective only when computing width"
       :dynamic true}
      *pt-min-cols-width* (repeat 2))

(def ^{:doc "Maximum width for each column; effective only when computing width"
       :dynamic true}
      *pt-max-cols-width* (repeat 200))

(def ^{:doc "Default width for each column; non-positive number implies COMPUTE"
       :dynamic true}
      *pt-cols-width* (repeat -1))


(defn compute-width
  "Compute each column width for a collection of data rows; return result as a
  collection."
  [data-rows]
  (when (coll? (try
                 (first (first data-rows))
                 (catch Exception e nil)))
    (println "WARNING: Table data may not be in correct format"))
  (let [col-count (count (first data-rows))
        default-w (take col-count *pt-cols-width*)]
    (if (every? posnum? default-w) default-w
      ;; else compute
      (let [sp-indices (keep-indexed #(if (posnum? %2) %1) default-w) ; indices of specified width values
            as-v       #(if (coll? %) (into [] %) [%])
            cols-width (atom (as-v (map #(if (posnum? %) % 0) default-w)))
            compute-fn (fn [idx v]
                         (if (.contains ^List sp-indices idx) (nth default-w idx)
                           ;; else compute, really this time
                           (->> v
                             (max (nth @cols-width idx))
                             (min (nth *pt-max-cols-width* idx))
                             (max (nth *pt-min-cols-width* idx)))))]
        (doseq [each-row data-rows]
          (let [each-cols-width (as-v (map #(count (in/xlat-np-chars (str %)))
                                         each-row))
                computed-width  (as-v (map-indexed compute-fn each-cols-width))]
            (reset! cols-width computed-width)))
        (into [] @cols-width)))))


(defn fixed-width-str
  "Return fixed width string based on passed value and width. Alignment can
  either be specified as :left, :right or :center/:centre; if unspecified, it
  is inferred from the given value."
  ([value width alignment]
    (let [xlt (in/xlat-np-chars (str value)) ; translated text
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
  of equi-count data cols (a list/vector each).
  Example:
    (print-table-from-coll [[1 2 3]
                            [4 5 6]])
    (print-table-from-coll [[\"James\" 2 3]
                            [\"Henry\" 5 6]] [10 4 4])"
  [data-rows]
  (let [cols-width (compute-width data-rows)]
    (doseq [each-row data-rows]
      (print-cols each-row cols-width))))


(defn print-table-with-header
  "Print table with header columns (i.e. column titles). Accept collection of
  equi-count data cols (a list/vector each).
  Example:
    (print-table-with-header [[1 2 3]
                              [4 5 6]] [\"Q1\" \"Q2\" \"Q3\"])
    (print-table-with-header [[\"James\" 2 3]
                              [\"Henry\" 5 6]]
                             [\"Name\" \"Days#\" \"Score\"] [10 4 4])"
  [header-cols data-rows]
  (let [all-rows   (cons header-cols data-rows)
        all-width  (compute-width all-rows)
        sep-cols   (doall (map #(apply str (take % (repeat \-))) all-width))
        final-rows (into [header-cols sep-cols] data-rows)]
    (doseq [each-row final-rows]
      (print-cols each-row all-width))))


(defn print-table-from-maps
  "Print a collection of maps as a table; you must ensure all maps contain the
  same keys.
  Example:
    (print-table-from-maps [{:name \"John\"  :age 34 :regular true}
                            {:name \"Filip\" :age 23 :regular false}])
    (print-table-from-maps [{:name \"John\"  :age 34 :regular true}
                            {:name \"Filip\" :age 23 :regular false}]
                           [20 5 10])"
  [data-map-rows]
  (let [header-cols (keys (first data-map-rows))
        find-vals   (fn [m] (doall (map #(m %) header-cols)))
        data-rows   (map find-vals data-map-rows)]
    (print-table-with-header header-cols data-rows)))


(defn print-table
  "Print a table from collections of rows or maps.
  Example:
    (print-table [[10 20] [30 40]])             ; print table without header
    (print-table [{:a 10 :b 20} {:a 30 :b 40}]) ; print table with header
    (print-table [:a :b] [[10 20] [30 40]])     ; print table with header"
  ([data-rows]
    (if (map? (first data-rows)) (print-table-from-maps data-rows)
      (print-table-from-coll data-rows)))
  ([header-cols data-rows]
    (let [data-coll (if (map? (first data-rows)) (doall (map vals data-rows))
                      data-rows)]
      (print-table-with-header header-cols data-coll))))


;; ===== Var metadata  =====

(defmacro var-name
  "Return name of the given var as a string."
  [v]
  `(or
     (try (str (:name (meta (resolve (quote ~v)))))
       (catch Exception _# nil))
     (try (str (:name (meta (var ~v))))
       (catch Exception _# nil))
     (str ~v)))


(defn var-body
  "Return the body of the var."
  [v]
  (or
    (with-out-str (repl/source v))
    (try (str (quote v))
      (catch Exception _# nil))))


;; ===== Throwing exceptions =====

(defn val-dump
  "Return type and value of v in string form for diagnosis."
  [v]
  (if (nil? v) "<nil>\n"
    (format "(%s) %s"
      (as-vstr (type v))
      (with-out-str
        (pp/pprint v)))))


(defn illegal-arg-wrap
  "Throw IllegalArgumentException with specified arguments. Use this when you
   encounter bad/invalid parameters due to another exception."
  [^Throwable t ^String reason & more] {:pre [(instance? Throwable t)
                                              (string? reason)]}
    (if (empty? more)
      (throw (IllegalArgumentException. reason))
      (throw (IllegalArgumentException. ^String (apply str more) reason))))


(defn illegal-arg
  "Throw IllegalArgumentException with specified arguments. Use this when you
   encounter bad/invalid parameters."
  [^String reason & more] {:pre [(string? reason)]}
  (if (instance? Throwable reason)
    (apply illegal-arg-wrap reason more)
    (throw (IllegalArgumentException. ^String (apply str reason more)))))


(defn illegal-argval
  "Construct IllegalArgumentException for unmatched argument value"
  [arg-name expected found-value]
  (illegal-arg (format "Invalid argument '%s' - Expected: %s, Found: %s"
                 arg-name expected (val-dump found-value))))


(defn illegal-state-wrap
  "Throw IllegalStateException with specified arguments. Use this when you
   encounter bad/invalid state while running the program."
  [^Throwable t ^String reason & more] {:pre [(instance? Throwable t)
                                              (string? reason)]}
  (if (empty? more)
    (throw (IllegalStateException. reason))
    (throw (IllegalStateException. ^String (apply str more) reason))))


(defn illegal-state
  "Throw IllegalStateException with specified arguments. Use this when you
   encounter bad/invalid state while running the program."
  [^String reason & more] {:pre [(string? reason)]}
  (if (instance? Throwable reason)
    (apply illegal-state-wrap reason more)
    (throw (IllegalStateException. ^String (apply str reason more)))))


(defn unsupported-op-wrap
  "Throw UnsupportedOperationException with specified arguments. Use this when
  you do not support an operation due to an underlying exception."
  [^Throwable t ^String reason & more] {:pre [(instance? Throwable t)
                                              (string? reason)]}
  (if (empty? more)
    (throw (UnsupportedOperationException. reason))
    (throw (UnsupportedOperationException. ^String (apply str more) reason))))


(defn unsupported-op
  "Throw UnsupportedOperationException with specified arguments. Use this when
  you do not support an operation."
  [^String reason & more] {:pre [(string? reason)]}
  (throw (UnsupportedOperationException. ^String (apply str reason more))))


;; ===== Non-breaking error handling =====


(defmacro maybe
  "Execute given body of code in a try-catch block and return a 2-element
  vector [value exception], such that if the body of code returns a value
  without throwing any exception then the vector contains [value nil],
  otherwise (in event of an exception) the vector contains [nil exception].
  Example:
    (defn valid-name [name]
      (if (and (string? name)
            (not (empty? name)))
        name
        (throw (IllegalArgumentException. \"Bad name\"))))
    ;; using the 'valid-name' fn
    (defn say-hello [name]
      (let [[vname error] (maybe (valid-name name))]
        (if error (println \"Error: \" error)
          (println \"Hello \" name))))"
  [& body]
  `(try [(do ~@body) nil]
     (catch Exception e#
       [nil e#])))


(defmacro maybe-ret
  "Return what the body of code returns when executed. If it throws exception
  return nil. Use this when you need only the return value and don't care about
  the exception.
  See also: maybe"
  [& body]
  `(let [[ret# ex#] (maybe ~@body)]
     ret#))


(defmacro maybe-ex
  "Return the exception a body of code throws when executed. Return nil if it
  throws no exception. Use this when you are interested in the exception rather
  than the return value.
  See also: maybe"
  [& body]
  `(let [[ret# ex#] (maybe ~@body)]
     ex#))


(defmacro filter-exception
  "Execute body of code and in case of an exception, ignore it if (pred ex)
  returns false (i.e. rethrow if true) and return nil."
  [pred & body]
  `(try ~@body
     (catch Exception e#
       (when (~pred e#)
         (throw e#)))))


(defmacro with-exceptions
  "Execute body of code in the context of exceptions to be re-thrown or ignored.
  Args:
    throw-exceptions - List of exceptions that should be re-thrown
    leave-exceptions - List of exceptions that should be suppressed
  Note: 'throw-exceptions' is given preference over 'leave-exceptions'
  Example usage:
    ;; ignore all runtime exceptions except
    ;; IllegalArgumentException and IllegalStateException
    (with-exceptions [IllegalArgumentException IllegalStateException] [RuntimeException]
      ...)"
  [throw-exceptions leave-exceptions & body]
  `(filter-exception (fn [ex#]
                       (cond
                         (some #(instance? % ex#) ~throw-exceptions) true
                         (some #(instance? % ex#) ~leave-exceptions) false
                         :else true))
     ~@body))


(defn repeat-exec
  "Returns a lazy (infinite!, or length n if supplied) sequence of results after
  executing f (no-arg function) successively."
  {:added "0.3"}
  ([f]  {:post [(seq? %)]
         :pre  [(fn? f)]}
    (let [run (fn g[] (cons (f) (lazy-seq (g))))]
      (run))) 
  ([n f] {:pre [(and (number? n) (pos? n))]}
    (take n (repeat-exec f))))


(defmacro try-while
  "Return the result after executing code body; on exception keep re-trying as
  long as pred returns true. The predicate function accepts thrown exception
  as argument. Unless pred throws an exception, no exception will escape the
  code body itself."
  {:added "0.3"}
  [pred & body] {:pre [`(fn? ~pred)]}
  `(first (some #(let [e# (last %)]
                   (if e# (if (~pred e#) false
                            (throw e#))
                     %))
            (repeat-exec #(maybe ~@body)))))


(defmacro try-times
  "Execute body of code; on exception retry maximum n-1 times. Throw last
  encountered exception if none of the tries were successful."
  {:added "0.3"}
  [n & body] {:pre [`(posnum? ~n)]}
  `(let [c# (repeat-exec (dec ~n) #(maybe ~@body))
         r# (some #(if (last %) nil %) c#)]
     (first (or r# [(do ~@body)]))))


(defmacro with-safe-open
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order.
  (This is only a modified version of clojure.core/with-open where any
  exception arising due to closing of resources is ignored.)"
  [bindings & body]
  (assert (vector? bindings))       ; a vector for its binding
  (assert (even? (count bindings))) ; an even number of forms in binding vector
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (try
                                    (. ~(bindings 0) close)
                                    (catch Exception _#)))))
    :else (throw (IllegalArgumentException.
                   "with-safe-open only allows Symbols in bindings"))))


;; ===== Type conversion =====

(defn ^String as-string
  "Convert given argument to string.
  Example:
    (str       \":one\") ; returns \":one\"
    (as-string \":one\") ; returns \"one\"
  See also: as-vstr"
  [x]
  (if (or (keyword? x) (symbol? x)) (name x)
    (str x)))


(defn ^String java-filepath
  "Accept path (of a file) as argument and return a uniform file path for all
  operating systems.
  Example: \"C:\\path\\to\\file.txt\" becomes \"C:/path/to/file.txt\"
  See also: split-filepath"
  [s]
  (let [p (if (instance? File s) (.getAbsolutePath ^File s)
            (str s))]
    (.replace ^String p "\\" "/")))


(defn ^String split-filepath
  "Given a complete path, split into filedir and filename and return as vector.
  The filedir is normalized as uniform Java filepath.
  See also: java-filepath"
  [s]
  (let [jf (java-filepath s)
        sf (sr/split jf #"/")]
    [(sr/join "/" (drop-last sf)) (last sf)]))


(defn ^String pick-filedir
  "Given a filepath, return the directory portion from it."
  [s]
  (first (split-filepath s)))


(defn ^String pick-filename
  "Given a filepath, return the filename portion from it."
  [s]
  (last (split-filepath s)))


(defn split-filename
  "Given a partial or complete file path, split into filename and extension and
  return as vector."
  {:added "0.3"}
  [s]
  (let [f   (pick-filename s)
        sfe (sr/split f #"\.")
        sfc (count sfe)
        sf1 (first sfe)
        sf2 (second sfe)]
    (cond
      (= 1 sfc)                    (conj sfe "")
      (and (= 2 sfc) (empty? sf1)) [(str "." sf2) ""]
      :else                        [(sr/join "." (drop-last sfe)) (last sfe)])))


(defn ^String pick-filename-name
  "Given a filepath, return the filename (without extension) portion from it."
  {:added "0.3"}
  [s]
  (first (split-filename s)))


(defn ^String pick-filename-ext
  "Given a filepath, return the file extension portion from it."
  {:added "0.3"}
  [s]
  (last (split-filename s)))


(defn ^String as-vstr
  "Convert to verbose string - useful for diagnostics and error messages. Like
  as-string, but distinguishes nil as \"<nil>\". 
  Example:
    (as-string  nil) ; returns \"\"
    (as-vstr    nil) ; returns \"<nil>\"
  See also: as-string"
  [x]
  (if-let [y x] (as-string y) "<nil>"))


(defn as-keys
  "Return keys if coll is a map, 'not-map' otherwise."
  ([coll not-map]
    (if (map? coll) (into [] (keys coll))
      not-map))
  ([coll]
    (as-keys coll coll)))


(defn as-vals
  "Return vals if coll is a map, 'not-map' otherwise."
  ([coll not-map]
    (if (map? coll) (into [] (vals coll))
      not-map))
  ([coll]
    (as-vals coll coll)))


(defn as-vector
  "Convert/wrap given argument as a vector."
  [anything]
  (if (vector? anything) anything
    (if (or (seq? anything) (set? anything)) (into [] anything)
      (if (map? anything) (into [] (vals anything))
        (if (nil? anything) []
          [anything])))))


(defn as-set
  "Convert/wrap given argument as a set."
  [anything]
  (if (set? anything) anything
    (if (or (seq? anything) (vector? anything)) (into #{} anything)
      (if (map? anything) (into #{} (vals anything))
        (if (nil? anything) #{}
          #{anything})))))


(defn as-map
  "Convert given collection as a map."
  [coll]
  (if (map? coll) coll
    (if (and (coll? coll) (even? (count coll))) (apply array-map coll)
      (if (nil? coll) {}
        (illegal-arg
          (str "Expected collection with size a multiple of 2, but found "
            (type coll) ": " (as-vstr coll)))))))


(defn coerce
  "Coerce x using f. Return nil if f throws exception."
  [f x]
  (let [s (as-string (first (as-vector x)))]
    (maybe-ret (f s))))


(defn ^Boolean as-boolean
  "Coerce any value to Boolean. Return nil if it cannot be."
  [x]
  (coerce #(Boolean/parseBoolean %) x))


(defn ^Short as-short
  "Coerce any value to short integer. Return nil if it cannot be."
  [x]
  (coerce #(Short/parseShort %) x))


(defn ^Integer as-integer
  "Coerce any value to integer. Return nil if it cannot be."
  [x]
  (coerce #(Integer/parseInt %) x))


(defn ^Long as-long
  "Coerce any value to long. Return nil if it cannot be."
  [x]
  (coerce #(Long/parseLong %) x))


(defn ^Float as-float
  "Coerce any value to float. Return nil if it cannot be."
  [x]
  (coerce #(Float/parseFloat %) x))


(defn ^Double as-double
  "Coerce any value to double-precision number. Return nil if it cannot be."
  [x]
  (coerce #(Double/parseDouble %) x))


;; ===== "not" stripped functions =====

(defn any?                  [& x] (not (apply not-any?          x)))


;; ===== "not" prefixed functions =====

(defn not-associative?      [& x] (not (apply associative?      x)))
(defn not-bound?            [& x] (not (apply bound?            x)))
(defn not-char?             [& x] (not (apply char?             x)))
(defn not-chunked-seq?      [& x] (not (apply chunked-seq?      x)))
(defn not-class?            [& x] (not (apply class?            x)))
(defn not-coll?             [& x] (not (apply coll?             x)))
(defn not-contains?         [& x] (not (apply contains?         x)))
(defn not-counted?          [& x] (not (apply counted?          x)))
(defn not-decimal?          [& x] (not (apply decimal?          x)))
(defn not-delay?            [& x] (not (apply delay?            x)))
(defn not-distinct?         [& x] (not (apply distinct?         x)))
(defn not-empty?            [& x] (not (apply empty?            x)))
(defn not-even?             [& x] (not (apply even?             x)))
(defn not-extends?          [& x] (not (apply extends?          x)))
(defn not-false?            [& x] (not (apply false?            x)))
(defn not-float?            [& x] (not (apply float?            x)))
(defn not-fn?               [& x] (not (apply fn?               x)))
(defn not-future-cancelled? [& x] (not (apply future-cancelled? x)))
(defn not-future-done?      [& x] (not (apply future-done?      x)))
(defn not-future?           [& x] (not (apply future?           x)))
(defn not-identical?        [& x] (not (apply identical?        x)))
(defn not-ifn?              [& x] (not (apply ifn?              x)))
(defn not-instance?         [& x] (not (apply instance?         x)))
(defn not-integer?          [& x] (not (apply integer?          x)))
(defn not-isa?              [& x] (not (apply isa?              x)))
(defn not-keyword?          [& x] (not (apply keyword?          x)))
(defn not-list?             [& x] (not (apply list?             x)))
(defn not-map?              [& x] (not (apply map?              x)))
(defn not-neg?              [& x] (not (apply neg?              x)))
(defn not-nil?              [& x] (not (apply nil?              x)))
(defn not-number?           [& x] (not (apply number?           x)))
(defn not-odd?              [& x] (not (apply odd?              x)))
(defn not-pos?              [& x] (not (apply pos?              x)))
(defn not-ratio?            [& x] (not (apply ratio?            x)))
(defn not-rational?         [& x] (not (apply rational?         x)))
(defn not-reversible?       [& x] (not (apply reversible?       x)))
(defn not-satisfies?        [& x] (not (apply satisfies?        x)))
(defn not-seq?              [& x] (not (apply seq?              x)))
(defn not-sequential?       [& x] (not (apply sequential?       x)))
(defn not-set?              [& x] (not (apply set?              x)))
(defn not-sorted?           [& x] (not (apply sorted?           x)))
(defn not-special-symbol?   [& x] (not (apply special-symbol?   x)))
(defn not-string?           [& x] (not (apply string?           x)))
(defn not-symbol?           [& x] (not (apply symbol?           x)))
(defn not-thread-bound?     [& x] (not (apply thread-bound?     x)))
(defn not-true?             [& x] (not (apply true?             x)))
(defn not-var?              [& x] (not (apply var?              x)))
(defn not-vector?           [& x] (not (apply vector?           x)))
(defn not-zero?             [& x] (not (apply zero?             x)))


;; ===== Maps Transformation =====


(defn map-keys
  "Given a map m, apply f (takes one arg) to each key and return a new map."
  [f m]
  (zipmap (map f (keys m)) (vals m)))


(defn map-vals
  "Given a map m, apply f (takes one arg) to each val and return a new map."
  [f m]
  (zipmap (keys m) (map f (vals m))))


;; ===== Array types =====


(defn array-type
  "Return array type (class object) if given object is an array, nil otherwise."
  [obj]
  (if (nil? obj) nil
    (.getComponentType (class obj))))


(defn array?
  "Return true if given object is an array, false otherwise."
  ([obj]
    (if (nil? obj) false
      (not-nil? (array-type obj))))
  ([obj elem-type]
    (if (or (nil? obj) (nil? elem-type)) false
      (= (as-string elem-type) (as-string (array-type obj))))))


(defn not-array?
  "Same as (not (array? obj)) or (not (array? obj elem-type))."
  ([obj] (not (array? obj)))
  ([obj elem-type] (not (array? obj elem-type))))


;; ===== contains-val? -- complement of contains? =====

(defn contains-val?
  "Look for value unlike 'contains?', which looks for key. Works for indexed
  collections (e.g. vectors, arrays) etc.
  See also: contains? (in clojure.core)"
  [coll needle]
  (cond
    (not-coll?
      coll)     false
    (map? coll) (.containsValue ^Map coll needle)
    :else       (.contains ^Collection coll needle)))


(defn not-contains-val?
  "Same as (not (contains-val? coll needle))."
  [coll needle]
  (not (contains-val? coll needle)))


;; ===== Stack trace and Exceptions =====


(def ^{:doc "Flag: Whether include IDE reference in stack trace dump"
       :dynamic true} *show-ide-reference* false)


(defn stacktrace-rows
  "Given an array or collection of StackTraceElement objects, return a vector
  of [file-name line-number class-name method-name ide-reference] objects. The
  ide-reference field is to enable clickable links in IDEs.
  See also: print-stacktrace"
  ([stack-trace]
    (map #(let [class-name  (or (.getClassName  ^StackTraceElement %) "")
                method-name (or (.getMethodName ^StackTraceElement %) "")
                file-name   (or (.getFileName   ^StackTraceElement %) "")
                line-number (.getLineNumber ^StackTraceElement %)]
            (if *show-ide-reference*
              ;; then
              [file-name line-number class-name method-name
               (str "at " class-name "(" file-name ":" line-number ")")]
              ;; else
              [file-name line-number class-name method-name]))
      (into [] stack-trace)))
  ([]
    (stacktrace-rows (.getStackTrace (Thread/currentThread)))))


(defn clj-stacktrace-row?
  "Return true if given stack trace row is Clojure-specific, false otherwise.
  See also: print-stacktrace"
  ([[^String file-name line-number ^String class-name ^String method-name]
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
  "Return true if given stack trace row is app-specific, false otherwise.
  See also: print-stacktrace"
  ([row]
    (clj-stacktrace-row? row [] ["clojure."]))
  ([row classname-begin-tokens classname-not-begin-tokens]
    (clj-stacktrace-row? row classname-begin-tokens
      classname-not-begin-tokens)))


(defn print-first-nonempty-stacktrace
  "Print first non-empty stack trace from a given set of stack traces.
  See also: print-stacktrace"
  [& stacktrace-rows]
  (print-table-with-header
    (if *show-ide-reference*
      ["File" "Line#" "Class" "Method" "IDE Reference"]
      ["File" "Line#" "Class" "Method"])
    (first (filter not-empty? stacktrace-rows))))


(defn print-stacktrace-rows
  "Print first non-empty stack trace in the following order:
  1. Application specific
  2. Clojure specific
  3. Entire stack trace
  See also: print-stacktrace"
  ([pred all-stacktrace-rows]
    (let [all-stacktrace all-stacktrace-rows
          clj-stacktrace (filter clj-stacktrace-row? all-stacktrace)
          app-stacktrace (filter pred clj-stacktrace)]
      (print-first-nonempty-stacktrace
        app-stacktrace
        clj-stacktrace
        all-stacktrace)))
  ([all-stacktrace-rows]
    (print-stacktrace-rows app-stacktrace-row? all-stacktrace-rows))
  ([]
    (print-stacktrace-rows (stacktrace-rows))))


(defn print-stacktrace
  "Print given stack trace.
  See also: print-exception-stacktrace, print-stacktrace-rows"
  ([stack-trace classname-begin-tokens classname-not-begin-tokens]
    (print-stacktrace-rows
      #(app-stacktrace-row? % classname-begin-tokens classname-not-begin-tokens)
      (stacktrace-rows stack-trace)))
  ([stack-trace]
    (print-stacktrace-rows (stacktrace-rows stack-trace)))
  ([]
    (print-stacktrace-rows)))


(defn print-exception-stacktrace
  "Print stack trace for a given exception.
  See also: print-stacktrace"
  ([^Throwable t ^PrintWriter pw]
    (binding [*out* pw]
      (println (str t))
      (loop [th t]
        (try
          (print-stacktrace (.getStackTrace th))
          (catch Exception e
            (println "Error while printing stack-trace: " e)
            (.printStackTrace e ^PrintWriter *out*)))
        (let [cause (.getCause th)]
          (when (not-nil? cause)
            (println "Caused by: " (str cause))
            (recur cause))))))
  ([^Throwable t]
    (print-exception-stacktrace t *err*)))


(defmacro !
  "Execute given body of code in a try-catch block; if exception occurs then
  print friendly stack trace.
  See also: print-exception-stacktrace, print-stacktrace"
  [& body]
  `(try ~@body
     (catch Exception e#
       (print-exception-stacktrace e#)
       (.printStackTrace e# ^PrintWriter *err*))))


(defmacro !!
  "Execute body of code using ! operator with *show-ide-reference* set to true.
  In other words, print friendly stack trace but also include IDE reference so
  that files are clickable in an IDE."
  [& body]
  `(binding [*show-ide-reference* true]
     (! ~@body)))


;; ===== Assertion helpers =====


(defmacro verify-arg
  "Like assert, except for the following differences:
  1. does not check for *assert* flag
  2. throws IllegalArgumentException"
  [arg]
  `(if ~arg true
     (illegal-arg "Verify (argument) failed: " (pr-str '~arg))))


(defmacro verify-type
  "Return true if arg is of type expected-type, throw IllegalArgumentException
  othewise."
  [expected-type arg]
  `(if (isa? (type ~arg) ~expected-type) true
     (illegal-arg "Verify (argument-type) failed - Expected: " ~expected-type
       ", Found: " (val-dump ~arg))))


(defmacro verify-cond
  "Like assert, except for the following differences:
  1. does not check for *assert* flag
  2. throws IllegalStateException"
  [arg]
  `(if ~arg true
     (illegal-state "Verify (condition) failed: " (pr-str '~arg))))


(defn verify-opt
  "Verify that the optional arguments (keys) are from a known set of keywords.
  Return true if constraint satisfied, throw IllegalArgumentException otherwise."
  [known-coll input-coll]
  (let [known-set (as-set known-coll)]
    (doseq [each (if (map? input-coll)
                   (keys input-coll)
                   input-coll)]
      (when-not (contains? known-set each)
        (throw (IllegalArgumentException.
                 (format "Invalid optional argument key %s, Allowed keys: %s"
                   each
                   (with-out-str (pp/pprint known-set)))))))
    true))


;; ===== Type annotation =====


(defn obj?
  "Return true if argument is a Clojure object (IObj), false otherwise."
  [obj]
  (instance? clojure.lang.IObj obj))


(defn not-obj?
  "Same as (not (obj? arg))"
  [arg]
  (not (obj? arg)))


(defn- mdata-types
  "Return meta data (map) and type(s) the object belongs to."
  [obj]
  (let [mdata (or (meta obj) {})
        types (:type mdata)]
    [mdata types]))


(defn type-meta
  "Return the set of types the given object belongs to; return empty set if it
  belongs to no type."
  [obj]
  (:type (meta obj)))


(defn typed?
  "Return true if child-object is of implied-type, false otherwise. h is the
  hierarchy of types (map), which defaults to global hierarchy if unspecified."
  ([child-obj implied-type] {:post [(boolean? %)]
                             :pre  [(verify-arg (obj? child-obj))
                                    (verify-arg (not-nil?  implied-type))
                                    (verify-arg (not-coll? implied-type))]}
    (or (some #(isa? % implied-type)
          (as-vector (type-meta child-obj)))
      false))
  ([h child-obj implied-type] {:post [(boolean? %)]
                             :pre  [(verify-arg (map? h))
                                    (verify-arg (obj? child-obj))
                                    (verify-arg (not-nil?  implied-type))
                                    (verify-arg (not-coll? implied-type))]}
    (or (some #(isa? h % implied-type)
          (as-vector (type-meta child-obj)))
      false)))


(defn typed
  "Annotate given object with specified types.
  Example:
    (typed [67.6 14.0 7.9] :discount-list :special-offers)"
  ([obj type-1] {:post [(verify-arg (obj? obj))]
                 :pre  [(verify-arg (obj? obj))
                        (verify-arg (keyword? type-1))]}
    (let [[mdata types] (mdata-types obj)]
      (with-meta obj
        (into mdata
          {:type (if (nil? types) type-1
                   (if (coll? types) (conj types type-1)
                     type-1))} ))))
  ([obj type-1 type-2 & more] {:post [(verify-arg (obj? obj))]
                               :pre  [(verify-arg (every? keyword?
                                                    (into [type-1 type-2]
                                                      more)))]}
    (let [of-types (into [type-1 type-2] more)]
      (let [[mdata types] (mdata-types obj)]
        (with-meta obj
          (into mdata
            {:type (if (nil? types) of-types
                     (if (coll? types) (apply conj types of-types)
                       (into [types] of-types)))} ))))))


(defn ftyped
  "Short for 'force-typed' and/or 'function-typed'. Same as 'typed' but coreces
  a non-obj value (see 'obj?') as a no-arg function. Use this function instead
  of 'typed' for basic data types such as string, keyword, number, date etc."
  [obj type-1 & more]
  (let [types (into [type-1] more)
        t-obj (if (obj? obj) obj
                (constantly obj))]
    (apply typed t-obj types)))


(defn untyped
  "Return object with all type annotation removed."
  [obj] {:post [(verify-arg (obj? %))]
         :pre  [(verify-arg (obj? obj))]}
  (let [m (meta obj)]
    (with-meta obj (dissoc m :type))))


;; ===== Keyword to/from string conversion =====


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
        ucase1 #(str (Character/toUpperCase ^Character (first %))
                  (apply str (rest %)))
        lcase  #(if (not-nil? %) (.toLowerCase ^String %))
        cctoks (map ucase1 tokens)]
    (apply str (lcase (first cctoks)) (rest cctoks))))


(defn camelstr-to-k
  "Given a camel-case string, convert it into a dash-delimited keyword. Upper
  case character triggers insertion of the dash."
  [cs]
  (let [b (StringBuilder.)
        f #(do
             (if (and (Character/isUpperCase ^Character %) (not (empty? b)))
               (.append b \-))
             (.append b (Character/toLowerCase ^Character %)))
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


(defn ^List coll-as-string
  "Convert each element in a collection to string and return a vector."
  [ks]
  (vec (map as-string (as-vector ks))))


(defn ^List coll-as-keys
  "Convert each element in a collection to keyword and return a vector."
  [ks]
  (vec (map keyword (as-vector ks))))


(defn ^Map keys-to-str
  "Given a map with every key a keyword, convert keys to strings.
  Input: {:a 10 :b \"20\"}
  Returns: {\"a\" 10 \"b\" \"20\"}"
  [m]
  (let [ks (keys m)
        vs (vals m)]
    (zipmap (coll-as-string ks) vs)))


(defn ^Map str-to-keys
  "Given a map with every key a string, convert keys to keywords.
  Input: {\"a\" 10 \"b\" \"20\"}
  Returns: {:a 10 :b \"20\"}"
  [m]
  (let [ks (keys m)
        vs (vals m)]
    (zipmap (coll-as-keys ks) vs)))


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
  [target method-spec & more]
  (let [method-specs (into [method-spec] more)]
    (into [] (map #(into [] (cons target (as-vector %))) method-specs))))


(defn class-methods
  "Return a lazy list of method-specs reflecting all the methods declared by the
  class or interface represented by the given Class object. If the argument is
  not a class instance, then its class is derived.
  See also: public-methods, clojure.contrib.repl-utils/show"
  [^Class klass]
  (map method-sig
    (.getDeclaredMethods (if (class? klass) klass
                           (class klass)))))


(defn public-methods
  "Return a lazy list of method-specs reflecting all the public member methods
  of the class or interface represented by the given Class object, including
  those declared by the class or interface and those inherited from superclasses
  and superinterfaces. If the argument is not a class instance, then its class
  is derived.
  See also: class-methods, clojure.contrib.repl-utils/show"
  [^Class klass]
  (map method-sig
    (.getMethods (if (class? klass) klass
                           (class klass)))))


(defn method
  "Call instance method on the target object. Wrapper for
  Reflector/invokeInstanceMethod (see link):
  http://github.com/richhickey/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
  Short link: http://j.mp/a2Kd9R
  Examples:
    (method \"Hello\" :char-at 0)     ; returns \\H
    (method \"Hello\" :substring 3 4) ; returns \"l\"
    ;; the call below returns [\\H \"l\"]
    (method [[\"Hello\" :char-at 0]
             [\"Hello\" :substring 3 4]])
    ;; same call as above expressed using target-method-specs
    (method (target-method-specs \"Hello\"
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
  ([pojo] {:pre [(verify-arg (not-nil? pojo))]}
    (fn [method-spec] {:pre [(verify-arg (vector? method-spec))]}
      (let [[method-name & args] (as-vector method-spec)]
        (apply method pojo method-name args))))
  ([pojo method-name & args] {:pre [(verify-arg (not-nil? pojo))
                                    (verify-arg (not-nil? method-name))]}
    (fn [& more-args]
      (apply method pojo method-name
        (concat args more-args)))))


(defn setter
  "Call setter method on a target object using args. 'setter' is either a
  keyword or a string.
  Examples:
    (setter obj :price 67.88)   ; .setPrice(67.88)   - returns 67.88
    (setter obj :min-max 30 75) ; .setMinMax(30, 75) - returns void
    (setter obj :auto-commit)   ; .setAutoCommit()   - returns true
    ;; same stuff in a single call - returns [67.88 nil true]
    (setter [[obj :price 67.88]
             [obj :min-max 30 75]
             [obj :auto-commit]])
    ;; same stuff without repeating the target object - returns [67.88 nil true]
         (setter (call-specs obj [[:price 67.88]
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
        (concat args more-args)))))


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
    ;; assuming a Person class has getters getName, getAddress and getEmail
    (map (getter-fn person) [:name :address :email])"
  ([pojo]
    (fn [method-spec]
      (let [[method-name & args] (as-vector method-spec)]
        (apply method pojo (k-to-getter method-name) args))))
  ([pojo method-name & args]
    (fn [& more-args]
      (apply method pojo (k-to-getter method-name)
        (concat args more-args)))))


;; ===== Properties handling =====

(defn load-properties
  "Load properties from a given InputStream or Reader instance."
  ^Properties
  [input]
  (let [p (Properties.)]
    (cond
      (instance? InputStream input) (.load p ^InputStream input)
      (instance? Reader      input) (.load p ^Reader      input)
      :else (illegal-arg "input" "java.io.InputStream or java.io.Reader"
              (as-vstr (type input))))
    p))


(defn property-map
  "Transform a given Properties instance to a map."
  [^Properties properties]
  (let [ks (into [] (.stringPropertyNames properties))
        vs (into [] (map #(.getProperty properties %) ks))]
    (zipmap ks vs)))


(defn is-true?
  "Tell whether a given value is equivalent to true."
  [any]
  (cond
    (string? any)  (let [v (.toLowerCase ^String any)]
                     (or
                       (= "true" v)
                       (= "yes"  v)
                       (= "on"   v)))
    (keyword? any) (let [v (keyword (.toLowerCase ^String (name any)))]
                     (or
                       (= :true v)
                       (= :yes  v)
                       (= :on   v)))
    (number? any)  (> any 0)
    :else (true? any)))


;; ===== JNDI functions (tree-printing not recommended for production use) =====

(defmacro with-root-context
  [root-context & body]
  `(do
    (assert (not (nil? ~root-context)))
    (assert (instance? Context ~root-context))
    (binding [in/*root-context* ~root-context]
      ~@body)))


(defn- increase-indent []
  (swap! in/*indent* #(+ % 4)))


(defn- decrease-indent []
  (swap! in/*indent* #(- % 4)))


(defn- print-entry
  [^NameClassPair next-elem]
  (let [indent-str (apply str
                     (take @in/*indent* (repeat " ")))]
    (if (nil? next-elem) (println indent-str "--> <nil>")
      (println indent-str "-->"
        (.getName next-elem)
        " (" (type next-elem) "->" (.getClassName next-elem) ")"))))


(declare do-print-jndi-tree)


(defn- print-ne
  [^NamingEnumeration ne ^String parent-ctx]
  (loop []
    (when (.hasMoreElements ne)
      (let [^NameClassPair next-elem (.nextElement ne)]
        (print-entry next-elem)
        (increase-indent)
        (if (or (instance? Context next-elem)
              (and (instance? NameClassPair next-elem)
                (instance? Context (.getObject ^Binding next-elem))))
          (do-print-jndi-tree
            (if (zero? (.length parent-ctx))
              (.getName next-elem)
              (str parent-ctx "/" (.getName next-elem))))
          (println "** Not drilling "
            (type (.getObject ^Binding next-elem))))
        (decrease-indent))
      (recur))))


(defn- do-print-jndi-tree
  [^String ct]
  (assert (not (nil? ct)))
  (if (instance? Context in/*root-context*)
    (print-ne (.list in/*root-context* ct) ct)
    (print-entry in/*root-context*)))


(defn print-jndi-tree
  "Print JNDI tree. You should have JNDI environment configured beforehand."
  ([^String ct]
    (binding [in/*indent* (atom 0)]
      (do-print-jndi-tree ct)))
  ([]
   (print-jndi-tree "")))


(defn jndi-lookup
  "Lookup key in JNDI context."
  ([^Context context ^String k]
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
  (let [lookup-fn (fn [^Context ctx ^String k] (.lookup ctx k))
        new-ctx (reduce lookup-fn context args)]
    new-ctx))
