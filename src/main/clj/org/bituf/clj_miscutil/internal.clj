(ns org.bituf.clj-miscutil.internal
  (:import
    (java.util Random)
    (javax.naming Context)))


(def ^{:doc "Random seed" :tag Random} random-seed (Random.))


(def ^{:doc "Typically bound to javax.naming.Context"
       :tag Context
       :dynamic true}
      *root-context* nil)


(def ^{:doc "Typically bound to an integer wrapped in an atom, e.g. (atom 0)"
       :dynamic true}
      *indent* nil)


;; ===== Table printing =====


(def pt-np-xlat-map {"\b" "\\b"
                     "\f" "\\f"
                     "\n" "\\n"
                     "\r" "\\r"
                     "\t" "\\t"})

(def pt-np-xlat-keys (keys pt-np-xlat-map))

(defn xlat-np-chars
  "Translate non-printable characters in a given string; return translated str.
  See also: http://hyperpolyglot.wikidot.com/lisp"
  [^String fs]
  (apply str
    (map #(let [s (str %)]
            (if (.contains ^java.util.Collection pt-np-xlat-keys s)
              (get pt-np-xlat-map s) s))
      fs)))
