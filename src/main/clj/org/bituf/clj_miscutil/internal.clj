(ns org.bituf.clj-miscutil.internal
  (:import
    (java.util Map Random)
    (javax.naming Binding Context InitialContext)))


(def ^{:doc "Random seed" :tag Random} random-seed (Random.))


(def ^{:doc "Flag to determine whether arguments/conditions are to be asserted"}
      *assert-cond* true) ; true: development/testing, false: production


(def ^{:doc "Typically bound to javax.naming.Context"
       :tag Context}
      *root-context* nil)


(def ^{:doc "Typically bound to an integer wrapped in an atom, e.g. (atom 0)"}
      *indent* nil)


(def ^{:doc "The 'types' keyword"} types-keyword (keyword (str *ns* ".types")))


(def ^{:doc
  "Implied super-types (can be used to express hierarchies).
  Example:
    {:employee [:salaried :person] ; 'employee' implies 'salaried' and 'person'
     :salaried [:person]           ; 'salaried' implies 'person'
     }"}
      *implied-types* {})


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
            (if (.containsKey ^java.util.Map pt-np-xlat-keys s)
              (get pt-np-xlat-map s) s))
      fs)))
