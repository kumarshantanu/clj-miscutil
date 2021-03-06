# Changes and TODO

## 0.5.0 / 2014-Sep-09

* Support Clojure 1.3 through Clojure 1.7
* Drop table printing functions (as they are built into Clojure 1.3 or higher)
* Add `sb-str` macro for faster string concatenation


## 0.4.1 / 2012-Sep-17

* Fix reflection warning in `static-method`
* Use semantic versioning (starting with `0.4.0`)
* Require Leiningen 2 for builds


## 0.4 / 2012-Mar-02

* Move from Bitbucket to Github
* Move from Maven build to Leiningen build
* Move from Apache license to Eclipse license
* Add documentation
* Support Clojure 1.2.0 through Clojure 1.4.0-beta2
* Add static method-call/setter/getter for classes
* Rename `maybe-ret` to `maybe-val`
* Rename `pick-filename-name` to `pick-filename-only`


## 0.3 / 2011-Apr-01

* Filename-splitting functions
* Version information as a var


## 0.2 / 2011-Mar-06

* Clojure 1.3 compatibility
* random values
* boolean values
* table printing
* var meta data
* constructing exceptions
* non-breaking error handling (exception catching)
* type conversion/wrapping
* more "not" associated functions
* array types
* contains-val? (complement for contains?)
* friendly stack trace printing
* type annotation (typed abstraction)
* STDERR capture macros


## 0.1 / 2010-Oct-22

* "not" prefixed functions
* type conversion/wrapping
* argument/condition assertion
* keyword to/from string conversion
* calling setter and getter methods
* java.util.Properties transformation
* JNDI tree-printing and lookup
