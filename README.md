# Clj-MiscUtil v0.4

Clj-MiscUtil is an assortment of Clojure functions/macros to carry out
miscellaneous common activities. The functions deal with the following:

* boolean values
* table printing
* var meta data
* constructing exceptions
* type conversion/wrapping
* "not" associated functions
* array types
* contains-val? (complement for contains?)
* exception and stack trace printing
* assertion helpers
* type annotation (typed abstraction)
* keyword to/from string conversion
* calling methods by reflection
* java.util.Properties transformation
* JNDI tree-printing and lookup


## Usage

Maven/Leiningen dependency details are here: [http://clojars.org/clj-miscutil](http://clojars.org/clj-miscutil)

Clj-MiscUtil v0.4 is tested for Clojure versions 1.2 through 1.4, and can be used by including the following in your namespace:

    (ns example.app
      (:use clj-miscutil))

Please check `doc` directory for documentation.


## Building/Installation

You will need Leiningen to build from sources. Execute the following:

    $ lein deps
    $ lein multi test
    $ lein jar
    $ lein install

To build documentation you will need to have `asciidoc` (and optionally, `pygments`) installed and in PATH. Run this:

    $ cd doc
    $ ./build-doc.sh


## License

Copyright (C) 2010-2012 Shantanu Kumar (kumar.shantanu@gmail.com)

Distributed under the Eclipse Public License, the same as Clojure.


## Getting in touch

Email: [kumar.shantanu@gmail.com](kumar.shantanu@gmail.com)

Twitter: [@kumarshantanu](twitter.com/kumarshantanu)


## Contributing

File bug reports on Github; fork and send pull requests.