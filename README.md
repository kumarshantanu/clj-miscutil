# clj-miscutil

This project is an assortment of Clojure functions/macros to carry out
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

On Clojars: [http://clojars.org/clj-miscutil](http://clojars.org/clj-miscutil)

Leiningen dependency: `[clj-miscutil "0.4.1"]`

Supported Clojure versions: Clojure versions 1.2 through 1.5

Include the following in your namespace:

```clojure
(ns example.app
  (:use clj-miscutil.core))
```

Please check `doc` directory for documentation.


## Building/Installation

You will need Leiningen 2 to build from sources. Execute the following:

```bash
$ lein dev test
$ lein all test
$ lein jar
$ lein install
```

To build documentation you will need to have `asciidoc` (and optionally, `pygments`) installed and in PATH. Run this:

```bash
$ cd doc
$ ./build-doc.sh
```


## License

Copyright © 2010-2012 Shantanu Kumar

Distributed under the Eclipse Public License, the same as Clojure.


## Getting in touch

Email: [kumar.shantanu@gmail.com](kumar.shantanu@gmail.com)

Twitter: [@kumarshantanu](twitter.com/kumarshantanu)


## Contributing

File bug reports on Github; fork and send pull requests.