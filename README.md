# Clj-MiscUtil v0.3

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

Maven/Leiningen dependency details are here: [http://clojars.org/org.bituf/clj-miscutil](http://clojars.org/org.bituf/clj-miscutil)

Examples for usage can be found in the tutorial below:


## Building/Installation

You will need Maven 2 to build from sources. Execute the following:

    $ mvn clean package  # packages up a JAR in "target" dir
    $ mvn install        # to install to your local Maven repo
    $ mvn clojure:gendoc # generate Clojure API documentation


## License

   Copyright (C) 2010, 2011 Shantanu Kumar (kumar[dot]shantanu[at]gmail[dot]com)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this software except in compliance with the License.
   You may obtain a copy of the License at

   [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


# Documentation

Clj-MiscUtil can be used by including the following in your namespace:

    (ns example.app
      (:use org.bituf.clj-miscutil))


## Reference

Please check The Bitumen Framework Handbook [https://bitbucket.org/kumarshantanu/bituf-handbook/src](https://bitbucket.org/kumarshantanu/bituf-handbook/src)