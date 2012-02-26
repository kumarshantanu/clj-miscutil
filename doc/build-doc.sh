#!/usr/bin/env bash

rm -rf target
mkdir -p target
if [ -z "`which pygmentize`" ]; then
  pyg=""
  cat manual.asciidoc | sed -e 's/\:language\:\ clojure/\:language\:\ lisp/' > target/manual.asciidoc
else
  pyg="-a pygments"
  cp manual.asciidoc target/manual.asciidoc
fi

asciidoc $pyg -b html -o target/manual.html target/manual.asciidoc
asciidoc $pyg -b docbook -o target/manual.xml target/manual.asciidoc
cp by-nd-88x31.png target

rm -rf .lein-plugins
rm -rf lib
lein deps && cp lib/dev/docbook-xsl-1.76.1-ns-resources.zip lib/dev/docbook-xsl-1.76.1-ns-resources.jar
#lein docbkx html
lein docbkx pdf epub

