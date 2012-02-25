#!/usr/bin/env bash

rm -rf target
mkdir -p target
asciidoc -a pygments -b html -o target/manual.html manual.asciidoc
asciidoc -a pygments -b docbook -o target/manual.xml manual.asciidoc
cp by-nd-88x31.png target

rm -rf .lein-plugins
rm -rf lib
lein deps && cp lib/dev/docbook-xsl-1.76.1-ns-resources.zip lib/dev/docbook-xsl-1.76.1-ns-resources.jar
#lein docbkx html
lein docbkx pdf epub

