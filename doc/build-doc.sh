#!/usr/bin/env bash

mkdir -p target
rm -rf target/manual.xml
asciidoc -b docbook -o target/manual.xml manual.asciidoc
cp by-nd-88x31.png target

rm -rf .lein-plugins
rm -rf lib
lein deps && cp lib/dev/docbook-xsl-1.76.1-ns-resources.zip lib/dev/docbook-xsl-1.76.1-ns-resources.jar
#lein docbkx html
lein docbkx pdf
#lein docbkx epub

