#!/bin/bash


# enable globs
shopt -s extglob

#clean lib
rm lib/!(.gitignore)

#copy in all .jars from BIDMat repo checkout
git clone https://github.com/jcanny/BIDMat.git
cp BIDMat/*.jar lib/
cp BIDMat/lib/*.jar lib/
cp $JAVA_HOME/lib/tools.jar lib/
rm -rf BIDMat

#clean resources
rm -rf src/main/resources/!(.gitignore)

#get data and unpack into resources
wget http://www.cs.cornell.edu/People/pabo/movie-review-data/review_polarity.tar.gz
tar -xzf review_polarity.tar.gz -C src/main/resources/
rm review_polarity.tar.gz
