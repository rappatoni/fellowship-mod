#!/bin/sh
# Fetch TweetyProject 1.31 from Maven Central into eval/tweety/.tweety (not
# committed) and compile the reference driver java/TweetyRef.java.
# Needs java/javac (tested with OpenJDK 26) and python3.
set -e
here=$(cd "$(dirname "$0")" && pwd)
T=${TWEETY_DIR:-$here/.tweety}
mkdir -p "$T/lib" "$T/refclasses"
python3 -I "$here/java/fetch.py" "$T/lib" \
  org.tweetyproject.arg:dung org.tweetyproject.arg:aspic org.tweetyproject.arg:aba \
  org.tweetyproject.arg:delp org.tweetyproject.logics:pl org.tweetyproject.logics:fol \
  org.tweetyproject.logics:rdl org.tweetyproject:sat
# two artifacts are both named commons-1.31.jar; keep both under distinct names
B=https://repo1.maven.org/maven2/org/tweetyproject
curl -sL -o "$T/lib/tcommons-1.31.jar" "$B/commons/1.31/commons-1.31.jar"
curl -sL -o "$T/lib/lcommons-1.31.jar" "$B/logics/commons/1.31/commons-1.31.jar"
rm -f "$T/lib/commons-1.31.jar"
# dependencies whose poms the fetcher cannot resolve
curl -sL -o "$T/lib/jgrapht-core-1.5.0.jar" https://repo1.maven.org/maven2/org/jgrapht/jgrapht-core/1.5.0/jgrapht-core-1.5.0.jar
curl -sL -o "$T/lib/ojalgo-35.0.jar" https://repo1.maven.org/maven2/org/ojalgo/ojalgo/35.0/ojalgo-35.0.jar
javac -nowarn -d "$T/refclasses" -cp "$(ls "$T"/lib/*.jar | tr '\n' ':')" "$here/java/TweetyRef.java"
echo "Tweety reference ready in $T"
