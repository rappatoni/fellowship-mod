#!/bin/sh
# Run the Tweety reference driver java/TweetyRef.java. Set it up once with
# ./setup_tweety.sh (fetches Tweety 1.31 from Maven Central into .tweety/).
here=$(cd "$(dirname "$0")" && pwd)
T=${TWEETY_DIR:-$here/.tweety}
exec java -cp "$(ls "$T"/lib/*.jar | tr '\n' ':')$T/refclasses" TweetyRef "$@"
