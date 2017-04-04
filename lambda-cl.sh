#!/bin/sh

cl_path="/FILL_ME/cl.scm"

if test -n "$1"
then
  echo -n "" | \
    mit-scheme --quiet --load "$cl_path" --eval "(display $1)"
  echo
else
  tmp=`mktemp`
  cat > "$tmp"
  (cat "$tmp" | head -n-1 ; echo "(display $(cat "$tmp" | tail -n-1))") | \
    mit-scheme --quiet --load "$cl_path"
  echo
  rm $tmp
fi
