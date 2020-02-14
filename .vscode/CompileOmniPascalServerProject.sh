#!/bin/bash

LAZBUILD="lazbuild"
PROJECT="/Users/herux/Documents/couchbasefpc/tests/cbtest.lpi"

# Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo. >> "/Users/herux/Documents/couchbasefpc/tests/cbtest.lpr"

if $LAZBUILD $PROJECT; then

  if [ $1 = "test" ]; then
    "/Users/herux/Documents/couchbasefpc/tests/cbtest" 
  fi
fi
