#!/bin/sh

################################################################################
CUR_FILE="NONE"

################################################################################
assert_equal ()
{
  CUR_FILE=$1
  EXPECT=$2
  ACTUAL=`runhaskell -Wall -Werror $CUR_FILE`

  if [ "$ACTUAL" = "$EXPECT" ]; then
    return 0
  else
    echo "${CUR_FILE}: expected $EXPECT but actual value is $ACTUAL"
    return 1
  fi
}

################################################################################
[ -d src ] && cd src

################################################################################
assert_equal intro.hs 10
assert_equal rec.hs "Peter Jones"
assert_equal product.hs "Peter"
assert_equal sum.hs 1
assert_equal poly.hs 6
