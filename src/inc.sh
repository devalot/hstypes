#!/bin/sh

################################################################################
# Very stupid load generator for the stm.hs tool.

################################################################################
CLIENTS=25
COUNT=500

################################################################################
client ()
{
  echo "--> Client $1"
  (for i in `seq $COUNT`; do echo "inc foo"; done) | \
    nc localhost 9090 > /dev/null
}

################################################################################
for i in `seq $CLIENTS`; do
  client $i &
done

################################################################################
wait
echo -n "--> Total: "
echo "get foo" | nc localhost 9090
