#!/bin/bash

echo "------------------------------------------"
echo "COM.FLOW.ABORT"
echo "Fichier OK de TU"
echo "------------------------------------------"

# Find process with name ruby
PID=`ps aux | grep -i "ruby" | cut -d " " -f 1`
#kill all processes with id in PID variable
echo "Kill the following processes:"
echo "$PID"

pkill -f $PID

