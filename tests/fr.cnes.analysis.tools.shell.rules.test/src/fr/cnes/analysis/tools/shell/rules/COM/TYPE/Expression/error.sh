#!/bin/bash
echo "--------------------------"
echo "-   COM.TYPE.Expression  -"
echo "--------------------------"

VAL=5
ESTIMATION='abc'
TOTAL=$VAL
CUMUL=`expr $VAL > $ESTIMATION`


DEBUG=0
if [ "$DEBUG" = "1" ]
then
	echo "no debug!"
fi
