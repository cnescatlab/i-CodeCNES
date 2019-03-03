#!/bin/bash
echo "--------------------------"
echo "-   COM.TYPE.Expression  -"
echo "--------------------------"

VAL=5
ESTIMATION=15
TOTAL=$VAL
CUMUL=`expr $VAL + $ESTIMATION - $TOTAL`
echo "CUMUL = $CUMUL"

DEBUG=0
if [ "$DEBUG" = 1 ]
then
	echo "no debug!"
fi
