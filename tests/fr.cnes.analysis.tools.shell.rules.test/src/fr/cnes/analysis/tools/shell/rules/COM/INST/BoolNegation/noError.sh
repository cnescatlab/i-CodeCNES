#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.BOOLEANNEGATION"
echo "Fichier OK de TU"
echo "------------------------------------------"

echo "Insert num:"
read a
if [ ! $a -eq 0 ] && [ ! $a -eq 1 ] ; then
   echo "You entered a different number from 0 or 1"
fi


