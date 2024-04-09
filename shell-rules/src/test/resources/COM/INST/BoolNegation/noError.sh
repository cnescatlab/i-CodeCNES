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

if [ ! -r "$1" -o ! -f "$1" ] ; then
    echo "This should be ok"
fi

debug ${!F}_${!L} "----------------> BEGIN: existsAndNotEmpty $item"
debug ${!F}_${!L} $(ls ${reportsDir})
