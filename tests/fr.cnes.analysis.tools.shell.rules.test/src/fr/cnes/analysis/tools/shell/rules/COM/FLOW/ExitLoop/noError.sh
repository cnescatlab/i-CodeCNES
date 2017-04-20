#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.EXITLOOP"
echo "Fichier KO de TU"
echo "------------------------------------------"

echo "Printing Numbers 1 through 10."

LIMIT=9
b=0

while [[ $b -le "$LIMIT" ]]
do
 b=$(($b+1))
 echo -n "$b "
 
done

echo "." 
