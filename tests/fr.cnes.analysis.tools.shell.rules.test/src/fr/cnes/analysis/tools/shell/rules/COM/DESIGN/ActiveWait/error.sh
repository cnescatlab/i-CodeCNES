#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.ACTIVEWAIT"
echo "Fichier KO de test"
echo "------------------------------------------"

echo "Iterate 5 times"
i=1
while [ 1 ]
do
   echo "Iteration num $i"
   if [ $i == 5 ]
   then
      break
   fi
   i=$(($i+1))
done

echo "Insert one number"
read x
echo "You insert: $x"
