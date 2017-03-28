#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.EXITLOOP"
echo "Fichier KO de TU"
echo "------------------------------------------"

LIMIT=10
a=0
c=0
LIMIT2=10
while [[ $b -le "$LIMIT1" ]]
do
 b=$(($a+1))

 while [[ $c -le "$LIMIT2" ]]
 do
   c=$(($b+1))
   LIMIT2=$(($LIMIT+1))
   if [ "$c" -ne 3 ] # Excludes 3
   then
      echo -n "XXX"
      break      # Skip rest of this particular loop iteration.
   else
      echo -n "$c "
      break		# second BREAK > error
   fi
 done
 
 if [$LIMIT2 -eq 10]
 then
 	exit 3  
 fi  
 
done

echo "." 
