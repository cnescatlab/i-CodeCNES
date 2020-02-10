#!/bin/bash
echo "------------------------------------------"
echo "COM.DATA.LOOPCONDITION"
echo "Fichier KO de TU"
echo "------------------------------------------"

echo "Printing Numbers 1 through 10 (but not 3)."

function testFunction ()
{
LIMIT=1
a=0

while [ $a -le "$LIMIT" ]
do
 a=$(($a+1))

 if [ "$a" -eq 3 ] # Excludes 3
 then
   continue      # Skip rest of this particular loop iteration.
 fi

 echo -n "$a "   # This will not execute for 3

 if [ "$LIMIT" -ne 9 ]
 then
   LIMIT=$(($LIMIT+1))
 fi
done
}

# Imbric loops to verify variables changes
b=0
c=0
LIMIT1=10
LIMIT2=10
while [ $b -le "$LIMIT1" ]
do
 b=$(($b+1))
 echo "Iteration $b "
 until [ $c -le "$LIMIT2" ]
 do
   c=$(($c+1))
   LIMIT2=$(($LIMIT2-1))
   echo "Value $c" 
 done
 
done

echo "." 
