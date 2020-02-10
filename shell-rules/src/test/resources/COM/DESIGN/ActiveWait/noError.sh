!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.ACTIVEWAIT"
echo "Fichier OK de test"
echo "------------------------------------------"

echo "Iterate 5 times"
i=1
while [ $i -le 5 ]
do
   echo "Iteration num $i"
   i=$(($i+1))
done
