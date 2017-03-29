#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.LOOPCONDITION"
echo "Fichier OK de TU"
echo "------------------------------------------"

echo "Boucle entre  0 et 10"
cpt=0
while [ $cpt -le 10 ]
do
   # Instructions quelconques
   let cpt=cpt+1
   echo "Iteration $cpt"
done

