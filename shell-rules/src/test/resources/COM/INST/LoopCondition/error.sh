#!/bin/bash
echo "------------------------------------------"
echo "COM.INST.LOOPCONDITION"
echo "Fichier KO de TU"
echo "------------------------------------------"

echo "Boucle entre  0 et 10"
cpt=0
while [ $cpt -ne 10 ]
do
   # Instructions quelconques
   let cpt=cpt+1
   echo "Iteration $cpt"
done
