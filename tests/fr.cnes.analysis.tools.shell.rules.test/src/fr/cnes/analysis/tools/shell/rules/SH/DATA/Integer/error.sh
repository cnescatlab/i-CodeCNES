#!/bin/bash

echo "-------------------"
echo "- SH.DATA.Integer -"
echo "-       KO        -"
echo "-------------------"

#
# *** MAUVAIS EXEMPLE ***
# -> la variable cpt n'est pas typee
#
cpt=0
while [ $cpt -lt 10 ]
do
   echo "Iteration $cpt"
   let cpt=cpt+1
done
