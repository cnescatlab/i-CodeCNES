#!/bin/bash
echo "--------------------"
echo "- SH.MET.PipeLine  -"
echo "-    OK            -"
echo "--------------------"

# Ce script liste les fichiers qu'il y a dans le dossier actuel
# et fait la substituition de les vocals [aeio] pas [u]

 ls -l | sed -e "s/[aeio]/u/g"  
