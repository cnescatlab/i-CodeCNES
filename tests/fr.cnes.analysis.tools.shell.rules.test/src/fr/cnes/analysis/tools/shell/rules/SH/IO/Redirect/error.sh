#!/bin/bash
set -x

echo "--------------------"
echo "- SH.IO.Redirect   -"
echo "-    KO            -"
echo "--------------------"

res=`ls -l 2>&1`
#Redirection standard => ligne 9 n'est pas une erreur.


3<>./f3
4<>./f4
dd if=$randomizer of=$output_file bs=$block_size count=3
dd_stats='^[0-9]+\+[0-9]+ records (in|out)$'
res=`((dd if=$output_file ibs=$block_size 2>&1 1>&3 3>&- 4>&-;
echo $?>&4
egrep -v "$dd_stats" 1>&2 3>&- 4>&-) 4>&1`