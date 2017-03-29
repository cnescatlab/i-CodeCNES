#!/bin/bash
set -x

echo "--------------------"
echo "- SH.IO.Redirect   -"
echo "-    OK            -"
echo "--------------------"

# 
# Explication sur les redirections : 
#    . 0 : correspond a stdin, 1 a stdout, 2 a stderr
#    . 2>&1 : redirige stderr vers stdout 
res=`ls -l 2>&1`

set -x
# --- Definition de variables
randomizer=/dev/urandom
output_file=./mes_octets_aleatoires
block_size=1
# --- Associe le descripteur 3 au fichier f3 : ce fichier est en input/output
3<>./f3
# --- Associe le descripteur 4 au fichier f4 : ce fichier est en input/output
4<>./f4
# Generation d'un fichier contenant 3 blocs de 1 caractere
dd if=$randomizer of=$output_file bs=$block_size count=3
#On cherche maintenant a voir comment ce fichier est organise
dd_stats='^[0-9]+\+[0-9]+ records (in|out)$'
# Explication sur les redirections :
# . 0 : correspond a stdin, 1 a stdout, 2 a stderr
# . 2>&1 : redirige stderr vers stdout
# . 1>&2 : redirige stdout vers stderr
# . 1>&3 : redirige stdout vers le descripteur 3
# . 3>&- : ferme le fichier avec le descripteur 3
# . 4>&- : ferme le fichier avec le descripteur 4
res=`((dd if=$output_file ibs=$block_size 2>&1 1>&3 3>&- 4>&-;
#On fait quelque chose
echo $?>&4) |
#On refait encore quelque chose.
egrep -v "$dd_stats" 1>&2 3>&- 4>&-) 4>&1`