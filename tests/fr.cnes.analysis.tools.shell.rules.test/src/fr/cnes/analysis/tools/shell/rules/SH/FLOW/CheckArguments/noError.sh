#!/bin/bash
code_erreur_nbargs=-128
r_ma_fonction_affine=0

# --- Definition d'une fonction qui doit comporter 3 arguments 
ma_fonction_affine () 
{ 
   if [ $# -ne 3 ]
   then
      r_ma_fonction_affine=$code_erreur_nbargs
      echo "WARNING !!! ma fonction ma_fonction_affine() a été lancée !"
   else 
      # calcul : y = ax + b
      let y=$1*$2+$3 
      r_ma_fonction_affine=$y
   fi
}

typeset -i a=3
typeset -i x=12
typeset -i b=5

# --- Appel de la fonction avec 3 parametres 
ma_fonction_affine $a $x $b

# --- Appel de la fonction avec 2 parametres 
ma_fonction_affine $a $x
