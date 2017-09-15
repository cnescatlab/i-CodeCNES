#!/bin/bash
echo "------------------------------------------"
echo "COM.PRES.HEADER"
echo "Fichier KO de TU"
echo "Pas d'entete a les fonctions"
echo "------------------------------------------"

code_erreur_nbargs=-128

r_ma_fonction_affine=

ma_fonction_affine () 
{ 
   if [ $# -ne 3 ]
   then
      r_ma_fonction_affine=$code_erreur_nbargs
   else 

      let y=$1*$2+$3 
      r_ma_fonction_affine=$y
   fi
}


affiche_resultat ()
{
   if [ $# -ne 2 ]
   then
      printf "Erreur grave dans affiche_resultat : nombre d'arguments incorrects\n"
   else 
      p1=$1
      p2=$2
      if [ $p2 -ge 0 ]
      then
         printf "execution de 'ma_fonction_affine' avec chaine de calcul : %s resultat = %s \n" $p1 $2
      else
         printf "erreur d'execution de 'ma_fonction_affine' avec chaine de calcul : %s code retour = %s\n" $p1 $p2
         printf "   ===>Erreur grave dans ma_fonction_affine : nombre d'arguments incorrects<===\n"
      fi
   fi
}

ma_fonction_affine 1 2 3
affiche_resultat 1 2
