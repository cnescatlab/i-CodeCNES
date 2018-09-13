#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.CHECKCODERETURN"
echo "Fichier KO de TU"
echo "------------------------------------------"

factorial()
{
        if [ $# -eq 0 ]; then
          echo "Proc "$0" : manque argument - (entier positif) "
        else
           echo "Proc "$0": "$1" "
           echo ""
        #
           compteur=$1
           factorielle=1
           while [ $compteur -gt 0 ]
           do
                  factorielle=$(( $factorielle * $compteur ))
                  compteur=$(( $compteur - 1 ))
           done
        fi
        echo "Fin Function"
        return $factorielle
}

# calcul du factorial de le nombre passe com a parametre
# le return de la function n'est pas verifie
factorial $1

# calcul du factorial de le nombre 10
factorial 10

cd $HOME
ls

nettoyer_repertoire() {
    if [ -d "$1" ] && [ "$1" != / ]
    then
        rm -rf "$1"
    fi
}

date +%d_%m_%y | sed -e 's,_, ,g' | awk '{print $1,$2,$3+2000}' | read jj mm aaaa

nettoyer_repertoire "${tmpdir}"

grep "Date :" $ficresarc | awk '{print $3}' > datejul.tmp