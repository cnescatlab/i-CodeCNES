#!/bin/bash
echo "------------------------------------------"
echo "COM.FLOW.CHECKCODERETURN"
echo "Fichier OK de TU"
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
factorial $1
retour=$?

if [ $retour -ne 1 ]; then
   echo "Factorielle de $1 = $retour"
fi

cd $HOME
if [ $? -eq 0 ]; then
   echo "echec du cd"
   exit 1
else
   ls
   if [ $? -eq 0 ]; then
      echo "echec du ls"
      exit 1
   fi
fi

date +%d_%m_%y | sed -e 's,_, ,g' | awk '{print $1,$2,$3+2000}' | read jj mm aaaa
retour=$?

cd -- "$idir" || { echo >&2 can not cd to "$idir"; exit 1; }

funcname () 
{
    echo "hello"
}

export –f funcName

cd -- "$idir"
case $? in
  0) echo -n "0";;
  *) echo -n "cd error";;
esac

right_now=$(date +"%x %r %Z")
# limitation: $() commands are not tested
files=$(ls)