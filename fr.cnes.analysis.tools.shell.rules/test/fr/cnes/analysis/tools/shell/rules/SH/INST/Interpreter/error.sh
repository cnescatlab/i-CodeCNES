echo "-----------------------"
echo "- SH.INST.Interpreter -"
echo "-    KO               -"
echo "-----------------------"
#
# Ce script effectue le calcul de la factorielle d'un nombre entier passe en parametre
# Comme le shell a utiliser n'est pas precise (1ere ligne du script), le shell courant sera utilise 
#    si bash : cela fonctionnera
#    si ksh  : aussi 
#    si csh  : ne fonctionne pas 
#

factorielle() 
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
	   echo "Factorielle de $1 = $factorielle"
	fi
}

factorielle 5


