#!/bin/bash
echo "---------------------"
echo "- SH.INST.Basename  -"
echo "-    KO             -"
echo "---------------------"

# Erreur possible a l'execution : tentative de creation
# du fichier /tmp/<repertoire du script>/shell_Tr.Global\$0_ko.sh
# notamment, si le script est appele avec son path absolu 
#    /var/lib/postgresql/shell/exemples_shell/shell_Tr.Global\$0_ko.sh
#    touch: cannot touch `/tmp//var/lib/postgresql/shell/exemples_shell/shell_Tr.Global$0_ko.sh.en_cours_execution': No such file or directory
# fonctionne avec un appel via son path relatif (en etant positionne dans le bon repertoire) : 
#    ./shell_Tr.Global$0_ko.sh

nom_prog=$0
nom_verrou=/tmp/$nom_prog.en_cours_execution

# Fonction appelee en cas de sortie du script (normale ou inopinee), afin de supprimer le fichier verrou
function nettoie
{
        # Suppression de la copie de fichier
        if [ -f $nom_verrou ]
        then
                printf "   Suppression du fichier:%s\n" $nom_verrou
                rm $nom_verrou
        fi
        exit
}

# -------------------------------------------------------
#       M A I N 
# -------------------------------------------------------

# On ne doit avoir qu'une seul instance de ce script en cours d'execution 
# Si le fichier verrou est present cela indique que ce script est en cours d'execution 
if [ -f $nom_verrou ]
then
   printf "Fichier verrou : %s est present - Script %s en cours d'execution \n" $nom_verrou $nom_prog
   exit 1
fi

# En cas de reception des signaux EXIT, HUP, INT, QUIT, TERM -> appel de la fonction qui supprime le fichier verrou 
trap nettoie EXIT HUP INT QUIT TERM

# Positionne le fichier verrou : le present script est en cours d'execution 
touch $nom_verrou

# Boucle de traitement : 4 appels a la commande vmsat -s espac�s de 5 secondes chacun 
cpt=0
max_cpt=4
while [[ $cpt -lt $max_cpt ]]
do
   timbre=`date +%Y%m%d-%H%M%S`
   echo "------- Date - Heure systeme : [$timbre] -------------" 
   let cpt=cpt+1
   sleep 5
done
