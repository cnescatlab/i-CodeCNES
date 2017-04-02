#!/bin/ksh

#*******************************************************************************
#$<AM-V2.0>
# 
#$Type
#    PROG
#
#$Nom
#    chercher_apache.ksh
#
#$Projet
#    ELECTRA
#
#$Application
#    ELECTRA
#
#$Resume
#    Recherche de la presence d'un serveur Apache HTTP pour l'installation et 
#    la generation des sources ORESTE
#
#$Auteur
#   CAPGEMINI
#
#$Description
#    Programme shell de verification pour l'installation des produits ELECTRA 
#    et ORESTE a partir des sources des produits :
#
#    -> Verification de la presence d'un serveur Apache HTTP sur la machine
#
#$version
#	$Id: chercher_apache.ksh 2077 2013-10-28 12:49:22Z mbeuvelo $
#
#$Historique
#    VERSION:3.0:DM-ID:323:17/09/2013:Procedures automatiques d installation ELECTRA et ORESTE
#
#
#$FinHistorique
#
#$Mots-cles
#   ELECTRA
#
#$Acces
#   PUBLIC
#
#$Usage
#
#$Arguments
#
#$Remarques
#
#$Voir-Aussi
#
#$<>
#*******************************************************************************

# Le fichier properties doit etre dans le meme repertoire que le script
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# Fichier de log recupere par l'IHM en sortie du script
fichier_log="${chemin}"/chercher_apache.log

# Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm "${fichier_log}"

#*******************************************************************************
#                  Verification du pre-requis apache
#*******************************************************************************

echo "----------------------------------------------------------"
echo " Recherche de la presence du binaire httpd sur le systeme "
echo "----------------------------------------------------------"
echo

# Nom du binaire a rechercher pour apache
apache_bin='httpd'
# Chemin de recherche par defaut
[ -z "${default_path}" ] && default_path=('/users/soft')
# Chemin de recherche de secours si non trouve dans chemin de recherche par defaut
[ -z "${fallback_path}" ] && fallback_path=('/usr/local' '/etc')

# Recherche si deja installe dans le chemin de recherche par defaut,
# le resultat de la recherche est enregistre dans un tableau pour permettre
# de traiter plus facilement le cas ou plusieurs resultats sont renvoyes 
echo "Recherche dans le chemin par defaut : ${default_path[@]}"
echo
oldifs="${IFS}"
IFS=$'\n'
liste_binaire=($(find "${default_path[@]}" -type f -name "${apache_bin}" 2>/dev/null))
IFS="${oldifs}"

# Le binaire n'a pas ete trouve sous le chemin de recherche par defaut,
# on regarde sous le chemin de recherche de secours
if [ ! "${#liste_binaire[@]}" -ne 0 ]
then     
    echo "    => ${apache_bin} n'a pas ete trouve dans le chemin par defaut."
    echo
    echo "Recherche dans le chemin de secours : ${fallback_path[@]}"
    echo
    oldifs="${IFS}"
    IFS=$'\n'
    liste_binaire=($(find "${fallback_path[@]}" -type f -name "${apache_bin}" 2>/dev/null))
    IFS="${oldifs}"
fi

# Le binaire n'a ete trouve dans aucun des chemins de recherche,
# on considere qu'il est manquant
if [ "${#liste_binaire[@]}" -eq 0 ]
then     
    echo "    => ${apache_bin} n'a pas ete localise sur le systeme"
    echo "flag_${apache_bin}=0" >> "${fichier_log}"
# Si le binaire a finalement ete trouve, on renvoie le chemin vers le premier 
# resultat renvoye par la recherche
else
    echo "    => ${apache_bin} a ete localise sur le systeme : ${liste_binaire[@]}"
    echo "flag_${apache_bin}=1"                              >> "${fichier_log}"
    # Par exemple si Apache a ete trouve sous /usr/local/httpd-2.2.15/bin/httpd,
    # on renvoie /usr/local comme repertoire d'installation et non pas /usr/local/httpd-2.2.15,
    # cela est necessaire pour la compatibilite avec le script de generation d'oreste :
    #   => La variable rep_inst_ores_httpd deviendra /usr/local et apache_rep sera httpd-2.2.15
    echo "rep_inst_ores_${apache_bin}=$(readlink -f "$(dirname ${liste_binaire[0]})"/../..)"  >> "${fichier_log}"
fi

