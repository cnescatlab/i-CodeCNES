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
#    la generation des sources ELECTRA/ORESTE
#
#$Auteur
#   CAPGEMINI
#
#$Description
#    Programme shell de verification pour l'installation des produits ELECTRA
#    et ORESTE a partir des sources des produits :
#
#    -> Verification de la presence d'un serveur Apache HTTP sur la machine
#          * Recherche du binaire httpd
#          * Recherche du fichier de configuration par defaut du daemon httpd
#
#$version
#       $Id: nettoyer_installation.ksh 2077 2013-10-28 12:49:22Z mbeuvelo $
#
#$Historique
#    VERSION:3.0:DM-ID:323:27/09/2013:Procedures automatiques d installation ELECTRA et ORESTE
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
#$<>
#*******************************************************************************

current_user="$(id -u -n)"

if [ "${current_user}" != "root" ]
then
   echo "! Il faut être root -- Sortie du script !"
   exit
fi

chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

nettoyer_repertoire() {
    if [ -d "$1" ] && [ "$1" != / ]
    then
        rm -rf "$1"
    fi
}

nettoyer_repertoire "${tmpdir}"
nettoyer_repertoire "${rpmdir}"
