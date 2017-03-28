#!/bin/ksh
#*****************************************************************
#$<AM-V2.0>
# 
#$Type
#	PROG
#
#$Nom
#	chercher_prerequis.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#

#$Resume
#	"Verification des pre-requis pour l'installation et la generation des sources ELECTRA/ORESTE"
#
#$Auteur
#   CAPGEMINI
#
#$Description
#	Programme shell de verification pour l'installation des produits ELECTRA et ORESTE a partir des sources des produits
#    1 - Verification des pre-requis systemes de type .rpm
#           * Verifications des librairies 32 bits suivantes : libX11.a, libX11.so et libxml2.so et 
#           * Verifications des librairies 64 bits suivantes : libsqlite3.so, libX11.a, libX11.so, 
#			libxerces-c.so, libxerces-depdom.so, libapr-1.so, libaprutil-1.so. 
#    2 - Verification de la disponibilite des librairies (Global Insigth et Infield Gateway)
#           Attention : GlobalInsight et Infield Energy Gateway ayant des licenses payantes, ils ne sont livres qu' aux personnes CNES, 
#           i.e. que dans le cas de la procedure livrant les sources (et encore pas forcement). 
#           Si ces 2 produits ne se trouvent pas dans le produit ELECTRA livre, 
#           il faut demander a l' utilisateur s'il les a qqpart chez lui 
#           et si oui ou, sinon ils ne seront pas installes, mais ce n'est pas grave, 
#           ce sont des options (non obligatoire pour le bon deroulement d'Oreste), 
#           il faudra juste informer l'utilisateur qu'il pourra les rajouter a posteriori (cf doc) 
#           et qu'il ne disposera pas en attendant des couches GI et des infos plateformes petrolieres.
#
#$version
#	$Id: chercher_prerequis.ksh 2077 2013-10-28 12:49:22Z mbeuvelo $
#
#$Historique
#    VERSION:3.0:DM-ID:323:16/10/2013:Procedures automatiques d installation ELECTRA et ORESTE
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

# Specification des chemins installation/creation/positionnement des archives
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# fichier de log recupere par l'IHM en sortie du script
export fichier_log="${chemin}"/chercher_prerequis.log

#Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm -f "${fichier_log}"

#*******************************************************************************
#                  Verification des pre-requis systemes de type .rpm
#*******************************************************************************

echo "1/2 Verification des pre-requis systemes de type .rpm "
echo "------------------------------------------------------"

# *****************************************************
# Simple verification des elements livres
#  !! toute modification sur ces noms 
#       doit etre repercutee dans la suite du script !!
# *****************************************************
liste_repertoires="UTILITAIRES PSIMU pre_ELECTRA ELECTRA pre_ORESTE ORESTE DOCUMENTATION"
for i in ${liste_repertoires[*]}
do
    if [ ! -d $archives_dir/$i ]
    then
        echo "!! WARNING : Il manque le repertoire entree $archives_dir/$i "
        echo "flag_livraison=0" >> $fichier_log 
    fi
done
echo "flag_livraison=1" >> $fichier_log

# *****************************************
# Complements librairies (mode 32 bits) 
#  => specifiques configuration ASGARD/OLYMPE
# *****************************************
echo "** Verification de la disponibilite des librairies systemes"

#**********
# LIBX11.a
#**********
flag_lib_sys=1
if [ ! -e /usr/X11R6/lib/libX11.a ]
then
    echo "!! WARNING : manque element systeme /usr/X11R6/lib/libX11.a"
    echo "flag_libX11.a_32=0" >> $fichier_log 
    flag_lib_sys=0
else
    echo "flag_libX11.a_32=1" >> $fichier_log 
fi

#***********
# LIBX11.so
#***********
if [ ! -e /usr/X11R6/lib/libX11.so.6.2 ]
then
    echo "!! WARNING : manque element systeme /usr/X11R6/lib/libX11.so.6.2"
    echo "flag_libX11.so_32=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libX11.so_32=1" >> $fichier_log 
fi


#**********
#LIBXML2.so
#**********
if [ ! -e /usr/lib/libxml2.so.2.6.23 ]
then
    echo "!! WARNING : manque element systeme /usr/lib/libxml2.so.2.6.23"
    echo "flag_libxml2.so_32=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libxml2.so_32=1" >> $fichier_log 
fi



# *************************************
# Complements librairies (mode 64 bits) 
#  => specifiques configuration ASGARD/OLYMPE
# *************************************


#**************
#libsqlite3.so
#**************
if [ ! -e /usr/lib64/libsqlite3.so.0.8.6 ]
then
    echo "!! WARNING : manque element systeme usr/lib64/libsqlite3.so.0.8.6"
    echo "flag_libsqlite3.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libsqlite3.so_64=1" >> $fichier_log 
fi

#**********
#libx11.a
#**********
if [ ! -e /usr/X11R6/lib64/libX11.a ]
then
    echo "!! WARNING : manque element systeme /usr/X11R6/lib64/libX11.a"
    echo "flag_libX11.a_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libX11.a_64=1" >> $fichier_log 
fi

#************
#libX11.so
#************
if [ ! -e /usr/X11R6/lib64/libX11.so.6.2 ]
then
    echo "!! WARNING : manque element systeme /usr/X11R6/lib64/libX11.so.6.2"
    echo "flag_libX11.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libX11.so_64=1" >> $fichier_log 
fi

#****************
#libxerces-c.so
#****************
if [ ! -e /usr/lib64/libxerces-c.so.27.0 ]
then
    echo "!! WARNING : manque element systeme /usr/lib64/libxerces-c.so.27.0"
    echo "flag_libxerces-c.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libxerces-c.so_64=1" >> $fichier_log 
fi

#********************
#libxerces-depdom.so
#********************
if [ ! -e /usr/lib64/libxerces-depdom.so.27.0 ]
then
    echo "!! WARNING : manque element systeme /usr/lib64/libxerces-depdom.so.27.0"
    echo "flag_libxerces-depdom.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libxerces-depdom.so_64=1" >> $fichier_log 
fi

#***********
#libapr-1.so
#***********
res=`find /lib* /usr/lib* -name "libapr-1.so.0.2.2" | wc -l`
if [ $res -eq 0  ]
then
    echo "!! WARNING : manque element systeme libapr-1.so.0.2.2"
    echo "flag_libapr-1.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libapr-1.so_64=1" >> $fichier_log 
fi

#****************
#libaprutil-1.so
#****************
res=`find /lib* /usr/lib* -name "libaprutil-1.so.0.2.2" | wc -l`
if [ $res -eq 0  ]
then
    echo "!! WARNING : manque element systeme libaprutil-1.so.0.2.2"
    echo "flag_libaprutil-1.so_64=0" >> $fichier_log
    flag_lib_sys=0
else
    echo "flag_libaprutil-1.so_64=1" >> $fichier_log
fi

# 
echo "flag_lib_sys=$flag_lib_sys" >> $fichier_log


#*******************************************************************************
#                  Verification des pre-requis autres que systeme
#*******************************************************************************

#ELECTRA
########
if [ $gen_electra -eq 1 ]
then
    liste_binaires="gcc-4.7.2 openmpi PSIMU"
    for i in ${liste_binaires[*]} 
    do
        #on regarde si deja installe, en priorite sous /users/soft
        #si oui, on le cherche et on recupere son chemin
        nb_binaire=`find /users/soft/ -name $i 2>/dev/null | wc -l`
        if [ $nb_binaire -ge 1 ]
        then     
            binaire=`find /users/soft/ -name $i 2>/dev/null | head -1`
            echo "$i est deja installe sur la machine $binaire"
            if [ "$i" = gcc-4.7.2 ]
                then
                    echo "flag_g95=1" >> $fichier_log
                    echo "rep_g95=$binaire" >> $fichier_log
                else
                    echo "flag_$i=1" >> $fichier_log
                    echo "rep_$i=$binaire" >> $fichier_log
                fi
        else
            # la librairie n'a pas ete trouvee sous soft, on regarde sous /usr/local et /etc
            nb_binaire=`find /usr/local /etc -name $i 2>/dev/null | wc -l`
            if [ $nb_binaire -ge 1 ]
            then     
                binaire=`find /usr/local /etc -name $i 2>/dev/null | head -1`
                echo "$i est deja installe sur la machine $binaire"
                if [ "$i" = gcc-4.7.2 ]
                then
                    echo "flag_g95=1" >> $fichier_log
                    echo "rep_g95=$binaire" >> $fichier_log
                else
                    echo "flag_$i=1" >> $fichier_log
                    echo "rep_$i=$binaire" >> $fichier_log
                fi
            else
                #si non, on sort
                echo "$i n'est pas installe sur la machine"
                echo "flag_$i=0" >> $fichier_log
            fi
        fi
    done
fi

#ORESTE
#######
if [ $gen_oreste -eq 1 ]
then
    #Autres pre-requis ORESTE
    flag_lib_oreste=1
    
    liste_binaire="mapfish firefox gdal mapserver curl gd m4 proj"
    for i in ${liste_binaire[*]} 
    do
        #on regarde si deja installe, en priorite sous /users/soft
        #si oui, on le cherche et on recupere son chemin
        nb_binaire=`find /users/soft/ -iname $i-* 2>/dev/null | wc -l`
        if [ $nb_binaire -ge 1 ]
        then     
            binaire=`find /users/soft/ -iname $i-* 2>/dev/null | head -1`
            echo "$i est deja installe sur la machine $binaire"
            echo "flag_$i=1" >> $fichier_log
            echo "rep_$i=$binaire" >> $fichier_log
        else
            # la librairie n'a pas ete trouvee sous soft, on regarde sous /usr/local et /etc
            nb_binaire=`find /usr/local /etc -iname $i-* 2>/dev/null | wc -l`
            if [ $nb_binaire -ge 1 ]
            then     
                binaire=`find /usr/local /etc -iname $i-* 2>/dev/null | head -1`
                echo "$i est deja installe sur la machine $binaire"
                echo "flag_$i=1" >> $fichier_log
                echo "rep_$i=$binaire" >> $fichier_log
            else
                #si non, on sort
                echo "$i n'est pas installe sur la machine"
                echo "flag_$i=0" >> $fichier_log
                flag_lib_oreste=0
            fi
        fi
    done
fi

#lib oreste
echo "flag_lib_oreste=$flag_lib_oreste" >> $fichier_log
if [ $flag_lib_oreste -eq 1 ]
then
    rep_lib_oreste=`dirname $binaire`
    echo "rep_lib_oreste=$rep_lib_oreste" >> $fichier_log
fi

#*******************************************************************************
#                  Verification de la disponibilite des librairies GD et IEG
#*******************************************************************************

echo "2/2 Verification de la disponibilite des librairies   "
echo "------------------------------------------------------"

liste_binaire="GlobalInsight InfieldEnergyGateway"
for i in ${liste_binaire[*]} 
do
    #on regarde si deja installe, en priorite sous /users/soft
    #si oui, on le cherche et on recupere son chemin
    nb_binaire=`find /users/soft/ -type d -name $i 2>/dev/null | wc -l`
    if [ $nb_binaire -ge 1 ]
    then     
        binaire=`find /users/soft/ -type d -name $i 2>/dev/null | head -1`
        echo "$i est deja installe sur la machine $binaire"
        echo "flag_$i=1" >> $fichier_log
        echo "rep_$i=$binaire" >> $fichier_log
    else
        # la librairie n'a pas ete trouvee sous soft, on regarde sous /usr/local et /etc
        nb_binaire=`find /usr/local /etc -type d -name $i 2>/dev/null | wc -l`
        if [ $nb_binaire -ge 1 ]
        then     
            binaire=`find /usr/local /etc -type d -name $i 2>/dev/null | head -1`
            echo "$i est deja installe sur la machine $binaire"
            echo "flag_$i=1" >> $fichier_log
            echo "rep_$i=$binaire" >> $fichier_log
        else
            #si non, on sort
            echo "$i n'est pas installe sur la machine"
            echo "flag_$i=0" >> $fichier_log
        fi
    fi
done

