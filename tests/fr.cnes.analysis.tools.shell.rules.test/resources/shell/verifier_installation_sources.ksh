#!/bin/ksh
#*****************************************************************
#$<AM-V2.0>
# 
#$Type
#	PROG
#
#$Nom
#	verifier_installation_sources.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#
#$Resume
#   "Verification pour l'installation et la generation d'ELECTRA/ORESTE"
#
#$Auteur
#   CAPGEMINI
#
#$Description
#   Programme shell de verification pour l'installation des produits ELECTRA et ORESTE a partir des sources des produits
#    1 - Verification des pre-requis systemes de type .rpm
#           * Verifications des librairies 32 bits suivantes : libX11.a, libX11.so et libxml2.so et 
#           * Verifications des librairies 64 bits suivantes : libsqlite3.so, libX11.a, libX11.so, 
#           libxerces-c.so, libxerces-depdom.so, libapr-1.so, libaprutil-1.so. 
#    2 - Verification de la disponibilite des librairies (Global Insigth et Infield Gateway)
#           Attention : GlobalInsight et Infield Energy Gateway ayant des licenses payantes, ils ne sont livres qu' aux personnes CNES, 
#           i.e. que dans le cas de la procedure livrant les sources (et encore pas forcement). 
#           Si ces 2 produits ne se trouvent pas dans le produit ELECTRA livre, 
#           il faut demander a l' utilisateur s'il les a qqpart chez lui 
#           et si oui ou, sinon ils ne seront pas installes, mais ce n'est pas grave, 
#           ce sont des options (non obligatoire pour le bon deroulement d'Oreste), 
#           il faudra juste informer l'utilisateur qu'il pourra les rajouter a posteriori (cf doc) 
#           et qu'il ne disposera pas en attendant des couches GI et des infos plateformes petrolieres.
#$version
#	$Id: verifier_installation_sources.ksh 2496 2014-07-18 09:27:04Z mbeuvelo $
#
#$Historique
#    VERSION:3.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.0.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.0:DM-ID:323:20/09/2013:Procedures automatiques d installation ELECTRA et ORESTE
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
export fichier_log="${chemin}"/verifier_installation_sources.log

#Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm -f "${fichier_log}"

#*******************************************************************************
#                  Verification des pre-requis systeme
#*******************************************************************************
echo "1/3 Verification des prerequis autres que systeme     "
echo "------------------------------------------------------"

if [ ${flag_binaries} != 1 ]
then

    # ********************************************************
    # Deploiement RPM en complement dans repertoire de travail
    #  => specifiques configuration ASGARD
    # ********************************************************

    echo "** Deploiment des rpm complementaires temporaires..."

    # Creation du repertoire rpmdir
    # *****************************
    if [ -d "${rpmdir}" ]
    then
        echo " WARNING, repertoire $i existant: nettoyage active"
        rm -rf "${rpmdir}"
    fi
    mkdir -p "${rpmdir}"
    cd "${rpmdir}"
    liste_rpm="$(ls "${archives_dir}"/UTILITAIRES/RPM_complements/*.rpm)"
    for i in ${liste_rpm[*]}
    do
        if [ -e "$i" ]
        then
            rpm2cpio "$i" | cpio -di  > /tmp/rpm2cpio_$$.log 2>&1
        fi
    done
    # Effacer fichiers configuration "XXX.la" car ils font reference a racine systeme "/"
    find "${rpmdir}" -name "*.la" | xargs rm
    # Modification script ld GNU pour chemin libc_nonshared.a et libpthread_nonshared.a
    sed -i 's|usr/lib/libc_nonshared.a|'"${rpmdir}"'/usr/lib/libc_nonshared.a|g'             "${rpmdir}"/usr/lib/libc.so
    sed -i 's|usr/lib/libpthread_nonshared.a|'"${rpmdir}"'/usr/lib/libpthread_nonshared.a|g' "${rpmdir}"/usr/lib/libpthread.so

    echo "** Deploiment des rpm complementaires temporaires OK"
fi

# Lien vers Java
export JAVA_HOME="${JAVA_HOME:-/usr/java/jdk1.5.0_22}"
export PATH="${JAVA_HOME}"/jre/bin:"${JAVA_HOME}"/bin:"${PATH}"

# Mise a jour de l'environnement
export PATH="${rpmdir}/bin:${rpmdir}/usr/bin:${PATH}"
export LD_LIBRARY_PATH="${rpmdir}/usr/lib:${rpmdir}/usr/lib64"
export TEMP_FLAGS="-I${rpmdir}/usr/include -I${rpmdir}/include"
export TEMP_LDFLAGS="-L${rpmdir}/usr/lib"

# *****************************************
# Complements librairies (mode 32 bits) 
#  => specifiques configuration ASGARD/OLYMPE
# *****************************************

if [ ${flag_binaries} != 1 ]
then
    echo "** Deploiment des librairies systemes..."
    #Utilisateur veut les installer
    if [ $flag_lib_sys -eq 1 ]
    then

        #Initialisation du flag global
        flag_lib_32=1

        #**********
        # LIBX11.a
        #**********
        if [ ! -e /usr/X11R6/lib/libX11.a ]
        then
            if [ ! -e $archives_dir/UTILITAIRES/libX11.a ]
            then
                echo "!! ERREUR : manque element systeme /usr/X11R6/lib/libX11.a"
                echo "flag_libX11.a_32=-1" >> $fichier_log
                flag_lib_32=-1
            fi
        fi

        #***********
        # LIBX11.so
        #***********
        if [ ! -e /usr/X11R6/lib/libX11.so.6.2 ]
        then
            if [ ! -e $rpmdir/usr/X11R6/lib/libX11.so.6.2 ]
            then
                echo "!! ERREUR : manque element systeme /usr/X11R6/lib/libX11.so.6.2"
                echo "flag_libX11.so_32=-1" >> $fichier_log
                flag_lib_32=-1
            fi
        fi

        #**********
        # LIBXML2.so
        #**********
        if [ ! -e /usr/lib/libxml2.so.2.6.23 ]
        then
            if [ ! -e $rpmdir/usr/lib/libxml2.so.2.6.23 ]
            then 
                echo "!! ERREUR : manque element systeme /usr/lib/libxml2.so.2.6.23"
                echo "flag_libxml2.so_32=-1" >> $fichier_log
                flag_lib_32=-1
            fi
        fi

        # Ecrit le bilan des librairies 32bits
        echo "flag_lib_32=$flag_lib_32" >> $fichier_log

        # *************************************
        # Complements librairies (mode 64 bits) 
        #  => specifiques configuration ASGARD/OLYMPE
        # *************************************

        # Initialisation du flag global
        flag_lib_64=1

        #**************
        # libsqlite3.so
        #**************
        if [ ! -e /usr/lib64/libsqlite3.so.0.8.6 ]
        then
            if [ ! -e $rpmdir/usr/lib64/libsqlite3.so.0.8.6 ]
            then
                echo "!! ERREUR : manque element systeme usr/lib64/libsqlite3.so.0.8.6"
                echo "flag_libsqlite3.so_64=-1" >> $fichier_log
                flag_lib_64=-1
            fi
        fi

        #**********
        # libx11.a
        #**********
        if [ ! -e /usr/X11R6/lib64/libX11.a ]
        then
            if [ ! -e $rpmdir/usr/X11R6/lib64/libX11.a ]
            then
                echo "!! ERREUR : manque element systeme /usr/X11R6/lib64/libX11.a"
                echo "flag_libX11.a_64=-1" >> $fichier_log
                flag_lib_64=-1
            fi  
        fi

        #************
        # libX11.so
        #************
        if [ ! -e /usr/X11R6/lib64/libX11.so.6.2 ]
        then
            if [ ! -e $rpmdir/usr/X11R6/lib64/libX11.so.6.2 ]
            then
                echo "!! ERREUR : manque element systeme /usr/X11R6/lib64/libX11.so.6.2"
                echo "flag_libX11.so_64=-1" >> $fichier_log
                flag_lib_64=-1
            fi  
        fi

        #****************
        # libxerces-c.so
        #****************
        if [ ! -e /usr/lib64/libxerces-c.so.27.0 ]
        then
            if [ ! -e $rpmdir/usr/lib64/libxerces-c.so.27.0 ]
            then
                echo "!! ERREUR : manque element systeme /usr/lib64/libxerces-c.so.27.0 dans RPM complement"
                echo "flag_libxerces-c.so_64=-1" >> $fichier_log
                flag_lib_64=-1
            fi
        fi

        #********************
        # libxerces-depdom.so
        #********************
        if [ ! -e /usr/lib64/libxerces-depdom.so.27.0 ]
        then
            if [ ! -e $rpmdir/usr/lib64/libxerces-depdom.so.27.0 ]
            then
                echo "!! ERREUR : manque element systeme /usr/lib64/libxerces-depdom.so.27.0 dans RPM complement"
                echo "flag_libxerces-depdom.so_64=-1" >> $fichier_log
                flag_lib_64=-1
            fi
        fi

        #***********
        # libapr-1.so
        #***********
        if [ ! -e $archives_dir/UTILITAIRES/lib/libapr-1.so.0.2.2  ]
        then
            echo "!! ERREUR : manque element livraison $archives_dir/UTILITAIRES/lib/libapr-1.so.0.2.2"
            echo "flag_libapr-1.so_64=-1" >> $fichier_log
            flag_lib_64=-1
        fi

        #****************
        # libaprutil-1.so
        #****************
        if [ ! -e $archives_dir/UTILITAIRES/lib/libaprutil-1.so.0.2.2 ]
        then
            echo "!! ERREUR : manque element livraison $archives_dir/UTILITAIRES/libaprutil-1.so.0.2.2"
            echo "flag_libaprutil-1.so_64=-1" >> $fichier_log
            flag_lib_64=-1
        fi

        # Ecrit le bilan des librairies 64bits
        echo "flag_lib_64=$flag_lib_64" >> $fichier_log

    fi
else
    echo "flag_lib_32=1" >> $fichier_log
    echo "flag_lib_64=1" >> $fichier_log
fi

#*******************************************************************************
#                  Verification des pre-requis autres que systeme
#*******************************************************************************
echo "2/3 Verification des prerequis autres que systeme     "
echo "------------------------------------------------------"

#ELECTRA
########
if [ $gen_electra -eq 1 ]
then
    #utilisateur ne veut pas installer le produit OPENMPI
    if [ $flag_openmpi -eq 0 ]
    then
        #on regarde le chemin fourni
        if [ ! -z "$rep_openmpi" ] && [ -d $rep_openmpi ]
        then     
            echo "openmpi est deja installe sur la machine : $rep_openmpi"
            echo "flag_openmpi=0" >> $fichier_log
            echo "rep_openmpi=$rep_openmpi" >> $fichier_log
        else
            #si non, on sort : absent et utilisateur ne veut pas l'installer => pb
            echo "flag_openmpi=-1" >> $fichier_log
        fi
    else
        if [ ${flag_binaries} != 1 ]
        then
            if [ -e $archives_dir/pre_ELECTRA/$arch_openmpi ]
            then
                echo "flag_openmpi=1" >> $fichier_log
            else
                echo "Fichier archive $arch_openmpi absent : $archives_dir/UTILITAIRES/$arch_openmpi"
                echo "flag_openmpi=-2" >> $fichier_log
            fi
        else
            echo "flag_openmpi=1" >> $fichier_log
        fi
   fi

    # G95 n'est utilise que lors de la compilation des sources,
    if [ ${flag_binaries} != 1 ]
    then
        # L'utilisateur ne veut pas installer le produit G95
        if [ "${flag_g95}" -eq 0 ]
        then
            # On regarde le chemin fourni
            if [ ! -z "${rep_g95}" ] && [ -d "${rep_g95}" ]
            then     
                echo "g95 est deja installe sur la machine : ${rep_g95}"
                echo "flag_g95=0" >> "${fichier_log}"
                echo "rep_g95=${rep_g95}" >> "${fichier_log}"
            else
                # Sinon, on sort : absent et utilisateur ne veut pas l'installer => pb
                echo "flag_g95=-1" >> "${fichier_log}"
            fi
        else
            if [ -e "${archives_dir}"/UTILITAIRES/"${arch_g95}" ]
            then
                echo "flag_g95=1" >> "${fichier_log}"
            else
                echo "Fichier archive $arch_g95 absent : ${archives_dir}/UTILITAIRES/${arch_g95}"
                echo "flag_g95=-2" >> "${fichier_log}"
            fi
        fi
    else
        echo "flag_g95=0" >> "${fichier_log}"
    fi
   
   #utilisateur ne veut pas installer le produit PSIMU
   if [ $flag_PSIMU -eq 0 ]
   then
      #on regarde le chemin fourni
      if [ ! -z "$rep_PSIMU" ] && [ -d $rep_PSIMU ]
      then     
         echo "PSIMU est deja installe sur la machine : $rep_PSIMU"
         echo "flag_PSIMU=0" >> $fichier_log
         echo "rep_PSIMU=$rep_PSIMU" >> $fichier_log
      else
        #si non, on sort : absent et utilisateur ne veut pas l'installer => pb
        echo "flag_PSIMU=-1" >> $fichier_log
      fi
    else
        if [ ${flag_binaries} != 1 ]
        then
            if [ -e $archives_dir/UTILITAIRES/$arch_PSIMU ]
            then
                echo "flag_PSIMU=1" >> $fichier_log
            else
                echo "Fichier archive $arch_PSIMU absent : $archives_dir/UTILITAIRES/$arch_PSIMU"
                echo "flag_PSIMU=-2" >> $fichier_log
            fi
        else
            echo "flag_PSIMU=1" >> $fichier_log
        fi
    fi
fi

#ORESTE
#######
if [ $gen_oreste -eq 1 ]
then
#Autres pre-requis ORESTE
#liste_prerequis="MapFish-1.1 firefox-3 gdal-1.7.2 mapserver-5.6.3 curl-7.14.1 gd-2.0.35 m4-1.4.11 proj-4.7.0"
   liste_binaire="mapfish firefox gdal mapserver curl gd m4 proj"
   for i in ${liste_binaire[*]} 
   do
     flag="\$flag_${i}"
     val_flag=`eval echo $flag`
     #utilisateur ne veut pas installer le produit
     if [ $val_flag -eq 0 ]
     then
        #on regarde le chemin fourni
        rep="\$rep_${i}"
        val_rep=`eval echo $rep`
        if [ ! -z "$val_rep" ] && [ -d $val_rep ]
        then     
           echo "$i est deja installe sur la machine $val_rep"
           echo "flag_$i=0" >> $fichier_log
           echo "rep_$i=$val_rep" >> $fichier_log
        else
          #si non, on sort : absent et utilisateur ne veut pas l'installer => pb
          echo "flag_$i=-1" >> $fichier_log
        fi
     else
        if [ ${flag_binaries} != 1 ]
        then
           arch="\$arch_${i}"
           val_arch=`eval echo $arch`
           if [ -e $archives_dir/pre_ORESTE/$val_arch ]
           then
              echo "flag_$i=1" >> $fichier_log
           else
              echo "Fichier archive $val_arch absent : $archives_dir/UTILITAIRES/$val_arch"
              echo "flag_$i=-2" >> $fichier_log
           fi
        else
            echo "flag_$i=1" >> $fichier_log
        fi
     fi
   done
fi

#*******************************************************************************
#                  Verification de la disponibilite des librairies GD et IEG
#*******************************************************************************

echo "3/3 Verification de la disponibilite des librairies   "
echo "------------------------------------------------------"


# ***************************************************************************
# Global Insigth
# ***************************************************************************

echo "  --> Disponibilite Global Insight"
#Si on installe
if [ $flag_GlobalInsight = 1 ]
then
    if [ ${flag_binaries} != 1 ]
    then
        if [ -e $archives_dir/pre_ORESTE/$arch_GlobalInsight ]
        then
            echo "flag_GlobalInsight=1" >> $fichier_log
            echo "       ==>  OK"
        else
            echo "       ==>  WARNING, absence archive global insight : $archives_dir/pre_ORESTE/$arch_GlobalInsight"
            echo "flag_GlobalInsight=-2" >> $fichier_log
        fi
    else
        echo "flag_GlobalInsight=1" >> $fichier_log
    fi
else
    # on regarde le chemin fourni
    if [ -n "$rep_GlobalInsight" ] && [ -d $rep_GlobalInsight ]
    then
        echo "GlobalInsight est deja installe sur la machine $rep_GlobalInsight"
        echo "flag_GlobalInsight=0" >> $fichier_log
        echo "rep_GlobalInsight=$rep_GlobalInsight" >> $fichier_log
    else
        #si non, on sort : absent et utilisateur ne veut pas l'installer => pb
        echo "flag_GlobalInsight=0" >> $fichier_log
    fi
fi


# ***************************************************************************
# Infield Energy Gateway
# ***************************************************************************

echo "  --> Disponibilite base de donnees Infield Energy Gateway"
# Si on installe
if [ $flag_InfieldEnergyGateway = 1 ]
then
    if [ ${flag_binaries} != 1 ]
    then
        if [ -e $archives_dir/pre_ORESTE/$arch_InfieldEnergyGateway ]
        then
            # si installation demandee par utilisateur : c'est ok, sinon message informatif
            echo "flag_InfieldEnergyGateway=1" >> $fichier_log
            echo "       ==>  OK"
        else
            echo "       ==>  WARNING, absence archive Infield Energy Gateway : $archives_dir/pre_ORESTE/$arch_InfieldEnergyGateway"
            echo "flag_InfieldEnergyGateway=-2" >> $fichier_log
        fi
    else
        echo "flag_InfieldEnergyGateway=1" >> $fichier_log
    fi
else
    # on regarde le chemin fourni
    if [ -n "$rep_InfieldEnergyGateway" ] && [ -d $rep_InfieldEnergyGateway ]
    then
        echo "La base Infield Energy Gateway est deja installe sur la machine $rep_InfieldEnergyGateway"
        echo "flag_InfieldEnergyGateway=0" >> $fichier_log
        echo "rep_InfieldEnergyGateway=$rep_InfieldEnergyGateway" >> $fichier_log
    else
        # si non, on sort : absent et utilisateur ne veut pas l'installer => pb
        echo "flag_InfieldEnergyGateway=0" >> $fichier_log
    fi
fi
