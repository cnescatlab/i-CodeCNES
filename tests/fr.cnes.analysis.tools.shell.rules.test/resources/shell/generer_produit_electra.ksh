#!/bin/ksh +f
#*****************************************************************
#$<AM-V2.0>
#
#$Type
#	PROG
#
#$Nom
#	generer_produit_electra.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#
#$Resume
#	"Generation d ELECTRA"
#
#$Auteur
#   CAPGEMINI
#
#$Description
# Ce script a pour but de deployer ELECTRA sous environnement ASGARD du CSG
# Il prepare un autre script pour archiver les elements necessaires au deploiement sous OLYMPE
# Il tient compte de packages RPM manquant pour arriver a generer ELECTRA/ORESTE et leurs prerequis
# Il est a noter les points suivants :
#     - La non adequation de versions de prerequis entre ceux livres avec PSIMU et ceux attendus par
#       ELECTRA : des modifications sont apportees a l archive ELECTRA comme paliatif
#     - L'environnement ASGARD dans lequel a ete deploye ce script ne permettait pas
#       la compilation en mode 32 bits (manque de glibc-devel-32bit-2.4-31.54.x86_64.rpm ).
#     - Un repertoire de deploiement temporaire de packages RPM manquant est genere (c.f $rpmdir).
#     - Des complements de variables d environnement (LD_LIBRARY_PATH, PATH, CFLAGS, ...)
#       sont realises tout au long du script.
#     - Une modification est apportee au script dep_bibms de l'archive PSIMU, rajout de l'option -H a
#       la commande systeme grep pour tenir compte de l'ensemble des fichiers de conf.
#     - Des modifications sont apportees pour tenir compte du complment de la librairie X11 en mode 32 bits
#       Ces modifications concernent l'installation de GENESIS et l'archive ELECTRA
#     - GDAL apporte un souci de comportement de compilation, les packages RPM JPEG/TIFF/PNG/ZLIB
#       ont ete redeployes specifiquement pour GDAL (dans $tmpdir, c.f. etape compilation GDAL).
#     - La generation de GD, phase configure, emet des messages sur l absence des autotools  (autoconf,automake,...)
#       Cette absence n empeche pas la compilation/generation de GD.
#     - Le JDK 1.6 deploye dans /opt d ASGARD est utilise. Le script contient des instructions commentees pour
#       tenir compte d un eventuel deploiement des RPM java-1.5.0 present sur le DVD suse Server 10 SP2 x86_64
#     - Le script configure de MapServer ne prend pas en compte les variables environnements CPPFLAGS et LDFLAGS
#       Il faut donc initier les variables CC et CXX pour tenir compte du contenu des options CPPFLAGS et LDFLAGS
#     - Pour le serveur APACHE HTTP, il faut redefinir le Group dans le fichier de conf conf/httpd.conf.
#       Le Group est positionne par defaut a daemon, mais cela le rendra "invisible" au groupe users comme ce
#       serveur est lance par root.
#       Donc il faut le positionner Group a users.
#
#$version
#	$Id: generer_produit_electra.ksh 2496 2014-07-18 09:27:04Z mbeuvelo $
#
#$Historique
#    VERSION:3.1:FA-ID:705:26/03/2014:Les icones de lancement des produits electra et oreste ne fonctionnent pas
#
#    VERSION:3.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
#
#    VERSION:3.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.0.1:FA-ID:705:26/03/2014:Les icones de lancement des produits electra et oreste ne fonctionnent pas
#
#    VERSION:3.0.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
#
#    VERSION:3.0.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.0:DM-ID:323:16/09/2013:Procedures automatiques d installation ELECTRA et ORESTE
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
# Mode verbose (ligne suivante a decommenter pour activer mode verbose)
# set -x

### FA 707: spécificité Asgard/Olympe ########################
#pour ne pas être limité sur la taille des fichiers notamment Electra.tar
ulimit unlimited 
#######################################################################################

# *********************
# Differentes fonctions
# *********************
nettoie_tmpdir() {
    if [ -d "${tmpdir}" ]
    then
        if  [ "${tmpdir}" != "/" ]
        then
            rm -rf "${tmpdir}"/*
        else
            echo "!! ERREUR, souci de definition de repertoire temporaire pour nettoyage : ${tmpdir}"
            exit 1
        fi
    else
        echo "!! WARNING, appel nettoie_tmpdir() sur repertoire tmpdir(=${tmpdir}) non existant"
    fi
}
# nettoie_tmpdir()


# ***************************************************************************
# Specification des chemins installation/creation/positionnement des archives
# ***************************************************************************
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# fichier de log recupere par l'IHM en sortie du script
export fichier_log="${chemin}"/generer_produit_electra.log

#Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm -f "${fichier_log}"

# ***************************
# Creation de l'arborescence
# ***************************
liste_repertoires=("${rep_inst_elec}" "${rep_inst_elec}"/misc "${tmpdir}")
for i in "${liste_repertoires[@]}"
do
    if [ -d "$i" ]
    then
        echo " WARNING, repertoire $i existant: nettoyage active"
        rm -rf "$i"
    fi
    mkdir -p "$i"
done

# Lien vers Java
export JAVA_HOME="${JAVA_HOME:-/usr/java/jdk1.5.0_22}"
export PATH="${JAVA_HOME}"/jre/bin:"${JAVA_HOME}"/bin:"${PATH}"

# Mise a jour de l'environnement
export PATH="${rpmdir}/bin:${rpmdir}/usr/bin:${PATH}"
export LD_LIBRARY_PATH="${rpmdir}/usr/lib:${rpmdir}/usr/lib64"
export TEMP_FLAGS="-I${rpmdir}/usr/include -I${rpmdir}/include"
export TEMP_LDFLAGS="-L${rpmdir}/usr/lib"

#########################################################################


# *****************************************
# Complements librairies (mode 32 bits)
#  => specifiques configuration ASGARD/OLYMPE
# *****************************************

echo "** Deploiment des librairies systemes..."

mkdir -p "${rep_inst_elec}"/misc/lib
mkdir -p "${rep_inst_elec}"/misc/lib64


#definition fonction lier_librairie()
lier_librairie () {
    cible="$1"
    nom_lien="$2"

    if [ -e "${cible}" ]
    then
        ln -s "${cible}" "${nom_lien}"
    else
        echo "!! ERREUR : L'element ${cible} n'est pas present sur le systeme."
        exit 1
    fi
}


# Utilisateur veut les installer
if [ "${flag_lib_sys}" = 1 ]
then
    cd "${rep_inst_elec}"/misc/lib

    #**********
    # LIBX11.a
    #**********
    if [ ! -e /usr/X11R6/lib/libX11.a ]
    then
        if [ -e "${archives_dir}"/UTILITAIRES/libX11.a ]
        then
            cp "${archives_dir}"/UTILITAIRES/libX11.a "${rep_inst_elec}"/misc/lib/ >> /tmp/libs_$$.log 2>&1
        else
            echo "!! ERREUR : manque element systeme /usr/X11R6/lib/libX11.a"
            echo "   Probleme lors du deploiement automatique de libX11.a"
            echo "   Manque libX11.a (version 32 bits) dans elements livres ${archives_dir}/UTILITAIRES"
            echo "   ==> positionner libX11.a dans elements livres, ${archives_dir}/UTILITAIRES/libX11.a"
            echo "flag_lib_32=1" >> "${fichier_log}"
            exit 1
        fi
    else
       ln -s /usr/X11R6/lib/libX11.a "${rep_inst_elec}"/misc/lib/libX11.a
    fi

    #***********
    # LIBX11.so
    #***********
    if [ ! -e /usr/X11R6/lib/libX11.so.6.2 ]
    then
        if [ -e "${rpmdir}"/usr/X11R6/lib/libX11.so.6.2 ]
        then
            cp "${rpmdir}"/usr/X11R6/lib/libX11.so.6.2  "${rep_inst_elec}"/misc/lib/ >> /tmp/libs_$$.log 2>&1
	else
            echo "!! ERREUR : manque element systeme /usr/X11R6/lib/libX11.so.6.2"
            echo "   Probleme lors du deploiement automatique du package xorg-x11-libs-32bit-6.9.0-50.58 dans RPM complement"
            echo "Liste packages x11 deployes:"
            rpm -qa | grep x11
            echo "flag_lib_32=2" >> "${fichier_log}"
            exit 1
        fi 
    else
        ln -s /usr/X11R6/lib/libX11.so.6.2 "${rep_inst_elec}"/misc/lib/libX11.so.6.2
    fi
    ln -s "${rep_inst_elec}"/misc/lib/libX11.so.6.2 "${rep_inst_elec}"/misc/lib/libX11.so

    #**********
    # LIBXML2.so
    #**********
    if [ ! -e /usr/lib/libxml2.so.2.6.23 ]
    then
        if [ -e "${rpmdir}"/usr/lib/libxml2.so.2.6.23 ]
        then
            cp "${rpmdir}"/usr/lib/libxml2.so.2.6.23 "${rep_inst_elec}"/misc/lib/ >> /tmp/libs_$$.log 2>&1
	else
            echo "!! ERREUR : manque element systeme /usr/lib/libxml2.so.2.6.23"
            echo "   Probleme lors du deploiement automatique du package libxml2-32bit-2.6.23-15.8 dans RPM complement"
            echo "Liste packages xml2 deployes:"
            rpm -qa | grep xml2
            echo "flag_lib_32=3" >> "${fichier_log}"
            exit 1
        fi
    else
	ln -s /usr/lib/libxml2.so.2.6.23 "${rep_inst_elec}"/misc/lib/libxml2.so.2.6.23
    fi
    ln -s "${rep_inst_elec}"/misc/lib/libxml2.so.2.6.23 "${rep_inst_elec}"/misc/lib/libxml2.so

    # *************************************
    # Complements librairies (mode 64 bits)
    #  => specifiques configuration ASGARD/OLYMPE
    # *************************************
    cd "${rep_inst_elec}"/misc/lib64

    # On supprime les librairies existantes
    liste_fichiers=('libsqlite3.so' 'libX11.a' 'libX11.so')
    liste_fichiers+=(' libxerces-c.so' 'libxerces-depdom.so')
    liste_fichiers+=('libapr-1.so' 'libaprutil-1.so' 'libapr-1.so.0.2.2' 'libaprutil-1.so.0.2.2')
    for i in "${liste_fichiers[@]}"
    do
        if [ -e "$i" ]
        then
            rm -f "$i"
        fi
    done

    #**************
    # libsqlite3.so
    #**************
    if [ ! -e /usr/lib64/libsqlite3.so.0.8.6 ]
    then
        if [ -e "${rpmdir}"/usr/lib64/libsqlite3.so.0.8.6 ]
        then
            cp "${rpmdir}"/usr/lib64/libsqlite3.so.0.8.6  "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	else
            echo "!! ERREUR : manque element systeme usr/lib64/libsqlite3.so.0.8.6"
            echo "   Probleme lors du deploiement automatique du package sqlite-3.2.8-15.2 dans RPM complement"
            echo "Liste packages sqlite deployes:"
            rpm -qa | grep sqlite
            echo "flag_lib_64=1" >> "${fichier_log}"
            exit 1
         fi
    else
	 ln -s /usr/lib64/libsqlite3.so.0.8.6 "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6
    fi
    ln -s "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6 "${rep_inst_elec}"/misc/lib64/libsqlite3.so

    #**********
    # libx11.a
    #**********
    if [ ! -e /usr/X11R6/lib64/libX11.a ]
    then
        if [ -e "${rpmdir}"/usr/X11R6/lib64/libX11.a ]
        then
            cp "${rpmdir}"/usr/X11R6/lib64/libX11.a  "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
        else
        echo "!! ERREUR : manque element systeme /usr/X11R6/lib64/libX11.a"
            echo "   Probleme lors du deploiement automatique du package xorg-x11-devel-6.9.0-50.58 dans RPM complement"
            echo "Liste packages x11 deployes:"
            rpm -qa | grep x11
            echo "flag_lib_64=2" >> "${fichier_log}"
            exit 1
        fi
    else
        ln -s /usr/X11R6/lib64/libX11.a "${rep_inst_elec}"/misc/lib64/libX11.a
    fi

    #************
    # libX11.so
    #************
    if [ ! -e /usr/X11R6/lib64/libX11.so.6.2 ]
    then
        if [ -e "${rpmdir}"/usr/X11R6/lib64/libX11.so.6.2 ]
        then
            cp "${rpmdir}"/usr/X11R6/lib64/libX11.so.6.2 "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	else
            echo "!! ERREUR : manque element systeme /usr/X11R6/lib64/libX11.so.6.2"
            echo "   Probleme lors du deploiement automatique du package xorg-x11-libs-6.9.0-50.58 dans RPM complement"
            echo "Liste packages x11 deployes:"
            rpm -qa | grep x11
            echo "flag_lib_64=3" >> "${fichier_log}"
            exit 1
        fi
    else
	ln -s /usr/X11R6/lib64/libX11.so.6.2 "${rep_inst_elec}"/misc/lib64/libX11.so.6.2
    fi
    ln -s "${rep_inst_elec}"/misc/lib64/libX11.so.6.2 "${rep_inst_elec}"/misc/lib64/libX11.so

    #****************
    # libxerces-c.so
    #****************
    if [ ! -e /usr/lib64/libxerces-c.so.27.0 ]
    then
        if [ -e "${rpmdir}"/usr/lib64/libxerces-c.so.27.0 ]
        then
            cp "${rpmdir}"/usr/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	else
            echo " !! ERREUR : manque element systeme /usr/lib64/libxerces-c.so.27.0 dans RPM complement"
            echo "    Probleme lors du deploiement automatique du package xerces-c-2.7.0-13.4 dans RPM complement"
            echo " Liste packages xerces deployes:"
            rpm -qa | grep -i xerces
            echo "flag_lib_64=4" >> "${fichier_log}"
            exit 1
        fi
     else
	ln -s /usr/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0
     fi
     ln -s "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so
     ln -s "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27

    #********************
    # libxerces-depdom.so
    #********************
    if [ ! -e /usr/lib64/libxerces-depdom.so.27.0 ]
    then
        if [ -e "${rpmdir}"/usr/lib64/libxerces-depdom.so.27.0 ]
        then
            cp "${rpmdir}"/usr/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	else
            echo " !! ERREUR : manque element systeme /usr/lib64/libxerces-depdom.so.27.0 dans RPM complement"
            echo "    Probleme lors du deploiement automatique du package xerces-depdom.so.27.0 dans RPM complement"
            echo " Liste packages xerces deployes:"
            rpm -qa | grep -i xerces
            echo "flag_lib_64=5" >> "${fichier_log}"
            exit 1
        fi
     else
	ln -s /usr/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0
     fi
     ln -s "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so            
     ln -s "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27            

    #***********
    # libapr-1.so
    ##***********
    if [ ! -e /usr/lib64/libapr-1.so.0.2.2 ]
    then
        if [ -e "${archives_dir}"/UTILITAIRES/lib/libapr-1.so.0.2.2  ]
        then
            cp "${archives_dir}"/UTILITAIRES/lib/libapr-1.so.0.2.2  "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	    else
           echo "!! ERREUR : manque element systeme /usr/lib64/libapr-1.so.0.2.2"
           echo "   Probleme lors du deploiement automatique de libapr-1.so.0.2.2"
           echo "   Manque libapr-1.so.0.2.2 (version 64 bits) dans elements livres ${archives_dir}/UTILITAIRES/lib"
           echo "   ==> positionner libapr-1.so.0.2.2 dans elements livres, ${archives_dir}/UTILITAIRES/lib/libapr-1.so.0.2.2"
           echo "flag_lib_64=6" >> "${fichier_log}"
           exit 1
        fi
     else
	     ln -s /usr/lib64/libapr-1.so.0.2.2 "${rep_inst_elec}"/misc/lib64/libapr-1.so.0.2.2
     fi
     ln -s "${rep_inst_elec}"/misc/lib64/libapr-1.so.0.2.2 "${rep_inst_elec}"/misc/lib64/libapr-1.so

    #****************
    # libaprutil-1.so
    #****************
    if [ ! -e /usr/lib64/libaprutil-1.so.0.2.2 ]
    then
        if [ -e "${archives_dir}"/UTILITAIRES/lib/libaprutil-1.so.0.2.2 ]
        then
            cp "${archives_dir}"/UTILITAIRES/lib/libaprutil-1.so.0.2.2 "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
	else
           echo "!! ERREUR : manque element systeme /usr/lib64/libaprutil-1.so.0.2.2"
           echo "   Probleme lors du deploiement automatique de libaprutil-1.so.0.2.2"
           echo "   Manque libaprutil-1.so.0.2.2 (version 64 bits) dans elements livres ${archives_dir}/UTILITAIRES/lib"
           echo "   ==> positionner libaprutil-1.so.0.2.2 dans elements livres, ${archives_dir}/UTILITAIRES/lib/libaprutil-1.so.0.2.2"
           echo "flag_lib_64=7" >> "${fichier_log}"
           exit 1
        fi
     else
	ln -s /usr/lib64/libaprutil-1.so.0.2.2 "${rep_inst_elec}"/misc/lib64/libaprutil-1.so.0.2.2
     fi
     ln -s "${rep_inst_elec}"/misc/lib64/libaprutil-1.so.0.2.2 "${rep_inst_elec}"/misc/lib64/libaprutil-1.so

else

   ## L'utilisateur ne veut pas installer les lib systèmes #########
    cd "${rep_inst_elec}"/misc/lib
    lier_librairie /usr/X11R6/lib/libX11.a libX11.a
    lier_librairie /usr/X11R6/lib/libX11.so.6.2 libX11.so.6.2
    lier_librairie /usr/X11R6/lib/libX11.so.6.2 libX11.so
    lier_librairie /usr/lib/libxml2.so.2.6.23 libxml2.so.2.6.23
    lier_librairie /usr/lib/libxml2.so.2.6.23 libxml2.so
    echo "flag_lib_32=0" >> "${fichier_log}"

    cd "${rep_inst_elec}"/misc/lib64
    lier_librairie /usr/lib64/libsqlite3.so.0.8.6 libsqlite3.so.0.8.6 
    lier_librairie /usr/lib64/libsqlite3.so.0.8.6 libsqlite3.so
    lier_librairie /usr/X11R6/lib64/libX11.a libX11.a
    lier_librairie /usr/X11R6/lib64/libX11.so.6.2 libX11.so.6.2
    lier_librairie /usr/X11R6/lib64/libX11.so.6.2 libX11.so
    lier_librairie /usr/lib64/libxerces-c.so.27.0 libxerces-c.so.27.0
    lier_librairie /usr/lib64/libxerces-c.so.27.0 libxerces-c.so
    lier_librairie /usr/lib64/libxerces-c.so.27.0 libxerces-c.so.27
    lier_librairie /usr/lib64/libxerces-depdom.so.27.0 libxerces-depdom.so.27.0
    lier_librairie /usr/lib64/libxerces-depdom.so.27.0 libxerces-depdom.so
    lier_librairie /usr/lib64/libxerces-depdom.so.27.0 libxerces-depdom.so.27
    lier_librairie /usr/lib64/libapr-1.so.0.2.2 libapr-1.so.2.2
    lier_librairie /usr/lib64/libapr-1.so.0.2.2 libapr-1.so
    lier_librairie /usr/lib64/libaprutil-1.so.0.2.2 libaprutil-1.so.2.2
    lier_librairie /usr/lib64/libaprutil-1.so.0.2.2 libaprutil-1.so
    echo "flag_lib_64=0" >> "${fichier_log}"
fi

echo "** Deploiment des librairies systemes : OK "

# Mise a jour du LD_LIBRARY_PATH
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib:${rep_inst_elec}/misc/lib:${rep_inst_elec}/misc/lib64"

#######################################################################################################
#
#
#                                         GENERATION ELECTRA
#
#
#######################################################################################################

# **********************
# 1-Installation de GFortran
# **********************
echo "** Installation GFortran ${v_gcc} ..."

if [ "${flag_g95}" = 1 ]
then
    if [ -e "${archives_dir}"/UTILITAIRES/"${arch_g95}" ]
    then
        cd ${rep_inst_elec}/misc
        tar -xzf "${archives_dir}"/UTILITAIRES/"${arch_g95}"
        if [ ! -e gcc-${v_gcc}/bin ] || [ ! -e  gcc-${v_gcc}/lib ]
        then
            echo "!! ERREUR sur archive GFortran : ${arch_g95}"
            echo "    ==> nom repertoire decompresse attendu est gcc-${v_gcc}"
            echo "        revoir contenu archive gcc-${v_gcc}"
            exit 1
        fi
    else
        echo "Fichier archive pour compilateur GFortran absent : ${archives_dir}/UTILITAIRES/${arch_g95}"
        exit 1
    fi
    echo "   GFortran situe sous " ${rep_inst_elec}/misc
    echo "** Installation GFortran ${v_gcc} OK"
else
    echo "   GFortran situe sous " ${rep_g95}
    echo "** Installation de GFortran non demandee"
    cd "${rep_inst_elec}"/misc
    ln -s "${rep_g95}" gcc-${v_gcc}
    ln -s "$(readlink -f "$(dirname "${rep_g95}")")"/gmp gmp
    ln -s "$(readlink -f "$(dirname "${rep_g95}")")"/mpc mpc
    ln -s "$(readlink -f "$(dirname "${rep_g95}")")"/mpfr mpfr
fi


alias g95="gfortran -B ${rpmdir}/usr/lib -Wa,-32 -m32 -fno-second-underscore"
export PATH="${rep_inst_elec}/misc/gcc-${v_gcc}/bin:${PATH}"
export LD_LIBRARY_PATH="${rep_inst_elec}/misc/gmp/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="${rep_inst_elec}/misc/mpfr/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="${rep_inst_elec}/misc/mpc/lib:${LD_LIBRARY_PATH}"
export LD_LIBRARY_PATH="${rep_inst_elec}/misc/gcc-${v_gcc}/lib:${LD_LIBRARY_PATH}"

# *************************
# 2-Installation des BIBMS : 2eme etape : a remplacer par la nouvelle procedure d'installation de BIBMS
# *************************
echo "** Installation de BIBMS..."

if [ "${flag_PSIMU}" = 1 ]
then
    cd "${rep_inst_elec}"/misc
    if [ ! -e "${archives_dir}"/PSIMU/"${arch_psimu}" ]
    then
        echo "=> ERREUR archive PSIMU (PSIMU/${arch_psimu}) non presente dans elements de livraison"
        exit 1
    else
        tar -xf "${archives_dir}"/PSIMU/"${arch_psimu}"
        if [ -e ./installation_PSIMU.sh ]
        then
            chmod a+x installation_PSIMU.sh
            ./installation_PSIMU.sh
        else
            echo "!! ERREUR, archive PSIMU : le script d'installation installation_PSIMU.sh n'est pas present"
            exit 1
        fi
        # Maj du compas.rc
        sed -i -e "/DB_COMPAS_LOCAL/ s@.*@DB_COMPAS_LOCAL = \"${rep_inst_elec}/misc/share/data/BD_COMPAS_locale\"@g" \
               -e "/DB_COMPAS_REF/   s@.*@DB_COMPAS_REF = \"${rep_inst_elec}/misc/share/data/db_ref\"@g"             \
                  "${rep_inst_elec}"/misc/share/rc/compas.rc
        chmod -R +rx "${rep_inst_elec}"/misc/share/data
        nettoie_tmpdir
        export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib:${rep_inst_elec}/misc/lib:${rep_inst_elec}/misc/lib64"
    fi
    echo "   BIBMS situee sous " ${rep_inst_elec}/misc


# Modifications apres installation BIBMS
#  Objectifs : manque de la librairie X11 mode 32 bits au niveau systeme
#                => specifique configuration ASGARD/OLYMPE
#
    FICHIER_MODIFIE="$rep_inst_elec/misc/GENESIS/V${v_genesis}/xpl/GenesisInitVarEnv.sh"
    if [ -e $FICHIER_MODIFIE ]
    then
      sed -i 's|/usr/X11R6|'$rep_inst_elec'/misc|g' $FICHIER_MODIFIE
    else
      echo "!! ERREUR, souci presence $FICHIER_MODIFIE "
      echo "   ==> modification non faite sur localisation librairie X11 en mode 32 bits"
      exit -1
    fi
    FICHIER_MODIFIE="$rep_inst_elec/misc/GENESIS/V${v_genesis}/fcf/genesis.rc"
    if [ -e $FICHIER_MODIFIE ]
    then
      sed -i 's|GENESIS_LINK\=\"gfortran|GENESIS_LINK\=\"gfortran\ \-B\ '$rpmdir'/usr/lib\ |g' $FICHIER_MODIFIE
    else
      echo "!! ERREUR, souci presence $FICHIER_MODIFIE "
      echo "   ==> modification non faite sur localisation crtl.o et crti.o en mode 32 bits"
      exit -1
    fi

#   Installation terminee
    echo "** Installation de BIBMS : OK"

else
    echo "   BIBMS situee sous " ${rep_BIBMS}
    echo "** Installation de BIBMS non demandee"
    cd "${rep_inst_elec}"/misc
    rep_BIBMS="$(dirname "${rep_PSIMU}")"
    liste_rep=('admin' 'ARCH' 'bin' 'COMPAS' 'GENESIS' 'GSLIB')
    liste_rep+=('MADONA' 'MAGE' 'MECASPA' 'mod' 'MSLIB90')
    liste_rep+=('MSPRO' 'NAIF' 'NETCDF' 'noarch' 'PSIMU' 'share' 'TCL_TK' 'TESTS_PSIMU')

    for i in "${liste_rep[@]}"
    do
        ln -s "${rep_BIBMS}"/"$i" "$i"
    done
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib:${rep_inst_elec}/misc/lib:${rep_inst_elec}/misc/lib64"
fi
# Mise a jour variables environnements
. "${rep_inst_elec}"/misc/bin/env_psimu.sh  >> /tmp/bibms_$$.log 2>&1

# ***************************
# 3-Installation de Openmpi 
# ***************************

echo "** Installation de OpenMpi ${v_openmpi}..."

if [ "${flag_openmpi}" = 1 ]
then
    cd "${tmpdir}"
    if [ ! -e "${archives_dir}"/pre_ELECTRA/"${arch_openmpi}" ]
    then
        echo "!! ERREUR, archive ${arch_openmpi} non trouvee dans ${archives_dir}/pre_ELECTRA"
        echo "   ==> verifier presence archive ${arch_openmpi} dans ${archives_dir}/pre_ELECTRA"
        exit 1
    else
        tar -xzvf "${archives_dir}/pre_ELECTRA/${arch_openmpi}" >> /tmp/prerequis_electra_$$.log 2>&1
        mkdir -p "${rep_inst_elec}"/misc/openmpi
        if [ ! -d "${mpi_rep}" ]
        then
            echo "!! ERREUR, souci decompression archive ${arch_openmpi} de ${archives_dir}/pre_ELECTRA"
            echo "   ==> verifier contenu archive ${arch_openmpi} dans ${archives_dir}/pre_ELECTRA"
            exit 1
        fi
        cd "${mpi_rep}"

        export LDFLAGS="-L${tmpdir}/usr/lib -m32 ${TEMP_LDFLAGS}"
        export CPPFLAGS="-I${tmpdir}/usr/include -I${rep_inst_elec}/misc/gcc-${v_gcc}/include/c++/${v_gcc}/"
        export CXXFLAGS="-I${tmpdir}/usr/include -I${rep_inst_elec}/misc/gcc-${v_gcc}/include/c++/${v_gcc}/"
        ./configure --prefix="${rep_inst_elec}"/misc/openmpi CC=gcc CXX=g++ FC=gfortran F77=gfortran \
                    CFLAGS="-m32 -march=i586 ${TEMP_FLAGS} ${TEMP_LDFLAGS}" \
                    CXXFLAGS="-m32 -march=i586 ${TEMP_FLAGS} ${TEMP_LDFLAGS}" \
                    FFLAGS="-O2 -B ${rep_inst_elec}/misc/gcc-${v_gcc}/lib -Wa,-32 -m32 -fno-second-underscore" \
                    FCFLAGS="-O2 -B $rep_inst_elec}/misc/gcc-${v_gcc}/lib -Wa,-32 -m32 -fno-second-underscore" \
                    --with-wrapper-cflags="-m32 ${TEMP_FLAGS} ${TEMP_LDFLAGS}" \
                    --with-wrapper-cxxflags="-m32 ${TEMP_FLAGS} ${TEMP_LDFLAGS}" \
                    --enable-mpi-threads --enable-mpirun-prefix-by-default --disable-shared --enable-static \
                     > /tmp/prerequis_electra_$$.log 2>&1

        unset LDFLAGS
        unset CPPFLAGS
        unset CXXFLAGS
        make         >> /tmp/prerequis_electra_$$.log 2>&1
        make install >> /tmp/prerequis_electra_$$.log 2>&1

        if [ ! -f "${rep_inst_elec}"/misc/openmpi/bin/ompi_info ]
        then
            echo "!! ERREUR PROBLEME DANS LA COMPILATION DE OPENMPI"
            echo "==> voir /tmp/prerequis_electra_$$.log"
            exit 1
        fi
        nettoie_tmpdir
        cd "${tmpdir}"
        echo "** Installation de OpenMpi ${v_openmpi} OK" >> /tmp/prerequis_electra_$$.log
    fi
    export PATH="${rep_inst_elec}/misc/openmpi/bin:${PATH}"
    export PATH="${rep_inst_elec}/misc/openmpi/sbin:${PATH}"
    export LD_LIBRARY_PATH="${rep_inst_elec}/misc/openmpi/lib:${LD_LIBRARY_PATH}"
    # Souci sur prise en compte localisation crt1.o crtn.o
    echo "** Integration editions des liens pour localisation crt1.o et crtn.o" >> /tmp/electra_$$.log
    if [ -e "${rep_inst_elec}"/misc/openmpi/share/openmpi/mpif90-wrapper-data.txt ]
    then
        sed -i s"|linker_flags=|linker_flags=-B ${rpmdir}/usr/lib|"g "${rep_inst_elec}"/misc/openmpi/share/openmpi/mpif90-wrapper-data.txt
    else
        echo "!!! WARNING, modification fichier OpenMPI mpif90-wrapper-data.txt pour prise en compte localisation crt1.o et crtn.o impossible" >> /tmp/electra_$$.log
        echo "  --> fichier ${rep_inst_elec}/misc/openmpi/share/openmpi/mpif90-wrapper-data.txt non existant" >> /tmp/electra_$$.log
    fi
    echo "   OpenMpi situee sous " ${rep_inst_elec}/misc/openmpi
    echo "** Installation de OpenMpi ${v_openmpi} OK"

else
    echo "   OpenMpi situee sous " ${rep_openmpi}
    echo "** Installation de Openmpi non demandee"
    cd "${rep_inst_elec}"/misc
    ln -s "${rep_openmpi}" openmpi
    export PATH="${rep_inst_elec}/misc/openmpi/bin:${PATH}"
    export PATH="${rep_inst_elec}/misc/openmpi/sbin:${PATH}"
    export LD_LIBRARY_PATH="${rep_inst_elec}/misc/openmpi/lib:${LD_LIBRARY_PATH}"
fi

# Pour eviter d'avoir des warning de compilation lors du lancement de generation_ELECTRA
sed -i 's@character\*32@character(32)@g' "${rep_inst_elec}"/misc/openmpi/include/mpif-config.h


#Mise a jour fiches compilateur gcc en 32 et 64 bits
if [ -e "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc.conf ]
then
   sed -i s"|LD_FLAGS_SUPP=|LD_FLAGS_SUPP=-B $rpmdir/usr/lib|"g "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc.conf
   echo " Modification fichier Makemake compilateur_gcc.conf pour prise en compte localisation crtl.o et crtn.o" >> /tmp/electra_$$.log
else
   echo "!! WARNING, modification fichier Makemake compilateur_gcc.conf pour prise en compte localisation crtl.o et crtn.o impossible" >> /tmp/electra_$$.log
   echo " --> fichier "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc.conf non existant" >> /tmp/electra_$$.log
fi
if [ -e "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc-64.conf ]
then
   sed -i s"|LD_FLAGS_SUPP=|LD_FLAGS_SUPP=-B $rpmdir/usr/lib|"g "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc-64.conf
   echo " Modification fichier Makemake compilateur_gcc-64.conf pour prise en compte localisation crtl.o et crtn.o" >> /tmp/electra_$$.log
else
   echo "!! WARNING, modification fichier Makemake compilateur_gcc-64.conf pour prise en compte localisation crtl.o et crtn.o impossible" >> /tmp/electra_$$.log
   echo " --> fichier "${rep_inst_elec}"misc/noarch/MAKEMAKE/V"${v_makemake}"/fcf/compilateur_gcc-64.conf non existant" >> /tmp/electra_$$.log
fi


# **************************
# 4-Installation de ELECTRA
# **************************

echo "** Installation ELECTRA ${version_electra}..."
if [ "${gen_electra}" = 1 ]
then
    if [ ! -e "${archives_dir}"/ELECTRA/"${arch_electra_gz}" ]
    then
        echo "!! ERREUR, archive ${electra_arch_gz} non trouve dans ${archives_dir}/ELECTRA/"
        echo "   ==> revoir contenu de ${archives_dir}/ELECTRA/ et nom archive ELECTRA"
        exit 1
    else
        cd "${tmpdir}"
        gunzip -c "${archives_dir}"/ELECTRA/"${arch_electra_gz}" > "${arch_electra}"
        tar -xf "${arch_electra}" ./gen/configure ./gen/generation_ELECTRA

        # Modification sur les versions (A corriger pour la prochaine monte de niveau)
        if [ -e ./gen/configure ]
        then
            sed -i 's/V_MADONA=V4\.6/V_MADONA=V'${v_madona//./\\.}'/'     ./gen/configure
            sed -i 's/V_MAGE=V1\.12X2/V_MAGE=V'${v_mage//./\\.}'/'        ./gen/configure
            sed -i 's|/usr/X11R6|'"${rep_inst_elec}"/'misc|' ./gen/configure
        else
            echo "!! ERREUR, archive ${electra_arch_gz} ne contient par ./gen/configure"
            exit 1
        fi
        # Modification sur le chemin de la librairie X11 mode 32 bits
        if [ -e  ./gen/generation_ELECTRA ]
        then
            sed -i 's|/usr/X11R6|'"${rep_inst_elec}"/'misc|' ./gen/generation_ELECTRA
        else
            echo "!! ERREUR, archive ${arch_electra_gz} ne contient par ./gen/generation_ELECTRA"
            exit 1
        fi
        tar -rf "${arch_electra}" ./gen/configure ./gen/generation_ELECTRA
        rm -rf ./gen

        # Fin de modification des versions

        tar -xf "${arch_electra}" ./gen/generation_ELECTRA.sh
        if [ ! -e ./gen/generation_ELECTRA.sh ]
        then
            echo "!! ERREUR, absence de ./gen/generation_ELECTRA.sh dans archive ${electra_arch}"
            echo "   ==> verifier presence ./gen/generation_ELECTRA.sh dans archive ${archives_dir}/ELECTRA/${electra_arch}"
            exit 1
        else
            # Installation de variables genesis
            # Sinon il a du mal a les retrouver.
            export GENESIS_DIR="${rep_inst_elec}/misc/GENESIS/V${v_genesis}"
            export MADONA_DIR="${rep_inst_elec}/misc/MADONA/V${v_madona}"
            export TCL_DIR="${rep_inst_elec}/misc/TCL_TK/V${v_tcl_tk}"
            export TK_DIR="${TCL_DIR}"
            . "${GENESIS_DIR}"/xpl/GenesisInitVarEnv.sh >/dev/null

            echo "debut generation"
            ./gen/generation_ELECTRA.sh << EOF >> /tmp/electra_$$.log 2>&1
${arch_electra}

N
N
EOF
        fi
        echo "fin generation"
        # Copie de l'installation du repertoire temporaire vers destination
        export elec_export="${rep_inst_elec}/ELECTRA/${version_electra}"

        if [ -e "${tmpdir}"/"${nom_export}" ]
        then
            mkdir -p "${rep_inst_elec}"/ELECTRA
            mv "${tmpdir}"/"${nom_export}"/ "${elec_export}"
        else
            echo "!! ERREUR, absence de ${tmpdir}/${nom_export}/"
            echo "   ==> verifier le log de generation d'ELECTRA /tmp/electra_$$.log "
            exit 1
        fi
        export PATH="${elec_export}/xpl:${PATH}"
        export MADONA_RC_PATH="${elec_export}/fcf:${MADONA_RC_PATH}"
        if [ -e "${elec_export}"/fcf/electra_"${version_electra}".rc ]
        then
            sed -i 's|'"${tmpdir}"'|'"${rep_inst_elec}"'|g' "${elec_export}"/fcf/electra_"${version_electra}".rc
            sed -i 's/'"${nom_export}"'/${version_electra}/g' "${elec_export}"/fcf/electra_"${version_electra}".rc
            sed -i 's|dir_electra=.*|dir_electra=\"'"${elec_export}"'\"|g' "${elec_export}"/fcf/electra_"${version_electra}".rc

            # Il semble que les scripts de generation d'electra ne mettent pas a jour le chemin vers GENESIS
            sed -i 's|GENESIS=.*|GENESIS=\"'"${rep_inst_elec}"'/misc/GENESIS/V'"${v_genesis}"'\"|g' "${elec_export}"/fcf/electra_"${version_electra}".rc
        else
            echo "!! ERREUR, non existence du fichier de configuration ${elec_export}/fcf/electra_${version_electra}.rc"
            echo "   ==> probable souci dans compilation/installation electra"
            exit 1
        fi
       if [ ! -e "${elec_export}"/bin/ELECTRA.ihm ]
       then
           echo "!! ERREUR, Mauvaise installation ELECTRA : executable IHM non defini ({$elec_export}/bin/ELECTRA.IHM)"
       fi
       if [ ! -e "${elec_export}"/bin/ELECTRA.exe ]
       then
           echo "!! ERREUR, Mauvaise installation ELECTRA : executable non defini (${elec_export}/bin/ELECTRA.exe)"
       fi
       # Recuperation du fichier de log electra
       cp "${tmpdir}"/electra_generation.log   /tmp/electra_generation_$$.log

       # Creation script pour raccourci bureau
       script_raccourci="${elec_export}"/xpl/start_electra
       cat > "${script_raccourci}" <<EOF
#!/bin/ksh
. ${rep_inst_elec}/env_electra
cd \$HOME/electra
electra
EOF

        # Creation script pour creation Icone bureau
        script_bureau="${elec_export}"/xpl/ELECTRA.Desktop
        cat > "${script_bureau}" <<EOF
[Desktop Entry]
Comment=Soumission Trajectoire
Comment[fr]=Soumission Trajectoire
Encoding=UTF-8
Exec=${script_raccourci}
GenericName=Application ELECTRA
GenericName[fr]=Application ELECTRA
Icon=${rep_inst_elec}/misc/Electra.png
MimeType=
Name=ELECTRA
Name[fr]=ELECTRA
Path=\$\$HOME
StartupNotify=false
Terminal=true
TerminalOptions=
Type=Application
X-DCOP-ServiceType=
X-KDE-SubstituteUID=false
X-KDE-UserName=
EOF

        # Creation fichier environnement
        fichier_env="${rep_inst_elec}"/env_electra
        cat > "${fichier_env}" <<EOF
#!/bin/ksh
export rep_electra=${rep_inst_elec}
export rep_oreste=${rep_inst_ores}
export rep_oreste_httpd=${rep_inst_ores_httpd}
export dep_database=\$rep_electra/noarch/version
export LD_LIBRARY_PATH=\$rep_electra/misc/lib:\$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=\$rep_electra/misc/gcc-${v_gcc}/lib:\$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=\$rep_electra/misc/TCL_TK/V${v_tcl_tk}/lib:\$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=\$rep_electra/misc/GENESIS/V${v_genesis}/lib:\$LD_LIBRARY_PATH
export GSLIB_DIR=\$rep_electra/misc/GSLIB_DIR/V${v_gslib}
export MADONA_DIR=\$rep_electra/misc/MADONA/V${v_madona}
export MECASPA_DIR=\$rep_electra/misc/MECASPA/V${v_mecaspa}
export GENESIS_DIR=\$rep_electra/misc/GENESIS/V${v_genesis}
export PSIMU_DIR=\$rep_electra/misc/PSIMU/V${v_psimu}
export TCL_DIR=\$rep_electra/misc/TCL_TK/V${v_tcl_tk}
export TK_DIR=\$TCL_DIR
. \${GENESIS_DIR}/xpl/GenesisInitVarEnv.sh>/dev/null
. \${rep_electra}/misc/bin/env_psimu.sh>/dev/null
export REGISTER_PATH=\$rep_electra/admin/registre
export PATH=\$rep_electra/misc/bin:\$PATH
export PATH=\$rep_electra/misc/noarch/MAKEMAKE/V${v_makemake}/bin:\$PATH
export PATH=\$rep_electra/misc/noarch/REGISTER/V${v_register}/bin:\$PATH
export PATH=\$rep_electra/misc/gcc-${v_gcc}/bin:\$PATH
export PATH=\$rep_electra/misc/openmpi/bin:\$PATH
export PATH=\$rep_electra/misc/openmpi/sbin:\$PATH
export PATH=\$rep_electra/misc/TCL_TK/V${v_tcl_tk}/bin:\$PATH
export PATH=\$rep_oreste_httpd/${apache_rep}/bin:\$PATH
export PATH=\$rep_electra/ELECTRA/${version_electra}/xpl:\$PATH
export PATH=\$rep_oreste/ORESTE/${version_oreste}/xpl:\$PATH
export MADONA_RC_PATH=\$MADONA_RC_PATH:\$rep_electra/misc/PSIMU/${v_psimu}/fcf:\$rep_electra/misc/share/rc:\$rep_electra/misc/GENESIS/V${v_genesis}/fcf:\$rep_electra/misc/COMPAS/V${v_compas}/fcf:\$rep_electra/ELECTRA/${version_electra}/fcf
export LD_LIBRARY_PATH=\$rep_oreste/outils/firefox-3/prerequis/lib:\$LD_LIBRARY_PATH
export PATH=\$rep_oreste/outils/firefox-3:\$rep_oreste/outils/firefox-3/prerequis/bin:\$PATH
export PKG_CONFIG_PATH=\$rep_oreste/outils/firefox-3/prerequis/lib/pkg_config:\$PKG_CONFIG_PATH
EOF

        # Integration de la mise a jour de l'environnement dans le script de lancement
        sed -i "s,liste_versions=,. ${fichier_env} \\nliste_versions=,1" ${elec_export}/xpl/electra

        # Installation de l'icone d'electra
        if [ -e "${archives_dir}"/UTILITAIRES/Electra.png ]
        then
            cp "${archives_dir}"/UTILITAIRES/Electra.png "${rep_inst_elec}"/misc/
        else
            echo "!! ERREUR, archive Icon electra.png non trouvee dans ${archives_dir}/UTILITAIRES"
            echo "   ==> revoir contenu de ${archives_dir}/UTILITAIRES"
            exit 1
        fi
        # Installation du MU
        if [ -e "${archives_dir}"/DOCUMENTATION/"${electra_mu}" ]
        then
            cp -f "${archives_dir}"/DOCUMENTATION/"${electra_mu}" "${elec_export}"/fcf/MU.pdf
        else
            echo "!! WARNING, archive ${electra_mu} non trouvee dans ${archives_dir}/DOCUMENTATION/"
            echo "   ==> revoir contenu de ${archives_dir}/DOCUMENTATION"
        fi

        if [ -e "${archives_dir}"/UTILITAIRES/shells/installation_user.ksh ]
        then
            mkdir "${rep_inst_elec}"/shells
            cp "${archives_dir}"/UTILITAIRES/shells/installation_user.ksh "${rep_inst_elec}"/shells/installation_user.ksh
            cp "${chemin}"/properties.sh "${rep_inst_elec}"/shells/properties.sh
        else
            echo "!! ERREUR, archive installation_user.ksh non trouvee dans ${archives_dir}/UTILITAIRES"
            echo "   ==> revoir contenu de ${archives_dir}/UTILITAIRES"
            exit 1
        fi

        # Mise a jour du lecteur pdf (chemin en absolu)
        if [ -e "${fic_pdf}" ]
        then
            sed -i 's,lec_pdf=.*, lec_pdf="'"${fic_pdf}"'",g' "${rep_inst_elec}"/ELECTRA/"${version_electra}"/fcf/electra_"${version_electra}".rc
        else
            echo "!! ERREUR : ${fic_pdf} n'existe pas"
            echo "flag_fic_pdf=0" >> "${fichier_log}"
            exit 1
        fi

        # Mise a jour de editeur de texte (chemin en absolu)
        if [ -e "${fic_txt}" ]
        then
            sed -i 's,EDIT_TXT=.*, EDIT_TXT="'"${fic_txt}"'",g' "${rep_inst_elec}"/misc/GENESIS/V"${v_genesis}"/fcf/genesis.rc
        else
            echo "!! ERREUR : ${fic_txt} n'existe pas"
            echo "flag_fic_txt=0" >> "${fichier_log}"
            exit 1
        fi

        # machines.txt
        nb_proc="$(grep processor /proc/cpuinfo | wc -l)"
        nom_machine="$(uname -n)"
        echo "${nom_machine} ${nb_proc}" > "${elec_export}"/fcf/machines.txt

        # Preparation du test d'installation
        cp "${archives_dir}"/ELECTRA/"${arch_electra_gz}" "${tmpdir}"
        cd "${tmpdir}"
        tar -xvf "${arch_electra}" ./test/* >> /tmp/electra_$$.log 2>&1
        cd test/validation

        # Cree la liste des fichiers a inclure dans le tar puis creee le tar

        # Recupere la liste des fichiers de donnees necessaires
        liste_data="$(./scripts/passage_test_ref.sh -f)"

        for i in ${liste_data[*]}
        do
            liste="${liste} data/$i"
        done

        # Recupere la liste des fichiers de resultats de reference necessaires
        liste_conf="$(./scripts/passage_test_ref.sh -l)"
        for i in ${liste_conf[*]}
        do
            liste_conf_bis="${liste_conf_bis} ${i/ELC_/}"
        done

        for i in ${liste_conf_bis[*]}
        do
            liste="${liste} $(ls results_ref/*$i*)"
        done

        # Ajoute le fichier de performance a la liste des fichiers
        liste="${liste} results_ref/perfo_bonfct.txt"

        # Ajoute le script a la liste des fichiers
        liste="${liste} scripts/passage_test_ref.sh"

        tar -czhvf "${elec_export}"/test_numeriques.tar.gz ${liste} >> /tmp/electra_$$.log 2>&1

        # Modification des fichiers d'exemples fournis par installation_users.ksh (electra -u)
        if [ -e "${archives_dir}"/UTILITAIRES/exemples.tar.gz ]
        then
            cp "${archives_dir}"/UTILITAIRES/exemples.tar.gz "${elec_export}"/fcf
            rm -rf "${elec_export}"/fcf/exemples
            cd "${elec_export}"/fcf
            tar -xvf "${elec_export}"/fcf/exemples.tar.gz >> /tmp/electra_$$.log 2>&1
        else
            echo "!! ERREUR, archive exemples.tar.gz non trouvee dans ${archives_dir}/UTILITAIRES"
            echo "   ==> revoir contenu de ${archives_dir}/UTILITAIRES"
            exit 1
        fi

        # Restriction de droits dans repertoire installation
        chown -R electra:"${user_group}" "${rep_inst_elec}"
        chmod -R 750 "${rep_inst_elec}"
        nettoie_tmpdir
    fi

    # Supression des repertoires temporaires
    if [ -d "${rpmdir}" ] && [ "${rpmdir}" != "/" ]
    then
        rm -rf "${rpmdir}"
    else
        echo "!! ERREUR, souci de definition de repertoire temporaire pour nettoyage : ${rpmdir}"
        exit 1
    fi
    if [ -d "${tmpdir}" ] && [ "${tmpdir}" != "/" ]
    then
        rm -rf "${tmpdir}"
    else
        echo "!! ERREUR, souci de definition de repertoire temporaire pour nettoyage : ${tmpdir}"
        exit 1
    fi
    echo "** Installation ELECTRA sous " ${rep_inst_elec}
    echo "** Installation ELECTRA ${version_electra} OK"
    echo "** Installation ELECTRA, date fin installation : $(date)"

else
    echo "** Installation ELECTRA ${version_electra} non demandee"
fi

