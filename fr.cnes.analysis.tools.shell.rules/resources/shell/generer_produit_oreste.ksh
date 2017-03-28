#!/bin/ksh +f
#*****************************************************************
#$<AM-V2.0>
#
#$Type
#	PROG
#
#$Nom
#	generer_produit_oreste.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#
#$Resume
#	"Generation d'ORESTE"
#
#$Auteur
#   CAPGEMINI
#
#$Description
# Ce script a pour but de deployer ORESTE sous environnement ASGARD du CSG
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
#	$Id: generer_produit_oreste.ksh 2566 2014-08-06 13:50:42Z mbeuvelo $
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
# Pour lancer le shell sans passer par l'installeur, mettre le flag a oui, sinon laisser a yes
reponse_keytool=oui

### FA 707: spécificité Asgard/Olympe ########################
#pour ne pas être limité sur la taille des fichiers notamment Oreste.tar et GlobalInsight.tar
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


# ***************************************************************************
# Specification des chemins installation/creation/positionnement des archives
# ***************************************************************************
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# fichier de log recupere par l'IHM en sortie du script
export fichier_log="${chemin}"/generer_produit_oreste.log

#Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm -f "${fichier_log}"

# ***************************
# Creation de l'arborescence
# ***************************
liste_repertoires=("${rpmdir}" "${tmpdir}")

# L'utilisateur souhaite installer oreste
if [ "${gen_oreste}" -eq 1 ]
then
    liste_repertoires+=("${rep_inst_ores}")
fi

# Apache est present et l'utilisateur souhaite utiliser l'existant
if [ "${flag_httpd}" -eq 0 ]
then
    # Description des variables rep_inst_ores_httpd et apache_rep
    # Exemple dans le cas ou Apache installe sous : /usr/local
    # => rep_inst_ores_httpd=/usr/local
    # => apache_bin=/usr/local/http-2.2/bin/httpd
    # => apache_rep=http-2.2
    # => si plusieurs serveurs apache presents sous apache_rep, on  utilise le 1er de la liste 
    apache_bin="$(find "${rep_inst_ores_httpd}" -type f -name httpd | head -n 1)"
    apache_rep="$(basename "$(readlink -f "$(dirname "${apache_bin}")"/..)")"
# L'utilisateur souhaite installer un Apache
else
    liste_repertoires+=("${rep_inst_ores_httpd}")
fi

for i in "${liste_repertoires[@]}"
do
    if [ -d "$i" ]
    then
        echo " WARNING, repertoire $i existant: nettoyage active"
        rm -rf "$i"
    fi
    mkdir -p "$i"
done


# ********************************************************
# Deploiement RPM en complement dans repertoire de travail
#  => specifiques configuration ASGARD
# ********************************************************

echo "** Deploiment des rpm complementaires temporaires..."
cd "${rpmdir}"
liste_rpm="$(ls "${archives_dir}"/UTILITAIRES/RPM_complements/*.rpm)"
for i in ${liste_rpm[*]}
do
    if [ -e "$i" ]
    then
        rpm2cpio "$i" | cpio -di  > /tmp/rpm2cpio_$$.log  2>&1
    fi
done
# Effacer fichiers configuration "XXX.la" car ils font reference a racine systeme "/"
find "${rpmdir}" -name '*.la' | xargs rm
# Modification script ld GNU pour chemin libc_nonshared.a et libpthread_nonshared.a
sed -i 's|usr/lib/libc_nonshared.a|'"${rpmdir}"'/usr/lib/libc_nonshared.a|g'             "${rpmdir}"/usr/lib/libc.so
sed -i 's|usr/lib/libpthread_nonshared.a|'"${rpmdir}"'/usr/lib/libpthread_nonshared.a|g' "${rpmdir}"/usr/lib/libpthread.so

# Lien vers Java
export JAVA_HOME="${JAVA_HOME:-/usr/java/jdk1.5.0_22}"
export PATH="${JAVA_HOME}"/jre/bin:"${JAVA_HOME}"/bin:"${PATH}"

# Mise a jour de l'environnement
export PATH="${rpmdir}/bin:${rpmdir}/usr/bin:${PATH}"
export LD_LIBRARY_PATH="${rpmdir}/usr/lib:${rpmdir}/usr/lib64"
export TEMP_FLAGS="-I${rpmdir}/usr/include -I${rpmdir}/include"
export TEMP_LDFLAGS="-L${rpmdir}/usr/lib"

#echo "** Deploiment des rpm complementaires temporaires OK"

#########################################################################

echo "** Deploiment des librairies systemes..."
#Utilisateur ne veut pas les installer
if [ "${flag_lib_sys}" = 0 ]
then
    #on verifie qu elles y soient, sinon on stoppe
    if [ "${flag_lib_sys_installe}" = 0 ]
    then
       echo "!! ERREUR: Installation des librairies systemes non demandee mais lib. manquantes "
       echo "** Deploiment des librairies systemes : NOK"
       exit 1
    fi

#Utilisateur veut les installer
else

# *************************************
# Complements librairies (mode 64 bits)
#  => specifiques configuration ASGARD/OLYMPE
# *************************************

#**************
#libsqlite3.so
#**************
if [ ! -e /usr/lib64/libsqlite3.so.0.8.6 ]
then
    if [ ! -e "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6 ]
    then
       if [ -e "${rpmdir}"/usr/lib64/libsqlite3.so.0.8.6 ]
       then
          cp "${rpmdir}"/usr/lib64/libsqlite3.so.0.8.6 "${rep_inst_elec}"/misc/lib64/ >> /tmp/libs_$$.log 2>&1
       else
          echo "!! ERREUR : manque element systeme usr/lib64/libsqlite3.so.0.8.6"
          echo "   Probleme lors du deploiement automatique du package sqlite-3.2.8-15.2 dans RPM complement"
          echo "Liste packages sqlite deployes:"
          rpm -qa | grep sqlite
          echo "flag_lib_64=1" >> "${fichier_log}"
          exit
       fi
     fi
else
    if [ ! -e "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6 ]
    then
        ln -s /usr/lib64/libsqlite3.so.0.8.6 "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6
    fi
fi
if [ ! -e "${rep_inst_elec}"/misc/lib64/libsqlite3.so ]
then
    ln -s "${rep_inst_elec}"/misc/lib64/libsqlite3.so.0.8.6 "${rep_inst_elec}"/misc/lib64/libsqlite3.so
fi
###########################################################################################################

#****************
#libxerces-c.so
#****************
if [ ! -e /usr/lib64/libxerces-c.so.27.0 ]
then
   if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 ]
    then
       if [ -e "${rpmdir}"/usr/lib64/libxerces-c.so.27.0 ]
       then
          cp "${rpmdir}"/usr/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64 >> /tmp/libs_$$.log 2>&1
    else
        echo " !! ERREUR : manque element systeme /usr/lib64/libxerces-c.so.27.0 dans RPM complement"
        echo "    Probleme lors du deploiement automatique du package xerces-c-2.7.0-13.4 dans RPM complement"
        echo " Liste packages xerces deployes:"
        rpm -qa | grep -i xerces
        echo "flag_lib_64=4" >> "${fichier_log}"
        exit
    fi
   fi
else
    if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 ]
    then
       ln -s /usr/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0
    fi
fi
if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-c.so ]
then
   ln -s "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so
fi
if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27 ]
then
   ln -s "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27
fi
###########################################################################################################

#********************
#libxerces-depdom.so
#********************
if [ ! -e /usr/lib64/libxerces-depdom.so.27.0 ]
then
    if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 ]
    then
       if [ -e "${rpmdir}"/usr/lib64/libxerces-depdom.so.27.0 ]
       then
           cp "${rpmdir}"/usr/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64 >> /tmp/libs_$$.log 2>&1
       else
        echo " !! ERREUR : manque element systeme /usr/lib64/libxerces-depdom.so.27.0 dans RPM complement"
        echo "    Probleme lors du deploiement automatique du package xerces-depdom.so.27.0 dans RPM complement"
        echo " Liste packages xerces deployes:"
        rpm -qa | grep -i xerces
        echo "flag_lib_64=5" >> "${fichier_log}"
        exit
       fi
    fi
else
    if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 ]
    then
       ln -s /usr/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0
    fi
fi
if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so ]
then
    ln -s "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so
fi            
if [ ! -e "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27 ]
then
   ln -s "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27.0 "${rep_inst_elec}"/misc/lib64/libxerces-depdom.so.27            
fi
###########################################################################################################
#si tout est ok
echo "   Librairies systemes sous /usr/lib64 ou sous ${rep_inst_elec}/misc"
echo "** Deploiment des librairies systemes : OK "
fi

# Mise a jour du LD_LIBRARY_PATH
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib:${rep_inst_elec}/misc/lib:${rep_inst_elec}/misc/lib64"
###########################################################################################################



#######################################################################################################
#
#
#                                         GENERATION ORESTE
#
#
#######################################################################################################

echo "** Installation ORESTE, date debut installation : $(date)"

if [ "${gen_oreste}" = 1 ]
then

    # *************************************
    # 5- Installation des prerequis ORESTE
    # *************************************
    echo "** Installation prerequis ORESTE ${version_oreste} ..."

    export LD_LIBRARY_PATH="/usr/lib64:${LD_LIBRARY_PATH}"

    if [ "${flag_lib_oreste}" -eq 1 ]
    then

        # Installation de curl
        echo "  --> Installation Curl"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_curl}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_curl}"
            if [ -e "${tmpdir}"/"${curl_rep}" ]
            then
                cd "${tmpdir}"/"${curl_rep}"
                ./configure --prefix="${rep_inst_ores}"/outils/"${curl_rep}" > /tmp/prerequis_oreste_$$.log  2>&1
                make          >> /tmp/prerequis_oreste_$$.log  2>&1
                make install  >> /tmp/prerequis_oreste_$$.log  2>&1
                cd "${tmpdir}"/
                nettoie_tmpdir
            else
                echo "!! ERREUR, souci contenu archive curl : non presence de ${curl_rep} dans archive"
                exit 1
            fi
        else
            echo "!! ERREUR, pas de presence archive curl : ${archives_dir}/pre_ORESTE/${arch_curl}"
            exit 1
        fi

        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${curl_rep}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/${curl_rep}/bin:${PATH}"

        # Installation de m4
        echo "  --> Installation m4"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_m4}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_m4}"
        else
            echo "!! ERREUR, pas de presence archive m4 : ${archives_dir}/pre_ORESTE/${arch_m4}"
            exit 1
        fi
        if [ -e "${tmpdir}"/"${m4_rep}" ]
        then
            cd "${tmpdir}"/"${m4_rep}"
        else
            echo "!! ERREUR, souci contenu archive m4 : non presence de ${m4_rep} dans archive"
            exit 1
        fi
        ./configure  --prefix="${rep_inst_ores}"/outils/"${m4_rep}" \
                     >> /tmp/prerequis_oreste_$$.log 2>&1
        make         >> /tmp/prerequis_oreste_$$.log 2>&1
        make install >> /tmp/prerequis_oreste_$$.log 2>&1
        cd "${tmpdir}"
        nettoie_tmpdir
        export PATH="${rep_inst_ores}/outils/${m4_rep}/bin:${PATH}"

        # Installation de proj
        echo "  --> Installation proj"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_proj}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_proj}"
        else
            echo "!! ERREUR, pas de presence archive proj : ${archives_dir}/pre_ORESTE/${arch_proj}"
            exit 1
        fi
        if [ -e "${tmpdir}"/"${proj_rep}" ]
        then
            cd "${tmpdir}"/"${proj_rep}"
        else
            echo "!! ERREUR, souci contenu archive proj : non presence de ${proj_rep} dans archive"
            exit 1
        fi
        ./configure  --prefix="${rep_inst_ores}"/outils/"${proj_rep}" \
                     >> /tmp/prerequis_oreste_$$.log  2>&1
        make         >> /tmp/prerequis_oreste_$$.log  2>&1
        make install >> /tmp/prerequis_oreste_$$.log  2>&1
        cd "${tmpdir}"
        nettoie_tmpdir
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${proj_rep}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/${proj_rep}/bin:${PATH}"

        # Installation de gd
        echo "  --> Installation gd"
        export LDFLAGS="-L${rpmdir}/usr/lib64"
        export CPPFLAGS="-I${rpmdir}/usr/include"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_gd}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_gd}"
        else
            echo "!! ERREUR, pas de presence archive gd : ${archives_dir}/pre_ORESTE/${arch_gd}"
            exit 1
        fi
        if [ -e "${tmpdir}"/"${gd_rep}" ]
        then
            cd "${tmpdir}"/"${gd_rep}"
        else
            echo "!! ERREUR, souci contenu archive gd : non presence de ${gd_rep} dans archive"
            exit 1
        fi
        ./configure  --prefix="${rep_inst_ores}"/outils/"${gd_rep}" \
                     >> /tmp/prerequis_oreste_$$.log 2>&1
        make         >> /tmp/prerequis_oreste_$$.log 2>&1
        make install >> /tmp/prerequis_oreste_$$.log 2>&1
        cd "${tmpdir}"
        nettoie_tmpdir
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${gd_rep}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/${gd_rep}/bin:${PATH}"
        unset LDFLAGS
        unset CPPFLAGS

        # Installation de gdal
        echo "  --> Installation gdal"
        cd "${tmpdir}"
        # Souci particulier a GDAL, ne peut positionner le chemin /usr/include des RPM deployes en complements
        # Necessite de redeployer RPM JPEG/TIFF/PNG en local au repertoire compilation
        liste_rpm="$(ls "${archives_dir}"/UTILITAIRES/RPM_complements/*jpeg*.rpm \
                        "${archives_dir}"/UTILITAIRES/RPM_complements/*png*.rpm  \
                        "${archives_dir}"/UTILITAIRES/RPM_complements/*tiff*     \
                        "${archives_dir}"/UTILITAIRES/RPM_complements/*zlib*.rpm)"
        for i in ${liste_rpm[*]}
        do
            if [ -e "$i" ]
            then
                rpm2cpio "$i" | cpio -di  >> /tmp/rpm2cpio_$$.log  2>&1
            fi
        done
        # Effacer fichiers configuration "XXX.la" car font reference a racine systeme "/"
        find "${tmpdir}" -name "*.la" | xargs rm
        export LDFLAGS="-L${tmpdir}/usr/lib64"
        export CPPFLAGS="-I${tmpdir}/usr/include -I${rpmdir}/usr/include/c++/4.1.2/ -I${rpmdir}/usr/include/c++/4.1.2/x86_64-suse-linux/"
        export CXXFLAGS="-I${tmpdir}/usr/include -I${rpmdir}/usr/include/c++/4.1.2/ -I${rpmdir}/usr/include/c++/4.1.2/x86_64-suse-linux/"

        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_gdal}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_gdal}"
        else
            echo "!! ERREUR, pas de presence archive gdal : ${archives_dir}/pre_ORESTE/${arch_gdal}"
            exit 1
        fi
        if [ -e "${tmpdir}"/"${gdal_rep}" ]
        then
            cd "${tmpdir}"/"${gdal_rep}"
        else
            echo "!! ERREUR, souci contenu archive gdal : non presence de $gdal_rep dans archive"
            exit 1
        fi
        ./configure  --prefix="${rep_inst_ores}"/outils/"${gdal_rep}" \
                     --without-libtool \
                     --with-ogr \
                     --with-jpeg \
                     --with-gif \
                     --with-libtiff \
                     --with-png \
                     --with-oci=yes \
                     --with-expat=no \
                     >> /tmp/prerequis_oreste_$$.log 2>&1
        echo "-----fin configure gdal---------" >> /tmp/prerequis_oreste_$$.log
        make         >> /tmp/prerequis_oreste_$$.log 2>&1
        echo "-----fin make gdal---------" >> /tmp/prerequis_oreste_$$.log
        make install >> /tmp/prerequis_oreste_$$.log 2>&1
        echo "-----fin make install gdal---------" >> /tmp/prerequis_oreste_$$.log

        cd "${tmpdir}"
        nettoie_tmpdir
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${gdal_rep}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/${gdal_rep}/bin:${PATH}"
        unset LDFLAGS
        unset CPPFLAGS
        unset CXXFLAGS

        # Necessaire car ORESTE a besoin d'avoir toutes les librairies dans le meme repertoire
        echo "export PATH=\"\${rep_oreste}/outils/${gdal_rep}/bin:\${PATH}\""                       >> "${fichier_env}"
        echo "export LD_LIBRARY_PATH=\"\${rep_oreste}/outils/${gdal_rep}/lib:\${LD_LIBRARY_PATH}\"" >> "${fichier_env}"

        if [ -d "${rep_inst_ores}"/outils/"${gdal_rep}"/lib ]
        then
            cd "${rep_inst_ores}"/outils/"${gdal_rep}"/lib
        else
            echo "!! ERREUR, probleme installation gdal : absence de ${rep_inst_ores}/outils/${gdal_rep}/lib"
            exit 1
        fi

        # Creation dans le repertoire de gdal de liens symboliques vers les librairies utilisees par gdal
        # Cela permet de retrouver toutes les librairies en rajoutant seulement le chemin vers gdal dans la
        # Les liens doivent etre faits en relatif pour pouvoir installer ORESTE a un endroit different
        # lorsque l'on utilise le script d'installation des binaires
        if [ -e ../../"${gd_rep}"/lib/libgd.so.2 ]
        then
            ln -s ../../"${gd_rep}"/lib/libgd.so.2 libgd.so.2
        else
            echo "!! ERREUR, souci pour creation lien libgd.so.2 dans repertoire ${rep_inst_ores}/outils/${gd_rep}/lib"
            exit 1
        fi
        if [ -e ../../"${proj_rep}"/lib/libproj.so.0 ]
        then
            ln -s ../../"${proj_rep}"/lib/libproj.so.0 libproj.so.0
        else
            echo "!! ERREUR, souci pour creation lien libproj.so.0 dans repertoire ${rep_inst_ores}/outils/${gd_rep}/lib"
            exit 1
        fi
        if [ -e ../../"${curl_rep}"/lib/libcurl.so.3 ]
        then
            ln -s ../../"${curl_rep}"/lib/libcurl.so.3 libcurl.so.3
        else
            echo "!! ERREUR, souci pour creation lien libcurl.so.3 dans repertoire ${rep_inst_ores}/outils/${gd_rep}/lib"
            exit 1
        fi
        if [ -d "${rep_inst_ores}"/outils/"${gdal_rep}"/share ]
        then
            cd "${rep_inst_ores}"/outils/"${gdal_rep}"/share
        else
            echo "!! ERREUR, inexistence repertoire installation ${rep_inst_ores}/outils/${gdal_rep}/share"
            exit 1
        fi
        if [ -d ../../"${proj_rep}"/share/proj ]
        then
            ln -s ../../"${proj_rep}"/share/proj proj
        else
            echo "!! ERREUR, inexistence repertoire installation ${rep_inst_ores}/outils/${proj_rep}/share/proj"
            exit 1
        fi

        # Installation de MapFish
        echo "  --> Installation MapFish"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_mapfish}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_mapfish}"
        else
            echo "!! ERREUR, absence archive mapfish : ${archives_dir}/pre_ORESTE/${arch_mapfish}"
            exit 1
        fi
        if [ -d "${tmpdir}"/"${mapfish_rep}" ]
        then
            mv "${tmpdir}"/"${mapfish_rep}" "${rep_inst_ores}"/outils/
        else
            echo "!! ERREUR, mauvais contenu archive mapfish : absence de ${mapfish_rep}"
            exit 1
        fi

    # Si les prerequis ne sont pas installes, on les fait pointer sur le repertoire fourni
    else
        liste_prerequis=("${mapfish_rep}" "${gdal_rep}" "${curl_rep}" "${gd_rep}" "${m4_rep}" "${proj_rep}")

        if [ ! -d "${rep_inst_ores}"/outils ]
        then
            mkdir "${rep_inst_ores}"/outils
        fi

        cd "${rep_inst_ores}"/outils

        for i in "${liste_prerequis[@]}"
        do
            echo "  --> Creation lien vers $i"
            ln -s "${rep_lib_oreste}"/"$i" .
        done

        # mise a jour de l'environnement
        export LD_LIBRARY_PATH="${rep_curl}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_curl}/bin:${PATH}"

        export LD_LIBRARY_PATH="${rep_proj}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_proj}/bin:${PATH}"

        export LD_LIBRARY_PATH="${rep_gd}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_gd}/bin:${PATH}"

        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${gdal_rep}/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/${gdal_rep}/bin:${PATH}"
    fi

    # Installation de Apache seulement si demande par l'utilisateur
    if [ "${flag_httpd}" -eq 1 ]
    then
        echo "  --> Installation Apache"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_apache}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_apache}"
        else
            echo "!! ERREUR, absence archive apache : ${archives_dir}/pre_ORESTE/${arch_apache}"
            exit 1
        fi
        if [ -d "${tmpdir}"/"${apache_rep}" ]
        then
            cd "${tmpdir}"/"${apache_rep}"
        else
            echo "!! ERREUR, mauvais contenu archive apache : absence de ${apache_rep}"
            exit 1
        fi
        ./configure  --prefix="${rep_inst_ores_httpd}/${apache_rep}" \
                     >> /tmp/prerequis_oreste_$$.log 2>&1
        make         >> /tmp/prerequis_oreste_$$.log 2>&1
        make install >> /tmp/prerequis_oreste_$$.log 2>&1

        cd "${tmpdir}"
        nettoie_tmpdir
    fi

    sed -i -e 's/^Listen.*/Listen localhost:1036/g'  \
           -e 's/ :1036/:1036/g'                     \
           "${rep_inst_ores_httpd}"/"${apache_rep}"/conf/httpd.conf

    # Installation de php5 s'il n'est pas present avec l'installation de Apache
    if [ ! -e "${rep_inst_ores_httpd}"/"${apache_rep}"/modules/libphp5.so ]
    then
        echo "  --> Installation de php5"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_php5}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_php5}"
        else
            echo "!! ERREUR, absence archive php5 : ${archives_dir}/pre_ORESTE/${arch_php5}"
            exit 1
        fi
        if [ -d "${tmpdir}/${php5_rep}" ]
        then
            cd "${tmpdir}/${php5_rep}"
        else
            echo "!! ERREUR, mauvais contenu archive php5: absence de ${php5_rep}"
            exit 1
        fi
        export LDFLAGS="-L${rpmdir}/usr/lib64"
        export CPPFLAGS="-I${rpmdir}/usr/include"
        ./configure  --prefix="${rep_inst_ores_httpd}"/"${apache_rep}"/php \
                     --enable-zip \
                     --enable-gd-native-ttf \
                     --with-apxs2="${rep_inst_ores_httpd}"/"${apache_rep}"/bin/apxs \
                     --with-gd="${rep_inst_ores}"/outils/"${gd_rep}" \
                     --with-zlib-dir="${rpmdir}"/usr/ \
                     --with-libxml-dir="${rpmdir}"/usr/ \
                     >> /tmp/prerequis_oreste_$$.log  2>&1
        make         >> /tmp/prerequis_oreste_$$.log  2>&1
        make install >> /tmp/prerequis_oreste_$$.log  2>&1
        unset LDFLAGS
        unset CPPFLAGS

        cd "${tmpdir}"
        nettoie_tmpdir
    fi

    if [ "${flag_lib_oreste}" -eq 1 ]
    then

        # Installation de MapServer
        echo "  --> Installation de MapServer"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_mapserver}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_mapserver}"
        else
            echo "!! ERREUR, absence archive mapserver : ${archives_dir}/pre_ORESTE/${arch_mapserver}"
            exit 1
        fi
        if [ -d "${tmpdir}"/"${mapserver_rep}" ]
        then
            cd "${tmpdir}"/"${mapserver_rep}"
        else
            echo "!! ERREUR, mauvais contenu archive mapserver: absence de ${mapserver_rep}"
            exit 1
        fi

        export CC="gcc -I${rpmdir}/usr/include -L${rpmdir}/usr/lib64"
        export CXX="g++ -I${rpmdir}/usr/include -L${rpmdir}/usr/lib64"
        ./configure --with-jpeg \
                    --with-threads \
                    --with-ogr="${rep_inst_ores}"/outils/"${gdal_rep}"/bin/gdal-config \
                    --with-gdal="${rep_inst_ores}"/outils/"${gdal_rep}"/bin/gdal-config \
                    --with-curl-config="${rep_inst_ores}"/outils/"${curl_rep}"/bin/curl-config \
                    --with-proj="${rep_inst_ores}"/outils/"${proj_rep}" \
                    --with-httpd="${rep_inst_ores_httpd}"/"${apache_rep}"/bin/httpd \
                    --with-gd="${rep_inst_ores}"/outils/"${gd_rep}" \
                    >> /tmp/prerequis_oreste_$$.log 2>&1

        make  >> /tmp/prerequis_oreste_$$.log  2>&1

        if [ -e "${rep_inst_ores}"/outils/"${mapserver_rep}" ]
        then
            rm -rf "${rep_inst_ores}"/outils/"${mapserver_rep}"
        fi
        mkdir -p "${rep_inst_ores}"/outils/"${mapserver_rep}"

        liste_fichiers_mapserve=('mapserv' 'mapserver-config' 'scalebar' 'shptreevis' 'legend' 'shp2img' 'sortshp' 'shp2pdf' 'shptree' 'tile4ms' 'msencrypt' 'shptreetst')
        for i in "${liste_fichiers_mapserve[@]}"
        do
            if [ -e "$i" ]
            then
                cp "$i" "${rep_inst_ores}"/outils/"${mapserver_rep}"
            else
                echo "!! ERREUR, le deploiement de mapserver ne donne pas executable $i, dont la presence est verifiee par ./xpl/install_oreste"
                exit 1
            fi
        done
        cd "${tmpdir}"
        nettoie_tmpdir
        export PATH="${rep_inst_ores}/outils/${mapserver_rep}:${PATH}"
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${gdal_rep}/lib:${LD_LIBRARY_PATH}"
        unset CC
        unset CXX


        #Dans le cas ou les prerequis ne sont pas reinstalles
    else
        prerequis=('mapserver-5.6.3')

        cd "${rep_inst_ores}"/outils
        for i in "${prerequis[@]}"
        do
            echo "  --> Creation lien vers $i"
            ln -s "${rep_lib_oreste}"/"$i" .
        done

        export PATH="${rep_inst_ores}/outils/${mapserver_rep}:${PATH}"
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/${gdal_rep}/lib:${LD_LIBRARY_PATH}"
    fi

    # Installation de Global Insight

    # Creation de la variable d'option pour le shell xpl/install_oreste
    eof_globalinsight=-
    flag_globalinsight=N

    if [ "${flag_GlobalInsight}" -eq 1 ]
    then
        echo "  --> Installation Global Insight"
        cd "${rep_inst_ores}"/
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_GlobalInsight}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_GlobalInsight}"
            if [ -d "${rep_inst_ores}"/GlobalInsight/7.1/ESRI ]
            then
                cd "${rep_inst_ores}"/GlobalInsight/7.1/ESRI
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                eof_globalinsight="${rep_inst_ores}/GlobalInsight/7.1/ESRI"
                flag_globalinsight="O"
                echo "   Global insight situee sous " ${rep_inst_ores}/GlobalInsight
            else
                echo "!! WARNING, mauvais contenu archive global insight : absence ${rep_inst_ores}/GlobalInsight/7.1/ESRI. Installation avortee"
            fi
        else
            echo "!! WARNING, absence archive global insight : ${archives_dir}/pre_ORESTE/${arch_GlobalInsight}. Installation avortee"
        fi
    else
        if [ -z "${rep_GlobalInsight}" ] || [ "${rep_GlobalInsight}" = "null" ]
        then
            echo "Global insight n'est pas installe"
        else
            if [ -d "${rep_GlobalInsight}"/7.1/ESRI ]
            then
                echo "  --> Creation lien vers GlobalInsight"
                cd "${rep_inst_ores}"
                ln -s "${rep_GlobalInsight}" GlobalInsight
                cd "${rep_inst_ores}"/GlobalInsight/7.1/ESRI
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                eof_globalinsight="${rep_inst_ores}/GlobalInsight/7.1/ESRI"
                flag_globalinsight="O"
                echo "   Global insight situee sous " ${rep_GlobalInsight}
            else
                echo "!! WARNING, mauvais contenu du repertoire global insight : absence ${rep_GlobalInsight}/7.1/ESRI. I.nstallation avortee"
            fi
        
        fi
    fi

    # Installation des plateformes petrolieres

    # Creation de la variable d'option pour le shell xpl/install_oreste
    eof_infieldenergygateway=-
    flag_infieldenergygateway=N

    if [ "${flag_InfieldEnergyGateway}" -eq 1 ]
    then
        echo "  --> Installation de la base de donnees Infield Energy Gateway"
        cd "${rep_inst_ores}"/
        if [ -e "${archives_dir}"/pre_ORESTE/"${arch_InfieldEnergyGateway}" ]
        then
            tar -xzf "${archives_dir}"/pre_ORESTE/"${arch_InfieldEnergyGateway}"           
            if [ -e "${rep_inst_ores}"/InfieldEnergyGateway/platforms.shp ]
            then
                cd "${rep_inst_ores}"/InfieldEnergyGateway
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                eof_infieldenergygateway="${rep_inst_ores}"/InfieldEnergyGateway
                flag_infieldenergygateway="O"
                echo "   InfieldEnergyGateway situee sous " ${rep_inst_ores}/InfieldEnergyGateway
            else
                echo "!! WARNING, mauvais contenu archive Infield energy Gateway : absence ${rep_inst_ores}/InfieldEnergyGateway/platforms.shp. Installation avortee"
            fi           
        else
            echo "!! WARNING, absence archive Infield Energy Gateway : ${archives_dir}/pre_ORESTE/${arch_InfieldEnergyGateway}. Installation avortee"
        fi
        
    else
        if [ -z "${rep_InfieldEnergyGateway}" ] || [ "${rep_InfieldEnergyGateway}" = "null" ]
        then
            echo "Les plateformes ne sont pas installees"
        else
            if [ -d "${rep_InfieldEnergyGateway}" ]
            then
                echo "  --> Creation lien vers InfieldEnergyGateway"
                cd "${rep_inst_ores}"
                ln -s "${rep_InfieldEnergyGateway}" InfieldEnergyGateway
                cd "${rep_inst_ores}"/InfieldEnergyGateway
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                eof_infieldenergygateway="${rep_inst_ores}"/InfieldEnergyGateway
                flag_infieldenergygateway="O"
                echo "   InfieldEnergyGateway situee sous " ${rep_InfieldEnergyGateway}
            else
                echo "!! WARNING, mauvais contenu du repertoire InfieldEnergyGateway : absence ${rep_InfieldEnergyGateway}. Installation avortee"
            fi
        fi
    fi

    if [ "${flag_lib_oreste}" -eq 1 ]
    then

        # Installation de Firefox 3
        echo "  --> Installation Firefox 3"
        cd "${tmpdir}"
        if [ -e "${archives_dir}"/UTILITAIRES/installation_firefox.tar ]
        then
            tar -xf "${archives_dir}"/UTILITAIRES/installation_firefox.tar
            cd installation_firefox
            ./compilation_firefox << EOF >> /tmp/prerequis_oreste_$$.log  2>&1
${rep_inst_ores}/outils/firefox-3/
${rpmdir}/
EOF
            cd "${tmpdir}"
            nettoie_tmpdir
        else
            echo "!! ERREUR, absence archive firefox : ${archives_dir}/UTILITAIRES/installation_firefox.tar"
            exit 1
        fi
        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/firefox-3/prerequis/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/firefox-3:${rep_inst_ores}/outils/firefox-3/prerequis/bin:${PATH}"

        # Installation du plugin Firefox pour graphe d'ORESTE
        echo "      -> Installation Plugin java pour Firefox 3"
        #  Lignes commentees suivantes pour utilisation plugin jre-1_5_0_22-linux-i586.bin
        cd "${rep_inst_ores}"/outils

        if [ -d "${rep_inst_ores}"/outils/firefox-3/plugins/ ]
        then
            cd  "${rep_inst_ores}"/outils/firefox-3/plugins
            if [ -e "${JAVA_HOME}"/jre/plugin/i386/ns7/libjavaplugin_oji.so ]
            then
                 #Sinon utilisation plugin 1.6 deja deploye sur Asgard
                ln -s "${JAVA_HOME}"/jre/plugin/i386/ns7/libjavaplugin_oji.so .
            else
                echo "!! WARNING, mauvais deploiement de ${JAVA_HOME}"
                echo "il manque le plugin ${JAVA_HOME}/jre/plugin/i386/ns7/libjavaplugin_oji.so"
                echo "Les graphes Oreste seront vides"
            fi
        else
            echo "!! ERREUR, mauvais deploiement firefox : absence repertoire ${rep_inst_ores}/outils/firefox-3/plugins/"
            exit 1
        fi
        echo "   Prerequis  ORESTE sous " ${rep_inst_ores}/outils

    # Dans le cas ou les prerequis ne sont pas reinstalles
    else
        echo "   Prerequis  ORESTE sous " ${rep_lib_oreste}
        cd "${rep_inst_ores}"/outils
        echo "  --> Creation lien vers firefox-3"
        ln -s "${rep_lib_oreste}"/firefox-3 .

        export LD_LIBRARY_PATH="${rep_inst_ores}/outils/firefox-3/prerequis/lib:${LD_LIBRARY_PATH}"
        export PATH="${rep_inst_ores}/outils/firefox-3:${rep_inst_ores}/outils/firefox-3/prerequis/bin:${PATH}"

    fi

    # Pour que le post-traitement utilise le bon java
    echo "export PATH="${JAVA_HOME}"/jre/bin:\$PATH"    >> "${fichier_env}"

    echo "** Installation prerequis ORESTE ${version_oreste} OK"


    # **************************
    # 6- Installation de ORESTE
    # **************************
    echo "** Installation ORESTE ${version_oreste} ..."
    cd "${tmpdir}"
    if [ -e "${archives_dir}"/ORESTE/"${arch_oreste_gz}" ]
    then
        cp "${archives_dir}"/ORESTE/"${arch_oreste_gz}" .
        gunzip -d "${arch_oreste_gz}"
        tar -xf "${arch_oreste}".tar ./xpl/install_oreste
    else
        echo "!! ERREUR, absence archive oreste : ${archives_dir}/ORESTE/${arch_oreste}"
        exit 1
    fi
    version_electra_sans_v=`echo "${version_electra}" | sed "s,V,,g"`
    # Probleme pour faire le test a la fin de install_oreste
    # Depuis le compte root il n'arrive pas a lire le fichier
    sed -i "s,java -cp, #java -cp,g" ./xpl/install_oreste

    if [ -e ./xpl/install_oreste ]
    then
        if [ "${flag_infieldenergygateway}" = "N" ]
        then
            if [ "${flag_globalinsight}" = "N" ]
            then
            # Appel de l'installation
                ./xpl/install_oreste << EOF > /tmp/oreste_$$.log 2>&1
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
N
N
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
            else
                # Appel de l'installation
                ./xpl/install_oreste << EOF > /tmp/oreste_$$.log 2>&1
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
O
${eof_globalinsight}
N
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
            fi
        else
            if [ "${flag_globalinsight}" = "N" ]
            then
                # Appel de l'installation
                ./xpl/install_oreste << EOF > /tmp/oreste_$$.log 2>&1
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
N
O
${eof_infieldenergygateway}
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
            else
                # Appel de l'installation
                ./xpl/install_oreste << EOF > /tmp/oreste_$$.log 2>&1
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
O
${eof_globalinsight}
O
${eof_infieldenergygateway}
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
            fi
        fi

        # Extraction des repertoires conf et test de l'archive que l'on recopie 
        # dans le repertoire d'installation d'ORESTE pour pouvoir reconfigurer
        # le serveur Apache dans la phase de deploiement des binaires si 
        # l'operateur demande de reutiliser une installation existante de Apache
        tar -C "${rep_inst_ores}"/ORESTE/"${version_oreste}" -xf "${arch_oreste}".tar ./conf/ ./test/

    else
        echo "!! ERREUR, mauvais contenu archive oreste : absence ./xpl/install_oreste"
        exit 1
    fi
    if [ ! -e "${rep_inst_ores}"/ORESTE/"${version_oreste}"/xpl/oreste ]
    then
        echo "!! ERREUR INSTALLATION D'ORESTE INCOMPLETE "
        echo "Voir /tmp/oreste_$$.log ou ${tmpdir}/oreste_installation.log"
        exit 1
    fi

    sed -i -e 's/Listen 80/Listen localhost:1036/g'  \
           -e 's/ :1036/:1036/g'                     \
           -e 's/User daemon/User electra/g'         \
           -e "s/Group daemon/Group ${user_group}/g" \
           "${rep_inst_ores_httpd}"/"${apache_rep}"/conf/httpd.conf

    cd $archives_dir
    # Creation script pour raccourci bureau
    script_raccourci="${rep_inst_ores}/ORESTE/${version_oreste}/xpl/start_oreste"
    cat > "${script_raccourci}" << EOF
#!/bin/ksh
. ${rep_inst_ores}/env_oreste
oreste
EOF

    # Creation script pour creation Icone bureau
    script_bureau="${rep_inst_ores}/ORESTE/${version_oreste}/xpl/ORESTE.Desktop"
    cat > "${script_bureau}" << EOF
[Desktop Entry]
Comment=Soumission Trajectoire
Comment[fr]=Soumission Trajectoire
Encoding=UTF-8
Exec=${script_raccourci}
GenericName=Application ORESTE
GenericName[fr]=Application ORESTE
Icon=${rep_inst_ores}/ORESTE/Oreste.png
MimeType=
Name=ORESTE
Name[fr]=ORESTE
Path=\$\$HOME
StartupNotify=false
Terminal=false
TerminalOptions=
Type=Application
X-DCOP-ServiceType=
X-KDE-SubstituteUID=false
X-KDE-UserName=
EOF

    export PATH="${rep_inst_ores}/ORESTE/${version_oreste}/xpl:${PATH}"

    # Copie fichier pour initier environnement ORESTE
    if [ -e "${fichier_env}" ]
    then
        cp "${fichier_env}" "${rep_inst_ores}"/env_oreste
        #Integration de la mise a jour de l'environnement dans le script de lancement
        sed "s,if,. ${rep_inst_ores}\/env_oreste \\nif,g" "${rep_inst_ores}"/ORESTE/"${version_oreste}"/xpl/oreste > /tmp/script_oreste
        mv /tmp/script_oreste "${rep_inst_ores}"/ORESTE/"${version_oreste}"/xpl/oreste

    else
        echo "!! WARNING, fichier environnement (${fichier_env}) non trouve"
    fi
    #Installation du MU
    if [ -e "${archives_dir}"/DOCUMENTATION/"${electra_mu}" ]
    then
        cp -f "${archives_dir}"/DOCUMENTATION/"${electra_mu}" "${rep_inst_ores}"/ORESTE/"${version_oreste}"/www/data/manuel.pdf
    else
        echo "!! ERREUR, archive ${electra_mu} non trouve dans ${archives_dir}/DOCUMENTATION/"
        echo "   ==> revoir contenu de ${archives_dir}/DOCUMENTATION"
        exit 1
    fi

    if [ -e "${archives_dir}"/UTILITAIRES/template_english.odt ]
    then
    	rm "${rep_inst_ores}"/ORESTE/"${version_oreste}"/www/data/test/templates/*
    	cp "${archives_dir}"/UTILITAIRES/template_english.odt "${rep_inst_ores}"/ORESTE/"${version_oreste}"/www/data/test/templates/
    else
    	echo "!! WARNING, archive template_english.odt non trouve dans ${archives_dir}/UTILITAIRES/"
    	echo "   ==> revoir contenu de ${archives_dir}/UTILITAIRES/"
    fi

    if [ -e "${archives_dir}"/UTILITAIRES/Oreste.png ]
    then
        cp "${archives_dir}"/UTILITAIRES/Oreste.png "${rep_inst_ores}"/ORESTE
    else
        echo "!! ERREUR, archive Icon electra.png non trouve dans ${archives_dir}/UTILITAIRES"
        echo "   ==> revoir contenu de ${archives_dir}/UTILITAIRES"
        exit 1
    fi
    #-------------------------
    #Signature de l'applet graph d'ORESTE
    #-----------------------
    cd "${rep_inst_ores}"/ORESTE/"${version_oreste}"/applets/graph

    #Generation d'une cle de signature
    LC_ALL=C "${JAVA_HOME}"/bin/keytool -genkey -validity 999 -keypass electra -alias electra << EOF >  /tmp/oreste_$$.log 2>&1
electra
projet electra
DCT/SB/MO
CNES
Toulouse
France
fr
yes
EOF
    "${JAVA_HOME}"/bin/jarsigner -verbose JChartOreste.jar electra << EOF >  /tmp/oreste_$$.log 2>&1
electra
EOF
    if [ -e /root/.keystore ]
    then
        rm /root/.keystore
    else
        echo "------------------------------------------"         > /tmp/oreste_$$.log
        echo "!! Warning, impossible d'effacer .keystore"         > /tmp/oreste_$$.log
        echo "Le fichier ne se trouve pas sur /root/.keystore"    > /tmp/oreste_$$.log
        echo "Regarder sur /repertoireroot/[superuser]/.keystore" > /tmp/oreste_$$.log
        echo "------------------------------------------"         > /tmp/oreste_$$.log
        echo "     Warning :/tmp/oreste_$$.log"
    fi

    # Modification droits pour ORESTE
    chown -R electra:"${user_group}" "${rep_inst_ores}"
    chmod -R 750 "${rep_inst_ores}"
    if [ "${flag_httpd}" -eq 1 ]
    then
        # Modification droits pour Apache ORESTE
        #  => pour plan securite R.16
        chown -R electra:"${user_group}" "${rep_inst_ores_httpd}"
        chmod -R 700 "${rep_inst_ores_httpd}"
        chown -R root:"${user_group}" "${rep_inst_ores_httpd}"/"${apache_rep}"/bin \
                                      "${rep_inst_ores_httpd}"/"${apache_rep}"/conf
        chmod -R 750 "${rep_inst_ores_httpd}"/"${apache_rep}"/bin  \
                     "${rep_inst_ores_httpd}"/"${apache_rep}"/conf
    fi
    echo "** Installation ORESTE ${version_oreste} OK"
    echo "** Installation ORESTE, date fin installation : $(date)"
else
    echo "** Installation ORESTE ${version_oreste} non demandee"
fi

#********************************************************
# Creation des liens pour future installation sur Asgard
#********************************************************
echo "Creation des liens"

cd "${rep_inst_ores}"/outils/"${gdal_rep}"/lib
if [ ! -e libxerces-c.so.27 ]
then
    if [ -e /usr/lib64/libxerces-c.so.27 ]
    then
       ln -s /usr/lib64/libxerces-c.so.27.0 libxerces-c.so.27
    else
       ln -s "${rep_inst_elec}"/misc/lib64/libxerces-c.so.27.0 libxerces-c.so.27
    fi
fi
cd "${archives_dir}"

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

#****************************
# Demarrage du serveur Apache
#****************************
echo "Demarrage du serveur Apache"

# Arret de Apache s'il est demarre
if pgrep httpd >/dev/null
then
    "${rep_inst_ores_httpd}"/"${apache_rep}"/bin/httpd -k stop
    sleep 3
    killall httpd
    sleep 3
fi
"${rep_inst_ores_httpd}"/"${apache_rep}"/bin/httpd -k start

echo "** Installation ORESTE sous " ${rep_inst_ores}
echo "** Installation ORESTE ${version_oreste} OK"
echo "** Installation ORESTE, date fin installation : $(date)"
