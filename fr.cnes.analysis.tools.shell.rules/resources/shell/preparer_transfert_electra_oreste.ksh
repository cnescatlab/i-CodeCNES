#!/bin/ksh
#*****************************************************************
#$<AM-V2.0>
#
#$Type
#	PROG
#
#$Nom
#	preparer_transfert_electra_oreste.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#
#$Resume
#	"Creation de l'archive d'ELECTRA et ORESTE pour transfert"
#
#$Auteur
#   CAPGEMINI
#
#$Description
#	Ce script shell permet de creer l'archive pour le transfert d'ELECTRA et ORESTE.
#       - Preparation du repertoire de transfert
#       - Archivage d'ELECTRA
#       - Archivage d'ORESTE
#       - Archivage repertoire d'installation Apache ORESTE
#       - Archivage repertoire d'installation Global Insigth
#       - Archivage repertoire d'installation Plateformes petrolieres
#       - Creation script d'installation
#
#$version
#	$Id: preparer_transfert_electra_oreste.ksh 2496 2014-07-18 09:27:04Z mbeuvelo $
#
#$Historique
#    VERSION:3.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
#
#    VERSION:3.0.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
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

### FA 707: spécificité Asgard/Olympe ########################
#pour ne pas être limité sur la taille des fichiers notamment les archives *.tar
ulimit unlimited 
#######################################################################################

# ***********************
#  Definition repertoires
# ***********************
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# Apache est present et l'utilisateur souhaite utiliser l'existant
if [ "${flag_httpd}" -eq 0 ]
then
    # Description des variables rep_inst_ores_httpd et apache_rep
    # Exemple dans le cas ou Apache installe sous : /usr/local
    # => rep_inst_ores_httpd=/usr/local
    # => apache_bin=/usr/local/http-2.2/bin/httpd
    # => apache_rep=http-2.2
    apache_bin="$(find "${rep_inst_ores_httpd}" -type f -name httpd)"
    apache_rep="$(basename "$(readlink -f "$(dirname "${apache_bin}")"/..)")"
fi

# *********************
# Differentes fonctions
# *********************
nettoie_elements() {
    echo " --> Nettoyage elements *.F90 *.o *.tar *.gz *.java *.f *.c *.C *.mod *.a *.la"
    find . -name "*.[Ff]90" | xargs rm -f
    find . -name "*.o"      | xargs rm -f
    find . -name "*.tar"    | xargs rm -f
    find . -name "*.gz"     | xargs rm -f
    find . -name "*.java"   | xargs rm -f
    find . -name "*.[fF]"   | xargs rm -f
    find . -name "*.[Cc]"   | xargs rm -f
    find . -name "*.mod"    | xargs rm -f
    find . -name "*.a"      | xargs rm -f
    find . -name "*.la"     | xargs rm -f
}
# nettoie_elements()

echo "** Lancement script pour creation archive transfert..."

# Nettoyage du repertoire d'archivage
if [ -e "${rep_transf}" ]
then
    if [ "x${rep_transf}" != "x/" ] && [ "x${rep_transf}" != "x" ]
    then
        echo "Repertoire de transfert existant"
        echo "Le script va l'effacer"
        rm -rf "${rep_transf}"
    else
        echo "!! ERREUR, repertoire pour creation archive (rep_transf) renvoie a la racine du serveur : \$rep_transf = ${rep_transf}"
        echo "  => changer le nom de ce repertoire dans le script $0"
        exit 1
    fi
fi
mkdir -p "${rep_transf}"
echo

# Les archives sont creees avec l'option -h pour stocker la cible des
# liens symboliques et non pas le lien. C'est necessaire pour pouvoir
# redeployer sur une machine vierge sur laquelle la cible des liens ne
# sont pas presentes. Certins elements n'etant pas relocalisables, il
# faut alors enregistrer en plus l'arborescence initiale

# Archivage de l'application ELECTRA
if [ -d "${rep_inst_elec}" ]
then
    echo "** Repertoire installation Electra:"

    echo " --> Phase archivage repertoire installation ELECTRA..."
    # Copie du fichier properties.sh dans le repertoire d'installation
    if [ ! -d "${rep_inst_elec}"/shells ]
    then
        mkdir "${rep_inst_elec}"/shells
    fi
    cp -p "${chemin}"/properties.sh "${rep_inst_elec}"/shells/properties.sh

    # copie du repertoire d installation complet sous tmp
    cp -rp "${rep_inst_elec}" "${rep_transf}"/tmp
    cd "${rep_transf}"/tmp
    nettoie_elements
    fich_install_user="${rep_transf}/tmp/shells/installation_user.ksh"
    sed -i -e 's,export version_electra=.*,export version_electra='${version_electra}',g' \
           -e 's,export version_oreste=.*,export version_oreste='${version_oreste}',g'    \
           "${fich_install_user}"
    fic='test_numeriques.tar.gz'
    cp -p "${rep_inst_elec}"/ELECTRA/"${version_electra}"/"${fic}" ELECTRA/"${version_electra}"/"${fic}"
    liste_elements=('ELECTRA' 'env_electra' 'misc' 'shells')
    unset liste_archive
    for i in "${liste_elements[@]}"
    do
        if [ ! -e "$i" ]
        then
            echo "!! WARNING, manque  element $i logiquement installe avec ELECTRA ${version_electra}"
            echo "  => verifier bon deploiement ELECTRA ${version_electra}"
        else
            liste_archive+=("$i")
        fi

    done

    # OpenMPI n'est pas relocalisable, si c'est un lien symbolique il faut
    # se souvenir de toute l'arborescence pointee par le lien pour pouvoir
    # la recreer lors du deploiement des binaires.
    > "${rep_transf}"/tmp/liste_liens_symboliques
    if [ -L "${rep_inst_elec}"/misc/openmpi ]
    then
        openmpi="$(readlink -f "${rep_inst_elec}"/misc/openmpi)"
        echo "export symlink_openmpi=${openmpi}" >> "${rep_transf}"/tmp/liste_liens_symboliques
    fi

    if [ "${#liste_archive[@]}" -gt 0 ]
    then
        liste_archive+=('liste_liens_symboliques')
        tar -czhf "${arch_electra_bin}" "${liste_archive[@]}"
        mv "${arch_electra_bin}" "${rep_transf}"
    fi

    \rm -rf "${rep_transf}"/tmp
else
    echo "!! ERREUR, pas de repertoire d installation ELECTRA ${version_electra} : \$rep_inst_elec = ${rep_inst_elec}"
    exit 1
fi
echo

# Archivage de l'application ORESTE
if [ "${gen_oreste}" = 1 ] && [ -d "${rep_inst_ores}" ]
then
    echo "** Repertoire installation Oreste"
    cp -rp "${rep_inst_ores}" "${rep_transf}"/tmp
    echo " --> Nettoyage de liens pour les utilisateurs"
    cd "${rep_transf}"/tmp/ORESTE/V*/www/data
    unset liste_liens
    for fichier in *
    do
        [ -L "${fichier}" ] && liste_liens+=("${fichier}")
    done
    if [ "${#liste_liens[@]}" -gt 0 ]
    then
        for i in "${liste_liens[@]}"
        do
            rm "$i"
        done
    fi
    cd "${rep_transf}"/tmp
    nettoie_elements
    echo " --> Phase archivage repertoire installation ORESTE..."
    liste_elements=('env_oreste' 'ORESTE' 'outils')
    unset liste_archive
    for i in "${liste_elements[@]}"
    do
        if [ ! -e "$i" ]
        then
            echo "!! WARNING, manque  element $i logiquement installe avec ORESTE ${version_oreste}"
            echo "  => verifier bon deploiement ORESTE ${version_oreste}"
        else
            liste_archive+=("$i")
        fi
    done

    # Les pre-requis ne sont pas relocalisables, si c'est un lien symbolique il
    # faut se souvenir de toute l'arborescence pointee par le lien pour pouvoir
    # la recreer lors du deploiement des binaires.
    liste_prerequis=('firefox-3' "${mapserver_rep}" "${mapfish_rep}" "${gdal_rep}" "${curl_rep}" "${gd_rep}" "${m4_rep}" "${proj_rep}")
    > "${rep_transf}"/tmp/liste_liens_symboliques
    for prerequis in "${liste_prerequis[@]}"
    do
        if [ -L "${rep_inst_ores}"/outils/"${prerequis}" ]
        then
            lien_present=1
            lien_prerequis="$(readlink -f "${rep_inst_ores}"/outils/"${prerequis}")"
            echo "export symlink_${prerequis%%-*}=${lien_prerequis}" >> "${rep_transf}"/tmp/liste_liens_symboliques
        fi
    done
    
    if [ "${#liste_archive[@]}" -gt 0 ]
    then
        liste_archive+=('liste_liens_symboliques')
        tar -czhf "${arch_oreste_bin}" "${liste_archive[@]}"
        mv "${arch_oreste_bin}" "${rep_transf}"
    fi
    \rm -rf "${rep_transf}"/tmp
else
    echo "!! ERREUR, pas de repertoire d installation ORESTE : \$rep_inst_ores = ${rep_inst_ores}"
    exit 1
fi
echo

# Archivage du serveur Apache qui a ete indique lors de la compilation des
# differents produits avec entre autres l'option --with-httpd=/chemin/vers/httpd
# Il faut donc archiver son chemin en entier et non pas seulement le repertoire
# terminal du prefixe utilise lors de la compilation de Apache (--prefix=)
if [ -d "${rep_inst_ores_httpd}"/"${apache_rep}" ]
then
    echo "** Repertoire installation Apache Oreste"
    nettoie_elements
    echo " --> Phase archivage repertoire installation Apache ORESTE..."
    cd "${rep_inst_ores_httpd}"/"${apache_rep}"
    liste_elements=('bin' 'cgi-bin' 'error' 'icons' 'lib' 'man' 'modules' 'build' 'conf' 'htdocs' 'include' 'logs' 'manual' 'php')
    unset liste_archive
    for i in "${liste_elements[@]}"
    do
        if [ ! -e "$i" ]
        then
            echo "!! WARNING, manque  element $i logiquement installe avec Apache"
            echo "  => verifier bon deploiement Apache"
        else
            liste_archive+=("$i")
        fi
    done
    if [ "${#liste_archive[@]}" -gt 0 ]
    then
        cd "${rep_transf}"
        tar -czhf "${arch_oreste_httpd_bin}" "${rep_inst_ores_httpd}"/"${apache_rep}"
    fi
else
    echo "!! ERREUR, pas de repertoire d installation Apache :"
    echo "    \$rep_inst_ores_httpd = ${rep_inst_ores_httpd}"
    echo "    \$apache_rep = ${apache_rep}"
    exit 1
fi
echo

# Archivage du produit GlobalInsight
nom="Global Insight Plus"
if [ -d "${rep_inst_ores}"/GlobalInsight ]
then
    echo "** Repertoire installation ${nom}"
    echo "${rep_inst_ores}/GlobalInsight"
    cd "${rep_inst_ores}/GlobalInsight"
    nettoie_elements
    echo " --> Phase archivage repertoire installation ${nom}"
    liste_elements=('7.1')
    unset liste_archive
    for i in "${liste_elements[@]}"
    do
        if [ ! -e "$i" ]
        then
            echo "!! WARNING, manque  element $i logiquement installe avec ${nom}"
            echo "  => verifier bon deploiement ${nom}"
        else
            liste_archive+=("$i")
        fi
    done
    if [ "${#liste_archive[@]}" -gt 0 ]
    then
        tar -czhf "${arch_GlobalInsight}" "${liste_archive[@]}"
        mv "${arch_GlobalInsight}" "${rep_transf}"
    fi
else
    echo "!! WARNING, pas de repertoire d installation ${nom} : \$rep_inst_ores/GlobalInsight = ${rep_inst_ores}/GlobalInsight"
fi
echo

# Archivage du produit Infield Energy Gateway
nom="Infield Energy Gateway"
if [ -d "${rep_inst_ores}"/InfieldEnergyGateway ]
then
    echo "** Repertoire installation ${nom}"
    cd "${rep_inst_ores}"/InfieldEnergyGateway
    nettoie_elements
    echo " --> Phase archivage repertoire installation ${nom}"
    liste_elements=('platforms.dbf' 'platforms.prj' 'platforms.qpj' 'platforms.shp' 'platforms.shx')
    unset liste_archive
    for i in "${liste_elements[@]}"
    do
        if [ ! -e "$i" ]
        then
            echo "!! WARNING, manque  element $i logiquement installe avec ${nom}"
            echo "  => verifier bon deploiement ${nom}"
        else
            liste_archive+=("$i")
        fi
    done
    if [ "${#liste_archive[@]}" -gt 0 ]
    then
        tar -czhf "${arch_InfieldEnergyGateway}" "${liste_archive[@]}"
        mv "${arch_InfieldEnergyGateway}" "${rep_transf}"
    fi
else
    echo "!! WARNING, pas de repertoire d installation ${nom} : \$rep_inst_ores/InfieldEnergyGateway = ${rep_inst_ores}/InfieldEnergyGateway"
fi

# Verification que les archives ont bien ete creees
cd "${rep_transf}"
if [ -e "${arch_electra_bin}" ] && [ -e "${arch_oreste_bin}" ] && [ -e "${arch_oreste_httpd_bin}" ] && [ -e "${arch_GlobalInsight}" ]
then
    echo "** Creation archive ELECTRA et ORESTE OK"
else
    echo "!! ERREUR, probleme de creation archive ELECTRA (${arch_electra_bin}) et ORESTE (${arch_oreste_bin} ${arch_oreste_httpd_bin})"
    exit 1
fi

echo
echo "** Liste des rpm installes sur la machine"
rpm -qa >"${rep_transf}"/liste_rpm.txt

echo
echo "**********************************"
echo "Fin de la preparation du transfert"
echo "----------------------------------"
echo
