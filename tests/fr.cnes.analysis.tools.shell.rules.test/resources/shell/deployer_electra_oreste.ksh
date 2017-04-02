#!/bin/ksh
#*****************************************************************
#$<AM-V2.0>
#
#$Type
#	PROG
#
#$Nom
#	deployer_electra_oreste.ksh
#
#$Projet
#	ELECTRA
#h
#$Application
#   ELECTRA
#
#$Resume
#	"Deploiement d ELECTRA et/ou ORESTE"
#
#$Auteur
#   CAPGEMINI
#
#$Description
#	Programme shell de deploiement des produits ELECTRA et ORESTE en 6 etapes :
#	1 - Installation d'ELECTRA,d'ORESTE et du serveur APACHE
#       2 - Installation de Global Insight Plus
#	3 - Installation des Plateformes Petrolieres
#	4 - Mise a jour de l installation
#	5 - Securisation de l'installation
#	6 - Demarrage du serveur Apache
#
#$version
#	$Id: deployer_electra_oreste.ksh 2496 2014-07-18 09:27:04Z mbeuvelo $
#
#$Historique
#    VERSION:3.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
#
#    VERSION:3.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.1:FA-ID:741:26/06/2014:Pas de mise a jour du chemin JAVA dans env_electra et env_oreste
#
#    VERSION:3.0.1:FA-ID:707:26/03/2014:Les lib systemes doivente etre installees en local si elles ne sont pas presentes sur la machine
#
#    VERSION:3.0.1:DM-ID:678:26/03/2014:Amelioration des installeurs
#
#    VERSION:3.0.1:FA-ID:666:27/12/2013:Installeurs automatiques : anomalie fichier env_psimu.sh
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

# ***************************************************************************
# Specification des chemins installation/creation/positionnement des archives
# ***************************************************************************
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# fichier de log recupere par l'IHM en sortie du script
export fichier_log="${chemin}"/deployer_electra_oreste.log

#Purge du fichier log avant de lancer les verifications
[ -e "${fichier_log}" ] && rm -f "${fichier_log}"

#Definition JAVA
export JAVA_HOME="${JAVA_HOME:-/usr/java/jdk1.5.0_22}"
export PATH="${JAVA_HOME}"/jre/bin:"${JAVA_HOME}"/bin:"${PATH}"


# ***********************************
# Traitement du cas particulier d'Apache
# ***********************************

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


# ******************************************************
# Installation d'ELECTRA, d'ORESTE et du serveur APACHE
# ******************************************************

echo "1/6 Installation d'ELECTRA, d'ORESTE et du serveur APACHE"
echo "---------------------------------------------------------"

# Cas deploiement ELECTRA
if [ "${gen_electra}" -eq 1 ]
then
    echo " -->Installation d'ELECTRA"

    if [ -e "${rep_inst_elec}" ] && [ "${rep_inst_elec}" != '/' ]
    then 
        rm -r "${rep_inst_elec}"
    fi

    mkdir -p "${rep_inst_elec}"

    liste_electra=('ELECTRA' 'misc' 'shells' 'env_electra' 'liste_liens_symboliques')
    tar -C "${rep_inst_elec}" -xzf "${archives_dir}"/"${arch_electra_bin}" "${liste_electra[@]}"

    if [ ! -e "${rep_inst_elec}"/ELECTRA/"${version_electra}"/fcf/electra_"${version_electra}".rc ]
    then
        echo "!! ERREUR : Probleme avec l'installation d'ELECTRA"
        echo "            Installation avortee"
        echo "flag_install_electra=0" >> "${fichier_log}"
        exit 1
    else
        echo "flag_install_electra=1" >> "${fichier_log}"
    fi

    # Utilisation d'une installation deja existante de OpenMPI
    . "${rep_inst_elec}"/liste_liens_symboliques
    if [ "${flag_openmpi}" != 1 ]
    then
        cd "${rep_inst_elec}"/misc
        if [ -n "${rep_openmpi}" ]
        then
            # Suppression de OpenMPI qui a ete deploye avec ELECTRA
            rm -r openmpi
            # Lien vers l'installation deja existante
            ln -s "${rep_openmpi}" openmpi
        fi
        echo "    * OpenMPI situe sous " ${rep_openmpi}
    else
        # Si OpenMPI etait un lien symbolique, on doit recreer l'arborescence entiere
        # et recreer le lien
        if [ -n "${symlink_openmpi}" ] && [ "${symlink_openmpi}" != "${rep_inst_ores}"/misc/openmpi ]
        then
            if [ ! -e "${symlink_openmpi}" ]
            then
                mkdir -p "$(dirname "${symlink_openmpi}")"
                mv "${rep_inst_elec}"/misc/openmpi "${symlink_openmpi}"
            else
                echo "!! WARNING : Une installation de OpenMPI est deja presente au meme endroit que dans l'archive:"
                echo "             => ${symlink_openmpi}"
                echo "             OpenMPI ne sera pas reinstalle"
                rm -r "${rep_inst_ores}"/misc/openmpi
            fi
            ln -s "${symlink_openmpi}" "${rep_inst_elec}"/misc/openmpi
        fi
        echo "    * OpenMPI situe sous " ${rep_inst_elec}/misc/openmpi
    fi


    # Branchement a un BIBMS deja existant
    if [ "${flag_PSIMU}" != 1 ]
    then
        cd "${rep_inst_elec}"/misc
        rep_BIBMS="$(dirname "${rep_PSIMU}")"
        liste_rep=('admin' 'ARCH' 'bin' 'COMPAS' 'GENESIS' 'GSLIB' )
        liste_rep+=('MADONA' 'MAGE' 'MECASPA' 'mod' 'MSLIB90')
        liste_rep+=('MSPRO' 'NAIF' 'NETCDF' 'noarch' 'PSIMU' 'share' 'TCL_TK' 'TESTS_PSIMU')

        for i in "${liste_rep[@]}"
        do
            rm -r "$i"
            ln -s "${rep_BIBMS}"/"$i" "$i"
        done
        export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/lib:${rep_inst_elec}/misc/lib:${rep_inst_elec}/misc/lib64"
        echo "    * PSIMU situe sous " ${rep_BIBMS}/PSIMU
    else
        echo "    * PSIMU situe sous " ${rep_inst_elec}/misc/PSIMU
    fi
    echo " -->Logiciel ELECTRA deploye sous ${rep_inst_elec}"

fi

# Cas deploiement oreste
if [ "${gen_oreste}" -eq 1 ]
then
    echo 
    echo " -->Installation d'ORESTE"

    if [ -e "${rep_inst_ores}" ] && [ "${rep_inst_ores}" != '/' ]
    then 
        rm -r "${rep_inst_ores}"
    fi
    mkdir -p "${rep_inst_ores}"

    tar -C "${rep_inst_ores}" -xzf "${archives_dir}"/"${arch_oreste_bin}" ORESTE/ env_oreste

    # Deploiement des prerequis seulement si demande
    liste_prerequis=('firefox-3' "${mapserver_rep}" "${mapfish_rep}" "${gdal_rep}" "${curl_rep}" "${gd_rep}" "${m4_rep}" "${proj_rep}")
    if [ "${flag_lib_oreste}" -eq 1 ]
    then
        echo " -->Installation des prerequis ORESTE"
        tar -C "${rep_inst_ores}" -xzf "${archives_dir}"/"${arch_oreste_bin}" outils liste_liens_symboliques
        echo
     
        # Si un pre-requis etait un lien symbolique, on doit recreer l'arborescence
        # en entier et recreer le lien
        # Pour chaque prerequis qui etait un lien symbolique, une variable 
        # symlink_${prerequis} est presente dans le fichier liste_liens_symboliques
        # Il faut donc passer par un eval pour dereferencer deux fois la valeur
        . "${rep_inst_ores}"/liste_liens_symboliques
        for prerequis in "${liste_prerequis[@]}"
        do
            nom_variable_lien="symlink_${prerequis%%-*}"
            if eval [ -n "\"\$${nom_variable_lien}\"" ]
            then
                if eval [ ! -e "\$${nom_variable_lien}" ]
                then
                    mkdir -p "$(eval dirname "\$${nom_variable_lien}")"
                    eval mv "${rep_inst_ores}"/outils/"${prerequis}" "\$${nom_variable_lien}"
                else
                    echo "!! WARNING : Une installation de ${prerequis} est deja presente au meme endroit que dans l'archive:"
                    eval echo "             => \$${nom_variable_lien}"
                    echo "             ${prerequis} ne sera pas reinstalle"
                    rm -r "${rep_inst_ores}"/outils/"${prerequis}"
                fi
                eval ln -s "\$${nom_variable_lien}" "${rep_inst_ores}"/outils/"${prerequis}"
            fi
        done
        echo "    * Lib pour Oreste situees sous ${rep_inst_ores}/outils"
    else
        mkdir -p "${rep_inst_ores}"/outils
        cd "${rep_inst_ores}"/outils
        for prerequis in "${liste_prerequis[@]}"
        do
            ln -s "${rep_lib_oreste}"/"${prerequis}" .
        done
        echo "    * Librairies pour Oreste situees sous " ${rep_lib_oreste}
    fi

    # Deploiement du serveur Apache HTTP seulement si demande
    if [ "${flag_httpd}" -eq 1 ]
    then
        echo 
        echo " -->Installation du serveur apache"

        # Certains chemins sont donnes en dur lors de la compilation de Apache
        # avec l'utilisation de l'option --prefix du configure.
        # L'installation d'Apache doit donc se faire en utilisant les memes chemins
        # que lors de la compilation.
        httpd=/"$(tar -tzf "${archives_dir}"/"${arch_oreste_httpd_bin}" '*/bin/httpd')"

        if [ "${httpd}" != '/' ]
        then
            # Verif presence d'une installation existante du serveur apache
            if [ -e "${httpd}" ]
            then
                echo "!! WARNING : Un serveur Apache est deja installe au meme endroit que celui contenu dans l'archive:"
                echo "             => ${httpd}"
                echo "             Apache ne sera pas reinstalle"
                echo "flag_install_oreste_httpd=0" >> "${fichier_log}"
            else
                tar -C / -xzf "${archives_dir}"/"${arch_oreste_httpd_bin}"
                echo "flag_install_oreste_httpd=1" >> "${fichier_log}"
                echo
            fi

            apache_rep="$(basename "$(readlink -f "$(dirname "${httpd}")"/..)")"
            rep_inst_ores_httpd="$(readlink -f "$(dirname "${httpd}")"/../..)"
        fi
        echo "    * Serveur apache situe sous " $rep_inst_ores_httpd
    fi

    echo " -->Logiciel ORESTE deploye sous ${rep_inst_ores}"
    echo " -->Serveur Apache situe sous ${rep_inst_ores_httpd}"
    echo  
fi

#*****************************************
# Installation GD et Infield Enegy Gateway
#*****************************************
echo 
echo "2/6 Installation de Global Insight Plus"
echo "---------------------------------------"

# Cas deploiement ORESTE
if [ "${gen_oreste}" -eq 1 ]
then
    # Creation de la variable d'option pour le shell xpl/install_oreste
    global='n'

    if [ "${flag_GlobalInsight}" -eq 1 ]
    then
        echo "  --> Installation Global Insight"
        rep_GlobalInsight="${rep_inst_ores}/GlobalInsight/"
        if [ -e "${rep_GlobalInsight}" ]
        then 
            rm -r "${rep_GlobalInsight}"
        fi
        mkdir -p "${rep_GlobalInsight}"
        cd "${rep_GlobalInsight}"

        if [ "${flag_binaries}" != 1 ]
        then
            path_arch_global="${archives_dir}"/pre_ORESTE/"${arch_GlobalInsight}"
        else
            path_arch_global="${archives_dir}"/"${arch_GlobalInsight}"
        fi

        if [ -e "${path_arch_global}" ]
        then
            tar -xzf "${path_arch_global}"
            if [ -d "${rep_GlobalInsight}"/7.1/ESRI ]
            then
                cd "${rep_GlobalInsight}"/7.1/ESRI
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                global='o'
                echo "   Global insight situee sous ${rep_GlobalInsight}"
            else
                echo "!! WARNING, mauvais contenu archive global insight : absence ${rep_GlobalInsight}. Installation avortee"
            fi
        else
            echo "!! WARNING, absence archive global insight : ${path_arch_global}. Installation avortee"
        fi
    else
        if [ -z "${rep_GlobalInsight}" ] || [ "${rep_GlobalInsight}" = "null" ]
        then
            echo "Global insight n'est pas installe"
        else
            if [ -d "${rep_GlobalInsight}" ]
            then
                echo "  --> Creation lien vers GlobalInsight"
                cd "${rep_inst_ores}"
                ln -s "${rep_GlobalInsight}" GlobalInsight
                cd "${rep_GlobalInsight}"/7.1/ESRI
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                # Creation de la variable d'option pour le shell xpl/install_oreste
                global='o'
                echo "   Global insight situee sous " ${rep_GlobalInsight}
            else
                echo "!! WARNING, mauvais contenu du repertoire global insight : absence "${rep_GlobalInsight}"/7.1/ESRI. Installation avortee"
            fi
        
        fi
    fi
else
    echo "!! WARNING : Non effectue car installation d'ORESTE non demandee"
    echo
fi

echo
echo "3/6 Installation des plateformes petrolieres"
echo "--------------------------------------------"

# Cas deploiement oreste
if [ "${gen_oreste}" -eq 1 ]
then
    # Creation de la variable d'option pour le shell xpl/install_oreste
    infield='n'

    if [ "${flag_InfieldEnergyGateway}" -eq 1 ]
    then
        echo "  --> Installation de la base de donnees Infield Energy Gateway"

        rep_InfieldEnergyGateway="${rep_inst_ores}/InfieldEnergyGateway"
        if [ -e "${rep_InfieldEnergyGateway}" ]
        then 
            rm -r "${rep_InfieldEnergyGateway}"
        fi
        mkdir -p "${rep_InfieldEnergyGateway}"
        cd "${rep_InfieldEnergyGateway}"

        if [ "${flag_binaries}" != 1 ]
        then
            path_arch_infield="${archives_dir}"/pre_ORESTE/"${arch_InfieldEnergyGateway}"
        else
            path_arch_infield="${archives_dir}"/"${arch_InfieldEnergyGateway}"
        fi

        if [ -e "${path_arch_infield}" ]
        then
            tar -xzf "${path_arch_infield}"           
            if [ -e "${rep_InfieldEnergyGateway}"/platforms.shp ]
            then
                for i in *.shp
                do
                    "${rep_inst_ores}"/outils/"${mapserver_rep}"/shptree "$i"  >> /tmp/prerequis_oreste_$$.log  2>&1
                done
                infield='o'
                echo "   InfieldEnergyGateway situee sous ${rep_InfieldEnergyGateway}"
            else
                echo "!! WARNING, mauvais contenu archive Infield energy Gateway : absence ${rep_InfieldEnergyGateway}/platforms.shp. Installation avortee"
            fi           
        else
            echo "!! WARNING, absence archive Infield Energy Gateway : ${path_arch_infield}. Installation avortee"
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
                rep_InfieldEnergyGateway="${rep_inst_ores}"/InfieldEnergyGateway
                infield='o'
                echo "   InfieldEnergyGateway situee sous " ${rep_InfieldEnergyGateway}
            else
                echo "!! WARNING, mauvais contenu du repertoire InfieldEnergyGateway : absence ${rep_InfieldEnergyGateway}. Installation avortee"
            fi
        fi
    fi
else
    echo "!! WARNING : Non effectue car installation d'ORESTE non demandee"
    echo
fi

#*******************************
# Mise a jour de l'installation
#*******************************

echo
echo "4/6 Mise a jour de l'installation"
echo "---------------------------------------"

# Cas deploiement electra
if [ "${gen_electra}" -eq 1 ]
then
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
    echo "${nom_machine} ${nb_proc}" > "${rep_inst_elec}"/ELECTRA/"${version_electra}"/fcf/machines.txt
fi

# Cas deploiement oreste
if [ "${gen_oreste}" -eq 1 ]
then
    sed -i -e 's/^Listen.*/Listen localhost:1036/g'  \
           -e 's/ :1036/:1036/g'                     \
           "${rep_inst_ores_httpd}"/"${apache_rep}"/conf/httpd.conf

    if [ "${global}" = n ]
    then
        if [ "${infield}" = n ]
        then
            ${rep_inst_ores}/ORESTE/${version_oreste}/xpl/install_oreste reconfigure >> /tmp/deploie_electra_oreste_$$.log 2>&1 << EOF
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
            ${rep_inst_ores}/ORESTE/${version_oreste}/xpl/install_oreste reconfigure >> /tmp/deploie_electra_oreste_$$.log 2>&1 << EOF
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
${rep_InfieldEnergyGateway}
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
        fi
    else
        if [ "${infield}" = n ]
        then
            ${rep_inst_ores}/ORESTE/${version_oreste}/xpl/install_oreste reconfigure >> /tmp/deploie_electra_oreste_$$.log 2>&1 << EOF
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
O
${rep_GlobalInsight}/7.1/ESRI
N
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
        else
            ${rep_inst_ores}/ORESTE/${version_oreste}/xpl/install_oreste reconfigure >> /tmp/deploie_electra_oreste_$$.log 2>&1 << EOF
${arch_oreste}.tar
${tmpdir}/test_oreste
${rep_inst_ores}/ORESTE/${version_oreste}
${rep_inst_elec}/ELECTRA/${version_electra}/fcf
${version_electra_sans_v}
${rep_inst_ores_httpd}/${apache_rep}
${rep_inst_ores}/outils/${mapserver_rep}
${rep_inst_ores}/outils/${mapfish_rep}
O
${rep_GlobalInsight}/7.1/ESRI
O
${rep_InfieldEnergyGateway}
${rep_inst_ores}/outils/${gdal_rep}
${rep_inst_ores}/outils/${gd_rep}
${rep_inst_ores}/outils/${proj_rep}
${rep_inst_ores}/outils/firefox-3/firefox


EOF
        fi
    fi

    sed -i -e 's/User daemon/User electra/g'         \
           -e "s/Group daemon/Group ${user_group}/g" \
           "${rep_inst_ores_httpd}"/"${apache_rep}"/conf/httpd.conf
fi

# Mise a jour du lien pour le plugin JAVA de Firefox
if [ ! -L "${rep_inst_ores}"/outils/firefox-3/plugins/libjavaplugin_oji.so ]
then
    if [ -e "${JAVA_HOME}"/jre/plugin/i386/ns7/libjavaplugin_oji.so ]
    then
        ln -sf "${JAVA_HOME}"/jre/plugin/i386/ns7/libjavaplugin_oji.so "${rep_inst_ores}"/outils/firefox-3/plugins/libjavaplugin_oji.so
    else
        echo "!! ERREUR, mauvais deploiement de ${JAVA_HOME}"
        echo "il manque le plugin ${JAVA_HOME}/jre/plugin/i386/ns7/libjavaplugin_oji.so"
        echo "Les graphes Oreste seront vides"
    fi
fi

# Remplacement des occurences des chemins utilises pour la generation
# par le chemin utilise pour l'installation
if [ "${gen_oreste}" = 1 ]
then
    old_rep_inst_ores="$(sed -n '/Exec=/ s@Exec=\(.*\)/ORESTE/.*@\1@p' "${rep_inst_ores}"/ORESTE/"${version_oreste}"/xpl/ORESTE.Desktop)"
    sed -i -e 's@\(export rep_electra=\).*@\1'"${rep_inst_elec}"'@' \
           -e 's@\(export rep_oreste=\).*@\1'"${rep_inst_ores}"'@' \
           -e 's@\(export rep_oreste_httpd=\).*@\1'"${rep_inst_ores_httpd}"'@' \
           "${rep_inst_ores}"/env_oreste 

    # Surcharge du PATH pour utiliser le JAVA defini sur la machine de deploiement
    # au cas ou il soit different du JAVA utilise sur la machine de generation
    if [ -n "${JAVA_HOME}" ] && ! fgrep -q "export PATH=${JAVA_HOME}/jre/bin:\$PATH" "${rep_inst_ores}"/env_oreste
    then
        echo "export PATH=${JAVA_HOME}/jre/bin:\$PATH" >> "${rep_inst_ores}"/env_oreste
    fi

    fgrep -RIl "${old_rep_inst_ores}" "${rep_inst_ores}"/ORESTE/"${version_oreste}" \
    | xargs sed -i -e "s@${old_rep_inst_ores}@${rep_inst_ores}/@g"
fi

if [ "${gen_electra}" = 1 ]
then
    old_rep_inst_elec="$(sed -n '/Exec=/ s@Exec=\(.*\)/ELECTRA/.*@\1@p' "${rep_inst_elec}"/ELECTRA/"${version_electra}"/xpl/ELECTRA.Desktop)"
    sed -i -e 's@\(export rep_electra=\).*@\1'"${rep_inst_elec}"'@' \
           -e 's@\(export rep_oreste=\).*@\1'"${rep_inst_ores}"'@' \
           -e 's@\(export rep_oreste_httpd=\).*@\1'"${rep_inst_ores_httpd}"'@' \
           "${rep_inst_elec}"/env_electra
    sed -i "s@${old_rep_inst_elec}@${rep_inst_elec}/@g" \
           "${rep_inst_elec}"/ELECTRA/"${version_electra}"/xpl/ELECTRA.Desktop \
           "${rep_inst_elec}"/ELECTRA/"${version_electra}"/xpl/start_electra \
           "${rep_inst_elec}"/ELECTRA/"${version_electra}"/fcf/electra_"${version_electra}".rc \
           "${rep_inst_elec}"/ELECTRA/"${version_electra}"/xpl/electra

    # Surcharge du PATH pour utiliser le JAVA defini sur la machine de deploiement
    # au cas ou il soit different du JAVA utilise sur la machine de generation
    if [ -n "${JAVA_HOME}" ] && ! fgrep -q "export PATH=${JAVA_HOME}/jre/bin:\$PATH" "${rep_inst_elec}"/env_electra
    then
        echo "export PATH=${JAVA_HOME}/jre/bin:\$PATH" >> "${rep_inst_elec}"/env_electra
    fi

    old_rep_inst_elec="$(sed -n '/GENESIS_DIR=/ s@.* GENESIS_DIR=\(.*\)/misc/.*@\1@p' "${rep_inst_elec}"/misc/bin/env_psimu.sh)"
    fgrep -RIl "${old_rep_inst_elec}" "$(dirname "${rep_PSIMU}")" \
    | fgrep -v openmpi \
    | xargs sed -i -e "s@${old_rep_inst_elec}@${rep_inst_elec}/@g"
fi

# FA 666 : Installeurs automatiques: anomalie dans le fichier env_psimu.sh
# Dans le cas d'un deploiement binaire, il faut sourcer le fichier
# GenesisInitVarEnvExe.sh et non pas GenesisInitVarEnv.sh car il ne verifie pas
# la presence de makedepend sur la machine
sed -i 's@GenesisInitVarEnv.sh@GenesisInitVarEnvExe.sh@' "${rep_inst_elec}"/misc/bin/env_psimu.sh

#*******************************
# Securisation de l'installation
#*******************************
echo
echo "5/6 Securisation de l'installation (finalisation ) "
echo "---------------------------------------------------"

#
# Securisation des repertoires
#
# Cas deploiement electra
if [ "${gen_electra}" -eq 1 ]
then
    chown -R electra:"${user_group}" "${rep_inst_elec}"
    chmod -R 750 "${rep_inst_elec}"
fi
# Cas deploiement oreste
if [ "${gen_oreste}" -eq 1 ]
then
    chown -R electra:"${user_group}" "${rep_inst_ores}"
    chmod -R 750 "${rep_inst_ores}"
    if [ "${flag_httpd}" -eq 1 ]
    then
        chown -R electra:"${user_group}" "${rep_inst_ores_httpd}"/"${apache_rep}"/
        chmod -R 700 "${rep_inst_ores_httpd}"/"${apache_rep}"/
        chown -R root:"${user_group}" "${rep_inst_ores_httpd}"/"${apache_rep}"/bin \
                                      "${rep_inst_ores_httpd}"/"${apache_rep}"/conf
        chmod -R 750 "${rep_inst_ores_httpd}"/"${apache_rep}"/bin \
                     "${rep_inst_ores_httpd}"/"${apache_rep}"/conf
    fi
fi

echo


#****************************
# Demarrage du serveur Apache
#****************************
echo
echo "6/6 (Re-)Demarrage du serveur Apache  "
echo "---------------------------------"

# Cas deploiement oreste
if [ "${gen_oreste}" -eq 1 ]
then
    export LD_LIBRARY_PATH=".:/lib64:${rep_inst_ores}/outils/gdal-1.7.2/lib:/usr/X11R6/lib64:/usr/lib64/outils/gdal-1.7.2/lib:${LD_LIBRARY_PATH}"
    # Arret de Apache s'il est demarre
    if pgrep httpd >/dev/null
    then
        "${rep_inst_ores_httpd}"/"${apache_rep}"/bin/httpd -k stop >> /tmp/deploie_electra_oreste_$$.log  2>&1
        sleep 3
        killall httpd >> /tmp/deploie_electra_oreste_$$.log  2>&1
        sleep 3
    fi
    "${rep_inst_ores_httpd}"/"${apache_rep}"/bin/httpd -k start >> /tmp/deploie_electra_oreste_$$.log  2>&1
else
    echo "!!WARNING : Non effectue car installation d'ORESTE non demandee"
    echo
fi

