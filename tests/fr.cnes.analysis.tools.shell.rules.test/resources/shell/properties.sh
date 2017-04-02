# ***************************************************************************
# Specification des chemins installation/creation/positionnement des archives
# ***************************************************************************
# Positionnement archives initiales
export archives_dir=/root/electra_livraison/

#########################
# Configuration de l'installation via IHM
#########################
# installation type binaire ou source
export flag_binaries=1
# installation par defaut ou manuelle
export gen_defaut=0

# installation ELECTRA
export gen_electra=1

# installation ORESTE
export gen_oreste=1

# Sauvegarde des repertoires
export rep_sauvegarde=
export sauv_rep_electra=
export sauv_rep_oreste=
export sauv_rep_util=

export electra_mu=ELECT-MU-3300-0010-CG_03_05.pdf
# repertoires d'installation ELECTRA
export rep_inst_elec=/users/soft/electra/

# repertoires d'installation ORESTE
export rep_inst_ores=/users/soft/oreste/
export rep_inst_ores_httpd=/users/soft/oreste_httpd/
export rep_transf=/tmp/electra_transfert
# flag d'installation des pre-requis
export flag_lib_sys=1

# electra
export flag_openmpi=1
export rep_openmpi=
export flag_g95=1
export rep_g95=
export flag_PSIMU=1
export rep_PSIMU=
export db_compas_locale=/user/soft/electra/misc/share/data/BD_COMPAS_locale
export db_compas_ref=/users/soft/electra/misc/share/data/db_ref

# oreste
export flag_lib_oreste=1
export rep_lib_oreste=
export flag_httpd=1
export rep_inst_ores_httpd=/users/soft/oreste_httpd/
export flag_InfieldEnergyGateway=1
export rep_InfieldEnergyGateway=/users/soft/oreste/
export flag_GlobalInsight=1
export rep_GlobalInsight=/users/soft/oreste/

#########################
# parametrage env
#########################
# definition user group
export user_group=`id -gn electra`
export fichier_env=/users/soft/electra//env_electra

# Creation/compilation
export tmpdir=/tmp/electra_deploiment
export rpmdir=/tmp/electra_rpm

# elements de livraison
export version_electra=V3.1
export version_oreste=V1.6
export version_java=1_5_0_22-linux-i586
export fic_pdf=/opt/kde3/bin/kpdf
export fic_txt=/opt/kde3/bin/kate
export rep_transf=/tmp/electra_transfert
export arch_electra_bin=ELECTRA-installation-${version_electra}.tar.gz
export arch_oreste_bin=ORESTE-installation-${version_oreste}.tar.gz
export arch_oreste_httpd_bin=ORESTE-httpd-installation.tar.gz

# PRE-REQUIS
# Versions
export v_openmpi=1.4.3
export v_mage=2.0
export v_gcc=4.7.2
export v_mecaspa=5.0
export v_register=2.1.1
export v_gslib=7.0
export nom_export=export_LinuxV2.6.16.60_x86_64_gccV${v_gcc}_opt3_ieee
export v_psimu=10.0
export v_makemake=2.2
export v_genesis=9.0
export v_madona=5.0
export v_tcl_tk=8.4.19
export v_compas=3.0

# Archives
export arch_openmpi=openmpi-1.4.3.tar.gz
export arch_oreste_gz=Oreste_${version_oreste}.tar.gz
export arch_electra_gz=ELECTRA_${version_electra}.tar.gz
export arch_m4=m4-1.4.11.tar.gz
export arch_curl=curl-7.14.1.tar.gz
export arch_proj=proj-4.7.0.tar.gz
export arch_gd=gd-2.0.35.tar.gz
export arch_gdal=gdal-1.7.2.tar.gz
export arch_mapfish=MapFish-1.1.tar.gz
export arch_apache=httpd-2.2.15.tar.gz
export arch_php5=php-5.2.13.tar.gz
export arch_mapserver=mapserver-5.6.3.tar.gz
export arch_GlobalInsight=GlobalInsight.tar.gz
export arch_psimu=archPSIMU_10_Cap.tar.gz
export arch_g95=gcc-4.7.2.tar.gz
export arch_InfieldEnergyGateway=InfieldEnergyGateway.tar.gz

# Fin de la partie a mettre a jour
export mpi_rep=`basename $arch_openmpi .tar.gz`
export arch_electra=`basename $arch_electra_gz .gz`
export curl_rep=`basename $arch_curl .tar.gz`
export m4_rep=`basename $arch_m4 .tar.gz`
export proj_rep=`basename $arch_proj .tar.gz`
export gd_rep=`basename $arch_gd .tar.gz`
export gdal_rep=`basename $arch_gdal .tar.gz`
export mapfish_rep=`basename $arch_mapfish .tar.gz`
export apache_rep=`basename $arch_apache .tar.gz`
export php5_rep=`basename $arch_php5 .tar.gz`
export mapserver_rep=`basename $arch_mapserver .tar.gz`
export arch_oreste=`basename $arch_oreste_gz .tar.gz`

# ***************************************************************************
# utilisateur
# ***************************************************************************
export login=vacaress
export rep_user=/home/vacaress
# creer des racources sur le bureau de l'utilisateur.
export flag_raccourcis=o
# utiliser le repertoire tmp global sinon on en recree un.
export flag_utiliser_tmp=o

