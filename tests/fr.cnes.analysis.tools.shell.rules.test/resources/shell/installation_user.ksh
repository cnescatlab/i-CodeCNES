#!/bin/ksh
#*****************************************************************
#$<AM-V2.0>
# 
#$Type
#	PROG
#
#$Nom
#	installation_user.ksh
#
#$Projet
#	ELECTRA
#
#$Application
#   ELECTRA
#
#$Resume
#	"Installation et parametrage d'un compte utilisateur par defaut, appele par l'installeur java"
#
#$Auteur
#   CAPGEMINI
#
#$Description
#	Ce script shell sert a installer et parametrer un compte utilisateur.
#
#$version
#	$Id: installation_user.ksh 2263 2014-01-10 10:02:58Z mbeuvelo $
#
#$Historique
#    VERSION:3.0.1:FA-ID:666:27/12/2013:Installeurs automatiques : anomalie fichier env_psimu.sh
#
#    VERSION:3.0:DM-ID:323:10/10/2013:Procedures automatiques d installation ELECTRA et ORESTE
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

# ***************************************************************************
# Specification des chemins installation/creation/positionnement des archives
# ***************************************************************************
chemin="$(readlink -f "$(dirname "$0")")"
. "${chemin}"/properties.sh

# verification des chemins d'installation d'Electra et Oreste
if [ $gen_electra -eq 1 ] && [ ! -d $rep_inst_elec/ELECTRA/$version_electra ]
then
   echo "!! ERREUR Electra $version_electra n'est pas installe "
fi

if [ $gen_oreste -eq 1 ] && [ ! -d $rep_inst_ores/ORESTE/$version_oreste ]
then
   echo "!! ERREUR Oreste $version_oreste n'est pas installe "
fi

# groupe d'electra
user_group=`id -g electra`

# Partie pour les Icones bureau
if [ $flag_raccourcis=o ]
then
  if [ ! -e $rep_user/Desktop ]
  then
    mkdir $rep_user/Desktop
  fi

  if [ $gen_electra -eq 1 ]
  then
    cp -f $rep_inst_elec/ELECTRA/$version_electra/xpl/ELECTRA.Desktop $rep_user/Desktop/
    chmod 440 $rep_user/Desktop/ELECTRA.Desktop
    chown ${login}:${user_group} $rep_user/Desktop/ELECTRA.Desktop
  fi

  if [ $gen_oreste -eq 1 ]
  then
    cp -f $rep_inst_ores/ORESTE/$version_oreste/xpl/ORESTE.Desktop $rep_user/Desktop/
    chmod 440 $rep_user/Desktop/ORESTE.Desktop
    chown ${login}:${user_group} $rep_user/Desktop/ORESTE.Desktop
  fi
fi

# Partie arborescence d'utilisateur
if [ $gen_electra -eq 1 ]
then
    if [ -e $rep_user/electra ]
    then
        cp -r $rep_user/electra $rep_user/electra_before$version_electra
        chown ${login}:${user_group} $rep_user/electra_before$version_electra
        echo "WARNING !! Le repertoire $rep_user/electra est copie vers $rep_user/electra_before$version_electra"
    fi
    echo "Creation de l'arborescence utilisateur $rep_user/electra"
    rm -rf $rep_user/electra
    mkdir $rep_user/electra
    chown -R ${login}:${user_group} $rep_user/electra
    cd $rep_user/electra
    . $rep_inst_elec/env_electra
electra -u << EOF >  /tmp/installation_user_$$.log 2>&1
o
o
o
o
o
o
o
o
o
o
o
o
EOF

    cp /tmp/installation_user_$$.log "${chemin}"/installation_user.log 

    # Test si aucune erreur remontee dans le fichier de log
    if ! grep -q ERREUR "${chemin}"/installation_user.log
    then
        echo  "flag_installation_user=1" >> "${chemin}"/installation_user.log 
    else
        echo  "flag_installation_user=0" >> "${chemin}"/installation_user.log 
    fi

fi

# Test si l'utilisateur peut utiliser /tmp pour faire des calculs
if [ $gen_electra -eq 1 ]
then
    if [ ! $flag_utiliser_tmp!="o" ]
    then
        rep_csv=/tmp/oreste_${login}
    else
        rep_csv=$rep_user/electra/tmp
        echo "Pour les calculs, c'est le repertoire $rep_csv qui sera utilise"
    fi

    sed -i 's,REP_CSV=.*,REP_CSV="'${rep_csv}'",g' $rep_user/electra/electra.rc
    mkdir -p $rep_csv
    chown -R ${login}:${user_group} $rep_csv
    chown -R ${login}:${user_group} $rep_user/electra
    chmod g+s $rep_user/electra
    chmod g+rx $rep_user
fi

if [ $gen_oreste -eq 1 ]
then
  if [ ! -e $rep_user/electra/oreste/templates ]
  then
    mkdir -p  $rep_user/electra/oreste/templates
  fi
  cp $rep_inst_ores/ORESTE/$version_oreste/www/data/test/templates/template_english.odt $rep_user/electra/oreste/templates/.
  chown -R ${login}:${user_group} $rep_user/electra/oreste/templates
fi


# Partie pour informer Oreste du nouvel utilisateur
if [ $gen_oreste -eq 1 ]
then
  rm -f $rep_inst_ores/ORESTE/$version_oreste/www/data/$login
  cd  $rep_inst_ores/ORESTE/$version_oreste/www/data
  ln -s $rep_user/electra/oreste $login
  chown -h electra:${user_group} $login
fi

echo "Installation de l'utilisateur $login finie "

echo "Il manque la mise a jour du fichier users.rc"
echo "Par raison de securite cette mise a jour est manuelle"
echo "Veuillez editer et faire la mise a jour manuellement: "
echo "$rep_inst_elec/ELECTRA/$version_electra/fcf/users.rc"
echo ""
