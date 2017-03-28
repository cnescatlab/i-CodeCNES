#!/bin/bash
set -

###############################################################
# sauve.sh
VERSION="5.0"

### Information général
# Script de sauvegarde système
# IL se base sur un fichier de configuration qui indique qui, quoi et ou sauvegarder
# Les paramètres a indiquer au script paramètres le comportement le comment sauvegarder
#
# Dans ce script 3 rôles important sont dissociés.
# Ces 3 rôles peuvent être pourvue par 1, 2 ou 3 machines différentes.
# MACHINE_EXE => machine qui exécute le script
# MACHINE_S => machine source est la machine a sauvegarder
# MACHINE_D => machine destination est la machine qui a le périphérique de destination

# Script de gestion des sauvegardes.
# Voir le manuel d'installation pour ce qui concerne l'installation,
# la configuration et l'utilisation de ce script.

### historique
# 5.0 11/10/2012 : SBECUE : Réécriture complète du code
# 5.0.1 31/01/2014 : P. CASENOVE : Correction bugs:
# 	- si on ne demande pas de MAIL,  les fichiers de logs ne sont pas renomméen _OK ou _NOK
# 	- La variable TAPE_HOST est mal analysé: bug si le caractere @ est absent

### TODO ######
##  1- ajouter l'ajout automatique de FS voir la fonction Pre_Traitement_Sources
##  2- les fichiers de log $MTX_LOG_FILE $ERROR_FILE $MACH_ERROR devrait être supprrime avec adaptation du code
##  3- le mail contient des information dite binaire, iconv regle le pb sur linux,
#  mais ne fonctionne pas sur Solaris. Trouver d'ou viens cette information binaire
##  4- bug dans la fonction "Sauvegarde_Informations_Systeme" prtvtoc ne fonctione pas.
#  le bug devait exister en version 3.20 le peripherique teste est /dev/rdsk/s2
#  Dans la même fonction dvrais ce base sur les tableau de partition au leux de refaire un df sur la source
##  5- Si ctrl+c ou autre signaux le nettoyage n'est pas net, pour resoudre ce probleme, il faudrait deplacer tous le code apres la sauvegarde das le sortir.
#   Une adaptation du code est a prevoir. Il est necesaire de metre des condition d'execution dans la fonction sortir pour executer ces parties de codes
##  6-

# PATH pour la commande ssh
PATH=$PATH:/usr/local/bin
export PATH

####################################################
## Options d'exécution
# 2 variables peuvent être exporté avant d'exécuter le script
# Elles vous permettrons de debuguer le script.
# Le mode debug active set -x sur tous le code fonction incluses
# le mode verbeux affiche des messages envoyer par la fonction Info ayant l'option -v
[ -z "$VERBOSE" ] && VERBOSE=0
[ -z "$DEBUG" ] && DEBUG=0
export DEBUG
export VERBOSE
[ "$DEBUG" = "1" ] && set -x
[ "$VEBOSE" = "1" ] && echo "# Verbose: Le mode verbeux est actif"


###############################################################
## LES FONCTIONS                                             ##
###############################################################

###### Usage
# Affiche l'aide d'utilisation du script pour les problèmes de syntaxe
###############################################################
Usage() {
  echo " "
  echo "Usage :"
  echo "     $0 [xxx|all|eject|test] [0-9] [erase|eom|taille] [media|box|changer] {1-$MAX_BANDE}"
  echo " "
  echo " * arg 1"
  echo "   xxx    : sauvegarde la liste LST_SAUVE_XXX correspondante declaree dans"
  echo "            config_save. La liste LST_SAUVE_SYS ET LST_SAUVE_USER devant"
  echo "            etre obligatoirement presente dans le fichier de configuration."
  echo "   all    : sauvegarde la liste LST_SAUVE_ALL du fichier config_save,"
  echo "            si cette liste n'existe pas, la concatenation de LST_SAUVE_SYS"
  echo "            et de LST_SAUVE_USER est effectuee."
  echo "   eject  : N'effectue pas de sauvegarde, ejecte simplement la bande."
  echo "   test   : Teste la presense d'une bande dans le lecteur."
  echo " "
  echo " * arg 2"
  echo "   Niveau de la sauvegarde."
  echo " "
  echo " * arg 3"
  echo "   erase  : effectue la sauvegarde en debut de bande en l'ecrasant."
  echo "   eom    : effectue la sauvegarde a la suite de la derniere sauvegarde se"
  echo "            trouvant sur la bande."
  echo "   taille : estime la taille de la sauvegarde sans la realiser."
  echo " "
  echo " * arg 4"
  echo "   media  : gestion des numeros de media interne (1-$MAX_BANDE) au robot avec mtx."
  echo "            Pas de gestion de sauvegarde multivolumes."
  echo "   box    : pas de gestion de bande avec mtx. Les sauvegardes s'effectuent"
  echo "            sur les bandes les unes a la suite des autres en multivolumes."
  echo "   changer: gestion des numeros de media interne (1-$MAX_BANDE) au robot avec mtx."
  echo "            Les sauvegardes s'effectuent sur les bandes les unes a la"
  echo "            suite des autres en multivolumes."
  echo " "
  echo " * arg 5"
  echo "   Numero de bande du robot sur laquelle la sauvegarde devra commencer."
  echo "   S'il n'est pas present, la sauvegarde sera effectuee sur la bande inseree."
  echo "   Pour un simple lecteur, ce numero ne doit pas etre specifie."
  echo "   Si ce numero est 0, la bande suivante sera chargee."
  echo " "
exit 1
}

###### Info
## Permet un formatage cohérent de message
## Le message est envoyé sur la sortie standard + dans
## le fichier de log
## le répertoire contenant le fichier de log est créé si abs
## Le premier paramètre est l'option, le reste le texte devant être logué
## Les options valides sont -i -o -n -a -d -v -e -t -m
## -v -d Verbose et Debug n'affichent rien si la variable correspondante n'est pas valorisée à 1
## VERBOSE ou DEBUG
###############################################################
Info () {
[ "$DEBUG" = "1" ] && set -x
  # Variable locales
  typeset PREFIX

  # Choix des options
  case "$1" in
    -i) # Information : le message commence par "# Info:"
      PREFIX="# Info:" ; shift
    ;;
    -o) # OK : Le message commence par "# OK:"
      PREFIX="# OK:" ; shift
    ;;
    -n) # NOK : Le message commence par "# NOK:"
      PREFIX="# NOK:" ; shift
    ;;
    -a) # Alert : Le message commence par "# Alert:"
      PREFIX="# Alert:" ; shift
    ;;
    -d) # Debug: Le message commence par "# Debug:" et n'est affiche qu'en mode debug
      [ "$DEBUG" != "1" ] && return 0
      PREFIX="# Debug:" ; shift
    ;;
    -v)  # Verbose: Le message commence par "# Verbose:"  et n'est affiche qu'en mode Verbeux
      [ "$VERBOSE" != "1" ] && return 0
      PREFIX="# Verbose:" ; shift
    ;;
    -e) # Erreur : le message commence par "# Erreur:"
      PREFIX="# Erreur:" ; shift
    ;;
    -t) # ecrit dans le fichier qui sera l'en-tete du fichier de LOG
      shift
      if [ -n "${ENTETE_FILE}" ] ; then
        # Creer le repertoire hebergeant le fichier d'entête si il n'existe pas
        DIR_ENTETE_F=`dirname ${ENTETE_FILE}`
        if [ ! -d "$DIR_ENTETE_F" ] ; then
          mkdir -p ${DIR_ENTETE_F}
          [ "$?" -ne 0 ] && echo -e "# Exception:Impossible de creer le repertoire d'entete $DIR_ENTETE_F" && exit 10
        fi

        # Copie de la ligne dans le fichier de log dans le fichier d'entete
        echo -e "$@" | tee -a $ENTETE_FILE
        [ "$?" -ne 0 ] && echo -e "Exception:Impossible d'ecrire dans le fichier de fichier ${ENTETE_FILE}"
      else
        echo -e "$@"
      fi
      # sort de la fonction pour ne pas écrire aussi dans le fichier de log
      return 0
    ;;
    -m) # écrit dans le fichier qui servira de corps au mail
      shift
      if [ -n "${MAIL_CORP}" ] ; then
        # Créer le répertoire hébergeant le fichier de mail
        DIR_MAIL_CORP=`dirname ${MAIL_CORP}`
        if [ ! -d "$DIR_MAIL_CORP" ] ; then
          mkdir -p ${DIR_MAIL_CORP}
          [ "$?" -ne 0 ] && echo -e "# Exception:Impossible de creer le repertoire $DIR_MAIL_CORP" && exit 11
        fi
        # On copie la ligne dans le fichier de mail
        echo -e "$@" | tee -a $MAIL_CORP
        [ "$?" -ne 0 ] && echo -e "Exception:Impossible d'ecrire dans le fichier de fichier ${MAIL_CORP}"
      else
        echo -e "$@"
      fi
      # Les lignes seront aussi copier dans le fichier de LOG avec "# Mail:" comme prefix
      PREFIX="# Mail:"
    ;;
  esac

 ## option par defaut
  if [ -z "$PREFIX" ] ; then
    PREFIX="# Info:";
  fi

 # log du message dans un fichier si les variables existent
  typeset String=$@
  if [ -n "${LOG_FILE}" ] ; then
    # Créer le répertoire hébergeant le fichier de log si il n'existe pas
    typeset DIR_LOG=`dirname ${LOG_FILE}`
    if [ ! -d "$DIR_LOG" ] ; then
      mkdir -p ${DIR_LOG}
      [ "$?" -ne 0 ] && echo -e "# Exception:Impossible de creer le repertoire de log $DIR_LOG" && exit 12
    fi

    # On copie la ligne dans le fichier de log dans le fichier de log
    echo -e "${PREFIX}${String} #" >> ${LOG_FILE}
    [ "$?" -ne 0 ] && echo -e "Exception:Impossible d'ecrire dans le fichier de log ${LOG_FILE}"
  fi

  # Affichage du message a l'ecran.
  echo -e "${PREFIX}${String} #"
  return 0
}

###### Sortir
# DEPENDANCES : Info -e Info
# Usage : Sortir
# Description :A utiliser a la place de exit
# s'utilise de la même manière, mais permet d'effectuer des taches avant l'arret de l'execution
# ex: Supprimer un verrou
###############################################################
Sortir () {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Sortir $*"

  # Variable local
  typeset CODE_EXIT
  typeset MESSAGE

  # sauvegarde du code de sortie
  CODE_EXIT=$1
  shift
  # Il est possible de mettre un message en paramètre après le code de sortie
  # Mais attention il sera indique en tant que message de type Info (-i)
  MESSAGE=$*

  ### Taches de maintenances
  ## Suppression des Verrous
  # permet de sortir du répertoire temporaire
  cd

  # vérifie la présence d'un fichier verrou
  # Attention : si vous utilisez cette variable pour la gestion du verrou
  # il ne faut pas exécuter la fonction Sortir si vous teste l'exécution du verrou et qu'un verrou est présent si non il est supprimer
  if [ -n "$VERROU" ] ; then
    if [ -f "$VERROU" ] ; then
      rm -f $VERROU
      typeset RETOUR=$?
      # vérifie que la suppression c'est bien passé
      if [ "${RETOUR}" -ne 0 ] ; then
        Info -e "Impossible de supprimer le verrou : $VERROU"
        CODE_EXIT=20
      fi
    fi
  fi

  # Imprime le tableau d'erreur
  [ ${#TAB_ERREUR[$@]} -gt 0 ] && Affiche_Tableau_Erreur  && echo "TAB_ERREUR : ${#TAB_ERREUR[$@]}"
  #~ [ ${#TAB_ERREUR[$@]} -gt 0 ] && Affiche_Tableau
  #~ Affiche_Tableau_Erreur
  #~ echo "TAB_ERREUR : ${#TAB_ERREUR[$@]}"
  #~ Affiche_Tableaux

  # Sort avec le code de sortie d'origine
  # Si il est différent de 0 On le fait remarqué
  if [ "${CODE_EXIT}" -gt 0 ] ; then
    [ -n "${MESSAGE}" ] && Info -e ${MESSAGE}
    Info -e "Code de sortie -${CODE_EXIT}-"
  else
    # Affiche le message mis en paramètre après le code de sortie
    # le message est du type Info
    [ -n "${MESSAGE}" ] && Info -i ${MESSAGE}
  fi

  # Si la sortie est différente de 0 le mail doit être KO
  if [ "${CODE_EXIT}" -gt 0 ] ; then
    # Si le code de retour est différent de 0 le mail ne doit par être OK
    ERROR="Nok"
    # Si on veux faire d'autre chose quand le processus à reçu un message
    if [ "${CODE_EXIT}" -eq "4" ] ; then
      ERROR="Nok"
    fi
  fi


  Entete_Completion
  echo "##############################################################" | tee -a $ENTETE_FILE
  cat $LOG_FILE >> $ENTETE_FILE
  mv $ENTETE_FILE $LOG_FILE

  ## Envoie de mail
  #[ -n "$DEST_EMAIL" ] && Envoie_Mail
  Envoie_Mail

  #nettoie et compresse les logs
  #~ Nettoyage_Logs

  exit ${CODE_EXIT}

}

###### Uniforme_Commandes
## Crée des variables et définit les commandes binaires suivant l'OS
## Des Variables doivent être utilisées à la place des commandes
## Elles seront exécuter correctement en fonction de l'OS
## Cependant elles ne sont valables que pour les commandes locales
## si les commandes sont distantes, l'OS peut être différent.
###############################################################
Uniforme_Commandes() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Unifome_Commandes $*"

  # Commandes commune ou par defauts
  PATH=/sbin:/usr/sbin:/etc:/usr/etc:/bin:/usr/bin:$PATH
  export LANG=C
  export PATH LANG
  export AWK=/bin/awk
  export CAT=/bin/cat
  export DF=/bin/df
  export ECHO=/bin/echo
  export EGREP=/bin/egrep
  export GREP=/bin/grep
  export RM=/bin/rm
  export SED=/bin/sed
  export SORT=/bin/sort
  export TR=/usr/bin/tr
  GZIP="`type -p gzip`"

  # Chargement des commande spécifique a l'OS
  case `/bin/uname -s` in
     # Spécifique à Linux
    "Linux")
      export _XPG4GREP="/bin/egrep"
      export PING=/bin/ping
      export PING_OPTIONS="-c 3"
      export MAILER=/bin/mail
      export MOUNT=/bin/mount
      export UMOUNT=/bin/umount
      export OPT_DF="-Pk"
      # Contrôle la présence de gzip en local ou utilise la commande gzip embarquée avec l'outil
      if [ -z "${GZIP}" ] ;then
        GZIP="${BASE_DIR}/BIN/gzip.linux"
      fi
    ;;
    # Spécifique à Solaris
    "SunOS")
      export _XPG4GREP="/bin/egrep"
      export PING=/usr/sbin/ping
      export PING_OPTIONS=""
      export MAILER="/bin/mailx"
      export MOUNT=/sbin/mount
      export UMOUNT=/sbin/umount
      export AWK=/bin/nawk
      export OPT_DF="-k"
      export PATH=/usr/opt/SUNWmd/sbin:$PATH
      # Contrôle la présence de gzip en local ou utilise la commande gzip embarquée avec l'outil
      if [ -z "${GZIP}" ] ;then
         if [ -x /usr/local/bin/gzip ] ; then
            GZIP="/usr/local/bin/gzip"
         else
            GZIP="${BASE_DIR}/BIN/gzip.sparc"
         fi
      fi
    ;;
  esac
}

###### Cherche_Repertoire_Base
## Recherche le répertoire de base du script
## BASE_DIR contient le chemin absolue du répertoire père du script $0 #
## BASE_NAME contient le nom du script
###############################################################
Cherche_Repertoire_Base() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Cherche_Repertoire_Base $*"

  # Variable local
  typeset REP_ORG

  BASE_DIR=`dirname $0`
  BASE_NAME=`basename $0`

  # sauvegarde le répertoire actuel
  REP_ORG=`pwd`

  cd $BASE_DIR
  BASE_DIR=`pwd`

  # le script est dans ./BIN ont veux que BASE_DIR soit dans le répertoire père
  BASE_DIR=`echo $BASE_DIR | sed -e 's^/BIN$^^'`

  # retourne dans le répertoire dans lequel il était
  cd $REP_ORG

  Info -v "BASE_DIR = ${BASE_DIR}"
  Info -v "BASE_NAME = ${BASE_NAME}"
}

###### Controle_Charge_Config
## Vérifie le contenu minimal du fichier de configuration
## Récupère le type de sauvegarde en paramètre par $1
## Supprime les fichiers de logs portant la meme date, la    ##
## même heure, le même type et le même niveau.
## Attend 2 paramètres, $1 et $2         ##
###############################################################
Controle_Charge_Config() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Charge_Config $*"

  # Variable locale
  typeset L

  #########################################################
  ### Parametre du script ###
  # Crer des variables pour chaque paramètre mise a l'exécution du script
  export TYPE_ACTION=$1
  export NIVEAU_SAUVEGARDE=$2
  export TYPE_SAUVEGARDE=$3
  export GESTION_MEDIA=$4
  export NUMERO_BANDE=$5

  #########################################################
  ### Variables globales systèmes ####
  # La date d'exécution
  export DATE_EXE=`date '+%Y%m%d_%H%M%S'`
  # Le nom de la machine sans le domaine
  export MACHINE_EXE=`/bin/uname -n | cut -f1 -d.`
  # Contient le nom de l'OS qui exécute le script (Linux, SunOS, etc...)
  export NOM_OS_EXE=`/bin/uname -s`
  export VERSION_OS_EXE=`/bin/uname -r`

  #########################################################
  ### Chargement du fichier de conf ###
  # Contrôle l'existence du fichier et qu'il ne soit pas vide
  if [ -s "${BASE_DIR}/ETC/config_save" ] ; then
    # Vérifie qu'il existe au moins les listes SYS et USER
    for L in LST_SAUVE_SYS LST_SAUVE_USER ; do
      Info -v "L = $L"

      if [ -z "`/bin/grep '^'$L ${BASE_DIR}/ETC/config_save`" ] ; then
        echo "Il n'existe pas de section $L dans ${BASE_DIR}/ETC/config_save"
        exit 16
      fi
    done

    # Conversion du type en majuscule
    TYPE_ACTION_MAJ="`echo $TYPE_ACTION | tr '[:lower:]' '[:upper:]'`"
    Info -v "TYPE_ACTION_MAJ = $TYPE_ACTION_MAJ"

    # Si ni "eject", ni "test", vérifie que le fichier de config contient les informations du type demandées
    if [ "$TYPE_ACTION_MAJ" != "EJECT" ] && [ "$TYPE_ACTION_MAJ" != "TEST" ] ; then
      if [ -z "`/bin/grep '^LST_SAUVE_'$TYPE_ACTION_MAJ'=\".$' ${BASE_DIR}/ETC/config_save`" ] ; then
        echo "La liste LST_SAUVE_$TYPE_ACTION_MAJ n'apparait pas dans ${BASE_DIR}/ETC/config_save"
        exit 16
      fi
    fi
  else
    echo "Le fichier de configuration ${BASE_DIR}/ETC/config_save est introuvable"
    exit 17
  fi

  # Chargement du fichier de configuration
  # Ce fichier contient des variables valorisées ou non
  . ${BASE_DIR}/ETC/config_save

  #########################################################
  ### Compare la version du script et la version minimum atendu indiqué dans le fichier de configuration
  if [ -z $VERSION_SCRIPT_MIN ] ; then
    echo "Le fichier de configuration : ${BASE_DIR}/ETC/config_save"
      echo "n'est pas compatible avec cette version de script"
      echo "Veuillez l'adapter et indiquer la variable VERSION_SCRIPT_MIN"
      echo "Version du script : $VERSION"
      echo "VERSION_SCRIPT_MIN = $VERSION"
      exit 23
  else
    # Bash ne comprend pas nombre réel on demande a bc de le faire
    if [ `echo "$VERSION:$VERSION_SCRIPT_MIN" | awk -F: ' $1 <= $2 {print "1"}'` -ne 1 ] ; then
      echo "Le fichier de configuration : ${BASE_DIR}/ETC/config_save"
      echo "n'est pas compatible avec cette version de script"
      echo "Veuillez l'adapter"
      echo "Version du script : $VERSION"
      echo "Version minimum attendu : $VERSION_SCRIPT_MIN"
      exit 23
    fi
  fi


  #########################################################
  ###  DESTINATION
  ## Defini le type
  #  Variables globales construites depuis les variables du fichier de CONF
  # contient le type de destination 1er champ avec : comme diviseur
  TYPE_DESTINATION="`echo $TYPE_DEV | cut -d: -f1 | tr '[:upper:]' '[:lower:]'`"
  # Par défaut le type de sauvegarde est sur disque
  [ -z "$TYPE_DESTINATION" ] && TYPE_DESTINATION="disk"

  ##  Valeur par defaut de la machine de destination
  # Si vide, Par défaut le TAPE_HOST contient le nom de la machine local
  [ -z "$TAPE_HOST" ] && TAPE_HOST=$MACHINE_EXE

  # Si la valeur de TAPE_HOST contient un "@", c'est que le nom d'utilisateur de connexion est présent
  # le récupère le nom de connexion
  #if [ `echo "$TAPE_HOST"| grep @` ] ; then
  if [  `echo "$TAPE_HOST"| grep  -c  @` -ne 0 ] ; then
    NOM_UTILISATEUR_D=`echo $TAPE_HOST | cut -d@ -f1`
    MACHINE_D=`echo $TAPE_HOST | cut -d@ -f2`
  else
    NOM_UTILISATEUR_D=""
    MACHINE_D=$TAPE_HOST
  fi

  # Si le nom indiqué est localhost cela complique les tests devant etre effectuer par la suite
  # En récupèrent le vrai nom de machine, les futures tests sont plus simple a écrire et relire.
  [ "$MACHINE_D" = "localhost" ] && $MACHINE_D=$MACHINE_EXE


  ######################################################################
  ## PROCOLE DE DESTINATION
  ## Découpe le protocole et le sens du flux de sauvegarde
  # si les 3 machines sont distantes, le flux de source repasse par la machine qui exécute ou vas directement a la machine destination ?
  # Les valeurs sont transformé en minuscule, cela permet de ne pas avoir de problème de case
  # tr '[:upper:]' '[:lower:]'
  # Il est possible de mettre plusieurs informations dans le champ 1, séparées par des "," ex: PROTO_DEV=ssh,-p 2222,-qx:indirecte

  ## Par default le protocole est ssh et le sens et indirecte
  [ -z "$PROTO_DEV" ] && PROTO_DEV="ssh:indirecte"
  PROTOCOLE_D=`echo $PROTO_DEV | cut -d: -f1`
  ARGUMENT_PROTOCOLE_D=`echo $PROTOCOLE_D | cut -d, -f2-`

  # Si il n'y a pas de , et donc pas d'argument le protocole sera une valeur dans les arguments
  if [ "$ARGUMENT_PROTOCOLE_D" = "$PROTOCOLE_D" ] ; then
    ARGUMENT_PROTOCOLE_D=""
  else
    ARGUMENT_PROTOCOLE_D=`echo $ARGUMENT_PROTOCOLE_D | sed 's/,/ /g'`
  fi

  PROTOCOLE_D=`echo $PROTOCOLE_D | cut -d, -f1 | tr '[:upper:]' '[:lower:]'`
  SENS_PROTOCOLE_D=`echo $PROTO_DEV | awk -F: '{print $2}' | tr '[:upper:]' '[:lower:]'`

  Info -v "SENS_PROTOCOLE_D = $SENS_PROTOCOLE_D"
  Info -v "PROTOCOLE_D = $PROTOCOLE_D"
  Info -v "ARGUMENT_PROTOCOLE_D = $ARGUMENT_PROTOCOLE_D"


  #########################################################
  ### Fichier /.plan
  # Contrôle l'existence du fichier .plan
  if [ ! -f /.plan ] ; then
    echo "Le fichier /.plan sur $SERVER n'existe pas..."
    exit 18
  else
    # Récupération du bâtiment et de la plate-forme du .plan
    PLATEFORME=`/bin/cat /.plan | /bin/grep PLATE-FORME | awk '{print $3}' | sed -e 's,/,_,g'`
  fi

  #########################################################
  ### LOG ###
  ## Construit et Charge les variables de LOGS
  [ ! -d "${BASE_DIR}/LOGS" ] && mkdir -p -m 750 "${BASE_DIR}/LOGS"
  LOGS_DIR="${BASE_DIR}/LOGS"
  ENTETE_FILE="${LOGS_DIR}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_ENTETE"
  LOG_FILE="${LOGS_DIR}/SAVE_${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}"
  MTX_LOG_FILE="/tmp/mtx_log.${DATE_EXE}"
  ERROR_FILE="/tmp/save_errors.${DATE_EXE}"
  MACH_ERROR="/tmp/mach_errors.${DATE_EXE}"
  TMP_LOGS="/tmp/messages_tmp.$0.${DATE_EXE}"

  # Nettoyages d'anciennes traces de logue ne devant pas exister
  #~ Nettoyage_Logs

  ##### A partir d'ICI on peux utiliser les fonctions Info et Sortir ####

  #########################################################
  ### Environnement
  # Affiche l'environnement dans les logs
  #~ Info -v "#################### ENV ####################################"
  #~ env | while read ligne ; do
          #~ Info -v "$ligne"
        #~ done
  #~ Info -v "############################################################"

  Info -v "DATE_EXE = $DATE_EXE"
  Info -v "MACHINE_EXE = $MACHINE_EXE"
  Info -v "NOM_OS_EXE = $NOM_OS_EXE"
  Info -v "VERSION_OS_EXE = $VERSION_OS_EXE"

  Info -v "TYPE_ACTION = $TYPE_ACTION"
  Info -v "NIVEAU_SAUVEGARDE = $NIVEAU_SAUVEGARDE"
  Info -v "TYPE_SAUVEGARDE = $TYPE_SAUVEGARDE"
  Info -v "GESTION_MEDIA = $GESTION_MEDIA"
  Info -v "NUMERO_BANDE = $NUMERO_BANDE"

  Info -v "$TYPE_DEV = $TYPE_DEV"
  Info -v "TYPE_DESTINATION = $TYPE_DESTINATION"
  Info -v "TAPE_HOST = $TAPE_HOST"
  Info -v "MACHINE_D = $MACHINE_D"
  Info -v "NOM_UTILISATEUR_D = $NOM_UTILISATEUR_D"

  Info -v "PLATEFORME = $PLATEFORME"

  Info -v "LOGS_DIR = $LOGS_DIR"
  Info -v "ENTETE_FILE = $ENTETE_FILE"
  Info -v "LOG_FILE = $LOG_FILE"
  Info -v "MTX_LOG_FILE = $MTX_LOG_FILE"
  Info -v "ERROR_FILE = $ERROR_FILE"
  Info -v "MACH_ERROR = $MACH_ERROR"
  Info -v "TMP_LOGS = $TMP_LOGS"

  return 0
}

###### Controle_Parametres
## Test si tous les arguments sont présents lors de         ##
## l'exécution du script. Si ce n'est pas le cas la fonction "Usage" affichera l'aide ##
## Le fichier doit être charger pour utiliser les variables nécessaires
###############################################################
Controle_Parametres() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Parametres $*"

  # Variable local
  typeset LEVEL

  # Si il 'y a pas de parametre c'est une erreur
  if [ -z "TYPE_ACTION" ] ; then
    echo "/!\ Le type de sauvegarde doit etre specifie."
    echo ""
    Usage
  fi

  LEVEL="`echo ${NIVEAU_SAUVEGARDE} | sed 's/[a-zA-Z]//g'`"

  # Si le type d'action est eject ou test, les autres paramètre n'ont pas d'importance
  if [ "$TYPE_ACTION" != "eject" ] && [ "$TYPE_ACTION" != "test" ] ; then
    # Le niveau de sauvegarde doit etre un chiffre entre 0 et 9
    if [ -z "$LEVEL" ] || [ "$LEVEL" -lt 0 ] || [ "$LEVEL" -gt 9 ] ; then
      echo "/!\ Le niveau de sauvegarde doit etre un chiffre compris entre 0 et 9."
      Info -v "LEVEL = $LEVEL"
      echo ""
      Usage
    fi
    # le type de sauvegarde doit etre l'un des 3 termes test
    if [ "${TYPE_SAUVEGARDE}" != "erase" ] && [ "${TYPE_SAUVEGARDE}" != "eom" ] && [ "${TYPE_SAUVEGARDE}" != "taille" ] ; then
      echo "/!\ L'option erase, eom, ou taille doit etre precisee."
      Info -v "TYPE_SAUVEGARDE = $TYPE_SAUVEGARDE"
      echo ""
      Usage
    fi
    # Si le type n'est pas taille la gestion du media doit contenir l'une des trois chaîne suivante : media, box ou changer
    if [ "${TYPE_SAUVEGARDE}" != "taille" ] && [ "${GESTION_MEDIA}" != "media" ] && [ "${GESTION_MEDIA}" != "box" ] && [ "${GESTION_MEDIA}" != "changer" ] ; then
      echo "/!\ L'option media ou box ou changer doit etre precisee."
      Info -v "TYPE_SAUVEGARDE = $TYPE_SAUVEGARDE"
      Info -v "GESTION_MEDIA = $GESTION_MEDIA"
      echo ""
      Usage
    fi
    # Si le type n'est pas taille et la gestion du media est changer, un numero de bande doit etre indiqué
    if [ "${TYPE_SAUVEGARDE}" != "taille" ] && [ "${GESTION_MEDIA}" = "changer" ] && [ -z "${NUMERO_BANDE}" ] ; then
      echo "/!\ L'option changer necessite la specification"
      echo "    d'un numero de bande."
      Info -v "TYPE_SAUVEGARDE = $TYPE_SAUVEGARDE"
      echo ""
      Usage
    fi
    # Si le type n'est pas taille, et que le numero de bande n'est pas vide
    if [ "${TYPE_SAUVEGARDE}" != "taille" ] && [ ! -z "${NUMERO_BANDE}" ] ; then
      # Controle que le numero de bande soit compris entre 0 et le N° maximum MAX_BANDE
      # Si -z $MAX_BANDE alors MAX_BANDE=0
      [ -z $MAX_BANDE ] && MAX_BANDE=0
      if [ "${NUMERO_BANDE}" -lt 0 ] || [ "${NUMERO_BANDE}" -gt "$MAX_BANDE" ] ; then
        echo "/!\ Le numero de bande doit etre compris entre 0 et $MAX_BANDE."
        Info -v "NUMERO_BANDE = $NUMERO_BANDE"
        Info -v "MAX_BANDE = $MAX_BANDE"
        echo ""
        Usage
      fi
    fi
  fi

  return 0
}

####### Controle_Absence_Verrou
## Vérifie qu'il n'y ait pas un fichier de lock
# Arret de l'execution du script si il existe
# Crée le fichier lock si il n'existe pas                    ##
###############################################################
Controle_Absence_Verrou(){
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Absence_Verrou $*"

  ## Cette variable est connue de la fonction Sortir
  VERROU="${LOGS_DIR}/${BASE_NAME}.lock"
  if [ -f "$VERROU" ] ; then
    Info -v "le verrou est present : $VERROU"
    Info -e "Une autre sauvegarde est deja en cours..."
    # ici on n'utilise pas la fonction Sortir. Le verrou serait supprimé
    exit 13
  else
    echo "$$:$DATE_EXE" > $VERROU
  fi

  Info -v "VERROU = $VERROU"

  return 0
}

###### Nettoyage_Logs
## Supprime les fichiers de logs portant le même nom que ceux
#  de l'execution en cours. ##
###############################################################
Nettoyage_Logs() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Nettoyage_Logs $*"

  # Variable local
  typeset LOG
  #~ Info -v "LOG_FILE = $LOG_FILE"

  # Supprime les fichiers de log
  /bin/rm -f $LOG_FILE `echo "$MTX_LOG_FILE $ERROR_FILE $MACH_ERROR " | sed -e 's/\.[0-9]* /\* /g'` 2>/dev/null

  # Concatène les fichiers dans un fichier unique
  for LOG in $LST_LOG_TO_SEND; do
    /bin/cat $LOG 2>/dev/null >> $LOG_FILE
  done

  # Supprime les fichiers correspondants
  for LOG in `/bin/ls ${LOG_FILE}[_-]* 2>> /dev/null`; do
    Info -v "LOG = $LOG"
    /bin/rm -f $LOG 2>/dev/null
  done

  # renomme le fichier de log concaténé
  # Mise a jour la valeur de la variable hébergeant le nom du fichier par le nouveau nom
  if [ ! -z "`echo ${TYPE_ACTION} | /bin/grep 'taille'`" ] ; then
    mv $LOG_FILE ${LOG_FILE}_taille 2>/dev/null
    LOG_FILE="${LOG_FILE}_taille"
  fi

  # Compresse et supprime les LOGS
  typeset x=`pwd`
  cd ${LOGS_DIR}
  if [ -f ${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE} ] ; then
    tar cf - ${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE} | ${GZIP} -9c > ${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}.tar.gz
    /bin/rm -rf ${LOGS_DIR}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}
  fi
  cd $x
  Info -v "LOG_FILE = $LOG_FILE"

  return 0
}

####### Envoie_Mail
## Envoie le mail de résultat                                ##
###############################################################
Envoie_Mail() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Envoie_Mail $*"

  # Ajoute le résultat et les bandes à l'intérieur du fichier de LOG
  N_LINE=`/bin/grep -n "^LOG.*:" $LOG_FILE | cut -f1 -d:`

  Info -v "N_LINE = $N_LINE"

  # Construction des différentes partie du mail
  SUBJECT=SVGS:$PLATEFORME:${MACHINE_EXE}:$TYPE_ACTION:$NIVEAU_SAUVEGARDE:${ERROR}:`echo $DATE_EXE | sed -e 's/_//'`
  RESULT="RESULTAT     : $ERROR"
  USED_TAPES="BANDE        : ${FIRST_BANDE}-${LAST_BANDE}"

  Info -v "SUBJECT = $SUBJECT"
  Info -v "RESULT = $RESULT"
  Info -v "USED_TAPES = $USED_TAPES"
  Info -v "DEST_EMAIL = $DEST_EMAIL"

  # Si la fonction Info -m a ete utiliser sont texte doit etre en haut du corp du mail
  if [ -f "$MAIL_CORP" ] ; then
    cat "${LOG_FILE}" >> "${MAIL_CORP}"
    mv "${MAIL_CORP}" "${LOG_FILE}"
  fi

  cat "${LOG_FILE}" >> $ENTETE_FILE
  cat $ENTETE_FILE  >> "${LOG_FILE}"
  rm -f $ENTETE_FILE

  mv ${LOG_FILE} ${LOG_FILE}_${STAT}

  # Envoie du mail si une adresse mail est indiqué
  if [ -n "$DEST_EMAIL" ] ; then
    # Iconv permet de régler temporairement un BUG de contenue binaire dans le mail
    /bin/cat ${LOG_FILE}_${STAT} | fold -w 78 | ${MAILER} -s "$SUBJECT" $DEST_EMAIL
    #~ /bin/cat ${LOG_FILE}_${STAT} | fold -w 78 | iconv -t ascii 2>/dev/null | ${MAILER} -s "$SUBJECT" $DEST_EMAIL
  else
    echo "Adresse de messagerie vide"
  fi

  return 0
}

###### Controle_Binnaire
# DEPENDANCES : Info -e Info
# Usage : Controle_Binaire /chemin/commande
# Description : Permet de contrôler la présence d'un outils localement
# contrôle que vous ayez le droit d'exécuter l'outil
# le chemin peut-être indiqué en /absolue ou en ../relatif
# Code retour : 0 = OK , 1 bin abs et 2 bin noexec
###############################################################
Controle_Binaire () {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Binaire $*"

  # Variable local
  typeset NOM_BINAIRE=$1

  # La variable mise en paramètre a la fonction est vide
  [ -z "$NOM_BINAIRE" ] && return 1

  # Contrôle la présence du binaire
  if [ -f "$NOM_BINAIRE" ] ; then
    Info -v "Le fichier $NOM_BINAIRE est pesent"
    # Contrôle des droits d'exécution
    if [ ! -x "$NOM_BINAIRE" ] ; then
      Info -v "Mais ne peux etre execute"
      Info -e "Vous n'avez pas les droit d'execution sur -${NOM_BINAIRE}-"
      Sortir 3
    fi
  else
    Info -e "Le binnaire -${NOM_BINAIRE}- n'est pas present"
    Sortir 2
  fi

  # Si pas sortie avant, c'est que tous c'est bien passé
  return 0
}

####### Test_Execution_Commandes
## Teste l'existence et la possibilité d'exécuter les commandes nécessaires au script
# Attention : ce contrôle n'est valable que pour la machine qui exécute le script
###############################################################
Test_Execution_Commandes() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Test_Execution_Commandes $*"

# Test de Binaire
  Controle_Binaire $AWK
  Controle_Binaire $CAT
  Controle_Binaire $DF
  Controle_Binaire $ECHO
  Controle_Binaire $EGREP
  Controle_Binaire $GREP
  Controle_Binaire $RM
  Controle_Binaire $SED
  Controle_Binaire $SORT
  Controle_Binaire $TR
  Controle_Binaire $PING
  Controle_Binaire $_XPG4GREP

  Controle_Binaire $MAILER
  Controle_Binaire $MOUNT
  Controle_Binaire $UMOUNT

  ## cas ou la variabl n'existe que pour un OS
  case "`/bin/uname -s`" in
    # Spécifique à Solaris
    "SunOS")
       # Seul SUNOS a ce binaire,
       Info -v "Teste de la presence de ${SVGTOC} "
      Controle_Binaire "${SVGTOC}"
    ;;
  esac

  return 0
}

####### Definir_Type_Sauvegarde
## Définit les variables nécessaires à la destination de la sauvegarde ##
###############################################################
Definir_Type_Sauvegarde() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Definir_Type_Sauvegarde $*"

  # compare avec les 3 1er caractères de TYPE_DEV
  case "$TYPE_DESTINATION" in
    "robot" )
      Info -i "Le type de sauvegarde est ROBOT"
      SAVE_TYPE="BANDE (robot local ou distant)"
    ;;
    "bande") # si rien trouvé il est considéré un lecteur de bande
      Info -i "Le type de sauvegarde est BANDE locale ou distante"
      SAVE_TYPE="BANDE (locale ou distante)"
      Info -v "TYPE_DESTINATION = $TYPE_DESTINATION"
    ;;
    "disk"|*)
      Info -i "Le type de sauvegarde est DISK locale ou distante"
      SAVE_TYPE="DISK (locale ou distante)"
      Info -v "TYPE_DESTINATION = $TYPE_DESTINATION"
    ;;
    "changer")
    Info -i "Le type de sauvegarde est BANDE locale ou distante avec robot (librairie)"
    SAVE_TYPE="BANDE (locale ou distante)"
    ;;
  esac

  Info -v "SAVE_TYPE = $SAVE_TYPE"
  Info -v "TYPE_DESTINATION = $TYPE_DESTINATION"
  Info -v "ERROR = $ERROR"
  return 0
}

####### Construction_Prefix_Destination
# La variable PREFIX doit contenir la commande qui sera mise
# devant les commandes devant accéder au lecteur de bande ou au robot.
# Afin de déterminer le préfixe, il est nécessaire de définir si les rôles sont tenue par des machines distantes ou non
# Le sens de la connexion a la destination est soit déduit, soit pris dans le fichiers de configuration
#
#
# Cette variable est ici valorisée par cette commande en fonction de l'emplacement du lecteur de bande et du type de sauvegarde.
# De façon général si le lecteur de bande est local, PREFIX vaudra "eval".
# si il est distant, PREFIX vaudra rsh ou ssh avec les paramètres nécessaires au bon fonctionnement
## Par défaut le protocole utilise pour l'accès distant est rsh
###############################################################
Construction_Prefix_Destination() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Construction_Prefix_Destination $*"
  # Si la machine de destination est la meme que la machine source ou la machine qui execute
  # Alors nous sommes en local
  PREFIX_SOURCE_COPY=$PREFIX_SOURCE
  [ "$PREFIX_SOURCE_COPY" = "eval" ] && PREFIX_SOURCE_COPY=""
  case $PROTOCOLE_D in
    "rsh"|"ssh") #
      # Choix de la commande en fonction du protocole
      case $PROTOCOLE_D in
      "rsh")
        CMD_DISTANTE="rsh $ARGUMENT_PROTOCOLE_D"
      ;;
      "ssh")
        CMD_DISTANTE="ssh -qx $ARGUMENT_PROTOCOLE_D"
      ;;
      esac

      # Crée le préfixe et le sens
      Info -v "Connection $PROTOCOLE_D detecte"
      # local : tous les roles sont sur la même machine
      if [ "${MACHINE_EXE}" = "${MACHINE_D}" ] && [ "${MACHINE_EXE}" = "${MACHINE_S}" ]  ; then
        Info -v "Les 3 roles sont sur la meme machine"
        Info -v "Pour cette machine le type de sauvegarde est local"
        SENS_PROTOCOLE_D="directe"
        PREFIX_DESTINATION="eval"
        PREFIX_DESTINATION_SAUVE="eval"
      else
        # Les 3 rôles sont sur des machines différentes, il faut que le SENS soit définit dans la conf
        if [ "${MACHINE_EXE}" != "${MACHINE_D}" ] && [ "${MACHINE_EXE}" != "${MACHINE_S}" ] && [ "${MACHINE_D}" != "${MACHINE_S}" ] ; then
          Info -v "Chaque role est sur une machine distincte"
          [ -z "$SENS_PROTOCOLE_D" ] && Ajout_Tableau_Erreur "Le sens du protocole dans PROTO_DEV n'est pas définit " && continue
          [ -z "$MACHINE_D" ] && Ajout_Tableau_Erreur "Le nom de machine n est pas spécifie dans TAPE_HOST" && continue
          if [ "$SENS_PROTOCOLE_D" = "directe" ] ; then
            PREFIX_DESTINATION="$PREFIX_SOURCE_COPY $CMD_DISTANTE $TAPE_HOST"
            PREFIX_DESTINATION_SAUVE="$CMD_DISTANTE $TAPE_HOST"
          else
            PREFIX_DESTINATION="$CMD_DISTANTE $TAPE_HOST"
            PREFIX_DESTINATION_SAUVE="$CMD_DISTANTE $TAPE_HOST"
          fi
        else
          # Un rôle sur une machine, les 2 autres rôles sur une autre machine
          # c'est l'une ou l'autre, mais pas les 2 car elle passerait dans la condition au dessus et non dans ce "else"
          if [ "${MACHINE_EXE}" = "${MACHINE_D}" ] ; then
            # La machine exe a aussi le rôle de destination, le sens est indirecte, la machine source est sur une autre machine
            Info -v "La machine exe a aussi le role de destination"
            SENS_PROTOCOLE_D="indirecte"
            PREFIX_DESTINATION="eval"
            PREFIX_DESTINATION_SAUVE="eval"
          elif [ "${MACHINE_EXE}" = "${MACHINE_S}" ] ; then
            # La machine source a aussi le rôle d'exécution, le sens est directe, le rôle destination est sur une autre machine
            Info -v "la machine qui execute est la machine a sauvegarder"
            SENS_PROTOCOLE_D="directe"
            PREFIX_DESTINATION="$CMD_DISTANTE $TAPE_HOST"
            PREFIX_DESTINATION_SAUVE="$CMD_DISTANTE $TAPE_HOST"
          else
            # La machine source à aussi le rôle de destination , le rôle exe est sur une autre machine
            Info -v "La machine source a aussi le role de destination"
            SENS_PROTOCOLE_D="directe"
            #~ PREFIX_DESTINATION="eval"
            PREFIX_DESTINATION="$CMD_DISTANTE $TAPE_HOST"
            PREFIX_DESTINATION_SAUVE="eval"
          fi
        fi
      fi
    ;;
    "local") # le local est local en fonction de EXE donc indirecte
      PREFIX_DESTINATION="eval"
      SENS_PROTOCOLE_D="directe"
      PREFIX_DESTINATION_SAUVE="eval"
    ;;
  esac

  Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
  Info -v "SENS_PROTOCOLE_D = $SENS_PROTOCOLE_D"

  return 0
}

####### Execute_Commande_C_Retour
## Execute une commande sur la source ou la destination puis renvoie le code de sortie
# Attention la sortie standard n'est pas renvoyé. Voir Execute_Commande_R_Resul .
# Utiliser l'option -d pour destination ou -s pour la source.
Execute_Commande_C_Retour() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Execute_Commande_C_Retour $*"

  typeset PREFIX_CMD
  typeset RETOUR_CMD
  typeset COMMANDE
  case "$1" in
    -d) # l exécution doit être effectuée sur la destination"
      shift
      COMMANDE="$* ; echo RETOUR_DE_COMMANDE=\$?"
      PREFIX_CMD="${PREFIX_DESTINATION_SAUVE}"

      # Si c'est la machine source qui communique avec la destination
      if [ "$SENS_PROTOCOLE_D" = "directe" ] ; then
        if [ "$PREFIX_SOURCE" != "eval" ] ; then
          # La source est distante et c'est elle qui communique avec la destination (distante ou local d source)
          Info -v "Directe prefixe source != eval"
          Info -v "$PREFIX_SOURCE \"$PREFIX_CMD \"$COMMANDE\"\""
          $PREFIX_SOURCE "$PREFIX_CMD \"$COMMANDE\"" | tee -a $LOG_FILE > ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
          RETOUR_CMD=`grep ^RETOUR_DE_COMMANDE= ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp | cut -d= -f2 | cut -c1`
        else
          # La source est local, la destination peux être local ou distante
          Info -v "Directe prefixe source = eval"
          $PREFIX_SOURCE $PREFIX_CMD" \"$COMMANDE\"" | tee -a $LOG_FILE > ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
          RETOUR_CMD=`grep ^RETOUR_DE_COMMANDE= ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp | cut -d= -f2 | cut -c1`
        fi
          # Nettoie le fichier de test
      else
        # Indirecte
        # La source est distante mais le flux de sauvegarde revient par la machine exe pour être sauvegardé
        if [ "$PREFIX_SOURCE" != "eval" ] ; then
          Info -v "Indirecte est le préfixe source est != de eval"
          $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE > ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
          RETOUR_CMD=`grep ^RETOUR_DE_COMMANDE= ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp | cut -d= -f2 | cut -c1`
        else
          # La source est local ### Ne devrait pas passer par ici car si la source est local c'est du directe
          Info -v "Indirecte est le préfixe source = eval"
          Info -v "ATTENTION : NE devrait pas passer ICI : local ne doit pas être "
          $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE > ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
          RETOUR_CMD=`grep ^RETOUR_DE_COMMANDE= ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp | cut -d= -f2 | cut -c1`
        fi
      fi
    ;;
    -s) # L exécution doit être effectuée sur la source"
      PREFIX_CMD="${PREFIX_SOURCE}"
      shift
      Info -v "Le prefix est : $PREFIX_CMD"
      Info -v "La commande est : $*"
      COMMANDE="$* ; echo RETOUR_DE_COMMANDE=\\$?"
      $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE > ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
      RETOUR_CMD=`grep ^RETOUR_DE_COMMANDE= ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp | cut -d= -f2 | cut -c1`
    ;;
  esac

  Info -v "RETOUR_CMD = $RETOUR_CMD"
  Info -v "Le fichier tempraire contient :"
  Info -v "`cat ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp`"
  rm -f ${LOGS_DIR}/${BASE_NAME}_${DATE_EXE}_RC.tmp
  return $RETOUR_CMD

}

####### Execute_Commande_R_Resul
## Execute une commande sur la source ou la destination puis renvoie la sortie.
# Utiliser l'option -d pour destination ou -s pour la source.
Execute_Commande_R_Resul() {
# [ "$DEBUG" = "1" ] && set -x
# NE PAS UTILISER LA FONCTION INFO
# NE PAS METRE d'echo
# CETTE FONCTION NE DOIT RENVOYER QUE LE RESULTAT DE LA COMANDE MISE EN PARAMETRE


  typeset PREFIX_CMD
  typeset RETOUR_CMD
  typeset COMMANDE
  case "$1" in
    -d) # l exécution doit être effectuée sur la destination"
      shift
      COMMANDE="$*"
      PREFIX_CMD="${PREFIX_DESTINATION_SAUVE}"

      # Si c'est la machine source qui communique avec la destination
      if [ "$SENS_PROTOCOLE_D" = "directe" ] ; then
        if [ "$PREFIX_SOURCE" != "eval" ] ; then
          # La source est distante et c'est elle qui communique avec la destination (distante ou local d source)
          # Directe prefixe source != eval"
          $PREFIX_SOURCE "$PREFIX_CMD \"$COMMANDE\"" | tee -a $LOG_FILE
        else
          # La source est local, la destination peux être local ou distante
          #~ Info -v "Directe prefixe source = eval"
          $PREFIX_SOURCE "$PREFIX_CMD \"$COMMANDE\"" | tee -a $LOG_FILE

        fi
          # Nettoie le fichier de test
      else
        # Indirecte
        # La source est distante mais le flux de sauvegarde revient par la machine exe pour être sauvegardé
        if [ "$PREFIX_SOURCE" != "eval" ] ; then
          #~ Info -v "Indirecte est le préfixe source est != de eval"
          $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE
        else
          # La source est local ### Ne devrait pas passer par ici car si la source est local c'est du directe
          $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE
        fi
      fi
    ;;
    -s) # L exécution doit être effectuée sur la source"
      PREFIX_CMD="${PREFIX_SOURCE}"
      shift
      COMMANDE="$*"
      $PREFIX_CMD "$COMMANDE" | tee -a $LOG_FILE
    ;;
  esac


}

####### Parametre_Sauvegarde
## Parametre la source en fonction du type de sauvegarde
# Permet de définir la liste des machines à sauvegarder      ##
###############################################################
Parametre_Sauvegarde() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Parametre_Sauvegarde $*"

  case ${TYPE_ACTION} in
    "all")
      # récupère les éléments à sauvegarder
      if [ -n "$LST_SAUVE_ALL" ] ; then
        LST_SAUVE=$LST_SAUVE_ALL
      else
        LST_SAUVE=${LST_SAUVE_SYS}${LST_SAUVE_USER}${LST_SAUVE_SOFT}
      fi
    ;;
    "eject")
      LST_SAUVE=NONE
      TYPE_ACTION="eject"
      NIVEAU_SAUVEGARDE="NONE"
    ;;
    "test")
      LST_SAUVE="\${LST_SAUVE_`echo $1 | tr '[:lower:]' '[:upper:]'`}"
      eval LST_SAUVE="${LST_SAUVE}"
      [ -z "$LST_SAUVE" ] && LST_SAUVE=NONE
      TYPE_ACTION="test"
      NIVEAU_SAUVEGARDE="NONE"
    ;;
    *)
      Info -v "Parametre definit par defaut"
      LST_SAUVE="\${LST_SAUVE_`echo $1 | tr '[:lower:]' '[:upper:]'`}"
      LST_SAUVE=`eval "echo ${LST_SAUVE}"`
      TYPE_ACTION="$TYPE_ACTION"
    ;;
  esac

  Info -v "LST_SAUVE = $LST_SAUVE"
  Info -v "TYPE_ACTION = $TYPE_ACTION"
  Info -v "NIVEAU_SAUVEGARDE = $NIVEAU_SAUVEGARDE"

  return 0
}

####### Detecte_Information_Fs
# Définit le type de FS
# Déterminer le Point de montage
# Se base sur le point de montage et non sur le répertoire mis en paramètre
# Déterminer la partition logique ou physique attachée au point de montage
# Déterminer le type de FS
#"${FILE_SYSTEM}:${TAILLE_K}:${ESPACE_UTILISE}:${POINT_DE_MONTAGE}:${TAILLE_BLOCK}:${PART_TYPE_DE_FS}"
###############################################################
Detecte_Information_Fs() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Detecte_Information_Fs $*"

  typeset REP_DEMANDER=$@
  TEST_EXISTE_PART=`${PREFIX_SOURCE} "if [ -d \"$REP_DEMANDER\" ] ; then echo OK ; else echo KO; fi"`

  Info -v "TEST_EXISTE_PART = $TEST_EXISTE_PART"

  if [ "$TEST_EXISTE_PART" = "OK" ] ; then
    case $OS_SOURCE in
      "Linux")
        # Déterminer le Point de montage
        INFO_PART=`${PREFIX_SOURCE} "df -Pk $REP_DEMANDER | tail -1"`

        FILE_SYSTEM=`echo $INFO_PART | awk '{print $1}'`
        TAILLE_K=`echo $INFO_PART | awk '{print $2}'`
        ESPACE_UTILISE=`echo $INFO_PART | awk '{print $3}'`
        POINT_DE_MONTAGE=`echo $INFO_PART | awk '{print $6}'`

         # Si le champ 4 (point de montage ) est égal à la partition en cours affiche le champ 5 (type fs)
        Info -v "Cherche linux le type de FS methode 1"
        PART_TYPE_DE_FS=`${PREFIX_SOURCE} 'mount ' | grep " $POINT_DE_MONTAGE " | cut -d " " -f 5`

        if [ -z "$PART_TYPE_DE_FS" ] ; then
          Info -v "Cherche linux le type de FS methode 2"
          PART_TYPE_DE_FS=`${PREFIX_SOURCE} 'cat /etc/mtab 2>/dev/null | grep -v ^# | grep " $POINT_DE_MONTAGE " | cut -d " " -f 3'`
        fi

        if [ -z "$PART_TYPE_DE_FS" ] ; then
          Info -v "Cherche linux le type de FS methode 3"
          PART_TYPE_DE_FS=`${PREFIX_SOURCE} 'cat /etc/fstab 2>/dev/null | grep -v ^# | grep " $POINT_DE_MONTAGE " | cut -d " " -f 3'`
        fi
        TAILLE_BLOCK=1024
      ;;
      "SunOS")
        INFO_PART=`${PREFIX_SOURCE} "df -k $REP_DEMANDER | tail -1"`

        typeset FILE_SYSTEM=`echo "$INFO_PART" | awk '{print $1}'`
        typeset TAILLE_K=`echo "$INFO_PART" | awk '{print $2}'`
        typeset ESPACE_UTILISE=`echo "$INFO_PART" | awk '{print $3}'`
        typeset POINT_DE_MONTAGE=`echo "$INFO_PART" | awk '{print $6}' `

        # Si le champ 4 (point de montage ) est égal a la partition en cours affiche le champ 5 (type fs)
        Info -v "Cherche SunOs le type de FS methode 1 "
        Info -v "Attention : seul cette methode est compatible avec ZFS"
        PART_TYPE_DE_FS=`${PREFIX_SOURCE} 'mount -p '| grep " $POINT_DE_MONTAGE " | cut -d" " -f4`
        # Non compatible ZFS
        if [ -z "$PART_TYPE_DE_FS" ] ; then
          Info -v "Cherche SunOs le type de FS methode 2"
          Info -v "POINT_DE_MONTAGE = $POINT_DE_MONTAGE"
          ${PREFIX_SOURCE} 'cat /etc/vfstab' | grep -v ^"#"
          typeset PART_TYPE_DE_FS=`${PREFIX_SOURCE} 'cat /etc/vfstab' | grep -v ^# | grep " ${POINT_DE_MONTAGE} " | cut -d" " -f4`
        fi
        # Non compatible ZFS
        if [ -z "$PART_TYPE_DE_FS" ] ; then
          Info -v "Cherche SunOs le type de FS methode 3"
          typeset PART_TYPE_DE_FS=`${PREFIX_SOURCE} /usr/sbin/fstyp $FILE_SYSTEM`
        fi
        typeset TAILLE_BLOCK=1024
      ;;
      *)
        # Ne sait pas sauvegarde les machines de type $OS_SOURCE
        PART_TYPE_DE_FS="KO"
        Ajout_Tableau_Erreur "Systeme d exploitation non pris en charge"
        Sortir 5
      ;;
    esac
  else
      # Le répertoire n'existe pas, devra être ignoré
      Info -a "Le repertoire $REP_DEMANDER n existe pas. Il sera ignore"
      PART_TYPE_DE_FS="IGNORE"
  fi

  # Renvoie les informations dans un format par champ séparé par ":"
  INFO_PART="${FILE_SYSTEM}:${TAILLE_K}:${ESPACE_UTILISE}:${POINT_DE_MONTAGE}:${TAILLE_BLOCK}:${PART_TYPE_DE_FS}:$REP_DEMANDER"

  Info -v "OS_SOURCE = $OS_SOURCE"
  Info -v "INFO_PART = $INFO_PART"
  Info -v "PART_TYPE_DE_FS = $PART_TYPE_DE_FS"

  # géré un code retour
  if [ -z "$PART_TYPE_DE_FS" ] ; then
    return 1
  else
    return 0
  fi
}

####### Estime_Taille_Partition
## Permet d'estimer la taille d'une partition en récupérant
# l'information dans le tableau de la partition en cours de traitement ##
###############################################################
Estime_Taille_Partition() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Estime_Taille_Partition $*"
  typeset REP_DEMANDER=$1

  # Si c'est du ZFS
  if [ "$DUMP_ESTIME" = "zfs" ] ; then

    # Si le niveau de sauvegarde est "full" DF est utilisé pour estimer la taille du dump
    # Aucune méthode trouvée pour déterminer la taille d'un différentiel donc utilisation de la même méthode
    # Le if n'est présent que pour l'évolution du script
    if [ "$NIVEAU_SAUVEGARDE" -eq 0 ] ; then
      # Récupère dans le tableau la valeur enregistrée
      eval ESTIMATION_KB="\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[2]}"
    else
      # niveau de sauvegarde non full
      # A FAIRE : Reste a trouver comment faire
      # pour le moment nous utilisons la même méthode que celle d'une full
      eval ESTIMATION_KB="\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[2]}"
    fi

    ESTIMATION_MO=`expr "$ESTIMATION_KB" / 1024`
    # Pour la compatibilitée avec l'ancienne version
    ESTIMATION_BYT=`expr "$ESTIMATION_MO" \* 1048576`

    Info -v "Estimation effectue par df : $ESTIMATION_MO Mo trouve"

  else
    # Si ce n'est pas du ZFS, on utilise dump pour évalué
    # Récupère l'espace en bytes
    typeset COM="$PREFIX_SOURCE \"$DUMPER ${NIVEAU_SAUVEGARDE}${DUMP_ESTIME} $REP_DEMANDER 2>/dev/null\""
    ESTIMATION_BYT=`eval $COM`

    # Convertion des blocks en Mega-octects
    ESTIMATION_MO=`expr "$ESTIMATION_BYT" / 1048576`

    # Convertion des blocks en Kilo-octects
    ESTIMATION_KB=`expr "$ESTIMATION_BYT" / 1024`

    Info -v "Estimation effectue par $DUMPER : $ESTIMATION_MO Mo trouve"
  fi

  # Si rien trouvé, on indique a 0
  [ -z "$ESTIMATION_MO" ] && ESTIMATION_MO=0
  [ -z "$ESTIMATION_BYT" ] && ESTIMATION_BYT=0

  # Affiche dans les log la taille estime
  printf "%s:%-16s%13s bytes estimes - %6s MB\n" $MACHINE_S $REP_DEMANDER \
  $ESTIMATION_BYT $ESTIMATION_MO | tee -a $LOG_FILE_MACHINE_S

  Info -v "ESTIMATION_MO = $ESTIMATION_MO"
  Info -v "ESTIMATION_BYT = $ESTIMATION_BYT"
  Info -v "ESTIMATION_KB = $ESTIMATION_KB"
  Info -v "DUMP_ESTIME = $DUMP_ESTIME"
  Info -v "COM = $COM"

return 0
}

####### Entete_Completion
## Ajoute les résultats de la machine à l'entete               ##
###############################################################
Entete_Completion() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Entete_Completion $*"

  typeset LOG

  # Liste les stations qui n'ont pas pu être sauvegardés
  if [ -r "$MACH_ERROR" ] ; then
    Info -t "=============================================================="
    Info -t "| STATIONS INACCESSIBLES :                                    "
    while read MACH; do
      Info -t "| $MACH "
    done <$MACH_ERROR
  fi

  # Calcul la taille de chaque machine
  Info -t "=============================================================="
  for COUNT_MACHINE_S in ${LIST_REF_MACHINES_S}; do
    eval typeset TAILLE=\${CONF_${COUNT_MACHINE_S}[10]}
    eval typeset MACHINE_S=\${CONF_${COUNT_MACHINE_S}[0]}
    Info -v "TAILLE = $TAILLE"
    Info -v "MACHINE_S = $MACHINE_S"

    if [ "$MACHINE_S" != "ENTETE" ] ; then
      [ -z "$TAILLE" ] && TAILLE=0
      printf "| %-20s:\t\t%6d (MB)\n" $MACHINE_S $TAILLE | tee -a $ENTETE_FILE
    fi
  done

  #~ # Concaténation de tous les logs générés
  #~ for LOG in $LST_LOG_TO_SEND; do
    #~ /bin/cat $LOG >> $LOG_FILE
  #~ done

  CUMUL_ESTIMATION_TOUTE_MACHINE_S=${TAB_GENERAL[1]}
  # Calcul de la taille totale sauvegardée, résultat dans $SOMME
  Info -t "=============================================================="
  printf "Volume total        :\t\t%6d (MB)\n" $CUMUL_ESTIMATION_TOUTE_MACHINE_S | tee -a $ENTETE_FILE

  # Rajout des statistiques par stations
  Info -t "=============================================================="
  for RES_MACH in `echo $LOG_RES | awk 'BEGIN {RS =":"} /^[^#]*[a-zA-Z0-9]+/ {print $0}'`; do
    Info -t "`echo $RES_MACH`                "
  done

  if [ -f "$MTX_LOG_FILE" ] ; then
    Info -t "=============================================================="
    /bin/cat $MTX_LOG_FILE                                                     | tee -a $ENTETE_FILE
    /bin/rm -f $MTX_LOG_FILE
  fi

  Info -t "=============================================================="

  return 0

}

####### Cherche_Erreur_Dans_Log
## Analyse le LOG de fin pour déceler les erreurs            ##
###############################################################
Cherche_Erreur_Dans_Log() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Cherche_Erreur_Dans_Log $*"

  # Attention : la variable ERROR ne devrait pas être utilisé comme variable
  # Surtout pour une variable global
  # Elle peux être un mot réservé ou utilisé ailleurs

  # Recherche de mot clef pouvant être des erreurs
  if [ "${TYPE_ACTION}" = "test" ] ; then
    ERROR=`/bin/cat $LOG_FILE | /bin/grep -i 'offline'`
  else
    ERROR=`echo $LOG_RES | /bin/grep ',Nok'`
  fi

  # Contrôle si des mots clef d'erreur ont être trouvé
  # Valorise une variable en fonction du résultat
  # Le contenue pourra être contrôler dans le code
  if [ -z "$ERROR" ] ; then
    ERROR="Ok"
  else
    ERROR="Nok"
  fi

  Info -v "ERROR = $ERROR"
  return 0
}

####### Controle_Connexion
## Permet de vérifier que le client répond                   ##
###############################################################
Controle_Connexion() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Connexion $*"

  DATE_DEBUT_DUMP=`date +%Y%m%d%H%M%S`

  case "$1" in
    -s)
      shift
       # ping vers le client
      if [ "${MACHINE_S}" != "${MACHINE_EXE}" ] ; then
        ($PING $MACHINE_S $PING_OPTIONS 2>&1) > /dev/null
        if [ "$?" -ne 0 ] ; then
          echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
          echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
          Info -a "La station $MACHINE_S ne repond pas au ping          " | tee -a $LOG_FILE_MACHINE_S
          echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
          echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        fi
      fi

      # Tente l'exécution de la commande sur la machine source
      # test accès au client
      OS_DESTINATION=`$PREFIX_SOURCE  "uname -s" 2>&1 >/dev/null`
      if [ "$?" -ne 0 ] ; then
        echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        Info -e "$PREFIX_SOURCE interdit sur la station $MACHINE_S   " | tee -a $LOG_FILE_MACHINE_S
        echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        echo "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        CLIENT_OK=0
        echo $MACHINE_S >>$MACH_ERROR
        LOG_RES=$LOG_RES:$MACHINE_S,ALL,$CODE_BANDENUM,$ID_POSITION_BANDE,0,$DATE_DEBUT_DUMP,$DATE_FIN_DUMP,Nok
        return 1
      else
        CLIENT_OK=1
      fi

      Info -v "CLIENT_OK = $CLIENT_OK"
    ;;
    -d) # Supprime la machine du fichier rhost
      shift
      # Tente l'exécution de la commande sur la machine source
      # test accès au client
      OS_DESTINATION=`Execute_Commande_R_Resul -d  "uname -s" 2>&1 >/dev/null`
      if [ "$?" -ne 0 ] ; then
        Info -v "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        Info -v "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        Info -e "$Execute_Commande_R_Resul -d impossible de se connecter a la destination:  -${MACHINE_D}-   " | tee -a $LOG_FILE_MACHINE_S
        Info -v "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        Info -v "###########################################################" | tee -a $LOG_FILE_MACHINE_S
        TEST_CONNEXION_D=0
        #~ LOG_RES=$LOG_RES:$MACHINE_S,ALL,$CODE_BANDENUM,$ID_POSITION_BANDE,0,$DATE_DEBUT_DUMP,$DATE_FIN_DUMP,Nok
        return 1
      else
        TEST_CONNEXION_D=1
      fi
      Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
      Info -v "TEST_CONNEXION_D = $TEST_CONNEXION_D"
    ;;
  esac

  return 0
}

####### Controle_Ecriture_Destination
## Contrôle l'écriture sur la destination
# si elle n'est pas de type lecteur de bande                  ##
###############################################################
Controle_Ecriture_Destination() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Ecriture_Destination $*"

  TAPE_DEV_REP="${TAPE_DEV}/${MACHINE_S}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}"

  case $TYPE_DESTINATION in
  "disk")
    Info -v "PREFIX_DESTINATION_SAUVE = $PREFIX_DESTINATION_SAUVE"
    # 
    if [ "$SENS_PROTOCOLE_D" = "directe" ] ; then
      if [ "$PREFIX_SOURCE" = "eval" ] ; then
        Info -v "Sens Directe et eval pour le prefix source "
        # Si la source est local a exe, eval sera le préfixe, on supprime le eval et du coup les cotes
        Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV}\" ] && mkdir -p -m 750 \"${TAPE_DEV}\""
        Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV_REP}\" ] && mkdir -p -m 750 \"${TAPE_DEV_REP}\""
        Execute_Commande_C_Retour -d "echo test | dd obs=8192 of=${TAPE_DEV_REP}/test_write 1>/dev/null 2>&1"
        RC_ECRIT_DEST=$?
        # Nettoie le fichier de test, mais garde les répertoires
        Execute_Commande_C_Retour -d "/bin/rm ${TAPE_DEV_REP}/test_write"
      else
        Info -v "Sens Directe != eval pour le prefix source "
        # Si c'est en directe c'est la machine source qui contacte la destination
        Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV}\" ] && mkdir -p -m 750 \"${TAPE_DEV}\""
        Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV_REP}\" ] && mkdir -p -m 750 \"${TAPE_DEV_REP}\""
        Execute_Commande_C_Retour -d "echo test | dd obs=8192 of=${TAPE_DEV_REP}/test_write 1>/dev/null 2>&1"
        RC_ECRIT_DEST=$?
        # Nettoie le fichier de test, mais garde les répertoires
        Execute_Commande_C_Retour -d "/bin/rm ${TAPE_DEV_REP}/test_write"
      fi
    else
      Info -v "Sens Indirecte - $PREFIX_SOURCE pour le préfixe source "
      # En indirecte c'est la machine exe qui contacte la destination
      Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV}\" ] && mkdir -p -m 750 \"${TAPE_DEV}\""
      Execute_Commande_C_Retour -d "[ ! -d \"${TAPE_DEV_REP}\" ] && mkdir -p -m 750 \"${TAPE_DEV_REP}\""
      Execute_Commande_C_Retour -d "echo test | dd obs=8192 of=${TAPE_DEV_REP}/test_write 1>/dev/null 2>&1"
      RC_ECRIT_DEST=$?
      # Nettoie le fichier de test, mais garde les répertoires
      Execute_Commande_C_Retour -d "/bin/rm ${TAPE_DEV_REP}/test_write"
    fi

    # Contrôle la création du fichier
    if [ "$RC_ECRIT_DEST" -ne 0 ] ; then
      Info -e "L ecriture sur la destination a echoue"
      ERROR="Nok"
      Sortir 15
    fi
  ;;
  *)
    Info -v "Le test d ecriture ne peux pas etre effectue sur des bandes"
  ;;
  esac

  Info -v "ERROR = $ERROR"

  return 0
}

####### Ajout_Tableau_Erreur
## Crée un tableau par erreur.
# Un variable contient toutes les références à ces tableau.
# Chaque tableau d'erreur contient la machine et la partition en cours du traitement au moment de l'erreur
# Elle contient aussi le message d'erreur
# Il est aisé de savoir si des erreurs ont été rencontré et de les afficher.
# Voir la fonction Affiche_Tableau_Erreur
##Important : Pour que cela fonction toute les boucle sur les partitions devront utiliser la variable $COUNT_PART
# Les boucles sur les machines devront utiliser $MACHINE_S
###############################################################
Ajout_Tableau_Erreur() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution d Ajout_Tableau_Erreur $*"

  # message à récupérer pour l'erreur
  typeset MESSAGE_ERREUR="$*"

  if [ -z "$COUNT_TAB_ERREUR" ] ; then
    # 1er erreurs
    COUNT_TAB_ERREUR=0
    # liste des références des erreurs pour le tableau TAB_DETAIL_ERREUR_
    REF_TAB_ERREUR="$COUNT_TAB_ERREUR"
  else
    # liste des références des erreurs pour le tableau TAB_DETAIL_ERREUR_
    REF_TAB_ERREUR="$REF_TAB_ERREUR $COUNT_TAB_ERREUR"
  fi

  # Stockage du message
  eval TAB_DETAIL_ERREUR_$COUNT_TAB_ERREUR[0]=\"$MESSAGE_ERREUR\"

  # Si la variable machine est valorisé, on stoque le nom de machine
  if [ -n "$MACHINE_S" ] ; then
     eval TAB_DETAIL_ERREUR_$COUNT_TAB_ERREUR[1]="$MACHINE_S"

    # Si la variable est valorise, récupère le nom de la partition
    if [ -n "$COUNT_PART" ] ; then
      eval TAB_DETAIL_ERREUR_$COUNT_TAB_ERREUR[2]=\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[0]}
    fi

    # Ajout dans le tableau des machines la référence de la partition si l'erreur est sur une partition
    if [ -n "$COUNT_PART" ] ; then

      eval REF_PART_ERREUR=\"\${CONF_${COUNT_MACHINE_S}[12]}\"
      echo $COUNT_PART
      echo $COUNT_MACHINE_S
      eval CONF_$COUNT_MACHINE_S[12]=\'$REF_PART_ERREUR $COUNT_PART\'
    fi
  fi

  # A FAIRE : ajouter dans le tableau de machine et dans le tableau de partition
  # une référence a ce message d'erreur ou même le message d'erreur.

  # Affiche le message sur l'entrée standard et dans le fichier de log
  Info -e "$MESSAGE_ERREUR"

  Info -v "COUNT_TAB_ERREUR = $COUNT_TAB_ERREUR"
  Info -v "COUNT_PART = $COUNT_PART"
  Info -v "REF_TAB_ERREUR = $REF_TAB_ERREUR"

  # Ajoute a la variable global
  (( COUNT_TAB_ERREUR++ ))

  Info -v "ERROR = $ERROR"
  return 0

}

####### Affiche_Tableau_Erreur
# Afficher les messages d'erreurs stokées par la fonction Ajout_Tableau_Erreur
###############################################################
Affiche_Tableau_Erreur() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Affiche_Tableau_Erreur $*"

  # Affiche le résultat de la détection d'erreurs
  if [ -z "$REF_TAB_ERREUR" ] ; then
    Info -i "Aucune erreur detecte par la fonction"
  else

    Info -e "Il y a eu $COUNT_TAB_ERREUR erreurs"

    # Construit le message en fonction des valeurs trouvées
    for REF in $REF_TAB_ERREUR ; do
      eval typeset MESSAGE=\${TAB_DETAIL_ERREUR_${REF}[0]}
      eval typeset HOST=\${TAB_DETAIL_ERREUR_${REF}[1]}
      eval typeset PART=\${TAB_DETAIL_ERREUR_${REF}[2]}

      #~ Info -e "Le message d'erreur : - $MESSAGE -"
      # Si il n'y a pas de nom de machine et de partition c'est un message de niveau général
      if [ -z $HOST ] && [ -z $PART ] ; then
        Info -e "Niveau General"
      else
        # affiche le nom de machine et de partition si existante
        [ -n $HOST] && Info -e "Pour la machine : - $HOST - "
        [ -n $PART ] && Info -e "Pour la partition : - $PART - "
      fi
       Info -e "Le message d'erreur : - $MESSAGE -"
    done
    Info -v "Aout du flag ERROR=Nok"
    ERROR=Nok
  fi

  Info -v "MESSAGE = $MESSAGE"
  Info -v "REF_TAB_ERREUR = $REF_TAB_ERREUR"
  Info -v "HOST = $HOST"
  Info -v "PART = $PART"

  return 0
}

####### Construction_Commande_Sauvegarde
## Définit les commandes et options utilisées pour sauvegarder
# Construit la commande complète permettant d'effectuer la sauvegarde ##
###############################################################
Construction_Commande_Sauvegarde() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Construction_Commande_Sauvegarde $*"

  typeset INFO_PART=$1

  #~ # Prépare des informations de destination distante
  #~ /bin/cat /dev/null > "$DUMP_LOG_FILE"

  # Définit la commande à utiliser pour la sauvegarde
  # En fonction du type de FS et de l'OS
  # On construit la commandes
  DEVICE_DE_PART=`echo $INFO_PART | awk -F: '{print $1}'`
  POINT_DE_MONTAGE=`echo $INFO_PART | awk -F: '{print $4}'`
  CONVERT_NOM_POINT_2_MONTAGE_LETTRE=`echo $POINT_DE_MONTAGE | sed 's/\//_/g'`

  if [ -n "$PART_TYPE_DE_FS" ] ; then
    case $OS_SOURCE in
    "Linux")
      Info -v "Linux detecte"
      case $PART_TYPE_DE_FS in
      "ext2"|"ext3"|"ext4" )
        Info -v "Type de fichier $PART_TYPE_DE_FS detecte"
        BLOCK_LINUX=1   # Blocks de 1024
        DUMPER=`${PREFIX_SOURCE} '/bin/ls /sbin/dump 2>/dev/null' `
        if [ -z "${DUMPER}" ] ; then
          # commande dump pas installee
          DUMPER=""
          DUMP_CMD=""
          Ajout_Tableau_Erreur "Manque la commande dump"
          ERROR="Nok"
          Info -v "ERROR = $ERROR"
          break
        fi

        # teste le type de destination pour activer ou pas la compression
        case $TYPE_DESTINATION in
        "bande"|"robot"|"changer" )
          # la tape est un device, pas de compression
          # block sector a 32 kilos pour le dump linux
          DUMP_OPTIONS="ubaAf 32"
        ;;
        *)
          # la tape est un fichier, compression avec zlib
          # block sector a 32 kilos pour le dump linux
          DUMP_OPTIONS="ubaAfz9 32"
        ;;
        esac

        DUMP_ESTIME="S"
        RESTORER="/sbin/restore"
        RESTORE_OPTIONS="At"
        export SVGTOC=/usr/sbin/prtvtoc

        if [ "$TYPE_DESTINATION" = "changer" ] ; then
          DUMP_CMD="export RSH=/usr/bin/rsh LANG=C; $DUMPER ${NIVEAU_SAUVEGARDE}${DUMP_OPTIONS} /tmp/${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.${DATE_EXE} - -z9 -F ${BASE_DIR}/BIN/robot.sh $DEVICE_DE_PART"
        else
          DUMP_CMD="export RSH=/usr/bin/rsh LANG=C; $DUMPER ${NIVEAU_SAUVEGARDE}${DUMP_OPTIONS} /tmp/${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.${DATE_EXE} - $DEVICE_DE_PART"
        fi
      ;;
      *)
        # Système de fichier non prévue
        DUMPER=""
        DUMP_CMD=""
        Ajout_Tableau_Erreur "Systeme de fichier inconnu"
        ERROR="Nok"
        Info -v "ERROR = $ERROR"
        break
      ;;
      esac
    ;;
    "SunOS")
      Info -v "SunOS detecte"
      BLOCK_SOLARIS=2   # Blocks de 512
      BLOCK_SOLVXFS=1   # Blocks de 1024
      FSTYP="/usr/sbin/fstyp"

      case $PART_TYPE_DE_FS in
      "ufs")
        Info -v "Type de fichier UFS detecte"
        DUMPER="/usr/sbin/ufsdump"
        if [ "$TYPE_DESTINATION" = "robot" ] && [ "$GESTION_MEDIA" = "box" ] ; then
          # Option l pour autoloader automatiquement et
          # T timeout attente changement de bande
          case $VERSION_OS_SOURCE in
            # option T non supportee en dessous de solaris 8
            5.4|5.5|5.5.1|5.6|5.7)
              DUMP_OPTIONS="ulaf"
            ;;
            *)
              DUMP_OPTIONS="ulTaf 72h"
            ;;
          esac
        else
           DUMP_OPTIONS="uaf"
        fi
        DUMP_ESTIME="S"

        RESTORER="/usr/sbin/ufsrestore"
        RESTORE_OPTIONS="ta"

        # pour Sauvegarde_Informations_Systeme de MACHINE_S
        METASTAT=` ${PREFIX_SOURCE} '/bin/ls /usr/sbin/metastat 2>/dev/null' `
        if [ -z "${METASTAT}" ] ; then
          # ancienne version
          METASTAT=/usr/opt/SUNWmd/sbin/metastat
        fi
        METADB=` ${PREFIX_SOURCE} '/bin/ls /usr/sbin/metadb 2>/dev/null' `
        if [ -z "${METADB}" ] ; then
          # ancienne version
          METADB=/usr/opt/SUNWmd/sbin/metadb
        fi

        DUMP_CMD="LANG=C; $DUMPER ${NIVEAU_SAUVEGARDE}${DUMP_OPTIONS} /tmp/${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.${DATE_EXE} - $DEVICE_DE_PART"
        Info -v "La commande DUMP est : $DUMP_CMD"
      ;;
      "vxfs")
        Info -v "Type de fichier VXFS detecte"
        DUMPER=/usr/sbin/vxdump
          DUMP_OPTIONS=ulf
          RESTORER="cat"
          RESTORE_OPTIONS=""
          DUMP_CMD="LANG=C; $DUMPER ${NIVEAU_SAUVEGARDE}${DUMP_OPTIONS} - $DEVICE_DE_PART"
      ;;
      "zfs")
        Info -v "Type de fichier ZFS detecte"
        DUMPER="zfs send "
        case NIVEAU_SAUVEGARDE in
        0)
          DUMP_OPTIONS=""
        ;;
        *)
          DUMP_OPTIONS="-i"
        ;;
        esac
        RESTORER=""
        RESTORE_OPTIONS=""
        DUMP_ESTIME="zfs"

       # On cherche à savoir si un snapshot est déja existant
        # récupère la liste des snapshots pour le FS en cours de traitement
        SNAP_BASE=`${PREFIX_SOURCE} "zfs list -Ht snapshot 2>/dev/null"  | grep ^${DEVICE_DE_PART}@SVGS_ | awk '{print $1}' | sort | tail -1`
        NOM_SNAP_DU_JOUR="${DEVICE_DE_PART}@$SUFFIX_SNAPSHOT"

        # Pour les sauvegardes incrémentales
        if [ "$NIVEAU_SAUVEGARDE" -ne "0" ] ; then
          # il est nécessaire qu'un snap de base soit déja existant"
          #  En effet la 1er exécution du script doit être full
          if [ -z "$SNAP_BASE" ] ; then
      Ajout_Tableau_Erreur "Manque base pour faire incremental pour $DEVICE_DE_PART"
            Info -a "Impossible de créer une incrementale pour $DEVICE_DE_PART"
            Info -a "Le FS n'a aucun snapshot existant"
            FLAG_ACTION="MANQUE_BASE"
          else
            # Ce flag vas déterminer les options de la commande de sauvegarde
            FLAG_ACTION="INCRE"
            SNAP_A_DETRUIRE="$SNAP_BASE"
            SNAP_A_DETRUIRE_SI_ERREUR="$NOM_SNAP_DU_JOUR"
          fi
        else # sauvegarde full
          # Ce flag vas déterminer les options de la commande de sauvegarde
          FLAG_ACTION=FULL
          SNAP_A_DETRUIRE="$SNAP_BASE"
        fi

        # le snap du jour devient la base
        SNAP_A_DETRUIRE="$SNAP_BASE"
        CMD_CREATION_SNAP="zfs snapshot $NOM_SNAP_DU_JOUR"
        # Si la sauvegarde ne sait pas bien passé, on supprime le snap du jour
        # il ne faut pas prendre en compte une sauvegarde mal fini, ont revient a l'état initial
        SNAP_A_DETRUIRE_SI_ERREUR="$NOM_SNAP_DU_JOUR"

        DUMP_PRE_COM="$CMD_CREATION_SNAP"
        # Crée la commande de sauvegarde
        if [ $FLAG_ACTION = "INCRE" ] ; then
          # Commande incrémentale
          DUMP_CMD="zfs send -i $SNAP_BASE $NOM_SNAP_DU_JOUR"
        else
          # Création de la commande de sauvegarde du snap
          DUMP_CMD="zfs send $NOM_SNAP_DU_JOUR"
        fi

        # A exécuter si la sauvegarde c'est bien passé
        [ -n $SNAP_A_DETRUIRE ] && DUMP_POST_COM="zfs destroy $SNAP_A_DETRUIRE"
      ;;
      *) # Systéme de fichier non prévue
        DUMPER=""
        DUMP_CMD=""
        Ajout_Tableau_Erreur "Systeme de fichier inconnu"
        ERROR="Nok"
        Info -v "ERROR = $ERROR"
      ;;
      esac
    ;;
    *)
      DUMPER=""
      DUMP_CMD=""
      Ajout_Tableau_Erreur "Systeme de fichier inconnu"
      ERROR="Nok"
    ;;
    esac
  else
    DUMPER=""
    DUMP_CMD=""
    Ajout_Tableau_Erreur "Type de FS nom trouve pour $REP_DEMANDER "
    ERROR="Nok"
  fi


  ####################################################################
  ## Construction du nom de fichier de destination
  # Si c'est une bande, le fichier doit être indiqué dans TAPE_DEV
  # Si c'est sur disque il faut construire le nom du fichier
  Info -v "Contruction du fichier de destination"
  case $TYPE_DESTINATION in
  "robot"|"bande"|"changer" )
    FICHIER_DESTINATION=$TAPE_DEV
    Execute_Commande_C_Retour -d "[ -f $TAPE_DEV ]"
    if [ $? -ne 0 ] ; then
      Sortir 24 "Le fichier du lecteur de bande ou du robot n'existe pas !"
    fi
  ;;
  "disk")
    # Construction du suffixe si cela est nécessaire
    case $PART_TYPE_DE_FS in
        "zfs")
          SUFIX_FICHIER_SAV=zfs
        ;;
        *)
          SUFIX_FICHIER_SAV=fsdump
        ;;
        esac
    # Construction du nom de fichier
    NOM_FIC_FSDUMP_PART="`echo $INFO_PART | awk -F: '{print $4}' | sed 's/\//_/g'`.$SUFIX_FICHIER_SAV"
    FICHIER_DESTINATION="${TAPE_DEV_REP}/${MACHINE_S}_${NOM_FIC_FSDUMP_PART}"
  ;;
  esac

  ####################################################################
  # Construction de la commande d'écriture sur la destination
  Info -v "Construction de la commande d'ecriture"
  case $PROTOCOLE_D in
  "ssh"|"rsh")
    Info -v "Protocole distant detecté"
    if [ "$PREFIX_SOURCE" = "eval" ] ; then
      CMD_ECRITURE_D="$PREFIX_DESTINATION_SAUVE dd bs=32k of=$FICHIER_DESTINATION 1>/dev/null 2>&1"
    else
      CMD_ECRITURE_D="$PREFIX_DESTINATION_SAUVE dd bs=32k of=$FICHIER_DESTINATION 1>/dev/null 2>&1"
    fi
  ;;
  *)
    Info -v "Protocole local detecte"
    CMD_ECRITURE_D="$PREFIX_DESTINATION_SAUVE dd bs=32k of=$FICHIER_DESTINATION 1>/dev/null 2>&1"
  ;;
  esac

  Info -v "REP_DEMANDER = $REP_DEMANDER"
  Info -v "PART_TYPE_DE_FS = $PART_TYPE_DE_FS"
  Info -v "DUMP_LOG_FILE = $DUMP_LOG_FILE"
  Info -v "CMD_ECRITURE_D = $CMD_ECRITURE_D"
  Info -v "FICHIER_DESTINATION = $FICHIER_DESTINATION"
  Info -v "OS_SOURCE = $OS_SOURCE"
  Info -v "DUMPER = $DUMPER"
  Info -v "DUMP_OPTIONS = $DUMP_OPTIONS"
  Info -v "PREFIX_SOURCE = $PREFIX_SOURCE"
  Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
  Info -v "RESTORER = $RESTORER"
  Info -v "RESTORE_OPTIONS = $RESTORE_OPTIONS"
  Info -v "DUMP_CMD = $DUMP_CMD"
  Info -v "ERROR = $ERROR"
  Info -v "METASTAT = $METASTAT"
  Info -v "FLAG_ACTION = $FLAG_ACTION"
  Info -v "ERROR = $ERROR"
  Info -v "DUMP_PRE_COM = $DUMP_PRE_COM"
  Info -v "DUMP_POST_COM = $DUMP_POST_COM"
  Info -v "CMD_CREATION_SNAP = $CMD_CREATION_SNAP"
  Info -v "NOM_SNAP_DU_JOUR = $NOM_SNAP_DU_JOUR"
  Info -v "SNAP_A_DETRUIRE = $SNAP_A_DETRUIRE"
  Info -v "SNAP_A_DETRUIRE_SI_ERREUR = $SNAP_A_DETRUIRE_SI_ERREUR"

  return 0

}

######## Controle_Fichier_Rhost
## Ajoute ou Supprime la machine a sauvegarder dans le rhost du tapehost ##
###############################################################
Controle_Fichier_Rhost() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Fichier_Rhost $*"

  case "$1" in
    -a) # Ajout la machine dans le fichier rhost
      shift
      case "$PROTOCLE_D" in
        "rsh")
          if [ "${MACHINE_S}" != "${MACHINE_D}" ] ; then
            # pouvoir se connecter au TAPE_HOST sans mot de passe.
            Execute_Commande_C_Retour -d "sort -u \\$HOME/.rhosts > \\$HOME/.rhosts.save 2>/dev/null"
            Execute_Commande_C_Retour -d "echo \${MACHINE_S} >> \\$HOME/.rhosts 2>/dev/null"
            Execute_Commande_C_Retour -d "echo ${MACHINE_S}.cst.cnes.fr >> \\$HOME/.rhosts 2>/dev/null"
          fi
        ;;
      esac
    ;;
    -s) # Supprime la machine du fichier rhost
      shift
      case "$PROTOCLE_D" in
        *) # Attention ce code n'est pas compatible pour des exécution simultané
          if [ "${MACHINE_S}" != "${MACHINE_D}" ] ; then
            # Il existe forcement un .rhosts.save puisqu'on l'a généré
            # avant le début de la sauvegarde.
            Execute_Commande_C_Retour -d "\"/bin/cp \$HOME/.rhosts.save \$HOME/.rhosts 2>/dev/null\""
          fi
        ;;
      esac
    ;;
  esac

  Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
  Info -v "MACHINE_S = $MACHINE_S"
  Info -v "TAPE_HOST = $TAPE_HOST"
  Info -v "HOME = $HOME"

  return 0
}

######## Controle_Acces_Ecriture
## Effectue un test d ecriture de fichier via la cmd touch   ##
###############################################################
Controle_Acces_Ecriture() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Controle_Acces_Ecriture $*"

  RC=0
  if [ -n "$1" ] ; then
    typeset FILE=$1
    typeset DIR_FILE=`dirname $FILE`

    # Si le répertoire devant héberger le fichier de test n'existe pas on le crée
    if [ ! -d "${DIR_FILE}" ] ; then
      mkdir -p -m 750 ${DIR_FILE}
      Info -v "Creation du repetoire de destination ${DIR_FILE}"
      if [ "$?" -ne 0 ] ; then
        Info -a "La creation du repertoire de destination ${DIR_FILE} a echoue"
        Sortir 21
      fi
    else
      Info -i "Le repertoire ${DIR_FILE} existe"
    fi

    # tentative de création du fichier
    touch $FILE
    if [ "$?" -ne 0 ] ; then
      Info -t "###########################################################"
      Info -t "###########################################################"
      Info -t "ERREUR: Probleme d ecriture du test $FILE                  "
      Info -t "###########################################################"
      Info -t "###########################################################"
      RC=1
    else
      Info -i "Le test d ecriture a reussi : $FILE "
      RC=0
      # Nettoyage du teste"
      rm -f $FILE
    fi
  fi

  Info -v "FILE = $FILE"
  Info -v "DIR_FILE = $DIR_FILE"

  return $RC
}

######## Sauvegarde_Numero_Bande
## Sauvegarde dans un fichier le numéro de bande actuel #
#########################################################
Sauvegarde_Numero_Bande() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Sauvegarde_Numero_Bande $*"

  Info -v "Numero de bande actuel $NUMERO_BANDE"
  echo $NUMERO_BANDE > ${BASE_DIR}/ETC/bande_en_cours

  return 0
}

######## Definir_Commandes_Mt
## Définit des variables contenant les commandes permettant de manipuler les bandes
## La position courante sur la bande est aussi définit dans la variable POSITION_BANDE       ##
###############################################################
Definir_Commandes_Mt() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Definir_Commandes_Mt $*"
  Info -v "TYPE_DESTINATION = $TYPE_DESTINATION"
  case $TYPE_DESTINATION in
    "disk")
      POSITION_BANDE="echo 1"
    ;;
    "bande"|"robot"|"changer") # bande locale ou robot
      OS_DESTINATION=`Execute_Commande_R_Resul -d /bin/uname -s`
      Info -v "OS_DESTINATION = $OS_DESTINATION"

      case "$OS_DESTINATION" in
        "Linux")
          ## Commande de gestion du lecteur de bande de sauvegarde
          MT_COMMAND="/bin/mt"
          MT_STATUS="$MT_COMMAND -f $TAPE_DEV status"
          MT_REWIND="$MT_COMMAND -f $TAPE_DEV rewind"
          MT_EJECT="$MT_COMMAND -f $TAPE_DEV offline"
          MT_EOM="$MT_COMMAND -f $TAPE_DEV eod"
          MT_ASF="$MT_COMMAND -f $TAPE_DEV asf"
          MT_ERASE="$MT_COMMAND -f $TAPE_DEV weof"
          ## récupère la position courante de la bande présente
          POSITION_BANDE="Execute_Commande_R_Resul -d $MT_STATUS | /bin/grep 'File number=' | cut -f1 -d',' | sed -e 's/[A-z= ]//g'"

          # Gestion du cas d'un Robot de sauvegarde
          # le type de destination est bande et le 4eme paramètres est changer
          if [ "$TYPE_DESTINATION" = "changer" ] && [ "$GESTION_MEDIA" = "changer" ] ; then
            MTX_COMMAND="/usr/sbin/mtx"
            # recupere le n° de la bande courrante
            BANDE="Execute_Commande_R_Resul -d $MTX_COMMAND -f $ROBOT_DEV status | /bin/grep 'Data Transfer Element' | cut -d'(' -f2 | cut -d' ' -f3"
          else
            BANDE="printf 1"
          fi
        ;;
        "SunOS")
          MT_COMMAND="/usr/bin/mt"
          MT_STATUS="$MT_COMMAND -f $TAPE_DEV status"
          MT_REWIND="$MT_COMMAND -f $TAPE_DEV rewind"
          MT_EJECT="$MT_COMMAND -f $TAPE_DEV offline"
          MT_EOM="$MT_COMMAND -f $TAPE_DEV eom"
          MT_ASF="$MT_COMMAND -f $TAPE_DEV asf"
          MT_ERASE="$MT_COMMAND -f $TAPE_DEV weof"
          POSITION_BANDE="Execute_Commande_R_Resul -d $MT_STATUS | /bin/grep 'file no=' | cut -f6 -d' '"
          # la destination est un robot et le 4eme paramètre = media
          if [ "$TYPE_DESTINATION" = "robot" ] && [ "$GESTION_MEDIA" = "media" ] ; then
            MTX_COMMAND="$BASE_DIR/BIN/mtx"
            # Recupere le N° de la bande
            BANDE="Execute_Commande_R_Resul -d $MTX_COMMAND -f $TAPE_DEV status | /bin/grep 'Data Transfer Element' | cut -d'(' -f2 | cut -d' ' -f3"
          else
            BANDE="printf 1"
          fi
        ;;
        *)
          POSITION_BANDE="echo Systeme inconnu, pas d'index disponible."
          return 3
        ;;
      esac
    ;;
  esac

  Info -v "POSITION_BANDE = $POSITION_BANDE"
  Info -v "MTX_COMMAND = $MTX_COMMAND"
  Info -v "BANDE = $BANDE"
  Info -v "GZIP = $GZIP"
  Info -v "POSITION_BANDE = $POSITION_BANDE"

  return 0
}

######## Entete
## Génère une entete a la sauvegarde                         ##
###############################################################
Entete() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Entete $*"

  Info -t "=============================================================="
  Info -t "DATE         : `date +%Y%m%d%H%M%S` : `date '+%d/%m/%Y %H:%M:%S'`"
  Info -t "COMMANDE     : $@"
  Info -t "VERSION      : $VERSION"
  Info -t "SERVEUR      : $MACHINE_EXE "
  Info -t "PLATEFORME   : $PLATEFORME "
  Info -t "PERIPHERIQUE : $TAPE_HOST:$TAPE_DEV"
  Info -t "TYPE_ACTION  : $SAVE_TYPE"
  Info -t "POLICE       : $TYPE_ACTION"
  Info -t "NIVEAU       : $NIVEAU_SAUVEGARDE"
  Info -t "LOG          : $LOG_FILE"
  Info -t "=============================================================="

  return 0
}

######## Preparation_Destination
# Prépare les commandes et les variables permettant de joindre la destination
## Permet de vérifier que le lecteur de bande soit disponible      ##
## Positionne le lecteur sur la bande et la position voulue       ##
###############################################################
Preparation_Destination() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Preparation_Destination $*"

  case "$TYPE_DESTINATION" in
  "bande"|"robot"|"changer")  # 0,1 et 5 utilisation d'un lecteur de bande ou robot, local ou distant (idem robot)
    # exécution si une estimation est demande avec "taille" dans $4

    ## Permet de vérifier que le lecteur de bande soit disponible      ##
    # Si ce n'est pas juste une estimation que l'on veut faire
    if [ "$TYPE_SAUVEGARDE" != "taille" ] ; then
      # Si il s'agit d'un robot que le paramètre $4 et media et que $3 et supérieur ou egal à 0 est qu'il est plus petit que MAX_BANDE
      if [ "$TYPE_DESTINATION" = "bande" ] && [ "$GESTION_MEDIA" = "media" ] && [ ! -z "$NUMERO_BANDE" ] && [ "$NUMERO_BANDE" -ge 0 ] && [ "$NUMERO_BANDE" -le $MAX_BANDE ] ; then
        # Cherche le nombre de bande que peut contenir le robot
        STORAGE="`Execute_Commande_R_Resul -d $MTX_COMMAND -f $TAPE_DEV status | /bin/grep 'Storage Element' \
                | tail -1 | sed -e 's/://g' | awk '{ print $3 }'`"
        # Si on demande a passer a la bande suivante
        if [ "$NUMERO_BANDE" = "0" ] ; then
          # Si c'est la dernière, passer à la première, sinon, passer à la suivante
          if [ "`$BANDE`" = "$STORAGE" ] ; then
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $TAPE_DEV unload 2>&1" | tee -a $MTX_LOG_FILE
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $TAPE_DEV load 1 2>&1" | tee -a $MTX_LOG_FILE
          else
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $TAPE_DEV next 2>&1 "  | tee -a $MTX_LOG_FILE
          fi
        else
          # Sinon, charger simplement celle qui est demandée
          if [ "`$BANDE`" != "$3" ] ; then
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $TAPE_DEV unload 2>&1"     | tee -a $MTX_LOG_FILE
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $TAPE_DEV load $NUMERO_BANDE 2>&1 "   | tee -a $MTX_LOG_FILE
          fi
        fi
      fi
      # TYPE_DESTINATION = change 5 n'est pas documenté. On ne devrait jamais passer par ici.
      if [ "$TYPE_DESTINATION" = "changer" ] && [ "$GESTION_MEDIA" = "changer" ] && [ ! -z "$NUMERO_BANDE" ] && [ "$NUMERO_BANDE" -ge 0 ] && [ "$NUMERO_BANDE" -le "$MAX_BANDE" ] ; then
        # Cherche le nombre de bande que peut contenir le robot
        STORAGE="`Execute_Commande_R_Resul -d $MTX_COMMAND -f $TAPE_DEV status | /bin/grep 'Storage Element' \
                | tail -1 | sed -e 's/:.*//g' | awk '{ print $3 }'`"
        # Si on demande a passer a la bande suivante
        if [ "$NUMERO_BANDE" = "0" ] ; then
          # Si c'est la dernière, passer à la première, sinon, passer a la suivante
          if [ "`$BANDE`" = "$STORAGE" ] ; then
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $ROBOT_DEV unload 2>&1" | tee -a $MTX_LOG_FILE
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $ROBOT_DEV load $MIN_BANDE 2>&1 "| tee -a $MTX_LOG_FILE
          else
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $ROBOT_DEV next 2>&1 "| tee -a $MTX_LOG_FILE
          fi
        else
          # Sinon, charger simplement celle qui est demandée
          if [ "`$BANDE`" != "$NUMERO_BANDE" ] ; then
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $ROBOT_DEV unload 2>&1" | tee -a $MTX_LOG_FILE
            Execute_Commande_C_Retour -d "$MTX_COMMAND -f $ROBOT_DEV load $3 2>&1" | tee -a $MTX_LOG_FILE
          fi
        fi

      fi

      if [ -z "`$POSITION_BANDE 2> $ERROR_FILE`" ] ; then
        Info -v "POSITION_BANDE = $POSITION_BANDE"
        Info -t " "
        Info -t "***********************************************************"
        Info -t "ERREUR: Le lecteur $TAPE_DEV de $TAPE_HOST n'est pas dispo "
        Info -t "***********************************************************"
        #~ cat $LOG_FILE >> $ENTETE_FILE
        #~ mv $ENTETE_FILE $LOG_FILE
        ERROR="Nok"
        Info -v "ERROR = $ERROR"
        Sortir 19
      fi
    fi

    # Information pour le mode verbeux
    Info -v "BANDENUM = $BANDENUM"
    Info -v "CODE_BANDENUM = $CODE_BANDENUM"
    Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
    Info -v "POSITION_BANDE = $POSITION_BANDE"

    # Ecrit le code de la bande
    Ecrit_Code_Bande ${CODE_BANDENUM}
    LAST_POSITION_BANDE="`$POSITION_BANDE`"
  ;;
  esac

  FIRST_BANDE=${CODE_BANDENUM}

  Info -v "FIRST_BANDE = $FIRST_BANDE"
  return 0
}

######## Prochain_Code_Bande
## Retourne le prochain code bande non utilisé, puis incrémente le fichier next_bande   ##
###############################################################
Prochain_Code_Bande() {
[ $DEBUG -eq 1 ] && set -x
Info -v "# Execution de Prochain_Code_Bande $*"

  typeset CODEB=`/bin/cat ${BASE_DIR}/ETC/next_bande`
  echo "`expr $CODEB + 1`" > ${BASE_DIR}/ETC/next_bande

return 0
}

######## Ecrit_Code_Bande
## Ecrit sur la bande le code de la bande
## prend en paramètre ${CODE_BANDENUM}               ##
###############################################################
Ecrit_Code_Bande() {
[ $DEBUG -eq 1 ] && set -x
Info -v "# Execution de Ecrit_Code_Bande $*"

  Info -v "Execute_Commande_C_Retour -d \"echo BANDE_CODE:$1 | dd obs=512b conv=sync of=$TAPE_DEV 1>/dev/null 2>&1\""
  Execute_Commande_C_Retour -d "echo BANDE_CODE:$1 | dd obs=512b conv=sync of=$TAPE_DEV 1>/dev/null 2>&1"
}

######## Recherche_Code_Bande
## Recherche et récupère le numéro de référence de la bande     ##
###############################################################
Recherche_Code_Bande() {
[ $DEBUG -eq 1 ] && set -x
Info -v "# Execution de Recherche_Code_Bande $*"

  # Enregistrement le N° de la position (POSITION_BANDE) d'origine avant toute manipulation
  START_POSITION_BANDE="`$POSITION_BANDE`"

  # cherche le numéro de bande à la position de la bande actuelle-1 ou en début de bande
  # Si la position courante > 0, commence la recherche du numéro de bande a la position courante - 1
  if [ "$START_POSITION_BANDE" -gt "0" ] ; then
    Execute_Commande_C_Retour -d "$MT_ASF `expr $START_POSITION_BANDE - 1` 2>/dev/null"
    Lit_Code_Bande
  else
    # Sinon, on rembobine la bande et on le cherche a la position 0 et 1
    Execute_Commande_C_Retour -d "$MT_REWIND 2>/dev/null"
    Lit_Code_Bande
    # Si le code de bande n'ai pas trouve a la position 0 mais qu'il n'était pas vide, cherche a la position 1
    if [ -z "$CODE" ] && [ "`$POSITION_BANDE`" -eq "1" ] ; then
      Lit_Code_Bande
    fi
  fi

  # Si le code n'ai toujours pas trouve, cherche le numéro de bande en fin de bande
  if [ -z "$CODE" ] ; then
    Execute_Commande_C_Retour -d "$MT_EOM 2>/dev/null"
    LASTEST_POSITION_BANDE="`$POSITION_BANDE`"

    # Si le dernier index est >= 3, alors chercher le numéro de bande en fin de bande
    if [ "$LASTEST_POSITION_BANDE" -ge "3" ] ; then
      LASTEST_POSITION_BANDE=`expr $LASTEST_POSITION_BANDE - 1`
      # Position la bande sur la dernière position (fin de bande), puis Recherche le numéro de bande
      Execute_Commande_C_Retour -d "$MT_ASF $LASTEST_POSITION_BANDE 2>/dev/null"
      Lit_Code_Bande

      if [ -z "$CODE" ] ; then
        # Positionne la bande sur l'avant dernière position, puis recherche le numéro de bande
        LASTEST_POSITION_BANDE=`expr $LASTEST_POSITION_BANDE - 1`
        Execute_Commande_C_Retour -d "$MT_ASF $LASTEST_POSITION_BANDE 2>/dev/null"
        Lit_Code_Bande
      fi
    fi
  fi

  # Si le numéro n'a pas été trouve en fin de bande, recherche le numéro en début de bande
  if [ -z "$CODE" ] ; then
    Execute_Commande_C_Retour -d "$MT_REWIND 2>/dev/null"
    Lit_Code_Bande

    # Si le code n'a pas été trouvé à l'index 0 mais qu'il n'était pas vide, cherche a l'index 1
    if [ -z "$CODE" ] && [ "`$POSITION_BANDE`" -eq "1" ] ; then
      Lit_Code_Bande
    fi

    # Si le code n'a pas été trouvé à l'index 1 mais qu'il n'était pas vide, cherche a l'index 2
    if [ -z "$CODE" ] && [ "`$POSITION_BANDE`" -eq "2" ] ; then
      Lit_Code_Bande
    fi
  fi

  # Si le code n'a pas été trouvé, numéroté la bande avec un nouveau numéro
  [ -z "$CODE" ] && CODE="`/bin/cat ${BASE_DIR}/ETC/next_bande`" && Prochain_Code_Bande

  # Repositionnement à l'index avant toute manipulation
  Execute_Commande_C_Retour -d "$MT_ASF $START_POSITION_BANDE 2>/dev/null"

return 0
}

######## Lit_Code_Bande
## Lit le secteur sur lequel la bande est positionné et retourne le BANDE_CODE si il le trouve          ##
###############################################################
Lit_Code_Bande() {
[ $DEBUG -eq 1 ] && set -x
Info -v "# Execution de Lit_Code_Bande $*"

  # Lit 1 secteur sur la bande
  CODE="`Execute_Commande_R_Resul -d \"dd if=$TAPE_DEV count=1 2>/dev/null\"`"
  CODE="`echo ${CODE} | /bin/grep '^BANDE_CODE:' | cut -f2 -d:`"
}

######## Pre_Traitement_Sources
## Réuni et déduit les informations sur les machines devant être sauvegardé
###############################################################
Pre_Traitement_Sources() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Pre_Traitement_Sources $*"

    # Utile que pour ZFS
    DATE_SNAPSHOT=$DATE_EXE
    SUFFIX_SNAPSHOT="SVGS_${DATE_SNAPSHOT}"

    typeset COUNT_MACHINE_S=0
    ####################################################################
    ## MACHINE_S
    # Construction des tableaux d'information des sources
    # Découpe la LST_SAUVE en liste de machine avec les partitions à sauvegarder
    for MACHINE_S_ET_PART in `echo $LST_SAUVE |sed -e 's/ //g'|awk 'BEGIN {RS =":"} /^[^#]*[a-zA-Z0-9]+/ {print $0}'`; do

      ###################################
      # Recupere le nom de machine source
      MACHINE_S="`echo $MACHINE_S_ET_PART|cut -f1 -d,`"
      # Le nom de machine sera toujours sont vrai nom
      if [ "$MACHINE_S" = "localhost" ] ; then
        MACHINE_S="$MACHINE_EXE"
      fi

      Info -v "#########################################################"
      Info -v "## PRETRAITEMENT POUR $MACHINE_S                #########"
      Info -v "#########################################################"

      # Récupère les informations intéressante sur la machine en cours
      LOG_FILE_MACHINE_S="${LOG_FILE}_${MACHINE_S}"
      LST_LOG_TO_SEND="$LST_LOG_TO_SEND $LOG_FILE_MACHINE_S"
      RSH_OPTION="`echo $MACHINE_S_ET_PART|cut -f2 -d,`"
      RSH_ARGS="`echo $MACHINE_S_ET_PART|sed -e 's/_/ /g'|cut -f3 -d,`"
      LST_PARTITIONS="`echo $MACHINE_S_ET_PART| cut -f4- -d, | sed 's/,/ /g'`"

      ## FONCTION A AJOUTER ICI
      # A jouter des FS automatiquement
      # ex: contrôler que / /usr /var /lib soit prise en compte si ils sont des points de montage car se sont forcement des partitions vitales
      # ex2: ajouter les points de montage qui ne sont pas dans la liste et qui commence par /usr /var /lib : /usr sera prix en compte mais /usr/local aussi si il est sur un autre FS
      # ex3: si partition commence par GROUP=VG, on détecte tous les points de montages du VG ou POOL
      # Il suffira d'ajouter la liste des points de montage à la variable LST_PARTITIONS

      #####################
      ## Détermine les informations nécessaire pour atteindre la machine à sauvegarder
      # Si RSH_OPTION est vide, par défaut, c'est du rsh
      # Prefixe les commandes pour contacter la machine a sauvegarder
      # Si la sauvegarde est locale, retourner eval
      if [ "$MACHINE_S" = "$MACHINE_EXE" ] ; then
        MACHINE_S="$MACHINE_EXE"
        RSH_OPTION=""
        RSH_ARG=""
        PREFIX_SOURCE="eval"
      else
        [ -z "$RSH_OPTION" ] && RSH_OPTION="ssh"
        PREFIX_SOURCE="$RSH_OPTION $RSH_ARGS $MACHINE_S"
      fi

      OS_SOURCE=`${PREFIX_SOURCE} /bin/uname -s`
      VERSION_OS_SOURCE=` ${PREFIX_SOURCE} /bin/uname -r`

      # Reference la machine en cours
      eval TAB_LST_MACHINES[$COUNT_MACHINE_S]=$MACHINE_S
      LIST_REF_MACHINES_S="$LIST_REF_MACHINES_S $COUNT_MACHINE_S"

      # Teste de connexion à la machine devant être sauvegardé
      Controle_Connexion -s

      Info -v "MACHINE_S = $MACHINE_S"
      Info -v "LOG_FILE_MACHINE_S = $LOG_FILE_MACHINE_S"
      Info -v "LST_LOG_TO_SEND = $LST_LOG_TO_SEND"
      Info -v "RSH_OPTION = $RSH_OPTION"
      Info -v "RSH_ARGS = $RSH_ARGS"
      Info -v "LST_PARTITIONS = $LST_PARTITIONS"
      Info -v "CLIENT_OK = $CLIENT_OK"
      Info -v "OS_SOURCE = $OS_SOURCE"
      Info -v "VERSION_OS_SOURCE = $VERSION_OS_SOURCE"
      Info -v "PREFIX_SOURCE = $PREFIX_SOURCE"
      Info -v "COUNT_MACHINE_S = $COUNT_MACHINE_S"

      # Peuple le tableau avec les informations que nous venons de trouver pour la machine courante
      eval CONF_${COUNT_MACHINE_S}[0]=\"${MACHINE_S}\"
      eval CONF_${COUNT_MACHINE_S}[1]=\"${LOG_FILE_MACHINE_S}\"
      eval CONF_${COUNT_MACHINE_S}[2]=\"${LST_LOG_TO_SEND}\"
      eval CONF_${COUNT_MACHINE_S}[3]=\"${RSH_OPTION}\"
      eval CONF_${COUNT_MACHINE_S}[4]=\"${RSH_ARGS}\"
      eval CONF_${COUNT_MACHINE_S}[5]=\"${LST_PARTITIONS}\"
      eval CONF_${COUNT_MACHINE_S}[6]=\"${CLIENT_OK}\"
      eval CONF_${COUNT_MACHINE_S}[7]=\"${OS_SOURCE}\"
      eval CONF_${COUNT_MACHINE_S}[8]=\"${VERSION_OS_SOURCE}\"
      eval CONF_${COUNT_MACHINE_S}[9]=\"${PREFIX_SOURCE}\"
      #~ eval CONF_${COUNT_MACHINE_S}[10]=\"\"
      # Messsage si erreur au niveau machine
      #~ eval CONF_${COUNT_MACHINE_S}[11]=\"\"
      #~ eval CONF_${COUNT_MACHINE_S}[12]=\"\"

      # Ajoute a la liste des log celui de la machine courante
      eval TAB_GENERAL[0]=\"${LST_LOG_TO_SEND}\"

      # Si test non réussi passé la machine suivante.
      # il faudra parser ce tableau a la fin pour indiqué si il y a des erreurs
      # OK si $CLIENT_OK" -eq 1
      if [ $CLIENT_OK -ne 1 ] ; then
        CONF_${COUNT_MACHINE_S}[11]="Impossible de ce connecter a : $MACHINE_S"
        Ajout_Tableau_Erreur "Impossible de ce connecter a : $MACHINE_S"
        continue
      fi

      ##################################################################
      ## DESTINATION
      ## Construction de la variable PREFIX_DESTINATION
      # Elle contient la commande permettant d'accéder à la destination
      Construction_Prefix_Destination
      # Definit les commande en fonction de l'OS et le type de sauvegarde
      Definir_Commandes_Mt
      # Test de connexion a la destination
      Controle_Connexion -d
      # test un écriture sur la destination
      Controle_Ecriture_Destination

      # Ajout dans le tableau de la machine les informations sur la destination
      eval CONF_${COUNT_MACHINE_S}[13]=\"$PREFIX_DESTINATION\"
      eval CONF_${COUNT_MACHINE_S}[14]=\"$SENS_PROTOCOLE_D\"
      eval CONF_${COUNT_MACHINE_S}[15]=\"$PREFIX_DESTINATION_SAUVE\"
      eval CONF_${COUNT_MACHINE_S}[16]=\"$TAPE_DEV_REP\"
      eval CONF_${COUNT_MACHINE_S}[17]=\"$OS_DESTINATION\"

      ##################################################################
      ## PARTITIONS

      # Pour calculer le cumule partition sur une machine
      TAILLES_PARTITIONS_MACHINE_S=0
      # Initialisation du compteur a 0
      typeset COUNT_PART=0
      # Récupère les informations de chaque chemin indique comme étant à sauvegarder
      # Les informations sont ranger dans des tableaux qui pourrons être exploites
      for PART in $LST_PARTITIONS ; do

        # Vide les variables de la partition traitée précédemment
        # Cela permet de ne pas récupérer les informations d'une autre partition
        DUMP_CMD="" ; INFO_PART="" ; DUMPER="" ; DUMP_OPTIONS=""
        RESTORER="" ; RESTORE_OPTIONS="" ; DUMP_ESTIME="" ; FLAG_ACTION=""
        CMD_CREATION_BASE_SNAP="" ; SNAP_A_SAUVEGARDER="" ; DUMP_CMD=""
        SNAP_A_DETRUIRE="" ; ESTIMATION_MO=0 ; FLAG_SAUVEGARDE=""
        SNAP_A_DETRUIRE_SI_ERREUR="" ;  DUMP_POST_COM=""; DUMP_PRE_COM=""
        CMD_CREATION_SNAP="" ; NOM_SNAP_DU_JOUR=""

        # Définit le type de FS, le Point de montage, partition logique ou physique attache au point de montage
        # valorise la variable de la façon suivante :
        # INFO_PART="${FILE_SYSTEM}:${TAILLE_K}:${ESPACE_UTILISE}:${POINT_DE_MONTAGE}:${TAILLE_BLOCK}:${PART_TYPE_DE_FS}:$REP_DEMANDER"
        Detecte_Information_Fs $PART
        if [ $? -ne 0 ] ; then
          Ajout_Tableau_Erreur "Impossible de detreminer le type de FS de cette partition"
          continue
        fi
        # Ajoute les informations au tableau

        # Créer une référence des partitions par machine
        eval PART_REF_${COUNT_MACHINE_S}[$COUNT_PART]=$COUNT_PART

        # Detecte_Information_Fs renvoie :
        #INFO_PART="${FILE_SYSTEM}:${TAILLE_K}:${ESPACE_UTILISE}:${POINT_DE_MONTAGE}:${TAILLE_BLOCK}:${PART_TYPE_DE_FS}:$REP_DEMANDER"
        DEVICE_DE_PART=`echo $INFO_PART | awk -F: '{print $1}'`
        PART_TAILLE=`echo $INFO_PART | awk -F: '{print $2}'`
        PART_ESPACE_UTILSE=`echo $INFO_PART | awk -F: '{print $3}'`
        PART_POINT_DE_MONTAGE=`echo $INFO_PART | awk -F: '{print $4}'`
        PART_TAILLE_EN_BLK=`echo $INFO_PART | awk -F: '{print $5}'`
        PART_TYPE_DE_FS=`echo $INFO_PART | awk -F: '{print $6}'`
        PART_REP_DEMANDE=`echo $INFO_PART | awk -F: '{print $7}'`

        Info -v "#########################################################"
        Info -v "## PT POUR LE POINT DE MONTAGE  $PART_POINT_DE_MONTAGE #########"
        Info -v "#########################################################"

        # on stocke les valeurs trouvées dans le tableau de la partition en cours de traitement
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[0]=\"$DEVICE_DE_PART\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[1]=\"$PART_TAILLE\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[2]=\"$PART_ESPACE_UTILSE\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[3]=\"$PART_POINT_DE_MONTAGE\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[4]=\"$PART_TAILLE_EN_BLK\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[5]=\"$PART_TYPE_DE_FS\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[6]=\"$PART_REP_DEMANDE\"



        ###########################################
        ### Contrôle les répertoires à sauvegarder devant être ignoré
        ## Les répertoires devant être ignoré sont :
        #   -le FS hébergeant le répertoire est déjà marqué a sauvegardé car un autre répertoire déjà traité est hébergé par le même FS
        #   -Le répertoire indiqué n'existe pas
        # Détection si le FS est déjà marqué pour être sauvegardé, pour un autre répertoire a sauvegarder
        # explication : c'est un répertoire qui est indiqué dans le fichier de configuration
        # si / et /usr ne sont sur le même FS, le FS sera sauvegardé 2 fois
        # Si on contrôle que le DEVICE n'est pas déjà dans les devices à sauvegarder,
        # ajout d'un warning indiquer que ce répertoire sera pas pris en compte car déjà sauvegardé
        Info -v "Controle des doublons de FS"
        DEVICE_EN_COURS=`echo $INFO_PART | awk -F: '{print $1}'`
        eval LST_DEVICE_A_SAUV=\${TAB_DEVICE_A_SAUVEGARDER_${COUNT_MACHINE_S}[@]}
        FLAG_SAUVEGARDE="SAVE"
        for DEVICE_A_BACKUP in ${LST_DEVICE_A_SAUV} ; do
          if [ "$DEVICE_A_BACKUP" = "$DEVICE_EN_COURS" ] ; then
            Info -a "Repertoire non prise en compte car deja sauvegarde"
            FLAG_SAUVEGARDE="IGNORE"
            break
          fi
        done

        # Le répertoire n'existe pas. Ceux-ci a été détecté pendant la découverte du type de FS
        if [ $PART_TYPE_DE_FS = "IGNORE" ] ; then
          Info -a "Le repertoire n existe pas il sera ignore"
          FLAG_SAUVEGARDE="IGNORE"
        fi

        ## Si c'est ignorer il ne faut pas faire c'est tache
        if [ "$FLAG_SAUVEGARDE" != "IGNORE" ] ; then
          ################################################################
          ## Constuit les commandes de sauvegarde de pre-sauvegarde et post sauvegarde
          Construction_Commande_Sauvegarde $INFO_PART

          #################################################
          ## Calculer l'espace nécessaire pour la sauvegarde du FS
          # Déterminer la commande à utiliser pour le calcul
          # Si la commande est de type dump
          # On utiliser dump pour évaluer
          # Si le type de FS est ZFS
          # On utilise df pour une full
          Estime_Taille_Partition $PART
        fi

        # Ajout des information
        eval TAB_DEVICE_A_SAUVEGARDER_${COUNT_MACHINE_S}[$COUNT_PART]="$DEVICE_EN_COURS"

        # Cumul les résultats trouvé pour avoir le total par machine
        eval ANCIEN_VAL_CUMUL_MACHINE_S=\"\${CONF_${COUNT_MACHINE_S}[10]}\"
        [ -z "$ANCIEN_VAL_CUMUL_MACHINE_S" ] && ANCIEN_VAL_CUMUL_MACHINE_S=0
        eval CONF_${COUNT_MACHINE_S}[10]=\"`expr  $ANCIEN_VAL_CUMUL_MACHINE_S + $ESTIMATION_MO`\"

        ## Définit en fonction du type de FS et de l'OS la commande a utiliser pour sauvegarder ce FS
        # ajout des informations dans le tableau de la partition en cours de traitement
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[7]=\"${DUMPER}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[8]=\"${DUMP_OPTIONS}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[9]=\"${RESTORER}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[10]=\"${RESTORE_OPTIONS}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[11]=\"${DUMP_ESTIME}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[12]=\"${FLAG_ACTION}\" # si "MANQUE_BASE" ne peux faire l'incrementale
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[13]=\"${CMD_CREATION_SNAP}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[14]=\"${NOM_SNAP_DU_JOUR}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[15]=\"${DUMP_CMD}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[16]=\"${SNAP_A_DETRUIRE}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[17]=\"${ESTIMATION_MO}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[18]=\"${FLAG_SAUVEGARDE}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[19]=\"${DUMP_PRE_COM}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[20]=\"${DUMP_POST_COM}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[21]=\"${SNAP_A_DETRUIRE_SI_ERREUR}\"
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[22]=\'${CMD_ECRITURE_D}\'
        eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[23]=\"${FICHIER_DESTINATION}\"
        #~ eval PART_${COUNT_MACHINE_S}_${COUNT_PART}[24]=\"${}\"

        Info -v "INFO_PART = $INFO_PART"
        Info -v "FLAG_SAUVEGARDE = $FLAG_SAUVEGARDE"
        Info -v "DEVICE_EN_COURS = $DEVICE_EN_COURS"
        Info -v "COUNT_PART = $COUNT_PART"
        Info -v "DEVICE_DE_PART = $DEVICE_DE_PART"
        Info -v "PART_TYPE_DE_FS = $PART_TYPE_DE_FS"
        Info -v "PART_REP_DEMANDE = $PART_REP_DEMANDE"
        Info -v "DUMP_PRE_COM = $DUMP_PRE_COM"
        Info -v "DUMP_POST_COM = $DUMP_POST_COM"

        (( COUNT_PART++ ))

      done

      # NIVEAU MACHINE


      #~ # Ajoute l'autorisation dans le fichier .rhost
      #~ Controle_Fichier_Rhost -a
      #~ # Fin boucle partition
      #~ # restaure le fichier .rhost d'origine
      #~ Controle_Fichier_Rhost -s

      # Cumul les estimations par machine. On obtient le cumul total de toute les machines a la fin de la boucle
      [ -z $CUMUL_ESTIMATION_TOUTE_MACHINE_S ]  && CUMUL_ESTIMATION_TOUTE_MACHINE_S=0
      eval VAL_CUMUL_MACHINE_S_EN_COURS=\"\${CONF_${COUNT_MACHINE_S}[10]}\"
      eval CUMUL_ESTIMATION_TOUTE_MACHINE_S=\"`expr $VAL_CUMUL_MACHINE_S_EN_COURS  + $CUMUL_ESTIMATION_TOUTE_MACHINE_S `\"
      eval TAB_GENERAL[1]=\"${CUMUL_ESTIMATION_TOUTE_MACHINE_S}\"
      eval Info -i "La taille total estimé pour la machine - \${CONF_${COUNT_MACHINE_S}[0]}- est de : \${CONF_${COUNT_MACHINE_S}[10]}"
      (( COUNT_MACHINE_S++ ))
    done
    Info -i "La taille total estimé est  : ${TAB_GENERAL[1]}"

    Info -v "#########################################################"
    Info -v "## FIN DES PRE-TRAITEMENT                       #########"
    Info -v "#########################################################"
    # Fin boucle machine

return 0
}

######## Execute_Test
# Execute le choix test dans la configuration
# Actuellement ne fait qu'un statu sur le lecteur de bande
###############################################################
Execute_Test() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Execute_Test $*"

  typeset RETOUR_CMD
# Exécution des taches d'éjection sans calcul de la taille
  #~ Entete $0 $TYPE_ACTION
  Info -t "###############################################################"
  # ici on perd tous les Log du début d'exécution sauf en mode verbeux.
  # Si on supprime le test de Verbosité sur la ligne suivante, on a bien l'entete au début et les log du début d'exécution ne sont pas perdu

  #~ [ "$VERBOSE" -eq 1 ] && cat $LOG_FILE >> $ENTETE_FILE
  #~ mv $ENTETE_FILE $LOG_FILE
  Execute_Commande_C_Retour -d "$MT_STATUS 2>&1"
  RETOUR_CMD=$?

  return $RETOUR_CMD
}

######## Sauvegarde_Informations_Systeme
# liste les informations systèmes et les sauvegardes
###############################################################
Sauvegarde_Informations_Systeme() {
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Sauvegarde_Informations_Systeme $*"

  Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"

  case $PROTOCOLE_D in
    "local"|"ssh"|"rsh") # local nfs ssh
      case ${OS_SOURCE} in
        "Linux")
          Info -v "Linux detecte"
          (
          $ECHO "--- Machine :"
          ${PREFIX_SOURCE} "/bin/uname -a"
          $ECHO "--- Fichier /etc/fstab"
          ${PREFIX_SOURCE} "/bin/cat /etc/fstab"
          $ECHO "--- Label des Mount"
          ${PREFIX_SOURCE} "/bin/mount -l 2>/dev/null"
          $ECHO "--- df"
          ${PREFIX_SOURCE} "/bin/df -Ph -t ext2 -t ext3 -t ext4"
          $ECHO "--- Partionnement"
          $ECHO "-- fdisk -l"
          ${PREFIX_SOURCE} /sbin/fdisk -l 2>/dev/null
          $ECHO "-- sfdisk -d"
          ${PREFIX_SOURCE} /sbin/sfdisk -d 2>/dev/null
          $ECHO "--- Liste des partition avec parted"
          ${PREFIX_SOURCE} "parted -l 2>/dev/null"
          $ECHO "-- vgcfgbackup"
          ${PREFIX_SOURCE} /sbin/lvm vgcfgbackup -f lvmconf_${MACHINE_S}_%s 2>/dev/null
          ${PREFIX_SOURCE} /bin/cat lvmconf_${MACHINE_S}_*  2>/dev/null
          $ECHO "--- Fichier grub.conf ou lilo.conf"
          ${PREFIX_SOURCE} /bin/cat /boot/grub/grub.conf /etc/lilo.conf 2>/dev/null

          $ECHO "--- config_save"
          $EGREP -v "^$|^#"   ${BASE_DIR}/ETC/config_save
          $ECHO "--- ifconfig"
          ${PREFIX_SOURCE} '/sbin/ifconfig -a'  2>/dev/null
          $ECHO "---- netstat les routes"
          ${PREFIX_SOURCE} 'netstat -rn'

          ) > ${LOGS_DIR}/${DATE_EXE}_${MACHINE_S}_partition
        ;;
        "SunOS")
          Info -v "Solaris detecte"
          (
          $ECHO "--- Machine : "
          ${PREFIX_SOURCE} /bin/uname -a
          $ECHO "--- Fichier /etc/vfstab"
          ${PREFIX_SOURCE} /bin/cat /etc/vfstab
          $ECHO
          $ECHO "--- metastat"
          METASTATP=` ${PREFIX_SOURCE} $METASTAT -p 2>/dev/null `
          if [ -n "$METASTATP" ] ; then
            $ECHO "--- metastat -p"
            $ECHO "$METASTATP"
            $ECHO "--- metadb -i"
            ${PREFIX_SOURCE} ${METADB} -i
            $ECHO
          fi

          $ECHO "--- DISQUES et PARTITIONS : "
          # Compte le nombre de disque
          typeset NB_DISK=`$PREFIX_SOURCE " echo -e \"disk\\n\" | format 2>/dev/null" | grep '/pci' | wc -l`
          # Les références commencent a 0 , donc le dernière index est le nombre - 1
          DERNIER_REF_DISK=`expr $NB_DISK - 1`

          typeset REF_DISK_EN_COURS=0
          # On boucle sur chaque référence de disque et on liste les partitions
          while [ "$REF_DISK_EN_COURS" -eq "$DERNIER_REF_DISK" ] ; do
            echo "partitionnement du Disk $REF_DISK"
            $PREFIX_SOURCE "echo -e \"disk\n${REF_DISK_EN_COURS}\npartition\nprint\n\" | format 2>/dev/null "| grep -v change | grep ^' *[0-7]' | tail -8
            REF_DISK_EN_COURS=`expr $REF_DISK_EN_COURS + 1`
          done

          # Contrôle si ZFS
          # Pour chaque file system recherche le device associé
          # Puis affiche le prtvtoc de tous les disques utilisés
          ALL_DEV=""
          for FS in ` ${PREFIX_SOURCE} $DF -k -F ufs | $AWK 'NR>1{ printf "%s ", $6}' `
          do
            $ECHO
            DEV=` ${PREFIX_SOURCE} $DF -k $FS | $AWK -F/ 'NR>1{ printf "%s", \$3}'`

            case "$DEV" in
              "md")
                #$ECHO "avec SVM"
                METAVDEV=` ${PREFIX_SOURCE} $DF -k $FS | $AWK  'NR>1{ split($1,TAB, "/"); printf "%s", TAB[5] }' `
                $ECHO "--- Partition $FS sur metadevice $METAVDEV"

                # tout ce qui commence par un "c" est un DEVICE
                DEVICE=` ${PREFIX_SOURCE} $METASTAT -p | $AWK '\
                        {
                        for (I=1; I<=NF; I+=1) {
                          if ( index ($I, "c") == 1 ) {
                            printf "%s ",  $I
                          }
                        }
                          }' `

                ALL_DEV=` $ECHO "$ALL_DEV $DEVICE" `
              ;;
              "dsk")
                #$ECHO "sans SVM"
                DEVICE=` ${PREFIX_SOURCE} $DF -k $FS | $AWK  'NR>1{ split($1,TAB, "/"); printf "%s", TAB[4] }' `
                ALL_DEV=` $ECHO "$ALL_DEV $DEVICE" `
              ;;
              *)
               :
              ;;
              # Fin du case dev
            esac


          # fin chaque partition
          done

          for D in ` $ECHO  $ALL_DEV | $TR " " "\n" | $AWK '{split($1,TAB,"s"); printf "%ss2\n", TAB[1 ]}' | $SORT -u `
          do
            $ECHO "--- Partitionnement du disque $D"
            ${PREFIX_SOURCE} "prtvtoc -s /dev/rdsk/$D 2>/dev/null"
          done

          # information pour reconstruite ZFS
          $ECHO "--- Information ZFS"
          ZFS_CMD=`type -p zfs`
          ZPOOL_CMD=`type -p zpool`
          [ -n $ZFS_CMD ] && zfs get -rp all
          [ -n $ZPOOL_CMD ] && zpool history


          $ECHO "--- config_save"
          $EGREP -v "^$|^#"   ${BASE_DIR}/ETC/config_save
          $ECHO "--- ifconfig"
          ${PREFIX_SOURCE} '/sbin/ifconfig -a'  2>/dev/null
          $ECHO "---- netstat les routes"
          ${PREFIX_SOURCE} 'netstat -rn'
          $ECHO "--- eeprom"
          ${PREFIX_SOURCE} '/usr/platform/`/bin/uname -i`/sbin/eeprom'

          ) > ${LOGS_DIR}/${DATE_EXE}_${MACHINE_S}_partition
        ;;
        # Fin du case de chaque OS
      esac
      ;;
    # Fin robot
    esac
} # fin Sauvegarde_Informations_Systeme

######## Affiche_Tableaux
# Imprime tous les tableaux avec leurs valeurs
# Il est possible de ce référer à ce code pour connaître la syntaxe permettant de récupérer les informations des tableaux
# Attention
###############################################################
Affiche_Tableaux(){
[ "$DEBUG" = "1" ] && set -x
Info -v "# Execution de Affiche_Tableaux $*"

  echo " Affichage du tableau général"
  eval echo \" le nom ds fichier de log machine :${TAB_GENERAL[0]}\"
  eval echo \"La taille de l estimation total : ${TAB_GENERAL[1]}\"
  #~ eval echo \"${TAB_GENERAL[2]}\"

  for REF_MACHINE_S in  ${LIST_REF_MACHINES_S} ; do

    # Remarque : si utilisation de echo -e et de \t pour insérer les tabulations

    ## Les des machines et des information associe
    echo "############################### $MACHINE_S"
    echo "La machine en cours de traitement est $MACHINE_S"
    echo "Sa configuration est :"
    eval echo -e \"\\tNom de machine : \${CONF_${REF_MACHINE_S}[0]}\"
    eval echo -e \"\\tSont fichier de log est  : \${CONF_${REF_MACHINE_S}[1]}\"
    eval echo -e \"\\tLes fichiers de log devant etre envoyé sont  : \${CONF_${REF_MACHINE_S}[2]}\"
    eval echo -e \"\\tLes informations pour accerder à la machine sont  : \${CONF_${REF_MACHINE_S}[3]}\"
    eval echo -e \"\\tLes options necesaire a la connexion sont  : \${CONF_${REF_MACHINE_S}[4]}\"
    eval echo -e \"\\tLa liste des partitions devant etre sauvegarder  : \${CONF_${REF_MACHINE_S}[5]}\"
    eval echo -e \"\\tLe resultat du teste de connexion a la machine devant etre sauvegarder  : \${CONF_${REF_MACHINE_S}[6]}\"
    eval echo -e \"\\tL OS de la machine est : \${CONF_${REF_MACHINE_S}[7]}\"
    eval echo -e \"\\tLa version de l OS est : \${CONF_${REF_MACHINE_S}[8]}\"
    eval echo -e \"\\tLe prefix permettant d executer des commandes sur la machine est : \${CONF_${REF_MACHINE_S}[9]}\"
    eval echo -e \"\\tL estimation de la sauvegarde pour cette machine est de  : \${CONF_${REF_MACHINE_S}[10]} Mo\"
    eval echo -e \"\\tSi il y a eux des erreurs sur la machine : \${CONF_${REF_MACHINE_S}[11]}\"
    eval echo -e \"\\tLes partition ayant eu des erreurs lors de leur sauvegarde sont : \${CONF_${REF_MACHINE_S}[12]}\"
    eval echo -e \"\\tLe prefixe pour la destination est : \${CONF_${REF_MACHINE_S}[13]}\"
    eval echo -e \"\\tLa machine client acced a la destination de façon : \${CONF_${REF_MACHINE_S}[14]}\"
    eval echo -e \"\\tLe prefixe de destination qui servira pour construire la commande de sauvegarde est : \${CONF_${REF_MACHINE_S}[15]}\"
    eval echo -e \"\\tLe repertoire de destination est : \${CONF_${REF_MACHINE_S}[16]}\"
    eval echo -e \"\\tLe le systeme d exploitation de la destination est : \${CONF_${COUNT_MACHINE_S}[17]}

    ## Liste les partitions
    eval echo -e \"\\t\\tListes des partitions de la machine $MACHINE_S : \${CONF_${REF_MACHINE_S}[5]}\"
    echo -e "\t\t########## ou de cette facon ################"
    eval LIST_PART_REF_MACHINE_S=\${PART_REF_${REF_MACHINE_S}[@]}
    for REF_PART in $LIST_PART_REF_MACHINE_S ; do
      eval echo -e \"\\t\\t\${PART_${REF_MACHINE_S}_${REF_PART}[0]}\"
    done

    ## Liste les informations de chaque partition
    eval LIST_PART_REF_MACHINE_S=\${PART_REF_${REF_MACHINE_S}[@]}
    for REF_PART in $LIST_PART_REF_MACHINE_S ; do
      eval echo -e \"\\t\\t\\t"#######" Detail de la partitions : \${PART_${REF_MACHINE_S}_${REF_PART}[0]} "######"\"
      eval echo -e \"\\t\\t\\tLe device qui heberge le repertoire : \${PART_${REF_MACHINE_S}_${REF_PART}[0]}\"
      eval echo -e \"\\t\\t\\tLa taille en Ko de cette partition : \${PART_${REF_MACHINE_S}_${REF_PART}[1]}\"
      eval echo -e \"\\t\\t\\tL espace utilise de cette partition : \${PART_${REF_MACHINE_S}_${REF_PART}[2]}\"
      eval echo -e \"\\t\\t\\tLe point de montage de cette partition : \${PART_${REF_MACHINE_S}_${REF_PART}[3]}\"
      eval echo -e \"\\t\\t\\tLa taille en block de la partition : \${PART_${REF_MACHINE_S}_${REF_PART}[4]}\"
      eval echo -e \"\\t\\t\\tLe type de FS de la partition : \${PART_${REF_MACHINE_S}_${REF_PART}[5]}\"
      eval echo -e \"\\t\\t\\tLa demande concerne le repertoire : \${PART_${REF_MACHINE_S}_${REF_PART}[6]}\"
      eval echo -e \"\\t\\t\\tLa commande pour sauvegarder le FS : \${PART_${REF_MACHINE_S}_${REF_PART}[7]}\"
      eval echo -e \"\\t\\t\\tles options de la commande pour sauvegarder : \${PART_${REF_MACHINE_S}_${REF_PART}[8]}\"
      eval echo -e \"\\t\\t\\tla commande de restauration : \${PART_${REF_MACHINE_S}_${REF_PART}[9]}\"
      eval echo -e \"\\t\\t\\tLes options pour la commande de restauration : \${PART_${REF_MACHINE_S}_${REF_PART}[10]}\"
      eval echo -e \"\\t\\t\\tL information pour effectuer l estimation : \${PART_${REF_MACHINE_S}_${REF_PART}[11]}\"
      eval echo -e \"\\t\\t\\tLa partition est maquer pour l action suivante : \${PART_${REF_MACHINE_S}_${REF_PART}[12]}\"
      eval echo -e \"\\t\\t\\tZFS : La commande de création du snapshot du jour est : \${PART_${REF_MACHINE_S}_${REF_PART}[13]}\"
      eval echo -e \"\\t\\t\\tZFS: Le nom du snapshot du jour est : \${PART_${REF_MACHINE_S}_${REF_PART}[14]}\"
      eval echo -e \"\\t\\t\\tLa commande qui sera executer pour effectuer la sauvegarde : \${PART_${REF_MACHINE_S}_${REF_PART}[15]}\"
      eval echo -e \"\\t\\t\\tZFS: Nom du snapshot devant etre supprimer : \${PART_${REF_MACHINE_S}_${REF_PART}[16]}\"
      eval echo -e \"\\t\\t\\tL estimation en Mo de l espace necessaire est de : \${PART_${REF_MACHINE_S}_${REF_PART}[17]}\"
      eval echo -e \"\\t\\t\\tSi la valeur vaux IGNORE ce FS ne sera pas sauvegarder : \${PART_${REF_MACHINE_S}_${REF_PART}[18]}\"
      eval echo -e \"\\t\\t\\tZFS: La commande a executer avant la sauvegarde : \${PART_${REF_MACHINE_S}_${REF_PART}[19]}\"
      eval echo -e \"\\t\\t\\tZFS: La commande a executer apres la sauvegarde : \${PART_${REF_MACHINE_S}_${REF_PART}[20]}\"
      eval echo -e \"\\t\\t\\tZFS: Nom du snapshot a detruire si erreur de sauvegarde : \${PART_${REF_MACHINE_S}_${REF_PART}[21]}\"
      eval echo -e \"\\t\\t\\tZFS: Commande permettant d ecrire sur la destination : \${PART_${REF_MACHINE_S}_${REF_PART}[22]}\"
      eval echo -e \"\\t\\t\\tZFS: Nom du fichier de destination : \${PART_${REF_MACHINE_S}_${REF_PART}[23]}\"
      eval echo -e \"\\t\\t\\t"#######################################################"\"
    done

  done
}

######## Supprime_Old_Dumps
# Supprime les fichiers dumps
###############################################################
Supprime_Old_Dumps () {
[ $DEBUG -eq 1 ] && set -x
Info -v "# Execution de Supprime_Old_Dumps $*"

  case $TYPE_DESTINATION in
    "disk") # si la sauvegarde et de type distant
      echo  "        Purge les fichiers dumps"
      if [ -z "$NBR_DUMP" ] ; then
        echo " Manque NBR_DUMP dans fichier config_save "
        return 1
      fi

      COMPTEUR=1
      EFFACER=0

      # pour tous les fichiers qui sont dans la destination de la sauvegarde
      # récupère les informations entre les parenthèses du sed
      #
      Execute_Commande_C_Retour -d "[ -d \"$TAPE_DEV/${MACHINE_S}\" ] "
      if [ $? -eq 0 ]  ; then
        for I in `Execute_Commande_R_Resul -d "/bin/ls -1 $TAPE_DEV/${MACHINE_S}" | \
            sed -n -e 's/.*\(20......_......_.*_[0-9]\).*/\1/p' |\
            sort  -u  -r `
        do
          if [ "$EFFACER" -eq "1" ] ; then
            echo "  Effacer $TAPE_DEV/${MACHINE_S}/$I"
            Execute_Commande_C_Retour -d /bin/rm -rf $TAPE_DEV/${MACHINE_S}/$I
          else
            echo "  Garder $TAPE_DEV/${MACHINE_S}/$I"
          fi

          # compte sauvegarde de level 0
          # Active la suppression des fichiers qu'une fois les sauvegarde
          # de level 0 on atteind le seuil voulue
          echo $I | /bin/egrep -s "_0$" >/dev/null
          if [ $? -eq 0 ] ; then
            COMPTEUR=`expr  $COMPTEUR + 1 `
            if [ "$COMPTEUR" -le "$NBR_DUMP" ] ; then
              :
              EFFACER=0
            else
              EFFACER=1
            fi
          fi
        done
      fi
    ;;
  esac
  return 0
}

######## Supprime_Old_Logs
# Supprime les LOGS de plus de 190 Jours
###############################################################
Supprime_Old_Logs() {
[ $DEBUG -eq 1 ] && set -x
Info -v "Execution de Supprime_Old_Logs"

  if [ -n "$NBR_JOUR_LOG" ] ; then
    Info -i "Le nombre de jours parametrés dans le fichier config_save est : $NBR_JOUR_LOG"
    Info -v "$NBR_JOUR_LOG = $NBR_JOUR_LOG"
  else
    # Nombre de jours par défaut
    NBR_JOUR_LOG=190
    Info -a "NBR_JOUR_LOG n'est pas valorise dans le fichier config_save"
    Info -a "Utilisation de la valeur par defaut : $NBR_JOUR_LOG"
  fi

  # A VOIR SI LES LIGNES SUIVANTES NE PEUVENT PAS REMPLACER LE RESTE DU CODE
  #~ if [ -d $LOGS_DIR ] ; then
    #~ find $LOGS_DIR -name "*${VAR}*" -type f -mtime +${NBR_JOUR_LOG} -exec rm -f {} \;
  #~ else
    #~ Info -a "Impossible de trouver le repertoire de logs"
  #~ fi

  # calcul du nombre approximatif de jours depuis 1 jan 2000
  set `date -u '+%Y %j'`
  TODAY=`expr 365 \* \( $1 - 2000  \) + \( $1 - 1969 \) / 4 + $2 - 1`

  echo  "        Purge les LOGS"
  COMPTEUR=1
  EFFACER=0

  #  sur la liste des fichiers du répertoire de log
  if [ -d "$LOGS_DIR" ] ; then
    for I in `/bin/ls -1 $LOGS_DIR | \
      sed -n -e 's/.*\(20......_......_.*_[0-9]\).*/\1/p' |\
        sort  -u  -r `
    do
      # récupère les dates du nom dans le nom du répertoire
      AGE_A=`echo $I | cut -c2-4`
      AGE_M=`echo $I | cut -c5-6`
      AGE_D=`echo $I | cut -c7-8`
      # calcul de l'age du fichier
      AGE_LOG=`expr $AGE_A \* 365 + $AGE_M \* 30 + $AGE_D - 30 `
      AGE_LOG=`expr $TODAY - $AGE_LOG `

      if [ $EFFACER -eq 1 ] ; then
        Info -i "  Effacer $LOGS_DIR/*${I}*"
        /bin/rm -f $LOGS_DIR/*${I}*
      #~ else
        #~ echo "  Garder"
        #~ /bin/ls -1 $LOGS_DIR/*${I}*
      fi

      # compte les sauvegardes de level 0
      # Active la suppression des fichiers une fois le nombre de fichier atteint
      # On suppose que les fichiers sont listé dans le bonne ordre
      # les plus récent au début les plus vieux a la fin
      echo $I | /bin/egrep -s "_0$" >/dev/null
      if [ $? -eq 0 ] ; then
        if [ $AGE_LOG -lt $NBR_JOUR_LOG ] ; then
          # echo ne rien faire
          :
          EFFACER=0
        else
          EFFACER=1
        fi
      fi
    done
  fi
}

###############################################################
###############################################################
######  PROGRAMME PRINCIPAL                             #######
###############################################################
###############################################################
# Crée un tableau dans lequel serons stocké des informations général
declare -a TAB_GENERAL

###############################################################
##### Général               ######
###############################################################
## Valorise des variables par des commandes ayant le chemin absolut
## la variable porte le nom de la commande
## Le chemin des commandes peut être différent en fonction de l'OS
## Des Variables doivent être utilisé a la place des commandes
## Elles seront exécuter correctement en fonction de l'OS
Uniforme_Commandes

## Recherche le répertoire de base du script (d'ou ce trouve le script)
## BASE_DIR contient le chemin absolue du répertoire père du script $0 #
## BASE_NAME contient le nom du script
Cherche_Repertoire_Base

###############################################################
##### Charge et contrôle la configuration                ######
###############################################################
# Charge les paramètres misent en argument au script
# Charge le fichier de configuration
# Contrôle que l'exécution du script soit exécutable
#   - Contrôle que ces paramètre soient cohérent
#   - Contrôle l'absence de verrou
#   - Teste les commandes de base pouvant être utilisé sur la machine qui exécute le script
##
# Contrôle le minimum dans le fichier de conf de sauve.sh
# Le fichier et ensuite chargé
# Initialisation des variables de log
# Initialisation des variables global ( paramètre d'entrés, informations systèmes )
# Contrôle l'existence du fichier /.plan
# , puis l'environnement affiché
Controle_Charge_Config $1 $2 $3 $4 $5

## Teste si les arguments mises en paramètre au script sont cohérent
# l'exécution du script. Si ce n'est pas le cas exécution des fonctions "Usage" puis "Sortir"
Controle_Parametres

## Sort du script si le verrou est existant. Si non il crée un verrou
# Évite l'exécution de 2 fois le même outils
# Le verrou est créé dans l'arborescence de l'outil.
# Il est possible avec plusieurs déploiement de l'outil
# d'exécuter plusieurs instances de l'outil
Controle_Absence_Verrou

## Si le processus du script reçois un signal (kill, Ctrl+c), la fonction Sortir sera exécuté
trap "Sortir 4 \"le processus a recu un signal\"" 1 2 3 4 5 6 7 8 15

## Contrôle si il est possible d'exécuter les commandes initialisé dans Uniform_Commande
# et certaines autres. Si une échoue, l'exécution s'arrête
Test_Execution_Commandes

###############################################################
##### Pre-traitement                                      ######
###############################################################
# Definit le type et le lieu de la destination
# Definit les commandes devant utilisées pour atteindre la destination
# Definit la commande

## Definition les variables nécessaire a la destination de la sauvegarde
Definir_Type_Sauvegarde

## Creer l'entete general du mail
Entete $0 $1

# Recherche le type de sauvegarde est :
# Exécute les taches pour les types test ou eject ou valorise le variable LST_SAUVE pour les autres
Parametre_Sauvegarde

#~ # Modification de la fonction pour prise en compte des valeurs 2 (local), 3 (nfs) et 4 (ssh) de ROBOT
#~ # Valorise des variables contenant les commandes de gestion des lecteurs de bande ou des robot en fonction
#~ Definir_Commandes_Mt

# Si le parametre "changer" est indiqué, on écrit le numéro de bande dans le fichier bande_en_cours
#
if [ "$TYPE_DEV" = "changer" ] ; then
  Sauvegarde_Numero_Bande
fi

#### Gere les cas ou la sauvegarde ne doit pas s'effectuer
## On traite le cas du type d'action "eject", avec sortie une fois l'action réalisé.
# Dans le cas contraire.
# exécuter les pre-traitements d'informations des sources
# si il n'est pas demandé de calculer la taille
# pas nécessaire de charger les informations sur les machines source
# dans ce cas
if [ "${TYPE_ACTION}" != "eject" ]  ; then
    Info -v "Le type d action n est pas eject"

  # Cela apporte la fonction de calculer la taille dans le type d'action "test"
  ## Exécution des actions qui ne sont pas des sauvegardes
  if [ "$TYPE_SAUVEGARDE" = "taille" ] && [ "${TYPE_ACTION}" = "test" ] ; then
    # Génère des tableaux dynamiques pour chaque machine et chaque partition
    Info -v "Execution de test et l'estimation"
    Pre_Traitement_Sources
    Execute_Test
    Sortir $?

  elif [ "$TYPE_SAUVEGARDE" != "taille" ] && [ "${TYPE_ACTION}" = "test" ] ; then
    Info -v "Execution de test sans l'estimation"
    Execute_Test
    Sortir $?

  elif [ "$TYPE_SAUVEGARDE" = "taille" ] ; then
    Info -v "ne pas executer test"
    Pre_Traitement_Sources
    Info -v "Le type de sauvegarde est taille"
    Info -v "Seul l'estimation de la taille sera réalisé"
    # Ajoute les information des log plus la taille de la sauvegarde
    #~ Entete_Completion
    # Cherche quelque cas d'erreur dans les LOG
    Cherche_Erreur_Dans_Log
    Sortir 0
  fi

else
  Info -v "Le type d action est eject"
  # C'est que le type d'action est eject
  #~ Entete $0 $TYPE_ACTION
  #~ Info -t "###############################################################"
  ## A FAIRE : Des informations peuvent être ajouté dans les recherches effectué au dessus : test de connexion , taille total estimé , partition prise en compte

  # ici on perd tous les Log du début d'exécution sauf en mode verbeux.
  # Si on supprime le test de Verbosité sur la ligne suivante, on a bien l'entete au début et les log du debut d'execution ne sont pas perdu
  #~ [ "$VERBOSE" -eq 1 ] && cat $LOG_FILE >> $ENTETE_FILE
  #~ mv $ENTETE_FILE $LOG_FILE
  Execute_Commande_C_Retour -d "$MT_EJECT"
  RETOUR_CMD=$?
  Sortir $RETOUR_CMD
fi

# Execution du pre traitement
Pre_Traitement_Sources

Info -v "TYPE_ACTION = $TYPE_ACTION"
Info -v "TYPE_SAUVEGARDE = $TYPE_SAUVEGARDE"
Info -v "${LIST_REF_MACHINES_S}"

#~ # Prépare la destination (principalement le lecteur de bande)
#~ Preparation_Destination

###############################################################
##### Sauvegarde des partitions                          ######
###############################################################

# Boucle sur la liste des machines
for COUNT_MACHINE_S in ${LIST_REF_MACHINES_S} ; do
  FLAG_ERREUR_MACHINE=OK


  # Récupère les informations construites par le pre-traitement
  # pour la machine en cours
  eval MACHINE_S=\${CONF_${COUNT_MACHINE_S}[0]}
  eval LOG_FILE_MACHINE_S=\${CONF_${COUNT_MACHINE_S}[1]}
  eval LST_LOG_TO_SEND=\${CONF_${COUNT_MACHINE_S}[2]}
  eval RSH_OPTION=\${CONF_${COUNT_MACHINE_S}[3]}
  eval RSH_ARGS=\${CONF_${COUNT_MACHINE_S}[4]}
  eval LST_PARTITIONS=\${CONF_${COUNT_MACHINE_S}[5]}
  eval CLIENT_OK=\${CONF_${COUNT_MACHINE_S}[6]}
  eval OS_SOURCE=\${CONF_${COUNT_MACHINE_S}[7]}
  eval VERSION_OS_SOURCE=\${CONF_${COUNT_MACHINE_S}[8]}
  eval PREFIX_SOURCE=\${CONF_${COUNT_MACHINE_S}[9]}
  eval ESTIM_TAILLE_MACHINE_S=\${CONF_${COUNT_MACHINE_S}[10]}
  eval PART_MACHINE_S_KO=\${CONF_${COUNT_MACHINE_S}[12]}
  eval PREFIX_DESTINATION=\${CONF_${COUNT_MACHINE_S}[13]}
  eval SENS_PROTOCOLE_D=\"\${CONF_${COUNT_MACHINE_S}[14]}\"
  eval PREFIX_DESTINATION_SAUVE=\${CONF_${COUNT_MACHINE_S}[15]}
  eval TAPE_DEV_REP=\${CONF_${COUNT_MACHINE_S}[16]}

  Info -v "SENS_PROTOCOLE_D = $SENS_PROTOCOLE_D"
  Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
  Info -v "PREFIX_DESTINATION_SAUVE = $PREFIX_DESTINATION_SAUVE"

  #~ Construction_Prefix_Destination
  # Definit les commande en fonction de l'OS et le type de sauvegarde
  Definir_Commandes_Mt

  # Prépare la destination (principalement le lecteur de bande)
  if [ -z $PREPAR_UNE_SEUL_FOIS ] ; then
    ## Positionne le lecteur sur la bande et la position voulue       ##
    # Si c'est juste une estimation que l'on veut faire
    # Sinon, positionne la bande
    # Numéro de bande dans le robot
    # LIGNE A NE PAS DEPLACER CAR MTX REMBOBINE LA BANDE A LA POSITION_BANDE 0
    BANDENUM="`$BANDE`"

    # Positionne la bande
    case $TYPE_SAUVEGARDE in
    "erase")
      # si erase, on attribue un nouveau code a la bande
      CODE_BANDENUM="`/bin/cat ${BASE_DIR}/ETC/next_bande`"
      Prochain_Code_Bande
      Info -v "MT_REWIND = $MT_REWIND"
      Execute_Commande_C_Retour -d "$MT_REWIND"
    ;;
    "eom")
      # On récupère le code de la bande
      Recherche_Code_Bande
      CODE_BANDENUM="$CODE"
      Info -v "MT_EOM = $MT_EOM"
      Execute_Commande_C_Retour -d "$MT_EOM 2>/dev/null"
    ;;
    esac

    (( PREPAR_UNE_SEUL_FOIS++ ))
  fi

  Preparation_Destination

  Info -v "LST_LOG_TO_SEND = $LST_LOG_TO_SEND"

  #~ # Si la connexion n'a pas réussi, on passe a la machine suivante
  eval TEST_CONNEXION="\${CONF_${COUNT_MACHINE_S}[6]}"
  if [ "$TEST_CONNEXION" -ne "1" ] ; then
    Info -v "Le test de connexion a echoue. Passe a la machine suivante"
    eval CONF_${COUNT_MACHINE_S}[11]=\"Impossible de joindre la machine\"
    continue
  fi

  # Boucle sur les partition de la machine en cours
  eval LIST_PART_REF_MACHINE_S=\${PART_REF_${COUNT_MACHINE_S}[@]}

  for COUNT_PART in $LIST_PART_REF_MACHINE_S ; do
    ERROR=""
    FLAG_ERREUR_PART=OK
    DATE_DEBUT_DUMP=`date +%Y%m%d%H%M%S`
    ID_POSITION_BANDE=`$POSITION_BANDE`
    CODE_ERREUR_DESTINATION=0

    # Récupère les informations construites par le pre-traitement
    # pour la partition en cours
    eval DEVICE_DE_PART=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[0]}\"
    eval PART_TAILLE_K=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[1]}\"
    eval PART_ESPACE_UTIL=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[2]}\"
    eval POINT_DE_MONTAGE=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[3]}\"
    eval PART_TAILLE_B=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[4]}\"
    eval PART_TYPE_DE_FS=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[5]}\"
    eval PART_REP_DEMANDE=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[6]}\"
    eval DUMPER=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[7]}\"
    eval DUMP_OPTIONS=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[8]}\"
    eval RESTORER=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[9]}\"
    eval RESTORE_OPTIONS=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[10]}\"
    eval DUMP_ESTIME=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[11]}\"
    eval FLAG_ACTION=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[12]}\" # si "MANQUE_BASE" ne peux faire l'incrementale
    eval CMD_CREATION_SNAP=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[13]}\"
    eval NOM_SNAP_DU_JOUR=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[14]}\"
    eval CMD_CREATION_SAUVEGARDE=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[15]}\"
    eval SNAP_A_DETRUIRE=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[16]}\"
    eval ESTIMATION_MO=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[17]}\"
    eval FLAG_SAUVEGARDE=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[18]}\"
    eval DUMP_PRE_COM=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[19]}\"
    eval DUMP_POST_COM=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[20]}\"
    eval SNAP_A_DETRUIRE_SI_ERREUR=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[21]}\"
    eval CMD_ECRITURE_D=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[22]}\"
    eval FICHIER_DESTINATION=\"\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[23]}\"

    # Cree le fichier de log pour la patition
    PART_LOG_TMP="${LOG_FILE}_${MACHINE_S}_${ID_POSITION_BANDE}_${COUNT_PART}.${DATE_EXE}"
    Info -v "PART_LOG_TMP = $PART_LOG_TMP"

    Info -i "##################################" | tee -a $PART_LOG_TMP
    Info -i "Sauvegarde : partition en cours $DEVICE_DE_PART" | tee -a $PART_LOG_TMP
    Info -i "##################################" | tee -a $PART_LOG_TMP
    Info -v "CMD_CREATION_SAUVEGARDE = $CMD_CREATION_SAUVEGARDE"
    Info -v "LIST_PART_REF_MACHINE_S = $LIST_PART_REF_MACHINE_S"

    # Si mode verbeux alors on imprime le contenue des tableaux
    #  [ "$VERBOSE" = "1" ] && Affiche_Tableaux

    # Ne traite pas les répertoires marqué comme ignoré
    if [ "$FLAG_SAUVEGARDE" = "IGNORE" ] ; then
      Info -v "Saute ce repertoire"
      continue
    fi

    ###################################################################################
    ### AJOUTER TOUT LES CONTROLES DANS LE TABLEAU ICI AVANT D'EFFECTUER LA SAUVEGARDE
    ###################################################################################
    case $TYPE_DESTINATION in
    "robot"|"bande")
      echo "- "                                        | tee -a $PART_LOG_TMP
      printf "BANDE : %4d (%1d) - POSITION_BANDE : %5d\t--- ${MACHINE_S}:${PART_REP_DEMANDE} (${UNAMES})\n" \
          ${CODE_BANDENUM} ${BANDENUM} ${ID_POSITION_BANDE} | tee -a $PART_LOG_TMP
    ;;
    esac

    ####################################################################
    ########## PRE-SAUVEGARDE                                     ######
    ####################################################################
    if [ -n "$DUMP_PRE_COM" ] ; then
      Info -v "Execution d une commande de pre sauvegarde"
      Info -v "P_CMD : $PREFIX_SOURCE  \"$DUMP_PRE_COM\" "
      Execute_Commande_C_Retour -s "$DUMP_PRE_COM"
      if [ $? -ne 0 ] ; then
        Info -e "La commande de pre-sauvegarde -${DUMP_PRE_COM}-ne c est pas correctement termine"
        Ajout_Tableau_Erreur "La commande de pre-sauvegarde ne c est pas correctement termine"
        continue
      fi
    fi

    ####################################################################
    ########## SAUVEGARDE                                         ######
    ####################################################################
    Info -v "Execution de la sauvegarde du FS : ${MACHINE_S}: $PART_REP_DEMANDE "
    Info -i "#############################################" | tee -a $PART_LOG_TMP
    Info -i "COMMANDE : $CMD_CREATION_SAUVEGARDE" | tee -a $PART_LOG_TMP
    Info -i "#############################################" | tee -a $PART_LOG_TMP
    Info -v "CMD : $PREFIX_SOURCE $CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE | $CMD_ECRITURE_D"

    DUMP_LOG_FILE=/tmp/dump.${DATE_EXE}

    ####################################################################
    ## Construit et envoie la commande en fonction de la direction de la sauvegarde
    if [ "$SENS_PROTOCOLE_D" = "directe" ] ; then
      if [ "$PREFIX_SOURCE" != "eval" ] ; then
        # La source est distante et c'est elle qui communique avec la destination (distante ou local d source)
        Info -v "Directe prefixe source != eval"
        $PREFIX_SOURCE "$CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE | $CMD_ECRITURE_D"
        RC_ECRIT_DEST=$?
      else
        # La source est local, la destination peux être local ou distante
        Info -v "Directe prefixe source = eval"
        $PREFIX_SOURCE "$CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE | $CMD_ECRITURE_D"
        RC_ECRIT_DEST=$?
      fi
        # Nettoie le fichier de test
    else
      # Indirecte
      # La source est distante mais le flux de sauvegarde revient par la machine exe pour être sauvegardé
      if [ "$PREFIX_SOURCE" != "eval" ] ; then
        Info -v "Indirecte est le préfixe source est != de eval"
        $PREFIX_SOURCE "$CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE" | $CMD_ECRITURE_D
        RC_ECRIT_DEST=$?
      else
        # La source est local ### Ne devrait pas passer par ici car si la source est local c'est du directe
        Info -v "Indirecte est le préfixe source = eval"
        Info -v "ATTENTION : NE devrait pas passer ICI : local ne doit pas être "
        $PREFIX_SOURCE "$CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE" | $CMD_ECRITURE_D
        RC_ECRIT_DEST=$?
      fi
    fi

    ####################################################################
    ## Conrole des Erreurs de sauvegarde de la partition
    if [ $RC_ECRIT_DEST -ne 0 ] ; then
      Info -e "La commande $CMD_CREATION_SAUVEGARDE ne c est pas correctement termine"
      Ajout_Tableau_Erreur "La commande $CMD_CREATION_SAUVEGARDE 2>>$DUMP_LOG_FILE | $CMD_ECRITURE_D ne c 'est pas correctement termine"
      FLAG_ERREUR_PART=KO && FLAG_ERREUR_MACHINE=KO
    fi

    # Controle sur bande et robot
    case $TYPE_DESTINATION in
    "robot"|"bande" )
      # Si, apres la sauvegarde, l index est 0, il y a eu une write error
      if [ -z "`$POSITION_BANDE`" ] || [ "`$POSITION_BANDE`" = "0" ] ; then
        CODE_ERREUR_DESTINATION=1
        Ajout_Tableau_Erreur "L index est a 0 il y a eu des write error"
        FLAG_ERREUR_PART=KO && FLAG_ERREUR_MACHINE=KO
      fi
    ;;
    esac

    Info -v "Récupère le fichier distant de log du dump"
    Execute_Commande_R_Resul -s "/bin/cat $DUMP_LOG_FILE" > ${LOGS_DIR}/sauvegarde.${DATE_EXE}
    Execute_Commande_C_Retour -s "/bin/rm -f $DUMP_LOG_FILE 2>/dev/null"

    # Contrôle qu'il n'y ai pas eu d'erreur
    # Recuperation de la sortie erreur du dump et menage
    # Création du tag de sauvegarde Ok ou Nok
    STAT=`/bin/cat ${LOGS_DIR}/sauvegarde.${DATE_EXE} | ${_XPG4GREP} -i -e '(\
    erreur|attention|denied|enough space|abort|offline|cannot|error|killed|no such file\
    )'`
    # STAT gardé pour compatibilité , utiliser dans l'entete et le sujet du mail
    if [ -z "$STAT" ] || [ "$STAT" = "Ok" ] ; then
      Info -v "STAT = $STAT"
      STAT="Ok"
    else
      Info -v "STAT = $STAT"
      STAT="Nok"
    fi

    case $PART_TYPE_DE_FS in
    "zfs")
      # Tache que pour ZFS
      :
    ;;
    *)
      # erreur si la chaîne DUMP: DUMP IS DONE n est pas présente
      ISDONE=`/bin/cat ${LOGS_DIR}/sauvegarde.${DATE_EXE} | $TR -d " " | $GREP -ci DUMP:DUMPISDONE `
      if [ "$ISDONE" -ne "1" ] ; then
        Info -s
        STAT="Nok"
      fi
    ;;
    esac

    # Nettoyage du fichier de log de la sauvegarde du FS
    cat ${LOGS_DIR}/sauvegarde.${DATE_EXE} >> $PART_LOG_TMP
    rm -f ${LOGS_DIR}/sauvegarde.${DATE_EXE}

    # Référence l'erreur
    if [ "$STAT" != "Ok" ] ; then
      Info -v "STAT = $STAT : doit etre a Ok sinon partition en erreur"
      Ajout_Tableau_Erreur "Erreurs dans les logs de la partition $PART_REP_DEMANDE"
      FLAG_ERREUR_PART=KO && FLAG_ERREUR_MACHINE=KO
    fi


    ####################################################################
    ## Tache si erreur de sauvegarde
    # On considère qu'une partition KO = machine KO
    if [ "$FLAG_ERREUR_PART" = "KO" ] || [ "$FLAG_ERREUR_MACHINE" = "KO" ] ; then
      ################ A FAIRE : ajouter les taches de nettoyage

      # Si la sauvegarde de ce FS ne c'est pas bien passer la sauvegarde complete est KO,
      # Il n'est pas necessaire de continuer cette machine
      Info -v "Erreur detecté Passe a la machine suivante"
      break

    fi

    ####################################################################
    ## Rapatriement des informations et menage de logs
    # Recupere la liste des fichiers si cela est possible
     # Liste les fichiers
    # Recréé le nom du point de montage sans /
    CONVERT_NOM_POINT_2_MONTAGE_LETTRE=`echo $POINT_DE_MONTAGE | sed 's/\//_/g'`

    ##############################################################
    ### CREE LA LISTE DES FICHIERS SAUVEGARDES
    # La variable $RESTORER doit etre vide si on ne sait pas lister les fichiers
    if [ -n "$RESTORER" ] ; then

      # Crée la liste des fichiers sauvegardés
      Info -v "Creation de la liste des fichiers sauvegarde"
      LOG_LIST_FILE="${LOGS_DIR}/${DATE_EXE}_${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.lst"
      LOG_LIST_FILE_M_S="${LOGS_DIR}/${DATE_EXE}_${MACHINE_S}.lst"
      $PREFIX_SOURCE "$RESTORER ${RESTORE_OPTIONS} /tmp/${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.${DATE_EXE}" >$LOG_LIST_FILE 2>${LOG_LIST_FILE}.2
      # incertion dans les log

      # Envoie les erreurs de listage des fichier dans les log de la partition
      Info -v "Gestion des erreurs si il y en a"
      if [ -s "${LOG_LIST_FILE}.2" ] ; then
        echo "## Erreur lors du listage des fichiers" | tee -a $PART_LOG_TMP
        cat ${LOG_LIST_FILE}.2 >> $LOG_LIST_FILE_M_S
      fi

      # Envoie la liste des fichiers sauvegardés dans le fichier de log de la partition
      echo "## Liste des fichiers sauvegarde pour $POINT_DE_MONTAGE" | tee -a ${LOG_LIST_FILE_M_S}
      cat $LOG_LIST_FILE >> ${LOG_LIST_FILE_M_S}

      ##############################
      ## Suppression des fichiers obsoletes
      # Le fichier sur la source qui a permis le listage
      $PREFIX_SOURCE "rm -f /tmp/${MACHINE_S}_${CONVERT_NOM_POINT_2_MONTAGE_LETTRE}.${DATE_EXE}"
       # les fichiers rapatriés temporairement
      [ -f "${LOG_LIST_FILE}.2" ] && rm -f "${LOG_LIST_FILE}.2"
      rm -f "$LOG_LIST_FILE"
    fi

    Info -v "LOG_FILE_MACHINE_S = $LOG_FILE_MACHINE_S"
    Info -v "PART_LOG_TMP = $PART_LOG_TMP"
    Info -v "DUMP_LOG_FILE = $DUMP_LOG_FILE"
    Info -v "LOG_RES = $LOG_RES"
    Info -v "ERROR = $ERROR"

    # NE SEMBLE PLUS NECESSAIRE : Recuperation du nom de fichier de dump, dans le cas d une sauvegarde de type ssh
    if [ "$TYPE_DESTINATION" = "disk" ] ; then # SSH
      echo "  DUMP: Dump to `Execute_Commande_R_Resul -d \"/bin/ls $FICHIER_DESTINATION\"`" | tee -a $PART_LOG_TMP
    fi

    # Mise en forme de la liste des fichiers sauvegardes et archivage
    DATE_FIN_DUMP=`date +%Y%m%d%H%M%S`

    # Controle des erreurs
    if [ "$CODE_ERREUR_DESTINATION" -eq "0" ] ; then
      case $TYPE_DESTINATION in
        "bande"|"robot")
          # gère les bandes au fur et à mesure des sauvegardes
          ## bande locale ou robot
          # Si l'index actuelle est plus petit ou egal au derniere indexe,
          # incrémenter le N° de bande et l'écrire sur la bande
          if [ "`$POSITION_BANDE`" -le "$LAST_POSITION_BANDE" ] ; then
            BANDENUM=`expr $BANDENUM + 1`
            Recherche_Code_Bande
            CODE_BANDENUM="$CODE"
          fi
          LAST_POSITION_BANDE="`$POSITION_BANDE`"
          Ecrit_Code_Bande ${CODE_BANDENUM}
        ;;
      esac
    else
      echo "======================================================" | tee -a $PART_LOG_TMP
      echo "ARRET de la sauvegarde sur retour a l'index 0 anormal." | tee -a $PART_LOG_TMP
      echo "======================================================" | tee -a $PART_LOG_TMP
      Ajout_Tableau_Erreur "ARRET de la sauvegarde sur retour a l index 0 anormal"
      Sortir 22
    fi

    ########## A FAIRE GERER UNE BANIERE PAR MACHINE
    LOG_RES=$LOG_RES:$MACHINE_S,$PART_REP_DEMANDE,$CODE_BANDENUM,$ID_POSITION_BANDE,$VOLUME_DUMP,$DATE_DEBUT_DUMP,$DATE_FIN_DUMP,$STAT

    Info -v "LOG_FILE_MACHINE_S = $LOG_FILE_MACHINE_S"
    cat $PART_LOG_TMP >> $LOG_FILE_MACHINE_S
    /bin/rm -f $PART_LOG_TMP
  done
  #####################################################
  ## NIVEAU MACHINE_S
  #####################################################

  #####################################################
  ### Traitement de poste sauvegarde
  ######################################################
  # La sauvegarde est considédé comme correcte que si aucun FS n'a eux d'erreur
  # On supprime le snapshot les sauvegardes et logs qui ne servent plus

  # récupère les informations systèmes de la machine a sauvegarder
  # Ces informations seront nécessaires pour la restauration
  # Cette fonction cree le fichier ${LOGS_DIR}/${DATE_EXE}_${MACHINE_S}_partition
  Sauvegarde_Informations_Systeme


  ######################################################
  ## GESTION DES ERREURS
  eval LIST_PART_EN_ERREUR=\${CONF_$COUNT_MACHINE_S[12]}
  # Tout c'est bien passé
  if [ -z "$LIST_PART_EN_ERREUR" ] ; then
    #  Si aucune erreur on peux supprimer les anciens snapshot de base
    # Ceux créé par cette exécution deviennent la nouvelle base
    for COUNT_PART in $LIST_PART_REF_MACHINE_S ; do
      Info -v "COUNT_PART = $COUNT_PART"
      eval SNAP_A_DETRUIRE=\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[16]}
      if [ -n "$SNAP_A_DETRUIRE" ] ; then
        Info -v "Suppresssion du snapshot avec la commande : zfs destroy $SNAP_A_DETRUIRE "
        Info -v "P_CMD : $PREFIX_SOURCE  zfs destroy $SNAP_A_DETRUIRE"
        Execute_Commande_C_Retour -s "zfs destroy $SNAP_A_DETRUIRE"
      else
        Info -v "Aucun snapshot a supprimer pour cette patition"
      fi
    done

    # Ce repertoire vas contenir les log concernant la machine source
    # puis une archive sera créé
    # si la sauvegarde est sur disque, elle sera envoyé sur la destination avec la sauvegarde
    REP_ARCHIVE_LOG_MACHINE_S="ARCHIVE_${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}"

    ## Suppression des anciennes sauvegardes et de logs vieux
    # supprime les anciennes sauvegarde si elles sont local
    # si il y a eu des erreurs sur 1 partition l'ancienne sauvegarde ne doit pas être supprimé.
    # Il ne faut pas mettre la suppression des dumps au niveau partition, mais au niveau machine
    Supprime_Old_Dumps
    # Supprime les anciens log
    Supprime_Old_Logs
  else
    # Il y a eu au moins une erreur, la sauvegarde n'est pas valide.
    # Il faut supprimer les snapshots créés aujourd'hui
    # La sauvegarde n'est pas cohérente donc pas valide
    Info -v "Suppression du snap du jours"
    for COUNT_PART in $LIST_PART_REF_MACHINE_S ; do
      eval SNAP_A_DETRUIRE_SI_ERREUR=\${PART_${COUNT_MACHINE_S}_${COUNT_PART}[21]}
      if [ -n "$SNAP_A_DETRUIRE_SI_ERREUR" ] ; then
        Info -v "Suppresssion du snapshot avec la commande : zfs destroy $SNAP_A_DETRUIRE_SI_ERREUR "
        Execute_Commande_C_Retour -s "zfs destroy $SNAP_A_DETRUIRE_SI_ERREUR"
      else
        Info -v "Aucun snapshot a supprimer pour cette partition"
      fi
    done

    REP_ARCHIVE_LOG_MACHINE_S="ARCHIVE_${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}_Ko"


    case $TYPE_DESTINATION in
    "disk")
      # Suppression des fichiers de sauvegarde que nous venons de créer
      Execute_Commande_C_Retour -d "rm -rf ${TAPE_DEV_REP}"
    ;;
    *)
      # ICI PEUX ETRE AJOUTER UNE ACTION QUI REMET LA BANDE AVANT LA SAUVEGARDE DE CETTE MACHINE
      :
    ;;
    esac
  fi

  #########################################################
  ## Crée une archive des logs pour la machine en cours de traitement
  cd ${LOGS_DIR}
  if [ ! -d "${LOGS_DIR}/$REP_ARCHIVE_LOG_MACHINE_S" ] ; then
    mkdir -p "${LOGS_DIR}/$REP_ARCHIVE_LOG_MACHINE_S"
  fi

  Info -i "###############################"
  Info -i "LOG de la machine $MACHINE_S"
  Info -i "###############################"
  cat $LOG_FILE_MACHINE_S >> $LOG_FILE
  # effectue une copie des fichier de logs de la machine en cours de  traitement
  mv -f ${DATE_EXE}_${MACHINE_S}_partition $REP_ARCHIVE_LOG_MACHINE_S/
  cp $LOG_FILE $REP_ARCHIVE_LOG_MACHINE_S/
  [ -n ${LOG_LIST_FILE_M_S} ] && cp ${LOG_LIST_FILE_M_S} $REP_ARCHIVE_LOG_MACHINE_S/
  #~ cp $LOG_FILE_MACHINE_S $REP_ARCHIVE_LOG_MACHINE_S/
  mv $LOG_FILE_MACHINE_S $REP_ARCHIVE_LOG_MACHINE_S/
  tar cf - ./$REP_ARCHIVE_LOG_MACHINE_S | ${GZIP} -9c > ${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz

  rm -rf $REP_ARCHIVE_LOG_MACHINE_S

  # Copie le fichier ayant la liste des fichiers sauvegardés pour la partition en cours
  # dans le repertoire de destination
  case $TYPE_DESTINATION in
  "disk")
    Info -v "LOG_LIST_FILE = $LOG_LIST_FILE"
    Info -v "PREFIX_DESTINATION = $PREFIX_DESTINATION"
    cat "${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz" | Execute_Commande_C_Retour -d "cat - > ${TAPE_DEV_REP}/${DATE_EXE}_${TYPE_ACTION}_${NIVEAU_SAUVEGARDE}_${MACHINE_S}.tar.gz"
  ;;
  *)
    :
  ;;
  esac
  cd - >/dev/null

done
Info -i "#####################################################"
Info -i "## NIVEAU GLOBAL                                     "
Info -i "#####################################################"


Sortir 0
