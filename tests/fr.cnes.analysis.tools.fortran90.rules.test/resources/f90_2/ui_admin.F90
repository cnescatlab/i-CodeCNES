!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!  Routines utiles pour les IHM COMPAS (administration et processus automatiques)
!
!$Version
!  $Id: ui_admin.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_admin.F90,v $
!  Revision 1.5  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.4  2008/04/11 12:44:18  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.3  2008/04/11 12:02:23  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.2  2008/03/04 10:35:13  vivaresf
!  FA-ID 843 : erreur de mise à jour des préférences
!
!  Revision 1.1  2008/02/08 17:51:20  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!
!  Revision 1.9  2006/06/16 17:31:59  vivaresf
!  Cartouches d'entete
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cps_ihm_getParamProcess(process, acces_admin, nom_process, &
     ftpuser, ftppwd, ftpip, repScripts, repServeur, repClient,       &
     repLoc, fichier, envoi_mail, minutes, heure, jour_mois, mois,    &
     jour_semaine, premier_jour, nb_jour_mois, cmd, activer)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getParamProcess
!
!$Resume
!
!$Description
!  Extraction des parametres d'un processus automatise depuis le
!  fichier de configuration administrateur.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getParamProcess(process, acces_admin, nom_process, &
!.         ftpuser, ftppwd, ftpip, repScripts, repServeur, repClient,       &
!.         repLoc, fichier, envoi_mail, minutes, heure, jour_mois, mois,    &
!.         jour_semaine, premier_jour, nb_jour_mois, cmd, activer)
!.    integer :: process, acces_admin
!.    character(LEN=*) :: nom_process, ftpuser, ftppwd, ftpip
!.    character(LEN=*) :: repScripts, repServeur, repClient, repLoc
!.    character(LEN=*) :: fichier, envoi_mail, minutes, heure
!.    character(LEN=*) :: jour_mois, mois, jour_semaine, premier_jour
!.    character(LEN=*) :: nb_jour_mois, cmd, activer
!
!$Arguments
!>E     process       :<integer>   numéro du processus
!>E     acces_admin   :<integer>   accès MADONA ouvert sur le fichier de configuration administrateur
!>S     nom_process   :<LEN=*>     nom du processus
!>S     ftpuser       :<LEN=*>     nom de connexion ftp
!>S     ftppwd        :<LEN=*>     mot de pass ede connexion ftp
!>S     ftpip         :<LEN=*>     nom ou adresse ip du serveur ftp
!>S     repScripts    :<LEN=*>     répertoire contenant les scripts
!>S     repServeur    :<LEN=*>     répertoire sur le serveur où sont situés les fichiers à transférer
!>S     repClient     :<LEN=*>     répertoire sur la machine cible où déposer les fichiers transférés
!>S     repLoc        :<LEN=*>     répertoire complet dans la base associé au processus
!>S     fichier       :<LEN=*>     fichier contenant tous les fichiers à transférer
!>S     envoi_mail    :<LEN=*>     'oui' ou 'non'
!>S     minutes       :<LEN=*>     minutes pour la date du transfert
!>S     heure         :<LEN=*>     heure pour la date du transfert
!>S     jour_mois     :<LEN=*>     numéro des jours dans le mois pour la date du transfert (* = tous les jours)
!>S     mois          :<LEN=*>     numéro des mois pour la date du transfert (* = tous les mois)
!>S     jour_semaine  :<LEN=*>     jours dans la semaine pour la date du transfert (* = tous les jours)
!>S     premier_jour  :<LEN=*>     numéro du premier du jour dans le mois pour définir une fréquence d'exécution
!>S     nb_jour_mois  :<LEN=*>     nombre de jours séparant deux exécutions consécutives
!>S     cmd           :<LEN=*>     nom du script principal associé au processus
!>S     activer       :<LEN=*>     
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: process, acces_admin
  character(LEN=*), intent(out) :: nom_process, ftpuser, ftppwd, ftpip
  character(LEN=*), intent(out) :: repScripts, repServeur, repClient, repLoc
  character(LEN=*), intent(out) :: fichier, envoi_mail, minutes, heure
  character(LEN=*), intent(out) :: jour_mois, mois, jour_semaine, premier_jour
  character(LEN=*), intent(out) :: nb_jour_mois, cmd, activer
  
  ! variables locales
  integer :: ret
  character(LEN=256) :: buffer

  ! selection du tableau 'processus' dans le fichier de configuration
  ! administrateur
  ret = acc_select(acces_admin, "processus", ACC_TABL)
  
  ! selection du processus
  ret = acc_set_index(acces_admin, process)
  if (ret.eq.0) then
     ! selection de la structure
     ret = acc_select(acces_admin, ACC_INDEX, ACC_STRUCT)
     
     ret = acc_gets(acces_admin, "nom", buffer)
     nom_process = trim(buffer)

     ret = acc_gets(acces_admin, "ftpuser", buffer)
     ftpuser = trim(buffer)
     ret = acc_gets(acces_admin, "ftppwd", buffer)
     ftppwd = trim(buffer)
     ret = acc_gets(acces_admin, "ftpip", buffer)
     ftpip = trim(buffer)
     
     ret = acc_gets(acces_admin, "repScripts", buffer)
     repScripts = trim(buffer)
     ret = acc_gets(acces_admin, "repServeur", buffer)
     repServeur = trim(buffer)
     ret = acc_gets(acces_admin, "repClient", buffer)
     repClient = trim(buffer)
     ret = acc_gets(acces_admin, "repLoc", buffer)
     repLoc = trim(buffer)
     
     ret = acc_gets(acces_admin, "fichier", buffer)
     fichier = trim(buffer)
     ret = acc_gets(acces_admin, "envoi_mail", buffer)
     envoi_mail = trim(buffer)
     
     ret = acc_gets(acces_admin, "minutes", buffer)
     minutes = trim(buffer)
     ret = acc_gets(acces_admin, "heure", buffer)
     heure = trim(buffer)
     ret = acc_gets(acces_admin, "jour_mois", buffer)
     jour_mois = trim(buffer)
     ret = acc_gets(acces_admin, "mois", buffer)
     mois = trim(buffer)
     ret = acc_gets(acces_admin, "jour_semaine", buffer)
     jour_semaine = trim(buffer)
     ret = acc_gets(acces_admin, "premier_jour", buffer)
     premier_jour = trim(buffer)
     ret = acc_gets(acces_admin, "nb_jour_mois", buffer)
     nb_jour_mois = trim(buffer)
     ret = acc_gets(acces_admin, "cmd", buffer)
     cmd = trim(buffer)
     ret = acc_gets(acces_admin, "activer", buffer)
     activer = trim(buffer)

     ! fin de la selection 
     ret = acc_select_end(acces_admin)
  end if
  
  
  ! fin de la selection
  ret = acc_select_end(acces_admin)
  
end subroutine cps_ihm_getParamProcess



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Mise a jour des parametres d'un processus automatise dans le       !!
!! fichier de configuration administrateur                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_majParamProcess(process, acces_admin, nom_param,    &
     val_param)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_majParamProcess
!
!$Resume
!
!$Description
!  Mise a jour des parametres d'un processus automatise dans le
!  fichier de configuration administrateur.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_majParamProcess(process, acces_admin, nom_param,    &
!.         val_param)
!.    integer :: process, acces_admin
!.    character(LEN=*) :: nom_param
!.    character(LEN=*) :: val_param
!
!$Arguments
!>E     process      :<integer>   numéro du processus
!>E     acces_admin  :<integer>   accès MADONA ouvert sur le fichier de configuration admin
!>E     nom_param    :<LEN=*>     nom du paramètre à modifier
!>S     val_param    :<LEN=*>     nouvelle valeur du paramètre
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: process, acces_admin
  character(LEN=*), intent(in) :: nom_param
  character(LEN=*), intent(out) :: val_param
  
  ! variables locales
  integer :: ret

  ! selection du tableau 'processus' 
  ret = acc_select(acces_admin, "processus", ACC_TABL)
  
  ! selection de l'element correspondant au processus
  ret = acc_set_index(acces_admin, process)
  if (ret.eq.0) then
     ! selection de la structure
     ret = acc_select(acces_admin, ACC_INDEX, ACC_STRUCT)
     
     ! ecriture du parametre
     ret = acc_puts(acces_admin, trim(nom_param), val_param)

     ! fin de la selection de la structure
     ret = acc_select_end(acces_admin)
  end if

  ! fin de la selection du tableau
  ret = acc_select_end(acces_admin)
  
end subroutine cps_ihm_majParamProcess


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getCronParam(jour_semaine, mois, ind_jour, ind_mois)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getCronParam
!
!$Resume
!  Construit deux tableaux avec semaine/mois avec les jours indiqués par une crontab
!$Description
!  Cette routine positionne le ieme élément du tableau 'ind_jour'
!  (resp. 'ind_mois') si le ieme jour (resp. mois) est present dans 'jour_semaine'
!  (resp. 'mois')
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getCronParam(jour_semaine, mois, ind_jour, ind_mois)
!.    character(LEN=*) :: jour_semaine, mois
!.    integer, dimension(7) :: ind_jour
!.    integer, dimension(12) :: ind_mois
!
!$Arguments
!>E     jour_semaine  :<LEN=*>              jours dans la semaine
!>E     mois          :<LEN=*>              mois
!>S     ind_jour      :<integer,DIM=(7)>    le ieme element vaut 1 si le ieme jour est present dans 'jour_semaine', sinon 0
!>S     ind_mois      :<integer,DIM=(12)>   le ieme element vaut 1 si le ieme mois est present dans 'mois', sinon 0
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  ! arguments
  character(LEN=*), intent(in) :: jour_semaine, mois
  integer, dimension(7), intent(out) :: ind_jour
  integer, dimension(12), intent(out) :: ind_mois
  
  ! variables locales
  integer :: ind, i
  character(LEN=256) :: buffer
  
  ! initialisation
  ind_jour(:) = 0
  ind_mois(:) = 0

  ! les jours de la semaine
  if (jour_semaine.eq."*") then
     ind_jour(:) = 1
  else
     do i=1,7
        write(buffer,10) i
        ind = index(trim(jour_semaine), trim(buffer))
        if (ind.gt.0) then
           ! le jour i est present
           ind_jour(i) = 1
        else
           ind_jour(i) = 0
        end if
     end do
  end if

  ! les mois
  if (mois.eq."*") then
     ind_mois(:) = 1
  else
     do i=1,12
        write(buffer,10) i
        ind = index(trim(mois), trim(buffer))
        if (ind.gt.0) then
           ! le mois i est present
           ind_mois(i) = 1
        else
           ind_mois(i) = 0
        end if
     end do
  end if
  
10  format (i2)
end subroutine cps_ihm_getCronParam


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Ecriture des tables cron a partir du fichier de configuration      !!
!! administrateur                                                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_ecrireCron(acces_config)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_ecrireCron
!
!$Resume
!  Ecriture des tables cron 
!$Description
!  Ecriture des tables cron a partir du fichier de configuration
!  administrateur.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_ecrireCron(acces_config)
!.    integer :: acces_config
!
!$Arguments
!>E     acces_config  :<integer>   accès MADONA ouvert sur le fichier de configuration admin
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_util, only : cps_file_unit

  implicit none
#include "acces_F.h"

  ! arguments
  integer, intent(in) :: acces_config
  
  ! variables locales
  integer :: ret, nb_processus, unit, i, ier
  character(LEN=256) :: nom_process, ftpuser, ftppwd, ftpip, repScripts,&
       repServeur, repClient, repLoc, fichier, envoi_mail, minutes,     &
       heure, jour_mois, mois, jour_semaine, premier_jour, nb_jour_mois,&
       cmd, str_activer
  character(LEN=256) :: crontab
  
  ! initialisation
  nb_processus = acc_get_dim(acces_config, "processus")
!  unit = 14
  
  ! recuperation de la crontab
  ret = acc_gets(acces_config, "crontab", crontab)
  ! desactivation de la crontab
  call system("\crontab -r ")
  ! ouverture de la crontab
  call cps_file_unit(unit,ier)
  if(ier.lt.0)then
     return
  endif
  open(unit=unit, file=trim(crontab), status="REPLACE")

  ! boucle sur chaque processus
  do i=1,nb_processus
     call cps_ihm_getParamProcess(i, acces_config,                     &
          nom_process, ftpuser, ftppwd, ftpip, repScripts, repServeur, &
          repClient, repLoc, fichier, envoi_mail, minutes, heure,      &
          jour_mois, mois, jour_semaine, premier_jour, nb_jour_mois,   &
          cmd, str_activer)
     if (trim(str_activer).eq."oui") then
        write(unit,*) trim(minutes), " ", trim(heure), " ",            &
          trim(jour_mois), " ", trim(mois), " ", trim(jour_semaine),   &
          " ", trim(repScripts)//"/"//trim(cmd), " ", trim(ftpip), " ",&
          trim(ftpuser), " ", trim(ftppwd), " ", trim(repScripts), " ",&
          trim(repServeur), " ", trim(repClient), " ", trim(repLoc),   &
          " ", trim(fichier), " ", trim(envoi_mail), " > ",            &
          trim(repClient)//"/trace_cron"
     end if
  end do

  ! fermeture de la crontab
  close(unit)
  ! activation de la crontab
  call system("\crontab "//trim(crontab))

end subroutine cps_ihm_ecrireCron



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Extraire les preferences                     !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_getPreferences(acces_admin, editeur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_getPreferences
!
!$Resume
!  Lecture de préférences (éditeur)
!
!$Description
!  Extraire les preferences.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_getPreferences(acces_admin, editeur)
!.    integer :: acces_admin
!.    character(LEN=*) :: editeur
!
!$Arguments
!>E     acces_admin  :<integer>   accès MADONA ouvert sur le fichier de configuration admin
!>S     editeur      :<LEN=*>     nom de l'éditeur de texte utilisé dans l'IHM admin
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: acces_admin
  character(LEN=*), intent(out) :: editeur
  
  ! variables locales
  integer :: ret

  ! initialisation  
  editeur = ""

  ! selection de la structure 'preferences'
  ret = acc_select(acces_admin, "preferences", ACC_STRUCT)
  
  ret = acc_gets(acces_admin, "editeur", editeur)

  ! fin de la selection
  ret = acc_select_end(acces_admin)
  
end subroutine cps_ihm_getPreferences


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                       Mise a jour des preferences                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cps_ihm_setPreferences(acces_admin, pref, val)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_ihm_setPreferences
!
!$Resume
!  Ecriture des préférences (éditeur)
!$Description
!  Mise a jour des preferences.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_ihm_setPreferences(acces_admin, pref, val)
!.    integer :: acces_admin
!.    character(LEN=*) :: pref, val
!
!$Arguments
!>E     acces_admin  :<integer>   accès MADONA ouvert sur le fichier de configuration admin
!>E     pref         :<LEN=*>     nom du paramètre à mettre à jour
!>E     val          :<LEN=*>     nouvelle valeur du paramètre
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
#include "acces_F.h"
  ! arguments
  integer, intent(in) :: acces_admin
  character(LEN=*), intent(in) :: pref, val
  
  ! variables locales
  integer :: ret
  
  ! selection de la structure 'preferences'
  ret = acc_select(acces_admin, "preferences", ACC_STRUCT)
  
  ret = acc_puts(acces_admin, trim(pref), trim(val))

  ! fin de la selection
  ret = acc_select_end(acces_admin)

end subroutine cps_ihm_setPreferences

