program ui_atm_cira

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_cira
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère CIRA
!
!$Description
! Utilitaire pour le fonctionnement du modèle d'atmosphère CIRA
!
!
! Usage
!.     ui_atm_cira -mois mois -lat lat -long long -haut haut
!
!
!>E  mois          : <integer>          mois
!>E  long          : <PM_REEL>          longitude (rad)
!>E  lat           : <PM_REEL>          latitude (rad)
!>E  haut          : <PM_REEL>          altitude (m)
!>S  tempe         : <PM_REEL>          température (K)
!>S  pres          : <PM_REEL>          pression (Pa)
!>S  dens          : <PM_REEL>          Masse volumique (kg/m3)
!
!$Auteur
! Julien Bouillant(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_cira.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_cira.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.19  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.18  2010/11/02 14:16:24  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.17  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.16  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.15  2008/10/28 14:11:36  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.14  2008/10/03 07:21:58  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.13  2008/07/04 12:25:16  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!
!  Revision 1.12  2008/04/04 18:00:28  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.11  2008/04/04 17:10:32  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.10  2008/04/03 18:02:50  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!  Revision 1.9  2007/11/13 14:39:24  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!  Revision 1.8  2007/07/04 12:22:42  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!  Revision 1.7  2006/11/20 08:13:21  vpg
!  Mise a jour des cartouches
!  Revision 1.6  2006/10/24 09:31:20  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.5  2006/10/18 09:53:04  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.4.2.1  2006/09/26 12:15:29  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.4  2006/05/23 09:32:56  vivaresf
!  DM-ID 387 (lot 5) : mise au point des utilitaires
!  - simplification du code
!  - suppression des variables inutilisées
!  Revision 1.3  2006/05/12 12:06:12  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.2  2006/05/02 09:38:38  vpg
!  Suppression des variables non utilisees
!  Revision 1.1  2006/01/30 09:10:13  bouillaj
!  creation de l utilitaire pour tester le modele d atmosphere correspondant
!#V
!#V
!>  pos_geod      : <tm_geodesique>    position géodésique
!>  mois          : <integer>          mois
!>  tempe         : <PM_REEL>          température (K)
!>  pres          : <PM_REEL>          pression (Pa)
!>  dens          : <PM_REEL>          Masse volumique (kg.m-3)
!>  code_retour   : <tm_code_retour>   
!>  messages      : <MSP_MESSAGE>      
!>  long          : <PM_REEL>          longitude (rad)
!>  lat           : <PM_REEL>          latitude (rad)
!>  haut          : <PM_REEL>          altitude (m)
!>  noptions      : <integer>          
!>  ii            : <integer>          
!>  iargc         : <integer>          
!>  ierfin        : <integer>          
!>  l_opt         : <LEN=20,DIM=(50)>  
!>  logmois       : <logical>          
!>  loglat        : <logical>          
!>  loglong       : <logical>          
!>  loghaut       : <logical>          
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atm_cira_mod
!- cps_acces
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atm_cira
!- MSP_recuperer_message
!- MSP_afficher_message
!
!$FinHistorique
!
!$Include
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  use mslib
  use msp_gestion_erreur
  use ui_io
  use cps_atm_cira_mod
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256),parameter :: SVN_VER = &
   '$Id: ui_atm_cira.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées/sorties
  type(tm_geodesique) :: pos_geod 
  integer :: mois

  real(KIND=PM_REEL) :: tempe,pres, dens     
  type(tm_code_retour) ::  code_retour

! variables locales

  type(MSP_MESSAGE) :: messages
  real(KIND=PM_REEL) :: long, lat, haut
  integer :: noptions, ii, ierfin
  character(len=20),dimension(50):: l_opt
  logical :: logmois, loglat, loglong, loghaut

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_cira") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logmois =.false.
  loglong =.false.
  loglat = .false.
  loghaut = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_cira")
         goto 999
      else
         ! Lecture des arguments : -mois, -long, -lat, -haut
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_cira")
            goto 999
         else
            do ii=1, noptions
               if (l_opt(2*ii-1)=="-mois") then
                  read(l_opt(2*ii), fmt='(i2)') mois
                  logmois = .true.
               elseif(l_opt(2*ii-1)=="-long") then
                  read(l_opt(2*ii), fmt=*) long
                  loglong=.true.
               elseif(l_opt(2*ii-1)=="-lat") then
                 read(l_opt(2*ii), fmt=*) lat
                  loglat=.true.
               elseif(l_opt(2*ii-1)=="-haut") then
                  read(l_opt(2*ii), fmt=*) haut
                  loghaut=.true.
               endif
            enddo
         endif
         
      endif

! corps du programme

! Initialisations preliminaires

      if(logmois.and.loglat.and.loglong.and.loghaut) then
         pos_geod%long = long
         pos_geod%lat = lat
         pos_geod%haut = haut
         
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_atm_cira(mois, pos_geod,tempe, pres, dens, code_retour)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
         write (*,1002) "Température     (K)     = ", tempe
         write (*,1002) "Pression        (Pa)    = ", pres
         write (*,1002) "Masse volumique (kg/m3) = ", dens
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"

      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_close_utilisateur()

1002 FORMAT(a,g21.12)

  end program ui_atm_cira
