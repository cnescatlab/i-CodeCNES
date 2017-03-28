program ui_atm_roat77

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_roat77
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère ROAT77
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère ROAT77
! Il utilise la fonction cps_roat77 de cps_atmosphere.F90 de COMPAS_BASE.
!
!
! Usage
!.     ui_atm_roat77 -jour jour -x x -y y -z z -flux flux -ap ap 
!
!
!>E  jour          : <PM_REEL>          jour (JJ50)
!>E  x             : <PM_REEL>          x (km)
!>E  y             : <PM_REEL>          y (km)
!>E  z             : <PM_REEL>          z (km)
!>E  ap            : <PM_REEL>          indice géomagnétique 
!>E  flux	   : <PM_REEL>          flux 
!>S  roat77        : <PM_REEL>          masse volumique (kg/m^3)
!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_roat77.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_roat77.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.11  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.10  2010/11/02 14:23:46  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.9  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.8  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.7  2008/10/28 14:11:43  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.6  2008/10/03 07:22:05  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.5  2008/08/04 13:55:14  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 de la sortie.
!
!  Revision 1.4  2008/07/04 12:04:21  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!
!  Revision 1.3  2008/04/03 18:03:27  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.2  2007/10/31 17:58:59  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.1  2007/10/30 11:09:18  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!
!
!#V
!#
!
!#V
!>  pos           : <tm_geodesique>    position Greenwich (km)
!>  roat77        : <PM_REEL>          masse volumique (kg.m-3)
!>  jour          : <PM_REEL>          jour 
!>  code_retour   : <tm_code_retour>   
!>  messages      : <MSP_MESSAGE>      
!>  x          	  : <PM_REEL>          X  dans  Greenwich(km)
!>  y             : <PM_REEL>          Y  dans  Greenwich (km)
!>  z	          : <PM_REEL>          Z  dans  Greenwich (km)
!>  noptions      : <integer>          
!>  ii            : <integer>          
!>  iargc         : <integer>          
!>  ierfin        : <integer>          
!>  l_opt         : <LEN=20,DIM=(50)>  
!>  logjour       : <logical>          
!>  logx          : <logical>          
!>  logy          : <logical>          
!>  logz          : <logical>          
!#
!
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_acces
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_roat77
!- MSP_recuperer_message
!- MSP_afficher_message
!#
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
  use cps_atmosphere
! fonction : cps_roat77
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_roat77.F90 355 2013-02-14 12:16:41Z aadt $'


! entrees
  real(KIND=PM_REEL) :: x, y, z
  real(KIND=PM_REEL) :: jour 
  real(KIND=PM_REEL) :: flux,ap

! sorties
  real(KIND=PM_REEL) :: roat77 = 0._pm_reel

! variables locales
  real(KIND=PM_REEL), dimension(3) :: pos
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logjour, logx, logy, logz
  logical :: logflux, logap

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_roat77") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logjour =.false.
  logx =.false.
  logy = .false.
  logz = .false.
  logflux = .false.
  logap = .false.

! Lecture des paramètres (en ligne)

  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_roat77")
         goto 999
      else
         ! Lecture des arguments : -jour, -x, -y, -z
         ! -flux -ap 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_roat77")
            goto 999
         else
            do ii=1, noptions
               if (l_opt(2*ii-1)=="-jour") then
                  read(l_opt(2*ii), fmt=*) jour
                  logjour = .true.
               elseif(l_opt(2*ii-1)=="-x") then
                  read(l_opt(2*ii), fmt=*) x
                  logx=.true.
               elseif(l_opt(2*ii-1)=="-y") then
                 read(l_opt(2*ii), fmt=*) y
                  logy=.true.
               elseif(l_opt(2*ii-1)=="-z") then
                  read(l_opt(2*ii), fmt=*) z
                  logz=.true.
               elseif(l_opt(2*ii-1)=="-flux") then
                  read(l_opt(2*ii), fmt=*) flux
                  logflux=.true.
               elseif(l_opt(2*ii-1)=="-ap") then
                  read(l_opt(2*ii), fmt=*) ap
                  logap=.true.
               endif
            enddo
         endif       
      endif

! corps du programme

! Initialisations preliminaires

      if(logjour.and.logx.and.logy.and.logz.and.logflux.and.logap) then
         pos(1) = x
         pos(2) = y
         pos(3) = z
       
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_roat77(jour,pos,flux,ap,roat77)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
         ! sortie écran
         write (*,'(a,e15.9)') "Masse volumique (kg/m3) = ", roat77
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"
         goto 999
      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_close_utilisateur()

  end program ui_atm_roat77
