program ui_atm_gram2001

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_gram2001
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère GRAM2001
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère martien GRAM2001
!
!
! Usage
!.     ui_atm_gram2001 -r r -xlat xlat -xlon xlon -xdate xdate 
!.                     -scena scena -dust dust -disp disp -seed seed -rpscale rpscale
!. ou
!.    ui_atm_gram2001 -fic fichier_contenant_la_sequence_d_appel 
!
!
!>E  r          : <PM_REEL>  distance au centre de Mars (m)
!>E  xlat       : <PM_REEL>  latitude (rad)
!>E  xlon       : <PM_REEL>  longitude Est (rad)
!>E  xdate      : <PM_REEL>  date (jour julien terrestre)
!>E  scena      : <integer>  scénario de poussière :
!.       1 = épaisseur optique des poussières Viking (tau est calculé par le code)
!.       2 = tau spécifique  (tau est lu dans la variable <dust>)
!>E  dust       : <PM_REEL>   épaisseur optique des poussières
!.                             < 0 : scénario de poussières Viking
!.                             > 0 (doit être dans l'intervalle [0.1-3.0])
!>E  disp       : <integer>  type de perturbations
!.                             1 = atmosphère nominale
!.                             2 = atmosphère perturbée
!>E  seed       : <integer>  graine du générateur aléatoire pour le modèle de perturbation
!>E  rpscale    : <PM_REEL>  facteur multiplicatif pour la masse volumique et les perturbations dues aux vents
!>S  z          : <PM_REEL>  hauteur / MOLA aréoïde  (m)
!>S  pressure   : <PM_REEL>  pression (Pa)
!>S  ro         : <PM_REEL>  masse volumique (kg/m^3) 
!>S  windu      : <PM_REEL>  composant zonal du vent  (Est-Ouest) (m/s)
!>S  windv      : <PM_REEL>  composant méridional du vent (Nord-Sud) (m/s)
!>S  meanvar    : <PM_REEL, dim=5>  tableau avec les valeurs moyennes des 5 variables précédentes 
!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_gram2001.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_gram2001.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.14  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.13  2010/11/02 14:23:21  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.12  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.11  2010/10/04 11:08:33  ogarat
!  VERSION::FA-ID:1446:04/10/2010: Alignement des signes = dans l'affichage des résultats
!
!  Revision 1.10  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.9  2008/10/28 14:11:40  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.8  2008/10/03 07:22:02  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.7  2008/08/04 13:53:53  gss
!  DM-ID 1058 : (portage g95) initialisation des sorties.
!
!  Revision 1.6  2008/07/11 12:08:16  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.5  2008/07/04 12:23:26  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.4  2008/04/03 18:02:55  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.3  2007/11/21 11:49:21  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.2  2007/10/31 17:59:03  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.1  2007/10/30 11:09:25  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!
!
!#V
!#
!
!#V
      
!#
!
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atm_gram
!- cps_acces
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_modele_emcd23
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
  use cps_atm_gram
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_gram2001.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  REAL(KIND=PM_REEL) :: r,xlat,xlon
  REAL(KIND=PM_REEL) :: xdate
  REAL(KIND=PM_REEL) :: dust
  REAL(KIND=PM_REEL) :: rpscale
       
  character(len=80) :: dir, file
  integer :: scena,disp,seed

! variables locales
  REAL(KIND=PM_REEL) :: z
  REAL(KIND=PM_REEL) :: pressure,ro,temperature,windu,windv
  REAL(KIND=PM_REEL) :: meanvar(5),extvar(25)
  integer :: ier
  logical :: init_atm
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logr,logxlat,logxlon,logxdate
  logical :: logdust, logrpscale
  logical :: logdisp,logscena,logseed

  integer :: numfich=11

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_gram2001") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  meanvar(1:5) = 0._pm_reel
  extvar(1:25) = 0._pm_reel
  pressure = 0._pm_reel
  ro = 0._pm_reel
  temperature = 0._pm_reel
  z = 0._pm_reel
  windu = 0._pm_reel
  windv = 0._pm_reel

  ier = 0

  dir =" "
  file = ""
  ! reinitilisation du modele d'atmosphere 
  init_atm = .true.

  logr = .false.
  logxlat = .false.
  logxlon = .false.
  logxdate = .false.
  logdust = .false.
  logrpscale = .false.
  logdisp = .false.
  logscena = .false.
  logseed = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_gram2001")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
         read(unit=numfich,fmt=*) (l_opt(jj),jj=1,18)
         noptions=9
      else
         ! Lecture des arguments :
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_gram2001")
            goto 999
      endif
      endif
      do ii=1, noptions
               if (l_opt(2*ii-1)=="-r") then
                  read(l_opt(2*ii), fmt=*) r
                  logr = .true.
             elseif(l_opt(2*ii-1)=="-xlat") then
                  read(l_opt(2*ii), fmt=*) xlat
                  logxlat=.true.
               elseif(l_opt(2*ii-1)=="-xlon") then
                 read(l_opt(2*ii), fmt=*) xlon
                  logxlon=.true.
               elseif(l_opt(2*ii-1)=="-xdate") then
                  read(l_opt(2*ii), fmt=*) xdate
                  logxdate=.true.
               elseif(l_opt(2*ii-1)=="-scena") then
                  read(l_opt(2*ii), fmt=*) scena
                  logscena=.true.
               elseif(l_opt(2*ii-1)=="-dust") then
                  read(l_opt(2*ii), fmt=*) dust
                  logdust=.true.
               elseif(l_opt(2*ii-1)=="-disp") then
                  read(l_opt(2*ii), fmt=*) disp
                  logdisp=.true.
               elseif(l_opt(2*ii-1)=="-seed") then
                  read(l_opt(2*ii), fmt=*) seed
                  logseed=.true.
               elseif(l_opt(2*ii-1)=="-rpscale") then
                  read(l_opt(2*ii), fmt=*) rpscale
                  logrpscale=.true.
               endif
            enddo

! corps du programme

! Initialisations preliminaires

      if(logr.and.logxlat.and.logxlon.and.logxdate.and.logdust.and.logrpscale&
         .and.logdisp.and.logscena.and.logseed) then
           
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_atmmarsgram_2001 (r,xlat,xlon,xdate,dir,file, &
             init_atm,scena,dust,disp,seed,rpscale,z,pressure, &
             ro,temperature,windu,windv,meanvar,extvar,ier)
        
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
         ! sortie écran
 	 write(*,1008) "Altitude        (m)     = ",z
         write(*,1006) "Pression        (Pa)    = ",pressure
         write(*,1006) "Masse volumique (kg/m3) = ",ro
         write(*,1006) "Température     (K)     = ",temperature
         write(*,1006) "Vent Est-Ouest  (m/s)   = ",windu	 
         write(*,1006) "Vent Nord-Sud   (m/s)   = ",windv
         write(*,1007) "Meanvar                 = ",meanvar
    
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"

      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_close_utilisateur()

! format pour l'affichage des resultats
1006 FORMAT(a,g15.9)
1007 FORMAT(a,5(g15.9))
1008 FORMAT(a,g18.12)

  end program ui_atm_gram2001
