program ui_atm_gram05

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_gram05
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère GRAM05
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère martien GRAM05
!
!
! Usage
!.     ui_atm_gram05 -r r -xlat xlat -xlon xlon -xdate xdate -xmapyear xmapyear
!.                     -scena scena -dust dust -xdustmin xdustmin -xdustmax xdustmax 
!.                     -disp disp -seed seed -rpscale rpscale -xrwscale xrwscale
!.                     -xwlscale xwlscale -xwmscale xwmscale -xcorlmin xcorlmin
!. ou
!.    ui_atm_gram05  -fic fichier_contenant_la_sequence_d_appel 
!
!
!>E  r          : <PM_REEL>  distance au centre de Mars (m)
!>E  xlat       : <PM_REEL>  latitude (rad)
!>E  xlon       : <PM_REEL>  longitude Est (rad)
!>E  xdate      : <PM_REEL>  date (jour julien terrestre)
!>E xmapyear : <integer>        année de mesures pour les données
!.                              1 : pour les mesures TES de l'année 1
!.                              2 : pour les mesures TES de l'année 2 
!.                              0 : sinon
!>E  scena      : <integer>  scénario de poussière :
!.       1 = épaisseur optique des poussières Viking (tau est calculé par le code)
!.       2 = tau spécifique  (tau est lu dans la variable <dust>)
!>E  dust       : <PM_REEL>   épaisseur optique des poussières
!.                             < 0 : scénario de poussières Viking
!.                             > 0 (doit être dans l'intervalle [0.1-3.0])
!>E xdustmin:<PM_REEL> profondeur optique des poussière minimum si dust=0 (>=0.1)
!>E xdustmax:<PM_REEL> profondeur optique des poussière maximum si dust=0 (<=3.0)
!>E  disp       : <integer>  type de perturbations
!.                             1 = atmosphère nominale
!.                             2 = atmosphère perturbée
!>E  seed       : <integer>  graine du générateur aléatoire pour le modèle de perturbation
!>E rpscale: <PM_REEL> facteur multiplicatif d'échelle des perturbations de masse volumique (>=0 et <=2)
!>E xrwscale:<PM_REEL> facteur multiplicatif d'échelle des perturbations de vent (>=0)
!>E xwlscale:<PM_REEL> facteur multiplicatif d'échelle des perturbations de longueur d'onde (>=0.1 et <=10)
!>E xwmcale: <PM_REEL> facteur multiplicatif d'échelle des perturbations de vent moyen
!>E xcorlmin:<PM_REEL> la taille du pas minimum pour les perturbations (>=0 et <=1)
!>S  z          : <PM_REEL>  hauteur / MOLA aréoïde  (m)
!>S  pressure   : <PM_REEL>  pression (Pa)
!>S  ro         : <PM_REEL>  masse volumique (kg/m^3) 
!>S  windu      : <PM_REEL>  composant zonal du vent  (Est-Ouest) (m/s)
!>S  windv      : <PM_REEL>  composant méridional du vent (Nord-Sud) (m/s)
!>S  meanvar    : <PM_REEL, dim=5>  tableau avec les valeurs moyennes des 5 variables précédentes 
!
!$Auteur
! Jean-Luc ROBIN (ATOS)
!
!$Version
!  $Id: ui_atm_gram05.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_gram05.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.8  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.7  2010/11/02 14:23:15  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.6  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.5  2010/10/05 14:01:49  ogarat
!  VERSION::FA-ID:1449:05/10/2010: Mise à jour des accents et majuscules pour l'aide en ligne
!
!  Revision 1.4  2010/10/04 11:08:33  ogarat
!  VERSION::FA-ID:1446:04/10/2010: Alignement des signes = dans l'affichage des résultats
!
!  Revision 1.3  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.2  2010/04/30 14:14:02  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.1.2.2  2010/04/28 14:32:34  jlrobin
!  V2.9::DM-ID:1359:28/04/2010:correction pour assurer la gestion par fichier
!
!  Revision 1.1.2.1  2010/03/01 13:18:25  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Integration du modele d'atmosphere MARS GRAM 2005
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
!- cps_atm_gram05
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
  use cps_atm_gram05
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_gram05.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  REAL(KIND=PM_REEL) :: r,xlat,xlon
  REAL(KIND=PM_REEL) :: xdate
  REAL(KIND=PM_REEL) :: dust, xdustmin, xdustmax
  REAL(KIND=PM_REEL) :: rpscale, xrwscale, xwlscale, xwmscale, xcorlmin
       
  character(len=80) :: dir, file
  integer :: scena,disp,seed, xmapyear

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
  logical :: logdust, logrpscale,logxrwscale,logxwlscale,logxwmscale,logxcorlmin
  logical :: logdisp,logscena,logseed,logxmapyear,logxdustmin,logxdustmax

  integer :: numfich=11

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_gram05") ) then
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
  logxrwscale= .false.
  logxwlscale= .false.
  logxwmscale= .false.
  logxcorlmin= .false.
  logxmapyear= .false.
  logxdustmin= .false.
  logxdustmax= .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_gram05")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
         read(unit=numfich,fmt=*) (l_opt(jj),jj=1,32)
         noptions=16
      else
         ! Lecture des arguments :
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_gram05")
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
               elseif(l_opt(2*ii-1)=="-xmapyear") then
                  read(l_opt(2*ii), fmt=*) xmapyear
                  logxmapyear=.true.
               elseif(l_opt(2*ii-1)=="-scena") then
                  read(l_opt(2*ii), fmt=*) scena
                  logscena=.true.
               elseif(l_opt(2*ii-1)=="-dust") then
                  read(l_opt(2*ii), fmt=*) dust
                  logdust=.true.
               elseif(l_opt(2*ii-1)=="-xdustmin") then
                  read(l_opt(2*ii), fmt=*) xdustmin 
                  logxdustmin=.true.
               elseif(l_opt(2*ii-1)=="-xdustmax") then
                  read(l_opt(2*ii), fmt=*) xdustmax
                  logxdustmax=.true.
               elseif(l_opt(2*ii-1)=="-disp") then
                  read(l_opt(2*ii), fmt=*) disp
                  logdisp=.true.
               elseif(l_opt(2*ii-1)=="-seed") then
                  read(l_opt(2*ii), fmt=*) seed
                  logseed=.true.
               elseif(l_opt(2*ii-1)=="-rpscale") then
                  read(l_opt(2*ii), fmt=*) rpscale
                  logrpscale=.true.
              elseif(l_opt(2*ii-1)=="-xrwscale") then
                 read(l_opt(2*ii), fmt=*) xrwscale
                  logxrwscale=.true.
               elseif(l_opt(2*ii-1)=="-xwlscale") then
                  read(l_opt(2*ii), fmt=*) xwlscale
                  logxwlscale=.true.
               elseif(l_opt(2*ii-1)=="-xwmscale") then
                  read(l_opt(2*ii), fmt=*) xwmscale
                  logxwmscale=.true.
               elseif(l_opt(2*ii-1)=="-xcorlmin") then
                  read(l_opt(2*ii), fmt=*) xcorlmin
                  logxcorlmin=.true.
               endif
            enddo

! corps du programme

! Initialisations preliminaires

      if(logr.and.logxlat.and.logxlon.and.logxdate.and.logxmapyear &
         .and.logdust.and.logxdustmin.and.logxdustmax &
         .and.logrpscale.and.logxrwscale.and.logxwlscale.and.logxwmscale.and.logxcorlmin &
         .and.logdisp.and.logscena.and.logseed) then
           
         ! erreur sur l'init
         if (MSP_erreur) goto 999

         ! appel effectif
         call  cps_atmmarsgram_05(r,xlat,xlon,xdate,dir,file, &
              init_atm,xmapyear,scena,dust,xdustmin,xdustmax,disp,seed,rpscale,&
              xrwscale,xwlscale,xwmscale,xcorlmin,z,pressure, &
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

  end program ui_atm_gram05
