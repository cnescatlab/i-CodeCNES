program ui_atm_msis90

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_msis90
!
!$Resume
!  Programme de calcul du modèle d'atmosphère MSIS90
!
!$Description
!  Programme de calcul du modèle d'atmosphère MSIS90
!  Utilise cps_calculer_msis90 de cps_atmosphere.F90
!
! Usage
!.     ui_atm_msis90 -jour jour -sec sec
!.                   -long long -lat lat -alt alt 
!.		     -flux_veille flux_veille -flux_3rot flux_3rot -heure heure 
!.                   -tab1 tab1 -tab2 tab2 -tab3 tab3 -tab4 tab4 -tab5 tab5 -tab6 tab6 -tab7 tab7
!. ou
!.    ui_atm_msis90 -fic fichier_contenant_la_sequence_d_appel 
!
!>E  jour             : <integer>          jour (JJ50)
!>E  sec              : <PM_REEL>          secondes dans le jour
!>E  long             : <pm_reel>          longitude (rad)
!>E  lat              : <pm_reel>          latitude (rad)
!>E  alt              : <pm_reel>          altitude (m)
!>E  flux_veille      : <pm_reel>          flux du jour précédent
!>E  flux_3rot        : <pm_reel>          flux moyen sur les 3 dernières rotations
!>E  heure            : <pm_reel>          heure solaire (rad)
!>E  tab_ap           : <pm_reel,DIM=(7)>  évolution de l'activité de l'indice géomagnétique (60h)
!
!>S  dens             : <pm_reel>          masse volumique (kg/m^3)
!>S  temp             : <pm_reel>          température (K)
!
!$Auteur
!  Sandrine Baudrand (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_msis90.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_msis90.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.10  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.9  2010/11/02 14:23:41  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.8  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.7  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.6  2008/10/28 14:11:42  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.5  2008/10/03 07:22:04  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.4  2008/07/04 12:22:54  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.3  2008/04/03 18:03:25  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.2  2007/10/31 17:59:00  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.1  2007/10/30 11:09:19  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!
!
!
!#V
!>  date             : <tm_jour_sec>      date de calcul
!>  flux_veille      : <pm_reel>          flux solaire du jour précéden
!>  flux_3rot        : <pm_reel>          flux solaire moyen sur les 3 dernières rotations de la Terre
!>  lat              : <pm_reel>          latitude (rad)
!>  long             : <pm_reel>          longitude (rad)
!>  tab_ap           : <pm_reel,DIM=(7)>  évolution de l'activité de l'indice géomagnétique (60h précédentes)
!>  alt              : <pm_reel>          altitude (m)
!>  heure            : <pm_reel>          haure solaire (rad)
!>  dens             : <pm_reel>          masse volumique (kg/m-3)
!>  temp             : <pm_reel>          température (K)
!>  messages         : <MSP_MESSAGE>      
!>  sec              : <PM_REEL>          seconde
!>  jour             : <integer>          jour 
!>  noptions         : <integer>          
!>  ii               : <integer>          
!>  iargc            : <integer>          
!>  ierfin           : <integer>          
!>  l_opt            : <LEN=20,DIM=(50)>  
!>  logjour          : <logical>          
!>  logsec           : <logical>       
!>  logflux_veille   : <logical>          
!>  logflux_3rot     : <logical>          
!>  loglat           : <logical>          
!>  logalt           : <logical>          
!>  logheure         : <logical>          
!>  loglong          : <logical>          
!>  ok               : <logical>          
!>  logtab           : <logical,DIM=(7)>  
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_calculer_msis90
!- MSP_recuperer_message
!- MSP_afficher_message
!#
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_acces
!#
!
!#V
!#
!
!$FinHistorique
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
!  fonction :  cps_calculer_msis90
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_msis90.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  real(KIND=PM_REEL) :: sec
  integer :: jour
  real(kind=pm_reel) :: flux_veille 
  real(kind=pm_reel) :: flux_3rot  
  real(kind=pm_reel) :: lat, long, alt
  real(kind=pm_reel), dimension(7) :: tab_ap
  real(kind=pm_reel) :: heure  
  
! sortie
  real(kind=pm_reel) :: dens, temp 

! variables locales
  type(tm_jour_sec) :: date 
  type(MSP_MESSAGE) :: messages

  integer :: noptions, ii, jj, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logjour, logsec, logflux_veille, logflux_3rot
  logical :: loglat, logalt, logheure, loglong, ok
  logical, dimension(7) :: logtab

  integer :: numfich=11

! initialisation
  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_msis90") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif


  logjour =.false.
  logsec =.false.
  logflux_veille = .false.
  logflux_3rot = .false.
  do ii = 1, 7
     logtab(ii) = .false.
  enddo
  logheure = .false.
  logalt = .false.
  loglong = .false.
  loglat = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_msis90")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
         read(unit=numfich,fmt=*) (l_opt(jj),jj=1,30)
         noptions=15
      else
         ! Lecture des arguments : 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_msis90")
            goto 999
         endif
      endif
      
      do ii=1, noptions
         if (l_opt(2*ii-1)=="-jour") then
            read(l_opt(2*ii), fmt='(i5)') jour
            logjour = .true.
         elseif(l_opt(2*ii-1)=="-sec") then
            read(l_opt(2*ii), fmt=*) sec
            logsec=.true.
         elseif(l_opt(2*ii-1)=="-long") then
            read(l_opt(2*ii), fmt=*) long
            loglong=.true.
         elseif(l_opt(2*ii-1)=="-lat") then
            read(l_opt(2*ii), fmt=*) lat
            loglat=.true.
         elseif(l_opt(2*ii-1)=="-alt") then
            read(l_opt(2*ii), fmt=*) alt
            logalt = .true.
         elseif(l_opt(2*ii-1)=="-flux_veille") then
            read(l_opt(2*ii), fmt=*) flux_veille
            logflux_veille=.true.
         elseif(l_opt(2*ii-1)=="-flux_3rot") then
            read(l_opt(2*ii), fmt=*) flux_3rot
            logflux_3rot=.true.
         elseif(l_opt(2*ii-1)=="-heure") then
            read(l_opt(2*ii), fmt=*) heure
            logheure = .true.
         elseif(l_opt(2*ii-1)=="-tab1") then
            read(l_opt(2*ii), fmt=*) tab_ap(1)
            logtab(1) = .true.
         elseif(l_opt(2*ii-1)=="-tab2") then
            read(l_opt(2*ii), fmt=*) tab_ap(2)
            logtab(2) = .true.
         elseif(l_opt(2*ii-1)=="-tab3") then
            read(l_opt(2*ii), fmt=*) tab_ap(3)
            logtab(3) = .true.
         elseif(l_opt(2*ii-1)=="-tab4") then
            read(l_opt(2*ii), fmt=*) tab_ap(4)
            logtab(4) = .true.
         elseif(l_opt(2*ii-1)=="-tab5") then
            read(l_opt(2*ii), fmt=*) tab_ap(5)
            logtab(5) = .true.
         elseif(l_opt(2*ii-1)=="-tab6") then
            read(l_opt(2*ii), fmt=*) tab_ap(6)
            logtab(6) = .true.
         elseif(l_opt(2*ii-1)=="-tab7") then
            read(l_opt(2*ii), fmt=*) tab_ap(7)
            logtab(7) = .true.
         endif
      enddo
      
! corps du programme

! Initialisations preliminaires
      ok = .true.
      do ii = 1, 7
         if (.not.logtab(ii)) ok =.false.
      enddo

      if(logjour.and.loglat.and.logsec.and.logalt.and.logflux_veille &
           .and.logflux_3rot.and.logheure.and.loglong.and.ok) then
         date%jour = jour
         date%sec = sec         
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         call cps_calculer_msis90 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt, &
	 heure, dens,temp=temp)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
     	 write(*,'(a,e15.9)') "Masse volumique (kg/m3) = ",dens
     	 write(*,'(a,e15.9)') "Température     (K)     = ",temp
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

  end program ui_atm_msis90
