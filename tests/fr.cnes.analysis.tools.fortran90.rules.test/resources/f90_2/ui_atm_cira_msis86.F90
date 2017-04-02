program ui_atm_cira_msis86

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_cira_msis86
!
!$Resume
!  Programme de calcul du modèle d'atmosphère CIRA_MSIS86
!
!$Description
! Programme de calcul du modèle d'atmosphère CIRA_MSIS86
!
!
! Usage
!.    ui_atm_cira_msis86 -jour jour -sec sec -flux_veille flux_veille  -flux_3rot flux_3rot 
!.                       -tab1 tab1 -tab2 tab2 -tab3 tab3 -tab4 tab4 -tab5 tab5 -tab6 tab6 
!.                       -tab7 tab7 -lat lat   -long long -alt  alt  -heure heure_sol
!
!
!>E  sec              : <PM_REEL>          secondes dans le jour
!>E  jour             : <integer>          jour (JJ50)
!>E  flux_veille      : <pm_reel>          flux du jour précédent
!>E  flux_3rot        : <pm_reel>          flux moyen sur les 3 dernières rotations
!>E  lat              : <pm_reel>          latitude (rad)
!>E  long             : <pm_reel>          longitude (rad)
!>E  tab_ap           : <pm_reel,DIM=(7)>  évolution de l'activité de l'indice géomagnétique (60h)
!>E  alt              : <pm_reel>          altitude (m)
!>E  heure_sol        : <pm_reel>          heure solaire (rad)
!>S  dens             : <pm_reel>          Masse volumique (kg/m3)
!>S  pres             : <pm_reel>          pression (Pa)
!>S  temp             : <pm_reel>          température (K)
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_cira_msis86.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_cira_msis86.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.23  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.22  2010/11/02 14:16:27  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.21  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.20  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.19  2008/10/28 14:11:36  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.18  2008/10/03 07:21:59  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.17  2008/07/11 12:07:45  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.16  2008/07/04 12:25:07  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.15  2008/04/04 18:00:29  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.14  2008/04/04 17:10:33  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.13  2008/04/03 18:02:51  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!  Revision 1.12  2007/11/13 14:39:26  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!  Revision 1.11  2007/07/04 12:22:43  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!  Revision 1.10  2006/11/20 08:13:21  vpg
!  Mise a jour des cartouches
!  Revision 1.9  2006/10/24 09:31:20  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.8  2006/10/18 09:53:14  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.7.2.1  2006/09/26 12:15:32  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.7  2006/08/30 08:55:33  vivaresf
!  FA-ID 576 : maj de l_opt pour la saisie complete des arguments
!  Revision 1.6  2006/05/23 09:32:57  vivaresf
!  DM-ID 387 (lot 5) : mise au point des utilitaires
!  - simplification du code
!  - suppression des variables inutilisées
!  Revision 1.5  2006/05/12 12:06:12  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.4  2006/05/02 09:38:38  vpg
!  Suppression des variables non utilisees
!  Revision 1.3  2006/03/23 17:10:07  bouillaj
!  Amelioration qualite
!  Revision 1.2  2006/02/10 10:26:29  bouillaj
!  Mise a jour pour creation de l ihm
!  Revision 1.1  2006/01/30 09:10:13  bouillaj
!  creation de l utilitaire pour tester le modele d atmosphere correspondant
!#V
!>  date             : <tm_jour_sec>      date (jj1950)
!>  flux_veille      : <pm_reel>          flux du jour précédent
!>  flux_3rot        : <pm_reel>          flux moyen sur les 3 dernières rotations
!>  lat              : <pm_reel>          latitude (rad)
!>  long             : <pm_reel>          longitude (rad)
!>  tab_ap           : <pm_reel,DIM=(7)>  Evolution de l'activité de l'indice géomagnétique (60h)
!>  alt              : <pm_reel>          altitude (m)
!>  heure_sol        : <pm_reel>          heure solaire (rad)
!>  dens             : <pm_reel>          masse volumique (kg.m-3)
!>  pres             : <pm_reel>          pression (Pa)
!>  temp             : <pm_reel>          température (K)
!>  code_retour      : <tm_code_retour>   
!>  messages         : <MSP_MESSAGE>      
!>  sec              : <PM_REEL>          secondes
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
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atm_cira_msis86
!- MSP_recuperer_message
!- MSP_afficher_message
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atm_cira_msis86_mod
!- cps_acces
!#V
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
  use cps_atm_cira_msis86_mod
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_cira_msis86.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées/sorties
  type(tm_jour_sec) :: date 
  real(kind=pm_reel) :: flux_veille 
  real(kind=pm_reel) :: flux_3rot  
  real(kind=pm_reel) :: lat, long
  real(kind=pm_reel), dimension(7) :: tab_ap
  real(kind=pm_reel) :: alt 
  real(kind=pm_reel) :: heure_sol 
  real(kind=pm_reel) :: dens, pres, temp 
  type(tm_code_retour) :: code_retour
  

! variables locales

  type(MSP_MESSAGE) :: messages
  real(KIND=PM_REEL) :: sec
  integer :: jour

  integer :: noptions, ii, jj, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logjour, logsec, logflux_veille, logflux_3rot
  logical :: loglat, logalt, logheure, loglong, ok
  logical, dimension(7) :: logtab
  
  integer :: numfich=11

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_cira_msis86") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

! initialisation
  logjour =.false.
  logsec =.false.
  loglat = .false.
  logflux_veille = .false.
  logflux_3rot = .false.
  do ii = 1, 7
     logtab(ii) = .false.
  enddo
  logheure = .false.
  logalt = .false.
  loglong = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_cira_msis86")
         goto 999
      elseif (ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
         read(unit=numfich,fmt=*) (l_opt(jj),jj=1,30)
         noptions=15
      else
         ! Lecture des arguments : 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_cira_msis86")
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
         elseif(l_opt(2*ii-1)=="-lat") then
            read(l_opt(2*ii), fmt=*) lat
            loglat=.true.
         elseif(l_opt(2*ii-1)=="-long") then
            read(l_opt(2*ii), fmt=*) long
            loglong=.true.
         elseif(l_opt(2*ii-1)=="-flux_veille") then
            read(l_opt(2*ii), fmt=*) flux_veille
            logflux_veille=.true.
         elseif(l_opt(2*ii-1)=="-flux_3rot") then
            read(l_opt(2*ii), fmt=*) flux_3rot
            logflux_3rot=.true.
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
         elseif(l_opt(2*ii-1)=="-heure") then
            read(l_opt(2*ii), fmt=*) heure_sol
            logheure = .true.
         elseif(l_opt(2*ii-1)=="-alt") then
            read(l_opt(2*ii), fmt=*) alt
            logalt = .true.
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
         call cps_atm_cira_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
       dens,code_retour,temp,pres)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
         write (*,1002) "Masse volumique (kg/m3) = ", dens 
         write (*,1002) "Pression        (Pa)    = ", pres
         write (*,1002) "Température     (K)     = ", temp 
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

1002 FORMAT(a,g21.12)

  end program ui_atm_cira_msis86
