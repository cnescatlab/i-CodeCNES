program ui_atm_dtm78

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_dtm78
!
!$Resume
! Programme de calcul du modèle d'atmosphère DTM78
!
!$Description
!  Programme de calcul du modèle d'atmosphère DTM78
!
!
! Usage
!.   ui_atm_dtm78  -jour jour -sec sec -flux_veille flux_veille -flux_3rot flux_3rot -ap_3h ap_3h 
!.                 -alt alt -lat lat -heure heure
!
!
!>E  sec              : <PM_REEL>          secondes dans le jour
!>E  jour             : <integer>          jour  (JJ50)
!>E  flux_veille      : <pm_reel>          flux solaire du jour précédent
!>E  flux_3rot        : <pm_reel>          flux moyen sur les 3 dernières rotations
!>E  ap_3h            : <pm_reel>          indice géomagnétique ap
!>E  lat              : <pm_reel>          latitude (rad)
!>E  alt              : <pm_reel>          altitude (m)
!>E  heure_sol        : <pm_reel>          heure solaire (rad)
!>S  dens             : <pm_reel>          masse volumique (kg/m3)
!>S  inv_haut_ech     : <pm_reel>          inverse de la hauteur d'échelle (m)
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_dtm78.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_dtm78.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.23  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.22  2010/11/02 14:16:33  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.21  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.20  2010/10/04 11:08:33  ogarat
!  VERSION::FA-ID:1446:04/10/2010: Alignement des signes = dans l'affichage des résultats
!
!  Revision 1.19  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.18  2009/09/09 07:55:29  cmartel
!  AQ : Suppression d'une variable inutilisée
!
!  Revision 1.17  2009/05/28 08:15:05  cml
!  FA-ID 1302 : Amelioration des tests d'erreur dans DTM78
!
!  Revision 1.16  2008/10/28 14:11:37  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.15  2008/10/03 07:21:59  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.14  2008/07/04 12:24:55  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!
!  Revision 1.13  2008/04/04 18:00:29  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.12  2008/04/04 17:10:33  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.11  2008/04/03 18:02:51  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!  Revision 1.10  2007/11/13 14:39:28  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!  Revision 1.9  2007/07/04 12:22:43  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!  Revision 1.8  2006/11/20 08:13:22  vpg
!  Mise a jour des cartouches
!  Revision 1.7  2006/10/24 09:31:20  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.6  2006/10/18 09:53:37  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.5.2.1  2006/09/26 12:15:34  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.5  2006/05/23 09:32:57  vivaresf
!  DM-ID 387 (lot 5) : mise au point des utilitaires
!  - simplification du code
!  - suppression des variables inutilisées
!  Revision 1.4  2006/05/12 12:06:13  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.3  2006/05/02 09:38:38  vpg
!  Suppression des variables non utilisees
!  Revision 1.2  2006/03/23 17:10:07  bouillaj
!  Amelioration qualite
!  Revision 1.1  2006/01/30 09:10:14  bouillaj
!  creation de l utilitaire pour tester le modele d atmosphere correspondant
!#V
!>  date             : <tm_jour_sec>      date (jj 195à)
!>  flux_veille      : <pm_reel>          flux solaire du jour précédent
!>  flux_3rot        : <pm_reel>          flux moyen sur les 3 dernières rotations
!>  ap_3h            : <pm_reel>          indice géomagnétique ap
!>  lat              : <pm_reel>          latitude (rad)
!>  alt              : <pm_reel>          altitude (m)
!>  heure_sol        : <pm_reel>          haure solaire (rad)
!>  dens             : <pm_reel>          masse volumique (kg.m-3)
!>  inv_haut_ech     : <pm_reel>          inverse de la hauteur d'échelle (m)
!>  messages         : <MSP_MESSAGE>      
!>  sec              : <PM_REEL>          seconde
!>  jour             : <integer>          jour
!>  noptions         : <integer>          
!>  ii               : <integer>          
!>  iargc            : <integer>          
!>  ierfin           : <integer>          
!>  lrep_fcf         : <integer>          
!>  l_opt            : <LEN=20,DIM=(50)>  
!>  logjour          : <logical>          
!>  logsec           : <logical>          
!>  logflux_veille   : <logical>          
!>  logflux_3rot     : <logical>          
!>  logap_3h         : <logical>          
!>  loglat           : <logical>          
!>  logalt           : <logical>          
!>  logheure         : <logical>          
!>  rep_fcf          : <LEN=100>          
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atm_dtm78
!- MSP_recuperer_message
!- MSP_afficher_message
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atm_dtm78_mod
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
  use cps_atm_dtm78_mod
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_dtm78.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées/sorties
  type(tm_jour_sec) :: date 
  real(pm_reel) :: flux_veille 
  real(pm_reel) :: flux_3rot 
  real(pm_reel) :: ap_3h 
  real(pm_reel) :: lat 
  real(pm_reel) :: alt 
  real(pm_reel) :: heure_sol 
  real(pm_reel) :: dens 
  real(pm_reel) :: inv_haut_ech 
  

! variables locales

  type(MSP_MESSAGE) :: messages
  real(KIND=PM_REEL) :: sec
  integer :: jour

  integer :: noptions, ii, ierfin
  character(len=20),dimension(50):: l_opt
  logical :: logjour, logsec, logflux_veille, logflux_3rot
  logical :: logap_3h, loglat, logalt, logheure

! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_dtm78") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logjour =.false.
  logsec =.false.
  loglat = .false.
  logflux_veille = .false.
  logflux_3rot = .false.
  logap_3h = .false.
  logheure = .false.
  logalt = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_dtm78")
         goto 999
      else
         ! Lecture des arguments : 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_dtm78")
            goto 999
         else
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
               elseif(l_opt(2*ii-1)=="-flux_veille") then
                  read(l_opt(2*ii), fmt=*) flux_veille
                  logflux_veille=.true.
               elseif(l_opt(2*ii-1)=="-flux_3rot") then
                  read(l_opt(2*ii), fmt=*) flux_3rot
                  logflux_3rot=.true.
               elseif(l_opt(2*ii-1)=="-ap_3h") then
                  read(l_opt(2*ii), fmt=*) ap_3h
                  logap_3h = .true.
               elseif(l_opt(2*ii-1)=="-heure") then
                  read(l_opt(2*ii), fmt=*) heure_sol
                  logheure = .true.
               elseif(l_opt(2*ii-1)=="-alt") then
                  read(l_opt(2*ii), fmt=*) alt
                  logalt = .true.
               endif
            enddo
         endif
         
      endif

! corps du programme

! Initialisations preliminaires

      if(logjour.and.loglat.and.logsec.and.logalt.and.logflux_veille &
           .and.logflux_3rot.and.logap_3h.and.logheure) then
         date%jour = jour
         date%sec = sec         
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         call cps_atm_dtm78 (date,flux_veille,flux_3rot,ap_3h,lat,alt,heure_sol, &
                          dens,inv_haut_ech=inv_haut_ech)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
         write(*,1002) "Masse volumique                (kg/m3) = ", dens
         write(*,1002) "Inverse de la hauteur d'échelle        = ", inv_haut_ech 

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

  end program ui_atm_dtm78
