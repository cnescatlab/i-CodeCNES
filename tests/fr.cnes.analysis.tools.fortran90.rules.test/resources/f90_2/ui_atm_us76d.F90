program ui_atm_us76d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_us76d
!
!$Resume
!  Programme de calcul du modèle d'atmosphère US76D
!$Description
!  Programme de calcul du modèle d'atmosphère US76D
!
! Usage
!.    ui_atm_us76d  -delat1 delat1 -delta2 delta2 -delta3 delta3 -delta4 delta4 
!.                  -delta5 delta5 -delta6 delta6 -delta7 delta7 -alt alt [-delta_dens delta_dens]
!
!
!>E  delta_t         : <pm_reel,DIM=(0:7)>  delta à ajouter sur les températures
!>E  alt             : <pm_reel>            altitude (m)
!>[E]  delta_dens      : <pm_reel>          delta pour la variation de masse volumique
!
!>S  dens            : <pm_reel>            masse volumique (kg/m3)
!>S  vit_son         : <pm_reel>            vitesse du son (m/s)
!>S  temp            : <pm_reel>            température (K)
!>S  pres            : <pm_reel>            pression (Pa)
!>S  visco           : <pm_reel>            viscosité dynamique (kg/m/sec)
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_us76d.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_us76d.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.22  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.21  2010/11/02 14:23:56  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.20  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.19  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.18  2009/01/28 14:10:10  cml
!  AQ : Correction des unites pour l affichage
!
!  Revision 1.17  2008/10/28 14:11:43  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.16  2008/10/03 07:22:05  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.15  2008/07/04 12:22:37  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.14  2008/04/04 18:00:30  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.13  2008/04/03 18:03:27  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.12  2007/11/13 14:39:35  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!
!  Revision 1.11  2007/07/04 12:22:45  vivaresf
!  FA-ID 664 : majuscules pour la présentation
!
!  Revision 1.10  2007/01/22 15:13:50  fabrec
!  FA-ID 672 : mise a jour suite aux modifications de cps_atm_terre.G de COMPAS_UI
!
!  Revision 1.9  2006/11/20 08:13:22  vpg
!  Mise a jour des cartouches
!
!  Revision 1.8  2006/10/24 09:31:21  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!
!  Revision 1.7  2006/10/18 09:53:55  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!
!  Revision 1.6.2.1  2006/09/26 12:15:40  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!
!  Revision 1.6  2006/05/23 09:32:58  vivaresf
!  DM-ID 387 (lot 5) : mise au point des utilitaires
!  - simplification du code
!  - suppression des variables inutilisées
!
!  Revision 1.5  2006/05/12 12:06:13  bouillaj
!  Amelioration qualite : complements sur les cartouches
!
!  Revision 1.4  2006/05/02 09:38:39  vpg
!  Suppression des variables non utilisees
!
!  Revision 1.3  2006/04/25 09:19:57  bouillaj
!  Correction pour la prise en compte de 8 termes sur les deltaTM
!
!  Revision 1.2  2006/02/10 10:27:34  bouillaj
!  Mise a jour pour creation ihm
!
!  Revision 1.1  2006/01/30 09:10:15  bouillaj
!  creation de l utilitaire pour tester le modele d atmosphere correspondant
!
!
!#V
!>  delta_t         : <pm_reel,DIM=(0:7)>  delta à ajouter sur les températures
!>  alt             : <pm_reel>            altitude (m)
!>  dens            : <pm_reel>            masse volumique (kg/m3)
!>  code_retour     : <tm_code_retour>     
!>  delta_dens      : <pm_reel>            delta pour la variation de masse volumique
!>  vit_son         : <pm_reel>            vitesse du son (m/sec)
!>  temp            : <pm_reel>            température (K)
!>  pres            : <pm_reel>            pression (Pa)
!>  visco           : <pm_reel>            viscosité dynamique (kg/m/sec)
!>  messages        : <MSP_MESSAGE>        
!>  noptions        : <integer>            
!>  ii              : <integer>            
!>  iargc           : <integer>            
!>  ierfin          : <integer>            
!>  l_opt           : <LEN=20,DIM=(50)>    
!>  logdelta_dens   : <logical>            
!>  logalt          : <logical>            
!>  ok              : <logical>            
!>  logdelta_t      : <logical,DIM=(7)>    
!#
!
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atm_us76d
!- MSP_recuperer_message
!- MSP_afficher_message
!#
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atm_us76d_mod
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
  use cps_atm_us76d_mod
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_us76d.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées/sorties
    real(pm_reel),dimension(8) :: delta_t 
    real(pm_reel) :: alt 
    real(pm_reel) :: dens 
    type(tm_code_retour) :: code_retour 
    real(pm_reel) :: delta_dens 
    real(pm_reel) :: vit_son 
    real(pm_reel) :: temp 
    real(pm_reel) :: pres 
    real(pm_reel) :: visco 

! variables locales

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logdelta_dens, logalt, ok
  logical, dimension(8) :: logdelta_t
  integer :: numfich=11

! initialisation

  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_us76d") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logdelta_dens =.false.
  do ii = 1, 8
     logdelta_t(ii) = .false.
  enddo
  logalt = .false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_us76d")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
         read(unit=numfich,fmt=*) (l_opt(jj),jj=1,20)
         noptions=10
      else
         ! Lecture des arguments : 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_us76d")
            goto 999
         endif
      endif

      do ii=1, noptions
         if (l_opt(2*ii-1)=="-delta_dens") then
            read(l_opt(2*ii), fmt=*) delta_dens
            logdelta_dens = .true.
         elseif(l_opt(2*ii-1)=="-delta0") then
            read(l_opt(2*ii), fmt=*) delta_t(1)
            logdelta_t(1) = .true.
         elseif(l_opt(2*ii-1)=="-delta1") then
            read(l_opt(2*ii), fmt=*) delta_t(2)
            logdelta_t(2) = .true.
         elseif(l_opt(2*ii-1)=="-delta2") then
            read(l_opt(2*ii), fmt=*) delta_t(3)
            logdelta_t(3) = .true.
         elseif(l_opt(2*ii-1)=="-delta3") then
            read(l_opt(2*ii), fmt=*) delta_t(4)
            logdelta_t(4) = .true.
         elseif(l_opt(2*ii-1)=="-delta4") then
            read(l_opt(2*ii), fmt=*) delta_t(5)
            logdelta_t(5) = .true.
         elseif(l_opt(2*ii-1)=="-delta5") then
            read(l_opt(2*ii), fmt=*) delta_t(6)
            logdelta_t(6) = .true.
         elseif(l_opt(2*ii-1)=="-delta6") then
            read(l_opt(2*ii), fmt=*) delta_t(7)
            logdelta_t(7) = .true.
         elseif(l_opt(2*ii-1)=="-delta7") then
            read(l_opt(2*ii), fmt=*) delta_t(8)
            logdelta_t(8) = .true.
         elseif(l_opt(2*ii-1)=="-alt") then
            read(l_opt(2*ii), fmt=*) alt
            logalt = .true.
         endif
      enddo
         

! corps du programme

! Initialisations preliminaires
      ok = .true.
      do ii = 1, 8
         if (.not.logdelta_t(ii)) ok =.false.
      enddo

      if(logalt.and.ok.and.logdelta_dens) then  
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         call cps_atm_us76d(delta_t,alt,dens,code_retour,delta_dens=delta_dens, &
              vit_son=vit_son,temp=temp,pres=pres,visco=visco)
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
         
         ! sortie écran
         write (*,1002) "Masse volumique     (kg/m3)  = ", dens
         write (*,1002) "Vitesse du son      (m/s)    = ", vit_son
         write (*,1002) "Pression            (Pa)     = ", pres
         write (*,1002) "Température         (K)      = ", temp
         write (*,1002) "Viscosité dynamique (kg/m/s) = ", visco
      elseif(logalt.and.ok) then
         
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         call cps_atm_us76d(delta_t,alt,dens,code_retour, &
              vit_son=vit_son,temp=temp,pres=pres,visco=visco)     
         
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
         
         ! sortie écran
         write (*,1002) "Masse volumique     (kg/m3)  = ", dens
         write (*,1002) "Vitesse du son      (m/s)    = ", vit_son 
         write (*,1002) "Pression            (Pa)     = ", pres
         write (*,1002) "Température         (K)      = ", temp
         write (*,1002) "Viscosité dynamique (kg/m/s) = ", visco
         
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"
         goto 999
      endif
      
999   continue
      if (MSP_PROBLEME) then
         call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
         call MSP_afficher_message (message=messages,unit=0)
      endif

      call cps_close_utilisateur()

1002 FORMAT(a,g21.12)
      
    end program ui_atm_us76d
    
