program ui_atm_mars90

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_mars90
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère MARS90
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère martien MARS90
! Utilisation de cps_atmophere.F90 : cps_atmars90()
!
!
! Usage
!.     ui_atm_mars90 -indatm indatm -coef coef -z z
!
!>E     indatm  :<integer>             indice pour le modèle : 
!.                                1 : modèle nominal      
!.                                2 : modèle maximal       
!.                                3 : modèle minimal
!>E     coeff   :<PM_REEL>             coefficient multiplicatif de r        
!>E     z       :<PM_REEL>             altitude planétocentrique/Mars sphérique (m)
!>S     t       :<PM_REEL,DIM=(out)>   température (k)      
!>S     p       :<PM_REEL>             pression (pa)           
!>S     r       :<PM_REEL>             masse volumique (kg/m3)               
!>S     a       :<PM_REEL>             vitesse du son (m/s)            
!>S     xmu     :<PM_REEL>             viscosité dynamique (kg/m/s)        
!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_mars90.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_mars90.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.11  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.10  2010/11/02 14:23:26  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.9  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.8  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.7  2009/09/09 07:55:50  cmartel
!  FA-ID 1150 : Correction mineures sur les sorties des utilitaires
!
!  Revision 1.6  2008/10/28 14:11:40  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.5  2008/10/03 07:22:02  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.4  2008/07/11 12:08:22  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.3  2008/07/04 12:23:46  huec
!  DM-ID 1058 : Gestion de la memoire, appel a cps_close_utilisateur
!
!  Revision 1.2  2008/04/03 18:02:55  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.1  2007/10/30 11:09:26  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!
!
!#V
!#
!>     indatm  :<integer>             indice pour le modele : 
!.                                1 : modèle nominal      
!.                                2 : modèle maximal       
!.                                3 : modèle minimal
!>     coeff   :<PM_REEL>             coefficient multiplicatif de r        
!>     z       :<PM_REEL>             altitude planétocentrique/Mars sphèrique (m)
!>     t       :<PM_REEL,DIM=(out)>   température (k)      
!>     p       :<PM_REEL>             pression (pa)           
!>     r       :<PM_REEL>             masse volumique (kg/m3)               
!>     a       :<PM_REEL>             vitesse du son (m/s)            
!>     xmu     :<PM_REEL>             viscosité dynamique (kg/m/s)        
!>     ier     :<integer>             test d' erreur (OK si 0)
!
!#V
      
!#
!
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_atmosphere
!- cps_acces
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_atmosphere
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
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_mars90.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  ! indice pour le modele
  integer :: indatm  
  ! coefficient multiplicatif de r  
  real(kind=PM_REEL) :: coef
  ! altitude planétocentrique/Mars sphèrique (m)
  real(kind=PM_REEL)::  z 

! variables locales
  real(kind=PM_REEL) :: t,p,r,a,xmu
  integer :: ier

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin
  character(len=60),dimension(50):: l_opt
  logical :: logindatm, logcoef, logz

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_mars90") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logindatm =.false.
  logcoef =.false.
  logz =.false.

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_mars90")
         goto 999
      else
         ! Lecture des arguments :
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_mars90")
            goto 999
         else
            do ii=1, noptions
               if (l_opt(2*ii-1)=="-indatm") then
                  read(l_opt(2*ii), fmt=*) indatm
                  logindatm = .true.
               elseif(l_opt(2*ii-1)=="-coef") then
                  read(l_opt(2*ii), fmt=*) coef
                  logcoef=.true.
               elseif(l_opt(2*ii-1)=="-z") then
                 read(l_opt(2*ii), fmt=*) z
                  logz=.true.
               endif
            enddo
         endif
         
      endif

! corps du programme

! Initialisations preliminaires

      if(logindatm.and.logcoef.and.logz) then
           
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_atmars90 (indatm,coef,z,t,p,r,a,xmu,ier)
        
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
          
         ! sortie écran
  	 write(*,'(a,e15.9)') "Vitesse du son      (m/s)    = ",a
  	 write(*,'(a,e15.9)') "Pression            (Pa)     = ",p
  	 write(*,'(a,e15.9)') "Masse volumique     (kg/m3)  = ",r
  	 write(*,'(a,e15.9)') "Viscosité dynamique (kg/m/s) = ",xmu
  	 write(*,'(a,e15.9)') "Température         (K)      = ",t

      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"

      endif

999 continue
       if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_close_utilisateur()

  end program ui_atm_mars90
