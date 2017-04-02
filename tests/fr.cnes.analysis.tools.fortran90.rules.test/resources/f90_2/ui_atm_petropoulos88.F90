program ui_atm_petropolous88

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_petropolous88
!
!$Resume
! Programme de calcul du modèle d'atmosphère PETROPOULOS88
!
!$Description
! Programme d'appel au modèle d'atmosphère PETROPOULOS88
!
!
! Usage
!
!. ui_atm_petropolous88 -alt alt [ -rho rho ] [ -beta beta ]
!
!>E   z    :<pm_reel>   altitude (m)
!>[E] rho0 :<pm_reel>   masse volumique à la hauteur de référence z0, par défaut = 530.0  (kg/m^3)
!>[E] beta :<pm_reel>   constante de Petropoulos, par défaut = 1.58e-04 (1/m)
!
!
!. ou ui_atm_petropolous88 -fic <fichier>
!
!>E   <fichier> : nom d'un fichier contenant les paramètres ci-dessus (attention,
!     ceux-ci doivent être séparés par un seul espace)
!
!$Auteur
!  J.G. Piccinali
!
!$Version
!  $Id: ui_atm_petropoulos88.F90
!
!$Historique
!  $Log: ui_atm_petropoulos88.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.17  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.16  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.15  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.14  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.13  2008/10/28 14:11:43  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.12  2008/10/03 07:22:05  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.11  2008/07/11 12:08:29  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.10  2008/07/04 12:22:45  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.9  2008/04/04 17:10:36  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.8  2008/04/03 18:03:26  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.7  2008/02/07 10:02:25  vivaresf
!  FA-ID 934 : rajout de l'option -fic et mise
!  à jour du cartouche
!
!  Revision 1.6  2007/11/21 16:49:58  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!
!  Revision 1.5  2007/11/13 14:39:33  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!
!  Revision 1.4  2007/11/13 10:40:26  vivaresf
!  DM-ID 744 : validation des cartouches
!
!  Revision 1.3  2007/11/05 12:40:41  jpi
!  DM-ID551 : coquilles (apostrophes, print inutiles)
!
!  Revision 1.2  2007/10/19 12:02:15  jpi
!  DM-ID744 : modele atmosphere Venus petropoulos88 : tests ui_atm_petropoulos88
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
  use cps_atmosphere, only : cps_atm_venus
  use cps_acces
  
  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_petropoulos88.F90 355 2013-02-14 12:16:41Z aadt $'

  
  ! Arguments
  ! =========
  real(kind=pm_reel) :: rho
  real(kind=pm_reel) :: z
  real(kind=pm_reel) :: rho0_optional, beta_optional


  ! Variables
  ! =========
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin, pos
  character(len=80),dimension(50):: l_opt
  character(len=132) :: tmpstr
  logical :: logalt
  logical :: logrhoo, logbetao

  integer :: numfich=11

  ! Initialisation
  ! ==============

  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_petropoulos88") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  logalt     = .false.
  logrhoo    = .false.
  logbetao   = .false.
  noptions   = 0

  ! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

  ! lecture effective
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_atm_petropoulos88")
     goto 999
  elseif(ierfin.eq.0) then
     call ui_lire_options(noptions, l_opt)

     ! Cas 3 options
     open(unit=numfich, file=trim(l_opt(2)))
     read(unit=numfich,fmt='(a132)') tmpstr
     close (numfich)

     do ii=1, 6
        l_opt(ii)=""
        pos=index(tmpstr, " ")
        if (pos>0) l_opt(ii)=tmpstr(1:pos)
        tmpstr=tmpstr(pos+1:)
     enddo

     if (trim(l_opt(1)).ne."") noptions=1
     if (trim(l_opt(3)).ne."") noptions=2
     if (trim(l_opt(5)).ne."") noptions=3
    

  else
     ! Lecture des arguments : 
     call ui_lire_options(noptions, l_opt)
     if (noptions == 0) then
        call ui_ecrire_help("ui_atm_petropoulos88")
        goto 999
     endif
  endif

  do ii=1, noptions
     if (l_opt(2*ii-1)=="-alt") then
        read(l_opt(2*ii), fmt=*) z
        logalt     = .true.
        
     elseif(l_opt(2*ii-1)=="-rho") then
        read(l_opt(2*ii), fmt=*) rho0_optional 
        logrhoo     = .true.
        
     elseif(l_opt(2*ii-1)=="-beta") then
        read(l_opt(2*ii), fmt=*) beta_optional
        logbetao   = .true.
        
     endif
  enddo

  ! Corps du programme
  ! =================

  ! Initialisations preliminaires

  if(logalt) then

     ! erreur sur l'init
     if (MSP_erreur) goto 999

     ! appel effectif
     if (logrhoo) then
        if (logbetao) then
           call cps_atm_venus (z ,rho, rho0_optional, beta_optional)
        else
           call cps_atm_venus (z ,rho, rho0_optional=rho0_optional)
        endif
     else
        if (logbetao) then
           call cps_atm_venus (z ,rho, beta_optional=beta_optional)
        else
           call cps_atm_venus (z ,rho)
        endif
     endif

     ! erreur dans l'appel
     if (MSP_erreur) goto 999

     ! sortie écran
     write(*,'(a)')" "
     write(*,'(a,g21.12)')" Altitude                            (m)     = ", z         
     write(*,'(a,g21.12)')" Masse volumique                     (kg/m3) = ", rho
     if (logrhoo) &
     write(*,'(a,g21.12)')" Masse volumique constante rho0 à z0 (kg/m3) = ", rho0_optional
     if (logbetao) &
     write(*,'(a,g21.12)')" Bêta constant                       (/m)    = ", beta_optional
     write(*,'(a)')" "

  else
     write(0,*) "Erreur : les arguments ne sont pas correctement renseignés."
     goto 999
  endif

999 continue
  if (MSP_PROBLEME) then
     call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif

  call cps_close_utilisateur()

end program ui_atm_petropolous88
