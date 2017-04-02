subroutine mr_nuta ( model_nuta, jul1950, nuta, code_retour, delta_tai, offset_nuta, deriv1_nuta, deriv2_nuta)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But: Calcul des NUTAtions en longitude et en obliquite 
! ===
!
! Note d'utilisation:  Le calcul des derivees premieres et secondes de la nutation est en option.
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 280 ed01 rev00): creation a partir de la routine MRNUTA de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 363 ed01 rev00): suppression des tests sur les dates - utilisation de md_joursec_jourfrac - ajout du parametre delta_tai en entree optionnelle
!                         (Date: 11/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.3 (DM-ID 337 ed01 rev02): ajout de la prise en compte des corrections en longitude et obliquite
!                         (Date: 10/2005 - Realisation: Equipe Patrimoine Mécanique spatiale - ATOS ORIGIN)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
!                   (Date: 03/2008 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use int_rep_internes, only : mri_arg_fon
use int_dates, only : md_joursec_jourfrac
use int_dates, only : md_joursec_norme

use int_util_internes, only : mui_dot_product5
use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model_nuta      ! indicateur du modele de nutation 
type(tm_jour_sec), intent(in)       :: jul1950         ! date julienne

type(tm_nuta), intent(out)          :: nuta            ! nutation en longitude et en obliquite
type(tm_code_retour), intent(out)   :: code_retour
real(pm_reel), intent(in),  optional:: delta_tai       ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
type(tm_nuta), intent(in),  optional:: offset_nuta     ! corrections en longitude et obliquite
type(tm_nuta), intent(out), optional:: deriv1_nuta     ! derivee premiere de la nutation en longitude et en obliquite
type(tm_nuta), intent(out), optional:: deriv2_nuta     ! derivee seconde de la nutation en longitude et en obliquite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
!     declarations des constantes
real(pm_reel), parameter  ::  secrd = .48481368110953e-09_pm_reel!     ... passage de 1.e-04 secondes d'arc en radian
real(pm_reel), parameter  ::  cderp = 3.168808781403e-10_pm_reel !     ... constantes utilisees dans les systemes de reference
real(pm_reel), parameter  ::  delta_te_tai = 32.184_pm_reel      !     ... difference entre le temps des ephemerides et le tai
integer, parameter        ::  profil1 = 9, profil2 =106          !     ... dimensions du tableau de coefficients
real (pm_reel)            ::  mu_retour
integer                   ::  ii                                 ! indices de boucle

!     coefficients de l'expression des nutations
integer, parameter   ::  max_cont_card = 30
integer, parameter   ::  nb_colonnes   = 9

! Modele de Wahr
real(pm_reel), dimension(max_cont_card*nb_colonnes), parameter  :: rcoef11 =(/    &
      0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,-171996._pm_reel,-174.2_pm_reel,92025._pm_reel,8.9_pm_reel,& 
      0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel,   2062._pm_reel, 0.2_pm_reel,-895._pm_reel,0.5_pm_reel, &
     -2._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,     46._pm_reel, 0.0_pm_reel, -24._pm_reel,0.0_pm_reel, &
      2._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel, 0._pm_reel,     11._pm_reel, 0.0_pm_reel,   0._pm_reel,0.0_pm_reel, &
     -2._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,     -3._pm_reel, 0.0_pm_reel,   1._pm_reel,0.0_pm_reel, &
      1._pm_reel,-1._pm_reel, 0._pm_reel,-1._pm_reel, 0._pm_reel,     -3._pm_reel, 0.0_pm_reel,   0._pm_reel,0.0_pm_reel, &
      0._pm_reel,-2._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,     -2._pm_reel, 0.0_pm_reel,   1._pm_reel,0.0_pm_reel, &
      2._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel, 1._pm_reel,      1._pm_reel, 0.0_pm_reel,   0._pm_reel,0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,-13187._pm_reel,-1.6_pm_reel,5736._pm_reel,-3.1_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,  1426._pm_reel,-3.4_pm_reel,  54._pm_reel,-0.1_pm_reel, &

      0._pm_reel, 1._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,  -517._pm_reel, 1.2_pm_reel, 224._pm_reel,-0.6_pm_reel, &
      0._pm_reel,-1._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,   217._pm_reel,-0.5_pm_reel, -95._pm_reel, 0.3_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,   129._pm_reel, 0.1_pm_reel, -70._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,    48._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 0._pm_reel,   -22._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 2._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,    17._pm_reel,-0.1_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,   -15._pm_reel, 0.0_pm_reel,   9._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 2._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,   -16._pm_reel,  .1_pm_reel,   7._pm_reel, 0.0_pm_reel, &
      0._pm_reel,-1._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,   -12._pm_reel, 0.0_pm_reel,   6._pm_reel, 0.0_pm_reel, &
     -2._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel,    -6._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &

      0._pm_reel,-1._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,    -5._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel,-2._pm_reel, 1._pm_reel,     4._pm_reel, 0.0_pm_reel,  -2._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,     4._pm_reel, 0.0_pm_reel,  -2._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel,-1._pm_reel, 0._pm_reel,    -4._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 1._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel,-2._pm_reel, 2._pm_reel, 1._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel,-2._pm_reel, 2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel, 1._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 2._pm_reel,-2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel/)

real(pm_reel), dimension(max_cont_card*nb_colonnes), parameter  :: rcoef12 =(/    &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel, -2274._pm_reel,-0.2_pm_reel, 977._pm_reel,-0.5_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,   712._pm_reel, 0.1_pm_reel,  -7._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,  -386._pm_reel,-0.4_pm_reel, 200._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,  -301._pm_reel, 0.0_pm_reel, 129._pm_reel,-0.1_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,  -158._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,   123._pm_reel, 0.0_pm_reel, -53._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel,    63._pm_reel, 0.0_pm_reel,  -2._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,    63._pm_reel, 0.1_pm_reel, -33._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,   -58._pm_reel,-0.1_pm_reel,  32._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,   -59._pm_reel, 0.0_pm_reel,  26._pm_reel, 0.0_pm_reel, &

      1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,   -51._pm_reel,0.0_pm_reel,   27._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,   -38._pm_reel, 0.0_pm_reel,  16._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,    29._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,    29._pm_reel, 0.0_pm_reel, -12._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,   -31._pm_reel, 0.0_pm_reel,  13._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 0._pm_reel,    26._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,    21._pm_reel, 0.0_pm_reel, -10._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel,    16._pm_reel, 0.0_pm_reel,  -8._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel,-2._pm_reel, 1._pm_reel,   -13._pm_reel, 0.0_pm_reel,   7._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 1._pm_reel,   -10._pm_reel, 0.0_pm_reel,   5._pm_reel, 0.0_pm_reel, &

      1._pm_reel, 1._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,    -7._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,     7._pm_reel, 0.0_pm_reel,  -3._pm_reel, 0.0_pm_reel, &
      0._pm_reel,-1._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,    -7._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,    -8._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel,     6._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,     6._pm_reel, 0.0_pm_reel,  -3._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel,    -6._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 1._pm_reel,    -7._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,     6._pm_reel, 0.0_pm_reel,  -3._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 0._pm_reel,-2._pm_reel, 1._pm_reel,    -5._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel/)

real(pm_reel), dimension(max_cont_card*nb_colonnes), parameter  :: rcoef13 =(/    &
      1._pm_reel,-1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,     5._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,    -5._pm_reel, 0.0_pm_reel,   3._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,    -4._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel, 0._pm_reel,     4._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel, 0._pm_reel,    -4._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,    -3._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 0._pm_reel,     3._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel,-1._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,    -3._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel,-1._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,    -3._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
     -2._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,    -2._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &

      3._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,    -3._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      0._pm_reel,-1._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,    -3._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 1._pm_reel, 2._pm_reel, 0._pm_reel, 2._pm_reel,     2._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,    -2._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 1._pm_reel,     2._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel,    -2._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      3._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel,     2._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel, 2._pm_reel,     2._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 0._pm_reel,-4._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &

     -2._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 2._pm_reel, 4._pm_reel, 2._pm_reel,    -2._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel,-4._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 1._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 1._pm_reel,    -1._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
     -2._pm_reel, 0._pm_reel, 2._pm_reel, 4._pm_reel, 2._pm_reel,    -1._pm_reel, 0.0_pm_reel,   1._pm_reel, 0.0_pm_reel, &
     -1._pm_reel, 0._pm_reel, 4._pm_reel, 0._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel,-1._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 1._pm_reel,     1._pm_reel, 0.0_pm_reel,  -1._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 2._pm_reel, 2._pm_reel, 2._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel/)

integer, parameter  ::  nb_tab = int(profil2/max_cont_card)
integer, parameter  ::  profil_total = profil1*profil2

real(pm_reel), dimension(profil_total - max_cont_card*nb_colonnes*nb_tab), parameter  :: rcoef14 =(/    &
      1._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 4._pm_reel,-2._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      3._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 2._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel, 2._pm_reel,-2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
     -1._pm_reel,-1._pm_reel, 0._pm_reel, 2._pm_reel, 1._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &  
      0._pm_reel, 0._pm_reel,-2._pm_reel, 0._pm_reel, 1._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel,-1._pm_reel, 2._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel,-2._pm_reel,-2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel,-1._pm_reel, 2._pm_reel, 0._pm_reel, 1._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 1._pm_reel, 0._pm_reel,-2._pm_reel, 1._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      1._pm_reel, 0._pm_reel,-2._pm_reel, 2._pm_reel, 0._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      2._pm_reel, 0._pm_reel, 0._pm_reel, 2._pm_reel, 0._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 0._pm_reel, 2._pm_reel, 4._pm_reel, 2._pm_reel,    -1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel, &
      0._pm_reel, 1._pm_reel, 0._pm_reel, 1._pm_reel, 0._pm_reel,     1._pm_reel, 0.0_pm_reel,   0._pm_reel, 0.0_pm_reel/)

real(pm_reel),dimension(profil_total), parameter       :: rcoef1 =(/rcoef11(:), rcoef12(:), rcoef13(:), rcoef14(:)/)

! Coefficients de l'expression des nutations pour le modele de Wahr
real(pm_reel),dimension(profil1,profil2)::rcoef2

real(pm_reel)                  ::  temps               !     date en jour julien fractionnaire
real(pm_reel), dimension(5)    ::  arg_fon             !     arguments fondamentaux 
real(pm_reel), dimension(5,2)  ::  arg_deriv           !     et leurs derivees
real(pm_reel), dimension(9)    ::  coeff               !     coefficients de l'expression des nutations
real(pm_reel)                  ::  nuta_lon, nuta_obli !     valeurs intermediaires de la nutation
real(pm_reel), dimension(2,2)  ::  dnuta               !     valeurs intermediaires des derivees
!     variables de travail
real(pm_reel)        ::  rargf1,rargfp,rargfs,rcargf,rsargf,rsecln,rsecob
real(pm_reel)        ::  rargp2,rdcarg,rdsarg,rdern1,rdern2,rdero1,rdero2

integer              ::  j , retour                    ! indice de boucle et code retour de la routine interne
type(tm_jour_sec)    :: joursec_tai                    ! variable intermediaire
type(tm_jour_sec)    :: joursec_norme                  ! variable intermediaire pour l appel a md_joursec_norme
type(tm_jour_sec)    :: jul1950_temp                   ! variable intermediaire pour l appel a md_joursec_jourfrac
type(tm_code_retour) :: code_retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_nuta.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_nuta.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour
! ..........................................
 rcoef2 = reshape(rcoef1, (/profil1,profil2/))
code_retour%valeur = pm_OK

! controle des donnees
! ....................

if (model_nuta /= pm_wahr) then          ! modele de nutation inconnu
   code_retour%valeur = pm_err_ind_nuta
   go to 6000
end if

joursec_tai%jour = jul1950%jour

if (.not. present(delta_tai)) then

   joursec_tai%sec  = jul1950%sec               ! delta_tai est fixe a 0._pm_reel par defaut

else

   joursec_tai%sec  = jul1950%sec + delta_tai   ! si delta_tai est precise par l'appelant

end if

call md_joursec_norme ( joursec_tai, joursec_norme, code_retour_local) ! code retour non teste car toujours = a 0

! les entrees sont correctes : CALCUL DE LA NUTATION
call mri_arg_fon (joursec_norme, arg_fon, retour, arg_deriv=arg_deriv)    ! arguments fondamentaux

! le code retour ne peut etre que nul

! date en siecles juliens
! conversion secondes tai en temps dynamique terrestre (tdt)
jul1950_temp%jour = joursec_norme%jour
jul1950_temp%sec  = joursec_norme%sec + delta_te_tai
call md_joursec_jourfrac ( jul1950_temp, temps, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste
temps = temps / 36525._pm_reel
temps = temps - 0.5_pm_reel

!     initialisations

nuta_lon = 0._pm_reel
nuta_obli = 0._pm_reel
dnuta(1,1) = 0._pm_reel
dnuta(1,2) = 0._pm_reel
dnuta(2,1) = 0._pm_reel
dnuta(2,2) = 0._pm_reel

!     ---------------------------------
!     boucle sur les lignes de la table
!     _________________________________

do  j = 1,profil2

   do ii = 1,profil1
      coeff(ii) = rcoef2(ii,j)      ! transfert des coefficents de la table dans variables de travail
   end do

   !     ... sommation des arguments et de leurs derivees
   !
   call mui_dot_product5 ( arg_fon(:) , coeff(1:5) , rargf1 , mu_retour )
   !
   !     ... nutations

   rcargf = cos(rargf1)
   rsargf = sin(rargf1)

   rsecln = coeff(6) + coeff(7)*temps
   rsecob = coeff(8) + coeff(9)*temps

   nuta_lon = nuta_lon + rsargf*rsecln
   nuta_obli = nuta_obli + rcargf*rsecob

   if ((present(deriv1_nuta)).or.(present(deriv2_nuta))) then 

      call mui_dot_product5 ( arg_deriv(:,1) , coeff(1:5) , rargfp , mu_retour )
   
      rdcarg = -rsargf*rargfp
      rdsarg =  rcargf*rargfp

      if (present(deriv1_nuta)) then

         !     derivee premiere de la nutation en longitude
         rdern1      = rdsarg*rsecln + rsargf*coeff(7)*cderp
         dnuta(1,1)  = dnuta(1,1) + rdern1

         !     derivee premiere de la nutation en obliquite
         rdero1      = rdcarg*rsecob + rcargf*coeff(9)*cderp
         dnuta(2,1)  = dnuta(2,1) + rdero1

      end if

      if (present(deriv2_nuta)) then     ! demande de calcul de la derivee seconde

         rargp2=   rargfp*rargfp
         
         call mui_dot_product5 ( arg_deriv(:,2) , coeff(1:5) , rargfs , mu_retour )

         !     derivee seconde de la nutation en longitude
         rdern2      = ( - rsargf*rargp2 + rcargf*rargfs ) * rsecln
         rdern2      = rdern2 + 2._pm_reel*rdsarg*coeff(7)*cderp
         dnuta(1,2) = dnuta(1,2) + rdern2

         !     derivee seconde de la nutation en obliquite
         rdero2      = -( rcargf*rargp2 + rsargf*rargfs ) * rsecob
         rdero2      = rdero2 + 2._pm_reel*rdcarg*coeff(9)*cderp
         dnuta(2,2)  = dnuta(2,2) + rdero2    

      end if
   end if

end do  ! fin de boucle sur les lignes de la table

! Conversion des secondes d'arc en radians pour les sorties

nuta%long = nuta_lon*secrd
nuta%obli = nuta_obli*secrd

if (present(deriv1_nuta)) then
   deriv1_nuta%long = dnuta(1,1) * secrd
   deriv1_nuta%obli = dnuta(2,1) * secrd
end if

if (present(deriv2_nuta)) then
   deriv2_nuta%long = dnuta(1,2) * secrd
   deriv2_nuta%obli = dnuta(2,2) * secrd
end if

! Corrections en logitude et en obliquite

if (present(offset_nuta)) then
   nuta%long = nuta%long + offset_nuta%long
   nuta%obli = nuta%obli + offset_nuta%obli
end if

6000 continue

code_retour%routine = pm_num_mr_nuta
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_nuta
