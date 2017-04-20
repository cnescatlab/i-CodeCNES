module parametres_internes_mslib

! (C) Copyright CNES - MSLIB - 2003-2005

!************************************************************************
!
! But:  Definition des parametres internes de la MSLIB90
! ===  
!
! Note d'utilisation: Documents de reference:
! ==================  [DR1] : "Report of the IAU/IAG working group on cartographic
!                              coordinates and rotational elements of the planets 
!                              and satellites: 2000"
!                              par P.K. Seidelmann et al.
!                     [DR2] : AD
!
!$Historique
! ==========
!   + Version 5.0 (SP 499 ed01 rev00): creation
!                         (Date: 10/2003 - Bruno Revelin)
!   + Version 6.2 (DE globale 1): creation de parameter communs au theme R
!                         (Date: 01/2005 - Realisation: Guylaine Prat)
!   + Version 6.5 : FA-ID 545 : calcul du temps sidéral UAI2000 pour Mars
!                         (Date: 08/2006 - Realisation: Claire Fabre - Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib (supprimé) par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1092 : Ajout d une constante pour routines IERS 2003
!                   (Date: 09/2008 - Realisation: Atos origin)
!   + Version 6.13 : FA-ID 1428 : Correction pm_i_uai2000_mars_w
!   + VERSION::FA-ID:1428:20/09/2010:Correction du parametre pm_i_uai2000_mars_w
!                   (Date: 09/2010 - Realisation: Atos origin)
!
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres
use precision_mslib

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: parametres_internes_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


! parametres mathematiques
! ------------------------

! definition des axes X,Y,Z
real(pm_reel), parameter, dimension(3)  ::  pm_i_axe_x=(/1._pm_reel,0._pm_reel,0._pm_reel/) ! init de l'axe X
real(pm_reel), parameter, dimension(3)  ::  pm_i_axe_y=(/0._pm_reel,1._pm_reel,0._pm_reel/) ! init de l'axe Y
real(pm_reel), parameter, dimension(3)  ::  pm_i_axe_z=(/0._pm_reel,0._pm_reel,1._pm_reel/) ! init de l'axe Z

! parametres physiques
! --------------------

! ==============================
! constantes diverses
! ==============================
real(pm_reel), parameter :: pm_i_delta_TAI_TE = 32.184_pm_reel ! difference TAI-TE

! date julienne CNES au 01-01-2000 a 12h TDB~TE (confondu avec TE)
real(pm_reel), parameter :: pm_i_date_t2000 = 18262.5_pm_reel ! date an 2000

! jours juliens dans un siecles julien
real(pm_reel), parameter :: pm_i_jj_par_cj = 36525._pm_reel ! en j

! 1 / nombre de secondes par jour
real(pm_reel), parameter :: pm_i_unsur86400 = 1._pm_reel/86400._pm_reel ! 1 / sec par j

! 1 / nombre de jours juliens en mille ans (K = kilo = 1000; Kan = 1000 ans)
real(pm_reel), parameter :: pm_i_unsurKanJul = 1._pm_reel/365250._pm_reel ! 1 / 1000 ans en jj

! conversion Secondes d'arc en radians
real(pm_reel), parameter :: pm_i_sec2rad = pm_deg_rad/3600._pm_reel ! " -> rad

! ==============================
! modele de precession de Lieske
! ==============================

! PBD1,PBD2,PBD3: coefficients pour le calcul de dzeta (secondes d'arc)
! PBZ1,PBZ2,PBZ3: coefficients pour le calcul de z (secondes d'arc)
! PBT1,PBT2,PBT3: coefficients pour le calcul de theta (secondes d'arc)
real(pm_reel), parameter :: pm_i_Lieske_PBD1 = 23062.181_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBD2 =    30.188_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBD3 =    17.998_pm_reel

real(pm_reel), parameter :: pm_i_Lieske_PBZ1 = 23062.181_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBZ2 =   109.468_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBZ3 =    18.203_pm_reel

real(pm_reel), parameter :: pm_i_Lieske_PBT1 = 20043.109_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBT2 =   -42.665_pm_reel
real(pm_reel), parameter :: pm_i_Lieske_PBT3 =   -41.833_pm_reel

! derivees premieres des parametres de la precession
real(pm_reel), parameter :: pm_i_dzetad = (3.1e-7_pm_reel * pm_i_unsur86400)
real(pm_reel), parameter :: pm_i_zd     = (3.1e-7_pm_reel * pm_i_unsur86400)
real(pm_reel), parameter :: pm_i_thetad = (2.7e-7_pm_reel * pm_i_unsur86400)

! ==============================
! Temps sideral de Veis
! ==============================

! constante pour le calcul de la derivee du temps sideral modifie (rad/jour)
real(pm_reel), parameter :: pm_i_Veis_eps = 1.7202179573714597e-02_pm_reel ! voir mr_tsid_veis pour plus de details

! derivee du temps sideral modifie (Veis)
real(pm_reel), parameter :: pm_i_Veis_dtsid_sur_dt = (pm_deux_pi + pm_i_Veis_eps)/86400._pm_reel ! d(tsid Veis)/dt

! ==============================================================================
! valeurs des ascension droite alpha0 et declinaison beta0 des axes de rotation 
! de differents astres
! Ces valeurs sont donnees en degres decimales
! ==============================================================================

! modele UAI 2000 - cf reference [DR1] table I

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mercure_alpha0 = (/ &
     281.01_pm_reel , -0.033_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mercure_delta0 = (/ &
     61.45_pm_reel  , -0.005_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mercure_w =      (/ &
     329.548_pm_reel  , 6.1385025_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai2000_venus_alpha0 =   (/ &
     272.76_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai2000_venus_delta0 =   (/ &
     67.16_pm_reel  /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_venus_w =        (/ &
     160.20_pm_reel  , -1.4813688_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_terre_alpha0 =   (/ &
     0._pm_reel , -0.641_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_terre_delta0 =   (/ &
     90._pm_reel  , -0.557_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_terre_w =        (/ &
     190.147_pm_reel  , 360.9856235_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mars_alpha0 =    (/ &
     317.68143_pm_reel , -0.1061_pm_reel /) 
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mars_delta0 =    (/ &
     52.88650_pm_reel  , -0.0609_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_mars_w =         (/ &
     176.630_pm_reel   , 350.89198226_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_jupiter_alpha0 = (/ &
     268.05_pm_reel , -0.009_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_jupiter_delta0 = (/ &
     64.49_pm_reel  , 0.003_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_jupiter_w =      (/ &
     284.95_pm_reel  , 870.5366420_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_saturne_alpha0 = (/ &
     40.589_pm_reel , -0.036_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_saturne_delta0 = (/ &
     83.537_pm_reel  , -0.004_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_saturne_w =      (/ &
     38.90_pm_reel  , 810.7939024_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai2000_uranus_alpha0 =  (/ &
     257.311_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai2000_uranus_delta0 =  (/ &
     -15.175_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_uranus_w =       (/ &
     203.81_pm_reel  , -501.1600928_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai2000_neptune_alpha0 = (/ &
     299.36_pm_reel , 0.70_pm_reel/)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_neptune_delta0 = (/ &
     43.46_pm_reel  , -0.51_pm_reel /)
real(pm_reel), dimension(3), parameter :: pm_i_uai2000_neptune_w =      (/ &
     253.18_pm_reel  , 536.3128492_pm_reel , -0.48_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_neptune_n =      (/ &
     357.85_pm_reel  , 52.316_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai2000_pluton_alpha0 =  (/ &
     313.02_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai2000_pluton_delta0 =  (/ &
     9.09_pm_reel /) 
real(pm_reel), dimension(2), parameter :: pm_i_uai2000_pluton_w =       (/ &
     236.77_pm_reel  , -56.3623195_pm_reel /)

! modele uai 1994 - cf reference [DR2]

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mercure_alpha0 = (/ &
     281.01_pm_reel , -0.033_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mercure_delta0 = (/ &
     61.45_pm_reel  , -0.005_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mercure_w =      (/ &
     329.68_pm_reel  , 6.1385025_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai1994_venus_alpha0 =   (/ &
     272.76_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai1994_venus_delta0 =   (/ &
     67.16_pm_reel  /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_venus_w =        (/ &
     160.20_pm_reel  , -1.4813688_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_terre_alpha0 =   (/ &
     0._pm_reel , -0.641_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_terre_delta0 =   (/ &
     90._pm_reel  , -0.557_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_terre_w =        (/ &
     190.16_pm_reel  , 360.9856235_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mars_alpha0 =    (/ &
     317.681_pm_reel , -0.108_pm_reel /) 
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mars_delta0 =    (/ &
     52.886_pm_reel  , -0.061_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_mars_w =         (/ &
     176.901_pm_reel   , 350.8919830_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_jupiter_alpha0 = (/ &
     268.05_pm_reel , -0.009_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_jupiter_delta0 = (/ &
     64.49_pm_reel  , 0.003_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_jupiter_w =      (/ &
     284.95_pm_reel  , 870.5360000_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_saturne_alpha0 = (/ &
     40.589_pm_reel , -0.036_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_saturne_delta0 = (/ &
     83.537_pm_reel  , -0.004_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_saturne_w =      (/ &
     38.90_pm_reel  , 810.7939024_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai1994_uranus_alpha0 =  (/ &
     257.311_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai1994_uranus_delta0 =  (/ &
     -15.175_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_uranus_w =       (/ &
     203.81_pm_reel  , -501.1600928_pm_reel /)

real(pm_reel), dimension(2), parameter :: pm_i_uai1994_neptune_alpha0 = (/ &
     299.36_pm_reel , 0.7_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_neptune_delta0 = (/ &
     43.46_pm_reel  , -0.51_pm_reel /)
real(pm_reel), dimension(3), parameter :: pm_i_uai1994_neptune_w =      (/ &
     253.18_pm_reel  , 536.3128492_pm_reel , -0.48_pm_reel /)
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_neptune_n =      (/ &
     357.85_pm_reel  , 52.316_pm_reel /)

real(pm_reel), dimension(1), parameter :: pm_i_uai1994_pluton_alpha0 =  (/ &
     313.02_pm_reel /)
real(pm_reel), dimension(1), parameter :: pm_i_uai1994_pluton_delta0 =  (/ &
     9.09_pm_reel /) 
real(pm_reel), dimension(2), parameter :: pm_i_uai1994_pluton_w =       (/ &
     236.77_pm_reel  , -56.3623195_pm_reel /)

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB parametres_internes_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), private, parameter :: rcs_id = &
     ' $Id: parametres_internes_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module parametres_internes_mslib

