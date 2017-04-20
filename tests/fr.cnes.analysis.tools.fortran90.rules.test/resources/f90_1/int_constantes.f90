module int_constantes

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème C
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!                     [DR1] "Les systemes de reference utilises en astronomie"
!                            de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!                     [DR2} "Report of the IAU/IAG working group on cartographic
!                            coordinates and rotational elements of the planets 
!                            and satellites: 2000"
!                            par P.K. Seidelmann et al.
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!  + Version 6.6 : DM-ID 616 (option) : inclusion des constantes des modules
!     en vue de leur suppression : GRGS1980_mslib,  math_mslib, phys_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!  
!  + Version 6.9 : DM-ID 1092  : Ajout des constantes IERS :
!                                pm_as_rad, pm_DJC, pm_DM1092_TerVrai_CIP_cte1,
!                                pm_DM1092_TerVrai_CIP_cte2
!                   (Date: 05/2007 - Realisation: Atos origin)  
!
!  + VERSION:6.12:FA-ID:1326:26/05/2010: Correction d'une constante IERS "pm_DM1092_TerVrai_CIP_cte2"
!                   (Realisation: Atos origin) 
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
  use precision_mslib           ! definition des precisions retenues
  use longueur_chaine_mslib

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_constantes.f90 362 2013-02-15 18:01:28Z bbjc $'


! Constantes du modèle GRS1980
!-----------------------------
real(pm_reel), parameter :: pm_r_equa_GRS1980   = 6.378137e6_pm_reel                    ! rayon equatorial de la terre (m)
real(pm_reel), parameter :: pm_inv_apla_GRS1980 = 2.98257222101e2_pm_reel               ! inverse de l'aplatissement de la terre
real(pm_reel), parameter :: pm_apla_GRS1980     = 1._pm_reel/pm_inv_apla_GRS1980       ! aplatissement de la terre

! Constantes matéhmatiques générales
!------------------------------------
  real(pm_reel),parameter :: pm_pi      = 3.1415926535897932384626433832795028841972_pm_reel
  real(pm_reel),parameter :: pm_deux_pi = 6.2831853071795864769252867665590057683943_pm_reel
  real(pm_reel),parameter :: pm_pi_sur2 = 1.5707963267948966192313216916397514420986_pm_reel
  real(pm_reel),parameter :: pm_deg_rad = 0.017453292519943295769236907684886127134429_pm_reel
  real(pm_reel),parameter :: pm_rad_deg = 57.295779513082320876798154814105170332405_pm_reel
  real(pm_reel),parameter :: pm_as_rad = 4.848136811095359935899141E-6_pm_reel

! Constantes diverses
!--------------------
  real(kind=pm_reel), parameter :: pm_DJC = 36525._pm_reel

real(pm_reel),parameter :: pm_ua       = 1.495987e8_pm_reel   ! unite astronomique (km)

real(pm_reel),parameter :: pm_vit_lum  = 2.99792458e8_pm_reel ! vitesse de la lumiere (m/s)

! Inclinaisons critiques, solution de 1 - 5*cos(i))*cos(i) = 0

real(pm_reel),parameter :: pm_i_critique_non_retro = 1.1071487177940905030170654601785370400700476454014_pm_reel ! i critique non retrograde (rad)

real(pm_reel),parameter :: pm_i_critique_retro     = 2.0344439357957027354455779231009658441271217539737_pm_reel ! i critique retrograde (rad)

real(pm_reel), parameter :: pm_DM1092_TerVrai_CIP_cte1=0.7790572732640_pm_reel ! constantes pour les fonctions mr_TerVrai_CIP_iers2003.f90
                                                                               ! et mr_CIP_TerVrai_iers2003.f90 :
									       ! teta=2-pi*(t_frac+cte1+cte2*t)

real(pm_reel), parameter :: pm_DM1092_TerVrai_CIP_cte2=0.00273781191135448_pm_reel ! cf pm_DM1092_TerVrai_CIP_cte1 (au-dessus)



! Valeur de l'obliquite du pole moyen de la Terre a l'epoque J2000
! cf reference [DR1] p91 (IERS 1992):  23 deg, 26 min, 21.4119 secondes

real(pm_reel), parameter :: pm_obliquite2000 = 0.40909262920459006_pm_reel

! Rayon equatorial (m) et aplatissement pour les planetes du systeme solaire
! cf reference [DR2] table IV
! --------------------------------------------------------------------------

! mercure
real(pm_reel), parameter :: pm_mercure_r_equa_UAI   = 2439.7e3_pm_reel
real(pm_reel), parameter :: pm_mercure_apla_UAI     = 0._pm_reel

! venus
real(pm_reel), parameter :: pm_venus_r_equa_UAI   = 6051.8e3_pm_reel
real(pm_reel), parameter :: pm_venus_apla_UAI     = 0._pm_reel

! Terre
real(pm_reel), parameter :: pm_terre_r_equa_UAI   = 6378.14e3_pm_reel
real(pm_reel), parameter :: pm_terre_apla_UAI     = (pm_terre_r_equa_UAI-6356.75e3_pm_reel)/pm_terre_r_equa_UAI

! mars
real(pm_reel), parameter :: pm_mars_r_equa_UAI   = 3396.19e3_pm_reel
real(pm_reel), parameter :: pm_mars_apla_UAI     = (pm_mars_r_equa_UAI-3376.20e3_pm_reel)/pm_mars_r_equa_UAI ! moyen

! jupiter
real(pm_reel), parameter :: pm_jupiter_r_equa_UAI   = 71492.e3_pm_reel
real(pm_reel), parameter :: pm_jupiter_apla_UAI     = (pm_jupiter_r_equa_UAI-66854.e3_pm_reel)/pm_jupiter_r_equa_UAI

! saturne
real(pm_reel), parameter :: pm_saturne_r_equa_UAI   = 60268.e3_pm_reel
real(pm_reel), parameter :: pm_saturne_apla_UAI     = (pm_saturne_r_equa_UAI-54364.e3_pm_reel)/pm_saturne_r_equa_UAI

! uranus
real(pm_reel), parameter :: pm_uranus_r_equa_UAI   = 25559.e3_pm_reel
real(pm_reel), parameter :: pm_uranus_apla_UAI     = (pm_uranus_r_equa_UAI-24973.e3_pm_reel)/pm_uranus_r_equa_UAI

! neptune
real(pm_reel), parameter :: pm_neptune_r_equa_UAI   = 24764.e3_pm_reel
real(pm_reel), parameter :: pm_neptune_apla_UAI     = (pm_neptune_r_equa_UAI-24341.e3_pm_reel)/pm_neptune_r_equa_UAI

! pluton
real(pm_reel), parameter :: pm_pluton_r_equa_UAI   = 1195.e3_pm_reel
real(pm_reel), parameter :: pm_pluton_apla_UAI     = 0._pm_reel



public
interface
     subroutine mc_GRS1980 ( code_retour, r_equa, inv_apla, apla )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_code_retour), intent(out)        :: code_retour     !code retour
real(pm_reel), intent(out), optional     :: r_equa          ! rayon de la Terre (m)
real(pm_reel), intent(out), optional     :: inv_apla        ! inverse de l'aplatissement terrestre
real(pm_reel), intent(out), optional     :: apla            ! aplatissement terrestre


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_GRS1980
     subroutine mc_math (code_retour, pi, deux_pi, pi_sur2, deg_rad, rad_deg)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(out)        :: code_retour !code retour                                                      
real(pm_reel), intent(out), optional   :: pi         !     pi
real(pm_reel), intent(out), optional   :: deux_pi    !     2*pi
real(pm_reel), intent(out), optional   :: pi_sur2    !     pi/2
real(pm_reel), intent(out), optional   :: deg_rad    !     constante de conversion degres -> radians
real(pm_reel), intent(out), optional   :: rad_deg    !     constante de conversion radians-> degres


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_math
     subroutine mc_phys ( code_retour, ua, vit_lum, i_critique_non_retro, i_critique_retro )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_code_retour), intent(out)        :: code_retour          ! code retour
real(pm_reel), intent(out), optional     :: ua                   ! unite astronomique (km)
real(pm_reel), intent(out), optional     :: vit_lum              ! celerite (m/s)
real(pm_reel), intent(out), optional     :: i_critique_non_retro ! inclinaison critique non retrograde solution de 1-5cos^2(i)=0
real(pm_reel), intent(out), optional     :: i_critique_retro     ! inclinaison critique retrograde solution de 1-5cos^2(i)=0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_phys
     subroutine mc_positionner_wahr5 (code_retour, drapeau_wahr5)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(out)       :: code_retour
logical , optional, intent(in)          :: drapeau_wahr5


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_positionner_wahr5
     subroutine mc_recuperer_wahr5 (code_retour, drapeau_wahr5)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(out)       :: code_retour
logical , optional, intent(out)         :: drapeau_wahr5


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_recuperer_wahr5
     subroutine mc_test ( code_retour, eps_cir, eps_parab, eps_equa, eps_i_critique )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type(tm_code_retour), intent(out)        :: code_retour !code retour
                                                       
real(pm_reel), intent(out),optional      :: eps_cir        ! epsilon de test pour orbite circulaire
real(pm_reel), intent(out),optional      :: eps_parab      ! epsilon de test pour orbite parabolique
real(pm_reel), intent(out),optional      :: eps_equa       ! epsilon de test pour orbite equatoriale
real(pm_reel), intent(out),optional      :: eps_i_critique ! epsilon de test pour inclinaisons critiques


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mc_test
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_constantes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_constantes
