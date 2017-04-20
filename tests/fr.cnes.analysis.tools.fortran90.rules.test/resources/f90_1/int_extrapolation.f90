module int_extrapolation
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème E
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_extrapolation.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine me_brouwer( mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2 )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                :: mu          !mu                                               
real(pm_reel), intent(in)                :: r_equa      !rayon equatorial
real(pm_reel), intent(in), dimension(2:5):: cn0         !coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_jour_sec), intent(in)            :: t1          !date t1
type(tm_orb_kep), intent(in)             :: moy_t1      !parametres moyens a la date t1
type(tm_jour_sec), intent(in)            :: t2          !date t2

type(tm_orb_kep), intent(out)            :: moy_t2      !parametres moyens a la date t2   
type(tm_code_retour), intent(out)        :: code_retour !code retour

type(tm_orb_kep), intent(out), optional  :: osc_t2      ! parametres osculateurs a la date t2





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_brouwer
     subroutine me_brouwer_moy( r_equa, cn0, ecart_admi, osc, moy, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                :: r_equa      !rayon equatorial
real(pm_reel), intent(in), dimension(2:5):: cn0         !coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_orb_kep), intent(in)             :: ecart_admi  !ecarts admissibles sur a, e et les angles i, pom, gom et M
type(tm_orb_kep), intent(in)             :: osc         !parametres osculateurs (a,e,i,pom,gom,M)

type(tm_orb_kep), intent(out)            :: moy         !parametres moyens au sens de Brouwer
type(tm_code_retour), intent(out)        :: code_retour !code retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_brouwer_moy
     subroutine me_deriv_secul_j2( mu, r_equa, c20, a, e, i, deriv_pom, deriv_gom, deriv_M, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                            ::  mu          ! constante de la gravitation
real(pm_reel), intent(in)                            ::  r_equa      ! rayon equatorial du corps attractif
real(pm_reel), intent(in)                            ::  c20         ! coefficient zonal C20 (denormalise)
real(pm_reel), intent(in)                            ::  a           ! demi grand axe de l'orbite
real(pm_reel), intent(in)                            ::  e           ! excentricite de l'orbite
real(pm_reel), intent(in)                            ::  i           ! inclinaison de l'orbite

real(pm_reel), intent(out)                           ::  deriv_pom   ! derive seculaire de l'argument du perigee
real(pm_reel), intent(out)                           ::  deriv_gom   ! derive seculaire de la longitude du noeud ascendant
real(pm_reel), intent(out)                           ::  deriv_M     ! derive seculaire de l'anomalie moyenne
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_deriv_secul_j2
     subroutine me_eck_hech( mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2 )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                                  intent(in)  :: mu          ! constante gravitationnelle terrestre
real(pm_reel),                                  intent(in)  :: r_equa      ! rayon equatorial terrestre
real(pm_reel),        dimension(2:6),           intent(in)  :: cn0         ! coeff. zonaux (c20 a c60) du developpement
type(tm_jour_sec),                              intent(in)  :: t1          ! date initiale t1 des parametres moyens (jours, sec)
type(tm_orb_cir),                               intent(in)  :: moy_t1      ! parametres moyens a la date t1
type(tm_jour_sec),                              intent(in)  :: t2          ! date finale t2 des parametres osculateurs (jours, sec)
type(tm_orb_cir),                               intent(out) :: moy_t2      ! parametres moyens a la date t2
type(tm_code_retour),                           intent(out) :: code_retour
type(tm_orb_cir),                     optional, intent(out) :: osc_t2      ! parametres osculateurs extrapoles a t2



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_eck_hech
     subroutine me_eck_hech_moy( r_equa, cn0, ecart_admi, osc, moy, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                        intent(in)  :: r_equa      ! rayon equatorial terrestre
real(pm_reel),        dimension(2:6), intent(in)  :: cn0         ! coefficients C20 a C60 denormalises
type(tm_orb_cir),                     intent(in)  :: ecart_admi  ! ecarts admissibles sur a,ex,ey,i,gom,pom+M
type(tm_orb_cir),                     intent(in)  :: osc         ! parametres osculateurs
type(tm_orb_cir),                     intent(out) :: moy         ! parametres moyens
type(tm_code_retour),                 intent(out) :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_eck_hech_moy
     subroutine me_lyddane (mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2 )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                :: mu          ! constante de la gravitation
real(pm_reel), intent(in)                :: r_equa      ! rayon equatorial terrestre
real(pm_reel), dimension(2:5), intent(in):: cn0         ! coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_jour_sec), intent(in)            :: t1          ! date t1
type(tm_orb_cir_equa), intent(in)        :: moy_t1      ! parametres moyens a la date t1
type(tm_jour_sec), intent(in)            :: t2          ! date t2
type(tm_orb_cir_equa), intent(out)       :: moy_t2      ! parametres moyens a la date t2
type(tm_code_retour), intent(out)        :: code_retour

type(tm_orb_cir_equa), intent(out), optional  :: osc_t2 ! parametres osculateurs a la date t2


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_lyddane
     subroutine me_lyddane_moy ( r_equa, cn0, osc, moy, code_retour, ecart_admi, nb_max_iter )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                :: r_equa      ! rayon equatorial terrestre
real(pm_reel), dimension(2:5), intent(in):: cn0         ! coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_orb_cir_equa), intent(in)        :: osc         ! parametres osculateurs

type(tm_orb_cir_equa), intent(out)       :: moy         ! parametres moyens au sens de Lyddane
type(tm_code_retour), intent(out)        :: code_retour
type(tm_orb_cir_equa), intent(in), optional :: ecart_admi  ! ecarts admissibles sur a,ex,ey,ix,iy,pso_M
integer, intent(in), optional               :: nb_max_iter ! nombre maximum d'iterations



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine me_lyddane_moy
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_extrapolation.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_extrapolation
