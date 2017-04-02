subroutine mv_kep_Vinf (mu, kep, Vinf, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Passage des parametres kepleriens aux parametres orbitaux 
! ===  hyperboliques a vitesse infinie depart
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_kep),intent(in)                          ::  kep    ! parametres kepleriens
type(tm_orb_Vinf),intent(out)                        ::  Vinf   ! parametres hyperboliques
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: DLA_tmp, RLA_tmp                   ! valeurs temporaires des parametres Vinf

real(pm_reel) :: theta_infini, theta ! variables intermediaires
real(pm_reel) :: cos_alpha_omega_cos_delta, sin_alpha_omega_cos_delta, alpha_moins_omega ! variables intermediaires
real(pm_reel) :: un_sur_e, j24, j34                 ! variables intermediaires

real(pm_reel) :: eps100                             ! variable epsilon machine * 100 
real(pm_reel) :: eps_parab                          ! variable de comparaison pour l'excentricite 
type(tm_code_retour) :: code_retour_local           ! code retour local

intrinsic acos,asin,sin,cos,sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mv_kep_Vinf.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
call mc_test(code_retour,eps_parab = eps_parab)  ! pas de code retour a tester
eps100 = 100._pm_reel * epsilon(1._pm_reel) 

! Verifications
! =============

if (mu <= eps100) then
   if (mu < 0._pm_reel) then  ! constante de gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else                       ! constante de gravitation proche de 0
      code_retour%valeur = pm_err_mu_nul 
   end if
   go to 6000
end if

if (kep%a <= eps100) then
   if (kep%a < 0._pm_reel) then ! demi-grand axe < 0
      code_retour%valeur= pm_err_a_negatif
      go to 6000
    else                        ! demi-grand axe = 0
      code_retour%valeur= pm_err_a_nul
      go to 6000
   end if
end if

if (kep%e < (1._pm_reel + eps_parab)) then ! ce n'est pas une hyperbole
   code_retour%valeur = pm_err_e_non_hyperb
   go to 6000
end if

un_sur_e = 1._pm_reel / kep%e ! valeur entre 0 et 1

! Calcul des parametres
! =====================

theta_infini = acos(- un_sur_e)
theta = theta_infini + kep%pom

! C3
! --

Vinf%C3 = mu / kep%a

! Declinaison delta
! -----------------

DLA_tmp = asin(sin(theta)*sin(kep%i)) ! solution delta dans [-pi/2, +pi/2] uniquement

Vinf%DLA = DLA_tmp

! Ascension droite alpha
! ----------------------

cos_alpha_omega_cos_delta = cos(theta)
sin_alpha_omega_cos_delta = sin(theta)*cos(kep%i)

if (abs(cos(DLA_tmp)) > eps100) then ! cas cos(delta) non nul

   call mu_angle2(cos_alpha_omega_cos_delta,sin_alpha_omega_cos_delta,alpha_moins_omega,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then ! NB: le cas pm_err_vect_nul ne peut arriver ici
     code_retour%valeur = code_retour_local%valeur
     if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

else ! traitement pour cas cos(delta) = 0 : indetermination pour alpha

     alpha_moins_omega = pm_pi_sur2 - kep%gom ! choix arbitraire: alpha = pi/2

end if

RLA_tmp = alpha_moins_omega + kep%gom

Vinf%RLA = modulo(RLA_tmp, pm_deux_pi)

! Rayon du periastre
! ------------------

Vinf%Rp = kep%a*(kep%e - 1._pm_reel) ! avec e - 1 > 0

! Ascension droite du noeud ascendant
! -----------------------------------

Vinf%gom = kep%gom

! anomalie moyenne
! ----------------

Vinf%M = kep%M

!---------------------------------
!   Calcul de la jacobienne
!---------------------------------

if (present(jacob)) then

   ! test des formes indeterminees

   if (abs(cos(DLA_tmp)) < eps100) then
      code_retour%valeur = pm_err_jac_non_calc_decl_pisur2
      go to 6000
   end if

   ! Calcul

   jacob(:,:) = 0._pm_reel

   jacob(1,1) = - mu/(kep%a**2)

   jacob(2,3) = sin(alpha_moins_omega)

   j24 = cos(theta)*sin(kep%i)/cos(DLA_tmp)

   jacob(2,4) =  j24

   jacob(2,2) = - j24 * un_sur_e/sqrt(kep%e**2-1._pm_reel) ! avec e > 1: e*e -1 > 0

   jacob(3,3) = - sin(DLA_tmp)*cos(alpha_moins_omega)/cos(DLA_tmp)

   j34 = cos(kep%i)/(cos(DLA_tmp)**2)

   jacob(3,4) =  j34

   jacob(3,2) = - j34 * un_sur_e/sqrt(kep%e**2 - 1._pm_reel)

   jacob(3,5) = 1._pm_reel

   jacob(4,1) = kep%e-1._pm_reel

   jacob(4,2) = kep%a

   jacob(5,5) = 1._pm_reel

   jacob(6,6) = 1._pm_reel

end if

6000 continue

code_retour%routine = pm_num_mv_kep_Vinf
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kep_Vinf
