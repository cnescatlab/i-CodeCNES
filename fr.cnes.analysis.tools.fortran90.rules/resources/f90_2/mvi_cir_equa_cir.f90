subroutine mvi_cir_equa_cir ( cir_equa, cir, retour, jacob )

! (C) Copyright CNES - MSPRO - 2003-2004

!************************************************************************
!
! But:   Passage des parametres orbitaux dits adaptes aux orbites 
! ===   circulaires equatoriales aux parametres orbitaux dits adaptes aux
!       orbites circulaires
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.1 : creation
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE globale 11): suppression du use a valeur_code_retour_mspro
!                          du fait du passage de codes retour dans la MSLIB90
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_cir_equa),          intent(in)               :: cir_equa  ! parametre adaptes circulaire equatorial
type(tm_orb_cir),               intent(out)              :: cir       ! parametre adaptes circulaire
integer,                        intent(out)              :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob     ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)        :: norme_carree            ! norme au carree du vecteur inclinaison
real(pm_reel)        :: deux_sinus_i_sur_deux   ! 2*sin(i/2)
real(pm_reel)        :: d_ix, d_iy, cir_ex, cir_ey ! valeurs intermediaires
real(pm_reel)        :: grand_omega             ! valeur intermediaire de gom
real(pm_reel)        :: inclinaison             ! valeur intermediaire de i
real(pm_reel)        :: eps_equa                ! pour test orbite equatoriale
type(tm_code_retour) :: code_retour_local       ! code retour local

intrinsic sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mvi_cir_equa_cir.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! valeurs de test
call mc_test(code_retour_local, eps_equa = eps_equa) ! pas de code retour a tester

! Calculs 
! =======

! calcul de a
cir%a = cir_equa%a

! calcul de i
norme_carree = cir_equa%ix * cir_equa%ix + cir_equa%iy * cir_equa%iy
if (norme_carree > 4._pm_reel) then  !  norme du vecteur inclinaison trop grande
   retour = pm_err_ix_iy_sup2
   go to 6000
end if

deux_sinus_i_sur_deux = sqrt(norme_carree)
inclinaison = 2._pm_reel*asin (deux_sinus_i_sur_deux/2._pm_reel) ! resultat dans [0,+pi] car sin(i/2) > 0
cir%i = inclinaison

! calcul de grand omega
if (sin(inclinaison) < eps_equa) then ! orbite equatoriale: grand omega indetermine

   grand_omega = 0._pm_reel   ! choix arbitraire: mise a 0
   retour = pm_warn_i_equa  

else

   call mu_angle2(cir_equa%ix, cir_equa%iy, grand_omega, code_retour_local) ! pas de code retour a tester

end if

cir%gom = grand_omega

! calcul de ex, ey
cir_ex = cir_equa%ex * cos(grand_omega) + cir_equa%ey * sin(grand_omega)
cir_ey = cir_equa%ey * cos(grand_omega) - cir_equa%ex * sin(grand_omega)
cir%ex = cir_ex
cir%ey = cir_ey

! calcul de pso_M
cir%pso_M = cir_equa%pso_M - grand_omega

if (present(jacob)) then
   if (sin(inclinaison) < eps_equa) then
      retour = pm_err_jac_non_calc_i_equa
      go to 6000
   end if
   d_ix = - cir_equa%iy / norme_carree
   d_iy = cir_equa%ix / norme_carree
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cos(grand_omega)
   jacob(2,3) = sin(grand_omega)
   jacob(2,4) = cir_ey * d_ix
   jacob(2,5) = cir_ey * d_iy
   jacob(3,2) = -sin(grand_omega)
   jacob(3,3) = cos(grand_omega)
   jacob(3,4) = - cir_ex * d_ix
   jacob(3,5) = - cir_ex * d_iy
   jacob(4,4) = cir_equa%ix / sin(inclinaison)
   jacob(4,5) = cir_equa%iy / sin(inclinaison)
   jacob(5,4) = d_ix
   jacob(5,5) = d_iy
   jacob(6,4) = - d_ix
   jacob(6,5) = - d_iy
   jacob(6,6) = 1._pm_reel
end if

6000 continue

end subroutine mvi_cir_equa_cir
