subroutine mvi_cir_equa_equa ( cir_equa, equa, retour, jacob )

! (C) Copyright CNES - MSPRO - 2003-2004

!************************************************************************
!
! But:   Passage des parametres orbitaux dits adaptes aux orbites 
! ===   circulaires equatoriales aux parametres orbitaux dits adaptes aux
!       orbites equatoriales non circulaires
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

type(tm_orb_cir_equa),          intent(in)               :: cir_equa  ! parametre circulaire equatorial
type(tm_orb_equa),              intent(out)              :: equa      ! parametre equatorial
integer,                        intent(out)              :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob     ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)        :: pom_plus_gom            ! petit_omega+grand_omega
real(pm_reel)        :: excentricite            ! e 
real(pm_reel)        :: un_sur_e                ! inverse de e
real(pm_reel)        :: eps_cir                 ! pour test orbite circulaire
type(tm_code_retour) :: code_retour_local       ! code retour local

intrinsic sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mvi_cir_equa_equa.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

retour = pm_OK

! valeurs de test
call mc_test(code_retour_local, eps_cir = eps_cir) ! pas de code retour a tester

! Calculs 
! =======

! report de ix et iy
equa%ix = cir_equa%ix
equa%iy = cir_equa%iy

! calcul de a
equa%a = cir_equa%a

! calcul de e
excentricite = sqrt(cir_equa%ex * cir_equa%ex + cir_equa%ey * cir_equa%ey)
equa%e = excentricite

! calcul de petit omega + grand omega
if (excentricite < eps_cir) then ! orbite circulaire: petit omega + grand omega indetermine

   pom_plus_gom = 0._pm_reel   ! choix arbitraire: mise a 0
   retour = pm_warn_e_circul

else
   
   call mu_angle2(cir_equa%ex, cir_equa%ey, pom_plus_gom , code_retour_local) ! pas de code retour a tester

end if
equa%pgom = pom_plus_gom

! calcul de M
equa%M = cir_equa%pso_M - pom_plus_gom

if (present(jacob)) then

   if (excentricite < eps_cir) then
      retour = pm_err_jac_non_calc_e_circul
      go to 6000
   end if
   un_sur_e = 1._pm_reel / excentricite
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cir_equa%ex * un_sur_e
   jacob(2,3) = cir_equa%ey * un_sur_e
   jacob(3,2) = - sin(pom_plus_gom) * un_sur_e
   jacob(3,3) = cos(pom_plus_gom) * un_sur_e
   jacob(4,4) = 1._pm_reel
   jacob(5,5) = 1._pm_reel
   jacob(6,2) = sin(pom_plus_gom) * un_sur_e
   jacob(6,3) = - cos(pom_plus_gom) * un_sur_e
   jacob(6,6) = 1._pm_reel
end if

6000 continue

end subroutine mvi_cir_equa_equa
