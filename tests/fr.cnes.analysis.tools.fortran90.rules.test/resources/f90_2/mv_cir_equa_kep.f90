subroutine mv_cir_equa_kep (cir_equa, kep, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites 
! ===   circulaires equatoriales aux parametres kepleriens
! 
!
! Note d'utilisation: La transformation inverse peut se faire par la routine  
! ==================  mv_kep_cir_equa. 
!
!$Historique
! ==========
!  + Version 6.0 (SP 622 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (date: 02/2004 - realisation: veronique lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
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
use int_utilitaires, only : mu_angle2

use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_cir_equa), intent(in)     ::  cir_equa     ! parametres adaptes aux orbites circulaires equatoriales
type(tm_orb_kep), intent(out)         ::  kep          ! parametres kepleriens
type(tm_code_retour), intent(out)     ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob  ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)        :: norme_carree            ! norme au carree du vecteur inclinaison
real(pm_reel)        :: un_sur_nc               ! inverse de norme_carree
real(pm_reel)        :: deux_sinus_i_sur_deux   ! 2*sin(i/2)
real(pm_reel)        :: grand_omega, pom_plus_gom
real(pm_reel)        :: excentricite            ! e 
real(pm_reel)        :: e2                      ! e*e 
real(pm_reel)        :: eysure2                 ! ey/e2
real(pm_reel)        :: exsure2                 ! ex/e2
real(pm_reel)        :: un_sur_e                ! inverse de e
real(pm_reel)        :: inclinaison             ! i
real(pm_reel)        :: eps_cir                 ! pour test orbite circulaire
real(pm_reel)        :: eps_equa                ! pour test orbite equatoriale
type(tm_code_retour) :: code_retour_local       ! code retour local

intrinsic sqrt, asin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_cir_equa_kep.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_cir_equa_kep.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! valeurs de test
eps_cir = pm_eps_cir
eps_equa = pm_eps_equa

! Calculs 
! =======

! calcul de a
kep%a = cir_equa%a

! calcul de i
norme_carree = cir_equa%ix * cir_equa%ix + cir_equa%iy * cir_equa%iy
if (norme_carree > 4._pm_reel) then  !  norme du vecteur inclinaison trop grande
   code_retour%valeur = pm_err_ix_iy_sup2
   go to 6000
end if

deux_sinus_i_sur_deux = sqrt(norme_carree)
inclinaison = 2._pm_reel*asin (deux_sinus_i_sur_deux/2._pm_reel) ! resultat dans [0,+pi] car sin(i/2) > 0
kep%i = inclinaison

! calcul de grand omega
if (sin(inclinaison) < eps_equa) then ! orbite equatoriale: grand omega indetermine

   grand_omega = 0._pm_reel   ! choix arbitraire: mise a 0
   code_retour%valeur = pm_warn_i_equa  

else

   call mu_angle2(cir_equa%ix, cir_equa%iy, grand_omega, code_retour_local) ! pas de code retour a tester

end if

kep%gom = grand_omega

! calcul de e
e2 = cir_equa%ex * cir_equa%ex + cir_equa%ey * cir_equa%ey
excentricite = sqrt(e2)
kep%e = excentricite

! calcul de petit omega + grand omega
if (excentricite < eps_cir) then ! orbite circulaire: petit omega + grand omega indetermine

   pom_plus_gom = 0._pm_reel   ! choix arbitraire: mise a 0

   if (code_retour%valeur == pm_warn_i_equa) then
      code_retour%valeur = pm_warn_e_circul_i_equa
   else
      code_retour%valeur = pm_warn_e_circul
   end if

else
   
   call mu_angle2(cir_equa%ex, cir_equa%ey, pom_plus_gom , code_retour_local) ! pas de code retour a tester

end if

! calcul de M
kep%M = cir_equa%pso_M - pom_plus_gom

! calcul de petit omega
kep%pom = pom_plus_gom - grand_omega

if (present(jacob)) then
   if (sin(inclinaison) < eps_equa) then
      code_retour%valeur = pm_err_jac_non_calc_i_equa
      go to 6000
   else if (excentricite < eps_cir) then
      code_retour%valeur = pm_err_jac_non_calc_e_circul
      go to 6000
   end if
   un_sur_e = 1._pm_reel / excentricite
   un_sur_nc = 1._pm_reel / norme_carree
   exsure2 = cir_equa%ex / e2
   eysure2 = cir_equa%ey / e2
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cir_equa%ex * un_sur_e
   jacob(2,3) = cir_equa%ey * un_sur_e
   jacob(3,4) = cir_equa%ix / sin(inclinaison)
   jacob(3,5) = cir_equa%iy / sin(inclinaison)
   jacob(4,2) = - eysure2
   jacob(4,3) =   exsure2
   jacob(4,4) = cir_equa%iy * un_sur_nc
   jacob(4,5) = - cir_equa%ix * un_sur_nc
   jacob(5,4) = - cir_equa%iy * un_sur_nc
   jacob(5,5) = cir_equa%ix * un_sur_nc
   jacob(6,2) =   eysure2
   jacob(6,3) = - exsure2
   jacob(6,6) = 1._pm_reel
end if

6000 continue

code_retour%routine = pm_num_mv_cir_equa_kep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_cir_equa_kep
