subroutine mr_J2000_EcliJ2000 ( pos_J2000, pos_EcliJ2000, code_retour,  &
                                 obliquite, vit_J2000, vit_EcliJ2000, jacob)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Passage du repere EME2000 au repere ecliptique J2000
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 493 ed01 rev00): creation
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module de constantes phys_mslib
!       par le module  int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_quat_mat
use int_utilitaires, only : mu_quat_rep
use int_util_internes, only : mui_axe_norme_quat

use int_constantes, only : pm_obliquite2000
use parametres_internes_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3),   intent(in)            ::  pos_J2000     ! position dans le repere EME2000
real(pm_reel), dimension(3),   intent(out)           ::  pos_EcliJ2000 ! position dans le repere ecliptique J2000
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in) , optional ::  obliquite     ! obliquite (rad)
real(pm_reel), dimension(3),   intent(in) , optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EcliJ2000 ! vitesse dans le repere ecliptique J2000
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: angle                 ! angle de la rotation
type(tm_quat) :: quat                  ! quaternion de la rotation
real(pm_reel), dimension(3,3) :: mat   ! matrice de passage 
type(tm_code_retour)  :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_J2000_EcliJ2000.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_J2000_EcliJ2000.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! rotation autour de l'axe X
! -------------------
! Calcul des positions
! -------------------

if (present(obliquite)) then
   angle = obliquite
else
   angle = pm_obliquite2000
end if

quat = mui_axe_norme_quat(pm_i_axe_x, angle)
call mu_quat_rep(pos_J2000,quat,pos_EcliJ2000,code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! -------------------
! calcul optionnels
! -------------------

if ((present(vit_EcliJ2000)) .and. (.not. present(vit_J2000))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_J2000)) .and. (.not. present(vit_EcliJ2000))) then
   code_retour%valeur = pm_warn_para_option
end if

if  (present(vit_J2000).and.present(vit_EcliJ2000)) then      ! calcul des composantes vitesses
   call mu_quat_rep(vit_J2000,quat,vit_EcliJ2000,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if
end if

if (present(jacob)) then  
! calcul du jacobien : calcul des derivees partielles des vecteurs position et vitesse 
! On calcule en fait la matrice de passage

   call mu_quat_mat(quat,mat,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

   jacob(1:3,1:3) = mat(:,:)
   jacob(1:3,4:6) = 0._pm_reel
   jacob(4:6,4:6) = mat(:,:)
   jacob(4:6,1:3) = 0._pm_reel

end if

6000 continue

code_retour%routine = pm_num_mr_J2000_EcliJ2000
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_J2000_EcliJ2000
