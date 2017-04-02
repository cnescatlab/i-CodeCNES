subroutine mr_J2000_EquaUAI ( planete, modeleUAI, jul1950, pos_J2000, pos_EquaUAI, code_retour, &
          asc_droite, declinaison, vit_J2000, vit_EquaUAI, jacob )

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Passage du repere EME2000 au repere equatorial planetaire UAI d'un astre
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 495 ed01 rev00): creation
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
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
use int_utilitaires, only : mu_3rot_quat
use int_utilitaires, only : mu_quat_mat
use int_utilitaires, only : mu_quat_rep
use int_rep_internes, only : mri_def_rep_UAI

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                       intent(in)            ::  planete       ! planete
integer,                       intent(in)            ::  modeleUAI     ! modele UAI definissant le pole de rotation de l'astre
type(tm_jour_sec),             intent(in)            ::  jul1950       ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_J2000     ! position dans le repere EME2000
real(pm_reel), dimension(3),   intent(out)           ::  pos_EquaUAI   ! position dans le repere equatorial planetaire de l'astre
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  asc_droite    ! ascension droite (alpha0) du pole
real(pm_reel),                 intent(in),  optional ::  declinaison   ! declinaison (delta0) du pole
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EquaUAI   ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer                      :: def_3rot               ! definition de la rotation
real(pm_reel)                :: alpha0                 ! ascension droite
real(pm_reel)                :: delta0                 ! declinaison
real(pm_reel)                :: W                      ! meridien origine (seulement pour l'appel)
real(pm_reel)                :: dW                     ! derivee (seulement pour l'appel)
real(pm_reel)                :: angle1, angle2, angle3 ! angles de la rotation
type(tm_quat)                :: quat                   ! quaternion de la rotation
real(pm_reel),dimension(3,3) :: mat                    ! matrice de la rotation
type(tm_code_retour)         :: code_retour_local
integer                      :: retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_J2000_EquaUAI.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_J2000_EquaUAI.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Verifications
! ===============

! modeles
if ((modeleUAI < pm_UAI_modeles_min).OR.(modeleUAI > pm_UAI_modeles_max)) then
   code_retour%valeur = pm_err_ind_model
   go to 6000
end if

if ((modeleUAI /= pm_UAI_autre_modele).AND.(present(asc_droite).OR.present(declinaison))) then
   code_retour%valeur = pm_warn_para_option
end if

! Calculs
! =======

! determination des angles du pole de rotation de l'astre
if ( modeleUAI == pm_UAI_autre_modele) then   ! modele fourni par l'utilisateur

   if (present(asc_droite).AND.present(declinaison)) then
      alpha0 = asc_droite
      delta0  = declinaison
   else
      code_retour%valeur = pm_err_para_option
      go to 6000
   end if

else    ! modele predefini

   call  mri_def_rep_UAI ( planete, modeleUAI, jul1950, alpha0, delta0, W, dW, retour_local )
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK)  go to 6000
   end if

end if

! determination des angles de rotation   
def_3rot = pm_1z_2x_3y
angle1 = alpha0 + pm_pi_sur2
angle2 = pm_pi_sur2 - delta0
angle3 = 0._pm_reel

! calcul du quaternion de la rotation
call mu_3rot_quat(def_3rot, angle1, angle2, angle3, quat, code_retour_local) 
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! calcul du vecteur position
call mu_quat_rep (pos_J2000, quat, pos_EquaUAI, code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! calcul du vecteur vitesse si demande
if ((present(vit_EquaUAI)) .and. (.not. present(vit_J2000))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_J2000)) .and. (.not. present(vit_EquaUAI))) then
   code_retour%valeur = pm_warn_para_option
end if

if  (present(vit_J2000).and.present(vit_EquaUAI)) then
   call mu_quat_rep (vit_J2000,quat,vit_EquaUAI, code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if
end if

! calcul du jacobien si demande
if (present(jacob)) then
   
   call mu_quat_mat (quat, mat, code_retour_local)
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

code_retour%routine = pm_num_mr_J2000_EquaUAI
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_J2000_EquaUAI
