subroutine mr_PlanetVrai_EquaUAI (planete, modeleUAI, jul1950, pos_PlanetVrai, pos_EquaUAI, code_retour, &
          tsid, deriv_tsid, vit_PlanetVrai, vit_EquaUAI, jacob)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Passage du repere planetocentrique au repere equatorial planetaire UAI
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 498 ed01 rev00): creation
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
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
use int_rep_internes, only : mri_def_rep_UAI

use parametres_internes_mslib
use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant le meridien origine
type(tm_jour_sec),             intent(in)            ::  jul1950          ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_PlanetVrai   ! position dans le repere planetocentrique
real(pm_reel), dimension(3),   intent(out)           ::  pos_EquaUAI      ! position dans le repere equatorial planetaire de l'astre
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  tsid             ! position du meridien origine
real(pm_reel),                 intent(in),  optional ::  deriv_tsid       ! derivee du meridien origine = vitesse de rotation
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_PlanetVrai   ! position dans le repere planetocentrique
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EquaUAI      ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)                :: W                      ! longitude du meridien origine 
real(pm_reel)                :: dW                     ! derivee de la longitude du meridien origine 
real(pm_reel)                :: angle                  ! angle de la rotation
type(tm_quat)                :: quat                   ! quaternion de la rotation
real(pm_reel),dimension(3,3) :: mat, mat_entr          ! matrices de la rotation (repere, entrainement)
real(pm_reel),dimension(3)   :: pos                    ! vecteur position intermediaire
real(pm_reel),dimension(3)   :: vit                    ! vecteur vitesse intermediaire
real(pm_reel),dimension(3)   :: vit_entr               ! vitesse d'entrainement
real(pm_reel)                :: alpha0, delta0         ! pour l'appel a mri_def_rep_UAI
type(tm_code_retour)         :: code_retour_local
integer                      :: retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_PlanetVrai_EquaUAI.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_PlanetVrai_EquaUAI.f90 362 2013-02-15 18:01:28Z bbjc $ '

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

if ((modeleUAI /= pm_UAI_autre_modele).AND.(present(tsid).OR.present(deriv_tsid))) then
   code_retour%valeur = pm_warn_para_option
end if

! Calculs
! =======

! determination du meridien origine de l'astre
if ( modeleUAI == pm_UAI_autre_modele) then   ! modele fourni par l'utilisateur

   if (present(tsid).AND.present(deriv_tsid))  then
      W = tsid
      dW = deriv_tsid
   else
      code_retour%valeur = pm_err_para_option
      go to 6000
   end if

else    ! modele predefini. On recupere le meridien origine

   call  mri_def_rep_UAI ( planete, modeleUAI, jul1950, alpha0, delta0, W, dW, retour_local )
   if (retour_local /= pm_OK) then
      code_retour%valeur = retour_local
      if (retour_local < pm_OK)  go to 6000
   end if

end if

! rotation autour de l'axe Z
! -------------------
! Calcul des positions
! -------------------

angle = - W
quat = mui_axe_norme_quat(pm_i_axe_z, angle)        ! quaternion associe a la rotation

call mu_quat_rep(pos_PlanetVrai,quat,pos,code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if
pos_EquaUAI(:) = pos(:)

! -------------------
! calcul optionnels
! -------------------

if ((present(vit_EquaUAI)) .and. (.not. present(vit_PlanetVrai))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_PlanetVrai)) .and. (.not. present(vit_EquaUAI))) then
   code_retour%valeur = pm_warn_para_option
end if

if  (present(vit_PlanetVrai).and.present(vit_EquaUAI)) then      ! calcul des composantes vitesses

   call mu_quat_rep(vit_PlanetVrai,quat,vit,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

   ! vitesses d'entrainement
   vit_entr(1) = - dW * pos(2)
   vit_entr(2) = dW * pos(1)
   vit_entr(3) = 0._pm_reel

   vit_EquaUAI(:) = vit(:) + vit_entr(:)

end if

if (present(jacob)) then  
! calcul du jacobien : calcul des derivees partielles des vecteurs position et vitesse 
! On calcule en fait la matrice de passage

   call mu_quat_mat(quat,mat,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

   mat_entr(:,:) = 0._pm_reel
   mat_entr(1,2) = - dW
   mat_entr(2,1) = dW

   jacob(1:3,1:3) = mat(:,:)
   jacob(1:3,4:6) = 0._pm_reel
   jacob(4:6,4:6) = mat(:,:)
   call mu_matmul3(mat_entr,mat,jacob(4:6,1:3),code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

end if

6000 continue

code_retour%routine = pm_num_mr_PlanetVrai_EquaUAI
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_PlanetVrai_EquaUAI
