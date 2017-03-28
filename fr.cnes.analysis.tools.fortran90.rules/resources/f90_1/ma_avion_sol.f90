subroutine ma_avion_sol (incidence, derapage, pente_vit, azimut_vit, gite_vit, &
                         azimut_avion, assiette_avion, gite_avion, code_retour)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Positon du triedre avion par rapport au triedre normal lie au sol 
! ===  et porte par l'avion.
!
! Note d'utilisation:   voir la documentation utilisateur
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 01/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE 1): modification suite au transfert vers la MSLIB de 5 routines du theme U
!                         (Date: 11/2003 - Realisation: Veronique Lepine
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

use int_util_internes_mspro, only : mui_recale_angle

use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in)  :: incidence  ! angle d'incidence
real(pm_reel) , intent(in)  :: derapage   ! angle de derapage
real(pm_reel) , intent(in)  :: pente_vit  ! pente de la vitesse
real(pm_reel) , intent(in)  :: azimut_vit ! azimut de la vitesse
real(pm_reel) , intent(in)  :: gite_vit   ! gite (rotation autour du vecteur vitesse)

real(pm_reel) , intent(out)  :: azimut_avion   ! azimut de l'avion (fuselage)
real(pm_reel) , intent(out)  :: assiette_avion ! assiette longitudinale de l'avion (fuselage)
real(pm_reel) , intent(out)  :: gite_avion     ! gite de l'avion (fuselage) 

type(tm_code_retour), intent(out) ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

type(tm_quat) :: quat_terre_avion,quat_terre_vit,quat_avion_vit,quat_vit_avion ! quaternions intermediaires de calcul
real(pm_reel) :: azimut_avion_brut, gite_avion_brut     ! elements vitesse de l'avion
real(pm_reel) :: moins_alpha                            ! oppose de l'incidence

type(tm_code_retour) :: code_local

real(pm_reel),parameter :: zero = 0._pm_reel            ! reel nul

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO ma_avion_sol.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Calcul du quaternion associe a la matrice de rotation terre -> avion
! ==========================================================================

! calcul du quaternion terre->vit
call mu_3rot_quat(pm_1z_2y_3x,azimut_vit,pente_vit,gite_vit,quat_terre_vit,code_local)
! pas de test necessaire sur le code retour

! calcul du quaternion avion->vit
moins_alpha = - incidence
call mu_3rot_quat(pm_1y_2z_3x,moins_alpha,derapage,zero,quat_avion_vit,code_local)
! pas de test necessaire sur le code retour

! calcul du quaternion inverse avion->vit : vit->avion
quat_vit_avion%q0 = - quat_avion_vit%q0
quat_vit_avion%q123(:) = quat_avion_vit%q123(:)

! calcul du quaternion terre -> avion
call mu_prod_quat (quat_terre_vit,quat_vit_avion, quat_terre_avion, code_local ) ! le code retour est toujours OK

! Calculs des angles: azimut avion, assiette avion, et gite avion
! ===============================================================
call mu_quat_3rot ( pm_1z_2y_3x, quat_terre_avion, azimut_avion_brut, assiette_avion, gite_avion_brut, code_local)

if (code_local%valeur /= pm_OK) then
   if (code_local%valeur == pm_warn_angle1_ou_3_indef) then 
      code_retour%valeur = pm_warn_fuse_vertic      ! warning: azimut mis a zero
                                                    ! car assiette = +/- pi/2
   else
      code_retour%valeur = code_local%valeur
      if (code_retour%valeur < pm_OK) go to 6000 
   end if
end if

! Recalage dans [-pi,+pi] des angles azimut avion et gite avion
! =============================================================
azimut_avion = mui_recale_angle(azimut_avion_brut,zero) ! le recalage ne change pas la valeur arbitraire
gite_avion   = mui_recale_angle(gite_avion_brut,zero)

6000 continue

code_retour%routine = pm_num_ma_avion_sol
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine ma_avion_sol

