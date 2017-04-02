subroutine mr_TerRef_TerVrai_iers2003(Xp, Yp, pos_ref, pos_vrai, code_retour, s_prime, vit_ref, vit_vrai, jacob)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Passage du repere terrestre de reference au repere terrestre vrai a la meme date
!       d'après la définition de l'IERS2003
! ===
!
! $Remarques : ce code est couvert par la DV BIBMS n°18 (Code lié aux changements de repères de l'IERS 2003 (MSLIB90))
!              Plus d'informations sur ces routines est disponible dans la note algorithmique du thème R de la MSLIB90
!              -> BIBMS-SME-19-2025-ATOS
! 
!
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 Création
!                   (Date: 07/2008 - Realisation: Atos origin)
!   + Version 6.10 : AQ : application de la DV 18
!                   (Date: 10/2008 - Realisation: Atos origin)
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

use type_mslib
use parametre_mslib
use valeur_code_retour_mslib
use numero_routine_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),        dimension(3), intent(in)            :: pos_ref     ! vecteur position dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out)           :: pos_vrai    ! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out), optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel), dimension(3,3) :: W ! matrice de changement de repere
real(pm_reel), dimension(3,3) :: R1_YP, R2_XP, R3_moins_s_prime,W_inter ! matrices intermédiaires
integer :: ii, jj
type(tm_code_retour) :: code_retour_local ! code retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerRef_TerVrai_iers2003.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_TerRef_TerVrai_iers2003.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees
if ((present(vit_vrai)) .and. (.not. present(vit_ref))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_ref)) .and. (.not. present(vit_vrai))) then
   code_retour%valeur = pm_warn_para_option
end if

! Calculs
! =======

! Calculs des matrices intermédiaires
call mri_R1_iers2003(Yp,R1_YP,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_mat
   go to 6000
end if

call mri_R2_iers2003(Xp,R2_XP,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_mat
   go to 6000
end if

if (present(s_prime)) then
   call mri_R3_iers2003(-s_prime,R3_moins_s_prime,code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = pm_err_calc_mat
      go to 6000
   end if
end if

! Calcul de la matice de rotation W
call mu_matmul3(R2_XP,R1_YP,W,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_mat
   go to 6000
end if
if (present(s_prime)) then
   do ii=1,3
      do jj=1,3
         W_inter(ii,jj) = W(ii,jj)
      end do
   end do
   call mu_matmul3(R3_moins_s_prime,W_inter,W,code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = pm_err_calc_mat
      go to 6000
   end if
end if

! Calcul des nouvelles positions Xnew = W*Xold
call mu_mulvect3(W,pos_ref,pos_vrai, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = pm_err_calc_transfo
   go to 6000
end if

! Calcul des vitesses si demandé
if (present(vit_vrai)) then
   ! On considère que le changement de repère est inertiel
   call mu_mulvect3(W,vit_ref,vit_vrai, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = pm_err_calc_transfo
      go to 6000
   end if
end if

! Calcul de la jacobienne si demandée
if (present(jacob)) then
   !        [ W 0 ]
   !jacob = [ 0 W ]
   do ii=1,3
      do jj=1,3
         jacob(ii,jj) = W(ii,jj)
         jacob(ii+3,jj+3) = W(ii,jj)
         jacob(ii+3,jj) = 0._pm_reel
         jacob(ii,jj+3) = 0._pm_reel
      end do
   end do
end if
   
6000 continue

code_retour%routine = pm_num_mr_TerRef_TerVrai_iers
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerRef_TerVrai_iers2003
