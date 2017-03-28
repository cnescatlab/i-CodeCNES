subroutine mr_TerVrai_veis ( jul1950, delta_tu1, pos_TerVrai, pos_veis, code_retour, vit_TerVrai, vit_veis, jacob )

! (C) Copyright CNES - MSLIB - 1999-2005

!************************************************************************
!
! But: Passage du repere terrestre vrai a la date t au repere de Veis a la meme date t 
! ===
!
! Notes d'utlisation:  [DR1] "Les systemes de reference utilises en astronomie"
! ===================        de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!
!                      Les vitesses declarees en sorties optionnelles ne peuvent etre calculees que si les vitesses declarees
!                      en entree optionnelles sont specifiees.
!
!$Historique
! ==========
!   + Version 2.0 (SP 376 ed01 rev00): creation a partir de la routine MRChgRepTVVeis de la MSLIB f77
!                         (Date: 10/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (FA globale 479 ed01 rev00) : Modif du test en sortie sur les vitesses optionnelles
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 480 ed01 rev00) : remplacement des tests de report de code retour en select
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 484 ed01 rev00): calcul de la jacobienne 
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.2 (DE globale 1): remplacement des calculs matriciels par des calculs bases sur des quaternions
!                         (Date: 01/2005 - Realisation: Samuel Fayard)
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
use int_rep_fondamentaux, only : mr_tsid_veis
use int_utilitaires, only : mu_3rot_quat
use int_utilitaires, only : mu_quat_rep
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_quat_mat

use parametres_internes_mslib
use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_veis    ! vecteur position dans le repere de Veis a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_veis    ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer       :: def_3rot               ! definition de la rotation
real(pm_reel) :: angle1, angle2, angle3 ! angles de la rotation

type(tm_quat) :: quat ! quaternion de la rotation
real(pm_reel), dimension(3,3) :: mat, mat_vect_rot ! matrices de changement de repere et rotation
real(pm_reel), dimension(3)   :: vect_omega ! vecteur rotation (cas non inertiel)
real(pm_reel), dimension(3)   :: vit_veis_iner, vit_entrainement ! vecteur vitesse dans le repere de Veis

real(pm_reel) :: tsveis  ! temps sideral modifie (Veis)

type(tm_code_retour) :: code_retour_local  

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerVrai_veis.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_TerVrai_veis.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees
if ((present(vit_veis)) .and. (.not. present(vit_TerVrai))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_TerVrai)) .and. (.not. present(vit_veis))) then
   code_retour%valeur = pm_warn_para_option
end if

! Calculs
! =======

! ---- Voir le paragraphe 12.10 de [DR1] pour la theorie ----

! calcul du temps sideral modifie (Veis) de la date t et de sa derivee 

call mr_tsid_veis ( jul1950, delta_tu1, tsveis, code_retour_local )
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! determination des parametres de la rotation

def_3rot = pm_1y_2x_3z

angle1 = 0._pm_reel
angle2 = 0._pm_reel
angle3 =  - tsveis

! calcul du quaternion associe a la rotation

call mu_3rot_quat(def_3rot, angle1, angle2, angle3, quat, code_retour_local) 
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur position

call mu_quat_rep (pos_TerVrai, quat, pos_veis, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur vitesse si demande

if  ((present(vit_TerVrai)) .and. (present(vit_veis))) then

   call mu_quat_rep (vit_TerVrai, quat, vit_veis_iner, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if

   !  vecteur vitesse cas non inertiel (avec vitesse d'entrainement)

   ! calcul du vecteur rotation
   vect_omega(1) = 0._pm_reel
   vect_omega(2) = 0._pm_reel
   vect_omega(3) = - pm_i_Veis_dtsid_sur_dt

   ! calcul de la vitesse d'entrainement

   call mu_prod_vect(vect_omega, pos_veis, vit_entrainement, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if

   ! vecteur vitesse avec vitesse d'entrainement

   vit_veis(:) = vit_veis_iner(:) - vit_entrainement(:)

end if

! calcul du jacobien si demande

if (present(jacob)) then

   ! matrice de rotation associee au quaternion de changement de repere

   call mu_quat_mat (quat, mat, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if

   jacob(1:3,1:3) = mat(:,:)
   jacob(1:3,4:6) = 0._pm_reel

   if  ((present(vit_TerVrai)) .and. (present(vit_veis))) then ! partie liee aux vitesses

      ! matrice associee au vecteur rotation

      mat_vect_rot(1,1) = 0._pm_reel
      mat_vect_rot(1,2) = vect_omega(3)
      mat_vect_rot(1,3) = - vect_omega(2)
      mat_vect_rot(2,1) = - vect_omega(3)
      mat_vect_rot(2,2) = 0._pm_reel
      mat_vect_rot(2,3) = vect_omega(1)
      mat_vect_rot(3,1) = vect_omega(2)
      mat_vect_rot(3,2) = - vect_omega(1)
      mat_vect_rot(3,3) = 0._pm_reel

      ! calcul de la jacobienne

      jacob(4:6,4:6) = mat(:,:)
      call mu_matmul3(mat_vect_rot,mat,jacob(4:6,1:3),code_retour_local)
      ! pas d'erreur possible donc pas de test dur code retour

   else ! mise a zero de la partie inutile

      jacob(4:6,1:3) = 0._pm_reel
      jacob(4:6,4:6) = 0._pm_reel

   end if
end if

6000 continue

code_retour%routine = pm_num_mr_TerVrai_veis
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerVrai_veis
