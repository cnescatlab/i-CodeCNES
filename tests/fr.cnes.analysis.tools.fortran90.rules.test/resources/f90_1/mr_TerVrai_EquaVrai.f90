subroutine mr_TerVrai_EquaVrai (model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai, code_retour, &
     inertiel, vit_TerVrai, vit_EquaVrai, jacob)

! (C) Copyright CNES - MSLIB - 2002-2005

!************************************************************************
!
! But: Passage du repere terrestre vrai a la date t au repere equatorial vrai a la meme date t  
! ===
!
! Note d'utilisation:  [DR1] "Les systemes de reference utilises en astronomie"
! ===================        de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!                      Nota: Les epoques B1900, J1900 et J2000 sont explicitees en
!                            page 23 table 1.1
!
!                      Les vitesses declarees en sorties optionnelles ne peuvent etre calculees que si les vitesses declarees
!                      en entree optionnelles sont specifiees.
!
!$Historique
! ==========
!   + Version 4.0 (SP 458 ed01 rev00): creation a partir de mr_veis_EquaVrai
!                         (Date: 10/2002 - Realisation:Bruno Revelin)
!   + Version 4.1 (DE globale 484 ed01 rev00): calcul de la jacobienne 
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 6.0 (DE 565 ed01 rev00): ajout de l'option inertiel (calcul sans vitesse d'entrainement) 
!                         (Date: 04/2004 - Realisation: Bruno Revelin et Guylaine Prat)
!   + Version 6.2 (DE globale 1): remplacement des calculs matriciels par des calculs bases sur des quaternions
!                         (Date: 01/2005 - Realisation: Samuel Fayard)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_rep_fondamentaux, only : mr_tsid_vrai
use int_utilitaires, only : mu_3rot_quat
use int_utilitaires, only : mu_quat_rep
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_quat_mat

use type_mslib
use parametre_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1    ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai  ! vecteur position dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour

logical,                            intent(in),  optional :: inertiel     ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai  ! vecteur vitesse dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer       :: def_3rot               ! definition de la rotation
real(pm_reel) :: angle1, angle2, angle3 ! angles de la rotation

type(tm_quat) :: quat ! quaternion de la rotation
real(pm_reel), dimension(3,3) :: mat, mat_vect_rot ! matrices de changement de repere et rotation
real(pm_reel), dimension(3)   :: vect_omega ! vecteur rotation (cas non inertiel)
real(pm_reel), dimension(3)   :: vit_EquaVrai_iner, vit_entrainement ! vecteur vitesse dans le repere equatorial vrai 
!(sans prise en compte de la vitesse d'entrainement) 
! et vecteur vitesse d'entrainement

real(pm_reel) :: tsvrai, tsvraid         ! temps sideral vrai,  derivee du temps sideral vrai

logical :: flag_iner=.false. ! variable de test de la presence ou non de flag inertiel

type(tm_code_retour) :: code_retour_local ! code retour local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerVrai_EquaVrai.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id = &
     ' $Id: mr_TerVrai_EquaVrai.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

! Verifications
! ===============

! test sur la coherence des entrees/sorties optionnelles si precisees

if ((present(vit_EquaVrai)) .and. (.not. present(vit_TerVrai))) then
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ((present(vit_TerVrai)) .and. (.not. present(vit_EquaVrai))) then
   code_retour%valeur = pm_warn_para_option
end if

! test sur le modele choisi

if (model /= pm_lieske_wahr) then
   code_retour%valeur = pm_err_ind_model
   go to 6000
end if

! Calculs
! =======

! ---- Voir le paragraphe 4.4 de [DR1] pour la theorie ----

! calcul du temps sideral vrai de la date t et de sa derivee

call mr_tsid_vrai ( model, jul1950, delta_tu1, delta_tai, tsvrai, code_retour_local, &
     deriv_tsid_vrai = tsvraid )
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! determination des parametres de la rotation

def_3rot = pm_1y_2x_3z

angle1 = 0._pm_reel
angle2 = 0._pm_reel
angle3 =  - tsvrai

! calcul du quaternion associe a la rotation

call mu_3rot_quat(def_3rot, angle1, angle2, angle3, quat, code_retour_local) 
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur position

call mu_quat_rep (pos_TerVrai, quat, pos_EquaVrai, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur vitesse si demande

if  ((present(vit_TerVrai)) .and. (present(vit_EquaVrai))) then

   call mu_quat_rep (vit_TerVrai, quat, vit_EquaVrai_iner, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if

   ! test de la presence ou non du flag inertiel
   if (.not.present(inertiel)) then
      flag_iner = .false.
   else
      flag_iner = inertiel
   end if

   if (flag_iner) then ! vecteur vitesse cas inertiel

      vit_EquaVrai(:) = vit_EquaVrai_iner(:)

   else ! vecteur vitesse cas non inertiel (avec vitesse d'entrainement)

      ! calcul du vecteur rotation

      vect_omega(1) = 0._pm_reel
      vect_omega(2) = 0._pm_reel
      vect_omega(3) = - tsvraid

      ! calcul de la vitesse d'entrainement

      call mu_prod_vect(vect_omega, pos_EquaVrai, vit_entrainement, code_retour_local)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur
         if (code_retour_local%valeur < pm_OK) go to 6000

      end if

      ! vecteur vitesse avec vitesse d'entrainement

      vit_EquaVrai(:) = vit_EquaVrai_iner(:) - vit_entrainement(:)

   end if
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

   if  ((present(vit_TerVrai)) .and. (present(vit_EquaVrai))) then ! partie liee aux vitesses

      jacob(4:6,4:6) = mat(:,:)

      if (flag_iner) then ! cas inertiel

         jacob(4:6,1:3) = 0._pm_reel

      else ! cas non inertiel

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

         ! elements de la jacobienne lies a la non inertialite

         call mu_matmul3(mat_vect_rot,mat,jacob(4:6,1:3),code_retour_local)
         ! pas d'erreur possible donc pas de test dur code retour

      end if

   else ! mise a zero de la partie inutile
      jacob(4:6,1:3) = 0._pm_reel
      jacob(4:6,4:6) = 0._pm_reel
   end if

end if

6000 continue

code_retour%routine = pm_num_mr_TerVrai_EquaVrai
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerVrai_EquaVrai
