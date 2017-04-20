subroutine mr_TerRef_TerVrai ( U, V, pos_ref, pos_vrai, code_retour, vit_ref, vit_vrai, jacob )

! (C) Copyright CNES - MSLIB - 1999-2005

!************************************************************************
!
! But:  Passage du repere terrestre de reference au repere terrestre vrai a la date t
! ===
!
! Note d'utilisation: Definition du repere R:
! ==================       - il est contenu dans le plan tangent a la Terre au pole Nord
!                            geographique
!                          - axe des coordonnees U: axe selon la direction du meridien 
!                            d'origine (Greenwich), et oriente positivement vers 
!                            Greenwich (Londres)
!                          - axe des coordonnees V: axe selon la direction perpendiculaire
!                            a l'axe U, et oriente positivement vers le Canada
!
!                      [DR1] "Les systemes de reference utilises en astronomie"
!                            de M. Chapront-Touze, G. Francou et B. Morando
!                            Bureau Des Longitudes (BDL) novembre 1994
!                            ISSN 1243-4272
!                            ISBN 2-910015-05-X
!                            nomenclature MSLIB M-NT-0-160-CN
!                      Nota: Dans ce changement de repere, la composition des vitesses est negligee
!
!                      Les vitesses declarees en sorties optionnelles ne peuvent etre calculees que si les vitesses declarees
!                      en entree optionnelles sont specifiees.
!
!$Historique
! ==========
!   + Version 2.0 (SP 375 ed01 rev00): creation a partir de la routine MRChgRepTRefTV de la MSLIB f77
!                         (Date: 10/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (FA globale 479 ed01 rev00) : Modif du test en sortie sur les vitesses optionnelles
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 480 ed01 rev00) : remplacement des tests de report de code retour en select
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 484 ed01 rev00): calcul de la jacobienne 
!                         (Date: 07/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.2 (DE globale 1): remplacement des calculs matriciels par des calculs bases sur des quaternions
!                         (Date: 01/2005 - Realisation: Samuel Fayard)
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
use int_utilitaires, only : mu_quat_rep
use int_utilitaires, only : mu_quat_mat

use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: U           ! coordonnee U du pole vrai a la date t dans le repere R
real(pm_reel),                      intent(in)            :: V           ! coordonnee V du pole vrai a la date t dans le repere R
real(pm_reel),        dimension(3), intent(in)            :: pos_ref     ! vecteur position dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out)           :: pos_vrai    ! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in) , optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out), optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer       :: def_3rot               ! definition de la rotation
real(pm_reel) :: angle1, angle2, angle3 ! angles de la rotation
type(tm_quat) :: quat ! quaternion de la rotation
real(pm_reel), dimension(3,3) :: mat ! matrice de changement de repere

type(tm_code_retour) :: code_retour_local ! code retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_TerRef_TerVrai.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_TerRef_TerVrai.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

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

! ---- Voir le paragraphe 4.8 de [DR1] ----

! determination des parametres de la rotation

def_3rot = pm_1y_2x_3z

angle1 = U
angle2 = V
angle3 = 0._pm_reel

! calcul du quaternion associe a la rotation

call mu_3rot_quat(def_3rot, angle1, angle2, angle3, quat, code_retour_local) 
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur position

call mu_quat_rep (pos_ref, quat, pos_vrai, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

! calcul du vecteur vitesse si demande

if  ((present(vit_ref)) .and. (present(vit_vrai))) then

   call mu_quat_rep (vit_ref, quat, vit_vrai, code_retour_local)
   if (code_retour_local%valeur /= pm_OK) then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
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

   if  ((present(vit_ref)) .and. (present(vit_vrai))) then ! partie liee aux vitesses

      jacob(4:6,4:6) = mat(:,:)
      jacob(4:6,1:3) = 0._pm_reel

   else

      jacob(4:6,4:6) = 0._pm_reel
      jacob(4:6,1:3) = 0._pm_reel

   endif

end if

6000 continue

code_retour%routine = pm_num_mr_TerRef_TerVrai
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_TerRef_TerVrai
