subroutine mt_topo_sgd_N_E (pos_N, pos_E, code_retour, vit_N, vit_E, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage d'un repere TOPOcentrique Nord (convention axe Ox vers le Nord) au repere topocentrique Est associe
! ===   (convention axe Ox vers l'Est) en coordonnees Site/Gisement/Distance.
!
! Note d'utilisation:  La transformation inverse peut s'effectuer par la routine mt_topo_sgd_E_N
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 259 ed01 rev00): creation a partir de la routine MVTOPO (MVTRSG) de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
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

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_sgd), intent(in)                ::  pos_N  ! position en site, gisement, distance avec la convention axe Ox vers le Nord
type(tm_sgd), intent(out)               ::  pos_E  ! position en site, gisement, distance avec la convention axe Ox vers l'Est
type(tm_code_retour), intent(out)       ::  code_retour
type(tm_sgd), intent(in) , optional     ::  vit_N  ! vitesse en site, gisement, distance avec la convention axe Ox vers le Nord
type(tm_sgd), intent(out), optional     ::  vit_E  ! vitesse en site, gisement, distance avec la convention axe Ox vers l'Est
real(pm_reel), dimension(6,6), intent(out), optional  ::  jacob   ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mt_topo_sgd_N_E.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_topo_sgd_N_E.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

!-----------------------------------------------
! calcul vecteur position en repere topocentrique 
!-----------------------------------------------

pos_E%s = pos_N%s
pos_E%g = pm_deux_pi - pos_N%g
pos_E%d = pos_N%d

!----------------------
! calcul optionnels 
!----------------------
if (present(jacob)) then           ! calcul du jacobien
   jacob(:,:) = 0._pm_reel

   ! calcul des derivees partielles des vecteurs position et vitesse en composantes site/gisement/distance

   jacob(1,1) = 1._pm_reel
   jacob(2,2) = -1._pm_reel
   jacob(3,3) = 1._pm_reel
   jacob(4,4) = 1._pm_reel
   jacob(5,5) = -1._pm_reel
   jacob(6,6) = 1._pm_reel

end if

if (present(vit_E).and..not.present(vit_N)) then ! erreur de parametrage
   code_retour%valeur = pm_err_para_option
   go to 6000
else if (present(vit_E).and.present(vit_N)) then !  calcul vecteur vitesse en repere topocentrique
   vit_E%s = vit_N%s
   vit_E%g = - vit_N%g
   vit_E%d = vit_N%d
end if

if (.not.present(vit_E).and. present(vit_N))  code_retour%valeur = pm_warn_para_option ! parametrage optionnel incoherent

6000 continue

code_retour%routine = pm_num_mt_topo_sgd_N_E
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_topo_sgd_N_E
