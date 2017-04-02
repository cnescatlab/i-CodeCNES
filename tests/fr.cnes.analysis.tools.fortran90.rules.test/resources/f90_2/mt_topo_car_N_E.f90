subroutine mt_topo_car_N_E (pos_N, pos_E, code_retour, vit_N, vit_E, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage d'un repere TOPOcentrique Nord (convention axe Ox vers le Nord) au repere topocentrique  associe
! ===   (convention axe Ox vers l'Est) en coordonnees CARtesiennes.
!
! Note d'utilisation:  La transformation inverse peut s'effectuer par la routine mt_topo_car_E_N
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 257 ed01 rev00): creation a partir de la routine MVTOPO (MVTRXY) de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  pos_N  ! position cartesienne avec la convention axe Ox vers le Nord
real(pm_reel), dimension(3), intent(out)             ::  pos_E  ! position cartesienne avec la convention axe Ox vers l'Est
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3), intent(in) , optional   ::  vit_N  ! vitesse cartesienne avec la convention axe Ox vers le Nord
real(pm_reel), dimension(3), intent(out), optional   ::  vit_E  ! vitesse cartesienne avec la convention axe Ox vers l'Est
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob   ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mt_topo_car_N_E.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_topo_car_N_E.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

!-----------------------------------------------
! calcul vecteur position en repere topocentrique 
!-----------------------------------------------

pos_E(1) = -pos_N(2)
pos_E(2) = pos_N(1)
pos_E(3) = pos_N(3)

!----------------------
! calcul optionnels 
!----------------------
if (present(jacob)) then           ! calcul du jacobien 

   jacob(:,:) = 0._pm_reel

   ! calcul des derivees partielles des vecteurs position et vitesse par rapport a x,y,z,vx,vy,vz
   !
   jacob(1,2) = -1._pm_reel   !     par rapport a x 
   jacob(2,1) = 1._pm_reel    !     par rapport a y 
   jacob(3,3) = 1._pm_reel    !     par rapport a z 
   jacob(4,5) = -1._pm_reel   !     par rapport a vx 
   jacob(5,4) = 1._pm_reel    !     par rapport a vy 
   jacob(6,6) = 1._pm_reel    !     par rapport a vz 

end if

if (present(vit_E).and..not.present(vit_N)) then    ! erreur de parametrage
   code_retour%valeur = pm_err_para_option
   go to 6000
else  if (present(vit_N).and.present(vit_E)) then   !  calcul vecteur vitesse en repere topocentrique
   vit_E(1) = -vit_N(2)
   vit_E(2) = vit_N(1)
   vit_E(3) = vit_N(3)
end if
if (.not.present(vit_E).and. present(vit_N))  code_retour%valeur = pm_warn_para_option ! parametrage optionnel incoherent

6000 continue

code_retour%routine = pm_num_mt_topo_car_N_E
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_topo_car_N_E
