subroutine mv_kep_equa (kep, equa, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Passage des parametres kepleriens aux parametres orbitaux dits 
! ===   adaptes aux orbites equatoriales non circulaires
! 
!
! Note d'utilisation: La transformation inverse peut s'effectuer a l'aide de la  
! ==================  routine mv_equa_kep
!
!$Historique
! ==========
!   + Version 6.0 (SP 625 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_kep), intent(in)                         ::  kep   ! parametres kepleriens
type(tm_orb_equa), intent(out)                       ::  equa  ! parametres adaptes aux orbites equatoriales
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: sinus_i_sur_2   ! calcul intermediaire
real(pm_reel) :: ix,iy           ! calculs a reporter pour la jacobienne

intrinsic sin, cos

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_kep_equa.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kep_equa.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

equa%a        = kep%a
equa%e        = kep%e
equa%pgom     = kep%pom+kep%gom
sinus_i_sur_2 =sin(kep%i/2._pm_reel)
ix = 2._pm_reel * sinus_i_sur_2 * cos(kep%gom)
equa%ix       = ix
iy = 2._pm_reel * sinus_i_sur_2 * sin(kep%gom)
equa%iy       = iy
equa%M = kep%M

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = 1._pm_reel
   jacob(3,4) = 1._pm_reel
   jacob(3,5) = 1._pm_reel
   jacob(4,3) = cos(kep%i/2._pm_reel)*cos(kep%gom)
   jacob(4,5) = - iy
   jacob(5,3) = cos(kep%i/2._pm_reel)*sin(kep%gom)
   jacob(5,5) = ix
   jacob(6,6) = 1._pm_reel
end if

code_retour%routine = pm_num_mv_kep_equa
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kep_equa
