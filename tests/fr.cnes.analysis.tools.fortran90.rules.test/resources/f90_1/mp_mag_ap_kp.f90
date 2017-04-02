subroutine mp_mag_ap_kp (ap,kp,code_retour)

! (C) Copyright CNES - MSLIB - 2001

!************************************************************************
!
! But: transformation de l'indice d'activite geomagnetique ap en kp
! ===
!
! Note d'utilisation: non valable lorsque ap<0
! ==================            
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 08/2001 - Realisation: Mickael Hazak)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)  :: ap       !  entree indice d'activite geomagnetique ap
real(pm_reel),intent(out)  :: kp       !  sortie indice d'activite geomagnetique kp
type(tm_code_retour), intent(out)      :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! Declarations pour l'appel a la routine IOLIB fortran 77 mpi_IO_e_trapkp
integer                     :: ind       ! clef: >o si transformation ap->kp, <=0 sinon
logical                     :: erreur    ! code d'erreur
real(pm_reel)               :: apx,akpx  ! Coefficient de type Ap, et Kp

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mp_mag_ap_kp.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK

! verification des arguments d'entree
! ===================================

if (ap < 0._pm_reel) then ! indice d'activite geomagnetique ap negatif

   code_retour%valeur = pm_err_ap_negatif
   go to 6000

end if

! calcul de kp en fonction de ap
! ======================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_IO_e_trapkp
ind  = 1
apx  = ap

call mpi_IO_e_trapkp (apx,akpx,ind,erreur)

if (erreur) then ! erreur vaut .false. si tout est OK

! les valeurs non nulles possibles pour le code retour de mpi_IO_e_trapkp sont 
! normalement deja traitees avant l'appel

   code_retour%valeur = pm_err_IOLIB
   go to 6000 ! pas d'affectation des sorties

end if

! affectation des sorties
! =======================

kp = akpx

6000 continue

code_retour%routine = pm_num_mp_mag_ap_kp
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_mag_ap_kp
