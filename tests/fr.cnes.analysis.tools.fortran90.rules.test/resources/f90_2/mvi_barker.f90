subroutine mvi_barker (anom_M, D, retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Resolution de l'equation de BARKER
! ===
!
! Note d'utilisation:  routine interne
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 225 ed01 rev00): creation a partir du code traitant le cas parabolique dans la routine MVRYKN de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(out)                :: D        ! tangente[anomalie vraie/2]
integer, intent(out)                      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: aux,bux,delta,uns3,anom_M3               ! variables intermediaires

intrinsic sqrt, sign, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_barker.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_barker.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

anom_M3 = 3._pm_reel * anom_M
delta = sqrt(1._pm_reel+anom_M3*anom_M3)
aux = anom_M3 + delta
bux = anom_M3 - delta
uns3 = 1._pm_reel / 3._pm_reel
D = sign(1._pm_reel,aux)*abs(aux)**uns3+sign(1._pm_reel,bux)*abs(bux)**uns3

end subroutine mvi_barker
