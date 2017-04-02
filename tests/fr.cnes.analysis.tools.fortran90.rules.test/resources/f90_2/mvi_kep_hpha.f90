subroutine mvi_kep_hpha (r_equa, kep, hpha, retour, jacob)

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But: Passage des parametres kepleriens aux parametres perigee/apogee
!
! Note d'utilisation: La transformation inverse peut se faire par la routine
! ==================  mvi_hpha_kep
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.1 : creation 
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

use mslib
use type_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel)     , intent(in)                           :: r_equa   ! rayon equatorial
type(tm_orb_kep)  , intent(in)                           :: kep      ! parametres kepleriens
type(tm_orb_hpha) , intent(out)                          :: hpha     ! parametres perigee/apogee
integer           , intent(out)                          :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob    ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer  :: i   ! indices

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mvi_kep_hpha.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
retour = pm_OK

hpha%hp  = kep%a*(1._pm_reel-kep%e)-r_equa
hpha%ha  = kep%a*(1._pm_reel+kep%e)-r_equa
hpha%i   = kep%i
hpha%pom = kep%pom
hpha%gom = kep%gom
hpha%M   = kep%M

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel-kep%e
   jacob(1,2) = -kep%a
   jacob(2,1) = 1._pm_reel+kep%e
   jacob(2,2) = kep%a
   do i=3,6
      jacob(i,i) = 1._pm_reel
   end do
end if

end subroutine mvi_kep_hpha
