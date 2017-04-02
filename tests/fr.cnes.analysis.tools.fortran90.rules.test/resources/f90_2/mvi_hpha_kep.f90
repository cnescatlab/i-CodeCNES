subroutine mvi_hpha_kep (r_equa, hpha, kep, retour, jacob)

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But: Passage des parametres perigee/apogee aux parametres kepleriens
!
! Note d'utilisation: La transformation inverse peut se faire par la routine
! ==================  mvi_kep_hpha
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
type(tm_orb_hpha) , intent(in)                           :: hpha     ! parametres perigee/apogee
type(tm_orb_kep)  , intent(out)                          :: kep      ! parametres kepleriens
integer           , intent(out)                          :: retour
real(pm_reel), dimension (6,6), intent(out), optional    :: jacob    ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer       :: i                ! indices
real(pm_reel) :: a, e, un_sur_2a  ! valeurs des parametres a tester
real(pm_reel),parameter :: eps_a = 100._pm_reel*epsilon(1._pm_reel)        ! epsilon de test pour a

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mvi_hpha_kep.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
retour = pm_OK

a = (hpha%hp + hpha%ha)*0.5_pm_reel+r_equa
if (a < eps_a) then
   if (a < 0._pm_reel) then
      retour = pm_err_a_negatif
      go to 6000
   else 
      retour = pm_err_a_nul
      go to 6000
   end if
else
   kep%a = a
   un_sur_2a = 1._pm_reel/(2._pm_reel*a)
end if
   
e = (hpha%ha - hpha%hp)*un_sur_2a
if (e < 0._pm_reel) then
   retour = pm_err_e_negatif
   go to 6000
else
   kep%e = e
end if

kep%i   = hpha%i
kep%pom = hpha%pom
kep%gom = hpha%gom
kep%M   = hpha%M

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 0.5_pm_reel
   jacob(1,2) = 0.5_pm_reel
   jacob(2,1) = - 2._pm_reel*(hpha%ha + r_equa)* un_sur_2a * un_sur_2a
   jacob(2,2) = 2._pm_reel*(hpha%hp + r_equa) * un_sur_2a * un_sur_2a
   do i=3,6
      jacob(i,i) = 1._pm_reel
   end do
end if
   
6000 continue

end subroutine mvi_hpha_kep
