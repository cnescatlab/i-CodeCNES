subroutine mpi_atmi_inter (lat,long,tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,tarra,zarra,retour )

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But:  Realisation des INTERpolations pour le modele atmospherique cira.
! ===
!
! Note d'utilisation:  Sans objet.
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MCINTE 
!                   (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : ajout du use au module interface de mui_interp_newton
!                         (Date: 04/2002 - Realisation: Guylaine Prat)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:5.15:FA-ID:1398:30/09/2010:Ajout du marqueur de fin historique
!
!$FinHistorique
!
!************************************************************************

  ! Modules
  ! =======
  use mslib

  use parametre_mspro

use int_util_internes_mspro, only : mui_interp_newton
use int_geo_internes, only : mpi_atmi_temp
use int_geo_internes, only : mpi_atmi_alt

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: lat  ! latitude
real(pm_reel), intent(in) :: long ! longitude
real(pm_reel), dimension(:,:), intent(in) :: tbar, zbar! temperatures et altitudes
real(pm_reel), dimension(:,:), intent(in) :: z1, phi1  ! amplitude et phase de l'altitude de l'onde 1
real(pm_reel), dimension(:,:), intent(in) :: t1, phit1 ! amplitude et phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(in) :: z2, phi2  ! amplitude et phase de l'altitude de l'onde 2
real(pm_reel), dimension(:,:), intent(in) :: t2, phit2 ! amplitude et phase de la temperature de l'onde 2 
real(pm_reel), dimension(:), intent(out)  :: tarra     ! interpolation des temperatures
real(pm_reel), dimension(:), intent(out)  :: zarra     ! interpolation des altitudes
integer, intent(out)                      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel) :: eps100          ! epsilon de comparaison pour les reels
  integer       :: latmin, latmax  ! latitudes minimum et maximum
  real(pm_reel), dimension(pm_nb_alt, 2) :: tempe, altit 
  integer       :: i               ! indice de boucle
  real(pm_reel) :: latmi, latma
  real(pm_reel) :: temp1, temp2, alt1, alt2, tar, zar

  intrinsic epsilon, int, abs, real

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_inter.f90: derniere modification V5.15 >'

  !************************************************************************
  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  ! initialisations 
  !...................................................
  eps100 = 100._pm_reel * epsilon(1._pm_reel)  !epsilon de test pour les reels

  latmin = int (lat/10._pm_reel) + 9           ! latitude minimum
  latmax = latmin                              ! latitude maximum

  !     test si (lat/10 + 9) differe de latmin
  !     la precision du test est en deca du maximum de precision 
  !     possible sur la latitude en radians

  if ( abs( lat/10._pm_reel+9._pm_reel - real(latmin,pm_reel))  >  eps100 ) then

     if (lat < 0._pm_reel) then
        latmin = latmin - 1
     else
        latmax = latmax + 1
     end if

  end if

  ! calcul des temperatures
  ! -----------------------

  call mpi_atmi_temp (long,latmin,latmax ,tbar,t1,phit1,t2,phit2, tempe,retour)
  if (retour < pm_OK) go to 6000

  ! calcul des altitudes
  ! --------------------

  call mpi_atmi_alt (long,latmin,latmax,zbar,z1,phi1,z2,phi2,altit,retour)
  if (retour < pm_OK) go to 6000

  ! realisation de l'interpolation
  ! ------------------------------

  if ( latmin == latmax ) then
     tarra (:) = tempe (:,1)
     zarra (:) = altit (:,1)
  else
     latmi     = real(latmin,pm_reel) * 10._pm_reel - 90._pm_reel
     latma     = real(latmax,pm_reel) * 10._pm_reel - 90._pm_reel
     do i =1, pm_nb_alt
        temp1 = tempe(i,1)
        temp2 = tempe(i,2)
        tar = mui_interp_newton (temp1,temp2,latmi,latma,lat)
        tarra(i) = tar
        alt1 = altit(i,1)
        alt2 = altit(i,2)
        zar = mui_interp_newton (alt1,alt2,latmi,latma,lat)
        zarra(i) = zar
     end do
  end if

6000 continue

   end subroutine mpi_atmi_inter
