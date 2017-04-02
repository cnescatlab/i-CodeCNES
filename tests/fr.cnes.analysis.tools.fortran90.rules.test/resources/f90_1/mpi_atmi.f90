subroutine mpi_atmi ( lat, long, mois, tarra, zarra, press, retour )

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Pour le modele ATMospherique cira, preparation de l'Interpolation.
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
!   + Version 1.0 : creation a partir de la routine MCATMI de la MSLIB f77
!                         (Date: 12/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:5.15:FA-ID:1398:30/09/2010:Ajout du marqueur de fin historique
!
!$FinHistorique
!************************************************************************

  ! Modules
  ! =======
use int_geo_internes, only : mpi_atmi_janvier
use int_geo_internes, only : mpi_atmi_fevrier
use int_geo_internes, only : mpi_atmi_mars
use int_geo_internes, only : mpi_atmi_avril
use int_geo_internes, only : mpi_atmi_mai
use int_geo_internes, only : mpi_atmi_juin
use int_geo_internes, only : mpi_atmi_juillet
use int_geo_internes, only : mpi_atmi_aout
use int_geo_internes, only : mpi_atmi_septembre
use int_geo_internes, only : mpi_atmi_octobre
use int_geo_internes, only : mpi_atmi_novembre
use int_geo_internes, only : mpi_atmi_decembre
use int_geo_internes, only : mpi_atmi_inter

  use mslib

  use parametre_mspro
  use valeur_code_retour_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: lat  ! latitude
real(pm_reel), intent(in) :: long ! longitude
integer, intent(in)       :: mois ! mois de l'annee
real(pm_reel), dimension(:), intent(out):: tarra! interpolation des temperatures
real(pm_reel), dimension(:), intent(out):: zarra! interpolation des altitudes
real(pm_reel), dimension(:), intent(out):: press! tableau des pressions
integer, intent(out)      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel), dimension(pm_nb_alt, pm_nb_lat) :: tbar, zbar   ! temperatures, altitudes 
  real(pm_reel), dimension(pm_nb_alt, pm_nb_lat) :: z1, phi1     ! amplitude et phase de l'altitude de l'onde 1
  real(pm_reel), dimension(pm_nb_alt, pm_nb_lat) :: t1, phit1    ! amplitude et phase de temperature de l'onde 1
  real(pm_reel), dimension(pm_nb_alt, pm_nb_lat) :: z2, phi2     ! amplitude et phase de l'altitude de l'onde 2
  real(pm_reel), dimension(pm_nb_alt, pm_nb_lat) :: t2, phit2    ! amplitude et phase de temperature de l'onde 2
  integer                                        :: ialt         ! indice de boucle du calcul des pressions

  intrinsic real, exp

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  do  ialt = 1 , pm_nb_alt , 1
     press (ialt) = 1013._pm_reel * exp (-.5_pm_reel * real( (ialt-1), pm_reel))
  end do

  select case (mois)
  case (pm_janvier) 
     call mpi_atmi_janvier(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_fevrier) 
     call mpi_atmi_fevrier(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_mars) 
     call mpi_atmi_mars(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_avril) 
     call mpi_atmi_avril(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_mai) 
     call mpi_atmi_mai(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_juin) 
     call mpi_atmi_juin(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_juillet) 
     call mpi_atmi_juillet(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_aout) 
     call mpi_atmi_aout(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_septembre) 
     call mpi_atmi_septembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_octobre) 
     call mpi_atmi_octobre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_novembre) 
     call mpi_atmi_novembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case (pm_decembre) 
     call mpi_atmi_decembre(tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,retour)
  case default
     retour = pm_err_mois
  end select
  if (retour < pm_OK) go to 6000

  call mpi_atmi_inter(lat,long,tbar,zbar,z1,phi1,t1,phit1,z2,phi2,t2,phit2,tarra,zarra,retour)

6000 continue

end subroutine mpi_atmi
