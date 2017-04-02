subroutine mpi_atmi_alt ( long, latmin, latmax, zbar, z1, phi1, z2, phi2, altit, retour )

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But:  Pour l'interpolation dans le modele ATMospherique cira, calcul des
! ===   ALTitudes
!
! Note d'utilisation:  Sans objet.
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MCCALT 
!                   (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!VERSION:5.15:FA-ID:1398:30/09/2010:Ajout du marqueur de fin historique
!
!$FinHistorique
!
!************************************************************************

  ! Modules
  ! =======
  use mslib

  use parametre_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: long ! longitude
integer, intent(in)                                        ::  latmin   ! latitude minimum
integer, intent(in)                                        ::  latmax   ! latitude maximum
real(pm_reel), dimension(:,:), intent(in) ::  zbar     ! altitudes
real(pm_reel), dimension(:,:), intent(in) ::  z1, phi1 ! amplitudes et phases des altitudes de l'onde 1
real(pm_reel), dimension(:,:), intent(in) ::  z2, phi2 ! amplitudes et phases des altitudes de l'onde 2
real(pm_reel),dimension(:,:), intent(out) ::  altit    ! altitudes en sortie
integer, intent(out)                      ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel) :: zlat, tmp, gravi ! variables intermediaires de calcul
  integer       :: i,j              ! indices de boucle

  intrinsic real, cos

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_alt.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  do i = latmin,latmax
     zlat = real(i,pm_reel) * 10._pm_reel -90._pm_reel
     do j = 1, pm_nb_alt
        tmp = zbar(j,i)+z1(j,i)*10._pm_reel*cos(pm_deg_rad*(long-phi1(j,i))) &
             +z2(j,i)*10._pm_reel*cos(pm_deg_rad*(2._pm_reel*long-phi2(j,i)))

        ! note--actual mks (si) gravity is gravity*9.81

        gravi = 1._pm_reel - pm_rv1 *  cos(pm_deg_rad*zlat*2._pm_reel)&
             + pm_rv2 * (cos(pm_deg_rad*zlat*2._pm_reel))**2._pm_reel
        altit(j,i-latmin+1) = pm_rt*tmp/(pm_rt*gravi-tmp)/pm_rv3
     end do
  end do

end subroutine mpi_atmi_alt
