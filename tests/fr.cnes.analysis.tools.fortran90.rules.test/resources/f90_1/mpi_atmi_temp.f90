subroutine mpi_atmi_temp ( long, latmin, latmax, tbar, t1, phit1, t2, phit2, tempe, retour )

! (C) Copyright CNES - MSPRO - 2001-2003

!************************************************************************
!
! But:  Pour l'interpolation dans le modele ATMospherique de cira, calcul des
! ===   TEMPeratures.
!
! Note d'utilisation:  Sans objet.
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MCCATE 
!                   (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                   (Date: 07/2003 - Realisation: Bruno Revelin)
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

  use valeur_code_retour_mspro
  use parametre_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                 :: long      ! longitude
integer, intent(in)                       :: latmin    ! latitude minimum
integer, intent(in)                       :: latmax    ! latitude maximum
real(pm_reel), dimension(:,:), intent(in) :: tbar      ! temperatures
real(pm_reel), dimension(:,:), intent(in) :: t1, phit1 ! amplitude et phase de la temperature de l'onde 1
real(pm_reel), dimension(:,:), intent(in) :: t2, phit2 ! amplitude et phase de la temperature de l'onde 2
real(pm_reel),dimension(:,:), intent(out) :: tempe     ! temperature
integer, intent(out)                      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  integer :: i,kk ! indices de boucle

  intrinsic cos

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmi_temp.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  do i = latmin , latmax
        kk      = i-latmin+1
        tempe (:,kk) = tbar(:,i)+t1(:,i)* &
                       cos(pm_deg_rad*(long-phit1(:,i)))+t2(:,i)*cos(pm_deg_rad*(2._pm_reel*long-phit2(:,i)))
  end do

end subroutine mpi_atmi_temp
