 function mui_interp_newton ( A1, A2, B1, B2, B )

! (C) Copyright CNES - MSPRO - 2001

!************************************************************************
!
! But:  INTERPolation de NEWTON
! ===
!
! Note d'utilisation:  Cette fonction ne necessite pas de code retour
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la fonction MUINTE de la MSLIB f77
!                         (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : creation d'un numero OT pour mui_interp_newton
!                         (Date: 04/2002 - Realisation: Guylaine Prat)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)!
!************************************************************************

  ! Modules
  ! =======
  use mslib

  use parametre_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: a1 ! element 1 de type a
real(pm_reel), intent(in) :: a2 ! element 2 de type a
real(pm_reel), intent(in) :: b1 ! element 1 de type b
real(pm_reel), intent(in) :: b2 ! element 2 de type b
real(pm_reel), intent(in) :: b  ! grandeur physique
real(pm_reel)             :: mui_interp_newton ! valeur calculee

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  intrinsic abs

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mui_interp_newton.f90: derniere modification V5.15 >'

  !************************************************************************

  if ( abs(b2-b1) < pm_v1 ) then
     mui_interp_newton = a1
  else
     mui_interp_newton = a1 + (a2-a1)/(b2-b1) * (b-b1)
  end if

end function mui_interp_newton
