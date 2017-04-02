subroutine mu_spher_car (spher, car, code_retour)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Passage des coordonnees spheriques aux coordonnees cartesiennes
! ===
!
! Note d'utilisation:  La transformation inverse peut s'effectuer a l'aide de
! ==================   la routine mu_car_spher. 
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 0.1 : creation a partir de la routine MUSPHERCAR de la MSLIB f77
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!************************************************************************

  ! Modules
  ! =======

  use mslib

  use type_mspro
  use valeur_code_retour_mspro
  use numero_routine_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_spher), intent(in)                           ::  spher    !  coordonnees spheriques
real(pm_reel), dimension(3), intent(out)             ::  car      !  coordonnees cartesiennes correspondantes
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  intrinsic cos, sin

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mu_spher_car.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  ! Calculs

  car(1) = spher%dist * cos(spher%lat) * cos(spher%long)
  car(2) = spher%dist * cos(spher%lat) * sin(spher%long)
  car(3) = spher%dist * sin(spher%lat)

  code_retour%routine = pm_num_mu_spher_car
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_spher_car
