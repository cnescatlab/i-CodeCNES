subroutine mu_car_spher (car, spher, code_retour)

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But:  Passage des coordonnees cartesiennes aux cordonnees spheriques.
! ===
!
! Note d'utilisation:  La transformation inverse peut etre effectuee a l'aide de
! ==================   la routine mu_spher_car.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 0.1 : creation a partir de la routine MUCARSPHER de la MSLIB f77
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 5.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 5.4 : modification
!                   FA-ID 439 : remarques qualite
!                   (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
  !************************************************************************

  ! Modules
  ! =======

  use mslib
use int_util_internes_mspro, only : mui_dot_product3

  use type_mspro
  use valeur_code_retour_mspro
  use numero_routine_mspro

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3),intent(in)               ::  car   ! coordonnees cartesiennes
type(tm_spher), intent(out)                          ::  spher ! coordonnees spheriques correspondantes
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  real(pm_reel) :: presque_zero ! plus petit reel positif non nul
  real(pm_reel) :: longitude    ! valeur intermediaire de la longitude
  real(pm_reel) :: distance     ! valeur intermediaire de la distance
  real(pm_reel) :: prod_scal    ! produit scalaire
  integer       :: mu_retour

  intrinsic tiny, sqrt, abs, asin

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mu_car_spher.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  ! autres initialisations

  spher%lat  = 0._pm_reel
  spher%long = 0._pm_reel
  spher%dist = 0._pm_reel
  presque_zero = tiny(1._pm_reel) ! recherche du plus petit reel positif non nul

  ! Calcul des coordonnees spheriques

  call mui_dot_product3 ( car , car , prod_scal , mu_retour )
  ! retour non teste car toujours positif

  distance = sqrt ( prod_scal )          ! calcul de la distance

  if (distance <= presque_zero) then     ! la distance est quasi nulle : longitude et latitude sont indefinies
     code_retour%valeur = pm_warn_lat_long_indef 
     spher%lat  = 0._pm_reel               ! par convention, on pose longitude = latitude = 0
     spher%long = 0._pm_reel
     ! on a deja initialise : spher%dist = 0._pm_reel
     go to 6000
  end if

  if ((abs(car(1))<= presque_zero).and.(abs(car(2)) <= presque_zero)) then ! la longitude est indefinie
     code_retour%valeur = pm_warn_long_indef
     spher%long = 0._pm_reel             ! par convention, on pose longitude = 0

  else
     call mu_angle2(car(1), car(2), longitude , code_retour)
     ! code retour de mu_angle2 teste par le "if"
     spher%long = longitude

  end if

  spher%lat = asin(car(3) / distance) ! latitude dans [-pi/2, +pi/2]
  spher%dist = distance

6000 continue

  code_retour%routine = pm_num_mu_car_spher
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_car_spher
