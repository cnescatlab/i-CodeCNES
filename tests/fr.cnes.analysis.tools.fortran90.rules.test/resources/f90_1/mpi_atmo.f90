subroutine mpi_atmo (ralti1,rtarra,rzarra,rpress,rtemp,rpres,rdens,retour )

! (C) Copyright CNES - MSPRO - 2001-2003

!************************************************************************
!
! But:  Calcul de la temperature, pression, densite du modele ATMOspherique cira. 
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
!   + Version 1.0: creation a partir de la routine MCATMO 
!                         (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : ajout du use au module interface de mui_interp_newton
!                         (Date: 04/2002 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV (dont revision algorithme)
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
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
  use parametre_interne_mspro

use int_util_internes_mspro, only : mui_interp_newton

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)               ::  ralti1 ! altitude
real(pm_reel), dimension(:), intent(in) ::  rtarra ! interplation des temperatures
real(pm_reel), dimension(:), intent(in) ::  rzarra ! interpolation des altitude
real(pm_reel), dimension(:), intent(in) ::  rpress ! tableau des pressions
real(pm_reel), intent(out)              ::  rtemp  ! temperature
real(pm_reel), intent(out)              ::  rpres  ! pression
real(pm_reel), intent(out)              ::  rdens  ! densite atmospherique
integer, intent(out)                    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! -------------------

  real(pm_reel) :: alti, rgrad, rbeta, ralfa ! variables intermediaires de calcul
  integer       :: i                         ! indice de boucle
  real(pm_reel) :: temperature, pression     ! variables intermediaires du calcul de la temperature et de
                                             ! la pression
  logical       :: termine                   ! flag d'arret de boucle

  intrinsic abs, log

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mpi_atmo.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisation de la valeur du code retour
  ! ..........................................
  retour = pm_OK

  ! Calculs
  ! .......

  i    = 0
  termine = pm_i_non

  do while (.not.termine)
     i    = i + 1
     if ((ralti1 <= rzarra(i+1)) .or. (i >= pm_nb_alt-1)) termine = pm_i_oui
  end do

  ! calcul de la temperature

  alti = ralti1  
  temperature = mui_interp_newton (rtarra(i),rtarra(i+1),rzarra(i),rzarra(i+1),alti)

  rtemp = temperature
  rgrad = (rtarra(i+1)-rtarra(i)) / (rzarra(i+1)-rzarra(i))
  

  ! calcul de la pression
 
  if ( abs(rgrad) >= 1.e-3_pm_reel ) then
     rbeta = log(rpress(i+1)/rpress(i)) / log(rtarra(i+1)/rtarra(i))
     pression = (temperature/rtarra(i))**rbeta * rpress(i)
  else
     ralfa = (ralti1-rzarra(i)) / (rzarra(i+1)-rzarra(i))
     pression = rpress(i) * (rpress(i+1)/rpress(i))**ralfa
  end if
  rpres = pression
  

  ! calcul de la densite atmospherique

  rdens = pression / ( pm_ro * temperature )
  

end subroutine mpi_atmo
