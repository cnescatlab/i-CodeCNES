subroutine mu_eq2degre_reel (a, b, c, nb_racine, x1, x2, code_retour, type_eq)

! (C) Copyright CNES - MSPRO - 2000

!************************************************************************
!
! But:  Calcul des racines reelles de l'equation du second degre: 
! ===                    ax2 + bx + c = 0
!
! Note d'utilisation: 
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MUDISCRIM de la MSLIB f77
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
 
  use parametre_mspro
  use valeur_code_retour_mspro
  use numero_routine_mspro

  use mslib

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            :: a ! coefficient du terme en x2
real(pm_reel), intent(in)                            :: b ! coefficient du terme en x
real(pm_reel), intent(in)                            :: c ! coefficient de degre 0
integer, intent(out)                                 :: nb_racine ! nombre de racines
real(pm_reel), intent(out)                           :: x1 ! premiere racine
real(pm_reel), intent(out)                           :: x2 ! seconde racine
type(tm_code_retour), intent(out)                    :: code_retour
integer, intent(out), optional                       :: type_eq ! type d'equation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  real(pm_reel) :: presque_zero    ! plus petit nombre machine
  real(pm_reel) :: racine          ! valeur de la racine
  real(pm_reel) :: p, q, r, s, sdn ! variables intermediaires
  real(pm_reel) :: discrim         ! discriminant

  intrinsic abs, tiny, sqrt, sign

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mu_eq2degre_reel.f90: derniere modification V5.15 >'

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  ! Autres initialisations

  presque_zero = tiny(1._pm_reel)

  x1 = 0._pm_reel ! par convention pour les cas degrades
  x2 = 0._pm_reel ! par convention pour les cas degrades

  ! test du cas ou a, b et c sont nuls (infinite de solutions)

  if ((abs(a) < presque_zero).and.((abs(b) < presque_zero).and.(abs(c) < presque_zero))) then
     nb_racine = pm_infinite_racines
     type_eq = pm_eq_degenere

  else if ((abs(a) < presque_zero).and.(abs(b) < presque_zero)) then ! test du cas ou a et b sont nuls
     nb_racine = pm_0racine
     type_eq = pm_eq_degenere

  elseif (abs(a) < presque_zero) then   !test du cas ou a est nul
     nb_racine = pm_1racine_simple
     type_eq = pm_eq1degre
     x1 = -c / b

  else
     type_eq = pm_eq2degre

     ! traitement du cas nominal :
     ! mise sous forme px2 - 2qx + r = 0 de l'equation initiale ax2 + bx + c = 0

     p =  a
     q =  - b / 2._pm_reel
     r =  c

     discrim = q**2 - p * r ! calcul du discriminant

     ! traitement du cas b = 0 et racines reelles (a.c < 0.)

     if ((abs(q) <= presque_zero).and.((p * r) < 0._pm_reel) ) then
        nb_racine = pm_2racines
        racine = sqrt (- r / p)
        x1 =  racine
        x2 = -racine

     else if (discrim < 0._pm_reel) then !traitement du cas ou il n'y a pas de racines reelles
        nb_racine = pm_0racine

     else if (abs(discrim) <= presque_zero) then ! traitement du cas ou il y a une racine double
        nb_racine  = pm_1racine_double
        racine = q/p
        x1 = racine
        x2 = racine

     else !  traitement du cas ou il y a deux racines distinctes
        nb_racine = pm_2racines
        s = sqrt(discrim)
        sdn = q + sign (s, q)
        x1 = r / sdn
        x2 = sdn / p

     end if

  end if
  

  code_retour%routine = pm_num_mu_eq2degre_reel
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_eq2degre_reel
