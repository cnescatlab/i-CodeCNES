subroutine mu_spline_cub_eval ( nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, mat_d2Y,&
     x_int, y_int, code_retour, d1y_int, d2y_int )

! (C) Copyright CNES - MSPRO - 2000-2004

!************************************************************************
!
! But: Interpolation par SPLINE CUBique :  EVALuation
! ===
!
! Note d'utilisation: L'initialisation aura du etre effectuee par 
! ==================  la routine mu_spline_cub_init auparavant
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0  creation a partir de la version f77 de cette routine et de la note 
!                  technique M-ST-0-306-CN (Interpolation par une spline cubique - 
!                  Position du probleme, comparaison avec l'interpolation de Lagrange -
!                  Specification des routines MSLIB correspondantes)
!                         (Date: 11/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 13): suppression des dimensions implicites dans l'include arg_*.h 
!                         (Date: 12/2004 - Realisation: Bruno Revelin)
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
use valeur_code_retour_mspro
use numero_routine_mspro
use parametre_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                                   intent(in):: nb_pt      ! nombre de points
integer,                                   intent(in):: nb_pt_max  ! taille de tableau
real(pm_reel), dimension(nb_pt_max),       intent(in):: vect_X     ! abscisses
integer,                                   intent(in):: dim_y      ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in):: mat_Y      ! ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in):: mat_d2Y    ! derivees secondes
real(pm_reel),                             intent(in):: x_int      ! abscisse du point d'interpolation
real(pm_reel), dimension(dim_y),          intent(out):: y_int      ! ordonnees au point d'interpolation
type(tm_code_retour),                     intent(out):: code_retour
real(pm_reel), dimension(dim_y), intent(out), optional   :: d1y_int    ! derivees premieres en y
real(pm_reel), dimension(dim_y), intent(out), optional   :: d2y_int    ! derivees secondes en y

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)          :: eps100               ! epsilon de comparaison de reels
real(pm_reel)          :: ecart, h             ! ecarts entre 2 abscisses
real(pm_reel)          :: a, b                 ! intermediaires de calcul
integer                :: i, j, k, klo, khi    ! indices

intrinsic epsilon, max, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mu_spline_cub_eval.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
eps100 = 100._pm_reel * epsilon(1._pm_reel) !epsilon de comparaison pour les reels

! verifications sur les arguments d'entree
! ========================================

if (nb_pt  <  2) then ! le nombre d'abscisses d'interpolation doit etre > ou = 2
   code_retour%valeur = pm_err_nb_pt_inf2  
   go to 6000
end if
if (nb_pt  >  nb_pt_max) then  ! on verifie que nb_pt est inferieur ou egal a nb_pt_max
   code_retour%valeur = pm_err_nb_pt_max 
   go to 6000
end if
if (dim_Y  <  1) then ! on verifie que dim_Y est bien superieur ou egal a 1 
   code_retour%valeur = pm_err_dim_y_inf1
   go to 6000
end if

i=1
do while ( i <= (nb_pt -1)) ! on verifie que les elements du tableau x sont tous distincts et tries
                            ! dans l'ordre croissant
   ecart = eps100*max(1.0_pm_reel , abs(vect_X(i+1)))
   if (vect_X(i)  >=  vect_X(i+1) + ecart) then       ! les abscisses ne sont pas ordonnees
      code_retour%valeur = pm_err_x_non_ord_croi
      go to 6000
   elseif (vect_X(i)  >=  vect_X(i+1) - ecart) then   ! les abscisses sont trop proches
      code_retour%valeur = pm_err_x_non_diff
      go to 6000
   else
      i = i + 1
   end if

end do

if ((x_int  <  vect_X(1)) .or. (x_int  >  vect_X(nb_pt))) then ! abscissse hors du domaine
   code_retour%valeur = pm_err_int_hors_domaine
   go to 6000
end if

! on determine par bisection l'intervalle [vect_X(i), vect_X(i+1)] sur lequel se
! trouve l'abscisse x_int (notations klo = i, kli = i + 1)

klo = 1
khi = nb_pt

do while ((khi - klo)  >  1)
   k = (khi + klo)/2
   if (vect_X(k)  >  x_int) then
      khi = k
   else
      klo = k
   end if
end do

h = vect_X(khi) - vect_X(klo)

! on ne peut avoir ici h = 0 car dans la subroutine spline appellee 
! precedemment on a verifie que tous les elements du tableau vect_X etaient 
! distincts (a une tolerance relative pres). on calcule ensuite les elements
! du tableau y_int en evaluant les polynomes correspondants au point x_int
! ainsi que la derivee premiere d1y_int et la derivee seconde d2y_int si demandees.

a = (vect_X(khi) - x_int)/h
b = (x_int - vect_X(klo))/h

do j = 1, dim_Y
   y_int(j)  = a*mat_Y(j,klo) + b*mat_Y(j,khi) + ((a**3 - a)*mat_d2Y(j,klo) + &
               (b**3 - b)*mat_d2Y(j,khi))*h*h/6.0_pm_reel
   if (present(d1y_int)) d1y_int(j) = (mat_Y(j,khi) - mat_Y(j,klo))/h + &
                         ((1.0_pm_reel - 3.0_pm_reel*a*a)* mat_d2Y(j,klo) + &
                         (3.0_pm_reel*b*b - 1.0_pm_reel)*mat_d2Y(j,khi))*h/6.0_pm_reel
   if (present(d2y_int)) d2y_int(j) = a*mat_d2Y(j,klo) + b*mat_d2Y(j,khi)
end do

6000 continue

code_retour%routine = pm_num_mu_spline_cub_eval
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_spline_cub_eval
