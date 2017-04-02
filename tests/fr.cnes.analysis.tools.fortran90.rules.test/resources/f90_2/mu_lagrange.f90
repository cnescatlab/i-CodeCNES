subroutine mu_lagrange (nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, x_int, y_int, code_retour)

! (C) Copyright CNES - MSPRO - 2000-2004

!************************************************************************
!
! But: Calcul d'un point interpole par le polynome de Lagrange d'ordre n
! ===
!
! Note d'utilisation: . La dimension nb_pt_max du tableau vect_X doit etre superieure ou egale a nb_pt
! ==================  . le nombre de points nb_pt doit etre strictement superieur a 1
!                     . La dimension des ordonnees dim_y doit etre superieure ou egale a 1
!                     . Les abscisses doivent etre toutes differentes et ordonnees
!                     . L'abscisse x dont on veut l'ordonnes doit appartenir au domaine des abscisses
!                       [vect_X(1), vect_X(nb_pt)]
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MUINTLAGN de la MSLIB f77
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV (dont revision algorithme)
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
use valeur_code_retour_mspro
use numero_routine_mspro
use parametre_interne_mspro

use mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                  ::  nb_pt        ! nombre de points
integer, intent(in)                                  ::  nb_pt_max    ! taille du tableau
real(pm_reel), dimension(nb_pt_max), intent(in)      ::  vect_X       ! abscisses
integer, intent(in)                                  ::  dim_y        ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in)::  mat_Y        ! ordonnees
real(pm_reel), intent(in)                            ::  x_int        ! abscisse x du point dont on veut les ordonnees
real(pm_reel), dimension(dim_y),intent(out)          ::  y_int        ! ordonnees du point x               
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)                                   ::  vmin, vmax              ! minimum et maximum des abscisses
real(pm_reel)                                   ::  eps100                  ! epsilon de comparaison pour les reels
real(pm_reel)                                   ::  ecart, ecartmin, ho, hp ! ecarts entre 2 abscisses
real(pm_reel)                                   ::  w, den                  ! elements de calcul de d et c 
real(pm_reel), dimension(:,:), allocatable      ::  d, c                    ! tableaux de travail dynamiques
real(pm_reel), dimension(:), allocatable        ::  yint                    ! sortie intermediaire
integer                                         ::  i, j, m, ind            ! indices
integer                                         ::  ok                      ! retour de l'allocation
logical                                         ::  termine                 ! flag d'interruption de boucle

intrinsic epsilon, max, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mu_lagrange.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations

eps100 = 100._pm_reel * epsilon(1._pm_reel)

if (dim_y <= 0) then                      ! la dimension des ordonnees est negative ou nulle
   code_retour%valeur = pm_err_dim_y_inf1
   go to 6000

else if (nb_pt <= 2) then                 ! le nombre de points est inferieur ou egal a 2
   code_retour%valeur = pm_err_nb_pt_inf2
   go to 6000

else if (nb_pt > nb_pt_max) then          ! le nombre de points est superieur a la taille du tableau
   code_retour%valeur = pm_err_nb_pt_max
   go to 6000
else                                      !on verifie que tous les vect_X sont differents et que la suite est ordonnee
   i = 1
   termine = pm_i_non
   do while (.not.termine)
      ecart = eps100 * max(1._pm_reel, abs(vect_X(i+1)))
      
      if (vect_X(i) >= (vect_X(i+1) + ecart)) then   !les abscisses ne sont pas ordonnees
         code_retour%valeur = pm_err_x_non_ord_croi
         termine = pm_i_oui
      
      else if (vect_X(i) >= (vect_X(i+1) - ecart)) then ! les abscisses ne sont pas toutes differentes
         code_retour%valeur = pm_err_x_non_diff
         termine = pm_i_oui
         
      else
         i = i + 1
      end if
      if (i > (nb_pt - 1)) termine = pm_i_oui
   end do
   if (code_retour%valeur /= pm_OK) go to 6000
end if
   
! min et max des abscisses

vmin = vect_X(1)
vmax = vect_X(nb_pt)

if ((x_int < vmin).or.(x_int > vmax)) then ! l'abscisse x_int n'appartient pas a [min,max]
   code_retour%valeur = pm_err_int_hors_domaine
   go to 6000
end if

! algorithme de neuville
! on recherche l'indice ind du point de discretisation le plus proche de x_int

ind = 1
ecartmin = abs(x_int - vect_X(1))
do  i = 2, nb_pt
   ecart = abs(x_int - vect_X(i))
   if (ecart < ecartmin) then
      ind = i
      ecartmin = ecart
   end if
end do

! allocation de la taille des tableaux c, d et yint

allocate(c(dim_y,nb_pt_max), stat= ok)
if (ok /= pm_OK) then ! l'allocation dynamique de c a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
allocate(d(dim_y,nb_pt_max), stat= ok)
if (ok /= pm_OK) then ! l'allocation dynamique de d a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if
allocate(yint(dim_y), stat= ok)
if (ok /= pm_OK) then ! l'allocation dynamique de yint a echoue
   code_retour%valeur = pm_err_allocate
   go to 6000
end if

! initialisation des tableaux c et d de l'algorithme

c(:, :) = mat_Y(1:dim_y,1:nb_pt_max)
d(:, :) = mat_Y(1:dim_y,1:nb_pt_max)

yint(:) = mat_Y(1:dim_y, ind) ! approximation initiale de yint

ind  = ind - 1

! pour chaque colonne du tableau on calcule c et d 
! et on met a jour le tableau yint de maniere a 
! ce que le chemin parcouru soit le plus rectiligne 
! possible dans ce tableau.

do  m = 1, nb_pt - 1

   do  i = 1, nb_pt - m

      ho  = vect_X(i) - x_int
      hp  = vect_X(i + m) - x_int

      do  j = 1, dim_y

         w   = c(j, i + 1) - d(j, i)
         den = w/(ho - hp)
         c(j, i) = ho*den
         d(j, i) = hp*den

      end do

   end do

   if ((2*ind) < (nb_pt - m)) then

      yint(:) = yint(:) + c(:, ind + 1)

   else

      yint(:) = yint(:) + d(:, ind)
      ind  = ind - 1

   end if

end do

y_int(1:dim_y) = yint(:)

! desallocation des tableaux de travail
deallocate(c, stat= ok)
if (ok /= pm_OK) code_retour%valeur = pm_err_deallocate  ! la desallocation de c a echoue
deallocate(d, stat= ok)
if (ok /= pm_OK) code_retour%valeur = pm_err_deallocate  ! la desallocation de d a echoue
deallocate(yint, stat= ok)
if (ok /= pm_OK) code_retour%valeur = pm_err_deallocate  ! la desallocation de yint a echoue

6000 continue

code_retour%routine = pm_num_mu_lagrange
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_lagrange
