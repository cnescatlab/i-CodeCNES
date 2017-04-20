subroutine mu_inter_ind (dim_x,vect_X,x_int,i_x,code_retour,pt_double,interpol)

! (C) Copyright CNES - MSPRO - 2001-2004

!************************************************************************
!
! But:  Dans le cadre d'une interpolation, recherche de l'indice d'un  
! ===   point appartenant a un vecteur de points ordonnes
!
! Note d'utilisation: pour plus de details se reporter a la documentation 
! ==================  utilisateur et a la note algorithmique (M-NT-0-96-CIS)
!
!   L'algorithme a ete cree de facon a optimiser le temps calcul:
!   il est donc autonome et ne fait appel a aucune autre routine ou fonction.
!
!   Les contraintes sur les entrees sont:
!       - les valeurs sont ordonnees de maniere croissante
!       - la taille dim_x du vecteur vect_X est > 1
!   Le premier critere est appele "critere d'ordonnancement de mu_inter_ind".
!   Aucune verification n'est faite sur ce critere pour des raisons de temps calcul.
!
!   La notion de points doubles (ou multiples) indique que 2 (ou plus) points sont confondus.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 09/2001 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE 1 ed01 rev00) : amelioration temps calcul 1ere phase
!                         (Date: 02/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE 2 ed01 rev00) : Suppression de la contrainte sur les bornes
!                                     (une borne peut etre un point multiple)
!                         (Date: 08/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE 3 ed01 rev00) : Suppression du warning pm_warn_interpol_pt_double
!                                     et ajout de l'argument interpol
!                         (Date: 09/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (FA 1 ed01 rev00) : calcul errone de ic dans 1ere phase 
!                         (Date: 09/2003 - Realisation: Guylaine Prat)
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

use parametre_mspro
use parametre_interne_mspro ! valeurs logiques oui et non

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                         intent(in) :: dim_x   ! dimension n du vecteur X
real(pm_reel), dimension(dim_x), intent(in) :: vect_X  ! vecteur X contenant x(1), ..., x(n)
real(pm_reel),                   intent(in) :: x_int   ! valeur pour laquelle on cherche l'indice i_x
integer,                      intent(inout) :: i_x     ! indice associe a x_int 
type(tm_code_retour), intent(out)       :: code_retour

integer, intent(in),optional            :: pt_double ! parametre pour gestion des points double
integer, intent(out),optional           :: interpol  ! type d'interpolation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! indicateur pour le traitement des points doubles 
integer :: direction                 ! (reaffectee si pt_double non present)

! valeurs pour les tests sur les reels
real(pm_reel), parameter :: epsilon_machine = (epsilon(1._pm_reel)) ! epsilon machine
real(pm_reel), parameter :: fois_epsilon_machine = (64._pm_reel*epsilon_machine) ! multiple d'epsilon machine

real(pm_reel)            :: epsX ! epsilon pour tests sur les valeurs de vect_X

! valeurs pour definir si les bornes sont des points multiples
logical :: min_multiple, max_multiple ! bornes min et max

! valeurs pour la recherche grossiere
integer       :: min_abs, max_abs, i_min, i_max   ! indices des extrema
real(pm_reel) :: x_min, x_max                     ! abscisses des extremas

! valeurs pour la 1ere phase
integer, parameter :: taille_K = 5  ! taille du tableau grossier (avant inclusion points multiples)
integer       :: i_c, i_med         ! indices intermediaires
real(pm_reel) :: x_c, x_med         ! abscisses intermediaires      
real(pm_reel) :: d_12, d_23, d_13, d_x1, d_x2, d_x3  ! distances entre 2 abscisses (pour estimation quadratique)

intrinsic epsilon, max, floor

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_inter_ind.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! Verifications
! =============

if (dim_x < 2) then ! taille du vecteur vect_X inferieure a 2
   code_retour%valeur = pm_err_dim_inf2
   go to 6000
end if

if (present(pt_double)) then ! test de la presence et de la validite de pt_double

   if ((pt_double /= pm_avant) .AND. (pt_double /= pm_apres)) then ! mauvaise valeur pour le parametre pt_double
      code_retour%valeur = pm_err_pt_double
      go to 6000
   end if

   direction = pt_double

else ! affectation de la valeur par defaut

   direction = pm_avant
   
end if

! Initialisations
! ===============
epsX = fois_epsilon_machine * max (1._pm_reel, abs(vect_X(1)),abs(vect_X(dim_x)))

min_abs = 1
max_abs = dim_x
x_min = vect_X(min_abs)
x_max = vect_X(max_abs)
min_multiple = pm_i_non
max_multiple = pm_i_non
if (present(interpol)) interpol = pm_pt_simple

i_c = i_x ! valeur de l'indice courant (issu d'un appel precedent)

! Traitement des bornes 
! =====================
! recadrage (si besoin) des bornes min et max en cas de points multiples

do while ((min_abs < max_abs) .AND. ((vect_X(min_abs+1) - x_min) < epsX))
   min_abs = min_abs + 1
   x_min = vect_X(min_abs)
   min_multiple = pm_i_oui
end do

do while ((max_abs > min_abs) .AND. ((x_max - vect_X(max_abs-1)) < epsX))
   max_abs = max_abs - 1
   x_max = vect_X(max_abs)
   max_multiple = pm_i_oui
end do

! Verification de securite contre un tableau degenere
! ===================================================
if ((vect_X(max_abs) - vect_X(min_abs)) < epsX) then
   code_retour%valeur = pm_err_valeur_repetee
   go to 6000
end if

! Verfication si mode extrapolation (point hors domaine)
! ======================================================
! (definition du mode extrapolation et donc du test associe: choix du CNES;
!  pas de prise en compte de marge avec epsX)

if ((x_int < vect_X(min_abs)) .OR. (x_int > vect_X(max_abs))) then

    code_retour%valeur = pm_warn_extrapol ! mode extrapolation sans bornes multiples

    if ((x_int < vect_X(min_abs)) .AND. (min_multiple) ) & ! la borne min est multiple
         code_retour%valeur = pm_warn_extrapol_borne_double

    if ((x_int > vect_X(max_abs)) .AND. (max_multiple) ) & ! la borne max est multiple
         code_retour%valeur = pm_warn_extrapol_borne_double

 end if

! Exclusion du dernier point
! ==========================
max_abs = max_abs - 1 ! on ne veut pas du dernier point de l'intervalle

! Initialisation de la recherche avec tout l'intervalle
! =====================================================
i_min = min_abs
x_min = vect_X(i_min)
i_max = max_abs
x_max = vect_X(i_max)

! 1ere phase: reduction de la taille du tableau
! =============================================
do while (((i_max - i_min) > taille_K) .AND. ((x_max - x_min) > epsX))

   if (i_c <= i_min) then
      i_c = i_min + 1
   else if (i_c >= i_max) then
      i_c = i_max - 1
   end if

   x_c = vect_X(i_c)

   if ((x_c - x_int) < epsX) then
      i_min = i_c
      x_min = x_c
   else
      i_max = i_c
      x_max = x_c
   end if

   ! estimation de l'index
   i_med = (i_min+i_max)/2  ! division entiere
   x_med = vect_X(i_med)

   if (((x_med - x_min) < epsX) .OR. ((x_max - x_med) < epsX)) then
      i_c = i_med
   else
      ! estimation quadratique
      d_12 = x_max - x_med
      d_23 = x_med - x_min
      d_13 = x_max - x_min
      d_x1 = x_int - x_max
      d_x2 = x_int - x_med
      d_x3 = x_int - x_min

      i_c = floor( ((d_x2*d_x3*d_23)*i_max - (d_x1*d_x3*d_13)*i_med + (d_x1*d_x2*d_12)*i_min) / &
                    (d_12*d_23*d_13) )

      if ((i_c <= i_min) .OR. (i_c >= i_max)) then ! estimation lineaire

         i_c = floor( (d_x3*i_max - d_x1*i_min) / d_13 )

      end if

   end if

end do

! 2ieme phase: inclusion des points multiples
! ===========================================
if (direction == pm_avant) then
   do while ( (i_min > min_abs) .AND. ((x_int - vect_X(i_min)) < epsX) ) 
      i_min = i_min - 1
   end do
else ! direction == pm_apres
   do while ( (i_max < max_abs) .AND. ((vect_X(i_max) - x_int) < epsX) ) 
      i_max = i_max + 1
   end do

end if

! 3ieme phase: recherche iterative
! ================================

i_c = i_min

do while ( (i_c < i_max) .AND. ((vect_X(i_c+1) - x_int) < epsX) )
  i_c = i_c + 1
end do

if ((x_int - vect_X(i_c)) < epsX) then

  if (direction == pm_avant) then

    if ( (i_c > min_abs) .AND. ((x_int - vect_X(i_c-1)) < epsX) ) then

       do while ( (i_c > min_abs) .AND. ((x_int - vect_X(i_c-1)) < epsX) )
          i_c = i_c - 1
       end do

       if (present(interpol)) interpol = pm_pt_double ! cas hors bornes

       i_x = i_c - 1

       go to 5000 ! traitement commun des bornes

    else

       i_x = i_c

       go to 5000 ! traitement commun des bornes

    end if

  else ! direction == pm_apres

    if ( (i_c > min_abs) .AND. ((x_int - vect_X(i_c-1)) < epsX) ) then

       if (present(interpol)) interpol = pm_pt_double ! cas hors bornes

    end if

    i_x = i_c

    go to 5000 ! traitement commun des bornes

  end if ! fin test sur direction

end if

i_x = i_c

5000 continue

! traitement commun des bornes 
if (present(interpol)) then

   if ((i_x == min_abs) .AND. (min_multiple)) interpol = pm_borne_min_double
   if ((i_x == max_abs) .AND. (max_multiple)) interpol = pm_borne_max_double

end if

6000 continue

code_retour%routine = pm_num_mu_inter_ind
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_inter_ind
