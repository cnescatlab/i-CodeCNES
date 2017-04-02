subroutine mu_spline_cub_init (nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, mat_d2Y, code_retour, &
                             vect_d1y1, vect_d1yn, d1y1_fixe, d1yn_fixe)

! (C) Copyright CNES - MSPRO - 2000-2004

!************************************************************************
!
! But: Interpolation par SPLINE CUBique : INITialisation. 
! ===
!
! Note d'utilisation: . La routine d'evaluation proprement dite est mu_spline_cub_eval 
! ==================  . Si les entrees facultatives vect_d1y1 et vect_d1yn sont absentes,
!                       alors les derivees premieres en x(1) et x(n) sont libres,
!                       et les derivees secondes sont imposees a 0 en x(1) et x(n).
!                     . Si les seules entrees facultatives sont vect_d1y1 et vect_d1yn,
!                       alors les derivees premieres en x(1) et x(n) sont imposees.
!                     . S'il y a les quatre entrees facultatives et que d1y1_fixe(i) (resp. d1yn_fixe(i))
!                       est faux, alors on ne tient pas compte de la valeur contenue dans vect_d1y1(i)
!                       (resp. vect_d1yn(i)); on en tient compte sinon.
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
!   + Version 3.0 (FA 1) : correction du bug sur j dans les boucles sans d1y1_fixe et sans d1yn_fixe
!                         (Date: 01/2003 - Auteur: Bruno Revelin)
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

use valeur_code_retour_mspro
use numero_routine_mspro
use parametre_mspro
use mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                                   intent(in) :: nb_pt      ! nombre de points
integer,                                   intent(in) :: nb_pt_max  ! taille de tableau
real(pm_reel), dimension(nb_pt_max),       intent(in) :: vect_X     ! abscisses
integer,                                   intent(in) :: dim_y      ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in) :: mat_Y      ! ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(out):: mat_d2Y    ! derivees secondes
type(tm_code_retour),                      intent(out):: code_retour
real(pm_reel), dimension(dim_y), intent(in), optional    :: vect_d1y1  ! derivees premieres en x(1)
real(pm_reel), dimension(dim_y), intent(in), optional    :: vect_d1yn  ! derivees premieres en x(n)
logical,       dimension(dim_y), intent(in), optional    :: d1y1_fixe  ! d1y1_fixe
logical,       dimension(dim_y), intent(in), optional    :: d1yn_fixe  ! d1yn_fixe

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)                                        :: eps100   ! epsilon de comparaison des reels
integer                                              :: i, j, k  ! indices
real(pm_reel)                                        :: ecart    ! ecart entre 2 abscisses
real(pm_reel)                                        :: sig, p   ! intermediaires de calcul
real(pm_reel), dimension(pm_max_ordonnees,nb_pt_max) :: u        ! terme de la matrice finale
real(pm_reel), dimension(pm_max_ordonnees)           :: un, qn   ! termes de ;a matrice finale au nieme point
real(pm_reel), dimension(dim_y, nb_pt_max)           :: matd2Y   ! affectation intermediaire de mat_d2Y

intrinsic epsilon, max, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mu_spline_cub_init.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
eps100 = 100._pm_reel * epsilon(1._pm_reel) !epsilon de comparaison pour les reels

do i = 1, dim_y
   do j = 1, nb_pt_max
      mat_d2Y(i,j) = 0.0_pm_reel
      matd2Y(i,j) = 0.0_pm_reel
   enddo
enddo

do i = 1, pm_max_ordonnees
   do j = 1, nb_pt_max
      u(i,j) = 0.0_pm_reel
   enddo
enddo

do i = 1, pm_max_ordonnees
   un(i) = 0.0_pm_reel
   qn(i) = 0.0_pm_reel
enddo

! Verifications sur les arguments d'entree 
! ========================================

if (nb_pt  <  2) then              ! on verifie que nb_pt est superieur ou egal a 2
   code_retour%valeur = pm_err_nb_pt_inf2  
   go to 6000
end if

if (nb_pt  >  nb_pt_max) then      ! on verifie que nb_pt est inferieur ou egal a nb_pt_max
   code_retour%valeur = pm_err_nb_pt_max 
   go to 6000
end if

if (dim_y  <  1) then              !on verifie que dim_y est bien superieur ou egal a 1
   code_retour%valeur = pm_err_dim_y_inf1
   go to 6000
end if

i = 1
do while ( i <= (nb_pt -1)) !on verifie que les elements du tableau x sont tous distincts et tries
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

if ( ((.not.present(vect_d1y1)).and.(present(d1y1_fixe))).or.&  ! parametrage optionnel incoherent
     ((.not.present(vect_d1yn)).and.(present(d1yn_fixe))) ) then
   code_retour%valeur = pm_warn_para_option
end if

! en x(1) on distingue le cas ou la derivee premiere est libre (derivee 
! seconde nulle) du cas ou elle est imposee

if(.not.present(vect_d1y1)) then ! les derivees premieres sont libres et
                                 ! les derivees secondes mises a 0
   matd2Y(:,1) = 0.0_pm_reel
   u(:,1)       = 0.0_pm_reel
else
   if (present(d1y1_fixe)) then
      do j = 1, dim_y
         if (d1y1_fixe(j)) then ! les derivees premieres sont imposees
            matd2Y(j,1) = -0.5_pm_reel
            u(j,1)       = (3.0_pm_reel/(vect_X(2) - vect_X(1)))*((mat_Y(j,2) - mat_Y(j,1))/&
                 (vect_X(2) - vect_X(1)) - vect_d1y1(j))
         else                   ! les derivees premieres sont libres et les derivees secondes mises a 0
            matd2Y(j,1) = 0.0_pm_reel
            u(j,1)       = 0.0_pm_reel
         end if
      end do
   else                         ! les derivees premieres sont imposees
      matd2Y(:,1) = -0.5_pm_reel
      u(:,1)       = (3.0_pm_reel/(vect_X(2) - vect_X(1)))*((mat_Y(:,2) - mat_Y(:,1))/&
           (vect_X(2) - vect_X(1)) - vect_d1y1(:))
   end if
end if

! triangularisation des systemes lineaires tridiagonaux a resoudre

do i = 2, nb_pt - 1
   sig    = (vect_X(i) - vect_X(i-1))/(vect_X(i+1) - vect_X(i-1))
   do j = 1, dim_y
      p        = sig*matd2Y(j,i-1) + 2.0_pm_reel
      matd2Y(j,i) = (sig - 1.0_pm_reel)/p
      u(j,i)   = (6.0_pm_reel*((mat_Y(j,i+1) - mat_Y(j,i))/(vect_X(i+1) - vect_X(i)) - &
           (mat_Y(j,i) - mat_Y(j,i-1))/(vect_X(i) - vect_X(i-1)))/(vect_X(i+1) - vect_X(i-1)) - &    
           sig*u(j,i-1))/p
   end do
end do

! en x(n) on distingue le cas ou la derivee premiere est libre (derivee 
! seconde nulle) du cas ou elle est imposee

if(.not.present(vect_d1yn)) then ! les derivees premieres sont libres et les derivees secondes mises a 0
   qn(:) = 0.0_pm_reel
   un(:) = 0.0_pm_reel
else
   if (present(d1yn_fixe)) then
      do j = 1, dim_y
         if (d1yn_fixe(j)) then ! les derivees premieres sont imposees
            qn(j) = 0.5_pm_reel
            un(j) = (3.0_pm_reel/(vect_X(nb_pt) - vect_X(nb_pt-1)))*(vect_d1yn(j) - & 
                 (mat_Y(j,nb_pt) - mat_Y(j,nb_pt-1))/(vect_X(nb_pt) - vect_X(nb_pt-1)))
         else                   ! les derivees premieres sont libres et les derivees secondes mises a 0
            qn(j) = 0.0_pm_reel
            un(j) = 0.0_pm_reel 
         end if
      end do
   else                         ! les derivees premieres sont imposees
      qn(:) = 0.5_pm_reel
      un(:) = (3.0_pm_reel/(vect_X(nb_pt) - vect_X(nb_pt-1)))*(vect_d1yn(:) - & 
           (mat_Y(:,nb_pt) - mat_Y(:,nb_pt-1))/(vect_X(nb_pt) - vect_X(nb_pt-1)))
   end if
end if

! calcul des derivees secondes par resolution des systemes lineaires 
! triangularises c'est a dire par substitution inverse (de mat_d2Y(j,nb_pt) a mat_d2Y(j,1)
! j = 1...dim_y)

do j = 1, dim_y

   matd2Y(j,nb_pt) = (un(j) - qn(j)*u(j,nb_pt-1))/(qn(j)*matd2Y(j,nb_pt-1) + 1.0_pm_reel)

   do  k = nb_pt - 1, 1, -1
      matd2Y(j,k) = matd2Y(j,k)*matd2Y(j,k+1) + u(j,k)
   end do

end do

do i = 1, dim_y
   do j = 1, nb_pt_max
      mat_d2Y(i,j) = matd2Y(i,j) ! affectation de mat_d2Y
   enddo
enddo
!mat_d2Y(:,:) = matd2Y(:,:) ! affectation de mat_d2Y

6000 continue

code_retour%routine = pm_num_mu_spline_cub_init
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_spline_cub_init
