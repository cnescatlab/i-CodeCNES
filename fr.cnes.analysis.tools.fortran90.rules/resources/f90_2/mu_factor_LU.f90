subroutine mu_factor_LU (n,mat_A,eps,mat_LU,vect_P,code_retour)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Factorisation PA = LU d'une matrice carree A en deux matrices
! ===  triangulaires inferieure L et superieure U
!
! Note d'utilisation: 
! ==================
! * La matrice mat_LU contient les elements des 2 matrices triangulaires L et U, 
!   correspondant au resultat de la permutation P sur les lignes de 
!   (L + U - I), avec I matrice identite.
!
!   Cette matrice est gardee telle quelle pour des raisons de gain en temps calcul.
!
! * La dimension n doit etre > 1
!
! * Cette routine sert de preambule a d'autres routines d'algebre lineaire 
!   telles que: mu_det_mat, mu_resol_sys_lin, mu_inv_mat
!
! * Le code a ete recopie directement depuis le code de la bibliotheque Mantissa
! (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90. 
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
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

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_A     ! matrice carree a factoriser
real(pm_reel),                  intent(in)           :: eps       ! pivot maximum pour les permutations
real(pm_reel), dimension (n,n), intent(out)          :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(out)          :: vect_P    ! vecteur de permutation
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer       :: i,j,k     ! compteurs

integer       :: i_max     ! ligne ou se trouve le pivot max
real(pm_reel) :: pivot_max ! pivot maximal sur la colonne (en valeur absolue)

integer                        :: n_tmp      ! entier temporaire pour la permutation
real(pm_reel)                  :: inv_ajj, facteur  ! reels intermediaires de calcul
real(pm_reel), dimension (n,n) :: mat_LU_tmp ! matrice LU intermediaire de calcul
integer      , dimension (n)   :: vect_P_tmp ! vecteur P intermediaire de calcul

intrinsic abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_factor_LU.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations et tests
! ========================
code_retour%valeur = pm_OK

if (n < 2) then
   code_retour%valeur = pm_err_dim_mat
   go to 6000
end if

! calculs
! =======

mat_LU_tmp(:,:) = mat_A(:,:)

do i = 1,n
   vect_P_tmp(i) = i
end do

do j = 1,n ! boucle sur la dimension

   ! recherche du pivot maximal (en valeur absolue) dans la colonne 

   pivot_max = abs(mat_LU_tmp(vect_P_tmp(j),j))

   i_max = j
   do i = j+1,n

      if (abs(mat_LU_tmp(vect_P_tmp(i),j)) > pivot_max) then
         ! nouveau pivot max
         pivot_max = abs(mat_LU_tmp(vect_P_tmp(i),j))
         i_max = i
      end if

   end do

   ! erreur si le pivot max est inferieur a l'epsilon de l'utilisateur
   if (pivot_max < eps) then

      code_retour%valeur = pm_err_pivot_max_trop_faible   
      go to 6000

   else if (i_max /= j) then

     ! permutation a effectuer 
      n_tmp = vect_P_tmp(j)
      vect_P_tmp(j) = vect_P_tmp(i_max)
      vect_P_tmp(i_max) = n_tmp

   end if

   ! calculs des coefficients matriciels de L et U pour cette ligne
   inv_ajj = 1._pm_reel / mat_LU_tmp(vect_P_tmp(j),j)

   do i = j+1,n

      facteur = inv_ajj * mat_LU_tmp(vect_P_tmp(i),j)
      mat_LU_tmp(vect_P_tmp(i),j) = facteur

      do k = j+1,n
         mat_LU_tmp(vect_P_tmp(i),k) = mat_LU_tmp(vect_P_tmp(i),k) - facteur * mat_LU_tmp(vect_P_tmp(j),k)
      end do

   end do
   
end do ! fin boucle sur la dimension
      
! affectation des sorties
mat_LU(:,:) = mat_LU_tmp(:,:)
vect_P(:) = vect_P_tmp(:)

6000 continue

code_retour%routine = pm_num_mu_factor_LU
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_factor_LU
