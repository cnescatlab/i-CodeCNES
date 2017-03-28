subroutine mu_inv_mat (n,mat_LU,vect_P,mat_inv,code_retour)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Inversion d'une matrice A donnee sous sa forme factorisee PA = LU
! ===
!
! Note d'utilisation:  
! ==================
! * La dimension n doit etre > 1 (teste via mu_resol_sys_lin)
!
! * La factorisation LU doit etre obtenue a l'aide de mu_factor_LU.
!
!   Rappel:
!   la matrice mat_LU contient les elements des 2 matrices triangulaires L et U, 
!   correspondant au resultat de la permutation P sur les lignes de 
!   (L + U - I), avec I matrice identite.
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

use int_utilitaires_mspro, only : mu_resol_sys_lin

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel), dimension (n,n), intent(out)          :: mat_inv   ! matrice inverse
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer       :: i         ! compteur

real(pm_reel), dimension (n)   :: vect_X      ! vecteur intermediaire de calcul
real(pm_reel), dimension (n)   :: vect_B      ! vecteur intermediaire de calcul

type(tm_code_retour) ::  code_retour_local ! code retour des routines appelees

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_inv_mat.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations et tests
! ========================
code_retour%valeur = pm_OK

vect_X(:) = 0._pm_reel
vect_B(:) = 0._pm_reel

! calculs
! =======
!                                                                         -1
! on considere l'inversion de matrice comme un pb a systeme lineaire : A.A  = I(n,n)
!                   -1
! On pose x(n,n) = A     et b(n,n) = I(n,n)
! et on resoud n systemes lineaires: A.X(i) = B(i) 
!                                    ou x(n,n) = [X(1) | X(2) | ... | X(n)]
!                                    et b(n,n) = [B(1) | B(2) | ... | B(n)]

! 1er systeme a resoudre
vect_B(1) = 1._pm_reel

call mu_resol_sys_lin(n,mat_LU,vect_P,vect_B,vect_X,code_retour_local)
if (code_retour_local%valeur /= pm_OK) then 
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

mat_inv(:,1) = vect_X(:)

! resolution de tous les autres systemes lineaires A.X(i) = B(i) pour i > 1
do i = 2,n

   vect_B(i-1) = 0._pm_reel
   vect_B(i)   = 1._pm_reel

   call mu_resol_sys_lin(n,mat_LU,vect_P,vect_B,vect_X,code_retour_local)

   if (code_retour_local%valeur /= pm_OK) then 
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK) go to 6000
   end if

   mat_inv(:,i) = vect_X(:)

end do

6000 continue

code_retour%routine = pm_num_mu_inv_mat
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_inv_mat
