subroutine mu_resol_sys_lin (n,mat_LU,vect_P,vect_B,vect_X,code_retour)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Resolution d''un systeme lineaire AX = B ou A est donnee 
! ===  sous sa forme factorisee PA = LU, avec X et B des vecteurs
!
! Note d'utilisation:  
! ==================
! * La dimension n doit etre > 1
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

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice 1er membre factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel), dimension (n),   intent(in)           :: vect_B    ! vecteur 2nd membre
real(pm_reel), dimension (n),   intent(out)          :: vect_X    ! vecteur inconnu
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer       :: i,j       ! compteur

real(pm_reel), dimension (n)  :: Y           ! vecteur intermediaire de calcul
real(pm_reel), dimension (n)  :: vect_X_tmp  ! vecteur intermediaire de calcul

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_resol_sys_lin.f90: derniere modification V5.15 >'

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

! calcul de Y = U.X en resolvant L.Y = P.B
Y(1) = vect_B(vect_P(1))

do i = 2,n

   Y(i) = vect_B(vect_P(i))

   do j = 1,i-1
      Y(i) = Y(i) - Y(j) * mat_LU(vect_P(i),j)
   end do

end do

! calcul de X avec U.X = Y

! Rappel: la matrice U est inversible, donc son determinant est non nul.
!         Or U est triangulaire donc det(U) = produit des termes diagonaux 
!         Par consequent tous les termes (quelque soit i) 
!                  mat_LU(vect_P(i),i) sont differents de 0

vect_X_tmp(n) = Y(n) / mat_LU(vect_P(n),n)

do i = n-1,1,-1

   vect_X_tmp(i) = Y(i)

   do j = i+1,n
      vect_X_tmp(i) = vect_X_tmp(i) - vect_X_tmp(j) * mat_LU(vect_P(i),j)
   end do

   vect_X_tmp(i) = vect_X_tmp(i) / mat_LU(vect_P(i),i)
end do

! affectation de la sortie
vect_X(:) = vect_X_tmp(:)

6000 continue

code_retour%routine = pm_num_mu_resol_sys_lin
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_resol_sys_lin
