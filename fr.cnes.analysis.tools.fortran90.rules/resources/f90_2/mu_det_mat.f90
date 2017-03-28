subroutine mu_det_mat (n,mat_LU,vect_P,det,code_retour)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But:  Calcul du determinant d''une matrice carree A donnee sous 
! ===   sa forme factorisee PA = LU
!
! Note d'utilisation: 
! ==================
! * Pas de tests effectue sur n < 2 (algorithme marche pour n = 1)
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
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
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
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel),                  intent(out)          :: det       ! determinant
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer       :: i,j       ! compteur
logical       :: signat    ! signature de la permutation
real(pm_reel) :: det_tmp   ! valeur temporaire du determinant

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_det_mat.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations et tests
! ========================
code_retour%valeur = pm_OK

det_tmp = 1._pm_reel
signat = .true.

! calculs
! =======

! calcul du produit des termes diagonaux de la matrice U

do i = 1,n
   det_tmp = det_tmp * mat_LU(vect_P(i),i)
end do

! calcul de la signature de la permutation vect_P =
! on compte le nombre de couples i < j, tels que vect_P(i) > vect_P(j)

do i = 1,n-1
   do j = i+1,n
      if (vect_P(i) > vect_P(j)) signat = .not.signat
   end do
end do

! d'ou le determinant

if (.not.signat) then
   det = - det_tmp
else
   det = det_tmp
end if

code_retour%routine = pm_num_mu_det_mat
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_det_mat
