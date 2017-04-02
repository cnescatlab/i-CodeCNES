subroutine mu_matmul3 (mat33_1, mat33_2, mat33_res, code_retour)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Calcul du produit matriciel (3,3)x(3,3)
! ===   mat33_res = mat33_1 * mat33_2
!
! Note d'utilisation:  Les deux matrices en entree doivent etre de dimension
!                      (3,3). Pour des matrices (6,6), voir mu_matmul6
! ==================
!
!$Historique
! ==========
!   + Version 6.8 : DM-ID 859 : Optimisation des performances
!                   (Date: 03/2008 - Realisation: C. Hue Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
! Modules
! =======
use parametre_mslib

use parametres_internes_mslib
use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33_1     ! matrice 1
real(pm_reel), intent(in), dimension(3,3)  :: mat33_2     ! matrice 2
real(pm_reel), intent(out), dimension(3,3) :: mat33_res   ! matrice en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_matmul3.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_matmul3.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK
! Initialisation de la matrice résultat
mat33_res(:,:) = 0._pm_reel

! Pas de controle sur les entrees car pas de risque d'erreur pour un produit matriciel
! Calcul
! ======
mat33_res(1,1) = mat33_1(1,1)*mat33_2(1,1) + mat33_1(1,2)*mat33_2(2,1) + mat33_1(1,3)*mat33_2(3,1)
mat33_res(1,2) = mat33_1(1,1)*mat33_2(1,2) + mat33_1(1,2)*mat33_2(2,2) + mat33_1(1,3)*mat33_2(3,2)
mat33_res(1,3) = mat33_1(1,1)*mat33_2(1,3) + mat33_1(1,2)*mat33_2(2,3) + mat33_1(1,3)*mat33_2(3,3)

mat33_res(2,1) = mat33_1(2,1)*mat33_2(1,1) + mat33_1(2,2)*mat33_2(2,1) + mat33_1(2,3)*mat33_2(3,1)
mat33_res(2,2) = mat33_1(2,1)*mat33_2(1,2) + mat33_1(2,2)*mat33_2(2,2) + mat33_1(2,3)*mat33_2(3,2)
mat33_res(2,3) = mat33_1(2,1)*mat33_2(1,3) + mat33_1(2,2)*mat33_2(2,3) + mat33_1(2,3)*mat33_2(3,3)

mat33_res(3,1) = mat33_1(3,1)*mat33_2(1,1) + mat33_1(3,2)*mat33_2(2,1) + mat33_1(3,3)*mat33_2(3,1)
mat33_res(3,2) = mat33_1(3,1)*mat33_2(1,2) + mat33_1(3,2)*mat33_2(2,2) + mat33_1(3,3)*mat33_2(3,2)
mat33_res(3,3) = mat33_1(3,1)*mat33_2(1,3) + mat33_1(3,2)*mat33_2(2,3) + mat33_1(3,3)*mat33_2(3,3)

code_retour%routine = pm_num_mu_matmul3
code_retour%biblio = pm_mslib90

end subroutine mu_matmul3

