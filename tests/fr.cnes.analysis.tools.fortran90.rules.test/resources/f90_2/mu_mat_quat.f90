subroutine mu_mat_quat (mat, quat, code_retour)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Calcul du QUATernion associe a une MATrice de rotation.
! ===
!
! Note d'utilisation:  
! ==================
!           La matrice est de rotation si elle est orthonormale 
!                                         et de determinant = +1
!           La transformation inverse peut s'effectuer par la routine mu_quat_mat
!
!$Historique
! ==========
!   + Version 5.0 (SP 604 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de transpose3 et matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3,3), intent(in)           ::  mat     ! matrice 
type(tm_quat), intent(out)                          ::  quat    ! quaternion associe a la matrice
type(tm_code_retour), intent(out)                   ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel), parameter :: eps_norme_frob = 1.e-10_pm_reel ! epsilon pour la norme de Frobenius

real(pm_reel) :: determinant ! determinant de la matrice M

real(pm_reel), dimension(3,3) :: matT               ! transpose MT de mat (= M)
real(pm_reel), dimension(3,3) :: prod_MTM           ! produit MT * M
real(pm_reel), dimension(3,3) :: MTM_moins_I        ! calcul: MT * M - I

type(tm_code_retour)          ::  code_retour_local

real(pm_reel), dimension(9), parameter :: matI_ligne = (/ &  ! matrice identite I (en ligne)
       1._pm_reel, 0._pm_reel, 0._pm_reel,  &
       0._pm_reel, 1._pm_reel, 0._pm_reel,  &
       0._pm_reel, 0._pm_reel, 1._pm_reel   /) 

real(pm_reel),dimension(3,3)::mati

real(pm_reel), dimension (3,3) :: carre_terme  ! carre de chacun des elements de la matrice
real(pm_reel)                  :: norme_frob ! norme de Frobenius
! matrices de masques pour calculs de la trace
logical, dimension(9), parameter  :: masque = (/ &
        .true. , .false., .false., &
        .false., .true. , .false., &
        .false., .false., .true.  /)

logical,dimension(3,3)::mat_masque

real(pm_reel) :: trace

! quaternion et multiples
real(pm_reel), dimension (0:3) :: quatre_q_carre ! 4*qi*qi (i = 0 a 3)
real(pm_reel)                  :: quatre_q0, q0=0._pm_reel, quatre_qjj
real(pm_reel), dimension (1:3) :: vect_q ! vecteur q123

integer              :: jj, kk, nn ! indices pour calcul generique du vecteur q123
integer,dimension(1) :: jj_max     ! a cause de la fonction maxloc

integer :: i,j ! indices de boucles

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_mat_quat.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_mat_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

 matI = reshape(matI_ligne, (/3,3/)) ! matrice identite I
 mat_masque = reshape(masque, (/3,3/))
code_retour%valeur = pm_OK

! Verification que la matrice est bien une matrice de rotation
! ============================================================

! -----------------------------------------------------------------------
! matrice orthonormale: MT * M = I ? ou MT est la matrice transpose de M
!                                       et I la matrice identite
! si oui, cela assure egalement que  | determinant | = 1
! -----------------------------------------------------------------------

! calcul de MT * M
call mu_transpose3(mat,matT, code_retour_local)
call mu_matmul3(mat,matT,prod_MTM,code_retour_local)
! pas d'erreur possible donc pas de test dur code retour

! calcul de MT * M - I
MTM_moins_I(1,1) = prod_MTM(1,1) - matI(1,1)
MTM_moins_I(1,2) = prod_MTM(1,2) - matI(1,2)
MTM_moins_I(1,3) = prod_MTM(1,3) - matI(1,3)
MTM_moins_I(2,1) = prod_MTM(2,1) - matI(2,1)
MTM_moins_I(2,2) = prod_MTM(2,2) - matI(2,2)
MTM_moins_I(2,3) = prod_MTM(2,3) - matI(2,3)
MTM_moins_I(3,1) = prod_MTM(3,1) - matI(3,1)
MTM_moins_I(3,2) = prod_MTM(3,2) - matI(3,2)
MTM_moins_I(3,3) = prod_MTM(3,3) - matI(3,3)

! calcul du produit terme a terme de la matrice  MT * M - I
do i = 1,3
   do j = 1,3
      carre_terme(i,j) = MTM_moins_I(i,j) * MTM_moins_I(i,j)
   end do
end do

! calcul de la norme de Frobenius de la matrice A = 
! racine carre (somme des carres des elements de la matrice A)
norme_frob = sqrt(sum(carre_terme))

! test si la norme est differente de zero
if (norme_frob > eps_norme_frob) then
   code_retour%valeur = pm_err_mat_non_rot
   go to 6000
end if

! determinant vaut + 1 ?
determinant = mat(1,1) * (mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2)) + &
              mat(2,1) * (mat(1,3)*mat(3,2) - mat(1,2)*mat(3,3)) + &
              mat(3,1) * (mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))

if (determinant < 0._pm_reel) then ! il suffit de verifier que determinant < 0 
                                   ! car vaut +/- 1 a ce stade des calculs
   code_retour%valeur = pm_err_mat_non_rot
   go to 6000
end if

! Calcul des elements du quaternion
! =================================

! calcul de la trace de la matrice (somme des elements diagonaux)
trace = sum(pack(mat,mat_masque))

! calcul des carres des elements du quaternion ( * 4 )
quatre_q_carre(0) = trace + 1._pm_reel  ! 4*q0*q0

do i = 1,3
   quatre_q_carre(i) = 2._pm_reel * mat(i,i) - trace + 1._pm_reel ! 4*qi*qi
end do

! recherche de l'indice associe a la plus grande valeur parmi 
! 4*q0*q0 , 4*q1*q1 , 4*q2*q2  , 4*q3*q3
! (les valeurs numeriques sont comprises entre 0 et 4;
!  et la plus grande est superieure a 1)

jj_max = maxloc(quatre_q_carre) - 1  ! a cause du decalage de position lie au tableau

jj = jj_max(1)   ! jj = 0 si concerne q0
                 ! jj = 1 si concerne q1 , 2 pour q2 et 3 pour q3

select case(jj) ! traitement selon la plus grande valeur trouvee

case (0)   ! cas ou 4*q0*q0 est la plus grande valeur

   quatre_q0 = 2._pm_reel * sqrt (quatre_q_carre(0)) ! solution positive retenue
   q0        = quatre_q0 * 0.25_pm_reel

   vect_q(1) = (mat(2,3) - mat(3,2)) / quatre_q0
   vect_q(2) = (mat(3,1) - mat(1,3)) / quatre_q0
   vect_q(3) = (mat(1,2) - mat(2,1)) / quatre_q0

case(1:3)  ! cas ou 4*q1*q1 , 4*q2*q2  ou 4*q3*q3 est la plus grande valeur

   ! constitution des triplets (jj,kk,nn) : 
   !   jj = 1 => (1,2,3)
   !   jj = 2 => (2,3,1)
   !   jj = 3 => (3,1,2)
   kk = modulo(jj,3) + 1 
   nn = modulo(kk,3) + 1

   quatre_qjj = 2._pm_reel * sqrt (quatre_q_carre(jj)) ! solution positive retenue

   vect_q(jj) = quatre_qjj * 0.25_pm_reel
   vect_q(kk) = (mat(jj,kk) + mat(kk,jj)) / quatre_qjj
   vect_q(nn) = (mat(jj,nn) + mat(nn,jj)) / quatre_qjj

   q0 = (mat(kk,nn) - mat(nn,kk)) / quatre_qjj

! pas de cas default necessaire ici

end select

! Affectation finale
! Comme le select case ci-dessus est exhaustif, les variables q0 et vect_q
! sont forcement initialisees a ce stade

quat%q0      = q0
quat%q123(1) = vect_q(1)
quat%q123(2) = vect_q(2)
quat%q123(3) = vect_q(3)

6000 continue

code_retour%routine = pm_num_mu_mat_quat
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_mat_quat
