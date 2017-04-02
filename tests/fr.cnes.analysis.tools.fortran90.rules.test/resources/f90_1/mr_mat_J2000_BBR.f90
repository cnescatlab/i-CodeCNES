subroutine mr_mat_J2000_BBR (pos_Pla1, vit_Pla1, pos_Pla2, vit_Pla2, mat, code_retour)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Calcul de la matrice de passage du repere J2000 au repere BBR
! ===
!
! Note d'utilisation:
! ==================
!
!$Historique
! ==========
!   + Version 6.2 : creation a partir de la routine MOD_MAT_EME2000RLAG de la LIBIP90
!                         (Date: 11/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3 et mulvect3
!                   (Date: 03/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_norme
use int_utilitaires, only : mu_prod_vect

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    :: pos_Pla1 ! position de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: vit_Pla1 ! vitesse de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: pos_Pla2 ! position de la planete 2 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: vit_Pla2 ! vitesse de la planete 2 dans le J2000
real(pm_reel), dimension(6,6), intent(out) :: mat      ! matrice de passage de J2000 au BBR
type(tm_code_retour), intent(out)          :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel), dimension(6)  :: p1p2     ! position-vitesse Pla2 - Pla1
real(pm_reel), dimension(3)  :: x        ! cosinus directeurs de Pla2 - Pla1
real(pm_reel), dimension(3)  :: ztmp, z  ! axe Z du repere BBR
real(pm_reel), dimension(3)  :: y        ! troisieme axe completant le triedre
real(pm_reel), dimension(3,3):: pmat     ! matrice de passage de J2000 a BBR (positions seules)
real(pm_reel), dimension(3)  :: Vtmp                        ! vitesse temporaire
real(pm_reel), dimension(3,3):: omegatmp, omega             ! pour la rotation J2000 -> BBR
real(pm_reel)                :: dist_p1p2, norme2z, eps100  ! valeurs intermediaires
type(tm_code_retour)         :: code_retour_local

intrinsic epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mr_mat_J2000_BBR.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_mat_J2000_BBR.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

eps100 = 100._pm_reel  * epsilon(1._pm_reel)! constante eps pour les reels

! calculs
! =======

! calcul de l'axe x du nouveau repere
! -----------------------------------

p1p2(1:3) = pos_Pla2(1:3) - pos_Pla1(1:3)
p1p2(4:6) = vit_Pla2(1:3) - vit_Pla1(1:3)

call mu_norme(p1p2(1:3), dist_p1p2, code_retour, vect_norme = x)
if (dist_p1p2 < eps100) then  ! les deux planetes sont confondues
                              ! test plus fort que pm_err_vect_nul
   code_retour%valeur = pm_err_meme_planete
   go to 6000
end if

! calcul des vecteurs z et y pour completer le triedre
! ----------------------------------------------------

! z est suivant le moment cinetique du mouvement de Pla2 
! par rapport a Pla1

call mu_prod_vect(x, p1p2(4:6), ztmp, code_retour)
if (code_retour%valeur /= pm_OK) go to 6000

call mu_norme(ztmp,  norme2z ,  code_retour,  vect_norme = z)
if (code_retour%valeur /= pm_OK) go to 6000

! y complete le triedre (avec x et z normes)
call mu_prod_vect(z, x, y, code_retour)
if (code_retour%valeur /= pm_OK) go to 6000

pmat(1,1:3) = x(1:3)
pmat(2,1:3) = y(1:3)
pmat(3,1:3) = z(1:3)

! calcul de la vitesse de Pla2 par rapport a Pla1 exprimee dans BBR
! -----------------------------------------------------------------

call mu_mulvect3(pmat,p1p2(4:6),Vtmp,code_retour_local)

! calcul de la matrice adjointe a la rotation de J2000 vers le BBR exprimee dans le BBR

omegatmp(:,:) =   0._pm_reel
omegatmp(1,2) =   vtmp(2)/dist_p1p2
omegatmp(2,1) = - vtmp(2)/dist_p1p2

! calcul de la matrice adjointe a la rotation de J2000 vers le BBR exprimee dans le J2000

call mu_matmul3(omegatmp,pmat,omega,code_retour_local)
! pas d'erreur possible donc pas de test dur code retour

! Formation de la matrice finale

mat(:,:)     = 0._pm_reel
mat(1:3,1:3) = pmat(1:3,1:3)
mat(4:6,4:6) = pmat(1:3,1:3)
mat(4:6,1:3) = omega(1:3,1:3)

6000 continue

code_retour%routine = pm_num_mr_mat_J2000_BBR
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_mat_J2000_BBR
