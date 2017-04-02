subroutine mu_compar_rot_quat (quat1, quat2, angle, code_retour)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  COMPARaison des ROTations associees a 2 QUATernions
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 6.0 (SP 627 ed01 rev00): creation 
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des
!                   boucles explicites
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
use int_utilitaires, only : mu_prod_quat
use int_utilitaires, only : mu_quat_norme
use int_utilitaires, only : mu_quat_axe_angle

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                           ::  quat1         ! quaternion a comparer
type(tm_quat), intent(in)                           ::  quat2         ! quaternion a comparer
real(pm_reel), intent(out)                          ::  angle         ! ecart angulaire entre les deux rotations associees a quat1 et quat2
type(tm_code_retour), intent(out)                   ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

type(tm_quat)    ::  quat_inv, quat3         ! quaternion inverse de quat2 et produit de quat1 par cet inverse
type(tm_quat)    ::  quat1_norme, quat2_norme! quaternions normes de quat1 et quat2
real(pm_reel), dimension(3) :: axe            
real(pm_reel)               :: angle_temp    ! angle d'ecart entre les rotations
real(pm_reel)               :: norme         ! norme des quaternions
real(pm_reel)               :: presque_zero  ! plus petit reel machine
type(tm_code_retour)        :: code_retour_local

intrinsic tiny

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_compar_rot_quat.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_compar_rot_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! plus petit reel machine
presque_zero = tiny(1._pm_reel)

! calculs
! =======

! normalisation des quaternions
call mu_quat_norme ( quat1, quat1_norme, norme, code_retour_local )
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

call mu_quat_norme ( quat2, quat2_norme, norme, code_retour_local )
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! A ce stade des calculs: quat1_norme et quat2_norme sont des quaternions non nuls

! Calcul de l'inverse de quat2
quat_inv%q0=-quat2_norme%q0
quat_inv%q123(1)=quat2_norme%q123(1)
quat_inv%q123(2)=quat2_norme%q123(2)
quat_inv%q123(3)=quat2_norme%q123(3)

! produit de quat1_norme * inverse(quat2) 
call mu_prod_quat ( quat1_norme, quat_inv, quat3, code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! angle associe au produit
call mu_quat_axe_angle ( quat3, axe, angle_temp, code_retour_local )
if (code_retour_local%valeur /= pm_OK)  then

    if (code_retour_local%valeur == pm_err_axe_rot_indef) then 
    ! Cas special: cela signifie qu'on compare des quaternions identiques ou 
    !              opposes, ce n'est donc pas une erreur, au contraire
       code_retour_local%valeur = pm_OK                        
    endif 

   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! angle est dans [0, 2pi] a ce stade des calculs

if ((angle_temp - pm_pi) >= presque_zero)  then ! angle obtus entre pi et 2pi: l'ecart de rotation n'ayant pas de sens 
   angle = pm_deux_pi - angle_temp   !(direct ou retrograde) on recadre entre 0 et pi
else
   angle = angle_temp  
end if

6000 continue

code_retour%routine = pm_num_mu_compar_rot_quat
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_compar_rot_quat
