subroutine mu_3rot_quat (def_3rot, angle1, angle2, angle3, quat,code_retour)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Calcul du quaternion associe a une rotation definie par trois angles d'Euler
! ===   ou de Cardan
!
! Note d'utilisation:  voir la documentation utilisateur et la note algorithmique
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 607 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
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
use int_util_internes, only : mui_axe_norme_quat
use int_utilitaires, only : mu_prod_quat
use parametre_mslib

use parametres_internes_mslib
use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                      :: def_3rot   ! definition et ordre d'enchainement des trois rotations
real(pm_reel), intent(in)                :: angle1     ! valeur de l'angle associe a la premiere rotation
real(pm_reel), intent(in)                :: angle2     ! valeur de l'angle associe a la deuxieme rotation
real(pm_reel), intent(in)                :: angle3     ! valeur de l'angle associe a la troisieme rotation
type(tm_quat), intent(out)               :: quat       ! quaternion
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
type (tm_quat)        :: quat1           ! quaternion 1 de rotation elementaire
type (tm_quat)        :: quat2           ! quaternion 2 de rotation elementaire
type (tm_quat)        :: quat3           ! quaternion 3 de rotation elementaire
type (tm_quat)        :: quat_prod1      ! produit de 2 quaternions
type (tm_quat)        :: quat_prod2      ! produit de 2 quaternions
integer               :: ii              ! indice de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_3rot_quat.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter:: rcs_id =' $Id: mu_3rot_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Calculs des rotations elementaires
! ==================================

select case (def_3rot)

case(pm_1x_2y_3z)
   quat1 = mui_axe_norme_quat (pm_i_axe_x,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_y,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_z,angle3)

case(pm_1x_2z_3y)
   quat1 = mui_axe_norme_quat (pm_i_axe_x,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_z,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_y,angle3)

case(pm_1y_2x_3z)
   quat1 = mui_axe_norme_quat (pm_i_axe_y,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_x,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_z,angle3)

case(pm_1y_2z_3x)
   quat1 = mui_axe_norme_quat (pm_i_axe_y,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_z,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_x,angle3)

case(pm_1z_2x_3y)
   quat1 = mui_axe_norme_quat (pm_i_axe_z,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_x,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_y,angle3)

case(pm_1z_2y_3x)
   quat1 = mui_axe_norme_quat (pm_i_axe_z,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_y,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_x,angle3)

case(pm_1x_2y_3x)
   quat1 = mui_axe_norme_quat (pm_i_axe_x,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_y,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_x,angle3)

case(pm_1x_2z_3x)
   quat1 = mui_axe_norme_quat (pm_i_axe_x,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_z,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_x,angle3)

case(pm_1y_2x_3y)
   quat1 = mui_axe_norme_quat (pm_i_axe_y,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_x,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_y,angle3)

case(pm_1y_2z_3y)
   quat1 = mui_axe_norme_quat (pm_i_axe_y,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_z,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_y,angle3)

case(pm_1z_2x_3z)
   quat1 = mui_axe_norme_quat (pm_i_axe_z,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_x,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_z,angle3)

case(pm_1z_2y_3z)
   quat1 = mui_axe_norme_quat (pm_i_axe_z,angle1)
   quat2 = mui_axe_norme_quat (pm_i_axe_y,angle2)
   quat3 = mui_axe_norme_quat (pm_i_axe_z,angle3)

case default
   code_retour%valeur=pm_err_clef_rot
   go to 6000
end select

call mu_prod_quat ( quat1, quat2, quat_prod1, code_retour ) ! le code retour est toujours OK

call mu_prod_quat ( quat_prod1, quat3, quat_prod2, code_retour ) ! le code retour est toujours OK

quat%q0 = quat_prod2%q0
do ii=1,3
   quat%q123(ii)= quat_prod2%q123(ii)
end do

6000 continue

code_retour%routine = pm_num_mu_3rot_quat
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_3rot_quat
