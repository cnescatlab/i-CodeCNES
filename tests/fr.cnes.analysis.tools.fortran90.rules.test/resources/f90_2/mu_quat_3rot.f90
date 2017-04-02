subroutine mu_quat_3rot (def_3rot, quat, angle1, angle2, angle3, code_retour)

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Calcul des trois angles de Cardan ou d'Euler associe a une rotation 
! ===   definie par un quaternion 
!
! Note d'utilisation:   voir la documentation utilisateur et la note algorithmique
! ==================
!
!$Historique
! ==========
!   + Version 5.0 (SP 606 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 6.3 (DM-ID 394) : Qualite : augmentation du taux de commentaires
!                 (Date: 11/2005 - Realisation: Atos Origin)   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 suppression du module math_mslib
!     déjà inclu par parametres_internes_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites
!                     par des boucles explicites
!                   (Date: 03/2008 - Realisation: Atos origin)
!
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

use int_utilitaires, only : mu_quat_norme
use int_utilitaires, only : mu_quat_rep
use int_utilitaires, only : mu_angle2

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

integer, intent(in)                       :: def_3rot   ! definition et ordre d'enchainement des trois rotations
type(tm_quat), intent(in)                 :: quat       ! quaternion

real(pm_reel), intent(out)                :: angle1     ! valeur de l'angle associe a la premiere rotation
real(pm_reel), intent(out)                :: angle2     ! valeur de l'angle associe a la deuxieme rotation
real(pm_reel), intent(out)                :: angle3     ! valeur de l'angle associe a la troisieme rotation
type(tm_code_retour), intent(out)         ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

type (tm_quat)        :: quat_norme      ! quaternion norme
type (tm_quat)        :: quatinv         ! quaternion precedent inverse
real(pm_reel)         :: norme           ! norme
real(pm_reel), dimension(3) ::V, V1, V2  ! vecteurs transformes par rotation

! Parametres
!============

real(pm_reel),parameter :: eps_angle = 1.e-9_pm_reel ! environ 1 milliseconde d'arc
real(pm_reel),parameter :: un_moins_epsilon = 1._pm_reel-eps_angle   ! valeur de comparaison pour un vecteur

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_quat_3rot.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_quat_3rot.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

!normalisation du quaternion
!==========================
call mu_quat_norme ( quat, quat_norme, norme, code_retour )
if (code_retour%valeur < pm_ok) go to 6000

!calcul du quaternion inverse q-1
!================================
quatinv%q0   = -quat_norme%q0
quatinv%q123(1) = quat_norme%q123(1)
quatinv%q123(2) = quat_norme%q123(2)
quatinv%q123(3) = quat_norme%q123(3)

! ALGO
! ======

! A ce stade des calculs: quat_norme et quatinv sont des quaternions non nuls
! (pas de test necessaire sur le code retour de mu_quat_rep)

select case (def_3rot)
! def_3rot correspond a un entier qui est defini par un parametre associe
! a une rotation
! il y a 6 possibilites pour les angles de Cardan et 6 possibilites pour les angles d'Euler

   ! Angle d'Euler
   !==============

case(pm_1x_2y_3x)
!  1ere rotation autour de X, 2eme rotation autour de Y, 3eme rotation autour de Z

   call mu_quat_rep ( pm_i_axe_x, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_x, quatinv, V2, code_retour )
   
   if (abs(V1(1)) < un_moins_epsilon)then
      call mu_angle2 ( V1(3) , V1(2) , angle3 , code_retour )
      call mu_angle2 ( -V2(3) , V2(2) , angle1 , code_retour )
      angle2 = acos(V1(1))

   else ! cosinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_y, quatinv, V, code_retour )     
      if (V1(1)>0._pm_reel) then
         angle2 = 0._pm_reel
      else
         angle2 = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(2) , V(3) , angle1 , code_retour ) 
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1x_2z_3x)
!  1ere rotation autour de X, 2eme rotation autour de Z, 3eme rotation autour de X

   call mu_quat_rep ( pm_i_axe_x, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_x, quatinv, V2, code_retour )

   if (abs(V1(1)) < un_moins_epsilon)then
      call mu_angle2 ( -V1(2) , V1(3) , angle3 , code_retour )
      call mu_angle2 ( V2(2) , V2(3) , angle1 , code_retour )
      angle2 = acos(V1(1))

   else ! cosinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_z, quatinv, V, code_retour )

      if (V1(1)>0._pm_reel) then
         angle2 = 0._pm_reel      
      else
         angle2 = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(3) , -V(2) , angle1 , code_retour )
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1y_2x_3y)
!  1ere rotation autour de Y, 2eme rotation autour de X, 3eme rotation autour de Y

   call mu_quat_rep ( pm_i_axe_y, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_y, quatinv, V2, code_retour )

   if (abs(V1(2)) < un_moins_epsilon) then
      call mu_angle2 ( -V1(3) , V1(1) , angle3 , code_retour )
      call mu_angle2 ( V2(3) , V2(1) , angle1 , code_retour )
      angle2 = acos(V1(2))

   else ! cosinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_x, quatinv, V, code_retour )

      if (V1(2)>0._pm_reel) then
         angle2 = 0._pm_reel     
      else
         angle2  = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(1) , -V(3) , angle1 , code_retour )
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1y_2z_3y)
!  1ere rotation autour de Y, 2eme rotation autour de Z, 3eme rotation autour de Y

   call mu_quat_rep ( pm_i_axe_y, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_y, quatinv, V2, code_retour )

   if (abs(V1(2)) < un_moins_epsilon) then
      call mu_angle2 ( V1(1) , V1(3) , angle3 , code_retour )
      call mu_angle2 (  -V2(1) ,  V2(3) , angle1 , code_retour )
      angle2 = acos(V1(2))

   else ! cosinus(angle2)= +/-1
      call mu_quat_rep ( pm_i_axe_z, quatinv, V, code_retour )

      if (V1(2)>0._pm_reel) then
         angle2 = 0._pm_reel     
      else
         angle2 = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(3) , V(1) , angle1 , code_retour )
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1z_2x_3z)
!  1ere rotation autour de Z, 2eme rotation autour de X, 3eme rotation autour de Z

   call mu_quat_rep ( pm_i_axe_z, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_z, quatinv, V2, code_retour )

   if ( abs(V1(3)) < un_moins_epsilon) then
      call mu_angle2 ( V1(2) , V1(1) , angle3 , code_retour )
      call mu_angle2 ( -V2(2) , V2(1) , angle1 , code_retour )
      angle2 = acos(V1(3))

   else ! cosinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_x, quatinv, V, code_retour )

      if (V1(3)>0._pm_reel) then
         angle2 = 0._pm_reel     
      else
         angle2 = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(1) , V(2) , angle1 , code_retour )
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1z_2y_3z)
!  1ere rotation autour de Z, 2eme rotation autour de Y, 3eme rotation autour de Z

   call mu_quat_rep ( pm_i_axe_z, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_z, quatinv, V2, code_retour )

   if (abs(V1(3)) < un_moins_epsilon) then
      call mu_angle2 ( -V1(1) , V1(2) , angle3 , code_retour )
      call mu_angle2 ( V2(1) , V2(2) , angle1 , code_retour )
      angle2 = acos(V1(3))

   else ! cosinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_y, quatinv, V, code_retour )

      if (V1(3)>0._pm_reel) then
         angle2 = 0._pm_reel

      else
         angle2 = pm_pi
      end if
      angle3 = 0._pm_reel
      call mu_angle2 ( V(2), -V(1) , angle1 , code_retour )
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

   ! Angle de Cardan
   !==================

case(pm_1x_2y_3z)
!  1ere rotation autour de X, 2eme rotation autour de Y, 3eme rotation autour de Z

   call mu_quat_rep ( pm_i_axe_x, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_z, quatinv, V2, code_retour )

   if (abs(V1(3)) < un_moins_epsilon) then
      call mu_angle2 ( V1(1) ,-V1(2) , angle3 , code_retour )
      call mu_angle2 ( V2(3) , -V2(2) , angle1 , code_retour )
      angle2 = asin(V1(3))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_y, quat_norme, V, code_retour ) 
      angle1 = 0._pm_reel
      call mu_angle2 ( v(2) , V(1) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,V1(3))   
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1x_2z_3y)
!  1ere rotation autour de X, 2eme rotation autour de Z, 3eme rotation autour de Y

   call mu_quat_rep ( pm_i_axe_x, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_y, quatinv, V2, code_retour )

   if (abs(V1(2)) < un_moins_epsilon) then
      call mu_angle2 (  V1(1), V1(3) , angle3 , code_retour )
      call mu_angle2 ( V2(2) , V2(3) , angle1 , code_retour )
      angle2 = asin( -V1(2))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_z, quat_norme, V, code_retour ) 
      angle1 = 0._pm_reel
      call mu_angle2 ( v(3) ,- V(1) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,-V1(2))  
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1y_2x_3z)
!  1ere rotation autour de Y, 2eme rotation autour de X, 3eme rotation autour de Z

   call mu_quat_rep ( pm_i_axe_y, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_z, quatinv, V2, code_retour )

   if (abs(V1(3)) < un_moins_epsilon) then
      call mu_angle2 ( V1(2) , V1(1) , angle3 , code_retour )
      call mu_angle2 ( V2(3),  V2(1) , angle1 , code_retour )
      angle2 = asin(-V1(3))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_x, quat_norme, V, code_retour )
      angle1 = 0._pm_reel
      call mu_angle2 ( v(1) , -V(2) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,-V1(3))
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1y_2z_3x)
!  1ere rotation autour de Y, 2eme rotation autour de Z, 3eme rotation autour de X

   call mu_quat_rep ( pm_i_axe_y, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_x, quatinv, V2, code_retour )

   if (abs(V1(1)) < un_moins_epsilon) then
      call mu_angle2 ( V1(2), -V1(3)  , angle3 , code_retour )
      call mu_angle2 ( V2(1), -V2(3)  , angle1 , code_retour )
      angle2 = asin(V1(1))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_z, quat_norme, V, code_retour )
      angle1 = 0._pm_reel
      call mu_angle2 ( V(3) , V(2) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,V1(1))
      code_retour%valeur = pm_warn_angle1_ou_3_indef 
   end if

case(pm_1z_2x_3y)
!  1ere rotation autour de Z, 2eme rotation autour de X, 3eme rotation autour de Y

   call mu_quat_rep ( pm_i_axe_z, quat_norme, V1, code_retour )
   call mu_quat_rep ( pm_i_axe_y, quatinv, V2, code_retour )

   if (abs(V1(2)) < un_moins_epsilon) then
      call mu_angle2 ( V1(3)  ,- V1(1)  , angle3 , code_retour )
      call mu_angle2 ( V2(2), -V2(1) , angle1 , code_retour )
      angle2 = asin(V1(2))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_x, quat_norme, V, code_retour )
      angle1 = 0._pm_reel
      call mu_angle2 ( v(1), V(3) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,V1(2))
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case(pm_1z_2y_3x)
!  1ere rotation autour de Z, 2eme rotation autour de Y, 3eme rotation autour de X

   call mu_quat_rep ( pm_i_axe_z, quat_norme, V1, code_retour )

   call mu_quat_rep ( pm_i_axe_x, quatinv, V2, code_retour )

   if (abs(V1(1)) < un_moins_epsilon) then
      call mu_angle2 ( V1(3), V1(2) , angle3 , code_retour )
      call mu_angle2 ( V2(1) , V2(2) , angle1 , code_retour )
      angle2 = asin(-V1(1))

   else ! sinus(angle2)= +/- 1
      call mu_quat_rep ( pm_i_axe_y, quat_norme, V, code_retour )
      angle1 = 0._pm_reel
      call mu_angle2 ( V(2) , -V(3) , angle3 , code_retour )
      angle2 = sign( pm_pi_sur2 ,-V1(1))
      code_retour%valeur = pm_warn_angle1_ou_3_indef
   end if

case default
   code_retour%valeur=pm_err_clef_rot
   go to 6000
end select

6000 continue

code_retour%routine = pm_num_mu_quat_3rot
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_quat_3rot
