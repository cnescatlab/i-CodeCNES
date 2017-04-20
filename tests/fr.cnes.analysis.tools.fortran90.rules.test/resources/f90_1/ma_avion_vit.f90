subroutine ma_avion_vit (pente_vit, azimut_vit, azimut_avion, assiette_avion, gite_avion, &
                         incidence, derapage, gite_vit, code_retour)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Position du triedre lie a la vitesse par rapport 
! ===   au triedre avion et calcul de la gite. 
!
! Note d'utilisation:  voir la documentation utilisateur
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                   (Date: 01/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 4.0 (DE 1): modification suite au transfert vers la MSLIB de 5 routines du theme U
!                         (Date: 11/2003 - Realisation: Veronique Lepine
!   + Version 5.5 : FA-ID 491 : Anomalie ma_avion_vit
!             (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_recale_angle

use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel) , intent(in) :: pente_vit      ! pente de la vitesse  
real(pm_reel) , intent(in) :: azimut_vit     ! azimut de la vitesse 
real(pm_reel) , intent(in) :: azimut_avion   ! azimut de l'avion (fuselage) 
real(pm_reel) , intent(in) :: assiette_avion ! assiette longitudinale de l'avion (fuselage) 
real(pm_reel) , intent(in) :: gite_avion     ! gite de l'avion (fuselage) 

real(pm_reel) , intent(out) :: incidence  ! angle d'incidence 
real(pm_reel) , intent(out) :: derapage   ! angle de derapage 
real(pm_reel) , intent(out) :: gite_vit   ! gite (rotation autour du vecteur vitesse)
type(tm_code_retour), intent(out) ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations pour calculs intermediaires
! ===============================================

real(pm_reel),dimension(3) :: dir_vecvit_terre, dir_vecvit_avion 

real(pm_reel)        :: alpha1, alpha2, alpha3   ! triplet d'angles associe au quaternion
real(pm_reel)        :: alpha, beta, moins_alpha ! angles d'incidence et de derapage
type(tm_quat)        :: quat_terre_vit,quat_terre_avion,quat_avion_vit ! quaternions intermediaires
real(pm_reel)        :: azimut_vit_alpha1,gitevit, produit
type(tm_code_retour) :: code_local

real(pm_reel),parameter :: epsilon_angle= 1.e-9_pm_reel ! environ 1 milliseconde d'arc
real(pm_reel),parameter :: un_moins_epsilon = (1._pm_reel - epsilon_angle)
real(pm_reel),parameter :: zero = 0._pm_reel

intrinsic cos,sin,atan,asin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO ma_avion_vit.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! Direction du vecteur vitesse dans le repere normal lie au sol et porte par l'avion
! ==================================================================================

dir_vecvit_terre(1)= cos(pente_vit)*cos(azimut_vit)
dir_vecvit_terre(2)= cos(pente_vit)*sin(azimut_vit)
dir_vecvit_terre(3)= -sin(pente_vit)

! Direction du vecteur vitesse dans le repere avion
! =================================================

! calcul du quaternion terre -> avion
call mu_3rot_quat(pm_1z_2y_3x,azimut_avion,assiette_avion,gite_avion,quat_terre_avion,code_local)
! pas de test necessaire sur le code retour

! calcul de la direction du vecteur vitesse dans le repere avion
call mu_quat_rep(dir_vecvit_terre,quat_terre_avion,dir_vecvit_avion,code_retour)

if (code_retour%valeur < pm_OK) go to 6000 
! pour se premunir d'eventuels problemes numeriques 
! (matrice non orthonormale suite aux calculs) 

! Calcul de l'incidence et du derapage
! ====================================

if (abs(dir_vecvit_avion(2)) >= un_moins_epsilon) then ! cos(beta) nul

   ! derapage vaut +/- pi/2
   code_retour%valeur= pm_err_vit_plan_sym_avion_ortho
   go to 6000

else ! cos(beta) non nul

   ! calcul de l'angle de derapage
   ! -----------------------------
   beta = asin(dir_vecvit_avion(2)) ! resultat dans ]-pi/2,+pi/2[

   ! calcul de l'angle d'incidence
   ! -----------------------------
   if (abs(dir_vecvit_avion(1)) < epsilon_angle) then ! incidence = +/- pi/2

      produit = dir_vecvit_avion(3)*cos(beta) ! si de memes signes > 0
                                                  ! sinon < 0

      alpha = sign(pm_pi_sur2,produit) ! si produit > 0 ==> incidence = +pi/2
                                       ! sinon                        = -pi/2

   else ! incidence differente de +/- pi/2

      alpha = atan(dir_vecvit_avion(3)/dir_vecvit_avion(1))

      ! L'angle d'incidence doit etre compris entre -pi et pi
      if(dir_vecvit_avion(1) < 0._pm_reel) then
         alpha = alpha +sign(pm_pi, dir_vecvit_avion(3))
      endif

   end if

end if

incidence = alpha  ! valeurs utilisees dans des calculs: 
derapage = beta    ! non compatible avec intent(out)

! Calcul du quaternion associe a la matrice de rotation terre -> vitesse
! ======================================================================

! calcul du quaternion terre -> avion : inutile deja fait plus haut

! calcul du quaternion avion -> vit
moins_alpha = - alpha
call mu_3rot_quat(pm_1y_2z_3x,moins_alpha,beta,zero,quat_avion_vit,code_local)
! pas de test necessaire sur le code retour

! calcul du quaternion terre -> vitesse
call mu_prod_quat (quat_terre_avion,quat_avion_vit, quat_terre_vit, code_local ) ! le code retour est toujours OK

! Calcul de la gite de la vitesse
! ===============================
! pour plus de details se reporter a la note algorithmique

! calcul du triplet (alpha1,alpha2,alpha3) associe au quaternion
call mu_quat_3rot ( pm_1z_2y_3x, quat_terre_vit, alpha1, alpha2, alpha3, code_local)

! on ramene l'entree azimut_vit entre [alpha1 - pi,alpha1 + pi[ pour comparaison
azimut_vit_alpha1= mui_recale_angle(azimut_vit,alpha1)

! determination si le triplet (alpha1,alpha2,alpha3) calcule est celui recherche
! ou s'il s'agit de (pi + alpha1,pi - alpha2,pi + alpha3)
! et calcul de la gite en fonction

if (abs(alpha1-azimut_vit_alpha1) > pm_pi_sur2) then
   gitevit = alpha3 + pm_pi
else
   gitevit = alpha3
end if

! recalage de la gite dans [-pi,+pi]
gite_vit = mui_recale_angle(gitevit,zero)

6000 continue

code_retour%routine = pm_num_ma_avion_vit
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine ma_avion_vit
