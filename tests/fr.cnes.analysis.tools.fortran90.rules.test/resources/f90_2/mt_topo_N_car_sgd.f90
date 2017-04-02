subroutine mt_topo_N_car_sgd(pos_car, pos_sgd, code_retour, vit_car, vit_sgd, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  dans un repere TOPOcentrique Nord (convention axe Ox vers le Nord),
! ===   passage des coordonnees CARtesiennes aux 
!       coordonnees Site/Gisement/Distance
!
!$Historique
! ==========
!   + Version 1.0 (SP 261 ed01 rev00): creation a partir de la routine MVTSGD de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use int_utilitaires, only : mu_angle2

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              :: pos_car  ! position dans le repere topo Nord en cartesien
type(tm_sgd), intent(out)                            :: pos_sgd  ! position dans le repere topo Nord en sgd
type(tm_code_retour), intent(out)                    :: code_retour

real(pm_reel), dimension(3), intent(in), optional    :: vit_car  ! vitesse dans le repere topo Nord en cartesien
type(tm_sgd), intent(out), optional                  :: vit_sgd  ! vitesse dans le repere topo Nord en sgd
real(pm_reel), dimension(6,6), intent(out), optional :: jacob    ! jacobienne de la transfomation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer       :: clef         ! clef pour les calculs optionnels

real(pm_reel) :: eps100       ! valeur de test pour les reels
                
real(pm_reel) :: dist_xy,dist2_xy,x,y,distance,sin_site,angle  ! variables intermediaires pour calcul des positions

real(pm_reel) :: prod_scal,z_vit_d_sur_d,prod_vect_z  ! variables intermediaires pour calcul des vitesses
real(pm_reel) :: vit_site,vit_gisement,vit_distance   ! variables intermediaires pour calcul des vitesses

real(pm_reel) :: d2,z_sur_d,z_sur_d2,z_vit_d_sur_d3                              ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: var1,var2,var3,var4                                             ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: ds_sur_dx,ds_sur_dy,ds_sur_dz,dg_sur_dx,dg_sur_dy               ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: dd_sur_dx,dd_sur_dy,dvit_d_sur_dx,dvit_d_sur_dy,dvit_d_sur_dz   ! variables intermediaires pour calcul du jacobien

intrinsic abs, asin, sign, sqrt, dot_product, epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_topo_N_car_sgd.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_topo_N_car_sgd.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Traitement des parametres optionnels
! ....................................

clef = 0   ! seul le calcul des positions sera fait avec cette valeur de la clef

! Choix des clefs:
!  - si calcul des vitesses sgd (sans jacobien) : clef = 1
!  - si calcul du jacobien (sans vitesse sgd)   : clef = 2
!  - si calcul des vitesses sgd et du jacobien  : clef = 3

if (present(vit_sgd)) then ! pour le calcul de la vitesse sgd: il faut la vitesse cartesienne en entree
   if (.not. present(vit_car)) then
      code_retour%valeur = pm_err_para_option
      go to 6000
   else
      clef = 1  ! affectation pour le calcul des vitesses
   end if
end if

if (present(jacob)) then  ! pour le calcul du jacobien: il faut la vitesse cartesienne en entree

   if (.not. present(vit_car)) then
      code_retour%valeur = pm_err_para_option
      go to 6000
   else
      clef = clef + 2  ! affectation pour le calcul du jacobien
   end if

else ! le calcul du jacobien n'est pas demande

   ! entrees/sorties optionnelles incoherentes
   if (present(vit_car) .and. .not.present(vit_sgd)) code_retour%valeur = pm_warn_para_option 

end if

   
! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

! Calcul des positions site/gisement/distance
! =============================================

dist2_xy = pos_car(1)*pos_car(1) + pos_car(2)*pos_car(2)
dist_xy = sqrt(dist2_xy)

if (dist_xy < eps100) then     ! position sur l'axe Oz

   if (abs(pos_car(3)) < eps100) then  ! position confondue avec origine repere 
      code_retour%valeur = pm_err_pos_orig_topo
   else

      ! calcul site et distance uniquement et mise a zero arbitraire du gisement
      pos_sgd%s = sign(pm_pi_sur2,pos_car(3))
      pos_sgd%d = abs(pos_car(3))
      pos_sgd%g = 0._pm_reel
      code_retour%valeur = pm_warn_pos_Oz_topo

      if (clef /= 0) then
         code_retour%valeur = pm_err_pos_Oz_topo
      end if

   end if

   go to 6000

end if

distance = sqrt(dist2_xy + pos_car(3)*pos_car(3))
pos_sgd%d = distance
sin_site = pos_car(3) / distance
pos_sgd%s = asin(sin_site)
x = pos_car(1)
y = pos_car(2)
call mu_angle2(x,y,angle,code_retour) ! pas de code retour a tester car entrees non nulles en meme temps
pos_sgd%g = angle

! Calcul des vitesses site/gisement/distance
! =============================================

if (clef < 1) go to 6000  ! pas de calculs de vitesse ou jacobien

! pour le jacobien des calculs lies a la vitesse sgd sont necessaires ....

!     en distance :
!     -------------
prod_scal = dot_product(pos_car,vit_car)
vit_distance = prod_scal / distance

!     en site :
!     ---------
z_vit_d_sur_d = pos_car(3) * (vit_distance/distance)
vit_site = (vit_car(3) - z_vit_d_sur_d) / dist_xy

!     en gisement :
!     -------------
prod_vect_z = pos_car(1)*vit_car(2) - pos_car(2)*vit_car(1) ! = - composante sur Z du produit vectoriel position-vitesse
vit_gisement = prod_vect_z / dist2_xy

if ((clef == 1) .OR. (clef == 3)) then ! affectation de la vitesse sgd en sortie si demandee
   vit_sgd%d = vit_distance
   vit_sgd%s = vit_site
   vit_sgd%g = vit_gisement
end if 

! Calcul du jacobien : calcul des derivees partielles des vecteurs position et vitesse
! ====================================================================================

if (clef < 2) go to 6000  ! pas de calcul du jacobien

jacob(:,:) = 0._pm_reel   ! mise a zero de la jacobienne

d2             = distance * distance
z_sur_d        = pos_car(3) / distance
z_sur_d2       = pos_car(3) / d2
z_vit_d_sur_d3 = z_sur_d2 * vit_distance / distance

!     derivees partielles de s :
!     --------------------------
var1 = - z_sur_d2 / dist_xy

ds_sur_dx  = pos_car(1) * var1
jacob(1,1) = ds_sur_dx

ds_sur_dy  = pos_car(2) * var1
jacob(1,2) = ds_sur_dy

ds_sur_dz  = dist_xy / d2
jacob(1,3) = ds_sur_dz

!     derivees partielles de g :
!     --------------------------
dg_sur_dx  =  -pos_car(2) / dist2_xy
jacob(2,1) =  dg_sur_dx

dg_sur_dy  = pos_car(1) / dist2_xy
jacob(2,2) = dg_sur_dy

!     derivees partielles de d :
!     --------------------------
dd_sur_dx  = pos_car(1) / distance
jacob(3,1) = dd_sur_dx

dd_sur_dy  = pos_car(2) / distance
jacob(3,2) = dd_sur_dy

jacob(3,3) = z_sur_d

!     derivees partielles de la vitesse sur d :
!     -----------------------------------------
var4 = vit_distance / distance

dvit_d_sur_dx = (vit_car(1) - pos_car(1)*var4) / distance
jacob(6,1)    = dvit_d_sur_dx

dvit_d_sur_dy = (vit_car(2) - pos_car(2)*var4) / distance
jacob(6,2)    = dvit_d_sur_dy

dvit_d_sur_dz = (vit_car(3) - pos_car(3)*var4) / distance
jacob(6,3)    = dvit_d_sur_dz

jacob(6,4) = dd_sur_dx
jacob(6,5) = dd_sur_dy
jacob(6,6) = z_sur_d

!     derivees partielles de la vitesse sur s :
!     -----------------------------------------
var2 = z_vit_d_sur_d3 - (vit_site/dist_xy)
jacob(4,1) = (pos_car(1)*var2 - z_sur_d*dvit_d_sur_dx) / dist_xy
jacob(4,2) = (pos_car(2)*var2 - z_sur_d*dvit_d_sur_dy) / dist_xy
jacob(4,3) = (z_vit_d_sur_d3*pos_car(3) - z_sur_d*dvit_d_sur_dz - vit_distance/distance) / dist_xy
jacob(4,4) = ds_sur_dx
jacob(4,5) = ds_sur_dy
jacob(4,6) = ds_sur_dz

!     derivees partielles de la vitesse sur g :
!     -----------------------------------------
var3 = 2._pm_reel * vit_gisement
jacob(5,1) =   (vit_car(2) - pos_car(1)*var3) /  dist2_xy
jacob(5,2) =  -(vit_car(1) + pos_car(2)*var3) / dist2_xy
jacob(5,4) = dg_sur_dx
jacob(5,5) = dg_sur_dy

6000 continue

code_retour%routine = pm_num_mt_topo_N_car_sgd
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_topo_N_car_sgd
