subroutine mt_car_geoc (pos_car, pos_geoc, code_retour, vit_car, vit_geoc, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des coordonnees CARtesiennes aux coordonnees GEOCentriques.
! ===
!
! Note d'utilisation: La transformation inverse peur s'effectuer par mt_geoc_car. 
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 249 ed01 rev00): creation a partir de la routine MVCAGC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 402 ed01 rev00): ajout de l'utilisation du code retour pm_warn_pos_Oz_ref
!                         (Date: 01/2000 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (DE 462 ed01 rev00) : ajout de la transformation des vitesses, et de la jacobienne (optionnelles)
!                         (Date: 10/2002 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_angle2
use int_util_internes, only : mui_dot_product3

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  pos_car  ! coordonnees cartesiennes dans le repere de reference
type(tm_geocentrique), intent(out)                   ::  pos_geoc ! latitude, longitude et distance geocentriques 
type(tm_code_retour), intent(out)                    ::  code_retour

real(pm_reel), dimension(3), intent(in), optional    ::  vit_car  ! vitesse cartesienne dans le repere de reference
type(tm_geocentrique), intent(out), optional         ::  vit_geoc ! vitesse en geocentrique
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob    ! jacobienne de la transfomation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel)        :: eps100              !     epsilon de test pour les reels
real(pm_reel)        :: zsurd               !     variable intermediaire
real(pm_reel)        :: distance_terre      !     distance centre Terre
type(tm_code_retour) :: code_retour_interne ! code retour intermediaire
real (pm_reel)       :: retour

integer       :: clef                ! clef pour les calculs optionnels
real(pm_reel) :: dist_xy,dist2_xy                     ! variables intermediaires pour calcul des vitesses
real(pm_reel) :: prod_scal,z_vit_d_sur_d,prod_vect_z  ! variables intermediaires pour calcul des vitesses
real(pm_reel) :: vit_lat,vit_long,vit_distance        ! variables intermediaires pour calcul des vitesses
real(pm_reel) :: d2,z_sur_d,z_sur_d2,z_vit_d_sur_d3                              ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: var1,var2,var3,var4                                             ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: dlat_sur_dx,dlat_sur_dy,dlat_sur_dz,dlong_sur_dx,dlong_sur_dy   ! variables intermediaires pour calcul du jacobien
real(pm_reel) :: dd_sur_dx,dd_sur_dy,dvit_d_sur_dx,dvit_d_sur_dy,dvit_d_sur_dz   ! variables intermediaires pour calcul du jacobien

intrinsic  asin, sqrt, epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_car_geoc.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_car_geoc.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Traitement des parametres optionnels
! ....................................

clef = 0   ! seul le calcul des positions sera fait avec cette valeur de la clef

! Choix des clefs:
!  - si calcul des vitesses geocentriques (sans jacobien) : clef = 1
!  - si calcul du jacobien (sans vitesses geocentriques)  : clef = 2
!  - si calcul des vitesses geocentriques et du jacobien  : clef = 3

if (present(vit_geoc)) then ! pour le calcul de la vitesse geoc: il faut la vitesse cartesienne en entree
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
   if (present(vit_car) .and. .not.present(vit_geoc)) code_retour%valeur = pm_warn_para_option 

end if

! initialisation de la valeur de l'epsilon de test pour les reels
! ...............................................................
eps100 = 100._pm_reel * epsilon(1._pm_reel)

!------------------------------------------------------------------
! calcul des coordonnees (latitude,longitude,distance) geocentriques
!------------------------------------------------------------------

!     notations: x=pos_car(1), y=pos_car(2), z=pos_car(3)
!                lat=pos_geoc%lat, long=pos_geoc%long, d=pos_geoc%dist
!     relations:
!                x = d.cos(lat).cos(long)
!                y = d.cos(lat).sin(long)
!                z = d.sin(lat)

call mu_norme (pos_car, distance_terre, code_retour_interne) ! calcul distance centre terre

if (distance_terre < eps100) then                            ! vecteur position nul.
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

!     distance
!     -------
pos_geoc%dist = distance_terre

!     latitude
!     --------
zsurd = pos_car(3)/distance_terre
pos_geoc%lat = asin(zsurd)                                                 ! calcul de la latitude dans [-pi/2,+pi/2]

!     longitude
!     ---------
call mu_angle2(pos_car(1), pos_car(2), pos_geoc%long, code_retour_interne) ! calcul de la longitude dans [0,2.pi[

if (code_retour_interne%valeur == pm_err_vect_nul) then                    ! on est au pole, la longitude est donc indefinie 
   code_retour%valeur = pm_warn_pos_Oz_ref                                 ! on lui affecte la valeur 0
   pos_geoc%long      = 0._pm_reel

   if (clef>=1)  code_retour%valeur = pm_err_pos_Oz_ref                    ! calcul des vitesses impossible
  
   go to 6000

end if
                                                                  
! Calcul des vitesses geocentriques
! =================================

if (clef < 1) go to 6000  ! pas de calculs de vitesse ou jacobien

! pour le jacobien des calculs lies a la vitesse geoc sont necessaires ....

!     en distance :
!     -------------

call mui_dot_product3 ( pos_car , vit_car , prod_scal , retour )

vit_distance = prod_scal / distance_terre

!     en latitude :
!     -------------
z_vit_d_sur_d = pos_car(3) * (vit_distance/distance_terre)
dist2_xy = pos_car(1)*pos_car(1) + pos_car(2)*pos_car(2)
dist_xy = sqrt(dist2_xy)
vit_lat = (vit_car(3) - z_vit_d_sur_d) / dist_xy

!     en longitude :
!     -------------
prod_vect_z = pos_car(1)*vit_car(2) - pos_car(2)*vit_car(1) ! = - composante sur Z du produit vectoriel position-vitesse
vit_long = prod_vect_z / dist2_xy

if ((clef == 1) .OR. (clef == 3)) then ! affectation de la vitesse geocentrique en sortie si demandee
   vit_geoc%dist = vit_distance
   vit_geoc%lat = vit_lat
   vit_geoc%long = vit_long
end if 

! Calcul du jacobien : calcul des derivees partielles des vecteurs position et vitesse
! ====================================================================================

if (clef < 2) go to 6000  ! pas de calcul du jacobien

jacob(:,:) = 0._pm_reel   ! mise a zero de la jacobienne

d2             = distance_terre * distance_terre
z_sur_d        = pos_car(3) / distance_terre
z_sur_d2       = pos_car(3) / d2
z_vit_d_sur_d3 = z_sur_d2 * vit_distance / distance_terre

!     derivees partielles de la latitude :
!     ------------------------------------
var1 = - z_sur_d2 / dist_xy

dlat_sur_dx  = pos_car(1) * var1
jacob(1,1) = dlat_sur_dx

dlat_sur_dy  = pos_car(2) * var1
jacob(1,2) = dlat_sur_dy

dlat_sur_dz  =  dist_xy / d2
jacob(1,3) = dlat_sur_dz

!     derivees partielles de la longitude :
!     -------------------------------------
dlong_sur_dx  =  -pos_car(2) / dist2_xy
jacob(2,1) =  dlong_sur_dx

dlong_sur_dy  = pos_car(1) / dist2_xy
jacob(2,2) = dlong_sur_dy

!     derivees partielles de d :
!     --------------------------
dd_sur_dx  = pos_car(1) / distance_terre
jacob(3,1) = dd_sur_dx

dd_sur_dy  = pos_car(2) / distance_terre
jacob(3,2) = dd_sur_dy

jacob(3,3) = z_sur_d

!     derivees partielles de la vitesse sur d :
!     -----------------------------------------
var4 = vit_distance / distance_terre

dvit_d_sur_dx = (vit_car(1) - pos_car(1)*var4) / distance_terre
jacob(6,1)    = dvit_d_sur_dx

dvit_d_sur_dy = (vit_car(2) - pos_car(2)*var4) / distance_terre
jacob(6,2)    = dvit_d_sur_dy

dvit_d_sur_dz = (vit_car(3) - pos_car(3)*var4) / distance_terre
jacob(6,3)    = dvit_d_sur_dz

jacob(6,4) = dd_sur_dx
jacob(6,5) = dd_sur_dy
jacob(6,6) = z_sur_d

!     derivees partielles de la vitesse sur la latitude :
!     ---------------------------------------------------
var2 = z_vit_d_sur_d3 - (vit_lat/dist_xy)
jacob(4,1) = (pos_car(1)*var2 - z_sur_d*dvit_d_sur_dx) / dist_xy
jacob(4,2) = (pos_car(2)*var2 - z_sur_d*dvit_d_sur_dy) / dist_xy
jacob(4,3) = (z_vit_d_sur_d3*pos_car(3) - z_sur_d*dvit_d_sur_dz - vit_distance/distance_terre) / dist_xy
jacob(4,4) = dlat_sur_dx
jacob(4,5) = dlat_sur_dy
jacob(4,6) = dlat_sur_dz

!     derivees partielles de la vitesse sur la longitude :
!     ----------------------------------------------------
var3 = 2._pm_reel * vit_long
jacob(5,1) =   (vit_car(2) - pos_car(1)*var3) /  dist2_xy
jacob(5,2) =  -(vit_car(1) + pos_car(2)*var3) / dist2_xy
jacob(5,4) = dlong_sur_dx
jacob(5,5) = dlong_sur_dy

6000 continue

code_retour%routine = pm_num_mt_car_geoc
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_car_geoc
