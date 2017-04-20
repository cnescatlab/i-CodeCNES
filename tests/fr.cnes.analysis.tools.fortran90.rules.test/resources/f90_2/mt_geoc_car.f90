subroutine mt_geoc_car (pos_geoc, pos_car, code_retour, vit_geoc, vit_car, jacob)

! (C) Copyright CNES - MSLIB - 1998-2002

!************************************************************************
!
! But:  Passage des coordonnees GEOCentriques aux coordonnees CARtesiennes
! ===
!
! Note d'utilisation:  La transformation inverse peut s'effectuer par mt_car_geoc.
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 250 ed01 rev00): creation a partir de la routine MVGCCA de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (DE 463 ed01 rev00) : ajout de la transformation des vitesses, et de la jacobienne (optionnelles)
!                         (Date: 10/2002 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : (Date : 02/05/2007 - Realisation: Sandrine Avril - Atos origin)
!                   DM-ID 636 : contrôle de la latitude pour la fonction mt_geoc_car
!                   DM-ID 616 remplacement du module math_mslib
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

use int_constantes, only : pm_pi_sur2, pm_deux_pi 
! Declarations
! ============
implicit none

intrinsic present,cos, sin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_geocentrique), intent(in)                    ::  pos_geoc     ! latitude, longitude, distance geocentriques             
real(pm_reel), dimension(3), intent(out)             ::  pos_car      ! position en coordonnees cartesiennes
type(tm_code_retour), intent(out)                    ::  code_retour

type(tm_geocentrique), intent(in), optional          ::  vit_geoc     ! vitesse en geocentrique
real(pm_reel), dimension(3), intent(out), optional   ::  vit_car      ! vitesse en cartesien
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob        ! jacobienne de la transfomation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel), dimension(3)  :: vitcar     !     variable intermediaire contenant la vitesse cartesienne
real(pm_reel), dimension(3)  :: poscar     !     variable intermediaire contenant la position cartesienne
real(pm_reel)  ::  rc1c2,rc1s2,rs1s2,rs1c2 !     variables intermediaires pour le calcul du jacobien
real(pm_reel)  ::  rcos1,rsin1,rcos2,rsin2 !     variables intermediaires
 

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_geoc_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_geoc_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! tests sur les paramètres initiaux
! .................................

!  test sur la latitude : doit appartenir à l'intervalle [-pi/2,pi/2]
if ((pos_geoc%lat < -pm_pi_sur2) .or. (pos_geoc%lat > pm_pi_sur2)) then 
   code_retour%valeur = pm_err_mlat_sup_pisur2
   goto 6000
end if

!  test sur la longitude : doit appartenie à l'intertvalle [0,2pi[
if ((pos_geoc%long < 0._pm_reel) .or. (pos_geoc%long > pm_deux_pi)) then
   code_retour%valeur = pm_err_long_interval_0_2pi
   goto 6000
end if

! ---------------------
! calcul intermediaires 
! ---------------------

rcos1 = cos(pos_geoc%lat)
rsin1 = sin(pos_geoc%lat)
rcos2 = cos(pos_geoc%long)
rsin2 = sin(pos_geoc%long)

rc1c2 = rcos1 * rcos2
rs1s2 = rsin1 * rsin2
rs1c2 = rsin1 * rcos2
rc1s2 = rcos1 * rsin2

! ------------------------------------------------------------
! calcul des coordonnees cartesiennes 
! ------------------------------------------------------------

!     notations: x=pos_car(1), y=pos_car(2), z=pos_car(3)
!                lat=pos_geoc%lat, long=pos_geoc%long, d=pos_geoc%dist
!     relations:
!                x = d.cos(lat).cos(long)
!                y = d.cos(lat).sin(long)
!                z = d.sin(lat)

         poscar(1) = pos_geoc%dist * rc1c2                     !     axe des x 
         poscar(2) = pos_geoc%dist * rc1s2                     !     axe des y 
         poscar(3) = pos_geoc%dist * rsin1                     !     axe des z 

pos_car(:) = poscar(:)

!----------------------
! calcul optionnels 
!----------------------

if ((present(vit_car) .and. .not.present(vit_geoc)) .or.(present(jacob).and. .not.present(vit_geoc))) then  ! mauvais parametrage
   code_retour%valeur = pm_err_para_option
   go to 6000
end if

if ( present(vit_geoc).and. (.not.present(vit_car).and..not.present(jacob)))  code_retour%valeur = pm_warn_para_option 
                                                                           ! parametrage optionnel incoherent

if (present(vit_car).or.present(jacob)) then  !  calcul vecteur vitesse en coordonnees cartesiennes

   vitcar(1) = vit_geoc%dist * rc1c2 - poscar(3) * rcos2 * vit_geoc%lat- poscar(2) * vit_geoc%long      !     axe des x 
   vitcar(2) = vit_geoc%dist * rc1s2 - poscar(3) * rsin2 * vit_geoc%lat+ poscar(1) * vit_geoc%long      !     axe des y 
   vitcar(3) = vit_geoc%dist * rsin1 + pos_geoc%dist * rcos1 * vit_geoc%lat                               !     axe des z 

end if

if (present(jacob)) then            ! calcul du jacobien 

   jacob(:,:) = 0._pm_reel

   !     derivee partielle de x :
   !     ------------------------
   jacob(1,1) = - pos_geoc%dist * rs1c2
   jacob(1,2) = - poscar(2)
   jacob(1,3) = rc1c2

   !     derivee partielle de y :
   !     ------------------------
   jacob(2,1) = - pos_geoc%dist * rs1s2
   jacob(2,2) = poscar(1)
   jacob(2,3) = rc1s2

   !     derivee partielle de z :
   !     ------------------------
   jacob(3,1) = pos_geoc%dist * rcos1
   jacob(3,3) = rsin1

   !     derivee partielle de vx :
   !     ------------------------
   jacob(4,1) = - vitcar(3) * rcos2 + poscar(3) * rsin2 *vit_geoc%long
   jacob(4,2) = - vitcar(2) 
   jacob(4,3) = - rs1c2 * vit_geoc%lat - rc1s2 * vit_geoc%long
   jacob(4,4) = - pos_geoc%dist * rs1c2
   jacob(4,5) = - poscar(2)
   jacob(4,6) = rc1c2

   !     derivee partielle de vy :
   !     ------------------------
   jacob(5,1) = - vitcar(3) * rsin2 - poscar(3) * rcos2 * vit_geoc%long
   jacob(5,2) =   vitcar(1) 
   jacob(5,3) =   rc1c2 * vit_geoc%long - rs1s2 * vit_geoc%lat
   jacob(5,4) = - pos_geoc%dist * rs1s2 
   jacob(5,5) =   poscar(1)
   jacob(5,6) =   rc1s2

   !     derivee partielle de vz :
   !     ------------------------
   jacob(6,1) = vit_geoc%dist * rcos1 - poscar(3) * vit_geoc%lat
   jacob(6,3) = rcos1 * vit_geoc%lat
   jacob(6,4) = pos_geoc%dist * rcos1
   jacob(6,6) = rsin1

end if

if (present(vit_car)) vit_car(:) = vitcar(:)

6000 continue

code_retour%routine = pm_num_mt_geoc_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_geoc_car
