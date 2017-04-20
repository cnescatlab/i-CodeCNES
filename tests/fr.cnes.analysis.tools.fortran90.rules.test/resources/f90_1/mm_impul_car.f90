subroutine mm_impul_car (pos_car, vit_car_avant, rep, impul, vit_car_apres, code_retour)

! (C) Copyright CNES - MSLIB - 1999-2003

!************************************************************************
!
! But: Pour un satellite, calcul du bulletin d'orbite en coordonnee CARtesiennes suite a la realisation d'une manoeuvre orbitale modelisee par une mono-IMPULsion quelconque 
! ===
!
!$Historique
! ==========
!   + Version 2.0 (SP 337 ed01 rev00): creation a partir de la routine MMIMPS de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module indic_reperes_mslib par 
!     une sélection de int_manoeuvres
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_rep_orbitaux, only : mo_qsw_geo
use int_rep_orbitaux, only : mo_tnw_geo

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib
use int_manoeuvres, only : pm_rep_geo,pm_rep_qsw,pm_rep_tnw

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    :: pos_car        ! position du satellite
real(pm_reel), dimension(3), intent(in)    :: vit_car_avant  ! vitesse du satellite avant la manoeuvre
integer,                     intent(in)    :: rep            ! indicateur du repere dans lequel est 
real(pm_reel), dimension(3), intent(in)    :: impul          ! vecteur impulsion
real(pm_reel), dimension(3), intent(out)   :: vit_car_apres  ! vitesse du satellite apres la manoeuvre
type(tm_code_retour)       , intent(out)   :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel), dimension(3) :: rveco
type(tm_code_retour)        :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mm_impul_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mm_impul_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! calcul des composantes de l'impulsion dans le repere choisi
! ...........................................................

select case (rep)

case (pm_rep_geo)
   rveco(:) = impul(:)
   
case (pm_rep_qsw)                   ! vecteur impulsion donne dans le repere qsw
   call mo_qsw_geo ( pos_car, vit_car_avant, impul, rveco, code_retour_local )
   if (code_retour_local%valeur < 0) then
      code_retour%valeur = code_retour_local%valeur
      go to 6000
   else
      code_retour%valeur = code_retour_local%valeur
   end if

case (pm_rep_tnw)                   ! vecteur impulsion donne dans le repere tnw
   call mo_tnw_geo( pos_car, vit_car_avant, impul, rveco, code_retour_local )
   if (code_retour_local%valeur < 0) then
      code_retour%valeur = code_retour_local%valeur
      go to 6000
   else
      code_retour%valeur = code_retour_local%valeur
   end if

case default                        ! repere non prevu 
   code_retour%valeur = pm_err_ind_rep
   go to 6000

end select

! calcul des positions vitesses apres impulsion
! .............................................

vit_car_apres(:) = vit_car_avant(:) + rveco(:)

6000 continue

code_retour%routine = pm_num_mm_impul_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mm_impul_car
