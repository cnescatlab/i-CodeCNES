subroutine mm_impul_kep (mu, kep_avant, rep, impul, kep_apres, code_retour)

! (C) Copyright CNES - MSLIB - 1999-2003

!************************************************************************
!
! But: Pour un satellite, calcul du bulletin d'orbite en parametres KEPleriens apres realisation d'une manoeuvre orbitale modelisee par une mono-IMPULsion quelconque
! ===
!
!$Historique
! ==========
!   + Version 2.0 (SP 338 ed01 rev00): creation a partir de la routine MMIMPS de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use int_manoeuvres, only : mm_impul_car
use int_chgmnt_variables, only : mv_kep_car
use int_chgmnt_variables, only : mv_car_kep

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)  :: mu           ! constante de la gravitation
type(tm_orb_kep),                   intent(in)  :: kep_avant    ! parametres kepleriens avant la manoeuvre
integer,                            intent(in)  :: rep          ! indicateur du repere dans lequel est 
real(pm_reel), dimension(3),        intent(in)  :: impul        ! vecteur impulsion
type(tm_orb_kep),                   intent(out) :: kep_apres    ! parametres kepleriens apres la manoeuvre
type(tm_code_retour),               intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel), dimension(3) :: pos_car_local                ! position en repere inertiel du satellite
real(pm_reel), dimension(3) :: vit_car_local_av             ! vitesse en repere inertiel du satellite avant manoeuvre
real(pm_reel), dimension(3) :: vit_car_local_ap             ! vitesse en repere inertiel du satellite apres manoeuvre
type(tm_code_retour)        :: code_retour_local            ! code retour des routines appellees

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mm_impul_kep.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mm_impul_kep.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK
code_retour_local%valeur = pm_OK

! passage du bulletin (a,e,i,pom,gom,M) aux positions vitesses
! ............................................................

call mv_kep_car ( mu, kep_avant, pos_car_local, vit_car_local_av, code_retour_local )
if (code_retour_local%valeur /= 0) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < 0) then
      go to 6000
   end if
end if

! incrementation de l'impulsion
! .............................

call mm_impul_car ( pos_car_local, vit_car_local_av, rep, impul, vit_car_local_ap, code_retour_local )
if (code_retour_local%valeur /= 0) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < 0) then
      go to 6000
   end if
end if

! calcul du bulletin apres impulsion
! ..................................
call mv_car_kep ( mu, pos_car_local, vit_car_local_ap, kep_apres, code_retour_local )
if (code_retour_local%valeur /= 0) then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < 0) then
      go to 6000
   end if
end if

6000 continue

code_retour%routine = pm_num_mm_impul_kep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mm_impul_kep
