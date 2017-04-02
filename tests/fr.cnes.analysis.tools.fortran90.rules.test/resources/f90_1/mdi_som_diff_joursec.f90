subroutine mdi_som_diff_joursec (joursec1, joursec2, retour, joursec_somme, joursec_diff)
! (C) Copyright CNES - MSLIB - 1999-2003

!************************************************************************
!
! But:  Addition ou soustraction de deux quantites exprimees en jours et secondes.
! ===   
!
! Note d'utilisation:  -  Routine interne 
! ==================
!
!$Historique
! ==========
!   + Version 2.0 (SP 334 ed01 rev00): creation a partir de la routine MUADDURJJS de la MSLIB f77
!                         (Date:07/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)!
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
use int_dates, only : md_joursec_norme

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),    intent(in)            :: joursec1         ! quantite exprimee en jours et secondes
type(tm_jour_sec),    intent(in)            :: joursec2         ! quantite exprimee en jours et secondes
integer          ,    intent(out)           :: retour           ! code retour de la routine
type(tm_jour_sec),    intent(out), optional :: joursec_somme    ! quantite, somme des deux quantites en entree, exprimee en jours et secondes dans le jour
type(tm_jour_sec),    intent(out), optional :: joursec_diff     ! quantite, difference des deux quantites en entree (joursec1-joursec2), exprimee en jours et secondes dans le jour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
type(tm_jour_sec)    :: joursecloc1, joursecloc2  ! quantite jours et secondes pour les calculs intermediaires
type(tm_code_retour) :: code_inter                ! code retour intermediaire

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mdi_som_diff_joursec.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mdi_som_diff_joursec.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! calcul selon que la somme ou/et la difference est/sont demandee(s)
! ..................................................................
if (present(joursec_somme)) then

   joursecloc1%jour = joursec1%jour + joursec2%jour
   joursecloc1%sec  = joursec1%sec  + joursec2%sec

   call md_joursec_norme (joursecloc1, joursec_somme, code_inter)

   retour = code_inter%valeur

   if (code_inter%valeur < pm_OK) then
      go to 6000
   end if

end if

if (present(joursec_diff)) then

   joursecloc2%jour = joursec1%jour - joursec2%jour
   joursecloc2%sec  = joursec1%sec  - joursec2%sec

   call md_joursec_norme (joursecloc2, joursec_diff, code_inter)

   retour = code_inter%valeur

   if (code_inter%valeur < pm_OK) then
      go to 6000
   end if

end if

6000 continue

end subroutine mdi_som_diff_joursec 
