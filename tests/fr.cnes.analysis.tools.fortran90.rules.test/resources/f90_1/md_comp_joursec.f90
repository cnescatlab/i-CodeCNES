subroutine md_comp_joursec (joursec1, joursec2, eps, ordre, code_retour)
! (C) Copyright CNES - MSLIB - 1999
!************************************************************************
!
! But:  comparaison de deux quantites exprimees en jours et secondes.
! ===   
!
!$Historique
! ==========
!   + Version 2.0 (SP 333 ed01 rev00): creation a partir de la routine MUCOMPJJS de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 (option) : remplacement du module
!                   indic_comp_joursec_mslib par un appel à int_dates avec sélection
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

use int_dates_internes, only : mdi_som_diff_joursec
use int_dates, only : md_joursec_jourfrac, &
     pm_joursec1_sup_joursec2, &
     pm_joursec1_egal_joursec2,&
     pm_joursec1_inf_joursec2

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),    intent(in)        :: joursec1         ! quantite #1 (en jours, sec) 
type(tm_jour_sec),    intent(in)        :: joursec2         ! quantite #2 (en jours, sec) 
real(pm_reel),        intent(in)        :: eps              ! epsilon de comparaison (en s)
integer,              intent(out)       :: ordre            ! resultat de la comparaison : pm_joursec1_sup_joursec2 si joursec1 > joursec2
type(tm_code_retour), intent(out)       :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!declaration des variables locales
!---------------------------------
type(tm_jour_sec)    :: joursec3                       ! quantite (jours, sec) tampon
integer              :: code_inter                     ! code retour des routines intermediaires
type(tm_code_retour) :: code_retour_local              ! code retour des routines intermediaires

real(pm_reel)        :: jourfrac3, presque_zero        ! variables intermediaires pour les tests

intrinsic tiny

!declaration des initialisations
!-------------------------------
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB md_comp_joursec.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: md_comp_joursec.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour 
! ..........................................
code_retour%valeur = pm_OK

! initialisation constantes de test
! .................................
presque_zero = tiny(1._pm_reel)             ! recherche du plus petit reel positif non nul

! test pour savoir si eps > 0
! ...........................
if (eps < 0._pm_reel) then
   code_retour%valeur = pm_err_eps_negatif
   go to 6000
end if

if (eps <= presque_zero) then
   code_retour%valeur = pm_err_eps_nul
   go to 6000
end if

! calcul de la soustraction des 2 quantitees
! ..........................................
call mdi_som_diff_joursec (joursec1, joursec2, code_inter, joursec_diff=joursec3)
if (code_inter < 0) then
   code_retour%valeur = code_inter
   go to 6000
else
   code_retour%valeur = code_inter
end if

call md_joursec_jourfrac(joursec3, jourfrac3, code_retour_local)
if (code_retour_local%valeur < 0) then
   code_retour%valeur = code_retour_local%valeur
   go to 6000
end if

! test pour savoir si la premiere quantitee est plus grande que la deuxieme
! .........................................................................
if ((jourfrac3*86400._pm_reel) > eps) then
   ordre = pm_joursec1_sup_joursec2
   go to 6000
end if

! test pour savoir si la premiere quantitee est egale a la deuxieme
! .................................................................
if (abs(jourfrac3*86400._pm_reel) <= eps) then
   ordre = pm_joursec1_egal_joursec2
   go to 6000
end if

! test pour savoir si la premiere quantitee est plus petite que la deuxieme
! .........................................................................
if ((jourfrac3*86400._pm_reel) < (- eps)) then
   ordre = pm_joursec1_inf_joursec2
   go to 6000
end if

6000 continue

code_retour%routine = pm_num_md_comp_joursec
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_comp_joursec
