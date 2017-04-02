subroutine mc_GRS1980 ( code_retour, r_equa, inv_apla, apla)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Acces aux constantes relatives au systeme geodesique de reference 1980 (= GRS80)
! ===

!$Historique
! ==========
!   + Version 1.0 (SP 214 ed01 rev00): creation a partir de la routine MCRPOT de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module GRS1980_mslib par 
!     une sélection de int_constantes
!   + Version 6.9 : DM-ID 1058 : Suppression de warning G95
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

use int_constantes, only : pm_r_equa_GRS1980,pm_inv_apla_GRS1980,pm_apla_GRS1980

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_code_retour), intent(out)        :: code_retour     !code retour
real(pm_reel), intent(out), optional     :: r_equa          ! rayon de la Terre (m)
real(pm_reel), intent(out), optional     :: inv_apla        ! inverse de l'aplatissement terrestre
real(pm_reel), intent(out), optional     :: apla            ! aplatissement terrestre

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mc_GRS1980.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mc_GRS1980.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation des valeurs des constantes demandees

if (present(r_equa))   r_equa   = pm_r_equa_GRS1980
if (present(inv_apla)) inv_apla = pm_inv_apla_GRS1980
if (present(apla))     apla     = pm_apla_GRS1980

code_retour%routine = pm_num_mc_GRS1980
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mc_GRS1980
