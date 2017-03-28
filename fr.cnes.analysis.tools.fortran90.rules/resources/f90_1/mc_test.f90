subroutine mc_test ( code_retour, eps_cir, eps_parab, eps_equa, eps_i_critique )

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Affectation des constantes physiques de test sur l'excentricite et l'inclinaison
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 209 ed01 rev00): creation a partir de la routine MCONST de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 400 ed01 rev00): ajout de la sortie eps_i_critique
!                         (Date: 12/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type(tm_code_retour), intent(out)        :: code_retour !code retour
                                                       
real(pm_reel), intent(out),optional      :: eps_cir        ! epsilon de test pour orbite circulaire
real(pm_reel), intent(out),optional      :: eps_parab      ! epsilon de test pour orbite parabolique
real(pm_reel), intent(out),optional      :: eps_equa       ! epsilon de test pour orbite equatoriale
real(pm_reel), intent(out),optional      :: eps_i_critique ! epsilon de test pour inclinaisons critiques

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mc_test.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mc_test.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation des valeurs des constantes demandees

if (present(eps_cir))        eps_cir        = pm_eps_cir         ! epsilon de test pour l'orbite circulaire
if (present(eps_parab))      eps_parab      = pm_eps_parab       ! epsilon de test pour l'orbite parabolique
if (present(eps_equa))       eps_equa       = pm_eps_equa        ! epsilon de test pour l'orbite equatoriale
if (present(eps_i_critique)) eps_i_critique = pm_eps_i_critique  ! epsilon de test pour l'inclinaison critique

code_retour%routine = pm_num_mc_test
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mc_test
