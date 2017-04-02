subroutine mc_phys ( code_retour, ua, vit_lum, i_critique_non_retro, i_critique_retro )

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Acces aux constantes physiques
! ===

!$Historique
! ==========
!   + Version 1.0 (SP 211 ed.1 rev00): creation a partir de la routine MCONST de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 399 ed.1 rev00): ajout des sorties i_critique_non_retro et i_critique_retro
!                         (Date: 12/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module phys_mslib par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppressions des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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

use int_constantes, only :pm_ua, pm_vit_lum, &
     pm_i_critique_non_retro,pm_i_critique_retro, &
     pm_obliquite2000, &
     pm_mercure_r_equa_UAI, pm_mercure_apla_UAI, &
     pm_venus_r_equa_UAI, pm_venus_apla_UAI, &
     pm_terre_r_equa_UAI, pm_terre_apla_UAI, &
     pm_mars_r_equa_UAI, pm_mars_apla_UAI, &
     pm_jupiter_r_equa_UAI, pm_jupiter_apla_UAI, &
     pm_saturne_r_equa_UAI, pm_saturne_apla_UAI, &
     pm_uranus_r_equa_UAI, pm_uranus_apla_UAI, &
     pm_neptune_r_equa_UAI, pm_neptune_apla_UAI, &
     pm_pluton_r_equa_UAI, pm_pluton_apla_UAI

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
type(tm_code_retour), intent(out)        :: code_retour          ! code retour
real(pm_reel), intent(out), optional     :: ua                   ! unite astronomique (km)
real(pm_reel), intent(out), optional     :: vit_lum              ! celerite (m/s)
real(pm_reel), intent(out), optional     :: i_critique_non_retro ! inclinaison critique non retrograde solution de 1-5cos^2(i)=0
real(pm_reel), intent(out), optional     :: i_critique_retro     ! inclinaison critique retrograde solution de 1-5cos^2(i)=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mc_phys.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mc_phys.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation des valeurs des constantes demandees

if (present(ua))                                     ua = pm_ua
if (present(vit_lum))                           vit_lum = pm_vit_lum
if (present(i_critique_non_retro)) i_critique_non_retro = pm_i_critique_non_retro
if (present(i_critique_retro))         i_critique_retro = pm_i_critique_retro

code_retour%routine = pm_num_mc_phys
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mc_phys
