subroutine mc_math ( code_retour, pi, deux_pi, pi_sur2, deg_rad, rad_deg)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Acces aux constantes mathematiques elementaires
! ===

!$Historique
! ==========
!   + Version 1.0 (SP 210 ed01 rev00): creation a partir de la routine MCONST de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib (supprimé) par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

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
real(pm_reel), intent(out), optional   :: pi         !     pi
real(pm_reel), intent(out), optional   :: deux_pi    !     2*pi
real(pm_reel), intent(out), optional   :: pi_sur2    !     pi/2
real(pm_reel), intent(out), optional   :: deg_rad    !     constante de conversion degres -> radians
real(pm_reel), intent(out), optional   :: rad_deg    !     constante de conversion radians-> degres

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mc_math.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mc_math.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation des valeurs des constantes demandees

if (present(pi))       pi      = pm_pi
if (present(deux_pi))  deux_pi = pm_deux_pi
if (present(pi_sur2))  pi_sur2 = pm_pi_sur2
if (present(deg_rad))  deg_rad = pm_deg_rad
if (present(rad_deg))  rad_deg = pm_rad_deg

code_retour%routine = pm_num_mc_math
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mc_math
