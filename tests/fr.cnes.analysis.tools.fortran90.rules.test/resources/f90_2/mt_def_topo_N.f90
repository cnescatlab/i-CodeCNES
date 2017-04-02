subroutine mt_def_topo_N (lat, long, vect_i, vect_j, vect_k, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  DEFinition des cosinus directeurs d'un repere TOPOcentrique Nord (convention axe Ox vers le Nord).
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 251 ed01 rev00): creation a partir de la routine MVCAST de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                  ::  lat                     ! latitude geodesique de l'origine du repere topocentrique
real(pm_reel), intent(in)                  ::  long                    ! longitude geodesique de l'origine du repere topocentrique
real(pm_reel), intent(out), dimension(3)   ::  vect_i  ! vecteur i dans le repere de reference
real(pm_reel), intent(out), dimension(3)   ::  vect_j  ! vecteur j dans le repere de reference
real(pm_reel), intent(out), dimension(3)   ::  vect_k  ! vecteur k dans le repere de reference
type(tm_code_retour), intent(out)           ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_def_topo_N.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_def_topo_N.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! --------------------------------------------------------------
! calcul des composantes de i : horizontale locale dirigee vers
!                               le nord
!             i = (-sin(lat)cos(long),-sin(lat)sin(long),cos(lat))
! --------------------------------------------------------------

vect_i (1) = - sin (lat) * cos (long)
vect_i (2) = - sin (lat) * sin (long)
vect_i (3) =   cos (lat)

!-----------------------------------------------------------------
! calcul des composantes de j : tangente au parallele local dirigee
!                               vers l'ouest
!             j = (sin(long),-cos(long),0)
!-----------------------------------------------------------------

vect_j (1) =   sin (long)
vect_j (2) = - cos (long)
vect_j (3) = 0._pm_reel

!------------------------------------------------------------
! calcul des composantes de k : verticale locale
!             k = (cos(lat)cos(long),cos(lat)sin(long),sin(lat))
!------------------------------------------------------------

vect_k (1) = cos (lat) * cos (long)
vect_k (2) = cos (lat) * sin (long)
vect_k (3) = sin (lat)

code_retour%routine = pm_num_mt_def_topo_N
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_def_topo_N
