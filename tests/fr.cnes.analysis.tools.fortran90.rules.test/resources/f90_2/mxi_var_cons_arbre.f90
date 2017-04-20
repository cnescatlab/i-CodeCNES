subroutine mxi_var_cons_arbre ( arbre, retour )

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  construction de l'arbre des routines de changement de variables
! ===
!
! Note d'utilisation:  La variable arbre est declaree dans mx_var
! ==================   en save, donc sauvegardee au cours des appels 
!                      successifs a cette routine
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Michel Lagreca)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 72 : Coordonnees hyperboliques dans l'objet bulletin de la GSLIB
!                              (Date: 09/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_themeX_interne_mspro     ! inclus use type_mspro
use parametre_themeX_interne_mspro
use int_chapeau_internes, only : mxi_construit_fils

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_i_noeud),dimension(:), intent(out) :: arbre ! arbre
integer, intent(out) :: retour 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: retour_local     ! code retour local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_var_cons_arbre.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations des champs 'peres' de chaque noeud de l'arbre

arbre(pm_i_car)%pere           = pm_i_rien
arbre(pm_i_Vinf)%pere          = pm_i_kep
arbre(pm_i_Vinfarr)%pere       = pm_i_kep
arbre(pm_i_hpha)%pere          = pm_i_kep
arbre(pm_i_kep)%pere           = pm_i_car
arbre(pm_i_cir)%pere           = pm_i_car
arbre(pm_i_equa)%pere          = pm_i_car
arbre(pm_i_cir_equa)%pere      = pm_i_car
arbre(pm_i_geoc)%pere          = pm_i_car
arbre(pm_i_geod_meca_vol)%pere = pm_i_car
arbre(pm_i_geod_gps_ard)%pere  = pm_i_car
arbre(pm_i_sgd_nord)%pere      = pm_i_car
arbre(pm_i_sgd_est)%pere       = pm_i_car

! calcul des fils en fonction des peres definis ci-dessus

call mxi_construit_fils( pm_i_nombre_variable, arbre, retour_local )
retour = retour_local

end subroutine mxi_var_cons_arbre
