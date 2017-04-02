subroutine mxi_var_typ2noeud ( typ_var, var_int, retour )

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Initialisation du numero du noeud initial ou du noeud final, de la 
! ===   transformation dans l'arbre des variables, en fonction du type
!       de variable demande

!
! Note d'utilisation:
! ===================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                  (Date: 11/2002 - Realisation: Michel Lagreca et Bruno Revelin)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 72 : Coordonnees hyperboliques dans l'objet bulletin de la GSLIB
!                              (Date: 09/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use parametre_themeX_interne_mspro
use parametre_themeX_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)   :: typ_var   ! type de variable utilisateur
integer, intent(out)  :: var_int   ! type de variable interne
integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mxi_var_typ2noeud.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! initialisation du numero du noeud a partir du type de variable fourni
! (par l'utilisateur)
! .....................................................................

if (typ_var == pm_car)           var_int = pm_i_car
if (typ_var == pm_Vinf)          var_int = pm_i_Vinf
if (typ_var == pm_Vinfarr)       var_int = pm_i_Vinfarr
if (typ_var == pm_hpha)          var_int = pm_i_hpha
if (typ_var == pm_kep)           var_int = pm_i_kep
if (typ_var == pm_cir)           var_int = pm_i_cir
if (typ_var == pm_equa)          var_int = pm_i_equa
if (typ_var == pm_cir_equa)      var_int = pm_i_cir_equa
if (typ_var == pm_geoc)          var_int = pm_i_geoc
if (typ_var == pm_geod_meca_vol) var_int = pm_i_geod_meca_vol
if (typ_var == pm_geod_gps_ard)  var_int = pm_i_geod_gps_ard
if (typ_var == pm_sgd_nord)      var_int = pm_i_sgd_nord
if (typ_var == pm_sgd_est)       var_int = pm_i_sgd_est

end subroutine mxi_var_typ2noeud
