module surcharge_egal_mspro

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Acces simplifie a tous les modules necessaires 
! ===   a l'utilisation des surcharges de l'operateur d'egalite
!
! Note d'utilisation: 
! ==================
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib

use mwp_sgd_pos_vit_mspro           ! acces a la surcharge de l'operateur d'egalite pour tm_i_sgd_pos_vit
use mwp_car_pos_vit_mspro           ! acces a la surcharge de l'operateur d'egalite pour tm_i_car_pos_vit
use mwp_geoc_pos_vit_mspro          ! acces a la surcharge de l'operateur d'egalite pour tm_i_geoc_pos_vit
use mwp_geod_pos_mspro              ! acces a la surcharge de l'operateur d'egalite pour tm_geodesique
use mwp_gps_ard_pos_vit_mspro       ! acces a la surcharge de l'operateur d'egalite pour tm_i_gps_ard_pos_vit
use mwp_meca_vol_pos_vit_mspro      ! acces a la surcharge de l'operateur d'egalite pour tm_i_meca_vol_pos_vit
use mwp_orb_cir_equa_pos_vit_mspro  ! acces a la surcharge de l'operateur d'egalite pour tm_orb_cir_equa
use mwp_orb_cir_pos_vit_mspro       ! acces a la surcharge de l'operateur d'egalite pour tm_orb_cir
use mwp_orb_equa_pos_vit_mspro      ! acces a la surcharge de l'operateur d'egalite pour tm_orb_equa
use mwp_orb_kep_pos_vit_mspro       ! acces a la surcharge de l'operateur d'egalite pour tm_orb_kep
use mwp_orb_hpha_pos_vit_mspro      ! acces a la surcharge de l'operateur d'egalite pour tm_orb_hpha
use mwp_orb_vinf_pos_vit_mspro      ! acces a la surcharge de l'operateur d'egalite pour tm_orb_Vinf
use mwp_pole_uv_pos_mspro           ! acces a la surcharge de l'operateur d'egalite pour tm_pole_uv  
use mwp_ellips_pos_mspro            ! acces a la surcharge de l'operateur d'egalite pour tm_ellipsoide    
use mwp_def_topo_mspro              ! acces a la surcharge de l'operateur d'egalite pour tm_def_topo      
!use mwp_jour_sec_mspro              ! acces a la surcharge de l'operateur d'egalite pour tm_jour_sec
!mwp_jour_sec_mspro est passe dans la mslib : mdi_joursec_egal_joursec
use mwp_para_opt_mspro              ! acces a la surcharge de l'operateur d'egalite pour tm_i_rep_para_opt
use mwp_pole_tsid_planete_mspro     ! acces a la surcharge de l'operateur d'egalite pour tm_i_pole_tsid_planete

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: surcharge_egal_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


!..............................................................................................
character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
          '@(#) Fichier MSPRO surcharge_egal_mspro.f90: derniere modification V5.15 >'
!..............................................................................................

end module surcharge_egal_mspro
