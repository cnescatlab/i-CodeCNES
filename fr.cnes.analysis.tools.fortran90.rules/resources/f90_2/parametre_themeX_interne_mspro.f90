module parametre_themeX_interne_mspro

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Definition des parametres internes MSPRO du theme X
! ===   
!
! Note d'utilisation: 
! ==================  
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 12/2002 - Realisation: Bruno Revelin et Michel Lagreca)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE 2 mx_rep) : revision de l'arbre (redefinition du Terrestre Reference Inertiel)
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (DE 3 mx_rep) : passage du repere Terrestre Reference Inertiel dans une autre branche
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
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

use mslib

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: parametre_themeX_interne_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


intrinsic epsilon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! definition de parameter lies aux arbres (a faire evoluer si besoin)
! .......................................

! nombre maximum de fils pour un noeud 
integer, parameter :: pm_i_taille_fils_max = 10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! valeurs de dates pour 1 janvier2000 12h00 TDB et 1950 1 janvier1950 00h00 TE
! ............................................................................

real(pm_reel),parameter      :: pm_i_dtu1_1janvier1950_00h00 = -29.184_pm_reel   
real(pm_reel),parameter      :: pm_i_dtai_1janvier1950_00h00 = -32.184_pm_reel   
real(pm_reel),parameter      :: pm_i_dtu1_1janvier2000_12h00 = -63.829_pm_reel   
real(pm_reel),parameter      :: pm_i_dtai_1janvier2000_12h00 = -32.184_pm_reel   
integer      ,parameter      :: pm_i_jour_1janvier1950_00h00 = 0
real(pm_reel),parameter      :: pm_i_sec_1janvier1950_00h00 = 0._pm_reel
integer      ,parameter      :: pm_i_jour_1janvier2000_12h00 = 18262
real(pm_reel),parameter      :: pm_i_sec_1janvier2000_12h00 = 43200._pm_reel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! constantes de comparaison
! .........................

real(pm_reel), parameter :: pm_i_eps_ecart_sec_defaut = 1.e-7_pm_reel*epsilon(1._pm_reel) ! dates (s)
real(pm_reel), parameter :: pm_i_eps_coord_pole = 1.e-10_pm_reel ! coordonnees du pole (radians)
real(pm_reel), parameter :: pm_i_eps_angle_nul = 100._pm_reel*epsilon(1._pm_reel) ! angle nul (radians)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! definition de la numerotation des noeuds dans l'arbre des reperes (utilise par mx_rep)
! .................................................................

integer, parameter :: pm_i_rien = 0

! Branche Terre1
integer, parameter :: pm_i_veis_Terre1 = 1
integer, parameter :: pm_i_topo_Cin_Terre1 = 2
integer, parameter :: pm_i_topo_Cout_Terre1 = 3
integer, parameter :: pm_i_terre_ref_long_Cin_Terre1 = 4
integer, parameter :: pm_i_terre_ref_long_Cout_Terre1 = 5
integer, parameter :: pm_i_terre_ref_long_nul_Terre1 = 6
integer, parameter :: pm_i_terre_vrai_Terre1 = 7
integer, parameter :: pm_i_equa_vrai_Terre1 = 8
integer, parameter :: pm_i_equa_moy_Terre1 = 9

! Branche Terre2

integer, parameter :: pm_i_veis_Terre2 = 10
integer, parameter :: pm_i_topo_Cout_Terre2 = 11
integer, parameter :: pm_i_terre_ref_long_Cout_Terre2 = 12
integer, parameter :: pm_i_terre_ref_long_nul_Terre2 = 13
integer, parameter :: pm_i_terre_vrai_Terre2 = 14
integer, parameter :: pm_i_equa_vrai_Terre2 =15
integer, parameter :: pm_i_equa_moy_Terre2 = 16

! Branche Terre Ref Iner
integer, parameter :: pm_i_terre_ref_iner_Cin = 17
integer, parameter :: pm_i_terre_ref_iner_Cout = 18

! Branche Planete1
integer, parameter :: pm_i_topo_Cin_Planete1 = 19
integer, parameter :: pm_i_topo_Cout_Planete1 = 20
integer, parameter :: pm_i_planeto_iner_Cin_Planete1 = 21
integer, parameter :: pm_i_planeto_iner_Cout_Planete1 = 22
integer, parameter :: pm_i_planeto_ref_Planete1 = 23
integer, parameter :: pm_i_equa_uai_Planete1 = 24

! Branche Planete2
integer, parameter :: pm_i_topo_Cout_Planete2 = 25
integer, parameter :: pm_i_planeto_iner_Cout_Planete2 = 26
integer, parameter :: pm_i_planeto_ref_Planete2 = 27
integer, parameter :: pm_i_equa_uai_Planete2 = 28

! Branche Ecli 2000
integer, parameter :: pm_i_ecli_2000 = 29

! noeud racine
integer, parameter :: pm_i_EME2000 = 30

integer, parameter :: pm_i_nombre_repere = pm_i_EME2000 ! nombre de reperes

! definition des branches
integer, parameter :: pm_i_branche_Terre1 = 1
integer, parameter :: pm_i_branche_Terre2 = 2
integer, parameter :: pm_i_branche_Planete1 = 3
integer, parameter :: pm_i_branche_Planete2 = 4
integer, parameter :: pm_i_branche_Ecli2000 = 5
integer, parameter :: pm_i_branche_Terre_Ref_Iner = 6

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! definition de la numerotation des noeuds dans l'arbre des variables (utilise par mx_var)
! ...................................................................

integer, parameter :: pm_i_Vinfarr       = 1
integer, parameter :: pm_i_Vinf          = 2
integer, parameter :: pm_i_hpha          = 3
integer, parameter :: pm_i_kep           = 4
integer, parameter :: pm_i_cir           = 5
integer, parameter :: pm_i_equa          = 6
integer, parameter :: pm_i_cir_equa      = 7
integer, parameter :: pm_i_geoc          = 8
integer, parameter :: pm_i_geod_meca_vol = 9
integer, parameter :: pm_i_geod_gps_ard  = 10
integer, parameter :: pm_i_sgd_nord      = 11
integer, parameter :: pm_i_sgd_est       = 12

! noeud racine
integer, parameter :: pm_i_car           = 13

integer, parameter :: pm_i_nombre_variable = pm_i_car ! nombre de variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!................................................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
     '@(#) Fichier MSPRO parametre_themeX_interne_mspro.f90: derniere modification V5.15 >'

!................................................................................................................

end module parametre_themeX_interne_mspro
