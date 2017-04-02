module valeur_code_retour_mspro

! (C) Copyright CNES - MSPRO - 2000-2005

!************************************************************************
!
! But:  Definition des valeurs (numeriques) des codes retour. 
! ===
!
! Note d'utilisation:  toute routine ou module utilisant le module mspro connait implicitement ce module.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 1.0 (sans DE): ajout de nouveaux codes retour
!                         (Date: 12/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (sans DE) : ajout de nouveaux codes retour
!                             et de nouvelles plages
!                         (Date: 04/2002 - Realisation: Mickael Hazak et Guylaine Prat)
!   + Version 2.0 (DE 1) : suppression des codes retour pm_err_numero_routine_ng
!                          et pm_err_val_code_retour_ng
!                         (Date: 06/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE globale 2) : suppression des codes retour specifiques 
!                         aux routines passant dans la MSLIGHT
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 10/2002 - Realisation: Bruno Revelin et Michel Lagreca)
!   + Version 3.1 (DE 2) : suppression de codes retour suite a l'evolution d'algorithmes
!                          (dont passage dans la mslib90)
!                         (Date: 09/2003 - Realisation: Bruno Revelin et Guylaine Prat)
!   + Version 3.1 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 09/2003 - Realisation: Guylaine Prat)
!   + Version 4.0 (DE globale 8): suppression des codes retour specifiques aux routines
!                         passant dans la MSLIB90
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 5.0 (DE globale 11): suppression des codes retour specifiques aux routines
!                         passant dans la MSLIB90
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 5.1 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : creation DM 408
!                   (Date: 10/2005 - Realisation: Equipe Patrimoine de Mecanique Spatiale)
!   + Version 5.4 : FA-ID 475 : Division par zero
!                   FA-ID 474 : Anomalie pas min
!                         (Date: 02/02/2006 - Realisation: Atos Origin)
!                   FA-ID 439 : remarques qualite
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.7 : DM-ID 738 : Evolution du Cowell
!                   (Date: 06/2007 - Realisation: Sandrine Avril - Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!   + VERSION:5.13:FA-ID:1381:05/03/2010: Code d'erreur pm_err_integ_pas_min décommenté 
!                   
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: valeur_code_retour_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


!************************************************************************

!........................................................................
! Probleme sur la valeur d'une constante physique [ +/- 4001 , +/- 4099 ]
!........................................................................

integer,parameter :: pm_warn_apla_nul           = +4001

!..................................................................................
! Probleme sur la valeur du demi grand-axe de la conique: a [ +/- 4101 , +/- 4199 ]
!..................................................................................

!...................................................................
! Probleme sur la valeur de l'excentricite e [ +/- 4201 , +/- 4299 ]
!...................................................................

!!! DE globale 11: integer, parameter :: pm_warn_e_circul             = +4201 !!!  passe dans la mslib90 V6.0
!!! DE globale 11: integer, parameter :: pm_warn_e_circul_i_equa      = +4202 !!!  passe dans la mslib90 V6.0
!!! DE globale 11: integer, parameter :: pm_err_jac_non_calc_e_circul = -4201 !!!  passe dans la mslib90 V6.0
integer, parameter :: pm_err_e_non_hyperb             = -4202

!........................................................................................................
! Probleme sur la valeur de l'inclinaison i ou sur le vecteur inclinaison (ix,iy) [ +/- 4301 , +/- 4399 ]
!........................................................................................................

!!! DE globale 11: integer, parameter :: pm_warn_i_equa              = +4302 !!!  passe dans la mslib90 V6.0
!!! DE globale 11: integer, parameter :: pm_err_jac_non_calc_i_equa  = -4301 !!!  passe dans la mslib90 V6.0

!...................................................................
! Probleme sur une date, une duree, un temps [ +/- 4401 , +/- 4499 ]
!...................................................................

integer,parameter :: pm_err_duree_nul           = -4401
integer,parameter :: pm_err_mois                = -4402
integer,parameter :: pm_err_jour_interval_an    = -4403
integer,parameter :: pm_err_nb_saut_tuc_max     = -4404
integer,parameter :: pm_err_ind_ech_temps       = -4405
integer,parameter :: pm_err_date_inf_1saut_tuc  = -4406
integer,parameter :: pm_err_nb_saut_tuc_nul     = -4407
integer,parameter :: pm_err_fic_saut_tuc        = -4408

!..................................................................
! Probleme sur la position et/ou la vitesse [ +/- 4501 , +/- 4599 ]
!..................................................................

integer,parameter :: pm_warn_lamb_pi              = +4501
integer,parameter :: pm_warn_lat_cira             = +4502
!!! DE globale 2: integer,parameter :: pm_warn_alt_sup600km = +4503 !!!
integer,parameter :: pm_warn_vit_vertic           = +4504
integer,parameter :: pm_warn_pos_Oz_ref_meca_vol  = +4506
integer,parameter :: pm_warn_fuse_vertic          = +4507

integer,parameter :: pm_err_alt_sup120km          = -4501
integer,parameter :: pm_err_alt_inf90km           = -4502
integer,parameter :: pm_err_alt_inf120km          = -4503

!!! DE 2: integer,parameter :: pm_err_jac_non_calc_poles       = -4504 !!! passe dans la mslib90 V4.1
integer,parameter :: pm_err_jac_non_calc_vit_vertic  = -4505
integer,parameter :: pm_err_vit_plan_sym_avion_ortho = -4506
!!! DE 2: integer,parameter :: pm_err_jac_non_calc_alt_neg     = -4507 !!! passe dans la mslib90 V4.1
integer,parameter :: pm_err_jac_non_calc_decl_pisur2 = -4508
integer,parameter :: pm_err_Bplane_decli_pisur2      = -4509
integer,parameter :: pm_err_Bplane_BT_nulle          = -4510
integer,parameter :: pm_err_C3_negatif_ou_nul        = -4511
integer,parameter :: pm_err_Rp_negatif_ou_nul        = -4512
integer,parameter :: pm_err_jac_non_calc_sin_theta   = -4513
integer,parameter :: pm_err_non_ellip                = -4514

!........................................................................
! Probleme sur la valeur d'une variable physique [ +/- 4601 , +/- 4699 ]
!........................................................................

integer,parameter :: pm_err_flux_sol_negatif    = -4601
integer,parameter :: pm_err_ap_negatif          = -4602

!.........................................................................
! Probleme de codage dans le code de l'utilisateur [ +/- 4801 , +/- 4898 ]
!.........................................................................

integer,parameter :: pm_warn_sub                = +4801
integer,parameter :: pm_warn_para_opt_trop      = +4802 
integer,parameter :: pm_warn_trsf_atypique      = +4803 
integer,parameter :: pm_warn_commutation        = +4804

integer,parameter :: pm_err_sub                 = -4801
integer,parameter :: pm_err_option              = -4802
!!! DE 1: Numero supprime  pm_err_numero_routine_ng   = -4803 !!!
!!! DE 1: Numero supprime  pm_err_val_code_retour_ng  = -4804 !!!
integer,parameter :: pm_err_biblio_inconnu      = -4805
integer,parameter :: pm_err_pt_double           = -4806
!!! DE globale 8: integer,parameter :: pm_err_clef_rot = -4807 !!!
integer,parameter :: pm_err_val_para            = -4808 
integer,parameter :: pm_err_transfo             = -4809
integer,parameter :: pm_err_para_opt_abs        = -4810
integer,parameter :: pm_err_para_incoherents    = -4811 
!!! DE 2: Numero supprime  pm_err_jacob_impossible    = -4812 !!!
integer,parameter :: pm_err_type_integ          = -4813
integer,parameter :: pm_err_dans_sub_commut     = -4814
integer,parameter :: pm_err_integ_ordre_1       = -4815
integer,parameter :: pm_err_integ_ordre_2       = -4816
integer,parameter :: pm_err_integ_ordre_3       = -4817
integer,parameter :: pm_err_integ_ordre_4       = -4818
integer,parameter :: pm_err_integ_ireg          = -4819
integer,parameter :: pm_err_integ_rxmu          = -4820
integer,parameter :: pm_err_integ_hyp           = -4821

!.................................................
! Probleme de codage dans le code MSPRO [ - 4899 ]
!.................................................

!........................................................................................
! Probleme de convergence dans des routines de mecanique spatiale [ +/- 4901 , +/- 4997 ]
!........................................................................................

!!! DE globale 2: integer,parameter :: pm_err_conv_battin = -4901 !!!
integer,parameter :: pm_err_conv_cowell      = -4901
integer,parameter :: pm_err_dim_etat_cowell  = -4902

!..................................................................................................
! Probleme du a une routine issue de l'IOLIB ou de ZOOM [+/- 4998,+/-4999]
!..................................................................................................

integer,parameter :: pm_err_IOLIB               = -4998
integer,parameter :: pm_warn_IOLIB              = +4998
integer,parameter :: pm_err_ZOOM                = -4999

!..............................................
! Probleme mathematique [ +/- 5001 , +/- 5900 ]
!..............................................

integer,parameter :: pm_warn_long_indef            = +5001
integer,parameter :: pm_warn_lat_long_indef        = +5002
integer,parameter :: pm_warn_int_hors_domaine      = +5003
integer,parameter :: pm_warn_extrapol              = +5004
!!! DE 2: integer,parameter :: pm_warn_interpol_pt_double    = +5005 !!!
!!! DE globale 8: integer,parameter :: pm_warn_angle1_ou_3_indef     = +5006 !!!
integer,parameter :: pm_warn_extrapol_borne_double = +5007
integer,parameter :: pm_warn_integ_pas_min = +5008

integer,parameter :: pm_err_nb_pt_inf2          = -5001
integer,parameter :: pm_err_nb_pt_max           = -5002
integer,parameter :: pm_err_dim_y_inf1          = -5003
integer,parameter :: pm_err_x_non_diff          = -5004
integer,parameter :: pm_err_x_non_ord_croi      = -5005
integer,parameter :: pm_err_int_hors_domaine    = -5006
integer,parameter :: pm_err_pas_tabul_negatif   = -5007
integer,parameter :: pm_err_pas_tabul_nul       = -5008
integer,parameter :: pm_err_dim_mat             = -5009
integer,parameter :: pm_err_nb_pt_inf1          = -5010
!!! DE globale 8: integer,parameter :: pm_err_mat_non_rot         = -5011 !!!
integer,parameter :: pm_err_dim_inf2            = -5012
integer,parameter :: pm_err_valeur_repetee      = -5013
integer,parameter :: pm_err_pivot_max_trop_faible = -5014
integer,parameter :: pm_err_dimension           = -5015
integer,parameter :: pm_err_cherche_racine      = -5016
integer,parameter :: pm_err_integ_pas_min       = -5017
integer,parameter :: pm_err_integ_dates      = -5018

!......................................
!Problemes systeme [+/- 5901, +/- 5999] 
!......................................

integer,parameter :: pm_err_allocate            = -5901
integer,parameter :: pm_err_deallocate          = -5902
integer,parameter :: pm_err_fichier_absent      = -5903
integer,parameter :: pm_err_fichier_ouvert      = -5904
integer,parameter :: pm_err_alc_unite_logique   = -5905
integer,parameter :: pm_err_ouvrir_fichier      = -5906
integer,parameter :: pm_err_lire_don            = -5907
integer,parameter :: pm_err_fermer_fichier      = -5908
!...............................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                  '@(#) Fichier MSPRO valeur_code_retour_mspro.f90: derniere modification V5.15 >'

end module valeur_code_retour_mspro

