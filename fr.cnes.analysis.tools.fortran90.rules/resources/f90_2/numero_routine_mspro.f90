module numero_routine_mspro

! (C) Copyright CNES - MSPRO - 2000-2005

!************************************************************************
!
! But:  Definition des numeros des routines via des parameter. 
! ===   Les routines possedent des numeros compris entre 4000 et 4999.
!
! Note d'utilisation:  toute routine ou module utilisant le module mspro connait implicitement ce module.
! ==================   Les modules possedent des numeros compris entre 5001 et 5999.
!
!$Historique
! ==========
!   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 1.0 (sans DE): ajout de nouvelles routines et de nouveaux modules
!                         (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : ajout du parameter pm_mspro, ajout de nouvelles routines
!                          et changement de standard pre-mslib90 => MSPRO90 pour certaines routines
!                         (Date: 04/2002 - Realisation: Guylaine Prat et Mickael Hazak)
!   + Version 3.0 (DE globale 2) : suppression des routines passant dans la MSLIGHT
!                          et suppression du standard MSPRO90
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (sans DE): ajout de nouvelles routines et de nouveaux modules
!                            ajout de commentaires pour les sources 77
!                         (Date: 02/2003 - Realisation: Michel Lagreca, Bruno Revelin et Guylaine Prat)
!   + Version 3.0 (DE 2): modification de la plage des numeros OT pour les modules
!                         pour prise en compte des modules procedures
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (sans DE) : ajout nouvelles routines (internes) et de nouveaux modules 
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 8): suppression des routines passant dans la MSLIB90
!                         (Date: 11/2003 - Realisation: Veronique Lepine)
!   + Version 4.0 (sans DE) : ajout nouveau module interne 
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE globale 11): suppression des routines passant dans la MSLIB90
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (sans DE): ajout de nouvelles routines
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (sans DE) : ajout nouvelles routines
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   transfert de mu_racine de la MSPRO a la MSLIB90
!                         (Date: 09/2005 - Realisation: Claire Fabre - Atos origin) 
!   + Version 5.5 : DM-ID 413 : Calcul du systeme de parametres Vinfini d arrivee
!                         (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 473 : inhibition de la détection d'événements
!                         (Date: 09/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

  use mslib

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: numero_routine_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Indicateur du numero de librairie 
  ! (pour affectation du champ %biblio du type code_retour)

  integer, parameter, public :: pm_mspro = 2

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Routines au standard pre-mslib90 - Plage de numeros = [4000,4499]
  ! =================================================================
   
  ! Le classement se fait par ordre numerique.
  ! Toute nouvelle routine prend le numero en suivant 
  ! (afin d'eviter des trous ou des doublons de numerotation).

  ! Les numeros indiques pour les routines internes serviront uniquement a la gestion en configuration

  integer,parameter :: pm_num_mu_car_spher              = 4000
  integer,parameter :: pm_num_mu_spher_car              = 4001
  integer,parameter :: pm_num_mzpro_code_retour         = 4002
  integer,parameter :: pm_num_mzpro_traiter_retour      = 4003
!  integer,parameter :: pm_num_mu_racine                 = 4004
  integer,parameter :: pm_num_mu_eq2degre_reel          = 4005
  integer,parameter :: pm_num_mu_spline_cub_init        = 4006
  integer,parameter :: pm_num_mu_spline_cub_eval        = 4007
  integer,parameter :: pm_num_mu_lagrange               = 4008

!!! DE globale 2: integer,parameter :: pm_num_mu_lagrange_8bul  = 4009 !!!

  integer,parameter :: pm_num_mu_statis                 = 4010

!!! DE globale 8: integer,parameter :: pm_num_mu_mat_quat           = 4011 !!!
!!! DE globale 8: integer,parameter :: pm_num_mu_quat_mat           = 4012 !!!

!!! DE globale 2: integer,parameter :: pm_num_mm_car_impul          = 4013 !!!
!!! DE globale 2: integer,parameter :: pm_num_mm_propa_cov_orb      = 4014 !!!
!!! m-de-4501-1-CIS: integer,parameter :: pm_num_mm_lamb_battin_m1t = 4015 !!!

!!!  DE globale 11 : integer,parameter :: pm_num_mv_kep_cir         = 4016 !!!
!!!  DE globale 11 : integer,parameter :: pm_num_mv_cir_kep         = 4017 !!!
!!!  DE globale 11 : integer,parameter :: pm_num_mv_kep_equa        = 4018 !!!
!!!  DE globale 11 : integer,parameter :: pm_num_mv_equa_kep        = 4019 !!!
!!!  DE globale 11 : integer,parameter :: pm_num_mv_kep_cir_equa    = 4020 !!!
!!!  DE globale 11 : integer,parameter :: pm_num_mv_cir_equa_kep    = 4021 !!!

  integer,parameter :: pm_num_mp_atm_cira               = 4022

!!! m-de-4502-1-CIS: integer,parameter :: pm_num_mmi_lamb_battin_para  = 4023 !!!
!!! m-de-4503-1-CIS: integer,parameter :: pm_num_mmi_lamb_battin_frac1 = 4024 !!!
!!! m-de-4504-1-CIS: integer,parameter :: pm_num_mmi_lamb_battin_frac2 = 4025 !!!

  integer,parameter :: pm_num_mpi_atmi                  = 4026
  integer,parameter :: pm_num_mpi_atmi_alt              = 4027
  integer,parameter :: pm_num_mpi_atmi_inter            = 4028
  integer,parameter :: pm_num_mpi_atmi_temp             = 4029
  integer,parameter :: pm_num_mpi_atmi_janvier          = 4030
  integer,parameter :: pm_num_mpi_atmi_fevrier          = 4031
  integer,parameter :: pm_num_mpi_atmi_mars             = 4032
  integer,parameter :: pm_num_mpi_atmi_avril            = 4033
  integer,parameter :: pm_num_mpi_atmi_mai              = 4034
  integer,parameter :: pm_num_mpi_atmi_juin             = 4035
  integer,parameter :: pm_num_mpi_atmi_juillet          = 4036
  integer,parameter :: pm_num_mpi_atmi_aout             = 4037
  integer,parameter :: pm_num_mpi_atmi_septembre        = 4038
  integer,parameter :: pm_num_mpi_atmi_octobre          = 4039
  integer,parameter :: pm_num_mpi_atmi_novembre         = 4040
  integer,parameter :: pm_num_mpi_atmi_decembre         = 4041
  integer,parameter :: pm_num_mpi_atmo                  = 4042

  integer,parameter :: pm_num_mp_atm_msis86             = 4043 
                       ! et pour mpi_data_msis86 et source77/mpi_IO_e_msis86

!!! DE globale 2: integer,parameter :: pm_num_mp_atm_roat77    = 4044 !!!

  integer,parameter :: pm_num_mp_mag_ap_kp              = 4045
  integer,parameter :: pm_num_mp_mag_kp_ap              = 4046
                       ! et pour source77/mpi_IO_e_trapkp  (commun aux deux routines ci-dessus)

  integer,parameter :: pm_num_mp_atm_us76d              = 4047
                       ! et pour source77/mpi_IO_e_us76

  integer,parameter :: pm_num_mp_atm_cira_msis86        = 4048

  integer,parameter :: pm_num_mp_atm_dtm78              = 4049
                       ! et pour source77/mpi_zoom_mdtm et les includes associes

  integer,parameter :: pm_num_md_calend_anjour          = 4050
  integer,parameter :: pm_num_md_anjour_calend          = 4051
  integer,parameter :: pm_num_mu_inter_ind              = 4052
  integer,parameter :: pm_num_mu_inter_dim1_lin         = 4053
  integer,parameter :: pm_num_mu_inter_dim2_deg2        = 4054
  integer,parameter :: pm_num_mu_inter_dim3_deg3        = 4055
  integer,parameter :: pm_num_mt_car_meca_vol           = 4056
  integer,parameter :: pm_num_mt_meca_vol_car           = 4057
  integer,parameter :: pm_num_mt_gps_ard_car            = 4058
  integer,parameter :: pm_num_mt_car_gps_ard            = 4059
  integer,parameter :: pm_num_mti_car_pseudo_topoN      = 4060
  integer,parameter :: pm_num_mti_pseudo_topoN_car      = 4061

!!! DE globale 8:  integer,parameter :: pm_num_mu_3rot_quat              = 4062 !!!
!!! DE globale 8:  integer,parameter :: pm_num_mu_quat_3rot              = 4063 !!!

  integer,parameter :: pm_num_ma_avion_sol              = 4064
  integer,parameter :: pm_num_ma_avion_vit              = 4065

!!! DE globale 8:  integer,parameter :: pm_num_mui_axe_norme_quat        = 4066 !!!

  integer,parameter :: pm_num_mui_mat_rot_elem          = 4067
  integer,parameter :: pm_num_mui_recale_angle          = 4068
  integer,parameter :: pm_num_mui_interp_newton         = 4069
  integer,parameter :: pm_num_mzipro_val_retour         = 4070
  integer,parameter :: pm_num_mzipro_numero_routine     = 4071
  integer,parameter :: pm_num_md_lire_saut_tuc          = 4072
  integer,parameter :: pm_num_md_ech_temps              = 4073
  integer,parameter :: pm_num_mdi_lire_don_saut_tuc     = 4074
  integer,parameter :: pm_num_mdi_ech_temps_te_tai      = 4075
  integer,parameter :: pm_num_mdi_ech_temps_tai_tuc     = 4076
  integer,parameter :: pm_num_mpi_data_msis86           = 4077
  integer,parameter :: pm_num_mwi_alc_unite_logique     = 4078    
  integer,parameter :: pm_num_mwi_fermer_fichier        = 4079 
  integer,parameter :: pm_num_mwi_ouvrir_fichier        = 4080 
  integer,parameter :: pm_num_mwi_chercher_fichier      = 4081
  integer,parameter :: pm_num_mti_rot_axeZ              = 4082
  integer,parameter :: pm_num_mx_rep                    = 4083
  integer,parameter :: pm_num_mxi_rep_cons_arbre        = 4084
  integer,parameter :: pm_num_mxi_construit_fils        = 4085
  integer,parameter :: pm_num_mxi_verinit_dates         = 4086
  integer,parameter :: pm_num_mxi_def_parcours          = 4087
  integer,parameter :: pm_num_mx_var                    = 4088
  integer,parameter :: pm_num_mxi_var_cons_arbre        = 4089 
  integer,parameter :: pm_num_mxi_var_ench_transfo      = 4090 
  integer,parameter :: pm_num_mxi_var_ident             = 4091 
  integer,parameter :: pm_num_mxi_var_typ_atyp          = 4092 
  integer,parameter :: pm_num_mxi_var_typ2noeud         = 4093
  integer,parameter :: pm_num_mxi_rep_ench_transfo      = 4094
  integer,parameter :: pm_num_mxi_transfo_identique     = 4095
  integer,parameter :: pm_num_mvi_hpha_kep              = 4096
  integer,parameter :: pm_num_mvi_kep_hpha              = 4097
  integer,parameter :: pm_num_mvi_cir_equa_cir          = 4098
  integer,parameter :: pm_num_mvi_cir_equa_equa         = 4099
  integer,parameter :: pm_num_mv_kep_Bplane             = 4100
  integer,parameter :: pm_num_mv_Bplane_kep             = 4101
  integer,parameter :: pm_num_mu_factor_LU              = 4102
  integer,parameter :: pm_num_mu_det_mat                = 4103
  integer,parameter :: pm_num_mu_resol_sys_lin          = 4104
  integer,parameter :: pm_num_mu_inv_mat                = 4105
  integer,parameter :: pm_num_mv_Vinf_kep               = 4106
  integer,parameter :: pm_num_mv_kep_Vinf               = 4107
  integer,parameter :: pm_num_mu_creer_integrateur      = 4108
  integer,parameter :: pm_num_mu_creer_gest_pas         = 4109
  integer,parameter :: pm_num_mu_integrer               = 4110
  integer,parameter :: pm_num_mu_ajouter_evenement      = 4111
  integer,parameter :: pm_num_mu_liberer_integ          = 4112
  integer,parameter :: pm_num_mui_integ_butcher         = 4113
  integer,parameter :: pm_num_mui_integ_RK              = 4114
  integer,parameter :: pm_num_mui_integ_RKF             = 4115
  integer,parameter :: pm_num_mui_integ_commut          = 4116
  integer,parameter :: pm_num_mui_integ_interp          = 4117
  integer,parameter :: pm_num_mui_integ_racine          = 4118
  integer,parameter :: pm_num_mui_interp_Gill           = 4119
  integer,parameter :: pm_num_mui_integ_erreur          = 4120
  integer,parameter :: pm_num_mui_integ_init_pas        = 4121
  integer,parameter :: pm_num_mui_interp_DOP853         = 4122
  integer,parameter :: pm_num_mui_interp_init           = 4123
  integer,parameter :: pm_num_mv_Vinfarr_kep            = 4124
  integer,parameter :: pm_num_mv_kep_Vinfarr            = 4125
  integer,parameter :: pm_num_mu_supprimer_evenement    = 4126

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Routines au standard MSPRO90  - Plage de numeros = [4500,4749]
  ! ==============================================================

  ! Le classement se fait par ordre numerique.
  ! Toute nouvelle routine prend le numero en suivant 
  ! (afin d'eviter des trous ou des doublons de numerotation).

!!! DE globale 2: integer,parameter :: pm_num_mzpro_traiter_retour   = 4500 !!!
!!! DE globale 2: integer,parameter :: pm_num_mm_lamb_battin_m1t     = 4501 !!!
!!! DE globale 2: integer,parameter :: pm_num_mmi_lamb_battin_para   = 4502 !!!
!!! DE globale 2: integer,parameter :: pm_num_mmi_lamb_battin_frac1  = 4503 !!!
!!! DE globale 2: integer,parameter :: pm_num_mmi_lamb_battin_frac2  = 4504 !!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !   Liste des modules   - Plage de numeros = [5001,5499]
  ! ==============================================================

  ! Le classement se fait par ordre numerique.  
  ! Tout nouveau module prend le numero en suivant 
  ! (afin d'eviter des trous ou des doublons de numerotation).

  !   mspro                     5001
  !   type_mspro                5002
  !   parametre_mspro           5003
  !   interface_mspro           5005
  !   valeur_code_retour_mspro  5006
  !   numero_routine_mspro      5007
  !   surcharge_egal_mspro      5008
  !   surcharge_plus_mspro      5009
  !   surcharge_moins_mspro     5010
  !   type_themeX_mspro         5011
  !   parametre_themeX_mspro    5012
  !   parametre_themeX_interne_mspro    5013
  !   parametre_interne_mspro   5014

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !   Liste des modules-procedures   - Plage de numeros = [5500,5999]
  ! =================================================================

  ! Le classement se fait par ordre numerique.  
  ! Tout nouveau module-procedure prend le numero en suivant 
  ! (afin d'eviter des trous ou des doublons de numerotation).

  !   mwp_car_pos_vit_mspro              5500
  !   mwp_def_topo_mspro                 5501
  !   mwp_ellips_pos_mspro               5502
  !   mwp_geoc_pos_vit_mspro             5503
  !   mwp_geod_pos_mspro                 5504
  !   mwp_gps_ard_pos_vit_mspro          5505
  !   mwp_jour_sec_moins_mspro           5506
  !   mwp_jour_sec_mspro                 5507
  !   mwp_jour_sec_plus_mspro            5508
  !   mwp_meca_vol_pos_vit_mspro         5509
  !   mwp_orb_cir_equa_pos_vit_mspro     5510
  !   mwp_orb_cir_pos_vit_mspro          5511
  !   mwp_orb_equa_pos_vit_mspro         5512
  !   mwp_orb_kep_pos_vit_mspro          5513
  !   mwp_para_opt_mspro                 5514
  !   mwp_pole_uv_pos_mspro              5515
  !   mwp_sgd_pos_vit_mspro              5516
  !   mwp_orb_hpha_pos_vit_mspro         5517
  !   mwp_pole_tsid_planete_mspro        5518

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO numero_routine_mspro.f90: derniere modification V5.15 >'

!................................................................................................................

end module numero_routine_mspro
