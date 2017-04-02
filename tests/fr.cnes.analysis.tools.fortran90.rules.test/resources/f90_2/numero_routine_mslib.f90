
module numero_routine_mslib

! (C) Copyright CNES - MSLIB - 1997-2005

!************************************************************************
!
! But:  Definition des numeros des routines via des parameter. 
! ===   Les routines possedent des numeros compris entre 1001 a 1999.
!
! Note d'utilisation:  toute routine ou module utilisant parametre_mslib connait implicitement ce module.
! ==================   Les modules possedent des numeros compris entre 2001 et 2999.
!
!$Historique
! ==========
!   + Version 0.1 (SP 176 ed01 rev00): creation
!                         (Date: 12/1997 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         et rajout de numero
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (sans DE): ajout de nouvelles routines et de nouveaux modules
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 1.0.1 (DE 294 ed01 rev 00): mauvais nom de parameter pour 3 routines internes
!                         (Date: 01/1999 - Realisation: Guylaine Prat)
!   + Version 2.0 (sans DE): ajout de nouvelles routines et de nouveaux modules
!                         (Date: 10/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (sans DE): ajout de nouvelles routines et de nouveaux modules
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE 442 ed01 rev00) : ajout du parameter pm_mslib90
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (sans DE) : modif de la ligne rcs_id, ajout de nouvelles routines et suppression de certaines routines internes
!                         (Date: 01/2003 - Realisation: Bruno Revelin et Guylaine Prat)
!   + Version 5.0 (sans DE) : ajout de nouvelles routines et modules
!                         (Date: 10/2003 - Realisation: Bruno Revelin et Veronique Lepine)
!   + Version 6.0 (sans DE): ajout de nouvelles routines
!                         (Date: 04/2004 - Realisation: Veronique Lepine)
!   + Version 6.1 (sans DE): ajout de nouvelles routines
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 6.2 (sans DE): ajout de nouvelles routines
!                         (Date: 10/2004 - Realisation: Veronique Lepine)
!   + Version 6.2 (DE globale 1): suppression de routines internes
!                         (Date: 01/2005 - Realisation: Guylaine Prat)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul      
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN)          
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                           ajout de nouvelles routines du theme I
!                           (Date: 09/2005 - Realisation: Atos Origin)
!   + version 6.3 : DM-ID 162 : introduction d'une routine de propagation d'orbite prenant en compte le 
!                               terme central du potentiel gravitationnel et les effets seculaires du J2
!                   creation a partir de la routine AM_pro_exorj2 de l' AMLIB
!                   (date: 11/2005 - realisation: Julien Bouillant - Atos Origin)
!   + Version 6.4 : DM-ID 426 : routine de calcul des angles de precession
!                 (Date: 04/2006 - Realisation: Claire Fabre - Atos Origin)
!                   DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                         (Date: 05/2006 - Realisation : Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : ajout des numéros des nouvelles fonctions (calcul optimise)
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1092 : Nouvelles routines IERS 2003
!                   (Date: 09/2008 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use longueur_chaine_mslib

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: numero_routine_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Indicateur du numero de librairie 
! (pour affectation du champ %biblio du type code_retour)

integer, parameter, public :: pm_mslib90 = 1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Le classement se fait par ordre numerique.
! Toute nouvelle routine prend le numero en suivant 
! (afin d'eviter des trous ou des doublons de numerotation).

! Les numeros indiques pour les routines internes serviront uniquement a la gestion en configuration

integer,parameter :: pm_num_mz_val_code_retour  = 1001
integer,parameter :: pm_num_mz_numero_routine   = 1002
integer,parameter :: pm_num_mt_topo_E_car_sgd   = 1003
integer,parameter :: pm_num_mu_angle2           = 1004
integer,parameter :: pm_num_routines_compar     = 1005 ! pour les routines communes aux programmes de comparaison
integer,parameter :: pm_num_md_calend_julien    = 1006
integer,parameter :: pm_num_md_julien_calend    = 1007
integer,parameter :: pm_num_md_duree_jhms       = 1008
integer,parameter :: pm_num_mu_angle3           = 1009
integer,parameter :: pm_num_mu_prod_vect        = 1010
integer,parameter :: pm_num_mu_norme            = 1011
integer,parameter :: pm_num_me_brouwer_moy      = 1012
integer,parameter :: pm_num_me_brouwer          = 1013
integer,parameter :: pm_num_mv_kepler_std       = 1014
integer,parameter :: pm_num_mc_test             = 1015
integer,parameter :: pm_num_mc_math             = 1016
integer,parameter :: pm_num_mc_phys             = 1017
integer,parameter :: pm_num_mc_GRS1980          = 1018
integer,parameter :: pm_num_mo_geo_qsw          = 1019
integer,parameter :: pm_num_mo_qsw_geo          = 1020
integer,parameter :: pm_num_mo_geo_tnw          = 1021
integer,parameter :: pm_num_mo_tnw_geo          = 1022
integer,parameter :: pm_num_mv_kepler_gene      = 1023
integer,parameter :: pm_num_mv_kepler_bar       = 1024
integer,parameter :: pm_num_mvi_kepler_hyperb   = 1025
integer,parameter :: pm_num_mvi_barker          = 1026
integer,parameter :: pm_num_mv_car_kep          = 1027
integer,parameter :: pm_num_mvi_car_kep_ellip   = 1028
integer,parameter :: pm_num_mvi_car_kep_hyperb  = 1029
integer,parameter :: pm_num_mvi_car_kep_parab   = 1030
integer,parameter :: pm_num_mv_kep_car          = 1031
integer,parameter :: pm_num_mvi_kep_car_ellip   = 1032
integer,parameter :: pm_num_mvi_kep_car_hyperb  = 1033
integer,parameter :: pm_num_mvi_kep_car_parab   = 1034
integer,parameter :: pm_num_mv_car_cir          = 1035
integer,parameter :: pm_num_mv_cir_car          = 1036
integer,parameter :: pm_num_mv_car_equa         = 1037
integer,parameter :: pm_num_mvi_car_equa_ellip  = 1038
integer,parameter :: pm_num_mvi_car_equa_hyperb = 1039
integer,parameter :: pm_num_mvi_car_equa_parab  = 1040
integer,parameter :: pm_num_mv_equa_car         = 1041
integer,parameter :: pm_num_mvi_equa_car_ellip  = 1042
integer,parameter :: pm_num_mvi_equa_car_hyperb = 1043
integer,parameter :: pm_num_mvi_equa_car_parab  = 1044
integer,parameter :: pm_num_mv_car_cir_equa     = 1045
integer,parameter :: pm_num_mv_cir_equa_car     = 1046
integer,parameter :: pm_num_me_deriv_secul_j2   = 1047
integer,parameter :: pm_num_mt_car_geod         = 1048
integer,parameter :: pm_num_mt_geod_car         = 1049
integer,parameter :: pm_num_mt_car_geoc         = 1050
integer,parameter :: pm_num_mt_geoc_car         = 1051
integer,parameter :: pm_num_mt_def_topo_N       = 1052
integer,parameter :: pm_num_mt_ref_topo_N       = 1053
integer,parameter :: pm_num_mt_topo_N_ref       = 1054
integer,parameter :: pm_num_mt_ref_iner         = 1055
integer,parameter :: pm_num_mt_iner_ref         = 1056
integer,parameter :: pm_num_mt_topo_car_E_N     = 1057
integer,parameter :: pm_num_mt_topo_car_N_E     = 1058
integer,parameter :: pm_num_mt_topo_sgd_E_N     = 1059
integer,parameter :: pm_num_mt_topo_sgd_N_E     = 1060
integer,parameter :: pm_num_mt_topo_E_sgd_car   = 1061
integer,parameter :: pm_num_mt_topo_N_car_sgd   = 1062
integer,parameter :: pm_num_mt_topo_N_sgd_car   = 1063
integer,parameter :: pm_num_mr_rep_fon          = 1064
integer,parameter :: pm_num_mri_equa_moy_t1_t2  = 1065
integer,parameter :: pm_num_mri_equa_M_equa_V   = 1066
integer,parameter :: pm_num_mri_equa_M_eclip_M  = 1067
integer,parameter :: pm_num_mri_equa_M_eclip_V  = 1068
integer,parameter :: pm_num_mri_equa_V_equa_M   = 1069
integer,parameter :: pm_num_mri_equa_vrai_t1_t2 = 1070
integer,parameter :: pm_num_mri_equa_V_eclip_M  = 1071
integer,parameter :: pm_num_mri_equa_V_eclip_V  = 1072
integer,parameter :: pm_num_mri_eclip_M_equa_M  = 1073
integer,parameter :: pm_num_mri_eclip_M_equa_V  = 1074
integer,parameter :: pm_num_mri_eclip_moy_t1_t2 = 1075
integer,parameter :: pm_num_mri_eclip_M_eclip_V = 1076
integer,parameter :: pm_num_mri_eclip_V_equa_M  = 1077
integer,parameter :: pm_num_mri_eclip_V_equa_V  = 1078
integer,parameter :: pm_num_mri_eclip_V_eclip_M = 1079
integer,parameter :: pm_num_mri_eclip_vrai_t1_t2= 1080
integer,parameter :: pm_num_mr_nuta             = 1081
integer,parameter :: pm_num_mri_arg_fon         = 1082
integer,parameter :: pm_num_mr_obli_moy         = 1083
integer,parameter :: pm_num_mr_mat_nuta         = 1084
integer,parameter :: pm_num_mr_tsid_veis        = 1085
integer,parameter :: pm_num_mr_tsid_aoki        = 1086
integer,parameter :: pm_num_mp_atm_US76         = 1087
integer,parameter :: pm_num_mdi_som_diff_joursec= 1088
integer,parameter :: pm_num_md_joursec_norme    = 1089
integer,parameter :: pm_num_md_jourfrac_joursec = 1090
integer,parameter :: pm_num_md_joursec_jourfrac = 1091
integer,parameter :: pm_num_md_comp_joursec     = 1092
integer,parameter :: pm_num_mo_def_qsw          = 1093
integer,parameter :: pm_num_mo_def_tnw          = 1094
integer,parameter :: pm_num_mm_impul_car        = 1095
integer,parameter :: pm_num_mm_impul_kep        = 1096
integer,parameter :: pm_num_me_eck_hech         = 1097
integer,parameter :: pm_num_me_eck_hech_moy     = 1098
integer,parameter :: pm_num_mr_TerVrai_TerRef   = 1099  
integer,parameter :: pm_num_mr_TerRef_TerVrai   = 1100  
integer,parameter :: pm_num_mr_TerVrai_veis     = 1101  
integer,parameter :: pm_num_mr_veis_TerVrai     = 1102  
integer,parameter :: pm_num_mr_veis_J2000       = 1103  
integer,parameter :: pm_num_mr_J2000_veis       = 1104
integer,parameter :: pm_num_mr_EquaMoy_J2000    = 1105
integer,parameter :: pm_num_mr_J2000_EquaMoy    = 1106
integer,parameter :: pm_num_mr_veis_EquaVrai    = 1107
integer,parameter :: pm_num_mr_EquaVrai_veis    = 1108  
! m-de-1109-469-CIS  integer,parameter :: pm_num_mri_EquaVrai_EquaMoy= 1109
! m-de-1110-468-CIS  integer,parameter :: pm_num_mri_EquaMoy_EquaVrai= 1110
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_EMoy_EVrai  = 1111
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_EMoy_J2000  = 1112
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_EVrai_EMoy  = 1113
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_J2000_EMoy  = 1114
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_TRef_TVrai  = 1115
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_TVrai_TRef  = 1116
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_TVrai_veis  = 1117
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_prec2000    = 1118
! m-de-1119-464-CIS  integer,parameter :: pm_num_mri_mat_veis_EVrai  = 1119
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_veis_TVrai  = 1120
! m-de-1121-470-CIS  integer,parameter :: pm_num_mri_tsid_vrai       = 1121
! m-de-1122-465-CIS  integer,parameter :: pm_num_mri_mat_EVrai_veis  = 1122
integer,parameter :: pm_num_mu_axe_angle_quat   = 1123
integer,parameter :: pm_num_mu_quat_axe_angle   = 1124
integer,parameter :: pm_num_mu_quat_norme       = 1125
integer,parameter :: pm_num_mu_quat_conjug      = 1126
integer,parameter :: pm_num_mu_prod_quat        = 1127
integer,parameter :: pm_num_mu_quat_rep         = 1128
integer,parameter :: pm_num_mr_EquaVrai_TerVrai = 1129
integer,parameter :: pm_num_mr_TerVrai_EquaVrai = 1130
integer,parameter :: pm_num_mr_TerVrai_J2000    = 1131
integer,parameter :: pm_num_mr_J2000_TerVrai    = 1132
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_EVrai_TVrai = 1133
! m-de-1000-1-CIS    integer,parameter :: pm_num_mri_mat_TVrai_EVrai = 1134
integer,parameter :: pm_num_mr_EquaVrai_EquaMoy = 1135
integer,parameter :: pm_num_mr_EquaMoy_EquaVrai = 1136
integer,parameter :: pm_num_mr_tsid_vrai        = 1137
integer,parameter :: pm_num_mr_J2000_EcliJ2000  = 1138
integer,parameter :: pm_num_mr_EcliJ2000_J2000  = 1139
integer,parameter :: pm_num_mr_J2000_EquaUAI    = 1140
integer,parameter :: pm_num_mr_EquaUAI_J2000    = 1141
integer,parameter :: pm_num_mri_def_rep_UAI     = 1142
integer,parameter :: pm_num_mr_PlanetVrai_EquaUAI = 1143
integer,parameter :: pm_num_mr_EquaUAI_PlanetVrai = 1144
integer,parameter :: pm_num_mu_3rot_quat        = 1145
integer,parameter :: pm_num_mui_axe_norme_quat  = 1146
integer,parameter :: pm_num_mu_quat_3rot        = 1147
integer,parameter :: pm_num_mu_quat_mat         = 1148
integer,parameter :: pm_num_mu_mat_quat         = 1149
integer,parameter :: pm_num_mr_PlaVrai_PlaIner  = 1150
integer,parameter :: pm_num_mr_PlaIner_PlaVrai  = 1151
integer,parameter :: pm_num_me_lyddane          = 1152
integer,parameter :: pm_num_me_lyddane_moy      = 1153
integer,parameter :: pm_num_mv_kep_cir_equa     = 1154
integer,parameter :: pm_num_mv_cir_equa_kep     = 1155
integer,parameter :: pm_num_mv_cir_kep          = 1156
integer,parameter :: pm_num_mv_kep_cir          = 1157
integer,parameter :: pm_num_mv_kep_equa         = 1158
integer,parameter :: pm_num_mv_equa_kep         = 1159
integer,parameter :: pm_num_mu_compar_rot_quat  = 1160
integer,parameter :: pm_num_ms_pos_soleil_lune  = 1161
integer,parameter :: pm_num_mv_conv_anom        = 1162
integer,parameter :: pm_num_mvi_conv_anom_ellip = 1163
integer,parameter :: pm_num_mvi_conv_anom_hyperb= 1164
integer,parameter :: pm_num_mvi_conv_anom_parab = 1165
integer,parameter :: pm_num_mr_J2000_BBR        = 1166
integer,parameter :: pm_num_mr_mat_J2000_BBR    = 1167
integer,parameter :: pm_num_mi_pb_deux_corps    = 1168
integer,parameter :: pm_num_mi_pb_lambert0      = 1169
integer,parameter :: pm_num_mi_pb_lambert1      = 1170
integer,parameter :: pm_num_mu_racine           = 1171
integer,parameter :: pm_num_mui_dot_product3    = 1172
integer,parameter :: pm_num_mui_dot_product4    = 1173
integer,parameter :: pm_num_mui_dot_product5    = 1174
integer,parameter :: pm_num_mui_dot_product6    = 1175
integer,parameter :: pm_num_mi_pro_j2           = 1176
integer,parameter :: pm_num_mr_prec             = 1177
integer,parameter :: pm_num_mc_positionner_wahr5= 1178
integer,parameter :: pm_num_mc_recuperer_wahr5  = 1179
integer,parameter :: pm_num_mu_matmul3          = 1180
integer,parameter :: pm_num_mu_matmul6          = 1181
integer,parameter :: pm_num_mu_transpose3       = 1182
integer,parameter :: pm_num_mu_mulvect3         = 1183
integer,parameter :: pm_num_mu_mulvecttrans3    = 1184
integer,parameter :: pm_num_mr_s_prime_iers2003 = 1185
integer,parameter :: pm_num_mri_R1_iers2003     = 1186
integer,parameter :: pm_num_mr_TerRef_TerVrai_iers = 1187
integer,parameter :: pm_num_mri_R2_iers2003     = 1188
integer,parameter :: pm_num_mri_R3_iers2003     = 1189
integer,parameter :: pm_num_mr_TerVrai_TerRef_iers = 1190
integer,parameter :: pm_num_mr_TerVrai_CIP_iers = 1191
integer,parameter :: pm_num_mr_CIP_TerVrai_iers = 1192
integer,parameter :: pm_num_mri_XYS_iers2003    = 1193
integer,parameter :: pm_num_mr_CIP_EME2000_iers = 1194
integer,parameter :: pm_num_mr_EME2000_CIP_iers = 1195
integer,parameter :: pm_num_mr_TerRef_EME2000_iers = 1196
integer,parameter :: pm_num_mr_EME2000_TerRef_iers = 1197

! Le classement se fait par ordre numerique.
! Tout nouveau module prend le numero en suivant 
! (afin d'eviter des trous ou des doublons de numerotation).

!   Liste des modules             Numeros
!-----------------------------------
!   mslib                         2001
!   type_mslib                    2002
!   parametre_mslib               2003
!   longueur_chaine_mslib         2004
!   interface_mslib               2005
!   valeur_code_retour_mslib      2006
!   numero_routine_mslib          2007
!   math_mslib                    2008 (supprimé)
!   precision_mslib               2009
!   modules communs aux programmes de tests:        2010  format_test_mslib
!   modules communs aux programmes de comparaisons: 2011  format_compar_mslib
!   phys_mslib                    2012 (supprimé)
!   GRS1980_mslib                 2013 (supprimé)
!   test_mslib                    2014 
!   constantes_terre_mslib        2015 (supprimé)
!   code_transfo_mslib            2016 (supprimé)
!   code_modeles_mslib            2017 (supprimé)
!   indic_reperes_mslib           2018 (supprimé)
!   indic_comp_joursec_mslib      2019 (supprimé)
!   parametres_internes_mslib     2020
!   code_planetes_mslib           2021 (supprimé)
!   flags_mslib                   2022

!................................................................................................................

character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB numero_routine_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

character(len=pm_longueur_rcs_id), private, parameter :: rcs_id = &
     ' $Id: numero_routine_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module numero_routine_mslib

