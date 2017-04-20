module parametre_themeX_mspro

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Definition des parametres MSPRO du theme X (hors numeros de routines et de codes retour)
! ===   
!
! Note d'utilisation: 
! ==================  
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Bruno Revelin et Michel Lagreca)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE 1) : transfert de parametres (planetes, UAI1994, obliquite) vers la MSLIB90
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 72 : Coordonnees hyperboliques dans l'objet bulletin de la GSLIB
!                              (Date: 09/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)

!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: parametre_themeX_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


!........................................................................................
! parametres associes a mx_rep
!........................................................................................

! valeurs possibles pour bv_pla_in et bv_pla_out

integer, parameter         :: pm_mx_pla_min = pm_pla_mercure
integer, parameter         :: pm_mx_pla_max = pm_pla_pluton
integer, parameter         :: pm_mx_pla_nb = 9

! valeurs possibles pour tbv_rep_in et tbv_rep_out

integer, parameter         :: pm_mx_rep_min = pm_mx_pla_max + 1
integer, parameter         :: pm_equa_vrai = pm_mx_rep_min
integer, parameter         :: pm_equa_moy = pm_equa_vrai + 1
integer, parameter         :: pm_planeto_vrai = pm_equa_moy + 1
integer, parameter         :: pm_planeto_ref = pm_planeto_vrai + 1
integer, parameter         :: pm_planeto_ref_iner = pm_planeto_ref + 1
integer, parameter         :: pm_topo = pm_planeto_ref_iner + 1
integer, parameter         :: pm_equa_uai = pm_topo + 1
integer, parameter         :: pm_veis = pm_equa_uai + 1
integer, parameter         :: pm_ecli_moy = pm_veis + 1
integer, parameter         :: pm_mx_rep_max = pm_ecli_moy

! valeurs possibles pour prec_nuta_tsid_in et prec_nuta_tsid_out

integer, parameter         :: pm_mx_prec_nuta_tsid_min = pm_mx_rep_max + 50
integer, parameter         :: pm_lieske_wahr_aoki = pm_mx_prec_nuta_tsid_min
! pour les modeles UAI (pm_UAI94, ....):
! cf MSLIB90: on utilise les noms des modeles UAI definis dans la MSLIB90
integer, parameter         :: pm_mx_prec_nuta_tsid_max = pm_lieske_wahr_aoki

! valeurs possibles pour date_in et date_out

integer, parameter         :: pm_mx_date_min = pm_mx_prec_nuta_tsid_max + 50
integer, parameter         :: pm_1janvier1950_00h00 = pm_mx_date_min
integer, parameter         :: pm_1janvier2000_12h00 = pm_1janvier1950_00h00 + 1
integer, parameter         :: pm_autre_date = pm_1janvier2000_12h00 + 1
integer, parameter         :: pm_mx_date_max = pm_autre_date

!........................................................................................
! parametres associes a mx_var
!........................................................................................

! valeurs possibles pour typ_var_in et typ_var_out

integer, parameter         :: pm_mx_type_var_min = pm_mx_date_max + 50
integer, parameter         :: pm_mx_type_var_interv0 = pm_mx_type_var_min
integer, parameter         :: pm_Vinf = pm_mx_type_var_interv0
integer, parameter         :: pm_Vinfarr = pm_Vinf + 1
integer, parameter         :: pm_hpha = pm_Vinfarr + 1
integer, parameter         :: pm_kep = pm_hpha + 1
integer, parameter         :: pm_cir = pm_kep + 1
integer, parameter         :: pm_equa = pm_cir + 1
integer, parameter         :: pm_cir_equa = pm_equa + 1

integer, parameter         :: pm_mx_type_var_interv1 = pm_cir_equa + 1
integer, parameter         :: pm_geoc = pm_mx_type_var_interv1
integer, parameter         :: pm_geod_meca_vol = pm_geoc + 1
integer, parameter         :: pm_geod_gps_ard = pm_geod_meca_vol + 1

integer, parameter         :: pm_mx_type_var_interv2 = pm_geod_gps_ard + 1
integer, parameter         :: pm_sgd_nord = pm_mx_type_var_interv2

integer, parameter         :: pm_mx_type_var_interv3 = pm_sgd_nord + 1
integer, parameter         :: pm_sgd_est = pm_mx_type_var_interv3

integer, parameter         :: pm_mx_type_var_interv4 = pm_sgd_est + 1
integer, parameter         :: pm_car = pm_mx_type_var_interv4
integer, parameter         :: pm_mx_type_var_max = pm_car 
!...............................................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                  '@(#) Fichier MSPRO parametre_themeX_mspro.f90: derniere modification V5.15 >'

!................................................................................................................

end module parametre_themeX_mspro
