subroutine mxi_rep_cons_arbre ( arbre, retour )

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  construction de l'arbre des routines de changement de repere
! ===
!
! Note d'utilisation: La variable arbre est declaree dans mx_rep 
! ==================  en save, donc sauvegardee au cours des appels successifs
!                     a cette routine
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 9) : ajout des reperes interplanetaires dans le theme X
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE 2 mx_rep) : revision de l'arbre (redefinition du Terrestre Reference Inertiel)
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (DE 3 mx_rep) : passage du repere Terrestre Reference Inertiel dans une autre branche
!                         (Date: 08/2004 - Realisation: Bruno Revelin)
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

type(tm_i_noeud), dimension(:), intent(out)                       ::  arbre   ! arbre
integer, intent(out)                                              ::  retour  ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: retour_local
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_rep_cons_arbre.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! initialisations des champs 'peres' de chaque noeud de l'arbre

arbre(pm_i_EME2000)%pere = pm_i_rien

! Branche Terre1
arbre(pm_i_equa_moy_Terre1)%pere = pm_i_EME2000
arbre(pm_i_equa_vrai_Terre1)%pere = pm_i_equa_moy_Terre1
arbre(pm_i_terre_vrai_Terre1)%pere = pm_i_equa_vrai_Terre1
arbre(pm_i_veis_Terre1)%pere = pm_i_terre_vrai_Terre1
arbre(pm_i_terre_ref_long_nul_Terre1)%pere = pm_i_terre_vrai_Terre1
arbre(pm_i_terre_ref_long_Cin_Terre1)%pere = pm_i_terre_ref_long_nul_Terre1
arbre(pm_i_terre_ref_long_Cout_Terre1)%pere = pm_i_terre_ref_long_nul_Terre1
arbre(pm_i_topo_Cin_Terre1)%pere = pm_i_terre_ref_long_nul_Terre1
arbre(pm_i_topo_Cout_Terre1)%pere = pm_i_terre_ref_long_nul_Terre1

! Branche Terre2
arbre(pm_i_equa_moy_Terre2)%pere = pm_i_EME2000
arbre(pm_i_equa_vrai_Terre2)%pere = pm_i_equa_moy_Terre2
arbre(pm_i_terre_vrai_Terre2)%pere = pm_i_equa_vrai_Terre2
arbre(pm_i_veis_Terre2)%pere = pm_i_terre_vrai_Terre2
arbre(pm_i_terre_ref_long_nul_Terre2)%pere = pm_i_terre_vrai_Terre2
arbre(pm_i_terre_ref_long_Cout_Terre2)%pere = pm_i_terre_ref_long_nul_Terre2
arbre(pm_i_topo_Cout_Terre2)%pere = pm_i_terre_ref_long_nul_Terre2

! Branche Ecli 2000
arbre(pm_i_ecli_2000)%pere = pm_i_EME2000

! Branche Terre Ref Iner
arbre(pm_i_terre_ref_iner_Cin)%pere = pm_i_EME2000
arbre(pm_i_terre_ref_iner_Cout)%pere = pm_i_EME2000

! Branche Planete1
arbre(pm_i_equa_uai_Planete1)%pere = pm_i_EME2000
arbre(pm_i_planeto_ref_Planete1)%pere = pm_i_equa_uai_Planete1
arbre(pm_i_topo_Cin_Planete1)%pere = pm_i_planeto_ref_Planete1
arbre(pm_i_topo_Cout_Planete1)%pere = pm_i_planeto_ref_Planete1
arbre(pm_i_planeto_iner_Cin_Planete1)%pere = pm_i_planeto_ref_Planete1
arbre(pm_i_planeto_iner_Cout_Planete1)%pere = pm_i_planeto_ref_Planete1

! Branche Planete2
arbre(pm_i_equa_uai_Planete2)%pere = pm_i_EME2000
arbre(pm_i_planeto_ref_Planete2)%pere = pm_i_equa_uai_Planete2
arbre(pm_i_topo_Cout_Planete2)%pere = pm_i_planeto_ref_Planete2
arbre(pm_i_planeto_iner_Cout_Planete2)%pere = pm_i_planeto_ref_Planete2

! calcul des fils en fonction des peres definis ci-dessus

call mxi_construit_fils( pm_i_nombre_repere, arbre, retour_local )
retour = retour_local

end subroutine mxi_rep_cons_arbre
