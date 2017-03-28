module mwp_para_opt_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant le type tm_i_rep_para_opt
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Ces procedures possedent deux arguments obligatoires :
!                     Le premier , du genre intent(out) correspond
!                     a celui qui apparait a gauche du signe "=",
!                     Le second , du genre intent(in) correspond a celui
!                     qui apparait a droite du signe "=".
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Michel Lagreca)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

use mslib
use type_mspro
use type_themeX_interne_mspro ! acces en interne aux types du theme X

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: mwp_para_opt_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_para_opt_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge de l'egalite

interface assignment (=)

   module procedure mwpi_rep_para_opt_2_para_opt

end interface

CONTAINS

subroutine mwpi_rep_para_opt_2_para_opt (para_opt_out, para_opt_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de definition d'un repere topocentrique vers un autre
!       repere topocentrique a partir de structures de type tm_i_rep_para_opt.
! ===   (ceci evite des transferts implicites)
!
! Note d'utilisation: 
! ==================
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Michel Lagreca)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

type(tm_i_rep_para_opt), intent(out) :: para_opt_out     ! resultat
type(tm_i_rep_para_opt), intent(in)  :: para_opt_in      ! operande

! Corps du sous programme
! =======================

! vitesse de rotation de la planete
para_opt_out%vit_rot%presence               = para_opt_in%vit_rot%presence
para_opt_out%vit_rot%superflu               = para_opt_in%vit_rot%superflu
para_opt_out%vit_rot%valeur                 = para_opt_in%vit_rot%valeur

! obliquite
para_opt_out%obliquite%presence             = para_opt_in%obliquite%presence
para_opt_out%obliquite%superflu             = para_opt_in%obliquite%superflu
para_opt_out%obliquite%valeur               = para_opt_in%obliquite%valeur

! coordonnes du pole vrai
para_opt_out%pole%presence                  = para_opt_in%pole%presence
para_opt_out%pole%superflu                  = para_opt_in%pole%superflu
para_opt_out%pole%valeur                    = para_opt_in%pole%valeur

! longitude de reference
para_opt_out%long_ref%presence              = para_opt_in%long_ref%presence
para_opt_out%long_ref%superflu              = para_opt_in%long_ref%superflu
para_opt_out%long_ref%valeur                = para_opt_in%long_ref%valeur
para_opt_out%long_ref%long_nul              = para_opt_in%long_ref%long_nul

! valeur de la date
para_opt_out%val_date%presence              = para_opt_in%val_date%presence
para_opt_out%val_date%superflu              = para_opt_in%val_date%superflu
para_opt_out%val_date%valeur                = para_opt_in%val_date%valeur

! ecart entre echelle de temps TU1 et utilisateur
para_opt_out%delta_tu1%presence             = para_opt_in%delta_tu1%presence
para_opt_out%delta_tu1%superflu             = para_opt_in%delta_tu1%superflu
para_opt_out%delta_tu1%valeur               = para_opt_in%delta_tu1%valeur

! ecart entre echelle de temps TAI et utilisateur
para_opt_out%delta_tai%presence             = para_opt_in%delta_tai%presence
para_opt_out%delta_tai%superflu             = para_opt_in%delta_tai%superflu
para_opt_out%delta_tai%valeur               = para_opt_in%delta_tai%valeur

! epsilon pour comparaison de 2 dates
para_opt_out%eps_date%presence              = para_opt_in%eps_date%presence
para_opt_out%eps_date%superflu              = para_opt_in%eps_date%superflu
para_opt_out%eps_date%valeur                = para_opt_in%eps_date%valeur

! definition du repere topocentrique
para_opt_out%def_topo%presence              = para_opt_in%def_topo%presence
para_opt_out%def_topo%superflu              = para_opt_in%def_topo%superflu
para_opt_out%def_topo%valeur                = para_opt_in%def_topo%valeur
para_opt_out%def_topo%axe_nul               = para_opt_in%def_topo%axe_nul

end subroutine mwpi_rep_para_opt_2_para_opt

end module mwp_para_opt_mspro
