module mwp_def_topo_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
! ===   donnees concernant la definition du repere topocentrique
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Ces procedures possedent deux arguments obligatoires
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

use mslib              ! acces a la librairie MSLIB
use type_mspro         ! acces aux types utilisateurs de la librairie MSPRO
use type_themeX_interne_mspro ! acces en interne aux types du theme X

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: mwp_def_topo_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_def_topo_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge 

interface assignment (=)

   module procedure mwpi_def_topo_2_def_topo

end interface

CONTAINS

subroutine mwpi_def_topo_2_def_topo (def_topo_out, def_topo_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de definition d'un repere topocentrique vers un autre
!       repere topocentrique a partir de structures de type tm_def_topo.
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

type(tm_def_topo), intent(out) :: def_topo_out     ! resultat
type(tm_def_topo), intent(in)  :: def_topo_in      ! operande

! Corps du sous programme
! =======================

def_topo_out%geod   = def_topo_in%geod
def_topo_out%ellips = def_topo_in%ellips
def_topo_out%axe_x  = def_topo_in%axe_x

end subroutine mwpi_def_topo_2_def_topo

end module mwp_def_topo_mspro
