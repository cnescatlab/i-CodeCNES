module mwp_geod_pos_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
! ===   donnees concernant des coordonnees geodesiques
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Cets procedures possedent deux arguments obligatoires :
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_geod_pos_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_geod_pos_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_geod_2_geod

   module procedure mwpi_tab3_2_geod
   module procedure mwpi_geod_2_tab3

end interface

CONTAINS

subroutine mwpi_geod_2_geod (coord_geod_out, coord_geod_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_geodesique vers des coordonnees tm_geodesique
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

type(tm_geodesique), intent(out) :: coord_geod_out   ! resultat
type(tm_geodesique), intent(in)  :: coord_geod_in    ! operande

! Corps du sous programme
! =======================

coord_geod_out%lat  = coord_geod_in%lat
coord_geod_out%long = coord_geod_in%long
coord_geod_out%haut = coord_geod_in%haut

end subroutine mwpi_geod_2_geod

subroutine mwpi_tab3_2_geod (coord_geod, coord_tab3)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees (positions ou vitesses)
! ===   contenues dans un tableau coord_tab3 de dimension 3 vers des 
!       coordonnees de type tm_geodesique .
!       Cette methode est valable pour des positions  ou des vitesses.
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

type(tm_geodesique), intent(out)         :: coord_geod   ! resultat
real(pm_reel), dimension(3), intent(in)  :: coord_tab3   ! operande

! Corps du sous programme
! =======================

coord_geod%lat  = coord_tab3(1)
coord_geod%long = coord_tab3(2)
coord_geod%haut = coord_tab3(3)

end subroutine mwpi_tab3_2_geod

subroutine mwpi_geod_2_tab3 (coord_tab3, coord_geod)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees tm_geodesique en tableau coord_tab3 de 
! ===   coordonnees de dimension 3(positions ou vitesse)  
!       Cette methode est valable pour des positions ou des vitesses.
!       ou des vitesses.
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

real(pm_reel), dimension(3), intent(out) :: coord_tab3   ! resultat
type(tm_geodesique), intent(in)          :: coord_geod   ! operande

! Corps du sous programme
! =======================

coord_tab3(1) = coord_geod%lat
coord_tab3(2) = coord_geod%long
coord_tab3(3) = coord_geod%haut

end subroutine mwpi_geod_2_tab3

end module mwp_geod_pos_mspro
