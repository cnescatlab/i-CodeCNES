module mwp_orb_hpha_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant une TM sous la forme hp,ha,i,pom,gom,M        
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
!   + Version 3.1 : creation
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
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

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: mwp_orb_hpha_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_orb_hpha_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_orb_hpha_2_orb_hpha

   module procedure mwpi_tab6_2_orb_hpha
   module procedure mwpi_orb_hpha_2_tab6

end interface

CONTAINS

subroutine mwpi_orb_hpha_2_orb_hpha (coord_orb_hpha_out, coord_orb_hpha_in)

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  Transfert de coordonnees tm_orb_hpha vers des coordonnees tm_orb_hpha
! ===   (ceci evite des transferts implicites)
!
! Note d'utilisation: 
! ==================
!
!$Historique
! ==========
!   + Version 3.1 : creation
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

type(tm_orb_hpha), intent(out) :: coord_orb_hpha_out   ! resultat
type(tm_orb_hpha), intent(in)  :: coord_orb_hpha_in    ! operande

! Corps du sous programme
! =======================

coord_orb_hpha_out%hp  = coord_orb_hpha_in%hp
coord_orb_hpha_out%ha  = coord_orb_hpha_in%ha
coord_orb_hpha_out%i   = coord_orb_hpha_in%i
coord_orb_hpha_out%pom = coord_orb_hpha_in%pom
coord_orb_hpha_out%gom = coord_orb_hpha_in%gom
coord_orb_hpha_out%M   = coord_orb_hpha_in%M

end subroutine mwpi_orb_hpha_2_orb_hpha

subroutine mwpi_tab6_2_orb_hpha (coord_orb_hpha, coord_tab6)

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  Transfert des coordonnees contenues dans un tableau coord_tab6
! ===   de dimension 6 vers des coordonnees tm_orb_hpha .
!
! Note d'utilisation: 
! ==================
!
!$Historique
! ==========
!   + Version 3.1 : creation
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

type(tm_orb_hpha), intent(out)            :: coord_orb_hpha  ! resultat
real(pm_reel), dimension(6), intent(in)   :: coord_tab6      ! operande

! Corps du sous programme
! =======================

coord_orb_hpha%hp  = coord_tab6(1) 
coord_orb_hpha%ha  = coord_tab6(2) 
coord_orb_hpha%i   = coord_tab6(3) 
coord_orb_hpha%pom = coord_tab6(4) 
coord_orb_hpha%gom = coord_tab6(5) 
coord_orb_hpha%M   = coord_tab6(6) 

end subroutine mwpi_tab6_2_orb_hpha

subroutine mwpi_orb_hpha_2_tab6 (coord_tab6, coord_orb_hpha)

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  Transfert des coordonnees tm_orb_hpha en tableau coord_tab6 de 
! ===   coordonnees de dimension 6.                                     
!
! Note d'utilisation:
! ==================
!
!$Historique
! ==========
!   + Version 3.1 : creation
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

real(pm_reel), dimension(6), intent(out) :: coord_tab6      ! resultat
type(tm_orb_hpha), intent(in)            :: coord_orb_hpha  ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_orb_hpha%hp
coord_tab6(2) = coord_orb_hpha%ha
coord_tab6(3) = coord_orb_hpha%i
coord_tab6(4) = coord_orb_hpha%pom
coord_tab6(5) = coord_orb_hpha%gom
coord_tab6(6) = coord_orb_hpha%M

end subroutine mwpi_orb_hpha_2_tab6

end module mwp_orb_hpha_pos_vit_mspro
