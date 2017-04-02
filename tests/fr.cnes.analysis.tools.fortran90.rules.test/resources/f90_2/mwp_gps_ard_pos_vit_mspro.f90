module mwp_gps_ard_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant le recepteur GPS de la capsule spatiale ARD 
!       en position et vitesse
!
! Note d'utilisation: accessible via les modules de surcharge 
! ==================  Cette procedure possedent deux arguments obligatoires :
!                     Le premier , du genre intent(out) correspond
!                     a celui qui apparait a gauche du signe "=",
!                     Le second , du genre intent(in) correspond a celui
!                     qui apparait a droite du signe "=".!
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_gps_ard_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_gps_ard_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_gps_ard_2_gps_ard
   module procedure mwpi_tab6_2_gps_ard
   module procedure mwpi_gps_ard_2_tab6

end interface

CONTAINS

subroutine mwpi_gps_ard_2_gps_ard (coord_gps_ard_out, coord_gps_ard_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_i_gps_ard_pos_vit vers des coordonnees tm_i_gps_ard_pos_vit
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

type(tm_i_gps_ard_pos_vit), intent(out) :: coord_gps_ard_out    ! resultat
type(tm_i_gps_ard_pos_vit), intent(in)  :: coord_gps_ard_in     ! operande

! Corps du sous programme
! =======================

coord_gps_ard_out%pos = coord_gps_ard_in%pos
coord_gps_ard_out%vit = coord_gps_ard_in%vit

end subroutine mwpi_gps_ard_2_gps_ard

subroutine mwpi_tab6_2_gps_ard (coord_gps_ard, coord_tab6)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees (positions et vitesses)
! ===   contenues dans un tableau coord_tab6 de dimension 6 vers des 
!       coordonnees de type tm_i_gps_ard_pos_vit .
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

type(tm_i_gps_ard_pos_vit), intent(out)  :: coord_gps_ard   ! resultat
real(pm_reel), dimension(6), intent(in)  :: coord_tab6      ! operande

! Corps du sous programme
! =======================

coord_gps_ard%pos%lat  = coord_tab6(1) 
coord_gps_ard%pos%long = coord_tab6(2)
coord_gps_ard%pos%haut = coord_tab6(3)

coord_gps_ard%vit%sn = coord_tab6(4) 
coord_gps_ard%vit%oe = coord_tab6(5)
coord_gps_ard%vit%hb = coord_tab6(6)

end subroutine mwpi_tab6_2_gps_ard

subroutine mwpi_gps_ard_2_tab6 (coord_tab6, coord_gps_ard)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees tm_i_gps_ard_pos_vit en tableau coord_tab6 de 
! ===   coordonnees de dimension 6 (positions et vitesse)  
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

real(pm_reel), dimension(6), intent(out) :: coord_tab6      ! resultat
type(tm_i_gps_ard_pos_vit), intent(in)   :: coord_gps_ard   ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_gps_ard%pos%lat
coord_tab6(2) = coord_gps_ard%pos%long
coord_tab6(3) = coord_gps_ard%pos%haut

coord_tab6(4) = coord_gps_ard%vit%sn
coord_tab6(5) = coord_gps_ard%vit%oe
coord_tab6(6) = coord_gps_ard%vit%hb

end subroutine mwpi_gps_ard_2_tab6

end module mwp_gps_ard_pos_vit_mspro
