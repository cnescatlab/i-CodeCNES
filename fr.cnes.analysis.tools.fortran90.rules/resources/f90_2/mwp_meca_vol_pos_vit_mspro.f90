module mwp_meca_vol_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant la mecanique du vol en position et vitesse
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

use mslib              ! acces a la librairie MSLIB
use type_mspro         ! acces aux types utilisateurs de la librairie MSPRO
use type_themeX_interne_mspro ! acces en interne aux types du theme X

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: mwp_meca_vol_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_meca_vol_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_meca_vol_2_meca_vol

   module procedure mwpi_tab6_2_meca_vol
   module procedure mwpi_meca_vol_2_tab6

end interface

CONTAINS

subroutine mwpi_meca_vol_2_meca_vol (coord_meca_vol_out, coord_meca_vol_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_i_meca_vol_pos_vit vers des coordonnees tm_i_meca_vol_pos_vit
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

type(tm_i_meca_vol_pos_vit), intent(out) :: coord_meca_vol_out ! resultat
type(tm_i_meca_vol_pos_vit), intent(in)  :: coord_meca_vol_in  ! operande

! Corps du sous programme
! =======================

coord_meca_vol_out%pos = coord_meca_vol_in%pos
coord_meca_vol_out%vit = coord_meca_vol_in%vit

end subroutine mwpi_meca_vol_2_meca_vol

subroutine mwpi_tab6_2_meca_vol (coord_meca_vol, coord_tab6)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees (positions et vitesses)
! ===   contenues dans un tableau coord_tab6 de dimension 6 vers des 
!       coordonnees de type tm_i_meca_vol_pos_vit .
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

type(tm_i_meca_vol_pos_vit), intent(out)  :: coord_meca_vol  ! resultat
real(pm_reel), dimension(6), intent(in)   :: coord_tab6      ! operande

! Corps du sous programme
! =======================

coord_meca_vol%pos%lat    = coord_tab6(1)
coord_meca_vol%pos%long   = coord_tab6(2)
coord_meca_vol%pos%haut   = coord_tab6(3)
coord_meca_vol%vit%pente  = coord_tab6(4)
coord_meca_vol%vit%azimut = coord_tab6(5)
coord_meca_vol%vit%norme  = coord_tab6(6)

end subroutine mwpi_tab6_2_meca_vol

subroutine mwpi_meca_vol_2_tab6 (coord_tab6, coord_meca_vol)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees tm_i_meca_vol_pos_vit en tableau coord_tab6 de 
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

real(pm_reel), dimension(6), intent(out)  :: coord_tab6     ! resultat
type(tm_i_meca_vol_pos_vit), intent(in)   :: coord_meca_vol ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_meca_vol%pos%lat
coord_tab6(2) = coord_meca_vol%pos%long
coord_tab6(3) = coord_meca_vol%pos%haut
coord_tab6(4) = coord_meca_vol%vit%pente
coord_tab6(5) = coord_meca_vol%vit%azimut
coord_tab6(6) = coord_meca_vol%vit%norme

end subroutine mwpi_meca_vol_2_tab6

end module mwp_meca_vol_pos_vit_mspro
