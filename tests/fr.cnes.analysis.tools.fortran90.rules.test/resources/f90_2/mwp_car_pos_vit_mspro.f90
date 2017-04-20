module mwp_car_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2002-2003

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
! ===   coordonnees cartesiennes, afin d'eviter les risques d'erreur d'affectation
!       des sous tableaux du type coord(1:3) = pos_car et coord(4:6) = vit_car                
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Ces procedures possedent deux arguments obligatoires :
!                     Le premier , du genre intent(out) correspond
!                     a celui qui apparait a gauche du signe "="
!                     Le second , du genre intent(in) correspond a celui
!                     qui apparait a droite du signe "="
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 04/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE globale 4) : suppression d'une surcharge
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_car_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_car_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_pos_vit_car_2_tab6
   module procedure mwpi_tab6_2_pos_vit_car

end interface

CONTAINS

subroutine mwpi_tab6_2_pos_vit_car (pos_vit_car, coord_tab6)

!************************************************************************
!
! But:  Transfert d'un tableau de coordonnees (positions et vitesses)
! ===   contenues dans un tableau coord_tab6 de dimension 6 vers des 
!       coordonnees pos_vit_car de type tm_i_car_pos_vit . 
!
! Note d'utilisation: 
! ==================  
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 04/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

type(tm_i_car_pos_vit), intent(out)      :: pos_vit_car  ! resultat
real(pm_reel), dimension(6), intent(in)  :: coord_tab6   ! operande

! Corps du sous programme
! =======================

pos_vit_car%pos(:) = coord_tab6(1:3)
pos_vit_car%vit(:) = coord_tab6(4:6)

end subroutine mwpi_tab6_2_pos_vit_car

subroutine mwpi_pos_vit_car_2_tab6 (coord_tab6, pos_vit_car)

!************************************************************************
!
! But:  Transfert des coordonnees pos_vit_car de type tm_i_car_pos_vit en  
! ===   tableau coord_tab6 de dimension 6  
!
! Note d'utilisation: 
! ==================  
!
!$Historique
! ==========
!   + Version 3.0 : creation
!                         (Date: 04/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

real(pm_reel), dimension(6), intent(out) :: coord_tab6    ! resultat
type(tm_i_car_pos_vit), intent(in)       :: pos_vit_car   ! operande

! Corps du sous programme
! =======================

coord_tab6(1:3) = pos_vit_car%pos(:) 
coord_tab6(4:6) = pos_vit_car%vit(:)

end subroutine mwpi_pos_vit_car_2_tab6

end module mwp_car_pos_vit_mspro
