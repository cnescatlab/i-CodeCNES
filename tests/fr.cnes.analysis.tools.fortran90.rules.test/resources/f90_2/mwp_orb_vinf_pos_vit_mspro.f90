module mwp_orb_vinf_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant une TM sous la forme C3,DLA,RLA,Rp,gom,M        
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
!   + Version 5.2 : creation
!                         (Date: 08/2004 - Realisation: Bruno Revelin)
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_orb_vinf_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_orb_vinf_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_orb_Vinf_2_orb_Vinf

   module procedure mwpi_tab6_2_orb_Vinf
   module procedure mwpi_orb_Vinf_2_tab6

end interface

CONTAINS

subroutine mwpi_orb_Vinf_2_orb_Vinf (coord_orb_Vinf_out, coord_orb_Vinf_in)

!************************************************************************
!
! But:  Transfert de coordonnees tm_orb_Vinf vers des coordonnees tm_orb_Vinf
! ===   (ceci evite des transferts implicites)
!
! Note d'utilisation: 
! ==================
!************************************************************************

! Declarations
! ============

type(tm_orb_Vinf), intent(out) :: coord_orb_Vinf_out   ! resultat
type(tm_orb_Vinf), intent(in)  :: coord_orb_Vinf_in    ! operande

! Corps du sous programme
! =======================

coord_orb_Vinf_out%C3   = coord_orb_Vinf_in%C3
coord_orb_Vinf_out%DLA  = coord_orb_Vinf_in%DLA
coord_orb_Vinf_out%RLA  = coord_orb_Vinf_in%RLA
coord_orb_Vinf_out%Rp   = coord_orb_Vinf_in%Rp
coord_orb_Vinf_out%gom  = coord_orb_Vinf_in%gom
coord_orb_Vinf_out%M    = coord_orb_Vinf_in%M

end subroutine mwpi_orb_Vinf_2_orb_Vinf

subroutine mwpi_tab6_2_orb_Vinf (coord_orb_Vinf, coord_tab6)

!************************************************************************
!
! But:  Transfert des coordonnees contenues dans un tableau coord_tab6
! ===   de dimension 6 vers des coordonnees tm_orb_Vinf .
!
! Note d'utilisation: 
! ==================
!
!************************************************************************

! Declarations
! ============

type(tm_orb_Vinf), intent(out)           :: coord_orb_Vinf  ! resultat
real(pm_reel), dimension(6), intent(in)  :: coord_tab6      ! operande

! Corps du sous programme
! =======================

coord_orb_Vinf%C3   = coord_tab6(1) 
coord_orb_Vinf%DLA  = coord_tab6(2) 
coord_orb_Vinf%RLA  = coord_tab6(3) 
coord_orb_Vinf%Rp   = coord_tab6(4) 
coord_orb_Vinf%gom  = coord_tab6(5) 
coord_orb_Vinf%M    = coord_tab6(6) 

end subroutine mwpi_tab6_2_orb_Vinf

subroutine mwpi_orb_Vinf_2_tab6 (coord_tab6, coord_orb_Vinf)

!************************************************************************
!
! But:  Transfert des coordonnees tm_orb_Vinf en tableau coord_tab6 de 
! ===   coordonnees de dimension 6.                                     
!
! Note d'utilisation:
! ==================
!
!************************************************************************

! Declarations
! ============

real(pm_reel), dimension(6), intent(out)  :: coord_tab6      ! resultat
type(tm_orb_Vinf), intent(in)             :: coord_orb_Vinf  ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_orb_Vinf%C3
coord_tab6(2) = coord_orb_Vinf%DLA
coord_tab6(3) = coord_orb_Vinf%RLA
coord_tab6(4) = coord_orb_Vinf%Rp
coord_tab6(5) = coord_orb_Vinf%gom
coord_tab6(6) = coord_orb_Vinf%M

end subroutine mwpi_orb_Vinf_2_tab6

end module mwp_orb_vinf_pos_vit_mspro
