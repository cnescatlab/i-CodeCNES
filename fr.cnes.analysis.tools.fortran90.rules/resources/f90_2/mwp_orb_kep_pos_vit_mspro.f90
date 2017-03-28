module mwp_orb_kep_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant une TM sous la forme a,e,i,pom,gom,M        
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_orb_kep_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_orb_kep_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_orb_kep_2_orb_kep

   module procedure mwpi_tab6_2_orb_kep
   module procedure mwpi_orb_kep_2_tab6

end interface

CONTAINS

subroutine mwpi_orb_kep_2_orb_kep (coord_orb_kep_out, coord_orb_kep_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_orb_kep vers des coordonnees tm_orb_kep
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

type(tm_orb_kep), intent(out) :: coord_orb_kep_out   ! resultat
type(tm_orb_kep), intent(in)  :: coord_orb_kep_in    ! operande

! Corps du sous programme
! =======================

coord_orb_kep_out%a   = coord_orb_kep_in%a
coord_orb_kep_out%e   = coord_orb_kep_in%e
coord_orb_kep_out%i   = coord_orb_kep_in%i
coord_orb_kep_out%pom = coord_orb_kep_in%pom
coord_orb_kep_out%gom = coord_orb_kep_in%gom
coord_orb_kep_out%M   = coord_orb_kep_in%M

end subroutine mwpi_orb_kep_2_orb_kep

subroutine mwpi_tab6_2_orb_kep (coord_orb_kep, coord_tab6)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees contenues dans un tableau coord_tab6
! ===   de dimension 6 vers des coordonnees tm_orb_kep .
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

type(tm_orb_kep), intent(out)            :: coord_orb_kep  ! resultat
real(pm_reel), dimension(6), intent(in)  :: coord_tab6     ! operande

! Corps du sous programme
! =======================

coord_orb_kep%a   = coord_tab6(1) 
coord_orb_kep%e   = coord_tab6(2) 
coord_orb_kep%i   = coord_tab6(3) 
coord_orb_kep%pom = coord_tab6(4) 
coord_orb_kep%gom = coord_tab6(5) 
coord_orb_kep%M   = coord_tab6(6) 

end subroutine mwpi_tab6_2_orb_kep

subroutine mwpi_orb_kep_2_tab6 (coord_tab6, coord_orb_kep)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees tm_orb_kep en tableau coord_tab6 de 
! ===   coordonnees de dimension 6.                                     
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

real(pm_reel), dimension(6), intent(out) :: coord_tab6     ! resultat
type(tm_orb_kep), intent(in)             :: coord_orb_kep  ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_orb_kep%a
coord_tab6(2) = coord_orb_kep%e
coord_tab6(3) = coord_orb_kep%i
coord_tab6(4) = coord_orb_kep%pom
coord_tab6(5) = coord_orb_kep%gom
coord_tab6(6) = coord_orb_kep%M

end subroutine mwpi_orb_kep_2_tab6

end module mwp_orb_kep_pos_vit_mspro
