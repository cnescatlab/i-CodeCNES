module mwp_orb_cir_pos_vit_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
!       donnees concernant une TM sous la forme a,ex,ey,i,gom,pso_M        
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_orb_cir_pos_vit_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_orb_cir_pos_vit_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_orb_cir_2_orb_cir

   module procedure mwpi_tab6_2_orb_cir
   module procedure mwpi_orb_cir_2_tab6

end interface

CONTAINS

subroutine mwpi_orb_cir_2_orb_cir (coord_orb_cir_out, coord_orb_cir_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_orb_cir vers des coordonnees tm_orb_cir
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

type(tm_orb_cir), intent(out) :: coord_orb_cir_out ! resultat
type(tm_orb_cir), intent(in)  :: coord_orb_cir_in  ! operande

! Corps du sous programme
! =======================

coord_orb_cir_out%a   = coord_orb_cir_in%a
coord_orb_cir_out%ex  = coord_orb_cir_in%ex
coord_orb_cir_out%ey  = coord_orb_cir_in%ey
coord_orb_cir_out%i   = coord_orb_cir_in%i
coord_orb_cir_out%gom = coord_orb_cir_in%gom
coord_orb_cir_out%pso_M = coord_orb_cir_in%pso_M

end subroutine mwpi_orb_cir_2_orb_cir

subroutine mwpi_tab6_2_orb_cir (coord_orb_cir, coord_tab6)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees contenues dans un tableau coord_tab6
! ===   de dimension 6 vers des coordonnees tm_orb_cir .
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

type(tm_orb_cir), intent(out)            :: coord_orb_cir   ! resultat
real(pm_reel), dimension(6), intent(in)  :: coord_tab6      ! operande

! Corps du sous programme
! =======================

coord_orb_cir%a     = coord_tab6(1) 
coord_orb_cir%ex    = coord_tab6(2) 
coord_orb_cir%ey    = coord_tab6(3) 
coord_orb_cir%i     = coord_tab6(4) 
coord_orb_cir%gom   = coord_tab6(5) 
coord_orb_cir%pso_M = coord_tab6(6) 

end subroutine mwpi_tab6_2_orb_cir

subroutine mwpi_orb_cir_2_tab6 (coord_tab6, coord_orb_cir)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert des coordonnees tm_orb_cir en tableau coord_tab6 de 
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

real(pm_reel), dimension(6), intent(out) :: coord_tab6      ! resulta
type(tm_orb_cir), intent(in)             :: coord_orb_cir   ! operande

! Corps du sous programme
! =======================

coord_tab6(1) = coord_orb_cir%a
coord_tab6(2) = coord_orb_cir%ex
coord_tab6(3) = coord_orb_cir%ey
coord_tab6(4) = coord_orb_cir%i
coord_tab6(5) = coord_orb_cir%gom
coord_tab6(6) = coord_orb_cir%pso_M

end subroutine mwpi_orb_cir_2_tab6

end module mwp_orb_cir_pos_vit_mspro
