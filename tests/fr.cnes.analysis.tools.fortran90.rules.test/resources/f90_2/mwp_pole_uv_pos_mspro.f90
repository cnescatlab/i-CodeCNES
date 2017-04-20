module mwp_pole_uv_pos_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne a la bibliotheque MSPRO necessaire dans le cadre 
! ===   de l'utilisation des surcharges de l'operateur d'egalite pour des
!       donnees sous la forme tm_pole_uv (u,v)        
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_pole_uv_pos_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_pole_uv_pos_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_pole_uv_2_pole_uv

end interface

CONTAINS

subroutine mwpi_pole_uv_2_pole_uv (pole_uv_out, pole_uv_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_pole_uv vers des coordonnees tm_pole_uv
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

type(tm_pole_uv), intent(out) :: pole_uv_out    ! resultat
type(tm_pole_uv), intent(in)  :: pole_uv_in     ! operande

! Corps du sous programme
! =======================

pole_uv_out%u = pole_uv_in%u
pole_uv_out%v = pole_uv_in%v

end subroutine mwpi_pole_uv_2_pole_uv

end module mwp_pole_uv_pos_mspro
