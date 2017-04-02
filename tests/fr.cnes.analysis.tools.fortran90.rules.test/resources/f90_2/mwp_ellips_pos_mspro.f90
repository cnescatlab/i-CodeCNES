module mwp_ellips_pos_mspro

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
! ===   donnees concernant une TM sous la forme tm_ellips (r_equa,apla)        
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Ces procedures possedent deux arguments obigatoires :
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_ellips_pos_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_ellips_pos_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge

interface assignment (=)

   module procedure mwpi_ellips_2_ellips

end interface

CONTAINS

subroutine mwpi_ellips_2_ellips (ellips_out, ellips_in)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Transfert de coordonnees tm_ellips vers des coordonnees tm_ellips
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

type(tm_ellipsoide), intent(out) :: ellips_out   ! resultat
type(tm_ellipsoide), intent(in)  :: ellips_in    ! operande

! Corps du sous programme
! =======================

ellips_out%r_equa = ellips_in%r_equa
ellips_out%apla = ellips_in%apla

end subroutine mwpi_ellips_2_ellips

end module mwp_ellips_pos_mspro
