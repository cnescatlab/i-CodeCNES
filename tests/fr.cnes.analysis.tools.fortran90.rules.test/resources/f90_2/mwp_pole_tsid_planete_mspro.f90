module mwp_pole_tsid_planete_mspro

! (C) Copyright CNES - MSPRO - 2003

!************************************************************************
!
! But:  Module interne de surcharge de l'operateur d'egalite pour des
! ===   donnees concernant la definition des pole et meridiens origine d'une planete
!
! Note d'utilisation: accessible via les modules de surcharge
! ==================  Ces procedures possedent deux arguments obligatoires
!                     Le premier , du genre intent(out) correspond
!                     a celui qui apparait a gauche du signe "=",
!                     Le second , du genre intent(in) correspond a celui
!                     qui apparait a droite du signe "=".
!
!$Historique
! ==========
!   + Version 4.0 : creation
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
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
  character(len=256), private :: SVN_VER =  '$Id: mwp_pole_tsid_planete_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mwp_pole_tsid_planete_mspro.f90: derniere modification V5.15 >'

! definition de l'interface de la surcharge 

interface assignment (=)

   module procedure mwpi_pole_tsid_p_2_pole_tsid_p

end interface

CONTAINS

subroutine mwpi_pole_tsid_p_2_pole_tsid_p (pole_tsid_planete_out, pole_tsid_planete_in)

!************************************************************************
!
! But:  Copie du type vers le meme type
! ===   (ceci evite des transferts implicites)
!
! Note d'utilisation: 
! ==================  
!
!$Historique
! ==========
!   + Version 4.0 : creation
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Declarations
! ============

type(tm_pole_tsid_planete), intent(out) :: pole_tsid_planete_out     ! resultat
type(tm_pole_tsid_planete), intent(in)  :: pole_tsid_planete_in      ! operande

! Corps du sous programme
! =======================

pole_tsid_planete_out%alpha0 = pole_tsid_planete_in%alpha0
pole_tsid_planete_out%delta0 = pole_tsid_planete_in%delta0
pole_tsid_planete_out%W  = pole_tsid_planete_in%W
pole_tsid_planete_out%dW = pole_tsid_planete_in%dW

end subroutine mwpi_pole_tsid_p_2_pole_tsid_p

end module mwp_pole_tsid_planete_mspro
