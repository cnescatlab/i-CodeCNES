module ephem

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ephem
!
!$Resume
!    Module principal de la librairie éphémérides
!
!$Description
!    Module faisant les "use" de tous les modules de la librairie
!
!$Auteur
!    Florence VIVARES (SchlumbergerSema)
!
!$Version
!  $Id: ephem.F90 360 2013-02-15 11:38:21Z aadt $
!
!$Historique
!  $Log: ephem.F90,v $
!  Revision 360  2013/02/14 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.13  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.12  2009/02/20 11:06:46  cml
!  AQ : Remise en place d'un use necessaire
!
!  Revision 1.11  2009/02/20 10:26:38  cml
!  AQ : Suppression de use inutiles
!
!  Revision 1.10  2009/02/20 09:19:16  cml
!  DM-ID 960 : Mise a jour de eph_poscor pour VSOP87
!
!  Revision 1.9  2009/02/12 15:34:51  cml
!  DM-ID 960 : Ajout de la methode JPL aux methodes d'ephemerides disponibles
!
!  Revision 1.8  2008/10/02 13:37:05  tanguyy
!  AQ : maj des cartouches
!
!  Revision 1.7  2008/10/02 13:32:41  tanguyy
!  DM-ID 1058 / AQ : controle de coherence sur la methode employee lors de l'appel a eph_poscor
!
!  Revision 1.6  2008/08/04 13:35:46  gss
!  DM-ID 1058 : (portage g95) utilisation de la variable codes pour écrire le
!  code dans le message d'erreur de la fonction eph_poscor.
!
!  Revision 1.5  2006/05/30 08:19:09  vivaresf
!  DM-ID 387 : variables locales inutilisées
!
!  Revision 1.4  2006/04/18 09:53:30  vivaresf
!  FA 500 : traces da la DM 391
!
!  Revision 1.3  2006/01/23 13:43:45  bouillaj
!  Suppression de la metheode EPROC
!
!  Revision 1.2  2005/12/08 18:25:04  vivaresf
!  implicit none
!
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.7  2005/11/07 15:56:27  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.6  2005/10/12 13:03:43  bouillaj
!  DM-ID 147. Ajout extrapolation keplerienne
!  Revision 1.5  2005/05/09 14:17:33  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.4  2005/03/09 09:12:05  vivaresf
!  Correction des cartouches
!  Revision 1.3  2004/12/17 14:58:12  vivaresf
!  Documentation
!  Revision 1.2  2004/05/25 13:54:15  vivaresf
!   Version V1_9 : sans la MECASPA
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.15  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.14  2004/01/09 16:20:01  bremard
!  Mise à jour des cartouches
!  Revision 1.13  2004/01/07 16:02:54  bremard
!  Mise à jour cartouche
!  Revision 1.12  2003/12/30 16:04:44  bremard
!  Suppression du cas Extrapolation Keplerienne (61) + commentaires
!  Revision 1.11  2001/12/18 16:10:16  vivaresf
!  Suppression sortie bulletin
!  Revision 1.10  2001/12/13 15:03:32  bremard
!  PhB - Correction parametres MSP_creer_bulletin
!  Revision 1.9  2001/12/12 12:34:29  bremard
!  PhB - ajout sortie optionnelles bulletin
!  Revision 1.8  2001/12/07 16:44:44  vivaresf
!  Presentation fm des cartouches
!  Revision 1.7  2001/12/05 16:14:20  bremard
!  PhB - Mise à jour du cartouche
!  Revision 1.6  2001/12/05 10:10:12  vivaresf
!  appel NAIF avec tau en option
!  Revision 1.5  2001/12/04 17:30:30  bremard
!  PhB - pas de calcul de tau dans eph_pvnaif
!  Revision 1.4  2001/12/04 09:18:30  bremard
!  PhB - Appel à eph_tcheb
!  Revision 1.3  2001/11/30 11:18:39  vivaresf
!  ni dans l'appel de eph_pvkep
!  Revision 1.2  2001/11/28 13:34:52  bremard
!  PhB - Ajout du cas BDL (eph_pvbdl)
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Usage
!  use ephem
!
!$Module
!#V
!- eph_util
!- eph_constantes
!- eph_info
!- eph_analytique
!- eph_init
!- eph_naif
!- eph_tcheb
!- eph_tchebmad
!- eph_eproc
!- eph_bdl
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_poscor
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use eph_analytique
  use eph_naif
  use eph_tcheb
  use eph_tchebmad
  use eph_bdl
  use eph_jpl_ephemeris
  use eph_vsop87
  use eph_init

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ephem.F90 360 2013-02-15 11:38:21Z aadt $'


contains

  subroutine eph_poscor(code, date, nc, ni, coord, tau)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_poscor
!
!$Resume
!  Sous-programme chapeau de calcul d'éphémérides 
!
!$Description
!  Sous-programme chapeau de calcul d'éphémérides couvrant
!  les différentes méthodes proposées
!
!$Auteur
!  Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_poscor(code, date, nc, ni, coord, [tau])
!.    real(KIND=PM_REEL) :: date
!.    integer :: code, nc, ni
!.    real(KIND=PM_REEL), dimension(6) :: coord
!.    real(KIND=PM_REEL) :: tau
!
!$Arguments
!>E     code   :<integer>           code méthode/théorie
!>E     date   :<PM_REEL>           date en JJ50 
!>E     nc     :<integer>           numéro du corps central 
!>E     ni     :<integer>           numéro du corps d'intérêt
!>S     coord  :<PM_REEL,DIM=(6)>   position/vitesse de ci par rapport à cc
!                                   dans le repère d'expression : 
!.                                   - repère dans lequel sont définies les 
!                                     éphémérides tchebychevisées, 
!.                                   - EME2000 pour les autres méthodes.
!.                                  Rmq : les sorties de eph_poscor sont en km et km/s
!>[S]   tau    :<PM_REEL>           temps lumière entre cc et ci
!.                                   Attention: si tau est demandé, le calcul des 
!                                   positions/vitesses est réalisé en tenant compte
!                                   du temps lumière
!
!$Remarques
! Les sorties de eph_poscor sont en km et km/s
!$Mots-cles
!
!$Voir-Aussi
!
!$Routines
!- eph_pvcnes
!- eph_pvkep
!- eph_pvnaif
!- eph_pvbdl
!- eph_pveproc
!- eph_pvtcheb
!- eph_pvtchebmad
!- MSP_signaler_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    ! Arguments
    !==========
    real(KIND=PM_REEL), intent(in) :: date
    integer, intent(in) :: code, nc, ni
    real(KIND=PM_REEL), dimension(6), intent(out) :: coord
    real(KIND=PM_REEL),optional, intent(out) :: tau

    ! Variables locales
    !==================
    integer :: code0
    character(len=4) :: codes,ch_methode
    
    ! Initialisation du code de la méthode utilisée
    !==============================================
    ! On divise par 10 pour obtenir le code de la méthode 
    code0=code/10
    ! code par défaut
    if (code==0) code0=eph_methode_analytique


    ! Contrôle de la cohérence entre le code appelé
    ! et le code ayant servi pour la dernière initialisation
    !=======================================================
    if (code0 /= eph_methode_analytique) then
       if (code0 /= code_courant) then
          ! Emission d'un message pour indiquer le code employé
          write (ch_methode,'(I0)') code
          call Msp_signaler_message(cle_mes="EPH_METHODE_INCORRECTE", &
               partie_variable=ch_methode, &
               routine="eph_poscor" )
          return 
       end if
    end if

    
    select case (code0)

    case(eph_methode_analytique)     
       if(code.eq.10) then
          ! VSOP82 analytique : code 10
          call eph_pvcnes(ni, nc, date, coord)
       else     
          ! Extrapolation képlérienne : code 11
          call eph_pvkep(ni, nc, date, coord)
       endif 
    case(eph_methode_naif)     
       ! NAIF : code 2x (20, 21, 22 ..)
       ! NAIF (sans prise en du temps lumière)
       if(present(tau)) then
          call eph_pvnaif(ni, nc, date, coord, tau=tau)
       else
          call eph_pvnaif(ni, nc, date, coord)
       endif
    case(eph_methode_bdl)     ! BDL
       if(code.eq.30) then
          call eph_pvbdl(ni, nc, date, coord)
       endif
    case(4)     
       ! Tchebytchev : codes 4x
       call eph_pvtcheb(nc, ni, date, coord)       
    case(5)     
       ! Madona : codes 5x
       call eph_pvtchebmad(nc, ni, date, coord) 
    case(6)     
       ! JPL : codes 6x
       call eph_pvjpl(ni, nc, date, coord)
    case(7)     
       ! IMCEE : codes 6x
       call eph_pvvsop87(ni, nc, date, coord)

    case default
       write (codes,'(i4)') code
       call MSP_signaler_message(cle_mes="EPH_ERR_CODEMETHODE", &
                                 partie_variable=codes, &
                                 routine="eph_poscor")
    end select

  end subroutine eph_poscor

end module ephem

