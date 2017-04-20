
module interface_mslib

! (C) Copyright CNES - MSLIB - 1997-2004

!************************************************************************
!
! But:  Definition des interfaces explicites des routines MSLIB. 
! ===
!
! Note d'utilisation:  Ce module est accessible a l'utilisateur via le module mslib.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 174 ed01 rev00): creation
!                         (Date: 12/1997 - Realisation: Guylaine Prat)
!   + Version 1.0 (DE globale 182 ed01 rev01): creation chaine info_utilisateur
!                 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 11/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 4.0 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 10/2003 - Realisation: Bruno Revelin et Veronique Lepine)
!   + Version 6.0 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 02/2004 - Realisation:Veronique Lepine)
!   + Version 6.1 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 6.2 (sans DE)                  : ajout de nouveaux modules interfaces
!                         (Date: 10/2004 - Realisation: Veronique Lepine)
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                    ajout de nouveaux modules interfaces
!                   (Date: 09/2005 - Realisation: Claire Fabre- Atos origin)
!                   DM-ID 162 : Propagation d'orbite analytique avec J2 seculaire
!                    (Date: 11/2005 - Realisation: Julien Bouillant- Atos origin)
!   + Version 6.4 : DM-ID 426 : routine de calcul des angles de precession
!                 (Date: 04/2006 - Realisation: Claire Fabre - Atos Origin)
!                  DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                 (Date: 05/2006 - Realisation: Claire Fabre - Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

  use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

  ! liste de tous les modules interfaces existants
  ! ==============================================


use int_constantes
use int_dates
use int_extrapolation
use int_interplanetaire
use int_manoeuvres
use int_rep_orbitaux
use int_geophysique
use int_rep_fondamentaux
use int_ephemerides
use int_chgmnt_reperes
use int_utilitaires
use int_chgmnt_variables
use int_codes_retours

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: interface_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


  character(len=pm_longueur_info_utilisateur), private, parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB interface_mslib.f90: derniere modification V6.13 >'

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: interface_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module interface_mslib
