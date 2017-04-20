
module mslib

! (C) Copyright CNES - MSLIB - 1997-2003

!************************************************************************
!
! But:  acces simplifie pour l'utilisateur a tous les modules necessaires. 
! ===   Definition de la chaine de caracteres "version_MSLIB_id" donnant
!       via le binaire (commande what) le numero de version de la librairie.
!
! Note d'utilisation: l'utilisateur n'a besoin que de faire: use mslib
! ==================  En interne il n'est pas possible d'utiliser ce module
!                     a cause du module interface (compilation impossible).
!
!$Historique
! ==========
!   + Version 0.1 (SP 148 ed01 rev00): creation
!                         (Date: 12/1997 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (DE 282 ed01 rev01): rajout de l'acces a des constantes mathematiques ou physiques
!                         (Date: 08/1998 - Realisation: Guylaine Prat)
!   + Version 3.2 (DE 447 ed01 rev00): Changement de l'utilisation du parameter pm_version_MSLIB par pm_version_MSLIB90
!                         (Date: 04/2002 - Realisation: Mickael Hazak)
!   + Version 4.0 (FA globale 478 ed01 rev00): Remplacement de l'anti-slash par le caractere >
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.5 : DM-ID 478 : integrateur de cowell, modification de l'interface
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 suppression des modules de constantes *_mslib 
!                   du fait de la passage dans int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!     Revision 362 2013/02/15 bbjc
!     DM-ID 1513: Suppression des warnings de compilation
!
!     Revision 389  2013/02/25 ffsm
!     DM-ID 1513: Montee de niveau Gfortran
!
!$FinHistorique
!
!************************************************************************

  use interface_mslib    ! Definition de toutes les interfaces des routines utilisateurs.
  use parametre_mslib    ! Definition de parametres MSLIB.
  use type_mslib         ! Definition de tous les types MSLIB.

  ! Fourniture de l'acces direct a certaines constantes (decrites dans le Volume 3 de la documentation) : déjà dans int_constantes (inclue par interface_mslib)

  ! Fourniture de l'acces aux operateurs de surcharge
  use surcharge_mslib         ! 

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: mslib.f90 389 2013-02-25 14:03:50Z ffsm $'


  ! numero de version MSLIB pour le binaire (avec commande what)
  character(len=300),private  :: version_MSLIB_id = '@(#) '//pm_version_MSLIB90//'>'

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB mslib.f90: derniere modification V7.0 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: mslib.f90 389 2013-02-25 14:03:50Z ffsm $ '

end module mslib
