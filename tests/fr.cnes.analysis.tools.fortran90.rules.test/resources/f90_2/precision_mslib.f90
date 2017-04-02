
module precision_mslib

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Definition des precisions MSLIB. 
! ===
!
! Note d'utilisation:  toute routine ou module utilisant parametre_mslib connait implicitement ce module.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 178 ed01 rev00): creation
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

  use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: precision_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


  ! Precisions retenues
  ! ===================

  integer, parameter, public :: pm_reel = selected_real_kind(13)
  integer, parameter, public :: pm_entier = selected_int_kind(9)

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB precision_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: precision_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module precision_mslib

