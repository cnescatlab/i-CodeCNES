
module longueur_chaine_mslib

! (C) Copyright CNES - MSLIB - 1997-2003

!************************************************************************
!
! But:  Definition des longueurs de chaines de caracteres. 
! ===
!
! Note d'utilisation:  toute routine ou module utilisant parametre_mslib connait implicitement ce module.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 173 ed01 rev00): creation
!                         (Date: 12/1997 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE 441 ed01 rev00): ajout de la longueur pm_message
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 5.0 (DE 616 ed01 rev00): revision de la longueur pm_nom_routine
!                         (Date: 12/2003 - Realisation: Guylaine Prat)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: longueur_chaine_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


  ! Connu de l'utilisateur
  integer, parameter, public :: pm_nom_routine = 31                ! nom de routine
  integer, parameter, public :: pm_identification_routine = 500    ! identification de routine
  integer, parameter, public :: pm_signification_code_retour = 550 ! signification valeur code retour
  integer, parameter, public :: pm_message = 512                   ! champ message (code retour)

  ! interne projet (mais utilise partout)
  integer, parameter, public :: pm_longueur_info_utilisateur = 100 ! pour le marquage par what
  integer, parameter, public :: pm_longueur_rcs_id = 100           ! pour le marquage par RCS

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB longueur_chaine_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id = &
            ' $Id: longueur_chaine_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module longueur_chaine_mslib
