
module flags_mslib

! (C) Copyright CNES - MSLIB - 1999

!************************************************************************
!
! But:  Definition des flags de la MSLIB. 
! ===
!
! Note d'utilisation:  toute routine ou module utilisant mslib connait implicitement ce module.
! ==================
!
!$Historique
! ==========
!   + Version 6.4 : DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                         (Date: 05/2006 - Realisation : Atos Origin)
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
use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: flags_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


! Drapeau associe au modele de nutation:
! ======================================
! si whar_p5 est positionné a .TRUE. on utilise :
! Le standard IERS 1996/ IERS 2003 polynome de degré 5
! (par défaut utilisation du modèle polynome de degré 4 (whar)

logical :: wahr_p5 = .FALSE.

!................................................................................................................

  character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB flags_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: flags_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module flags_mslib
