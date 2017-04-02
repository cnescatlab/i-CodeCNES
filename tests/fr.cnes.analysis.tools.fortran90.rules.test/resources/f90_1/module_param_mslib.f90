
module module_param_mslib

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPLAM1 de la MSLIB f77                   
!                   (Date: 09/2005 - Realisation: Claire Fabre - Atos origin) 
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
use type_mslib

implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: module_param_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


! Parametres associes a mu_racine

! Justification de l'utilisation de l'attribut save aux variables de module (don(6) du RNC CNES):
! Ces 2 variables sont initialisees dans mi_pb_lambert0.f90 et mi_pb_lambert1.f90 et sont utilisees ensuite 
! dans la routine passee en parametre de mu_racine. (cette routine ayant une sequence d'appel figee,
! on ne peut pas passer ces variables en parametres d'entree)

real (pm_reel), save :: param_q          ! parametre Q de la conique
integer, save :: nbre_tours              ! nombre de tours effectues par la sonde

!...............................................................................................................

 

 character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB module_param_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: module_param_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module module_param_mslib

