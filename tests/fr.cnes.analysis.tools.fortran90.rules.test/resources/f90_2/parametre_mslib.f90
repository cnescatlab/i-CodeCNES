module parametre_mslib

! (C) Copyright CNES - MSLIB - 1998-2005

!************************************************************************
!
! But:  Definition des parametres MSLIB. 
! ===   En particulier la chaine de caracteres "pm_version_MSLIB90" donnant le numero
!       de version MSLIB.
!
! Note d'utilisation: Penser a chaque evolution de la librairie a changer: pm_version_MSLIB90.
! ==================  Ce module est accessible a l'utilisateur via le module mslib.
!
!      Attention à mettre à jour la liste des constantes dans ce module chaque que
!      l'on en rajoute une dans un des module "int_<theme>"
!
!$Historique
! ==========
!   + Version 0.1 (SP 172 ed01 rev00): creation
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (DE 290 ed01 rev00) : ajout de nouveaux modules
!                         (Date: 10/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 340 ed01 rev00) : ajout de nouveaux modules
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (sans DE)           : modification du numero de version
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (sans DE) : modification du numero de version
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 3.2 (DE 448 ed01 rev00) : remplacement du parameter pm_version_MSLIB par pm_version_MSLIB90
!                         (Date: 04/2002 - Realisation: Mickael Hazak)
!   + Version 4.0 (sans DE)           : modification du numero de version
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 4.1 (sans DE)           : modification du numero de version
!                         (Date: 08/2003 - Realisation: Guylaine Prat)
!   + Version 5.0 (sans DE)           : modification du numero de version
!                         (Date: 10/2003 - Realisation: Guylaine Prat)
!   + Version 5.0 (DE 602 ed01 rev00) : ajout de code_planetes_mslib
!                         (Date: 12/2003 - Realisation: Bruno Revelin)
!   + Version 6.0 (sans DE)           : modification du numero de version
!                         (Date: 04/2004 - Realisation: Veronique Lepine)
!   + Version 6.1 (sans DE)           : modification du numero de version
!                         (Date: 09/2004 - Realisation: Guylaine Prat)
!   + Version 6.2 (sans DE)           : modification du numero de version
!                         (Date: 01/2005 - Realisation: Guylaine Prat)
!   + Version 6.2 (DE 1)              : ajout du module code_anomalies_mslib
!                         (Date: 01/2005 - Realisation: Veronique Lepine)
!   + Version 6.3 (sans DE)              : modification du numero de version
!                         (Date: 10/2005 - Realisation: Claire Fabre)
!   + Version 6.4 (sans DE)              : modification du numero de version
!                         (Date: 04/2006 - Realisation: Claire Fabre)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 636 : controle de la latitude pour la fonction mt_geoc_car
!                   (Date: 05/2007 - Realisation: Atos origin) 
!   + Version 6.6 : DM-ID 616 (option) : remplacement des sous-modules :
!    - indic_reperes_mslib par une sélection de constantes de int_manoeuvres
!    - indic_comp_joursec_mslib par une sélection de constantes de int_dates
!    - code_anomalies_mslib par une sélection de constantes de int_chgmnt_variables
!    - code_transfo_mslib par une sélection de constantes de int_rep_fondamentaux
!    - code_modeles_mslib par une sélection de constantes de int_rep_fondamentaux
!    - code_planetes_mslib par une sélection de constantes de int_rep_fondamentaux
!    - code_racine_mslib par une sélection de constantes de int_utilitaires
!                   (Date: 05/2007 - Realisation: Atos origin) 
!   + Version 6.7 : (sans DE)            : modification du numero de version
!                   (Date: 11/2007 - Realisation : Atos origin)
!   + Version 6.8 : (sans DE)            : modification du numero de version
!                   (Date: 03/2008 - Realisation : Atos origin)
!   + Version 6.10 : FA-ID 1126          : modification du numero de version
!                   (Date: 02/2009 - Realisation : Atos origin)
!   + Version 6.11 : (sans DE)          : modification du numero de version
!                   (Date: 10/2009 - Realisation : Atos origin)
!   + Version 6.12 : (sans DE)          : modification du numero de version
!                   (Date: 05/2010 - Realisation : Atos origin)
!   + VERSION::AQ::20/09/2010:Modification du numero de version
!   + Version 6.13 : (sans DE)          : modification du numero de version
!                   (Date: 09/2010 - Realisation : Atos origin)
!     VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!   + Version 6.14 : (sans DE)          : modification du numero de version
!     VERSION:V6.14.1:DM-ID:1489:14/09/2011:version
!   + Version 6.14.1 : (sans DE)        : modification du numero de version
!
!     Revision 42 2012/08/16 Beatriz Jilete/BBJC - GMV
!     MSLIB90 préparé pour la livraison de la prise en charge
!
!     Revision 157  2012/11/27 Fran Simarro/FFSM - GMV
!     FA-ID 1522-1523 : Traçabilité FTs dans les cartouches
!
!     Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!     Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!     Revision 362 2013/03/15 bbjc
!     DM-ID 1513: Suppression des warnings de compilation
!
!     Revision 389  2013/02/25 ffsm
!     DM-ID 1513: Montee de niveau Gfortran
!
!$FinHistorique
!
!************************************************************************

  use precision_mslib           ! definition des precisions retenues
  use valeur_code_retour_mslib  ! definition des valeurs des codes retour
  use numero_routine_mslib      ! definition des numeros des routines
  use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

  ! definition des indicateurs des reperes de la MSLIB
  use int_manoeuvres, only : pm_rep_geo,pm_rep_qsw,pm_rep_tnw

  ! definition des indicateurs de comparaison de jour
  use int_dates, only : pm_joursec1_sup_joursec2, pm_joursec1_egal_joursec2,&
       pm_joursec1_inf_joursec2

  ! definition des clefs definissant les types d'anomalies
  use int_chgmnt_variables, only : pm_anom_E,pm_anom_v,pm_anom_M

  ! definition des indicateurs de passage d'un repere a un autre
  use int_rep_fondamentaux, only : pm_equa_moy_equa_moy, &
       pm_equa_moy_equa_vrai, pm_equa_moy_ecli_moy, pm_equa_moy_ecli_vrai
  use int_rep_fondamentaux, only : pm_equa_vrai_equa_moy, &
      pm_equa_vrai_equa_vrai, pm_equa_vrai_ecli_moy, pm_equa_vrai_ecli_vrai
  use int_rep_fondamentaux, only : pm_ecli_moy_equa_moy, &
       pm_ecli_moy_equa_vrai, pm_ecli_moy_ecli_moy, pm_ecli_moy_ecli_vrai
  use int_rep_fondamentaux, only : pm_ecli_vrai_equa_moy, &
       pm_ecli_vrai_equa_vrai, pm_ecli_vrai_ecli_moy, pm_ecli_vrai_ecli_vrai

  use int_rep_fondamentaux, only : pm_1x_2y_3x, pm_1x_2z_3x, pm_1y_2x_3y, &
       pm_1y_2z_3y, pm_1z_2x_3z, pm_1z_2y_3z, &
       pm_1x_2y_3z, pm_1x_2z_3y, pm_1y_2x_3z, pm_1y_2z_3x, pm_1z_2x_3y, pm_1z_2y_3x

  ! definition des modeles de la MSLIB
  use int_rep_fondamentaux, only : pm_lieske_wahr,pm_lieske,pm_wahr,&
       pm_nb_modelesUAI,pm_UAI_modeles_min,pm_UAI_autre_modele,pm_UAI1994,&
       pm_UAI2000,pm_UAI_modeles_max

  ! definition des clefs definissant les planetes
    use int_rep_fondamentaux, only : pm_pla_mercure,pm_pla_venus,pm_pla_terre, &
         pm_pla_mars, pm_pla_jupiter,pm_pla_saturne,&
         pm_pla_uranus,pm_pla_neptune,pm_pla_pluton

    ! definition des clefs definissant le nombre de racines
    use int_utilitaires, only : pm_A_B_racine,pm_1racine,pm_0racine_nb_pair,&
         pm_criter_arret_x,pm_criter_arret_fx,pm_criter_arret_iter_max

  implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: parametre_mslib.f90 389 2013-02-25 14:03:50Z ffsm $'



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Numero courant de version MSLIB Fortran 90
  character(len=*),parameter :: pm_version_MSLIB90 =  'MSLIB Fortran 90 version 7.0'
  character(len=*),parameter :: pm_version_MSLIB =  pm_version_MSLIB90 ! pour assurer la compatibilite ascendante

  !................................................................................................................

  character(len=pm_longueur_info_utilisateur), private, parameter :: info_utilisateur = &
                    '@(#) Fichier MSLIB parametre_mslib.f90: derniere modification V7.0 >'

!................................................................................................................

  character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: parametre_mslib.f90 389 2013-02-25 14:03:50Z ffsm $ '

end module parametre_mslib

