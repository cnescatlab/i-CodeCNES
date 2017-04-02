module parametre_mspro

! (C) Copyright CNES - MSPRO - 2000-2005

!************************************************************************
!
! But:  Definition des parametres MSPRO (hors numeros de routines et de codes retour)
! ===   En particulier la chaine de caracteres "pm_version_MSPRO" donnant le numero
!       de version MSPRO.
!
! Note d'utilisation: Penser a chaque evolution de la librairie a changer: pm_version_MSPRO.
! ==================  Ce module est accessible a l'utilisateur via le module mspro.
!
!$Historique
! ==========
 !   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 1.0 (DE 1) : ajout de nouveaux parameters
!                         (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 2) : ajout de nouveaux parameters
!                          et creation du parameter pm_version_MSLIB90_compatible
!                         (Date: 02/2002 - Realisation: Guylaine Prat et Mickael Hazak)
!   + Version 3.0 (sans DE) : modification du numero de version
!                         (Date: 10/2002 - Realisation: Guylaine Prat )
!   + Version 3.0 (DE globale 2) : suppression des parameter lies aux routines passant dans la MSLIGHT
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE 3) : ajout de nouveaux parameters (themes D et X)
!                         (Date: 10/2002 - Realisation: Bruno Revelin et Michel Lagreca)
!   + Version 3.1 (sans DE) : modification des numeros de version MSPRO et MSLIB90 compatible
!                         (Date: 08/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE 4) : ajout de nouveaux parameters (theme U)
!                         (Date: 09/2003 - Realisation: Guylaine Prat)
!   + Version 4.0 (sans DE) : modification des numeros de version MSPRO et MSLIB90 compatible
!                         (Date: 11/2003 - Realisation: Guylaine Prat)
!   + Version 4.0 (DE globale 8): suppression des parameter lies aux routines passant dans la MSLIB90
!                         (Date: 11/2003 - Realisation: Veronique Lepine)
!   + Version 5.0 (sans DE) : modification des numeros de version MSPRO et MSLIB90 compatible
!                         (Date: 04/2004 - Realisation: Veronique Lepine)
!   + Version 5.1 (sans DE) : modification des numeros de version MSPRO et MSLIB90 compatible
!                         (Date: 09/2004 - Realisation: Guylaine Prat)
!   + Version 5.2 (sans DE) : modification des numeros de version MSPRO et MSLIB90 compatible
!                         (Date: 01/2005 - Realisation: Guylaine Prat)
!   + Version 5.2 (DE 5) : ajout des types d'integrateurs
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : DM-ID 381 :Integrer des routines du theme trajectoires interplanetaires
!                        modification des numeros de version MSPRO et MSLIB90 compatible
!                        transert de parametres lies a mu_racine vers la MSLIB90
!                         (Date: 10/2005 - Realisation: Claire Fabre)
!   + Version 5.3 : DM-ID 408 : ajout de l integrateur Cowell
!                         (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale)
!   + Version 5.4 (sans DE) : modification du numero de version MSPRO 
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.5 (sans DE) : modification du numero de version MSPRO 
!                         (Date: 04/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.7 : DM-ID 738 : Evolution du Cowell
!                   (Date: 06/2007 - Realisation: Atos origin)
!   + Version 5.8 : (sans DE) : modification du numero de version MSPRO
!   + Version 5.9 : (sans DE) : modification du numero de version MSPRO
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 5.10: (sans DE) : modification du numero de version MSPRO
!                   (Date: 8/2008 - Realisation: Atos origin)
!   + Version 5.11: (sans DE) : modification du numero de version MSPRO
!                   (Date: 02/2009 - Realisation: Atos Origin)
!   + Version 5.12: (sans DE) : modification du numero de version MSPRO
!                   (Date: 10/2009 - Realisation: Atos Origin)
!   + VERSION:5.13:AQ:05/03/2010: modification du numero de version (realisation Atos Origin)
!   + VERSION:5.14:AQ::27/05/2010: modification du numero de version (realisation Atos Origin)
!   + Version 5.15: (sans DE) : modification du numero de version MSPRO
!   + VERSION::AQ::22/09/2010: modification du numero de version (realisation Atos Origin)
!
!     VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!     Revision 30 2012/08/14 Fran Simarro/FFSM - GMV
!     MSPRO préparé pour la livraison de la prise en charge
!
!     Revision 157  2012/11/27 Fran Simarro/FFSM - GMV
!     FA-ID 1522-1523 : Traçabilité FTs dans les cartouches
!
!     Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!     Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!     Revision 395  2013/02/26 ffsm
!     DM-ID 1513: Montee de niveau Gfortran
!
!$FinHistorique
!
!************************************************************************

use mslib 
use parametre_themeX_mspro

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: parametre_mspro.f90 395 2013-02-26 12:05:38Z ffsm $'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Numero courant de versions: MSPRO et MSLIB fortran 90 compatible
character(len=*), parameter, public :: pm_version_MSPRO =  'MSPRO version 6.0'
character(len=*), parameter, public :: pm_version_MSLIB90_compatible =  &
                'La MSLIB Fortran 90 compatible avec cette version de la MSPRO est la version 7.0'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! General a tous les themes

! Pour des questions de longueur de ligne dans les mpi_atmi_xxxx, redefinition de la precision
! pour les reels
integer, parameter :: pm_r = pm_reel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Specifiques au theme D

! nombre maximum de sauts de TUC en faisant l'hypothese qu'il y a en moyenne 1 saut du TUC
! par an. Cette valeur, qui est un majorant, permet de couvrir la periode 1966 - 2100
! pm_nb_max_saut_tuc = 2100 - 1966 = 134 sauts , majore par la valeur 150
integer, parameter :: pm_nb_max_saut_tuc = 150

! identificateur d'une echelle de datation TE (Temps des Ephemerides)
integer, parameter :: pm_TE = 111
! identificateur d'une echelle de datation TAI (Temps Atomique International)
integer, parameter :: pm_TAI = 222
! identificateur d'une echelle de datation TUC (Temps Universel Coordonne)
integer, parameter :: pm_TUC = 333

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Specifiques au theme Z

!........................................................................................
! numero logique de la sortie standard
integer, parameter, public :: pm_sortie_standard = 6

!........................................................................................
! longueur de chaines de caracteres
integer, parameter, public :: pm_chaine_libre    = 500 ! message utilisateur
integer, parameter, public :: pm_nom_biblio      = 20  ! pour le nom de la bibliotheque
integer, parameter, public :: pm_numero_version  = 50  ! pour le numero de version MSPRO ou MSLIB90

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Specifiques au theme M

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Specifiques au theme P
!........................................................................................
! Parametres associes a mp_atm_msis86
real(pm_reel), parameter :: pm_alt_min_MSIS86 = 90.e+03_pm_reel ! 90 km (en metres)

!........................................................................................
! Parametres associes a mp_atm_cira
real(pm_reel), parameter :: pm_alt_max = 120.e+03_pm_reel       ! 120 km (en metres)
real(pm_reel), parameter :: pm_lat_borne = 1.396263_pm_reel     ! 79.999977 < 80 degres (en radians)
real(pm_reel), parameter :: pm_trans_m_km = 1000._pm_reel       ! facteur entre metres et km
integer, parameter       :: pm_nb_alt = 36                      ! nombre de donnees d'altitude (mpi_atmi)
integer, parameter       :: pm_nb_lat = 17                      ! nombre de donnees de latitude (mpi_atmi)
real(pm_reel), parameter :: pm_rt     = 6371220._pm_reel  ! rayon terrestre pour le modele
real(pm_reel), parameter :: pm_rv1    = 2637.e-06_pm_reel
real(pm_reel), parameter :: pm_rv2    =   59.e-07_pm_reel
real(pm_reel), parameter :: pm_rv3    = 1000._pm_reel
real(pm_reel), parameter :: pm_ro     = 2.8705_pm_reel
real(pm_reel), parameter :: pm_v1     = 1.e-05_pm_reel    ! parametre associe a la fonction mui_interp_newton

integer, parameter         :: pm_janvier  = 1
integer, parameter         :: pm_fevrier  = 2
integer, parameter         :: pm_mars     = 3
integer, parameter         :: pm_avril    = 4
integer, parameter         :: pm_mai      = 5
integer, parameter         :: pm_juin     = 6
integer, parameter         :: pm_juillet  = 7
integer, parameter         :: pm_aout     = 8
integer, parameter         :: pm_septembre= 9
integer, parameter         :: pm_octobre  = 10
integer, parameter         :: pm_novembre = 11
integer, parameter         :: pm_decembre = 12

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Specifiques au theme U

!........................................................................................
! Parametres associes a mu_eq2degre_reel
integer, parameter :: pm_2racines = 2           ! deux racines distinctes a l'equation
integer, parameter :: pm_1racine_double = 1     ! deux racines egales a l'equation
integer, parameter :: pm_0racine = 0            ! zero racine a l'equation
integer, parameter :: pm_1racine_simple = -1    ! une racine simple a l'equation
integer, parameter :: pm_infinite_racines = 999 ! infinite de racines a l'equation
integer, parameter :: pm_eq2degre = 2           ! equation du second degre
integer, parameter :: pm_eq1degre = 1           ! equation du premier degre
integer, parameter :: pm_eq_degenere = 999      ! equation degeneree

!........................................................................................
! Parametres associes a mu_inter_ind
                                     
! gestion des points doubles
integer, parameter :: pm_avant = 319  ! pour avoir la valeur avant la discontinuite
integer, parameter :: pm_apres = 321  ! pour avoir la valeur apres la discontinuite

! indicateur pour definir le type d'interpolation
integer, parameter :: pm_pt_simple = 111        ! point simple
integer, parameter :: pm_pt_double = 121        ! point double
integer, parameter :: pm_borne_min_double = 211 ! borne min double
integer, parameter :: pm_borne_max_double = 112 ! borne max double

!........................................................................................
! Parametres associes a mu_lagrange
integer, parameter :: pm_max_abscisses = 50
integer, parameter :: pm_max_ordonnees = 10 ! associe aussi a mu_spline_cub_init

!........................................................................................
! Parametres associes aux integrateurs
integer, parameter :: pm_Gill            = 1        ! integrateur Gill
integer, parameter :: pm_DOP853          = 2        ! integrateur Dorman-Prince-853
integer, parameter :: pm_Cowell          = 3        ! integrateur Cowell

integer, parameter :: pm_STOP            = 101      ! action STOP apres un evenement d'une commutation  
integer, parameter :: pm_CONTINUE        = 102      ! action CONTINUE apres un evenement d'une commutation  

!........................................................................................

character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                  '@(#) Fichier MSPRO parametre_mspro.f90: derniere modification V6.0 >'

!................................................................................................................

end module parametre_mspro

