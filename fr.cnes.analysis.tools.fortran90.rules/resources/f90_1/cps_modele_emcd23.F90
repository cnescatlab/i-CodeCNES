module cps_modele_emcd23

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_emcd23
!
!$Resume
!  Modèles d'Atmosphère martien EMCD 2.3
!
!$Description
! Ce module contient la version 2.3 du modèle EMCD (European Martian
! Climate Database) :
! - routine cps_atmemcd_23
! 
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_modele_emcd23.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: cps_modele_emcd23.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.10  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2008/07/04 11:58:17  huec
!  DM-ID 1058 : Precision numerique : transformations de parametres en pm_reel
!
!  Revision 1.8  2008/04/11 17:44:08  vivaresf
!  FA-ID 1009 : intégration PSIMU,
!  gestion des / en fin de répertoire
!
!  Revision 1.7  2008/04/07 09:17:11  vivaresf
!  FA-ID 1009 : suppression des use cps_acces
!  correction des cartouches
!  vérification des accès par le biais de la base COMPAS aux modèles
!
!  Revision 1.6  2008/02/26 14:28:55  vivaresf
!  FA-ID 939 : écriture des messages d'erreur sur le stderr au lieu du stdout
!
!  Revision 1.5  2006/11/14 17:57:28  vivaresf
!  DM-ID 425 : formattage des sorties
!
!  Revision 1.4  2006/11/13 11:55:13  vivaresf
!  DM-ID 425 : utilisation des routines intrinseques mod, sqrt, real au lieu de dmod, dsqrt ou dfloat
!
!  initialisation de gset dans gasdev
!
!  Revision 1.3  2006/03/13 09:24:55  vivaresf
!  DM-ID 493 : traitement d'erreur
!
!  Revision 1.2  2006/03/03 15:21:24  vivaresf
!  DM-ID 493 : variables en common passees en tableaux dynamique
!
!  rajout des fonctions de desallocation (cps_atmemcd_*_close)
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.6  2005/10/12 11:01:22  vivaresf
!  DM-ID 388 : fonctions de parametrisation exportées pour
!  les objets GS_LIB
!
!  Revision 1.5  2005/03/07 15:32:16  vivaresf
!  DM-ID 318 : integration
!
!  Revision 1.5  2005/01/27 15:19:10  vivaresf
!  Fichiers d'include pour les modeles d'atmosphere
!
!  Revision 1.4  2005/03/07 08:15:49  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!  DM-ID 318 : entetes des fichiers et des routines utilisateurs
!  nommage des fonctions publique, privatisation des fonctions internes
!  declaration des variables
!
!
!  Revision 1.3  2005/03/04 16:39:50  vivaresf
!  DM-ID 318 : traduction des entêtes
!
!  Revision 1.2  2005/02/25 18:03:33  vivaresf
!  Mise a jour des entetes
!
!  Revision 1.1.2.1  2005/03/07 10:09:21  pauh
!  DM 318 : ajout de modeles d atmospheres
!
!  Revision 1.1  2005/02/11 17:08:40  pauh
!  DM 318 : module cps_modele_emcd23 cree a partir du module cps_atm_emcd qui
!           regroupait les deux modeles d emcd dans un seul module. Celui-ci
!           ne contient donc que le modele emcd 2.3
!
!  Revision 1.5.4.1  2005/02/07 13:59:58  pauh
!  dm 318 : version de transfert, dm non achevee
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_emcd23
!
!$Mots-cles
!  ATMOSPHERE MARS EMCD
!
!$Voir-Aussi
!#V
!
!#
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB
  use cps_utilisateur


! Fonctions ou subroutines internes (modele EMCD 2.3)
  private atime_23, eofpb_23, getsi_23, grid4_23, grwpb_23, loadvar_23
  private mars_ltime_23, orbit_23, profi_23, season_23, sol2ls_23, var2d_23
  private var3d_23,max2d_23, min2d_23,gasdev_23, ran1_23

! Informations pour la configuration IHM

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_modele_emcd23.F90 379 2013-02-22 16:59:49Z ffsm $'

  interface cps_season_23
  module procedure season_23
  end interface
!  
  interface cps_orbit_23
  module procedure orbit_23
  end interface


!c     include file defining contants and arrays used by the 
!c     European Martian Climate Database

!c CONSTANTS
!c     dimension  of mean and std. dev.values  in longitude, latitude, 
!c     sigma levels and database universal time
      integer, private :: dimlon,dimlat,dimlevs,dimuti
      parameter(dimlon=96,dimlat=48,dimlevs=25,dimuti=12)
!c     dimensions of EOF values in longitude, latitude, numbers of EOFs 
      integer, private :: dimloneo,dimlateo,dimnevecs,dimeoday
      parameter(dimloneo=18,dimlateo=9,dimnevecs=72,dimeoday=669)
!c     range of latitude, longitude, sigma levels and time for mean 
!c     and std. dev.values
      real(kind=pm_reel), private :: lonmin,lonmax,latmin,latmax,levsmin,levsmax,utimin,utimax
      parameter(lonmin=-180.,lonmax=176.25)
      parameter(latmin=-88.125,latmax=88.125)
      parameter(levsmin=0.9995000,levsmax=0.0000563)
      parameter(utimin=1.,utimax=12.)
!c     range of latitude, longitude, number of EOFs and time for EOFs values 
      real(kind=pm_reel), private :: lonmineo,lonmaxeo,latmineo,latmaxeo,nevecsmin,nevecsmax
      real(kind=pm_reel), private :: daymineo,daymaxeo
      parameter(lonmineo=-180.,lonmaxeo=160.)
      parameter(latmineo=-82.5,latmaxeo=77.5)
      parameter(nevecsmin=1.,nevecsmax=72.)
      parameter(daymineo=1.,daymaxeo=669.)
!c     step of grid in longitude and latitude for mean and std. dev. values
      real(kind=pm_reel), private :: deltal
      parameter(deltal=3.75)
!c     step of grid in longitude and latitude for EOFs values
      real(kind=pm_reel), private :: deltaleo
      parameter(deltaleo=20.)

!c COMMON
!      common /niveau_23/ sigma
!      common /orogra_23/ taborog,tabsubstd
!      common /moyenne_23/ tabps,tabtsurf,tabt,tabrho,tabu,tabv
!      common /stddev_23/ tabsdps,tabsdtsurf,tabsdt,tabsdrho,tabsdu,tabsdv
!      common /eofs_23/ tabeonormu,tabeonormv,tabeonormt,tabeonormp,  &
!          tabeonormr,tabpcsmth,tabpcvar,tabeops,tabeot,tabeorho,tabeou,tabeov 

!c     sigma level array
      real, save, private :: sigma(dimlevs)
!c     orographic data array
      real, save, private :: taborog(dimlon,dimlat,1)
!c     orographic variance array
      real, save, private  :: tabsubstd(dimlon,dimlat,1)
!c     mean value arrays : surface pressure, surface temperature
!c     temperature, density, zonal and meriodional wind components
! (dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabps => NULL()
! (dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabtsurf => NULL()
! (dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabt => NULL()
! (dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabrho => NULL()
! (dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabu => NULL()
! (dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabv => NULL()

!c standard  deviation value arrays : surface, pressure, surface temperature
!c temperature, density, zonal and meriodional wind components
! (dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: tabsdps => NULL()
! (dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: tabsdtsurf => NULL()
! (dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdt => NULL()
! (dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdrho => NULL()
! (dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdu => NULL()
! (dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdv => NULL()
!c     arrays related to EOFs
!c     normalisation factors for zonal wind, meriodional wind, temperature
!c     pressure and density
      real, save, private  :: tabeonormu(dimlateo)
      real, save, private  :: tabeonormv(dimlateo)
      real, save, private  :: tabeonormt(dimlateo)
      real, save, private  :: tabeonormp(dimlateo)
      real, save, private  :: tabeonormr(dimlateo)
!c     smoothed PCs
! (dimlateo,dimeoday,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabpcsmth => NULL()
!c     PCs variance
! (dimlateo,dimeoday,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabpcvar => NULL()
!c     EOFS for surface pressure, temperature, density, zonal wind
!c     and meriodional wind
! (dimloneo,dimlateo,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabeops => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeot => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeorho => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeou => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs) 
      real, save, private, dimension(:,:,:,:), pointer :: tabeov => NULL()

! tableaux alloués oui/non
      logical, save, private :: modele_init=.false.

contains

      subroutine cps_atmemcd_23(xz,xlat,xlon,xdate,dset,scena,typper,invar &
          ,seedout,ikey,pres,ro,temp,ventu,ventv,meanvar,extvar,ier,reset)
!**********************************************************************
!$<AM-V1.0>
!
!$Nom
!     cps_atmemcd_23
!
!$Resume
!     Calcul de variables météorologiques avec la Base de données 
!     Européenne des Climats Martiens. (EMCD, version 2.3)
!
!$Description
!     Calcul de variables météorologiques avec la Base de données 
!     Européenne des Climats Martiens. (EMCD, version 2.3)
!     
!     Les données sont cherchées dans les données distribuées par
!     COMPAS (Modèles d'atmosphère martienne), sous le répertoire "data_emcd_2.3"
!     L'utilisateur peut aussi initialiser la variable "dset"
!     avec un nom de répertoire pour accéder à un jeu de données en local.
!     Les anciens appels (dset='s' ou 'b') ne sont plus maintenus.
!
!$Acces
!  PUBLIC
!
!$Version
!     2.2 23/03/00 - modified version for the new MCD release
!     3.0 05/10/99 - CNES version number
!     
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Author
!     C. HOURTOLLE
!
!     Modifications -- SRL
!     dset :  now a string for more flexibility
!             can usually be a single space to get the default data set
!     invar : a new argument added to list to allow control of variability
!             model features, at the moment just small-scale wavelength
!             Code added to set and test lamda
!
!$Usage
!     call cps_atmemcd_23(xz,xlat,xlon,xdate,dset,scena,typper,seedout,ikey,
!                  pres,ro,temp,ventu,ventv,meanvar,extvar,ier)
!
!$Arguments
!>E xz    : <PM_REEL>  Hauteur  / surface (m)
!>E xlat  : <PM_REEL> Latitude (rad)
!>E xlon  : <PM_REEL> Longitude Est(rad)
!>E xdate : <PM_REEL> Date (jours juliens)
!>E dset  : <character*(*)>    Jeu de données :
!. Un ou plusieurs blancs pour avoir les données par défaut (répertoire distribué par COMPAS)
!.     ou Répertoire des données EMCD avec slash final (ex : '/dir/path/'),
!.     ou un lien sur le répertoire des données EMCD  (ex : 'EMCD_DATA/').
!>E scena : <integer> Scénario de poussières : 
!.                           1 = modèle Viking,
!.                           2 = modèle faible,
!.                           3 = tempête (tau=2),
!.                           4 = tempête (tau=5).
!>E typper: <PM_REEL, dim=2> Type de perturbation
!.   aucune : typper(1)= 1. typper(2)=0.
!.   grande échelle : typper(1)= 2. typper(2)=seed number
!.   petite échelle : typper(1)= 3. typper(2)=seed number
!.   grande et petite échelle : typper(1)= 4. typper(2)=seed number
!             (seed number shall be the same for all the trajectory)
!.   n fois l'écart type : typper(1)= 5. typper(2)=n
!>E invar : <PM_REEL> Valeurs controlant la variabilité des modèles
!.     invar= Longueur d'onde des perturbations dues aux ondes de gravités (km)
!.     invar=0 pour avoir la valeur par défaut (16km)
!>E ikey  : <integer> Type de sortie
!.                 0 = pression, densité, température, vent
!.                 1 = calcul aussi les variables statistiques dans extvar
!>S seedout : <PM_REEL> graine des variables aléatoires pour l'atmosphère suivante
!>S pres    : <PM_REEL> pression (Pa)
!>S ro      : <PM_REEL> densité (kg/m^3)
!>S temp    : <PM_REEL> température (K)
!>S ventu   : <PM_REEL> composante zonale du vent (Est-Ouest)
!>S ventv   : <PM_REEL> composante méridionale du vent (Nord-Sud)
!>S meanvar : <PM_REEL, dim=5> Tableau des valeurs moyennes
!.                 meanvar(1)= pression moyenne
!.                 meanvar(2)= densité moyenne
!.                 meanvar(3)= température moyenne
!.                 meanvar(4)= composante zonale moyenne du vent
!.                 meanvar(5)= composante méridionale moyenne du vent
!>S extvar : <PM_REEL, dim=14>  Tableau de 14 variables statistiques :
!.                 extvar(1)= valeur maximum de la densité (kg/m^3)
!.                 extvar(2)= valeur minimum de la densité (kg/m^3)
!.                 extvar(3)= écart type sur la densité (kg/m^3)
!.                 extvar(4)= perturbations sur la densité (kg/m^3)
!.                 extvar(5)= Hauteur à pression "pressure" H(p)
!.                 extvar(6)= Hauteur à densité "rho" H(rho) (km)
!.                 extvar(7)= inutilisé
!.                 extvar(8)= inutilisé
!.                 extvar(9)= inutilisé
!.                 extvar(10)= Température moyenne à la surface du sol (K)
!.                 extvar(11)= Température maximum à la surface du sol (K)
!.                 extvar(12)= Température minimum à la surface du sol (K)
!.                 extvar(13)= Perturbations à petites échelle (ondes de gravité) (kg/m^3)
!.                 extvar(14)= Hauteur orographique(m)
!>S ier  : <integer> code retour
!.                 0 = OK
!.                 1 = Base de données inconnue
!.                 2 = scénario inconnu
!.                 3 = type de perturbation inconnu
!.                 4 = objet souterrain
!.                 5 = Pas de tempêtes de poussières pour cette saison
!.                 6 = impossible d'ouvrir le fichier de base de données
!.                 7 = impossible de charger les données du fichier
!.                 8 = sigmas indeterminés
!
!$Include
!    constants_atmemcd_23.h
!$<>
!**********************************************************************
!
!
!$Type
!     Subroutine 
!
!$Name
!     cps_atmemcd_23
!
!$Resume
!     Computation of meteorological variables with the 
!     European Martian Climate Database
!
!$Description
!     Computation of meteorological variables with the 
!     European Martian Climate Database, version 2.3.
!     
!     Les donnees sont cherchees dans les donnees distribuees par
!     COMPAS (Modeles d'atmosphere martienne), sous le repertoire "data_emcd_2.3"
!     L'utilisateur peut aussi initialiser la variable "dset"
!     avec un nom de repertoire pour acceder a un jeu de données en local
!     Les anciens appels (dset='s' ou 'b') ne sont plus maintenus.
!
!$Version
!     2.2 23/03/00 - modified version for the new MCD release
!     3.0 05/10/99 - CNES version number
!     
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Author
!     C. HOURTOLLE
!
!     Modifications -- SRL
!     dset :  now a string for more flexibility
!             can usually be a single space to get the default data set
!     invar : a new argument added to list to allow control of variability
!             model features, at the moment just small-scale wavelength
!             Code added to set and test lamda
!
!$Use
!     call cps_atmemcd_23(xz,xlat,xlon,xdate,dset,scena,typper,seedout,ikey,
!                  pres,ro,temp,ventu,ventv,meanvar,extvar,ier)
!
!$Arguments
!>E xz    : <double precision> height  / zero datum (m)
!>E xlat  : <double precision> latitude (rad)
!>E xlon  : <double precision> longitude East(rad)
!>E xdate : <double precision> date (julian days)
!>E dset  : <character*(*)>    data set
!>E                            One or more blanks to get the default data set,
!>E                            or full directory path of MCD data required
!>E                            including trailing slash (e.g. '/dir/path/'),
!>E                            or a link to a directory (e.g. 'EMCD_DATA/').
!>E scena : <integer>          scenario 
!>E                            1 = Viking dust, 
!>E                            2 = low dust, 
!>E                            3 = dust storm tau=2,
!>E                            4 = dust storm tau=5,
!>E typper: <double precision> dim 2  perturbation type
!>E                          none : typper(1)= 1. typper(2)=0.
!>E                   large scale : typper(1)= 2. typper(2)=seed number
!>E                   small scale : typper(1)= 3. typper(2)=seed number
!>E         large and small scale : typper(1)= 4. typper(2)=seed number
!>E             (seed number shall be the same for all the trajectory)
!>E n times the standard deviation : typper(1)= 5. typper(2)=n
!>E invar : <double precision> Values controlling the variability models
!>E                 invar(1)= wavelength of gravity wave perturbation (km)
!>E                           set to zero to get default (16km)
!>E ikey  : <integer> output type
!>E                 0 = pressure, density, temperature, wind
!>E                 1 = compute also extra variables written in extvar
!>S seedout : <double precision> seed number for the next atmosphere
!>S pres  : <double precision> pressure (Pa)
!>S ro    : <double precision> density (kg/m^3)
!>S temp  : <double precision> temperature (K)
!>S ventu : <double precision> zonal wind component (East-West)
!>S ventv : <double precision> meridional wind component (North-South)
!>S meanvar : <double precision> dim 5 mean values array
!>S                 meanvar(1)= mean pressure
!>S                 meanvar(2)= mean density
!>S                 meanvar(3)= mean temperature
!>S                 meanvar(4)= mean zonal wind component
!>S                 meanvar(5)= mean meridional wind component
!>S extvar : <double precision> dim 14  extra variables array
!>S                 extvar(1)= upper density value (kg/m^3)
!>S                 extvar(2)= lower density value (kg/m^3)
!>S                 extvar(3)= density standard deviation (kg/m^3)
!>S                 extvar(4)= random density perturbation (kg/m^3)
!>S                 extvar(5)= scale height H(p)
!>S                 extvar(6)= density scale height H(rho) (km)
!>S                 extvar(7)= reserved
!>S                 extvar(8)= reserved
!>S                 extvar(9)= reserved
!>S                 extvar(10)= Mean ground temperature (K)
!>S                 extvar(11)= Maximum ground temperature (K)
!>S                 extvar(12)= Minimum ground temperature (K)
!>S                 extvar(13)= Small Scale perturbation (gravity wave)(kg/m^3)
!>S                 extvar(14)= orographic height(m)
!>S ier  : <integer> error flag
!>S                 0 = OK
!>S                 1 = unknown database
!>S                 2 = unknown scenario
!>S                 3 = unknown perturbation type
!>S                 4 = underground object
!>S                 5 = no dust storm for the season
!>S                 6 = impossible to open database file
!>S                 7 = impossible to load data from database
!>S                 8 = undetermined sigma levels
!
!$Include
!    constants_atmemcd_23.h

      implicit none

! #include "constants_atmemcd_23.h"

      real(kind=PM_REEL) :: xz,xlat,xlon,xdate,typper(2),invar
      real(kind=PM_REEL) :: seedout,pres,ro,temp,ventu,ventv,extvar(14) &
          ,meanvar(5)
      integer :: scena,ikey,ier
      
      !!! variable ajoutee pour satisfaire les tests de non regression
      !!! qui etaient realises independemment les uns des autres dans
      !!! la LIB_ATMMARS et sont desormais enchaines les uns a la suite
      !!! des autres dans COMPAS. Si on passe la valeur 1 a reset, toutes
      !!! les variables "SAVE" de la routine sont reinitialisees.
      integer,intent(in),optional :: reset
      

      character(len=*) :: dset

!c     channel number :
!c     60 DRS dic file: mean data
!c     61 DRS dat file: mean data
!c     62 DRS dic file: std dev data
!c     63 DRS dat file: std dev data
!c     64 DRS dic file: orography data
!c     65 DRS dat file: orography data
!c     66 DRS dic file: eof data
!c     67 DRS dat file: eof data

!c     local variables
!c     DRS channel number -  orographic data
      integer :: udrs
!      parameter(udrs=64)
!c     DRS channel number - mean fields      
      integer :: udrsm
!c     DRS channel number - std dev fields
      integer :: udrssd
!c     DRS channel number - eof fields
      integer :: ueof
!      parameter(udrsm=60,udrssd=62,ueof=66)
      ! temoin d ouverture des fichiers
      integer :: file_open
!c     seed for random number generator
      integer :: seed
      real(kind=PM_REEL)   :: R 
      parameter(R=191.2)
      real(kind=PM_REEL)   :: g=3.72_PM_REEL
!c     Intensity of dust storm
      real(kind=PM_REEL)    :: intens
!c     dust scenario
      integer  :: dust
!c     perturbation type
      integer :: varflag
!c     number of standard deviation
      real(kind=PM_REEL)  :: nbsig
      real(kind=PM_REEL)  :: pi 
      parameter(pi=3.14159265359)
      real(kind=PM_REEL)   :: crd
      parameter(crd=180./pi)
!c     difference in degree between 2 consecutive points  
!c     leading to change random phase for gravity wave
      real(kind=PM_REEL)  :: deltalgw
      parameter(deltalgw=1.)

      integer  :: i,ierr
      real(kind=PM_REEL)   :: utime
      real(kind=PM_REEL)   ::  height     !height (km)
      real(kind=PM_REEL)   :: oroheight  !orographic height
      real(kind=PM_REEL)   :: absheight  !height above surface
      real(kind=PM_REEL)   :: ls
      real(kind=PM_REEL)   :: sunlon
      real(kind=PM_REEL)   :: sunlat
      real(kind=PM_REEL)   :: marsau
      real(kind=PM_REEL)   :: localtime
      real(kind=PM_REEL)   :: lon        !longitude west  {0 .. 360]
      real(kind=PM_REEL)   :: lat        !latitude (degrees)
      real(kind=PM_REEL)   :: t
      real(kind=PM_REEL)   :: tl         !l indicates low level variable from database
      real(kind=PM_REEL)   :: ps
      real(kind=PM_REEL)   :: p
      real(kind=PM_REEL)   :: pl
      real(kind=PM_REEL)   :: rho
      real(kind=PM_REEL)   :: rhol
      real(kind=PM_REEL)   :: pertm
      real(kind=PM_REEL)   :: pertr
      real(kind=PM_REEL)   :: pertrho
      real(kind=PM_REEL)   :: pertps
      real(kind=PM_REEL)   :: u
      real(kind=PM_REEL)   :: ul
      real(kind=PM_REEL)   :: v
      real(kind=PM_REEL)   :: vl
      real(kind=PM_REEL)   :: pertu, pertv, pertt
      integer :: levhi
      integer :: levlow
      real(kind=PM_REEL)   :: levweight
      real(kind=PM_REEL)   :: pratio
      real(kind=PM_REEL)   :: lamda        !gravity wave vertical wavelength (km)
      real(kind=PM_REEL)   :: rdnos(dimnevecs)
      real(kind=PM_REEL)   :: dev
      real(kind=PM_REEL)   :: modelday
      character*16 :: name
      real(kind=PM_REEL)   :: pscaleheight      !scale height
      real(kind=PM_REEL)   :: sdrhol, rholol, rhohil, pertmrho, pertrrhol
      real(kind=PM_REEL)   :: pertrrho, rholo, rhohi, sdrho
      real(kind=PM_REEL)   :: rscaleheight   !Density scale height
      real(kind=PM_REEL)   :: tsurf,tmin,tmax
      real(kind=PM_REEL)   :: tmeanl(5)
      character*4 :: typevar
      character*255 :: dataset
      integer :: lendataset
!c     season numbers
      integer :: nums
      integer :: numsprec=0
      integer :: numsprec_eo=0
      integer :: numsprec_sd=0
      integer :: numsprec_gw=0
      integer :: seed_prec=0
!c     previous scenario
      integer :: dustprec=0
!c     previous latitude and longitude
      real(kind=PM_REEL) :: prevlon=0._PM_REEL
      real(kind=PM_REEL) :: prevlat=0._PM_REEL
      save numsprec,numsprec_eo,numsprec_sd,numsprec_gw &
          ,seed_prec,dustprec,prevlat,prevlon

! Variable pour la gestion d'erreur de allocate/deallocate
      integer :: ierralloc

! ===============================================================

      ier=0
      
      ! pour l instant pas de fichiers ouverts
      file_open = 0

      if (present(reset)) then
         if (reset.eq.1) then
            numsprec = 0
            numsprec_eo = 0
            numsprec_sd = 0
            numsprec_gw = 0
            seed_prec = 0
            dustprec = 0
            prevlon = 0._PM_REEL
            prevlat = 0._PM_REEL
         endif
      endif

!c     inputs check and conversions
!c     **********************
!c
!c     data set
!c     find last non-blank character
      do lendataset = len(dset), 1, -1
         if (dset(lendataset:lendataset).ne.' ') go to 100
      end do
  100 continue
      if (lendataset.eq.0) then  !default data set case
!       acces au repertoire contenant les fichiers du modele par COMPAS
         ier = cps_getFichierModele("atmosphere", "EMCD2_3", dataset, &
           rep=.true.)
         if (MSP_gen_messages("cps_atmemcd_23")) return
! Valeur attendue un pointeur sur :
!         dataset = '/usr/local_ms/data/data_emcd_2.3/'
!         lendataset = 10
      else  !symbolic link or full path given explicitly
         dataset = dset
      end if
      lendataset = len_trim(dataset)
      if (dataset(lendataset:lendataset).ne."/") then
         dataset = trim(dataset) // "/"
         lendataset=lendataset+1
      endif

!c     scenario
      dust = scena
!c     set 'intens' to the value corresponding to the scenario
      if (dust.eq.1) then
         intens=1.0
      else if (dust.eq.2) then
         intens=0.0    !TBC
      else if (dust.eq.3) then
         intens=2.0
      else if (dust.eq.4) then
         intens=3.0
      else
         write(0,*)'ATMEMCD Error: ',scena,' unknown scenario'
         ier=2
         goto 9999
      endif

!c     perturbations
      varflag = int(typper(1))
      if ((varflag.le.0).OR.(varflag.gt.5)) then
         write(0,*)'ATMEMCD Error: ',varflag &
             ,' unknown  perturbation type'
         ier=3
         goto 9999
      end if
      if (((varflag.ge.2).and.(varflag.le.4)).or.(ikey.eq.1)) then
!c     gaussian deviates for eofs model and square dist. random number 
!c     for gravity wave are computed one time per trajectory
!c     i.e. computed if seed number changes
         if ((varflag.eq.1).or.(varflag.eq.5)) then
!c     random numbers are needed for extra variable 
!c     = no seed number specified by the user
!c     the seed number is forced to -1
            seed=-1
         else if ((varflag.ge.2).and.(varflag.le.4)) then
!c     the seed number has to be a negative integer
            if (int(typper(2)).eq.0) then
               seed=-1
            else if (int(typper(2)).gt.0) then
               seed=-int(typper(2))
            else
               seed=int(typper(2))
            endif
         endif
!c     if new seed number is given recompute rdnos and dev
         if (seed_prec.ne.seed) then
            seed_prec=seed
            do i=1,dimnevecs
               rdnos(i)=gasdev_23(seed)
            enddo
            dev=ran1_23(seed)
            seedout=real(-seed,kind=PM_REEL)
         endif
      else
         seedout=0.d0
      endif
!c     case  n std dev perturbation
      if (varflag.eq.5) then
         nbsig = real(typper(2),kind=PM_REEL)
      else
         nbsig=0.
      end if

!c     set wavelength for small-scale (gravity wave) perturbations
      if (invar.eq.0.0) then
         lamda = 16.0  !default wavelength
      else if (invar.lt.2.0) then
         write(0,*)'ATMEMCD Error: small-scale wavelength too small,', &
             ' set to 2km'
         lamda = 2.0
      else if (invar.gt.30.0) then
         write(0,*)'ATMEMCD Error: small-scale wavelength too large,', &
             ' set to 30km'
         lamda = 30.0
      else
         lamda = invar
      endif

!c     convert longitude (rd) East to longitude (deg) West between 0..360 degrees
      lon=real(xlon,kind=PM_REEL)*crd
      lon=mod(lon,360._PM_REEL)
      if(lon.lt.0.) then
         lon=-lon
      elseif (lon.gt.0.) then
         lon=360._PM_REEL-lon
      endif
!c     convert latitude from radians to degrees
      lat=real(xlat,kind=PM_REEL)*crd
!c     convert height from  m to km
      height = real(xz,kind=PM_REEL)/1000.

!c     if you moved a large distance, recompute gravity wave random phase dev
      if((varflag.eq.3).or.(varflag.eq.4).or.(ikey.eq.1)) then
         if ((abs(prevlat-lat).gt.deltalgw).or. &
             (abs(prevlon-lon).gt.deltalgw)) then
            dev=ran1_23(seed)
            seedout=real(-seed,kind=PM_REEL)
         endif
      endif
      prevlon=lon
      prevlat=lat

!c     Compute  Ls and the corresponding season number
!c     *************************************************
      call orbit_23(xdate,sunlat,sunlon,ls,marsau,modelday)
      call season_23(ls,nums)
!c     call mars_ltime with current longitude for correct local time
      if ((dust.ge.3).and.((nums.lt.8).or.(nums.gt.10))) then
         !!! comment nums peut-il etre a la fois inferieur a 8 et superieur a 10 ?!?
         write(0,*)'ATMEMCD Error : no dust storm for season ',nums
         ier=5
         goto 9999
      endif
      call mars_ltime_23(lon,sunlon,localtime)
      call atime_23(localtime,utime,lon)

!c     load arrays  from data base
!c     ********************************************
!c     if the scenario changes, reset season numbers to reload all needed arrays
      if (dust.ne.dustprec) then
         numsprec=0
         numsprec_eo=0
         numsprec_sd=0
         numsprec_gw=0
         dustprec=dust
      endif

      if (.not.modele_init) then
!
! allocation des variables globales :
!
         if (associated(tabps))   deallocate(tabps, stat=ierralloc)
         if (associated(tabtsurf))deallocate(tabtsurf, stat=ierralloc)
         if (associated(tabt))    deallocate(tabt, stat=ierralloc)
         if (associated(tabrho))  deallocate(tabrho, stat=ierralloc)
         if (associated(tabu))    deallocate(tabu, stat=ierralloc)
         if (associated(tabv))    deallocate(tabv, stat=ierralloc)
         allocate(tabps(dimlon,dimlat,dimuti))
         tabps(:,:,:)=0._PM_REEL
         allocate(tabtsurf(dimlon,dimlat,dimuti))
         tabtsurf(:,:,:)=0._PM_REEL
         allocate(tabt(dimlon,dimlat,dimlevs,dimuti))
         tabt(:,:,:,:)=0._PM_REEL
         allocate(tabrho(dimlon,dimlat,dimlevs,dimuti))
         tabrho(:,:,:,:)=0._PM_REEL
         allocate(tabu(dimlon,dimlat,dimlevs,dimuti))
         tabu(:,:,:,:)=0._PM_REEL
         allocate(tabv(dimlon,dimlat,dimlevs,dimuti))
         tabv(:,:,:,:)=0._PM_REEL

         if (associated(tabsdps))   deallocate(tabsdps, stat=ierralloc)
         if (associated(tabsdtsurf))deallocate(tabsdtsurf, stat=ierralloc)
         if (associated(tabsdt))    deallocate(tabsdt, stat=ierralloc)
         if (associated(tabsdrho))  deallocate(tabsdrho, stat=ierralloc)
         if (associated(tabsdu))    deallocate(tabsdu, stat=ierralloc)
         if (associated(tabsdv))    deallocate(tabsdv, stat=ierralloc)
         allocate(tabsdps(dimlon,dimlat,1))
         tabsdps(:,:,:)=0._PM_REEL
         allocate(tabsdtsurf(dimlon,dimlat,1))
         tabsdtsurf(:,:,:)=0._PM_REEL
         allocate(tabsdt(dimlon,dimlat,dimlevs,1))
         tabsdt(:,:,:,:)=0._PM_REEL
         allocate(tabsdrho(dimlon,dimlat,dimlevs,1))
         tabsdrho(:,:,:,:)=0._PM_REEL
         allocate(tabsdu(dimlon,dimlat,dimlevs,1))
         tabsdu(:,:,:,:)=0._PM_REEL
         allocate(tabsdv(dimlon,dimlat,dimlevs,1))
         tabsdv(:,:,:,:)=0._PM_REEL

         if (associated(tabpcsmth))   deallocate(tabpcsmth, stat=ierralloc)
         if (associated(tabpcvar))    deallocate(tabpcvar, stat=ierralloc)
         allocate(tabpcsmth(dimlateo,dimeoday,dimnevecs))
         tabpcsmth(:,:,:)=0._PM_REEL
         allocate(tabpcvar(dimlateo,dimeoday,dimnevecs))
         tabpcvar(:,:,:)=0._PM_REEL

         if (associated(tabeops))   deallocate(tabeops, stat=ierralloc)
         if (associated(tabeot))    deallocate(tabeot, stat=ierralloc)
         if (associated(tabeorho))  deallocate(tabeorho, stat=ierralloc)
         if (associated(tabeou))    deallocate(tabeou, stat=ierralloc)
         if (associated(tabeov))    deallocate(tabeov, stat=ierralloc)
         allocate(tabeops(dimloneo,dimlateo,dimnevecs))
         tabeops(:,:,:)=0._PM_REEL
         allocate(tabeot(dimloneo,dimlateo,dimlevs,dimnevecs))
         tabeot(:,:,:,:)=0._PM_REEL
         allocate(tabeorho(dimloneo,dimlateo,dimlevs,dimnevecs))
         tabeorho(:,:,:,:)=0._PM_REEL
         allocate(tabeou(dimloneo,dimlateo,dimlevs,dimnevecs))
         tabeou(:,:,:,:)=0._PM_REEL
         allocate(tabeov(dimloneo,dimlateo,dimlevs,dimnevecs))
         tabeov(:,:,:,:)=0._PM_REEL

         modele_init=.true.

      endif

!c     if the season number changes, mean value have to be reloaded
      if (nums.ne.numsprec) then
!c     Open appropriate  DRS file
         call opend_23(udrs,udrsm,udrssd,ueof,nums,dust, &
                   dataset(1:lendataset),file_open,ierr)
         if (ierr.ne.0) then
            ier=6
            goto 9999
         endif
         typevar='mean'
         call loadvar_23(udrsm,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
!c     at the first call, load sigma and orographic data
         if (numsprec.eq.0) then
            typevar='sigm'
            call loadvar_23(udrsm,typevar,ierr)
            if (ierr.ne.0) then
               ier=7
               go to 9999
            endif
            typevar='orog'
            call loadvar_23(udrs,typevar,ierr)
            if (ierr.ne.0) then
               ier=7
               go to 9999
            endif
         end if
         numsprec=nums
      end if
!c     if large scale perturbations or extra variables are requested, 
!c     load data if not yet done   
      if (((varflag.eq.2).or.(varflag.eq.4).or.(ikey.eq.1)) &
          .and.(nums.ne.numsprec_eo)) then
         typevar='eofs'
         call loadvar_23(ueof,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
         numsprec_eo=nums
      end if
!c     if small scale perturbations or extra variables are requested, 
!c     load data if not yet done
      if (((varflag.eq.3).or.(varflag.eq.4).or.(ikey.eq.1)) &
          .and.(nums.ne.numsprec_gw)) then
         typevar='grwp'
         call loadvar_23(udrs,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
         numsprec_gw=nums
      end if
!c     if n std dev perturbations or extra variables are requested, 
!c     load data if not yet done
      if (((varflag.eq.5).or.(ikey.eq.1)) &
          .and.(nums.ne.numsprec_sd)) then
         typevar='stdv'
         call loadvar_23(udrssd,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
         numsprec_sd=nums
      end if
      
      if (file_open.eq.1) then
        close(udrs)
	close(udrsm)
	close(udrssd)
	close(ueof)
      endif
      

!c     *************************************
!c     *Meteorological variable computation*
!c     *************************************

!c     calculate oroheight
      name='orography'
      call var2d_23(oroheight,lon,lat,1.0_PM_REEL,name,ierr)
      extvar(14)=oroheight
!c     calculate sigma level at height=absheight
!c     (absheight=height above ground)
      absheight=height-oroheight/1000.

!c     if underground : stop
      if (absheight.lt.0.) then
         write(0,*)'ATMEMCD Error: underground object '
         ier=4
         goto 9999
      endif
      call getsi_23(lon,lat,absheight,utime, &
          levhi,levlow,levweight,pratio,ierr)
      if (ierr.ne.0) then
         write(0,*)'ATMEMCD Error: undefined sigma levels'
         ier=8
         goto 9999
      end if
!c     read mean data from stored arrays.
      name='u'
      call var3d_23(ul,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='v' 
      call var3d_23(vl,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='t'
      call var3d_23(tl,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='rho'
      call var3d_23(rhol,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)         
      name='ps' 
      call var2d_23(ps,lon,lat,utime,name,ierr)
!c     storage of mean values
      tmeanl(1)=pratio*ps*(sigma(levlow) &
          +(sigma(levhi)-sigma(levlow))*levweight)
      tmeanl(2)=rhol
      tmeanl(3)=tl
      tmeanl(4)=ul
      tmeanl(5)=vl
      do i=1,5
         meanvar(i)=real(tmeanl(i),kind=PM_REEL)
      enddo

!c     add large scale variability to u,v,t and ps if required
      if ((varflag.eq.2).or.(varflag.eq.4)) then
         name='rho'
         call eofpb_23(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         rhol=rhol+pertr
         name='u'
         call eofpb_23(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         ul=ul+pertr
         name='v'
         call eofpb_23(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         vl=vl+pertr
         name='t'
         call eofpb_23(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         tl=tl+pertr
         name='ps'
         call eofpb_23(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         ps=ps+pertr
      endif

!c     add small scale (gravity wave) variability to u,v,t,rho if required
      if ((varflag.eq.3).or.(varflag.eq.4)) then
         name='u'
         call grwpb_23(pertu,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='v'
         call grwpb_23(pertv,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='t'
         call grwpb_23(pertt,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='rho'
         call grwpb_23(pertrho,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         ul = ul + pertu
         vl = vl + pertv
         tl = tl + pertt
         rhol= rhol + pertrho
      endif

!c     add n sigmas if required
      if (varflag.eq.5) then
         name='sdu'
         call var3d_23(pertu,lon,lat,levhi,levlow,levweight,pratio, &
             1.0_PM_REEL,name,ierr)
         name='sdv'
         call var3d_23(pertv,lon,lat,levhi,levlow,levweight,pratio, &
             1.0_PM_REEL,name,ierr)
         name='sdrho'
         call var3d_23(pertrho,lon,lat,levhi,levlow,levweight,pratio, &
             1.0_PM_REEL,name,ierr)
         name='sdt'
         call var3d_23(pertt,lon,lat,levhi,levlow,levweight,pratio, &
             1.0_PM_REEL,name,ierr)
         name='sdps'
         call var2d_23(pertps,lon,lat,1.0_PM_REEL,name,ierr)
         ul = ul + (nbsig*pertu)
         vl = vl + (nbsig*pertv)
         rhol = rhol + (nbsig*pertrho)
         tl = tl + (nbsig*pertt)
         ps = ps + (nbsig*pertps)
      endif

!c     calculate pression
      pl=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow))*levweight)
      pl=pratio*pl

!c     compute extra variables relative to scale height and density
      if (ikey.eq.1) then
         pscaleheight=R*tl/g
         pscaleheight=pscaleheight/1000.
         rscaleheight=pscaleheight
         name='sdrho'
         call var3d_23(sdrhol,lon,lat,levhi,levlow,levweight,pratio, &
             1.0_PM_REEL,name,ierr)
         rholol=tmeanl(2)-sdrhol
         rhohil=tmeanl(2)+sdrhol
         name='rho'
         call eofpb_23(pertmrho,pertrrhol,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         pertrrho=pertrrhol
         call grwpb_23(pertr,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         rholol=rholol-abs(pertr)
         rhohil=rhohil+abs(pertr)
         rholo=rholol
         rhohi=rhohil
         sdrho=sdrhol
      endif
!c     
      t=tl
      p=pl
      rho=rhol
      u=ul
      v=vl

      if (ikey.eq.1) then
!c     compute extra variables relative to ground temperatures
         name='tsurf'
         call var2d_23(Tsurf,lon,lat,utime,name,ierr)
         call max2d_23(Tmax,lon,lat,name,ierr)
         call min2d_23(Tmin,lon,lat,name,ierr)
!c     affect value to extra variables
         extvar(1)=real(rhohi,kind=PM_REEL)
         extvar(2)=real(rholo,kind=PM_REEL)
         extvar(3)=real(sdrho,kind=PM_REEL)
         extvar(4)=real(pertrrho,kind=PM_REEL)
         extvar(5)=real(pscaleheight,kind=PM_REEL)
         extvar(6)=real(rscaleheight,kind=PM_REEL)
         extvar(7)=-999.0d0
         extvar(8)=-999.0d0
         extvar(9)=-999.0d0
         extvar(10)=real(tsurf,kind=PM_REEL)
         extvar(11)=real(tmax,kind=PM_REEL)
         extvar(12)=real(tmin,kind=PM_REEL)
         extvar(13)=real(pertr,kind=PM_REEL)
      else
         do i=1,13
            extvar(i)=0.d0
         enddo
      endif

!c     convert to double precision
      pres=real(p,kind=PM_REEL)
      ro=real(rho,kind=PM_REEL)
      temp=real(t,kind=PM_REEL)
      ventu=real(u,kind=PM_REEL)
      ventv=real(v,kind=PM_REEL)

      return


!c     Error handling : all the outputs are set to a missing data value
 9999 pres=-999.d0
      ro=-999.d0
      temp=-999.d0
      ventu=-999.d0
      ventv=-999.d0
      do i=1,5
         meanvar(i)=-999.d0
      end do
      do i=1,14
         extvar(i)=-999.d0
      enddo
      return

    end subroutine cps_atmemcd_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine atime_23(lt,ut,lonw)
!c
!c     Convert local time, lt (0..24) at west longitude=lonw to 
!c     database universal time level, ut (1..12).
!c     Note that it is midnight at longitude=0.0 at universal time=1.

      implicit none
!c
!c     input
!c
      real(kind=PM_REEL) :: lt       !local time
      real(kind=PM_REEL) :: lonw     !longitude west
!c
!c     output
!c
      real(kind=PM_REEL) :: ut       !universal time level
!c
!c     local
!c
      real(kind=PM_REEL) :: lon      !longitude east
      integer :: iut

      lon=360.-lonw
      iut=mod(nint(-lon/30.+lt/2.+12.),12)+1
      ut=real(iut,kind=PM_REEL)
      if (iut.eq.13) ut=1.0
      return
    end subroutine atime_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine eofpb_23(pertm,pertr,rdnos,lonw,lat,levhi,levlow, &
                      levweight,pratio,day,name,ier)
!c
!c     Calculate an EOF perturbation on the 3d variable=name at 
!c     longitude=lon, latitude=lat sigma=sigma and day=day.
!c
!c     Improved variability model...EOFs calculated in longitude-sigma plane
!c
      implicit none

! #include "constants_atmemcd_23.h"
!c
!c     input
!c
      real(kind=PM_REEL)          :: rdnos(dimnevecs) !Uniform deviates for a single profile
      real(kind=PM_REEL)          :: lonw      !longitude west of point 
      real(kind=PM_REEL)          :: lat       !latitude of point
      integer       :: levhi     !database level upper bound
      integer       :: levlow    !database level lower bound
      real(kind=PM_REEL)          :: levweight !level weight for interpolation
      real(kind=PM_REEL)          :: pratio    !ratio of pressure to extreme value if out of range
      real(kind=PM_REEL)          :: day       !model day
      character*16  :: name      !Name of variable
!c
!c     output
!c
      integer :: ier          !error flag (0=OK, 1=NOK)
      real(kind=PM_REEL)    :: pertm        !perturbation corresponding to trend
      real(kind=PM_REEL)    :: pertr        !perturbation corresponding to random component
!c
!c     local variables
!c
      integer :: i
      integer :: indlat,indlon,indday
!c      integer indhig,indlow
      real(kind=PM_REEL)    :: norm
      real(kind=PM_REEL)    :: pcsmth(dimnevecs)
      real(kind=PM_REEL)    :: pcvar(dimnevecs)
      real(kind=PM_REEL)    :: evecs(dimnevecs)
      real(kind=PM_REEL)    :: lon         !longitude between -180 and 180
      real(kind=PM_REEL)    :: alat,alon,aday,u 

! Initialisations
      ier = 0
      indlat = 0
      indlon = 0
      indday = 0

!c     determination of the index of the nearest latitude MCD grid point
      if (lat.le.latmineo) then
         indlat=1
      elseif (lat.ge.latmaxeo) then
         indlat=dimlateo
      else
         alat=latmineo
         do i=1,dimlateo-1
            if ((lat.ge.alat).and.(lat.lt.alat+deltaleo)) then
               u=(lat-alat)/deltaleo
               if (u.le.0.5)then
                  indlat=i
               else
                  indlat=i+1
               endif
            endif
            alat=alat+deltaleo
         enddo       
      endif

!c     convert longitude west [0..360[ to longitude east[-180..180[
      if (lonw.eq.0.) then
         lon=0.
      elseif (lonw.le.180.)then
         lon=-lonw
      else
         lon=360.-lonw
      endif

!c     determination of the index of the nearest longitude MCD grid point
      if (lon.ge.lonmaxeo) then
         u=(lon-lonmaxeo)/deltaleo
         if (u.le.0.5)then
            indlon=dimloneo
         else
            indlon=1
         endif
      else
         alon=lonmineo
         do i=1,dimloneo-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltaleo)) then
               u=(lon-alon)/deltaleo
               if (u.le.0.5)then
                  indlon=i
               else
                  indlon=i+1
               endif
            endif
            alon=alon+deltaleo
         enddo
      endif

!c     determination of the nearest database day
      if (day.ge.daymaxeo) then
         u=day-daymaxeo
         if (u.le.0.5)then
            indday=dimeoday
         else
            indday=1
         endif
      else
         aday=daymineo
         do i=1,dimeoday-1
            if ((day.ge.aday).and.(day.lt.aday+1.)) then
               u=day-aday
               if (u.le.0.5)then
                  indday=i
               else
                  indday=i+1
               endif
            endif
            aday=aday+1.
         enddo
      endif

      
!c     read  normalisation factor and eofs
!c     normalisation factor is no longer the standard deviation
!c
      if (name.eq.'u') then
         norm=tabeonormu(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeou(indlon,indlat,levlow,i) &
                  +  (tabeou(indlon,indlat,levhi,i) &
                  -  tabeou(indlon,indlat,levlow,i))*levweight
         enddo
      elseif (name.eq.'v') then
         norm=tabeonormv(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeov(indlon,indlat,levlow,i) &
                  +  (tabeov(indlon,indlat,levhi,i) &
                  -  tabeov(indlon,indlat,levlow,i))*levweight
         enddo
      elseif (name.eq.'t') then
         norm=tabeonormt(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeot(indlon,indlat,levlow,i) &
                  +  (tabeot(indlon,indlat,levhi,i) &
                  -  tabeot(indlon,indlat,levlow,i))*levweight
         enddo
      elseif (name.eq.'rho') then
         norm=tabeonormr(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeorho(indlon,indlat,levlow,i) &
                  +  (tabeorho(indlon,indlat,levhi,i) &
                  -  tabeorho(indlon,indlat,levlow,i))*levweight
         enddo
      elseif (name.eq.'ps') then
         norm=tabeonormp(indlat)
         do i=1,dimnevecs
               evecs(i)= tabeops(indlon,indlat,i)
         enddo
      else
         write(0,*)'EOFPB Error: ',name,' unknown variable name'
         ier=1
         goto 9999
      endif
!c
!c     read smoothed PCs
!c
      do i=1,dimnevecs
         pcsmth(i)=tabpcsmth(indlat,indday,i)
      enddo
!c
!c     read PCs variance
!c
      do i=1,dimnevecs
         pcvar(i)=tabpcvar(indlat,indday,i)
      enddo
!c
!c     calculate perturbation
!c
      pertm=0.0
      pertr=0.0
      do i=1,dimnevecs
        pertm=pertm+pcsmth(i)*evecs(i)
        pertr=pertr+pcvar(i)*rdnos(i)*evecs(i)
      enddo
!c
!c     renormalise
!c
      pertm=pertm*norm
      pertr=pertr*norm
!c
!c re-scaling correction for densities which are out of sigma range
!c
      if (name.eq.'rho') then
         pertm=pratio*pertm
         pertr=pratio*pertr
      endif

      return

!c     error handling
 9999 pertm = 0.0
      pertr = 0.0
      return

    end subroutine eofpb_23
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real(kind=pm_reel) function gasdev_23(idum)
!c
!c     Return a Gaussian deviate with zero mean and unit standard deviation
!c
      implicit none
!c     input
!c
      integer :: idum    !Seed for random number generator
!c
!c     local
!c

      integer :: iset
      data  iset/0/
      real(kind=PM_REEL) :: v1
      real(kind=PM_REEL) :: v2
      real(kind=PM_REEL) :: r
      real(kind=PM_REEL) :: fac
      real(kind=PM_REEL) :: gset=0._PM_REEL
!      real ran1_23,gasdev_23

! Initialisations
!      gset = 0

! Calculs
      if (iset.eq.0) then
1       v1=2.*ran1_23(idum)-1.
        v2=2.*ran1_23(idum)-1.
        r=v1**2+v2**2
        if (r.ge.1.) goto 1
        fac=sqrt(-2.*log(r)/r)
        gset=v1*fac
        gasdev_23=v2*fac
        iset=1
      else
        gasdev_23=gset
        iset=0
      endif
      return
    end function gasdev_23
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsi_23(lonw,lat,height,time, &
                      levhi,levlow,levweight,pratio,ier)
!c
!c     Find the nearest sigma levels and interpolation weights at
!c     longitude=lonw, latitude=lat and height=height.
!c     Also returns p/p(top, bottom) if p is out of range, for
!c     possible extrapolations.
!c
!c     inputs
!c
      implicit none

! #include "constants_atmemcd_23.h"

      real(kind=PM_REEL)    :: height     !height of point
      real(kind=PM_REEL)    :: lonw       !west longitude
      real(kind=PM_REEL)    :: lat        !latitude
      real(kind=PM_REEL)    :: time       !Universal time
!c
!c     outputs
!c
      integer :: ier       !error flag (0=OK, 1=NOK)
      integer :: levhi     !database level upper bound
      integer :: levlow    !database level lower bound
      real(kind=PM_REEL)    :: levweight !level weight for interpolation
      real(kind=PM_REEL)    :: pratio    !p/p(top, bottom) for extrapolation, 1 if in range
!c
!c     locals
!c
      integer :: l,ierr
      real(kind=PM_REEL)   ::  dsigma(dimlevs)
      real(kind=PM_REEL)    :: hsigma(dimlevs+1)
      real(kind=PM_REEL)    :: sheight(dimlevs)
      real(kind=PM_REEL)    :: g=3.72_PM_REEL
      real(kind=PM_REEL)    :: R=191.1_PM_REEL
      real(kind=PM_REEL)    :: Rogct
      character*16 ::  name='t'
      real(kind=PM_REEL)    :: t(dimlevs)
      real(kind=PM_REEL)   ::  lon

! Initialisations
      ier=0
!      dsigma(:) =0._PM_REEL
!      hsigma(:) =0._PM_REEL
!      sheight(:) =0._PM_REEL
!      t(:) =0._PM_REEL


! Calculs

!c     convert longitude west [0..360[ to longitude east[-180..180[
      if (lonw.eq.0.) then
         lon=0.
      elseif (lonw.le.180.)then
         lon=-lonw
      else
         lon=360.-lonw
      endif         

!c
!c     Compute mean and delta sigma for each layer
!c     (hsigma is not the same as the half levels in the model)
!c
      hsigma(1)=1.
      do l=2,dimlevs
        hsigma(l)=.5*(sigma(l-1)+sigma(l))
      enddo
      hsigma(dimlevs+1)=0.
      dsigma(1)=1.-sigma(1)
      do l=2,dimlevs
        dsigma(l)=sigma(l-1)-sigma(l)
      enddo
!c
!c     get temperature profile
!c
      call profi_23(t,lon,lat,time,name,ierr)
      if (ierr.ne.0) then
         write(0,*)'GETSI Error : temperature profile not available'
         ier=1
         goto 9999
      endif
!c
!c     Integrate hydrostatic equation
!c     Rogct is the R/g constant divided by 1000 for km and and 2 to average T
!c
      Rogct=0.0005*R/g
      sheight(1)=(4.0*Rogct*t(1)*dsigma(1))/(sigma(1)+hsigma(1))
      do l=2,dimlevs
        sheight(l)=sheight(l-1) &
                 +(Rogct*(t(l)+t(l-1))*dsigma(l))/hsigma(l)
      enddo
!c
!c     find levhi, levlow and levweight
!c
      if (height.lt.sheight(1)) then
        levhi=1
        levlow=1
        levweight=1.
        pratio=exp(1000.*(sheight(1)-height)*g/(R*t(1)))
      endif
      do l=1,dimlevs-1
        if ((height.ge.sheight(l)).and.(height.lt.sheight(l+1))) then
          levhi=l+1
          levlow=l
          levweight=(height-sheight(levlow)) &
                   /(sheight(levhi)-sheight(levlow))
          pratio=1.
        endif
      enddo
      if (height.ge.sheight(dimlevs)) then
        levhi=dimlevs
        levlow=dimlevs
        levweight=1.
        pratio=exp(1000.*(sheight(dimlevs)-height)*g/(R*t(dimlevs)))
      endif
      return

!c     Errror handling
 9999 levhi=0
      levlow=0
      levweight=0.
      pratio=0.
      return

    end subroutine getsi_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grid4_23(lon,lat,dlon,dlat,t,u)
!c
!c     Given longitude=lon and latitude=lat find the nearest 4 horizontal
!c     gridpoints in the database and bilinear interpolation weights.
!c
!c     Grid points are arranged as in Numerical Recipies page 117
!c
!c     4 3
!c     1 2 
!c
      implicit none

! #include "constants_atmemcd_23.h"

!c     inputs
!c
      real(kind=PM_REEL) :: lon      !east longitude of point
      real(kind=PM_REEL) :: lat      !latitude of point
!c
!c     outputs
!c
      integer :: dlon(4)  !indices longitudes of database points
      integer :: dlat(4)  !indices latitudes of database points
      real(kind=PM_REEL) :: t        !weight
      real(kind=PM_REEL) :: u        !weight
!c
!c     local
!c
      real(kind=PM_REEL) ::  alon,alat
      integer :: i

! INitialisations
!      alon=0._PM_REEL
!      alat =0._PM_REEL
      
! Calculs

!c
!c     if lon > 176.5 ==> lon(96)<lon<lon(1)
      if (lon.ge.lonmax) then
         dlon(1)=dimlon
         dlon(2)=1
         dlon(3)=1
         dlon(4)=dimlon
         t=(lon-lonmax)/deltal
      else
         alon=lonmin
         do i=1,dimlon-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltal)) then
               dlon(4)=i
               dlon(3)=i+1
               dlon(1)=i
               dlon(2)=i+1
               t=(lon-alon)/deltal
            endif
            alon=alon+deltal
         enddo
      endif

      if (lat.le.latmin) then
         dlat(4)=1
         dlat(3)=1
         dlat(1)=1
         dlat(2)=1
         u=0.
      elseif (lat.ge.latmax) then
         dlat(4)=dimlat
         dlat(3)=dimlat
         dlat(1)=dimlat
         dlat(2)=dimlat
         u=0.
      else
         alat=latmin
         do i=1,dimlat-1
            if ((lat.ge.alat).and.(lat.lt.alat+deltal)) then
               dlat(4)=i+1
               dlat(3)=i+1
               dlat(1)=i
               dlat(2)=i
               u=(lat-alat)/deltal
            endif
            alat=alat+deltal
         enddo       
      endif
         
      return
    end subroutine grid4_23

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grwpb_23(pert,dev,lamda,lonw,lat,height,rho,u,v,time, &
          name,ier)
!c
!c     Small scale gravity wave perturbation model.
!c     Computes perturbation on u, v, theta or rho, with wave phase factor.
!c
!c     input
!c
      implicit none

      real(kind=PM_REEL) :: dev          !random number between 0 and 1
      real(kind=PM_REEL) :: lamda        !vertical wavelength of g.w. (km)
      real(kind=PM_REEL) :: lonw         !west longitude of point
      real(kind=PM_REEL) :: lat          !latitude of point
      real(kind=PM_REEL) :: height       !height of point
      real(kind=PM_REEL) :: rho          !density at height=height
      real(kind=PM_REEL) :: u            !zonal wind at height=height
      real(kind=PM_REEL) :: v            !meridional wind at height=height
      real(kind=PM_REEL) :: time         !database time
      character*16 :: name !name of variable to perturb
!c
!c     output
!c
      integer :: ier   !error flag (0=OK, 1=NOK)
      real(kind=PM_REEL) :: pert     !perturbation
!c
!c     local
!c
      integer :: ierr
      real(kind=PM_REEL) :: a, aprime
      real(kind=PM_REEL) :: rho0,rho1
      real(kind=PM_REEL) :: u0,u1
      real(kind=PM_REEL) :: v0,v1
      real(kind=PM_REEL) :: tmpheight
      integer :: levlow
      integer :: levhi
      real(kind=PM_REEL) :: levweight
      real(kind=PM_REEL) :: pratio
      real(kind=PM_REEL) :: dz
      real(kind=PM_REEL) :: dz_max 
      real(kind=PM_REEL) :: sig
      real(kind=PM_REEL) :: umag
      real(kind=PM_REEL) :: dalr=4.5_PM_REEL         !dry adiabatic lapse rate (K/km)
!!!      parameter(dalr=4.5)
      real(kind=PM_REEL) :: pi=3.14159265359_PM_REEL
!!!      parameter(pi=3.14159265359)
      character*16 :: tmpname


! INitialisations
      


      ier=0

! Calculs

!c
!c     get rho0, u0, v0
!c     use mean data from level 1, not height=0.0
      levlow=1
      levhi=1
      levweight=1.0_PM_REEL
      pratio=1.0_PM_REEL
      tmpname='u'
      call var3d_23(u0,lonw,lat,levhi,levlow,levweight,pratio,time, &
                tmpname,ierr)
      tmpname='v'
      call var3d_23(v0,lonw,lat,levhi,levlow,levweight,pratio,time, &
                tmpname,ierr)
      tmpname='rho'
      call var3d_23(rho0,lonw,lat,levhi,levlow,levweight,pratio,time, &
                tmpname,ierr)
!c
!c     get sub-grid scale variance
!c
      tmpname='substd'
      call var2d_23(sig,lonw,lat,1.0_PM_REEL,tmpname,ierr)
      sig=sig/1000.
!c
!c     compute delta z (N=N0 is implicit)
!c
      if (height.le.75.) then
         umag=sqrt(u**2+v**2)
!c     the wave amplitude becomes large as the wind speed becomes small
!c     and the wave should saturate
         if (umag.lt.0.1) umag=0.1_PM_REEL
         dz=(rho0*sqrt(u0**2+v0**2)*sig)/(rho*umag)
         dz=sqrt(dz)
         tmpheight=height
      else
!c         perturbation above 75 km:
!c         Using the same amplitude and same perturbation
!c         as at 75 km for T,u,v. The perturbation for rho is
!c         scaled on the T perturbation for rho at height=height     
         tmpheight=75._PM_REEL          ! set height=75 for perturbation calculation
!c         get info on variable at 75 km:
         call getsi_23(lonw,lat,tmpheight,time, &
                levhi,levlow,levweight,pratio,ierr)
         tmpname='rho'
         call var3d_23(rho1,lonw,lat,levhi,levlow,levweight, &
             pratio,time, tmpname,ierr)
         tmpname='u'
         call var3d_23(u1,lonw,lat,levhi,levlow,levweight, &
             pratio,time, tmpname,ierr)
         call var3d_23(v1,lonw,lat,levhi,levlow,levweight, &
             pratio,time, tmpname,ierr)
         umag=sqrt(u1**2+v1**2)
!c     the wave amplitude becomes large as the wind speed becomes small
!c     and the wave should saturate
         if (umag.lt.0.1) umag=0.1
         dz=(rho0*sqrt(u0**2+v0**2)*sig)/(rho1*umag)
         dz=sqrt(dz)
      end if
!c
!c     apply simple test for saturation (require theta_z > 0)
!c     and compute wave
      dz_max=lamda/(2.*pi)
      if (dz.gt.dz_max) then
         dz=dz_max
      endif
      dz=dz*sin(2.*pi*dev+(2.*pi*height)/lamda)
!c
!c     find perturbation from change delta z in mean profile
!c     (more accurate than taking first derivative)
!c
!c     don't allow dz to exceed the current height (close to the ground)
      if ((tmpheight+dz).le.0.) then
         dz = -0.99*tmpheight
      endif
      tmpname = name
      if (name.eq.'rho') tmpname= 't' 
      call getsi_23(lonw,lat,tmpheight,time, &
                levhi,levlow,levweight,pratio,ierr)
      call var3d_23(a,lonw,lat,levhi,levlow,levweight,pratio, &
                time,tmpname,ierr)
      call getsi_23(lonw,lat,tmpheight+dz,time, &
                levhi,levlow,levweight,pratio,ierr)
      call var3d_23(aprime,lonw,lat,levhi,levlow,levweight,pratio, &
                time,tmpname,ierr)
      pert = aprime - a
!c
!c     correct for perturbation to potential temperature and density
!c
      if (tmpname.eq.'t') pert = pert + dalr*dz
!c     pert(rho) = -rho*pert(T)/(T+pert(T))
      if (name.eq.'rho') then
         pert = -rho*pert/(a+pert)
      end if 
!c
      return
    end subroutine grwpb_23

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadvar_23(nf,type,ier)

!c     load arrays corresponding to the variable type

      implicit none

!c     exDRS channel number
      integer :: nf
!c     variable type to load :
!c       mean = values of ps,tsurf,t,rho,u,v of mean field file (nf=udrsm)
!c       sigm = sigma values of mean field file (nf=udrsm)
!c       orog = orography values of mountain file (nf=udrs)
!c       eofs = normu,normu,normt,normp,normr,u,v,t,rho,ps,pcsmth,pcvar
!c                    of eof field file (nf=ueof) 
!c       grwp = substd values of mountain file (nf=udrs)
!c       stdv = sdps,sdtsurf,sdt,sdrho,sdu and sdv values of
!c                     standard deviation field file (nf=udrssd)
      character*4 :: type
      integer :: ier   !error flag (0=OK, 1=NOK)

      integer :: iboucle
      integer :: dim1,dim2,dim3
      integer :: position

      ier = 0

      if (type.eq.'mean') then
         position=26
         do iboucle=1,12
	    !! decallage recno, co2ice et emis
	    position = position + 4608+4608+1 
	    dim1=96
	    dim2=48
	    call lis_tableau_23(var2d=tabtsurf(1:dim1,1:dim2,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,position=position)
	    position = position + dim1*dim2
	    call lis_tableau_23(var2d=tabps(1:dim1,1:dim2,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,position=position)
	    position = position + dim1*dim2
	    dim3=25
	    call lis_tableau_23(var3d=tabt(1:dim1,1:dim2,1:dim3,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,dim3=dim3,&
				position=position)
	    position = position + dim1*dim2*dim3
	    call lis_tableau_23(var3d=tabu(1:dim1,1:dim2,1:dim3,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,dim3=dim3,&
				position=position)
	    position = position + dim1*dim2*dim3
	    call lis_tableau_23(var3d=tabv(1:dim1,1:dim2,1:dim3,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,dim3=dim3,&
				position=position)
	    position = position + dim1*dim2*dim3
	    call lis_tableau_23(var3d=tabrho(1:dim1,1:dim2,1:dim3,iboucle),unit=nf,&
	                        dim1=dim1,dim2=dim2,dim3=dim3,&
				position=position)
	    position = position + dim1*dim2*dim3
	    !! decallage q2
	    position = position + dim1*dim2*dim3
	 enddo
      elseif (type.eq.'sigm') then
         call lis_tableau_23(var1d=sigma,unit=nf,dim1=25,position=1)
      elseif (type.eq.'orog') then

         call lis_tableau_23(var3d=taborog,unit=nf,dim1=96,dim2=48,dim3=1,&
	                     position=2)
      elseif (type.eq.'eofs') then
         call lis_tableau_23(var1d=tabeonormu,unit=nf,dim1=9,position=26)
	 call lis_tableau_23(var1d=tabeonormv,unit=nf,dim1=9,position=35)
	 call lis_tableau_23(var1d=tabeonormt,unit=nf,dim1=9,position=44)
	 call lis_tableau_23(var1d=tabeonormp,unit=nf,dim1=9,position=53)
	 call lis_tableau_23(var1d=tabeonormr,unit=nf,dim1=9,position=62)
	 call lis_tableau_23(var4d=tabeou,unit=nf,&
	                     dim1=18,dim2=9,dim3=25,dim4=72,&
		             position=719)
         call lis_tableau_23(var4d=tabeov,unit=nf,&
	                     dim1=18,dim2=9,dim3=25,dim4=72,&
		             position=292319)
         call lis_tableau_23(var4d=tabeot,unit=nf,&
	                     dim1=18,dim2=9,dim3=25,dim4=72,&
		             position=583919)
         call lis_tableau_23(var3d=tabeops,unit=nf,&
	                     dim1=18,dim2=9,dim3=72,&
		             position=875519)
         call lis_tableau_23(var4d=tabeorho,unit=nf,&
	                     dim1=18,dim2=9,dim3=25,dim4=72,&
		             position=887183)
         call lis_tableau_23(var3d=tabpcsmth,unit=nf,&
	                     dim1=9,dim2=669,dim3=72,&
		             position=1178783)
         call lis_tableau_23(var3d=tabpcvar,unit=nf,&
	                     dim1=9,dim2=669,dim3=72,&
		             position=1612295)
      elseif (type.eq.'grwp') then
         call lis_tableau_23(var3d=tabsubstd,unit=nf,dim1=96,dim2=48,dim3=1,&
	                     position=4610)
      elseif (type.eq.'stdv') then
         call lis_tableau_23(var3d=tabsdtsurf,unit=nf,&
	                     dim1=96,dim2=48,dim3=1,&
	                     position=9243)
	 call lis_tableau_23(var3d=tabsdps,unit=nf,&
	                     dim1=96,dim2=48,dim3=1,&
	                     position=13851)
	 call lis_tableau_23(var4d=tabsdt,unit=nf,&
	                     dim1=96,dim2=48,dim3=25,dim4=1,&
	                     position=18459)
	 call lis_tableau_23(var4d=tabsdu,unit=nf,&
	                     dim1=96,dim2=48,dim3=25,dim4=1,&
	                     position=133659)
	 call lis_tableau_23(var4d=tabsdv,unit=nf,&
	                     dim1=96,dim2=48,dim3=25,dim4=1,&
	                     position=248859)
	 call lis_tableau_23(var4d=tabsdrho,unit=nf,&
	                     dim1=96,dim2=48,dim3=25,dim4=1,&
	                     position=364059)
      endif

      return

    end subroutine loadvar_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ltime_23(longitude,sunlon,localtime)
!c
!c     Compute local time at longitude=longitude when the sun is
!c     at longitude=sunlon.
!c
      implicit none

!c     input
!c
      real(kind=PM_REEL) :: longitude !west longitude
      real(kind=PM_REEL) :: sunlon    !west longitude of sun
!c
!c     output
!c
      real(kind=PM_REEL) :: localtime !local time
!c
      localtime=12.+(sunlon-longitude)/15.
      if (localtime.lt.0.) localtime=localtime+24.
      if (localtime.gt.24.) localtime=localtime-24.
!c
      return
    end subroutine mars_ltime_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine opend_23(udrs,udrsm,udrssd,ueof,num,dust,dset,opened,ier)
!c
!c     Open the appropriate DRS file corresponding to Ls,dust scenario  
!c     and data set.
!c
      use cps_util
      implicit none

!c     Input
!c
      integer :: udrs     !exDRS channel number for mountain fields
      integer :: udrsm    !exDRS channel number for mean fields
      integer :: udrssd   !exDRS channel number for standard deviation fields
      integer :: ueof     !exDRS channel number for EOF fields
      integer :: num      !season num
      integer :: dust     !Dust senario
      character(len=*) :: dset   !Dataset
!c     Output
      integer :: opened   ! fichiers ouverts
      integer :: ier      !Error flag (0=OK, 1=NOK)
!c
!c     Local
!c
      integer :: ierr
      character*255 :: datfile
!      character*255 :: dicfile
      character*3   :: scen
      character*3   :: saison
      character*2   :: type1

!c     DRS function
!      integer :: cllun,aslun

      ier=0
      opened = 1

!c     dust scenario
      if (dust.eq.1) then
         scen='vik'
      elseif (dust.eq.2) then
         scen='low'
      elseif (dust.eq.3) then
         scen='ds2'
      elseif (dust.eq.4) then
         scen='ds5'
      endif

!c     season number
      write(saison,'(i2.2,1x)') num 
      saison = "s"//saison(1:2)
!c
!c     Mean file
      type1='me'
      datfile=dset//scen//saison//type1//'.dat'
!      dicfile=dset//scen//saison//type1//'.dic'
!      ierr=aslun(udrsm,dicfile,udrsm+1,datfile,IDRS_READ)
      call cps_file_unit(udrsm,ierr)
      if (ierr.ne.0) goto 9999
#ifdef __GFORTRAN__
      open (unit=udrsm,file=datfile,status='old', convert='big_endian', &
            access='direct',form='unformatted',recl=512,iostat=ierr)
#else
      open (unit=udrsm,file=datfile,status='old',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#endif
      if (ierr.ne.0) goto 9999
!c
!c     Standard Deviation file
      type1='sd'
      datfile=dset//scen//saison//type1//'.dat'
!      dicfile=dset//scen//saison//type1//'.dic'
!      ierr=aslun(udrssd,dicfile,udrssd+1,datfile,IDRS_READ)
      call cps_file_unit(udrssd,ierr)
      if (ierr.ne.0) goto 9999
#ifdef __GFORTRAN__
      open (unit=udrssd,file=datfile,status='old', convert='big_endian',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#else
      open (unit=udrssd,file=datfile,status='old',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#endif
      if (ierr.ne.0) goto 9999
!c
!c     eof file
      datfile=dset//scen//'alleo.dat'
!      dicfile=dset//scen//'alleo.dic'
!      ierr=aslun(ueof,dicfile,ueof+1,datfile,IDRS_READ)
      call cps_file_unit(ueof,ierr)
      if (ierr.ne.0) goto 9999
#ifdef __GFORTRAN__
      open (unit=ueof,file=datfile,status='old', convert='big_endian',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#else
      open (unit=ueof,file=datfile,status='old',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#endif
      if (ierr.ne.0) goto 9999
!c
!c     mountain file
!c
      datfile=dset//'mountain.dat'
!      dicfile=dset//'mountain.dic'
!      ierr=aslun(udrs,dicfile,udrs+1,datfile,IDRS_READ)
      call cps_file_unit(udrs,ierr)
      if (ierr.ne.0) goto 9999
#ifdef __GFORTRAN__
      open (unit=udrs,file=datfile,status='old', convert='big_endian',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#else
      open (unit=udrs,file=datfile,status='old',&
            access='direct',form='unformatted',recl=512,iostat=ierr)
#endif
      if (ierr.ne.0) goto 9999

      return

!c     Error handling
 9999 ier=1
      write(*,'(a)')"OPEND Error : impossible to open "//trim(datfile)
      return

    end subroutine opend_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine orbit_23(date,sunlat,sunlon,ls,marsau,outmodelday)
!c
!c     Given Julian date solves the equation of time to give
!c     solar longitude and latitude, Ls and distance of Mars
!c     from the sun in AU and model day

      implicit none
!c
!c     inputs
!c
      real(kind=PM_REEL) :: date   !Julian date
!c
!c     outputs
!c
      real(kind=PM_REEL)   :: sunlat !subsolar latitude
      real(kind=PM_REEL)  ::  sunlon !subsolar longitude
      real(kind=PM_REEL)   :: ls     !Ls
      real(kind=PM_REEL)   :: marsau !Sun-Mars distance in AU
      real(kind=PM_REEL)   :: outmodelday
!c
!c     local
!c
      real(kind=PM_REEL) :: modelday
      real(kind=PM_REEL) :: marsday=88775.245_PM_REEL
      real(kind=PM_REEL) :: earthday=86400._PM_REEL
      real(kind=PM_REEL)   :: dummy
      real(kind=PM_REEL) :: mdoffset=222.4_PM_REEL
      real(kind=PM_REEL)   :: slonoff=197.2_PM_REEL
      real(kind=PM_REEL)   :: inclin=25.3_PM_REEL
      real(kind=PM_REEL)   :: pi=3.14159_PM_REEL
!c
!c     convert Julian day to model day
!c
      modelday=date*earthday/marsday
      modelday=mod(modelday+mdoffset,669.d0)
      dummy=modelday
      outmodelday=modelday
      call sol2ls_23(dummy,ls)
      sunlon=360._pm_reel*(dummy-real(int(dummy),kind=PM_REEL))+slonoff
      sunlon=mod(sunlon,360._pm_reel)
      sunlat=inclin*sin(ls*pi/180._pm_reel)
      marsau=1.5237_pm_reel*7.73396_pm_reel*((1._pm_reel-0.934_pm_reel**2)) &
            /(1._pm_reel+0.0934_pm_reel*cos((ls+109._pm_reel)*pi/180._pm_reel))
      return
    end subroutine orbit_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine profi_23(a,lon,lat,time,name,ier)
!c
!c     Retrieve a profile of 3-d variable=name at longitude=lon, latitude=lat
!c     and universal time=time.
!c
      implicit none

! #include "constants_atmemcd_23.h"
!c
!c     input
!c
      real(kind=PM_REEL)          :: lon    !east longitude of point
      real(kind=PM_REEL)          :: lat    !latitude of point
      real(kind=PM_REEL)          :: time   !Global time
      character*16  :: name   !Name of variable
!c
!c     output
!c
      integer :: ier               !error flag (0=OK, 1=NOK)
      real(kind=PM_REEL)    :: a(dimlevs)        !Profile
!c
!c     local variables
!c
      integer :: i,l
      integer :: iut
      integer ::    dlon(4)
      integer ::    dlat(4)
      real(kind=PM_REEL)    :: y(dimlevs,4)
      real(kind=PM_REEL)    :: t
      real(kind=PM_REEL)    :: u

      ier = 0
!c
!c     find nearest 4 grid points
!c
      call grid4_23(lon,lat,dlon,dlat,t,u)

      iut=int(time)
!c
!c     get 4 profiles at those points
!c
      if (name.eq.'t') then
         do i=1,4
            do l=1,25
               y(l,i)=tabt(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'rho') then
         do i=1,4
            do l=1,25
               y(l,i)=tabrho(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'u') then
         do i=1,4
            do l=1,25
               y(l,i)=tabu(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'v') then
         do i=1,4
            do l=1,25
               y(l,i)=tabv(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'sdt') then
         do i=1,4
            do l=1,25
               y(l,i)=tabsdt(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'sdrho') then
         do i=1,4
            do l=1,25
               y(l,i)=tabsdrho(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'sdu') then
         do i=1,4
            do l=1,25
               y(l,i)=tabsdu(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      elseif (name.eq.'sdv') then
         do i=1,4
            do l=1,25
               y(l,i)=tabsdv(dlon(i),dlat(i),l,iut)
            enddo
         enddo
      else
         write(0,*)'PROFI Error: ',name,' unknown name'
         ier=1
         go to 9999
      endif

!c
!c     bilinear interpolation
!c
      do l=1,dimlevs
        a(l)=(1.-t)*(1.-u)*y(l,1) &
            +t*(1.-u)*y(l,2) &
            +t*u*y(l,3) &
            +(1.-t)*u*y(l,4)
      enddo
      return

 9999 do l=1,dimlevs
        a(l)=0.
      enddo
      return
    end subroutine profi_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season_23(ls,numsaison)

!c     compute the season number from aerocentric longitude

      implicit none

!c     input
      real(kind=PM_REEL) :: ls
!c     output 
      integer :: numsaison

      if ((ls.ge.0.0).and.(ls.le.30)) then
         numsaison=1
      endif
      if ((ls.ge.30.0).and.(ls.le.60)) then
         numsaison=2
      endif
      if ((ls.ge.60.0).and.(ls.le.90)) then
         numsaison=3
      endif
      if ((ls.ge.90.0).and.(ls.le.120)) then
         numsaison=4
      endif
      if ((ls.ge.120.0).and.(ls.le.150)) then
         numsaison=5
      endif
      if ((ls.ge.150.0).and.(ls.le.180)) then
         numsaison=6
      endif
      if ((ls.ge.180.0).and.(ls.le.210)) then
         numsaison=7
      endif
      if ((ls.ge.210.0).and.(ls.le.240)) then
         numsaison=8
      endif
      if ((ls.ge.240.0).and.(ls.le.270)) then
         numsaison=9
      endif
      if ((ls.ge.270.0).and.(ls.le.300)) then
         numsaison=10
      endif
      if ((ls.ge.300.0).and.(ls.le.330)) then
         numsaison=11
      endif
      if ((ls.ge.330.0).and.(ls.le.360)) then
         numsaison=12
      endif
      
      return
    end subroutine season_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine sol2ls_23(sol,ls)
!c
!c  Convert day number, sol, to solar longitude, Ls, where sol=0 is the
!c  spring equinox.

      implicit none

!c  Arguments:
      real(kind=PM_REEL) :: sol
      real(kind=PM_REEL) :: ls

!c  Local:
      real(kind=PM_REEL) :: year_day=668.6_PM_REEL
      real(kind=PM_REEL) :: peri_day=485.0_PM_REEL
      real(kind=PM_REEL) :: timeperi=1.905637_PM_REEL
      real(kind=PM_REEL) :: e_elips=0.093358_PM_REEL
      real(kind=PM_REEL) :: pi=3.1415927_PM_REEL
      real(kind=PM_REEL) :: degrad=57.295779_PM_REEL
      real(kind=PM_REEL) :: zanom,xref,zx0,zdx,zteta,zz
      integer :: iter

      zz=(sol-peri_day)/year_day
      zanom=2.*pi*(zz-nint(zz))
      xref=abs(zanom)

!c  The equation zx0 - e * sin (zx0) = xref, solved by Newton
      zx0=xref+e_elips*sin(xref)
      do 110 iter=1,10
         zdx=-(zx0-e_elips*sin(zx0)-xref)/(1.-e_elips*cos(zx0))
         if(abs(zdx).le.(1.e-7)) goto 120
         zx0=zx0+zdx
  110 enddo
  120 continue
      zx0=zx0+zdx
      if(zanom.lt.0.) zx0=-zx0

      zteta=2.*atan(sqrt((1.+e_elips)/(1.-e_elips))*tan(zx0/2.))
      ls=zteta-timeperi
      if(ls.lt.0.) ls=ls+2.*pi
      if(ls.gt.2.*pi) ls=ls-2.*pi
      ls=degrad*ls
      return
    end subroutine sol2ls_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var2d_23(a,lonw,lat,time,name,ier)
!c
!c     Retrieve the value of 2-d variable=name at longitude=lon, latitude=lat 
!c     and universal time=time. Use bilinear interpolation to get user
!c     specified longitude and latitude.
!c
      implicit none
      
! #include "constants_atmemcd_23.h"
!c
!c     input
!c
      real(kind=PM_REEL)          :: lonw   !longitude (west) of point
      real(kind=PM_REEL)          :: lat    !latitude of point
      real(kind=PM_REEL)          :: time   !Global time
      character*16  :: name   !Name of variable
!c
!c     output
!c
      integer :: ier          !error flag (0=OK, 1=NOK)
      real(kind=PM_REEL)    :: a            !value of variable
!c
!c     local variables
!c
      integer :: i
      integer :: iut
      integer  ::   dlon(4)
      integer ::    dlat(4)
      real(kind=PM_REEL)    :: y(4)
      real(kind=PM_REEL)    :: t
      real(kind=PM_REEL)    :: u
      real(kind=PM_REEL)    :: lon          !longitude east

      ier = 0
!c
!c     convert longitude west [0..360] to longitude east [-180..180]
!c
!c      lon=360.-lonw
      if (lonw.eq.0.) then
         lon=0.
      elseif (lonw.le.180.)then
         lon=-lonw
      else
         lon=360.-lonw
      endif         
!c
!c     find nearest 4 grid points
!c
      call grid4_23(lon,lat,dlon,dlat,t,u)

      iut=int(time)
!c     
!c     retrieve of the four values of the nearest grid points from the array
      if (name.eq.'orography') then
         do i=1,4
            y(i)=taborog(dlon(i),dlat(i),iut)
         enddo
      elseif (name.eq.'ps') then
         do i=1,4
            y(i)=tabps(dlon(i),dlat(i),iut)
         enddo
      elseif (name.eq.'tsurf') then
         do i=1,4
            y(i)=tabtsurf(dlon(i),dlat(i),iut)
         enddo
      elseif (name.eq.'sdps') then
         do i=1,4
            y(i)=tabsdps(dlon(i),dlat(i),iut)
         enddo
      elseif (name.eq.'substd') then
         do i=1,4
            y(i)=tabsubstd(dlon(i),dlat(i),iut)
         enddo
      elseif (name.eq.'sdtsurf') then
         do i=1,4
            y(i)=tabsdtsurf(dlon(i),dlat(i),iut)
         enddo      
      else
         ier=1
         write(0,*)'VAR2D Error: ',name &
             ,' unknown variable name'
         goto 9999
      endif
                 
!c
!c     bilinear interpolation
!c
      a=(1.-t)*(1.-u)*y(1) &
      +t*(1.-u)*y(2) &
      +t*u*y(3) &
      +(1.-t)*u*y(4)
      return

!c     error handling
 9999 a=0.
      return

    end subroutine var2d_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine var3d_23(a,lonw,lat,levhi,levlow,levweight,pratio, &
                      time,name,ier)
!c
!c     Retrieve the value of 3-d variable=name at longitude=lon, latitude=lat
!c     and universal time=time. The height of the variable is controlled by
!c     levhi, levlow and levweight.  Use bilinear interpolation to get user
!c     specified longitude and latitude.
!c
      implicit none

! #include "constants_atmemcd_23.h"
!c
!c     input
!c
      real(kind=PM_REEL)          :: lonw      !longitude (west) of point
      real(kind=PM_REEL)          :: lat       !latitude of point
      integer       :: levhi     !database level upper bound
      integer       :: levlow    !database level lower bound
      real(kind=PM_REEL)          :: levweight !level weight for interpolation
      real(kind=PM_REEL)          :: pratio    !ratio of pressure to extreme value if out of range
      real(kind=PM_REEL)          :: time      !Global time
      character*16 ::  name      !Name of variable
!c
!c     output
!c
      integer :: ier             !error flag (0=OK, 1=NOK)
      real(kind=PM_REEL)    :: a               !value of variable
!c
!c     local variables
!c
      integer :: ierr
      real(kind=PM_REEL)    :: profile(dimlevs)
      real(kind=PM_REEL)    :: lon

      ier = 0
!c
!cc     convert longitude west to longitude east
!c      lon=360.-lonw
!c     convert longitude west [0..360[ to longitude east[-180..180[
      if (lonw.eq.0.) then
         lon=0.
      elseif (lonw.le.180.)then
         lon=-lonw
      else
         lon=360.-lonw
      endif
!c
      call profi_23(profile,lon,lat,time,name,ierr)
      if (ierr.ne.0) then 
         write(0,*)'VAR3D Error : profile not available for variable' &
             ,name
         ier=1
         goto 9999
      endif

      a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight
!c
!c correction for densities which are out of sigma range
      if (name.eq.'rho'.or.name.eq.'sdrho') a=pratio*a
!c
      return

!c     Error handling
 9999 a=0.
      return

    end subroutine var3d_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine max2d_23(a,lon,lat,name,ier)
!c
!c     Retrieve the daily maximum value of 2-d variable=name at 
!c     longitude=lon, latitude=lat
!c
      implicit none
!c
!c     input
!c
      real(kind=PM_REEL)          :: lon    !west longitude of point
      real(kind=PM_REEL)          :: lat    !latitude of point
      character*16  :: name   !Name of variable
!c
!c     output
!c
      integer :: ier          !Error flag (0=OK, 1=NOK)
      real(kind=PM_REEL) :: a               !maximum of variable
!c
!c     local variables
!c
      integer :: ierr,i
      real(kind=PM_REEL)    :: x
      real(kind=PM_REEL)    :: xmax
      real(kind=PM_REEL)    :: time
!c
      ier=0
      xmax=-1.e20
      do i=1,12
        time=real(i,kind=PM_REEL)
        call var2d_23(x,lon,lat,time,name,ierr)
        if(ierr.ne.0) then
           ier=1
           write(0,*)"MAX2D Error : impossible to find ",name, &
               " maximum"
           go to 9999
        endif
        if (x.gt.xmax) xmax=x
      enddo
      a=xmax
      return

!c     Error handling
 9999 a=0.
      return

    end subroutine max2d_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine min2d_23(a,lon,lat,name,ier)
!c
!c     Retrieve the daily minimum value of 2-d variable=name at 
!c     longitude=lon, latitude=lat
!c
      implicit none
!c
!c     input
!c
      real(kind=PM_REEL)          :: lon    !longitude of point
      real(kind=PM_REEL)          :: lat    !latitude of point
      character*16  :: name   !Name of variable
!c
!c     output
!c
      integer :: ier          !Error flag (0=OK, 1=NOK)
      real(kind=PM_REEL) :: a               !minimum of variable
!c
!c     local variables
!c
      integer :: ierr,i
      real(kind=PM_REEL)    :: x
      real(kind=PM_REEL)    :: xmin
      real(kind=PM_REEL)    :: time

      ier=0
      xmin=1.e20
      do i=1,12
        time=real(i,kind=PM_REEL)
        call var2d_23(x,lon,lat,time,name,ierr)
        if(ierr.ne.0) then
           ier=1
           write(0,*)"MIN2D Error : impossible to find ",name, &
               " minimum"
           go to 9999
        endif
        if (x.lt.xmin) xmin=x
      enddo
      a=xmin
      return

!c     Error handling
 9999 a=0.
      return

    end subroutine min2d_23

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    real(kind=pm_reel)  FUNCTION ran1_23(idum)
!c     "Minimal" random number generator of Park and Miller with Bays-Durham
!c     shuffle and added safeguards 
!c     Reference : Numerical Recipes in Fortran 77 : the art of scientific 
!c     computing - 2d edition (1992)- p 271
!c     Return a uniform random deviate between 0.0 and 1.0 (eclusive of the
!c     endpoints values). Call witj idum a negative integer to initialize;
!c     thereafter, do not alter idum between successive deviates in a sequence.
!c     RNMX should approximate the largest floating value that is less than 1.

      implicit none

      INTEGER :: idum,IA,IM,IQ,IR,NTAB,NDIV
      
      real(kind=pm_reel) :: AM,EPS,RNMX
      !!!REAL(kind=PM_REEL) :: ran1_23,AM,EPS,RNMX
      !!!PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
      !!!           NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      parameter(IA=16807,IM=2147483647,IQ=127773,IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB)
      !!!
      INTEGER :: j,k
      integer,SAVE :: iv(NTAB),iy
      DATA iv /NTAB*0/, iy /0/
      !!!
      AM= 1._pm_reel / real(IM,kind=PM_REEL)
      EPS=0.00000012_pm_reel
      RNMX= 1._pm_reel - EPS
      !!!

!      integer, save, parameter :: iy=0

      if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do j=NTAB+8,1,-1
            k=idum/IQ
            idum=IA*(idum-k*IQ)-IR*k
            if (idum.lt.0) idum=idum+IM
            if (j.le.NTAB) iv(j)=idum
         end do
         iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1_23=min(AM*iy,RNMX)
      return
    END FUNCTION ran1_23
    
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

    subroutine lis_tableau_23(var1d,var2d,var3d,var4d,unit,&
                              dim1,dim2,dim3,dim4,position)

! Routine de lecture des fichiers du modele EMCD 2.3 
! Attention : cette routine remplace la LIBDRS, obsolete
    
       implicit none
       
       real,dimension(:),optional,intent(inout) :: var1d         !tableau de donnees 1D
       real,dimension(:,:),optional,intent(inout) :: var2d       !tableau de donnees 2D
       real,dimension(:,:,:),optional,intent(inout) :: var3d     !tableau de donnees 3D
       real,dimension(:,:,:,:),optional,intent(inout) :: var4d   !tableau de donnees 4D
       integer,intent(in) :: unit           ! unite logique du fichier de donnees
       integer,intent(in) :: dim1           ! taille de la 1ere dimension du tableau
       integer,optional,intent(in) :: dim2  ! taille de la 2eme dimension du tableau
       integer,optional,intent(in) :: dim3  ! taille de la 3eme dimension du tableau
       integer,optional,intent(in) :: dim4  ! taille de la 4eme dimension du tableau
       integer,intent(in) :: position       ! position du premier element du tableau
                                            ! dans le fichier en nombre de reels 
					    ! (i.e. on compte 1 tous les 4 octets)
       
       !variables locales
       real,dimension(128) :: decallage    ! elements du record a ne pas lire
       integer :: irecord                  ! numero du record courant dans le fichier
       integer :: position_record          ! position courante de lecture du tableau 
                                           !  dans le record courant du fichier
       integer :: reste_record             ! nombre de reels non lus dans le record 
                                           !  courant
       integer :: position_dim1            ! position courante de l indice de la 
                                           !  premiere dimension du tableau attendu
					   !  en sortie
       integer :: reste_dim1               ! nombre de reels restant a lire pour 
                                           !  completer la suite de longueur dim1
       integer :: idim2,idim3,idim4        ! indices de boucles
       
       irecord = position / 128 +1
       position_record = position - 128 * (irecord -1)
       reste_record = 128 - position_record +1
       
       if (present(var1d)) then
         reste_dim1=dim1
	 do while (reste_dim1 > 0)
	   position_dim1 = dim1 - reste_dim1 +1
	   if (reste_dim1<reste_record) then ! il reste assez de reels dans le record
	                                    ! courant pour finir la sequence de dim1
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var1d(position_dim1:position_dim1+reste_dim1-1)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var1d(position_dim1:position_dim1+reste_dim1-1)
             endif
             reste_record=reste_record-reste_dim1
	     position_record=position_record+reste_dim1
	     reste_dim1=0
	   else  ! il n y a pas assez de reels dans le record courant
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var1d(position_dim1:position_dim1+reste_record-1)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var1d(position_dim1:position_dim1+reste_record-1)
             endif
             reste_dim1=reste_dim1-reste_record
	     reste_record=128
	     position_record=1
	     irecord=irecord+1
	   endif
	 enddo
       elseif (present(var2d)) then
!         if (.not.present(dim2)) !!!message erreur a ajouter
	 do idim2=1,dim2
	 reste_dim1=dim1
	 do while (reste_dim1 > 0)
	   position_dim1 = dim1 - reste_dim1 +1
	   if (reste_dim1<reste_record) then ! il reste assez de reels dans le record
	                                    ! courant pour finir la sequence de dim1
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var2d(position_dim1:position_dim1+reste_dim1-1,&
	                                         idim2)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var2d(position_dim1:position_dim1+reste_dim1-1,&
                          idim2)
             endif
             reste_record=reste_record-reste_dim1
	     position_record=position_record+reste_dim1
	     reste_dim1=0
	   else  ! il n y a pas assez de reels dans le record courant
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var2d(position_dim1:position_dim1+reste_record-1,&
                                                 idim2)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var2d(position_dim1:position_dim1+reste_record-1,&
                          idim2)
             endif
             reste_dim1=reste_dim1-reste_record
	     reste_record=128
	     position_record=1
	     irecord=irecord+1
	   endif
	 enddo
	 enddo
       elseif (present(var3d)) then
!         if (.not.present(dim2)) !!!message erreur a ajouter
!         if (.not.present(dim3)) !!!message erreur a ajouter
	 do idim3=1,dim3
	 do idim2=1,dim2
	 reste_dim1=dim1
	 do while (reste_dim1 > 0)
	   position_dim1 = dim1 - reste_dim1 +1
	   if (reste_dim1<reste_record) then ! il reste assez de reels dans le record
	                                    ! courant pour finir la sequence de dim1
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var3d(position_dim1:position_dim1+reste_dim1-1,&
	                                         idim2,idim3)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var3d(position_dim1:position_dim1+reste_dim1-1,&
                          idim2,idim3)
             endif
             reste_record=reste_record-reste_dim1
	     position_record=position_record+reste_dim1
	     reste_dim1=0
	   else  ! il n y a pas assez de reels dans le record courant
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var3d(position_dim1:position_dim1+reste_record-1,&
                                                 idim2,idim3)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var3d(position_dim1:position_dim1+reste_record-1,&
                          idim2,idim3)
             endif
             reste_dim1=reste_dim1-reste_record
	     reste_record=128
	     position_record=1
	     irecord=irecord+1
	   endif
	 enddo
	 enddo
	 enddo
       elseif (present(var4d)) then
!         if (.not.present(dim2)) !!!message erreur a ajouter
!         if (.not.present(dim3)) !!!message erreur a ajouter
!         if (.not.present(dim4)) !!!message erreur a ajouter
	 do idim4=1,dim4
	 do idim3=1,dim3
	 do idim2=1,dim2
	 reste_dim1=dim1
	 do while (reste_dim1 > 0)
	   position_dim1 = dim1 - reste_dim1 +1
	   if (reste_dim1<reste_record) then ! il reste assez de reels dans le record
	                                    ! courant pour finir la sequence de dim1
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var4d(position_dim1:position_dim1+reste_dim1-1,&
	                                         idim2,idim3,idim4)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var4d(position_dim1:position_dim1+reste_dim1-1,&
                          idim2,idim3,idim4)
             endif
             reste_record=reste_record-reste_dim1
	     position_record=position_record+reste_dim1
	     reste_dim1=0
	   else  ! il n y a pas assez de reels dans le record courant
	     if (position_record.eq.1) then
	       read(unit=unit,rec=irecord) var4d(position_dim1:position_dim1+reste_record-1,&
                                                 idim2,idim3,idim4)
	     else
	       read(unit=unit,rec=irecord) decallage(1:position_record-1),&
	            var4d(position_dim1:position_dim1+reste_record-1,&
                          idim2,idim3,idim4)
             endif
             reste_dim1=reste_dim1-reste_record
	     reste_record=128
	     position_record=1
	     irecord=irecord+1
	   endif
	 enddo
	 enddo
	 enddo
	 enddo
       else
         !!message d erreur a rajouter : pas de tableau a remplir
       endif
    
    end subroutine lis_tableau_23
      
    subroutine cps_atmemcd_23_close ()
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine                                                       *
!$Nom                                                                  *
!     cps_atmemcd_23_close                                             *
!                                                                      *
!$Resume                                                               *
!      routine de desallocation memoire                                *
!$Auteur                                                               *
!      Florence VIVARES (ATOS Origin)                                  *
!$Usage                                                                *
!     call cps_atmemcd_23_close()                                      *
!                                                                      *
!$<>                                                                   *
!***********************************************************************

! Variable pour la gestion d'erreur de deallocate
      integer :: ierralloc

      if (modele_init) then
!
! desallocation des variables globales
!
         if (associated(tabps))     deallocate(tabps,      stat=ierralloc)
         if (associated(tabtsurf))  deallocate(tabtsurf,   stat=ierralloc)
         if (associated(tabt))      deallocate(tabt,       stat=ierralloc)
         if (associated(tabrho))    deallocate(tabrho,     stat=ierralloc)
         if (associated(tabu))      deallocate(tabu,       stat=ierralloc)
         if (associated(tabv))      deallocate(tabv,       stat=ierralloc)

         if (associated(tabsdps))   deallocate(tabsdps,    stat=ierralloc)
         if (associated(tabsdtsurf))deallocate(tabsdtsurf, stat=ierralloc)
         if (associated(tabsdt))    deallocate(tabsdt,     stat=ierralloc)
         if (associated(tabsdrho))  deallocate(tabsdrho,   stat=ierralloc)
         if (associated(tabsdu))    deallocate(tabsdu,     stat=ierralloc)
         if (associated(tabsdv))    deallocate(tabsdv,     stat=ierralloc)
 
         if (associated(tabpcsmth)) deallocate(tabpcsmth,  stat=ierralloc)
         if (associated(tabpcvar))  deallocate(tabpcvar,   stat=ierralloc)

         if (associated(tabeops))   deallocate(tabeops,    stat=ierralloc)
         if (associated(tabeot))    deallocate(tabeot,     stat=ierralloc)
         if (associated(tabeorho))  deallocate(tabeorho,   stat=ierralloc)
         if (associated(tabeou))    deallocate(tabeou,     stat=ierralloc)
         if (associated(tabeov))    deallocate(tabeov,     stat=ierralloc)
         
         modele_init = .false.
      endif
      

    end subroutine cps_atmemcd_23_close

end module cps_modele_emcd23
