module cps_modele_emcd31

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_emcd31
!
!$Resume
!  Modèles d'Atmosphère martien EMCD 3.1
!
!$Description
! Ce module contient la version 3.1 du modèle EMCD (European Martian
! Climate Database) :
! - routine cps_atmemcd_31
! 
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_modele_emcd31.F90 420 2013-03-12 13:27:07Z aadt $
!
!$Historique
!  $Log: cps_modele_emcd31.F90,v $
!  Revision 420  2013/03/12 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.12  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.11  2008/10/16 12:48:21  cml
!  DM-ID 1058 : Ajout d initialisations
!
!  Revision 1.10  2008/04/22 16:45:39  vivaresf
!  COMPAS V2.4, AQ : rajout de la gestion des répertoires dans cps_height
!
!  Revision 1.9  2008/04/11 17:44:08  vivaresf
!  FA-ID 1009 : intégration PSIMU,
!  gestion des / en fin de répertoire
!
!  Revision 1.8  2008/04/07 09:17:13  vivaresf
!  FA-ID 1009 : suppression des use cps_acces
!  correction des cartouches
!  vérification des accès par le biais de la base COMPAS aux modèles
!
!  Revision 1.7  2008/02/26 14:28:56  vivaresf
!  FA-ID 939 : écriture des messages d'erreur sur le stderr au lieu du stdout
!
!  Revision 1.6  2006/11/17 07:01:53  vivaresf
!  DM-ID 425 : code plus portage (float en real, alog en log, dsqrt en sqrt)
!
!  Revision 1.5  2006/11/14 17:57:29  vivaresf
!  DM-ID 425 : formattage des sorties
!
!  Revision 1.4  2006/11/13 11:55:12  vivaresf
!  DM-ID 425 : utilisation des routines intrinseques mod, sqrt, real au lieu de dmod, dsqrt ou dfloat
!
!  initialisation de gset dans gasdev
!
!  Revision 1.3  2006/05/30 15:17:39  vivaresf
!  regle de codage : suppression de *(*) obsolete
!
!  Revision 1.2  2006/03/03 15:21:24  vivaresf
!  DM-ID 493 : variables en common passees en tableaux dynamique
!
!  rajout des fonctions de desallocation (cps_atmemcd_*_close)
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.6  2005/10/12 11:01:21  vivaresf
!  DM-ID 388 : fonctions de parametrisation exportées pour
!  les objets GS_LIB
!
!  Revision 1.5  2005/10/05 09:29:10  bouchacp
!  DM-ID 386 : gasdev decalré deux fois en private
!
!  Revision 1.4  2005/03/07 08:15:50  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!
!  Revision 1.3  2005/03/04 16:39:50  vivaresf
!  DM-ID 318 : traduction des entêtes
!
!  Revision 1.2  2005/02/25 18:03:33  vivaresf
!  Mise a jour des entetes
!
!  Revision 1.1  2005/02/10 12:46:32  pauh
!  DM 318 : module cps_modele_emcd31 cree a partir du module cps_atm_emcd qui
!           regroupait les deux modeles d emcd dans un seul module. Celui-ci
!           ne contient donc que le modele emcd 3.1
!
!  Revision 1.5.4.1  2005/02/07 13:59:58  pauh
!  dm 318 : version de transfert, dm non achevee
!
!  Revision 1.5  2005/01/27 15:19:10  vivaresf
!  Fichiers d'include pour les modeles d'atmosphere
!
!  Revision 1.4  2005/01/27 12:57:14  vivaresf
!  DM-ID 318 : entetes des fichiers et des routines utilisateurs
!  nommage des fonctions publique, privatisation des fonctions internes
!  declaration des variables
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_emcd31
!
!$Remarques
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

  private eofpb_31, getsi_31,grid4_31,grwpb_31,loadvar_31,mars_ltime_31
  private mars_ptime_31,mcd_time_31,orbit_31,profi_31,season_31,sol2ls_31
  private var2d_31,var3d_31,max2d_31,min2d_31,gasdev_31, ran1_31

! Informations pour la configuration IHM

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_modele_emcd31.F90 420 2013-03-12 13:27:07Z aadt $'

  interface cps_season_31
  module procedure season_31
  end interface
!  
  interface cps_orbit_31
  module procedure orbit_31
  end interface


! Donnees

!c     include file defining contants and arrays used by the 
!c     European Martian Climate Database

!c CONSTANTS
!c     dimension  of mean and std. dev.values  in longitude, latitude, 
!c     sigma levels and database universal time
      integer, private :: dimlon,dimlat,dimlevs,dimuti
      parameter(dimlon=72,dimlat=36,dimlevs=32,dimuti=12)
!c     dimensions of EOF values in longitude, latitude, numbers of EOFs 
      integer, private :: dimloneo,dimlateo,dimnevecs,dimeoday
      parameter(dimloneo=18,dimlateo=9,dimnevecs=72,dimeoday=669)
!c     range of latitude, longitude, sigma levels and time for mean 
!c     and std. dev. values
      real, private :: lonmin,lonmax,latmin,latmax,levsmin,levsmax,utimin,utimax
      parameter(lonmin=-180.0,lonmax=175.0)
      parameter(latmin=-87.5,latmax=87.5)
      parameter(levsmin=0.9995000,levsmax=0.000000221)
      parameter(utimin=1.0,utimax=12.0)
!c     range of latitude, longitude, number of EOFs and time for EOFs values 
      real, private :: lonmineo,lonmaxeo,latmineo,latmaxeo,nevecsmin,nevecsmax
      real, private :: daymineo,daymaxeo
      parameter(lonmineo=-180.0,lonmaxeo=160.0)
      parameter(latmineo=-77.5,latmaxeo=82.5)
      parameter(nevecsmin=1.0,nevecsmax=72.0)
      parameter(daymineo=1.0,daymaxeo=669.0)
!c     step of grid in longitude and latitude for mean and std. dev. values
      real, private :: deltal
      parameter(deltal=5.0)
!c     step of grid in longitude and latitude for EOFs values
      real, private :: deltaleo
      parameter(deltaleo=20.0)
!c     mean Thermosphere temperature above database top (K)
      real, private :: t_thermo
      parameter (t_thermo=200.)

!c COMMON : common des sources d'origine
!      common /niveau_31/ sigma
!      common /orogra_31/ taborog,tabsubstd
!      common /moyenne_31/ tabps,tabtsurf,tabt,tabrho,tabu,tabv, &
!           tabfslw,tabfssw,tabftlw,tabftsw
!      common /stddev_31/ tabsdps,tabsdtsurf,tabsdt,tabsdrho,tabsdu,tabsdv
!      common /eofs_31/ tabeonormu,tabeonormv,tabeonormt,tabeonormp, &
!           tabeonormr,tabpcsmth,tabpcvar,tabeops,tabeot,tabeorho,   &
!           tabeou,tabeov 

!c     sigma level array (GCM layers + 1 thermosphere layer)
      real, save, private :: sigma(dimlevs +1 )
!c     orographic data array
!(dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: taborog => NULL()
!c     orographic variance array
!(dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: tabsubstd => NULL()
!c     mean value arrays : surface pressure, surface temperature
!c     temperature, density, zonal and meriodional wind components
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabps => NULL()
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabtsurf => NULL()
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabfslw => NULL()
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabfssw => NULL()
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabftlw => NULL()
!(dimlon,dimlat,dimuti)
      real, save, private, dimension(:,:,:), pointer :: tabftsw => NULL()
!(dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabt => NULL()
!(dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabrho => NULL()
!(dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabu => NULL()
!(dimlon,dimlat,dimlevs,dimuti)
      real, save, private, dimension(:,:,:,:), pointer :: tabv => NULL()
!c     standard deviation value arrays : surface pressure, surface temperature
!c     temperature, density, zonal and meriodional wind components
!(dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: tabsdps => NULL()
!(dimlon,dimlat,1)
      real, save, private, dimension(:,:,:), pointer :: tabsdtsurf => NULL()
!(dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdt => NULL()
!(dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdrho => NULL()
!(dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdu => NULL()
!(dimlon,dimlat,dimlevs,1)
      real, save, private, dimension(:,:,:,:), pointer :: tabsdv => NULL()
!c     arrays related to EOFs
!c     normalisation factors for zonal wind, meriodional wind, temperature
!c     pressure and density
      real, save, private :: tabeonormu(dimlateo)
      real, save, private :: tabeonormv(dimlateo)
      real, save, private :: tabeonormt(dimlateo)
      real, save, private :: tabeonormp(dimlateo)
      real, save, private :: tabeonormr(dimlateo)
!c     smoothed PCs
!(dimlateo,dimeoday,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabpcsmth => NULL()
!c     PCs variance
!(dimlateo,dimeoday,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabpcvar => NULL()
!c     EOFS for surface pressure, temperature, density, zonal wind
!c     and meriodional wind
!(dimloneo,dimlateo,dimnevecs)
      real, save, private, dimension(:,:,:), pointer :: tabeops => NULL()
!(dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeot => NULL()
!(dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeorho => NULL()
!(dimloneo,dimlateo,dimlevs,dimnevecs)
      real, save, private, dimension(:,:,:,:), pointer :: tabeou => NULL()
!(dimloneo,dimlateo,dimlevs,dimnevecs) 
      real, save, private, dimension(:,:,:,:), pointer :: tabeov => NULL()

! signale si les variables sont allouées ou non
      logical, save, private :: modele_init=.false.

  contains


    subroutine cps_atmemcd_31(xz,xlat,xlon,xdate,dset,scena,typper,invar &
          ,init_atm,seedout,ikey,pres,ro,temp,ventu,ventv &
          ,meanvar,extvar,ier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!     cps_atmemcd_31
!
!$Resume
!     Calcul de variables météorologiques avec le modèle EMCD version 3.1.
!
!$Acces
!  PUBLIC
!
!$Description
!     Calcul de variables météorologiques avec la Base de données 
!     Européenne des Climats Martiens. (EMCD, version 3.1)
! 
!    
!     Les donnees sont cherchées dans les données distribuées par
!     COMPAS (Modeles d'atmosphere martienne), sous le répertoire "data_emcd_3.1"
!     L'utilisateur peut aussi initialiser la variable "dset"
!     avec un nom de répertoire pour accéder à un jeu de données en local.
!
!$Version
!
!     3.1 05/01 bug fixes and improved interpolations
!     3.0 02/01 - Major MCD release based on earlier code
!     2.3 05/09/00
!     2.2 23/03/00
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Auteur
!
!     v3.1 Bugs fixed, new ls2sol routine and various improvements
!     to interpolation above model top, especially for EOFs -- SRL
!
!     Major updates for v3.0 MCD -- SRL + FF
!       See Changes file for more information.
!
!     Modifications for v2.3 MCD -- SRL
!
!     Derived from code for version 2 by C. HOURTOLLE
!
!$Usage
!     call cps_atmemcd_31(xz,xlat,xlon,xdate,dset,scena,typper,invar,init_atm,
!                  seedout,ikey,pres,ro,temp,ventu,ventv,meanvar,extvar,ier)
!
!$Arguments
!>E xz    : <PM_REEL>  Hauteur  / surface (m)
!>E xlat  : <PM_REEL>  Latitude (rad)
!>E xlon  : <PM_REEL>  Longitude Est(rad)
!>E xdate : <PM_REEL>  Date
!.             si xdate > 0 : jours juliens terrestre
!.             si xdate < 0 : 
!. date martienne = -(Int(solar longitude [deg] ) + localtime/100)
!. (ex : xdate = -90.12 correspond à Ls=90 ; LT=12:00) 
!. (ex : xdate = -173.18 correspond à Ls=173 ; LT=18:00) 
!>E dset  : <character(len=*)>    Jeu de données :
!.        Un ou plusieurs blancs pour avoir les données par défaut (répertoire
!         distribué par COMPAS)
!.     ou Répertoire des données EMCD avec slash final (e.g. '/dir/path/'),
!.     ou un lien sur le répertoire des données EMCD  (e.g. 'EMCD_DATA/').
!>E scena : <integer>  scénario de poussières : 
!.                           1 = modèle MGS,
!.                           2 = modèle Viking,
!.                           3 = modèle faible,
!.                           4 = tempête (tau=2),
!.                           5 = tempête (tau=5).
!>E typper: <PM_REEL, dim=2> Type de perturbation
!.   aucune : typper(1)= 1. typper(2)=0.
!.   grande échelle : typper(1)= 2. typper(2)=graine pour les variables aléatoires (voir seedout)
!.   petite échelle : typper(1)= 3. typper(2)=graine pour les variables aléatoires
!.   grande et petite échelle : typper(1)= 4. typper(2)=graine pour les variables aléatoires
!.   n fois l'écart type : typper(1)= 5. typper(2)=n
!>E invar : <double precision> Valeurs controlant la variabilité des modèles
!.     invar= Longueur d'onde des perturbations dues aux ondes de gravités (km)
!.     invar=0 pour avoir la valeur par défaut (16km)
!>E init_atm : <logical> Re-initialise le modèle de perturbations atmosphérique
!>E ikey  : <integer> Type de sortie
!.                 0 = pression, densité, température, vent
!.                 1 = calcul aussi les variables statistiques dans extvar
!>S seedout : <PM_REEL> Graine des variables aléatoires pour l'appel suivant
!>S pres  : <PM_REEL>   Pression (Pa)
!>S ro    : <PM_REEL>   Densité (kg/m^3)
!>S temp  : <PM_REEL>   Température (K)
!>S ventu : <PM_REEL>   Composante zonale du vent (Est-Ouest)
!>S ventv : <PM_REEL>   Composante méridionale du vent (Nord-Sud)
!>S meanvar : <PM_REEL, dim=5> Tableau des valeurs moyennes
!.                 meanvar(1)= pression moyenne
!.                 meanvar(2)= densité moyenne
!.                 meanvar(3)= température moyenne
!.                 meanvar(4)= composante zonale moyenne du vent
!.                 meanvar(5)= composante méridionale moyenne du vent
!>S extvar : <PM_REEL, dim=25>  Tableau de 25 variables statistiques :
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
!.                 extvar(11)= Température maximum quotidienne à la surface du sol (K)
!.                 extvar(12)= Température minimum quotidienne à la surface du sol (K)
!.                 extvar(13)= Perturbations à petites échelle
!                              (ondes de gravité) (kg/m^3)
!.                 extvar(14)= Hauteur orographique(m)
!.                 extvar(15)= Pression de surface moyenne (Pa)
!.                 extvar(16)= Pression surface quotidienne moyenne maximum  (Pa)
!.                 extvar(17)= Pression surface quotidienne moyenne minimum (Pa)
!.                 extvar(18)= Ecart type saisonnier de la pression de surface (Pa)
!.                 extvar(19)= Flux LW moyen vers la surface (W/m2)
!.                 extvar(20)= Flux SW moyen vers la surface (W/m2)
!.                 extvar(21)= Flux LW moyen vers l'espace (W/m2)
!.                 extvar(22)= Flux SW moyen vers l'espace (W/m2)
!.                 extvar(23)= Longitude aréocentric de Mars, Ls (deg)
!.                 extvar(24)= Heure solaire locale (hrs)
!.                 extvar(25)= Temps universel (hrs) (=heure locale à lon=0)
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
!    constants_atmemcd_31.h
!$<>
!***********************************************************************
!
!$Type
!     Subroutine 
!
!$Name
!     cps_atmemcd_31
!
!$Resume
!     Computation of meteorological variables with the 
!     European Martian Climate Database
!
!$Description
!     Computation of meteorological variables with the 
!     European Martian Climate Database, version 3.1
!     
!     Les donnees sont cherchees dans les donnees distribuees par
!     COMPAS (Modeles d'atmosphere martienne), sous le repertoire "data_emcd_3.1"
!     L'utilisateur peut aussi initialiser la variable "dset"
!     avec un nom de repertoire pour acceder a un jeu de données en local
!
!$Version
!
!     3.1 05/01 bug fixes and improved interpolations
!     3.0 02/01 - Major MCD release based on earlier code
!     2.3 05/09/00
!     2.2 23/03/00
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Author
!
!     v3.1 Bugs fixed, new ls2sol routine and various improvements
!     to interpolation above model top, especially for EOFs -- SRL
!
!     Major updates for v3.0 MCD -- SRL + FF
!       See Changes file for more information.
!
!     Modifications for v2.3 MCD -- SRL
!
!     Derived from code for version 2 by C. HOURTOLLE
!
!$Use
!     call cps_atmemcd_31(xz,xlat,xlon,xdate,dset,scena,typper,invar,init_atm,
!                  seedout,ikey,pres,ro,temp,ventu,ventv,meanvar,extvar,ier)
!
!$Arguments
!>E xz    : <double precision> height / zero datum (m)
!>E xlat  : <double precision> latitude (rad)
!>E xlon  : <double precision> longitude East (rad)
!>E xdate : <double precision> date 
!>                             IF xdate > 0 : Earth julian days
!>                             IF xdate < 0 : Mars date = 
!>                         -(Int(solar longitude [deg] ) + localtime/100)
!>                        (e.g. xdate = -90.12 correspond to Ls=90 ; LT=12:00) 
!>                        (e.g. xdate = -173.18 correspond to Ls=173 ; LT=18:00) 
!>E dset  : <character(len=*)>    data set
!>E                            One or more blanks to get the default,
!>E                            or full directory path of MCD data required
!>E                            including trailing slash (e.g. '/dir/path/'),
!>E                            or a link to a directory (e.g. 'EMCD_DATA/').
!>E                            Default is link EMCD_DATA in working directory.
!>E scena : <integer>          scenario
!>E                            1 = MGS dust,
!>E                            2 = Viking dust,
!>E                            3 = low dust,
!>E                            4 = dust storm tau=2,
!>E                            5 = dust storm tau=5,
!>E typper: <double precision> dim 2  perturbation type
!>E                           none : typper(1)= 1. typper(2)=0.
!>E                    large scale : typper(1)= 2. typper(2)=seed number
!>E                    small scale : typper(1)= 3. typper(2)=seed number
!>E          large and small scale : typper(1)= 4. typper(2)=seed number
!>E n times the standard deviation : typper(1)= 5. typper(2)=n
!>E invar : <double precision> Values controlling the variability models
!>E                 invar(1)= wavelength of gravity wave perturbation (km)
!>E                           set to zero to get default (16km)
!>E init_atm : <logical> Re-initialize atmospheric model perturbations.
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
!>S extvar : <double precision> dim 25 extra variables array
!>S                 extvar(1) = upper density value (kg/m^3)
!>S                 extvar(2) = lower density value (kg/m^3)
!>S                 extvar(3) = density standard deviation (kg/m^3)
!>S                 extvar(4) = random density perturbation (kg/m^3)
!>S                 extvar(5) = scale height H(p) (km)
!>S                 extvar(6) = density scale height H(rho) (km)
!>S                 extvar(7) = reserved
!>S                 extvar(8) = reserved
!>S                 extvar(9) = reserved
!>S                 extvar(10)= mean ground temperature (K)
!>S                 extvar(11)= daily maximum mean ground temperature (K)
!>S                 extvar(12)= daily minimum mean ground temperature (K)
!>S                 extvar(13)= small scale perturbation (gravity wave) (kg/m^3)
!>S                 extvar(14)= orographic height (m)
!>S                 extvar(15)= mean surface pressure (Pa)
!>S                 extvar(16)= daily maximum mean surface pressure (Pa)
!>S                 extvar(17)= daily minimum mean surface pressure (Pa)
!>S                 extvar(18)= seasonal std. dev. surface pressure (Pa)
!>S                 extvar(19)= mean LW flux to surface (W/m2)
!>S                 extvar(20)= mean SW flux to surface (W/m2)
!>S                 extvar(21)= mean LW flux to space (W/m2)
!>S                 extvar(22)= mean SW flux to space (W/m2)
!>S                 extvar(23)= areocentric longitude of Mars, Ls (deg)
!>S                 extvar(24)= local solar time (hrs)
!>S                 extvar(25)= universal time (hrs) (=loc time at lon=0)
!>S               ier  : <integer> error flag
!>S                 0 = OK
!>S                 1 = unknown database
!>S                 2 = unknown scenario
!>S                 3 = unknown perturbation type
!>S                 4 = underground object
!>S                 5 = no dust storm for the season
!>S                 6 = impossible to open database file
!>S                 7 = impossible to load data from database
!>S                 8 = undetermined sigma levels
!$Include
!    constants_atmemcd_31.h

      implicit none

!#include "constants_atmemcd_31.h"

      real(kind=PM_REEL) :: xz,xlat,xlon,xdate,typper(2),invar
      real(kind=PM_REEL) :: seedout,pres,ro,temp,ventu,ventv,extvar(25) &
          ,meanvar(5)
      integer scena,ikey,ier
      character(len=*) dset
      logical init_atm

!c     channel number :
!c     NETCDF nc file: mean data
!c     NETCDF nc file: std dev data
!c     NETCDF nc file: orography data
!c     NETCDF nc file: eof data

!c     local variables
!c     NETCDF channel number - orographic data
      integer unet
!c     NETCDF channel number - mean fields      
      integer unetm
!c     NETCDF channel number - std dev fields
      integer unetsd
!c     NETCDF channel number - eof fields
      integer ueof

      save unet,unetm,unetsd,ueof

!c     seed for random number generator
      integer seed
      real    R 
      parameter(R=191.2)
      real    g
      data g  /3.72/
!c     dust scenario
      integer  dust
!c     perturbation type
      integer varflag
!c     number of standard deviation
      real nbsig
      real   pi 
      parameter(pi=3.14159265359)
      real    crd
      parameter(crd=180./pi)
!c     difference in degree between 2 consecutive points  
!c     leading to change random phase for gravity wave
      real deltalgw
      parameter(deltalgw=1.)

      integer i,ierr
      real    height     !height (km)
      real    oroheight  !orographic height
      real    absheight  !height above surface
      real    ls
      real    sunlon
      real    sunlat
      real    marsau
      real    localtime
      real    utime      !Universal time (0. to 24. hrs) =local time at lon=0
      real    lon        !longitude west [0 .. 360]
      real    lat        !latitude (degrees)
      real    t
      real    tl         !l indicates low level variable from database
      real    ps
      real    p
      real    pl
      real    rho
      real    rhol
      real    pertm
      real    pertr
      real    pertrho
      real    pertps
      real    u
      real    ul
      real    v
      real    vl
      real    pertu, pertv, pertt
      integer levhi
      integer levlow
      real    levweight(2)
      real    pratio
      real    lamda       !gravity wave vertical wavelength (km)
      real, dimension(dimnevecs), save :: rdnos
      real, save :: dev
      real    modelday
      character(len=16) name
      real pscaleheight   !scale height
      real sdrhol, rholol, rhohil, pertmrho, pertrrhol
      real pertrrho, rholo, rhohi, sdrho
      real rscaleheight   !density scale height
      real tsurf,tsurfmax,tsurfmin
      real psurf,psurfmax,psurfmin,psurfsd
      real fluxsurf_lw, fluxsurf_sw, fluxtop_lw, fluxtop_sw
      real tmeanl(5)
      character(len=4) typevar
      character(len=255) dataset
      integer lendataset
!c     season numbers
      integer nums,numsprec, numsprec_eo,numsprec_sd,numsprec_gw
      data numsprec /0/
      data numsprec_eo /0/
      data numsprec_sd /0/
      data numsprec_gw /0/
!c     previous scenario
      integer dustprec 
      data dustprec /0/
!c     previous latitude and longitude
      real prevlon,  prevlat
      data prevlon /-999./
      data prevlat /-999./
      save numsprec,numsprec_eo,numsprec_sd,numsprec_gw &
         ,dustprec,prevlat,prevlon
!c     function declarations
!      real gasdev_31,ran1_31,ls2sol_31
! Variable pour la gestion d'erreur de allocate/deallocate
      integer :: ierralloc

!
! Fin declarations
!
      ier=0

      ! Initialisations
      sdrho = 0.
      rhohi = 0.
      rholo = 0.
      pertrrho = 0.
      rscaleheight = 0.
      pscaleheight = 0.

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
        ier = cps_getFichierModele("atmosphere", "EMCD3_1", dataset, &
           rep=.true.)
         if (MSP_gen_messages("cps_atmemcd_31")) return
! Valeur attendue un lien sur :
!         dataset = '/usr/local_ms/data/data_emcd_3.1/'
      else  !symbolic link or full path given explicitly
         dataset = dset
      end if
      lendataset = len_trim(dataset)
      if (dataset(lendataset:lendataset).ne."/") then
         dataset = trim(dataset) // "/"
         lendataset=lendataset+1
      endif

!c     check value of dust scenario
      dust = scena
      if (dust.lt.1.or.dust.gt.5) then
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
!c     if init_atm recompute rdnos and dev
         if (init_atm) then
            do i=1,dimnevecs
               rdnos(i)=gasdev_31(seed)
            enddo
            dev=ran1_31(seed)
            seedout=real(-seed,kind=PM_REEL)
         endif
      else
         seedout=0.d0
      endif
!c     case n std dev perturbation
      if (varflag.eq.5) then
         nbsig = real(typper(2))
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
         lamda = real(invar)
      endif

!c     convert longitude (rd) East to longitude (deg) West between 0..360 degrees
      lon=real(xlon)*crd
      lon=mod(lon,360.)
      if(lon.lt.0.) then
         lon=-lon
      elseif (lon.gt.0.) then
         lon=360.-lon
      endif
!c     convert latitude from radians to degrees
      lat=real(xlat)*crd
!c     convert height from  m to km
      height = real(xz)/1000.

!c     if you moved a large distance, recompute gravity wave random phase dev
!c     moving is not tested if a new perturbed atmosphere is required
      if (.not.init_atm) then 
         if((varflag.eq.3).or.(varflag.eq.4).or.(ikey.eq.1)) then
            if ((abs(prevlat-lat).gt.deltalgw).or. &
                (abs(prevlon-lon).gt.deltalgw)) then
               dev=ran1_31(seed)
               seedout=real(-seed,kind=PM_REEL)
            endif
         endif
      end if
      prevlon=lon
      prevlat=lat
      init_atm = .false.

!c     Compute Ls and the corresponding season number
!c     **********************************************
      if (xdate.gt.0) then    ! Earth Julian date

         if(xdate.lt.360) &
        write(0,*)'ATMEMCD Warning: Are you sure you want to use Earth'  &
         , ' julian date ? Mars date must be <0 in input'

         call orbit_31(xdate,sunlat,sunlon,ls,marsau,modelday)
!c        call mars_ltime with current longitude for correct local time
         call mars_ltime_31(lon,sunlon,localtime)

      else                     ! Mars "xdate" (see manuals)
         ls=real(int(-xdate))
         localtime = real(100.*(-xdate -ls),kind=4)
         if(localtime.gt.24) &
           write(0,*) 'ATMEMCD warning : problem with local time > 24'
!c        find modelday corresponding to ls
         ls = mod(ls,360.)
         modelday = ls2sol_31(ls)
      end if

      call season_31(ls,nums)
      if ((dust.ge.4).and.((nums.lt.8).or.(nums.gt.10))) then
           write(0,*)'ATMEMCD Error : no dust storm for season ',nums
           ier=5
           goto 9999
      endif
      call mars_ptime_31(lon,localtime,utime)

! allocations dynamiques
      if (.not.modele_init) then
         
         if (associated(taborog)) deallocate(taborog, stat=ierralloc)
         allocate(taborog(dimlon,dimlat,1))
         if (associated(tabsubstd)) deallocate(tabsubstd, stat=ierralloc)
         allocate(tabsubstd(dimlon,dimlat,1))
         if (associated(tabps)) deallocate(tabps, stat=ierralloc)
         allocate(tabps(dimlon,dimlat,dimuti))
         if (associated(tabtsurf)) deallocate(tabtsurf, stat=ierralloc)
         allocate(tabtsurf(dimlon,dimlat,dimuti))
         if (associated(tabfslw)) deallocate(tabfslw, stat=ierralloc)
         allocate(tabfslw(dimlon,dimlat,dimuti))
         if (associated(tabfssw)) deallocate(tabfssw, stat=ierralloc)
         allocate(tabfssw(dimlon,dimlat,dimuti))
         if (associated(tabftlw)) deallocate(tabftlw, stat=ierralloc)
         allocate(tabftlw(dimlon,dimlat,dimuti))
         if (associated(tabftsw)) deallocate(tabftsw, stat=ierralloc)
         allocate(tabftsw(dimlon,dimlat,dimuti))
         if (associated(tabt)) deallocate(tabt, stat=ierralloc)
         allocate(tabt(dimlon,dimlat,dimlevs,dimuti))
         if (associated(tabrho)) deallocate(tabrho, stat=ierralloc)
         allocate(tabrho(dimlon,dimlat,dimlevs,dimuti))
         if (associated(tabu)) deallocate(tabu, stat=ierralloc)
         allocate(tabu(dimlon,dimlat,dimlevs,dimuti))
         if (associated(tabv)) deallocate(tabv, stat=ierralloc)
         allocate(tabv(dimlon,dimlat,dimlevs,dimuti))
         if (associated(tabsdps)) deallocate(tabsdps, stat=ierralloc)
         allocate(tabsdps(dimlon,dimlat,1))
         if (associated(tabsdtsurf)) deallocate(tabsdtsurf, stat=ierralloc)
         allocate(tabsdtsurf(dimlon,dimlat,1))
         if (associated(tabsdt)) deallocate(tabsdt, stat=ierralloc)
         allocate(tabsdt(dimlon,dimlat,dimlevs,1))
         if (associated(tabsdrho)) deallocate(tabsdrho, stat=ierralloc)
         allocate(tabsdrho(dimlon,dimlat,dimlevs,1))
         if (associated(tabsdu)) deallocate(tabsdu, stat=ierralloc)
         allocate(tabsdu(dimlon,dimlat,dimlevs,1))
         if (associated(tabsdv)) deallocate(tabsdv, stat=ierralloc)
         allocate(tabsdv(dimlon,dimlat,dimlevs,1))

         if (associated(tabpcsmth)) deallocate(tabpcsmth, stat=ierralloc)
         allocate(tabpcsmth(dimlateo,dimeoday,dimnevecs))
         if (associated(tabpcvar)) deallocate(tabpcvar, stat=ierralloc)
         allocate(tabpcvar(dimlateo,dimeoday,dimnevecs))
         if (associated(tabeops)) deallocate(tabeops, stat=ierralloc)
         allocate(tabeops(dimloneo,dimlateo,dimnevecs))
         if (associated(tabeot)) deallocate(tabeot, stat=ierralloc)
         allocate(tabeot(dimloneo,dimlateo,dimlevs,dimnevecs))

         if (associated(tabeorho)) deallocate(tabeorho, stat=ierralloc)
         allocate(tabeorho(dimloneo,dimlateo,dimlevs,dimnevecs))
         if (associated(tabeou)) deallocate(tabeou, stat=ierralloc)
         allocate(tabeou(dimloneo,dimlateo,dimlevs,dimnevecs))
         if (associated(tabeov)) deallocate(tabeov, stat=ierralloc)
         allocate(tabeov(dimloneo,dimlateo,dimlevs,dimnevecs))

         ! initialisation des tableaux dynamiques
         taborog(:,:,:) = 0.
         tabsubstd(:,:,:) = 0.
         tabps(:,:,:) = 0.
         tabtsurf(:,:,:) = 0.
         tabfslw(:,:,:) = 0.
         tabfssw(:,:,:) = 0.
         tabftlw(:,:,:) = 0.
         tabftsw(:,:,:) = 0.
         tabt(:,:,:,:) = 0.
         tabrho(:,:,:,:) = 0.
         tabu(:,:,:,:) = 0.
         tabv(:,:,:,:) = 0.
         tabsdps(:,:,:) = 0.
         tabsdtsurf(:,:,:) = 0.
         tabsdt(:,:,:,:) = 0.
         tabsdrho(:,:,:,:) = 0.
         tabsdu(:,:,:,:) = 0.
         tabsdv(:,:,:,:) = 0.
         tabpcsmth(:,:,:) = 0.
         tabpcvar(:,:,:) = 0.
         tabeops(:,:,:) = 0.
         tabeot(:,:,:,:) = 0.
         tabeorho(:,:,:,:) = 0.
         tabeou(:,:,:,:) = 0.
         tabeov(:,:,:,:) = 0.
         modele_init = .true.

      endif

!c     load arrays from data base
!c     **************************
!c     if the scenario changes, reset season numbers to reload all needed arrays
      if (dust.ne.dustprec) then
         numsprec=0
         numsprec_eo=0
         numsprec_sd=0
         numsprec_gw=0
         dustprec=dust
      endif
!c     if the season number changes, mean value have to be reloaded
      if (nums.ne.numsprec) then
!c     Open appropriate  NETCDF file
         call opend_31(unet,unetm,unetsd,ueof,nums,dust, &
                   dataset(1:lendataset),ierr)
         if (ierr.ne.0) then
            ier=6
            goto 9999
         endif
         typevar='mean'
         call loadvar_31(unetm,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
!c     at the first call, load sigma and orographic data
         if (numsprec.eq.0) then
            typevar='sigm'
            call loadvar_31(unetm,typevar,ierr)
            if (ierr.ne.0) then
               ier=7
               go to 9999
            endif
            typevar='orog'
            call loadvar_31(unet,typevar,ierr)
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
         call loadvar_31(ueof,typevar,ierr)
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
         call loadvar_31(unet,typevar,ierr)
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
         call loadvar_31(unetsd,typevar,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
         numsprec_sd=nums
      end if
      

!c     *************************************
!c     *Meteorological variable computation*
!c     *************************************

!c     calculate oroheight
      name='orography'
      call var2d_31(oroheight,lon,lat,1.0,name,ierr)
!c     calculate sigma level at height=absheight
!c     (absheight=height above ground)
      absheight=height-oroheight/1000.

!c     if underground : stop
      if (absheight.lt.0.) then
         write(0,*)'ATMEMCD Error: underground object '
         ier=4
         goto 9999
      endif
      call getsi_31(lon,lat,absheight,utime, &
          levhi,levlow,levweight,pratio,ierr)
      if (ierr.ne.0) then
         write(0,*)'ATMEMCD Error: undefined sigma levels'
         ier=8
         goto 9999
      end if
!c     read mean data from stored arrays.
      name='u'
      call var3d_31(ul,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='v' 
      call var3d_31(vl,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='t'
      call var3d_31(tl,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)
      name='rho'
      call var3d_31(rhol,lon,lat,levhi,levlow,levweight,pratio, &
          utime,name,ierr)         
      name='ps' 
      call var2d_31(ps,lon,lat,utime,name,ierr)
!c     storage of mean values
      tmeanl(1)=pratio*ps*(sigma(levlow) &
          +(sigma(levhi)-sigma(levlow))*levweight(1))
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
         call eofpb_31(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         rhol=rhol+pertr
         name='u'
         call eofpb_31(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         ul=ul+pertr
         name='v'
         call eofpb_31(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         vl=vl+pertr
         name='t'
         call eofpb_31(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         tl=tl+pertr
         name='ps'
         call eofpb_31(pertm,pertr,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         ps=ps+pertr
      endif

!c     add small scale (gravity wave) variability to u,v,t,rho if required
      if ((varflag.eq.3).or.(varflag.eq.4)) then
         name='u'
         call grwpb_31(pertu,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='v'
         call grwpb_31(pertv,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='t'
         call grwpb_31(pertt,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         name='rho'
         call grwpb_31(pertrho,dev,lamda,lon,lat,absheight,tmeanl(2), &
             tmeanl(4),tmeanl(5),utime,name,ierr)
         ul = ul + pertu
         vl = vl + pertv
         tl = tl + pertt
         rhol= rhol + pertrho
      endif

!c     add n sigmas if required
      if (varflag.eq.5) then
         name='sdu'
         call var3d_31(pertu,lon,lat,levhi,levlow,levweight,pratio, &
             1.0,name,ierr)
         name='sdv'
         call var3d_31(pertv,lon,lat,levhi,levlow,levweight,pratio, &
             1.0,name,ierr)
         name='sdrho'
         call var3d_31(pertrho,lon,lat,levhi,levlow,levweight,pratio, &
             1.0,name,ierr)
         name='sdt'
         call var3d_31(pertt,lon,lat,levhi,levlow,levweight,pratio, &
             1.0,name,ierr)
         name='sdps'
         call var2d_31(pertps,lon,lat,1.0,name,ierr)
         ul = ul + (nbsig*pertu)
         vl = vl + (nbsig*pertv)
         rhol = rhol + (nbsig*pertrho)
         tl = tl + (nbsig*pertt)
         ps = ps + (nbsig*pertps)
      endif

!c     calculate pressure
      pl=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow))*levweight(1))
      pl=pratio*pl

!c     compute extra variables relative to scale height and density
      if (ikey.eq.1) then
         pscaleheight=R*tl/g
         pscaleheight=pscaleheight/1000.
         rscaleheight=pscaleheight
         name='sdrho'
         call var3d_31(sdrhol,lon,lat,levhi,levlow,levweight,pratio, &
             1.0,name,ierr)
         rholol=tmeanl(2)-sdrhol
         rhohil=tmeanl(2)+sdrhol
         name='rho'
         call eofpb_31(pertmrho,pertrrhol,rdnos,lon,lat, &
             levhi,levlow,levweight,pratio,modelday,name,ierr)
         pertrrho=pertrrhol
         call grwpb_31(pertr,dev,lamda,lon,lat,absheight,tmeanl(2), &
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
!c        (re)compute extra variables where required
         name='tsurf'
         call var2d_31(tsurf,lon,lat,utime,name,ierr)
         call max2d_31(tsurfmax,lon,lat,name,ierr)
         call min2d_31(tsurfmin,lon,lat,name,ierr)
         name='ps'
         call var2d_31(psurf,lon,lat,utime,name,ierr)
         call max2d_31(psurfmax,lon,lat,name,ierr)
         call min2d_31(psurfmin,lon,lat,name,ierr)
         name='sdps'
         call var2d_31(psurfsd,lon,lat,1.0,name,ierr)
         name='fluxsurf_lw'
         call var2d_31(fluxsurf_lw,lon,lat,utime,name,ierr)
         name='fluxsurf_sw'
         call var2d_31(fluxsurf_sw,lon,lat,utime,name,ierr)
         name='fluxtop_lw'
         call var2d_31(fluxtop_lw,lon,lat,utime,name,ierr)
         name='fluxtop_sw'
         call var2d_31(fluxtop_sw,lon,lat,utime,name,ierr)
!c        store values of extra variables
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
         extvar(11)=real(tsurfmax,kind=PM_REEL)
         extvar(12)=real(tsurfmin,kind=PM_REEL)
         extvar(13)=real(pertr,kind=PM_REEL)
         extvar(14)=oroheight
         extvar(15)=real(psurf,kind=PM_REEL)
         extvar(16)=real(psurfmax,kind=PM_REEL)
         extvar(17)=real(psurfmin,kind=PM_REEL)
         extvar(18)=real(psurfsd,kind=PM_REEL)
         extvar(19)=real(fluxsurf_lw,kind=PM_REEL)
         extvar(20)=real(fluxsurf_sw,kind=PM_REEL)
         extvar(21)=real(fluxtop_lw,kind=PM_REEL)
         extvar(22)=real(fluxtop_sw,kind=PM_REEL)
         extvar(23)=real(ls,kind=PM_REEL)
         extvar(24)=real(localtime,kind=PM_REEL)
         extvar(25)=real(utime,kind=PM_REEL)
      else
         do i=1,25
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
      do i=1,25
         extvar(i)=-999.d0
      enddo
      return

    end subroutine cps_atmemcd_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine eofpb_31(pertm,pertr,rdnos,lonw,lat,levhi,levlow, &
                      levweight,pratio,day,name,ier)
!c
!c     Calculate an EOF perturbation on the 3d variable=name at 
!c     longitude=lon, latitude=lat sigma=sigma and day=day.
!c
!c     Improved variability model...EOFs calculated in longitude-sigma plane
!c
      implicit none

!#include "constants_atmemcd_31.h"
!c
!c     input
!c
      real          rdnos(dimnevecs) !Uniform deviates for a single profile
      real          lonw      !longitude west of point 
      real          lat       !latitude of point
      integer       levhi     !database level upper bound
      integer       levlow    !database level lower bound
      real          levweight(2) !level weight for interpolation
!c     (1) for linear in height (2) for linear in pressure
      real          pratio    !ratio of pressure to extreme value if out of range
      real          day       !model day
      character(len=16)  name      !Name of variable
!c
!c     output
!c
      integer ier          !error flag (0=OK, 1=NOK)
      real    pertm        !perturbation corresponding to trend
      real    pertr        !perturbation corresponding to random component
!c
!c     local variables
!c
      integer i
      integer indlat,indlon,indday
      integer iilat,iilon
      real    norm
      real    pcsmth(dimnevecs)
      real    pcvar(dimnevecs)
      real    evecs(dimnevecs)
      real    lon         !longitude between -180 and 180
      real    alat,alon,aday,u

      indlat = 0
      indlon = 0
      indday = 0
      
      ier = 0

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
      if (lon.le.lonmineo) then
         u=(lonmineo-lon)/deltaleo
         if (u.le.0.5)then
            indlon=1
         else
            indlon=dimloneo
         endif
      elseif (lon.ge.lonmaxeo) then
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
      if (day.le.daymineo) then
         u=daymineo-day
         if (u.le.0.5)then
            indday=1
         else
            indday=dimeoday
         endif
      elseif (day.ge.daymaxeo) then
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

!c     read normalisation factor and eofs
!c     normalisation factor is no longer the standard deviation
!c
!c     EOF perturbations are only stored up to the last model level
!c     (not the thermosphere level).
      if (levlow.lt.dimlevs) then  ! we are entirely within EOF range
      if (name.eq.'u') then
         norm=tabeonormu(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeou(indlon,indlat,levlow,i) &
                  +  (tabeou(indlon,indlat,levhi,i) &
                  -  tabeou(indlon,indlat,levlow,i))*levweight(1)
         enddo
      elseif (name.eq.'v') then
         norm=tabeonormv(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeov(indlon,indlat,levlow,i) &
                  +  (tabeov(indlon,indlat,levhi,i) &
                  -  tabeov(indlon,indlat,levlow,i))*levweight(1)
         enddo
      elseif (name.eq.'t') then
         norm=tabeonormt(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeot(indlon,indlat,levlow,i) &
                  +  (tabeot(indlon,indlat,levhi,i) &
                  -  tabeot(indlon,indlat,levlow,i))*levweight(1)
         enddo
      elseif (name.eq.'rho') then
         norm=tabeonormr(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeorho(indlon,indlat,levlow,i) &
                  +  (tabeorho(indlon,indlat,levhi,i) &
                  -  tabeorho(indlon,indlat,levlow,i))*levweight(2)
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
      else  ! we are above the top EOF level, most perturbations constant
      if (name.eq.'u') then
         norm=tabeonormu(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeou(indlon,indlat,dimlevs,i)
         enddo
      elseif (name.eq.'v') then
         norm=tabeonormv(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeov(indlon,indlat,dimlevs,i)
         enddo
      elseif (name.eq.'t') then
         norm=tabeonormt(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeot(indlon,indlat,dimlevs,i)
         enddo
      elseif (name.eq.'rho') then
         norm=tabeonormr(indlat)
         do i=1,dimnevecs
!c scale density perturbation between top EOF level and thermosphere
!c           pick a mean grid point close to EOF point for temperature
            iilon = int((indlon-1)*deltaleo/deltal) + 1
            iilat = int((indlat-0.5)*deltaleo/deltal) + 1
            evecs(i)= tabeorho(indlon,indlat,dimlevs,i) &
                *(1.+levweight(2)*(sigma(dimlevs+1)/sigma(dimlevs) &
                  *(tabt(iilon,iilat,dimlevs,1)/t_thermo)-1.))
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
!c re-scaling correction for densities which are above thermosphere level
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

    end subroutine eofpb_31
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    real function gasdev_31(idum)
!c
!c     Return a Gaussian deviate with zero mean and unit standard deviation
!c
      implicit none
!c     input
!c
      integer idum    !Seed for random number generator
!c
!c     local
!c
      integer :: iset = 0
      real :: v1
      real :: v2
      real :: r
      real :: fac
      real :: gset = 0.

! function decalartions
!      real ran1_31,gasdev_31
      if (iset.eq.0) then
1       v1=2.*ran1_31(idum)-1.
        v2=2.*ran1_31(idum)-1.
        r=v1**2+v2**2
        if (r.ge.1.) goto 1
        fac=sqrt(-2.*log(r)/r)
        gset=v1*fac
        gasdev_31=v2*fac
        iset=1
      else
        gasdev_31=gset
        iset=0
      endif
      return
    end function gasdev_31
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsi_31(lonw,lat,height,utime, &
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

!#include "constants_atmemcd_31.h"

      real    height     !height of point
      real    lonw       !west longitude
      real    lat        !latitude
      real    utime      !Universal time (0. to 24. hrs) =local time at lon=0
!c
!c     outputs
!c
      integer ier       !error flag (0=OK, 1=NOK)
      integer levhi     !database level upper bound
      integer levlow    !database level lower bound
      real    levweight(2) !level weight for interpolation
!c     (1) for linear in height (2) for linear in pressure
      real    pratio    !p/p(top, bottom) for extrapolation, 1 if in range
!c
!c     locals
!c
      integer l,ierr
      real    dsigma(dimlevs+1)   ! database layers + 1 thermosphere
      real    hsigma(dimlevs+2)
      real    sheight(dimlevs+1)
      real    g 
      data    g /3.72/
      real    R 
      data    R /191.1/
      real    Rogct , Tmean
      character(len=16)  name 
      data  name /'t'/
      real    t(dimlevs+1)        ! database layers + 1 thermosphere
      real    lon

      ier=0

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
      do l=2,dimlevs+1
        hsigma(l)=.5*(sigma(l-1)+sigma(l))
      enddo
      hsigma(dimlevs+2)=0.
      dsigma(1)=1.-sigma(1)
      do l=2,dimlevs+1
        dsigma(l)=sigma(l-1)-sigma(l)
      enddo
!c
!c     get temperature profile
!c
      call profi_31(t,lon,lat,utime,name,ierr)
      if (ierr.ne.0) then
         write(0,*)'GETSI Error : temperature profile not available'
         ier=1
         goto 9999
      endif
!c
!c     Integrate hydrostatic equation
!c     Rogct is the R/g constant divided by 1000 for km 
       Rogct=0.001*R/g
       Tmean = t(1)
       sheight(1)= - Rogct*t(1)*log(sigma(1))
       do l=2 , dimlevs+1
         if (t(l).ne.t(l-1)) then
           Tmean = (t(l) -t(l-1)) / log(t(l)/t(l-1))
         else
           Tmean = t(l)
         end if
         sheight(l)=sheight(l-1) - Rogct*Tmean*log(sigma(l)/sigma(l-1))
       end do

!c     find levhi, levlow and levweight
!c
      if (height.lt.sheight(1)) then
!c       below the lowest layer
        levhi=1
        levlow=1
        levweight(1)=1.
        levweight(2)=1.
        pratio=exp(1000.*(sheight(1)-height)*g/(R*t(1)))
      elseif (height.ge.sheight(dimlevs+1)) then
!c       above the additional "thermosphere" layer
        levhi=dimlevs+1
        levlow=dimlevs+1
        levweight(1)=1.
        levweight(2)=1.
        pratio=exp(1000.*(sheight(dimlevs+1)-height)*g/(R*t(dimlevs+1)))
      else
      do l=1,dimlevs
        if ((height.ge.sheight(l)).and.(height.lt.sheight(l+1))) then
          levhi=l+1
          levlow=l
          levweight(1)=(height-sheight(levlow)) &
                     /(sheight(levhi)-sheight(levlow))
          levweight(2)=(1.-(sigma(levhi)/sigma(levlow))**levweight(1)) &
                     /(1.-(sigma(levhi)/sigma(levlow)))
          pratio=1.
        endif
      enddo
      endif
      return

!c     Errror handling
 9999 levhi=0
      levlow=0
      levweight(1)=0.
      levweight(2)=0.
      pratio=0.
      return

    end subroutine getsi_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grid4_31(lon,lat,dlon,dlat,t,u)
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

!#include "constants_atmemcd_31.h"

!c     inputs
!c
      real lon      !east longitude of point
      real lat      !latitude of point
!c
!c     outputs
!c
      integer dlon(4)  !indices longitudes of database points
      integer dlat(4)  !indices latitudes of database points
      real t        !weight
      real u        !weight
!c
!c     local
!c
      real alon,alat
      integer i
!c
!c     Initialisation
      dlat(:) = 0
      dlon(:) = 0
      u = 0.0
      t = 0.0

!c     wraparound
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
    end subroutine grid4_31

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grwpb_31(pert,dev,lamda,lonw,lat,height,rho,u,v,utime, &
          name,ier)
!c
!c     Small scale gravity wave perturbation model.
!c     Computes perturbation on u, v, theta or rho, with wave phase factor.
!c
!c     input
!c
      implicit none

      real dev          !random number between 0 and 1
      real lamda        !vertical wavelength of g.w. (km)
      real lonw         !west longitude of point
      real lat          !latitude of point
      real height       !height of point
      real rho          !density at height=height
      real u            !zonal wind at height=height
      real v            !meridional wind at height=height
      real    utime     !Universal time (0. to 24. hrs) =local time at lon=0



      character(len=16) name !name of variable to perturb
!c
!c     output
!c
      integer ier   !error flag (0=OK, 1=NOK)
      real pert     !perturbation
!c
!c     local
!c
      integer ierr
      real a, aprime
      real rho0,rho1
      real u0,u1
      real v0,v1
      real tmpheight
      integer levlow
      integer levhi
      real levweight(2)
      real pratio
      real dz
      real dz_max 
      real sig
      real umag
      real hmax         !for perturbations above hmax, use amplitude at hmax (km)
      parameter(hmax=100.0)
      real usat         !for windspeeds below usat, wave amplitude saturates
      parameter(usat=0.5)
      real dalr         !dry adiabatic lapse rate (K/km)
      parameter(dalr=4.5)
      real pi 
      parameter(pi=3.14159265359)
      character(len=16) tmpname

      ier=0
!c
!c     get rho0, u0, v0
!c     use mean data from level 1, not height=0.0
      levlow=1
      levhi=1
      levweight(1)=1.
      levweight(2)=1.
      pratio=1.0
      tmpname='u'
      call var3d_31(u0,lonw,lat,levhi,levlow,levweight,pratio,utime, &
                tmpname,ierr)
      tmpname='v'
      call var3d_31(v0,lonw,lat,levhi,levlow,levweight,pratio,utime, &
                tmpname,ierr)
      tmpname='rho'
      call var3d_31(rho0,lonw,lat,levhi,levlow,levweight,pratio,utime, &
                tmpname,ierr)
!c
!c     get sub-grid scale variance
!c
      tmpname='substd'
      call var2d_31(sig,lonw,lat,1.0,tmpname,ierr)
      sig=sig/1000.
!c
!c     compute delta z (N=N0 is implicit)
!c
      if (height.le.hmax) then
         umag=sqrt(u**2+v**2)
!c        the wave amplitude becomes large as the wind speed becomes small
!c        and the wave should saturate
         if (umag.lt.usat) umag=usat
         dz=(rho0*sqrt(u0**2+v0**2)*sig)/(rho*umag)
         dz=sqrt(dz)
         tmpheight=height
      else
!c         perturbation above hmax km:
!c         Using the same amplitude and same perturbation
!c         as at hmax km for T,u,v. The perturbation for rho is
!c         scaled on the T perturbation for rho at height=height     
         tmpheight=hmax          ! set height for perturbation calculation
!c         get info on variable at hmax km:
         call getsi_31(lonw,lat,tmpheight,utime, &
                levhi,levlow,levweight,pratio,ierr)
         tmpname='rho'
         call var3d_31(rho1,lonw,lat,levhi,levlow,levweight, &
             pratio,utime, tmpname,ierr)
         tmpname='u'
         call var3d_31(u1,lonw,lat,levhi,levlow,levweight, &
             pratio,utime, tmpname,ierr)
         call var3d_31(v1,lonw,lat,levhi,levlow,levweight, &
             pratio,utime, tmpname,ierr)
         umag=sqrt(u1**2+v1**2)
!c        the wave amplitude becomes large as the wind speed becomes small
!c        and the wave should saturate
         if (umag.lt.usat) umag=usat
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
      call getsi_31(lonw,lat,tmpheight,utime, &
                levhi,levlow,levweight,pratio,ierr)
      call var3d_31(a,lonw,lat,levhi,levlow,levweight,pratio, &
                utime,tmpname,ierr)
      call getsi_31(lonw,lat,tmpheight+dz,utime, &
                levhi,levlow,levweight,pratio,ierr)
      call var3d_31(aprime,lonw,lat,levhi,levlow,levweight,pratio, &
                utime,tmpname,ierr)
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
    end subroutine grwpb_31

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadvar_31(nf,type,ier)

!c     load arrays corresponding to the variable type
      USE NETCDF_F90
      implicit none

!c     NETCDF channel number
      integer nf
!c     variable type to load :
!c       mean = values of ps,tsurf,t,rho,u,v from mean field file (nf=unetm)
!c              also integrated fluxes at surface and top of atmosphere
!c       sigm = sigma values of mean field file (nf=unetm)
!c       orog = orography values of mountain file (nf=unet)
!c       eofs = normu,normu,normt,normp,normr,u,v,t,rho,ps,pcsmth,pcvar
!c                    of eof field file (nf=ueof) 
!c       grwp = substd values of mountain file (nf=unet)
!c       stdv = sdps,sdtsurf,sdt,sdrho,sdu and sdv values of
!c                     standard deviation field file (nf=unetsd)
      character(len=4) type
      integer ier   !error flag (0=OK, 1=NOK)
      integer ierr
      integer varid

!#include "constants_atmemcd_31.h"

      character(len=16) name

      if (type.eq.'mean') then

         name='ps'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabps)
         if (ierr.ne.nf_noerr) goto 9999

         name='tsurf'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabtsurf)
         if (ierr.ne.nf_noerr) goto 9999

         name='fluxsurf_lw'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabfslw)
         if (ierr.ne.nf_noerr) goto 9999

         name='fluxsurf_sw'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabfssw)
         if (ierr.ne.nf_noerr) goto 9999

         name='fluxtop_lw'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabftlw)
         if (ierr.ne.nf_noerr) goto 9999

         name='fluxtop_sw'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabftsw)
         if (ierr.ne.nf_noerr) goto 9999

         name='temp' 
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabt)
         if (ierr.ne.nf_noerr) goto 9999

         name='rho'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabrho)
         if (ierr.ne.nf_noerr) goto 9999

         name='u'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabu)
         if (ierr.ne.nf_noerr) goto 9999

         name='v'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabv)
         if (ierr.ne.nf_noerr) goto 9999

      elseif (type.eq.'sigm') then

         name='sig_s'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,sigma)
         if (ierr.ne.nf_noerr) goto 9999

!c        sigma Thermosphere : 
         sigma(dimlevs+1) = sigma(dimlevs)*exp(-20./7.)  

      elseif (type.eq.'orog') then

         name='orography'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,taborog)
         if (ierr.ne.nf_noerr) goto 9999

      elseif (type.eq.'eofs') then

         name='normu'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeonormu)
         if (ierr.ne.nf_noerr) goto 9999

         name='normv'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeonormv)
         if (ierr.ne.nf_noerr) goto 9999

         name='normt'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeonormt)
         if (ierr.ne.nf_noerr) goto 9999

         name='normp'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeonormp)
         if (ierr.ne.nf_noerr) goto 9999

         name='normr'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeonormr)
         if (ierr.ne.nf_noerr) goto 9999

         name='u'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeou)
         if (ierr.ne.nf_noerr) goto 9999

         name='v'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeov)
         if (ierr.ne.nf_noerr) goto 9999

         name='temp' ! Au lieu de 't' (Ferret)
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeot)
         if (ierr.ne.nf_noerr) goto 9999

         name='rho'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeorho)
         if (ierr.ne.nf_noerr) goto 9999

         name='ps'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabeops)
         if (ierr.ne.nf_noerr) goto 9999

         name='pcsmth'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabpcsmth)
         if (ierr.ne.nf_noerr) goto 9999

         name='pcvar'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabpcvar)
         if (ierr.ne.nf_noerr) goto 9999

      elseif (type.eq.'grwp') then

         name='substd'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsubstd)
         if (ierr.ne.nf_noerr) goto 9999

      elseif (type.eq.'stdv') then

         name='sdps'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdps)
         if (ierr.ne.nf_noerr) goto 9999

         name='sdtsurf'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdtsurf)
         if (ierr.ne.nf_noerr) goto 9999

         name='sdtemp'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdt)
         if (ierr.ne.nf_noerr) goto 9999

         name='sdrho'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdrho)
         if (ierr.ne.nf_noerr) goto 9999

         name='sdu'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdu)
         if (ierr.ne.nf_noerr) goto 9999

         name='sdv'
         ierr = NF_INQ_VARID(nf,name,varid) 
         if (ierr.ne.nf_noerr) goto 9999
         ierr = NF_GET_VAR_REAL(nf,varid,tabsdv)
         if (ierr.ne.nf_noerr) goto 9999

      endif

      return

!c     Error handling
 9999 ier=1
      write(0,*)"LOADVAR Error : impossible to load ", &
          name," from ",type, "value file"
      return

    end subroutine loadvar_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ltime_31(longitude,sunlon,localtime)
!c
!c     Compute local time at longitude=longitude when the sun is
!c     at longitude=sunlon.
!c
      implicit none

!c     input
!c
      real longitude !west longitude
      real sunlon    !west longitude of sun
!c
!c     output
!c
      real localtime !local time
!c
      localtime=12.+(sunlon-longitude)/15.
      if (localtime.lt.0.) localtime=localtime+24.
      if (localtime.ge.24.) localtime=localtime-24.
!c
      return
    end subroutine mars_ltime_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ptime_31(lonw,lt,ut)
!c
!c     Convert local time, lt (0..24) at west longitude=lonw to 
!c     universal time  ut (0..24).

      implicit none
!c
!c     input
!c
      real lonw     !longitude west
      real lt       !local time
!c
!c     output
!c
      real ut       !universal time  ( =local time at lon=0)
!c
      ut=mod((lonw/15.+lt+24.),24.)
      return
    end subroutine mars_ptime_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mcd_time_31(ut,itime,w)
!c
!c     From universal time ut (0..24 hrs) , find the 
!c     2 nearest timestep in the database (1 - 12) 
!c     and give the linear interpolation weight of itime(1)
!c 
!c     Note that it is midnight universal time (and thus at longitude=0.0)
!c     at itime = 1

      implicit none
!c
!c     input
!c
      real ut       !universal time (0. to 24. hrs) =local time at lon=0
!c
!c     output
!c
      integer  itime(2)     ! 2 nearest timestep in database 
      real  w            ! linear interpolation weight of itime(1)
!c
      itime(1) = int(ut/2.)+1
      itime(2) = itime(1) +1
      if (itime(2).ge.13.) itime(2) = itime(2) -12

      w = 1 - ut/2. + int(ut/2.)
      
      return
    end subroutine mcd_time_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine opend_31(unet,unetm,unetsd,ueof,num,dust,dset,ier)
!c
!c     Open the appropriate NETCDF file corresponding to Ls, dust scenario  
!c     and data set.
!c
      USE NETCDF_F90
      implicit none


!c
!c     Input
!c
      integer unet     !NETCDF channel number for mountain fields
      integer unetm    !NETCDF channel number for mean fields
      integer unetsd   !NETCDF channel number for standard deviation fields
      integer ueof     !NETCDF channel number for EOF fields
      integer num      !season num
      integer dust     !Dust senario
      character(len=*) dset   !Dataset
!c     Output
      integer ier      !Error flag (0=OK, 1=NOK)
!c
!c     Local
!c
      integer ierr
      character(len=255) datfile
      character(len=3)   scen
      character(len=3)   saison
      character(len=2)   type1

      ier=0

!c
!c     Close files
!c
      ierr=NF_CLOSE(unetm)
      ierr=NF_CLOSE(unetsd)
      ierr=NF_CLOSE(ueof)
      ierr=NF_CLOSE(unet)

!c     dust scenario
      if (dust.eq.1) then
         scen='mgs'
      elseif (dust.eq.2) then
         scen='vik'
      elseif (dust.eq.3) then
         scen='low'
      elseif (dust.eq.4) then
         scen='ds2'
      elseif (dust.eq.5) then
         scen='ds5'
      endif

!c     season number
      write(saison,'(i2.2,1x)') num 
      saison = "s"//saison(1:2)
!c
!c     Mean file
      type1='me'
      datfile=dset//scen//saison//type1//'.nc'
!c     write (*,*) "Ouverture de  ",datfile
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm)
      if (ierr.ne.NF_NOERR) goto 9999
!c
!c     Standard Deviation file
      type1='sd'
      datfile=dset//scen//saison//type1//'.nc'
!c     write (*,*) "Ouverture de  ",datfile
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd)
      if (ierr.ne.NF_NOERR) goto 9999
!c
!c     eof file
      datfile=dset//scen//'alleo.nc'
!c     write (*,*) "Ouverture de  ",datfile
      ierr=NF_OPEN(datfile,NF_NOWRITE,ueof)
      if (ierr.ne.NF_NOERR) goto 9999
!c
!c     mountain file
!c
      datfile=dset//'mountain.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unet)
      if (ierr.ne.NF_NOERR) goto 9999

      return

!c     Error handling
 9999 ier=1
      write(*,'(a)')"OPEND Error : impossible to open "//trim(datfile)
      return

    end subroutine opend_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine orbit_31(date,sunlat,sunlon,ls,marsau,outmodelday)
!c
!c     Given Julian date solves the equation of time to give
!c     solar longitude and latitude, Ls and distance of Mars
!c     from the sun in AU and model day

      implicit none
!c
!c     inputs
!c
      real(kind=PM_REEL) date   !Julian date
!c
!c     outputs
!c
      real   sunlat !subsolar latitude
      real   sunlon !subsolar longitude
      real   ls     !Ls
      real   marsau !Sun-Mars distance in AU
      real   outmodelday
!c
!c     local
!c
      real(kind=PM_REEL) modelday
      real(kind=PM_REEL) marsday
      parameter (marsday=88775.245d0)
      real(kind=PM_REEL) earthday
      parameter (earthday=86400.d0)
      real(kind=PM_REEL) mdoffset
      parameter (mdoffset=222.4d0)
      real   slonoff
      parameter (slonoff=197.2)
      real   inclin
      parameter (inclin=25.3)
      real   pi
      parameter (pi=3.14159)
      real   dummy
!c
!c     convert Julian day to model day
!c
      modelday=date*earthday/marsday
      modelday=mod(modelday+mdoffset,669.d0)
      dummy=real(modelday,kind=4)
      outmodelday=real(modelday,kind=4)
      call sol2ls_31(dummy,ls)
      sunlon=360.*(dummy-real(int(dummy)))+slonoff
      sunlon=mod(sunlon,360.)
      sunlat=inclin*sin(ls*pi/180.)
      marsau=1.5237*7.73396*((1.-0.934**2)) &
            /(1.+0.0934*cos((ls+109.)*pi/180.))
      return
    end subroutine orbit_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine profi_31(a,lon,lat,utime,name,ier)
!c
!c     Retrieve a profile of 3-d variable=name at east longitude=lon, latitude=lat
!c     and universal time=utime.
!c
      implicit none

!#include "constants_atmemcd_31.h"
!c
!c     input
!c
      real          lon    !east longitude of point
      real          lat    !latitude of point
      real          utime  !Universal time (0. to 24. hrs) =local time at lon=0
      character(len=16)  name   !Name of variable
!c
!c     output
!c
      integer ier            !error flag (0=OK, 1=NOK)
      real    a(dimlevs +1)  !Profile (dimlevs mcd point + 1 thermosphere point)
!c
!c     local variables
!c
      integer i,j,l
      integer iut, itime(2)
      integer dlon(4)
      integer dlat(4)
      real    y(dimlevs,4), x(dimlevs,2)
      real    t,u,w

      ier = 0
!c
!c     find nearest 4 grid points
!c
      call grid4_31(lon,lat,dlon,dlat,t,u)

!c     find nearest 2 timestep :
      call mcd_time_31(utime,itime,w) 

!c     loop on the 2 nearest timestep :
      do j = 1 , 2
         iut = itime(j)

!c        get 4 profiles at those points
!c
         if (name.eq.'t') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabt(dlon(i),dlat(i),l,iut)
               enddo
            enddo
         elseif (name.eq.'rho') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabrho(dlon(i),dlat(i),l,iut)
               enddo
            enddo
         elseif (name.eq.'u') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabu(dlon(i),dlat(i),l,iut)
               enddo
            enddo
         elseif (name.eq.'v') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabv(dlon(i),dlat(i),l,iut)
               enddo
            enddo
         elseif (name.eq.'sdt') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabsdt(dlon(i),dlat(i),l,1)
               enddo
            enddo
         elseif (name.eq.'sdrho') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabsdrho(dlon(i),dlat(i),l,1)
               enddo
            enddo
         elseif (name.eq.'sdu') then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=tabsdu(dlon(i),dlat(i),l,1)
               enddo
            enddo
         elseif (name.eq.'sdv') then
           do i=1,4
              do l=1,dimlevs
                 y(l,i)=tabsdv(dlon(i),dlat(i),l,1)
               enddo
            enddo
         else
            write(0,*)'PROFI Error: ',name,' unknown name'
            ier=1
            go to 9999
         endif

!c
!c        bilinear interpolation
!c
         do l=1,dimlevs
           x(l,j)=(1.-t)*(1.-u)*y(l,1) &
               +t*(1.-u)*y(l,2) &
               +t*u*y(l,3) &
               +(1.-t)*u*y(l,4)
         enddo
      end do

!c     Linear interpolation in time :
      do l=1,dimlevs
        a(l) = w*x(l,1) + (1-w)*x(l,2)
      enddo

!c     Thermospheric point :
      if (name.eq.'t') then
         a(dimlevs+1) = T_thermo
      else if ((name.eq.'rho').or.(name.eq.'sdrho')) then
         a(dimlevs+1) = a(dimlevs) * (sigma(dimlevs+1)/sigma(dimlevs)) &
        *(tabt(dlon(1),dlat(1),dimlevs,iut) / T_thermo)
      else 
         a(dimlevs+1) = a(dimlevs)
      end if

      return

 9999 do l=1,dimlevs
        a(l)=0.
      enddo
      return
    end subroutine profi_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season_31(ls,numsaison)

!c     compute the season number from areocentric longitude

      implicit none

!c     input
      real ls
!c     output 
      integer numsaison

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
    end subroutine season_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine sol2ls_31(sol,ls)
!c
!c  Convert day number, sol, to solar longitude, Ls, where sol=0 is the
!c  spring equinox.

      implicit none

!c  Arguments:
      real sol
      real ls

!c  Local:
      real year_day,peri_day,timeperi,e_elips,pi,degrad
      data year_day /668.6/
      data peri_day /485.0/
      data timeperi /1.905637/
      data e_elips  /0.093358/
      data pi       /3.1415927/
      data degrad   /57.295779/
      real zanom,xref,zx0,zdx,zteta,zz
      integer iter

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
    end subroutine sol2ls_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real function ls2sol_31(ls)
!c
!c  Convert solar longitude, Ls, to day number, sol, where sol=0 is the
!c  spring equinox.

      implicit none

!c  Arguments:
      real :: ls

!c  Local:
      real :: xref,zx0,zteta,zz

      real :: year_day=668.6
      real :: peri_day=485.0
      real :: timeperi=1.905637
      real :: e_elips=0.093358
      real :: pi=3.1415927
      real :: degrad=57.295779

      if (abs(ls).lt.1.0e-5) then
         if (ls.ge.0.0) then
            ls2sol_31 = 0.0
         else
            ls2sol_31 = year_day
         end if
         return
      end if

      zteta = ls/degrad + timeperi
      zx0 = 2.0*atan(tan(0.5*zteta)/sqrt((1.+e_elips)/(1.-e_elips)))
      xref = zx0-e_elips*sin(zx0)
      zz = xref/(2.*pi)
      ls2sol_31 = zz*year_day + peri_day
      if (ls2sol_31.lt.0.0) ls2sol_31 = ls2sol_31 + year_day
      if (ls2sol_31.ge.year_day) ls2sol_31 = ls2sol_31 - year_day

      return
    end function ls2sol_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var2d_31(a,lonw,lat,utime,name,ier)
!c
!c     Retrieve the value of 2-d variable=name at longitude=lon, latitude=lat 
!c     and universal time=utime. Use bilinear interpolation to get user
!c     specified longitude and latitude and time.
!c
      implicit none
      
!#include "constants_atmemcd_31.h"
!c
!c     input
!c
      real          lonw   !longitude (west) of point
      real          lat    !latitude of point
      real          utime  !Universal time (0. to 24. hrs) =local time at lon=0 
      character(len=16)  name   !Name of variable
!c
!c     output
!c
      integer ier          !error flag (0=OK, 1=NOK)
      real    a            !value of variable
!c
!c     local variables
!c
      integer i, j
      integer iut,itime(2)
      integer dlon(4)
      integer dlat(4)
      real    y(4) , x(2)
      real    t,u,w
      real    lon          !longitude east

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
      call grid4_31(lon,lat,dlon,dlat,t,u)

!c     find nearest 2 timestep :
      call mcd_time_31(utime,itime,w)

!c     loop on the 2 nearest timestep :
      do j = 1 , 2
         iut = itime(j)

!c        retrieve the four values at the nearest grid points from the array
         if (name.eq.'orography') then
            do i=1,4
               y(i)=taborog(dlon(i),dlat(i),1)
            enddo
         elseif (name.eq.'ps') then
            do i=1,4
               y(i)=tabps(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'tsurf') then
            do i=1,4
               y(i)=tabtsurf(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'fluxsurf_lw') then
            do i=1,4
               y(i)=tabfslw(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'fluxsurf_sw') then
            do i=1,4
               y(i)=tabfssw(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'fluxtop_lw') then
            do i=1,4
               y(i)=tabftlw(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'fluxtop_sw') then
            do i=1,4
               y(i)=tabftsw(dlon(i),dlat(i),iut)
            enddo
         elseif (name.eq.'sdps') then
            do i=1,4
               y(i)=tabsdps(dlon(i),dlat(i),1)
            enddo
         elseif (name.eq.'substd') then
            do i=1,4
               y(i)=tabsubstd(dlon(i),dlat(i),1)
            enddo
         elseif (name.eq.'sdtsurf') then
            do i=1,4
               y(i)=tabsdtsurf(dlon(i),dlat(i),1)
            enddo      
         else
            ier=1
            write(0,*)'VAR2D Error: ',name &
             ,' unknown variable name'
            goto 9999
         endif
                 
!c
!c        bilinear interpolation in space
!c
         x(j)=(1.-t)*(1.-u)*y(1) &
         +t*(1.-u)*y(2) &
         +t*u*y(3) &
         +(1.-t)*u*y(4)
      end do
!c     Linear interpolation in time :
      a = w*x(1) + (1-w)*x(2)

      return

!c     error handling
 9999 a=0.
      return

    end subroutine var2d_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine var3d_31(a,lonw,lat,levhi,levlow,levweight,pratio, &
                      utime,name,ier)
!c
!c     Retrieve the value of 3-d variable=name at longitude=lon, latitude=lat
!c     and Universal time=utime. The height of the variable is controlled by
!c     levhi, levlow and levweight.  Use bilinear interpolation to get user
!c     specified longitude and latitude.
!c
      implicit none

!#include "constants_atmemcd_31.h"
!c
!c     input
!c
      real          lonw      !longitude (west) of point
      real          lat       !latitude of point
      integer       levhi     !database level upper bound
      integer       levlow    !database level lower bound
      real          levweight(2) !level weight for interpolation
!c     (1) for linear in height (2) for linear in pressure
      real          pratio    !ratio of pressure to extreme value if out of range

      real    utime   !Universal time (0. to 24. hrs) =local time at lon=0

      character(len=16)  name      !Name of variable
!c
!c     output
!c
      integer ier             !error flag (0=OK, 1=NOK)
      real    a               !value of variable
!c
!c     local variables
!c
      integer ierr
      real    profile(dimlevs+1) !profile in MCD grid + 1 "thermospheric" point
      real    lon

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
      call profi_31(profile,lon,lat,utime,name,ierr)
      if (ierr.ne.0) then 
         write(0,*)'VAR3D Error : profile not available for variable' &
             ,name
         ier=1
         goto 9999
      endif

      if (name.eq.'rho'.or.name.eq.'sdrho') then
!c interpolate densities linearly in pressure
         a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(2)
!c correction for densities which are out of sigma range
         a=pratio*a
      else ! most variables are interpolated linearly in height
         a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
      endif
!c
      return

!c     Error handling
 9999 a=0.
      return

    end subroutine var3d_31
      
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine max2d_31(a,lon,lat,name,ier)
!c
!c     Retrieve the daily maximum value of 2-d variable=name at 
!c     longitude=lon, latitude=lat
!c
      implicit none
!c
!c     input
!c
      real          lon    !west longitude of point
      real          lat    !latitude of point
      character(len=16)  name   !Name of variable
!c
!c     output
!c
      integer ier          !Error flag (0=OK, 1=NOK)
      real a               !maximum of variable
!c
!c     local variables
!c
      integer ierr,i
      real    x
      real    xmax
      real    utime
!c
      ier=0
      xmax=-1.e20
      do i=0,22,2
        utime=real(i)
        call var2d_31(x,lon,lat,utime,name,ierr)
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

    end subroutine max2d_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine min2d_31(a,lon,lat,name,ier)
!c
!c     Retrieve the daily minimum value of 2-d variable=name at 
!c     longitude=lon, latitude=lat
!c
      implicit none
!c
!c     input
!c
      real          lon    !longitude of point
      real          lat    !latitude of point
      character(len=16)  name   !Name of variable
!c
!c     output
!c
      integer ier          !Error flag (0=OK, 1=NOK)
      real a               !minimum of variable
!c
!c     local variables
!c
      integer ierr,i
      real    x
      real    xmin
      real    utime

      ier=0
      xmin=1.e20
      do i=0,22,2
        utime=real(i)
        call var2d_31(x,lon,lat,utime,name,ierr)
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

    end subroutine min2d_31

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            FUNCTION ran1_31(idum)
!c     "Minimal" random number generator of Park and Miller with Bays-Durham
!c     shuffle and added safeguards 
!c     Reference : Numerical Recipes in Fortran 77 : the art of scientific 
!c     computing - 2d edition (1992)- p 271
!c     Return a uniform random deviate between 0.0 and 1.0 (eclusive of the
!c     endpoints values). Call witj idum a negative integer to initialize;
!c     thereafter, do not alter idum between successive deviates in a sequence.
!c     RNMX should approximate the largest floating value that is less than 1.

      implicit none

      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1_31,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
                 NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/

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
      ran1_31=min(AM*iy,RNMX)
      return
    END FUNCTION ran1_31

!!! Modele EMCD 3.1, fonction height_31

      subroutine cps_height_31(xlat,xlon,zradius,zareoid,zsurface,iconv,dset,ier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!     cps_height_31
!
!$Resume
!     Conversion entre les différentes types de rayons martiens (rayon depuis le
!     centre de la planète , la hauteur au dessus de l'altitude zéro de aréoïde
!     et la hauteur au dessus de la surface locale).
!
!$Acces
!  PUBLIC
!
!$Description
!     Conversion entre la rayon depuis le centre de la planète ,
!     la hauteur au dessus de l'altitude zéro de aréoïde et la hauteur au dessus
!     de la surface locale.
!     Etant donné un des trois éléments ci-dessus, cette routine calcule les
!     deux autres suivant la valeur de "iconv".
!
!     A noter : l'aréoïde de référence "MOLA" n'est pas le même que
!     l'aréoïde de référence "DTM".
!
!
!     Remarque : cette routine n'est pas prévue pour un calcul précis à
!     petite echelle. L'aréoïde et les données topographiques ont été
!     re-échantillonnées et lissées à la résolution de la base de données,
!     qui est interpolée par cette routine dans le but de produire un jeu
!     de données consistant avec les données "GCM".
!
!     On peut toutefois utiliser cette routine pour des haure résolution 
!     en lui fournissant un autre fichier de données et en modifiant 
!     les noms des fichiers et les parametres de définition de la grille.
!
!
!     Cette routine est écrite  pour être utilisée avec le modèle ATMEMCD
!     mais est utilisable isolément et ne partage pas de données (common)
!     avec le modèle. Elle peut donc être utilisée indépendemment.
!
!$Version
!
!     1.0 26/01/01 released with MCD v3.0
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Auteur
!
!     SRL
!
!$Usage
!     call cps_height_31(xlat,xlon,zradius,zareoid,zsurface,iconv,dset)
!
!$Arguments
!>E xlat       : <PM_REEL> Latitude aréocentrique (rad)
!>E xlon       : <PM_REEL> Longitude Est aréocentrique (rad)
!>E/S zradius  : <PM_REEL> Rayon depuis le centre de la planète (m)
!>E/S zareoid  : <PM_REEL> Hauteur au dessus du zéro de l'aréoïde (m)
!>E/S zsurface : <PM_REEL> Hauteur au dessus de la surface locale (m)
!>E iconv    : <integer> indique laquelle des 3 variables "z" est l'entrée
! permettant de trouver les deux autres
!.                      iconv=1  zradius  -> zareoid, zsurface
!.                      iconv=2  zareoid  -> zradius, zsurface
!.                      iconv=3  zsurface -> zradius, zareoid
!>E dset     : <character(len=*)> Jeu de données (premier appel seulement)
!.        Un ou plusieurs blancs pour avoir les données par défaut (répertoire
!         donné par la base de données COMPAS)
!.     ou Répertoire des données EMCD avec slash final (e.g. '/dir/path/'),
!.     ou un lien sur le répertoire des données EMCD  (e.g. 'EMCD_DATA/').
!>S ier      : <integer> Code retour 
!.                      0 = OK
!.                      1 = impossible d'ouvrir le fichier de base de données
!.                      2 = impossible de charger les données du fichier
!.                      3 = iconv invalide
!
!$<>
!***********************************************************************
!
!$Name
!     cps_height_31
!
!$Resume
!     Conversion between radius from the centre of the planet,
!     height above the zero datum areoid and height above the
!     local surface
!
!$Description
!     Conversion between radius from the centre of the planet,
!     height above the zero datum areoid and height above the
!     local surface.  Given any one of the above, this routine
!     finds the other two, according to the value of "iconv"
!     Note that the MOLA reference areoid is not the same as
!     the DTM reference.
!
!     Note that this is not intended to provide an accurate,
!     small-scale map.  The areoid and topography data has
!     already been sampled and smoothed at the database
!     resolution, and is interpolated by this routine in the
!     same way, in order to provide a dataset which is consistent
!     with the GCM data.  This same routine may be used for high
!     resoltuion data by supplying an alternate data file and
!     changing data file names and grid definition parameters.
!
!     This routine is written for use with ATMEMCD, but is
!     standalone and does not share any data so it can be
!     called independently.
!
!$Version
!
!     1.0 26/01/01 released with MCD v3.0
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Author
!
!     SRL
!
!$Use
!     call cps_height_31(xlat,xlon,zradius,zareoid,zsurface,iconv,dset)
!
!$Arguments
!>E xlat     : <double precision> areocentric latitude (rad)
!>E xlon     : <double precision> areocentric longitude east (rad)
!>E zradius  : <double precision> radius from centre of planet (m)
!>E zareoid  : <double precision> height above zero datum (m)
!>E zsurface : <double precision> height above local surface (m)
!>E iconv    : <integer>          switch to choose which z variable
!>E                               is used to find other two
!>E                      iconv=1  zradius  -> zareoid, zsurface
!>E                      iconv=2  zareoid  -> zradius, zsurface
!>E                      iconv=3  zsurface -> zradius, zareoid
!>E dset     : <character(len=*)>    data set (only used on first call)
!>E                      One or more blanks to get the default,
!>E                      or full directory path of MCD data required
!>E                      including trailing slash (e.g. '/dir/path/'),
!>E                      or a link to a directory (e.g. 'EMCD_DATA/').
!>E                      Default is link EMCD_DATA in working directory.
!>S ier      : <integer> error flag
!>S                      0 = OK
!>S                      1 = can't open dataset
!>S                      2 = can't read data
!>S                      3 = invalid iconv
!

      USE NETCDF_F90
      implicit none

      real(kind=PM_REEL) xlat, xlon, zradius, zareoid, zsurface
      integer iconv, ier
      character(len=*) dset

!c     data file names
      character(len=12) datname
      parameter(datname='mountain.nc')

!c     define grid
      integer dimlon,dimlat
      parameter(dimlon=72,dimlat=36)
      real lonmin,lonmax,latmin,latmax
      parameter(lonmin=-180.0,lonmax=175.0)
      parameter(latmin=-87.5,latmax=87.5)
      real deltal
      parameter(deltal=5.0)

      integer unet              ! NETCDF channel number
      real pi 
      parameter(pi=3.14159265359)
      real crd
      parameter(crd=180./pi)

      real lat, lon, alat, alon
      real xorog, xareoid
      character(len=255) dataset, datfile
      integer lendataset
      integer ierr, ierr1, ierr2
      integer i

      integer dlon(4)           ! longitude index of nearest points
      integer dlat(4)           ! latitude index of nearest points
      real t                    ! weight
      real u                    ! weight

      real orog(dimlon,dimlat)
      real areoid(dimlon,dimlat)
      save orog, areoid
      logical firstcall 
      data firstcall /.true./
      save firstcall

      character(len=16) name
      integer varid

      ! Initialisations
      ier=0
      do i=1,4
         dlon(i) = 0
         dlat(i) = 0
      enddo
      t = 0
      u = 0

!c     check value of iconv
      if (iconv.lt.1.or.iconv.gt.3) then
         ier=3
         goto 999
      endif

!c     read in the data on the first call to the routine only
      if (firstcall) then
!c     data set name
         do lendataset = len(dset), 1, -1
            if (dset(lendataset:lendataset).ne.' ') go to 100
         end do
  100    continue
         if (lendataset.eq.0) then !default data set case
!       acces au repertoire contenant les fichiers du modele par COMPAS
            ier = cps_getFichierModele("atmosphere", "EMCD3_1", dataset, &
                 rep=.true.)
            if (MSP_gen_messages("cps_atmemcd_31")) return
            dataset = trim(dataset) // "/"
! Valeur attendue un lien sur :
!         dataset = '/usr/local_ms/data/data_emcd_3.1/'
         else  !symbolic link or full path given explicitly
            dataset = dset
         end if
         lendataset = len_trim(dataset)
         if (dataset(lendataset:lendataset).ne."/") then
            dataset = trim(dataset) // "/"
            lendataset=lendataset+1
         endif

!c     open NETCDF file
         datfile=dataset(1:lendataset)//datname
         ierr=NF_OPEN(datfile,NF_NOWRITE,unet)
         if (ierr.ne.0) then
            ier=1
            goto 999
         endif

         name='orography'
         ierr1 = NF_INQ_VARID(unet,name,varid)
         ierr2 = NF_GET_VAR_REAL(unet,varid,orog)
         if ((ierr1.ne.nf_noerr).or.(ierr2.ne.nf_noerr)) then
            ier=2
            goto 999
         endif

         name='areoid'
         ierr1 = NF_INQ_VARID(unet,name,varid)
         ierr2 = NF_GET_VAR_REAL(unet,varid,areoid)
         if ((ierr1.ne.nf_noerr).or.(ierr2.ne.nf_noerr)) then
            ier=2
            goto 999
         endif
 
         ierr=NF_CLOSE(unet)
         firstcall=.false.
      end if

!c     convert longitude east (rad) to longitude east (deg) [-180..180]
      lon=real(xlon)*crd
      lon=mod(lon,360.)
      if(lon.gt.180.) lon=lon-360.
!c     convert latitude from radians to degrees
      lat=real(xlat)*crd
!c     find longitude and latitude of four nearest points (as in grid4)
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
!c     bilinear interpolation to find local orography and areoid
      xorog=(1.-t)*(1.-u)*orog(dlon(1),dlat(1)) &
      +t*(1.-u)*orog(dlon(2),dlat(2)) &
      +t*u*orog(dlon(3),dlat(3)) &
      +(1.-t)*u*orog(dlon(4),dlat(4))
      xareoid=(1.-t)*(1.-u)*areoid(dlon(1),dlat(1)) &
      +t*(1.-u)*areoid(dlon(2),dlat(2)) &
      +t*u*areoid(dlon(3),dlat(3)) &
      +(1.-t)*u*areoid(dlon(4),dlat(4))

!c     do the conversions according to iconv
      if (iconv.eq.1) then
         zareoid = zradius - xareoid
         zsurface = zareoid - xorog
      else if (iconv.eq.2) then
         zradius = zareoid + xareoid
         zsurface = zareoid - xorog
      else ! iconv.eq.3
         zareoid = zsurface + xorog
         zradius = zareoid + xareoid
      end if

      return

!c     handle errors
  999 zradius=-999.0
      zareoid=-999.0
      zsurface=-999.0
      return
    end subroutine cps_height_31


    subroutine cps_atmemcd_31_close ()
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine                                                       *
!$Nom                                                                  *
!     cps_atmemcd_31_close                                             *
!                                                                      *
!$Resume                                                               *
!      routine de desallocation memoire                                *
!$Auteur                                                               *
!      Florence VIVARES (ATOS Origin)                                  *
!$Usage                                                                *
!     call cps_atmemcd_31_close()                                      *
!                                                                      *
!$<>                                                                   *
!***********************************************************************

! Variable pour la gestion d'erreur de deallocate
      integer :: ierralloc


      if (modele_init) then
!
! desallocation des variables globales :
!
         if (associated(taborog)) deallocate(taborog, stat=ierralloc)
         if (associated(tabsubstd)) deallocate(tabsubstd, stat=ierralloc)
         if (associated(tabps))   deallocate(tabps, stat=ierralloc)
         if (associated(tabtsurf)) deallocate(tabtsurf, stat=ierralloc)
         if (associated(tabfslw)) deallocate(tabfslw, stat=ierralloc)
         if (associated(tabfssw)) deallocate(tabfssw, stat=ierralloc)
         if (associated(tabftlw)) deallocate(tabftlw, stat=ierralloc)
         if (associated(tabftsw)) deallocate(tabftsw, stat=ierralloc)
         if (associated(tabt))    deallocate(tabt, stat=ierralloc)
         if (associated(tabrho))  deallocate(tabrho, stat=ierralloc)
         if (associated(tabu))    deallocate(tabu, stat=ierralloc)
         if (associated(tabv))    deallocate(tabv, stat=ierralloc)
         if (associated(tabsdps)) deallocate(tabsdps, stat=ierralloc)
         if (associated(tabsdtsurf)) deallocate(tabsdtsurf, stat=ierralloc)
         if (associated(tabsdt))  deallocate(tabsdt, stat=ierralloc)
         if (associated(tabsdrho)) deallocate(tabsdrho, stat=ierralloc)
         if (associated(tabsdu))  deallocate(tabsdu, stat=ierralloc)
         if (associated(tabsdv))  deallocate(tabsdv, stat=ierralloc)
         if (associated(tabpcsmth)) deallocate(tabpcsmth, stat=ierralloc)
         if (associated(tabeops)) deallocate(tabeops, stat=ierralloc)
         if (associated(tabpcvar)) deallocate(tabpcvar, stat=ierralloc)
         if (associated(tabeot))  deallocate(tabeot, stat=ierralloc)
         if (associated(tabeorho)) deallocate(tabeorho, stat=ierralloc)
         if (associated(tabeou))  deallocate(tabeou, stat=ierralloc)
         if (associated(tabeov))  deallocate(tabeov, stat=ierralloc)

         modele_init = .false.
      endif


    end subroutine cps_atmemcd_31_close

end module cps_modele_emcd31
