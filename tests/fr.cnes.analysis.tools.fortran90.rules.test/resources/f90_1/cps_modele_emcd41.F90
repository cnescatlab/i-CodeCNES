module cps_modele_emcd41
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_modele_emcd41
!
!$Resume
!  Modèles d'Atmosphère martien EMCD 4.1
!
!$Description
! Ce module contient la version 4.1 du modèle EMCD (European Martian
! Climate Database) :
! - routine atmemcd_41
! 
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_modele_emcd41.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: cps_modele_emcd41.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.15  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.14  2010/03/02 10:30:55  mercadig
!  VERSION:2.8:DM-ID:1350:02/03/2010: Correction log cvs
!
!  Revision 1.13  2010/02/15 15:13:45  vivaresf
!  VERSION::DM-ID 1350 : conversions explicites des réels en double pour le portage Linux (et des entiers en réels ou double)
!
!  Revision 1.12  2008/10/16 12:48:23  cml
!  DM-ID 1058 : Ajout d initialisations
!
!  Revision 1.11  2008/07/04 11:57:04  huec
!  DM-ID 1058 : Initialisation de variables
!
!  Revision 1.10  2008/04/22 16:46:09  vivaresf
!  COMPAS V2.4, AQ : rajout de la gestion des répertoires dans cps_height
!
!  Revision 1.9  2008/04/11 17:44:09  vivaresf
!  FA-ID 1009 : intégration PSIMU,
!  gestion des / en fin de répertoire
!
!  Revision 1.8  2008/04/07 09:17:16  vivaresf
!  FA-ID 1009 : suppression des use cps_acces
!  correction des cartouches
!  vérification des accès par le biais de la base COMPAS aux modèles
!
!  Revision 1.7  2008/02/26 14:28:57  vivaresf
!  FA-ID 939 : écriture des messages d'erreur sur le stderr au lieu du stdout
!
!  Revision 1.6  2007/11/21 11:49:49  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.5  2006/11/17 07:01:55  vivaresf
!  DM-ID 425 : code plus portage (float en real, alog en log, dsqrt en sqrt)
!
!  Revision 1.4  2006/11/14 17:57:30  vivaresf
!  DM-ID 425 : formattage des sorties
!
!  Revision 1.3  2006/11/13 11:55:11  vivaresf
!  DM-ID 425 : utilisation des routines intrinseques mod, sqrt, real au lieu de dmod, dsqrt ou dfloat
!
!  initialisation de gset dans gasdev
!
!  Revision 1.2  2006/03/03 15:21:25  vivaresf
!  DM-ID 493 : variables en common passees en tableaux dynamique
!
!  rajout des fonctions de desallocation (cps_atmemcd_*_close)
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.2  2005/10/17 17:10:57  vivaresf
!  DM-ID 388 modèle documenté
!
!  Revision 1.1  2005/10/05 09:27:35  bouchacp
!  DM-ID 386 : modèles d'atmosphere EMCD 4.1
!
!
!$FinHistorique
!
!$Usage
!  use cps_modele_emcd41
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
!***********************************************************************
!  déclaration des interfaces externes à ce module                     *
!***********************************************************************
!
  use cps_utilisateur
  use netcdf_f90
!   
!***********************************************************************
!***********************************************************************
!  déclaration des interfaces disponibles de ce module                 *
!***********************************************************************
!***********************************************************************
!

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_modele_emcd41.F90 355 2013-02-14 12:16:41Z aadt $'

  interface cps_atmemcd_41
!
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     sous programme FORTRAN 90                                        *
!$Nom                                                                  *
!     ATMEMCD41                                                        *
!$Resume                                                               *
!     Calcul de variables meteorologiques à partir d'une base de       *
!.    donnees climatique marsienne                                     *
!$Acces                                                                *
!     Publique                                                         *
!$Version                                                              *
!                                                                      *
!$Auteur                                                               *
!     v4.0 Final version with new variables, bug fixed, EOF            *
!     v4.0 combining low and up atmosphere, variables added            *
!          min, mean and max solar scenario for thermosphere           *
!          choice of vertical coordinates, flag zkey, zareoid,         *
!          zsurface,zradius,zpressure in input output                  *
!          altitude in meters everywhere                               *
!     v3.2 Variables saved in subroutines,seasonal interpolation added *
!          Gravity allowed to vary with height in hydrostatic eqn      *
!     v3.1 Bugs fixed, new ls2sol routine and various improvements     *
!          to interpolation above model top, especially for EOFs       *
!          Major updates for v3.0 MCD                                  *
!          See Changes file for more information.                      *
!$Usage                                                                *
!     subroutine atmemcd (xz,zkey,xlat,xlon,xdate,dset,scena,typper    *
!.   &     ,invar,init_atm,seedout,ikey,pres,ro,temp,ventu,ventv       *
!.   &     ,meanvar,extvar,ier)                                        *
!                                                                      *
!$Arguments                                                            *
!>E xz    : <double pre.> hauteur / surface (m) fonction de zkey       *
!>E zkey  : <integer>     type de hauteur / surface                    *
!.                        1 = rayon depuis le centre de la planete (m) *
!.                        2 = hauteur depuis l'areoide (m) (MOLA)      *
!.                        3 = hauteur depuis la surface (m)            *
!.                        4 = niveau de pression (Pa)                  *
!                                                                      *
!>E xlat  : <double pre.> latitude (rad)                               *
!>E xlon  : <double pre.> longitude Est (rad)                          *
!>E xdate : <double pre.> date                                         *
!.          si xdate > 0 : jours juliens terrestre                     *
!.          si xdate < 0 : date marsienne =                            *
!.          -(Int(longitude solaire [deg] ) + date locale/100)         *
!.          (e.g. xdate = -90.12 correspond to Ls=90 ; LT=12           *
!.          (e.g. xdate = -173.18 correspond to Ls=173 ; LT=18:00)     *
!                                                                      *
!>E dset  : <char*(*)>    jeu de donnees                               *
!.                        Un ou plusieurs blancs pour avoir les        *
!.                        donnees par defaut ( repertoire COMPAS ) ou  *
!.                        repertoire des donnees EMCD avec slash final *
!.                        ('/dir/path/'), ou un lien sur le repertoire *
!.                        ('EMCD_DATA/').                              *
!                                                                      *
!>E scena : <integer>     scenario de poussieres                       *
!.                        1 = MY24 minimum solaire                     *
!.                        2 = MY24 moyen solaire                       *
!.                        3 = MY24 maximum solaire                     *
!.                        4 = tempete tau=3 minimum solaire            *
!.                        5 = tempete tau=3 moyen solaire              *
!.                        6 = tempete tau=3 maximum solaire            *
!.                        7 = scenario chaud - maximum solaire         *
!.                        8 = scenario froid - minimum solaire         *
!.                                                                     *
!>E typper: <double pre.> type de perturbation , dim =2                *
!.           aucune : typper(1)= 1. typper(2)=0.                       *
!.           grande echelle : typper(1)= 2. typper(2)= nb graine       *
!.           petite echelle : typper(1)= 3. typper(2)= nb graine       *
!.           gde et pte echelle : typper(1)= 4. typper(2)= nb graine   *
!.           n fois l'ecart type : typper(1)= 5. typper(2)= n          *
!                                                                      *
!>E invar : <double pre.> valeur controlant la variabilite des modeles *
!.           longueur d'onde des perturbations dues aux ondes de       *
!.           gravite (m), invar = 0 valeur par defaut (16000m)         *
!                                                                      *
!>E init_atm : <logical>  Re initialise le modele de perturbation      *
!>E ikey  : <integer>     type de sortie                               *
!.           0 = pression, densite, temperature, vent                  *
!.           1 = calcul aussi les variables statistiques dans extvar   *
!                                                                      *
!>S seedout : <double pre.> graine des variables aleatoires            *
!>S pres  : <double pre.> pression (Pa)                                *
!>S ro    : <double pre.> densite (kg/m^3)                             *
!>S temp  : <double pre.> temperature (K)                              *
!>S ventu : <double pre.> composante zonale du vent (Est-Ouest)        *
!>S ventv : <double pre.> composante meridionale du vent (Nord-Sud)    *
!>S meanvar : <double pre.> tableau des valeurs moyennes , dim = 5     *
!.           meanvar(1) = pression moyenne                             *
!.           meanvar(2) = densite moyenne                              *
!.           meanvar(3) = temperature moyenne                          *
!.           meanvar(4) = composante zonale du vent moyenne            *
!.           meanvar(5) = composante meridionale du vent moyenne       *
!                                                                      *
!>S extvar : <double pre.> tableau de 50 variables statistiques        *
!.           extvar(1) = dist. radiale depuis centre de la planete (m) *
!.           extvar(2) = altitude depuis l'aeroide, geoide marsien (m) *
!.           extvar(3) = altitude depuis la surface (m)                *
!.           extvar(4) = hauteur orographique (m)                      *
!.           extvar(5) = longitude aerocentrique de mars (deg)         *
!.           extvar(6) = heure locale solaire (hrs)                    *
!.           extvar(7) = temps universel solaire (LST a lon=0) (hrs)   *
!.           extvar(8) = capacite calorifique de l'air Cp (J kg-1 K-1) *
!.           extvar(9) = rapport calorifique specifique gamma = Cp/Cv  *
!.           extvar(10)= variation journaliere densite RMS (kg/m^3)    *
!.           extvar(11)= ecart type total saisonnier densite (kg/m^3)  *
!.           extvar(12)= perturbation moyenne de la densite (kg/m^3)   *
!.           extvar(13)= echelle de hauteur H(p) (km)                  *
!.           extvar(14)= inutilise ( = 0 )                             *
!.           extvar(15)= temperature de surface (K)                    *
!.           extvar(16)= temperature moyenne max de surface (K)        *
!.           extvar(17)= temperature moyenne min de surface (K)        *
!.           extvar(18)= variation journaliere temperature surface (K) *
!.           extvar(19)= pression moyenne de surface (Pa)              *
!.           extvar(20)= pression moyenne max de surface (Pa)          *
!.           extvar(21)= pression moyenne min de surface (Pa)          *
!.           extvar(22)= variation journaliere pression surface (Pa)   *
!.           extvar(23)= variation journaliere temperature surface (K) *
!.           extvar(24)= variation journaliere vent zonal (m/s)        *
!.           extvar(25)= variation journaliere vent meridional (m/s)   *
!.           extvar(26)= composante verticale du vent (m/s)            *
!.           extvar(27)= variation journaliere vent vertical (m/s)     *
!.           extvar(28)= pert. petite echelle onde gravite (kg/m^3)    *
!.           extvar(29)= q2 : energie cinetique turbuilence (m2/s2)    *
!.           extvar(30)= inutilise ( = 0 )                             *
!.           extvar(31)= flux thermique IR de surface (W/m2)           *
!.           extvar(32)= flux solaire de surface (W/m2)                *
!.           extvar(33)= flux thermique IR d'espace (W/m2)             *
!.           extvar(34)= flux solaire reflechit dans l'espace (W/m2)   *
!.           extvar(35)= CO2 : niveau gele en surface (kg/m2)          *
!.           extvar(36)= DOD : profondeur visible optique colonne      *
!.           extvar(37)= rapport du melange poussiere / masse (kg/kg)  *
!.           extvar(38)= DOD : variation journaliere RMS               *
!.           extvar(39)= DOD : ecart type total saisonnier de densite  *
!.           extvar(40)= colonne de vapeur (kg/m2)                     *
!.           extvar(41)= rapport melange volume eau / vapeur (mol/mol) *
!.           extvar(42)= colonne eau glacee (kg/m2)                    *
!.           extvar(43)= rapport melange eau / glace (mol/mol)         *
!.           extvar(44)= [O3]  rapport melande volumique (mol/mol)     *
!.           extvar(45)= [CO2] rapport melande volumique (mol/mol)     *
!.           extvar(46)= [O]   rapport melande volumique (mol/mol)     *
!.           extvar(47)= [N2]  rapport melande volumique (mol/mol)     *
!.           extvar(48)= [CO]  rapport melande volumique (mol/mol)     *
!.           extvar(49)= R : constante moleculaire du gaz (J K-1 kg-1) *
!.           extvar(50)= estimation viscosite de l'air (N s m-2)       *
!                                                                      *
!>S ikey  : <integer>     ier  : <integer> error flag                  *
!.          0 = OK                                                     *
!.          1 = base de donnees inconnue                               *
!.          2 = scenario inconnu                                       *
!.          3 = type perturbation inconnu                              *
!.          4 = object souterrain                                      *
!.          5 = pas de tempete de poussiere pour cette saison          *
!.          6 = impossible d'ouvrir le fichier base de donnees         *
!.          7 = impossible de charger les donnees du fichier           *
!.          8 = sigma indetermine                                      *
!.          9 = coordonnees verticale inconnue                         *
!                                                                      *
!>K solar: <character*255> 'min','mean','max': scenario solaire pour   *
!>K                                            la thermosphere         *
!$<>                                                                   *
!***********************************************************************
!
     module procedure atmemcd_41 
  end interface
!  
  interface cps_height_41
!
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!     calc_height_41                                                   *
!                                                                      *
!$Resume                                                               *
!     Calcul le rayon depuis le centre de la planete, hauteur par      *
!.    rapport au 0 de l'areoide et hauteur par rapport à la surface    *
!.    locale. A partir d'une des donnees precedentes, ce sous          *
!.    programme calcule lesw deux autres ( en fonction du parametre    *
!.    iconv ). La reference de l'areoide MOLA n'est pas la meme que    *
!.    la reference DTM.                                                *
!.    Ce sous programme est utilise par atmemcd_41                     *
!                                                                      *
!$Acces                                                                *
!     Publique                                                         *
!$Version                                                              *
!                                                                      *
!$Author                                                               *
!                                                                      *
!$Usage                                                                *
!     call calc_height_41 (xlat,xlon,zradius,zareoid,zsurface, &       *
!.                        iconv,dset)                                  *
!                                                                      *
!$Arguments                                                            *
!>E xlat     : <double precision> latitude areocentrique (rad)         *
!>E xlon     : <double precision> longitude areocentrique Est (rad)    *
!>E zradius  : <double precision> rayon / centre de la planete (m)     *
!>E zareoid  : <double precision> hauteur depuis le zero datum (m)     *
!>E zsurface : <double precision> hauteur depuis la surface locale (m) *
!>E iconv    : <integer>          switch pour choisir la variable z    *
!.                                utilise pour definir les deux autres *
!.                       iconv=1  zradius  -> zareoid, zsurface        *
!.                       iconv=2  zareoid  -> zradius, zsurface        *
!.                       iconv=3  zsurface -> zradius, zareoid         *
!>E dset     : <character*(*)>    jeu de donnees ( premier appel )     *
!.                       Un ou pluisiurs blancs : donnees par defaut   *
!.                       ( repertoire COMPAS ) ou repertoire des       *
!.                       donnees EMCD avec slash final ('/dir/path/')  *
!.                       , ou un lien sur le repertoire ('EMCD_DATA/') *
!>S ier      : <integer> code d'erreur                                 *
!.                       0 = OK                                        *
!.                       1 = impossible d'ouvrir le jeu de donnees     *
!.                       2 = impossible de lire les donnees            *
!.                       3 = parametre iconv invalide                  *
!$<>                                                                   *
!***********************************************************************
!
     module procedure  calc_height_41
  end interface
!  
  interface cps_orbit_41
!
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!     orbit                                                            *
!$Resume                                                               *
!     Pour une date julienne, resolution de l'equation du temps        *
!.    pour le calcul des longitude et latitude de mars dans le repere  *
!.    solaire et de la distance solei / mars                           *
!$Acces                                                                *
!     Publique                                                         *
!                                                                      *
!$Version                                                              *
!                                                                      *
!$Auteur                                                               *
!                                                                      *
!$Usage                                                                *
!     subroutine orbit (date,sunlat,sunlon,ls,marsau,outmodelday)      *
!                                                                      *
!$Arguments                                                            *
!>E date        : <double precision> date julienne                     *
!>S sunlat      : <double precision> latitude de mars / soleil (deg)   *
!>S sunlon      : <double precision> longitude solaire / soleil (deg)  *
!>S ls          : <double precision> longitude solaire (deg)           *
!>S marsau      : <double precision> distance soleil / mars (UA)       *
!>S outmodelday : <double precision> date marsienne                    *
!$<>                                                                   *
!***********************************************************************
!
     module procedure orbit
  end interface
! 
  interface cps_season_41
!
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!      season                                                          *
!$Resume                                                               *
!     Calcul le numero du mois depuis la longitude solaire             *
!$Acces                                                                *
!     Publique                                                         *
!                                                                      *
!$Version                                                              *
!                                                                      *
!$Auteur                                                               *
!                                                                      *
!$Usage                                                                *
!     subroutine  season (ls,numsaison)                                *
!                                                                      *
!$Arguments                                                            *
!>E ls        : <double precision> longitude solaire (deg)             *
!>S numsaison : <integer>          numero du mois                      *
!$<>                                                                   *
!***********************************************************************
!
     module procedure season
  end interface

!***********************************************************************
!  déclaration des sous programmes et fonctions de ce module           *
!***********************************************************************
  private eofpb, getsi,grid4,grwpb,loadvar,loadeof
  private mars_ltime,mars_ptime,mcd_time,opend,orbit,profi
  private season,sol2ls,var2d,interpol,extrapol3d,get_2d
  private get_3d,getsd_2d,var3d,max2d,min2d,dustmix
  private calc_height_41,getsd_3d,gasdev,ls2sol,ran1


! Donnees
!     include file defining constants and arrays used by the
!     European Martian Climate Database

! CONSTANTS
!     dimension of mean and std. dev.values  in longitude, latitude,
!     sigma levels and database universal time
      integer,private :: dimlon,dimlat,dimlevs,dimuti
      parameter(dimlon=64,dimlat=49,dimlevs=50,dimuti=12)
!     dimensions of EOF values in longitude, latitude, numbers of EOFs
      integer,private :: dimloneo,dimlateo,dimnevecs,dimeoday
      parameter(dimloneo=16,dimlateo=12,dimnevecs=72,dimeoday=669)
!     range of latitude, longitude  and time for mean
!     and std. dev. values
      real,private :: lonmin,lonmax,latmin,latmax
      parameter(lonmin=-180.,lonmax=174.375)
      parameter(latmin=-90.,latmax=90.)
!     range of latitude, longitude, number of EOFs and time for EOFs values
      real,private :: lonmineo,lonmaxeo,latmineo,latmaxeo,nevecsmin,nevecsmax
      real,private ::  daymineo,daymaxeo
      parameter(lonmineo=-180.0,lonmaxeo=157.5)
      parameter(latmineo=-82.5,latmaxeo=82.5)
      parameter(nevecsmin=1.0,nevecsmax=72.0)
      parameter(daymineo=1.0,daymaxeo=669.0)
!     step of grid in longitude for mean and std. dev. values
      real,private ::  deltalon
      parameter(deltalon=5.625)
!     step of grid in latitude for mean and std. dev. values
      real,private ::  deltalat
      parameter(deltalat=3.75)
!     step of grid in longitude and latitude for EOFs values
      real,private ::  deltalateo,deltaloneo
      parameter(deltalateo=15.0)
      parameter(deltaloneo=22.5)
!     number of 2d, low and up variables
      integer,private :: nbvar2d,nbvarlow,nbvarup
      parameter(nbvar2d=10,nbvarlow=9,nbvarup=9)
      integer,private :: nbsd2d,nbsdlow,nbsdup
      parameter(nbsd2d=3, nbsdlow=5,nbsdup=5)
      integer,private :: nbvar3d,nbcom
      parameter(nbvar3d=13,nbcom=5)
      integer,private :: nbsd3d,nbcomsd
      parameter(nbsd3d=5,nbcomsd=5)
! Layer below which the mean is done between 3 runs.
      integer,private :: low, up
      parameter(low=30,up=20)


! COMMON
!      common /niveau/ aps,bps,sigma,pseudoalt
!      common /orogra/ taborog,tabsubstd
!      common /moyenne/ var_2d,var_2d2,var_3d,var_3d2
!      common /stddev/ varsd2d,varsd2d2,varsd3d,varsd3d2
!      common /rms/ varrms2d,varrms2d2,varrms3d,varrms3d2
!      common /eofs/ tabeonormu,tabeonormv,tabeonormt,tabeonormp, &
!           tabeonormr,tabpcsmth,tabpcvar,tabeops,tabeot,tabeorho, &
!           tabeou,tabeov

!     sigma level array (GCM layers + 1 thermosphere layer)
      real,save,private :: sigma(dimlevs+1)
      real,save,private :: aps(dimlevs)
      real,save,private ::  bps(dimlevs)
      real,save,private ::  pseudoalt(dimlevs)
!     orographic data array
      real,save,private ::  taborog(dimlon,dimlat,1)
!     orographic variance array
      real,save,private ::  tabsubstd(dimlon,dimlat,1)

! 2D fields
! tsurf        ! surface temperature
! ps           ! surface pressure
! co2ice       ! co2ice cover
! fluxsurf_lw  ! Thermal IR radiative flux to surface
! fluxtop_lw   ! Thermal IR radiative flux to space
! fluxsurf_sw  ! Solar IR radiative flux to surface
! fluxtop_sw   ! Solar IR radiative flux to space
! dod          ! Dust optical depth
! (dimlon,dimlat,dimuti,nbvar2d)
      real,save,private, dimension(:,:,:,:),pointer :: var_2d => NULL()
! (dimlon,dimlat,dimuti,nbvar2d)
      real,save,private, dimension(:,:,:,:),pointer :: var_2d2 => NULL()
! 3D fields 	
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)
      real,save,private, dimension(:,:,:,:,:),pointer :: var_3d => NULL()
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)
      real,save,private, dimension(:,:,:,:,:),pointer :: var_3d2 => NULL()

! Standard deviation value arrays : surface pressure, surface temperature
! temperature, density, zonal, meridional and vertical wind components
! (dimlon,dimlat,nbsd2d)
      real,save,private, dimension(:,:,:), pointer :: varsd2d => NULL()
! (dimlon,dimlat,nbsd2d)
      real,save,private, dimension(:,:,:), pointer :: varsd2d2 => NULL()
! (dimlon,dimlat,dimlevs,nbsd3d)
      real,save,private, dimension(:,:,:,:),pointer:: varsd3d => NULL()
! (dimlon,dimlat,dimlevs,nbsd3d)
      real,save,private, dimension(:,:,:,:),pointer:: varsd3d2 => NULL()

! RMS value arrays : surface pressure, surface temperature
! temperature, density, zonal and meriodional wind components

! dimension (dimlon,dimlat,nbsd2d)
      real,save,private, dimension(:,:,:), pointer ::  varrms2d => NULL()
! dimension (dimlon,dimlat,nbsd2d)
      real,save,private, dimension(:,:,:), pointer ::  varrms2d2 => NULL()
! dimension (dimlon,dimlat,dimlevs,nbsd3d)
      real,save,private, dimension(:,:,:,:), pointer ::  varrms3d => NULL()
! dimension (dimlon,dimlat,dimlevs,nbsd3d)
      real,save,private, dimension(:,:,:,:), pointer ::  varrms3d2 => NULL()

!     arrays related to EOFs
!     normalisation factors for zonal wind, meriodional wind, temperature
!     pressure and density
      real,save,private ::  tabeonormu(dimlateo)
      real,save,private ::  tabeonormv(dimlateo)
      real,save,private ::  tabeonormt(dimlateo)
      real,save,private ::  tabeonormp(dimlateo)
      real,save,private ::  tabeonormr(dimlateo)

!     smoothed PCs
! (dimlateo,dimeoday,dimnevecs)
      real,save,private, dimension(:,:,:), pointer ::  tabpcsmth  => NULL()
!     PCs variance
! (dimlateo,dimeoday,dimnevecs)
      real,save,private, dimension(:,:,:), pointer ::  tabpcvar  => NULL()

!     EOFS for surface pressure, temperature, density, zonal wind
!     and meriodional wind
! (dimloneo,dimlateo,dimnevecs)
      real,save,private, dimension(:,:,:), pointer ::  tabeops  => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real,save,private, dimension(:,:,:,:), pointer ::  tabeot  => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real,save,private, dimension(:,:,:,:), pointer ::  tabeorho  => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real,save,private, dimension(:,:,:,:), pointer ::  tabeou  => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real,save,private, dimension(:,:,:,:), pointer ::  tabeov  => NULL()

! gestion de l'allocation/desallocation des tableaux dynamiques
      logical, save, private :: modele_init=.false.

  contains
!
!***********************************************************************
!***********************************************************************
!  corps du module                                                     *
!***********************************************************************
!***********************************************************************
!
      subroutine atmemcd_41 (xz,zkey,xlat,xlon,xdate,dset,scena,typper &
           ,invar,init_atm,seedout,ikey,pres,ro,temp,ventu,ventv &
           ,meanvar,extvar,ier)
!***********************************************************************
!$Type
!     Subroutine FORTRAN 90
!$Nom 
!     ATMEMCD
!$Resume
!     Computation of meteorological variables with the
!     Mars Climate Database

!$Version
!     4.0 12/04 add many new output variables.
!     4.0 09/04 height calculation in atmemcd (not in test_emcd anymore)
!     4.0 09/04 choice of vertical coordinates, input output
!     4.0 09/04 R changes with altitude
!     4.0 06/04 add variables, add RMS, up and low atmosphere
!     3.2 03/04 saved variables, seasonal interpolation, variable gravity
!     3.1 05/01 bug fixes and improved interpolations
!     3.0 02/01 - Major MCD release based on earlier code
!     2.3 05/09/00
!     2.2 23/03/00
!$Auteur 
!     v4.0 Final version with new variables, bug fixed, EOF -- FF
!     v4.0 combining low and up atmosphere, variables added --
!          min, mean and max solar scenario for thermosphere --
!          choice of vertical coordinates, flag zkey, zareoid, zsurface,
!          zradius,zpressure in input output --
!          altitude in meters everywhere -- KD, FF
!     v3.2 Variables saved in subroutines, seasonal interpolation added -- SJB
!          Gravity allowed to vary with height in hydrostatic eqn  -- SRL
!     v3.1 Bugs fixed, new ls2sol routine and various improvements
!     to interpolation above model top, especially for EOFs -- SRL
!     Major updates for v3.0 MCD -- SRL + FF
!       See Changes file for more information.
!     Modifications for v2.3 MCD -- SRL
!     Derived from code for version 2 by C. HOURTOLLE
!$Use
!     subroutine atmemcd(xz,zkey,xlat,xlon,xdate,dset,scena,typper,invar
!    &     ,init_atm,seedout,ikey,pres,ro,temp,ventu,ventv
!    &     ,meanvar,extvar,ier)
!$Arguments
!>E xz    : <double precision> vertical coordinate (depends on zkey
!>E zkey  : <integer>          type of vertical coordinate xz
!>                             1 = radius from centre of planet (m)
!>                             2 = height above areoid (m) (MOLA zero datum)
!>                             3 = height above surface (m)
!>                             4 = pressure level (Pa)
!>E xlat  : <double precision> latitude (rad)
!>E xlon  : <double precision> longitude East (rad)
!>E xdate : <double precision> date
!>                             IF xdate > 0 : Earth julian days
!>                             IF xdate < 0 : Mars date =
!>                         -(Int(solar longitude [deg] ) + localtime/100)
!>                        (e.g. xdate = -90.12 correspond to Ls=90 ; LT=12:00)
!>                        (e.g. xdate = -173.18 correspond to Ls=173 ; LT=18:00)
!>E dset  : <character*(*)>    data set
!>E                            One or more blanks to get the default,
!>E                            or full directory path of MCD data required
!>E                            including trailing slash (e.g. '/dir/path/'),
!>E                            or a link to a directory (e.g. 'EMCD_DATA/').
!>E                            Default is link EMCD_DATA in working directory.
!>E scena : <integer>          scenario
!>                            1 = MY24 min solar
!>                            2 = MY24 ave solar
!>                            3 = MY24 max solar
!>                            4 = dust storm tau=3 min solar
!>                            5 = dust storm tau=3 ave solar
!>                            6 = dust storm tau=3 max solar
!>                            7 = warm scenario - dusty, max solar
!>                            8 = cold scenario - low dust, min solar
!>E typper: <double precision> dim 2  perturbation type
!>E                           none : typper(1)= 1. typper(2)=0.
!>E                    large scale : typper(1)= 2. typper(2)=seed number
!>E                    small scale : typper(1)= 3. typper(2)=seed number
!>E          large and small scale : typper(1)= 4. typper(2)=seed number
!>E n times the standard deviation : typper(1)= 5. typper(2)=n
!>E invar : <double precision> Values controlling the variability models
!>E                 invar(1)= wavelength of gravity wave perturbation (m)
!>E                           set to zero to get default (16000m)
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
!>S             meanvar(1)= mean pressure
!>S             meanvar(2)= mean density
!>S             meanvar(3)= mean temperature
!>S             meanvar(4)= mean zonal wind component
!>S             meanvar(5)= mean meridional wind component
!>S extvar : <double precision> dim 50 extra variables array
!>S             extvar(1) = Radial distance from planet center (m)
!>S             extvar(2) = Altitude above areoid (Mars geoid) (m)
!>S             extvar(3) = Altitude above local surface (m)
!>S             extvar(4) = orographic height (m) (surface altitude above areoid)
!>S             extvar(5) = Ls, solar longitude of Mars (deg)
!>S             extvar(6) = LST local solar time (hrs)
!>S             extvar(7) = Universal solar time (LST at lon=0) (hrs)
!>S             extvar(8) =  Air heat capacity Cp (J kg-1 K-1)
!>S             extvar(9) =  gamma=Cp/Cv Ratio of specific heats
!>S             extvar(10)= density RMS day to day variations (kg/m^3)
!>S             extvar(11)= density total standard deviation over season (kg/m^3)
!>S             extvar(12)= random density perturbation (kg/m^3)
!>S             extvar(13)= scale height H(p) (km)
!>S             extvar(14)= not used. (set to zero)
!>S             extvar(15)= surface temperature (K)
!>S             extvar(16)= daily maximum mean surface temperature (K)
!>S             extvar(17)= daily minimum mean surface temperature (K)
!>S             extvar(18)= surf. temperature RMS day to day variations (K)
!>S             extvar(19)= mean surface pressure (Pa)
!>S             extvar(20)= daily maximum mean surface pressure (Pa)
!>S             extvar(21)= daily minimum mean surface pressure (Pa)
!>S             extvar(22)= surface pressure RMS day to day variations (Pa)
!>S             extvar(23)= temperature RMS day to day variations (K)
!>S             extvar(24)= zonal wind RMS day to day variations (m/s)
!>S             extvar(25)= meridional wind RMS day to day variations (m/s)
!>S             extvar(26)= vertical wind component (m/s) >0 when down!
!>S             extvar(27)= vertical wind RMS day to day variations (m/s)
!>S             extvar(28)= small scale perturbation (gravity wave) (kg/m^3)
!>S             extvar(29)= q2: turbulent kinetic energy (m2/s2)
!>S             extvar(30)= not used yet
!>S             extvar(31)= thermal IR flux to surface (W/m2)
!>S             extvar(32)= solar flux to surface (W/m2)
!>S             extvar(33)= thermal IR flux to space (W/m2)
!>S             extvar(34)= solar flux reflected to space (W/m2)
!>S             extvar(35)= surface CO2 ice layer (kg/m2)
!>S             extvar(36)= DOD: Dust column visible optical depth
!>S             extvar(37)= Dust mass mixing ratio (kg/kg)
!>S             extvar(38)= DOD RMS day to day variations
!>S             extvar(39)= DOD total standard deviation over season
!>S             extvar(40)= Water vapor column (kg/m2)
!>S             extvar(41)= Water vapor vol. mixing ratio (mol/mol)
!>S             extvar(42)= Water ice column (kg/m2)
!>S             extvar(43)= Water ice mixing ratio (mol/mol)
!>S             extvar(44)= O3 ozone vol. mixing ratio (mol/mol)
!>S             extvar(45)= [CO2] vol. mixing ratio (mol/mol)
!>S             extvar(46)= [O] vol. mixing ratio (mol/mol)
!>S             extvar(47)= [N2] vol. mixing ratio (mol/mol)
!>S             extvar(48)= [CO] vol. mixing ratio (mol/mol)
!>S             extvar(49)= R: Molecular gas constant (J K-1 kg-1)
!>S             extvar(50)= Air viscosity estimation (N s m-2)
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
!>S                 9 = unknown vertical coordinate
!>K solar: <character*255> 'min','mean','max': solar scenario for
!>K                                             thermosphere
!***********************************************************************

      implicit none

!#include "constants_atmemcd_41.h"

! -------------------------------------------
!     Additional Option (not in the input)
!     itimint, flag for seasonal interpolation
!     itimint 0: no seasonal interpolation
!     itimint 1: seasonal interpolation
      integer ::           itimint
      parameter        (itimint=1)
! -------------------------------------------
      integer :: nextvar,nmeanvar
      parameter (nextvar=50,nmeanvar=5)

!     inputs
!     ******
      real (KIND=8) ::  xz,xlat,xlon,xdate,typper(2),invar
      real (KIND=8) ::  zradius,zareoid,zsurface,zpressure
      real (KIND=8) ::  seedout,pres,ro,temp,ventu,ventv
      integer ::           scena,ikey,zkey,ier
      character (LEN=*) ::     dset
      logical ::           init_atm
!     dust scenario
      integer ::           dust

!     outputs
!     *******
      real (KIND=8) ::  meanvar(nmeanvar),extvar(nextvar)

!     local variables
!     ***************
!     NETCDF channel number :
!     orographic data
      integer ::           unet
!     mean fields
      integer ::           unetm
!     std dev and rms fields
      integer ::           unetsd
!     mean up fields (thermosphere)
      integer ::           unetm_up
!     std dev up and rms fields (thermosphere)
      integer ::           unetsd_up
!     eof fields
      integer ::           ueof
!     season2 for seasonal interpolation- mean fields
      integer ::           unetm2
!     season2 for seasonal interpolation- std dev fields
      integer ::           unetsd2
!     season2 for seasonal interpolation- mean fields - up
      integer ::           unetm2_up
!     season2 for seasonal interpolation- std dev fields - up
      integer ::           unetsd2_up

      save              unet,unetm,unetsd,ueof,unetm2,unetsd2
      save              unetm_up,unetsd_up,unetm2_up,unetsd2_up

!     seed for random number generator
      integer ::           seed
!     save seed, etc to ensure code works with standard F77
      save              seed, dev, rdnos
      real              R , Rnew
      parameter        (R=191.2)
!     perturbation type
      integer ::           varflag
!     number of standard deviation
      real              nbsig
      real              pi
      parameter        (pi=3.14159265359)
      real              crd
      parameter        (crd=180./pi)
!     difference in degree between 2 consecutive points
!     leading to change random phase for gravity wave
      real              deltalgw
      parameter        (deltalgw=1.)

      integer ::           i,l,ierr
      integer ::           levlow,levhi
      real              height         !height (m)
      real              oroheight      !orographic height (m)
      real              absheight      !height above surface (m)
      real              ls
      real              sunlon,sunlat
      real              marsau
      real              localtime
      real              utime    !universal time (0. to 24. hrs)=local time at lon=0
      real              lon      !longitude east [-180  180]
      real              lat      !latitude (degrees)
!     variables
      real              t,tl     !l indicates low level variable from database
      real              ps
      real              p,pl
      real              rho,rhol
      real              u,ul,v,vl,w_l
      real              col_h2ovapor,col_h2oice
      real              q2,vmr_h2oice,vmr_h2o,vmr_o,vmr_co2,vmr_co
      real              vmr_n2,vmr_o3
      real              pertm,pertr,pertrho,pertrhogw,pertrhoeof
      real              pertps
      real              pertu, pertv, pertt
      real              levweight(2)
      real              pratio
      real              lamda   !gravity wave vertical wavelength (m)
      real              rdnos(dimnevecs)
      real              dev
      real              modelday
      real              pscaleheight    !scale height
      real              sdrhol
      real              rscaleheight    !density scale height
      real              tsurf,tsurfmax,tsurfmin,rmstsurf
      real              psurf,psurfmax,psurfmin,rmspsurf
      real              co2ice, dod, rmsdod,tsddod,dust_mmr
      real              Cp, gamma, viscosity,Rgas
      real              fluxsurf_lw, fluxsurf_sw, fluxtop_lw, fluxtop_sw
      real              rmst, rmsu, rmsv, rmsw, rmsrho
      real              tmeanl(6)
      character (LEN=16) ::      name
      character (LEN=4) ::       typevar
      character (LEN=255) ::     dataset
      integer ::           lendataset
      character (LEN=255) ::     dataset2
!     season numbers
      integer ::           numsprec, numsprec_eo,numsprec_sd,numsprec_gw
      integer ::           nums,nums2
!     seasonal interpolation weightings
      real              wl,wh
      data              numsprec /0/
      data              numsprec_eo /0/
      data              numsprec_sd /0/
      data              numsprec_gw /0/
!     previous scenario
      integer ::           dustprec
      data              dustprec /0/
!     previous latitude and longitude
      real              prevlon,  prevlat
      data              prevlon /-999./
      data              prevlat /-999./
      save              numsprec,numsprec_eo,numsprec_sd,numsprec_gw
      save              dustprec,prevlat,prevlon
!        function declarations
!        real              gasdev,ran1,ls2sol
      real          g0, a0
      data          a0, g0 /3396.E3, 3.7257964/

! Variable pour la gestion d'erreur de allocate/deallocate
      integer :: ierralloc

! ===============================================================
      ier=0

!     write(*,*) 'starting atmemcd version 4.1'


!     ****************************
!     inputs check and conversions
!     ****************************
!     data set
!     find last non-blank character
      do lendataset = len(dset), 1, -1
         if (dset(lendataset:lendataset).ne.' ') go to 100
      end do
  100 continue
      if (lendataset.eq.0) then  !default data set case
!***********************************************************************
!  acces au repertoire du nouveau modele version 4.1 par COMPAS        *
!***********************************************************************
         ier = cps_getFichierModele("atmosphere", "EMCD4_1", dataset, &
           rep=.true.)
         if ( MSP_gen_messages ("cps_atmemcd_41") ) then
            return
         end if
         dataset2 = dataset
!***********************************************************************
! valeur attendue dataset = '/usr/local_ms/data/data_emcd_4.1/'        *
!***********************************************************************
      else  !symbolic link or full path given explicitly
         dataset  = dset
         dataset2 = dset
      end if
!
      lendataset = len_trim (dataset)   
      if (dataset(lendataset:lendataset).ne."/") then
         dataset = trim(dataset) // "/"
         dataset2 = dataset
         lendataset=lendataset+1
      endif

!
!     check value of dust scenario
      dust=scena
      if (dust.lt.1.or.dust.gt.8) then
         write(0,*)'ATMEMCD Error: ',scena,' unknown scenario'
         ier=2
         goto 9999
      endif

!     check value of vertical coordinate :
      if (zkey.lt.0.or.zkey.gt.4) then
          write(0,*)'ATMEMCD Error: zkey=',zkey,' unknown vert. coord'
          ier=9
          goto 9999
      end if


!     perturbations
      varflag = int(typper(1))
      if ((varflag.le.0).OR.(varflag.gt.5)) then
         write(0,*)'ATMEMCD Error: ',varflag &
              ,' unknown  perturbation type'
         ier=3
         goto 9999
      end if
      if (((varflag.ge.2).and.(varflag.le.4)).or.(ikey.eq.1)) then
!     gaussian deviates for eofs model and square dist. random number
!     for gravity wave are computed one time per trajectory
!     i.e. computed if seed number changes
         if ((varflag.eq.1).or.(varflag.eq.5)) then
!     random numbers are needed for extra variable
!     = no seed number specified by the user
!     the seed number is forced to -1
            seed=-1
         else if ((varflag.ge.2).and.(varflag.le.4)) then
!     the seed number has to be a negative integer
            if (int(typper(2)).eq.0) then
               seed=-1
            else if (int(typper(2)).gt.0) then
               seed=-int(typper(2))
            else
               seed=int(typper(2))
            endif
         endif
!     if init_atm recompute rdnos and dev
         if (init_atm) then
            do i=1,dimnevecs
               rdnos(i)=gasdev(seed)
            enddo
            dev=ran1(seed)
            seedout=dble(-seed)
         endif
      else
         seedout=0.d0
      endif

!     case n std dev perturbation
      if (varflag.eq.5) then
         nbsig = real(typper(2))
      else
         nbsig=0.
      end if

!     set wavelength for small-scale (gravity wave) perturbations
      if (invar.eq.0.0) then
         lamda = 16.E3  !default wavelength
      else if (invar.lt.2.d3) then
         write(0,*)'ATMEMCD Error: small-scale wavelength too small,', &
              ' set to 2km'
         lamda = 2.E3
      else if (invar.gt.30.d3) then
         write(0,*)'ATMEMCD Error: small-scale wavelength too large,', &
              ' set to 30km'
         lamda = 30.e3
      else
         lamda = real(invar)
      endif
!     convert longitude (rd) East to longitude East (deg)
      lon=real(xlon)*crd
      if(lon.gt.180.) lon=lon-360.
      if(lon.lt.-180.) lon=lon+360.
!     convert latitude from radians to degrees
      lat=real(xlat)*crd

!     if you moved a large distance, recompute gravity wave random phase dev
!     moving is not tested if a new perturbed atmosphere is required
      if (.not.init_atm) then
         if((varflag.eq.3).or.(varflag.eq.4).or.(ikey.eq.1)) then
            if ((abs(prevlat-lat).gt.deltalgw).or. &
                 (abs(prevlon-lon).gt.deltalgw)) then
               dev=ran1(seed)
               seedout=dble(-seed)
            endif
         endif
      end if
      prevlon=lon
      prevlat=lat
      init_atm = .false.

!     **********************************************
!     Compute Ls and the corresponding season number
!     **********************************************
      if (xdate.gt.0d0) then    ! Earth Julian date

        if(xdate.lt.360.d0) &
        write(0,*)'ATMEMCD Warning: Are you sure you want to use Earth' &
         , ' julian date ? Mars date must be <0 in input'

        call orbit(xdate,sunlat,sunlon,ls,marsau,modelday)
!       call mars_ltime with current longitude for correct local time
        call mars_ltime(lon,sunlon,localtime)

      else                     ! Mars "xdate" (see manuals)
         ls=real(int(-xdate))
!        find modelday corresponding to ls
         modelday = ls2sol(ls)
         localtime = 100.0*(-real(xdate) -ls)
         if(localtime.gt.24.) &
            write(0,*) 'ATMEMCD warning : problem with local time > 24'
      end if
!     Compute season numbers :
      if ((dust.eq.4.or.dust.eq.5.or.dust.eq.6).and. &
       (ls.lt.180.)) then
         write(0,*)'ATMEMCD Error: no dust storm scenario for Ls=',Ls
         ier=5
         goto 9999
      endif

      if (itimint.eq.0) then
         call season(ls,nums)
         else if (itimint.ge.1) then
!        if seasonal interpolation
         call season2(ls,nums,nums2,wl,wh,dust)
      endif
      if (.not.modele_init) then
!
! allocation des variables globales :
!
         if (associated(varrms2d)) deallocate(varrms2d, stat=ierralloc)
         if (associated(varrms2d2)) deallocate(varrms2d2, stat=ierralloc)
         if (associated(varrms3d)) deallocate(varrms3d, stat=ierralloc)
         if (associated(varrms3d2)) deallocate(varrms3d2, stat=ierralloc)
         if (associated(varsd2d)) deallocate(varsd2d, stat=ierralloc)
         if (associated(varsd2d2)) deallocate(varsd2d2, stat=ierralloc)
         if (associated(varsd3d)) deallocate(varsd3d, stat=ierralloc)
         if (associated(varsd3d2)) deallocate(varsd3d2, stat=ierralloc)
         if (associated(var_2d)) deallocate(var_2d, stat=ierralloc)
         if (associated(var_2d2)) deallocate(var_2d2, stat=ierralloc)
         if (associated(var_3d)) deallocate(var_3d, stat=ierralloc)
         if (associated(var_3d2)) deallocate(var_3d2, stat=ierralloc)
         if (associated(tabpcsmth)) deallocate(tabpcsmth, stat=ierralloc)
         if (associated(tabpcvar)) deallocate(tabpcvar, stat=ierralloc)
         if (associated(tabeops)) deallocate(tabeops, stat=ierralloc)
         if (associated(tabeot)) deallocate(tabeot, stat=ierralloc)
         if (associated(tabeou)) deallocate(tabeou, stat=ierralloc)
         if (associated(tabeov)) deallocate(tabeov, stat=ierralloc)
         if (associated(tabeorho)) deallocate(tabeorho, stat=ierralloc)
         allocate(varrms2d(dimlon,dimlat,nbsd2d))
         allocate(varrms2d2(dimlon,dimlat,nbsd2d))
         allocate(varrms3d(dimlon,dimlat,dimlevs,nbsd3d))
         allocate(varrms3d2(dimlon,dimlat,dimlevs,nbsd3d))
         allocate(varsd2d(dimlon,dimlat,nbsd2d))
         allocate(varsd2d2(dimlon,dimlat,nbsd2d))
         allocate(varsd3d(dimlon,dimlat,dimlevs,nbsd3d))
         allocate(varsd3d2(dimlon,dimlat,dimlevs,nbsd3d))
         allocate(var_2d(dimlon,dimlat,dimuti,nbvar2d))
         allocate(var_2d2(dimlon,dimlat,dimuti,nbvar2d))
         allocate(var_3d(dimlon,dimlat,dimlevs,dimuti,nbvar3d))
         allocate(var_3d2(dimlon,dimlat,dimlevs,dimuti,nbvar3d))
         allocate(tabpcsmth(dimlateo,dimeoday,dimnevecs))
         allocate(tabpcvar(dimlateo,dimeoday,dimnevecs))
         allocate(tabeops(dimloneo,dimlateo,dimnevecs))
         allocate(tabeot(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeorho(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeou(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeov(dimloneo,dimlateo,dimlevs,dimnevecs))
         varrms2d(:,:,:) = 0.0
         varrms2d2(:,:,:) = 0.0
         varrms3d(:,:,:,:) = 0.0
         varrms3d2(:,:,:,:) = 0.0
         varsd2d(:,:,:) = 0.0
         varsd2d2(:,:,:) = 0.0
         varsd3d(:,:,:,:) = 0.0
         varsd3d2(:,:,:,:) = 0.0
         var_2d(:,:,:,:) = 0.0
         var_2d2(:,:,:,:) = 0.0
         var_3d(:,:,:,:,:) = 0.0
         var_3d2(:,:,:,:,:) = 0.0
         tabpcsmth(:,:,:) = 0.0
         tabpcvar(:,:,:) = 0.0
         tabeops(:,:,:) = 0.0
         tabeot(:,:,:,:) = 0.0
         tabeorho(:,:,:,:) = 0.0
         tabeou(:,:,:,:) = 0.0
         tabeov(:,:,:,:) = 0.0
         modele_init = .true.
      endif

      call mars_ptime(lon,localtime,utime)


!     **************************
!     load arrays from data base
!     **************************

!     if the scenario changes, reset season numbers to reload all needed arrays
      if (dust.ne.dustprec) then
         numsprec=0
         numsprec_eo=0
         numsprec_sd=0
         numsprec_gw=0
         dustprec=dust
      endif

!     if the season number changes, mean value has to be reloaded
      if (nums.ne.numsprec) then
!       open appropriate  NETCDF file
!       *****************************
        call opend(unet,unetm,unetsd,unetm_up,unetsd_up,ueof, &
              nums,dust,dataset(1:lendataset),ierr)
        if (itimint.ge.1) then
           call opend(unet,unetm2,unetsd2,unetm2_up,unetsd2_up,ueof, &
            nums2,dust,dataset2(1:lendataset),ierr)
        elseif (itimint.eq.0) then
           call opend(unet,unetm2,unetsd2,unetm2_up,unetsd2_up,ueof, &
           nums,dust,dataset2(1:lendataset),ierr)
        endif
        if (ierr.ne.0) then
           ier=6
           goto 9999
        endif

!       at the first call, load hybrid coordinates and orographic data
!       **************************************************************
        if (numsprec.eq.0) then
           typevar='hybr'
           call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar,ierr)
           if (ierr.ne.0) then
             ier=7
             go to 9999
           endif
           typevar='orog'
           call loadvar(unet,unet,unet,unet,typevar,ierr)
           if (ierr.ne.0) then
              ier=7
              go to 9999
           endif
        end if !numsprec.eq.0
        numsprec=nums
!       load mean variables
!       *******************
        typevar='mean'
        call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar,ierr)
        if (ierr.ne.0) then
           ier=7
           go to 9999
        endif
      end if !end if nums.ne.numsprec

!     if large scale perturbations or extra variables are requested,
!     load data if not yet done (reading once is enough for all season)
!     **************************************************************
      if (((varflag.eq.2).or.(varflag.eq.4).or.(ikey.eq.1)) &
           .and.(numsprec_eo.eq.0)) then
         call loadeof(ueof,ierr)
         if (ierr.ne.0) then
            ier=7
            go to 9999
         endif
         numsprec_eo=1
      end if

!     if small scale perturbations or extra variables are requested,
!     load data if not yet done
!     **************************************************************
      if (((varflag.eq.3).or.(varflag.eq.4).or.(ikey.eq.1)) &
           .and.(nums.ne.numsprec_gw)) then
        typevar='grwp'
        call loadvar(unet,unet,unet,unet,typevar,ierr)
        if (ierr.ne.0) then
           ier=7
           go to 9999
        endif
        numsprec_gw=nums
      end if
!     if n std dev perturbations or extra variables are requested,
!     load data if not yet done
!     ************************************************************
      if (((varflag.eq.5).or.(ikey.eq.1)) &
           .and.(nums.ne.numsprec_sd)) then
        typevar='stdv'
        call loadvar(unetsd,unetsd_up,unetsd2,unetsd2_up,typevar,ierr)
        typevar='rms'
        call loadvar(unetsd,unetsd_up,unetsd2,unetsd2_up,typevar,ierr)
        if (ierr.ne.0) then
           ier=7
           go to 9999
        endif
        numsprec_sd=nums
      end if

!     ***********************************
!     Meteorological variable computation
!     ***********************************

!     calculate oroheight
!     *******************
      name='orography'
      call var2d(oroheight,lon,lat,1.0,name,ierr,itimint,wl,wh)

!     Calculate datafile levels surrounding point and weight for interpolation
!     ************************************************************************
!     get surface pressure :
      name='ps'
      call var2d(ps,lon,lat,utime,name,ierr,itimint,wl,wh)
      do l=1,dimlevs
        sigma(l)=aps(l)/ps+bps(l)
      enddo
      call getsi(xz,zkey,lon,lat,oroheight,utime,dset, &
       levhi,levlow,levweight,pratio,ierr,itimint,wl,wh, &
       zareoid,zradius,zsurface,zpressure)

      if (ierr.ne.0) then
        write(0,*)'ATMEMCD Error: undefined file hybrid levels'
        ier=8
        goto 9999
      end if

      height=real(zareoid)
      absheight=height-oroheight
!      if underground : stop
      if (absheight.lt.-0.00001) then
        write(0,*)'ATMEMCD Error: underground object '
        ier=4
        goto 9999
      endif
!     read mean data from stored arrays
!     *********************************
      name='u'
      call var3d(ul,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
      name='v'
      call var3d(vl,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
      name='temp'
      call var3d(tl,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
      name='rho'
      call var3d(rhol,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
      tmeanl(1)=pratio*ps*(sigma(levlow) &
           +(sigma(levhi)-sigma(levlow))*levweight(2))
      tmeanl(2)=rhol
      tmeanl(3)=tl
      tmeanl(4)=ul
      tmeanl(5)=vl
      do i=1,nmeanvar
         meanvar(i)=dble(tmeanl(i))
      enddo

!     add large scale variability if required
!     ***************************************
      if ((varflag.eq.2).or.(varflag.eq.4)) then
         name='rho'
         call eofpb(pertm,pertr,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
         pertrhoeof=pertr
         rhol=rhol+pertr
         name='u'
         call eofpb(pertm,pertr,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
         ul=ul+pertr
         name='v'
         call eofpb(pertm,pertr,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
         vl=vl+pertr
         name='temp'
         call eofpb(pertm,pertr,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
         tl=tl+pertr
         name='ps'
         call eofpb(pertm,pertr,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
         ps=ps+pertr
      else if(ikey.eq.1) then ! just compute for density as an extra variable
        name='rho'
        call eofpb(pertm,pertrhoeof,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
      endif
!     add small scale (gravity wave) variability  if required
!     *******************************************************
      if ((varflag.eq.3).or.(varflag.eq.4)) then
         name='u'
         absheight=height-oroheight
         call grwpb(dset,pertu,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),utime,name,ierr,itimint,wl,wh)
         name='v'
         call grwpb(dset,pertv,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),utime,name,ierr,itimint,wl,wh)
         name='temp'
         call grwpb(dset,pertt,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),utime,name,ierr,itimint,wl,wh)
         name='rho'
         call grwpb(dset,pertrho,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),utime,name,ierr,itimint,wl,wh)
         pertrhogw = pertrho
         ul = ul + pertu
         vl = vl + pertv
         tl = tl + pertt
         rhol= rhol + pertrho
      else if(ikey.eq.1) then ! just compute for density as an extra variable
        name='rho'
        call grwpb(dset,pertrhogw,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),utime,name,ierr,itimint,wl,wh)
      endif

!     add n sigmas if required
!     ************************
      if (varflag.eq.5) then
         name='tsdu'
         call var3d(pertu,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='tsdv'
         call var3d(pertv,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='tsdrho'
         call var3d(pertrho,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='tsdtemp'
         call var3d(pertt,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='tsdps'
         call var2d(pertps,lon,lat,1.0,name,ierr,itimint,wl,wh)
         ul = ul + (nbsig*pertu)
         vl = vl + (nbsig*pertv)
         rhol = rhol + (nbsig*pertrho)
         tl = tl + (nbsig*pertt)
         ps = ps + (nbsig*pertps)
      endif
!     calculate pressure
!     ******************
      pl=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow))*levweight(2))
      pl=pratio*pl

      t=tl
      p=pl
      rho=rhol
      u=ul
      v=vl

!     (re)compute extra variables if required
!     ***************************************
      if (ikey.eq.1) then
         Rnew=p/(rho*t)
         pscaleheight=Rnew*t/(g0*a0**2/(a0+real(zareoid))**2)
         rscaleheight=pscaleheight

         name='tsdrho'
         call var3d(sdrhol,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='tsurf'
         call var2d(tsurf,lon,lat,utime,name,ierr,itimint,wl,wh)
         call max2d(tsurfmax,lon,lat,name,ierr,itimint,wl,wh)
         call min2d(tsurfmin,lon,lat,name,ierr,itimint,wl,wh)
         name='rmstsurf'
         call var2d(rmstsurf,lon,lat,1.0,name,ierr,itimint,wl,wh)
         name='ps'
         call var2d(psurf,lon,lat,utime,name,ierr,itimint,wl,wh)
         call max2d(psurfmax,lon,lat,name,ierr,itimint,wl,wh)
         call min2d(psurfmin,lon,lat,name,ierr,itimint,wl,wh)
         name='rmsps'
         call var2d(rmspsurf,lon,lat,1.0,name,ierr,itimint,wl,wh)
         name='co2ice'
         call var2d(co2ice,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='fluxsurf_lw'
         call var2d(fluxsurf_lw,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='fluxsurf_sw'
         call var2d(fluxsurf_sw,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='fluxtop_lw'
         call var2d(fluxtop_lw,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='fluxtop_sw'
         call var2d(fluxtop_sw,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='rmstemp'
         call var3d(rmst,lon,lat,levhi,levlow,levweight,pratio, &
              1.0,name,ierr,itimint,wl,wh)
         name='rmsrho'
         call var3d(rmsrho,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='rmsu'
         call var3d(rmsu,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='rmsv'
         call var3d(rmsv,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
           name='w'
         call var3d(w_l,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='rmsw'
         call var3d(rmsw,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='q2'
         call var3d(q2,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_h2ovapor'
         call var3d(vmr_h2o,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_h2oice'
         call var3d(vmr_h2oice,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_o3'
         call var3d(vmr_o3,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_o'
         call var3d(vmr_o,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_co2'
         call var3d(vmr_co2,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_co'
         call var3d(vmr_co,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
         name='vmr_n2'
         call var3d(vmr_n2,lon,lat,levhi,levlow,levweight,pratio, &
           utime,name,ierr,itimint,wl,wh)
!        name='vmr_h2'
!        call var3d(vmr_h2,lon,lat,levhi,levlow,levweight,pratio,
!    :     utime,name,ierr,itimint,wl,wh)
         name='dod'
         call var2d(dod,lon,lat,utime,name,ierr,itimint,wl,wh)
         call dustmix(scena,lat,Ls,dod,psurf,p,dust_mmr)
         name='rmsdod'
         call var2d(rmsdod,lon,lat,1.0,name,ierr,itimint,wl,wh)
         name='tsddod'
         call var2d(tsddod,lon,lat,1.0,name,ierr,itimint,wl,wh)
         name='col_h2ovapor'
         call var2d(col_h2ovapor,lon,lat,utime,name,ierr,itimint,wl,wh)
         name='col_h2oice'
         call var2d(col_h2oice,lon,lat,utime,name,ierr,itimint,wl,wh)
         call air_properties(t,vmr_co2,vmr_n2,vmr_o,vmr_co, &
                             Cp,gamma,viscosity,Rgas)
!        store values of extra variables

         extvar(1)=dble(zradius)
         extvar(2)=dble(zareoid)
         extvar(3)=dble(zsurface)
         extvar(4)=dble(oroheight)
         extvar(5)=dble(ls)
         extvar(6)=dble(localtime)
         extvar(7)=dble(utime)
         extvar(8)=dble(Cp)
         extvar(9)=dble(gamma)
         extvar(10)=dble(rmsrho)
         extvar(11)=dble(sdrhol)
         extvar(12)=dble(pertrhoeof)
         extvar(13)=dble(pscaleheight)
         extvar(14)=0.d0
         extvar(15)=dble(tsurf)
         extvar(16)=dble(tsurfmax)
         extvar(17)=dble(tsurfmin)
         extvar(18)=dble(rmstsurf)
         extvar(19)=dble(psurf)
         extvar(20)=dble(psurfmax)
         extvar(21)=dble(psurfmin)
         extvar(22)=dble(rmspsurf)
         extvar(23)=dble(rmst)
         extvar(24)=dble(rmsu)
         extvar(25)=dble(rmsv)
         extvar(26)=dble(w_l)
         extvar(27)=dble(rmsw)
         extvar(28)=dble(pertrhogw)
         extvar(29)=dble(q2)
         extvar(30)=0.d0
         extvar(31)=dble(fluxsurf_lw)
         extvar(32)=dble(fluxsurf_sw)
         extvar(33)=dble(fluxtop_lw)
         extvar(34)=dble(fluxtop_sw)
         extvar(35)=dble(co2ice)
         extvar(36)=dble(dod)
         extvar(37)= dble(dust_mmr)
         extvar(38)= dble(rmsdod)
         extvar(39)= dble(tsddod)
         extvar(40)= dble(col_h2ovapor)
         extvar(41)= dble(vmr_h2o)
         extvar(42)= dble(col_h2oice)
         extvar(43)= dble(vmr_h2oice)
         extvar(44)= dble(vmr_o3)
         extvar(45)= dble(vmr_co2)
         extvar(46)= dble(vmr_o)
         extvar(47)= dble(vmr_n2)
         extvar(48)= dble(vmr_co)
         extvar(49)= dble(Rgas)
         extvar(50)= dble(viscosity)
      else
         do i=1,nextvar
            extvar(i)=0.d0
         enddo
         extvar(1)=dble(zradius)
         extvar(2)=dble(zareoid)
         extvar(3)=dble(zsurface)
         extvar(4)=dble(oroheight)
         extvar(5)=dble(ls)
         extvar(6)=dble(localtime)
         extvar(7)=dble(utime)
      endif

!     convert to double precision
      pres=dble(p)
      ro=dble(rho)
      temp=dble(t)
      ventu=dble(u)
      ventv=dble(v)

      return

!     Error handling : all the outputs are set to a missing data value
 9999 pres=-999.d0
      ro=-999.d0
      temp=-999.d0
      ventu=-999.d0
      ventv=-999.d0
      do i=1,nmeanvar
         meanvar(i)=-999.d0
      end do
      do i=1,nextvar
         extvar(i)=-999.d0
      enddo

      return
      end  subroutine  atmemcd_41

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine eofpb(pertm,pertr,rdnos,lon,lat,levhi,levlow, &
                       levweight,pratio,day,name,ier)

!     Calculate an EOF perturbation on the 3d variable=name at
!     longitude=lon, latitude=lat sigma=sigma and day=day.

!     Improved variability model...EOFs calculated in longitude-sigma plane

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real          rdnos(dimnevecs) !Uniform deviates for a single profile
      real          lon              !longitude east of point
      real          lat              !latitude of point
      integer ::       levhi            !database level upper bound
      integer ::       levlow           !database level lower bound
      real          levweight(2)     !level weight for interpolation
!     (1) for linear in height (2) for linear in pressure
      real          pratio           !ratio of pressure to extreme value if out of range
      real          day              !model day
      character (LEN=16) ::  name             !name of variable

!     outputs
      integer ::       ier              !error flag (0=OK, 1=NOK)
      real          pertm            !perturbation corresponding to trend
      real          pertr            !perturbation corresponding to random component

!     local variables
      integer ::       i,indlat,indlon,indday
      real          norm
      real          pcsmth(dimnevecs)
      real          pcvar(dimnevecs)
      real          evecs(dimnevecs)
      real          alon,aday,u


      ier = 0

!     determination of the index of the nearest latitude MCD grid point
      indlat=int((90.-lat)/deltalateo)+1
      indlat=max(indlat,dimlateo)

!     determination of the index of the nearest longitude MCD grid point
      indlon=1 ! just initialize to avoid somecompiler warning...
      if (lon.le.lonmineo) then
         u=(lonmineo-lon)/deltaloneo
         if (u.le.0.5)then
            indlon=1
         else
            indlon=dimloneo
         endif
      elseif (lon.ge.lonmaxeo) then
         u=(lon-lonmaxeo)/deltaloneo
         if (u.le.0.5)then
            indlon=dimloneo
         else
            indlon=1
         endif
      else
         alon=lonmineo
         do i=1,dimloneo-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltaloneo)) then
               u=(lon-alon)/deltaloneo
               if (u.le.0.5)then
                  indlon=i
               else
                  indlon=i+1
               endif
            endif
            alon=alon+deltaloneo
         enddo
      endif

!     determination of the nearest database day
      indday=1 ! just initialize to avoid somecompiler warning...
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

!     read normalisation factor and eofs
!     normalisation factor is no longer the standard deviation

!     EOF perturbations are stored up to the last model level
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
      elseif (name.eq.'temp') then
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
      elseif (name.eq.'temp') then
         norm=tabeonormt(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeot(indlon,indlat,dimlevs,i)
         enddo
      elseif (name.eq.'rho') then
         norm=tabeonormr(indlat)
         do i=1,dimnevecs
            evecs(i)= tabeorho(indlon,indlat,dimlevs,i)
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

!     read smoothed PCs

      do i=1,dimnevecs
         pcsmth(i)=tabpcsmth(indlat,indday,i)
      enddo

!     read PCs variance

      do i=1,dimnevecs
         pcvar(i)=tabpcvar(indlat,indday,i)
      enddo

!     calculate perturbation

      pertm=0.0
      pertr=0.0
      do i=1,dimnevecs
        pertm=pertm+pcsmth(i)*evecs(i)
        pertr=pertr+pcvar(i)*rdnos(i)*evecs(i)
      enddo

!     renormalise

      pertm=pertm*norm
      pertr=pertr*norm

! re-scaling correction for densities which are above top level

      if (name.eq.'rho') then
         pertm=pratio*pertm
         pertr=pratio*pertr
      endif

      return

!     error handling
 9999 pertm = 0.0
      pertr = 0.0
      return
      end subroutine  eofpb

!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function gasdev(idum) result(res)

!     Return a Gaussian deviate with zero mean and unit standard deviation

      implicit none

!     inputs
      integer :: idum    !Seed for random number generator

!     local variables
      integer :: iset
      data    iset/0/
      real :: v1
      real :: v2
      real :: r
      real :: fac
      real :: gset = 0.
      real :: res
!      real    ran1

      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        r=v1**2+v2**2
        if (r.ge.1.) goto 1
        fac=sqrt(-2.*log(r)/r)
        gset=v1*fac
        res=v2*fac
        iset=1
      else
        res=gset
        iset=0
      endif

      return
      end function  gasdev

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsi(xz,zkey,lon,lat,oroheight,utime,dset, &
                     levhi,levlow,levweight,pratio,ier,itimint,wl,wh, &
          zareoid,zradius,zsurface,zpressure)

!     Find the nearest datafile levels and interpolation weights at
!     longitude=lon, latitude=lat and vertical coordinate xz

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real                      ps             !surface pressure (Pa)

      real (KIND=8) ::          xz
      integer ::                   zkey
!  zkey    : <integer>          switch to choose which z variable
!                               is used to find other three :
!                      zkey=1  zradius (m)  -> zareoid, zsurface, zpressure
!                      zkey=2  zareoid (m)  -> zradius, zsurface, zpressure
!                      zkey=3  zsurface (m) -> zradius, zareoid, zpressure
!                      zkey=4  zpressure (Pa) -> zradius, zareoid, zsurface
!                      zkey=5  case of getsi call by grwpb

      real          oroheight      !height of surface above reference areoid (m)
      real          lon            !east longitude
      real          lat            !latitude
      real          utime          !Universal time (0 to 24 hrs)=local time at lon=0
      character (LEN=*) :: dset           !Dataset
      integer ::       itimint        !seasonal interpolation flag
      real          wl, wh         !seasonal interpolation weightings

!     outputs
      integer ::       ier            !error flag (0=OK, not 0 =NOK)
      integer ::       levhi          !database level upper bound
      integer ::       levlow         !database level lower bound
      real          levweight(2)   !level weight for interpolation
!     (1) for linear in height (2) for linear in pressure
      real          pratio         !p/p(top, bottom) for extrapolation, 1 if in range

      real          height         !height of point above surface (m)
      real          absheight         !height of point above surface (m)
      real (KIND=8) ::       zradius,zareoid,zsurface,zpressure

!     locals
      real (KIND=8) ::          xlon           ! longitude radians
      real (KIND=8) ::          xlat           !latitude radians
      integer ::       l
      real          dsigma(dimlevs)
      real          hsigma(dimlevs)
      real          sheight(dimlevs), zsig
      integer :: zkey_height

!     Gravity on mean areoid
!     The areoid is defined as a surface of constant gravitational plus
!     rotational potential. The inertial rotation rate of Mars is assumed
!     to be 0.70882187E-4 rad/s. This potential is the mean value at the
!     equator at a radius of 3396.000 km, namely 12652804.7 m^2/s^2,
!     calculated from Goddard Mars Gravity Model mgm1025
!     [LEMOINEETAL2001] evaluated to degree and order 50.
      real          g, g0, a0
      data          g0 /3.7257964/
      data          a0 /3396.E3/
!      real          R
!      data          R /191.1/
!     R replaced by Rnew  (vary with altitude)
      real          Rnew(dimlevs)
      real          Tmean
      real          Rogct(dimlevs)
      character (LEN=16) ::  name
      real          t(dimlevs)        !database layers
      real          rho(dimlevs)        !database layers
      save g0,a0
      ier=0

!     compute mean and delta sigma for each layer
!     (hsigma is not the same as the half levels in the model)
      hsigma(1)=1.
      do l=2,dimlevs
      hsigma(l)=.5*(sigma(l-1)+sigma(l))
      enddo
      dsigma(1)=1.-sigma(1)
      do l=2,dimlevs
      dsigma(l)=sigma(l-1)-sigma(l)
      enddo

!     get temperature profile
      name='temp'
      call profi(t,lon,lat,utime,name,ier,itimint,wl,wh)
      if (ier.ne.0) then
             write(0,*)'GETSI Error : temperature profile not available'
              ier=8
              goto 9999
      endif

!     get  rho
      name='rho'
      call profi(rho,lon,lat,utime,name,ier,itimint,wl,wh)
      if (ier.ne.0) then
             write(0,*)'GETSI Error : rho profile not available'
              ier=8
              goto 9999
      endif

!     get surface pressure
      name='ps'
      call var2d(ps,lon,lat,utime,name,ier,itimint,wl,wh)
      do l=1,dimlevs
             Rnew(l) =  (aps(l)+bps(l)*ps) &
                            / ( rho(l) * T(l) )
      enddo


!     Calculate altitude above the surface of each model layer: sheight(l)
!     integrate hydrostatic equation
!     Rogct is the R/g variable depending on dimlevs
      g = g0*a0**2/(a0+oroheight)**2
      Rogct(1) = Rnew(1)/g
      Tmean = t(1)
      sheight(1)= - Rogct(1)*t(1)*log(sigma(1))

      do l=2, dimlevs
      if (t(l).ne.t(l-1)) then
         Tmean = (t(l)-t(l-1)) / log(t(l)/t(l-1))
      else
         Tmean = t(l)
      end if
      g = g0*a0**2/(a0+oroheight+sheight(l-1))**2
      Rogct(l) = Rnew(l)/g
      sheight(l) = sheight(l-1) - Rogct(l)*Tmean* &
       log(sigma(l)/sigma(l-1))
      end do

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     height calculation
      if (zkey.eq.1) then
              zradius=xz
      elseif (zkey.eq.2) then
              zareoid=xz
      elseif (zkey.eq.3) then
            zsurface=xz
      elseif (zkey.eq.4) then
          zpressure=xz
!         calcul de zsurface
          if(zpressure.gt.ps) then
             write(0,*)'ATMEMCD Error: underground object '
             ier=4
             goto 9999
          end if
          zsig = real(zpressure) / ps
          if (zsig.gt. sigma(1)) then
             zsurface= real((-log(zsig)*Rnew(1)*t(1)/g0),kind=8)
          else if (zsig.lt.sigma(dimlevs) ) then
             g = g0*a0**2/(a0+sheight(dimlevs))**2
             zsurface= real((sheight(dimlevs) &
              -log(zsig/sigma(dimlevs))*Rnew(dimlevs)*t(dimlevs)/g), kind=8)
          else
            do  l=1,dimlevs
              if ((zsig.ge.sigma(l+1)).and.(zsig.le.sigma(l))) then
                zsurface= real((sheight(l) + (sheight(l+1) -sheight(l)) &
                  * log(zsig/sigma(l)) / log(sigma(l+1)/sigma(l))),kind=8)
              end if
            end do
          end if
      endif

! absheight = zsurface for the first call of getsi
! absheight = xz for call of getsi in  grwpb
      if (zkey.eq.5) then
      absheight=real(xz)
      else
      ! lat and lon converted to radians
      xlat = real(lat * 3.1415927/180., kind=8)
      xlon = real(lon * 3.1415927/180., kind=8)
      zkey_height =min(zkey,3)
      call calc_height_41(xlat,xlon,zradius,zareoid,zsurface, &
          zkey_height,dset,ier)
      height=real(zareoid)
      absheight=height-oroheight
      endif
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!     find levhi, levlow and levweight
      g = g0*a0**2/(a0+oroheight+absheight)**2
      if (absheight.lt.sheight(1)) then
!       below the lowest layer
        levhi=1
        levlow=1
        levweight(1)=1.
        levweight(2)=0.
!       pratio=exp(1000.*(sheight(1)-absheight)*g/(Rnew(1)*t(1)))
        pratio=exp((sheight(1)-absheight)*g/(Rnew(1)*t(1)))
      elseif (absheight.ge.sheight(dimlevs)) then
!       above the top layer
        levhi=dimlevs
        levlow=dimlevs
        levweight(1)=0.
        levweight(2)=1.
!       pratio=exp(1000.*(sheight(dimlevs)-absheight)*g/(Rnew(dimlevs)
!    &  *t(dimlevs)))
        pratio=exp((sheight(dimlevs)-absheight)*g/(Rnew(dimlevs) &
        *t(dimlevs)))
      else
       do l=1,dimlevs-1
       if ((absheight.ge.sheight(l)) &
       .and.(absheight.lt.sheight(l+1))) then
          levhi=l+1
          levlow=l
          levweight(1)=(absheight-sheight(levlow)) &
                      /(sheight(levhi)-sheight(levlow))
          levweight(2)=(1.-(sigma(levhi)/sigma(levlow))**levweight(1)) &
                      /(1.-(sigma(levhi)/sigma(levlow)))
          pratio=1.
        endif
       enddo
      endif

      zpressure=real(ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow)) &
       *levweight(2)), kind=8)
      return

!     Errror handling
 9999 levhi=0
      levlow=0
      levweight(1)=0.
      levweight(2)=0.
      pratio=0.

      return
      end subroutine  getsi

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grid4(lon,lat,dlon,dlat,t,u)

!     Given longitude=lon and latitude=lat find the nearest 4 horizontal
!     gridpoints in the database and bilinear interpolation weights.

!     Grid points are arranged as in Numerical Recipies page 117

!     4 3
!     1 2

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real    lon      !east longitude of point
      real    lat      !latitude of point

!     outputs
      integer :: dlon(4)  !indices longitudes of database points
      integer :: dlat(4)  !indices latitudes of database points
      real    t        !weight
      real    u        !weight

!     local variables
      real    alon,alat
      integer :: i

      dlon(:) = 0

!     wraparound
      if (lon.ge.lonmax) then
         dlon(1)=dimlon
         dlon(2)=1
         dlon(3)=1
         dlon(4)=dimlon
         t=(lon-lonmax)/deltalon
      else
         alon=lonmin
         do i=1,dimlon-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltalon)) then
               dlon(4)=i
               dlon(3)=i+1
               dlon(1)=i
               dlon(2)=i+1
               t=(lon-alon)/deltalon
            endif
            alon=alon+deltalon
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
            if ((lat.ge.alat).and.(lat.lt.alat+deltalat)) then
               dlat(4)=i+1
               dlat(3)=i+1
               dlat(1)=i
               dlat(2)=i
               u=(lat-alat)/deltalat
            endif
            alat=alat+deltalat
         enddo
      endif

      return
      end subroutine  grid4

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grwpb(dset,pert,dev,lamda,lon,lat,absheight,rho,u,v, &
        utime,name ,ier,itimint,wl,wh)

!     Small scale gravity wave perturbation model.
!     Computes perturbation on u, v, theta or rho, with wave phase factor.

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      character (LEN=*) ::     dset     ! dataset directory
      real         dev     !random number between 0 and 1
      real         lamda   !vertical wavelength of g.w. (m)
      real         lon     !east longitude of point
      real         lat     !latitude of point
      real         absheight  !height of point
      real         rho     !density at height=height
      real         u       !zonal wind at height=height
      real         v       !meridional wind at height=height
      real         utime   !Universal time (0. to 24. hrs) = local time at lon=0
      real         ps
      integer ::      itimint !seasonal interpolation flag
      real         wl,wh   !seasonal interpolation weightings

      character (LEN=16) :: name    !name of variable to perturb

!     outputs
      integer ::      ier     !error flag (0=OK, 1=NOK)
      real         pert    !perturbation

!     local variables
      ! zareoid,zpressure,zradius,zsurface only local
      ! we do not care of those variables here
      real (KIND=8) :: xz,zareoid,zpressure,zradius,zsurface
      integer ::      zkey
      integer ::      ierr
      real         a, aprime
      real         rho0,rho1
      real         u0,u1
      real         v0,v1
      real         tmpheight
      integer ::      levlow,levhi
      real         levweight(2)
      real         pratio
      real         dz,dz_max
      real         sig
      real         umag
      real         hmax    !for perturbations above hmax, use amplitude at hmax (m)
      parameter   (hmax=100.E3)
      real         usat    !for windspeeds below usat, wave amplitude saturates
      Parameter   (usat=0.5)
      real         dalr    !dry adiabatic lapse rate (K/m)
      parameter   (dalr=4.5E-3)
      real         pi
      parameter   (pi=3.14159265359)
      character (LEN=16) :: tmpname

      ier=0

!     get rho0, u0, v0
!     use mean data from level 1, not height=0.0
      levlow=1
      levhi=1
      levweight(1)=1.
      levweight(2)=1.
      pratio=1.0
      tmpname='u'
      call var3d(u0,lon,lat,levhi,levlow,levweight,pratio,utime, &
                 tmpname,ierr,itimint,wl,wh)
      tmpname='v'
      call var3d(v0,lon,lat,levhi,levlow,levweight,pratio,utime, &
                 tmpname,ierr,itimint,wl,wh)
      tmpname='rho'
      call var3d(rho0,lon,lat,levhi,levlow,levweight,pratio,utime, &
                 tmpname,ierr,itimint,wl,wh)
      tmpname='ps'
      call var2d(ps,lon,lat,utime,tmpname,ierr,itimint,wl,wh)

!     get sub-grid scale variance
      tmpname='substd'
      call var2d(sig,lon,lat,1.0,tmpname,ierr,itimint,wl,wh)

!     compute delta z (N=N0 is implicit)
      if (absheight.le.hmax) then
         umag=sqrt(u**2+v**2)
!        the wave amplitude becomes large as the wind speed becomes small
!        and the wave should saturate
         if (umag.lt.usat) umag=usat
!        *********************
!         theoretical version:
          dz=sig*sqrt((rho0*sqrt(u0**2+v0**2))/(rho*umag))
!        *********************
!         Version 3 :
!        dz=1.E3*sqrt((rho0*sqrt(u0**2+v0**2)*sig*1.E-3)/(rho*umag))
!        *********************
         tmpheight=absheight
      else
!         perturbation above hmax km:
!         Using the same amplitude and same perturbation
!         as at hmax km for T,u,v. The perturbation for rho is
!         scaled on the T perturbation for rho at height=height
         tmpheight=hmax          ! set height for perturbation calculation
!         get info on variable at hmax m:
         xz=real(tmpheight, kind=8)
         zkey=5
         call getsi(xz,zkey,lon,lat,0.0,utime,dset, &
          levhi,levlow,levweight,pratio,ierr,itimint,wl,wh, &
          zareoid,zradius,zsurface,zpressure)
         tmpname='rho'
         call var3d(rho1,lon,lat,levhi,levlow,levweight, &
              pratio,utime, tmpname,ierr,itimint,wl,wh)
         tmpname='u'
         call var3d(u1,lon,lat,levhi,levlow,levweight, &
              pratio,utime, tmpname,ierr,itimint,wl,wh)
         tmpname='v'
         call var3d(v1,lon,lat,levhi,levlow,levweight, &
              pratio,utime, tmpname,ierr,itimint,wl,wh)
         umag=sqrt(u1**2+v1**2)
!        the wave amplitude becomes large as the wind speed becomes small
!        and the wave should saturate
         if (umag.lt.usat) umag=usat
!        dz=(rho0*sqrt(u0**2+v0**2)*sig)/(rho1*umag)
!        dz=sqrt(dz)
          dz=sig*sqrt((rho0*sqrt(u0**2+v0**2))/(rho1*umag))
      end if

!     apply simple test for saturation (require theta_z > 0)
!     and compute wave
      dz_max=lamda/(2.*pi)
      if (dz.gt.dz_max) then
         dz=dz_max
      endif
      dz=dz*sin(2.*pi*dev+(2.*pi*absheight)/lamda)

!     Find perturbation from change delta z in mean profile
!     (more accurate than taking first derivative).
!     Using an oroheight of 0km in getsi here implies an insignificant
!     error in the calculated value of g used to find the distance to
!     the perturbation, very much smaller than other assumptions made
!     (interpolation, adiabatic, constant g, etc.).

!     don't allow dz to exceed the current height (close to the ground)
      if ((tmpheight+dz).le.0.) then
         dz = -0.99*tmpheight
      endif
      tmpname = name
      if (name.eq.'rho') tmpname= 'temp'
         xz=real(tmpheight, kind=8)
         zkey=5
      call getsi(xz,zkey,lon,lat,0.0,utime,dset, &
        levhi,levlow,levweight,pratio,ierr,itimint,wl,wh, &
          zareoid,zradius,zsurface,zpressure)
      call var3d(a,lon,lat,levhi,levlow,levweight,pratio, &
              utime,tmpname,ierr,itimint,wl,wh)
         xz=real(tmpheight+dz, kind=8)
      call getsi(xz,zkey,lon,lat,0.0,utime,dset, &
          levhi,levlow,levweight,pratio,ierr,itimint,wl,wh, &
          zareoid,zradius,zsurface,zpressure)
      call var3d(aprime,lon,lat,levhi,levlow,levweight,pratio, &
                 utime,tmpname,ierr,itimint,wl,wh)
      pert = aprime - a

!     correct for perturbation to potential temperature and density
      if (tmpname.eq.'temp') pert = pert + dalr*dz
!     pert(rho) = -rho*pert(T)/(T+pert(T))
      if (name.eq.'rho') then
         pert = -rho*pert/(a+pert)
      end if

      return
      end subroutine  grwpb

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadvar(nf,nf_up,nf2,nf_up2,type,ier)

!     load arrays corresponding to the variable type

      implicit none

!     include "netcdf.inc"
! #include "constants_atmemcd_41.h"

! variable type to load :
! 2D LOW mean = tsurf,ps,co2ice,fluxsurf_lw,fluxtop_lw,fluxsurf_sw,
!               fluxtop_sw,dod,col_h2ovapor,col_h2oice! 2D values
!               q2,vmr_h2ovapor,vmr_h2oice,vmr_o3,temp,u,v,rho ! 3D low (nf=unem)
! UP     mean = temp,u,v,w,rho,vmr_o,vmr_co2,_vmr_co !3D up (nf=unem_up)
!        sigm = sigma values of mean field file (nf=unetm)
!        orog = orography values of mountain file (nf=unet)
!        eofs = normu,normu,normt,normp,normr,u,v,t,rho,ps,pcsmth,pcvar
!               of eof field file (nf=ueof)
!        grwp = substd values of mountain file (nf=unet)
! 2D&LOW stdv = tsdtsurf,tsdps,tsddod, !2D
!               tsdtemp,tsdsu,tsdsv,tsdrho !3D low values of
!               standard deviation field file (nf=unetsd)
! 2D&LOW rms  = rmstsurf,rmsps,rmsco2ice, !2D
!               rmstemp,rmsu,rmsv,rmsrho !3D low values of
!               rms field file (nf=unetsd)
! UP     stdv = tsdtemp,tsdsu,tsdsv,tsdw,tsdrho !3D up values of
!               standard deviation field file (nf=unetsd_up)
! UP     rms  = rmstemp,rmssu,rmssv,rmsw,rmsrho !3D up values of
!               rms field file (nf=unetsd_up)

!     inputs
      integer ::      nf,nf_up,nf2,nf_up2 !NETCDF channel number
      character (LEN=4) ::  type
      character (LEN=16) :: name

!     outputs
      integer ::      ier                 !error flag (0=OK, 1=NOK)

!     local variables
      integer ::      ierr
      integer ::      varid
      integer ::      l,k,iloop,jloop
      real         aps_low(dimlevs),bps_low(dimlevs)
      real         aps_up(dimlevs)
      character (LEN=50) :: varname2d(nbvar2d)
      character (LEN=50) :: varname3d(nbvar3d)
      character (LEN=50) :: sd2d(nbsd2d)
      character (LEN=50) :: sd3d(nbsd3d)
      real temp2d(dimlon,dimlat)


      varname2d(1)="tsurf"
      varname2d(2)="ps"
      varname2d(3)="co2ice"
      varname2d(4)="fluxsurf_lw"
      varname2d(5)="fluxtop_lw"
      varname2d(6)="fluxsurf_sw"
      varname2d(7)="fluxtop_sw"
      varname2d(8)="dod"
      varname2d(9)="col_h2ovapor"
      varname2d(10)="col_h2oice"

      sd2d(1)="tsurf"
      sd2d(2)="ps"
      sd2d(3)="dod"

      varname3d(1)="q2"
      varname3d(2)="vmr_h2ovapor"
      varname3d(3)="vmr_h2oice"
      varname3d(4)="vmr_o3"
      varname3d(5)="temp"
      varname3d(6)="u"
      varname3d(7)="v"
      varname3d(8)="rho"
      varname3d(9)="w"
      varname3d(10)="vmr_o"
      varname3d(11)="vmr_co2"
      varname3d(12)="vmr_co"
      varname3d(13)="vmr_n2"
!cccc varname3d(14)="vmr_h2"

      sd3d(1)="temp"
      sd3d(2)="u"
      sd3d(3)="v"
      sd3d(4)="rho"
      sd3d(5)="w"

      if (type.eq.'mean') then

! MEAN 2d
        do k=1,nbvar2d
        call get_2d(nf,k,varname2d(k),1)
        call get_2d(nf2,k,varname2d(k),2)
        enddo
! MEAN  only in low file! up data extrapolated
        do k=1,nbvarlow-nbcom
        call get_3d(nf,k,varname3d(k),low,1)
        call get_3d(nf2,k,varname3d(k),low,2)
        call extrapol3d(type,k,up)
        enddo
! MEAN up and low COMMON
        do k=nbvarlow-nbcom+1,nbvarlow
        call get_3d(nf,k,varname3d(k),low,1)
        call get_3d(nf2,k,varname3d(k),low,2)
        call get_3d(nf_up,k,varname3d(k),up,1)
        call get_3d(nf_up2,k,varname3d(k),up,2)
        enddo
! MEAN up - low data extrapolated
        do k=nbvarlow+1,nbvar3d
        call get_3d(nf_up,k,varname3d(k),up,1)
        call get_3d(nf_up2,k,varname3d(k),up,2)
        call extrapol3d(type,k,low)
        enddo

      elseif (type.eq.'hybr') then

        name='aps'
        ierr = NF_INQ_VARID(nf_up,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf_up,varid,aps_up)
        if (ierr.ne.nf_noerr) goto 9999
! if low atmosphere, need aps and bps
        name='aps'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,aps_low)
        if (ierr.ne.nf_noerr) goto 9999
        name='bps'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,bps_low)
        if (ierr.ne.nf_noerr) goto 9999

        do l=1,low
        aps(l)=aps_low(l)
        bps(l)=bps_low(l)
        enddo
        do l=low+1,dimlevs
        aps(l)=aps_up(l-low)
        bps(l)=0.
        enddo
!       pseudoalt
        do l=1,dimlevs
        pseudoalt(l)=-10.*log(aps(l)/610.+bps(l))
        enddo


!       sigma Thermosphere :
        sigma(dimlevs+1) = sigma(dimlevs)*exp(-20./7.)

      elseif (type.eq.'orog') then

        name='orography'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon
           taborog(iloop,jloop,1)=temp2d(iloop,dimlat+1-jloop)
          enddo
        enddo
      elseif (type.eq.'grwp') then

        name='substd'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon
           tabsubstd(iloop,jloop,1)=temp2d(iloop,dimlat+1-jloop)
          enddo
        enddo
      elseif (type.eq.'stdv') then

! STD 2d
        do k=1,nbsd2d
        call getsd_2d(nf,type,k,"tsd"//sd2d(k),1)
        call getsd_2d(nf2,type,k,"tsd"//sd2d(k),2)
        enddo
! STD low UP extrapolated
!      call getsd_3d(nf,type,1,nbsdlow-nbcomsd,"tsd"//sd3d(k),
!     $ low)
! STD up and low
        do k=nbsdlow-nbcomsd+1,nbsdlow
        call getsd_3d(nf,type,k,"tsd"//sd3d(k),low,1)
        call getsd_3d(nf2,type,k,"tsd"//sd3d(k),low,2)
        call getsd_3d(nf_up,type,k,"tsd"//sd3d(k),up,1)
        call getsd_3d(nf_up2,type,k,"tsd"//sd3d(k),up,2)
        enddo
! STD up - low data extrapolated
!        WARNING : uncomment these lines if to have a sd only in the
!        low atm (not the case in version 4)
!        do k=nbsdlow+1,nbsd3d
!          call getsd_3d(nf_up,type,k,"tsd"//sd3d(k),up,1)
!          call getsd_3d(nf_up2,type,k,"tsd"//sd3d(k),up,2)
!          call extrapol3d(type,k,low)
!        enddo

      elseif (type.eq.'rms') then
! RMS 2d
        do k=1,nbsd2d
        call getsd_2d(nf,type,k,"rms"//sd2d(k),1)
        call getsd_2d(nf2,type,k,"rms"//sd2d(k),2)
        enddo
! RMS low - up data extrapolated
!      call getsd_3d(nf,type,1,nbsdlow-nbcomsd,"tsd"//sd3d(k),
!     $ low)
! RMS up and low
        do k=nbsdlow-nbcomsd+1,nbsdlow
        call getsd_3d(nf,type,k,"rms"//sd3d(k),low,1)
        call getsd_3d(nf2,type,k,"rms"//sd3d(k),low,2)
        call getsd_3d(nf_up,type,k,"rms"//sd3d(k),up,1)
        call getsd_3d(nf_up2,type,k,"rms"//sd3d(k),up,2)
        enddo
! RMS up - low data extrapolated
!        WARNING : uncomment these lines if to have a rms only in the
!        low atm (not the case in version 4)
!        do k=nbsdlow+1,nbsd3d
!         call getsd_3d(nf_up,type,k,"rms"//sd3d(k),up,1)
!         call getsd_3d(nf_up2,type,k,"rms"//sd3d(k),up,2)
!         call extrapol3d(type,k,low)
!        enddo

      endif!type rms

      return

!     Error handling
 9999 ier=1
      write(0,*)"LOADVAR Error : impossible to load ", &
           name," from ",type, "value file"
      return
      end subroutine  loadvar

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine loadeof(nf,ier)

!     load EOF arrays
      implicit none

!     include "netcdf.inc"
!#include "constants_atmemcd_41.h"

!     inputs
      integer ::   nf !NETCDF channel number
!     outputs
      integer ::      ier                 !error flag (0=OK, 1=NOK)

      character (LEN=16) :: name
      integer :: ierr, varid

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

         name='temp'
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

        return
!     Error handling
 9999 ier=1
      write(0,*)"LOADEOF Error : impossible to load ", &
           name," from eof file"
      return

        end subroutine  loadeof
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ltime(lon,sunlon,localtime)

!     Compute local time at longitude=longitude when the sun is
!     at longitude=sunlon.

      implicit none

!     inputs
      real lon !east longitude
      real sunlon    !west longitude of sun

!     outputs
      real localtime !local time

!     local
      real lonw !west longitude

! converting lon east (deg) in lon west (deg)
      lonw=mod(lon,360.)
      if(lon.lt.0.) then
         lonw=-lon
      elseif (lon.gt.0.) then
         lonw=360.-lon
      endif

      localtime=12.+(sunlon-lonw)/15.
      if (localtime.lt.0.) localtime=localtime+24.
      if (localtime.ge.24.) localtime=localtime-24.

      return
      end subroutine  mars_ltime

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ptime(lon,lt,ut)

!     Convert local time, lt (0..24) at west longitude=lonw to
!     universal time  ut (0..24).

      implicit none

!     inputs
      real lon    !longitude east
      real lt     !local time

!     output
      real ut     !universal time  ( =local time at lon=0)

!     local
      real lonw   !longitude west

! converting lon east (deg) in lon west (deg)
      lonw=mod(lon,360.)
      if(lon.lt.0.) then
         lonw=-lon
      elseif (lon.gt.0.) then
         lonw=360.-lon
      endif

      ut=mod((lonw/15.+lt+24.),24.)

      return
      end subroutine  mars_ptime

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mcd_time(ut,itime,w)

!     From universal time ut (0..24 hrs) , find the
!     2 nearest timestep in the database (1 - 12)
!     and give the linear interpolation weight of itime(1)

!     Note that it is midnight universal time (and thus at longitude=0.0)
!     at itime = 12

      implicit none

!     input
      real ut       !universal time (0. to 24. hrs) =local time at lon=0

!     outputs
      integer ::  itime(2)     ! 2 nearest timestep in database
      real  w            ! linear interpolation weight of itime(1)

      itime(1) = int(ut/2.)
      if (itime(1).eq.0) itime(1) = 12
      itime(2) = itime(1) +1
      if (itime(2).ge.13) itime(2) = itime(2) -12

      w = 1. - ut/2. + real(int(ut/2.))

      return
      end subroutine  mcd_time

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine opend(unet,unetm,unetsd,unetm_up,unetsd_up,ueof, &
                  num,dust,dset,ier)

!     Open the appropriate NETCDF file corresponding to Ls, dust scenario
!     and data set.

      implicit none

!     include "netcdf.inc"

!     inputs
! NETCDF channels number
      integer ::       unet        !for mountain fields
      integer ::       unetm       !for mean fields
      integer ::       unetsd      !for STD and RMS deviation fields
      integer ::       unetm_up    !for mean fields in thermosphere
      integer ::       unetsd_up   !for STD and RMS fields in thermosphere
      integer ::       ueof        !for EOF fields

      integer ::       num         !season num
      integer ::       dust        !Dust senario
      character (LEN=*) :: dset        !Dataset

!     output
      integer ::       ier         !Error flag (0=OK, 1=NOK)

!     local variables
      integer ::       ierr
      character (LEN=255) :: datfile
      character (LEN=4) ::   scen
      character (LEN=3) ::   solar
      character (LEN=2) ::   saison
      character (LEN=2) ::   type

      ier=0

!     close files
      ierr=NF_CLOSE(unetm)
      ierr=NF_CLOSE(unetsd)
      ierr=NF_CLOSE(unetm_up)
      ierr=NF_CLOSE(unetsd_up)
      ierr=NF_CLOSE(ueof)
      ierr=NF_CLOSE(unet)

!     dust scenario
      if (dust.eq.1.or.dust.eq.2.or.dust.eq.3) then
         scen='MY24'
         if (dust.eq.1) solar='min'
         if (dust.eq.2) solar='ave'
         if (dust.eq.3) solar='max'
      elseif (dust.eq.4.or.dust.eq.5.or.dust.eq.6) then
         scen='strm'
         if (dust.eq.4) solar='min'
         if (dust.eq.5) solar='ave'
         if (dust.eq.6) solar='max'
      elseif (dust.eq.7) then
         scen='warm'
         solar='max'
      elseif (dust.eq.8) then
         scen='cold'
         solar='min'
      else
         write(0,*) 'pb in opend with dust= ', dust
         stop
      endif

!     season number
      write(saison,'(i2.2,1x)') num
!     write(*,*) 'saison ' ,saison
!     write(*,*) 'scen ' , scen
!     write(*,*) 'dust ' , dust

!     mean file
      type='me'
      datfile=dset//scen//'_'//saison//'_'//type//'.nc'
!     write (*,'(a)') "Opening "//trim(datfile)
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm)
      if (ierr.ne.NF_NOERR) goto 9999

!     standard deviation file
      type='sd'
      datfile=dset//scen//'_'//saison//'_'//type//'.nc'
!      write (*,'(a)') "Opening "//trim(datfile)
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd)
      if (ierr.ne.NF_NOERR) goto 9999

!     thermo Mean file
      type='me'
      datfile=dset//scen//'_'//saison// &
       '_thermo_'//solar//'_'//type//'.nc'
!      write (*,'(a)') "Opening "//trim(datfile)
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm_up)
      if (ierr.ne.NF_NOERR) goto 9999

!     thermo standard deviation file
      type='sd'
      datfile=dset//scen//'_'//saison// &
       '_thermo_'//solar//'_'//type//'.nc'
!      write (*,'(a)') "Opening "//trim(datfile)
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd_up)
       if (ierr.ne.NF_NOERR) goto 9999

!     eof file
      type='eo'
      datfile=dset//scen//'_all_'//solar//'_eo.nc'
!      write (*,'(a)') "Opening "//trim(datfile)
      ierr=NF_OPEN(datfile,NF_NOWRITE,ueof)
      if (ierr.ne.NF_NOERR) goto 9999

!     mountain file
      datfile=dset//'mountain.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unet)
      if (ierr.ne.NF_NOERR) goto 9999

      return

!     Error handling
 9999 ier=1
      write(0,*)"OPEND Error : impossible to open "
      write(0,*) trim(datfile)
      return
      end subroutine  opend
!
      subroutine orbit (date,sunlat,sunlon,ls,marsau,outmodelday)
!***********************************************************************
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!     orbit                                                            *
!                                                                      *                                                    *
!$Resume                                                               *
!     Pour une date julienne, resolution de l'equation du temps        *
!     pour le calcul des longitude et latitude de mars dans le repere  *
!     solaire et de la distance solei / mars                           *
!                                                                      *
!$Version                                                              *
!                                                                      *
!$Auteur                                                               *
!                                                                      *
!$Usage                                                                *
!     subroutine orbit (date,sunlat,sunlon,ls,marsau,outmodelday)      *
!                                                                      *
!$Arguments                                                            *
!>E date        : <double precision> date julienne                     *
!>S sunlat      : <double precision> latitude de mars / soleil (deg)   *
!>S sunlon      : <double precision> longitude solaire / soleil (deg)  *
!>S ls          : <double precision> longitude solaire (deg)           *
!>S marsau      : <double precision> distance soleil / mars (UA)       *
!>S outmodelday : <double precision> date marsienne                    *
!***********************************************************************
!
      implicit none
!
!     input
      real (KIND=8) ::     date   !Julian date
!
!     outputs
      real       sunlat !subsolar latitude
      real       sunlon !subsolar longitude
      real       ls     !Ls
      real       marsau !Sun-Mars distance in AU
      real       outmodelday
!
!     local variables
      real (KIND=8) ::     modelday
      real (KIND=8) ::     marsday
      parameter (marsday=88775.245d0)
      real (KIND=8) ::     earthday
      parameter (earthday=86400.d0)
      real (KIND=8) ::     mdoffset
      parameter (mdoffset=222.4d0)
      real       slonoff
      parameter (slonoff=197.2)
      real       inclin
      parameter (inclin=25.3)
      real       pi
      parameter (pi=3.14159)
      real       dummy
!***********************************************************************
!     convert Julian day to model day                                  *
!***********************************************************************
      modelday    = date*earthday/marsday
      modelday    = mod(modelday+mdoffset,669.d0)
      dummy       = real(modelday)
      outmodelday = real(modelday)
!***********************************************************************
! calcul de la longitude et latitude solaire                           *
!***********************************************************************
      call sol2ls (dummy,ls)
!
      sunlon = 360.*(dummy-real(int(dummy)))+slonoff
      sunlon = mod(sunlon,360.)
      sunlat = inclin*sin(ls*pi/180.)
      marsau = 1.5237*7.73396*((1.-0.934**2)) &
              /(1.+0.0934*cos((ls+109.)*pi/180.))
!
      return
      end subroutine  orbit

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine profi(a,lon,lat,utime,name,ier,itimint,wl,wh)

!     Retrieve a profile of 3-d variable=name at east longitude=lon, latitude=lat
!     and universal time=utime.

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real         lon        !east longitude of point
      real         lat        !latitude of point
      real         utime      !Universal time (0. to 24. hrs)=local time at lon=0
      character (LEN=16) :: name       !Name of variable
      integer ::      itimint    !seasonal interpolation flag
      real         wl,wh      !seasonal interpolation weightings

!     outputs
      integer ::      ier        !error flag (0=OK, 1=NOK)
      real         a(dimlevs) !Profile
      real         a2(dimlevs)

!     local variables
      integer ::      i,j,l,k,sd,rms
      integer ::      iut, itime(2)
      integer ::      dlon(4)
      integer ::      dlat(4)
      real         y(dimlevs,4), x(dimlevs,2)
      real         y2(dimlevs,4), x2(dimlevs,2)
      real         t,u,w

!     flags sd and rms initialized to 999
      sd=999
      rms=999
      k=0
      ier=0

!     Associate numbers with variable name to read loaded variable
!     ------------------------------------------------------------

!!!!!!!! MEAN
      if (name.eq.'q2') then
      k=1
      sd=0
      elseif (name.eq.'vmr_h2ovapor') then
      k=2
      sd=0
      elseif (name.eq.'vmr_h2oice') then
      k=3
      sd=0
      elseif (name.eq.'vmr_o3') then
      k=4
      sd=0
      elseif (name.eq.'temp') then
      k=5
      sd=0
      elseif (name.eq.'u') then
      k=6
      sd=0
      elseif (name.eq.'v') then
      k=7
      sd=0
      elseif (name.eq.'rho') then
      k=8
      sd=0
      elseif (name.eq.'w') then
      k=9
      sd=0
      elseif (name.eq.'vmr_o') then
      k=10
      sd=0
      elseif (name.eq.'vmr_co2') then
      k=11
      sd=0
      elseif (name.eq.'vmr_co') then
      k=12
      sd=0
      elseif (name.eq.'vmr_n2') then
      k=13
      sd=0
!     elseif (name.eq.'vmr_h2') then
!     k=14
!     sd=0
!!!!!!!! SDT
      elseif (name.eq.'tsdtemp') then
      k=1
      sd=1
      elseif (name.eq.'tsdu') then
      k=2
      sd=1
      elseif (name.eq.'tsdv') then
      k=3
      sd=1
      elseif (name.eq.'tsdrho') then
      k=4
      sd=1
      elseif (name.eq.'tsdw') then
      k=5
      sd=1
!!!!!!!! RMS
      elseif (name.eq.'rmstemp') then
      k=1
      rms=1
      elseif (name.eq.'rmsu') then
      k=2
      rms=1
      elseif (name.eq.'rmsv') then
      k=3
      rms=1
      elseif (name.eq.'rmsrho') then
      k=4
      rms=1
      elseif (name.eq.'rmsw') then
      k=5
      rms=1
      else
!        CASE of an unexpected name
         write(0,*) 'problem using subroutine profi : the name ',name
         write(0,*) 'is not recognized'
         stop
      endif

!     Retrieving variable for  "lower" season
!     ------------------------------------------------
!     find nearest 4 grid points
      call grid4(lon,lat,dlon,dlat,t,u)

!     find nearest 2 timestep :
      call mcd_time(utime,itime,w)

!     initialization
      do i=1,4
        do l=1,dimlevs
           y(l,i)=0.
        enddo
      enddo
!     loop on the 2 nearest timestep :
      do j = 1 , 2
         iut = itime(j)
!        get 4 profiles at those points
!    MEAN variables
       if (sd.eq.0) then
            do i=1,4
               do l=1,dimlevs
               y(l,i)=var_3d(dlon(i),dlat(i),l,iut,k)
               enddo
            enddo
          endif!sd=0
!    STD variables
       if (sd.eq.1) then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=varsd3d(dlon(i),dlat(i),l,k)
               enddo
            enddo
       endif!sd=1
!    RMS variables
       if (rms.eq.1) then
           do i=1,4
              do l=1,dimlevs
                 y(l,i)=varrms3d(dlon(i),dlat(i),l,k)
              enddo
           enddo
       endif

!    bilinear interpolation
       do l=1,dimlevs
           x(l,j)=(1.-t)*(1.-u)*y(l,1) &
                +t*(1.-u)*y(l,2) &
                +t*u*y(l,3) &
                +(1.-t)*u*y(l,4)
       enddo
      end do!2 nearest timestep

!    linear interpolation in time :
      do l=1,dimlevs
        a(l) = w*x(l,1) + (1.-w)*x(l,2)
!      if(k.eq.4) write(*,*)'l,t ',l,a(l)
      enddo

!     Retrieving variable for "higher" season (if seasonal interpolation)
!     --------------------------------------
!     For seasonal interpolation
!     loop on the 2 nearest timestep :

      if (itimint.ge.1) then
         do j = 1,2
            iut = itime(j)

!        get 4 profiles at those points
!        MEAN variables
          if (sd.eq.0) then
            do i=1,4
               do l=1,dimlevs
                 y2(l,i)=var_3d2(dlon(i),dlat(i),l,iut,k)
               enddo
            enddo
          endif!sd=0
!  STD variables
          if (sd.eq.1) then
            do i=1,4
               do l=1,dimlevs
                  y2(l,i)=varsd3d2(dlon(i),dlat(i),l,k)
               enddo
            enddo
          endif!sd=1
!  RMS variables
          if (rms.eq.1) then
            do i=1,4
               do l=1,dimlevs
                  y2(l,i)=varrms3d2(dlon(i),dlat(i),l,k)
               enddo
            enddo
          endif

!        bilinear interpolation
         do l=1,dimlevs
           x2(l,j)=(1.-t)*(1.-u)*y2(l,1) &
                +t*(1.-u)*y2(l,2) &
                +t*u*y2(l,3) &
                +(1.-t)*u*y2(l,4)
         enddo
      end do

!     Linear interpolation in time :
      do l=1,dimlevs
        a2(l) = w*x2(l,1) + (1.-w)*x2(l,2)

!     Season interpolation between the two seasons
!     --------------------------------------------
        a(l) = (wl)*a(l) + (wh)*a2(l)
      enddo

      endif!if itimint=1 (seasonal interpolation)

      return

      end subroutine

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season (ls,numsaison)
!
!***********************************************************************
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!      season                                                          *
!                                                                      *
!$Resume                                                               *
!     Calcul le numero du mois depuis la longitude solaire             *
!                                                                      *
!$Version                                                              *
!                                                                      *
!$Auteur                                                               *
!                                                                      *
!$Usage                                                                *
!     subroutine  season (ls,numsaison)                                *
!                                                                      *
!$Arguments                                                            *
!>E ls        : <double precision> longitude solaire (deg)             *
!>S numsaison : <integer>          numero du mois                      *
!***********************************************************************
!
      implicit none
!     input
      real    ls
!     output
      integer :: numsaison
!***********************************************************************
      if ((ls.ge.0.0).and.(ls.le.30.)) then
         numsaison=1
      endif
      if ((ls.ge.30.0).and.(ls.le.60.)) then
         numsaison=2
      endif
      if ((ls.ge.60.0).and.(ls.le.90.)) then
         numsaison=3
      endif
      if ((ls.ge.90.0).and.(ls.le.120.)) then
         numsaison=4
      endif
      if ((ls.ge.120.0).and.(ls.le.150.)) then
         numsaison=5
      endif
      if ((ls.ge.150.0).and.(ls.le.180.)) then
         numsaison=6
      endif
      if ((ls.ge.180.0).and.(ls.le.210.)) then
         numsaison=7
      endif
      if ((ls.ge.210.0).and.(ls.le.240.)) then
         numsaison=8
      endif
      if ((ls.ge.240.0).and.(ls.le.270.)) then
         numsaison=9
      endif
      if ((ls.ge.270.0).and.(ls.le.300.)) then
         numsaison=10
      endif
      if ((ls.ge.300.0).and.(ls.le.330.)) then
         numsaison=11
      endif
      if ((ls.ge.330.0).and.(ls.le.360.)) then
         numsaison=12
      endif
!***********************************************************************
      return
      end subroutine season

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season2(ls,numsaison,nums2,wl,wh,dust)

!     compute the 2 season numbers(nums,nums2) from areocentric longitude
!     for when itimint = 1, ie for seasonal interpolation
!     wl= lower season weighting, wh=higher season weighting

      implicit none

!     inputs
      real    ls
      integer :: dust

!     outputs
      integer :: numsaison,nums2
      real    wl,wh

!     local variable
      real    lsc

!     compute lower season
      numsaison=int((ls/30)+0.5)
      if (numsaison.eq.0) numsaison=12
!     compute higher season
      nums2=numsaison+1
      if (nums2.eq.13) nums2=1
!     compute centre of season to find weightings
      lsc=real((numsaison-1)*30)+15.
      if (ls.lt.15.) lsc=-15.
!     compute weightings
      wh=(ls-lsc)/30.
      wl=1.-wh

!     Dust storm scenarion Special case
      if ((dust.eq.4).or.(dust.eq.5).or.(dust.eq.6))then
          if(numsaison.eq.6) numsaison=7
          if(nums2.eq.1) nums2=12
      endif

      return
      end subroutine  season2

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine sol2ls(sol,ls)

!  convert day number, sol, to solar longitude, Ls, where sol=0 is the
!  spring equinox.

      implicit none

!     input
      real    sol
!     output
      real    ls

!     local
      real    year_day,peri_day,timeperi,e_elips,pi,degrad
      data    year_day /668.6/
      data    peri_day /485.0/
      data    timeperi /1.905637/
      data    e_elips  /0.093358/
      data    pi       /3.1415927/
      data    degrad   /57.295779/
      real    zanom,xref,zx0,zdx,zteta,zz
      integer :: iter

      zz=(sol-peri_day)/year_day
      zanom=2.*pi*(zz-real(nint(zz)))
      xref=abs(zanom)

!  The equation zx0 - e * sin (zx0) = xref, solved by Newton
      zx0=xref+e_elips*sin(xref)
      do iter=1,10
         zdx=-(zx0-e_elips*sin(zx0)-xref)/(1.-e_elips*cos(zx0))
         if(abs(zdx).le.(1.e-7)) goto 120
         zx0=zx0+zdx
      enddo
  120 continue
      zx0=zx0+zdx
      if(zanom.lt.0.) zx0=-zx0

      zteta=2.*atan(sqrt((1.+e_elips)/(1.-e_elips))*tan(zx0/2.))
      ls=zteta-timeperi
      if(ls.lt.0.) ls=ls+2.*pi
      if(ls.gt.2.*pi) ls=ls-2.*pi
      ls=degrad*ls

      return
      end subroutine  sol2ls

!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real function ls2sol(ls)

!  Convert solar longitude, Ls, to day number, sol, where sol=0 is the
!  spring equinox.

      implicit none

!  Arguments:
      real ls

!  Local:
      real xref,zx0,zteta,zz

      real year_day
      real peri_day
      real timeperi
      real e_elips
      real pi
      real degrad
      data year_day /668.6/
      data peri_day /485.0/
      data timeperi /1.905637/
      data e_elips  /0.093358/
      data pi       /3.1415927/
      data degrad   /57.295779/

      if (abs(ls).lt.1.0e-5) then
         if (ls.ge.0.0) then
            ls2sol = 0.0
         else
            ls2sol = year_day
         end if
         return
      end if

      zteta = ls/degrad + timeperi
      zx0 = 2.0*atan(tan(0.5*zteta)/sqrt((1.+e_elips)/(1.-e_elips)))
      xref = zx0-e_elips*sin(zx0)
      zz = xref/(2.*pi)
      ls2sol = zz*year_day + peri_day
      if (ls2sol.lt.0.0) ls2sol = ls2sol + year_day
      if (ls2sol.ge.year_day) ls2sol = ls2sol - year_day

      return
      end function  ls2sol

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var2d(a,lon,lat,utime,name,ier,itimint,wl,wh)

!     Retrieve the value of 2-d variable=name at longitude=lon, latitude=lat
!     and universal time=utime. Use bilinear interpolation to get user
!     specified longitude and latitude and time.
!     Use linear seasonal interpolation.  wl=lower season weighting,
!     wh=higher season weighting.

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real         lon    !longitude (east) of point
      real         lat     !latitude of point
      real         utime   !Universal time (0. to 24. hrs) =local time at lon=0
      character (LEN=16) :: name    !Name of variable
      integer ::      itimint !seasonal interpolation flag
      real         wl,wh   !seasonal interpolation weightings

!     outputs
      integer ::      ier     !error flag (0=OK, 1=NOK)
      real         a       !value of variable
      real         a2      !seasonal interpolation variable

!     local variables
      integer ::      i, j, k, sd, rms
      integer ::      iut,itime(2)
      integer ::      dlon(4)
      integer ::      dlat(4)
      real         y(4),x(2)
      real         t,u,w
      real         y2(4),x2(2)

      ier = 0
      k=0
      y(:) = 0.
      y2(:) = 0.

!     find nearest 4 grid points
      call grid4(lon,lat,dlon,dlat,t,u)
!     find nearest 2 timestep :
      call mcd_time(utime,itime,w)
!     flags sd and rms initialized to 999
      k=0
      sd=999
      rms=999
!     Associate numbers with variable name to read loaded variable
!     ------------------------------------------------------------
      if (name.eq.'tsurf') then
      k=1
      sd=0
      elseif (name.eq.'ps') then
      k=2
      sd=0
      elseif (name.eq.'co2ice') then
      k=3
      sd=0
      elseif (name.eq.'fluxsurf_lw') then
      k=4
      sd=0
      elseif (name.eq.'fluxsurf_sw') then
      k=6
      sd=0
      elseif (name.eq.'fluxtop_lw') then
      k=5
      sd=0
      elseif (name.eq.'fluxtop_sw') then
      k=7
      sd=0
      elseif (name.eq.'dod') then
      k=8
      sd=0
      elseif (name.eq.'col_h2ovapor') then
      k=9
      sd=0
      elseif (name.eq.'col_h2oice') then
      k=10
      sd=0
      else if (name.eq.'tsdps') then
      k=2
      sd=1
      elseif (name.eq.'tsdtsurf') then
      k=1
      sd=1
      elseif (name.eq.'tsddod') then
      k=3
      sd=1
      else if (name.eq.'rmsps') then
      k=2
      rms=1
      elseif (name.eq.'rmstsurf') then
      k=1
      rms=1
      elseif (name.eq.'rmsdod') then
      k=3
      rms=1
      else  if(name.ne.'orography'.and.name.ne.'substd') then
!        CASE of an unexpected name (orography and substd are traeated below)
         write(0,*) 'problem using subroutine var2d : the name ',name
         write(0,*) 'is not recognized'
         stop
      endif

!     Retrieving variable for "lower" season
!     --------------------------------------
!     loop on the 2 nearest timestep :
      do j = 1 , 2
         iut = itime(j)

!     retrieve the four values at the nearest grid points from the array
          if (name.eq.'orography') then
            do i=1,4
               y(i)=taborog(dlon(i),dlat(i),1)
            enddo
          elseif (name.eq.'substd') then
            do i=1,4
               y(i)=tabsubstd(dlon(i),dlat(i),1)
            enddo
          elseif (sd.eq.0) then
            do i=1,4
               y(i)=var_2d(dlon(i),dlat(i),iut,k)
            enddo
          elseif (sd.eq.1) then
            do i=1,4
               y(i)=varsd2d(dlon(i),dlat(i),k)
            enddo
          elseif (rms.eq.1) then
            do i=1,4
               y(i)=varrms2d(dlon(i),dlat(i),k)
            enddo
          endif

!     bilinear interpolation in space
         x(j)=(1.-t)*(1.-u)*y(1) &
          +t*(1.-u)*y(2) &
          +t*u*y(3) &
          +(1.-t)*u*y(4)
      end do

!     linear interpolation in time:
      a = w*x(1) + (1.-w)*x(2)

!     Retrieving variable for "higher" season (if seasonal interpolation)
!     --------------------------------------
!     retrieve the four values at the nearest grid points from the array
!     for seasonal interpolation
      if (itimint.ge.1) then

!     loop on the 2 nearest timestep :
        do j = 1 , 2
            iut = itime(j)

!     retrieve the four values at the nearest grid points from the array
           if (name.eq.'orography') then
           do i=1,4
              y2(i)=taborog(dlon(i),dlat(i),1)
           enddo
           elseif (name.eq.'substd') then
           do i=1,4
              y2(i)=tabsubstd(dlon(i),dlat(i),1)
           enddo
           elseif (sd.eq.0) then
           do i=1,4
              y2(i)=var_2d2(dlon(i),dlat(i),iut,k)
           enddo
           elseif (sd.eq.1) then
           do i=1,4
              y2(i)=varsd2d2(dlon(i),dlat(i),k)
           enddo
           elseif (rms.eq.1) then
           do i=1,4
              y2(i)=varrms2d2(dlon(i),dlat(i),k)
           enddo
           endif

!     bilinear interpolation in space for higher season
         x2(j)=(1.-t)*(1.-u)*y2(1) &
          +t*(1.-u)*y2(2) &
          +t*u*y2(3) &
          +(1.-t)*u*y2(4)
        end do
!       Linear interpolation in utime for higher season
        a2 = w*x2(1) + (1.-w)*x2(2)

!     Season interpolation between the two seasons
!     --------------------------------------------
         a = wl*a + wh*a2
      endif
      return

      end subroutine

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine interpol(k,up_low,type)

!     calculate the vertical interpolation between low and up atmosphere
!     when data are not in both files (low and thermo)

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::    k,up_low
      character (LEN=4) :: type

!     local variables
      integer ::     startk,endk
      integer ::     iloop,kloop,jloop,it

      if (up_low.eq.up) then
      startk=low+1
      endk=low+3
      else
      startk=low-2
      endk=low
      endif

!     3D extrapolation for the first 3 layers of the thermosphere
      if (type.eq.'mean') then
       do it=1,dimuti
        if (k.ne.10) then
         do kloop=startk,endk
           do jloop=1,dimlat
            do iloop=1,dimlon
       var_3d(iloop,jloop,kloop,it,k)=var_3d(iloop,jloop,startk-1,it,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (var_3d(iloop,jloop,endk+1,it,k)- &
       var_3d(iloop,jloop,startk-1,it,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))

       var_3d2(iloop,jloop,kloop,it,k)= &
       var_3d2(iloop,jloop,startk-1,it,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (var_3d2(iloop,jloop,endk+1,it,k)- &
       var_3d2(iloop,jloop,startk-1,it,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))
            enddo
           enddo
          enddo
      elseif(k.eq.10) then   ! "o" case
         do kloop=2,30      ! interpolation for the all low atmosphere
           do jloop=1,dimlat
            do iloop=1,dimlon
        var_3d(iloop,jloop,kloop,it,k)= &
       exp( &
       log(var_3d(iloop,jloop,1,it,k))+ &
       (pseudoalt(kloop)-pseudoalt(1))* &
       (log(max(var_3d(iloop,jloop,31,it,k),1.E-30))- &
       log(var_3d(iloop,jloop,1,it,k))) &
       /(pseudoalt(31)-pseudoalt(1)) &
       )

       var_3d2(iloop,jloop,kloop,it,k)= &
       exp( &
       log(var_3d2(iloop,jloop,1,it,k))+ &
       (pseudoalt(kloop)-pseudoalt(1))* &
       (log(max(var_3d2(iloop,jloop,31,it,k),1.E-30))- &
       log(var_3d2(iloop,jloop,1,it,k))) &
       /(pseudoalt(31)-pseudoalt(1)) &
       )

            enddo !iloop
           enddo !jloop
          enddo !kloop
         endif! "o" case
        enddo
      elseif (type.eq.'rms'.or.type.eq.'stdv') then
!    2D extrapolation for the first 3 layers of the thermosphere
         do kloop=startk,endk
           do jloop=1,dimlat
            do iloop=1,dimlon
              if(type.eq.'rms')  then
       varrms3d(iloop,jloop,kloop,k)= &
       varrms3d(iloop,jloop,startk-1,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (varrms3d(iloop,jloop,endk+1,k)- &
       varrms3d(iloop,jloop,startk-1,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))

       varrms3d2(iloop,jloop,kloop,k)= &
       varrms3d2(iloop,jloop,startk-1,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (varrms3d2(iloop,jloop,endk+1,k)- &
       varrms3d2(iloop,jloop,startk-1,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))
               elseif(type.eq.'stdv') then
       varsd3d(iloop,jloop,kloop,k)= &
       varsd3d(iloop,jloop,startk-1,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (varsd3d(iloop,jloop,endk+1,k)- &
       varsd3d(iloop,jloop,startk-1,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))

       varsd3d2(iloop,jloop,kloop,k)= &
       varsd3d2(iloop,jloop,startk-1,k)+ &
       (pseudoalt(kloop)-pseudoalt(startk-1))* &
       (varsd3d2(iloop,jloop,endk+1,k)- &
       varsd3d2(iloop,jloop,startk-1,k)) &
       /(pseudoalt(endk+1)-pseudoalt(startk-1))
               endif
            enddo
           enddo
          enddo
       endif!type
      end subroutine  interpol

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine extrapol3d(type,k,up_low)

!  extrapolate 3d data when not in the initial netcdf files
!  call subroutine interpol

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::     k,up_low
      character (LEN=4) :: type

!     local variables
      integer ::     iloop,kloop,jloop,it
      real        var_ini(nbvarup-nbcom)

!     initial values in low atmosphere
      var_ini(1)=0.2E-11!o : only for surface
      var_ini(2)=95.32E-2 !co2
      var_ini(3)=8.E-4 !co
      var_ini(4)=0.027!n2

      if (up_low.eq.up) then !extrapolation in the thermosphere

       if (type.eq.'mean') then
        do it=1,dimuti
         do kloop=low+4,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon
                 var_3d(iloop,jloop,kloop,it,k)=0.
                 var_3d2(iloop,jloop,kloop,it,k)=0.
            enddo
           enddo
          enddo
         enddo

       elseif (type.eq.'stdv'.or.type.eq.'rms')    then
         do kloop=low+4,dimlevs
          do jloop=1,dimlat
           do iloop=1,dimlon
                      if (type.eq.'stdv') then
                  varsd3d(iloop,jloop,kloop,k)=0.
                  varsd3d2(iloop,jloop,kloop,k)=0.
                      elseif(type.eq.'rms') then
                  varrms3d(iloop,jloop,kloop,k)=0.
                  varrms3d2(iloop,jloop,kloop,k)=0.
                      endif
            enddo
           enddo
          enddo

       endif

! extrapolation for the first 3 layers of the thermosphere
! *********************************************************
      call  interpol(k,up,type)

      else  ! extrapolation in the lower atmosphere

       if (type.eq.'mean') then
         do it=1,dimuti
          do kloop=1,low-3
           do jloop=1,dimlat
            do iloop=1,dimlon
               var_3d(iloop,jloop,kloop,it,k)=var_ini(k-nbvarlow)
               var_3d2(iloop,jloop,kloop,it,k)=var_ini(k-nbvarlow)
             enddo
            enddo
           enddo
           enddo

       elseif (type.eq.'stdv'.or.type.eq.'rms')    then
         do kloop=1,low-3
           do jloop=1,dimlat
            do iloop=1,dimlon
                      if (type.eq.'stdv') then
               varsd3d(iloop,jloop,kloop,k)=0.
               varsd3d2(iloop,jloop,kloop,k)=0.
                      elseif(type.eq.'rms') then
               varrms3d(iloop,jloop,kloop,k)=0.
               varrms3d2(iloop,jloop,kloop,k)=0.
                      endif
            enddo
           enddo
          enddo
        endif

! extrapolation for the last 3 layers of the low atmosphere
! *********************************************************
       call  interpol(k,low,type)

       endif! up_low

      return
      end subroutine  extrapol3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_2d(nf,k,varname,order)

!  get_2d reads the 2d mean variables in low netcdf files
!  results in var_2d (defined in constants_atmemcd_41.h)

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::      nf,k,order
      character (LEN=50) :: varname

!     local variables
      integer ::      ierr,iloop,jloop,it
      integer ::      var2didin(nbvar2d)
      real         temp2d(dimlon,dimlat,dimuti)

!     write(*,*),'varname(k) k',k,varname
!     NETCDF reading 2d mean variable
         ierr=NF_INQ_VARID(nf,varname,var2didin(k))
         if(ierr.ne.NF_NOERR) then
           write(0,*) NF_STRERROR(ierr), varname
           stop
         endif
         ierr = NF_GET_VAR_REAL(nf,var2didin(k),temp2d)
         if(ierr.ne.NF_NOERR) then
            write(0,*) NF_STRERROR(ierr)
            stop "get2d"
         endif

         do it=1,dimuti
           do jloop=1,dimlat
            do iloop=1,dimlon
                  if (order.eq.1) then
                  var_2d(iloop,jloop,it,k)= &
                  temp2d(iloop,dimlat+1-jloop,it)
                  elseif (order.eq.2) then
                  var_2d2(iloop,jloop,it,k)= &
                  temp2d(iloop,dimlat+1-jloop,it)
                  endif
            enddo
           enddo
          enddo

      return
      end subroutine  get_2d

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_3d(nf,k,varname,up_low,order)

!  getsd_3D reads the 3D mean variables in up or low netcdf files

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::      ierr,nf,k,up_low,order
      character (LEN=50) :: varname

!     local variables
      integer ::      iloop,kloop,jloop,it
      integer ::      var3didin(nbvar3d)
      real         temp3dlow(dimlon,dimlat,low,dimuti)
      real         temp3dup(dimlon,dimlat,up,dimuti)

!     write(*,*),'varname(k) k',k,varname
      ierr=NF_INQ_VARID(nf,varname,var3didin(k))
!         ierr=NF_INQ_VARID(nf,varname,var3didin2(k))
      if(ierr.ne.NF_NOERR) then
         write(0,*) NF_STRERROR(ierr),varname
          stop
      endif
      if (up_low.eq.up) then
         ierr = NF_GET_VAR_REAL(nf,var3didin(k),temp3dup)
         if(ierr.ne.NF_NOERR) then
            write(0,*) NF_STRERROR(ierr)
            stop "get3d"
         endif

         do it=1,dimuti
          do kloop=low+1,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon
             if (order.eq.1) then
                  var_3d(iloop,jloop,kloop,it,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,it)
             elseif(order.eq.2) then
                  var_3d2(iloop,jloop,kloop,it,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,it)
             endif
            enddo
           enddo
          enddo
         enddo

      else

        ierr = NF_GET_VAR_REAL(nf,var3didin(k),temp3dlow)
        if(ierr.ne.NF_NOERR) then
           write(0,*) NF_STRERROR(ierr)
           stop "get3d"
        endif

        do it=1,dimuti
         do kloop=1,low
          do jloop=1,dimlat
           do iloop=1,dimlon
            if (order.eq.1) then
                  var_3d(iloop,jloop,kloop,it,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,it)
            elseif(order.eq.2) then
                  var_3d2(iloop,jloop,kloop,it,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,it)
            endif
             enddo
            enddo
          enddo
         enddo

      endif! up_low

      return
      end subroutine  get_3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsd_2d(nf,type,k,varname,order)

!  getsd_2D reads the 2D RMS ans STD variables in up or low netcdf files

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::      nf,k,order
      character (LEN=50) :: varname
      character (LEN=4) ::  type

!     local variables
      integer ::      ierr,iloop,jloop
      integer ::      sd2didin(nbsd2d)
      real         temp2d(dimlon,dimlat,dimuti)

!      write(*,*),'varname(k) k',k,varname
! NETCDF reading std or rms variable
         ierr=NF_INQ_VARID(nf,varname,sd2didin(k))
         if(ierr.ne.NF_NOERR) then
           write(0,*) NF_STRERROR(ierr),varname
           stop
         endif
         ierr = NF_GET_VAR_REAL(nf,sd2didin(k),temp2d)
         if(ierr.ne.NF_NOERR) then
            write(0,*) NF_STRERROR(ierr) , 'stop in getsd3d'
            stop
         endif

           do jloop=1,dimlat
            do iloop=1,dimlon

             if (type.eq.'stdv') then
                  if (order.eq.1) then
                  varsd2d(iloop,jloop,k)= &
                  temp2d(iloop,dimlat+1-jloop,1)
                  elseif (order.eq.2) then
                  varsd2d2(iloop,jloop,k)= &
                  temp2d(iloop,dimlat+1-jloop,1)
                  endif
             elseif (type.eq.'rms') then
                  if (order.eq.1) then
                  varrms2d(iloop,jloop,k)= &
                  temp2d(iloop,dimlat+1-jloop,1)
                  elseif (order.eq.2) then
                  varrms2d2(iloop,jloop,k)= &
                  temp2d(iloop,dimlat+1-jloop,1)
                  endif
             endif

            enddo
           enddo

      return
      end subroutine  getsd_2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsd_3d(nf,type,k,varname,up_low,order)

!  getsd_3D reads the 3D RMS ans STD variables in up or low netcdf files

      implicit none

!#include "constants_atmemcd_41.h"
!     include "netcdf.inc"

!     inputs
      integer ::      nf,k,up_low,order
      character (LEN=50) :: varname
      character (LEN=4) ::  type

!     local variables
      integer ::      ierr,iloop,kloop,jloop
      integer ::      sd3didin(nbsd3d)
      real         temp3dlow(dimlon,dimlat,low,dimuti)
      real         temp3dup(dimlon,dimlat,up,dimuti)

!      write(*,*),'varname(k) k',k,varname
! NETCDF reading sdt or rms variable
         ierr=NF_INQ_VARID(nf,varname,sd3didin(k))
         if(ierr.ne.NF_NOERR) then
           write(0,*) NF_STRERROR(ierr),varname
           stop
         endif
      if (up_low.eq.up) then
         ierr = NF_GET_VAR_REAL(nf,sd3didin(k),temp3dup)
         if(ierr.ne.NF_NOERR) then
            write(0,*) NF_STRERROR(ierr)
            stop "getsd3d"
         endif

          do kloop=low+1,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon

             if (type.eq.'stdv') then
                  if (order.eq.1) then
                  varsd3d(iloop,jloop,kloop,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,1)
                  elseif (order.eq.2) then
                  varsd3d2(iloop,jloop,kloop,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,1)
                  endif
             elseif (type.eq.'rms') then
                  if (order.eq.1) then
                  varrms3d(iloop,jloop,kloop,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,1)
                  elseif (order.eq.2) then
                  varrms3d2(iloop,jloop,kloop,k)= &
                  temp3dup(iloop,dimlat+1-jloop,kloop-low,1)
                  endif
             endif
            enddo
           enddo
          enddo

      else

         ierr = NF_GET_VAR_REAL(nf,sd3didin(k),temp3dlow)
         if(ierr.ne.NF_NOERR) then
            write(0,*) NF_STRERROR(ierr)
            stop "getsd3d"
         endif

          do kloop=1,low
           do jloop=1,dimlat
            do iloop=1,dimlon
              if (type.eq.'stdv') then
                  if (order.eq.1) then
                  varsd3d(iloop,jloop,kloop,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,1)
                  elseif (order.eq.2) then
                  varsd3d2(iloop,jloop,kloop,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,1)
                  endif
              elseif (type.eq.'rms') then
                  if (order.eq.1) then
                  varrms3d(iloop,jloop,kloop,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,1)
                  elseif (order.eq.2) then
                  varrms3d2(iloop,jloop,kloop,k)= &
                  temp3dlow(iloop,dimlat+1-jloop,kloop,1)
                  endif
               endif
              enddo
             enddo
            enddo
       endif! up_low

      return
      end subroutine  getsd_3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var3d(a,lon,lat,levhi,levlow,levweight,pratio, &
                       utime,name,ier,itimint,wl,wh)

!     Retrieve the value of 3-d variable=name at longitude=lon, latitude=lat
!     and Universal time=utime. The height of the variable is controlled by
!     levhi, levlow and levweight.  Use bilinear interpolation to get user
!     specified longitude and latitude.

      implicit none

!#include "constants_atmemcd_41.h"

!     inputs
      real         lon        !longitude (east) of point
      real         lat         !latitude of point
      integer ::      levhi       !database level upper bound
      integer ::      levlow      !database level lower bound
      real         levweight(2)!level weight for interpolation
!     (1) for linear in height (2) for linear in pressure
      real         pratio      !ratio of pressure to extreme value if out of range
      real         utime       !Universal time (0. to 24. hrs) =local time at lon=0
      integer ::      itimint     !seasonal interpolation flags
      real         wl,wh       !seasonal interpolation weightings
      character (LEN=16) :: name        !Name of variable

!     outputs
      integer ::       ier        !error flag (0=OK, 1=NOK)
      real          a          !value of variable

!     local variables
      real    profile(dimlevs) !profile in MCD grid + 1 "thermospheric" point

      ier = 0

      call profi(profile,lon,lat,utime,name,ier,itimint,wl,wh)

      if (ier.ne.0) then
         write(0,*)'VAR3D Error : profile not available for variable' &
              ,name
         ier=1
         goto 9999
      endif

      if (name.eq.'rho'.or.name.eq.'sdrho') then
!     interpolate densities linearly in pressure
         a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(2)
!     correction for densities which are out of sigma range
         a=pratio*a
      else ! most variables are interpolated linearly in height
         a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
      endif

      return

!     Error handling
 9999 a=0.
      return
      end subroutine  var3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine max2d(a,lon,lat,name,ier,itimint,wl,wh)

!     Retrieve the daily maximum value of 2-d variable=name at
!     longitude=lon, latitude=lat

      implicit none

!     inputs
      real         lon     !west longitude of point
      real         lat     !latitude of point
      character (LEN=16) :: name    !Name of variable
      integer ::      itimint !seasonal interpolation flag
      real         wl,wh   !seasonal interpolation weightings

!     outputs
      integer ::      ier     !Error flag (0=OK, 1=NOK)
      real         a       !maximum of variable

!     local variables
      integer ::      i
      real         x
      real         xmax
      real         utime

      ier=0
      xmax=-1.e20
      do i=0,22,2
        utime=real(i)
        call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh)
        if(ier.ne.0) then
           ier=1
           write(0,*)"MAX2D Error : impossible to find ",name, &
                " maximum"
           go to 9999
        endif
        if (x.gt.xmax) xmax=x
      enddo
      a=xmax

      return

!     Error handling
 9999 a=0.
      return
      end subroutine  max2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine min2d(a,lon,lat,name,ier,itimint,wl,wh)

!     Retrieve the daily minimum value of 2-d variable=name at
!     longitude=lon, latitude=lat

      implicit none

!     inputs
      real         lon     !longitude of point
      real         lat     !latitude of point
      character (LEN=16) :: name    !Name of variable

!     output
      integer ::      ier     !Error flag (0=OK, 1=NOK)
      real         a       !minimum of variable
      real         wl,wh   !seasonal interpolation weightings
      integer ::      itimint !seasonal interpolation flag

!     local variables
      integer ::      i
      real         x
      real         xmin
      real         utime

      ier=0
      xmin=1.e20
      do i=0,22,2
        utime=real(i)
        call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh)
        if(ier.ne.0) then
           ier=1
           write(0,*)"MIN2D Error : impossible to find ",name, &
                " minimum"
           go to 9999
        endif
        if (x.lt.xmin) xmin=x
      enddo
      a=xmin
      return

!     Error handling
 9999 a=0.
      return
      end subroutine  min2d

!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            FUNCTION ran1(idum)
!     "Minimal" random number generator of Park and Miller with Bays-Durham
!     shuffle and added safeguards
!     Reference : Numerical Recipes in Fortran 77 : the art of scientific
!     computing - 2d edition (1992)- p 271
!     Return a uniform random deviate between 0.0 and 1.0 (exclusive of the
!     endpoints values). Call with idum a negative integer to initialize;
!     thereafter, do not alter idum between successive deviates in a sequence.
!     RNMX should approximate the largest floating value that is less than 1.

      implicit none

      INTEGER :: idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
      NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER :: j,k,iv(NTAB),iy
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
      ran1=min(AM*real(iy),RNMX)

      return
      END function  ran1
! ***********************************************************
      subroutine dustmix(scena,lat,Ls,dod,psurf,p,dust_mmr)
      implicit none

!     Subroutine used to reconstruct the dust mixing ratio
!     assumed in the GCM run, at a given pressure level.
!     F. Forget 2005

!     INPUT
      integer :: scena  ! scenario
      real lat  ! latitude  (degree)
      real Ls       ! solar longitude (deg)
      real dod      ! dust optical depth
      real psurf      ! surface pressure (Pa)
      real p        ! local pressure (Pa)
!     OUTPUT
      real dust_mmr  ! dust mass mixing ratio (kg/kg)
!     Local Variable
      real radius, Qext, rho_dust
      real zlsconst, topdust,topdust0,qextrhor,g
      real zp, expfactor, xlat
      logical :: firstcall
      data firstcall /.true./
      save firstcall,qextrhor,g
      real pi
      parameter   (pi=3.14159265359)


!     initialisation
!     --------------
      topdust = 0.
      if (firstcall) then
         radius=1.8e-6  ! effective radius of dust (m)
         Qext=3.  ! dust visible single scattering extinction coeff.
         rho_dust=2500.  ! Mars dust density (kg.m-3)
         qextrhor= (3./4.)*Qext/(rho_dust*radius)
         g=3.72
         firstcall = .false.
      end if
      xlat = lat*pi/180.

!     Altitude of the top of the dust layer
!     -------------------------------------

      zlsconst=SIN(ls*pi/180. - 2.76)
      if (scena.eq.8) then
             topdust= 30.        ! constant dust layer top = 30 km
      else if (scena.eq.7) then      ! "Viking" scenario
            topdust0=60. -22.*SIN(xlat)**2
            topdust=topdust0+18.*zlsconst
      else if(scena.le.6) then         !"MGS" scenario (MY24, storm)
            topdust=60.+18.*zlsconst &
                     -(32.+18.*zlsconst)*sin(xlat)**4 &
                     - 8.*zlsconst*(sin(xlat))**5
      else
            stop 'problem with scena in dust_mmr'
      endif

!     Computing dust mass mixing ratio
!     --------------------------------
      if(p.gt.700./(988.**(topdust/70.))) then
           zp=(700./p)**(70./topdust)
           expfactor=max(exp(0.007*(1.-max(zp,1.))),1.E-10)
      else
           expfactor=1.E-10
      endif
!     qextrhor is  (3/4)*Qext/(rho*reff)
      dust_mmr = expfactor* dod * g / (psurf * qextrhor)
      return
      end subroutine  dustmix

! ****************************************************************
      subroutine air_properties(T,vmr_co2,vmr_n2,vmr_o,vmr_co, &
                             Cp,gamma,viscosity,Rgas)

      implicit none

!     Subroutine used to compute some useful gas properties
!     from the climate database data
!     F. Forget 2005

!     INPUT
      real  T ! temperature   (K)
      real vmr_co2,vmr_n2,vmr_o,vmr_co ! gas vomume mixing ratio (mol/mol)

!     OUTPUT
      real cp   ! Heat capacity at constant pressure (J kg-1? K-1)
      real gamma   ! Cp/Cv Ratio of specific heats
      real viscosity    ! air viscosity (N s m-2)
      real  Rgas   ! gas constant  (J kg-1 K-1)

!     Local Variable
!     --------------

      real vmr_ar   ! Argon vmr
!     Molecular heat capacity (J mol-1 K-1)
      real Cp_co2, Cp_co, Cp_n2, Cp_ar, Cp_O
!     Coefficient to compute conductivity = A*T**0.69  (W m-1 K-1)
      real A_co2, A_co, A_n2, A_ar, A_O
!     Molecular mass
      real M_co2, M_co, M_n2, M_ar, M_O
      real conduct,meanmolmass,svmr
      logical :: firstcall
      data firstcall /.true./
      save firstcall

      save Cp_co2, Cp_co, Cp_n2, Cp_ar, Cp_O
      save A_co2, A_co, A_n2, A_ar, A_O
      save M_co2, M_co, M_n2, M_ar, M_O

!     initialisation
!     --------------
      if (firstcall) then
!           Molecular mass (kg mol-1)
            M_co2=44.01e-3
            M_co=28.01e-3
            M_n2=28.013e-3
            M_Ar=39.95e-3
            M_o=16.e-3
!           Molecular heat capacity (J mol-1 K-1)
            Cp_co2 = 37.
            Cp_co  = 29.
            Cp_n2 = 29.
            Cp_ar = 20.
            Cp_o = 21.
!           Coeff for conductivity :
            A_co2=3.07e-4
            A_o=7.6e-4
            A_n2=5.6e-4
            A_co=4.87e-4
            A_ar=3.4e-4
            firstcall=.false.
      end if

!     Estimating Argon mixing ratio
!     -----------------------------
      vmr_ar = 0.6 *vmr_n2

!     Mean Molecular mass
!     ------------------
      Svmr = vmr_co2+vmr_n2+vmr_o+vmr_co+vmr_ar
      meanmolmass= (M_co2*vmr_co2+M_n2*vmr_n2+M_o*vmr_o &
            + M_co*vmr_co+M_ar*vmr_ar)/Svmr
      Rgas = 8.3144/meanmolmass

!     Computing Cp (Neglecting H2 => not valid if very high)
!     ------------
      Cp = (Cp_co2*vmr_co2+Cp_n2*vmr_n2+Cp_o*vmr_o &
            + Cp_co*vmr_co+Cp_ar*vmr_ar)/Svmr


!     Conversion from J mol-1 K-1 to J kg-1 K-1
      Cp= Cp / meanmolmass

!     Computing gamma
      gamma = 1./(1.-Rgas/Cp)

!     Computing viscosity
!     -------------------
!     Therma molecular Conductivity (W m-1 K-1)
      conduct = (A_co2*vmr_co2+A_n2*vmr_n2+A_o*vmr_o &
              + A_co*vmr_co + A_ar*vmr_ar)/Svmr
      conduct=(T**0.69) * conduct
      viscosity = conduct / (0.25*(4.*Cp + 5.*Rgas))

      return
      end subroutine  air_properties
!*********************************************************************** 
      subroutine calc_height_41 (xlat,xlon,zradius,zareoid,zsurface, &
                                 iconv,dset,ier)
!***********************************************************************
!$Type                                                                 *
!     Subroutine FORTRAN 90                                            *
!$Nom                                                                  *
!     calc_height_41                                                   *
!                                                                      *
!$Resume                                                               *
!     Calcul le rayon depuis le centre de la planete, hauteur par      *
!     rapport au 0 de l'areoide et hauteur par rapport à la surface    *
!     locale. A partir d'une des donnees precedentes, ce sous          *
!     programme calcule lesw deux autres ( en fonction du parametre    *
!     iconv ). La reference de l'areoide MOLA n'est pas la meme que    *
!     la reference DTM.                                                *
!                                                                      *
!     Note that this is not intended to provide an accurate,           *
!     small-scale map.  The areoid and topography data has             *
!     already been sampled and smoothed at the database                *
!     resolution, and is interpolated by this routine in the           *
!     same way, in order to provide a dataset which is consistent      *
!     with the GCM data.  This same routine may be used for high       *
!     resoltuion data by supplying an alternate data file and          *
!     changing data file names and grid definition parameters.         *
!                                                                      *
!     Ce sous programme est utilise par atmemcd_41                     *
!                                                                      *
!$Version                                                              *
!                                                                      *
!$Author                                                               *
!                                                                      *
!$Usage                                                                *
!     call calc_height_41 (xlat,xlon,zradius,zareoid,zsurface, &       *
!                         iconv,dset)                                  *
!                                                                      *
!$Arguments                                                            *
!>E xlat     : <double precision> latitude areocentrique (rad)         *
!>E xlon     : <double precision> longitude areocentrique Est (rad)    *
!>E zradius  : <double precision> rayon / centre de la planete (m)     *
!>E zareoid  : <double precision> hauteur depuis le zero datum (m)     *
!>E zsurface : <double precision> hauteur depuis la surface locale (m) *
!>E iconv    : <integer>          switch pour choisir la variable z    *
!>E                               utilise pour definir les deux autres *
!>E                      iconv=1  zradius  -> zareoid, zsurface        *
!>E                      iconv=2  zareoid  -> zradius, zsurface        *
!>E                      iconv=3  zsurface -> zradius, zareoid         *
!>E dset     : <character*(*)>    jeu de donnees ( premier appel )     *
!>E                      Un ou pluisiurs blancs : donnees par defaut   *
!>E                      ( repertoire COMPAS ) ou repertoire des       *
!>E                      donnees EMCD avec slash final ('/dir/path/')  *
!>E                      , ou un lien sur le repertoire ('EMCD_DATA/') *
!>S ier      : <integer> code d'erreur                                 *
!>S                      0 = OK                                        *
!>S                      1 = impossible d'ouvrir le jeu de donnees     *
!>S                      2 = impossible de lire les donnees            *
!>S                      3 = parametre iconv invalide                  *
!***********************************************************************
!
      implicit none
!
      real (KIND=8) :: xlat, xlon, zradius, zareoid, zsurface
      integer :: iconv, ier
      character (LEN=*) :: dset
!
!     data file names
      character (LEN=12) :: datname
      parameter(datname='mountain.nc')
!
!     define grid
      integer :: unet              ! NETCDF channel number
      real pi
      parameter(pi=3.14159265359)
      real crd
      parameter(crd=180./pi)
!
      real lat, lon, alat, alon
      real xorog, xareoid
      character (LEN=255) :: dataset, datfile
      integer :: lendataset
      integer :: ierr1, ierr2
      integer :: i
!
      integer :: dlon(4)           ! longitude index of nearest points
      integer :: dlat(4)           ! latitude index of nearest points
      real t                       ! weight
      real u                       ! weight
!
      real orog(dimlon,dimlat)
      real areoid(dimlon,dimlat)
      save orog, areoid
      logical :: firstcall
      data firstcall /.true./
      save firstcall
!
      character (LEN=16) :: name
      integer :: varid
!***********************************************************************
! corps du sous programme                                              *
!***********************************************************************
      dlon(:) = 0
      dlat(:) = 0
      t = 0.
      u = 0.
      ier=0
!     check value of iconv
      if (iconv.lt.1.or.iconv.gt.3) then
         ier=3
         goto 999
      endif

!     read in the data on the first call to the routine only
      if (firstcall) then
!     data set name
         do lendataset = len(dset), 1, -1
            if (dset(lendataset:lendataset).ne.' ') go to 100
         end do
  100    continue
!
         if ( lendataset .eq. 0 ) then !default data set case
!
!***********************************************************************
!  acces au repertoire du nouveau modele version 4.1 par COMPAS        *
!***********************************************************************
            ier = cps_getFichierModele("atmosphere", "EMCD4_1", dataset, &
                 rep=.true.)
            if ( MSP_gen_messages ("cps_atmemcd_41") ) then
               return
            end if
!
            dataset = trim(dataset) // "/"
!***********************************************************************
! valeur attendue dataset = '/usr/local_ms/data/data_emcd_4.1/'        *
!***********************************************************************
         else  !symbolic link or full path given explicitly
            dataset = dset
         end if
!
         lendataset = len_trim (dataset)   

         if (dataset(lendataset:lendataset).ne."/") then
            dataset = trim(dataset) // "/"
            lendataset=lendataset+1
         endif

!
!     open NETCDF file
         datfile = dataset (1:lendataset)//datname
         ier     = NF_OPEN (datfile,NF_NOWRITE,unet)
!        write(*,*) 'height: open ',datfile
         if (ier.ne.0) then
            ier=1
            goto 999
         endif
!
         name='orography'
         ierr1 = NF_INQ_VARID (unet,name,varid)
         ierr2 = NF_GET_VAR_REAL (unet,varid,orog)
         if ((ierr1.ne.nf_noerr).or.(ierr2.ne.nf_noerr)) then
            ier=2
            goto 999
         endif
!
         name='areoid'
         ierr1 = NF_INQ_VARID (unet,name,varid)
         ierr2 = NF_GET_VAR_REAL (unet,varid,areoid)
         if ((ierr1.ne.nf_noerr).or.(ierr2.ne.nf_noerr)) then
            ier=2
            goto 999
         endif
!
         ier = NF_CLOSE (unet)
         firstcall=.false.
      end if
!     convert longitude east (rad) to longitude east (deg) [-180..180]
      lon=real(xlon)*crd
      lon=mod(lon,360.)
      if(lon.gt.180.) lon=lon-360.
!     convert latitude from radians to degrees
      lat=real(xlat)*crd
!     find longitude and latitude of four nearest points (as in grid4)
      if (lon.ge.lonmax) then
         dlon(1)=dimlon
         dlon(2)=1
         dlon(3)=1
         dlon(4)=dimlon
         t=(lon-lonmax)/deltalon
      else
         alon=lonmin
         do i=1,dimlon-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltalon)) then
               dlon(4)=i
               dlon(3)=i+1
               dlon(1)=i
               dlon(2)=i+1
               t=(lon-alon)/deltalon
            endif
            alon=alon+deltalon
         enddo
      endif
      if (lat.ge.latmax) then
         dlat(4)=1
         dlat(3)=1
         dlat(1)=1
         dlat(2)=1
         u=0.
      elseif (lat.le.latmin) then
         dlat(4)=dimlat
         dlat(3)=dimlat
         dlat(1)=dimlat
         dlat(2)=dimlat
         u=0.
      else
         alat=latmin
         do i=dimlat,2,-1
            if ((lat.ge.alat).and.(lat.lt.alat+deltalat)) then
               dlat(1)=i
               dlat(2)=i
               dlat(3)=i-1
               dlat(4)=i-1
               u=(lat-alat)/deltalat
            endif
            alat=alat+deltalat
         enddo
      endif
!     bilinear interpolation to find local orography and areoid
!
      xorog   = (1.-t)*(1.-u)*orog(dlon(1),dlat(1)) &
                +t*(1.-u)*orog(dlon(2),dlat(2)) &
                +t*u*orog(dlon(3),dlat(3)) &
                +(1.-t)*u*orog(dlon(4),dlat(4))
      xareoid = (1.-t)*(1.-u)*areoid(dlon(1),dlat(1)) &
                +t*(1.-u)*areoid(dlon(2),dlat(2)) &
                +t*u*areoid(dlon(3),dlat(3)) &
                +(1.-t)*u*areoid(dlon(4),dlat(4))
!
!     do the conversions according to iconv
      if (iconv.eq.1) then
         zareoid  = zradius - real(xareoid, kind=8)
         zsurface = zareoid - real(xorog, kind=8)
      else if (iconv.eq.2) then
         zradius  = zareoid + real(xareoid, kind=8)
         zsurface = zareoid - real(xorog, kind=8)
      else ! iconv.eq.3
         zareoid  = zsurface + real(xorog, kind=8)
         zradius  = zareoid  + real(xareoid, kind=8)
      end if
!
      return
!***********************************************************************
!     handle errors
  999 zradius  = -999.0d0
      zareoid  = -999.0d0
      zsurface = -999.0d0
      return
    end subroutine  calc_height_41


    subroutine cps_atmemcd_41_close ()
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine                                                       *
!$Nom                                                                  *
!     cps_atmemcd_41_close                                             *
!                                                                      *
!$Resume                                                               *
!      routine de desallocation memoire                                *
!$Auteur                                                               *
!      Florence VIVARES (ATOS Origin)                                  *
!$Usage                                                                *
!     call cps_atmemcd_41_close()                                      *
!                                                                      *
!$<>                                                                   *
!***********************************************************************

! Variable pour la gestion d'erreur de deallocate
      integer :: ierralloc


      if (modele_init) then
!
! desallocation des variables globales :
!
         if (associated(varrms2d)) deallocate(varrms2d, stat=ierralloc)
         if (associated(varrms2d2))deallocate(varrms2d2,stat=ierralloc)
         if (associated(varrms3d)) deallocate(varrms3d, stat=ierralloc)
         if (associated(varrms3d2))deallocate(varrms3d2,stat=ierralloc)
         if (associated(varsd2d))  deallocate(varsd2d,  stat=ierralloc)
         if (associated(varsd2d2)) deallocate(varsd2d2, stat=ierralloc)
         if (associated(varsd3d))  deallocate(varsd3d,  stat=ierralloc)
         if (associated(varsd3d2)) deallocate(varsd3d2, stat=ierralloc)
         if (associated(var_2d))   deallocate(var_2d,   stat=ierralloc)
         if (associated(var_2d2))  deallocate(var_2d2,  stat=ierralloc)
         if (associated(var_3d))   deallocate(var_3d,   stat=ierralloc)
         if (associated(var_3d2))  deallocate(var_3d2,  stat=ierralloc)
         if (associated(tabpcsmth))deallocate(tabpcsmth,stat=ierralloc)
         if (associated(tabpcvar)) deallocate(tabpcvar, stat=ierralloc)
         if (associated(tabeops))  deallocate(tabeops,  stat=ierralloc)
         if (associated(tabeot))   deallocate(tabeot,   stat=ierralloc)
         if (associated(tabeou))   deallocate(tabeou,   stat=ierralloc)
         if (associated(tabeov))   deallocate(tabeov,   stat=ierralloc)
         if (associated(tabeorho)) deallocate(tabeorho, stat=ierralloc)
         modele_init = .false.
      endif


    end subroutine cps_atmemcd_41_close


end module cps_modele_emcd41
