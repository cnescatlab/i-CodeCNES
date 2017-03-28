module cps_modele_emcd42
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_modele_emcd42
!
!$Resume
!  Modèles d'Atmosphère martien EMCD 4.2
!
!$Description
! Ce module contient la version 4.2 du modèle EMCD (European Martian
! Climate Database) :
! - routine cps_atmemcd_42
! 
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_modele_emcd42.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: cps_modele_emcd42.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.11  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.10  2010/02/24 09:06:16  cmartel
!  VERSION::AQ::24/02/2010:Corrections suite a la recette interne
!
!  Revision 1.9  2008/10/14 07:54:15  cml
!  DM-ID 1058 : Suppression d une declaration inutile
!
!  Revision 1.8  2008/04/22 16:45:00  vivaresf
!  COMPAS V2.4, AQ : suppression d'une référence à une routine non définie
!
!  Revision 1.7  2008/04/11 17:44:10  vivaresf
!  FA-ID 1009 : intégration PSIMU,
!  gestion des / en fin de répertoire
!
!  Revision 1.6  2008/04/07 09:17:18  vivaresf
!  FA-ID 1009 : suppression des use cps_acces
!  correction des cartouches
!  vérification des accès par le biais de la base COMPAS aux modèles
!
!  Revision 1.5  2008/02/26 14:28:59  vivaresf
!  FA-ID 939 : écriture des messages d'erreur sur le stderr au lieu du stdout
!
!  Revision 1.4  2007/11/21 13:00:41  sbd
!  DM-ID 797 suppression affichage MOLA dans resultats
!
!  Revision 1.3  2007/11/21 11:49:49  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.2  2007/11/15 08:35:37  vivaresf
!  DM-ID 808 : modèle EMCD42
!
!  Revision 1.1  2007/10/30 13:31:09  jpi
!  DM-ID 551 : premiere version du modele EMCD42
!
!  Revision 1.0  2007/10/30 00:00:00  vivaresf
!  DM-ID 551 : code plus portage
!
!$FinHistorique
!
!$Usage
!  use cps_modele_emcd42
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
  character(len=256), private :: SVN_VER =  '$Id: cps_modele_emcd42.F90 379 2013-02-22 16:59:49Z ffsm $'

  interface cps_atmemcd_42
     module procedure call_mcd
  end interface

!***********************************************************************
!  déclaration des sous programmes et fonctions de ce module           *
!***********************************************************************
  private eofpb,gasdev,getsi,grid4,grwpb,loadvar,loadeof
  private mars_ltime,mars_ptime,mcd_time,opend,orbit,profi
  private season,sol2ls,ls2sol,var2d,interpol,extrapol3d,get_2d
  private get_3d,getsd_2d,getsd_3d,var3d,max2d,min2d,ran1,dustmix
  private air_properties
  private build_sigma_hr,pres0,read_vl,calc_factcor,mola,molareoid
  private readcs,geoid,LGNDR

  ! La routine de désallocation de la mémoire est cps_atmemcd_42_close
  ! Elle doit etre appelee a la fin de l'ensemble des utilisation du modèle

!***********************************************************************
! include "constants_mcd.inc"
!***********************************************************************
! include file defining constants and arrays used by the 
! European Martian Climate Database
! CONSTANTS
!     information and error messages will be written to output if
!     output_message is .true. Switch to .false. for a "silent" mode
      logical, parameter :: output_messages = .true.
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
      parameter(lonmin=-180,lonmax=174.375)
      parameter(latmin=-90,latmax=90)
!     range of latitude, longitude, number of EOFs and time for EOFs values
      real,private :: lonmineo,lonmaxeo,latmineo,latmaxeo,nevecsmin,nevecsmax
      real,private ::  daymineo,daymaxeo
      parameter(lonmineo=-180.0,lonmaxeo=157.5)
      parameter(latmineo=-82.5,latmaxeo=82.5)
      parameter(nevecsmin=1.0,nevecsmax=72.0)
      parameter(daymineo=1.0,daymaxeo=669.0)
!     step of grid in longitude for mean and std. dev. values
      real,private, parameter :: deltalon=5.625 ! 360./64
!     step of grid in latitude for mean and std. dev. values
      real,private,parameter ::  deltalat=3.75  ! 180./48
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
!      common /orogra/ taborog,tabsubstd,tabareo
!      common /moyenne/ var_2d,var_2d2,var_3d,var_3d2
!      common /stddev/ varsd2d,varsd2d2,varsd3d,varsd3d2
!      common /rms/ varrms2d,varrms2d2,varrms3d,varrms3d2
!      common /eofs/ tabeonormu,tabeonormv,tabeonormt,tabeonormp,
!     &     tabeonormr,tabpcsmth,tabpcvar,tabeops,tabeot,tabeorho,
!     &     tabeou,tabeov 
! -------------------------------------------
!     orographic data array
      real,save,private ::  taborog(dimlon,dimlat,1)
!     orographic variance array
      real,save,private ::  tabsubstd(dimlon,dimlat,1)
!     GCM areoid data array
      real,save,private :: tabareo(dimlon,dimlat,1)
! -------------------------------------------
! 2D fields:
! ---------
! tsurf        ! 1. surface temperature
! ps           ! 2. surface pressure
! co2ice       ! 3. co2ice cover
! fluxsurf_lw  ! 4. Thermal IR radiative flux to surface
! fluxtop_lw   ! 5. Thermal IR radiative flux to space
! fluxsurf_sw  ! 6. Solar IR radiative flux to surface
! fluxtop_sw   ! 7. Solar IR radiative flux to space
! dod          ! 8. Dust optical depth
! col_h2ovapor ! 9. H2O vapour column
! col_h2oice   !10. H20 ice column
! (dimlon,dimlat,dimuti,nbvar2d)
      real,save,private, dimension(:,:,:,:),pointer :: var_2d => NULL()
! (dimlon,dimlat,dimuti,nbvar2d)
      real,save,private, dimension(:,:,:,:),pointer :: var_2d2 => NULL()
! -------------------------------------------
! 3D fields 	
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)
      real,save,private, dimension(:,:,:,:,:),pointer :: var_3d => NULL()
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)
      real,save,private, dimension(:,:,:,:,:),pointer :: var_3d2 => NULL()
! -------------------------------------------
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
! -------------------------------------------
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
! -------------------------------------------
!     arrays related to EOFs
!     normalisation factors for zonal wind, meriodional wind, temperature
!     pressure and density
      real,save,private,dimension(dimlateo) :: tabeonormu
      real,save,private,dimension(dimlateo) :: tabeonormv
      real,save,private,dimension(dimlateo) :: tabeonormt 
      real,save,private,dimension(dimlateo) :: tabeonormp 
      real,save,private,dimension(dimlateo) :: tabeonormr 
! -------------------------------------------
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
! -------------------------------------------
! gestion de l'allocation/desallocation des tableaux dynamiques
      logical, save, private :: modele_init=.false.
! -------------------------------------------

contains

!***********************************************************************
!  corps du module                                                     *
!***********************************************************************

      subroutine call_mcd(zkey,xz,xlon,xlat,hireskey, &
            datekey,xdate,localtime,dset,scena, &
            perturkey,seedin,gwlength,extvarkey, &
            pres,dens,temp,zonwind,merwind, &
            meanvar,extvar,seedout,ier)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Purpose:
! =======
!     call_mcd is a fortran 77 subroutine which extracts and computes
!     meteorogical variables from the Mars Climate Database
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Version:
! =======
!     4.2 12/06 changed "atmemcd" routine to "call_mcd"; added high resolution
!     4.1 03/06 improved time routines (orbit,mars_ltime,sol2ls,ls2sol)
!     4.1 02/05 add many new output variables.
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
!
! Authors:
! =======
!     v4.2 Changed argument list; implemented high resolution
!          and many other small changes  -- EM
!     v4.1 Improved Earth to Mars date conversion + local time -- EM
!     v4.1 Final version with further new variables, bug fixed, EOF -- FF
!     v4.0 version with new variables, bug fixed, EOF -- FF
!     v4.0 combining low and up atmosphere, variables added --
!          min, mean and max solar scenario for thermosphere --
!          choice of vertical coordinates, flag zkey, zareoid, zsurface,
!          zradius,zpressure in input output --
!          altitude in meters everywhere -- KD, FF
!     v3.2 Variables saved in subroutines, seasonal interpolation added -- SJB
!          Gravity allowed to vary with height in hydrostatic eqn  -- SRL
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
!
! Arguments (inputs):
! ==================
!   zkey  : <integer>          type of vertical coordinate xz
!                              1 = radius from centre of planet (m)
!                              2 = height above areoid (m) (MOLA zero datum)
!                              3 = height above surface (m)
!                              4 = pressure level (Pa)
!   xz    : <real> vertical coordinate (m or Pa, depends on zkey)
!   xlon  : <real> longitude (degrees east)
!   xlat  : <real> latitude (degrees north)
!   hireskey: <integer> flag to switch to high resolution topography
!                    0 = use GCM resolution
!                    1 = switch to high resolution topography
!   datekey: <integer>    type of date (see 'xdate' below)
!   xdate : <double precision> date
!                              IF datekey = 0 : Earth julian date
!                              IF datekey = 1 : Mars date (= Ls)
!                          -(Int(solar longitude [deg] ) + localtime/100)
!                         (e.g. xdate = -90.12 correspond to Ls=90 ; LT=12:00)
!                         (e.g. xdate = -173.18 correspond to Ls=173 ; LT=18:00)
!   localtime : <real> local time (martian hours)
!              ONLY USED IF datekey=1 (must be set to 0 if datekey=0)
!   dset  : <character*(*)>    data set
!                              One or more blanks to get the default,
!                              or full directory path of MCD data required
!                              including trailing slash (e.g. '/dir/path/'),
!                              or a link to a directory (e.g. 'EMCD_DATA/').
!                              Default is link EMCD_DATA in working directory.
!   scena : <integer>          scenario
!                             1 = MY24 min solar
!                             2 = MY24 ave solar
!                             3 = MY24 max solar
!                             4 = dust storm tau=3 min solar
!                             5 = dust storm tau=3 ave solar
!                             6 = dust storm tau=3 max solar
!                             7 = warm scenario - dusty, max solar
!                             8 = cold scenario - low dust, min solar
!   perturkey: <integer>  perturbation type
!             1: none
!             2: large scale : 'seedin' is seed or signals reseting the
!                EOF perturbations if changed between calls to CALL_MCD
!             3: small scale : 'seedin' is seed or signals reseting the
!                GW perturbations if changed between calls to CALL_MCD
!             4: large and small scale : does both 2 and 3 above
!             5: add 'seedin' times the standard deviation
!                (seedin must not be greater than 4 or less than -4)
!   gwlength : <real>  for small scale (ie: gravity wave) perturbations;
!                   gwlength= wavelength of gravity wave perturbation (m)
!                             set to zero to get default (16000m)
!   extvarkey  : <integer> output type
!                   0 = pressure, density, temperature, wind
!                   1 = compute also extra variables written in extvar
!
! Arguments (outputs):
! ===================
!   seedout : <real> currrent value of the seed number
!   pres  : <real> pressure (Pa)
!   dens    : <real> density (kg/m^3)
!   temp  : <real> temperature (K)
!   zonwind : <real> zonal wind component (East-West)
!   merwind : <real> meridional wind component (North-South)
!   meanvar : <real> mean values array (of dimension nmeanvar=5)
!               meanvar(1)= mean pressure
!               meanvar(2)= mean density
!               meanvar(3)= mean temperature
!               meanvar(4)= mean zonal wind component
!               meanvar(5)= mean meridional wind component
!   extvar : <real>  extra variables array (of dimension nextvar=100)
!               extvar(1) = Radial distance from planet center (m)
!               extvar(2) = Altitude above areoid (Mars geoid) (m)
!               extvar(3) = Altitude above local surface (m)
!               extvar(4) = orographic height (m) (surface altitude above areoid)
!               extvar(5) = Ls, solar longitude of Mars (deg)
!               extvar(6) = LST local true solar time (hrs)
!               extvar(7) = Universal solar time (LST at lon=0) (hrs)
!               extvar(8) =  Air heat capacity Cp (J kg-1 K-1)
!               extvar(9) =  gamma=Cp/Cv Ratio of specific heats
!               extvar(10)= density RMS day to day variations (kg/m^3)
!               extvar(11)= not used (set to zero)
!               extvar(12)= random density perturbation (kg/m^3)
!               extvar(13)= scale height H(p) (m)
!               extvar(14)= GCM orography (m)
!               extvar(15)= surface temperature (K)
!               extvar(16)= daily maximum mean surface temperature (K)
!               extvar(17)= daily minimum mean surface temperature (K)
!               extvar(18)= surf. temperature RMS day to day variations (K)
!               extvar(19)= GCM surface pressure (Pa)
!               extvar(20)= high resolution surface pressure (if hireskey=1)
!               extvar(21)= not used. (set to zero)
!               extvar(22)= surface pressure RMS day to day variations (Pa)
!               extvar(23)= temperature RMS day to day variations (K)
!               extvar(24)= zonal wind RMS day to day variations (m/s)
!               extvar(25)= meridional wind RMS day to day variations (m/s)
!               extvar(26)= vertical wind component (m/s) >0 when downwards!
!               extvar(27)= vertical wind RMS day to day variations (m/s)
!               extvar(28)= small scale perturbation (gravity wave) (kg/m^3)
!               extvar(29)= q2: turbulent kinetic energy (m2/s2)
!               extvar(30)= not used yet
!               extvar(31)= thermal IR flux to surface (W/m2)
!               extvar(32)= solar flux to surface (W/m2)
!               extvar(33)= thermal IR flux to space (W/m2)
!               extvar(34)= solar flux reflected to space (W/m2)
!               extvar(35)= surface CO2 ice layer (kg/m2)
!               extvar(36)= DOD: Dust column visible optical depth
!               extvar(37)= Dust mass mixing ratio (kg/kg)
!               extvar(38)= DOD RMS day to day variations
!               extvar(39)= DOD total standard deviation over season
!               extvar(40)= Water vapor column (kg/m2)
!               extvar(41)= Water vapor vol. mixing ratio (mol/mol)
!               extvar(42)= Water ice column (kg/m2)
!               extvar(43)= Water ice mixing ratio (mol/mol)
!               extvar(44)= O3 ozone vol. mixing ratio (mol/mol)
!               extvar(45)= [CO2] vol. mixing ratio (mol/mol)
!               extvar(46)= [O] vol. mixing ratio (mol/mol)
!               extvar(47)= [N2] vol. mixing ratio (mol/mol)
!               extvar(48)= [CO] vol. mixing ratio (mol/mol)
!               extvar(49)= R: Molecular gas constant (J K-1 kg-1)
!               extvar(50)= Air viscosity estimation (N s m-2)
!               extvar(51:100) = unused (set to zero)
!
!    ier  : <integer> error flag
!     0 = OK
!     1 = wrong vertical coordinate flag (zkey)
!     2 = wrong dust scenario (scena)
!     3 = wrong value for perturbation flag (perturkey)
!     4 = wrong value for high resolution flag (hireskey)
!     5 = wrong value for date flag (datekey)
!     6 = wrong value for extra variables flag (extvarkey)
!     7 = wrong value for latitude (xlat)
!     8 = inadequate value for gravity wave wavelength (gwlength)
!     9 = wrong value of solar longitude (xdate, if datekey=1)
!    10 = Julian date lies outside [jdate_min:jdate_max] range
!    11 = wrong value of local time (should be in [0:24])
!    12 = Incompatible localtime(.ne.0) and datekey(=0)
!    13 = Unresonable value of 'seedin' (in perturkey=5 case)
!    14 = No dust storm scenario available at such date
!    15 = Could not open a database file (wrong path to database?)
!    16 = Failed loading data from a database file
!    17 = Given (or computed) altitude is underground
!    18 = adding (perturkey=5) perturbation yields unphysical density
!    19 = adding (perturkey=5) perturbation yields unphysical temperature
!    20 = adding (perturkey=5) perturbation yields unphysical pressure
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

! -------------------------------------------
!     Additional Option (not in the input)
!     itimint, flag for seasonal interpolation
!     itimint 0: no seasonal interpolation
!     itimint 1: seasonal interpolation
      integer ::       itimint
      parameter     (itimint=1)
! -------------------------------------------
      integer :: nextvar  ! size of extra variable array extvar()
      integer :: nmeanvar ! size of meanvar() array
      parameter (nextvar=100,nmeanvar=5)

!     inputs
!     ******
      integer ::       zkey ! flag for vertical coordinate type
      real          xz   ! vertical coordinate (m or Pa, depending on zkey)
      real          xlon ! east longitude (degrees)
      real          xlat ! latitude (degrees)
      integer ::       hireskey  ! flag: high resolution if =1
      integer ::       datekey   ! flag: 0=earth date 1= Mars date
      real (KIND=pm_reel) ::        xdate     ! earth julian date (or Ls)
      real          localtime ! true solar time at longitude lon
! Note: localtime can only be imposed if datekey=1 and should be 0 otherwise
      integer ::       perturkey ! flag: perturbation type
      real          seedin    ! seed for random number generation
! seedin is also used to trigger a reinitialization of perturbation
! (triggered if value of seedin changes between subsequent calls to CALL_EMCD)
      real  gwlength ! gravity wave perturbation (vertical wavelength of)
      integer ::       scena     ! dust scenario
      integer ::       extvarkey ! flag: compute extra variables if =1
      character (LEN=*) :: dset      ! path to datafiles

!     outputs
!     *******
      real    meanvar(nmeanvar) ! array for mean values
      real    extvar(nextvar)   ! array for extra outputs
      real    seedout ! current value of 'seed' (used by ran1)
      real    pres    ! atmospheric pressure
      real    dens    ! atmospheric density
      real    temp    ! atmospheric temperature
      real    zonwind ! zonal (eastward) wind
      real    merwind ! meridional (northward) wind
      integer :: ier     ! status (error) code

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
!     dust scenario
      integer ::           dust
!     seed (and also current index) for random number generator
      integer ::           seed
!     save seed, etc to ensure code works with standard F77
      save              seed
!      real              R        ! not used any more, see Rnew
!      parameter        (R=191.2)
      real              Rnew
      real              R_gcm(dimlevs)    ! R at GCM levels
      real              rho_gcm(dimlevs)  ! rho at GCM levels
      real              temp_gcm(dimlevs) ! temperature at GCM levels
!     number of standard deviations to add
      real              nbsig
!   Julian dates that should bracket "xdate" (if given as a Juilan date)
      real (KIND=pm_reel) ::        jdate_min
      parameter     (jdate_min=2378496.5) ! 01-01-1800 00:00
      real (KIND=pm_reel) ::        jdate_max
      parameter     (jdate_max=2524593.5) ! 01-01-2200 00:00
      integer ::       i,l    ! for loops
      integer ::       ierr   ! to store return codes from called routines
      integer ::       levlow ! database level, lower bound (wrt sought alt.)
      integer ::       levhi  ! database level, upper bound (wrt sought alt.)
      real          areoid     ! distance to center of Mars of reference areoid
      real          areoid_gcm ! GCM areoid (m)
      real          areoid_hr  ! High resolution areoid (m)
      real          height    ! height above areoid (m)
      real          oroheight ! orographic height (m)
      real          oro_hr    ! MOLA orographic height (m)
      real          oro_gcm   ! GCM orographic height (m)
      real          absheight ! height above surface (m)
      real          ls        ! Solar longitude (deg.)
      real          marsau    ! Sun-Mars distance (in AU)
      real          modelday  ! GCM day [0:668.6]
      real          loctime   ! true solar time at longitude lon
      real          utime     !universal time (0. to 24. hrs)
                              !=local time at lon=0
      real          lon      !longitude east [-180  180]
      real          lat      !latitude (degrees)
      real          ps       ! surface pressure
      real          ps_gcm   ! surface pressure (as read from MCD)
      real          ps_hr    ! surface pressure (high res, using MOLA topo.)
      real          ps_psgcm ! high res to GCM surface pressure ratio
      real          p_pgcm(dimlevs) ! high res over GCM pressure ratios
!     variables  (trailing l indicates lower level variable from database)
      real          t,tl     ! atmospheric temperature
      real          p,pl     ! atmospheric pressure
      real          rho,rhol ! density
      real          u,ul,v,vl,w_l ! zonal, meridional and vertical winds
      real          col_h2ovapor,col_h2oice
      real          q2,vmr_h2oice,vmr_h2o,vmr_o,vmr_co2,vmr_co
      real          vmr_n2,vmr_o3
      real          pertm,pertr,pertrho,pertrhogw,pertrhoeof
      data          pertrhoeof /0.0/
      real          pertps
      real          pertu, pertv, pertt
      real          levweight(2) ! weights for vertical interpolation
      real          pratio ! 'correction' parameter (usually = 1.0, unless
      		! outside of sigma range; it then accounts for
      		! being above/below lowest/highest sigma level)
      real          lamda   !gravity wave vertical wavelength (m)
      real          rdnos(dimnevecs) ! deviates for EOF perturbations
      save          rdnos
      real          dev  ! (random) phase angle for gravity waves
      save          dev
      real          pscaleheight    ! scale height
      real          tsurf,tsurfmax,tsurfmin,rmstsurf
      real          rmsps
      real          co2ice, dod, rmsdod,tsddod,dust_mmr
      real          Cp, gamma, viscosity,Rgas
      real          fluxsurf_lw, fluxsurf_sw, fluxtop_lw, fluxtop_sw
      real          rmst, rmsu, rmsv, rmsw, rmsrho
      real          tmeanl(nmeanvar) ! to temporarily store meanvar()
      real          zradius   ! distance to center of planet
      real          zareoid   ! height above areoid
      real          zsurface  ! height above surface
      real          zpressure ! atmospheric pressure

      real          g0  ! reference gravitational acceleration
      real          a0  ! reference distance at which g=g0
      parameter     (a0=3396.E3,g0=3.7257964)

      character (LEN=16) ::  name
      character (LEN=4) ::   typevar
      character (LEN=255) :: dataset
      integer ::       lendataset
      character (LEN=255) :: dataset2
!  season numbers
      integer ::       nums,nums2
      integer ::       numsprec, numsprec_eo,numsprec_sd,numsprec_gw
      data          numsprec /0/
      data          numsprec_eo /0/
      data          numsprec_sd /0/
      data          numsprec_gw /0/
      save          numsprec,numsprec_eo,numsprec_sd,numsprec_gw
!  seasonal interpolation weights
      real          wl,wh
!  previous dust scenario
      integer ::       dustprec
      data          dustprec /0/
      save          dustprec
!  previous wavelength of gravity wave perturbation
      real          prevgwlength
      save          prevgwlength
! previous value of seedin
      real          prevseedin
      save          prevseedin
!     function declarations
!      real          gasdev,ran1,ls2sol
!     vertical coordinates
      real          aps(dimlevs)       ! hybrid coordinate
      save          aps
      real          bps(dimlevs)       ! hybrid coordinate
      save          bps
      real          sigma(dimlevs)     ! sigma levels to work with
      save          sigma
      real          sigma_gcm(dimlevs) ! sigma levels in GCM
      save          sigma_gcm
      real          sigma_hr(dimlevs)  ! high resolution sigma levels
      save          sigma_hr
      real          pseudoalt(dimlevs) ! pseudo altitude
      save          pseudoalt
! flag for first call to call_mcd
      logical ::       firstcall
      data          firstcall /.true./
      save          firstcall

! Variable pour la gestion d'erreur de allocate/deallocate
      integer :: ierralloc
! ===============================================================

! 0. Initializations
      if (firstcall) then ! only for very first call to CALL_MCD
         ! set dummy previous value of seedin
         prevseedin=seedin-1.0
         prevgwlength=gwlength-1.0 ! dummy previous value of gwlength
         ! set firstcall to false
         firstcall=.false.
      else ! initializations for every call to CALL_MCD
         ier=0
      endif

!***********************************************************************
!  1. Check (and eventually convert) input arguments
!***********************************************************************

!  1.1 Path to data set
!     find last non-blank character
      do lendataset = len(dset), 1, -1
         if (dset(lendataset:lendataset).ne.' ') go to 100
      end do
100   continue
      if (lendataset.eq.0) then  !default data set case
!***********************************************************************
!  acces au repertoire du nouveau modele version 4.2 par COMPAS        *
!***********************************************************************
         ier = cps_getFichierModele("atmosphere", "EMCD4_2", dataset, &
           rep=.true.)
         if ( MSP_gen_messages ("cps_atmemcd_42") ) then
            return
         end if
         dataset2 = dataset
!***********************************************************************
! valeur attendue dataset = '/usr/local_ms/data/data_emcd_4.2/'        *
!***********************************************************************
      else  !symbolic link or full path to datasets is given explicitly
         dataset = dset
         dataset2 = dset
      end if

      lendataset = len_trim(dataset)
      if (dataset(lendataset:lendataset).ne."/") then
         dataset  = trim(dataset) // "/"
         dataset2 = dataset
         lendataset=lendataset+1
      endif

!  1.2 Check value of dust scenario
      dust=scena
      if ((dust.lt.1).or.(dust.gt.8)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: ',scena,' unknown scenario'
         endif
         ier=2
         goto 9999
      endif

!  1.3 Check vertical coordinate type (and set high_res_topo flag)
      if ((zkey.gt.4).or.(zkey.le.0)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: zkey=',zkey,' unknown vert. coord'
         endif
         ier=1
         goto 9999
      endif

! 1.4 Check various flags
! 'hireskey' flag:
      if ((hireskey.lt.0).or.(hireskey.gt.1)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter hireskey'
            write(0,*)'               hireskey=',hireskey
         endif
         ier=4
         goto 9999
      endif

! 'datekey' flag:
      if ((datekey.lt.0).or.(datekey.gt.1)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter datekey'
            write(0,*)'               datekey=',datekey
         endif
         ier=5
         goto 9999
      endif

! 'perturkey' flag:
      if ((perturkey.lt.0).or.(perturkey.gt.5)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter perturkey'
            write(0,*)'               perturkey=',perturkey
         endif
         ier=3
         goto 9999
      endif

! 'extvarkey' flag:
      if ((extvarkey.lt.0).or.(extvarkey.gt.1)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter extvarkey'
            write(0,*)'               extvarkey=',extvarkey
         endif
         ier=6
         goto 9999
      endif

! check that the value of gwlength is resonable
      if (((gwlength.lt.2.E3).and.(gwlength.ne.0.)).or. &
           (gwlength.gt.30.E3)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter gwlength'
            write(0,*)'               gwlength=',gwlength
            write(0,*)'(should be in [2000:30000] range)'
         endif
         ier=8
         goto 9999
      endif

! check that the value of 'seedin' is reasonable (in perturkey=5 case only)
      if ((perturkey.eq.5).and.(abs(seedin).gt.4.0)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for parameter seedin'
            write(0,*)'               seedin=',seedin
            write(0,*)'(should not be more/less than +/-4 when perturkey=5)'
         endif
         ier=13
         goto 9999
      endif

! 1.4 Check and set latitude and longitude
      if (xlat.gt.90.0) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: wrong value for latitude=',xlat
         endif
         ier=7
         goto 9999
      else
         lat=xlat
      endif

      ! we want longitude to be in [-180:180]
      lon=mod(xlon,360.0)
      if (lon.gt.180.0) then
         lon=lon-360.
      endif

! 1.5 Perturbations

! 1.5.1 (re-)initialize EOF and GW perturbations, if instructed to do so
! Note: since on first call seedin.ne.prevseedin, the random number generator
! will always be seeded, which is good, since even when no perturbations
! are added, some extra variables need random numbers
      if (seedin.ne.prevseedin) then
         ! generate the seed for random number generation
         ! (seed must then be a strictly negative integer ::)
         seed=-abs(int(seedin))
         if (seed.eq.0) then
            seed=-1
         endif
         ! compute rdnos() for EOFs
         do i=1,dimnevecs
            rdnos(i)=gasdev(seed)
         enddo
         ! compute dev for GW
         dev=ran1(seed)
         ! store current seed in seedout
         seedout=seed
         ! set prevseedin to seedin and prevgwlength to gwlength
         prevseedin=seedin
         prevgwlength=gwlength
      endif

! 1.5.2 re-initialize only GW perturbations, if instructed to do so
      if ((gwlength.ne.prevgwlength).and. &
           ((perturkey.eq.3).or.(perturkey.eq.4))) then
         ! the random number generator necessarily has already been seeded
         ! get a new random phase for the gravity waves
         dev=ran1(seed)
         ! store current value of seed in seedout
         seedout=seed
         ! set prevgwlength to gwlength
         prevgwlength=gwlength
      endif

! 1.5.3 Check and set GW wavelength, if necessary
      if ((perturkey.eq.3).or.(perturkey.eq.4).or. (extvarkey.eq.1)) then 
         ! GW perturbations & some extra variables need 'gwlength'
         ! set wavelength 'lamda'
         if (gwlength.eq.0.0) then ! use a default wavelength
            lamda=16.E3
         else
            lamda=gwlength
            ! Note: it was checked that 2000<gwlength<30000 in 1.4
         endif
      endif

! 1.5.4 Check value of 'seedin' for the 'n times standard dev' perturbation
      nbsig=0 ! dummy initialization (to get rid of compiler warnings)
      if (perturkey.eq.5) then
         ! check that -4<seedin<+4 was done in 1.4
         nbsig=seedin
      endif


!***********************************************************************
! 1.6 Read/evaluate Ls and the corresponding season number
!***********************************************************************

! 1.6.1 Solar longitude and local time
      if (datekey.eq.0) then  ! Earth date
         !       (ie: "xdate" contains the Earth Julian date)

         ! check that "xdate" is a date that is not less than jdate_min
         ! and no more than jdate_max
         if ((xdate.lt.jdate_min).or.(xdate.gt.jdate_max)) then
            if (output_messages) then
               write(0,*)'CALL_MCD error: The given Julian date xdate=',xdate
               write(0,*)' lies outside of the [jdate_min:jdate_max] range!!'
            endif
            ier=10
            goto 9999
         endif

         ! check that user did not try to impose a localtime
         if (localtime.ne.0.0) then
            if (output_messages) then
               write(0,*)'CALL_MCD error: If using Julian dates, localtime=', &
                    ' must be set to zero!'
            endif
            ier=12
            goto 9999
         endif

         ! compute values of ls,marsau,modelday corresponding to "xdate"
         call orbit(xdate,ls,marsau,modelday)
         !       call mars_ltime with current longitude for correct local time:
         !       localtime is a "local true solar time" or "local apparent time"
         !       which is such that the sun is always highest at noon
         !      (differs from mean local time...)
         call mars_ltime(lon,xdate,ls,loctime)

      else    ! Mars date (ie: "xdate" contains the value of Ls)
         ls=real(xdate)
         ! check that the value of ls makes sense
         if ((ls.lt.0.0).or.(ls.gt.360.0)) then
            if (output_messages) then
               write(0,*)'CALL_MCD error: The given value of ls, xdate=', &
                    xdate
               write(0,*)' should not lie outside the [0:360] range!!'
            endif
            ier=9
            goto 9999
         endif

         ! find the modelday which corresponds to ls
         modelday=ls2sol(ls)

         ! get localtime from input arguments
         loctime=localtime

         ! check that localtime makes sense
         if ((loctime.lt.0.0).or.(loctime.gt.24.0)) then
            if (output_messages) then
               write(0,*)'CALL_MCD error: local time, localtime=',localtime
               write(0,*)' should not lie outside the [0:24] range!!'
            endif
            ier=11
            goto 9999
         endif

      endif ! of if (datekey.eq.0)

! 1.6.2 Compute the season numbers date corresponds to
      if (((dust.eq.4).or.(dust.eq.5).or.(dust.eq.6)).and. &
           (ls.lt.180.)) then
         if (output_messages) then
            write(0,*)'CALL_MCD Error: no dust storm scenario for Ls=',Ls
         endif
         ier=14
         goto 9999
      endif

      if (itimint.eq.0) then ! no seasonal interpolation
         call season(ls,nums)
      else if (itimint.ge.1) then
         !        if seasonal interpolation
         call season2(ls,nums,nums2,wl,wh,dust)
         ! nums, nums2, wl and wh are now set
      endif

!************************************************************************
! allocation des variables globales
!************************************************************************
      if (.not.modele_init) then
         if (associated(varrms2d))  deallocate(varrms2d,  stat=ierralloc)
         if (associated(varrms2d2)) deallocate(varrms2d2, stat=ierralloc)
         if (associated(varrms3d))  deallocate(varrms3d,  stat=ierralloc)
         if (associated(varrms3d2)) deallocate(varrms3d2, stat=ierralloc)

         if (associated(varsd2d))   deallocate(varsd2d,  stat=ierralloc)
         if (associated(varsd2d2))  deallocate(varsd2d2, stat=ierralloc)
         if (associated(varsd3d))   deallocate(varsd3d,  stat=ierralloc)
         if (associated(varsd3d2))  deallocate(varsd3d2, stat=ierralloc)

         if (associated(var_2d))    deallocate(var_2d,  stat=ierralloc)
         if (associated(var_2d2))   deallocate(var_2d2, stat=ierralloc)
         if (associated(var_3d))    deallocate(var_3d,  stat=ierralloc)
         if (associated(var_3d2))   deallocate(var_3d2, stat=ierralloc)

         if (associated(tabpcsmth)) deallocate(tabpcsmth, stat=ierralloc)
         if (associated(tabpcvar))  deallocate(tabpcvar,  stat=ierralloc)

         if (associated(tabeops))   deallocate(tabeops,  stat=ierralloc)
         if (associated(tabeot))    deallocate(tabeot,   stat=ierralloc)
         if (associated(tabeou))    deallocate(tabeou,   stat=ierralloc)
         if (associated(tabeov))    deallocate(tabeov,   stat=ierralloc)
         if (associated(tabeorho))  deallocate(tabeorho, stat=ierralloc)
! -----------------------------------------------------------
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
         allocate(tabeou(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeov(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeorho(dimloneo,dimlateo,dimlevs,dimnevecs))
! ------------------------------------------------------------
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
         tabeou(:,:,:,:) = 0.0
         tabeov(:,:,:,:) = 0.0
         tabeorho(:,:,:,:) = 0.0
! ------------------------------------------------------------
         modele_init = .true.
! ------------------------------------------------------------
      endif

!     Compute utime (true solar time at lon=0)
      call mars_ptime(lon,loctime,utime)

!***********************************************************************
! 2. load appropriate datasets from the database
!***********************************************************************

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
!       open appropriate  NETCDF file(s)
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
            ier=ierr ! error code is set in opend
            goto 9999
         endif

         !       at the first call, load hybrid coordinates and orographic data
         !       **************************************************************
         if (numsprec.eq.0) then 
            ! true for the very first call to "CALL_MCD"
            ! load hybrid coordinates
            typevar='hybr'
            call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar, &
                 aps,bps,pseudoalt,ierr) 
                    ! aps() and bps() (and pseudoalt()) are now set
            if (ierr.ne.0) then
               ier=ierr ! error value is set in loadvar
               go to 9999
            endif

            ! load orography and areoid (on GCM grid)
            typevar='orog'
            call loadvar(unet,unet,unet,unet,typevar, &
                 aps,bps,pseudoalt,ierr)
            ! taborog() and tabareo() are now set
            if (ierr.ne.0) then
               ier=ierr ! error value is set in loadvar
               go to 9999
            endif
         end if
         !numsprec.eq.0
         ! set numprec to the current value of nums
         numsprec=nums

         !       load mean variables
         !       *******************
         typevar='mean'
         call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar, &
              aps,bps,pseudoalt,ierr)
         ! var_2d(), var_2d2(), var_3d() and var_3d2() are now set
         if (ierr.ne.0) then
            ier=ierr ! error value is set in loadvar
            go to 9999
         endif
      end if !end if nums.ne.numsprec

!     if large scale perturbations or extra variables are requested,
!     load data if not yet done (reading once is enough for all season)
!     **************************************************************
      if (((perturkey.eq.2).or.(perturkey.eq.4).or.(extvarkey.eq.1)) &
           .and.(numsprec_eo.eq.0)) then
         call loadeof(ueof,ierr)
         if (ierr.ne.0) then
            ier=ierr ! error value is set in loadeof
            go to 9999
         endif
         numsprec_eo=1
      end if

!     if small scale perturbations or extra variables are requested,
!     load data if not yet done
!     **************************************************************
      if (((perturkey.eq.3).or.(perturkey.eq.4).or.(extvarkey.eq.1)) &
           .and.(nums.ne.numsprec_gw)) then
         typevar='grwp'
         call loadvar(unet,unet,unet,unet,typevar, &
              aps,bps,pseudoalt,ierr)
         if (ierr.ne.0) then
            ier=ierr ! error value is set in loadvar
            go to 9999
         endif
         numsprec_gw=nums
      end if

!     if n std dev perturbations or extra variables are requested,
!     load data if not yet done
!     ************************************************************
      if (((perturkey.eq.5).or.(extvarkey.eq.1)) &
           .and.(nums.ne.numsprec_sd)) then
         typevar='stdv'
         call loadvar(unetsd,unetsd_up,unetsd2,unetsd2_up,typevar, &
              aps,bps,pseudoalt,ierr)
         if (ierr.ne.0) then
            ier=ierr ! error value is set in loadvar
            go to 9999
         endif
         typevar='rms'
         call loadvar(unetsd,unetsd_up,unetsd2,unetsd2_up,typevar, &
              aps,bps,pseudoalt,ierr)
         if (ierr.ne.0) then
            ier=ierr ! error value is set in loadvar
            go to 9999
         endif
         numsprec_sd=nums
      end if

!***********************************************************************
!  3.  Retrieve main meteorological variables
!***********************************************************************

!  3.1 Get GCM orography for location lon,lat
      name='orography'
      call var2d(oro_gcm,lon,lat,1.0,name,ierr,itimint,wl,wh,1.0)
      ! oro_gcm is now set to the value of GCM orography at (lon,lat)

      name='areoid'
      call var2d(areoid_gcm,lon,lat,1.0,name,ierr,itimint,wl,wh,1.0)
      ! areoid_gcm is now set to the value of GCM areoid at (lon,lat)

      if (hireskey.eq.1) then
      ! get high resolution areoid at (lon,lat)
        call molareoid(dataset(1:lendataset),lon,lat,areoid_hr)
      endif

!  3.2 Get surface pressure at desired lon,lat and utime
      ! low resolution (ie: GCM) surface pressure
      name='ps'
      call var2d(ps_gcm,lon,lat,utime,name,ierr,itimint,wl,wh,1.0)

      ! high resolution (ie: using MOLA topography) surface pressure
      if (hireskey.eq.1) then
        call pres0(dataset(1:lendataset),lat,lon,ls,utime, &
                   ps_gcm,oro_gcm,wl,wh,ps_hr,oro_hr,ierr)
        if (ierr.ne.0) then
          ier=ierr ! error value set in pres0
          goto 9999
        endif
        ! ps_hr and oro_hr are now set
      oroheight=oro_hr
        areoid=areoid_hr
        ps=ps_hr
        ps_psgcm=ps_hr/ps_gcm
      else
        oroheight=oro_gcm
        areoid=areoid_gcm
        ps=ps_gcm
        ps_psgcm=1.
      endif

!  3.3 Build sigma levels
      ! GCM sigma levels
      do l=1,dimlevs
        sigma_gcm(l)=aps(l)/ps_gcm+bps(l)
      enddo

      ! high res sigma levels
      if (hireskey.eq.1) then
        call build_sigma_hr(sigma_gcm,ps_gcm,ps_hr,sigma_hr,p_pgcm)
      endif

      ! sigma levels that will be used further on
      if (hireskey.eq.1) then
        do l=1,dimlevs
          sigma(l)=sigma_hr(l)
        enddo
      else
        do l=1,dimlevs
          sigma(l)=sigma_gcm(l)
          p_pgcm(l)=1. ! No high_res, so set p_pgcm(:) to 1
        enddo
      endif

!  3.4 Get GCM temperature profile for location lon,lat and time utime
      name='temp'
      call profi(temp_gcm,lon,lat,utime,name,ierr,itimint,wl,wh)

!  3.5 Get GCM density profile for location lon,lat and time utime
      name='rho'
      call profi(rho_gcm,lon,lat,utime,name,ierr,itimint,wl,wh)

!  3.6 Compute R profile at location lon,lat and utime
      do i=1,dimlevs
        R_gcm(i)=(aps(i)+bps(i)*ps_gcm)/(rho_gcm(i)*temp_gcm(i))
      enddo

!  3.7 Find database levels encompassing sought altitude and determine
!      corresponding weights for vertical interpolation

      call getsi(xz,zkey,oroheight,areoid,ps,sigma, &
                 levhi,levlow,levweight,pratio,ierr, &
                 R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure)
      ! levhi,levlow,levweight(),pratio are now set
      ! zareoid,zradius,zsurface and zpressure are also set
      if (ierr.ne.0) then
! Note: Error message and error value are given in getsi
         ier=ierr
        goto 9999
      end if

      height=zareoid              ! height above areoid
      absheight=height-oroheight  ! height above surface

!      if underground : stop
      if (absheight.lt.-0.01) then
        if (output_messages) then
          write(0,*)'ATMEMCD Error: underground object '
          write(0,*)'               absheight=',absheight
        endif
        ier=17
        goto 9999
      endif

!  3.8 Compute mean field values (winds,density,temperature,pressure)
!      at chosen location

      name='u'
      call var3d(ul,lon,lat,levhi,levlow,levweight,pratio, &
           p_pgcm,utime,name,ierr,itimint,wl,wh)
      name='v'
      call var3d(vl,lon,lat,levhi,levlow,levweight,pratio, &
           p_pgcm,utime,name,ierr,itimint,wl,wh)
      name='temp'
      call var3d(tl,lon,lat,levhi,levlow,levweight,pratio, &
           p_pgcm,utime,name,ierr,itimint,wl,wh)
      name='rho'
      call var3d(rhol,lon,lat,levhi,levlow,levweight,pratio, &
           p_pgcm,utime,name,ierr,itimint,wl,wh)

!     Mean pressure:
      tmeanl(1)=pratio*ps*(sigma(levlow) &
           +(sigma(levhi)-sigma(levlow))*levweight(2))
      tmeanl(2)=rhol ! mean density
      tmeanl(3)=tl   ! mean temperature
      tmeanl(4)=ul   ! mean zonal wind
      tmeanl(5)=vl   ! mean meridional wind

      ! store these mean values for output
      do i=1,nmeanvar
         meanvar(i)=tmeanl(i)
      enddo

!***********************************************************************
! 4. Add perturbations, if required
!***********************************************************************

! 4.1 Add large scale variability, if required
!*********************************************
      if ((perturkey.eq.2).or.(perturkey.eq.4)) then
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
      else if(extvarkey.eq.1) then
      ! just compute for density as an extra variable
        name='rho'
        call eofpb(pertm,pertrhoeof,rdnos,lon,lat, &
              levhi,levlow,levweight,pratio,modelday,name,ierr)
      endif

! 4.2 Add small scale (gravity wave) variability, if required
!************************************************************
      if ((perturkey.eq.3).or.(perturkey.eq.4)) then
         name='u'
         absheight=height-oroheight
         call grwpb(pertu,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),ps,sigma, &
              utime,name,ierr,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)
         name='v'
         call grwpb(pertv,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),ps,sigma, &
              utime,name,ierr,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)
         name='temp'
         call grwpb(pertt,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),ps,sigma, &
              utime,name,ierr,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)
         name='rho'
         call grwpb(pertrho,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),ps,sigma, &
              utime,name,ierr,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)
         pertrhogw = pertrho
         ul = ul + pertu
         vl = vl + pertv
         tl = tl + pertt
         rhol= rhol + pertrho
      else if(extvarkey.eq.1) then
      ! just compute for density as an extra variable
        name='rho'
        call grwpb(pertrhogw,dev,lamda,lon,lat,absheight,tmeanl(2), &
              tmeanl(4),tmeanl(5),ps,sigma, &
              utime,name,ierr,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)
      endif
      
      ! 4.3 Add n sigmas, if required
      !******************************
      if (perturkey.eq.5) then
         name='rmsu'
         call var3d(pertu,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,1.0,name,ierr,itimint,wl,wh)
         name='rmsv'
         call var3d(pertv,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,1.0,name,ierr,itimint,wl,wh)
         name='rmsrho'
         call var3d(pertrho,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,1.0,name,ierr,itimint,wl,wh)
         name='rmstemp'
         call var3d(pertt,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,1.0,name,ierr,itimint,wl,wh)
         name='rmsps'
         call var2d(pertps,lon,lat,1.0,name,ierr,itimint,wl,wh,ps_psgcm)
         ul = ul + (nbsig*pertu)
         vl = vl + (nbsig*pertv)
         rhol = rhol + (nbsig*pertrho)
         tl = tl + (nbsig*pertt)
         ps = ps + (nbsig*pertps)
         ! check that this has not led to unphysical values
         if (rhol.lt.0.) then
            if (output_messages) then
               write(0,*)'CALL_MCD Error: unphysical density:',rhol
               write(0,*)'  due to addition of perturbation'
            endif
            ier=18
            goto 9999
         endif
         if (tl.lt.0.) then
            if (output_messages) then
               write(0,*)'CALL_MCD Error: unphysical temperature:',tl
               write(0,*)'  due to addition of perturbation'
            endif
            ier=19
            goto 9999
         endif
         if (ps.lt.0.) then
            if (output_messages) then
               write(0,*)'CALL_MCD Error: unphysical pressure:',ps
               write(0,*)'  due to addition of perturbation'
            endif
            ier=20
            goto 9999
         endif
      endif ! of if (perturkey.eq.5)

      ! 4.4 Store mean atmospheric fields (pressure, temperature, density, winds)
      !     calculate pressure
      pl=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow))*levweight(2))
      pl=pratio*pl

      t=tl      ! temperature
      p=pl      ! pressure
      rho=rhol  ! density
      u=ul      ! zonal wind
      v=vl      ! meridional wind

      ! 4.5 (re)compute extra variables, if required
      !*********************************************
      if (extvarkey.eq.1) then
         Rnew=p/(rho*t)
         ! Compute scale height
         pscaleheight=Rnew*t/(g0*a0**2/(a0+real(zareoid))**2)

         name='tsurf'
         call var2d(tsurf,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         call max2d(tsurfmax,lon,lat,name,ierr,itimint,wl,wh)
         call min2d(tsurfmin,lon,lat,name,ierr,itimint,wl,wh)
         name='rmstsurf'
         call var2d(rmstsurf,lon,lat,1.0,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='rmsps'
         call var2d(rmsps,lon,lat,1.0,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='co2ice'
         call var2d(co2ice,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='fluxsurf_lw'
         call var2d(fluxsurf_lw,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='fluxsurf_sw'
         call var2d(fluxsurf_sw,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='fluxtop_lw'
         call var2d(fluxtop_lw,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='fluxtop_sw'
         call var2d(fluxtop_sw,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='rmstemp'
         call var3d(rmst,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,1.0,name,ierr,itimint,wl,wh)
         name='rmsrho'
         call var3d(rmsrho,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='rmsu'
         call var3d(rmsu,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='rmsv'
         call var3d(rmsv,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='w'
         call var3d(w_l,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='rmsw'
         call var3d(rmsw,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='q2'
         call var3d(q2,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_h2ovapor'
         call var3d(vmr_h2o,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_h2oice'
         call var3d(vmr_h2oice,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_o3'
         call var3d(vmr_o3,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_o'
         call var3d(vmr_o,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_co2'
         call var3d(vmr_co2,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_co'
         call var3d(vmr_co,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='vmr_n2'
         call var3d(vmr_n2,lon,lat,levhi,levlow,levweight,pratio, &
              p_pgcm,utime,name,ierr,itimint,wl,wh)
         name='dod'
         call var2d(dod,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         call dustmix(scena,lat,Ls,dod,ps,p,dust_mmr)
         name='rmsdod'
         call var2d(rmsdod,lon,lat,1.0,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='tsddod'
         call var2d(tsddod,lon,lat,1.0,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='col_h2ovapor'
         call var2d(col_h2ovapor,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         name='col_h2oice'
         call var2d(col_h2oice,lon,lat,utime,name,ierr,itimint,wl,wh, &
              ps_psgcm)
         call air_properties(t,vmr_co2,vmr_n2,vmr_o,vmr_co, &
              Cp,gamma,viscosity,Rgas)

         !        store values of extra variables

         extvar(1)=zradius
         extvar(2)=zareoid
         extvar(3)=zsurface
         extvar(4)=oroheight
         extvar(5)=ls
         extvar(6)=loctime
         extvar(7)=utime
         extvar(8)=Cp
         extvar(9)=gamma
         extvar(10)=rmsrho
         extvar(11)=0.0
         extvar(12)=pertrhoeof
         extvar(13)=pscaleheight
         extvar(14)=oro_gcm      ! GCM orography
         extvar(15)=tsurf
         extvar(16)=tsurfmax
         extvar(17)=tsurfmin
         extvar(18)=rmstsurf
         extvar(19)=ps_gcm      ! low res Ps
         if (hireskey.eq.1) then
            extvar(20)=ps_hr     ! high res Ps
         else
            extvar(20)=0.
         endif
         extvar(21)=0.0
         extvar(22)=rmsps
         extvar(23)=rmst
         extvar(24)=rmsu
         extvar(25)=rmsv
         extvar(26)=w_l
         extvar(27)=rmsw
         extvar(28)=pertrhogw
         extvar(29)=q2
         extvar(30)=0.0
         extvar(31)=fluxsurf_lw
         extvar(32)=fluxsurf_sw
         extvar(33)=fluxtop_lw
         extvar(34)=fluxtop_sw
         extvar(35)=co2ice
         extvar(36)=dod
         extvar(37)=dust_mmr
         extvar(38)=rmsdod
         extvar(39)=tsddod
         extvar(40)=col_h2ovapor
         extvar(41)=vmr_h2o
         extvar(42)=col_h2oice
         extvar(43)=vmr_h2oice
         extvar(44)=vmr_o3
         extvar(45)=vmr_co2
         extvar(46)=vmr_o
         extvar(47)=vmr_n2
         extvar(48)=vmr_co
         extvar(49)=Rgas
         extvar(50)=viscosity
         do i=51,nextvar ! fill trailing unused slots with zeros
            extvar(i)=0.0
         enddo
      else ! i.e.: if (extvarkey.eq.0)
         extvar(1)=zradius
         extvar(2)=zareoid
         extvar(3)=zsurface
         extvar(4)=oroheight
         extvar(5)=ls
         extvar(6)=loctime
         extvar(7)=utime
         do i=8,nextvar
            extvar(i)=0.0
         enddo
      endif ! of if (extvarkey.eq.1) then

      !     copy to output
      pres=p
      dens=rho
      temp=t
      zonwind=u
      merwind=v

      return

      !     Error handling : all the outputs are set to a missing data value
9999  pres=-999.0
      dens=-999.0
      temp=-999.0
      zonwind=-999.0
      merwind=-999.0
      do i=1,nmeanvar
         meanvar(i)=-999.0
      end do
      do i=1,nextvar
         extvar(i)=-999.0
      enddo

      return
    end subroutine call_mcd ! end of call_mcd subroutine  call_mcd

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    subroutine eofpb(pertm,pertr,rdnos,lon,lat,levhi,levlow, &
         levweight,pratio,day,name,ier)

      !     Calculate an EOF perturbation on the 3d variable=name at
      !     longitude=lon, latitude=lat sigma=sigma and day=day.
      !
      !     Improved variability model...EOFs calculated in longitude-sigma plane

      implicit none

      !      include "constants_mcd.inc"

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

      !      write(*,*) "Entering eofpb, pertr:",pertr," name:",name
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
!
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
            !	 write(*,*)'norm:',norm
            do i=1,dimnevecs
               evecs(i)= tabeorho(indlon,indlat,levlow,i) &
                    +  (tabeorho(indlon,indlat,levhi,i) &
                    -  tabeorho(indlon,indlat,levlow,i))*levweight(2)
               !         write(*,*)'evecs(',i,'):',evecs(i)
            enddo
         elseif (name.eq.'ps') then
            norm=tabeonormp(indlat)
            do i=1,dimnevecs
               evecs(i)= tabeops(indlon,indlat,i)
            enddo
         else
            if (output_messages) then
               write(0,*)'EOFPB Error: ',name,' unknown variable name'
            endif
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
            if (output_messages) then
               write(0,*)'EOFPB Error: ',name,' unknown variable name'
            endif
            ier=1
            goto 9999
         endif
      endif
!
!     read smoothed PCs
!
      do i=1,dimnevecs
         pcsmth(i)=tabpcsmth(indlat,indday,i)
      enddo
!
!     read PCs variance
!
      do i=1,dimnevecs
         pcvar(i)=tabpcvar(indlat,indday,i)
      enddo
!
!     calculate perturbation
!
      pertm=0.0
      pertr=0.0
      do i=1,dimnevecs
        pertm=pertm+pcsmth(i)*evecs(i)
!	write(*,*)'i:',i,' pcvar:',pcvar(i),' rdnos:',rdnos(i),
!     &            ' evecs:',evecs(i)
        pertr=pertr+pcvar(i)*rdnos(i)*evecs(i)
!	write(*,*)' pertr:',pertr
      enddo
!
!     renormalise
!
      pertm=pertm*norm
      pertr=pertr*norm
!
! re-scaling correction for densities which are above top level
!
      if (name.eq.'rho') then
         pertm=pratio*pertm
         pertr=pratio*pertr
!	 write(*,*)'pratio:',pratio,'pertr:',pertr
      endif

      return

!     error handling
 9999 pertm = 0.0
      pertr = 0.0
      return
      end subroutine  eofpb

      ! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
      function gasdev(idum)

        !     Return a Gaussian deviate with zero mean and unit standard deviation.
        !     Uses function ran1()
        !     If idum is negative, then deviates are (re-)initialized.
        ! Note: When initialized, the function actually computes 2 deviates,
        !       'gset' and 'gasdev'. It then returns 'gasdev' and saves 'gset'
        !       which will be returned at the next call of the function.
        implicit none

        !     inputs
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
        integer :: idum    ! Seed for random number generator
        ! is altered if calls to function ran1() occur

        real    gasdev

        !     local variables
        integer :: iset    ! flag (triggers the generation of 2 deviates if =0)
        data    iset/0/
        save    iset
        real    v1      ! random number (between -1 and 1)
        real    v2      ! random number (between -1 and 1)
        real    r
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
        real    fac
        real    gset    ! extra deviate (saved for next call to function)
        save    gset

        ! called function
! jgp
!        real    ran1
! jgp

        if (idum.lt.0) iset=0  ! Reinitialize deviates generation
        if (iset.eq.0) then ! compute 2 deviates, gasdev and gset
1          v1=2.*ran1(idum)-1.
           v2=2.*ran1(idum)-1.
           r=v1*v1+v2*v2
           if ((r.ge.1.).or.(r.eq.0.)) goto 1
           fac=sqrt(-2.*log(r)/r)
           gset=v1*fac
           gasdev=v2*fac
           iset=1  ! set flag to signal there is an extra deviate at hand
        else   ! return saved deviate 'gset'
           gasdev=gset
           iset=0  ! set flag to signal there is no extra deviate at hand
        endif

        return
      end function gasdev

    
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    subroutine getsi(xz,zkey,oroheight,areoid,ps,sigma, &
         levhi,levlow,levweight,pratio,ier, &
         R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure)

      !     Find the nearest datafile levels and vertical interpolation weights at
      !     longitude=lon, latitude=lat and vertical position xz (which may be
      !     any of 'above areoid' or 'above surface'
      !      or 'radius from center of planet' or 'Pressure' coordinate)

      implicit none

      !      include "constants_mcd.inc"

      !     inputs

      real ::  xz ! altitude (see zkey)
      integer ::       zkey
      !  zkey    :                   switch to choose which z variable
      !                               is input and used to find other three :
      !                      zkey=1  zradius (m)  -> zareoid, zsurface, zpressure
      !                      zkey=2  zareoid (m)  -> zradius, zsurface, zpressure
      !                      zkey=3  zsurface (m) -> zradius, zareoid, zpressure
      !                      zkey=4  zpressure (Pa) -> zradius, zareoid, zsurface
      !                      zkey=5  case of getsi call by grwpb
      real          oroheight      !height of surface above reference areoid (m)
      real          areoid         ! reference areoid (m)
      real          ps             ! surface pressure (Pa)
      real          sigma(dimlevs) ! sigma levels
      real          R_gcm(dimlevs) ! R at GCM levels
      real          temp_gcm(dimlevs) ! temperature at GCM levels

      !     outputs
      integer ::       levhi          !database level upper bound
      integer ::       levlow         !database level lower bound
      real          levweight(2)   !level weight for interpolation
      !     (1) for linear in height (2) for linear in pressure
      real          pratio         !p/p(top, bottom) for extrapolation,
      ! 1 if in range
      integer ::       ier            !error flag (0=OK, not 0 =NOK)

      real :: zradius     ! distance to center of planet
      real :: zareoid     ! height above areoid
      real :: zsurface    ! height above surface
      real :: zpressure   ! pressure

      !     local variables
      real          height         !height of point above areoid (m)
      real          absheight         !height of point above surface (m)
      !      double precision    xlon     ! longitude (radians)
      !      double precision    xlat     ! latitude (radians)
      integer ::       l
      !      real          dsigma(dimlevs),hsigma(dimlevs)
      real          sheight(dimlevs), zsig
      !      integer zkey_height

      !     Gravity on mean areoid
      !     The areoid is defined as a surface of constant gravitational plus
      !     rotational potential. The inertial rotation rate of Mars is assumed
      !     to be 0.70882187E-4 rad/s. This potential is the mean value at the
      !     equator at a radius of 3396.000 km, namely 12652804.7 m^2/s^2,
      !     calculated from Goddard Mars Gravity Model mgm1025
      !     [LEMOINEETAL2001] evaluated to degree and order 50.
      real          g, g0, a0
      parameter (a0=3396.E3,g0=3.7257964)
      !      data          g0 /3.7257964/
      !      data          a0 /3396.E3/
      !      save g0,a0
      !      real          R
      !      data          R /191.1/
      !     R replaced by Rnew  (varies with altitude)
      !      real          Rnew(dimlevs)
      real          Tmean          ! "mean" temperature of a layer
      real          Rogct(dimlevs)
      !      character*16  name
      !      real          t(dimlevs)    ! temperature of database layers
      !      real          rho(dimlevs)  ! density of database layers
      ier=0


      !     Calculate altitude above the surface of each model layer: sheight(l)
      !     integrate hydrostatic equation
      !     Rogct is the R/g variable depending on dimlevs
      g = g0*a0**2/(a0+oroheight)**2
      Rogct(1) = R_gcm(1)/g          !Rnew(1)/g
      Tmean = temp_gcm(1)            !t(1)
      sheight(1)= - Rogct(1)*temp_gcm(1)*log(sigma(1))

      do l=2, dimlevs
         if (temp_gcm(l).ne.temp_gcm(l-1)) then
            Tmean = (temp_gcm(l)-temp_gcm(l-1)) / &
                 log(temp_gcm(l)/temp_gcm(l-1))
         else
            Tmean = temp_gcm(l)
         end if

         g = g0*a0**2/(a0+oroheight+sheight(l-1))**2
         Rogct(l) = R_gcm(l)/g
         sheight(l) = sheight(l-1) - Rogct(l)*Tmean* &
              log(sigma(l)/sigma(l-1))
      end do

      !ccccccccccccccccccccccccccccccccccccccccccccccc
      !     height calculation
      if (zkey.eq.1) then
         zradius=xz
      elseif (zkey.eq.2) then
         zareoid=xz
      elseif (zkey.eq.3) then
         zsurface=xz
      elseif (zkey.eq.4) then
         zpressure=xz
         !         compute zsurface
         if(zpressure.gt.ps) then
            if (output_messages) then
               write(0,*)'GETSI Error: underground object '
            endif
            ier=17
            goto 9999
         end if

         zsig = real(zpressure)/ps
         if (zsig.gt. sigma(1)) then
            zsurface= -log(zsig)*R_gcm(1)*temp_gcm(1)/g0
         else if (zsig.lt.sigma(dimlevs) ) then
            g = g0*a0**2/(a0+sheight(dimlevs))**2
            zsurface= sheight(dimlevs) &
                 -log(zsig/sigma(dimlevs))*R_gcm(dimlevs) &
                 *temp_gcm(dimlevs)/g
         else
            do  l=1,dimlevs
               if ((zsig.ge.sigma(l+1)).and.(zsig.le.sigma(l))) then
                  zsurface= sheight(l) + (sheight(l+1) -sheight(l)) &
                       * log(zsig/sigma(l)) / log(sigma(l+1)/sigma(l))
               end if
            end do
         end if
      endif

      ! absheight = zsurface for the first call of getsi
      ! absheight = xz for call to getsi from  grwpb
      if (zkey.eq.5) then
         absheight=xz
      else

         ! compute required zradius,zareoid and/or zsurface
         if (zkey.eq.1.) then ! zradius is known
            zareoid=zradius-areoid
            zsurface=zareoid-oroheight
         elseif (zkey.eq.2) then ! zareoid is known
            zradius=zareoid+areoid
            zsurface=zareoid-oroheight
         elseif ((zkey.eq.3).or.(zkey.eq.4)) then ! zsurface is known
            zareoid=zsurface+oroheight
            zradius=zareoid+areoid
         endif

         height=zareoid              ! height above areoid
         absheight=height-oroheight  ! height above surface
      endif

      !cccccccccccccccccccccccccccccccccccccccccccccccccc

      !     find levhi, levlow and levweight
      !     compute g (at height=zareoid)
      g = g0*a0**2/(a0+oroheight+absheight)**2
      if (absheight.lt.sheight(1)) then
         !       below the lowest layer
         levhi=1
         levlow=1
         levweight(1)=1.
         levweight(2)=0.
         pratio=exp((sheight(1)-absheight)*g/(R_gcm(1)*temp_gcm(1)))
      elseif (absheight.ge.sheight(dimlevs)) then
         !       above the top layer
         levhi=dimlevs
         levlow=dimlevs
         levweight(1)=0.
         levweight(2)=1.
         pratio=exp((sheight(dimlevs)-absheight)*g/(R_gcm(dimlevs) &
              *temp_gcm(dimlevs)))
      else
         !       general case: sheight(1)<=absheight<=sheight(dimlevs)
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

      !     Compute pressure
      zpressure=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow)) &
           *levweight(2))*pratio

      return

      !     Errror handling
9999  levhi=0
      levlow=0
      levweight(1)=0.
      levweight(2)=0.
      pratio=0.

      return
    end subroutine  getsi
      
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grid4(lon,lat,dlon,dlat,t,u)

        !     Given longitude=lon and latitude=lat find the nearest 4 horizontal
        !     gridpoints in the database and bilinear interpolation weights.
        !
        !     Grid points are arranged as follows:
        !              4 3
        !              1 2

        implicit none

        !      include "constants_mcd.inc"

        !     inputs
        real    lon      !east longitude of point
        real    lat      !latitude of point

        !     outputs
        integer :: dlon(4)  !index, along longitudes, of database points
        integer :: dlat(4)  !index, along latitudes, of database points
        real :: t        !weight (normalized longitudinal distance to point 1)
        real :: u        !weight (normalized latitudinal distance to point 1)

        !     local variables
        real    alon,alat
        integer :: i

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

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      subroutine grwpb(pert,dev,lamda,lon,lat,absheight,rho,u,v, &
           ps,sigma,utime,name,ier,itimint,wl,wh,R_gcm,temp_gcm,p_pgcm)

        !     Small scale gravity wave perturbation model.
        !     Computes perturbation on u, v, theta or rho, with wave phase factor.

        implicit none

        !      include "constants_mcd.inc"

        !     inputs
        real         dev     !phase of perturbation (rand number between 0 and 1)
        real         lamda   !vertical wavelength of g.w. (m)
        real         lon     !east longitude of point
        real         lat     !latitude of point
        real         absheight  !height (above surface) of point
        real         rho     !density at height=height
        real         u       !zonal wind at height=height
        real         v       !meridional wind at height=height
        real         ps      ! surface pressure
        real         sigma(dimlevs) ! sigma levels
        real         utime   !Universal time (0. to 24. hrs) = local time at lon=0
        integer ::      itimint !seasonal interpolation flag
        real         wl,wh   !seasonal interpolation weights
        real         R_gcm(dimlevs) ! R at GCM levels
        real         temp_gcm(dimlevs) ! temperature at GCM levels
        real         p_pgcm(dimlevs) ! high res to GCM pressure ratios
        character (LEN=16) :: name    !name of variable to perturb

        !     outputs
        integer ::      ier     !error flag (0=OK, 1=NOK)
        real         pert    !perturbation

        !     local variables
        ! zareoid,zpressure,zradius,zsurface only local
        ! we do not care of those variables here
        real         xz,zareoid,zpressure,zradius,zsurface
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
        call var3d(u0,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
             utime,tmpname,ierr,itimint,wl,wh)
        tmpname='v'
        call var3d(v0,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
             utime,tmpname,ierr,itimint,wl,wh)
        tmpname='rho'
        call var3d(rho0,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
             utime,tmpname,ierr,itimint,wl,wh)

!!! ps is now a subroutine input argument
        !      tmpname='ps'
        !      call var2d(ps,lon,lat,utime,tmpname,ierr,itimint,wl,wh)

        !     get sub-grid scale variance
        tmpname='substd'
        call var2d(sig,lon,lat,1.0,tmpname,ierr,itimint,wl,wh,1.0)

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
           xz=tmpheight
           zkey=5
           call getsi(xz,zkey,0.0,0.0,ps,sigma, &
                levhi,levlow,levweight,pratio,ierr, &
                R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure)
           tmpname='rho'
           call var3d(rho1,lon,lat,levhi,levlow,levweight, &
                pratio,p_pgcm,utime,tmpname,ierr,itimint,wl,wh)
           tmpname='u'
           call var3d(u1,lon,lat,levhi,levlow,levweight, &
                pratio,p_pgcm,utime,tmpname,ierr,itimint,wl,wh)
           tmpname='v'
           call var3d(v1,lon,lat,levhi,levlow,levweight, &
                pratio,p_pgcm,utime,tmpname,ierr,itimint,wl,wh)
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
        xz=tmpheight
        zkey=5
        call getsi(xz,zkey,0.0,0.0,ps,sigma, &
             levhi,levlow,levweight,pratio,ierr, &
             R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure)
        call var3d(a,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
             utime,tmpname,ierr,itimint,wl,wh)
        xz=tmpheight+dz
        call getsi(xz,zkey,0.0,0.0,ps,sigma, &
             levhi,levlow,levweight,pratio,ierr, &
             R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure)
        call var3d(aprime,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
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

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine loadvar(nf,nf_up,nf2,nf_up2,typevar, &
                         aps,bps,pseudoalt,ier)

!     load arrays corresponding to the variable typevar

      implicit none

!      include "netcdf.inc"
!      include "constants_mcd.inc"

! variable typevar to load :
! 2D LOW mean = tsurf,ps,co2ice,fluxsurf_lw,fluxtop_lw,fluxsurf_sw,
!               fluxtop_sw,dod,col_h2ovapor,col_h2oice! 2D values
!               q2,vmr_h2ovapor,vmr_h2oice,vmr_o3,temp,u,v,rho ! 3D low (nf=unem)
! UP     mean = temp,u,v,w,rho,vmr_o,vmr_co2,_vmr_co !3D up (nf=unem_up)
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
      integer ::      nf,nf_up,nf2,nf_up2 !NetCDF channel number
      character (LEN=4) ::  typevar ! variable 'type'
      character (LEN=16) :: name    ! variable name

!     outputs
      real aps,bps,pseudoalt
      dimension aps(dimlevs)        ! hybrid coordinate
      dimension bps(dimlevs)        ! hybrid coordinate
      dimension pseudoalt(dimlevs)  ! pseudo altitude
      integer ::      ier              ! error flag (0=OK, 1=NOK)

!     local variables
      integer ::      ierr
      integer ::      varid ! NetCDF variable ID
      integer ::      l,k,iloop,jloop
      real         aps_low(dimlevs),bps_low(dimlevs) ! hybrid coord.
      real         aps_up(dimlevs) ! hybrid coord. upper atm.
      character (LEN=50) :: varname2d(nbvar2d)
      character (LEN=50) :: varname3d(nbvar3d)
      character (LEN=50) :: sd2d(nbsd2d)
      character (LEN=50) :: sd3d(nbsd3d)
      real temp2d(dimlon,dimlat) ! temporary array


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

      if (typevar.eq.'mean') then

! MEAN 2d
        do k=1,nbvar2d
          call get_2d(nf,k,varname2d(k),1)
          call get_2d(nf2,k,varname2d(k),2)
        enddo
! MEAN  only in low file! up data extrapolated
        do k=1,nbvarlow-nbcom
          call get_3d(nf,k,varname3d(k),low,1)
          call get_3d(nf2,k,varname3d(k),low,2)
          call extrapol3d(typevar,k,up,pseudoalt)
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
          call extrapol3d(typevar,k,low,pseudoalt)
        enddo

      elseif (typevar.eq.'hybr') then

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

!       hybrid coordinates: aps() and bps()
        do l=1,low
          aps(l)=aps_low(l)
          bps(l)=bps_low(l)
        enddo
        do l=low+1,dimlevs
          aps(l)=aps_up(l-low)
          bps(l)=0
        enddo

!       pseudoalt:
        do l=1,dimlevs
          pseudoalt(l)=-10*log(aps(l)/610.+bps(l))
        enddo

      elseif (typevar.eq.'orog') then

        name='orography'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon ! taborog() is a common in constants_mcd.inc
           taborog(iloop,jloop,1)=temp2d(iloop,dimlat+1-jloop)
          enddo
        enddo

        name='areoid'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon ! tabareo() is a common in constants_mcd.inc
           tabareo(iloop,jloop,1)=temp2d(iloop,dimlat+1-jloop)
          enddo
        enddo

      elseif (typevar.eq.'grwp') then

        name='substd'
        ierr = NF_INQ_VARID(nf,name,varid)
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon ! tabsubstd() is a common in constants_mcd.inc
           tabsubstd(iloop,jloop,1)=temp2d(iloop,dimlat+1-jloop)
          enddo
        enddo

     elseif (typevar.eq.'stdv') then

        ! STD 2d
        do k=1,nbsd2d
           call getsd_2d(nf,typevar,k,"tsd"//sd2d(k),1)
           call getsd_2d(nf2,typevar,k,"tsd"//sd2d(k),2)
        enddo
        ! STD low UP extrapolated
        !      call getsd_3d(nf,typevar,1,nbsdlow-nbcomsd,"tsd"//sd3d(k),
        !     $ low)
        ! STD up and low
        do k=nbsdlow-nbcomsd+1,nbsdlow
           call getsd_3d(nf,typevar,k,"tsd"//sd3d(k),low,1)
           call getsd_3d(nf2,typevar,k,"tsd"//sd3d(k),low,2)
           call getsd_3d(nf_up,typevar,k,"tsd"//sd3d(k),up,1)
           call getsd_3d(nf_up2,typevar,k,"tsd"//sd3d(k),up,2)
        enddo
        ! STD up - low data extrapolated
        !        WARNING : uncomment these lines if to have a sd only in the
        !        low atm (not the case in version 4)
        !        do k=nbsdlow+1,nbsd3d
        !          call getsd_3d(nf_up,typevar,k,"tsd"//sd3d(k),up,1)
        !          call getsd_3d(nf_up2,typevar,k,"tsd"//sd3d(k),up,2)
        !          call extrapol3d(typevar,k,low)
        !        enddo

     elseif (typevar.eq.'rms') then

        ! RMS 2d
        do k=1,nbsd2d
           call getsd_2d(nf,typevar,k,"rms"//sd2d(k),1)
           call getsd_2d(nf2,typevar,k,"rms"//sd2d(k),2)
        enddo
        ! RMS low - up data extrapolated
        !      call getsd_3d(nf,typevar,1,nbsdlow-nbcomsd,"tsd"//sd3d(k),
        !     $ low)
        ! RMS up and low
        do k=nbsdlow-nbcomsd+1,nbsdlow
           call getsd_3d(nf,typevar,k,"rms"//sd3d(k),low,1)
           call getsd_3d(nf2,typevar,k,"rms"//sd3d(k),low,2)
           call getsd_3d(nf_up,typevar,k,"rms"//sd3d(k),up,1)
           call getsd_3d(nf_up2,typevar,k,"rms"//sd3d(k),up,2)
        enddo
        ! RMS up - low data extrapolated
        !        WARNING : uncomment these lines if to have a rms only in the
        !        low atm (not the case in version 4)
        !        do k=nbsdlow+1,nbsd3d
        !         call getsd_3d(nf_up,typevar,k,"rms"//sd3d(k),up,1)
        !         call getsd_3d(nf_up2,typevar,k,"rms"//sd3d(k),up,2)
        !         call extrapol3d(typevar,k,low)
        !        enddo

     endif !typevar rms

     return

     !     Error handling
9999 ier=16
     if (output_messages) then
        write(0,*)"LOADVAR Error : impossible to load ", &
             name," from ",typevar, "value file"
     endif
     return
   end subroutine  loadvar

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadeof(nf,ier)

        !     load EOF arrays
        implicit none

        !      include "netcdf.inc"
        !      include "constants_mcd.inc"

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
9999     ier=16
         if (output_messages) then
            write(0,*)"LOADEOF Error : failed to load ", &
                 name," from eof file"
         endif
         return

        end subroutine  loadeof
        !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine mars_ltime(lon,jdate,ls,localtime)

          !     Compute local true solar time at longitude=lon (in deg. east)
          !     at julian date jdate, also given in Ls.

          implicit none

          !     inputs
          real lon        !east longitude (in deg.)
          real (KIND=pm_reel) :: jdate    !julian date
          real ls       !aerocentric longitude (in deg.)

          !     outputs
          real localtime !local true solar time (in martian hours)

          !     local variables
          real (KIND=pm_reel) ::  LMT_0,E_T,LTST_0
          ! marsday: number of seconds in a sol (matian day)
          real (KIND=pm_reel) ::     marsday
          parameter (marsday=88775.245d0)
          !  earthday: number of seconds in a day: 24*60*60
          real (KIND=pm_reel) ::     earthday
          parameter (earthday=86400.d0)
          ! julian date of reference date: 01-01-1976 at 00:00:00
          real (KIND=pm_reel) ::     jdate_ref
          parameter (jdate_ref=2442778.5d0)
          ! LMT_ref: mean solar time at 0 deg. longitude at date jdate_ref
          real (KIND=pm_reel) ::    LMT_ref ! in martian hours
          parameter (LMT_ref=16.1725d0) ! 01-01-1976 at 00:00:00
          ! orbital eccentricity
          real (KIND=pm_reel) ::    eccentricity
          parameter (eccentricity=0.0934d0)
          ! Ls of perihelion (in degrees)
          real (KIND=pm_reel) ::    Ls_peri
          parameter (Ls_peri=250.99d0)
          ! obliquity of equator to orbit (in deg.)
          real (KIND=pm_reel) ::    obliquity
          parameter (obliquity=25.1919d0)
          real (KIND=pm_reel) ::    pi,degtorad
          parameter (pi=3.14159265358979d0)
          parameter (degtorad=pi/180.0d0)

          ! compute LMT, local mean time, (in martian hours) at longitude 0
          LMT_0=LMT_ref+24.0*(jdate-jdate_ref)*(earthday/marsday)
          do while(LMT_0.le.0.d0) ! in case jdate<jdate_ref
             LMT_0=LMT_0+24.0d0
          enddo
          LMT_0=dmod(LMT_0,24.0d0)

          ! compute equation of time (see eq. 10.17, p.419 in
          ! "satellites orbits and missions", by M. Capderou),
          ! with a 24/(2*pi) factor to express it in martian hours
          E_T=(2.d0*eccentricity*dsin((ls-Ls_peri)*degtorad) &
               -dtan(obliquity*degtorad/2.0) &
               *dtan(obliquity*degtorad/2.0)*dsin(2.0*ls*degtorad)) &
               *24.0d0/(2.0*pi)


          ! compute true solar time at longitude 0
          LTST_0=LMT_0-E_T


          ! compute local true solar time at longitude lon
          localtime=real(LTST_0) + (lon/15.0)

          localtime=mod(24.0+localtime,24.0)

          return
        end subroutine  mars_ltime

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ptime(lon,lt,ut)

        !     Convert local true solar time, lt (0..24) at east longitude (in deg.)
        !     into "universal time"  ut (0..24), the local true solar time at lon=0

        implicit none

        !     inputs
        real lon    !longitude east (in degrees)
        real lt     !local time (in martian hours)

        !     output
        real ut     !universal time  (local true solar time at lon=0)


        ut=mod(24.0+lt-lon/15.0,24.0)

        return
      end subroutine  mars_ptime

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mcd_time(ut,itime,w)

        !     From universal time ut (0..24 hrs) , find the
        !     2 nearest timestep in the database (1 - 12)
        !     and give the linear interpolation weight of itime(1)
        !
        !     Note that it is midnight universal time (and thus at longitude=0.0)
        !     at itime = 12

        implicit none

        !     input
        real ut       !universal time (0. to 24. hrs) =local time at lon=0

        !     outputs
        integer ::  itime(2)     ! 2 nearest timestep in database
        real  w            ! linear interpolation weight of itime(1)
        ! ie: w=1 if ut=itime(1) and w=0 if ut=itime(2)

        itime(1) = int(ut/2.)
        if (itime(1).eq.0) itime(1) = 12
        itime(2) = itime(1) +1
        if (itime(2).ge.13.) itime(2) = itime(2) -12

        w = 1 - ut/2. + int(ut/2.)

        return
      end subroutine  mcd_time

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine opend(unet,unetm,unetsd,unetm_up,unetsd_up,ueof, &
           num,dust,dset,ier)

        !     Open the appropriate NETCDF file corresponding to Ls, dust scenario
        !     and data set.

        implicit none

        !      include "netcdf.inc"
        !      include "constants_mcd.inc"

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
           if (output_messages) then
              write(0,*) 'pb in opend with dust= ', dust
           endif
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
           ierr=NF_OPEN(datfile,NF_NOWRITE,unetm)
           if (ierr.ne.NF_NOERR) goto 9999

!     standard deviation file
      type='sd'
      datfile=dset//scen//'_'//saison//'_'//type//'.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd)
      if (ierr.ne.NF_NOERR) goto 9999

!     thermo Mean file
      type='me'
      datfile=dset//scen//'_'//saison// &
       '_thermo_'//solar//'_'//type//'.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm_up)
      if (ierr.ne.NF_NOERR) goto 9999

!     thermo standard deviation file
      type='sd'
      datfile=dset//scen//'_'//saison// &
       '_thermo_'//solar//'_'//type//'.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd_up)
       if (ierr.ne.NF_NOERR) goto 9999

!     eof file
      type='eo'
      datfile=dset//scen//'_all_'//solar//'_eo.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,ueof)
      if (ierr.ne.NF_NOERR) goto 9999

!     mountain file
      datfile=dset//'mountain.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unet)
      if (ierr.ne.NF_NOERR) goto 9999

      return

!     Error handling
 9999 ier=15
      if (output_messages) then
         write(0,*)"Error in opend: cannot open file",datfile
      endif
      return
    end subroutine  opend

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine orbit(date,ls,marsau,outmodelday)
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
        !     Given Julian date,compute Ls, sun-Mars distance
        !     in AU and model day

        implicit none

        !     input
        real (KIND=pm_reel) ::     date   !Julian date

        !     outputs
        real       ls     !Ls: Aerocentric longitude (in deg.)
        real       marsau !Sun-Mars distance (in AU)
        real       outmodelday

        !     local variables
        real (KIND=pm_reel) ::     modelday
        ! marsday: number of seconds in a sol (matian day)
        real (KIND=pm_reel) ::     marsday
        parameter (marsday=88775.245d0)
        !  earthday: number of seconds in a day: 24*60*60
        real (KIND=pm_reel) ::     earthday
        parameter (earthday=86400.d0)
        real (KIND=pm_reel) ::     marsyear
        parameter (marsyear=668.6d0) ! number of sols in a martian year
        ! julian date for 19-12-1975 at 4:00:00, at which Ls=0.0
        real (KIND=pm_reel) :: jdate_ref
        parameter (jdate_ref=2442765.667d0)
        real (KIND=pm_reel) :: sma ! semi-major axis of orbit (in AU)
        parameter (sma=1.52368d0)
        real (KIND=pm_reel) :: eccentricity ! orbital eccentricity
        parameter (eccentricity=0.09340d0)
        real (KIND=pm_reel) :: Ls_peri ! Ls of perihelion
        parameter (Ls_peri=250.99)
        real (KIND=pm_reel) :: pi,degtorad
        parameter (pi=3.14159265358979d0)
        parameter (degtorad=pi/180.0d0)

        real       dummy

        !     convert Julian day to model day
        modelday=(date-jdate_ref)*earthday/marsday
        do while(modelday.le.0.d0) !in case date<jdate_ref
           modelday=modelday+marsyear
        enddo
        modelday=dmod(modelday,marsyear)
        dummy=real(modelday)
        outmodelday=real(modelday)

        call sol2ls(dummy,ls)

        ! compute sun-mars distance (in AU)
        marsau=real(sma*(1.0d0-eccentricity*eccentricity)/ &
             (1.0d0+eccentricity*dcos((dble(ls)-Ls_peri)*degtorad)))

        return
      end subroutine  orbit

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      subroutine profi(a,lon,lat,utime,name,ier,itimint,wl,wh)

        !     Retrieve a vertical profile of 3-d variable=name at east longitude=lon,
        !     latitude=lat and universal time=utime.
        !     horizontal bilinear interpolation is used for all variables exept
        !     density, for which the horizontal interpolation is bilinear
        !     interpolation of log(rho)

        implicit none

        !      include "constants_mcd.inc"

        !     inputs
        real         lon     ! east longitude of point
        real         lat     ! latitude of point
        real         utime   ! Universal time (0. to 24. hrs)=local time at lon=0
        character (LEN=16) :: name    ! Name of variable
        integer ::      itimint ! seasonal interpolation flag (1==yes,0==no)
        real         wl,wh   ! weights for seasonal interpolation

        !     outputs
        integer ::  ier        ! error flag (0=OK, 1=Not OK)
        real     a(dimlevs) ! Interpolated profile

        !     local variables
        integer ::  i,j,l    ! for loops
        integer ::  k,sd,rms ! flags/index of variable
        integer ::  itime(2) ! nearest MCD time indexes (1-12)
        integer ::  iut      ! temporary variable to store itime()
        integer ::  dlon(4)  ! longitude indexes of nearby MCD grid points
        integer ::  dlat(4)  ! latitude indexes of nearby MCD grid points
        real     t,u ! weights for horizontal (bilinear) interpolation
        real     w ! weight for time-of-day interpolation
        real     y(dimlevs,4) ! MCD profiles at nearby grid points
        real     x(dimlevs,2) ! horizontally interpolated y() at times itime()
        real     y2(dimlevs,4), x2(dimlevs,2)
        real     a2(dimlevs)

        !     flags sd and rms initialized to 999
        sd=999
        rms=999
        k=0
        ier=0    ! initialize error flag to 0 (== OK)
        u = 0.0
        t = 0.0

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
         if (output_messages) then
            write(0,*) 'problem using subroutine profi : the name ',name
            write(0,*) 'is not recognized'
         endif
         ier=1
         stop
      endif

      !     Retrieving variable for  "lower" season
      !     ------------------------------------------------
      !     find the 4 neighbouring MCD grid points
      call grid4(lon,lat,dlon,dlat,t,u)

      !     find nearest 2 timestep :
      call mcd_time(utime,itime,w)

      !     initialization
      do i=1,4
         do l=1,dimlevs
            y(l,i)=0
         enddo
      enddo
      
      !     loop on the 2 nearest timestep :
      do j = 1 , 2
         iut = itime(j) ! MCD time
         !        get 4 profiles at those points
         !    MEAN variables
         if (sd.eq.0) then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=var_3d(dlon(i),dlat(i),l,iut,k)
               enddo
            enddo
         endif !sd=0
         !    STD variables
         if (sd.eq.1) then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=varsd3d(dlon(i),dlat(i),l,k)
               enddo
            enddo
         endif !sd=1
         !    RMS variables
         if (rms.eq.1) then
            do i=1,4
               do l=1,dimlevs
                  y(l,i)=varrms3d(dlon(i),dlat(i),l,k)
               enddo
            enddo
         endif

         if ((name.eq.'rho').or.(name.eq.'tsdrho').or. &
              (name.eq.'rmsrho')) then
            !    bilinear interpolation of log(rho)
            do l=1,dimlevs
               x(l,j)=(1.-t)*(1.-u)*log(y(l,1))+t*(1.-u)*log(y(l,2)) &
                    +t*u*log(y(l,3))+(1.-t)*u*log(y(l,4))
               x(l,j)=exp(x(l,j))
            enddo
         else
            !    bilinear interpolation
            do l=1,dimlevs
               x(l,j)=(1.-t)*(1.-u)*y(l,1)+t*(1.-u)*y(l,2) &
                    +t*u*y(l,3)+(1.-t)*u*y(l,4)
            enddo
         endif
      enddo ! loop on 2 nearest MCD timestep

      !    linear interpolation in (hour-of-day) time :
      do l=1,dimlevs
         a(l) = w*x(l,1) + (1-w)*x(l,2)
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
            endif !sd=0
            !  STD variables
            if (sd.eq.1) then
               do i=1,4
                  do l=1,dimlevs
                     y2(l,i)=varsd3d2(dlon(i),dlat(i),l,k)
                  enddo
               enddo
            endif !sd=1
            !  RMS variables
            if (rms.eq.1) then
               do i=1,4
                  do l=1,dimlevs
                     y2(l,i)=varrms3d2(dlon(i),dlat(i),l,k)
                  enddo
               enddo
            endif
            !
            if ((name.eq.'rho').or.(name.eq.'tsdrho').or. &
                 (name.eq.'rmsrho')) then
               !        bilinear interpolation of log(rho)
               do l=1,dimlevs
                  x2(l,j)=(1.-t)*(1.-u)*log(y2(l,1))+t*(1.-u)*log(y2(l,2)) &
                       +t*u*log(y2(l,3))+(1.-t)*u*log(y2(l,4))
                  x2(l,j)=exp(x2(l,j))
               enddo
            else
               !        bilinear interpolation
               do l=1,dimlevs
                  x2(l,j)=(1.-t)*(1.-u)*y2(l,1)+t*(1.-u)*y2(l,2) &
                       +t*u*y2(l,3)+(1.-t)*u*y2(l,4)
               enddo
            endif

         enddo !do j = 1,2

         !     Linear interpolation in time :
         do l=1,dimlevs
            a2(l) = w*x2(l,1) + (1-w)*x2(l,2)
            !     Season interpolation between the two seasons
            !     --------------------------------------------
            a(l) = (wl)*a(l) + (wh)*a2(l)
         enddo

      endif !if itimint=1 (seasonal interpolation)

      return
! Unused:
! 9999 do l=1,dimlevs
!        a(l)=0.
!      enddo
!      return
    end subroutine profi

    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    subroutine season(ls,numsaison)
      !
      !***********************************************************************
      !$Type                                                                 *
      !     Subroutine FORTRAN 90                                            *
      !$Nom                                                                  *
      !      season                                                          *
      !                                                                      *
      !$Resume                                                               *
      !     Calcul le numero du mois depuis la longitude solaire             *
      !     compute the season number for a given areocentric longitude      *
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
    end subroutine  season

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
        lsc=((numsaison-1)*30)+15
        if (ls.lt.15) lsc=-15
        !     compute weightings
        wh=(ls-lsc)/30.
        wl=1-wh

        !     Dust storm scenario Special case
        if ((dust.eq.4).or.(dust.eq.5).or.(dust.eq.6))then
           if(numsaison.eq.6) numsaison=7
           if(nums2.eq.1) nums2=12
        endif

        return
      end subroutine  season2

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine sol2ls(sol,ls)

        !  convert a given martian day number (sol)
        !  into corresponding solar longitude, Ls (in degr.),
        !  where sol=0=Ls=0 is the
        !  northern hemisphere spring equinox.

        implicit none

        !     input
        real    sol
        !     output
        real    ls

        !     local variables
        real (KIND=pm_reel) ::    year_day,peri_day,timeperi,e_elips
        real (KIND=pm_reel) ::    pi,radtodeg
        ! number of martian days (sols) in a martian year
        parameter (year_day=668.6d0)
        ! perihelion date (in sols)
        parameter (peri_day=485.35d0)
        ! orbital eccentricity
        parameter (e_elips=0.09340d0)
        parameter (pi=3.14159265358979d0)
        !  radtodeg: 180/pi
        parameter (radtodeg=57.2957795130823d0)
        !  timeperi: 2*pi*( 1 - Ls(perihelion)/ 360 ); Ls(perihelion)=250.99
        parameter (timeperi=1.90258341759902d0)

        real (KIND=pm_reel) ::    zanom,xref,zx0,zdx,zteta,zz
        !  xref: mean anomaly, zx0: eccentric anomaly, zteta: true anomaly	
        integer :: iter

        zz=(sol-peri_day)/year_day
        zanom=2.*pi*(zz-nint(zz))
        xref=dabs(zanom)

        !  The equation zx0 - e * sin (zx0) = xref, solved by Newton
        zx0=xref+e_elips*dsin(xref)
        !      do 110 iter=1,10
        do iter=1,10
           zdx=-(zx0-e_elips*dsin(zx0)-xref)/(1.-e_elips*dcos(zx0))
           if(dabs(zdx).le.(1.d-7)) then ! typically, 2 or 3 iterations are enough
              goto 120
           endif
           zx0=zx0+zdx
           !110      continue
        end do
120     continue
        zx0=zx0+zdx
        if(zanom.lt.0.) zx0=-zx0

        ! compute true anomaly zteta, now that eccentric anomaly zx0 is known
        zteta=2.*datan(dsqrt((1.+e_elips)/(1.-e_elips))*dtan(zx0/2.))

        ! compute Ls
        ls=real(zteta-timeperi)
        if(ls.lt.0.) ls=ls+2.*real(pi)
        if(ls.gt.2.*pi) ls=ls-2.*real(pi)
        ! convert Ls in deg.
        ls=real(radtodeg)*ls

        return
      end subroutine  sol2ls

!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real function ls2sol(ls)

        !  Returns solar longitude, Ls (in deg.), from day number (in sol),
        !  where sol=0=Ls=0 at the northern hemisphere spring equinox

        implicit none

        !  Arguments:
        real ls

        !  Local:
        real (KIND=pm_reel) :: xref,zx0,zteta,zz
        !	xref: mean anomaly, zteta: true anomaly, zx0: eccentric anomaly
        real (KIND=pm_reel) :: year_day
        real (KIND=pm_reel) :: peri_day,timeperi,e_elips
        real (KIND=pm_reel) :: pi,degrad
        parameter (year_day=668.6d0) ! number of sols in a amartian year
        !      data peri_day /485.0/
        parameter (peri_day=485.35d0) ! date (in sols) of perihelion
        !  timeperi: 2*pi*( 1 - Ls(perihelion)/ 360 ); Ls(perihelion)=250.99
        parameter (timeperi=1.90258341759902d0)
        parameter (e_elips=0.0934d0)  ! eccentricity of orbit
        parameter (pi=3.14159265358979d0)
        parameter (degrad=57.2957795130823d0)

        if (abs(ls).lt.1.0e-5) then
           if (ls.ge.0.0) then
              ls2sol = 0.0
           else
              ls2sol = real(year_day)
           end if
           return
        end if

        zteta = ls/degrad + timeperi
        zx0 = 2.0*datan(dtan(0.5*zteta)/dsqrt((1.+e_elips)/(1.-e_elips)))
        xref = zx0-e_elips*dsin(zx0)
        zz = xref/(2.*pi)
        ls2sol = real(zz*year_day + peri_day)
        if (ls2sol.lt.0.0) ls2sol = ls2sol + real(year_day)
        if (ls2sol.ge.year_day) ls2sol = ls2sol - real(year_day)

        return
      end function  ls2sol

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var2d(a,lon,lat,utime,name,ier,itimint,wl,wh,ps_psgcm)

        !     Retrieve the value of 2-d variable 'name' at longitude=lon, latitude=lat
        !     and universal time=utime.
        !     - Bilinear interpolation is used to interpolate variable to user
        !       specified longitude and latitude. For surface pressure (name='ps'),
        !       bilinear interpolation of log(pressure) is used.
        !     - Linear interpolation is used to interpolate variable to user
        !       specified time (of day) 'utime'.
        !     - If (itimint>=1) then linear seasonal interpolation (with wl=weight
        !       of lower season and wh=weight of higher season) is then used.

        implicit none

        !      include "constants_mcd.inc"

        !     inputs
        real         lon     !longitude (east) of point
        real         lat     !latitude of point
        real         utime   !Universal time (0. to 24. hrs) =local time at lon=0
        character (LEN=16) :: name    !Name of variable
        integer ::      itimint !seasonal interpolation flag
        !  (==1 if seasonal interpolation is required)
        real         wl,wh   !seasonal interpolation weights
        real         ps_psgcm ! High res to GCM surface pressure ratio

        !     outputs
        integer ::      ier     !error flag (0=OK, 1=NOK)
        real         a       !value of variable

        !     local variables
        real         a2      !seasonal interpolation variable
        integer ::      i, j
        integer ::      k,sd,rms ! "flags" associated to variable 'name'
        integer ::      itime(2) ! nearest MCD time indexes
        integer ::      iut
        integer ::      dlon(4) ! nearest grid point longitude indexes
        integer ::      dlat(4) ! nearest grid point latitude indexes
        real         y(4)    ! to store nearest grid point values
        real         x(2)  ! bilinearly interpolated values ("lower" season)
        real         t,u   ! longitude and latitude-wise interpolation weights
        real         w     ! time-wise interpolation weight
        real         y2(4) ! to store nearest grid point values
        real         x2(2) ! bilinearly interpolated values ("higher" season)

        ier = 0
        k=0
        u = 0.0
        t = 0.0
        y(:) = 0.0
        y2(:) = 0.0
        dlat(:) = 0
        dlon(:) = 0

        !     find nearest 4 grid points
        call grid4(lon,lat,dlon,dlat,t,u)
        !     dlon() and dlat() now contain longitude and latitude indexes
        !     and t and u (interpolation weights) are consequently set
        !      if (name.eq.'ps') then
        !      write(*,*)"var2d: dlon(1)=",dlon(1),"  dlon(2)=",dlon(2)
        !      write(*,*)"var2d: dlat(1)=",dlat(1),"  dlat(4)=",dlat(4)
        !      write(*,*)"var2d: t=",t,"  u=",u
        !      endif

        !     find nearest 2 timestep :
        call mcd_time(utime,itime,w)
        !      if (name.eq.'ps') then
        !      write(*,*)"var2d: utime=",utime
        !      write(*,*)"var2d: itime(1)=",itime(1)," itime(2)=",itime(2)
        !      write(*,*)"var2d: w=",w
        !      endif

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
        elseif (name.eq.'tsdtsurf') then
           k=1
           sd=1
        else if (name.eq.'tsdps') then
           k=2
           sd=1
        elseif (name.eq.'tsddod') then
           k=3
           sd=1
        elseif (name.eq.'rmstsurf') then
           k=1
           rms=1
        else if (name.eq.'rmsps') then
           k=2
           rms=1
        elseif (name.eq.'rmsdod') then
           k=3
           rms=1
        else  
           if((name.ne.'orography').and. &
                (name.ne.'areoid').and.(name.ne.'substd')) then
           !        CASE of an unexpected name (orography, areoid and substd
           !             are treated below)
              if (output_messages) then
                 write(0,*) 'problem using subroutine var2d : the name ',name
                 write(0,*) 'is not recognized'
              endif
              stop
           endif
          !line added by atos 10/2007:     
        endif
        
          !     Retrieving variable for "lower" season
          !     --------------------------------------
          ! Note: Arrays taborog(), tabsubstd(), var_2d(),....
          !       are known from "constants_mcd.inc" and have been previously filled

          !     loop on the 2 nearest timestep :
          do j = 1 , 2
             iut = itime(j)

             !     retrieve the four values at the nearest grid points from the array
             if (name.eq.'orography') then
                do i=1,4
                   y(i)=taborog(dlon(i),dlat(i),1)
                enddo
             elseif (name.eq.'areoid') then
                do i=1,4
                   y(i)=tabareo(dlon(i),dlat(i),1)
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

             if ((name.eq.'ps').or.(name.eq.'tsdps').or. &
                  (name.eq.'rmsps')) then
                !         bilinear interpolation of log(surface pressure)
                x(j)=(1.-t)*(1.-u)*log(y(1))+t*(1.-u)*log(y(2)) &
                     +t*u*log(y(3))+(1.-t)*u*log(y(4))
                x(j)=exp(x(j))
             else
                !         bilinear interpolation in space
                x(j)=(1.-t)*(1.-u)*y(1)+t*(1.-u)*y(2) &
                     +t*u*y(3)+(1.-t)*u*y(4)
             endif
             !      if (name.eq.'ps') then
             !      write(*,*)"var2d: y(1)=",y(1)," y(2)=",y(2)
             !      write(*,*)"var2d: y(1)=",y(3)," y(2)=",y(4)
             !      write(*,*)"var2d: j=",j,"x(j)=",x(j)
             !      endif
          end do ! of do j=1,2 loop

          !     linear interpolation in time:
          a = w*x(1) + (1-w)*x(2)

          !      if (name.eq.'ps') then
          !      write(*,*)"var2d: x(1)=",x(1),"x(2)=",x(2)
          !      write(*,*)"var2d: season1, a=",a
          !      endif

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
                elseif (name.eq.'areoid') then
                   do i=1,4
                      y2(i)=tabareo(dlon(i),dlat(i),1)
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

                if ((name.eq.'ps').or.(name.eq.'tsdps').or. &
                     (name.eq.'rmsps')) then
                   !          bilinear interpolation of log(surface pressure)
                   x2(j)=(1.-t)*(1.-u)*log(y2(1))+t*(1.-u)*log(y2(2)) &
                        +t*u*log(y2(3))+(1.-t)*u*log(y2(4))
                   x2(j)=exp(x2(j))
                else
                   !          bilinear interpolation in space
                   x2(j)=(1.-t)*(1.-u)*y2(1)+t*(1.-u)*y2(2) &
                        +t*u*y2(3)+(1.-t)*u*y2(4)
                endif
             end do  ! of do j=1,2 loop

             !       Linear interpolation in utime for higher season
             a2 = w*x2(1) + (1-w)*x2(2)

             !     Season interpolation between the two seasons
             !     --------------------------------------------
             a = wl*a + wh*a2

             !      if (name.eq.'ps') then
             !      write(*,*)"var2d: season2, a2=",a2
             !      write(*,*)"var2d: wl=",wl," wh=",wh
             !      write(*,*)"var2d: a=wl*a + wh*a2=",a
             !      endif

          endif ! of if (itimint.ge.1)


          !     Multiply (ie: rescale) by ps_psgcm for some variables
          if ((name.eq.'dod').or.(name.eq.'tsddod').or. &
               (name.eq.'rmsdod').or.(name.eq.'rmsps').or. &
               (name.eq.'tsdps').or.(name.eq.'col_h2ovapor')) then
             a=a*ps_psgcm
          endif

          return

          !     error handling (unused yet)
          ! 9999 a=0.
          !      return
        end subroutine var2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine interpol(k,up_low,type,pseudoalt)

!     calculate the vertical interpolation between low and up atmosphere
!     when data are not in both files (low and thermo)

      implicit none

!      include "constants_mcd.inc"

!     inputs
      integer ::    k,up_low
      character (LEN=4) :: type
      real       pseudoalt
      dimension  pseudoalt(dimlevs)

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
      subroutine extrapol3d(type,k,up_low,pseudoalt)

        !  extrapolate 3d data when not in the initial netcdf files
        !  call subroutine interpol

        implicit none

        !      include "constants_mcd.inc"
        !      include "netcdf.inc"

        !     inputs
        integer ::     k,up_low
        character (LEN=4) :: type
        real        pseudoalt
        dimension   pseudoalt(dimlevs)

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
           call  interpol(k,up,type,pseudoalt)

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
           call  interpol(k,low,type,pseudoalt)

        endif! up_low

        return
      end subroutine  extrapol3d

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_2d(nf,k,varname,order)

        !  get_2d reads the 2d mean variables in low netcdf files.
        !  Fills var_2d() and var_2d2() (defined in constants_mcd.inc)

        implicit none

        !      include "constants_mcd.inc"
        !      include "netcdf.inc"

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
           if (output_messages) then
              write(0,*) "GET_2D Error:"
              write(0,*) NF_STRERROR(ierr), varname
           endif
           stop
        endif
        ierr = NF_GET_VAR_REAL(nf,var2didin(k),temp2d)
        if(ierr.ne.NF_NOERR) then
           if (output_messages) then
              write(0,*) "GET_2D Error:"
              write(0,*) NF_STRERROR(ierr)
           endif
           stop
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

        !  getsd_3D reads the 3D mean variables in up or low netcdf files.
        !  Fills var_3d() and var_3d2() (defined in constants_mcd.inc)

        implicit none

        !      include "constants_mcd.inc"
        !      include "netcdf.inc"

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
           if (output_messages) then
              write(0,*) "GET_3D Error:"
              write(0,*) NF_STRERROR(ierr),varname
           endif
           stop
        endif
        if (up_low.eq.up) then
           ierr = NF_GET_VAR_REAL(nf,var3didin(k),temp3dup)
           if(ierr.ne.NF_NOERR) then
              if (output_messages) then
                 write(0,*) "GET_3D Error:"
                 write(0,*) NF_STRERROR(ierr)
              endif
              stop
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
              if (output_messages) then
                 write(0,*) "GET_3D Error:"
                 write(0,*) NF_STRERROR(ierr)
              endif
              stop
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

        endif ! of if (up_low.eq.up)

        return
      end subroutine  get_3d

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsd_2d(nf,type,k,varname,order)

        !  getsd_2D reads the 2D RMS ans STD variables in up or low netcdf files

        implicit none

        !      include "constants_mcd.inc"
        !      include "netcdf.inc"

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
           if (output_messages) then
              write(0,*) "GETSD_2D Error:"
              write(0,*) NF_STRERROR(ierr),varname
           endif
           stop
        endif
        ierr = NF_GET_VAR_REAL(nf,sd2didin(k),temp2d)
        if(ierr.ne.NF_NOERR) then
           if (output_messages) then
              write(0,*) "GETSD_2D Error:"
              write(0,*) NF_STRERROR(ierr) , 'stop in getsd3d'
           endif
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

        !      include "constants_mcd.inc"
        !      include "netcdf.inc"

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
           if (output_messages) then
              write(0,*) "GETSD_3D Error:"
              write(0,*) NF_STRERROR(ierr),varname
           endif
           stop
        endif
        if (up_low.eq.up) then
           ierr = NF_GET_VAR_REAL(nf,sd3didin(k),temp3dup)
           if(ierr.ne.NF_NOERR) then
              if (output_messages) then
                 write(0,*) "GETSD_3D Error:"
                 write(0,*) NF_STRERROR(ierr)
              endif
              stop
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
              if (output_messages) then
                 write(0,*) "GETSD_3D Error:"
                 write(0,*) NF_STRERROR(ierr)
              endif
              stop
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
      subroutine var3d(a,lon,lat,levhi,levlow,levweight,pratio,p_pgcm, &
           utime,name,ier,itimint,wl,wh)

        !     Retrieve the value of 3-d variable=name at longitude=lon, latitude=lat
        !     and Universal time=utime. The height of the variable is controlled by
        !     levhi, levlow and levweight.  Use bilinear interpolation to get user
        !     specified longitude and latitude.

        implicit none

        !      include "constants_mcd.inc"

        !     inputs
        real         lon        !longitude (east) of point
        real         lat         !latitude of point
        integer ::      levhi       !database level upper bound
        integer ::      levlow      !database level lower bound
        real         levweight(2)!level weight for vertical interpolation
        !     (1) for linear in height (2) for linear in pressure
        real         pratio   !ratio of pressure to extreme value if out of range
        real         p_pgcm(dimlevs) ! high res to GCM pressure ratios
        real         utime    !Universal time (0. to 24. hrs)=local time at lon=0
        integer ::      itimint  !seasonal interpolation flags
        real         wl,wh    !seasonal interpolation weightings
        character (LEN=16) :: name     !Name of variable

        !     outputs
        integer ::       ier        !error flag (0=OK, 1=NOK)
        real          a          !value of variable

        !     local variables
        real    profile(dimlevs) !profile in MCD grid

        ier = 0

        ! get horizontally interpolated GCM vertical profile of variable 'name'
        call profi(profile,lon,lat,utime,name,ier,itimint,wl,wh)

        if (ier.ne.0) then
           if (output_messages) then
              write(0,*)'VAR3D Error : profile not available for variable' &
                   ,name
           endif
           ier=1
           goto 9999
        endif

        !      write(*,*)
        !      write(*,*) "in var3d: name=",name

        if ((name.eq.'rho').or.(name.eq.'tsdrho').or. &
             (name.eq.'rmsrho')) then
           !     interpolate densities linearly in pressure
           !         write(*,*) "profile(levlow)=",profile(levlow)
           !         write(*,*) "profile(levhi)=",profile(levhi)
           !         write(*,*) "p_pgcm(levlow)=",p_pgcm(levlow)
           !         write(*,*) "p_pgcm(levhi)=",p_pgcm(levhi)
           a=profile(levlow)*p_pgcm(levlow)+levweight(2)* &
                (profile(levhi)*p_pgcm(levhi)-profile(levlow)*p_pgcm(levlow))
           !     correction for densities which are out of sigma range
           a=pratio*a
           !         write(*,*) "a=",a
        else ! most variables are interpolated linearly in height
           !         write(*,*) "profile(levlow)=",profile(levlow)
           !         write(*,*) "profile(levhi)=",profile(levhi)
           a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
           !         write(*,*) "a=",a
        endif
        !
        return

        !     Error handling
9999    a=0.
        return
      end subroutine  var3d

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine max2d(a,lon,lat,name,ier,itimint,wl,wh)

        !     Retrieve the daily maximum value of 2-d variable=name at
        !     longitude=lon, latitude=lat

        implicit none

        !      include "constants_mcd.inc"

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
           utime=float(i)
           call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh,1.0)
           if(ier.ne.0) then
              ier=1
              if (output_messages) then
                 write(0,*)"MAX2D Error : impossible to find ",name, &
                      " maximum"
              endif
              goto 9999
           endif
           if (x.gt.xmax) xmax=x
        enddo
        a=xmax

        return

        !     Error handling
9999    a=0.
        return
      end subroutine  max2d

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine min2d(a,lon,lat,name,ier,itimint,wl,wh)

        !     Retrieve the daily minimum value of 2-d variable=name at
        !     longitude=lon, latitude=lat

        implicit none

        !      include "constants_mcd.inc"

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
           utime=float(i)
           call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh,1.0)
           if(ier.ne.0) then
              ier=1
              if (output_messages) then
                 write(0,*)"MIN2D Error : impossible to find ",name, &
                      " minimum"
              endif
              goto 9999
           endif
           if (x.lt.xmin) xmin=x
        enddo
        a=xmin
        return

        !     Error handling
9999    a=0.
        return
      end subroutine  min2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
      function ran1(idum)
        !     "Minimal" random number generator of Park and Miller with Bays-Durham
        !     shuffle and added safeguards
        !     Return a uniform random deviate between 0.0 and 1.0 (exclusive of the
        !     endpoints values). Call with idum a negative integer to initialize;
        !     thereafter, do not alter idum between successive deviates in a sequence.
        !     RNMX should approximate the largest floating value that is less than 1.

        implicit none

        ! input:
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
        integer :: idum ! Seed for random number generator (if negative)
        ! the value of idum is altered by the function

        ! returned value:
        real ran1

        ! local variables:
        integer :: IA,IM,IQ,IR,NTAB,NDIV
        real AM,EPS,RNMX
        parameter (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
             NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
        integer :: j,k
        integer :: iv(NTAB),iy
        save iv,iy
        data iv /NTAB*0/, iy /0/

        if ((idum.le.0).or.(iy.eq.0)) then
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
        ran1=min(AM*iy,RNMX)

        return
      end function ran1

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine dustmix(scena,lat,Ls,dod,psurf,p,dust_mmr)

        !     Subroutine used to reconstruct the dust mixing ratio
        !     assumed in the GCM run, at a given pressure level.
        !     F. Forget 2005

        implicit none

        !      include "constants_mcd.inc"

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


        topdust=0. ! dummy initialization to get rid of compiler warnings

        !     initialisation
        !     --------------
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
                -(32+18*zlsconst)*sin(xlat)**4 &
                - 8*zlsconst*(sin(xlat))**5
        else
           if (output_messages) then
              write(0,*) "DUST_MMR error: problem with scena"
           endif
           stop
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
           Cp_co  = 29
           Cp_n2 = 29
           Cp_ar = 20
           Cp_o = 21
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
        gamma = 1./(1-Rgas/Cp)

        !     Computing viscosity
        !     -------------------
        !     Therma molecular Conductivity (W m-1 K-1)
        conduct = (A_co2*vmr_co2+A_n2*vmr_n2+A_o*vmr_o &
             + A_co*vmr_co + A_ar*vmr_ar)/Svmr
        conduct=T**0.69 * conduct
        viscosity = conduct / (0.25*(4*Cp + 5*Rgas))

        return
      end subroutine  air_properties
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine build_sigma_hr(sigma_gcm,ps_gcm,ps_hr,sigma_hr,p_pgcm)

        implicit none
        !      include "constants_mcd.inc"

        !     inputs
        real sigma_gcm(dimlevs) ! GCM sigma levels
        real ps_gcm             ! GCM surface pressure
        real ps_hr              ! High res surface pressure
        !     outputs
        real sigma_hr(dimlevs)  ! High res sigma levels
        real p_pgcm(dimlevs)    ! high res to GCM pressure ratios

        !     local variables
        integer :: l
        real x  ! lower layer compression (-0.9<x<0) or dilatation (0.<x<0.9)
        real rp     ! surface pressure ratio ps_hr/ps_gcm
        real deltaz   ! corresponding pseudo-altitude difference (km)
        real f  ! coefficient f= p_hr / p_gcm
        real z  ! altitude of transition of p_hr toward p_gcm (km)

        ! 1. Coefficients
        rp=ps_hr/ps_gcm
        deltaz=-10.*log(rp)
        x = min(max(0.12*(abs(deltaz)-1.),0.),0.8)
        if(deltaz.gt.0) x=-x
        z=max(deltaz + 3.,3.)

        do l=1,dimlevs
           f=rp*sigma_gcm(l)**x
           !        f=f+(1-f)*0.5*(1+tanh(6.*(-10.*log(sigma_gcm(l))-z)/z))
           !        sigma_hr(l)=f*sigma_gcm(l)/rp
           p_pgcm(l)=f+(1-f)*0.5*(1+tanh(6.*(-10.*log(sigma_gcm(l))-z)/z))
           sigma_hr(l)=p_pgcm(l)*sigma_gcm(l)/rp
        enddo

      end subroutine  build_sigma_hr

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine pres0(dset,lat,lon,solar,utime,ps_MCD,oro_MCD,wl,wh, &
           pres,alt,ierr)
        ! ###############################################################"
        !  Pres0 si a subroutine designed to determine accurate
        !  surface pressure (as well as MOLA altitude) on Mars at
        !  any given point in latitude, longitude, local time and solar longitude. It
        !  uses:
        !     1) Reference pressure measurements at Viking Lander 1 site
        !     2) High resolution MOLA topography
        !     3)  data issued from the Mars Climate Database (file ps_spline.nc) but is
        !  independant from it, so you don't need the MCD to be installed to
        !  use pres0.
        !  F. Forget, Y. Wanherdrick, LMD 2002
        !  Interpolation improvement and code acceleration F.Forget 10/2005 02/2006
        ! ###############################################################"

        implicit none

        !      include "constants_mcd.inc"

        ! inputs:
        character (LEN=*) :: dset ! Path to MCD datafiles
        real lon      ! Longitude coordinate of the point (East degrees)
        real lat      ! Latitude coordinate of the point (North degrees)
        real solar    ! Solar longitude (degrees)
        real utime    ! Universal time (hours) (time at lon=0)
        real ps_MCD   ! surface pressure (from MCD)
        real oro_MCD  ! orography from MCD
        real wl,wh    ! season-wise interpolation weights

        ! outputs:
        real pres     ! high resolution surface pressure (Pa)
        real alt      ! surface altitude from MOLA (m)
        integer :: ierr  ! Control variable

        ! local variables:
        real factcor  ! Pressure correction factor
        real zlon     ! East longitude (deg.) [0:360]
        real H        ! Scale height (m)
        real temp     ! Temperature at altitude ~1km
        !      real oro
        character (LEN=16) :: name    !Name of variable to retrieve from MCD
        integer :: ier
        real profile(dimlevs) ! to temporarily store temperature profile
        integer :: ref_alt ! reference altitude index for temperature retrieval
        parameter (ref_alt=7)

        ! Ensure that (local) longitude zlon is in [0:360]
        ! since given "lon" is in [-180:180]

        zlon=lon
        if(zlon.lt.0.) zlon =zlon + 360.


        ierr=0

        ! 1. Read MOLA orography
        call mola(dset,lat,zlon,alt,ierr)
        if (ierr.ne.0) return

        ! 2. Compute a correction factor using VL1 pressure
        call calc_factcor(dset,solar,wl,wh,factcor,ierr)
        if (ierr.ne.0) return

        ! 3. Get MCD temperature (at ~1km above surface)
        name='temp'
        call profi(profile,lon,lat,utime, &
             name,ier,1,wl,wh)
        ! keep temperature of ref_alt layer (~1km)
        temp=profile(ref_alt)

        ! 4. Build Scale height H=R.T/g
        ! Gas Constant: R (m2.s-2.K-1) = 190
        ! surface gravity: g (m.s-2) = 3.73
        H=190*temp/3.73

        ! 5. Compute topography-corrected surface pressure
        pres=ps_MCD*factcor*exp(-(alt-oro_MCD)/H)

      end subroutine  pres0

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine read_vl(dset,tab,ierr)
        !Routine to read the VL1 or VL2 mean viking pressure files.
        implicit none
        !      include "constants_mcd.inc"

        ! Arguments:
        ! Inputs:
        character (LEN=*) :: dset ! path to MCD datafiles
        real tab(669,2) ! tab(:,1) contains Ls and tab(:,2) contains pressure
        ! Output:
        integer :: ierr ! returned status code (==0 if OK)

        ! local variables
        integer :: vik_num,count
        character (LEN=140) :: vlfile ! (smoothed) surface pressure measured by VL
        !      data vlfile/'MCD_DATA/VL1.ls'/

        ! 1. Open file VL1.ls
        vik_num=18
        vlfile=dset//'VL1.ls'
#ifdef __GFORTRAN__
        open(vik_num,file=vlfile,status="old",iostat=ierr, &
             convert='big_endian', form="formatted")
#else
        open(vik_num,file=vlfile,status="old",iostat=ierr, &
             form="formatted")
#endif

        if (ierr.ne.0) then
           if (output_messages) then
              write(0,*) "READ_VL Error: Failed opening file ",vlfile
           endif
           return
        endif

        ! 2. Read the 669 lines of file
        do count=1,669
           read(vik_num,*) tab(count,1),tab(count,2)
        enddo

        ! 3. Close file and return
        close (vik_num)

      end subroutine  read_vl

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine calc_factcor(dset,solar,wl,wh,factcor,ierr)
        ! Calculate a correction factor between Viking1 surface pressure data
        ! and MCD output at the same point

        implicit none
        !      include "constants_mcd.inc"

        ! Arguments:
        !     inputs:
        character (LEN=*) :: dset ! path to MCD datafiles
        real solar	 ! Solar longitude (degrees)
        real wl,wh         ! weights for interpolation
        !     outputs:
        real factcor	! correction factor
        integer :: ierr	! status ierr=0 if everything OK

        ! Local variables
        ! Viking 1 coordinates
        real vik1_lat, vik1_lon, vik1_alt
        parameter(vik1_lat=22.2692, vik1_lon=311.7783, vik1_alt=-3627)
        ! Viking2 coordinates
        !      real vik2_lat, vik2_lon, vik2_alt
        !      parameter(vik2_lat=47.6680, vik2_lon=134.0100, vik2_alt=-4505)
        !  Pathfinder coordinates
        !      real path_lat, path_lon, path_alt
        !      parameter(path_lat=19.0949, path_lon=326.4762, path_alt=-3682)

        real vik1_MCD_oro ! MCD orography at VL1 coordinates
        real prescalc(12) ! surface pressure at VL1 site, at times 2,4,...24 hours
        real tempcalc(12) ! temperature above VL1, at times 2,4,...24 hours
        integer :: ref_alt ! reference altitude index for temperature retrieval
        parameter (ref_alt=7)
        real H        ! scale height
        real presmean ! mean value (over a day) of surface pressure
        real lsinf,lssup,psinf,pssup,presvik !,alt
        integer :: iloop
        logical :: firstcall
        data firstcall/.true./
        save firstcall
        real vik_tab(669,2)
        save vik_tab

        character (LEN=16) :: name    !Name of variable to retrieve from MCD
        integer :: ier
        real profile(dimlevs) ! to temporarily store temperature profile

        ! 1. If first call, load (smoothed) VL1 measured pressure

        if (firstcall) then
           firstcall=.false.
           call read_vl(dset,vik_tab,ierr)
           if (ierr.ne.0) return
        endif

        ! 2. Read surface pressure and temperature (1km above surface) from MCD,
        !    at VL1 site, at universal times (of day) 2, 4, ... 24 hours.

        ! 2.1 Read pressure and temperature from MCD at utime 2, 4, ... 24
        do iloop=1,12
           name='ps'
           call var2d(prescalc(iloop),vik1_lon-360.,vik1_lat,real(2*iloop), &
                name,ier,1,wl,wh,1.0)
           ! retrieve temperature profile
           name='temp'
           call profi(profile,vik1_lon-360.,vik1_lat,2.0*iloop, &
                name,ier,1,wl,wh)
           ! keep temperature of ref_alt layer (~1km)
           tempcalc(iloop)=profile(ref_alt)
           !        write(*,*)'CALC_FACTCOR: i=',iloop,
           !     &            ' prescalc(i)=',prescalc(iloop),
           !     &            ' tempcalc(i)=',tempcalc(iloop)
        enddo

        ! 2.2 Get MCD orography corresponding to VL1 site
        name='orography'
        call var2d(vik1_MCD_oro,vik1_lon-360.,vik1_lat,0.0, &
             name,ier,1,wl,wh,1.0)
        !      write(*,*) 'CALC_FACTCOR: vik1_MCD_oro=',vik1_MCD_oro


        ! 3. Compute the correction factor due to difference between orocalc
        !    and viking1 altitude

        do iloop=1,12
           H=190*tempcalc(iloop)/3.73
           prescalc(iloop)=prescalc(iloop)*exp(-(vik1_alt-vik1_MCD_oro)/H)
        enddo

        ! 4. Compute average (over the day) pressure
        presmean=0.
        do iloop=1,12
           presmean = presmean + prescalc(iloop)/12.
        enddo
        !      write(*,*)'CALC_FACTCOR: presmean=',presmean

        ! 5. Get encompassing VL1 pressure data indexes and values
        if (solar.le.vik_tab(1,1)) then
           lsinf=vik_tab(669,1)-360
           lssup=vik_tab(1,1)
           psinf=vik_tab(669,2)
           pssup=vik_tab(1,2)
        else if (solar.ge.vik_tab(669,1)) then
           lsinf=vik_tab(669,1)
           lssup=vik_tab(1,1)+360
           psinf=vik_tab(669,2)
           pssup=vik_tab(1,2)
        else
           iloop=1
           do while ((vik_tab(iloop,1).lt.solar).and.(iloop.lt.669))
              iloop=iloop+1
           enddo
           lsinf=vik_tab(iloop-1,1)
           lssup=vik_tab(iloop,1)+360
           psinf=vik_tab(iloop-1,2)
           pssup=vik_tab(iloop,2)
        endif

        ! 6. Interpolate VL1 pressure data to solar longitude 'solar'

        ! iloop-1 is the "lower limit" and iloop the "upper limit" in tab_vik
        presvik=psinf+(solar-lsinf)*(pssup-psinf)/(lssup-lsinf)
        !      write(*,*)'CALC_FACTCOR: presvik=',presvik

        ! 7. Compute the pressure correction factor
        factcor=presvik/presmean

      end subroutine  calc_factcor

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mola(dset,latitude,longitude,alt,ierr)
        !Give the MOLA altitude (alt), given lat and lon coordinates
        !Using bilinear interpolation from 32 pixels/degree MOLA file

        USE netcdf_f90

        implicit none

        !      include "netcdf.inc"
        !      include "constants_mcd.inc"

        ! Arguments
        ! inputs
        character (LEN=*) :: dset ! Path to MCD datafiles
        real latitude  ! north latitude (degrees)
        real longitude ! east longitude (degrees)
        ! outputs
        real alt     ! above areoid altitude of surface (meters)
        integer :: ierr ! returned status code (==0 if OK)

        ! Local variables

        logical :: firstcall
        data firstcall/.true./
        save firstcall
        character (LEN=140) :: molafile ! MOLA datafile
        !      data molafile/'mola_32.2.nc'/
        !      data molafile/'mola16.nc'/
        !      data molafile/'mola32.nc'/
        real resol
        !      parameter(resol=16) ! MOLA pixel/degree resolution
        parameter(resol=32)
        !      real invresol
        !      parameter(invresol=1./resol)
        integer :: jjm, iim   ! # of longitude and latitude MOLA data values
        parameter(jjm=int(180*resol), iim=2*jjm)
        integer*2 :: altmola(iim,jjm) ! MOLA altitude dataset
        save altmola

        integer (kind=2) :: mintopo_check,maxtopo_check ! known min and max of MOLA dataset
        !      parameter(mintopo_check=-8156,maxtopo_check=21191) ! mola_32.2.nc
        !      parameter(mintopo_check=-8177,maxtopo_check=21191) ! mola16.nc
        parameter(mintopo_check=-8206,maxtopo_check=21191) ! mola32.nc
        real dlat, dlon ! , lontmp
        integer :: i,j ! ,count
        real topo(4) ! neighboring MOLA points (for bilinear interpolation)
        integer :: latsup,latinf,loninf,lonsup ! indexes of neighboring points
        integer (kind=2) :: mintopo, maxtopo ! min and max of read dataset
        integer :: nid,nvarid ! NetCDF file and variable IDs
        real colat ! colatitude
        real lat,lon ! longitude and latitude, local values (in degrees)

        ! 1. Load MOLA dataset upon first call
        !    (dataset is stored in a 'save' array)

        if (firstcall) then
           firstcall=.false.
           ! 1.1. Open MOLA file
           molafile=dset//'mola32.nc'
           ierr = NF_OPEN (molafile, NF_NOWRITE,nid)
           if (ierr.NE.NF_NOERR) then
              if (output_messages) then
                 write(0,*)"Error in mola: Could not open file ",molafile
              endif
              ierr=15 !set appropriate error code
              return
           endif

           ! 1.2. Load data
           ierr = NF_INQ_VARID (nid, "alt", nvarid)
           ! note that MOLA "alt" are given as "short" (16 bits integer ::
!ko           ierr = NF_GET_VAR_INT2(nid, nvarid, altmola(iim,jjm))
           ierr = NF_GET_VAR_INT2(nid, nvarid, altmola)
           if (ierr.ne.NF_NOERR) then
              if (output_messages) then
                 write(0,*)"Error in mola: <alt> not found"
              endif
              ierr=16 ! set appropriate error code
              return
           endif


           ! 1.3. Close MOLA file
           ierr=NF_CLOSE(nid)

           ! 1.4 Check that the MOLA dataset was coorectly loaded

           mintopo=mintopo_check
           maxtopo=maxtopo_check
           do i=1,iim
              do j=1,jjm
                 mintopo=min(mintopo,altmola(i,j))
                 maxtopo=max(maxtopo,altmola(i,j))
              enddo
           enddo
           if ((mintopo.ne.mintopo_check).or. &
                (maxtopo.ne.maxtopo_check)) then
              if (output_messages) then
                 write(0,*)"***ERROR Mola file ",molafile," is not well read"
                 write(0,*) "Minimum found: ", mintopo
                 write(0,*) "Minimum should be:",mintopo_check
                 write(0,*) "Maximum found: ", maxtopo
                 write(0,*) "Maximum should be:",maxtopo_check
              endif
              ierr=16
              return
           endif
        endif ! End of if(firstcall)

        ! 2. Check that input longitude and latitude make sense
        lat=latitude
        if((lat.gt.90).or.(lat.lt.-90)) then
           if (output_messages) then
              write(0,*)"Error in mola: Wrong value for latitude"
           endif
           stop
        endif

        ! longitude must range from 0 to 360
        lon=longitude
        do while(lon.gt.360.)
           lon=lon-360.
        enddo

        do while(lon.lt.0.)
           lon=lon+360.
        enddo

        ! 3. Identify the four neighboring points from MOLA dataset
        !    These points are arranged as follows:  1 2
        !                                           3 4

        colat=90-lat

        if (colat.lt.1./(2.*resol)) then
           latsup=1
           latinf=1
           dlat=0
        else if (colat.gt.180.-1./(2.*resol)) then
           latsup=jjm
           latinf=jjm
           dlat=0
        else
           latsup=1+int((colat-1./(2.*resol))*resol)
           latinf=latsup+1
           dlat=1-(colat-(1./(2.*resol)+(latsup-1)/resol))*resol
        endif
        ! Note: dlat is the (normalized) "latitudinal distance" to point 3
        !       ie: dlat=0 if lat=latitude of point 3
        !           dlat=1 if lat=latitude of point 1

        if ((lon.lt.1./(2.*resol)).or.(lon.ge.(360-1./(2.*resol)))) then
           loninf=iim
           lonsup=1
           if (lon.lt.1./(2.*resol)) then
              dlon=lon*resol+0.5
           else
              dlon=(lon-(1./(2.*resol)+(loninf-1)/resol))*resol
           endif
        else
           if (((lon-1./(2.*resol))*resol).ge.0) then
              loninf=1+int((lon-1./(2.*resol))*resol)
           else
              loninf=int((lon-1./(2.*resol))*resol)
           endif
           lonsup=loninf+1
           dlon=(lon-(1./(2.*resol)+(loninf-1)/resol))*resol
        endif
        ! Note: dlon is the (normalized) "longitudinal distance" to point 3
        !       ie: dlon=0 if lon=longitude of point 3
        !           dlon=1 if lon=longitude of point 4

        if((dlat.gt.1).or.(dlon.gt.1).or.(dlat.lt.0).or.(dlon.lt.0)) then
           if (output_messages) then
              write(0,*)"Error in mola: on dlat or dlon"
              write(0,*) "dlat: ",dlat
              write(0,*) "lat: ",lat
              write(0,*) "dlon: ",dlon
              write(0,*) "lon: ",lon
           endif
           ierr=-5
           return
        endif

        ! 4. Interpolation

        ! Four nearest points are aranged as follows:  1 2
        !                                              3 4
        topo(1)=real(altmola(loninf,latsup))
        topo(2)=real(altmola(lonsup,latsup))
        topo(3)=real(altmola(loninf,latinf))
        topo(4)=real(altmola(lonsup,latinf))

        ! Use bilinear interpolation to evaluate alt

        alt=(1.-dlon)*(1.-dlat)*topo(3)+(1.-dlat)*dlon*topo(4) &
             +dlat*(1.-dlon)*topo(1)+dlat*dlon*topo(2)

      end subroutine  mola

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine molareoid(dset,lon,lat,rareoid)
        ! This subroutine returns the radial position (ie: distance to the center
        ! of Mars) of the reference areoid for a given position (given values of
        ! longitude and latitude)
        ! Based on "areoid.f" program by G. Neumann, which is available at
        ! ftp://ltpftp.gsfc.nasa.gov/projects/tharsis/MOLA/SOFTWARE/
        ! uses the gravity field coefficients file 'mgm1025' available in
        ! the same ftp directory
        implicit none

        !        include "constants_mcd.inc"

        ! inputs:
        character (LEN=*) :: dset ! path to datafiles
        real lon ! East longitude (degrees)
        real lat ! North latitude (degrees)
        ! output:
        real rareoid ! distance (in m) of areoid to center of Mars

        ! COMMON: (shared with readcs.F and geoid.F)
        integer :: ndeg,ndeg2,nd2p3
        parameter (ndeg=90,ndeg2=2*ndeg,nd2p3=ndeg2+3)
        ! data structure of gravity field
        real (KIND=pm_reel) :: v0,omega,ae,gm
        real (KIND=pm_reel) :: clm(0:ndeg,0:ndeg),slm(0:ndeg,0:ndeg)
        common /gmm1/v0,omega,ae,gm,clm,slm
        integer :: lmin,lmax
        real (KIND=pm_reel) :: root(nd2p3)
        real (KIND=pm_reel) :: requator
        common /sqr/ lmin,lmax,root,requator

        real (KIND=pm_reel) :: dlon    ! double precision version of lon
        real (KIND=pm_reel) :: dlat    ! double precision version of lat
        real (KIND=pm_reel) :: dareoid ! double precision version of rareoid
        !	double precision pi,d2r
        !	parameter (pi=3.141592653589792D0, d2r=pi/180.d0)
        character (LEN=140) :: mgm     ! gravity field coefficients
        !	data mgm /'mgm1025'/
        integer :: llmax=50
        !      data llmax /50/
        ! flag for coefficients' initialization
        logical :: firstcall
        data firstcall /.true./

        if (firstcall) then
           ! initialize gravity coefficients
           lmax = llmax
           ! build datafile path+name
           mgm=dset//'mgm1025'
           call readcs(mgm) ! read coefficients from file

           if((lmax.le.0).or.(lmax.gt.ndeg)) then
              if (output_messages) then
                 write(0,*)'MOLAREOID Error: Gravity file in wrong format!'
                 write(0,*)' lmin,max=',lmin,lmax
              endif
              stop
           endif
           firstcall=.false.
        endif ! of if (firstcall)

        dlon=dble(lon)
        dlat=dble(lat)
        call geoid(dlon,dlat,dareoid)
        rareoid=real(dareoid)

      end subroutine molareoid


      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine readcs(mgm)

        implicit none

        !        include "constants_mcd.inc"

        ! input
        character (LEN=*) :: mgm ! file to read coefficients from

        character (LEN=64) :: title ! first line of
        character (LEN=6) :: gcoef
        ! data structure of gravity field
        integer :: ndeg,ndeg2,nd2p3
        parameter (ndeg=90,ndeg2=2*ndeg,nd2p3=ndeg2+3)
        real (KIND=pm_reel) :: c(0:ndeg,0:ndeg),s(0:ndeg,0:ndeg)
        real (KIND=pm_reel) ::  v0,omega,ae,gm !,cl
        real (KIND=pm_reel) ::  plm(ndeg)
        common /gmm1/ v0,omega,ae,gm,c,s
        integer :: lmin, lmax
        real (KIND=pm_reel) :: root(nd2p3)
        real (KIND=pm_reel) :: r
        common /sqr/ lmin,lmax, root,r
        !
        integer :: lcmax,mmax
        integer :: k,l,m
        real (KIND=pm_reel) :: coef
        real (KIND=pm_reel) :: x,xi,sum

        ae= 3396000.d0
        gm =42828.37d9
        omega=0.70882181d-4
        ! this value makes the equatorial mean radius equal to 3396 km.
        do k=1,nd2p3
           root(k)=sqrt(dble(k))
        enddo
        !  initialize
        do l=0, ndeg
           do m=0,l
              c(l,m)=0.
              s(l,m)=0.
           enddo
        enddo
        open(unit=11,file=mgm,status='old',err=999)
        read (11,'(a)') title
        read (11,1000) gcoef,lcmax,mmax,gm,ae
1	continue
	read(11,1000,end=99) gcoef,l,m,coef
1000    format(a6,8x,i3,i3,d24.14,d15.9)

        !	read(1,1000,end=99) l,m,clm,slm
        !1000   format(6x,i3,i3,1p,2d18.9)
        if(l.lt.0 .or. m.lt.0 .or. l.gt.ndeg .or. m.gt.l) goto 999
        if(gcoef.eq.'GCOEFC') c(l,m)=coef
        if(gcoef.eq.'GCOEFS') s(l,m)=coef
        !	c(l,m)=clm
        !	s(l,m)=slm
        goto 1
99	continue
        close(unit=11)
        r = 3396000.d0 !  MOLA potential surface
        xi = ae/r
        m=0
        x=0.
        sum=0.
        call lgndr(lmax,m,x,plm,root)
        do l=2, lmax
           sum = sum + c(l,m) *xi**l *plm(l-m+1)
        enddo
        sum = sum+1.
        ! centrifugal potential	
        v0=(gm*sum/r + 0.5*omega**2 * r**2)
        !	write(*,*)'v0=', v0
        return

999	continue
        if (output_messages) then
           write(0,*)'READCS Error: file not found or incorrect data',lmax
           write(0,*)'title: ',title
           write(0,*)'gcoef,lcmax,mmax,gm,ae:',gcoef,lcmax,mmax,gm,ae
        endif
      end subroutine readcs

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine geoid(dlon,dlat,rg)
        ! marsgeoid.f
        ! calculate radii of surface of constant gravity potential on rotating body
        ! given spherical harmonic potential of body at lat,lon,elevation
        !
        ! W = V +Phi = gm/r [1+ (R/r)**n [harmonics]] -1/2 omega**2 r**2

        implicit none

        real (KIND=pm_reel) :: dlon,dlat
        real (KIND=pm_reel) :: rg

        real (KIND=pm_reel) :: pi,d2r
        parameter (pi=3.141592653589792D0, d2r=pi/180.d0)
        ! data structure of gravity field
        integer :: ndeg,ndeg1,ndeg2,nd2p3
        parameter (ndeg=90,ndeg1=ndeg+1,ndeg2=2*ndeg,nd2p3=ndeg2+3)
        real (KIND=pm_reel) :: v0,omega,ae,gm
        real (KIND=pm_reel) :: c(0:ndeg,0:ndeg),s(0:ndeg,0:ndeg)
        common /gmm1/v0,omega,ae,gm,c,s
        integer :: lmin,lmax
        real (KIND=pm_reel) :: root(nd2p3)
        real (KIND=pm_reel) :: r
        common /sqr/ lmin,lmax,root,r
        real (KIND=pm_reel) :: plm(ndeg1)
        double precision tol
        data tol /0.125d0/  ! tolerence on computed value of rg

        integer :: i,m,l
        real (KIND=pm_reel) :: rlon,rlat
        real (KIND=pm_reel) :: x,cslt,xi,sum,cslm
        real (KIND=pm_reel) :: diff
        ! save r
        rg = r

        rlon=dlon*d2r
        rlat=dlat*d2r
        x = sin(rlat)
        cslt= cos(rlat)

        !	csln= cos(rlon)
        !	snln= sin(rlon)

 	do i=1,8 ! usually 3 iterations suffice

           xi = ae/r
           sum = 0.
           do m=0, lmax
              call lgndr(lmax,m,x,plm,root)
              do l=m, lmax
                 cslm =  c(l,m)*cos(m*rlon) + s(l,m)*sin(m*rlon)
                 sum = sum + cslm*xi**l *plm(l-m+1)
              enddo
           enddo
           sum=sum+1.
           ! centrifugal potential	
           rg=(gm*sum+0.5*(omega**2)*(r**3)*(cslt**2))/v0
           !          write(*,*) i,r,rg
           diff = r-rg
           r = rg
           if( abs(diff).lt. tol) goto 400
        enddo ! do i=1,8
400     continue
        !	write(*,500) dlon,dlat,r
        !500	format(2f9.2, f12.3)
      end subroutine geoid

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE LGNDR(LMAX,M,X,PLM,SQR)
        ! Return vector of plm's of degree from m to lmax, for order m
        implicit none
        integer :: LMAX
        integer :: M
        real (KIND=pm_reel) :: X
        real (KIND=pm_reel) :: PLM(*),SQR(*)
        integer :: IFACT,I,LL
        real (KIND=pm_reel) :: OM, PMM, PMMP1, PLL, SOMX2
        !  needs sqrt of integers from 1 to 2*L+3
        !  SIGN MODIF. OF NUM.REC. ALGORITHM, TO MATCH GEOPHYS. CONVENTION.
        !  AND NORMALIZATION TO MEAN SQUARE UNITY.

        PMMP1=0 !dummy initialization to get rid of compiler warnings

        OM = 1.
        PMM=SQR(2*M+1)
        IF(M.gt.0) then
           PMM=SQR(2)*PMM
           SOMX2=SQRT((1.-X)*(1.+X))
           IFACT=1
           do I=1,M
              OM=-OM
              PMM=-PMM*(SQR(IFACT)/SQR(IFACT+1))*SOMX2
              IFACT=IFACT+2
           enddo
        ENDIF
        PLM(1)=OM*PMM
        if(LMAX.gt.M) then
           PMMP1=X*SQR(2*M+3)*PMM
           PLM(2)=OM*PMMP1
        endif
        if (LMAX.gt.M+1) then
           DO LL=M+2,LMAX
              PLL=SQR(2*LL+1)*SQR(2*LL-1)/(SQR(LL+M)*SQR(LL-M)) &
                   * (X*PMMP1-PMM*SQR(LL+M-1)*SQR(LL-M-1)/(SQR(2*LL-1)*SQR(2*LL-3)))
              PLM(LL-M+1)=OM*PLL
              PMM=PMMP1
              PMMP1=PLL
           ENDDO
        endif
      END subroutine  LGNDR

    subroutine cps_atmemcd_42_close ()
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine                                                       *
!$Nom                                                                  *
!     cps_atmemcd_42_close                                             *
!                                                                      *
!$Resume                                                               *
!      routine de desallocation memoire                                *
!$Auteur                                                               *
!      Florence VIVARES (ATOS Origin)                                  *
!$Usage                                                                *
!     call cps_atmemcd_42_close()                                      *
!                                                                      *
!$<>                                                                   *
!***********************************************************************

! Variable pour la gestion d'erreur de deallocate
      integer :: ierralloc

      if (modele_init) then
!
! desallocation des variables globales :
!
         if (associated(var_2d))   deallocate(var_2d,   stat=ierralloc)
         if (associated(var_2d2))  deallocate(var_2d2,  stat=ierralloc)
         if (associated(var_3d))   deallocate(var_3d,   stat=ierralloc)
         if (associated(var_3d2))  deallocate(var_3d2,  stat=ierralloc)
!
         if (associated(varsd2d))  deallocate(varsd2d,  stat=ierralloc)
         if (associated(varsd2d2)) deallocate(varsd2d2, stat=ierralloc)
         if (associated(varsd3d))  deallocate(varsd3d,  stat=ierralloc)
         if (associated(varsd3d2)) deallocate(varsd3d2, stat=ierralloc)
!
         if (associated(varrms2d)) deallocate(varrms2d, stat=ierralloc)
         if (associated(varrms2d2))deallocate(varrms2d2,stat=ierralloc)
         if (associated(varrms3d)) deallocate(varrms3d, stat=ierralloc)
         if (associated(varrms3d2))deallocate(varrms3d2,stat=ierralloc)
!
         if (associated(tabpcsmth))deallocate(tabpcsmth,stat=ierralloc)
         if (associated(tabpcvar)) deallocate(tabpcvar, stat=ierralloc)

         if (associated(tabeops))  deallocate(tabeops,  stat=ierralloc)
         if (associated(tabeot))   deallocate(tabeot,   stat=ierralloc)
         if (associated(tabeou))   deallocate(tabeou,   stat=ierralloc)
         if (associated(tabeov))   deallocate(tabeov,   stat=ierralloc)
         if (associated(tabeorho)) deallocate(tabeorho, stat=ierralloc)

         modele_init = .false.
      endif

    end subroutine cps_atmemcd_42_close

    subroutine julian(month,day,year,hour,minute,second,ierr,date)

      implicit none
      !
      !     Given Earth date and time compute and local time on Mars
      !     Updated version by B. Dolla and F. Forget, 2005
      !     Inputs
      !
      integer month   
      integer day
      integer year
      integer hour       !All Earth GMT values
      integer minute
      integer second
      !
      !     Output
      !
      integer ierr       !0 if ok >0 if there is a problem
      real*8  date       !Julian date
      !
      !     Local
      !
      integer nday
      integer daynumber(12)   !days for months of the year
      data    daynumber/0,31,59,90,120,151,181,212,243,273,304,334/
      integer jul
      integer j
      !
      !     Check ranges
      !
      ierr=0
      if ((month.lt.1).or.(month.gt.12)) then
         ierr=1
         return
      endif
      if ((day.lt.1).or.(day.gt.31)) then
         ierr=2
         return
      endif
      if (year.lt.1) then
         ierr=3
         return
      endif
      if ((hour.lt.0).or.(hour.gt.23)) then
         ierr=4
         return
      endif
      if ((minute.lt.0).or.(minute.gt.59)) then
         ierr=5
         return
      endif
      if ((second.lt.0).or.(second.gt.60)) then
         ierr=6
         return
      endif
      !
      !     Calculate Julian date
      !
      nday=daynumber(month)+day-1
      !
      !     Correct for leap year
      !     We use the followings conventions
      !     GREGORIAN CALENDAR: a year is bissextil if it is a multiple
      !     of 4 but not of 100 or if it is a multiple of 400.
      !     JULIAN CALENDAR: a year is bissextil if it is a multiple of 4
      !
      !     The JULIAN calendar ends on the 4th october 1582
      !     The GREGORIAN calendar begins on the 15th october 1582
      !     Hence there are 10 days missing... e.g. the 10th october 1582 does not exist!!
      !

      jul=0
      IF (year.LT.1582) jul=1
      IF ((year.EQ.1582).AND.(month.LT.10)) jul=1
      IF ((year.EQ.1582).AND.(month.EQ.10).AND.(day.LT.15)) jul=1

      IF (jul.EQ.0) THEN
         IF ((mod(year,4).EQ.0).AND.(mod(year,100).NE.0) &
         &       .AND.(month.GT.2)) nday=nday+1
         IF ((mod(year,400).EQ.0) &
         &    .AND.(month.GT.2)) nday=nday+1
      ENDIF
      IF (jul.EQ.1) THEN
         IF ((mod(year,4).EQ.0).AND.(month.GT.2)) nday=nday+1
         nday=nday+10
      ENDIF
      !
      !     We use 1968 as a year of reference, the julian date being 2.4398565d6
      !
      IF (year.GT.1968) THEN
         DO j=1968,year-1,1
            nday=nday+365
            IF ((mod(j,4).EQ.0).AND.(mod(j,100).NE.0)) nday=nday+1
            IF (mod(j,400).EQ.0) nday=nday+1
         ENDDO
      ENDIF

      IF (year.LT.1968) THEN
         jul=1
         DO j=year,1967,1
            IF (j.GT.1581) jul=0
            IF (jul.EQ.0) THEN
               nday=nday-365
               IF ((mod(j,4).EQ.0).AND.(mod(j,100).NE.0)) nday=nday-1
               IF (mod(j,400).EQ.0) nday=nday-1
            ENDIF
            IF (jul.EQ.1) THEN
               nday=nday-365
               IF (mod(j,4).EQ.0) nday=nday-1
            ENDIF
         ENDDO
      ENDIF
      !
      !     Compute Julian date
      !
      date=2.4398565d6+nday
      date=date+hour/24.0d0+minute/1.440d3+second/8.6400d4
      !
      return
    end subroutine julian

    end module cps_modele_emcd42
