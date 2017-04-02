module cps_modele_emcd431

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_modele_emcd431
!
!$Resume
!  Modèles d'Atmosphère martien EMCD 4.3.1
!
!$Description
! Ce module contient la version 4.3.1 du modèle EMCD (European Martian
! Climate Database) :
! - routine cps_atmemcd_431
! 
!$Auteur
!  Cédric MARTEL (ATOS Origin)
!
!$Version
!  $Id: cps_modele_emcd431.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: cps_modele_emcd431.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.3  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.2  2010/04/30 14:12:37  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.1.2.3  2010/03/10 14:11:14  jlrobin
!  V2.9::DM-ID:1360:02/03/2010:Ajout du modele EMCD 4.3.1
!
!  Revision 1.1.2.2  2010/03/05 15:19:09  jlrobin
!  V2.9::DM-ID:1360:02/03/2010:Ajout du modele EMCD 4.3.1
!
!  Revision 1.1.2.1  2010/03/03 13:16:01  cmartel
!  V2.9::DM-ID:1360:02/03/2010:Ajout du modele EMCD 4.3.1
!
!
!$FinHistorique
!
!$Usage
!  use cps_modele_emcd431
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

  ! Accès à la base pour chemin par defaut d'accès aux données de l'EMCD431
  use cps_utilisateur 
  use netcdf_f90

!***********************************************************************
!  déclaration des interfaces disponibles de ce module                 *
!***********************************************************************


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_modele_emcd431.F90 379 2013-02-22 16:59:49Z ffsm $'

  interface cps_atmemcd_431

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmemcd_431
!
!$Resume
!  Méthode d'extraction et de calcul de variables météorologiques 
!  par la base de données "Mars Climate Database"
!
!$Description
!  Méthode d'extraction et de calcul de variables météorologiques 
!  par la base de données "Mars Climate Database"
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmemcd_431(zkey,xz,xlon,xlat,hireskey, &
!.               datekey,xdate,localtime,dset,scena, &
!.               perturkey,seedin,gwlength,extvarkey, &
!.               pres,dens,temp,zonwind,merwind, &
!.               meanvar,extvar,seedout,ier)
!.    integer :: zkey
!.    real(pm_reel) :: xz
!.    real(pm_reel) :: xlon
!.    real(pm_reel) :: xlat
!.    integer :: hireskey
!.    integer :: datekey
!.    real(pm_reel) :: xdate
!.    real(pm_reel) :: localtime
!.    integer :: perturkey
!.    real(pm_reel) :: seedin
!.    real(pm_reel) :: gwlength
!.    integer :: scena
!.    integer :: extvarkey
!.    character*(*) :: dset
!.    real(pm_reel) :: meanvar(nmeanvar)
!.    real(pm_reel) :: extvar(nextvar)
!.    real(pm_reel) :: seedout
!.    real(pm_reel) :: pres
!.    real(pm_reel) :: dens
!.    real(pm_reel) :: temp
!.    real(pm_reel) :: zonwind
!.    real(pm_reel) :: merwind
!.    integer :: ier
!
!$Arguments
!>E     zkey       :<integer> type de coordonnées verticales pour xz
!.   1 = rayon depuis le centre de la planète (m)
!.   2 = hauteur au dessus de l'aréoïde (m) 
!.   3 = hauteur au dessus de la surface (m)
!.   4 = niveau de pression (Pa)
!.   5 = altitude au dessus du rayon moyen de Mars (=3396000m) (m)
!>E     xz         :<pm_reel> coordonnées verticales (dépend de zkey)
!>E     xlon       :<pm_reel> longitude Est (degrés)
!>E     xlat       :<pm_reel> latitude Nord (degrés)
!>E     hireskey   :<integer> flag permettant l'utilisation 
!                             de la topographie en haute résolution.
!.               0 = utiliser la résolution GCM (modèle de circulation général)
!.               1 = basculement en topographie haute résolution 
!>E     datekey    :<integer> type de date en entrée
!.               0 = "Temps terrestre" : xdate est donnée en jours juliens fractionnaires
!                    et alors localtime est fixée à zéro
!.               1 = "Date martienne" : xdate est donnée en Ls
!>E     xdate      :<pm_reel> date donnée en jour juliens ou en degrés et en Ls
!                             (dépend de la valeur de datekey)
!>E     localtime  :<pm_reel> heure locale (en heures martiennes) à la longitude xlon
!>E     dset       :<LEN=*>   chemin d'accès aux données ou par défaut utilisation du chemin 
!                             spécifié par le champs EMCD_43 du fichier d'atmosphere 
!                             de la base COMPAS.
!>E     scena      :<integer> scénario de poussière en suspension
!.   1 = MY24 (Mars Year 24) avec conditions solaires UV minimums 
!.   2 = MY24 avec conditions UV solaires moyennes
!.   3 = MY24 avec conditions UV solaires maximums
!.   4 = tempête de poussière (tau = 4) et conditions solaires UV minimums
!.   5 = tempête de poussière (tau = 4) et conditions solaires UV moyennes
!.   6 = tempête de poussière (tau = 4) et conditions solaires UV maximums
!.   7 = scénario chaud : poussières et conditions solaires UV maximums
!.   8 = scénario froid : peu de poussière et conditions solaires UV minimums
!>E     perturkey  :<integer> type de perturbation à appliquer
!.               1 = aucune perturbation
!.               2 = perturbations grande échelle (utilisation de seedin pour la génération
!                    de perturbation par EOF (fonctions orthogonales empiriques)
!.               3 = perturbations de petite échelle (utilisation de seedin pour la génération
!                    de perturbations de type GW (ondes de gravités verticales)
!.               4 = combinaison des deux perturbations de grande et petite échelles
!.               5 = ajout de seedin fois l'écart type (alors seedin doit être compris entre -4 et 4)      
!>E     seedin     :<pm_reel> 'graine' utilisée pour le générateur de valeurs aléatoires 
!                             (uniquement pour les valeurs 2, 3 et 4 de perturkey) 
!                             OU, pour pertukey = 5, coefficient à utiliser avec l'écart type
!                             avant son ajout à la valeur moyenne
!>E     gwlength   :<pm_reel> longueur d'onde pour les ondes de gravités verticales (m),
!                             une valeur de zéro fixe la longueur d'onde à 16000m par défaut
!>E     extvarkey  :<integer> flag d'ajout des variables supplémentaires en sortie
!.               0 = seules les valeurs de pression, densité, température et composantes du vent sont
!        données en sorties
!.               1 = ajout aux variables précédente des autres valeurs (stockées dans extvar)
!>S     pres       :<pm_reel> pression atmosphérique (Pa)
!>S     dens       :<pm_reel> densité atmosphérique (kg/m^3)
!>S     temp       :<pm_reel> température atmosphérique (K)
!>S     zonwind    :<pm_reel> composante zonale du vent (Est-Ouest)
!>S     merwind    :<pm_reel> composante méridionale du vent (Nord-Sud)
!>S     meanvar    :<pm_reel,DIM=(nmeanvar)> valeurs moyennes sans perturbations
!.              meanvar(1) = pression moyenne
!.              meanvar(2) = densité moyenne
!.              meanvar(3) = température moyenne
!.              meanvar(4) = composante zonale moyenne du vent
!.              meanvar(5) = composante méridionale moyenne 
!>S     extvar     :<pm_reel,DIM=(nextvar)> valeurs calculées supplémentaires
!.              extvar(1)  = distance radiale depuis le centre de la planète (m)
!.              extvar(2)  = altitude au dessus de l'aréoïde (géoïde martien) (m)
!.              extvar(3)  = altitude au dessus de la surface (m)
!.              extvar(4)  = hauteur orographique (m) (altitude de la surface au dessus de l'aréoïde)
!.              extvar(5)  = Ls, longitude solaire de Mars (deg)
!.              extvar(6)  = LST heure locale solaire réelle (hrs)
!.              extvar(7)  = temps solaire universel (LST à la longitude 0) (hrs)
!.              extvar(8)  = capacity caloriphique de l'air Cp (J kg-1 K-1)
!.              extvar(9)  = valeur de gamma, le ratio Cp/Cv de chaleur spécifique
!.              extvar(10) = variations RMS (moyenne quadratique) de la densité au jour le jour (kg/m^3)
!.              extvar(11) = valeur non utilisée et fixée à zéro
!.              extvar(12) = valeur non utilisée et fixée à zéro
!.              extvar(13) = hauteur d'échelle H(p) (m)
!.              extvar(14) = orographie GCM (m)
!.              extvar(15) = température de surface (K)
!.              extvar(16) = température de surface maximum pour ce jour (K)
!.              extvar(17) = température de surface minimum pour ce jour (K)
!.              extvar(18) = variations RMS au jour le jour de la température de surface (K) 
!.              extvar(19) = pression en surface (et haute résolution si hireskey =1)
!.              extvar(20) = pression de surface GCM (Pa)
!.              extvar(21) = variation RMS au jour le jour de la pression atmosphérique (Pa)
!.              extvar(22) = variation RMS au jour le jour de la pression de surface (Pa)
!.              extvar(23) = variation RMS au jour le jour de température (K)
!.              extvar(24) = variation RMS au jour le jour du vent zonal (m/s)
!.              extvar(25) = variation RMS au jour le jour de du vent méridional (m/s)
!.              extvar(26) = composante verticale du vent (m/s) (>0 quand le vent est descendant)
!.              extvar(27) = variation RMS au jour le jour de la composante verticale du vent (m/s)
!.              extvar(28) = perturbation de petite échelle (onde de gravités) (kg/m^3)
!.              extvar(29) = valeur de q2 : énergie cinétique turbulente (m2/s2)
!.              extvar(30) = valeur non utilisée 
!.              extvar(31) = flux IR thermique vers la surface (W/m2)
!.              extvar(31) = flux solaire vers la surface (W/m2)
!.              extvar(33) = flux IR thermique vers l'espace (W/m2)
!.              extvar(34) = flux solaire réfléchi vers l'espace (W/m2)
!.              extvar(35) = couche de glace de CO2 en surface (kg/m2)
!.              extvar(36) = valeur DOD : profondeur optique visible de la colonne de poussière 
!.              extvar(37) = ratio du mélange de masse de la poussière (kg/kg)
!.              extvar(38) = variation RMS de la valeur de DOD au jour le jour 
!.              extvar(39) = écart type totale de la valeur de DOD sur toute la saison
!.              extvar(40) = colonne de vapeur d'eau (kg/m2)
!.              extvar(41) = ratio de mélange de la valeur d'eau (mol/mol)
!.              extvar(42) = colonne de glace d'eau (kg/m2)
!.              extvar(43) = ratio de glace d'eau (mol/mol)
!.              extvar(44) = ratio d'ozone 03 (mol/mol)
!.              extvar(45) = ratio de carbone CO2 (mol/mol)
!.              extvar(46) = ratio d'oxygene O (mol/mol)
!.              extvar(47) = ratio d'azote N2 (mol/mol)
!.              extvar(48) = ratio de monoxyde de carbone CO (mol/mol)
!.              extvar(49) = valeur de R : constante de gaz moléculaire (J K-1 kg-1)
!.              extvar(50) = estimation de la viscosité de l'air (N s m-2)
!.              extvar(51:100) = valeurs non utilisées et fixées à zéro
!>S     seedout    :<pm_reel> valeur de l'indexe dans le générateur de nombre (peut être utilisé
!                             pour générer un nouvel ensemble de perturbations)
!>S     ier        :<integer> code de retour
!.              0 = retour OK
!.              1 = mauvais code pour les coordonnes verticales (zkey)
!.              2 = mauvais code pour le scénario de poussière (scena)
!.              3 = mauvais code pour le flag de perturbations (perturkey)
!.              4 = mauvais code pour le flag haute résolution (hireskey)
!.              5 = mauvais code pour le flag de la date (datekey)
!.              6 = mauvais code pour le flag d'extraction de valeurs supplémentaires (extvarkey)
!.              7 = mauvais code pour la latitude (xlat)
!.              8 = valeur inadéquate pour la longueur d'onde des ondes de gravitation (gwlength)
!.              9 = mauvaise valeur pour la longitude solaire Ls (xdate, si datekey = 1)
!.             10 = la date julienne n'est pas incluse dans l'intervalle [jdate_min:jdate_max]
!.             11 = mauvaise valeur pour l'heure locale (elle doit être incluse dans [0:24])
!.             12 = valeur de localtime (~= 0) incompatible avec le flag datekey(= 0)
!.             13 = valeur inadéquate pour 'seedin' (dans le cas perturkey = 5)
!.             14 = aucun scénario de poussière n'est disponible pour cette date
!.             15 = n'a pas réussi à ouvrir un des fichiers de la base de données 
!                   (vérifier le chemin d'accès)
!.             16 = échec de chargement d'un des fichiers de la base de données
!.             17 = une des attitudes (donnée ou calculée) est souterraine
!.             18 = l'ajout de la perturbation en entrée (perturkey = 5) a donné lieu à une valeur 
!                   de densité impossible
!.             19 = l'ajout de la perturbation en entrée (perturkey = 5) a donné lieu à une valeur 
!                   de température impossible
!.             20 = l'ajout de la perturbation en entrée (perturkey = 5) a donné lieu à une valeur 
!                   de pression impossible
!.             21 = le fichier de la base n'a pu être ouvert, toutefois il existe un fichier
!                   avec l'extension '.gz' qui devrait être décompressé.
!
!$Procedures
!#V
!- call_mcd
!#
!
!$Remarques
!  Ce cartouche est une traduction de celui initialement fourni pour la routine call_mcd
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure call_mcd
  end interface


!***********************************************************************
!  déclaration des sous programmes et fonctions de ce module           *
!***********************************************************************
  private call_mcd, eofpb, profi_eof 
  private getsi, grid4, grwpb
  private loadvar, loadeof
  private mars_ltime, mars_ptime, mcd_time
  private opend, orbit, profi, season
  private season2, sol2ls, ls2sol, var2d
  private interpol, extrapol3d, get_2d, get_3d
  private getsd_2d, getsd_3d, var3d, max2d
  private min2d, ran1, dustmix, air_properties
  private build_sigma_hr, pres0, read_vl, calc_factcor
  private nearsurfacenoise, gasdev

  ! Fonctions et subroutines issues du fichiers height.F fournit avec le modèle
  private heights, mola, molareoid, readcs, geoid

  ! La routine de désallocation de la mémoire est cps_atmemcd_431_close
  ! Elle doit etre appelee a la fin de l'ensemble des utilisation du modèle
 
!***********************************************************************
! include "constants_mcd.inc"
!***********************************************************************
!     include file defining constants and arrays used by the 
!     Martian Climate Database

! CONSTANTS
!     information and error messages will be written to output if
!     output_message is .true. Switch to .false. for a "silent" mode
      logical, parameter :: output_messages = .true.
!     default unit to which messages will be sent:
!     typically out=6 implies screen, other (positive) values (except 0
!     which is sometimes preconected to standard error, and 5, which is
!     standard input) should be OK (messages will be sent to file 'fort.out',
!     unless an open(out, file="myfilename") call was issued prior to the
!     very first call to call_mcd
      integer, private, parameter :: out = 6
!     dimension of mean and std. dev.values  in longitude, latitude, 
!     sigma levels and database universal time
      integer, private :: dimlon,dimlat,dimlevs,dimuti
      parameter(dimlon=64,dimlat=49,dimlevs=50,dimuti=12)
!     dimensions of EOF values in longitude, latitude, numbers of EOFs 
      integer, private :: dimloneo,dimlateo,dimnevecs,dimeoday
      parameter(dimloneo=16,dimlateo=12,dimnevecs=200,dimeoday=669)
!     range of latitude, longitude  and time for mean 
!     and std. dev. values
      real(pm_reel), private :: lonmin,lonmax,latmin,latmax
      parameter(lonmin=-180_pm_reel,lonmax=174.375_pm_reel)
      parameter(latmin=-90_pm_reel,latmax=90_pm_reel)
!     range of latitude, longitude, number of EOFs and time for EOFs values 
      real(pm_reel), private :: lonmineo,lonmaxeo,latmineo,latmaxeo,nevecsmin,nevecsmax
      real(pm_reel), private :: daymineo,daymaxeo
      parameter(lonmineo=-180.0_pm_reel,lonmaxeo=157.5_pm_reel)
      parameter(latmineo=-82.5_pm_reel,latmaxeo=82.5_pm_reel)
      parameter(nevecsmin=1.0_pm_reel,nevecsmax=72.0_pm_reel)
      parameter(daymineo=1.0_pm_reel,daymaxeo=669.0_pm_reel)
!     step of grid in longitude for mean and std. dev. values
      real(pm_reel), private, parameter :: deltalon = 5.625_pm_reel ! 360./64
!     step of grid in latitude for mean and std. dev. values
      real(pm_reel), private :: deltalat
      parameter(deltalat=3.75_pm_reel) ! 180./48
!     step of grid in longitude and latitude for EOFs values
      real(pm_reel), private :: deltalateo,deltaloneo
      parameter(deltalateo=15.0_pm_reel)
      parameter(deltaloneo=22.5_pm_reel)
!     number of 2d, low and up variables
      integer, private :: nbvar2d,nbvarlow,nbvarup
      parameter(nbvar2d=10,nbvarlow=9,nbvarup=9)
      integer, private :: nbsd2d,nbsdlow,nbsdup
      parameter(nbsd2d=3, nbsdlow=5,nbsdup=5)
      integer, private :: nbvar3d,nbcom
      parameter(nbvar3d=13,nbcom=5)
      integer, private :: nbsd3d,nbcomsd
      parameter(nbsd3d=5,nbcomsd=5)      
! Layer below which the mean is done between 3 runs.
      integer, private :: low, up
      parameter(low=30,up=20)
! Aerodynamic roughness length (for Boundary layer behavior of horizontal
!  velocities between ground and 1st GCM level).
      real(pm_reel), private :: z_0
      parameter (z_0=0.01_pm_reel) ! z_0 = 1 cm

!--------------------------------------------
! Note : Ces tableaux sont conservés en l'état 
!        car ils sont toujours nécessaires
!     GCM orographic data array
      real(pm_reel),save,private :: taborog(dimlon,dimlat,1)
!     GCM areoid data array
      real(pm_reel),save,private :: tabareo(dimlon,dimlat,1)
!     GCM orographic variance array
      real(pm_reel),save,private :: tabsubstd(dimlon,dimlat,1)

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
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: var_2d => NULL() ! previous season
! (dimlon,dimlat,dimuti,nbvar2d)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: var_2d2 => NULL() ! next season

! -------------------------------------------
! 3D fields 
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)	
      real(pm_reel),save,private, dimension(:,:,:,:,:),pointer :: var_3d => NULL()  ! previous season
! (dimlon,dimlat,dimlevs,dimuti,nbvar3d)
      real(pm_reel),save,private, dimension(:,:,:,:,:),pointer :: var_3d2 => NULL() ! next season
	
! -------------------------------------------
! RMS value arrays : surface pressure, surface temperature
! temperature, density, zonal and meriodional wind components
! (dimlon,dimlat,nbsd2d)
      real(pm_reel),save,private, dimension(:,:,:),pointer :: varrms2d => NULL()
! (dimlon,dimlat,nbsd2d)
      real(pm_reel),save,private, dimension(:,:,:),pointer :: varrms2d2 => NULL()
! (dimlon,dimlat,dimlevs,nbsd3d)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: varrms3d => NULL() 
! (dimlon,dimlat,dimlevs,nbsd3d)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: varrms3d2 => NULL() 

! -------------------------------------------
! Altitude-wise RMS for temperature, density, winds and pressure
! (dimlon,dimlat,dimlevs,nbsd3d+1)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: vararms3d => NULL()
! (dimlon,dimlat,dimlevs,nbsd3d+1)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: vararms3d2 => NULL() 

! -------------------------------------------
!     arrays related to EOFs
!     normalisation factors for zonal wind, meriodional wind, temperature
!     pressure and density
      real(pm_reel) ::  tabeonormu(dimlateo)
      real(pm_reel) ::  tabeonormv(dimlateo)
      real(pm_reel) ::  tabeonormt(dimlateo)
      real(pm_reel) ::  tabeonormp(dimlateo)
! -------------------------------------------
! (dimlateo,dimeoday,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:),pointer :: tabpc => NULL()
!     smoothed PCs 
! (dimlateo,dimeoday,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:),pointer :: tabpcsmth => NULL()
!     EOFS for surface pressure, temperature, density, zonal wind
!     and meriodional wind
! (dimloneo,dimlateo,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:),pointer :: tabeops => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: tabeot => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: tabeou => NULL()
! (dimloneo,dimlateo,dimlevs,dimnevecs)
      real(pm_reel),save,private, dimension(:,:,:,:),pointer :: tabeov => NULL()       
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
!     4.3 03/09 additional near-surface noise/perturbation scheme
!     4.3 04/08 new improved large scale (EOF) perturbations model
!     4.2 02/08 improved GW perturbations behaviour near surface (grwpb.F)
!     4.2 02/08 fixed 2 minor bugs (random seed initialization & getsi.F)
!     4.2 02/08 implemented EOF perturbations interpolations
!     4.2 02/08 added zkey=5 option 
!     4.2 06/07 various minor bug fixes
!     4.2 03/07 added interpolation for temp. and winds below first layer
!     4.2 03/07 added altitude-wise RMS
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
!     v4.3 Implemented improved large scale perturbations model -- EM
!     v4.2 Changed argument list; implemented high resolution and altitude-wise
!          RMS, and many other small changes  -- EM
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
!   zkey  : <integer>   type of vertical coordinate xz
!                       1 = radius from centre of planet (m)
!                       2 = height above areoid (m) (MOLA zero datum)
!                       3 = height above surface (m)
!                       4 = pressure level (Pa)
!                       5 = altitude above mean Mars Radius(=3396000m) (m)
!   xz    : <real> vertical coordinate (m or Pa, depends on zkey)
!   xlon  : <real> longitude (degrees east)
!   xlat  : <real> latitude (degrees north)
!   hireskey: <integer> flag to switch to high resolution topography
!                    0 = use GCM resolution
!                    1 = switch to high resolution topography
!   datekey: <integer>    type of input date
!                    0 = "Earth time": xdate is given in Julian days
!                                      (localtime must be set to zero)
!                    1 = "Mars date": xdate is the value of Ls
!   xdate : <real(pm_reel)> date 
!                      IF datekey = 0 : Earth julian date
!                      IF datekey = 1 : Value of Ls (in degrees)
!   localtime : <real> local time (in martian hours) at longitude xlon
!              ONLY USED IF datekey=1 (must be set to 0 if datekey=0)
!   dset  : <character*(*)>    data set
!                              One or more blanks to get the default,
!                              or full directory path of MCD data required
!                              including trailing slash (e.g. '/dir/path/'),
!                              or a link to a directory (e.g. 'MCD_DATA/').
!                              Default is link MCD_DATA in working directory.
!   scena : <integer>          scenario
!                             1 = MY24 min solar
!                             2 = MY24 ave solar
!                             3 = MY24 max solar
!                             4 = dust storm tau=4 min solar           
!                             5 = dust storm tau=4 ave solar           
!                             6 = dust storm tau=4 max solar           
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
!   seedin: <real> 
!           IF perturkey=2,3,4: seed for random number generation
!                               changes in seedin between subsequent calls
!                               trigger reseeding and regeneration of
!                               perturbations
!           IF perturkey=5: coefficient by which standard deviations should
!                           be multiplied before being added to mean values
!   gwlength : <real>  for small scale (ie: gravity wave) perturbations;
!                   gwlength= wavelength of gravity wave perturbation (m)
!                             set to zero to get default (16000m)
!   extvarkey  : <integer> output type
!                   0 = pressure, density, temperature, wind
!                   1 = compute also extra variables written in extvar
!
! Arguments (outputs):
! ===================
!   pres    : <real> atmospheric pressure (Pa)
!   dens    : <real> atmospheric density (kg/m^3)
!   temp    : <real> atmospheric temperature (K)
!   zonwind : <real> zonal wind component (East-West)
!   merwind : <real> meridional wind component (North-South)
!   meanvar : <real> mean unperturbed values (array of dimension nmeanvar=5)
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
!               extvar(8) = Air heat capacity Cp (J kg-1 K-1)
!               extvar(9) = gamma=Cp/Cv Ratio of specific heats
!               extvar(10)= density RMS day to day variations (kg/m^3)
!               extvar(11)= not used (set to zero)
!               extvar(12)= not used (set to zero)
!               extvar(13)= scale height H(p) (m)
!               extvar(14)= GCM orography (m)
!               extvar(15)= surface temperature (K)
!               extvar(16)= daily maximum mean surface temperature (K)
!               extvar(17)= daily minimum mean surface temperature (K)
!               extvar(18)= surf. temperature RMS day to day variations (K) 
!               extvar(19)= surface pressure (high resolution if hireskey=1)
!               extvar(20)= GCM surface pressure (Pa)
!               extvar(21)= atmospheric pressure RMS day to day variations (Pa)
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
!   seedout : <real> current value of the seed of the random number generator
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
!    21 = Could not open a database file, but did find corresponding file.gz
!         (in dset) which should be de-compressed (e.g. gunzip file.gz).
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

! -------------------------------------------
!     Additional Option (not in the input) / Obsolete as of version 4.2
!     itimint, flag for seasonal interpolation
!     itimint 0: no seasonal interpolation
!     itimint 1: seasonal interpolation
      integer, parameter ::    itimint=1
! -------------------------------------------
      integer, parameter :: nextvar=100  ! size of extra variable array extvar()
      integer, parameter :: nmeanvar=5 ! size of meanvar() array
      
!     Entrées
!     ******      
      integer,intent(in) :: zkey ! flag for vertical coordinate type
      real(pm_reel),intent(in) ::          xz   ! vertical coordinate  
      real(pm_reel),intent(in) ::          xlon ! east longitude (degrees) 
      real(pm_reel),intent(in) ::          xlat ! latitude (degrees)
      integer,intent(in) ::       hireskey  ! flag: high resolution if =1
      integer,intent(in) ::       datekey   ! flag: 0=earth date 1= Mars date
      real(pm_reel),intent(in) :: xdate     ! earth julian date (or Ls)
      real(pm_reel),intent(in) ::          localtime ! true solar time at longitude lon
! Note: localtime can only be imposed if datekey=1 and should be 0 otherwise
      integer,intent(in) ::       perturkey ! flag: perturbation type
      real(pm_reel),intent(in) ::          seedin    ! seed for random number generation
! seedin is also used to trigger a reinitialization of perturbation
! (triggered if value of seedin changes between subsequent calls to CALL_EMCD)
      real(pm_reel),intent(in) ::          gwlength  ! gravity wave perturbation (vertical wavelength of)
      integer,intent(in) ::       scena     ! dust scenario
      integer,intent(in) ::       extvarkey ! flag: compute extra variables if =1
      character*(*),intent(in) :: dset      ! path to datafiles

!     Sorties
!     *******      
      real(pm_reel),intent(out) :: meanvar(nmeanvar) ! array for mean values
      real(pm_reel),intent(out) :: extvar(nextvar)   ! array for extra outputs
      real(pm_reel),intent(out) :: seedout ! current value of 'seed' (used by ran1)
      real(pm_reel),intent(out) :: pres    ! atmospheric pressure
      real(pm_reel),intent(out) :: dens    ! atmospheric density
      real(pm_reel),intent(out) :: temp    ! atmospheric temperature
      real(pm_reel),intent(out) :: zonwind ! zonal (eastward) wind
      real(pm_reel),intent(out) :: merwind ! meridional (northward) wind
      integer,intent(out) :: ier  ! status (error) code

!     Variables locales
!     *************** 
!     NETCDF file IDs :
!     orographic data
      integer ::        unet
!     mean fields      
      integer ::        unetm
!     std dev and rms fields
      integer ::        unetsd
!     mean up fields (thermosphere)
      integer ::        unetm_up
!     std dev up and rms fields (thermosphere)
      integer ::        unetsd_up
!     eof fields
      integer ::        ueof
!     season2 for seasonal interpolation- mean fields   
      integer ::        unetm2
!     season2 for seasonal interpolation- std dev fields
      integer ::        unetsd2
!     season2 for seasonal interpolation- mean fields - up 
      integer ::        unetm2_up
!     season2 for seasonal interpolation- std dev fields - up
      integer ::        unetsd2_up
      save              unet,unetm,unetsd,ueof,unetm2,unetsd2
      save              unetm_up,unetsd_up,unetm2_up,unetsd2_up
!     dust scenario
      integer ::        dust
!     seed (and also current index) for random number generator
      integer ::        seed
!     save seed, etc to ensure code works with standard F77
      save              seed
!      real(pm_reel) ::              R        ! not used any more, see Rnew
!      parameter        (R=191.2)
      real(pm_reel) ::           Rnew
      real(pm_reel) ::           R_gcm(dimlevs)    ! R at GCM levels
      real(pm_reel) ::           rho_gcm(dimlevs)  ! rho at GCM levels
      real(pm_reel) ::           temp_gcm(dimlevs) ! temperature at GCM levels
!     number of standard deviations to add
      real(pm_reel) ::           nbsig
!   Julian dates that should bracket "xdate" (if given as a Juilan date)
      real(pm_reel) ::  jdate_min
      parameter     (jdate_min=2378496.5_pm_reel) ! 01-01-1800 00:00
      real(pm_reel) ::  jdate_max
      parameter     (jdate_max=2524593.5_pm_reel) ! 01-01-2200 00:00
      integer ::    i,l    ! for loops
      integer ::    ierr   ! to store return codes from called routines
      integer ::    levlow ! database level, lower bound (wrt sought alt.)
      integer ::    levhi  ! database level, upper bound (wrt sought alt.)
      real(pm_reel) ::       areoid     ! distance to center of Mars of reference areoid
      real(pm_reel) ::       areoid_gcm ! GCM areoid (m)
      real(pm_reel) ::       areoid_hr  ! High resolution areoid (m)
      real(pm_reel) ::       height    ! height above areoid (m) 
      real(pm_reel) ::       oroheight ! orographic height (m)
      real(pm_reel) ::       oro_hr    ! MOLA orographic height (m)
      real(pm_reel) ::       oro_gcm   ! GCM orographic height (m)
      real(pm_reel) ::       absheight ! height above surface (m)
      real(pm_reel) ::       ls        ! Solar longitude (deg.)
      real(pm_reel) ::       marsau    ! Sun-Mars distance (in AU)
      real(pm_reel) ::       modelday  ! GCM day [0:668.6]
      real(pm_reel) ::       loctime   ! true solar time at longitude lon
      real(pm_reel) ::       utime     ! universal time (0. to 24. hrs)
                              ! =local time at lon=0
      real(pm_reel) ::       lon      ! longitude east [-180  180]
      real(pm_reel) ::       lat      ! latitude (degrees)
      real(pm_reel) ::       ps       ! surface pressure
      real(pm_reel) ::       ps_gcm   ! surface pressure (as read from MCD)
      real(pm_reel) ::       ps_hr    ! surface pressure (high res, using MOLA topo.)
      real(pm_reel) ::       ps_psgcm ! high res to GCM surface pressure ratio
      real(pm_reel) ::       p_pgcm(dimlevs) ! high res over GCM pressure ratios
!     variables  (trailing l indicates lower level variable from database)
      real(pm_reel) ::       t,tl     ! atmospheric temperature
      real(pm_reel) ::       p,pl     ! atmospheric pressure
      real(pm_reel) ::       rho,rhol ! density
      real(pm_reel) ::       u,ul,v,vl,w_l ! zonal, meridional and vertical winds
      real(pm_reel) ::       col_h2ovapor,col_h2oice
      real(pm_reel) ::       q2,vmr_h2oice,vmr_h2o,vmr_o,vmr_co2,vmr_co
      real(pm_reel) ::       vmr_n2,vmr_o3
      real(pm_reel) ::       pertm,pertr,pertrho,pertrhogw
      real(pm_reel) ::       pertps
      real(pm_reel) ::       pertu, pertv, pertt
      real(pm_reel) ::       levweight(2) ! weights for vertical interpolation
      real(pm_reel) ::       pratio ! 'correction' parameter (usually = 1.0, unless
			! outside of sigma range; it then accounts for
			! being above/below lowest/highest sigma level)
      real(pm_reel) ::       lamda   !gravity wave vertical wavelength (m) 
!      real(pm_reel) ::          rdnos(dimnevecs) ! deviates for EOF perturbations
!      save          rdnos
      real(pm_reel) ::          rdeof ! uniform random number in [0,1] for EOF perturb.
      save          rdeof
      real(pm_reel) ::       dev  ! (random) phase angle for gravity waves
      save          dev
      real(pm_reel) ::       pscaleheight    ! scale height
      real(pm_reel) ::       tsurf,tsurfmax,tsurfmin
      real(pm_reel) ::       rmstsurf ! RMS of surface temperature
      real(pm_reel) ::       rmsps    ! RMS of surface pressure
      real(pm_reel) ::       co2ice, dod, rmsdod,dust_mmr !,tsddod
      real(pm_reel) ::       Cp, gamma, viscosity,Rgas
      real(pm_reel) ::       fluxsurf_lw, fluxsurf_sw, fluxtop_lw, fluxtop_sw
      real(pm_reel) ::       rmst, rmsu, rmsv, rmsw
      real(pm_reel) ::       rmsrho    ! rms of density at a given pressure level
      real(pm_reel) ::       altrmsp   ! altitude-wise rms of atmospheric pressure
      real(pm_reel) ::       tmeanl(nmeanvar) ! to temporarily store meanvar()
      real(pm_reel) ::       zradius   ! distance to center of planet
      real(pm_reel) ::       zareoid   ! height above areoid
      real(pm_reel) ::       zsurface  ! height above surface
      real(pm_reel) ::       zpressure ! atmospheric pressure
      real(pm_reel) ::       zmradius  ! altitude above mean Mars radius (3.390E6 m)
      real(pm_reel) ::       sheight(dimlevs) ! altitudes (above surface) of GCM
                                     ! sigma levels

      real(pm_reel) ::       g0  ! reference gravitational acceleration
      real(pm_reel) ::       a0  ! reference distance at which g=g0
      parameter     (a0=3396.E3_pm_reel,g0=3.7257964_pm_reel)

      character(len=16) ::  name
      character(len=4) ::  typevar
      character(len=255) :: dataset
      integer ::      lendataset
      character(len=255) :: dataset2
!  season numbers
      integer ::    nums,nums2 ! encompassing 'lower' and 'higher' month #
      integer ::    numsprec,nums2prec ! previous values of nums & nums2
      save          numsprec,nums2prec
      integer ::    numsprec_eo,numsprec_sd,numsprec_gw
      save          numsprec_eo,numsprec_sd,numsprec_gw
!  seasonal interpolation weights 
      real(pm_reel) ::       wl,wh
!  previous dust scenario
      integer ::    dustprec 
      save          dustprec
!  previous wavelength of gravity wave perturbation
      real(pm_reel) ::       prevgwlength
      save          prevgwlength
! previous value of seedin
      real(pm_reel) ::       prevseedin
      save          prevseedin
! Large scale EOF perturbed fields
      real(pm_reel) ::       ps_gcm_pert   ! perturbed GCM surface pressure
      real(pm_reel) ::       ps_hr_pert    ! perturbed high res surface pressure
      real(pm_reel) ::       temp_gcm_pert(dimlevs) ! perturbed temperature profile
! Near-surface noise (on Ps, Ts and atmospheric temperatures)
      real(pm_reel) ::       ps_noise ! 'noise' to add to surface pressure
      save          ps_noise
      real(pm_reel) ::       temp_noise ! 'noise' to add to temperature
      save          temp_noise
      real(pm_reel) ::       temp_gcm_noise(dimlevs) ! 'noise' to add to temperatures
      save          temp_gcm_noise          ! at GCM levels
      real(pm_reel) ::       ps_noise_dev ! deviate to generate ps_noise
      save          ps_noise_dev
      real(pm_reel) ::       temp_gcm_noise_dev ! deviate to generate temp_noise
      save          temp_gcm_noise_dev
!     vertical coordinates
      real(pm_reel) ::       aps(dimlevs)       ! hybrid coordinate
      save          aps                ! (saved, because loaded only once)
      real(pm_reel) ::       bps(dimlevs)       ! hybrid coordinate
      save          bps                ! (saved, because loaded only once)
      real(pm_reel) ::       sigma(dimlevs)     ! sigma levels to work with
      real(pm_reel) ::       sigma_gcm(dimlevs) ! sigma levels in GCM
      real(pm_reel) ::       sigma_hr(dimlevs)  ! high resolution sigma levels
      real(pm_reel) ::       pseudoalt(dimlevs) ! pseudo altitude
      save          pseudoalt
! flag for first call to call_mcd
      logical ::    firstcall
      data          firstcall /.true./
      save          firstcall
! flag to (internally) set EOF coordinates-related interpolation (or not)
      logical ::    inicoordeof ! see eofpb
      save          inicoordeof
! flag to interally add near-surface noise with perturbations
      logical       nearsurfnoise
      parameter (nearsurfnoise=.true.)

! Variable pour la gestion d'erreur de allocate/deallocate
      integer :: ierralloc

! ===============================================================

! 0. Initializations
      if (firstcall) then ! only for very first call to CALL_MCD
        ! set dummy previous value of seedin
        if (seedin.eq.0) then
          prevseedin=-1.0_pm_reel
        else
          prevseedin=-seedin
        endif
        ! set dummy previous value of gwlength
        if (prevgwlength.eq.0) then
          prevgwlength=1.0_pm_reel
        else
          prevgwlength=-gwlength
        endif      
        ! initialisation factices pour suppression des warning a la compilation
        unet = 0
        unetm = 0
        unetsd = 0
        ueof = 0
        unetm2 = 0
        unetsd2 = 0
        unetm_up = 0
        unetsd_up = 0 
        unetm2_up = 0
        unetsd2_up = 0

        ! set dummy previous values of months
        numsprec=0
        nums2prec=0
        numsprec_eo=0
        numsprec_sd=0
        numsprec_gw=0
        ! set dummy previous value of dust scenario
        dustprec=0
        ! set firstcall to false
        firstcall=.false.
      endif

      ! initializations for every call to CALL_MCD
      ier=0
      inicoordeof=.true.



!***********************************************************************
!  1. Check (and eventually convert) input arguments
!***********************************************************************

!  1.1 Path to data set
!     find last non-blank character
      do lendataset = len(dset), 1, -1
         if (dset(lendataset:lendataset).ne.' ') go to 100
      end do
100   continue

      ! Si l'option dset n'a pas été spécifiée, on utilise le chemin
      ! par défaut.
      if (lendataset.eq.0) then 
         !  acces au repertoire du nouveau modele version 4.3 par COMPAS 
         ier = cps_getFichierModele("atmosphere", "EMCD4_3_1", dataset, &
           rep=.true.)
         if ( MSP_gen_messages ("cps_atmemcd_431") ) then
            return
         end if
         dataset2 = dataset 
         ! valeur attendue dataset = '/usr/local_ms/data/data_emcd_4.2/' 
      else  !symbolic link or full path to datasets is given explicitly
         dataset = dset
         dataset2 = dset
      end if

      ! Ajout du caractere "/" en fin de chaine si besoin
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
         write(out,*)'CALL_MCD Error: ',scena,' unknown scenario'
        endif
        ier=2
        goto 9999
      endif

!  1.3 Check vertical coordinate type (and set high_res_topo flag)
      if ((zkey.gt.5).or.(zkey.le.0)) then
        if (output_messages) then
          write(out,*)'CALL_MCD Error: zkey=',zkey, &
                      ' unknown vert. coord'
        endif
        ier=1
        goto 9999
      endif


! 1.4 Check various flags
! 'hireskey' flag:
      if ((hireskey.lt.0).or.(hireskey.gt.1)) then
        if (output_messages) then
          write(out,*)'CALL_MCD Error: wrong value for ', &
                      'parameter hireskey'
          write(out,*)'               hireskey=',hireskey
        endif
        ier=4
        goto 9999
      endif

! 'datekey' flag:
      if ((datekey.lt.0).or.(datekey.gt.1)) then
        if (output_messages) then
          write(out,*)'CALL_MCD Error: wrong value for ', &
                      'parameter datekey'
          write(out,*)'               datekey=',datekey
        endif
        ier=5
        goto 9999
      endif

! 'perturkey' flag:
      if ((perturkey.lt.0).or.(perturkey.gt.5)) then
        if (output_messages) then
         write(out,*)'CALL_MCD Error: wrong value for ', &
                     'parameter perturkey'
         write(out,*)'               perturkey=',perturkey
        endif
        ier=3
        goto 9999
      endif

! 'extvarkey' flag:
      if ((extvarkey.lt.0).or.(extvarkey.gt.1)) then
       if (output_messages) then
        write(out,*)'CALL_MCD Error: wrong value for ', &
                    'parameter extvarkey'
        write(out,*)'               extvarkey=',extvarkey
       endif
       ier=6
       goto 9999
      endif

! check that the value of gwlength is resonable
      if (((gwlength.lt.2.E3_pm_reel).and.(gwlength.ne.0._pm_reel)).or. &
           (gwlength.gt.30.E3_pm_reel)) then
        if (output_messages) then
         write(out,*)'CALL_MCD Error: wrong value for ', &
                     'parameter gwlength'
         write(out,*)'               gwlength=',gwlength
         write(out,*)'(should be in [2000:30000] range)'
        endif
        ier=8
        goto 9999
      endif

! check that the value of 'seedin' is reasonable (in perturkey=5 case only)
      if ((perturkey.eq.5).and.(abs(seedin).gt.4.0_pm_reel)) then
       if (output_messages) then
        write(out,*)'CALL_MCD Error: wrong value for parameter seedin'
        write(out,*)'               seedin=',seedin
        write(out,*)'(should not be more/less ', &
                    'than +/-4 when perturkey=5)'
       endif
       ier=13
       goto 9999
      endif
      
! 1.4 Check and set latitude and longitude
      if (abs(xlat).gt.90.0_pm_reel) then
        if (output_messages) then
         write(out,*)'CALL_MCD Error: wrong value for latitude=',xlat
        endif
        ier=7
        goto 9999
      else
        lat=xlat
      endif

      ! we want longitude to be in [-180:180]
      lon=mod(xlon,360.0_pm_reel)
      if (lon.lt.-180._pm_reel) then ! in case lon in [-360:-180]
        lon=lon+360._pm_reel
      endif
      if (lon.gt.180.0_pm_reel) then ! in case lon in [180:360]
        lon=lon-360._pm_reel
      endif

! 1.5 Perturbations

! 1.5.1 (re-)initialize EOF and GW perturbations, if instructed to do so
! Note: since on first call seedin.ne.prevseedin, the random number generator
! will always be seeded, which is good, since even when no perturbations
! are added, some extra variables need random numbers
      if (seedin.ne.prevseedin) then
        ! generate the seed for random number generation
        ! (seed must then be a strictly negative integer)
        seed=-abs(int(seedin))
        if (seed.eq.0) then
          seed=-1
        endif
        ! compute rdeof for EOFs
        rdeof=ran1(seed)
        ! compute dev for GW
        dev=ran1(seed)
        ! compute gaussian deviates for near-surface perturbations
        temp_gcm_noise_dev=gasdev(seed)
        ps_noise_dev=gasdev(seed)
        ! store current seed in seedout
        seedout=seed
        ! set prevseedin to seedin and prevgwlength to gwlength
        prevseedin=seedin
        prevgwlength=gwlength
      endif ! of if (seedin.ne.prevseedin)

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
      if ((perturkey.eq.3).or.(perturkey.eq.4).or. & ! GW perturbations
           (extvarkey.eq.1)) then ! some extra variables need 'gwlength'
        ! set wavelength 'lamda'
        if (gwlength.eq.0.0_pm_reel) then ! use a default wavelength
          lamda=16.E3_pm_reel
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
          write(out,*)'CALL_MCD error: The given Julian date ', &
                      'xdate=',xdate
          write(out,*)' lies outside of the [jdate_min:jdate_max] ', &
                      'range!!'
         endif
         ier=10
         goto 9999
        endif
        
        ! check that user did not try to impose a localtime
        if (localtime.ne.0.0_pm_reel) then
         if (output_messages) then
          write(out,*)'CALL_MCD error: If using Julian dates, ', &
                    'localtime=',localtime,' must be set to zero!'
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
        ls=xdate
        ! check that the value of ls makes sense
        if ((ls.lt.0.0_pm_reel).or.(ls.gt.360.0_pm_reel)) then
         if (output_messages) then
           write(out,*)'CALL_MCD error: The given value of ls, xdate=', &
                     xdate
           write(out,*)' should not lie outside the [0:360] range!!'
         endif
         ier=9
         goto 9999
        endif
        
        ! find the modelday which corresponds to ls
        modelday=ls2sol(ls)
        
        ! get localtime from input arguments
        loctime=localtime
        
        ! check that localtime makes sense
        if ((loctime.lt.0.0_pm_reel).or.(loctime.gt.24.0_pm_reel)) then
         if (output_messages) then
           write(out,*)'CALL_MCD error: local time, localtime=', &
                       localtime
           write(out,*)' should not lie outside the [0:24] range!!'
         endif
         ier=11
         goto 9999
        endif
        
      endif ! of if (datekey.eq.0)

! 1.6.2 Compute the season numbers date corresponds to
      if (((dust.eq.4).or.(dust.eq.5).or.(dust.eq.6)).and. &
       (ls.lt.180._pm_reel)) then
        if (output_messages) then
         write(out,*)'CALL_MCD Error: no dust storm scenario for Ls=',Ls
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

!     Compute utime (true solar time at lon=0)
      call mars_ptime(lon,loctime,utime)

! -------------------------------------------
!  Allocation des variables globales
!  La désallocation se fait par un appel a cps_modele_emcd431_close()
! -------------------------------------------
      if (.not.modele_init) then

         ! Desallocation si besoin est
         if ( associated(var_2d) )     deallocate(var_2d,  stat=ierralloc)
         if ( associated(var_2d2) )    deallocate(var_2d2,  stat=ierralloc)

         if ( associated(var_3d) )     deallocate(var_3d,  stat=ierralloc)
         if ( associated(var_3d2) )    deallocate(var_3d2,  stat=ierralloc)

         if ( associated(varrms2d) )   deallocate(varrms2d,  stat=ierralloc)
         if ( associated(varrms2d2) )  deallocate(varrms2d2,  stat=ierralloc)
         if ( associated(varrms3d) )   deallocate(varrms3d,  stat=ierralloc)
         if ( associated(varrms3d2) )  deallocate(varrms3d2,  stat=ierralloc)


         if ( associated(vararms3d) )  deallocate(vararms3d,  stat=ierralloc)
         if ( associated(vararms3d2) ) deallocate(vararms3d2,  stat=ierralloc)


         if ( associated(tabpc) )      deallocate(tabpc,  stat=ierralloc)
         if ( associated(tabpcsmth) )  deallocate(tabpcsmth,  stat=ierralloc)
         if ( associated(tabeops) )    deallocate(tabeops,  stat=ierralloc)
         if ( associated(tabeot) )     deallocate(tabeot,  stat=ierralloc)
         if ( associated(tabeou) )     deallocate(tabeou,  stat=ierralloc)
         if ( associated(tabeov) )     deallocate(tabeov,  stat=ierralloc)

         ! Allocation de la taille définitive des tableaux
         allocate(var_2d(dimlon,dimlat,dimuti,nbvar2d))
         allocate(var_2d2(dimlon,dimlat,dimuti,nbvar2d))

         allocate(var_3d(dimlon,dimlat,dimlevs,dimuti,nbvar3d))
         allocate(var_3d2(dimlon,dimlat,dimlevs,dimuti,nbvar3d))

         allocate(varrms2d(dimlon,dimlat,nbsd2d))
         allocate(varrms2d2(dimlon,dimlat,nbsd2d))
         allocate(varrms3d(dimlon,dimlat,dimlevs,nbsd3d) )
         allocate(varrms3d2(dimlon,dimlat,dimlevs,nbsd3d) )

         allocate(vararms3d(dimlon,dimlat,dimlevs,nbsd3d+1) )
         allocate(vararms3d2(dimlon,dimlat,dimlevs,nbsd3d+1)) 

         allocate(tabpc(dimlateo,dimeoday,dimnevecs))
         allocate(tabpcsmth(dimlateo,dimeoday,dimnevecs))
         allocate(tabeops(dimloneo,dimlateo,dimnevecs))
         allocate(tabeot(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeou(dimloneo,dimlateo,dimlevs,dimnevecs))
         allocate(tabeov(dimloneo,dimlateo,dimlevs,dimnevecs))

         ! Initialisation des tableau a zero       
         var_2d(:,:,:,:) = 0.0_pm_reel
         var_2d2 (:,:,:,:) = 0.0_pm_reel
	
         var_3d(:,:,:,:,:) = 0.0_pm_reel
         var_3d2(:,:,:,:,:) = 0.0_pm_reel
	
         varrms2d(:,:,:) = 0.0_pm_reel
         varrms2d2(:,:,:) = 0.0_pm_reel
         varrms3d (:,:,:,:) = 0.0_pm_reel
         varrms3d2(:,:,:,:)  = 0.0_pm_reel

         vararms3d(:,:,:,:) = 0.0_pm_reel
         vararms3d2 (:,:,:,:) = 0.0_pm_reel

         tabpc(:,:,:) = 0.0_pm_reel
         tabpcsmth(:,:,:) = 0.0_pm_reel
         tabeops(:,:,:) = 0.0_pm_reel
         tabeot(:,:,:,:) = 0.0_pm_reel
         tabeou(:,:,:,:) = 0.0_pm_reel
         tabeov(:,:,:,:)  = 0.0_pm_reel   

         ! Mise a jour du flag pour l'identification
         modele_init = .true.
      endif
! ------------------------------------------------------------

!***********************************************************************
! 2. load appropriate datasets from the database
!***********************************************************************

!     if the scenario changes, reset season numbers to reload all needed arrays
      if (dust.ne.dustprec) then
         numsprec=0
         nums2prec=0
         numsprec_eo=0
         numsprec_sd=0
         numsprec_gw=0
         dustprec=dust
      endif

!     if the season number changes, mean value has to be reloaded
      if ((nums.ne.numsprec).or.(nums2.ne.nums2prec)) then
      ! Note: both nums and nums2 must be checked because for dust storm
      !       scenarios for limit cases (month 7 and 12) nums=nums2
      !       but moving on to month 8 (or 11), only nums2 (or nums)
      !       has changed
!       if (output_messages) then
!        write(out,*) 'Loading variables for new season'
!       endif

!       open appropriate  NETCDF file(s)
!       *****************************              
        call opend(unet,unetm,unetsd,unetm_up,unetsd_up,ueof, &
              nums,dust,dataset(1:lendataset),ierr)
	
        if (ierr.ne.0) then
           ier=ierr ! error code is set in opend
           goto 9999
        endif
        
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
        if (numsprec.eq.0) then ! true for the very first call to "CALL_MCD"
	   ! load hybrid coordinates
           typevar='hybr'
           call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar, &
                        aps,bps,pseudoalt,ierr)	   
	   		
     	   ! aps() and bps() (and pseudoalt()) are now set
           ! note that these are 'saved' and need only be read once
           if (ierr.ne.0) then
             ier=ierr ! error value is set in loadvar
             go to 9999
           endif
	   
	   ! load orography and areoid (on GCM grid)
           typevar='orog'
           call loadvar(unet,unet,unet,unet,typevar, &
                        aps,bps,pseudoalt,ierr)
	   ! taborog() and tabareo() (which are commons in constants_mcd.inc)
           ! are now set
           if (ierr.ne.0) then
              ier=ierr ! error value is set in loadvar
              go to 9999
           endif
        endif ! of if (numsprec.eq.0)

!       load mean variables      
!       *******************
!        if (output_messages) then
!          write(out,*) 'Loading new mean variable'
!        endif
        typevar='mean'
        call loadvar(unetm,unetm_up,unetm2,unetm2_up,typevar, &
                        aps,bps,pseudoalt,ierr)
	! var_2d(), var_2d2(), var_3d() and var_3d2() are now set
        if (ierr.ne.0) then
           ier=ierr ! error value is set in loadvar
           go to 9999
        endif

        ! set numsprec and nums2prec to the current value of nums and nums2
        numsprec=nums
        nums2prec=nums2

      end if !end if ((nums.ne.numsprec).or.(nums2.ne.nums2prec))

!     if large scale perturbations or extra variables are requested, 
!     load data if not yet done (reading once is enough for all season)   
!     **************************************************************      
      if (((perturkey.eq.2).or.(perturkey.eq.4).or.(extvarkey.eq.1)) &
           .and.(numsprec_eo.eq.0)) then
!         if (output_messages) then
!          write(out,*) 'Loading new EOF variable'
!         endif
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
!        if (output_messages) then
!          write(out,*) 'Loading new grwp variable'
!        endif
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
       ! Note: we load both kinds of RMS since (a rare but possible
       !       eventuality) 'zkey' could change from one call to call_mcd
       !       to the next
        ! Pressure-wise RMS
        typevar='rms'
        call loadvar(unetsd,unetsd_up,unetsd2,unetsd2_up,typevar, &
                        aps,bps,pseudoalt,ierr)
        if (ierr.ne.0) then
           ier=ierr ! error value is set in loadvar
           go to 9999
        endif
        ! Altitude-wise RMS
        typevar='arms'
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
      call var2d(oro_gcm,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh,1.0_pm_reel)
      ! oro_gcm is now set to the value of GCM orography at (lon,lat)

      name='areoid'
      call var2d(areoid_gcm,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh,1.0_pm_reel)
      ! areoid_gcm is now set to the value of GCM areoid at (lon,lat)
      
      if (hireskey.eq.1) then
      ! get high resolution areoid at (lon,lat)
        call molareoid(dataset(1:lendataset),lon,lat,areoid_hr)
      endif
      
!  3.2 Get surface pressure at desired lon,lat and utime
      ! low resolution (ie: GCM) surface pressure
      name='ps'
      call var2d(ps_gcm,lon,lat,utime,name,ierr,itimint,wl,wh,1.0_pm_reel)

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
        ps_psgcm=1._pm_reel
      endif ! of if (hireskey.eq.1)

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
          p_pgcm(l)=1._pm_reel ! No high_res, so set p_pgcm(:) to 1
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
                 R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure, &
                 zmradius,sheight)
      ! levhi,levlow,levweight(),pratio are now set
      ! zareoid,zradius,zsurface,zpressure and sheight() are also set
      
      if (ierr.ne.0) then
! Note: Error message and error value are given in getsi
         ier=ierr
        goto 9999
      end if

      height=zareoid              ! height above areoid
      absheight=height-oroheight  ! height above surface
      
!      if underground : stop
      if (absheight.lt.-0.01_pm_reel) then
        if (output_messages) then
          write(out,*)'CALL_MCD Error: underground object '
          write(out,*)'                absheight=',absheight
        endif
        ier=17
        goto 9999
      endif

!  3.8 Compute mean field values (winds,density,temperature,pressure)
!      at chosen location

      name='u'
      call var3d(ul,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,name,ierr,itimint,wl,wh)
      name='v' 
      call var3d(vl,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,name,ierr,itimint,wl,wh)
      name='temp'
      call var3d(tl,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,name,ierr,itimint,wl,wh)
      name='rho'
      call var3d(rhol,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,name,ierr,itimint,wl,wh)

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

      ! 4.1.0 Compute and add near-surface perturbations
        if (nearsurfnoise.and.(perturkey.gt.1)) then
          call nearsurfacenoise(ps,ps_noise_dev,ps_noise, &
           temp_gcm_noise_dev,temp_gcm_noise,sheight)

          do l=1,dimlevs
            temp_gcm(l)=temp_gcm(l)+temp_gcm_noise(l)
          enddo
        else
          ps_noise=0
          temp_noise=0
        endif ! of if (nearsurfnoise.and.(perturkey.gt.1))

      ! 4.1.1 Build perturbed surface pressure
         name='ps'
         call eofpb(inicoordeof,scena,pertm,pertr,rdeof,lon,lat, &
              levhi,levlow,levweight,modelday,name,ierr)
         ps_gcm_pert=ps_gcm+pertr+ps_noise
         ! recompute 'high resolution' surface pressure, if required
         if (hireskey.eq.1) then
           call pres0(dataset(1:lendataset),lat,lon,ls,utime, &
                      ps_gcm_pert,oro_gcm,wl,wh,ps_hr_pert,oro_hr,ierr)
           if (ierr.ne.0) then
             ier=ierr ! error value set in pres0
             goto 9999
           endif
           ps=ps_hr_pert
           ps_psgcm=ps_hr_pert/ps_gcm_pert
         else
           ps=ps_gcm_pert
           ps_psgcm=1._pm_reel
         endif ! if (hireskey.eq.1)

      ! 4.1.2 Build corresponding perturbed sigma levels
         ! perturbed GCM sigma levels
         do l=1,dimlevs
           sigma_gcm(l)=aps(l)/ps_gcm_pert+bps(l)
         enddo
         ! perturbed high res. sigma levels
         if (hireskey.eq.1) then
           call build_sigma_hr(sigma_gcm,ps_gcm_pert, &
                               ps_hr_pert,sigma_hr,p_pgcm)
         endif         
         ! sigma levels which will be used further on
         if (hireskey.eq.1) then
           do l=1,dimlevs
             sigma(l)=sigma_hr(l)
           enddo
         else
           do l=1,dimlevs
             sigma(l)=sigma_gcm(l)
             p_pgcm(l)=1  ! no high res, p_pgcm(:)=1
           enddo
         endif
         
      ! 4.1.3 Build perturbed temperature profile
         name='temp'
         ! NB: imposed perturbed temperatures on the profile up to
         !     levhi+2 (since what happens above won't matter when
         !     integrating the hydrostatic equation in getsi)
         call profi_eof(inicoordeof,scena,temp_gcm,rdeof,lon,lat, &
                        modelday,levhi+2,name,temp_gcm_pert,ierr) 

         ! store 'unperturbed' values of levlow and levhi
!         oldlevlow=levlow
!         oldlevhi=levhi
!         oldlevweight2=levweight(2)

      ! 4.1.4 Compute new 'levhi,levlow,levweight,pratio'
         call getsi(xz,zkey,oroheight,areoid,ps,sigma, &
                levhi,levlow,levweight,pratio,ierr, &
                R_gcm,temp_gcm_pert,zareoid,zradius,zsurface,zpressure, &
                zmradius,sheight)
      ! levhi,levlow,levweight(),pratio are now set
      ! zareoid,zradius,zsurface,zpressure and sheight are also set
         if (ierr.ne.0) then
! Note: Error message and error value are given in getsi
           ier=ierr
           goto 9999
         endif

         height=zareoid              ! height above areoid
         absheight=height-oroheight  ! height above surface

!      if underground : stop
         if (absheight.lt.-0.01_pm_reel) then
           if (output_messages) then
             write(out,*)'CALL_MCD Error: underground object '
             write(out,*)'                absheight=',absheight
           endif
           ier=17
           goto 9999
         endif

      ! 4.1.5 Get new mean values and add EOF perturbations
         name='u'
         if (zsurface.ge.z_0) then
         ! if above aerodynamic roughness length, compute and add perturbation
           call eofpb(inicoordeof,scena,pertm,pertr,rdeof,lon,lat, &
                    levhi,levlow,levweight,modelday,name,ierr)
           ul=ul+pertr
!         else ! below aerodynamic roughness length, no perturbation
         endif

         name='v'
         if (zsurface.ge.z_0) then
         ! if above aerodynamic roughness length, compute and add perturbation
           call eofpb(inicoordeof,scena,pertm,pertr,rdeof,lon,lat, &
                    levhi,levlow,levweight,modelday,name,ierr)
           vl=vl+pertr
!         else ! below aerodynamic roughness length, no perturbation
         endif
         
         name='temp'
         call eofpb(inicoordeof,scena,pertm,pertr,rdeof,lon,lat, &
                    levhi,levlow,levweight,modelday,name,ierr)
         if (nearsurfnoise) then
           temp_noise=temp_gcm_noise(levlow)+ &
            (temp_gcm_noise(levhi)-temp_gcm_noise(levlow))*levweight(1)
         else
           temp_noise=0.0
         endif
         tl=tl+pertr+temp_noise

! straightforward (model 0) version:
!        get density as 'mean field' from perturbed state
!         name='rho'
!         call var3d(rhol,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
!                    sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
!                    itimint,wl,wh) 

      endif ! if ((perturkey.eq.2).or.(perturkey.eq.4)) 

! 4.2 Add small scale (gravity wave) variability, if required
!************************************************************      
      if ((perturkey.eq.3).or.(perturkey.eq.4)) then
         name='u'
         absheight=height-oroheight
         call grwpb(pertu,dev,lamda,lon,lat,absheight,tmeanl(2), &
                    tmeanl(4),tmeanl(5),ps,sigma, &
                    utime,name,ierr,itimint,wl,wh, &
                    R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)
         name='v'
         call grwpb(pertv,dev,lamda,lon,lat,absheight,tmeanl(2), &
                    tmeanl(4),tmeanl(5),ps,sigma, &
                    utime,name,ierr,itimint,wl,wh, &
                    R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)
         name='temp'
         call grwpb(pertt,dev,lamda,lon,lat,absheight,tmeanl(2), &
                    tmeanl(4),tmeanl(5),ps,sigma, &
                    utime,name,ierr,itimint,wl,wh, &
                    R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)
         name='rho'
         call grwpb(pertrho,dev,lamda,lon,lat,absheight,tmeanl(2), &
                    tmeanl(4),tmeanl(5),ps,sigma, &
                    utime,name,ierr,itimint,wl,wh, &
                    R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)
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
                   utime,name,ierr,itimint,wl,wh, &
                   R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)
      endif

! 4.3 Add n sigmas, if required
!****************************** 
      if (perturkey.eq.5) then
        if (zkey.eq.4) then ! Get pressure-wise RMSs
          name='rmsu'
          call var3d(pertu,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
          name='rmsv'
          call var3d(pertv,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
          name='rmsrho'
          call var3d(pertrho,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
          name='rmstemp'
          call var3d(pertt,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
        else ! get altitude-wise RMSs
          name='armsu'
          call var3d(pertu,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
          name='armsv'
          call var3d(pertv,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh) 
          name='armsrho'
          call var3d(pertrho,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
          name='armstemp'
          call var3d(pertt,lon,lat,zsurface,levhi,levlow,levweight, &
                     pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                     itimint,wl,wh)
        endif ! of if (zkey.eq.4)
         name='rmsps'
         call var2d(pertps,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh,ps_psgcm)
         ul = ul + (nbsig*pertu)
         vl = vl + (nbsig*pertv)
         rhol = rhol + (nbsig*pertrho)
         tl = tl + (nbsig*pertt)
         ps = ps + (nbsig*pertps)
         ! check that this has not led to unphysical values
         if (rhol.lt.0._pm_reel) then
           if (output_messages) then
             write(out,*)'CALL_MCD Error: unphysical density:',rhol
             write(out,*)'  due to addition of ',nbsig, &
                       ' times the day to day variability'
           endif
           ier=18
           goto 9999
         endif
         if (tl.lt.0._pm_reel) then
           if (output_messages) then
             write(out,*)'CALL_MCD Error: unphysical temperature:',tl
             write(out,*)'  due to addition of ',nbsig, &
                       ' times the day to day variability'
           endif
           ier=19
           goto 9999
         endif
         if (ps.lt.0._pm_reel) then
           if (output_messages) then
             write(out,*)'CALL_MCD Error: unphysical pressure:',ps 
             write(out,*)'  due to addition of ',nbsig, &
                       ' times the day to day variability'
           endif
           ier=20
           goto 9999
         endif
      endif ! of if (perturkey.eq.5)

! 4.4 Store atmospheric fields (pressure, temperature, density, winds)
!     near-surface noise (on Ps, Ts and atmospheric temperatures)
!      if ((nearsurfnoise.and.(perturkey.gt.1)).and.
!     &    (zsurface.le.sheight(1))) then
      ! the vertical interpolation scheme (see var3d() routine) uses
      ! unperturbed surface temperature, so we must correct temperatures
      ! at altitudes below sheight(1) to account for perturbed surface
      ! temperature
!        tl=tl+tsurf_noise*(1.0-zsurface/sheight(1))
!      endif

! 4.5 Store atmospheric fields (pressure, temperature, density, winds)
!     (re-)compute atmospheric pressure
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
         pscaleheight=Rnew*t/(g0*a0**2/(a0+zareoid)**2)

         name='tsurf'
         call var2d(tsurf,lon,lat,utime,name,ierr,itimint,wl,wh, &
                    ps_psgcm)
         call max2d(tsurfmax,lon,lat,name,ierr,itimint,wl,wh)
         call min2d(tsurfmin,lon,lat,name,ierr,itimint,wl,wh)
         name='rmstsurf'
         call var2d(rmstsurf,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh, &
                    ps_psgcm)
         name='rmsps'
         call var2d(rmsps,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh, &
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
         if (zkey.eq.4) then ! pressure-wise RMS
           name='rmstemp'
           call var3d(rmst,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='rmsrho'
           call var3d(rmsrho,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='rmsu'
           call var3d(rmsu,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='rmsv'
           call var3d(rmsv,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='rmsw'
           call var3d(rmsw,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
         else ! altitude-wise RMS
           name='armstemp'
           call var3d(rmst,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='armsu'
           call var3d(rmsu,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='armsv'
           call var3d(rmsv,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='armsw'
           call var3d(rmsw,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='armsrho'
           call var3d(rmsrho,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
           name='armspressure'
           call var3d(altrmsp,lon,lat,zsurface,levhi,levlow,levweight, &
                      pratio,sheight,p_pgcm,ps_psgcm,1.0_pm_reel,name,ierr, &
                      itimint,wl,wh)
         endif
           name='w' 
         call var3d(w_l,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='q2'
         call var3d(q2,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_h2ovapor'
         call var3d(vmr_h2o,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_h2oice'
         call var3d(vmr_h2oice,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_o3'
         call var3d(vmr_o3,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_o'
         call var3d(vmr_o,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_co2'
         call var3d(vmr_co2,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_co'
         call var3d(vmr_co,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='vmr_n2'
         call var3d(vmr_n2,lon,lat,zsurface,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,name,ierr, &
                    itimint,wl,wh)
         name='dod'
         call var2d(dod,lon,lat,utime,name,ierr,itimint,wl,wh, &
                    ps_psgcm)
         call dustmix(scena,lat,Ls,dod,ps,p,dust_mmr)
         name='rmsdod'
         call var2d(rmsdod,lon,lat,1.0_pm_reel,name,ierr,itimint,wl,wh, &
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
         extvar(11)=0.0_pm_reel
         extvar(12)=0.0_pm_reel  ! no pertrhoeof any more
         extvar(13)=pscaleheight
         extvar(14)=oro_gcm      ! GCM orography
         extvar(15)=tsurf
         extvar(16)=tsurfmax
         extvar(17)=tsurfmin
         extvar(18)=rmstsurf
         if (hireskey.eq.1) then
           extvar(19)=ps_hr     ! high res Ps
         else
           extvar(19)=ps_gcm
         endif
         extvar(20)=ps_gcm      ! low res Ps
         if (zkey.eq.4) then   ! vertical coordinate is pressure
           extvar(21)=0.0_pm_reel
         else
           extvar(21)=altrmsp  ! RMS of atmospheric pressure
         endif
         extvar(22)=rmsps ! RMS of surface pressure
         extvar(23)=rmst
         extvar(24)=rmsu
         extvar(25)=rmsv
         extvar(26)=w_l
         extvar(27)=rmsw
         extvar(28)=pertrhogw
         extvar(29)=q2
         extvar(30)=0.0_pm_reel
         extvar(31)=fluxsurf_lw
         extvar(32)=fluxsurf_sw
         extvar(33)=fluxtop_lw
         extvar(34)=fluxtop_sw
         extvar(35)=co2ice
         extvar(36)=dod
         extvar(37)=dust_mmr
         extvar(38)=rmsdod
         extvar(39)=0.0_pm_reel
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
           extvar(i)=0.0_pm_reel
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
            extvar(i)=0.0_pm_reel
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
 9999 pres=-999.0_pm_reel
      dens=-999.0_pm_reel
      temp=-999.0_pm_reel
      zonwind=-999.0_pm_reel
      merwind=-999.0_pm_reel
      do i=1,nmeanvar
         meanvar(i)=-999.0_pm_reel
      end do
      do i=1,nextvar
         extvar(i)=-999.0_pm_reel
      enddo

      return
      end subroutine call_mcd ! end of call_mcd

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine eofpb(inicoord,scena,pertm,pertr,rdeof,lon,lat, &
                       levhi,levlow,levweight,day,name,ier)
      
!     Calculate an EOF perturbation on the 3d variable=name at 
!     longitude=lon, latitude=lat and day=otherday.
!     where otherday=day+(rdeof-0.5)*eof_twindow , with rdeof in [0:1]
!
!     Improved variability model...EOFs calculated in longitude-sigma plane
!
!         Updated feb-2008: added lon-lat-time interpolation of perturbations
!                           + fixed "dust storm case" (where data is only
!                            available over half of the year). E.M.
      implicit none

  
!----------------      
!     Entrées
      logical, intent(inout) :: inicoord ! flag to (re-)set evaluation of lon/lat/time
                                       ! index of encompassing neighbours
      integer, intent(in) ::       scena   ! dust scenario (4,5 or 6 == dust storm)
!      real(pm_reel) ::       rdnos(dimnevecs) !Uniform deviates for a single profile
      real(pm_reel), intent(in) ::          rdeof            ! uniform deviate
      real(pm_reel), intent(in) ::          lon              !longitude east of point 
      real(pm_reel), intent(in) ::          lat              !latitude of point
      integer, intent(in) ::       levhi            !database level upper bound
      integer, intent(in) ::       levlow           !database level lower bound
      real(pm_reel), intent(in) ::          levweight(2)     !level weight for interpolation
!     (1) for linear in height (2) for linear in pressure
      real(pm_reel), intent(in) ::          day              !model day [0:668.6]
      character(len=16), intent(in) ::  name             !name of variable
 
!----------------          
!     Sorties
      integer, intent(out) ::   ier     ! error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) ::      pertm   ! perturbation corresponding to trend (not used!!)
      real(pm_reel), intent(out) ::      pertr   ! perturbation corresponding to random component
  
!----------------         
!     Variables locales
      logical :: firstcall              ! flag to signal initializations
      data firstcall/.true./
      save firstcall
      real(pm_reel) :: lat_eof(dimlateo)         ! latitudes, along EOF grid
      real(pm_reel) :: lon_eof(dimloneo)        ! longitudes, along EOF grid
      save lat_eof,lon_eof
      
      integer :: indlat(2) ! indexes of encompassing latitudes
      data indlat /0,0/ ! dummy initialization to get rid of compiler warning
      integer :: indlon(2) ! indexes of encompassing longitudes
      data indlon /0,0/ ! dummy initialization to get rid of compiler warning
      integer :: indtime(2) ! indexes of encompassing dates
      data indtime /0,0/ ! dummy initialization to get rid of compiler warning
      save indlat,indlon,indtime
      
      real(pm_reel) :: rel_lat ! relative position of lat between encompassing latitudes
      data rel_lat /0.0_pm_reel/ ! dummy initialization to get rid of compiler warning
      real(pm_reel) :: rel_lon ! relative position of lon between encompassing longitudes
      data rel_lon /0.0_pm_reel/ ! dummy initialization to get rid of compiler warning
      real(pm_reel) :: rel_time ! relative position of time between encompassing times
      data rel_time /0.0_pm_reel/ ! dummy initialization to get rid of compiler warning
      save rel_lat,rel_lon,rel_time
      
      real(pm_reel) :: marsyear
      parameter (marsyear=668.6_pm_reel) ! number of sols in a martian year
      ! Time window over which EOF perturbations are picked:
      real(pm_reel) :: eof_twindow ! window size (sols)
      parameter (eof_twindow=30.0_pm_reel)
      
      real(pm_reel) :: otherday  ! model day at which values are sought
      ! values at 4 encompassing nodes and 2 encompassing times
      real(pm_reel) :: values(4,2) !,smthvalues(4,2)
      ! bilinearly interpolated values, at 2 encompassing times
      real(pm_reel) ::  val(2) !,smthval(2)
      ! final values, interpolated in time
      real(pm_reel) :: fval !,fsmthval
      
      integer ::       i,inode,ilon,ilat,itim
      real(pm_reel) ::  norm    ! normalization factor
      real(pm_reel) ::     pcpert(dimnevecs,4,2) ! 'perturbation' principal components
      save     pcpert
      real(pm_reel) ::         evecs(dimnevecs)

      ! 1. Initializations
      
      ! first call only initializations
      if (firstcall) then
       ! build latitudes of EOF grid
       ! (NB: dimlateo,latmaxeo,... are known from constants_mcd.inc)
       do i=1,dimlateo
         lat_eof(i)=latmaxeo-(i-1)*deltalateo
       enddo
       ! build longitudes of EOF grid
       do i=1,dimloneo
         lon_eof(i)=lonmineo+(i-1)*deltaloneo
       enddo
        firstcall=.false.
      endif

      ! initialize return code
      ier = 0
      

      ! 2. find indexes of encompassing points
      ! NB: Only need to do this once every call to call_mcd
      if (inicoord) then
      ! 2.1. find encompassing longitudes
       if (lon.ge.lon_eof(dimloneo)) then ! wraparound
        indlon(1)=dimloneo
        indlon(2)=1
        rel_lon=(lon-lon_eof(dimloneo))/deltaloneo
       else
        indlon(1)=1
        do i=1,dimloneo-1
          if ((lon.ge.lon_eof(i)).and.(lon.lt.lon_eof(i+1))) then
            indlon(1)=i
          endif
        enddo
        indlon(2)=indlon(1)+1
        rel_lon=(lon-lon_eof(indlon(1)))/deltaloneo
       endif ! of if (lon.ge.lon_eof(dimloneo))

      ! 2.2 Find encompassing latitudes
       if (lat.ge.lat_eof(1)) then ! beyond northernest latitude
        indlat(1)=1
        indlat(2)=1
        rel_lat=0
       elseif (lat.le.lat_eof(dimlateo)) then ! beyond southernest latitude
        indlat(1)=dimlateo
        indlat(2)=dimlateo
        rel_lat=0
       else ! find encompassing latitudes
        indlat(1)=1
        do i=1,dimlateo-1
          if ((lat.le.lat_eof(i)).and.(lat.gt.lat_eof(i+1))) then
          indlat(1)=i
          endif
        enddo
        indlat(2)=indlat(1)+1
        rel_lat=(lat_eof(indlat(1))-lat)/deltalateo
       endif ! of if (lat.ge.lat_eo(1)) elseif 

      ! 2.3 compute time at which values will be sought
       otherday=mod(marsyear+day+(rdeof-0.5_pm_reel)*eof_twindow,marsyear)
!      otherday=day ! temporary, for tests
!      write(out,*)'eofpb: day=',day,' otherday=',otherday

      ! 2.4 Find encompassing times
       if ((scena.ge.4).and.(scena.le.6)) then
        ! specific to dust storm scenarios (data in Ls 180-360)
        if (otherday.lt.373) then
         ! first day of series; no interpolation
         indtime(1)=1
         indtime(2)=1
         rel_time=0.0_pm_reel
        elseif (otherday.ge.(dimeoday-1)) then
         ! last day of series; no interpolation
         indtime(1)=dimeoday-372
         indtime(2)=dimeoday-372
         rel_time=0.0_pm_reel
        else
         indtime(1)=1
         do i=373,dimeoday-1
          if ((otherday.ge.i).and.(otherday.lt.(i+1))) then
            indtime(1)=i-372 ! because dust storm data starts at month #7
          endif
         enddo
         indtime(2)=indtime(1)+1
         rel_time=(otherday-372.0_pm_reel)-indtime(1)
        endif ! of if (otherday.lt.373) elseif (otherday.ge.(dimeoday-1))
       else ! data is known all year round (MY24,cold & warm scenarios)
        if (otherday.lt.1.0_pm_reel) then ! wraparound
         indtime(1)=dimeoday
         indtime(2)=1
         rel_time=otherday
        elseif (otherday.ge.(dimeoday-1)) then
        ! specific workaround to handle case of
        ! last day of year (which is not of unity length, in the true calendar)
         indtime(1)=dimeoday-1
         indtime(2)=dimeoday
         rel_time=(otherday-int(otherday))*1._pm_reel/0.6_pm_reel ! so that rel_time in [0:1]
        else
         indtime(1)=1
         do i=1,dimeoday-1
          if ((otherday.ge.i).and.(otherday.lt.(i+1))) then
            indtime(1)=i
          endif
         enddo
         indtime(2)=indtime(1)+1
         rel_time=otherday-indtime(1)
        endif ! of if (otherday.lt.1.0)
       endif ! of if (scena.ge.4).and.(scena.le.6)
      
!        write(out,*) "eofpb: day=",day,' otherday=',otherday
!        write(out,*) "eofpb: indtime(1)=",indtime(1)
!        write(out,*) "eofpb: rel_time=",rel_time

      ! 2.5 Build pcpert(dimnevecs,4,2)
       do itim=1,2
        do inode=1,4
          if (inode.le.2) then ! node # 1 or # 2
            ilat=indlat(1)
          else ! node # 3 or # 4
            ilat=indlat(2)
          endif
          ! build pcpert, 'perturbation' component of principal component
          do i=1,dimnevecs
            ! Note: we multiply by 1.205 to preserve overall variance
            pcpert(i,inode,itim)=1.205_pm_reel*(tabpc(ilat,indtime(itim),i) &
                                       -tabpcsmth(ilat,indtime(itim),i))
          enddo
        enddo ! of do inode=1,4
       enddo ! of do itim=1,2
      
      endif ! of if (inicoord)


      ! 3. compute values at encompassing nodes and time
      ! values are organized as follows:  v(1,i) v(2,i)
      !                                   v(4,i) v(3,i)

      do itim=1,2 ! loop on encompassing time
        do inode=1,4 ! loop on encompassing nodes
         if (inode.eq.1) then
          ilon=indlon(1)
          ilat=indlat(1)
         elseif (inode.eq.2) then
          ilon=indlon(2)
          ilat=indlat(1)
         elseif (inode.eq.3) then
          ilon=indlon(2)
          ilat=indlat(2)
         else
          ilon=indlon(1)
          ilat=indlat(2)
         endif
!     read normalisation factor, averages and eofs
!     normalisation factor is no longer the standard deviation
!
!     EOF perturbations are stored up to the last model level
         if (levlow.lt.dimlevs) then  ! we are entirely within EOF range
          if (name.eq.'u') then
            norm=tabeonormu(ilat)
            do i=1,dimnevecs
            evecs(i)= tabeou(ilon,ilat,levlow,i) &
                   +  (tabeou(ilon,ilat,levhi,i) &
                   -  tabeou(ilon,ilat,levlow,i))*levweight(1)
            enddo
          elseif (name.eq.'v') then
            norm=tabeonormv(ilat)
            do i=1,dimnevecs
            evecs(i)= tabeov(ilon,ilat,levlow,i) &
                   +  (tabeov(ilon,ilat,levhi,i) &
                   -  tabeov(ilon,ilat,levlow,i))*levweight(1)
            enddo
          elseif (name.eq.'temp') then
            norm=tabeonormt(ilat)
            if (levweight(1).eq.0.0_pm_reel) then ! happens a lot when
              ! eofpb is called from profi_eof
              do i=1,dimnevecs
                evecs(i)= tabeot(ilon,ilat,levlow,i)
              enddo
            else ! standard case do a vertical interpolation
              do i=1,dimnevecs
                evecs(i)= tabeot(ilon,ilat,levlow,i) &
                       +  (tabeot(ilon,ilat,levhi,i) &
                       -  tabeot(ilon,ilat,levlow,i))*levweight(1)
             enddo
            endif
          elseif (name.eq.'ps') then
            norm=tabeonormp(ilat)
            do i=1,dimnevecs
               evecs(i)= tabeops(ilon,ilat,i)
            enddo
!            average=tabeopsave(ilon,ilat)
          else
           if (output_messages) then
            write(out,*)'EOFPB Error: ',name,' unknown variable name'
           endif
           ier=1
           goto 9999
          endif
         else  ! we are above the top EOF level, most perturbations constant
          if (name.eq.'u') then
            norm=tabeonormu(ilat)
            do i=1,dimnevecs
              evecs(i)= tabeou(ilon,ilat,dimlevs,i)
            enddo
!            average=tabeouave(ilon,ilat,levlow)
          elseif (name.eq.'v') then
            norm=tabeonormv(ilat)
            do i=1,dimnevecs
              evecs(i)= tabeov(ilon,ilat,dimlevs,i)
            enddo
!            average=tabeovave(ilon,ilat,levlow)
          elseif (name.eq.'temp') then
            norm=tabeonormt(ilat)
            do i=1,dimnevecs
              evecs(i)= tabeot(ilon,ilat,dimlevs,i)
            enddo
!            average=tabeotave(ilon,ilat,levlow)
!          elseif (name.eq.'rho') then
!            norm=tabeonormr(ilat)
!            do i=1,dimnevecs
!              evecs(i)= tabeorho(ilon,ilat,dimlevs,i)
!            enddo
          elseif (name.eq.'ps') then
            norm=tabeonormp(ilat)
            do i=1,dimnevecs
              evecs(i)= tabeops(ilon,ilat,i)
            enddo
!            average=tabeopsave(ilon,ilat)
          else
            if (output_messages) then
             write(out,*)'EOFPB Error: ',name,' unknown variable name'
            endif
            ier=1
            goto 9999
          endif ! of if (name.eq.'u') elseif ...
         endif ! of if (levlow.lt.dimlevs)
!
!     calculate perturbation
!
          values(inode,itim)=0.0 ! initialization
          do i=1,dimnevecs !72 !dimnevecs
            values(inode,itim)=values(inode,itim)+ &
                               pcpert(i,inode,itim)*evecs(i)
          enddo
!
!     renormalise and add average
!
          values(inode,itim)=values(inode,itim)*norm
        enddo ! do inode=1,4
        
        ! bilinear interpolation using four neighbours
          ! interpolate the log() ! NO, because these are deviations to mean
          val(itim)=(1._pm_reel-rel_lon)*(1._pm_reel-rel_lat)*values(1,itim) &
                    +rel_lon*(1._pm_reel-rel_lat)*values(2,itim) &
                    +rel_lon*rel_lat*values(3,itim) &
                    +(1._pm_reel-rel_lon)*rel_lat*values(4,itim)
!        endif
      enddo ! do itim=1,2

      ! 4. linear interpolation in time
      fval=(1.0_pm_reel-rel_time)*val(1)+rel_time*val(2)

      pertr=fval

      ! set inicoord to false, since rel_lat, rel_lon, rel_time ... are
      ! now known (and won't change aduring current call to call_mcd).
      inicoord=.false.
      return

!     error handling
 9999 pertm = 0.0_pm_reel
      pertr = 0.0_pm_reel
      return

      end subroutine eofpb

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine profi_eof(inicoord,scena,profile,rdeof,lon,lat,day, &
                           toplevpert,name,profile_pert,ier)

!     Add EOF perturbations to all vertical levels of a (known) profile
      implicit none

!------------------
!     Entrées
      logical, intent(inout) :: inicoord ! flag to (re-)set evaluation of lon/lat/time
                                         ! passed on to eofpb
      integer, intent(in) :: scena ! dust scenario 
      real(pm_reel), intent(in) ::    profile(dimlevs) ! the GCM profile at GCM levels
      real(pm_reel), intent(in) ::    rdeof ! uniform deviate, for EOF perturbations
      real(pm_reel), intent(in) ::    lon   ! east longitude (degrees)
      real(pm_reel), intent(in) ::    lat   ! latitude (degrees)
      real(pm_reel), intent(in) ::    day   ! model day, in [0:668.6]
      integer, intent(in) :: toplevpert ! model layer up to which perturbation will be added
      character(len=16), intent(in) :: name !name of variable

!------------------
!     Sorties
      real(pm_reel), intent(out) ::    profile_pert(dimlevs) ! perturbed profile at GCM levels
      integer, intent(out) :: ier     ! error flag (=0 if OK)

!------------------
! local variables
      integer :: i
      real(pm_reel) :: pertm ! perturbation corresponding to trend (from eofpb)
      real(pm_reel) :: pertr ! perturbation corresponding to random component (from eofpb)
      integer :: levlo ! local value for call to eofpb
      integer :: levhi ! local value for call to eofpb
      real(pm_reel) :: levweight(2) ! local value for call to eofpb
      real(pm_reel) :: pratio ! local (unused) value for call to eofpb
      
      integer :: toppertlev ! level up to which perturbation will be added
      
      ! initialisation des valeurs pour suppression des warning de la compilation
      pratio = 0._pm_reel

      if (toplevpert.gt.dimlevs) then
      ! in case input toplevpert is greater than dimlevs
        toppertlev=dimlevs
      else
        toppertlev=toplevpert
      endif
      
      if (name.eq.'temp') then
        ! add perturbation to levels up to toppertlev
        do i=1,toppertlev
         if (i.lt.dimlevs) then ! general case
          levweight(1)=0.0_pm_reel ! all the weight on levlo
          levlo=i
          levhi=i+1
          ! get EOF perturbation for level levlo
          call eofpb(inicoord,scena,pertm,pertr,rdeof,lon,lat, &
                     levhi,levlo,levweight,day,name,ier)
          ! add EOF perturbation
          profile_pert(levlo)=profile(levlo)+pertr
         else ! special case if last level is also perturbed
          levweight(1)=1.0_pm_reel ! i.e. all the weight on levhi
          levlo=dimlevs-1
          levhi=dimlevs
          ! get EOF perturbation for level levhi
          call eofpb(inicoord,scena,pertm,pertr,rdeof,lon,lat, &
                     levhi,levlo,levweight,day,name,ier)
          ! add EOF perturbation
          profile_pert(levhi)=profile(levhi)+pertr
         endif ! of if (i.lt.dimlevs)
        enddo
        
        ! levels above toppertlev are not perturbed
        do i=toppertlev+1,dimlevs
          profile_pert(i)=profile(i)
        enddo
      else
        if (output_messages) then
          write(out,*) 'PROFI_EOF Error: unknowm name:',name
        endif
        ier=1
      endif ! of if (name.eq.'temp')
      
      end subroutine profi_eof


      function gasdev(idum)

!     Return a Gaussian deviate with zero mean and unit standard deviation.
!     Uses function ran1()
!     If idum is negative, then deviates are (re-)initialized.
! Note: When initialized, the function actually computes 2 deviates,
!       'gset' and 'gasdev'. It then returns 'gasdev' and saves 'gset'
!       which will be returned at the next call of the function.
      implicit none

!     inputs
      integer, intent(inout) :: idum    ! Seed for random number generator
                      ! is altered if calls to function ran1() occur

      real    gasdev

!     local variables
      integer iset    ! flag (triggers the generation of 2 deviates if =0)
      data    iset/0/
      save    iset
      real    v1      ! random number (between -1 and 1)
      real    v2      ! random number (between -1 and 1)
      real    r
      real    fac
      real    gset    ! extra deviate (saved for next call to function)
      save    gset

      if (idum.lt.0) iset=0  ! Reinitialize deviates generation
      if (iset.eq.0) then ! compute 2 deviates, gasdev and gset
1       v1=real(2.*ran1(idum)-1.,kind=4)
        v2=real(2.*ran1(idum)-1.,kind=4)
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
      subroutine getsi(xz,zkey,oroheight,areoid,ps,sigma,levhi,levlow, &
         levweight,pratio,ier,R_gcm,temp_gcm,zareoid,zradius,zsurface, &
         zpressure,zmradius,sheight)

!     Find the nearest datafile levels and vertical interpolation weights at
!     longitude=lon, latitude=lat and vertical position xz (which may be
!     any of 'above areoid' or 'above surface'
!      or 'radius from center of planet' 
!      or 'altitude above mean radius'
!      or 'Pressure' coordinate)
!     Last Updated feb-2008: added 'zmradius' input
!                           +improved Tmean interpolation   E.M. 
      implicit none    
 
!----------------     
!     Entrées

      real(pm_reel), intent(in) ::          xz ! altitude (see zkey)
      integer, intent(in) ::       zkey             
!  zkey    :                   switch to choose which z variable
!                               is input and used to find other three :
!           zkey=1  zradius (m)  -> zareoid, zsurface, zpressure, zmradius
!           zkey=2  zareoid (m)  -> zradius, zsurface, zpressure, zmradius
!           zkey=3  zsurface (m) -> zradius, zareoid, zpressure, zmradius
!           zkey=4  zpressure (Pa) -> zradius, zareoid, zsurface, zmradius
!           zkey=5  zmradius (m) -> zradius, zareoid, zsurface, zpressure
!           zkey=-5  case of getsi call by grwpb 
      real(pm_reel), intent(in) ::          oroheight      !height of surface above reference areoid (m)
      real(pm_reel), intent(in) ::          areoid         ! reference areoid (m)
      real(pm_reel), intent(in) ::          ps             ! surface pressure (Pa)
      real(pm_reel), intent(in) ::          sigma(dimlevs) ! sigma levels
      real(pm_reel), intent(in) ::          R_gcm(dimlevs) ! R at GCM levels
      real(pm_reel), intent(in) ::          temp_gcm(dimlevs) ! temperature at GCM levels

!----------------
!     Sorties
      integer, intent(out) ::       levhi          !database level upper bound
      integer, intent(out) ::       levlow         !database level lower bound
      real(pm_reel), intent(out) ::          levweight(2)   !level weight for interpolation
!     (1) for linear in height (2) for linear in pressure
      real(pm_reel), intent(out) ::          pratio         !p/p(top, bottom) for extrapolation,
                                   ! 1 if in range
      integer, intent(out) ::       ier            !error flag (0=OK, not 0 =NOK)

      real(pm_reel), intent(out) :: zradius     ! distance to center of planet
      real(pm_reel), intent(out) :: zareoid     ! height above areoid
      real(pm_reel), intent(out) :: zsurface    ! height above surface
      real(pm_reel), intent(out) :: zpressure   ! pressure
      real(pm_reel), intent(out) :: zmradius    ! altitude above mean radius of planet
      real(pm_reel), intent(out) :: sheight(dimlevs) ! altitude above surface of sigma levels

!----------------
!     Variables locales
      real(pm_reel) :: marsradius ! mean Mars radius (m)
      parameter (marsradius=3.396E6_pm_reel)
      real(pm_reel) ::       height         !height of point above areoid (m)
      real(pm_reel) ::       absheight      !height of point above surface (m)
      integer ::    l
      real(pm_reel) ::       zsig
      
!     Gravity on mean areoid
!     The areoid is defined as a surface of constant gravitational plus
!     rotational potential. The inertial rotation rate of Mars is assumed
!     to be 0.70882187E-4 rad/s. This potential is the mean value at the
!     equator at a radius of 3396.000 km, namely 12652804.7 m^2/s^2,
!     calculated from Goddard Mars Gravity Model mgm1025
!     [LEMOINEETAL2001] evaluated to degree and order 50.
      real(pm_reel) ::       g, g0, a0
      parameter (a0=3396.E3_pm_reel,g0=3.7257964_pm_reel)

      real(pm_reel) ::       Tmean          ! "mean" temperature of a layer
      real(pm_reel) ::       Rogct(dimlevs) 

      ier=0


!     Calculate altitude above the surface of each model layer: sheight(l)
!     integrate hydrostatic equation
!     Rogct is the R/g variable depending on dimlevs 
      g = g0*(a0/(a0+oroheight))**2
      Rogct(1) = R_gcm(1)/g          !Rnew(1)/g
      Tmean = temp_gcm(1)            !t(1)
      sheight(1)= - Rogct(1)*temp_gcm(1)*log(sigma(1))

      do l=2, dimlevs
        if (temp_gcm(l).ne.temp_gcm(l-1)) then
         Tmean = (temp_gcm(l)-temp_gcm(l-1)) / &
                      log(temp_gcm(l)/temp_gcm(l-1))
         ! NB: double precision must be used here in case
         ! temp_gcm(l) and temp_gcm(l-1) are almost equal  
        else
         Tmean = temp_gcm(l)
        end if
        g = g0*(a0/(a0+oroheight+sheight(l-1)))**2
        Rogct(l) = R_gcm(l)/g
        sheight(l) = sheight(l-1) - Rogct(l)*Tmean* &
                     log(sigma(l)/sigma(l-1))
      end do

!ccccccccccccccccccccccccccccccccccccccccccccccc     
!     height calculation      
      if (zkey.eq.1) then ! input xz is distance to center of planet
              zradius=xz
      elseif (zkey.eq.2) then ! input xz is height above areoid
              zareoid=xz
      elseif (zkey.eq.3) then ! input xz is altitude above surface
            zsurface=xz
      elseif (zkey.eq.4) then ! input xz is atmospheric pressure
          zpressure=xz
!         compute zsurface
          if(zpressure.gt.ps) then
            if (output_messages) then
              write(out,*)'GETSI Error: underground object '
            endif
            ier=17
            goto 9999
          end if
	  
          zsig = zpressure/ps
          if (zsig.gt. sigma(1)) then
             zsurface= -log(zsig)*R_gcm(1)*temp_gcm(1)/g0
          else if (zsig.lt.sigma(dimlevs) ) then
             g = g0*(a0/(a0+sheight(dimlevs)))**2
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
      elseif (zkey.eq.5) then ! input xz is altitude above mean Mars radius
        zmradius=xz
      endif      
      
! absheight = zsurface for the first call of getsi       
! absheight = xz for call to getsi from  grwpb      
      if (zkey.eq.-5) then ! call from grwpb
        absheight=xz
      else
        ! compute required zradius,zareoid,zsurface and/or zmradius
        if (zkey.eq.1) then ! zradius is known
          zareoid=zradius-areoid
          zsurface=zareoid-oroheight
          zmradius=zradius-marsradius
        elseif (zkey.eq.2) then ! zareoid is known
          zradius=zareoid+areoid
          zsurface=zareoid-oroheight
          zmradius=(areoid-marsradius)+zareoid
        elseif ((zkey.eq.3).or.(zkey.eq.4)) then ! zsurface is known
          zareoid=zsurface+oroheight
          zradius=zareoid+areoid
          zmradius=(areoid-marsradius)+zareoid
        elseif (zkey.eq.5) then ! zmradius is known
          zradius=marsradius+zmradius
          zareoid=zmradius-(areoid-marsradius)
          zsurface=zareoid-oroheight
        endif
        
        height=zareoid              ! height above areoid
        absheight=height-oroheight  ! height above surface
      endif

!cccccccccccccccccccccccccccccccccccccccccccccccccc      
      
!     find levhi, levlow and levweight
!     compute g (at height=zareoid)
      g = g0*(a0/(a0+oroheight+absheight))**2
      if (absheight.lt.sheight(1)) then
!       below the lowest layer
        levhi=1
        levlow=1
        levweight(1)=1._pm_reel
        levweight(2)=0._pm_reel
        pratio=exp((sheight(1)-absheight)*g/(R_gcm(1)*temp_gcm(1)))
      elseif (absheight.ge.sheight(dimlevs)) then
!       above the top layer
        levhi=dimlevs
        levlow=dimlevs
        levweight(1)=0._pm_reel
        levweight(2)=1._pm_reel
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
          levweight(2)=(1._pm_reel-(sigma(levhi)/sigma(levlow))**levweight(1)) &
                      /(1._pm_reel-(sigma(levhi)/sigma(levlow)))
          pratio=1._pm_reel
        endif
       enddo
      endif

!     Compute pressure
      zpressure=ps*(sigma(levlow)+(sigma(levhi)-sigma(levlow)) &
                                    *levweight(2))*pratio

      return

!     Errror handling
 9999 levhi=0
      levlow=0
      levweight(1)=0._pm_reel
      levweight(2)=0._pm_reel
      pratio=0._pm_reel

      return
      end subroutine getsi

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine grid4(lon,lat,dlon,dlat,t,u)

!     Given longitude=lon and latitude=lat find the nearest 4 horizontal
!     gridpoints in the database and bilinear interpolation weights.
!
!     Grid points are arranged as follows:
!              4 3
!              1 2 
      
      implicit none

!--------------      
!     Entrées
      real(pm_reel), intent(in) :: lon      !east longitude of point
      real(pm_reel), intent(in) ::lat      !latitude of point

!--------------   
!     Sorties
      integer, intent(out) :: dlon(4)  !index, along longitudes, of database points
      integer, intent(out) :: dlat(4)  !index, along latitudes, of database points
      real(pm_reel), intent(out) :: t        ! weight (normalized longitudinal distance to point 1)
      real(pm_reel), intent(out) :: u        ! weight (normalized latitudinal distance to point 1)

!-----------------------   
!     Variables locales
      real(pm_reel) ::    alon,alat
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
         u=0._pm_reel
      elseif (lat.ge.latmax) then
         dlat(4)=dimlat
         dlat(3)=dimlat
         dlat(1)=dimlat
         dlat(2)=dimlat
         u=0._pm_reel
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
                       ps,sigma,utime,name,ier,itimint,wl,wh, &
                       R_gcm,temp_gcm,sheight,p_pgcm,ps_psgcm)

!     Small scale gravity wave perturbation model.
!     Computes perturbation on u, v, theta or rho, with wave phase factor.
!
!     Last updated Feb. 2008: improved behaviour when near surface (in order
!                             to get rid of 'jumps' in perturbations when run
!                             at very fine resolution)
      implicit none

      
!----------------
!     Entrées
      real(pm_reel), intent(in) :: dev     !phase of perturbation (rand number between 0 and 1)
      real(pm_reel), intent(in) :: lamda   !vertical wavelength of g.w. (m)
      real(pm_reel), intent(in) :: lon     !east longitude of point
      real(pm_reel), intent(in) :: lat     !latitude of point
      real(pm_reel), intent(in) :: absheight  !height (above surface) of point
      real(pm_reel), intent(in) :: rho     !density at height=height
      real(pm_reel), intent(in) :: u       !zonal wind at height=height
      real(pm_reel), intent(in) :: v       !meridional wind at height=height
      real(pm_reel), intent(in) :: ps      ! surface pressure
      real(pm_reel), intent(in) :: sigma(dimlevs) ! sigma levels 
      real(pm_reel), intent(in) :: utime   !Universal time (0. to 24. hrs) = local time at lon=0
      integer, intent(in) ::       itimint !seasonal interpolation flag
      real(pm_reel), intent(in) :: wl,wh   !seasonal interpolation weights
      real(pm_reel), intent(in) :: R_gcm(dimlevs) ! R at GCM levels
      real(pm_reel), intent(in) :: temp_gcm(dimlevs) ! temperature at GCM levels
      real(pm_reel), intent(in) :: p_pgcm(dimlevs) ! high res to GCM pressure ratios
      real(pm_reel), intent(in) :: ps_psgcm ! High res to GCM surface pressure ratio 
      character(len=16), intent(in) ::  name    !name of variable to perturb

!----------------
!     Sorties
      real(pm_reel), intent(out) :: sheight(dimlevs) ! altitude of GCM sigma levels
      integer, intent(out) ::       ier     !error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) :: pert    !perturbation

!----------------
!     Variables locales
      ! zareoid,zpressure,zradius,zsurface only local
      ! we do not care of those variables here
      real(pm_reel) :: xz,zareoid,zpressure,zradius,zsurface,zmradius
      integer ::       zkey
      integer ::       ierr
      real(pm_reel) :: a, aprime
      real(pm_reel) :: rho0,rho1
      real(pm_reel) :: u0,u1
      real(pm_reel) :: v0,v1
      real(pm_reel) :: tmpheight
      integer ::       levlow,levhi
      real(pm_reel) :: levweight(2)
      real(pm_reel) :: pratio
      real(pm_reel) :: dz,dz_max
      real(pm_reel) :: sig
      real(pm_reel) :: umag
      real(pm_reel) :: hmax    !for perturbations above hmax, use amplitude at hmax (m)
      parameter   (hmax=100.E3_pm_reel) 
      real(pm_reel) ::         usat    !for windspeeds below usat, wave amplitude saturates
      Parameter   (usat=0.5_pm_reel)
      real(pm_reel) ::         dalr    !dry adiabatic lapse rate (K/m)
      parameter   (dalr=4.5E-3_pm_reel)
      real(pm_reel) ::         pi 
      parameter   (pi=3.14159265359_pm_reel)
      character*16 tmpname

!      write(out,*)""
!      write(out,*) "Entering grwpb, name:",name
!      write(out,*) "                dev=",dev
!      write(out,*) "                lamda=",lamda
!      write(out,*) "                absheight=",absheight
!      write(out,*) "                rho=",rho
!      write(out,*) "                u=",u
!      write(out,*) "                v=",v

      ier=0

!     1. get rho0, u0, v0
!        use mean data from level 1, not height=0.0
      levlow=1
      levhi=1
      levweight(1)=1._pm_reel
      levweight(2)=1._pm_reel
      pratio=1.0_pm_reel
      tmpname='u'
      call var3d(u0,lon,lat,sheight(1),levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                 itimint,wl,wh)
      tmpname='v'
      call var3d(v0,lon,lat,sheight(1),levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                 itimint,wl,wh)
      tmpname='rho'
      call var3d(rho0,lon,lat,sheight(1),levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                 itimint,wl,wh)

!      write(out,*) "grwpb: u0=",u0," v0=",v0," rho0=",rho0 
      
!     get sub-grid scale variance
      tmpname='substd'
      call var2d(sig,lon,lat,1.0_pm_reel,tmpname,ierr,itimint,wl,wh,1.0_pm_reel)

!     2. Compute delta z, the vertical displacement
!        (note that here we assume N=N0, i.e. no change in Brunt Vaisala 
!          frequency between altitude 'absheight' and 1st layer)
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
         zkey=-5
         call getsi(xz,zkey,0.0_pm_reel,0.0_pm_reel,ps,sigma, &
                    levhi,levlow,levweight,pratio,ierr, &
                    R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure, &
                    zmradius,sheight)
         tmpname='rho'
         call var3d(rho1,lon,lat,absheight,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                    itimint,wl,wh)
         tmpname='u'
         call var3d(u1,lon,lat,absheight,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                    itimint,wl,wh)
         tmpname='v'
         call var3d(v1,lon,lat,absheight,levhi,levlow,levweight, &
                    pratio,sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                    itimint,wl,wh)
         umag=sqrt(u1**2+v1**2)
!        the wave amplitude becomes large as the wind speed becomes small
!        and the wave should saturate
         if (umag.lt.usat) umag=usat
          dz=sig*sqrt((rho0*sqrt(u0**2+v0**2))/(rho1*umag))
      end if

!     apply simple test for saturation (require theta_z > 0)
!     and compute wave
      dz_max=lamda/(2._pm_reel*pi)
      if (dz.gt.dz_max) then
         dz=dz_max
      endif
      dz=dz*sin(2._pm_reel*pi*dev+(2._pm_reel*pi*absheight)/lamda)


!     3. Find perturbation from change delta z in mean profile
!     (more accurate than taking first derivative).
!     Using an oroheight of 0km in getsi here implies an insignificant
!     error in the calculated value of g used to find the distance to
!     the perturbation, very much smaller than other assumptions made
!     (interpolation, adiabatic, constant g, etc.).

!     don't allow dz to take us below the surface
!      if ((tmpheight+dz).le.0.) then
      if ((tmpheight+dz).le.0) then
! old version rescale underground to near-surface
!         dz = -0.99*tmpheight
! new version: mirror underground depths to above surface heights
        dz=-tmpheight-(tmpheight+dz)
      endif

! But, avoid being taken too close to the surface
      if ((tmpheight+dz).le.1000._pm_reel) then
! map dz so that (tmpheight+dz) spans [100:1000] instead of [0:1000]
        dz=(((1000.0_pm_reel-100.0_pm_reel)/1000.0_pm_reel)*(tmpheight+dz)+100.0_pm_reel)-tmpheight
      endif

!      write(*,*) "grwpb: finally dz=",dz," tmpheight+dz=",tmpheight+dz

      tmpname = name
      if (name.eq.'rho') then
        ! density perturbation is computed from temperature perturbation
        ! (see below)
        tmpname= 'temp'
      endif
      
      ! get 'a': value of variable at altitude 'tmpheight'
      xz=tmpheight
      zkey=-5
      call getsi(xz,zkey,0.0_pm_reel,0.0_pm_reel,ps,sigma, &
                 levhi,levlow,levweight,pratio,ierr, &
                 R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure, &
                 zmradius,sheight)
      call var3d(a,lon,lat,xz,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                 itimint,wl,wh)
      
      ! get 'aprime': value of variable at altitude 'tmpheight+dz'
      xz=tmpheight+dz
      call getsi(xz,zkey,0.0_pm_reel,0.0_pm_reel,ps,sigma, &
                 levhi,levlow,levweight,pratio,ierr, &
                 R_gcm,temp_gcm,zareoid,zradius,zsurface,zpressure, &
                 zmradius,sheight)
      call var3d(aprime,lon,lat,xz,levhi,levlow,levweight,pratio, &
                 sheight,p_pgcm,ps_psgcm,utime,tmpname,ierr, &
                 itimint,wl,wh)
      
      ! perturbation is just the difference between values at altitudes
      ! tmpheight and (tmpheight+dz) (with corrections for temperature
      ! and density)
      pert = aprime - a


!     correction for perturbation to potential temperature
      if (tmpname.eq.'temp') then
        pert = pert + dalr*dz
      endif

      ! reduce 'pert' if below 2000.0m (so that it smoothly goes to zero
      ! at surface)
      if (absheight.le.2000.0_pm_reel) then
        pert=pert*sin((absheight/2000.0_pm_reel)*(pi/2._pm_reel))**2
      endif

!     correction for perturbation on density
!     pert(rho) = -rho*pert(T)/(T+pert(T))
      if (name.eq.'rho') then
         pert = -rho*pert/(a+pert)
      end if 

      ! if near the surface then quench perturbations

!      write(out,*) "grwpb: pert=",pert
      return
      end subroutine grwpb

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadvar(nf,nf_up,nf2,nf_up2,typevar, &
                         aps,bps,pseudoalt,ier)

!     load arrays corresponding to the variable typevar
      implicit none
      
      

! variable typevar to load :
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

!---------------
!     Entrées
      integer, intent(in) :: nf,nf_up,nf2,nf_up2 !NetCDF file IDs
      character(len=4), intent(in)  :: typevar   ! variable 'type'

!---------------
!     Sorties      
      real(pm_reel), intent(out):: aps,bps,pseudoalt
      dimension aps(dimlevs)        ! hybrid coordinate
      dimension bps(dimlevs)        ! hybrid coordinate
      dimension pseudoalt(dimlevs)  ! pseudo altitude
      integer , intent(out) :: ier  ! error flag (0=OK, 1=NOK)

!---------------
!     Variables locales   
      character(len=16) :: name      ! variable name
      integer ::      ierr
      integer ::      varid ! NetCDF variable ID
      integer ::      l,k,iloop,jloop
      ! Valeurs lues par NETCDF en simple precision
      real ::         aps_low(dimlevs),bps_low(dimlevs) ! hybrid coord.
      real ::         aps_up(dimlevs) ! hybrid coord. upper atm.
      real ::         temp2d(dimlon,dimlat) ! temporary array
      character*50 varname2d(nbvar2d) 
      character*50 varname3d(nbvar3d)
      character*50 sd2d(nbsd2d) 
      character*50 sd3d(nbsd3d+1) 


      ! Initialisation des champs qui seront remplis par netcdf
      do iloop = 1, dimlevs
         aps_low(iloop) = 0.
         bps_low(iloop) = 0.
         aps_up(iloop) = 0.
      end do
      do jloop = 1,dimlat
         do iloop = 1, dimlon
            temp2d(iloop, jloop) = 0.
         end do
      end do
      varid = 0
      
      ! Init des differentes chaines de caracteres
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
      sd3d(6)="pressure"

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
          ! conversion  en double precision
          aps(l)=real(aps_low(l), kind=pm_reel)
          bps(l)=real(bps_low(l), kind=pm_reel)
        enddo
        do l=low+1,dimlevs
          ! conversion  en double precision
          aps(l)=real(aps_up(l-low), kind=pm_reel)
          bps(l)=0._pm_reel
        enddo

!       pseudoalt:
        do l=1,dimlevs
          pseudoalt(l)=-10._pm_reel*log(aps(l)/610._pm_reel+bps(l))
        enddo
             
      elseif (typevar.eq.'orog') then

        name='orography'
        ierr = NF_INQ_VARID(nf,name,varid) 
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon ! taborog() is a common in constants_mcd.inc
           taborog(iloop,jloop,1)=real(temp2d(iloop,dimlat+1-jloop), kind=pm_reel) 
          enddo
        enddo
        
        name='areoid'
        ierr = NF_INQ_VARID(nf,name,varid) 
        if (ierr.ne.nf_noerr) goto 9999
        ierr = NF_GET_VAR_REAL(nf,varid,temp2d)
        if (ierr.ne.nf_noerr) goto 9999
        do jloop=1,dimlat
          do iloop=1,dimlon ! tabareo() is a common in constants_mcd.inc
           tabareo(iloop,jloop,1)=real(temp2d(iloop,dimlat+1-jloop) , kind=pm_reel) 
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
           tabsubstd(iloop,jloop,1)=real(temp2d(iloop,dimlat+1-jloop) , kind=pm_reel) 
          enddo
        enddo

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

      elseif (typevar.eq.'arms') then
! Altitude-wise RMS
        do k=nbsdlow-nbcomsd+1,nbsdlow+1 ! +1 because we also want pressure
          call getsd_3d(nf,typevar,k,"arms"//sd3d(k),low,1)
          call getsd_3d(nf2,typevar,k,"arms"//sd3d(k),low,2)
          call getsd_3d(nf_up,typevar,k,"arms"//sd3d(k),up,1)
          call getsd_3d(nf_up2,typevar,k,"arms"//sd3d(k),up,2)
!          write(out,*)'k=',k,'vararms3d(2,48,2,k)=',vararms3d(2,48,2,k)
        enddo

      endif ! of if (typevar.eq.'mean') elseif ...

      return

!     Error handling
 9999 ier=16
      if (output_messages) then
        write(out,*)"LOADVAR Error : impossible to load ", &
                  name," from ",typevar, "value file"
      endif
      return

      end subroutine  loadvar

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine loadeof(nf,ier)

!     load EOF arrays (which are commons in 'constants_mcd.inc')
      implicit none

      
!-----------------------
!     Entrées
      integer, intent(in) :: nf !NetCDF file ID (file must have been previously opened)
!-----------------------
!     Sorties
      integer, intent(out) :: ier !error flag (0=OK, 1=NOK)

!-----------------------
!     Variables locales
      character(len=16) :: name  ! name of variable to read
      integer :: ierr            ! NetCDF routine (returned) status
      integer :: varid           ! NeCDF indetifier
      integer :: ii, jj, kk, ll  ! Indices de boucles de parcours
      ! Tableaux simples precision tampon à remplir par NETCDF
      real :: tab_temp1d(dimlateo)
      real :: tab_temp3d(dimloneo,dimlateo,dimnevecs)
      real :: tab_temp3d_pc(dimlateo,dimeoday,dimnevecs)
      real :: tab_temp4D(dimloneo,dimlateo,dimlevs,dimnevecs)
 
      ! Init du champ remplit par NETCDF
      varid = 0

      ! Norm verctor for u
      name='normu'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp1d(:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp1d)
      if (ierr.ne.nf_noerr) goto 9999
      ! Conversion en double précision
      do ii = 1,dimlateo
         tabeonormu(ii)=real(tab_temp1d(ii), kind=pm_reel)
      end do

      ! Norm vector for v
      name='normv'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp1d(:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp1d)
      if (ierr.ne.nf_noerr) goto 9999
      ! Conversion en double précision
      do ii = 1,dimlateo
         tabeonormv(ii)=real(tab_temp1d(ii), kind=pm_reel)
      end do

      ! Norm vector for temperature
      name='normt'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp1d(:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp1d)
      if (ierr.ne.nf_noerr) goto 9999
      ! Conversion en double précision
      do ii = 1,dimlateo
         tabeonormt(ii)=real(tab_temp1d(ii), kind=pm_reel)
      end do

      ! Norm vector for surface pressure
      name='normp'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp1d(:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp1d)
      if (ierr.ne.nf_noerr) goto 9999
      ! Conversion en double précision
      do ii = 1,dimlateo
         tabeonormp(ii)=real(tab_temp1d(ii), kind=pm_reel)
      end do
       
      ! zonal velocity
      name='u'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp4d(:,:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp4d)
      if (ierr.ne.nf_noerr) goto 9999
      do ll = 1,dimnevecs
         do kk = 1,dimlevs
            do jj = 1,dimlateo
               do ii = 1,dimloneo
                  tabeou(ii, jj, kk, ll)=real(tab_temp4d(ii, jj, kk, ll), kind=pm_reel)
               end do
            end do
         end do
      end do
 
      ! meridional velocity
      name='v'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp4d(:,:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp4d)
      if (ierr.ne.nf_noerr) goto 9999
      do ll = 1,dimnevecs
         do kk = 1,dimlevs
            do jj = 1,dimlateo
               do ii = 1,dimloneo
                  tabeov(ii, jj, kk, ll)=real(tab_temp4d(ii, jj, kk, ll), kind=pm_reel)
               end do
            end do
         end do
      end do

      ! atmospheric tempeerature
      name='temp' 
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp4d(:,:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp4d)
      if (ierr.ne.nf_noerr) goto 9999
      do ll = 1,dimnevecs
         do kk = 1,dimlevs
            do jj = 1,dimlateo
               do ii = 1,dimloneo
                  tabeot(ii, jj, kk, ll)=real(tab_temp4d(ii, jj, kk, ll), kind=pm_reel)
               end do
            end do
         end do
      end do

      ! surface pressure
      name='ps'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp3d(:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp3d)
      if (ierr.ne.nf_noerr) goto 9999
      do kk = 1,dimnevecs
         do jj = 1,dimlateo
            do ii = 1,dimloneo
               tabeops(ii, jj, kk)=real(tab_temp3d(ii, jj, kk), kind=pm_reel)
            end do
         end do
      end do

      ! principal components
      name='pc'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp3d_pc(:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp3d_pc)
      if (ierr.ne.nf_noerr) goto 9999
      do kk = 1,dimnevecs
         do jj = 1,dimeoday
            do ii = 1,dimlateo
               tabpc(ii, jj, kk)=real(tab_temp3d_pc(ii, jj, kk), kind=pm_reel)
            end do
         end do
      end do

      ! smoothed principal components
      name='pcsmth'
      ierr = NF_INQ_VARID(nf,name,varid)
      if (ierr.ne.nf_noerr) goto 9999
      tab_temp3d_pc(:,:,:)=0.
      ierr = NF_GET_VAR_REAL(nf,varid,tab_temp3d_pc)
      if (ierr.ne.nf_noerr) goto 9999
      do kk = 1,dimnevecs
         do jj = 1,dimeoday
            do ii = 1,dimlateo
               tabpcsmth(ii, jj, kk)=real(tab_temp3d_pc(ii, jj, kk), kind=pm_reel)
            end do
         end do
      end do

      return 

      !     Error handling
9999  ier=16
      if (output_messages) then
         write(out,*)"LOADEOF Error : failed to load ", &
              name," from eof file"
      endif
      return

    end subroutine  loadeof


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ltime(lon,jdate,ls,localtime)

!     Compute local true solar time at longitude=lon (in deg. east)
!     at julian date jdate, also given in Ls.

      implicit none

!----------------------
!     Entrées
      real(pm_reel) :: lon        !east longitude (in deg.)
      real(pm_reel) ::  jdate    !julian date
      real(pm_reel) :: ls       !aerocentric longitude (in deg.)

!----------------------
!     Sorties
      real(pm_reel) :: localtime !local true solar time (in martian hours)

!----------------------
!     Variables locales
      real(pm_reel) ::   LMT_0,E_T,LTST_0
! marsday: number of seconds in a sol (matian day)
      real(pm_reel) ::      marsday
      parameter (marsday=88775.245_pm_reel)
!  earthday: number of seconds in a day: 24*60*60
      real(pm_reel) ::      earthday
      parameter (earthday=86400._pm_reel)
! julian date of reference date: 01-01-1976 at 00:00:00
      real(pm_reel) ::      jdate_ref
      parameter (jdate_ref=2442778.5_pm_reel) 
! LMT_ref: mean solar time at 0 deg. longitude at date jdate_ref
      real(pm_reel) ::     LMT_ref ! in martian hours
      parameter (LMT_ref=16.1725_pm_reel) ! 01-01-1976 at 00:00:00
! orbital eccentricity
      real(pm_reel) ::     eccentricity
      parameter (eccentricity=0.0934_pm_reel)
! Ls of perihelion (in degrees)
      real(pm_reel) ::     Ls_peri
      parameter (Ls_peri=250.99_pm_reel)
! obliquity of equator to orbit (in deg.)
      real(pm_reel) ::     obliquity
      parameter (obliquity=25.1919_pm_reel)
      real(pm_reel) ::     pi,degtorad
      parameter (pi=3.14159265358979_pm_reel)
      parameter (degtorad=pi/180.0_pm_reel)

! compute LMT, local mean time, (in martian hours) at longitude 0
      LMT_0=LMT_ref+24.0_pm_reel*(jdate-jdate_ref)*(earthday/marsday)
      do while(LMT_0.le.0._pm_reel) ! in case jdate<jdate_ref
        LMT_0=LMT_0+24.0_pm_reel
      enddo
      LMT_0=dmod(LMT_0,24.0_pm_reel)

! compute equation of time (see eq. 10.17, p.419 in
! "satellites orbits and missions", by M. Capderou),
! with a 24/(2*pi) factor to express it in martian hours
      E_T=(2._pm_reel*eccentricity*sin((ls-Ls_peri)*degtorad) &
           -dtan(obliquity*degtorad/2.0_pm_reel) &
            *dtan(obliquity*degtorad/2.0_pm_reel)*sin(2.0_pm_reel*ls*degtorad)) &
             *24.0_pm_reel/(2.0_pm_reel*pi)


! compute true solar time at longitude 0
      LTST_0=LMT_0-E_T


! compute local true solar time at longitude lon
      localtime=LTST_0 + (lon/15.0_pm_reel)

      localtime=mod(24.0_pm_reel+localtime,24.0_pm_reel)

      return
      end subroutine  mars_ltime

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mars_ptime(lon,lt,ut)

!     Convert local true solar time, lt (0..24) at east longitude (in deg.) 
!     into "universal time"  ut (0..24), the local true solar time at lon=0

      implicit none
      
!     Entrées
      real(pm_reel), intent(in) :: lon    !longitude east (in degrees)
      real(pm_reel), intent(in) :: lt     !local time (in martian hours)
      
!     Sorties
      real(pm_reel), intent(out) :: ut    !universal time  (local true solar time at lon=0)


      ut=mod(24.0_pm_reel+lt-lon/15.0_pm_reel,24.0_pm_reel)

      return
      end subroutine mars_ptime

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine mcd_time(ut,itime,w)
      
!     From universal time ut (0..24 hrs) , find the 
!     2 nearest timestep in the database (1 - 12) 
!     and give the linear interpolation weight of itime(1)
! 
!     Note that it is midnight universal time (and thus at longitude=0.0)
!     at itime = 12

      implicit none

!     Entrées
      real(pm_reel), intent(in) :: ut       !universal time (0. to 24. hrs) =local time at lon=0

!     outputs
      integer, intent(out) :: itime(2)  ! 2 nearest timestep in database 
      real(pm_reel), intent(out) :: w            ! linear interpolation weight of itime(1)
                         ! ie: w=1 if ut=itime(1) and w=0 if ut=itime(2)

      itime(1) = int(ut/2._pm_reel)
      if (itime(1).eq.0) itime(1) = 12
      itime(2) = itime(1) +1
      if (itime(2).ge.13._pm_reel) itime(2) = itime(2) -12

      w = 1._pm_reel - ut/2._pm_reel + int(ut/2._pm_reel)
      
      
      return
      end subroutine mcd_time

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine opend(unet,unetm,unetsd,unetm_up,unetsd_up,ueof, &
                  num,dust,dset,ier)

!     Open the appropriate NETCDF file corresponding to Ls, dust scenario  
!     and data set.
!     
!     April 2008. Added search for 'datafile.gz' in same directory in case
!                'datafile' is not found. EM

      implicit none

      
      
!----------------
!     Entrées
! NETCDF channels number      
      integer, intent(inout) ::  unet       !for mountain fields
      integer, intent(inout) ::  unetm      !for mean fields
      integer, intent(inout) ::  unetsd     !for STD and RMS deviation fields
      integer, intent(inout) ::  unetm_up   !for mean fields in thermosphere 
      integer, intent(inout) ::  unetsd_up  !for STD and RMS fields in thermosphere 
      integer, intent(inout) ::  ueof       !for EOF fields

      integer, intent(in) ::       num        !season num
      integer, intent(in) ::       dust       !Dust senario
      character*(*), intent(in) :: dset       !Dataset

!----------------
!     Sorties
      integer, intent(out) :: ier        !Error flag (0=OK, 1=NOK)

!----------------
!     Variables locales
      integer :: ierr     ! return code from NetCDF functions
      character(len=255) ::   datfile  ! full datafile name
      character(len=4) ::     scen     ! dust scenario ('MY24','cold','warm' or 'strm')
      character(len=3) ::     solar    ! EUV scenario ('min' or 'ave' or 'max')
      character(len=2) ::     saison   ! month # (from '01' to '12')
      character(len=2) ::     typ      ! type of datafile ('me' or 'sd' or 'eo')
      character(len=258) ::   gzfile   ! 'datfile'//'.gz'
      
!     Init du code de retour
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
           write(out,*) 'pb in opend with dust= ', dust
         endif
         stop
      endif

!     season number
      write(saison,'(i2.2,1x)') num 

!     mean file
      typ='me'
      datfile=dset//scen//'_'//saison//'_'//typ//'.nc'
!      if (output_messages) then
!        write(out,*) "Opening "
!        write(out,*) datfile(1:140)
!      endif
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm)
      if (ierr.ne.NF_NOERR) then ! failed to open file
        ! check for a '.gz' version of the datafile
        gzfile=dset//scen//'_'//saison//'_'//typ//'.nc.gz'
        open(40,file=gzfile,iostat=ierr,status="old")
        if (ierr.ne.NF_NOERR) then ! failed to open file.gz
          goto 9999
        else ! there is a file.gz around
          goto 8888
        endif
      endif

!     standard deviation file
      typ='sd'
      datfile=dset//scen//'_'//saison//'_'//typ//'.nc'
!      if (output_messages) then
!        write(out,*) "Opening "
!        write(out,*) datfile(1:140)
!      endif
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd)
      if (ierr.ne.NF_NOERR) then ! failed to open file
        ! check for a '.gz' version of the datafile
        gzfile=dset//scen//'_'//saison//'_'//typ//'.nc.gz'
        open(40,file=gzfile,iostat=ierr,status="old")
        if (ierr.ne.NF_NOERR) then ! failed to open file.gz
          goto 9999
        else ! there is a file.gz around
          goto 8888
        endif
      endif
      
!     thermo Mean file
      typ='me'
      datfile=dset//scen//'_'//saison// &
             '_thermo_'//solar//'_'//typ//'.nc'
!      if (output_messages) then
!        write(out,*) "Opening "
!        write(out,*) datfile(1:140)
!      endif
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetm_up)
      if (ierr.ne.NF_NOERR) then ! failed to open file
        ! check for a '.gz' version of the datafile
        gzfile=dset//scen//'_'//saison// &
              '_thermo_'//solar//'_'//typ//'.nc.gz'
        open(40,file=gzfile,iostat=ierr,status="old")
        if (ierr.ne.NF_NOERR) then ! failed to open file.gz
          goto 9999
        else ! there is a file.gz around
          goto 8888
        endif
      endif

!     thermo standard deviation file
      typ='sd'
      datfile=dset//scen//'_'//saison// &
             '_thermo_'//solar//'_'//typ//'.nc'
!      if (output_messages) then
!        write(out,*) "Opening "
!        write(out,*) datfile(1:140)
!      endif
      ierr=NF_OPEN(datfile,NF_NOWRITE,unetsd_up)
      if (ierr.ne.NF_NOERR) then ! failed to open file
        ! check for a '.gz' version of the datafile
        gzfile=dset//scen//'_'//saison// &
               '_thermo_'//solar//'_'//typ//'.nc.gz'
        open(40,file=gzfile,iostat=ierr,status="old")
        if (ierr.ne.NF_NOERR) then ! failed to open file.gz
          goto 9999
        else ! there is a file.gz around
          goto 8888
        endif
      endif
      
!     eof file
      typ='eo'
      datfile=dset//scen//'_all_'//solar//'_eo.nc'
!      if (output_messages) then
!        write(out,*) "Opening "
!        write(out,*) datfile(1:140)
!      endif
      ierr=NF_OPEN(datfile,NF_NOWRITE,ueof)
      if (ierr.ne.NF_NOERR) then ! failed to open file
        ! check for a '.gz' version of the datafile
        gzfile=dset//scen//'_all_'//solar//'_eo.nc.gz'
        open(40,file=gzfile,iostat=ierr,status="old")
        if (ierr.ne.NF_NOERR) then ! failed to open file.gz
          goto 9999
        else ! there is a file.gz around
          goto 8888
        endif
      endif

!     mountain file
      datfile=dset//'mountain.nc'
      ierr=NF_OPEN(datfile,NF_NOWRITE,unet)
      if (ierr.ne.NF_NOERR) goto 9999

      return

!     Error handling

 8888 ier=21    ! could not open file but found file.gz instead
      if (output_messages) then
        write(out,*)"Error in opend: cannot open file ",datfile
        write(out,*)"  but found file ",gzfile
        write(out,*)"  which should be uncompressed using third-party"// &
                    " software (e.g. gunzip on Unix or Winzip on "// &
                    " Windows) to produce sought file."
      endif
      return
      
 9999 ier=15    ! could not open file (and no file.gz around)
      if (output_messages) then
        write(out,*)"Error in opend: cannot open file ",datfile
      endif
      return

      end subroutine opend

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine orbit(date,ls,marsau,outmodelday)

!     Given Julian date,compute Ls, sun-Mars distance
!     in AU and model day

      implicit none

!-----------------------
!     Entrée
      real(pm_reel), intent(in) ::     date   !Julian date

!-----------------------
!     Sorties
      real(pm_reel), intent(out) ::  ls     !Ls: Aerocentric longitude (in deg.)
      real(pm_reel), intent(out) ::  marsau !Sun-Mars distance (in AU)
      real(pm_reel), intent(out) ::  outmodelday

!-----------------------
!     Variables locales
      real(pm_reel) :: modelday
! marsday: number of seconds in a sol (matian day)
      real(pm_reel) :: marsday
      parameter (marsday=88775.245_pm_reel)
!  earthday: number of seconds in a day: 24*60*60
      real(pm_reel) ::  earthday
      parameter (earthday=86400._pm_reel)
      real(pm_reel) ::  marsyear
      parameter (marsyear=668.6_pm_reel) ! number of sols in a martian year
! julian date for 19-12-1975 at 4:00:00, at which Ls=0.0
      real(pm_reel) :: jdate_ref
      parameter (jdate_ref=2442765.667_pm_reel)
      real(pm_reel) :: sma ! semi-major axis of orbit (in AU)
      parameter (sma=1.52368_pm_reel)
      real(pm_reel) :: eccentricity ! orbital eccentricity
      parameter (eccentricity=0.09340_pm_reel)
      real(pm_reel) :: Ls_peri ! Ls of perihelion
      parameter (Ls_peri=250.99_pm_reel)
      real(pm_reel) :: pi,degtorad
      parameter (pi=3.14159265358979_pm_reel)
      parameter (degtorad=pi/180.0_pm_reel) 

      real(pm_reel) :: dummy

!     convert Julian day to model day
      modelday=(date-jdate_ref)*earthday/marsday
       do while(modelday.le.0._pm_reel) !in case date<jdate_ref
         modelday=modelday+marsyear
       enddo
      modelday=dmod(modelday,marsyear)
      dummy=modelday
      outmodelday=modelday

      call sol2ls(dummy,ls)

! compute sun-mars distance (in AU)
      marsau=sma*(1.0_pm_reel-eccentricity*eccentricity) / &
             (1.0_pm_reel+eccentricity*cos((ls-Ls_peri)*degtorad))

      return
      end subroutine orbit

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine profi(a,lon,lat,utime,name,ier,itimint,wl,wh)

!     Retrieve a vertical profile of 3-d variable=name at east longitude=lon,
!     latitude=lat and universal time=utime.
!     horizontal bilinear interpolation is used for all variables exept
!     density, for which the horizontal interpolation is bilinear
!     interpolation of log(rho)
!     Note: see routine loadvar.F about the way data arrays are organized.

      implicit none

!-----------------------
!     Entrées
      real(pm_reel), intent(in) ::              lon     ! east longitude of point
      real(pm_reel), intent(in) ::              lat     ! latitude of point
      real(pm_reel), intent(in) ::              utime   ! Universal time (0. to 24. hrs)=local time at lon=0
      character(len=16), intent(in) :: name    ! Name of variable
      integer, intent(in) ::           itimint ! seasonal interpolation flag (1==yes,0==no)
      real(pm_reel), intent(in) ::              wl,wh   ! weights for seasonal interpolation  

!-----------------------
!     Sorties
      integer, intent(out) :: ier        ! error flag (0=OK, 1=Not OK)
      real(pm_reel), intent(out) ::    a(dimlevs) ! Interpolated profile 

!-----------------------
!     Variables locales
      integer :: i,j,l    ! for loops
      integer :: k,sd,rms,arms ! flags/index of variable
      integer :: itime(2) ! nearest MCD time indexes (1-12)
      integer :: iut      ! temporary variable to store itime()
      integer :: dlon(4)  ! longitude indexes of nearby MCD grid points
      integer :: dlat(4)  ! latitude indexes of nearby MCD grid points
      integer :: idlon ! to store dlon(i) outside i loop to save time (?)
      integer :: idlat ! to store dlat(i) outside i loop to save time (?)
      real(pm_reel) ::    t,u ! weights for horizontal (bilinear) interpolation
      real(pm_reel) ::    w ! weight for time-of-day interpolation
      real(pm_reel) ::    y(dimlevs,4) ! MCD profiles at nearby grid points
      real(pm_reel) ::    x(dimlevs,2) ! horizontally interpolated y() at times itime()
      real(pm_reel) ::    y2(dimlevs,4), x2(dimlevs,2)
      real(pm_reel) ::    a2(dimlevs)

!     flags sd, rms and arms initialized to 999
      sd=999 
      rms=999
      arms=999
      k=0
      ier=0    ! initialize error flag to 0 (== OK)

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
!!!!!!!! Pressure-wise RMS  
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
!!!!!!!! Altitude-wise RMS
      elseif (name.eq.'armstemp') then
        k=1
        arms=1
      elseif (name.eq.'armsu') then
        k=2
        arms=1
      elseif (name.eq.'armsv') then
        k=3
        arms=1
      elseif (name.eq.'armsrho') then
        k=4
        arms=1
      elseif (name.eq.'armsw') then
        k=5
        arms=1
      elseif (name.eq.'armspressure') then
        k=6
        arms=1
      else
!        CASE of an unexpected name
         if (output_messages) then
          write(out,*) 'problem using subroutine profi : the name ',name
          write(out,*) 'is not recognized'
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
              idlon=dlon(i)
              idlat=dlat(i)
               do l=1,dimlevs
                 y(l,i)=var_3d(idlon,idlat,l,iut,k)
               enddo
            enddo
       endif!sd=0
!    RMS variables
       if (rms.eq.1) then
           do i=1,4
             idlon=dlon(i)
             idlat=dlat(i)
              do l=1,dimlevs
                y(l,i)=varrms3d(idlon,idlat,l,k)
              enddo
           enddo
       endif
!    ARMS variables
       if (arms.eq.1) then
           do i=1,4
             idlon=dlon(i)
             idlat=dlat(i)
              do l=1,dimlevs
                y(l,i)=vararms3d(idlon,idlat,l,k)
              enddo
           enddo
       endif

       if ((name.eq.'rho').or.(name.eq.'tsdrho').or. &
           (name.eq.'rmsrho').or.(name.eq.'armsrho').or. &
               (name.eq.'armspressure')) then
!    bilinear interpolation of log(rho)
         do l=1,dimlevs
           x(l,j)=(1._pm_reel-t)*(1._pm_reel-u)*log(y(l,1))+t*(1._pm_reel-u)*log(y(l,2)) &
                 +t*u*log(y(l,3))+(1._pm_reel-t)*u*log(y(l,4))
           x(l,j)=exp(x(l,j))
           
         enddo
       else
!    bilinear interpolation
         do l=1,dimlevs
           x(l,j)=(1._pm_reel-t)*(1._pm_reel-u)*y(l,1)+t*(1._pm_reel-u)*y(l,2) &
                 +t*u*y(l,3)+(1._pm_reel-t)*u*y(l,4)
         enddo
       endif
      enddo! loop on 2 nearest MCD timestep

!    linear interpolation in (hour-of-day) time :
      do l=1,dimlevs
        a(l) = w*x(l,1) + (1._pm_reel-w)*x(l,2)
!      if(k.eq.4) write(out,*)'l,t ',l,a(l)
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
              idlon=dlon(i)
              idlat=dlat(i)
               do l=1,dimlevs
                 y2(l,i)=var_3d2(idlon,idlat,l,iut,k)
               enddo
            enddo
          endif!sd=0
!  RMS variables
          if (rms.eq.1) then
            do i=1,4
              idlon=dlon(i)
              idlat=dlat(i)
               do l=1,dimlevs
                  y2(l,i)=varrms3d2(idlon,idlat,l,k)
               enddo
            enddo
          endif
! ARMS variables
          if (arms.eq.1) then
            do i=1,4
              idlon=dlon(i)
              idlat=dlat(i)
               do l=1,dimlevs
                  y2(l,i)=vararms3d2(idlon,idlat,l,k)
               enddo
            enddo
          endif
!
          if ((name.eq.'rho').or.(name.eq.'tsdrho').or. &
              (name.eq.'rmsrho').or.(name.eq.'armsrho').or. &
               (name.eq.'armspressure')) then
!        bilinear interpolation of log(rho)
           do l=1,dimlevs
             x2(l,j)=(1._pm_reel-t)*(1._pm_reel-u)*log(y2(l,1))+t*(1._pm_reel-u)*log(y2(l,2)) &
                    +t*u*log(y2(l,3))+(1._pm_reel-t)*u*log(y2(l,4))
             x2(l,j)=exp(x2(l,j))
           enddo
          else
!        bilinear interpolation
           do l=1,dimlevs
             x2(l,j)=(1._pm_reel-t)*(1._pm_reel-u)*y2(l,1)+t*(1._pm_reel-u)*y2(l,2) &
                    +t*u*y2(l,3)+(1._pm_reel-t)*u*y2(l,4)
           enddo
          endif
         enddo !do j = 1,2

!     Linear interpolation in time :
        do l=1,dimlevs
          a2(l) = w*x2(l,1) + (1._pm_reel-w)*x2(l,2)

!     Season interpolation between the two seasons
!     --------------------------------------------
          a(l) = (wl)*a(l) + (wh)*a2(l) 
        enddo

      endif !if itimint=1 (seasonal interpolation)

      return

      end subroutine profi

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season(ls,numsaison)

!     compute the season number for a given areocentric longitude

      implicit none

!     Entrées
      real(pm_reel), intent(in) :: ls

!     Sorties 
      integer, intent(out) :: numsaison

      if ((ls.ge.0.0_pm_reel).and.(ls.lt.30._pm_reel)) then
         numsaison=1
      endif
      if ((ls.ge.30.0_pm_reel).and.(ls.lt.60._pm_reel)) then
         numsaison=2
      endif
      if ((ls.ge.60.0_pm_reel).and.(ls.lt.90._pm_reel)) then
         numsaison=3
      endif
      if ((ls.ge.90.0_pm_reel).and.(ls.lt.120._pm_reel)) then
         numsaison=4
      endif
      if ((ls.ge.120.0_pm_reel).and.(ls.lt.150._pm_reel)) then
         numsaison=5
      endif
      if ((ls.ge.150.0_pm_reel).and.(ls.lt.180._pm_reel)) then
         numsaison=6
      endif
      if ((ls.ge.180.0_pm_reel).and.(ls.lt.210._pm_reel)) then
         numsaison=7
      endif
      if ((ls.ge.210.0_pm_reel).and.(ls.lt.240._pm_reel)) then
         numsaison=8
      endif
      if ((ls.ge.240.0_pm_reel).and.(ls.lt.270._pm_reel)) then
         numsaison=9
      endif
      if ((ls.ge.270.0_pm_reel).and.(ls.lt.300._pm_reel)) then
         numsaison=10
      endif
      if ((ls.ge.300.0_pm_reel).and.(ls.lt.330._pm_reel)) then
         numsaison=11
      endif
      if ((ls.ge.330.0_pm_reel).and.(ls.le.360._pm_reel)) then
         numsaison=12
      endif
      
      return
      end subroutine season

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine season2(ls,numsaison,nums2,wl,wh,dust)

!     compute the 2 season numbers(nums,nums2) from areocentric longitude
!     for when itimint = 1, ie for seasonal interpolation
!     wl= lower season weighting, wh=higher season weighting

      implicit none

!     Entrées
      real(pm_reel), intent(in) ::   ls
      integer, intent(in) :: dust

!     Sorties 
      integer, intent(out) :: numsaison,nums2
      real(pm_reel), intent(out) ::    wl,wh

!     Variable locale
      real(pm_reel) :: lsc      

!     compute lower season
      numsaison=int((ls/30)+0.5_pm_reel)
      if (numsaison.eq.0) numsaison=12
!     compute higher season
      nums2=numsaison+1   
      if (nums2.eq.13) nums2=1
!     compute centre of season to find weightings
      lsc=((numsaison-1)*30)+15._pm_reel
      if (ls.lt.15._pm_reel) lsc=-15._pm_reel
!     compute weightings
      wh=(ls-lsc)/30._pm_reel
      wl=1._pm_reel-wh

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

!     Entrées 
      real(pm_reel), intent(in) ::  sol
!     Sorties
      real(pm_reel), intent(out) :: ls

!     Variables locales
      real(pm_reel) :: year_day,peri_day,timeperi,e_elips
      real(pm_reel) :: pi,radtodeg
! number of martian days (sols) in a martian year
      parameter (year_day=668.6_pm_reel)
! perihelion date (in sols)
      parameter (peri_day=485.35_pm_reel)
! orbital eccentricity
      parameter (e_elips=0.09340_pm_reel)
      parameter (pi=3.14159265358979_pm_reel)
!  radtodeg: 180/pi
      parameter (radtodeg=57.2957795130823_pm_reel)
!  timeperi: 2*pi*( 1 - Ls(perihelion)/ 360 ); Ls(perihelion)=250.99
      parameter (timeperi=1.90258341759902_pm_reel)

      real(pm_reel) :: zanom,xref,zx0,zdx,zteta,zz
!  xref: mean anomaly, zx0: eccentric anomaly, zteta: true anomaly	
      integer :: iter

      zz=(sol-peri_day)/year_day
      zanom=2._pm_reel*pi*(zz-nint(zz))
      xref=abs(zanom)

!  The equation zx0 - e * sin (zx0) = xref, solved by Newton
      zx0=xref+e_elips*sin(xref)
      do 110 iter=1,10
         zdx=-(zx0-e_elips*sin(zx0)-xref)/(1.-e_elips*cos(zx0))
         if(abs(zdx).le.(1.e-7_pm_reel)) then ! typically, 2 or 3 iterations are enough
	   goto 120
	 endif
         zx0=zx0+zdx
  110 continue
  120 continue
      zx0=zx0+zdx
      if(zanom.lt.0._pm_reel) zx0=-zx0

! compute true anomaly zteta, now that eccentric anomaly zx0 is known
      zteta=2._pm_reel*atan(sqrt((1._pm_reel+e_elips)/(1._pm_reel-e_elips)) * &
           tan(zx0/2._pm_reel))

! compute Ls
      ls=zteta-timeperi
      if(ls.lt.0._pm_reel) ls=ls+2._pm_reel*pi
      if(ls.gt.2._pm_reel*pi) ls=ls-2._pm_reel*pi
! convert Ls in deg.
      ls=radtodeg*ls

      return
      end subroutine sol2ls

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real(pm_reel) function ls2sol(ls)

!  Returns solar longitude, Ls (in deg.), from day number (in sol),
!  where sol=0=Ls=0 at the northern hemisphere spring equinox

      implicit none

!  Arguments:
      real(pm_reel), intent(in) :: ls

!  Local:
      real(pm_reel) :: xref,zx0,zteta,zz
!	xref: mean anomaly, zteta: true anomaly, zx0: eccentric anomaly
      real(pm_reel) :: year_day 
      real(pm_reel) :: peri_day,timeperi,e_elips
      real(pm_reel) :: pi,degrad 
      parameter (year_day=668.6_pm_reel) ! number of sols in a amartian year
!      data peri_day /485.0/
      parameter (peri_day=485.35_pm_reel) ! date (in sols) of perihelion
!  timeperi: 2*pi*( 1 - Ls(perihelion)/ 360 ); Ls(perihelion)=250.99
      parameter (timeperi=1.90258341759902_pm_reel)
      parameter (e_elips=0.0934_pm_reel)  ! eccentricity of orbit
      parameter (pi=3.14159265358979_pm_reel)
      parameter (degrad=57.2957795130823_pm_reel)

      if (abs(ls).lt.1.0e-5_pm_reel) then
         if (ls.ge.0.0_pm_reel) then
            ls2sol = 0.0_pm_reel
         else
            ls2sol = year_day
         end if
         return
      end if

      zteta = ls/degrad + timeperi
      zx0 = 2.0_pm_reel*datan(dtan(0.5_pm_reel*zteta)/sqrt((1._pm_reel+e_elips)/(1._pm_reel-e_elips)))
      xref = zx0-e_elips*sin(zx0)
      zz = xref/(2._pm_reel*pi)
      ls2sol = zz*year_day + peri_day
      if (ls2sol.lt.0.0_pm_reel) ls2sol = ls2sol + year_day
      if (ls2sol.ge.year_day) ls2sol = ls2sol - year_day

      return
      end function ls2sol

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
    
!--------------  
!     Entrées
      real(pm_reel), intent(in) ::    lon        !longitude (east) of point
      real(pm_reel), intent(in) ::    lat        !latitude of point
      real(pm_reel), intent(in) ::    utime      !Universal time (0. to 24. hrs) =local time at lon=0
      character(len=16), intent(in) :: name  !Name of variable
      integer, intent(in) :: itimint    !seasonal interpolation flag
                                  !  (==1 if seasonal interpolation is required)
      real(pm_reel), intent(in) ::    wl,wh      !seasonal interpolation weights
      real(pm_reel), intent(in) ::    ps_psgcm   ! High res to GCM surface pressure ratio 

!--------------
!     Sorties
      integer, intent(out) :: ier     !error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) ::    a       !value of variable

!---------------------
!     Variables locales
      real(pm_reel) ::      a2      !seasonal interpolation variable  
      integer ::   i, j
      integer ::   k,sd,rms ! "flags" associated to variable 'name'
      integer ::   itime(2) ! nearest MCD time indexes
      integer ::   iut
      integer ::   dlon(4) ! nearest grid point longitude indexes
      integer ::   dlat(4) ! nearest grid point latitude indexes
      real(pm_reel) ::      y(4)    ! to store nearest grid point values
      real(pm_reel) ::      x(2)  ! bilinearly interpolated values ("lower" season)
      real(pm_reel) ::      t,u   ! longitude and latitude-wise interpolation weights
      real(pm_reel) ::      w     ! time-wise interpolation weight
      real(pm_reel) ::      y2(4) ! to store nearest grid point values
      real(pm_reel) ::      x2(2) ! bilinearly interpolated values ("higher" season)

!     Init
      ier = 0
      k=0
      y(:) = 0._pm_reel
      y2(:) = 0._pm_reel

!     find nearest 4 grid points
      call grid4(lon,lat,dlon,dlat,t,u)
!     dlon() and dlat() now contain longitude and latitude indexes
!     and t and u (interpolation weights) are consequently set

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
      elseif (name.eq.'rmstsurf') then
        k=1
        rms=1
      else if (name.eq.'rmsps') then
        k=2
        rms=1 
      elseif (name.eq.'rmsdod') then
        k=3
        rms=1
      else  if((name.ne.'orography').and. &
               (name.ne.'areoid').and.(name.ne.'substd')) then
!        CASE of an unexpected name (orography, areoid and substd
!             are treated below)
         if (output_messages) then
          write(out,*) 'problem using subroutine var2d : the name ',name
          write(out,*) 'is not recognized'
         endif
         stop
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
          elseif (rms.eq.1) then
            do i=1,4
               y(i)=varrms2d(dlon(i),dlat(i),k)
            enddo
          endif
          
          if ((name.eq.'ps').or. &
              (name.eq.'rmsps')) then
!         bilinear interpolation of log(surface pressure)
            x(j)=(1._pm_reel-t)*(1._pm_reel-u)*log(y(1))+t*(1._pm_reel-u)*log(y(2)) &
                +t*u*log(y(3))+(1._pm_reel-t)*u*log(y(4))
            x(j)=exp(x(j))
          else
!         bilinear interpolation in space
            x(j)=(1._pm_reel-t)*(1._pm_reel-u)*y(1)+t*(1._pm_reel-u)*y(2) &
                +t*u*y(3)+(1._pm_reel-t)*u*y(4)
          endif
      end do ! of do j=1,2 loop
      
!     linear interpolation in time:
      a = w*x(1) + (1._pm_reel-w)*x(2)
            
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
           elseif (rms.eq.1) then
             do i=1,4
              y2(i)=varrms2d2(dlon(i),dlat(i),k)
             enddo
           endif
           
          if ((name.eq.'ps').or. &
              (name.eq.'rmsps')) then
!          bilinear interpolation of log(surface pressure)
             x2(j)=(1._pm_reel-t)*(1._pm_reel-u)*log(y2(1))+t*(1._pm_reel-u)*log(y2(2)) &
                  +t*u*log(y2(3))+(1._pm_reel-t)*u*log(y2(4))
             x2(j)=exp(x2(j))
           else
!          bilinear interpolation in space 
             x2(j)=(1._pm_reel-t)*(1._pm_reel-u)*y2(1)+t*(1._pm_reel-u)*y2(2) &
                  +t*u*y2(3)+(1._pm_reel-t)*u*y2(4)
           endif 
        end do  ! of do j=1,2 loop

!       Linear interpolation in utime for higher season          
        a2 = w*x2(1) + (1._pm_reel-w)*x2(2)

!     Season interpolation between the two seasons
!     --------------------------------------------        
         a = wl*a + wh*a2

      endif ! of if (itimint.ge.1)


!     Multiply (ie: rescale) by ps_psgcm for some variables
      if ((name.eq.'dod').or. &
          (name.eq.'rmsdod').or.(name.eq.'rmsps').or. &
          (name.eq.'col_h2ovapor')) then
        a=a*ps_psgcm
      endif

      return

!     error handling (unused yet)
! 9999 a=0.
!      return
      end subroutine var2d


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine interpol(k,up_low,vtype,pseudoalt)

!     calculate the vertical interpolation between low and up atmosphere    
!     when data are not in both files (low and thermo)    
      
      implicit none
      
      
!--------------
!     Entrées      
      integer, intent(in) :: k,up_low
      character(len=4), intent(in) :: vtype
      real(pm_reel), intent(in) ::    pseudoalt
      dimension pseudoalt(dimlevs)

!     local variables
      integer ::  startk,endk
      integer ::  iloop,kloop,jloop,it

      if (up_low.eq.up) then 
      startk=low+1
      endk=low+3
      else
      startk=low-2        
      endk=low
      endif

!     3D extrapolation for the first 3 layers of the thermosphere          
      if (vtype.eq.'mean') then
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
       (log(max(var_3d(iloop,jloop,31,it,k),1.E-30_pm_reel))- &
       log(var_3d(iloop,jloop,1,it,k))) &
       /(pseudoalt(31)-pseudoalt(1)) &
       )

       var_3d2(iloop,jloop,kloop,it,k)= &
       exp( &
       log(var_3d2(iloop,jloop,1,it,k))+ &
       (pseudoalt(kloop)-pseudoalt(1))* &
       (log(max(var_3d2(iloop,jloop,31,it,k),1.E-30_pm_reel))- &
       log(var_3d2(iloop,jloop,1,it,k))) &
       /(pseudoalt(31)-pseudoalt(1)) &
       )
            
            enddo !iloop
           enddo !jloop
          enddo !kloop
         endif! "o" case 
        enddo   
      elseif (vtype.eq.'rms') then
!    2D extrapolation for the first 3 layers of the thermosphere          
         do kloop=startk,endk
           do jloop=1,dimlat
            do iloop=1,dimlon
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
            enddo
           enddo
          enddo
       endif !vtype
      end subroutine interpol

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine extrapol3d(vtype,k,up_low,pseudoalt)

!  extrapolate 3d data when not in the initial netcdf files 
!  call subroutine interpol 

      implicit none
      
      
      
!---------------
!     Entrées      
      integer, intent(in) :: k,up_low
      character(len=4), intent(in) ::  vtype
      real(pm_reel), intent(in) ::     pseudoalt
      dimension  pseudoalt(dimlevs)

!---------------------
!     Variables locales      
      integer ::  iloop,kloop,jloop,it
      real(pm_reel) ::     var_ini(nbvarup-nbcom)

!     initial values in low atmosphere 
      var_ini(1)=0.2E-11_pm_reel!o : only for surface
      var_ini(2)=95.32E-2_pm_reel!co2
      var_ini(3)=8.E-4_pm_reel !co
      var_ini(4)=0.027_pm_reel!n2

      if (up_low.eq.up) then !extrapolation in the thermosphere 

       if (vtype.eq.'mean') then
        do it=1,dimuti
         do kloop=low+4,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon
                 var_3d(iloop,jloop,kloop,it,k)=0._pm_reel
                 var_3d2(iloop,jloop,kloop,it,k)=0._pm_reel
            enddo
           enddo
          enddo
         enddo

       elseif (vtype.eq.'stdv'.or.vtype.eq.'rms')    then 
         do kloop=low+4,dimlevs
          do jloop=1,dimlat
           do iloop=1,dimlon
             if (vtype.eq.'rms') then
                varrms3d(iloop,jloop,kloop,k)=0._pm_reel
                varrms3d2(iloop,jloop,kloop,k)=0._pm_reel
             endif
            enddo
           enddo
          enddo

       endif    

! extrapolation for the first 3 layers of the thermosphere          
! *********************************************************        
      call  interpol(k,up,vtype,pseudoalt)

      else  ! extrapolation in the lower atmosphere   

       if (vtype.eq.'mean') then
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

       elseif (vtype.eq.'stdv'.or.vtype.eq.'rms')    then 
         do kloop=1,low-3
           do jloop=1,dimlat
            do iloop=1,dimlon
              if (vtype.eq.'rms') then
               varrms3d(iloop,jloop,kloop,k)=0._pm_reel
               varrms3d2(iloop,jloop,kloop,k)=0._pm_reel
              endif
            enddo
           enddo
          enddo
        endif    

! extrapolation for the last 3 layers of the low atmosphere          
! *********************************************************        
       call  interpol(k,low,vtype,pseudoalt)
        
       endif! up_low          

      return
      end subroutine extrapol3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_2d(nf,k,varname,order)

!  get_2d reads the 2d mean variables in low netcdf files.
!  Fills var_2d() and var_2d2() (defined in constants_mcd.inc)  

      implicit none

! ------------------
!     Entrées      
      integer ::           nf,k,order
      character(len=50) :: varname

! ------------------
!     Variables locales     
      integer ::   ierr,iloop,jloop,kloop,it
      integer ::   var2didin(nbvar2d)
      real ::      temp2d(dimlon,dimlat,dimuti)


!     Init des champs remplis par NETCDF
      do iloop = 1, nbvar2d
         var2didin(iloop) = 0
      end do
      do kloop = 1, dimuti
         do jloop = 1, dimlat
            do iloop = 1, dimlon
               temp2d(iloop, jloop, kloop) = 0.
            end do
         end do
      end do

!     NETCDF reading 2d mean variable      
         ierr=NF_INQ_VARID(nf,varname,var2didin(k))
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GET_2D Error:"
             write(out,*) NF_STRERROR(ierr), varname
           endif
           stop
         endif
         ierr = NF_GET_VAR_REAL(nf,var2didin(k),temp2d)
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GET_2D Error:"
             write(out,*) NF_STRERROR(ierr)
           endif
           stop
         endif

         do it=1,dimuti
           do jloop=1,dimlat
            do iloop=1,dimlon
                  if (order.eq.1) then
                  ! Recopie et conversion en double précision
                  var_2d(iloop,jloop,it,k)= &
                  real(temp2d(iloop,dimlat+1-jloop,it) , kind=pm_reel)
                  elseif (order.eq.2) then 
                  ! Recopie et conversion en double précision
                  var_2d2(iloop,jloop,it,k)= &
                  real(temp2d(iloop,dimlat+1-jloop,it), kind=pm_reel) 
                  endif
            enddo
           enddo
          enddo

      return
      end subroutine get_2d

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine get_3d(nf,k,varname,up_low,order)

!  getsd_3D reads the 3D mean variables in up or low netcdf files.
!  Fills var_3d() and var_3d2() (defined in constants_mcd.inc)  

      implicit none

      
! ------------------
!     Entrées      
      integer,intent(in) :: nf,k,up_low,order
      character(len=50), intent(in) :: varname

! ------------------
!     local variables      
      integer ::   iloop,jloop,kloop,lloop,it, ierr
      integer ::   var3didin(nbvar3d)
      ! Tableaux en simple précision destinés à être remplis par NETCDF
      real ::      temp3dlow(dimlon,dimlat,low,dimuti)
      real ::      temp3dup(dimlon,dimlat,up,dimuti)
       
!     Init des champs remplis par NETCDF
      do iloop = 1, nbvar3d
         var3didin(iloop) = 0
      end do

      do lloop = 1, dimuti
         do kloop = 1, low 
            do jloop = 1, dimlat
               do iloop = 1, dimlon
                  temp3dlow(iloop, jloop, kloop, lloop) = 0.
               end do
            end do
         end do
      end do

      do lloop = 1, dimuti
         do kloop = 1, up 
            do jloop = 1, dimlat
               do iloop = 1, dimlon
                  temp3dup(iloop, jloop, kloop, lloop) = 0.
               end do
            end do
         end do
      end do


!     write(out,*),'varname(k) k',k,varname
      ierr=NF_INQ_VARID(nf,varname,var3didin(k))
!         ierr=NF_INQ_VARID(nf,varname,var3didin2(k))
      if(ierr.ne.NF_NOERR) then
        if (output_messages) then
          write(out,*) "GET_3D Error:"
          write(out,*) NF_STRERROR(ierr),varname
        endif
        stop 
      endif
      if (up_low.eq.up) then
         ierr = NF_GET_VAR_REAL(nf,var3didin(k),temp3dup)
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GET_3D Error:"
             write(out,*) NF_STRERROR(ierr)
           endif
           stop
         endif

         do it=1,dimuti
          do kloop=low+1,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon
             if (order.eq.1) then
                  ! recopie de la valeur en double précision
                  var_3d(iloop,jloop,kloop,it,k)= &
                  real(temp3dup(iloop,dimlat+1-jloop,kloop-low,it) , kind=pm_reel) 
             elseif(order.eq.2) then    
                  ! recopie de la valeur en double précision  
                  var_3d2(iloop,jloop,kloop,it,k)= &
                  real(temp3dup(iloop,dimlat+1-jloop,kloop-low,it), kind=pm_reel) 
             endif
            enddo
           enddo
          enddo
         enddo 

      else    

        ierr = NF_GET_VAR_REAL(nf,var3didin(k),temp3dlow)
        if(ierr.ne.NF_NOERR) then
          if (output_messages) then
            write(out,*) "GET_3D Error:"
            write(out,*) NF_STRERROR(ierr)
          endif
          stop
        endif

        do it=1,dimuti
         do kloop=1,low 
          do jloop=1,dimlat
           do iloop=1,dimlon
            if (order.eq.1) then
                  ! recopie de la valeur en double précision
                  var_3d(iloop,jloop,kloop,it,k)= &
                  real(temp3dlow(iloop,dimlat+1-jloop,kloop,it), kind=pm_reel)
            elseif(order.eq.2) then      
                  ! recopie de la valeur en double précision
                  var_3d2(iloop,jloop,kloop,it,k)= &
                  real(temp3dlow(iloop,dimlat+1-jloop,kloop,it), kind=pm_reel) 
            endif     
             enddo
            enddo
          enddo
         enddo

      endif ! of if (up_low.eq.up)

      return
      end subroutine get_3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsd_2d(nf,vtype,k,varname,order)

!  getsd_2D reads the 2D RMS variables from netcdf files and
! fills corresponding varrms2d(k) and varrms2d2(k) arrays (which are defined
! as common in constants_mcd.inc)

      implicit none

      
      
! -------------
!     Entrées      
      integer ::           nf      ! Input NetCDF file ID
      character(len=4) ::  vtype
      integer ::           k
      character(len=50) :: varname
      integer ::           order

! -------------
!     Variables locales      
      integer ::   ierr,iloop,jloop,kloop
      integer ::   sd2didin(nbsd2d)    ! to store ID # of variable
      real ::      temp2d(dimlon,dimlat,dimuti) ! to temporarely store variable

!  Init des champs remplits par NETCDF
      do iloop = 1, nbsd2d
         sd2didin(iloop) = 0
      end do
     
      do kloop = 1,dimuti
         do jloop = 1,dimlat
            do iloop = 1,dimlon
               temp2d(iloop, jloop,kloop) = 0.
            end do
         end do
      end do

!  Lecture de la variable dans le fichier NetCDF      
      ierr=NF_INQ_VARID(nf,varname,sd2didin(k))
      if(ierr.ne.NF_NOERR) then
         if (output_messages) then
            write(out,*) "GETSD_2D Error:"
            write(out,*) NF_STRERROR(ierr),varname
         endif
         stop 
      endif
      ierr = NF_GET_VAR_REAL(nf,sd2didin(k),temp2d)
      if(ierr.ne.NF_NOERR) then
         if (output_messages) then
            write(out,*) "GETSD_2D Error:"
            write(out,*) NF_STRERROR(ierr) , 'stop in getsd3d'
         endif
         stop 
      endif

! Recopie des variables
      do jloop=1,dimlat
         do iloop=1,dimlon

            if (vtype.eq.'rms') then
               if (order.eq.1) then
                  ! Recopie et conversion en double précision
                  varrms2d(iloop,jloop,k)= &
                       real(temp2d(iloop,dimlat+1-jloop,1), kind=pm_reel)
               elseif (order.eq.2) then
                  ! Recopie et conversion en double précision
                  varrms2d2(iloop,jloop,k)= &
                       real(temp2d(iloop,dimlat+1-jloop,1), kind=pm_reel)
               endif
            endif

         enddo
      enddo

      return
    end subroutine getsd_2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getsd_3d(nf,vtype,k,varname,up_low,order)

! getsd_3D reads the 3D RMS and ARMS variables in up or low netcdf files 
! and fills corresponding varrms3d(k) and vararms3d(k) arrays (which are 
! defined as common in constants_mcd.inc)
      implicit none
      
      
      
 
! -------------   
!     Entrées      
      integer ::   nf      ! Input NetCDF file ID
      character(len=4) :: vtype
      integer ::   k
      character(len=50) :: varname
      integer ::   up_low
      integer ::   order

! ----------------------
!     Variables locales
      integer ::   iloop,kloop,jloop,lloop ! indices de boucle
      integer ::   ierr
      integer ::   sd3didin(nbsd3d+1)     ! to store ID # of variable
      ! Les deux tableaux suivant sont en simple précision pour être
      ! remplis par NETCDF
      real ::      temp3dlow(dimlon,dimlat,low,dimuti)
! temp3dlow(:,:,:,:) to temporarely store variable from "low" file
      real ::      temp3dup(dimlon,dimlat,up,dimuti)
! temp3dup(:,:,:,:) to temporarely store variable from "up" file

!     Init des champs remplis par NETCDF
      do iloop = 1, nbsd3d+1
         sd3didin(iloop) = 0
      end do

      do lloop = 1, dimuti
         do kloop = 1, low 
            do jloop = 1, dimlat
               do iloop = 1, dimlon
                  temp3dlow(iloop, jloop, kloop, lloop) = 0.
               end do
            end do
         end do
      end do

      do lloop = 1, dimuti
         do kloop = 1, up 
            do jloop = 1, dimlat
               do iloop = 1, dimlon
                  temp3dup(iloop, jloop, kloop, lloop) = 0.
               end do
            end do
         end do
      end do

!  Get ID of variable from NetCDF file      
         ierr=NF_INQ_VARID(nf,varname,sd3didin(k))
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GETSD_3D Error:"
             write(out,*) NF_STRERROR(ierr),varname
           endif
           stop 
         endif

      if (up_low.eq.up) then
      ! Read variable from "up" file
         ierr = NF_GET_VAR_REAL(nf,sd3didin(k),temp3dup)
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GETSD_3D Error:"
             write(out,*) NF_STRERROR(ierr)
           endif
           stop
         endif

         ! Copy variable
          do kloop=low+1,dimlevs
           do jloop=1,dimlat
            do iloop=1,dimlon

             if (vtype.eq.'rms') then
                  if (order.eq.1) then
                    varrms3d(iloop,jloop,kloop,k)= &
                      temp3dup(iloop,dimlat+1-jloop,kloop-low,1) 
                  elseif (order.eq.2) then
                    varrms3d2(iloop,jloop,kloop,k)= &
                      temp3dup(iloop,dimlat+1-jloop,kloop-low,1) 
                  endif
             else if (vtype.eq.'arms') then
                  if (order.eq.1) then
                    vararms3d(iloop,jloop,kloop,k)= &
                      temp3dup(iloop,dimlat+1-jloop,kloop-low,1) 
                  elseif (order.eq.2) then
                    vararms3d2(iloop,jloop,kloop,k)= &
                      temp3dup(iloop,dimlat+1-jloop,kloop-low,1) 
                  endif
             endif
             
            enddo
           enddo
          enddo

      else ! Read variable from "low" file
         ierr = NF_GET_VAR_REAL(nf,sd3didin(k),temp3dlow)
         if(ierr.ne.NF_NOERR) then
           if (output_messages) then
             write(out,*) "GETSD_3D Error:"
             write(out,*) NF_STRERROR(ierr)
           endif
           stop
         endif

         ! Copy variable (and permute latitude index)
          do kloop=1,low
           do jloop=1,dimlat
            do iloop=1,dimlon

             if (vtype.eq.'rms') then
                  if (order.eq.1) then
                  ! recopie et conversion en double précision
                  varrms3d(iloop,jloop,kloop,k)= &
                    real(temp3dlow(iloop,dimlat+1-jloop,kloop,1), kind=pm_reel) 
                  elseif (order.eq.2) then
                  ! recopie et conversion en double précision
                  varrms3d2(iloop,jloop,kloop,k)= &
                    real(temp3dlow(iloop,dimlat+1-jloop,kloop,1) , kind=pm_reel) 
                  endif
             else if (vtype.eq.'arms') then
                  if (order.eq.1) then
                  ! recopie et conversion en double précision
                  vararms3d(iloop,jloop,kloop,k)= &
                    real(temp3dlow(iloop,dimlat+1-jloop,kloop,1), kind=pm_reel)  
                  elseif (order.eq.2) then
                  ! recopie et conversion en double précision
                  vararms3d2(iloop,jloop,kloop,k)= &
                    real(temp3dlow(iloop,dimlat+1-jloop,kloop,1), kind=pm_reel)  
                  endif
             endif
              
            enddo
           enddo
          enddo
       endif! of if (up_low.eq.up)         

      return
      end subroutine getsd_3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine var3d(a,lon,lat,zsurface,levhi,levlow,levweight,pratio, &
                       sheight,p_pgcm,ps_psgcm,utime,name,ier, &
                       itimint,wl,wh)

!     Retrieve the value of 3-d variable=name at longitude=lon, latitude=lat
!     and Universal time=utime. The height (above local surface) of the
!     variable is given by zsurface, although only levhi, levlow and levweight
!     are needed for most vertical interpolations.
!     Bilinear interpolation (see called routine profi) is used to get user
!     specified longitude and latitude.
!     Linear interpolation of variables with height is done in the vertical
!     direction, exept for density (and similar fields), which is
!     interpolated linearly with pressure.
!     When below first atmospheric level, specific "interpolations" are used
!     for temperature (linear interpolation with using surface temp.) and
!     horizontal winds (using log law).

      implicit none

      
!------------------
!     Entrées
      real(pm_reel),intent(in) :: lon         ! longitude (east) of point (deg.)
      real(pm_reel),intent(in) ::         lat         ! latitude of point (deg.)
      real(pm_reel),intent(in) ::         zsurface    ! altitude above surface of point (m)
      integer,intent(in) ::      levhi       ! database level upper bound
      integer,intent(in) ::      levlow      ! database level lower bound
      real(pm_reel),intent(in) ::         levweight(2)! level weight for vertical interpolation
!     (1) for linear in height (2) for linear in pressure
      real(pm_reel),intent(in) ::         pratio   !ratio of pressure to extreme value if out of range
      real(pm_reel),intent(in) ::         sheight(dimlevs) ! altitude of GCM sigma levels
      real(pm_reel),intent(in) ::         p_pgcm(dimlevs) ! high res to GCM pressure ratios
      real(pm_reel),intent(in) ::         ps_psgcm ! High res to GCM surface pressure ratio 
      real(pm_reel),intent(in) ::        utime    !Universal time (0. to 24. hrs)=local time at lon=0
      integer,intent(in) ::      itimint  !seasonal interpolation flags
      real(pm_reel),intent(in) ::       wl,wh    !seasonal interpolation weights 
      character(len=16),intent(in) :: name     !Name of variable

!------------------
!     Sorties
      integer, intent(out) ::       ier        !error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) ::          a          !value of variable

!------------------
!     local variables
      real(pm_reel) :: profile(dimlevs) ! profile in MCD grid 
      character(len=16) :: tmpname     ! Name of variable (for call to var2d)
      real(pm_reel) :: tsurf ! surface temperature (used for temperature interpolation)
      integer :: ierr

      ier = 0

      ! get horizontally interpolated GCM vertical profile of variable 'name'
      call profi(profile,lon,lat,utime,name,ier,itimint,wl,wh)

      if (ier.ne.0) then
        if (output_messages) then
         write(out,*)'VAR3D Error : profile not available for variable',name
        endif
        ier=1
        goto 9999
      endif


      if ((name.eq.'rho').or. &
          (name.eq.'rmsrho').or.(name.eq.'armsrho').or. &
          (name.eq.'armspressure')) then
!     interpolate densities linearly in pressure
         a=profile(levlow)*p_pgcm(levlow)+levweight(2)* &
          (profile(levhi)*p_pgcm(levhi)-profile(levlow)*p_pgcm(levlow))
!     correction for densities which are out of sigma range
         a=pratio*a
      elseif (name.eq.'temp') then
        ! Default linear interpolation between levels:
        a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
        ! If below first atmospheric level, use linear
        ! interpolation with surface temperature
        if ((levlow.eq.levhi).and.(levlow.eq.1)) then
        ! for temperature, below first atmospheric level, we do
        ! an interpolation with surface temperature
          ! get surface temperature at same location
          tmpname="tsurf"
          call var2d(tsurf,lon,lat,utime,tmpname, &
                     ierr,itimint,wl,wh,ps_psgcm)
          ! interpolate
          a=tsurf+(zsurface/sheight(1))*(profile(1)-tsurf)
        endif ! of if ((levlow.eq.levhi).and.(levlow.eq.1))
      elseif ((name.eq.'u').or.(name.eq.'v')) then
        ! Default linear interpolation between levels:
        a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
        ! If below first atmopheric level, mimic log boundary layer
        ! with roughness length z_0; below z_0, set wind velocity to zero
        if ((levlow.eq.levhi).and.(levlow.eq.1)) then
          if (zsurface.gt.z_0) then
            a=profile(1)*(log(zsurface/z_0)/log(sheight(1)/z_0))
          else
            a=0 ! no wind below atmospheric roughness length z_0
          endif
        endif
      else ! most variables are interpolated linearly in height
        a=profile(levlow)+(profile(levhi)-profile(levlow))*levweight(1)
      endif
!
      return

!     Error handling
 9999 a=0._pm_reel
      return
      end subroutine var3d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine max2d(a,lon,lat,name,ier,itimint,wl,wh)

!     Retrieve the daily maximum value of 2-d variable=name at 
!     longitude=lon, latitude=lat

      implicit none

!------------------
!     Entrées
      real(pm_reel), intent(in) :: lon     !west longitude of point
      real(pm_reel), intent(in) :: lat     !latitude of point
      character(len=16), intent(in) :: name    !Name of variable
      integer, intent(in) ::      itimint !seasonal interpolation flag 
      real(pm_reel), intent(in) ::         wl,wh   !seasonal interpolation weightings

!------------------
!     Sorties
      integer, intent(out) ::      ier     !Error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) ::         a       !maximum of variable

!------------------
!     local variables
      integer ::   i
      real(pm_reel) ::      x
      real(pm_reel) ::      xmax
      real(pm_reel) ::      utime
      
      ier=0
      xmax=-1.e20_pm_reel
      do i=0,22,2
        utime=float(i)
        call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh,1.0_pm_reel)
        if(ier.ne.0) then
           ier=1
           if (output_messages) then
             write(out,*)"MAX2D Error : impossible to find ",name, &
                       " maximum"
           endif
           goto 9999
        endif
        if (x.gt.xmax) xmax=x
      enddo
      a=xmax

      return

!     Error handling
 9999 a=0._pm_reel
      return
      end subroutine max2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine min2d(a,lon,lat,name,ier,itimint,wl,wh)

!     Retrieve the daily minimum value of 2-d variable=name at 
!     longitude=lon, latitude=lat
      implicit none

      

!------------------
!     Entrées
      real(pm_reel), intent(in) :: lon     !longitude of point
      real(pm_reel), intent(in) :: lat     !latitude of point
      character(len=16), intent(in) :: name    !Name of variable
      real(pm_reel), intent(in) ::         wl,wh   !seasonal interpolation weightings
      integer, intent(in) ::      itimint !seasonal interpolation flag

!------------------
!     Sorties
      integer, intent(out) :: ier     !Error flag (0=OK, 1=NOK)
      real(pm_reel), intent(out) ::    a       !minimum of variable

!------------------
!     local variables
      integer ::   i
      real(pm_reel) ::      x
      real(pm_reel) ::      xmin
      real(pm_reel) ::      utime

      ier=0
      xmin=1.e20_pm_reel
      do i=0,22,2
        utime=real(i, kind=pm_reel)
        call var2d(x,lon,lat,utime,name,ier,itimint,wl,wh,1.0_pm_reel)
        if(ier.ne.0) then
           ier=1
           if (output_messages) then
             write(out,*)"MIN2D Error : impossible to find ",name, &
                       " minimum"
           endif
           goto 9999
        endif
        if (x.lt.xmin) xmin=x
      enddo
      a=xmin
      return

!     Error handling
 9999 a=0._pm_reel
      return
      end subroutine min2d

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      function ran1(idum)
!     "Minimal" random number generator of Park and Miller with Bays-Durham
!     shuffle and added safeguards 
!     Return a uniform random deviate between 0.0 and 1.0 (exclusive of the
!     endpoints values). Call with idum a negative integer to initialize;
!     thereafter, do not alter idum between successive deviates in a sequence.
!     RNMX should approximate the largest floating value that is less than 1.

      implicit none

!     Entrée / Sortie
      integer, intent(inout) :: idum ! Seed for random number generator (if negative)
                                  ! the value of idum is altered by the function

!     Valeur de retour
      real(pm_reel) :: ran1

! local variables:
      integer :: ia,im,iq,ir,ntab,ndiv
      real(pm_reel) :: am,eps,rnmx
      parameter (ia=16807,im=2147483647,am=1._pm_reel/im,iq=127773,ir=2836, &
       ntab=32,ndiv=1+(im-1)/ntab,eps=1.2e-7_pm_reel,rnmx=1._pm_reel-eps)
      integer :: j,k
      integer :: iv(ntab),iy
      save iv,iy
      data iv /ntab*0/, iy /0/

      if ((idum.le.0).or.(iy.eq.0)) then
         idum=max(-idum,1)
         do j=ntab+8,1,-1
            k=idum/iq
            idum=ia*(idum-k*iq)-ir*k
            if (idum.lt.0) idum=idum+im
            if (j.le.ntab) iv(j)=idum
         end do
         iy=iv(1)
      endif
      k=idum/iq
      idum=ia*(idum-k*iq)-ir*k
      if (idum.lt.0) idum=idum+im
      j=1+iy/ndiv
      iy=iv(j)
      iv(j)=idum
      ran1=min(am*iy,rnmx)

      return
      end function ran1

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine dustmix(scena,lat,Ls,dod,psurf,p,dust_mmr)

!     Subroutine used to reconstruct the dust mixing ratio 
!     assumed in the GCM run, at a given pressure level. 
!     F. Forget 2005

      implicit none

      

!     Entrées
      integer, intent(in) :: scena  ! scenario
      real(pm_reel), intent(in) :: lat  ! latitude  (degree) 
      real(pm_reel), intent(in) :: Ls       ! solar longitude (deg)
      real(pm_reel), intent(in) :: dod      ! dust optical depth
      real(pm_reel), intent(in) :: psurf      ! surface pressure (Pa)
      real(pm_reel), intent(in) :: p        ! local pressure (Pa)
!     Sortie
      real(pm_reel), intent(out) :: dust_mmr  ! dust mass mixing ratio (kg/kg)
!     Variables locales
      real(pm_reel) :: radius, Qext, rho_dust
      real(pm_reel) :: zlsconst, topdust,topdust0,qextrhor,g
      real(pm_reel) :: zp, expfactor, xlat
      logical :: firstcall
      data firstcall /.true./
      save firstcall,qextrhor,g
      real(pm_reel) :: pi
      parameter   (pi=3.14159265359_pm_reel)


      topdust=0._pm_reel ! dummy initialization to get rid of compiler warnings

!     initialisation
!     --------------
      if (firstcall) then
         radius=1.8e-6_pm_reel  ! effective radius of dust (m)
         Qext=3._pm_reel  ! dust visible single scattering extinction coeff.
         rho_dust=2500._pm_reel  ! Mars dust density (kg.m-3)
         qextrhor= (0.75_pm_reel)*Qext/(rho_dust*radius)
         g=3.72_pm_reel
         firstcall = .false. 
      end if
      xlat = lat*pi/180._pm_reel

!     Altitude of the top of the dust layer
!     -------------------------------------

      zlsconst=sin(ls*pi/180._pm_reel - 2.76_pm_reel)
      if (scena.eq.8) then
             topdust= 30._pm_reel        ! constant dust layer top = 30 km
      else if (scena.eq.7) then      ! "Viking" scenario
            topdust0=60._pm_reel -22._pm_reel*sin(xlat)**2
            topdust=topdust0+18._pm_reel*zlsconst
      else if(scena.le.6) then         !"MGS" scenario (MY24, storm)
            topdust=60._pm_reel+18._pm_reel*zlsconst &
                     -(32_pm_reel+18_pm_reel*zlsconst)*sin(xlat)**4 &
                     - 8_pm_reel*zlsconst*(sin(xlat))**5
      else
        if (output_messages) then
          write(out,*) "DUST_MMR error: problem with scena"
        endif
        stop 
      endif

!     Computing dust mass mixing ratio
!     --------------------------------
      if(p.gt.700._pm_reel/(988._pm_reel**(topdust/70._pm_reel))) then
           zp=(700._pm_reel/p)**(70._pm_reel/topdust)
           expfactor=max(exp(0.007_pm_reel*(1._pm_reel-max(zp,1._pm_reel))),1.E-10_pm_reel)
      else
           expfactor=1.E-10_pm_reel
      endif
!     qextrhor is  (3/4)*Qext/(rho*reff)
      dust_mmr = expfactor* dod * g / (psurf * qextrhor)
      return
      end subroutine dustmix

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine air_properties(T,vmr_co2,vmr_n2,vmr_o,vmr_co, &
                             Cp,gamma,viscosity,Rgas)

      implicit none
                                                                                
!     Subroutine used to compute some useful gas properties
!     from the climate database data
!     F. Forget 2005
           
!     --------------                                                                     
!     Entrées
      real(pm_reel), intent(in) :: T ! temperature   (K)
      real(pm_reel), intent(in) :: vmr_co2,vmr_n2,vmr_o,vmr_co ! gas vomume mixing ratio (mol/mol)
      
!     --------------
!     Sorties
      real(pm_reel), intent(out) :: cp   ! Heat capacity at constant pressure (J kg-1? K-1)
      real(pm_reel), intent(out) :: gamma   ! Cp/Cv Ratio of specific heats 
      real(pm_reel), intent(out) :: viscosity    ! air viscosity (N s m-2)
      real(pm_reel), intent(out) ::  Rgas   ! gas constant  (J kg-1 K-1)
      
!     Variables locales
!     --------------

      real(pm_reel) :: vmr_ar   ! Argon vmr
!     Molecular heat capacity (J mol-1 K-1)
      real(pm_reel) :: Cp_co2, Cp_co, Cp_n2, Cp_ar, Cp_O
!     Coefficient to compute conductivity = A*T**0.69  (W m-1 K-1)
      real(pm_reel) :: A_co2, A_co, A_n2, A_ar, A_O
!     Molecular mass
      real(pm_reel) :: M_co2, M_co, M_n2, M_ar, M_O
      real(pm_reel) :: conduct,meanmolmass,svmr
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
            M_co2=44.01e-3_pm_reel
            M_co=28.01e-3_pm_reel
            M_n2=28.013e-3_pm_reel
            M_Ar=39.95e-3_pm_reel
            M_o=16.e-3_pm_reel
!           Molecular heat capacity (J mol-1 K-1)
            Cp_co2 = 37._pm_reel
            Cp_co  = 29_pm_reel
            Cp_n2 = 29_pm_reel
            Cp_ar = 20_pm_reel
            Cp_o = 21_pm_reel
!           Coeff for conductivity :
            A_co2=3.07e-4_pm_reel
            A_o=7.6e-4_pm_reel
            A_n2=5.6e-4_pm_reel
            A_co=4.87e-4_pm_reel
            A_ar=3.4e-4_pm_reel
            firstcall=.false.
      end if

!     Estimating Argon mixing ratio
!     -----------------------------   
      vmr_ar = 0.6_pm_reel *vmr_n2

!     Mean Molecular mass
!     ------------------
      Svmr = vmr_co2+vmr_n2+vmr_o+vmr_co+vmr_ar
      meanmolmass= (M_co2*vmr_co2+M_n2*vmr_n2+M_o*vmr_o &
            + M_co*vmr_co+M_ar*vmr_ar)/Svmr
      Rgas = 8.3144_pm_reel/meanmolmass

!     Computing Cp (Neglecting H2 => not valid if very high)
!     ------------
      Cp = (Cp_co2*vmr_co2+Cp_n2*vmr_n2+Cp_o*vmr_o &
            + Cp_co*vmr_co+Cp_ar*vmr_ar)/Svmr


!     Conversion from J mol-1 K-1 to J kg-1 K-1
      Cp= Cp / meanmolmass

!     Computing gamma
      gamma = 1._pm_reel/(1._pm_reel-Rgas/Cp)

!     Computing viscosity
!     -------------------
!     Therma molecular Conductivity (W m-1 K-1)
      conduct = (A_co2*vmr_co2+A_n2*vmr_n2+A_o*vmr_o &
              + A_co*vmr_co + A_ar*vmr_ar)/Svmr
      conduct=T**0.69_pm_reel * conduct
      viscosity = conduct / (0.25_pm_reel*(4*Cp + 5*Rgas))  
      
      return
      end subroutine air_properties

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine build_sigma_hr(sigma_gcm,ps_gcm,ps_hr,sigma_hr,p_pgcm)
      
      implicit none
      

!     Entrées
      real(pm_reel), intent(in) :: sigma_gcm(dimlevs) ! GCM sigma levels
      real(pm_reel), intent(in) :: ps_gcm             ! GCM surface pressure
      real(pm_reel), intent(in) :: ps_hr              ! High res surface pressure
!     Sorties
      real(pm_reel), intent(out) :: sigma_hr(dimlevs)  ! High res sigma levels
      real(pm_reel), intent(out) :: p_pgcm(dimlevs)    ! high res to GCM pressure ratios
      
!     local variables
      integer :: l
      real(pm_reel) :: x  ! lower layer compression (-0.9<x<0) or dilatation (0.<x<0.9)
      real(pm_reel) :: rp     ! surface pressure ratio ps_hr/ps_gcm
      real(pm_reel) :: deltaz   ! corresponding pseudo-altitude difference (km)
      real(pm_reel) :: f  ! coefficient f= p_hr / p_gcm
      real(pm_reel) :: z  ! altitude of transition of p_hr toward p_gcm (km)

! 1. Coefficients
      rp=ps_hr/ps_gcm
      deltaz=-10._pm_reel*log(rp)
      x = min(max(0.12_pm_reel*(abs(deltaz)-1._pm_reel),0._pm_reel),0.8_pm_reel)
      if(deltaz.gt.0_pm_reel) x=-x
      z=max(deltaz + 3._pm_reel,3._pm_reel)

      do l=1,dimlevs
        f=rp*sigma_gcm(l)**x
        p_pgcm(l)=f+(1-f)*0.5_pm_reel* &
             (1+tanh(6._pm_reel*(-10._pm_reel*log(sigma_gcm(l))-z)/z))
        sigma_hr(l)=p_pgcm(l)*sigma_gcm(l)/rp
      enddo

      end subroutine build_sigma_hr

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine pres0(dset,lat,lon,solar,utime,ps_MCD,oro_MCD,wl,wh, &
                       pres,alt,ierr)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Purpose:
! =======
!  Pres0 si a subroutine designed to yield high resolution MOLA topography
!  and recompute surface pressure accordingly. 
!  It uses:
!     1) Reference pressure measurements at Viking Lander 1 site (file VL1.ls)
!     2) High resolution MOLA topography (file mola32.nc)
!     3) Data from the Mars Climate Database
!
! Built from the v4.1 pres0 tool + improvements
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      

!     Entrées
      character*(*), intent(in) :: dset ! Path to MCD datafiles 
      real(pm_reel), intent(in) :: lon      ! Longitude coordinate of the point (East degrees)
      real(pm_reel), intent(in) :: lat      ! Latitude coordinate of the point (North degrees)
      real(pm_reel), intent(in) :: solar    ! Solar longitude (degrees)
      real(pm_reel), intent(in) :: utime    ! Universal time (hours) (time at lon=0)
      real(pm_reel), intent(in) :: ps_MCD   ! surface pressure (from MCD)
      real(pm_reel), intent(in) :: oro_MCD  ! orography from MCD
      real(pm_reel), intent(in) :: wl,wh    ! season-wise interpolation weights

!     Sorties
      real(pm_reel), intent(out) :: pres     ! high resolution surface pressure (Pa)
      real(pm_reel), intent(out) :: alt      ! surface altitude from MOLA (m)
      integer, intent(out) :: ierr  ! Control variable

!     Variables locales
      real(pm_reel) :: factcor  ! Pressure correction factor
      real(pm_reel) :: zlon     ! East longitude (deg.) [0:360]
      real(pm_reel) :: H        ! Scale height (m)
      real(pm_reel) :: temp     ! Temperature at altitude ~1km

      character(len=16) :: name    !Name of variable to retrieve from MCD
      integer :: ier
      real(pm_reel) :: profile(dimlevs) ! to temporarily store temperature profile
      integer :: ref_alt ! reference altitude index for temperature retrieval
      parameter (ref_alt=7) ! 7th level is ~ 1km above surface
      
! Ensure that (local) longitude zlon is in [0:360]
! since given "lon" is in [-180:180]

      zlon=lon
      if(zlon.lt.0._pm_reel) zlon =zlon + 360._pm_reel


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
! Gas Constant: R (m2.s-2.K-1) = 191
! surface gravity: g (m.s-2) = 3.73
      H=191_pm_reel*temp/3.73_pm_reel

! 5. Compute topography-corrected surface pressure
      pres=ps_MCD*factcor*exp(-(alt-oro_MCD)/H)

      end subroutine pres0
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine read_vl(dset,tab,ierr)
!Routine to read the VL1 or VL2 mean viking pressure files.
      implicit none
      

! Arguments:
!     Entrées
      character*(*), intent(in) :: dset ! path to MCD datafiles
!     Sorties
      real(pm_reel), intent(out) :: tab(669,2) ! tab(:,1) contains Ls and tab(:,2) contains pressure
      integer, intent(out) :: ierr ! returned status code (==0 if OK)

!     Variables locales
      integer :: vik_num,count
      character(len=140) :: vlfile ! (smoothed) surface pressure measured by VL

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
          write(out,*) "READ_VL Error: Failed opening file ",vlfile
        endif
        return
      endif

! 2. Read the 669 lines of file
      do count=1,669
        read(vik_num,*) tab(count,1),tab(count,2)
      enddo

! 3. Close file and return
      close (vik_num)

      end subroutine read_vl

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine calc_factcor(dset,solar,wl,wh,factcor,ierr)
! Calculate a correction factor between Viking1 surface pressure data
! and MCD output at the same point
! Note: Position of VL1 is taken from Konopliv et al. 2006 
!       and is expressed in the IAU2000 reference system.
!       Corresponding altitude was computed using bilinear interpolation of
!       MOLA 32 topography data (and using areoid computed from the mgm1025
!       spherical harmonics coefficients) 

      implicit none
      

! Arguments:
!     Entrées:
      character*(*), intent(in) :: dset ! path to MCD datafiles
      real(pm_reel), intent(in) :: solar	 ! Solar longitude (degrees)
      real(pm_reel), intent(in) :: wl,wh         ! weights for interpolation
!     Sorties:
      real(pm_reel), intent(out) :: factcor	! correction factor
      integer, intent(out) :: ierr	! status ierr=0 if everything OK

! Local variables
      ! Viking 1 coordinates
      real(pm_reel) :: vik1_lat, vik1_lon, vik1_alt
!      parameter(vik1_lat=22.2692, vik1_lon=311.7783, vik1_alt=-3627) !old ref.
      parameter(vik1_lat=22.269628_pm_reel,vik1_lon=312.050381_pm_reel, &
                vik1_alt=-3637.1396_pm_reel)

      real(pm_reel) :: vik1_MCD_oro ! MCD orography at VL1 coordinates
      real(pm_reel) :: prescalc(12) ! surface pressure at VL1 site, at times 2,4,...24 hours
      real(pm_reel) :: tempcalc(12) ! temperature above VL1, at times 2,4,...24 hours
      integer :: ref_alt ! reference altitude index for temperature retrieval
      parameter (ref_alt=7)
      real(pm_reel) :: H        ! scale height
      real(pm_reel) :: presmean ! mean value (over a day) of surface pressure
      real(pm_reel) :: lsinf,lssup,psinf,pssup,presvik !,alt
      integer :: iloop
      logical :: firstcall
      data firstcall/.true./
      save firstcall
      real(pm_reel) :: vik_tab(669,2)
      save vik_tab

      character(len=16) :: name    ! Name of variable to retrieve from MCD
      integer :: ier
      real(pm_reel) :: profile(dimlevs) ! to temporarily store temperature profile
      
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
        ! N.B. vard2d expects longitudes in [-180:180]
        call var2d(prescalc(iloop),vik1_lon-360._pm_reel,vik1_lat,2._pm_reel*iloop, &
                    name,ier,1,wl,wh,1.0_pm_reel)
        ! retrieve temperature profile
        name='temp'
        call profi(profile,vik1_lon-360._pm_reel,vik1_lat,2._pm_reel*iloop, &
                    name,ier,1,wl,wh) ! 1 pour la saison
        ! keep temperature of ref_alt layer (~1km)
        tempcalc(iloop)=profile(ref_alt)
!        write(out,*)'CALC_FACTCOR: i=',iloop,
!     &            ' prescalc(i)=',prescalc(iloop),
!     &            ' tempcalc(i)=',tempcalc(iloop)
      enddo  

! 2.2 Get MCD orography corresponding to VL1 site
      name='orography'
      call var2d(vik1_MCD_oro,vik1_lon-360._pm_reel,vik1_lat,0.0_pm_reel, &
                 name,ier,1,wl,wh,1.0_pm_reel)
!      write(out,*) 'CALC_FACTCOR: vik1_MCD_oro=',vik1_MCD_oro


! 3. Compute the correction factor due to difference between orocalc
!    and viking1 altitude

      do iloop=1,12
! Gas Constant: R (m2.s-2.K-1) = 191
! surface gravity: g (m.s-2) = 3.73
         H=191_pm_reel*tempcalc(iloop)/3.73_pm_reel
         prescalc(iloop)=prescalc(iloop)*exp(-(vik1_alt-vik1_MCD_oro)/H)
      enddo

! 4. Compute average (over the day) pressure
      presmean=0._pm_reel
      do iloop=1,12
         presmean = presmean + prescalc(iloop)/12._pm_reel
      enddo
!      write(out,*)'CALC_FACTCOR: presmean=',presmean

! 5. Get encompassing VL1 pressure data indexes and values
      if (solar.le.vik_tab(1,1)) then
         lsinf=vik_tab(669,1)-360_pm_reel
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
         do while ((vik_tab(iloop,1).lt.solar).and.(iloop.lt.669_pm_reel))
            iloop=iloop+1
         enddo
         lsinf=vik_tab(iloop-1,1)
         lssup=vik_tab(iloop,1)+360_pm_reel
         psinf=vik_tab(iloop-1,2)
         pssup=vik_tab(iloop,2)
      endif

! 6. Interpolate VL1 pressure data to solar longitude 'solar'

! iloop-1 is the "lower limit" and iloop the "upper limit" in tab_vik
      presvik=psinf+(solar-lsinf)*(pssup-psinf)/(lssup-lsinf)
!      write(out,*)'CALC_FACTCOR: presvik=',presvik

! 7. Compute the pressure correction factor
      factcor=presvik/presmean

      end subroutine calc_factcor

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine nearsurfacenoise(ps,ps_noise_dev,ps_noise, &
          temp_gcm_noise_dev,temp_gcm_noise,sheight)

! Routine to build perturbations (noise) for surface pressure and 
! atmospheric temperatures near the surface. The amplitudes and 
! range over which these perturbations are computed are hard coded 
! in the routine (see below). E.M. 03/2009.
      implicit none
!      include "constants_mcd.inc"
! arguments -inputs-:
      real(kind=pm_reel), intent(in) :: ps ! surface pressure
      real(kind=pm_reel), intent(in) :: ps_noise_dev ! gaussian deviate (of unit standard deviation)
!      real temp_gcm(dimlevs) ! atmospheric temperatures
      real(kind=pm_reel), intent(in) :: temp_gcm_noise_dev ! gaussian deviate
      real(kind=pm_reel), intent(in) :: sheight(dimlevs) ! altitude of layers
! arguments -outputs-:
      real(kind=pm_reel), intent(out) :: ps_noise ! perturbation to add to surface pressure
      real(kind=pm_reel), intent(out) :: temp_gcm_noise(dimlevs) ! perturbations to add to temperatures

! local variables:
      real(kind=pm_reel) old_ps_noise_dev ! to store value from previous calls
      data old_ps_noise_dev /-77._pm_reel/ ! dummy initial value
      save old_ps_noise_dev

      real(kind=pm_reel) ps_noise_level ! standard deviation of (relative) pert. in pressure
      parameter (ps_noise_level=0.01_pm_reel) !  e.g. 0.02=2%

      real(kind=pm_reel) T_noise_level ! standard deviation of pert in temperature (K)
      parameter (T_noise_level=3_pm_reel) ! e.g. 5K

      real(kind=pm_reel) T_noise_mid ! altitude (m) at which temp pert. will be half of max
      parameter (T_noise_mid=6000_pm_reel)

      real(kind=pm_reel) T_noise_delta ! range (m) over which noise drops from max to zero
      parameter (T_noise_delta=4000_pm_reel)

      integer lay
      real(kind=pm_reel) coeff ! altitude-dependent coefficient for temperature pert.

      if (ps_noise_dev.ne.old_ps_noise_dev) then ! compute perturbations

        ! 1. surface pressure perturbation
        ps_noise=ps*ps_noise_dev*ps_noise_level
!        write(*,*) 'ps_noise_dev=',ps_noise_dev,
!     &             ' temp_gcm_noise_dev=',temp_gcm_noise_dev
!        write(*,*) 'ps=',ps,
!     &             ' ps_noise=',ps_noise

        ! 2. atmospheric temperature perturbations
        do lay=1,dimlevs
          coeff=0.5_pm_reel* &
               (1.-tanh(6._pm_reel*(sheight(lay)-T_noise_mid)/T_noise_delta)) 
          temp_gcm_noise(lay)=T_noise_level*coeff*temp_gcm_noise_dev
!          write(*,*) 'l=',lay,'sheight(l)=',sheight(lay),
!     &                 'temp_gcm_noise(l)=',temp_gcm_noise(lay)
        enddo

        ! reset old_ps_moise_dev
        old_ps_noise_dev=ps_noise_dev
      endif ! of if (ps_noise_dev.ne.old_ps_noise_dev)

      end subroutine nearsurfacenoise


      subroutine heights(dset,xlat,xlon,hireskey,convkey, &
                        zradius,zareoid,zsurface,ier)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Purpose:
! =======
! heights is a subroutine that can be used to convert vertical coordinates
! expressed as distance to the center of the planet, height above the
! areoid (zero datum) and height above the local surface. Given any of these,
! this routine computes the other two, according to the "convkey" flag.
!
! This routine works at two distinct resolution, according to flag "hireskey".
! At low resolution, distances are computed with respect to GCM (64x48) grid.
! At high resolution, MOLA 32 pix/deg topography (and high resolution areoid,
! computed from mgm1025 spherical harmonics expansion) are used.
!
! The main routine call_mcd does such conversions; these are provided here
! for users interested in a light and fast tool to do just that.
!
! "Low resolution" procedure based on MCD v4.1 "calc_height" routine
!
! Returned status/error code 'ier' are identical to call_mcd ones:
!       ier=0:  No problem
!       ier=4:  Wrong value fpr input flag 'hireskey'
!       ier=7:  Wrong value for input latitude 'xlat'
!       ier=15: Could not open a datafile
!       ier=16: Failed loading data from a database file
!
!       ier=24: Wrong value for input switch 'convkey'
!
! Additionnaly, if an error occurs, zradius,zareoid and zsurface are
! are set to -999.0
!
! Author:
! ======
!        E. Millour 12/2006
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       implicit none

! Arguments:
!      Entrées
       character*(*), intent(in) :: dset ! Path to MCD datafiles
                          ! if an empty string (ie: dset='')
                          ! then default dset='MCD_DATA/' path is assumed
       real(pm_reel), intent(in) ::  xlon        ! East longitude (in degrees)
       real(pm_reel), intent(in) ::   xlat       ! North latitude (in degrees)
       integer, intent(in) :: hireskey  ! resolution flag (=0 GCM resolution;=1 high res)
       integer, intent(in) :: convkey   ! switch to choose which z variable is known
                          ! and used to find the other two:
               ! convkey=1 zradius is known, compute zareoid and zsurface
               ! convkey=2 zareoid is known, compute zradius and zsurface
               ! convkey=3 zsurface is known, compute zradius and zareoid
!      Entrées / Sorties
       real(pm_reel), intent(inout) :: zradius ! distance to center of planet (m)
       real(pm_reel), intent(inout) :: zareoid ! altitude above areoid (m)
       real(pm_reel), intent(inout) :: zsurface ! altitude above local surface (m)
!      Sorites
       integer, intent(out) :: ier ! status/error code (=0 if OK)

!     Variables locales
      integer :: lendataset ! length of 'dset' string
      character(len=140) ::  path ! path given by 'dset'
      character(len=140) ::  datafile ! full path to GCM 'mountain.nc' data file
      integer :: unet ! 'mounatin.nc' file ID
      character(len=16) ::  name ! variable name
      integer :: varid     ! variable ID
      integer :: ierr1,ierr2 ! to store NetCDF routine error codes

      real(pm_reel) :: lon  ! local value of longitude (degrees east)
      real(pm_reel) :: lat  ! local value of latitude (degrees north)
      
      integer :: dlon(4)  !index, along longitudes, of database points
      ! dummy initialization to get rid of compiler warnings
      data dlon /1,1,1,1/
      integer :: dlat(4)  !index, along latitudes, of database points
      ! dummy initialization to get rid of compiler warnings
      data dlat /1,1,1,1/
      real(pm_reel) :: t        !weight (normalized longitudinal distance to point 1)
      real(pm_reel) :: u        !weight (normalized latitudinal distance to point 1)
      data u /0_pm_reel/   ! dummy initialization to get rid of compiler warnings
      data t /0_pm_reel/   ! dummy initialization to get rid of compiler warnings
      real(pm_reel) ::   alon,alat
      integer :: iloop, jloop ! indices de boucle

      ! Valeurs lues par NETCDF en simple précision
      real :: orog(dimlon,dimlat)   ! to store GCM orography
      real :: areoid(dimlon,dimlat) ! to store GCM areoid
      save orog, areoid          ! saved for further use
      logical :: firstcall 
      data firstcall /.true./
      save firstcall

      real(pm_reel) :: xorog ! orography at position lon,lat
      real(pm_reel) :: xareoid ! distance to center of planet of areoid at position lon,lat
      
      
      ier=0

! 1. Check input arguments
! 1.1 Check latitude
      if (abs(xlat).gt.90.0_pm_reel) then
        if (output_messages) then
         write(out,*)'HEIGHTS Error: wrong value for latitude=',xlat
        endif
        ier=7
        goto 999
      else
        lat=xlat
      endif

! 1.2 Set longitude
      ! At GCM resolution, longitude must be in [-180:180]
      lon=mod(xlon,360.0_pm_reel)
      if (lon.lt.-180._pm_reel) then ! in case lon in [-360:-180]
        lon=lon+360._pm_reel
      endif
      if (lon.gt.180.0_pm_reel) then ! in case lon in [180:360]
        lon=lon-360._pm_reel
      endif
      ! Note: at high resolution, 'mola' subroutine
      ! needs longitude to be in [0:360] but handles the conversion

! 1.3 Check hireskey
      if ((hireskey.lt.0).or.(hireskey.gt.1)) then
        if (output_messages) then
         write(out,*)'HEIGHTS Error: wrong value for parameter hireskey'
         write(out,*)'               hireskey=',hireskey
        endif
        ier=4
        goto 999
      endif

! 1.4 Check convkey
      if ((convkey.lt.1).or.(convkey.gt.3)) then
        if (output_messages) then
          write(out,*)'HEIGHTS Error: wrong value for parameter convkey'
          write(out,*)'               convkey=',convkey
        endif
        ier=24
        goto 999
      endif

! 1.5 set 'dset' to default 'MCD_DATA' , if necessary
!     find last non-blank character and length of 'dset' srting
      do lendataset=len(dset),1,-1
         if (dset(lendataset:lendataset).ne.' ') goto 100
      enddo
  100 continue

      if (lendataset.eq.0) then ! 'dset' not specified; set default path
        path='MCD_DATA/'
        lendataset=9
      else
        path=dset
      endif

! 2. If first call and low resolution, load GCM orography and areoid
!    from 'mountain.nc' file
      if (firstcall .and.(hireskey.eq.0)) then
        ! Initialisation des champs lus par NetCDF
        varid = 0
        unet = 0
        do jloop = 1, dimlat
           do iloop = 1, dimlon
              orog(iloop, jloop) = 0.
              areoid(iloop, jloop) = 0.
           end do
        end do

        datafile=path(1:lendataset)//'mountain.nc'
        ! open datafile
        ier=NF_OPEN(datafile,NF_NOWRITE,unet)
        if (ier.ne.0) then
          ier=15
          if (output_messages) then
            write(out,*)'HEIGHTS Error: cannot open file ',datafile
          endif
          goto 999
        endif
        
        ! load orography from file
        name='orography'
        ierr1=NF_INQ_VARID(unet,name,varid)
        ierr2=NF_GET_VAR_REAL(unet,varid,orog)
        if ((ierr1.ne.NF_NOERR).or.(ierr2.ne.NF_NOERR)) then
          ier=16
          if (output_messages) then
            write(out,*)'HEIGHTS Error: failed loading ',name
          endif
          goto 999
        endif
        
        ! load areoid from file
        name='areoid'
        ierr1=NF_INQ_VARID(unet,name,varid)
        ierr2=NF_GET_VAR_REAL(unet,varid,areoid)
        if ((ierr1.ne.NF_NOERR).or.(ierr2.ne.NF_NOERR)) then
          ier=16
          if (output_messages) then
            write(out,*)'HEIGHTS Error: failed loading ',name
          endif
          goto 999
        endif
        
        firstcall=.false.
      endif

! 3. get values of orography and areoid at position lon,lat
      if (hireskey.eq.0) then ! low resolution
      ! find longitude of four nearest points (as in grid4 routine)
        if (lon.ge.lonmax) then
         dlon(1)=dimlon
         dlon(2)=1
         dlon(3)=1
         dlon(4)=dimlon
         t=(lon-lonmax)/deltalon
        else
         alon=lonmin
         do iloop=1,dimlon-1
            if ((lon.ge.alon).and.(lon.lt.alon+deltalon)) then
               dlon(4)=iloop
               dlon(3)=iloop+1
               dlon(1)=iloop
               dlon(2)=iloop+1
               t=(lon-alon)/deltalon
            endif
            alon=alon+deltalon
         enddo
        endif
      ! find latitude of four nearest points (as in grid4)
        if (lat.ge.latmax) then
         dlat(4)=1
         dlat(3)=1
         dlat(1)=1
         dlat(2)=1
         u=0._pm_reel
        elseif (lat.le.latmin) then
         dlat(4)=dimlat
         dlat(3)=dimlat
         dlat(1)=dimlat
         dlat(2)=dimlat
         u=0._pm_reel
        else
         alat=latmin
         do iloop=dimlat,2,-1
            if ((lat.ge.alat).and.(lat.lt.alat+deltalat)) then
               dlat(1)=iloop
               dlat(2)=iloop
               dlat(3)=iloop-1
               dlat(4)=iloop-1
               u=(lat-alat)/deltalat
            endif
            alat=alat+deltalat
         enddo       
        endif
      
      ! bilinear interpolation to find local orography and areoid
        xorog=(1._pm_reel-t)*(1._pm_reel-u)*real(orog(dlon(1),dlat(1)), kind=pm_reel) &
             +t*(1._pm_reel-u)*real(orog(dlon(2),dlat(2)), kind=pm_reel) &
             +t*u*real(orog(dlon(3),dlat(3)), kind=pm_reel) &
             +(1._pm_reel-t)*u*real(orog(dlon(4),dlat(4)), kind=pm_reel)
 
        xareoid=(1._pm_reel-t)*(1._pm_reel-u)*real(areoid(dlon(1),dlat(1)), kind=pm_reel) &
             +t*(1._pm_reel-u)*real(areoid(dlon(2),dlat(2)), kind=pm_reel) &
             +t*u*real(areoid(dlon(3),dlat(3)), kind=pm_reel) &
             +(1._pm_reel-t)*u*real(areoid(dlon(4),dlat(4)), kind=pm_reel)

      else ! high resolution mode
        ! orography interpolated from MOLA file
        call mola(path(1:lendataset),lat,lon,xorog,ier)
        if (ier.ne.0) then
          goto 999 ! ier is set in 'mola' and error message is issued there
        endif
        
        ! areoid computed from mgm1025 spherical harminics expansion
        call molareoid(path(1:lendataset),lon,lat,xareoid)
        if (ier.ne.0) then
          goto 999 ! ier is set in 'molareoid' and error message is issued there
        endif
        
      endif ! of if (hireskey.eq.0)
      
! 4. convert between heights
      if (convkey.eq.1) then
         zareoid=zradius-xareoid
         zsurface=zareoid-xorog
      else if (convkey.eq.2) then
         zradius = zareoid+xareoid
         zsurface = zareoid-xorog
      else ! convkey.eq.3
         zareoid=zsurface+xorog
         zradius=zareoid+xareoid
      endif

      return

!     error handling
  999 zradius=-999.0_pm_reel
      zareoid=-999.0_pm_reel
      zsurface=-999.0_pm_reel
      return
      
      end subroutine heights

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mola(dset,latitude,longitude,alt,ierr)
!Give the MOLA altitude (alt), given lat and lon coordinates
!Using bilinear interpolation from 32 pixels/degree MOLA file

      implicit none

! Arguments
!     Entrées
      character*(*), intent(in) :: dset ! Path to MCD datafiles 
      real(pm_reel), intent(in) ::  latitude  ! north latitude (degrees)
      real(pm_reel), intent(in) ::  longitude ! east longitude (degrees)

!     Sorties
      real(pm_reel), intent(out) ::  alt     ! above areoid altitude of surface (meters)
      integer, intent(out) :: ierr ! returned status code (==0 if OK)

! Local variables

      logical firstcall
      data firstcall/.true./
      
      save firstcall
      character(len=140) ::  molafile ! MOLA datafile 
!      data molafile/'mola_32.2.nc'/
!      data molafile/'mola16.nc'/
!      data molafile/'mola32.nc'/
      integer :: resol
!      parameter(resol=16) ! MOLA pixel/degree resolution
!     Note de développement : passage en réel car indice de boucle
      parameter(resol=32)
      integer :: jjm, iim   ! # of longitude and latitude MOLA data values
      parameter(jjm=180*resol, iim=2*jjm)
      integer*2 :: altmola(iim,jjm) ! MOLA altitude dataset
      save altmola

      integer*2 :: mintopo_check,maxtopo_check ! known min and max of MOLA dataset
      parameter(mintopo_check=-8206,maxtopo_check=21191) ! mola32.nc
      real(pm_reel) :: dlat, dlon 
      integer :: iloop, jloop ! indices de boucle
      real(pm_reel) :: topo(4) ! neighboring MOLA points (for bilinear interpolation)
      integer :: latsup,latinf,loninf,lonsup ! indexes of neighboring points
      integer*2 :: mintopo, maxtopo ! min and max of read dataset
      integer :: nid,nvarid ! NetCDF file and variable IDs
      real(pm_reel) :: colat ! colatitude
      real(pm_reel) :: lat,lon ! longitude and latitude, local values (in degrees)

! 1. Load MOLA dataset upon first call
!    (dataset is stored in a 'save' array)

      if (firstcall) then
         firstcall=.false.

! 0. Initialisation des champs lus par NetCDF

        nid = 0
        nvarid = 0

        do jloop = 1,jjm
           do iloop = 1,iim
              altmola(iloop, jloop) = 0
           end do
        end do

! 1.1. Open MOLA file
         molafile=dset//'mola32.nc'
!         if (output_messages) then
!           write(out,*)"Loading MOLA topography from file "
!           write(out,*) molafile
!         endif
         ierr = NF_OPEN (molafile, NF_NOWRITE,nid)
         if (ierr.NE.NF_NOERR) then
            if (output_messages) then
              write(out,*) "Error in mola: Could not open file "
              write(out,*) molafile
            endif
            ierr=15 !set appropriate error code
            return
         endif

! 1.2. Load data
         ierr = NF_INQ_VARID (nid, "alt", nvarid)
	 ! note that MOLA "alt" are given as "short" (16 bits integers)
         ierr = NF_GET_VAR_INT2(nid, nvarid, altmola)
         if (ierr.ne.NF_NOERR) then
           if (output_messages) then
            write(out,*)"Error in mola: <alt> not found"
           endif
           ierr=16 ! set appropriate error code
           return
         endif


! 1.3. Close MOLA file
         ierr=NF_CLOSE(nid)

! 1.4 Check that the MOLA dataset was correctly loaded

         mintopo=mintopo_check
         maxtopo=maxtopo_check
         do iloop=1,iim
            do jloop=1,jjm
               mintopo=min(mintopo,altmola(iloop,jloop))
               maxtopo=max(maxtopo,altmola(iloop,jloop))
            enddo
         enddo
         if ((mintopo.ne.mintopo_check).or. &
             (maxtopo.ne.maxtopo_check)) then
           if (output_messages) then
            write(out,*)"***ERROR Mola file ",molafile, &
                        " is not well read"
            write(out,*) "Minimum found: ", mintopo
            write(out,*) "Minimum should be:",mintopo_check
            write(out,*) "Maximum found: ", maxtopo
            write(out,*) "Maximum should be:",maxtopo_check
           endif
           ierr=16
           return
         endif
 !        if (output_messages) then
 !          write(out,*) "Done reading MOLA data"
 !        endif
      endif ! End of if(firstcall)

! 2. Check that input longitude and latitude make sense
      lat=latitude
      if((lat.gt.90_pm_reel).or.(lat.lt.-90_pm_reel)) then
        if (output_messages) then
          write(out,*)"Error in mola: Wrong value for latitude"
        endif
        stop
      endif

! longitude must range from 0 to 360
      lon=longitude
      do while(lon.gt.360._pm_reel)
        lon=lon-360._pm_reel
      enddo
      
      do while(lon.lt.0._pm_reel)
        lon=lon+360._pm_reel
      enddo
      
! 3. Identify the four neighboring points from MOLA dataset
!    These points are arranged as follows:  1 2
!                                           3 4

      colat=90._pm_reel-lat

      if (colat.lt.1._pm_reel/(2._pm_reel*resol)) then
         latsup=1 
         latinf=1
         dlat=0
      else if (colat.gt.180._pm_reel-1._pm_reel/(2._pm_reel*resol)) then
         latsup=jjm
         latinf=jjm
         dlat=0
      else
         latsup=1+int((colat-1._pm_reel/(2._pm_reel*resol))*resol)
         latinf=latsup+1
         dlat=1-(colat-(1._pm_reel/(2._pm_reel*resol)+(latsup-1._pm_reel)/resol))*resol
      endif
! Note: dlat is the (normalized) "latitudinal distance" to point 3 
!       ie: dlat=0 if lat=latitude of point 3
!           dlat=1 if lat=latitude of point 1

      if ((lon.lt.1._pm_reel/(2._pm_reel*resol)).or.(lon.ge.(360_pm_reel-1._pm_reel/(2._pm_reel*resol)))) then
         loninf=iim
         lonsup=1
         if (lon.lt.1._pm_reel/(2._pm_reel*resol)) then
            dlon=lon*resol+0.5_pm_reel
         else
            dlon=(lon-(1._pm_reel/(2._pm_reel*resol)+(loninf-1._pm_reel)/resol))*resol
         endif
      else
         if (((lon-1._pm_reel/(2._pm_reel*resol))*resol).ge.0_pm_reel) then
            loninf=1+int((lon-1._pm_reel/(2._pm_reel*resol))*resol)
         else
            loninf=int((lon-1._pm_reel/(2._pm_reel*resol))*resol)
         endif
         lonsup=loninf+1
         dlon=(lon-(1._pm_reel/(2._pm_reel*resol)+(loninf-1._pm_reel)/resol))*resol
      endif
! Note: dlon is the (normalized) "longitudinal distance" to point 3
!       ie: dlon=0 if lon=longitude of point 3
!           dlon=1 if lon=longitude of point 4

      if((dlat.gt.1_pm_reel).or.(dlon.gt.1_pm_reel).or.(dlat.lt.0_pm_reel).or.(dlon.lt.0_pm_reel)) then
        if (output_messages) then
         write(out,*)"Error in mola: on dlat or dlon" 
         write(out,*) "dlat: ",dlat
         write(out,*) "lat: ",lat
         write(out,*) "dlon: ",dlon
         write(out,*) "lon: ",lon
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

      alt=(1._pm_reel-dlon)*(1._pm_reel-dlat)*topo(3)+(1._pm_reel-dlat)*dlon*topo(4) & 
           +dlat*(1._pm_reel-dlon)*topo(1)+dlat*dlon*topo(2)

      end subroutine mola

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
	
        
!       Entrées
        character*(*), intent(in) :: dset ! path to datafiles
        real(pm_reel), intent(in) ::  lon ! East longitude (degrees)
        real(pm_reel), intent(in) ::  lat ! North latitude (degrees)
!       Sorties
        real(pm_reel), intent(out) ::  rareoid ! distance (in m) of areoid to center of Mars
        
! COMMON: (shared with readcs.F and geoid.F)
        integer :: ndeg,ndeg2,nd2p3
	parameter (ndeg=90,ndeg2=2*ndeg,nd2p3=ndeg2+3)
! data structure of gravity field
        real(pm_reel) :: v0,omega,ae,gm
	real(pm_reel) :: clm(0:ndeg,0:ndeg),slm(0:ndeg,0:ndeg)
	common /gmm1/v0,omega,ae,gm,clm,slm
        integer :: lmin,lmax
        real(pm_reel) :: root(nd2p3)
        real(pm_reel) :: requator
	common /sqr/ lmin,lmax,root,requator

	character(len=140) ::  mgm     ! gravity field coefficients file
	integer :: llmax
        data llmax /50/
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
            write(out,*)'MOLAREOID Error: Gravity file in wrong format!'
            write(out,*)' lmin,max=',lmin,lmax
           endif
           stop
          endif
        firstcall=.false.
        endif ! of if (firstcall)

        call geoid(lon,lat,rareoid)
	
	end subroutine molareoid


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        subroutine readcs(mgm)

        implicit none


!       Entrées
        character*(*), intent(in) :: mgm ! file to read coefficients from

!       Variables locales
	character(len=64) ::  title ! first line of file
	character(len=6) ::  gcoef
! data structure of gravity field
        integer :: ndeg,ndeg2,nd2p3
	parameter (ndeg=90,ndeg2=2*ndeg,nd2p3=ndeg2+3)
	real(pm_reel) :: c(0:ndeg,0:ndeg),s(0:ndeg,0:ndeg)
	real(pm_reel) ::  v0,omega,ae,gm !,clm,slm
	real(pm_reel) ::  plm(ndeg)
	common /gmm1/ v0,omega,ae,gm,c,s
	integer :: lmin,lmax
        real(pm_reel) :: root(nd2p3)
        real(pm_reel) :: r
        common /sqr/ lmin,lmax, root,r
!
        integer :: lcmax,mmax
        integer :: k,l,m
        real(pm_reel) :: coef
        real(pm_reel) :: x,xi,sum
        
	  ae= 3396000._pm_reel
	  gm =42828.37e9_pm_reel
	  omega=0.70882181e-4_pm_reel
! this value makes the equatorial mean radius equal to 3396 km.
	do k=1,nd2p3
	 root(k)=sqrt(real(k, kind=pm_reel))
	enddo
!  initialize
	do l=0, ndeg
	 do m=0,l
	  c(l,m)=0._pm_reel
	  s(l,m)=0._pm_reel
	 enddo
	enddo
	open(unit=11,file=mgm,status='old',err=999)
        if (output_messages) then
!          write(out,*) 'Loading gravity field coefficients from file '
!          write(out,*) mgm
        endif 
!	write(*,'(a,a)')' GCOEF potential model: ',mgm
	read (11,'(a)') title
!	write(out,*) title
	read (11,1000) gcoef,lcmax,mmax,gm,ae
!	write(out,*) gcoef,lcmax,mmax,gm,ae
!	write(out,*)
1	continue
	read(11,1000,end=99) gcoef,l,m,coef
1000   format(a6,8x,i3,i3,d24.14,d15.9)

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
	r = 3396000._pm_reel !  MOLA potential surface
	xi = ae/r
	m=0
	x=0._pm_reel
	sum=0._pm_reel
	call lgndr(lmax,m,x,plm,root)
	do l=2, lmax
	  sum = sum + c(l,m) *xi**l *plm(l-m+1)
	enddo
	sum = sum+1._pm_reel
! centrifugal potential	
	v0=(gm*sum/r + 0.5_pm_reel*omega**2 * r**2)
!	write(out,*)'v0=', v0
	return

999	continue
        if (output_messages) then
	 write(out,*)'READCS Error: file not found or incorrect data',lmax
         write(out,*)'title: ',title
         write(out,*)'gcoef,lcmax,mmax,gm,ae:',gcoef,lcmax,mmax,gm,ae
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

!       Entrées
        real(pm_reel), intent(in) :: dlon,dlat
!       Sortie
        real(pm_reel), intent(out) :: rg
        
!       Variables locales
        real(pm_reel) :: pi,d2r
	parameter (pi=3.141592653589792_pm_reel, d2r=pi/180._pm_reel)
! data structure of gravity field
        integer :: ndeg,ndeg1,ndeg2,nd2p3
	parameter (ndeg=90,ndeg1=ndeg+1,ndeg2=2*ndeg,nd2p3=ndeg2+3)
        real(pm_reel) ::  v0,omega,ae,gm
	real(pm_reel) ::  c(0:ndeg,0:ndeg),s(0:ndeg,0:ndeg)
	common /gmm1/v0,omega,ae,gm,c,s
        integer ::  lmin,lmax
        real(pm_reel) ::  root(nd2p3)
        real(pm_reel) ::  r
	common /sqr/ lmin,lmax,root,r
	real(pm_reel) ::  plm(ndeg1)
	real(pm_reel) ::  tol
        data tol /0.125_pm_reel/  ! tolerence on computed value of rg

        integer :: i,m,l
        real(pm_reel) ::  rlon,rlat
        real(pm_reel) ::  x,cslt,xi,sum,cslm
        real(pm_reel) ::  diff
! save r
	rg = r

	rlon=dlon*d2r
	rlat=dlat*d2r
	x = sin(rlat)
	cslt= cos(rlat)

 	do i=1,8 ! usually 3 iterations suffice

	  xi = ae/r
	  sum = 0._pm_reel
	  do m=0, lmax
	   call lgndr(lmax,m,x,plm,root)
	   do l=m, lmax
	     cslm =  c(l,m)*cos(m*rlon) + s(l,m)*sin(m*rlon)
	     sum = sum + cslm*xi**l *plm(l-m+1)
	   enddo
	  enddo
	  sum=sum+1._pm_reel
! centrifugal potential	
	  rg=(gm*sum+0.5_pm_reel*(omega**2)*(r**3)*(cslt**2))/v0
!          write(out,*) i,r,rg
          diff = r-rg
          r = rg
          if( abs(diff).lt. tol) goto 400
	enddo ! do i=1,8

 400	continue
!	write(*,500) dlon,dlat,r
!500	format(2f9.2, f12.3)
	end subroutine geoid

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine lgndr(lmax,m,x,plm,sqr)

! Return vector of plm's of degree from m to lmax, for order m
      implicit none

!     Entrées
      integer, intent(in) :: lmax
      integer, intent(in) :: m
      real(pm_reel), intent(in) :: x
      real(pm_reel), intent(in) :: sqr(*)
      real(pm_reel), intent(out) :: plm(*)

!     Variables locales
      integer :: ifact,i,ll
      real(pm_reel) :: om, pmm, pmmp1, pll, somx2
!  needs sqrt of integers from 1 to 2*L+3
!  SIGN MODIF. OF NUM.REC. ALGORITHM, TO MATCH GEOPHYS. CONVENTION.
!  AND NORMALIZATION TO MEAN SQUARE UNITY.

      pmmp1=0_pm_reel !dummy initialization to get rid of compiler warnings

      om = 1._pm_reel
      pmm=sqr(2*m+1)
      if(m.gt.0) then
        pmm=sqr(2)*pmm
        somx2=sqrt((1._pm_reel-x)*(1._pm_reel+x))
        ifact=1
        do i=1,m
          om=-om
          pmm=-pmm*(sqr(ifact)/sqr(ifact+1))*somx2
          ifact=ifact+2
        enddo
      endif
      plm(1)=om*pmm
      if(lmax.gt.m) then
        pmmp1=x*sqr(2*m+3)*pmm
        plm(2)=om*pmmp1
      endif
      if (lmax.gt.m+1) then
         do ll=m+2,lmax
	   pll=sqr(2*ll+1)*sqr(2*ll-1)/(sqr(ll+m)*sqr(ll-m)) &
      * (x*pmmp1-pmm*sqr(ll+m-1)*sqr(ll-m-1)/(sqr(2*ll-1)*sqr(2*ll-3)))
           plm(ll-m+1)=om*pll
           pmm=pmmp1
           pmmp1=pll
         enddo
      endif
    end subroutine lgndr

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Routine de calcul d'un jour julien a partir du jour calendaire
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine julian(month,day,year,hour,minute,second,ierr, &
                date)


      implicit none
!
!     Given Earth date and time compute and local time on Mars
!     Updated version by B. Dolla and F. Forget, 2005
!     Entrées
!
      integer, intent(in) :: month   
      integer, intent(in) :: day
      integer, intent(in) :: year
      integer, intent(in) :: hour       !All Earth GMT values
      integer, intent(in) :: minute
      integer, intent(in) :: second
!
!     Sorties
!
      integer, intent(out) :: ierr       !0 if ok >0 if there is a problem
      real(pm_reel), intent(out) ::  date       !Julian date
!
!     Local
!
      integer :: nday
      integer :: daynumber(12)   !days for months of the year
      data    daynumber/0,31,59,90,120,151,181,212,243,273,304,334/
      integer :: jul
      integer :: j
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
      if (year.lt.1582) jul=1
      if ((year.eq.1582).and.(month.lt.10)) jul=1
      if ((year.eq.1582).and.(month.eq.10).and.(day.lt.15)) jul=1
      
      if (jul.eq.0) then
         if ((mod(year,4).eq.0).and.(mod(year,100).ne.0) &
             .and.(month.gt.2)) nday=nday+1
         if ((mod(year,400).eq.0) &
          .and.(month.gt.2)) nday=nday+1
      endif
      if (jul.eq.1) then
         if ((mod(year,4).eq.0).and.(month.gt.2)) nday=nday+1
         nday=nday+10
      endif
!
!     we use 1968 as a year of reference, the julian date being 2.4398565e6
!
      if (year.gt.1968) then
         do j=1968,year-1,1
            nday=nday+365
            if ((mod(j,4).eq.0).and.(mod(j,100).ne.0)) nday=nday+1
            if (mod(j,400).eq.0) nday=nday+1
         enddo
      endif

      if (year.lt.1968) then
         jul=1
         do j=year,1967,1
            if (j.gt.1581) jul=0
            if (jul.eq.0) then
               nday=nday-365
               if ((mod(j,4).eq.0).and.(mod(j,100).ne.0)) nday=nday-1
               if (mod(j,400).eq.0) nday=nday-1
            endif
            if (jul.eq.1) then
               nday=nday-365
               if (mod(j,4).eq.0) nday=nday-1
            endif
         enddo
      endif
!
!     Compute Julian date
!
      date=2.4398565e6_pm_reel+nday
      date=date+hour/24.0_pm_reel+minute/1.440e3_pm_reel+second/8.6400e4_pm_reel
!
      return
    end subroutine julian



    subroutine cps_atmemcd_431_close ()
!***********************************************************************
!$<AM-V2.0>                                                            *
!$Type                                                                 *
!     Subroutine                                                       *
!$Nom                                                                  *
!     cps_atmemcd_431_close                                             *
!                                                                      *
!$Resume                                                               *
!      routine de desallocation memoire des tableaux de grandes        *
!      dimensions                                                      *
!$Auteur                                                               *
!      Cédric MARTEL    (ATOS Origin)                                  *
!$Usage                                                                *
!     call cps_atmemcd_431_close()                                      *
!                                                                      *
!$<>                                                                   *
!***********************************************************************

      implicit none

      ! Variable pour la gestion d'erreur de deallocate
      integer :: ierralloc

      if (modele_init) then

         ! Desallocation si besoin est
         if ( associated(var_2d) )     deallocate(var_2d,  stat=ierralloc)
         if ( associated(var_2d2) )    deallocate(var_2d2,  stat=ierralloc)

         if ( associated(var_3d) )     deallocate(var_3d,  stat=ierralloc)
         if ( associated(var_3d2) )    deallocate(var_3d2,  stat=ierralloc)

         if ( associated(varrms2d) )   deallocate(varrms2d,  stat=ierralloc)
         if ( associated(varrms2d2) )  deallocate(varrms2d2,  stat=ierralloc)
         if ( associated(varrms3d) )   deallocate(varrms3d,  stat=ierralloc)
         if ( associated(varrms3d2) )  deallocate(varrms3d2,  stat=ierralloc)


         if ( associated(vararms3d) )  deallocate(vararms3d,  stat=ierralloc)
         if ( associated(vararms3d2) ) deallocate(vararms3d2,  stat=ierralloc)


         if ( associated(tabpc) )      deallocate(tabpc,  stat=ierralloc)
         if ( associated(tabpcsmth) )  deallocate(tabpcsmth,  stat=ierralloc)
         if ( associated(tabeops) )    deallocate(tabeops,  stat=ierralloc)
         if ( associated(tabeot) )     deallocate(tabeot,  stat=ierralloc)
         if ( associated(tabeou) )     deallocate(tabeou,  stat=ierralloc)
         if ( associated(tabeov) )     deallocate(tabeov,  stat=ierralloc)

         ! Remise a zéro du flag
         modele_init = .false.
      endif

    end subroutine cps_atmemcd_431_close

    end module cps_modele_emcd431
