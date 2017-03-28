module cps_atm_gram05

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_gram
!
!$Resume
!  Calcul de variables météorologiques avec le modèle MarsGRAM 05
!
!$Description
!  Calcul de variables météorologiques avec le modèle MarsGRAM 05
!  à partir des coordonées géodésiques date, lat, lon, hauteur.
!  Le modèle fonctionne par deltas sur la position. D'autre part
!  il offre la possibilité d'obtenir des perturbations sur les 
!  les grandeurs météorologique. 
!
!$Auteur
!  Jean-Luc ROBIN (ATOS)
!
!$Version
!  $Id: cps_atm_gram05.F90 393 2013-02-26 09:34:07Z ffsm $
!
!$Historique
!  $Log: cps_atm_gram05.F90,v $
!  Revision 393  2013/02/26 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.5  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.4  2010/10/11 08:11:52  ogarat
!  VERSION::FA-ID:1427:11/10/2010:Tests sur les parametres des perturbation uniquement si disp=1
!
!  Revision 1.3  2010/05/05 09:44:47  jlrobin
!  V2.9::DM-ID:1359:05/05/2010:portage vers LINUX du modele atm GRAM05
!
!  Revision 1.2  2010/04/30 14:12:37  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.1.2.9  2010/04/01 16:41:31  jlrobin
!  V2.9::DM-ID:461:01/04/2010:Integration avec CRASH
!
!  Revision 1.1.2.8  2010/03/29 07:57:58  jlrobin
!  V2.9::DM-ID:461:29/03/2010:Intgration avec CRASH
!
!  Revision 1.1.2.7  2010/03/18 15:39:32  jlrobin
!  V2.9::DM-ID:1359:18/03/2010:augmentation de la taille des paths
!
!  Revision 1.1.2.6  2010/03/15 10:36:46  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Initialisation de variables
!
!  Revision 1.1.2.5  2010/03/01 17:27:50  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Integration du modele d'atmosphere MARS GRAM 2005
!
!  Revision 1.1.2.4  2010/03/01 13:13:56  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Integration du modele d'atmosphere MARS GRAM 2005
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_gram
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atmmarsgram_05
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MSLIB
!- cps_utilisateur
!#
!
!$Interface
!
!$Mots-cles
!  ATMOSPHERE MARS GRAM2005
!
!$Voir-Aussi
!#V
!
!#
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! inclusions
  use MSLIB, only : PM_REEL
  use cps_utilisateur

! move from external to private
! Main file private declaration
  private Datastep_M05, Randinit_M05, Setup_M05
! Additional declaration from Setup file
  private RdProf_M05, RdTESmgcm_M05, RdTESsrf_M05
  private RdTEStgcm_M05, ReadMGCM_M05, ReadTES_M05, ReadTGCM_M05
  private Readsurf_M05, chkdt_M05, marsephm_M05
! Additional declaration from Marssub file
  private ATMOS2_M05, bltp_M05, CaltoJul_M05, cospar_M05, GeocenttoGeodet_M05
  private GeodettoGeocent_M05, Dustfact_M05, EScalc_M05, FourD_M05
  private MGCMterp_M05, MarsGCM_M05, perturb_M05, PRSEAS_M05
  private Rescale_M05, Shiftdif_M05, slopewind_M05, species_M05, SublTchk_M05
  private surfterp_M05, STEWART2_M05, ThreeD_M05, THERMOS_M05
  private topoareo_M05,  Thermpar_M05, TGCMterp_M05, TwoD_M05
  private Wavelon_M05, TESOD_M05
! Additional declaration from TESsubs_M05.F90 file
  private TESsrftrp_M05, TESTterp_M05, TESGCM_M055, ProfTerp_M05, TESGterp_M05
! Additional declaration from private pour les fonctions
  private ppnd_m05, ifloor_m05, random_m05, dustvsls_m05, cp_m05
  private qrhtp_m05, ttidey_m05, tidex_m05, tidey_m05, zlogr_m05 


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_gram05.F90 393 2013-02-26 09:34:07Z ffsm $'

interface cps_marsephm_M05
      module procedure marsephm_M05
end interface

interface cps_dustvsls_M05
     module procedure dustvsls_M05
end interface


contains

subroutine cps_atmmarsgram_05(r,xlat,xlon,xdate,dir,file, &
       init_atm,xmapyear,scena,dust,xdustmin,xdustmax,disp,seed,rpscale,&
       xrwscale,xwlscale,xwmscale,xcorlmin,z,pressure, &
       ro,temperature,windu,windv,meanvar,extvar,ier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!     cps_atmmarsgram_05
!
!$Resume
!     Calcul de variables météorologiques avec le modèle MarsGRAM 05
!
!$Description
!     Calcul de variables météorologiques avec le modèle MarsGRAM 05
!
!$Acces
!  PUBLIC
!
!$Auteur
!
!     Jean-Luc ROBIN (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!      call cps_atmmarsgram_05(r,xlat,xlon,xdate,dir,file, &
!         init_atm,xmapyear,scena,dust,xdustmin,xdustmax,disp,seed,rpscale,&
!         xrwscale,xwlscale,xwmscale,xcorlmin,z,pressure, &
!         ro,temperature,windu,windv,meanvar,extvar,ier)
!
!$Arguments
!>E r      : <PM_REEL> distance au centre de Mars (m)
!>E xlat   : <PM_REEL> latitude (rad)
!>E xlon   : <PM_REEL> Longitude East (rad)
!>E xdate  : <PM_REEL> date (jour julien Terrestre)
!>E dir    : <character*(*)>    Répertoire des données Mars-GRAM 2005 :
!                             par défaut celui-ci est distribué par COMPAS.
!. si les ressources COMPAS ne sont pas accessibles, recherche un lien "GRAM_DATA" sur ce
!  répertoire.
!>E file   : <character*(*)>    Fichier de configuration GRAM
!                 par défaut il s'agit du fichier input_gram (distribué par COMPAS)
!>E init_atm : <logical>        demande la réinitialisation du modèle.
!>E xmapyear : <integer>        Année de mesures pour les données
!.                              1 : Pour les mesures TES de l'année 1
!.                              2 : Pour les mesures TES de l'année 2 
!.                              0 : Sinon
!>E scena  : <integer>          Scénario de poussière :
!.       1 = épaisseur optique des poussières Viking (tau is calculé par le code)
!.       2 = tau spécifique  (tau est lu dans la variable <dust>)
!>E dust   : <PM_REEL> épaisseur optique des poussières
!.                             < 0 : Scénario de poussières Viking
!.                             > 0 (doit être dans l'intervalle [0.1-3.0])
!>E xdustmin:<PM_REEL> Profondeur optique des poussière minimum si dust=0 (>=0.1)
!>E xdustmax:<PM_REEL> Profondeur optique des poussère maximum si dust=0 (<=3.0)   
!>E disp   : <integer>         Type de perturbations
!.                             1 = atmosphère nominale
!.                             2 = atmosphère perturbée
!>E seed   : <integer>    Graine du générateur aléatoire pour le modèle de perturbation
!>E rpscale: <PM_REEL> Facteur multiplicatif d'echelle des perturbations de densité (>=0 et <=2)
!>E xrwscale:<PM_REEL> Facteur multiplicatif d'echelle des perturbations de vent (>=0)
!>E xwlscale:<PM_REEL> Facteur multiplicatif d'echelle des perturbations de longueur d'onde (>=0.1 et <=10)
!>E xwmcale: <PM_REEL> Facteur multiplicatif d'echelle des perturbations de vent moyen
!>E xcorlmin:<PM_REEL> La taille du pas minimum pour les perturbations (>=0 et <=1)
!
!>S z      : <PM_REEL> Hauteur / MOLA aréoïde  (m)
!>S pressure : <PM_REEL> pression (Pa)
!>S ro     : <PM_REEL> densité (kg/m^3)
!>S temperature : <PM_REEL> température (K)
!>S windu  : <PM_REEL> composant zonal du vent  (Est-Ouest) (m/s)
!>S windv  : <PM_REEL> composant méridional du vent (Nord-Sud) (m/s)
!>S meanvar : <PM_REEL> Tableau de dimension 5 avec le svaleurs moyennes :
!.                  meanvar(1)=  pression moyenne
!.                  meanvar(2)=  densité moyenne (sans ondes ni perturbations aléatoires)
!.                  meanvar(3)=  température moyenne
!.                  meanvar(4)=  composant moyen zonal du vent
!.                  meanvar(5)=  composant moyen méridional du vent 
!>S extvar : <PM_REEL> Tableau de 25 variables statistiques supplémentaires :
!.                 extvar(1) =  valeur maximum de la densité (kg/m^3)
!.                 extvar(2) =  valeur minimum de la densité (kg/m^3)
!.                 extvar(3) =  écart type sur la densité (kg/m^3)
!.                 extvar(4) =  perturbations sur la densité (wave +random) (kg/m^3)
!.                 extvar(5) = Hauteur à pression "pressure" H(p) (km)
!.                 extvar(6) = Hauteur à densité "rho" H(rho) (km)
!.                 extvar(7) = inutilisé
!.                 extvar(8) = inutilisé 
!.                 extvar(9) = inutilisé 
!.                 extvar(10)= inutilisé
!.                 extvar(11)= inutilisé 
!.                 extvar(12)= inutilisé 
!.                 extvar(13)= perturbation aléatoire (sans ondes wave pert.) (kg/m^3)
!.                 extvar(14)= hauteur orographique (m)
!.                 extvar(15)= inutilisé
!.                 extvar(16) = Test MAJ des perturbations et de leurs pas (1, 0 ou -1) 
!.                 extvar(17)= Angle Zénith Solaire (deg)
!.                 extvar(18)= Temps de parcours (Mars -> Terre) de la lumière (minutes) 
!.                 extvar(19)= Lattitude Sub-Solaire (deg) 
!.                 extvar(20)= Longitude Sub-Solaire (deg) 
!.                 extvar(21)= inutilisé 
!.                 extvar(22)= inutilisé 
!.                 extvar(23)= Longitude aréocentrique de Mars, Ls (deg)
!.                 extvar(24)= Heure solaire locale  (hrs)
!.                 extvar(25)= temps universel (hrs) (=heure locale à longitude=0)
!>S ier  : <integer> code retour 
!.                 0 = OK
!.                 1 = scénario inconnu
!.                 2 = type de perturbation inconnu
!.                 3 = objet souterrain
!.                 4 = lattitude dépasse 90deg ou -90deg
!
!$Remarques
!     Mars Global Reference Atmospheric Model 2005                     
!                  (Version 1.1) - October 2005                         
!                                                                      
!     A program to evaluate density, temperature, pressure and wind    
!     components at any given time and position in the atmosphere     
!     of Mars                                                     
!                                                                       
!     Mars-GRAM 2005 is not a US ITAR item.  Export of Mars-GRAM 2005   
!     is allowed under Export Control Classification EAR-99.           
!     However, no recipient of this code should forward copies outside  
!     the United States without explicit approval by NASA Marshall     
!     Space Flight Center.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

!.. Implicit Declarations .. 
  implicit none

! Variables Entrees/Sorties
      real(kind=PM_REEL),intent(inout) :: r,xlat,xlon,xdate,dust,rpscale,xdustmin,xdustmax
      real(kind=PM_REEL),intent(inout) :: xrwscale,xwlscale,xwmscale,xcorlmin
      integer,intent(inout) :: scena,disp,seed,xmapyear
      character*(*),intent(inout) :: dir, file
   !   logical,intent(inout) :: init_atm
      logical :: init_atm

      real(kind=PM_REEL),intent(out) :: z,pressure,ro,temperature,windu,windv
      real(kind=PM_REEL),intent(out) :: meanvar(5),extvar(25)
      integer,intent(out) :: ier

!Variables cps_RELLIPS_M05
      real(kind=PM_REEL) :: Lat, LonW, Rmola, Rell, topohgt, gz, alb

!Other Variables
      real(kind=PM_REEL) :: absheight
      real :: UTIME
      integer :: i_var

      real(kind=PM_REEL) :: crd, pi
      parameter (pi=3.14159265359d0)
      parameter (crd=180.d0/pi)

!** New arguments added to the original MarsGRAM Datastep subroutine **
      real(kind=PM_REEL) :: xDENS0,xSIG

!.. Local Scalars ..
  integer :: EOF,IERT,IUTC,LonEW,MAXNUM,NMONTE,NR1,iulist,iupdate, &
             iustdout,nprof,numwave
  real(kind=PM_REEL) :: ALS,DELHGT,DELLAT,DELLON, &
                      DELTIME,DENS,DENSHI,DENSLO,DENSP,DENSTOT,EWWIND,EWpert, &
                      Hpres,Hrho,MarsAU,NSWIND,NSpert,PRES,SZA,TEMP, &
                      TLOCAL,VWpert,corlim
  real(kind=PM_REEL) :: corlmin,hgtasfc,owlt,pertstep,proffar,profnear,sunlat, &
                      sunlon
  real(kind=PM_REEL),save :: FDHGT,FDLAT,FDLON,FDTIME,FHGT,FLAT,FLON,FSEC
  
! variables globales
  real(kind=PM_REEL),save :: CLAT,CHGT,CLON,CSEC,DAY0
  real(kind=PM_REEL),save :: RHOd,RHOu,RHOv,RHOw

! déclaration COMMON DATACOM_M05
  real(kind=PM_REEL) :: DAY,DTR,DUSTOD,DUSTDENS,DUSTDIAM,DUSTMAX,DUSTMIN,DUSTNU, &
                      DUSTTAU,RREF,ALS0,ALSDUR,BLWINFAC,DTEX,DUSTLAT, &
                      DUSTLON,INTENS,RADMAX,REQUA,RPOLE,RPFACTOR,RWSCALE, &
                      WLSCALE,WMSCALE
  integer :: MOLAHGTS,MAPYEAR,NPOS,NVARX,NVARY,IDAYDATA,IPCLAT,IU0,IUP, &
             LOGSCALE
  common /DATACOM_M05/ DTR,DAY,DUSTLAT,DUSTLON,RADMAX,RREF,ALS0,ALSDUR,INTENS, &
                       DTEX,RPFACTOR,DUSTTAU,DUSTMIN,DUSTMAX,DUSTOD,DUSTNU, &
                       DUSTDIAM,DUSTDENS,RWSCALE,WLSCALE,REQUA,RPOLE,WMSCALE, &
                       BLWINFAC,MAPYEAR,IDAYDATA,NPOS,NVARX,NVARY,LOGSCALE, &
                       IU0,IUP,IPCLAT,MOLAHGTS

  logical :: first_call
  data first_call/.true./

  ! 
  ! ... Executable Statements ...
  !  

! initialisation de l'erreur
    ier = 0

!----------------------
! first_call management
!----------------------
      if (first_call) then
          !if blank string, set default Data Directory
          !acces au repertoire contenant les fichiers du modele par COMPAS
          ier = cps_getFichierModele("atmosphere", "GRAM05", dir, &
              rep=.true.)
          if (MSP_gen_messages("atmmarsgram_05")) return
        
          !if blank string from file parameter and dir has been find out
          !extract file from the base ref
          !otherwise keep file sent thru parameter from above layer 
          if (index(file,' ').le.1.and.index(dir,' ').ge.1) &
               file = trim(dir)//'/inputstd_10.txt'

          !if blank string from file parameter and dir has not been find out
          !set to default file 
          if (index(file,' ').le.1.and.index(dir,' ').le.1) &
               file = 'inputstd_10.txt'

          !dir has not been find out: set to default dir
          if (index(dir,' ').le.1) dir = 'GRAM_DATA/'
  
          !set dir that refers to binary, dat and txt data for GRAM05
          dir = trim(dir) // "/MGbindat/"             

          !Initialize data and read input file with setup subroutine
          !See Mars-GRAM code for description of NAMELIST format input
          !file
          call Setup_M05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,RHOw,DELHGT,DELLAT, &
                 DELLON,DELTIME,MAXNUM,NR1,NMONTE,0.0d0,0.0d0,0.0d0,LonEW, &
                 file,iustdout,iulist,hgtasfc,IERT,IUTC,corlmin,profnear, &
                 proffar,nprof,dir)
          !...  Save initial position, date, and position displacement values  
          FHGT = CHGT
          FLAT = CLAT
          FLON = CLON
          FSEC = CSEC
          FDHGT = DELHGT
          FDLAT = DELLAT
          FDLON = DELLON
          FDTIME = DELTIME
          !...  Initialize total perturbation step                                
          pertstep = 0.0d0
          iupdate = 1
  
          first_call = .false.
          !Forced initialization of some parameters
          !NPOS = 1
          LonEW = 0
          MOLAhgts = 1
          numwave = 0
          IERT = 1
          IUTC = 1
      endif
      !endif first_call

!--------------
!Initialisation 
!--------------
          LonEW = 0
          IERT = 1
          IUTC = 1
          profnear = 0.0d0
          proffar = 0.0d0
          hgtasfc = 0.0d0
          nprof = 0
          pertstep = 0.0d0
          iupdate = 1

!-------------------
!Integrity Tests
!--------------------
!=== dust scenario within GRAM 2001 year
!check value of dust scenario Viking or customized
!insure dusttau value within proper range
 
  if (mapyear.eq.0) then

      ! if scenario calculé
      if (scena.eq.1) then
         Dusttau = 0._pm_reel
         !=== Dustmin, Dustmax 
         !insure Dustmin,Dustmax value within proper range
         if (dusttau.eq.0.0d0) then
            if (xdustmin.lt.0.1d0) then
              write(*,*)'ATMMARSGRAM Warning: Must have  dustmin>=1'
              write(*,*)'                     set to default = 0.3'
              dustmin = 0.3d0
            else
              dustmin = xdustmin
            endif
            if (xdustmax.gt.3.0d0) then
              write(*,*)'ATMMARSGRAM Warning: Must have  dustmax<=3.0'
              write(*,*)'                     set to default = 1.0'
              dustmax = 1.0d0
            else
              dustmax = xdustmax
            endif 
          endif !endif dusttau 
      
      ! if scenario personalisé
      else if (scena.eq.2) then
         if (dust.gt.0.d0) then
            if (dust.lt.0.1d0) then
               write(*,*)'ATMMARSGRAM Warning: Must have dusttau>=0.1'
               write(*,*)'                     set to 0.1'
               Dusttau = 0.1d0
            else if (dust.gt.3.d0) then
               write(*,*)'ATMMARSGRAM Warning: Must have dusttau<=3.0'
               write(*,*)'                     set to 3.0'
               Dusttau = 3.0d0
            else
               Dusttau = real(dust, kind=pm_reel)
            endif
         else
            write(*,*)'ATMMARSGRAM Warning: Must have positive dusttau'
            write(*,*)'                     set to default Viking tau'
            Dusttau = 0.
         endif ! endif dust
      else
         write(*,*)'ATMMARSGRAM Error: Unknown dust scenario'
         ier=1
         goto 9999
      endif ! endif scena
  
  endif !mapyear
 
!=== perturbations
      if ((disp.lt.1).OR.(disp.gt.2)) then
         write(*,*)'ATMMARSGRAM Error: Unknown perturbation type'
         ier=2
         goto 9999
      end if

!=== case nominal (without perturbation)
      if (disp.eq.1) then
         ! default value
         rpfactor = 1.d0               
         rwscale = 1.d0
         wlscale = 1.d0
         wmscale = xwmscale
         corlmin = 0.0d0

!=== case with perturbation
      else
         !=== multiplicative factor for density perturbations
         !insure rpscale value within proper range
         if ((rpscale.lt.0.d0).or.(rpscale.gt.2.d0)) then
            write(*,*)'ATMMARSGRAM Warning: Must have 0.<=rpscale<=2.'
            write(*,*)'                     set to default = 1.'
            rpfactor = 1.d0
         else
            rpfactor = real(rpscale, kind=pm_reel)
         endif
         
         !=== rwscale, wlscale, wmscale
         !=== multiplicative factor for wind perturbations 
         !insure rwscale value within proper range
         if (xrwscale.lt.0.d0) then
            write(*,*)'ATMMARSGRAM Warning: Must have rwscale>=0.'
            write(*,*)'                     set to default = 1.'
            rwscale = 1.d0
         else
            rwscale = xrwscale 
         endif
         
         !=== multiplicative factor for wavelenght perturbations 
         !insure wlscale value within proper range
         if ((xwlscale.lt.0.1d0).or.(xwlscale.gt.10.d0)) then
            write(*,*)'ATMMARSGRAM Warning: Must have  0.1<=wlscale<=10'
            write(*,*)'                     set to default = 1.'
            wlscale = 1.d0
         else
            wlscale = xwlscale
         endif
         
         !=== multiplicative factor for meanwind perturbations      
         wmscale = xwmscale
         
         !=== minimum relative step size for perturbation 
         !insure corlmin value within proper range
         if ((xcorlmin.lt.0.0d0).or.(xcorlmin.gt.1.d0)) then
            write(*,*)'ATMMARSGRAM Warning: Must have  0.<=corlmin<=1'
            write(*,*)'                     set to default = 0.0'
            corlmin = 0.0d0
         else
            corlmin = xcorlmin
         endif
         
      endif

!=== MAPYEAR 
!insure MAPYEAR value within proper range 
!if MAPYEAR is out of range then MAPYEAR refered to input file
      if ((xmapyear.eq.0).or.(xmapyear.eq.1).or.(xmapyear.eq.2)) then
          mapyear = xmapyear
      else 
          write(*,*)'ATMMARSGRAM Error: Unknown dust mapping year'
          ier=2
          goto 9999
      endif

!----------------------------------------
!convert latitude from radians to degrees
!----------------------------------------
      Lat = real(xlat*crd,kind=pm_reel)
      if ((Lat.gt.90d0).or.(Lat.lt.-90d0)) then
         write(*,*)'ATMMARSGRAM Error: Lattitude out of range <-90deg or >90deg'
         ier=4
         goto 9999
      endif         

!------------------------------------------------------------
!convert longitude (radians) East to longitude (degrees) West 
!between 0..360 degrees
!------------------------------------------------------------
      LonW = real(xlon*crd,kind=pm_reel)
      LonW = mod(LonW,360.d0)
      if(LonW.lt.0.) then
         LonW = - LonW
      elseif (LonW.gt.0.) then
         LonW = 360.d0 - LonW
      endif

!-----------------------------------------------------------------------
!Compute altitude wrt to aeroid
!     Rmola   : Mars areoid radius (km) from 1/2 by 1/2 degree MOLA data
!     Rell    : Mars radius of reference ellipsoid (km)
!     topohgt : Orographic height (km)
!     ( unused outputs
!         gz  : Acceleration of gravity including J2 and centrifugal terms
!         alb : Mars surface albedo )
!------------------------------------------------------------------------
     call RELLIPS_M05(Lat,LonW,Rmola,0.0d0,gz,Rell,topohgt,alb,REQUA, &
                     RPOLE)
     !Altitude z wrt MOLA areoid (m)
     z = r - dble(Rmola)*1.d3

     !check positive height above ground (m) else stop
     absheight = z - topohgt*1.d3
     if (absheight.lt.0.d0) then
         write(*,*)'ATMMARSGRAM Error: Underground object'
         ier=3
         goto 9999
     endif

!------------------------------------
! dispersion processing with seed/NR1
!------------------------------------      
      if (init_atm) then    
         if (disp.eq.1) then
            ! seed is ignored
            RHOd = 0.0d0
            RHOu = 0.0d0
            RHOv = 0.0d0
            RHOw = 0.0d0
         else if (disp.eq.2) then
            ! Warning if the seed number is outside [1 - 29999] and
            ! new seed max(mod(abs(seed),30000),1) is used
            if ((seed.LE.0) .OR. (seed.GE.30000)) then
               write(*,'("Seed number (",i5,") is outside prescribed range")') seed
               seed = max(mod(abs(seed),30000),1)
               write(*,'("Replaced by seed (",i5,")")') seed
            endif
            NR1 = seed 
            call Randinit_M05(0,NR1,RHOd,RHOu,RHOv,RHOw,iulist,iustdout)
            CHGT = FHGT
            CLAT = FLAT
            CLON = FLON
            CSEC = FSEC
            DELHGT = FDHGT
            DELLAT = FDLAT
            DELLON = FDLON
            DELTIME = FDTIME
            !...      Re-initialize total perturbation step       
            pertstep = 0.0d0
            iupdate = 1
         endif
         ! endif disp
     endif
     ! endif init_atm or pass
 
!---------------------------------------------------------------
! Call for Model processing steps by steps through Datastep_M05
!---------------------------------------------------------------
     if (init_atm) then
          ! Set current position and date at initial time
          ! activate if without MOLA: CHGT = real (r/1.d3, kind=pm_reel)
          CHGT = real(z/1.d3, kind=pm_reel)
          CLAT = Lat
          CLON = LonW
          CSEC = 0._pm_reel
          ! Initial Date (dble prec)
          DAY0 = xdate 
          call Datastep_M05(0,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,RHOw,EOF, &
                        DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS, &
                        DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,VWpert,Hrho, &
                        Hpres,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,LonEW,corlim, &
                        DENSTOT,numwave,hgtasfc,IERT,IUTC,pertstep,corlmin, &
                        iupdate,ALS,SZA,owlt,sunlat,sunlon,MarsAU,TLOCAL, &
                        profnear,proffar,nprof,xDENS0,xSIG)
          init_atm = .false.

     else
         !Compute displacement magnitude of new from previous position and date
         ! activate if without MOLA DELHGT = real(r/1.d3, kind=pm_reel) - CHGT
         DELHGT = real(z/1.d3,kind=pm_reel) - CHGT
         DELLAT = Lat - CLAT
         DELLON = LonW - CLON    
         ! activate if without date-sec DELTIME = xdate - DAY0
         DELTIME = real(((xdate*86400._pm_reel) - (DAY0*86400._pm_reel)), kind=pm_reel)
         DELTIME = DELTIME - CSEC    
         call Datastep_M05(1,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,RHOw,EOF, &
                        DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS, &
                        DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,VWpert,Hrho, &
                        Hpres,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,LonEW,corlim, &
                        DENSTOT,numwave,hgtasfc,IERT,IUTC,pertstep,corlmin, &
                        iupdate,ALS,SZA,owlt,sunlat,sunlon,MarsAU,TLOCAL, &
                        profnear,proffar,nprof,xDENS0,xSIG)
         init_atm = .false.
      endif
      !endif init_atm

      !----------------------------------------------------------------------
      !       Parameters passed as output from Datastep_M05 are:             
      !       TEMP   = temperature (K)                                      
      !       PRES   = pressure (N/m**2)                                    
      !       DENSLO = nominal low density (kg/m**3), approx. -1 sigma       
      !       DENS   = mean density (kg/m**3)                                
      !       DENSHI = nominal high density (kg/m**3), approx. +1 sigma     
      !       DENSP  = density perturbation (% of unperturbed mean)         
      !       EWWIND = mean eastward wind component (m/s)                   
      !       EWpert = eastward wind component perturbation (m/s)          
      !       NSWIND = mean northward wind component (m/s)                  
      !       NSpert = northward wind component perturbation (m/s)           
      !       Hrho   = density scale height (km)                            
      !       Hpres  = pressure scale height (km)                            
      !       corlim = ratio of step size to minimum step size for assured   
      !                accuracy in perturbations (should be >= 1)             
      !       DENSTOT= total density (mean plus perturbed), kg/m**3          
      !       iupdate  = 1 if perturbations updated, 0 if perturbations not  
      !                     updated but perturbation step updated, -1 if      
      !                     neither perturbations nor step updated            
      !       ALS    = Planeto-centric longitude of Sun (Ls, degrees)       
      !       SZA    = Solar zenith angle (degrees)                          
      !       owlt   = Mars-Earth one-way light time (minutes)             
      !       sunlat = Sub-solar latitude (degrees)                        
      !       sunlon = Sub-solar longitude (degrees)                        
      !       MarsAU = Mars orbital radius (AU)                            
      !       TLOCAL = Local Solar rime (Mars hours)                       
      !                                                                    
      !       In addition to being passed to other routines, these          
      !       parameters may also be written out here.                    
      !                                                                     
      !       Optional high resolution ephemeris inputs are:                
      !         dsunlat = latitude of sub-solar point (deg)                
      !         dsunlon = longitude of sub-solar point (deg)                 
      !         dsunLs  = solar Ls angle (deg)                               
      !         dradau  = Mars orbital radius (AU)                          
      !         dowlt   = Earth-Mars one-way light-time (minutes)             
      !       Values of 0.0D0, used here, cause use of the internal epheremis
      !       subroutine to compute these parameters                        
      !-----------------------------------------------------------------   
 
!--------------------------     
! Ajout des sorties GRAM_05
!--------------------------
      pressure = dble(PRES)
      temperature = dble(TEMP)

      if (disp.eq.1) then
         ro = dble(DENS)
         windu = dble(EWWIND)
         windv = dble(NSWIND)
      else
         ro = dble(DENSTOT)
         !ro = dble(DENS*(1.+0.01*DENSP))
         windu = dble(EWWIND+EWpert)
         windv = dble(NSWIND+NSpert)
      endif

!store mean values
      meanvar(1) = pres
      meanvar(2) = dble(xDENS0)
      meanvar(3) = temperature
      meanvar(4) = dble(EWWIND)
      meanvar(5) = dble(NSWIND)

!store values of extra variables
      extvar(1) = dble(DENSHI)
      extvar(2) = dble(DENSLO)
      !density standard deviation (kg/m^3)
      extvar(3) = dble(xSIG/100.*xDENS0)
      extvar(4) = dble(DENSP/100.*DENS)
      extvar(5) = dble(Hpres)
      extvar(6) = dble(Hrho)

      extvar(7) = -999.0d0
      extvar(8) = -999.0d0
      extvar(9) = -999.0d0

! deactivate TSURF which does not exist in GRAM05
! extvar(10) = dble(TSURF)
      extvar(11) = -999.0d0
      extvar(12) = -999.0d0

      extvar(13) = dble(DENSTOT-DENS)

!Orographic Height (m)
      extvar(14) = dble(topohgt)*1.d3
    
! deactivate TSURF which does not exist in GRAM05
! extvar(15) = dble(PSURF)

      extvar(16) = dble(iupdate)
      extvar(17) = dble(SZA)
      extvar(18) = dble(owlt)
      extvar(19) = dble(sunlat)
      extvar(20) = dble(sunlon)
      extvar(21) = -999.0d0
      extvar(22) = -999.0d0

!areocentric longitude of the Sun Ls (deg)
      extvar(23) = dble(ALS)
!local solar time (hrs)
      extvar(24) = dble(TLOCAL)
!universal time (hrs) (=local time at longitude 0 deg)
      UTIME = real(mod(TLOCAL + CLON/15.d0 + 24.d0,24.d0),kind=4)
      extvar(25)=dble(UTIME)

return
! return OK

!--------------
!error handling
!--------------
9999 pressure    = -999.d0
      ro          = -999.d0
      temperature = -999.d0
      windu       = -999.d0
      windv       = -999.d0
      do i_var = 1,5
         meanvar(i_var) = -999.d0
      end do
      do i_var = 1,25
         extvar(i_var) = -999.d0
      enddo
return

end subroutine cps_atmmarsgram_05
!-----------------------------------------------------------------------------
!--------- END of cps_atmmarsgram_05 subroutine ------------------------------
!-----------------------------------------------------------------------------





!-----------------------------------------------------------------------------
!------- ATMOS2_M05 sub routine ----------------------------------------------
!-----------------------------------------------------------------------------
!     Subroutines and Functions for                                     ATM2  1
!     Mars-GRAM 2005 - Version 1.1,  October, 2005                      ATM2  2
!-----------------------------------------------------------------------ATM2  3
!                                                                       ATM2  4
subroutine ATMOS2_M05(HGTIN,CLAT,CLON,MARSAU,SUNLAT,SUNLON,ALS,H,TEMP,DENST, &
                      UPFCTR,LWFCTR,PRES,thgt,careoid,ZF,iu0,deltaTEX,Texos, &
                      Tbase,Hrho,AMz,Dusttau,Dustmin,Dustmax,DustOD,EWWIND, &
                      NSWIND,blwindew,blwindns,blwindvert,HatZF,wavepert, &
                      TempDay,PresDay,DensDay,EWwnDay,NSwnDay,bluday,blvday, &
                      hgtasfc,Patsurf,Tempmax,Tempmin,Densmax,Densmin,Tgrnd, &
                      talb,icepolar,gz,Oldrref,requa,rpole,MapYear,profnear, &
                      proffar,nprof,profwgt,idaydata)
!C     HGTIN    Planeto-Centric HEIGHT ABOVE MOLA AREOID (KM)(INPUT)     ATM2 16
!C     CLAT     Planeto-Centric LATITUDE (DEGREES) (INPUT)               ATM2 17
!C     CLON     WEST LONGITUDE OF SPACECRAFT (DEGREES) (INPUT)           ATM2 18
!C     MARSAU   MARS ORBITAL RADIUS (AU) (INPUT)                         ATM2 19
!C     SUNLAT   AREOCENTRIC LATITUDE OF SUN (DEGREES) (INPUT)            ATM2 20
!C     SUNLON   MARS WEST LONGITUDE OF SUN (DEGREES) (INPUT)             ATM2 21
!C     ALS      AREOCENTRIC LONGITUDE OF SUN ORBIT (INPUT)               ATM2 22
!C     H        SCALE HEIGHT AT SPACECRAFT POSITION (KM) (OUTPUT)        ATM2 23
!C     TEMP     TEMPERATURE AT SPACECRAFT POSITION (K) (OUTPUT)          ATM2 24
!C     DENST    MASS DENSITY AT SPACECRAFT POSITION (KG/M**3) (OUTPUT)   ATM2 25
!C     UPFCTR   UPPER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)         ATM2 26
!C     LWFCTR   LOWER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)         ATM2 27
!C     PRES     PRESSURE AT SPACECRAFT POSITION (N/M**2) (OUTPUT)        ATM2 28
!C     thgt     LOCAL SURFACE HEIGHT RELATIVE TO MOLA AREOID (KM)        ATM2 29
!C              (INPUT)                                                  ATM2 30
!C     careoid  MOLA 1/2 degree areoid (km) (OUTPUT)                     ATM2 31
!C     ZF       Local height of base of thermosphere (OUTPUT)            ATM2 32
!C     iu0      unit number for messages (ONPUT)                         ATM2 33
!C     deltaTEX adjustment in exospheric temperature (K) (INPUT)         ATM2 34
!C     Texos    local exospheric temperature (K) (OUTPUT)                ATM2 35
!C     Tbase    local temperature for base of exosphere (K) (OUTPUT)     ATM2 36
!C     Hrho     density scale height (km) (OUTPUT)                       ATM2 37
!C     AMz      molecular weight (OUTPUT)                                ATM2 38
!C     Dusttau  dust optical depth (or 0 for assumed seasonal            ATM2 39
!C               variation) (INPUT)                                      ATM2 40
!C     Dustmin  minimum optical depth for seasonal variation (INPUT)     ATM2 41
!C     Dustmax  maximum optical depth for seasonal variation (INPUT)     ATM2 42
!C     DustOD   dust optical depth (OUTPUT)                              ATM2 43
!C     EWWIND   Eastward wind component (m/s) (OUTPUT)                   ATM2 44
!C     NSWIND   Northward wind component (m/s) (OUTPUT)                  ATM2 45
!C     blwindew Eastward b.l. slope wind (m/s) (OUTPUT)                  ATM2 46
!C     blwindns Northward b.l. slope wind (m/s) (OUTPUT)                 ATM2 47
!C     HatZF    pressure scale height at altitude ZF (km) (OUTPUT)       ATM2 48
!C     wavepert perturbation (% of mean) from longitude-dependent wave   ATM2 49
!C     TempDay  daily average temperature (K) (OUTPUT)                   ATM2 50
!C     PresDay  daily average pressure (N/m**2) (OUTPUT)                 ATM2 51
!C     DensDay  daily average density (kg/m**3) (OUTPUT)                 ATM2 52
!C     EWwnDay  daily average eastward wind component (m/s) (OUTPUT)     ATM2 53
!C     NSwnDay  dailt average northward wind component (m/s) (OUTPUT)    ATM2 54
!C     hgtasfc  height above surface for evaluation of boundary layer    ATM2 55
!C               variables (km) (INPUT)                                  ATM2 56
!C     Patsurf  atmospheric pressure at surface (N/m**2) (OUTPUT)        ATM2 57
!C     Tempmax  daily maximum temperature (K) (OUTPUT)                   ATM2 58
!C     Tempmin  daily minimum temperature (K) (OUTPUT)                   ATM2 59
!C     Densmax  daily maximum density (kg/m**3) (OUTPUT)                 ATM2 60
!C     Densmin  daily minimum density (kg/m**3) (OUTPUT)                 ATM2 61
!C     Tgrnd    ground surface temperature (K) (OUTPUT)                  ATM2 62
!C     talb     surface albedo (OUTPUT)                                  ATM2 63
!C     icepolar polar ice indicator (0=no, 1=yes) (OUTPUT)               ATM2 64
!C     gz       gravity (m/s**2) at input height (OUTPUT)                ATM2 65
!C     Oldrref  local radius (km) of reference ellipsoid (OUTPUT)        ATM2 66
!C     requa    equatorial radius (km) of reference ellipsoid (INPUT)    ATM2 67
!C     rpole    polar radius (km) of reference ellipsoid (INPUT)         ATM2 68
!C                                                                       ATM2 69
!C---------------------------------------------------------------------  ATM2 70

  ! 
  !.. Implicit Declarations .. 
  implicit none

 ! 
  !.. Parameters .. 
  real(kind=PM_REEL), parameter :: pertmax = 0.15d0
  real(kind=PM_REEL), parameter :: pertmin = 0.02d0
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: MapYear,idaydata,iu0,nprof
  integer, intent(out) :: icepolar
  real(kind=PM_REEL), intent(in) :: &
    ALS,CLAT,CLON,Dustmax,Dustmin,Dusttau,HGTIN,MARSAU,SUNLAT,SUNLON,deltaTEX, &
    hgtasfc,proffar,profnear,requa,rpole,wavepert
  real(kind=PM_REEL), intent(out) :: &
    AMz,DENST,DensDay,Densmax,Densmin,DustOD,EWWIND,EWwnDay,H,HatZF,Hrho, &
    LWFCTR,NSWIND,NSwnDay,Oldrref,PRES,Patsurf,PresDay,TEMP,Tbase,TempDay, &
    Tempmax,Tempmin,Texos,Tgrnd,UPFCTR,ZF,bluday,blvday,blwindew,blwindns, &
    blwindvert,careoid,gz,profwgt,talb,thgt
  ! 
  !.. Local Scalars .. 
  integer :: LoH,ice1,icex,icez
  real(kind=PM_REEL) :: AMW = 43.49d0
  real(kind=PM_REEL) :: AMLO,AMTop,CHGT,DENSTop,DENSTz,Ddx,DensDay1,Densmax1, &
                      Densmin1,Dmaxz,Dminx,Dminz,EWWND1,HHI,Hrhi,Hrho1, &
                      HrhoTop,Hrhoz,NSWND1,NSwnDay1,PRES1,PRESLO,PRESTop,PX, &
                      Pdayz,Rgas,StewFpres,TEMP1,TEMPHI,TEMPz,Tdz,TempDay1, &
                      Tgrndx,Tminx,TopZ,UX,Ublx,Udayz,Vdx,Wblx
  real(kind=PM_REEL) :: Wblz,ZFp,blUdx,bluDay1,blwvz,blwwz,ddtex,dustM, &
                      dustoffset,fLH,pout,tfactor,tgrad,tophgtX,uout,wavemax
  real(kind=PM_REEL) :: RSTAR = 8.314472d3
  real(kind=PM_REEL) :: AMHI,AMz1,DENSHI,DENSLO,DENST1,DX,Ddayz,Dmaxx,EWwnDay1, &
                      H1,HDX,HLO,HTop,Hrlo,Hz,PRESHI,PRESz,Pdx,PresDay1,Shgt, &
                      StewFdens,TEMPLO,TEMPTop,TF,TIME,TINF,Tat5m,Tdayz,Tdx, &
                      Tempmax1,Tempmin1,Tgrdnx,Tgrnd1,Tgrndz,Tmaxx,Tmaxz, &
                      Tminz,UPF,Udx,VX
  real(kind=PM_REEL) :: Vblx,Vdayz,ZF1,ZFz,areoidX,blVdx,blvDay1,blwuz,dout,gCp, &
                      goR,pitwo,stormdz,talbx,talbz,tgradTop,tgradm,tgradp, &
                      tout,vout
  ! 
  !.. External Calls .. 
  ! remove for DM1359 : private  
  !external Dustfact_M05, MarsGCM_M05, ProfTerp_M05, RELLIPS_M05, STEWART2_M05, &
  !         TESGCM_M05, TESOD_M05
  
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Cp_M05
  !real(kind=PM_REEL), external :: dustvsls_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, log, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  !---  pi/2                                                              ATM2 78
  pitwo = 2.0d0 * atan(1.0d0)
  !---  Top height = 170 km for MapYear=0 otherwise Top height = 240 km   ATM2 80
  TopZ = 170.0d0
  if (Mapyear > 0) then
    TopZ = 240.0d0
  end if
  !---  Set slope winds to zero                                           ATM2 83
  blwindew = 0.0d0
  blwindns = 0.0d0
  !---  Local solar time (Mars hours = 1/24th Sol)                        ATM2 86
  TIME = 12.0d0 + (SUNLON-CLON)/15.0d0
  if (TIME < 0.0d0) then
    TIME = TIME + 24.0d0
  end if
  if (TIME > 24.0d0) then
    TIME = TIME - 24.0d0
  end if
  if (MapYear > 0) then
    !---    Evaluate dust from Mapping Year 1 or 2 input file               ATM2 91
    call TESOD_M05(MapYear,ALS,CLAT,CLON,DustOD)
  !---    Evaluate dust optical depth from NameList input parameters      ATM2 94
  elseif (Dusttau > 0.0d0) then
    DustOD = Dusttau
    if (DustOD < 0.1d0) then
      DustOD = 0.1d0
    end if
  else
    DustOD = dustvsls_M05(ALS,Dustmin,Dustmax)
  end if
  !---  Evaluate factor for dust storm model                              ATM2102
  call Dustfact_M05(CLAT,CLON,als,dustM,stormdz)
  DustOD = DustOD + dustM
  !---  Evaluate MTGCM height offset due to dust storm                    ATM2105
  dustoffset = stormdz
  !---  Get areoid radius and topographic height at current lat, lon      ATM2107
  call RELLIPS_M05(clat,clon,careoid,HGTIN,gz,Oldrref,thgt,talb,requa,rpole)
  !---  Evaluate pressure and temperature at surface, Patsurf and Tat5m   ATM2110
  Tat5m = 0.0d0
  if (MapYear == 0) then
    call MarsGCM_M05(thgt,CLAT,CLON,ALS,DustOD,TIME,TEMPz,PRESz,DENSTz,EWWIND, &
                     NSWIND,blwuz,blwvz,blwwz,Hz,Hrhoz,ZFz,tfactor,thgt, &
                     hgtasfc,careoid,Tdayz,Pdayz,Ddayz,Udayz,Vdayz,blUdx, &
                     blVdx,Tmaxz,Tminz,Dmaxz,Dminz,Tgrndz,talbz,icez,Tat5m, &
                     dustoffset,requa,rpole,idaydata)
  else
    call TESGCM_M05(thgt,CLAT,CLON,ALS,TIME,TEMPz,PRESz,DENSTz,EWWIND,NSWIND, &
                    blwuz,blwvz,blwwz,Hz,Hrhoz,ZFz,tfactor,thgt,hgtasfc, &
                    careoid,Tdayz,Pdayz,Ddayz,Udayz,Vdayz,blUdx,blVdx,Tmaxz, &
                    Tminz,Dmaxz,Dminz,Tgrndz,talbz,icez,Tat5m,requa,rpole, &
                    MapYear,idaydata)
  end if
  Patsurf = PRESz
  Tgrnd = Tgrndz
  !---  Adjust Patsurf for wave perturbation                              ATM2127
  Patsurf = Patsurf * (1.0d0+wavepert)
  !---  Tat5m is air temperature at surface + 5 meters                    ATM2129
  !---  Set evaluation hgt (CHGT) to HGTIN or to thgt+hgtasfc if HGTIN <= ATM2130
  !     -8.7 km                                                           ATM2131
  CHGT = HGTIN
  if (HGTIN <= -8.7d0) then
    CHGT = thgt + hgtasfc
  end if
  !---  Compute atmosphere using only MGCM data if height below 80 km     ATM2136
  if (CHGT <= 80.0d0) then
    if (MapYear == 0) then
      call MarsGCM_M05(CHGT,CLAT,CLON,ALS,DustOD,TIME,TEMP,PRES,DENST,EWWIND, &
                       NSWIND,blwindew,blwindns,blwindvert,H,Hrho,ZF,tfactor, &
                       thgt,hgtasfc,careoid,TempDay,PresDay,DensDay,EWwnDay, &
                       NSwnDay,bluday,blvday,Tempmax,Tempmin,Densmax,Densmin, &
                       Tgrndz,talb,icepolar,Tat5m,dustoffset,requa,rpole, &
                       idaydata)
    else
      call TESGCM_M05(CHGT,CLAT,CLON,ALS,TIME,TEMP,PRES,DENST,EWWIND,NSWIND, &
                      blwindew,blwindns,blwindvert,H,Hrho,ZF,tfactor,thgt, &
                      hgtasfc,careoid,TempDay,PresDay,DensDay,EWwnDay,NSwnDay, &
                      bluday,blvday,Tempmax,Tempmin,Densmax,Densmin,Tgrndz, &
                      talb,icepolar,Tat5m,requa,rpole,MapYear,idaydata)
    end if
    !---    Adjust PRES, and DENST for wave perturbation                    ATM2153
    PRES = PRES * (1.0d0+wavepert)
    DENST = DENST * (1.0d0+wavepert)
    DensDay = DensDay * (1.0d0+wavepert)
    if (idaydata > 0) then
      Densmax = Densmax * (1.0d0+wavepert)
      Densmin = Densmin * (1.0d0+wavepert)
    end if
    HatZF = 0.0d0
    UPFCTR = 1.0d0 + tfactor
    LWFCTR = 0.0d0
    AMz = AMW
  elseif (CHGT <= TopZ) then
    !---    This section uses MTGCM for density, pressure, temperature and  ATM2166
    !       Stewart model for mixing ratios                                 ATM2167
    !---    Evaluate MTGCM at current height                                ATM2168
    if (MapYear == 0) then
      call MarsGCM_M05(CHGT,CLAT,CLON,ALS,DustOD,TIME,TEMP,PRES,DENST,EWWIND, &
                       NSWIND,blwindew,blwindns,blwindvert,H,Hrho,ZF,tfactor, &
                       thgt,hgtasfc,careoid,TempDay,PresDay,DensDay,EWwnDay, &
                       NSwnDay,bluday,blvday,Tempmax,Tempmin,Densmax,Densmin, &
                       Tgrndz,talb,icepolar,Tat5m,dustoffset,requa,rpole, &
                       idaydata)
    else
      call TESGCM_M05(CHGT,CLAT,CLON,ALS,TIME,TEMP,PRES,DENST,EWWIND,NSWIND, &
                      blwindew,blwindns,blwindvert,H,Hrho,ZF,tfactor,thgt, &
                      hgtasfc,careoid,TempDay,PresDay,DensDay,EWwnDay,NSwnDay, &
                      bluday,blvday,Tempmax,Tempmin,Densmax,Densmin,Tgrndz, &
                      talb,icepolar,Tat5m,requa,rpole,MapYear,idaydata)
    end if
    !---    Adjust PRES, and DENST for wave perturbation                    ATM2184
    PRES = PRES * (1.0d0+wavepert)
    DENST = DENST * (1.0d0+wavepert)
    DensDay = DensDay * (1.0d0+wavepert)
    if (idaydata > 0) then
      Densmax = Densmax * (1.0d0+wavepert)
      Densmin = Densmin * (1.0d0+wavepert)
    end if
    !---    Compute molecular weight                                        ATM2192
    AMz = DENST * RSTAR * TEMP / PRES
    !---    Find temperature TF at height ZF = altitude of 1.26 nbar level  ATM2194
    if (ZF > 900.0d0) then
      TF = 999.9d0
    else
      if (MapYear == 0) then
        call MarsGCM_M05(ZF,CLAT,CLON,ALS,DustOD,TIME,TF,PX,DX,UX,VX,Ublx, &
                         Vblx,Wblz,HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX, &
                         Tdz,Pdx,Ddx,Udx,Vdx,blUdx,blVdx,Tmaxx,Tminx,Dmaxx, &
                         Dminx,Tgrdnx,talbx,icex,Tat5m,dustoffset,requa,rpole, &
                         idaydata)
      else
        call TESGCM_M05(ZF,CLAT,CLON,ALS,TIME,TF,PX,DX,UX,VX,Ublx,Vblx,Wblz, &
                        HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX,Tdz,Pdx,Ddx, &
                        Udx,Vdx,blUdx,blVdx,Tmaxx,Tminx,Dmaxx,Dminx,Tgrdnx, &
                        talbx,icex,Tat5m,requa,rpole,MapYear,idaydata)
      end if
      !---      Adjust ZF for wave perturbation                               ATM2211
      ZF = ZF + HatZF*log(1.0d0+wavepert)
    end if
    !---    Save temperature at 1.26 nbar level                             ATM2214
    Tbase = TF
    UPFCTR = 1.0d0 + tfactor
    LWFCTR = 1.0d0 - tfactor
    !---    Use MTGCM data to get (unperturbed) values at TopZ km           ATM2218
    if (MapYear == 0) then
      call MarsGCM_M05(TopZ,CLAT,CLON,ALS,DustOD,TIME,TEMP1,PRES1,DENST1, &
                       EWWND1,NSWND1,blwindew,blwindns,blwindvert,H1,Hrho1, &
                       ZF1,tfactor,thgt,hgtasfc,careoid,TempDay1,PresDay1, &
                       DensDay1,EWwnDay1,NSwnDay1,bluDay1,blvDay1,Tempmax1, &
                       Tempmin1,Densmax1,Densmin1,Tgrnd1,talb,ice1,Tat5m, &
                       dustoffset,requa,rpole,idaydata)
    else
      call TESGCM_M05(TopZ,CLAT,CLON,ALS,TIME,TEMP1,PRES1,DENST1,EWWND1, &
                      NSWND1,blwindew,blwindns,blwindvert,H1,Hrho1,ZF1, &
                      tfactor,thgt,hgtasfc,careoid,TempDay1,PresDay1,DensDay1, &
                      EWwnDay1,NSwnDay1,bluDay1,blvDay1,Tempmax1,Tempmin1, &
                      Densmax1,Densmin1,Tgrnd1,talb,ice1,Tat5m,requa,rpole, &
                      MapYear,idaydata)
    end if
    !---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment   ATM2234
    Shgt = TopZ
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRESTop,TEMPTop,DENSTop,Shgt, &
                      Rstar,HTop,AMTop,0.0d0,iu0,sunlat,deltaTEX,TINF,TF,ZF1, &
                      HrhoTop,requa,rpole,tgradTop)
    !---    Adjust deltaTEX for temperature difference at TopZ              ATM2239
    ddtex = deltaTEX + TEMP1 - TEMPTop
    Shgt = CHGT
    if (CHGT < ZF) then
      Shgt = ZF
    end if
    !---    Evaluate thermospheric parameters at current height             ATM2243
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRES1,TEMP1,DENST1,Shgt,Rstar,H1, &
                      AMz1,0.0d0,iu0,sunlat,ddtex,TINF,TF,ZF,Hrho1,requa, &
                      rpole,tgrad)
    !---    Save exospheric temperature                                     ATM2247
    Texos = TINF
  else
    !---    For height above TopZ -  Use MTGCM data to get (unperturbed)    ATM2250
    !       values at TopZ                                                  ATM2251
    if (MapYear == 0) then
      call MarsGCM_M05(TopZ,CLAT,CLON,ALS,DustOD,TIME,TEMP1,PRES1,DENST1, &
                       EWWIND,NSWIND,blwindew,blwindns,blwindvert,H1,Hrho1,ZF, &
                       tfactor,thgt,hgtasfc,careoid,TempDay1,PresDay1, &
                       DensDay1,EWwnDay1,NSwnDay1,bluDay1,blvDay1,Tempmax1, &
                       Tempmin1,Densmax1,Densmin1,Tgrnd1,talb,ice1,Tat5m, &
                       dustoffset,requa,rpole,idaydata)
    else
      call TESGCM_M05(TopZ,CLAT,CLON,ALS,TIME,TEMP1,PRES1,DENST1,EWWIND, &
                      NSWIND,blwindew,blwindns,blwindvert,H1,Hrho1,ZF,tfactor, &
                      thgt,hgtasfc,careoid,TempDay1,PresDay1,DensDay1, &
                      EWwnDay1,NSwnDay1,bluDay1,blvDay1,Tempmax1,Tempmin1, &
                      Densmax1,Densmin1,Tgrnd1,talb,ice1,Tat5m,requa,rpole, &
                      MapYear,idaydata)
    end if
    !---    Find temperature TF at height ZF  = height of 1.26 nbar level   ATM2267
    if (MapYear == 0) then
      call MarsGCM_M05(ZF,CLAT,CLON,ALS,DustOD,TIME,TF,PX,DX,UX,VX,Ublx,Vblx, &
                       Wblx,HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX,Tdx,Pdx, &
                       Ddx,Udx,Vdx,bludx,blvdx,Tmaxx,Tminx,Dmaxx,Dminx,Tgrndx, &
                       talbx,icex,Tat5m,dustoffset,requa,rpole,idaydata)
    else
      call TESGCM_M05(ZF,CLAT,CLON,ALS,TIME,TF,PX,DX,UX,VX,Ublx,Vblx,Wblx, &
                      HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX,Tdx,Pdx,Ddx, &
                      Udx,Vdx,bludx,blvdx,Tmaxx,Tminx,Dmaxx,Dminx,Tgrndx, &
                      talbx,icex,Tat5m,requa,rpole,MapYear,idaydata)
    end if
    !---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment   ATM2280
    Shgt = TopZ
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRESTop,TEMPTop,DENSTop,Shgt, &
                      Rstar,HTop,AMTop,0.0d0,iu0,sunlat,deltaTEX,TINF,TF,ZF, &
                      HrhoTop,requa,rpole,tgradTop)
    !---    Adjust deltaTEX for temperature difference at TopZ              ATM2285
    ddtex = deltaTEX + TEMP1 - TEMPTop
    !---    Evaluate Stewart thermosphere at Top km for adjustment factors  ATM2287
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRESTop,TEMPTop,DENSTop,Shgt, &
                      Rstar,HTop,AMTop,0.0d0,iu0,sunlat,ddtex,TINF,TF,ZF, &
                      HrhoTop,requa,rpole,tgradTop)
    !---    Set daily average values to zero above TopZ                     ATM2291
    TempDay = 0.0d0
    PresDay = 0.0d0
    DensDay = 0.0d0
    EWwnDay = 0.0d0
    NSwnDay = 0.0d0
    !---    Set daily max, min Temp and Density to zero above TopZ          ATM2297
    Tempmax = 0.0d0
    Tempmin = 0.0d0
    Densmax = 0.0d0
    Densmin = 0.0d0
    !---    Adjust ZF for wave perturbation, using scale height at ZF       ATM2302
    ZF = ZF + HatZF*log(1.0d0+wavepert)
    !---    Save temperature at 1.26 nbar level                             ATM2304
    Tbase = TF
    Shgt = CHGT
    !---    Evaluate thermospheric parameters at current height             ATM2307
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRES,TEMP,DENST,Shgt,Rstar,H,AMz, &
                      0.0d0,iu0,sunlat,ddtex,TINF,TF,ZF,Hrho,requa,rpole,tgrad &
                     )
    !---    Save exospheric temperature                                     ATM2311
    Texos = TINF
    !---    Evaluate thermospheric parameters at deviation=+1, for DENSHI   ATM2313
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRESHI,TEMPHI,DENSHI,Shgt,Rstar, &
                      HHI,AMHI,1.0d0,iu0,sunlat,ddtex,TINF,TF,ZF,Hrhi,requa, &
                      rpole,tgradp)
    !---    Evaluate thermospheric parameters at deviation=-1, for DENSLO   ATM2317
    call STEWART2_M05(MARSAU,CLAT,CLON,TIME,PRESLO,TEMPLO,DENSLO,Shgt,Rstar, &
                      HLO,AMLO,-1.0d0,iu0,sunlat,ddtex,TINF,TF,ZF,Hrlo,requa, &
                      rpole,tgradm)
    UPFCTR = DENSHI / DENST
    LWFCTR = DENSLO / DENST
    if (UPFCTR < LWFCTR) then
      UPFCTR = DENSLO / DENST
      LWFCTR = DENSHI / DENST
    end if
    LoH = INT(2.0d0)
    fLH = DBLE(LoH) / 6.283185d0
    fLH = fLH * sqrt(1.0d0+fLH**2)
    gCp = 1000.0d0 * gz / Cp_M05(TEMP)
    Rgas = PRES / (DENST*TEMP)
    goR = 1000.0d0 * gz / Rgas
    wavemax = fLH * (tgrad+gCp) / goR
    if (UPFCTR > 1.0d0+wavemax) then
      UPFCTR = 1.0d0 + wavemax
    end if
    if (LWFCTR < 1.0d0-wavemax) then
      LWFCTR = 1.0d0 - wavemax
    end if
    !---    Apply adjustment factors to density and pressure                ATM2336
    StewFdens = DENST1 / DENSTop
    DENST = DENST * StewFdens
    StewFpres = PRES1 / PRESTop
    PRES = PRES * StewFpres
    !---    Adjust molecular weight                                         ATM2341
    AMz = RSTAR * DENST * TEMP / PRES
  end if
  !---  Insure thermospheric variability factors are in allowable ranges  ATM2344
  if (LWFCTR > 0.0d0) then
    if (UPFCTR > 1.0d0+pertmax) then
      UPFCTR = 1.0d0 + pertmax
    end if
    if (UPFCTR < 1.0d0+pertmin) then
      UPFCTR = 1.0d0 + pertmin
    end if
    if (LWFCTR < 1.0d0-pertmax) then
      LWFCTR = 1.0d0 - pertmax
    end if
    if (LWFCTR > 1.0d0-pertmin) then
      LWFCTR = 1.0d0 - pertmin
    end if
    if (tfactor > pertmax) then
      tfactor = pertmax
    end if
    if (tfactor < pertmin) then
      tfactor = pertmin
    end if
  end if
  !---  Use weighted average profile data if profnear > 0. Weight=1 if    ATM2353
  !     lat-lon radius < profnear. Weight=0 if lat-lon radius > proffar.  ATM2354
  if (profnear > 0.0d0) then
    call ProfTerp_M05(CHGT,CLAT,CLON,TEMP,PRES,DENST,EWWIND,NSWIND,tout,pout, &
                      dout,uout,vout,nprof,profnear,proffar,profwgt)
    TEMP = tout
    PRES = pout
    DENST = dout
    EWWIND = uout
    NSWIND = vout
  end if
end subroutine ATMOS2_M05
!-----------------------------------------------------------------------ATM2366
subroutine bltp_M05(gz,Cp,Tg,z5,T5,U5,V5,zeval,factor,Tempz)
  !                                                                       BLTP  2
  !---    Mars boundary layer temperature from methods used in the NASA   BLTP  3
  !       Ames Mars General Circulation Model (MGCM), as described by     BLTP  4
  !       Haberle et al., Jour. Geophys. Res. 104(E4), 8957-8974, 1999,   BLTP  5
  !       (referred to as H99 below).                                     BLTP  6
  !                                                                       BLTP  7
  !---    Input parameters:                                               BLTP  8
  !       gz = local acceleration of gravity (m/s**2)                     BLTP  9
  !       Cp = specific heat at constant pressure (joules kg**-1 K**-1)   BLTP 10
  !       Tg = temperature (K) at ground surface                          BLTP 11
  !       z5 = first MGCM height level (nominally 5 m)                    BLTP 12
  !       T5 = temperature (K) at first MGCM level (nominally 5 m)        BLTP 13
  !       U5 = zonal wind (m/s) at first MGCM level                       BLTP 14
  !       V5 = meridional wind (m/s) at first MGCM level                  BLTP 15
  !       zeval = height (m) at which to evaluate temperature             BLTP 16
  !       factor = factor for calculations = Log(zeval/z0)/Log(5./z0),    BLTP 17
  !                where z0 is surface roughness parameter (m)            BLTP 18
  !---    Output parameter:                                               BLTP 19
  !       Tempz = temperature (K) at height zeval                         BLTP 20
  !---------------------------------------------------------------------  BLTP 21
  !                                                                       BLTP 22
  !---    Set some initial values for iterative solution                  BLTP 23
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: Cp,T5,Tg,U5,V5,factor,gz,z5,zeval
  real(kind=PM_REEL), intent(out) :: Tempz
  ! 
  !.. Local Scalars .. 
  integer :: i
  real(kind=PM_REEL) :: Ri,T0,dT,sqrtFh,thet5,udenom
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  sqrtFh = 1.0d0
  dT = T5 - Tg
  T0 = T5
  !---    Potential temperature at first MGCM level                       BLTP 28
  thet5 = T5 + gz*z5/Cp
  !---    Iterative calculation of boundary layer parameters              BLTP 30
  do i = 1,10
    !---      Richardson number from temperature and wind gradients         BLTP 32
    udenom = U5**2 + V5**2
    if (udenom <= 0.1d0) then
      udenom = 0.1d0
    end if
    Ri = (gz*sqrtFh/thet5) * ((thet5-Tg)/(1.0d0+sqrtFh)) * z5 / udenom
    !---      Next iteration of temperature solution by method in Section   BLTP 36
    !         4 of H99 (convert from potential to regular temperature)      BLTP 37
    Tempz = Tg + (thet5-Tg)*(1.0d0+sqrtFh*factor)/(1.0d0+sqrtFh) - gz*zeval/Cp
    !---      Change in temperature from previous iteration                 BLTP 40
    dT = Tempz - T0
    T0 = Tempz
    !---      End iteration if sufficient temperature precision achieved    BLTP 43
    if (abs(dT) < 0.01d0) then
      exit
    !---      Next iteration of Sqrt(Fh), where Fh is stability function    BLTP 45
    !         from Section 4 of H99                                         BLTP 46
    elseif (Ri < 0.0d0) then
      sqrtFh = (1.0d0-16.0d0*Ri)**0.25d0
    else
      sqrtFh = (1.0d0+15.0d0*Ri/sqrt(1.0d0+5.0d0*Ri))**(-0.5d0)
    end if
  end do
end subroutine bltp_M05
!-----------------------------------------------------------------------BLTP 55
subroutine CaltoJul_M05(iY,iM,iD,ihour,imin,sec,xJD)
  !                                                                       CTOJ  2
  !     Compute Julian day (Real*8) by method of Meeus, Astronomical      CTOJ  3
  !       Algorithms, 2nd Edition, 1998, page 61. Inputs are year iY,     CTOJ  4
  !       month iM, day of month iD, and time of day in hours, minutes,   CTOJ  5
  !       and seconds (all integer except seconds).  Output is Real*8     CTOJ  6
  !       Julian day, xJD.                                                CTOJ  7
  !                                                                       CTOJ  8
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: iD,iM,iY,ihour,imin
  real(kind=PM_REEL), intent(in) :: sec
  real(kind=PM_REEL), intent(out) :: xJD
  ! 
  !.. Local Scalars .. 
  integer :: A,B,M,Y
  real(kind=PM_REEL) :: D
  ! 
  !.. Intrinsic Functions .. 
  intrinsic int
  ! 
  ! ... Executable Statements ...
  ! 
  Y = iY
  M = iM
  !---  Consider Jan or Feb as if months 13 and 14 of previous year       CTOJ 14
  if (iM <= 2) then
    Y = iY - 1
    M = iM + 12
  end if
  !---  Compute day of month plus fractional part                         CTOJ 19
  D = DBLE(iD) + DBLE(ihour)/2.4d1 + DBLE(imin)/1.440d3 + sec/8.64d4
  A = int(DBLE(Y)/100.0d0)
  B = 2 - A + int(DBLE(A)/4.0d0)
  !---  Compute Julian day with fractional part                           CTOJ 23
  xJD = DBLE(int(365.25d0*DBLE((Y+4716)))+int(30.6001d0*DBLE((M+1)))) + D + &
        DBLE(B) - 1524.5d0
end subroutine CaltoJul_M05
!---------------------------------------------------------------------- CTOJ 28
subroutine cospar_M05(z,t,p,rho)
  !                                                                       COSP  2
  !     COSPAR N.H. mean temperature (t, K), pressure (p, N/m**2) and     COSP  3
  !     density (rho, kg/m**3) versus height (z, km)                      COSP  4
  !     Note: input pressures (pc) are mb, densities (dc) are g/cm**3     COSP  5
  !                                                                       COSP  6
  !     COSPAR values from Table XI, "The Mars Atmosphere: Observations   COSP  7
  !     and Model Profiles for Mars Missions", David E. Pitts et al.,     COSP  8
  !     eds., JSC-24455                                                   COSP  9
  !                                                                       COSP 10
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: z
  real(kind=PM_REEL), intent(out) :: p,rho,t
  ! 
  !.. Local Scalars .. 
  integer :: iz
  real(kind=PM_REEL) :: H,R,R1,R2,aexp,dz
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, exp, int, log
  ! 
  !.. Common Blocks .. 
  common /COSPARNH_M05/ ZC,TC,PC,DC
  !     For zc to dc: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /cosparnh_M05/ ... 
  real(kind=PM_REEL), dimension(164) :: DC,PC,TC,ZC
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       COSP 13
  !     1 km interval from -10 to 130 km, 10 km interval 130-360 km       COSP 14
  if (z < 130.0d0) then
    iz = int(z+11.0d0)
  else
    iz = 141 + int((z-130.0d0)/10.0d0)
  end if
  !     Set values to 0 if z out of range                                 COSP 20
  if (iz<1 .or. iz>164) then
    t = 0.0d0
    p = 0.0d0
    rho = 0.0d0
  else
    if (iz > 163) then
      iz = 163
    end if
    !     Linear interpolation on temperature                               COSP 28
    dz = (z-ZC(iz)) / (ZC(iz+1)-ZC(iz))
    t = TC(iz) + (TC(iz+1)-TC(iz))*dz
    !     Pressure from hydrostatic relation (with special isothermal case) COSP 31
    if (abs(TC(iz+1)-TC(iz)) > 0.01d0) then
      aexp = log(PC(iz+1)/PC(iz)) / log(TC(iz+1)/TC(iz))
      p = 100.0d0 * PC(iz) * (t/TC(iz))**aexp
    else
      H = (ZC(iz+1)-ZC(iz)) / log(PC(iz)/PC(iz+1))
      p = 100.0d0 * PC(iz) * exp(-(z-ZC(iz))/H)
    end if
    !     Linear interpolation on gas constant                              COSP 39
    R1 = PC(iz) / (DC(iz)*TC(iz))
    R2 = PC(iz+1) / (DC(iz+1)*TC(iz+1))
    R = R1 + (R2-R1)*dz
    !     density from perfect gas law (and convert units to kg/m**3)       COSP 43
    rho = 10.0d0 * p / (R*t)
  end if
end subroutine cospar_M05
!-----------------------------------------------------------------------COSP 47
real(kind=PM_REEL) function Cp_M05(T)
  !---  Specific heat at constant pressure, as function of temperature    CPOT  2
  !---  T in kelvins; Cp in joules kg**-1 K**-1                           CPOT  3
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: T
  ! 
  ! ... Executable Statements ...
  ! 
  Cp_M05 = 639.5d0 + 0.123687d0*T + 0.00200225d0*T*T
end function Cp_M05
!-----------------------------------------------------------------------CPOT  8
subroutine GeocenttoGeodet_M05(r,zin,fi,h,a,b)
  !---  Program to transform Cartesian to geodetic coordinates            CTOD  2
  !     Code adapted from Polish version of K.M.Borkowski, Bull. Geod.    CTOD  3
  !     vol 63, pp.50-56 (1989).                                          CTOD  4
  !---  Input:   r, z = equatorial and polar Cartesian components [km]    CTOD  5
  !              a, b = equatorial and polar planetary radii [km]         CTOD  6
  !---  Output: fi, h = geodetic coord's (latitude [deg], height [km])    CTOD  7
  !     Special case for poles                                            CTOD  8
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: r,zin
  real(kind=PM_REEL), intent(out) :: fi,h
  real(kind=PM_REEL), intent(in) :: a,b
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: D,E,F,G,P,Q,s,t,v,z
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, acos, atan, cos, exp, log, sign, sin, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  if (abs(r) < 0.001d0) then
    fi = sign(90.0d0,zin)
    h = abs(zin) - b
  else
    !---   Analytical solution for non-polar case                           CTOD 14
    !      See also page K12 of Astronomical Almanac for iterative          CTOD 15
    !      solution                                                         CTOD 16
    z = abs(zin)
    E = ((z+b)*b/a-a) / r
    F = ((z-b)*b/a+a) / r
    P = (E*F+1.0d0) * 4.0d0 / 3.0d0
    Q = (E*E-F*F) * 2.0d0
    D = P*P*P + Q*Q
    if (D >= 0.0d0) then
      s = sqrt(D) + Q
      s = sign(exp(log(abs(s))/3.0d0),s)
      v = P/s - s
      v = -(Q+Q+v*v*v)/(3.0d0*P)
    else
      v = 2.0d0 * sqrt(-P) * cos(acos(Q/P/sqrt(-P))/3.0d0)
    end if
    G = 0.5d0 * (E+sqrt(E*E+v))
    t = sqrt(G*G+(F-v*G)/(G+G-E)) - G
    fi = atan((1.0d0-t*t)*a/(2.0d0*b*t))
    h = (r-a*t)*cos(fi) + (z-b)*sin(fi)
    !---     Convert to degrees                                             CTOD 35
    fi = fi * 45.0d0 / atan(1.0d0)
    if (zin < 0.0d0) then
      fi = -fi
    end if
  end if
end subroutine GeocenttoGeodet_M05
!---------------------------------------------------------------------- CTOD 41
subroutine GeodettoGeocent_M05(fidet,h,ficent,rtot,xy,z,a,b)
  !---  Program to transform geodetic latitude, height to geocentric      DTOC  2
  !     latitude, radius                                                  DTOC  3
  !     Method from page K12 of Astronomical Almanac                      DTOC  4
  !---  Input:  fidet, h = geodetic lat (deg), height (km)                DTOC  5
  !             a, b = equatorial and polar planetary radii (km)          DTOC  6
  !---  Output: ficent = geocentric lat (deg)                             DTOC  7
  !             rtot = geocentric total radius (km)                       DTOC  8
  !             xy, z = equatorial, polar cartesian components (km)       DTOC  9
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: a,b,fidet,h
  real(kind=PM_REEL), intent(out) :: ficent,rtot,xy,z
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: C,cphi,omf,pi180,sphi
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, cos, sign, sin, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  pi180 = atan(1.0d0) / 45.0d0
  !---  Special case for poles                                            DTOC 12
  if (abs(fidet) == 90.0d0) then
    ficent = sign(90.0d0,fidet)
    rtot = b + h
    xy = 0.0d0
    z = sign(rtot,fidet)
  else
    !---    1 - flattening                                                  DTOC 19
    omf = b / a
    !---    Sin and Cos of geodetic lat                                     DTOC 21
    sphi = sin(pi180*fidet)
    cphi = cos(pi180*fidet)
    !---    Computational factor C for cartesian coordinates                DTOC 24
    C = 1.0d0 / sqrt(cphi**2+(omf*sphi)**2)
    !---    Polar and equatorial cartesian coordinates                      DTOC 26
    z = (a*C*omf**2+h) * sphi
    xy = (a*C+h) * cphi
    !---    Total geocentric radius                                         DTOC 29
    rtot = sqrt(xy**2+z**2)
    !---    Geocentric latitude, deg                                        DTOC 31
    ficent = atan(z/xy) / pi180
  end if
end subroutine GeodettoGeocent_M05
!---------------------------------------------------------------------- DTOC 36
subroutine Dustfact_M05(CLAT,CLON,als,dustM,stormdz)
  !---  Computes dust storm intensity factor dustM                        DSTF  2
  !---  as a function of the time since start of the storm,               DSTF  3
  !---  (als - als0), measured in Ls angle (degrees), and as a            DSTF  4
  !---  function of the storm intensity, intens.  dustM is for            DSTF  5
  !---  magnitude of effect on dust optical depth (0.0 - 3.0).            DSTF  6
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: CLAT,CLON,als
  real(kind=PM_REEL), intent(out) :: dustM,stormdz
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: dew,dlon,dls,dlsmax,dns,rad,raddust,sizefact
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, cos, sqrt
  ! 
  !.. Common Blocks .. 
  common /DATACOM_M05/ DTR,DAY,DUSTLAT,DUSTLON,RADMAX,RREF,ALS0,ALSDUR,INTENS, &
                       DELTATEX,RPSCALE,DUSTTAU,DUSTMIN,DUSTMAX,DUSTOD,DUSTNU, &
                       DUSTDIAM,DUSTDENS,RWSCALE,WLSCALE,REQUA,RPOLE,WMSCALE, &
                       BLWINFAC,MAPYEAR,IDAYDATA,NPOS,NVARX,NVARY,LOGSCALE, &
                       IU0,IUP,IPCLAT,MOLAHGTS
  !     For DTR: Maybe Read, Not Written
  !     For DAY: Not Read, Not Written
  !     For dustlat to Rref: Maybe Read, Not Written
  !     For als0 to intens: Read, Not Written
  !     For deltaTEX to MOLAhgts: Not Read, Not Written
  ! 
  !... Variables in Common Block /DATACOM_M05/ ... 
  real(kind=PM_REEL) :: DAY,DTR,DUSTOD,DUSTDENS,DUSTDIAM,DUSTMAX,DUSTMIN,DUSTNU, &
                      DUSTTAU,RREF,ALS0,ALSDUR,BLWINFAC,DELTATEX,DUSTLAT, &
                      DUSTLON,INTENS,RADMAX,REQUA,RPOLE,RPSCALE,RWSCALE, &
                      WLSCALE,WMSCALE
  integer :: MOLAHGTS,MAPYEAR,NPOS,NVARX,NVARY,IDAYDATA,IPCLAT,IU0,IUP, &
             LOGSCALE
  ! 
  ! ... Executable Statements ...
  ! 
  dls = als - ALS0
  dlsmax = ALSDUR
  if (dlsmax < 12.0d0) then
    dlsmax = 12.0d0
  end if
  if (dlsmax > 48.0d0) then
    dlsmax = 48.0d0
  end if
  !---  Return dust factor of 0 if Ls-Ls0 < 0 or > dlsmax degrees         DSTF 18
  if (dls<=0.0d0 .or. dls>dlsmax .or. INTENS<=0.0d0) then
    dustM = 0.0d0
    stormdz = 0.0d0
    return
  end if
  !---  Compute initial dustM factor (0-1) from time (Ls) profile         DSTF 24
  if (dls <= dlsmax/8.0d0) then
    dustM = 8.0d0 * dls / dlsmax
  elseif (dls >= dlsmax/2.0d0) then
    dustM = 2.0d0 * (1.0d0-dls/dlsmax)
  else
    dustM = 1.0d0
  end if
  sizefact = 1.0d0
  !---  Compute parameters of local storm if radmax is not 0              DSTF 33
  if (RADMAX /= 0.0d0) then
    sizefact = 0.0d0
    !---    dns,dew,rad = horizontal coordinates from center of dust storm  DSTF 36
    dns = DTR * RREF * (CLAT-DUSTLAT)
    dlon = abs(CLON-DUSTLON)
    if (dlon > 180.0d0) then
      dlon = 360.0d0 - dlon
    end if
    dew = DTR * RREF * cos(DTR*CLAT) * dlon
    rad = sqrt(dns**2+dew**2)
    !---    raddust = actual horizontal size of storm                       DSTF 42
    raddust = dustM * RADMAX
    !---    sizefact = position-dependent measure of relative storm effect  DSTF 44
    if (rad < 2.0d0*raddust) then
      sizefact = 0.5d0 * (1.0d0+cos(90.0d0*DTR*rad/raddust))
    end if
  end if
  !---  Final factor dependent on position and on storm intensity         DSTF 48
  dustM = sizefact * dustM
  stormdz = 5.0d0 * dustM * sqrt(INTENS)
  dustM = dustM * INTENS
end subroutine Dustfact_M05
!-----------------------------------------------------------------------DSTF 54
subroutine Datastep_M05(I,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,RHOw,EOF, &
                        DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS, &
                        DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,VWpert,Hrho, &
                        HSCALE,dsunlat,dsunlon,dsunLs,dradau,dowlt,LonEW, &
                        corlim,DENSTOT,numwave,hgtasfc,IERT,IUTC,pertstep, &
                        corlmin,iupdate,ALS,szang,owlt,sunlat,sunlon,MarsAU, &
                        TLOCAL,profnear,proffar,nprof,DENS0,SIGD)
! DM1359 : adding DENS0 and SIGD for GRAM05 integration
  !---  If ipclat (in Common DATACOM) is 1, latitude CLAT and height CHGT DSTP  7
  !     are planeto-centric, otherwise CLAT and CHGT are planeto-graphic, DSTP  8
  !     with CHGT measured above reference ellipsoid.                     DSTP  9
  !     If input parameter MOLAhgts is 1, CHGT is height above the MOLA   DSTP 10
  !     areoid; otherwise CHGT is height above reference ellipsoid,       DSTP 11
  !     in which case height is converted to HGTM, height above MOLA      DSTP 12
  !     areoid for call to subroutine ATMOS2_M05.                         DSTP 13
  !---  If MOLAhgts = 1, then ipclat must also be 1                       DSTP 14
  !                                                                       DSTP 15
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ndust = 3
  integer, parameter :: ntesy = 2
  real(kind=PM_REEL), parameter :: onesol = 88775.245d0
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: I,IERT,IUTC,LonEW,nprof
  integer, intent(inout) :: iupdate,numwave
  integer, intent(out) :: EOF
  real(kind=PM_REEL), intent(in) :: &
    DAY0,corlmin,dowlt,dradau,dsunLs,dsunlat,dsunlon,hgtasfc,proffar,profnear
  real(kind=PM_REEL), intent(inout) :: &
    CHGT,CLAT,CLON,CSEC,DELHGT,DELLAT,DELLON,DELTIME,RHOd,RHOu,RHOv,RHOw, &
    pertstep
  real(kind=PM_REEL), intent(out) :: &
    ALS,DENS,DENSHI,DENSLO,DENSP,DENSTOT,EWWIND,EWpert,HSCALE,Hrho,MarsAU, &
    NSWIND,NSpert,PRES,TEMP,TLOCAL,VWpert,corlim,owlt,sunlat,sunlon,szang,DENS0,SIGD
  ! 
  !.. Local Scalars .. 
  character(LEN=4) :: lonvar
  character(LEN=8) :: densunits
  integer :: IFAULT,L,icepolar
  real(kind=PM_REEL) :: AMz,Alogdens,CHGTc,CHGTg,CLATc,CLATg,CORREL,DELEW,DELNS, &
                      DELZ,DMINUS,DPLUS,DensDay,DensRand,Densmax, &
                      Densmin,Dxy,Dz,EOT,EWtot,EWwnDay,Elon,F1peak,FACTHI, &
                      FACTLO,HGTM,HGTMS,HLS,HatZF,NStot,NSwnDay,OHGT,OHGTS, &
                      OLAT,OLON,Oldhgt,Oldrref,PRESmb,Patsurf
  real(kind=PM_REEL) :: PresDay,RSC,SIGU,SIGW,Tbase,TempDay,Tempmax, &
                      Tempmin,Texos,Tgrnd,TopZ,TrajHGT,TrajLAT,TrajLON, &
                      TrajSEC,VAR,VARX,VARY,VLS,Z1,Z2,ZF,Zbase,airmass,albedo, &
                      bluday,blvday,blwindew,blwindns,blwindvert,careoid, &
                      corbeta,cszang,dAreoid,dareaden,dcosp,denswave,devDay, &
                      devav
  real(kind=PM_REEL) :: devhi,devlo,devmax,devmin,devtot,dmasden,dmixrat, &
                      dnumden,dt,dtimecor,expfact,fmassH2O,fmolH2O,gz,offsetL, &
                      ogz,pcos,preshgt,profwgt,qsurf,radtotal,sigmalevel, &
                      ssposr2,talb,tcos,thgt,topohgt,ttsec,wavepert
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(0:8) :: fmass
  ! 
  !.. External Calls .. 
  ! DM 1359 remove : private
  ! external ATMOS2_M05, GeocenttoGeodet_M05, GeodettoGeocent_M05, RELLIPS_M05, &
  !           Wavelon_M05, cospar_M05, marsephm_M05, species_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Cp_M05
  !real(kind=PM_REEL), external :: PPND_M05
  !real(kind=PM_REEL), external :: RANDOM_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, acos, atan, cos, exp, log, log10, sign, sin, sqrt, tan
  ! 
  !.. Common Blocks .. 
  common /DATACOM_M05/ DTR,DAY,DUSTLAT,DUSTLON,RADMAX,RREF,ALS0,ALSDUR,INTENS, &
                       DELTATEX,RPSCALE,DUSTTAU,DUSTMIN,DUSTMAX,DUSTOD,DUSTNU, &
                       DUSTDIAM,DUSTDENS,RWSCALE,WLSCALE,REQUA,RPOLE,WMSCALE, &
                       BLWINFAC,MAPYEAR,IDAYDATA,NPOS,NVARX,NVARY,LOGSCALE, &
                       IU0,IUP,IPCLAT,MOLAHGTS
  !     For DTR: Read, Not Written
  !     For DAY: Not Read, Maybe Written
  !     For dustlat to radmax: Maybe Read, Not Written
  !     For Rref: Not Read, Overwritten
  !     For als0 to Dustmax: Maybe Read, Not Written
  !     For DustOD: Not Read, Maybe Written
  !     For Dustnu to rwscale: Maybe Read, Not Written
  !     For wlscale to rpole: Read, Not Written
  !     For wmscale to blwinfac: Maybe Read, Not Written
  !     For MapYear: Read, Not Written
  !     For idaydata: Maybe Read, Not Written
  !     For NPOS: Read, Not Written
  !     For NVARX to iup: Maybe Read, Not Written
  !     For ipclat to MOLAhgts: Read, Not Written
  ! 
  !... Variables in Common Block /DATACOM_M05/ ... 
  real(kind=PM_REEL) :: DAY,DTR,DUSTOD,DUSTDENS,DUSTDIAM,DUSTMAX,DUSTMIN,DUSTNU, &
                      DUSTTAU,RREF,ALS0,ALSDUR,BLWINFAC,DELTATEX,DUSTLAT, &
                      DUSTLON,INTENS,RADMAX,REQUA,RPOLE,RPSCALE,RWSCALE, &
                      WLSCALE,WMSCALE
  integer :: MOLAHGTS,MAPYEAR,NPOS,NVARX,NVARY,IDAYDATA,IPCLAT,IU0,IUP, &
             LOGSCALE
  common /TGCMOFFSET_M05/ OFFSETS,TOFFSETS,ZOFFSET,HGTOFFSET,OFSZL,IBOUGHER
  !     For offsets to zoffset: Maybe Read, Not Written
  !     For hgtoffset to ofszL: Not Read, Maybe Written
  !     For ibougher: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /TGCMoffset_M05/ ... 
  real(kind=PM_REEL), dimension(0:12,ndust) :: OFFSETS
  real(kind=PM_REEL), dimension(0:12,ntesy) :: TOFFSETS
  real(kind=PM_REEL) :: HGTOFFSET,OFSZL,ZOFFSET
  integer :: IBOUGHER
  common /THERM_M05/ F107,STDL,FMOL
  !     For F107 to stdl: Maybe Read, Not Written
  !     For fmol: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /THERM_M05/ ... 
  real(kind=PM_REEL) :: F107,STDL
  real(kind=PM_REEL), dimension(0:8) :: FMOL
  common /WAVECOEF_M05/ WAVEA0,WAVEA1,WAVEPHI1,WAVEA2,WAVEPHI2,WAVEA3, &
                        WAVEPHI3,WAVETIME,WAVEDATA,WSCALE,PHI1DOT,PHI2DOT, &
                        PHI3DOT,WAVEDATE,NWAVE,IUWAVE
  !     For WaveA0 to Wavephi3: Maybe Read, Maybe Written
  !     For wavetime to Wscale: Maybe Read, Not Written
  !     For phi1dot to WaveDate: Maybe Read, Maybe Written
  !     For nwave to iuwave: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /Wavecoef_M05/ ... 
  real(kind=PM_REEL) :: WAVEA0,WAVEA1,WAVEA2,WAVEA3,WAVEPHI1,WAVEPHI2,WAVEPHI3
  real(kind=PM_REEL), dimension(100) :: WAVETIME
  real(kind=PM_REEL), dimension(100,11) :: WAVEDATA
  real(kind=PM_REEL) :: WAVEDATE,WSCALE,PHI1DOT,PHI2DOT,PHI3DOT
  integer :: IUWAVE,NWAVE
  ! 
  ! ... Executable Statements ...
  ! 
 
! DM11359 adding initialisation of FACTHI
FACTHI = 0.0d0
FACTLO = 0.0d0

 devmin=0.d0
 devmax=0.d0
 Z1=0.d0
 EOF = 0
  !---  Top height = 170 for 2001 MGCM data = 240 km for TES yr1&2 data   DSTP 40
  TopZ = 170.0d0
  if (MAPYEAR > 0) then
    TopZ = 240.0d0
  end if
  !---  Compute planeto-centric and planeto-graphic coordinates, as       DSTP 43
  !      necessary: CLATc,CHGTc=planeto-centric; CLATg,CHGTg=planeto-     DSTP 44
  !      graphic (same as geodetic)                                       DSTP 45
  if (MOLAHGTS==1 .or. IPCLAT==1) then
    !---    Input CLAT is planeto-centric wrt MOLA or ellipsoid             DSTP 46a
    CLATc = CLAT
    !---    Get MOLA radius Rref and ellipsoid radius Oldrref for current   DSTP 46c
    !         position at height = 0                                        DSTP 46d
    call RELLIPS_M05(CLATc,CLON,RREF,0.0d0,ogz,Oldrref,topohgt,albedo,REQUA, &
                     RPOLE)
    !---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km      DSTP 46g
    if (CHGT > 3.0d3) then
      RSC = CHGT
      if (MOLAHGTS == 1) then
        CHGTc = CHGT - RREF
        HGTM = CHGTc
      else
        CHGTc = CHGT - Oldrref
        HGTM = CHGT - RREF
      end if
    else
      !---      Interpret CHGT as planeto-centric altitude above MOLA or      DSTP 53a
      !         ellipsoid (depending on MOLAhgts value)                       DSTP 53b
      CHGTc = CHGT
      !---      Total radius RSC                                              DSTP 55
      RSC = RREF + CHGTc
      HGTM = CHGTc
      !---      Height wrt ellipsoid if MOLAhgts ne 1                         DSTP 57
      if (MOLAHGTS /= 1) then
        RSC = Oldrref + CHGTc
        HGTM = RSC - RREF
      end if
    end if
    !---    Get equatorial and polar cartesian components                   DSTP 62
    Dxy = RSC * cos(DTR*CLATc)
    Dz = RSC * sin(DTR*CLATc)
    !---    Convert planeto-centric to planeto-graphic                      DSTP 65
    call GeocenttoGeodet_M05(Dxy,Dz,CLATg,CHGTg,REQUA,RPOLE)
  else
    !---    Error for radius input (height > 3000 km) if ipclat ne 1        DSTP 67a
    if (CHGT > 3.0d3) stop " Radius input requires ipclat=1"
    !---    Input CLAT&CHGT are planeto-graphic (wrt to ellipsoid)          DSTP 68
    CLATg = CLAT
    CHGTg = CHGT
    !---    Convert planeto-graphic to planeto-centric                      DSTP 71
    call GeodettoGeocent_M05(CLATg,CHGTg,CLATc,RSC,Dxy,Dz,REQUA,RPOLE)
    !---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)         DSTP 74
    call RELLIPS_M05(CLATc,CLON,RREF,CHGTg,ogz,Oldrref,topohgt,albedo,REQUA, &
                     RPOLE)
    CHGTc = RSC - Oldrref
    HGTM = RSC - RREF
  end if
  !---  Set vertical and horizontal scale parameters                      DSTP 80
  VLS = 8.0d0 * WLSCALE
  HLS = (30.0d0+0.01875d0*HGTM**2)
  if (HLS > 600.0d0) then
    HLS = 600.0d0
  end if
  HLS = HLS * WLSCALE
  !---  Relative displacements between previous and current position      DSTP 85
  DELNS = DTR * RSC * (DELLAT) / HLS
  DELEW = -DTR*RSC*cos(DTR*CLATc)*DELLON/HLS
  DELZ = DELHGT / VLS
  if (NPOS <= 0) then
    !---    Read new position if trajectory data file is being used         DSTP 90
    read (7,*,ERR = 1200,END = 1000) TrajSEC, TrajHGT, TrajLAT, TrajLON
    !---    Convert negative longitudes                                     DSTP 92
    if (TrajLON < 0.0d0) then
      TrajLON = TrajLON + 360.0d0
    end if
    !---    Convert to West Longitude if LonEW = 1                          DSTP 94
    if (LonEW == 1) then
      TrajLON = 360.0d0 - TrajLON
    end if
    if (I > 0) then
      !---      Compute displacement magnitude of new from previous position  DSTP 97
      DELTIME = TrajSEC - CSEC
      DELHGT = TrajHGT - CHGT
      DELLAT = abs(TrajLAT-CLAT)
      DELLON = abs(TrajLON-CLON)
    end if
    !---    Correct DELLON for cases near 0/360 longitude discontinuity     DSTP103
    if (DELLON > 180.0d0) then
      DELLON = 360.0d0 - DELLON
    end if
    if (DELLON < 0.0d0) then
      DELLON = DELLON + 360.0d0
    end if
    !---    Correct DELLON and DELLAT near polar discontinuities            DSTP106
    if (DELLON>90.0d0 .and. (abs(TrajLAT)>=70.0d0.or.abs(CLAT)>=70.0d0)) then
      DELLON = abs(180.0d0-DELLON)
      DELLAT = 180.0d0 - abs(TrajLAT) - abs(CLAT)
    end if
    !---    Relative displacements between previous and current position    DSTP112
    DELNS = DTR * RSC * (DELLAT) / HLS
    DELEW = -DTR*RSC*cos(DTR*CLATc)*DELLON/HLS
    DELZ = DELHGT / VLS
    !---    Set current position to new position                            DSTP116
    CSEC = TrajSEC
    CHGT = TrajHGT
    CLAT = TrajLAT
    CLON = TrajLON
    goto 1100
    1000 EOF = 1
    if (NPOS <= 0) then
      rewind (7)
    end if
    return
  elseif (I > 0) then
    CHGT = CHGT + DELHGT
    CLAT = CLAT + DELLAT
    CLON = CLON + DELLON
    CSEC = CSEC + DELTIME
  end if
  !---  Correct latitude and longitude if position crosses either pole    DSTP127
  1100 if (abs(CLAT) > 90.0d0) then
         CLAT = sign(180.0d0,CLAT) - CLAT
         CLON = CLON + 180.0d0
         DELLAT = -DELLAT
       end if
  if (CLON < 0.0d0) then
    CLON = CLON + 360.0d0
  end if
  if (CLON >= 360.0d0) then
    CLON = CLON - 360.0d0
  end if
  !---  Compute planeto-centric and planeto-grapic coordinates for new    DSTP135
  !     position, as necessary                                            DSTP136
  if (MOLAHGTS==1 .or. IPCLAT==1) then
    !---    Input CLAT is planeto-centric wrt MOLA or ellipsoid             DSTP137a
    CLATc = CLAT
    !---    Get MOLA radius Rref and ellipsoid radius Oldrref for current   DSTP137c
    !         position at height = 0                                        DSTP137d
    call RELLIPS_M05(CLATc,CLON,RREF,0.0d0,ogz,Oldrref,topohgt,albedo,REQUA, &
                     RPOLE)
    !---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km      DSTP137g
    if (CHGT > 3.0d3) then
      RSC = CHGT
      if (MOLAHGTS == 1) then
        CHGTc = CHGT - RREF
        HGTM = CHGTc
      else
        CHGTc = CHGT - Oldrref
        HGTM = CHGT - RREF
      end if
    else
      !---      Interpret CHGT as planeto-centric altitude above MOLA or      DSTP144a
      !         ellipsoid (depending on MOLAhgts value)                       DSTP144b
      CHGTc = CHGT
      !---      Total radius RSC                                              DSTP146
      RSC = RREF + CHGTc
      HGTM = CHGTc
      !---      Height wrt ellipsoid if MOLAhgts ne 1                         DSTP148
      if (MOLAHGTS /= 1) then
        RSC = Oldrref + CHGTc
        HGTM = RSC - RREF
      end if
    end if
    !---    Get equatorial and polar cartesian components                   DSTP153
    Dxy = RSC * cos(DTR*CLATc)
    Dz = RSC * sin(DTR*CLATc)
    !---    Convert planeto-centric to planeto-graphic                      DSTP156
    call GeocenttoGeodet_M05(Dxy,Dz,CLATg,CHGTg,REQUA,RPOLE)
  else
    !---    Error for radius input (height > 3000 km) if ipclat ne 1        DSTP158a
    if (CHGT > 3.0d3) stop " Radius input requires ipclat=1"
    !---    CLAT&CHGT are planeto-graphic (wrt to ellipsoid)                DSTP159
    CLATg = CLAT
    CHGTg = CHGT
    !---    Convert planeto-graphic to planeto-centric                      DSTP162
    call GeodettoGeocent_M05(CLATg,CHGTg,CLATc,RSC,Dxy,Dz,REQUA,RPOLE)
    !---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)         DSTP165
    call RELLIPS_M05(CLATc,CLON,RREF,CHGTg,ogz,Oldrref,topohgt,albedo,REQUA, &
                     RPOLE)
    CHGTc = RSC - Oldrref
    HGTM = RSC - RREF
  end if
  

  DAY = DAY0 + CSEC/8.6400d4

  !---  Update wave coefficients if necessary                             DSTP172
  if (IUWAVE>0 .and. numwave<NWAVE) then
    if (CSEC >= WAVETIME(numwave+1)) then
      do
        numwave = numwave + 1
        WAVEA0 = WAVEDATA(numwave,1)
        WAVEDATE = WAVEDATA(numwave,2)
        WAVEA1 = WAVEDATA(numwave,3)
        WAVEPHI1 = WAVEDATA(numwave,4)
        PHI1DOT = WAVEDATA(numwave,5)
        WAVEA2 = WAVEDATA(numwave,6)
        WAVEPHI2 = WAVEDATA(numwave,7)
        PHI2DOT = WAVEDATA(numwave,8)
        WAVEA3 = WAVEDATA(numwave,9)
        WAVEPHI3 = WAVEDATA(numwave,10)
        PHI3DOT = WAVEDATA(numwave,11)
        !---      Check to see if more than one wave time exceeded              DSTP187
        if (numwave >= NWAVE) exit
        if (CSEC < WAVETIME(numwave+1)) exit
      end do
      !---      Write out updated wave coefficients                           DSTP191
      if (IUP > 0) then
        write (IUP,10000) &
              WAVEA0, WAVEA1, WAVEPHI1, WAVEA2, WAVEPHI2, WAVEA3, WAVEPHI3
        if (WAVEDATE > 0.0d0) then
          write (IUP,10001) WAVEDATE, PHI1DOT, PHI2DOT, PHI3DOT
        end if
        lonvar = "West"
        if (LonEW == 1) then
          lonvar = "East"
        end if
        write (IUP,10002) WSCALE, lonvar
      end if
    end if
  end if
  !---  Write wave coefficients if not read from file                     DSTP209
  if (IUWAVE==0 .and. numwave==0) then
    if (IUP > 0) then
      write (IUP,10000) &
            WAVEA0, WAVEA1, WAVEPHI1, WAVEA2, WAVEPHI2, WAVEA3, WAVEPHI3
      if (WAVEDATE > 0.0d0) then
        write (IUP,10001) WAVEDATE, PHI1DOT, PHI2DOT, PHI3DOT
      end if
      lonvar = "West"
      if (LonEW == 1) then
        lonvar = "East"
      end if
      write (IUP,10002) WSCALE, lonvar
    end if
    numwave = 1
  end if
  !---  Sun and Mars positions at new time                                DSTP222
  !     Use high precision values if inputs > 0 or ephemeris subroutine   DSTP223
  !     otherwise                                                         DSTP224
  if (dradau > 0.0d0) then
    SUNLON = dsunlon
    SUNLAT = dsunlat
    ALS = dsunLs
    MARSAU = dradau
    owlt = dowlt
  else
    !---    Use built-in Mars ephemeris routine                             DSTP232
    !---    Convert to Terrestrial (Dynamical) Time, if necessary           DSTP233
    ttsec = 0.0d0
    if (iutc == 1) then
      !---      Get terrestrial dynamical time offset (seconds)               DSTP236
      dt = (DAY-2451545.0d0) / 36525.0d0
      !---      Terrestrial time offset (in seconds) TT = UTC + ttsec         DSTP238
      ttsec = (64.184d0+95.0d0*dt+35.0d0*dt**2) / 86400.0d0
    end if
    call marsephm_M05(DAY+ttsec,sunlat,sunlon,ALS,MARSAU,owlt,EOT)
    !---    Convert to Mars-Event Time, if necessary                        DSTP242
    if (IERT == 1) then
      call marsephm_M05(DAY+ttsec-owlt/1440.0d0,sunlat,sunlon,ALS,MARSAU,owlt, &
                        EOT)
    end if
    !---    Convert planetographic sun latitude to planetocentric           DSTP245
    sunlat = atan(tan(sunlat*DTR)/(3396.0d0/3378.32d0)**2) / DTR
  end if
  !---  Evaluate longitude-dependent wave perturbation                    DSTP248
  call Wavelon_M05(LonEW,CLON,CLATc,HGTM,DAY,wavepert)
  !---  Convert wave perturbation to % for output                         DSTP250
  denswave = 100.0d0 * wavepert
  !---  Evaluate atmospheric parameters                                   DSTP252
  call ATMOS2_M05(HGTM,CLATc,CLON,MARSAU,SUNLAT,SUNLON,ALS,HSCALE,TEMP,DENS, &
                  FACTHI,FACTLO,PRES,thgt,careoid,ZF,IU0,DELTATEX,Texos,Tbase, &
                  Hrho,AMz,DUSTTAU,DUSTMIN,DUSTMAX,DUSTOD,EWWIND,NSWIND, &
                  blwindew,blwindns,blwindvert,HatZF,wavepert,TempDay,PresDay, &
                  DensDay,EWwnDay,NSwnDay,bluday,blvday,hgtasfc,Patsurf, &
                  Tempmax,Tempmin,Densmax,Densmin,Tgrnd,talb,icepolar,gz, &
                  Oldrref,REQUA,RPOLE,MAPYEAR,profnear,proffar,nprof,profwgt, &
                  IDAYDATA)




  !---  Save value of height of 1.26 nbar level                           DSTP260
  Zbase = ZF
  !---  Compute exponential correlation across displacement from          DSTP262
  !     previous position.                                                DSTP263
  !---  Include effects of time displacement, using wind speed magnitude  DSTP264
  !     and horizontal perturbation scale size.                           DSTP265
  dtimecor = sqrt(EWWIND**2+NSWIND**2) * DELTIME / (1000.0d0*HLS)
  !---  Unperturbed mean density (approximate if HGTM > ZF)               DSTP267
  DENS0 = DENS / (1.0d0+wavepert)
  !---  HGTM = height above MOLA areoid                                   DSTP269
  !---  HGTMS = height above local MOLA topographic surface               DSTP270
  HGTMS = HGTM - thgt
  !---  Evaluate correlation and step size relative to accuracy limit     DSTP272
  if (iupdate >= 0) then
    pertstep = pertstep + abs(DELNS) + abs(DELEW) + abs(DELZ) + abs(dtimecor)
  end if
  corlim = -pertstep/log(0.995d0)
  if (corlim<=corlmin .or. iupdate<0) then
    CORREL = 1.0d0
    corbeta = 0.0d0
    if (iupdate < 0) then
      iupdate = -1
    else
      iupdate = 0
    end if
  else
    do
      !---    Get uniform RANDOM number and Gaussian random number from PPND  DSTP285
      Z2 = RANDOM_M05(L)
      if (L /= 1) exit
    end do
    Z1 = PPND_M05(Z2,IFAULT)
    if (IFAULT == 1) stop " PPND ERROR"
    CORREL = exp(-pertstep)
    corbeta = sqrt(1.0d0-CORREL**2)
    pertstep = 0.0d0
    iupdate = 1
  end if
  DENSHI = DENS * FACTHI
  DPLUS = DENSHI - DENS
  if (FACTLO <= 0.0d0) then
    FACTLO = 2.0d0 - FACTHI
  end if
  DENSLO = DENS * FACTLO
  DMINUS = DENS - DENSLO

  !---  Local time in "Martian hours" (1/24th Sols)                       DSTP300
  TLOCAL = 12.0d0 + (SUNLON-CLON)/15.0d0
  if (TLOCAL < 0.0d0) then
    TLOCAL = TLOCAL + 24.0d0
  end if
  if (TLOCAL > 24.0d0) then
    TLOCAL = TLOCAL - 24.0d0
  end if
  !---  Output height above MOLA areoid (HGTM) or above local             DSTP304
  !---  terrain (HGTMS)                                                   DSTP305
  OHGT = HGTM
  OHGTS = HGTMS
  !---  Set output heights to terrain height if <= -8.7 km                DSTP308
  if (OHGT <= -8.7d0) then
    OHGT = thgt + hgtasfc
    OHGTS = hgtasfc
  end if
  !---  Current random density perturbation value, correlated with        DSTP313
  !---  previous random density perturbation                              DSTP314
  RHOd = CORREL*RHOd + corbeta*Z1
  if (RHOd < 0.0d0) then
    DensRand = RHOd * DMINUS * RPSCALE
  end if
  if (RHOd >= 0.0d0) then
    DensRand = RHOd * DPLUS * RPSCALE
  end if
  !---  Add random density perturbation                                   DSTP318
  DENSP = DENS + DensRand
  !---  Check upper and lower bounds on density perturbations             DSTP320
  if (DENSP < 0.1d0*DENS0) then
    DENSP = 0.1d0 * DENS0
  end if
  if (DENSP > 10.0d0*DENS0) then
    DENSP = 10.0d0 * DENS0
  end if
  !---  Save as total density, for output                                 DSTP323
  DENSTOT = DENSP
  !---  Standard deviation in random density perturbation (% of           DSTP325
  !---  unperturbed mean) for output                                      DSTP326
  SIGD = 50.0d0 * abs(DENSHI-DENSLO) / DENS0
  SIGD = RPSCALE * SIGD
  !---  Standard deviations for wind perturbations                        DSTP329
  SIGU = 2.0d0 + 0.1d0*CHGTc
  !---  Added contribution to SIGU for near-surface heights               DSTP330a
  if (OHGTS>=0.0d0 .and. OHGTS<=4.5d0) then
    SIGU = SIGU + 1.5d0*(1.0d0-OHGTS/4.5d0)
  end if
  if (SIGU > 25.0d0) then
    SIGU = 25.0d0
  end if
  SIGU = RWSCALE * SIGU
  if (SIGU > 50.0d0) then
    SIGU = 50.0d0
  end if
  SIGW = SIGU / 5.0d0
  !---  Added contribution to SIGW for near-surface heights               DSTP334a
  if (OHGTS>=0.0d0 .and. OHGTS<=4.5d0) then
    SIGW = SIGW + 1.5d0*(1.0d0-OHGTS/4.5d0)
  end if
  !---  Adjust random DENSHI, DENSLO for rpscale                          DSTP335
  DENSHI = DENS + RPSCALE*(DENSHI-DENS)
  DENSLO = DENS + RPSCALE*(DENSLO-DENS)
  if (DENSLO < 0.1d0*DENS0) then
    DENSLO = 0.1d0 * DENS0
  end if
  !---  Convert random density perturbation to % of (unperturbed) mean    DSTP339
  DensRand = 100.0d0 * (DENSP-DENS) / DENS0
  !---  Compute total density perturbation as % of (unperturbed) mean     DSTP341
  DENSP = DensRand + denswave
  !---  Compute EW and NS wind perturbations and total wind               DSTP343
  do while (corbeta /= 0.0d0)
    Z2 = RANDOM_M05(L)
    if (L /= 1) then
      Z1 = PPND_M05(Z2,ifault)
      exit
    end if
  end do
  RHOu = CORREL*RHOu + corbeta*Z1
  !---  Limit winds to sound speed/Sqrt(2)   (ssposr2)                    DSTP348
  !     Assume specific heat ratio = 4/3                                  DSTP349
  ssposr2 = sqrt(Cp_M05(TEMP)*TEMP/6.0d0)
  !---  Add slope winds, scale mean winds, and limit to 0.7*ssposr2       DSTP351
  EWWIND = WMSCALE*EWWIND + BLWINFAC*blwindew
  NSWIND = WMSCALE*NSWIND + BLWINFAC*blwindns
  if (abs(EWWIND) > 0.7d0*ssposr2) then
    EWWIND = sign(0.7d0*ssposr2,EWWIND)
  end if
  if (abs(NSWIND) > 0.7d0*ssposr2) then
    NSWIND = sign(0.7d0*ssposr2,NSWIND)
  end if
  !---  Add slope winds, scale daily mean winds, and limit to 0.7*ssposr2 DSTP358
  EWwnDay = WMSCALE*EWwnDay + BLWINFAC*bluday
  NSwnDay = WMSCALE*NSwnDay + BLWINFAC*blvday
  if (abs(EWwnDay) > 0.7d0*ssposr2) then
    EWwnDay = sign(0.7d0*ssposr2,EWwnDay)
  end if
  if (abs(NSwnDay) > 0.7d0*ssposr2) then
    NSwnDay = sign(0.7d0*ssposr2,NSwnDay)
  end if
  !---  EW component of perturbation in wind and total wind               DSTP365
  EWpert = RHOu * SIGU 
  EWtot = EWWIND + EWpert
  if (abs(EWtot) > ssposr2) then
    EWtot = sign(ssposr2,EWtot)
    EWpert = EWtot - EWWIND
  end if
  do while (corbeta /= 0.0d0)
    Z2 = RANDOM_M05(L)
    if (L /= 1) then
      Z1 = PPND_M05(Z2,ifault)
      exit
    end if
  end do
  RHOv = CORREL*RHOv + corbeta*Z1
  !---  NS component of perturbation in wind and total wind               DSTP376
  NSpert = RHOv * SIGU
  NStot = NSWIND + NSpert
  if (abs(NStot) > ssposr2) then
    NStot = sign(ssposr2,NStot)
    NSpert = NStot - NSWIND
  end if
  do while (corbeta /= 0.0d0)
    Z2 = RANDOM_M05(L)
    if (L /= 1) then
      Z1 = PPND_M05(Z2,ifault)
      exit
    end if
  end do
  RHOw = CORREL*RHOw + corbeta*Z1
  !---  Vertical component of perturbation in wind plus slope wind        DSTP387
  VWpert = RHOw*SIGW + BLWINFAC*blwindvert
  !---  Compute cosine of solar zenith angle                              DSTP389
  cszang = sin(DTR*SUNLAT)*sin(DTR*CLATc) + &
           cos(DTR*SUNLAT)*cos(DTR*CLATc)*cos(DTR*(SUNLON-CLON))
  szang = acos(cszang) / DTR
  F1peak = 999.9d0
  !---  Compute height of F1 peak if solar zenith angle < 90 deg          DSTP394
  if (cszang>0.0d0 .and. Zbase<900.0d0) then
    !---    relative Air mass                                               DSTP396
    airmass = 1.0d0 / (cszang+0.15d0*(93.885d0-szang)**(-1.253d0))
    !---    F1 peak height (km)                                             DSTP398
    F1peak = Zbase + HatZF*log(airmass)
  end if
  !---  Write descriptively formatted data on LIST.txt file               DSTP401
  Elon = 360.0d0 - CLON
  !---  Total radius (areoid + height)                                    DSTP403
  radtotal = careoid + OHGT
  !---  Compute height above reference ellipsoid                          DSTP405
  Oldhgt = radtotal - Oldrref
  !---  Difference of MOLA areoid from ellipsoid radius                   DSTP407
  dAreoid = careoid - Oldrref
  !---  Set output value of local height offset for MGCM or MTGCM data    DSTP409
  offsetL = HGTOFFSET
  if (OHGT <= 80.0d0) then
    offsetL = OFSZL
  end if
  if (IUP > 0) then
    write (IUP,10003) &
          CSEC,CSEC/onesol,ALS,DUSTOD,OHGT,OHGTS,owlt,thgt,radtotal,careoid, &
          Oldhgt,HSCALE,Hrho,IBOUGHER,offsetL,CLATc,CLON,Elon,CLATg,CHGTg, &
          SUNLAT,MARSAU,SUNLON,TLOCAL
  end if
  if (IUP>0 .and. OHGT>80.0d0) then
    write (IUP,10004) Texos, Tbase, Zbase
    write (IUP,10005) szang, F1peak
  end if
  !---  Compute percent deviations from COSPAR values                     DSTP437
  call cospar_M05(OHGT,tcos,pcos,dcosp)
  if (dcosp <= 0.0d0) then
    devlo = -99.9d0
    devav = -99.9d0
    devhi = -99.9d0
    devtot = -99.9d0
    devDay = -99.9d0
    devmax = -99.9d0
    devmin = -99.9d0
  else
    devlo = 100.0d0 * (DENSLO-dcosp) / dcosp
    devav = 100.0d0 * (DENS-dcosp) / dcosp
    devhi = 100.0d0 * (DENSHI-dcosp) / dcosp
    devtot = 100.0d0 * (DENSTOT-dcosp) / dcosp
    devDay = 100.0d0 * (DensDay-dcosp) / dcosp
    if (IDAYDATA > 0) then
      devmax = 100.0d0 * (Densmax-dcosp) / dcosp
      devmin = 100.0d0 * (Densmin-dcosp) / dcosp
    end if
  end if
  densunits = "kg/m**3 "
  !---  Convert density units to kg/km**3 if logscale = 3                 DSTP459
  if (LOGSCALE == 3) then
    DENS = 1.0d9 * DENS
    DENSLO = 1.0d9 * DENSLO
    DENSHI = 1.0d9 * DENSHI
    DENSTOT = 1.0d9 * DENSTOT
    DensDay = 1.0d9 * DensDay
    if (IDAYDATA > 0) then
      Densmax = 1.0d9 * Densmax
      Densmin = 1.0d9 * Densmin
    end if
    densunits = "kg/km**3"
  end if
  !---  Write formatted output to list file                               DSTP472
  if (IUP > 0) then
    write (IUP,10006) &
          TEMP,PRES,profwgt,DENSLO,DENS,DENSHI,densunits,devlo,devav,devhi, &
          iupdate,DENSTOT,densunits,DENSP,denswave,EWWIND,EWpert,EWtot,NSWIND, &
          NSpert,NStot,VWpert
    call species_M05(OHGT,CLATc,als,Zbase,AMz,DENS,PRES,TEMP,IUP,FMOL,fmass, &
                     fmolH2O,fmassH2O)
    if (I>0 .and. corlim<1.0d0 .and. abs(CLAT)<89.99d0) then
      write (IUP,10007) corlim
    end if
    write (IUP,10008)
  end if
  PRESmb = PRES / 100.d0
  sigmalevel = PRES / Patsurf
  preshgt = -HSCALE*log(sigmalevel)
  !---  Compute dust density variables by methods of Haberle et al.,      DSTP498
  !      Icarus, 50, 322 (1982) and Haberle et al., J. Geophys. Res.,     DSTP499
  !      104, 8957 (1999)                                                 DSTP500
  !---  Dust column areal density (kg/m**2)                               DSTP501
  dareaden = 5.0d-3 * DUSTOD
  !---  Dust mixing ratio (kg dust/kg air) at surface                     DSTP503
  qsurf = dareaden * gz / (0.994d0*exp(-DUSTNU)*Patsurf)
  !---  Dust mixing ratio at current position and pressure                DSTP505
  dmixrat = 0.0d0
  expfact = DUSTNU * (1.0d0-1.0d0/sigmalevel)
  if (expfact > -85.0d0) then
    dmixrat = qsurf * exp(expfact)
  end if
  !---  Dust mass density (micrograms dust / m**3)                        DSTP509
  dmasden = 1.0d9 * dmixrat * DENS
  if (LOGSCALE == 3) then
    dmasden = dmasden / 1.0d9
  end if
  !---  Dust number density (number dust particles / m**3)                DSTP511
  dnumden = dmasden / (5.23599d-10*DUSTDENS*DUSTDIAM**3)
  if (NVARX == 9) then
    VARX = PRESmb
  end if
  if (NVARY == 9) then
    VARY = PRESmb
  end if
  if (NVARX == 10) then
    VARX = preshgt
  end if
  if (NVARY == 10) then
    VARY = preshgt
  end if
  if (NVARX == 11) then
    VARX = sigmalevel
  end if
  if (NVARY == 11) then
    VARY = sigmalevel
  end if
  if (NVARX == 12) then
    VARX = Oldhgt
  end if
  if (NVARY == 12) then
    VARY = Oldhgt
  end if
  if (NVARX == 13) then
    VARX = CHGTg
  end if
  if (NVARY == 13) then
    VARY = CHGTg
  end if
  if (NVARX == 14) then
    VARX = CLATg
  end if
  if (NVARY == 14) then
    VARY = CLATg
  end if
  !---  Output deviations from COSPAR if logscale = 2                     DSTP525
  if (LOGSCALE == 2) then
    DENSLO = devlo
    DENS = devav
    DENSHI = devhi
    DENSTOT = devtot
    DensDay = devDay
    Densmax = devmax
    Densmin = devmin
    if (pcos <= 0.0d0) then
      PRES = -99.9d0
      PresDay = -99.9d0
    else
      PRES = 100.0d0 * (PRES-pcos) / pcos
      PresDay = 100.0d0 * (PresDay-pcos) / pcos
    end if
  end if
  !---  Write parameters on plot format files                             DSTP542
  if (NVARX == 1) then
    VARX = OHGT
  end if
  if (NVARX == 2) then
    VARX = OHGTS
  end if
  if (NVARX == 3) then
    VARX = CLATc
  end if
  if (NVARX == 4) then
    VARX = CLON
    if (LonEW == 1) then
      VARX = 360.0d0 - CLON
    end if
  end if
  if (NVARX == 15) then
    VARX = CLON
    if (VARX > 180.0d0) then
      VARX = VARX - 360.0d0
    end if
    if (LonEW == 1) then
      VARX = -VARX
    end if
  end if
  if (NVARX == 5) then
    VARX = CSEC
  end if
  if (NVARX == 6) then
    VARX = CSEC / onesol
  end if
  if (NVARX == 7) then
    VARX = ALS
  end if
  if (NVARX == 8) then
    VARX = TLOCAL
  end if
  Alogdens = 0.0d0
  if (LOGSCALE /= 2) then
    Alogdens = log10(DENS)
  end if
  if (LOGSCALE == 1) then
    DENS = Alogdens
    PRES = log10(PRES)
    DENSLO = log10(DENSLO)
    DENSHI = log10(DENSHI)
    DENSTOT = log10(DENSTOT)
    if (OHGT <= TopZ) then
      DensDay = log10(DensDay)
      PresDay = log10(PresDay)
      if (IDAYDATA > 0) then
        Densmax = log10(Densmax)
        Densmin = log10(Densmin)
      end if
    end if
  end if
  if (NVARY==0 .and. IUP>0) then
    write (21,10019) &
          VARX, DENSLO, DENS, DENSHI, DENSTOT, DUSTOD, radtotal, gz, MARSAU, &
          LOGSCALE, offsetL, IBOUGHER, MAPYEAR, profwgt
    write (22,10013) &
          VARX, SIGD, DensRand, denswave, DENSP, corlim, SIGU, SIGW, iupdate
    write (23,10010) &
          VARX, EWWIND, EWpert, EWtot, NSWIND, NSpert, NStot, VWpert, iupdate
    write (24,10021) &
          VARX, TEMP, PRES, TEMP-273.15d0, PRESmb, Hrho, HSCALE, AMz, thgt, &
          Tgrnd, careoid, dAreoid, FMOL, fmolH2O, LOGSCALE
    if (OHGT<=TopZ .and. profnear<=0.0d0) then
      write (25,10009) &
            VARX, TempDay, PresDay, DensDay, EWwnDay, NSwnDay, Tempmin, &
            Tempmax, Densmin, Densmax, LOGSCALE
    end if
    if (OHGT > 80.0d0) then
      write (26,10014) &
            VARX, Tbase, Zbase, F1peak, AMz, Texos, HGTOFFSET, IBOUGHER
    end if
    write (27,10016) &
          VARX, talb, cszang, dareaden, dmixrat, dmasden, dnumden, icepolar
  elseif (IUP > 0) then
    if (NVARY == 1) then
      VARY = OHGT
    end if
    if (NVARY == 2) then
      VARY = OHGTS
    end if
    if (NVARY == 3) then
      VARY = CLATc
    end if
    if (NVARY == 4) then
      VARY = CLON
      if (LonEW == 1) then
        VARY = 360.0d0 - CLON
      end if
    end if
    if (NVARY == 15) then
      VARY = CLON
      if (VARY > 180.0d0) then
        VARY = VARY - 360.0d0
      end if
      if (LonEW == 1) then
        VARY = -VARY
      end if
    end if
    if (NVARY == 5) then
      VARY = CSEC
    end if
    if (NVARY == 6) then
      VARY = CSEC / onesol
    end if
    if (NVARY == 7) then
      VARY = ALS
    end if
    if (NVARY == 8) then
      VARY = TLOCAL
    end if
    write (21,10020) &
          VARX, VARY, DENSLO, DENS, DENSHI, DENSTOT, DUSTOD, radtotal, gz, &
          MARSAU, LOGSCALE, offsetL, IBOUGHER, MAPYEAR, profwgt
    write (22,10018) &
          VARX, VARY, SIGD, DensRand, denswave, DENSP, corlim, SIGU, SIGW, &
          iupdate
    write (23,10011) &
          VARX, VARY, EWWIND, EWpert, EWtot, NSWIND, NSpert, NStot, VWpert, &
          iupdate
    write (24,10022) &
          VARX, VARY, TEMP, PRES, TEMP-273.15d0, PRESmb, Hrho, HSCALE, AMz, &
          thgt, Tgrnd, careoid, dAreoid, FMOL, fmolH2O, LOGSCALE
    if (OHGT<=TopZ .and. profnear<=0.0d0) then
      write (25,10012) &
            VARX, VARY, TempDay, PresDay, DensDay, EWwnDay, NSwnDay, Tempmin, &
            Tempmax, Densmin, Densmax, LOGSCALE
    end if
    if (OHGT > 80.0d0) then
      write (26,10015) &
            VARX, VARY, Tbase, Zbase, F1peak, AMz, Texos, HGTOFFSET, IBOUGHER
    end if
    write (27,10017) &
          VARX, VARY, talb, cszang, dareaden, dmixrat, dmasden, dnumden, &
          icepolar
  end if
  !---  Write non-descriptively formatted data on OUTPUT file             DSTP641
  if (MOLAHGTS == 1) then
    VAR = OHGT
    if (NVARX==2 .or. NVARY==2) then
      VAR = OHGTS
    end if
    OLAT = CLATc
  else
    VAR = Oldhgt
    OLAT = CLATc
    if (IPCLAT /= 1) then
      VAR = CHGTg
      OLAT = CLATg
    end if
  end if
  if (IUP <= 0) return
  OLON = CLON
  if (LonEW == 1) then
    OLON = 360.0d0 - CLON
  end if
  if ((NVARX==15.or.NVARY==15) .and. OLON>180.0d0) then
    OLON = OLON - 360.0d0
  end if
  if (LOGSCALE==0 .or. LOGSCALE==3) then
    write (29,10023) &
          CSEC, VAR, OLAT, OLON, dens, TEMP, EWWIND, NSWIND, SIGD, ALS, &
          DUSTOD, fmass, fmassH2O
  else
    write (29,10024) &
          CSEC, VAR, OLAT, OLON, dens, TEMP, EWWIND, NSWIND, SIGD, ALS, &
          DUSTOD, fmass, fmassH2O
  end if
  return
  1200 stop " Error termination reading trajectory data file!"
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format (" A0,A1,phi1,A2,phi2,A3,phi3=",f6.3,3(f6.3,f7.1))
  10001 format ( &
        " Traveling wave phases initialized at Julian DAY"," =",f13.3/ &
        " at phase rates (phi1dot,phi2dot,phi3dot,"," deg/day)=",3f8.3)
  10002 format ( &
        "   Wave Scale =",f8.1," km.    Wave phases are in"," degrees of ",a4, &
        " Longitude")
  10003 format ( &
        " Time (rel. to T0) =",f10.1," sec. (",f8.3," sols)","  Ls =",f6.1, &
        "  Dust =",f5.2/" Height Above MOLA (or Surface)"," =",f8.3," km (" &
        ,f8.3," km)  OWLT ="f6.2," Min"/" Topographic Height = ",f8.3, &
        " km   Radius (Areoid) = ",f8.3," (",f8.3,") km"/ &
        " Hgt Above Ellipsoid =",f8.3," km  "," Scale Hgt H(p)=",f7.2, &
        " H(rho)=",f7.2," km"/" Height Offset Parameters:   ibougher =",i2, &
        "    Local Height"," Offset =",f7.3," km"/" Planeto-Centric Lat = " &
        ,f7.2," deg  Longitude = ",f7.2," W (",f7.2," E) deg."/ &
        " Planeto-Graphic Lat =",f8.2," deg  Planeto-Graphic Hgt (Ellps)=" &
        ,f9.3, " km"/ &
        " Planeto-Cent Sun Lat = ",f6.2," deg  Mars Orbital Radius =",f6.3, &
        " AU"/" Sun Longitude =",f8.2," deg.W      Local True Solar Time = " &
        ,f6.2," Mars hrs")
  10004 format ( &
        " Exospheric Temp. = ",f6.1," K",8x,"Tbase = ",f6.1," K",3x, &
        " Zbase = ",f6.1," km")
  10005 format (" Solar Zenith Angle =",f6.1," deg     F1 peak =",f6.1," km")
  10006 format ( &
        " Temperature = ",f7.1," K",7x," Pressure =",1p,e10.3,0p, &
        " N/m**2   profwgt =",f6.3/" Density (Low, Avg., High) =", &
        1p,3e12.3,0p,1x,a8/" Departure, COSPAR NH Mean =",f9.1," %",2(f10.1, &
        " %"),"  iupdate ="i2/" Tot.Dens. =",1p,e10.3,0p,1x,a8, &
        "  Dens.Pert. =",f7.2,"% Wave =",f7.2,"% of mean"/ &
        " Eastward Wind (Mean,Perturbed,Total) =",3f7.1," m/s   "," VertWind"/ &
        " Northward Wind(Mean,Perturbed,Total) =",3f7.1," m/s",f8.1," m/s")
  10007 format ( &
        " Warning: Step size smaller than accuracy limit by a ","factor of" &
        ,f6.3)
  10008 format ( &
        " -----------------------------------------------------", &
        "------------------------")
  10009 format (g13.5,f7.1,1p,2e11.3,0p,4f8.1,1p,2e11.3,i5)
  10010 format (g13.5,7f8.2,i3)
  10011 format (2g13.5,7f8.2,i3)
  10012 format (2g13.5,f7.1,1p,2e11.3,0p,4f8.1,1p,2e11.3,i5)
  10013 format (g13.5,f6.2,3f10.3,1p,e10.3,0p,2f7.2,i5)
  10014 format (g13.5,3f8.1,f8.2,f8.1,f10.3,i6)
  10015 format (2g13.5,3f8.1,f8.2,f8.1,f10.3,i6)
  10016 format (g13.5,f6.3,f9.5,1p,4e9.2,0p,i3)
  10017 format (2g13.5,f6.3,f9.5,1p,4e9.2,0p,i3)
  10018 format (2g13.5,f6.2,3f10.3,1p,e10.3,0p,2f7.2,i5)
  10019 format (g13.5,1p,4e11.3,0p,f7.4,f9.3,2f6.3,i5,f13.3,i6,i8,f9.3)
  10020 format (2g13.5,1p,4e11.3,0p,f7.4,f9.3,2f6.3,i5,f13.3,i6,i8,f9.3)
  10021 format ( &
        g13.5,2(f7.1,1p,e11.3,0p),2f8.2,f6.2,f7.3,f6.1,f9.3,f8.3,10f6.2,i5)
  10022 format ( &
        2g13.5,2(f7.1,1p,e11.3,0p),2f8.2,f6.2,f7.3,f6.1,f9.3,f8.3,10f6.2,i5)
!  DM1359: improvement of precision F90: pour affichage OUTPUT.TXT en precision scientifique
!  10023 format (f10.0,2f7.2,f8.2,1p,e9.2,0p,3f7.1,2f6.1,f5.2,10f6.2)
!  10024 format (f10.0,2f7.2,f8.2,f9.2,3f7.1,2f6.1,f5.2,10f6.2)
  10023 format (21g30.19)
  10024 format (21g30.19)
end subroutine Datastep_M05
!-----------------------------------------------------------------------DSTP676
real(kind=PM_REEL) function dustvsls_M05(als,Dustmin,Dustmax)
  !---  Assumed seasonal variation (versus solar angle Ls) for non-       DVLS  2
  !     dust-storm optical depth (Dustmin at Ls=90; Dustmax at Ls=270)    DVLS  3
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: Dustmax,Dustmin,als
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: pi180
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, sin
  ! 
  ! ... Executable Statements ...
  ! 
  pi180 = atan(1.0d0) / 45.0d0
  dustvsls_M05 = ((DustMax+Dustmin)-(Dustmax-Dustmin)*sin(pi180*als)) / 2.0d0
end function dustvsls_M05
!-----------------------------------------------------------------------DVLS 10
subroutine EScalc_M05(stdl,SIGMA,ES)
  !---  EPS ARE STD VARNS FROM NOMINAL VALUES                             ESCL  2
  !---  0,2,....10  LONG - TERM                                           ESCL  3
  !---  1,3.....,11  SHORT-TERM                                           ESCL  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: SIGMA,stdl
  real(kind=PM_REEL), dimension(0:11), intent(out) :: ES
  ! 
  !.. Local Scalars .. 
  integer :: I
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(0:11) :: EPS
  real(kind=PM_REEL), dimension(0:11) :: SIG
  ! 
  !.. Data Declarations .. 
  data EPS/12*0.0d0/
  ! 
  ! ... Executable Statements ...
  ! 
  do I = 2,10,2
    EPS(I) = stdl
  end do
  do I = 3,9,2
    EPS(I) = SIGMA
  end do
  SIG(0) = 0.25d0
  !---  LONG TERM -FBAR                                                   ESCL 15
  !---  THESE ARE COEF. OF VARIATION, I.E. SIGMA/MU,    %/100.0           ESCL 16
  SIG(1) = 0.0d0
  !---  SHORT TERM-FBAR                                                   ESCL 18
  SIG(2) = 0.16d0
  !---  LONG TERM-TINF                                                    ESCL 20
  SIG(3) = 0.12d0
  !---  SHORT TERM-TINF                                                   ESCL 22
  SIG(4) = 0.40d0
  !---  LONG TERM - FOXY                                                  ESCL 24
  SIG(5) = 0.12d0
  !---  SHORT TERM -FOXY                                                  ESCL 26
  SIG(6) = 0.0d0
  !---  LONG TERM -AOXY                                                   ESCL 28
  SIG(7) = 0.21d0
  !---  SHORT TERM -AOXY                                                  ESCL 30
  SIG(8) = 0.045d0
  !---  LONG TERM - ZF                                                    ESCL 32
  SIG(9) = 0.0225d0
  !---  SHORT TERM - ZF                                                   ESCL 34
  SIG(10) = 0.30d0
  !---  LONG TERM- DZDUST                                                 ESCL 36
  SIG(11) = 0.0d0
  !---  SHORT TERM - DZDUST                                               ESCL 38
  do I = 0,11
    ES(I) = EPS(I) * SIG(I)
  end do
end subroutine EScalc_M05
!-----------------------------------------------------------------------ESCL 44
subroutine FourD_M05(dx,dy,dz,dq,Array,Value)
  !---    4-Dimensional linear interpolation within a 1x1x1x1 hypercube   FORD  2
  !       (x,y,z,q) of Array(2,2,2,2) to position dx,dy,dz,dq (all 0-1).  FORD  3
  !       Value is value of interpolated output.                          FORD  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: dq,dx,dy,dz
  real(kind=PM_REEL), dimension(2,2,2,2), intent(in) :: Array
  real(kind=PM_REEL), intent(out) :: Value
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: dqp,dxp,dyp,dzp
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Complementary displacements in x,y,z,q                          FORD  7
  dxp = 1.0d0 - dx
  dyp = 1.0d0 - dy
  dzp = 1.0d0 - dz
  dqp = 1.0d0 - dq
  !---    Compute 4-dimensional linear interpolated value                 FORD 12
  Value = dxp*dyp*dzp*dqp*Array(1,1,1,1) + dxp*dyp*dzp*dq*Array(1,1,1,2) + &
          dxp*dyp*dz*dqp*Array(1,1,2,1) + dxp*dy*dzp*dqp*Array(1,2,1,1) + &
          dx*dyp*dzp*dqp*Array(2,1,1,1) + dxp*dyp*dz*dq*Array(1,1,2,2) + &
          dxp*dy*dzp*dq*Array(1,2,1,2) + dxp*dy*dz*dqp*Array(1,2,2,1) + &
          dx*dyp*dzp*dq*Array(2,1,1,2) + dx*dyp*dz*dqp*Array(2,1,2,1) + &
          dx*dy*dzp*dqp*Array(2,2,1,1) + dxp*dy*dz*dq*Array(1,2,2,2) + &
          dx*dyp*dz*dq*Array(2,1,2,2) + dx*dy*dzp*dq*Array(2,2,1,2) + &
          dx*dy*dz*dqp*Array(2,2,2,1) + dx*dy*dz*dq*Array(2,2,2,2)
end subroutine FourD_M05
!-----------------------------------------------------------------------FORD 31
subroutine MGCMterp_M05(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,VMGCM,TempDay, &
                        PresDay,DensDay,UwndDay,VwndDay,Tempmax,Tempmin, &
                        Densmax,Densmin,idaydata)
  !---    Interpolates Ames Mars General Circulation Model (MGCM) data    GTRP  4
  !       to a given latitude, time of year (Ls), and dust optical        GTRP  5
  !       depth, for a given height index (khgt) and time of day (time).  GTRP  6
  !       Some input data is provided by the Common "Interp".             GTRP  7
  !---    Set parameter values for number of heights, latitudes, and      GTRP  8
  !       number of dust optical depth values                             GTRP  9
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nhgt = 17
  integer, parameter :: nlat = 25
  integer, parameter :: ndust = 3
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgt
  real(kind=PM_REEL), intent(in) :: time
  real(kind=PM_REEL), intent(out) :: &
    DMGCM,DensDay,Densmax,Densmin,PMGCM,PresDay,TMGCM,TempDay,Tempmax,Tempmin, &
    UMGCM,UwndDay,VMGCM,VwndDay
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,l,m
  real(kind=PM_REEL) :: A1,A1p,A1t,A2,A2p,A2t,D0,Dtime,P0,P1,P1p,P1t,P2,P2p,P2t, &
                      Ptime,RMGCM,T0,Ttime,U0,V0,polefac,upolefac,xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2,2) :: &
    Dmax,Dmin,PM,Pday,R0,TM,Tday,Tmax,Tmin,UM,Uday,VM,Vday
  ! 
! DM1359 remove : private  
!.. External Calls .. 
!  external ThreeD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: TideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic real
  ! 
  !.. Common Blocks .. 
  common /INTERP_M05/ DLAT,DLON,DLS,DDUST,DLATW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                      ILAT,JLON,LS,MDUST,K1ST,ILATW,ILATT,MF10
  !     For dlat: Read, Not Written
  !     For dlon: Not Read, Not Written
  !     For dls to dlatw: Read, Not Written
  !     For dlatt to df10: Not Read, Not Written
  !     For wpolefac: Maybe Read, Not Written
  !     For tpolefac: Not Read, Not Written
  !     For ilat: Read, Not Written
  !     For jlon: Not Read, Not Written
  !     For ls to mdust: Read, Not Written
  !     For k1st: Not Read, Not Written
  !     For ilatw: Read, Not Written
  !     For ilatt to mf10: Not Read, Not Written
  ! 
  !... Variables in Common Block /Interp_M05/ ... 
  real(kind=PM_REEL) :: DDUST,DF10,DLAT,DLATT,DLATW,DLON,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,K1ST,LS,MDUST,MF10
  common /MGCMDATA_M05/ TZA0,TZA1,TZP1,TZA2,TZP2,PZA0,PZA1,PZP1,PZA2,PZP2, &
                        DZA0,UZA0,UZA1,UZP1,UZA2,UZP2,VZA0,VZA1,VZP1,VZA2,VZP2
  !     For TZA0 to VZP2: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /MGCMdata_M05/ ... 
  real(kind=PM_REEL), dimension(nhgt,nlat,0:12,ndust) :: &
    DZA0,PZA0,PZA1,PZA2,PZP1,PZP2,TZA0,TZA1,TZA2,TZP1,TZP2,UZA0,UZA1,UZA2, &
    UZP1,UZP2,VZA0,VZA1,VZA2,VZP1,VZP2
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MGCM values at corners of a 3-dimensional cube in     GTRP 31
  !       latitude-Ls-dust space, at the given height index (khgt), and   GTRP 32
  !       time of day (time)                                              GTRP 33
  do i = 1,2
    polefac = 1.0d0
    upolefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == nlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    if (ILATW == 2) then
      if (i == 1) then
        upolefac = WPOLEFAC
      end if
    elseif (ILATW==nlat-1 .and. i==2) then
      upolefac = WPOLEFAC
    end if
    do l = 1,2
      do m = 1,2
        !---      Daily mean temperature                                        GTRP 49
        T0 = TZA0(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        Tday(i,l,m) = T0
        !---      Temperature tide amplitudes and phases                        GTRP 52
        A1t = TZA1(khgt,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P1t = TZP1(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        A2t = TZA2(khgt,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P2t = TZP2(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Temperature at corners of 3-D cube                            GTRP 57
        TM(i,l,m) = TideX_M05(T0,A1t,P1t,A2t,P2t,time)
        !---      Daily mean pressure                                           GTRP 59
        P0 = PZA0(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        Pday(i,l,m) = P0
        !---      Pressure tide amplitudes and phases                           GTRP 62
        A1p = PZA1(khgt,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P1p = PZP1(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        A2p = PZA2(khgt,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P2p = PZP2(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Pressure at corners of 3-D cube                               GTRP 67
        PM(i,l,m) = TideY_M05(P0,A1p,P1p,A2p,P2p,time)
        !---      Daily average density D0                                      GTRP 69
        D0 = DZA0(khgt,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Gas constant from pressure, density and temperature           GTRP 71
        R0(i,l,m) = 190.0d0
        if (T0/=0.0d0 .and. D0/=0.0d0) then
          R0(i,l,m) = P0 / (T0*D0)
        end if
        !---      Max and Min temperature and density at corners of 3-D cube    GTRP 74
        Tmax(i,l,m) = -9999.0d0
        Tmin(i,l,m) = 9999.0d0
        Dmax(i,l,m) = -9999.0d0
        Dmin(i,l,m) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            Ttime = TideX_M05(T0,A1t,P1t,A2t,P2t,xtime)
            Ptime = TideY_M05(P0,A1p,P1p,A2p,P2p,xtime)
            Dtime = Ptime / (R0(i,l,m)*Ttime)
            if (Ttime > Tmax(i,l,m)) then
              Tmax(i,l,m) = Ttime
            end if
            if (Ttime < Tmin(i,l,m)) then
              Tmin(i,l,m) = Ttime
            end if
            if (Dtime > Dmax(i,l,m)) then
              Dmax(i,l,m) = Dtime
            end if
            if (Dtime < Dmin(i,l,m)) then
              Dmin(i,l,m) = Dtime
            end if
          end do
        end if
        !---      Daily mean EW wind                                            GTRP 91
        U0 = UZA0(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        Uday(i,l,m) = U0
        !---      EW wind tide amplitudes and phases                            GTRP 94
        A1 = UZA1(khgt,ILATW+i-1,LS+l-1,MDUST+m-1) * upolefac
        P1 = UZP1(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        A2 = UZA2(khgt,ILATW+i-1,LS+l-1,MDUST+m-1) * upolefac
        P2 = UZP2(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        !---      EW wind at corners of 3-D cube                                GTRP 99
        UM(i,l,m) = TideX_M05(U0,A1,P1,A2,P2,time)
        !---      Daily mean NS wind                                            GTRP101
        V0 = VZA0(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        Vday(i,l,m) = V0
        !---      NS wind tide amplitudes and phases                            GTRP104
        A1 = VZA1(khgt,ILATW+i-1,LS+l-1,MDUST+m-1) * upolefac
        P1 = VZP1(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        A2 = VZA2(khgt,ILATW+i-1,LS+l-1,MDUST+m-1) * upolefac
        P2 = VZP2(khgt,ILATW+i-1,LS+l-1,MDUST+m-1)
        !---      NS wind at corners of 3-D cube                                GTRP109
        VM(i,l,m) = TideX_M05(V0,A1,P1,A2,P2,time)
      end do
    end do
  end do
  !---    Use 3-D interpolation to get temperature, pressure, gas         GTRP114
  !       constant, EW wind, and NS wind at given latitude, Ls, and       GTRP115
  !       dust optical depth                                              GTRP116
  call ThreeD_M05(DLAT,DLS,DDUST,TM,TMGCM)
  call ThreeD_M05(DLAT,DLS,DDUST,Tday,TempDay)
  call ThreeD_M05(DLAT,DLS,DDUST,Tmax,Tempmax)
  call ThreeD_M05(DLAT,DLS,DDUST,Tmin,Tempmin)
  call ThreeD_M05(DLAT,DLS,DDUST,Dmax,Densmax)
  call ThreeD_M05(DLAT,DLS,DDUST,Dmin,Densmin)
  call ThreeD_M05(DLAT,DLS,DDUST,PM,PMGCM)
  call ThreeD_M05(DLAT,DLS,DDUST,Pday,PresDay)
  call ThreeD_M05(DLAT,DLS,DDUST,R0,RMGCM)
  call ThreeD_M05(DLATW,DLS,DDUST,UM,UMGCM)
  call ThreeD_M05(DLATW,DLS,DDUST,Uday,UwndDay)
  call ThreeD_M05(DLATW,DLS,DDUST,VM,VMGCM)
  call ThreeD_M05(DLATW,DLS,DDUST,Vday,VwndDay)
  !---    Compute density from temperature, pressure, and gas constant    GTRP130
  DMGCM = PMGCM / (RMGCM*TMGCM)
  DensDay = PresDay / (RMGCM*TempDay)
end subroutine MGCMterp_M05
!-----------------------------------------------------------------------GTRP135
integer function Ifloor_M05(x)
  !---    Integer floor function, greatest integer <= x (provided for     IFLR  2
  !       compilers that do not have this intrinsic function)             IFLR  3
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: x
  ! 
  !.. Local Scalars .. 
  integer :: Iflr
  ! 
  !.. Intrinsic Functions .. 
  intrinsic int
  ! 
  ! ... Executable Statements ...
  ! 
  Iflr = int(x)
  if (DBLE(Iflr) > x) then
    Iflr = Iflr - 1
  end if
  Ifloor_M05 = Iflr
end function Ifloor_M05
!-----------------------------------------------------------------------IFLR 10
subroutine marsephm_M05(xday,sunlat,sunlon,sunLsubs,radius,owlt,EOT)
  !---  Computes sunlat, sunlon= latitude and longitude of sub-solar      MEPH  3
  !     point on the surface, sunLsubs= areocentric longitude of the Sun  MEPH  4
  !     (Ls), radius= current orbital radius from Sun to Mars, heliolon=  MEPH  5
  !     Mars heliocentric longitude, owlt= Mars-Earth one-way light       MEPH  6
  !     time (minutes), and EOT= equation of time (deg), calculated from  MEPH  7
  !     Julian day and time, xday.  Notes: input xday is NOT UTC, but     MEPH  8
  !     Terrestrial (Dynamical) Mars-Event Time (NOT Earth-Receive Time). MEPH  9
  !     Mars Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)   MEPH 10
  !     minus EOT (in hrs). Output is for Terrestrial (Dynamical)         MEPH 11
  !     Mars Event Time (corresponding to input xday).                    MEPH 12
  !                                                                       MEPH 13
  !     Equations for "moderately accurate" Mars solar time, seasonal     MEPH 14
  !     parameters, and one-way Mars-Earth light time, from Allison and   MEPH 15
  !     McEwen, Planet. Space Sci., 48, 215-235 (2000), and Allison       MEPH 16
  !     Geophys Res. Lett., 24(16), 1967-1970 (1997).                     MEPH 17
  !                                                                       MEPH 18
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: xday
  real(kind=PM_REEL), intent(out) :: EOT,owlt,radius,sunLsubs,sunlat,sunlon
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: PBS,Vm,Vm0,Vmday0,alphFMS,alphs,alsrad,anlon0,anom0, &
                      anomM,argper,coslat,dlat1,dlat2,dlat3,dt,ecc,ecc2,ecc3, &
                      ecc4,ecc5,ecc6,eqcenter,gE,helilon,helonE,inc,obl, &
                      perlon0,pi180,pmr,rE,rad0,siday,trueanom,veqlon0, &
                      veqlon1,xE,xpl,yE
  real(kind=PM_REEL) :: ypl,yranom,yrtrop,zpl
  ! 
! DM1359 remove : private  
!.. External Calls .. 
!  external Rescale_M05, Shiftdif_M05, perturb_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, aint, asin, atan, cos, sin, sqrt, tan
  ! 
  ! ... Executable Statements ...
  ! 
  pi180 = atan(1.0d0) / 45.0d0
  !---  Days since 2000 January 1.5                                       MEPH 28
  dt = xday - 2451545.0d0
  !                                                                       MEPH 30
  !---------------------------------------------------------------------  MEPH 31
  !                                                                       MEPH 32
  !---  Planetary orbit parameters                                        MEPH 33
  !                                                                       MEPH 34
  !     Semi-major axis (AU) = mean distance from Sun                     MEPH 35
  rad0 = 1.52368d0
  !     Anomalistic year (days, perihelion-to-perihelion)                 MEPH 37
  yranom = 686.9957d0
  !     Tropical year (days, for rate of fictitious mean sun)             MEPH 39
  yrtrop = 686.9726d0
  !     Mean anomaly for J2000 (degrees)                                  MEPH 41
  anom0 = 19.387d0
  !     Heliocentric longitude of perihelion at J2000 (deg)               MEPH 43
  perlon0 = 336.0602d0
  !     Terms for heliocentric longitude at Ls=0 (deg)                    MEPH 45
  veqlon0 = 85.061d0
  veqlon1 = 5.5d-6
  !     Eccentricity and powers                                           MEPH 48
  ecc = 0.09340d0 + 2.477d-9*dt
  ecc2 = ecc**2
  ecc3 = ecc2 * ecc
  ecc4 = ecc3 * ecc
  ecc5 = ecc4 * ecc
  ecc6 = ecc5 * ecc
  !     Obliquity angle (radians)                                         MEPH 55
  obl = (25.1919d0+3.45d-7*dt) * pi180
  !     Inclination (radians)                                             MEPH 57
  inc = (1.8497d0-2.23d-7*dt) * pi180
  !     Longitude of ascending node at J2000 (deg)                        MEPH 59
  anlon0 = 49.5581d0
  !     Sidereal period of rotation (Earth days)                          MEPH 61
  !---  Empirical correction in last digit, to agree with Horizons        MEPH 62
  siday = 1.025956749d0
  !     Heliocentric lon of prime meridian (deg) at Julian day Vmday0     MEPH 64
  Vm0 = 133.476d0
  !---  Empirical correction for agreement with Horizons data             MEPH 66
  Vm0 = Vm0 - 0.09746d0
  Vmday0 = 2451545.0d0
  !     Difference terms, planetocentric to planetographic lat (deg)      MEPH 69
  dlat1 = 0.269d0
  dlat2 = 0.003d0
  dlat3 = 0.008d0
  !                                                                       MEPH 73
  !---------------------------------------------------------------------  MEPH 74
  !                                                                       MEPH 75
  !---  Mean anomaly (radians)                                            MEPH 76
  !---  Allison & McEwen (2000) equation (16)                             MEPH 77
  anomM = (anom0+(360.0d0/yranom)*dt) * pi180
  !---  Right ascension of fictitious mean sun (deg)                      MEPH 79
  !---  Allison & McEwen (2000) equation (17)                             MEPH 80
  alphFMS = perlon0 - veqlon0 + anom0 + (360.0d0/yrtrop)*dt
  !---  Mars equation of center, A&M eqn. (4) (degrees)                   MEPH 82
  eqcenter = ((2.0d0*ecc-0.25d0*ecc3+(5.0d0/96.0d0)*ecc5)*sin(anomM)+ &
              (1.25d0*ecc2-(11.0d0/24.0d0)*ecc4+(17.0d0/192.0d0)*ecc6)* &
              sin(2.0d0*anomM)+ &
              ((13.0d0/12.0d0)*ecc3-(43.0d0/63.0d0)*ecc5)*sin(3.0d0*anomM)+ &
              ((103.0d0/96.0d0)*ecc4-(451.0d0/480.0d0)*ecc6)*sin(4.0d0*anomM)+ &
              ((1097.0d0/960.0d0)*ecc5)*sin(5.0d0*anomM)+ &
              ((12323.0d0/960.0d0)*ecc6)*sin(6.0d0*anomM)) / pi180
  !---  True areocentric solar longitude (Ls), A&M eqns. (2) and (4)      MEPH 92
  sunLsubs = alphFMS + eqcenter
  !---  Add perturbations due to Jupiter, Earth, and Venus, A&M eqns.     MEPH 94
  !     (18) and (19)                                                     MEPH 95
  call perturb_M05(dt,PBS)
  sunLsubs = sunLsubs + PBS

  call Rescale_M05(sunLsubs)
  !---  Ls angle in radians                                               MEPH 99
  alsrad = sunLsubs * pi180
  !---  Sub-solar latitude of sun (planetographic solar declination),     MEPH101
  !     Allison (1997) eqn. (5) with empirical Ls and 3*Ls terms          MEPH102
  sunlat = asin(sin(obl)*sin(alsrad))/pi180 + dlat1*sin(alsrad) + &
           dlat2*cos(alsrad) + dlat3*sin(3.0d0*alsrad)
  !---  Solar right ascension, un-numbered equation, A&M page 217         MEPH105
  alphs = atan(cos(obl)*tan(alsrad)) / pi180
  !---  Put alphs into right quadrant                                     MEPH107
  if (abs(sunLsubs-alphs) > 270.0d0) then
    alphs = alphs + 360.0d0
  elseif (abs(sunLsubs-alphs) > 90.0d0) then
    alphs = alphs + 180.0d0
  end if
  call Rescale_M05(alphs)
  !---  Mars orbital radius, Astronomical Almanac page E4                 MEPH114
  radius = rad0 * (1.0d0-ecc2) / (1.0d0+ecc*cos(anomM+alsrad-alphFMS*pi180))
  !---  Approximate Mars heliocentric longitude, A&M eqn, (11)            MEPH117
  helilon = sunLsubs + veqlon0 - veqlon1*dt - &
            (tan(0.5d0*inc)**2)*sin(2.0d0*(alsrad+(veqlon0-anlon0)*pi180))/ &
            pi180
  call Rescale_M05(helilon)
  !---  Equation of time (deg)                                            MEPH121
  EOT = alphFMS - alphs
  call Rescale_M05(EOT)
  call Shiftdif_M05(EOT)
  !---  Earth heliocentric distance and longitude, Allison eqns (20)-     MEPH125
  !     (22)                                                              MEPH126
  gE = (357.528d0+0.9856003d0*dt) * pi180
  rE = 1.00014d0 - 0.01671d0*cos(gE) - 0.00014d0*cos(2.0d0*gE)
  helonE = 100.472d0 + 0.9856474d0*dt + 1.915d0*sin(gE) + &
           0.020d0*sin(2.0d0*gE)
  !---  Earth Cartesian coordinates                                       MEPH131
  xE = rE * cos(helonE*pi180)
  yE = rE * sin(helonE*pi180)
  !---  Mars true anolmaly (radians)                                      MEPH134
  trueanom = eqcenter*pi180 + anomM
  !---  Mars argument of perihelion (radians)                             MEPH136
  argper = (286.5016d0+2.92961d-5*dt) * pi180
  !---  Mars Cartesian coordinates                                        MEPH138
  zpl = radius * sin(trueanom+argper) * sin(inc)
  coslat = sqrt(1.0d0-(zpl/radius)**2)
  xpl = radius * cos((helilon+3.82394d-5*dt)*pi180) * coslat
  ypl = radius * sin((helilon+3.82394d-5*dt)*pi180) * coslat
  !---  One-way light time (minutes), Allison eqn.(19)                    MEPH143
  owlt = sqrt((xpl-xE)**2+(ypl-yE)**2+zpl**2) * 499.005d0 / 60.0d0
  !---  Mars (Heliocentric) prime meridian, Allison eqn (11)              MEPH145
  Vm = Vm0 + (360.0d0/siday)*(xday-Vmday0)
  !---  True solar time (Mars hours) at Mars prime meridian, A&M          MEPH147
  !     page 217                                                          MEPH148
  pmr = (Vm-alphs) / 360.0d0
  sunlon = (pmr-aint(pmr))*360.0d0 - 180.0d0
  call Rescale_M05(sunlon)
end subroutine marsephm_M05
!---------------------------------------------------------------------  MEPH154
subroutine MarsGCM_M05(chgt,clat,clonw,als,dusttau,time,ctemp,cpres,cdens, &
                       cuwin,cvwin,blwindew,blwindns,blwindvert,Hpres,Hdens, &
                       ZF,pertfact,ctopohgt,hgtasfc,careoid,TempDay,PresDay, &
                       DensDay,EWwnDay,NSwnDay,bluday,blvday,Tempmax,Tempmin, &
                       Densmax,Densmin,Tgrnd,calbedo,icepolar,Tat5m, &
                       dustoffset,requa,rpole,idaydata)
  !---    Uses interpolation routines to evaluate:                        MGCM  7
  !                                                                       MGCM  8
  !       ctemp    = temperature (K) at current position                  MGCM  9
  !       cpres    = pressure (N/m**2) at current position                MGCM 10
  !       cdens    = density (kg/m**3) at current position                MGCM 11
  !       cuwin    = eastward wind component (m/s) at current position    MGCM 12
  !       cvwin    = northward wind component (m/s) at current position   MGCM 13
  !       blwinew  = eastward b.l. slope wind (m/s)                       MGCM 14
  !       blwinns  = northward b.l. slope wind (m/s)                      MGCM 15
  !       Hpres    = pressure scale height (km) at current position       MGCM 16
  !       Hdens    = density scale height (km) at current position        MGCM 17
  !       ZF       = height of 1.26 nbar level at current position        MGCM 18
  !       pertfact = perturbation factor from random perturbation model   MGCM 19
  !       ctopohgt = topographic height (km) at current position          MGCM 20
  !       careoid  = local radius (km) of MOLA 1/2 degree areoid          MGCM 21
  !       TempDay  = Local daily average temperature (K)                  MGCM 22
  !       PresDay  = Local daily average pressure (N/m**2)                MGCM 23
  !       DensDay  = Local daily average density (kg/m**3)                MGCM 24
  !       EWwnDay  = Local daily average Eastward wind (m/s)              MGCM 25
  !       NSwnDay  = Local daily average Northward wind (m/s)             MGCM 26
  !       Tempmax  = Local daily maximum temperature (K)                  MGCM 27
  !       Tempmin  = Local daily minimum temperature (K)                  MGCM 28
  !       Densmax  = Local daily maximum density (kg/m**3)                MGCM 29
  !       Densmin  = Local daily minimum density (kg/m**3)                MGCM 30
  !       Tgrnd    = ground surface temperature (K)                       MGCM 31
  !       calbedo  = surface albedo                                       MGCM 32
  !       icepolar = polar ice indicator (0=no; 1=yes)                    MGCM 33
  !                                                                       MGCM 34
  !       at the current height (chgt), latitude (clat), current (West)   MGCM 35
  !       longitude (clonw), for time of year given by Ls=als, and time   MGCM 36
  !       of day (time).  Interpolation is done using either boundary     MGCM 37
  !       layer or 0-80 km data from the Ames Mars General Circulation    MGCM 38
  !       model (MGCM) or from 80-170 km data from the University of      MGCM 39
  !       Michigan Mars Thermospheric General Circulation Model (MTGCM).  MGCM 40
  !                                                                       MGCM 41
  !---    Set parameter values for number of MGCM heights (nhgt), number  MGCM 42
  !       of MTGCM heights (nhgtt), number of MGCM boundary layer levels  MGCM 43
  !       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-   MGCM 44
  !       tudes (nlatt), number of MGCM longitudes (nlon), number of dust MGCM 45
  !       optical depths (ndust), and minimum perturbation magnitude at   MGCM 46
  !       surface (pert0)                                                 MGCM 47
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nhgt = 17
  integer, parameter :: nhgtt = 19
  integer, parameter :: nbl = 3
  integer, parameter :: nlat = 25
  integer, parameter :: nlatt = 36
  integer, parameter :: nlon = 40
  integer, parameter :: ndust = 3
  real(kind=PM_REEL), parameter :: pert0 = .02d0
  real(kind=PM_REEL), parameter :: pertmin = 0.1d0
  integer, parameter :: nf10 = 2
  integer, parameter :: ntesy = 2
  integer, parameter :: ntf10 = 3
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata
  integer, intent(out) :: icepolar
  real(kind=PM_REEL), intent(in) :: &
    als,clat,clonw,dustoffset,dusttau,hgtasfc,requa,rpole,time
  real(kind=PM_REEL), intent(inout) :: Tat5m,chgt
  real(kind=PM_REEL), intent(out) :: &
    DensDay,Densmax,Densmin,EWwnDay,Hdens,Hpres,NSwnDay,PresDay,TempDay, &
    Tempmax,Tempmin,Tgrnd,ZF,bluday,blvday,blwindew,blwindns,blwindvert, &
    calbedo,careoid,cdens,cpres,ctemp,ctopohgt,cuwin,cvwin,pertfact
  ! 
  !.. Local Scalars .. 
  integer :: LoH,itime,jbl,khgt,khgtt,lhgtt
  real(kind=PM_REEL) :: CpofT,DMGCM1,DMGCM2,DMGCMx,Dday1,Dday2,Ddayx,Dmax1, &
                      Dmax2,Dmaxx,Dmin1,Dmin2,Dminx,Hden12,Hdens1,Hdensc, &
                      Hdensdayc,Hpres1,Hpresday,Oldrref,PMGCM1,PMGCM2,PMGCMx, &
                      Pday1,Pday2,Pdayx,R1,R2,Rgas,Rgas1,Rgas2,Rgasday,Rref, &
                      TMGCM1,TMGCM2,TMGCMx,Tbar,Tcheck,Tday1,Tday2
  real(kind=PM_REEL) :: Tdayx,Tmax1,Tmax2,Tmaxx,Tmin1,Tmin2,Tminx,Tsubl,UMGCM1, &
                      UMGCM2,UMGCMx,Uday1,Uday2,Udayx,VMGCM1,VMGCM2,VMGCMx, &
                      Vday1,Vday2,Vdayx,ZF80,albedo,blu,blv,blw,curoffset, &
                      dTdz,dhgt,fLH,factor,gCp,globoffst,goR,gz,ofsmgcm, &
                      ofsmult1,ofsmult2,ofsz1,ofsz2,pertlo
  real(kind=PM_REEL) :: pertmax,polefac,steplat,steplon,tlat1st,topohgt, &
                      toprelief,tsteplat,uhgt,wavemax,z0,z1,z1ofs,z2,z2ofs, &
                      z2x,z5,zeval
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2) :: offset
  ! 
! DM1359: remove : private   
!.. External Calls .. 
!  external MGCMterp_M05, RELLIPS_M05, SublTchk_M05, TGCMterp_M05, TwoD_M05, &
!           bltp_M05, slopewind_M05, surfterp_M05
 
 ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Cp_M05
  !real(kind=PM_REEL), external :: Zlogr_M05
  !integer, external :: Ifloor_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, exp, log, sin, sqrt, dble
  ! 
  !.. Common Blocks .. 
  common /INTERP_M05/ DLAT,DLON,DLS,DDUST,DLATW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                      ILAT,JLON,LS,MDUST,K1ST,ILATW,ILATT,MF10
  !     For dlat to mf10: Not Read, Overwritten
  ! 
  !... Variables in Common Block /Interp_M05/ ... 
  real(kind=PM_REEL) :: DDUST,DF10,DLAT,DLATT,DLATW,DLON,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,K1ST,LS,MDUST,MF10
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to f10val: Maybe Read, Not Written
  !     For f10TES to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /TGCMOFFSET_M05/ OFFSETS,TOFFSETS,ZOFFSET,HGTOFFSET,OFSZL,IBOUGHER
  !     For offsets: Maybe Read, Not Written
  !     For toffsets: Not Read, Not Written
  !     For zoffset: Maybe Read, Not Written
  !     For hgtoffset to ofszL: Not Read, Overwritten
  !     For ibougher: Read, Not Written
  ! 
  !... Variables in Common Block /TGCMoffset_M05/ ... 
  real(kind=PM_REEL), dimension(0:12,ndust) :: OFFSETS
  real(kind=PM_REEL), dimension(0:12,ntesy) :: TOFFSETS
  real(kind=PM_REEL) :: HGTOFFSET,OFSZL,ZOFFSET
  integer :: IBOUGHER
  common /THERM_M05/ F107,STDL,FMOL
  !     For F107: Read, Not Written
  !     For stdl to fmol: Not Read, Not Written
  ! 
  !... Variables in Common Block /THERM_M05/ ... 
  real(kind=PM_REEL) :: F107,STDL
  real(kind=PM_REEL), dimension(0:8) :: FMOL
  ! 
  ! ... Executable Statements ...
  ! 
  pertmax = 0.2d0
  pertfact = 0.0d0
  !---    Initialize ground surface temperature and polar ice indicator   MGCM 73
  Tgrnd = 999.9d0
  icepolar = 99
  !---    Insure latitude, longitude, Ls, and time of day within proper   MGCM 76
  !       bounds                                                          MGCM 77
  if (abs(clat) > 90.0d0) stop " Latitude error in MarsGCM_M05"
  if (abs(clonw) > 360.0d0) stop " Longitude error: MarsGCM_M05"
  if (als<0.0d0 .or. als>360.0d0) stop " Ls error MarsGCM_M05"
  if (time<0.0d0 .or. time>24.0d0) stop " time error in MarsGCM_M05"
  !---    Latitude step size for MGCM and MTGCM data                      MGCM 83
  steplat = 180.0d0 / (dble(nlat)-1.0d0)
  tsteplat = 180.0d0 / dble(nlatt)
  !---    Most southerly MTGCM latitude                                   MGCM 86
  tlat1st = -90.0d0 + tsteplat/2.0d0
  !---    Longitude step size for MGCM boundary layer data                MGCM 88
  steplon = 360.0d0 / dble(nlon)
  !---    MGCM height index (khgt) for current height (chgt)              MGCM 90
  khgt = Ifloor_M05(1.0d0+chgt/5.0d0)
  !---    Insure khgt within proper limits                                MGCM 92
  if (khgt < 1) then
    khgt = 1
  end if
  if (khgt > nhgt-1) then
    khgt = nhgt - 1
  end if
  !---    MGCM latitude index (ilat) from current latitude (clat)         MGCM 95
  ILAT = 1 + Ifloor_M05((clat+90.0d0)/steplat)
  if (ILAT > nlat-1) then
    ILAT = nlat - 1
  end if
  !---    MGCM wind latitude index (ilatw).  MGCM winds are offset in     MGCM 98
  !       latitude by 1/2 latitude grid step.                             MGCM 99
  ILATW = 2 + Ifloor_M05((clat+86.25d0)/steplat)
  !---    Insure ilatw within proper bounds                               MGCM101
  if (ILATW < 2) then
    ILATW = 2
  end if
  if (ILATW > nlat-1) then
    ILATW = nlat - 1
  end if
  !---    MTGCM latitude index (ilatt) from current latitude (clat)       MGCM104
  ILATT = 1 + Ifloor_M05((clat-tlat1st)/tsteplat)
  !---    Insure ilatt within proper bounds                               MGCM106
  if (ILATT < 1) then
    ILATT = 1
  end if
  if (ILATT > nlatt-1) then
    ILATT = nlatt - 1
  end if
  !---    MGCM boundary layer longitude index (jlon)                      MGCM109
  JLON = Ifloor_M05(clonw/steplon)
  if (JLON > nlon-1) then
    JLON = nlon - 1
  end if
  !---    Time of year index (ls) from input Ls value (als)               MGCM112
  LS = Ifloor_M05(als/30.0d0)
  if (LS > 11) then
    LS = 11
  end if
  !---    Dust index (mdust) for input dust optical depth (dusttau)       MGCM115
  if (dusttau < DUST(2)) then
    MDUST = 1
  else
    MDUST = 2
  end if
  !---    Increment of MGCM latitude (dlat) from grid point               MGCM121
  DLAT = (clat-steplat*(dble(ILAT)-1.0d0)+90.0d0) / steplat
  !---    Increment of MTGCM latitude (dlatt) from grid point             MGCM123
  DLATT = (clat-tsteplat*(dble(ILATT)-1.0d0)-tlat1st) / tsteplat
  !---    Insure dlatt within proper bounds near poles                    MGCM125
  TPOLEFAC = 1.0d0
  if (ILATT == 1) then
    TPOLEFAC = 0.5d0
    if (DLATT <= 0.0d0) then
      DLATT = 0.0d0
      TPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
    end if
  elseif (ILATT >= nlat-1) then
    TPOLEFAC = 0.5d0
    if (DLATT >= 1.0d0) then
      DLATT = 1.0d0
      TPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
    end if
  end if
  !---    Increment of MGCM longitude (dlon) from grid point              MGCM140
  DLON = (clonw-steplon*dble(JLON)) / steplon
  !---    Increment of MGCM latitude from (offset) wind grid point        MGCM142
  DLATW = (clat-steplat*(dble(ILATW)-2.0d0)+86.25d0) / steplat
  WPOLEFAC = 1.0d0
  if (ILATW == 2) then
    WPOLEFAC = 0.75d0
    if (DLATW <= 0.0d0) then
      WPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      DLATW = 0.0d0
    end if
  elseif (ILATW >= nlat-1) then
    WPOLEFAC = 0.75d0
    if (DLATW >= 1.0d0) then
      WPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      DLATW = 1.0d0
    end if
  end if
  !---    Increment of solar activity (F10.7 at 1AU) for MTGCM data       MGCM158
  MF10 = 1
  DF10 = Zlogr_M05(F107,F10VAL(MF10),"MGCM-01") / &
         Zlogr_M05(F10VAL(MF10+1),F10VAL(MF10),"MGCM-02")
  !---    Get areoid radius and topographic height at current lat, lon    MGCM162
  call RELLIPS_M05(clat,clonw,careoid,chgt,gz,Oldrref,ctopohgt,calbedo,requa, &
                   rpole)
  !---    Compute topographic relief factor for simplified mountain       MGCM165
  !       wave perturbation model                                         MGCM166
  toprelief = 25.0d0 + ctopohgt
  !---    Use topographic height if input height is <= -8.7 km            MGCM168
  if (chgt <= -8.7d0) then
    chgt = ctopohgt + hgtasfc
  end if
  !---    Find height index (k1st) of first 0-80 km MGCM level above      MGCM170
  !       surface topographic height                                      MGCM171
  K1ST = Ifloor_M05(2.0d0+(ctopohgt+1.0d0)/5.0d0)
  if (K1ST < 1) then
    K1ST = 1
  end if
  !---    Find Ls increment (dls) from Ls "grid" on input data            MGCM174
  DLS = (als-30.0d0*dble(LS)) / 30.0d0
  !---    Compute dust increment (ddust) from dust optical depth "grid"   MGCM176
  !       points                                                          MGCM177
  DDUST = Zlogr_M05(dusttau,DUST(MDUST),"MGCM-03") / &
          Zlogr_M05(DUST(MDUST+1),DUST(MDUST),"MGCM-04")
  !---    Insure ddust within proper range                                MGCM180
  if (DDUST<0.0d0 .and. MDUST==1) then
    DDUST = (dusttau-DUST(1)) / (DUST(2)-DUST(1))
  end if
  if (DDUST > 1.0d0) then
    DDUST = 1.0d0
  end if
  !---    Initialize ZF = height of 1.26 nbar level (output value if      MGCM184
  !       current height < 80 km)                                         MGCM185
  ZF = 999.0d0
  !---    Assign MTGCM height offset from input zoffset or array offsets  MGCM187
  globoffst = 0.0d0
  curoffset = 0.0d0
  if (IBOUGHER == 2) then
    offset(1,1) = OFFSETS(LS,MDUST)
    offset(1,2) = OFFSETS(LS,MDUST+1)
    offset(2,1) = OFFSETS(LS+1,MDUST)
    offset(2,2) = OFFSETS(LS+1,MDUST+1)
    call TwoD_M05(DLS,DDUST,offset,globoffst)
  else
    call TGCMterp_M05(1,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,ZF,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,idaydata &
                     )
    call TGCMterp_M05(2,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,idaydata &
                     )
    Hdensc = 5.0d0 / Zlogr_M05(DMGCM1,DMGCM2,"MGCM-05")
    Hdensdayc = 5.0d0 / Zlogr_M05(Dday1,Dday2,"MGCM-06")
    call MGCMterp_M05(nhgt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,idaydata &
                     )
    if (IBOUGHER == 3) then
      curoffset = Hdensdayc * Zlogr_M05(Dday2,Dday1,"MGCM-07")
    else
      curoffset = Hdensc * Zlogr_M05(DMGCM2,DMGCM1,"MGCM-08")
    end if
  end if
  if (IBOUGHER <= 0) then
    HGTOFFSET = ZOFFSET
  elseif (IBOUGHER == 1) then
    HGTOFFSET = ZOFFSET - 2.5d0*sin(atan(1.0d0)*als/45.0d0)
  elseif (IBOUGHER == 2) then
    HGTOFFSET = globoffst
  else
    HGTOFFSET = curoffset
  end if
  !---    Add height offset due to dust storm                             MGCM223
  HGTOFFSET = HGTOFFSET + dustoffset
  !---    MTGCM height index (khgtt) for current height                   MGCM225
  khgtt = Ifloor_M05((chgt-HGTOFFSET-75.0d0)/5.0d0)
  !---    Insure khgtt within proper limits                               MGCM227
  if (khgtt < 1) then
    khgtt = 1
  end if
  lhgtt = 1
  if (khgtt==1 .and. HGTOFFSET<-4.0d0) then
    khgtt = 2
    lhgtt = 2
  end if
  if (khgtt > nhgtt-1) then
    khgtt = nhgtt - 1
  end if
  !---    Initialize MGCM height offset to zero                           MGCM235
  OFSZL = 0.0d0

  !---    Use MTGCM interpolation if height >= 80 km                      MGCM237
  if (chgt >= 80.0d0+HGTOFFSET+5.0d0*(dble(lhgtt)-1.0d0)) then
    !---      Get temperature, pressure, density, and wind components at    MGCM239
    !         height indexes above and below current height                 MGCM240
    call TGCMterp_M05(khgtt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,ZF,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,idaydata &
                     )
    call TGCMterp_M05(khgtt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF, &
                      Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2, &
                      idaydata)
    !---      Height grid points above and below current height             MGCM247
    z1 = 75.0d0 + 5.0d0*dble(khgtt) + HGTOFFSET
    z2 = 80.0d0 + 5.0d0*dble(khgtt) + HGTOFFSET
    !---      Apply MTGCM height offset to ZF altitude                      MGCM250
    ZF = ZF + HGTOFFSET
    !---      Pressure and density scale heights                            MGCM252
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"MGCM-09")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"MGCM-10")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"MGCM-11")
    OFSZL = HGTOFFSET
  !---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80   MGCM257
  !       km if height between 75 and 80 km                               MGCM258
  elseif (chgt >= 75.0d0) then
    !---      Get temperature, pressure, density, and wind components at    MGCM260
    !         heights above and below current height                        MGCM261
    call MGCMterp_M05(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,idaydata &
                     )
    call TGCMterp_M05(lhgtt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF80, &
                      Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2, &
                      idaydata)
    z1 = 75.0d0
    z2 = 80.0d0 + HGTOFFSET + 5.0d0*(dble(lhgtt)-1.0d0)
    !---      Apply 'equivalent' multiplier for offset between 75 & 80 km   MGCM270
    if (IBOUGHER<=1 .or. dustoffset>0.0d0) then
      z1ofs = 60.0d0
      ofsmgcm = HGTOFFSET - curoffset
      if (IBOUGHER > 1) then
        ofsmgcm = dustoffset
      end if
      ofsz1 = ofsmgcm * (z1-z1ofs) / (z2-z1ofs)
      call MGCMterp_M05(khgt+1,time,TMGCMx,PMGCMx,DMGCMx,UMGCMx,VMGCMx,Tdayx, &
                        Pdayx,Ddayx,Udayx,Vdayx,Tmaxx,Tminx,Dmaxx,Dminx, &
                        idaydata)
      Hden12 = 5.0d0 / log(DMGCM1/DMGCMx)
      ofsmult1 = exp(ofsz1/Hden12)
      !---        Local MGCM height offset                                    MGCM281
      OFSZL = ofsmgcm * (CHGT-z1ofs) / (z2-z1ofs)
      PMGCM1 = PMGCM1 * ofsmult1
      DMGCM1 = DMGCM1 * ofsmult1
      Pday1 = Pday1 * ofsmult1
      Dday1 = Dday1 * ofsmult1
      Dmax1 = Dmax1 * ofsmult1
      Dmin1 = Dmin1 * ofsmult1
    end if
    !---      Pressure and density scale heights (km)                       MGCM290
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"MGCM-12")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"MGCM-13")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"MGCM-14")
  !---    Use surfterp_M05 routine if height within boundary layer        MGCM294
  elseif (chgt <= ctopohgt+DZBL(nbl)) then
    !---      Set index for surface layer data                              MGCM296
    jbl = 1
    if (chgt >= ctopohgt+DZBL(2)) then
      jbl = 2
    end if
    !---      Get temperature, pressure, density, and wind components at    MGCM299
    !         heights above and below current height                        MGCM300
    call surfterp_M05(jbl+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Hpres, &
                      Hdens,ctopohgt,Tday2,Pday2,Dday2,Uday2,Vday2,Hpresday, &
                      Tmax2,Tmin2,Dmax2,Dmin2,Tat5m,idaydata)
    call surfterp_M05(jbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Hpres1, &
                      Hdens1,ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1,Hpresday, &
                      Tmax1,Tmin1,Dmax1,Dmin1,Tat5m,idaydata)
    !---      Heights at two boundary layer levels                          MGCM307
    z1 = ctopohgt + DZBL(jbl)
    z2 = ctopohgt + DZBL(jbl+1)
    !---      Get Temperature at 1st MGCM height above BL, for computing    MGCM310
    !         density scale height                                          MGCM311
    call MGCMterp_M05(K1ST,time,TMGCMx,PMGCMx,DMGCMx,UMGCMx,VMGCMx,Tdayx, &
                      Pdayx,Ddayx,Udayx,Vdayx,Tmaxx,Tminx,Dmaxx,Dminx,idaydata &
                     )
    !---      Temperature gradient for density scale height calculation     MGCM315
    z2x = 5.0d0 * (dble(K1ST)-1.0d0)
    dTdz = (TMGCMx-TMGCM1) / (z2x-z1)
    if (chgt <= ctopohgt) then
      dTdz = 0.0d0
    end if
    !---      Average layer temperature for density scale height            MGCM319
    Tbar = (TMGCM1+TMGCM2) / 2.0d0
    !---      Density scale height from pressure scale height and           MGCM321
    !         temperature gradient                                          MGCM322
    Hdens = Hpres / (1.0d0+(Hpres/Tbar)*dTdz)
    !---      Perturbation factor = surface value                           MGCM324
    pertfact = pert0
  !---    Use MGCMterp_M05 routine if height above boundary layer levels  MGCM326
  !        and height <= 75 km                                            MGCM327
  elseif (chgt >= 5.0d0*(dble(K1ST)-1.0d0)) then
    !---      Get temperature, pressure, density, and wind components at    MGCM329
    !         heights above and below current height                        MGCM330
    call MGCMterp_M05(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,idaydata &
                     ) !---    Use surfterp_M05 at top of boundary layer and MGCMterp_M05 at   MGCM379
                       !       1st level above boundary layer if height between boundary       MGCM380
                       !       layer and height index k1st                                     MGCM381
    call MGCMterp_M05(khgt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,idaydata &
                     )
    !---      Heights at grid points above and below current level          MGCM337
    z1 = 5.0d0 * (dble(khgt)-1.0d0)
    z2 = 5.0d0 * dble(khgt)
    !---      Apply 'equivalent' multiplier for offset below 75 km          MGCM340
    if (IBOUGHER<=1 .or. dustoffset>0.0d0) then
      z1ofs = 60.0d0
      z2ofs = 80.0d0 + HGTOFFSET + 5.0d0*(dble(lhgtt)-1.0d0)
      ofsmgcm = HGTOFFSET - curoffset
      if (IBOUGHER > 1) then
        ofsmgcm = dustoffset
      end if
      if (z1 <= z1ofs) then
        ofsmult1 = 1.0d0
      else
        Hden12 = 5.0d0 / log(DMGCM1/DMGCM2)
        ofsz1 = ofsmgcm * (z1-z1ofs) / (z2ofs-z1ofs)
        ofsmult1 = exp(ofsz1/Hden12)
      end if
      if (z2 <= z1ofs) then
        ofsmult2 = 1.0d0
      else
        Hden12 = 5.0d0 / log(DMGCM1/DMGCM2)
        ofsz2 = ofsmgcm * (z2-z1ofs) / (z2ofs-z1ofs)
        ofsmult2 = exp(ofsz2/Hden12)
      end if
      !---        Local MGCM height offset                                    MGCM360
      if (CHGT > z1ofs) then
        OFSZL = ofsmgcm * (CHGT-z1ofs) / (z2ofs-z1ofs)
      end if
      PMGCM1 = PMGCM1 * ofsmult1
      DMGCM1 = DMGCM1 * ofsmult1
      PMGCM2 = PMGCM2 * ofsmult2
      DMGCM2 = DMGCM2 * ofsmult2
      Pday1 = Pday1 * ofsmult1
      Dday1 = Dday1 * ofsmult1
      Dmax1 = Dmax1 * ofsmult1
      Dmin1 = Dmin1 * ofsmult1
      Pday2 = Pday2 * ofsmult2
      Dday2 = Dday2 * ofsmult2
      Dmax2 = Dmax2 * ofsmult2
      Dmin2 = Dmin2 * ofsmult2
    end if
    !---      Pressure and density scale heights (km)                       MGCM375
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"MGCM-15")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"MGCM-16")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"MGCM-17")
  else
    !---      Get temperature, pressure, density, and wind components at    MGCM383
    !         heights above and below current height                        MGCM384
    call surfterp_M05(nbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Hpres,Hdens, &
                      ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1,Hpresday,Tmax1, &
                      Tmin1,Dmax1,Dmin1,Tat5m,idaydata)
    call MGCMterp_M05(K1ST,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,idaydata &
                     )
    !---      Heights at grid points above and below current level          MGCM391
    z1 = ctopohgt + DZBL(nbl)
    z2 = 5.0d0 * (dble(K1ST)-1.0d0)
    !---      Temperature gradient and mean temperature for density scale   MGCM394
    !         height calculation                                            MGCM395
    dTdz = (TMGCM2-TMGCM1) / (z2-z1)
    Tbar = (TMGCM1+TMGCM2) / 2.0d0
    !---      Density scale height from pressure scale height and           MGCM398
    !         temperature gradient                                          MGCM399
    Hdens = Hpres / (1.0d0+(Hpres/Tbar)*dTdz)
  end if
  !---    Get gas constant from pressure, density, and temperature        MGCM402
  if (chgt <= ctopohgt) then
    Rgas = PMGCM1 / (DMGCM1*TMGCM1)
    Rgasday = Pday1 / (Dday1*Tday1)
    dhgt = (ctopohgt-z1) / (z2-z1)
  else
    dhgt = (chgt-z1) / (z2-z1)
    R1 = PMGCM1 / (DMGCM1*TMGCM1)
    R2 = PMGCM2 / (DMGCM2*TMGCM2)
    Rgas1 = Pday1 / (Dday1*Tday1)
    Rgas2 = Pday2 / (Dday2*Tday2)
    Rgas = R1 + dhgt*(R2-R1)
    Rgasday = Rgas1 + dhgt*(Rgas2-Rgas1)
  end if
  !---    Use logarithmic wind and temperature profiles (with surface     MGCM416
  !       roughness z0) if height below lowest boundary layer level       MGCM417
  if (chgt < ctopohgt+DZBL(2)) then
    !---      Convert surface roughness to km                               MGCM419
    z0 = ZWSFC / 1000.0d0 !---    Use linear height interpolation if above logarithmic            MGCM484
                          !       surface layer                                                   MGCM485
    !---      Save ground surface temperature for output                    MGCM421
    Tgrnd = TMGCM1
    !---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if    MGCM423
    !         over ice (T <= CO2 sublimation temperature + 5K)              MGCM424
    Tcheck = TMGCM1
    call SublTchk_M05(Tcheck,PMGCM2,Tsubl)
    !---      If surface temperature near sublimation point, set polar ice  MGCM427
    !           indicator on (= 1) and re-set surface roughness             MGCM428
    icepolar = 0
    if (TMGCM1 <= Tsubl+5.0d0) then
      z0 = 1.0d-7
      icepolar = 1
    end if
    uhgt = chgt - ctopohgt
    if (uhgt < z0) then
      uhgt = z0
    end if
    !---      Compute logarithmic boundary layer shape factor for surface   MGCM436
    !         to lowest boundary layer level                                MGCM437
    factor = Zlogr_M05(uhgt,z0,"MGCM-18") / Zlogr_M05(DZBL(2),z0,"MGCM-19")
    !---      Apply wind factor; assume no-slip wind condition at surface   MGCM440
    cuwin = UMGCM2 * factor
    cvwin = VMGCM2 * factor
    EWwnDay = Uday2 * factor
    NSwnDay = Vday2 * factor
    !---      Set up parameters to evaluate temperature boundary layer      MGCM445
    !         Convert heights to meters for input to bltp_M05 subroutine    MGCM446
    z5 = DZBL(2) * 1000.0d0
    zeval = uhgt * 1000.0d0
    !---      Get value of local gravity                                    MGCM449
    call RELLIPS_M05(clat,clonw,Rref,chgt,gz,Oldrref,topohgt,albedo,requa, &
                     rpole)
    !---      Use Ames MGCM boundary layer model for current temperature    MGCM452
    !         Get specific heat at constant pressure                        MGCM453
    CpofT = Cp_M05(TMGCM2)
    call bltp_M05(gz,CpofT,TMGCM1,z5,TMGCM2,UMGCM2,VMGCM2,zeval,factor,ctemp)
    !---      Use Ames MGCM boundary layer model for daily avg temperature  MGCM457
    CpofT = Cp_M05(Tday2)
    call bltp_M05(gz,CpofT,Tday1,z5,Tday2,Uday2,Vday2,zeval,factor,Tempday)
    !---      Use Ames MGCM boundary layer model for daily max temperature  MGCM461
    CpofT = Cp_M05(Tmax2)
    call bltp_M05(gz,CpofT,Tmax1,z5,Tmax2,Uday2,Vday2,zeval,factor,Tempmax)
    !---      Use Ames MGCM boundary layer model for daily min temperature  MGCM465
    CpofT = Cp_M05(Tmin2)
    call bltp_M05(gz,CpofT,Tmin1,z5,Tmin2,Uday2,Vday2,zeval,factor,Tempmin)
    !---      Pressure at current position from pressure scale height       MGCM469
    cpres = PMGCM2 * exp((z2-chgt)/Hpres)
    PresDay = Pday2 * exp((z2-chgt)/Hpresday)
    !---      Density at current position from gas law                      MGCM472
    cdens = cpres / (Rgas*ctemp)
    DensDay = Presday / (RgasDay*TempDay)
    Densmin = 9999.0d0
    Densmax = -9999.0d0
    if (idaydata > 0) then
      !---      Daily maximum and minimum density                             MGCM478
      Densmin = DensDay * (Dmin1/Dday1+factor*((Dmin2/Dday2)-(Dmin1/Dday1)))
      Densmax = DensDay * (Dmax1/Dday1+factor*((Dmax2/Dday2)-(Dmax1/Dday1)))
    end if
  else
    dhgt = (chgt-z1) / (z2-z1)
    cuwin = UMGCM1 + dhgt*(UMGCM2-UMGCM1)
    cvwin = VMGCM1 + dhgt*(VMGCM2-VMGCM1)
    EWwnDay = Uday1 + dhgt*(Uday2-Uday1)
    NSwnDay = Vday1 + dhgt*(Vday2-Vday1)
    !---      Interpolate temperature to current height                     MGCM492
    ctemp = TMGCM1 + dhgt*(TMGCM2-TMGCM1)
    TempDay = Tday1 + dhgt*(Tday2-Tday1)
    Tempmax = Tmax1 + dhgt*(Tmax2-Tmax1)
    Tempmin = Tmin1 + dhgt*(Tmin2-Tmin1)
    !---      Pressure at current position from pressure scale height       MGCM497
    cpres = PMGCM2 * exp((z2-chgt)/Hpres)
    PresDay = Pday2 * exp((z2-chgt)/Hpresday)
    !---      Density at current position from gas law                      MGCM500
    cdens = cpres / (Rgas*ctemp)
    DensDay = Presday / (RgasDay*TempDay)
    Densmin = 9999.0d0
    Densmax = -9999.0d0
    if (idaydata > 0) then
      !---      Daily maximum and minimum density                             MGCM506
      Densmin = DensDay * (Dmin1/Dday1+dhgt*((Dmin2/Dday2)-(Dmin1/Dday1)))
      Densmax = DensDay * (Dmax1/Dday1+dhgt*((Dmax2/Dday2)-(Dmax1/Dday1)))
    end if
  end if
  if (chgt < ctopohgt+0.5d0) then
    if (abs(clat) >= 85.0d0) then
      polefac = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      cpres = polefac*cpres + (1.0d0-polefac)*PresDay
      cdens = polefac*cdens + (1.0d0-polefac)*DensDay
      Densmin = 9999.0d0
      Densmax = -9999.0d0
      if (idaydata > 0) then
        Densmax = polefac*Densmax + (1.0d0-polefac)*DensDay
        Densmin = polefac*Densmin + (1.0d0-polefac)*DensDay
      end if
    end if
  end if
  !---    Set specific bogus values of pressure or density scale heights  MGCM526
  !       are out of range                                                MGCM527
  if (Hpres < -9.99d0) then
    Hpres = -9.99d0
  end if
  if (Hpres > 99.99d0) then
    Hpres = 99.99d0
  end if
  if (Hdens < -9.99d0) then
    Hdens = -9.99d0
  end if
  if (Hdens > 99.99d0) then
    Hdens = 99.99d0
  end if
  wavemax = 1.0d0
  if (abs(z2-z1) >= 1.0d0) then
    dTdz = (TMGCM2-TMGCM1) / (z2-z1)
    LoH = INT(2.0d0)
    fLH = dble(LoH) / 6.283185d0
    fLH = fLH * sqrt(1.0d0+fLH**2)
    gCp = 1000.0d0 * gz / Cp_M05(ctemp)
    goR = 1000.0d0 * gz / Rgas
    if (dTdz < -0.1d0*gCp) then
      dTdz = -0.1d0*gCp
    end if
    wavemax = fLH * (dTdz+gCp) / goR
    pertlo = pert0 + 0.0004d0*chgt
    if (pertlo > 0.1d0) then
      pertlo = 0.1d0
    end if
    if (wavemax < pertlo) then
      wavemax = pertlo
    end if
    if (wavemax < pertmax) then
      pertmax = wavemax
    end if
  end if
  !---    Compute perturbation factor, unless it has already been set     MGCM547
  if (pertfact < pert0) then
    !---      Perturbation factor from simplified mountain wave model       MGCM549
    pertfact = 0.01d0 * toprelief * exp((chgt-100.0d0)/40.0d0)
    if (pertfact > pertmax) then
      pertfact = pertmax
    end if
    if (pertfact < pert0+.00025d0*chgt) then
      pertfact = pert0 + .00025d0*chgt
    end if
    if (chgt>=100.0d0 .and. pertfact<pertmin) then
      pertfact = pertmin
    end if
  end if
  !---    Get slope winds (0 below surface and > 4.5 km above surface)    MGCM556
  call slopewind_M05(clat,clonW,chgt,time,cuwin,cvwin,blwindew,blwindns, &
                     blwindvert)
  !---    Compute daily average slope winds                               MGCM559
  bluday = 0.0d0
  blvday = 0.0d0
  if (idaydata > 0) then
    do itime = 0,22,2
      call slopewind_M05(clat,clonW,chgt,dble(itime),cuwin,cvwin,blu,blv,blw)
      bluday = bluday + blu
      blvday = blvday + blv
    end do
    bluday = bluday / 12.0d0
    blvday = blvday / 12.0d0
  end if
end subroutine MarsGCM_M05
!-----------------------------------------------------------------------MGCM574
subroutine perturb_M05(dt,PBS)
  !---  Mars Ls perturbations from Jupiter, Earth, and Venus, Table 5     PTRB  2
  !     and eqn (18) of Allison and McEwen, Planet. Space Sci., 48, 215-  PTRB  3
  !     235 (2000). dt is time (days) after J2000 Terrestrial Time.       PTRB  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: dt
  real(kind=PM_REEL), intent(out) :: PBS
  ! 
  !.. Local Scalars .. 
  integer :: i
  real(kind=PM_REEL) :: per,pi180
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(7) :: A,phi,tau
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, cos
  ! 
  !.. Data Declarations .. 
  data A/0.007d0,0.006d0,0.004d0,0.004d0,0.002d0,0.002d0,0.002d0/
  data tau/2.2353d0,2.7543d0,1.1177d0,15.7866d0,2.1354d0,2.4694d0,32.8493d0/
  data phi/49.909d0,168.173d0,191.837d0,21.736d0,15.704d0,95.528d0,49.095d0/
  ! 
  ! ... Executable Statements ...
  ! 
  pi180 = atan(1.0d0) / 45.0d0
  per = (360.0d0/365.25d0) * pi180
  PBS = 0.0d0
  do i = 1,7
    PBS = PBS + A(i)*cos(per*dt/tau(i)+phi(i)*pi180)
  end do
end subroutine perturb_M05
!-----------------------------------------------------------------------PTRB 22
real(kind=PM_REEL) function PPND_M05(p,ifault)
  !                                                                       PPND  2
  !     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118            PPND  3
  !                                                                       PPND  4
  !     Produces normal deviate corresponding to lower tail area of p.    PPND  5
  !     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0            PPND  6
  !     otherwise.  If ifault = 1, PPND_M05 value is set to 0.            PPND  7
  !     Single precision version with error epsilon = 2 ** (-31).         PPND  8
  !     For real(kind=PM_REEL) version, change REAL to REAL(KIND=PM_REEL)     PPND  9
  !     in the FUNCTION statement and the declaration of variables;       PPND 10
  !     change E0 to D0 in the DATA statements and change ABS, ALOG       PPND 11
  !     and SQRT to DABS, DLOG and DSQRT in the assignment statements.    PPND 12
  !     The hash sums are the sums of the moduli of the coefficients.     PPND 13
  !     They have no inherent meanings, but are included for use in       PPND 14
  !     checking transpositions.                                          PPND 15
  !                                                                       PPND 16
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: p
  integer, intent(out) :: ifault
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: a0 = 2.50662823884d0,a1 = -18.61500062529d0, &
                      a2 = 41.39119773534d0,a3 = -25.44106049637d0, &
                      b1 = -8.47351093090d0,b2 = 23.08336743743d0, &
                      b3 = -21.06224101826d0,b4 = 3.13082909833d0, &
                      c0 = -2.78718931138d0,c1 = -2.29796479134d0, &
                      c2 = 4.85014127135d0,c3 = 2.32121276858d0, &
                      d1 = 3.54388924762d0,d2 = 1.63706781897d0,half = 0.5d0, &
                      one = 1.0d0,split = 0.42d0,zero = 0.0d0
  real(kind=PM_REEL) :: q,r
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, log, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       PPND 38
  !     Hash sum for c & d = 17.43746520924                               PPND 39
  !                                                                       PPND 40
  !                                                                       PPND 41
  ifault = 0
  q = p - half
  if (abs(q) <= split) then
    r = q * q
    PPND_M05 = q * (((a3*r+a2)*r+a1)*r+a0) / ((((b4*r+b3)*r+b2)*r+b1)*r+one)
    return
  end if
  r = p
  if (q > zero) then
    r = one - p
  end if
  if (r < zero) then
    ifault = 1
    PPND_M05 = zero
  else
    r = sqrt(-log(r))
    PPND_M05 = (((c3*r+c2)*r+c1)*r+c0) / ((d2*r+d1)*r+one)
    if (q < zero) then
      PPND_M05 = -PPND_M05
    end if
  end if
end function PPND_M05
!-----------------------------------------------------------------------PPND 61
subroutine PRSEAS_M05(Lsun,Lat,PR)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: Lat,Lsun
  real(kind=PM_REEL), intent(out) :: PR
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: Lat2,a1
  real(kind=PM_REEL) :: a11 = 0.0847194d0,a21 = 0.0690599d0
  real(kind=PM_REEL) :: a2,phi1
  real(kind=PM_REEL) :: a12 = -0.570405d-5,a22 = -0.132689d-5,phi11 = 304.041d0, &
                      phi12 = 0.0080602d0
  real(kind=PM_REEL) :: phi2
  real(kind=PM_REEL) :: phi21 = 61.362d0,phi22 = 0.0016533d0
  real(kind=PM_REEL) :: pi180
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, cos
  ! 
  ! ... Executable Statements ...
  ! 
  pi180 = atan(1.0d0) / 45.0d0
  Lat2 = Lat**2
  !---  a1, a2 = amplitudes of cos(Ls) and cos(2*Ls) terms                PRSE 11
  a1 = a11 + a12*Lat2
  a2 = a21 + a22*Lat2
  !---  phi1, phi2 = phases of cos(Ls) and cos(2*Ls) terms                PRSE 14
  phi1 = phi11 + phi12*Lat2
  phi2 = phi21 + phi22*Lat2
  !---  Relative variation in pressure on reference ellipsoid, due to     PRSE 17
  !     latitude and time (Ls) variations                                 PRSE 18
  PR = 1.0d0 + a1*cos(pi180*(Lsun-phi1)) + a2*cos(2.0d0*pi180*(Lsun-phi2))
end subroutine PRSEAS_M05
!---------------------------------------------------------------------- PRSE 23
real(kind=PM_REEL) function qrhtp_M05(rh,T,p)
  !     Specific humidity q (g/kg) versus RH (0-1), temperature (K), and  QRHT  2
  !     pressure (mb) (Savijarvi, Contr. Atmos. Phys., 64, 103, 1991)     QRHT  3
  !                                                                       QRHT  4
  !     Saturation water vapor pressure es vs temperature T               QRHT  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: T,p,rh
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: es,esmax
  ! 
  !.. Intrinsic Functions .. 
  intrinsic exp
  ! 
  ! ... Executable Statements ...
  ! 
  es = 6.1135d0 * exp(22.542d0*(T-273.16d0)/(T+0.32d0))
  esmax = p / 1.59d0
  if (es > esmax) then
    es = esmax
  end if
  qrhtp_M05 = 1000.0d0 * rh * 0.407d0 * es / (p-0.59d0*es)
end function qrhtp_M05
!---------------------------------------------------------------------- QRHT 11
real(kind=PM_REEL) function RANDOM_M05(L)
  !                                                                       RAND  2
  !     Algorithm AS 183 Appl. Statist. (1982) Vol. 31, p.188             RAND  3
  !                                                                       RAND  4
  !     Returns a pseudo-random number rectangularly distributed          RAND  5
  !     between 0 and 1.                                                  RAND  6
  !                                                                       RAND  7
  !     IX, IY and IZ should be set to integer values between             RAND  8
  !     1 and 30,000 before first entry.                                  RAND  9
  !                                                                       RAND 10
  !     Integer arithmetic up to 30323 is required.                       RAND 11
  !                                                                       RAND 12
  !     Returns L = 0 unless random = 0 or random = 1, in which           RAND 13
  !     case L = 1                                                        RAND 14
  !                                                                       RAND 15
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(out) :: L
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: one = 1.0d0,zero = 0.0d0
  ! 
  !.. Intrinsic Functions .. 
  intrinsic mod, dble
  ! 
  !.. Common Blocks .. 
  common /RANDCOM_M05/ IX,IY,IZ
  !     For IX to IZ: Read, Overwritten
  ! 
  !... Variables in Common Block /RANDCOM_M05/ ... 
  integer :: IX,IY,IZ
  ! 
  ! ... Executable Statements ...
  ! 
  !     IX = 171 * Mod(IX, 177) -  2 * (IX / 177)                         RAND 19
  !     IY = 172 * Mod(IY, 176) - 35 * (IY / 176)                         RAND 20
  !     IZ = 170 * Mod(IZ, 178) - 63 * (IZ / 178)                         RAND 21
  !                                                                       RAND 22
  !     If (IX .lt. 0) IX = IX + 30269                                    RAND 23
  !     If (IY .lt. 0) IY = IY + 30307                                    RAND 24
  !     If (IZ .lt. 0) IZ = IZ + 30323                                    RAND 25
  !                                                                       RAND 26
  !     If integer arithmetic up to 5,212,632 is not available,           RAND 27
  !     the preceding 6 statements may be used instead of the following 3 RAND 28
  !                                                                       RAND 29
  IX = mod(171*IX,30269)
  IY = mod(172*IY,30307)
  IZ = mod(170*IZ,30323)
  !                                                                       RAND 33
  Random_M05 = mod(dble(IX)/30269.0d0+dble(IY)/30307.0d0+dble(IZ)/30323.0d0, &
                   one)
  L = 0
  if (Random_M05<=zero .or. Random_M05>=one) then
    L = 1
  end if
end function RANDOM_M05
!-----------------------------------------------------------------------RAND 40
subroutine ReadMGCM_M05(GCMDIR,version)
  !---    Reads NASA Ames Mars General Circulation Model (MGCM) 0-80 km   RDMG  2
  !       data (in binary format) and loads into data arrays for common   RDMG  3
  !       MGCMdata                                                        RDMG  4
  !       GCMDIR is directory name where MGCM data resides                RDMG  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ndust = 3
  integer, parameter :: nhgt = 17
  integer, parameter :: nlat = 25
  integer, parameter :: nbl = 3
  integer, parameter :: nf10 = 2
  integer, parameter :: ntf10 = 3
  integer, parameter :: ntesy = 2
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ihgt,ilatstep,ils,k,lastls,lat,lendir,ls,lsea,m
  real(kind=PM_REEL) :: xlat,ylat
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMDATA_M05/ TZA0,TZA1,TZP1,TZA2,TZP2,PZA0,PZA1,PZP1,PZA2,PZP2, &
                        DZA0,UZA0,UZA1,UZP1,UZA2,UZP2,VZA0,VZA1,VZP1,VZA2,VZP2
  !     For TZA0 to VZP2: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /MGCMdata_M05/ ... 
  real(kind=PM_REEL), dimension(nhgt,nlat,0:12,ndust) :: &
    DZA0,PZA0,PZA1,PZA2,PZP1,PZP2,TZA0,TZA1,TZA2,TZP1,TZP2,UZA0,UZA1,UZA2, &
    UZP1,UZP2,VZA0,VZA1,VZA2,VZP1,VZP2
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to f10TES: Not Read, Not Written
  !     For dustc: Maybe Read, Not Written
  !     For solact to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDMG 36
  !---    Initialize last Ls value processed to 0                         RDMG 37
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDMG 39
  ilatstep = 1800 / (nlat-1)
  !---    Compute string length for directory name                        RDMG 41
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all dust optical depths                            RDMG 44
  do m = 1,ndust
#ifdef __GFORTRAN__
    !---      Open MGCM input files for temperature, pressure, and density  RDMG 46
    open (32,file = GCMDIR(1:lendir)//"tpdlo"//DUSTC(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open MGCM input files for wind components                     RDMG 49
    open (33,file = GCMDIR(1:lendir)//"uvlo"//DUSTC(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
#else
    !---      Open MGCM input files for temperature, pressure, and density  RDMG 46
    open (32,file = GCMDIR(1:lendir)//"tpdlo"//DUSTC(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open MGCM input files for wind components                     RDMG 49
    open (33,file = GCMDIR(1:lendir)//"uvlo"//DUSTC(m)//version//".bin", &
          form = sysform,status = "old")
#endif
    !---      Step through all Ls values                                    RDMG 52
    do lsea = 30,360,30
      ls = lsea / 30
      !---      Step through all latitude grid points                         RDMG 55
      do lat = -900,900,ilatstep
        xlat = DBLE(lat) / 10.0d0
        i = 1 + (lat+900)/ilatstep
        !---      Step through all height levels                                RDMG 59
        do k = nhgt,1,-1
          !---        Read (binary) tide coefficients for temperature, pressure,  RDMG 61
          !           and density                                                 RDMG 62
          read (32,End = 1000) &
               ils, ihgt, ylat, TZA0(k,i,ls,m), TZA1(k,i,ls,m), &
               TZP1(k,i,ls,m), TZA2(k,i,ls,m), TZP2(k,i,ls,m), PZA0(k,i,ls,m), &
               PZA1(k,i,ls,m), PZP1(k,i,ls,m), PZA2(k,i,ls,m), PZP2(k,i,ls,m), &
               DZA0(k,i,ls,m)
          if (ils /= lsea) stop " Bad tpd Ls"
          if (ihgt /= 5*(k-1)) stop " Bad tpd Height"
          if (ylat /= xlat) stop " Bad tpd Latitude"
          !---        Read (binary) tide coefficients for wind components         RDMG 70
          read (33,End = 1000) &
               ils, ihgt, ylat, UZA0(k,i,ls,m), UZA1(k,i,ls,m), &
               UZP1(k,i,ls,m), UZA2(k,i,ls,m), UZP2(k,i,ls,m), VZA0(k,i,ls,m), &
               VZA1(k,i,ls,m), VZP1(k,i,ls,m), VZA2(k,i,ls,m), VZP2(k,i,ls,m)
          if (ils /= lsea) stop " Bad uv Ls"
          !---        Reset value of last Ls processed                            RDMG 76
          lastLs = iLs
          if (ihgt /= 5*(k-1)) then
            stop " Bad uv Height"
          elseif (ylat /= xlat) then
            stop " Bad uv Latitude"
          end if
        end do
      end do
    end do
    !---      Set data for Ls = 0 to data for Ls = 360                      RDMG 83
    do k = 1,nhgt
      do i = 1,nlat
        TZA0(k,i,0,m) = TZA0(k,i,12,m)
        TZA1(k,i,0,m) = TZA1(k,i,12,m)
        TZP1(k,i,0,m) = TZP1(k,i,12,m)
        TZA2(k,i,0,m) = TZA2(k,i,12,m)
        TZP2(k,i,0,m) = TZP2(k,i,12,m)
        PZA0(k,i,0,m) = PZA0(k,i,12,m)
        PZA1(k,i,0,m) = PZA1(k,i,12,m)
        PZP1(k,i,0,m) = PZP1(k,i,12,m)
        PZA2(k,i,0,m) = PZA2(k,i,12,m)
        PZP2(k,i,0,m) = PZP2(k,i,12,m)
        DZA0(k,i,0,m) = DZA0(k,i,12,m)
        UZA0(k,i,0,m) = UZA0(k,i,12,m)
        UZA1(k,i,0,m) = UZA1(k,i,12,m)
        UZP1(k,i,0,m) = UZP1(k,i,12,m)
        UZA2(k,i,0,m) = UZA2(k,i,12,m)
        UZP2(k,i,0,m) = UZP2(k,i,12,m)
        VZA0(k,i,0,m) = VZA0(k,i,12,m)
        VZA1(k,i,0,m) = VZA1(k,i,12,m)
        VZP1(k,i,0,m) = VZP1(k,i,12,m)
        VZA2(k,i,0,m) = VZA2(k,i,12,m)
        VZP2(k,i,0,m) = VZP2(k,i,12,m)
      end do
    end do
    !---    Close input files to re-use same unit number for next dust      RDMG109
    !       value                                                           RDMG110
    close (32)
    close (33)
    cycle
    !---    Terminate if not all Ls values have been processed              RDMG114
    1000 if (lastLs /= 360) stop " Incomplete 0-80 km MGCM data"
  end do
end subroutine ReadMGCM_M05
!-----------------------------------------------------------------------RDMG119
subroutine Readsurf_M05(GCMDIR,version)
  !---    Reads NASA Ames Mars General Circulation Model (MGCM) surface   RDSF  2
  !       data (in binary format) and loads into data arrays for common   RDSF  3
  !       surfdata                                                        RDSF  4
  !       GCMDIR is directory name where MGCM data resides                RDSF  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ndust = 3
  integer, parameter :: nbl = 3
  integer, parameter :: nlat = 25
  integer, parameter :: nlon = 40
  integer, parameter :: nf10 = 2
  integer, parameter :: ntf10 = 3
  integer, parameter :: ntesy = 2
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ilatstep,ils,jlon,k,lastls,lat,lendir,lon,ls,lsea,m
  real(kind=PM_REEL) :: xlat,ylat
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to f10TES: Not Read, Not Written
  !     For dustc: Maybe Read, Not Written
  !     For solact to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /SURFDATA_M05/ TSA0,TSA1,TSP1,TSA2,TSP2,USA0,USA1,USP1,USA2,USP2, &
                        VSA0,VSA1,VSP1,VSA2,VSP2
  !     For TSA0 to VSP2: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /surfdata_M05/ ... 
  real(kind=PM_REEL), dimension(nbl,nlat,0:nlon,0:12,ndust) :: &
    TSA0,TSA1,TSA2,TSP1,TSP2,USA0,USA1,USA2,USP1,USP2,VSA0,VSA1,VSA2,VSP1,VSP2
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDSF 41
  !---    Initialize last Ls value processed to 0                         RDSF 42
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDSF 44
  ilatstep = 1800 / (nlat-1)
  !---    Compute string length for directory name                        RDSF 46
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all dust optical depths                            RDSF 49
  do m = 1,ndust
#ifdef __GFORTRAN__
    !---      Open surface data files for surface level                     RDSF 51
    open (33,file = GCMDIR(1:lendir)//"sfc00"//DUSTC(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open surface data files for 5 meter level above surface       RDSF 54
    open (34,file = GCMDIR(1:lendir)//"sfc05"//DUSTC(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open surface data files for 30 meter level above surface      RDSF 57
    open (35,file = GCMDIR(1:lendir)//"sfc30"//DUSTC(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
#else
    !---      Open surface data files for surface level                     RDSF 51
    open (33,file = GCMDIR(1:lendir)//"sfc00"//DUSTC(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open surface data files for 5 meter level above surface       RDSF 54
    open (34,file = GCMDIR(1:lendir)//"sfc05"//DUSTC(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open surface data files for 30 meter level above surface      RDSF 57
    open (35,file = GCMDIR(1:lendir)//"sfc30"//DUSTC(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Step through all Ls values                                    RDSF 60
#endif
    do lsea = 30,360,30
      ls = lsea / 30
      !---      Step through all latitudes                                    RDSF 63
      do lat = -900,900,ilatstep
        xlat = DBLE(lat) / 10.0d0
        i = 1 + (lat+900)/ilatstep
        !---      Step through all boundary layer levels                        RDSF 67
        do k = 1,nbl
          !---      Step through all longitudes                                   RDSF 69
          do lon = nlon,1,-1
            !---        Read (binary) tide coefficients for temperature and wind    RDSF 71
            !           components at all boundary layer levels                     RDSF 72
            if (k == 1) then
              read (32+k,End = 1000) &
                   ils, ylat, jlon, TSA0(k,i,lon,ls,m), TSA1(k,i,lon,ls,m), &
                   TSP1(k,i,lon,ls,m), TSA2(k,i,lon,ls,m), TSP2(k,i,lon,ls,m)
              !---         Assume surface wind = 0 (no slip condition)                RDSF 77
              USA0(k,i,lon,ls,m) = 0.0d0
              USA1(k,i,lon,ls,m) = 0.0d0
              USP1(k,i,lon,ls,m) = 0.0d0
              USA2(k,i,lon,ls,m) = 0.0d0
              USP2(k,i,lon,ls,m) = 0.0d0
              VSA0(k,i,lon,ls,m) = 0.0d0
              VSA1(k,i,lon,ls,m) = 0.0d0
              VSP1(k,i,lon,ls,m) = 0.0d0
              VSA2(k,i,lon,ls,m) = 0.0d0
              VSP2(k,i,lon,ls,m) = 0.0d0
            else
              read (32+k,End = 1000) &
                   ils, ylat, jlon, TSA0(k,i,lon,ls,m), TSA1(k,i,lon,ls,m), &
                   TSP1(k,i,lon,ls,m), TSA2(k,i,lon,ls,m), TSP2(k,i,lon,ls,m), &
                   USA0(k,i,lon,ls,m), USA1(k,i,lon,ls,m), USP1(k,i,lon,ls,m), &
                   USA2(k,i,lon,ls,m), USP2(k,i,lon,ls,m), VSA0(k,i,lon,ls,m), &
                   VSA1(k,i,lon,ls,m), VSP1(k,i,lon,ls,m), VSA2(k,i,lon,ls,m), &
                   VSP2(k,i,lon,ls,m)
            end if
            if (ils /= lsea) stop " Bad surface Ls"
            !---        Reset value of last Ls processed                            RDSF 97
            lastLs = iLs
            if (ylat /= xlat) then
              stop " Bad surface Latitude"
            elseif (jlon /= 9*lon) then
              stop " Bad surface Longitude"
            end if
          end do
        end do
      end do
    end do
    !---      Set all values at Ls=0 to values at Ls=360                    RDSF105
    do k = 1,nbl
      do i = 1,nlat
        do lon = 1,nlon
          TSA0(k,i,lon,0,m) = TSA0(k,i,lon,12,m)
          TSA1(k,i,lon,0,m) = TSA1(k,i,lon,12,m)
          TSP1(k,i,lon,0,m) = TSP1(k,i,lon,12,m)
          TSA2(k,i,lon,0,m) = TSA2(k,i,lon,12,m)
          TSP2(k,i,lon,0,m) = TSP2(k,i,lon,12,m)
          USA0(k,i,lon,0,m) = USA0(k,i,lon,12,m)
          USA1(k,i,lon,0,m) = USA1(k,i,lon,12,m)
          USP1(k,i,lon,0,m) = USP1(k,i,lon,12,m)
          USA2(k,i,lon,0,m) = USA2(k,i,lon,12,m)
          USP2(k,i,lon,0,m) = USP2(k,i,lon,12,m)
          VSA0(k,i,lon,0,m) = VSA0(k,i,lon,12,m)
          VSA1(k,i,lon,0,m) = VSA1(k,i,lon,12,m)
          VSP1(k,i,lon,0,m) = VSP1(k,i,lon,12,m)
          VSA2(k,i,lon,0,m) = VSA2(k,i,lon,12,m)
          VSP2(k,i,lon,0,m) = VSP2(k,i,lon,12,m)
        end do
        !           Set all values at Lon=0 to values at Lon=360                RDSF125
        do ls = 0,12
          TSA0(k,i,0,ls,m) = TSA0(k,i,nlon,ls,m)
          TSA1(k,i,0,ls,m) = TSA1(k,i,nlon,ls,m)
          TSP1(k,i,0,ls,m) = TSP1(k,i,nlon,ls,m)
          TSA2(k,i,0,ls,m) = TSA2(k,i,nlon,ls,m)
          TSP2(k,i,0,ls,m) = TSP2(k,i,nlon,ls,m)
          USA0(k,i,0,ls,m) = USA0(k,i,nlon,ls,m)
          USA1(k,i,0,ls,m) = USA1(k,i,nlon,ls,m)
          USP1(k,i,0,ls,m) = USP1(k,i,nlon,ls,m)
          USA2(k,i,0,ls,m) = USA2(k,i,nlon,ls,m)
          USP2(k,i,0,ls,m) = USP2(k,i,nlon,ls,m)
          VSA0(k,i,0,ls,m) = VSA0(k,i,nlon,ls,m)
          VSA1(k,i,0,ls,m) = VSA1(k,i,nlon,ls,m)
          VSP1(k,i,0,ls,m) = VSP1(k,i,nlon,ls,m)
          VSA2(k,i,0,ls,m) = VSA2(k,i,nlon,ls,m)
          VSP2(k,i,0,ls,m) = VSP2(k,i,nlon,ls,m)
        end do
      end do
    end do
    !---      Close input file units                                        RDSF144
    close (33)
    close (34)
    close (35)
    cycle
    !---      Terminate if not all Ls values processed                      RDSF149
    1000 if (lastLs /= 360) stop " Incomplete surface GCM data"
  end do
end subroutine Readsurf_M05
!-----------------------------------------------------------------------RDSF154
subroutine ReadTGCM_M05(GCMDIR,version)
  !---    Reads University of Michigan Mars Thermospheric General Circu-  RDTG  2
  !       lation Model (MTGCM) data (in binary format) and loads into     RDTG  3
  !       data arrays for common TGCMdata                                 RDTG  4
  !       GCMDIR is directory name where MTGCM data resides               RDTG  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ndust = 3
  integer, parameter :: nhgtt = 19
  integer, parameter :: nlatt = 36
  integer, parameter :: nf10 = 2
  integer, parameter :: nbl = 3
  integer, parameter :: ntf10 = 3
  integer, parameter :: ntesy = 2
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ihgt,ilat1,ilat2,ilatstep,ils,k,lastls,lat,lendir,ls,lsea,m,n
  real(kind=PM_REEL) :: xlat,ylat,zfdust
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust: Maybe Read, Not Written
  !     For dzbl to f10TES: Not Read, Not Written
  !     For dustc to solact: Maybe Read, Not Written
  !     For TESyr to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /TGCMDATA_M05/ TTA0,TTA1,TTP1,TTA2,TTP2,PTA0,PTA1,PTP1,PTA2,PTP2, &
                        DTA0,DTA1,DTP1,DTA2,DTP2,UTA0,UTA1,UTP1,UTA2,UTP2, &
                        VTA0,VTA1,VTP1,VTA2,VTP2,ZFA0,ZFA1,ZFP1,ZFA2,ZFP2
  !     For TTA0 to ZFP2: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /TGCMdata_M05/ ... 
  real(kind=PM_REEL), dimension(nhgtt,nlatt,0:12,ndust,nf10) :: &
    DTA0,DTA1,DTA2,DTP1,DTP2,PTA0,PTA1,PTA2,PTP1,PTP2,TTA0,TTA1,TTA2,TTP1, &
    TTP2,UTA0,UTA1,UTA2,UTP1,UTP2,VTA0,VTA1,VTA2,VTP1,VTP2
  real(kind=PM_REEL), dimension(nlatt,0:12,ndust,nf10) :: &
    ZFA0,ZFA1,ZFA2,ZFP1,ZFP2
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDTG 44
  !---    Initialize last Ls value processed to 0                         RDTG 45
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDTG 47
  ilatstep = 1800 / nlatt
  !---    Set initial and final latitudes (x 10) for stepping             RDTG 49
  ilat1 = -900 + ilatstep/2
  ilat2 = 900 - ilatstep/2
  !---    Compute string length for directory name                        RDTG 52
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all solar activity levels                          RDTG 55
  do n = 1,nf10
    open (34,file = GCMDIR(1:lendir)//"zfht"//SOLACT(n)//version//".txt", &
          status = "old")
    !---    Step through all dust optical depths                            RDTG 59
    do m = 1,ndust
#ifdef __GFORTRAN__
      !---      Open MTGCM data files for temperature, pressure, and density  RDTG 61
      open (32, &
            file = GCMDIR(1:lendir)//"tpd"//SOLACT(n)//DUSTC(m)//version// &
                   ".bin", convert='big_endian', form = sysform,status = "old")
      !---      Open MTGCM data files for wind components                     RDTG 64
      open (33, &
            file = GCMDIR(1:lendir)//"uv"//SOLACT(n)//DUSTC(m)//version// &
                   ".bin", convert='big_endian', form = sysform,status = "old")
#else
      !---      Open MTGCM data files for temperature, pressure, and density  RDTG 61
      open (32, &
            file = GCMDIR(1:lendir)//"tpd"//SOLACT(n)//DUSTC(m)//version// &
                   ".bin",form = sysform,status = "old")
      !---      Open MTGCM data files for wind components                     RDTG 64
      open (33, &
            file = GCMDIR(1:lendir)//"uv"//SOLACT(n)//DUSTC(m)//version// &
                   ".bin",form = sysform,status = "old")
#endif

      !---      Step through all Ls values                                    RDTG 67
      do lsea = 30,360,30
        ls = lsea / 30
        !---      Step through all latitudes                                    RDTG 70
        do lat = ilat1,ilat2,ilatstep
          xlat = DBLE(lat) / 10.0d0
          i = 1 + (lat-ilat1)/ilatstep
          !---      Step through all heights                                      RDTG 74
          do k = 1,nhgtt
            !---        Read (binary) tide coefficients for temperature, pressure,  RDTG 76
            !           and density                                                 RDTG 77
            read (32,End = 1000) &
                 ils, ihgt, ylat, TTA0(k,i,ls,m,n), TTA1(k,i,ls,m,n), &
                 TTP1(k,i,ls,m,n), TTA2(k,i,ls,m,n), TTP2(k,i,ls,m,n), &
                 PTA0(k,i,ls,m,n), PTA1(k,i,ls,m,n), PTP1(k,i,ls,m,n), &
                 PTA2(k,i,ls,m,n), PTP2(k,i,ls,m,n), DTA0(k,i,ls,m,n), &
                 DTA1(k,i,ls,m,n), DTP1(k,i,ls,m,n), DTA2(k,i,ls,m,n), &
                 DTP2(k,i,ls,m,n)
            if (ils /= lsea) stop " Bad tpd Ls"
            if (ihgt /= 80+5*(k-1)) stop " Bad tpd Height"
            if (ylat /= xlat) stop " Bad tpd Latitude"
            !---        Read (binary) tide coefficients for wind components         RDTG 87
            read (33,End = 1000) &
                 ils, ihgt, ylat, UTA0(k,i,ls,m,n), UTA1(k,i,ls,m,n), &
                 UTP1(k,i,ls,m,n), UTA2(k,i,ls,m,n), UTP2(k,i,ls,m,n), &
                 VTA0(k,i,ls,m,n), VTA1(k,i,ls,m,n), VTP1(k,i,ls,m,n), &
                 VTA2(k,i,ls,m,n), VTP2(k,i,ls,m,n)
            if (ils /= lsea) stop " Bad uv Ls"
            !---        Reset last Ls value processed                               RDTG 93
            lastLs = iLs
            if (ihgt /= 80+5*(k-1)) then
              stop " Bad uv Height"
            elseif (ylat /= xlat) then
              stop " Bad uv Latitude"
            end if
          end do
          !---      Read tide coefficient data for ZF=height of 1.26 nbar level   RDTG 98
          read (34,*,End = 1000) &
               zfdust, ils, ylat, ZFA0(i,ls,m,n), ZFA1(i,ls,m,n), &
               ZFP1(i,ls,m,n), ZFA2(i,ls,m,n), ZFP2(i,ls,m,n)
          if (zfdust /= DUST(m)) then
            stop " Bad ZF dust value"
          elseif (ils /= lsea) then
            stop " Bad ZF Ls"
          elseif (ylat /= xlat) then
            stop " Bad ZF Latitude"
          end if
        end do
      !---      Step through all latitudes                                    RDTG 70
      end do
      !---      Set all values at Ls=0 to values at Ls=360                    RDTG105
      do i = 1,nlatt
        ZFA0(i,0,m,n) = ZFA0(i,12,m,n)
        ZFA1(i,0,m,n) = ZFA1(i,12,m,n)
        ZFP1(i,0,m,n) = ZFP1(i,12,m,n)
        ZFA2(i,0,m,n) = ZFA2(i,12,m,n)
        ZFP2(i,0,m,n) = ZFP2(i,12,m,n)
        do k = 1,nhgtt
          TTA0(k,i,0,m,n) = TTA0(k,i,12,m,n)
          TTA1(k,i,0,m,n) = TTA1(k,i,12,m,n)
          TTP1(k,i,0,m,n) = TTP1(k,i,12,m,n)
          TTA2(k,i,0,m,n) = TTA2(k,i,12,m,n)
          TTP2(k,i,0,m,n) = TTP2(k,i,12,m,n)
          PTA0(k,i,0,m,n) = PTA0(k,i,12,m,n)
          PTA1(k,i,0,m,n) = PTA1(k,i,12,m,n)
          PTP1(k,i,0,m,n) = PTP1(k,i,12,m,n)
          PTA2(k,i,0,m,n) = PTA2(k,i,12,m,n)
          PTP2(k,i,0,m,n) = PTP2(k,i,12,m,n)
          DTA0(k,i,0,m,n) = DTA0(k,i,12,m,n)
          DTA1(k,i,0,m,n) = DTA1(k,i,12,m,n)
          DTP1(k,i,0,m,n) = DTP1(k,i,12,m,n)
          DTA2(k,i,0,m,n) = DTA2(k,i,12,m,n)
          DTP2(k,i,0,m,n) = DTP2(k,i,12,m,n)
          UTA0(k,i,0,m,n) = UTA0(k,i,12,m,n)
          UTA1(k,i,0,m,n) = UTA1(k,i,12,m,n)
          UTP1(k,i,0,m,n) = UTP1(k,i,12,m,n)
          UTA2(k,i,0,m,n) = UTA2(k,i,12,m,n)
          UTP2(k,i,0,m,n) = UTP2(k,i,12,m,n)
          VTA0(k,i,0,m,n) = VTA0(k,i,12,m,n)
          VTA1(k,i,0,m,n) = VTA1(k,i,12,m,n)
          VTP1(k,i,0,m,n) = VTP1(k,i,12,m,n)
          VTA2(k,i,0,m,n) = VTA2(k,i,12,m,n)
          VTP2(k,i,0,m,n) = VTP2(k,i,12,m,n)
        end do
      end do
      !---    Close input file unit numbers                                   RDTG139
      close (32)
      close (33)
      cycle
      !---    Terminate if not all Ls values processed                        RDTG143
      1000 if (lastLs /= 360) stop " Incomplete 80-170 km MTGCM data"
    end do
    close (34)
  end do
end subroutine ReadTGCM_M05
!-----------------------------------------------------------------------RDTG150
subroutine RELLIPS_M05(LAT,LONW,Rref,z,gz,Oldrref,ctopohgt,calbedo,requa,rpole &
                      )
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  real(kind=PM_REEL), parameter :: RADEG = 57.29577958d0
  real(kind=PM_REEL), parameter :: omega = 0.004061250d0/RADEG
  real(kind=PM_REEL), parameter :: GM = 4.2828314258d7
  real(kind=PM_REEL), parameter :: J2 = 0.001958616128d0
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: LAT,LONW,requa,rpole,z
  real(kind=PM_REEL), intent(out) :: Oldrref,Rref,calbedo,ctopohgt,gz
  ! 
  !.. Local Scalars .. 
  integer :: IAU
  real(kind=PM_REEL) :: A,AB,B,C,P2,Rz,TLAT,XX,YY
  ! 
! DM1359 remove : private  
!.. External Calls .. 
!  external topoareo_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic cos, sin, sqrt, tan
  ! 
  ! ... Executable Statements ...
  ! 
  !---  User-selectable inputs for equatorial and polar ellipsoid radii   RLPS 23
  A = requa
  B = requa
  C = rpole
  !---  Mean equatorial radius for ellipsoid                              RLPS 27
  AB = sqrt(A*B)
  TLAT = tan(LAT/RADEG)
  !---  XX, YY = squares of x, y components of local ellipsoid radius     RLPS 30
  XX = (AB*C)**2 / (C**2+(AB*TLAT)**2)
  YY = XX * TLAT**2
  !---  Reference ellipsoid radius                                        RLPS 33
  Oldrref = sqrt(XX+YY)
  !---  Get MOLA areoid (Rref)                                            RLPS 35
  IAU = 2000
  call topoareo_M05(LAT,LONW,Rref,ctopohgt,calbedo,IAU)
  !---  Rz = total radius to current height z                             RLPS 38
  Rz = Rref + z
  !---  Acceleration of gravity including J2 and centrifugal terms        RLPS 40
  P2 = 1.5d0*(sin(LAT/RADEG)**2) - 0.5d0
  gz = (GM/Rz**2) * (1.0d0-3.0d0*J2*((AB/Rz)**2)*P2)
  gz = gz - 1000.0d0*Rz*(omega*cos(LAT/RADEG))**2
end subroutine RELLIPS_M05
!-----------------------------------------------------------------------RLPS 46
subroutine Rescale_M05(x)
  !---  Puts x into range 0 - 360                                         RSCL  2
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(inout) :: x
  ! 
  !.. Intrinsic Functions .. 
  intrinsic aint
  ! 
  ! ... Executable Statements ...
  ! 
  x = x/360.0d0 - aint(x/360.0d0) + 1.0d0
  x = (x-aint(x)) * 360.0d0
end subroutine Rescale_M05
!---------------------------------------------------------------------- RSCL  8
subroutine Shiftdif_M05(x)
  !---  Shifts difference x to be +/- and close to 0.0                    SHFD  2
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(inout) :: x
  ! 
  ! ... Executable Statements ...
  ! 
  if (x > 180.0d0) then
    x = x - 360.0d0
  elseif (x < -180.0d0) then
    x = x + 360.0d0
  end if
end subroutine Shiftdif_M05
!---------------------------------------------------------------------- SHFD 11
subroutine slopewind_M05(xlat,xlonW,hgtMOLA,time,umean,vmean,uew,vns,wvert)
  !---    Analytical slope winds solution from Ye, Segal, and Pielke,     SLWN  3
  !       J. Atmos. Sci., 47, 612, 1990.  (hereafter YSP)                 SLWN  4
  !                                                                       SLWN  5
  !       Inputs:                                                         SLWN  6
  !         xlat, xlonW = planeto-centric latitude, longitude(West), deg  SLWN  7
  !         hgtMOLA = planeto-centric height above MOLA areoid (km)       SLWN  8
  !         time    = local solar time (Mars hours)                       SLWN  9
  !       Outputs:                                                        SLWN 10
  !         uew, vns = eastward, northward wind components (from MOLA     SLWN 11
  !           slopes in eastward and northward directions                 SLWN 12
  !                                                                       SLWN 13
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: hgtMOLA,time,umean,vmean,xlat,xlonW
  real(kind=PM_REEL), intent(out) :: uew,vns,wvert
  ! 
  !.. Local Scalars .. 
  integer :: IAU
  real(kind=PM_REEL) :: B,B1,B2,B3,B4,C0,Cosfact,Fcross,Fupslp,Qslam,Rm,Rp,Rref, &
                      Wb,alphax,alphaxw,alphay,alphayw,calbedo,ctopohgt,f,h, &
                      hgtsfc,omega,p,pi180,topom,topop,xi,xlatm,xlatp,xlonm, &
                      xlonp,zfactor
  ! 
! DM1359: remove : private  
!.. External Calls .. 
!  external topoareo_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, cos, exp, sign, sin, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  alphaxw = 0.d0
  pi180 = atan(1.0d0) / 45.0d0
  IAU = 2000
  !---    Cosine factor for diurnal time dependence                       SLWN 17
  Cosfact = cos(15.0d0*pi180*(time-15.0d0))
  !---    Assumed diurnal variation in BL height                          SLWN 19
  h = 3.5d0 + Cosfact
  !---    Get MOLA areoid (Rref, km)                                      SLWN 21
  call topoareo_M05(xlat,xlonW,Rref,ctopohgt,calbedo,IAU)
  !---    Get height above surface                                        SLWN 23
  hgtsfc = hgtMOLA - ctopohgt
  !---    Zero slope winds at and below surface and above 4.5 km          SLWN 25
  if (hgtsfc<=0.0d0 .or. hgtsfc>=h) then
    uew = 0.0d0
    vns = 0.0d0
    wvert = 0.0d0
    return
  end if
  !---      Set typical values of parameters from YSP                     SLWN 32
  C0 = 0.07d0
  !---      Assumed diurnal variation in magnitude of Qs                  SLWN 34
  Qslam = 550.0d0 + 150.0d0*Cosfact
  !---      Normalized height above surface                               SLWN 36
  xi = hgtsfc / h
  !---      Get northward slope (alphay) from MOLA                        SLWN 38
  xlatp = xlat + 0.25d0
  xlatm = xlat - 0.25d0
  xlonp = xlonW
  xlonm = xlonW
  if (xlatp > 90.0d0) then
    xlatp = 180.0d0 - xlatp
    xlonp = xlonp + 180.0d0
    if (xlonp > 360.0d0) then
      xlonp = xlonp - 360.0d0
    end if
  end if
  if (xlatm < -90.0d0) then
    xlatm = -180.0d0 - xlatm
    xlonm = xlonm + 180.0d0
    if (xlonm > 360.0d0) then
      xlonm = xlonm - 360.0d0
    end if
  end if
  call topoareo_M05(xlatp,xlonp,Rp,topop,calbedo,IAU)
  call topoareo_M05(xlatm,xlonm,Rm,topom,calbedo,IAU)
  alphay = (topop-topom) / (0.5d0*pi180*Rref)
  alphayw = alphay
  !---      Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)       SLWN 57
  if (abs(alphay) > 0.0525d0) then
    alphay = sign(0.0525d0,alphay)
  end if
  !---      Get eastward slope (alphax) from MOLA                         SLWN 59
  if (abs(xlat) > 89.75d0) then
    alphax = 0.0d0
  else
    xlonp = xlonW - 0.25d0
    xlonm = xlonW + 0.25d0
    if (xlonp < 0.0d0) then
      xlonp = xlonp + 360.0d0
    end if
    if (xlonm > 360.0d0) then
      xlonm = xlonm - 360.0d0
    end if
    call topoareo_M05(xlat,xlonp,Rp,topop,calbedo,IAU)
    call topoareo_M05(xlat,xlonm,Rm,topom,calbedo,IAU)
    alphax = (topop-topom) / (0.5d0*pi180*cos(pi180*xlat)*Rref)
    alphaxw = alphax
    !---        Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)     SLWN 71
    if (abs(alphax) > 0.0525d0) then
      alphax = sign(0.0525d0,alphax)
    end if
  end if
  !---      For xlat ~ 0 use YSP equation (12)                            SLWN 74
  if (abs(xlat) < 0.01d0) then
    Fupslp = (Qslam/C0) * ((2.0d0+xi**2)/3.0d0-xi) * xi
    Fcross = 0.0d0
  else
    !---        For xlat ne 0, use YSP equations (13) and (14)              SLWN 79
    omega = 7.0777d-5
    f = 2.0d0 * omega * sin(pi180*xlat)
    Wb = 2.0d0 * Qslam / (1000.0d0*h*abs(f))
    B = sqrt(1000.0d0*abs(f)*h/(2.0d0*C0))
    p = exp(-4.0d0*B) - 2.0d0*exp(-2.0d0*B)*cos(2.0d0*B) + 1.0d0
    B1 = exp(-B*xi)
    B2 = exp(-B*(2.0d0+xi))
    B3 = exp(-B*(2.0d0-xi))
    B4 = exp(-B*(4.0d0-xi))
    !---        Upslope wind factor (YSP equation 13)                       SLWN 89
    Fupslp = Wb * ((B1-B4)*sin(B*xi)+(B2-B3)*sin(B*(2.0d0-xi))) / p
    !---        Cross-slope wind factor (YSP equation 14)                   SLWN 92
    Fcross = (f/abs(f)) * Wb * &
             (xi-1.0d0+((B1+B4)*cos(B*xi)-(B2+B3)*cos(B*(2.0d0-xi)))/p)
  end if
  !---      Eastward and northward wind components from up-slope and      SLWN 96
  !         cross-slope components and eastward and northward slopes      SLWN 97
  uew = Fupslp*alphax + Fcross*alphay
  vns = Fupslp*alphay - Fcross*alphax
  !---    Compute assumed time-of-day dependence on wind components       SLWN101
  uew = uew * Cosfact
  vns = vns * sin(15.0d0*pi180*(time-11.0d0))
  zfactor = 1.0d0
  if (xi >= 0.5d0) then
    zfactor = 0.5d0 * (1.0d0+cos(pi180*360.0d0*(xi-0.5d0)))
  end if
  wvert = zfactor * (alphaxw*(umean+uew)+alphayw*(vmean+vns))
end subroutine slopewind_M05
!-----------------------------------------------------------------------SLWN110
subroutine species_M05(hgt,xlat,als,Zbase,AMz,DENS,PRES,TEMP,iup,fmol,fmass, &
                       fmolH2O,fmassH2O)
  !                                                                       SPEC  3
  !---  Computes species concentrations as mole (or volume) fraction,     SPEC  4
  !     mass fraction, and number density (#/m**3).  Average mole         SPEC  5
  !     fraction and isotope ratio data are taken from Kieffer et al.,    SPEC  6
  !     editors, "Mars" (1992) and Tables A-5 and  A-6 of NASA/TM-2001-   SPEC  7
  !     210935 (2001).                                                    SPEC  8
  !                                                                       SPEC  9
  !     Notes:                                                            SPEC 10
  !                                                                       SPEC 11
  !     (1) Below 80 km, Mars MGCM assumes pure CO2 atmosphere (for       SPEC 12
  !     which molecular weight would be 44.01).  In this height range,    SPEC 13
  !     molecular weight computed from the perfect gas law relation       SPEC 14
  !     M = R0 * rho * T / p would give values close to, but not exactly  SPEC 15
  !     this value.  Deviations would be caused by the fact that the      SPEC 16
  !     ratio of the averages is not the same as the average of the       SPEC 17
  !     ratio, i.e.                                                       SPEC 18
  !          Avg(rho) * Avg(T) / Avg(p)    .ne.    Avg( rho * T / p)      SPEC 19
  !                                                                       SPEC 20
  !     (2) Below 80 km, this subroutine computes species concentrations  SPEC 21
  !     and resultant average molecular weight by assumptions given in    SPEC 22
  !     note (3). Therefore average molecular weight given by this        SPEC 23
  !     subroutine is not exactly the same as average molecular weight    SPEC 24
  !     computed from the perfect gas law [as given in note (1)].         SPEC 25
  !                                                                       SPEC 26
  !     (3) Below 80 km, this subroutine computes species concentrations  SPEC 27
  !     by assuming atmospheric mass density from MGCM is correct,        SPEC 28
  !     but species concentrations are calculated using the following     SPEC 29
  !     assumptions: (a) Average dry-atmosphere mole fractions (fbar),    SPEC 30
  !     are assumed. (b) Mole fractions are adjusted for seasonal         SPEC 31
  !     variation of mass of CO2 in the atmosphere, due to freezing and   SPEC 32
  !     sublimation of CO2 at the poles. (c) Only the partial pressure    SPEC 33
  !     of CO2 is assumed to vary seasonally, not the partial pressures   SPEC 34
  !     of other constituents, which are assumed not to freeze out of     SPEC 35
  !     the atmosphere. However, mole fractions of all species vary       SPEC 36
  !     seasonally due to this effect. (d) Seasonal variation of          SPEC 37
  !     total pressure is taken from subroutine PRSEAS_M05, which was     SPEC 38
  !     used in the original Stewart thermosphere model, and was          SPEC 39
  !     employed in Mars-GRAM up through version 3.8.  (e) Water vapor    SPEC 40
  !     concentration is added to the dry atmosphere by assuming relative SPEC 41
  !     humidity = 20% (computed by function qrhtp, the same as used in   SPEC 42
  !     marsrad.f calculations).                                          SPEC 43
  !                                                                       SPEC 44
  !     (4) Between 80 km and the base of the Stewart thermosphere (at    SPEC 45
  !     altitude zbase), a combination of information is used from MTGCM  SPEC 46
  !     and the modified Stewart model. MTGCM data used in Mars-GRAM do   SPEC 47
  !     not include calculated mole fractions or number densities.  Mole  SPEC 48
  !     fractions between 80 km and zbase height are computed from the    SPEC 49
  !     following assumptions: (a) Mole fractions for N2, Ar, and O2 are  SPEC 50
  !     assumed to be the same as their value at 80 km (from methods      SPEC 51
  !     described in note (3). (b) Mole fractions for He, H2, H, and H2O  SPEC 52
  !     are assumed to be zero. (c) Mole fractions for N2, Ar, O2, and CO SPEC 53
  !     are assumed to vary linearly from their values at 80 km [from     SPEC 54
  !     method in note (3)] to their values at zbase altitude (from the   SPEC 55
  !     Stewart model). (d) Mole fractions for CO2 and O are computed     SPEC 56
  !     from two constraint conditions - (i) Sum of the mole fractions    SPEC 57
  !     must be 1.0, and (ii) Input molecular weight (AMz, from MTGCM     SPEC 58
  !     value) is preserved.                                              SPEC 59
  !                                                                       SPEC 60
  !     (5) From height zbase up until MTGCM data runs out, a combination SPEC 61
  !     of information is used from MTGCM and the modified Stewart model. SPEC 62
  !     In this height range, the following assumptions are used: (a)     SPEC 63
  !     Mole fractions for constituents other than CO2 and O are taken    SPEC 64
  !     from the modified Stewart model, (b) Molecular weight from MTGCM  SPEC 65
  !     is assumed, (c)  Mole fractions for CO2 and O are computed from   SPEC 66
  !     two constraint conditions - (i) Sum of the mole fractions must    SPEC 67
  !     be 1.0, and (ii) Input molecular weight (AMz, from MTGCM value)   SPEC 68
  !     is preserved.                                                     SPEC 69
  !                                                                       SPEC 70
  !     (6) Above the top altitude for which MTGCM data are available,    SPEC 71
  !     the same methodology is used as in note (5), except that the      SPEC 72
  !     input value for molecular weight (AMz) is taken directly from     SPEC 73
  !     the modified Stewart model.                                       SPEC 74
  !                                                                       SPEC 75
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: iup
  real(kind=PM_REEL), intent(in) :: DENS,PRES,TEMP,Zbase,als,hgt,xlat
  real(kind=PM_REEL), intent(inout) :: AMz
  real(kind=PM_REEL), intent(out) :: fmassH2O,fmolH2O
  real(kind=PM_REEL), dimension(0:8), intent(inout) :: fmol
  real(kind=PM_REEL), dimension(0:8), intent(out) :: fmass
  ! 
  !.. Local Scalars .. 
  integer :: i
  real(kind=PM_REEL) :: AvN,D,PR,amH2O,farin,fcoin,fmoltot,fmsum,fn2in,fo2in, &
                      fsum,hgtfact,sum,totnd,xndH2O
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(0:8) :: amx,fbar
  real(kind=PM_REEL), dimension(0:8) :: xnd
  ! 
! DM1359 remove external : private
  !.. External Calls .. 
 ! external PRSEAS_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: qrhtp_M05
  ! 
  !.. Data Declarations .. 
  data amx/ &
       44.00903d0,28.01781d0,39.96093d0,31.99799d0,28.01003d0,15.99900d0, &
       4.00260d0,2.01746d0,1.00873d0/
  data fbar/0.9537d0,0.0275d0,0.0165d0,0.0013d0,0.0010d0,4*0.0d0/
  ! 
  ! ... Executable Statements ...
  ! 
  !---  Avaogadro's number (number per kg-mole)                           SPEC 88
  AvN = 6.02214d26
  !---  Molecular weight for water vapor                                  SPEC 90
  amH2O = 18.01646d0
  !---  Store input values of N2, Ar, O2, and CO mole fraction (= values  SPEC 92
  !     at height zbase if input height is between 80 km and zbase)       SPEC 93
  fn2in = fmol(1)
  farin = fmol(2)
  fo2in = fmol(3)
  fcoin = fmol(4)
  !---  Assume dry atmosphere, unless modified for heights up to 80 km    SPEC 98
  fmassH2O = 0.0d0
  fmolH2O = 0.0d0
  xndH2O = 0.0d0
  if (hgt <= zbase) then
    call PRSEAS_M05(als,xlat,PR) !---  Calculations for heights above zbase altitude                     SPEC163
    fmol(0) = 1.0d0 - (1.0d0-fbar(0))/PR
    fmoltot = fmol(0)
    do i = 1,8
      fmol(i) = fbar(i) / PR
      fmoltot = fmoltot + fmol(i)
    end do
    !---    Calculations for heights up to 80 km                            SPEC110
    if (hgt <= 80.0d0) then
      AMz = 0.0d0 !---    Calculations for heights between 80 km and zbase altitude       SPEC133
      !---      Molecular weight of dry atmosphere                            SPEC113
      do i = 0,8
        AMz = AMz + fmol(i)*amx(i)
      end do
      !---      Water vapor mass mixing ratio (kg-water/kg-dry atmosphere)    SPEC117
      fmassH2O = qrhtp_M05(0.2d0,TEMP,PRES/100.0d0) / 1000.0d0
      !---      Water vapor mole fraction                                     SPEC119
      fmolH2O = AMz * fmassH2O / amH2O
      !---      Rescale total mole fraction                                   SPEC121
      fmoltot = fmoltot + fmolH2O
      !---      Reclculate mole fractions and molecular weight of moist       SPEC123
      !         atmosphere                                                    SPEC124
      AMz = 0.0d0
      do i = 0,8
        fmol(i) = fmol(i) / fmoltot
        AMz = AMz + fmol(i)*amx(i)
      end do
      AMz = AMz + fmolH2O*amH2O
      !---      Water vapor number density                                    SPEC131
      xndH2O = fmolH2O * DENS * AvN / AMz
    else
      !---      Assume N2, Ar, O2, and CO mole fractions varies linearly      SPEC135
      !         between 80 km and zbase altitude                              SPEC136
      hgtfact = (hgt-80.0d0) / (zbase-80.0d0)
      fmol(1) = fmol(1) + (fn2in-fmol(1))*hgtfact
      fmol(2) = fmol(2) + (farin-fmol(2))*hgtfact
      fmol(3) = fmol(3) + (fo2in-fmol(3))*hgtfact
      fmol(4) = fmol(4) + (fcoin-fmol(4))*hgtfact
      !---      Solve for CO2 and O mole fractions using the following        SPEC142
      !         assumptions: (a) sum of mole fractions must be 1.0, and       SPEC143
      !         (b) input value of molecular weight (AMz) is preserved.       SPEC144
      !---      Set up two simultaneous linear equations from these           SPEC145
      !         assumptions.                                                  SPEC146
      fsum = 1.0d0 - fmol(1) - fmol(2) - fmol(3) - fmol(4) - fmol(6) - fmol(7) &
             - fmol(8)
      fmsum = AMz - fmol(1)*amx(1) - fmol(2)*amx(2) - fmol(3)*amx(3) - &
              fmol(4)*amx(4) - fmol(6)*amx(6) - fmol(7)*amx(7) - &
              fmol(8)*amx(8)
      D = amx(5) - amx(0)
      !---      Re-computed mole fractions for CO2 and O from two linear      SPEC153
      !         constraint equations                                          SPEC154
      fmol(0) = (fsum*amx(5)-fmsum) / D
      fmol(5) = (fmsum-amx(0)*fsum) / D
      if (fmol(5) < 0.0d0) then
        fmol(5) = 0.0d0
        fmol(0) = fsum
        AMz = fmol(0)*amx(0) + AMz - fmsum
      end if
    end if
  else
    !---    Solve for CO2 and O mole fractions using the following          SPEC165
    !       assumptions: (a) sum of mole fractions must be 1.0, and         SPEC166
    !       (b) input value of molecular weight (AMz) is preserved.         SPEC167
    !---    Set up two simultaneous linear equations from these             SPEC168
    !       assumptions.                                                    SPEC169
    fsum = 1.0d0 - fmol(1) - fmol(2) - fmol(3) - fmol(4) - fmol(6) - fmol(7) - &
           fmol(8)
    fmsum = AMz - fmol(1)*amx(1) - fmol(2)*amx(2) - fmol(3)*amx(3) - &
            fmol(4)*amx(4) - fmol(6)*amx(6) - fmol(7)*amx(7) - fmol(8)*amx(8)
    D = amx(5) - amx(0)
    !---    Re-computed mole fractions for CO2 and O from two linear        SPEC176
    !       constraint equations                                            SPEC177
    fmol(0) = (fsum*amx(5)-fmsum) / D
    fmol(5) = (fmsum-amx(0)*fsum) / D
    if (fmol(5) < 0.0d0) then
      fmol(5) = 0.0d0
      fmol(0) = fsum
      AMz = fmol(0)*amx(0) + AMz - fmsum
    end if
    if (fmol(0) < 0.0d0) then
      fmol(0) = 0.0d0
      fmol(5) = fsum
      AMz = fmol(5)*amx(5) + AMz - fmsum
    end if
  end if
  !---  Calculation of number densities and mass fractions (applicable    SPEC191
  !     for all height ranges)                                            SPEC192
  totnd = 0.0d0
  sum = 0.0d0
  !---  Species number densities and total number density                 SPEC195
  do i = 0,8
    xnd(i) = fmol(i) * DENS * AvN / AMz
    totnd = totnd + xnd(i)
    sum = sum + xnd(i)*amx(i)
  end do
  !---  Add water vapor number density to total                           SPEC201
  totnd = totnd + xndH2O
  !---  Compute mass fraction in % and rescale mole fraction to %         SPEC203
  sum = sum + xndH2O*amH2O
  do i = 0,8
    fmass(i) = 100.0d0 * xnd(i) * amx(i) / sum
    fmol(i) = 100.0d0 * fmol(i)
  end do
  if (iup > 0) then
    !---    Write species concentration data to LIST.txt file               SPEC210
    write (iup,10000) &
          (xnd(i), i = 0,4), (fmass(i), i = 0,4), (fmol(i), i = 0,4)
    !---    Water vapor not included above 80 km                            SPEC212
    if (hgt > 80.0d0) then
      write (iup,10001) &
            (xnd(i), i = 5,8), totnd, (fmass(i), i = 5,8), AMz, &
            (fmol(i), i = 5,8)
    else
      !---      Water vapor included below 80 km                              SPEC217
      fmassH2O = 100.0d0 * fmassH2O
      fmolH2O = 100.0d0 * fmolH2O
      write (iup,10002) &
            xndH2O, (xnd(i), i = 5,7), totnd, fmassH2O, (fmass(i), i = 5,7), &
            AMz, fmolH2O, (fmol(i), i = 5,7)
    end if
  end if
  return
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format ( &
        " CO2=",1p,e9.3," N2=",e9.3," Ar=",e9.3," O2=",e9.3," CO=",e9.3, &
        " #/m**3"/0p,f14.3,4f13.3," % by mass"/f14.3,4f13.3," % by volume")
  10001 format ( &
        "   O=",1p,e9.3," He=",e9.3," H2=",e9.3,"  H=",e9.3,"   Total=",e9.3, &
        " #/m**3"/0p,f14.3,3f13.3," % by mass  ","MolWgt=",f6.3/f14.3,3f13.3, &
        " % volume (mole) fraction")
  10002 format ( &
        " H2O=",1p,e9.3,"  O=",e9.3," He=",e9.3," H2=",e9.3,"   Total=",e9.3, &
        " #/m**3"/0p,f14.3,3f13.3," % by mass  ","MolWgt=",f6.3/f14.3,3f13.3, &
        " % volume (mole) fraction")
end subroutine species_M05
!-----------------------------------------------------------------------SPEC235
subroutine SublTchk_M05(temp,pres,Tsubl)
  !---  Assure temperatures greater than sublimation point for CO2.       STCK  2
  !     Coefficients from Kieffer et al. eds. "Mars" (Univ. Arizona       STCK  3
  !     Press book) 1992, page 959.                                       STCK  4
  !     Inputs are temp = temperature (K), pres = pressure (N/m**2)       STCK  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(inout) :: temp
  real(kind=PM_REEL), intent(in) :: pres
  real(kind=PM_REEL), intent(out) :: Tsubl
  ! 
  !.. Intrinsic Functions .. 
  intrinsic log
  ! 
  ! ... Executable Statements ...
  ! 
  Tsubl = 3182.48d0 / (23.3494d0-log(pres/100.0d0))
  if (temp < Tsubl) then
    temp = Tsubl
  end if
end subroutine SublTchk_M05
!-----------------------------------------------------------------------STCK 11
subroutine surfterp_M05(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,VMGCM,Hpres,Hdens, &
                        ctopohgt,TempDay,PresDay,DensDay,UwndDay,VwndDay, &
                        Hpres0,Tempmax,Tempmin,Densmax,Densmin,Tat5m,idaydata)
  !---    Interpolates Ames Mars General Circulation Model (MGCM) surface STRP  4
  !       data to a given latitude, longitude, time of year (Ls), and     STRP  5
  !       dust optical depth, for a given height index (khgt) and time of STRP  6
  !       day (time).                                                     STRP  7
  !       Some input data is provided by the Common "Interp".             STRP  8
  !---    Set parameter values for number of heights, boundary layer      STRP  9
  !       levels, latitudes, longitudes, and number of dust optical depth STRP 10
  !       values                                                          STRP 11
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nhgt = 17
  integer, parameter :: nbl = 3
  integer, parameter :: nlat = 25
  integer, parameter :: nlon = 40
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  integer, parameter :: ntf10 = 3
  integer, parameter :: ntesy = 2
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgt
  real(kind=PM_REEL), intent(in) :: ctopohgt,time
  real(kind=PM_REEL), intent(inout) :: Tat5m
  real(kind=PM_REEL), intent(out) :: &
    DMGCM,DensDay,Densmax,Densmin,Hdens,Hpres,Hpres0,PMGCM,PresDay,TMGCM, &
    TempDay,Tempmax,Tempmin,UMGCM,UwndDay,VMGCM,VwndDay
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,j,k1h,l,m
  real(kind=PM_REEL) :: A1,A1t,A2,A2t,D0,D1st,DZk1h,DZk1h1,Hdens0,Hpmax,Hpmin, &
                      P0,P1,P1max,P1min,P1st,P1t,P2,P2t,PZk1h,PZk1h1,Ptime, &
                      Pzero,Rzero,T0,T1st,T1time,TSzero,Tbar,Tbar0,Tmax1, &
                      Tmin1,Ttime,Tzero,U0,V0,Z1st,height,polefac,upolefac
  real(kind=PM_REEL) :: xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2,2) :: &
    DZh,DZh1,PZ0,PZ1,PZh,PZh1,Pmax,Pmin,RZ0,T1max,T1min,TZ0,TZ1
  real(kind=PM_REEL), dimension(2,2,2,2) :: &
    TM,TS0,Tday,Tmax,Tmin,UM,Uday,VM,Vday
  ! 
! DM1359 remove external : private
  !.. External Calls .. 
 ! external FourD_M05, ThreeD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Zlogr_M05
  !real(kind=PM_REEL), external :: TideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic exp, real
  ! 
  !.. Common Blocks .. 
  common /INTERP_M05/ DLAT,DLON,DLS,DDUST,DLATW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                      ILAT,JLON,LS,MDUST,K1ST,ILATW,ILATT,MF10
  !     For dlat to dlatw: Read, Not Written
  !     For dlatt to df10: Not Read, Not Written
  !     For wpolefac: Maybe Read, Not Written
  !     For tpolefac: Not Read, Not Written
  !     For ilat to ilatw: Read, Not Written
  !     For ilatt to mf10: Not Read, Not Written
  ! 
  !... Variables in Common Block /Interp_M05/ ... 
  real(kind=PM_REEL) :: DDUST,DF10,DLAT,DLATT,DLATW,DLON,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,K1ST,LS,MDUST,MF10
  common /MGCMDATA_M05/ TZA0,TZA1,TZP1,TZA2,TZP2,PZA0,PZA1,PZP1,PZA2,PZP2, &
                        DZA0,UZA0,UZA1,UZP1,UZA2,UZP2,VZA0,VZA1,VZP1,VZA2,VZP2
  !     For TZA0 to DZA0: Maybe Read, Not Written
  !     For UZA0 to VZP2: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMdata_M05/ ... 
  real(kind=PM_REEL), dimension(nhgt,nlat,0:12,ndust) :: &
    DZA0,PZA0,PZA1,PZA2,PZP1,PZP2,TZA0,TZA1,TZA2,TZP1,TZP2,UZA0,UZA1,UZA2, &
    UZP1,UZP2,VZA0,VZA1,VZA2,VZP1,VZP2
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust: Not Read, Not Written
  !     For dzbl: Maybe Read, Not Written
  !     For zwsfc to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /SURFDATA_M05/ TSA0,TSA1,TSP1,TSA2,TSP2,USA0,USA1,USP1,USA2,USP2, &
                        VSA0,VSA1,VSP1,VSA2,VSP2
  !     For TSA0 to VSP2: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /surfdata_M05/ ... 
  real(kind=PM_REEL), dimension(nbl,nlat,0:nlon,0:12,ndust) :: &
    TSA0,TSA1,TSA2,TSP1,TSP2,USA0,USA1,USA2,USP1,USP2,VSA0,VSA1,VSA2,VSP1,VSP2
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MGCM surface values at corners of a 4-dimensional     STRP 61
  !       cube in latitude-longitude-Ls-dust space, at a given height     STRP 62
  !       index (khgt) and time of day (time)                             STRP 63
  do i = 1,2
    polefac = 1.0d0
    upolefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == nlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    if (ILATW == 2) then
      if (i == 1) then
        upolefac = WPOLEFAC
      end if
    elseif (ILATW==nlat-1 .and. i==2) then
      upolefac = WPOLEFAC
    end if
    do j = 1,2
      do l = 1,2
        do m = 1,2
          !---      Daily mean temperature at level khgt                          STRP 80
          T0 = TSA0(khgt,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          Tday(i,j,l,m) = T0
          !---      Temperature tide amplitudes and phases                        STRP 83
          A1t = TSA1(khgt,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * polefac
          P1t = TSP1(khgt,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          A2t = TSA2(khgt,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * polefac
          P2t = TSP2(khgt,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          !---      Temperature at corners of 4-D cube                            STRP 88
          TM(i,j,l,m) = TideX_M05(T0,A1t,P1t,A2t,P2t,time)
          !---      Daily mean temperature at surface                             STRP 90
          TS0(i,j,l,m) = TSA0(1,ILAT+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          !---      Max and Min temperatures at corners of 4-D cube               STRP 92
          Tmax(i,j,l,m) = -9999.0d0
          Tmin(i,j,l,m) = 9999.0d0
          if (idaydata > 0) then
            do itime = 0,23
              xtime = DBLE(real(itime))
              Ttime = TideX_M05(T0,A1t,P1t,A2t,P2t,xtime)
              if (Ttime > Tmax(i,j,l,m)) then
                Tmax(i,j,l,m) = Ttime
              end if
              if (Ttime < Tmin(i,j,l,m)) then
                Tmin(i,j,l,m) = Ttime
              end if
            end do
          end if
          !---      Daily mean EW wind at level khgt                              STRP103
          U0 = USA0(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          Uday(i,j,l,m) = U0
          !---      EW wind tide coefficient amplitudes and phases                STRP106
          A1 = USA1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * upolefac
          P1 = USP1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          A2 = USA2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * upolefac
          P2 = USP2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          !---      EW wind at corners of 4-D cube                                STRP111
          UM(i,j,l,m) = TideX_M05(U0,A1,P1,A2,P2,time)
          !---      Daily mean NS wind at level khgt                              STRP113
          V0 = VSA0(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          Vday(i,j,l,m) = V0
          !---      NS wind coefficient amplitudes and phases                     STRP116
          A1 = VSA1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * upolefac
          P1 = VSP1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          A2 = VSA2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1) * upolefac
          P2 = VSP2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,MDUST+m-1)
          !---      NS wind at corners of 4-D cube                                STRP121
          VM(i,j,l,m) = TideX_M05(V0,A1,P1,A2,P2,time)
        end do
      end do
    end do
  end do
  !---    Use 4-D interpolation to get temperature, EW wind, NS wind,     STRP127
  !       and daily mean surface temperature at given latitude,           STRP128
  !       longitude, Ls, and dust optical depth                           STRP129
  call FourD_M05(DLAT,DLON,DLS,DDUST,TM,TMGCM)
  call FourD_M05(DLATW,DLON,DLS,DDUST,UM,UMGCM)
  call FourD_M05(DLATW,DLON,DLS,DDUST,VM,VMGCM)
  call FourD_M05(DLAT,DLON,DLS,DDUST,TS0,TSzero)
  call FourD_M05(DLAT,DLON,DLS,DDUST,Tday,TempDay)
  call FourD_M05(DLAT,DLON,DLS,DDUST,Tmax,Tempmax)
  call FourD_M05(DLAT,DLON,DLS,DDUST,Tmin,Tempmin)
  call FourD_M05(DLATW,DLON,DLS,DDUST,Uday,UwndDay)
  call FourD_M05(DLATW,DLON,DLS,DDUST,Vday,VwndDay)
  !---    k1h = height index just below k1st                              STRP139
  k1h = K1ST - 1
  if (k1h < 1) then
    k1h = 1
  end if
  !---    Establish MGCM values at height levels k1h, k1st and corners of STRP142
  !       a 3-dimensional cube in latitude-Ls-dust space, at given time   STRP143
  !       of day (time)                                                   STRP144
  do i = 1,2
    polefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == nlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    do l = 1,2
      do m = 1,2
        !---      Daily average pressure and density at level k1h               STRP154
        PZh(i,l,m) = PZA0(k1h,ILAT+i-1,LS+l-1,MDUST+m-1)
        DZh(i,l,m) = DZA0(k1h,ILAT+i-1,LS+l-1,MDUST+m-1)
        PZh1(i,l,m) = PZA0(k1h+1,ILAT+i-1,LS+l-1,MDUST+m-1)
        DZh1(i,l,m) = DZA0(k1h+1,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Pressure tide coefficient amplitudes and phases               STRP159
        P0 = PZA0(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        A1 = PZA1(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P1 = PZP1(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        A2 = PZA2(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P2 = PZP2(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Pressure values at corners of 3-D cube                        STRP165
        PZ1(i,l,m) = TideY_M05(P0,A1,P1,A2,P2,time)
        !---      Daily average pressure at level k1st                          STRP167
        PZ0(i,l,m) = P0
        !---      Level k1st Pressure at corners of 3-D cube                    STRP169
        Pmax(i,l,m) = -9999.0d0
        Pmin(i,l,m) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            Ptime = TideY_M05(P0,A1,P1,A2,P2,xtime)
            if (Ptime > Pmax(i,l,m)) then
              Pmax(i,l,m) = Ptime
            end if
            if (Ptime < Pmin(i,l,m)) then
              Pmin(i,l,m) = Ptime
            end if
          end do
        end if
        !---      Temperature tide coefficient amplitudes and phases            STRP180
        T0 = TZA0(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        A1 = TZA1(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P1 = TZP1(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        A2 = TZA2(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1) * polefac
        P2 = TZP2(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      temperature values at corners of 3-D cube                     STRP186
        TZ1(i,l,m) = TideX_M05(T0,A1,P1,A2,P2,time)
        !---      Level k1st Temperature at corners of 3-D cube                 STRP188
        T1max(i,l,m) = -9999.0d0
        T1min(i,l,m) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            T1time = TideX_M05(T0,A1,P1,A2,P2,xtime)
            if (T1time > T1max(i,l,m)) then
              T1max(i,l,m) = T1time
            end if
            if (T1time < T1min(i,l,m)) then
              T1min(i,l,m) = T1time
            end if
          end do
        end if
        !---      Daily average temperature at level k1st                       STRP199
        TZ0(i,l,m) = T0
        !---      Daily average density at level k1st                           STRP201
        D0 = DZA0(K1ST,ILAT+i-1,LS+l-1,MDUST+m-1)
        !---      Gas constant from pressure, density, and temperature          STRP203
        RZ0(i,l,m) = 190.0d0
        if (T0/=0.0d0 .and. D0/=0.0d0) then
          RZ0(i,l,m) = P0 / (T0*D0)
        end if
      end do
    end do
  end do
  !---    Do 3-D interpolation on pressure                                STRP209
  call ThreeD_M05(DLAT,DLS,DDUST,PZh,PZk1h)
  call ThreeD_M05(DLAT,DLS,DDUST,PZh1,PZk1h1)
  !---    Daily average pressure scale height                             STRP212
  Hpres0 = 5.0d0 / Zlogr_M05(PZk1h,PZk1h1,"STRP-01")
  !---    Do 3-D interpolation on density                                 STRP214
  call ThreeD_M05(DLAT,DLS,DDUST,DZh,DZk1h)
  call ThreeD_M05(DLAT,DLS,DDUST,DZh1,DZk1h1)
  !---    Daily average density scale height                              STRP217
  Hdens0 = 5.0d0 / Zlogr_M05(DZk1h,DZk1h1,"STRP-02")
  !---    Do 3-D interpolation on daily mean temperature                  STRP219
  call ThreeD_M05(DLAT,DLS,DDUST,TZ0,Tzero)
  call ThreeD_M05(DLAT,DLS,DDUST,PZ0,Pzero)
  !---    Daily average layer mean temperature                            STRP222
  Tbar0 = (Tzero+TSzero) / 2.0d0
  !---    Do 3-D interpolation on gas constant                            STRP224
  call ThreeD_M05(DLAT,DLS,DDUST,RZ0,Rzero)
  !---    Do 3-D interpolation on temperature and pressure                STRP226
  call ThreeD_M05(DLAT,DLS,DDUST,TZ1,T1st)
  call ThreeD_M05(DLAT,DLS,DDUST,PZ1,P1st)
  !---    Do 3-D interpolation on max,min pressure at level k1st          STRP229
  call ThreeD_M05(DLAT,DLS,DDUST,Pmax,P1max)
  call ThreeD_M05(DLAT,DLS,DDUST,Pmin,P1min)
  !---    Do 3-D interpolation on max,min temperature at level k1st       STRP232
  call ThreeD_M05(DLAT,DLS,DDUST,T1max,Tmax1)
  call ThreeD_M05(DLAT,DLS,DDUST,T1min,Tmin1)
  !---    Density from gas law                                            STRP235
  D1st = P1st / (Rzero*T1st)
  !---    Layer mean temperature at current time                          STRP237
  if (khgt==2 .and. Tat5m<=0.0d0) then
    Tat5m = TMGCM
  end if
  Tbar = (T1st+Tat5m) / 2.0d0
  !---    Pressure scale height and density scale height at current time  STRP240
  Hpres = Hpres0 * Tbar / Tbar0
  Hdens = Hdens0 * Tbar / Tbar0
  !---    Adjust pressure to height level, using pressure scale height    STRP243
  height = ctopohgt + DZBL(khgt)
  Z1st = 5.0d0 * (DBLE(K1ST)-1.0d0)
  PMGCM = P1st * exp((Z1st-height)/Hpres)
  PresDay = Pzero * exp((Z1st-height)/Hpres0)
  !---    Compute density from gas law, using pressure and temperature    STRP248
  DMGCM = PMGCM / (Rzero*TMGCM)
  DensDay = PresDay / (Rzero*TempDay)
  Densmax = -9999.0d0
  Densmin = 9999.0d0
  if (idaydata > 0) then
    !---    Daily maximum and minimum density                               STRP254
    Hpmin = Hpres0 * 0.5d0 * (Tmax1+Tempmax) / Tbar0
    Hpmax = Hpres0 * 0.5d0 * (Tmax1+Tempmin) / Tbar0
    P1max = P1max * exp((Z1st-height)/Hpmax)
    P1min = P1min * exp((Z1st-height)/Hpmin)
    Densmax = DensDay * (P1max/PresDay) * (TempDay/Tempmin)
    Densmin = DensDay * (P1min/PresDay) * (Tempday/Tempmax)
  end if
end subroutine surfterp_M05
!-----------------------------------------------------------------------STRP264
subroutine STEWART2_M05(RAUI,LAT,LON,LST,TOTALPRZ,TZ,TOTALMDZ,CHGT,RSTAR,H, &
                        MOLWTG,SIGMA,iu0,sunlat,deltaTEX,TINF,TF,ZF,Hrho, &
                        requa,rpole,tgrad)
  !                                                                       STW2  4
  !---  TIME-DEPENDENT MARS ATMOSPHERE MODEL, FORTRAN VERSION OF PROGRAM  STW2  5
  !     BY IAN STEWART, LABORATORY FOR ATMOSPHERIC AND SPACE PHYSICS,     STW2  6
  !     UNIV. OF COLORADO. FINAL REPORT JPL PO # NQ-802429                STW2  7
  !--------------------------------------------------------------------   STW2  8
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: iu0
  real(kind=PM_REEL), intent(in) :: &
    CHGT,LAT,LON,LST,RAUI,RSTAR,SIGMA,TF,ZF,deltaTEX,requa,rpole,sunlat
  real(kind=PM_REEL), intent(out) :: &
    H,Hrho,MOLWTG,TINF,TOTALMDZ,TOTALPRZ,TZ,tgrad
  ! 
  !.. Local Scalars .. 
  integer :: FLAG,i
  real(kind=PM_REEL) :: FBAR,FBARR,GZ,Oldrref,RAU,RF,RREF,Rog,SCALE,TF0,TINF0, &
                      TOTALNDZ,ZF0,ZZF,albedo,ctopohgt,dMdz
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(0:8) :: MDZ,NDZ,PRZ,fmolx
  real(kind=PM_REEL), dimension(0:11) :: ES
  ! 
  ! DM1359: remove external : private
  !.. External Calls .. 
  !external EScalc_M05, RELLIPS_M05, THERMOS_M05, Thermpar_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, exp
  ! 
  !.. Common Blocks .. 
  common /THERM_M05/ F107,STDL,FMOL
  !     For F107 to stdl: Read, Not Written
  !     For fmol: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /THERM_M05/ ... 
  real(kind=PM_REEL) :: F107,STDL
  real(kind=PM_REEL), dimension(0:8) :: FMOL
  ! 
  ! ... Executable Statements ...
  ! 
  FLAG = 0
  call EScalc_M05(STDL,SIGMA,ES)
  !---  DEVIATIONS FROM NOMINAL VALUES                                    STW2 24
  FBAR = F107 * exp(ES(0))
  !---  3 MONTH RUNNING MEAN OF 10.7 CM SOLAR FLUX                        STW2 26
  !---  IN UNITS OF 1.0E-22 W/CM**2                                       STW2 27
  RAU = RAUI
  !---  Convert solar 10.7 cm flux to Mars position                       STW2 29
  FBARR = FBAR / (RAU**2)
  call RELLIPS_M05(LAT,LON,RREF,CHGT,GZ,Oldrref,ctopohgt,albedo,requa,rpole)
  !---  Evaluate the basic parameters for the thermosphere model          STW2 33
  call Thermpar_M05(RAU,FBARR,LAT,LST,SUNLAT,TINF0,TF0,ZF0,SCALE)
  if (FLAG > 0) then
    write (iu0,*) " RREF, RAU, GZ = ", RREF, RAU, GZ
  end if
  !---  Height above base of thermosphere                                 STW2 36
  ZZF = CHGT - ZF
  if (FLAG > 0) then
    write (iu0,*) " ZF,CHGT,ZZF = ", ZF, CHGT, ZZF
  end if
  RF = RREF + ZF
  !---  Exospheric temperature                                            STW2 40
  TINF = TINF0*exp(ES(2)+ES(3)) + deltaTEX
  if (FLAG > 0) then
    write (iu0,*) " "
    write (iu0,*) "FROM PROC. DRAG-- ZF,RF,TINF,TF = "
    write (iu0,10000) ZF, RF, TINF, TF
    write (iu0,*) " "
  end if
  call THERMOS_M05(FLAG,ES,TINF,TF,LAT,LON,LST,ZF,RF,ZZF,TOTALPRZ,TOTALNDZ,TZ, &
                   MOLWTG,PRZ,NDZ,MDZ,TOTALMDZ,iu0,SCALE,tgrad,dMdz,requa, &
                   rpole,fmolx)
  if (abs(SIGMA) < 0.01d0) then
    do i = 0,8
      FMOL(i) = fmolx(i)
    end do
  end if
  !---  SCALE HEIGHT, km                                                  STW2 57
  Rog = RSTAR / (1000.0d0*MOLWTG*GZ)
  H = Rog * TZ
  Hrho = H / (1.0d0+tgrad*Rog-(H/MOLWTG)*dMdz)
  !---  Convert pressure to N/m**2                                        STW2 61
  TOTALPRZ = TOTALPRZ * 1.0d5
  !---  Convert density to kg/m**3                                        STW2 63
  TOTALMDZ = TOTALMDZ * 1000.0d0
  return
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format (f7.2,f8.2,3f7.2)
end subroutine STEWART2_M05
!-----------------------------------------------------------------------STW2 67
subroutine ThreeD_M05(dx,dy,dz,Array,Value)
  !---    3-Dimensional linear interpolation within a 1x1x1 cube (x,y,z)  THRD  2
  !       of Array(2,2,2) to position dx,dy,dz (all 0-1).                 THRD  3
  !       Value is value of interpolated output.                          THRD  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: dx,dy,dz
  real(kind=PM_REEL), dimension(2,2,2), intent(in) :: Array
  real(kind=PM_REEL), intent(out) :: Value
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: dxp,dyp,dzp
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Complementary displacements in x,y,z                            THRD  7
  dxp = 1.0d0 - dx
  dyp = 1.0d0 - dy
  dzp = 1.0d0 - dz
  !---    3-D interpolated Value from Array                               THRD 11
  Value = dxp*dyp*dzp*Array(1,1,1) + dxp*dyp*dz*Array(1,1,2) + &
          dxp*dy*dzp*Array(1,2,1) + dx*dyp*dzp*Array(2,1,1) + &
          dxp*dy*dz*Array(1,2,2) + dx*dyp*dz*Array(2,1,2) + &
          dx*dy*dzp*Array(2,2,1) + dx*dy*dz*Array(2,2,2)
end subroutine ThreeD_M05
!-----------------------------------------------------------------------THRD 18
subroutine THERMOS_M05(FLAG,ES,TINF,TF,LAT,LON,LST,ZF,RF,ZZF,TOTALPRZ, &
                       TOTALNDZ,TZ,MOLWTG,PRZ,NDZ,MDZ,TOTALMDZ,iu0,SCALE, &
                       tgrad,dMdz,requa,rpole,fmol)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: FLAG,iu0
  real(kind=PM_REEL), intent(in) :: &
    LAT,LON,LST,RF,SCALE,TF,TINF,ZF,ZZF,requa,rpole
  real(kind=PM_REEL), intent(out) :: &
    MOLWTG,TOTALMDZ,TOTALNDZ,TOTALPRZ,TZ,dMdz,tgrad
  real(kind=PM_REEL), dimension(0:8), intent(inout) :: MDZ,NDZ,PRZ
  real(kind=PM_REEL), dimension(0:8), intent(out) :: fmol
  real(kind=PM_REEL), dimension(0:11), intent(in) :: ES
  ! 
  !.. Local Scalars .. 
  integer :: I,J,K
  real(kind=PM_REEL) :: AO,BK,EXPS,FO,GF,Oldrref,P1BAR,PFH,PFH2,PFHE,PRESSF, &
                      RADEG,RATIO,RREF,Tsubl,YSC,albedo,amumass,ctopohgt, &
                      smolwtg,sprzi,sprzj,stotpr,stz,sysc
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(0:8) :: DM,FF,HH,M,XDM,XFF,XHH
  ! 
! DM1359: remove external : private
  !.. External Calls .. 
  !external RELLIPS_M05, SublTchk_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic cos, exp, log, sin, sqrt
  ! 
  ! ... Executable Statements ...
  ! 
  RADEG = 57.29577958d0
  !     RADEG = DEGREES/RADIAN                                            THRM 27
  BK = 1.38065d-16
  amumass = 1.66054d-24
  !---  CONSTANT FOR NUMBER DENSITY CALCULATIONS                          THRM 30
  call RELLIPS_M05(LAT,LON,RREF,ZF,GF,Oldrref,ctopohgt,albedo,requa,rpole)
  GF = 100.0d0 * GF
  !---  ACC. OF GRAVITY AT ZF                                             THRM 34
  !---  PRESSF = 1.24E-9   (as originally in Stewart's model)             THRM 35
  PRESSF = 1.26d-9
  !---  1.26E-3 dynes/cm**2 = 1.26 nbar AT ZF, BASE OF THERMOSPHERE       THRM 37
  !---  P1BAR = 1.013E6   (as originally in Stewart's model)              THRM 38
  P1BAR = 1.0d6
  !---  EARTH SURFACE PRESSURE                                            THRM 40
  AO = 0.18d0 * (1.0d0+ES(7))
  !---  AO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT              THRM 42
  FO = 0.01d0 * exp(ES(4)+ES(5))
  !---  FO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT I            THRM 44
  !---  Molecular weights using Mars isotopic ratios                      THRM 45
  M(0) = 44.0090d0
  !---  CO2 MOLECULAR WEIGHT                                              THRM 47
  M(1) = 28.0178d0
  !---  N2                                                                THRM 49
  M(2) = 39.9609d0
  !---  ARGON                                                             THRM 51
  M(3) = 31.9980d0
  !---  MOLECULAR OXYGEN                                                  THRM 53
  M(4) = 28.0100d0
  !---  CARBON MONOXIDE                                                   THRM 55
  M(5) = 15.9990d0
  !---  ATOMIC OXYGEN                                                     THRM 57
  M(6) = 4.00260d0
  !---  HELIUM                                                            THRM 59
  M(7) = 2.01746d0
  !---  MOLECULAR HYDROGEN                                                THRM 61
  M(8) = 1.00873d0
  !---  ATOMIC HYDROGEN MOLECULAR WEIGHT                                  THRM 63
  DM(0) = amumass * M(0)
  !---  CO2 MOLECULAR MASS                                                THRM 65
  DM(1) = amumass * M(1)
  !---  N2 MOLECULAR MASS                                                 THRM 67
  DM(2) = amumass * M(2)
  !---  ARGON MOLECULAR MASS                                              THRM 69
  DM(3) = amumass * M(3)
  !---  02 MOLECULAR MASS                                                 THRM 71
  DM(4) = amumass * M(4)
  !---  CARBON MONOXIDE MOLECULAR MASS                                    THRM 73
  DM(5) = amumass * M(5)
  !---  ATOMIC OXYGEN MOLECULAR MASS                                      THRM 75
  XDM(0) = amumass * M(6)
  !---  HELIUM MOLECULAR MASS                                             THRM 77
  XDM(1) = amumass * M(7)
  !---  H2 MOLECULAR MASS                                                 THRM 79
  XDM(2) = amumass * M(8)
  !---  H   MOLECULAR MASS                                                THRM 81
  !---  THE FOLLOWING IS THE COMPOSITION OF THE HETEROSPHERE              THRM 82
  FF(0) = 0.932d0
  !---  CO2                                                               THRM 84
  FF(1) = 0.027d0
  !---  NITROGEN                                                          THRM 86
  FF(2) = 0.016d0
  !---  ARGON                                                             THRM 88
  FF(3) = 0.002d0
  !---  MOLECULAR OXYGEN                                                  THRM 90
  FF(4) = 0.013d0
  !---  CARBON MONOXIDE                                                   THRM 92
  FF(5) = 0.010d0
  !---  ATOMIC OXYGEN                                                     THRM 94
  FF(5) = FO * (1.0d0-AO*sin(15.0d0*LST/RADEG)*cos(LAT/RADEG))
  PFHE = 3.3d-16 * TINF
  !---  EXOBASE (ZF) HELIUM PARTIAL PRESSURE                              THRM 97
  PFH2 = 2.4d-15
  !---  EXOBASE (ZF) H2 PARTIAL PRESSURE                                  THRM 99
  if (TINF <= 330.0d0) then
    PFH = 5.2d-16 * TINF * exp(-TINF/70.0d0)
  else
    RATIO = 1440.0d0 / TINF
    PFH = 5.8d-18 * sqrt(TINF) * exp(RATIO) / (1.0d0+RATIO)
  end if
  XFF(0) = PFHE / PRESSF
  XFF(1) = PFH2 / PRESSF
  XFF(2) = PFH / PRESSF
  MOLWTG = 0.0d0
  TOTALPRZ = 0.0d0
  TOTALMDZ = 0.0d0
  YSC = ZZF * RF / (RF+ZZF)
  EXPS = exp(-YSC/SCALE)
  TZ = TINF - (TINF-TF)*EXPS
  !---  Parameters for gradient of molecular weight                       THRM116
  smolwtg = 0.0d0
  stotpr = 0.0d0
  sysc = (ZZF+1.0d0) * RF / (RF+ZZF+1.0d0)
  stz = TINF - (TINF-TF)*exp(-sysc/SCALE)
  !---  Temperature gradient, K/km                                        THRM121
  tgrad = (TINF-TF) * (RF+YSC) / (SCALE*(RF+ZZF)) * EXPS
  do I = 0,5
    HH(I) = BK * TINF / (GF*DM(I)) / 1.0d5
    !---    SCALE HEIGHT                                                    THRM125
    PRZ(I) = PRESSF * FF(I) * exp(-YSC/HH(I)-(SCALE/HH(I))*log(TZ/TF))
    NDZ(I) = P1BAR * PRZ(I) / (BK*TZ)
    !---    NUMBER DENSITY HEAVY GASES                                      THRM129
    MDZ(I) = NDZ(I) * DM(I)
    TOTALMDZ = TOTALMDZ + MDZ(I)
    TOTALPRZ = TOTALPRZ + PRZ(I)
    MOLWTG = MOLWTG + PRZ(I)*M(I)
    !---    Molecular weight 1 km higher                                    THRM134
    sprzi = PRESSF * FF(I) * exp(-sysc/HH(I)-(SCALE/HH(I))*log(stz/TF))
    stotpr = stotpr + sprzi
    smolwtg = smolwtg + sprzi*M(I)
  end do
  do J = 0,2
    XHH(J) = BK * TINF / (GF*XDM(J)) / 1.0d5
    !---    SCALE HEIGHT                                                    THRM142
    PRZ(J+6) = PRESSF * XFF(J) * exp(-YSC/XHH(J)-(SCALE/XHH(J))*log(TZ/TF))
    NDZ(J+6) = P1BAR * PRZ(J+6) / (BK*TZ)
    !---    NUMBER DENSITY LIGHT GASES                                      THRM146
    MDZ(J+6) = NDZ(J+6) * XDM(J)
    TOTALMDZ = TOTALMDZ + MDZ(J+6)
    TOTALPRZ = TOTALPRZ + PRZ(J+6)
    MOLWTG = MOLWTG + PRZ(J+6)*M(J+6)
    !---    Molecular weight 1 km higher                                    THRM151
    sprzj = PRESSF * XFF(J) * exp(-sysc/XHH(J)-(SCALE/XHH(J))*log(stz/TF))
    stotpr = stotpr + sprzj
    smolwtg = smolwtg + sprzj*M(J+6)
  end do
  !---  Check that temperature >= sublimation temperature                 THRM157
  call SublTchk_M05(TZ,TOTALPRZ,Tsubl)
  MOLWTG = MOLWTG / TOTALPRZ
  !---  Change in molecular weight over 1 km                              THRM160
  dMdz = smolwtg/stotpr - MOLWTG
  TOTALNDZ = P1BAR * TOTALPRZ / (BK*TZ)
  if (FLAG > 0) then
    write (iu0,10000) &
          ZZF, TZ, MOLWTG, TOTALPRZ, TOTALNDZ, (NDZ(K), K = 0,3), TOTALMDZ, &
          (NDZ(K), K = 4,8)
    write (iu0,*) " "
  end if
  do K = 0,8
    fmol(K) = NDZ(K) / TOTALNDZ
  end do
  return
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format (2f6.1,f5.1,6e10.3/17x,6e10.3)
end subroutine THERMOS_M05
!-----------------------------------------------------------------------THRM174
real(kind=PM_REEL) function TideX_M05(A0,A1,phi1,A2,phi2,t)
  !---    Tide value at local solar time t, from mean value A0, amplitude TIDX  2
  !       A1 and phase phi1 of 24-hour period component, and amplitude A2 TIDX  3
  !       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TIDX  4
  !       A2 are in same physical units as mean term A0.  Phases are in   TIDX  5
  !       hours of local solar time.                                      TIDX  6
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: A0,A1,A2,phi1,phi2,t
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: pi
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, cos
  ! 
  ! ... Executable Statements ...
  ! 
  pi = 4.0d0 * atan(1.0d0)
  TideX_M05 = A0 + A1*cos(pi*(t-phi1)/12.0d0) + A2*cos(pi*(t-phi2)/6.0d0)
end function TideX_M05
!-----------------------------------------------------------------------TIDX 13
real(kind=PM_REEL) function TideY_M05(A0,A1,phi1,A2,phi2,t)
  !---    Tide value at local solar time t, from mean value A0, amplitude TIDY  2
  !       A1 and phase phi1 of 24-hour period component, and amplitude A2 TIDY  3
  !       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TIDY  4
  !       A2 are in relative units (% of mean term A0).  Phases are in    TIDY  5
  !       hours of local solar time.                                      TIDY  6
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: A0,A1,A2,phi1,phi2,t
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: pi
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, cos
  ! 
  ! ... Executable Statements ...
  ! 
  pi = 4.0d0 * atan(1.0d0)
  TideY_M05 = A0 * &
              (1.0d0+ &
               (A1*cos(pi*(t-phi1)/12.0d0)+A2*cos(pi*(t-phi2)/6.0d0))/100.0d0)
end function TideY_M05
!-----------------------------------------------------------------------TIDY 13
subroutine topoareo_M05(clat,clonwin,careoid,ctopohgt,calbedo,IAU)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nmtlat = 361
  integer, parameter :: nmtlon = 721
  integer, parameter :: nalblat = 181
  integer, parameter :: nalblon = 361
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: clat,clonwin
  real(kind=PM_REEL), intent(out) :: calbedo,careoid,ctopohgt
  integer, intent(in) :: IAU
  ! 
  !.. Local Scalars .. 
  integer :: ilatalb,ilatmt,jlonalb,jlonmt,n1lat,n2lat,n3lat,n4lat
  real(kind=PM_REEL) :: clonw,dlatalb,dlatmt,dlonalb,dlonmt,stepalb,stepalblat, &
                      stepalblon,stepmola,stepmolalat,stepmolalon
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2) :: albint,areoid,topohgt
  ! 
  ! DM 1359 remove external : private
  !.. External Calls .. 
  !external TwoD_M05
  ! 
  !.. External Functions .. 
  !integer, external :: Ifloor_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic nint
  ! 
  !.. Common Blocks .. 
  common /TERHGT_M05/ AREORAD,TOPOMOLA,ALBEDO
  !     For areorad to albedo: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /TERHGT_M05/ ... 
  real(kind=PM_REEL), dimension(0:nmtlat,0:nmtlon) :: AREORAD,TOPOMOLA
  real(kind=PM_REEL), dimension(0:nalblat,0:nalblon) :: ALBEDO
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Convert back to IAU 1991 rotation coordinates, if necessary     TOPO 11
  clonw = clonwin
  if (IAU == 2000) then
    clonw = clonw + 0.238d0
    if (clonw > 360.0d0) then
      clonw = clonw - 360.0d0
    end if
  end if
  !---    Latitude, Longitude steps sizes for MOLA data                   TOPO 17
  stepmolalat = 180.0d0 / (DBLE(nmtlat)-1.0d0)
  stepmolalon = 360.0d0 / (DBLE(nmtlon)-1.0d0)
  n1lat = nint(1000.0d0/stepmolalat)
  n2lat = 90*n1lat + 500
  !---    Compute MOLA latitude index and lat increment from gridpoint    TOPO 22
  stepmola = stepmolalat
  if (clat < -90.0d0+stepmolalat/2.0d0) then
    stepmola = stepmolalat / 2.0d0
    ilatmt = 0
    dlatmt = (clat+90.0d0) / stepmola
  elseif (clat > 90.0d0-stepmolalat/2.0d0) then
    stepmola = stepmolalat / 2.0d0
    ilatmt = nmtlat - 1
    dlatmt = (clat-90.0d0+stepmola) / stepmola
  else
    ilatmt = (Ifloor_M05(clat*DBLE(n1lat))+n2lat) / 1000
    dlatmt = ((clat+90.0d0)/stepmolalat) - (DBLE(ilatmt)-0.5d0)
  end if
  if (ilatmt > nmtlat-1) then
    ilatmt = nmtlat - 1
  end if
  if (ilatmt < 0) then
    ilatmt = 0
  end if
  !---    Compute MOLA longitude index and lon increment from gridpoint   TOPO 38
  jlonmt = Ifloor_M05((clonw+stepmolalon/2.0d0)/stepmolalon)
  if (jlonmt > nmtlon-1) then
    jlonmt = nmtlon - 1
  end if
  if (jlonmt < 0) then
    jlonmt = 0
  end if
  dlonmt = (clonw+stepmolalon/2.0d0-stepmolalon*DBLE(jlonmt)) / stepmolalon
  !---    Topographic heights at corners of 2-D square grid points        TOPO 43
  topohgt(1,1) = TOPOMOLA(ilatmt,jlonmt)
  topohgt(1,2) = TOPOMOLA(ilatmt,jlonmt+1)
  topohgt(2,1) = TOPOMOLA(ilatmt+1,jlonmt)
  topohgt(2,2) = TOPOMOLA(ilatmt+1,jlonmt+1)
  !---    Areoid radius at corners of 2-D square grid                     TOPO 48
  areoid(1,1) = AREORAD(ilatmt,jlonmt)
  areoid(1,2) = AREORAD(ilatmt,jlonmt+1)
  areoid(2,1) = AREORAD(ilatmt+1,jlonmt)
  areoid(2,2) = AREORAD(ilatmt+1,jlonmt+1)
  !---    Use 2-D interpolation to get topographic height at current      TOPO 53
  !       position                                                        TOPO 54
  call TwoD_M05(dlatmt,dlonmt,topohgt,ctopohgt)
  !---    Use 2-D interpolation to get areoid radius at current position  TOPO 56
  call TwoD_M05(dlatmt,dlonmt,areoid,careoid)
  !---    Latitude, Longitude steps sizes for albedo data                 TOPO 58
  stepalblat = 180.0d0 / (DBLE(nalblat)-1.0d0)
  stepalblon = 360.0d0 / (DBLE(nalblon)-1.0d0)
  n3lat = nint(1000.0d0/stepalblat)
  n4lat = 90*n3lat + 500
  !---    Compute albedo latitude index and lat increment from gridpoint  TOPO 63
  stepalb = stepalblat
  if (clat < -90.0d0+stepalblat/2.0d0) then
    stepalb = stepalblat / 2.0d0
    ilatalb = 0
    dlatalb = (clat+90.0d0) / stepalb
  elseif (clat > 90.0d0-stepalblat/2.0d0) then
    stepalb = stepalblat / 2.0d0
    ilatalb = nalblat - 1
    dlatalb = (clat-90.0d0+stepalb) / stepalb
  else
    ilatalb = (Ifloor_M05(clat*DBLE(n3lat))+n4lat) / 1000
    dlatalb = ((clat+90.0d0)/stepalblat) - (DBLE(ilatalb)-0.5d0)
  end if
  if (ilatalb > nalblat-1) then
    ilatalb = nalblat - 1
  end if
  if (ilatalb < 0) then
    ilatalb = 0
  end if
  !---    Compute albedo longitude index and lon increment from gridpoint TOPO 79
  jlonalb = Ifloor_M05((clonw+stepalblon/2.0d0)/stepalblon)
  if (jlonalb > nalblon-1) then
    jlonalb = nalblon - 1
  end if
  if (jlonalb < 0) then
    jlonalb = 0
  end if
  dlonalb = (clonw+stepalblon/2.0d0-stepalblon*DBLE(jlonalb)) / stepalblon
  !---    Albedo at corners of 2-D square grid points                     TOPO 84
  albint(1,1) = ALBEDO(ilatalb,jlonalb)
  albint(1,2) = ALBEDO(ilatalb,jlonalb+1)
  albint(2,1) = ALBEDO(ilatalb+1,jlonalb)
  albint(2,2) = ALBEDO(ilatalb+1,jlonalb+1)
  !---    Use 2-D interpolation to get albedo at current position         TOPO 89
  call TwoD_M05(dlatalb,dlonalb,albint,calbedo)
end subroutine topoareo_M05
!-----------------------------------------------------------------------TOPO 93
subroutine Thermpar_M05(RAU,FBARR,lat,LST,sunlat,TINF0,TF0,ZF0,SCALE)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: FBARR,LST,RAU,lat,sunlat
  real(kind=PM_REEL), intent(out) :: SCALE,TF0,TINF0,ZF0
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: LATMAX = 25.4d0
  real(kind=PM_REEL) :: A1,A2,Tavg,Tbar,Zavg,Zbar,cphi,factlat,pi180,poleamp,t1, &
                      t2
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, cos
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       TPAR  6
  !--------------------------------------------------------------------   TPAR  7
  !                                                                       TPAR  8
  !---  Thermospheric parameters, revised from the original Stewart       TPAR  9
  !     parameterizations:                                                TPAR 10
  !     SMA = 1.523691                                                    TPAR 11
  !     ZF0 = 124.4 * (SMA/RAU)                                           TPAR 12
  !     TINF0 = 4.11 * (11.0 + FBARR)                                     TPAR 13
  !     TF0 = 170.0 * (SMA/RAU)                                           TPAR 14
  !     SCALE = TF0 / 9.20                                                TPAR 15
  !                                                                       TPAR 16
  !     The new parameterizations are based on four data sets from the    TPAR 17
  !     University of Michigan Mars Thermospheric General Circulation     TPAR 18
  !     Model (MTGCM), cases MGS97L, MGS98L, MANC00, and MGS97E. For      TPAR 19
  !     a description of the MTGCM model and its output, see Bougher,     TPAR 20
  !     et al., Journal of Geophysical Research, vol. 95 (B9), pp.        TPAR 21
  !     14,811 - 14,827, August 30, 1990.                                 TPAR 22
  !                                                                       TPAR 23
  !-------------------------------------------------------------------    TPAR 24
  !                                                                       TPAR 25
  !     Inputs:                                                           TPAR 26
  !       RAU    = orbital position radius (AU)                           TPAR 27
  !       FBARR  = 10.7 cm solar flux at Mars position                    TPAR 28
  !       lat    = latitude for evaluation of parameters (degrees)        TPAR 29
  !       LST    = local solar time (Mars hours) at evaluation point      TPAR 30
  !       sunlat = latitude of sun (degrees)                              TPAR 31
  !     Outputs:                                                          TPAR 32
  !       TINF0  = Exospheric temperature (K)                             TPAR 33
  !       TF0    = Temperature at base of thermosphere (K)                TPAR 34
  !       ZF0    = Height of base of thermosphere (km)                    TPAR 35
  !       SCALE  = Scale height for temperature variations (km)           TPAR 36
  !                                                                       TPAR 37
  !     Output values are un-corrected for Stewart (ES array) variations, TPAR 38
  !     pressure and dust effects.  These factors are accounted for in    TPAR 39
  !     the STEWART2_M05 subroutine.  Adjustment factor deltaTEX is added TPAR 40
  !     after computation of exospheric temperature.                      TPAR 41
  !--------------------------------------------------------------------   TPAR 42
  !                                                                       TPAR 43
  !---  Degrees to radians conversion factor                              TPAR 44
  pi180 = atan(1.0d0) / 45.0d0
  !---  Global mean exospheric temperature (K) versus 10.7 cm flux        TPAR 46
  Tbar = 156.3d0 + 0.9427d0*FBARR
  !---  Zonal average exospheric temperature (K) versus latitude          TPAR 48
  Tavg = Tbar * (1.0d0+1.369d-4*sunlat*lat)
  !---  Phase angles (hours) for local solar time variation               TPAR 50
  t1 = 13.2d0 - 0.00119d0*sunlat*lat
  t2 = 9.4d0 - 0.00231d0*sunlat*lat
  !---  Amplitude factor for local solar time variation                   TPAR 53
  poleamp = 1.0d0
  if (abs(lat) > 85.0d0) then
    poleamp = 1.0d0 - (abs(lat)-85.0d0)/5.0d0
  end if
  cphi = cos(pi180*(lat+sunlat)/(1.0d0+LATMAX/90.0d0)) * poleamp
  !---  Exospheric temperature (K) versus local solar time                TPAR 57
  TINF0 = Tavg * &
          (1.0d0+0.22d0*cphi*cos(pi180*15.0d0*(LST-t1))+ &
           0.04d0*cphi*cos(pi180*30.0d0*(LST-t2)))
  !---  Global mean height of thermosphere base (km)                      TPAR 60
  Zbar = 197.94d0 - 49.058d0*RAU
  !---  Latitude variation factor                                         TPAR 62
  factlat = (sunlat/LATMAX) * (lat/77.5d0)**3
  !---  Zonal average base height (km) versus latitude                    TPAR 64
  Zavg = Zbar + 4.3d0*factlat
  !---  Amplitudes for local solar time variation                         TPAR 66
  A1 = (1.5d0-cos(pi180*4.0d0*lat)) * poleamp
  A2 = (2.3d0*(cos(pi180*(lat+0.5d0*sunlat)))**3) * poleamp
  !---  Phase angles (hours) for local solar time variation               TPAR 69
  t1 = 16.2d0 - (sunlat/LATMAX)*atan(pi180*10.0d0*lat)
  t2 = 11.5d0
  !---  Base height of thermosphere (km) versus local solar time          TPAR 72
  ZF0 = Zavg + A1*cos(pi180*15.0d0*(LST-t1)) + A2*cos(pi180*30.0d0*(LST-t2))
  !---  Global mean temperature (K) at thermosphere base, versus FBARR    TPAR 75
  Tbar = 113.7d0 + 0.5791d0*FBARR
  !---  Zonal average temperature at thermosphere base (K) vs. latitude   TPAR 77
  Tavg = Tbar * (1.0d0+0.186d0*factlat)
  !---  Amplitudes for local solar time variation                         TPAR 79
  A1 = (0.06d0-0.05d0*cos(pi180*4.0d0*lat)) * poleamp
  A2 = (0.1d0*(cos(pi180*(lat+0.5d0*sunlat)))**3) * poleamp
  !---  Phase angles (hours) for local solar time variation               TPAR 82
  t1 = 17.5d0 - 2.5d0*(sunlat/LATMAX)*atan(pi180*10.0d0*lat)
  t2 = 10.0d0 + 2.0d0*(lat/77.5d0)**2
  !---  Thermosphere base temperature (K) versus local solar time         TPAR 85
  TF0 = Tavg * &
        (1.0d0+A1*cos(pi180*15.0d0*(LST-t1))+A2*cos(pi180*30.0d0*(LST-t2)))
  !---  Global mean scale height (km) of thermospheric temperature        TPAR 88
  SCALE = 8.38d0 + 0.09725d0*FBARR
  !---  Zonal average temperature scale height (km) vs. latitude          TPAR 90
  SCALE = SCALE * (1.14d0-0.18d0*cos(pi180*lat))
end subroutine Thermpar_M05
!-----------------------------------------------------------------------TPAR 94
subroutine TGCMterp_M05(khgtt,time,TTGCM,PTGCM,DTGCM,UTGCM,VTGCM,ZF,TempDay, &
                        PresDay,DensDay,UwndDay,VwndDay,Tempmax,Tempmin, &
                        Densmax,Densmin,idaydata)
  !---    Interpolates University of Michigan Mars Thermospheric General  TTRP  4
  !       Circulation Model (MTGCM) data to a given latitude, time of     TTRP  5
  !       year (Ls), and dust optical depth, for a given height index     TTRP  6
  !       (khgtt) and time of day (time).                                 TTRP  7
  !       Some input data is provided by the Common "Interp".             TTRP  8
  !---    Set parameter values for number of heights (nhgtt), number      TTRP  9
  !       of latitudes (nlatt), and number of dust optical depth values   TTRP 10
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nhgtt = 19
  integer, parameter :: nlatt = 36
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgtt
  real(kind=PM_REEL), intent(in) :: time
  real(kind=PM_REEL), intent(out) :: &
    DTGCM,DensDay,Densmax,Densmin,PTGCM,PresDay,TTGCM,TempDay,Tempmax,Tempmin, &
    UTGCM,UwndDay,VTGCM,VwndDay,ZF
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,l,m,n
  real(kind=PM_REEL) :: A1,A1p,A2,A2p,D0,Dtime,P0,P1,P1p,P2,P2p,RTGCM,T0,Ttime, &
                      U0,V0,Z0,polefac,xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2,2,2) :: &
    DT,Dmax,Dmin,PT,Pday,R0,TT,Tday,Tmax,Tmin,UT,Uday,VT,Vday,ZT
  !
  ! DM1359 remove external : private
  !.. External Calls .. 
  !external FourD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: TideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic real
  ! 
  !.. Common Blocks .. 
  common /INTERP_M05/ DLAT,DLON,DLS,DDUST,DLATW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                      ILAT,JLON,LS,MDUST,K1ST,ILATW,ILATT,MF10
  !     For dlat to dlon: Not Read, Not Written
  !     For dls to ddust: Read, Not Written
  !     For dlatw: Not Read, Not Written
  !     For dlatt to df10: Read, Not Written
  !     For wpolefac: Not Read, Not Written
  !     For tpolefac: Maybe Read, Not Written
  !     For ilat to jlon: Not Read, Not Written
  !     For ls to mdust: Read, Not Written
  !     For k1st to ilatw: Not Read, Not Written
  !     For ilatt to mf10: Read, Not Written
  ! 
  !... Variables in Common Block /Interp_M05/ ... 
  real(kind=PM_REEL) :: DDUST,DF10,DLAT,DLATT,DLATW,DLON,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,K1ST,LS,MDUST,MF10
  common /TGCMDATA_M05/ TTA0,TTA1,TTP1,TTA2,TTP2,PTA0,PTA1,PTP1,PTA2,PTP2, &
                        DTA0,DTA1,DTP1,DTA2,DTP2,UTA0,UTA1,UTP1,UTA2,UTP2, &
                        VTA0,VTA1,VTP1,VTA2,VTP2,ZFA0,ZFA1,ZFP1,ZFA2,ZFP2
  !     For TTA0 to ZFP2: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /TGCMdata_M05/ ... 
  real(kind=PM_REEL), dimension(nhgtt,nlatt,0:12,ndust,nf10) :: &
    DTA0,DTA1,DTA2,DTP1,DTP2,PTA0,PTA1,PTA2,PTP1,PTP2,TTA0,TTA1,TTA2,TTP1, &
    TTP2,UTA0,UTA1,UTA2,UTP1,UTP2,VTA0,VTA1,VTA2,VTP1,VTP2
  real(kind=PM_REEL), dimension(nlatt,0:12,ndust,nf10) :: &
    ZFA0,ZFA1,ZFA2,ZFP1,ZFP2
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MTGCM values at corners of a 4-dimensional cube in    TTRP 42
  !       latitude-Ls-dust-F107 space, at the given height index (khgtt), TTRP 43
  !       and time of day (time)                                          TTRP 44
  do i = 1,2
    polefac = 1.0d0
    if (ILATT == 1) then
      if (i == 1) then
        polefac = TPOLEFAC
      end if
    elseif (ILATT==nlatt-1 .and. i==2) then
      polefac = TPOLEFAC
    end if
    do l = 1,2
      do m = 1,2
        do n = 1,2
          !---      Daily mean temperature                                        TTRP 55
          T0 = TTA0(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          Tday(i,l,m,n) = T0
          !---      Temperature tide amplitudes and phases                        TTRP 58
          A1 = TTA1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1 = TTP1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2 = TTA2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2 = TTP2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      Temperature at corners of 3-D cube                            TTRP 63
          TT(i,l,m,n) = TideX_M05(T0,A1,P1,A2,P2,time)
          !---      Max and Min temperatures at corners of 3-D cube               TTRP 65
          Tmax(i,l,m,n) = -9999.0d0
          Tmin(i,l,m,n) = 9999.0d0
          if (idaydata > 0) then
            do itime = 0,23
              xtime = DBLE(real(itime))
              Ttime = TideX_M05(T0,A1,P1,A2,P2,xtime)
              if (Ttime > Tmax(i,l,m,n)) then
                Tmax(i,l,m,n) = Ttime
              end if
              if (Ttime < Tmin(i,l,m,n)) then
                Tmin(i,l,m,n) = Ttime
              end if
            end do
          end if
          !---      Daily mean pressure                                           TTRP 76
          P0 = PTA0(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          Pday(i,l,m,n) = P0
          !---      Pressure tide amplitudes and phases                           TTRP 79
          A1p = PTA1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1p = PTP1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2p = PTA2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2p = PTP2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      Pressure at corners of 3-D cube                               TTRP 84
          PT(i,l,m,n) = TideY_M05(P0,A1p,P1p,A2p,P2p,time)
          !---      Daily mean density                                            TTRP 86
          D0 = DTA0(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      Density tide amplitudes and phases                            TTRP 88
          A1 = DTA1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1 = DTP1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2 = DTA2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2 = DTP2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      Density at corners of 3-D cube                                TTRP 93
          DT(i,l,m,n) = TideY_M05(D0,A1,P1,A2,P2,time)
          !---      Max and Min densities at corners of 3-D cube                  TTRP 95
          Dmax(i,l,m,n) = -9999.0d0
          Dmin(i,l,m,n) = 9999.0d0
          if (idaydata > 0) then
            do itime = 0,23
              xtime = DBLE(real(itime))
              Dtime = TideY_M05(D0,A1,P1,A2,P2,xtime)
              if (Dtime > Dmax(i,l,m,n)) then
                Dmax(i,l,m,n) = Dtime
              end if
              if (Dtime < Dmin(i,l,m,n)) then
                Dmin(i,l,m,n) = Dtime
              end if
            end do
          end if
          !---      Gas constant from pressure, density, and temperature          TTRP106
          R0(i,l,m,n) = 190.0d0
          if (T0/=0.0d0 .and. D0/=0.0d0) then
            R0(i,l,m,n) = P0 / (T0*D0)
          end if
          !---      Daily mean EW wind                                            TTRP109
          U0 = UTA0(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          Uday(i,l,m,n) = U0
          !---      EW wind tide amplitudes and phases                            TTRP112
          A1 = UTA1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1 = UTP1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2 = UTA2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2 = UTP2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      EW wind at corners of 3-D cube                                TTRP117
          UT(i,l,m,n) = TideX_M05(U0,A1,P1,A2,P2,time)
          !---      Daily mean NS wind                                            TTRP119
          V0 = VTA0(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          Vday(i,l,m,n) = V0
          !---      NS wind tide amplitudes and phases                            TTRP122
          A1 = VTA1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1 = VTP1(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2 = VTA2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2 = VTP2(khgtt,ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      NS wind at corners of 3-D cube                                TTRP127
          VT(i,l,m,n) = TideX_M05(V0,A1,P1,A2,P2,time)
          !---      Tide amplitudes and phases for ZF=height of 1.26 nbar level   TTRP129
          Z0 = ZFA0(ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A1 = ZFA1(ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P1 = ZFP1(ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          A2 = ZFA2(ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1) * polefac
          P2 = ZFP2(ILATT+i-1,LS+l-1,MDUST+m-1,MF10+n-1)
          !---      ZF values at corners of 3-D cube                              TTRP135
          ZT(i,l,m,n) = TideX_M05(Z0,A1,P1,A2,P2,time)
        end do
      end do
    end do
  end do
  !---    Use 4-D interpolation to get temperature, pressure, density,    TTRP141
  !       gas constant, EW wind, NS wind, and ZF height at given lati-    TTRP142
  !       tude, Ls, dust optical depth, and solar activity                TTRP143
  call FourD_M05(DLATT,DLS,DDUST,DF10,TT,TTGCM)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Tday,TempDay)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Tmax,Tempmax)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Tmin,Tempmin)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Dmax,Densmax)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Dmin,Densmin)
  call FourD_M05(DLATT,DLS,DDUST,DF10,PT,PTGCM)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Pday,PresDay)
  call FourD_M05(DLATT,DLS,DDUST,DF10,DT,DTGCM)
  call FourD_M05(DLATT,DLS,DDUST,DF10,R0,RTGCM)
  !---    Daily density from gas constant                                 TTRP154
  DensDay = PresDay / (RTGCM*TempDay)
  call FourD_M05(DLATT,DLS,DDUST,DF10,UT,UTGCM)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Uday,UwndDay)
  call FourD_M05(DLATT,DLS,DDUST,DF10,VT,VTGCM)
  call FourD_M05(DLATT,DLS,DDUST,DF10,Vday,VwndDay)
  call FourD_M05(DLATT,DLS,DDUST,DF10,ZT,ZF)
end subroutine TGCMterp_M05
!-----------------------------------------------------------------------TTRP163
subroutine TwoD_M05(dx,dy,Array,Value)
  !---    2-Dimensional linear interpolation within a 1x1 cube (x,y) of   TWOD  2
  !       Array(2,2) to position dx,dy (both 0-1).                        TWOD  3
  !       Value is value of interpolated output.                          TWOD  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: dx,dy
  real(kind=PM_REEL), dimension(2,2), intent(in) :: Array
  real(kind=PM_REEL), intent(out) :: Value
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: dxp,dyp
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Complementary displacements in x,y,z                            TWOD  7
  dxp = 1.0d0 - dx
  dyp = 1.0d0 - dy
  !---    Do 2-D linear interpolation to get Value from Array             TWOD 10
  Value = dxp*dyp*Array(1,1) + dxp*dy*Array(1,2) + dx*dyp*Array(2,1) + &
          dx*dy*Array(2,2)
end subroutine TwoD_M05
!-----------------------------------------------------------------------TWOD 15
subroutine Wavelon_M05(LonEW,Wlon,CLat,Height,DAY,wavepert)
  !---  Inputs                                                            WAVE  2
  !       LonEW:  0 if West Longitude phases, 1 if East Longitude phases  WAVE  3
  !       Wlon:   Current West Longitude (degrees)                        WAVE  4
  !       CLat:   Current Latitude (degrees)                              WAVE  5
  !       Height: Current altitude (km)                                   WAVE  6
  !     Output:                                                           WAVE  7
  !       wavepert: relative perturbation due to wave model               WAVE  8
  !                                                                       WAVE  9
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: LonEW
  real(kind=PM_REEL), intent(in) :: CLat,DAY,Height,Wlon
  real(kind=PM_REEL), intent(out) :: wavepert
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: Heightfact,Scale,dd,dphi1dt,dphi2dt,dphi3dt,phi1,phi2, &
                      phi3,pi180,polefact
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, cos, exp
  ! 
  !.. Common Blocks .. 
  common /WAVECOEF_M05/ WAVEA0,WAVEA1,WAVEPHI1,WAVEA2,WAVEPHI2,WAVEA3, &
                        WAVEPHI3,WAVETIME,WAVEDATA,WSCALE,PHI1DOT,PHI2DOT, &
                        PHI3DOT,WAVEDATE,NWAVE,IUWAVE
  !     For WaveA0 to Wavephi3: Read, Not Written
  !     For wavetime to wavedata: Not Read, Not Written
  !     For Wscale to WaveDate: Read, Not Written
  !     For nwave to iuwave: Not Read, Not Written
  ! 
  !... Variables in Common Block /Wavecoef_M05/ ... 
  real(kind=PM_REEL) :: WAVEA0,WAVEA1,WAVEA2,WAVEA3,WAVEPHI1,WAVEPHI2,WAVEPHI3
  real(kind=PM_REEL), dimension(100) :: WAVETIME
  real(kind=PM_REEL), dimension(100,11) :: WAVEDATA
  real(kind=PM_REEL) :: WAVEDATE,WSCALE,PHI1DOT,PHI2DOT,PHI3DOT
  integer :: IUWAVE,NWAVE
  ! 
  ! ... Executable Statements ...
  ! 
  !---  Scale for height variation of wave perturbations below 100 km     WAVE 14
  Scale = WSCALE
  Heightfact = 1.0d0
  !---  Assume exponential variation with height for waves below 100 km   WAVE 17
  if (Height < 100.0d0) then
    Heightfact = exp((Height-100.0d0)/Scale)
  end if
  pi180 = atan(1.0d0) / 45.0d0
  dd = DAY - WAVEDATE
  phi1 = WAVEPHI1
  phi2 = WAVEPHI2
  phi3 = WAVEPHI3
  dphi1dt = PHI1DOT
  dphi2dt = PHI2DOT
  dphi3dt = PHI3DOT
  !---  Convert phases to West longitude if LonEW = 1                     WAVE 27
  if (LonEW == 1) then
    phi1 = 360.0d0 - phi1
    phi2 = 360.0d0 - phi2
    phi3 = 360.0d0 - phi3
    dphi1dt = -PHI1DOT
    dphi2dt = -PHI2DOT
    dphi3dt = -PHI3DOT
  end if
  if (WAVEDATE <= 0.0d0) then
    dd = 0.0d0
    dphi1dt = 0.0d0
    dphi2dt = 0.0d0
    dphi3dt = 0.0d0
  end if
  !---  Relative perturbation factor due to wave model                    WAVE 42
  wavepert = (WAVEA0+WAVEA1*cos(pi180*(Wlon-phi1-dphi1dt*dd))+ &
              WAVEA2*cos(2.0d0*pi180*(Wlon-phi2-dphi2dt*dd))+ &
              WAVEA3*cos(3.0d0*pi180*(Wlon-phi3-dphi3dt*dd))-1.0d0) * &
             Heightfact
  !---  Insure wave perturbation goes to (WaveA0-1.)*Heightfact at poles  WAVE 47
  if (abs(CLat) >= 85.0d0) then
    polefact = (cos(18.0d0*pi180*(abs(CLat)-85.0d0)))**2
    wavepert = (wavepert-(WAVEA0-1.0d0)*Heightfact)*polefact + &
               (WAVEA0-1.0d0)*Heightfact
  end if
  !---  Insure wave perturbation within proper limits                     WAVE 53
  if (wavepert < -0.9d0) then
    wavepert = -0.9d0
  end if
  if (wavepert > 9.0d0) then
    wavepert = 9.0d0
  end if
end subroutine Wavelon_M05
!-----------------------------------------------------------------------WAVE 58
real(kind=PM_REEL) function Zlogr_M05(znumer,zdenom,label)
  !---    Log of znumer/zdenom with error message for znumer, zdenom <= 0 ZLGR  2
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: zdenom,znumer
  character(LEN=7), intent(in) :: label
  ! 
  !.. Intrinsic Functions .. 
  intrinsic log
  ! 
  ! ... Executable Statements ...
  ! 
  if (znumer<=0.0d0 .or. zdenom<=0.0d0) then
    Zlogr_M05 = 1.0d0
    write (*,*) " Log error at ", label, znumer, zdenom
  else
    Zlogr_M05 = log(znumer/zdenom)
  end if
end function Zlogr_M05
!-----------------------------------------------------------------------ZLGR 13
subroutine ReadTES_M05(DATADIR,version)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nmapyear = 2
  integer, parameter :: ntesls = 72
  integer, parameter :: nteslat = 25
  integer, parameter :: nteslon = 40
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: DATADIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: iunit,lendir
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /TESDUST_M05/ TESTAU
  !     For testau: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /TESdust_M05/ ... 
  real(kind=PM_REEL), dimension(nmapyear,ntesls,nteslat,0:nteslon) :: TESTAU
  ! 
  ! ... Executable Statements ...
  ! 
  iunit = 67
  !---     Compute character string length of DATADIR path name           RTES 15
  lendir = index(DATADIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---     Open and read TES dust optical depths vs year, Ls, lat, lon    RTES 18
#ifdef __GFORTRAN__
  open (iunit,File = DATADIR(1:lendir)//"TESdust"//version//".bin", &
        convert='big_endian', form = sysform,status = "old")
#else
  open (iunit,File = DATADIR(1:lendir)//"TESdust"//version//".bin", &
        form = sysform,status = "old")
#endif
  read (iunit) TESTAU
  close (iunit)
end subroutine ReadTES_M05
!---------------------------------------------------------------------  RTES 25
subroutine TESOD_M05(MapYear,xls,ylat,zlon,DustOD)
  !---     Interpolate TES optical depths vs Ls, lat and lon for given    TESD  2
  !        TES mapping year                                               TESD  3
  !---     Set parameter values for number of years, Ls's, lats and lons  TESD  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nmapyear = 2
  integer, parameter :: ntesls = 72
  integer, parameter :: nteslat = 25
  integer, parameter :: nteslon = 40
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: MapYear
  real(kind=PM_REEL), intent(in) :: xls,ylat,zlon
  real(kind=PM_REEL), intent(out) :: DustOD
  ! 
  !.. Local Scalars .. 
  integer :: ils1,ils2,jlat1,jlat2,jlon1,jlon2
  real(kind=PM_REEL) :: dlat,dlon,dls,halfstep,steplat,steplon,stepls,xls1,ylat1
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2,2) :: Array
  !
  ! DM1359 remove external : private
  !.. External Calls .. 
  !external ThreeD_M05
  ! 
  !.. External Functions .. 
  !integer, external :: Ifloor_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic int
  ! 
  !.. Common Blocks .. 
  common /TESDUST_M05/ TESTAU
  !     For testau: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /TESdust_M05/ ... 
  real(kind=PM_REEL), dimension(nmapyear,ntesls,nteslat,0:nteslon) :: TESTAU
  ! 
  ! ... Executable Statements ...
  ! 
  !---     Set latitude, longitude and Ls data step intervals             TESD 12
  steplat = 180.0d0 / (DBLE(nteslat)-1.0d0)
  steplon = 360.0d0 / DBLE(nteslon)
  stepls = 360.0d0 / DBLE(ntesls)
  halfstep = stepls / 2.0d0
  !---     Find lat interpolation index values jlat1, jlat2               TESD 17
  jlat1 = int((ylat+90.0d0+steplat)/steplat)
  if (jlat1 >= nteslat) then
    jlat1 = nteslat - 1
  end if
  jlat2 = jlat1 + 1
  ylat1 = -(90.0d0+steplat) + steplat*DBLE(jlat1)
  !---     Increment in lat within interpolation box                      TESD 22
  dlat = (ylat-ylat1) / steplat
  ils1 = int((xls+halfstep)/stepls)
  xls1 = stepls*DBLE(ils1) - halfstep
  !---     Find Ls interpolation index values ils1, ils2                  TESD 26
  ils2 = ils1 + 1
  !---     Increment in Ls within interpolation box                       TESD 28
  dls = (xls-xls1) / stepls
  !---     Adjust Ls index values near Ls = 0 or 360                      TESD 30
  if (ils1==0 .or. ils1==72) then
    ils1 = 72
    ils2 = 1
  end if
  !---     Find lon interpolation index jlon                              TESD 35
  jlon1 = Ifloor_M05(zlon/steplon)
  if (jlon1 > nteslon-1) then
    jlon1 = nteslon - 1
  end if
  jlon2 = jlon1 + 1
  dlon = (zlon-steplon*DBLE(jlon1)) / steplon
  !---     Fill 3-d array box for interpolation                           TESD 40
  Array(1,1,1) = TESTAU(MapYear,ils1,jlat1,jlon1)
  Array(1,1,2) = TESTAU(MapYear,ils1,jlat1,jlon2)
  Array(1,2,1) = TESTAU(MapYear,ils1,jlat2,jlon1)
  Array(1,2,2) = TESTAU(MapYear,ils1,jlat2,jlon2)
  Array(2,1,1) = TESTAU(MapYear,ils2,jlat1,jlon1)
  Array(2,1,2) = TESTAU(MapYear,ils2,jlat1,jlon2)
  Array(2,2,1) = TESTAU(MapYear,ils2,jlat2,jlon1)
  Array(2,2,2) = TESTAU(MapYear,ils2,jlat2,jlon2)
  !---     Do 3-D interpolation on Ls, lat and lon                        TESD 49
  call ThreeD_M05(dls,dlat,dlon,Array,DustOD)
end subroutine TESOD_M05
!---------------------------------------------------------------------  TESD 53
!-------------------------- END OF FILE marssub_M05.F90 -----------------------
!------------------------------------------------------------------------------






!------------------------------------------------------------------------------
!------------------------- START OF FILE TESsubs_M05.F90 ----------------------
!------------------------------------------------------------------------------ 

subroutine RdTESsrf_M05(GCMDIR,version)
  !---    Reads NASA Ames Mars General Circulation Model (MGCM) surface   RDTS  2
  !       data (in binary format) for TES Mapping Years, and loads into   RDTS  3
  !       data arrays for common surfTES                                  RDTS  4
  !       GCMDIR is directory name where MGCM data resides                RDTS  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ntesy = 2
  integer, parameter :: ntbl = 3
  integer, parameter :: ntlat = 25
  integer, parameter :: ntlon = 40
  integer, parameter :: ntf10 = 3
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ilatstep,ils,jlon,k,lastls,lat,lendir,lon,ls,lsea,m
  real(kind=PM_REEL) :: xlat,ylat
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to solact: Not Read, Not Written
  !     For TESyr: Maybe Read, Not Written
  !     For solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(ntbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /SURFTES_M05/ TTSA0,TTSA1,TTSP1,TTSA2,TTSP2,TUSA0,TUSA1,TUSP1,TUSA2, &
                       TUSP2,TVSA0,TVSA1,TVSP1,TVSA2,TVSP2
  !     For tTSA0 to tVSP2: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /surfTES_M05/ ... 
  real(kind=PM_REEL), dimension(ntbl,ntlat,0:ntlon,0:12,ntesy) :: &
    TTSA0,TTSA1,TTSA2,TTSP1,TTSP2,TUSA0,TUSA1,TUSA2,TUSP1,TUSP2,TVSA0,TVSA1, &
    TVSA2,TVSP1,TVSP2
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDTS 41
  !---    Initialize last Ls value processed to 0                         RDTS 42
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDTS 44
  ilatstep = 1800 / (ntlat-1)
  !---    Compute string length for directory name                        RDTS 46
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all dust optical depths                            RDTS 49
  do m = 1,ntesy

#ifdef __GFORTRAN__
    !---      Open surface data files for surface level                     RDTS 51
    open (33,file = GCMDIR(1:lendir)//"sfc00"//TESYR(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open surface data files for 5 meter level above surface       RDTS 54
    open (34,file = GCMDIR(1:lendir)//"sfc05"//TESYR(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open surface data files for 30 meter level above surface      RDTS 57
    open (35,file = GCMDIR(1:lendir)//"sfc30"//TESYR(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
#else
    !---      Open surface data files for surface level                     RDTS 51
    open (33,file = GCMDIR(1:lendir)//"sfc00"//TESYR(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open surface data files for 5 meter level above surface       RDTS 54
    open (34,file = GCMDIR(1:lendir)//"sfc05"//TESYR(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open surface data files for 30 meter level above surface      RDTS 57
    open (35,file = GCMDIR(1:lendir)//"sfc30"//TESYR(m)//version//".bin", &
          form = sysform,status = "old")
#endif

    !---      Step through all Ls values                                    RDTS 60
    do lsea = 30,360,30
      ls = lsea / 30
      !---      Step through all latitudes                                    RDTS 63
      do lat = -900,900,ilatstep
        xlat = DBLE(lat) / 10.0d0
        i = 1 + (lat+900)/ilatstep
        !---      Step through all boundary layer levels                        RDTS 67
        do k = 1,ntbl
          !---      Step through all longitudes                                   RDTS 69
          do lon = ntlon,1,-1
            !---        Read (binary) tide coefficients for temperature and wind    RDTS 71
            !           components at all boundary layer levels                     RDTS 72
            if (k == 1) then
              read (32+k,End = 1000) &
                   ils, ylat, jlon, TTSA0(k,i,lon,ls,m), TTSA1(k,i,lon,ls,m), &
                   TTSP1(k,i,lon,ls,m), TTSA2(k,i,lon,ls,m), &
                   TTSP2(k,i,lon,ls,m)
              !---         Assume surface wind = 0 (no slip condition)                RDTS 77
              TUSA0(k,i,lon,ls,m) = 0.0d0
              TUSA1(k,i,lon,ls,m) = 0.0d0
              TUSP1(k,i,lon,ls,m) = 0.0d0
              TUSA2(k,i,lon,ls,m) = 0.0d0
              TUSP2(k,i,lon,ls,m) = 0.0d0
              TVSA0(k,i,lon,ls,m) = 0.0d0
              TVSA1(k,i,lon,ls,m) = 0.0d0
              TVSP1(k,i,lon,ls,m) = 0.0d0
              TVSA2(k,i,lon,ls,m) = 0.0d0
              TVSP2(k,i,lon,ls,m) = 0.0d0
            else
              read (32+k,End = 1000) &
                   ils, ylat, jlon, TTSA0(k,i,lon,ls,m), TTSA1(k,i,lon,ls,m), &
                   TTSP1(k,i,lon,ls,m), TTSA2(k,i,lon,ls,m), &
                   TTSP2(k,i,lon,ls,m), TUSA0(k,i,lon,ls,m), &
                   TUSA1(k,i,lon,ls,m), TUSP1(k,i,lon,ls,m), &
                   TUSA2(k,i,lon,ls,m), TUSP2(k,i,lon,ls,m), &
                   TVSA0(k,i,lon,ls,m), TVSA1(k,i,lon,ls,m), &
                   TVSP1(k,i,lon,ls,m), TVSA2(k,i,lon,ls,m), &
                   TVSP2(k,i,lon,ls,m)
            end if
            if (ils /= lsea) stop " Bad surface Ls"
            !---        Reset value of last Ls processed                            RDTS 99
            lastLs = iLs
            if (ylat /= xlat) then
              stop " Bad surface Latitude"
            elseif (jlon /= 9*lon) then
              stop " Bad surface Longitude"
            end if
          end do
        end do
      end do
    end do
    !---      Set all values at Ls=0 to values at Ls=360                    RDTS107
    do k = 1,ntbl
      do i = 1,ntlat
        do lon = 1,ntlon
          TTSA0(k,i,lon,0,m) = TTSA0(k,i,lon,12,m)
          TTSA1(k,i,lon,0,m) = TTSA1(k,i,lon,12,m)
          TTSP1(k,i,lon,0,m) = TTSP1(k,i,lon,12,m)
          TTSA2(k,i,lon,0,m) = TTSA2(k,i,lon,12,m)
          TTSP2(k,i,lon,0,m) = TTSP2(k,i,lon,12,m)
          TUSA0(k,i,lon,0,m) = TUSA0(k,i,lon,12,m)
          TUSA1(k,i,lon,0,m) = TUSA1(k,i,lon,12,m)
          TUSP1(k,i,lon,0,m) = TUSP1(k,i,lon,12,m)
          TUSA2(k,i,lon,0,m) = TUSA2(k,i,lon,12,m)
          TUSP2(k,i,lon,0,m) = TUSP2(k,i,lon,12,m)
          TVSA0(k,i,lon,0,m) = TVSA0(k,i,lon,12,m)
          TVSA1(k,i,lon,0,m) = TVSA1(k,i,lon,12,m)
          TVSP1(k,i,lon,0,m) = TVSP1(k,i,lon,12,m)
          TVSA2(k,i,lon,0,m) = TVSA2(k,i,lon,12,m)
          TVSP2(k,i,lon,0,m) = TVSP2(k,i,lon,12,m)
        end do
        !           Set all values at Lon=0 to values at Lon=360                RDTS127
        do ls = 0,12
          TTSA0(k,i,0,ls,m) = TTSA0(k,i,ntlon,ls,m)
          TTSA1(k,i,0,ls,m) = TTSA1(k,i,ntlon,ls,m)
          TTSP1(k,i,0,ls,m) = TTSP1(k,i,ntlon,ls,m)
          TTSA2(k,i,0,ls,m) = TTSA2(k,i,ntlon,ls,m)
          TTSP2(k,i,0,ls,m) = TTSP2(k,i,ntlon,ls,m)
          TUSA0(k,i,0,ls,m) = TUSA0(k,i,ntlon,ls,m)
          TUSA1(k,i,0,ls,m) = TUSA1(k,i,ntlon,ls,m)
          TUSP1(k,i,0,ls,m) = TUSP1(k,i,ntlon,ls,m)
          TUSA2(k,i,0,ls,m) = TUSA2(k,i,ntlon,ls,m)
          TUSP2(k,i,0,ls,m) = TUSP2(k,i,ntlon,ls,m)
          TVSA0(k,i,0,ls,m) = TVSA0(k,i,ntlon,ls,m)
          TVSA1(k,i,0,ls,m) = TVSA1(k,i,ntlon,ls,m)
          TVSP1(k,i,0,ls,m) = TVSP1(k,i,ntlon,ls,m)
          TVSA2(k,i,0,ls,m) = TVSA2(k,i,ntlon,ls,m)
          TVSP2(k,i,0,ls,m) = TVSP2(k,i,ntlon,ls,m)
        end do
      end do
    end do
    !---      Close input file units                                        RDTS146
    close (33)
    close (34)
    close (35)
    cycle
    !---      Terminate if not all Ls values processed                      RDTS151
    1000 if (lastLs /= 360) stop " Incomplete TES surface GCM data"
  end do
end subroutine RdTESsrf_M05
!-----------------------------------------------------------------------RDTS156
subroutine RdTESmgcm_M05(GCMDIR,version)
  !---    Reads NASA Ames Mars General Circulation Model (MGCM) -5 to 80  RDTM  2
  !       km data (in binary format) for TES years 1& 2 and loads into    RDTM  3
  !       data arrays for common MGCMTES                                  RDTM  4
  !       GCMDIR is directory name where MGCM data resides                RDTM  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ntesy = 2
  integer, parameter :: nhgt = 30
  integer, parameter :: nlat = 25
  integer, parameter :: ntbl = 3
  integer, parameter :: ntf10 = 3
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ihgt,ilatstep,ils,k,khgt,lastls,lat,lendir,ls,lsea,m
  real(kind=PM_REEL) :: xlat,ylat
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to solact: Not Read, Not Written
  !     For TESyr: Maybe Read, Not Written
  !     For solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(ntbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /MGCMTES_M05/ TTZA0,TTZA1,TTZP1,TTZA2,TTZP2,TDZA0,TDZA1,TDZP1,TDZA2, &
                       TDZP2,TPZA0,TUZA0,TUZA1,TUZP1,TUZA2,TUZP2,TVZA0,TVZA1, &
                       TVZP1,TVZA2,TVZP2
  !     For tTZA0 to tVZP2: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /MGCMTES_M05/ ... 
  real(kind=PM_REEL), dimension(nhgt,nlat,0:12,ntesy) :: &
    TDZA0,TDZA1,TDZA2,TDZP1,TDZP2,TPZA0,TTZA0,TTZA1,TTZA2,TTZP1,TTZP2,TUZA0, &
    TUZA1,TUZA2,TUZP1,TUZP2,TVZA0,TVZA1,TVZA2,TVZP1,TVZP2
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDTM 36
  !---    Initialize last Ls value processed to 0                         RDTM 37
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDTM 39
  ilatstep = 1800 / (nlat-1)
  !---    Compute string length for directory name                        RDTM 41
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all dust optical depths                            RDTM 44
  do m = 1,ntesy
#ifdef __GFORTRAN__
    !---      Open MGCM input files for temperature, pressure, and density  RDTM 46
    open (32,file = GCMDIR(1:lendir)//"tpdlo"//TESYR(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
    !---      Open MGCM input files for wind components                     RDTM 49
    open (33,file = GCMDIR(1:lendir)//"uvlo"//TESYR(m)//version//".bin", &
          convert='big_endian', form = sysform,status = "old")
#else
    !---      Open MGCM input files for temperature, pressure, and density  RDTM 46
    open (32,file = GCMDIR(1:lendir)//"tpdlo"//TESYR(m)//version//".bin", &
          form = sysform,status = "old")
    !---      Open MGCM input files for wind components                     RDTM 49
    open (33,file = GCMDIR(1:lendir)//"uvlo"//TESYR(m)//version//".bin", &
          form = sysform,status = "old")
#endif
    !---      Step through all Ls values                                    RDTM 52
    do lsea = 30,360,30
      ls = lsea / 30
      !---      Step through all latitude grid points                         RDTM 55
      do lat = -900,900,ilatstep
        xlat = DBLE(lat) / 10.0d0
        i = 1 + (lat+900)/ilatstep
        !---      Step through all height levels                                RDTM 59
        do k = nhgt,1,-1
          !---        Read (binary) tide coefficients for temperature, pressure,  RDTM 61
          !           and density                                                 RDTM 62
          read (32,End = 1000) &
               ils, ihgt, ylat, TTZA0(k,i,ls,m), TTZA1(k,i,ls,m), &
               TTZP1(k,i,ls,m), TTZA2(k,i,ls,m), TTZP2(k,i,ls,m), &
               TDZA0(k,i,ls,m), TDZA1(k,i,ls,m), TDZP1(k,i,ls,m), &
               TDZA2(k,i,ls,m), TDZP2(k,i,ls,m), TPZA0(k,i,ls,m)
          if (ils /= lsea) stop " Bad tpd Ls"
          khgt = 5 * (k-14)
          if (k < 16) then
            khgt = k - 6
          end if
          if (ihgt /= khgt) stop " Bad tpd Height"
          if (ylat /= xlat) stop " Bad tpd Latitude"
          !---        Read (binary) tide coefficients for wind components         RDTM 73
          read (33,End = 1000) &
               ils, ihgt, ylat, TUZA0(k,i,ls,m), TUZA1(k,i,ls,m), &
               TUZP1(k,i,ls,m), TUZA2(k,i,ls,m), TUZP2(k,i,ls,m), &
               TVZA0(k,i,ls,m), TVZA1(k,i,ls,m), TVZP1(k,i,ls,m), &
               TVZA2(k,i,ls,m), TVZP2(k,i,ls,m)
          if (ils /= lsea) stop " Bad uv Ls"
          !---        Reset value of last Ls processed                            RDTM 79
          lastLs = iLs
          if (ihgt /= khgt) then
            stop " Bad uv Height"
          elseif (ylat /= xlat) then
            stop " Bad uv Latitude"
          end if
        end do
      end do
    end do
    !---      Set data for Ls = 0 to data for Ls = 360                      RDTM 86
    do k = 1,nhgt
      do i = 1,nlat
        TTZA0(k,i,0,m) = TTZA0(k,i,12,m)
        TTZA1(k,i,0,m) = TTZA1(k,i,12,m)
        TTZP1(k,i,0,m) = TTZP1(k,i,12,m)
        TTZA2(k,i,0,m) = TTZA2(k,i,12,m)
        TTZP2(k,i,0,m) = TTZP2(k,i,12,m)
        TDZA0(k,i,0,m) = TDZA0(k,i,12,m)
        TDZA1(k,i,0,m) = TDZA1(k,i,12,m)
        TDZP1(k,i,0,m) = TDZP1(k,i,12,m)
        TDZA2(k,i,0,m) = TDZA2(k,i,12,m)
        TDZP2(k,i,0,m) = TDZP2(k,i,12,m)
        TPZA0(k,i,0,m) = TPZA0(k,i,12,m)
        TUZA0(k,i,0,m) = TUZA0(k,i,12,m)
        TUZA1(k,i,0,m) = TUZA1(k,i,12,m)
        TUZP1(k,i,0,m) = TUZP1(k,i,12,m)
        TUZA2(k,i,0,m) = TUZA2(k,i,12,m)
        TUZP2(k,i,0,m) = TUZP2(k,i,12,m)
        TVZA0(k,i,0,m) = TVZA0(k,i,12,m)
        TVZA1(k,i,0,m) = TVZA1(k,i,12,m)
        TVZP1(k,i,0,m) = TVZP1(k,i,12,m)
        TVZA2(k,i,0,m) = TVZA2(k,i,12,m)
        TVZP2(k,i,0,m) = TVZP2(k,i,12,m)
      end do
    end do
    !---    Close input files to re-use same unit number for next dust      RDTM112
    !       value                                                           RDTM113
    close (32)
    close (33)
    cycle
    !---    Terminate if not all Ls values have been processed              RDTM117
    1000 if (lastLs /= 360) stop " Incomplete -5 to 80 km MGCM data"
  end do
end subroutine RdTESmgcm_M05
!-----------------------------------------------------------------------RDTM122
subroutine RdTEStgcm_M05(GCMDIR,version)
  !---    Reads University of Michigan Mars Thermospheric General Circu-  RDTT  2
  !       lation Model (MTGCM) data (in binary format) for TES years 1&2  RDTT  3
  !       and loads into dataarrays for common TGCMTES                    RDTT  4
  !       GCMDIR is directory name where MTGCM data resides               RDTT  5
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: ntesy = 2
  integer, parameter :: nhgtt = 33
  integer, parameter :: nlatt = 36
  integer, parameter :: ntf10 = 3
  integer, parameter :: ntbl = 3
  integer, parameter :: nf10 = 2
  integer, parameter :: ndust = 3
  character(LEN=11), parameter :: sysform = "unformatted"
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: GCMDIR
  character, intent(in) :: version
  ! 
  !.. Local Scalars .. 
  integer :: i,ihgt,ilat1,ilat2,ilatstep,ils,iyr,k,lastls,lat,lendir,ls,lsea, &
             m,n,nhgttop
  real(kind=PM_REEL) :: xlat,ylat
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust to solact: Not Read, Not Written
  !     For TESyr to solTES: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(ntbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /TGCMTES_M05/ TTTA0,TTTA1,TTTP1,TTTA2,TTTP2,TPTA0,TPTA1,TPTP1,TPTA2, &
                       TPTP2,TDTA0,TDTA1,TDTP1,TDTA2,TDTP2,TUTA0,TUTA1,TUTP1, &
                       TUTA2,TUTP2,TVTA0,TVTA1,TVTP1,TVTA2,TVTP2,TZFA0,TZFA1, &
                       TZFP1,TZFA2,TZFP2,IZTOP
  !     For tTTA0 to iztop: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /TGCMTES_M05/ ... 
  real(kind=PM_REEL), dimension(nhgtt,nlatt,0:12,ntesy,ntf10) :: &
    TDTA0,TDTA1,TDTA2,TDTP1,TDTP2,TPTA0,TPTA1,TPTA2,TPTP1,TPTP2,TTTA0,TTTA1, &
    TTTA2,TTTP1,TTTP2,TUTA0,TUTA1,TUTA2,TUTP1,TUTP2,TVTA0,TVTA1,TVTA2,TVTP1, &
    TVTP2
  real(kind=PM_REEL), dimension(nlatt,0:12,ntesy,ntf10) :: &
    TZFA0,TZFA1,TZFA2,TZFP1,TZFP2
  integer, dimension(0:12,ntesy,ntf10) :: IZTOP
  ! 
  ! ... Executable Statements ...
  ! 
  !                                                                       RDTT 44
  !---    Initialize last Ls value processed to 0                         RDTT 45
  lastls = 0
  !---    Set ilatstep = latitude step size x 10                          RDTT 47
  ilatstep = 1800 / nlatt
  !---    Set initial and final latitudes (x 10) for stepping             RDTT 49
  ilat1 = -900 + ilatstep/2
  ilat2 = 900 - ilatstep/2
  !---    Compute string length for directory name                        RDTT 52
  lendir = index(GCMDIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !---    Step through all solar activity levels                          RDTT 55
  do n = 1,ntf10
    open (34,file = GCMDIR(1:lendir)//"zfTES"//SOLTES(n)//version//".txt", &
          status = "old")
    !---    Step through all dust optical depths                            RDTT 59
    do m = 1,ntesy
#ifdef __GFORTRAN__
      !---      Open MTGCM data files for temperature, pressure, and density  RDTT 61
      open (32, &
            file = GCMDIR(1:lendir)//"tpd"//SOLTES(n)//TESYR(m)//version// &
                   ".bin", convert='big_endian', form = sysform,status = "old")
      !---      Open MTGCM data files for wind components                     RDTT 64
      open (33, &
            file = GCMDIR(1:lendir)//"uv"//SOLTES(n)//TESYR(m)//version// &
                   ".bin", convert='big_endian', form = sysform,status = "old")
#else
      !---      Open MTGCM data files for temperature, pressure, and density  RDTT 61
      open (32, &
            file = GCMDIR(1:lendir)//"tpd"//SOLTES(n)//TESYR(m)//version// &
                   ".bin",form = sysform,status = "old")
      !---      Open MTGCM data files for wind components                     RDTT 64
      open (33, &
            file = GCMDIR(1:lendir)//"uv"//SOLTES(n)//TESYR(m)//version// &
                   ".bin",form = sysform,status = "old")
#endif
      !---      Step through all Ls values                                    RDTT 67
      do lsea = 30,360,30
        ls = lsea / 30
        !---      Step through all latitudes                                    RDTT 70
        do lat = ilat1,ilat2,ilatstep
          xlat = DBLE(lat) / 10.0d0
          i = 1 + (lat-ilat1)/ilatstep
          !---        Read tide coefficient data for ZF=height of 1.26 nbar level RDTT 74
          read (34,*,End = 1000) &
               iyr, ils, ylat, TZFA0(i,ls,m,n), TZFA1(i,ls,m,n), &
               TZFP1(i,ls,m,n), TZFA2(i,ls,m,n), TZFP2(i,ls,m,n), nhgttop
          if (nhgttop /= 240) goto 1100
          if (iyr /= m) stop " Bad ZF year value"
          if (ils /= lsea) stop " Bad ZF Ls"
          if (ylat /= xlat) stop " Bad ZF Latitude"
          nhgttop = (nhgttop-75) / 5
          IZTOP(ls,m,n) = nhgttop
          !---      Step through all heights                                      RDTT 83
          do k = 1,nhgttop
            !---        Read (binary) tide coefficients for temperature, pressure,  RDTT 85
            !           and density                                                 RDTT 86
            read (32,End = 1000) &
                 ils, ihgt, ylat, TTTA0(k,i,ls,m,n), TTTA1(k,i,ls,m,n), &
                 TTTP1(k,i,ls,m,n), TTTA2(k,i,ls,m,n), TTTP2(k,i,ls,m,n), &
                 TPTA0(k,i,ls,m,n), TPTA1(k,i,ls,m,n), TPTP1(k,i,ls,m,n), &
                 TPTA2(k,i,ls,m,n), TPTP2(k,i,ls,m,n), TDTA0(k,i,ls,m,n), &
                 TDTA1(k,i,ls,m,n), TDTP1(k,i,ls,m,n), TDTA2(k,i,ls,m,n), &
                 TDTP2(k,i,ls,m,n)
            if (ils /= lsea) stop " Bad tpd Ls"
            if (ihgt /= 80+5*(k-1)) stop " Bad tpd Height"
            if (ylat /= xlat) stop " Bad tpd Latitude"
            !---        Read (binary) tide coefficients for wind components         RDTT 96
            read (33,End = 1000) &
                 ils, ihgt, ylat, TUTA0(k,i,ls,m,n), TUTA1(k,i,ls,m,n), &
                 TUTP1(k,i,ls,m,n), TUTA2(k,i,ls,m,n), TUTP2(k,i,ls,m,n), &
                 TVTA0(k,i,ls,m,n), TVTA1(k,i,ls,m,n), TVTP1(k,i,ls,m,n), &
                 TVTA2(k,i,ls,m,n), TVTP2(k,i,ls,m,n)
            if (ils /= lsea) stop " Bad uv Ls"
            !---        Reset last Ls value processed                               RDTT102
            lastLs = iLs
            if (ihgt /= 80+5*(k-1)) then
              stop " Bad uv Height"
            elseif (ylat /= xlat) then
              stop " Bad uv Latitude"
            end if
          end do
        end do
      !---      Step through all latitudes                                    RDTT 70
      end do
      !---      Set all values at Ls=0 to values at Ls=360                    RDTT108
      IZTOP(0,m,n) = IZTOP(12,m,n)
      do i = 1,nlatt
        TZFA0(i,0,m,n) = TZFA0(i,12,m,n)
        TZFA1(i,0,m,n) = TZFA1(i,12,m,n)
        TZFP1(i,0,m,n) = TZFP1(i,12,m,n)
        TZFA2(i,0,m,n) = TZFA2(i,12,m,n)
        TZFP2(i,0,m,n) = TZFP2(i,12,m,n)
        do k = 1,IZTOP(12,m,n)
          TTTA0(k,i,0,m,n) = TTTA0(k,i,12,m,n)
          TTTA1(k,i,0,m,n) = TTTA1(k,i,12,m,n)
          TTTP1(k,i,0,m,n) = TTTP1(k,i,12,m,n)
          TTTA2(k,i,0,m,n) = TTTA2(k,i,12,m,n)
          TTTP2(k,i,0,m,n) = TTTP2(k,i,12,m,n)
          TPTA0(k,i,0,m,n) = TPTA0(k,i,12,m,n)
          TPTA1(k,i,0,m,n) = TPTA1(k,i,12,m,n)
          TPTP1(k,i,0,m,n) = TPTP1(k,i,12,m,n)
          TPTA2(k,i,0,m,n) = TPTA2(k,i,12,m,n)
          TPTP2(k,i,0,m,n) = TPTP2(k,i,12,m,n)
          TDTA0(k,i,0,m,n) = TDTA0(k,i,12,m,n)
          TDTA1(k,i,0,m,n) = TDTA1(k,i,12,m,n)
          TDTP1(k,i,0,m,n) = TDTP1(k,i,12,m,n)
          TDTA2(k,i,0,m,n) = TDTA2(k,i,12,m,n)
          TDTP2(k,i,0,m,n) = TDTP2(k,i,12,m,n)
          TUTA0(k,i,0,m,n) = TUTA0(k,i,12,m,n)
          TUTA1(k,i,0,m,n) = TUTA1(k,i,12,m,n)
          TUTP1(k,i,0,m,n) = TUTP1(k,i,12,m,n)
          TUTA2(k,i,0,m,n) = TUTA2(k,i,12,m,n)
          TUTP2(k,i,0,m,n) = TUTP2(k,i,12,m,n)
          TVTA0(k,i,0,m,n) = TVTA0(k,i,12,m,n)
          TVTA1(k,i,0,m,n) = TVTA1(k,i,12,m,n)
          TVTP1(k,i,0,m,n) = TVTP1(k,i,12,m,n)
          TVTA2(k,i,0,m,n) = TVTA2(k,i,12,m,n)
          TVTP2(k,i,0,m,n) = TVTP2(k,i,12,m,n)
        end do
      end do
      !---    Close input file unit numbers                                   RDTT143
      close (32)
      close (33)
      cycle
      !---    Terminate if not all Ls values processed                        RDTT147
      1000 if (lastLs /= 360) stop " Incomplete 80-240  km MTGCM data"
    end do
    close (34)
  end do
  return
  1100 write (*,*) "iyr,ils,ylat,nhgttop=", iyr, ils, ylat, nhgttop
  stop " Bad nhgttop value"
end subroutine RdTEStgcm_M05
!-----------------------------------------------------------------------RDTT154
subroutine TESGterp_M05(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,VMGCM,TempDay, &
                        PresDay,DensDay,UwndDay,VwndDay,Tempmax,Tempmin, &
                        Densmax,Densmin,mtesy,idaydata)
  !---    Interpolates Ames Mars General Circulation Model (MGCM) data    TGTP  4
  !       to a given latitude, time of year (Ls), for a given TES year,   TGTP  5
  !       height index (khgt) and time of day (time).                     TGTP  6
  !       Some input data is provided by the Common "Interp".             TGTP  7
  !---    Set parameter values for number of heights, latitudes           TGTP  8
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nthgt = 30
  integer, parameter :: ntlat = 25
  integer, parameter :: ntesy = 2
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgt,mtesy
  real(kind=PM_REEL), intent(in) :: time
  real(kind=PM_REEL), intent(out) :: &
    DMGCM,DensDay,Densmax,Densmin,PMGCM,PresDay,TMGCM,TempDay,Tempmax,Tempmin, &
    UMGCM,UwndDay,VMGCM,VwndDay
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,l
  real(kind=PM_REEL) :: A1,A1d,A1t,A2,A2d,A2t,D0,Dtime,P0,P1,P1d,P1t,P2,P2d,P2t, &
                      Ptime,RMGCM,T0,Ttime,U0,V0,polefac,upolefac,xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2) :: &
    DM,Dday,Dmax,Dmin,R0,TM,Tday,Tmax,Tmin,UM,Uday,VM,Vday
  ! 
  ! DM 1359 remove external : private
  !.. External Calls .. 
 ! external TwoD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: TideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic real
  ! 
  !.. Common Blocks .. 
  common /MGCMTES_M05/ TTZA0,TTZA1,TTZP1,TTZA2,TTZP2,TDZA0,TDZA1,TDZP1,TDZA2, &
                       TDZP2,TPZA0,TUZA0,TUZA1,TUZP1,TUZA2,TUZP2,TVZA0,TVZA1, &
                       TVZP1,TVZA2,TVZP2
  !     For tTZA0 to tVZP2: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /MGCMTES_M05/ ... 
  real(kind=PM_REEL), dimension(nthgt,ntlat,0:12,ntesy) :: &
    TDZA0,TDZA1,TDZA2,TDZP1,TDZP2,TPZA0,TTZA0,TTZA1,TTZA2,TTZP1,TTZP2,TUZA0, &
    TUZA1,TUZA2,TUZP1,TUZP2,TVZA0,TVZA1,TVZA2,TVZP1,TVZP2
  common /TESTERP_M05/ DLAT,DLON,DLS,DLATW,DLONW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                       ILAT,JLON,LS,K1ST,ILATW,JLONW,ILATT,MF10
  !     For dlat: Read, Not Written
  !     For dlon: Not Read, Not Written
  !     For dls to dlatw: Read, Not Written
  !     For dlonw to df10: Not Read, Not Written
  !     For wpolefac: Maybe Read, Not Written
  !     For tpolefac: Not Read, Not Written
  !     For ilat: Read, Not Written
  !     For jlon: Not Read, Not Written
  !     For ls: Read, Not Written
  !     For k1st: Not Read, Not Written
  !     For ilatw: Read, Not Written
  !     For jlonw to mf10: Not Read, Not Written
  ! 
  !... Variables in Common Block /TESterp_M05/ ... 
  real(kind=PM_REEL) :: DF10,DLAT,DLATT,DLATW,DLON,DLONW,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,JLONW,K1ST,LS,MF10
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MGCM values at corners of a 2-dimensional cube in     TGTP 30
  !       latitude-Ls space, at the given height index (khgt), and        TGTP 31
  !       time of day (time)                                              TGTP 32
  do i = 1,2
    polefac = 1.0d0
    upolefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == ntlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    if (ILATW == 2) then
      if (i == 1) then
        upolefac = WPOLEFAC
      end if
    elseif (ILATW==ntlat-1 .and. i==2) then
      upolefac = WPOLEFAC
    end if
    do l = 1,2
      !---      Daily mean temperature                                        TGTP 47
      T0 = TTZA0(khgt,ILAT+i-1,LS+l-1,mtesy)
      Tday(i,l) = T0
      !---      Temperature tide amplitudes and phases                        TGTP 50
      A1t = TTZA1(khgt,ILAT+i-1,LS+l-1,mtesy) * polefac
      P1t = TTZP1(khgt,ILAT+i-1,LS+l-1,mtesy)
      A2t = TTZA2(khgt,ILAT+i-1,LS+l-1,mtesy) * polefac
      P2t = TTZP2(khgt,ILAT+i-1,LS+l-1,mtesy)
      !---      Temperature at corners of 2-D cube                            TGTP 55
      TM(i,l) = TideX_M05(T0,A1t,P1t,A2t,P2t,time)
      !---      Daily mean density                                            TGTP 57
      D0 = TDZA0(khgt,ILAT+i-1,LS+l-1,mtesy)
      Dday(i,l) = D0
      !---      Density tide amplitudes and phases                            TGTP 60
      A1d = TDZA1(khgt,ILAT+i-1,LS+l-1,mtesy) * polefac
      P1d = TDZP1(khgt,ILAT+i-1,LS+l-1,mtesy)
      A2d = TDZA2(khgt,ILAT+i-1,LS+l-1,mtesy) * polefac
      P2d = TDZP2(khgt,ILAT+i-1,LS+l-1,mtesy)
      !---      Density at corners of 2-D cube                                TGTP 65
      DM(i,l) = TideY_M05(D0,A1d,P1d,A2d,P2d,time)
      !---      Daily average pressure P0                                     TGTP 67
      P0 = TPZA0(khgt,ILAT+i-1,LS+l-1,mtesy)
      !---      Gas constant from pressure, density and temperature           TGTP 69
      R0(i,l) = 190.0d0
      if (T0/=0.0d0 .and. D0/=0.0d0) then
        R0(i,l) = P0 / (T0*D0)
      end if
      !---      Max and Min temperature and density at corners of 2-D cube    TGTP 72
      Tmax(i,l) = -9999.0d0
      Tmin(i,l) = 9999.0d0
      Dmax(i,l) = -9999.0d0
      Dmin(i,l) = 9999.0d0
      if (idaydata > 0) then
        do itime = 0,23
          xtime = DBLE(real(itime))
          Ttime = TideX_M05(T0,A1t,P1t,A2t,P2t,xtime)
          Dtime = TideY_M05(D0,A1d,P1d,A2d,P2d,xtime)
          Ptime = Dtime * R0(i,l) * Ttime
          if (Ttime > Tmax(i,l)) then
            Tmax(i,l) = Ttime
          end if
          if (Ttime < Tmin(i,l)) then
            Tmin(i,l) = Ttime
          end if
          if (Dtime > Dmax(i,l)) then
            Dmax(i,l) = Dtime
          end if
          if (Dtime < Dmin(i,l)) then
            Dmin(i,l) = Dtime
          end if
        end do
      end if
      !---      Daily mean EW wind                                            TGTP 89
      U0 = TUZA0(khgt,ILAT+i-1,LS+l-1,mtesy)
      Uday(i,l) = U0
      !---      EW wind tide amplitudes and phases                            TGTP 92
      A1 = TUZA1(khgt,ILAT+i-1,LS+l-1,mtesy) * upolefac
      P1 = TUZP1(khgt,ILAT+i-1,LS+l-1,mtesy)
      A2 = TUZA2(khgt,ILAT+i-1,LS+l-1,mtesy) * upolefac
      P2 = TUZP2(khgt,ILAT+i-1,LS+l-1,mtesy)
      !---      EW wind at corners of 2-D cube                                TGTP 97
      UM(i,l) = TideX_M05(U0,A1,P1,A2,P2,time)
      !---      Daily mean NS wind                                            TGTP 99
      V0 = TVZA0(khgt,ILATW+i-1,LS+l-1,mtesy)
      Vday(i,l) = V0
      !---      NS wind tide amplitudes and phases                            TGTP102
      A1 = TVZA1(khgt,ILATW+i-1,LS+l-1,mtesy) * upolefac
      P1 = TVZP1(khgt,ILATW+i-1,LS+l-1,mtesy)
      A2 = TVZA2(khgt,ILATW+i-1,LS+l-1,mtesy) * upolefac
      P2 = TVZP2(khgt,ILATW+i-1,LS+l-1,mtesy)
      !---      NS wind at corners of 2-D cube                                TGTP107
      VM(i,l) = TideX_M05(V0,A1,P1,A2,P2,time)
    end do
  end do
  !---    Use 2-D interpolation to get temperature, pressure, gas         TGTP111
  !       constant, EW wind, and NS wind at given latitude, and Ls        TGTP112
  call TwoD_M05(DLAT,DLS,TM,TMGCM)
  call TwoD_M05(DLAT,DLS,Tday,TempDay)
  call TwoD_M05(DLAT,DLS,Tmax,Tempmax)
  call TwoD_M05(DLAT,DLS,Tmin,Tempmin)
  call TwoD_M05(DLAT,DLS,Dmax,Densmax)
  call TwoD_M05(DLAT,DLS,Dmin,Densmin)
  call TwoD_M05(DLAT,DLS,DM,DMGCM)
  call TwoD_M05(DLAT,DLS,Dday,DensDay)
  call TwoD_M05(DLAT,DLS,R0,RMGCM)
  call TwoD_M05(DLAT,DLS,UM,UMGCM)
  call TwoD_M05(DLAT,DLS,Uday,UwndDay)
  call TwoD_M05(DLATW,DLS,VM,VMGCM)
  call TwoD_M05(DLATW,DLS,Vday,VwndDay)
  !---    Compute pressure from temperature, density, and gas constant    TGTP126
  PMGCM = DMGCM * RMGCM * TMGCM
  PresDay = DensDay * RMGCM * TempDay
end subroutine TESGterp_M05
!-----------------------------------------------------------------------TGTP131
subroutine TESsrftrp_M05(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,VMGCM,Hpres,Hdens, &
                         ctopohgt,TempDay,PresDay,DensDay,UwndDay,VwndDay, &
                         Hpres0,Tempmax,Tempmin,Densmax,Densmin,Tat5m,mtesy, &
                         idaydata)
  !---    Interpolates Ames Mars General Circulation Model (MGCM) surface TSTP  5
  !       data to a given latitude, longitude, time of year (Ls), for a   TSTP  6
  !       given TES year (mtesy), height index (khgt) and time of day     TSTP  7
  !       (time).  Some input data is provided by the Common "Interp".    TSTP  8
  !---    Set parameter values for number of heights, boundary layer      TSTP  9
  !       levels, latitudes, longitudes                                   TSTP 10
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nthgt = 30
  integer, parameter :: ntbl = 3
  integer, parameter :: ntlat = 25
  integer, parameter :: ntlon = 40
  integer, parameter :: ntesy = 2
  integer, parameter :: ntf10 = 3
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgt,mtesy
  real(kind=PM_REEL), intent(in) :: ctopohgt,time
  real(kind=PM_REEL), intent(inout) :: Tat5m
  real(kind=PM_REEL), intent(out) :: &
    DMGCM,DensDay,Densmax,Densmin,Hdens,Hpres,Hpres0,PMGCM,PresDay,TMGCM, &
    TempDay,Tempmax,Tempmin,UMGCM,UwndDay,VMGCM,VwndDay
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,j,k1h,l
  real(kind=PM_REEL) :: A1,A1t,A2,A2t,D0,D1max,D1min,D1st,DZk1h,DZk1h1,Dtime, &
                      Dzero,Hdens0,Hdmax,Hdmin,P0,P1,P1st,P1t,P2,P2t,PZk1h, &
                      PZk1h1,Pzero,Rzero,T0,T1st,T1time,TSzero,Tbar,Tbar0, &
                      Tmax1,Tmin1,Ttime,Tzero,U0,V0,Z1st,dz1h,height
  real(kind=PM_REEL) :: polefac,upolefac,xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2) :: &
    DZ0,DZ1,DZh,DZh1,Dmax,Dmin,PZ0,PZh,PZh1,RZ0,T1max,T1min,TZ0,TZ1
  real(kind=PM_REEL), dimension(2,2,2) :: TM,TS0,Tday,Tmax,Tmin,UM,Uday,VM,Vday
  ! 
  ! DM1359 remove external : private
  !.. External Calls .. 
  !external ThreeD_M05, TwoD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Zlogr_M05
  !real(kind=PM_REEL), external :: TideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic exp, real
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust: Not Read, Not Written
  !     For dzbl: Maybe Read, Not Written
  !     For zwsfc to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(ntbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /MGCMTES_M05/ TTZA0,TTZA1,TTZP1,TTZA2,TTZP2,TDZA0,TDZA1,TDZP1,TDZA2, &
                       TDZP2,TPZA0,TUZA0,TUZA1,TUZP1,TUZA2,TUZP2,TVZA0,TVZA1, &
                       TVZP1,TVZA2,TVZP2
  !     For tTZA0 to tPZA0: Maybe Read, Not Written
  !     For tUZA0 to tVZP2: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMTES_M05/ ... 
  real(kind=PM_REEL), dimension(nthgt,ntlat,0:12,ntesy) :: &
    TDZA0,TDZA1,TDZA2,TDZP1,TDZP2,TPZA0,TTZA0,TTZA1,TTZA2,TTZP1,TTZP2,TUZA0, &
    TUZA1,TUZA2,TUZP1,TUZP2,TVZA0,TVZA1,TVZA2,TVZP1,TVZP2
  common /SURFTES_M05/ TTSA0,TTSA1,TTSP1,TTSA2,TTSP2,TUSA0,TUSA1,TUSP1,TUSA2, &
                       TUSP2,TVSA0,TVSA1,TVSP1,TVSA2,TVSP2
  !     For tTSA0 to tVSP2: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /surfTES_M05/ ... 
  real(kind=PM_REEL), dimension(ntbl,ntlat,0:ntlon,0:12,ntesy) :: &
    TTSA0,TTSA1,TTSA2,TTSP1,TTSP2,TUSA0,TUSA1,TUSA2,TUSP1,TUSP2,TVSA0,TVSA1, &
    TVSA2,TVSP1,TVSP2
  common /TESTERP_M05/ DLAT,DLON,DLS,DLATW,DLONW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                       ILAT,JLON,LS,K1ST,ILATW,JLONW,ILATT,MF10
  !     For dlat to dlonw: Read, Not Written
  !     For dlatt to df10: Not Read, Not Written
  !     For wpolefac: Maybe Read, Not Written
  !     For tpolefac: Not Read, Not Written
  !     For ilat to jlonw: Read, Not Written
  !     For ilatt to mf10: Not Read, Not Written
  ! 
  !... Variables in Common Block /TESterp_M05/ ... 
  real(kind=PM_REEL) :: DF10,DLAT,DLATT,DLATW,DLON,DLONW,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,JLONW,K1ST,LS,MF10
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MGCM surface values at corners of a 3-dimensional     TSTP 60
  !       cube in latitude-longitude-Ls space, at a given height          TSTP 61
  !       index (khgt) and time of day (time)                             TSTP 62
  
  do i = 1,2
    polefac = 1.0d0
    upolefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == ntlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    if (ILATW == 2) then
      if (i == 1) then
        upolefac = WPOLEFAC
      end if
    elseif (ILATW==ntlat-1 .and. i==2) then
      upolefac = WPOLEFAC
    end if
    do j = 1,2
      do l = 1,2
        !---      Daily mean temperature at level khgt                          TSTP 78
        T0 = TTSA0(khgt,ILAT+i-1,JLON+j-1,LS+l-1,mtesy)
        Tday(i,j,l) = T0
        !---      Temperature tide amplitudes and phases                        TSTP 81
        A1t = TTSA1(khgt,ILAT+i-1,JLON+j-1,LS+l-1,mtesy) * polefac
        P1t = TTSP1(khgt,ILAT+i-1,JLON+j-1,LS+l-1,mtesy)
        A2t = TTSA2(khgt,ILAT+i-1,JLON+j-1,LS+l-1,mtesy) * polefac
        P2t = TTSP2(khgt,ILAT+i-1,JLON+j-1,LS+l-1,mtesy)
        !---      Temperature at corners of 3-D cube                            TSTP 86
        TM(i,j,l) = TideX_M05(T0,A1t,P1t,A2t,P2t,time)
        !---      Daily mean temperature at surface                             TSTP 88
        TS0(i,j,l) = TTSA0(1,ILAT+i-1,JLON+j-1,LS+l-1,mtesy)
        !---      Max and Min temperatures at corners of 3-D cube               TSTP 90
        Tmax(i,j,l) = -9999.0d0
        Tmin(i,j,l) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            Ttime = TideX_M05(T0,A1t,P1t,A2t,P2t,xtime)
            if (Ttime > Tmax(i,j,l)) then
              Tmax(i,j,l) = Ttime
            end if
            if (Ttime < Tmin(i,j,l)) then
              Tmin(i,j,l) = Ttime
            end if
          end do
        end if
        !---      Daily mean EW wind at level khgt                              TSTP101
        U0 = TUSA0(khgt,ILAT+i-1,JLONW+j-1,LS+l-1,mtesy)
        Uday(i,j,l) = U0
        !---      EW wind tide coefficient amplitudes and phases                TSTP104
        A1 = TUSA1(khgt,ILAT+i-1,JLONW+j-1,LS+l-1,mtesy) * upolefac
        P1 = TUSP1(khgt,ILAT+i-1,JLONW+j-1,LS+l-1,mtesy)
        A2 = TUSA2(khgt,ILAT+i-1,JLONW+j-1,LS+l-1,mtesy) * upolefac
        P2 = TUSP2(khgt,ILAT+i-1,JLONW+j-1,LS+l-1,mtesy)
        !---      EW wind at corners of 3-D cube                                TSTP109
        UM(i,j,l) = TideX_M05(U0,A1,P1,A2,P2,time)
        !---      Daily mean NS wind at level khgt                              TSTP111
        V0 = TVSA0(khgt,ILATW+i-1,JLON+j-1,LS+l-1,mtesy)
        Vday(i,j,l) = V0
        !---      NS wind coefficient amplitudes and phases                     TSTP114
        A1 = TVSA1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,mtesy) * upolefac
        P1 = TVSP1(khgt,ILATW+i-1,JLON+j-1,LS+l-1,mtesy)
        A2 = TVSA2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,mtesy) * upolefac
        P2 = TVSP2(khgt,ILATW+i-1,JLON+j-1,LS+l-1,mtesy)
        !---      NS wind at corners of 3-D cube                                TSTP119
        VM(i,j,l) = TideX_M05(V0,A1,P1,A2,P2,time)
      end do
    end do
  end do
  !---    Use 3-D interpolation to get temperature, EW wind, NS wind,     TSTP124
  !       and daily mean surface temperature at given latitude,           TSTP125
  !       longitude, Ls                                                   TSTP126
  call ThreeD_M05(DLAT,DLON,DLS,TM,TMGCM)
  call ThreeD_M05(DLAT,DLONW,DLS,UM,UMGCM)
  call ThreeD_M05(DLATW,DLON,DLS,VM,VMGCM)
  call ThreeD_M05(DLAT,DLON,DLS,TS0,TSzero)
  call ThreeD_M05(DLAT,DLON,DLS,Tday,TempDay)
  call ThreeD_M05(DLAT,DLON,DLS,Tmax,Tempmax)
  call ThreeD_M05(DLAT,DLON,DLS,Tmin,Tempmin)
  call ThreeD_M05(DLAT,DLONW,DLS,Uday,UwndDay)
  call ThreeD_M05(DLATW,DLON,DLS,Vday,VwndDay)
  !---    k1h = height index just below k1st                              TSTP136
  k1h = K1ST - 1
  if (k1h < 1) then
    k1h = 1
  end if
  !---    Establish MGCM values at height levels k1h, k1st and corners of TSTP139
  !       a 2-dimensional cube in latitude-Ls, at given time of day       TSTP140
  !       (time)                                                          TSTP141
  do i = 1,2
    polefac = 1.0d0
    if (ILAT == 1) then
      polefac = DBLE(i) - 1.0d0
    elseif (ILAT == ntlat-1) then
      polefac = 2.0d0 - DBLE(i)
    end if
    do l = 1,2
      !---      Daily average pressure and density at level k1h               TSTP150
      PZh(i,l) = TPZA0(k1h,ILAT+i-1,LS+l-1,mtesy)
      DZh(i,l) = TDZA0(k1h,ILAT+i-1,LS+l-1,mtesy)
      PZh1(i,l) = TPZA0(k1h+1,ILAT+i-1,LS+l-1,mtesy)
      DZh1(i,l) = TDZA0(k1h+1,ILAT+i-1,LS+l-1,mtesy)
      !---      Density tide coefficient amplitudes and phases                TSTP155
      D0 = TDZA0(K1ST,ILAT+i-1,LS+l-1,mtesy)
      A1 = TDZA1(K1ST,ILAT+i-1,LS+l-1,mtesy) * polefac
      P1 = TDZP1(K1ST,ILAT+i-1,LS+l-1,mtesy)
      A2 = TDZA2(K1ST,ILAT+i-1,LS+l-1,mtesy) * polefac
      P2 = TDZP2(K1ST,ILAT+i-1,LS+l-1,mtesy)
      !---      Density values at corners of 2-D cube                         TSTP161
      DZ1(i,l) = TideY_M05(D0,A1,P1,A2,P2,time)
      !---      Daily average density at level k1st                           TSTP163
      DZ0(i,l) = D0
      !---      Level k1st density at corners of 2-D cube                     TSTP165
      Dmax(i,l) = -9999.0d0
      Dmin(i,l) = 9999.0d0
      if (idaydata > 0) then
        do itime = 0,23
          xtime = DBLE(real(itime))
          Dtime = TideY_M05(D0,A1,P1,A2,P2,xtime)
          if (Dtime > Dmax(i,l)) then
            Dmax(i,l) = Dtime
          end if
          if (Dtime < Dmin(i,l)) then
            Dmin(i,l) = Dtime
          end if
        end do
      end if
      !---      Temperature tide coefficient amplitudes and phases            TSTP176
      T0 = TTZA0(K1ST,ILAT+i-1,LS+l-1,mtesy)
      A1 = TTZA1(K1ST,ILAT+i-1,LS+l-1,mtesy) * polefac
      P1 = TTZP1(K1ST,ILAT+i-1,LS+l-1,mtesy)
      A2 = TTZA2(K1ST,ILAT+i-1,LS+l-1,mtesy) * polefac
      P2 = TTZP2(K1ST,ILAT+i-1,LS+l-1,mtesy)
      !---      temperature values at corners of 2-D cube                     TSTP182
      TZ1(i,l) = TideX_M05(T0,A1,P1,A2,P2,time)
      !---      Level k1st Temperature at corners of 2-D cube                 TSTP184
      T1max(i,l) = -9999.0d0
      T1min(i,l) = 9999.0d0
      if (idaydata > 0) then
        do itime = 0,23
          xtime = DBLE(real(itime))
          T1time = TideX_M05(T0,A1,P1,A2,P2,xtime)
          if (T1time > T1max(i,l)) then
            T1max(i,l) = T1time
          end if
          if (T1time < T1min(i,l)) then
            T1min(i,l) = T1time
          end if
        end do
      end if
      !---      Daily average temperature at level k1st                       TSTP195
      TZ0(i,l) = T0
      !---      Daily average pressure at level k1st                          TSTP197
      P0 = TPZA0(K1ST,ILAT+i-1,LS+l-1,mtesy)
      PZ0(i,l) = P0
      !---      Gas constant from pressure, density, and temperature          TSTP200
      RZ0(i,l) = 190.0d0
      if (T0/=0.0d0 .and. D0/=0.0d0) then
        RZ0(i,l) = P0 / (T0*D0)
      end if
    end do
  end do
  !---    Do 2-D interpolation on pressure                                TSTP205
  call TwoD_M05(DLAT,DLS,PZh,PZk1h)
  call TwoD_M05(DLAT,DLS,PZh1,PZk1h1)
  !---    Daily average pressure scale height                             TSTP208
  dz1h = 1.0d0
  if (K1ST >= 16) then
    dz1h = 5.0d0
  end if
  Hpres0 = dz1h / Zlogr_M05(PZk1h,PZk1h1,"TSTP-01")
  !---    Do 2-D interpolation on density                                 TSTP212
  call TwoD_M05(DLAT,DLS,DZh,DZk1h)
  call TwoD_M05(DLAT,DLS,DZh1,DZk1h1)
  !---    Daily average density scale height                              TSTP215
  Hdens0 = dz1h / Zlogr_M05(DZk1h,DZk1h1,"TSTP-02")
  !---    Do 2-D interpolation on daily mean temperature                  TSTP217
  call TwoD_M05(DLAT,DLS,TZ0,Tzero)
  call TwoD_M05(DLAT,DLS,DZ0,Dzero)
  call TwoD_M05(DLAT,DLS,PZ0,Pzero)
  !---    Daily average layer mean temperature                            TSTP221
  Tbar0 = (Tzero+TSzero) / 2.0d0
  !---    Do 2-D interpolation on gas constant                            TSTP223
  call TwoD_M05(DLAT,DLS,RZ0,Rzero)
  !---    Do 2-D interpolation on temperature and density                 TSTP225
  call TwoD_M05(DLAT,DLS,TZ1,T1st)
  call TwoD_M05(DLAT,DLS,DZ1,D1st)
  !---    Do 2-D interpolation on max,min pressure at level k1st          TSTP228
  call TwoD_M05(DLAT,DLS,Dmax,D1max)
  call TwoD_M05(DLAT,DLS,Dmin,D1min)
  !---    Do 2-D interpolation on max,min temperature at level k1st       TSTP231
  call TwoD_M05(DLAT,DLS,T1max,Tmax1)
  call TwoD_M05(DLAT,DLS,T1min,Tmin1)
  !---    Pressure from gas law                                           TSTP234
  P1st = D1st * Rzero * T1st
  !---    Layer mean temperature at current time                          TSTP236
  if (khgt==2 .and. Tat5m<=0.0d0) then
    Tat5m = TMGCM
  end if
  Tbar = (T1st+Tat5m) / 2.0d0
  !---    Pressure scale height and density scale height at current time  TSTP239
  Hpres = Hpres0 * Tbar / Tbar0
  Hdens = Hdens0 * Tbar / Tbar0
  !---    Adjust pressure to height level, using pressure scale height    TSTP242
  height = ctopohgt + DZBL(khgt)
  Z1st = -6.0d0 + DBLE(K1ST)
  if (K1ST >= 16) then
    Z1st = 10.0d0 + 5.0d0*(DBLE(K1ST)-16.0d0)
  end if
  PMGCM = P1st * exp((Z1st-height)/Hpres)
  PresDay = Pzero * exp((Z1st-height)/Hpres0)
  !---    Compute density from gas law, using pressure and temperature    TSTP248
  DMGCM = PMGCM / (Rzero*TMGCM)
  Densday = PresDay / (Rzero*TempDay)
  !---    Daily maximum and minimum density                               TSTP251
  Densmin = 9999.0d0
  Densmax = -9999.0d0
  if (idaydata > 0) then
    Hdmin = Hdens0 * 0.5d0 * (Tmax1+Tempmax) / Tbar0
    Hdmax = Hdens0 * 0.5d0 * (Tmax1+Tempmin) / Tbar0
    Densmax = D1max * exp((Z1st-height)/Hdmax)
    Densmin = D1min * exp((Z1st-height)/Hdmin)
  end if
end subroutine TESsrftrp_M05
!-----------------------------------------------------------------------TSTP262
subroutine TESTterp_M05(khgtt,time,TTGCM,PTGCM,DTGCM,UTGCM,VTGCM,ZF,TempDay, &
                        PresDay,DensDay,UwndDay,VwndDay,Tempmax,Tempmin, &
                        Densmax,Densmin,mtesy,idaydata)
  !---    Interpolates University of Michigan Mars Thermospheric General  TTTP  4
  !       Circulation Model (MTGCM) data to a given latitude, time of     TTTP  5
  !       year (Ls), for a given TES year, height index (khgtt) and       TTTP  6
  !       time of day (time).                                             TTTP  7
  !       Some input data is provided by the Common "Interp".             TTTP  8
  !---    Set parameter values for number of heights (nhgtt), number      TTTP  9
  !       of latitudes (nlatt), and number of dust optical depth values   TTTP 10
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nhgtt = 33
  integer, parameter :: nlatt = 36
  integer, parameter :: ntesy = 2
  integer, parameter :: ntf10 = 3
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: idaydata,khgtt,mtesy
  real(kind=PM_REEL), intent(in) :: time
  real(kind=PM_REEL), intent(out) :: &
    DTGCM,DensDay,Densmax,Densmin,PTGCM,PresDay,TTGCM,TempDay,Tempmax,Tempmin, &
    UTGCM,UwndDay,VTGCM,VwndDay,ZF
  ! 
  !.. Local Scalars .. 
  integer :: i,itime,l,n
  real(kind=PM_REEL) :: A1,A1p,A2,A2p,D0,Dtime,P0,P1,P1p,P2,P2p,RTGCM,T0,Ttime, &
                      U0,V0,Z0,polefac,xtime
  ! 
  !.. Local Arrays .. 
  real(kind=PM_REEL), dimension(2,2,2) :: &
    DT,Dmax,Dmin,PT,Pday,R0,TT,Tday,Tmax,Tmin,UT,Uday,VT,Vday,ZT
  ! 
  ! DM 1359 remove external : private
  !.. External Calls .. 
  ! external ThreeD_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: tTideY_M05
  !real(kind=PM_REEL), external :: TideX_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic real
  ! 
  !.. Common Blocks .. 
  common /TESTERP_M05/ DLAT,DLON,DLS,DLATW,DLONW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                       ILAT,JLON,LS,K1ST,ILATW,JLONW,ILATT,MF10
  !     For dlat to dlon: Not Read, Not Written
  !     For dls: Read, Not Written
  !     For dlatw to dlonw: Not Read, Not Written
  !     For dlatt to df10: Read, Not Written
  !     For wpolefac: Not Read, Not Written
  !     For tpolefac: Maybe Read, Not Written
  !     For ilat to jlon: Not Read, Not Written
  !     For ls: Read, Not Written
  !     For k1st to jlonw: Not Read, Not Written
  !     For ilatt to mf10: Read, Not Written
  ! 
  !... Variables in Common Block /TESterp_M05/ ... 
  real(kind=PM_REEL) :: DF10,DLAT,DLATT,DLATW,DLON,DLONW,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,JLONW,K1ST,LS,MF10
  common /TGCMTES_M05/ TTTA0,TTTA1,TTTP1,TTTA2,TTTP2,TPTA0,TPTA1,TPTP1,TPTA2, &
                       TPTP2,TDTA0,TDTA1,TDTP1,TDTA2,TDTP2,TUTA0,TUTA1,TUTP1, &
                       TUTA2,TUTP2,TVTA0,TVTA1,TVTP1,TVTA2,TVTP2,TZFA0,TZFA1, &
                       TZFP1,TZFA2,TZFP2,IZTOP
  !     For tTTA0 to tZFP2: Maybe Read, Not Written
  !     For iztop: Not Read, Not Written
  ! 
  !... Variables in Common Block /TGCMTES_M05/ ... 
  real(kind=PM_REEL), dimension(nhgtt,nlatt,0:12,ntesy,ntf10) :: &
    TDTA0,TDTA1,TDTA2,TDTP1,TDTP2,TPTA0,TPTA1,TPTA2,TPTP1,TPTP2,TTTA0,TTTA1, &
    TTTA2,TTTP1,TTTP2,TUTA0,TUTA1,TUTA2,TUTP1,TUTP2,TVTA0,TVTA1,TVTA2,TVTP1, &
    TVTP2
  real(kind=PM_REEL), dimension(nlatt,0:12,ntesy,ntf10) :: &
    TZFA0,TZFA1,TZFA2,TZFP1,TZFP2
  integer, dimension(0:12,ntesy,ntf10) :: IZTOP
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Establish MTGCM values at corners of a 3-dimensional cube in    TTTP 42
  !       latitude-Ls-dust-F107 space, at the given height index (khgtt), TTTP 43
  !       and time of day (time)                                          TTTP 44
  do i = 1,2
    polefac = 1.0d0
    if (ILATT == 1) then
      if (i == 1) then
        polefac = TPOLEFAC
      end if
    elseif (ILATT==nlatt-1 .and. i==2) then
      polefac = TPOLEFAC
    end if
    do l = 1,2
      do n = 1,2
        !---      Daily mean temperature                                        TTTP 54
        T0 = TTTA0(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        Tday(i,l,n) = T0
        !---      Temperature tide amplitudes and phases                        TTTP 57
        A1 = TTTA1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1 = TTTP1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2 = TTTA2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2 = TTTP2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      Temperature at corners of 3-D cube                            TTTP 62
        TT(i,l,n) = TideX_M05(T0,A1,P1,A2,P2,time)
        !---      Max and Min temperatures at corners of 3-D cube               TTTP 64
        Tmax(i,l,n) = -9999.0d0
        Tmin(i,l,n) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            Ttime = TideX_M05(T0,A1,P1,A2,P2,xtime)
            if (Ttime > Tmax(i,l,n)) then
              Tmax(i,l,n) = Ttime
            end if
            if (Ttime < Tmin(i,l,n)) then
              Tmin(i,l,n) = Ttime
            end if
          end do
        end if
        !---      Daily mean pressure                                           TTTP 75
        P0 = TPTA0(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        Pday(i,l,n) = P0
        !---      Pressure tide amplitudes and phases                           TTTP 78
        A1p = TPTA1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1p = TPTP1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2p = TPTA2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2p = TPTP2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      Pressure at corners of 3-D cube                               TTTP 83
        PT(i,l,n) = tTideY_M05(P0,A1p,P1p,A2p,P2p,time)
        !---      Daily mean density                                            TTTP 85
        D0 = TDTA0(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      Density tide amplitudes and phases                            TTTP 87
        A1 = TDTA1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1 = TDTP1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2 = TDTA2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2 = TDTP2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      Density at corners of 3-D cube                                TTTP 92
        DT(i,l,n) = tTideY_M05(D0,A1,P1,A2,P2,time)
        !---      Max and Min densities at corners of 3-D cube                  TTTP 94
        Dmax(i,l,n) = -9999.0d0
        Dmin(i,l,n) = 9999.0d0
        if (idaydata > 0) then
          do itime = 0,23
            xtime = DBLE(real(itime))
            Dtime = tTideY_M05(D0,A1,P1,A2,P2,xtime)
            if (Dtime > Dmax(i,l,n)) then
              Dmax(i,l,n) = Dtime
            end if
            if (Dtime < Dmin(i,l,n)) then
              Dmin(i,l,n) = Dtime
            end if
          end do
        end if
        !---      Gas constant from pressure, density, and temperature          TTTP105
        R0(i,l,n) = 190.0d0
        if (T0/=0.0d0 .and. D0/=0.0d0) then
          R0(i,l,n) = P0 / (T0*D0)
        end if
        !---      Daily mean EW wind                                            TTTP108
        U0 = TUTA0(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        Uday(i,l,n) = U0
        !---      EW wind tide amplitudes and phases                            TTTP111
        A1 = TUTA1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1 = TUTP1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2 = TUTA2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2 = TUTP2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      EW wind at corners of 3-D cube                                TTTP116
        UT(i,l,n) = TideX_M05(U0,A1,P1,A2,P2,time)
        !---      Daily mean NS wind                                            TTTP118
        V0 = TVTA0(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        Vday(i,l,n) = V0
        !---      NS wind tide amplitudes and phases                            TTTP121
        A1 = TVTA1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1 = TVTP1(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2 = TVTA2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2 = TVTP2(khgtt,ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      NS wind at corners of 3-D cube                                TTTP126
        VT(i,l,n) = TideX_M05(V0,A1,P1,A2,P2,time)
        !---      Tide amplitudes and phases for ZF=height of 1.26 nbar level   TTTP128
        Z0 = TZFA0(ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A1 = TZFA1(ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P1 = TZFP1(ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        A2 = TZFA2(ILATT+i-1,LS+l-1,mtesy,MF10+n-1) * polefac
        P2 = TZFP2(ILATT+i-1,LS+l-1,mtesy,MF10+n-1)
        !---      ZF values at corners of 3-D cube                              TTTP134
        ZT(i,l,n) = TideX_M05(Z0,A1,P1,A2,P2,time)
      end do
    end do
  end do
  !---    Use 3-D interpolation to get temperature, pressure, density,    TTTP139
  !       gas constant, EW wind, NS wind, and ZF height at given lati-    TTTP140
  !       tude, Ls, dust optical depth, and solar activity                TTTP141
  call ThreeD_M05(DLATT,DLS,DF10,TT,TTGCM)
  call ThreeD_M05(DLATT,DLS,DF10,Tday,TempDay)
  call ThreeD_M05(DLATT,DLS,DF10,Tmax,Tempmax)
  call ThreeD_M05(DLATT,DLS,DF10,Tmin,Tempmin)
  call ThreeD_M05(DLATT,DLS,DF10,Dmax,Densmax)
  call ThreeD_M05(DLATT,DLS,DF10,Dmin,Densmin)
  call ThreeD_M05(DLATT,DLS,DF10,PT,PTGCM)
  call ThreeD_M05(DLATT,DLS,DF10,Pday,PresDay)
  call ThreeD_M05(DLATT,DLS,DF10,DT,DTGCM)
  call ThreeD_M05(DLATT,DLS,DF10,R0,RTGCM)
  !---    Daily density from gas constant                                 TTTP152
  DensDay = PresDay / (RTGCM*TempDay)
  call ThreeD_M05(DLATT,DLS,DF10,UT,UTGCM)
  call ThreeD_M05(DLATT,DLS,DF10,Uday,UwndDay)
  call ThreeD_M05(DLATT,DLS,DF10,VT,VTGCM)
  call ThreeD_M05(DLATT,DLS,DF10,Vday,VwndDay)
  call ThreeD_M05(DLATT,DLS,DF10,ZT,ZF)
end subroutine TESTterp_M05
!-----------------------------------------------------------------------TTTP161
real(kind=PM_REEL) function tTideY_M05(A0,A1,phi1,A2,phi2,t)
  !---    Tide value at local solar time t, from mean value A0, amplitude TTDY  2
  !       A1 and phase phi1 of 24-hour period component, and amplitude A2 TTDY  3
  !       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TTDY  4
  !       A2 are in relative units (% of mean term A0).  Phases are in    TTDY  5
  !       hours of local solar time.                                      TTDY  6
  !---    This form, based on cosine variation of log of tide, allows     TTDY  7
  !       amplitudes to exceed 100% without tide going negative (as       TTDY  8
  !       required for temperature, density, and pressure).  This form is TTDY  9
  !       used for new, higher-altitude MTGCM data, where tidal ampitudes TTDY 10
  !       are more likely to get large.                                   TTDY 11
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  real(kind=PM_REEL), intent(in) :: A0,A1,A2,phi1,phi2,t
  ! 
  !.. Local Scalars .. 
  real(kind=PM_REEL) :: pi
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, cos
  ! 
  ! ... Executable Statements ...
  ! 
  pi = 4.0d0 * atan(1.0d0)
  tTideY_M05 = A0 * ((1.0d0+0.01d0*A1)**cos(pi*(t-phi1)/12.0d0)) * &
               ((1.0d0+0.01d0*A2)**cos(pi*(t-phi2)/6.0d0))
end function tTideY_M05
!-----------------------------------------------------------------------TTDY 18
subroutine TESGCM_M05(chgt,clat,clonw,als,time,ctemp,cpres,cdens,cuwin,cvwin, &
                      blwindew,blwindns,blwindvert,Hpres,Hdens,ZF,pertfact, &
                      ctopohgt,hgtasfc,careoid,TempDay,PresDay,DensDay, &
                      EWwnDay,NSwnDay,bluday,blvday,Tempmax,Tempmin,Densmax, &
                      Densmin,Tgrnd,calbedo,icepolar,Tat5m,requa,rpole, &
                      MapYear,idaydata)
  !---    Uses interpolation routines to evaluate:                        TESG  7
  !                                                                       TESG  8
  !       ctemp    = temperature (K) at current position                  TESG  9
  !       cpres    = pressure (N/m**2) at current position                TESG 10
  !       cdens    = density (kg/m**3) at current position                TESG 11
  !       cuwin    = eastward wind component (m/s) at current position    TESG 12
  !       cvwin    = northward wind component (m/s) at current position   TESG 13
  !       blwinew  = eastward b.l. slope wind (m/s)                       TESG 14
  !       blwinns  = northward b.l. slope wind (m/s)                      TESG 15
  !       Hpres    = pressure scale height (km) at current position       TESG 16
  !       Hdens    = density scale height (km) at current position        TESG 17
  !       ZF       = height of 1.26 nbar level at current position        TESG 18
  !       pertfact = perturbation factor from random perturbation model   TESG 19
  !       ctopohgt = topographic height (km) at current position          TESG 20
  !       careoid  = local radius (km) of MOLA 1/2 degree areoid          TESG 21
  !       TempDay  = Local daily average temperature (K)                  TESG 22
  !       PresDay  = Local daily average pressure (N/m**2)                TESG 23
  !       DensDay  = Local daily average density (kg/m**3)                TESG 24
  !       EWwnDay  = Local daily average Eastward wind (m/s)              TESG 25
  !       NSwnDay  = Local daily average Northward wind (m/s)             TESG 26
  !       Tempmax  = Local daily maximum temperature (K)                  TESG 27
  !       Tempmin  = Local daily minimum temperature (K)                  TESG 28
  !       Densmax  = Local daily maximum density (kg/m**3)                TESG 29
  !       Densmin  = Local daily minimum density (kg/m**3)                TESG 30
  !       Tgrnd    = ground surface temperature (K)                       TESG 31
  !       calbedo  = surface albedo                                       TESG 32
  !       icepolar = polar ice indicator (0=no; 1=yes)                    TESG 33
  !                                                                       TESG 34
  !       at the current height (chgt), latitude (clat), current (West)   TESG 35
  !       longitude (clonw), for time of year given by Ls=als, and time   TESG 36
  !       of day (time).  Interpolation is done using either boundary     TESG 37
  !       layer or -5 to 80 km data from Ames Mars General Circulation    TESG 38
  !       model (MGCM) or from 80 to 240 km data from the University of   TESG 39
  !       Michigan Mars Thermospheric General Circulation Model (MTGCM).  TESG 40
  !                                                                       TESG 41
  !---    Set parameter values for number of MGCM heights (tnhgt), number TESG 42
  !       of MTGCM heights (nthgtt), number of MGCM boundary layer levels TESG 43
  !       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-   TESG 44
  !       tudes (nlatt), number of MGCM longitudes (nlon), and minimum    TESG 45
  !       perturbation magnitude at surface (pert0)                       TESG 46
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nthgt = 30
  integer, parameter :: nthgtt = 33
  integer, parameter :: nbl = 3
  integer, parameter :: nlat = 25
  integer, parameter :: nlatt = 36
  integer, parameter :: nlon = 40
  integer, parameter :: ntesy = 2
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  integer, parameter :: ntf10 = 3
  real(kind=PM_REEL), parameter :: pert0 = 0.02d0
  real(kind=PM_REEL), parameter :: pertmin = 0.1d0
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: MapYear,idaydata
  integer, intent(out) :: icepolar
  real(kind=PM_REEL), intent(in) :: als,clat,clonw,hgtasfc,requa,rpole,time
  real(kind=PM_REEL), intent(inout) :: Tat5m,chgt
  real(kind=PM_REEL), intent(out) :: &
    DensDay,Densmax,Densmin,EWwnDay,Hdens,Hpres,NSwnDay,PresDay,TempDay, &
    Tempmax,Tempmin,Tgrnd,ZF,bluday,blvday,blwindew,blwindns,blwindvert, &
    calbedo,careoid,cdens,cpres,ctemp,ctopohgt,cuwin,cvwin,pertfact
  ! 
  !.. Local Scalars .. 
  integer :: LoH,itime,jbl,khgt,khgtt,lhgtt
  real(kind=PM_REEL) :: CpofT,DMGCM1,DMGCM2,DMGCMx,Dday1,Dday2,Ddayx,Dmax1, &
                      Dmax2,Dmaxx,Dmin1,Dmin2,Dminx,Hden12,Hdens1,Hdensc, &
                      Hdensdayc,Hpres1,Hpresday,Oldrref,PMGCM1,PMGCM2,PMGCMx, &
                      Pday1,Pday2,Pdayx,R1,R2,Rgas,Rgas1,Rgas2,Rgasday,Rref, &
                      TMGCM1,TMGCM2,TMGCMx,Tbar,Tcheck,Tday1,Tday2
  real(kind=PM_REEL) :: Tdayx,Tmax1,Tmax2,Tmaxx,Tmin1,Tmin2,Tminx,Tsubl,UMGCM1, &
                      UMGCM2,UMGCMx,Uday1,Uday2,Udayx,VMGCM1,VMGCM2,VMGCMx, &
                      Vday1,Vday2,Vdayx,ZF80,albedo,blu,blv,blw,curoffset, &
                      dTdz,dhgt,fLH,factor,gCp,globoffst,goR,gz,hgtk1,offset1, &
                      offset2,ofsmgcm,ofsmult1,ofsmult2
  real(kind=PM_REEL) :: ofsz1,ofsz2,pertlo,pertmax,polefac,steplat,steplon, &
                      tlat1st,topohgt,toprelief,tsteplat,uhgt,wavemax,z0,z1, &
                      z1ofs,z2,z2ofs,z2x,z5,zeval
  ! 
  ! DM 1359 remove external : private
  !.. External Calls .. 
  !external RELLIPS_M05, TESGterp_M05, TESTterp_M05, TESsrftrp_M05, bltp_M05, &
  !         slopewind_M05, subltchk_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: Cp_M05
  !real(kind=PM_REEL), external :: Zlogr_M05
  !integer, external :: Ifloor_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, exp, log, sin, sqrt, dble
  ! 
  !.. Common Blocks .. 
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust: Not Read, Not Written
  !     For dzbl to zwsfc: Maybe Read, Not Written
  !     For f10val: Not Read, Not Written
  !     For f10TES: Maybe Read, Not Written
  !     For dustc to solTES: Not Read, Not Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /TESTERP_M05/ DLAT,DLON,DLS,DLATW,DLONW,DLATT,DF10,WPOLEFAC,TPOLEFAC, &
                       ILAT,JLON,LS,K1ST,ILATW,JLONW,ILATT,MF10
  !     For dlat to mf10: Not Read, Overwritten
  ! 
  !... Variables in Common Block /TESterp_M05/ ... 
  real(kind=PM_REEL) :: DF10,DLAT,DLATT,DLATW,DLON,DLONW,DLS,TPOLEFAC,WPOLEFAC
  integer :: ILAT,ILATT,ILATW,JLON,JLONW,K1ST,LS,MF10
  common /TGCMOFFSET_M05/ OFFSETS,TOFFSETS,ZOFFSET,HGTOFFSET,OFSZL,IBOUGHER
  !     For offsets: Not Read, Not Written
  !     For toffsets to zoffset: Maybe Read, Not Written
  !     For hgtoffset to ofszL: Not Read, Overwritten
  !     For ibougher: Read, Not Written
  ! 
  !... Variables in Common Block /TGCMoffset_M05/ ... 
  real(kind=PM_REEL), dimension(0:12,ndust) :: OFFSETS
  real(kind=PM_REEL), dimension(0:12,ntesy) :: TOFFSETS
  real(kind=PM_REEL) :: HGTOFFSET,OFSZL,ZOFFSET
  integer :: IBOUGHER
  common /THERM_M05/ F107,STDL,FMOL
  !     For F107: Read, Not Written
  !     For stdl to fmol: Not Read, Not Written
  ! 
  !... Variables in Common Block /THERM_M05/ ... 
  real(kind=PM_REEL) :: F107,STDL
  real(kind=PM_REEL), dimension(0:8) :: FMOL
  ! 
  ! ... Executable Statements ...
  ! 
  pertmax = 0.2d0
  pertfact = 0.0d0
  !---    Initialize ground surface temperature and polar ice indicator   TESG 72
  Tgrnd = 999.9d0
  icepolar = 99
  !---    Insure latitude, longitude, Ls, and time of day within proper   TESG 75
  !       bounds                                                          TESG 76
  if (abs(clat) > 90.0d0) stop " Latitude error in TESGCM_M05"
  if (abs(clonw) > 360.0d0) stop " Longitude error: TESGCM_M05"
  if (als<0.0d0 .or. als>360.0d0) stop " Ls error: TESGCM_M05"
  if (time<0.0d0 .or. time>24.0d0) stop " time error in TESGCM_M05"
  !---    Latitude step size for MGCM and MTGCM data                      TESG 82
  steplat = 180.0d0 / (dble(nlat)-1.0d0)
  tsteplat = 180.0d0 / dble(nlatt)
  !---    Most southerly MTGCM latitude                                   TESG 85
  tlat1st = -90.0d0 + tsteplat/2.0d0
  !---    Longitude step size for MGCM boundary layer data                TESG 87
  steplon = 360.0d0 / dble(nlon)
  !---    MGCM height index (khgt) for current height (chgt)              TESG 89
  khgt = Ifloor_M05(chgt+6.0d0)
  if (khgt > 16) then
    khgt = 16 + Ifloor_M05((chgt-10.0d0)/5.0d0)
  end if
  !---    Insure khgt within proper limits                                TESG 92
  if (khgt < 1) then
    khgt = 1
  end if
  if (khgt > nthgt-1) then
    khgt = nthgt - 1
  end if
  !---    MGCM latitude index (ilat) from current latitude (clat)         TESG 95
  ILAT = 1 + Ifloor_M05((clat+90.0d0)/steplat)
  if (ILAT > nlat-1) then
    ILAT = nlat - 1
  end if
  !---    MGCM wind latitude index (ilatw).  MGCM V winds are offset in   TESG 98
  !       lat by 1/2 lat grid step.  No lat offset for U (Arakawa C-grid) TESG 99
  ILATW = Ifloor_M05((clat+90.0d0+1.5d0*steplat)/steplat)
  !---    Insure ilatw within proper bounds                               TESG101
  if (ILATW < 2) then
    ILATW = 2
  end if
  if (ILATW > nlat-1) then
    ILATW = nlat - 1
  end if
  !---    MTGCM latitude index (ilatt) from current latitude (clat)       TESG104
  ILATT = 1 + Ifloor_M05((clat-tlat1st)/tsteplat)
  !---    Insure ilatt within proper bounds                               TESG106
  if (ILATT < 1) then
    ILATT = 1
  end if
  if (ILATT > nlatt-1) then
    ILATT = nlatt - 1
  end if
  !---    MGCM boundary layer longitude index (jlon)                      TESG109
  JLON = Ifloor_M05(clonw/steplon)
  if (JLON > nlon-1) then
    JLON = nlon - 1
  end if
  !---    Lon offset for C-GRID (1/2 step Eastward for U component)       TESG112
  JLONW = Ifloor_M05((clonw+steplon/2.0d0)/steplon)
  if (JLONW > nlon-1) then
    JLONW = 0
  end if
  !---    Time of year index (ls) from input Ls value (als)               TESG115
  LS = Ifloor_M05(als/30.0d0)
  if (LS > 11) then
    LS = 11
  end if
  !---    Increment of MGCM latitude (dlat) from grid point               TESG118
  DLAT = (clat-steplat*(dble(ILAT)-1.0d0)+90.0d0) / steplat
  !---    Increment of MTGCM latitude (dlatt) from grid point             TESG120
  DLATT = (clat-tsteplat*(dble(ILATT)-1.0d0)-tlat1st) / tsteplat
  !---    Insure dlatt within proper bounds near poles                    TESG122
  TPOLEFAC = 1.0d0
  if (ILATT == 1) then
    TPOLEFAC = 0.5d0
    if (DLATT <= 0.0d0) then
      DLATT = 0.0d0
      TPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
    end if
  elseif (ILATT >= nlat-1) then
    TPOLEFAC = 0.5d0
    if (DLATT >= 1.0d0) then
      DLATT = 1.0d0
      TPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
    end if
  end if
  !---    Increment of MGCM longitude (dlon) from grid point              TESG137
  DLON = (clonw-steplon*dble(JLON)) / steplon
  DLONW = (clonw-steplon*(dble(JLONW)-0.5d0)) / steplon
  if (DLONW > 40.0d0) then
    DLONW = DLONW - 40.0d0
  end if
  !---    Increment of MGCM latitude from (offset) wind grid point        TESG141
  DLATW = (clat-steplat*(dble(ILATW)-2.0d0)+86.25d0) / steplat
  WPOLEFAC = 1.0d0
  if (ILATW == 2) then
    WPOLEFAC = 0.75d0
    if (DLATW <= 0.0d0) then
      WPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      DLATW = 0.0d0
    end if
  elseif (ILATW >= nlat-1) then
    WPOLEFAC = 0.75d0
    if (DLATW >= 1.0d0) then
      WPOLEFAC = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      DLATW = 1.0d0
    end if
  end if
  !---    Increment of solar activity (F10.7 at 1AU) for MTGCM data       TESG157
  MF10 = 1
  if (F107 > F10TES(2)) then
    MF10 = 2
  end if
  DF10 = Zlogr_M05(F107,F10TES(MF10),"TESG-01") / &
         Zlogr_M05(F10TES(MF10+1),F10TES(MF10),"TESG-02")
  !---    Get areoid radius and topographic height at current lat, lon    TESG162
  call RELLIPS_M05(clat,clonw,careoid,chgt,gz,Oldrref,ctopohgt,calbedo,requa, &
                   rpole)
  !---    Compute topographic relief factor for simplified mountain       TESG165
  !       wave perturbation model                                         TESG166
  toprelief = 25.0d0 + ctopohgt
  !---    Use topographic height if input height is <= -8.7 km            TESG168
  if (chgt <= -8.7d0) then
    chgt = ctopohgt + hgtasfc
  end if
  !---    Find height index (k1st) of first -5 to 80 km MGCM level above  TESG170
  !       surface topographic height                                      TESG171
  K1ST = Ifloor_M05(7.0d0+ctopohgt+0.3d0)
  if (K1ST >= 16) then
    K1ST = Ifloor_M05(15.0d0+(ctopohgt+1.0d0)/5.0d0)
  end if
  if (K1ST < 1) then
    K1ST = 1
  end if
  hgtk1 = -6.0d0 + dble(K1ST)
  if (K1ST >= 16) then
    hgtk1 = 10.0d0 + 5.0d0*(dble(K1ST)-16.0d0)
  end if
  !---    Find Ls increment (dls) from Ls "grid" on input data            TESG177
  DLS = (als-30.0d0*dble(LS)) / 30.0d0
  !---    Initialize ZF = height of 1.26 nbar level (output value if      TESG179
  !       current height < 80 km)                                         TESG180
  ZF = 999.0d0
  !---    Assign MTGCM height offset from input zoffset or array offsets  TESG182
  globoffst = 0.0d0
  curoffset = 0.0d0
  if (IBOUGHER == 2) then
    offset1 = TOFFSETS(LS,MapYear)
    offset2 = TOFFSETS(LS+1,MapYear)
    globoffst = offset1 + (offset2-offset1)*DLS
  else
    call TESTterp_M05(1,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,ZF,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,MapYear, &
                      idaydata)
    call TESTterp_M05(2,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,MapYear, &
                      idaydata)
    Hdensc = 5.0d0 / Zlogr_M05(DMGCM1,DMGCM2,"TESG-03")
    Hdensdayc = 5.0d0 / Zlogr_M05(Dday1,Dday2,"TESG-04")
    call TESGterp_M05(nthgt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,MapYear, &
                      idaydata)
    if (IBOUGHER == 3) then
      curoffset = Hdensdayc * Zlogr_M05(Dday2,Dday1,"TESG-05")
    else
      curoffset = Hdensc * Zlogr_M05(DMGCM2,DMGCM1,"TESG-06")
    end if
  end if
  if (IBOUGHER <= 0) then
    HGTOFFSET = ZOFFSET
  elseif (IBOUGHER == 1) then
    HGTOFFSET = ZOFFSET - 0.5d0*sin(atan(1.0d0)*als/45.0d0)
  elseif (IBOUGHER == 2) then
    HGTOFFSET = globoffst
  else
    HGTOFFSET = curoffset
  end if
  !---    MTGCM height index (khgtt) for current height                   TESG216
  khgtt = Ifloor_M05((chgt-HGTOFFSET-75.0d0)/5.0d0)
  !---    Insure khgtt within proper limits                               TESG218
  if (khgtt < 1) then
    khgtt = 1
  end if
  lhgtt = 1
  if (khgtt==1 .and. HGTOFFSET<-4.0d0) then
    khgtt = 2
    lhgtt = 2
  end if
  if (khgtt > nthgtt-1) then
    khgtt = nthgtt - 1
  end if
  !---    Initialize MGCM height offset to zero                           TESG226
  OFSZL = 0.0d0
  !---    Use MTGCM interpolation if height >= 80 km                      TESG228
  if (chgt >= 80.0d0+HGTOFFSET+5.0d0*(dble(lhgtt)-1.0d0)) then
    !---      Get temperature, pressure, density, and wind components at    TESG230
    !         height indexes above and below current height                 TESG231
    call TESTterp_M05(khgtt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,ZF,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,MapYear, &
                      idaydata)
    call TESTterp_M05(khgtt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF, &
                      Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2, &
                      MapYear,idaydata)
    !---      Height grid points above and below current height             TESG238
    z1 = 75.0d0 + 5.0d0*dble(khgtt) + HGTOFFSET
    z2 = 80.0d0 + 5.0d0*dble(khgtt) + HGTOFFSET
    !---      Apply MTGCM height offset to ZF altitude                      TESG241
    ZF = ZF + HGTOFFSET
    !---      Pressure and density scale heights                            TESG243
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"TESG-07")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"TESG-08")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"TESG-09")
    OFSZL = HGTOFFSET
  !---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80   TESG248
  !       km if height between 75 and 80 km                               TESG249
  elseif (chgt >= 75.0d0) then
    !---      Get temperature, pressure, density, and wind components at    TESG251
    !         heights above and below current height                        TESG252
    call TESGterp_M05(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,MapYear, &
                      idaydata)
    call TESTterp_M05(lhgtt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,ZF80, &
                      Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2, &
                      MapYear,idaydata)
    z1 = 75.0d0
    z2 = 80.0d0 + HGTOFFSET + 5.0d0*(dble(lhgtt)-1.0d0)
    !---      Apply 'equivalent' multiplier for offset between 75 & 80 km   TESG261
    if (IBOUGHER <= 1) then
      z1ofs = 60.0d0
      ofsmgcm = HGTOFFSET - curoffset
      ofsz1 = ofsmgcm * (z1-z1ofs) / (z2-z1ofs)
      call TESGterp_M05(khgt+1,time,TMGCMx,PMGCMx,DMGCMx,UMGCMx,VMGCMx,Tdayx, &
                        Pdayx,Ddayx,Udayx,Vdayx,Tmaxx,Tminx,Dmaxx,Dminx, &
                        MapYear,idaydata)
      Hden12 = 5.0d0 / log(DMGCM1/DMGCMx)
      ofsmult1 = exp(ofsz1/Hden12)
      !---        Local MGCM height offset                                    TESG271
      OFSZL = ofsmgcm * (CHGT-z1ofs) / (z2-z1ofs)
      PMGCM1 = PMGCM1 * ofsmult1
      DMGCM1 = DMGCM1 * ofsmult1
      Pday1 = Pday1 * ofsmult1
      Dday1 = Dday1 * ofsmult1
      Dmax1 = Dmax1 * ofsmult1
      Dmin1 = Dmin1 * ofsmult1
    end if
    !---      Pressure and density scale heights (km)                       TESG280
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"TESG-10")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"TESG-11")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"TESG-12")
  !---    Use TESsrftrp_M05 routine if height within boundary layer       TESG284
  elseif (chgt <= ctopohgt+DZBL(nbl)) then

    !---      Set index for surface layer data                              TESG286
    jbl = 1
    if (chgt >= ctopohgt+DZBL(2)) then
      jbl = 2
    end if
    !---      Get temperature, pressure, density, and wind components at    TESG289
    !         heights above and below current height                        TESG290
    call TESsrftrp_M05(jbl+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Hpres, &
                       Hdens,ctopohgt,Tday2,Pday2,Dday2,Uday2,Vday2,presday, &
                       Tmax2,Tmin2,Dmax2,Dmin2,Tat5m,MapYear,idaydata)
    call TESsrftrp_M05(jbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Hpres1, &
                       Hdens1,ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1,Hpresday, &
                       Tmax1,Tmin1,Dmax1,Dmin1,Tat5m,MapYear,idaydata)
    !---      Heights at two boundary layer levels                          TESG299
    z1 = ctopohgt + DZBL(jbl)
    z2 = ctopohgt + DZBL(jbl+1)
    !---      Get Temp at 1st height above BL, for density scale height     TESG302
    call TESGterp_M05(K1ST,time,TMGCMx,PMGCMx,DMGCMx,UMGCMx,VMGCMx,Tdayx, &
                      Pdayx,Ddayx,Udayx,Vdayx,Tmaxx,Tminx,Dmaxx,Dminx,MapYear, &
                      idaydata)
    !---      Temperature gradient for density scale height calculation     TESG306
    z2x = -6.0d0 + dble(K1ST)
    if (K1ST >= 16) then
      z2x = 10.0d0 + 5.0d0*(dble(K1ST)-16.0d0)
    end if
    dTdz = (TMGCMx-TMGCM1) / (z2x-z1)
    if (chgt <= ctopohgt) then
      dTdz = 0.0d0
    end if
    !---      Average layer temperature for density scale height            TESG311
    Tbar = (TMGCM1+TMGCM2) / 2.0d0
    !---      Density scale height from pressure scale height and           TESG313
    !         temperature gradient                                          TESG314
    Hdens = Hpres / (1.0d0+(Hpres/Tbar)*dTdz)
    !---      Perturbation factor = surface value                           TESG316
    pertfact = pert0
  !---    Use TESGterp_M05 routine if height above boundary layer levels  TESG318
  !        and height <= 75 km                                            TESG319
  elseif (chgt >= hgtk1) then
    !---      Get temperature, pressure, density, and wind components at    TESG321
    !         heights above and below current height                        TESG322
    call TESGterp_M05(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Tday1, &
                      Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1,MapYear, &
                      idaydata) !---    Use TESsrftrp_M05 at top of boundary layer and TESGterp_M05 at  TESG374
                                !       1st level above boundary layer if height between boundary layer TESG375
                                !       and height index k1st                                           TESG376
    call TESGterp_M05(khgt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,MapYear, &
                      idaydata)
    !---      Heights at grid points above and below current level          TESG329
    z1 = -6.0d0 + dble(khgt)
    z2 = -5.0d0 + dble(khgt)
    if (khgt >= 16) then
      z1 = 10.0d0 + 5.0d0*dble((khgt-16))
      z2 = 15.0d0 + 5.0d0*dble((khgt-16))
    end if
    !---      Apply 'equivalent' multiplier for offset below 75 km          TESG336
    if (IBOUGHER <= 1) then
      z1ofs = 60.0d0
      z2ofs = 80.0d0 + HGTOFFSET + 5.0d0*(dble(lhgtt)-1.0d0)
      ofsmgcm = HGTOFFSET - curoffset
      if (z1 <= z1ofs) then
        ofsmult1 = 1.0d0
      else
        ofsz1 = ofsmgcm * (z1-z1ofs) / (z2ofs-z1ofs)
        Hden12 = 5.0d0 / log(DMGCM1/DMGCM2)
        ofsmult1 = exp(ofsz1/Hden12)
      end if
      if (z2 <= z1ofs) then
        ofsmult2 = 1.0d0
      else
        ofsz2 = ofsmgcm * (z2-z1ofs) / (z2ofs-z1ofs)
        Hden12 = 5.0d0 / log(DMGCM1/DMGCM2)
        ofsmult2 = exp(ofsz2/Hden12)
      end if
      !---        Local MGCM height offset                                    TESG355
      if (CHGT > z1ofs) then
        OFSZL = ofsmgcm * (CHGT-z1ofs) / (z2ofs-z1ofs)
      end if
      PMGCM1 = PMGCM1 * ofsmult1
      DMGCM1 = DMGCM1 * ofsmult1
      PMGCM2 = PMGCM2 * ofsmult2
      DMGCM2 = DMGCM2 * ofsmult2
      Pday1 = Pday1 * ofsmult1
      Dday1 = Dday1 * ofsmult1
      Dmax1 = Dmax1 * ofsmult1
      Dmin1 = Dmin1 * ofsmult1
      Pday2 = Pday2 * ofsmult2
      Dday2 = Dday2 * ofsmult2
      Dmax2 = Dmax2 * ofsmult2
      Dmin2 = Dmin2 * ofsmult2
    end if
    !---      Pressure and density scale heights (km)                       TESG370
    Hpres = (z2-z1) / Zlogr_M05(PMGCM1,PMGCM2,"TESG-13")
    Hpresday = (z2-z1) / Zlogr_M05(Pday1,Pday2,"TESG-14")
    Hdens = (z2-z1) / Zlogr_M05(DMGCM1,DMGCM2,"TESG-15")
  else
    !---      Get temperature, pressure, density, and wind components at    TESG378
    !         heights above and below current height                        TESG379
    call TESsrftrp_M05(nbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,VMGCM1,Hpres, &
                       Hdens,ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1,Hpresday, &
                       Tmax1,Tmin1,Dmax1,Dmin1,Tat5m,MapYear,idaydata)
    call TESGterp_M05(K1ST,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,VMGCM2,Tday2, &
                      Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2,MapYear, &
                      idaydata)
    !---      Heights at grid points above and below current level          TESG386
    z1 = ctopohgt + DZBL(nbl)
    z2 = -6.0d0 + dble(K1ST)
    if (K1ST >= 16) then
      z2 = 10.0d0 + 5.0d0*(dble(K1ST)-16.0d0)
    end if
    !---      Temperature gradient and mean temperature for density scale   TESG390
    !         height calculation                                            TESG391
    dTdz = (TMGCM2-TMGCM1) / (z2-z1)
    Tbar = (TMGCM1+TMGCM2) / 2.0d0
    !---      Density scale height from pressure scale height and           TESG394
    !         temperature gradient                                          TESG395
    Hdens = Hpres / (1.0d0+(Hpres/Tbar)*dTdz)
  end if
  !---    Get gas constant from pressure, density, and temperature        TESG398
  if (chgt <= ctopohgt) then
    Rgas = PMGCM1 / (DMGCM1*TMGCM1)
    Rgasday = Pday1 / (Dday1*Tday1)
    dhgt = (ctopohgt-z1) / (z2-z1)
  else
    dhgt = (chgt-z1) / (z2-z1)
    R1 = PMGCM1 / (DMGCM1*TMGCM1)
    R2 = PMGCM2 / (DMGCM2*TMGCM2)
    Rgas1 = Pday1 / (Dday1*Tday1)
    Rgas2 = Pday2 / (Dday2*Tday2)
    Rgas = R1 + dhgt*(R2-R1)
    Rgasday = Rgas1 + dhgt*(Rgas2-Rgas1)
  end if
  !---    Use logarithmic wind and temperature profiles (with surface     TESG412
  !       roughness z0) if height below lowest boundary layer level       TESG413
  if (chgt < ctopohgt+DZBL(2)) then
    !---      Convert surface roughness to km                               TESG415
    z0 = ZWSFC / 1000.0d0 !---    Use linear height interpolation if above logarithmic            TESG480
                          !       surface layer                                                   TESG481
    !---      Save ground surface temperature for output                    TESG417
    Tgrnd = TMGCM1
    !---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if    TESG419
    !         over ice (T <= CO2 sublimation temperature + 5K)              TESG420
    Tcheck = TMGCM1
    call subltchk_M05(Tcheck,PMGCM2,Tsubl)
    !---      If surface temperature near sublimation point, set polar ice  TESG423
    !           indicator on (= 1) and re-set surface roughness             TESG424
    icepolar = 0
    if (TMGCM1 <= Tsubl+5.0d0) then
      z0 = 1.0d-7
      icepolar = 1
    end if
    uhgt = chgt - ctopohgt
    if (uhgt < z0) then
      uhgt = z0
    end if
    !---      Compute logarithmic boundary layer shape factor for surface   TESG432
    !         to lowest boundary layer level                                TESG433
    factor = Zlogr_M05(uhgt,z0,"TESG-16") / Zlogr_M05(DZBL(2),z0,"TESG-17")
    !---      Apply factor for wind; assume no-slip condition at surface    TESG436
    cuwin = UMGCM2 * factor
    cvwin = VMGCM2 * factor
    EWwnDay = Uday2 * factor
    NSwnDay = Vday2 * factor
    !---      Set up parameters to evaluate temperature boundary layer      TESG441
    !         Convert heights to meters for input to bltp_M05 subroutine    TESG442
    z5 = DZBL(2) * 1000.0d0
    zeval = uhgt * 1000.0d0
    !---      Get value of local gravity                                    TESG445
    call RELLIPS_M05(clat,clonw,Rref,chgt,gz,Oldrref,topohgt,albedo,requa, &
                     rpole)
    !---      Use Ames MGCM boundary layer model for current temperature    TESG448
    !         Get specific heat at constant pressure                        TESG449
    CpofT = Cp_M05(TMGCM2)
    call bltp_M05(gz,CpofT,TMGCM1,z5,TMGCM2,UMGCM2,VMGCM2,zeval,factor,ctemp)
    !---      Use Ames MGCM boundary layer model for daily avg temperature  TESG453
    CpofT = Cp_M05(Tday2)
    call bltp_M05(gz,CpofT,Tday1,z5,Tday2,Uday2,Vday2,zeval,factor,Tempday)
    !---      Use Ames MGCM boundary layer model for daily max temperature  TESG457
    CpofT = Cp_M05(Tmax2)
    call bltp_M05(gz,CpofT,Tmax1,z5,Tmax2,Uday2,Vday2,zeval,factor,Tempmax)
    !---      Use Ames MGCM boundary layer model for daily min temperature  TESG461
    CpofT = Cp_M05(Tmin2)
    call bltp_M05(gz,CpofT,Tmin1,z5,Tmin2,Uday2,Vday2,zeval,factor,Tempmin)
    !---      Pressure at current position from pressure scale height       TESG465
    cpres = PMGCM2 * exp((z2-chgt)/Hpres)
    PresDay = Pday2 * exp((z2-chgt)/Hpresday)
    !---      Density at current position from gas law                      TESG468
    cdens = cpres / (Rgas*ctemp)
    DensDay = Presday / (RgasDay*TempDay)
    Densmin = 9999.0d0
    Densmax = -9999.0d0
    if (idaydata > 0) then
      !---      Daily maximum and minimum density                             TESG474
      Densmin = DensDay * (Dmin1/Dday1+factor*((Dmin2/Dday2)-(Dmin1/Dday1)))
      Densmax = DensDay * (Dmax1/Dday1+factor*((Dmax2/Dday2)-(Dmax1/Dday1)))
    end if
  else
    dhgt = (chgt-z1) / (z2-z1)
    cuwin = UMGCM1 + dhgt*(UMGCM2-UMGCM1)
    cvwin = VMGCM1 + dhgt*(VMGCM2-VMGCM1)
    EWwnDay = Uday1 + dhgt*(Uday2-Uday1)
    NSwnDay = Vday1 + dhgt*(Vday2-Vday1)
    !---      Interpolate temperature to current height                     TESG488
    ctemp = TMGCM1 + dhgt*(TMGCM2-TMGCM1)
    TempDay = Tday1 + dhgt*(Tday2-Tday1)
    Tempmax = Tmax1 + dhgt*(Tmax2-Tmax1)
    Tempmin = Tmin1 + dhgt*(Tmin2-Tmin1)
    !---      Pressure at current position from pressure scale height       TESG493
    cpres = PMGCM2 * exp((z2-chgt)/Hpres)
    PresDay = Pday2 * exp((z2-chgt)/Hpresday)
    !---      Density at current position from gas law                      TESG496
    cdens = cpres / (Rgas*ctemp)
    DensDay = Presday / (RgasDay*TempDay)
    Densmin = 9999.0d0
    Densmax = -9999.0d0
    if (idaydata > 0) then
      !---      Daily maximum and minimum density                             TESG502
      Densmin = DensDay * (Dmin1/Dday1+dhgt*((Dmin2/Dday2)-(Dmin1/Dday1)))
      Densmax = DensDay * (Dmax1/Dday1+dhgt*((Dmax2/Dday2)-(Dmax1/Dday1)))
    end if
  end if
  if (chgt < ctopohgt+0.5d0) then
    if (abs(clat) >= 85.0d0) then
      polefac = 1.0d0 - (abs(clat)-85.0d0)/5.0d0
      cpres = polefac*cpres + (1.0d0-polefac)*PresDay
      cdens = polefac*cdens + (1.0d0-polefac)*DensDay
      Densmin = 9999.0d0
      Densmax = -9999.0d0
      if (idaydata > 0) then
        Densmax = polefac*Densmax + (1.0d0-polefac)*DensDay
        Densmin = polefac*Densmin + (1.0d0-polefac)*DensDay
      end if
    end if
  end if
  !---    Set specific bogus values of pressure or density scale heights  TESG522
  !       are out of range                                                TESG523
  if (Hpres < -9.99d0) then
    Hpres = -9.99d0
  end if
  if (Hpres > 99.99d0) then
    Hpres = 99.99d0
  end if
  if (Hdens < -9.99d0) then
    Hdens = -9.99d0
  end if
  if (Hdens > 99.99d0) then
    Hdens = 99.99d0
  end if
  wavemax = 1.0d0
  if (abs(z2-z1) >= 1.0d0) then
    dTdz = (TMGCM2-TMGCM1) / (z2-z1)
    LoH = INT(2.0d0)
    fLH = dble(LoH) / 6.283185d0
    fLH = fLH * sqrt(1.0d0+fLH**2)
    gCp = 1000.0d0 * gz / Cp_M05(ctemp)
    goR = 1000.0d0 * gz / Rgas
    if (dTdz < -0.1d0*gCp) then
      dTdz = -0.1d0*gCp
    end if
    wavemax = fLH * (dTdz+gCp) / goR
    pertlo = pert0 + 0.0004d0*chgt
    if (pertlo > 0.1d0) then
      pertlo = 0.1d0
    end if
    if (wavemax < pertlo) then
      wavemax = pertlo
    end if
    if (wavemax < pertmax) then
      pertmax = wavemax
    end if
  end if
  !---    Compute perturbation factor, unless it has already been set     TESG543
  if (pertfact < pert0) then
    !---      Perturbation factor from simplified mountain wave model       TESG545
    pertfact = 0.01d0 * toprelief * exp((chgt-100.0d0)/40.0d0)
    if (pertfact > pertmax) then
      pertfact = pertmax
    end if
    if (pertfact < pert0+.00025d0*chgt) then
      pertfact = pert0 + .00025d0*chgt
    end if
    if (chgt>=100.0d0 .and. pertfact<pertmin) then
      pertfact = pertmin
    end if
  end if
  !---    Get slope winds (0 below surface and > 4.5 km above surface)    TESG552
  call slopewind_M05(clat,clonW,chgt,time,cuwin,cvwin,blwindew,blwindns, &
                     blwindvert)
  !---    Compute daily average slope winds                               TESG555
  bluday = 0.0d0
  blvday = 0.0d0
  do itime = 0,22,2
    call slopewind_M05(clat,clonW,chgt,dble(itime),cuwin,cvwin,blu,blv,blw)
    bluday = bluday + blu
    blvday = blvday + blv
  end do
  bluday = bluday / 12.0d0
  blvday = blvday / 12.0d0
end subroutine TESGCM_M05
!-----------------------------------------------------------------------TESG568
subroutine ProfTerp_M05(chgt,clat,clon,tin,pin,din,uin,vin,ptemp,ppres,pdens, &
                        puwin,pvwin,nprof,profnear,proffar,profwgt)
  !---    Interpolates profile data to current position (chgt,clat,clon)  PTRP  3
  !       and weights results (with factor profwgt) with input values     PTRP  4
  !       (tin,pin,din,uin,vin), yielding weighted average (ptemp,ppres,  PTRP  5
  !       pdens,puwin,pvwin).  Input profnear is lat-lon radius over      PTRP  6
  !       which profile is weighted with 1.0; proffar is lat-lon radius   PTRP  7
  !       beyond which profile is given zero weight.                      PTRP  8
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: npmax = 100000
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: nprof
  real(kind=PM_REEL), intent(in) :: &
    chgt,clat,clon,din,pin,proffar,profnear,tin,uin,vin
  real(kind=PM_REEL), intent(inout) :: ppres,ptemp
  real(kind=PM_REEL), intent(out) :: pdens,profwgt,puwin,pvwin
  ! 
  !.. Local Scalars .. 
  integer :: i,i1,i2,ni
  real(kind=PM_REEL) :: dlat,dlat1,dlat2,dlon,dlon1,dlon2,dplon,facthgt,factll, &
                      factor,pi2,pilat,pilon,radius,radius1,radius2,tpdwgt, &
                      uvwgt
  ! 
  !.. Local Arrays .. 
  integer, dimension(2) :: ia
  real(kind=PM_REEL), dimension(2) :: adll
  ! 
  !.. Intrinsic Functions .. 
  intrinsic abs, atan, sin, sqrt
  ! 
  !.. Common Blocks .. 
  common /PTERP_M05/ PHGT,PLAT,PLON,PTMP,PPRS,PDEN,PUWN,PVWN
  !     For phgt to pvwn: Maybe Read, Not Written
  ! 
  !... Variables in Common Block /pterp_M05/ ... 
  real(kind=PM_REEL), dimension(npmax) :: &
    PDEN,PHGT,PLAT,PLON,PPRS,PTMP,PUWN,PVWN
  ! 
  !.. Data Declarations .. 
  data ia/2*0/
  data adll/2*0.0d0/
  data PDEN/npmax*0.0d0/
  data PHGT/npmax*0.0d0/
  data PLAT/npmax*0.0d0/
  data PLON/npmax*0.0d0/
  data PPRS/npmax*0.0d0/
  data PTMP/npmax*0.0d0/
  data PUWN/npmax*0.0d0/
  data PVWN/npmax*0.0d0/

  ! 
  ! ... Executable Statements ...
  ! 
  !---    Calculate pi/2                                                  PTRP 13
  pi2 = 2.0d0 * atan(1.0d0)
  i1 = 0
  i2 = 0
  ni = 0
  profwgt = 0.0d0
  !---    Find nearest pair of points above and below current height      PTRP 18
  do i = 1,nprof-1
    if ((chgt-PHGT(i))*(chgt-PHGT(i+1))<0.0d0 .or. chgt==PHGT(i)) then
      ni = ni + 1
      if (ni > 2) stop " Too many height pairs in profile"
      ia(ni) = i
      dlat1 = abs(clat-PLAT(i))
      dlat2 = abs(clat-PLAT(i+1))
      dlon1 = abs(clon-PLON(i))
      dlon2 = abs(clon-PLON(i+1))
      !---        Adjust lon difference for wrap at lon 360                   PTRP 24
      if (dlon1 > 180.0d0) then
        dlon1 = 360.0d0 - dlon1
      end if
      if (dlon2 > 180.0d0) then
        dlon2 = 360.0d0 - dlon2
      end if
      !---        Lat-lon radius from positions of points i1 and i2           PTRP 27
      radius1 = sqrt(dlat1**2+dlon1**2)
      radius2 = sqrt(dlat2**2+dlon2**2)
      adll(ni) = (radius1+radius2) / 2.0d0
    end if
  end do
  if (ni > 0) then
    i1 = ia(1)
    if (ni==2 .and. adll(2)<adll(1)) then
      i1 = ia(2)
    end if
    i2 = i1 + 1
  end if
  if (i1 == 0) then
    pdens = 0.0d0
    puwin = 0.0d0
    pvwin = 0.0d0
  else
    !---    Compute factor for linear height interpolation                  PTRP 45
    factor = (chgt-PHGT(i1)) / (PHGT(i2)-PHGT(i1))
    !---    Linear height interpolation for lat,lon,temperature,winds       PTRP 47
    pilat = PLAT(i1) + factor*(PLAT(i2)-PLAT(i1))
    dplon = PLON(i2) - PLON(i1)
    if (dplon > 180.0d0) then
      dplon = dplon - 360.0d0
    end if
    if (dplon < -180.0d0) then
      dplon = dplon + 360.0d0
    end if
    pilon = PLON(i1) + factor*dplon
    ptemp = PTMP(i1) + factor*(PTMP(i2)-PTMP(i1))
    puwin = PUWN(i1) + factor*(PUWN(i2)-PUWN(i1))
    pvwin = PVWN(i1) + factor*(PVWN(i2)-PVWN(i1))
    !---    Power-law interpolation for density (unless profile density     PTRP 53
    !       is zero, for which zero weight will be used)                    PTRP 54
    pdens = 0.0d0
    if (PDEN(i1) > 0.0d0) then
      pdens = PDEN(i1) * (PDEN(i2)/PDEN(i1))**factor
    end if
    !---    Power-law interpolation for pressure (unless profile pressure   PTRP 58
    !       is zero, for which zero weight will be used)                    PTRP 59
    ppres = 0.0d0
    if (PPRS(i1) > 0.0d0) then
      ppres = PPRS(i1) * (PPRS(i2)/PPRS(i1))**factor
    end if
    !---    Initialize weighting factor components for height and lat-lon   PTRP 63
    facthgt = 1.0d0
    factll = 1.0d0
    if (i1 == 1) then
      !---    Sine-squared variation of height weighting from 0 at 1st point  PTRP 67
      !       to 1 at 2nd point                                               PTRP 68
      facthgt = (chgt-PHGT(1)) / (PHGT(2)-PHGT(1))
      facthgt = (sin(pi2*facthgt))**2
    elseif (i2 == nprof) then
      !---    Sine-squared variation of height weighting from 0 at next-to-   PTRP 72
      !       last point to 1 at last point                                   PTRP 73
      facthgt = (chgt-PHGT(nprof)) / (PHGT(nprof-1)-PHGT(nprof))
      facthgt = (sin(pi2*facthgt))**2
    end if
    !---    Compute absolute lat-lon difference of current position from    PTRP 77
    !       profile lat-lon                                                 PTRP 78
    dlat = abs(clat-pilat)
    dlon = abs(clon-pilon)
    !---    Adjust lon difference for wrap at lon 360                       PTRP 81
    if (dlon > 180.0d0) then
      dlon = 360.0d0 - dlon
    end if
    !---    Lat-lon radius of current position from profile lat-lon         PTRP 83
    radius = sqrt(dlat**2+dlon**2)
    !---    Use weight=0 if radius>proffar, weight=1 if radius<profnear,    PTRP 85
    !       with sine-squared variation between proffar and profnear        PTRP 86
    if (radius >= proffar) then
      factll = 0.0d0
    elseif (radius <= profnear) then
      factll = 1.0d0
    else
      factll = (proffar-radius) / (proffar-profnear)
      factll = (sin(pi2*factll))**2
    end if
    !---    Total weight = product of weights for lat-lon and height        PTRP 95
    profwgt = factll * facthgt
  end if
  tpdwgt = profwgt
  uvwgt = profwgt
  !---    Set profile weight to zero for p,d, & t if profile values are 0 PTRP 99
  if (ptemp*ppres*pdens == 0.0d0) then
    tpdwgt = 0.0d0
  end if
  !---    Set profile weight to zero for u & v if profile values are 0    PTRP101
  if (abs(puwin)+abs(pvwin) == 0.0d0) then
    uvwgt = 0.0d0
  end if
  !---    Apply weighted averaging of profile values with input values    PTRP103
  ptemp = tpdwgt*ptemp + (1.0d0-tpdwgt)*tin
  ppres = tpdwgt*ppres + (1.0d0-tpdwgt)*pin
  pdens = tpdwgt*pdens + (1.0d0-tpdwgt)*din
  puwin = uvwgt*puwin + (1.0d0-uvwgt)*uin
  pvwin = uvwgt*pvwin + (1.0d0-uvwgt)*vin
end subroutine ProfTerp_M05
!---------------------------------------------------------------------- PTRP111
subroutine RdProf_M05(profile,nprof,LonEast)
  !---    Reads alternate profile data file profile. Returns number of    RDPF  2
  !       lines of data (nprof).  Converts input longitudes from East to  RDPF  3
  !       West if LonEast = 1                                             RDPF  4
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: npmax = 100000
  ! 
  !.. Formal Arguments .. 
  character(LEN=256), intent(in) :: profile
  integer, intent(out) :: nprof
  integer, intent(in) :: LonEast
  ! 
  !.. Local Scalars .. 
  character :: dummy
  integer :: lenprof,n
  real(kind=PM_REEL) :: d,p,t,u,v,xlat,xlon,zhgt
  ! 
  !.. Intrinsic Functions .. 
  intrinsic index
  ! 
  !.. Common Blocks .. 
  common /PTERP_M05/ PHGT,PLAT,PLON,PTMP,PPRS,PDEN,PUWN,PVWN
  !     For phgt: Maybe Read, Maybe Written
  !     For plat to pvwn: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /pterp_M05/ ... 
  real(kind=PM_REEL), dimension(npmax) :: &
    PDEN,PHGT,PLAT,PLON,PPRS,PTMP,PUWN,PVWN
  ! 
  ! ... Executable Statements ...
  ! 
  !---    Compute string length for profile file name                     RDPF 11
  lenprof = index(profile," ") - 1
  if (lenprof<1 .or. lenprof>60) then
    lenprof = 60
  end if
  !---    Open profile data file                                          RDPF 14
  open (33,file = profile(1:lenprof),status = "old")
  !---    Read and ignore header line                                     RDPF 16
  read (33,10000) dummy
  n = 0
  do
    !---    Start of loop to read profile data                              RDPF 20
    read (33,*,End = 1000) zhgt, xlat, xlon, t, p, d, u, v
    !---    Convert negative longitudes                                     RDPF 22
    if (xlon < 0.0d0) then
      xlon = xlon + 360.0d0
    end if
    !---    Convert to West Longitude if LonEast = 1                        RDPF 24
    if (LonEast == 1) then
      xlon = 360.0d0 - xlon
    end if
    !---    Count number of lines read                                      RDPF 26
    n = n + 1
    !---    Store profile data in arrays, for common pterp                  RDPF 28
    PHGT(n) = zhgt
    !---    Stop if two successive heights are the same                     RDPF 30
    if (n>1 .and. PHGT(n)==PHGT(n-1)) then
      write (*,*) n, PHGT(n)
      stop " Consecutive profile heights cannot be same"
    else
      PLAT(n) = xlat
      PLON(n) = xlon
      PTMP(n) = t
      PPRS(n) = p
      PDEN(n) = d
      PUWN(n) = u
      PVWN(n) = v
      !---    Cycle back to read another line of profile data                 RDPF 41
      nprof = n
    end if
  end do
  !---    Close profile input file when end-of-file encountered           RDPF 43
  1000 close (33)
  return
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format (a1)
end subroutine RdProf_M05
!---------------------------------------------------------------------- RDPF 47
!----------- END OF FILE  : TESsubs_M05.F90  --------------------------
!----------------------------------------------------------------------






!----------------------------------------------------------------------
!---------- START OF FILE   : setup_M05.F90 ---------------------------
!----------------------------------------------------------------------

subroutine Setup_M05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,RHOw,DHGT,DLAT, &
                     DLON,DTIME,MAXNUM,NRN1,NMCR1,dsunLs,dradau,dowlt,LnEW, &
                     INPUTFL,iustdout,iulist,hgtasfc,InERT,InUTC,stepmin, &
                     profnr,proffr,nprof,DIR)
! DM 1359: adding DIR for GRAM_05 integration
  !--------------------------------------------------------------------
  ! Parameters returned by Setup_M05 function:                              
  !--------------------------------------------------------------------
  !     Input is read by Setup_M05 routine in NAMELIST form.  Example:   
  !                                                                      
  ! $INPUT                                                                
  !  LSTFL    = 'LIST.txt'               ! List file name (CON for        
  !                                      !   console listing)             
  !  OUTFL    = 'OUTPUT.txt'             ! Output file name               
  !  TRAJFL   = 'TRAJDATA.txt'           ! (Optional) Trajectory input    
  !                                      !   file name                  
  !  profile  = 'null'                   ! (Optional) auxiliary profile   
  !                                      !   input file name            
  !  WaveFile = 'null'                   ! (Optional) file for time-  
  !                                      !   dependent wave model data    
  !  DATADIR  = 'C:\Mars\Mars2005\'      ! Directory for COSPAR data and  
  !                                      !   topographic height data      
  !  GCMDIR   = 'C:\Mars\Mars2005\'      ! Directory for GCM binary data  
  !                                      !   files                        
  !  IERT     = 1        ! 1 for time input as Earth-Receive time (ERT)
  !                      !   or 0 Mars-event time (MET)                   
  !  IUTC     = 1        ! 1 for time input as Coordinated Universal Time 
  !                      !   (UTC), or 0 for Terrestrial (Dynamical) Time 
  !                      !   (TT)                                        
  !  MONTH    = 7        ! month of year                                
  !  MDAY     = 20       ! day of month                                 
  !  MYEAR    = 76       ! year (4-digit; 1970-2069 can be 2-digit)     
  !  NPOS     = 21       ! max # positions to evaluate (0 = read data     
  !                      !                   from trajectory input file)  
  !  IHR       = 0       ! Hour of day (ERT or MET, controlled by IERT    
  !                      !   and UTC or TT, controlled by IUTC)          
  !  IMIN      = 0       ! minute of hour (meaning controlled by IERT and
  !                      !   IUTC)                                      
  !  SEC       = 0.0     ! seconds of minute (meaning controlled by IERT  
  !                      !   and IUTC).  IHR:IMIN:SEC is time for initial 
  !                      ! position to be evaluated                      
  !  LonEW    = 0        ! 0 for input and output West longitudes         
  !                      !   positive; 1 for East longitudes positive    
  !  Dusttau  = 0.3      ! Optical depth of background dust level (no   
  !                      !   time-developing dust storm, just uniformly  
  !                      !   mixed dust), 0.1 to 3.0, or use 0 for       
  !                      !   assumed seasonal variation of background     
  !                      !   dust                                         
  !  Dustmin  = 0.3      ! Minimum seasonal dust tau if input Dusttau=0  
  !                      !   (>=0.1)                                    
  !  Dustmax  = 1.0      ! Maximum seasonal dust tau if input Dusttau=0 
  !                      !   (<=3.0)                                     
  !  Dustnu   = 0.003    ! Parameter for vertical distribution of dust    
  !                      !   density (Haberle et al., J. Geophys. Res.,  
  !                      !   104, 8957, 1999)                            
  !  Dustdiam = 5.0      ! Dust particle diameter (micrometers, assumed 
  !                      !   monodisperse)                             
  !  Dustdens = 3000.    ! Dust particle density (kg/m**3)               
  !  ALS0     = 0.0      ! starting Ls value (degrees) for dust storm     
  !                      !                                   (0 = none) 
  !  INTENS   = 0.0      ! dust storm intensity (0.0 - 3.0). Storm       
  !                      !   intensity (>0) is added to Dusttau.          
  !  RADMAX   = 0.0      ! max. radius (km) of dust storm (0 or           
  !                      !                             >10000 = global)   
  !  DUSTLAT  = 0.0      ! Latitude (degrees) for center of dust storm    
  !  DUSTLON  = 0.0      ! Longitude (degrees) (West positive if LonEW =  
  !                      !   0 ,or East positive if LonEW = 1) for center 
  !                      !   of dust storm                                
  !  MapYear  = 1        ! 1 or 2 for TES mapping year 1,2 GCM input data 
  !                      !   or 0 for Mars-GRAM 2001 GCM input data sets  
  !  F107     = 68.0     ! 10.7 cm solar flux (10**-22 W/cm**2 at 1 AU)   
  !  STDL     = 0.0      ! std. dev. for thermosphere variation (-3.0    
  !                      !                                     to +3.0)  
  !  NR1      = 1001     ! starting random number (0 < NR1 < 30000)       
  !  NVARX    = 1        ! x-code for plotable output (1=hgt above MOLA  
  !                      !   areoid). See file xycodes.txt                
  !  NVARY    = 0        ! y-code for 2-D plotable output (0 for 1-D     
  !                      !                                       plots)  
  !  LOGSCALE = 0        ! 0=regular SI units, 1=log-base-10 scale,      
  !                      !    2=percentage deviations from COSPAR model,  
  !                      !    3=SI units with density in kg/km**3        
  !  FLAT     = 22.0     ! initial latitude (N positive), degrees       
  !  FLON     = 48.0     ! initial longitude (West positive if LowEW = 0 
  !                      !   or East positive if LonEW = 1), degrees     
  !  FHGT     = -5.0     ! initial height (km) (<=-10 means use surface)
  !  MOLAhgts = 1        ! 1 for input heights relative to MOLA areoid;  
  !                      !   otherwise input heights are relative to      
  !                      !   reference ellipsoid                          
  !  hgtasfcm = 0.0      ! height above surface (0-1000 m); use if FHGT   
  !                      !    <= -10. km                                  
  !  zoffset  = 5.0      ! constant height offset (km) for MTGCM data   
  !                      !  or constant part of Ls-dependent (Bougher)   
  !                      !  height offset (0.0 means no constant offset). 
  !                      !  Positive offset increases density, negative  
  !                      !  offset decreases density.                     
  !  ibougher = 2        ! 0 for no Ls-dependent (Bougher) height offset 
  !                      !  term; 1 means add Ls-dependent (Bougher)    
  !                      !  term, -A*Sin(Ls) (km), to constant term     
  !                      !  (zoffset) [A=2.5 for MapYear=0 or 0.5 for    
  !                      !  MapYear>0]; 2 means use global mean height   
  !                      !  offset from data file hgtoffst.dat; 3 means   
  !                      !  use daily average height offset at local      
  !                      !  position; 4 means use height offset at        
  !                      !  current time and local position. Value of   
  !                      !  zoffset is ignored if ibougher = 2, 3, or 4   
  !  DELHGT   = 10.0     ! height increment (km) between steps          
  !  DELLAT   = 0.0      ! latitude increment (deg) between steps      
  !  DELLON   = 0.0      ! Longitude increment (deg) between steps (West  
  !                      !   positive if LonEW = 0, East positive if      
  !                      !   LonEW = 1)                                  
  !  DELTIME  = 0.0      ! time increment (sec) between steps            
  !  deltaTEX = 0.0      ! adjustment for exospheric temperature (K)    
  !  profnear = 0.0      ! Lat-lon radius within which weight for        
  !                      !   auxiliary profile is 1.0                    
  !  proffar  = 0.0      ! Lat-lon radius beyond which weight for        
  !                      !   auxiliary profile is 0.0                     
  !  rpscale  = 1.0      ! random density perturbation scale factor (0-2) 
  !  rwscale  = 1.0      ! random wind perturbation scale factor (>=0)
  !  wlscale  = 1.0      ! scale factor for perturbation wavelengths    
  !                      !   (0.1-10)                                  
  !  wmscale  = 1.0      ! scale factor for mean winds                   
  !  blwinfac = 1.0      ! scale factor for boundary layer slope winds   
  !                      !   (0 = none)                                  
  !  NMONTE   = 1        ! number of Monte Carlo runs                     
  !  iup      = 11       ! 0 for no LIST and graphics output, unit number
  !                      !  for LIST file otherwise                       
  !  corlmin   = 0.0     ! Minimum relative step size for perturbations 
  !                      !  (0.0 - 1.0)                                  
  !  WaveA0   = 1.0      ! Mean term of longitude-dependent wave         
  !                      !   multiplier for density                   
  !  WaveDate = 0.0      ! Julian day for (primary) peak(s) of wave      
  !                      !   (0 for no traveling component)              
  !  WaveA1   = 0.0      ! Amplitude of wave-1 component of longitude-    
  !                      !   dependent wave multiplier for density       
  !  Wavephi1 = 0.0      ! Phase of wave-1 component of longitude-     
  !                      !   dependent wave multiplier (longitude, with   
  !                      !   West positive if LonEW = 0, East positive   
  !                      !   if LonEW = 1)                                
  !  phi1dot  = 0.0      ! Rate of longitude movement (degrees per day)
  !                      !   for wave-1 component (Westward positive if  
  !                      !   LonEW = 0, Eastward positive if LonEW = 1)   
  !  WaveA2   = 0.0      ! Amplitude of wave-2 component of longitude-  
  !                      !   dependent wave multiplier for density      
  !  Wavephi2 = 0.0      ! Phase of wave-2 component of longitude-      
  !                      !   dependent wave multiplier (longitude, with   
  !                      !   West positive if LonEW = 0, East positive   
  !                      !   if LonEW = 1)                                
  !  phi2dot  = 0.0      ! Rate of longitude movement (degrees per day)   
  !                      !   for wave-2 component (Westward positive if 
  !                      !   LonEW = 0, Eastward positive if LonEW = 1)  
  !  WaveA3   = 0.0      ! Amplitude of wave-3 component of longitude- 
  !                      !   dependent wave multiplier for density     
  !  Wavephi3 = 0.0      ! Phase of wave-3 component of longitude-       
  !                      !   dependent wave multiplier (longitude, with   
  !                      !   West positive if LonEW = 0, East positive  
  !                      !   if LonEW = 1)                                
  !  phi3dot  = 0.0      ! Rate of longitude movement (degrees per day)   
  !                      !   for wave-3 component (Westward positive if   
  !                      !   LonEW = 0, Eastward positive if LonEW = 1)   
  !  iuwave   = 0        ! Unit number for (Optional) time-dependent wave
  !                      !    coefficient data file (or 0 for none)       
  !  Wscale   = 20.      ! Vertical scale (km) of longitude-dependent     
  !                      !    wave damping at altitudes below 100 km      
  !                      !    (10<=Wscale<=10,000 km)                    
  !  ipclat   = 1        ! 1 = Planeto-centric latitude and height input,
  !                      ! 0 = Planeto-graphic latitude and height input 
  !  requa    = 3396.19  ! Equatorial radius (km) for reference ellipsoid
  !  rpole    = 3376.20  ! Polar radius (km) for reference ellipsoid     
  !  idaydata = 1        ! 1 = output daily max/min data; 0 = none      
  ! $END                                                                    
  !                                                                       
  !...  Setup_M05 information for start of run        
  !---------------------------------------------------------------------- 
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Parameters .. 
  integer, parameter :: nmtlat = 361
  integer, parameter :: nmtlon = 721
  integer, parameter :: nalblat = 181
  integer, parameter :: nalblon = 361
  integer, parameter :: nbl = 3
  integer, parameter :: ndust = 3
  integer, parameter :: nf10 = 2
  integer, parameter :: ntesy = 2
  integer, parameter :: ntf10 = 3
  character, parameter :: version = "1"
  ! 
  !.. Formal Arguments .. 
  !character(LEN=256), intent(in) :: INPUTFL
  character*(*) :: DIR
   character*(*) :: INPUTFL
  integer, intent(in) :: iustdout
  integer, intent(out) :: InERT,InUTC,LnEW,MAXNUM,NMCR1,NRN1,iulist,nprof
  real(kind=PM_REEL), intent(in) :: dowlt,dradau,dsunLs
  real(kind=PM_REEL), intent(out) :: &
    CHGT,CLAT,CLON,CSEC,DAY0,DHGT,DLAT,DLON,DTIME,RHOd,RHOu,RHOv,RHOw,hgtasfc, &
    proffr,profnr,stepmin
  ! 
  !.. Local Scalars .. 
  character :: EWlon,dummy
  character(LEN=5) :: LatLbl
  character(LEN=7) :: HgtLbl
  character(LEN=8) :: DensLbl
  character(LEN=256) :: DATADIR = "null",GCMDIR = "null", &
                       TRAJFL = "TRAJDATA.txt",WaveFile = "null", &
                       profile = "null"
  integer :: IERR1,IERR10,IERR11,IERR12,IERR13,IERR2,IERR3,IERR4,IERR5,IERR6, &
             IERR7,IERR8,IERR9,IERT,IUTC,L,LonEW,NMONTE,NR1,i,idterr,inpclat, &
             ioerr,j,lendir,ls
  integer :: Ihr = 12,Imin = 30,Mday = 20,Month = 7,Myear = 76
  real(kind=PM_REEL) :: ALS
  real(kind=PM_REEL) :: Dellon = 0.0d0,Flat = 22.0d0
  real(kind=PM_REEL) :: MARSAU
  real(kind=PM_REEL) :: Delhgt = 10.0d0,Dellat = 0.0d0,Deltime = 0.0d0, &
                      Fhgt = -0.5d0,Flon = 48.0d0,Sec = 0.0d0,deltaTEX = 0.0d0
  real(kind=PM_REEL) :: EOT,blwfactor,corlmin,hgtasfcm,proffar,profnear,requat, &
                      rpoles,sunlat,ttsec,wlfactor
  real(kind=PM_REEL) :: rpscale = 1.0d0
  real(kind=PM_REEL) :: dt,owlt,rwfactor,sunlon,wmfactor,z1
  ! 
  !.. Local Arrays .. 
  character(LEN=256), dimension(13) :: FILES
  integer, dimension(13) :: IERR
  ! 
  ! DM 1359 remove external : private  
  !.. External Calls .. 
  !external CaltoJul_M05, RdProf_M05, RdTESmgcm_M05, RdTESsrf_M05, &
  !         RdTEStgcm_M05, ReadMGCM_M05, ReadTES_M05, ReadTGCM_M05, &
  !         Readsurf_M05, chkdt_M05, marsephm_M05
  ! 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: PPND_M05
  !real(kind=PM_REEL), external :: RANDOM_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic atan, index, mod, tan
  ! 
  !.. Common Blocks .. 
  common /COSPARNH_M05/ ZC,TC,PC,DC
  !     For zc to dc: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /cosparnh_M05/ ... 
  real(kind=PM_REEL), dimension(164) :: DC,PC,TC,ZC
  common /DATACOM_M05/ DTR,DAY,DUSTLAT,DUSTLON,RADMAX,RREF,ALS0,ALSDUR,INTENS, &
                       DTEX,RPFACTOR,DUSTTAU,DUSTMIN,DUSTMAX,DUSTOD,DUSTNU, &
                       DUSTDIAM,DUSTDENS,RWSCALE,WLSCALE,REQUA,RPOLE,WMSCALE, &
                       BLWINFAC,MAPYEAR,IDAYDATA,NPOS,NVARX,NVARY,LOGSCALE, &
                       IU0,IUP,IPCLAT,MOLAHGTS
  !     For DTR: Not Read, Maybe Written
  !     For DAY to radmax: Not Read, Overwritten
  !     For Rref: Not Read, Not Written
  !     For als0 to intens: Not Read, Overwritten
  !     For dTEX to rpfactor: Not Read, Maybe Written
  !     For Dusttau to Dustmax: Not Read, Overwritten
  !     For DustOD: Not Read, Not Written
  !     For Dustnu to wmscale: Not Read, Overwritten
  !     For blwinfac: Maybe Read, Maybe Written
  !     For MapYear to MOLAhgts: Not Read, Overwritten
  ! 
  !... Variables in Common Block /DATACOM_M05/ ... 
  real(kind=PM_REEL) :: DAY,DTR,DUSTOD,DUSTDENS,DUSTDIAM,DUSTMAX,DUSTMIN,DUSTNU, &
                      DUSTTAU,RREF,ALS0,ALSDUR,BLWINFAC,DTEX,DUSTLAT,DUSTLON, &
                      INTENS,RADMAX,REQUA,RPFACTOR,RPOLE,RWSCALE,WLSCALE, &
                      WMSCALE
  integer :: MOLAHGTS,MAPYEAR,NPOS,NVARX,NVARY,IDAYDATA,IPCLAT,IU0,IUP, &
             LOGSCALE
  common /FILENAME_M05/ LSTFL,OUTFL
  !     For lstfl to outfl: Not Read, Overwritten
  ! 
  !... Variables in Common Block /FILENAME_M05/ ... 
  character(LEN=256) :: LSTFL,OUTFL
  common /MGCMPARM_M05/ DUST,DZBL,ZWSFC,F10VAL,F10TES,DUSTC,SOLACT,TESYR, &
                        SOLTES
  !     For dust: Maybe Read, Maybe Written
  !     For dzbl: Not Read, Maybe Written
  !     For zwsfc: Not Read, Overwritten
  !     For f10val to f10TES: Not Read, Maybe Written
  !     For dustc to solTES: Maybe Read, Maybe Written
  ! 
  !... Variables in Common Block /MGCMparm_M05/ ... 
  real(kind=PM_REEL), dimension(ndust) :: DUST
  real(kind=PM_REEL), dimension(nbl) :: DZBL
  real(kind=PM_REEL) :: ZWSFC
  real(kind=PM_REEL), dimension(nf10) :: F10VAL
  real(kind=PM_REEL), dimension(ntf10) :: F10TES
  character(LEN=2), dimension(ndust) :: DUSTC
  character(LEN=2), dimension(nf10) :: SOLACT
  character(LEN=2), dimension(ntesy) :: TESYR
  character(LEN=2), dimension(ntf10) :: SOLTES
  common /RANDCOM_M05/ IX,IY,IZ
  !     For IX to IZ: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /RANDCOM_M05/ ... 
  integer :: IX,IY,IZ
  common /TERHGT_M05/ AREORAD,TOPOMOLA,ALBEDO
  !     For areorad to albedo: Not Read, Maybe Written
  ! 
  !... Variables in Common Block /TERHGT_M05/ ... 
  real(kind=PM_REEL), dimension(0:nmtlat,0:nmtlon) :: AREORAD,TOPOMOLA
  real(kind=PM_REEL), dimension(0:nalblat,0:nalblon) :: ALBEDO
  common /TGCMOFFSET_M05/ OFFSETS,TOFFSETS,ZOFFSET,HGTOFFSET,OFSZL,IBOUGHER
  !     For offsets to toffsets: Not Read, Maybe Written
  !     For zoffset: Not Read, Overwritten
  !     For hgtoffset to ofszL: Not Read, Not Written
  !     For ibougher: Not Read, Overwritten
  ! 
  !... Variables in Common Block /TGCMoffset_M05/ ... 
  real(kind=PM_REEL), dimension(0:12,ndust) :: OFFSETS
  real(kind=PM_REEL), dimension(0:12,ntesy) :: TOFFSETS
  real(kind=PM_REEL) :: HGTOFFSET,OFSZL,ZOFFSET
  integer :: IBOUGHER
  common /THERM_M05/ F107,STDL,FMOL
  !     For F107 to stdl: Not Read, Overwritten
  !     For fmol: Not Read, Not Written
  ! 
  !... Variables in Common Block /THERM_M05/ ... 
  real(kind=PM_REEL) :: F107,STDL
  real(kind=PM_REEL), dimension(0:8) :: FMOL
  common /WAVECOEF_M05/ WAVEA0,WAVEA1,WAVEPHI1,WAVEA2,WAVEPHI2,WAVEA3, &
                        WAVEPHI3,WAVETIME,WAVEDATA,WSCALE,PHI1DOT,PHI2DOT, &
                        PHI3DOT,WAVEDATE,NWAVE,IUWAVE
  !     For WaveA0 to Wavephi3: Not Read, Overwritten
  !     For wavetime to wavedata: Maybe Read, Maybe Written
  !     For Wscale to WaveDate: Not Read, Overwritten
  !     For nwave: Not Read, Maybe Written
  !     For iuwave: Not Read, Overwritten
  ! 
  !... Variables in Common Block /Wavecoef_M05/ ... 
  real(kind=PM_REEL) :: WAVEA0,WAVEA1,WAVEA2,WAVEA3,WAVEPHI1,WAVEPHI2,WAVEPHI3
  real(kind=PM_REEL), dimension(100) :: WAVETIME
  real(kind=PM_REEL), dimension(100,11) :: WAVEDATA
  real(kind=PM_REEL) :: WAVEDATE,WSCALE,PHI1DOT,PHI2DOT,PHI3DOT
  integer :: IUWAVE,NWAVE
  ! 
  !.. Equivalences .. 
  equivalence (IERR13,IERR(13)), (IERR12,IERR(12)), (IERR11,IERR(11)), &
              (IERR10,IERR(10)), (IERR9,IERR(9)), (IERR8,IERR(8)), &
              (IERR7,IERR(7)), (IERR6,IERR(6)), (IERR5,IERR(5)), &
              (IERR4,IERR(4)), (IERR3,IERR(3)), (IERR2,IERR(2)), &
              (IERR1,IERR(1))
  ! 
  !.. Namelist Declarations .. 
  namelist /INPUT/ LSTFL, OUTFL, TRAJFL, profile, WaveFile, DATADIR, IERT, &
                   IUTC, GCMDIR, Month, Mday, Myear, NPOS, Ihr, Imin, Sec, &
                   LonEW, DUSTTAU, DUSTMIN, DUSTMAX, DUSTNU, DUSTDIAM, &
                   DUSTDENS, ALS0, ALSDUR, INTENS, RADMAX, DUSTLAT, DUSTLON, &
                   F107, STDL, NR1, NVARX, NVARY, LOGSCALE, Flat, Flon, Fhgt, &
                   MOLAHGTS, hgtasfcm, ZOFFSET, IBOUGHER, Delhgt, Dellat, &
                   Dellon, Deltime, deltaTEX, profnear, proffar, rpscale, &
                   RWSCALE, WLSCALE, WMSCALE, BLWINFAC, NMONTE, IUP, WAVEA0, &
                   WAVEDATE, WAVEA1, WAVEPHI1, PHI1DOT, WAVEA2, WAVEPHI2, &
                   PHI2DOT, WAVEA3, WAVEPHI3, PHI3DOT, IUWAVE, WSCALE, &
                   corlmin, IPCLAT, REQUA, RPOLE, MAPYEAR, IDAYDATA
  ! 
  !.. Data Declarations .. 
  data FILES/ &
       "LIST.txt","Density.txt","Perturb.txt","Winds.txt","TPresHgt.txt", &
       "DayData.txt","ThrmData.txt","molatoph.bin","COSPAR2.DAT","OUTPUT.txt", &
       "hgtoffst.dat","albedo1.bin","MarsRad.txt"/
  ! 
  ! ... Executable Statements ...
  ! 
  !.....................................................................  SETU 95
  !                                                                       SETU 96
  !...  Default Mapping Year = 1                                          SETU 97
  MAPYEAR = 1
  !...  Default profile parameters                                        SETU 99
  profnear = 0.0d0
  proffar = 0.0d0
  !...  Default daily max/min data                                        SETU102
  IDAYDATA = 1
  !...  Default perturbation factors                                      SETU104
  RWSCALE = 1.0d0
  WLSCALE = 1.0d0
  !...  Default mean wind scale factor                                    SETU107
  WMSCALE = 1.0d0
  !...  Default corlmin value                                             SETU109
  corlmin = 0.0d0
  !...  Default time options IERT = 1 for Earth-receive time and IUTC =   SETU111
  !     1 for UTC time (not Terrestrial Dynamical Time)                   SETU112
  IERT = 1
  IUTC = 1
  !...  Default planetry radii and lat/height input                       SETU115
  REQUA = 3396.19d0
  RPOLE = 3376.20d0
  IPCLAT = 1
  !...  Default with no wave model modification                           SETU119
  WAVEA0 = 1.0d0
  WAVEDATE = 0.0d0
  WAVEA1 = 0.0d0
  WAVEPHI1 = 0.0d0
  PHI1DOT = 0.0d0
  WAVEA2 = 0.0d0
  WAVEPHI2 = 0.0d0
  PHI2DOT = 0.0d0
  WAVEA3 = 0.0d0
  WAVEPHI3 = 0.0d0
  PHI3DOT = 0.0d0
  IUWAVE = 0
  WSCALE = 20.0d0
  !...  Set dust optical depths for GCM data                              SETU133
  DUST(1) = 0.3d0
  DUST(2) = 1.0d0
  DUST(3) = 3.0d0
  DUSTC(1) = "03"
  DUSTC(2) = "10"
  DUSTC(3) = "30"
  !...  Set heights for GCM boundary layer data                           SETU140
  DZBL(1) = 0.0d0
  DZBL(2) = 0.005d0
  DZBL(3) = 0.030d0
  !...  Set surface roughness parameter (m).  Value 1 cm (0.01 m) is      SETU144
  !     consistent with NASA Ames MGCM boundary layer model               SETU145
  ZWSFC = 0.01d0
  !...  Set solar activity values (F10.7 at 1AU)                          SETU147
  F10VAL(1) = 70.0d0
  F10VAL(2) = 130.0d0
  SOLACT(1) = "ls"
  SOLACT(2) = "ms"
  !...  Set values for TES year 1 and year 2 data files                   SETU152
  TESYR(1) = "y1"
  TESYR(2) = "y2"
  SOLTES(1) = "ls"
  SOLTES(2) = "ms"
  SOLTES(3) = "hs"
  F10TES(1) = 70.0d0
  F10TES(2) = 130.0d0
  F10TES(3) = 200.0d0
  !...  Set unit number for screen I/O and pass it into Common            SETU161
  IU0 = iustdout
  !...  default list and output files                                     SETU163
  LSTFL = "LIST.txt"
  OUTFL = "OUTPUT.txt"
  !...  Default number of positions                                       SETU166
  NPOS = 21
  !...  Default use West Longitude positive (USGS Convention)             SETU168
  LonEW = 0
  !...  Default background dust optical depth, and min & max              SETU170
  DUSTTAU = 0.3d0
  DUSTMIN = 0.3d0
  DUSTMAX = 1.0d0
  !...  Default dust particle density parameters                          SETU174
  !     See Fig. 2 of Haberle et al., J. geophys. Res., vol 104, p. 8957  SETU175
  !     (1999) and Haberle et al., Icarus, vol 50, p. 322 (1982)          SETU176
  DUSTNU = 0.003d0
  DUSTDIAM = 5.0d0
  DUSTDENS = 3000.0d0
  !...  Default no dust storm                                             SETU180
  ALS0 = 0.0d0
  ALSDUR = 48.0d0
  INTENS = 0.0d0
  RADMAX = 0.0d0
  DUSTLAT = 0.0d0
  DUSTLON = 0.0d0
  !...  Default Solar Flux parameters                                     SETU187
  F107 = 68.0d0
  STDL = 0.0d0
  !...  Default plot variable = height above MOLA areoid                  SETU190
  NVARX = 1
  NVARY = 0
  !...  Default to regular linear scale                                   SETU193
  LOGSCALE = 0
  !...  Default random number seed and Number of Monte Carlo runs         SETU195
  NR1 = 1001
  NMONTE = 1
  !...  Default unit number for print output data file                    SETU198
  IUP = 13
  !...  Set length of Mars day                                            SETU200
  DAY = 24.622962d0
  !...  Default to input heights above MOLA areoid                        SETU202
  MOLAHGTS = 1
  !...  Default height above surface                                      SETU204
  hgtasfcm = 0.0d0
  !...  Default (global) MTGCM height offset                              SETU206
  ZOFFSET = 5.0d0
  IBOUGHER = 2
  !...  Open Namelist data file                                           SETU209
  open (8,file = INPUTFL,status = "old",iostat = ioerr)
  if (ioerr /= 0) then
    write (iustdout,*) " Error opening NAMELIST input file"
    stop
  end if
  !...  Read Namelist data                                                SETU215
  read (8,INPUT)
  close (8)
  !.....................................................................  SETU218
  !     For compilers not supporting the NAMELIST input mode, the         SETU219
  !     previous Read statement may be replaced by:                       SETU220
  !                                                                       SETU221
  !     Read(8,10)LSTFL                                                   SETU222
  !     Read(8,10)OUTFL                                                   SETU223
  !     Read(8,10)TRAJFL                                                  SETU224
  !     Read(8,10)WaveFile                                                SETU225
  !     Read(8,10)DATADIR                                                 SETU226
  !     Read(8,10)GCMDIR                                                  SETU227
  !     Read(8,10)profile                                                 SETU228
  ! 10  Format(A)                                                         SETU229
  !     Read(8,*)IERT,IUTC,MONTH,MDAY,MYEAR,NPOS,IHR,IMIN,SEC,LonEW,      SETU230
  !    & Dusttau,Dustmin,Dustmax,Dustnu,Dustdiam,Dustdens,ALS0,ALSDUR,    SETU231
  !    & INTENS,RADMAX,DUSTLAT,DUSTLON,F107,STDL,NR1,NVARX,NVARY,         SETU232
  !    & LOGSCALE,FLAT,FLON,FHGT,MOLAhgts,hgtasfcm,zoffset,ibougher,      SETU233
  !    & DELHGT,DELLAT,DELLON,DELTIME,deltaTEX,profnear,proffar,rpscale,  SETU234
  !    & rwscale,wlscale,wmscale,blwinfac,NMONTE,iup,WaveA0,WaveDate,     SETU235
  !    & WaveA1,Wavephi1,phi1dot,WaveA2,Wavephi2,phi2dot,WaveA3,          SETU236
  !    & Wavephi3,phi3dot,iuwave,Wscale,corlmin,ipclat,requa,rpole,       SETU237
  !    & MapYear,idaydata                                                 SETU238
  !                                                                       SETU239
  !     and the NAMELIST file INPUT may be modified to contain free-      SETU240
  !     field input data as in the above list.                            SETU241
  !.....................................................................  SETU242
  !...  Check that unit iup is in allowable range                         SETU243
  if ((IUP>=5.and.IUP<=12) .or. (IUP>=21.and.IUP<=29)) &
    stop " Unit iup conflict with another file"
  if (FLAT<-90.0d0 .or. FLAT>90.0d0) then
    write (IU0,10001)
    stop " Error termination! Check the LIST file for messages."
  end if
  if (FLON < 0.0d0) then
    FLON = FLON + 360.0d0
  end if
  if (FLON<0.0d0 .or. FLON>360.0d0) then
    write (IU0,10001)
    stop " Error termination! Check the LIST file for messages."
  end if
  !...  Store values for output arguments                                 SETU256
  profnr = profnear
  proffr = proffar
  LnEW = LonEW
  !...  Test profnear and proffar.  Read profile data if profnear > 0     SETU260
  if (profnear > 0.0d0) then
    if (proffar <= profnear) stop " proffar must be > profnear"
    call RdProf_M05(profile,nprof,LonEW)
  end if
  !...  Must use planeto-centric height and latitude if MOLAhgts = 1      SETU265
  if (MOLAHGTS == 1) then
    IPCLAT = 1
  end if
  iulist = IUP
  !...  Convert Height above surface to km and insure proper range        SETU268
  if (hgtasfcm < 0.0d0) then
    hgtasfcm = 0.0d0
  end if
  if (hgtasfcm > 4500.0d0) then
    hgtasfcm = 4500.0d0
  end if
  hgtasfc = hgtasfcm / 1000.0d0
  !...  Insure Wscale within proper range                                 SETU272
  if (WSCALE <= 10.0d0) then
    WSCALE = 10.0d0
  end if
  if (WSCALE >= 10000.0d0) then
    WSCALE = 10000.0d0
  end if
  !...  Set traveling wave parameters if WaveDate le 0                    SETU275
  if (WAVEDATE <= 0.0d0) then
    WAVEDATE = 0.0d0
    PHI1DOT = 0.0d0
    PHI2DOT = 0.0d0
    PHI3DOT = 0.0d0
  end if
  !...  Open and read WaveFile and load wavedata array if iuwave>0        SETU282
  if (IUWAVE > 0) then
    open (IUWAVE,File = WaveFile,iostat = ierr1)
    if (ierr1 /= 0) stop "  Error opening WaveFile"
    NWAVE = 1
    do
      !...    Each WaveFile record contains: (1) time at which wave model     SETU287
      !         coefficients first apply (1st record must start at time=0),   SETU288
      !         (2) WaveA0, (3) WaveDate, (4) WaveA1, (5) Wavephi1, (6)       SETU289
      !         phi1dot, (6), WaveA2, (7)Wavephi2, (8) phi2dot, (9) WaveA3,   SETU290
      !         (10) Wavephi3, (11) phi3dot.  Times for wave model            SETU291
      !          coefficients must be in ascending order.                     SETU292
      read (IUWAVE,*,End = 1000) &
           WAVETIME(NWAVE), (WAVEDATA(NWAVE,i), i = 1,11)
      if (NWAVE == 1) then
        if (WAVETIME(1) > 0.0d0) &
          stop " First wave data in file must be at time 0"
      elseif (WAVETIME(NWAVE) <= WAVETIME(NWAVE-1)) then
        stop " Wave data in file must in increasing order by time"
      end if
      if (WAVEDATA(NWAVE,1)<0.1d0 .or. WAVEDATA(NWAVE,1)>12.0d0) &
        stop " WaveA0 from input file is out of range"
      NWAVE = NWAVE + 1
    end do
    !...    Close wave data file                                            SETU305
    1000 close (IUWAVE)
    !...    Re-set nwave to number of wave data                             SETU307
    NWAVE = NWAVE - 1
    !...    Re-initialize wave coefficients                                 SETU309
    WAVEA0 = WAVEDATA(1,1)
    WAVEDATE = WAVEDATA(1,2)
    WAVEA1 = WAVEDATA(1,3)
    WAVEPHI1 = WAVEDATA(1,4)
    PHI1DOT = WAVEDATA(1,5)
    WAVEA2 = WAVEDATA(1,6)
    WAVEPHI2 = WAVEDATA(1,7)
    PHI2DOT = WAVEDATA(1,8)
    WAVEA3 = WAVEDATA(1,9)
    WAVEPHI3 = WAVEDATA(1,10)
    PHI3DOT = WAVEDATA(1,11)
  end if
  !...  Open and read TES dust optical depths 
  ! DM 1359: adding DIR 
  !  call ReadTES_M05(DATADIR,version)
   call ReadTES_M05(DIR,version)

  !...  Convert FLON, DUSTLON, DELLON to West if LonEW=1                  SETU324
  if (LonEW == 1) then
    DUSTLON = 360.0d0 - DUSTLON
    FLON = 360.0d0 - FLON
    DELLON = -DELLON
  end if
  !...  Insure corlmin input value within proper bounds                   SETU330
  if (corlmin < 0.0d0) then
    corlmin = 0.0d0
  end if
  if (corlmin > 1.0d0) then
    corlmin = 1.0d0
  end if
  RPFACTOR = rpscale
  if (rpscale<0.0d0 .or. rpscale>2.0d0) stop " Must have 0 <= rpscale <= 2"
  !...  Insure Dustmin and Dustmax in proper ranges                       SETU336
  if (DUSTMIN < 0.1d0) then
    DUSTMIN = 0.1d0
  end if
  if (DUSTMAX > 1.0d0) then
    DUSTMAX = 1.0d0
  end if
  if (WAVEA0<=0.1d0 .or. WAVEA0>12.0d0) stop " WaveA0 out of range"
  if (NMONTE < 1) then
    NMONTE = 1
  end if
  !...  Pass corlmin value to output                                      SETU342
  stepmin = corlmin
  !...  Pass 1st random number and Number Monte Carlo runs to output      SETU344
  NRN1 = NR1
  NMCR1 = NMONTE
  !...  Check option values and pass to output or Common DATACOM          SETU347
  if (WLSCALE < 0.1d0) then
    WLSCALE = 0.1d0
  end if
  if (WLSCALE > 10.0d0) then
    WLSCALE = 10.0d0
  end if
  if (BLWINFAC < 0.0d0) then
    BLWINFAC = 0.0d0
  end if
  if (RWSCALE < 0.0d0) then
    RWSCALE = 0.0d0
  end if
  InERT = IERT
  InUTC = IUTC
  inpclat = IPCLAT
  !...  Insure requa and rpole within bounds                              SETU355
  if (REQUA<3300.0d0 .or. REQUA>3500.0d0 .or. RPOLE<3300.0d0 .or. &
      RPOLE>3500.0d0) stop " requa or rpole out of bounds"
  requat = REQUA
  rpoles = RPOLE
  rwfactor = RWSCALE
  wlfactor = WLSCALE
  wmfactor = WMSCALE
  blwfactor = BLWINFAC
  !...  Insure MapYear within legal limits                                SETU364
  if (MAPYEAR < 0) then
    MAPYEAR = 0
  end if
  if (MAPYEAR > 2) then
    MAPYEAR = 2
  end if
  DHGT = DELHGT
  DLAT = DELLAT
  DLON = DELLON
  DTIME = DELTIME
  !                                                                       SETU371
  !     If output to the list file, output file, and plotable files       SETU372
  !     is not desired, the following statements may be removed.          SETU373
  !     Note, however, that the HEIGHTS.DAT file is required.             SETU374
  !     Note that output to list and other files can also be suppressed   SETU375
  !     by setting iup to 0 above, near line SETU153                      SETU376
  !                                                                       SETU377
  files(1) = LSTFL
  files(10) = OUTFL
  if (LSTFL/="CON" .and. LSTFL/="con" .and. IUP>0) then
    open (IUP,file = LSTFL,iostat = ierr1)
  end if
  if (NPOS <= 0) then
    !...  If NPOS = 0 is entered, program reads position data from          SETU393
    !      unit 7, trajectory data file                                     SETU394
    !...  Each trajectory file record contains time (seconds from initial   SETU395
    !      time), height (km), latitude (degrees, North positive), and      SETU396
    !      longitude (degrees, West positive if LonEW=0 or East positive    SETU397
    !      if LonEW=1).                                                     SETU398
    open (7,file = TRAJFL,status = "old",iostat = ierr7)
    if (ierr7 /= 0) then
      write (IU0,10002)
      stop " Error termination! Check the LIST file for messages."
    end if
  end if
  MAXNUM = NPOS - 1
  if (profnear > 0.0d0) then
    write (IU0,10003) profile
  end if
  if (IUWAVE > 0) then
    write (IU0,10007) WaveFile
  end if
  if (NPOS <= 0) then
    MAXNUM = 99999
  end if
  !...  Write version number and file names to LIST file                  SETU409
  if (IUP > 0) then
    write (IUP,10000) version
    write (IUP,10004) LSTFL, OUTFL
! DM 1359 : adding DIR
!    write (IUP,10008) DATADIR, GCMDIR
     write (IUP,10008) DIR, DIR
    if (NPOS == 0) then
      write (IUP,10006) TRAJFL
    end if
    if (profnear > 0.0d0) then
      write (IUP,10003) profile
    end if
    if (IUWAVE > 0) then
      write (IUP,10007) WaveFile
    end if
  end if
  !...  Files on units 21-27 contain parameters suitable for plotting.    SETU425
  !...  Data are in either of two forms: (1)  X  Y1 Y2 ..., where X       SETU426
  !...  is the variable to be plotted against (e.g. height), and Y1, Y2,  SETU427
  !...  etc. are variables to be plotted, or (2) X Y Z1 Z2 ..., where X   SETU428
  !...  and Y are two variables (e.g. latitude and height) to provide     SETU429
  !...  position for plotting contour plots of one of the variables       SETU430
  !...  Z1, Z2, etc.                                                      SETU431
  if (IUP > 0) then
    !...    Unit 21 file = 'Density.txt': Headers for variables are -       SETU433
    !         DENSLO  =  low (-1 sigma) density                             SETU434
    !         DENSAV  =  average (mean plus wave-perturbed) density         SETU435
    !         DENSHI  =  high (+1 sigma) density                            SETU436
    !         DENSTOT =  total (average+perturbed) density                  SETU437
    !           (density units kg/m**3, log-10 scale, % from COSPAR, or     SETU438
    !             kg/km**3, depending on value of LOGSCALE input parameter) SETU439
    !         DustOD  =  dust optical depth                                 SETU440
    !         Radius  =  Radial distance from planetary center of mass to   SETU441
    !                    spacecraft position (areoid radius plus altitude)  SETU442
    !         Grav    =  local acceleration of gravity (m/s**2)             SETU443
    !         RadAU   =  Mars orbital radius (Astronomical Units)           SETU444
    !        LOGSCALE =  option controlling units of density output         SETU445
    !        hgtoffset=  local height offset (km) for MTGCM and MGCM data   SETU446
    !        ibougher =  input parameter controlling height offset option   SETU447
    !        MapYear  =  TES mapping year (0 for Mars-GRAM 2001 data)       SETU448
    !        profwgt  =  Weight factor for auxiliary input profile data     SETU449
    !...                                                                    SETU450
    open (21,file = files(2),iostat = ierr2)
    if (NVARY == 0) then
      write (21,10009)
    else
      write (21,10010)
    end if
    !...    Unit 22 file = 'Perturb.txt': Headers for variables are:        SETU463
    !         SigD     = Density standard deviation (% of unperturbed mean) SETU464
    !         DensRand = Random density perturbation (% of unpert. mean)    SETU465
    !         DensWave = Density wave perturbation (% of unperturbed mean)  SETU466
    !         DensP    = Total density perturbation (% of unperturbed mean) SETU467
    !         corlim   = Ratio of step size to correlation accuracy limit   SETU468
    !                     (ideally should be 1.0 or larger)                 SETU469
    !         SigU     = Standard deviation for wind perturbations (m/s)    SETU470
    !         SigW     = Standard deviation of vertical wind perturbations  SETU471
    !                    (m/s)                                              SETU472
    !         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU473
    !                     updated but perturbation step updated, -1 if      SETU474
    !                     neither perturbations nor step updated            SETU475
    !...                                                                    SETU476
    open (22,file = files(3),iostat = ierr3)
    if (NVARY == 0) then
      write (22,10011)
    else
      write (22,10012)
    end if
    !...    Unit 23 file = 'Winds.txt' : Headers for variables are -        SETU487
    !         EWmean = mean eastward wind component (m/s)                   SETU488
    !         EWpert = perturbation in eastward wind component (m/s)        SETU489
    !         EWtot  = total (mean + perturbed) eastward wind (m/s)         SETU490
    !         NSmean = mean northward wind component (m/s)                  SETU491
    !         NSpert = perturbation in northward wind component (m/s)       SETU492
    !         NStot  = total (mean + perturbed) northward wind (m/s)        SETU493
    !         VWpert = vertical wind perturbation (m/s)                     SETU494
    !         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU495
    !                     updated but perturbation step updated, -1 if      SETU496
    !                     neither perturbations nor step updated            SETU497
    !...                                                                    SETU498
    open (23,file = files(4),iostat = ierr4)
    if (NVARY == 0) then
      write (23,10013)
    else
      write (23,10014)
    end if
    !...    Unit 24 file = 'TPresHgt.txt' : Headers for variables are -     SETU509
    !         Temp    - Mean temperature (K)                                SETU510
    !         Pres    - Mean plus wave-perturbed pressure (N/m**2, log-10,  SETU511
    !                    or % from COSPAR)                                  SETU512
    !         TdegC   - Mean temperature (degrees C)                        SETU513
    !         Pres_mb - Mean plus wave-perturbed pressure (mb)              SETU514
    !         Hrho    - Density scale height (km)                           SETU515
    !         Hpres   - Pressure scale height (km)                          SETU516
    !         MolWt   - molecular weight (kg/kg-mole)                       SETU517
    !         Terhgt  - Altitude of local surface above MOLA 1/2-degree     SETU518
    !                   areoid                                              SETU519
    !         Tgrnd   - ground surface temperature (K)                      SETU520
    !         Areoid  - local radius (km) of MOLA 1/2-degree areoid         SETU521
    !         dAreoid - MOLA areoid minus radius of reference               SETU522
    !                   ellipsoid (km)                                      SETU523
    !         CO2%v   - mole fraction (%) Carbon Dioxide concentration (%   SETU524
    !                   by volume)                                          SETU525
    !          N2%v   - mole fraction (%) Nitrogen concentration (% by      SETU526
    !                   volume)                                             SETU527
    !          Ar%v   - mole fraction (%) Argon concentration (% by volume) SETU528
    !          O2%v   - mole fraction (%) Molecular Oxygen concentration    SETU529
    !                   (% by volume)                                       SETU530
    !          CO%v   - mole fraction (%) Carbon Monoxide concentration (%  SETU531
    !                   by volume)                                          SETU532
    !           O%v   - mole fraction (%) Atomic Oxygen concentration (% by SETU533
    !                   volume)                                             SETU534
    !          He%v   - mole fraction (%) Helium concentration (% by        SETU535
    !                   volume)                                             SETU536
    !          H2%v   - mole fraction (%) Molecular Hydrogen concentration  SETU537
    !                   (% by volume)                                       SETU538
    !           H%v   - mole fraction (%) Atomic Hydrogen concentration (%  SETU539
    !                   by volume)                                          SETU540
    !         H2O%v   - mole fraction (%) Water vapor concentration (% by   SETU541
    !                    volume)                                            SETU542
    !        LOGSCALE - option controlling units of pressure output         SETU543
    !...                                                                    SETU544
    open (24,file = files(5),iostat = ierr5)
    if (NVARY == 0) then
      write (24,10015)
    else
      write (24,10016)
    end if
    !...                                                                    SETU559
    !...    Unit 25 file = 'DayData.txt' : Headers for variables are -      SETU560
    !         TempDay = Local daily average temperature (K)                 SETU561
    !         PresDay = Local daily average pressure (N/m**2)               SETU562
    !         DensDay = Local daily average density (kg/m**3)               SETU563
    !         EWwnDay = Local daily average Eastward wind (m/s)             SETU564
    !         NSwnDay = Local daily average Northward wind (m/s)            SETU565
    !         Tempmin = Local daily minimum temperature (K)                 SETU566
    !         Tempmax = Local daily maximum temperature (K)                 SETU567
    !         Densmin = Local daily minimum density (kg/m**3)               SETU568
    !         Densmax = Local daily maximum density (kg/m**3)               SETU569
    !        LOGSCALE = option controlling units of pressure and density    SETU570
    !...                                                                    SETU571
    open (25,file = files(6),iostat = ierr6)
    if (NVARY == 0) then
      write (25,10017)
    else
      write (25,10018)
    end if
    if (profnear > 0.0d0) then
      write (25,*) " No output; profile being used"
    end if
    !...                                                                    SETU585
    !...    Unit 26 file = 'ThrmData.txt' : Headers for variables are -     SETU586
    !         Tbase     = temperature at 1.26 nbar level (K)                SETU587
    !         Zbase     = height of 1.26 nbar level (km)                    SETU588
    !         F1peak    = height of F1 peak layer (km)                      SETU589
    !         MolWgt    = molecular weight (kg/kg mole)                     SETU590
    !         Texos     = exospheric temperature (K)                        SETU591
    !         hgtoffset = local height offset (km) for MTGCM and MGCM data  SETU592
    !         ibougher  = input parameter controlling height offset option  SETU593
    !...                                                                    SETU594
    open (26,file = files(7),iostat = ierr7)
    if (NVARY == 0) then
      write (26,10019)
    else
      write (26,10020)
    end if
    !...                                                                    SETU605
    !...    Unit 27 file = 'MarsRad.txt' : Headers for variables are -      SETU606
    !         alb      = surface albedo                                     SETU607
    !         mu0      = cosine of solar zenith angle                       SETU608
    !         Dareaden = dust column areal density (kg/m**2)                SETU609
    !         Dmixrat  = dust mixing ratio (kg dust / kg air)               SETU610
    !         Dmasden  = dust mass density (micrograms dust / m**3)         SETU611
    !         Dnumden  = dust number density (number dust particles / m**3) SETU612
    !         Ice      = surface polar ice indicator (0 = no, 1 = yes)      SETU613
    !                                                                       SETU614
    !...                                                                    SETU615
    open (27,file = files(13),iostat = ierr13)
    if (NVARY == 0) then
      write (27,10021)
    else
      write (27,10022)
    end if
    !                                                                       SETU626
    !...    Unit 29 file = OUTPUT file containing list of variables given   SETU627
    !       in Datastep_M05 routine (Format 800 or Format 810)              SETU628
    !                                                                       SETU629
    !       Headers for variables are:                                      SETU630
    !         Time    = time after initial input time (sec)                 SETU631
    !         Height  = planeto-centric height (km) above MOLA areoid       SETU632
    !                   (Height=HgtMOLA) OR planeto-centric height (km)     SETU633
    !                   above ellipsoid (Height=HgtELPS) OR planeto-centric SETU634
    !                   height (km) above local MOLA topographic surface    SETU635
    !                   (Height=HgtSFCM) OR planeto-graphic height (km)     SETU636
    !                   above ellipsoid (Height=HgtGRPH), as determined by  SETU637
    !                   input parameters MOLAhgts, NVARX, NVARY, and ipclat SETU638
    !          Lat    = planeto-centric latitude (Lat=LatPC) or planeto-    SETU639
    !                   graphic latitude (Lat=LatPG) in degrees (North      SETU640
    !                   positive)                                           SETU641
    !       LonW/LonE = longitude (degrees, West positive or East Positive) SETU642
    !        Denkgm3  = Average (mean plus wave=perturbed) density          SETU643
    !                   (kg/m**3) OR "Logkgm3" for Log10(kg/m**3) OR        SETU644
    !                   "Den%Avg" for percent deviation from COSPAR         SETU645
    !                   average, OR "Denkgkm3" for kg/km**3, depending on   SETU646
    !                   input value of LOGSCALE                             SETU647
    !         Temp    = average temperature (K)                             SETU648
    !        EWind    = eastward wind component (m/s, positive toward East) SETU649
    !        NWind    = northward wind component (m/s, positive toward      SETU650
    !                   North)                                              SETU651
    !         sigD    = standard deviation for density perturbations (% of  SETU652
    !                   unperturbed mean)                                   SETU653
    !          Ls     = areocentric longitude of Sun from Mars (degrees)    SETU654
    !         Dust    = dust optical depth                                  SETU655
    !        CO2%m    = Carbon Dioxide mass concentration (% by mass)       SETU656
    !         N2%m    = Nitrogen mass concentration (% by mass)             SETU657
    !         Ar%m    = Argon mass concentration (% by mass)                SETU658
    !         O2%m    = Molecular Oxygen mass concentration (% by mass)     SETU659
    !         CO%m    = Carbon Monoxide mass concentration (% by mass)      SETU660
    !          O%m    = Atomic Oxygen mass concentration (% by mass)        SETU661
    !         He%m    = Helium mass concentration (% by mass)               SETU662
    !         H2%m    = Molecular Hydrogen mass concentration (% by mass)   SETU663
    !          H%m    = Atomic Hydrogen mass concentration (% by mass)      SETU664
    !        H2O%m    = Water vapor mass concentration (% by mass)          SETU665
    open (29,file = files(10),iostat = ierr10)
    EWlon = "W"
    if (LonEW == 1) then
      EWlon = "E"
    end if
    if (MOLAHGTS == 1) then
      HgtLbl = "HgtMOLA"
      if (NVARX==2 .or. NVARY==2) then
        HgtLbl = "HgtSFCM"
      end if
      LatLbl = "LatPC"
    else
      HgtLbl = "HgtELPS"
      LatLbl = "LatPC"
      if (IPCLAT /= 1) then
        HgtLbl = "HgtGRPH"
        LatLbl = "LatPG"
      end if
    end if
    if (LOGSCALE == 0) then
      DensLbl = " Denkgm3"
    end if
    if (LOGSCALE == 1) then
      DensLbl = " Logkgm3"
    end if
    if (LOGSCALE == 2) then
      DensLbl = " Den%Avg"
    end if
    if (LOGSCALE == 3) then
      DensLbl = "Denkgkm3"
    end if
    write (29,10023) HgtLbl, LatLbl, EWlon, DensLbl
  end if

  ! DM1359 adding DIR  
  !...  Compute character string length of DATADIR path name              SETU690
!  lendir = index(DATADIR," ") - 1
!  if (lendir<1 .or. lendir>256) then
!    lendir = 256
!  end if
  !...  Unit 9 molatoph.bin file contains MOLA 1/2 degree resolution      SETU693
  !     areoid radius and topographic heights above the areoid, versus    SETU694
  !     latitude and longitude                                            SETU695
!  open (9,file = DATADIR(1:lendir)//files(8),status = "old", &
!        form = "unformatted",iostat = ierr8)
  !...  Unit 10 COSPAR2.DAT file contains COSPAR atmosphere data values   SETU698
!  open (10,file = DATADIR(1:lendir)//files(9),status = "old",iostat = ierr9)
  !...  Unit 11 hgtoffst.dat file with MTGCM height offset array          SETU701
!  open (11,file = DATADIR(1:lendir)//files(11),status = "old",iostat = ierr11)
  !...  Unit 12 albedo1.bin binary format albedo file                     SETU704
!  open (12,file = DATADIR(1:lendir)//files(12),status = "old", &
!        form = "unformatted",iostat = ierr12)
  !...  Test for file open error condition                                SETU707
  
!...  Compute character string length of DIR path name              SETU690
  lendir = index(DIR," ") - 1
  if (lendir<1 .or. lendir>256) then
    lendir = 256
  end if
  !...  Unit 9 molatoph.bin file contains MOLA 1/2 degree resolution      SETU693
  !     areoid radius and topographic heights above the areoid, versus    SETU694
  !     latitude and longitude                                            SETU695
#ifdef __GFORTRAN__
  open (9,file = DIR(1:lendir)//files(8),status = "old", &
        form = "unformatted", convert='big_endian', iostat = ierr8)
#else
  open (9,file = DIR(1:lendir)//files(8),status = "old", &
        form = "unformatted",iostat = ierr8)
#endif

  !...  Unit 10 COSPAR2.DAT file contains COSPAR atmosphere data values   SETU698
  open (10,file = DIR(1:lendir)//files(9),status = "old",iostat = ierr9)
  !...  Unit 11 hgtoffst.dat file with MTGCM height offset array          SETU701
  open (11,file = DIR(1:lendir)//files(11),status = "old",iostat = ierr11)
  !...  Unit 12 albedo1.bin binary format albedo file                     SETU704
#ifdef __GFORTRAN__
  open (12,file = DIR(1:lendir)//files(12),status = "old", &
        form = "unformatted", convert='big_endian', iostat = ierr12)
#else
  open (12,file = DIR(1:lendir)//files(12),status = "old", &
        form = "unformatted",iostat = ierr12)
#endif
  !...  Test for file open error condition                                SETU707

  do j = 1,13
    if (ierr(j) /= 0) then
      write (IU0,10024) ierr(j), files(j)
      stop " Error termination! Check the LIST file for messages."
    end if
  end do
  !...................................................................... SETU715
  !...  Read COSPAR2.DAT atmosphere data                                  SETU716
  do i = 1,164
    read (10,*,end = 1100,err = 1100) ZC(i), TC(i), PC(i), DC(i)
  end do
  !...  Read MTGCM and TES yr1 and yr 2 height offsets                    SETU722
  read (11,10025) dummy
  do i = 0,12
    read (11,*,Err = 1200) ls, (OFFSETS(i,j), j = 1,ndust)
    if (ls /= 30*i) stop " Error reading MTGCM height offsets"
  end do
  read (11,10025) dummy
  do i = 0,12
    read (11,*,Err = 1300) ls, (TOFFSETS(i,j), j = 1,ntesy)
    if (ls /= 30*i) stop " Error reading TES Yr1 and Yr2 height offsets"
  end do
  ! DM 1359: remove information displayed on screen
  !...  Read topographic height data file                                 SETU736
  !write (IU0,*) " Reading MOLA 1/2 degree areoid and topography"
  read (9) AREORAD, TOPOMOLA
  !...  Read 1 degree resolution albedo file                              SETU739
  !write (IU0,*) " Reading 1 degree albedo data"
  read (12) ALBEDO

!  DM1359 : adding DIR
!  write (IU0,*) " Reading Mars GCM surface data files"
!  call Readsurf_M05(GCMDIR,version)
!  write (IU0,*) " Reading Mars GCM 0-80 km data files"
!  call ReadMGCM_M05(GCMDIR,version)
!  write (IU0,*) " Reading Mars TGCM 80-170 km data files"
!  call ReadTGCM_M05(GCMDIR,version)
!  write (IU0,*) " Reading TES Yr1 & Yr2 MGCM surface data files"
!  call RdTESsrf_M05(GCMDIR,version)
!  write (IU0,*) " Reading TES Yr1 & Yr2 MGCM -5 to 80 km data files"
!  call RdTESmgcm_M05(GCMDIR,version)
!  write (IU0,*) " Reading TES Yr1 & Yr2 MTGCM 80-240 km data files"
!  call RdTEStgcm_M05(GCMDIR,version)

  !write (IU0,*) " Reading Mars GCM surface data files"
  call Readsurf_M05(DIR,version)
  !write (IU0,*) " Reading Mars GCM 0-80 km data files"
  call ReadMGCM_M05(DIR,version)
  !write (IU0,*) " Reading Mars TGCM 80-170 km data files"
  call ReadTGCM_M05(DIR,version)
  !write (IU0,*) " Reading TES Yr1 & Yr2 MGCM surface data files"
  call RdTESsrf_M05(DIR,version)
  !write (IU0,*) " Reading TES Yr1 & Yr2 MGCM -5 to 80 km data files"
  call RdTESmgcm_M05(DIR,version)
  !write (IU0,*) " Reading TES Yr1 & Yr2 MTGCM 80-240 km data files"
  call RdTEStgcm_M05(DIR,version)

  DTR = atan(1.0d0) / 45.0d0
  !...  Check date; If error,write message and stop                       SETU755
  call chkdt_M05(MYEAR,month,mday,ihr,imin,sec,idterr)
  if (idterr < -6) then
    write (IU0,10027)
  elseif (idterr < 0) then
    write (IU0,10026)
  end if
  if (idterr < 0) stop " Error termination! Check the LIST file for messages."
  !...  Compute Julian day                                                SETU765
  call CaltoJul_M05(MYEAR,MONTH,MDAY,IHR,IMIN,SEC,DAY)
  DAY0 = DAY
  if (IUP > 0) then
    if (IERT == 1) then
      write (IUP,10028)
    else
      write (IUP,10029)
    end if
    if (IUTC == 1) then
      write (IUP,10030)
    else
      write (IUP,10031)
    end if
    write (IUP,10032) MONTH, MDAY, MYEAR, DAY, IHR, IMIN, SEC
  end if
  DTEX = deltaTEX
  if (IUP > 0) then
    if (deltaTEX /= 0.0d0) then
      write (IUP,10033) deltaTEX
    end if
    if (MOLAHGTS == 1) then
      write (IUP,10034)
    elseif (IPCLAT == 1) then
      write (IUP,10036)
    else
      write (IUP,10035)
    end if
    write (IUP,10037) REQUA, RPOLE
    write (IUP,10038)
  end if
  !...  Sun position in Mars latitude (areocentric latitude) and          SETU813
  !...  longitude. ALS = Ls = areocentric longitude of sun in orbital     SETU814
  !...  position (Ls = 0 at spring equinox). MARSAU = Mars orbital        SETU815
  !...  radius in Astronomical Units                                      SETU816
  if (dradau > 0.0d0) then
    ALS = dsunLs
    MARSAU = dradau
    owlt = dowlt
  else
    !...    Use built-in Mars ephemeris routine                             SETU822
    !...    Convert to Terrestrial (Dynamical) Time, if necessary           SETU823
    ttsec = 0.0d0
    if (iutc == 1) then
      !...      Get terrestrial dynamical time offset (seconds)               SETU826
      dt = (DAY0-2451545.0d0) / 36525.0d0
      !...      Terrestrial time offset (in seconds) TT = UTC + ttsec         SETU828
      ttsec = (64.184d0+95.0d0*dt+35.0d0*dt**2) / 86400.0d0
    end if
    call marsephm_M05(DAY0+ttsec,sunlat,sunlon,ALS,MARSAU,owlt,EOT)
    !...    Convert to Mars-Event Time, if necessary                        SETU832
    if (IERT == 1) then
      call marsephm_M05(DAY0+ttsec-owlt/6050.0d0,sunlat,sunlon,ALS,MARSAU, &
                        owlt,EOT)
    end if
    !...    Convert planetographic sun latitude to planetocentric           SETU835
    sunlat = atan(tan(sunlat*DTR)/(3396.0d0/3378.32d0)**2) / DTR
  end if
  if (ALS0<180.0d0 .or. ALS0>320.0d0) then
    if (ALS0 > 0.0d0) then
      write (IU0,*) " ** Ls0 outside range. No dust storm assumed."
      if (IUP > 0) then
        write (IUP,*) " ** Ls0 outside range. ", "No dust storm assumed."
      end if
    end if
    ALS0 = -999.0d0
    INTENS = 0.0d0
  elseif (INTENS<0.0d0 .or. INTENS>3.0d0) then
    write (IU0,*) " Intensity must be between 0 and 3"
    stop " Error termination! Check the LIST file for messages."
  elseif (RADMAX<=0.0d0 .or. RADMAX>10000.0d0) then
    RADMAX = 0.0d0
  end if
  !...  Set ALSDUR within allowable range                                 SETU853
  if (ALSDUR < 12.0d0) then
    ALSDUR = 12.0d0
  end if
  if (ALSDUR > 48.0d0) then
    ALSDUR = 48.0d0
  end if
  if (F107<50.0d0 .or. F107>450.0d0) then
    write (IU0,*) " F10.7 must be between 50 and 450"
    stop " Error termination! Check the LIST file for messages."
  end if
  if (STDL<-3.0d0 .or. STDL>3.0d0) then
    write (IU0,*) " Std. deviations must be between -3 and +3"
    stop " Error termination! Check the LIST file for messages."
  end if
  if (NR1<=0 .or. NR1>=30000) then
    write (IU0,10039)
    stop " Error termination! Check the LIST file for messages."
  end if
  IX = NR1
  IY = 172*mod(IX,176) - 35*(IX/176)
  IZ = 170*mod(IX,178) - 63*(IX/178)
  if (IY < 0) then
    IY = IY + 30307
  end if
  if (IZ < 0) then
    IZ = IZ + 30323
  end if
  z1 = RANDOM_M05(L)
  RHOd = PPND_M05(z1,L)
  z1 = RANDOM_M05(L)
  RHOu = PPND_M05(z1,L)
  z1 = RANDOM_M05(L)
  RHOv = PPND_M05(z1,L)
  z1 = RANDOM_M05(L)
  RHOw = PPND_M05(z1,L)
  if (L == 1) then
    write (IU0,10039)
    stop " Error termination! Check the LIST file for messages."
  end if
  if (LSTFL/="CON" .and. LSTFL/="con" .and. IUP>0) then
    if (ALS0 > 0.0d0) then
      if (RADMAX /= 0.0d0) then
        write (IUP,10040) &
              ALS0, INTENS, ALSDUR, RADMAX, DUSTLAT, DUSTLON, 360.0d0-DUSTLON
      else
        write (IUP,10041) ALS0, INTENS, ALSDUR
      end if
    end if
    if (IUP > 0) then
      write (IUP,10042) F107, F107/MARSAU**2, STDL
      if (MAPYEAR == 0) then
        write (IUP,10045)
      else
        write (IUP,10046) MAPYEAR
      end if
      write (IUP,10043) DUSTNU, DUSTDIAM, DUSTDENS
    end if
    if (IUP > 0) then
      write (IUP,10044) &
            NR1, rpscale, corlmin, RWSCALE, WLSCALE, WMSCALE, BLWINFAC
    end if
  end if
  if (NVARX<1 .or. NVARX>15) then
    write (IU0,10048)
    write (IU0,10047)
    stop " Error termination! Check the LIST file for messages."
  end if
  if (NVARY<0 .or. NVARY>15) then
    write (IU0,10048)
    write (IU0,10047)
    stop " Error termination! Check the LIST file for messages."
  else
    if (LOGSCALE<0 .or. LOGSCALE>3) then
      LOGSCALE = 0
    end if
    !...  Initialize position data                                          SETU959
    CHGT = FHGT
    CLAT = FLAT
    CLON = FLON
    CSEC = 0.0d0
! DM 1359: remove information from display   
!    write (IU0,*) " Finished Setup_M05 - Starting computations"
  end if
  return
  1100 stop " Error or EOF on COSPAR2.DAT file!"
  1200 stop " Error reading MTGCM height offsets"
  1300 stop " Error reading TES Yr1 and Yr2 height offsets"
  ! 
  ! ... Format Declarations ...
  ! 
  !                                                                       SETU  5
  10000 format (" Mars-GRAM 2005 (Version ",a1,".1) - October, 2005")
  10001 format (" Error in first latitude or longitude.")
  10002 format (" Unable to open Trajectory Data file!")
  10003 format (" Profile file= ",(a))
  10004 format (" LIST file= ",(a)/" OUTPUT file= ",(a))
  10006 format (" Trajectory file= ",(a))
  10007 format (" Wave file= ",(a))
  10008 format (" Data directory= ",(a)/" GCM  directory= ",(a))
  10009 format ( &
        "    Var_X        DENSLO     DENSAV     DENSHI", &
        "    DENSTOT  DustOD  Radius   Grav RadAU LOGSCALE", &
        " hgtoffset ibougher MapYear profwgt")
  10010 format ( &
        "    Var_X        Var_Y        DENSLO     DENSAV     ", &
        "DENSHI    DENSTOT  DustOD  Radius   Grav RadAU LOGSCALE", &
        " hgtoffset ibougher MapYear profwgt")
  10011 format ( &
        "    Var_X      SigD  DensRand  DensWave", &
        "     DensP    corlim   SigU   SigW iupdate")
  10012 format ( &
        "    Var_X        Var_Y      SigD  DensRand", &
        "  DensWave     DensP    corlim   SigU   SigW iupdate")
  10013 format ( &
        "    Var_X      EWmean  EWpert   EWtot  ", &
        "NSmean  NSpert   NStot  VWpert iupdate")
  10014 format ( &
        "    Var_X        Var_Y      EWmean  EWpert   ", &
        "EWtot  NSmean  NSpert   NStot  VWpert iupdate")
  10015 format ( &
        "    Var_X       Temp      Pres   TdegC   Pres_mb", &
        "     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid  dAreoid", &
        " CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v   H%v", &
        " H2O%v LOGSCALE")
  10016 format ( &
        "    Var_X        Var_Y       Temp      Pres   TdegC", &
        "   Pres_mb     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid", &
        "  dAreoid CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v", &
        "   H%v H2O%v LOGSCALE")
  10017 format ( &
        "    Var_X    TempDay  PresDay    DensDay", &
        "   EWwnDay NSwnDay Tempmin Tempmax   Densmin    Densmax","  LOGSCALE" &
        )
  10018 format ( &
        "    Var_X        Var_Y    TempDay  PresDay", &
        "    DensDay   EWwnDay NSwnDay Tempmin Tempmax   Densmin", &
        "    Densmax  LOGSCALE")
  10019 format ( &
        "    Var_X       Tbase   Zbase  F1peak", &
        "  MolWgt   Texos  hgtoffset ibougher")
  10020 format ( &
        "    Var_X        Var_Y       Tbase   Zbase", &
        "  F1peak  MolWgt   Texos  hgtoffset ibougher")
  10021 format ( &
        "    Var_X       alb      mu0 Dareaden  Dmixrat", &
        "  Dmasden  Dnumden Ice")
  10022 format ( &
        "    Var_X        Var_Y       alb      mu0 Dareaden", &
        "  Dmixrat  Dmasden  Dnumden Ice")
  10023 format ( &
        "     Time  ",a7,1x,a5,"   Lon",a1,2x,a8,"   Temp", &
        "  EWind  NWind  sigD  Ls   Dust CO2%m  N2%m  Ar%m  O2%m", &
        "  CO%m   O%m  He%m  H2%m   H%m H2O%m")
  10024 format (" File open error! Error =",i5,1x,(a))
  10025 format (a1)
  10026 format (" Input error in month, day or year.")
  10027 format (" Input error in hour, minute or seconds.")
  10028 format (" Input time is Earth-Receive Time (ERT)")
  10029 format (" Input time is Mars-Event Time (MET)")
  10030 format (" Input time is Coordinated Universal Time (UTC)")
  10031 format (" Input time is Terrestrial (Dynamical) Time (TT)")
  10032 format ( &
        " Date = ",i2,"/",i2,"/",i4,"  Julian Day = ",f13.5,"  Time = ",i2,":" &
        ,i2,":",f4.1)
  10033 format (" deltaTEX=",f7.1,"K")
  10034 format ( &
        " Input heights are planeto-centric, relative to MOLA"," areoid")
  10035 format ( &
        " Input heights are planeto-graphic (relative to ", &
        " reference ellipsoid)")
  10036 format ( &
        " Input heights are planeto-centric, relative to ", &
        " reference ellipsoid")
  10037 format ( &
        " Reference ellipsoid radii (km): Equator =",f8.2," Pole =",f8.2)
  10038 format ( &
        " Output heights are planeto-centric, except as noted."/ &
        " Longitude & ephemeris use IAU 2000 rotational system.")
  10039 format (" Error in starting random number.")
  10040 format ( &
        " Local scale dust storm, starting at Ls = ",f5.1, &
        " deg.,  Intensity = ",f3.1/"  with duration = ",f5.1, &
        " degrees of Ls angle."/" Max. radius = ",f7.1," km,  At Lat-Lon = " &
        ,f6.2," N,  ",f6.2," W (",f6.2," E)")
  10041 format ( &
        " Global scale dust storm, starting at Ls = ",f5.1, &
        " deg.,  Intensity = ",f3.1/"  with duration = ",f5.1, &
        " degrees of Ls angle.")
  10042 format ( &
        " F10.7 flux = ",f5.1," (1 AU)  ",f5.1, &
        " (Mars),  standard deviation = ",f4.1)
  10043 format ( &
        " Dustnu =",f7.4,"   Dustdiam =",f6.2," E-6 meters   Dustdens =",f8.1, &
        " kg/m**3")
  10044 format ( &
        "   Random seed =",i6,"  Dens.Pert.Scale Factor =",f5.2,"   corlmin =" &
        ,f6.3/"   Wind.Pert.Scale Factor =",f6.2, &
        "    Wavelength Scale Factor =",f6.2/"   Mean Wind Scale"," Factor =" &
        ,f6.2,"    Slope Wind Scale Factor =",f6.2)
  10045 format (" Dust optical depth from NAMELIST input")
  10046 format ( &
        " Dust optical depth vs lat and Ls from TES Mapping Year ",i1," data")
  10047 format ( &
        /" Select x-code and y-code for plotable output versus", &
        " desired parameter(s):"//" Code              Parameter"/ &
        " ----   -------------------------------------------------"/ &
        "   1    Height (above MOLA areoid, km)"/ &
        "   2    Height (above local MOLA topographic surface, km)"/ &
        "   3    Latitude (deg.)"/ &
        "   4    Longitude (deg.) West if LonEW=0, East if LonEW=1"/ &
        "   5    Time from start (Earth seconds)"/ &
        "   6    Time from start (Martian Sols)"/ &
        "   7    Areocentric Longitude of Sun, Ls (deg.)"/ &
        "   8    Local Solar Time (Mars hours = 1/24th sol)"/ &
        "   9    Pressure (mb)"/ &
        "  10    Pressure Height (km) [-H*log(Pres/PresSurf) = ", &
        "-H*log(sigma)]"/ &
        "  11    Sigma coordinate [sigma=Pressure/(Surface Pressure)]"/ &
        "  12    Height (km) above reference ellipsoid"/ &
        "  13    Planeto-Graphic Height (km) above reference ellipsoid"/ &
        "  14    Planeto-Graphic Latitude (deg.)"/ &
        "  15    Longitude in range -180 to +180 deg. (East or West)"// &
        " Use y-code = 0 for plotable output vs x-code variable only")
  10048 format (" x-code or y-code input error.")
end subroutine Setup_M05
!---------------------------------------------------------------------- SETU968
subroutine Randinit_M05(J,NR1,RHOd,RHOu,RHOv,RHOw,iup,iustdout)
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: J,iup,iustdout
  integer, intent(inout) :: NR1
  real(kind=PM_REEL), intent(out) :: RHOd,RHOu,RHOv,RHOw
  ! 
  !.. Local Scalars .. 
  integer :: L
  !
  !DM 1359 move external function to private 
  !.. External Functions .. 
  !real(kind=PM_REEL), external :: PPND_M05
  !real(kind=PM_REEL), external :: RANDOM_M05
  ! 
  !.. Intrinsic Functions .. 
  intrinsic mod
  ! 
  !.. Common Blocks .. 
  common /RANDCOM_M05/ IX,IY,IZ
  !     For IX to IZ: Not Read, Overwritten
  ! 
  !... Variables in Common Block /RANDCOM_M05/ ... 
  integer :: IX,IY,IZ
  ! 
  ! ... Executable Statements ...
  ! 
  !...  Re-initialize NR1, e.g. by reading from a file, or some algorithm RNDI  4
  !     from previous NR1 value, or by some computation on index J.       RNDI  5
  !     Note that it is not necessary to randomly select each seed value  RNDI  6
  !     NR1 in order to get a random sequence of output.  Any regular     RNDI  7
  !     progression of selected NR1 values will do for this process.      RNDI  8
  NR1 = NR1 + 11
  if (NR1 > 30000) then
    NR1 = mod(NR1,30000)
  end if
  !...  Write random seed value to list file                              RNDI 11
  if (iup /= 0) then
    write (iup,10000) J, NR1
  else
    write (iustdout,10000) J, NR1
  end if
  IX = NR1
  IY = 172*mod(IX,176) - 35*(IX/176)
  IZ = 170*mod(IX,178) - 63*(IX/178)
  if (IY < 0) then
    IY = IY + 30307
  end if
  if (IZ < 0) then
    IZ = IZ + 30323
  end if
  RHOd = RANDOM_M05(L)
  RHOd = PPND_M05(RHOd,L)
  RHOu = RANDOM_M05(L)
  RHOu = PPND_M05(RHOu,L)
  RHOv = RANDOM_M05(L)
  RHOv = PPND_M05(RHOv,L)
  RHOw = RANDOM_M05(L)
  RHOw = PPND_M05(RHOw,L)
  return
  ! 
  ! ... Format Declarations ...
  ! 
  10000 format ("   Random seed number",i6," =",i6)
end subroutine Randinit_M05
!---------------------------------------------------------------------- RNDI 33
subroutine chkdt_M05(MYEAR,month,iday,ihour,minutes,sec,err)
  !                                                                       CKDT  2
  !      CHecKs input Date and Time for validity and internal             CKDT  3
  !      consistency.  Returns error message(s) and prompts               CKDT  4
  !      user to re-enter inputs.                                         CKDT  5
  !                                                                       CKDT  6
  ! 
  !.. Implicit Declarations .. 
  implicit none
  ! 
  !.. Formal Arguments .. 
  integer, intent(in) :: iday,ihour,minutes,month
  integer, intent(inout) :: MYEAR
  integer, intent(out) :: err
  real(kind=PM_REEL), intent(in) :: sec
  ! 
  !.. Local Scalars .. 
  logical :: centyear,leapyear
  ! 
  !.. Intrinsic Functions .. 
  intrinsic mod
  ! 
  ! ... Executable Statements ...
  ! 
  err = 0
  centyear = .false.
  leapyear = .false.
  !...   Convert to 4-digit year, if necessary                            CKDT 14
  if (MYEAR>=0 .and. MYEAR<=69) then
    MYEAR = MYEAR + 2000
  end if
  if (MYEAR>=70 .and. MYEAR<=99) then
    MYEAR = MYEAR + 1900
  end if
  if (MYEAR<1970 .or. MYEAR>2069) then
    write (*,*) " Year must be 1970-2069"
    err = -1
  end if
  if (mod(MYEAR,100) == 0) then
    centyear = .true.
  end if
  if (mod(MYEAR,4) == 0) then
    leapyear = .true.
  end if
  if (centyear) then
    if (mod(MYEAR,400) /= 0) then
      leapyear = .false.
    end if
  end if
  if (month<1 .or. month>12) then
    !...     Write(*,*)' Month must be 1-12'                                CKDT 27
    err = -2
  end if
  if (month==4 .or. month==6 .or. month==9 .or. month==11) then
    if (iday<1 .or. iday>30) then
      !...       Write(*,*)' Day of month must be 1-30'                       CKDT 32
      err = -3
    end if
  elseif (month == 2) then
    if (leapyear) then
      if (iday<1 .or. iday>29) then
        !...         Write(*,*)' Day of month must be 1-29 (Leap Year)'         CKDT 38
        err = -4
      end if
    elseif (iday<1 .or. iday>28) then
      !...       Write(*,*)' Day of month must be 1-28 (Non-Leap Year)'       CKDT 42
      err = -5
    end if
  elseif (iday<1 .or. iday>31) then
    !...       Write(*,*)' Day of month must be 1-31'                       CKDT 46
    err = -6
  end if
  if (ihour<0 .or. ihour>24) then
    !...     Write(*,*)' Hour must be 0-24'                                 CKDT 50
    err = -7
  end if
  if (ihour==24 .and. (minutes/=0.or.sec/=0.0d0)) then
    !...     Write(*,*)' Hour must be 23 or Time must be 24:00:00'          CKDT 54
    err = -7
  end if
  if (minutes<0 .or. minutes>60) then
    !...     Write(*,*)' Minutes must be 0-60'                              CKDT 58
    err = -8
  end if
  if (minutes==60 .and. sec/=0.0d0) then
    !...     Write(*,*)' Minutes must be 59 or Seconds must be 0'           CKDT 62
    err = -8
  end if
  if (sec<0.0d0 .or. sec>60.0d0) then
    !...     Write(*,*)' Seconds must be 0.0-60.0'                          CKDT 66
    err = -9
  end if
end subroutine chkdt_M05
!---------------------------------------------------------------------- CKDT 71
!-------------- END OF FILE :  setup_M05.F90 --------------------------
!---------------------------------------------------------------------- 

! end of module
end module cps_atm_gram05
