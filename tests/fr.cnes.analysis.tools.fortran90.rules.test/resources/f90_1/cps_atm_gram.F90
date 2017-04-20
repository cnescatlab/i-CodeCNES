² cps_atm_gram

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
!  Modèle d'Atmosphère martien GRAM 2001
!
!$Description
! Ce module contient le modèle GRAM 2001, routine cps_atmmarsgram_2001
! porté en FORTRAN 90 depuis la LIB_ATMMARS
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_atm_gram.F90 393 2013-02-26 09:34:07Z ffsm $
!
!$Historique
!  $Log: cps_atm_gram.F90,v $
!  Revision 393  2013/02/26 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.10  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2008/10/16 08:18:39  cml
!  DM-ID 1058 : Correction de l utilisation de Tgrnd
!
!  Revision 1.8  2008/07/04 12:03:05  huec
!  DM-ID 1058 : Precision numerique : transformations de parametres en pm_reel
!
!  Revision 1.7  2008/04/08 06:50:46  vivaresf
!  FA-ID 1009 : suppression de cps_acces, récupération des
!  répertoires
!
!  Revision 1.6  2008/04/07 09:17:07  vivaresf
!  FA-ID 1009 : suppression des use cps_acces
!  correction des cartouches
!  vérification des accès par le biais de la base COMPAS aux modèles
!
!  Revision 1.5  2007/11/21 11:49:48  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.4  2006/11/11 12:33:13  vivaresf
!  DM-ID 425 : version commune Solaris Linux
!
!  Revision 1.3  2006/11/10 10:48:17  vivaresf
!  Version 2.1 : validation
!
!  Revision 1.2  2006/10/18 09:54:24  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!
!  Revision 1.1.1.1.2.1  2006/09/26 12:19:56  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!
!  Revision 1.1.1.1  2005/12/07 07:23:07  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.13  2005/10/17 17:11:30  vivaresf
!  DM-ID 388 : interface s pour les fonctions de paramétrage
!
!  Revision 1.12  2005/03/07 08:15:48  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!
!  Revision 1.11  2005/03/04 16:40:29  vivaresf
!  DM-ID 318 : traduction des entêtes
!
!  Revision 1.10  2005/02/25 17:38:55  vivaresf
!  Precision sur les constantes
!
!  Revision 1.9  2005/02/03 14:50:35  vivaresf
!  Typage
!
!  Revision 1.8  2005/02/02 08:24:24  vivaresf
!  Variable mal nommée
!
!  Revision 1.7  2005/01/28 18:05:16  vivaresf
!  Ca marche presque
!
!  Revision 1.6  2005/01/27 12:57:14  vivaresf
!  DM-ID 318 : entetes des fichiers et des routines utilisateurs
!  nommage des fonctions publique, privatisation des fonctions internes
!  declaration des variables
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_gram
!

!
!$Mots-cles
!  ATMOSPHERE MARS GRAM2001
!
!$Voir-Aussi
!#V
!
!#
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB, only : PM_REEL
  use cps_utilisateur

  implicit real (a-h,o-z)
  implicit integer (i-n)

  private Cp_2001,dustvsls_2001,Ifloor_2001,PPND_2001,Random_2001,TideX_2001
  private TideY_2001,Zlogr_2001,ATMOS2_2001,bltp_2001,cospar_2001,Dustfact_2001
  private Datastep_2001,EScalc_2001,FourD_2001,MGCMterp_2001,MarsGCM_2001
  private orbit_2001,ReadMGCM_2001,Readsurf_2001,ReadTGCM_2001
  private SublTchk_2001,surfterp_2001,STEWART2_2001,ThreeD_2001,THERMOS_2001
  private topoareo_2001,Thermpar_2001,TGCMterp_2001,TwoD_2001,Wavelon_2001
  private Setup_2001,Randinit_2001


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_gram.F90 393 2013-02-26 09:34:07Z ffsm $'

  interface cps_orbit_2001
     module procedure orbit_2001
  end interface

  interface cps_dustvsls_2001
     module procedure dustvsls_2001
  end interface


contains

!!!   atmmarsgram_2001 : seule fonction externe a retenir

  subroutine cps_atmmarsgram_2001 (r,xlat,xlon,xdate,dir,file, &
       init_atm,scena,dust,disp,seed,rpscale,z,pressure, &
       ro,temperature,windu,windv,meanvar,extvar,ier)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!     cps_atmmarsgram_2001
!
!$Resume
!     Calcul de variables météorologiques avec le modèle MarsGRAM 2001
!
!$Description
!     Calcul de variables météorologiques avec le modèle MarsGRAM 2001
!
!$Acces
!  PUBLIC
!
!$Version
!
!     1.0 06/2002
!     27/01/2005   - integration dans COMPAS avec portage en fortran 90
!
!$Auteur
!
!     E. Perot (COFRAMI)
!
!$Acces
!  PUBLIC
!
!$Usage
!     call cps_atmmarsgram_2001 (r,xlat,xlon,xdate,dir,file,init_atm,
!                            scena,dust,disp,seed,rpscale,z,pressure,
!                            ro,temperature,windu,windv,meanvar,extvar,ier)
!
!$Arguments
!>E r      : <PM_REEL> distance au centre de Mars (m)
!>E xlat   : <PM_REEL> latitude (rad)
!>E xlon   : <PM_REEL> Longitude East (rad)
!>E xdate  : <PM_REEL> date (jour julien Terrestre)
!>E dir    : <character*(*)>    Répertoire des données Mars-GRAM 2001 :
!                             par défaut celui-ci est distribué par COMPAS.
!. si les ressources COMPAS ne sont pas accessibles, recherche un lien "GRAM_DATA" sur ce
!  répertoire.
!>E file   : <character*(*)>    Fichier de configuration GRAM
!                 par défaut il s'agit du fichier input_gram (distribué par COMPAS)
!>E init_atm : <logical>        demande la réinitialisation du modèle.
!>E scena  : <integer>          Scénario de poussière :
!.       1 = épaisseur optique des poussières Viking (tau is calculé par le code)
!.       2 = tau spécifique  (tau est lu dans la variable <dust>)
!>E dust   : <PM_REEL> épaisseur optique des poussières
!.                             < 0 : Scénario de poussières Viking
!.                             > 0 (doit être dans l'intervalle [0.1-3.0])
!>E disp   : <integer>         Type de perturbations
!.                             1 = atmosphère nominale
!.                             2 = atmosphère perturbée
!>E seed   : <integer>    Graine du générateur aléatoire pour le modèle de perturbation
!>E rpscale: <PM_REEL> Facteur multiplicatif  pour la densité et les perturbations dues au vents.
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
!.                 extvar(10)= température moyenne à la surface du sol (K)
!.                 extvar(11)= inutilisé 
!.                 extvar(12)= inutilisé 
!.                 extvar(13)= perturbation aléatoire (sans ondes wave pert.) (kg/m^3)
!.                 extvar(14)= hauteur orographique (m)
!.                 extvar(15)= pression moyenne à la surface du sol (Pa)
!.                 extvar(16)= inutilisé 
!.                 extvar(17)= inutilisé 
!.                 extvar(18)= inutilisé 
!.                 extvar(19)= inutilisé 
!.                 extvar(20)= inutilisé 
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
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

        implicit none

!     Variables Entrees/Sorties
      real(kind=PM_REEL) :: r,xlat,xlon,xdate,dust,rpscale
      real(kind=PM_REEL) :: z,pressure,ro,temperature,windu,windv
      real(kind=PM_REEL) :: meanvar(5),extvar(25)
      integer :: scena,disp,seed,ier
      character*(*) :: dir, file
      logical :: init_atm

!     SETUP_2001 and DATASTEP_2001 arguments
      real(kind=PM_REEL) :: hgtasfc
      real(kind=PM_REEL),save :: CLAT,CHGT,CLON,CSEC,DATE0
      real :: DELHGT,DELLAT,DELLON,DELTIME
      real,save :: RHOd,RHOu,RHOv
      real(kind=PM_REEL) :: TEMP,PRES,DENSLO,DENS,DENSHI,DENSP,DENSTOT
      real(kind=PM_REEL) :: EWWIND,EWpert,NSWIND,NSpert,Hrho,Hpres
      real(kind=PM_REEL) :: corlim,numwave
      integer :: LonEW
!     ** New arguments added to the original MarsGRAM Datastep subroutine **
      real(kind=PM_REEL) :: ALS,TLOCAL,DENS0,SIG,PSURF,TSURF

!     Variables cps_RELLIPS_2001
      real(kind=PM_REEL) :: Lat, LonW, Rmola, Rell, topohgt, gz, alb

!     Other Variables
      real(kind=PM_REEL) :: absheight
      real :: UTIME
      integer :: i

!     Useless Input/Output arguments of SETUP_2001 and DATASTEP_2001
      integer :: eof, maxnum, nr1, iu, nmonte

      real(kind=PM_REEL) :: crd, pi
      parameter (pi=3.14159265359d0)
      parameter (crd=180.d0/pi)

      logical :: first_call
      data first_call/.true./

!     common Variables in DATACOM_2001
      real (kind=PM_REEL) :: DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          dTEX,rpfactor,Dusttau,DustOD,Dustnu,Dustdiam,Dustdens
      integer :: NPOS,NVARX,NVARY,logscale,iu0,iup,maxfiles,MOLAhgts

      COMMON /DATACOM_2001/NPOS,NVARX,NVARY,logscale, iu0,iup,maxfiles,MOLAhgts,&
          DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          dTEX,rpfactor,Dusttau,DustOD,Dustnu,Dustdiam, &
          Dustdens

      ier = 0

      if (first_call) then

!       if blank string, set default Data Directory
!       acces au repertoire contenant les fichiers du modele par COMPAS
         ier = cps_getFichierModele("atmosphere", "GRAM2001", dir, &
           rep=.true.)
         if (MSP_gen_messages("atmmarsgram_2001")) return
         
         ! if blank string, set default INPUT file
         ! uniquement si le répertoire a été trouvé, et on supprime 
         ! "data_marsgram_2001" à la fin du répertoire.
         if (index(file,' ').le.1.and.index(dir,' ').ge.1) &
              file = dir(1:len_trim(dir)-18)//'input_gram'

         ! Répertoire, cas non trouvé
         if (index(dir,' ').le.1) dir = 'GRAM_DATA/'
         if (index(file,' ').le.1) file = trim(dir)//'input_gram'

         ! sous-repertoire des fichiers de données binaires
         dir = trim(dir) // "/MGbindat/"

!       Initialize data and read input file with setup subroutine
!       See Mars-GRAM code for description of NAMELIST format input
!       file
        call setup_2001(CHGT,CLAT,CLON,CSEC,DATE0,RHOd,RHOu,RHOv,DELHGT, &
           DELLAT,DELLON,DELTIME,maxnum,nr1,nmonte,0.d0,0.d0,LonEW, &
           file,6,iu,hgtasfc,dir)
!
!     Forced initialization of some parameters
        NPOS = 1
        LonEW = 0
        MOLAhgts = 1
        numwave = 0
!
        first_call = .false.
!
      endif

!     Integrity Tests

!     === dust scenario
!     check value of dust scenario Viking or customized
!     insure dusttau value within proper range
      if (scena.eq.1) then
         Dusttau = 0.
      else if (scena.eq.2) then
         if (dust.gt.0.d0) then
            if (dust.lt.0.1d0) then
               write(*,*)'ATMMARSGRAM Warning: Must have dusttau>=0.1'
               write(*,*)'                     set to 0.1'
               Dusttau = 0.1
            else if (dust.gt.3.d0) then
               write(*,*)'ATMMARSGRAM Warning: Must have dusttau<=3.0'
               write(*,*)'                     set to 3.0'
               Dusttau = 3.0
            else
               Dusttau = real(dust)
            endif
         else
            write(*,*)'ATMMARSGRAM Warning: Must have positive dusttau'
            write(*,*)'                     set to default Viking tau'
            Dusttau = 0.
         endif
      else
         write(*,*)'ATMMARSGRAM Error: Unknown dust scenario'
         ier=1
         goto 9999
      endif

!     === perturbations
      if ((disp.lt.1).OR.(disp.gt.2)) then
         write(*,*)'ATMMARSGRAM Error: Unknown perturbation type'
         ier=2
         goto 9999
      end if

!     === multiplicative factor for perturbations
!     insure rpscale value within proper range
      if ((rpscale.lt.0.d0).or.(rpscale.gt.2.d0)) then
         write(*,*)'ATMMARSGRAM Warning: Must have 0.<=rpscale<=2.'
         write(*,*)'                     set to default = 1.'
         rpfactor = 1.
      else
         rpfactor = real(rpscale)
      endif

!     convert latitude from radians to degrees
      Lat = real(xlat*crd,kind=pm_reel)
!     convert longitude (radians) East to longitude (degrees) West between 0..360 degrees
      LonW = real(xlon*crd)
      LonW = mod(LonW,360.d0)
      if(LonW.lt.0.) then
         LonW = - LonW
      elseif (LonW.gt.0.) then
         LonW = 360. - LonW
      endif

!     Rmola   : Mars areoid radius (km) from 1/2 by 1/2 degree MOLA data
!     Rell    : Mars radius of reference ellipsoid (km)
!     topohgt : Orographic height (km)
!     ( unused outputs
!         gz  : Acceleration of gravity including J2 and centrifugal terms
!         alb : Mars surface albedo )
      call cps_rellips_2001(Lat,LonW,Rmola,0d0,gz,Rell,topohgt,alb)

!     Altitude z wrt MOLA areoid (m)
      z = r - dble(Rmola)*1.d3

!     check positive height above ground (m) else stop
      absheight = z - topohgt*1.d3
      if (absheight.lt.0.d0) then
         write(*,*)'ATMMARSGRAM Error: Underground object'
         ier=3
!         goto 9999
      endif

      if (init_atm) then
!
         if (disp.eq.1) then
!           seed is ignored
            RHOd = 0.0
            RHOu = 0.0
            RHOv = 0.0
         else if (disp.eq.2) then
!       Warning if the seed number is outside [1 - 29999] and
!       new seed max(mod(abs(seed),30000),1) is used
            if ((seed.LE.0) .OR. (seed.GE.30000)) then
               write(*,'("Seed number (",i5,") is outside prescribed range")') seed
               seed = max(mod(abs(seed),30000),1)
               write(*,'("Replaced by seed (",i5,")")') seed
            endif

            call randinit_2001(0,seed,RHOd,RHOu,RHOv)

         endif

!       Current position and date at initial time
         CHGT = real(z/1.d3)
         CLAT = Lat
         CLON = LonW
         CSEC = 0.
!       Initial Date (dble prec)
         DATE0 = xdate
         call datastep_2001(0,CHGT,CLAT,CLON,CSEC,DATE0,RHOd,RHOu,RHOv, &
             eof,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS, &
             DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,Hpres, &
             0.d0,0.d0,0.d0,0.d0,LonEW,corlim,DENSTOT,int(numwave),hgtasfc, &
             ALS,TLOCAL,DENS0,SIG,PSURF,TSURF)

         init_atm = .false.

      else
!
!       Compute displacement magnitude of new from previous position and date
!
         DELHGT = real(real(z/1.d3) - CHGT,kind=4)
         DELLAT = real(Lat - CLAT,kind=4)
         DELLON = real(LonW - CLON,kind=4)
         DELTIME = real(real((xdate - DATE0)*8.64d4) - CSEC,kind=4)

         Call datastep_2001(1,CHGT,CLAT,CLON,CSEC,DATE0,RHOd,RHOu,RHOv,  &
             eof,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS,     &
             DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,Hpres,        &
             0.d0,0.d0,0.d0,0.d0,LonEW,corlim,DENSTOT,int(numwave),hgtasfc, &
             ALS,TLOCAL,DENS0,SIG,PSURF,TSURF)

!-----------------------------------------------------------
!    DATASTEP_2001 Outputs :
!-----------------------------------------------------------
!       TEMP   = temperature (K)
!       PRES   = average (mean plus wave-perturbed) pressure (N/m**2)
!       DENSLO = nominal low density (kg/m**3), approx. -1 sigma
!       DENS   = average (mean plus wave-perturbed) density (kg/m**3)
!       DENSHI = nominal high density (kg/m**3), approx. +1 sigma
!       DENSP  = density perturbation (% of unperturbed mean)
!       EWWIND = mean eastward wind component (m/s)
!       EWpert = perturbation in eastward wind component (m/s)
!       NSWIND = mean northward wind component (m/s)
!       NSpert = perturbation in northward wind component (m/s)
!       Hrho   = density scale height (km)
!       Hpres  = pressure scale height (km)
!       DENSTOT= total density (mean plus perturbed) (kg/m**3)
!       ALS    = Areocentric longitude of the Sun (deg)
!       TLOCAL = local time in "Martian hours" (1 hr = 88775.245/24 sec)
!       DENS0  = Unperturbed mean density (kg/m**3)
!       SIG    = Standard deviation in random density perturbation (% of unperturbed mean)
!       PSURF  = atmospheric pressure at surface (N/m**2)
!       TSURF  = ground surface temperature (K)
!-----------------------------------------------------------

      endif

      pressure = dble(PRES)
      temperature = dble(TEMP)

      if (disp.eq.1) then
         ro = dble(DENS)
         windu = dble(EWWIND)
         windv = dble(NSWIND)
      else
         ro = dble(DENSTOT)
!         ro = dble(DENS*(1.+0.01*DENSP))
         windu = dble(EWWIND+EWpert)
         windv = dble(NSWIND+NSpert)
      endif

!     store mean values
      meanvar(1) = pres
      meanvar(2) = dble(DENS0)
      meanvar(3) = temperature
      meanvar(4) = dble(EWWIND)
      meanvar(5) = dble(NSWIND)

!     store values of extra variables
      extvar(1) = dble(DENSHI)
      extvar(2) = dble(DENSLO)
!     density standard deviation (kg/m^3)
      extvar(3) = dble(SIG/100.*DENS0)
      extvar(4) = dble(DENSP/100.*DENS)
      extvar(5) = dble(Hpres)
      extvar(6) = dble(Hrho)

      extvar(7) = -999.0d0
      extvar(8) = -999.0d0
      extvar(9) = -999.0d0

      extvar(10) = dble(TSURF)
      extvar(11) = -999.0d0
      extvar(12) = -999.0d0

      extvar(13) = dble(DENSTOT-DENS)

!     Orographic Height (m)
      extvar(14) = dble(topohgt)*1.d3

      extvar(15) = dble(PSURF)

      extvar(16) = -999.0d0
      extvar(17) = -999.0d0
      extvar(18) = -999.0d0
      extvar(19) = -999.0d0
      extvar(20) = -999.0d0
      extvar(21) = -999.0d0
      extvar(22) = -999.0d0
!
!     areocentric longitude of the Sun Ls (deg)
      extvar(23) = dble(ALS)
!     local solar time (hrs)
      extvar(24) = dble(TLOCAL)
!     universal time (hrs) (=local time at longitude 0 deg)
      UTIME = real(mod(TLOCAL + CLON/15.d0 + 24.d0,24.d0),kind=4)
      extvar(25)=dble(UTIME)

      return

!     Error handling : all the outputs are set to a missing data value
 9999 pressure    = -999.d0
      ro          = -999.d0
      temperature = -999.d0
      windu       = -999.d0
      windv       = -999.d0
      do i = 1,5
         meanvar(i) = -999.d0
      end do
      do i = 1,25
         extvar(i) = -999.d0
      enddo

      return
    end subroutine cps_atmmarsgram_2001


!!! Sous-programmes locaux des fichiers marssubs_2001.f

!     Subroutines and Functions for
!     Mars-GRAM 2001 - Version 1,  July, 2001
!.......................................................................
!
    SUBROUTINE ATMOS2_2001 (HGTIN,CLAT,CLON,MARSAU,SUNLAT,SUNLON,ALS,   &
      H,TEMP,DENST,UPFCTR,LWFCTR,PRES,thgt,careoid,ZF,deltaTEX,         &
      Texos,Tbase,Hrho,AMz,Dusttau,DustOD,EWWIND,NSWIND,HatZF,          &
      wavepert,TempDay,PresDay,DensDay,EWwnDay,NSwnDay,hgtasfc,         &
      Patsurf,Tempmax,Tempmin,Densmax,Densmin,Tgrnd,talb,icepolar,      &
      gz,Oldrref)

      REAL(kind=PM_REEL) :: HGTIN,SUNLON,ALS,PresDay,Tgrnd,Patsurf,Hrho,H
      REAL(kind=PM_REEL) :: LWFCTR,MARSAU,NSWIND,LWFCTR1,NSwnDay,NSwnDay1
      real(kind=PM_REEL) :: Oldrref,ZF,CLAT,CLON,PRESLO,TEMPLO,TF,Dusttau
      real(kind=PM_REEL) :: TINF,AMLO,sunlat,DENSLO,DENSHI,Shgt,AMHI,hgtasfc
      real(kind=PM_REEL) :: TIME,PRESHI,TEMPHI,PRES,TEMP,DENST,wavepert
      real(kind=PM_REEL) :: Vdx,Tmaxx,Tminx,talbx,ZFp,tophgtX,areoidX,Tdx
      real(kind=PM_REEL) :: deltaTEX
      real(kind=PM_REEL) :: F107,stdl
      COMMON /THERM_2001/F107,stdl
!!
!!     HGTIN    SPACECRAFT HEIGHT ABOVE MOLA AREOID (KM)(INPUT)
!!     CLON     WEST LONGITUDE OF SPACECRAFT (DEGREES) (INPUT)
!!     MARSAU   MARS ORBITAL RADIUS (AU) (INPUT)
!!     SUNLAT   AREOCENTRIC LATITUDE OF SUN (DEGREES) (INPUT)
!!     SUNLON   MARS WEST LONGITUDE OF SUN (DEGREES) (INPUT)
!!     ALS      AREOCENTRIC LONGITUDE OF SUN ORBIT (INPUT)
!!     H        SCALE HEIGHT AT SPACECRAFT POSITION (KM) (OUTPUT)
!!     TEMP     TEMPERATURE AT SPACECRAFT POSITION (K) (OUTPUT)
!!     DENST    MASS DENSITY AT SPACECRAFT POSITION (KG/M**3) (OUTPUT)
!!     UPFCTR   UPPER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)
!!     LWFCTR   LOWER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)
!!     PRES     PRESSURE AT SPACECRAFT POSITION (N/M**2) (OUTPUT)
!!     thgt     LOCAL SURFACE HEIGHT RELATIVE TO MOLA AREOID (KM)
!!              (INPUT)
!!     careoid  MOLA 1/2 degree areoid (km) (OUTPUT)
!!     ZF       Local height of base of thermosphere (OUTPUT)
!!     deltaTEX adjustment in exospheric temperature (K) (INPUT)
!!     Texos    local exospheric temperature (K) (OUTPUT)
!!     Tbase    local temperature for base of exosphere (K) (OUTPUT)
!!     Hrho     density scale height (km) (OUTPUT)
!!     AMz      molecular weight (OUTPUT)
!!     Dusttau  dust optical depth (or 0 for Viking-like annual
!!               variation) (INPUT)
!!     DustOD   dust optical depth (OUTPUT)
!!     EWWIND   Eastward wind component (m/s) (OUTPUT)
!!     NSWIND   Northward wind component (m/s) (OUTPUT)
!!     HatZF    pressure scale height at altitude ZF (km) (OUTPUT)
!!     wavepert perturbation (% of mean) from longitude-dependent wave
!!     TempDay  daily average temperature (K) (OUTPUT)
!!     PresDay  daily average pressure (N/m**2) (OUTPUT)
!!     DensDay  daily average density (kg/m**3) (OUTPUT)
!!     EWwnDay  daily average eastward wind component (m/s) (OUTPUT)
!!     NSwnDay  dailt average northward wind component (m/s) (OUTPUT)
!!     hgtasfc  height above surface for evaluation of boundary layer
!!               variables (km) (INPUT)
!!     Patsurf  atmospheric pressure at surface (N/m**2) (OUTPUT)
!!     Tempmax  daily maximum temperature (K) (OUTPUT)
!!     Tempmin  daily minimum temperature (K) (OUTPUT)
!!     Densmax  daily maximum density (kg/m**3) (OUTPUT)
!!     Densmin  daily minimum density (kg/m**3) (OUTPUT)
!!     Tgrnd    ground surface temperature (K) (OUTPUT)
!!     talb     surface albedo (OUTPUT)
!!     icepolar polar ice indicator (0=no, 1=yes) (OUTPUT)
!!
!!.....................................................................
!!
      real(kind=PM_REEL) :: AMz,DustOD,Tempmax1,Tempmin1,careoid,Tdz
      real(kind=PM_REEL) :: AMz1,TempDay1,thgt,TEMP1,PRES1,DENST1,talb
      real(kind=PM_REEL) :: Tempmax,Tempmin,TempDay,CHGT
      real(kind=PM_REEL) :: talbz,Tdayz,Vdayz,Tmaxz,Tminz,ZFz,gz
      real(kind=PM_REEL) :: Tgrdnx
      real(kind=PM_REEL) :: TEMPz,UX,PX,DX,EWWIND,VX,DENSTz,PRESz
      real(kind=PM_REEL) :: PresDay1,Pdx,Pdayz,Hrlo,Hrhi,Tgrnd1,Hrho1
      real(kind=PM_REEL) :: Hrhoz,Tgrndz,HDX,Hz,HatZF, H1, HHi,HLO
      integer :: ice1,ICEZ, icex,icepolar

!!...  Min and max perturbation factors in thermosphere
      real(kind=PM_REEL), parameter :: pertmax = 0.30_PM_REEL
      real(kind=PM_REEL), parameter :: pertmin = 0.15_PM_REEL
!!...  Rstar = Universal gas constant
!!...  AMW = average molecular weight up to turbopause
      real :: RSTAR,AMW
      DATA RSTAR,AMW/8.31439E3,43.49/
!!...  pi/2
      pitwo = 2.*Atan(1.)
!!...  Local solar time (Mars hours = 1/24th Sol)
      TIME = 12. + (SUNLON - CLON)/15.
      IF (TIME .LT. 0.0)TIME = TIME + 24.
      IF (TIME .GT. 24.)TIME = TIME - 24.
!!...  Evaluate dust optical depth
      If (Dusttau.gt.0.0)Then
        DustOD = Dusttau
        If (DustOD.lt.0.1)DustOD = 0.1
      Else
        DustOD = dustvsls_2001(ALS)
      Endif
!!...  Evaluate factor for dust storm model
      Call Dustfact_2001(CLAT,CLON,als,dustM,stormdz)
      DustOD = DustOD + dustM
!!...  Evaluate MTGCM height offset due to dust storm
      dustoffset = stormdz
!!...  Get areoid radius and topographic height at current lat, lon
      Call cps_RELLIPS_2001(clat,clon,careoid,HGTIN,gz,Oldrref,thgt,talb)
!!...  Evaluate pressure and temperature at surface, Patsurf and Tat5m
      Tat5m   = 0.0
      Call MarsGCM_2001(thgt,CLAT,CLON,ALS,DustOD,TIME,TEMPz,PRESz,     &
        DENSTz,EWWIND,NSWIND,Hz,Hrhoz,ZFz,tfactor,thgt,hgtasfc,careoid, &
        Tdayz,Pdayz,Ddayz,Udayz,Vdayz,Tmaxz,Tminz,Dmaxz,Dminz,Tgrndz,   &
        talbz,icez,Tat5m,dustoffset)
      Patsurf = PRESz
!!...  Adjust Patsurf for wave perturbation
      Patsurf = Patsurf*(1. + wavepert)
!!...  Tat5m is air temperature at surface + 5 meters
!!...  Set evaluation hgt (CHGT) to HGTIN or to thgt+hgtasfc if HGTIN <=
!!     -10. km
      CHGT = HGTIN
      If (HGTIN.le.-10.)Then
        CHGT = thgt + hgtasfc
      Endif
!!...  Compute atmosphere using only MGCM data if height below 80 km
      If (CHGT.LE.80.)Then
        Call MarsGCM_2001(CHGT,CLAT,CLON,ALS,DustOD,TIME,TEMP,PRES,     &
          DENST,EWWIND,NSWIND,H,Hrho,ZF,tfactor,thgt,hgtasfc,careoid,   &
          TempDay,PresDay,DensDay,EWwnDay,NSwnDay,Tempmax,Tempmin,      &
          Densmax,Densmin,Tgrnd,talb,icepolar,Tat5m,dustoffset)
!!...    Adjust PRES, and DENST for wave perturbation
        PRES = PRES*(1. + wavepert)
        DENST = DENST*(1. + wavepert)
        DensDay = real(DensDay*(1. + wavepert),kind=4)
        Densmax = real(Densmax*(1. + wavepert),kind=4)
        Densmin = real(Densmin*(1. + wavepert),kind=4)
        HatZF = 0.
        UPFCTR = 1. + tfactor
        LWFCTR = 0.0
        AMz = AMW
      Else If (CHGT.LT.170.)Then
!!...    This section does fairing between MTGCM and Stewart models,
!!       as necessary (i.e. if height between ZF and 170 km)
!!...    Evaluate MTGCM at current height
        Call MarsGCM_2001(CHGT,CLAT,CLON,ALS,DustOD,TIME,TEMP,PRES,     &
          DENST,EWWIND,NSWIND,H,Hrho,ZF,tfactor,thgt,hgtasfc,careoid,   &
          TempDay,PresDay,DensDay,EWwnDay,NSwnDay,Tempmax,Tempmin,      &
          Densmax,Densmin,Tgrnd,talb,icepolar,Tat5m,dustoffset)
!!...    Adjust PRES, and DENST for wave perturbation
        PRES = PRES*(1. + wavepert)
        DENST = DENST*(1. + wavepert)
        DensDay = real(DensDay*(1. + wavepert),kind=4)
        Densmax = real(Densmax*(1. + wavepert),kind=4)
        Densmin = real(Densmin*(1. + wavepert),kind=4)
!!...    Compute molecular weight
        AMz = DENST*RSTAR*TEMP/PRES
!!...    Find temperature TF at height ZF = altitude of 1.26 nbar level
        If (ZF.gt.900.)Then
          TF = 999.9
        Else
          Call MarsGCM_2001(ZF,CLAT,CLON,ALS,DustOD,TIME,TF,PX,DX,           &
               UX,VX,HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX,Tdz,Pdx,Ddx,  &
               Udx,Vdx,Tmaxx,Tminx,Dmaxx,Dminx,Tgrdnx,talbx,icex,Tat5m,      &
          dustoffset)
!!...      Adjust ZF for wave perturbation
          ZF = ZF + HatZF*Dlog(1.d0 + wavepert)
        Endif
!!...    Save temperature at 1.26 nbar level
        Tbase = real(TF,kind=4)
        UPFCTR = 1. + tfactor
        LWFCTR = 1. - tfactor
        Shgt = CHGT
        If (CHGT.lt.ZF)Shgt = ZF
!!...    Evaluate thermospheric parameters at current height
        Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRES1,TEMP1,DENST1,    &
             Shgt,Rstar,H1,AMz1,0.0,sunlat, deltaTEX,TINF,TF,ZF,Hrho1)

!!...    Save exospheric temperature
        Texos = real(TINF,kind=4)
!!...    Do fairing as necessary if height above ZF and below 170 km
        If (CHGT.ge.ZF)Then
!!...      Evaluate thermospheric parameters at deviation=+1, for DENSHI
          Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRESHI,TEMPHI,       &
          DENSHI,Shgt,Rstar,HHI,AMHI,1.0,sunlat,                        &
          deltaTEX,TINF,TF,ZF,Hrhi)
!!...      Evaluate thermospheric parameters at deviation=-1, for DENSLO
          Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRESLO,TEMPLO,        &
          DENSLO,Shgt,Rstar,HLO,AMLO,-1.0,sunlat,                        &
          deltaTEX,TINF,TF,ZF,Hrlo)
          UPFCTR1 = real(DENSHI/DENST1,kind=4)
          LWFCTR1 = DENSLO/DENST1
          If (UPFCTR1 .lt. LWFCTR1) then
            UPFCTR1 = real(DENSLO/DENST1,kind=4)
            LWFCTR1 = DENSHI/DENST1
          Endif
!!...      Do fairing between MGCM and Stewart over heights between 155
!!         and 170 km
          fairfact = 1.
          If (CHGT.ge.155.) then
            fairfact = real((Cos((CHGT-155.)*pitwo/15.))**2,kind=4)
          end if
          TEMP = TEMP*fairfact + TEMP1*(1. - fairfact)
          PRES = PRES*fairfact + PRES1*(1. - fairfact)
          DENST = DENST*fairfact + DENST1*(1. - fairfact)
          H = H*fairfact + H1*(1. - fairfact)
          Hrho = Hrho*fairfact + Hrho1*(1. - fairfact)
          AMz = AMz*fairfact + AMz1*(1. - fairfact)
          UPFCTR = UPFCTR*fairfact + UPFCTR1*(1. - fairfact)
          LWFCTR = LWFCTR*fairfact + LWFCTR1*(1. - fairfact)
        Endif
      Else
!!...    For height above 170 km -  Use MTGCM data to get (unperturbed)
!!       value of height ZF = height of 1.26 nbar level
        Call MarsGCM_2001(170d0,CLAT,CLON,ALS,DustOD,TIME,TEMP1,PRES1,  &
          DENST1,EWWIND,NSWIND,H1,Hrho1,ZF,tfactor,thgt,hgtasfc,        &
          careoid,TempDay1,PresDay1,DensDay1,EWwnDay1,NSwnDay1,         &
          Tempmax1,Tempmin1,Densmax1,Densmin1,Tgrnd1,talb,ice1,         &
          Tat5m,dustoffset)
!!...    Find temperature TF at height ZF
        Call MarsGCM_2001(ZF,CLAT,CLON,ALS,DustOD,TIME,TF,PX,DX,        &
          UX,VX,HatZF,HDX,ZFp,UPF,tophgtX,hgtasfc,areoidX,Tdx,Pdx,Ddx,  &
          Udx,Vdx,Tmaxx,Tminx,Dmaxx,Dminx,Tgrnd,talbx,icex,Tat5m,      &
          dustoffset)
!!
!!...    Set daily average values to zero above 170 km
        TempDay = 0.0
        PresDay = 0.0
        DensDay = 0.0
        EWwnDay = 0.0
        NSwnDay = 0.0
!!...    Set daily max, min Temp and Density to zero above 170 km
        Tempmax = 0.0
        Tempmin = 0.0
        Densmax = 0.0
        Densmin = 0.0
!!...    Adjust ZF for wave perturbation, using scale height at ZF
        ZF = ZF + HatZF*Dlog(1.d0 + wavepert)
!!...    Save temperature at 1.26 nbar level
        Tbase = real(TF,kind=4)
        Shgt = CHGT
!!...    Evaluate thermospheric parameters at current height
        Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRES,TEMP,DENST,       &
             Shgt,Rstar,H,AMz,0.0,sunlat,                               &
             deltaTEX,TINF,TF,ZF,Hrho)
!!...    Save exospheric temperature
        Texos = real(TINF,kind=4)
!!...    Evaluate thermospheric parameters at deviation=+1, for DENSHI
        Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRESHI,TEMPHI,         &
             DENSHI,Shgt,Rstar,HHI,AMHI,1.0,sunlat,                     &
             deltaTEX,TINF,TF,ZF,Hrhi)
!!...    Evaluate thermospheric parameters at deviation=-1, for DENSLO
        Call STEWART2_2001(MARSAU,CLAT,CLON,TIME,PRESLO,TEMPLO,         &
             DENSLO,Shgt,Rstar,HLO,AMLO,-1.0,sunlat,                    &
             deltaTEX,TINF,TF,ZF,Hrlo)
        UPFCTR = real(DENSHI/DENST,kind=4)
        LWFCTR = DENSLO/DENST
        If (UPFCTR .lt. LWFCTR) then
          UPFCTR = real(DENSLO/DENST,kind=4)
          LWFCTR = DENSHI/DENST
        Endif
      Endif
!!...  Insure thermospheric variability factors are in allowable ranges
      If (LWFCTR.gt.0.0)Then
        If (UPFCTR.gt.1.+pertmax)UPFCTR = real(1.+pertmax,kind=4)
        If (UPFCTR.lt.1.+pertmin)UPFCTR = real(1.+pertmin,kind=4)
        If (LWFCTR.lt.1.-pertmax)LWFCTR = 1.-pertmax
        If (LWFCTR.gt.1.-pertmin)LWFCTR = 1.-pertmin
        If (tfactor.gt.pertmax)tfactor = real(pertmax,kind=4)
        If (tfactor.lt.pertmin)tfactor = real(pertmin,kind=4)
      Endif
      Return
    End SUBROUTINE ATMOS2_2001
!!-----------------------------------------------------------------------
    Subroutine bltp_2001(gz,Cp,Tg,z5,T5,U5,V5,zeval,factor,Tempz)
!!
!!...    Mars boundary layer temperature from methods used in the NASA
!!       Ames Mars General Circulation Model (MGCM), as described by
!!       Haberle et al., Jour. Geophys. Res. 104(E4), 8957-8974, 1999,
!!       (referred to as H99 below).
!!
!!...    Input parameters:
!!       gz = local acceleration of gravity (m/s**2)
!!       Cp = specific heat at constant pressure (joules kg**-1 K**-1)
!!       Tg = temperature (K) at ground surface
!!       z5 = first MGCM height level (nominally 5 m)
!!       T5 = temperature (K) at first MGCM level (nominally 5 m)
!!       U5 = zonal wind (m/s) at first MGCM level
!!       V5 = meridional wind (m/s) at first MGCM level
!!       zeval = height (m) at which to evaluate temperature
!!       factor = factor for calculations = Alog(zeval/z0)/Alog(5./z0),
!!                where z0 is surface roughness parameter (m)
!!...    Output parameter:
!!       Tempz = temperature (K) at height zeval
!!.....................................................................
      real(kind=PM_REEL) :: gz,Cp,Tg,z5,T5,U5,V5,zeval,factor,Tempz
! Variables locales
      integer :: i

!!
!!...    Set some initial values for iterative solution
        sqrtFh = 1.
        dT = real(T5 - Tg,kind=4)
        T0 = real(T5,kind=4)
!!...    Potential temperature at first MGCM level
        thet5 = real(T5 + gz*z5/Cp,kind=4)
!!...    Iterative calculation of boundary layer parameters
        Do 10 i = 1,10
!!...      Richardson number from temperature and wind gradients
          udenom = real(U5**2+V5**2,kind=4)
          If (udenom.le.0.1)udenom = 0.1
          Ri =real((gz*sqrtFh/thet5)*((thet5-Tg)/(1.+sqrtFh))*z5/udenom,kind=4)
!!...      Next iteration of temperature solution by method in Section
!!         4 of H99 (convert from potential to regular temperature)
          Tempz = Tg +(thet5-Tg)*(1. + sqrtFh*factor)/(1. + sqrtFh)      &
           - gz*zeval/Cp
!!...      Change in temperature from previous iteration
          dT = real(Tempz - T0,kind=4)
          T0 = real(Tempz,kind=4)
!!...      End iteration if sufficient temperature precision achieved
          If (Abs(dT).lt.0.01)Return
!!...      Next iteration of Sqrt(Fh), where Fh is stability function
!!         from Section 4 of H99
          If (Ri.lt.0.0)Then
            sqrtFh = (1. - 16.*Ri)**0.25
          Else
            sqrtFh = (1. + 15.*Ri/Sqrt(1. + 5.*Ri))**(-0.5)
          Endif
  10   Enddo
       Return
     End Subroutine bltp_2001
!!-----------------------------------------------------------------------
     Subroutine cospar_2001(z,t,p,rho)
!      real(kind=PM_REEL) :: z,t,p,rho
!!
!!     COSPAR N.H. mean temperature (t, K), pressure (p, N/m**2) and
!!     density (rho, kg/m**3) versus height (z, km)
!!     Note: input pressures (pc) are mb, densities (dc) are g/cm**3
!!
!!     COSPAR values from Table XI, "The Mars Atmosphere: Observations
!!     and Model Profiles for Mars Missions", David E. Pitts et al.,
!!     eds., JSC-24455
!!
! Common cosparnh_2001
      real(kind=PM_REEL) :: zc(164),tc(164),pc(164),dc(164)
      Common /cosparnh_2001/zc,tc,pc,dc

! Variables locales
      real(kind=PM_REEL) :: R,R2,R1,H,aexp,dz
      integer :: iz
!!
!!     1 km interval from -10 to 130 km, 10 km interval 130-360 km
      If (z.lt.130.)Then
        iz = Int(z+11.)
      Else
        iz = 141 + Int((z-130.)/10.)
      Endif
!!     Set values to 0 if z out of range
      If (iz.lt.1.or.iz.gt.164)Then
        t = 0.
        p = 0.
        rho = 0.
        Return
      Endif
      If (iz.gt.163)iz=163
!!     Linear interpolation on temperature
      dz = (z - zc(iz))/(zc(iz+1) - zc(iz))
      t = real(tc(iz) + (tc(iz+1) - tc(iz))*dz,kind=4)
!!     Pressure from hydrostatic relation (with special isothermal case)
      If (Abs(tc(iz+1)-tc(iz)).gt.0.01)Then
        aexp = Dlog(pc(iz+1)/pc(iz))/Dlog(tc(iz+1)/tc(iz))
        p = real(100.*pc(iz)*(t/tc(iz))**aexp,kind=4)
      Else
        H = (zc(iz+1) - zc(iz))/Dlog(pc(iz)/pc(iz+1))
        p = real(100.*pc(iz)*exp(-(z-zc(iz))/H),kind=4)
      Endif
!!     Linear interpolation on gas constant
      R1 = pc(iz)/(dc(iz)*tc(iz))
      R2 = pc(iz+1)/(dc(iz+1)*tc(iz+1))
      R = R1 + (R2-R1)*dz
!!     density from perfect gas law (and convert units to kg/m**3)
      rho = real(10.*p/(R*t),kind=4)
      Return
    End SubroutineSubroutine cospar_2001
!!-----------------------------------------------------------------------
    real(kind=PM_REEL) Function Cp_2001(T)
      real(kind=PM_REEL) :: T
!!...  Specific heat at constant pressure, as function of temperature
!!...  T in kelvins; Cp in joules kg**-1 K**-1
      Cp_2001 = 639.5 + 0.123687*T + 0.00200225*T*T
      Return
    End Function Cp_2001
!!-----------------------------------------------------------------------
    Subroutine Dustfact_2001(CLAT,CLON,als,dustM,stormdz)
!!...  Computes dust storm intensity factor dustM
!!...  as a function of the time since start of the storm,
!!...  (als - als0), measured in Ls angle (degrees), and as a
!!...  function of the storm intensity, intens.  dustM is for
!!...  magnitude of effect on dust optical depth (0.0 - 3.0).
      real(kind=PM_REEL) :: CLAT,CLON,als

!     common Variables in DATACOM_2001
      real (kind=PM_REEL) :: DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,&
           intens, deltaTEX,rpscale,Dusttau,DustOD,Dustnu,Dustdiam,Dustdens
      integer :: NPOS,NVARX,NVARY,logscale,iu0,iup,maxfiles,MOLAhgtsn

      COMMON /DATACOM_2001/NPOS,NVARX,NVARY,logscale, iu0,iup,maxfiles,&
          MOLAhgtsn,DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          deltaTEX,rpscale,Dusttau,DustOD,Dustnu,Dustdiam, &
          Dustdens

!
      dls = real(als - als0,kind=4)
      dlsmax = real(alsdur,kind=4)
      If (dlsmax.lt.12.)dlsmax = 12.
      If (dlsmax.gt.48.)dlsmax = 48.
!!...  Return dust factor of 0 if Ls-Ls0 < 0 or > dlsmax degrees
      If (dls .le. 0.0 .or. dls .gt. dlsmax .or. intens .le. 0.0)Then
        dustM = 0.0
        stormdz = 0.0
        Return
      Endif
!!...  Compute initial dustM factor (0-1) from time (Ls) profile
      If (dls .le. dlsmax/8.)then
        dustM = 8.*dls/dlsmax
      Else If (dls .ge. dlsmax/2.)then
        dustM = 2.*(1. - dls/dlsmax)
      Else
        dustM = 1.0
      Endif
      sizefact = 1.0
!!...  Compute parameters of local storm if radmax is not 0
      If (radmax .ne. 0.0)Then
        sizefact = 0.0
!!...    dns,dew,rad = horizontal coordinates from center of dust storm
        dns = real(DTR*Rref*(CLAT - dustlat),kind=4)
        dlon = real(Abs(CLON -dustlon),kind=4)
        If (dlon.gt.180.)dlon = 360. - dlon
        dew = real(DTR*Rref*cos(DTR*CLAT)*dlon,kind=4)
        rad = Sqrt(dns**2 + dew**2)
!!...    raddust = actual horizontal size of storm
        raddust = real(dustM*radmax,kind=4)
!!...    sizefact = position-dependent measure of relative storm effect
        If (rad .lt. 2.0*raddust)                                        &
        sizefact = real(0.5*(1.0 + cos(90.*DTR*rad/raddust)),kind=4)
      Endif
!!...  Final factor dependent on position and on storm intensity
      dustM = sizefact*dustM
      stormdz = real(5.*dustM*Sqrt(intens),kind=4)
      dustM = real(dustM*intens,kind=4)
      Return
    End Subroutine Dustfact_2001
!!-----------------------------------------------------------------------
    Subroutine Datastep_2001(I,CHGT,CLAT,CLON,CSEC,DATE0,RHOd,RHOu,     &
      RHOv,EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,DENS,      &
      DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,Hrho,HSCALE,dsunlat,     &
      dsunlon,dsunLs,dradau,LonEW,corlim,DENSTOT,numwave,hgtasfc,       &
!!     - Ajout de variables de sortie 06/2002 -
      ALS,TLOCAL,DENS0,SIGD,Patsurf,Tgrnd)
!!     If input parameter MOLAhgts is 1 CHGT is height above the MOLA
!!     areoid; otherwise CHGT is height above reference ellipsoid,
!!     in which case height is converted to HGTM, height above MOLA
!!     areoid for call to subroutine ATMOS2.
      REAL(KIND=PM_REEL) :: DATE,DATE0,dsunlat,dsunlon,dsunLs,dradau, Hrho
      real(kind=PM_REEL) :: TLOCAL,DENS0,SIGD,Patsurf,Tgrnd,corlim,EWpert
      INTEGER  :: I, EOF,numwave, LonEW
      REAL (KIND=PM_REEL) :: MARSAU,NSWIND,NSpert,NStot,NSwnDay,DENSTOT
      REAL (KIND=PM_REEL) :: hgtasfc,DENSHI,DENSP,DENSLO,CSEC
      REAL (KIND=PM_REEL) :: HSCALE, correl
      Character*4 :: lonvar
      Character*8 :: densunits
      ! variables locales
      REAL(KIND=PM_REEL) :: ssposr2, temp

! Common cosparnh_2001
      real(kind=PM_REEL) :: zc(164),tc(164),pc(164),dc(164)
      Common /cosparnh_2001/zc,tc,pc,dc

!     common Variables in DATACOM_2001
      real (kind=PM_REEL) :: DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          deltaTEX,rpscale,Dusttau,DustOD,Dustnu,Dustdiam,Dustdens
      integer :: NPOS,NVARX,NVARY,logscale,iu0,iup,maxfiles,MOLAhgts

      COMMON /DATACOM_2001/NPOS,NVARX,NVARY,logscale, iu0,iup,maxfiles,MOLAhgts,&
          DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          deltaTEX,rpscale,Dusttau,DustOD,Dustnu,Dustdiam, Dustdens

! common FILENAME_2001
      Character*12 :: lstfl,outfl
      COMMON /FILENAME_2001/lstfl,outfl


! common Wavecoef_2001
      REAL(KIND=PM_REEL) :: WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
           WaveA3,Wavephi3,Wscale
      integer :: nwave,iuwave
      real :: wavetime(100),wavedata(100,7)
      Common /Wavecoef_2001/WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
           WaveA3,Wavephi3,wavetime,wavedata,nwave,iuwave,Wscale
      integer, parameter :: ndust = 3


! common TGCMoffset_2001
      REAL(KIND=PM_REEL) :: offsets(0:12,ndust),zoffset,hgtoffset
      integer :: ibougher
      Common /TGCMoffset_2001/offsets,zoffset,hgtoffset,     &
           ibougher
      REAL(KIND=PM_REEL), parameter :: onesol = 88775.245_PM_REEL
      REAL(KIND=PM_REEL) :: FACTLO,albedo,HGTM,wavepert
      real(kind=PM_REEL) :: SUNLAT,SUNLON,ALS,DENS, pres
      REAL(KIND=PM_REEL) :: CLAT,CLON,CHGT,ogz,Oldrref,topohgt,ZF
      REAL(KIND=PM_REEL) :: AMz,careoid,TempDay,PresDay,talb,gz
      REAL(KIND=PM_REEL) :: Tempmax,Tempmin,thgt,EWWIND,HatZF

      ! fonctions
!      REAL(KIND=PM_REEL) :: Cp_2001
!
      Dlogdens=0.0
      EOF = 0
!!...  Get radius RSC to current position and convert height to MOLA
      Call cps_RELLIPS_2001(CLAT,CLON,Rref,CHGT,ogz,Oldrref,topohgt,albedo)
      If (MOLAhgts.ne.1)Then
        RSC = real(Oldrref + CHGT,kind=4)
        HGTM = RSC - Rref
      Else
        RSC = real(RREF + CHGT,kind=4)
        HGTM = CHGT
      Endif
!!...  Set vertical and horizontal scale parameters
      VLS = 8.0
      HLS = real(30. + 0.01875*HGTM**2,kind=4)
      If (HLS.gt.600.)HLS = 600.
!!...  Relative displacements between previous and current position
      DELNS = real(DTR*RSC*(DELLAT)/HLS,kind=4)
      DELEW = real(-DTR*RSC*COS(DTR*CLAT)*DELLON/HLS,kind=4)
      DELZ = DELHGT/VLS
      IF(NPOS.le.0)then
!!...    Read new position if trajectory data file is being used
        READ (7,*,ERR=9998,END=999)TrajSEC,TrajHGT,TrajLAT,TrajLON
!!...    Convert negative longitudes
        If (TrajLON.lt.0.)TrajLON = TrajLON + 360.
!!...    Convert to West Longitude if LonEW = 1
        If (LonEW.eq.1)TrajLON = 360. - TrajLON
        If (I.gt.0)Then
!!...      Compute displacement magnitude of new from previous position
          DELTIME = real(TrajSEC - CSEC,kind=4)
          DELHGT = real(TrajHGT - CHGT,kind=4)
          DELLAT = real(Abs(TrajLAT - CLAT),kind=4)
          DELLON = real(Abs(TrajLON - CLON),kind=4)
        Endif
!!...    Correct DELLON for cases near 0/360 longitude discontinuity
        If (DELLON.gt.180.0)DELLON = 360. - DELLON
        If (DELLON.lt.0.0)DELLON = DELLON + 360.
!!...    Correct DELLON and DELLAT near polar discontinuities
        If (DELLON.gt.90.0.and.(Abs(TrajLAT).ge.70.0.or.                 &
        Abs(CLAT).ge.70.0))Then
          DELLON = Abs(180. - DELLON)
          DELLAT = real(180. - Abs(TrajLAT) - Abs(CLAT),kind=4)
        Endif
!!...    Relative displacements between previous and current position
        DELNS = real(DTR*RSC*(DELLAT)/HLS,kind=4)
        DELEW = real(-DTR*RSC*COS(DTR*CLAT)*DELLON/HLS,kind=4)
        DELZ = DELHGT/VLS
!!...    Set current position to new position
        CSEC = TrajSEC
        CHGT = TrajHGT
        CLAT = TrajLAT
        CLON = TrajLON
      Else If (I.gt.0) then
        CHGT = CHGT + DELHGT
        CLAT = CLAT + DELLAT
        CLON = CLON + DELLON
        CSEC = CSEC + DELTIME
      Endif
      If (MOLAhgts.ne.1)Then
!!...    Get radius RSC to new position and convert height to MOLA
        Call cps_RELLIPS_2001(CLAT,CLON,Rref,CHGT,ogz,Oldrref,topohgt,       &
          albedo)
        RSC = real(Oldrref + CHGT,kind=4)
        HGTM = RSC - Rref
      Else
        HGTM = CHGT
      Endif
      DATE = DATE0 + CSEC/8.6400d4
!!...  Update wave coefficients if necessary
      If (iuwave.gt.0.and.numwave.lt.nwave)Then
        If (CSEC.ge.wavetime(numwave+1))Then
 100      numwave = numwave + 1
          WaveA0   = wavedata(numwave,1)
          WaveA1   = wavedata(numwave,2)
          Wavephi1 = wavedata(numwave,3)
          WaveA2   = wavedata(numwave,4)
          Wavephi2 = wavedata(numwave,5)
          WaveA3   = wavedata(numwave,6)
          Wavephi3 = wavedata(numwave,7)
!!...      Check to see if more than one wave time exceeded
          If (numwave.lt.nwave)Then
            If (CSEC.ge.wavetime(numwave+1))Goto 100
          Endif
!!...      Write out updated wave coefficients
          If (iup.gt.0)Then
            Write(iup,292)WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,        &
             WaveA3,Wavephi3
  292       Format(' A0,A1,phi1,A2,phi2,A3,phi3=',F6.3,3(F6.3,F7.1))
            lonvar = 'West'
            If (LonEW.eq.1)lonvar = 'East'
            Write(iup,293)Wscale,lonvar
  293       Format('   Wave Scale =',F8.1,' km.    Wave phases are in',  &
             ' degrees of ',A4,' Longitude')
          Endif
        Endif
      Endif
!!...  Write wave coefficients if not read from file
      If (iuwave.eq.0.and.numwave.eq.0)Then
          If (iup.gt.0)Then
            Write(iup,292)WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,        &
             WaveA3,Wavephi3
            lonvar = 'West'
            If (LonEW.eq.1)lonvar = 'East'
            Write(iup,293)Wscale,lonvar
          Endif
        numwave = 1
      Endif
!!...  Correct latitude and longitude if position crosses either pole
      IF(ABS(CLAT).GT.90.)then
        CLAT = DSIGN(180.D0,CLAT) - CLAT
        CLON = CLON + 180.
        DELLAT = -DELLAT
      Endif
      IF(CLON.LT.0.0)CLON = CLON + 360.
      IF(CLON.GE.360.) CLON = CLON - 360.
!!...  Sun and Mars positions at new time
!!     Use high precision values if inputs > 0 or else ORBIT subroutine
!!     otherwise
      If (dradau.gt.0.0)Then
        SUNLON = dsunlon
        SUNLAT = dsunlat
        ALS = dsunLs
        MARSAU = dradau
      Else
        CALL orbit_2001(DATE,SUNLAT,SUNLON,ALS,MARSAU)
      Endif
!!...  Evaluate longitude-dependent wave perturbation
      Call Wavelon_2001(LonEW,CLON,CLAT,HGTM,wavepert)
!!...  Convert wave perturbation to % for output
      denswave = real(100.*wavepert,kind=4)
!!...  Evaluate atmospheric parameters
      CALL ATMOS2_2001(HGTM,CLAT,CLON,MARSAU,SUNLAT,SUNLON,ALS, &
           HSCALE,TEMP,DENS,FACTHI,FACTLO,PRES,thgt,careoid,                 &
           ZF,deltaTEX,Texos,Tbase,Hrho,AMz,                                 &
           Dusttau,DustOD,EWWIND,NSWIND,HatZF,wavepert,TempDay,PresDay,      &
           DensDay,EWwnDay,NSwnDay,hgtasfc,Patsurf,Tempmax,Tempmin,          &
           Densmax,Densmin,Tgrnd,talb,icepolar,gz,Oldrref)
!!...  Save value of height of 1.26 nbar level
      Zbase = real(ZF,kind=4)
!!...  Exponential correlation across displacement from previous
!!...  position
      dtimecor = real(Sqrt(EWWIND**2 + NSWIND**2)*DELTIME/(1000.*HLS),kind=4)
      corlim = Abs(DELNS) + Abs(DELEW) + Abs(DELZ) + Abs(dtimecor)
      CORREL = Exp(-corlim)
      corlim = -corlim/Alog(0.995)
!!...  Unperturbed mean density (approximate if HGTM > ZF)
      DENS0 = DENS/(1. + wavepert)
!!...  HGTM = height above MOLA areoid
!!...  HGTMS = height above local MOLA topographic surface
      HGTMS = real(HGTM - thgt,kind=4)
      DENSHI = DENS*FACTHI
      DPLUS = real(DENSHI - DENS,kind=4)
      If (FACTLO.le.0.0)FACTLO = 2. - FACTHI
      DENSLO = DENS*FACTLO
      DMINUS = real(DENS - DENSLO,kind=4)
  480 Z2 = RANDOM_2001(L)
      IF (L .EQ. 1)GOTO 480
      Z1 = PPND_2001(Z2,IFAULT)
      IF (IFAULT .EQ. 1)STOP ' PPND ERROR'
!!...  Local time in "Martian hours" (1/24th Sols)
      TLOCAL = 12. + (SUNLON - CLON)/15.
      IF (TLOCAL .LT. 0.)TLOCAL = TLOCAL + 24.
      IF (TLOCAL .GT. 24.)TLOCAL = TLOCAL - 24.
!!...  Output height above MOLA areoid (HGTM) or above local
!!...  terrain (HGTMS)
      OHGT = real(HGTM,kind=4)
      OHGTS = HGTMS
!!...  Set output heights to terrain height if <= -10 km
      IF(OHGT .LE. -10.)THEN
        OHGT = real(thgt + hgtasfc,kind=4)
        OHGTS = real(hgtasfc,kind=4)
      ENDIF
!!...  Current random density perturbation value, correlated with
!!...  previous random density perturbation
      RHOd = real(CORREL*RHOd + SQRT(1.0 - CORREL**2)*Z1,kind=4)
      IF(RHOd.LT.0.0)DensRand = real(RHOd*DMINUS*rpscale,kind=4)
      IF(RHOd.GE.0.0)DensRand = real(RHOd*DPLUS*rpscale,kind=4)
!!...  Add random density perturbation
      DENSP = DENS + DensRand
!!...  Check upper and lower bounds on density perturbations
      If (DENSP .lt. 0.1*DENS0)DENSP = 0.1*DENS0
      If (DENSP .gt. 10.*DENS0)DENSP = 10.*DENS0
!!...  Save as total density, for output
      DENSTOT = DENSP
!!...  Standard deviation in random density perturbation (% of
!!...  unperturbed mean) for output
      SIGD = rpscale*50.*Abs(DENSHI-DENSLO)/DENS0
!!...  Adjust random DENSHI, DENSLO for rpscale
      DENSHI = DENS + rpscale*(DENSHI - DENS)
      DENSLO = DENS + rpscale*(DENSLO - DENS)
      If (DENSLO .lt. 0.1*DENS0)DENSLO = 0.1*DENS0
!!...  Convert random density perturbation to % of (unperturbed) mean
      DensRand = real(100.*(DENSP - DENS)/DENS0,kind=4)
!!...  Compute total density perturbation as % of (unperturbed) mean
      DENSP = DensRand + denswave
!!...  Standard deviation for wind perturbations
!!     (Note SIGD already adjusted for rpscale, above)
      SIGU = real(SIGD,kind=4)
!!...  Compute EW and NS wind perturbations and total wind
 586  Z2 = RANDOM_2001(L)
      If (L.eq.1)Goto 586
      Z1 = PPND_2001(Z2,ifault)
      RHOu = real(CORREL*RHOu + Sqrt(1.0 - CORREL**2)*Z1,kind=4)
!!...  Limit winds to sound speed/Sqrt(2)   (ssposr2)
!!     Assume specific heat ratio = 4/3
      ssposr2 = Sqrt(Cp_2001(TEMP)*TEMP/6.)
!!...  EW component of perturbation in wind and total wind
      EWpert = real(RHOu*SIGU,kind=PM_REEL)
      EWtot = real(EWWIND + EWpert,kind=4)
      If (Abs(EWtot).gt.ssposr2)Then
        EWtot = real(DSign(ssposr2,EWtot*1.D0),kind=4)
        EWpert = EWtot - EWWIND
      Endif
 587  Z2 = RANDOM_2001(L)
      If (L.eq.1)Goto 587
      Z1 = PPND_2001(Z2,ifault)
      RHOv = real(CORREL*RHOv + Sqrt(1.0 - CORREL**2)*Z1,kind=4)
!!...  NS component of perturbation in wind and total wind
      NSpert = real(RHOv*SIGU,kind=PM_REEL)
      NStot = NSWIND + NSpert
      If (Abs(NStot).gt.ssposr2)Then
        NStot = Sign(ssposr2,NStot)
        NSpert = NStot - NSWIND
      Endif
!!...  Compute cosine of solar zenith angle
      cszang = real(Sin(DTR*SUNLAT)*Sin(DTR*CLAT) + Cos(DTR*SUNLAT)*      &
       Cos(DTR*CLAT)*Cos(DTR*(SUNLON-CLON)),kind=4)
      szang = real(Acos(cszang)/DTR,kind=4)
      F1peak = 999.9
!!...  Compute height of F1 peak if solar zenith angle < 90 deg
      If (cszang.gt.0..and.Zbase.lt.900.)Then
!!...    relative Air mass
        airmass = 1./(cszang + 0.15*(93.885 - szang)**(-1.253))
!!...    F1 peak height (km)
        F1peak = real(Zbase + HatZF*Alog(airmass),kind=4)
      Endif
!!...  Write descriptively formatted data on LIST.txt file
      Elon = real(360. - CLON,kind=4)
!!...  Total radius (areoid + height)
      radtotal = real(careoid + OHGT,kind=4)
!!...  Compute height above reference ellipsoid
      Oldhgt = real(radtotal - Oldrref,kind=4)
!!...  Difference of MOLA areoid from ellipsoid radius
      dAreoid = real(careoid - Oldrref,kind=4)
      If(iup.gt.0)Write(iup,590)CSEC,CSEC/onesol,ALS,DustOD,OHGT,OHGTS,  &
      thgt,radtotal,careoid,Oldhgt,HSCALE,Hrho,CLAT,CLON,Elon,SUNLAT,   &
      MARSAU,SUNLON,TLOCAL
  590 FORMAT(' Time (rel. to T0) =',F10.1,' sec. (',F8.3,' sols)',       &
      '  Ls =',F6.1,'  Dust =',F5.2/' Height Above MOLA Areoid',        &
      ' (Above Surface) =',F8.3,' km (',F8.3,' km)'/                    &
      ' Topographic Height = ',F8.3,' km   Radius (Areoid) = ',F8.3,    &
      ' (',F8.3,') km'/' Hgt Above Ellipsoid =',F8.3,' km  ',           &
      ' Scale Hgt H(p) =',F6.2,' H(rho) =',F6.2,' km'/' Latitude = ',   &
      F7.2,'  degrees        Longitude = ',F7.2,' W (',F7.2,' E) deg.'  &
      /' Sun Latitude =',F9.2,' deg.       Mars Orbital Radius =',      &
      F6.3,' AU'/' Sun Longitude =',F8.2,' deg.W      Local Time = ',   &
      F6.2,' Mars hours')
      If (iup.gt.0.and.OHGT.gt.80.)Then
        Write(iup,595)Texos,Tbase,Zbase
        Write(iup,596)szang,F1peak,AMz
      Endif
  595 Format(' Exospheric Temp. = ',F6.1,' K',8X,'Tbase = ',F6.1,        &
      ' K',3X,' Zbase = ',F6.1,' km')
  596 Format(' Solar Zenith Angle =',F6.1,                               &
      ' deg     F1 peak =',F6.1,' km    Mol.Wgt. =',F6.2)
!!...  Compute percent deviations from COSPAR values
      Call cospar_2001(OHGT,tcos,pcos,dcos)
      If (dcos.le.0.0)Then
        devlo = -99.9
        devav = -99.9
        devhi = -99.9
        devtot = -99.9
        devDay = -99.9
        devmax = -99.9
        devmin = -99.9
      Else
        devlo = real(100.*(DENSLO-dcos)/dcos,kind=4)
        devav = real(100.*(DENS-dcos)/dcos,kind=4)
        devhi = real(100.*(DENSHI-dcos)/dcos,kind=4)
        devtot = real(100.*(DENSTOT-dcos)/dcos,kind=4)
        devDay = 100.*(DensDay-dcos)/dcos
        devmax = 100.*(Densmax-dcos)/dcos
        devmin = 100.*(Densmin-dcos)/dcos
      Endif
      densunits = 'kg/m**3 '
!!...  Convert density units to kg/km**3 if logscale = 3
      If (logscale .eq.3)Then
        DENS = 1.0E9*DENS
        DENSLO = 1.0E9*DENSLO
        DENSHI = 1.0E9*DENSHI
        DENSTOT = 1.0E9*DENSTOT
        DensDay = 1.0E9*DensDay
        Densmax = 1.0E9*Densmax
        Densmin = 1.0E9*Densmin
        densunits = 'kg/km**3'
      Endif
!!...  Write formatted output to list file
      If(iup.gt.0)Then
        Write(iup,600)TEMP,PRES,DENSLO,DENS,DENSHI,densunits,devlo,      &
       devav,devhi,DENSTOT,densunits,DENSP,denswave,EWWIND,EWpert,      &
       EWtot,NSWIND,NSpert,NStot
        If (corlim.lt.1..and.Abs(CLAT).lt.89.99)Write(iup,610)corlim
        Write(iup,650)
      Endif
  600 FORMAT(' Temperature = ',F7.1,' K',11X,' Pressure = ',1p,E12.3,    &
      ' N/m**2'/ ' Density (Low, Avg., High) =   ',3E12.3,1X,A8/        &
      ' Departure, COSPAR NH Mean =',0p,F12.1,' %',2(F10.1,' %')/       &
      ' Tot.Dens. =',1p,E10.3,0p,1X,A8,'  Dens.Pert. =',F7.2,           &
      '% Wave =',F7.2,'% of mean'/                                      &
      ' Eastward Wind  (Mean, Perturbed, Total) = ',3F7.1,' m/s'/       &
      ' Northward Wind (Mean, Perturbed, Total) = ',3F7.1,' m/s')
  610 Format(' Warning: Step size smaller than accuracy limit by a ',    &
       'factor of',F6.3)
  650 FORMAT(' -----------------------------------------------------',   &
      '----------------------')
      PRESmb = real(PRES/100.,kind=4)
      sigmalevel = real(PRES/Patsurf,kind=4)
      preshgt = -Alog(sigmalevel)
!!...  Compute dust density variables by methods of Haberle et al.,
!!      Icarus, 50, 322 (1982) and Haberle et al., J. Geophys. Res.,
!!      104, 8957 (1999)
!!...  Dust column areal density (kg/m**2)
      dareaden = real(5.0E-3*DustOD,kind=4)
!!...  Dust mixing ratio (kg dust/kg air) at surface
      qsurf = real(dareaden*gz/(0.994*Exp(-Dustnu)*Patsurf),kind=4)
!!...  Dust mixing ratio at current position and pressure
      dmixrat = 0.
      expfact = real(Dustnu*(1. - 1./sigmalevel),kind=4)
      If(expfact.gt.-85.)dmixrat = qsurf*Exp(expfact)
!!...  Dust mass density (micrograms dust / m**3)
      dmasden = real(1.0E9*dmixrat*DENS,kind=4)
!!...  Dust number density (number dust particles / m**3)
      dnumden = real(dmasden/(5.23599E-10*Dustdens*Dustdiam**3),kind=4)
      If(NVARX.EQ.9)VARX = PRESmb
      If(NVARY.EQ.9)VARY = PRESmb
      If(NVARX.EQ.10)VARX = preshgt
      If(NVARY.EQ.10)VARY = preshgt
      If(NVARX.EQ.11)VARX = sigmalevel
      If(NVARY.EQ.11)VARY = sigmalevel
      If(NVARX.EQ.12)VARX = Oldhgt
      If(NVARY.EQ.12)VARY = Oldhgt
!!...  Output deviations from COSPAR if logscale = 2
      If (logscale.eq.2)Then
        DENSLO = devlo
        DENS = devav
        DENSHI = devhi
        DENSTOT = devtot
        DensDay = devDay
        Densmax = devmax
        Densmin = devmin
        If (pcos.le.0.0)Then
          PRES = -99.9
          PresDay = -99.9
        Else
          PRES = 100.*(PRES-pcos)/pcos
          PresDay = 100.*(PresDay-pcos)/pcos
        Endif
      Endif
!!...  Write parameters on plot format files
      IF(NVARX.EQ.1)VARX = OHGT
      IF(NVARX.EQ.2)VARX = OHGTS
      IF(NVARX.EQ.3)VARX = real(CLAT,kind=4)
      IF(NVARX.EQ.4)Then
        VARX = real(CLON,kind=4)
        If (LonEW.eq.1)VARX = real(360. - CLON,kind=4)
      Endif
      IF(NVARX.EQ.5)VARX = real(CSEC,kind=4)
      If(NVARX.EQ.6)VARX = real(CSEC/onesol,kind=4)
      If(NVARX.EQ.7)VARX = real(ALS,kind=4)
      If(NVARX.EQ.8)VARX = real(TLOCAL,kind=4)
      Alogdens = 0.0 !!... Variable Alogdens n'est utilisé jamais
      If (logscale .ne. 2)Alogdens = real(Dlog10(DENS),kind=4)
      If (logscale .eq. 1)then
        DENS = Dlogdens  !!... DENS=Alogdens???
        PRES = Dlog10(PRES)
        DENSLO = Dlog10(DENSLO)
        DENSHI = DLOG10(DENSHI)
        DENSTOT = Dlog10(DENSTOT)
        If (OHGT.le.Zbase) Then
          DensDay = Alog10(DensDay)
          PresDay = Dlog10(PresDay)
          Densmax = Alog10(Densmax)
          Densmin = Alog10(Densmin)
        Endif
      Endif
      If (NVARY .eq. 0.and.iup.gt.0)then
        Write(21,796)VARX,DENSLO,DENS,DENSHI,DENSTOT,DustOD,radtotal,    &
         gz,MARSAU
        Write(22,790)VARX,SIGD,DensRand,denswave,DENSP,corlim,SIGU
        Write(23,790)VARX,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot
        Write(24,798)VARX,TEMP,PRES,TEMP-273.15,PRESmb,Hrho,thgt,Tgrnd,  &
        careoid,dAreoid
        If(OHGT.le.Zbase)Write(25,780)VARX,TempDay,PresDay,DensDay,      &
         EWwnDay,NSwnDay,Tempmin,Tempmax,Densmin,Densmax
        If(OHGT.gt.80.)Write(26,791)VARX,Tbase,Zbase,F1peak,AMz,Texos,   &
         hgtoffset
        Write(27,793)VARX,talb,cszang,dareaden,dmixrat,dmasden,dnumden,  &
         icepolar
      Else If (iup.gt.0) Then
        IF(NVARY.EQ.1)VARY = OHGT
        IF(NVARY.EQ.2)VARY = OHGTS
        IF(NVARY.EQ.3)VARY = real(CLAT,kind=4)
        IF(NVARY.EQ.4)Then
          VARY = real(CLON,kind=4)
          If (LonEW.eq.1)VARY = real(360. - CLON,kind=4)
        Endif
        If(NVARY.EQ.5)VARY = real(CSEC,kind=4)
        If(NVARY.EQ.6)VARY = real(CSEC/onesol,kind=4)
        If(NVARY.EQ.7)VARY = real(ALS,kind=4)
        If(NVARY.EQ.8)VARY = real(TLOCAL,kind=4)
        Write(21,797)VARX,VARY,DENSLO,DENS,DENSHI,DENSTOT,DustOD,        &
         radtotal,gz,MARSAU
        Write(22,795)VARX,VARY,SIGD,DensRand,denswave,DENSP,corlim,SIGU
        Write(23,795)VARX,VARY,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot
        Write(24,799)VARX,VARY,TEMP,PRES,TEMP-273.15,PRESmb,Hrho,thgt,   &
         Tgrnd,careoid,dAreoid
        If(OHGT.le.Zbase)Write(25,785)VARX,VARY,TempDay,PresDay,         &
         DensDay,EWwnDay,NSwnDay,Tempmin,Tempmax,Densmin,Densmax
        If(OHGT.gt.80.)Write(26,792)VARX,VARY,Tbase,Zbase,F1peak,AMz,    &
         Texos,hgtoffset
        Write(27,794)VARX,VARY,talb,cszang,dareaden,dmixrat,dmasden,     &
         dnumden,icepolar
      Endif
  780 Format(G13.5,F7.1,2G11.4,4F8.1,2G11.4)
  785 Format(2G13.5,F7.1,2G11.4,4F8.1,2G11.4)
  790 FORMAT(G13.5,6G12.4)
  791 Format(G13.5,5F8.1,F10.2)
  792 Format(2G13.5,5F8.1,F10.2)
  793 Format(G13.5,F6.3,F7.3,1p,4E9.2,0p,I3)
  794 Format(2G13.5,F6.3,F7.3,1p,4E9.2,0p,I3)
  795 Format(2G13.5,6G12.4)
  796 FORMAT(G13.5,1p,4E11.3,0p,F7.4,F9.3,2F6.3)
  797 Format(2G13.5,1p,4E11.3,0p,F7.4,F9.3,2F6.3)
  798 FORMAT(G13.5,2(F7.1,G11.4),F6.2,F7.3,F6.1,F9.3,F8.3)
  799 Format(2G13.5,2(F7.1,G11.4),F6.2,F7.3,F6.1,F9.3,F8.3)
!!...  Write non-descriptively formatted data on OUTPUT file
      VAR = OHGT
      If (MOLAhgts.ne.1)VAR = Oldhgt
      IF(NVARX.EQ.2.or.NVARY.eq.2)VAR = OHGTS
      If(iup.gt.0)Then
        OLON = real(CLON,kind=4)
        If (LonEW.eq.1)OLON = real(360. - CLON,kind=4)
        If(logscale.eq.0.or.logscale.eq.3)Then
          WRITE(29,800)CSEC,VAR,CLAT,OLON,dens,TEMP,EWWIND,              &
           NSWIND,SIGD,ALS,DustOD
        Else
          WRITE(29,810)CSEC,VAR,CLAT,OLON,dens,TEMP,EWWIND,              &
           NSWIND,SIGD,ALS,DustOD
        Endif
      Endif
  800 FORMAT(F10.0,3F7.2,1P,E9.2,0P,3F7.1,2F6.1,F5.2)
  810 FORMAT(F10.0,3F7.2,F9.2,3F7.1,2F6.1,F5.2)
      Return
  999 EOF = 1
      If (NPOS.LE.0)Rewind(7)
      Return
 9998 Stop ' Error termination reading trajectory data file!!'
    END Subroutine Datastep_2001
!!-----------------------------------------------------------------------
    Real(kind=PM_REEL) Function dustvsls_2001(als)
      real(kind=PM_REEL) :: als
      real(kind=PM_REEL) :: pi180
!!...  Viking-like annual variation (versus solar angle Ls) for non-
!!     dust-storm dust optical depth
      pi180 = Atan(1.)/45.
      dustvsls_2001 = 0.65 - 0.35*Sin(pi180*als)
      Return
    End Function dustvsls_2001
!!-----------------------------------------------------------------------
    Subroutine EScalc_2001(stdl,SIGMA,ES)
      real(kind=PM_REEL) :: stdl
!!.... EPS ARE STD VARNS FROM NOMINAL VALUES
!!.... 0,2,....10  LONG - TERM
!!.... 1,3.....,11  SHORT-TERM
      DIMENSION SIG(0:11), EPS(0:11), ES(0:11)
      real :: SIG, EPS, ES
      DATA EPS/12*0.0/
      integer :: i

      DO 10 I = 2,10,2
        EPS(I) = real(stdl,kind=4)
   10 Enddo
      Do 20 I = 3,9,2
        EPS(I) = SIGMA
   20 Enddo
      SIG(0) = 0.25
!!.... LONG TERM -FBAR
!!.... THESE ARE COEF. OF VARIATION, I.E. SIGMA/MU,    %/100.0
      SIG(1) = 0.0
!!.... SHORT TERM-FBAR
      SIG(2) = 0.16
!!.... LONG TERM-TINF
      SIG(3) = 0.12
!!.... SHORT TERM-TINF
      SIG(4) = 0.40
!!.... LONG TERM - FOXY
      SIG(5) = 0.12
!!.... SHORT TERM -FOXY
      SIG(6) = 0.0
!!.... LONG TERM -AOXY
      SIG(7) = 0.21
!!.... SHORT TERM -AOXY
      SIG(8) = 0.045
!!.... LONG TERM - ZF
      SIG(9) = 0.0225
!!.... SHORT TERM - ZF
      SIG(10) = 0.30
!!.... LONG TERM- DZDUST
      SIG(11) = 0.0
!!.... SHORT TERM - DZDUST
      DO 100 I = 0, 11
        ES(I) = EPS(I) * SIG(I)
  100 Enddo
      Return
    End Subroutine EScalc_2001
!!-----------------------------------------------------------------------
    Subroutine FourD_2001(dx,dy,dz,dq,Array,Value)
!!...    4-Dimensional linear interpolation within a 1x1x1x1 hypercube
!!       (x,y,z,q) of Array(2,2,2,2) to position dx,dy,dz,dq (all 0-1).
!!       Value is value of interpolated output.
        Real(kind=PM_REEL) :: Array(2,2,2,2)
        Real(kind=PM_REEL) :: dx,dy,dz,dq
        Real(kind=PM_REEL) :: Value
        Real(kind=PM_REEL) :: dxp, dyp, dzp,dqp

!!...    Complementary displacements in x,y,z,q
        dxp = 1. - dx
        dyp = 1. - dy
        dzp = 1. - dz
        dqp = 1. - dq
!!...    Compute 4-dimensional linear interpolated value
        Value = dxp*dyp*dzp*dqp*Array(1,1,1,1)                           &
         + dxp*dyp*dzp*dq*Array(1,1,1,2)                                &
         + dxp*dyp*dz*dqp*Array(1,1,2,1)                                &
         + dxp*dy*dzp*dqp*Array(1,2,1,1)                                &
         + dx*dyp*dzp*dqp*Array(2,1,1,1)                                &
         + dxp*dyp*dz*dq*Array(1,1,2,2)                                 &
         + dxp*dy*dzp*dq*Array(1,2,1,2)                                 &
         + dxp*dy*dz*dqp*Array(1,2,2,1)                                 &
         + dx*dyp*dzp*dq*Array(2,1,1,2)                                 &
         + dx*dyp*dz*dqp*Array(2,1,2,1)                                 &
         + dx*dy*dzp*dqp*Array(2,2,1,1)                                 &
         + dxp*dy*dz*dq*Array(1,2,2,2)                                  &
         + dx*dyp*dz*dq*Array(2,1,2,2)                                  &
         + dx*dy*dzp*dq*Array(2,2,1,2)                                  &
         + dx*dy*dz*dqp*Array(2,2,2,1)                                  &
         + dx*dy*dz*dq*Array(2,2,2,2)
        Return
      End Subroutine FourD_2001
!!-----------------------------------------------------------------------
      Subroutine MGCMterp_2001(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,       &
         VMGCM,TempDay,PresDay,DensDay,UwndDay,VwndDay,Tempmax,Tempmin, &
         Densmax,Densmin)
!!...    Interpolates Ames Mars General Circulation Model (MGCM) data
!!       to a given latitude, time of year (Ls), and dust optical
!!       depth, for a given height index (khgt) and time of day (time).
!!       Some input data is provided by the Common "Interp".
!!...    Set parameter values for number of heights, latitudes, and
!!       number of dust optical depth values
        integer :: khgt
        real(kind=PM_REEL) :: Tempmin,Densmin,PMGCM,PresDay,TMGCM,VMGCM,DMGCM
        real(kind=PM_REEL) :: Densmax,TempDay,Tempmax
        real(kind=PM_REEL) :: UwndDay,VwndDay,DensDay
        real(kind=PM_REEL) :: RMGCM,UMGCM

        integer, parameter :: nhgt = 17
        integer, parameter :: nlat = 25
        integer, parameter :: ndust = 3
!!...    MGCM 0-80 km data arrays for interpolation
! common MGCMdata_2001
        real :: TZA0(nhgt,nlat,0:12,ndust),               &
         TZA1(nhgt,nlat,0:12,ndust),TZP1(nhgt,nlat,0:12,ndust),         &
         TZA2(nhgt,nlat,0:12,ndust),TZP2(nhgt,nlat,0:12,ndust),         &
         PZA0(nhgt,nlat,0:12,ndust),PZA1(nhgt,nlat,0:12,ndust),         &
         PZP1(nhgt,nlat,0:12,ndust),PZA2(nhgt,nlat,0:12,ndust),         &
         PZP2(nhgt,nlat,0:12,ndust),DZA0(nhgt,nlat,0:12,ndust),         &
         UZA0(nhgt,nlat,0:12,ndust),UZA1(nhgt,nlat,0:12,ndust),         &
         UZP1(nhgt,nlat,0:12,ndust),UZA2(nhgt,nlat,0:12,ndust),         &
         UZP2(nhgt,nlat,0:12,ndust),VZA0(nhgt,nlat,0:12,ndust),         &
         VZA1(nhgt,nlat,0:12,ndust),VZP1(nhgt,nlat,0:12,ndust),         &
         VZA2(nhgt,nlat,0:12,ndust),VZP2(nhgt,nlat,0:12,ndust)
        Common /MGCMdata_2001/TZA0,               &
         TZA1,TZP1,         &
         TZA2,TZP2,         &
         PZA0,PZA1,         &
         PZP1,PZA2,         &
         PZP2,DZA0,         &
         UZA0,UZA1,         &
         UZP1,UZA2,         &
         UZP2,VZA0,         &
         VZA1,VZP1,         &
         VZA2,VZP2


! Common Interp_2001
        integer :: ilat,ilatw,ilatt,jlon,ls,mdust,k1st, mf10
        real(kind=PM_REEL) :: dlat, dlon,dlatw,dls,ddust,dlatt,df10,    &
             wpolefac,tpolefac
        Common /Interp_2001/ilat,ilatw,ilatt,jlon,ls,mdust,k1st,mf10,dlat, &
               dlon,dlatw,dls,ddust,dlatt,df10,wpolefac,tpolefac
        Real(kind=PM_REEL) :: TM(2,2,2),PM(2,2,2),UM(2,2,2),VM(2,2,2), &
             R0(2,2,2), Tday(2,2,2),Pday(2,2,2),Uday(2,2,2),Vday(2,2,2),&
             Tmax(2,2,2), Tmin(2,2,2),Dmax(2,2,2),Dmin(2,2,2)
        Real(kind=PM_REEL) :: U0,V0,A1,P1,A2,P2,time
        real(kind=PM_REEL) :: P0,A1p,P1p,A2p,P2p,xtime,T0,A1t,P1t,A2t,P2t
        integer :: itime, l, m,i

!!...    Establish MGCM values at corners of a 3-dimensional cube in
!!       latitude-Ls-dust space, at the given height index (khgt), and
!!       time of day (time)
        Do 102 i = 1,2
          polefac = 1.
          upolefac = 1.
          If (ilat.eq.1)Then
            polefac = i - 1.
          Else If (ilat.eq.nlat-1)Then
            polefac = 2. - i
          Endif
          If (ilatw.eq.2)Then
            If (i.eq.1)upolefac = real(wpolefac,kind=4)
          Else If (ilatw.eq.nlat-1)Then
            If (i.eq.2)upolefac = real(wpolefac,kind=4)
          Endif
        Do 101 l = 1,2
        Do 100 m = 1,2
!!...      Daily mean temperature
          T0 = real(TZA0(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Tday(i,l,m) = T0
!!...      Temperature tide amplitudes and phases
          A1t = real(TZA1(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P1t = real(TZP1(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2t = real(TZA2(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P2t = real(TZP2(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Temperature at corners of 3-D cube
          TM(i,l,m) = TideX_2001(T0,A1t,P1t,A2t,P2t,time)
!!...      Daily mean pressure
          P0 = real(PZA0(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Pday(i,l,m) = P0
!!...      Pressure tide amplitudes and phases
          A1p = real(PZA1(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P1p = real(PZP1(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2p = real(PZA2(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P2p = real(PZP2(khgt,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Pressure at corners of 3-D cube
          PM(i,l,m) = TideY_2001(P0,A1p,P1p,A2p,P2p,time)
!!...      Daily average density D0
          D0 = DZA0(khgt,ilat+i-1,ls+l-1,mdust+m-1)
!!...      Gas constant from pressure, density and temperature
          R0(i,l,m) = 190.
          If (T0.ne.0.0.and.D0.ne.0.0)R0(i,l,m) = P0/(T0*D0)
!!...      Max and Min temperature and density at corners of 3-D cube
          Tmax(i,l,m) = -9999.
          Tmin(i,l,m) = 9999.
          Dmax(i,l,m) = -9999.
          Dmin(i,l,m) = 9999.
          Do 50 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            Ttime = real(TideX_2001(T0,A1t,P1t,A2t,P2t,xtime),kind=4)
            Ptime = real(TideY_2001(P0,A1p,P1p,A2p,P2p,xtime),kind=4)
            Dtime = real(Ptime/(R0(i,l,m)*Ttime),kind=4)
            If (Ttime.gt.Tmax(i,l,m))Tmax(i,l,m) = Ttime
            If (Ttime.lt.Tmin(i,l,m))Tmin(i,l,m) = Ttime
            If (Dtime.gt.Dmax(i,l,m))Dmax(i,l,m) = Dtime
            If (Dtime.lt.Dmin(i,l,m))Dmin(i,l,m) = Dtime
  50      Enddo
!!...      Daily mean EW wind
          U0 = real(UZA0(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Uday(i,l,m) = U0
!!...      EW wind tide amplitudes and phases
          A1 = real(UZA1(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P1 = real(UZP1(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(UZA2(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P2 = real(UZP2(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      EW wind at corners of 3-D cube
          UM(i,l,m) = TideX_2001(U0,A1,P1,A2,P2,time)
!!...      Daily mean NS wind
          V0 = real(VZA0(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Vday(i,l,m) = V0
!!...      NS wind tide amplitudes and phases
          A1 = real(VZA1(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P1 = real(VZP1(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(VZA2(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P2 = real(VZP2(khgt,ilatw+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      NS wind at corners of 3-D cube
          VM(i,l,m) = TideX_2001(V0,A1,P1,A2,P2,time)
 100    Enddo
 101    Enddo
 102    Enddo
!!...    Use 3-D interpolation to get temperature, pressure, gas
!!       constant, EW wind, and NS wind at given latitude, Ls, and
!!       dust optical depth
        Call ThreeD_2001(dlat,dls,ddust,TM,TMGCM)
        Call ThreeD_2001(dlat,dls,ddust,Tday,TempDay)
        Call ThreeD_2001(dlat,dls,ddust,Tmax,Tempmax)
        Call ThreeD_2001(dlat,dls,ddust,Tmin,Tempmin)
        Call ThreeD_2001(dlat,dls,ddust,Dmax,Densmax)
        Call ThreeD_2001(dlat,dls,ddust,Dmin,Densmin)
        Call ThreeD_2001(dlat,dls,ddust,PM,PMGCM)
        Call ThreeD_2001(dlat,dls,ddust,Pday,PresDay)
        Call ThreeD_2001(dlat,dls,ddust,R0,RMGCM)
        Call ThreeD_2001(dlatw,dls,ddust,UM,UMGCM)
        Call ThreeD_2001(dlatw,dls,ddust,Uday,UwndDay)
        Call ThreeD_2001(dlatw,dls,ddust,VM,VMGCM)
        Call ThreeD_2001(dlatw,dls,ddust,Vday,VwndDay)
!!...    Compute density from temperature, pressure, and gas constant
        DMGCM = PMGCM/(RMGCM*TMGCM)
        DensDay = PresDay/(RMGCM*TempDay)
        Return
      End Subroutine MGCMterp_2001
!!-----------------------------------------------------------------------
      integer Function Ifloor_2001(x)
!!...    Integer floor function, greatest integer <= x (provided for
!!       compilers that do not have this intrinsic function)
        integer :: Iflr
        real(kind=PM_REEL) :: x
        Iflr = Int(x)
        If (Iflr.gt.x) then
          Ifloor_2001 = Iflr - 1
        else
          Ifloor_2001 = Iflr
        Endif
        Return
      End Function Ifloor_2001
!!-----------------------------------------------------------------------
      Subroutine MarsGCM_2001(chgt,clat,clonw,als,dusttau,time,ctemp,  &
         cpres,cdens,cuwin,cvwin,Hpres,Hdens,ZF,pertfact,ctopohgt,      &
         hgtasfc,careoid,TempDay,PresDay,DensDay,EWwnDay,NSwnDay,       &
         Tempmax,Tempmin,Densmax,Densmin,Tgrnd,calbedo,icepolar,        &
         Tat5m,dustoffset)
!!...    Uses interpolation routines to evaluate:
!!
!!       ctemp    = temperature (K) at current position
!!       cpres    = pressure (N/m**2) at current position
!!       cdens    = density (kg/m**3) at current position
!!       cuwin    = eastward wind component (m/s) at current position
!!       cvwin    = northward wind component (m/s) at current position
!!       Hpres    = pressure scale height (km) at current position
!!       Hdens    = density scale height (km) at current position
!!       ZF       = height of 1.26 nbar level at current position
!!       pertfact = perturbation factor from random perturbation model
!!       ctopohgt = topographic height (km) at current position
!!       careoid  = local radius (km) of MOLA 1/2 degree areoid
!!       TempDay  = Local daily average temperature (K)
!!       PresDay  = Local daily average pressure (N/m**2)
!!       DensDay  = Local daily average density (kg/m**3)
!!       EWwnDay  = Local daily average Eastward wind (m/s)
!!       NSwnDay  = Local daily average Northward wind (m/s)
!!       Tempmax  = Local daily maximum temperature (K)
!!       Tempmin  = Local daily minimum temperature (K)
!!       Densmax  = Local daily maximum density (kg/m**3)
!!       Densmin  = Local daily minimum density (kg/m**3)
!!       Tgrnd    = ground surface temperature (K)
!!       calbedo  = surface albedo
!!       icepolar = polar ice indicator (0=no; 1=yes)
!!
!!       at the current height (chgt), latitude (clat), current (West)
!!       longitude (clonw), for time of year given by Ls=als, and time
!!       of day (time).  Interpolation is done using either boundary
!!       layer or 0-80 km data from the Ames Mars General Circulation
!!       model (MGCM) or from 80-170 km data from the University of
!!       Arizona Mars Thermospheric General Circulation Model (MTGCM).
!!
!!...    Set parameter values for number of MGCM heights (nhgt), number
!!       of MTGCM heights (nhgtt), number of MGCM boundary layer levels
!!       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-
!!       tudes (nlatt), number of MGCM longitudes (nlon), number of dust
!!       optical depths (ndust), and minimum perturbation magnitude at
!!       surface (pert0)
        integer, parameter :: nhgt = 17
        integer, parameter :: nhgtt = 19
        integer, parameter :: nbl = 3
        integer, parameter :: nlat = 25
        integer, parameter :: nlatt = 36
        integer, parameter :: nlon = 40
        integer, parameter :: ndust = 3
        real(kind=PM_REEL), parameter :: pert0 = 0.02_PM_REEL
        real(kind=PM_REEL), parameter :: pertmax = 0.30_PM_REEL
        real(kind=PM_REEL), parameter :: pertmin = 0.15_PM_REEL
        integer, parameter :: nf10 = 2

        real(kind=PM_REEL) :: F107,als,cpres,Hdens
        real(kind=PM_REEL) :: cdens,cuwin,cvwin,PresDay,Tgrnd,hgtasfc,Hpres
        integer :: icepolar

! Common MGCMparm_2001
        Character*2 :: dustc(ndust),solact (nf10)
        real(kind=PM_REEL) :: dust(ndust),dzbl(nbl),zwsfc,f10val(nf10)
        Common /MGCMparm_2001/dust,dzbl,zwsfc,f10val, &
             dustc,solact

! Common Interp_2001
        integer :: ilat,jlon,ls,mdust, k1st,ilatw,ilatt,mf10
        real(kind=PM_REEL) :: dlat, dlon,dls,ddust,dlatw,dlatt,df10,    &
             wpolefac,tpolefac
        Common /Interp_2001/ilat,ilatw,ilatt,jlon,ls,mdust,k1st,mf10,dlat, &
               dlon,dlatw,dls,ddust,dlatt,df10,wpolefac,tpolefac

! common TGCMoffset_2001
        REAL(KIND=PM_REEL) :: offsets(0:12,ndust),zoffset,hgtoffset
        integer :: ibougher
        Common /TGCMoffset_2001/offsets,zoffset,hgtoffset,   &
        ibougher

        ! Common THERM_2001
        real(kind=PM_REEL) :: stdl
        Common /THERM_2001/F107,stdl
        Real(KIND=PM_REEL) :: NSwnDay,offset(2,2)
        REAL(KIND=PM_REEL) :: uhgt,z0
        REAL(KIND=PM_REEL) :: TMGCM2,PMGCM2,UMGCM2,VMGCM2,Tday2,Pday2,   &
             Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2
        REAL(KIND=PM_REEL) :: gz,CpofT,Tmin1,z5,zeval,      &
           factor,Tempmin,Tday1,Tempday,Tempmax,Tmax1,ctemp
        REAL(KIND=PM_REEL) :: TMGCM1,Tcheck,Tsubl
        REAL(KIND=PM_REEL) :: clat,clonw,Rref,chgt,Oldrref,topohgt, albedo
        REAL(KIND=PM_REEL) :: Uday1,Vday1,time,UMGCM1,VMGCM1,DMGCM1,DMGCM2,Pday1
        REAL(KIND=PM_REEL) :: PMGCM1,Dmax1,Dmin1,ZF80,ZF,Dday1
        REAL(KIND=PM_REEL) :: globoffst,dusttau,careoid
        REAL(KIND=PM_REEL) :: calbedo,ctopohgt,Hpres1,Hdens1

        integer :: khgt,khgtt,lhgtt,jbl

        ! fonctions
!        integer :: Ifloor_2001
!        REAL(KIND=PM_REEL) :: Cp_2001
!
        pertfact = 0.0
!!...    Initialize ground surface temperature and polar ice indicator
        Tgrnd = 999.9
        icepolar = 99
!!...    Insure latitude, longitude, Ls, and time of day within proper
!!       bounds
        If (Abs(clat).gt.90.)Stop ' Latitude error in MarsGCM'
        If (Abs(clonw).gt.360.)Stop ' Longitude error in MarsGCM'
        If (als.lt.0.0.or.als.gt.360.0)Stop ' Ls error in MarsGCM'
        If (time.lt.0.0.or.time.gt.24.0)Stop ' time error in MarsGCM'
!!...    Latitude step size for MGCM and MTGCM data
        steplat = 180./(nlat-1.)
        tsteplat = 180./nlatt
!!...    Most southerly MTGCM latitude
        tlat1st = -90. + tsteplat/2.
!!...    Longitude step size for MGCM boundary layer data
        steplon = 360./nlon
!!...    MGCM height index (khgt) for current height (chgt)
        khgt = Ifloor_2001(1. + chgt/5.)
!!...    Insure khgt within proper limits
        If (khgt.lt.1)khgt = 1
        If (khgt.gt.nhgt-1)khgt = nhgt - 1
!!...    MGCM latitude index (ilat) from current latitude (clat)
        ilat = 1 + Ifloor_2001((clat+90.)/steplat)
        If (ilat.gt.nlat-1)ilat = nlat - 1
!!...    MGCM wind latitude index (ilatw).  MGCM winds are offset in
!!       latitude by 1/2 latitude grid step.
        ilatw = 2 + Ifloor_2001((clat+86.25)/steplat)
!!...    Insure ilatw within proper bounds
        If (ilatw.lt.2)ilatw = 2
        If (ilatw.gt.nlat-1)ilatw = nlat - 1
!!...    MTGCM latitude index (ilatt) from current latitude (clat)
        ilatt = 1 + Ifloor_2001((clat-tlat1st)/tsteplat)
!!...    Insure ilatt within proper bounds
        If (ilatt.lt.1)ilatt = 1
        If (ilatt.gt.nlatt-1)ilatt = nlatt - 1
!!...    MGCM boundary layer longitude index (jlon)
        jlon = Ifloor_2001(clonw/steplon)
        If (jlon.gt.nlon-1)jlon = nlon -1
!!...    Time of year index (ls) from input Ls value (als)
        ls = Ifloor_2001(als/30.)
        If (ls.gt.11)ls = 11
!!...    Dust index (mdust) for input dust optical depth (dusttau)
        If (dusttau.lt.dust(2))Then
          mdust = 1
        Else
          mdust = 2
        Endif
!!...    Increment of MGCM latitude (dlat) from grid point
        dlat = (clat - steplat*(ilat-1.) + 90.)/steplat
!!...    Increment of MTGCM latitude (dlatt) from grid point
        dlatt = (clat - tsteplat*(ilatt-1.) - tlat1st)/tsteplat
!!...    Insure dlatt within proper bounds near poles
        tpolefac = 1.
        If (ilatt.eq.1)Then
          tpolefac = 0.5
          If (dlatt.le.0.)Then
            dlatt = 0.
            tpolefac = 1. - (Abs(clat)-85.)/5.
          Endif
        Else If (ilatt.ge.nlat-1)Then
          tpolefac = 0.5
          If (dlatt.ge.1.)Then
            dlatt = 1.
            tpolefac = 1. - (Abs(clat)-85.)/5.
          Endif
        Endif
        If (dlatt.lt.-0.00001.or.dlatt.gt.1.00001)Stop ' Bad dlatt'
!!...    Increment of MGCM longitude (dlon) from grid point
        dlon = (clonw - steplon*jlon)/steplon
        If (dlat.lt.-0.00001.or.dlat.gt.1.00001)Stop ' Bad dlat'
        If (dlon.lt.-0.00001.or.dlon.gt.1.00001)Stop ' Bad dlon'
!!...    Increment of MGCM latitude from (offset) wind grid point
        dlatw = (clat - steplat*(ilatw-2.) + 86.25)/steplat
        wpolefac = 1.
        If (ilatw.eq.2)Then
          wpolefac = 0.75
          If (dlatw.le.0.)Then
            wpolefac = 1. - (Abs(clat)-85.)/5.
            dlatw = 0.
          Endif
        Else If (ilatw.ge.nlat-1)Then
          wpolefac = 0.75
          If (dlatw.ge.1.)Then
            wpolefac = 1. - (Abs(clat)-85.)/5.
            dlatw = 1.
          Endif
        Endif
        If (dlatw.lt.-0.00001.or.dlatw.gt.1.00001)Stop ' Bad dlatw'
!!...    Increment of solar activity (F10.7 at 1AU) for MTGCM data
        mf10 = 1
        df10 = Zlogr_2001(F107,f10val(mf10))/Zlogr_2001(f10val(mf10+1),  &
         f10val(mf10))
!!...    Get areoid radius and topographic height at current lat, lon
        Call cps_RELLIPS_2001(clat,clonw,careoid,chgt,gz,Oldrref,ctopohgt,   &
         calbedo)
!!...    Compute topographic relief factor for simplified mountain
!!       wave perturbation model
        toprelief = real(25. + ctopohgt,kind=4)
!!...    Use topographic height if input height is <= -10 km
        If (chgt.le.-10.)chgt = ctopohgt + hgtasfc
!!...    Find height index (k1st) of first 0-80 km MGCM level above
!!       surface topographic height
        k1st = Ifloor_2001(2. + (ctopohgt + 1.)/5.)
        If (k1st.lt.1)k1st = 1
!!...    Find Ls increment (dls) from Ls "grid" on input data
        dls = (als - 30.*ls)/30.
!!...    Insure Ls increment within proper bounds
        If (dls.lt.-0.00001.or.dls.gt.1.00001)Then
          Write(*,*)als,ls,dls
          Stop ' Bad Ls'
        Endif
!!...    Compute dust increment (ddust) from dust optical depth "grid"
!!       points
        ddust = Zlogr_2001(dusttau,dust(mdust))/                         &
          Zlogr_2001(dust(mdust+1),dust(mdust))
!!...    Insure ddust within proper range
        If (ddust.lt.0.0_PM_REEL.and.mdust.eq.1)ddust = (dusttau - dust(1))/     &
         (dust(2) - dust(1))
        If (ddust.gt.1.0_PM_REEL)ddust = 1.0_PM_REEL
!!...    Initialize ZF = height of 1.26 nbar level (output value if
!!       current height < 80 km)
        ZF = 999._PM_REEL
!!...    Assign MTGCM height offset from input zoffset or array offsets
        globoffst = 0.0_PM_REEL
          If (ibougher.le.2)Then
            offset(1,1) = offsets(ls,mdust)
            offset(1,2) = offsets(ls,mdust+1)
            offset(2,1) = offsets(ls+1,mdust)
            offset(2,2) = offsets(ls+1,mdust+1)
            Call TwoD_2001(dls,ddust,offset,globoffst)
          Endif
          If (ibougher.le.0)Then
            hgtoffset = zoffset
          Else If (ibougher.eq.1)Then
            hgtoffset = zoffset - 2.5*Sin(Atan(1.)*als/45.)
          Else If (ibougher.eq.2)Then
            hgtoffset = globoffst
          Else
            Call TGCMterp_2001(1,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,       &
       VMGCM1,ZF,Tday1,Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1)
            Call TGCMterp_2001(2,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,       &
             VMGCM2,ZF,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,       &
             Dmax2,Dmin2)
            Hpres = 5./Zlogr_2001(PMGCM1,PMGCM2)
            Hpresday = real(5./Zlogr_2001(Pday1,Pday2),kind=4)
            Call MGCMterp_2001(nhgt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,    &
             VMGCM2,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,          &
             Dmax2,Dmin2)
            If (ibougher.eq.3)Then
              hgtoffset=Hpresday*Zlogr_2001(Pday2,Pday1)
            Else
              hgtoffset=Hpres*Zlogr_2001(PMGCM2,PMGCM1)
            Endif
          Endif
!!...      Add height offset due to dust storm
          hgtoffset = hgtoffset + dustoffset
!!...    MTGCM height index (khgtt) for current height
        khgtt = Ifloor_2001((chgt-hgtoffset-75.)/5.)
!!...    Insure khgtt within proper limits
        If (khgtt.lt.1)khgtt = 1
        lhgtt = 1
        If (khgtt.eq.1.and.hgtoffset.lt.-4.0)Then
          khgtt = 2
          lhgtt = 2
        Endif
        If (khgtt.gt.nhgtt-1)khgtt = nhgtt - 1
!!...    Use MTGCM interpolation if height >= 80 km
        If (chgt.ge.80.+hgtoffset+5.*(lhgtt-1.))Then
!!...      Get temperature, pressure, density, and wind components at
!!         height indexes above and below current height
          Call TGCMterp_2001(khgtt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,     &
       VMGCM1,ZF,Tday1,Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1)
          Call TGCMterp_2001(khgtt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,   &
           VMGCM2,ZF,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,         &
           Dmax2,Dmin2)
!!...      Height grid points above and below current height
          z1 = real(75. + 5.*khgtt + hgtoffset,kind=4)
          z2 = real(80. + 5.*khgtt + hgtoffset,kind=4)
!!...      Apply MTGCM height offset to ZF altitude
          ZF = ZF + hgtoffset
!!...      Pressure and density scale heights
          Hpres = (z2 - z1)/Zlogr_2001(PMGCM1,PMGCM2)
          Hpresday = real((z2 - z1)/Zlogr_2001(Pday1,Pday2),kind=4)
          Hdens = (z2 - z1)/Zlogr_2001(DMGCM1,DMGCM2)
!!...    Use MGCM interpolation at 75 km and MTGCM interpolation at 80
!!       km if height between 75 and 80 km
        Else If (chgt.ge.75.)Then
!!...      Get temperature, pressure, density, and wind components at
!!         heights above and below current height
          Call MGCMterp_2001(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,      &
          VMGCM1,Tday1,Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1)
          Call TGCMterp_2001(lhgtt,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,     &
              VMGCM2,ZF80,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,    &
              Dmax2,Dmin2)
          z1 = 75.
          z2 = real(80. + hgtoffset + 5.*(lhgtt-1.),kind=4)
!!...      Apply 'equivalent' multiplier for offset between 75 km & ZF
          If (ibougher.le.1.or.dustoffset.gt.0.0)Then
            z1ofs = 45.
            ofsmgcm = real(hgtoffset-globoffst,kind=4)
            If (ibougher.gt.1)ofsmgcm = dustoffset
            ofsz1 = ofsmgcm*(z1 - z1ofs)/(z2 - z1ofs)
            ofsmult1 = Exp(ofsz1/7.)
            PMGCM1 = PMGCM1*ofsmult1
            DMGCM1 = DMGCM1*ofsmult1
            Pday1 = Pday1*ofsmult1
            Dday1 = Dday1*ofsmult1
            Dmax1 = Dmax1*ofsmult1
            Dmin1 = Dmin1*ofsmult1
          Endif
!!...      Pressure and density scale heights (km)
          Hpres = (z2 - z1)/Zlogr_2001(PMGCM1,PMGCM2)
          Hpresday = real((z2-z1)/Zlogr_2001(Pday1,Pday2),kind=4)
          Hdens = (z2 - z1)/Zlogr_2001(DMGCM1,DMGCM2)
!!...    Use surfterp routine if current height within boundary layer
        Else If (chgt.le.ctopohgt+dzbl(nbl))Then
!!...      Set index for surface layer data
          jbl = 1
          If(chgt.ge.ctopohgt+dzbl(2))jbl = 2
!!...      Get temperature, pressure, density, and wind components at
!!         heights above and below current height
          Call surfterp_2001(jbl+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,    &
           VMGCM2,Hpres,Hdens,ctopohgt,Tday2,Pday2,Dday2,Uday2,Vday2,   &
           Hpresday,Tmax2,Tmin2,Dmax2,Dmin2,Tat5m  )
          Call surfterp_2001(jbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,      &
           VMGCM1,Hpres1,Hdens1,ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1, &
           Hpresday,Tmax1,Tmin1,Dmax1,Dmin1,Tat5m  )
!!...      Heights at two boundary layer levels
          z1 = real(ctopohgt + dzbl(jbl),kind=4)
          z2 = real(ctopohgt + dzbl(jbl+1),kind=4)
!!...      Density scale heights for max,min values
!!...      Perturbation factor = surface value
          pertfact = real(pert0,kind=4)
!!...    Use MGCMterp routine if height above boundary layer levels and
!!        height <= 75 km
        Else If (chgt.ge.5.*(k1st-1.))Then
!!...      Get temperature, pressure, density, and wind components at
!!         heights above and below current height
          Call MGCMterp_2001(khgt,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,      &
          VMGCM1,Tday1,Pday1,Dday1,Uday1,Vday1,Tmax1,Tmin1,Dmax1,Dmin1)
          Call MGCMterp_2001(khgt+1,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,    &
           VMGCM2,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,            &
           Dmax2,Dmin2)
!!...      Heights at grid points above and below current level
          z1 = 5.*(khgt-1.)
          z2 = 5.*khgt
!!...      Apply 'equivalent' multiplier for offset below 75 km
          If (ibougher.le.1.or.dustoffset.gt.0.0)Then
            z1ofs = 45.
            z2ofs = real(80. + hgtoffset + 5.*(lhgtt-1.),kind=4)
            ofsmgcm = real(hgtoffset-globoffst,kind=4)
            If (ibougher.gt.1)ofsmgcm = dustoffset
            If (z1.le.45.)Then
              ofsmult1 = 1.
            Else
              ofsz1 = ofsmgcm*(z1-z1ofs)/(z2ofs-z1ofs)
              ofsmult1 = Exp(ofsz1/7.)
            Endif
            If (z2.le.45.)Then
              ofsmult2 = 1.
            Else
              ofsz2 = ofsmgcm*(z2-z1ofs)/(z2ofs-z1ofs)
              ofsmult2 = Exp(ofsz2/7.)
            Endif
            PMGCM1 = PMGCM1*ofsmult1
            DMGCM1 = DMGCM1*ofsmult1
            PMGCM2 = PMGCM2*ofsmult2
            DMGCM2 = DMGCM2*ofsmult2
            Pday1 = Pday1*ofsmult1
            Dday1 = Dday1*ofsmult1
            Dmax1 = Dmax1*ofsmult1
            Dmin1 = Dmin1*ofsmult1
            Pday2 = Pday2*ofsmult2
            Dday2 = Dday2*ofsmult2
            Dmax2 = Dmax2*ofsmult2
            Dmin2 = Dmin2*ofsmult2
          Endif
!!...      Pressure and density scale heights (km)
          Hpres = (z2 - z1)/Zlogr_2001(PMGCM1,PMGCM2)
          Hpresday = real((z2 - z1)/Zlogr_2001(Pday1,Pday2),kind=4)
          Hdens = (z2 - z1)/Zlogr_2001(DMGCM1,DMGCM2)
!!...    Use surfterp at top of boundary layer and MGCMterp at 1st level
!!       above boundary layer if height between boundary layer and
!!       height index k1st
        Else
!!...      Get temperature, pressure, density, and wind components at
!!         heights above and below current height
          Call surfterp_2001(nbl,time,TMGCM1,PMGCM1,DMGCM1,UMGCM1,       &
           VMGCM1,Hpres,Hdens,ctopohgt,Tday1,Pday1,Dday1,Uday1,Vday1,   &
           Hpresday,Tmax1,Tmin1,Dmax1,Dmin1,Tat5m  )
          Call MGCMterp_2001(k1st,time,TMGCM2,PMGCM2,DMGCM2,UMGCM2,      &
          VMGCM2,Tday2,Pday2,Dday2,Uday2,Vday2,Tmax2,Tmin2,Dmax2,Dmin2)
!!...      Heights at grid points above and below current level
          z1 = real(ctopohgt + dzbl(nbl),kind=4)
          z2 = 5.*(k1st-1.)
!!...      Density scale heights for max,min values
        Endif
!!...    Get gas constant from pressure, density, and temperature
        If (chgt.le.ctopohgt)Then
          Rgas = real(PMGCM1/(DMGCM1*TMGCM1),kind=4)
          Rgasday = real(Pday1/(Dday1*Tday1),kind=4)
          dhgt = real((ctopohgt - z1)/(z2 - z1),kind=4)
        Else
          dhgt = real((chgt - z1)/(z2-z1),kind=4)
          R1 = real(PMGCM1/(DMGCM1*TMGCM1),kind=4)
          R2 = real(PMGCM2/(DMGCM2*TMGCM2),kind=4)
          Rgas1 = real(Pday1/(Dday1*Tday1),kind=4)
          Rgas2 = real(Pday2/(Dday2*Tday2),kind=4)
          Rgas = R1 + dhgt*(R2 - R1)
          Rgasday = Rgas1 + dhgt*(Rgas2 - Rgas1)
        Endif
!!...    Use logarithmic wind and temperature profiles (with surface
!!       roughness z0) if height below lowest boundary layer level
        If (chgt.le.ctopohgt + dzbl(2))Then
!!...      Convert surface roughness to km
          z0 = zwsfc/1000._PM_REEL
!!...      Save ground surface temperature for output
          Tgrnd = TMGCM1
!!...      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if
!!         over ice (T <= CO2 sublimation temperature + 5K)
          Tcheck = TMGCM1
          Call SublTchk_2001(Tcheck,PMGCM2,Tsubl)
!!...      If surface temperature near sublimation point, set polar ice
!!           indicator on (= 1) and re-set surface roughness
          icepolar = 0
          If (TMGCM1.le.Tsubl+5.)Then
            z0 = 1.0e-7
            icepolar = 1
          Endif
          uhgt = chgt - ctopohgt
          If (uhgt.lt.z0)uhgt = z0
!!...      Compute logarithmic boundary layer shape factor for surface
!!         to lowest boundary layer level
          factor = Zlogr_2001(uhgt,z0)/Zlogr_2001(dzbl(2),z0)
!!...      Apply factor for wind; assume non-slip condition (wind=0 at
!!         surface)
          cuwin = UMGCM2*factor
          cvwin = VMGCM2*factor
          EWwnDay = real(Uday2*factor,kind=4)
          NSwnDay = Vday2*factor
!!...      Set up parameters to evaluate temperature boundary layer
!!         Convert heights to meters for input to bltp subroutine
          z5 = dzbl(2)*1000.
          zeval = uhgt*1000.
!!...      Get value of local gravity
          Call cps_RELLIPS_2001(clat,clonw,Rref,chgt,gz,Oldrref,topohgt,     &
              albedo)
!!...      Use Ames MGCM boundary layer model for current temperature
!!         Get specific heat at constant pressure
          CpofT = Cp_2001(TMGCM2)
          Call bltp_2001(gz,CpofT,TMGCM1,z5,TMGCM2,UMGCM2,VMGCM2,zeval,  &
           factor,ctemp)
!!...      Use Ames MGCM boundary layer model for daily avg temperature
          CpofT = Cp_2001(Tday2)
          Call bltp_2001(gz,CpofT,Tday1,z5,Tday2,Uday2,Vday2,zeval,      &
           factor,Tempday)
!!...      Use Ames MGCM boundary layer model for daily max temperature
          CpofT = Cp_2001(Tmax2)
          Call bltp_2001(gz,CpofT,Tmax1,z5,Tmax2,Uday2,Vday2,zeval,      &
           factor,Tempmax)
!!...      Use Ames MGCM boundary layer model for daily min temperature
          CpofT = Cp_2001(Tmin2)
          Call bltp_2001(gz,CpofT,Tmin1,z5,Tmin2,Uday2,Vday2,zeval,      &
           factor,Tempmin)
!!...      Pressure at current position from pressure scale height
          cpres = PMGCM2* Exp((z2 - chgt)/Hpres)
          PresDay = Pday2*Exp((z2 - chgt)/Hpresday)
!!...      Density at current position from gas law
          cdens = cpres/(Rgas*ctemp)
          DensDay = real(Presday/(RgasDay*TempDay),kind=4)
!!...      Daily maximum and minimum density
          Densmin = real(DensDay*(Dmin1/Dday1 + factor*((Dmin2/Dday2)-   &
           (Dmin1/Dday1))),kind=4)
          Densmax = real(DensDay*(Dmax1/Dday1 + factor*((Dmax2/Dday2)-   &
           (Dmax1/Dday1))),kind=4)
!!...    Use linear height interpolation if above logarithmic
!!       surface layer
        Else
          dhgt = real((chgt - z1)/(z2-z1),kind=4)
          cuwin = UMGCM1 + dhgt*(UMGCM2 - UMGCM1)
          cvwin = VMGCM1 + dhgt*(VMGCM2 - VMGCM1)
          EWwnDay = real(Uday1 + dhgt*(Uday2 - Uday1),kind=4)
          NSwnDay = Vday1 + dhgt*(Vday2 - Vday1)
!!...      Interpolate temperature to current height
          ctemp = TMGCM1 + dhgt*(TMGCM2 - TMGCM1)
          TempDay = Tday1 + dhgt*(Tday2 - Tday1)
          Tempmax = Tmax1 + dhgt*(Tmax2 - Tmax1)
          Tempmin = Tmin1 + dhgt*(Tmin2 - Tmin1)
!!...      Pressure at current position from pressure scale height
          cpres = PMGCM2* Exp((z2 - chgt)/Hpres)
          PresDay = Pday2*Exp((z2 - chgt)/Hpresday)
!!...      Density at current position from gas law
          cdens = cpres/(Rgas*ctemp)
          DensDay = real(Presday/(RgasDay*TempDay),kind=4)
!!...      Daily maximum and minimum density
          Densmin = real(DensDay*(Dmin1/Dday1 + dhgt*((Dmin2/Dday2)-     &
           (Dmin1/Dday1))),kind=4)
          Densmax = real(DensDay*(Dmax1/Dday1 + dhgt*((Dmax2/Dday2)-     &
           (Dmax1/Dday1))),kind=4)
        Endif
!!...    Set specific bogus values of pressure or density scale heights
!!       are out of range
        If (Hpres.lt.-9.99)Hpres = -9.99
        If (Hpres.gt.99.99)Hpres = 99.99
        If (Hdens.lt.-9.99)Hdens = -9.99
        If (Hdens.gt.99.99)Hdens = 99.99
!!...    Compute perturbation factor, unless it has already been set
        If (pertfact.lt.pert0)Then
!!...      Perturbation factor from simplified mountain wave model
          pertfact = real(0.01*toprelief*Exp((chgt-100.)/40.),kind=4)
          If (pertfact.gt.pertmax)pertfact = real(pertmax,kind=4)
          If (pertfact.lt.pert0)pertfact = real(pert0,kind=4)
          If (chgt.ge.100.0.and.pertfact.lt.pertmin) then
            pertfact = real(pertmin,kind=4)
          end if
        Endif
        Return
      End Subroutine MarsGCM_2001
!!-----------------------------------------------------------------------
      Subroutine orbit_2001(xdate,latsun,lonsun,Lsubs,radius)
!!...  Computes latsun, lonsun = latitude and longitude of sub-solar
!!     point on the surface, Lsubs = areocentric longitude of the Sun
!!     (Ls), and radius = current orbital radius from Sun to Mars, for
!!     input Julian date and time, xdate
      implicit real(kind=PM_REEL) (a-h,o-z)
      real(kind=PM_REEL), intent(OUT) :: latsun,lonsun,Lsubs,radius
      real(kind=PM_REEL) :: ls0,lon0,ls,lon
!!     Use new value for perls (old value = 6.8697964d2)
      data ls0,perls,TWOPI/6.36d0,6.8697479d2,6.28318530718d0/
      data lon0,perlon/3.5758d2,1.02749118d0/
!!...  a0-a6 = Fourier coefficients for radius calculation
      real(kind=PM_REEL) :: a0,a1,a2,a3,a4,a5,a6
      data a0,a1,a2,a3,a4,a5,a6/1.5303331d0,.13661274d0,.38073649d-1,    &
      -.34125165d-2,.56508078d-2,-.31633117d-3,-.33458978d-3/
!!...  b0-b6 = Fourier coefficients for latsun calculation
      real(kind=PM_REEL) :: b0,b1,b2,b3,b4,b5,b6
      data b0,b1,b2,b3,b4,b5,b6/2.2057565d0,2.4703515d1,-1.5459399d0,    &
      -0.50388237d0,2.2083870d0,-0.39583136d0,-0.077578018d0/
!!...  c0-c6 = Fourier coefficients for Lsubs calculation
      real(kind=PM_REEL) :: c0,c1,c2,c3,c4,c5,c6
      data c0,c1,c2,c3,c4,c5,c6/-9.9300079d0,-2.8679217d0,1.0298739d1,   &
      -0.53266474d0,-0.32173731d0,0.36206144d-1,-0.034765340d0/
!!...  d0-d6 = Fourier coefficients for lonsun calculation
      real(kind=PM_REEL) :: e0,e1,e2,e3,e4,e5,e6
      Data e0,e1,e2,e3,e4,e5,e6/-.61024174D-01,1.8014752D0,.35080142D0,  &
      .11820528D0,-.23869660D0,-.56879633D-01,-.42787450D-01/
      Data d0,d1,d2,d3,d4,d5,d6/-3.4766116D0,10.199478D0,2.7450878D0,    &
      -3.2726522D0,-.67435053D-01,.52016400D0,.11903292D0/
!!...  per1 = 687 day period; per2 = 777 day period
      per1=6.87d2/TWOPI
      per2=7.77d2/TWOPI
      PI180 = TWOPI/3.60d2
!!...  DATE = modified Julian date and time for calculations
      DATE = XDATE - 2.442779d6 + 0.5d0
!!...  xls = part of Lsubs that increases linearly with date
      xls = ls0 + 3.60d2*date/perls
!!...  xlon = part of lonsun that increases linearly with date
      xlon = lon0 + 3.60d2*(date - 2.922d3)/perlon
!!...  TIME1 = time variable for period PER1
      TIME1=DATE/PER1
!!...  Lsubs calculation (REAL*8)
      LS=C0+C1*DSIN(TIME1)+C2*DCOS(TIME1)+C3*DSIN(2.0d0*TIME1)           &
      +C4*DCOS(2.0d0*TIME1)+C5*DSIN(3.0d0*TIME1)+C6*DCOS(3.0d0*TIME1)   &
      +XLS
!!...  latsun calculation (REAL*8)
      DS=B0+B1*DSIN(TIME1)+B2*DCOS(TIME1)+B3*DSIN(2.0d0*TIME1)           &
      +B4*DCOS(2.0d0*TIME1)+B5*DSIN(3.0d0*TIME1)+B6*DCOS(3.0d0*TIME1)
!!...  lonsun calculation (REAL*8)
      time = (date-2.921d3)/per1
      time2 = (date-2.921d3)/per2
      xlon=D0+D1*DSIN(TIME)+D2*DCOS(TIME)+D3*DSIN(2.0d0*TIME)            &
      +d4*DCOS(2.0d0*time)+d5*DSIN(3.0d0*time)+d6*DCOS(3.0d0*time)      &
      + xlon
      LON=E0+E1*DSIN(time2)+E2*DCOS(time2)+E3*DSIN(2.0d0*time2)          &
      +E4*DCOS(2.0d0*time2)+E5*DSIN(3.0d0*time2)+E6*DCOS(3.0d0*time2)   &
      + xlon
!!...  Put Lsubs and lonsun into 0-360 degree range
      LS = DMOD(LS,3.6D2)
      LON = DMOD(LON,3.6D2)
      if(ls.lt.0.)ls = ls + 3.60D2
      if(lon.lt.0.)lon = lon + 3.60D2
!!...  radius calculation (REAL*8)
      rad = a0 + a1*DSIN(time1) + a2*DCOS(time1)                         &
      + a3*DSIN(2.0d0*time1) + a4*DCOS(2.0d0*time1)                     &
      + a5*DSIN(3.0d0*time1) + a6*DCOS(3.0d0*time1)
!!...  Transfer latsun, lonsun, Lsubs and radius to REAL*4 values
      latsun = ds
      lonsun = lon
      Lsubs = LS
      radius = rad
      RETURN
    end Subroutine orbit_2001
!!-----------------------------------------------------------------------
    Real  Function PPND_2001(p, ifault)
      integer :: ifault
!!
!!     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118
!!
!!     Produces normal deviate corresponding to lower tail area of p.
!!     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0
!!     otherwise.  If ifault = 1, PPND value is set to 0.
!!     Single precision version with error epsilon = 2 ** (-31).
!!     For double precision version, change REAL to DOUBLE PRECISION
!!     in the FUNCTION statement and the declaration of variables;
!!     change E0 to D0 in the DATA statements and change ABS, ALOG
!!     and SQRT to DABS, DLOG and DSQRT in the assignment statements.
!!     The hash sums are the sums of the moduli of the coefficients.
!!     They have no inherent meanings, but are included for use in
!!     checking transpositions.
!!
      Real :: zero, split, half, one, a0, a1, a2, a3, b1, b2, b3, b4
      real :: c0, c1, c2, c3, d1, d2, p, q, r
!!
      Data zero, half, one, split /0.0E0, 0.5E0, 1.0E0, 0.42E0/
!!
      Data a0 /      2.50662823884E0/,                                  &
      a1 /     -18.61500062529E0/,                                      &
      a2 /      41.39119773534E0/,                                      &
      a3 /     -25.44106049637E0/,                                      &
      b1 /      -8.47351093090E0/,                                      &
      b2 /      23.08336743743E0/,                                      &
      b3 /     -21.06224101826E0/,                                      &
      b4 /       3.13082909833E0/
!!
!!     Hash sum for a & b = 143.70383558076
!!
      Data c0 /     -2.78718931138E0/,                                  &
      c1 /      -2.29796479134E0/,                                      &
      c2 /       4.85014127135E0/,                                      &
      c3 /       2.32121276858E0/,                                      &
      d1 /       3.54388924762E0/,                                      &
      d2 /       1.63706781897E0/
!!
!!     Hash sum for c & d = 17.43746520924
!!
!!
      ifault = 0
      q = p - half
      If (ABS(q) .gt. split) goto 1
      r = q * q
      PPND_2001 = q * (((a3 * r + a2) * r + a1) * r + a0) / &
           ((((b4 * r + b3) * r + b2) * r + b1) * r + one)
      Return
    1 r = p
      If (q .gt.zero) r = one - p
      If (r .lt.zero) goto 2
      r = SQRT(-ALOG(r))
      PPND_2001 = (((c3 * r + c2) * r + c1) * r + c0) / &
           ((d2 * r + d1) * r + one)
      If (q .lt. zero) PPND_2001 = -PPND_2001
      Return
    2 ifault = 1
      PPND_2001 = zero
      Return
    End Function PPND_2001
!!-----------------------------------------------------------------------
    Real Function Random_2001(L)
      integer :: L
!!
!!     Algorithm AS 183 Appl. Statist. (1982) Vol. 31, p.188
!!
!!     Returns a pseudo-random number rectangularly distributed
!!     between 0 and 1.
!!
!!     IX, IY and IZ should be set to integer values between
!!     1 and 30,000 before first entry.
!!
!!     Integer arithmetic up to 30323 is required.
!!
!!     Returns L = 0 unless random = 0 or random = 1, in which
!!     case L = 1
!!
      Real :: one,zero
      Data one,zero/1.0E0,0.0E0/

! Common RANDCOM_2001
      integer :: IX, IY, IZ
      Common /RANDCOM_2001/ IX, IY, IZ

      IX = 171 * Mod(IX, 177) -  2 * (IX / 177)
      IY = 172 * Mod(IY, 176) - 35 * (IY / 176)
      IZ = 170 * Mod(IZ, 178) - 63 * (IZ / 178)
!!
      If (IX .lt. 0) IX = IX + 30269
      If (IY .lt. 0) IY = IY + 30307
      If (IZ .lt. 0) IZ = IZ + 30323
!!
!!     If integer arithmetic up to 5,212,632 is available,
!!     the preceding 6 statements may be replaced by
!!
!!     IX = Mod(171 * IX, 30269)
!!     IY = Mod(172 * IY, 30307)
!!     IZ = Mod(170 * IZ, 30323)
!!
!!     On some machines, this may slightly increase the speed.
!!     The results should be identical.
!!
      Random_2001 = Amod(float(IX) / 30269.0 + float(IY) / 30307.0 + &
           float(IZ) / 30323.0, one)
      L = 0
      If (Random_2001 .le. zero .or. Random_2001 .ge. one) L = 1
      Return
    End Function Random_2001
!!-----------------------------------------------------------------------
    Subroutine ReadMGCM_2001(GCMDIR,version)
!!...    Reads NASA Ames Mars General Circulation Model (MGCM) 0-80 km
!!       data (in binary format) and loads into data arrays for common
!!       MGCMdata
!!       GCMDIR is directory name where MGCM data resides
        Character*(*) :: GCMDIR
        Character*11 :: sysform
        Character*1 :: version
!!...    Set parameters for ndust=number of dust optical depths, nhgt=
!!       number of MGCM heights, nlat=number of MGCM latitudes
        integer, parameter :: ndust = 3
        integer, parameter :: nhgt = 17
        integer, parameter :: nlat = 25
        integer, parameter :: nbl = 3
        integer, parameter :: nf10 = 2
!!!...    Set parameter for form= in binary file open statement
        Parameter (sysform = 'unformatted')
! Common MGCMparm_2001
        Character*2 :: dustc(ndust),solact (nf10)
        real(kind=PM_REEL) :: dust(ndust),dzbl(nbl),zwsfc,f10val(nf10)
        Common /MGCMparm_2001/dust,dzbl,zwsfc,f10val,  &
         dustc,solact

! common MGCMdata_2001
        real :: TZA0(nhgt,nlat,0:12,ndust),               &
         TZA1(nhgt,nlat,0:12,ndust),TZP1(nhgt,nlat,0:12,ndust),         &
         TZA2(nhgt,nlat,0:12,ndust),TZP2(nhgt,nlat,0:12,ndust),         &
         PZA0(nhgt,nlat,0:12,ndust),PZA1(nhgt,nlat,0:12,ndust),         &
         PZP1(nhgt,nlat,0:12,ndust),PZA2(nhgt,nlat,0:12,ndust),         &
         PZP2(nhgt,nlat,0:12,ndust),DZA0(nhgt,nlat,0:12,ndust),         &
         UZA0(nhgt,nlat,0:12,ndust),UZA1(nhgt,nlat,0:12,ndust),         &
         UZP1(nhgt,nlat,0:12,ndust),UZA2(nhgt,nlat,0:12,ndust),         &
         UZP2(nhgt,nlat,0:12,ndust),VZA0(nhgt,nlat,0:12,ndust),         &
         VZA1(nhgt,nlat,0:12,ndust),VZP1(nhgt,nlat,0:12,ndust),         &
         VZA2(nhgt,nlat,0:12,ndust),VZP2(nhgt,nlat,0:12,ndust)
        Common /MGCMdata_2001/TZA0,                &
         TZA1,TZP1,         &
         TZA2,TZP2,         &
         PZA0,PZA1,         &
         PZP1,PZA2,         &
         PZP2,DZA0,         &
         UZA0,UZA1,         &
         UZP1,UZA2,         &
         UZP2,VZA0,         &
         VZA1,VZP1,         &
         VZA2,VZP2
!! Variables locales
        integer :: i, k, ihgt, m,lendir,ils
        integer :: ls, lsea, ilatstep,lastls, lat
        Real :: xlat, ylat
!!
!!...    Initialize last Ls value processed to 0
        lastls = 0
!!...    Set ilatstep = latitude step size x 10
        ilatstep = 1800/(nlat - 1)
!!...    Compute string length for directory name
        lendir = index(GCMDIR,' ')-1
        If (lendir.lt.1.or.lendir.gt.250)lendir=250
!!...    Step through all dust optical depths
        Do 100 m = 1,ndust
#ifdef __GFORTRAN__
!!...      Open MGCM input files for temperature, pressure, and density
          Open(32,file=GCMDIR(1:lendir)//'tpdlo'//dustc(m)//version//    &
          '.bin', convert='big_endian', form=sysform,status='old')
!!...      Open MGCM input files for wind components
          Open(33,file=GCMDIR(1:lendir)//'uvlo'//dustc(m)//version//     &
          '.bin', convert='big_endian', form=sysform,status='old')
#else
!!...      Open MGCM input files for temperature, pressure, and density
          Open(32,file=GCMDIR(1:lendir)//'tpdlo'//dustc(m)//version//    &
          '.bin',form=sysform,status='old')
!!...      Open MGCM input files for wind components
          Open(33,file=GCMDIR(1:lendir)//'uvlo'//dustc(m)//version//     &
          '.bin',form=sysform,status='old')
#endif
!!...      Step through all Ls values
          Do 52 lsea = 30,360,30
            ls = lsea/30
!!...      Step through all latitude grid points
          Do 51 lat = -900,900,ilatstep
            xlat = lat/10.
            i = 1 + (lat+900)/ilatstep
!!...      Step through all height levels
          Do 50 k = nhgt,1,-1
!!...        Read (binary) tide coefficients for temperature, pressure,
!!           and density
            Read(32,End=99)ils,ihgt,ylat,TZA0(k,i,ls,m),TZA1(k,i,ls,m),  &
             TZP1(k,i,ls,m),TZA2(k,i,ls,m),TZP2(k,i,ls,m),              &
             PZA0(k,i,ls,m),PZA1(k,i,ls,m),PZP1(k,i,ls,m),              &
             PZA2(k,i,ls,m),PZP2(k,i,ls,m),DZA0(k,i,ls,m)
            If (ils.ne.lsea)Stop ' Bad tpd Ls'
            If (ihgt.ne.5*(k-1))Stop ' Bad tpd Height'
            If (ylat.ne.xlat)Stop ' Bad tpd Latitude'
!!...        Read (binary) tide coefficients for wind components
            Read(33,End=99)ils,ihgt,ylat,UZA0(k,i,ls,m),UZA1(k,i,ls,m),  &
             UZP1(k,i,ls,m),UZA2(k,i,ls,m),UZP2(k,i,ls,m),              &
             VZA0(k,i,ls,m),VZA1(k,i,ls,m),VZP1(k,i,ls,m),              &
             VZA2(k,i,ls,m),VZP2(k,i,ls,m)
            If (ils.ne.lsea)Stop ' Bad uv Ls'
!!...        Reset value of last Ls processed
            lastLs = iLs
            If (ihgt.ne.5*(k-1))Stop ' Bad uv Height'
            If (ylat.ne.xlat)Stop ' Bad uv Latitude'
  50      Enddo
  51      Enddo
  52      Enddo
!!...      Set data for Ls = 0 to data for Ls = 360
          Do 61 k = 1,nhgt
          Do 60 i = 1,nlat
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
  60      Enddo
  61      Enddo
!!...    Close input files to re-use same unit number for next dust
!!       value
        Close (32)
        Close (33)
        Goto 100
!!...    Terminate if not all Ls values have been processed
  99    If (lastLs.ne.360)Stop ' Incomplete 0-80 km MGCM data'
 100    Enddo
        Return
      End Subroutine ReadMGCM_2001
!!-----------------------------------------------------------------------
      Subroutine Readsurf_2001(GCMDIR,version)
!!...    Reads NASA Ames Mars General Circulation Model (MGCM) surface
!!       data (in binary format) and loads into data arrays for common
!!       surfdata
!!       GCMDIR is directory name where MGCM data resides
        Character*(*) :: GCMDIR
!!        Character*60 GCMDIR
        Character*11 :: sysform
        Character*1 :: version
!!...    Set parameter values for ndust=number of dust optical depths,
!!       nbl=number of boundary layer levels, nlat=number of MGCM lati-
!!       tudes, nlon=number of MGCM longitudes
        integer, parameter :: ndust = 3
        integer, parameter :: nbl = 3
        integer, parameter :: nlat = 25
        integer, parameter :: nlon = 40
        integer, parameter :: nf10 = 2
!!!...    Set parameter for form= in binary file open statement
        Parameter (sysform = 'unformatted')
! Common MGCMparm_2001
        Character*2 :: dustc(ndust),solact (nf10)
        real(kind=PM_REEL) :: dust(ndust),dzbl(nbl),zwsfc,f10val(nf10)
        Common /MGCMparm_2001/dust,dzbl,zwsfc,f10val,  &
         dustc,solact

! common surfdata_2001
        real :: TSA0(nbl,nlat,0:nlon,0:12,ndust),          &
         TSA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSP2(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA0(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         USP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         USP2(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA0(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSP2(nbl,nlat,0:nlon,0:12,ndust)
        Common /surfdata_2001/TSA0,         &
         TSA1,                              &
         TSP1,                              &
         TSA2,                              &
         TSP2,                              &
         USA0,                              &
         USA1,                              &
         USP1,                              &
         USA2,                              &
         USP2,                              &
         VSA0,                              &
         VSA1,                              &
         VSP1,                              &
         VSA2,                              &
         VSP2
!! Variables locales
        integer :: lon,k,ils, jlon, i, lat, ls, lsea, m, lendir,ilatstep,lastls
        Real :: ylat, xlat
!!
!!...    Initialize last Ls value processed to 0
        lastls = 0
!!...    Set ilatstep = latitude step size x 10
        ilatstep = 1800/(nlat - 1)
!!...    Compute string length for directory name
        lendir = index(GCMDIR,' ')-1
        If (lendir.lt.1.or.lendir.gt.250)lendir = 250
!!...    Step through all dust optical depths
        Do 100 m = 1,ndust
#ifdef __GFORTRAN__
!!...      Open surface data files for surface level
          Open(33,file=GCMDIR(1:lendir)//'sfc00'//dustc(m)//version//    &
          '.bin', convert='big_endian', form=sysform,status='old')
!!...      Open surface data files for 5 meter level above surface
          Open(34,file=GCMDIR(1:lendir)//'sfc05'//dustc(m)//version//    &
          '.bin', convert='big_endian', form=sysform,status='old')
!!...      Open surface data files for 30 meter level above surface
          Open(35,file=GCMDIR(1:lendir)//'sfc30'//dustc(m)//version//    &
          '.bin', convert='big_endian', form=sysform,status='old')
#else
!!...      Open surface data files for surface level
          Open(33,file=GCMDIR(1:lendir)//'sfc00'//dustc(m)//version//    &
          '.bin',form=sysform,status='old')
!!...      Open surface data files for 5 meter level above surface
          Open(34,file=GCMDIR(1:lendir)//'sfc05'//dustc(m)//version//    &
          '.bin',form=sysform,status='old')
!!...      Open surface data files for 30 meter level above surface
          Open(35,file=GCMDIR(1:lendir)//'sfc30'//dustc(m)//version//    &
          '.bin',form=sysform,status='old')
#endif
!!...      Step through all Ls values
          Do 53 lsea = 30,360,30
            ls = lsea/30
!!...      Step through all latitudes
          Do 52 lat = -900,900,ilatstep
            xlat = lat/10.
            i = 1 + (lat+900)/ilatstep
!!...      Step through all boundary layer levels
          Do 51 k = 1,nbl
!!...      Step through all longitudes
          Do 50 lon = nlon,1,-1
!!...        Read (binary) tide coefficients for temperature and wind
!!           components at all boundary layer levels
            If(k.eq.1)Then
             Read(32+k,End=99)ils,ylat,jlon,TSA0(k,i,lon,ls,m),          &
             TSA1(k,i,lon,ls,m),TSP1(k,i,lon,ls,m),TSA2(k,i,lon,ls,m),  &
             TSP2(k,i,lon,ls,m)
!!...         Assume surface wind = 0 (no slip condition)
             USA0(k,i,lon,ls,m)=0.0
             USA1(k,i,lon,ls,m)=0.0
             USP1(k,i,lon,ls,m)=0.0
             USA2(k,i,lon,ls,m)=0.0
             USP2(k,i,lon,ls,m)=0.0
             VSA0(k,i,lon,ls,m)=0.0
             VSA1(k,i,lon,ls,m)=0.0
             VSP1(k,i,lon,ls,m)=0.0
             VSA2(k,i,lon,ls,m)=0.0
             VSP2(k,i,lon,ls,m)=0.0
            Else
             Read(32+k,End=99)ils,ylat,jlon,TSA0(k,i,lon,ls,m),          &
             TSA1(k,i,lon,ls,m),TSP1(k,i,lon,ls,m),TSA2(k,i,lon,ls,m),  &
             TSP2(k,i,lon,ls,m),USA0(k,i,lon,ls,m),USA1(k,i,lon,ls,m),  &
             USP1(k,i,lon,ls,m),USA2(k,i,lon,ls,m),USP2(k,i,lon,ls,m),  &
             VSA0(k,i,lon,ls,m),VSA1(k,i,lon,ls,m),VSP1(k,i,lon,ls,m),  &
             VSA2(k,i,lon,ls,m),VSP2(k,i,lon,ls,m)
            Endif
            If (ils.ne.lsea)Stop ' Bad surface Ls'
!!...        Reset value of last Ls processed
            lastLs = iLs
            If (ylat.ne.xlat)Stop ' Bad surface Latitude'
            If (jlon.ne.9*lon)Stop ' Bad surface Longitude'
  50      Enddo
  51      Enddo
  52      Enddo
  53      Enddo
!!...      Set all values at Ls=0 to values at Ls=360
          Do 81 k = 1,nbl
          Do 80 i = 1,nlat
            Do 60 lon = 1,nlon
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
  60        Enddo
!!           Set all values at Lon=0 to values at Lon=360
            Do 70 ls = 0,12
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
  70        Enddo
80      Enddo
81      Enddo
!!...      Close input file units
          Close (33)
          Close (34)
          Close (35)
          Goto 100
!!...      Terminate if not all Ls values processed
  99      If (lastLs.ne.360)Stop ' Incomplete surface GCM data'
100    Enddo
        Return
      End Subroutine Readsurf_2001
!!-----------------------------------------------------------------------
     Subroutine ReadTGCM_2001(GCMDIR,version)
!!...    Reads University of Arizona Mars Thermospheric General Circula-
!!       tion Model (MTGCM) data (in binary format) and loads into data
!!       arrays for common TGCMdata
!!       GCMDIR is directory name where MTGCM data resides
        Character*(*) :: GCMDIR
        Character*11 :: sysform
        Character*1 :: version
!!...    Set parameter values for ndust=number of dust optical depths,
!!       nhgtt=number of MTGCM heights, nlatt=number of MTGCM latitudes
        integer, parameter :: ndust = 3
        integer, parameter :: nhgtt = 19
        integer, parameter :: nlatt = 36
        integer, parameter :: nf10 = 2
        integer, parameter :: nbl = 3
!!...    Set parameter for form= in binary file open statement
        Parameter (sysform = 'unformatted')

! Common MGCMparm_2001
        Character*2 :: dustc(ndust),solact (nf10)
        real(kind=PM_REEL) :: dustval(ndust),dzbl(nbl),zwsfc,f10val(nf10)
        Common /MGCMparm_2001/dustval,dzbl,zwsfc,f10val,dustc,solact

        Common /TGCMdata_2001/TTA0(nhgtt,nlatt,0:12,ndust,nf10),         &
         TTA1(nhgtt,nlatt,0:12,ndust,nf10),TTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),TTA2(nhgtt,nlatt,0:12,ndust,nf10),TTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),PTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         PTA1(nhgtt,nlatt,0:12,ndust,nf10),PTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),PTA2(nhgtt,nlatt,0:12,ndust,nf10),PTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),DTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         DTA1(nhgtt,nlatt,0:12,ndust,nf10),DTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),DTA2(nhgtt,nlatt,0:12,ndust,nf10),DTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),UTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         UTA1(nhgtt,nlatt,0:12,ndust,nf10),UTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),UTA2(nhgtt,nlatt,0:12,ndust,nf10),UTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),VTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         VTA1(nhgtt,nlatt,0:12,ndust,nf10),VTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),VTA2(nhgtt,nlatt,0:12,ndust,nf10),VTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),                                        &
         ZFA0(nlatt,0:12,ndust,nf10),ZFA1(nlatt,0:12,ndust,nf10),       &
         ZFP1(nlatt,0:12,ndust,nf10),ZFA2(nlatt,0:12,ndust,nf10),       &
         ZFP2(nlatt,0:12,ndust,nf10)
!! Variables locales
        Real :: zfdust
        integer :: ils, k, i, lat, lendir, m, n,ihgt
        integer :: ilat1, ilat2,ilatstep,ls, lsea,lastls
        Real :: xlat,YLAT
!!
!!...    Initialize last Ls value processed to 0
        lastls = 0
!!...    Set ilatstep = latitude step size x 10
        ilatstep = 1800/nlatt
!!...    Set initial and final latitudes (x 10) for stepping
        ilat1 = -900 + ilatstep/2
        ilat2 = 900 - ilatstep/2
!!...    Compute string length for directory name
        lendir = index(GCMDIR,' ')-1
        If (lendir.lt.1.or.lendir.gt.250)lendir = 250
!!...    Step through all solar activity levels
        Do 110 n = 1,nf10
          Open(34,file=GCMDIR(1:lendir)//'zfht'//solact(n)//version//    &
           '.txt',status='old')
!!...    Step through all dust optical depths
        Do 100 m = 1,ndust
#ifdef __GFORTRAN__
!!...      Open MTGCM data files for temperature, pressure, and density
          Open(32,file=GCMDIR(1:lendir)//'tpd'//solact(n)//dustc(m)//    &
           version//'.bin', convert='big_endian', form=sysform,status='old')
!!...      Open MTGCM data files for wind components
          Open(33,file=GCMDIR(1:lendir)//'uv'//solact(n)//dustc(m)//     &
           version//'.bin', convert='big_endian', form=sysform,status='old')
#else
!!...      Open MTGCM data files for temperature, pressure, and density
          Open(32,file=GCMDIR(1:lendir)//'tpd'//solact(n)//dustc(m)//    &
           version//'.bin',form=sysform,status='old')
!!...      Open MTGCM data files for wind components
          Open(33,file=GCMDIR(1:lendir)//'uv'//solact(n)//dustc(m)//     &
           version//'.bin',form=sysform,status='old')
#endif
!!...      Step through all Ls values
          Do 51 lsea = 30,360,30
            ls = lsea/30
!!...      Step through all latitudes
            Do 50 lat = ilat1,ilat2,ilatstep
               xlat = lat/10.
               i = 1 + (lat-ilat1)/ilatstep
               !!...      Step through all heights
               Do 40 k = 1,nhgtt
                  !!...        Read (binary) tide coefficients for temperature, pressure,
                  !!           and density
                  Read(32,End=99)ils,ihgt,ylat,TTA0(k,i,ls,m,n),               &
                       TTA1(k,i,ls,m,n),TTP1(k,i,ls,m,n),TTA2(k,i,ls,m,n),        &
                       TTP2(k,i,ls,m,n),PTA0(k,i,ls,m,n),PTA1(k,i,ls,m,n),        &
                       PTP1(k,i,ls,m,n),PTA2(k,i,ls,m,n),PTP2(k,i,ls,m,n),        &
                       DTA0(k,i,ls,m,n),DTA1(k,i,ls,m,n),DTP1(k,i,ls,m,n),        &
                       DTA2(k,i,ls,m,n),DTP2(k,i,ls,m,n)
                  If (ils.ne.lsea)Stop ' Bad tpd Ls'
                  If (ihgt.ne.80+5*(k-1))Stop ' Bad tpd Height'
                  If (ylat.ne.xlat)Stop ' Bad tpd Latitude'
!!...        Read (binary) tide coefficients for wind components
                  Read(33,End=99)ils,ihgt,ylat,UTA0(k,i,ls,m,n),               &
                       UTA1(k,i,ls,m,n),UTP1(k,i,ls,m,n),UTA2(k,i,ls,m,n),        &
                       UTP2(k,i,ls,m,n),VTA0(k,i,ls,m,n),VTA1(k,i,ls,m,n),        &
                       VTP1(k,i,ls,m,n),VTA2(k,i,ls,m,n),VTP2(k,i,ls,m,n)
                  If (ils.ne.lsea)Stop ' Bad uv Ls'
!!...        Reset last Ls value processed
                  lastLs = iLs
                  If (ihgt.ne.80+5*(k-1))Stop ' Bad uv Height'
                  If (ylat.ne.xlat)Stop ' Bad uv Latitude'
40             Enddo
!!...      Read tide coefficient data for ZF=height of 1.26 nbar level
               Read(34,*,End=99)zfdust,ils,ylat,ZFA0(i,ls,m,n),               &
                    ZFA1(i,ls,m,n),ZFP1(i,ls,m,n),ZFA2(i,ls,m,n),ZFP2(i,ls,m,n)
               dustdiff = zfdust-real(dustval(m))
               If (abs(dustdiff).gt.0)Stop ' Bad ZF dust value'
               If (ils.ne.lsea)Stop ' Bad ZF Ls'
               If (ylat.ne.xlat)Stop ' Bad ZF Latitude'
50          Enddo
51       Enddo
!!...      Set all values at Ls=0 to values at Ls=360
         Do 61 i = 1,nlatt
            ZFA0(i,0,m,n) = ZFA0(i,12,m,n)
            ZFA1(i,0,m,n) = ZFA1(i,12,m,n)
            ZFP1(i,0,m,n) = ZFP1(i,12,m,n)
            ZFA2(i,0,m,n) = ZFA2(i,12,m,n)
            ZFP2(i,0,m,n) = ZFP2(i,12,m,n)
            Do 60 k = 1,nhgtt
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
60          Enddo
61       Enddo
!!...    Close input file unit numbers
         Close (32)
         Close (33)
         Goto 100
!!...    Terminate if not all Ls values processed
99       If (lastLs.ne.360)Stop ' Incomplete 80-170 km MTGCM data'
100   Enddo
        Close (34)
110   Enddo
        Return
      End Subroutine ReadTGCM_2001
!!-----------------------------------------------------------------------
     SUBROUTINE cps_RELLIPS_2001 (LAT, LONW, Rref, z, gz ,Oldrref,ctopohgt, &
       calbedo)
      Real(kind=PM_REEL) :: LAT,LONW, Rref,z, gz ,Oldrref,ctopohgt, calbedo
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!     cps_RELLIPS_2001
!
!$Resume
!     Calcule le rayon de l'areoide martien pour le modèle GRAM2001
!
!$Description
!     Calcule le rayon de l'areoide martien pour le modele GRAM2001
!     Attention : s'utilise après le premier appel à cps_atmmarsgram_2001
!
!>    Calcule le rayon l'aréoide martien (Rref, km) de 1/2 degré en 1/2
! degré (MOLA), pour une latitude (LAT, deg), et une longitude Ouest (LonW, deg).
!> Calcule aussi l' acceleration due à la gravité gz (m/s**2), pour une
!     altitude z (km), à cette latitude et longitude.
!> Calcule enfin le rayon martien (Oldrref, km) de l'ellipsoïde de référence
!
!$Acces
!  PUBLIC
!
!$Usage
!     call cps_rellips_2001(xlat,xlon,Rmola,z,gz,Rell,topohgt,alb)
!
!$Arguments
!>E xlat     : <PM_REEL> Latitude (deg)
!>E xlon     : <PM_REEL> Longitude Ouest (deg)
!>E z        : <PM_REEL> Altitude (km)
!>S Rmola    : <PM_REEL> Rayon de l'areoide a xlat/xlon
!>S gz       : <PM_REEL> accélération due à la gravité (m/s**2)
!>S Oldrref  : <PM_REEL> le rayon de MARS a xlat, xlon l'ellipsoïde de référence (km)
!>S ctopohgt : <PM_REEL> Altitude (km)
!>S calbedo  : <PM_REEL> Albedo 
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

!!.... Calculates Mars areoid radius (Rref, in km) from 1/2 by 1/2
!!     degree MOLA data, for given latitude (LAT, in degrees), and West
!!     longitude (LonW, in degrees).
!!.... Also computes acceleration of gravity, gz in m/s**2, for
!!     altitude z in km, at given latitude, longitude
!!...  Also calculates MARS radius (Oldrref, in km) of reference
!!     ellipsoid
!!...  RADEG = DEGREES/RADIAN
      Real(kind=PM_REEL), parameter :: RADEG = 57.29577958_PM_REEL
!!...  Mars rotation rate, radians per second
      Real(kind=PM_REEL), parameter :: omega = 0.004061250_PM_REEL/RADEG
!!...  Planetary constant (GM) from Mars Pathfinder Project
!!     Planetary Constants and Models, JPL D-12947, Dec. 1995
!!     and JPL planetary ephemeris DE403
!!...  GM = gravitational constant * mass of Mars
      Real(kind=PM_REEL), parameter :: GM = 4.2828314258E7_PM_REEL
!!...  J2 = coefficient of 1st non-spherical gravity term
      Real(kind=PM_REEL), parameter :: J2 = 0.001958616128_PM_REEL
!!...  Old Mars-GRAM planetary radii A, B, and C
!!...  A, B = largest, smallest equatorial radius (km) of old ellipsoid
!!     Parameter (A = 3394.67)
!!     Parameter (B = 3393.21)
!!...  C = polar radius (km) of old ellipsoid
!!     Parameter (C = 3376.78)
!!...  New planetary radii A = B = equatorial radius of Goddard areoid
!!     used by MOLA team (average of 0.25N and 0.25S values); C = polar
!!     radius of Goddard areoid (average of 89.75N and 89.75S values)
      Real(kind=PM_REEL), parameter :: A = 3396.00_PM_REEL
      Real(kind=PM_REEL), parameter :: B = 3396.00_PM_REEL
      Real(kind=PM_REEL), parameter :: C = 3378.32_PM_REEL
! Variables locales
      Real(kind=PM_REEL) :: rz,P2, AB, AB1
      real(kind=PM_REEL) :: XX1,TLAT,TLAT1,YY1,Oldrref1
!!...  Mean equatorial radius for ellipsoid
      AB = SQRT(A * B)
      TLAT = TAN(LAT / RADEG)
!!...  XX, YY = squares of x, y components of local ellipsoid radius
      XX = real((AB * C)**2 / (C**2 + (AB * TLAT)**2),kind=4)
      YY = real(XX * TLAT**2,kind=4)
!!...  Reference ellipsoid radius
      Oldrref = SQRT(XX + YY)
      
      AB1 = SQRT(A * B)
      TLAT1 = TAN(LAT / RADEG)
!!...  XX, YY = squares of x, y components of local ellipsoid radius
      XX1 = (AB1 * C)**2 / (C**2 + (AB1 * TLAT1)**2)
      YY1 = XX1 * TLAT1**2
!!...  Reference ellipsoid radius
      Oldrref1 = SQRT(XX1 + YY1)
      
!!...  Get MOLA areoid (Rref)
      Call topoareo_2001(LAT,LONW,Rref,ctopohgt,calbedo)
!!...  Rz = total radius to current height z
      Rz = Rref + z
!!...  Acceleration of gravity including J2 and centrifugal terms
      P2 = 1.5_PM_REEL*(Sin(LAT/RADEG)**2) - 0.5_PM_REEL
      gz = (GM/Rz**2)*(1._PM_REEL - 3._PM_REEL*J2*((AB/Rz)**2)*P2)
      gz = gz - 1000._PM_REEL*Rz*(omega*Cos(LAT/RADEG))**2
      RETURN
    End SUBROUTINE cps_RELLIPS_2001
!!-----------------------------------------------------------------------
    Subroutine SublTchk_2001(temp,pres,Tsubl)
!!...  Assure temperatures greater than sublimation point for CO2.
!!     Coefficients from Kieffer et al. eds. "Mars" (Univ. Arizona
!!     Press book) 1992, page 959.
!!     Inputs are temp = temperature (K), pres = pressure (N/m**2)
      real(kind=PM_REEL) :: temp,pres,Tsubl
      Tsubl = 3182.48_PM_REEL/(23.3494_PM_REEL - Dlog(pres/100._PM_REEL))
      If (temp.lt.Tsubl) temp = Tsubl
      Return
    End Subroutine SublTchk_2001
!!-----------------------------------------------------------------------
    Subroutine surfterp_2001(khgt,time,TMGCM,PMGCM,DMGCM,UMGCM,        &
         VMGCM,Hpres,Hdens,ctopohgt,TempDay,PresDay,DensDay,UwndDay,   &
         VwndDay,Hpres0,Tempmax,Tempmin,Densmax,Densmin,Tat5m)
!!...    Interpolates Ames Mars General Circulation Model (MGCM) surface
!!       data to a given latitude, longitude, time of year (Ls), and
!!       dust optical depth, for a given height index (khgt) and time of
!!       day (time).
!!       Some input data is provided by the Common "Interp".
!!...    Set parameter values for number of heights, boundary layer
!!       levels, latitudes, longitudes, and number of dust optical depth
!!       values
        real(kind=PM_REEL) :: ctopohgt,Densmax,Densmin,PresDay,DensDay
        real(kind=PM_REEL) :: TMGCM,PMGCM,DMGCM,Hpres,Hdens
        integer :: khgt
        integer, Parameter :: nhgt = 17
        integer, Parameter :: nbl = 3
        integer, Parameter :: nlat = 25
        integer, Parameter :: nlon = 40
        integer, Parameter :: ndust = 3
        integer, Parameter :: nf10 = 2
!!...    MGCM surface data arrays

! common surfdata_2001
        real :: TSA0(nbl,nlat,0:nlon,0:12,ndust),          &
         TSA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         TSP2(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA0(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         USP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         USA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         USP2(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA0(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA1(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSP1(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSA2(nbl,nlat,0:nlon,0:12,ndust),                              &
         VSP2(nbl,nlat,0:nlon,0:12,ndust)
        Common /surfdata_2001/TSA0,          &
         TSA1,                              &
         TSP1,                              &
         TSA2,                              &
         TSP2,                              &
         USA0,                              &
         USA1,                              &
         USP1,                              &
         USA2,                              &
         USP2,                              &
         VSA0,                              &
         VSA1,                              &
         VSP1,                              &
         VSA2,                              &
         VSP2

!!...    MGCM 0-80 km data arrays
! common MGCMdata_2001
        real :: TZA0(nhgt,nlat,0:12,ndust),               &
         TZA1(nhgt,nlat,0:12,ndust),TZP1(nhgt,nlat,0:12,ndust),         &
         TZA2(nhgt,nlat,0:12,ndust),TZP2(nhgt,nlat,0:12,ndust),         &
         PZA0(nhgt,nlat,0:12,ndust),PZA1(nhgt,nlat,0:12,ndust),         &
         PZP1(nhgt,nlat,0:12,ndust),PZA2(nhgt,nlat,0:12,ndust),         &
         PZP2(nhgt,nlat,0:12,ndust),DZA0(nhgt,nlat,0:12,ndust),         &
         UZA0(nhgt,nlat,0:12,ndust),UZA1(nhgt,nlat,0:12,ndust),         &
         UZP1(nhgt,nlat,0:12,ndust),UZA2(nhgt,nlat,0:12,ndust),         &
         UZP2(nhgt,nlat,0:12,ndust),VZA0(nhgt,nlat,0:12,ndust),         &
         VZA1(nhgt,nlat,0:12,ndust),VZP1(nhgt,nlat,0:12,ndust),         &
         VZA2(nhgt,nlat,0:12,ndust),VZP2(nhgt,nlat,0:12,ndust)
        Common /MGCMdata_2001/TZA0,                &
         TZA1,TZP1,         &
         TZA2,TZP2,         &
         PZA0,PZA1,         &
         PZP1,PZA2,         &
         PZP2,DZA0,         &
         UZA0,UZA1,         &
         UZP1,UZA2,         &
         UZP2,VZA0,         &
         VZA1,VZP1,         &
         VZA2,VZP2


! Common Interp_2001
        integer :: ilat,jlon,ls,mdust, k1st,ilatw,ilatt,mf10
        real(kind=PM_REEL) :: dlat, dlon,dls,ddust,dlatw,dlatt,df10,    &
             wpolefac,tpolefac
        Common /Interp_2001/ilat,ilatw,ilatt,jlon,ls,mdust,k1st,mf10,dlat, &
               dlon,dlatw,dls,ddust,dlatt,df10,wpolefac,tpolefac

! Common MGCMparm_2001
        Character*2 :: dustc(ndust),solact (nf10)
        real(kind=PM_REEL) :: dust(ndust),dzbl(nbl),zwsfc,f10val(nf10)
        Common /MGCMparm_2001/dust,dzbl,zwsfc,f10val,dustc,solact
        Real(kind=PM_REEL) :: TM(2,2,2,2),UM(2,2,2,2),VM(2,2,2,2),TS0(2,2,2,2), &
         TZ1(2,2,2),PZ1(2,2,2),RZ0(2,2,2),PZh(2,2,2),PZh1(2,2,2),       &
         TZ0(2,2,2),PZ0(2,2,2),DZh(2,2,2),DZh1(2,2,2),Tday(2,2,2,2),    &
         Uday(2,2,2,2),Vday(2,2,2,2),Tmax(2,2,2,2),Tmin(2,2,2,2),       &
         Pmax(2,2,2),Pmin(2,2,2),T1max(2,2,2),T1min(2,2,2)

        real(kind=PM_REEL) :: PZk1h,PZk1h1,DZk1h,DZk1h1,Tzero,Pzero,Rzero,T1st
        real(kind=PM_REEL) :: P1st,P1max,UwndDay,VwndDay,Tempmin,Tempmax,TempDay
        real(kind=PM_REEL) :: TSzero,VMGCM,UMGCM
        real(kind=PM_REEL) :: P1min,Tmax1,Tmin1
        real(kind=PM_REEL) :: U0,V0,P0,T0,A1,P1,A2,P2,A1t,P1t,A2t,P2t,xtime,time
        integer :: k1h,itime,m,l,j,i

!!...    Establish MGCM surface values at corners of a 4-dimensional
!!       cube in latitude-longitude-Ls-dust space, at a given height
!!       index (khgt) and time of day (time)
        Do 103 i = 1,2
          polefac = 1.
          upolefac = 1.
          If (ilat.eq.1)Then
            polefac = i - 1.
          Else If (ilat.eq.nlat-1)Then
            polefac = 2. - i
          Endif
          If (ilatw.eq.2)Then
            If (i.eq.1)upolefac = real(wpolefac,kind=4)
          Else If (ilatw.eq.nlat-1)Then
            If (i.eq.2)upolefac = real(wpolefac,kind=4)
          Endif
        Do 102 j = 1,2
        Do 101 l = 1,2
        Do 100 m = 1,2
!!...      Daily mean temperature at level khgt
          T0 = real(TSA0(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Tday(i,j,l,m) = T0
!!...      Temperature tide amplitudes and phases
          A1t = real(TSA1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P1t = real(TSP1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2t = real(TSA2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P2t = real(TSP2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Temperature at corners of 4-D cube
          TM(i,j,l,m) = TideX_2001(T0,A1t,P1t,A2t,P2t,time)
!!...      Daily mean temperature at surface
          TS0(i,j,l,m) = real(TSA0(1,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Max and Min temperatures at corners of 4-D cube
          Tmax(i,j,l,m) = -9999.
          Tmin(i,j,l,m) = 9999.
          Do 50 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            Ttime = real(TideX_2001(T0,A1t,P1t,A2t,P2t,xtime),kind=4)
            If (Ttime.gt.Tmax(i,j,l,m))Tmax(i,j,l,m) = Ttime
            If (Ttime.lt.Tmin(i,j,l,m))Tmin(i,j,l,m) = Ttime
  50      Enddo
!!...      Daily mean EW wind at level khgt
          U0 = real(USA0(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Uday(i,j,l,m) = U0
!!...      EW wind tide coefficient amplitudes and phases
          A1 = real(USA1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P1 = real(USP1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(USA2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P2 = real(USP2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      EW wind at corners of 4-D cube
          UM(i,j,l,m) = TideX_2001(U0,A1,P1,A2,P2,time)
!!...      Daily mean NS wind at level khgt
          V0 = real(VSA0(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          Vday(i,j,l,m) = V0
!!...      NS wind coefficient amplitudes and phases
          A1 = real(VSA1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P1 = real(VSP1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(VSA2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)*upolefac
          P2 = real(VSP2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      NS wind at corners of 4-D cube
          VM(i,j,l,m) = TideX_2001(V0,A1,P1,A2,P2,time)
 100    Enddo
 101    Enddo
 102    Enddo
 103    Enddo
!!...    Use 4-D interpolation to get temperature, EW wind, NS wind,
!!       and daily mean surface temperature at given latitude,
!!       longitude, Ls, and dust optical depth
        Call FourD_2001(dlat,dlon,dls,ddust,TM,TMGCM)
        Call FourD_2001(dlatw,dlon,dls,ddust,UM,UMGCM)
        Call FourD_2001(dlatw,dlon,dls,ddust,VM,VMGCM)
        Call FourD_2001(dlat,dlon,dls,ddust,TS0,TSzero)
        Call FourD_2001(dlat,dlon,dls,ddust,Tday,TempDay)
        Call FourD_2001(dlat,dlon,dls,ddust,Tmax,Tempmax)
        Call FourD_2001(dlat,dlon,dls,ddust,Tmin,Tempmin)
        Call FourD_2001(dlatw,dlon,dls,ddust,Uday,UwndDay)
        Call FourD_2001(dlatw,dlon,dls,ddust,Vday,VwndDay)
!!...    k1h = height index just below k1st
        k1h = k1st - 1
        If (k1h.lt.1)k1h = 1
!!...    Establish MGCM values at height levels k1h, k1st and corners of
!!       a 3-dimensional cube in latitude-Ls-dust space, at given time
!!       of day (time)
        Do 202 i = 1,2
          polefac = 1.
          If (ilat.eq.1)Then
            polefac = i - 1.
          Else If (ilat.eq.nlat-1)Then
            polefac = 2. - i
          Endif
        Do 201 l = 1,2
        Do 200 m = 1,2
!!...      Daily average pressure and density at level k1h
          PZh(i,l,m) = real(PZA0(k1h,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          DZh(i,l,m) = real(DZA0(k1h,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          PZh1(i,l,m) = real(PZA0(k1h+1,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          DZh1(i,l,m) = real(DZA0(k1h+1,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Pressure tide coefficient amplitudes and phases
          P0 = real(PZA0(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A1 = real(PZA1(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P1 = real(PZP1(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(PZA2(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P2 = real(PZP2(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      Pressure values at corners of 3-D cube
          PZ1(i,l,m) = TideY_2001(P0,A1,P1,A2,P2,time)
!!...      Daily average pressure at level k1st
          PZ0(i,l,m) = P0
!!...      Level k1st Pressure at corners of 3-D cube
          Pmax(i,l,m) = -9999.
          Pmin(i,l,m) = 9999.
          Do 150 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            Ptime = real(TideY_2001(P0,A1,P1,A2,P2,xtime),kind=4)
            If (Ptime.gt.Pmax(i,l,m)) Pmax(i,l,m) = Ptime
            If (Ptime.lt.Pmin(i,l,m)) Pmin(i,l,m) = Ptime
  150     Enddo
!!...      Temperature tide coefficient amplitudes and phases
          T0 = real(TZA0(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A1 = real(TZA1(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P1 = real(TZP1(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
          A2 = real(TZA2(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)*polefac
          P2 = real(TZP2(k1st,ilat+i-1,ls+l-1,mdust+m-1),kind=PM_REEL)
!!...      temperature values at corners of 3-D cube
          TZ1(i,l,m) = TideX_2001(T0,A1,P1,A2,P2,time)
!!...      Level k1st Temperature at corners of 3-D cube
          T1max(i,l,m) = -9999.
          T1min(i,l,m) = 9999.
          Do 160 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            T1time = real(TideX_2001(T0,A1,P1,A2,P2,xtime),kind=4)
            If (T1time.gt.T1max(i,l,m)) T1max(i,l,m) = T1time
            If (T1time.lt.T1min(i,l,m)) T1min(i,l,m) = T1time
  160     Enddo
!!...      Daily average temperature at level k1st
          TZ0(i,l,m) = T0
!!...      Daily average density at level k1st
          D0 = DZA0(k1st,ilat+i-1,ls+l-1,mdust+m-1)
!!...      Gas constant from pressure, density, and temperature
          RZ0(i,l,m) = 190.
          If(T0.ne.0.0.and.D0.ne.0.0)RZ0(i,l,m) = P0/(T0*D0)
 200    Enddo
 201    Enddo
 202    Enddo
!!...    Do 3-D interpolation on pressure
        Call ThreeD_2001(dlat,dls,ddust,PZh,PZk1h)
        Call ThreeD_2001(dlat,dls,ddust,PZh1,PZk1h1)
!!...    Daily average pressure scale height
        Hpres0 = real(5./Zlogr_2001(PZk1h,PZk1h1),kind=4)
!!...    Do 3-D interpolation on density
        Call ThreeD_2001(dlat,dls,ddust,DZh,DZk1h)
        Call ThreeD_2001(dlat,dls,ddust,DZh1,DZk1h1)
!!...    Daily average density scale height
        Hdens0 = real(5./Zlogr_2001(DZk1h,DZk1h1),kind=4)
!!...    Do 3-D interpolation on daily mean temperature
        Call ThreeD_2001(dlat,dls,ddust,TZ0,Tzero)
        Call ThreeD_2001(dlat,dls,ddust,PZ0,Pzero)
!!...    Daily average layer mean temperature
        Tbar0 = real((Tzero + TSzero)/2.,kind=4)
!!...    Do 3-D interpolation on gas constant
        Call ThreeD_2001(dlat,dls,ddust,RZ0,Rzero)
!!...    Do 3-D interpolation on temperature and pressure
        Call ThreeD_2001(dlat,dls,ddust,TZ1,T1st)
        Call ThreeD_2001(dlat,dls,ddust,PZ1,P1st)
!!...    Do 3-D interpolation on max,min pressure at level k1st
        Call ThreeD_2001(dlat,dls,ddust,Pmax,P1max)
        Call ThreeD_2001(dlat,dls,ddust,Pmin,P1min)
!!...    Do 3-D interpolation on max,min temperature at level k1st
        Call ThreeD_2001(dlat,dls,ddust,T1max,Tmax1)
        Call ThreeD_2001(dlat,dls,ddust,T1min,Tmin1)
!!...    Density from gas law
        D1st = real(P1st/(Rzero*T1st),kind=4)
!!...    Layer mean temperature at current time
        If (khgt.eq.2.and.Tat5m  .le.0.0)Tat5m = real(TMGCM,kind=4)
        Tbar = real((T1st + Tat5m  )/2.,kind=4)
!!...    Pressure scale height and density scale height at current time
        Hpres = Hpres0*Tbar/Tbar0
        Hdens = Hdens0*Tbar/Tbar0
!!...    Adjust pressure to height level, using pressure scale height
        height = real(ctopohgt + dzbl(khgt),kind=4)
        Z1st = 5.*(k1st - 1.)
        PMGCM = P1st*Exp((Z1st - height)/Hpres)
        PresDay = Pzero*Exp((Z1st - height)/Hpres0)
!!...    Compute density from gas law, using pressure and temperature
        DMGCM = PMGCM/(Rzero*TMGCM)
        DensDay = PresDay/(Rzero*TempDay)
!!...    Daily maximum and minimum density
        Hpmin = real(Hpres0*0.5*(Tmax1+Tempmax)/Tbar0,kind=4)
        Hpmax = real(Hpres0*0.5*(Tmax1+Tempmin)/Tbar0,kind=4)
        P1max = P1max*Exp((Z1st-height)/Hpmax)
        P1min = P1min*Exp((Z1st-height)/Hpmin)
        Densmax = DensDay*(P1max/PresDay)*(TempDay/Tempmin)
        Densmin = DensDay*(P1min/PresDay)*(Tempday/Tempmax)
        Return
      End Subroutine surfterp_2001
!!-----------------------------------------------------------------------
      SUBROUTINE STEWART2_2001 ( RAUI, LAT, LON, LST, TOTALPRZ, TZ,      &
      TOTALMDZ, CHGT, RSTAR, H, MOLWTG, SIGMA,                           &
      sunlat,deltaTEX,TINF,TF,ZF,Hrho)
!!
!!...  TIME-DEPENDENT MARS ATMOSPHERE MODEL, FORTRAN VERSION OF PROGRAM
!!     BY IAN STEWART, LABORATORY FOR ATMOSPHERIC AND SPACE PHYSICS,
!!     UNIV. OF COLORADO. FINAL REPORT JPL PO # NQ-802429
!!....................................................................
      REAL(kind=PM_REEL) :: deltaTEX
      DIMENSION ES(0:11), PRZ(0:11), NDZ(0:11), MDZ(0:11)
      REAL(kind=PM_REEL) ::  RAUI, LAT, LST, ZZF, TOTALPRZ, TOTALNDZ, TZ, LON, &
      MOLWTG, NDZ, MDZ , TOTALMDZ, Hrho, H
      REAL(kind=PM_REEL) :: scale,TINF, TF, ZF,RF,RAU,SUNLAT,TINF0,TF0,ZF0
      REAL(kind=PM_REEL) :: RREF, CHGT, GZ , Oldrref, ctopohgt,FBARR,albedo
!!.... ES ARE STD DEVIATIONS FROM NOMINAL VALUES
!!.... 0,2,....10  LONG - TERM
!!.... 1,3.....,11  SHORT-TERM
!!.... FOR   FBAR,TINF,FOXY,AOXY,ZF,DZDUST
!!.... RETURNS TEMP,#DENS,MDENS,PRESSURE
      REAL :: FBAR,SIGMA

      ! Common THERM_2001
      real(kind=PM_REEL) :: F107,stdl
      COMMON /THERM_2001/F107,stdl

      Call EScalc_2001(stdl,SIGMA,ES)
!!.... DEVIATIONS FROM NOMINAL VALUES
      FBAR = real(F107 * EXP(ES(0)),kind=4)
!!.... 3 MONTH RUNNING MEAN OF 10.7 CM SOLAR FLUX
!!.... IN UNITS OF 1.0E-22 W/CM**2
      RAU = RAUI
!!...  Convert solar 10.7 cm flux to Mars position
      FBARR = FBAR / (RAU**2)
      CALL cps_RELLIPS_2001( LAT, LON, RREF, CHGT, GZ , Oldrref, ctopohgt,   &
       albedo)
!!...  Evaluate the basic parameters for the thermosphere model
      Call Thermpar_2001(RAU,FBARR,LAT,LST,SUNLAT,TINF0,TF0,ZF0,SCALE)
!!...  Height above base of thermosphere
      ZZF = CHGT - ZF
      RF = RREF + ZF
!!...  Exospheric temperature
      TINF = TINF0 * EXP(ES(2) + ES(3)) + deltaTEX

      CALL THERMOS_2001(ES, TINF, TF, LAT, LON,  LST, ZF,RF, ZZF,  &
      TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ,     &
      SCALE,tgrad, dMdz)
!!...  SCALE HEIGHT, km
      Rog = real(RSTAR/(1000.*MOLWTG*GZ),kind=4)
      H = Rog*TZ
      Hrho = H/(1. + tgrad*Rog - (H/MOLWTG)*dMdz)
!!...  Convert pressure to N/m**2
      TOTALPRZ = TOTALPRZ*1.0E5_PM_REEL
!!...  Convert density to kg/m**3
      TOTALMDZ = TOTALMDZ*1000._PM_REEL
      RETURN
    End SUBROUTINE STEWART2_2001
!!-----------------------------------------------------------------------
    Subroutine ThreeD_2001(dx,dy,dz,Array,Value)
!!...    3-Dimensional linear interpolation within a 1x1x1 cube (x,y,z)
!!       of Array(2,2,2) to position dx,dy,dz (all 0-1).
!!       Value is value of interpolated output.
        Real(kind=PM_REEL) :: Array(2,2,2)
        real(kind=PM_REEL) :: dx,dy,dz
        real(kind=PM_REEL) :: Value
! variables locales
        real(kind=PM_REEL) :: dxp,dyp,dzp
!!...    Complementary displacements in x,y,z
        dxp = 1._PM_REEL - dx
        dyp = 1._PM_REEL - dy
        dzp = 1._PM_REEL - dz
!!...    3-D interpolated Value from Array
        Value = dxp*dyp*dzp*Array(1,1,1) + dxp*dyp*dz*Array(1,1,2)       &
         + dxp*dy*dzp*Array(1,2,1) + dx*dyp*dzp*Array(2,1,1)            &
         + dxp*dy*dz*Array(1,2,2) + dx*dyp*dz*Array(2,1,2)              &
         + dx*dy*dzp*Array(2,2,1) + dx*dy*dz*Array(2,2,2)
        Return
      End Subroutine ThreeD_2001
!!-----------------------------------------------------------------------
      SUBROUTINE THERMOS_2001 (ES, TINF, TF, LAT, LON, LST, ZF,RF,      &
      ZZF, TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ,     &
      SCALE,tgrad,dMdz)
      DIMENSION ES(0:11),PRZ(0:11),NDZ(0:11),MDZ(0:11)
      REAL(kind=PM_REEL) :: TINF,TF,LAT,LST,ZF,RF,ZZF,TOTALPRZ,TOTALNDZ,&
           MOLWTG,NDZ,MDZ, LON,TZ,Tsubl,RREF,TOTALMDZ
      REAL(kind=PM_REEL) :: GF,ctopohgt,albedo,Oldrref

!      REAL(kind=PM_REEL) LAT,LON,RREF,ZF,GF,Oldrref,ctopohgt,albedo
!!.... RETURNS TEMP & COMPOSITION VS ALTITUDE ABOVE ZF
!!.... ES (EPS*SIG)   DEVIATIONS FROM NOMINAL VALUES
!!.... 0,2,....,10 LONG-TERM
!!.... 1,3,....11   SHORT-TERM
!!.... FOR   FBAR, TINF, FOXY, ADXY,ZF,DZDUST
!!.... ZZF  = INPUT ALTITUDES ABOVE ZF
!!.... TZ  = TEMPERATURE VS ALTITUDE, DEG K
!!.... NDZ= # DENSITY VS ALTITUDE, #/cc
!!.... MDZ= MASS DENSITY VS ALTITUDE, gm/cc
!!.... PRZ= PRESSURE VS ALTITUDE, BARS
      REAL(kind=PM_REEL) :: BK, PRESSF, P1BAR, AO, FO, PFHE, PFH2
      REAL(kind=PM_REEL) :: PFH, RATIO, SCALE, YSC
      real(kind=PM_REEL) :: HH, XHH, M,RADEG,DM,XDM, FF, XFF
      DIMENSION HH(0:11), XHH(0:11), DM(0:11), XDM(0:11), FF(0:11),      &
      XFF(0:11), M(0:11)
      INTEGER :: I, J
      RADEG = 57.29577958_PM_REEL
!!     RADEG = DEGREES/RADIAN
      BK = 1.3804E-16_PM_REEL
!!.... CONSTANT FOR NUMBER DENSITY CALCULATIONS

      CALL cps_RELLIPS_2001(LAT,LON,RREF,ZF,GF,Oldrref,ctopohgt,albedo)
      GF = 100._PM_REEL*GF
!!.... ACC. OF GRAVITY AT ZF
!!.... PRESSF = 1.24E-9   (as originally in Stewart's model)
      PRESSF = 1.26E-9_PM_REEL
!!.... 1.26E-3 dynes/cm**2 = 1.26 nbar AT ZF, BASE OF THERMOSPHERE
!!.... P1BAR = 1.013E6   (as originally in Stewart's model)
      P1BAR = 1.0E6_PM_REEL
!!.... EARTH SURFACE PRESSURE
      AO = 0.18_PM_REEL * (1.0_PM_REEL + ES(7))
!!.... AO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT
      FO = 0.01_PM_REEL * EXP(ES(4) + ES(5))
!!.... FO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT I
      M(0) = 44.011_PM_REEL
!!.... CO2 MOLECULAR WEIGHT
      M(1) = 28.016_PM_REEL
!!.... N2
      M(2) = 39.944_PM_REEL
!!.... ARGON
      M(3) = 32.00_PM_REEL
!!.... MOLECULAR OXYGEN
      M(4) = 28.011_PM_REEL
!!.... CARBON MONOXIDE
      M(5) = 16.00_PM_REEL
!!.... ATOMIC OXYGEN
      M(6) = 4.003_PM_REEL
!!.... HELIUM
      M(7) = 2.016_PM_REEL
!!.... MOLECULAR HYDROGEN
      M(8) = 1.008_PM_REEL
!!.... ATOMIC HYDROGEN MOLECULAR WEIGHT
      DM(0) = 7.31E-23_PM_REEL
!!.... CO2 MOLECULAR MASS
      DM(1) = 4.65E-23_PM_REEL
!!.... N2 MOLECULAR MASS
      DM(2) = 6.63E-23_PM_REEL
!!.... ARGON MOLECULAR MASS
      DM(3) = 5.31E-23_PM_REEL
!!.... 02 MOLECULAR MASS
      DM(4) = 4.65E-23_PM_REEL
!!.... CARBON MONOXIDE MOLECULAR MASS
      DM(5) = 2.66E-23_PM_REEL
!!.... ATOMIC OXYGEN MOLECULAR MASS
      XDM(0) = 6.65E-24_PM_REEL
!!.... HELIUM MOLECULAR MASS
      XDM(1) = 3.32E-24_PM_REEL
!!.... H2 MOLECULAR MASS
      XDM(2) = 1.66E-24_PM_REEL
!!.... H   MOLECULAR MASS
!!.... THE FOLLOWING IS THE COMPOSITION OF THE HETEROSPHERE
      FF(0) = 0.932_PM_REEL
!!.... CO2
      FF(1) = 0.027_PM_REEL
!!.... NITROGEN
      FF(2) = 0.016_PM_REEL
!!.... ARGON
      FF(3) = 0.002_PM_REEL
!!.... MOLECULAR OXYGEN
      FF(4) = 0.013_PM_REEL
!!.... CARBON MONOXIDE
      FF(5) = 0.010_PM_REEL
!!.... ATOMIC OXYGEN
      FF(5) = FO*(1.0-AO*SIN(15.0*LST/RADEG)*COS(LAT/RADEG))
      PFHE = 3.3E-16_PM_REEL * TINF
!!.... EXOBASE (ZF) HELIUM PARTIAL PRESSURE
      PFH2 = 2.4E-15_PM_REEL
!!.... EXOBASE (ZF) H2 PARTIAL PRESSURE
      if (TINF .LE. 330.0_PM_REEL) then
        PFH=5.2E-16_PM_REEL*TINF*EXP(-TINF/70.0_PM_REEL)
!!.... EXOBASE (ZF) H PARTIAL PRESSURE
      else
        RATIO = 1440.0_PM_REEL / TINF
        PFH=5.8E-18_PM_REEL*SQRT(TINF)*EXP(RATIO)/(1.0_PM_REEL+RATIO)
!!....   EXOBASE (ZF) H PARTIAL PRESSURE
      ENDIF
      XFF(0) = PFHE / PRESSF
      XFF(1) = PFH2 / PRESSF
      XFF(2) = PFH / PRESSF
      MOLWTG = 0.0_PM_REEL
      TOTALPRZ = 0.0_PM_REEL
      TOTALMDZ = 0.0_PM_REEL
      YSC = ZZF * RF / (RF + ZZF)
      EXPS = real(EXP(-YSC/SCALE),kind=4)
      TZ = TINF - (TINF - TF) * EXPS
!!.... Parameters for gradient of molecular weight
      smolwtg = 0.
      stotpr = 0.
      sysc = real((ZZF + 1._PM_REEL) * RF / (RF + ZZF + 1._PM_REEL),kind=4)
      stz = real(TINF - (TINF - TF) * EXP(-sysc/SCALE),kind=4)
!!.... Temperature gradient, K/km
      tgrad = real((TINF - TF)*(RF + YSC)/(SCALE*(RF + ZZF))*EXPS,kind=4)
      DO 200 I = 0, 5
        HH(I) = BK * TINF / (GF * DM(I)) / 1.0E5
!!....   SCALE HEIGHT
        PRZ(I) = real(PRESSF*FF(I)*EXP(-YSC/HH(I)- &
                      (SCALE/HH(I))*DLOG(TZ/TF)),kind=4)
        NDZ(I) = P1BAR * PRZ(I) / (BK * TZ)
!!....   NUMBER DENSITY HEAVY GASES
        MDZ(I) = NDZ(I) * DM(I)
        TOTALMDZ = TOTALMDZ + MDZ(I)
        TOTALPRZ = TOTALPRZ + PRZ(I)
        MOLWTG = MOLWTG + PRZ(I) * M(I)
!!....   Molecular weight 1 km higher
        sprzi=real(PRESSF*FF(I)*EXP(-sysc/HH(I)- &
                   (SCALE/HH(I))*DLOG(stz/TF)),kind=4)
        stotpr = stotpr + sprzi
        smolwtg = real(smolwtg + sprzi * M(I),kind=4)
200  enddo
      DO 210 J = 0, 2
        XHH(J) = BK * TINF / (GF * XDM(J)) / 1.0E5_PM_REEL
!!....   SCALE HEIGHT
        PRZ(J+6)=real(PRESSF*XFF(J)*EXP(-YSC/XHH(J)-(SCALE/XHH(J))*      &
                      DLOG(TZ/TF)),kind=4)
        NDZ(J+6) = P1BAR * PRZ(J + 6) / (BK * TZ)
!!....   NUMBER DENSITY LIGHT GASES
        MDZ(J + 6) = NDZ(J + 6) * XDM(J)
        TOTALMDZ = TOTALMDZ + MDZ(J + 6)
        TOTALPRZ = TOTALPRZ + PRZ(J + 6)
        MOLWTG = MOLWTG + PRZ(J + 6) * M(J + 6)
!!....   Molecular weight 1 km higher
        sprzj = real(PRESSF*XFF(J)*EXP(-sysc/XHH(J)-(SCALE/XHH(J))*      &
                     DLOG(stz/TF)),kind=4)
        stotpr = stotpr + sprzj
        smolwtg = real(smolwtg + sprzj * M(J + 6),kind=4)
210  enddo
!!...  Check that temperature >= sublimation temperature
      Call SublTchk_2001(TZ,TOTALPRZ,Tsubl)
      MOLWTG = MOLWTG / TOTALPRZ
!!...  Change in molecular weight over 1 km
      dMdz = real(smolwtg/stotpr - MOLWTG,kind=4)
      TOTALNDZ = P1BAR * TOTALPRZ / (BK * TZ)
      RETURN
     end SUBROUTINE THERMOS_2001
!!-----------------------------------------------------------------------
     real(kind=PM_REEL) Function TideX_2001(A0,A1,phi1,A2,phi2,t)
!!...    Tide value at local solar time t, from mean value A0, amplitude
!!       A1 and phase phi1 of 24-hour period component, and amplitude A2
!!       and phase phi2 of 12-hour period component.  Amplitudes A1 and
!!       A2 are in same physical units as mean term A0.  Phases are in
!!       hours of local solar time.
       real(kind=PM_REEL) :: A0,A1,phi1,A2,phi2,t, pi
       pi = 4._PM_REEL*Atan(1._PM_REEL)
       TideX_2001 = A0+A1*Cos(pi*(t-phi1)/12._PM_REEL)+&
            A2*Cos(pi*(t-phi2)/6._PM_REEL)
       Return
     End Function TideX_2001
!!-----------------------------------------------------------------------
     Real(kind=PM_REEL) Function TideY_2001(A0,A1,phi1,A2,phi2,t)
!!...    Tide value at local solar time t, from mean value A0, amplitude
!!       A1 and phase phi1 of 24-hour period component, and amplitude A2
!!       and phase phi2 of 12-hour period component.  Amplitudes A1 and
!!       A2 are in relative units (% of mean term A0).  Phases are in
!!       hours of local solar time.
       real(kind=PM_REEL) :: A0,A1,phi1,A2,phi2,t, pi
       pi = 4._PM_REEL*Atan(1._PM_REEL)
       TideY_2001 = A0*(1._PM_REEL + (A1*Cos(pi*(t-phi1)/12._PM_REEL) + &
            A2*Cos(pi*(t-phi2)/6._PM_REEL))/100._PM_REEL)
       Return
     End Function TideY_2001
!!-----------------------------------------------------------------------
   Subroutine topoareo_2001(clat,clonw,careoid,ctopohgt,calbedo)
     real(kind=PM_REEL) :: clat,clonw,careoid,ctopohgt,calbedo
        integer, parameter :: nmtlat = 361
        integer, parameter :: nmtlon = 721
        integer, parameter :: nalblat = 181
        integer, parameter :: nalblon = 361

! common TERHGT_2001
        real :: areorad(0:nmtlat,0:nmtlon), &
             topomola(0:nmtlat,0:nmtlon),albedo(0:nalblat,0:nalblon)
        Common /TERHGT_2001/areorad,                  &
         topomola,albedo
        Real (kind=PM_REEL), dimension(2,2)  :: topohgt,areoid,albint
        real (kind=PM_REEL) :: stepmolalon, stepmolalat,stepalblat,stepalblon
        real (kind=PM_REEL) :: dlatalb,dlonalb,dlatmt,dlonmt,stepmola
        integer :: jlonalb,ilatalb,jlonmt,ilatmt


!!...    Latitude, Longitude steps sizes for MOLA data
        stepmolalat = 180._PM_REEL/(real(nmtlat,kind=PM_REEL)-1._PM_REEL)
        stepmolalon = 360._PM_REEL/(real(nmtlon,kind=PM_REEL)-1._PM_REEL)
!!...    Compute MOLA latitude index and lat increment from gridpoint
        stepmola = stepmolalat
        If (clat.lt.-90._PM_REEL+stepmolalat/2._PM_REEL)Then
          stepmola = stepmolalat/2._PM_REEL
          ilatmt = 0
          dlatmt = (clat + 90._PM_REEL)/stepmola
        Else If (clat.gt.90.-stepmolalat/2._PM_REEL)Then
          stepmola = stepmolalat/2._PM_REEL
          ilatmt = nmtlat - 1
          dlatmt = (clat - 90._PM_REEL + stepmola)/stepmola
        Else
          ilatmt = Ifloor_2001((clat+90._PM_REEL+stepmolalat/2._PM_REEL)/stepmolalat)
          dlatmt = (clat - stepmolalat*(ilatmt-0.5_PM_REEL) + 90._PM_REEL)/stepmolalat
        Endif
        If (ilatmt.gt.nmtlat-1)ilatmt = nmtlat-1
        If (ilatmt.lt.0)ilatmt = 0
!!...    Compute MOLA longitude index and lon increment from gridpoint
        jlonmt = Ifloor_2001((clonw+stepmolalon/2._PM_REEL)/stepmolalon)
        If (jlonmt.gt.nmtlon-1)jlonmt = nmtlon-1
        If (jlonmt.lt.0)jlonmt = 0
        dlonmt = (clonw+stepmolalon/2._PM_REEL-stepmolalon*real(jlonmt,kind=pm_reel))/stepmolalon
        If (dlatmt.lt.-0.00001.or.dlatmt.gt.1.00001)Stop ' Bad dlatmt'
        If (dlonmt.lt.-0.00001.or.dlonmt.gt.1.00001)Stop ' Bad dlonmt'
!!...    Topographic heights at corners of 2-D square grid points
        topohgt(1,1) = real(topomola(ilatmt,jlonmt),kind=PM_REEL)
        topohgt(1,2) = real(topomola(ilatmt,jlonmt+1),kind=PM_REEL)
        topohgt(2,1) = real(topomola(ilatmt+1,jlonmt),kind=PM_REEL)
        topohgt(2,2) = real(topomola(ilatmt+1,jlonmt+1),kind=PM_REEL)
!!...    Areoid radius at corners of 2-D square grid
        areoid(1,1) = real(areorad(ilatmt,jlonmt),kind=PM_REEL)
        areoid(1,2) = real(areorad(ilatmt,jlonmt+1),kind=PM_REEL)
        areoid(2,1) = real(areorad(ilatmt+1,jlonmt),kind=PM_REEL)
        areoid(2,2) = real(areorad(ilatmt+1,jlonmt+1),kind=PM_REEL)
!!...    Use 2-D interpolation to get topographic height at current
!!       position
        Call TwoD_2001(dlatmt,dlonmt,topohgt,ctopohgt)
!!...    Use 2-D interpolation to get areoid radius at current position
        Call TwoD_2001(dlatmt,dlonmt,areoid,careoid)
        areoid = real(real(areoid,kind=PM_REEL),kind=4)
!!...    Latitude, Longitude steps sizes for albedo data
        stepalblat = 180._PM_REEL/(nalblat-1._PM_REEL)
        stepalblon = 360._PM_REEL/(nalblon-1._PM_REEL)
!!...    Compute albedo latitude index and lat increment from gridpoint
        stepalb = real(stepalblat,kind=4)
        If (clat.lt.-90._PM_REEL+stepalblat/2._PM_REEL)Then
          stepalb = real(stepalblat/2._PM_REEL,kind=4)
          ilatalb = 0
          dlatalb = (clat + 90._PM_REEL)/stepalb
        Else If (clat.gt.90._PM_REEL-stepalblat/2._PM_REEL)Then
          stepalb = real(stepalblat/2._PM_REEL,kind=4)
          ilatalb = nalblat - 1
          dlatalb = (clat - 90._PM_REEL + stepalb)/stepalb
        Else
          ilatalb = Ifloor_2001((clat+90._PM_REEL+stepalblat/2._PM_REEL)/stepalblat)
          dlatalb = (clat - stepalblat*(ilatalb-0.5_PM_REEL) + 90._PM_REEL)/stepalblat
        Endif
        If (ilatalb.gt.nalblat-1) ilatalb = nalblat-1
        If (ilatalb.lt.0) ilatalb = 0
!!...    Compute albedo longitude index and lon increment from gridpoint
        jlonalb = Ifloor_2001((clonw+stepalblon/2._PM_REEL)/stepalblon)
        If (jlonalb.gt.nalblon-1)jlonalb = nalblon-1
        If (jlonalb.lt.0)jlonalb = 0
        dlonalb = (clonw+stepalblon/2._PM_REEL-stepalblon*jlonalb)/stepalblon
        If(dlatalb.lt.-.00001_PM_REEL.or.dlatalb.gt.1.00001_PM_REEL) Stop ' Bad dlatalb'
        If(dlonalb.lt.-.00001_PM_REEL.or.dlonalb.gt.1.00001_PM_REEL) Stop ' Bad dlonalb'
!!...    Albedo at corners of 2-D square grid points
        albint(1,1) = real(albedo(ilatalb,jlonalb),kind=PM_REEL)
        albint(1,2) = real(albedo(ilatalb,jlonalb+1),kind=PM_REEL)
        albint(2,1) = real(albedo(ilatalb+1,jlonalb),kind=PM_REEL)
        albint(2,2) = real(albedo(ilatalb+1,jlonalb+1),kind=PM_REEL)
!!...    Use 2-D interpolation to get albedo at current position
        Call TwoD_2001(dlatalb,dlonalb,albint,calbedo)
        Return
      End Subroutine topoareo_2001
!!-----------------------------------------------------------------------
      Subroutine Thermpar_2001(RAU,FBARR,lat,LST,sunlat,TINF0,TF0,ZF0,   &
       SCALE)
      REAL(kind=PM_REEL) :: RAU,FBARR,lat,LST,LATMAX, sunlat,TINF0,TF0,ZF0, SCALE
      Data LATMAX/25.4_PM_REEL/
!!
!!....................................................................
!!
!!...  Thermospheric parameters, revised from the original Stewart
!!     parameterizations:
!!     SMA = 1.523691
!!     ZF0 = 124.4 * (SMA/RAU)
!!     TINF0 = 4.11 * (11.0 + FBARR)
!!     TF0 = 170.0 * (SMA/RAU)
!!     SCALE = TF0 / 9.20
!!
!!     The new parameterizations are based on four data sets from the
!!     University of Arizona Mars Thermospheric General Circulation
!!     Model (MTGCM), cases MGS97L, MGS98L, MANC00, and MGS97E. For
!!     a description of the MTGCM model and its output, see Bougher,
!!     et al., Journal of Geophysical Research, vol. 95 (B9), pp.
!!     14,811 - 14,827, August 30, 1990.
!!
!!...................................................................
!!
!!     Inputs:
!!       RAU    = orbital position radius (AU)
!!       FBARR  = 10.7 cm solar flux at Mars position
!!       lat    = latitude for evaluation of parameters (degrees)
!!       LST    = local solar time (Mars hours) at evaluation point
!!       sunlat = latitude of sun (degrees)
!!     Outputs:
!!       TINF0  = Exospheric temperature (K)
!!       TF0    = Temperature at base of thermosphere (K)
!!       ZF0    = Height of base of thermosphere (km)
!!       SCALE  = Scale height for temperature variations (km)
!!
!!     Output values are un-corrected for Stewart (ES array) variations,
!!     pressure and dust effects.  These factors are accounted for in
!!     the Stewart2 subroutine.  Adjustment factor deltaTEX is added
!!     after computation of exospheric temperature.
!!....................................................................
!!
!!...  Degrees to radians conversion factor
      pi180 = Atan(1.)/45.
!!...  Global mean exospheric temperature (K) versus 10.7 cm flux
      Tbar = real(156.3 + 0.9427*FBARR,kind=4)
!!...  Zonal average exospheric temperature (K) versus latitude
      Tavg = real(Tbar*(1. + 1.369E-4*sunlat*lat),kind=4)
!!...  Phase angles (hours) for local solar time variation
      t1 = real(13.2 - 0.00119*sunlat*lat,kind=4)
      t2 = real(9.4  - 0.00231*sunlat*lat,kind=4)
!!...  Amplitude factor for local solar time variation
      poleamp = 1.
      If (Abs(lat).gt.85._PM_REEL) &
           poleamp = real(1._PM_REEL -(Abs(lat)-85._PM_REEL)/5._PM_REEL,kind=4)
      cphi = real(Cos(pi180*(lat + sunlat)/(1. + LATMAX/90.))*poleamp,kind=4)
!!...  Exospheric temperature (K) versus local solar time
      TINF0 = Tavg*(1. + 0.22*cphi*Cos(pi180*15.0*(LST-t1)) +            &
       0.04*cphi*Cos(pi180*30.0*(LST-t2)))
!!...  Global mean height of thermosphere base (km)
      Zbar = real(197.94 - 49.058*RAU,kind=4)
!!...  Latitude variation factor
      factlat = real((sunlat/LATMAX)*(lat/77.5)**3,kind=4)
!!...  Zonal average base height (km) versus latitude
      Zavg = Zbar + 4.3*factlat
!!...  Amplitudes for local solar time variation
      A1 = real((1.5 - Cos(pi180*4.0*lat))*poleamp,kind=4)
      A2 = real((2.3*(Cos(pi180*(lat + 0.5*sunlat)))**3)*poleamp,kind=4)
!!...  Phase angles (hours) for local solar time variation
      t1 = real(16.2 - (sunlat/LATMAX)*Atan(pi180*10.0*lat),kind=4)
      t2 = 11.5
!!...  Base height of thermosphere (km) versus local solar time
      ZF0 = Zavg + A1*Cos(pi180*15.0*(LST-t1)) +                         &
      A2*Cos(pi180*30.0*(LST-t2))
!!...  Global mean temperature (K) at thermosphere base, versus FBARR
      Tbar = real(113.7 + 0.5791*FBARR,kind=4)
!!...  Zonal average temperature at thermosphere base (K) vs. latitude
      Tavg = Tbar*(1. + 0.186*factlat)
!!...  Amplitudes for local solar time variation
      A1 = real((0.06 - 0.05*Cos(pi180*4.0*lat))*poleamp,kind=4)
      A2 = real((0.1*(Cos(pi180*(lat + 0.5*sunlat)))**3)*poleamp,kind=4)
!!...  Phase angles (hours) for local solar time variation
      t1 = real(17.5 - 2.5*(sunlat/LATMAX)*Atan(pi180*10.0*lat),kind=4)
      t2 = real(10.0 + 2.0*(lat/77.5)**2,kind=4)
!!...  Thermosphere base temperature (K) versus local solar time
      TF0 = Tavg*(1.0 + A1*Cos(pi180*15.0*(LST-t1)) +                    &
      A2*Cos(pi180*30.0*(LST-t2)))
!!...  Global mean scale height (km) of thermospheric temperature
      SCALE = 8.38_PM_REEL + 0.09725_PM_REEL*FBARR
!!...  Zonal average temperature scale height (km) vs. latitude
      SCALE = SCALE*(1.14_PM_REEL - 0.18_PM_REEL*Cos(real(pi180,kind=PM_REEL)*lat))
      Return
    End Subroutine Thermpar_2001
!!-----------------------------------------------------------------------
    Subroutine TGCMterp_2001(khgtt,time,TTGCM,PTGCM,DTGCM,UTGCM,     &
         VTGCM,ZF,TempDay,PresDay,DensDay,UwndDay,VwndDay,Tempmax,      &
         Tempmin,Densmax,Densmin)
!!...    Interpolates University of Arizona Mars Thermospheric General
!!       Circulation Model (MTGCM) data to a given latitude, time of
!!       year (Ls), and dust optical depth, for a given height index
!!       (khgtt) and time of day (time).
!!       Some input data is provided by the Common "Interp".
!!...    Set parameter values for number of heights (nhgtt), number
!!       of latitudes (nlatt), and number of dust optical depth values
        integer, parameter :: nhgtt = 19
        integer, parameter :: nlatt = 36
        integer, parameter :: ndust = 3
        integer, parameter :: nf10 = 2
!!...    MTGCM 80-170 km data arrays for interpolation

        real(kind=PM_REEL) :: time,TTGCM,PTGCM,DTGCM,UTGCM,     &
         VTGCM,ZF,TempDay,PresDay,DensDay,UwndDay,VwndDay,Tempmax,      &
         Tempmin,Densmax,Densmin
!!        real(kind=PM_REEL) :: RTGCM, ZT,ZF
        real(kind=PM_REEL) :: RTGCM

        integer :: khgtt
        Common /TGCMdata_2001/TTA0(nhgtt,nlatt,0:12,ndust,nf10),        &
         TTA1(nhgtt,nlatt,0:12,ndust,nf10),TTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),TTA2(nhgtt,nlatt,0:12,ndust,nf10),TTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),PTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         PTA1(nhgtt,nlatt,0:12,ndust,nf10),PTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),PTA2(nhgtt,nlatt,0:12,ndust,nf10),PTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),DTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         DTA1(nhgtt,nlatt,0:12,ndust,nf10),DTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),DTA2(nhgtt,nlatt,0:12,ndust,nf10),DTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),UTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         UTA1(nhgtt,nlatt,0:12,ndust,nf10),UTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),UTA2(nhgtt,nlatt,0:12,ndust,nf10),UTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),VTA0(nhgtt,nlatt,0:12,ndust,nf10),      &
         VTA1(nhgtt,nlatt,0:12,ndust,nf10),VTP1(nhgtt,nlatt,0:12,       &
         ndust,nf10),VTA2(nhgtt,nlatt,0:12,ndust,nf10),VTP2(nhgtt,      &
         nlatt,0:12,ndust,nf10),                                        &
         ZFA0(nlatt,0:12,ndust,nf10),ZFA1(nlatt,0:12,ndust,nf10),       &
         ZFP1(nlatt,0:12,ndust,nf10),ZFA2(nlatt,0:12,ndust,nf10),       &
         ZFP2(nlatt,0:12,ndust,nf10)

! Common Interp_2001
        integer :: ilat,jlon,ls,mdust, k1st,ilatw,ilatt,mf10
        real(kind=PM_REEL) :: dlat, dlon,dls,ddust,dlatw,dlatt,df10,    &
             wpolefac,tpolefac
        Common /Interp_2001/ilat,ilatw,ilatt,jlon,ls,mdust,k1st,mf10,dlat, &
               dlon,dlatw,dls,ddust,dlatt,df10,wpolefac,tpolefac

! Variables locales
        real(kind=PM_REEL) :: TT(2,2,2,2),PT(2,2,2,2),UT(2,2,2,2),VT(2,2,2,2), &
         R0(2,2,2,2),DT(2,2,2,2),ZT(2,2,2,2),Tday(2,2,2,2),             &
         Pday(2,2,2,2),Uday(2,2,2,2),Vday(2,2,2,2),Tmax(2,2,2,2),       &
         Tmin(2,2,2,2),Dmax(2,2,2,2),Dmin(2,2,2,2)
        real(kind=PM_REEL) :: T0,P0,D0,U0,V0,Z0,A1,P1,A2,P2,xtime
        real(kind=PM_REEL) :: A1p,P1p,A2p,P2p
        integer :: itime, i, l, m, n

!!...    Establish MTGCM values at corners of a 4-dimensional cube in
!!       latitude-Ls-dust-F107 space, at the given height index (khgtt),
!!       and time of day (time)
        Do 102 i = 1,2
          polefac = 1.
          If (ilatt.eq.1)Then
            If (i.eq.1)polefac = real(tpolefac,kind=4)
          Else If (ilatt.eq.nlatt-1)Then
            If (i.eq.2)polefac = real(tpolefac,kind=4)
          Endif
        Do 101 l = 1,2
        Do 100 m = 1,2
        Do 99 n = 1,2
!!...      Daily mean temperature
          T0 = TTA0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          Tday(i,l,m,n) = T0
!!...      Temperature tide amplitudes and phases
          A1 = TTA1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1 = TTP1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2 = TTA2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2 = TTP2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      Temperature at corners of 3-D cube
          TT(i,l,m,n) = TideX_2001(T0,A1,P1,A2,P2,time)
!!...      Max and Min temperatures at corners of 3-D cube
          Tmax(i,l,m,n) = -9999.
          Tmin(i,l,m,n) = 9999.
          Do 50 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            Ttime = real(TideX_2001(T0,A1,P1,A2,P2,xtime),kind=4)
            If (Ttime.gt.Tmax(i,l,m,n))Tmax(i,l,m,n) = Ttime
            If (Ttime.lt.Tmin(i,l,m,n))Tmin(i,l,m,n) = Ttime
  50      Enddo
!!...      Daily mean pressure
          P0 = PTA0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          Pday(i,l,m,n) = P0
!!...      Pressure tide amplitudes and phases
          A1p = PTA1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1p = PTP1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2p = PTA2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2p = PTP2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      Pressure at corners of 3-D cube
          PT(i,l,m,n) = TideY_2001(P0,A1p,P1p,A2p,P2p,time)
!!...      Daily mean density
          D0 = DTA0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      Density tide amplitudes and phases
          A1 = DTA1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1 = DTP1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2 = DTA2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2 = DTP2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      Density at corners of 3-D cube
          DT(i,l,m,n) = TideY_2001(D0,A1,P1,A2,P2,time)
!!...      Max and Min densities at corners of 3-D cube
          Dmax(i,l,m,n) = -9999.
          Dmin(i,l,m,n) = 9999.
          Do 60 itime = 0,23
            xtime = real(itime,kind=PM_REEL)
            Dtime = real(TideY_2001(D0,A1,P1,A2,P2,xtime),kind=4)
            If (Dtime.gt.Dmax(i,l,m,n))Dmax(i,l,m,n) = Dtime
            If (Dtime.lt.Dmin(i,l,m,n))Dmin(i,l,m,n) = Dtime
  60      Enddo
!!...      Gas constant from pressure, density, and temperature
          R0(i,l,m,n) = 190.
          If (T0.ne.0.0.and.D0.ne.0.0)R0(i,l,m,n) = P0/(T0*D0)
!!...      Daily mean EW wind
          U0 = UTA0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          Uday(i,l,m,n) = U0
!!...      EW wind tide amplitudes and phases
          A1 = UTA1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1 = UTP1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2 = UTA2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2 = UTP2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      EW wind at corners of 3-D cube
          UT(i,l,m,n) = TideX_2001(U0,A1,P1,A2,P2,time)
!!...      Daily mean NS wind
          V0 = VTA0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          Vday(i,l,m,n) = V0
!!...      NS wind tide amplitudes and phases
          A1 = VTA1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1 = VTP1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2 = VTA2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2 = VTP2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      NS wind at corners of 3-D cube
          VT(i,l,m,n) = TideX_2001(V0,A1,P1,A2,P2,time)
!!...      Tide amplitudes and phases for ZF=height of 1.26 nbar level
          Z0 = ZFA0(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A1 = ZFA1(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P1 = ZFP1(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
          A2 = ZFA2(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac
          P2 = ZFP2(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)
!!...      ZF values at corners of 3-D cube
          ZT(i,l,m,n) = TideX_2001(Z0,A1,P1,A2,P2,time)
  99    Enddo
 100    Enddo
 101    Enddo
 102    Enddo
!!...    Use 4-D interpolation to get temperature, pressure, density,
!!       gas constant, EW wind, NS wind, and ZF height at given lati-
!!       tude, Ls, dust optical depth, and solar activity
        Call FourD_2001(dlatt,dls,ddust,df10,TT,TTGCM)
        Call FourD_2001(dlatt,dls,ddust,df10,Tday,TempDay)
        Call FourD_2001(dlatt,dls,ddust,df10,Tmax,Tempmax)
        Call FourD_2001(dlatt,dls,ddust,df10,Tmin,Tempmin)
        Call FourD_2001(dlatt,dls,ddust,df10,Dmax,Densmax)
        Call FourD_2001(dlatt,dls,ddust,df10,Dmin,Densmin)
        Call FourD_2001(dlatt,dls,ddust,df10,PT,PTGCM)
        Call FourD_2001(dlatt,dls,ddust,df10,Pday,PresDay)
        Call FourD_2001(dlatt,dls,ddust,df10,DT,DTGCM)
        Call FourD_2001(dlatt,dls,ddust,df10,R0,RTGCM)
!!...    Daily density from gas constant
        DensDay = PresDay/(RTGCM*TempDay)
        Call FourD_2001(dlatt,dls,ddust,df10,UT,UTGCM)
        Call FourD_2001(dlatt,dls,ddust,df10,Uday,UwndDay)
        Call FourD_2001(dlatt,dls,ddust,df10,VT,VTGCM)
        Call FourD_2001(dlatt,dls,ddust,df10,Vday,VwndDay)
        Call FourD_2001(dlatt,dls,ddust,df10,ZT,ZF)
        Return
      End Subroutine TGCMterp_2001
!!-----------------------------------------------------------------------
      Subroutine TwoD_2001(dx,dy,Array,Value)
!!...    2-Dimensional linear interpolation within a 1x1 cube (x,y) of
!!       Array(2,2) to position dx,dy (both 0-1).
!!       Value is value of interpolated output.
        Real(kind=PM_REEL) :: dx,dy, Array(2,2),Value
        Real(kind=PM_REEL) :: dxp,dyp
        real :: V

!!...    Complementary displacements in x,y,z
        dxp = 1. - dx
        dyp = 1. - dy
!!...    Do 2-D linear interpolation to get Value from Array
        Value = dxp*dyp*Array(1,1) + dxp*dy*Array(1,2) &
               + dx*dyp*Array(2,1) + dx*dy*Array(2,2)
        V = real(Value)
        Value = real(V,kind=PM_REEL)
        Return
      End Subroutine TwoD_2001
!!-----------------------------------------------------------------------
      Subroutine Wavelon_2001(LonEW,Wlon,CLat,Height,wavepert)
!!...  Inputs
!!       LonEW:  0 if West Longitude phases, 1 if East Longitude phases
!!       Wlon:   Current West Longitude (degrees)
!!       CLat:   Current Latitude (degrees)
!!       Height: Current altitude (km)
!!     Output:
!!       wavepert: relative perturbation due to wave model
!!
        integer :: LonEW
        real(kind=PM_REEL) :: Wlon,CLat,Height,wavepert


! common Wavecoef_2001
        REAL(KIND=PM_REEL) :: WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
             WaveA3,Wavephi3,Wscale
        integer :: nwave,iuwave
        real :: wavetime(100),wavedata(100,7)
        Common /Wavecoef_2001/WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
            WaveA3,Wavephi3,wavetime,wavedata,nwave,iuwave,Wscale

!!...  Scale for height variation of wave perturbations below 100 km
      Scale = real(Wscale,kind=4)
      Heightfact = 1.
!!...  Assume exponential variation with height for waves below 100 km
      If (Height.lt.100.)Heightfact = real(Exp((Height-100.)/Scale),kind=4)
      pi180 = Atan(1.)/45.
      phi1 = real(Wavephi1)
      phi2 = real(Wavephi2)
      phi3 = real(Wavephi3)
!!...  Convert phases to West longitude if LonEW = 1
      If (LonEW.eq.1)Then
        phi1 = 360. - phi1
        phi2 = 360. - phi2
        phi3 = 360. - phi3
      Endif
!!...  Relative perturbation factor due to wave model
      wavepert = (WaveA0 + WaveA1*Cos(pi180*(Wlon-phi1)) +               &
       WaveA2*Cos(pi180*(2.*Wlon-phi2)) + WaveA3*Cos(pi180*             &
       (3.*Wlon-phi3)) - 1.)*Heightfact
!!...  Insure wave perturbation goes to (WaveA0-1.)*Heightfact at poles
      If (Abs(CLat).ge.85.)Then
        polefact = real((Cos(18.*pi180*(Abs(CLat)-85.)))**2,kind=4)
        wavepert = (wavepert - (WaveA0-1.)*Heightfact)*polefact +        &
         (WaveA0-1.)*Heightfact
      Endif
!!...  Insure wave perturbation within proper limits
      If (wavepert.lt.-0.9_PM_REEL) wavepert = -0.9_PM_REEL
      If (wavepert.gt.9._PM_REEL) wavepert = 9._PM_REEL
      Return
    End Subroutine Wavelon_2001
!!-----------------------------------------------------------------------
    Real(kind=PM_REEL) Function Zlogr_2001(znumer,zdenom)
      real (kind=PM_REEL) :: znumer,zdenom
!!...    Log of znumer/zdenom with error message for znumer, zdenom <= 0
      If (znumer.le.0_PM_REEL.or.zdenom.le.0_PM_REEL)Then
         Zlogr_2001 = 1._PM_REEL
         Write(*,*)' Log error:',znumer,zdenom
      Else
         Zlogr_2001 = Dlog(znumer/zdenom)
      Endif
      Return
    End Function Zlogr_2001
!!-----------------------------------------------------------------------

!!!!!! Sous-programmes locaux, fichier setup_2001.f

    Subroutine  Setup_2001(CHGT,CLAT,CLON,CSEC,DATE0,RHOd,RHOu,RHOv,  &
      DHGT,DLAT,DLON,DTIME,MAXNUM,NRN1,NMCR1,dsunLs,dradau,LnEW,      &
      INPUTFL,iustdout,iulist,hgtasfc,DIR)
!!
1     Format(' Mars-GRAM 2001 (Version ',A1,') - July, 2001')
!!
!!...  Set parameter values for number of MGCM latitudes and longitudes,
!!     number of MOLA topography latitudes and longitudes, number of
!!     boundary layer levels, and number of dust optical depths
!!...  Parameter (nlat = 25)
!!...  Parameter (nlon = 40)
      integer, parameter :: nmtlat = 361
      integer, parameter :: nmtlon = 721
      integer, parameter :: nalblat = 181
      integer, parameter :: nalblon = 361
      integer, parameter :: nbl = 3
      integer, parameter :: ndust = 3
      integer, parameter :: nf10 = 2
      Character*1 :: version,EWlon
      Parameter (version = '1')
      Character*60 :: DATADIR,GCMDIR
      character*(*) :: DIR
      Character*(*) :: INPUTFL
      integer :: MAXNUM,NRN1,NMCR1, LNEW, iustdout,iulist
!!      Character*12 TRAJFL,INPUTFL,WaveFile
      Character*12 :: TRAJFL,WaveFile, nomvar
      REAL (KIND=PM_REEL) :: XYEAR,DATE,DATE0,dsunLs,dradau,chgt,clat,clon,csec
      REAL (KIND=PM_REEL) :: MARSAU,SUNLAT,SUNLON,ALS,hgtasfc
      integer :: IDAY(12),IERR(13)
!!
      Character*12 :: FILES(13)
      Character*7 :: HgtLbl
!!
! Common RANDCOM_2001
      integer :: IX, IY, IZ
      COMMON /RANDCOM_2001/IX,IY,IZ

! common TERHGT_2001
      real :: areorad(0:nmtlat,0:nmtlon), &
           topomola(0:nmtlat,0:nmtlon),albedo(0:nalblat,0:nalblon)
      COMMON /TERHGT_2001/areorad,topomola,albedo

! common TGCMoffset_2001
      REAL(KIND=PM_REEL) :: offsets(0:12,ndust),zoffset,hgtoffset
      integer :: ibougher
      Common /TGCMoffset_2001/offsets,zoffset,hgtoffset,ibougher

! Common cosparnh_2001
      real(kind=PM_REEL) :: zc(164),tc(164),pc(164),dc(164)
      Common /cosparnh_2001/zc,tc,pc,dc

! Common THERM_2001
      real(kind=PM_REEL) :: F107,stdl
      COMMON /THERM_2001/F107,stdl

!     common Variables in DATACOM_2001
      real (kind=PM_REEL) :: DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          dTEX,rpfactor,Dusttau,DustOD,Dustnu,Dustdiam,Dustdens
      integer :: NPOS,NVARX,NVARY,logscale,iu0,iup,maxfiles,MOLAhgts

      COMMON /DATACOM_2001/NPOS,NVARX,NVARY,logscale, iu0,iup,maxfiles,MOLAhgts,&
          DTR,DAY,dustlat,dustlon,radmax,Rref,als0,alsdur,intens, &
          dTEX,rpfactor,Dusttau,DustOD,Dustnu,Dustdiam, &
          Dustdens

! Common MGCMparm_2001
      Character*2 :: dustc(ndust),solact (nf10)
      real(kind=PM_REEL) :: dust(ndust),dzbl(nbl),zwsfc,f10val(nf10)
      Common /MGCMparm_2001/dust,dzbl,zwsfc,f10val, &
           dustc,solact

! common Wavecoef_2001
      REAL(KIND=PM_REEL) :: WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
           WaveA3,Wavephi3,Wscale
      integer :: nwave,iuwave
      real :: wavetime(100),wavedata(100,7)
      Common /Wavecoef_2001/WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,      &
           WaveA3,Wavephi3,wavetime,wavedata,nwave,iuwave,Wscale


!!......................................................................
!!     If output to the list file, output file, and plotable files
!!     is not desired, the following statements may be removed
!!
! common FILENAME_2001
      Character*12 :: lstfl,outfl
      COMMON /FILENAME_2001/lstfl,outfl

      integer :: L,IERR1,IERR2,IERR3,IERR4,IERR5,IERR6,IERR7,IERR8,IERR9, &
           IERR10,IERR11,IERR12,IERR13

      EQUIVALENCE (IERR1,IERR(1)),(IERR2,IERR(2)),(IERR3,IERR(3)),       &
      (IERR4,IERR(4)),(IERR5,IERR(5)),(IERR6,IERR(6)),(IERR7,IERR(7))   &
      ,(IERR8,IERR(8)),(IERR9,IERR(9)),(IERR10,IERR(10)),               &
      (IERR11,IERR(11)),(IERR12,IERR(12)),(IERR13,IERR(13))

! Variables locales 
      integer :: NDAY,ls, j,lendir,i,NMONTE,ioerr,nr1,LonEW, &
           Month,Mday,Myear,Ihr,Imin
      real :: dusthgt,hgtasfcm,rpscale,deltaTEX, &
           Flat,Flon,Fhgt,Delhgt,Dellat,Dellon,Deltime,Sec
      real :: z1

!!
!!     Note, however, molatoph.bin, COSPAR2.DAT, and albedo1.bin files
!!     are required
!!
      DATA FILES/'LIST.txt','Density.txt','Perturb.txt','Winds.txt',     &
      'TPresHgt.txt','DayData.txt','ThrmData.txt','molatoph.bin',       &
      'COSPAR2.DAT','OUTPUT.txt','hgtoffst.dat','albedo1.bin',          &
      'MarsRad.txt'/
!!......................................................................
!!...  Days for months of the year
      DATA IDAY/0,31,59,90,120,151,181,212,243,273,304,334/
!!...  Establish default values for input parameters
      Data TRAJFL/'null'/
      Data DATADIR,GCMDIR/'null','null'/
      Data WaveFile/'null'/
!!...  Default time = Viking 1 Lander
      Data Month,Mday,Myear,Ihr,Imin,Sec/7,20,76,12,30,0.0/
!!...  Default position = Viking 1 Lander Site, height = surface
      Data Flat,Flon,Fhgt,Delhgt,Dellat,Dellon,Deltime/22.,48.,-0.5,     &
       10.,3*0./
!!...  Default deltaTEX to zero
      Data deltaTEX/0.0/
!!...  Default random perturbation scale factor
      Data rpscale/1.0/
!!......................................................................
!!     Definition of the Namelist input data
      Namelist /INPUT/LSTFL,OUTFL,TRAJFL,WaveFile,DATADIR,GCMDIR,MONTH,  &
      MDAY,MYEAR,NPOS,IHR,IMIN,SEC,LonEW,Dusttau,Dustnu,Dustdiam,       &
      Dustdens,ALS0,ALSDUR,INTENS,RADMAX,DUSTLAT,DUSTLON,F107,STDL,     &
      NR1,NVARX,NVARY,LOGSCALE,FLAT,FLON,FHGT,MOLAhgts,hgtasfcm,        &
      zoffset,ibougher,DELHGT,DELLAT,DELLON,DELTIME,deltaTEX,rpscale,   &
      NMONTE,iup,WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,WaveA3,         &
      Wavephi3,iuwave,Wscale
!!.....................................................................
!!
!!...  Default with no wave model modification
      WaveA0 = 1.
      WaveA1 = 0.
      Wavephi1 = 0._PM_REEL
      WaveA2 = 0.
      Wavephi2 = 0._PM_REEL
      WaveA3 = 0.
      Wavephi3 = 0._PM_REEL
      iuwave = 0
      Wscale = 20.
!!...  Set dust optical depths for GCM data
      dust(1) = 0.3_PM_REEL
      dust(2) = 1.0_PM_REEL
      dust(3) = 3.0_PM_REEL
      dustc(1) = '03'
      dustc(2) = '10'
      dustc(3) = '30'
!!...  Set heights for GCM boundary layer data
      dzbl(1) = 0.0_PM_REEL
      dzbl(2) = 0.005_PM_REEL
      dzbl(3) = 0.030_PM_REEL
!!...  Set surface roughness parameter (m).  Value 1 cm (0.01 m) is
!!     consistent with NASA Ames MGCM boundary layer model
      zwsfc = 0.01_PM_REEL
!!...  Set solar activity values (F10.7 at 1AU)
      f10val(1) = 70._PM_REEL
      f10val(2) = 130._PM_REEL
      solact(1) = 'ls'
      solact(2) = 'ms'
!!...  Set unit number for screen I/O and pass it into Common
      iu0 = iustdout
!!...  default list and output files
      lstfl = 'LIST.txt'
      outfl = 'OUTPUT.txt'
!!...  Default number of positions
      NPOS = 21
!!...  Default use West Longitude positive (USGS Convention)
      LonEW = 0
!!...  Default background dust optical depth = 0.3
      Dusttau = 0.3
!!...  Default dust particle density parameters
!!     See Fig. 2 of Haberle et al., J. geophys. Res., vol 104, p. 8957
!!     (1999) and Haberle et al., Icarus, vol 50, p. 322 (1982)
      Dustnu = 0.003
      Dustdiam = 5.
      Dustdens = 3000.
!!...  Default no dust storm
      ALS0 = 0.0
      ALSDUR = 48.
      INTENS = 0.0
      RADMAX = 0.0
      DUSTLAT = 0.0
      DUSTLON = 0.0
!!...  Default Solar Flux parameters
      F107 = 68.0
      Stdl = 0.0
!!...  Default plot variable = height above MOLA areoid
      NVARX = 1
      NVARY = 0
!!...  Default to regular linear scale
      LOGSCALE = 0
!!...  Default random number seed and Number of Monte Carlo runs
      NR1 = 1001
      NMONTE = 1
!!...  Default unit number for print output data file
      iup = 13
!!...  Set length of Mars day
      DAY = 24.622962
!!...  Default to input heights above MOLA areoid
      MOLAhgts = 1
!!...  Default height above surface
      hgtasfcm = 0.
!!...  Default (global) MTGCM height offset
      zoffset = 5.
      ibougher = 2
!!...  Open Namelist data file
      OPEN(8,file=INPUTFL,status='old',iostat=ioerr)
      If (ioerr.ne.0)Then
        Write(iustdout,*)' Error opening NAMELIST input file'
        Stop
      Endif
!!...  Read Namelist data
!      Read(8,INPUT)
!      Close(8)
!!.....................................................................

     Read(8,10)nomvar,LSTFL
     Read(8,10)nomvar,OUTFL
     Read(8,10)nomvar,TRAJFL
     Read(8,10)nomvar,WaveFile
     Read(8,10)nomvar,DATADIR
     Read(8,10)nomvar,GCMDIR
10   Format(A12,A12)
101  Format(A12,i6)
102  Format(A12,e15.7)
     Read(8,101)nomvar,MONTH
     Read(8,101)nomvar,MDAY
     read(8,101)nomvar,MYEAR
     read(8,101)nomvar,NPOS
     read(8,101)nomvar,IHR
     read(8,101)nomvar,IMIN
     read(8,102)nomvar,SEC
     read(8,101)nomvar,LonEW
     read(8,102)nomvar,Dusttau
     read(8,102)nomvar,Dustnu
     read(8,102)nomvar,Dustdiam
     read(8,102)nomvar,Dustdens
     read(8,102)nomvar,ALS0
     read(8,102)nomvar,ALSDUR
     read(8,102)nomvar,INTENS
     read(8,102)nomvar,RADMAX
     read(8,102)nomvar,DUSTLAT
     read(8,102)nomvar,DUSTLON
     read(8,102)nomvar,F107
     read(8,102)nomvar,STDL
     read(8,101)nomvar,NR1
     read(8,101)nomvar,NVARX
     read(8,101)nomvar,NVARY
     read(8,101)nomvar,LOGSCALE
     read(8,102)nomvar,FLAT
     read(8,102)nomvar,FLON
     read(8,102)nomvar,FHGT
     read(8,101)nomvar,MOLAhgts
     read(8,102)nomvar,hgtasfcm
     read(8,102)nomvar,zoffset
     read(8,101)nomvar,ibougher
     read(8,102)nomvar,DELHGT
     read(8,102)nomvar,DELLAT
     read(8,102)nomvar,DELLON
     read(8,102)nomvar,DELTIME
     read(8,102)nomvar,deltaTEX
     read(8,102)nomvar,rpscale
     read(8,101)nomvar,NMONTE
     read(8,101)nomvar,iup
     read(8,102)nomvar,WaveA0
     read(8,102)nomvar,WaveA1
     read(8,102)nomvar,Wavephi1
     read(8,102)nomvar,WaveA2
     read(8,102)nomvar,Wavephi2
     read(8,102)nomvar,WaveA3
     read(8,102)nomvar,Wavephi3
     read(8,101)nomvar,iuwave
     read(8,102)nomvar,Wscale
     close(8)

!!     write(*,*) "Dustnu=",Dustnu

!!     For compilers not supporting the NAMELIST input mode, the
!!     previous Read statement may be replaced by:
!!
!!     Read(8,10)LSTFL
!!     Read(8,10)OUTFL
!!     Read(8,10)TRAJFL
!!     Read(8,10)WaveFile
!!     Read(8,10)DATADIR
!!     Read(8,10)GCMDIR
!! 10  Format(A)
!!     Read(8,*)MONTH,MDAY,MYEAR,NPOS,IHR,IMIN,SEC,LonEW,Dusttau,
!!    & Dustnu,Dustdiam,Dustdens,ALS0,ALSDUR,INTENS,RADMAX,DUSTLAT,
!!    & DUSTLON,F107,STDL,NR1,NVARX,NVARY,LOGSCALE,FLAT,FLON,FHGT,
!!    & MOLAhgts,hgtasfcm,zoffset,ibougher,DELHGT,DELLAT,DELLON,DELTIME,
!!    & deltaTEX,rpscale,NMONTE,iup,WaveA0,WaveA1,Wavephi1,WaveA2,
!!    & Wavephi2,WaveA3,Wavephi3,iuwave,Wscale
!!
!!     and the NAMELIST file INPUT may be modified to contain free-
!!     field input data as in the above list.
!!.....................................................................
!!...  Check that unit iup is in allowable range
      If ((iup.ge.5.and.iup.le.12).or.(iup.ge.21.and.iup.le.29))         &
        Stop ' Unit iup conflict with another file'
      If (FLAT .lt. -90.0 .or. FLAT .gt. 90.0)then
        Write(iu0,382)
382     Format(' Error in first latitude or longitude.')
        Goto 9998
      Endif
      If (FLON .lt. 0.0)FLON = FLON + 360.0
      If (FLON .lt. 0.0 .or. FLON .gt. 360.0)then
        Write(iu0,382)
        Goto 9998
      Endif
      LnEW = LonEW
      iulist = iup
!!...  Convert Height above surface to km and insure proper range
      If (hgtasfcm.lt.0.0)hgtasfcm = 0.0
      If (hgtasfcm.gt.1000.)hgtasfcm = 1000.
      hgtasfc = hgtasfcm/1000.
!!...  Insure Wscale within proper range
      If (Wscale.le.10.)Wscale = 10.
      If (Wscale.ge.10000.)Wscale = 10000.
!!...  Open and read WaveFile and load wavedata array if iuwave>0
      If (iuwave.gt.0)Then
        Open(iuwave,File=WaveFile,iostat=ierr1)
        If (ierr1.ne.0) Stop '  Error opening WaveFile'
        nwave = 1
!!...    Each WaveFile record contains: (1) time at which wave model
!!         coefficients first apply (1st record must start at time=0),
!!         (2) WaveA0, (3) WaveA1, (4) Wavephi1, (5), WaveA2, (6)
!!         Wavephi2, (7) WaveA3, (8) Wavephi3.  Times for wave model
!!         coefficients must be in ascending order.
5       Read(iuwave,*,End=6)wavetime(nwave),(wavedata(nwave,i),i=1,7)
        If(nwave.eq.1)Then
          If (wavetime(1).gt.0.0)                                        &
           Stop ' First wave data in file must be at time 0'
        Else
          If (wavetime(nwave).le.wavetime(nwave-1))                      &
           Stop ' Wave data in file must in increasing order by time'
        Endif
        If(wavedata(nwave,1).lt.0.1.or.wavedata(nwave,1).gt.12.0)        &
           Stop ' WaveA0 from input file is out of range'
        nwave = nwave + 1
        Goto 5
!!...    Close wave data file
6       Close(iuwave)
!!...    Re-set nwave to number of wave data
        nwave = nwave - 1
!!...    Re-initialize wave coefficients
        WaveA0   = wavedata(1,1)
        WaveA1   = wavedata(1,2)
        Wavephi1 = wavedata(1,3)
        WaveA2   = wavedata(1,4)
        Wavephi2 = wavedata(1,5)
        WaveA3   = wavedata(1,6)
        Wavephi3 = wavedata(1,7)
      Endif
!!...  Convert FLON, DUSTLON, DELLON to West if LonEW=1
      If (LonEW.eq.1)Then
        DUSTLON = 360. - DUSTLON
        FLON = 360. - FLON
        DELLON = -DELLON
      Endif
      rpfactor = rpscale
      If (rpscale.lt.0.0.or.rpscale.gt.2.0)                              &
       Stop ' Must have 0 <= rpscale <= 2'
      If (WaveA0.le.0.1.or.WaveA0.gt.12.0)Stop ' WaveA0 out of range'
      If (NMONTE.LT.1)NMONTE = 1
!!...  Pass 1st random number and Number Monte Carlo runs to output
      NRN1 = NR1
      NMCR1 = NMONTE
!!
      DHGT = DELHGT
      DLAT = DELLAT
      DLON = DELLON
      DTIME = DELTIME
!!
!!     If output to the list file, output file, and plotable files
!!     is not desired, the following statements may be removed.
!!     Note, however, that the HEIGHTS.DAT file is required.
!!     Note that output to list and other files can also be suppressed
!!     by setting iup to 0 above, near line SETU153
!!
      files(1) = lstfl
      files(10) = outfl
      If (lstfl .ne. 'CON' .and. lstfl .ne. 'con'.and.iup.gt.0)Then
        OPEN(iup,file=lstfl,iostat=ierr1)
      Endif
!!...  Write version number and file names to standard output
!!      Write(iu0,1)version
!!      If(iup.gt.0)Then
!!       Write(iu0,14)LSTFL,OUTFL
!!      Else
!!        Write(iu0,14)' null ',' null '
!!      Endif
!!      Write(iu0,15)DATADIR,GCMDIR
!!      Write(iu0,15)DIR,DIR
!!      If (npos.eq.0)Write(iu0,16)TRAJFL
      IF(NPOS.gt.0) Goto 12
!!...  If NPOS = 0 is entered, program reads position data from
!!      unit 7, trajectory data file
!!...  Each trajectory file record contains time (seconds from initial
!!      time), height (km), latitude (degrees, North positive), and
!!      longitude (degrees, West positive if LonEW=0 or East positive
!!      if LonEW=1).
      Open(7,file=TRAJFL,status='old',iostat=ierr7)
      If(ierr7.ne.0)then
        Write(iu0,11)
11      Format(' Unable to open Trajectory Data file!!')
        Goto 9998
      Endif
12    MAXNUM = NPOS - 1
      IF(NPOS.LE.0)MAXNUM = 99999
!!...  Write version number and file names to LIST file
      If (iup.gt.0)Then
        Write(iup,1)version
       Write(iup,14)LSTFL,OUTFL
        Write(iup,15)DIR,DIR
        If (npos.eq.0)Write(iup,16)TRAJFL
        If (iuwave.gt.0)Write(iup,17)WaveFile
      Endif
14    Format(' LIST file= ',A12,'    OUTPUT file= ',A12)
15    Format(' Data directory= ',A250,' GCM  directory= ',A250)
16    Format(' Position input from trajectory file= ',A12)
17    Format(' Wave coefficients read from file= ',A12)
!!...  Files on units 21-27 contain parameters suitable for plotting.
!!...  Data are in either of two forms: (1)  X  Y1 Y2 ..., where X
!!...  is the variable to be plotted against (e.g. height), and Y1, Y2,
!!...  etc. are variables to be plotted, or (2) X Y Z1 Z2 ..., where X
!!...  and Y are two variables (e.g. latitude and height) to provide
!!...  position for plotting contour plots of one of the variables
!!...  Z1, Z2, etc.
      If (iup.gt.0)Then
!!...    Unit 21 file = 'Density.txt': Headers for variables are -
!!         DENSLO  =  low (-1 sigma) density
!!         DENSAV  =  average (mean plus wave-perturbed) density
!!         DENSHI  =  high (+1 sigma) density
!!         DENSTOT =  total (average+perturbed) density
!!           (density units kg/m**3, log-10 scale, % from COSPAR, or
!!             kg/km**3, depending on value of LOGSCALE input parameter)
!!         DustOD  =  dust optical depth
!!         Radius  =  Radial distance from planetary center of mass to
!!                    spacecraft position (areoid radius plus altitude)
!!         Grav    =  local acceleration of gravity (m/s**2)
!!         RadAU   =  Mars orbital radius (Astronomical Units)
!!...
        OPEN(21,file=files(2),iostat=ierr2)
        If(NVARY.eq.0)Then
          Write(21,621)
        Else
          Write(21,721)
        Endif
 621    Format('    Var_X        DENSLO     DENSAV     DENSHI',          &
         '    DENSTOT  DustOD  Radius   Grav RadAU')
 721    Format('    Var_X        Var_Y        DENSLO     DENSAV',        &
         '     DENSHI    DENSTOT  DustOD  Radius   Grav RadAU')
!!...    Unit 22 file = 'Perturb.txt': Headers for variables are:
!!         SigD     = Density standard deviation (% of unperturbed mean)
!!         DensRand = Random density perturbation (% of unpert. mean)
!!         DensWave = Density wave perturbation (% of unperturbed mean)
!!         DensP    = Total density perturbation (% of unperturbed mean)
!!         corlim   = Ratio of step size to correlation accuracy limit
!!                     (ideally should be 1.0 or larger)
!!         SigU     = Standard deviation for wind perturbations (m/s)
!!...
        OPEN(22,file=files(3),iostat=ierr3)
        If(NVARY.eq.0)Then
          Write(22,622)
        Else
          Write(22,722)
        Endif
 622    Format('    Var_X        SigD    DensRand    DensWave',          &
         '       DensP      corlim       SigU')
 722    Format('    Var_X        Var_Y        SigD    DensRand',         &
         '    DensWave       DensP      corlim       SigU')
!!...    Unit 23 file = 'Winds.txt' : Headers for variables are -
!!         EWmean = mean eastward wind component (m/s)
!!         EWpert = perturbation in eastward wind component (m/s)
!!         EWtot  = total (mean + perturbed) eastward wind (m/s)
!!         NSmean = mean northward wind component (m/s)
!!         NSpert = perturbation in northward wind component (m/s)
!!         NStot  = total (mean + perturbed) northward wind (m/s)
!!...
        OPEN(23,file=files(4),iostat=ierr4)
        If (NVARY.eq.0)Then
          Write(23,623)
        Else
          Write(23,723)
        Endif
 623    Format('    Var_X      EWmean      EWpert       EWtot      ',    &
        'NSmean      NSpert       NStot')
 723    Format('    Var_X        Var_Y      EWmean      EWpert       ',  &
        'EWtot      NSmean      NSpert       NStot')
!!...    Unit 24 file = 'TPresHgt.txt' : Headers for variables are -
!!         Temp    - Mean temperature (K)
!!         Pres    - Mean plus wave-perturbed pressure (N/m**2, log-10,
!!                    or % from COSPAR)
!!         TdegC   - Mean temperature (degrees C)
!!         Pres_mb - Mean plus wave-perturbed pressure (mb)
!!         Hrho    - Density scale height (km)
!!         Terhgt  - Altitude of local surface above MOLA 1/2-degree
!!                   areoid
!!         Tgrnd   - ground surface temperature (K)
!!         Areoid  - local radius (km) of MOLA 1/2-degree areoid
!!         dAreoid - MOLA areoid minus radius of reference
!!                   ellipsoid (km)
!!...
        OPEN(24,file=files(5),iostat=ierr5)
        If (NVARY.eq.0)Then
          Write(24,624)
        Else
         Write(24,724)
        Endif
 624    Format('    Var_X       Temp      Pres   TdegC',                 &
        '   Pres_mb   Hrho TerHgt Tgrnd  Areoid  dAreoid')
 724    Format('    Var_X        Var_Y       Temp      Pres   TdegC',    &
        '   Pres_mb   Hrho TerHgt Tgrnd  Areoid  dAreoid')
!!...
!!...    Unit 25 file = 'DayData.txt' : Headers for variables are -
!!         TempDay = Local daily average temperature (K)
!!         PresDay = Local daily average pressure (N/m**2)
!!         DensDay = Local daily average density (kg/m**3)
!!         EWwnDay = Local daily average Eastward wind (m/s)
!!         NSwnDay = Local daily average Northward wind (m/s)
!!         Tempmin = Local daily minimum temperature (K)
!!         Tempmax = Local daily maximum temperature (K)
!!         Densmin = Local daily minimum density (kg/m**3)
!!         Densmax = Local daily maximum density (kg/m**3)
!!...
        Open(25,file=files(6),iostat=ierr6)
        If(NVARY.eq.0)Then
          Write(25,625)
        Else
          Write(25,725)
        Endif
 625    Format('    Var_X    TempDay  PresDay    DensDay',               &
        '   EWwnDay NSwnDay Tempmin Tempmax   Densmin    Densmax')
 725    Format('    Var_X        Var_Y    TempDay  PresDay',             &
        '    DensDay   EWwnDay NSwnDay Tempmin Tempmax   Densmin',      &
        '    Densmax')
!!...
!!...    Unit 26 file = 'ThrmData.txt' : Headers for variables are -
!!         Tbase  = temperature at 1.26 nbar level (K)
!!         Zbase  = height of 1.26 nbar level (km)
!!         F1peak = height of F1 peak layer (km)
!!         MolWgt = molecular weight (kg/kg mole)
!!         Texos  = exospheric temperature (K)
!!         hgtoffset = height offset (km) for MTGCM data
!!...
        OPEN(26,file=files(7),iostat=ierr7)
        If (NVARY.eq.0)Then
          Write(26,626)
        Else
          Write(26,726)
        Endif
 626    Format('    Var_X       Tbase   Zbase  F1peak',                  &
         '  MolWgt   Texos  hgtoffset')
 726    Format('    Var_X        Var_Y       Tbase   Zbase',             &
        '  F1peak  MolWgt   Texos  hgtoffset')
!!...
!!...    Unit 27 file = 'MarsRad.txt' : Headers for variables are -
!!         alb      = surface albedo
!!         mu0      = cosine of solar zenith angle
!!         Dareaden = dust column areal density (kg/m**2)
!!         Dmixrat  = dust mixing ratio (kg dust / kg air)
!!         Dmasden  = dust mass density (micrograms dust / m**3)
!!         Dnumden  = dust number density (number dust particles / m**3)
!!         Ice      = surface polar ice indicator (0 = no, 1 = yes)
!!
!!...
        Open(27,file=files(13),iostat=ierr13)
        If (NVARY.eq.0)Then
          Write(27,627)
        Else
          Write(27,727)
        Endif
 627    Format('    Var_X       alb    mu0 Dareaden  Dmixrat  Dmasden',  &
        '  Dnumden Ice')
727     Format('    Var_X        Var_Y       alb    mu0 Dareaden', &
        '  Dmixrat  Dmasden  Dnumden Ice')
!!
!!...    Unit 29 file = OUTPUT file containing list of variables given
!!       in Datastep routine (Format 800 or Format 810)
!!
        OPEN(29,file=files(10),iostat=ierr10)
        EWlon = 'W'
        If (LonEW.eq.1)EWlon = 'E'
        HgtLbl = 'HgtMOLA'
        If (MOLAhgts.ne.1)HgtLbl = 'HgtELPS'
        If (NVARX.eq.2.or.NVARY.eq.2)HgtLbl = 'HgtSFCM'
        Write(29,629)HgtLbl,EWlon
 629    Format('     Time  ',A7,'  Lat   Lon',A1,'   DensAV    Temp',    &
         '  EWind  NWind  sigD  Ls   Dust')
     Endif
!!...  Compute character string length of DATADIR path name
      lendir = Index(DIR,' ')-1
      If (lendir.lt.1.or.lendir.gt.250)lendir = 250
!!...  Unit 9 molatoph.bin file contains MOLA 1/2 degree resolution
!!     areoid radius and topographic heights above the areoid, versus
!!     latitude and longitude
#ifdef __GFORTRAN__
      OPEN(9,file=DIR(1:lendir)//files(8),status='old',                  &
       form='unformatted', convert='big_endian', iostat=ierr8)
#else
      OPEN(9,file=DIR(1:lendir)//files(8),status='old',                  &
       form='unformatted',iostat=ierr8)
#endif
!!...  Unit 10 COSPAR2.DAT file contains COSPAR atmosphere data values
      OPEN(10,file=DIR(1:lendir)//files(9),status='old',                 &
       iostat=ierr9)
!!...  Unit 11 hgtoffst.dat file with MTGCM height offset array
      Open(11,file=DIR(1:lendir)//files(11),status='old',                &
       iostat=ierr11)
!!...  Unit 12 albedo1.bin binary format albedo file
#ifdef __GFORTRAN__
      Open(12,file=DIR(1:lendir)//files(12),status='old',                &
       form='unformatted', convert='big_endian', iostat=ierr12)
#else
      Open(12,file=DIR(1:lendir)//files(12),status='old',                &
       form='unformatted',iostat=ierr12)
#endif
!!...  Test for file open error condition
      Do 40 j=1,13
        If(ierr(j).ne.0)then
          Write(iu0,60)files(j),ierr(j)
60        Format(1x,a12,' File open error!! Error =',i5)
          Goto 9998
        Endif
40   Enddo
!!......................................................................
!!...  Read COSPAR2.DAT atmosphere data
     Do 63 i = 1,164
        Read(10,*,end=65,err=65)zc(i),tc(i),pc(i),dc(i)
63   Enddo
     Goto 70
65    Stop ' Error or EOF on COSPAR2.DAT file!!'
!!...  Read MTGCM height offsets
70    Do 67 i = 0,12
66       Read(11,*,Err=66)ls,(offsets(i,j),j=1,3)
        If (ls.ne.30*i)Stop ' Error reading MTGCM height offsets'
67   Enddo
!!...  Read topographic height data file
!!     Write(iu0,*)' Reading MOLA 1/2 degree areoid and topography'
      Read(9)areorad,topomola
!!...  Read 1 degree resolution albedo file
!!      Write(iu0,*)' Reading 1 degree albedo data'
      Read(12)albedo
!!      Write(iu0,*)' Reading Mars GCM surface data files'
      Call Readsurf_2001(DIR,version)
!!      Write(iu0,*)' Reading Mars GCM 0-80 km data files'
      Call ReadMGCM_2001(DIR,version)
!!      Write(iu0,*)' Reading Mars TGCM 80-170 km data files'
      Call ReadTGCM_2001(DIR,version)
      DTR = Atan(1.)/45.
!!...  Go to next input set if month <1 or > 12
      If (MONTH .le. 0 .and. MDAY .le. 0 .and. MYEAR .le. 0)then
        Write(iu0,91)
91      Format(' Input error in month, day or year.')
        Goto 9998
      Endif
      IF(MONTH.LT.1.OR.MONTH.GT.12)then
        Write(iu0,91)
        Goto 9998
      Endif
      If(MDAY .lt. 1 .or. MDAY .gt. 31)then
        Write(iu0,91)
        Goto 9998
      Endif
      If (IHR .lt. 0 .or. IHR .gt. 23)then
        Write(iu0,92)
92      Format(' Input error in hour, minute or seconds.')
        Goto 9998
      Endif
      If (IMIN .lt. 0 .or. IMIN .gt. 59)then
        Write(iu0,92)
        Goto 9998
      Endif
      If (SEC .lt. 0.0 .or. SEC .gt. 60.0)then
        Write(iu0,92)
        Goto 9998
      Endif
!!...  20th century years >= 1970 can be entered in 2-digit form.
!!...  21st century years < 2070 can be entered in 2-digit form.
      If(MYEAR.LE.69)MYEAR = MYEAR + 2000
      IF(MYEAR.GE.70.and.MYEAR.LE.99)MYEAR = MYEAR + 1900
      NDAY = IDAY(MONTH) + MDAY
!!...  Correct for leap year
      IF(MOD(MYEAR,4).EQ.0.AND.MONTH.GT.2)NDAY = NDAY + 1
      XYEAR = (MYEAR - 1.9665d3)/4.0d0
!!...  Compute Julian date
      DATE = 2.439856d6 + 3.65d2*(MYEAR - 1.968d3) + NDAY +              &
      DINT(XYEAR + DSIGN(0.5d0,XYEAR)) - 0.5d0
!!...  Continuously running, fractional Julian date
      DATE = DATE + IHR/2.4d1 + IMIN/1.440d3 + SEC/8.6400d4
      DATE0 = DATE
      If (iup.gt.0)Write(iup,290)MONTH,MDAY,MYEAR,DATE,IHR,IMIN,SEC
290   FORMAT(' Date = ',I2,'/',I2,'/',I4,'  Julian Date = ',F9.1,        &
      '  UTC Time = ',I2,':',I2,':',F4.1)
      dTEX = deltaTEX
      If (iup.gt.0)Then
        If (deltaTEX.ne.0.0)Write(iup,291)deltaTEX
  291   Format(' deltaTEX=',F7.1,'K')
        If (MOLAhgts.eq.1)Then
          Write(iup,292)
        Else
          Write(iup,293)
        Endif
292     Format(' Input heights are relative to MOLA areoid')
293     Format(' Input heights are relative to reference',               &
         ' ellipsoid')
        If (zoffset.ne.0.0.or.ibougher.gt.0)Then
          If (ibougher.eq.3)Then
            Write(iup,20)
          Else If (ibougher.ge.4)Then
            Write(iup,21)
          Else If (ibougher.le.0)Then
            Write(iup,19)zoffset
          Else If (ibougher.eq.2)Then
            Write(iup,18)
          Else
            Write(iup,22)zoffset
          Endif
        Endif
      Endif
18    Format(' MTGCM Height offset from global offset file',             &
       ' hgtoffst.dat')
19    Format(' MTGCM Height offset from input value of',F6.2,' km')
20    Format(' MTGCM Height offset from local position, daily value')
21    Format(' MTGCM Height offset from local position, current time')
22    Format(' MTGCM Height offset from Ls-dependent model with',        &
      ' constant term',F6.2,' km'                         )
!!...  Sun position in Mars latitude (areocentric latitude) and
!!...  longitude. ALS = Ls = areocentric longitude of sun in orbital
!!...  position (Ls = 0 at spring equinox). MARSAU = Mars orbital
!!...  radius in Astronomical Units
      If (dradau.gt.0.0)Then
        ALS = dsunLs
        MARSAU = dradau
      Else
        CALL ORBIT_2001(DATE0,SUNLAT,SUNLON,ALS,MARSAU)
      Endif
      If (als0 .lt. 180.0 .or. als0 .gt. 320.0)then
        If (als0 .gt. 0.)Then
          Write(iu0,*)' ** Ls0 outside range. No dust storm assumed.'
          If(iup.gt.0)Write(iup,*)' ** Ls0 outside range. ',             &
            'No dust storm assumed.'
        Endif
        als0 = -999.
        intens = 0.0
      Else
        If (intens .lt. 0.0 .or. intens .gt. 3.0)then
          Write(iu0,*)' Intensity must be between 0 and 3'
          Goto 9998
        Endif
        If (radmax .le. 0.0 .or. radmax .gt. 10000.)radmax=0.
!!...    Set max 'half' height of local dust storms to 60. km
        dusthgt = 60.
        If (radmax .ne. 0.0)Then
          If (radmax .le. 180.)dusthgt = real(radmax/3.,kind=4)
        Endif
      Endif
!!...  Set ALSDUR within allowable range
      If (ALSDUR.lt.12.)ALSDUR = 12.
      If (ALSDUR.gt.48.)ALSDUR = 48.
      If (F107 .lt. 50.0 .or. F107 .gt. 450.0)then
        Write(iu0,*)' F10.7 must be between 50 and 450'
        Goto 9998
      Endif
      If (stdl .lt. -3.0 .or. stdl .gt. 3.0)then
        Write(iu0,*)' Std. deviations must be between -3 and +3'
        Goto 9998
      Endif
      IF (NR1 .LE. 0 .OR. NR1 .GE. 30000)then
        Write(iu0,298)
  298   Format(' Error in starting random number.')
        GOTO 9998
      Endif
      IX = NR1
      IY = 172 * Mod(IX, 176) - 35 * (IX / 176)
      IZ = 170 * Mod(IX, 178) - 63 * (IX / 178)
      If (IY .lt. 0) IY = IY + 30307
      If (IZ .lt. 0) IZ = IZ + 30323
      z1 = RANDOM_2001(L)
      RHOd = ppnd_2001(z1,L)
      z1 = RANDOM_2001(L)
      RHOu = ppnd_2001(z1,L)
      z1 = RANDOM_2001(L)
      RHOv = ppnd_2001(z1,L)
      IF (L .EQ. 1)then
        Write(iu0,298)
        GOTO 9998
      Endif
      If(lstfl .ne. 'CON' .and. lstfl .ne. 'con'.and.iup.gt.0)Then
        If (als0 .gt. 0.)Then
          If (radmax .ne. 0.0)Then
            Write(iup,363)als0,intens,alsdur,radmax,dustlat,dustlon,     &
            360.-dustlon
  363       Format(' Local scale dust storm, starting at Ls = ',         &
            F5.1,' deg.,  Intensity = ',F3.1/                           &
            '  with duration = ',F5.1,' degrees of Ls angle.'/          &
            ' Max. radius = ',F7.1, ' km,  At Lat-Lon = ',F6.2,         &
            ' N,  ',F6.2,' W (',F6.2,' E)')
          Else
            Write(iup,366)als0,intens,alsdur
  366       Format(' Global scale dust storm, starting at Ls = ',        &
            F5.1,' deg.,  Intensity = ',F3.1/                           &
            '  with duration = ',F5.1,' degrees of Ls angle.')
          Endif
        Endif
        If (iup.gt.0)Write(iup,368)F107,F107/MARSAU**2,stdl,Dustnu,      &
         Dustdiam,Dustdens
  368   Format(' F10.7 flux = ',F5.1,' (1 AU)  ',F5.1,                   &
         ' (Mars),  standard deviation = ',F4.1/' Dustnu =',F7.4,       &
         '   Dustdiam =',F6.2,' E-6 meters   Dustdens =',F8.1,          &
         ' kg/m**3')
        If (iup.gt.0)Write(iup,370)NR1,rpscale
      Endif
  370 FORMAT('   Random seed =',I6,'   Scale factor =',F4.1)
  380 FORMAT(/' Select x-code and y-code for plotable output versus',    &
      ' desired parameter(s):'//                                        &
      ' Code              Parameter'/                                   &
      ' ----   -------------------------------------------------'/      &
      '   1    Height (above MOLA areoid, km)'/                         &
      '   2    Height (above local MOLA topographic surface, km)'/      &
      '   3    Latitude (deg.)'/                                        &
      '   4    Longitude (deg.) West if LonEW=0, East if LonEW=1'/      &
      '   5    Time from start (Earth seconds)'/                        &
      '   6    Time from start (Martian Sols)'/                         &
      '   7    Areocentric Longitude of Sun, Ls (deg.)'/                &
      '   8    Local Solar Time (Mars hours = 1/24th sol)'/             &
      '   9    Pressure (mb)'/                                          &
      '  10    Pressure Height [-log(Pres/PresSurf) = -log(sigma)]'/    &
      '  11    Sigma coordinate [sigma=Pressure/(Surface Pressure)]'/   &
      '  12    Height (km) above reference ellipsoid'//                 &
      ' Use y-code = 0 for plotable output vs x-code variable only')
      IF(NVARX.LT.1.OR.NVARX.GT.12)then
        Write(iu0,381)
  381   Format(' x-code or y-code input error.')
        Write(iu0,380)
        Goto 9998
      Endif
      IF(NVARY.lt.0.or.NVARY.gt.12)then
        Write(iu0,381)
        Write(iu0,380)
        Goto 9998
      Endif
      If (logscale .lt. 0 .or. logscale .gt. 3 )logscale = 0
!!...  Initialize position data
      CHGT = FHGT
      CLAT = FLAT
      CLON = FLON
      CSEC = 0.
!!      Write(iu0,*)' Finished Setup - Starting computations'
      Return
 9998 Stop ' Error termination!! Check the LIST file for messages.'
    END Subroutine  Setup_2001
!!----------------------------------------------------------------------
    Subroutine Randinit_2001(J,NR1,RHOd,RHOu,RHOv)

!!      Subroutine Randinit_2001(J,NR1,RHOd,RHOu,RHOv,iup,iustdout)
      integer :: j, NR1
! Common RANDCOM_2001
      integer :: IX, IY, IZ
      COMMON /RANDCOM_2001/IX,IY,IZ
!!...  Re-initialize NR1, e.g. by reading from a file, or some algorithm
!!     from previous NR1 value, or by some computation on index J.
!!     Note that it is not necessary to randomly select each seed value
!!     NR1 in order to get a random sequence of output.  Any regular
!!     progression of selected NR1 values will do for this process.
      NR1 = NR1 + 11*J
      If (NR1.gt.30000)NR1 = NR1 - 30000
!!...  Write random seed value to list file
!!      If (iup.ne.0)Then
!!        Write(iup,10)J,NR1
!!      Else
!!        Write(iustdout,10)J,NR1
!!      Endif
!!  10  Format('   Random seed number',I6,' =',I6)
      IX = NR1
      IY = 172 * Mod(IX, 176) - 35 * (IX / 176)
      IZ = 170 * Mod(IX, 178) - 63 * (IX / 178)
      If (IY .lt. 0) IY = IY + 30307
      If (IZ .lt. 0) IZ = IZ + 30323
      RHOd = RANDOM_2001(L)
      RHOd = ppnd_2001(RHOd,L)
      RHOu = RANDOM_2001(L)
      RHOu = ppnd_2001(RHOu,L)
      RHOv = RANDOM_2001(L)
      RHOv = ppnd_2001(RHOv,L)
      Return
    End Subroutine Randinit_2001
!!----------------------------------------------------------------------

  end module cps_atm_gram
