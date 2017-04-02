module cps_atm_msis86_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_msis86_mod
!
!$Resume
! Modele d'atmosphere MSIS86.
!
!$Description
! Modele d'atmosphere MSIS86.
!
!$Auteur
! Julien bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: cps_atm_msis86.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atm_msis86.F90,v $
!  Revision 1.8  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.7  2009/01/28 11:04:09  cml
!  AQ : Correction du cartouche pour l argument AP
!
!  Revision 1.6  2008/10/14 07:51:25  cml
!  DM-ID 1058 : Suppression cibles et variable inutiles, ajout d initialisations
!
!  Revision 1.5  2008/07/04 12:00:55  huec
!  DM-ID 1058 : Compatibilite G95, suppression de variables declarees en double
!
!  Revision 1.4  2007/11/05 12:40:40  jpi
!  DM-ID551 : coquilles (apostrophes, print inutiles)
!
!  Revision 1.3  2006/11/13 07:50:25  vivaresf
!  FA-ID 633 : rajout des comonn CSW_MSPRO et LPOLY_MSPRO dans globe5 et globe5l
!  - renommage des commons de l'extension _mspro en l'extension _compas_msis86 pour eviter les conflits ultérieurs
!  Revision 1.2  2006/05/12 12:05:56  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.1  2006/01/30 09:07:38  bouillaj
!  Creation a partir de la routine MSPRO mp_atm_msis86
!
!$FinHistorique
!
!$Usage
!  use cps_atm_msis86_mod
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_msis86
!- cps_IO_e_msis86
!- CPS_IUCALD
!- cps_data_msis86
!
!$Fonctions
!- CPS_CCOR
!- CPS_DENSS
!- CPS_DERIV1
!- CPS_DERIV2
!- CPS_DNET
!- CPS_GLOB5L
!- CPS_GLOBE5
!- CPS_IUJJUL
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  CPS_CCOR CPS_DENSS CPS_DERIV1 CPS_DERIV2 CPS_DNET CPS_GLOB5L CPS_GLOBE5 CPS_IUJJUL cps_atm_msis86
!.  cps_IO_e_msis86 CPS_IUCALD cps_data_msis86
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_msis86.F90 69 2012-09-11 08:33:34Z ffsm $'


contains 
  
  subroutine cps_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
       dens,code_retour,temp,pres,inv_haut_ech)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_msis86
!
!$Resume
! Ce modele d'atmosphere est limite a des:
!              * 90km  < altitudes 
!
!$Description
! Ce modele d'atmosphere est limite a des:
!              * 90km  < altitudes 
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atm_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
!.           dens,code_retour,[temp],[pres],[inv_haut_ech])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_3rot 
!.    real(pm_reel), dimension(7) :: tab_ap 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: heure_sol 
!.    real(pm_reel) :: dens 
!.    type(tm_code_retour) :: code_retour
!.    real(pm_reel) :: temp 
!.    real(pm_reel) :: pres 
!.    real(pm_reel) :: inv_haut_ech 
!
!$Arguments
!>E     date          :<tm_jour_sec>       date julienne 1950 (JJ)
!>E     flux_veille   :<pm_reel>           flux solaire du jour précédent
!>E     flux_3rot     :<pm_reel>           flux solaire moyen sur les 3 
!                                          dernieres rotations solaires
!>E     tab_ap        :<pm_reel,DIM=(7)>   évolution de l'activité 
!                                          géomagnetique (60 h précédentes)
!>E     lat           :<pm_reel>           latitude géodésique (rad)
!>E     long          :<pm_reel>           longitude géodésique (rad)
!>E     alt           :<pm_reel>           altitude geodesique (m)
!>E     heure_sol     :<pm_reel>           heure solaire locale (rad)
!>S     dens          :<pm_reel>           densité atmosphérique (kg.m-3)
!>S     code_retour   :<tm_code_retour>    
!>[S]   temp          :<pm_reel>           température (Kelvin)
!>[S]   pres          :<pm_reel>           pression (Pa)
!>[S]   inv_haut_ech  :<pm_reel>           inverse de la hauteur d'échelle (m)
!
!$Common
!
!$Routines
!- cps_IO_e_msis86
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Modules
! =======
use mslib


! Declarations
! ============
implicit none

type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 
                                                        ! dernieres rotations solaires
real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
                                                        ! geomagnetique (60 h precedentes)
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: long         ! longitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: temp         ! temperature
real(pm_reel), intent(out), optional    :: pres         ! pression
real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle


! Autres declarations
! ===================

! Declarations pour l'appel a la routine COMPAS cps_IO_e_msis86
integer               :: JJ                ! date en JJ CNES
real(pm_reel)               :: SEC                  ! secondes dans le jour
integer                     :: retour            ! code retour
real(pm_reel)               :: F,FBAR            ! flux solaire du jour prec, et moyen sur trois mois
real(pm_reel)               :: ALTM,ALAT,ALONG   ! altitude geodesique, lat geodesique, longitude
real(pm_reel)               :: STL               ! heure solaire locale
real(pm_reel)               :: RHO               ! masse volumique a l'endroit considere
real(pm_reel)               :: RTEMP,RPRES,DM    ! temperature, pression, masse molaire moyenne
real(pm_reel)               :: ECH               ! hauteur d'echelle
real(pm_reel), dimension(7) :: AP                ! tableau de l'activite geomagnetique

real(pm_reel) :: alt_min_MSIS86 = 86.

intrinsic present

!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK


! verification des arguments d'entree
! ===================================

if ((flux_veille < 0._pm_reel) .OR. (flux_3rot < 0._pm_reel)) then ! flux solaire negatif

!   code_retour%valeur = pm_err_flux_sol_negatif
   go to 6000

end if

if (alt < alt_min_MSIS86) then ! altitude inferieure a 90 km

!   code_retour%valeur = pm_err_alt_inf90km
   go to 6000

end if

! pas de normalisation de date faites ici 
! compte tenu de l'utilisation jusqu'a present de mpi_IO_e_msis86
if (date%jour < 0) then

!   code_retour%valeur = pm_err_jul1950_negatif
   go to 6000

end if

if (date%jour > 54787) then ! 54787 = 01/01/2100 a 00:00:00

!   code_retour%valeur = pm_err_jul1950_sup2099
   go to 6000

end if



! calcul du modele atmospherique MSIS 86
! ======================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_IO_e_msis86


JJ = date%jour
SEC = date%sec
F = flux_veille
FBAR = flux_3rot
AP (1:7) = tab_ap(1:7)
ALTM = alt
ALAT = lat
ALONG = long
STL = heure_sol

call cps_IO_e_msis86 (JJ,SEC,F,FBAR,AP,ALTM,ALAT,ALONG,STL,RHO,RTEMP,RPRES,DM,ECH,retour)


if (retour /= pm_OK) then

! les valeurs non nulles possibles pour le code retour de mpi_IO_e_msis86 sont 
! normalement deja traitees avant l'appel

   if (retour > pm_OK) then ! warning
!      code_retour%valeur = pm_warn_IOLIB
   else ! retour < pm_OK : erreur
!      code_retour%valeur = pm_err_IOLIB
      go to 6000 ! pas d'affectation des sorties
   end if

end if

! affectation des sorties
! =======================

dens = RHO

if (present(temp)) temp = RTEMP
if (present(pres)) pres = RPRES
if (present(inv_haut_ech)) inv_haut_ech = ECH



6000 continue

!code_retour%routine = pm_num_mp_atm_msis86
!code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine cps_atm_msis86

SUBROUTINE cps_IO_e_msis86 (JJ,SEC,F,FBAR,AP,ALTM,ALAT,ALONG,STL, &
                              RHO,TEMP,PRES,DM,ECH,IER)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_IO_e_msis86
!
!$Resume
!     Modèle d atmosphère MSIS 86.
!
!$Description
!     Modèle d atmosphère MSIS 86.
!
!$Auteur
!     CH. VALORGE, R. MESNARD
!
!$Version
!     21/11/90
!
!$Usage
!  call cps_IO_e_msis86 (JJ,SEC,F,FBAR,AP,ALTM,ALAT,ALONG,STL, &
!.                                  RHO,TEMP,PRES,DM,ECH,IER)
!.    integer :: JJ
!.    real(kind=pm_reel) :: sec, f, fbar, altm, alat, along, stl
!.    real(kind=pm_reel), dimension(7) :: ap
!.    real(kind=pm_reel) :: rho, temp, pres, dm, ech
!.    integer :: ier
!
!$Remarques
!     Ancien nom: mmsis86
!     Ce sous-programme utilise les sous-programmes internes: \
!     CPS_PRMSG5,CPS_IUCALD,CPS_IUJJUL,CPS_IUCALD,CPS_CCOR,CPS_DENSS,CPS_DERIV1,CPS_DERIV2,CPS_DNET,CPS_GLOB5L,CPS_GLOBE5
!     De plus, il est nécessaire d avoir le répertoire (par défaut data_msis/) contenant \
!     le fichier data_msis86.
!
!$Arguments
!>E     JJ     :<integer>           date (JJ)
!>E     SEC    :<pm_reel>           seconde dans le jour (tai)
!>E     F      :<pm_reel>           flux solaire du jour précedent
!>E     FBAR   :<pm_reel>           flux solaire moyen sur 3 mois
!>E     AP     :<pm_reel,DIM=(7)>   tableau de l'activité géomagnetique contenant :
!.                   (1) ap journalier
!.                   (2) ap tri-horaire courant
!.                   (3) ap tri-horaire des 3h précédentes
!.                   (4) ap tri-horaire des 6h précédentes
!.                   (5) ap tri-horaire des 9h précédentes
!.                   (6) moyenne des 8 ap tri-horaires de 12 a 33h précédentes
!.                   (7) moyenne des 8 ap tri-horaires de 36 a 60h précédentes
!>E     ALTM   :<pm_reel>           altitude géodesique (supérieure a 85 km) (m)
!>E     ALAT   :<pm_reel>           latitude géodesique (rad)
!>E     ALONG  :<pm_reel>           longitude (rad)
!>E     STL    :<pm_reel>           heure solaire locale (rad)
!>S     RHO    :<pm_reel>           masse volumique à l endroit considéré (KG/M**3)
!>S     TEMP   :<pm_reel>           température à l endroit considéré (K)
!>S     PRES   :<pm_reel>           pression (PA)
!>S     DM     :<pm_reel>           masse molaire moyenne (kg/mol)
!>S     ECH    :<pm_reel>           hauteur d échelle : -1/RHO*(D(RHO)/DZ) (m-1)
!>S     IER    :<integer>           code retour (0=OK)
!
!$<>
!*******************************************************************************
use mslib

implicit none

      integer, intent(in) :: JJ
      real(kind=pm_reel), intent(in) :: sec, f, fbar, altm, alat, along, stl
      real(kind=pm_reel), dimension(7), intent(in) ::ap
      real(kind=pm_reel), intent(out) :: rho, temp, pres, dm, ech
      integer, intent(out) :: ier


!      integer :: CPS_IUJJUL
!      real (KIND=pm_reel) :: CPS_GLOBE5,CPS_GLOB5L,CPS_DENSS,CPS_DNET
!      real (KIND=pm_reel) :: CPS_DERIV1,CPS_DERIV2,CPS_CCOR
      real(kind=pm_reel) :: numj
      integer       ::  nj, nm, na
      real(kind=pm_reel), dimension(8) :: d, altl

      real(kind=pm_reel), PARAMETER :: PI = 0.31415926535898D+01


! ---
      real(kind=pm_reel) :: s, tinf, zlb, tlb, z0, t0, za, tr12

      COMMON/GTS3C_COMPAS_MSIS86/S,TINF,ZLB,TLB,Z0,T0,ZA,TR12

!     S      = GRADIENT VERTICAL DE LA TEMPERATURE (K/M)
!     TINF   = TEMPERATURE EXOSPHERIQUE (K)
!     ZLB    = ALTITUDE DE BASE (120 KM POUR MSIS86)
!     TLB    = TEMPERATURE A L'ALTITUDE DE BASE (K)
!     Z0     = ALTITUDE DE LA MESOPAUSE (KM)
!     T0     = TEMPERATURE A L'ALTITUDE DE LA MESOPAUSE (K)
!     ZA     = ALTITUDE DE JONCTION DES PROFILS DE TEMPERATURE (KM)
!     TR12   = PARAMETRE DE FORME DE LA MESOPAUSE


      real(kind=pm_reel), dimension(8) :: ptm
      real(kind=pm_reel), dimension(8,7) ::pdm
      COMMON/LOWER5_COMPAS_MSIS86/PTM,PDM
      
      real(kind=pm_reel), dimension(150) :: pt, ps
      real(kind=pm_reel), dimension(150,7) :: pd
      real(kind=pm_reel), dimension(25,2) :: pdl
      COMMON/PARM5_COMPAS_MSIS86/PT,PD,PS,PDL

      real(kind=pm_reel), dimension(25) ::sw, swc
      COMMON/CSW_COMPAS_MSIS86/SW,SWC

      real(kind=pm_reel) :: usavog, glat, glong, alt, g0, xmm, g28, db28, dd, zhm28
      real(kind=pm_reel) :: zh28, xmd, b28, dm28, dd28, g4, db04, zh04, b04, dm04, zhm04, rl
      real(kind=pm_reel) :: zc04, hc04, dd04, g16, db16, zh16, b16, dm16, zhm16
      real(kind=pm_reel) :: hc16, zc16, hcc16, zcc16, rc16, dd16, g32, db32, zh32, b32
      real(kind=pm_reel) :: dm32, zhm32, hc32, zc32, dd32, g40, db40, zh40, b40, dm40
      real(kind=pm_reel) :: zhm40, hc40, zc40, dd40, g1, db01, zh01, b01, dm01, zhm01
      real(kind=pm_reel) :: hc01, zc01, hcc01, zcc01, rc01, dd01, g14, db14, zh14, b14, dm14, zhm14
      real(kind=pm_reel) :: hc14, zc14, hcc14, zcc14, rc14, dd14, dn, ddm

! ---

      DATA ALTL/200.D0,400.D0,150.D0,200.D0,240.D0,450.D0,320.D0,450.D0/
!     ALTITUDES LIMITES DES REACTIONS CHIMIQUES DES ELEMENTS

      DATA USAVOG/1.66D-24/
!     USAVOG : INVERSE DU NOMBRE D'AVOGADRO UTILISE POUR MSIS

      integer :: INDLEC = 0

!***************************************************
!*    LECTURE DU FICHIER DE DONNEES DU MODELE MSIS86
!*    LORS DU 1ER APPEL A MMSI86 ( SI INDLEC=0)
!***************************************************
      IF (INDLEC.EQ.0) THEN
      CALL cps_data_msis86(IER)
!$$$      CALL CPS_PRMSG5(IER)
           IF (IER.NE.0) THEN
           RETURN
           ENDIF
      INDLEC=1
      ENDIF

!**********************************************************************
!*FON PASSAGE DU JOUR JULIEN AU NUMERO DE JOUR DANS L'ANNEE
!**********************************************************************

      CALL CPS_IUCALD(JJ,NJ,NM,NA,IER)
           IF (IER.NE.0) THEN
           RETURN
           ENDIF

      NUMJ=JJ-CPS_IUJJUL(1,1,NA,IER)+1
           IF (IER.NE.0) THEN
           RETURN
           ENDIF

!**********************************************************************
!*FON PASSAGE DES LATITUDE ET LONGITUDE DE RADIANS A DEGRES
!**********************************************************************

      GLAT  = ALAT  * 18.D1 / PI
      GLONG = ALONG * 18.D1 / PI

!**********************************************************************
!*FON PASSAGE DE L'ALTITUDE DE M A KM
!**********************************************************************

      ALT = ALTM / 1.D3

!**********************************************************************
!*FON CALCUL DES TEMPERATURES A UTILISER
!**********************************************************************

!       Eq. A7 : TEMPERATURE EXOSPHERIQUE
      TINF=PTM(1)*(1.D0+SW(16)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG, &
       STL,FBAR,F, &
       AP,PT))*PT(1)

!       ALTITUDE DE JONCTION DES PROFILS DE TEMPERATURE
      ZA=PTM(5)*PDL(16,2)

!       Eq. A9 : TEMPERATURE DE LA MESOPAUSE
      T0=PTM(3)*PD(76,3)*(1.D0+SW(18)*CPS_GLOB5L(PD(76,3)))

!       Eq. A8 : TEMPERATURE A 120 KM
      ZLB=120.D0
      TLB=PTM(2)*(1.+SW(17)*CPS_GLOB5L(PD(26,3)))*PD(26,3)

!       Eq. A10 : HAUTEUR DE LA MESOPAUSE
      Z0=PTM(7)*(1.+SW(20)*CPS_GLOB5L(PD(51,3)))*PD(51,3)

!       Eq. A6 : GRADIENT DE TEMPERATURE A 120 KM
      G0=PTM(4)*PS(1) &
       *(1.D0+SW(19)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PS))

!       Eq. A5 : GRADIENT VERTICAL DE LA TEMPERATURE
      S=G0/(TINF-TLB)

!       Eq. A11 : PARAMETRE DE FORME DE LA MESOPAUSE
      TR12=PD(101,3)*(1.D0+SW(22)*CPS_GLOB5L(PD(101,3)))

!       Eq. A1  : TEMPERATURE AMBIANTE
      TEMP=CPS_DENSS(ALT,1.D0,0.D0,1.D0)

!       MASSE MOLAIRE MOLECULAIRE MOYENNE
      XMM=PDM(5,3)

!***********************************************************************
!*FON DENSITE DE L'AZOTE DI-ATOMIQUE N2
!***********************************************************************

!       Eq. A18
      G28=SW(21)*CPS_GLOB5L(PD(1,3))
      DB28 = PDM(1,3)*DEXP(G28)*PD(1,3)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(3)=CPS_DENSS(ALT,DB28,28.D0,0.D0)
      DD=D(3)

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZHM28=PDM(4,3)*PDL(6,2)
      ZH28=PDM(3,3)
      XMD=28.D0-XMM
      B28=CPS_DENSS(ZH28,DB28,XMD,-1.D0)
      IF(ALT.GT.ALTL(3).OR.SW(15).EQ.(0.D0)) GO TO 17
      DM28=CPS_DENSS(ALT,B28,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(3)=CPS_DNET(D(3),DM28,ZHM28,XMM,28.D0)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD28 = D(3) * CPS_DERIV1(ALT,28.D0,XMM,ZHM28,0.D0,DD,DM28)
      GO TO 18

   17 CONTINUE
      DD28 = D(3) * CPS_DERIV1(ALT,28.D0,XMM,ZHM28,0.D0,DD,0.D0)

   18 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'HELIUM He
!***********************************************************************

!       Eq. A18
      G4 = SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,1))
      DB04 = PDM(1,1)*DEXP(G4)*PD(1,1)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(1)=CPS_DENSS(ALT,DB04,4.D0,-.4D0)
      DD=D(1)
      IF(ALT.GT.ALTL(1).OR.SW(15).EQ.(0.D0)) GO TO 24

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH04=PDM(3,1)
      B04=CPS_DENSS(ZH04,DB04,4.D0-XMM,-1.4D0)
      DM04=CPS_DENSS(ALT,B04,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM04=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(1)=CPS_DNET(D(1),DM04,ZHM04,XMM,4.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,1)/B04)
!       Eq. A20a
      ZC04=PDM(5,1)*PDL(1,2)
      HC04=PDM(6,1)*PDL(2,2)

      D(1)=D(1)*CPS_CCOR(ALT,RL,HC04,ZC04)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD04 = D(1) * ( CPS_DERIV1(ALT,4.D0,XMM,ZHM28,-.4D0,DD,DM04) &
                    + CPS_DERIV2(ALT,RL,HC04,ZC04) )
      GO TO 25

   24 CONTINUE
      DD04 = D(1) * CPS_DERIV1(ALT,4.D0,XMM,ZHM28,-.4D0,DD,0.D0)

   25 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'OXYGENE MONO-ATOMIQUE O
!***********************************************************************

!       Eq. A18
      G16= SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,2))
      DB16 =  PDM(1,2)*DEXP(G16)*PD(1,2)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(2)=CPS_DENSS(ALT,DB16,16.D0,0.D0)
      DD=D(2)
      IF(ALT.GT.ALTL(2).OR.SW(15).EQ.(0.D0)) GO TO 34

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH16=PDM(3,2)
      B16=CPS_DENSS(ZH16,DB16,16.D0-XMM,-1.D0)
      DM16=CPS_DENSS(ALT,B16,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM16=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(2)=CPS_DNET(D(2),DM16,ZHM16,XMM,16.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,2)*DABS(PDL(17,2))/B16)
!       Eq. A20a
      HC16=PDM(6,2)*PDL(4,2)
      ZC16=PDM(5,2)*PDL(3,2)
      D(2)=D(2)*CPS_CCOR(ALT,RL,HC16,ZC16)
!       Eq. A21
      HCC16=PDM(8,2)*PDL(14,2)
      ZCC16=PDM(7,2)*PDL(13,2)
      RC16=PDM(4,2)*PDL(15,2)

      D(2)=D(2)*CPS_CCOR(ALT,RC16,HCC16,ZCC16)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD16 = D(2) * ( CPS_DERIV1(ALT,16.D0,XMM,ZHM28,0.D0,DD,DM16) &
                    + CPS_DERIV2(ALT,RL,HC16,ZC16) &
                    + CPS_DERIV2(ALT,RC16,HCC16,ZCC16) )
      GO TO 35

   34 CONTINUE
      DD16 = D(2) * CPS_DERIV1(ALT,16.D0,XMM,ZHM28,0.D0,DD,0.D0)

   35 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'OXYGENE DI-ATOMIQUE O2
!***********************************************************************

!       Eq. A18
      G32= SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,4))
      DB32 = PDM(1,4)*DEXP(G32)*PD(1,4)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(4)=CPS_DENSS(ALT,DB32,32.D0,0.D0)
      DD=D(4)
      IF(ALT.GT.ALTL(4).OR.SW(15).EQ.(0.D0)) GO TO 39

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH32=PDM(3,4)
      B32=CPS_DENSS(ZH32,DB32,32.D0-XMM,-1.D0)
      DM32=CPS_DENSS(ALT,B32,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM32=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(4)=CPS_DNET(D(4),DM32,ZHM32,XMM,32.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,4)/B32)
!       Eq. A20a
      HC32=PDM(6,4)*PDL(8,2)
      ZC32=PDM(5,4)*PDL(7,2)

      D(4)=D(4)*CPS_CCOR(ALT,RL,HC32,ZC32)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD32 = D(4) * ( CPS_DERIV1(ALT,32.D0,XMM,ZHM28,0.D0,DD,DM32) &
                    + CPS_DERIV2(ALT,RL,HC32,ZC32) )
      GO TO 40

   39 CONTINUE
      DD32 = D(4) * CPS_DERIV1(ALT,32.D0,XMM,ZHM28,0.D0,DD,0.D0)

   40 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'ARGON Ar
!***********************************************************************

!       Eq. A18
      G40= SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,5))
      DB40 = PDM(1,5)*DEXP(G40)*PD(1,5)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(5)=CPS_DENSS(ALT,DB40,40.D0,0.D0)
      DD=D(5)
      IF(ALT.GT.ALTL(5).OR.SW(15).EQ.(0.D0)) GO TO 44

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH40=PDM(3,5)
      B40=CPS_DENSS(ZH40,DB40,40.D0-XMM,-1.D0)
      DM40=CPS_DENSS(ALT,B40,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM40=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(5)=CPS_DNET(D(5),DM40,ZHM40,XMM,40.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,5)/B40)
!       Eq. A20a
      HC40=PDM(6,5)*PDL(10,2)
      ZC40=PDM(5,5)*PDL(9,2)

      D(5)=D(5)*CPS_CCOR(ALT,RL,HC40,ZC40)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD40 = D(5) * ( CPS_DERIV1(ALT,40.D0,XMM,ZHM28,0.D0,DD,DM40) &
                    + CPS_DERIV2(ALT,RL,HC40,ZC40) )
      GO TO 45

   44 CONTINUE
      DD40 = D(5) * CPS_DERIV1(ALT,40.D0,XMM,ZHM28,0.D0,DD,0.D0)

   45 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'HYDROGENE MONO-ATOMIQUE H
!***********************************************************************

!       Eq. A18
      G1 = SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,6))
      DB01 = PDM(1,6)*DEXP(G1)*PD(1,6)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(7)=CPS_DENSS(ALT,DB01,1.D0,-.4D0)
      DD=D(7)
      IF(ALT.GT.ALTL(7).OR.SW(15).EQ.(0.D0)) GO TO 47

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH01=PDM(3,6)
      B01=CPS_DENSS(ZH01,DB01,1.D0-XMM,-1.4D0)
      DM01=CPS_DENSS(ALT,B01,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM01=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(7)=CPS_DNET(D(7),DM01,ZHM01,XMM,1.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,6)*DABS(PDL(18,2))/B01)
!       Eq. A20a
      HC01=PDM(6,6)*PDL(12,2)
      ZC01=PDM(5,6)*PDL(11,2)

      D(7)=D(7)*CPS_CCOR(ALT,RL,HC01,ZC01)
!       Eq. A21
      HCC01=PDM(8,6)*PDL(20,2)
      ZCC01=PDM(7,6)*PDL(19,2)
      RC01=PDM(4,6)*PDL(21,2)

      D(7)=D(7)*CPS_CCOR(ALT,RC01,HCC01,ZCC01)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD01 = D(7) * ( CPS_DERIV1(ALT,1.D0,XMM,ZHM28,-.4D0,DD,DM01) &
                    + CPS_DERIV2(ALT,RL,HC01,ZC01) &
                    + CPS_DERIV2(ALT,RC01,HCC01,ZCC01) )
      GO TO 48

   47 CONTINUE
      DD01 = D(7) * CPS_DERIV1(ALT,1.D0,XMM,ZHM28,-.4D0,DD,0.D0)

   48 CONTINUE

!***********************************************************************
!*FON DENSITE DE L'AZOTE MONO-ATOMIQUE N
!***********************************************************************

!       Eq. A18
      G14 = SW(21)*CPS_GLOBE5(NUMJ,SEC,GLAT,GLONG,STL,FBAR,F,AP,PD(1,7))
      DB14 = PDM(1,7)*DEXP(G14)*PD(1,7)

!       Eq. A13 - A17 : PROFIL DIFFUSIF
      D(8)=CPS_DENSS(ALT,DB14,14.D0,0.D0)
      DD=D(8)
      IF(ALT.GT.ALTL(8).OR.SW(15).EQ.(0.D0)) GO TO 49

!       Eq. A19 : PROFIL DE TURBOPAUSE
      ZH14=PDM(3,7)
      B14=CPS_DENSS(ZH14,DB14,14.D0-XMM,-1.D0)
      DM14=CPS_DENSS(ALT,B14,XMM,0.D0)

!       Eq. A12 : DENSITE TOTALE
      ZHM14=ZHM28

! **** Rq : L'ARTICLE DECRIVANT LE MODELE DONNE UNE VALEUR DE ZHM28
! **** EGALE A 28 G/MOL (Eq. A12b) ALORS QU'IL VAUT ICI 28.95 G/MOL !!

      D(8)=CPS_DNET(D(8),DM14,ZHM14,XMM,14.D0)

!       COEFFICIENTS CORRECTEURS DUS A LA DISSOCIATION
!       Eq. A20b
      RL=DLOG(B28*PDM(2,7)*DABS(PDL(3,1))/B14)
!       Eq. A20a
      HC14=PDM(6,7)*PDL(2,1)
      ZC14=PDM(5,7)*PDL(1,1)

      D(8)=D(8)*CPS_CCOR(ALT,RL,HC14,ZC14)
!       Eq. A21
      HCC14=PDM(8,7)*PDL(5,1)
      ZCC14=PDM(7,7)*PDL(4,1)
      RC14=PDM(4,7)*PDL(6,1)

      D(8)=D(8)*CPS_CCOR(ALT,RC14,HCC14,ZCC14)

!       DERIVEE DE LA DENSITE PAR RAPPORT A L'ALTITUDE

      DD14 = D(8) * ( CPS_DERIV1(ALT,14.D0,XMM,ZHM28,0.D0,DD,DM14) &
                    + CPS_DERIV2(ALT,RL,HC14,ZC14) &
                    + CPS_DERIV2(ALT,RC14,HCC14,ZCC14) )
      GO TO 50

   49 CONTINUE
      DD14 = D(8) * CPS_DERIV1(ALT,14.D0,XMM,ZHM28,0.D0,DD,0.D0)

   50 CONTINUE

!***********************************************************************
!*FON DENSITE TOTALE , MASSE VOLUMIQUE ET HAUTEUR D'ECHELLE
!***********************************************************************

      DN  = D(1)+D(2)+D(3)+D(4)+D(5)+D(7)+D(8)
      DM  = 4.D0*D(1)+16.D0*D(2)+28.D0*D(3)+32.D0*D(4)+ &
      40.D0*D(5)+D(7)+14.D0*D(8)
      DDM = 4.D0*DD04+16.D0*DD16+28.D0*DD28+32.D0*DD32+ &
      40.D0*DD40+DD01+14.D0*DD14
      RHO = USAVOG * DM * 1.D3
      ECH = 1.D-3 * DDM / DM
      DM  = 1.D-3 * DM / DN

!     RSTAR = 8314.32 J/(KMOL-K) (CONSTANTE DES GAZ PARFAITS)
      PRES = RHO*8314.32D0*TEMP/(DM*1000.d0)

      RETURN
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
    END subroutine  cps_IO_e_msis86

      real(kind=pm_reel) FUNCTION  CPS_CCOR(ALT,R,H1,ZH)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_CCOR
!
!$Resume
! CALCULE LE COEF CORRECTEUR DE LA DENSITE DE L'ELEMENT CONSIDERE,
! DU AUX PHENOMENES CHIMIQUES DE DISSOCIATION (Eq. A20a OU A21 DU
! MODELE D'ATMOSPHERE MSIS-86)
!
!$Description
! CALCULE LE COEF CORRECTEUR DE LA DENSITE DE L'ELEMENT CONSIDERE,
! DU AUX PHENOMENES CHIMIQUES DE DISSOCIATION (Eq. A20a OU A21 DU
! MODELE D'ATMOSPHERE MSIS-86)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION  CPS_CCOR(ALT,R,H1,ZH)
!.    real(kind=pm_reel) :: alt, r, h1, zh
!
!$Arguments
!>E     ALT  :<pm_reel>   ALTITUDE (KM)
!>E     R    :<pm_reel>   PARAMETRE DE CORRECTION DE DENSITE DE L'ELEMENT CONSIDERE
!>E     H1   :<pm_reel>   HAUTEUR D'ECHELLE DE LA CORRECTION DE L'ELEMENT CONSIDERE
!>E     ZH   :<pm_reel>   ALTITUDE A LAQUELLE LE COEF CORRECTEUR VAUT EXP(R/2)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none

        real(kind=pm_reel), intent(in) :: alt, r, h1, zh

        real(kind=pm_reel) :: E, ex

      E=(ALT-ZH)/H1

      IF(E.GT.70.D0) GO TO 20
      IF(E.LT.-70.D0) GO TO 10

      EX=DEXP(E)
      CPS_CCOR=R/(1.D0+EX)
      GO TO 50

   10 CPS_CCOR=R
      GO TO 50

   20 CPS_CCOR=0.D0
      GO TO 50

   50 CONTINUE
      CPS_CCOR=DEXP(CPS_CCOR)

      RETURN
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
    END function   CPS_CCOR

      real(kind=pm_reel) FUNCTION CPS_DENSS(ALT,DLB,XM,ALPHA)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_DENSS
!
!$Resume
! CALCUL DES PROFILS DE TEMPERATURE ET DE DENSITE DU MODELE MSIS86
! LA FONCTION RENVOIE LA DENSITE SI XM<>0, LA TEMPERATURE SINON
!
!$Description
! CALCUL DES PROFILS DE TEMPERATURE ET DE DENSITE DU MODELE MSIS86
! LA FONCTION RENVOIE LA DENSITE SI XM<>0, LA TEMPERATURE SINON
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_DENSS(ALT,DLB,XM,ALPHA)
!.    real(kind=pm_reel) :: alt, dlb, xm, alpha
!
!$Arguments
!>E     ALT    :<pm_reel>   ALTITUDE (KM)
!>E     DLB    :<pm_reel>   DENSITE DE L'ELEMENT A 120 KM (CM-3)
!>E     XM     :<pm_reel>   MASSE MOLAIRE DE L'ELEMENT (G/MOL)
!>E     ALPHA  :<pm_reel>   COEFFICIENT DE DIFFUSION THERMIQUE DE L'ELEMENT
!
!$Common
!#V
!- common /GTS3C_COMPAS_MSIS86/
!#
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none

        real(kind=pm_reel), intent(in) :: alt, dlb, xm, alpha

        real(kind=pm_reel) :: s, tinf, zlb, tlb, z0, t0, za, tr12, rgas
        real(kind=pm_reel) :: gsurf, rp, zz, zl
        real(kind=pm_reel) :: z, zg2, tt, ta, tz, zg0, dta, t12, zg1, dd, cc, bb, x, x2
        real(kind=pm_reel) :: glb, gamma, densa, gamm, zeta

      COMMON/GTS3C_COMPAS_MSIS86/S,TINF,ZLB,TLB,Z0,T0,ZA,TR12

!     S      = GRADIENT VERTICAL DE LA TEMPERATURE (K/M)
!     TINF   = TEMPERATURE EXOSPHERIQUE (K)
!     ZLB    = ALTITUDE DE BASE (120 KM POUR MSIS86)
!     TLB    = TEMPERATURE A L'ALTITUDE DE BASE (K)
!     Z0     = ALTITUDE DE LA MESOPAUSE (KM)
!     T0     = TEMPERATURE A L'ALTITUDE DE LA MESOPAUSE (K)
!     ZA     = ALTITUDE DE JONCTION DES PROFILS DE TEMPERATURE (KM)
!     TR12   = PARAMETRE DE FORME DE LA MESOPAUSE

! ---

      DATA RGAS/8.314D0/
!     RGAS : CONSTANTE DES GAZ PARFAITS

      DATA GSURF/9.80665D0/
!     GSURF : ACCELERATION DE LA PESANTEUR A LA SURFACE TERRESTRE

      DATA RP/6356.77D0/
!     RP : RAYON TERRESTRE AU POLE

!***********************************************************************
!*FON FONCTION STATIQUE DZETA
!***********************************************************************

      ZETA(ZZ,ZL)=(ZZ-ZL)*(RP+ZL)/(RP+ZZ)

!***********************************************************************
!*FON CALCUL DE LA TEMPERATURE POUR ALT>ZA
!***********************************************************************

!     Initialisations supplémentaires pour suppression des warning
      x2 = 0._pm_reel
      x = 0._pm_reel
      bb = 0._pm_reel
      cc = 0._pm_reel
      dd = 0._pm_reel
      zg0 = 0._pm_reel

      CPS_DENSS=1._pm_reel

      Z=DMAX1(ALT,ZA)
!      Eq. A4a
      ZG2=ZETA(Z,ZLB)
!      Eq. A1a
      TT=TINF-(TINF-TLB)*DEXP(-S*ZG2)
      TA=TT
      TZ=TT
      CPS_DENSS=TZ

!***********************************************************************
!*FON CALCUL DE LA TEMPERATURE POUR ALT<ZA
!***********************************************************************

      IF(ALT.GE.ZA) GO TO 10
!      Eq. A4b
      ZG0=ZETA(Z0,ZA)
!      Eq. A2b
      DTA=(TINF-TA)*S*((RP+ZLB)/(RP+ZA))**2
!      Eq. A3e
      T12=T0+TR12*(TA-T0)
!      Eq. A4b
      ZG1=ZETA(ALT,ZA)
!      Eq. A3a
      DD=0.666666D0*ZG0*DTA/TA**2 - 3.11111D0*(1.D0/TA-1.D0/T0)+ &
       7.11111D0*(1.D0/T12-1.D0/T0)
!      Eq. A3b
      CC=ZG0*DTA/(2.D0*TA*TA) - (1.D0/TA-1.D0/T0) - 2.D0*DD
!      Eq. A3c
      BB=(1.D0/TA-1.D0/T0) - CC - DD
!      Eq. A3d
      X=(-(ZG1-ZG0)/ZG0)
!      Eq. A1b
      X2=X*X
      TZ=1.D0/(1.D0/T0+BB*X2+CC*X2*X2+DD*X2*X2*X2)
      CPS_DENSS=TZ

   10 IF(XM.EQ.(0.D0)) GO TO 50

!***********************************************************************
!*FON CALCUL DE LA DENSITE POUR ALT>ZA
!***********************************************************************

      IF (TA.LE.(0.D0) .AND. TZ.LE.(0.D0)) THEN
         TT=TLB
         TA=TLB
         TZ=TLB
      ENDIF
!      Eq. A17a
      GLB=GSURF/(1.D0+ZLB/RP)**2
!      Eq. A16a
      GAMMA=XM*GLB/(S*RGAS*TINF)
!      Eq. A13, A14a, & A15
      DENSA=DLB*(TLB/TT)**(1.D0+ALPHA+GAMMA)*DEXP(-S*GAMMA*ZG2)
      CPS_DENSS=DENSA

!***********************************************************************
!*FON CALCUL DE LA DENSITE POUR ALT<ZA
!***********************************************************************

      IF(ALT.GE.ZA) GO TO 50
!      Eq. A17b
      GLB=GSURF/(1.D0+ZA/RP)**2
!      Eq. A16b
      GAMM=XM*GLB*ZG0/RGAS
!      Eq. A13, A14b, & A15
      CPS_DENSS=DENSA*(TA/TZ)**(1.D0+ALPHA)* &
       DEXP(GAMM*((X-1)/T0+BB*(X*X2-1.D0)/3.D0+CC*(X2*X2*X-1.D0)/5.D0+ &
       DD*(X2*X2*X2*X-1.D0)/7.D0))

   50 CONTINUE

      RETURN 
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
      END function  CPS_DENSS
      
      real(kind=pm_reel) FUNCTION CPS_DERIV1(ALT,XM,XMM,ZHM,ALPHA,DND,DNM)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_DERIV1
!
!$Resume
! CALCUL DE LA DERIVEE DE LA DENSITE DE L'ELEMENT PAR RAPPORT A L'
! ALTITUDE (PREMIERE PARTIE : SANS DERIVEE DES COEF CORRECTEURS)
! REMARQUE : CE CALCUL EST VALABLE POUR ALT>ZA
!
!$Description
! CALCUL DE LA DERIVEE DE LA DENSITE DE L'ELEMENT PAR RAPPORT A L'
! ALTITUDE (PREMIERE PARTIE : SANS DERIVEE DES COEF CORRECTEURS)
! REMARQUE : CE CALCUL EST VALABLE POUR ALT>ZA
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_DERIV1(ALT,XM,XMM,ZHM,ALPHA,DND,DNM)
!.    real(kind=pm_reel) :: alt, xm, xmm, zhm, alpha, dnd, dnm
!
!$Arguments
!>E     ALT    :<pm_reel>   ALTITUDE (KM)
!>E     XM     :<pm_reel>   MASSE MOLAIRE DE L'ELEMENT (G/MOL)
!>E     XMM    :<pm_reel>   MASSE MOLAIRE MOYENNE (G/MOL)
!>E     ZHM    :<pm_reel>   MASSE MOLAIRE DE L'AZOTE (G/MOL)
!>E     ALPHA  :<pm_reel>   COEFFICIENT DE DIFFUSION THERMIQUE DE L'ELEMENT
!>E     DND    :<pm_reel>   DENSITE DE L'ELEMENT DUE A L'EQUILIBRE DIFFUSIF
!>E     DNM    :<pm_reel>   DENSITE DE L'ELEMENT DUE A L'EQUILIBRE DE TURBOPAUSE
!
!$Common
!#V
!- common /GTS3C_COMPAS_MSIS86/
!#
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none
        
        real(kind=pm_reel), intent(in) :: alt, xm, xmm, zhm, alpha, dnd, dnm
        real(kind=pm_reel) :: s, tinf, zlb, tlb, z0, t0, za, tr12
        real(kind=pm_reel) :: rgas, gsurf, rp, zeta, emsz, glb, gamma, petita
        real(kind=pm_reel) :: coef1, coef2, granda, rap, fact

      COMMON/GTS3C_COMPAS_MSIS86/S,TINF,ZLB,TLB,Z0,T0,ZA,TR12

!     S      = GRADIENT VERTICAL DE LA TEMPERATURE (K/M)
!     TINF   = TEMPERATURE EXOSPHERIQUE (K)
!     ZLB    = ALTITUDE DE BASE (120 KM POUR MSIS86)
!     TLB    = TEMPERATURE A L'ALTITUDE DE BASE (K)
!     Z0     = ALTITUDE DE LA MESOPAUSE (KM)
!     T0     = TEMPERATURE A L'ALTITUDE DE LA MESOPAUSE (K)
!     ZA     = ALTITUDE DE JONCTION DES PROFILS DE TEMPERATURE (KM)
!     TR12   = PARAMETRE DE FORME DE LA MESOPAUSE

! ---

      DATA RGAS/8.314D0/
!     RGAS : CONSTANTE DES GAZ PARFAITS

      DATA GSURF/9.80665D0/
!     GSURF : ACCELERATION DE LA PESANTEUR A LA SURFACE TERRESTRE

      DATA RP/6356.77D0/
!     RP : RAYON TERRESTRE AU POLE

! ---

      ZETA = (ALT-ZLB)*(RP+ZLB)/(RP+ALT)
      EMSZ = DEXP(-S*ZETA)

      GLB = GSURF / (1.D0+ZLB/RP)**2
      GAMMA = XM*GLB/(S*RGAS*TINF)

      PETITA = 1.D0 - TLB/TINF

      COEF1 = GAMMA + PETITA*(1.D0+ALPHA)*EMSZ
      COEF2 = GAMMA*XMM/XM + PETITA*EMSZ

      GRANDA = ZHM / (XMM-XM)
      IF (DNM.EQ.(0.D0)) THEN
         RAP=0.D0
      ELSE
         RAP = DEXP( GRANDA * DLOG(DNM/DND) )
      ENDIF

      FACT = (S/(1.D0-PETITA*EMSZ)) * ((RP+ZLB)/(RP+ALT))**2

      CPS_DERIV1= FACT * (COEF1+COEF2*RAP) / (1.D0+RAP)

      RETURN
!!!convf77f90 : ATTENTION, ANCIENNE MANIERE DE DECLARER UNE FONCTION ...
    END function  CPS_DERIV1

      real(kind=pm_reel) FUNCTION CPS_DERIV2(ALT,R,H,ZH)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_DERIV2
!
!$Resume
!  CALCUL DE LA DERIVEE DU COEF CORRECTEUR DE LA DENSITE DE L'ELEMENT
! CONSIDERE PAR RAPPORT A L'ALTITUDE
!
!$Description
!  CALCUL DE LA DERIVEE DU COEF CORRECTEUR DE LA DENSITE DE L'ELEMENT
!  CONSIDERE PAR RAPPORT A L'ALTITUDE
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_DERIV2(ALT,R,H,ZH)
!.    real(kind=pm_reel) :: alt, r, h, zh
!
!$Arguments
!>E     ALT  :<pm_reel>   ALTITUDE (KM)
!>E     R    :<pm_reel>   PARAMETRE DE CORRECTION DE DENSITE DE L'ELEMENT CONSIDERE
!>E     H    :<pm_reel>   HAUTEUR D'ECHELLE DE LA CORRECTION DE L'ELEMENT CONSIDERE
!>E     ZH   :<pm_reel>   ALTITUDE A LAQUELLE LE COEF CORRECTEUR VAUT EXP(R/2)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none

        real(kind=pm_reel), intent(in) :: alt, r, h, zh
        real(kind=pm_reel) ::  e, ex

      CPS_DERIV2 = 0.D0
      E=(ALT-ZH)/H

      IF(E .GT. (70.D0) .OR. E .LT. (-70.D0)) GO TO 10

      EX=DEXP(E)
      CPS_DERIV2 = (R*EX/H) / (1+EX)**2

   10 CONTINUE

      RETURN
    END function  CPS_DERIV2

      real(kind=pm_reel) FUNCTION CPS_DNET(DD,DM,ZHM,XMM,XM)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_DNET
!
!$Resume
! CALCUL DE LA CORRECTION DE DENSITE DUE A L'EQUILIBRE DE TURBOPAUSE
!
!$Description
! CALCUL DE LA CORRECTION DE DENSITE DUE A L'EQUILIBRE DE TURBOPAUSE
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_DNET(DD,DM,ZHM,XMM,XM)
!.    real(kind=pm_reel) :: dd, dm, zhm, xmm, xm
!
!$Arguments
!>E     DD   :<pm_reel>   DENSITE DE L'ELEMENT CONSIDERE OBTENUE PAR LE CALCUL DE
!              L'EQUILIBRE DIFFUSIF
!>E     DM   :<pm_reel>   DENSITE DE L'ELEMENT CONSIDERE OBTENUE PAR LE CALCUL DE
!              L'EQUILIBRE DE TURBOPAUSE
!>E     ZHM  :<pm_reel>   MASSE MOLAIRE MOLECULAIRE DE L'AZOTE (28 G/MOL)
!>E     XMM  :<pm_reel>   MASSE MOLAIRE MOLECULAIRE MOYENNE (28.95 G/MOL)
!>E     XM   :<pm_reel>   MASSE MOLAIRE MOLECULAIRE DE L'ELEMENT CONSIDERE (G/MOL)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        
        implicit none
        
        real(kind=pm_reel), intent(in) :: dd, dm, zhm, xmm, xm
        real(kind=pm_reel) :: a, ylog

!       Eq. A12b
      A=ZHM/(XMM-XM)
!       Eq. A12a
      YLOG=A*DLOG(DM/DD)

      IF(YLOG.LT.(-10.D0)) GO TO 10
      IF(YLOG.GT.(10.D0))  GO TO 20

      CPS_DNET=DD*(1.+DEXP(YLOG))**(1/A)
      GO TO 50

   10 CONTINUE
      CPS_DNET=DD
      GO TO 50

   20 CONTINUE
      CPS_DNET=DM
      GO TO 50

   50 CONTINUE

      RETURN

    END function  CPS_DNET

      real(kind=pm_reel) FUNCTION CPS_GLOB5L(P)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_GLOB5L
!
!$Resume
! FONCTION G(L) DU MODELE MSIS86 (PARAMETRES DE LA THERMOSPHERE
! INFERIEURE)
!
!$Description
! FONCTION G(L) DU MODELE MSIS86 (PARAMETRES DE LA THERMOSPHERE
! INFERIEURE)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_GLOB5L(P)
!.    real(kind=pm_reel), dimension(150) :: p
!
!$Arguments
!>E     P  :<pm_reel,DIM=(150)>   TABLEAU DE 150 COEFFICIENTS DU MODELE MSIS86
!
!$Common
!#V
!- common /LPOLY_COMPAS_MSIS86/
!#
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none

        real(kind=pm_reel), dimension(150), intent(in) :: p
        real(kind=pm_reel), dimension(15) :: t
        real(kind=pm_reel) :: CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC,DAY,DF,DFA,APD,APDF
      COMMON/LPOLY_COMPAS_MSIS86/PLG,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC, &
                  S3TLOC,DAY,DF,DFA,APD,APDF,APT
       real(kind=pm_reel), dimension(9,4) :: PLG
        real(kind=pm_reel), dimension(4) :: apt
        real(kind=pm_reel), dimension(25) :: sw, swc 
      COMMON/CSW_COMPAS_MSIS86/SW,SWC

        integer :: i 
        real(kind=pm_reel) :: dr, cd7, cd9, cd11, tt


! ---

      DATA DR/1.72142D-2/,T/15*0.D0/

! ---

       CD7=DCOS(DR*(DAY-P(7)))
       CD9=DCOS(2.*DR*(DAY-P(9)))
       CD11=DCOS(DR*(DAY-P(11)))

      T(1)=P(2)*DFA
      T(2)=P(4)*PLG(3,1)
      T(3)=P(6)*CD7
      T(4)=(P(8) )*CD9
      T(5)=(P(10)*PLG(2,1)+P(22)*PLG(4,1))*CD11
      T(6)=0.
      T(7)=(P(14)*PLG(2,2)*CTLOC+P(15)*PLG(2,2)*STLOC)
      T(8)=(P(16)*PLG(3,3)+P(18)*PLG(5,3) &
           +(P(20)*PLG(6,3))*CD11*SWC(5) &
           )*C2TLOC &
           +(P(17)*PLG(3,3)+P(19)*PLG(5,3) &
           +(P(21)*PLG(6,3))*CD11*SWC(5) &
           )*S2TLOC
      T(14)=(P(12)*PLG(4,4)*C3TLOC &
           +P(25)*PLG(4,4)*S3TLOC)
      IF(SW(9).EQ.(1.D0)) &
       T(9)=APDF*(P(23)+P(24)*PLG(3,1)*SWC(2))
      IF(SW(9).EQ.(-1.D0)) &
       T(9)=(P(3)*APT(3)+P(5)*PLG(3,1)*APT(3)*SWC(2))
!       PARMS NOT USED: 13
      TT=0.D0
      DO  I=1,14
         TT=TT+DABS(SW(I))*T(I)
      ENDDO
         CPS_GLOB5L=TT
      RETURN

    END function  CPS_GLOB5L

      real(kind=pm_reel) FUNCTION CPS_GLOBE5(NUMJ,SEC,LAT,LONG,TLOC, &
      FBAR,F,AP,P)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_GLOBE5
!
!$Resume
! FONCTION G(L) DU MODELE MSIS86 (PARAMETRES DE LA THERMOSPHERE
! SUPERIEURE)
!
!$Description
! FONCTION G(L) DU MODELE MSIS86 (PARAMETRES DE LA THERMOSPHERE
! SUPERIEURE)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          real(kind=pm_reel) FUNCTION CPS_GLOBE5(NUMJ,SEC,LAT,LONG,TLOC, &
!.          FBAR,F,AP,P)
!.    real (KIND=pm_reel) :: LAT, LONG, numj, sec, tloc, fbar, f
!.    real (KIND=pm_reel), dimension(150) :: p
!.    real(kind=pm_reel), dimension(7) :: ap
!
!$Arguments
!>E     NUMJ  :<pm_reel>             NUMERO DU JOUR DANS L'ANNEE    
!>E     SEC   :<pm_reel>             SECONDE TU DANS LE JOUR
!>E     LAT   :<pm_reel>             LATITUDE GEODESIQUE EN DEGRES
!>E     LONG  :<pm_reel>             LONGITUDE GEODESIQUE EN DEGRES
!>E     TLOC  :<pm_reel>             HEURE SOLAIRE LOCALE EN RADIANS
!>E     FBAR  :<pm_reel>             FLUX SOLAIRE MOYEN SUR 3 MOIS
!>E     F     :<pm_reel>             FLUX SOLAIRE DU JOUR PRECEDENT
!>E     AP    :<pm_reel,DIM=(7)>     TABLEAU DE L'ACTIVITE GEOMAGNETIQUE CONTENANT :
!             (1) AP JOURNALIER
!             (2) AP TRI-HORAIRE COURANT
!             (3) AP TRI-HORAIRE DES 3H PRECEDENTES
!             (4) AP TRI-HORAIRE DES 6H PRECEDENTES
!             (5) AP TRI-HORAIRE DES 9H PRECEDENTES
!             (6) MOYENNE DES 8 AP TRI-HORAIRES DE 12 A 33H PRECEDENTES
!             (7) MOYENNE DES 8 AP TRI-HORAIRES DE 36 A 59H PRECEDENTES
!>E/S   P     :<pm_reel,DIM=(150)>   TABLEAU DE 150 COEFFICIENTS DU MODELE MSIS
!
!$Common
!#V
!- common /CSW_COMPAS_MSIS86/
!- common /LPOLY_COMPAS_MSIS86/
!#
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none


      real (KIND=pm_reel), intent(in) :: LAT, LONG, numj, sec, tloc, fbar, f
      real (KIND=pm_reel), intent(inout), dimension(150) :: p
      real(kind=pm_reel), dimension(7), intent(in) :: ap
      integer :: i
      real(kind=pm_reel) :: CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC, S3TLOC,DAY,DF,DFA,APD,APDF
      real(kind=pm_reel), dimension(4) :: apt
      real(kind=pm_reel), dimension(9,4) :: plg
      real(kind=pm_reel), dimension(25) :: sw, swc
      COMMON/CSW_COMPAS_MSIS86/SW,SWC

      real(kind=pm_reel) :: dgtr, dr, xl, tll, hr, sr, c, s, c2, c4, s2, ex,f1,f2,tinf
      real(kind=pm_reel) :: cd14,cd18,cd32,cd39
      real(kind=pm_reel) :: t71, t72, t81, t82, p44, p45, exp1, exp2
      real(kind=pm_reel) :: sumex, a, g0, sg0
      integer, parameter :: NSW = 14
!     NSW : NOMBRE MAXIMUM D'EFFETS JOUANT SUR LA FONCTION G(L)

! ---
      real(kind=pm_reel), dimension(NSW) :: T 


! ---

      COMMON/LPOLY_COMPAS_MSIS86/PLG,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC, &
                   S3TLOC,DAY,DF,DFA,APD,APDF,APT

! ---

      DATA DGTR/1.74533D-2/,DR/1.72142D-2/, XL/1000.D0/,TLL/1000.D0/ &
        HR/0.2618D0/,SR/7.2722D-5/

!***********************************************************************
!*FON DEFINITION DES 3 FONCTIONS STATIQUES G0, SUMEX ET SG0
!***********************************************************************

! Eq. A24d
      G0(A)=(A-4.+(P(26)-1.)*(A-4.+(DEXP(-DABS(P(25))*(A-4.))-1.)/ &
        DABS(P(25))))
! Eq. A24c
      SUMEX(EX)=1.+(1.-EX**19)/(1.-EX)*EX**(.5)
! Eq. A24a
      SG0(EX)=(G0(AP(2))+(G0(AP(3))*EX+G0(AP(4))*EX*EX+G0(AP(5))*EX**3 &
       +(G0(AP(6))*EX**4+G0(AP(7))*EX**12)*(1.D0-EX**8)/(1.D0-EX))) &
       /SUMEX(EX)

! ---

      DO  I=1,NSW
         T(I) = 0.D0
      ENDDO

      DAY = real(NUMJ)

!***********************************************************************
!*FON CALCUL DES COEF DE L'EQ. A22 SI BESOIN EST
!***********************************************************************

      IF(XL.EQ.LAT)   GO TO 15

! CALCUL DES POLYNOMES DE LEGENDRE DE LA LATITUDE

      C = DSIN(LAT*DGTR)
      S = DCOS(LAT*DGTR)
      C2 = C*C
      C4 = C2*C2
      S2 = S*S
      PLG(2,1) = C
      PLG(3,1) = 0.5D0*(3.D0*C2 -1.D0)
      PLG(4,1) = 0.5D0*(5.D0*C*C2-3.D0*C)
      PLG(5,1) = (35.D0*C4 - 30.D0*C2 + 3.D0)/8.D0
      PLG(6,1) = (63.D0*C2*C2*C - 70.D0*C2*C + 15.D0*C)/8.D0
      PLG(7,1) = (11.D0*C*PLG(6,1) - 5.D0*PLG(5,1))/6.D0
      PLG(2,2) = S
      PLG(3,2) = 3.D0*C*S
      PLG(4,2) = 1.5D0*(5.D0*C2-1.D0)*S
      PLG(5,2) = 2.5D0*(7.D0*C2*C-3.D0*C)*S
      PLG(6,2) = 1.875D0*(21.D0*C4 - 14.D0*C2 +1.D0)*S
      PLG(7,2) = (11.D0*C*PLG(6,2)-6.D0*PLG(5,2))/5.D0
      PLG(3,3) = 3.D0*S2
      PLG(4,3) = 15.D0*S2*C
      PLG(5,3) = 7.5D0*(7.D0*C2 -1.D0)*S2
      PLG(6,3) = 3.D0*C*PLG(5,3)-2.D0*PLG(4,3)
      PLG(7,3)=(11.D0*C*PLG(6,3)-7.D0*PLG(5,3))/4.D0
      PLG(8,3)=(13.D0*C*PLG(7,3)-8.D0*PLG(6,3))/5.D0
      PLG(4,4) = 15.D0*S2*S
      PLG(5,4) = 105.D0*S2*S*C
      PLG(6,4)=(9.D0*C*PLG(5,4)-7.D0*PLG(4,4))/2.D0
      PLG(7,4)=(11.D0*C*PLG(6,4)-8.D0*PLG(5,4))/3.D0
      XL=LAT
   15 CONTINUE

! COEF DE DEPENDANCE DE L'HEURE LOCALE

      IF(TLL.EQ.TLOC)   GO TO 16
      STLOC = DSIN(TLOC)
      CTLOC = DCOS(TLOC)
      S2TLOC = DSIN(2.*TLOC)
      C2TLOC = DCOS(2.*TLOC)
      S3TLOC = DSIN(3.*TLOC)
      C3TLOC = DCOS(3.*TLOC)
      TLL = TLOC
   16 CONTINUE

       CD14=DCOS(DR*(DAY-P(14)))
       CD18=DCOS(2.D0*DR*(DAY-P(18)))
       CD32=DCOS(DR*(DAY-P(32)))
       CD39=DCOS(2.D0*DR*(DAY-P(39)))

! ACTIVITE SOLAIRE

      DF = F- FBAR
      DFA=FBAR-150.D0
      T(1) =  P(20)*DF + P(21)*DF*DF + P(22)*DFA &
       + P(30)*DFA**2
      F1 = 1.D0 + (P(48)*DFA +P(20)*DF+P(21)*DF*DF)*SWC(1)
      F2 = 1.D0 + (P(50)*DFA+P(20)*DF+P(21)*DF*DF)*SWC(1)

! TERME CONSTANT

      T(2) = &
        (P(2)*PLG(3,1) + P(3)*PLG(5,1)+P(23)*PLG(7,1)) &
       +(P(15)*PLG(3,1))*DFA*SWC(1) &
       +P(27)*PLG(2,1)

! SYMETRIE ANNUELLE

      T(3) = &
       (P(19) )*CD32

! SYMETRIE SEMI-ANNUELLE

      T(4) = &
       (P(16)+P(17)*PLG(3,1))*CD18

! ASYMETRIE ANNUELLE

      T(5) =  F1* &
        (P(10)*PLG(2,1) + P(11)*PLG(4,1))*CD14

! ASYMETRIE SEMI-ANNUELLE

      T(6) =    P(38)*PLG(2,1)*CD39

! VARIATION DIURNE

      T71 = (P(12)*PLG(3,2) + P(36)*PLG(2,2))*CD14*SWC(5)
      T72 = (P(13)*PLG(3,2) + P(37)*PLG(2,2))*CD14*SWC(5)
      T(7) = F2* &
       ((P(4)*PLG(2,2) + P(5)*PLG(4,2) + P(28)*PLG(6,2) &
       + T71)*CTLOC &
       + (P(7)*PLG(2,2) + P(8)*PLG(4,2) +P(29)*PLG(6,2) &
       + T72)*STLOC)

! VARIATION SEMI-DIURNE

      T81 = (P(24)*PLG(4,3))*CD14*SWC(5)
      T82 = (P(34)*PLG(4,3))*CD14*SWC(5)
      T(8) = F2* &
       ((P(6)*PLG(3,3) + P(42)*PLG(5,3) + T81)*C2TLOC &
       +(P(9)*PLG(3,3) + P(43)*PLG(5,3) + T82)*S2TLOC)

! VARIATION TER-DIURNE

      T(14) = F2* &
       ((P(40)*PLG(4,4)+(P(94)*PLG(5,4)+P(47)*PLG(7,4))*CD14*SWC(5))* &
       S3TLOC &
       +(P(41)*PLG(4,4)+(P(95)*PLG(5,4)+P(49)*PLG(7,4))*CD14*SWC(5))* &
       C3TLOC)

! ACTIVITE GEOMAGNETIQUE

      IF(SW(9).EQ.(-1.D0) .AND. P(52).NE.(0.D0)) GO TO 30

! CAS SIMPLIFIE : UN AP JOURNALIER
      APD=(AP(1)-4.D0)
      P44=P(44)
      P45=P(45)
      IF(P44.LT.0) P44=1.D-5
      APDF = (APD+(P45-1.D0)*(APD+(DEXP(-P44  *APD)-1.D0)/P44  ))
      T(9)=APDF*(P(33)+P(46)*PLG(3,1)+P(35)*PLG(5,1)+ &
       (P(101)*PLG(2,1)+P(102)*PLG(4,1)+P(103)*PLG(6,1))*CD14*SWC(5)+ &
       (P(122)*PLG(2,2)+P(123)*PLG(4,2)+P(124)*PLG(6,2))*SWC(7)* &
       DCOS(TLOC-HR*P(125)))
      GO TO 40

   30 CONTINUE
      EXP1 = DEXP(-10800.D0*DABS(P(52)) &
      /(1.D0+P(139)*(45.D0-DABS(LAT))))
      IF(EXP1.GT.(.99999D0)) EXP1=.99999D0
      EXP2 = DEXP(-10800.D0*DABS(P(54)))
      IF(EXP2.GT.(.99999D0)) EXP2=.99999D0
      IF(P(25).LT.1.D-4) P(25)=1.D-4
      APT(1)=SG0(EXP1)
      APT(3)=SG0(EXP2)
      T(9) = APT(1)*(P(51)+P(97)*PLG(3,1)+P(55)*PLG(5,1)+ &
       (P(126)*PLG(2,1)+P(127)*PLG(4,1)+P(128)*PLG(6,1))*CD14*SWC(5)+ &
       (P(129)*PLG(2,2)+P(130)*PLG(4,2)+P(131)*PLG(6,2))*SWC(7)* &
       DCOS(TLOC-HR*P(132)))

  40  CONTINUE
      IF(SW(10).EQ.(0.D0).OR.LONG.LE.(-1000.D0)) GO TO 48

! EFFET LONGITUDINAL

      T(11)= (1.D0+P(90)*PLG(2,1))*(1.D0+P(81)*DFA*SWC(1))* &
      ((P(65)*PLG(3,2)+P(66)*PLG(5,2)+P(67)*PLG(7,2) &
       +P(104)*PLG(2,2)+P(105)*PLG(4,2)+P(106)*PLG(6,2) &
       +SWC(5)*(P(110)*PLG(2,2)+P(111)*PLG(4,2)+P(112)*PLG(6,2))*CD14)* &
           DCOS(DGTR*LONG) &
       +(P(91)*PLG(3,2)+P(92)*PLG(5,2)+P(93)*PLG(7,2) &
       +P(107)*PLG(2,2)+P(108)*PLG(4,2)+P(109)*PLG(6,2) &
       +SWC(5)*(P(113)*PLG(2,2)+P(114)*PLG(4,2)+P(115)*PLG(6,2))*CD14)* &
        DSIN(DGTR*LONG))

! EFFET DU TU ET TU,LONGITUDE

      T(12)=(1.D0+P(96)*PLG(2,1))*(1.D0+P(82)*DFA*SWC(1))* &
      (1.D0+P(120)*PLG(2,1)*SWC(5)*CD14)* &
      ((P(69)*PLG(2,1)+P(70)*PLG(4,1)+P(71)*PLG(6,1))* &
           DCOS(SR*(SEC-P(72))))
      T(12)=T(12)+SWC(11)* &
       (P(77)*PLG(4,3)+P(78)*PLG(6,3)+P(79)*PLG(8,3))* &
           DCOS(SR*(SEC-P(80))+2.D0*DGTR*LONG)*(1.D0+P(138)*DFA*SWC(1))

! EFFETS TU,LONGITUDE ET ACTIVITE GEOMAGNETIQUE

      IF(SW(9).EQ.(-1.D0) .AND. P(52).NE.(0.D0)) GO TO 45
      T(13)= APDF*SWC(11)*(1.D0+P(121)*PLG(2,1))* &
      ((P( 61)*PLG(3,2)+P( 62)*PLG(5,2)+P( 63)*PLG(7,2))* &
           DCOS(DGTR*(LONG-P( 64)))) &
       +APDF*SWC(11)*SWC(5)* &
       (P(116)*PLG(2,2)+P(117)*PLG(4,2)+P(118)*PLG(6,2))* &
           CD14*DCOS(DGTR*(LONG-P(119))) &
       + APDF*SWC(12)* &
       (P( 84)*PLG(2,1)+P( 85)*PLG(4,1)+P( 86)*PLG(6,1))* &
           DCOS(SR*(SEC-P( 76)))
      GOTO 48

   45 CONTINUE
      T(13)=APT(1)*SWC(11)*(1.D0+P(133)*PLG(2,1))* &
      ((P(53)*PLG(3,2)+P(99)*PLG(5,2)+P(68)*PLG(7,2))* &
           DCOS(DGTR*(LONG-P(98)))) &
       +APT(1)*SWC(11)*SWC(5)* &
       (P(134)*PLG(2,2)+P(135)*PLG(4,2)+P(136)*PLG(6,2))* &
           CD14*DCOS(DGTR*(LONG-P(137))) &
       +APT(1)*SWC(12)* &
       (P(56)*PLG(2,1)+P(57)*PLG(4,1)+P(58)*PLG(6,1))* &
           DCOS(SR*(SEC-P(59)))
   48 CONTINUE

!  PARMS NOT USED: 60,83,100,140-150
      TINF = 0.D0
      IF(SW(9).EQ.-1.D0) TINF=P(31)

      DO  I = 1,NSW
         TINF = TINF + DABS(SW(I))*T(I)
      ENDDO
      CPS_GLOBE5 = TINF

      RETURN
    END function  CPS_GLOBE5


      INTEGER FUNCTION CPS_IUJJUL (IJOUR,IMOIS,IAN,IER)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_IUJJUL
!
!$Resume
! CALCUL DU NOMBRE DE JOURS JULIENS ECOULES DEPUIS LE 01/01/1950
! A 0 HEURE, EN FONCTION D'UNE DATE CALENDAIRE (JOUR,MOIS,ANNEE)
!
!$Description
! CALCUL DU NOMBRE DE JOURS JULIENS ECOULES DEPUIS LE 01/01/1950
! A 0 HEURE, EN FONCTION D'UNE DATE CALENDAIRE (JOUR,MOIS,ANNEE)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          INTEGER FUNCTION CPS_IUJJUL (IJOUR,IMOIS,IAN,IER)
!.    INTEGER :: IJOUR,IMOIS,IAN
!.    integer :: ier
!
!$Arguments
!>E     IJOUR  :<integer>   NUMERO DU JOUR DANS LE MOIS
!>E     IMOIS  :<integer>   NUMERO DU MOIS DANS L'ANNEE
!>E     IAN    :<integer>   = 1950)
!>S     IER    :<integer>   CODE RETOUR
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use mslib
        implicit none

!     --------------------------
!     DECLARATION DES PARAMETRES
!     --------------------------

      INTEGER, intent(in) :: IJOUR,IMOIS,IAN
      integer, intent(out) :: ier
!     ---------------------------------
!     DECLARATION DES VARIABLES LOCALES
!     ---------------------------------

      INTEGER :: IBISEX
!     IBISEX = 0 SI ANNEE BISEXTILE
      INTEGER :: IY
!     IY  NOMBRE D'ANNEES ECOULEES DEPUIS LE 01/01/1948
      INTEGER :: IA
!     IA  NOMBRE D'ANNEES BISEXTILES DEPUIS 1948 A L'ANNEE COURANTE
      INTEGER :: N
!     N : INDICE DE BOUCLE

!     -------------------------------
!     DECLARATION DES INITIALISATIONS
!     -------------------------------

      INTEGER :: ITAB(12),NBJOUR(12)

!     . . . INITIALISATION DE L'IDENTIFICATEUR SCCS

      CHARACTER (LEN=50) :: SCCSID
      DATA SCCSID/"@(#)  mujjul.f    5.1  90/04/10  n"/


!     ITAB   : NOMBRE DE JOURS DU MOIS PRECEDENT
!     NBJOUR : NOMBRE DE JOURS DU MOIS COURANT

      DATA ITAB/-1,31,28,31,30,31,30,31,31,30,31,30/
      DATA NBJOUR/31,28,31,30,31,30,31,31,30,31,30,31/

!     ******************
!     DEBUT DE PROGRAMME
!     ******************

      SCCSID = SCCSID
      IER = 0
      CPS_IUJJUL = 0
      IBISEX = 0

!     -----------------------------------------------
!*FON VERIFICATION DE LA VALIDITE DE LA DATE D'ENTREE
!     -----------------------------------------------

!     ----------------
!*FON TEST SUR L'ANNEE
!     ----------------

      IF (IAN.LT.1950) THEN
         IER = -3
      ELSE

!        ----------------
!*FON    TEST SUR LE MOIS
!        ----------------

         IF (IMOIS.LT.1.OR.IMOIS.GT.12) THEN
            IER = -2
         ELSE
            IBISEX = MOD (IAN,4)

!           --------------------------------------------
!*FON       VERIFICATION VALIDITE JOUR (AVEC MOIS ET AN)
!           --------------------------------------------

            IF (IMOIS.NE.2) THEN

!*FON          - POUR LES MOIS AUTRES QUE FEVRIER

               IF (IJOUR.LT.1.OR.IJOUR.GT.NBJOUR(IMOIS)) IER = -1
            ELSE

!*FON          - POUR LE MOIS DE FEVRIER

               IF (IJOUR.LT.1.OR. &
                   (IBISEX.EQ.0.AND.IJOUR.GT.29).OR. &
                   (IBISEX.NE.0.AND.IJOUR.GT.28))      IER = -1
            ENDIF
         ENDIF
      ENDIF

!     ---------------------------------------------------------------
!*FON SI LES DONNEES D'ENTREE SONT VALIDES, CALCUL DU NOMBRE DE JOURS
!*FON JULIENS
!     ---------------------------------------------------------------

      IF (IER.EQ.0) THEN
         IY = IAN - 1948
         IA = (IY-1)/4
         CPS_IUJJUL = IA + 365*(IY-2)
         DO N=1,IMOIS
            CPS_IUJJUL = CPS_IUJJUL + ITAB (N)
         ENDDO

!        AJOUT EVENTUEL JOURNEE BISEXTILE DE L'ANNEE COURANTE

         IF (IMOIS.GT.2.AND.IBISEX.EQ.0) CPS_IUJJUL = CPS_IUJJUL + 1
         CPS_IUJJUL = CPS_IUJJUL + IJOUR
      ENDIF

!     ****************
!     FIN DE PROGRAMME
!     ****************

      RETURN
    END function  CPS_IUJJUL
      
    SUBROUTINE CPS_IUCALD (IJUL,IJOUR,IMOIS,IAN,IER)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  CPS_IUCALD
!
!$Resume
! DETERMINATION D'UNE DATE CALENDAIRE (JOUR,MOIS,ANNEE) A PARTIR
! D'UNE DATE DONNEE EN JOUR JULIEN (1ER JANVIER 1950 0 HEURE).
!
!$Description
! DETERMINATION D'UNE DATE CALENDAIRE (JOUR,MOIS,ANNEE) A PARTIR
! D'UNE DATE DONNEE EN JOUR JULIEN (1ER JANVIER 1950 0 HEURE).
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call CPS_IUCALD (IJUL,IJOUR,IMOIS,IAN,IER)
!.    integer :: IJUL
!.    INTEGER :: IJOUR,IMOIS,IAN,IER
!
!$Arguments
!>E     IJUL   :<integer>   NOMBRE DE JOURS JULIENS 1950
!>S     IJOUR  :<integer>   JOUR CALENDAIRE
!>S     IMOIS  :<integer>   MOIS CALENDAIRE
!>S     IAN    :<integer>   ANNEE CALENDAIRE
!>S     IER    :<integer>   CODE RETOUR (0 : OK, SINON ERREUR (IJUL < 0) )
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      implicit none
!     --------------------------
!     DECLARATION DES PARAMETRES
!     --------------------------
      integer, intent(in) :: IJUL
      INTEGER, intent(out) :: IJOUR,IMOIS,IAN,IER

!     ---------------------------------
!     DECLARATION DES VARIABLES LOCALES
!     ---------------------------------

      INTEGER :: NJUL,NA,NB,IBISEX,NDJ,NM1
!     NJUL   : NOMBRE DE JOURS ECOULES DEPUIS LE 01/01/1948
!     NA     : NOMBRE D'ANNEES ECOULEES DEPUIS LE 01/01/1948
!     NB     : NOMBRE D'ANNEES BISEXTILES ECOULEES DEPUIS LE 01/01/1950
!     IBISEX : INDICATEUR D'ANNEE BISEXTILE (0:OUI, 1 A 3:NON)
!     NDJ    : NOMBRE DE JOURS ECOULES DU 01/01 DE L'ANNEE COURANTE
!              A LA FIN DU MOIS COURANT.
!     NM1    : NOMBRE DE JOUR ECOULES DU 01/01 DE L'ANNEE COURANTE
!              A LA FIN DU MOIS PRECEDENT.

!     -------------------------------
!     DECLARATION DES INITIALISATIONS
!     -------------------------------

      INTEGER :: NBJOUR (12)
!     NBJOUR : NOMBRE DE JOUR DANS UN MOIS EN ANNEE NON BISEXTILE


!     . . . INITIALISATION DE L'IDENTIFICATEUR SCCS

      CHARACTER (LEN=50) :: SCCSID
      DATA SCCSID/"@(#)  mucald.f    5.1  90/04/10  n"/

      DATA NBJOUR/31,28,31,30,31,30,31,31,30,31,30,31/

!     ******************
!     DEBUT DE PROGRAMME
!     ******************

      SCCSID = SCCSID
      IER = 0
      IJOUR = 0
      IMOIS = 0
      IAN = 0

!     --------------------------
!*FON TEST VALEUR DU JOUR JULIEN
!     --------------------------

      IF (IJUL.LT.0) THEN
         IER = -1
         GO TO 6000
      ENDIF

!     ---------------------------------------------------------
!*FON CALCUL DU NOMBRE D'ANNEES BISEXTILES DEPUIS LE 01/01/1950
!     ---------------------------------------------------------

      NJUL = IJUL + 731
      NA = NJUL / 365
      NB = (NA - 1) / 4

!     ----------------------------
!*FON CALCUL DE LA DATE CALENDAIRE
!     ----------------------------

!     -------
!*FON - ANNEE
!     -------

      IJOUR = NJUL - 365 * NA
      IJOUR = IJOUR - NB
      IF (IJOUR.GT.0) THEN
         IAN = 1948 + NA
      ELSE
         IAN = 1948 + NA - 1
      ENDIF

      IBISEX = MOD (IAN,4)
      IF (IJOUR.LE.0) THEN
         IF (IBISEX.EQ.0) THEN
            IJOUR = 366 + IJOUR
         ELSE
            IJOUR = 365 + IJOUR
         ENDIF
      ENDIF

!     --------------
!*FON - MOIS ET JOUR
!     --------------

      NM1 = 0
      IMOIS = 1
      IF (IBISEX.EQ.0) THEN
         IF (IJOUR.EQ.60) THEN
            IMOIS = 2
            IJOUR = 29
            GO TO 6000
         ELSE
            IF (IJOUR.GT.60) THEN
               IMOIS = 3
               NM1 = 60
            ENDIF
         ENDIF
      ENDIF

 100  CONTINUE

      NDJ = NM1 + NBJOUR (IMOIS)
      IF (IJOUR.GT.NDJ) THEN
         IMOIS = IMOIS + 1
         NM1 = NDJ
         GO TO 100
      ENDIF
      IJOUR = IJOUR - NM1

!     ****************
!     FIN DE PROGRAMME
!     ****************

 6000 CONTINUE
      RETURN
    end subroutine  CPS_IUCALD

    subroutine cps_data_msis86 ( retour )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_data_msis86
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_data_msis86 ( retour )
!.    integer :: retour
!
!$Arguments
!>S     retour  :<integer>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- mslib
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      ! Modules
      ! =======
      use mslib
      
      implicit none
      
      integer, intent(out)       :: retour

! Autres declarations
! -------------------

real(kind=pm_reel), dimension(50), parameter :: donnees_PT1= (/ &       ! donnees PT1
    0.996040e+00_pm_reel,  0.385528e-01_pm_reel,  0.303445e-02_pm_reel, -0.105531e+00_pm_reel, -0.607134e-02_pm_reel, &
   -0.516278e-03_pm_reel, -0.115622e+00_pm_reel,  0.202240e-02_pm_reel,  0.990156e-02_pm_reel, -0.127371e+00_pm_reel, &
   -0.302449e-01_pm_reel,  0.123512e-01_pm_reel, -0.526277e-02_pm_reel, -0.845398e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.142370e-01_pm_reel,  0.000000e+00_pm_reel,  0.125818e+03_pm_reel,  0.805486e-02_pm_reel,  0.164419e-02_pm_reel, &
   -0.621452e-05_pm_reel,  0.311701e-02_pm_reel,  0.000000e+00_pm_reel,  0.386578e-02_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.641110e-05_pm_reel, &
    0.000000e+00_pm_reel,  0.300150e+02_pm_reel,  0.533297e-02_pm_reel,  0.389146e-02_pm_reel,  0.204725e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.192645e-01_pm_reel,  0.275905e+01_pm_reel,  0.147284e-02_pm_reel, &
    0.341345e-03_pm_reel, -0.117388e-02_pm_reel, -0.354589e-03_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
    0.508295e-02_pm_reel,  0.365016e-04_pm_reel,  0.426385e-02_pm_reel,  0.115102e-03_pm_reel,  0.511819e-02_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PT2= (/ &       ! donnees PT2
    0.609108e-02_pm_reel,  0.404995e-04_pm_reel,  0.153049e-02_pm_reel,  0.241470e-04_pm_reel,  0.230764e-02_pm_reel, &
    0.155267e-02_pm_reel,  0.133722e-02_pm_reel, -0.182318e-02_pm_reel, -0.263007e+03_pm_reel,  0.000000e+00_pm_reel, &
    0.137337e-02_pm_reel,  0.995774e-03_pm_reel,  0.000000e+00_pm_reel, -0.108983e+03_pm_reel,  0.562606e-02_pm_reel, &
    0.594053e-02_pm_reel,  0.109358e-02_pm_reel,  0.000000e+00_pm_reel, -0.133410e-01_pm_reel, -0.243409e-01_pm_reel, &
   -0.135688e-01_pm_reel,  0.311370e+05_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.283023e+04_pm_reel,  0.845583e-03_pm_reel,  0.538706e-03_pm_reel,  0.000000e+00_pm_reel,  0.247956e+03_pm_reel, &
    0.292246e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.747703e-04_pm_reel,  0.887993e-03_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.116540e-01_pm_reel, -0.449173e-02_pm_reel, -0.353189e-03_pm_reel, -0.173933e-03_pm_reel, -0.153218e-03_pm_reel, &
   -0.565411e+00_pm_reel,  0.777272e-02_pm_reel, -0.911784e+02_pm_reel,  0.645187e-03_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PT3= (/ &       ! donnees PT3
   -0.837685e-03_pm_reel,  0.242318e-02_pm_reel,  0.473796e-02_pm_reel, -0.301801e-02_pm_reel, -0.423564e-02_pm_reel, &
   -0.248289e-02_pm_reel,  0.919286e-03_pm_reel,  0.216372e-02_pm_reel,  0.863968e-03_pm_reel,  0.189689e-02_pm_reel, &
    0.415654e-02_pm_reel,  0.000000e+00_pm_reel,  0.118068e-01_pm_reel,  0.331190e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.120222e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.307246e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.672403e-03_pm_reel,  0.108930e-02_pm_reel,  0.972278e-03_pm_reel,  0.468242e+01_pm_reel, &
   -0.315034e-03_pm_reel,  0.400059e-02_pm_reel,  0.515036e-02_pm_reel,  0.162989e-02_pm_reel,  0.108824e-02_pm_reel, &
    0.995261e-03_pm_reel,  0.418955e+01_pm_reel, -0.364059e+00_pm_reel,  0.170182e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.320120e+01_pm_reel,  0.000000e+00_pm_reel,  0.580206e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PA1= (/ &       ! donnees PA1
    0.104934e+01_pm_reel, -0.288362e-01_pm_reel, -0.207095e+00_pm_reel, -0.103314e+00_pm_reel, -0.702373e-02_pm_reel, &
    0.129664e-01_pm_reel,  0.408853e+00_pm_reel, -0.919895e-02_pm_reel, -0.188660e-01_pm_reel,  0.140927e+01_pm_reel, &
    0.175033e+00_pm_reel,  0.187351e-01_pm_reel,  0.110979e+00_pm_reel, -0.742871e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.267143e+00_pm_reel, -0.595979e-01_pm_reel,  0.105038e+03_pm_reel, -0.840963e-01_pm_reel, -0.697632e-03_pm_reel, &
    0.206521e-05_pm_reel,  0.765306e-03_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.126762e+00_pm_reel, &
    0.128876e+00_pm_reel, -0.504479e-01_pm_reel, -0.130735e-01_pm_reel, -0.224348e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.150832e+03_pm_reel, -0.629928e-02_pm_reel,  0.000000e+00_pm_reel, -0.407760e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.525725e-01_pm_reel, -0.311486e+02_pm_reel, -0.313351e-02_pm_reel, &
    0.275838e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.111247e+00_pm_reel,  0.108815e+00_pm_reel, &
   -0.466713e-01_pm_reel,  0.000000e+00_pm_reel, -0.329329e-02_pm_reel,  0.000000e+00_pm_reel,  0.167838e-02_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PA2= (/ &       ! donnees PA2
   -0.916691e-02_pm_reel,  0.345044e-04_pm_reel, -0.971806e-02_pm_reel,  0.000000e+00_pm_reel, -0.204672e-02_pm_reel, &
   -0.786899e-02_pm_reel, -0.798285e-02_pm_reel,  0.536515e-02_pm_reel, -0.531172e+04_pm_reel,  0.000000e+00_pm_reel, &
   -0.642781e-02_pm_reel, -0.171690e-02_pm_reel,  0.000000e+00_pm_reel, -0.679131e+02_pm_reel, -0.179912e-01_pm_reel, &
   -0.158305e-01_pm_reel, -0.712313e-02_pm_reel,  0.000000e+00_pm_reel,  0.253477e-01_pm_reel,  0.852960e-01_pm_reel, &
    0.102163e+00_pm_reel,  0.295009e+05_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.684625e+04_pm_reel, -0.619098e-02_pm_reel, -0.269289e-02_pm_reel,  0.000000e+00_pm_reel, -0.520231e+03_pm_reel, &
   -0.633463e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.602428e-02_pm_reel, -0.407077e-02_pm_reel, &
    0.542264e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.407560e-01_pm_reel,  0.282288e-01_pm_reel,  0.908088e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.405204e+00_pm_reel, -0.597931e-01_pm_reel, -0.731823e+02_pm_reel, -0.206620e-02_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PA3= (/ &
   -0.372723e-02_pm_reel, -0.188146e-01_pm_reel, -0.101794e-01_pm_reel,  0.804633e-02_pm_reel,  0.101090e-01_pm_reel, &
    0.873253e-02_pm_reel,  0.238268e-01_pm_reel,  0.480444e-02_pm_reel,  0.171088e-02_pm_reel,  0.396369e-01_pm_reel, &
   -0.213809e-01_pm_reel,  0.000000e+00_pm_reel, -0.102588e+00_pm_reel, -0.591702e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.270923e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.175043e+03_pm_reel,  0.603489e+00_pm_reel, &
   -0.617589e+00_pm_reel,  0.838098e-02_pm_reel,  0.183871e-02_pm_reel, -0.705329e-03_pm_reel, -0.406644e+01_pm_reel, &
   -0.509347e-02_pm_reel, -0.284344e-01_pm_reel, -0.124160e-01_pm_reel,  0.133665e-01_pm_reel,  0.393410e-02_pm_reel, &
   -0.503723e-03_pm_reel, -0.457683e+01_pm_reel, -0.529542e+00_pm_reel, -0.425812e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.191541e+02_pm_reel,  0.000000e+00_pm_reel,  0.323247e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PB1= (/ &       ! donnees PB1
    0.931113e+00_pm_reel, -0.138721e+00_pm_reel, -0.133457e+00_pm_reel, -0.529542e-01_pm_reel, -0.444983e-02_pm_reel, &
    0.135264e-01_pm_reel,  0.598075e-01_pm_reel, -0.362880e-01_pm_reel, -0.312798e-01_pm_reel,  0.372068e+00_pm_reel, &
    0.295974e-01_pm_reel,  0.120509e-01_pm_reel,  0.521995e-01_pm_reel, -0.778888e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.118634e+00_pm_reel, -0.204495e-01_pm_reel,  0.103280e+03_pm_reel,  0.982432e-01_pm_reel,  0.477694e-03_pm_reel, &
    0.000000e+00_pm_reel,  0.274372e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.757809e-01_pm_reel, &
    0.171403e+00_pm_reel, -0.105205e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.873348e+01_pm_reel, -0.581094e-02_pm_reel,  0.000000e+00_pm_reel, -0.814944e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.517255e-01_pm_reel, -0.153028e+02_pm_reel, -0.348932e-02_pm_reel, &
    0.961771e-03_pm_reel,  0.557732e-02_pm_reel, -0.454180e-03_pm_reel,  0.988213e-01_pm_reel,  0.940456e-01_pm_reel, &
   -0.318797e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.232122e-02_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PB2= (/ &       ! donnees PB2
   -0.600220e-02_pm_reel,  0.277654e-04_pm_reel, -0.322019e-02_pm_reel,  0.000000e+00_pm_reel, -0.378551e-02_pm_reel, &
   -0.334809e-02_pm_reel, -0.170668e-02_pm_reel,  0.000000e+00_pm_reel,  0.636184e+04_pm_reel,  0.000000e+00_pm_reel, &
    0.159986e-02_pm_reel, -0.388204e-02_pm_reel, -0.164825e-02_pm_reel, -0.747955e+02_pm_reel, -0.105360e-01_pm_reel, &
   -0.945723e-02_pm_reel, -0.159824e-02_pm_reel, -0.706730e-03_pm_reel, -0.168513e-01_pm_reel, -0.113023e+00_pm_reel, &
   -0.636637e-01_pm_reel, -0.137709e+05_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.152368e+05_pm_reel, -0.586061e-02_pm_reel, -0.253108e-02_pm_reel,  0.000000e+00_pm_reel, -0.254837e+04_pm_reel, &
   -0.328988e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.276364e-02_pm_reel,  0.967923e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.434255e-01_pm_reel,  0.114020e-01_pm_reel, -0.618447e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.302568e+00_pm_reel, -0.327694e-01_pm_reel, -0.671589e+02_pm_reel, -0.228340e-02_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PB3= (/ &       ! donnees PB3
    0.306230e-02_pm_reel, -0.465113e-02_pm_reel, -0.973421e-02_pm_reel,  0.128326e-01_pm_reel,  0.788553e-02_pm_reel, &
    0.797197e-02_pm_reel, -0.120760e-01_pm_reel, -0.767547e-02_pm_reel, -0.120755e-02_pm_reel, -0.298523e-01_pm_reel, &
   -0.126560e-01_pm_reel,  0.000000e+00_pm_reel, -0.568350e-01_pm_reel, -0.153039e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.242911e-02_pm_reel, -0.401347e-02_pm_reel, -0.219074e-02_pm_reel,  0.311281e+01_pm_reel, &
    0.323251e-02_pm_reel, -0.639523e-02_pm_reel, -0.663069e-02_pm_reel, -0.304403e-03_pm_reel, -0.401920e-02_pm_reel, &
   -0.118708e-02_pm_reel,  0.415211e+01_pm_reel, -0.201896e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PC1= (/ &       ! donnees PC1
    0.106903e+01_pm_reel,  0.377113e-03_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.898481e-01_pm_reel, -0.236325e+02_pm_reel,  0.208180e-01_pm_reel,  0.139638e+03_pm_reel, -0.119444e+00_pm_reel, &
   -0.845398e+01_pm_reel, -0.399776e-05_pm_reel,  0.000000e+00_pm_reel,  0.366210e-02_pm_reel, -0.178929e-02_pm_reel, &
    0.190412e-01_pm_reel, -0.392257e-01_pm_reel,  0.632343e-02_pm_reel,  0.548144e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.243022e-02_pm_reel, &
    0.976619e+00_pm_reel,  0.568478e-03_pm_reel,  0.582026e-02_pm_reel,  0.000000e+00_pm_reel,  0.621998e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.107674e-01_pm_reel,  0.893820e+02_pm_reel, -0.192414e-01_pm_reel, &
   -0.845398e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.200200e-01_pm_reel, -0.195833e-02_pm_reel, &
   -0.938391e-02_pm_reel,  0.131480e-01_pm_reel, -0.260147e-02_pm_reel, -0.808556e-03_pm_reel,  0.511651e-04_pm_reel, &
    0.255717e-02_pm_reel,  0.000000e+00_pm_reel,  0.466814e-02_pm_reel,  0.664196e-02_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PC2= (/ &       ! donnees PC2
    0.998594e+00_pm_reel,  0.190038e-03_pm_reel,  0.000000e+00_pm_reel, -0.243825e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.522105e-01_pm_reel, &
   -0.845398e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.767271e-02_pm_reel,  0.564539e-02_pm_reel, -0.270623e-02_pm_reel, -0.526454e-03_pm_reel,  0.137075e-02_pm_reel, &
    0.133060e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.949197e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.768008e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.137993e-01_pm_reel, -0.140136e+01_pm_reel,  0.120481e+00_pm_reel, &
   -0.845398e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.987746e-02_pm_reel,  0.175330e-02_pm_reel, -0.688835e-03_pm_reel,  0.287022e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.744513e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PC3= (/ &       ! donnees PC3
    0.152840e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.116252e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.649190e+00_pm_reel, &
   -0.845398e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.584949e-01_pm_reel, -0.102105e+00_pm_reel,  0.299153e-01_pm_reel, -0.486227e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PD1= (/ &       ! donnees PD1
    0.931402e+00_pm_reel,  0.137976e+00_pm_reel,  0.000000e+00_pm_reel,  0.323736e-03_pm_reel,  0.000000e+00_pm_reel, &
   -0.910906e-02_pm_reel,  0.707506e-01_pm_reel,  0.000000e+00_pm_reel, -0.516650e-01_pm_reel,  0.689755e-01_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.845398e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.281140e-01_pm_reel,  0.000000e+00_pm_reel,  0.736009e+02_pm_reel,  0.596604e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.151792e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.948758e+01_pm_reel,  0.884541e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
    0.145192e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PD2= (/ &       ! donnees PD2
    0.107906e-01_pm_reel,  0.299942e-04_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.148930e-01_pm_reel, &
   -0.787184e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.683420e-01_pm_reel, -0.441778e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.229730e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PD3= (/ &       ! donnees PD3
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PE1= (/ &       ! donnees PE1
    0.868053e+00_pm_reel,  0.236364e+00_pm_reel,  0.134306e+00_pm_reel,  0.103086e-01_pm_reel,  0.000000e+00_pm_reel, &
   -0.379164e-02_pm_reel, -0.157806e+00_pm_reel,  0.000000e+00_pm_reel, -0.587644e-01_pm_reel, -0.312508e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.437387e-01_pm_reel, -0.354091e-01_pm_reel, -0.223636e+02_pm_reel,  0.000000e+00_pm_reel, &
   -0.533976e-01_pm_reel,  0.000000e+00_pm_reel,  0.114091e+03_pm_reel,  0.517497e-01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.342702e+03_pm_reel,  0.157033e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.366278e-02_pm_reel, &
   -0.116193e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
    0.178431e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PE2= (/ &       ! donnees PE2
    0.162864e-01_pm_reel,  0.316963e-04_pm_reel,  0.127968e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.704599e-02_pm_reel,  0.207921e-02_pm_reel,  0.636660e-02_pm_reel,  0.229940e+05_pm_reel,  0.000000e+00_pm_reel, &
    0.127833e-01_pm_reel, -0.208036e-02_pm_reel, -0.461820e-02_pm_reel, -0.629391e+02_pm_reel, -0.120745e-01_pm_reel, &
    0.136675e-01_pm_reel,  0.136011e-01_pm_reel, -0.537162e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.192509e+05_pm_reel,  0.835522e-02_pm_reel,  0.419439e-02_pm_reel,  0.000000e+00_pm_reel,  0.120366e+05_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.100034e-01_pm_reel, -0.233267e-02_pm_reel, &
    0.972374e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.265079e-01_pm_reel, -0.209125e-01_pm_reel, -0.109465e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.217252e-01_pm_reel, -0.712385e+02_pm_reel, -0.189428e-02_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PE3= (/ &       ! donnees PE3
   -0.602006e-02_pm_reel,  0.169058e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.290646e-01_pm_reel, &
    0.348971e-02_pm_reel,  0.000000e+00_pm_reel,  0.501174e-01_pm_reel,  0.550595e-01_pm_reel,  0.000000e+00_pm_reel, &
   -0.955897e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.151693e+04_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.129306e-01_pm_reel,  0.269567e-02_pm_reel,  0.000000e+00_pm_reel,  0.392243e+01_pm_reel, &
   -0.847690e-02_pm_reel,  0.116896e-01_pm_reel,  0.000000e+00_pm_reel,  0.148967e-01_pm_reel,  0.544521e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.564918e+01_pm_reel,  0.000000e+00_pm_reel, -0.772178e-02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.734042e+02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PF1= (/ &       ! donnees PF1
    0.127515e+01_pm_reel, -0.210472e+00_pm_reel, -0.177924e+00_pm_reel,  0.218900e+00_pm_reel,  0.288436e-01_pm_reel, &
    0.190077e-01_pm_reel,  0.291001e+00_pm_reel,  0.217437e-01_pm_reel, -0.105186e-01_pm_reel,  0.436141e+00_pm_reel, &
    0.107605e+00_pm_reel,  0.330755e-01_pm_reel,  0.400581e-01_pm_reel, -0.958051e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.154028e-01_pm_reel,  0.000000e+00_pm_reel,  0.734194e+02_pm_reel,  0.496540e-01_pm_reel, -0.595906e-02_pm_reel, &
    0.384512e-04_pm_reel, -0.136000e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel, -0.416610e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.146276e+03_pm_reel, -0.198408e-01_pm_reel,  0.000000e+00_pm_reel,  0.132530e-01_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.104687e-03_pm_reel, &
   -0.147562e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
   -0.126913e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.608370e-02_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PF2= (/ &       ! donnees PF2
   -0.257587e-01_pm_reel,  0.319022e-04_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.156644e-01_pm_reel, &
    0.103640e-01_pm_reel,  0.105771e-02_pm_reel,  0.000000e+00_pm_reel,  0.357949e+04_pm_reel,  0.000000e+00_pm_reel, &
   -0.125672e-02_pm_reel,  0.152783e-02_pm_reel,  0.130518e-02_pm_reel,  0.755558e+01_pm_reel, -0.920341e-02_pm_reel, &
   -0.209142e-01_pm_reel, -0.134106e-01_pm_reel,  0.000000e+00_pm_reel, -0.483312e-01_pm_reel,  0.830900e-01_pm_reel, &
    0.988009e-01_pm_reel, -0.141148e+05_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.105513e+04_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.673442e-02_pm_reel,  0.201691e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.598019e-01_pm_reel,  0.633298e-02_pm_reel, -0.112871e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.128604e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PF3= (/ &       ! donnees PF3
   -0.494960e-02_pm_reel, -0.136415e-01_pm_reel, -0.115039e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.586860e-02_pm_reel, -0.141732e-02_pm_reel,  0.213697e-02_pm_reel,  0.263845e+01_pm_reel, &
   -0.834186e-02_pm_reel, -0.187336e-01_pm_reel, -0.190870e-01_pm_reel, -0.803810e-02_pm_reel, -0.284279e-02_pm_reel, &
    0.256722e-02_pm_reel,  0.171429e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PG1= (/ &       ! donnees PG1
    0.573587e+02_pm_reel, -0.398747e+00_pm_reel,  0.000000e+00_pm_reel, -0.529554e+00_pm_reel, -0.582186e-02_pm_reel, &
    0.714177e-01_pm_reel, -0.679279e+00_pm_reel, -0.167715e+00_pm_reel, -0.642434e-01_pm_reel, -0.211569e+00_pm_reel, &
   -0.159922e+00_pm_reel, -0.171024e-03_pm_reel, -0.115885e+00_pm_reel,  0.651603e+01_pm_reel,  0.000000e+00_pm_reel, &
   -0.176683e+00_pm_reel,  0.650395e-01_pm_reel,  0.143504e+01_pm_reel,  0.928208e-01_pm_reel,  0.511662e-02_pm_reel, &
    0.000000e+00_pm_reel,  0.995121e-02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel,  0.101451e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.567667e+02_pm_reel,  0.238192e-02_pm_reel,  0.000000e+00_pm_reel, -0.188240e-01_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.476218e-01_pm_reel,  0.235206e+02_pm_reel,  0.475901e-02_pm_reel, &
    0.576162e-02_pm_reel,  0.151815e-01_pm_reel, -0.192730e-01_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
   -0.288771e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.118418e-02_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PG2= (/ &       ! donnees PG2
   -0.368927e-02_pm_reel,  0.314704e-04_pm_reel,  0.882198e-02_pm_reel,  0.000000e+00_pm_reel, -0.192562e-01_pm_reel, &
   -0.258674e-02_pm_reel, -0.219913e-01_pm_reel,  0.000000e+00_pm_reel,  0.438655e+04_pm_reel,  0.000000e+00_pm_reel, &
    0.760126e-02_pm_reel,  0.259438e-02_pm_reel,  0.172310e-02_pm_reel,  0.779204e+02_pm_reel,  0.797786e-03_pm_reel, &
   -0.770510e-02_pm_reel,  0.190982e-02_pm_reel,  0.272707e-02_pm_reel,  0.101016e-01_pm_reel,  0.116537e+00_pm_reel, &
   -0.312236e-02_pm_reel,  0.139783e+05_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
   -0.130712e+04_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.320544e-02_pm_reel, -0.206970e-01_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.159010e-01_pm_reel, -0.191427e-02_pm_reel, -0.342829e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.345379e-01_pm_reel,  0.894518e+02_pm_reel,  0.171556e-02_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PG3= (/ &       ! donnees PG3
   -0.765278e-02_pm_reel, -0.208987e-03_pm_reel, -0.157393e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel, -0.860673e-02_pm_reel, -0.119922e-01_pm_reel, -0.646356e-02_pm_reel, -0.300107e+01_pm_reel, &
   -0.932511e-02_pm_reel, -0.150205e-01_pm_reel, -0.867835e-02_pm_reel, -0.764801e-02_pm_reel, -0.131495e-01_pm_reel, &
   -0.676720e-02_pm_reel, -0.182396e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PH1= (/ &       ! donnees PH1
    0.951363e+00_pm_reel, -0.467542e-01_pm_reel,  0.120260e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.191357e-01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.125429e-02_pm_reel, -0.133240e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.845398e+01_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.252317e-02_pm_reel,  0.000000e+00_pm_reel, -0.973404e-02_pm_reel,  0.132397e+00_pm_reel, &
    0.213315e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, -0.718482e-03_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.787683e-02_pm_reel, -0.233698e-02_pm_reel,  0.113139e+00_pm_reel,  0.169134e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PH2= (/ &       ! donnees PH2
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PH3= (/ &       ! donnees PH3
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel/)

real(pm_reel), dimension(50), parameter :: donnees_PI1= (/ &       ! donnees PI1
    0.933804e+00_pm_reel,  0.547446e+01_pm_reel,  0.153263e+00_pm_reel,  0.919303e+00_pm_reel,  0.164109e+02_pm_reel, &
    0.427083e+01_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.115897e+01_pm_reel,  0.471094e+00_pm_reel,  0.109459e+01_pm_reel,  0.525012e+01_pm_reel,  0.100000e+01_pm_reel, &
    0.100000e+01_pm_reel,  0.103999e+01_pm_reel,  0.767132e+00_pm_reel,  0.110514e+01_pm_reel,  0.175636e+01_pm_reel, &
    0.110845e+01_pm_reel,  0.233439e+01_pm_reel,  0.796532e+00_pm_reel,  0.431520e+01_pm_reel,  0.407300e+01_pm_reel, &
    0.101885e+01_pm_reel,  0.239547e+00_pm_reel,  0.253791e-05_pm_reel,  0.842931e+00_pm_reel,  0.104192e+01_pm_reel, &
    0.200202e+01_pm_reel,  0.100000e+01_pm_reel,  0.100000e+01_pm_reel,  0.100000e+01_pm_reel,  0.100000e+01_pm_reel/)


real(pm_reel), dimension(8), parameter :: donnees_PTM= (/ &       ! donnees PTM
    0.104130e+04_pm_reel,  0.386000e+03_pm_reel,  0.190000e+03_pm_reel,  0.166728e+02_pm_reel,  0.115000e+03_pm_reel, &
    0.120000e+03_pm_reel,  0.945537e+02_pm_reel,  0.000000e+00_pm_reel/)


real(pm_reel), dimension(8*7), parameter :: donnees_PDM_tab= (/ &       ! donnees PDM
    0.245600e+08_pm_reel,  0.671072e-05_pm_reel, &
    0.100000e+03_pm_reel,  0.000000e+00_pm_reel,  0.110000e+03_pm_reel,  0.100000e+02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.859400e+11_pm_reel,  0.540000e+00_pm_reel,  0.105000e+03_pm_reel, -0.800000e+01_pm_reel, &
    0.110000e+03_pm_reel,  0.100000e+02_pm_reel,  0.900000e+02_pm_reel,  0.200000e+01_pm_reel,  0.281000e+12_pm_reel, &
    0.000000e+00_pm_reel,  0.105000e+03_pm_reel,  0.280000e+02_pm_reel,  0.289500e+02_pm_reel,  0.000000e+00_pm_reel, &
    0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.330000e+11_pm_reel,  0.268270e+00_pm_reel,  0.105000e+03_pm_reel, &
    0.000000e+00_pm_reel,  0.110000e+03_pm_reel,  0.100000e+02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel, &
    0.133000e+10_pm_reel,  0.119615e-01_pm_reel,  0.105000e+03_pm_reel,  0.000000e+00_pm_reel,  0.110000e+03_pm_reel, &
    0.100000e+02_pm_reel,  0.000000e+00_pm_reel,  0.000000e+00_pm_reel,  0.176100e+06_pm_reel,  0.100000e+01_pm_reel, &
    0.950000e+02_pm_reel, -0.800000e+01_pm_reel,  0.110000e+03_pm_reel,  0.100000e+02_pm_reel,  0.900000e+02_pm_reel, &
    0.200000e+01_pm_reel,  0.100000e+08_pm_reel,  0.100000e+01_pm_reel,  0.105000e+03_pm_reel, -0.800000e+01_pm_reel, &
    0.110000e+03_pm_reel,  0.100000e+02_pm_reel,  0.900000e+02_pm_reel,  0.200000e+01_pm_reel/)

real(pm_reel), dimension(8,7), parameter :: donnees_PDM=reshape(donnees_PDM_tab,(/8,7/)) ! donnees PDM redimensionnees


! Transfert dans les COMMONS utilises au niveau du code fortran 77

real(pm_reel), dimension(50) :: PT1,PT2,PT3,PA1,PA2,PA3,PB1,PB2,PB3, &     ! reunion des tab de donnees
                                PC1,PC2,PC3,PD1,PD2,PD3,PE1,PE2,PE3, &
                                PF1,PF2,PF3,PG1,PG2,PG3,PH1,PH2,PH3,PI1

COMMON/PARM5_COMPAS_MSIS86/PT1,PT2,PT3,PA1,PA2,PA3,PB1,PB2,PB3, &         ! distribution en common
             PC1,PC2,PC3,PD1,PD2,PD3,PE1,PE2,PE3, &
             PF1,PF2,PF3,PG1,PG2,PG3,PH1,PH2,PH3,PI1


real(pm_reel), dimension(8)   :: PTM       ! donnees PTM
real(pm_reel), dimension(8,7) :: PDM       ! donnees PDM

COMMON/LOWER5_COMPAS_MSIS86/PTM,PDM


real(pm_reel), dimension(25)  :: SW, SWC    ! switchs

COMMON/CSW_COMPAS_MSIS86/SW,SWC


!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK


!***********************************************************************
!*FON CHARGEMENT DU COMMON CSW DES SWITCHES
!***********************************************************************
SW(1:25) = 1._pm_reel
SWC(1:25)= 1._pm_reel
SW(9)    =-1._pm_reel

!***********************************************************************
!*FON CHARGEMENT DES COMMONS PARM5 ET LOWER5
!***********************************************************************

PT1(:) = donnees_PT1(:)
PT2(:) = donnees_PT2(:)
PT3(:) = donnees_PT3(:)
PA1(:) = donnees_PA1(:)
PA2(:) = donnees_PA2(:)
PA3(:) = donnees_PA3(:)
PB1(:) = donnees_PB1(:)
PB2(:) = donnees_PB2(:)
PB3(:) = donnees_PB3(:)
PC1(:) = donnees_PC1(:)
PC2(:) = donnees_PC2(:)
PC3(:) = donnees_PC3(:)
PD1(:) = donnees_PD1(:)
PD2(:) = donnees_PD2(:)
PD3(:) = donnees_PD3(:)
PE1(:) = donnees_PE1(:)
PE2(:) = donnees_PE2(:)
PE3(:) = donnees_PE3(:)
PF1(:) = donnees_PF1(:)
PF2(:) = donnees_PF2(:)
PF3(:) = donnees_PF3(:)
PG1(:) = donnees_PG1(:)
PG2(:) = donnees_PG2(:)
PG3(:) = donnees_PG3(:)
PH1(:) = donnees_PH1(:)
PH2(:) = donnees_PH2(:)
PH3(:) = donnees_PH3(:)
PI1(:) = donnees_PI1(:)

PTM(:) = donnees_PTM(:)
PDM(:,:) = donnees_PDM(:,:)


end subroutine cps_data_msis86


end module cps_atm_msis86_mod
