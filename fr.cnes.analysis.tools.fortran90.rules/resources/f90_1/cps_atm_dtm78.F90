module cps_atm_dtm78_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_dtm78_mod
!
!$Resume
!  Modele d'atmosphere DTM78.
!
!$Description
!  Modele d'atmosphere DTM78.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Version
!  $Id: cps_atm_dtm78.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: cps_atm_dtm78.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.10  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2009/11/13 14:04:32  cmartel
!  AQ : Utilisation d'un format fixe pour un message d'erreur
!
!  Revision 1.8  2009/05/28 08:14:48  cml
!  FA-ID 1302 : Amelioration des tests d'erreur dans DTM78
!
!  Revision 1.7  2008/10/14 07:47:58  cml
!  DM-ID 1058 : Ajout d initialisations
!  Revision 1.6  2008/10/01 16:11:05  cml
!  DM-ID 991 : Remplacement des fonctions sur les dates COMPAS par des appels a la MSLIB
!  Revision 1.5  2008/03/13 18:03:19  vivaresf
!  DM-ID 553 : portage Solaris 10, précision
!  Revision 1.4  2006/11/10 10:48:16  vivaresf
!  Version 2.1 : validation
!  Revision 1.3  2006/05/12 12:05:56  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.2  2006/04/11 09:16:59  bouillaj
!  Introduction des includes dans le modeles
!  Revision 1.1  2006/01/30 09:08:01  bouillaj
!  creation a partir de la routine MSPRO mp_atm_dtm78
!
!$FinHistorique
!
!$Usage
!  use cps_atm_dtm78_mod
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_dtm78
!#V
!- cps_zoom_anjour
!#
!
!$Fonctions
!- cps_zoom_EGALITE
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  cps_zoom_anjour
!#
!.  cps_zoom_EGALITE cps_atm_dtm78
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mslib
  use msp_gestion_erreur

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_dtm78.F90 355 2013-02-14 12:16:41Z aadt $'


  private cps_zoom_anjour, cps_zoom_MDTM, cps_zoom_PLGD33

contains

  subroutine cps_atm_dtm78 (date,flux_veille,flux_3rot,ap_3h,lat,alt,heure_sol, &
                          dens,inv_haut_ech)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atm_dtm78
!
!$Resume
!  Ce modele d'atmosphere est limite a des altitudes supérieures à 120 km 
!
!$Description
!  Ce modele d'atmosphere est limite a des altitudes supérieures à 120 km 
!  au delà un message d'avertissement est généré et la densité à 120 km 
!  est renvoyée.
!
!$Auteur
!  Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atm_dtm78 (date,flux_veille,flux_3rot,ap_3h,lat,alt,heure_sol, &
!.                              dens,[inv_haut_ech])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_3rot 
!.    real(pm_reel) :: ap_3h 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: heure_sol 
!.    real(pm_reel) :: dens 
!.    real(pm_reel) :: inv_haut_ech 
!
!$Arguments
!>E     date          :<tm_jour_sec>   date julienne 1950 (jj)
!>E     flux_veille   :<pm_reel>       flux solaire du jour precedent
!>E     flux_3rot     :<pm_reel>       flux solaire moyen sur les 3 dernieres rotations solaires
!>E     ap_3h         :<pm_reel>       indice geomagnetique ap des 3 heures precedentes
!>E     lat           :<pm_reel>       latitude geodesique (rad)
!>E     alt           :<pm_reel>       altitude geodesique (m)
!>E     heure_sol     :<pm_reel>       heure solaire locale (rad)
!>S     dens          :<pm_reel>       densite atmospherique (kg.m-3)
!>[S]   inv_haut_ech  :<pm_reel>       inverse de la hauteur d'echelle (m)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!#V
!- cps_zoom_MDTM
!#
!
!$Include
!
!$Module
!#V
!- mslib
!- msp_gestion_erreur
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
    use msp_gestion_erreur
    use cps_constantes, only : CPS_MAXLG
  
    ! Declarations
    ! ============
    implicit none
    
    type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
    real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
    real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 dernieres rotations solaires
    real(pm_reel), intent(in)               :: ap_3h        ! indice geomagnetique ap des 3 heures precedentes                                         
    real(pm_reel), intent(in)               :: lat          ! latitude geodesique
    real(pm_reel), intent(in)               :: alt          ! altitude geodesique
    real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
    real(pm_reel), intent(out)              :: dens         ! densite atmospherique
    real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle
    
    ! Autres declarations
    ! ===================
    
    ! Altitude minimum (en dessous on renvoie la densité à cette limite
    real(pm_reel), parameter    :: ALT_MIN_DTM78 = 120.e3_pm_reel
    ! Date maximum admissible pour un jour julien 1950 (54787 = 01/01/2100 a 00:00:00)
    integer, parameter          :: DATE_MAX_DTM78 = 54787

    ! Declarations pour l'appel a la routine ZOOM fortran 77 mpi_zoom_MDTM
    integer                     :: JJ                  ! date en jour julien CNES
    real(pm_reel)               :: SEC                 ! et secondes dans le jour
    real(pm_reel)               :: F,FBAR              ! flux solaire sur llambda=10.7cm , et moyen
    real(pm_reel)               :: Z,TLOC,TZ           ! altitude, heure loc, temperature exospherique
    real(pm_reel)               :: RHO,DM,ECH          ! masse volumique, moyenne, inverse de la hauteur d'echelle
    real(pm_reel)               :: AP                  ! indice geomagnetique planetaire
    logical                     :: erreur              ! code d'erreur
    character(len=CPS_MAXLG), dimension(3) :: msp_mess ! Structure pour message d'erreur

    intrinsic present
    
    
    !************************************************************************   
    ! initialisations
    ! ===============
    
    ! Initialisation pour suppression des warning
    DM = 0._pm_reel
    ECH = 0._pm_reel
    RHO = 0._pm_reel
    TZ = 0._pm_reel 
    erreur = .false.
    dens = 0._pm_reel

    ! verification des arguments d'entree
    ! ===================================
    
    if (flux_veille < 0._pm_reel) then  ! flux solaire negatif       
       ! Creation du message d'erreur
       msp_mess(1) = "flux_veille"
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_FLUX_NEG", &
             routine="cps_atm_dtm78", &
             partie_variable=msp_mess)
       return
    endif

    if (flux_3rot < 0._pm_reel) then ! flux solaire negatif       
       ! Creation du message d'erreur
       msp_mess(1) = "flux_3rot"
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_FLUX_NEG", &
             routine="cps_atm_dtm78", &
             partie_variable=msp_mess)
       return
    end if
    
    ! pas de normalisation de date faites ici 
    ! compte tenu de l'utilisation jusqu'a present de mpi_zoom_MDTM
    if ((date%jour < 0) .OR. (date%jour > DATE_MAX_DTM78 )) then 
       write(msp_mess(1),*) date%jour
       write(msp_mess(2),*) DATE_MAX_DTM78
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_DTM78_DATE", &
             partie_variable=msp_mess) 
       return             
    end if
        
    
    ! calcul du modele atmospherique DTM78
    ! ======================================
    
    ! pour les unites des donnees en entre et en sortie: 
    ! utilisation des commentaires en debut de code de mpi_zoom_MDTM    
    JJ = date%jour
    SEC = date%sec
    F = flux_veille
    FBAR = flux_3rot
    AP  = ap_3h
    TLOC = heure_sol
    z = alt

    ! Erreur non blocante
    if (z < ALT_MIN_DTM78 ) then ! altitude inferieure a 120 km
       write(msp_mess(1),*) z
       write(msp_mess(2),'(f9.1)') ALT_MIN_DTM78
       call MSP_signaler_message(cle_mes="CPS_WARN_ATM_DTM78_ALT", &
             partie_variable=msp_mess)  

       ! L'altitude est fixée à la limite
       z = ALT_MIN_DTM78     
    end if
    
    ! Lancement du calcul
    call cps_zoom_MDTM (JJ,SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM,ECH,ERREUR)
    if ( .not. erreur) then ! erreur vaut .false. si tout est OK       
       ! Recopie des variables de sortie
       ! =======================
       dens = RHO
       if (present(inv_haut_ech)) inv_haut_ech = ECH

    else
       ! Generation d'un message d'erreur
       msp_mess(1) = "cps_zoom_MDTM"
       call MSP_signaler_message(cle_mes="CPS_ERR_APPEL", &
            routine="cps_atm_dtm78", &            
            partie_variable=msp_mess)         
    end if
        
  end subroutine cps_atm_dtm78

  subroutine cps_zoom_anjour(JJCNES,JAN,JOUR,ERREUR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_anjour
!
!$Resume
! TRANSFORMATION D'UN JOUR JULIEN CNES EN ANNEE CALENDAIRE ET JOUR
! DANS L'ANNEE
!
!$Description
! TRANSFORMATION D'UN JOUR JULIEN CNES EN ANNEE CALENDAIRE ET JOUR
! DANS L'ANNEE
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PRIVE
!
!$Usage
!  call cps_zoom_anjour(JJCNES,JAN,JOUR,ERREUR)
!.    integer :: JJCNES
!.    integer :: JAN
!.    integer :: JOUR
!.    logical :: ERREUR
!
!$Arguments
!>E     JJCNES  :<integer>   JOUR JULIEN CNES
!>S     JAN     :<integer>   ANNEE
!>S     JOUR    :<integer>   JOUR DANS L'ANNEE
!>S     ERREUR  :<logical>   ode d'erreur
!                .FALSE. si OK
!                .TRUE. => Arret de l'execution
!
!$Common
!
!$Routines
!- md_julien_calend
!- md_calend_julien
!
!$Include
!
!$Module
!#V
!- MSLIB
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

use MSLIB, only : md_julien_calend, md_calend_julien

      implicit none

!.. Formal Arguments ..
      integer, intent(in) :: JJCNES
      integer, intent(out) :: JAN
      integer, intent(out) :: JOUR
      logical, intent(out) :: ERREUR

!.. Local Scalars ..
      integer :: jour_tmp, mois_tmp, heures_tmp, min_tmp ! Valeurs tampons pour jour, mois, heure et minute
      type(tm_jour_sec) :: jjcnes_tmp ! Jour julien du 1er janvier a 00h00 de la même année
      real(KIND=pm_reel) :: sec_tmp ! Variable pour les secondes
      type(tm_code_retour) :: code_retour ! Code de retour de la MSLIB

! ... Executable Statements ...

      ERREUR = .false.

!     CALCUL DE L'ANNEE
!     -----------------

      ! Initialisation du code retour et de la structure MSLIB
      code_retour%valeur = 0
      jjcnes_tmp%jour = JJCNES
      jjcnes_tmp%sec = 0._pm_reel

      call md_julien_calend (jjcnes_tmp, JAN, mois_tmp, jour_tmp, &
           heures_tmp, min_tmp, sec_tmp, code_retour)

      if (code_retour%valeur .ne. 0) then
         ! Pour compatibilité, pas de traitement d'erreur particulier        
         return
      endif

!     CALCUL DU JOUR DANS L'ANNEE
!     ---------------------------
!     on recherche le jour julien correspondant au 1er de l'an Jan
!     et on calcule la difference jjcnes-jjcnes(1/1/JAN à 0h0), difference
!     a laquelle on ajoute une unite
!     On réutilise la même structure
      call md_calend_julien(JAN, 1, 1, 0, 0, 0._pm_reel,jjcnes_tmp, code_retour) 

      if (code_retour%valeur .ne. 0) then
         ! Pour compatibilité, pas de traitement d'erreur particulier  
         return
      endif

      ! Mise a jour des sorties
      JOUR = JJCNES - jjcnes_tmp%jour + 1

      end subroutine  cps_zoom_anjour


      logical function cps_zoom_EGALITE(OPER1,OPER2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_EGALITE
!
!$Resume
! Egalite de deux nombres reels double precision.
! Retourne la valeur .true. si les deux nombres
! passes en arguments peuvent etre consideres comme
! egaux, retourne .false. dans le cas contraire.
! EPSILON_MACHINE provient de l'outil MACHAR lance sur sc2000.
! EGALITE = .true. si
! |OPER1-OPER2| <= K*EPSILON_MACHINE*(|OPER1|+|OPER2|)
!
!$Description
! Egalite de deux nombres reels double precision.
! Retourne la valeur .true. si les deux nombres
! passes en arguments peuvent etre consideres comme
! egaux, retourne .false. dans le cas contraire.
! EPSILON_MACHINE provient de l'outil MACHAR lance sur sc2000.
! EGALITE = .true. si
! |OPER1-OPER2| <= K*EPSILON_MACHINE*(|OPER1|+|OPER2|)
!
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!.          logical function cps_zoom_EGALITE(OPER1,OPER2)
!.    real (KIND=PM_REEL) :: OPER1,OPER2
!
!$Arguments
!>E     OPER1  :<PM_REEL>   premier nombre reel double precision
!>E     OPER2  :<PM_REEL>   deuxieme nombre reel double precision
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
        real (KIND=PM_REEL), intent(IN) :: OPER1,OPER2
        
        integer, parameter :: K =10
        
        real (KIND=PM_REEL), parameter :: EPSILON_MACHINE = 0.2220446d-15
        
        if (ABS(OPER1-OPER2) .le. &
             (DBLE(K)*EPSILON_MACHINE*(ABS(OPER1)+ABS(OPER2)))) then
           CPS_ZOOM_EGALITE = .true.
        else
           CPS_ZOOM_EGALITE = .false.
        endif
        
      end function  cps_zoom_EGALITE
      
      subroutine cps_zoom_MDTM(JJ,SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM, &
           ECH,ERREUR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_MDTM
!
!$Resume
! MODELE D'ATMOSPHERE DTM + HYDROGENE (BARLIER & AL. 1978)
! CALCUL DE LA TEMPERATURE ET DE LA DENSITE TOTALE
!$Description
! MODELE D'ATMOSPHERE DTM + HYDROGENE (BARLIER & AL. 1978)
! CALCUL DE LA TEMPERATURE ET DE LA DENSITE TOTALE
!$Auteur
!julien Bouillant (ATOS ORIGIN)
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_zoom_MDTM(JJ,SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM, &
!.                                   ECH,ERREUR)
!.    integer :: JJ
!.    logical :: ERREUR
!.    real (KIND=PM_REEL) :: SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM,ECH
!
!$Arguments
!>E/S   JJ      :<integer>   JOUR JULIEN CNES
!>E/S   SEC     :<PM_REEL>   SECONDES
!>E/S   F       :<PM_REEL>   FLUX SOLAIRE MESURE SUR LONGUEUR D'ONDE 10,7 CM
!                            RETARDE DE DTF (OBTENU PAR UN APPEL PRELIMINAIRE A DXXX)
!>E/S   FBAR    :<PM_REEL>   FLUX SOLAIRE MOYEN  (UNITES DE FLUX)
!                            ( FENETRE GAUSSIENNE - SIGMA = 3 ROTATIONS )
!>E/S   AP      :<PM_REEL>   INDICE GEOMAGNETIQUE PLANETAIRE (VALEUR ENTRE 0 ET 21)
!                            RETARDE DE DTG (OBTENU PAR UN APPEL PRELIMINAIRE A DXXX)
!>E/S   Z       :<PM_REEL>   ALTITUDE      (M )
!>E/S   LAT     :<PM_REEL>   LATITUDE      (RD)
!>E/S   TLOC    :<PM_REEL>   HEURE LOCALE  (RD)
!>E/S   RHO     :<PM_REEL>   MASSE VOLUMIQUE          (KG/M**3)
!>E/S   TZ      :<PM_REEL>   TEMPERATURE EXOSPHERIQUE (KELVIN)
!>E/S   DM      :<PM_REEL>   MASSE VOLUMIQUE MOYENNE  (KG/MOLE)
!>E/S   ECH     :<PM_REEL>   INVERSE DE LA HAUTEUR D'ECHELLE (1/M)
!>E/S   ERREUR  :<logical>   Code d'erreur
!                            .FALSE. si OK
!                            .TRUE. => Arret de l'execution
!
!$Common
!
!$Routines
!- cps_zoom_anjour
!- cps_zoom_PLGD33
!- cps_zoom_TRAPKP
!
!$Include
!#V
!#
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
        
        !.. Parameters .. 
        
        real(8), parameter :: GAZ_PARFAITS = 8.3143d0 
        !     CONSTANTE DES GAZ PARFAITS (J/K.mol)

        
        !       parameter sur les conversions entre les durees classiques
        !
        double precision SECONDES_PAR_JOUR
        parameter (SECONDES_PAR_JOUR = 86400.d0)
        !
        double precision HEURES_PAR_JOUR
        parameter (HEURES_PAR_JOUR = 24.d0)
        !
        integer MOIS_PAR_AN
        parameter (MOIS_PAR_AN = 12)
        !
        double precision JOUR_PAR_SEMAINE
        parameter (JOUR_PAR_SEMAINE = 7d0)
        !
        double precision JOURS_PAR_AN
        parameter (JOURS_PAR_AN = 365.25d0)
        !
        double precision ANNEES_PAR_SIECLE
        parameter (ANNEES_PAR_SIECLE = 100.d0)
        !
        double precision JOURS_PAR_SIECLE
        parameter (JOURS_PAR_SIECLE = ANNEES_PAR_SIECLE*JOURS_PAR_AN)
        !
        double precision  SECONDES_PAR_MINUTE
        parameter (SECONDES_PAR_MINUTE = 60.0d0)
        !
        double precision MINUTES_PAR_HEURE
        parameter (MINUTES_PAR_HEURE = 60.0d0)
        !
        double precision SECONDES_PAR_HEURE
        parameter (SECONDES_PAR_HEURE = 3600.0d0)
        !
        double precision MINUTES_PAR_JOUR
        parameter (MINUTES_PAR_JOUR = 1440.0d0)
        !
        double precision SECONDES_PAR_SIECLE
        parameter (SECONDES_PAR_SIECLE=SECONDES_PAR_JOUR*JOURS_PAR_SIECLE)
        
        
        !.. Parameters .. 
        double precision PI
        parameter (PI = 3.14159265358979323846264338327950288d0)
        !
        double precision DEUX_PI      
        parameter (DEUX_PI = 2.d0*PI)
        !
        double precision PI_SUR_DEUX
        parameter (PI_SUR_DEUX = PI/2.d0)
        
        !     Conversion d'un angle exprime en deg vers rad
        double precision DEGRES_RADIANS
        parameter (DEGRES_RADIANS = PI/180.d0)
        
        !     Conversion d'un angle exprime en rad vers deg
        double precision RADIANS_DEGRES
        parameter (RADIANS_DEGRES = 180.d0/PI)
        
        !     Conversion d'un angle exprime en degres vers minutes d'arc
        double precision DEGRES_MINUTES_ARC
        parameter (DEGRES_MINUTES_ARC = 60.d0)
        
        !     Conversion d'un angle exprime en minutes vers secondes d'arc
        double precision MINUTES_SECONDES_ARC
        parameter (MINUTES_SECONDES_ARC = 60.d0)
        
        !     Conversion d'un angle exprime en degres vers secondes d'arc
        double precision DEGRES_SECONDES_ARC
        parameter (DEGRES_SECONDES_ARC = 3600.d0)
        
        !     Conversion d'un angle exprime en secondes d'arc vers radians
        double precision SECONDES_RADIANS
        parameter (SECONDES_RADIANS = (PI/180.d0)/3600.d0)
        
        !     Conversion d'un angle exprime en radians vers secondes d'arc
        double precision RADIANS_SECONDES_ARC
        parameter (RADIANS_SECONDES_ARC = 180.d0*3600.d0/PI)
        
        
        !.. Parameters ..
        real (KIND=PM_REEL), parameter :: RE = 6356.77d3
        !   RAYON TERRESTRE (POLAIRE)
        real (KIND=PM_REEL), parameter :: GSURF = 9.80665d0
        !   ACCELERATION DE LA PESANTEUR A LA SURFACE
        real (KIND=PM_REEL), parameter :: USAVOG = 1.6603d-24
        !   INVERSE DE LA CONSTANTE D'AVOGADRO
        
        ! Initialisation des donnees du modele
        
        
        real (KIND=PM_REEL), parameter :: ZL = 120.d3
        real (KIND=PM_REEL), parameter :: TL = 380.d0
        real (KIND=PM_REEL), parameter :: GL = GSURF/((1.d0+ZL/RE)**2)
        real (KIND=PM_REEL) :: SSS = 2.d-5
        real (KIND=PM_REEL) :: SIGMA
        
        !.. Formal Arguments ..
        integer :: JJ
        logical :: ERREUR
        real (KIND=PM_REEL) :: SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM,ECH
        
        !.. Local Scalars ..
        integer :: I,IAN,JOUR_JULIEN
        real (KIND=PM_REEL) :: AAA,AKP,DAY,DN,DZETA,EXPSZ,FACT1,FACT2,FACT3,FF0, &
             GAMMA,RHOA,RHOG,RHOI,TINF,TO2,ZETA
        
        !.. Local Arrays ..
        real (KIND=PM_REEL) :: A(36),ALEFA(5),AMA(5),B(36),C(36),DBASE(5),H(36), &
             OGO(36)
           
        !.. Data Declarations ..
        
        data TO2/4.775d16/
        data ALEFA/-0.40d0,-0.38d0,0.d0,0.d0,0.d0/
        data AMA/1.d-3,4.d-3,16.d-3,28.d-3,32.d-3/
        
        SIGMA = SSS+1.d0/(RE+ZL)
        
        
        !   TEMPERATURE MODELE M2
        !   ---------------------
        data OGO/ &
             999.8d0,-0.36357d-02,0.24593d-01,0.13259d-02,-0.56234d-05, &
             0.25361d-02,0.17656d-01,0.33677d-01,-0.37643d-02,0.17452d-01, &
             -0.21150d03,-0.27270d-02,0.27465d-01,-0.95216d02,-0.13373d00, &
             -0.27321d-01,-0.96732d-02,-0.14584d02,-0.27469d-01, &
             -0.17398d03,-0.66567d-01,-0.59604d-02,0.67446d-02, &
             -0.26620d-01,0.14691d-01,-0.10971d00,0.88700d-02,0.36918d-02, &
             0.12219d-01,-0.76358d-02,-0.44894d-02,0.23646d-02, &
             0.50569d-02,0.10792d-02,-0.71610d-03,0.96385d-03/
        
        !   HELIUM
        !   ------
        data A/ &
             3.d+13,0.12000d00,-0.15000d00,0.23799d-02,-0.31008d-04, &
             0.56980d-02,0.17103d-01,-0.17997d00,-0.13251d00,-0.64239d-01, &
             0.22136d03,0.24859d00,-0.17732d00,0.10541d03,-0.11071d01, &
             -0.36255d-01,-0.10180d00,-0.19548d03,0.11711d00,-0.21532d03, &
             -0.31594d00,0.52452d-01,-0.31686d-01,-0.13975d00,0.83399d-01, &
             0.21382d00,-0.61816d-01,-0.15026d-01,0.10574d00,-0.97446d-01, &
             0.22606d-01,0.12125d-01,-0.22391d-01,-0.24648d-02, &
             0.32432d-02,-0.57766d-02/
        
        !   OXYGENE ATOMIQUE
        !   ----------------
        data B/ &
             0.93d+17,-0.16598d-02,-0.99095d-01,0.78453d-03,-0.23733d-04, &
             0.80001d-02,0.70000d-01,-0.18000d00,0.14597d00,0.10517d00, &
             0.37357d01,0.24620d00,-0.50845d-01,0.10775d03,0.39103d00, &
             0.96719d-01,0.12624d00,-0.16608d02,-0.14463d00,0.10964d03, &
             -0.20686d00,0.82922d-02,-0.30261d-01,0.14237d00,-0.28977d-01, &
             0.22409d00,-0.79313d-01,-0.16385d-01,-0.10113d00,0.65531d-01, &
             0.53655d-01,-0.23722d-02,0.18910d-01,-0.26522d-02, &
             0.83050d-02,-0.38860d-02/
        
        !   AZOTE MOLECULAIRE
        !   -----------------
        data C/ &
             3.842d+17,0.28076d-01,0.48462d-01,-0.81017d-03,0.20983d-04, &
             0.29998d-02,-0.10000d-01,+0.80000d-01,0.53709d-01, &
             -0.13732d00,0.86434d02,0.19930d-01,-0.84711d-01,0.89339d02, &
             -0.49083d-01,0.91420d-02,-0.16362d-01,0.49234d02, &
             -0.46712d-01,0.52774d02,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
           0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/
        
        !   HYDROGENE
        !   ---------
        data H/ &
             1.761d+11,-1.33700d-01,0.d0,-1.24600d-02,0.d0,-1.93000d-02, &
             -6.00000d-02,-0.20000d-02,5.87800d-02,0.d0,0.92270d02,0.d0, &
             0.d0,0.d0,3.30100d-01,1.04500d-01,0.d0,-0.14770d02, &
             -9.06500d-02,-0.72000d02,2.09400d-01,2.83000d-02,0.d0, &
             8.57100d-02,-2.47500d-02,3.83000d-01,2.94100d-02,0.d0, &
             -3.97400d-03,4.35600d-02,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/
        
        ! ... Executable Statements ...
        
        
        ERREUR = .false.
        
        ! Initalisation pour suppression des warning
        AKP = 0._pm_reel
        JOUR_JULIEN = 0

        !***********************************************************************
        !*FON CALCUL DU JOUR DANS L'ANNEE
        !***********************************************************************
        ! DAY = JOUR FRACTIONNAIRE DANS L'ANNEE
        
        call cps_zoom_anjour(JJ,IAN,JOUR_JULIEN,ERREUR)
        if (ERREUR) then
           !        call PROPAGER_ERREUR('MDTM','MPPI_ZOOM_anjour')
           return
        endif
        DAY = DBLE(JOUR_JULIEN-1) + SEC/SECONDES_PAR_JOUR
        
        !***********************************************************************
        !*FON FONCTIONS DE LEGENDRE ET CONDITIONS EN LIMITE BASSE
        !***********************************************************************
        
        call cps_zoom_PLGD33(LAT,TLOC)
        call cps_zoom_TRAPKP(AP,AKP,1,ERREUR)
        if (ERREUR) then
           !        call PROPAGER_ERREUR('MDTM','TRAPKP')
           return
        endif
        
        !   TEMPERATURE EXOSPHERIQUE
        FF0 = 1.d0
        TINF = OGO(1) * CPS_ZOOM_GEDELE(FBAR,F,AKP,DAY,OGO,FF0)
        
        !   HYDROGENE ATOMIQUE
        FF0 = 0.d0
        DBASE(1) = H(1) * EXP(CPS_ZOOM_GEDELE(FBAR,F,AKP,DAY,H,FF0)-1.d0)
        
        !   HELIUM
        FF0 = 0.d0
        DBASE(2) = A(1) * EXP(CPS_ZOOM_GEDELE(FBAR,F,AKP,DAY,A,FF0)-1.d0)
        
        !   OXYGENE ATOMIQUE
        FF0 = 1.d0
        DBASE(3) = B(1) * EXP(CPS_ZOOM_GEDELE(FBAR,F,AKP,DAY,B,FF0)-1.d0)
        
        !   AZOTE MOLECULAIRE
        FF0 = 1.d0
        DBASE(4) = C(1) * EXP(CPS_ZOOM_GEDELE(FBAR,F,AKP,DAY,C,FF0)-1.d0)
        
        DBASE(5) = TO2
        
        !***********************************************************************
        !*FON DEPENDANCE DE L'ALTITUDE : INITIALISATIONS
        !***********************************************************************
        
        ZETA = ((RE+ZL) / (RE+Z)) * (Z-ZL)
        DZETA = ((RE+ZL)/(RE+Z))**2
        EXPSZ = EXP((-SIGMA)*ZETA)
        
        TZ = TINF - (TINF-TL)*EXPSZ
        AAA = 1.d0 - TL/TINF
        FACT3 = AAA * EXPSZ
        FACT1 = 1.d0 / (1.d0-FACT3)
        FACT2 = (1.d0-AAA) * FACT1
        
        !***********************************************************************
        !*FON BOUCLE SUR LES CONSTITUANTS
        !***********************************************************************
        
        DM = 0.d0
        RHO = 0.d0
        RHOG = 0.d0
        RHOA = 0.d0
        
        do I = 1,5
           GAMMA = (GL / (SIGMA*GAZ_PARFAITS)) * AMA(I) / TINF
           DN = DBASE(I) * (FACT2**(1.d0+ALEFA(I)+GAMMA)) * &
                EXP((-SIGMA)*GAMMA*ZETA)
           DM = DM + DN
           RHOI = DN * AMA(I)
           RHO = RHO + RHOI
           RHOG = RHOG + RHOI*GAMMA
           RHOA = RHOA + RHOI*(1.d0+ALEFA(I))
        end do
        
        DM = RHO / DM
        ECH = SIGMA * DZETA * FACT1 * (RHOG+FACT3*RHOA) / RHO
        RHO = USAVOG * RHO
        
      end subroutine  cps_zoom_MDTM
      
      subroutine cps_zoom_PLGD33 (LAT,TLOC)
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_PLGD33
!
!$Resume
! CALCUL DES POLYNOMES DE LEGENDRE ET DES FONCTIONS ASSOCIEES
! DE (2,0) A (6,3). CALCUL DES FONCTIONS TRIGONOMETRIQUES DE
! L'HEURE LOCALE.
! NECESSAIRE POUR LES MODELES D'ATMOSPHERE MSIS ET DTM
!$Description
! CALCUL DES POLYNOMES DE LEGENDRE ET DES FONCTIONS ASSOCIEES
! DE (2,0) A (6,3). CALCUL DES FONCTIONS TRIGONOMETRIQUES DE
! L'HEURE LOCALE.
! NECESSAIRE POUR LES MODELES D'ATMOSPHERE MSIS ET DTM
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_zoom_PLGD33 (LAT,TLOC)
!.    real (KIND=PM_REEL) :: LAT
!.    real (KIND=PM_REEL) :: TLOC
!
!$Arguments
!>E/S   LAT   :<PM_REEL>   LATITUDE EN RADIANS
!>E/S   TLOC  :<PM_REEL>   HEURE LOCALE EN RADIANS
!
!$Common
!
!$Routines
!
!$Include
!#V
!#
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

! Variables globales

!.. Common Blocks .. 
      double precision CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC
      double precision PLG(7,0:3)
      common /LPOLY3_MSPRO/ plg,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC
!
!     PLG    : FONCTIONS DE LEGENDRE ASSOCIEES DE LA LATITUDE
!     ..TLOC : FONCTIONS TRIGONOMETRIQUES DE L'HEURE LOCALE
!              UTILISEES PAR MSIS ET DTM
!


!.. Formal Arguments ..
      real (KIND=PM_REEL) :: LAT
      real (KIND=PM_REEL) :: TLOC

!.. Local Scalars ..
      real (KIND=PM_REEL) :: C,S

! ... Executable Statements ...


!***********************************************************************
!*FON CALCUL DES FONCTIONS DE LEGENDRE DE LA COLATITUDE
!***********************************************************************

      C = SIN(LAT)
      S = COS(LAT)

      PLG(1,0) = C
      PLG(2,0) = 1.5d0*(C**2) - 0.5d0
      PLG(3,0) = (5.d0/3.d0)*C*PLG(2,0) - (2.d0/3.d0)*PLG(1,0)
      PLG(4,0) = (7.d0/4.d0)*C*PLG(3,0) - (3.d0/4.d0)*PLG(2,0)
      PLG(5,0) = (9.d0/5.d0)*C*PLG(4,0) - (4.d0/5.d0)*PLG(3,0)
      PLG(6,0) = (11.d0/6.d0)*C*PLG(5,0) - (5.d0/6.d0)*PLG(4,0)

      PLG(1,1) = S
      PLG(2,1) = 3.d0 * C * S
      PLG(3,1) = (5.d0/2.d0)*C*PLG(2,1) - (3.d0/2.d0)*PLG(1,1)
      PLG(4,1) = (7.d0/3.d0)*C*PLG(3,1) - (4.d0/3.d0)*PLG(2,1)
      PLG(5,1) = (9.d0/4.d0)*C*PLG(4,1) - (5.d0/4.d0)*PLG(3,1)
      PLG(6,1) = (11.d0/5.d0)*C*PLG(5,1) - (6.d0/5.d0)*PLG(4,1)

      PLG(2,2) = 3.d0 * (S**2)
      PLG(3,2) = 5.d0 * C * PLG(2,2)
      PLG(4,2) = (7.d0/2.d0)*C*PLG(3,2) - (5.d0/2.d0)*PLG(2,2)
      PLG(5,2) = 3.d0*C*PLG(4,2) - 2.d0*PLG(3,2)
      PLG(6,2) = (11.d0/4.d0)*C*PLG(5,2) - (7.d0/4.d0)*PLG(4,2)
      PLG(7,2) = (13.d0/5.d0)*C*PLG(6,2) - (8.d0/5.d0)*PLG(5,2)

      PLG(3,3) = (15.d0) * (S**3)
      PLG(4,3) = 7.d0 * C * PLG(3,3)
      PLG(5,3) = (9.d0/2.d0)*C*PLG(4,3) - (7.d0/2.d0)*PLG(3,3)
      PLG(6,3) = (11.d0/3.d0)*C*PLG(5,3) - (8.d0/3.d0)*PLG(4,3)

!***********************************************************************
!*FON CALCUL DES FONCTIONS TRIGONOMETRIQUES DE L'HEURE LOCALE
!***********************************************************************

      STLOC = SIN(TLOC)
      CTLOC = COS(TLOC)
      S2TLOC = 2.d0 * STLOC * CTLOC
      C2TLOC = CTLOC**2 - STLOC**2
      S3TLOC = STLOC*C2TLOC + CTLOC*S2TLOC
      C3TLOC = CTLOC*C2TLOC - STLOC*S2TLOC

      end subroutine  cps_zoom_PLGD33

      subroutine cps_zoom_TRAPKP(APX,AKPX,IND,ERREUR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_TRAPKP
!
!$Resume
! PASSAGE DES AP AUX KP ET INVERSEMENT

!$Description
! PASSAGE DES AP AUX KP ET INVERSEMENT

!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_zoom_TRAPKP(APX,AKPX,IND,ERREUR)
!.    real (KIND=PM_REEL) :: APX
!.    real (KIND=PM_REEL) :: AKPX
!.    integer :: IND
!.    logical :: ERREUR
!
!$Arguments
!>E/S   APX     :<PM_REEL>   
!>E/S   AKPX    :<PM_REEL>   
!>E/S   IND     :<integer>   CLE > 0 : TRANSFORMATION AP--->KP
!>E/S   ERREUR  :<logical>   Code d'erreur
!                            .FALSE. si OK
!                            .TRUE. => Arret de l'execution
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

!.. Formal Arguments ..
      real (KIND=PM_REEL) :: APX
      real (KIND=PM_REEL) :: AKPX
      integer :: IND
      logical :: ERREUR

!.. Local Scalars ..
      integer :: I,IM
      logical :: FIN_TRAITEMENT

!.. Local Arrays ..
      real (KIND=PM_REEL) :: AKP(28),AP(28)

!      logical :: CPS_ZOOM_EGALITE

!.. Data Declarations ..

! ---

      data AP/ &
           0.d0,2.d0,3.d0,4.d0,5.d0,6.d0,7.d0,9.d0,12.d0,15.d0,18.d0, &
           22.d0,27.d0,32.d0,39.d0,48.d0,56.d0,67.d0,80.d0,94.d0,111.d0, &
           132.d0,154.d0,179.d0,207.d0,236.d0,300.d0,400.d0/

      data AKP/ &
           0.d0,0.33d0,0.66d0,1.d0,1.33d0,1.66d0,2.d0,2.33d0,2.66d0, &
           3.d0,3.33d0,3.66d0,4.d0,4.33d0,4.66d0,5.d0,5.33d0,5.66d0, &
           6.d0,6.33d0,6.66d0,7.d0,7.33d0,7.66d0,8.d0,8.33d0,8.66d0,9.d0 &
           /

! ... Executable Statements ...


      ERREUR = .false.
! ---

      if (IND .gt. 0) then

!***********************************************************************
!*FON PASSAGE AP ---> KP
!***********************************************************************

        if (APX .lt. 0.d0) then
!          call SIGNALER_ERREUR_ZOOM('TRAPKP','UN AP NEGATIF',ERREUR)
!          write (STDOUT,1000) APX
          erreur=.true.
          return
        endif
        FIN_TRAITEMENT = .false.
        I = 1
        do while ((I .le. 28) .and. (.not. FIN_TRAITEMENT))

          if (CPS_ZOOM_EGALITE (APX, AP(I))) then
            AKPX = AKP(I)
            FIN_TRAITEMENT = .true.
          elseif (APX .lt. AP(I)) then
            IM = I - 1
            AKPX = AKP(IM) + &
                 (AKP(I)-AKP(IM))*(APX-AP(IM))/(AP(I)-AP(IM))
            FIN_TRAITEMENT = .true.
          else
            I = I+1
          endif

        end do

!***********************************************************************
!*FON PASSAGE KP ---> AP
!***********************************************************************

      elseif (AKPX .gt. 9.d0) then
        APX = 400.d0
      else

        IM = INT(3.d0*AKPX) + 1
        I = IM + 1
        APX = AP(IM) + (AP(I)-AP(IM))*(AKPX-AKP(IM))/(AKP(I)-AKP(IM))
      endif

! ... Format Declarations ...

      end subroutine  cps_zoom_TRAPKP


      real (KIND=PM_REEL) function cps_zoom_GEDELE(FBAR,F,AKP,DAY,A,FF0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_zoom_GEDELE
!
!$Resume
! CALCUL DE LA FONCTION G(L)
! DEFINITION VOIR HEDIN (J. GEOPHYS. RES. 79, 215-225, 1974)
!$Description
! CALCUL DE LA FONCTION G(L)
! DEFINITION VOIR HEDIN (J. GEOPHYS. RES. 79, 215-225, 1974)
!$Auteur
! Julien Bouillant (ATOS ORIGIN)
!$Acces
!  PUBLIC
!
!$Usage
!.          double precision function cps_zoom_GEDELE(FBAR,F,AKP,DAY,A,FF0)
!.    real (KIND=PM_REEL) :: FBAR
!.    real (KIND=PM_REEL) :: F
!.    real (KIND=PM_REEL) :: AKP
!.    real (KIND=PM_REEL) :: DAY
!.    real (KIND=PM_REEL) :: A(36)
!.    real (KIND=PM_REEL) :: FF0
!
!$Arguments
!>E/S   FBAR  :<PM_REEL>              FLUX SOLAIRE MOYEN A 10.7d0 CM       
!>E/S   F     :<PM_REEL>              FLUX SOLAIRE INSTANTANE A 10.7d0 CM 
!>E/S   AKP   :<PM_REEL>              INDICE GEOMAGNETIQUE PLANETAIRE KP   
!>E/S   DAY   :<PM_REEL>              JOUR FRACTIONNAIRE DANS L'ANNEE
!>E/S   A     :<PM_REEL,DIM=(36)>     TABLEAU DES COEFFICIENTS POUR CALCUL DE G(L) POUR
!                                     LA TEMPERATURE OU CHAQUE CONSTITUANT
!>E/S   FF0   :<PM_REEL>              CLE DE CALCUL ( =0 , HELIUM )
!                                     ( =1 , OXYGENE ATOMIQUE,AZOTE,TEMPERATURE )
!
!$Common
!
!$Routines
!
!$Include
!#V
!#
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

      !.. Parameters .. 
      double precision PI
      parameter (PI = 3.14159265358979323846264338327950288d0)
      !
      double precision DEUX_PI      
      parameter (DEUX_PI = 2.d0*PI)
      !
      double precision PI_SUR_DEUX
      parameter (PI_SUR_DEUX = PI/2.d0)
      
      !     Conversion d'un angle exprime en deg vers rad
      double precision DEGRES_RADIANS
      parameter (DEGRES_RADIANS = PI/180.d0)
      
      !     Conversion d'un angle exprime en rad vers deg
      double precision RADIANS_DEGRES
      parameter (RADIANS_DEGRES = 180.d0/PI)
      
      !     Conversion d'un angle exprime en degres vers minutes d'arc
      double precision DEGRES_MINUTES_ARC
      parameter (DEGRES_MINUTES_ARC = 60.d0)
      
      !     Conversion d'un angle exprime en minutes vers secondes d'arc
      double precision MINUTES_SECONDES_ARC
      parameter (MINUTES_SECONDES_ARC = 60.d0)
      
      !     Conversion d'un angle exprime en degres vers secondes d'arc
      double precision DEGRES_SECONDES_ARC
      parameter (DEGRES_SECONDES_ARC = 3600.d0)
      
      !     Conversion d'un angle exprime en secondes d'arc vers radians
      double precision SECONDES_RADIANS
      parameter (SECONDES_RADIANS = (PI/180.d0)/3600.d0)
      
      !     Conversion d'un angle exprime en radians vers secondes d'arc
      double precision RADIANS_SECONDES_ARC
      parameter (RADIANS_SECONDES_ARC = 180.d0*3600.d0/PI)
      
      
      !.. Common Blocks .. 
      double precision CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC
      double precision PLG(7,0:3)
      common /LPOLY3_MSPRO/ plg,CTLOC,STLOC,C2TLOC,S2TLOC,C3TLOC,S3TLOC
      !
      !     PLG    : FONCTIONS DE LEGENDRE ASSOCIEES DE LA LATITUDE
      !     ..TLOC : FONCTIONS TRIGONOMETRIQUES DE L'HEURE LOCALE
      !              UTILISEES PAR MSIS ET DTM
      !
      
      !.. Parameters ..
      real (KIND=PM_REEL) :: ROT = DEUX_PI/365.d0

      !.. Formal Arguments ..
      real (KIND=PM_REEL) :: FBAR
      real (KIND=PM_REEL) :: F
      real (KIND=PM_REEL) :: AKP
      real (KIND=PM_REEL) :: DAY
      real (KIND=PM_REEL) :: A(36)
      real (KIND=PM_REEL) :: FF0
      
      !.. Local Scalars ..
      real (KIND=PM_REEL) :: ACH,BCH,BETA,CCH,CD18,DCH,DF,DFA,F0,G
      
      
      ! ... Executable Statements ...
      
      
!***********************************************************************
!*FON INITIALISATION
!***********************************************************************

      CD18 = COS(ROT*(DAY-A(18)))

      DF = F - FBAR
      DFA = FBAR - 150.d0
      F0 = A(4)*DF + A(5)*(DF**2) + A(6)*DFA
      BETA = 1.d0 + FF0*F0

!***********************************************************************
!*FON DEPENDANCE 1) ZONALE LATITUDE ,
!*FON            2) DIRECTE FLUX ,
!*FON            3) GEOMAGNETIQUE ,
!*FON            4) ANNUELLE PAIRE (LAT) ,
!*FON            5) SEMI-ANNUELLE PAIRE ,
!*FON            6) ANNUELLE IMPAIRE ,
!*FON            7) SEMI-ANNUELLE IMPAIRE
!***********************************************************************

      G = 1.d0 + A(2)*PLG(2,0) + A(3)*PLG(4,0) + F0 + &
          (A(7)+A(8)*PLG(2,0))*AKP + &
          BETA* &
          ((A(9)+A(10)*PLG(2,0))*COS(ROT*(DAY-A(11)))+ &
           (A(12)+A(13)*PLG(2,0))*COS(2.d0*ROT*(DAY-A(14)))+ &
           (A(15)*PLG(1,0)+A(16)*PLG(3,0)+A(17)*PLG(5,0))*CD18+ &
           A(19)*PLG(1,0)*COS(2.d0*ROT*(DAY-A(20))))

!***********************************************************************
!*FON DEPENDANCE 1) DIURNE ,
!*FON            2) SEMI-DIURNE ,
!*FON            3) TER-DIURNE
!***********************************************************************

      ACH = A(21)*PLG(1,1) + A(22)*PLG(3,1) + A(23)*PLG(5,1)
      BCH = A(26)*PLG(1,1) + A(27)*PLG(3,1) + A(28)*PLG(5,1)
      CCH = A(24)*PLG(1,1) + A(25)*PLG(2,1)
      DCH = A(29)*PLG(1,1) + A(30)*PLG(2,1)

      G = G + &
          BETA* &
          ((ACH+CCH*CD18)*CTLOC+(BCH+DCH*CD18)*STLOC+ &
           (A(31)*PLG(2,2)+A(32)*PLG(3,2)*CD18)*C2TLOC+ &
           (A(33)*PLG(2,2)+A(34)*PLG(3,2)*CD18)*S2TLOC+ &
           A(35)*PLG(3,3)*C3TLOC+A(36)*PLG(3,3)*S3TLOC)

      CPS_ZOOM_GEDELE = G

      end function  cps_zoom_GEDELE

  
    end module cps_atm_dtm78_mod
