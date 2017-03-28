module MSP_ATM_FUNC

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_FUNC
!
!$Resume
!	Module contenant les routines de calcul des modèles terrestres ne nécessitant pas de structuration de donnée
!
!$Description
!	Module contenant les routines de calcul des modèles terrestres ne nécessitant pas de structuration de donnée
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_FUNC.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_ATM_FUNC.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.16  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.15  2009/05/28 13:57:21  cml
!  FA-ID 1302 : Au dessous de 120 km, DTM78 renvoie la densite 120 km
!
!  Revision 1.14  2008/11/18 19:05:49  mercadig
!  DM-ID 733 : Correction de la date passee en argument pour le modele DTM78
!
!  Revision 1.13  2008/11/12 10:16:00  mercadig
!  DM-ID 733: Remplacement des routines mspro de calcul atmosphere par les routines compas equivalentes
!  Revision 1.12  2008/07/04 15:01:43  huec
!  DM-ID 1058 : Initialisation de variable
!  Revision 1.11  2008/06/03 08:02:00  huec
!  FA-ID 1015 : Suppression de double declaration de variable
!  Revision 1.10  2008/05/26 11:55:05  huec
!  AQ : Suppression de code commente
!  Revision 1.9  2008/03/26 13:38:32  huec
!  AQ : Suppression de variables inutilisees
!  Revision 1.8  2008/02/22 13:56:17  huec
!  FA-ID 968 : Suppression de variables declarees en double
!  Revision 1.7  2007/11/26 10:24:54  huec
!  Oubli de declaration d une variable
!  Revision 1.6  2007/11/26 09:11:59  huec
!  FA-ID 776 : Variables locales non inutilisees
!  Revision 1.5  2007/11/05 16:03:56  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.4  2007/10/23 15:02:19  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.3  2005/03/03 09:13:22  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.2  2005/01/21 17:50:34  rostan
!  suppression des commentaires superflus
!  Revision 1.1  2005/01/21 09:57:25  rostan
!  dm-
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:53:31  simbad
!  Ajout du calcul des atmospheres
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_FUNC
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- MSP_calculer_atm_cira
!- MSP_calculer_atm_dtm
!- MSP_calculer_atm_at77
!
!$Fonctions
!- MSP_calculer_alt_terre
!- MSP_calculer_mois
!- MSP_calculer_an
!- MSP_heure_locale
!
!$Include
!
!$Module
!#V
!- MSP_ACTSOL_DEF
!- CPS_ATMOSPHERE
!- MSLIB
!- cps_atm_dtm78_mod
!- cps_atm_cira_msis86_mod
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE TERRESTRE
!
!$Voir-Aussi
!.  MSP_calculer_alt_terre MSP_calculer_mois MSP_calculer_an MSP_heure_locale MSP_calculer_atm_cira
!.  MSP_calculer_atm_dtm MSP_calculer_atm_at77
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSP_ACTSOL_DEF
  use CPS_ATMOSPHERE
  use MSLIB, only : tm_code_retour, tm_jour_sec, PM_REEL, tm_geodesique, &
       mt_car_geod, md_julien_calend, mu_angle2, mr_tsid_veis, &
       PM_DEG_RAD, PM_PI
  use cps_atm_dtm78_mod
  use cps_atm_cira_msis86_mod
  

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_FUNC.F90 365 2013-02-18 12:36:19Z aadt $'

CONTAINS


  function MSP_calculer_alt_terre(R,rlon,phisat) result (alt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_alt_terre
!
!$Resume
!	Calcul de l'altitude geodésique
!
!$Description
!	Calcul de l'altitude geodésique
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  alt = MSP_calculer_alt_terre(R,rlon,phisat)
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: alt
!
!$Arguments
!>E     R       :<PM_REEL>   distance centre plantète-sonde (m)
!>E     rlon    :<PM_REEL>   longitude (rad)
!>E     phisat  :<PM_REEL>   latitude géocentrique (rad)
!>S     alt     :<PM_REEL>   altitude géodésique (m)
!
!$Common
!
!$Routines
!- mt_car_geod
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!     Constantes terrestres utilisées pour le calcul de l'altitude
!      Req_terre = 6378.138e3_PM_REEL
!      alf_terre = 1._PM_REEL/298.257_PM_REEL
!
!$Mots-cles
! ALTITUDE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none

    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    
    real (KIND=PM_REEL) :: alt

    real(kind=PM_REEL),dimension(3) :: posgr
    type (tm_geodesique) :: corgeod
    type (tm_code_retour)::code_erreur
    ! Constantes terrestres utilisées pour le calcul de l'altitude
    real(kind=PM_REEL),parameter :: Req_terre = 6378.138e3_PM_REEL
    real(kind=PM_REEL),parameter :: alf_terre = 1._PM_REEL/298.257_PM_REEL

    alt = 0._PM_REEL

    ! Calcul des coordonnées cartésiennes du satellite
    ! Coordonnées du satellite dans le repère planétographique
    posgr(1) = R*cos(rlon)*cos(phisat)
    posgr(2) = R*sin(rlon)*cos(phisat)
    posgr(3) = R*sin(phisat)

    ! Calcul de l'altitude et de la latitude vraie
    call mt_car_geod (posgr,Req_terre,alf_terre,corgeod,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if ( MSP_ERREUR ) return
    if ( corgeod%haut <= 0._pm_reel ) then
       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_001", Routine="MSP_calculer_alt_terre")
       return
    else
       alt  = corgeod%haut
    endif
  end function MSP_calculer_alt_terre


  subroutine MSP_calculer_atm_cira(date_js,actsol,R,rlon,phisat,ro,vson,pression,temperature)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_cira
!
!$Resume
!	Routine permettant de calculer les caractéristiques de l'atmosphère CIRA-MSIS
!
!$Description
!	Routine permettant de calculer les caractéristiques de l'atmosphère CIRA-MSIS
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_cira(date_js,actsol,R,rlon,phisat,ro,vson,pression,temperature)
!.    type(tm_jour_sec) :: date_js 
!.    type(MSP_ACTSOL) :: actsol 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!.    real (KIND=PM_REEL) :: pression 
!.    real (KIND=PM_REEL) :: temperature 
!
!$Arguments
!>E     date_js      :<tm_jour_sec>   
!>E     actsol       :<MSP_ACTSOL>    structure activité solaire
!>E     R            :<PM_REEL>       Distance centre planete - sonde (m)
!>E     rlon         :<PM_REEL>       Longitude (m)
!>E     phisat       :<PM_REEL>       Latitude (m)
!>S     ro           :<PM_REEL>       Densite atmospherique (kg/m^3)
!>S     vson         :<PM_REEL>       Vitesse du son (m/s)
!>S     pression     :<PM_REEL>       Pression atmospherique (Pa)
!>S     temperature  :<PM_REEL>       Temperature atmospherique (K)
!
!$Common
!
!$Routines
!- MSP_calculer_actsol
!- cps_atm_cira_msis86
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE CIRA MSIS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none

    type(tm_jour_sec),   intent(IN)  :: date_js       ! Date JJ cnes 1950 en tuc
    type(MSP_ACTSOL)   , intent(IN)  :: actsol        ! structure activité solaire
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT) :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT) :: pression      ! Pression atmospherique
    real (KIND=PM_REEL), intent(OUT) :: temperature   ! Temperature atmospherique
 

    !-----------------------------------------------------------------------------------
    ! Variables locales
    !-----------------------------------------------------------------------------------
  
    ! Calcul du flux terrestre
    real (KIND=pm_reel) :: fljop,flmoy
    real(kind=pm_reel),dimension(7) ::ap
!!$    integer::ier
   ! Calcul de l'heure locale solaire
    real(kind=PM_REEL)::ahsos

    real(kind=PM_REEL)::alsat

    real(kind=PM_REEL),parameter::gama=1.4_pm_reel
    real(kind=PM_REEL),parameter::rstar=8.31432e+03_pm_reel

    real(kind=pm_reel)::xmol
    
    type (tm_code_retour) :: code_erreur


    alsat = MSP_calculer_alt_terre(R,rlon,phisat)
    ! Calcul du flux solaire 
    call MSP_calculer_actsol (actsol,date_js,fljop,flmoy,ap)
    if (MSP_gen_messages("MSP_calculer_atm_cira" )) return

    ! Calcul de l'heure locale
    ahsos=MSP_heure_locale(date_js,rlon)
    
    ! Calcul de ro, de vson, de la pression, de la temperature
    
    call cps_atm_cira_msis86 (date_js, fljop, flmoy, ap, phisat, rlon, &
         alsat, ahsos, ro, code_erreur, temp=temperature, pres=pression)
    
    if (code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("MSP_calculer_atm_cira" )) return
    end if
    
    xmol = (ro*rstar*temperature)/pression

    vson = SQRT(gama*rstar*temperature/xmol)

  end subroutine MSP_calculer_atm_cira


  function MSP_calculer_mois(date_tuc_js) result (imois)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_mois
!
!$Resume
!	Routine permettant de calculer le mois de l'année à partir d'un date en jour julien
!
!$Description
!	Routine permettant de calculer le mois de l'année à partir d'un date en jour julien
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  imois = MSP_calculer_mois(date_tuc_js)
!.    type(tm_jour_sec) :: date_tuc_js 
!.    integer :: imois
!
!$Arguments
!>E     date_tuc_js  :<tm_jour_sec>   
!>S     imois        :<integer>       mois
!
!$Common
!
!$Routines
!- md_julien_calend
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! MOIS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none
  
  ! Argument d'entrée
  type(tm_jour_sec), intent(IN)  :: date_tuc_js   ! Date JJ 1950 jours/sec en tuc
  !Sortie
  integer :: imois
  
  ! Variables locales pour la conversion de date
  integer:: ian, ijour, iheure, imin
  real(kind=PM_REEL) ::sec
  type(tm_code_retour) :: code_erreur

  call md_julien_calend (date_tuc_js, ian, imois, ijour, iheure, imin, sec,code_erreur)
  call MSP_signaler_message (ier_mslib=code_erreur)
  if (MSP_gen_messages("MSP_calculer_mois" )) return

end function MSP_calculer_mois

integer function MSP_calculer_an(date_tuc_js) result (ian)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_an
!
!$Resume
!	Routine permettant de calculer l'année à partir d'une date en jour julien CNES
!
!$Description
!	Routine permettant de calculer l'année à partir d'une date en jour julien CNES
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  ian = MSP_calculer_an(date_tuc_js)
!.    type(tm_jour_sec) :: date_tuc_js 
!
!$Arguments
!>E     date_tuc_js  :<tm_jour_sec>   
!>S     ian          :<integer>       année
!
!$Common
!
!$Routines
!- md_julien_calend
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! ANNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    ! Entrée
    type(tm_jour_sec), intent(IN)  :: date_tuc_js       ! Date JJ cnes 1950 en tuc
  
    ! Variables locales pour la conversion de date
    integer:: ijour, iheure, imin,imois
    real(kind=PM_REEL) ::sec
    type(tm_code_retour) :: code_erreur

    call md_julien_calend (date_tuc_js, ian, imois, ijour, iheure, imin, sec,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_calculer_an" )) return

  end function MSP_calculer_an

  subroutine MSP_calculer_atm_dtm(date_js,actsol,R,rlon,phisat,ro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_dtm
!
!$Resume
!	Routine permettant de calculer les caractéristiques de l'atmosphère DTM
!
!$Description
!	Routine permettant de calculer les caractéristiques de l'atmosphère DTM
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_dtm(date_js,actsol,R,rlon,phisat,ro)
!.    type(tm_jour_sec) :: date_js 
!.    type(MSP_ACTSOL) :: actsol 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!
!$Arguments
!>E     date_js  :<tm_jour_sec>   Date JJ cnes 1950 en tuc
!>E     actsol   :<MSP_ACTSOL>    structure activité solaire
!>E     R        :<PM_REEL>       Distance centre planete - sonde (m)
!>E     rlon     :<PM_REEL>       Longitude (m)
!>E     phisat   :<PM_REEL>       Latitude géocentrique (m)
!>S     ro       :<PM_REEL>       Densite atmospherique (kg/m^3)
!
!$Common
!
!$Routines
!- MSP_calculer_actsol
!- MSP_signaler_message
!- md_calend_julien
!- cps_atm_dtm78
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE DTM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(tm_jour_sec),   intent(IN)  :: date_js       ! Date JJ cnes 1950 en tuc
    type(MSP_ACTSOL)   , intent(IN)  :: actsol        ! structure activité solaire
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique


    !-----------------------------------------------------------------------------------
    ! Variables locales
    !-----------------------------------------------------------------------------------

    ! Calcul du flux terrestre
    real (KIND=pm_reel) :: fljop,flmoy
    real(kind=pm_reel),dimension(7) ::ap
    ! Calcul de l'heure locale solaire
    real(kind=PM_REEL)::ahsos

    real(kind=PM_REEL)::alsat

    ! Conversion de date
    integer:: ian
    type(tm_jour_sec) :: jul1950
    type(tm_code_retour) :: code_erreur
    
    alsat = MSP_calculer_alt_terre(R,rlon,phisat)
    if (MSP_gen_messages("MSP_calculer_atm_dtm" )) return

    ! Calcul du flux solaire 
    call MSP_calculer_actsol (actsol,date_js,fljop,flmoy,ap)
    if (MSP_gen_messages("MSP_calculer_atm_dtm" )) return

    ! Calcul de l'heure locale
    ahsos=MSP_heure_locale(date_js,rlon)
    ian=MSP_calculer_an(date_js)
    if (MSP_gen_messages("MSP_calculer_atm_dtm" )) return

    ! suppression de IO_e_trapkp et remplacement de IO_e_dtm (IOLIB) par mp_atm_dtm78 (MSPRO)
    call md_calend_julien (ian, 1, 1, 0, 0, 0._pm_reel, jul1950, code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_calculer_atm_dtm" )) return

    ! Calcul de ro, de vson, de la pression, de la temperature   
    call cps_atm_dtm78(date_js, fljop, flmoy, ap(3), phisat, alsat, ahsos, ro)
    if (MSP_gen_messages("MSP_calculer_atm_dtm" )) return
    
  end subroutine MSP_calculer_atm_dtm


  subroutine MSP_calculer_atm_at77(date_js,actsol,R,rlon,phisat,ro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_at77
!
!$Resume
!	Routine permettant de calculer les caractéristiques de l'atmosphère du modèle russe AT77
!
!$Description
!	Routine permettant de calculer les caractéristiques de l'atmosphère du modèle russe AT77
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_at77(date_js,actsol,R,rlon,phisat,ro)
!.    type(tm_jour_sec) :: date_js 
!.    type(MSP_ACTSOL) :: actsol 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!
!$Arguments
!>E     date_js  :<tm_jour_sec>   
!>E     actsol   :<MSP_ACTSOL>    structure activité solaire
!>E     R        :<PM_REEL>       Distance centre planete - sonde (m)
!>E     rlon     :<PM_REEL>       Longitude (rad)
!>E     phisat   :<PM_REEL>       Latitude géocentrique (rad)
!>S     ro       :<PM_REEL>       Densite atmospherique (kg/m^3)
!
!$Common
!
!$Routines
!- MSP_calculer_actsol
!- md_joursec_jourfrac
!- cps_roat77
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE RUSSE AT77
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(tm_jour_sec),   intent(IN)  :: date_js       ! Date Jour/Secondes 1950 en tuc
    type(MSP_ACTSOL)   , intent(IN)  :: actsol        ! structure activité solaire
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique

     ! Calcul du flux terrestre
    real (KIND=pm_reel) :: fljop,flmoy
    real(kind=pm_reel),dimension(7) ::ap

    real(kind=PM_REEL),dimension(3) :: posgr
    
    real(kind=pm_reel) :: date_jj
    type(tm_code_retour) :: code_erreur
    

    ! Calcul des coordonnées cartésiennes du satellite
    ! Coordonnées du satellite dans le repère planétographique
    posgr(1) = R*cos(rlon)*cos(phisat)
    posgr(2) = R*sin(rlon)*cos(phisat)
    posgr(3) = R*sin(phisat)
    
    ! calcul du flux solaire
    call MSP_calculer_actsol (actsol,date_js,fljop,flmoy,ap)       
    if (MSP_gen_messages("MSP_calculer_atm_at77" )) return

    call md_joursec_jourfrac(date_js,date_jj,code_erreur)

    ! Calcul de ro, de vson, de la pression, de la temperature
    ro = 0._pm_reel
    call cps_roat77 (date_jj,posgr,flmoy,ap(1),ro)

    if (MSP_gen_messages("MSP_calculer_atm_at77" )) return

  end subroutine MSP_calculer_atm_at77


  function MSP_heure_locale(date_tuc_js,lonsat) result (heure)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_heure_locale
!
!$Resume
!  Calcul de l'heure locale
!
!$Description
!  Calcul de l'heure locale
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  heure = MSP_heure_locale(date_tuc_js,lonsat)
!.    type(tm_jour_sec) :: date_tuc_js
!.    real(kind=PM_REEL) :: lonsat
!.    real(kind=PM_REEL) :: heure
!
!$Arguments
!>E     date_tuc_js  :<tm_jour_sec>   jour julien CNES exprimé en TUC
!>E     lonsat       :<PM_REEL>       longitude du satellite  
!>S     heure        :<PM_REEL>       heure locale
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- mu_angle2
!- MSP_signaler_message
!- mr_tsid_veis
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  HEURE LOCALE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    ! Arguments
    type(tm_jour_sec), intent(in)  :: date_tuc_js
    real(kind=PM_REEL),intent(in)  :: lonsat
    
    real(kind=PM_REEL) :: heure


    ! Varaibles locales
    real(kind=PM_REEL),parameter::xobliq=23.45_pm_reel*PM_DEG_RAD
    real(kind=PM_REEL),parameter::exsol=0.01672_pm_reel
    real(kind=PM_REEL)          ::coseps
    real(kind=PM_REEL),parameter::xls1 = 280.0804_pm_reel*PM_DEG_RAD
    real(kind=PM_REEL),parameter::xls2 = 0.9856473354_pm_reel*PM_DEG_RAD
    real(kind=PM_REEL),parameter::ams1 = 357.73050_pm_reel*PM_DEG_RAD
    real(kind=PM_REEL),parameter::ams2 = 0.985600267_pm_reel*PM_DEG_RAD
    real(kind=PM_REEL),parameter::exsol2 = 2._pm_reel*exsol
    real(kind=PM_REEL),parameter::exsolk = 1.25_pm_reel*exsol**2
    real(kind=PM_REEL),parameter::cxlsol = -20.47_pm_reel*PM_DEG_RAD/3600._pm_reel

    real(kind=PM_REEL)::xlsol,amsol,eqcen,colsol,silsol,adroit
    real(kind=pm_reel) :: date_tuc
    type (tm_code_retour)::code_erreur
    real(kind=PM_REEL)::tsid

    ! INITIALISATIONS
    ! ===============

    coseps=cos(xobliq)
    heure=0._pm_reel

    call md_joursec_jourfrac(date_tuc_js,date_tuc,code_erreur)

    ! Calcul de l'angle horaire et de la déclinaison du soleil
    xlsol = xls1 + xls2*date_tuc
    amsol = ams1 + ams2*date_tuc
    eqcen = exsol2*sin(amsol)+exsolk*sin(amsol+amsol)
    xlsol = xlsol + cxlsol + eqcen
    colsol = cos(xlsol)
    silsol = sin(xlsol)
    call mu_angle2 (colsol,silsol*coseps,adroit,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_heure_locale" )) return


    ! Heure locale (en radians):
    ! Formule extriate de PSIMU adaptée à une longitude planètocentrique

    call mr_tsid_veis (date_tuc_js,0._pm_reel,tsid,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_heure_locale" )) return

    heure = lonsat + tsid - adroit + PM_PI

  end function MSP_heure_locale

end module MSP_ATM_FUNC
