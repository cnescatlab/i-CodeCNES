module   CPS_ATMOSPHERE_TERRE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  CPS_ATMOSPHERE_TERRE
!
!$Resume
!  Modèles d'Atmosphère terrestre
!
!$Description
! Ce module contient 4 modèles :
! - le modele terrestre msis90
! - le modele terrestre msis2000
! - le modele terrestre met88
! - le modele terrestre "ROAT77" copié de la MECASPA (MSP_roat77)
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_atmosphere_terre.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!
!  $Log: cps_atmosphere_terre.F90,v $
!  Revision 1.10  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2010/10/14 09:09:52  ogarat
!  VERSION::FA-ID:1424:14/10/2010:Appel du modele d'atmosphere MSIS00 par cps_calculer_msis2000
!
!  Revision 1.8  2009/05/13 12:44:54  cml
!  FA-ID 1266 : Ajout d une conversion radians->heures manquante pour l heure locale
!
!  Revision 1.7  2008/12/23 15:35:23  cml
!  FA-ID 1162 : Correction de la transformation radian->degres pour les modeles MSIS90 et MSIS2000
!
!  Revision 1.6  2008/10/14 07:53:35  cml
!  DM-ID 1058 : Ajout d init et de conversions explicites
!
!  Revision 1.5  2008/07/04 11:58:55  huec
!  DM-ID 1058 : Precision numerique : transformations de parametres en pm_reel
!
!  Revision 1.4  2008/04/07 09:13:55  vivaresf
!  FA-ID 1009 :
!  - rajout de constantes pour chaque modèle
!  - suppression de cps_acces (obsolète)
!  - correction des cartouches
!  - rajout des modèles associés à chaque corps
!
!  Revision 1.3  2008/03/18 16:46:21  vivaresf
!  version 2.4 : rajout des Log pour l'historique
!
!
!$FinHistorique
!
!$Usage
!  use CPS_ATMOSPHERE_TERRE
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE MARS90
!
!$Voir-Aussi
!.  cps_calculer_msis90 cps_calculer_msis2000 cps_calculer_met88 cps_roat77
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules sans interface adaptée
  use cps_atm_cira_mod
  use cps_atm_cira_msis86_mod
  use cps_atm_msis86_mod
  use cps_atm_dtm78_mod
  use cps_atm_us76d_mod

  use cps_utilisateur
  

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atmosphere_terre.F90 69 2012-09-11 08:33:34Z ffsm $'


  ! Nom des modèles terrestres (idem noms enregistrés dans la base)

  character(len=20), parameter :: cps_modatm_cira="CIRA"
  character(len=20), parameter :: cps_modatm_cira_msis86="CIRA_MSIS86"
  character(len=20), parameter :: cps_modatm_msis86="MSIS86"
  character(len=20), parameter :: cps_modatm_dtm78="DTM78"
  character(len=20), parameter :: cps_modatm_us76d="US76D"
  character(len=20), parameter :: cps_modatm_roat77="ROAT77"
  character(len=20), parameter :: cps_modatm_msis90="MSIS90"
  character(len=20), parameter :: cps_modatm_msis2000="MSIS2000"
  character(len=20), parameter :: cps_modatm_met88="MET88"
  
  ! Constante de conversion des radians en heures
  real(kind=pm_reel), parameter :: cpsi_rad_to_hrs = 24.0_pm_reel / pm_deux_pi

   contains

      subroutine cps_calculer_msis90(date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol,&
           dens,temp)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_calculer_msis90
!
!$Resume
!  Routine chapeau d'appel au modèle d'atmosphère terrestre MSIS90
!
!$Description
!  Routine chapeau d'appel au modèle d'atmosphère terrestre MSIS90.
!  Cette routine facilite l'appel du modèle dans PSIMU, en convertissant 
!  les données d'entrée dans les bons formats     
!
!$Auteur
!  Y. TANGUY (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_calculer_msis90(date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol,&
!.               dens,[temp])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_3rot 
!.    real(pm_reel), dimension(7) :: tab_ap 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: heure_sol 
!.    real(pm_reel) :: dens 
!.    real(pm_reel) :: temp 
!
!$Arguments
!>E     date         :<tm_jour_sec>       date julienne 1950 (JJ)       
!>E     flux_veille  :<pm_reel>           flux solaire du jour précédent
!>E     flux_3rot    :<pm_reel>           flux solaire moyen sur les 3  
!                                         dernieres rotations solaires  
!>E     tab_ap       :<pm_reel,DIM=(7)>   évolution de l'activité       
!                                         géomagnetique (60 h précédente)
!>E     lat          :<pm_reel>           latitude géodésique (rad) 
!>E     long         :<pm_reel>           longitude géodésique (rad)
!>E     alt          :<pm_reel>           altitude geodesique (m)           
!>E     heure_sol    :<pm_reel>           heure solaire locale (rad)
!>S     dens         :<pm_reel>           densité atmosphérique (kg.m-3)
!>[S]   temp         :<pm_reel>           température à l'altitude Alt (degré Kelvin)    
!
!$Routines
!- md_julien_calend
!- MSP_Signaler_Message
!- md_calend_anjour
!- CPS_GTD6
!
!$Include
!
!$Module
!#V
!- cps_atm_msis90
!- msp_gestion_erreur
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
        use cps_atm_msis90
        use msp_gestion_erreur
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
        real(pm_reel), intent(in)               :: lat          ! latitude geodesique (rad)
        real(pm_reel), intent(in)               :: long         ! longitude geodesique(rad)
        real(pm_reel), intent(in)               :: alt          ! altitude geodesique (m)
        real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale(rad)
        real(pm_reel), intent(out)              :: dens         ! densite atmospherique (kg.m-3)
        real(pm_reel), intent(out), optional    :: temp         ! temperature           (kelvin)

        ! Variables locales
        ! =================
        real(kind=pm_reel) :: sec
        integer :: an, mois, jour, heure, min, jour_an
        integer :: date_YYDDD
        real(kind=pm_reel) :: glat, glon, galt
        integer :: code_masse
        real(kind=pm_reel), dimension(8) :: tab_densite         
        real(kind=pm_reel), dimension(2) :: tab_temp
        integer :: ii                                           ! Indice de boucle
        real(KIND=pm_reel) :: heure_sol_hrs  ! Heure solaire en heures

        type(tm_code_retour) :: code_retour


        ! Début du code
        ! =============

        ! Initialisation des tableaux
        do ii = 1,2
           tab_temp(ii) = 0._pm_reel
        enddo
        do ii = 1,8
           tab_densite(ii) = 0._pm_reel
        enddo

        ! Cette routine convertit les entrées pour l'appel au modèle MSIS90 (CPS_GTD6)


        ! Conversion de la date jj1950 au format DDYYY et Sec
        ! ===================================================
        call md_julien_calend(date, an, mois, jour, heure, min, sec, code_retour)
        if(code_retour%valeur < 0) then
           call MSP_Signaler_Message(cle_mes="CPS_ERR_MSIS90",partie_variable="Conversion date jj1950 -> date calendaire")
        end if
        
        ! Calcul du nombre de jours dans l'année
        call md_calend_anjour(an, mois, jour, jour_an, code_retour)
        if(code_retour%valeur < 0) then
           call MSP_Signaler_Message(cle_mes="CPS_ERR_MSIS90",partie_variable="Conversion date calendaire -> jours dans l'année")
        end if

        ! Calcul nombre de secondes dans le jour
        sec = sec + heure*3600 + min*60

        ! Ex : pour le 25 mai 1981 
        ! 25+152 = 177 --> date YYDDD = 81177
        date_YYDDD = mod(an,100)*1000+jour_an

        ! Conversion des autres paramètres d'entrée
        ! =========================================
        glat = lat*pm_rad_deg  ! conversion rad -> deg
        glon = long*pm_rad_deg 
        galt = alt / 1000._pm_reel         ! conversion m -> km
        ! Conversion de l'heure locale de radians en heure 
        heure_sol_hrs = heure_sol*cpsi_rad_to_hrs
        
        ! On effectue l'appel avec la variable MASS à 48. Ceci permet de calculer
        ! la densité moyenne qui nous intéresse.
        code_masse = 48

        ! Appel à la routine CPS_GTD6
        ! ===========================
        call CPS_GTD6(date_YYDDD,sec,galt,glat,glon,heure_sol_hrs,flux_3rot,flux_veille,tab_ap,code_masse,tab_densite,tab_temp)


        ! Extraction et conversion des sorties
        ! ====================================
        ! Densité de masse totale (GM/CM3) : tab_densite(6)
        ! conversion en kg/m3 : x 1000 
        dens = tab_densite(6)*(1000._pm_reel) 
        if(present(temp)) then
           temp = tab_temp(2)            ! Temperature à l'altitude alt
        end if

      end subroutine cps_calculer_msis90

      subroutine cps_calculer_msis2000(date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol,&
           dens,temp)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_calculer_msis2000
!
!$Resume
!  Routine chapeau d'appel au modèle d'atmosphère terrestre NRL-MSIS2000
!
!$Description
!  Routine chapeau d'appel au modèle d'atmosphère terrestre MSIS2000.
!  Cette routine facilite l'appel du modèle dans PSIMU, en convertissant 
!  les données d'entrée dans les bons formats     
!
!$Auteur
!  Y. TANGUY (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_calculer_msis2000(date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol,&
!.               dens,[temp])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_3rot 
!.    real(pm_reel), dimension(7) :: tab_ap 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: heure_sol 
!.    real(pm_reel) :: dens 
!.    real(pm_reel) :: temp 
!
!$Arguments
!>E     date         :<tm_jour_sec>       date julienne 1950 (JJ)       
!>E     flux_veille  :<pm_reel>           flux solaire du jour précédent
!>E     flux_3rot    :<pm_reel>           flux solaire moyen sur les 3  
!                                         dernieres rotations solaires  
!>E     tab_ap       :<pm_reel,DIM=(7)>   évolution de l'activité       
!                                         géomagnetique (60 h précédente)
!>E     lat          :<pm_reel>           latitude géodésique (rad) 
!>E     long         :<pm_reel>           longitude géodésique (rad)
!>E     alt          :<pm_reel>           altitude geodesique (m)           
!>E     heure_sol    :<pm_reel>           heure solaire locale (rad)
!>S     dens         :<pm_reel>           densité atmosphérique (kg.m-3)
!>[S]   temp         :<pm_reel>           température à l'altitude Alt (degré Kelvin)    
!
!$Routines
!- md_julien_calend
!- MSP_Signaler_Message
!- md_calend_anjour
!- CPS_GTD7
!
!$Include
!
!$Module
!#V
!- cps_atm_msis2000
!- msp_gestion_erreur
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
        use cps_atm_msis2000
        use msp_gestion_erreur
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
        real(pm_reel), intent(in)               :: lat          ! latitude geodesique (rad)
        real(pm_reel), intent(in)               :: long         ! longitude geodesique(rad)
        real(pm_reel), intent(in)               :: alt          ! altitude geodesique (m)
        real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale(rad)
        real(pm_reel), intent(out)              :: dens         ! densite atmospherique (kg.m-3)
        real(pm_reel), intent(out), optional    :: temp         ! temperature           (kelvin)

        ! Variables locales
        ! =================
        real(kind=pm_reel) :: sec
        integer :: an, mois, jour, heure, min, jour_an
        integer :: date_YYDDD
        real(kind=pm_reel) :: glat, glon, galt
        integer :: code_masse
        real(kind=pm_reel), dimension(9) :: tab_densite         
        real(kind=pm_reel), dimension(2) :: tab_temp
        integer :: ii  ! indice de boucle
        real(KIND=pm_reel) :: heure_sol_hrs  ! Heure solaire en heures
        real(kind=pm_reel),dimension(25) :: tab_sw ! tableau de clés de contrôle

        type(tm_code_retour) :: code_retour


        ! Début du code
        ! =============

        ! Init des tableaux
        do ii = 1,2
           tab_temp(ii) = 0._pm_reel
        enddo
        do ii = 1,9
           tab_densite(ii) = 0._pm_reel
        enddo

        ! Cette routine convertit les entrées pour l'appel au modèle MSIS2000 (CPS_GTD6)

        ! Conversion de la date jj1950 au format DDYYY et Sec
        ! ===================================================
        call md_julien_calend(date, an, mois, jour, heure, min, sec, code_retour)
        if(code_retour%valeur < 0) then
           call MSP_Signaler_Message(cle_mes="CPS_ERR_MSIS2000",partie_variable="Conversion date jj1950 -> date calendaire")
        end if
        
        ! Calcul du nombre de jours dans l'année
        call md_calend_anjour(an, mois, jour, jour_an, code_retour)
        if(code_retour%valeur < 0) then
           call MSP_Signaler_Message(cle_mes="CPS_ERR_MSIS2000",partie_variable="Conversion date calendaire -> jours dans l'année")
        end if

        ! Calcul nombre de secondes dans le jour
        sec = sec + heure*3600 + min*60

        ! Ex : pour le 25 mai 1981 
        ! 25+152 = 177 --> date YYDDD = 81177
        date_YYDDD = mod(an,100)*1000+jour_an

        ! Conversion des autres paramètres d'entrée
        ! =========================================
        glat = lat*pm_rad_deg  ! conversion rad -> deg
        glon = long*pm_rad_deg 
        galt = alt / 1000._pm_reel         ! conversion m -> km
        ! Conversion de l'heure locale de radians en heure compris entre 0 et 24h
        heure_sol_hrs = modulo(heure_sol*cpsi_rad_to_hrs,24.0_pm_reel) 
        
        ! On effectue l'appel avec la variable MASS à 48. Ceci permet de calculer
        ! la densité moyenne qui nous intéresse.
        code_masse = 48

        ! Appel à cps_tselec2000 pour que le vecteur de dimension 7 des AP soit pris en compte
        tab_SW = 1.
        tab_SW(9) = -1.
        call cps_TSELEC2000(tab_SW)

        ! Appel à la routine CPS_GTD7
        ! ===========================
        call CPS_GTD7D(date_YYDDD,sec,galt,glat,glon,heure_sol_hrs,flux_3rot,flux_veille,tab_ap,code_masse,tab_densite,tab_temp)


        ! Extraction et conversion des sorties
        ! ====================================
        ! Densité de masse totale (GM/CM3) : tab_densite(6)
        ! conversion en kg/m3 : x 1000 
        dens = tab_densite(6)*(1000._pm_reel) 
        if(present(temp)) then
           temp = tab_temp(2)            ! Temperature à l'altitude alt
        end if

      end subroutine cps_calculer_msis2000

      subroutine cps_calculer_met88(date,alt,lat,long,flux_veille,flux_moyen,&
           tab_ap,densite,temperature)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_calculer_met88
!
!$Resume
!  Routine chapeau d'appel au modèle d'atmosphère MET 88
!
!$Description
!  Routine chapeau d'appel au modèle d'atmosphère MET 88 : utilisable par
!  PSIMU (entre autres)
!
!$Auteur
!  Y. TANGUY (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_calculer_met88(date,alt,lat,long,flux_veille,flux_moyen,&
!.               tab_ap,densite,[temperature])
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel) :: alt 
!.    real(pm_reel) :: lat 
!.    real(pm_reel) :: long 
!.    real(pm_reel) :: flux_veille 
!.    real(pm_reel) :: flux_moyen 
!.    real(pm_reel), dimension(7) :: tab_ap 
!.    real(pm_reel) :: densite 
!.    real(pm_reel) :: temperature 
!
!$Arguments
!>E     date         :<tm_jour_sec>       Date jj1950 
!>E     alt          :<pm_reel>           Altitude en m
!>E     lat          :<pm_reel>           Latitude en rad
!>E     long         :<pm_reel>           Longitude en rad
!>E     flux_veille  :<pm_reel>           Flux de la veille
!>E     flux_moyen   :<pm_reel>           Flux moyen sur les dernières rotations du soleil
!>E     tab_ap       :<pm_reel,DIM=(7)>   Tableau des indices géomagnétiques (cf modèles COMPAS, et psacsol de PSIMU)
!>S     densite      :<pm_reel>           Densité en kg/m3
!>[S]   temperature  :<pm_reel>           Température en degrés Kelvin
!
!$Common
!
!$Routines
!- md_julien_calend
!- MSP_Signaler_Message
!- cps_atm_met88
!
!$Include
!
!$Module
!#V
!- cps_atm_met88_mod
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

        use cps_atm_met88_mod
        
        implicit none
           
        ! Declarations
        ! ============
        type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
        real(pm_reel), intent(in)               :: alt          ! altitude geodesique (m)
        real(pm_reel), intent(in)               :: lat          ! latitude geodesique (rad)
        real(pm_reel), intent(in)               :: long         ! longitude geodesique(rad)
        real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
        real(pm_reel), intent(in)               :: flux_moyen    ! flux solaire moyen sur les 3 
        ! dernieres rotations solaires
        real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
        ! geomagnetique (60 h precedentes)

        real(pm_reel), intent(out)              :: densite      ! densite atmospherique (kg.m-3)
        real(pm_reel), intent(out), optional    :: temperature  ! temperature           (kelvin)

        ! Variables locales
        ! =================
        integer :: an, mois, jour, heure, min
        real(kind=pm_reel) :: sec
        type(tm_code_retour):: code_retour
        
        ! Variables sur 4 octets pour l'appel au modèle MET-88
        real(kind=4), dimension(12) :: outdata
        real(kind=4),dimension(5)   :: auxdata
        real(kind=4) :: x_alt, x_lat, x_lng, x_gi, x_flux_veille, x_flux_moyen
        integer :: x_igeo_ind

        ! Début du code
        ! =============

        ! 1) Conversion de la date jj/sec en jour calendaire
        call md_julien_calend(date,an,mois,jour,heure,min,sec,code_retour)
        if (code_retour%valeur < 0) then
           call MSP_Signaler_Message(cle_mes="CPS_ERR_MET88",&
                partie_variable="Conversion date jj1950 -> calendaire")
        end if
           
        ! 2) Affectation des variables d'entrée, sur 4 octets
        ! Position du véhicule (altitude, latitude et longitude géodésique)
        ! /!\ Le modèle prend une altitude en km en entrée.
        x_alt = real( alt/1000.0_pm_reel)
        x_lat = real( lat)
        x_lng = real( long)
        ! Flux solaires 
        x_flux_veille = real( flux_veille)
        x_flux_moyen = real( flux_moyen)
        ! Indice géomagnétique. x_igeo_ind=2 signifie l'utilisation des Ap
        ! tab_ap(1) stocke l'indice géomagnétique quotidien
        x_igeo_ind = 2 
        x_gi = real( tab_ap(1))

        call cps_atm_met88 (x_alt, x_lat, x_lng, an, mois, jour, heure, min, &
             x_igeo_ind, x_flux_veille, x_flux_moyen, x_gi, outdata, auxdata)
        if(MSP_gen_messages("cps_calculer_met88")) then
           return
        end if

 
        ! 3) Extraction des sorties, et affectation dans les variables dédiées
        densite = outdata(10)
        temperature = outdata(2)

      end subroutine cps_calculer_met88

    subroutine cps_roat77 (date,position,flux,ap,ROAT77)

!******************************************************************************!
!$<AM-V2.0>
!
!$Nom
!  cps_roat77
!
!$Resume
!     Modèle d'atmosphère russe du KIAM.
!
!$Description
!     Sous-programme calculant les caractéristiques atmosphériques 
!     à une altitude donnée d'après le modèle russe du KIAM 
!     utilisé par les logiciels FLIGHT et SHUT.
!     Ce modèle est valide actuellement pour des altitudes entre 180km et 600km.
!
!$Auteur
!    J.F. GOESTER  
!
!$Acces
!  PUBLIC
!
!$Version
!    19/11/98 
!
!$Usage
!  call cps_roat77 (date,position,flux,ap,ROAT77)
!.    real(kind=pm_reel) :: date,flux,ap,ROAT77
!.    real(kind=pm_reel), dimension(3) :: position
!
!$Remarques
!    Routine importée depuis IOLIB (IO_e_roat77) dans l'attente de la disponibilité
!      de mp_atm_roat77 dans la MSPRO.
!   Le modèle ne traite pas les altitudes supérieures a 600km.
!   Attention, une correction a été apportée au code d'origine avec
!   comme commentaire "change for FLIGHT". Cette correction consiste
!   à n'effectuer les calculs prévus que pour les altitudes supérieures
!   à 180 km. Pour celles inférieures, l'altitude est ramenée à 180 km.
!   Le code concernant les altitudes inférieures a 120km ou 180km
!   n'est jamais exécuté.
!
!$Arguments
!>E/S   date      :<pm_reel>           date en Jours Juliens CNES
!>E/S   position  :<pm_reel,DIM=(3)>   position dans Greenwich (km)
!>E/S   flux      :<pm_reel>           flux solaire
!>E/S   ap        :<pm_reel>           indice géomagnétique
!>E/S   ROAT77    :<pm_reel>           densité atmosphérique (kg/m**3)
!
!$<>
!******************************************************************************
      IMPLICIT real(kind=pm_reel) (A-H,O-Z)

      real(kind=pm_reel)    ::  date,flux,ap,ROAT77
      real(kind=pm_reel), dimension(3) ::  position

      real(kind=pm_reel), dimension(3)    ::  X
      real(kind=pm_reel), dimension(4)    ::  SAEM
      real(kind=pm_reel), dimension(6,42) ::  TABL
      real(kind=pm_reel), dimension(38)   :: AD
      real(kind=pm_reel), dimension(3,12) :: HM
      real(kind=pm_reel), dimension(6)    :: F0T
      real(kind=pm_reel), dimension(126)  :: T1,T2

      real(kind=pm_reel) :: ROH, RAB, QK4, H, QK1, D, XS, HH, QK2
      real(kind=pm_reel) :: DC, DH, GAMMA, QK3, SZ, ZS, TM, A0,A
      real(kind=pm_reel) :: R, RM, ALFA, OMEGA, G0R, YS, CT,T, RV, F0,CMF2

      ! compteurs
      integer :: i, j, L, LD
!      DIMENSION X(3),SAEM(4),TABL(6,42),AD(38), &
!                  HM(3,12),F0T(6),T1(126),T2(126)

      EQUIVALENCE (ROH,RAB),(QK4,H),(L,LD),   &
                (QK1,D,XS,HH),(QK2,DC,YS,DH),  &
                (CT,GAMMA),(QK3,SZ,ZS,TM),  &
                (TABL(1,1),T1(1)),(TABL(1,22),T2(1))

      real(kind=pm_reel),dimension(42) ::  T11,T12,T13,T21,T22,T23
!      DIMENSION T11(42),T12(42),T13(42)
!      DIMENSION T21(42),T22(42),T23(42)

      EQUIVALENCE  (T1(1),T11(1)),(T1(43),T12(1))
      EQUIVALENCE  (T1(85),T13(1))

      EQUIVALENCE  (T2(1),T21(1)),(T2(43),T22(1))
      EQUIVALENCE  (T2(85),T23(1))

      DATA A0,R,RM,ALFA,OMEGA,G0R/9.80665_pm_reel, &
           6378.14_pm_reel,6356.766_pm_reel,0.00335289187_pm_reel, &
           6.300388008_pm_reel,34.16321878_pm_reel/,AD/-.067_pm_reel,-.088_pm_reel,&
           -.094_pm_reel,-.088_pm_reel,-.053_pm_reel,-.005_pm_reel,.039_pm_reel,.090_pm_reel,.123_pm_reel,   &
           .133_pm_reel,.123_pm_reel,.099_pm_reel,.059_pm_reel,.017_pm_reel,-.027_pm_reel,-.065_pm_reel,     &
           -.103_pm_reel,-.136_pm_reel,-.156_pm_reel,-.172_pm_reel,-.180_pm_reel,-.183_pm_reel,-.179_pm_reel,&
           -.163_pm_reel,-.133_pm_reel,-.085_pm_reel,-.018_pm_reel,.059_pm_reel,.123_pm_reel,.161_pm_reel,   &
           .170_pm_reel,.156_pm_reel,.119_pm_reel,.073_pm_reel,.027_pm_reel,-.023_pm_reel,-.055_pm_reel,     &
           -.078_pm_reel/,F0T/75._pm_reel,100._pm_reel,125._pm_reel,150._pm_reel,200._pm_reel,250._pm_reel/, &
           HM/0._pm_reel,288.15_pm_reel,1.225_pm_reel, 11._pm_reel,216.65_pm_reel,3.639E-1_pm_reel,          &
           20._pm_reel,216.65_pm_reel,8.803E-2_pm_reel, 32._pm_reel,228.65_pm_reel,1.322E-2_pm_reel,         &
           47._pm_reel,270.65_pm_reel,1.428E-3_pm_reel, 51._pm_reel,270.65_pm_reel,8.616E-4_pm_reel,         &
           71._pm_reel,214.65_pm_reel,6.421E-5_pm_reel, 80._pm_reel,196.65_pm_reel,1.570E-5_pm_reel,         & 
           85._pm_reel,186.65_pm_reel,6.783E-6_pm_reel, 94._pm_reel,186.65_pm_reel,1.307E-6_pm_reel,         &
           102.45_pm_reel                                                &
           ,212.0_pm_reel,2.693E-7_pm_reel,117.777_pm_reel,380.6_pm_reel,2.440E-8_pm_reel/

!  TAâìéãù äìñ KOüææéãéEHTOB B çOCT 22721-77
!  ðPé PAúìéþHùX YPOBHñX COìHEþHOê AKTéBHOCTé
!   TAâìéãA 1
      ! Tableau T11
      DATA T11/-18.873_pm_reel                                                                                         &
           , -19.308_pm_reel, -19.532_pm_reel, -19.592_pm_reel, -19.614_pm_reel, -19.682_pm_reel,                      &
           0.6660_pm_reel                                                                                              &  
           ,   0.5960_pm_reel,   0.5519_pm_reel,   0.5296_pm_reel,   0.5032_pm_reel,   0.4796_pm_reel,                 &
           118.013_pm_reel                                                                                             &
           , 119.285_pm_reel, 119.744_pm_reel, 119.828_pm_reel, 119.846_pm_reel, 119.927_pm_reel,                      &
           -0.3644_pm_reel                                                                                             & 
           ,  -0.3525_pm_reel,  -0.3406_pm_reel,  -0.3288_pm_reel,  -0.2931_pm_reel,  -0.2016_pm_reel,                 &
           .2618E-2_pm_reel, .2508E-2_pm_reel, .2398E-2_pm_reel, .2289E-2_pm_reel, .1961E-2_pm_reel, .9112E-3_pm_reel, &
           .3490E-5_pm_reel, .3579E-5_pm_reel, .3667E-5_pm_reel, .3752E-5_pm_reel, .4012E-5_pm_reel, .6411E-5_pm_reel, &
           -1.0445_pm_reel                                                                                             &
           ,  -0.8181_pm_reel,  -0.6404_pm_reel,  -0.4438_pm_reel,  -0.4581_pm_reel,  -0.2977_pm_reel/    

      ! Tableau T12
      data T12/.9532E-2_pm_reel, .7230E-2_pm_reel, .5594E-2_pm_reel, .3836E-2_pm_reel, .4157E-2_pm_reel, &
           .2401E-2_pm_reel,         -6.4688_pm_reel                                                     &
           ,  -6.8255_pm_reel,  -4.2892_pm_reel,  -1.4294_pm_reel,  -2.6263_pm_reel,   0.5736_pm_reel,   &
           -507.95_pm_reel                                                                               &
           ,-566.11_pm_reel,-632.63_pm_reel,-707.58_pm_reel,-712.00_pm_reel,-727.00_pm_reel,             &
           189.85_pm_reel                                                                                &          
           , 200.97_pm_reel, 230.76_pm_reel, 278.35_pm_reel, 290.00_pm_reel, 300.00_pm_reel,             &
           4.2_pm_reel                                                                                  & 
           ,   4.1_pm_reel,   4.4_pm_reel,   4.7_pm_reel,   4.5_pm_reel,   4.5_pm_reel,             &
           0.653_pm_reel                                                                                 &
           ,   0.621_pm_reel,   0.635_pm_reel,   0.632_pm_reel,   0.611_pm_reel,   0.611_pm_reel,        &
           6*-2.6122_pm_reel/                                             

      ! Tableau T13
      data T13 /6*0.2935E-1_pm_reel,       6*-0.6318E-4_pm_reel,                           &
           -0.4422_pm_reel ,  -0.4109_pm_reel,  -0.3814_pm_reel,  -0.3490_pm_reel,  &
           -0.2882_pm_reel,  -0.2255_pm_reel,   &
           .4809E-2_pm_reel, .4430E-2_pm_reel, .4074E-2_pm_reel, .3682E-2_pm_reel, &
           .2946E-2_pm_reel, .2188E-2_pm_reel,     &
           -.9367E-5_pm_reel,-.8384E-5_pm_reel,-.7461E-5_pm_reel, &
           -.6444E-5_pm_reel,-.4538E-5_pm_reel,-.2570E-5_pm_reel,    &
           0.80_pm_reel                                                         &
           , 0.89_pm_reel,  4*1.00_pm_reel,       2._pm_reel, 2._pm_reel, 3._pm_reel, 4._pm_reel, 5._pm_reel, 5._pm_reel/

!   TAâìéãA 2
      ! Tableau 21
      DATA T21/-14.608_pm_reel                                                  &
           , -14.469_pm_reel, -15.415_pm_reel, -16.559_pm_reel, -18.219_pm_reel, -19.068_pm_reel,   &
           0.8969_pm_reel                                                       &
           ,   0.8517_pm_reel,   0.7729_pm_reel,   0.6982_pm_reel,   0.5863_pm_reel,   0.5177_pm_reel,  &
           67.596_pm_reel                                                      &
           ,  56.026_pm_reel,  61.836_pm_reel,  75.401_pm_reel,  98.336_pm_reel, 109.999_pm_reel,  &
           -0.4016_pm_reel                                                      &
           ,  -0.3957_pm_reel,  -0.3898_pm_reel,  -0.3839_pm_reel,  -0.3472_pm_reel,  -0.3271_pm_reel,  &
           .3031E-2_pm_reel, .2988E-2_pm_reel, .2945E-2_pm_reel, .2902E-2_pm_reel, .2562E-2_pm_reel, .2305E-3_pm_reel,    &
           .2344E-5_pm_reel, .2246E-5_pm_reel, .2148E-5_pm_reel, .2051E-5_pm_reel, .2344E-5_pm_reel, .2539E-5_pm_reel,    &
           0.130_pm_reel                                                       &
           ,  -0.172_pm_reel,  -0.274_pm_reel,  -0.247_pm_reel,  -0.201_pm_reel,  -0.194_pm_reel/

     ! Tableau 22
       data T22/.140E-3_pm_reel, .217E-2_pm_reel, .257E-2_pm_reel, .199E-2_pm_reel, .161E-2_pm_reel,  &
            .134E-2_pm_reel,                                                       &
            3.733_pm_reel,                                                       &
            3.784_pm_reel,   4.048_pm_reel,   3.495_pm_reel,   3.200_pm_reel,   3.000_pm_reel,            &
            -507.95_pm_reel                                                     &
            ,-566.11_pm_reel,-632.63_pm_reel,-707.58_pm_reel,-712.00_pm_reel,-727.00_pm_reel,   &
            189.85_pm_reel                                                      &
            , 200.97_pm_reel, 230.76_pm_reel, 278.35_pm_reel, 290.00_pm_reel, 300.00_pm_reel,   &
            4.2_pm_reel                                                        &  
            ,   4.1_pm_reel,   4.4_pm_reel,   4.7_pm_reel,   4.5_pm_reel,   4.5_pm_reel,   &
            0.653_pm_reel                                                        &
            ,   0.621_pm_reel,   0.635_pm_reel,   0.632_pm_reel,   0.611_pm_reel,   0.611_pm_reel,   &
            6*-0.7379_pm_reel/

     ! Tableau 23
      data T23/ 6*0.8524E-2_pm_reel,       6*-0.5328E-5_pm_reel,                         &
           -0.1767_pm_reel                                                     &
           ,  -0.1785_pm_reel,  -0.1802_pm_reel,  -0.1820_pm_reel,  -0.1855_pm_reel,  -0.1891_pm_reel, &
           .1859E-2_pm_reel, .1848E-2_pm_reel, .1838E-2_pm_reel, .1826E-2_pm_reel, .1805E-2_pm_reel, .1783E-2_pm_reel,   &
           -.1172E-5_pm_reel,-.1211E-5_pm_reel,-.1250E-5_pm_reel,-.1289E-5_pm_reel,-.1367E-5_pm_reel,-.1445E-5_pm_reel,  &
           0.80_pm_reel                                                       &
           , 0.89_pm_reel, 4*1.00_pm_reel, 2._pm_reel, 2._pm_reel, 3._pm_reel, 4._pm_reel, 5._pm_reel, 5._pm_reel/

!   * Interface entre les données d'entrée et les données russes:
!
!     Date en Jours Juliens non CNES:
      T = date + 2433282.5d0 
!     Position en kilomètres:
      do i = 1 , 3 
         X(i) = position(i)/1.d+06 
      enddo 
!     Activité solaire:
      SAEM(1) = flux 
      SAEM(4) = ap 

      RV=SQRT(X(1)*X(1)+X(2)*X(2)+X(3)*X(3)) 
!   BùþéCìEHéE BùCOTù HAä OâýEúEMHùM üììéðCOéäOM - ( H )
      RAB=X(3)/RV 
      H=RV*1000._pm_reel-R*(1._pm_reel-ALFA*RAB*RAB) 
! change for FLIGHT
         if( h.le.180.d0 ) h=180.d0 
!   BùþéCìEHéE äOìé CTOìETéñ - ( CT )
      CT=(T-2415020._pm_reel)/36525._pm_reel 
!   BùþéCìEHéE úBEúäHOçO BPEMEHé  (B çPéHBéþCKYà ðOìHOþø) - ( SZ )
      SZ=628.33195099_pm_reel*CT+1.73993589_pm_reel 
!   BùþéCìEHéE þéCìA äHEê OT HAþAìA çOäA - ( D )
      D=(T-2433282.375_pm_reel)/365.25_pm_reel 
      LD=int(D)
      D=(D-LD)*365.25_pm_reel 
!   BùþéCìEHéE äOìé CYTOK - ( DC )
      LD=int(T)
      DC=T-LD-0.5_pm_reel 
!   BùþéCìEHéE  A(ä)
      LD=int(0.1_pm_reel*D+1._pm_reel) 
      A=AD(LD)*(LD-0.1_pm_reel*D)+AD(LD+1)*(0.1_pm_reel*D+1._pm_reel-LD) 
!   BùâOP úOHù BùCOT ATMOCæEPù
      IF(H.LT.120._pm_reel) GOTO 120 
      IF(H.LT.180._pm_reel) GOTO 180 
      IF(H.GT.600._pm_reel) GOTO 600 
      L=22 
      GOTO 111 
180   L=1 
111   CONTINUE 
!    BùâOP CPEäHEçO YPOBHñ COìHEþHOê AKTéBHOCTé ( F0 )
!    é COOTBETCTBYàýEçO EMY CTOìâãA B TAâìéãAX "TABL" ( J )
      DO 11 J=1,5 
      RAB=(F0T(J)+F0T(J+1))*0.5_pm_reel 
      IF (SAEM(1).LE.RAB) GOTO 22 
  11  CONTINUE 
  22  F0=F0T(J) 
!   BùþéCìEHéE çAMMA*
      GAMMA=SZ+OMEGA*DC-TABL(J,L+12) 
!   BùþéCìEHéE  XS,YS,ZS
      XS =-DCOS(SZ) 
      RAB= -DSIN(SZ) 
      YS =0.91747_pm_reel*RAB 
      ZS =0.397805_pm_reel*RAB 
!   BùþéCìEHéE  (DCOS(æé/2))**M
      RAB=DCOS(GAMMA) 
      CMF2=DSIN(GAMMA) 
      CMF2=((XS*RAB+YS*CMF2)*X(1)+(YS*RAB-XS*CMF2)*X(2)+ZS*X(3))/RV 
      CMF2=((CMF2+1._pm_reel)*0.5_pm_reel)**(TABL(J,L+11)*0.5_pm_reel) 
!   BùþéCìEHéE ROAT77 -- ( äìñ H>120 KM )
      RAB=(TABL(J,L+9)+H)/TABL(J,L+10) 
      QK1=(DEXP(-RAB*RAB)*TABL(J,L+8)+TABL(J,L+7)*H+TABL(J,L+6))*CMF2 &
           + 1._pm_reel 
      ROH=A0*DEXP(TABL(J,L)-TABL(J,L+1)*SQRT(H-TABL(J,L+2))) 
      QK2=((TABL(J,L+15)*H+TABL(J,L+14))*H+TABL(J,L+13))*A+1._pm_reel 
      QK3=((TABL(J,L+5)*H+TABL(J,L+4))*H+TABL(J,L+3))*(SAEM(1)-F0)/F0  &
           + 1._pm_reel
      QK4=((TABL(J,L+18)*H+TABL(J,L+17))*H+TABL(J,L+16)) &
           *DLOG(SAEM(4)/TABL(J,L+20)+TABL(J,L+19))+1._pm_reel
      ROAT77=ROH*QK1*QK2*QK3*QK4 
      RETURN 
120   CONTINUE 
!   BùþéCìEHéE  HH=H*RM/(H+RM) -- (äìñ H<120 KM)
      IF(H.LE.0._pm_reel) GOTO 7777 
      HH=H/RM+1._pm_reel 
      HH=H/HH 
!   BùþéCìEHéE ROAT77 -- ( äìñ H<120 KM )
      J=12 
1111  J=J-1 
      DH=HH-HM(1,J) 
      IF(DH) 1111,3333,2222 
2222  RAB=HM(2,J+1)-HM(2,J) 
      IF(RAB) 4444,5555,4444 
4444  RAB=(HM(1,J+1)-HM(1,J))/RAB 
      TM=DH/RAB+HM(2,J) 
      RAB=(HM(2,J)/TM)**(G0R*RAB+1._pm_reel) 
      GOTO 6666 
5555  RAB=DEXP(-G0R*DH/HM(2,J)) 
6666  ROAT77=RAB*HM(3,J) 
      RETURN 
7777  J=2 
3333  ROAT77=HM(3,J-1) 
      RETURN 
600   ROAT77=0._pm_reel 
      RETURN 
      
   end subroutine cps_roat77 

end module CPS_ATMOSPHERE_TERRE
