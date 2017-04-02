module MSP_PROPULSION_BUL_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PROPULSION_BUL_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux poussées de type bulletin.
!
!$Description
!  Module contenant les informations de base aux modules liés aux poussées de type bulletin.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_PROPULSION_BUL_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_PROPULSION_BUL_DEF.F90,v $
!  Revision 1.11  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.10  2008/11/19 13:33:38  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.9  2008/04/24 13:59:05  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.8  2006/11/09 09:14:00  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.7  2005/03/08 07:32:36  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.6  2004/07/07 10:38:43  vivaresf
!  DM_89
!  Revision 1.5.2.1  2004/06/24 10:48:22  vivaresf
!  DM-ID 89, MSP_afficher_propulsion_bul,
!  affichage en clair du type de bulletin
!  Revision 1.5  2004/05/06 09:38:17  vivaresf
!  DM-ID 83 prise en compte du bulletin avec dates en jours/secondes
!  Dates en jour/secondes et origine possible a J2000
!  Revision 1.4.2.1  2004/05/03 14:28:34  vivaresf
!  DM_83, version initiale
!  Revision 1.4  2003/02/19 10:21:01  adm_ipsi
!  Prise en compte de la nouvelle structure Bulletin
!  Revision 1.3  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/07 18:37:46  adm_ipsi
!  Ajout de l'usage du module MSP_MECASPA_DEF
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.2  2000/06/14 16:36:58  util_am
!  - Ajout du champ flag_func dans la structure MSP_POUSSEE_BULLETIN pour la gestion des fuites mémoires
!  - Privatisation du contenu de la structure MSP_POUSSEE_BULLETIN
!  - Ajout de la MSP_afficher_poussee_bulletin
!  - Transfert des routines MSP_consulter_loi_bul, MSP_modifier_loi_bul de MSP_PROPULSION_BUL.F90
!    en les renommant en MSP_consulter_poussee_bulletin MSP_modifier_poussee_bulletin
!  - Ajout d'interface anglaise
!  - Mise à jour des cartouches
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_PROPULSION_BUL_DEF
!
!$Structure
!
!: MSP_POUSSEE_BULLETIN : définition d'une loi de poussée de type bulletin.
!#V
!>     typdat    : <integer,private>       type de date:
!.                                1 => date [Jours Juliens CNES]
!.                                2 => durée [s]
!>     datdeb    : <tm_jour_sec,private>   date en jour/secondes
!>     duree     : <pm_reel,private>       durée en s
!>     origdat   : <integer,private>       origine des dates:
!.                                0 => J1950 (1/1/1950 00h)
!.                                1 => J2000 (1/1/2000 00h)
!>     bul       : <MSP_BULLETIN,private>  bulletin obtenu à la fin de la poussée
!>     merg      : <pm_reel,private>       masse d'ergols dépensée pendant la poussée [kg]
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_orbit_thrust
!- MSP_get_orbit_thrust_data
!- MSP_set_orbit_thrust_data
!- MSP_display_orbit_thrust
!- MSP_consulter_poussee_bulletin
!- MSP_modifier_poussee_bulletin
!- MSP_afficher_poussee_bulletin
!
!$Fonctions
!- MSP_creer_poussee_bulletin
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!- MSLIB
!- MSP_BULLETIN_DEF
!#
!
!$Interface
!> msp_set_orbit_thrust_data :  MSP_modifier_poussee_bulletin
!> msp_display_orbit_thrust :   MSP_afficher_poussee_bulletin
!> msp_create_orbit_thrust :    MSP_creer_poussee_bulletin
!> msp_get_orbit_thrust_data :  MSP_consulter_poussee_bulletin
!#V
!#
!
!$Remarques
!
!$Mots-cles
! PROPULSION BULLETIN
!
!$Voir-Aussi
!.  MSP_creer_poussee_bulletin MSP_create_orbit_thrust MSP_get_orbit_thrust_data MSP_set_orbit_thrust_data
!.  MSP_display_orbit_thrust MSP_consulter_poussee_bulletin MSP_modifier_poussee_bulletin
!.  MSP_afficher_poussee_bulletin
!
!$<>
!******************************************************************************

   use MSP_MECASPA_DEF
   use MSLIB, only : pm_reel
   use MSP_BULLETIN_DEF

   ! DEFINITIONS DE TYPES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PROPULSION_BUL_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   type MSP_POUSSEE_BULLETIN
      private
      integer            :: typdat
      type(tm_jour_sec)  :: datdeb
      real(KIND=pm_reel) :: duree
      integer            :: origdat
      type(MSP_BULLETIN) :: bul
      real(KIND=pm_reel) :: merg
   end type MSP_POUSSEE_BULLETIN

   ! SOUS-PROGRAMMES ET FONCTIONS

   interface MSP_create_orbit_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_orbit_thrust
!
!$Resume
!  Create a thrust defined by the end of thrust orbit
!
!$Description
!  Create a thrust defined by the end of thrust orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_orbit_thrust (typdat,datdeb,bul,[merg],[datdeb_js],[origdat])
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=pm_reel) :: merg
!.    type(tm_jour_sec) :: datdeb_js
!.    integer :: origdat
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!
!$Procedures
!- MSP_creer_poussee_bulletin
!
!$Remarques
!
!$Mots-cles
! PROPULSION BULLETIN CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_poussee_bulletin
   end interface

   interface MSP_get_orbit_thrust_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_orbit_thrust_data
!
!$Resume
!  Get characteristics of a thrust defined by the end of thrust orbit
!
!$Description
!  Get characteristics of a thrust defined by the end of thrust orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_orbit_thrust_data(loi_bul, [datefin], [typdat], [datedeb], [bul],&
!.           merg, datedeb_js, datefin_js, datbul, iorb, param,  typrep, lonref, &
!.           date_ref, lat, lon, alt, direction, planete, corcen, cle_date, &
!.           ech_temps_bul, modprec, mu, requa, apla, requa_r, apla_r, vitrot, &
!.           datbul_js, date_refjs,origdat)
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    type(tm_jour_sec) :: datedeb_js
!.    type(tm_jour_sec) :: datefin_js
!.    type(tm_jour_sec) :: datbul_js
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Procedures
!- MSP_consulter_poussee_bulletin
!
!$Remarques
!
!$Mots-cles
! PROPULSION BULLETIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_poussee_bulletin
   end interface

   interface MSP_set_orbit_thrust_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_orbit_thrust_data
!
!$Resume
!  Modify characteristics of a thrust defined by the end of thrust orbit
!
!$Description
!  Modify characteristics of a thrust defined by the end of thrust orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_orbit_thrust_data(loi_bul, [typdat], [datdeb], [bul], [merg], &
!.           datdeb_js, origdat, datbul, iorb, param, typrep, datbul_js, &
!.           lonref, date_ref, lat, lon, alt, direction,  &
!.           planete, corcen, cle_date, ech_temps_bul, ech_temps_rep, modprec, &
!.           mu, requa_r, apla_r, requa, apla, vitrot, pole_u, pole_v ,obli, date_refjs)
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul,ech_temps_rep, modprec
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: datdeb_js
!.    type(tm_jour_sec) :: datbul_js
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Procedures
!- MSP_modifier_poussee_bulletin
!
!$Remarques
!
!$Mots-cles
! PROPULSION BULLETIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_poussee_bulletin
   end interface

   interface MSP_display_orbit_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_orbit_thrust
!
!$Resume
!  Display characteristics of a thrust defined by the end of thrust orbit
!
!$Description
!  Display characteristics of a thrust defined by the end of thrust orbit
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_orbit_thrust(loi_bul, num)
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: num
!
!$Procedures
!- MSP_afficher_poussee_bulletin
!
!$Remarques
!
!$Mots-cles
! PROPULSION BULLETIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_poussee_bulletin
   end interface

   contains

   function MSP_creer_poussee_bulletin (typdat,datdeb,bul,merg,datdeb_js,origdat) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_poussee_bulletin
!
!$Resume
!  Fonction servant à crééer une loi de type MSP_POUSSEE_BULLETIN.
!
!$Description
!  Fonction servant à crééer une loi de type MSP_POUSSEE_BULLETIN.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_poussee_bulletin (typdat,datdeb,bul,[merg],[datdeb_js],[origdat])
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=pm_reel) :: merg
!.    type(tm_jour_sec) :: datdeb_js
!.    integer :: origdat
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!
!$Arguments
!>E     typdat     :<integer>                type de date:
!.                                        1 => date [Jours Juliens CNES]
!.                                        2 => durée [s]
!>E     datdeb     :<pm_reel>                date of thrust beginning
!>E     bul        :<MSP_BULLETIN>           bulletin obtenu à datefin (la date dans bul doit correspondre à datefin !)
!>[E]   merg       :<pm_reel>                masse d'ergols disponible [kg] [par défaut 0.]
!>[E]   datdeb_js  :<tm_jour_sec>            Date en jour/secondes
!>[E]   origdat    :<integer>                origine des dates 0 : J1950, 1 : J2000
!>S     loi        :<MSP_POUSSEE_BULLETIN>   structure contenant les données de la poussée continue
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION BULLETIN CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      ! Arguments obligatoires:
      integer, intent(IN)            :: typdat
      real(KIND=pm_reel), intent(IN) :: datdeb
      type(MSP_BULLETIN), intent(IN) :: bul

      ! Argument optionnel:
      real(KIND=pm_reel), intent(IN), optional :: merg
      type(tm_jour_sec) , intent(IN), optional :: datdeb_js
      integer, intent(IN), optional  :: origdat

      ! Variable de sortie de la fonction:
      type(MSP_POUSSEE_BULLETIN) :: loi

      ! Variables locales
      type(tm_code_retour)       :: code_retour

      loi%datdeb%jour = 0
      loi%datdeb%sec = 0._PM_REEL
      loi%duree = 0._PM_REEL

      loi%typdat = typdat
      if (loi%typdat == 1) then
         call md_jourfrac_joursec(datdeb, loi%datdeb, code_retour)
      else
         loi%duree = datdeb
      endif

      if(present(datdeb_js)) loi%datdeb = datdeb_js

      loi%bul    = bul

      if ( PRESENT(merg) ) then
         loi%merg  = merg
      else
         loi%merg  = 0._pm_reel
      endif

      if ( PRESENT(origdat) ) then
         loi%origdat  = origdat
      else
         loi%origdat  = 0
      endif

   end function MSP_creer_poussee_bulletin

  SUBROUTINE MSP_consulter_poussee_bulletin(loi_bul, datefin, typdat, datedeb, bul,&
       merg, datedeb_js, datefin_js, datbul, iorb, param,  typrep, lonref, &
       date_ref, lat, lon, alt, direction, planete, corcen, cle_date, &
       ech_temps_bul, modprec, mu, requa, apla, requa_r, apla_r, vitrot, &
       datbul_js, date_refjs,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_poussee_bulletin
!
!$Resume
!  Routine de consultation d'une loi de type bulletin
!
!$Description
!  Routine de consultation d'une loi de type bulletin
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_poussee_bulletin(loi_bul, [datefin], [typdat], [datedeb], [bul],&
!.           [merg], [datedeb_js], [datefin_js], [datbul], [iorb], [param],  [typrep], [lonref], &
!.           [date_ref], [lat], [lon], [alt], [direction], [planete], [corcen], [cle_date], &
!.           [ech_temps_bul], [modprec], [mu], [requa], [apla], [requa_r], [apla_r], [vitrot], &
!.           [datbul_js], [date_refjs],[origdat])
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul, modprec
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    type(tm_jour_sec) :: datedeb_js
!.    type(tm_jour_sec) :: datefin_js
!.    type(tm_jour_sec) :: datbul_js
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Arguments
!>E     loi_bul        :<MSP_POUSSEE_BULLETIN>   Loi de type bulletin a consulter
!>[S]   datefin        :<PM_REEL>                date de fin de poussée (JJ CNES ou s)
!>[S]   typdat         :<integer>                type de date (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVE)
!>[S]   datedeb        :<PM_REEL>                date de début de poussée (JJ CNES ou s)
!>[S]   bul            :<MSP_BULLETIN>           bulletin correspondant à celui de la loi bulletin
!>[S]   merg           :<PM_REEL>                masse d'ergols dépensée pendant la poussée [kg]
!>[S]   datedeb_js     :<tm_jour_sec>            Date de début de poussée (jour/secondes)
!>[S]   datefin_js     :<tm_jour_sec>            Date de fin de poussée (jour/secondes)
!>[S]   datbul         :<PM_REEL>                date du bulletin [JJ CNES]
!>[S]   iorb           :<integer>                type de coordonnées (pm_kep, pm_cir,
!>                                     pm_equa, pm_cir_equa, pm_car,
!>                                     pm_hpha, pm_geoc, 
!>                                     MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                     pm_geod_gps_ard)
!>[S]   param          :<PM_REEL,DIM=(6)>        coordonnées (cf. définition de iorb)
!>[S]   typrep         :<integer>                type de repère (pm_equa_vrai, pm_equa_moy, pm_ecli_moy,
!>                                     pm_equa_uai, pm_topo)
!>[S]   lonref         :<PM_REEL>                longitude de référence
!>[S]   date_ref       :<PM_REEL>                date de référence [JJ CNES]
!>[S]   lat            :<PM_REEL>                latitude station ou rampe [rad]
!>[S]   lon            :<PM_REEL>                longitude station ou rampe [rad]
!>[S]   alt            :<PM_REEL>                altitude station ou rampe [m]
!>[S]   direction      :<PM_REEL>                direction de la station
!>[S]   planete        :<integer>                planète (eph_mercure_PLA,eph_venus_PLA,eph_terre_PLA,eph_mars_PLA,
!>                                               eph_jupiter_PLA,eph_saturne_PLA,eph_uranus_PLA,eph_neptune_PLA,
!>                                               eph_pluton_PLA)
!>[S]   corcen         :<integer>                corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[S]   cle_date       :<integer>                échelle de date (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                          pm_autre_date, MSP_ENUM_ECHD_AUTRE_DATE)
!>[S]   ech_temps_bul  :<integer>                échelle de temps (pm_TUC, pm_TE) du bulletin
!>[S]   modprec        :<integer>                mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>[S]   mu             :<PM_REEL>                terme central du potentiel (m^3/s^2)
!>[S]   requa          :<PM_REEL>                rayon terrestre à l'équateur [m]
!>[S]   apla           :<PM_REEL>                aplatissement terrestre
!>[S]   requa_r        :<PM_REEL>                rayon terrestre à l'équateur pour les coordonnées station [m]
!>[S]   apla_r         :<PM_REEL>                aplatissement terrestre pour les coordonnées station 
!>[S]   vitrot         :<PM_REEL>                vitesse de rotation de la planete [rad/s]
!>[S]   datbul_js      :<tm_jour_sec>            Date du bulletin (jour/secondes)
!>[S]   date_refjs     :<tm_jour_sec>            Date de référence du repère bulletin (jour/secondes)
!>[S]   origdat        :<integer>                origine des dates 0 : J1950, 1 : J2000
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_consulter_bulletin
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION BULLETIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    ! Parametres
    type(MSP_POUSSEE_BULLETIN), intent(IN) :: loi_bul

    ! Parametres optionnels
    integer, intent(OUT), optional  :: typdat
    real(KIND=PM_REEL), intent(OUT), optional :: datedeb
    real(KIND=PM_REEL), intent(OUT), optional :: datefin
    type(MSP_BULLETIN), intent(OUT), optional :: bul
    real(KIND=PM_REEL), intent(OUT), optional :: merg

    real(KIND=PM_REEL), intent(OUT), optional :: datbul
    integer, intent(OUT), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(OUT), optional :: param
    integer, intent(OUT), optional            :: typrep

    real(KIND=PM_REEL), intent(OUT), optional :: lonref, date_ref, lat, lon, alt, direction
    integer, intent(OUT), optional            :: planete, corcen, cle_date, ech_temps_bul, modprec
    real(KIND=PM_REEL), intent(OUT), optional :: mu, requa, apla, requa_r, apla_r, vitrot
    type(tm_jour_sec) , intent(OUT), optional :: datedeb_js
    type(tm_jour_sec) , intent(OUT), optional :: datefin_js
    type(tm_jour_sec) , intent(OUT), optional :: datbul_js
    type(tm_jour_sec) , intent(OUT), optional :: date_refjs
    integer, intent(OUT), optional  :: origdat


    ! Variables locales
    type(tm_code_retour)       :: code_retour

    ! programme
    if (PRESENT(typdat))  typdat  = loi_bul%typdat
    if (PRESENT(origdat)) origdat = loi_bul%origdat

    if (PRESENT(datedeb)) then
       if (loi_bul%typdat == 1) then
          call md_joursec_jourfrac(loi_bul%datdeb, datedeb, code_retour)
       else
          datedeb = loi_bul%duree
       endif
    endif
    if(present(datedeb_js)) datedeb_js = loi_bul%datdeb
   
    if (PRESENT(bul))     bul     = loi_bul%bul
    if (PRESENT(merg))    merg    = loi_bul%merg
    if (PRESENT(datefin)) call MSP_consulter_bulletin(loi_bul%bul, datbul=datefin)
    if (PRESENT(datefin_js)) then
       call MSP_consulter_bulletin(loi_bul%bul, datbul_js=datefin_js)
    endif

    if ( PRESENT(datbul).or.PRESENT(iorb).or.PRESENT(param).or. &
         PRESENT(typrep).or.PRESENT(lonref).or.PRESENT(date_ref).or.PRESENT(lat).or. &
         PRESENT(lon).or.PRESENT(alt).or.PRESENT(datbul_js).or.  &
         PRESENT(direction).or.PRESENT(planete).or.PRESENT(corcen).or.PRESENT(cle_date).or. &
         PRESENT(ech_temps_bul).or.PRESENT(modprec).or.PRESENT(mu).or.PRESENT(requa) .or. &
         PRESENT(apla).or.PRESENT(vitrot).or.PRESENT(date_refjs) ) then 
    
       call MSP_consulter_bulletin(loi_bul%bul, datbul=datbul, iorb=iorb, &
            param=param, typrep=typrep, lonref=lonref, date_ref=date_ref, lat=lat, &
            lon=lon, alt=alt,  date_refjs=date_refjs, direction=direction, &
            planete=planete, corcen=corcen,cle_date=cle_date, mu=mu, &
            ech_temps_bul=ech_temps_bul, modprec=modprec, datbul_js=datbul_js, &
            requa=requa, apla=apla, requa_r=requa_r, apla_r=apla_r, vitrot=vitrot)

    end if

  end SUBROUTINE MSP_consulter_poussee_bulletin



  SUBROUTINE MSP_modifier_poussee_bulletin(loi_bul, typdat, datdeb, bul, merg, &
       datdeb_js, origdat, datbul, iorb, param, typrep, datbul_js, &
       lonref, date_ref, lat, lon, alt, direction,  &
       planete, corcen, cle_date, ech_temps_bul, ech_temps_rep, modprec, &
       mu, requa_r, apla_r, requa, apla, vitrot, pole_u, pole_v ,obli, date_refjs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_poussee_bulletin
!
!$Resume
!  Routine de modification d'une loi de type bulletin
!
!$Description
!  Routine de modification d'une loi de type bulletin
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_poussee_bulletin(loi_bul, [typdat], [datdeb], [bul], [merg], &
!.           [datdeb_js], [origdat], [datbul], [iorb], [param], [typrep], [datbul_js], &
!.           [lonref], [date_ref], [lat], [lon], [alt], [direction],  &
!.           [planete], [corcen], [cle_date], [ech_temps_bul], [ech_temps_rep], [modprec], &
!.           [mu], [requa_r], [apla_r], [requa], [apla], [vitrot], [pole_u], [pole_v] ,[obli], [date_refjs])
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datbul
!.    integer :: iorb 
!.    real(KIND=PM_REEL), dimension(6) :: param
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: lonref, date_ref, lat, lon, alt, direction
!.    integer :: planete, corcen, cle_date, ech_temps_bul,ech_temps_rep, modprec
!.    real(KIND=PM_REEL) :: mu, requa, apla, requa_r, apla_r, vitrot
!.    real(KIND=PM_REEL) :: pole_u, pole_v, obli
!.    type(tm_jour_sec) :: datdeb_js
!.    type(tm_jour_sec) :: datbul_js
!.    type(tm_jour_sec) :: date_refjs
!.    integer :: origdat
!
!$Arguments
!>E/S   loi_bul        :<MSP_POUSSEE_BULLETIN>   Loi de type bulletin a modifier
!>[E]   typdat         :<integer>                type de date utilisé (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVE)
!>[E]   datdeb         :<PM_REEL>                date de début de poussée (JJ CNES ou s)
!>[E]   bul            :<MSP_BULLETIN>           bulletin correspondant à celui de la loi bulletin
!>[E]   merg           :<PM_REEL>                masse d'ergols dépensée pendant la poussée [kg]
!>[E]   datdeb_js      :<tm_jour_sec>            date de début de poussée (jour/secondes)
!>[E]   origdat        :<integer>                origine des dates 0 : J1950, 1 : J2000
!>[E]   datbul         :<PM_REEL>                date du bulletin [JJ CNES]
!>[E]   iorb           :<integer>                type de coordonnées (pm_kep, pm_cir,
!>                                     pm_equa, pm_cir_equa, pm_car,
!>                                     pm_hpha, pm_geoc,
!>                                     MSP_ENUM_RENTREE_SPHERIQUE, pm_geod_meca_vol,
!>                                     pm_geod_gps_ard)
!>[E]   param          :<PM_REEL,DIM=(6)>        coordonnées (cf. définition de iorb)
!>[E]   typrep         :<integer>                type de repère (pm_equa_vrai, pm_equa_moy, pm_ecli_moy,
!>                                     pm_equa_uai, pm_topo)
!>[E]   datbul_js      :<tm_jour_sec>            date du bulletin (jour/secondes)
!>[E]   lonref         :<PM_REEL>                longitude de référence
!>[E]   date_ref       :<PM_REEL>                date de référence [JJ CNES]
!>[E]   lat            :<PM_REEL>                latitude station ou rampe [rad]
!>[E]   lon            :<PM_REEL>                longitude station ou rampe [rad]
!>[E]   alt            :<PM_REEL>                altitude station ou rampe [m]
!>[E]   direction      :<PM_REEL>                direction de la station
!>[E]   planete        :<integer>                planète (eph_mercure_PLA,eph_venus_PLA,eph_terre_PLA,eph_mars_PLA,
!>                                               eph_jupiter_PLA,eph_saturne_PLA,eph_uranus_PLA,
!>                                               eph_neptune,eph_pluton_PLA)
!>[E]   corcen         :<integer>                corps central (eph_mercure,eph_venus,eph_terre,eph_mars,
!>                                          eph_jupiter,eph_saturne,eph_uranus,eph_neptune,eph_pluton,
!>                                          eph_soleil)
!>[E]   cle_date       :<integer>                échelle de date (pm_1janvier1950_00h00, pm_1janvier2000_12h00,
!>                                          pm_autre_date, MSP_ENUM_ECHD_AUTRE_DATE)
!>[E]   ech_temps_bul  :<integer>                échelle de temps du bulletin (pm_TUC, pm_TE)
!>[E]   ech_temps_rep  :<integer>                échelle de temps du repere (pm_TUC, pm_TE)
!>[E]   modprec        :<integer>                mode de précession (pm_lieske_wahr_aoki, pm_uai1994)
!>[E]   mu             :<PM_REEL>                terme central du potentiel (m^3/s^2)
!>[E]   requa_r        :<PM_REEL>                rayon terrestre à l'équateur pour les coordonnées station  [m]
!>[E]   apla_r         :<PM_REEL>                aplatissement terrestre pour les coordonnées station 
!>[E]   requa          :<PM_REEL>                rayon terrestre à l'équateur [m]
!>[E]   apla           :<PM_REEL>                aplatissement terrestre
!>[E]   vitrot         :<PM_REEL>                vitesse de rotation de la planete [rad/s]
!>[E]   pole_u         :<PM_REEL>                angle u du pole vrai
!>[E]   pole_v         :<PM_REEL>                angle v du pole vrai
!>[E]   obli           :<PM_REEL>                obliquite
!>[E]   date_refjs     :<tm_jour_sec>            Date de référence du repère bulletin (jour/seocndes)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_modifier_bulletin
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION BULLETIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    ! Parametres
    type(MSP_POUSSEE_BULLETIN), intent(INOUT) :: loi_bul

    ! Parametres optionnels
    integer, intent(IN), optional  :: typdat
    real(KIND=PM_REEL), intent(IN), optional :: datdeb
    type(MSP_BULLETIN), intent(IN), optional :: bul
    real(KIND=PM_REEL), intent(IN), optional :: merg

    real(KIND=PM_REEL), intent(IN), optional :: datbul
    integer, intent(IN), optional            :: iorb         
    real(KIND=PM_REEL), dimension(6), intent(IN), optional :: param
    integer, intent(IN), optional            :: typrep

    real(KIND=PM_REEL), intent(IN), optional :: lonref, date_ref, lat, lon, alt, direction
    integer, intent(IN), optional            :: planete, corcen, cle_date, ech_temps_bul,ech_temps_rep, modprec
    real(KIND=PM_REEL), intent(IN), optional :: mu, requa, apla, requa_r, apla_r, vitrot
    real(KIND=PM_REEL), intent(IN), optional :: pole_u, pole_v, obli
    type(tm_jour_sec) , intent(IN), optional :: datdeb_js
    type(tm_jour_sec) , intent(IN), optional :: datbul_js
    type(tm_jour_sec) , intent(IN), optional :: date_refjs
    integer, intent(IN), optional  :: origdat

    ! Variables locales
    type(tm_code_retour)       :: code_retour

    ! programme
    if (PRESENT(bul))     loi_bul%bul     = bul

    if (PRESENT(typdat))  loi_bul%typdat  = typdat
    if (PRESENT(origdat)) loi_bul%origdat = origdat
    if (PRESENT(datdeb)) then
       if (loi_bul%typdat == 1) then
          call md_jourfrac_joursec(datdeb, loi_bul%datdeb, code_retour)
       else
          loi_bul%duree = datdeb
       endif
       if(present(datdeb_js)) loi_bul%datdeb = datdeb_js
    endif

    if (PRESENT(merg))   loi_bul%merg   = merg

    if ( PRESENT(datbul).or.PRESENT(iorb).or.PRESENT(param).or. &
         PRESENT(typrep).or.PRESENT(lonref).or.PRESENT(date_ref).or.PRESENT(lat).or. &
         PRESENT(lon).or.PRESENT(alt).or.PRESENT(direction).or. &
         PRESENT(planete).or.PRESENT(corcen).or.PRESENT(cle_date).or. &
         PRESENT(ech_temps_bul).or.PRESENT(ech_temps_rep).or.PRESENT(modprec).or.&
         PRESENT(mu).or.PRESENT(requa).or.PRESENT(datbul_js) .or. &
         PRESENT(apla).or.PRESENT(vitrot).or.PRESENT(pole_u).or.PRESENT(pole_v).or.&
         PRESENT(obli).or.PRESENT(date_refjs) ) then 
    
       call MSP_modifier_bulletin(loi_bul%bul, datbul=datbul, iorb=iorb, &
            param=param, typrep=typrep, lonref=lonref, date_ref=date_ref, lat=lat, &
            lon=lon, alt=alt, direction=direction, planete=planete, corcen=corcen, &
            cle_date=cle_date, ech_temps_bul=ech_temps_bul, datbul_js=datbul_js,&
            ech_temps_rep=ech_temps_rep, modprec=modprec, mu=mu, requa=requa, &
            apla=apla, requa_r=requa_r, apla_r=apla_r, vitrot=vitrot, &
            pole_u=pole_u, pole_v=pole_v, date_refjs=date_refjs, obli=obli)

    end if

  end SUBROUTINE MSP_modifier_poussee_bulletin

  
  subroutine MSP_afficher_poussee_bulletin(loi_bul, num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_poussee_bulletin
!
!$Resume
!  Routine réalisant l'affichage d'une loi de poussée de type bulletin
!
!$Description
!  Routine réalisant l'affichage d'une loi de poussée de type bulletin
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_poussee_bulletin(loi_bul, num)
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: num
!
!$Arguments
!>E     loi_bul  :<MSP_POUSSEE_BULLETIN>   Loi de type bulletin à afficher
!>E     num      :<integer>                unité logique d'affichage
!
!$Common
!
!$Routines
!- MSP_consulter_bulletin
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION BULLETIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Parametres
    type(MSP_POUSSEE_BULLETIN), intent(IN) :: loi_bul
    integer, intent(IN) :: num

    ! Variables locales
    type(tm_jour_sec) :: datbul
    integer :: iorb, origdat
    real(KIND=PM_REEL), dimension(10) :: rep
    real(KIND=PM_REEL), dimension(6)  :: param
    character(len=8) :: oo

    ! Programme
    write(num,'(a,i9)') "TYPDAT:   ",loi_bul%typdat
    oo="MJD1950"
    if (loi_bul%origdat == 1) oo="MJD2000"

    ! dates absolues
    if (loi_bul%typdat == 1) then
       write(num,'(a,a,i9,a,g21.12)') "DATDEB:   ", oo, loi_bul%datdeb%jour, "-", loi_bul%datdeb%sec
    ! dates relatives
    else
       write(num,'(a,g21.12)') "DATDEB:   ", loi_bul%duree
    endif

    call MSP_consulter_bulletin(loi_bul%bul, datbul_js=datbul, iorb=iorb, &
         param=param, rep=rep,origdat=origdat)
    oo="MJD1950"
    if (origdat == 1) oo="MJD2000"
    write(num,'(a,a,i9,a,g21.12)') "DATBUL:   ", oo, datbul%jour, "-", datbul%sec
    write(num,'(a,a)') "IORB:     ", trim(MSP_type_bulletin(iorb))
    write(num,'(a,6(g21.12))') "COORD:    ", param(:)
    write(num,'(a,10(g21.12))') "REP:      ", rep(:)
    write(num,'(a,g21.12)') "MERG:     ", loi_bul%merg

  end subroutine MSP_afficher_poussee_bulletin

end module  MSP_PROPULSION_BUL_DEF
  
