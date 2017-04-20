module MSP_PROPULSION_IMP_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PROPULSION_IMP_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux poussées impulsionnelles.
!
!$Description
!  Module contenant les informations de base aux modules liés aux poussées impulsionnelles.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_PROPULSION_IMP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_PROPULSION_IMP_DEF.F90,v $
!  Revision 1.10  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.9  2008/11/19 13:33:52  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.8  2008/04/24 13:59:06  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.7  2007/10/23 15:00:27  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.6  2005/03/08 07:32:37  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.5  2004/10/21 14:39:42  vivaresf
!  Messages d'erreurs génériques
!  Revision 1.4  2004/05/03 15:24:04  vivaresf
!  DM_83
!  Revision 1.3.2.2  2004/05/03 14:49:27  vivaresf
!  DM-ID 83, affichage de l'origine des dates
!  Revision 1.3.2.1  2004/04/26 14:46:02  vivaresf
!  DM-ID 83 : Dates stockées en jour/secondes (si absolues) avec le type tm_jour_sec
!  date_js en parametres optionnel des fonctions d'accès
!  Revision 1.3  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/07 18:37:35  adm_ipsi
!  Ajout de l'usage du module MSP_MECASPA_DEF
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.3  2002/09/16 11:12:37  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.2  2000/06/15 07:20:14  util_am
!  !  - Privatisation du contenu de la structure MSP_IMPULSION
!  !  - Ajout de la MSP_afficher_impulsion
!  !  - Transfert des routines MSP_consulter_loi_imp, MSP_modifier_loi_imp de MSP_PROPULSION_IMP.F90
!  !    en les renommant en MSP_consulter_impulsion MSP_modifier_impulsion
!  !  - Ajout d'interface anglaise
!  !  - Mise à jour des cartouches
!  !
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_PROPULSION_IMP_DEF
!
!$Structure
!
!: MSP_IMPULSION : définition d'une loi de poussée impulsionelle
!#V
!>     typdat    : <integer,private>      type de date:
!.                           1 => date [jour / secondes]
!.                           2 => durée [s]
!>     date      : <tm_jour_sec,private>  date [ jour / secondes]
!>     duree     : <pm_reel,private>      durée [s]
!>     origdat   : <integer,private>      origine des dates 0 : J1950, 1 : J2000
!>     deltav    : <pm_reel,private>      incrément de vitesse [m/s]
!>     dirref    : <integer,private>      repère de référence dans lequel est définie la direction de poussée:
!>                                   -1: par rapport au repère véhicule
!>                                   >0: voir les repères définis pour l'attitude
!>     omega     : <pm_reel,private>      angle dans le plan de symétrie du véhicule [rad]
!>     omegap    : <pm_reel,private>      angle hors du plan de symétrie du véhicule [rad]
!>     merg      : <pm_reel,private>      masse d'ergols dépensée pendant la poussée [kg]
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_impulse
!- MSP_get_impulse_data
!- MSP_set_impulse_data
!- MSP_display_impulse
!- MSP_consulter_impulsion
!- MSP_modifier_impulsion
!- MSP_afficher_impulsion
!
!$Fonctions
!- MSP_creer_impulsion
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!- MSLIB
!- MSPRO
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> msp_display_impulse :   MSP_afficher_impulsion
!> msp_set_impulse_data :  MSP_modifier_impulsion
!> msp_get_impulse_data :  MSP_consulter_impulsion
!> msp_create_impulse :    MSP_creer_impulsion
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  PROPULSION IMPULSION
!
!$Voir-Aussi
!.  MSP_creer_impulsion MSP_create_impulse MSP_get_impulse_data MSP_set_impulse_data MSP_display_impulse
!.  MSP_consulter_impulsion MSP_modifier_impulsion MSP_afficher_impulsion
!
!$<>
!******************************************************************************
   use MSP_MECASPA_DEF
   use MSLIB, only : pm_reel
   use MSPRO, only : tm_jour_sec, tm_code_retour

   ! TYPES DERIVES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PROPULSION_IMP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   type MSP_IMPULSION
      private
      integer            :: typdat
      type(tm_jour_sec)  :: date
      real(KIND=pm_reel) :: duree
      integer            :: origdat       ! 0 : JJ1950, 1 : JJ2000 (1/1/2000 12h)
      real(KIND=pm_reel) :: deltav
      integer            :: dirref
      real(KIND=pm_reel) :: omega
      real(KIND=pm_reel) :: omegap
      real(KIND=pm_reel) :: merg
   end type MSP_IMPULSION

   ! SOUS-PROGRAMMES ET FONCTIONS

   interface MSP_create_impulse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_impulse
!
!$Resume
!  Creation of an impulse law
!
!$Description
!  Creation of an impulse law
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_impulse (typdat,[date],[deltav],[dirref],[omega],[omegap],[merg],&
!.            date_js, origdat)
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date
!.    real(KIND=pm_reel) :: deltav
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega, omegap, merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!.    type(MSP_IMPULSION) :: loi
!
!$Procedures
!- MSP_creer_impulsion
!
!$Remarques
!
!$Mots-cles
! PROPULSION IMPULSION CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_impulsion
   end interface

   interface MSP_get_impulse_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_impulse_data
!
!$Resume
!  Get characteristics of an impulse law
!
!$Description
!  Get characteristics of an impulse law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_impulse_data(loi_imp, [typdat], [date], [datedeb], [datefin], [deltav], [dirref], [omega], [omegap], [merg], [date_js], [origdat])
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    integer :: dirref
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Procedures
!- MSP_consulter_impulsion
!
!$Remarques
!
!$Mots-cles
! PROPULSION IMPULSION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_impulsion
   end interface

   interface MSP_set_impulse_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_impulse_data
!
!$Resume
!  Modify the characteristics of an impulse law
!
!$Description
!  Modify the characteristics of an impulse law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_impulse_data (loi_imp, [date], [deltav], [dirref], [omega], [omegap], [merg], [date_js],[origdat])
!.    type(MSP_IMPULSION) :: loi_imp
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    integer :: dirref
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Procedures
!- MSP_modifier_impulsion
!
!$Remarques
!
!$Mots-cles
! PROPULSION IMPULSION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_impulsion
   end interface

   interface MSP_display_impulse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_impulse
!
!$Resume
!  Display the characteristics of an impulse law
!
!$Description
!  Display the characteristics of an impulse law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_impulse(loi_imp,num)
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: num
!
!$Procedures
!- MSP_afficher_impulsion
!
!$Remarques
!
!$Mots-cles
! PROPULSION IMPULSION AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_impulsion
   end interface

   contains

   function MSP_creer_impulsion (typdat,date,deltav,dirref,omega,omegap,merg,&
        date_js, origdat) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_impulsion
!
!$Resume
!  Fonction servant à créer une loi de type MSP_IMPULSION.
!
!$Description
!  Fonction servant à créer une loi de type MSP_IMPULSION.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_impulsion (typdat,[date],[deltav],[dirref],[omega],[omegap],[merg],&
!.            [date_js], [origdat])
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date
!.    real(KIND=pm_reel) :: deltav
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega, omegap, merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!.    type(MSP_IMPULSION) :: loi
!
!$Arguments
!>E     typdat   :<integer>         type de date:
!.                                 1 => date [Jours Juliens CNES]
!.                                 2 => durée [s]
!>[E]   date     :<pm_reel>         date [JJ CNES ou s], inutilise si typdat=1 et dat_js présent
!>[E]   deltav   :<pm_reel>         incrément de vitesse [m/s]
!>[E]   dirref   :<integer>         repère de référence dans lequel est définie la direction de poussée [par défaut -1]
!>[E]   omega    :<pm_reel>         angle dans le plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   omegap   :<pm_reel>         angle hors du plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   merg     :<pm_reel>         masse d'ergols dépensée pendant la poussée [kg] [par défaut 0.]
!>[E]   date_js  :<tm_jour_sec>     Date en jour /secondes 1950
!>[E]   origdat  :<integer>         origine des dates 0 : J1950, 1 : J2000
!>S     loi      :<MSP_IMPULSION>   structure contenant les données de l'impulsion
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!
!$Include
!
!$Module
!#V
!- MSP_GESTION_ERREUR
!#
!
!$Remarques
!
!$Mots-cles
!  PROPULSION IMPULSION CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      use MSP_GESTION_ERREUR
      implicit none

      ! Parametres
      integer, intent(IN)            :: typdat

      ! Parametres optionnels
      real(KIND=pm_reel), intent(IN), optional :: date
      real(KIND=pm_reel), intent(IN), optional :: deltav
      integer, intent(IN), optional  :: dirref
      real(KIND=pm_reel), intent(IN), optional :: omega, omegap, merg
      type(tm_jour_sec), intent(in), optional  :: date_js
      integer, intent(IN), optional  :: origdat

      ! Retour fonction
      type(MSP_IMPULSION)  :: loi

      ! Variables locales
      type(tm_code_retour) :: code_retour

      ! Initialisations

      loi%date%jour = 0
      loi%date%sec = 0._PM_REEL
      loi%duree = 0._PM_REEL

      loi%typdat = typdat

      if (loi%typdat == 2) then 
         if (present(date))then
            loi%duree = date
         else
            call MSP_signaler_message(cle_mes="MSP_dateabs", &
                 message="La variable date doit être renseignée",&
                 routine="MSP_creer_impulsion")
            return
         endif
      else if (loi%typdat == 1) then 
         if (present(date_js)) then
            loi%date=date_js
         elseif (present(date)) then
            call md_jourfrac_joursec(date, loi%date, code_retour)
         else
            call MSP_signaler_message(cle_mes="MSP_dateabs", &
                 message="Les variables date ou date_js doivent être renseignées",&
                 routine="MSP_creer_impulsion")
            return
         endif
      ! Autres cas (Erreurs orrigées plus tard)
      else
         if (present(date))    loi%duree = date
         if (present(date_js)) loi%date=date_js
      endif

      if (present(deltav)) then
         loi%deltav = deltav
      else
         call MSP_signaler_message(cle_mes="MSP_dateabs", &
              message="La variable deltav doit être renseignée",&
              routine="MSP_creer_impulsion")
         return
      endif
         
      if ( PRESENT(dirref) ) then
         loi%dirref  = dirref
      else
         loi%dirref  = -1
      endif

      if ( PRESENT(omega) ) then
         loi%omega  = omega
      else
         loi%omega  = 0._pm_reel
      endif

      if ( PRESENT(omegap) ) then
         loi%omegap  = omegap
      else
         loi%omegap  = 0._pm_reel
      endif

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

   end function MSP_creer_impulsion
      



  SUBROUTINE MSP_consulter_impulsion(loi_imp, typdat, date, datedeb, datefin, deltav, dirref, omega, omegap, merg, date_js, origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_impulsion
!
!$Resume
!  Routine de consultation d'une loi impulsionnelle
!
!$Description
!  Routine de consultation d'une loi impulsionnelle
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_impulsion(loi_imp, [typdat], [date], [datedeb], [datefin], [deltav], [dirref], [omega], [omegap], [merg], [date_js], [origdat])
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    integer :: dirref
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Arguments
!>E     loi_imp  :<MSP_IMPULSION>   Loi impulsionnelle a consulter
!>[S]   typdat   :<integer>         Type de date utilisée pour décrire la loi
!.                                 1 => date [Jours Juliens CNES]
!.                                 2 => durée [s]
!>[S]   date     :<PM_REEL>         date de l'impulsion (JJ CNES ou s)
!>[S]   datedeb  :<PM_REEL>         date de début de poussée (JJ CNES ou s)
!>[S]   datefin  :<PM_REEL>         date de fion de poussée (JJ CNES ou s)
!>[S]   deltav   :<PM_REEL>         incrément de vitesse [m/s]
!>[S]   dirref   :<integer>         repère de référence dans lequel est définie la direction de poussée
!>[S]   omega    :<PM_REEL>         angle dans le plan de symétrie du véhicule [rad]
!>[S]   omegap   :<PM_REEL>         angle hors du plan de symétrie du véhicule [rad]
!>[S]   merg     :<PM_REEL>         masse d'ergols dépensée pendant la poussée [kg]
!>[S]   date_js  :<tm_jour_sec>     
!>[S]   origdat  :<integer>         origine des dates 0 : J1950, 1 : J2000
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION IMPULSION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(MSP_IMPULSION), intent(IN) :: loi_imp
    integer, intent(OUT), optional            :: typdat
    real(KIND=PM_REEL), intent(OUT), optional :: date
    real(KIND=PM_REEL), intent(OUT), optional :: deltav
    integer, intent(OUT), optional :: dirref
    real(KIND=PM_REEL), intent(OUT), optional :: omega
    real(KIND=PM_REEL), intent(OUT), optional :: omegap
    real(KIND=PM_REEL), intent(OUT), optional :: merg
    real(KIND=PM_REEL), intent(OUT), optional :: datedeb
    real(KIND=PM_REEL), intent(OUT), optional :: datefin
    type(tm_jour_sec) , intent(OUT), optional :: date_js
    integer, intent(OUT), optional  :: origdat

    ! variables locales
    type(tm_code_retour) :: code_retour

    if (PRESENT(typdat))  typdat  = loi_imp%typdat
    if (PRESENT(date)) then
       if (loi_imp%typdat == 1) then 
          call md_joursec_jourfrac(loi_imp%date, date, code_retour)
       else
          date = loi_imp%duree
       endif
    endif

    if (PRESENT(deltav))  deltav  = loi_imp%deltav
    if (PRESENT(dirref))  dirref  = loi_imp%dirref
    if (PRESENT(omega))   omega   = loi_imp%omega
    if (PRESENT(omegap))  omegap  = loi_imp%omegap
    if (PRESENT(merg))    merg    = loi_imp%merg
    if (PRESENT(date_js)) date_js = loi_imp%date
    if (PRESENT(origdat)) origdat = loi_imp%origdat

    if (PRESENT(datedeb)) then
       if (loi_imp%typdat == 1) then 
          call md_joursec_jourfrac(loi_imp%date, datedeb, code_retour)
       else
          datedeb = loi_imp%duree
       endif
    endif

    if (PRESENT(datefin)) then
       if (loi_imp%typdat == 1) then 
          call md_joursec_jourfrac(loi_imp%date, datefin, code_retour)
       else
          datefin = loi_imp%duree
       endif
    endif

  end SUBROUTINE MSP_consulter_impulsion
    

  SUBROUTINE MSP_modifier_impulsion (loi_imp, date, deltav, dirref, omega, omegap, merg, date_js,origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_impulsion
!
!$Resume
!  Routine de modification d'une loi impulsionnelle
!
!$Description
!  Routine de modification d'une loi impulsionnelle
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_impulsion (loi_imp, [date], [deltav], [dirref], [omega], [omegap], [merg], [date_js],[origdat])
!.    type(MSP_IMPULSION) :: loi_imp
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: deltav
!.    integer :: dirref
!.    real(KIND=PM_REEL) :: omega
!.    real(KIND=PM_REEL) :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    type(tm_jour_sec) :: date_js
!.    integer :: origdat
!
!$Arguments
!>E/S   loi_imp  :<MSP_IMPULSION>   Loi impulsionnelle à modifier
!>[E]   date     :<PM_REEL>         date de l'impulsion (JJ CNES ou s)
!>[E]   deltav   :<PM_REEL>         incrément de vitesse [m/s]
!>[E]   dirref   :<integer>         repère de référence dans lequel est définie la direction de poussée
!>[E]   omega    :<PM_REEL>         angle dans le plan de symétrie du véhicule [rad]
!>[E]   omegap   :<PM_REEL>         angle hors du plan de symétrie du véhicule [rad]
!>[E]   merg     :<PM_REEL>         masse d'ergols dépensée pendant la poussée [kg]
!>[E]   date_js  :<tm_jour_sec>     
!>[E]   origdat  :<integer>         origine des dates 0 : J1950, 1 : J2000
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
!  PROPULSION IMPULSION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    ! Parametres
    type(MSP_IMPULSION), intent(INOUT) :: loi_imp

    ! Parametyres optionnels
    real(KIND=PM_REEL), intent(IN), optional :: date
    real(KIND=PM_REEL), intent(IN), optional :: deltav
    integer, intent(IN), optional :: dirref
    real(KIND=PM_REEL), intent(IN), optional :: omega
    real(KIND=PM_REEL), intent(IN), optional :: omegap
    real(KIND=PM_REEL), intent(IN), optional :: merg
    type(tm_jour_sec), intent(in), optional  :: date_js
    integer, intent(in), optional  :: origdat

    ! Variables locales
    type(tm_code_retour) :: code_retour

    ! programme
    if (PRESENT(date_js)) then
       loi_imp%date = date_js
    elseif (PRESENT(date)) then
       if (loi_imp%typdat == 1) then 
          call md_jourfrac_joursec(date,loi_imp%date,code_retour)
       endif
    endif
    if (PRESENT(date).and.loi_imp%typdat == 2) loi_imp%duree = date

    if (PRESENT(deltav))  loi_imp%deltav  = deltav
    if (PRESENT(dirref))  loi_imp%dirref  = dirref
    if (PRESENT(omega))   loi_imp%omega   = omega
    if (PRESENT(omegap))  loi_imp%omegap  = omegap
    if (PRESENT(merg))    loi_imp%merg    = merg
    if (PRESENT(origdat)) loi_imp%origdat = origdat

  end SUBROUTINE MSP_modifier_impulsion
    

  SUBROUTINE MSP_afficher_impulsion(loi_imp,num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_impulsion
!
!$Resume
!  Routine d'affichage des caractéristiques d'une loi impulsionnelle
!
!$Description
!  Routine d'affichage des caractéristiques d'une loi impulsionnelle
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_impulsion(loi_imp,num)
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: num
!
!$Arguments
!>E     loi_imp  :<MSP_IMPULSION>   
!>E     num      :<integer>         
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
!  PROPULSION IMPULSION AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_IMPULSION), intent(IN) :: loi_imp
    integer, intent(IN) :: num

    ! variables locales
    character(len=8) :: oo

    ! Ecriture
    oo="MJD1950"
    if (loi_imp%origdat == 1) oo="MJD2000"

    write(num,'(a,i9)') "TYPDAT: ",loi_imp%typdat
    if (loi_imp%typdat == 1) then 
       write(num,'(a,a,i9,a,g21.12)') "DATE:   ",oo, loi_imp%date%jour, "-", loi_imp%date%sec
    else if (loi_imp%typdat == 2) then 
       write(num,'(a,g21.12)') "DATE:   ",loi_imp%duree
    end if
    write(num,'(a,g21.12)') "DELTAV: ",loi_imp%deltav
    write(num,'(a,i9)') "DIRREF: ",loi_imp%dirref
    write(num,'(a,g21.12)') "OMEGA:  ",loi_imp%omega
    write(num,'(a,g21.12)') "OMEGAP: ",loi_imp%omegap
    write(num,'(a,g21.12)') "MERG:   ",loi_imp%merg

  end SUBROUTINE MSP_afficher_impulsion

end module MSP_PROPULSION_IMP_DEF
