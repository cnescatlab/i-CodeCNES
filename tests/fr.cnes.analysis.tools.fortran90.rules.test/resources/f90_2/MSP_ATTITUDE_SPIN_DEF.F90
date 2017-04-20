module MSP_ATTITUDE_SPIN_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATTITUDE_SPIN_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux lois d'attitude spinnées.
!
!$Description
!  Module contenant les informations de base aux modules liés aux lois d'attitude spinnées.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_ATTITUDE_SPIN_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATTITUDE_SPIN_DEF.F90,v $
!  Revision 1.12  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.11  2009/10/28 17:18:31  mercadig
!  DM-ID 1018: Mise a jour cartouche
!
!  Revision 1.10  2008/11/19 13:31:10  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.9  2008/04/24 13:59:02  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.8  2007/10/23 15:01:41  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.7  2006/11/15 10:09:37  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.6  2006/11/15 09:58:09  tanguyy
!  DM-ID 552 : rajout d'un champ pour distinguer les types d'angles (Cardan, Euler..)
!  Revision 1.5  2005/03/08 07:32:34  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.4  2004/05/03 15:24:21  vivaresf
!  DM_83
!  Revision 1.3.2.1  2004/05/03 14:59:11  vivaresf
!  DM-ID 83,dates en jour / secondes avec origine MJD1950 ou MJD2000
!  Revision 1.3  2002/12/03 17:21:01  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/07 18:36:52  adm_ipsi
!  Ajout de l'usage du module MSP_MECASPA_DEF
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.4  2000/07/04 13:43:40  util_am
!  Mise à jour des cartouches : Ajout des types de repères MSP_ENUM_ATTI_LVLH et MSP_ENUM_ATTI_YAW_STEERING
!  Revision 1.3  2000/06/13 11:33:09  util_am
!  Privatisation du contenu de la structure MSP_ATTITUDE_SPINNEE
!  Ajout des routines MSP_consulter_attitude_spinnee
!                     MSP_modifier_attitude_spinnee
!                     MSP_afficher_attitude_spinnee
!  Ajout des interface en anglais des routines publiques
!  Mise à jour des cartouches : ajout des sections Voir-Aussi et Mots-Cles
!  Revision 1.2  1999/10/25 15:27:05  util_am
!  Modification du type de repère inertiel (Gamma 50 CNES => inertiel)
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_ATTITUDE_SPIN_DEF
!
!$Structure
!
!: MSP_ATTITUDE_SPINNEE : définition d'une loi d'attitude spinnée
!#V
!>     typdat      : <integer,private>      type de date:
!.                           1 => date [Jours Juliens CNES]
!.                           2 => durée [s]
!.                           3 => datedeb en Jour / sec, datefin en durée [s]
!>     datdeb      : <tm_jour_sec,private>  si typdat==1, date de début de spin [ jour/secondes ]
!>     datfin      : <tm_jour_sec,private>  si typdat==1, date de fin de spin   [ jour/secondes ]
!>     datdebrel   : <pm_reel,private>      si typdat==2, date de début de spin (secondes)
!>     datfinrel   : <pm_reel,private>      si typdat==2, date de fin de spin (secondes)
!>     origdat     : <integer,private>      origine des dates (0=J1950,1=J2000)
!>     typangle    : <integer,private>      
!>     typrep      : <integer,private>      type de repère:
!.                           MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                           MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                           MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                           MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                           MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                           MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                           MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                           MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!.                           MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                           MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>     psi0        : <pm_reel,private>      lacet initial [rad]
!>     teta0       : <pm_reel,private>      tangage initial [rad]
!>     phi0        : <pm_reel,private>      roulis initial [rad]
!>     psip        : <pm_reel,private>      vitesse en lacet [rad/s]
!>     tetap       : <pm_reel,private>      vitesse en tangage [rad/s]
!>     phip        : <pm_reel,private>      vitesse en roulis [rad/s]
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_spinned_attitude
!- MSP_get_spinned_attitude_data
!- MSP_set_spinned_attitude_data
!- MSP_display_spinned_attitude
!- MSP_consulter_attitude_spinnee
!- MSP_modifier_attitude_spinnee
!- MSP_afficher_attitude_spinnee
!
!$Fonctions
!- MSP_creer_attitude_spinnee
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!- MSLIB
!- mspro
!- MSP_ATTITUDE_TAB_DEF
!#
!
!$Interface
!> msp_create_spinned_attitude :    MSP_creer_attitude_spinnee
!> msp_get_spinned_attitude_data :  MSP_consulter_attitude_spinnee
!> msp_set_spinned_attitude_data :  MSP_modifier_attitude_spinnee
!> msp_display_spinned_attitude :   MSP_afficher_attitude_spinnee
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ATTITUDE SPIN
!
!$Voir-Aussi
!.  MSP_creer_attitude_spinnee MSP_create_spinned_attitude MSP_get_spinned_attitude_data
!.  MSP_set_spinned_attitude_data MSP_display_spinned_attitude MSP_consulter_attitude_spinnee
!.  MSP_modifier_attitude_spinnee MSP_afficher_attitude_spinnee
!
!$<>
!******************************************************************************

   use MSP_MECASPA_DEF
   use MSLIB, only : pm_reel,tm_jour_sec
   use mspro
   use MSP_ATTITUDE_TAB_DEF

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATTITUDE_SPIN_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   ! DEFINITIONS DE TYPES:

   type MSP_ATTITUDE_SPINNEE
      private
      integer            :: typdat
      type(tm_jour_sec)  :: datdeb
      type(tm_jour_sec)  :: datfin
      real(KIND=pm_reel) :: datdebrel
      real(KIND=pm_reel) :: datfinrel
      integer            :: origdat
      integer            :: typangle
      integer            :: typrep
      real(KIND=pm_reel) :: psi0
      real(KIND=pm_reel) :: teta0
      real(KIND=pm_reel) :: phi0
      real(KIND=pm_reel) :: psip
      real(KIND=pm_reel) :: tetap
      real(KIND=pm_reel) :: phip
   end type MSP_ATTITUDE_SPINNEE

   ! SOUS-PROGRAMMES ET FONCTIONS


   ! >>>>>>>>>>>>>> Interface anglaise 
   interface MSP_create_spinned_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_spinned_attitude
!
!$Resume
!  Creation of spinned attitude law
!
!$Description
!  Creation of spinned attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_spinned_attitude(typdat,datedeb,datefin,typrep,[psi0],[teta0],&
!.            phi0,psip,tetap,phip, datedeb_js, datefin_js, origdat, typangle)
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datedeb,datefin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0,teta0,phi0
!.    real(KIND=pm_reel) :: psip,tetap,phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!
!$Procedures
!- MSP_creer_attitude_spinnee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE SPIN CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_attitude_spinnee
   end interface

   interface MSP_get_spinned_attitude_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_spinned_attitude_data
!
!$Resume
!  Get characteristics of a spinned attitude law
!
!$Description
!  Get characteristics of a spinned attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_spinned_attitude_data(loi_spin, [typdat], [datedeb], [datefin], &
!.           typrep, psi0, teta0, phi0, psip, tetap, phip, datedeb_js, datefin_js,origdat, typangle)
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: psi0, teta0, phi0
!.    real(KIND=PM_REEL) :: psip, tetap, phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!
!$Procedures
!- MSP_consulter_attitude_spinnee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE SPIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_attitude_spinnee
   end interface
   
   interface MSP_set_spinned_attitude_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_spinned_attitude_data
!
!$Resume
!  Modify characteristics of a spinned attitude law
!
!$Description
!  Modify characteristics of a spinned attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_spinned_attitude_data(loi_spin, [typdat], [datedeb], [datefin], &
!.           typrep, psi0, teta0, phi0, psip, tetap, phip, datedeb_js, datefin_js,origdat,typangle)
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0, teta0, phi0
!.    real(KIND=pm_reel) :: psip, tetap, phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!
!$Procedures
!- MSP_modifier_attitude_spinnee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE SPIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_attitude_spinnee
   end interface

   interface MSP_display_spinned_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_spinned_attitude
!
!$Resume
!  Display characteristics of a spinned attitude law
!
!$Description
!  Display characteristics of a spinned attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_spinned_attitude(loi_spin, num)
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: num
!
!$Procedures
!- MSP_afficher_attitude_spinnee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE SPIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_attitude_spinnee
   end interface


   contains

   function MSP_creer_attitude_spinnee(typdat,datedeb,datefin,typrep,psi0,teta0,&
        phi0,psip,tetap,phip, datedeb_js, datefin_js, origdat, typangle) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_attitude_spinnee
!
!$Resume
!  Fonction servant à créer une loi de type MSP_ATTITUDE_SPINNEE.
!
!$Description
!  Fonction servant à créer une loi de type MSP_ATTITUDE_SPINNEE.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_attitude_spinnee(typdat,datedeb,datefin,typrep,[psi0],[teta0],&
!.            [phi0],[psip],[tetap],[phip], [datedeb_js], [datefin_js], [origdat], [typangle])
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datedeb,datefin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0,teta0,phi0
!.    real(KIND=pm_reel) :: psip,tetap,phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!
!$Arguments
!>E     typdat      :<integer>                type de date:
!.                                        1 => date [Jours Juliens CNES]
!.                                        2 => durée [s]
!>E/S   datedeb     :<pm_reel>                Date de début de la loi
!>E/S   datefin     :<pm_reel>                Date de fin de la loi
!>E     typrep      :<integer>                type de repère:
!.                                        MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                        MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                        MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                        MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                        MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                        MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                        MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                        MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                        MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[E]   psi0        :<pm_reel>                lacet initial [rad] [par défaut 0.]
!>[E]   teta0       :<pm_reel>                tangage initial [rad] [par défaut 0.]
!>[E]   phi0        :<pm_reel>                roulis initial [rad] [par défaut 0.]
!>[E]   psip        :<pm_reel>                vitesse en lacet [rad/s] [par défaut 0.]
!>[E]   tetap       :<pm_reel>                vitesse en tangage [rad/s] [par défaut 0.]
!>[E]   phip        :<pm_reel>                vitesse en  roulis [rad/s] [par défaut 0.]
!>[E]   datedeb_js  :<tm_jour_sec>            Date de début de la loi (jour/secondes)    
!>[E]   datefin_js  :<tm_jour_sec>            Date de fin de la loi   (jour/secondes)
!>[E]   origdat     :<integer>                origine des dates (0=J1950,1=J2000)
!>[E]   typangle    :<integer>                Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
!>S     loi         :<MSP_ATTITUDE_SPINNEE>   structure contenant les données d'attitude de type spinnée
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
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
!  ATTITUDE SPIN CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************
     use MSLIB

      implicit none

      ! Arguments obligatoires:
      integer, intent(IN) :: typdat
      real(KIND=pm_reel)  :: datedeb,datefin
      integer, intent(IN) :: typrep

      ! Arguments optionnels:
      real(KIND=pm_reel), intent(IN), optional :: psi0,teta0,phi0
      real(KIND=pm_reel), intent(IN), optional :: psip,tetap,phip
      type(tm_jour_sec) , intent(IN), optional :: datedeb_js, datefin_js
      integer, intent(IN), optional            :: origdat
      integer, intent(IN), optional            :: typangle

      ! Variable de sortie de la fonction:
      type(MSP_ATTITUDE_SPINNEE) :: loi

      ! variables locales
      type(tm_code_retour) :: code_retour


      ! Initialisations
      loi%datdeb%jour = 0
      loi%datfin%jour = 0
      loi%datdeb%sec = 0._PM_REEL
      loi%datfin%sec = 0._PM_REEL
      loi%datdebrel = 0._PM_REEL
      loi%datfinrel = 0._PM_REEL

      ! corps de la fonction
      loi%typdat = typdat      

      if (loi%typdat == 1) then
         call md_jourfrac_joursec(datedeb,loi%datdeb,code_retour)
         call md_jourfrac_joursec(datefin,loi%datfin,code_retour)
      else if (loi%typdat == 3) then
         call md_jourfrac_joursec(datedeb,loi%datdeb,code_retour)
         loi%datfinrel = datefin
      else 
         loi%datdebrel = datedeb
         loi%datfinrel = datefin
      endif
      loi%typrep = typrep

      if ( PRESENT(datedeb_js) ) loi%datdeb = datedeb_js
      if ( PRESENT(datefin_js) ) loi%datfin = datefin_js

      if ( PRESENT(psi0) ) then
         loi%psi0 = psi0
      else
         loi%psi0 = 0._pm_reel
      endif

      if ( PRESENT(origdat) ) then
         loi%origdat = origdat
      else
         loi%origdat = 0
      endif

      if ( PRESENT(teta0) ) then
         loi%teta0 = teta0
      else
         loi%teta0 = 0._pm_reel
      endif

      if ( PRESENT(phi0) ) then
         loi%phi0 = phi0
      else
         loi%phi0 = 0._pm_reel
      endif

      if ( PRESENT(psip) ) then
         loi%psip = psip
      else
         loi%psip = 0._pm_reel
      endif

      if ( PRESENT(tetap) ) then
         loi%tetap = tetap
      else
         loi%tetap = 0._pm_reel
      endif

      if ( PRESENT(phip) ) then
         loi%phip = phip
      else
         loi%phip = 0._pm_reel
      endif

      if ( PRESENT(typangle)) then
         loi%typangle = typangle
      else
         ! Par défaut, on utilise la convention suivante : angles de Cardan selon Z, Y puis X
         loi%typangle = pm_1z_2y_3x
      end if

    end function MSP_creer_attitude_spinnee
      
  SUBROUTINE MSP_consulter_attitude_spinnee(loi_spin, typdat, datedeb, datefin, &
       typrep, psi0, teta0, phi0, psip, tetap, phip, datedeb_js, datefin_js,origdat, typangle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_attitude_spinnee
!
!$Resume
!  Routine de consultation d'une loi d'attitude spinnee
!
!$Description
!  Routine de consultation d'une loi d'attitude spinnee
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_attitude_spinnee(loi_spin, [typdat], [datedeb], [datefin], &
!.           [typrep], [psi0], [teta0], [phi0], [psip], [tetap], [phip], [datedeb_js], [datefin_js],[origdat], [typangle])
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: typrep
!.    real(KIND=PM_REEL) :: psi0, teta0, phi0
!.    real(KIND=PM_REEL) :: psip, tetap, phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!
!$Arguments
!>E     loi_spin    :<MSP_ATTITUDE_SPINNEE>   Loi d'attitude spinnee a consulter
!>[S]   typdat      :<integer>                type de date utilisé
!>[S]   datedeb     :<PM_REEL>                Date de début de la loi
!>[S]   datefin     :<PM_REEL>                Date de fin de la loi
!>[S]   typrep      :<integer>                type de repère:
!.                                               MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                               MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                               MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                               MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                               MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                               MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                               MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                               MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                               MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[S]   psi0        :<PM_REEL>                lacet initial [rad]
!>[S]   teta0       :<PM_REEL>                tangage initial [rad]
!>[S]   phi0        :<PM_REEL>                roulis initial [rad]
!>[S]   psip        :<PM_REEL>                vitesse en lacet [rad/s]
!>[S]   tetap       :<PM_REEL>                vitesse en tangage [rad/s]
!>[S]   phip        :<PM_REEL>                vitesse en roulis [rad/s]
!>[S]   datedeb_js  :<tm_jour_sec>            Date de début de la loi (jour/secondes)
!>[S]   datefin_js  :<tm_jour_sec>            Date de fin de la loi   (jour/secondes)
!>[S]   origdat     :<integer>                origine des dates (0=J1950,1=J2000)
!>[S]   typangle    :<integer>                Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
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
!  ATTITUDE SPIN CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_ATTITUDE_SPINNEE), intent(IN) :: loi_spin

    integer, intent(OUT), optional :: typdat
    real(KIND=PM_REEL), intent(OUT), optional :: datedeb
    real(KIND=PM_REEL), intent(OUT), optional :: datefin
    integer, intent(OUT), optional            :: typrep
    real(KIND=PM_REEL), intent(OUT), optional :: psi0, teta0, phi0
    real(KIND=PM_REEL), intent(OUT), optional :: psip, tetap, phip
    type(tm_jour_sec) , intent(OUT), optional :: datedeb_js, datefin_js
    integer, intent(OUT), optional            :: origdat
    integer, intent(OUT), optional            :: typangle

    ! variables locales
    type(tm_code_retour) :: code_retour

    ! corps de la fonction
    if (PRESENT(datedeb)) then
       if(loi_spin%typdat == 1.or.loi_spin%typdat == 3) then
          call md_joursec_jourfrac(loi_spin%datdeb, datedeb, code_retour)
       else
          datedeb = loi_spin%datdebrel
       endif
    endif
    if (PRESENT(datefin)) then
       if(loi_spin%typdat == 1) then
          call md_joursec_jourfrac(loi_spin%datfin, datefin, code_retour)
       else
          datefin = loi_spin%datfinrel
       endif
    endif

    if (PRESENT(datedeb_js))  datedeb_js  = loi_spin%datdeb
    if (PRESENT(datefin_js))  datefin_js  = loi_spin%datfin
    if (PRESENT(typrep))  typrep  = loi_spin%typrep
    if (PRESENT(typdat))  typdat  = loi_spin%typdat
    if (PRESENT(origdat)) origdat = loi_spin%origdat
    if (PRESENT(psi0))    psi0    = loi_spin%psi0
    if (PRESENT(teta0))   teta0   = loi_spin%teta0
    if (PRESENT(phi0))    phi0    = loi_spin%phi0
    if (PRESENT(psip))    psip    = loi_spin%psip
    if (PRESENT(tetap))   tetap   = loi_spin%tetap
    if (PRESENT(phip))    phip    = loi_spin%phip
    if (PRESENT(typangle)) typangle = loi_spin%typangle

  end SUBROUTINE MSP_consulter_attitude_spinnee
    

  SUBROUTINE MSP_modifier_attitude_spinnee(loi_spin, typdat, datedeb, datefin, &
       typrep, psi0, teta0, phi0, psip, tetap, phip, datedeb_js, datefin_js,origdat,typangle)

 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_attitude_spinnee
!
!$Resume
!  Routine de modification du contenu d'une loi d'attitude spinnee
!
!$Description
!  Routine de modification du contenu d'une loi d'attitude spinnee
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_attitude_spinnee(loi_spin, [typdat], [datedeb], [datefin], &
!.           [typrep], [psi0], [teta0], [phi0], [psip], [tetap], [phip], [datedeb_js], [datefin_js],[origdat],[typangle])
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: typdat
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0, teta0, phi0
!.    real(KIND=pm_reel) :: psip, tetap, phip
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!.    integer :: origdat
!.    integer :: typangle
!
!$Arguments
!>E/S   loi_spin    :<MSP_ATTITUDE_SPINNEE>   Loi d'attitude spinnée à modifier
!>[E]   typdat      :<integer>                type de date utilisé
!>[E]   datedeb     :<PM_REEL>                Date de début de la loi
!>[E]   datefin     :<PM_REEL>                Date de fin de la loi
!>[E]   typrep      :<integer>                type de repère:
!.                                               MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                               MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                               MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                               MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                               MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                               MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                               MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                               MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                               MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[E]   psi0        :<pm_reel>                lacet initial [rad]
!>[E]   teta0       :<pm_reel>                tangage initial [rad]
!>[E]   phi0        :<pm_reel>                roulis initial [rad]
!>[E]   psip        :<pm_reel>                vitesse en lacet [rad/s]
!>[E]   tetap       :<pm_reel>                vitesse en tangage [rad/s]
!>[E]   phip        :<pm_reel>                vitesse en roulis [rad/s]
!>[E]   datedeb_js  :<tm_jour_sec>            Date de début de la loi (jour/secondes)
!>[E]   datefin_js  :<tm_jour_sec>            Date de fin de la loi   (jour/secondes)
!>[E]   origdat     :<integer>                origine des dates (0=J1950,1=J2000)
!>[E]   typangle    :<integer>                Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
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
!  ATTITUDE SPIN MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_ATTITUDE_SPINNEE), intent(INOUT) :: loi_spin

    integer, intent(IN), optional :: typdat
    real(KIND=PM_REEL), intent(IN), optional :: datedeb
    real(KIND=PM_REEL), intent(IN), optional :: datefin
    integer, intent(IN), optional            :: typrep
    real(KIND=pm_reel), intent(IN), optional :: psi0, teta0, phi0
    real(KIND=pm_reel), intent(IN), optional :: psip, tetap, phip
    type(tm_jour_sec) , intent(IN), optional :: datedeb_js, datefin_js
    integer, intent(IN), optional            :: origdat
    integer, intent(in), optional            :: typangle
    
    ! variables locales
    type(tm_code_retour) :: code_retour 

    ! corps de la fonction
    if (PRESENT(datedeb)) then
       if(loi_spin%typdat == 1.or.loi_spin%typdat == 3) then
          call md_jourfrac_joursec(datedeb, loi_spin%datdeb, code_retour)
       else
          loi_spin%datdebrel = datedeb
       endif
    endif

    if (PRESENT(datefin)) then
       if(loi_spin%typdat == 1) then
          call md_jourfrac_joursec(datefin, loi_spin%datfin, code_retour)
       else
          loi_spin%datfinrel = datefin
       endif
    endif

    if (PRESENT(datedeb_js)) loi_spin%datdeb = datedeb_js
    if (PRESENT(datefin_js)) loi_spin%datfin = datefin_js
    if (PRESENT(typrep))  loi_spin%typrep  = typrep
    if (PRESENT(typdat))  loi_spin%typdat  = typdat
    if (PRESENT(origdat)) loi_spin%origdat = origdat
    if (PRESENT(psi0))    loi_spin%psi0    = psi0
    if (PRESENT(teta0))   loi_spin%teta0   = teta0
    if (PRESENT(phi0))    loi_spin%phi0    = phi0
    if (PRESENT(psip))    loi_spin%psip    = psip
    if (PRESENT(tetap))   loi_spin%tetap   = tetap
    if (PRESENT(phip))    loi_spin%phip    = phip
    if (PRESENT(typangle)) loi_spin%typangle = typangle
    
  end SUBROUTINE MSP_modifier_attitude_spinnee
    
  subroutine MSP_afficher_attitude_spinnee(loi_spin, num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_attitude_spinnee
!
!$Resume
!  Cette routine permet d'afficher dans un unité logique le contenu d'une loi spinnee
!
!$Description
!  Cette routine permet d'afficher dans un unité logique le contenu d'une loi spinnee
!
!$Auteur
!  J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_attitude_spinnee(loi_spin, num)
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_spin
!.    integer :: num
!
!$Arguments
!>E     loi_spin  :<MSP_ATTITUDE_SPINNEE>   
!>E     num       :<integer>                
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
!  ATTITUDE SPIN AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_ATTITUDE_SPINNEE), intent(IN) :: loi_spin
    integer, intent(IN) :: num

    ! variables locales
    character(len=8) :: oo

    write(num,'(a,i9)') "TYPDAT:  ",loi_spin%typdat
    oo="MJD1950"
    if (loi_spin%origdat == 1) oo="MJD2000"

    if (loi_spin%typdat /= 2) then
       write(num,'(a,a,i9,a,g21.12)') "DATDEB:  ", oo, loi_spin%datdeb%jour, "-", loi_spin%datdeb%sec
    else
       write(num,'(a,g21.12)') "DATDEB:  ", loi_spin%datdebrel
    endif

    if(loi_spin%typdat == 1) then
       write(num,'(a,a,i9,a,g21.12)') "DATFIN:  ", oo, loi_spin%datfin%jour, "-", loi_spin%datfin%sec
    else
       write(num,'(a,g21.12)') "DATFIN:  ", loi_spin%datfinrel
    endif

    write(num,'(a,i9)') "TYPREP:  ",loi_spin%typrep
    write(num,'(a,i9)') "Convention d'angles (code MSLIB) : ",loi_spin%typangle
    write(num,'(a,g21.12)') "PSI0:    ",loi_spin%psi0
    write(num,'(a,g21.12)') "TETA0:   ",loi_spin%teta0
    write(num,'(a,g21.12)') "PHI0:    ",loi_spin%phi0
    write(num,'(a,g21.12)') "PSIP:    ",loi_spin%psip
    write(num,'(a,g21.12)') "TETAP:   ",loi_spin%tetap
    write(num,'(a,g21.12)') "PHIP:    ",loi_spin%phip

  end subroutine MSP_afficher_attitude_spinnee

end module  MSP_ATTITUDE_SPIN_DEF

