module MSP_ATM_VENUS_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_VENUS_DEF
!
!$Resume
!  module permettant d'utiliser le modèle PETROPOULOS88
!
!$Description
!  module permettant d'utiliser le modèle PETROPOULOS88
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Version
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATM_VENUS_DEF.F90,v $
!  Revision 1.4  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.3  2008/11/19 13:30:32  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.2  2007/11/23 08:37:55  jpi
!  DM-ID 744 : tests modele venus petropoulos88
!  Revision 1.1  2007/10/16 12:58:44  jpi
!  DM-ID744:modele Venus petropoulos88
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_VENUS_DEF
!
!$Structure
!
!: MSP_ATM_VENUS : 
!>     rho0   : <pm_reel>  
!>     beta   : <pm_reel>  
!
!$Global
!
!$Common
!
!$Routines
!- MSP_egaler_atm_venus
!- MSP_effacer_atm_venus
!- MSP_modifier_atm_venus
!- MSP_consulter_atm_venus
!- MSP_cal_atm_venus
!
!$Fonctions
!- MSP_creer_atm_venus
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- CPS_ATMOSPHERE
!#
!
!$Interface
!> assignment :  MSP_egaler_atm_venus
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  MSP_creer_atm_venus MSP_egaler_atm_venus MSP_effacer_atm_venus MSP_modifier_atm_venus
!.  MSP_consulter_atm_venus MSP_cal_atm_venus
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB, only : PM_REEL
  use MSP_GESTION_ERREUR
  use CPS_ATMOSPHERE, only : cps_atm_venus

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_ATM_VENUS
     real(kind=pm_reel) :: rho0
     real(kind=pm_reel) :: beta
  end type MSP_ATM_VENUS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface assignment (=)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère
!
!$Acces
!  PUBLIC
!
!$Usage
!  atma=atmb
!.    type(MSP_ATM_VENUS) :: atma
!.    type(MSP_ATM_VENUS) :: atmb
!
!$Procedures
!- MSP_egaler_atm_venus
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHERE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     module procedure egaler_atm_venus
     module procedure MSP_egaler_atm_venus
  end interface

!  private :: MSP_egaler_atm_venus

CONTAINS

  subroutine MSP_egaler_atm_venus(atma,atmb)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_atm_venus
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère 
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère 
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_egaler_atm_venus(atma,atmb)
!.    type(MSP_ATM_VENUS) :: atma
!.    type(MSP_ATM_VENUS) :: atmb
!
!$Arguments
!>S     atma  :<MSP_ATM_VENUS>   
!>E     atmb  :<MSP_ATM_VENUS>   
!
!$Common
!
!$Routines
!- MSP_effacer_atm_venus
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!       EGALER ATMOSPHERE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    type(MSP_ATM_VENUS),intent(out)::atma
    type(MSP_ATM_VENUS),intent(in)::atmb

    call MSP_effacer_atm_venus(atma)

    atma%rho0  = atmb%rho0
    atma%beta  = atmb%beta

  end subroutine MSP_egaler_atm_venus

  subroutine MSP_effacer_atm_venus(atma)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_venus
!
!$Resume
!  Routine d'initialisation de la structure VENUS
!
!$Description
!  Routine d'initialisation de la structure VENUS
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_venus(atma)
!.    type(MSP_ATM_VENUS) :: atma
!
!$Arguments
!>E/S   atma  :<MSP_ATM_VENUS>   
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
!       EFFACER ATMOSPHERE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_VENUS),intent(inout)::atma

    atma%rho0  = 0._PM_REEL
    atma%beta  = 0._PM_REEL

  end subroutine MSP_effacer_atm_venus

  function MSP_creer_atm_venus(rho0,beta) result (atm)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_venus
!
!$Resume
!  Routine de création de la structure VENUS
!
!$Description
!  Routine de création de la structure VENUS
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm = MSP_creer_atm_venus([rho0],[beta])
!.    type(MSP_ATM_VENUS) :: atm
!.    real(kind=pm_reel) :: rho0, beta
!
!$Arguments
!>[E]   rho0  :<pm_reel>         
!>[E]   beta  :<pm_reel>         
!>S     atm   :<MSP_ATM_VENUS>   
!
!$Common
!
!$Routines
!- MSP_effacer_atm_venus
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!       CREER ATMOSPHERE VENUS       
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_VENUS) :: atm
    real(kind=pm_reel),intent(in),optional::rho0, beta

    call MSP_effacer_atm_venus(atm)

    if (MSP_gen_messages("MSP_creer_atm_venus" )) return

    if (present(rho0)) atm%rho0 = rho0
    if (present(beta)) atm%beta = beta

  end function MSP_CREER_ATM_VENUS

  subroutine  MSP_modifier_atm_venus(atm,rho0,beta) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_venus
!
!$Resume
!  Routine permettant de modifier une structure atmosphère VENUS
!
!$Description
!  Routine permettant de modifier une structure atmosphère VENUS
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_venus(atm,[rho0],[beta]) 
!.    type(MSP_ATM_VENUS) :: atm
!.    real(kind=pm_reel) :: rho0, beta
!
!$Arguments
!>S     atm   :<MSP_ATM_VENUS>   
!>[E]   rho0  :<pm_reel>         
!>[E]   beta  :<pm_reel>         
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
!       MODIFIER ATMOSPHÈRE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_VENUS),intent(out) :: atm
    real(kind=pm_reel),intent(in),optional::rho0, beta

    if (present(rho0)) atm%rho0 = rho0
    if (present(beta)) atm%beta = beta

  end subroutine  MSP_modifier_atm_venus

  subroutine MSP_consulter_atm_venus(atm,rho0,beta)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_atm_venus
!
!$Resume
!  Routine de consultation de la structure atmosphère VENUS
!
!$Description
!  Routine de consultation de la structure atmosphère VENUS
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_atm_venus(atm,[rho0],[beta])
!.    type(MSP_ATM_VENUS) :: atm
!.    real(kind=PM_REEL) :: rho0,beta
!
!$Arguments
!>E     atm   :<MSP_ATM_VENUS>   
!>[S]   rho0  :<PM_REEL>         
!>[S]   beta  :<PM_REEL>         
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
!       CONSULTER ATMOSPHÈRE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_VENUS),intent(IN)::atm
    real(kind=PM_REEL),intent(OUT),optional::rho0,beta

    if (present(rho0)) rho0 = atm%rho0
    if (present(beta)) beta = atm%beta

  end subroutine MSP_consulter_atm_venus

  subroutine MSP_cal_atm_venus(rho,altitude,atm)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_cal_atm_venus
!
!$Resume
!  Routine permettant de calculer les caractéristiques du modèle PETROPOULOS88
!
!$Description
!  Routine permettant de calculer les caractéristiques du modèle PETROPOULOS88
!
!$Auteur
!  $Id: MSP_ATM_VENUS_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_cal_atm_venus(rho,altitude,atm)
!.    real(kind=pm_reel) :: rho
!.    real(kind=pm_reel) :: altitude
!.    type(MSP_ATM_VENUS) :: atm
!
!$Arguments
!>S     rho       :<pm_reel>         
!>E     altitude  :<pm_reel>         
!>E     atm       :<MSP_ATM_VENUS>   
!
!$Common
!
!$Routines
!- cps_atm_venus
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!       CALCULER ATMOSPHERE VENUS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    real(kind=pm_reel), intent (OUT) :: rho
    real(kind=pm_reel), intent (IN)  :: altitude
    type(MSP_ATM_VENUS),intent(IN)   :: atm
    !real(KIND=PM_REEL), parameter :: petropoulos88_cste_alt_min = 40.e+03_pm_reel
    !real(KIND=PM_REEL), parameter :: petropoulos88_cste_alt_max = 160.e+03_pm_reel

    if ( altitude >= 0._pm_reel ) then
       call cps_atm_venus (altitude ,rho, atm%rho0, atm%beta)
    else
       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_001", Routine="MSP_ATM_VENUS_DEF")
       return
    endif

  end subroutine MSP_cal_atm_venus

end module MSP_ATM_VENUS_DEF
