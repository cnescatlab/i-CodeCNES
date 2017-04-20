module MSP_ATM_MARSRUSSE_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_MARSRUSSE_DEF
!
!$Resume
!	Routine permettant d'utiliser le modèle russe (atmars90) d'atmosphère martienne
!
!$Description
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_MARSRUSSE_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_ATM_MARSRUSSE_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.6  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.5  2008/11/19 13:30:17  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.4  2008/02/22 13:55:47  huec
!  FA-ID 968 : Utilisation de parentheses pour des calculs non triviaux
!  Revision 1.3  2005/03/08 07:32:33  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.2  2005/03/01 16:22:35  fabrec
!  DM-ID 111 : modeles d'atmosphere
!  Revision 1.1  2005/01/21 09:57:25  rostan
!  dm-
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:53:32  simbad
!  Ajout du calcul des atmospheres
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_MARSRUSSE_DEF
!
!$Structure
!
!: MSP_ATM_MARSRUSSE : 
!>     mars_russe_mod   : <integer>  type d'atmosphère (moyen=1, chaud=2 froid=3)
!>     coefro           : <PM_REEL>  coefficient mutiplicatif de la densité
!
!$Global
!
!$Common
!
!$Routines
!- MSP_effacer_atm_marsrusse
!- MSP_consulter_atm_marsrusse
!- MSP_modifier_atm_marsrusse
!- MSP_calculer_atm_marsrusse
!#V
!- egaler_atm_marsrusse
!#
!
!$Fonctions
!- MSP_creer_atm_marsrusse
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
!> assignment :  egaler_atm_marsrusse
!#V
!#
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!#V
!.  egaler_atm_marsrusse
!#
!.  MSP_creer_atm_marsrusse MSP_effacer_atm_marsrusse MSP_consulter_atm_marsrusse MSP_modifier_atm_marsrusse
!.  MSP_calculer_atm_marsrusse
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB, only : PM_REEL
  use MSP_GESTION_ERREUR
  use CPS_ATMOSPHERE
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_MARSRUSSE_DEF.F90 365 2013-02-18 12:36:19Z aadt $'



  type MSP_ATM_MARSRUSSE
     integer             :: mars_russe_mod
     real(kind=PM_REEL)  :: coefro
  end type MSP_ATM_MARSRUSSE

  interface assignment (=) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère mars russe
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère mars russe
!
!$Acces
!  PUBLIC
!
!$Usage
!  atma=atmb
!.    type(MSP_ATM_MARSRUSSE) :: atma
!.    type(MSP_ATM_MARSRUSSE) :: atmb
!
!$Procedures
!#V
!- egaler_atm_marsrusse
!#
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_atm_marsrusse
  end interface

  private :: egaler_atm_marsrusse

CONTAINS
  

  subroutine MSP_effacer_atm_marsrusse(atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_marsrusse
!
!$Resume
!	Routine d'initialisation d'une structure modèle d'atmosphère russe
!
!$Description
!	Routine d'initialisation d'une structure modèle d'atmosphère russe
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_marsrusse(atm)
!.    type(MSP_ATM_MARSRUSSE) :: atm
!
!$Arguments
!>E/S   atm  :<MSP_ATM_MARSRUSSE>   structure modèle d'atmosphère russe
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
! INITIALISER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_MARSRUSSE) :: atm
    atm%mars_russe_mod = 0
    atm%coefro         = 0._pm_reel
  end subroutine MSP_effacer_atm_marsrusse

  subroutine egaler_atm_marsrusse(atma,atmb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_atm_marsrusse
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère mars russe
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère mars russe
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_atm_marsrusse(atma,atmb)
!.    type(MSP_ATM_MARSRUSSE) :: atma
!.    type(MSP_ATM_MARSRUSSE) :: atmb
!
!$Arguments
!>E/S   atma  :<MSP_ATM_MARSRUSSE>   
!>E     atmb  :<MSP_ATM_MARSRUSSE>   
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
! EGALER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_MARSRUSSE),intent(inout) :: atma
    type(MSP_ATM_MARSRUSSE),intent(in) :: atmb

    atma%mars_russe_mod = atmb%mars_russe_mod
    atma%coefro         = atmb%coefro
  end subroutine egaler_atm_marsrusse

  function MSP_creer_atm_marsrusse(mars_russe_mod,coefro) result (atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_marsrusse
!
!$Resume
!	Routine permettant la création d'une structure atmosphère russe
!
!$Description
!	Routine permettant la création d'une structure atmosphère russe
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm = MSP_creer_atm_marsrusse([mars_russe_mod],[coefro])
!.    integer :: mars_russe_mod
!.    real(kind=PM_REEL) :: coefro
!.    type(MSP_ATM_MARSRUSSE) :: atm
!
!$Arguments
!>[E]   mars_russe_mod  :<integer>             type d'atmosphère (moyen=1, chaud=2 froid=3)
!>[E]   coefro          :<PM_REEL>             coefficient mutiplicatif de la densité
!>S     atm             :<MSP_ATM_MARSRUSSE>   structure atmosphère
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
! CREER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer,intent(in),optional              :: mars_russe_mod
    real(kind=PM_REEL),intent(in),optional   :: coefro
    type(MSP_ATM_MARSRUSSE)                  :: atm

    if ( present(mars_russe_mod) ) then
       atm%mars_russe_mod = mars_russe_mod
    else
       atm%mars_russe_mod = 1
    endif

    if ( present(coefro)) then
       atm%coefro = coefro
    else
       atm%coefro = 1._PM_REEL
    end if


  end function MSP_creer_atm_marsrusse

  subroutine MSP_consulter_atm_marsrusse(atm,mars_russe_mod,coefro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_atm_marsrusse
!
!$Resume
!	Routine de consultation d'une structure modèle d'atmosphère Russe
!
!$Description
!	Routine de consultation d'une structure modèle d'atmosphère Russe
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_atm_marsrusse(atm,[mars_russe_mod],[coefro])
!.    type(MSP_ATM_MARSRUSSE) :: atm
!.    integer :: mars_russe_mod
!.    real(kind=pm_reel) :: coefro
!
!$Arguments
!>E     atm             :<MSP_ATM_MARSRUSSE>   structure atmosphère
!>[S]   mars_russe_mod  :<integer>             type d'atmosphère (moyen=1, chaud=2 froid=3)
!>[S]   coefro          :<pm_reel>             coefficient mutiplicatif de la densité
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
! CONSULTER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_MARSRUSSE),intent(in)      :: atm
    integer,intent(out),optional            :: mars_russe_mod
    real(kind=pm_reel),intent(out),optional :: coefro

    if ( present(mars_russe_mod) ) mars_russe_mod = atm%mars_russe_mod
    if ( present(coefro) )         coefro         = atm%coefro
  end subroutine MSP_consulter_atm_marsrusse


  subroutine MSP_modifier_atm_marsrusse(atm,mars_russe_mod,coefro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_marsrusse
!
!$Resume
!	Routine permettant de modifier une modèle d'atmosphère russe
!
!$Description
!	Routine permettant de modifier une modèle d'atmosphère russe
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_marsrusse(atm,[mars_russe_mod],[coefro])
!.    type(MSP_ATM_MARSRUSSE) :: atm
!.    integer :: mars_russe_mod
!.    real(kind=pm_reel) :: coefro
!
!$Arguments
!>S     atm             :<MSP_ATM_MARSRUSSE>   structure atmosphère
!>[E]   mars_russe_mod  :<integer>             type d'atmosphère (moyen=1, chaud=2 froid=3)
!>[E]   coefro          :<pm_reel>             coefficient mutiplicatif de la densité
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
! MODIFIER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_MARSRUSSE),intent(out)      :: atm
    integer,intent(in),optional            :: mars_russe_mod
    real(kind=pm_reel),intent(in),optional :: coefro

    if ( present(mars_russe_mod) )  atm%mars_russe_mod = mars_russe_mod
    if ( present(coefro) )          atm%coefro         = coefro
  end subroutine MSP_modifier_atm_marsrusse


  subroutine MSP_calculer_atm_marsrusse(atm,R,phisat,ro,vson,pression,temperature)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_marsrusse
!
!$Resume
!	Routine permettant de calculer les caractéristisques du modèle d'atmosphère russe atmmars90
!
!$Description
!	Routine permettant de calculer les caractéristisques du modèle d'atmosphère russe atmmars90
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_marsrusse(atm,R,phisat,ro,vson,pression,temperature)
!.    type (MSP_ATM_MARSRUSSE) :: atm 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!.    real (KIND=PM_REEL) :: pression 
!.    real (KIND=PM_REEL) :: temperature 
!
!$Arguments
!>E     atm          :<MSP_ATM_MARSRUSSE>   structure atmosphère
!>E     R            :<PM_REEL>             Distance centre planete - sonde (m)
!>E     phisat       :<PM_REEL>             Latitude sphérique (rad)
!>S     ro           :<PM_REEL>             Densite atmospherique (kg/m^3)
!>S     vson         :<PM_REEL>             Vitesse du son (m/s)
!>S     pression     :<PM_REEL>             Pression atmospherique (Pa)
!>S     temperature  :<PM_REEL>             Temperature atmospherique (K)
!
!$Common
!
!$Routines
!- cps_atmars90
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CALCULER ATMOSPHERE MARS RUSSE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    ! Entrees
    type (MSP_ATM_MARSRUSSE), intent(IN):: atm        ! structure atmosphère
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT) :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT) :: pression      ! Pression atmospherique
    real (KIND=PM_REEL), intent(OUT) :: temperature   ! Temperature atmospherique

    ! Définition des constantes utilisées par le modèle Martienne
    real(kind=PM_REEL),parameter:: Req_Mars= 3393.4e+03_pm_reel
    real(kind=PM_REEL),parameter::Rpole_Mars = 3375.8e+03_pm_reel

    real(kind=PM_REEL)::alsat
    integer::ier
    real (KIND=PM_REEL) :: visco

    ! Altitude difference des rayons sur l'ellipsoide
!
!                            req_Mars
!alsat = R - __________________________________________
!                  ___________________________________
!            \    /                        req_Mars²
!             \  / cos²(phi) + sin²(phi)-------------
!              \/                        rpole_Mars²
!             

    alsat = R - (req_Mars/(sqrt(cos(phisat)*cos(phisat) + &
         sin(phisat)*sin(phisat)*(req_Mars*req_Mars)/(rpole_Mars*rpole_Mars))))
    ! Calcul de la temperature, de la pression, de ro, de vson

!
!           portage de IO_e_atmars90 (iolib) a atmars90


    call cps_atmars90  (atm%mars_russe_mod,atm%coefro,alsat,temperature,pression,ro,vson,visco,ier)
    if ( ier /= 0 ) then
       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_006",routine="MSP_calculer_atm_marsrusse")
       return
    endif

  end subroutine MSP_calculer_atm_marsrusse
end module MSP_ATM_MARSRUSSE_DEF
