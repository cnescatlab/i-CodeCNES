module MSP_ATM_EXP_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_EXP_DEF
!
!$Resume
!	Module permettant de définir une atmosphère exponentielle
!
!$Description
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_EXP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATM_EXP_DEF.F90,v $
!  Revision 1.12  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.11  2009/08/27 12:26:24  cml
!  DM-ID 1120 : Ajout de valeurs par défaut pour la structure
!
!  Revision 1.10  2009/08/24 16:03:59  cml
!  DM-ID 1120 : Prise en compte des altitudes min et max pour le modèle
!
!  Revision 1.9  2009/07/09 12:20:36  mercadig
!  AQ: Description des variables beta et tscale dans les cartouches
!  Revision 1.8  2008/11/18 19:06:44  mercadig
!  DM-ID 733 : Ajout des variables beta et tscale
!  Revision 1.7  2008/08/08 14:00:50  gss
!  DM-ID 1058 : (portage g95) suppression de la variable non utilisée alsat et
!  de son calcul.
!  Revision 1.6  2006/11/15 10:09:37  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.5  2006/11/09 09:13:57  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.4  2005/03/08 07:32:33  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.3  2005/03/03 15:05:19  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.2  2005/03/03 09:13:22  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.1  2005/01/21 09:57:26  rostan
!  dm-
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:53:31  simbad
!  Ajout du calcul des atmospheres
!  Revision 1.2  2004/01/22 18:27:24  simbad
!  Avant suppression de AMLIB et IOLIB
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_EXP_DEF
!
!$Structure
!
!: MSP_ATM_EXP : 
!>     hscale   : <pm_reel>  facteur d'échelle
!>     h0       : <pm_reel>  altitude de référence
!>     ro0      : <pm_reel>  densité de référence
!>     req      : <pm_reel>  rayon équatorial pour calculer l'altitude géodésique
!>     rpole    : <pm_reel>  rayon polaire
!>     beta     : <pm_reel>  inverse de la hauteur d'échelle
!>     tscale   : <integer>  type de hauteur d'échelle (=1 si hscale, =2 si beta)
!>     altmin   : <pm_reel>  altitude minimum de validite du modèle (m)
!>     altmax   : <pm_reel>  altitude maximum de validité du modèle (m)
!
!$Global
!
!$Common
!
!$Routines
!- MSP_effacer_atm_exp
!- MSP_modifier_atm_exp
!- MSP_cal_atm_exp
!#V
!- egaler_atm_exp
!#
!
!$Fonctions
!- MSP_creer_atm_exp
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- cps_atm_exp
!#
!
!$Interface
!> assignment :  egaler_atm_exp
!#V
!#
!
!$Remarques
!
!$Mots-cles
!   EXPONENTIELLE
!
!$Voir-Aussi
!#V
!.  egaler_atm_exp
!#
!.  MSP_creer_atm_exp MSP_effacer_atm_exp MSP_modifier_atm_exp MSP_cal_atm_exp
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB, only : PM_REEL, tm_geodesique,mt_car_geod
  use MSP_GESTION_ERREUR

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_EXP_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_ATM_EXP
     ! Valeurs par défaut
     real(kind=pm_reel) :: hscale = 0.0_pm_reel
     real(kind=pm_reel) :: h0     = 0.0_pm_reel
     real(kind=pm_reel) :: ro0    = 0.0_pm_reel
     real(kind=pm_reel) :: req    = 0.0_pm_reel
     real(kind=pm_reel) :: rpole  = 0.0_pm_reel
     real(kind=pm_reel) :: beta   = 0.0_pm_reel
     integer            :: tscale = 1
     real(kind=pm_reel) :: altmin = 0.0_pm_reel
     real(kind=pm_reel) :: altmax = 1000000.0_pm_reel
  end type MSP_ATM_EXP


  interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère exponentielle
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère exponentielle
!
!$Acces
!  PUBLIC
!
!$Usage
!  atma=atmb
!.    type(MSP_ATM_EXP) :: atma
!.    type(MSP_ATM_EXP) :: atmb
!
!$Procedures
!#V
!- egaler_atm_exp
!#
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHERE EXPONENTIELLE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_atm_exp
  end interface

  private :: egaler_atm_exp

CONTAINS

  subroutine egaler_atm_exp(atma,atmb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_atm_exp
!
!$Resume
!  Cette routine permet d'égaler deux structures atmosphère exponentielle
!
!$Description
!  Cette routine permet d'égaler deux structures atmosphère exponentielle
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_atm_exp(atma,atmb)
!.    type(MSP_ATM_EXP) :: atma
!.    type(MSP_ATM_EXP) :: atmb
!
!$Arguments
!>E/S   atma  :<MSP_ATM_EXP>   
!>E     atmb  :<MSP_ATM_EXP>   
!
!$Common
!
!$Routines
!- MSP_effacer_atm_exp
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHERE EXPONENTIELLE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_ATM_EXP),intent(inout)::atma
    type(MSP_ATM_EXP),intent(in)::atmb
    
    call MSP_effacer_atm_exp(atma)

    atma%hscale = atmb%hscale
    atma%h0     = atmb%h0
    atma%ro0    = atmb%ro0
    atma%req    = atmb%req
    atma%rpole  = atmb%rpole
    atma%beta   = atmb%beta
    atma%tscale = atmb%tscale
    atma%altmin = atmb%altmin
    atma%altmax = atmb%altmax
    
  end subroutine egaler_atm_exp

  subroutine MSP_effacer_atm_exp(atma)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_exp
!
!$Resume
!	Routine d'initialisation d'une structure atmosphere exponentielle
!
!$Description
!	Routine d'initialisation d'une structure atmosphere exponentielle
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_exp(atma)
!.    type(MSP_ATM_EXP) :: atma
!
!$Arguments
!>E/S   atma  :<MSP_ATM_EXP>   
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
! INITIALISER ATMOSPHERE EXPONENTIELLE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(MSP_ATM_EXP),intent(inout)::atma

    atma%hscale = 0._PM_REEL
    atma%h0     = 0._PM_REEL
    atma%ro0    = 0._PM_REEL
    atma%req    = 0._PM_REEL
    atma%rpole  = 0._PM_REEL
    atma%beta   = 0._PM_REEL
    atma%tscale = 1
    atma%altmin = 0._PM_REEL
    atma%altmax = 1000.0e3_PM_REEL

  end subroutine MSP_effacer_atm_exp




  function MSP_creer_atm_exp(hscale,h0,ro0,req,rpole,invapla,beta,tscale, &
       altmin, altmax) result (atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_exp
!
!$Resume
!	Routine de création d'une structure atmosphere exponentielle
!
!$Description
!	Routine de création d'une structure atmosphere exponentielle
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm = MSP_creer_atm_exp([hscale],[h0],[ro0],[req],[rpole],[invapla],[beta],[tscale], &
!.           [altmin], [altmax])
!.    real(kind=pm_reel) :: hscale,h0,ro0,req,rpole,invapla,beta
!.    real(kind=pm_reel) :: altmin, altmax
!.    integer :: tscale
!.    type(MSP_ATM_EXP) :: atm
!
!$Arguments
!>[E]   hscale   :<pm_reel>       facteur d'échelle
!>[E]   h0       :<pm_reel>       altitude de référence
!>[E]   ro0      :<pm_reel>       densité de référence
!>[E]   req      :<pm_reel>       rayon équatorial pour calculer l'altitude géodésique
!>[E]   rpole    :<pm_reel>       rayon polaire
!>[E]   invapla  :<pm_reel>       inverse de l'aplatissement
!>[E]   beta     :<pm_reel>       inverse de la hauteur d'échelle
!>[E]   tscale   :<integer>       type de hauteur d'échelle (=1 si hscale, =2 si beta)
!>[E]   altmin   :<pm_reel>       altitude minimum de validité du modèle (m)
!>[E]   altmax   :<pm_reel>       altitude maximum de validité du modèle (m)
!>S     atm      :<MSP_ATM_EXP>   structure exponentielle
!
!$Common
!
!$Routines
!- MSP_effacer_atm_EXP
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CREER ATMOSPHERE EXPONENTIELLE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

     real(kind=pm_reel),intent(in),optional::hscale,h0,ro0,req,rpole,invapla,beta
     real(kind=pm_reel),intent(in),optional::altmin, altmax
     integer,intent(in),optional::tscale

     type(MSP_ATM_EXP) :: atm

     call MSP_effacer_atm_EXP(atm)
     if (MSP_gen_messages("MSP_creer_atm_exp" )) return

 
     if (present(hscale)) then
        atm%hscale = hscale
     end if

     if (present(h0)) then
        atm%h0 = h0
     end if

     if (present(ro0)) then
        atm%ro0 = ro0
     end if

     if (present(req)) then
        atm%req = req
     end if


     if (present(rpole)) then
        atm%rpole = rpole
     end if
     
     if (present(beta)) then
        atm%beta = beta
     end if
     
     if (present(tscale)) then
        atm%tscale = tscale
     end if

     if (present(altmin)) then
        atm%altmin = altmin
     end if

     if (present(altmax)) then
        atm%altmax = altmax
     end if

     if (present(invapla).and.(present(req))) then

        if (invapla*req /= 0._pm_reel) then
           atm%rpole = atm%req*(1._PM_REEL-1._PM_REEL/invapla)
        else
           call MSP_signaler_message (cle_mes="MSP_DIV_ZERO", Routine="MSP_CREER_ATM_EXP")
           return
        end if
     end if

   end function MSP_CREER_ATM_EXP

  subroutine  MSP_modifier_atm_exp(atm,hscale,h0,ro0,req,rpole,invapla,beta,tscale, altmin, altmax) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_exp
!
!$Resume
!	Routine permettant de modifier une structure atmosphere exponentielle
!
!$Description
!	Routine permettant de modifier une structure atmosphere exponentielle
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_exp(atm,[hscale],[h0],[ro0],[req],[rpole],[invapla],[beta],[tscale], [altmin], [altmax]) 
!.    type(MSP_ATM_EXP) :: atm
!.    real(kind=pm_reel) :: hscale,h0,ro0,req,rpole,invapla,beta
!.    real(kind=pm_reel) :: altmin, altmax
!.    integer :: tscale
!
!$Arguments
!>E/S   atm      :<MSP_ATM_EXP>   structure exponentielle
!>[E]   hscale   :<pm_reel>       facteur d'échelle
!>[E]   h0       :<pm_reel>       altitude de référence
!>[E]   ro0      :<pm_reel>       densité de référence
!>[E]   req      :<pm_reel>       rayon équatorial pour calculer l'altitude géodésique
!>[E]   rpole    :<pm_reel>       rayon polaire
!>[E]   invapla  :<pm_reel>       inverse de l'applatissement
!>[E]   beta     :<pm_reel>       inverse de la hauteur d'échelle
!>[E]   tscale   :<integer>       type de hauteur d'échelle (=1 si hscale, =2 si beta)
!>[E]   altmin   :<pm_reel>       altitude minimum de validité du modèle (m)
!>[E]   altmax   :<pm_reel>       altitude maximum de validité du modèle (m)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! MODIFIER ATMOSPHERE EXPONENTIELLE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_ATM_EXP) :: atm
    real(kind=pm_reel),intent(in),optional::hscale,h0,ro0,req,rpole,invapla,beta
    real(kind=pm_reel),intent(in),optional::altmin, altmax
    integer,intent(in),optional::tscale
 
     if (present(hscale)) then
        atm%hscale = hscale
     end if

     if (present(h0)) then
        atm%h0 = h0
     end if

     if (present(ro0)) then
        atm%ro0 = ro0
     end if

     if (present(req)) then
        atm%req = req
     end if


     if (present(rpole)) then
        atm%rpole = rpole
     end if
     
     if (present(beta)) then
        atm%beta = beta
     end if
     
     if (present(tscale)) then
        atm%tscale = tscale
     end if

     if (present(altmin)) then
        atm%altmin = altmin
     end if

     if (present(altmax)) then
        atm%altmax = altmax
     end if

     if (present(invapla).and.(present(req))) then
        if (invapla*req /= 0._pm_reel) then
           atm%rpole = atm%req*(1._PM_REEL-1._PM_REEL/invapla)
        else
           call MSP_signaler_message (cle_mes="MSP_DIV_ZERO", Routine="MSP_modifier_atm_exp")
           return   
        end if
     end if

   end subroutine  MSP_modifier_atm_exp
   

   subroutine MSP_cal_atm_exp(atm,R,phisat,ro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_cal_atm_exp
!
!$Resume
!	Routine permettant de calculer la densité 
!
!$Description
!	Routine permettant de calculer la densité
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_cal_atm_exp(atm,R,phisat,ro)
!.    type(MSP_ATM_EXP) :: atm
!.    real(kind=PM_REEL) :: R,phisat
!.    real(kind=PM_REEL) :: ro
!
!$Arguments
!>E     atm     :<MSP_ATM_EXP>   structure atmosphère exponentielle
!>E     R       :<PM_REEL>       distance sonde-centre planète
!>E     phisat  :<PM_REEL>       latitude
!>S     ro      :<PM_REEL>       densité
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- mt_car_geod
!- cps_atm_exponentiel
!
!$Include
!
!$Module
!#V
!- cps_atm_exp
!#
!
!$Remarques
!
!$Mots-cles
!  DENSITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use cps_atm_exp

     implicit none

     type(MSP_ATM_EXP),intent(in)::atm
     real(kind=PM_REEL),intent(in)::R,phisat
     real(kind=PM_REEL),intent(out)::ro

     ! Variables locales
     real(kind=pm_reel)::coord_cart(3),apla
     type(tm_geodesique) :: coord_geod
     type(tm_code_retour) :: code_retour

     coord_cart(1) = R*cos(phisat)
     coord_cart(2) = 0.0_PM_REEL
     coord_cart(3) = R*sin(phisat)

     if (atm%req /= 0._pm_reel) then
     apla = 1.0_PM_REEL - atm%rpole/atm%req
     else
        call MSP_signaler_message (cle_mes="MSP_DIV_ZERO", Routine="MSP_cal_atm_exp")
        return
     end if

     call mt_car_geod(coord_cart,atm%req,apla,coord_geod,code_retour)
     call MSP_signaler_message (ier_mslib=code_retour)
     if ( MSP_ERREUR ) return
     if ( coord_geod%haut <= 0._pm_reel ) then
        call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_001")
        return
     endif

     if ( atm%tscale == 1 ) then
        if (atm%hscale /= 0._pm_reel) then
             call cps_atm_exponentiel (coord_geod%haut,atm%ro0,atm%h0,atm%hscale,&
                  atm%altmin,atm%altmax,ro)   
        else
             call MSP_signaler_message (cle_mes="MSP_DIV_ZERO", Routine="MSP_cal_atm_exp")
             return  
        endif
     else
        if (atm%beta /= 0._pm_reel) then
             call cps_atm_exponentiel (coord_geod%haut,atm%ro0,atm%h0,1/atm%beta,&
                  atm%altmin,atm%altmax,ro)
        else
           call MSP_signaler_message (cle_mes="MSP_DIV_ZERO", Routine="MSP_cal_atm_exp")
           return
        endif
     end if
   end subroutine MSP_cal_atm_exp

 end module MSP_ATM_EXP_DEF
