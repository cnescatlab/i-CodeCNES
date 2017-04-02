module cps_atm_exp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_atm_exp
!
!$Resume
!  Cédric Martel (Atos Origin)
!
!$Description
!  Module de calcul d'un modèle atmosphérique exponentiel quelconque 
!
!$Auteur
!  Module de calcul d'un modèle atmosphérique exponentiel quelconque 
!
!$Version
!  $Id: cps_atm_exp.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atm_exp.F90,v $
!  Revision 1.5  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.4  2009/11/17 09:21:53  cmartel
!  DM-ID 1120 : La hauteur de reference peut etre nulle
!
!  Revision 1.3  2009/11/17 09:11:06  cmartel
!  DM-ID 1120 : Ajout de tests sur les entrees
!
!  Revision 1.2  2009/08/26 15:11:06  cml
!  AQ : Ajout d'informations dans le cartouche
!
!  Revision 1.1  2009/08/21 09:48:40  cml
!  DM-ID 1120 : Ajout du modele exponentiel multi-planetes
!
!
!$FinHistorique
!
!$Usage
!  use cps_atm_exp
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_atm_exponentiel
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- mslib
!- cps_utilisateur
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
!.  cps_atm_exponentiel
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atm_exp.F90 69 2012-09-11 08:33:34Z ffsm $'

  contains

  subroutine cps_atm_exponentiel (h,ro0,h0,hscale,altmin, altmax, densite, &
       g, ml, gamma, pression, vson, temp)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  cps_atm_exponentiel
!
!$Resume
!     Modèle exponentiel de l'atmosphère martienne (40->100km).
!
!$Description
!     Calcul de la densité rho, pression P, température T et vitesse du
!     son Vsound suivant une loi exponentielle de l'atmosphère
!     (supposant une atmosphère composée de gaz parfaits homogène en température)
!     Ce modèle est bien adapté aux altitudes entre 40 et 100 km (cas Mars).
!     En dessous de 40 km une atmosphère isentropique serait préférable.
!>        rho(h) = ro0 * exp(- (h-h0) / hscale)
!>        T      = hscale * m * g / R = Cte
!>        P(h)   = ro(h) * R * T / m
!>        Vsound = sqrt(gamma * R * T / m) = sqrt(gamma*p(h)/rho(h)) = Cte
!     avec
!.        h      Hauteur aréodesique
!.        h0     Hauteur de référence
!.        ro0    Densité à h0
!.        hscale Constante d'echelle de densité/pression
!.        gamma  Capacité thermique = 1.3 pour le CO2 de l'atmosphère de Mars
!.        R      constante universelle des gaz = 8.3144 J.mol^-1.K^-1
!.        m      Masse moléculaire = 44 g pour le CO2 de l'atmosphère martienne
!.        g      accélération gravitationnelle locale = 3.6 m/s^2
!               (valeur approximative calculée à une hauteur de 50km au dessus
!                de l'aréoide de référence de Mars)
!.   La présence de la temperature, pression ou vitesse du son en sortie impose 
!    la présence de g, gamma et ml en entrée.
!.   Si h est au dessus de altmax, la densité est zéro
!.   Si h est au dessous de altmin, la densité est égale à celle obtenue à altmin
!
!$Auteur
!    Cédric MARTEL (Atos Origin)
!
!$Acces
!    PUBLIC
!
!$Arguments
!>E     h         :<pm_reel>   hauteur aréodesique (m)
!>E     ro0       :<pm_reel>   densité à la hauteur de référence (kg/m^3)
!>E     h0        :<pm_reel>   hauteur de référence (m)
!>E     hscale    :<pm_reel>   constante d'echelle de densité/pression (m)
!>E     altmin    :<pm_reel>   hauteur minimum de validité du modèle (m)
!>E     altmax    :<pm_reel>   hauteur maximum de validité du modèle (m)
!>S     densite   :<pm_reel>   densité (kg/m^3)
!>[E]   g         :<pm_reel>   accélération gravitationnelle locale (m/s^2)
!>[E]   ml        :<pm_reel>   masse moléculaire du gaz pricipal
!>[E]   gamma     :<pm_reel>   capacité thermique
!>[S]   pression  :<pm_reel>   pression (Pa)
!>[S]   vson      :<pm_reel>   vitesse du son (m/s)
!>[S]   temp      :<pm_reel>   température approchée  (K)
!
!$Remarque
!.  Cette routine est issue du module cps_atmosphere_mars.
!  Les constantes à utilisée pour pouvoir reproduire les résultats pour Mars
!  sont g = 3.6, ml = 0.044, et gamma = 1.3
!.  Note : "g" se calcule par  g=mu/((requa+h0)*(requa+h0)).
!
!$<>
!***********************************************************************

    use mslib
    use cps_utilisateur

    implicit none
    
    ! Entrées / sorties
    real(kind=pm_reel), intent (IN) :: h,ro0,h0,hscale,altmin, altmax
    real(kind=pm_reel), intent (OUT) :: densite
    real(kind=pm_reel), intent (IN), optional :: g, ml, gamma
    real(kind=pm_reel), intent (OUT), optional :: pression, vson, temp
    
    ! Constante des gaz parfaits
    real(kind=pm_reel), parameter    :: R =8.3144_pm_reel
    
    ! Variables locales
    logical :: calculer_temp           ! Flag commandant le calcul de la température
    real(kind=pm_reel) :: alt          ! Altitude fonction de h, altmin/max
    real(kind=pm_reel) :: temperature  ! Temperature calculee

    ! Initialisation
    temperature = 0.0_pm_reel
    alt = 0.0_pm_reel
    calculer_temp = .false.

    ! Vérification de la cohérence des paramètres
    if ( present(pression) .or. present(vson) .or. present(temp)) then
       ! Toutes ces variables sont calculées à partir de la température
       calculer_temp = .true.

       if (.not. present(g) .or. .not. present(ml) .or. .not. present(gamma) ) then
           call MSP_signaler_message(cle_mes="CPS_ERR_ATM_EXP_ENTREES",&
               routine="cps_atm_exponentiel")
       endif
    endif

    ! Verification des valeurs
    if ( ro0 < 0 ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_EXP_RO0",&
          routine="cps_atm_exponentiel")
       return
    endif
    if ( h0 < 0 ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_EXP_H0",&
          routine="cps_atm_exponentiel")
       return
    endif
    if ( hscale <= 0 ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ATM_EXP_HSCALE",&
          routine="cps_atm_exponentiel")
       return
    endif
       

    ! Cas particulier ou l'altitude est supérieure à altmax
    ! => on considère que la densité est nulle
    if ( h > altmax ) then
       ! Toutes les valeurs sont mises à zéro
       densite = 0.0_pm_reel
       if (present(temp ))   temp  = 0.0_pm_reel
       if(present(pression)) pression = 0.0_pm_reel
       if (present(vson))    vson = 0.0_pm_reel
       ! Emmission d'un warning
       call MSP_signaler_message(cle_mes="CPS_WARN_ATM_EXP_ALTMAX",&
           routine="cps_atm_exponentiel")
       return
    endif
    
    ! Cas particulier ou l'altitude est inférieure à altmin
    ! => on utilise altmin comme altitude
    if ( h < altmin ) then
       ! L'altitude de calcul est l'atitude minimum
       alt = altmin
       ! Emmission d'un warning
       call MSP_signaler_message(cle_mes="CPS_WARN_ATM_EXP_ALTMIN",&
           routine="cps_atm_exponentiel")
    else
       ! L'altitude pour le calcul est celle en entrée
       alt = h
    endif
    

    ! Calcul de la densité, la température, la pression et la vitesse du son
    densite = ro0*exp((h0-alt)/hscale)
    
    ! Si la température, la pression ou la vitesse du son sont demandés
    if (calculer_temp) then
       temperature = ml*g*hscale/R
    endif

    ! Calcul eventuel de la temperature, pression et/ou vitesse du son
    if (present(temp )) temp  = temperature
    if(present(pression)) pression = densite*R*temperature/ml
    if (present(vson)) vson = sqrt(gamma*R*temperature/ml)
    
  end subroutine cps_atm_exponentiel

end module cps_atm_exp
