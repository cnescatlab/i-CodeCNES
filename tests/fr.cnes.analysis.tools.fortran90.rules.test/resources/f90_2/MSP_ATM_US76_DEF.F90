module MSP_ATM_US76_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_US76_DEF
!
!$Resume
!	Module permettant d'utiliser le modèle US76
!
!$Description
!	Module permettant d'utiliser le modèle US76
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_US76_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATM_US76_DEF.F90,v $
!  Revision 1.13  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.12  2010/06/17 08:16:52  mercadig
!  VERSION::FA-ID:1411:17/06/2010: Mise a 0 des variables du modele si h > 1000 km, on ne retourne pas de warning pour les cas limites
!
!  Revision 1.11  2010/06/16 15:31:21  mercadig
!  VERSION::FA-ID:1411:16/06/2010: Meilleure gestion des cas limites
!
!  Revision 1.10  2010/06/16 08:19:14  mercadig
!  VERSION::FA-ID:1411:16/06/2010: Gestion des cas limites pour le modele US76 (retour de warning)
!
!  Revision 1.9  2009/01/28 08:37:57  cml
!  FA-ID 1225 : Utilisation du modele US76 de COMPAS au lieu de la MSPRO
!
!  Revision 1.8  2008/11/24 13:28:00  mercadig
!  DM-ID 733 : Suppression traces debug
!
!  Revision 1.7  2008/11/21 13:06:11  cml
!  AQ : Ajout d une initialisation manquante
!
!  Revision 1.6  2008/11/20 08:02:23  tanguyy
!  DM-ID 733 : pas de "bulle de densité" par défaut pour le modèle US76D : la densité (ro) rendue est ainsi strictement celle du modèle.
!
!  Revision 1.5  2008/11/12 10:22:05  mercadig
!  DM-ID 733: Ajout du test sur altitude pour utilisation du modele us76d
!
!  Revision 1.4  2007/10/23 15:01:52  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.3  2005/03/08 07:32:33  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.2  2005/03/03 09:13:22  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.1  2005/01/21 09:57:24  rostan
!  dm-
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:53:33  simbad
!  Ajout du calcul des atmospheres
!  Revision 1.2  2004/01/22 18:27:24  simbad
!  Avant suppression de AMLIB et IOLIB
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_US76_DEF
!
!$Structure
!
!: MSP_ATM_US76 : 
!>     xkro     : <PM_REEL,DIM=(5)>    
!>     hro1     : <PM_REEL,DIM=(5)>    
!>     hro2     : <PM_REEL,DIM=(5)>    
!>     roh      : <PM_REEL,DIM=(5)>    
!>     deltat   : <PM_REEL,DIM=(0:7)>  
!>     idens    : <integer>            
!
!$Global
!
!$Common
!
!$Routines
!- egaler_us76
!- MSP_effacer_atm_us76
!- MSP_modifier_atm_us76
!- MSP_cal_atm_us76
!
!$Fonctions
!- MSP_creer_atm_us76
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSPRO
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> assignment :  egaler_us76
!#V
!#
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE US76
!
!$Voir-Aussi
!.  MSP_creer_atm_us76 egaler_us76 MSP_effacer_atm_us76 MSP_modifier_atm_us76 MSP_cal_atm_us76
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB, only : PM_REEL, tm_geodesique, PM_PI,mt_car_geod
  use cps_atm_us76d_mod, only : cps_atm_us76d
  use MSP_GESTION_ERREUR

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_US76_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_ATM_US76
     real(kind=PM_REEL),dimension(5)   :: xkro,hro1,hro2,roh
     real(kind=PM_REEL),dimension(8) :: deltat
     integer ::idens
  end type MSP_ATM_US76


   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère US76
!
!$Description
! Cette routine permet d'égaler deux structures atmosphèreUS76
!
!$Acces
!  PUBLIC
!
!$Usage
!  atma=atmb
!.    type(MSP_ATM_US76) :: atma
!.    type(MSP_ATM_US76) :: atmb
!
!$Procedures
!- egaler_us76
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOPSHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_us76
   end interface

CONTAINS

  subroutine egaler_us76(atma,atmb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_us76
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère US76
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère US76
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call egaler_us76(atma,atmb)
!.    type(MSP_ATM_US76) :: atma
!.    type(MSP_ATM_US76) :: atmb
!
!$Arguments
!>S     atma  :<MSP_ATM_US76>   
!>E     atmb  :<MSP_ATM_US76>   
!
!$Common
!
!$Routines
!- MSP_effacer_atm_us76
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOPSHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    type(MSP_ATM_US76),intent(out)::atma
    type(MSP_ATM_US76),intent(in)::atmb

    call MSP_effacer_atm_us76(atma)

    atma%xkro(:) = atmb%xkro(:)
    atma%hro1(:) = atmb%hro1(:)
    atma%hro2(:) = atmb%hro2(:)
    atma%roh(:) = atmb%roh(:)
    atma%deltat(:) = atmb%deltat(:)
    atma%idens = atmb%idens

  end subroutine egaler_us76



  subroutine MSP_effacer_atm_us76(atma)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_us76
!
!$Resume
!	Routine d'initialisation d'une structure atmosphere US76
!
!$Description
!	Routine d'initialisation d'une structure atmosphere US76 
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_us76(atma)
!.    type(MSP_ATM_US76) :: atma
!
!$Arguments
!>S     atma  :<MSP_ATM_US76>   
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
! INITIALISATION ATMOSPHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_US76),intent(out)::atma


    atma%xkro(:) = 0._PM_REEL
    atma%hro1(:) = 0._PM_REEL
    atma%hro2(:) = 0._PM_REEL
    atma%roh(:) = 0._PM_REEL
    atma%deltat(:) = 0._PM_REEL
    atma%idens = 0

  end subroutine MSP_effacer_atm_us76


  function MSP_creer_atm_us76(xkro,hro1,hro2,roh,deltat,idens) result (atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_us76
!
!$Resume
!	Routine de création d'une structure atmosphere US76 
!
!$Description
!	Routine de création d'une structure atmosphere US76 
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm = MSP_creer_atm_us76([xkro],[hro1],[hro2],[roh],[deltat],[idens])
!.    real(kind=PM_REEL),dimension(5) :: xkro,hro1,hro2,roh
!.    real(kind=PM_REEL),dimension(0:7) :: deltat
!.    integer :: idens
!.    type (Msp_atm_us76) :: atm
!
!$Arguments
!>[E]   xkro    :<PM_REEL,DIM=(5)>     
!>[E]   hro1    :<PM_REEL,DIM=(5)>     
!>[E]   hro2    :<PM_REEL,DIM=(5)>     
!>[E]   roh     :<PM_REEL,DIM=(5)>     
!>[E]   deltat  :<PM_REEL,DIM=(8)>   
!>[E]   idens   :<integer>             
!>S     atm     :<Msp_atm_us76>        
!
!$Common
!
!$Routines
!- MSP_effacer_atm_us76
!- cps_atm_us76d
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CREER ATMOSPHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     real(kind=PM_REEL),dimension(5),intent(in),optional   :: xkro,hro1,hro2,roh
     real(kind=PM_REEL),dimension(8),intent(in),optional  :: deltat
     integer,intent(in),optional ::idens
     type (Msp_atm_us76) :: atm


     ! Variables locales
     real(kind=pm_reel)::text,pres,ro,vson,xmu
     type (tm_code_retour) :: code_erreur


     call MSP_effacer_atm_us76(atm)


     if (present(xkro)) then
        atm%xkro(:) = xkro(:)
     end if

     if (present(hro1)) then
        atm%hro1(:) = hro1(:)
     end if

     if (present(hro2)) then
        atm%hro2(:) = hro2(:)
     end if

     if (present(roh)) then
        atm%roh(:) = roh(:)
     end if

     if (present(deltat)) then
        atm%deltat(:) = deltat(:)
     end if

     if (present(idens)) then
        atm%idens = idens
     else
        ! par défaut : pas de "bulles" de densité
        ! -> la valeur de r0 rendue est strictement celle du 
        ! modèle US76.
        atm%idens = 0
     end if

     ! Initialisation du modèle

     call cps_atm_us76d (atm%deltat,0._pm_reel , ro, code_erreur, &
                            0._pm_reel, vson, text, &
                            pres, xmu)

     call MSP_signaler_message (ier_mslib=code_erreur)
     if (MSP_gen_messages("MSP_creer_atm_us76") ) return


   end function MSP_creer_atm_us76


  subroutine MSP_modifier_atm_us76(atm,xkro,hro1,hro2,roh,deltat,idens)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_us76
!
!$Resume
! 	Routine permettant de modifier une structure atmosphere US76
!
!$Description
!	Routine permettant de modifier une structure atmosphere US76
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_us76(atm,[xkro],[hro1],[hro2],[roh],[deltat],[idens])
!.    type (Msp_atm_us76) :: atm
!.    real(kind=PM_REEL),dimension(5) :: xkro,hro1,hro2,roh
!.    real(kind=PM_REEL),dimension(8) :: deltat
!.    integer :: idens
!
!$Arguments
!>E/S   atm     :<Msp_atm_us76>        structure atmosphère US76 
!>[E]   xkro    :<PM_REEL,DIM=(5)>     
!>[E]   hro1    :<PM_REEL,DIM=(5)>     
!>[E]   hro2    :<PM_REEL,DIM=(5)>     
!>[E]   roh     :<PM_REEL,DIM=(5)>     
!>[E]   deltat  :<PM_REEL,DIM=(8)>   
!>[E]   idens   :<integer>             
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
! MODIFIER ATMOSPHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type (Msp_atm_us76) :: atm
    real(kind=PM_REEL),dimension(5),intent(in),optional   :: xkro,hro1,hro2,roh
    real(kind=PM_REEL),dimension(8),intent(in),optional  :: deltat
    integer,intent(in),optional ::idens
    


    if (present(xkro)) then
       atm%xkro(:) = xkro(:)
    end if
    
    if (present(hro1)) then
       atm%hro1(:) = hro1(:)
    end if
    
    if (present(hro2)) then
        atm%hro2(:) = hro2(:)
     end if
     
     if (present(roh)) then
        atm%roh(:) = roh(:)
     end if
     
     if (present(deltat)) then
        atm%deltat(:) = deltat(:)
     end if
     
     if (present(idens)) then
        atm%idens = idens
     end if
   end subroutine MSP_modifier_atm_us76

  subroutine MSP_cal_atm_us76(atmos,R,rlon,phisat,vson,pres,ro,xmu,text)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_cal_atm_us76
!
!$Resume
!  Routine de calcul d'atmosphere US76
!
!$Description
!  Routine de calcul d'atmosphere US76
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_cal_atm_us76(atmos,R,rlon,phisat,vson,pres,ro,xmu,text)
!.    type(msp_atm_us76) :: atmos
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!.    real (KIND=PM_REEL) :: pres 
!.    real (KIND=PM_REEL) :: xmu 
!.    real (KIND=PM_REEL) :: text 
!
!$Arguments
!>E     atmos   :<msp_atm_us76>   structure atmosphère US76
!>E     R       :<PM_REEL>        distance sonde-centre planète   
!>E     rlon    :<PM_REEL>        longitude
!>E     phisat  :<PM_REEL>        latitude   
!>S     vson    :<PM_REEL>        vitesse du son   
!>S     pres    :<PM_REEL>        pression  
!>S     ro      :<PM_REEL>        densité 
!>S     xmu     :<PM_REEL>        viscosité dynamique   
!>S     text    :<PM_REEL>        température   
!
!$Common
!
!$Routines
!- mt_car_geod
!- MSP_signaler_message
!- cps_atm_us76d
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!CALCUL ATMOSPHERE US76
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
    type(msp_atm_us76),intent(in)::atmos
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT) :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT) :: pres          ! Pression atmospherique
    real (KIND=PM_REEL), intent(out) :: xmu           ! Viscosité
    real (KIND=PM_REEL), intent(OUT) :: text          ! Temperature atmospherique


    !-----------------------------------------------------------------------------------
    ! Variables locales
    !-----------------------------------------------------------------------------------
    real(kind=PM_REEL)::dro
    integer :: i
    real(kind=PM_REEL),dimension(3) :: posgr
    type (tm_geodesique) :: corgeod
    type (tm_code_retour)::code_erreur

    ! Constantes terrestres utilisées pour le calcul de l'altitude
    real(kind=PM_REEL),parameter :: Req_terre = 6378.138e3_PM_REEL
    real(kind=PM_REEL),parameter :: alf_terre = 1._PM_REEL/298.257_PM_REEL

    real(kind=PM_REEL)::alsat

    logical, save :: warning_us76_existe_0 = .false. ! flag de test si altg < 0 km
    ! Variables pour sauvegarder les valeurs du modèle à 0 km
    real (KIND=PM_REEL), save :: ro_0 = 0._pm_reel
    real (KIND=PM_REEL), save :: vson_0 = 0._pm_reel
    real (KIND=PM_REEL), save :: pres_0 = 0._pm_reel
    real (KIND=PM_REEL), save :: xmu_0 = 0._pm_reel
    real (KIND=PM_REEL), save :: text_0 = 0._pm_reel
    
    
    


    pres = 0.0_PM_REEL
    ro = 0.0_PM_REEL
    xmu = 0.0_PM_REEL
    text = 0.0_PM_REEL
    vson = 0.0_PM_REEL


    ! Calcul des coordonnées cartésiennes du satellite
    ! Coordonnées du satellite dans le repère planétographique
    posgr(1) = R*cos(rlon)*cos(phisat)
    posgr(2) = R*sin(rlon)*cos(phisat)
    posgr(3) = R*sin(phisat)
    
    ! Calcul de l'altitude et de la latitude vraie
    call mt_car_geod (posgr,Req_terre,alf_terre,corgeod,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if ( MSP_ERREUR ) return
    
    alsat  = corgeod%haut

    ! Calcul des bulles de densité
    dro = 0.0_PM_REEL

    !Modif pour pouvoir générer de bulle de sous densités
    ! la densité moyenne n'est pas utilisée, la formule est remplcée par ro=ro*(1+Delta)
    boucle_bulle : do i = 1,atmos%idens
       if ((alsat > atmos%hro1(i)) .and. (alsat <atmos% hro2(i))) then
          dro =atmos%xkro(i)*(SIN(PM_PI*(alsat-atmos%hro1(i))/(atmos%hro2(i)-atmos%hro1(i))))**2
          exit boucle_bulle
       end if
    end do boucle_bulle
 
    ! Domaine de validité du modèle : 0 km =< altitude =< 1000 km
    ! Note FA 1411:
    ! Le modèle COMPAS ne retourne plus de warning si l'altitude sort du domaine
    ! Si h < 0 km alors la densité est calculée à 0 km
    ! Si h > 1000 km alors la densité est nulle 

    if (corgeod%haut > 1000000._pm_reel) then
       ! Mise à 0 des variables du modèle, on ne retourne pas de warning (FA 1411)
       ro = 0._pm_reel
       vson = 0._pm_reel 
       text = 0._pm_reel
       pres = 0._pm_reel
       xmu = 0._pm_reel
    else if (corgeod%haut < 0._pm_reel) then
       if (.not.warning_us76_existe_0) then
          ! Calcul du modèle US76D, on ne retourne pas de warning (FA 1411)
          call cps_atm_us76d (atmos%deltat,alsat, ro, code_erreur, &
                        delta_dens=0._pm_reel, vit_son=vson, temp=text, &
                        pres=pres, visco=xmu)
          if(MSP_gen_messages("MSP_cal_atm_us76")) return

	  if(code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("MSP_cal_atm_us76")) return
          end if

          ! Sauvegarde des variables du modèle pour les prochains appels
          ro_0 = ro
          vson_0 = vson
          text_0 = text
          pres_0 = pres
          xmu_0 = xmu	  
          ! Flag à true => on ne repassera pas dans cette branche
          warning_us76_existe_0 = .true.
       else
          ! On ne rappelle pas le modèle COMPAS pour éviter de dégrader les performances
          ! Les variables du modèle prennent les valeurs déjà calculées la première fois 
          ! où l'altitude est inférieure à 0 km
          ro = ro_0
          vson = vson_0
          text = text_0
          pres = pres_0
          xmu = xmu_0
       end if  
    else
       call cps_atm_us76d (atmos%deltat,alsat, ro, code_erreur, &
                           delta_dens=0._pm_reel, vit_son=vson, temp=text, &
                           pres=pres, visco=xmu)
       if(MSP_gen_messages("MSP_cal_atm_us76")) return

       if(code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
       end if
    end if

    ro = ro*(1._PM_REEL+dro)

  end subroutine MSP_cal_atm_us76
end module MSP_ATM_US76_DEF
