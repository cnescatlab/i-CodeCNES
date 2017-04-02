module MSP_ATM_EMCD_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_EMCD_DEF
!
!$Resume
!	Module définissant les données nécessaire à l'utilisation du modèle
!       EMCD (V2.3A, V3.1, V4.2 et V4.3)
!
!$Description
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_EMCD_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATM_EMCD_DEF.F90,v $
!  Revision 1.15  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.14  2009/01/13 14:02:51  cml
!  FA-ID 1179 : Correction des appels aux modeles EMCD 42 et 43
!
!  Revision 1.13  2008/11/19 13:29:27  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.12  2008/08/11 12:17:46  cml
!  DM-ID 1091 : Branchement du modele d'atmosphere EMCD 4.3
!  Revision 1.11  2008/08/08 13:59:13  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 des variables temporaires de
!  calcul dans la fonction calcul_atm_emcd
!  Revision 1.10  2008/07/04 15:02:31  huec
!  DM-ID 1058 : Gestion memoire et passage des resultats de COMPAS en PM_REEL
!  Revision 1.9  2008/02/22 13:56:29  huec
!  FA-ID 968 : Suppression de variables declarees en double
!  Revision 1.8  2007/11/22 15:46:31  jpi
!  DM-ID 551 : ajout modele EMCD42 dans MECASPA
!  Revision 1.7  2007/11/05 16:03:55  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.6  2007/10/23 15:02:29  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.5  2006/11/15 10:09:36  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.4  2006/11/09 09:13:56  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.3  2005/03/08 07:32:33  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.2  2005/03/01 16:22:34  fabrec
!  DM-ID 111 : modeles d'atmosphere
!  Revision 1.1  2005/01/21 09:57:26  rostan
!  dm-
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:53:30  simbad
!  Ajout du calcul des atmospheres
!
!$FinHistorique
!
!$Usage
!  use MSP_ATM_EMCD_DEF
!
!$Structure
!
!: MSP_ATM_EMCD : 
!>     type_emcd       : <integer>          version du modèle 
!          (MSP_ENUM_EMCD23, MSP_ENUM_EMCD31, MSP_ENUM_EMCD42, ou MSP_ENUM_EMCD43)
!>     mars_emcd_sce   : <integer>          scenario (clair, viking, mgs)
!>     mars_emcd_per   : <PM_REEL,DIM=(2)>  type de perturbation
!>     lambda_gw       : <PM_REEL>          longueur d'onde
!>     linit           : <logical>          flag d'initialisation
!>     dir_emcd        : <LEN=256>          directory contenant les données EMCD
!
!$Global
!
!>  MSP_ENUM_EMCD23   : <integer,parameter>  version 2.3A
!>  MSP_ENUM_EMCD31   : <integer,parameter>  verion 3.1
!>  MSP_ENUM_EMCD42   : <integer,parameter>  
!>  MSP_ENUM_EMCD43   : <integer,parameter>  
!$Common
!
!$Routines
!- MSP_effacer_atm_emcd
!- MSP_consulter_atm_emcd
!- MSP_modifier_atm_emcd
!- MSP_calculer_atm_emcd
!#V
!- egaler_emcd
!#
!
!$Fonctions
!- MSP_creer_atm_emcd
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- cps_modele_emcd23
!- cps_modele_emcd31
!- cps_modele_emcd42
!- cps_modele_emcd43
!#
!
!$Interface
!> assignment(=) :  egaler_emcd
!#V
!#
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE EMCD
!
!$Voir-Aussi
!#V
!.  egaler_emcd
!#
!.  MSP_creer_atm_emcd MSP_effacer_atm_emcd MSP_consulter_atm_emcd MSP_modifier_atm_emcd
!.  MSP_calculer_atm_emcd
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB, only : PM_REEL,PM_PI_SUR2
  use MSP_GESTION_ERREUR
  use cps_modele_emcd23
  use cps_modele_emcd31
  use cps_modele_emcd42
  use cps_modele_emcd43

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_EMCD_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_ATM_EMCD
     integer             :: type_emcd
     integer             :: mars_emcd_sce
     real(kind=PM_REEL),dimension(2)  :: mars_emcd_per
     real(kind=PM_REEL)  :: lambda_gw
     logical             :: linit
     character(len=256)  :: dir_emcd
  end type MSP_ATM_EMCD
 
  interface ASSIGNMENT(=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT(=)
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère emcd
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère emcd
!
!$Acces
!  PUBLIC
!
!$Usage
!  =(atm_emcda=atm_emcdb)
!.    type(MSP_ATM_EMCD) :: atm_emcda
!.    type(MSP_ATM_EMCD) :: atm_emcdb
!
!$Procedures
!#V
!- egaler_emcd
!#
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHER EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_emcd
  end interface

  private :: egaler_emcd

  integer,parameter :: MSP_ENUM_EMCD23 = 23
  integer,parameter :: MSP_ENUM_EMCD31 = 31
  integer,parameter :: MSP_ENUM_EMCD42 = 42
  integer,parameter :: MSP_ENUM_EMCD43 = 43

CONTAINS

 subroutine MSP_effacer_atm_emcd(atm_emcd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_emcd
!
!$Resume
!	Routine d'initialisation d'un structure MSP_ATM_EMCD
!
!$Description
!	Routine d'initialisation d'un structure MSP_ATM_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_emcd(atm_emcd)
!.    type(MSP_ATM_EMCD) :: atm_emcd
!
!$Arguments
!>S     atm_emcd  :<MSP_ATM_EMCD>   
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
! INITIALISER ATMOSPHERE EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(MSP_ATM_EMCD),intent(out) :: atm_emcd
   atm_emcd%type_emcd         = 0 
   atm_emcd%mars_emcd_sce     = 0
   atm_emcd%mars_emcd_per(:)  = 0._PM_REEL
   atm_emcd%lambda_gw         = 0._PM_REEL
   atm_emcd%dir_emcd          = ""
   atm_emcd%linit             = .false.
   
 end subroutine MSP_effacer_atm_emcd

 subroutine egaler_emcd(atm_emcda,atm_emcdb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_emcd
!
!$Resume
!	Routine d'affectation de deux structures MSP_ATM_EMCD
!
!$Description
!	Routine d'affectation de deux structures MSP_ATM_EMCD
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_emcd(atm_emcda,atm_emcdb)
!.    type(MSP_ATM_EMCD) :: atm_emcda
!.    type(MSP_ATM_EMCD) :: atm_emcdb
!
!$Arguments
!>E/S   atm_emcda  :<MSP_ATM_EMCD>   
!>E     atm_emcdb  :<MSP_ATM_EMCD>   
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
! EGALER ATMOSPHER EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(MSP_ATM_EMCD),intent(inout) :: atm_emcda
   type(MSP_ATM_EMCD),intent(in) :: atm_emcdb
      
   atm_emcda%type_emcd         = atm_emcdb%type_emcd
   atm_emcda%mars_emcd_sce     = atm_emcdb%mars_emcd_sce
   atm_emcda%mars_emcd_per(:)  = atm_emcdb%mars_emcd_per(:)
   atm_emcda%lambda_gw         = atm_emcdb%lambda_gw
   atm_emcda%dir_emcd          = atm_emcdb%dir_emcd
   atm_emcda%linit             = atm_emcdb%linit
 end subroutine egaler_emcd


 function MSP_creer_atm_emcd(type_emcd,mars_emcd_sce,mars_emcd_per,lambda_gw,dir_emcd) result (atm_emcd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_emcd
!
!$Resume
!	Routine de création d'une structure MSP_ATM_EMCD
!
!$Description
!	Routine de création d'une structure MSP_ATM_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm_emcd = MSP_creer_atm_emcd([type_emcd],[mars_emcd_sce],[mars_emcd_per],[lambda_gw],[dir_emcd])
!.    integer :: type_emcd
!.    integer :: mars_emcd_sce
!.    real(kind=PM_REEL),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL) :: lambda_gw
!.    character(len=*) :: dir_emcd
!.    type(MSP_ATM_EMCD) :: atm_emcd
!
!$Arguments
!>[E]   type_emcd      :<integer>           version du modèle (MSP_ENUM_EMCD23, MSP_ENUM_EMCD31)
!>[E]   mars_emcd_sce  :<integer>           scenario (clair, vicking, mgs)
!.                         pour la version 2.3A
!.                            1 = Viking dust, 
!.                            2 = low dust, 
!.                            3 = dust storm tau=2,
!.                            4 = dust storm tau=5,
!.                         pour la version 3.1
!.                            1 = MGS dust,
!.                            2 = Viking dust,
!.                            3 = low dust,
!.                            4 = dust storm tau=2,
!.                            5 = dust storm tau=5
!>[E]   mars_emcd_per  :<PM_REEL,DIM=(2)>   type de perturbations
!.                           none : typper(1)= 1. typper(2)=0.
!.                    large scale : typper(1)= 2. typper(2)=seed number
!.                    small scale : typper(1)= 3. typper(2)=seed number
!.          large and small scale : typper(1)= 4. typper(2)=seed number
!. n times the standard deviation : typper(1)= 5. typper(2)=n
!>[E]   lambda_gw      :<PM_REEL>           longueur d'onde
!.                           wavelength of gravity wave perturbation (km)
!.                           set to zero to get default (16km)
!>[E]   dir_emcd       :<LEN=*>             directory contenant les données EMCD
!>S     atm_emcd       :<MSP_ATM_EMCD>      structure MSP_ATM_EMCD
!
!$Common
!
!$Routines
!- MSP_effacer_atm_emcd
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CREER ATMOSPHERE EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer,optional,intent(in)                          :: type_emcd
   integer,optional,intent(in)                          :: mars_emcd_sce
   real(kind=PM_REEL),optional,intent(in),dimension(2)  :: mars_emcd_per
   real(kind=PM_REEL),optional,intent(in)               :: lambda_gw
   character(len=*) ,optional,intent(in)                :: dir_emcd
   type(MSP_ATM_EMCD)                                   :: atm_emcd


   ! variables locales
   integer :: ldir

   call MSP_effacer_atm_emcd(atm_emcd)

   atm_emcd%linit = .true.

   if (present(type_emcd)) then
      atm_emcd%type_emcd=type_emcd
   else
      atm_emcd%type_emcd=MSP_ENUM_EMCD23
   end if

   if (present(mars_emcd_sce)) then
      atm_emcd%mars_emcd_sce = mars_emcd_sce
   else
      atm_emcd%mars_emcd_sce = 2
   end if

   if ( present(mars_emcd_per) ) then
      atm_emcd%mars_emcd_per(:) = mars_emcd_per(:)
   else
      ! par defaut le modele est initialise a aucune perturbations
      atm_emcd%mars_emcd_per(:) = (/ 1._pm_reel , 0._pm_reel /)
   endif
   
   if ( present(lambda_gw)) then
      atm_emcd%lambda_gw = lambda_gw
   else
      atm_emcd%lambda_gw = 16._PM_REEL
   end if

   if ( present(dir_emcd) ) then
      ldir = LEN_TRIM(dir_emcd)
      if ( ldir <= 256 ) then 
         atm_emcd%dir_emcd = dir_emcd
      else
         atm_emcd%dir_emcd = dir_emcd(:256)
         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
              routine="MSP_creer_atm_emcd",type=MSP_ENUM_WARNING,partie_variable='"Le directory du modèle EMCD""256"')
      endif
   endif
 end function MSP_creer_atm_emcd


 

 subroutine MSP_consulter_atm_emcd(atm_emcd,type_emcd,mars_emcd_sce,mars_emcd_per,lambda_gw,dir_emcd,linit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_atm_emcd
!
!$Resume
!	Routine permettant de consulter la structure EMCD
!
!$Description
!	Routine permettant de consulter la structure EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_atm_emcd(atm_emcd,[type_emcd],[mars_emcd_sce],[mars_emcd_per],[lambda_gw],[dir_emcd],[linit])
!.    type(MSP_ATM_EMCD) :: atm_emcd
!.    integer :: type_emcd
!.    integer :: mars_emcd_sce
!.    real(kind=PM_REEL),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL) :: lambda_gw 
!.    character(len=*) :: dir_emcd
!.    logical :: linit
!
!$Arguments
!>E     atm_emcd       :<MSP_ATM_EMCD>      structure MSP_ATM_EMCD
!>[S]   type_emcd      :<integer>           version du modèle (MSP_ENUM_EMCD23, MSP_ENUM_EMCD31)
!>[S]   mars_emcd_sce  :<integer>           scenario (clair, vicking, mgs)
!.                         pour la version 2.3A
!.                            1 = Viking dust, 
!.                            2 = low dust, 
!.                            3 = dust storm tau=2,
!.                            4 = dust storm tau=5,
!.                         pour la version 3.1
!.                            1 = MGS dust,
!.                            2 = Viking dust,
!.                            3 = low dust,
!.                            4 = dust storm tau=2,
!.                            5 = dust storm tau=5
!>[S]   mars_emcd_per  :<PM_REEL,DIM=(2)>   type de perturbations
!.                           none : typper(1)= 1. typper(2)=0.
!.                    large scale : typper(1)= 2. typper(2)=seed number
!.                    small scale : typper(1)= 3. typper(2)=seed number
!.          large and small scale : typper(1)= 4. typper(2)=seed number
!. n times the standard deviation : typper(1)= 5. typper(2)=n
!>[S]   lambda_gw      :<PM_REEL>           longueur d'onde
!.                           wavelength of gravity wave perturbation (km)
!.                           set to zero to get default (16km)
!>[S]   dir_emcd       :<LEN=*>             directory contenant les données
!>[S]   linit          :<logical>           flag d'initiallisation
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
! CONSULTER ATMOSPHERE EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(MSP_ATM_EMCD),intent(in)                         :: atm_emcd
   integer,optional,intent(out)                          :: type_emcd
   integer,optional,intent(out)                          :: mars_emcd_sce
   real(kind=PM_REEL),optional,intent(out),dimension(2)  :: mars_emcd_per
   real(kind=PM_REEL),optional,intent(out)               :: lambda_gw   
   character(len=*) ,optional,intent(out)                :: dir_emcd
   logical,optional,intent(out)                          :: linit

   if ( present(type_emcd) )     type_emcd        = atm_emcd%type_emcd
   if ( present(mars_emcd_sce) ) mars_emcd_sce    = atm_emcd%mars_emcd_sce
   if ( present(mars_emcd_per) ) mars_emcd_per(:) = atm_emcd%mars_emcd_per(:)
   if ( present(lambda_gw) )     lambda_gw        = atm_emcd%lambda_gw
   if ( present(dir_emcd) )      dir_emcd         = atm_emcd%dir_emcd
   if ( present(linit) )         linit            = atm_emcd%linit
 end subroutine MSP_consulter_atm_emcd

 subroutine MSP_modifier_atm_emcd(atm_emcd,type_emcd,mars_emcd_sce,mars_emcd_per,lambda_gw,dir_emcd,linit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_emcd
!
!$Resume
!	Routine permettant de modifier la structure EMCD
!
!$Description
!	Routine permettant de modifier la structure EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_emcd(atm_emcd,[type_emcd],[mars_emcd_sce],[mars_emcd_per],[lambda_gw],[dir_emcd],[linit])
!.    type(MSP_ATM_EMCD) :: atm_emcd
!.    integer :: type_emcd
!.    integer :: mars_emcd_sce
!.    real(kind=PM_REEL),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL) :: lambda_gw 
!.    character(len=*) :: dir_emcd
!.    logical :: linit
!
!$Arguments
!>S     atm_emcd       :<MSP_ATM_EMCD>      structure MSP_ATM_EMCD
!>[E]   type_emcd      :<integer>           version du modèle (MSP_ENUM_EMCD23, MSP_ENUM_EMCD31)
!>[E]   mars_emcd_sce  :<integer>           scenario (clair, vicking, mgs)
!.                         pour la version 2.3A
!.                            1 = Viking dust, 
!.                            2 = low dust, 
!.                            3 = dust storm tau=2,
!.                            4 = dust storm tau=5,
!.                         pour la version 3.1
!.                            1 = MGS dust,
!.                            2 = Viking dust,
!.                            3 = low dust,
!.                            4 = dust storm tau=2,
!.                            5 = dust storm tau=5
!>[E]   mars_emcd_per  :<PM_REEL,DIM=(2)>   type de perturbations
!.                           none : typper(1)= 1. typper(2)=0.
!.                    large scale : typper(1)= 2. typper(2)=seed number
!.                    small scale : typper(1)= 3. typper(2)=seed number
!.          large and small scale : typper(1)= 4. typper(2)=seed number
!. n times the standard deviation : typper(1)= 5. typper(2)=n
!>[E]   lambda_gw      :<PM_REEL>           longueur d'onde
!.                           wavelength of gravity wave perturbation (km)
!.                           set to zero to get default (16km)       
!>[E]   dir_emcd       :<LEN=*>             directory contenant les données EMCD     
!>[E]   linit          :<logical>           flag d'initiallisation  
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
! MODIFIER ATMOSPHERE EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(MSP_ATM_EMCD),intent(out)                       :: atm_emcd
   integer,optional,intent(in)                          :: type_emcd
   integer,optional,intent(in)                          :: mars_emcd_sce
   real(kind=PM_REEL),optional,intent(in),dimension(2)  :: mars_emcd_per
   real(kind=PM_REEL),optional,intent(in)               :: lambda_gw   
   character(len=*) ,optional,intent(in)                :: dir_emcd
   logical,optional,intent(in)                          :: linit

   if ( present(type_emcd) )      atm_emcd%type_emcd        = type_emcd
   if ( present(mars_emcd_sce) )  atm_emcd%mars_emcd_sce    = mars_emcd_sce   
   if ( present(mars_emcd_per) )  atm_emcd%mars_emcd_per(:) = mars_emcd_per(:) 
   if ( present(lambda_gw) )      atm_emcd%lambda_gw        = lambda_gw
   if ( present(dir_emcd) )       atm_emcd%dir_emcd         = dir_emcd     
   if ( present(linit) )          atm_emcd%linit            = linit
 end subroutine MSP_modifier_atm_emcd



 subroutine MSP_calculer_atm_emcd(atm_emcd,date_js,R,rlon,phisat,ro,vson,pression,temperature,windm,aziwm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_emcd
!
!$Resume
!	Routine permettant de calculer les caractéristiques de l'atmosphère
!
!$Description
!	Routine permettant de calculer les caractéristiques de l'atmosphère
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_emcd(atm_emcd,date_js,R,rlon,phisat,ro,vson,pression,temperature,windm,aziwm)
!.    type (MSP_ATM_emcd) :: atm_emcd 
!.    type(tm_jour_sec) :: date_js 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!.    real (KIND=PM_REEL) :: pression 
!.    real (KIND=PM_REEL) :: temperature 
!.    real (KIND=PM_REEL) :: windm 
!.    real (KIND=PM_REEL) :: aziwm 
!
!$Arguments
!>E     atm_emcd     :<MSP_ATM_emcd>   structure MSP_ATM_EMCD
!>E     date_js      :<tm_jour_sec>    Date JJ cnes 1950 en TE
!>E     R            :<PM_REEL>        Distance centre planete - sonde (m)
!>E     rlon         :<PM_REEL>        Longitude (rad)
!>E     phisat       :<PM_REEL>        Latitude (sphérique) (rad)
!>S     ro           :<PM_REEL>        Densite atmospherique (kg/m^3)
!>S     vson         :<PM_REEL>        Vitesse du son (m/s)
!>S     pression     :<PM_REEL>        Pression atmospherique (Pa)
!>S     temperature  :<PM_REEL>        Temperature atmospherique (K)
!>S     windm        :<PM_REEL>        Vitesse du vent en m/s
!>S     aziwm        :<PM_REEL>        Azimut du vent en radians
!
!$Common
!
!$Routines
!- cps_atmemcd_23
!- cps_atmemcd_23_close
!- MSP_signaler_message
!- cps_height_31
!- cps_atmemcd_31
!- cps_atmemcd_31_close
!- cps_atmemcd_42
!- cps_atmemcd_42_close
!- cps_atmemcd_43
!- cps_atmemcd_43_close
!- mu_angle2
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CALCULER ATMOSPHERE EMCD
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   use cps_modele_emcd42
   
    type (MSP_ATM_emcd), intent(IN)  :: atm_emcd      ! structure atmosphère
    type(tm_jour_sec)  , intent(IN)  :: date_js       ! Date JJ cnes 1950 en TE
    real (KIND=PM_REEL), intent(IN)  :: R             ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  :: rlon          ! Longitude
    real (KIND=PM_REEL), intent(IN)  :: phisat        ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT) :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT) :: pression      ! Pression atmospherique
    real (KIND=PM_REEL), intent(OUT) :: temperature   ! Temperature atmospherique
    real (KIND=PM_REEL), intent(OUT) :: windm         ! Vitesse du vent en m/s
    real (KIND=PM_REEL), intent(OUT) :: aziwm         ! Azimut du vent en radians

    ! Variables locales

    ! Définition des constantes utilisées par le modèle Martien
    real(kind=PM_REEL),parameter :: Req_Mars   = 3393.4e+03_pm_reel
    real(kind=PM_REEL),parameter :: Rpole_Mars = 3375.8e+03_pm_reel

    real(kind=PM_REEL)  :: alsat
    real(kind=PM_REEL)  :: date_jul
    real(kind=PM_REEL)  :: seedout
    real(kind=PM_REEL),dimension(5)  :: meanvar
    real(kind=PM_REEL),dimension(14) :: extvar
    real(kind=PM_REEL),dimension(25) :: extvar2
    real,dimension(100) :: extvar42                ! Variables suppl. calculées dans l'EMCD 4.2
    real(KIND=pm_reel),dimension(100) :: extvar43  ! Variables suppl. calculées dans l'EMCD 4.3
    real (KIND=pm_reel) :: vent_est,vent_nord      ! composantes est,nord du vent
    integer :: ier
    integer, parameter::ikey=0

    ! EMCD42 et 43
    integer :: zkey      ! flag to choose the type of z coordinates
    integer :: hireskey  ! high resolution flag (0: off, 1: on) 
    integer :: datekey   ! date flag (0: Earth date 1: Mars date)
    integer :: extvarkey ! extra output variables (1: yes, 0: no)
    real(KIND=pm_reel)    :: localtime ! local time at longitude xlon (only if datekey=1)

    type (tm_code_retour) :: code_erreur

    ! Hauteur par rapport a l'areoide de reference
    real(KIND=PM_REEL) :: zareoid
    ! Hauteur par rapport a la surface locale
    real(KIND=PM_REEL) :: zsurf
    real(kind=PM_REEL) :: MUANGL

    ! Utilisation de variables temporaires pour le modele EMCD42
    ! puis "castage" en real(kind=PM_REEL) --> intent(out)
    real :: pression_emcd42      ! Pression atmospherique
    real :: ro_emcd42            ! Densite atmospherique
    real :: temperature_emcd42   ! Temperature atmospherique
    real :: vent_est_emcd42      ! composante est
    real :: vent_nord_emcd42     ! composante nord
    real,dimension(5) :: meanvar_emcd42 ! meanvar
    real :: seedout_emcd42

    ! Initialisations
    extvar = 0._pm_reel
    extvar2 = 0._pm_reel
    zsurf = 0._pm_reel
    vent_est = 0._pm_reel
    vent_nord = 0._pm_reel
    ! Init des variables de sorties de l'EMCD 4.2
    meanvar_emcd42 = 0.0
    pression_emcd42 = 0.0
    ro_emcd42 = 0.0
    seedout_emcd42 = 0.0
    temperature_emcd42 = 0.0
    vent_nord_emcd42 = 0.0
    vent_est_emcd42 = 0.0
    extvar42 = 0.0
    ! Init des variables de sorties de l'EMCD 4.3
    extvar43 = 0._pm_reel

    ! Altitude difference des rayons sur l'ellipsoide
    alsat = R-req_Mars/(sqrt(cos(phisat)*cos(phisat)+sin(phisat)*sin(phisat)*req_Mars*req_Mars/rpole_Mars/rpole_Mars))

    if (alsat <= 0.3e+6_pm_reel) then
       
       date_jul = date_js%jour + date_js%sec/86400._pm_reel + 2433282.5_pm_reel ! conversion en jour julien
       select case (atm_emcd%type_emcd)
       ! ------ modele EMCD23 -----------
       case( MSP_ENUM_EMCD23 )
          
          ! Calcul de la pression, de ro, de la temperature, de vson

          call cps_atmemcd_23(alsat,phisat,rlon,date_jul,atm_emcd%dir_emcd,atm_emcd%mars_emcd_sce,&
               atm_emcd%mars_emcd_per,atm_emcd%lambda_gw,&
               seedout,ikey,pression,ro,temperature,vent_est,vent_nord,meanvar,extvar,ier)
          
          call cps_atmemcd_23_close()
          
          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_002",routine="MSP_calculer_atm_emcd")
             return
          endif
          vson=sqrt(1.3_pm_reel*pression/ro)
          
       ! ------ modele EMCD31 -----------
       case( MSP_ENUM_EMCD31 ) 
          ! Calcul de l'altitude par rapport a l'areoide de reference de MOLA
          call cps_height_31(phisat,rlon,R,zareoid,zsurf,1,atm_emcd%dir_emcd,ier)
          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_004",routine="MSP_calculer_atm_emcd")
             return
          endif
          ! Calcul de la presssion, de ro, de la temperature
          call cps_atmemcd_31(zareoid,phisat,rlon,date_jul,atm_emcd%dir_emcd,atm_emcd%mars_emcd_sce,&
               atm_emcd%mars_emcd_per,atm_emcd%lambda_gw,atm_emcd%linit,&
               seedout,ikey,pression,ro,temperature,vent_est,vent_nord,meanvar,extvar2,ier)
          
          call cps_atmemcd_31_close()

          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_003",routine="MSP_calculer_atm_emcd")
             return
          endif

       ! ------ modele EMCD42 ----------
       case( MSP_ENUM_EMCD42 )
          ! Calcul de pres, ro, temp, u, v
          ! On fixe les codes permetant de declarer les valeurs d'entree
          ! A savoir : date terrienne, distance depuis le centre, haute résolution topographique
          zkey=2            ! flag for vertical coordinate type
          hireskey=1        ! flag: high resolution if =1
          datekey=0         ! flag: 0=earth date 1= Mars date
          !localtime        ! localtime can only be imposed if datekey=1 and should be 0 otherwise
          localtime=0._pm_reel
          extvarkey=0       ! flag: compute extra variables if =1
          zareoid=R
          ier=0

          ! Calcul avec passage de la longueur d'onde de km en m
          call cps_atmemcd_42(zkey,real(zareoid),real(rlon),real(phisat),hireskey, &
               datekey,date_jul,real(localtime),atm_emcd%dir_emcd,atm_emcd%mars_emcd_sce, &
               int(atm_emcd%mars_emcd_per(1)), real(atm_emcd%mars_emcd_per(2)), &
               real(atm_emcd%lambda_gw) * 1000.0, extvarkey, &
               pression_emcd42, ro_emcd42, temperature_emcd42, vent_est_emcd42, vent_nord_emcd42, &
               meanvar_emcd42,extvar42,seedout_emcd42,ier)

          ! Utilisation de variables temporaires pour le modele EMCD42
          pression=     real(pression_emcd42,kind=pm_reel)
          ro=           real(ro_emcd42,kind=pm_reel)
          temperature=  real(temperature_emcd42,kind=pm_reel)
          vent_est=     real(vent_est_emcd42,kind=pm_reel)
          vent_nord=    real(vent_nord_emcd42,kind=pm_reel)
          meanvar(:)=   real(meanvar_emcd42(:),kind=pm_reel)
          seedout=      real(seedout_emcd42,kind=pm_reel)

          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_007",routine="MSP_calculer_atm_emcd")
             return
          endif

       ! ------ modele EMCD43 ----------
       case( MSP_ENUM_EMCD43 )
          ! Calcul de pres, ro, temp, u, v
          zkey=2               ! Type de coordonnées vertical fixé à 2 : hauteur au dessus de l'aréoïde
          hireskey=1           ! Flag de topographie fixé en haute résolution
          datekey=0            ! Flag spécifiant que la date sera terrienne
          localtime=0.         ! Champ inutile dans la mesure où datekey ~= 1
          extvarkey=0          ! Flag indiquant si des valeurs suppl doivent etre calculées
          zareoid=R            ! Hauteur au dessus de l'aréoïde
          ier=0                ! code de retour

          ! Contrairement au 4.2 pas besoin de passer par des variables
          ! de type real intermédiaire
          call cps_atmemcd_43(zkey, zareoid, rlon, phisat,hireskey, &
               datekey,date_jul,localtime, &
               atm_emcd%dir_emcd,atm_emcd%mars_emcd_sce, &
               int(atm_emcd%mars_emcd_per(1)), atm_emcd%mars_emcd_per(2), &
               atm_emcd%lambda_gw * 1000._pm_reel, extvarkey, &
               pression, ro, temperature, vent_est, vent_nord, &
               meanvar, extvar43, seedout,ier)

          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_008",routine="MSP_calculer_atm_emcd")
             return
          endif

       end select

       ! Calcul de vson
       vson=sqrt(1.3_pm_reel*pression/ro)
       ! Definition du vent tabule
       windm = sqrt(vent_est*vent_est+vent_nord*vent_nord)
       call mu_angle2(vent_est,vent_nord,MUANGL,code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("MSP_calculer_atm_emcd") ) return

       aziwm = PM_PI_SUR2 - MUANGL
    else
       pression = 0._pm_reel
       ro = 0._pm_reel
       temperature = 0._pm_reel
       vson = 0._PM_REEL
       windm = 0._PM_REEL
       aziwm = 0._PM_REEL
    endif

  end subroutine MSP_calculer_atm_emcd

end module MSP_ATM_EMCD_DEF
