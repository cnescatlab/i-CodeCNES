module MSP_ATM_MARSGRAM_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATM_MARSGRAM_DEF
!
!$Resume
!	module permettant d'utiliser le modèle MARSGRAM 2001
!
!$Description
!	module permettant d'utiliser le modèle MARSGRAM 2001
!
!$Auteur
!
!$Version
!  $Id: MSP_ATM_MARSGRAM_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATM_MARSGRAM_DEF.F90,v $
!  Revision 1.11  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.10  2008/11/19 13:30:05  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.9  2008/08/08 14:02:28  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 des variables locales de la
!  fonction calculer_atm_marsgram
!  Revision 1.8  2008/07/04 15:01:36  huec
!  DM-ID 1058 : Initialisation de variable
!  Revision 1.7  2008/04/08 12:59:24  huec
!  AQ : atm est inout car modifie
!  Revision 1.6  2007/11/27 09:07:07  huec
!  Correction d une anomalie
!  Revision 1.5  2007/11/05 16:03:56  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.4  2007/10/23 15:02:05  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
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
!  use MSP_ATM_MARSGRAM_DEF
!
!$Structure
!
!: MSP_ATM_MARSGRAM : 
!>     linit          : <logical>          flag d'initialisation du modèle
!>     marsgram_per   : <integer,DIM=(2)>  
!.                    marsgram_per(1) : Perturbation type
!.                             1 = nominal atmosphere
!.                             2 = perturbed atmosphere  
!.                    marsgram_per(2) : Seed number for perturbed atmosphere
!>     marsgram_sce   : <integer>          Dust scenario
!.                             1 = Viking dust optical depth (tau is provided
!.                                 by the Mars-GRAM code),
!.                             2 = specific tau (tau is given by <dust> variable)         
!>     dustOD         : <PM_REEL>          Dust optical depth
!.                             < 0 : Viking dust scenario
!.                             > 0 (forced to be in [0.1-3.0] range)         
!>     rpscale        : <PM_REEL>          Multiplicative factor for density and wind perturbations    
!>     directory      : <LEN=300>          Mars-GRAM 2001 database directory
!.                             One or more blanks to get the default,
!.                             or full directory path of GRAM 2001 data required
!.                             including trailing slash (e.g. '/dir/path/'),
!.                             or a link to a directory (e.g. 'GRAM_DATA/').
!.                             Default is link GRAM_DATA/ in working directory.         
!>     fichier        : <LEN=300>          GRAM input configuration file (NAMELIST format)
!.                             Default is GRAM_INPUT in working directory.       
!
!$Global
!
!$Common
!
!$Routines
!- MSP_effacer_atm_marsgram
!- MSP_consulter_atm_marsgram
!- MSP_modifier_atm_marsgram
!- MSP_calculer_atm_marsgram
!#V
!- egaler_marsgram
!#
!
!$Fonctions
!- MSP_creer_atm_marsgram
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- cps_atm_gram
!#
!
!$Interface
!> assignment :  egaler_marsgram
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  MARSGRAM
!
!$Voir-Aussi
!#V
!.  egaler_marsgram
!#
!.  MSP_creer_atm_marsgram MSP_effacer_atm_marsgram MSP_consulter_atm_marsgram MSP_modifier_atm_marsgram
!.  MSP_calculer_atm_marsgram
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB, only :PM_REEL,PM_PI_SUR2
  use MSP_GESTION_ERREUR
  use cps_atm_gram
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATM_MARSGRAM_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_ATM_MARSGRAM
     
     logical             :: linit  
     integer,dimension(2):: marsgram_per
     integer             :: marsgram_sce
     real(kind=PM_REEL)  :: dustOD
     real(kind=PM_REEL)  :: rpscale
     
     ! Directory ou sont stockes les donnees
     character(len=300) :: directory
     character(len=300) :: fichier
  end type MSP_ATM_MARSGRAM


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
!.    type(MSP_ATM_MARSGRAM) :: atma
!.    type(MSP_ATM_MARSGRAM) :: atmb
!
!$Procedures
!#V
!- egaler_marsgram
!#
!
!$Remarques
!
!$Mots-cles
! EGALER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_marsgram
  end interface

  private :: egaler_marsgram

CONTAINS

  subroutine MSP_effacer_atm_marsgram(atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atm_marsgram
!
!$Resume
!	Routine d'initialisation de la structure MARSGRAM
!
!$Description
!	Routine d'initialisation de la structure MARSGRAM
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atm_marsgram(atm)
!.    type(MSP_ATM_MARSGRAM) :: atm
!
!$Arguments
!>S     atm  :<MSP_ATM_MARSGRAM>   
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
!  EFFACER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_ATM_MARSGRAM),intent(out)::atm

    
     
     atm%linit           = .false.
     atm%marsgram_per(:) = 0
     atm%marsgram_sce    = 0
     atm%dustOD          = 0._PM_REEL
     atm%rpscale         = 0._PM_REEL
     atm%directory       = ""
     atm%fichier         = ""
   end subroutine MSP_effacer_atm_marsgram


   subroutine egaler_marsgram(atma,atmb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_marsgram
!
!$Resume
! Cette routine permet d'égaler deux structures atmosphère 
!
!$Description
! Cette routine permet d'égaler deux structures atmosphère 
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_marsgram(atma,atmb)
!.    type(MSP_ATM_MARSGRAM) :: atma
!.    type(MSP_ATM_MARSGRAM) :: atmb
!
!$Arguments
!>E/S   atma  :<MSP_ATM_MARSGRAM>   
!>E     atmb  :<MSP_ATM_MARSGRAM>   
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
! EGALER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     type(MSP_ATM_MARSGRAM),intent(inout) :: atma
     type(MSP_ATM_MARSGRAM),intent(in)    :: atmb

     atma%linit           = atmb%linit
     atma%marsgram_per(:) = atmb%marsgram_per(:)
     atma%marsgram_sce    = atmb%marsgram_sce
     atma%dustOD          = atmb%dustOD
     atma%rpscale         = atmb%rpscale
     atma%directory       = atmb%directory
     atma%fichier         = atmb%fichier
   end subroutine egaler_marsgram

   function MSP_creer_atm_marsgram(marsgram_per,marsgram_sce,dustOD,rpscale,directory,fichier) result (atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atm_marsgram
!
!$Resume
!	Routine de création de la structure MARSGRAM
!
!$Description
!	Routine de création de la structure MARSGRAM
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  atm = MSP_creer_atm_marsgram([marsgram_per],[marsgram_sce],[dustod],[rpscale],[directory],[fichier])
!.    integer,dimension(2) :: marsgram_per
!.    integer :: marsgram_sce
!.    real(kind=PM_REEL) :: dustOD
!.    real(kind=PM_REEL) :: rpscale
!.    character(len=*) :: directory
!.    character(len=*) :: fichier
!.    type(MSP_ATM_MARSGRAM) :: atm
!
!$Arguments
!>[E]   marsgram_per  :<integer,DIM=(2)>    
!.                    marsgram_per(1) : Perturbation type
!.                             1 = nominal atmosphere
!.                             2 = perturbed atmosphere  
!.                    marsgram_per(2) : Seed number for perturbed atmosphere
!>[E]   marsgram_sce  :<integer>            Dust scenario
!.                             1 = Viking dust optical depth (tau is provided
!.                                 by the Mars-GRAM code),
!.                             2 = specific tau (tau is given by <dust> variable)        
!>[E]   dustOD        :<PM_REEL>            Dust optical depth
!.                             < 0 : Viking dust scenario
!.                             > 0 (forced to be in [0.1-3.0] range)      
!>[E]   rpscale       :<PM_REEL>            Multiplicative factor for density and wind perturbations        
!>[E]   directory     :<LEN=*>              Mars-GRAM 2001 database directory
!.                             One or more blanks to get the default,
!.                             or full directory path of GRAM 2001 data required
!.                             including trailing slash (e.g. '/dir/path/'),
!.                             or a link to a directory (e.g. 'GRAM_DATA/').
!.                             Default is link GRAM_DATA/ in working directory.               
!>[E]   fichier       :<LEN=*>              GRAM input configuration file (NAMELIST format)
!.                             Default is GRAM_INPUT in working directory.         
!>S     atm           :<MSP_ATM_MARSGRAM>   structure atmosphère MARSGRAM
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
!  CREER ATMOPSHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
     integer,dimension(2),intent(in),optional:: marsgram_per
     integer             ,intent(in),optional:: marsgram_sce
     real(kind=PM_REEL)  ,intent(in),optional:: dustOD
     real(kind=PM_REEL)  ,intent(in),optional:: rpscale
     character(len=*)  ,intent(in),optional:: directory
     character(len=*)  ,intent(in),optional:: fichier

     type(MSP_ATM_MARSGRAM)::atm


     ! Variable locales
     integer ::lnom

     atm%linit = .true.
     

     if ( present(marsgram_sce) ) then
        atm%marsgram_sce = marsgram_sce
     else
        atm%marsgram_sce = 2
     endif


     if ( present(marsgram_per)) then
        atm%marsgram_per = marsgram_per
     else
        atm%marsgram_per(1) = 1
        atm%marsgram_per(2) = 0 
     end if


     if ( present(dustOD)) then
        atm%dustOD = dustOD
     else
        atm%dustOD = 0._PM_REEL
     end if
     
     if ( present(rpscale)) then
        atm%rpscale = rpscale
     else
        atm%rpscale = 1._PM_REEL
     end if

 

     if ( present(directory) ) then
        lnom = LEN_TRIM(directory)
        if ( lnom <= 300 ) then 
           atm%directory  = directory
        else
           atm%directory  = directory(:300)
           call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
                routine="MSP_creer_atm_marsgram",type=MSP_ENUM_WARNING,partie_variable='"Le nom du directory marsgram""300"')
        endif
     else
        atm%directory = ""
     endif
     
     if ( present(fichier) ) then
        lnom = LEN_TRIM(fichier)
        if ( lnom <= 300 ) then 
           atm%fichier  = fichier
        else
           atm%fichier  = fichier(:300)
           call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_atm_marsgram",type=MSP_ENUM_WARNING,partie_variable='"Le nom du fichier marsgram""300"')
        endif
     else
        atm%fichier = ""
     endif
     
   end function MSP_creer_atm_marsgram


   subroutine MSP_consulter_atm_marsgram(atm,marsgram_per,marsgram_sce,dustOD,rpscale,directory,fichier,linit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_atm_marsgram
!
!$Resume
!	Routine de consultation de la structure atmosphère MARSGRAM
!
!$Description
!	Routine de consultation de la structure atmosphère MARSGRAM
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_atm_marsgram(atm,[marsgram_per],[marsgram_sce],[dustod],[rpscale],[directory],[fichier],[linit])
!.    type(MSP_ATM_MARSGRAM) :: atm
!.    integer,dimension(2) :: marsgram_per
!.    integer :: marsgram_sce
!.    real(kind=PM_REEL) :: dustOD
!.    real(kind=PM_REEL) :: rpscale
!.    character(len=*) :: directory
!.    character(len=*) :: fichier
!.    logical :: linit
!
!$Arguments
!>E     atm           :<MSP_ATM_MARSGRAM>   structure atmosphère MARSGRAM
!>[S]   marsgram_per  :<integer,DIM=(2)>    
!.                    marsgram_per(1) : Perturbation type
!.                             1 = nominal atmosphere
!.                             2 = perturbed atmosphere  
!.                    marsgram_per(2) : Seed number for perturbed atmosphere
!>[S]   marsgram_sce  :<integer>            Dust scenario
!.                             1 = Viking dust optical depth (tau is provided
!.                                 by the Mars-GRAM code),
!.                             2 = specific tau (tau is given by <dust> variable)          
!>[S]   dustOD        :<PM_REEL>            Dust optical depth
!.                             < 0 : Viking dust scenario
!.                             > 0 (forced to be in [0.1-3.0] range)          
!>[S]   rpscale       :<PM_REEL>            Multiplicative factor for density and wind perturbations    
!>[S]   directory     :<LEN=*>              Mars-GRAM 2001 database directory
!.                             One or more blanks to get the default,
!.                             or full directory path of GRAM 2001 data required
!.                             including trailing slash (e.g. '/dir/path/'),
!.                             or a link to a directory (e.g. 'GRAM_DATA/').
!.                             Default is link GRAM_DATA/ in working directory.     
!>[S]   fichier       :<LEN=*>              GRAM input configuration file (NAMELIST format)
!.                             Default is GRAM_INPUT in working directory.      
!>[S]   linit         :<logical>            flag d'initialisation
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
! CONSULTER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     type(MSP_ATM_MARSGRAM),intent(in)::atm
     integer,dimension(2),intent(out),optional:: marsgram_per
     integer             ,intent(out),optional:: marsgram_sce
     real(kind=PM_REEL)  ,intent(out),optional:: dustOD
     real(kind=PM_REEL)  ,intent(out),optional:: rpscale
     character(len=*)  ,intent(out),optional:: directory
     character(len=*)  ,intent(out),optional:: fichier
     logical             ,intent(out),optional::linit

    
     if ( present(marsgram_per)) marsgram_per(:) = atm%marsgram_per(:)
     if ( present(marsgram_sce)) marsgram_sce    = atm%marsgram_sce
     if ( present(dustOD))       dustOD          = atm%dustOD
     if ( present(rpscale))      rpscale         = atm%rpscale
     if ( present(directory))    directory       = atm%directory
     if ( present(fichier))      fichier         = atm%fichier
     if ( present(linit))        linit           = atm%linit
   end subroutine MSP_consulter_atm_marsgram
   subroutine MSP_modifier_atm_marsgram(atm,marsgram_per,marsgram_sce,dustOD,rpscale,directory,fichier,linit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atm_marsgram
!
!$Resume
!	Routine permettant de modifier une structure atmosphère MARSGRAM
!
!$Description
!	Routine permettant de modifier une structure atmosphère MARSGRAM
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atm_marsgram(atm,[marsgram_per],[marsgram_sce],[dustod],[rpscale],[directory],[fichier],[linit])
!.    type(MSP_ATM_MARSGRAM) :: atm
!.    integer,dimension(2) :: marsgram_per
!.    integer :: marsgram_sce
!.    real(kind=PM_REEL) :: dustOD
!.    real(kind=PM_REEL) :: rpscale
!.    character(len=*) :: directory
!.    character(len=*) :: fichier
!.    logical :: linit
!
!$Arguments
!>E/S   atm           :<MSP_ATM_MARSGRAM>   structure atmosphère MARSGRAM
!>[E]   marsgram_per  :<integer,DIM=(2)>    
!.                    marsgram_per(1) : Perturbation type
!.                             1 = nominal atmosphere
!.                             2 = perturbed atmosphere  
!.                    marsgram_per(2) : Seed number for perturbed atmosphere
!>[E]   marsgram_sce  :<integer>            Dust scenario
!.                             1 = Viking dust optical depth (tau is provided
!.                                 by the Mars-GRAM code),
!.                             2 = specific tau (tau is given by <dust> variable)    
!>[E]   dustOD        :<PM_REEL>            Dust optical depth
!.                             < 0 : Viking dust scenario
!.                             > 0 (forced to be in [0.1-3.0] range)          
!>[E]   rpscale       :<PM_REEL>            Multiplicative factor for density and wind perturbations        
!>[E]   directory     :<LEN=*>              Mars-GRAM 2001 database directory
!.                             One or more blanks to get the default,
!.                             or full directory path of GRAM 2001 data required
!.                             including trailing slash (e.g. '/dir/path/'),
!.                             or a link to a directory (e.g. 'GRAM_DATA/').
!.                             Default is link GRAM_DATA/ in working directory.      
!>[E]   fichier       :<LEN=*>              GRAM input configuration file (NAMELIST format)
!.                             Default is GRAM_INPUT in working directory.             
!>[E]   linit         :<logical>            flag d'initialisation 
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
! MODIFIER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     type(MSP_ATM_MARSGRAM),intent(inout)::atm
     integer,dimension(2),intent(in),optional:: marsgram_per
     integer             ,intent(in),optional:: marsgram_sce
     real(kind=PM_REEL)  ,intent(in),optional:: dustOD
     real(kind=PM_REEL)  ,intent(in),optional:: rpscale
     character(len=*)  ,intent(in),optional:: directory
     character(len=*)  ,intent(in),optional:: fichier
     logical             ,intent(in),optional::linit


    
     if ( present(marsgram_per)) atm%marsgram_per(:) = marsgram_per(:)
     if ( present(marsgram_sce)) atm%marsgram_sce    = marsgram_sce
     if ( present(dustOD))       atm%dustOD          = dustOD
     if ( present(rpscale))      atm%rpscale         = rpscale  
     if ( present(directory))    atm%directory       = directory
     if ( present(fichier))      atm%fichier         = fichier
     if ( present(linit))        atm%linit           = linit   
   end subroutine MSP_modifier_atm_marsgram

   subroutine MSP_calculer_atm_marsgram(atm,date_js,R,rlon,phisat,ro,vson,pression,temperature,windm,aziwm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atm_marsgram
!
!$Resume
!	Routine permettant de calculer les caractéristiques du modèle MARSGRAM 2001
!
!$Description
!	Routine permettant de calculer les caractéristiques du modèle MARSGRAM 2001
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atm_marsgram(atm,date_js,R,rlon,phisat,ro,vson,pression,temperature,windm,aziwm)
!.    type (MSP_ATM_MARSGRAM) :: atm 
!.    type (tm_jour_sec) :: date_js 
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
!>E     atm          :<MSP_ATM_MARSGRAM>   structure MARSGRAM
!>E     date_js      :<tm_jour_sec>        
!>E     R            :<PM_REEL>            Distance centre planete - sonde (m)
!>E     rlon         :<PM_REEL>            Longitude (rad)
!>E     phisat       :<PM_REEL>            Latitude (sphérique) (rad)
!>S     ro           :<PM_REEL>            Densite atmospherique (kg/m^3)
!>S     vson         :<PM_REEL>            Vitesse du son (m/s)
!>S     pression     :<PM_REEL>            Pression atmospherique (Pa)
!>S     temperature  :<PM_REEL>            Temperature atmospherique (T)
!>S     windm        :<PM_REEL>            Vitesse du vent en m/s
!>S     aziwm        :<PM_REEL>            Azimut du vent en radians
!
!$Common
!
!$Routines
!- cps_atmmarsgram_2001
!- MSP_signaler_message
!- mu_angle2
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CALCULER ATMOSPHERE MARSGRAM
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !--------------------------------------------------------------------------------
    ! Arguments
    !--------------------------------------------------------------------------------

    ! Entrees
    type (MSP_ATM_MARSGRAM), intent(IN):: atm           ! structure atmosphère
    type (tm_jour_sec),  intent(IN)    :: date_js       ! Date j/s cnes 1950 en TE
    real (KIND=PM_REEL), intent(IN)    ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)    ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)    ::phisat         ! Latitude
    ! Sorties
    real (KIND=PM_REEL), intent(OUT)   :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT)   :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT)   :: pression      ! Pression atmospherique
    real (KIND=PM_REEL), intent(OUT)   :: temperature   ! Temperature atmospherique
    real (KIND=PM_REEL), intent(OUT)   :: windm          ! Vitesse du vent en m/s
    real (KIND=PM_REEL), intent(OUT)   :: aziwm          ! Azimut du vent en radians

    !-----------------------------------------------------------------------------------
    ! Variables locales
    !-----------------------------------------------------------------------------------

    real(kind=PM_REEL)::date_jul
    real(kind=PM_REEL),dimension(5)   ::meanvar
    real(kind=PM_REEL),dimension(25)   ::extvar2
    real (KIND=pm_reel) :: vent_est,vent_nord  ! composantes est,nord
    real(kind=PM_REEL) :: alt_MOLA
    integer ::ier
    real(kind=PM_REEL)::muangl
    type (tm_code_retour) :: code_erreur

    ! Initialisations
    alt_MOLA = 0._pm_reel
    extvar2 = 0._pm_reel
    meanvar = 0._pm_reel
    vent_est = 0._pm_reel
    vent_nord = 0._pm_reel

    ier = 0

    ! Calcul de la date
    date_jul = date_js%jour + date_js%sec/86400._pm_reel + 2433282.5_pm_reel

    ! Calcul de la pression, de ro, de la temperature
    ro = 0._pm_reel
    pression = 0._pm_reel
    temperature = 0._pm_reel
    call cps_atmmarsgram_2001(R,phisat,rlon,date_jul,atm%directory,atm%fichier,atm%linit,&
         atm%marsgram_sce,atm%dustOD,atm%marsgram_per(1),atm%marsgram_per(2),&
         atm%rpscale,alt_MOLA,pression,ro,temperature,vent_est,vent_nord,meanvar,extvar2,ier)
    if ( ier /= 0 ) then
       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_005",routine="MSP_calculer_atm_marsgram")
       return
    endif
    ! Calcul de vson
    if (ro .gt. 0._pm_reel) then
       vson=sqrt(1.3_pm_reel*pression/ro)
    else
       vson=0._pm_reel
    endif

    ! Definition du vent tabule
    windm = sqrt(vent_est*vent_est+vent_nord*vent_nord)
    call mu_angle2(vent_est,vent_nord,MUANGL,code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_calculer_atm_emcd") ) return
    
    aziwm = PM_PI_SUR2 - MUANGL

  end subroutine MSP_calculer_atm_marsgram
end module MSP_ATM_MARSGRAM_DEF
