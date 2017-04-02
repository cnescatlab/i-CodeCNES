module MSP_ACTSOL_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ACTSOL_DEF
!
!$Resume
! Module gérant les données liées à l'activité solaire standard ou réelle.
!
!$Description
! Module gérant les données liées à l'activité solaire standard ou réelle.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_ACTSOL_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_ACTSOL_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.22  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.21  2009/12/11 14:40:26  mercadig
!  VERSION::AQ::11/12/2009:suppression d une trace affichee a la console et ajout de commentaires
!
!  Revision 1.20  2009/12/11 14:21:28  mercadig
!  AQ: Suppression d une trace affichee a la console
!
!  Revision 1.19  2009/11/19 09:42:53  mercadig
!  AQ: Desallocations deplacees dans le cas COMPAS
!
!  Revision 1.18  2009/11/18 20:05:59  kvernelo
!  AQ : correction fuites memoire
!
!  Revision 1.17  2009/11/18 14:19:21  kvernelo
!  DM-ID 842 : correction de deux fuites mémoire
!
!  Revision 1.16  2009/11/09 14:07:44  mercadig
!  DM-ID 842: Gestion du mode d'activite solaire reelle via COMPAS (creation de la structure, calcul activite solaire)
!
!  Revision 1.15  2009/11/03 08:45:32  cmartel
!  DM-ID 842 : Appel des fichiers par leur identifiant en base
!  Revision 1.14  2009/11/02 09:49:54  cmartel
!  DM-ID 842 : Enrichissement de la structure MECASPA et remplacement des fichiers ACTSOL par ACSOL2
!  Revision 1.13  2008/11/12 10:25:08  mercadig
!  DM-ID 733: Mise a jour selon les corrections PSIMU suite a la FA 989
!  Revision 1.12  2008/07/04 15:03:12  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.11  2008/02/22 13:58:09  huec
!  FA-ID 968 : Suppression de l utilisation d un GOTO
!  Revision 1.10  2007/11/05 16:03:52  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.9  2007/10/23 15:02:57  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.8  2007/06/18 10:44:17  vivaresf
!  FA-ID 748 : suppression des dimensions en clair, écriture des chaines avec trim
!  Revision 1.7  2007/06/18 10:14:22  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.6  2006/11/15 10:09:34  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.5  2006/11/09 09:13:51  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.4  2005/03/08 07:32:32  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.3  2005/03/03 12:39:26  vivaresf
!  DM-ID 4 : tableaux dynamique
!  Revision 1.2  2005/03/03 09:12:49  vivaresf
!  DM-ID 4 : tableau dynamique le fichier d'activite solaire
!  Revision 1.1  2005/03/01 10:01:54  pauh
!  DM 111 : ajout de nouveaux modules pour les calculs d atmospheres
!            martiennes
!  Revision 1.1  2005/01/21 14:44:38  rostan
!  DM-ID 111: rapatriement depuis simbad
!  Revision 1.1  2004/04/14 13:57:50  simbad
!  Ajout du vent et de ACTSOL
!  Revision 1.2  2004/01/22 18:27:23  simbad
!  Avant suppression de AMLIB et IOLIB
!  Revision 1.2  2003/01/27 09:31:06  rousseaus
!  version industrialisée CS
!  Revision 1.1  2002/09/17 12:16:11  rousseaus
!  version temporaire de MECASPA spécifique à SIMBAD
!  Revision 1.7  2000/09/05 12:52:03  util_am
!  Passage à 10 véhicules au lieu de 100
!  Revision 1.6  2000/06/21 15:30:49  util_am
!  Gestion des ressources (fichiers de donnees)
!  Revision 1.5  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  1999/12/06 14:40:57  util_am
!  Génération d'un warning quand on dépasse les limites du fichier d'activité solaire
!  Revision 1.3  1999/10/26 10:52:43  util_am
!  Bug dans la lecture du dernier point du fichier d'activité solaire réelle
!  Revision 1.2  1999/08/04 11:28:09  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use MSP_ACTSOL_DEF
!
!$Structure
!
!: MSP_ACTSOL : 
!>     ficactsol     : <LEN=MSP_LONG_NOMFIC>        nom du fichier d'activité solaire
!>     type_actsol   : <integer>                    type d'activité (MSP_ENUM_ACTSOL_REELLE,MSP_ENUM_ACTSOL_STD,MSP_ENUM_ACTSOL_COMPAS)
!>     npsol         : <integer>                    nombre de points lu dans le fichier
!>     isjour        : <integer,DIM=(:),pointer>    
!>     isaa          : <integer,DIM=(:,:),pointer>  
!>     isaam         : <integer,DIM=(:),pointer>    
!>     isap          : <integer,DIM=(:,:),pointer>  
!>     isapm         : <integer,DIM=(:),pointer>    
!>     fpre          : <pm_reel,DIM=(:),pointer>    
!>     fmes          : <pm_reel,DIM=(:),pointer>    
!>     fmoy          : <pm_reel,DIM=(:),pointer>    
!>     faju          : <pm_reel,DIM=(:),pointer>    
!>     flux          : <pm_reel>                    flux solaire du jour précédent (10-22 W/m**2)
!>     app           : <pm_reel,DIM=(7)>            indice géomagnétique
!>     fluxmoy       : <pm_reel>                    flux solaire moyen sur trois mois (10-22 W/m**2)
!>     flag_func     : <logical>                    
!>     date_deb      : <pm_reel>                    
!>     date_fin      : <pm_reel>                    
!>     date_ref      : <integer>                    
!
!$Global
!
!>  nisa                     : <integer,parameter>  
!>  MSP_ENUM_VAL_FLUX        : <PM_REEL,parameter>  
!>  MSP_ENUM_VAL_AP          : <PM_REEL,parameter>  
!>  MSP_ENUM_ACTSOL_REELLE   : <integer,parameter>  activité solaire sur fichier
!>  MSP_ENUM_ACTSOL_STD      : <integer,parameter>  activité solaire standard (valeurs constantes)
!>  MSP_ENUM_ACTSOL_COMPAS   : <integer,parameter>  activité solaire réelle (données COMPAS)
!$Common
!
!$Routines
!- MSP_effacer_actsol
!- MSP_creer_actsol
!- MSP_calculer_actsol
!#V
!- MSP_egaler_actsol
!#
!
!$Fonctions
!- MSPi_lire_acsol2
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_Acces
!- MSP_MECASPA_DEF
!- cps_utilisateur
!- cps_acsol2
!#
!
!$Interface
!> assignment(=) :  MSP_egaler_actsol
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB
  use MSP_GESTION_ERREUR
  use MSP_Acces, only : MSP_ouverture_fichier 
  use MSP_MECASPA_DEF, only : MSP_LONG_CHAINE, MSP_LONG_NOMFIC

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ACTSOL_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


  ! Dimension courante des variables du fichier d'activite solaire
  integer,parameter :: nisa=8

  type MSP_ACTSOL
     ! Données lors de la lecture
     character(LEN=MSP_LONG_NOMFIC)  :: ficactsol
     integer :: type_actsol
     integer :: npsol
     integer, dimension(:),pointer   :: isjour
     integer, dimension(:,:),pointer :: isaa
     integer, dimension(:),pointer   :: isaam
     integer, dimension(:,:),pointer :: isap
     integer, dimension(:),pointer   :: isapm
     real (KIND=pm_reel), dimension(:),pointer :: fpre
     real (KIND=pm_reel), dimension(:),pointer :: fmes
     real (KIND=pm_reel), dimension(:),pointer :: fmoy
     real (KIND=pm_reel), dimension(:),pointer :: faju

     ! Valeurs saisie en mode standard constant
     real (KIND=pm_reel) :: flux
     real (KIND=pm_reel),dimension(7) :: app
     real (KIND=pm_reel) :: fluxmoy

     logical :: flag_func
    
     ! Variables internes
     real (KIND=pm_reel) :: date_deb
     real (KIND=pm_reel) :: date_fin
     integer             :: date_ref
     
  end type MSP_ACTSOL



  interface ASSIGNMENT(=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT(=)
!
!$Resume
!  Cette routine permet d'égaler deux structures activité solaire
!
!$Description
!  Cette routine permet d'égaler deux structures activité solaire
!
!$Acces
!  PUBLIC
!
!$Usage
!  =(str_acta=str_actb)
!.    type(MSP_ACTSOL) :: str_actb
!.    type(MSP_ACTSOL) :: str_acta
!
!$Procedures
!#V
!- MSP_egaler_actsol
!#
!
!$Remarques
!
!$Mots-cles
! EGALER ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_egaler_actsol
  end interface


   ! -- Valeurs par défaut

   ! Valeur par défaut du flux solaire
   real(kind=PM_REEL), parameter :: MSP_ENUM_VAL_FLUX   = 75._pm_reel
   ! Valeur par défaut de l'indice géomagnétique
   real(kind=PM_REEL), parameter :: MSP_ENUM_VAL_AP   = 12._pm_reel


   ! Ancienne activité réelle, maintenant activité sur fichier
   integer, parameter :: MSP_ENUM_ACTSOL_REELLE = 1
   ! Activité standard aka Activité constante
   integer, parameter :: MSP_ENUM_ACTSOL_STD    = 2
   ! Activite réelle en base COMPAS (données prédites et observées)
   integer, parameter :: MSP_ENUM_ACTSOL_COMPAS = 3


   private :: MSP_egaler_actsol

   contains

     subroutine MSP_egaler_actsol(str_acta,str_actb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_actsol
!
!$Resume
!	Routine permettant d'égaler deux structures actsol
!
!$Description
!	Routine permettant d'égaler deux structures actsol
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_actsol(str_acta,str_actb)
!.    type(MSP_ACTSOL) :: str_actb
!.    type(MSP_ACTSOL) :: str_acta
!
!$Arguments
!>E/S   str_acta  :<MSP_ACTSOL>   
!>E     str_actb  :<MSP_ACTSOL>   
!
!$Common
!
!$Routines
!- MSP_effacer_actsol
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! EGALER ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       type(MSP_ACTSOL),intent(IN)    :: str_actb
       type(MSP_ACTSOL),intent(INOUT) :: str_acta

       str_acta%flag_func  = .false.
       call MSP_effacer_actsol(str_acta)

       str_acta%type_actsol= str_actb%type_actsol
       str_acta%ficactsol  = str_actb%ficactsol
       str_acta%npsol      = str_actb%npsol

       if (associated(str_actb%isjour)) then
          allocate(str_acta%isjour(str_actb%npsol))
          str_acta%isjour(:)  = str_actb%isjour(:)
       endif

       if (associated(str_actb%isaa)) then
          allocate(str_acta%isaa(str_actb%npsol,nisa))
          str_acta%isaa(:,:)  = str_actb%isaa(:,:)
       endif
       if (associated(str_actb%isaam)) then
          allocate(str_acta%isaam(str_actb%npsol))
          str_acta%isaam(:)   = str_actb%isaam(:)
       endif
       if (associated(str_actb%isap)) then
          allocate(str_acta%isap(str_actb%npsol,nisa))
          str_acta%isap(:,:)  = str_actb%isap(:,:)
       endif
       if (associated(str_actb%isapm)) then
          allocate(str_acta%isapm(str_actb%npsol))
          str_acta%isapm(:)   = str_actb%isapm(:)
       endif
       if (associated(str_actb%fpre)) then
          allocate(str_acta%fpre(str_actb%npsol))
          str_acta%fpre(:)    = str_actb%fpre(:)
       endif
       if (associated(str_actb%fmes)) then
          allocate(str_acta%fmes(str_actb%npsol))
          str_acta%fmes(:)    = str_actb%fmes(:)
       endif
       if (associated(str_actb%fmoy)) then
          allocate(str_acta%fmoy(str_actb%npsol))
          str_acta%fmoy(:)    = str_actb%fmoy(:)
       endif
       if (associated(str_actb%faju)) then
          allocate(str_acta%faju(str_actb%npsol))
          str_acta%faju(:)    = str_actb%faju(:)
       endif
       str_acta%flux       = str_actb%flux
       str_acta%app(:)     = str_actb%app(:)
       str_acta%fluxmoy    = str_actb%fluxmoy

       str_acta%date_deb   = str_actb%date_deb
       str_acta%date_fin   = str_actb%date_fin
       str_acta%date_ref   = str_actb%date_ref
     end subroutine MSP_egaler_actsol


     subroutine MSP_effacer_actsol(str_act, nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_actsol
!
!$Resume
!	Routine permettant d'initialiser une structure actsol
!
!$Description
!	Routine permettant d'initialiser une structure actsol
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_actsol(str_act, [nul])
!.    type(MSP_ACTSOL) :: str_act
!.    logical :: nul
!
!$Arguments
!>E/S   str_act  :<MSP_ACTSOL>   
!>[E/S] nul      :<logical>      
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
! INITIALISER ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       type(MSP_ACTSOL) :: str_act
       logical, optional :: nul
       ! Variables locales
       integer :: ierr
       logical :: nul_tmp

       ! Initialisation des variables locales
       ierr = 0
       if ( present (nul) ) then
          nul_tmp = nul
       else
          nul_tmp = .false.
       endif

       ! RAZ des variables standards
       str_act%type_actsol = 0
       str_act%ficactsol   = ""
       str_act%npsol       = 0
       str_act%flux        = 0._PM_REEL 
       str_act%app(:)      = 0._PM_REEL
       str_act%fluxmoy     = 0._PM_REEL
       str_act%date_deb    = 0._PM_REEL
       str_act%date_fin    = 0._PM_REEL
       str_act%date_ref    = 0

       ! RAZ des pointeurs
       if ( nul_tmp ) then

          nullify(str_act%isjour)
          nullify(str_act%isaa)
          nullify(str_act%isaam)
          nullify(str_act%isap)
          nullify(str_act%isapm)
          nullify(str_act%fpre)
          nullify(str_act%fmes)
          nullify(str_act%fmoy)
          nullify(str_act%faju)
       else
          if (associated(str_act%isjour)) then
             deallocate(str_act%isjour, stat=ierr)
          endif
          if (associated(str_act%isaa)) then
             deallocate( str_act%isaa, stat=ierr)
          endif
          if (associated(str_act%isaam)) then
             deallocate( str_act%isaam, stat=ierr)
          endif
          if (associated(str_act%isap)) then
             deallocate( str_act%isap, stat=ierr)
          endif
          if (associated(str_act%isapm)) then
             deallocate( str_act%isapm, stat=ierr)
          endif
          if (associated(str_act%fpre)) then
             deallocate( str_act%fpre, stat=ierr)
          endif
          if (associated(str_act%fmes)) then
             deallocate( str_act%fmes, stat=ierr)
          endif
          if (associated(str_act%fmoy)) then
             deallocate( str_act%fmoy, stat=ierr)
          endif
          if (associated(str_act%faju)) then
             deallocate( str_act%faju, stat=ierr)
          endif
       endif


     end subroutine MSP_effacer_actsol

     subroutine MSP_creer_actsol(factsol,flux,fluxmoy,ap,apmoy,type_actsol,&
          str_actsol,dateref_actsol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_actsol
!
!$Resume
!	Routine permettant la création d'une structure actsol
!
!$Description
!	La création d'une structure peut se faire soit :
!		- à partir d'un fichier (activité solaire réelle mode fichier ACSOL2)
!               - par extraction des données COMPAS (activité solaire réelle mode COMPAS)
!		- soit à partir de valeurs constantes
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_creer_actsol([factsol],[flux],[fluxmoy],[ap],[apmoy],[type_actsol],&
!.              str_actsol,[dateref_actsol])
!.    character(LEN=*) :: factsol 
!.    real(kind=pm_reel) :: flux,fluxmoy 
!.    real(kind=pm_reel),dimension(7) :: ap 
!.    real(kind=pm_reel) :: apmoy 
!.    integer :: type_actsol 
!.    integer :: dateref_actsol 
!.    type(MSP_ACTSOL) :: str_actsol
!
!$Arguments
!>[E]   factsol         :<LEN=*>             fichier contenant les valeurs de flux et d'indice géomagnétique
!>[E]   flux            :<pm_reel>           flux solaire du jour précédent (10-22 W/m**2)
!>[E]   fluxmoy         :<pm_reel>           flux solaire moyen sur trois mois (10-22 W/m**2)
!>[E]   ap              :<pm_reel,DIM=(7)>   indice géomagnétique
!.                                  ap(1) : indice tri-horaire journalier
!.                                  ap(2) : indice courant
!.                                  ap(3) : indice des trois heures précédentes
!.                                  ap(4) : indice des six heures précédentes
!.                                  ap(5) : indice des neuf heures précédentes
!.                                  ap(6) : moyenne des 12 a 33 h précédentes
!.                                  ap(7) : moyenne des 36 a 60 h précédentes
!>[E]   apmoy           :<pm_reel>           indice géomagnétique moyen
!>[E]   type_actsol     :<integer>           type d'activité solaire (MSP_ENUM_ACTSOL_REELLE,MSP_ENUM_ACTSOL_STD,MSP_ENUM_ACTSOL_COMPAS)
!>E/S   str_actsol      :<MSP_ACTSOL>        
!>[E]   dateref_actsol  :<integer>           date de référence pour extraire les données observées et prédites dans COMPAS_BASE
!
!$Common
!
!$Routines
!- MSP_effacer_actsol
!- MSP_signaler_message
!- cps_extraireAcsol2Sure
!- cps_extraireAcsol2Pred
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Remarques
!
!$Mots-cles
! CREER ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      use cps_utilisateur
      
      implicit none
      
      ! Entrées
      character(LEN=*),intent(IN),optional                :: factsol      ! fichier de l'activité solaire
      real(kind=pm_reel),intent(in),optional              :: flux,fluxmoy ! flux solaire journalier et moyen
      real(kind=pm_reel),dimension(7),intent(in),optional :: ap           ! indices geomagnetiques
      real(kind=pm_reel),intent(in),optional              :: apmoy        ! indice geomagnetique moyen
      integer,intent(in),optional                         :: type_actsol  ! type d'activité solaire (standard ou réelle)
      integer,intent(in),optional                         :: dateref_actsol ! date de référence

      ! Sortie
      type(MSP_ACTSOL),intent(inout) :: str_actsol

!     Variables locales
      integer :: lnom, ii, jj
      character(LEN=80),dimension(2)       :: tmessage_var
      real(KIND=PM_REEL), dimension(:), pointer :: tab_date_mes => NULL() ! Tableau des dates (données mesurées et prédites)
      real(KIND=PM_REEL), dimension(:), pointer :: tab_date_pre => NULL() 
      real(KIND=PM_REEL), dimension(:,:), pointer  :: tab_data_flux_mes => NULL() ! Tableau des flux (mesuré et moyen)
      real(KIND=PM_REEL), dimension(:,:), pointer  :: tab_data_flux_pre => NULL()
      integer, dimension(:,:), pointer :: tab_data_ia_mes => NULL() ! Tableau des indices géomagnétiques
      integer, dimension(:,:), pointer :: tab_data_ia_pre => NULL() 
      integer :: nb_data_mes, nb_data_pre ! Nombre de données disponibles mesurées et prédites
      integer :: nb_data ! Nombre total de données disponibles 
      real(kind=pm_reel) :: date_fin_mes

      ! Initialisations
      call MSP_effacer_actsol(str_actsol,nul=.true.)

      ! -- Type d'activité solaire (par défaut MSP_ENUM_ACTSOL_STD)
      if (present(type_actsol)) then
         str_actsol%type_actsol = type_actsol
      else
         str_actsol%type_actsol = MSP_ENUM_ACTSOL_STD
      end if
      
      select case (str_actsol%type_actsol)
         case (MSP_ENUM_ACTSOL_REELLE)
            ! -- Fichier d'activité solaire
            ! lecture des coefficients en cas d'activité réelle
            if (present(factsol)) then

               lnom = LEN_TRIM(factsol)
               if ( lnom <= MSP_LONG_NOMFIC ) then 
                  ! Lecture au format ACSOL2
                  str_actsol=MSPi_lire_acsol2 (factsol)
                  if (MSP_gen_messages("MSP_creer_actsol")) return
  ! reecriture du nom de fichier et du type apres effacement par MSP_lire_actsol
                  str_actsol%ficactsol  = factsol
                  str_actsol%type_actsol = MSP_ENUM_ACTSOL_REELLE
               else
                  str_actsol%ficactsol  = factsol(1:MSP_LONG_NOMFIC)
                  tmessage_var(1) = 'Le nom du fichier d''activite solaire'
                  write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC
                  call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
                    routine="MSP_creer_actsol",type=MSP_ENUM_WARNING, &
                    partie_variable=tmessage_var)
               endif
            else
               ! mode par défaut activité solaire standard initialisée par défaut
               str_actsol%ficactsol = ""
               str_actsol%date_deb = 0.0_pm_reel
               str_actsol%date_fin = 0.0_pm_reel
               str_actsol%date_ref = 0
               str_actsol%type_actsol = MSP_ENUM_ACTSOL_STD
               call MSP_signaler_message (&
                    cle_mes="MSP_creer_actsol_000",routine="MSP_creer_actsol")
            endif

        case(MSP_ENUM_ACTSOL_STD)
           ! mode par défaut activité solaire standard
           str_actsol%ficactsol = ""
           str_actsol%date_deb = 0.0_pm_reel
           str_actsol%date_fin = 0.0_pm_reel
           str_actsol%date_ref = 0
           ! Les valeurs sont lues par la suite
	   
        case(MSP_ENUM_ACTSOL_COMPAS)
           ! mode activité solaire réelle
           if ( present(dateref_actsol) ) then

	      ! On extrait les données mesurées disponibles jusqu'à date_fin=date de référence
	      call cps_extraireAcsol2Sure(tab_date_mes,tab_data_flux_mes,tab_data_ia_mes,nb_data_mes,&
	                                  date_fin=real(dateref_actsol,kind=pm_reel))

	      if( nb_data_mes == 0 ) then
                 call MSP_signaler_message (cle_mes="MSP_creer_actsol_001")
                 return
              endif

	      ! On extrait les données prédites disponibles à partir de date_deb=date de référence
	      call cps_extraireAcsol2Pred(tab_date_pre,tab_data_flux_pre,tab_data_ia_pre,nb_data_pre,&
	                                  date_deb=real(dateref_actsol,kind=pm_reel))

	      if( nb_data_pre == 0 ) then
                 call MSP_signaler_message (cle_mes="MSP_creer_actsol_001")
                 return
              endif

              ! Mise à jour de la date de référence dans la structure
              str_actsol%date_ref = dateref_actsol 

           else

	      ! Pas de date de référence
	      ! On extrait toutes les données mesurées disponibles
	      call cps_extraireAcsol2Sure(tab_date_mes,tab_data_flux_mes,tab_data_ia_mes,nb_data_mes)

	      if( nb_data_mes == 0 ) then
                 call MSP_signaler_message (cle_mes="MSP_creer_actsol_001")
                 return
              endif
	      
	      ! On récupère la dernière date des données mesurées extraites
	      date_fin_mes = tab_date_mes(nb_data_mes)

	      call cps_extraireAcsol2Pred(tab_date_pre,tab_data_flux_pre,tab_data_ia_pre,nb_data_pre,&
	                                  date_deb=date_fin_mes)
	      
	      if( nb_data_pre == 0 ) then
                 call MSP_signaler_message (cle_mes="MSP_creer_actsol_001")
                 return
              endif
	      
	      ! Mise à jour de la date de référence dans la structure
              str_actsol%date_ref = -1
	      
           endif
           
     	   ! Stockage des données extraites, dans la structure MECASPA
     	   nb_data = nb_data_mes + nb_data_pre 
     	   
	   ! Nombre de données disponibles
	   str_actsol%npsol = nb_data
	   
     	   ! -> Dates
     	   allocate(str_actsol%isjour(nb_data))
     	   ! Données mesurées
     	   do ii = 1, nb_data_mes
	      str_actsol%isjour(ii) = int(tab_date_mes(ii))
	   enddo
     	   ! Données prédites
     	   do ii = 1, nb_data_pre
	      str_actsol%isjour(ii+nb_data_mes) = int(tab_date_pre(ii))
	   enddo

	   ! -> AA moyen
     	   allocate(str_actsol%isaam(nb_data))
     	   ! Données mesurées
	   do ii = 1, nb_data_mes
	      str_actsol%isaam(ii) = tab_data_ia_mes(ii,1)
	   enddo
     	   ! Données prédites
	   do ii = 1, nb_data_pre
	      str_actsol%isaam(ii+nb_data_mes) = tab_data_ia_pre(ii,1)
	   enddo

     	   ! -> AA
     	   allocate(str_actsol%isaa(nb_data,8))
     	   ! Données mesurées
	   do jj = 1, 8
	      do ii = 1, nb_data_mes
		 str_actsol%isaa(ii, jj) = tab_data_ia_mes(ii,jj+1)
	      enddo
	   enddo
     	   ! Données prédites
	   do jj = 1, 8
	      do ii = 1, nb_data_pre
		 str_actsol%isaa(ii+nb_data_mes, jj) = tab_data_ia_pre(ii,jj+1)
	      enddo
	   enddo
     	   
     	   ! NB : Les AP ne sont pas disponibles dans COMPAS
     	   ! Le calcul de l'activité solaire se fera dans ce cas avec les AA
     	   ! On met à 0 les valeurs des AP et AP moyen de façon à ce que le calcul
     	   ! s'effectue avec les AA
     	   
     	   ! -> AP moyen
	   allocate(str_actsol%isapm(nb_data))
	   do ii = 1, nb_data
	      str_actsol%isapm(ii) = 0
	   enddo
     	   
     	   ! -> AP
	   allocate(str_actsol%isap(nb_data,8))
     	   do jj = 1, 8
	      do ii = 1, nb_data
		 str_actsol%isap(ii,jj) = 0
	      enddo
	   enddo
     	   
     	   ! -> Flux
	   allocate(str_actsol%fmes(nb_data))
     	   ! Données mesurées
	   do ii = 1, nb_data_mes
	      str_actsol%fmes(ii) = tab_data_flux_mes(ii,1)
	   enddo
     	   ! Données prédites
	   do ii = 1, nb_data_pre
	      str_actsol%fmes(ii+nb_data_mes) = tab_data_flux_pre(ii,1)
	   enddo
     	   
     	   ! -> Flux moyen
	   allocate(str_actsol%fmoy(nb_data))
     	   ! Données mesurées
	   do ii = 1, nb_data_mes
	      str_actsol%fmoy(ii) = tab_data_flux_mes(ii,2)
	   enddo
     	   ! Données prédites
	   do ii = 1, nb_data_pre
	      str_actsol%fmoy(ii+nb_data_mes) = tab_data_flux_pre(ii,2)
	   enddo
     	   
     	   ! NB : Les valeurs fpre et faju ne sont pas disponibles dans COMPAS
     	   ! Elles ne sont pas exploitées dans le calcul. On les met à 0.

     	   ! -> fpre
	   allocate(str_actsol%fpre(nb_data))
	   do ii = 1, nb_data
	      str_actsol%fpre(ii) = 0._pm_reel
	   enddo

     	   ! -> faju
	   allocate(str_actsol%faju(nb_data))
	   do ii = 1, nb_data
	      str_actsol%faju(ii) = 0._pm_reel
	   enddo
     	   
	   ! Mise à 0 des autres champs
	   do ii = 1, 7
	      str_actsol%app = 0.0_pm_reel
	   enddo
	   str_actsol%flux	= 0.0_pm_reel
	   str_actsol%fluxmoy	= 0.0_pm_reel
  
	   ! Remplissage des bornes de validité
	   str_actsol%date_deb = str_actsol%isjour(1)
	   str_actsol%date_fin = str_actsol%isjour(nb_data)

           ! Libération mémoire  
	   if (associated(tab_date_mes)) deallocate(tab_date_mes)
	   if (associated(tab_date_pre)) deallocate(tab_date_pre)
	   if (associated(tab_data_flux_mes)) deallocate(tab_data_flux_mes) 
           if (associated(tab_data_flux_pre)) deallocate(tab_data_flux_pre)
	   if (associated(tab_data_ia_mes)) deallocate(tab_data_ia_mes)
	   if (associated(tab_data_ia_pre)) deallocate(tab_data_ia_pre)
	   
        case default 
           call MSP_signaler_message (cle_mes="MSP_CODE_NON_RECONNU", &
              routine="MSP_creer_actsol")
           return 
	   
      end select

      if (present(flux)) then
      ! -- Flux solaire (par défaut 75.0)
         str_actsol%flux = flux
      else
         str_actsol%flux = MSP_ENUM_VAL_FLUX ! valeur par défaut
      end if

     if (present(fluxmoy)) then
        str_actsol%fluxmoy = fluxmoy
     else
        str_actsol%fluxmoy = str_actsol%flux ! valeur par défaut
     end if
 
     if (present(ap)) then
      ! -- Indice géomagnétique (par défaut 12.0)
        str_actsol%app(:) = ap(:)
     else
        str_actsol%app(:) = MSP_ENUM_VAL_AP
     end if
     
     if (present(apmoy)) then
        str_actsol%app(:) = apmoy
     end if

     !Initialisation du flag_func a false car msp_creer est une routine
     str_actsol%flag_func = .false.


   end subroutine MSP_creer_actsol
       

    subroutine MSP_calculer_actsol (str_act,date_js,fljop,flmoy,ap)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_actsol
!
!$Resume
!  Sous-programme déterminant le flux et l'indice géomagnetique réels à une date donnée.
!
!$Description
!  Sous-programme déterminant le flux et l'indice géomagnetique réels à une date donnée.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_actsol (str_act,date_js,fljop,flmoy,ap)
!.    type(MSP_ACTSOL) :: str_act 
!.    type(tm_jour_sec) :: date_js
!.    real (KIND=pm_reel) :: fljop,flmoy,ap(7)
!
!$Arguments
!>E     str_act  :<MSP_ACTSOL>        structure actsol
!>E     date_js  :<tm_jour_sec>       
!>S     fljop    :<pm_reel>           flux solaire du jour précédent (10-22 W/m**2)
!>S     flmoy    :<pm_reel>           flux solaire moyen sur trois mois (10-22 W/m**2)
!>S     ap       :<pm_reel,DIM=(7)>   indices géomagnétiques AP:
!.                                  ap(1) : indice tri-horaire journalier
!.                                  ap(2) : indice courant
!.                                  ap(3) : indice des trois heures précédentes
!.                                  ap(4) : indice des six heures précédentes
!.                                  ap(5) : indice des neuf heures précédentes
!.                                  ap(6) : moyenne des 12 a 33 h précédentes
!.                                  ap(7) : moyenne des 36 a 60 h précédentes
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_ACTSOL),intent(IN)  :: str_act 
      type(tm_jour_sec), intent(IN)  :: date_js
      real (KIND=pm_reel), intent(OUT) :: fljop,flmoy,ap(7)

      real (KIND=pm_reel) :: fljopold,flmoyold,apold(7),heure,date
      integer :: i,ibid,ij,ih,j6,j7,jj,flag_w
      integer :: ith(2,20),idatold,indjold,indh,indhold,indj,idat
      
      
      type(tm_code_retour) :: code_erreur

      logical :: trouve

      data idatold,indjold,indhold,indj,flag_w /-1,-1,-1,2,0/

      save indj,idatold,indjold,indhold,fljopold,flmoyold,apold,flag_w

      ! Conversion Jours/Sec -> jours juliens
      call md_joursec_jourfrac(date_js,date,code_erreur)


      select case (str_act%type_actsol)

      case (MSP_ENUM_ACTSOL_STD)
         ! cas activité standard
         fljop = str_act%flux
         flmoy = str_act%fluxmoy
         ap(:) = str_act%app(:)
      case (MSP_ENUM_ACTSOL_REELLE, MSP_ENUM_ACTSOL_COMPAS)
	       
	 ! Test sur la date pour le mode COMPAS
	 ! Si la date est en dehors de l'intervalle de validité des données, alors
	 ! on remonte un warning utilisateur et on prend les valeurs à la date de début
	 ! ou de fin	 
	 if (str_act%type_actsol == MSP_ENUM_ACTSOL_COMPAS) then	 
	    if (date < str_act%date_deb) then       
	       call MSP_signaler_message (cle_mes="MSP_CALC_ACTSOL_002")
	       ! Les premières valeurs de l'activité solaire seront prises 
	       date = str_act%date_deb
	    endif
	    if (date > str_act%date_fin) then       
	       call MSP_signaler_message (cle_mes="MSP_CALC_ACTSOL_002")
	       date = str_act%date_fin
	    endif
	 endif

         !   ********************************************************************
         !   * Jour julien                                                      *
         !   ********************************************************************
         
         idat = int(date)

         !   ********************************************************************
         !   * Recherche de l'indice correspondant a idat                       *
         !   ********************************************************************
        
         if ( idat == idatold ) then
            !   * Cas ou idat est le meme que precedemment:
            indj = indjold  
         else if ( idat < str_act%isjour(1) ) then
            !   * Cas ou idat est < str_act%isjour(1)
            indj = 1 + (idat - str_act%isjour(1))         
	 else if ( idat > str_act%isjour(str_act%npsol)) then

            !* Cas ou idat est >= str_act%isjour(str_act%npsol)
            indj = str_act%npsol + (idat - str_act%isjour(str_act%npsol)) 
         else
            
	    !* Cas où il faut rechercher l'indice dans le fichier
            i = 1
            trouve = .false.
            do while (i <= str_act%npsol .and. (.not.trouve))
            
               if ( idat < str_act%isjour(i) ) then
                  indj = i-1
                  trouve = .true.
               endif
               i = i + 1
            enddo             
         endif

         !   ********************************************************************
         !   * Indices tri-horaire jusqu'a 60 heures avant                      *
         !   ********************************************************************
         
         heure = ( date-real(idat,kind=pm_reel) )*24._pm_reel
         indh = 1 + int(heure/3._pm_reel)
	 
         if (indh > 8) indh=8
           
         ! Comparaison de l'indice journalier (n° de la ligne contenant la date courante)
         ! puis de l'indice tri-horaire
         ! --> (1) si les indices sont égaux aux anciennes valeurs, on se contente de
         ! rendre les anciens résultats.
         ! --> (2) sinon, on recalcule les nouvelles valeurs.      
         if ( (indj == indjold) .and. (indh == indhold) ) then
            ! Cas (1) : les indices sont les mêmes, on rend les valeurs de flux et
            ! d'ap directement
	    	 
            fljop = fljopold
            flmoy = flmoyold
            do i = 1,7
               ap(i) = apold(i)
            enddo
            
         else
            ! Cas (2) : les indices sont différents, on recalcule les valeurs de flux
            ! et d'ap
         
            !   * Renumerotation:
	    
            ! On calcule des indices de 1 à 20, pour couvrir 20 périodes
            ! de 3h qui précédent la date courante.
         
            do i = 1 , 20
            
               ibid = indh - (20-i)
            
               if (ibid >= 1) then
                  ij = indj
                  ih = ibid
               else if (ibid >= -7) then
                  ij = indj - 1
                  ih = ibid + 8
               else if (ibid >= -15) then
                  ij = indj - 2
                  ih = ibid + 16
               else
                  ij = indj - 3
                  ih = ibid + 24
               endif
	       
               ! ith(1,:) : indices des jours (n° de la ligne dans le fichier)
               ! ith(2,:) : indices des heures (indice tri-horaire, compris entre 1 et 8)
               if (ij < 1) then
	          ! Si l'indice de la ligne dans le fichier est < 1
                  ! on émet un warning (positionnement du flag à une valeur < 0)
                  ! et on prend la 1ère valeur du fichier
                  flag_w = min(flag_w,0) - 1
                  ith(1,i) = 1
                  ith(2,i) = 1
               else if (ij > str_act%npsol) then
	          ! De même, si le n° de ligne dépasse le nb max de lignes
                  ! du fichier, on émet un warning 
                  ! et on prend la dernière valeur du fichier (dernier jour, dernières 3 h)
                  flag_w = max(flag_w,0) + 1
                  ith(1,i) = str_act%npsol
                  ith(2,i) = 8
               else
                  ith(1,i) = ij
                  ith(2,i) = ih
               endif

               ! Warning si on dépasse les dates min/max du fichier d'activité solaire:
            
               if ( flag_w == -1 ) then
                  call MSP_signaler_message (cle_mes="MSP_CALC_ACTSOL_001")
               else if ( flag_w == 1 ) then
                  call MSP_signaler_message (cle_mes="MSP_CALC_ACTSOL_001")
               endif

            enddo

         !   ********************************************************************
         !   * Calcul des ap                                                    *
         !   ********************************************************************

            ap(6) = 0._pm_reel
            ap(7) = 0._pm_reel

         !   * Cas ou les AP sont non nuls:

            if (str_act%isapm(ith(1,20)) /= 0) then 
	    
               ap(1) = real(str_act%isapm(ith(1,20)),KIND=pm_reel)
               ap(2) = real(str_act%isap(ith(1,20),ith(2,20)),KIND=pm_reel)
               ap(3) = real(str_act%isap(ith(1,19),ith(2,19)),KIND=pm_reel)
               ap(4) = real(str_act%isap(ith(1,18),ith(2,18)),KIND=pm_reel)
               ap(5) = real(str_act%isap(ith(1,17),ith(2,17)),KIND=pm_reel)
               do i = 1,8
                  j6 = 17 - i
                  ap(6) = ap(6) + real(str_act%isap(ith(1,j6),ith(2,j6)),KIND=pm_reel)
                  j7 = 9 - i
                  ap(7) = ap(7) + real(str_act%isap(ith(1,j7),ith(2,j7)),KIND=pm_reel)
               enddo

            !   * Sinon on prend les AA ...

            else 

	       ap(1) = real(str_act%isaam(ith(1,20)),KIND=pm_reel)
               ap(2) = real(str_act%isaa(ith(1,20),ith(2,20)),KIND=pm_reel)
               ap(3) = real(str_act%isaa(ith(1,19),ith(2,19)),KIND=pm_reel)
               ap(4) = real(str_act%isaa(ith(1,18),ith(2,18)),KIND=pm_reel)
               ap(5) = real(str_act%isaa(ith(1,17),ith(2,17)),KIND=pm_reel)
               do i = 1,8
                  j6 = 17 - i
                  ap(6) = ap(6) + real(str_act%isaa(ith(1,j6),ith(2,j6)),KIND=pm_reel)
                  j7 = 9 - i
                  ap(7) = ap(7) + real(str_act%isaa(ith(1,j7),ith(2,j7)),KIND=pm_reel)
               enddo
            
            endif
         
            ap(6) = ap(6) / 8
            ap(7) = ap(7) / 8

            do i = 1,7
               apold(i) = ap(i)
            enddo

         !   ********************************************************************
         !   * Calcul des flux                                                  *
         !   ********************************************************************
         
            if ( indj == indjold ) then
               ! Les indices tri-horaires étaient différents mais les indices journaliers
               ! sont les mêmes : on se contente de rendre les anciennes valeurs
             
               fljop = fljopold
               flmoy = flmoyold

            else

               !      * Flux du jour precedent:
               ! jj devient l'indice de la veille du jour courant
            
               jj = ith(1,20) - 1
	    
	       ! on traite les bornes inférieures et supérieures du fichier
               if ( jj < 1 ) then
                  jj = 1
               else if ( ij > str_act%npsol ) then
                  jj = str_act%npsol
               endif

               fljop = str_act%fmes(jj)

               !      * Flux moyen:

               jj = ith(1,20)
               if ( jj < 1 ) then
                  jj = 1
               else if ( ij > str_act%npsol ) then
                  jj = str_act%npsol
               endif
               flmoy = str_act%fmoy(jj)
            
               fljopold = fljop
               flmoyold = flmoy

            endif

         end if
	
         ! Sauvegarde des variables "save"        
         idatold = idat
         indjold = indj
         indhold = indh
	 
      end select
      
    end subroutine MSP_calculer_actsol


    function MSPi_lire_acsol2 (fichier) result (str_act)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSPi_lire_acsol2
!
!$Resume
!	Lecture des coefficients de flux et d'indice géomagnétique à partir 
!   d'un fichier ACSOL2
!
!$Description
!	Lecture des coefficients de flux et d'indice géomagnétique à partir 
!   d'un fichier ACSOL2
!
!$Auteur
!   Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  str_act = MSPi_lire_acsol2 (fichier)
!.    character(LEN=*) :: fichier
!.    type(MSP_ACTSOL) :: str_act
!
!$Arguments
!>E     fichier  :<LEN=*>        fichier contenant les valeurs    
!>S     str_act  :<MSP_ACTSOL>   structure de données MECASPA
!
!$Common
!
!$Routines
!- MSP_effacer_actsol
!- MSP_signaler_message
!- cps_lireAcsol2
!
!$Include
!
!$Module
!#V
!- cps_acsol2
!- cps_utilisateur
!#
!
!$Remarques
!
!$Mots-cles
! LIRE ACTIVITE SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acsol2
   use cps_utilisateur

   implicit none

   ! Entrées / sorties
   character(LEN=*), intent(IN) :: fichier
   type(MSP_ACTSOL) :: str_act

   ! Variables locales
   integer :: ii,jj
   ! Variables nécessaires à la lecture COMPAS
   integer :: nb_lignes, trouve
   character(LEN=MSP_LONG_NOMFIC) :: path_fichier
   type(ligne_acsol2), dimension(:), pointer :: tab_lignes
      
   ! initialisation de la structure actsol
   call MSP_effacer_actsol(str_act,nul=.true.)
   
   ! Appel à la routine COMPAS d'extraction du path du fichier
   trouve = cps_getFichierAcsol2(fichier, path_fichier, rep=.true.)
   if ( MSP_gen_messages("MSPi_lire_acsol2")) return
   if  ( trouve /= CPS_OK ) then
      call MSP_signaler_message (cle_mes="MSP_ACTSOL_COMPAS_001")
      return
   endif

   ! Appel à la lecture COMPAS des fichiers ACSOL2
   call cps_lireAcsol2(path_fichier, tab_lignes, nb_lignes)
   if (MSP_gen_messages("MSPi_lire_acsol2")) return
   ! Les fichiers sont stockées lignes par lignes
   ! On les veux colonnes par colonnes, i.e. par thème

   ! Transformation de la structure
   str_act%ficactsol = trim(fichier)
   str_act%type_actsol = MSP_ENUM_ACTSOL_REELLE
   str_act%npsol = nb_lignes

   ! Recopie des dates
   allocate(str_act%isjour(nb_lignes))
   do ii = 1, nb_lignes
      str_act%isjour(ii) = tab_lignes(ii)%julsol
   enddo

   ! Chmaps isaa et isaamoy
   allocate(str_act%isaa(nb_lignes, 8))
      do jj = 1, 8
   do ii = 1, nb_lignes
         str_act%isaa(ii, jj) = tab_lignes(ii)%iaa(jj)
      enddo
   enddo
   
   allocate(str_act%isaam(nb_lignes))
   do ii = 1, nb_lignes
      str_act%isaam(ii) = tab_lignes(ii)%iaamoy
   enddo

   ! Champs isap et isapm
   allocate(str_act%isap(nb_lignes, 8))
      do jj = 1, 8
   do ii = 1, nb_lignes
         str_act%isap(ii, jj) = tab_lignes(ii)%iap(jj)
      enddo
   enddo
   
   allocate(str_act%isapm(nb_lignes))
   do ii = 1, nb_lignes
      str_act%isapm(ii) = tab_lignes(ii)%iapmoy
   enddo

   ! Champs des flux

   allocate(str_act%fmes(nb_lignes))
   do ii = 1, nb_lignes
      str_act%fmes(ii) = tab_lignes(ii)%flux
   enddo

   allocate(str_act%fpre(nb_lignes))
   do ii = 1, nb_lignes
      str_act%fpre(ii) = tab_lignes(ii)%fluxpr
   enddo

   allocate(str_act%fmoy(nb_lignes))
   do ii = 1, nb_lignes
      str_act%fmoy(ii) = tab_lignes(ii)%fluxm
   enddo

   allocate(str_act%faju(nb_lignes))
   do ii = 1, nb_lignes
      str_act%faju(ii) = tab_lignes(ii)%fluxa
   enddo

   ! Mise a zero des autres champs
   do ii = 1, 7
      str_act%app    = 0.0_pm_reel
   enddo
   str_act%flux      = 0.0_pm_reel
   str_act%fluxmoy   = 0.0_pm_reel
   str_act%flag_func = .false.
     
   ! Remplissages des bornes de validité
   str_act%date_deb = tab_lignes(1)%julsol
   str_act%date_fin = tab_lignes(nb_lignes)%julsol
   ! Pas de date de réference (date utilisée par l'activité réelle au sens COMPAS)
   str_act%date_ref = 0
   str_act%flag_func=.true.
   deallocate(tab_lignes)
end function MSPi_lire_acsol2


end module MSP_ACTSOL_DEF
