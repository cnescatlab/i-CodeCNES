module cps_desc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_desc
!
!$Resume
!  Il s'agit du module gérant la configuration de la base locale et de la base de reference.
!
!$Description
!  Ce module lit le fichier de configuration de la base locale si elle existe, et le fichier de
!  configuration de la base de reference. Il construit à partir de là les descriptions internes
!  des fichiers geres par la base.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: cps_desc.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: cps_desc.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.39  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.38  2009/03/11 16:52:31  cml
!  AQ : Correction d'orthographe dans un message d'erreur
!
!  Revision 1.37  2009/01/06 10:52:10  cml
!  FA-ID 1151 : Correction orthographe
!
!  Revision 1.36  2008/10/28 16:40:55  cml
!  FA-ID 1061 : Amelioration des tests sur les fichiers de configuration des bases
!
!  Revision 1.35  2008/10/28 13:06:04  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.34  2008/10/28 12:42:19  tanguyy
!  DM-ID 1058 : codage de la fonction cpsi_size_ptr pour encapsuler l'appel a 'size' et obtenir un comportement coherent entre Sun et Linux
!
!  Revision 1.33  2008/10/07 08:11:43  cml
!  FA-ID 1062 : Ajout d une limite sur le nombre de champs du fichier de config
!  Revision 1.32  2008/10/06 16:12:23  cml
!  FA-ID 1062 : Ajout d une limite sur le nombre de champs du fichier de config
!  Revision 1.31  2008/10/06 15:14:06  cml
!  FA-ID 1061 : Ajout de traitement de cas d erreur d initialisation
!  Revision 1.30  2008/10/03 13:32:16  cml
!  FA-ID 1024 : Remplacement de la variable index par indice
!  Revision 1.29  2008/10/02 09:22:53  cml
!  AQ : Remise en place d un message qui avait ete supprime
!  Revision 1.28  2008/10/01 17:39:06  tanguyy
!  DM-ID 1058 / AQ : suppression des boucles infinies et controles sur la gestion de la memoire lors des allocate et deallocate
!  Revision 1.27  2008/08/04 13:05:22  gss
!  DM-ID 1058 : Suppression des variables non utilisées.
!  Initialisation des sorties de fonction dépacée avant le premier return.
!  Revision 1.26  2008/07/04 11:43:35  huec
!  DM-ID 1058 : Ameliorations de la gestion memoire, initialisations de variables
!  Revision 1.25  2008/05/02 15:44:36  vivaresf
!  FA-ID 892 : test que la base locale est non vide
!  Revision 1.24  2008/04/24 15:47:03  vivaresf
!  DM-ID 11 : validation
!  Revision 1.23  2008/03/21 15:07:19  vivaresf
!  DM-ID 11 : initialisation des messages d'erreur pour eph_posvit3corps
!  Revision 1.22  2008/02/29 16:55:28  vivaresf
!  FA-ID 892/893 : initialisations des pointeurs et des valeurs
!  allouées dans les variables pointées
!  Revision 1.21  2008/02/11 08:45:43  huec
!  AQ : Suppression d une ligne de code commentee
!  Revision 1.20  2007/06/15 14:59:51  bibms
!  FA-ID 746 : mise en inout de desc_copy pour permettre sa desallocation
!  Revision 1.19  2007/06/07 08:45:19  vivaresf
!  Erreur si cps_init appelé hors cadre COMPAS : pas de valeurs par défaut
!  Revision 1.18  2007/05/21 06:56:20  vivaresf
!  Version 2.2 : révision des cartouches
!  Revision 1.17  2006/11/09 14:37:27  vivaresf
!  DM-ID 425 : affichage normalisé des rééls
!  Revision 1.16  2006/10/26 17:17:36  vpg
!  DM-ID 462 : amélioration de l'accès à la description du contenu de la base de données COMPAS
!  Revision 1.15  2006/10/18 09:52:48  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.14.2.1  2006/09/26 12:13:31  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.14  2006/05/30 15:19:36  vivaresf
!  Regle de codage : supression de svariable sinutilisées
!  Revision 1.13  2006/05/30 12:29:03  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.12  2006/05/30 08:27:46  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.11  2006/05/15 15:06:39  vpg
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.10  2006/05/02 09:37:16  vpg
!  Diminution de la complexite de certaines fonctions pour respecter les seuils des metriques
!  Revision 1.9  2006/04/12 09:20:05  vpg
!  Correction d'un test d'egalite sur des chaines de caracteres dans la fonction cpsi_existeAttDesc()
!  Revision 1.8  2006/03/20 15:52:44  vpg
!  Mise a jour des cartouches
!  Revision 1.7  2006/02/06 15:16:18  vpg
!  Modification de la routine de lecture d'une description : on fournit le repertoire de la base. Ajout de la possibilite de charger une base locale sans tenir compte du compas.rc
!  Revision 1.6  2006/01/23 14:05:16  vpg
!  rajout des traitements MAGE, rajout de fonctions liees aux ihm
!  Revision 1.5  2005/12/08 18:20:31  vivaresf
!  Cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_desc
!
!$Structure
!
!: cpsi_desc : 
!>     local         : <integer>                         
!>     categorie     : <LEN=CPS_MAXLG>                   
!>     id            : <LEN=??>                          
!>     fichier       : <LEN=??>                          
!>     nbChamps      : <integer>                         
!>     infosChamps   : <cpsi_infoChamp,DIM=(:),pointer>  
!
!: cpsi_infoChamp : 
!>     nom           : <LEN=??>                          
!>     type          : <integer>                         
!>     vd_i          : <integer>                         
!>     vd_d          : <real>                            
!>     vd_s          : <LEN=??>                          
!>     unite         : <LEN=CPS_MAXLG>                   
!
!$Global
!
!>  listFichiersLocal          : <cpsi_desc,DIM=(:),pointer,public>      
!>  listFichiersRef            : <cpsi_desc,DIM=(:),pointer,public>      
!>  listIdLocal                : <LEN=CPS_MAXLG,DIM=(:),pointer,public>  
!>  listIdRef                  : <LEN=CPS_MAXLG,DIM=(:),pointer,public>  
!>  fichier_conf_local         : <LEN=CPS_MAXLG,public>                  
!>  rep_base_local             : <LEN=CPS_MAXLG,public>                  
!>  fichier_conf_ref           : <LEN=CPS_MAXLG,public>                  
!>  rep_base_ref               : <LEN=CPS_MAXLG,public>                  
!>  rep_cps_fcf                : <LEN=CPS_MAXLG,public>                  
!>  rep_mage_fcf               : <LEN=CPS_MAXLG,public>                  
!>  cps_base_locale_chargee    : <logical,public>                        
!#V
!>  cps_desc_init              : <logical,private>                       
!>  NB_MAX_ACCES_MADONA        : <LEN=19,parameter,private>              
!>  FICHIERS                   : <LEN=8,parameter,private>               
!>  UNITES_MADONA              : <LEN=14,parameter,private>              
!>  CPS_ERREURS_MSG            : <LEN=15,parameter,private>              
!>  DESC_CATEGORIE             : <LEN=9,parameter,private>               
!>  DESC_ID                    : <LEN=2,parameter,private>               
!>  DESC_FICHIER               : <LEN=7,parameter,private>               
!>  DESC_NB_CHAMPS             : <LEN=9,parameter,private>               
!>  DESC_INFOS_CHAMPS          : <LEN=12,parameter,private>              
!>  INFO_CHAMP_NOM             : <LEN=3,parameter,private>               
!>  INFO_CHAMP_TYPE            : <LEN=11,parameter,private>              
!>  INFO_CHAMP_UNITE           : <LEN=5,parameter,private>               
!>  INFO_CHAMP_VAL_DEF         : <LEN=13,parameter,private>              
!>  CPS_ID                     : <LEN=6,parameter,private>               
!>  CPS_NB_MAX_CHAMPS_CONFIG   : <integer,parameter,private>             
!>  nb_max_acces_madona_ok     : <logical,private>                       
!>  unites_madona_ok           : <logical,private>                       
!>  erreurs_msg_ok             : <logical,private>                       
!>  mage_msg_ok                : <logical,private>                       
!>  cps_id_local_ok            : <logical,private>                       
!>  cps_id_ref_ok              : <logical,private>                       
!>  fichier_unites             : <LEN=CPS_MAXLG,private>                 
!>  fichier_erreurs            : <LEN=CPS_MAXLG,private>                 
!>  fichier_mage               : <LEN=CPS_MAXLG,private>                 
!>  ret                        : <integer,private>                       
!>  iostat                     : <integer,private>                       
!#
!$Common
!
!$Routines
!- cpsi_size_ptr
!- cpsi_init_desc
!- cpsi_init_baseLocale
!- cpsi_init_baseRef
!- cpsi_lireDescription
!- afficherListCorps
!- afficherListTheories
!- afficherDesc
!- afficherInfoChamp
!- cpsi_copyDesc
!- cpsi_read_cps_id
!- cpsi_close_desc
!#V
!- cpsi_lireFichiers
!#
!
!$Fonctions
!- cpsi_size_ptr_entier
!- cpsi_size_ptr_reel
!- cpsi_size_ptr_chaine
!- cpsi_size_ptr_cpsi_desc
!- cpsi_size_ptr_cpsi_infoChamp
!- cpsi_getDescFichier
!- cpsi_existeAttDesc
!
!$Include
!
!$Module
!#V
!- mslib
!- cps_accesMadona
!#
!
!$Interface
!> cpsi_size_ptr :  cpsi_size_ptr_entier, cpsi_size_ptr_reel, 
!                   cpsi_size_ptr_chaine, cpsi_size_ptr_cpsi_desc, 
!                   cpsi_size_ptr_cpsi_infoChamp
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  cpsi_lireFichiers
!#
!.  cpsi_size_ptr_entier cpsi_size_ptr_reel cpsi_size_ptr_chaine cpsi_size_ptr_cpsi_desc
!.  cpsi_size_ptr_cpsi_infoChamp cpsi_getDescFichier cpsi_existeAttDesc cpsi_size_ptr cpsi_init_desc
!.  cpsi_init_baseLocale cpsi_init_baseRef cpsi_lireDescription afficherListCorps afficherListTheories
!.  afficherDesc afficherInfoChamp cpsi_copyDesc cpsi_read_cps_id cpsi_close_desc
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use mslib
  use cps_accesMadona

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_desc.F90 355 2013-02-14 12:16:41Z aadt $'


  type cpsi_infoChamp
     character(LEN=CPS_MAXLG) :: nom
     integer :: type
     integer :: vd_i
     real(kind=PM_REEL) :: vd_d
     character(LEN=CPS_MAXLG) :: vd_s
     character(LEN=CPS_MAXLG) :: unite
  end type cpsi_infoChamp

  type cpsi_desc
     integer :: local ! 1=oui, 0=non
     character(LEN=CPS_MAXLG) :: categorie
     character(LEN=CPS_MAXLG) :: id
     character(LEN=CPS_MAXLG) :: fichier
     integer :: nbChamps
     type(cpsi_infoChamp), dimension(:), pointer :: infosChamps => NULL()
  end type cpsi_desc

  logical, private, save :: cps_desc_init = .false.

  character(LEN=19), parameter, private :: NB_MAX_ACCES_MADONA = 'nb_max_acces_madona'
  character(LEN=8), parameter, private :: FICHIERS = 'fichiers'
  character(LEN=14), parameter, private :: UNITES_MADONA = 'fichier_unites'
  character(LEN=15), parameter, private :: CPS_ERREURS_MSG = 'cps_erreurs_msg'
  character(LEN=9), parameter, private :: DESC_CATEGORIE = 'categorie'
  character(LEN=2), parameter, private :: DESC_ID = 'id'
  character(LEN=7), parameter, private :: DESC_FICHIER = 'fichier'
  character(LEN=9), parameter, private :: DESC_NB_CHAMPS = 'nb_champs'
  character(LEN=12), parameter, private :: DESC_INFOS_CHAMPS = 'infos_champs'
  character(LEN=3), parameter, private :: INFO_CHAMP_NOM = 'nom'
  character(LEN=11), parameter, private :: INFO_CHAMP_TYPE = 'type_donnee'
  character(LEN=5), parameter, private :: INFO_CHAMP_UNITE = 'unite'
  character(LEN=13), parameter, private :: INFO_CHAMP_VAL_DEF = 'valeur_defaut'
  character(LEN=6), parameter, private :: CPS_ID = 'cps_id'

  ! Nombre maximum de champs dans les fichiers de configuration
  integer, parameter, private :: CPS_NB_MAX_CHAMPS_CONFIG = 10

  logical, private :: nb_max_acces_madona_ok, unites_madona_ok
  logical, save, private :: erreurs_msg_ok = .false.
  logical, save, private :: mage_msg_ok = .false.
  logical, private :: cps_id_local_ok = .false.
  logical, private :: cps_id_ref_ok = .false.

  ! liste des descriptions des fichiers de la base locale
  type(cpsi_desc), dimension(:), pointer, public :: listFichiersLocal => NULL()
  ! liste des descriptions des fichiers de la bas de reference 
  type(cpsi_desc), dimension(:), pointer, public :: listFichiersRef => NULL()

  ! liste des id de la base locale de COMPAS utilises pour la restriction des fichiers
  character(LEN=CPS_MAXLG), dimension(:), pointer, public :: listIdLocal
  ! liste des id de la base de reference de COMPAS utilises pour la restriction des fichiers
  character(LEN=CPS_MAXLG), dimension(:), pointer, public :: listIdRef

  character(LEN=CPS_MAXLG), public :: fichier_conf_local
  character(LEN=CPS_MAXLG), public :: rep_base_local
  character(LEN=CPS_MAXLG), public :: fichier_conf_ref
  character(LEN=CPS_MAXLG), public :: rep_base_ref
  character(LEN=CPS_MAXLG), public :: rep_cps_fcf
  character(LEN=CPS_MAXLG), public :: rep_mage_fcf
  character(LEN=CPS_MAXLG), private :: fichier_unites
  character(LEN=CPS_MAXLG), private :: fichier_erreurs
  character(LEN=CPS_MAXLG), private :: fichier_mage
  
  logical, public :: cps_base_locale_chargee = .false.

  integer, private :: ret

  ! Variables de robustesse (status de desallocation), interne
  integer, private :: iostat

  ! routines internes, locale au module 
  private :: cpsi_lireFichiers

  interface cpsi_size_ptr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr
!
!$Resume
!  Fonction de test d'un pointeur sur tableau
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon.
!$Acces
!  PUBLIC
!
!$Usage
!.      function cpsi_size_ptr(ptr)
!.    integer,dimension(:),pointer :: ptr
!
!.     function cpsi_size_ptr(ptr)
!.    real,dimension(:),pointer :: ptr
!
!.      function cpsi_size_ptr(ptr)
!.    character(len=*),dimension(:),pointer :: ptr
!
!.      function cpsi_size_ptr(ptr)
!.    type(cpsi_desc),dimension(:),pointer :: ptr
!
!.     function cpsi_size_ptr(ptr)
!.    type(cpsi_infoChamp),dimension(:),pointer :: ptr
!
!$Procedures
!- cpsi_size_ptr_entier
!- cpsi_size_ptr_reel
!- cpsi_size_ptr_chaine
!- cpsi_size_ptr_cpsi_desc
!- cpsi_size_ptr_cpsi_infoChamp
!
!$Remarques
!  L'interface cpsi_size_ptr est déclinée en plusieurs fonctions selon le type de pointeur passé
!  en paramètre. L'utilisation de cpsi_size_ptr est généralisée dans tout les modules COMPAS, sauf les modèles 
!  d'atmosphère.
!  Si un nouveau type de données doit être testé de cette manière, il suffit d'enrichir l'interface.
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     module procedure cpsi_size_ptr_entier
     module procedure cpsi_size_ptr_reel
     module procedure cpsi_size_ptr_chaine
     module procedure cpsi_size_ptr_cpsi_desc
     module procedure cpsi_size_ptr_cpsi_infoChamp
           
  end interface


contains
  

  function cpsi_size_ptr_entier(ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr_entier
!
!$Resume
!  Fonction de test d'un pointeur sur tableau d'entiers
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon.
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!.      function cpsi_size_ptr_entier(ptr)
!.    integer,dimension(:),pointer :: ptr
!
!$Arguments
!>E/S   ptr  :<integer,DIM=(:),pointer>   Pointeur sur un tableau d'entiers (alloué ou non)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Argument
    integer,dimension(:),pointer :: ptr
    ! Sortie : taille du pointeur, ou 0
    integer :: cpsi_size_ptr_entier
    
    ! Code
    if(associated(ptr)) then
       cpsi_size_ptr_entier = size(ptr)
    else
       cpsi_size_ptr_entier = 0
    end if

    return

  end function cpsi_size_ptr_entier

 function cpsi_size_ptr_reel(ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr_reel
!
!$Resume
!  Fonction de test d'un pointeur sur tableau de réels
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon.
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!.     function cpsi_size_ptr_reel(ptr)
!.    real,dimension(:),pointer :: ptr
!
!$Arguments
!>E/S   ptr  :<real,DIM=(:),pointer>   Pointeur sur un tableau de réels (alloué ou non)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Argument
    real(kind=pm_reel),dimension(:),pointer :: ptr
    ! Sortie : taille du pointeur, ou 0
    integer :: cpsi_size_ptr_reel
    
    ! Code
    if(associated(ptr)) then
       cpsi_size_ptr_reel = size(ptr)
    else
       cpsi_size_ptr_reel = 0
    end if

    return

  end function cpsi_size_ptr_reel

  function cpsi_size_ptr_chaine(ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr_chaine
!
!$Resume
!  Fonction de test d'un pointeur sur tableau d'entiers
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon.
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!.      function cpsi_size_ptr_chaine(ptr)
!.    character(len=*),dimension(:),pointer :: ptr
!
!$Arguments
!>E/S   ptr  :<LEN=*,DIM=(:),pointer>   Pointeur sur un tableau de chaines de caractères (alloué ou non)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Argument
    character(len=*),dimension(:),pointer :: ptr
    ! Sortie : taille du pointeur, ou 0
    integer :: cpsi_size_ptr_chaine
    
    ! Code
    if(associated(ptr)) then
       cpsi_size_ptr_chaine = size(ptr)
    else
       cpsi_size_ptr_chaine = 0
    end if

    return

  end function cpsi_size_ptr_chaine
 
  function cpsi_size_ptr_cpsi_desc(ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr_cpsi_desc
!
!$Resume
!  Fonction de test d'un pointeur sur tableau de structures "cpsi_desc"
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon.
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!.      function cpsi_size_ptr_cpsi_desc(ptr)
!.    type(cpsi_desc),dimension(:),pointer :: ptr
!
!$Arguments
!>E/S   ptr  :<cpsi_desc,DIM=(:),pointer>   Pointeur sur un tableau de structures cpsi_desc (alloué ou non)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Argument
    type(cpsi_desc),dimension(:),pointer :: ptr
    ! Sortie : taille du pointeur, ou 0
    integer :: cpsi_size_ptr_cpsi_desc
    
    ! Code
    if(associated(ptr)) then
       cpsi_size_ptr_cpsi_desc = size(ptr)
    else
       cpsi_size_ptr_cpsi_desc = 0
    end if

    return

  end function cpsi_size_ptr_cpsi_desc

 function cpsi_size_ptr_cpsi_infoChamp(ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_size_ptr_cpsi_infoChamp
!
!$Resume
!  Fonction de test d'un pointeur sur tableau de structures cpsi_infoChamp
!$Description
!  Fonction de test d'un pointeur sur tableau. : rend la taille du tableau s'il est alloué,
!  et 0 sinon
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!.     function cpsi_size_ptr_cpsi_infoChamp(ptr)
!.    type(cpsi_infoChamp),dimension(:),pointer :: ptr
!
!$Arguments
!>E/S   ptr  :<cpsi_infoChamp,DIM=(:),pointer>   Pointeur sur un tableau de structures cpsi_infoChamp (alloué ou non)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Argument
    type(cpsi_infoChamp),dimension(:),pointer :: ptr
    ! Sortie : taille du pointeur, ou 0
    integer :: cpsi_size_ptr_cpsi_infoChamp
    
    ! Code
    if(associated(ptr)) then
       cpsi_size_ptr_cpsi_infoChamp = size(ptr)
    else
       cpsi_size_ptr_cpsi_infoChamp = 0
    end if

    return

  end function cpsi_size_ptr_cpsi_infoChamp

  subroutine cpsi_init_desc()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_init_desc
!
!$Resume
! Initialisation du module.
!
!$Description
! Initialisation du module, ainsi que du module cps_accesMadona.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_init_desc()
!
!$Arguments
!
!$Common
!
!$Routines
!- cpsi_close_desc
!- MSP_signaler_message
!- MSP_ajouter_fichier_message
!- cpsi_init_baseLocale
!- cpsi_init_baseRef
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! variables locales
    integer :: acces_local, acces_ref
    integer ::lfichier_conf, lrep_base_ref,     &
         lrep_cps_fcf, lrep_mage_fcf, lrep_base_local
    character(LEN=CPS_MAXLG) :: fichier_conf ! Nom generique du fihier de conf
    character(LEN=CPS_MAXLG) :: ficunit 
    logical :: fic_existe  ! Resultats des tests sur fichier

    ! initialisations
    ficunit=""
    fichier_conf_local = "NULL"
    
    ret = acc_err_setmode(ACC_ERR_MEMO)
    
    if (cps_desc_init) then
       call cpsi_close_desc()
    end if
    
    nb_max_acces_madona_ok = .false.
    unites_madona_ok = .false.
    cps_id_local_ok = .false.
    cps_id_ref_ok = .false.
    cps_desc_init = .false.

    ! Note de developpement : 
    !  A ce moment de l'execution le fichier des erreurs n'est pas encore chargé.
    !  C'est pour cela que l'on utilise "CPS_ERR_NEXIST" défini "à la main" 

    ! --------------------------------
    ! Chargement du fichier des erreurs de COMPAS
    ret = AMv_rc_get("compas2_fcf", "compas", "", "", &
         rep_cps_fcf, lrep_cps_fcf)
    if ( ret < 0 .or. lrep_cps_fcf <= 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
            message="Le fichier de ressources compas.rc est absent du MADONA_RC_PATH (ou ne contient pas le champ compas2_fcf)", &
            routine="cpsi_init_desc", &
            type = MSP_ENUM_ERREUR)
       return
    endif
    ! fichiers erreurs
    if (ret >= 0 .and. .not. erreurs_msg_ok .and. lrep_cps_fcf>0 ) then

       ! Ajout du fichier d'erreur COMPAS
       fichier_erreurs = trim(rep_cps_fcf)//"/CPS_erreur_fr"
       ! Test de la présence et de l'accès en lecture du fichier
       inquire (FILE=trim(fichier_erreurs), EXIST=fic_existe)
       if ( fic_existe) then
          call MSP_ajouter_fichier_message(fichier_erreurs)
       else
          call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
             message="Le fichier de description des erreurs CPS_erreur_fr n'a pu être chargé (compas2_fcf incorrect)", &
             routine="cpsi_init_desc", &
             type = MSP_ENUM_ERREUR)
          return
       endif   

       ! Ajout du fichier d'erreur de la LIBEPHEM
       fichier_erreurs = trim(rep_cps_fcf)//"/EPH_erreur_fr"
       inquire (FILE=trim(fichier_erreurs), EXIST=fic_existe)
       if ( fic_existe) then
          call MSP_ajouter_fichier_message(fichier_erreurs)
       else
          call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
             message="Le fichier de description des erreurs EPHEM_erreur_fr n'a pu être chargé (compas2_fcf incorrect)", &
             routine="cpsi_init_desc", &
             type = MSP_ENUM_ERREUR)
          return
       endif   

       ! Flag mis a jour pour que l'init ne se fasse qu'une seule fois 
       erreurs_msg_ok = .true.
    end if
    
    ! --------------------------------
    ! Chargement du fichier des erreurs de MAGE
    ret = AMv_rc_get("mage_fcf", "compas", "", "", &
         rep_mage_fcf, lrep_mage_fcf)
    if ( ret < 0 .or. lrep_mage_fcf <= 0 ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
            message="Le fichier de description des erreurs n'a pu être chargé (mage_fcf inexistant ou incorrect)", &
            routine="cpsi_init_desc", &
            type = MSP_ENUM_ERREUR)
       return
    endif

    ! fichiers erreurs 
    if (ret >= 0 .and. .not. mage_msg_ok .and. lrep_mage_fcf>0) then
       fichier_mage = trim(rep_mage_fcf)//"/MAG_MESSAGES"
       inquire (FILE=trim(fichier_mage), EXIST=fic_existe)
       if ( fic_existe) then
          call MSP_ajouter_fichier_message(fichier_mage)
       else
          call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
             message="Le fichier de description des erreurs MAG_MESSAGES n'a pu être chargé (mage_fcf incorrect)", &
             routine="cpsi_init_desc", &
             type = MSP_ENUM_ERREUR)
          return
       endif

       ! Flag mis a jour pour que l'init ne se fasse qu'une seule fois 
       mage_msg_ok = .true.
    end if
        
    ! -------------------------------
    ! Extraction du chemin d'accès a la base de référence
    ret = AMv_rc_get("DB_COMPAS_REF", "compas", "", "", &
         rep_base_ref, lrep_base_ref)
    ! Traitement de l'erreur
    if (ret < 0 .or. lrep_base_ref <= 0) then
       ! erreur d'ouverture du fichier de configuration
       call MSP_signaler_message(cle_mes="CPS_ERR_NEXIST", &
            message="Pas de base COMPAS, ouverture impossible", &
            routine="cpsi_init_desc", &
             type = MSP_ENUM_ERREUR)
       ! Note dév : Le fichier des erreurs n'est pas encopre chargé.
       !   C'est pour cela que le code "CPS_ERR_NEXIST" n'est pas dans CPS_ERREUR
       return
    endif

    ! -------------------------------
    ! Fichier de configuration COMPAS
    ret = AMv_rc_get("DB_COMPAS_CONFIG", "compas", "", &
         "config", &
         fichier_conf, lfichier_conf)
    ! Pas de traitement d'erreur, par défaut le fichier "config" sera utilisé

    ! Construction du chemin d'acces au fichier de config
    fichier_conf_ref = trim(rep_base_ref)//"/"//trim(fichier_conf)
    ! Verification de la présence du fichier
    inquire(FILE=fichier_conf_ref, EXIST=fic_existe)
    if ( .not. fic_existe ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
          routine="cpsi_init_desc", &
          partie_variable=trim(fichier_conf_ref))
       return
    endif

    ! -------------------------------
    ! Fichier de configuration de la base locale, si elle existe
    ! repertoire de la base locale, si elle existe, et si la variable
    ! cps_base_locale_chargee est false.
    ! cps_base_locale_chargee est positionne a true par la routine
    ! cps_setBaseLocale du module cps_utilisateur.
    if (.not.cps_base_locale_chargee) then
       ! Recherche dans COMPAS.RC utilisation de NULL par défaut
       ret = AMv_rc_get("DB_COMPAS_LOCAL", "compas", "", &
            "NULL", &
            rep_base_local, lrep_base_local)
       ! Vu que l'on utilise NULL, pas besoin de traiter l'absence du champ
    end if

    ! ------------------------------------
    ! lecture du fichier de configuration de la base locale SI elle existe 
    if (trim(rep_base_local).ne."NULL") then

       ! On construit un nom potentiel a partir de config
       fichier_conf_local = trim(rep_base_local)//"/"//trim(fichier_conf)

       ! une base locale est utilisee
       acces_local = acc_load(fichier_conf_local)
       if (acces_local < 0) then
          ! erreur d'ouverture du fichier de configuration
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_init_desc", &
               partie_variable=trim(fichier_conf_local))
          ! Erreur blocante : la base peut ne pas exister, 
          ! Mais si elle est spécifiée elle doit être correcte
          return
       else
          ! Init de la base locale
          call cpsi_init_baseLocale(acces_local, ficunit)
          ! Remontée d'une éventuelle erreur
          if ( MSP_gen_messages("cpsi_init_desc") )return

          ret = acc_close(acces_local)
       end if
    end if

    ! ------------------------------------
    ! lecture du fichier de configuration de reference
    acces_ref = acc_load(fichier_conf_ref)
    if (acces_ref < 0) then
       ! erreur d'ouverture du fichier de configuration
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
            routine="cpsi_init_desc", &
            partie_variable=trim(fichier_conf_ref))
       return
    end if

    ! Init de la base de référence
    call cpsi_init_baseRef(acces_ref)
    ! Remontée d'une éventuelle erreur
    if ( MSP_gen_messages("cpsi_init_desc") )return

    ! Fermeture du fichier et mise a jour du flag d'initialisation
    ret = acc_close(acces_ref)
    cps_desc_init = .true.
    
  end subroutine cpsi_init_desc
  
  
  ! routine interne : initialisation de la base locale
  subroutine cpsi_init_baseLocale(acces_local, ficunit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_init_baseLocale
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_init_baseLocale(acces_local, ficunit)
!.    integer :: acces_local
!.    character(LEN=*) :: ficunit
!
!$Arguments
!>E     acces_local  :<integer>   
!>S     ficunit      :<LEN=*>     
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_init_accesMadona
!- cpsi_read_cps_id
!#V
!- cpsi_lireFichiers
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments 
    integer,intent(in) :: acces_local
    character(LEN=*), intent(out) :: ficunit

    ! Structure pour message d'erreur
    character(LEN=CPS_MAXLG), dimension(3) :: msp_mess 
    
    ! variables locales
    integer :: nature, n
    character(LEN=256) :: libelle
    integer :: cpt_champs    ! Compteur des champs présents
    logical :: fin_fichier   ! Flag indiquant que la fin du fichier est atteinte

    ! Initialisation de la condition de boucle
    ! -> initialisation à une valeur < 0
    ! la sortie de boucle se fait sur la valeur
    ! CPS_FIN_SEQ = 0 
    nature = CPS_ERR_DEF
    cpt_champs = 0
    fin_fichier = .false.
    do while (.not. fin_fichier &
         .and. cpt_champs <= CPS_NB_MAX_CHAMPS_CONFIG)
       ! Extraction du libelle
       ret = acc_scan(acces_local, libelle, nature)
       ! Pas d'erreur possible

       ! Traitement du champ lu
       select case (libelle)
       case (NB_MAX_ACCES_MADONA)
          ret = acc_geti(acces_local, libelle, n)
          ! Test du retour
          if ( ret < 0 ) then
             write(msp_mess(1),*) ret
             write(msp_mess(2),*) libelle
             write(msp_mess(3),*) "configuration de la base locale"
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_init_baseLocale", &
                  partie_variable=msp_mess)
             return
          endif 
          call cpsi_init_accesMadona(n)
          nb_max_acces_madona_ok = .true.

       case (UNITES_MADONA)
          ficunit = trim(fichier_unites)
          ret = acc_gets(acces_local, libelle, fichier_unites)
          ! Test du retour
          if ( ret < 0 ) then
             write(msp_mess(1),*) ret
             write(msp_mess(2),*) libelle
             write(msp_mess(3),*) "configuration de la base locale"
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_init_baseLocale", &
                  partie_variable=msp_mess)
             return
          endif 

          ! Création du chemin d'accès au fichier des unites
          fichier_unites = trim(rep_base_ref)//"/"//trim(fichier_unites)
          ! Test si le fichier est éjà chargé
          ret = acc_unit_loaded()
          if (trim(ficunit).ne.trim(fichier_unites)) ret=0
          if (ret.eq.1) then
             ! un fichier d'unite a deja ete charge
             ret = AMv_unit_unload()
          end if
          ! Chargement du fichier des unités
          ret = acc_unit_load(fichier_unites)
          if ( ret < 0 ) then
             call MSP_signaler_message(cle_mes="CPS_ERR_FIC_UNIT", &
                  routine="cpsi_init_baseLocale")             
             return
          endif
          unites_madona_ok = .true.

       case (CPS_ID)
          call cpsi_read_cps_id(acces_local, listIdLocal)
          cps_id_local_ok = .true.

       case (FICHIERS)
          call cpsi_lireFichiers(acces_local, 1)

       end select
       
       if ( nature == CPS_FIN_SEQ) then
          ! La fin du fichier est atteinte
          fin_fichier = .true.
       else
          ! Incrément du compteur
          cpt_champs = cpt_champs +1          
       endif

    end do
  
    ! Traitement du cas ou le fichier de configuration
    ! comporte plus que "CPS_NB_MAX_CHAMPS_CONFIG" champs
    ! La lecture n'a pas été achevée
    if ( cpt_champs > CPS_NB_MAX_CHAMPS_CONFIG ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_LECT_BASE", &
            routine="cpsi_init_baseLocale")       
    endif

  end subroutine cpsi_init_baseLocale


  ! routine interne : initialisation de la base de référence
  subroutine cpsi_init_baseRef(acces_ref)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_init_baseRef
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_init_baseRef(acces_ref)
!.    integer :: acces_ref
!
!$Arguments
!>E     acces_ref  :<integer>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_init_accesMadona
!- cpsi_read_cps_id
!#V
!- cpsi_lireFichiers
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    integer, intent(in) :: acces_ref
    
    ! variables locales
    character(LEN=256) :: libelle
    integer :: nature, n
    ! Structure pour message d'erreur
    character(LEN=CPS_MAXLG), dimension(3) :: msp_mess 
    integer :: cpt_champs ! Compteur des champs présents
    logical :: fin_fichier   ! Flag indiquant que la fin du fichier est atteinte


    ! Initialisation de la condition de boucle
    ! -> initialisation à une valeur < 0
    ! la sortie de boucle se fait sur la valeur
    ! CPS_FIN_SEQ = 0 
    nature = CPS_ERR_DEF

    cpt_champs = 0
    fin_fichier = .false.
    ! Tant que l'on a pas atteint la fin du fichier ou le nombre de champs max
    do while (.not. fin_fichier &
         .and. cpt_champs <= CPS_NB_MAX_CHAMPS_CONFIG)

       ! Lecture du champs courant
       ret = acc_scan(acces_ref, libelle, nature)
       ! Pas d'erreur possible

       ! Traitement du champ lu
       select case (libelle)
       case (NB_MAX_ACCES_MADONA)
          if (.not.nb_max_acces_madona_ok) then
             ret = acc_geti(acces_ref, libelle, n)
             ! Test du retour
             if ( ret < 0 ) then
                write(msp_mess(1),*) ret
                write(msp_mess(2),*) libelle
                write(msp_mess(3),*) "configuration de la base de réf."
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_init_baseRef", &
                  partie_variable=msp_mess)
                return
             endif
             call cpsi_init_accesMadona(n)
             nb_max_acces_madona_ok = .true.
          end if

       case (UNITES_MADONA)
          if (.not.unites_madona_ok) then
             ret = acc_gets(acces_ref, libelle, fichier_unites)
             ! Test du retour
             if ( ret < 0 ) then
                write(msp_mess(1),*) ret
                write(msp_mess(2),*) libelle
                write(msp_mess(3),*) "configuration de la base de réf."
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                  routine="cpsi_init_baseRef", &
                  partie_variable=msp_mess)
                return
             endif
             fichier_unites = trim(rep_base_ref)//"/"//trim(fichier_unites)
             ret = acc_unit_loaded()
             if (ret.eq.1) then
                ! un fichier d'unite a deja ete charge
                ret = AMv_unit_unload()
             end if
             
             ret = acc_unit_load(fichier_unites)
             if ( ret < 0 ) then
                call MSP_signaler_message(cle_mes="CPS_ERR_FIC_UNIT", &
                  routine="cpsi_init_baseRef")             
                return
             endif

             unites_madona_ok = .true.
          end if

       case (CPS_ID)
          call cpsi_read_cps_id(acces_ref, listIdRef)
          cps_id_ref_ok = .true.

       case (FICHIERS)
          call cpsi_lireFichiers(acces_ref, 0)

       end select

       if ( nature == CPS_FIN_SEQ) then
          ! La fin du fichier est atteinte
          fin_fichier = .true.
       else
          ! Incrément du compteur
          cpt_champs = cpt_champs +1          
       endif

    end do
      
    ! Traitement du cas ou le fichier de configuration
    ! comporte plus que "CPS_NB_MAX_CHAMPS_CONFIG" champs
    ! La lecture n'a pas été complète
    if ( cpt_champs > CPS_NB_MAX_CHAMPS_CONFIG ) then
       call MSP_signaler_message(cle_mes="CPS_ERR_LECT_BASE", &
            routine="cpsi_init_baseRef")       
    endif

  end subroutine cpsi_init_baseRef
  


  subroutine cpsi_lireFichiers(acces, local)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lireFichiers
!
!$Resume
! Lecture du tableau fichiers d'un fichier de configuration.
!
!$Description
! Lecture du tableau fichiers d'un fichier de configuration (base de reference ou base locale).
! Le tableau 'fichiers' doit avoir été sélectionné avant.
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_lireFichiers(acces, local)
!.    integer :: acces
!.    integer :: local
!
!$Arguments
!>E     acces  :<integer>   acces MADONA ouvert sur un fichier de  configuration d'une base
!>E     local  :<integer>   0 s'il s'agit de la base de référence, 1 s'il s'agit d'une base locale
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_lireDescription
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    integer, intent(in) :: acces
    integer, intent(in) :: local
    
    ! variables locales
    integer :: i, iostat
    integer :: nbFichiers
    
    ! début du code
    iostat=0

    nbFichiers = acc_get_dim(acces, FICHIERS)
    if (nbFichiers<=0) return

    if (local.eq.1) then
       if(associated(listFichiersLocal)) deallocate(listFichiersLocal,stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
               routine="cpsi_lire_fichiers")
          return
       end if
       allocate(listFichiersLocal(nbFichiers),stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
               routine="cpsi_lire_fichiers")
          return
       end if
       do i=1,nbFichiers
          nullify(listFichiersLocal(i)%infosChamps)
       end do
    else
       if(associated(listFichiersRef)) deallocate(listFichiersRef,stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
               routine="cpsi_lire_fichiers")
          return
       end if
       allocate(listFichiersRef(nbFichiers),stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
               routine="cpsi_lire_fichiers")
          return
       end if
       do i=1,nbFichiers
          nullify(listFichiersRef(i)%infosChamps)
       end do
    end if
    ret = acc_select(acces, FICHIERS, ACC_TABL)
    do i = 1, nbFichiers
       ret = acc_set_index(acces, i)
       ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
       if (local.eq.1) then
          call cpsi_lireDescription(acces, trim(rep_base_local),   &
               listFichiersLocal(i))
       else
          call cpsi_lireDescription(acces, trim(rep_base_ref),     &
               listFichiersRef(i))
       end if
       ret = acc_select_end(acces)
       
    end do
    
    ret = acc_select_end(acces)
    
  end subroutine cpsi_lireFichiers
  
  
  
  subroutine cpsi_lireDescription(acces, rep_base, desc)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lireDescription
!
!$Resume
! Lecture d'une structure qui est element du tableau 'fichiers' d'un fichier
! de configuration. La structure doit avoir été sélectionnée avant.
!
!$Description
! Lecture d'une structure qui est element du tableau 'fichiers' d'un fichier
! de configuration. Une telle structure correspond à la description d'un fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_lireDescription(acces, rep_base, desc)
!.    integer :: acces
!.    character(LEN=*) :: rep_base
!.    type(cpsi_desc) :: desc
!
!$Arguments
!>E     acces     :<integer>     accès MADONA ouvert sur le fichier de configuration
!>E     rep_base  :<LEN=*>       repertoire de la base
!>E/S   desc      :<cpsi_desc>   description du fichier courant
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: rep_base
    type(cpsi_desc), intent(inout) :: desc
    
    ! variables locales
    integer :: i, local
    character(LEN=CPS_MAXLG) :: fic
    
    if (trim(rep_base).eq.trim(rep_base_ref)) then
       local = 0
    else
       local = 1
    end if
    desc%local = local
    ret = acc_gets(acces, DESC_CATEGORIE, desc%categorie)
    ret = acc_gets(acces, DESC_ID, desc%id)
    ret = acc_gets(acces, DESC_FICHIER, fic)
    desc%fichier = trim(rep_base)//"/"//trim(fic)
        
    ret = acc_geti(acces, DESC_NB_CHAMPS, desc%nbChamps)
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    if (iostat < 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
            routine="cpsi_lire_Description")
       return
    end if

    allocate(desc%infosChamps(desc%nbChamps), stat=iostat)
    if (iostat < 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
            routine="cpsi_lire_Description")
       return
    end if

    desc%infosChamps(:)%nom=""
    desc%infosChamps(:)%type=0
    desc%infosChamps(:)%vd_i=0
    desc%infosChamps(:)%unite=""
    desc%infosChamps(:)%vd_d=0._pm_reel
    desc%infosChamps(:)%vd_s = ""
    ret = acc_select(acces, DESC_INFOS_CHAMPS, ACC_TABL)

    do i = 1, desc%nbChamps
       ret = acc_set_index(acces, i)
       ret = acc_select(acces, ACC_INDEX, ACC_STRUCT)
       ret = acc_gets(acces, INFO_CHAMP_NOM, desc%infosChamps(i)%nom)
       ret = acc_geti(acces, INFO_CHAMP_TYPE, desc%infosChamps(i)%type)
       select case (desc%infosChamps(i)%type)
       case (CPS_ENTIER)
          ret = acc_geti(acces, INFO_CHAMP_VAL_DEF, desc%infosChamps(i)%vd_i)
          desc%infosChamps(i)%vd_d = 0.0_pm_reel
          desc%infosChamps(i)%vd_s = ""
          desc%infosChamps(i)%unite = ""
       case (CPS_REEL)
          ret = acc_gets(acces, INFO_CHAMP_UNITE, desc%infosChamps(i)%unite)
          ret = acc_getd(acces, INFO_CHAMP_VAL_DEF, desc%infosChamps(i)%vd_d, desc%infosChamps(i)%unite)
          desc%infosChamps(i)%vd_i = 0
          desc%infosChamps(i)%vd_s = ""
       case (CPS_STRING)
          ret = acc_gets(acces, INFO_CHAMP_VAL_DEF, desc%infosChamps(i)%vd_s)
          desc%infosChamps(i)%vd_i = 0
          desc%infosChamps(i)%vd_d = 0.0
          desc%infosChamps(i)%unite = ""
       end select
       ret = acc_select_end(acces)
    end do
    ret = acc_select_end(acces)
    
  end subroutine cpsi_lireDescription
  

  subroutine afficherListCorps()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficherListCorps
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call afficherListCorps()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- afficherDesc
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer :: i

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="afficherListCorps", &
            partie_variable="cps_desc")
       return
    end if

    if (associated(listFichiersLocal)) then
       do i=1, cpsi_size_ptr(listFichiersLocal)
          if (listFichiersLocal(i)%categorie.eq.CPS_CATEG_CORPS) then
             call afficherDesc(listFichiersLocal(i))
          end if
       end do
    end if

    if (associated(listFichiersRef)) then
       do i=1, cpsi_size_ptr(listFichiersRef)
          if (listFichiersRef(i)%categorie.eq.CPS_CATEG_CORPS) then
             call afficherDesc(listFichiersRef(i))
          end if
       end do
    end if

  end subroutine afficherListCorps

  subroutine afficherListTheories()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficherListTheories
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call afficherListTheories()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- afficherDesc
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    integer :: i

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="affichierListTheories", &
            partie_variable="cps_desc")
       return
    end if

    if (associated(listFichiersLocal)) then
       do i=1, cpsi_size_ptr(listFichiersLocal)
          if (listFichiersLocal(i)%categorie.eq.CPS_CATEG_THEORIE) then
             call afficherDesc(listFichiersLocal(i))
          end if
       end do
    end if

    do i=1, cpsi_size_ptr(listFichiersRef)
       if (listFichiersRef(i)%categorie.eq.CPS_CATEG_THEORIE) then
          call afficherDesc(listFichiersRef(i))
       end if
    end do
    
  end subroutine afficherListTheories


  subroutine afficherDesc(desc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficherDesc
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call afficherDesc(desc)
!.    type(cpsi_desc) :: desc
!
!$Arguments
!>E     desc  :<cpsi_desc>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- afficherInfoChamp
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(cpsi_desc), intent(in) :: desc
    integer :: i

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="afficherDesc", &
            partie_variable="cps_desc")
       return
    end if

    !local fichier id nbChamps
    if (desc%local.eq.1) then
       write(*,*) "local"
    else
       write(*,*) "reference"
    end if
    write(*,*) "categorie"
    write(*,*) trim(desc%categorie)
    write(*,*) "id"
    write(*,*) trim(desc%id)
    write(*,*) "fichier"
    write(*,*) trim(desc%fichier)
    write(*,*) "nbChamps"
    write(*,*) desc%nbChamps
    
    write(*,*) "infosChamps"
    do i=1, cpsi_size_ptr(desc%infosChamps)
       call afficherInfoChamp(desc%infosChamps(i))
    end do
    write(*,*) "fin infosChamps"
  end subroutine afficherDesc

  subroutine afficherInfoChamp(infoChamp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficherInfoChamp
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call afficherInfoChamp(infoChamp)
!.    type(cpsi_infoChamp) :: infoChamp
!
!$Arguments
!>E     infoChamp  :<cpsi_infoChamp>   
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(cpsi_infoChamp), intent(in) :: infoChamp

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="afficherInfoChamp", &
            partie_variable="cps_desc")
       return
    end if

    write(*,*) "nom"
    write(*,*) trim(infoChamp%nom)
    write(*,*) "type"
    write(*,*) infoChamp%type
    
    write(*,*) "valeur par defaut"
    select case (infoChamp%type)
    case (CPS_ENTIER)
       write(*,*) infoChamp%vd_i
    case (CPS_REEL)
       write(*,'(e23.15)') infoChamp%vd_d
       write(*,*) "unite"
       write(*,*) trim(infoChamp%unite)
    case (CPS_STRING)
       write(*,*) trim(infoChamp%vd_s)
    end select
    
  end subroutine afficherInfoChamp


  function cpsi_getDescFichier(nom_fichier, desc) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getDescFichier
!
!$Resume
! Renvoie la description correspondant a un fichier.
!
!$Description
! Renvoie la description correspondant a un fichier.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getDescFichier(nom_fichier, desc)
!.    character(LEN=*) :: nom_fichier
!.    type(cpsi_desc) :: desc
!.    integer :: trouve
!
!$Arguments
!>E     nom_fichier  :<LEN=*>       nom du fichier
!>S     desc         :<cpsi_desc>   description du fichier
!>S     trouve       :<integer>     CPS_OK si la description est trouvée, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_copyDesc
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_fichier
    type(cpsi_desc), intent(out) :: desc
    
    ! variables locales
    integer :: trouve, i

    ! Initialisation
    trouve = CPS_ERR_DEF

    ! Initialisation de desc
    desc%local=0
    desc%categorie=""
    desc%id=""
    desc%fichier=""
    desc%nbChamps=0
    nullify(desc%infosChamps)

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getDescFichier", &
            partie_variable="cps_desc")
       return
    end if

    if (associated(listFichiersLocal)) then
       do i=1, cpsi_size_ptr(listFichiersLocal)
          if ((listFichiersLocal(i)%fichier.eq.nom_fichier).and.&
               (trouve.ne.CPS_OK)) then
             trouve = CPS_OK
             call cpsi_copyDesc(listFichiersLocal(i), desc)
             exit
          endif
       end do
    endif

    if (trouve.ne.CPS_OK) then
       do i=1, cpsi_size_ptr(listFichiersRef)
          if ((listFichiersRef(i)%fichier.eq.nom_fichier).and.&
               (trouve.ne.CPS_OK)) then
             trouve = CPS_OK
             call cpsi_copyDesc(listFichiersRef(i), desc)
             exit
          endif
       end do
    end if
    
    if (trouve.ne.CPS_OK) then
       ! aucune description trouvee pour le fichier demande
       call MSP_signaler_message(cle_mes="CPS_ERR_DESC", &
            routine="cpsi_getDescFichier", &
            partie_variable=trim(nom_fichier))
    end if

  end function cpsi_getDescFichier

  subroutine cpsi_copyDesc(desc_source, desc_copy)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_copyDesc
!
!$Resume
! Renvoie une copie d'une description d'un fichier.
!
!$Description
! Renvoie une copie d'une description d'un fichier. Cette routine est surement
! obsolete. A l'origine, listFichiersLocal et listFichiersRef etaient une liste chainee
! dont les elements (de type cpsi_desc) etaient des pointeurs.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_copyDesc(desc_source, desc_copy)
!.    type(cpsi_desc) :: desc_source
!.    type(cpsi_desc) :: desc_copy
!
!$Arguments
!>E     desc_source  :<cpsi_desc>   description à copier
!>E/S   desc_copy    :<cpsi_desc>   copie de la description
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    type(cpsi_desc), intent(in) :: desc_source
    type(cpsi_desc), intent(inout) :: desc_copy

    ! variable locale
    integer :: ii

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_copyDesc", &
            partie_variable="cps_desc")
       return
    end if

    desc_copy%local = desc_source%local
    desc_copy%categorie = desc_source%categorie
    desc_copy%id = desc_source%id
    desc_copy%fichier = desc_source%fichier
    desc_copy%nbChamps = desc_source%nbChamps
    
    if (associated(desc_copy%infosChamps)) then
       deallocate(desc_copy%infosChamps, stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
            routine="cpsi_copyDesc")
          return
       end if
    end if

    allocate(desc_copy%infosChamps(desc_copy%nbChamps),stat=iostat)
    if (iostat < 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
            routine="cpsi_copyDesc")
       return
    end if

    do ii=1, desc_copy%nbChamps
       desc_copy%infosChamps(ii)%nom = trim(desc_source%infosChamps(ii)%nom)
       desc_copy%infosChamps(ii)%type = desc_source%infosChamps(ii)%type
       desc_copy%infosChamps(ii)%vd_i = 0
       desc_copy%infosChamps(ii)%vd_d = 0.0
       desc_copy%infosChamps(ii)%vd_s = ""
       desc_copy%infosChamps(ii)%unite = ""
       select case (desc_copy%infosChamps(ii)%type)
          case (CPS_ENTIER)
             desc_copy%infosChamps(ii)%vd_i = desc_source%infosChamps(ii)%vd_i
          case (CPS_REEL)
             desc_copy%infosChamps(ii)%vd_d = desc_source%infosChamps(ii)%vd_d
             desc_copy%infosChamps(ii)%unite = trim(desc_source%infosChamps(ii)%unite)
          case (CPS_STRING)
             desc_copy%infosChamps(ii)%vd_s = trim(desc_source%infosChamps(ii)%vd_s)
       end select
    end do

  end subroutine cpsi_copyDesc


  function cpsi_existeAttDesc(nom_att, desc, indice) result (existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_existeAttDesc
!
!$Resume
! Teste si un attribut appartient a un fichier.
!
!$Description
! Cette fonction teste si un attribut appartient aux structures presentes dans le
! fichier associé à la description.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_existeAttDesc(nom_att, desc, [indice])
!.    character(LEN=*) :: nom_att
!.    type(cpsi_desc) :: desc
!.    integer :: indice
!.    logical :: existe
!
!$Arguments
!>E     nom_att  :<LEN=*>       nom de l'attribut
!>E/S   desc     :<cpsi_desc>   description dans laquelle effectuer la recherche
!>[S]   indice   :<integer>     numéro de l'attribut dans la description
!>S     existe   :<logical>     .true. si l'attribut existe, .false. sinon
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    type(cpsi_desc) :: desc
    integer, optional, intent(out) :: indice

    ! resultat
    logical :: existe

    ! variable locale
    integer ::i

    ! Initialisation
    existe = .false.

    if (.not.cps_desc_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_existeAttDesc", &
            partie_variable="cps_desc")
       return
    end if

    if (present(indice)) then
       indice = 0
    end if
    
    do i=1, desc%nbChamps
       if (trim(desc%infosChamps(i)%nom).eq.trim(nom_att)) then
          existe = .true.
          if (present(indice)) then
             indice = i
          end if
          exit
       end if
    end do

  end function cpsi_existeAttDesc

  
  subroutine cpsi_read_cps_id(acces, listId)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_read_cps_id
!
!$Resume
!
!$Description
! V2.0
! Lit les id (cps_id) du fichier de configuration
! ouvert par 'acces' ete contruit le tableau correspondant.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_read_cps_id(acces, listId)
!.    integer :: acces
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listId
!
!$Arguments
!>E     acces   :<integer>                         acces MADONA ouvert sur le fichier de configuration
!>E/S   listId  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste de ids contenus dans le fichier de configuration
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer, intent(in) :: acces
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listId

    integer :: nb, i
    
    nb = acc_get_dim(acces, "cps_id")
    if (nb.lt.0) then
       ! erreur
       
    else
       if (associated(listId)) then
          deallocate(listId, stat=iostat)
       end if
       allocate(listId(nb))
       ret = acc_select(acces, "cps_id", ACC_TABL)
       do i=1, nb
          ret = acc_set_index(acces, i)
          ret = acc_gets(acces, ACC_INDEX, listId(i))
       end do
       ret = acc_select_end(acces)
    end if
    
  end subroutine cpsi_read_cps_id
  

  subroutine cpsi_close_desc()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_close_desc
!
!$Resume
! Ferme le module.
!
!$Description
! Libere la memoire allouee par le module, ainsi que par le module cps_accesMadona.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_close_desc()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_close_accesMadona
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! variable locale
    integer :: ii
    
    if (cps_desc_init) then
       if (associated(listFichiersLocal)) then
          do ii=1, cpsi_size_ptr(listFichiersLocal)
             if (associated(listFichiersLocal(ii)%infosChamps)) then
                deallocate(listFichiersLocal(ii)%infosChamps, stat=iostat)
                if (iostat < 0) then
                   call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                        routine="cpsi_close_desc")
                   return
                end if
             end if
          end do
       end if

       if (associated(listFichiersRef)) then
          do ii=1, cpsi_size_ptr(listFichiersRef)
             if (associated(listFichiersRef(ii)%infosChamps)) then
                deallocate(listFichiersRef(ii)%infosChamps, stat=iostat)
                if (iostat < 0) then
                   call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                        routine="cpsi_close_desc")
                   return
                end if
             end if
          end do
       end if


       if (associated(listFichiersLocal)) then
          deallocate(listFichiersLocal, stat=iostat)
          if (iostat < 0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                  routine="cpsi_close_desc")
             return
          end if
       end if

       if (associated(listFichiersRef)) then
          deallocate(listFichiersRef, stat=iostat)
          if (iostat < 0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                  routine="cpsi_close_desc")
             return
          end if
       end if

       if (associated(listIdLocal)) then
          deallocate(listIdLocal, stat=iostat)
          if (iostat < 0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                  routine="cpsi_close_desc")
             return
          end if
       end if

       if (associated(listIdRef)) then
          deallocate(listIdRef, stat=iostat)
          if (iostat < 0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                  routine="cpsi_close_desc")
             return
          end if
       end if

       ! deallocations memoire du module cps_accesMadona
       call cpsi_close_accesMadona()

       cps_desc_init = .false.
       cps_base_locale_chargee = .false.
    end if
  end subroutine cpsi_close_desc

end module cps_desc
