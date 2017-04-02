module cps_base

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_base
!
!$Resume
!
!$Description
!  Il s'agit du module constituant l'interface de bas niveau de la
!  bibliothèque COMPAS.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: cps_base.F90 393 2013-02-26 09:34:07Z ffsm $
!
!$Historique
!  $Log: cps_base.F90,v $
!  Revision 393  2013/02/26 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!  Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!  Revision 150 2012/11/23 Fran Simarro/FFSM - GMV
!  FA-ID 1524 : Modifié pour prendre en compte l'échelle temps de la date d'entrée
!               à l'heure d'ajouter un saut TaiTuc
!
!  Revision 1.41  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.40  2009/10/06 08:50:37  cmartel
!  FA-ID 1254 : Ajout d'un test sur la lecture de lignes blanches dans les sauts de TUC
!
!  Revision 1.39  2009/07/17 13:37:35  cml
!  AQ : Correction d'une mauvaise initialisation
!
!  Revision 1.38  2009/05/27 07:50:47  cml
!  FA-ID 1284 : Ajout d'initialisations sur les variables de sorties
!
!  Revision 1.37  2008/10/28 16:40:33  cml
!  FA-ID 1061 : Mise a jour de la propagation d une evenetuelle erreur durant l init. de la base
!
!  Revision 1.36  2008/10/28 12:43:50  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.35  2008/10/20 12:34:38  cml
!  DM-ID 1058 : Ajout d une mise a jour du flag de lecture du saut de tuc
!
!  Revision 1.34  2008/10/01 17:36:42  tanguyy
!  DM-ID 1058 / AQ : amelioration de la gestion mémoire et suppression des boucles infinies
!
!  Revision 1.33  2008/08/04 13:03:26  gss
!  DM-ID 1058 : (portage g95) Initialisation à NULL des pointeurs lors de leur
!  déclaration. Suppression des variables non utilisées et modification du format
!  de lecture du saut TUC. Forcage du typage reel -> int pour la variable delta.
!  Initialisation des sorties de fonction dépacée avant le premier return.
!
!  Revision 1.32  2008/07/04 11:45:56  huec
!  DM-ID 1058 : Ameliorations de la gestion memoire, ajout d\' un parametre optionnel pour liberer les tableaux dates et deltas des sauts de TUC
!
!  Revision 1.31  2008/05/05 08:51:08  vivaresf
!  COMPAS V2.4.1 : correction des cartouches
!
!  Revision 1.30  2008/05/02 16:13:35  vivaresf
!  FA-ID 1031 : suppression de la variable inutilisée unite
!
!  Revision 1.29  2008/05/02 15:44:05  vivaresf
!   FA-ID 1031 : utilisation de unific + correction gestion d'erreur
!
!  Revision 1.28  2008/04/22 16:44:19  vivaresf
!  DM-ID 11, option, contrôle qualité : rajout du implicit none
!
!  Revision 1.27  2008/04/22 16:42:13  vivaresf
!  DM-ID 11, option : contrôle qualité :
!  - découpage de cpsi_get_sautTUC pour qu'il contienne moins de 70 instructions
!  - nouvelle subroutine interne cpsi_verif_ficsautTUC
!  - utilisation de eph_util_ficunit90 pour récupérer un lfn libre
!
!  Revision 1.26  2008/04/04 18:00:26  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.25  2008/03/21 15:07:39  vivaresf
!  DM-ID 11 : saust du TUC, gestion des erreurs
!
!  Revision 1.24  2008/03/18 16:08:03  vivaresf
!  FA-ID 892 : initialisation des pointeurs
!
!  Revision 1.23  2008/02/29 16:57:44  vivaresf
!  FA-ID 892/893 : contrôle sur l'entier passé à allocate (doit être >0)
!
!  Revision 1.22  2008/02/15 16:22:23  huec
!  DM-ID 11 : Optimisation de cps_get_sautTAITUC pour ne pas relire le fichier de saut de TUC a chaque appel
!
!  Revision 1.21  2008/02/11 08:50:50  huec
!  DM-ID 11 : Ajout des fonctions de lecture de fichier de sauts du TUC et de calcul du delta TAI-TUC
!
!  Revision 1.20  2007/11/20 08:56:59  vivaresf
!  DM-ID 539 : test pour éviter des messages divers
!
!  Revision 1.19  2007/06/20 08:51:19  vivaresf
!  FA-ID 746 : désallocation des variables de type cpsi_desc
!
!  Revision 1.18  2007/05/21 06:56:19  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.17  2006/10/26 17:17:35  vpg
!  DM-ID 462 : amélioration de l'accès à la description du contenu de la base de données COMPAS
!  Revision 1.16  2006/10/18 09:52:35  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.15.2.1  2006/09/26 12:13:26  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.15  2006/08/31 09:31:51  vpg
!  Ajustement mineur sur cpsi_getListeDescFichiers pour l'id *
!  Revision 1.14  2006/08/28 16:24:40  vpg
!  FA-ID 568 : anomalie de rafraichissement de l'IHM cps_acces de COMPAS_UI
!  Revision 1.13  2006/05/30 12:29:02  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.12  2006/05/30 08:27:46  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.11  2006/05/15 15:06:37  vpg
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.10  2006/05/02 09:36:56  vpg
!  Diminution de la complexite de certaines fonctions pour respecter les seuils des metriques
!  Revision 1.9  2006/03/20 15:52:42  vpg
!  Mise a jour des cartouches
!  Revision 1.8  2006/03/10 12:45:07  vpg
!  L'argument unite de cps_getAtt_d() devient obligatoire
!  Revision 1.7  2006/03/03 16:07:28  vpg
!  Suppression des cpsi_setCritere*() et cpsi_setCritereStructure(), et rajout de att_def dans cpsi_readStructure() indiquant si les attributs sont definis ou non
!  Revision 1.6  2006/02/13 14:53:29  vpg
!  Suppression de l'affectation de l'unite CPS_UNITE_UNDEF lors de la lecture d'un reel dans readStructure()
!  Revision 1.5  2006/02/06 15:17:21  vpg
!  Ajout de la routine cpsi_getListCateg
!  Revision 1.4  2006/01/23 14:05:15  vpg
!  rajout des traitements MAGE, rajout de fonctions liees aux ihm
!  Revision 1.3  2005/12/08 18:20:28  vivaresf
!  Cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_base
!
!$Structure
!
!$Global
!
!>  cps_restriction   : <LEN=CPS_MAXLG,public>  
!>  cps_base_init     : <logical,public>        
!#V
!>  ret               : <integer,private>       
!#
!$Common
!
!$Routines
!- cps_getCritere
!- cps_getListEltsCorps
!- cps_init_base
!- cps_getListFichiersCateg
!- cps_restreindre
!- cpsi_getListeDescFichiers
!- cpsi_getListId
!- cpsi_getListCateg
!- cpsi_deallocateDescFichiers
!- cpsi_readStructure
!- cps_close_base
!
!$Fonctions
!- cpsi_getCritere_ii
!- cpsi_getCritere_id
!- cpsi_getCritere_is
!- cpsi_getCritere_di
!- cpsi_getCritere_dd
!- cpsi_getCritere_ds
!- cpsi_getCritere_si
!- cpsi_getCritere_sd
!- cpsi_getCritere_ss
!- cpsi_getCritereStructure_ii
!- cpsi_getCritereStructure_id
!- cpsi_getCritereStructure_is
!- cpsi_getCritereStructure_di
!- cpsi_getCritereStructure_dd
!- cpsi_getCritereStructure_ds
!- cpsi_getCritereStructure_si
!- cpsi_getCritereStructure_sd
!- cpsi_getCritereStructure_ss
!- cps_getFichierCateg
!- cpsi_getListEltsCorps_s
!- cpsi_stringInTab
!- cpsi_getNbAttCateg
!
!$Include
!
!$Module
!#V
!- cps_desc
!#
!
!$Interface
!> cps_getlisteltscorps :  cpsi_getListEltsCorps_s
!> cps_getcritere :        cpsi_getCritere_ii, cpsi_getCritere_id,
!                          cpsi_getCritere_is, cpsi_getCritere_di, 
!                          cpsi_getCritere_dd, cpsi_getCritere_ds, 
!                          cpsi_getCritere_si, cpsi_getCritere_sd,
!                          cpsi_getCritere_ss
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cpsi_getCritere_ii cpsi_getCritere_id cpsi_getCritere_is cpsi_getCritere_di cpsi_getCritere_dd
!.  cpsi_getCritere_ds cpsi_getCritere_si cpsi_getCritere_sd cpsi_getCritere_ss cpsi_getCritereStructure_ii
!.  cpsi_getCritereStructure_id cpsi_getCritereStructure_is cpsi_getCritereStructure_di
!.  cpsi_getCritereStructure_dd cpsi_getCritereStructure_ds cpsi_getCritereStructure_si
!.  cpsi_getCritereStructure_sd cpsi_getCritereStructure_ss cps_getFichierCateg cpsi_getListEltsCorps_s
!.  cpsi_stringInTab cpsi_getNbAttCateg cps_getCritere cps_getListEltsCorps cps_init_base
!.  cps_getListFichiersCateg cps_restreindre cpsi_getListeDescFichiers cpsi_getListId cpsi_getListCateg
!.  cpsi_deallocateDescFichiers cpsi_readStructure cps_close_base
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use cps_desc

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_base.F90 393 2013-02-26 09:34:07Z ffsm $'


  character(LEN=CPS_MAXLG), public :: cps_restriction = CPS_NO_RESTRICTION
  logical, public :: cps_base_init = .false.
  integer, private :: ret

  interface cps_getCritere
     module procedure cpsi_getCritere_ii, cpsi_getCritere_id, &
          cpsi_getCritere_is, cpsi_getCritere_di, cpsi_getCritere_dd, &
          cpsi_getCritere_ds, cpsi_getCritere_si, cpsi_getCritere_sd, &

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCritere
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!
!  trouve = cps_getCritere(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Procedures
!- cpsi_getCritere_ii
!- cpsi_getCritere_id
!- cpsi_getCritere_is
!- cpsi_getCritere_di
!- cpsi_getCritere_dd
!- cpsi_getCritere_ds
!- cpsi_getCritere_si
!- cpsi_getCritere_sd
!- cpsi_getCritere_ss
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          cpsi_getCritere_ss
  end interface

  interface cps_getListEltsCorps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListEltsCorps
!
!$Resume
!
!$Description
!  Renvoie la liste des valeurs d'un même attribut défini plusieurs
!  fois pour un corps.
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getListEltsCorps(code_corps, &
!.           nom_att, listElts)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listElts
!.    integer :: trouve
!
!$Procedures
!- cpsi_getListEltsCorps_s
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cpsi_getListEltsCorps_s
  end interface

  private cpsi_verif_ficsautTUC


contains

  subroutine cps_init_base()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_init_base
!
!$Resume
!
!$Description
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_init_base()
!
!$Arguments
!
!$Common
!
!$Routines
!- cpsi_init_desc
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
    call cpsi_init_desc()
    if ( MSP_gen_messages("cps_init_base") ) return

    cps_restriction = CPS_NO_RESTRICTION
    cps_base_init = .true.

  end subroutine cps_init_base





  function cpsi_getCritere_ii(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_ii
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type entier.
!  nom_att_res est de type entier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_ii(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisations
    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_ii", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    ! boucle tant que l'on n'a pas trouvé le critère
    ! on sort également si on atteint la fin de la séquence.
    do while(trouve /= CPS_OK) 
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_ii(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_ii

  function cpsi_getCritere_id(acces, nom_att_rech, vrech, nom_att_res, vres, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_id
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type entier.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_id(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite         :<LEN=*>     unite de l'attribut recherche
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite

    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite = ""
    vres = 0.0_pm_reel

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_id", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_id(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_id

  function cpsi_getCritere_is(acces, nom_att_rech, vrech, nom_att_res, vres) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_is
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type entier.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_is(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_is", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_is(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_is

  function cpsi_getCritere_di(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_di
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type reel.
!  nom_att_res est de type entier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_di(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_di", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_di(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_di

  function cpsi_getCritere_dd(acces, nom_att_rech, vrech, nom_att_res, vres, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_dd
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type reel.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_dd(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite         :<LEN=*>     unite de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite = ""
    vres = 0.0_pm_reel

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_dd", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_dd(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_dd

  function cpsi_getCritere_ds(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_ds
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type reel.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_ds(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_ds", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_ds(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_ds

  function cpsi_getCritere_si(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_si
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type string.
!  nom_att_res est de type entier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_si(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_si", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_si(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_si

  function cpsi_getCritere_sd(acces, nom_att_rech, vrech, nom_att_res, vres, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_sd
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type string.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_sd(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite         :<LEN=*>     unite de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_sd", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_sd(acces, nom_att_rech, vrech, nom_att_res, vres, unite)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_sd

  function cpsi_getCritere_ss(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritere_ss
!
!$Resume
!
!$Description
!  Recherche dans un fichier de données la valeur vres de l'attribut
!  nom_att_res contenu dans la première structure qui contient
!  également l'attribut nom_att_rech dont la valeur est vrech.
!  nom_att_rech est de type string.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritere_ss(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    ! Variables locales
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, trouve

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritere_ss", &
            partie_variable="cps_base")
       return
    end if
    
    ret = acc_scan_reset(acces)

    do while (trouve /= CPS_OK)
       ret = acc_scan(acces, libelle, nature)
       select case (nature)
          case (ACC_STRUCT)
             ret = acc_select(acces, libelle, ACC_STRUCT)
             trouve = cpsi_getCritereStructure_ss(acces, nom_att_rech, vrech, nom_att_res, vres)
             ret = acc_select_end(acces)
          case (CPS_FIN_SEQ)
             exit
          end select
    end do

  end function cpsi_getCritere_ss




  function cpsi_getCritereStructure_ii(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_ii
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type entier.
!  nom_att_res est de type entier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_ii(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    integer :: trouve
    integer :: val

    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_ii", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_geti(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_geti(acces, nom_att_res, vres)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if
    
  end function cpsi_getCritereStructure_ii

  function cpsi_getCritereStructure_id(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_id
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type entier.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_id(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite_res
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite_res     :<LEN=*>     unite de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite_res

    character(LEN=CPS_MAXLG) :: fichier
    integer :: trouve, i, iostat
    integer :: val
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite_res = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_id", &
            partie_variable="cps_base")
       return
    end if

    ! determinaton de l'unite de att_res
    trouve = cpsi_getFichierAcces(acces, fichier)
    trouve = cpsi_getDescFichier(fichier, desc)
    do i=1, desc%nbChamps
       if (desc%infosChamps(i)%nom.eq.nom_att_res) then
          unite_res = trim(desc%infosChamps(i)%unite)
          exit
       end if
    end do

    trouve = CPS_ERR_DEF

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_geti(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_getd(acces, nom_att_res, vres, unite_res)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

    ! Libération mémoire
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    
  end function cpsi_getCritereStructure_id

  function cpsi_getCritereStructure_is(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_is
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type entier.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_is(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    integer :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<integer>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    integer, intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    integer :: trouve
    integer :: val

    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_is", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_geti(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_gets(acces, nom_att_res, vres)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if
    
  end function cpsi_getCritereStructure_is

  function cpsi_getCritereStructure_di(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_di
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type reel.
!  nom_att_res est de type unite.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_di(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    character(LEN=CPS_MAXLG) :: unite_rech, fichier
    integer :: trouve, i, res, iostat
    real(kind=PM_REEL) :: val
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_di", &
            partie_variable="cps_base")
       return
    end if

    ! determination de l'unite de att_rech
    trouve = cpsi_getFichierAcces(acces, fichier)
    trouve = cpsi_getDescFichier(fichier, desc)
    do i=1, desc%nbChamps
       if (desc%infosChamps(i)%nom.eq.nom_att_rech) then
          unite_rech = trim(desc%infosChamps(i)%unite)
          exit
       end if
    end do

    trouve = CPS_ERR_DEF

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_getd(acces, trim(nom_att_rech), val, unite_rech)
       res = cpsi_compareReels(val, vrech)
       if (res.eq.0) then
          ret = acc_geti(acces, nom_att_res, vres)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

    ! Libération mémoire
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
  end function cpsi_getCritereStructure_di

  function cpsi_getCritereStructure_dd(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_dd
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type reel.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_dd(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite_res
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite_res     :<LEN=*>     unite de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite_res

    ! Variables locales
    character(LEN=CPS_MAXLG) :: unite_rech, fichier
    integer :: trouve, i, res, iostat
    real(kind=PM_REEL) :: val
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite_res = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_dd", &
            partie_variable="cps_base")
       return
    end if

    ! determination des unites de att_rech et att_res
    trouve = cpsi_getFichierAcces(acces, fichier)
    trouve = cpsi_getDescFichier(fichier, desc)
    do i=1, desc%nbChamps
       if (desc%infosChamps(i)%nom.eq.nom_att_rech) then
          unite_rech = trim(desc%infosChamps(i)%unite)
       elseif (desc%infosChamps(i)%nom.eq.nom_att_res) then
          unite_res = trim(desc%infosChamps(i)%unite)
       end if
    end do

    trouve = CPS_ERR_DEF
    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_getd(acces, trim(nom_att_rech), val, unite_rech)
       res = cpsi_compareReels(val, vrech)
       if (res.eq.0) then
          ret = acc_getd(acces, nom_att_res, vres, unite_res)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

    ! Libération mémoire
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)

  end function cpsi_getCritereStructure_dd

  function cpsi_getCritereStructure_ds(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_ds
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type reel.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_ds(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    real(kind=PM_REEL) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<PM_REEL>   valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    real(kind=PM_REEL), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    ! Variables locales
    character(LEN=CPS_MAXLG) :: unite_rech, fichier
    integer :: trouve, i, res, iostat
    real(kind=PM_REEL) :: val
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_ds", &
            partie_variable="cps_base")
       return
    end if

    ! determination de l'unite de att_rech
    trouve = cpsi_getFichierAcces(acces, fichier)
    trouve = cpsi_getDescFichier(fichier, desc)
    do i=1, desc%nbChamps
       if (desc%infosChamps(i)%nom.eq.nom_att_rech) then
          unite_rech = trim(desc%infosChamps(i)%unite)
          exit
       end if
    end do

    trouve = CPS_ERR_DEF

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_getd(acces, trim(nom_att_rech), val, unite_rech)
       res = cpsi_compareReels(val, vrech)
       if (res.eq.0) then
          ret = acc_gets(acces, nom_att_res, vres)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

    ! libérarion mémoire
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
  end function cpsi_getCritereStructure_ds

  function cpsi_getCritereStructure_si(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_si
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type string.
!  nom_att_res est de type entier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_si(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<integer>   valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    integer, intent(out) :: vres

    ! Variables locales
    integer :: trouve
    character(LEN=CPS_MAXLG) :: val

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = 0

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_si", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_gets(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_exist(acces, nom_att_res)
          if (ret.eq.1) then
             ret = acc_geti(acces, nom_att_res, vres)    
             trouve = CPS_OK
          end if
       end if
    end if

  end function cpsi_getCritereStructure_si

  function cpsi_getCritereStructure_sd(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_sd
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type string.
!  nom_att_res est de type reel.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_sd(acces, nom_att_rech, vrech, nom_att_res, vres, unite_res)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=*) :: unite_res
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<PM_REEL>   valeur de l'attribut recherché
!>S     unite_res     :<LEN=*>     unite de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=*), intent(out) :: unite_res

    ! Variables locales
    character(LEN=CPS_MAXLG) :: fichier
    integer :: trouve, i, iostat
    character(LEN=CPS_MAXLG) :: val
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF
    unite_res = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_sd", &
            partie_variable="cps_base")
       return
    end if

    ! determinaton de l'unite de att_res
    trouve = cpsi_getFichierAcces(acces, fichier)
    trouve = cpsi_getDescFichier(fichier, desc)
    do i=1, desc%nbChamps
       if (desc%infosChamps(i)%nom.eq.nom_att_res) then
          unite_res = trim(desc%infosChamps(i)%unite)
          exit
       end if
    end do

    trouve = CPS_ERR_DEF

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_gets(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_getd(acces, nom_att_res, vres, unite_res)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
  end function cpsi_getCritereStructure_sd

  function cpsi_getCritereStructure_ss(acces, nom_att_rech, vrech, nom_att_res, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCritereStructure_ss
!
!$Resume
!
!$Description
!  Fonction interne.
!  Cette fonction lit une structure dans un fichier de donnees et extrait
!  la valeur vres de l'attribut nom_att_res si l'attribut nom_att_rech est
!  à la valeur vrech. La structure doit avoir été sélectionnée
!  auparavent.
!  nom_att_rech es de type string.
!  nom_att_res est de type string.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getCritereStructure_ss(acces, nom_att_rech, vrech, nom_att_res, vres)
!.    integer :: acces
!.    character(LEN=*) :: nom_att_rech
!.    character(LEN=*) :: vrech
!.    character(LEN=*) :: nom_att_res
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     acces         :<integer>   acces MADONA ouvert sur le fichier de données
!>E     nom_att_rech  :<LEN=*>     nom de l'attribut correspondant au critère de recherche
!>E     vrech         :<LEN=*>     valeur de l'attribut correspondant au critère de recherche
!>E     nom_att_res   :<LEN=*>     nom de l'attribut recherché
!>S     vres          :<LEN=*>     valeur de l'attribut recherché
!>S     trouve        :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
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
    integer, intent(in) :: acces
    character(LEN=*), intent(in) :: nom_att_rech
    character(LEN=*), intent(in) :: vrech
    character(LEN=*), intent(in) :: nom_att_res
    character(LEN=*), intent(out) :: vres

    ! Variables locales
    integer :: trouve
    character(LEN=CPS_MAXLG) :: val

    ! Initialisation
    trouve = CPS_ERR_DEF
    vres = ""

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCritereStructure_ss", &
            partie_variable="cps_base")
       return
    end if

    ret = acc_exist(acces, trim(nom_att_rech))
    if (ret.eq.1) then
       ret = acc_gets(acces, trim(nom_att_rech), val)
       if (val.eq.vrech) then
          ret = acc_gets(acces, nom_att_res, vres)
          if (ret.eq.0) then
             trouve = CPS_OK
          end if
       end if
    end if

  end function cpsi_getCritereStructure_ss

  
  function cps_getFichierCateg(categorie, id, fichier) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierCateg
!
!$Resume
!
!$Description
!  Fonction qui permet d'obtenir le fichier associe a une categorie et 
!  un id particulier.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierCateg(categorie, id, fichier)
!.    character(LEN=*) :: categorie, id
!.    character(LEN=*) :: fichier
!.    integer :: trouve
!
!$Arguments
!>E     categorie  :<LEN=*>     catégorie à laquelle est associé le fichier recherché
!>E     id         :<LEN=*>     id du fichier recherché
!>S     fichier    :<LEN=*>     fichier recherché
!>S     trouve     :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
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
    character(LEN=*), intent(in) :: categorie, id
    character(LEN=*), intent(out) :: fichier
    
    ! resultat
    integer :: trouve

    ! variables locales
    integer :: i

    ! Initialisations
    fichier = ""
    trouve = CPS_ERR_DEF
    
    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getFichierCateg", &
            partie_variable="cps_base")
       return
    end if

    if (associated(listFichiersLocal)) then
       do i=1, cpsi_size_ptr(listFichiersLocal)
          if ((listFichiersLocal(i)%categorie.eq.categorie).and.&
               (listFichiersLocal(i)%id.eq.id).and.&
               (trouve.ne.CPS_OK)) then
             fichier = listFichiersLocal(i)%fichier
             trouve = CPS_OK
             exit
          end if
       end do
    endif
    
    if (trouve.ne.CPS_OK) then
       if (associated(listFichiersRef)) then
          do i=1, cpsi_size_ptr(listFichiersRef)
             if ((listFichiersRef(i)%categorie.eq.categorie).and.&
                  (listFichiersRef(i)%id.eq.id).and.&
                  (trouve.ne.CPS_OK)) then
                fichier = listFichiersRef(i)%fichier
                trouve = CPS_OK
                exit
             end if
          end do
       end if
    end if

    if (trouve.ne.CPS_OK) then
       ! Aucun fichier trouve
       call MSP_signaler_message(cle_mes="CPS_WAR_CATEG_ID", &
            routine="cps_getFichierCateg", &
            partie_variable=trim(categorie)//" avec comme id "//trim(id))
    end if

  end function cps_getFichierCateg

  
  subroutine cps_getListFichiersCateg(categorie, fichiers, type_base)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListFichiersCateg
!
!$Resume
!
!$Description
!  Fonction qui permet d'obtenir la liste des tous les fichiers d'une categorie.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListFichiersCateg(categorie, fichiers, [type_base])
!.    character(LEN=*) :: categorie
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers
!.    integer :: type_base
!
!$Arguments
!>E     categorie  :<LEN=*>                           catégorie à laquelle sont associés les fichiers recherchés
!>E/S   fichiers   :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers recherchés
!>[E]   type_base  :<integer>                         
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_getListeDescFichiers
!- cpsi_deallocateDescFichiers
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
    character(LEN=*), intent(in) :: categorie
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers
    integer, optional, intent(in) :: type_base

    ! variables locales
    integer :: nbFichiers, i, ind, iostat
    type(cpsi_desc), dimension(:), pointer :: listeFichiers
    integer :: base

    if (present(type_base)) then
       base = type_base
    else
       ! par defaut : recherche dans la base de référence et dans la base locale
       base = 0
    end if
    
    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getListFichiersCateg", &
            partie_variable="cps_base")
       return
    end if

    ! liste des descriptions en tenant compte des restrictions
    nullify(listeFichiers)
    call cpsi_getListeDescFichiers(listeFichiers, type_base=base)

    ! determination du nombre de fichiers
    nbFichiers = 0

    if (cpsi_size_ptr(listeFichiers)>0) then
       do i=1, cpsi_size_ptr(listeFichiers)
          if (listeFichiers(i)%categorie.eq.categorie) then
             nbFichiers = nbFichiers+1
          end if
       end do
    endif

    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
               routine="cps_getListFichiersCateg")
          return
       end if
    end if

    if (nbfichiers>0) then 

       allocate(fichiers(nbFichiers), stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
               routine="cps_getListFichiersCateg")
          return
       end if

       ind = 1
       fichiers(1)=""

       do i=1, cpsi_size_ptr(listeFichiers)
          if (listeFichiers(i)%categorie.eq.categorie) then
             fichiers(ind) = listeFichiers(i)%fichier
             ind = ind+1
          end if
       end do
    endif



    call cpsi_deallocateDescFichiers(listeFichiers)

  end subroutine cps_getListFichiersCateg


  
  function cpsi_getListEltsCorps_s(code_corps, nom_att, listElts) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getListEltsCorps_s
!
!$Resume
!
!$Description
!  Retourne la liste de tous les elements nommes 'nom_att'
!  de type chaines de caracteres pour un corps.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cpsi_getListEltsCorps_s(code_corps, &
!.           nom_att, listElts)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listElts
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>                         code du corps  
!>E     nom_att     :<LEN=*>                           nom de l'attribut recherché
!>E/S   listElts    :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des valeurs de l'attribut
!>S     trouve      :<integer>                         CPS_OK s'il existe au moins une valeur
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_getListeDescFichiers
!- cpsi_getAccesMadona
!- cpsi_deallocateDescFichiers
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

    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listElts

    integer :: trouve

    ! Variables locales
    integer :: i, k, acces, nature, nbElts, code
    character(LEN=CPS_MAXLG) :: libelle, v_tmp
    logical :: existe
    type(cpsi_desc), dimension(:), pointer :: listeFichiers

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getListEltsCorps_s", &
            partie_variable="cps_base")
       return
    end if

    nbElts = 0

    nullify(listeFichiers)
    call cpsi_getListeDescFichiers(listeFichiers)

    do i=1, cpsi_size_ptr(listeFichiers)
       ! on regarde si l'attribut 'nom_att' est present dans le fichier
       existe = .false.
       existe = cpsi_existeAttDesc(nom_att, listeFichiers(i))
       if (existe) then
         ! l'attribut existe
         call cpsi_getAccesMadona(listeFichiers(i)%fichier, acces)
         if (acces.LT.0) then
            call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                 routine="cpsi_getListEltsCorps_s", &
                 partie_variable=trim(listeFichiers(i)%fichier))
            ! liberation memoire
            call cpsi_deallocateDescFichiers(listeFichiers)
            return
         end if
         ret = acc_scan_reset(acces)

         ! Init de nature (condition de boucle) à -1
         ! -> la boucle se termine sur CPS_FIN_SEQ (=0)
         nature = CPS_ERR_DEF
         do while (nature /= CPS_FIN_SEQ)
            ret = acc_scan(acces, libelle, nature)
            ret = acc_select(acces, libelle, ACC_STRUCT)
            ret = acc_geti(acces, "code", code)
            if (code.eq.code_corps) then
               nbElts = nbElts+1
            end if
            ret = acc_select_end(acces)
         end do
       end if
    end do

    ! allocation memoire
    allocate(listElts(nbElts))

    ! 2eme passe : lecture des fichiers
    k = 1

    do i=1, cpsi_size_ptr(listeFichiers)
       ! on regarde si l'attribut 'nom_att' est present dans le fichier
       existe = .false.
       existe = cpsi_existeAttDesc(nom_att, listeFichiers(i))
       if (existe) then
         ! l'attribut existe
         call cpsi_getAccesMadona(listeFichiers(i)%fichier, acces)
         if (acces.Lt.0) then
            call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                 routine="cpsi_getListEltsCorps_s", &
                 partie_variable=trim(listeFichiers(i)%fichier))
            ! liberation memoire
            call cpsi_deallocateDescFichiers(listeFichiers)
            return
         end if
         ret = acc_scan_reset(acces)
  
         ! Init de nature (condition de boucle) à -1
         ! -> la boucle se termine sur CPS_FIN_SEQ (=0)
         nature = CPS_ERR_DEF
         do while (nature /= CPS_FIN_SEQ)
            ret = acc_scan(acces, libelle, nature)
            ret = acc_select(acces, libelle, ACC_STRUCT)
            ret = acc_geti(acces, "code", code)
            if (code.eq.code_corps) then
               ret = acc_gets(acces, nom_att, v_tmp)
               listElts(k) = trim(v_tmp)
               k = k+1
            end if
            ret = acc_select_end(acces)
         end do
       end if
    end do

    if (nbElts.gt.0) then
       trouve = CPS_OK
    end if

    call cpsi_deallocateDescFichiers(listeFichiers)

  end function cpsi_getListEltsCorps_s


  


  subroutine cps_restreindre(id)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_restreindre
!
!$Resume
!
!$Description
! V2.0
! Permet d'utiliser COMPAS avec un nombre restreint de fichiers
! Valeurs possibles de id :
! - CPS_SOLAR_SYSTEM : fichiers relatifs aux Systeme solaire
! - CPS_ASTEROIDES_NEA : fichiers relatifs aux asteroides NEA
! - CPS_NO_RESTRICTION : tous les fichiers
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_restreindre(id)
!.    character(LEN=*) :: id
!
!$Arguments
!>E     id  :<LEN=*>   valeur de l'id
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
    implicit none
    ! argument
    character(LEN=*), intent(in) :: id
    
    cps_restriction = trim(id)
  end subroutine cps_restreindre



  subroutine cpsi_getListeDescFichiers(listeFichiers, type_base)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getListeDescFichiers
!
!$Resume
!
!$Description
! V2.0
! Renvoie la liste des descriptions des fichiers (locaux et reference)
! en tenant compte des restrictions eventuelles
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getListeDescFichiers(listeFichiers, [type_base])
!.    type(cpsi_desc), dimension(:), pointer :: listeFichiers
!.    integer :: type_base
!
!$Arguments
!>E/S   listeFichiers  :<cpsi_desc,DIM=(:),pointer>   liste des descriptions disponibles
!>[E]   type_base      :<integer>                     
!
!$Common
!
!$Routines
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
    type(cpsi_desc), dimension(:), pointer :: listeFichiers
    integer, optional, intent(in) :: type_base

    ! variables locales
    integer :: nb, ind, i, index_id, index_all_id, base, ii

    nb = 0
    
    if (present(type_base)) then
       base = type_base
    else
       ! par defaut : base de reference et base locale
       base = 0
    end if

    if (((base==0).or.(base==2)).and. associated(listFichiersLocal)) then
       ! determination du nombre de fichiers
       ! fichiers locaux
       do i=1, cpsi_size_ptr(listFichiersLocal)
          index_all_id = index(listFichiersLocal(i)%id, trim(CPS_ALL_ID))
          index_id = index(listFichiersLocal(i)%id, trim(cps_restriction))
          if ((cps_restriction.eq.CPS_NO_RESTRICTION).or.&
               (index_id.ge.1).or.(index_all_id.ge.1)) then
             nb = nb+1
          end if
       end do
    end if
    
    if (((base==0).or.(base==1)).and. associated(listFichiersRef)) then
       ! fichiers de reference
       do i=1, cpsi_size_ptr(listFichiersRef)
          index_all_id = index(listFichiersRef(i)%id, trim(CPS_ALL_ID))
          index_id = index(listFichiersRef(i)%id, trim(cps_restriction))
          if ((cps_restriction.eq.CPS_NO_RESTRICTION).or.&
               (index_id.ge.1).or.(index_all_id.ge.1)) then
             nb = nb+1
          end if
       end do
    end if
    
    ! allocation memoire
    call cpsi_deallocateDescFichiers(listeFichiers)
    allocate(listeFichiers(nb))
    do ii=1,nb
       nullify(listeFichiers(ii)%infosChamps)
    enddo

    ind = 1
    ! recopie des descriptions
    if (((base==0).or.(base==2)).and.associated(listFichiersLocal)) then
       ! fichiers locaux
       do i=1, cpsi_size_ptr(listFichiersLocal)
          index_all_id = index(listFichiersLocal(i)%id, trim(CPS_ALL_ID))
          index_id = index(listFichiersLocal(i)%id, trim(cps_restriction))
          if ((cps_restriction.eq.CPS_NO_RESTRICTION).or.&
               (index_id.ge.1).or.(index_all_id.ge.1)) then
             ! recopie de la description : 
             call cpsi_copyDesc(listFichiersLocal(i), listeFichiers(ind))
             ind = ind+1
          end if
       end do
    end if
    
    if (((base==0).or.(base==1)).and.associated(listFichiersRef)) then
       ! fichiers de reference
       do i=1, cpsi_size_ptr(listFichiersRef)
          index_all_id = index(listFichiersRef(i)%id, trim(CPS_ALL_ID))
          index_id = index(listFichiersRef(i)%id, trim(cps_restriction))
          if ((cps_restriction.eq.CPS_NO_RESTRICTION).or.&
               (index_id.ge.1).or.(index_all_id.ge.1)) then
             ! recopie de la description : 
             call cpsi_copyDesc(listFichiersRef(i), listeFichiers(ind))
             ind = ind+1
          end if
       end do
    end if

  end subroutine cpsi_getListeDescFichiers


  
  subroutine cpsi_getListId(listId)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getListId
!
!$Resume
!
!$Description
! V2.0
! Routine qui renvoie la liste des ids disponibles dans la base de reference
! et dans une eventuelle base locale.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getListId(listId)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listId
!
!$Arguments
!>E/S   listId  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des ids disponibles
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
    implicit none
    ! arguments
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listId

    ! variables locales
    integer :: nbLocal, nbRef, nb, i, ind
    logical :: id_local, id_ref, present

    present = .false.
    id_local = .false.
    id_ref = .false.
    nbLocal = 0
    nbRef = 0
    nb = 0
    
    if (associated(listIdLocal)) then
       nbLocal = cpsi_size_ptr(listIdLocal)
       if (nbLocal.gt.0) then
          id_local = .true.
       end if
    end if

    if (associated(listIdRef)) then
       nbRef = cpsi_size_ptr(listIdRef)
       if (nbRef.gt.0) then
          id_ref = .true.
       end if
    end if
    
    if ((id_ref).and.(.not.id_local)) then
       ! uniquement des id de la base de reference
       ! ne pas oublier CPS_NO_RESTRICTION
       allocate(listId(nbRef+1))
       listId(1) = trim(CPS_NO_RESTRICTION)
       ! recopie des elements de listIdRef dans listId
       do i=1, nbRef
          listId(i+1) = trim(listIdRef(i))
       end do
    elseif ((id_local).and.(.not.id_ref)) then
       ! uniquement des id de la base locale
       ! ne pas oublier CPS_NO_RESTRICTION
       allocate(listId(nbLocal+1))
       listId(1) = trim(CPS_NO_RESTRICTION)
       ! recopie des elements de listIdLocal dans listId
       do i=1, nbLocal
          listId(i+1) = trim(listIdLocal(i))
       end do
    else
       ! id de la base de reference et de la base locale
       ! on recopie dans listId les elements de listIdRef
       ! et ceux de listIdLocal qui sont differents.
       nb = nbRef
       ! determination du nombre d'elements de listIdLocal non presents
       ! dans listIdRef
       do i=1, nbLocal
          present = cpsi_stringInTab(trim(listIdLocal(i)), listIdRef)
          if (.not.present) then
             nb = nb+1
          end if
       end do
       ! ne pas oublier CPS_NO_RESTRICTION
       nb = nb+1
       allocate(listId(nb))
       ! ajout de CPS_RESCTRICTION
       listId(1) = trim(CPS_NO_RESTRICTION)
       ind = 2
       ! recopie des elements de listIdRef
       do i=1, nbRef
          listId(ind) = trim(listIdRef(i))
          ind = ind+1
       end do
       ! recopie des elements de listIdLocal
       ! differents de ceux de listIdRef
       do i=1, nbLocal
          present = cpsi_stringInTab(trim(listIdLocal(i)), listIdRef)
          if (.not.present) then
             listId(ind) = trim(listIdLocal(i))
             ind = ind+1
          end if
       end do
    end if

  end subroutine cpsi_getListId


  subroutine cpsi_getListCateg(listCateg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getListCateg
!
!$Resume
!
!$Description
! V2.0
! Routine qui renvoie la liste des categories disponibles dans la base
! de reference et eventuellement une base locale.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getListCateg(listCateg)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listCateg
!
!$Arguments
!>E/S   listCateg  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des catégories disponibles
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
    implicit none
    ! arguments
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listCateg

    ! variables locales
    integer :: nbCateg, i, nbRef, nbLocal
    logical :: present
    character(LEN=CPS_MAXLG), dimension(100) :: listCateg_tmp
    
    
    nbCateg = 0
    nbRef = cpsi_size_ptr(listFichiersRef)
    nbLocal = 0
    if (associated(listFichiersLocal)) then
       nbLocal = cpsi_size_ptr(listFichiersLocal)
    end if
    
    ! parcourt des fichiers de la base de reference
    do i=1, nbRef
       ! on regarde si la categorie du fichier courant est deja repertoriee
       present = cpsi_stringInTab(trim(listFichiersRef(i)%categorie), listCateg_tmp(1:nbCateg))
       if (.not.present) then
          ! s'il s'agit d'une nouvelle categorie, on l'ajoute
          nbCateg = nbCateg+1
          listCateg_tmp(nbcateg) = trim(listFichiersRef(i)%categorie)
       end if
    end do
    
    ! parcourt des fichiers de la base locale
    do i=1, nbLocal
       ! on regarde si la categorie du fichier courant est deja repertoriee
       present = cpsi_stringInTab(trim(listFichiersLocal(i)%categorie), listCateg_tmp(1:nbCateg))
       if (.not.present) then
          ! s'il s'agit d'une nouvelle categorie, on l'ajoute
          nbCateg = nbCateg+1
          listCateg_tmp(nbcateg) = trim(listFichiersLocal(i)%categorie)
       end if
    end do
    
    ! construction de la liste definitive
    allocate(listCateg(nbCateg))
    listCateg(1:nbCateg) = listCateg_tmp(1:nbCateg)

  end subroutine cpsi_getListCateg


  
  function cpsi_stringInTab(string, tab) result(present)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_stringInTab
!
!$Resume
!
!$Description
! V2.0
! Determine si la chaine 'string' est presente dans le tableau 'tab'.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  present = cpsi_stringInTab(string, tab)
!.    character(LEN=*) :: string
!.    character(LEN=CPS_MAXLG), dimension(:) :: tab
!.    logical :: present
!
!$Arguments
!>E     string   :<LEN=*>                   chaîne de caractères à rechercher
!>E     tab      :<LEN=CPS_MAXLG,DIM=(:)>   tableau dans lequel effectuer la recherche
!>S     present  :<logical>                 .true. si la chaîne est présente dans le tableau, .false. sinon
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
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: string
    character(LEN=CPS_MAXLG), dimension(:), intent(in) :: tab

    ! resultat
    logical :: present

    ! variables locales
    integer :: i, sizeTab

    present = .false.
    sizeTab = size(tab)

    do i=1, sizeTab
       if (trim(string).eq.trim(tab(i))) then
          present = .true.
          exit
       end if
    end do

  end function cpsi_stringInTab

  
  subroutine cpsi_deallocateDescFichiers(listeFichiers)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_deallocateDescFichiers
!
!$Resume
!
!$Description
! V2.0
! Liberation memoire d'un tableau contenant les descriptions des
! fichiers utilises, en tenant compte des restrictions.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_deallocateDescFichiers(listeFichiers)
!.    type(cpsi_desc), dimension(:), pointer :: listeFichiers
!
!$Arguments
!>E/S   listeFichiers  :<cpsi_desc,DIM=(:),pointer>   tableau des descriptions à libérer
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
    implicit none
    ! arguments
    type(cpsi_desc), dimension(:), pointer :: listeFichiers
    
    ! variable locale
    integer :: ii, iostat

    if (associated(listeFichiers)) then
       do ii=1, cpsi_size_ptr(listeFichiers)
          if (associated(listeFichiers(ii)%infosChamps)) then
             deallocate(listeFichiers(ii)%infosChamps, stat=iostat)
             if (iostat < 0) then
                call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                     routine="cpsi_deallocateDescFichiers")
                return
             end if
          end if
       end do
       deallocate(listeFichiers, stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
               routine="cpsi_deallocateDescFichiers")
          return
       end if
       
    end if
  end subroutine cpsi_deallocateDescFichiers


  
  function cpsi_getNbAttCateg(categorie) result(nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getNbAttCateg
!
!$Resume
!
!$Description
! V2.0
! Retourne le nombre d'attributs d'une categorie
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  nb = cpsi_getNbAttCateg(categorie)
!.    character(LEN=*) :: categorie
!.    integer :: nb
!
!$Arguments
!>E     categorie  :<LEN=*>     catégorie dont on souhaite connaitre le nombre d'attributs
!>S     nb         :<integer>   nombre d'attributs de la catégorie
!
!$Common
!
!$Routines
!- cpsi_getListeDescFichiers
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
    ! argument
    character(LEN=*), intent(in) :: categorie
    
    ! resultat
    integer :: nb
    
    ! variables locales
    integer :: i
    type(cpsi_desc), dimension(:), pointer :: listFichiers
    
    nb = 0
    nullify(listFichiers)
    call cpsi_getListeDescFichiers(listFichiers)
    do i=1, cpsi_size_ptr(listFichiers)
       if (listFichiers(i)%categorie.eq.categorie) then
          nb = listFichiers(i)%nbChamps
          exit
       end if
    end do
    
    ! liberation memoire
    call cpsi_deallocateDescFichiers(listFichiers)
    
  end function cpsi_getNbAttCateg

  
  subroutine cpsi_readStructure(acces, desc, noms_att, types_att, &
       vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_readStructure
!
!$Resume
!
!$Description
! V2.0
! Routine qui lit une structure MADONA
! 'acces' doit etre au niveau de la structure à lire
! Cette routine construit la liste des noms des attributs
! presents dans la structure à partir de la description desc
! Elle donne via vals_i, vals_d et vals_s les valeurs entieres,
! reelles et chaines de caracteres lues par le biais de l'acces MADONA
! exemple :
! code = 10
! nom_fr = "soleil"
! apla = 1.
! nb = 3
! noms_att(1) = "code"
! noms_att(2) = "nom_fr"
! noms_att(3) = "apla"
! vals_i(1) = 10
! vals_i(2) = 0
! vals_i(3) = 0
! vals_d(1) = 0.
! vals_d(2) = 0.
! vals_d(3) = 0.
! vals_s(1) = ""
! vals_s(2) = "soleil"
! vals_s(3) = ""
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_readStructure(acces, desc, noms_att, types_att, &
!.           vals_i, vals_d, vals_s, unites, att_def)
!.    integer :: acces
!.    type(cpsi_desc) :: desc
!.    character(LEN=CPS_MAXLG), dimension(:) :: noms_att
!.    integer, dimension(:) :: types_att
!.    integer, dimension(:) :: vals_i
!.    real(KIND=PM_REEL), dimension(:) :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(:) :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(:) :: unites
!.    logical, dimension(:) :: att_def
!
!$Arguments
!>E     acces      :<integer>                 accès MADONA ouvert sur le fichier de données
!>E     desc       :<cpsi_desc>               description du fichier de données
!>S     noms_att   :<LEN=CPS_MAXLG,DIM=(:)>   noms des attributs
!>S     types_att  :<integer,DIM=(:)>         types des attributs
!>S     vals_i     :<integer,DIM=(:)>         valeurs des attributs de type entier
!>S     vals_d     :<PM_REEL,DIM=(:)>         valeurs des attributs de type réel
!>S     vals_s     :<LEN=CPS_MAXLG,DIM=(:)>   valeurs des attributs de type chaîne de caractères
!>S     unites     :<LEN=CPS_MAXLG,DIM=(:)>   unités des attributs de type réel
!>S     att_def    :<logical,DIM=(:)>         tableau de booléens indiquant pour chaque attribut s'il est défini ou non
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
    implicit none
    ! arguments
    integer, intent(in) :: acces
    type(cpsi_desc), intent(in) :: desc
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: noms_att
    integer, dimension(:), intent(out) :: types_att
    integer, dimension(:), intent(out) :: vals_i
    real(KIND=PM_REEL), dimension(:), intent(out) :: vals_d
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: vals_s
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: unites
    logical, dimension(:), intent(out) :: att_def
    
    ! variables locales
    integer :: i, nb
    integer :: v_i
    real(KIND=PM_REEL) :: v_d
    character(LEN=CPS_MAXLG) :: v_s, v_uni

    nb = desc%nbChamps

    do i=1, nb
       unites(i) = ""
       noms_att(i) = trim(desc%infosChamps(i)%nom)
       types_att(i) = desc%infosChamps(i)%type
       select case (types_att(i))
       case (CPS_ENTIER)
          ret = acc_geti(acces, trim(noms_att(i)), v_i)
          if (ret==0) then
             vals_i(i) = v_i
          else
             vals_i(i) = desc%infosChamps(i)%vd_i
          end if
       case (CPS_REEL)
          unites(i) = trim(desc%infosChamps(i)%unite)
          ret = acc_get(acces, trim(noms_att(i)), v_s, v_uni)
          if (trim(v_uni).eq."undef") then
             ! la valeur existe, mais est non définie
             att_def(i) = .false.
          else
             ret = acc_getd(acces, trim(noms_att(i)), v_d, unites(i))
             ! le reel est defini
             att_def(i) = .true.
             ! valeur absente ou unité erronée
             if (ret/=0) att_def(i) = .false.
          endif
          vals_d(i) = v_d
          if(.not. att_def(i)) vals_d(i) = desc%infosChamps(i)%vd_d
       case (CPS_STRING)
          ret = acc_gets(acces, trim(noms_att(i)), v_s)
          if (ret==0) then
             vals_s(i) = trim(v_s)
          else
             vals_s(i) = trim(desc%infosChamps(i)%vd_s)
          end if
       end select
    end do
       
  end subroutine cpsi_readStructure

  subroutine cps_close_base()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_close_base
!
!$Resume
!
!$Description
!  Routine de fermeture du module.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_close_base()
!
!$Arguments
!
!$Common
!
!$Routines
!- cpsi_close_desc
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
    
    ! deallocations memoire des modules cps_desc et cps_accesMAdona
    call cpsi_close_desc()
    cps_base_init = .false.
  end subroutine cps_close_base

  subroutine cps_get_sautTAITUC(date,delta,dernier_appel, echel_tuc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_get_sautTAITUC
!
!$Resume
!
!$Description
!  Routine de calcul du delta du au saut de tuc a une date donnee
!
!$Auteur
!  Camille Hue (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_get_sautTAITUC(date,delta)
!
!$Arguments
!>E     date      :<tm_jour_sec>                 Date a laquelle on veut connaitre le delta
!>S     delta     :<integer>                     Delta du saut du TUC (en sec)
!>[E]   dernier_appel :<logical>                 vaut true pour libérer de la mémoire
!>[E]   echel_tuc :<logical>                     Vaut true si date est en TUC, false s'elle est en TAI
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

    ! Arguments
    type(tm_jour_sec), intent(IN) :: date
    integer, intent(OUT) :: delta
    logical, intent(IN), optional :: dernier_appel
    logical, intent(IN), optional :: echel_tuc

    ! Variables
    integer :: ii, trouve
    character(LEN=256) :: fichier
    integer, save :: nb_saut_tuc
    integer :: indice_delta
    type(tm_jour_sec), dimension(:), pointer, save :: date_saut_tuc => NULL()
    real(pm_reel),     dimension(:), pointer, save :: delta_saut_tuc => NULL()
    logical, save :: fichier_lu = .false.
    logical :: tuc_date
    type(tm_jour_sec) :: date_loc
    integer :: k
    real(kind=pm_reel) :: reste
 
    ! On initialise boolean de l'échelle de la date d'entrée
    if (present(echel_tuc)) then
       tuc_date = echel_tuc
    else
       tuc_date = .true.
    end if

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_get_sautTAITUC", &
            partie_variable="cpsi_getCritereStructure_si")
       return
    end if

    if (.not.fichier_lu) then
       ! Recherche du nom de fichier des sauts du TUC
       call cps_getFichierTaituc(fichier, trouve)
       if ( MSP_gen_messages("cps_get_sautTAITUC") ) return  

       ! Appel a la fonction de lecture du fichier de sauts du TUC
       call cpsi_get_sautTUC(fichier, nb_saut_tuc, date_saut_tuc, delta_saut_tuc)
       if (MSP_gen_messages("cps_get_sautTAITUC")) return

       fichier_lu = .true.
    endif

    ! Calcul du delta
    ! Si le nombre de saut du tuc est nul, le delat vaut 0
    if (nb_saut_tuc.eq.0) then
       delta = 0
    else
       ! Si la date est inferieure au premier saut du TUC, le delta vaut 0
       if (date%jour < date_saut_tuc(1)%jour) then
          delta = 0
       else
          ! On doit faire une recherche sur le tableau
          indice_delta = 0
          do ii=2,nb_saut_tuc
             if (date%jour < date_saut_tuc(ii)%jour) then
                ! On a trouve, on enregistre l indice precedent et on sort de la boucle
                indice_delta = ii-1
                exit
             endif
          end do
          ! Si on n a pas trouve l indice, c est que la date a la date du dernier saut du tuc
          ! et on retourne le dernier delta
          if (indice_delta.eq.0) then
             indice_delta = nb_saut_tuc
          endif
          ! On recupere le delta dans le tableau
          delta = nint(delta_saut_tuc(indice_delta))

             ! L'échelle de la date d'entrée est TAI
             if (.not.tuc_date) then
                ! On doit faire une première recherche sur le tableau
                ! pour transformer la date en UTC (au desus)
                date_loc%jour = date%jour
                date_loc%sec  = date%sec
                ! Maintenant on calcule la date en UTC
                date_loc%sec  = date_loc%sec - real(delta, kind=pm_reel) + 86400._pm_reel
                ! calcul du nombre de jours lies aux secondes
                reste = date_loc%sec / 86400._pm_reel
                k     = floor(reste)
                ! calcul de la quantite normalisee, k vaut 0 ou 1
                date_loc%jour = date_loc%jour + k - 1_pm_entier
                date_loc%sec  = date_loc%sec - real(k, kind=pm_reel) * 86400._pm_reel

                ! On répete la recherche, maintenant en UTC
                indice_delta = 0
                do ii=2,nb_saut_tuc
                   if (date_loc%jour < date_saut_tuc(ii)%jour) then
                      ! On a trouve, on enregistre l indice precedent et on sort de la boucle
                      indice_delta = ii-1
                      exit
                   end if
                end do
                ! Si on n a pas trouve l indice, c est que la date a la date du dernier saut du tuc
                ! et on retourne le dernier delta
                if (indice_delta.eq.0) then
                   indice_delta = nb_saut_tuc
                end if
                ! On recupere le delta dans le tableau
                delta = nint(delta_saut_tuc(indice_delta))

             end if
       endif
    endif

    ! On ne desalloue pas les pointeurs puisqu ils sont sauvegardes pour un prochain appel
    ! sauf si c'est demandé explicitement
    if (present(dernier_appel)) then
       if (dernier_appel) then
          if (associated(date_saut_tuc)) deallocate(date_saut_tuc)
          if (associated(delta_saut_tuc)) deallocate(delta_saut_tuc)
       end if
       ! Mise a jour du flag de lecture
       fichier_lu = .false.
    end if

  end subroutine cps_get_sautTAITUC

      subroutine cps_getFichierTaituc(fichierTaituc,trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierTaituc
!
!$Resume
!
!$Description
!  Obtenir le fichier de sauts du tuc
!
!$Auteur
!  Camille Hue (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierTaituc(fichierTaituc, trouve)
!.    character(LEN=*) :: fichierTaituc
!
!$Arguments
!>S     fichierTaituc  :<LEN=*>     fichier de sauts du tuc
!>S     trouve      :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
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
    character(LEN=*), intent(out) :: fichierTaituc
    integer, intent(out) :: trouve
    
    ! variables locales
    integer :: i, acces, iostat
    character(LEN=CPS_MAXLG) :: rep_tmp, f_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers => NULL()
    logical :: exist

    if (.not.cps_base_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichierPotentiel", &
            partie_variable="cps_getFichierTaituc")
       return
    end if

    trouve = CPS_ERR_DEF
    call cps_getListFichiersCateg(CPS_CATEG_TAITUC, listFichiers)
    do i=1, cpsi_size_ptr(listFichiers)
       call cpsi_getAccesMadona(listFichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichierTaituc", &
               partie_variable=trim(listFichiers(i)))
          return
       end if
       f_tmp=""
       trouve = cps_getCritere(acces, "code", 0, &
            "fichier_taituc", f_tmp)
       rep_tmp = ""
       trouve = cps_getCritere(acces, "code", 0, &
            "repertoire_taituc", rep_tmp)
       f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)
       fichierTaituc = trim(f_tmp)
       if (trouve.eq.CPS_OK) then
          ! on regarde d'abord si le fichier est dans la base locale
          fichiertaituc = trim(rep_base_local)//"/"//trim(f_tmp)
          inquire(file=trim(fichierTaituc), exist=exist)
          if (.not.exist) then
             ! si le fichier n'est pas dans la base locale, il est
             ! dans la base de reference
             fichierTaituc = trim(rep_base_ref)//"/"//trim(f_tmp)
          end if
       endif

       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do
             
    ! liberation memoire
    if (associated(listFichiers)) then
       deallocate(listFichiers, stat=iostat)
    end if

  end subroutine cps_getFichierTaituc


  subroutine cpsi_get_sautTUC(fichier, nb_saut_tuc, date_saut_tuc, delta_saut_tuc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_get_sautTUC
!
!$Resume
!
!$Description
!  Routine interne de lecture du fichier de saut du TUC
!
!$Auteur
!  Camille Hue (Atos Origin)
!
!$Acces
!  PRIVEE
!
!$Usage
!  call cpsi_get_sautTUC(fichier, nb_saut_tuc, date_saut_tuc, delta_saut_tuc)
!
!$Arguments
!>E     fichier       :<character(len=*)>         Nom du fichier de saut du TUC
!>S     nb_saut_tuc   :<integer>                  Nombre de sauts du TUC
!>S     date_saut_tuc :<tm_jour_sec,pointer(:)>   Dates des sauts du TUC
!>S     delta_saut_tuc:<pm_reel>                  TAI-TUC pour ces dates
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use eph_util, only : eph_util_ficunit90
  
    implicit none

    ! Declarations, des arguments
    ! ===========================
    character(len=*), intent(in)  :: fichier
    integer,          intent(out) :: nb_saut_tuc
    type(tm_jour_sec), dimension(:), pointer :: date_saut_tuc
    real(pm_reel),     dimension(:), pointer :: delta_saut_tuc

    !Declarations des variables
    !==========================
    integer :: unific      ! unite logique du fichier
    integer :: ier ! retour de eph_util_ficunit90
    character(len=*), parameter :: status_fichier="old"         ! attibut status
    character(len=*), parameter :: acces_fichier="sequential"   ! attibut acces
    character(len=*), parameter :: format_fichier="formatted"   ! attibut format
    character(len=*), parameter :: position_fichier="rewind"    ! attibut position
    character(len=*), parameter :: action_fichier="read"        ! attibut action
    integer :: resultat    ! status de l'operation "open"
    ! format de lecture d'une chaine de carateres
    character(len=*), parameter :: format_lect_ligne='(A)'
    ! formats de lecture des lignes du fichier sauts de TUC
    character(len=*), parameter :: format_lect_saut_tuc='(I5,7X,F8.3)'
    character(len=*), parameter :: diese='#'     ! caractere en 1ere colonne identifiant une ligne a eliminer
    character(len=500) :: ligne      ! chaine de caracteres contenant une ligne entiere lue
    integer            :: cr_lect    ! compte rendu d'une lecture
    integer(pm_entier) :: jjd        ! date de debut de validite du saut
    real(pm_reel)      :: ect        ! ecart constate entre les deux echelles de temps
    integer            :: nsaut      ! nombre local de sauts de tuc lus

    ! initialisation a 0 de l'unite logique avant affectation dynamique
    ! .................................................................
    unific = 0

    ! rechercher la presence du fichier des sauts de TUC a partir de son nom
    ! ......................................................................
    call cpsi_verif_ficsautTUC(fichier)
    if (MSP_gen_messages("cpsi_get_sautTUC")) return

    ! allouer une unite logique disponible pour ouvrir ce fichier
    ! ...........................................................

    call eph_util_ficunit90(unific,ier) 
    if (MSP_gen_messages("cpsi_get_sautTUC")) return

    ! ouvrir en lecture ce fichier pointeur au debut des donnees
    ! ..........................................................

    ! ouverture du fichier sequentiel ASCII 
    ! en acces lecture, le pointeur etant en debut de fichier
    ! .......................................................
    open(UNIT=unific,&
         FILE=fichier, &
         STATUS=status_fichier, &
         ACCESS=acces_fichier, &
#ifdef __GFORTRAN__
         CONVERT='big_endian', &
#endif
         FORM=format_fichier, &
         POSITION=position_fichier, &
         ACTION=action_fichier, &
         IOSTAT=resultat) 

    if (resultat /= 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OUV_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

    ! lire les donnees de ce fichier
    ! ..............................

    ! initialisation du compte rendu du read
    ! .................................................
    cr_lect = 0

    ! initialisation du nombre de sauts avant lecture
    ! ...............................................
    nsaut = 0

    ! Nullify sur les pointeurs
    ! ...................................................................
    nullify(date_saut_tuc)
    nullify(delta_saut_tuc)

    ! lecture du fichier en ne tenant pas compte des lignes debutant par le
    ! caractere "#" en 1ere colonne
    ! Le champ secondes de la date tm_jour_sec est arbitrairement initialise
    ! a 0._pm_reel car le fichier ne contient que le jour Julien entier.
    ! .....................................................................
    ! Premier passage poiur recuperer le nombre de sauts
    do while (cr_lect == 0    )
       read(UNIT=unific, FMT=format_lect_ligne, IOSTAT=cr_lect) ligne
       if (cr_lect == 0 .and. len_trim(ligne) /= 0 .and. ligne(1:1) /= diese) then
          read(ligne, FMT=format_lect_saut_tuc, IOSTAT=cr_lect) jjd, ect
          if (cr_lect == 0) nsaut = nsaut + 1
       end if
    end do
    ! traitement du cas d'erreur de lecture
    ! Les proprietes du parametre IOSTAT permettent de s'affranchir 
    ! du parametre END, dand la gestion de sortie du READ (cf MPM Fortran90)
    ! .....................................
    if (cr_lect > 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_LECT_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

    ! Allocations des tableaux et initialisation
    allocate(date_saut_tuc(nsaut))
    allocate(delta_saut_tuc(nsaut))
    date_saut_tuc(:)%jour = 0
    date_saut_tuc(:)%sec = 0._pm_reel
    delta_saut_tuc(:) = 0._pm_reel
    
    ! Deuxieme passage pour remplir les tableaux
    ! on rembobine le fichier en fermant et re-ouvrant
    close(UNIT=unific, IOSTAT=resultat)
    if (resultat /= 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_FERME_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

    open(UNIT=unific,&
         FILE=fichier, &
         STATUS=status_fichier, &
         ACCESS=acces_fichier, &
#ifdef __GFORTRAN__
         CONVERT='big_endian', &
#endif
         FORM=format_fichier, &
         POSITION=position_fichier, &
         ACTION=action_fichier, &
         IOSTAT=resultat) 
    if (resultat /= 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OUV_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if
    
    cr_lect = 0
    nsaut = 0
    do while (cr_lect == 0    )
       read(UNIT=unific, FMT=format_lect_ligne, IOSTAT=cr_lect) ligne
       if (cr_lect == 0 .and. len_trim(ligne) /= 0 .and. ligne(1:1) /= diese) then
          read(ligne, FMT=format_lect_saut_tuc, IOSTAT=cr_lect) jjd, ect
          if (cr_lect == 0) then
             nsaut = nsaut + 1
             date_saut_tuc(nsaut)%jour = jjd
             date_saut_tuc(nsaut)%sec = 0._pm_reel
             delta_saut_tuc(nsaut) = ect
          end if
       end if
    end do

    ! traitement du cas d'erreur de lecture
    ! Les proprietes du parametre IOSTAT permettent de s'affranchir 
    ! du parametre END, dand la gestion de sortie du READ (cf MPM Fortran90)
    ! .....................................
    if (cr_lect > 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_LECT_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

    ! affectation du nombre de sauts de TUC
    ! .....................................
    nb_saut_tuc = nsaut

    ! fermer (et desallouer l'unite) de ce fichier apres lecture
    ! ..........................................................
    close(UNIT=unific, IOSTAT=resultat)
    
    if (resultat /= 0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_FERME_FIC_TUC", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

  end subroutine cpsi_get_sautTUC

  subroutine cpsi_verif_ficsautTUC(fichier)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_verif_ficsautTUC
!
!$Resume
!
!$Description
!  Routine interne de controle que le fichier de saut du TUC n'est pas déjà ouvert
!  Cette génère au besoin une erreur MAGE  
!
!$Auteur
!  Camille Hue (Atos Origin)
!
!$Acces
!  PRIVEE
!
!$Usage
!  call cpsi_verif_ficsautTUC(fichier)
!
!$Arguments
!>E     fichier       :<character(len=*)>         Nom du fichier de saut du TUC
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

    character(len=*), intent(in)  :: fichier
    logical :: fichier_existe       ! indicateur d'existence du fichier
    logical :: fichier_ouvert       ! indicateur d'ouverture du fichier
    

    ! initialisation des valeurs de retour de la fonction inquire
    ! ...........................................................
    fichier_existe = .false.
    fichier_ouvert = .false.

    ! Recherche du fichier 'fichier' non ouvert
    ! ....................................................
    inquire(FILE=fichier, EXIST=fichier_existe, OPENED=fichier_ouvert)

    if(.not.fichier_existe) then
       call MSP_signaler_message(cle_mes="CPS_ERR_FIC_TUC_ABSENT", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    endif
    if(fichier_ouvert) then
       call MSP_signaler_message(cle_mes="CPS_ERR_FIC_TUC_OUVERT", &
            routine="cpsi_get_sautTUC", &
            partie_variable="cps_base")
       return
    end if

  end subroutine cpsi_verif_ficsautTUC

end module cps_base
