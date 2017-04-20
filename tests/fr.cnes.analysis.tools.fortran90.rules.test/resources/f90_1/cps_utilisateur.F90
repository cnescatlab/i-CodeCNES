module cps_utilisateur

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_utilisateur
!
!$Resume
!
!$Description
!  Il s'agit du module correspondant à l'interface utilisateur de 
!  haut niveau de la bibliothèque.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: cps_utilisateur.F90 360 2013-02-15 11:38:21Z aadt $
!
!$Historique
!  $Log: cps_utilisateur.F90,v $
!  Revision 360  2013/02/14 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.54  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.53  2010/10/21 10:44:16  ogarat
!  VERSION::FA-ID:1451:21/10/2010:Mise a jour des routines de lecture pour l'atmosphere tabulee
!
!  Revision 1.52  2010/05/05 10:31:33  jlrobin
!  V2.9::FA-ID:1327:05/05/2010:variables inutilisees a retirer
!
!  Revision 1.51  2010/04/30 14:12:31  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.50.2.1  2010/02/23 11:51:26  cmartel
!  VERSION::DM-ID:1361:Ajout de routines d'accès aux fichiers d'atmosphère tabulée
!
!  Revision 1.50  2009/11/02 10:05:19  cmartel
!  DM-ID 842 : Ajout des routines d'extraction des donnes d'act. sol. reelles
!  Revision 1.49  2008/10/28 16:40:32  cml
!  FA-ID 1061 : Mise a jour de la propagation d une evenetuelle erreur durant l init. de la base
!  Revision 1.48  2008/10/28 12:43:52  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!  Revision 1.47  2008/10/03 13:34:30  cml
!  FA-ID 1024 : Correction de nommage des variables (min, max, index, et mod)
!  Revision 1.46  2008/08/04 13:07:55  gss
!  DM-ID 1058 : (portage g95) Initialisation à NULL des pointeurs lors de leur
!  déclaration. Suppression des variables non utilisées. Suppression des labels
!  (format) non utilisés. Initialisation des sorties de fonction dépacée avant
!  le premier return.
!  Revision 1.45  2008/07/04 11:42:31  huec
!  DM-ID 1058 : Ameliorations de la gestion memoire de la structure desc
!  Revision 1.44  2008/03/04 08:15:45  vivaresf
!  FA-ID 892 : initialisation des booléens en sortie de
!  cpsi_chercherBullAPredAntePost
!  Revision 1.43  2008/02/29 16:55:27  vivaresf
!  FA-ID 892/893 : initialisations des pointeurs et des valeurs
!  allouées dans les variables pointées
!  Revision 1.42  2008/02/08 08:50:39  vivaresf
!  FA-ID 892, 893 : initialisation des maillons des listes
!  chaînées pour éviter la disparition d'une valeur
!  Revision 1.41  2007/07/03 16:08:40  vivaresf
!  FA-ID 746 : initialisation des pointeurs à leur création
!  Revision 1.40  2007/06/20 08:51:21  vivaresf
!  FA-ID 746 : désallocation des variables de type cpsi_desc
!  Revision 1.39  2007/06/18 08:51:56  vivaresf
!  FA-ID 746 : deallocate des variables locale pointeur
!  suppression des codes commentés
!  Revision 1.38  2007/06/15 14:59:28  bibms
!  FA-ID 746 : desallocation de modCourant pour eviter une fuite memoire
!  Revision 1.37  2007/05/21 06:56:21  vivaresf
!  Version 2.2 : révision des cartouches
!  Revision 1.36  2007/05/03 15:08:30  couturis
!  DM-538: Ajout de cps_setCodeTheorie et cps_CodeToTheorie qui demande le code de la theorie planetaire en entree.
!  Revision 1.35  2007/02/01 16:43:15  vivaresf
!  Version 2.2a1 : utilisation d'une variable locale
!  pour éviter un core avec un test qui ne ferait pas d'évaluation paraisseuse
!  Revision 1.34  2006/10/26 17:17:37  vpg
!  DM-ID 462 : amélioration de l'accès à la description du contenu de la base de données COMPAS
!  Revision 1.33  2006/10/18 09:52:56  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.31.2.1  2006/09/26 12:13:36  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.31  2006/08/30 09:38:52  vivaresf
!  FA-ID 576 : initailisation de f_tmp
!  Revision 1.30  2006/08/28 16:23:43  vpg
!  FA-ID 572 : blocage de l'IHM cps_acces de COMPAS_UI
!  Revision 1.29  2006/07/04 12:04:55  okd
!  Correction d'un message foresys
!  Revision 1.28  2006/07/03 13:51:12  vpg
!  Correction qualite : reduction du nombre d'imbrications
!  Revision 1.27  2006/05/30 12:29:04  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.26  2006/05/30 08:27:47  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.25  2006/05/15 15:06:40  vpg
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.24  2006/05/02 09:37:17  vpg
!  Diminution de la complexite de certaines fonctions pour respecter les seuils des metriques
!  Revision 1.23  2006/04/19 13:36:20  vpg
!  Utilisation des donnees issues du bulletin A et du fichier ACSOL2 provenant d'une base locale
!  Revision 1.22  2006/04/13 14:04:55  bouillaj
!  Modification d'affichage
!  Revision 1.21  2006/04/12 09:20:48  vpg
!  Ajout de l'interface cps_requete() et des fonctions cpsi_requeteInt(), cpsi_requeteReel() et cpsi_requeteString()
!  Revision 1.20  2006/04/06 15:29:45  vpg
!  Les predictions effectuees a un jour donne sont celles effectuees le plus recemment a partir de cette date
!  Revision 1.19  2006/03/27 10:13:43  vpg
!  Rajout du repertoire de la base en option du nom du fichier pour cps_getFichierPotentiel() et cps_getFichierModele()
!  Revision 1.18  2006/03/24 13:09:20  vpg
!  Ajout de la fonction cps_getFichierModele()
!  Revision 1.17  2006/03/20 15:52:44  vpg
!  Mise a jour des cartouches
!  Revision 1.16  2006/03/10 12:45:50  vpg
!  Mise a jour suite a la modification sur cpsi_getAtt_d() ou l'unite est obligatoire
!  Revision 1.15  2006/03/03 16:08:39  vpg
!  Utilisation de cpsi_compareReels() pour la comparaison de reels et rajout de att_def pour cps_getInfosCorps() indiquant si un attribut est defini ou non
!  Revision 1.14  2006/02/23 15:54:35  vpg
!  Ajout des fonctions cps_getBullASureCreneau() et cps_getAcsol2SureCreneau()
!  Revision 1.13  2006/02/21 14:54:44  vpg
!  Correction de la fonction cps_getBullADernierePred() : ajout du chemin rep_base_ref au fichier
!  Revision 1.12  2006/02/21 10:14:47  vpg
!  Ajout des routines et des fonctions relatives a l'utililsation des donnees issues du fichier ACSOL2 : 
! cps_getFichierAcsol2Sure(), cps_getFichierAcsol2Pred(), cps_getAcsol2Sure(), cps_getAcsol2DernieresPred(), cps_getAcsol2PredDate(), cps_getAcsol2Pred(), cpsi_getAcsol2PredFichier(), cps_getAcsol2DernierePred()
!  Revision 1.11  2006/02/20 10:34:33  vpg
!  Ajout de
!  Revision 1.10  2006/02/09 14:56:59  vpg
!  Pour la routine cps_getListAttCateg(), on ne tient pas compte de theorie
!  Revision 1.7  2006/02/08 17:29:22  vpg
!  Modification mineure d'un argument
!  Revision 1.6  2006/02/06 15:18:17  vpg
!  ajout des routines cps_getListFichiersLocal(), cps_getListFichiersRef et cps_setBaseLocale()
!  Revision 1.5  2006/01/23 14:05:17  vpg
!  rajout des traitements MAGE, rajout de fonctions liees aux ihm
!  Revision 1.4  2005/12/09 10:29:21  vpg
!  Modification de la recherche des constantes d'un corps ainsi que des parametres kepleriens : recherche dans tous les fichiers de la categorie concernee au lieu d'un seul fichier
!  Revision 1.3  2005/12/08 18:20:33  vivaresf
!  Cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_utilisateur
!
!$Structure
!
!: maillon_s : 
!>     val       : <LEN=CPS_MAXLG>      
!>     suivant   : <maillon_s,pointer>  
!
!: maillon_i : 
!>     val       : <integer>            
!>     suivant   : <maillon_i,pointer>  
!
!$Global
!
!>  cps_utilisateur_init   : <logical,public>             
!#V
!>  ret                    : <integer,private>            
!>  iostat                 : <integer,private>            
!>  undef_vali             : <integer,parameter,private>  
!>  theorie_courante       : <LEN=CPS_MAXLG,private>      
!#
!$Common
!
!$Routines
!- cps_getAtt
!- cps_getCorps
!- cps_requete
!- cps_init_utilisateur
!- cps_setTheorie
!- cps_setCodeTheorie
!- cps_CodeToTheorie
!- cps_getTheorie
!- cps_getListCorps
!- cps_getCorpsTheorieDef
!- cps_getCorpsTheorieCouranteDef
!- cps_getListAttCateg
!- cps_getInfosCorps
!- cps_getInfosCorpsSansDoublon
!- cps_getNoms
!- cps_intersecterCodes
!- cpsi_intersecter
!- cps_getListTheories
!- cps_getListFichiersLocal
!- cps_getListFichiersRef
!- cps_setBaseLocale
!- cpsi_chercherBullAPredAntePost
!- cps_getBullADernieresPred
!- cpsi_lireBullAPred
!- cps_extraireAcsol2Sure
!- cps_extraireAcsol2Pred
!- cpsi_extraireDonneesAcsol2
!- cps_getAcsol2DernieresPred
!- cpsi_lireAcsol2Pred
!- cpsi_getAcsol2PredFichier
!- cps_getListeFichiersAtmTab
!- cps_getListeFichiersAcsol2
!- cps_close_utilisateur
!#V
!- cpsi_getCorps_i
!- cpsi_getCorps_s
!- cpsi_getCorps_d
!#
!
!$Fonctions
!- cpsi_getAtt_i
!- cpsi_getAtt_d
!- cpsi_getAtt_s
!- cps_getCsteGenTh
!- cps_getCsteGenThCourante
!- cps_getCsteTh
!- cps_getCsteThCourante
!- cps_getKeplerTh
!- cps_getKeplerThCourante
!- cpsi_codeExisteDeja
!- cpsi_getCorpsFichier
!- cpsi_isCorpsDef
!- cps_existeAtt
!- cpsi_getValCorpsFichier_i
!- cpsi_getValCorpsFichier_d
!- cpsi_getValCorpsFichier_s
!- cps_getFichiersPotentielCorps
!- cps_getModelesPotentielCorps
!- cps_getListeFichiersPotentiel
!- cps_getListeModelesPotentiel
!- cps_getFichierPotentiel
!- cpsi_intInTab
!- cpsi_nomExistedeja
!- cps_getListModelesCorps
!- cps_getFichierModele
!- cps_getListEltsCateg
!- cps_getFichierBullASure
!- cps_getFichierBullAPred
!- cps_getBullASure
!- cps_getBullASureCreneau
!- cps_getBullAPred
!- cps_getBullADatePred
!- cps_getBullADernierePred
!- cps_getFichierAcsol2Sure
!- cps_getFichierAcsol2Pred
!- cps_getAcsol2Sure
!- cps_getAcsol2SureCreneau
!- cps_getAcsol2DatePred
!- cps_getAcsol2Pred
!- cps_getAcsol2DernierePred
!- cps_getFichierAtmTab
!- cps_getFichierAcsol2
!- cpsi_requeteInt
!- cpsi_requeteReel
!- cpsi_requeteString
!
!$Include
!
!$Module
!#V
!- cps_base
!- cps_bullA
!- cps_acsol2
!- cps_accesMadona
!- cps_desc
!#
!
!$Interface
!> cps_getatt :    cpsi_getAtt_i, cpsi_getAtt_d, cpsi_getAtt_s
!> cps_getcorps :  cpsi_getCorps_i, cpsi_getCorps_s, cpsi_getCorps_d
!> cps_requete :   cpsi_requeteInt, cpsi_requeteReel, cpsi_requeteString
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  cpsi_getAtt_i cpsi_getAtt_d cpsi_getAtt_s cpsi_requeteInt cpsi_requeteReel cpsi_requeteString
!.  cpsi_getCorps_i cpsi_getCorps_s cpsi_getCorps_d
!#
!.  cps_getCsteGenTh cps_getCsteGenThCourante cps_getCsteTh cps_getCsteThCourante cps_getKeplerTh
!.  cps_getKeplerThCourante cpsi_codeExisteDeja cpsi_getCorpsFichier cpsi_isCorpsDef cps_existeAtt
!.  cpsi_getValCorpsFichier_i cpsi_getValCorpsFichier_d cpsi_getValCorpsFichier_s cps_getFichiersPotentielCorps
!.  cps_getModelesPotentielCorps cps_getListeFichiersPotentiel cps_getListeModelesPotentiel
!.  cps_getFichierPotentiel cpsi_intInTab cpsi_nomExistedeja cps_getListModelesCorps cps_getFichierModele
!.  cps_getListEltsCateg cps_getFichierBullASure cps_getFichierBullAPred cps_getBullASure
!.  cps_getBullASureCreneau cps_getBullAPred cps_getBullADatePred cps_getBullADernierePred
!.  cps_getFichierAcsol2Sure cps_getFichierAcsol2Pred cps_getAcsol2Sure cps_getAcsol2SureCreneau
!.  cps_getAcsol2DatePred cps_getAcsol2Pred cps_getAcsol2DernierePred cps_getFichierAtmTab
!.  cps_getFichierAcsol2 cps_getAtt cps_getCorps cps_requete cps_init_utilisateur cps_setTheorie
!.  cps_setCodeTheorie cps_CodeToTheorie cps_getTheorie cps_getListCorps cps_getCorpsTheorieDef
!.  cps_getCorpsTheorieCouranteDef cps_getListAttCateg cps_getInfosCorps cps_getInfosCorpsSansDoublon
!.  cps_getNoms cps_intersecterCodes cpsi_intersecter cps_getListTheories cps_getListFichiersLocal
!.  cps_getListFichiersRef cps_setBaseLocale cpsi_chercherBullAPredAntePost cps_getBullADernieresPred
!.  cpsi_lireBullAPred cps_extraireAcsol2Sure cps_extraireAcsol2Pred cpsi_extraireDonneesAcsol2
!.  cps_getAcsol2DernieresPred cpsi_lireAcsol2Pred cpsi_getAcsol2PredFichier cps_getListeFichiersAtmTab
!.  cps_getListeFichiersAcsol2 cps_close_utilisateur
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use cps_base
  use cps_bullA
  use cps_acsol2

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_utilisateur.F90 360 2013-02-15 11:38:21Z aadt $'


  logical, public :: cps_utilisateur_init = .false.

  ! Variables de robustesse (retour, status de desallocation), internes
  integer, private :: ret, iostat

  ! Initialisation des entiers dans les maillons
  integer, parameter, private :: undef_vali=-9999

  ! Theorie courante (variable interne)
  character(LEN=CPS_MAXLG), private :: theorie_courante

  interface cps_getAtt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAtt
!
!$Resume
!  Valeur d'un attribut (categorie corps)
!
!$Description
!  Interface permettant d'otenir la valeur d'un attribut d'un corps
!  dans la categorie 'corps'.
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAtt(code_corps, nom_att, vres)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    integer :: vres
!.    integer :: trouve
!
!  trouve = cps_getAtt(code_corps, nom_att, vres, [unite])
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=CPS_MAXLG) :: unite
!.    integer :: trouve
!
!  trouve = cps_getAtt(code_corps, nom_att, vres)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Procedures
!#V
!- cpsi_getAtt_i
!- cpsi_getAtt_d
!- cpsi_getAtt_s
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cpsi_getAtt_i, cpsi_getAtt_d, cpsi_getAtt_s
  end interface

  interface cps_getCorps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorps
!
!$Resume
!  liste des corps
!
!$Description
!  Interface permettant d'obtenir la liste des codes des corps dont un 
!  attribut est à une valeur particulière.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorps(nom_att, val_att, listeCorps)
!.    character(LEN=*) :: nom_att
!.    integer :: val_att
!.    integer, dimension(:), pointer :: listeCorps
!
!  call cps_getCorps(nom_att, val_att, listeCorps)
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: val_att
!.    integer, dimension(:), pointer :: listeCorps
!
!  call cps_getCorps(nom_att, listCorps, [val], [valeurmin], [valeurmax])
!.    character(LEN=*) :: nom_att
!.    integer, dimension(:), pointer :: listCorps
!.    real(KIND=PM_REEL) :: val, valeurMin, valeurMax
!
!$Procedures
!#V
!- cpsi_getCorps_i
!- cpsi_getCorps_s
!- cpsi_getCorps_d
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cpsi_getCorps_i, cpsi_getCorps_s, cpsi_getCorps_d
  end interface

  interface cps_requete

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_requete
!
!$Resume
! extraction un attribut
!
!$Description
!  V2.0
!  Cette interface permet d'extraire un attribut (de type entier, reel 
!  ou chaine de caracteres) dans une structure MADONA. Il faut fournir
!  la categorie, l'id et le nom de la structure MADONA.
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_requete(categorie, cle, id, nom, val)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    integer :: val
!.    integer :: trouve
!
!  trouve = cps_requete(categorie, cle, id, nom, val, unite)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!  trouve = cps_requete(categorie, cle, id, nom, val)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    character(LEN=*) :: val
!.    integer :: trouve
!
!$Procedures
!#V
!- cpsi_requeteInt
!- cpsi_requeteReel
!- cpsi_requeteString
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cpsi_requeteInt, cpsi_requeteReel, cpsi_requeteString
  end interface

  private :: cpsi_getAtt_i, cpsi_getAtt_d, cpsi_getAtt_s
  private :: cpsi_getCorps_i, cpsi_getCorps_s, cpsi_getCorps_d
  private :: cpsi_requeteInt, cpsi_requeteReel, cpsi_requeteString


  type maillon_i
     integer :: val
     type(maillon_i), pointer :: suivant
  end type maillon_i

  type maillon_s
     character(LEN=CPS_MAXLG) :: val
     type(maillon_s), pointer :: suivant
  end type maillon_s

contains

  subroutine cps_init_utilisateur()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_init_utilisateur
!
!$Resume
!  Routine d'initialisation du module.
!
!$Description
!  Routine d'initialisation du module.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_init_utilisateur()
!
!$Arguments
!
!$Common
!
!$Routines
!- cps_init_base
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
    call cps_init_base()
    if ( MSP_gen_messages("cps_init_utilisateur") ) return

    theorie_courante = "UAI1994"
    cps_utilisateur_init = .true.
  end subroutine cps_init_utilisateur

  function cpsi_getAtt_i(code_corps, nom_att, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getAtt_i
!
!$Resume
!  extraction d'un attribut de type entier
!
!$Description
!  Fonction d'obtention de la valeur d'un attribut de type entier pour 
!  un corps définit dans la catégorie 'corps'.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_getAtt_i(code_corps, nom_att, vres)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    integer :: vres
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   code du corps
!>E     nom_att     :<LEN=*>     nom de l'attribut recherché
!>S     vres        :<integer>   valeur de l'attribut recherché
!>S     trouve      :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    integer, intent(out) :: vres

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersCorps => NULL()
    integer :: nbFichiers, i, acces
    character(LEN=CPS_MAXLG) :: cle

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getAtt_i", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! obtenir la liste des fichiers de la categorie corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, listFichiersCorps)
    nbFichiers = cpsi_size_ptr(listFichiersCorps)
    
    ! chercher dans chaque fichier
    i = 1
    do
       call cpsi_getAccesMadona(listFichiersCorps(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getAtt_i", &
               partie_variable=trim(listFichiersCorps(i)))
          ! liberation memoire
          if (associated(listFichiersCorps)) deallocate(listFichiersCorps)
          return
       else
          cle = cpsi_intToChar(code_corps)
          cle = "corps"//trim(cle)//"."//trim(nom_att)
          ret = acc_exist(acces, cle)
          if (ret.eq.1) then
             ret = acc_geti(acces, cle, vres)
             trouve = CPS_OK
             exit
          else 
             i = i+1
             if (i.gt.nbFichiers) then
                exit
             end if
          end if
       end if
    end do
    
    ! liberation de la memoire
    if(associated(listFichiersCorps)) deallocate(listFichiersCorps, stat=iostat)

  end function cpsi_getAtt_i

  function cpsi_getAtt_d(code_corps, nom_att, vres, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getAtt_d
!
!$Resume
!  extraction d'un attribut de type réel
!
!$Description
!  Fonction d'obtention de la valeur d'un attribut de type réel pour 
!  un corps définit dans la catégorie 'corps'.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_getAtt_d(code_corps, nom_att, vres, [unite])
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    real(kind=PM_REEL) :: vres
!.    character(LEN=CPS_MAXLG) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>         code du corps
!>E     nom_att     :<LEN=*>           nom de l'attribut recherché
!>S     vres        :<PM_REEL>         valeur de l'attribut recherché
!>[S]   unite       :<LEN=CPS_MAXLG>   unité de l'attribut
!>S     trouve      :<integer>         CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    real(kind=PM_REEL), intent(out) :: vres
    character(LEN=CPS_MAXLG), optional, intent(out) :: unite

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersCorps => NULL()
    integer :: nbFichiers, i, acces, j
    character(LEN=CPS_MAXLG) :: cle, unite_res
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getAtt_d", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! obtenir la liste des fichiers de la categorie corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, listFichiersCorps)
    nbFichiers = cpsi_size_ptr(listFichiersCorps)

    ! chercher
    i = 1
    do
       call cpsi_getAccesMadona(listFichiersCorps(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getAtt_d", &
               partie_variable=trim(listFichiersCorps(i)))
          ! liberation memoire
          if (associated(listFichiersCorps)) deallocate(listFichiersCorps)
          return
       !else
       end if
       cle = cpsi_intToChar(code_corps)
       cle = "corps"//trim(cle)//"."//trim(nom_att)
       ret = acc_exist(acces, cle)
       if (ret.eq.1) then
          ! determination de l'unite de nom_att
          ret = cpsi_getDescFichier(listFichiersCorps(i), desc)
          do j=1, desc%nbChamps
             if (desc%infosChamps(j)%nom.eq.nom_att) then
                unite_res = trim(desc%infosChamps(j)%unite)
                if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
                exit
             end if
          end do
          if (present(unite)) then
             unite = unite_res
          end if
          
          ret = acc_getd(acces, cle, vres, unite_res)
          trouve = CPS_OK
          if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
          exit
       else 
          i = i+1
          if (i.gt.nbFichiers) then
             exit
          end if
       end if
       !end if
    end do
    
    ! liberation de la memoire
    if(associated(listFichiersCorps)) deallocate(listFichiersCorps, stat=iostat)
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)

  end function cpsi_getAtt_d


  function cpsi_getAtt_s(code_corps, nom_att, vres) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getAtt_s
!
!$Resume
!  Extraction d'un attribut de type string
!
!$Description
!  Fonction d'obtention de la valeur d'un attribut de type string  pour 
!  un corps définit dans la catégorie 'corps'.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_getAtt_s(code_corps, nom_att, vres)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: vres
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   code du corps
!>E     nom_att     :<LEN=*>     nom de l'attribut recherché
!>S     vres        :<LEN=*>     valeur de l'attribut recherché
!>S     trouve      :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    character(LEN=*), intent(out) :: vres

    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersCorps => NULL()
    integer :: nbFichiers, i, acces
    character(LEN=CPS_MAXLG) :: cle

    ! initialisations
    trouve = CPS_ERR_DEF
    vres=""

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getAtt_s", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! obtenir la liste des fichiers de la categorie corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, listFichiersCorps)
    nbFichiers = cpsi_size_ptr(listFichiersCorps)

    ! chercher
    i = 1
    do
       call cpsi_getAccesMadona(listFichiersCorps(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getAtt_s", &
               partie_variable=trim(listFichiersCorps(i)))
          ! liberation memoire
          if (associated(listFichiersCorps)) deallocate(listFichiersCorps)
          return
       else
          cle = cpsi_intToChar(code_corps)
          cle = trim(cle)
          cle = "corps"//trim(cle)//"."//trim(nom_att)
          ret = acc_exist(acces, cle)
          if (ret.eq.1) then
             ret = acc_gets(acces, cle, vres)
             trouve = CPS_OK
             exit
          else 
             i = i+1
             if (i.gt.nbFichiers) then
                exit
             end if
          end if
       end if
    end do
       
    ! liberation de la memoire
    if(associated(listFichiersCorps)) deallocate(listFichiersCorps, stat=iostat)

  end function cpsi_getAtt_s




  subroutine cps_setTheorie(nom_theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_setTheorie
!
!$Resume
!  Positionne la théorie courante.
!
!$Description
!  Positionne la théorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_setTheorie(nom_theorie)
!.    character(LEN=*) :: nom_theorie
!
!$Arguments
!>E     nom_theorie  :<LEN=*>   nom de la nouvelle théorie courante
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
    ! argument
    character(LEN=*), intent(in) :: nom_theorie

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_setTheorie", &
            partie_variable="cps_utilisateur")
       return
    end if 

    theorie_courante = nom_theorie

  end subroutine cps_setTheorie

  subroutine cps_setCodeTheorie(code_theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_setCodeTheorie
!
!$Resume
!  Positionne la théorie planétaire correspondante au code théorie entré 
!  en tant que théorie courante.
!
!$Description
!  Positionne la théorie planétaire correspondante au code théorie entré 
!  en tant que théorie courante.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_setCodeTheorie(code_theorie)
!.    integer :: code_theorie
!
!$Arguments
!>E     code_theorie  :<integer>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_CodeToTheorie
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
    integer, intent(in) :: code_theorie

    ! variable locale
    character(LEN=256) :: nom_theorie  

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_setCodeTheorie", &
            partie_variable="cps_utilisateur")
       return
    end if 

    call cps_CodeToTheorie(code_theorie,nom_theorie)
    
    theorie_courante = nom_theorie

  end subroutine cps_setCodeTheorie

  subroutine cps_CodeToTheorie(code_theorie,nom_theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_CodeToTheorie
!
!$Resume
!  Sort le nom de la théorie planétaire correspondante au code entré.
!
!$Description
!  Sort le nom de la théorie planétaire correspondante au code entré.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_CodeToTheorie(code_theorie,nom_theorie)
!.    integer :: code_theorie
!.    character(LEN=256) :: nom_theorie
!
!$Arguments
!>E     code_theorie  :<integer>   
!>S     nom_theorie   :<LEN=256>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListTheories
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
    integer, intent(in) :: code_theorie
    character(LEN=256), intent(out) :: nom_theorie
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: ptr_noms_thpla  => NULL()
    integer :: ii, ier,nb_thpla
    character(len=256) :: unit
    real(kind=PM_REEL) :: val_reelle

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_CodeToTheorie", &
            partie_variable="cps_utilisateur")
       return
    end if 

    if (associated(ptr_noms_thpla)) deallocate(ptr_noms_thpla)
    
    call cps_getListTheories(ptr_noms_thpla)
    
    nb_thpla = cpsi_size_ptr(ptr_noms_thpla)
    do ii = 1, nb_thpla
       ier = cps_getCsteGenTh(ptr_noms_thpla(ii),"code_theorie",&
            val_reelle,unit)
       if (ier == CPS_ERR_DEF) then
          call MSP_signaler_message(cle_mes="CPS_ERR_THEORIE", &
               routine="cpsi_CodeToTheorie", &
               partie_variable=trim(ptr_noms_thpla(ii)))
          return
       endif
       if (code_theorie .EQ. int(val_reelle)) nom_theorie = ptr_noms_thpla(ii)
    end do
    
    if (associated(ptr_noms_thpla)) deallocate(ptr_noms_thpla)

  end subroutine cps_CodeToTheorie


  subroutine cps_getTheorie(nom_theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getTheorie
!
!$Resume
!  Donne la théorie courante.
!
!$Description
!  Obtention de la théorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getTheorie(nom_theorie)
!.    character(LEN=*) :: nom_theorie
!
!$Arguments
!>S     nom_theorie  :<LEN=*>   nom de la théorie courante
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
    ! argument
    character(LEN=*), intent(out) :: nom_theorie
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getTheorie", &
            partie_variable="cps_utilisateur")
       return
    end if 

    nom_theorie = theorie_courante

  end subroutine cps_getTheorie


  function cps_getCsteGenTh(nom_theorie, nom_cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteGenTh
!
!$Resume
!  Extraction d'un attribut d'une theorie
!
!$Description
!  Obtention d'une valeur dans la catégorie 'theorie'.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteGenTh(nom_theorie, nom_cste, val, unite)
!.    character(LEN=*) :: nom_theorie, nom_cste
!.    real(kind=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     nom_theorie  :<LEN=*>     nom de la théorie
!>E     nom_cste     :<LEN=*>     nom de la constante
!>S     val          :<PM_REEL>   valeur de la constante
!>S     unite        :<LEN=*>     unité de la constante
!>S     trouve       :<integer>   CPS_OK si la constante est trouvée, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: nom_theorie, nom_cste
    real(kind=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersTheories => NULL()
    integer :: i, nbFichiers, acces

    ! Initialisation
    trouve = CPS_ERR_DEF
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCsteGenTh", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! obtenir la liste des fichiers de la categorie theorie
    call cps_getListFichiersCateg(CPS_CATEG_THEORIE, listFichiersTheories)
    nbFichiers = cpsi_size_ptr(listFichiersTheories)
    
    do i = 1, nbFichiers
       call cpsi_getAccesMadona(listFichiersTheories(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getCsteGenTh", &
               partie_variable=trim(listFichiersTheories(i)))
          ! liberation memoire
          if (associated(listFichiersTheories)) deallocate(listFichiersTheories)
          return
       else
          trouve = cps_getCritere(acces, 'nom', nom_theorie, nom_cste, val, unite)
          if (trouve.eq.CPS_OK) then
             exit
          end if
       end if
    end do

    ! liberation de la memoire
    if(associated(listFichiersTheories)) &
         deallocate(listFichiersTheories, stat=iostat)

  end function cps_getCsteGenTh

  function cps_getCsteGenThCourante(nom_cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteGenThCourante
!
!$Resume
!  Extraction d'un attribut de la theorie courante
!
!$Description
!  Obtention d'une valeur dans la catégorie 'theorie' selon la théorie
!  courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteGenThCourante(nom_cste, val, unite)
!.    character(LEN=*) :: nom_cste
!.    real(kind=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     nom_cste  :<LEN=*>     nom de la constante
!>S     val       :<PM_REEL>   valeur de la constante
!>S     unite     :<LEN=*>     unité de la constante
!>S     trouve    :<integer>   CPS_OK si la constante est trouvée, CPS_ERR_DEF sinon
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
    character(LEN=*), intent(in) :: nom_cste
    real(kind=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite

    ! resultat
    integer :: trouve

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCsteGenThCourante", &
            partie_variable="cps_utilisateur")
       return
    end if 
    
    trouve = cps_getCsteGenTh(theorie_courante, nom_cste, val, unite)

  end function cps_getCsteGenThCourante







  function cps_getCsteTh(code_corps, nom_theorie, cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteTh
!
!$Resume
!  Extraction d'une constante suivant une theorie et un corps
!
!$Description
!  Obtention de la valeur d'une constante d'un corps.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteTh(code_corps, nom_theorie, cste, val, unite)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_theorie, cste
!.    real(kind=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     code_corps   :<integer>   code du corps
!>E     nom_theorie  :<LEN=*>     nom de la theorie
!>E     cste         :<LEN=*>     nom de la constante
!>S     val          :<PM_REEL>   valeur de la constante
!>S     unite        :<LEN=*>     unité de la constante
!>S     trouve       :<integer>   CPS_OK si la constante est trouvée, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_theorie, cste
    real(kind=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite

    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=CPS_MAXLG) :: cle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    integer :: acces, j, i, ind
    type(cpsi_desc) :: desc

    ! Initialisations
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCsteTh", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! Corps
    cle = cpsi_intToChar(code_corps)
    cle = "corps"//trim(cle)//"."//trim(cste)

    call cps_getListFichiersCateg(CPS_CATEG_CSTES_CORPS_THEORIE, fichiers)
    do i=1, cpsi_size_ptr(fichiers)
       call cpsi_getAccesMadona(fichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getCsteGenTh", &
               partie_variable=trim(fichiers(i)))
          return
       end if
       ret = cpsi_getDescFichier(fichiers(i), desc)
       ind = index(trim(desc%id), trim(nom_theorie))
       if (ind.gt.0) then
          do j=1, desc%nbChamps
             if (desc%infosChamps(j)%nom.eq.cste) then
                unite = trim(desc%infosChamps(j)%unite)
                exit
             end if
          end do
          
          ret = acc_exist(acces, cle)
          if (ret.eq.1) then
             ret = acc_getd(acces, cle, val, trim(unite))
             if (ret.eq.0) then
                trouve = CPS_OK
                ! désallocation de "desc"
                if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
                exit
             end if
          end if
       end if
       ! désallocation de "desc"
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    

  end function cps_getCsteTh


  function cps_getCsteThCourante(code_corps, cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteThCourante
!
!$Resume
!  Extraction d'une constante d'un corps dans la  theorie courante
!
!$Description
!  Obtention de la valeur d'une constante d'un corps selon la theorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteThCourante(code_corps, cste, val, unite)
!.    integer :: code_corps
!.    character(LEN=*) :: cste
!.    real(kind=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   code du corps
!>E     cste        :<LEN=*>     nom de la constante
!>S     val         :<PM_REEL>   valeur de la constante
!>S     unite       :<LEN=*>     unité de la constante
!>S     trouve      :<integer>   CPS_OK si la constante est trouvée, CPS_ERR_DEF sinon
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: cste
    real(kind=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite

    ! resultat
    integer :: trouve

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCsteThCourante", &
            partie_variable="cps_utilisateur")
       return
    end if 

    trouve = cps_getCsteTh(code_corps, theorie_courante, cste, val, unite)
    
  end function cps_getCsteThCourante


  function cps_getKeplerTh(code_corps, nom_theorie, param_kepl, dateRef) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getKeplerTh
!
!$Resume
!  Extraction des paramètres képlériens d'un corps (suivant un theorie)
!
!$Description
!  Obtention des paramètres képlériens d'un corps.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getKeplerTh(code_corps, nom_theorie, param_kepl, dateRef)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_theorie
!.    type(tm_orb_kep) :: param_kepl
!.    real(kind=PM_REEL) :: dateRef
!.    integer :: trouve
!
!$Arguments
!>E     code_corps   :<integer>      code du corps 
!>E     nom_theorie  :<LEN=*>        nom de la théorie
!>S     param_kepl   :<tm_orb_kep>   paramètres képlériens
!>S     dateRef      :<PM_REEL>      date de référence
!>S     trouve       :<integer>      CPS_OK si les paramètres sont trouvés, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_theorie
    type(tm_orb_kep), intent(out) :: param_kepl
    real(kind=PM_REEL), intent(out) :: dateRef

    ! resultat
    integer :: trouve

    ! variables locales
    integer :: acces, j, i, ind
    character(LEN=CPS_MAXLG) :: cle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    character(LEN=CPS_MAXLG) :: unite_date_ref, unite_a, unite_e, unite_i, unite_pom, unite_gom, unite_M
    type(cpsi_desc) :: desc

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getKeplerTh", &
            partie_variable="cps_utilisateur")
       return
    end if 

    cle = cpsi_intToChar(code_corps)
    cle = "corps"//trim(cle)

    call cps_getListFichiersCateg(CPS_CATEG_KEPLER_CORPS_THEORIE, fichiers)
    do i=1, cpsi_size_ptr(fichiers)
       call cpsi_getAccesMadona(fichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getKeplerTh", &
               partie_variable=trim(fichiers(i)))
          return
       end if
       ret = cpsi_getDescFichier(fichiers(i), desc)
       ind = index(trim(desc%id), trim(nom_theorie))
       if (ind.ge.1) then
          do j=1, desc%nbChamps
             select case (desc%infosChamps(j)%nom)
             case (CPS_DATE_REF)
                unite_date_ref = desc%infosChamps(j)%unite
             case (CPS_DGA)
                unite_a = desc%infosChamps(j)%unite
             case (CPS_EXC)
                unite_e = desc%infosChamps(j)%unite
             case (CPS_INC)
                unite_i = desc%infosChamps(j)%unite
             case (CPS_POM)
                unite_pom = desc%infosChamps(j)%unite
             case (CPS_GOM)
                unite_gom = desc%infosChamps(j)%unite
             case (CPS_ANM)
                unite_M = desc%infosChamps(j)%unite
             end select
          end do
          ret = acc_exist(acces, cle)
          if (ret.eq.1) then
             ret = acc_select(acces, cle, ACC_STRUCT)
             ret = acc_getd(acces, CPS_DATE_REF, dateRef, unite_date_ref)
             ret = acc_getd(acces, CPS_DGA, param_kepl%a, unite_a)
             ret = acc_getd(acces, CPS_EXC, param_kepl%e, unite_e)
             ret = acc_getd(acces, CPS_INC, param_kepl%i, unite_i)
             ret = acc_getd(acces, CPS_POM, param_kepl%pom, unite_pom)
             ret = acc_getd(acces, CPS_GOM, param_kepl%gom, unite_gom)
             ret = acc_getd(acces, CPS_ANM, param_kepl%M, unite_M)
             ret = acc_select_end(acces)
             trouve = CPS_OK
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
             exit
          end if
       end if
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end function cps_getKeplerTh

  function cps_getKeplerThCourante(code_corps, param_kepl, date_ref) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getKeplerThCourante
!
!$Resume
!  Extraction des paramètres képlériens d'un corps (suivant un theorie)
!
!$Description
!  Obtention des paramètres képlériens d'un corps selon la theorie
!  courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getKeplerThCourante(code_corps, param_kepl, date_ref)
!.    integer :: code_corps
!.    type(tm_orb_kep) :: param_kepl
!.    real(kind=PM_REEL) :: date_ref
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>      code du corps
!>S     param_kepl  :<tm_orb_kep>   paramètres képlériens
!>S     date_ref    :<PM_REEL>      date de référence
!>S     trouve      :<integer>      CPS_OK si les paramètres sont trouvés, CPS_ERR_DEF sinon
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
    integer, intent(in) :: code_corps
    type(tm_orb_kep), intent(out) :: param_kepl
    real(kind=PM_REEL), intent(out) :: date_ref
    
    ! resultat
    integer :: trouve

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getKeplerThCourante", &
            partie_variable="cps_utilisateur")
       return
    end if 

    trouve = cps_getKeplerTh(code_corps, theorie_courante, param_kepl, date_ref)

  end function cps_getKeplerThCourante


  subroutine cps_getListCorps(listeCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListCorps
!
!$Resume
!
!$Description
!  Renvoie la liste de tous les corps, en tenant compte des restrictions.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListCorps(listeCorps)
!.    integer, dimension(:), pointer :: listeCorps
!
!$Arguments
!>E/S   listeCorps  :<integer,DIM=(:),pointer>   liste des codes des corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, dimension(:), pointer :: listeCorps

    ! variables locales
    integer :: nbCorps
    integer :: i, acces, nature, code
    character(LEN=CPS_MAXLG) :: libelle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(maillon_i), pointer :: listCodes, codeCourant
    logical :: existe

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getListCorps", &
            partie_variable="cps_utilisateur")
       return
    end if 

    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiers)
    
    allocate(listCodes)
    nullify(listcodes%suivant)
    listcodes%val=undef_vali

    codeCourant => listCodes

    nbCorps = 0
    
    do i=1, cpsi_size_ptr(fichiers)
       ! fichier i
       call cpsi_getAccesMadona(fichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getListCorps", &
               partie_variable=trim(fichiers(i)))
          return
       else
          ret = acc_scan_reset(acces)
          do
             ret = acc_scan(acces, libelle, nature)
             if (nature.eq.0) then
                exit
             end if
             ret = acc_select(acces, libelle, ACC_STRUCT)
             ret = acc_geti(acces, "code", code)
             existe = cpsi_codeExisteDeja(code, listCodes)
             if (.not.existe) then
                codeCourant%val = code
                allocate(codeCourant%suivant)
                codeCourant => codeCourant%suivant
                nullify(codeCourant%suivant)
                codeCourant%val=undef_vali
                nbCorps = nbCorps + 1
             end if
             ret = acc_select_end(acces)
          end do
       end if
    end do
    
    allocate(listeCorps(nbCorps))

    codeCourant => listCodes
    do i=1, nbCorps
       listCodes => listCodes%suivant
       listeCorps(i) = codeCourant%val
       ! liberation memoire
       deallocate(codeCourant, stat=iostat)
       codeCourant => listCodes
    end do

    ! liberation memoire
    deallocate(fichiers, stat=iostat)
    deallocate(listCodes, stat=iostat)

  end subroutine cps_getListCorps

  
  function cpsi_codeExisteDeja(code, listCodes) result (existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_codeExisteDeja
!
!$Resume
!
!$Description
!  Fonction interne qui determine si le code passe en argument est present 
!  ou non dans la liste listCodes.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_codeExisteDeja(code, listCodes)
!.    integer :: code
!.    type(maillon_i), pointer :: listCodes
!.    logical :: existe
!
!$Arguments
!>E     code       :<integer>             code d'un corps
!>E/S   listCodes  :<maillon_i,pointer>   liste des maillons dans laquelle effectuer la recherche
!>S     existe     :<logical>             .true. si le code est présent dans la liste, .false. sinon
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
    integer, intent(in) :: code
    type(maillon_i), pointer :: listCodes
    
    ! resultat
    logical :: existe
    
    ! variables internes
    type(maillon_i), pointer :: codeCourant

    ! Initialisation
    existe = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_codeExisteDeja", &
            partie_variable="cps_utilisateur")
       return
    end if 

    codeCourant => listCodes
    
    do
       if (code.eq.codeCourant%val) then
          existe = .true.
       end if
       if (.not.associated(codeCourant%suivant)) then
          exit
       else
          codeCourant => codeCourant%suivant
       end if
    end do

  end function cpsi_codeExisteDeja



  subroutine cpsi_getCorps_i(nom_att, val_att, listeCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCorps_i
!
!$Resume
!
!$Description
!  Obtenir la liste de tous les corps dont un attribut de type entier ne
!  dependant pas d'une theorie est a une valeur particuliere.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_getCorps_i(nom_att, val_att, listeCorps)
!.    character(LEN=*) :: nom_att
!.    integer :: val_att
!.    integer, dimension(:), pointer :: listeCorps
!
!$Arguments
!>E     nom_att     :<LEN=*>                     nom de l'attribut correspondant au critère de recherche
!>E     val_att     :<integer>                   valeur de l'attribut correspondant au critère de recherche
!>E/S   listeCorps  :<integer,DIM=(:),pointer>   liste des codes des corps dont l'attribut est à la bonne valeur
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: val_att
    integer, dimension(:), pointer :: listeCorps

    ! variables locales
    integer :: nbCorps
    integer :: i, acces, nature, code
    character(LEN=CPS_MAXLG) :: libelle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(maillon_i), pointer :: listCodes, codeCourant
    logical :: existe
    integer :: val

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCorps_i", &
            partie_variable="cps_utilisateur")
       return
    end if 

    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiers)
    
    allocate(listCodes)
    nullify(listcodes%suivant)

    codeCourant => listCodes

    nbCorps = 0
    
    do i=1, cpsi_size_ptr(fichiers)
       ! fichier i
       call cpsi_getAccesMadona(fichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getCorps_i", &
               partie_variable=trim(fichiers(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          end if
          ret = acc_select(acces, libelle, ACC_STRUCT)
          ret = acc_geti(acces, "code", code)
          ret = acc_geti(acces, nom_att, val)
          if (val.eq.val_att) then
             existe = cpsi_codeExisteDeja(code, listCodes)
             if (.not.existe) then
                codeCourant%val = code
                allocate(codeCourant%suivant)
                codeCourant => codeCourant%suivant
                nullify(codeCourant%suivant)
                codeCourant%val=undef_vali
                nbCorps = nbCorps + 1
             end if
          end if
          ret = acc_select_end(acces)
       end do
    end do
    
    if (associated(listeCorps)) deallocate(listeCorps, stat=iostat)
    allocate(listeCorps(nbCorps))

    codeCourant => listCodes
    do i=1, nbCorps
       listCodes => listCodes%suivant
       listeCorps(i) = codeCourant%val
       ! liberation memoire
       deallocate(codeCourant, stat=iostat)
       codeCourant => listCodes
    end do

    ! liberation memoire
    deallocate(fichiers, stat=iostat)
    deallocate(listCodes, stat=iostat)

  end subroutine cpsi_getCorps_i


  
  subroutine cpsi_getCorps_s(nom_att, val_att, listeCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCorps_s
!
!$Resume
!
!$Description
!  Obtenir la liste de tous les corps dont un attribut de type chaine de
!  caracteres ne dependant pas d'une theorie est a une valeur particuliere.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_getCorps_s(nom_att, val_att, listeCorps)
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: val_att
!.    integer, dimension(:), pointer :: listeCorps
!
!$Arguments
!>E     nom_att     :<LEN=*>                     nom de l'attribut correspondant au critère de recherche
!>E     val_att     :<LEN=*>                     valeur de l'attribut correspondant au critère de recherche
!>E/S   listeCorps  :<integer,DIM=(:),pointer>   liste des codes des corps dont l'attribut est à la bonne valeur
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: val_att
    integer, dimension(:), pointer :: listeCorps

    ! variables locales
    integer :: nbCorps
    integer :: i, acces, nature, code
    character(LEN=CPS_MAXLG) :: libelle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(maillon_i), pointer :: listCodes, codeCourant
    logical :: existe
    character(LEN=CPS_MAXLG) :: val

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCorps_s", &
            partie_variable="cps_utilisateur")
       return
    end if 

    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiers)
    
    allocate(listCodes)
    nullify(listcodes%suivant)

    codeCourant => listCodes

    nbCorps = 0
    
    do i=1, cpsi_size_ptr(fichiers)
       ! fichier i
       call cpsi_getAccesMadona(fichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getCorps_s", &
               partie_variable=trim(fichiers(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          end if
          ret = acc_select(acces, libelle, ACC_STRUCT)
          ret = acc_geti(acces, "code", code)
          ret = acc_gets(acces, nom_att, val)
          if (val.eq.val_att) then
             existe = cpsi_codeExisteDeja(code, listCodes)
             if (.not.existe) then
                codeCourant%val = code
                allocate(codeCourant%suivant)
                codeCourant => codeCourant%suivant
                nullify(codeCourant%suivant)
                codeCourant%val=undef_vali
                nbCorps = nbCorps + 1
             end if
          end if
          ret = acc_select_end(acces)
       end do
    end do
    
    allocate(listeCorps(nbCorps))

    codeCourant => listCodes
    do i=1, nbCorps
       listCodes => listCodes%suivant
       listeCorps(i) = codeCourant%val
       ! liberation memoire
       deallocate(codeCourant, stat=iostat)
       codeCourant => listCodes
    end do

    ! liberation memoire
    deallocate(fichiers, stat=iostat)
    deallocate(listCodes, stat=iostat)

  end subroutine cpsi_getCorps_s

  
  subroutine cpsi_getCorps_d(nom_att, listCorps, val, valeurMin, valeurMax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCorps_d
!
!$Resume
!
!$Description
! V2.0
! Obtenir la liste des corps dont un attribut de type PM_REEL est :
! - soit a une valeur particuliere,
! - soit superieur a un minimum,
! - soit inferieur a un maximum,
! - soit compris dans un intervalle.
!
!$Auteur
!  vpg
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_getCorps_d(nom_att, listCorps, [val], [valeurmin], [valeurmax])
!.    character(LEN=*) :: nom_att
!.    integer, dimension(:), pointer :: listCorps
!.    real(KIND=PM_REEL) :: val, valeurMin, valeurMax
!
!$Arguments
!>E     nom_att    :<LEN=*>                     nom de l'attribut correspondant au critère de recherche
!>E/S   listCorps  :<integer,DIM=(:),pointer>   liste des codes des corps dont l'attribut possède une valeur dans l'intervalle
!>[E]   val        :<PM_REEL>                   valeur de l'attribut recherchée 
!>[E]   valeurMin  :<PM_REEL>                   valeur minimale de l'attribut
!>[E]   valeurMax  :<PM_REEL>                   valeur maximale de l'attribut
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getTheorie
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
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    integer, dimension(:), pointer :: listCorps
    real(KIND=PM_REEL), optional, intent(in) :: val, valeurMin, valeurMax
    
    ! variables locales
    integer :: i, acces, nbCorps, nb, ind, ind_th
    type(maillon_i), pointer :: lcorps, lcorps_tmp
    logical :: existe, existe_th, lire_fic
    character(LEN=CPS_MAXLG) :: unite, thCourante
    type(cpsi_desc), dimension(:), pointer :: listeFichiers => NULL()

    if ((.not.present(val)).and.&
         (.not.present(valeurMin)).and.&
         (.not.present(valeurMax))) then
       ! il faut au moins qu'un de ces trois paramètres soit present
       ! pour fixer un critere de recherche
       call MSP_signaler_message(cle_mes="CPS_ERR_GET_CORPS")
       return
    end if
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_getCorps_d", &
            partie_variable="cps_utilisateur")
       return
    end if 

    ! initialisation
    allocate(lcorps)
    nullify(lcorps%suivant)
    lcorps%val=undef_vali
    nbCorps = 0
    nb = 0

    call cps_getTheorie(thCourante)

    call cpsi_getListeDescFichiers(listeFichiers)

    do i=1, cpsi_size_ptr(listeFichiers)
       ! on regarde si l'attribut est dans le fichier
       existe = cpsi_existeAttDesc(nom_att, listeFichiers(i), indice=ind)
       if (existe) then
          ! l'attribut est present dans le fichier
          ! on regarde si la theorie correspond à la theorie courante
          existe_th = cpsi_existeAttDesc("theorie", listeFichiers(i))
          if (existe_th) then
             ind_th = index(trim(listeFichiers(i)%id), trim(thCourante))
             if (ind_Th.ge.1) then
                lire_fic = .true.
             else
                lire_fic = .false.
             end if
          else
             lire_fic = .true.
          end if
          if (lire_fic) then
             ! on recupere l'attribut
             unite = listeFichiers(i)%infosChamps(ind)%unite
             ! ouvrir le fichier
             call cpsi_getAccesMadona(listeFichiers(i)%fichier, acces)
             if (acces.LT.0) then
                call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                     routine="cpsi_getCorps_d", &
                     partie_variable=trim(listeFichiers(i)%fichier))
                return
             end if
             if (present(val)) then
                ! on recherche les corps dont l'attribut est à une valeur particulière
                nb = cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, val=val)
             elseif (present(valeurMin).and.present(valeurMax)) then
                ! on recherche les corps dont l'attribut est à compris dans un intervalle
                nb = cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, valeurMin=valeurMin, valeurMax=valeurMax)
             elseif (present(valeurMin)) then
                ! on recherche les corps dont l'attribut est à supérieur à une valeur
                nb = cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, valeurMin=valeurMin)
             elseif (present(valeurMax)) then
                ! on recherche les corps dont l'attribut est à inférieur à une valeur
                nb = cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, valeurMax=valeurMax)
             end if
             nbCorps = nbCorps+nb
          end if
       end if
    end do

    ! affectation du paramètre de sortie listCorps
    allocate(listCorps(nbCorps))
    do i=1, nbCorps
       lcorps_tmp => lcorps
       listCorps(i) = lcorps%val
       lcorps => lcorps%suivant
       ! liberation memoire
       deallocate(lcorps_tmp, stat=iostat)
    end do
    !Fin de la libération mémoire avec le dernier élément de la chaine
    deallocate(lcorps, stat=iostat)
    
    call cpsi_deallocateDescFichiers(listeFichiers)

  end subroutine cpsi_getCorps_d

  
  function cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, val, &
       valeurMin, valeurMax) result (nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCorpsFichier
!
!$Resume
!
!$Description
! V2.0
! Fonction interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  nb = cpsi_getCorpsFichier(acces, nom_att, lcorps, unite, [val], &
!.           [valeurmin], [valeurmax])
!.    integer :: acces
!.    character(LEN=*) :: nom_att, unite
!.    type(maillon_i), pointer :: lcorps
!.    real(KIND=PM_REEL) :: val, valeurMin, valeurMax
!.    integer :: nb
!
!$Arguments
!>E     acces      :<integer>             
!>E     nom_att    :<LEN=*>               
!>E/S   lcorps     :<maillon_i,pointer>   
!>E     unite      :<LEN=*>               
!>[E]   val        :<PM_REEL>             
!>[E]   valeurMin  :<PM_REEL>             Valeur minimum recherchée           
!>[E]   valeurMax  :<PM_REEL>             Valeur maximum recherchée
!>S     nb         :<integer>             
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
    character(LEN=*), intent(in) :: nom_att, unite
    type(maillon_i), pointer :: lcorps
    real(KIND=PM_REEL), optional, intent(in) :: val, valeurMin, valeurMax

    ! resultat
    integer :: nb

    ! variables locales
    type(maillon_i), pointer :: lcorps_courant
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nature, code_courant, res, res1, res2
    real(KIND=PM_REEL) :: v_tmp
    logical :: deja_present

    ! initialisation
    nb = 0
    lcorps_courant => lcorps
    ! lcorps_courant pointe à la fin de la liste
    do
       if (.not.associated(lcorps_courant%suivant)) then
          exit
       else
          lcorps_courant => lcorps_courant%suivant
       end if
    end do

    ! analyse du fichier courant
    ret = acc_scan_reset(acces)
    
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.CPS_FIN_SEQ) then
          exit
       end if
       ret = acc_select(acces, libelle, ACC_STRUCT)
       ret = acc_getd(acces, nom_att, v_tmp, unite)
       ret = acc_geti(acces, "code", code_courant)
       if (ret.LT.0) then
          return
       end if
       deja_present = cpsi_codeExisteDeja(code_courant, lcorps)
       if (.not.deja_present) then
          ! le corps n'a pas deja ete enregistre
          if (present(val)) then
             res = cpsi_compareReels(v_tmp, val)
             if (res.eq.0) then
                ! la valeur lue dans le fichier est egale à la valeur recherchee
                ! on ajoute le code du corps a la liste
                lcorps_courant%val = code_courant
                allocate(lcorps_courant%suivant)
                lcorps_courant => lcorps_courant%suivant
                nullify(lcorps_courant%suivant)
                lcorps_courant%val=undef_vali
                nb = nb+1
             end if
          elseif (present(valeurMin).and.present(valeurMax)) then
             res1 = cpsi_compareReels(v_tmp,valeurMin)
             res2 = cpsi_compareReels(v_tmp, valeurMax)
             if ((res1.eq.1).and.(res2.eq.-1)) then
                ! la valeur lue dans le fichier est comprise dans l'intervalle de recherche
                ! on ajoute le code du corps a la liste
                lcorps_courant%val = code_courant
                allocate(lcorps_courant%suivant)
                lcorps_courant => lcorps_courant%suivant
                nullify(lcorps_courant%suivant)
                lcorps_courant%val=undef_vali
                nb = nb+1
             end if
          elseif (present(valeurMin)) then
             res = cpsi_compareReels(v_tmp, valeurMin)
             if (res.eq.1) then
                ! la valeur lue dans le fichier est superieure ou egale au minimum
                ! on ajoute le code du corps a la liste
                lcorps_courant%val = code_courant
                allocate(lcorps_courant%suivant)
                lcorps_courant => lcorps_courant%suivant
                nullify(lcorps_courant%suivant)
                lcorps_courant%val=undef_vali
                nb = nb+1
             end if
          elseif (present(valeurMax)) then
             res = cpsi_compareReels(v_tmp, valeurMax)
             if (res.eq.-1) then
                ! la valeur lue dans le fichier est inferieure ou egale au maximum
                ! on ajoute le code du corps a la liste
                lcorps_courant%val = code_courant
                allocate(lcorps_courant%suivant)
                lcorps_courant => lcorps_courant%suivant
                nullify(lcorps_courant%suivant)
                lcorps_courant%val=undef_vali
                nb = nb+1
             end if
          end if
       end if
       ret = acc_select_end(acces)
       !end if
       !end if
    end do

  end function cpsi_getCorpsFichier


  
  subroutine cps_getCorpsTheorieDef(tab_att, nom_theorie, listeCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorpsTheorieDef
!
!$Resume
!
!$Description
! Obtenir la liste des corps pour lesquels des attributs specifies
! sont definis dans une theorie particuliere.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorpsTheorieDef(tab_att, nom_theorie, listeCorps)
!.    character(LEN=*), dimension(:) :: tab_att
!.    character(LEN=*) :: nom_theorie
!.    integer, dimension(:), pointer :: listeCorps
!
!$Arguments
!>E     tab_att      :<LEN=*,DIM=(:)>             noms des attributs
!>E     nom_theorie  :<LEN=*>                     nom de la theorie
!>E/S   listeCorps   :<integer,DIM=(:),pointer>   liste des codes des corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), dimension(:), intent(in) :: tab_att
    character(LEN=*), intent(in) :: nom_theorie
    integer, dimension(:), pointer :: listeCorps
    
    ! variables locales
    character(LEN=CPS_MAXLG) :: fichier, libelle
    integer :: trouve, i, nbCorps, nature, acces, code, ind
    logical :: correspond, existe
    character(LEN=CPS_MAXLG), dimension(:), pointer  :: list_fichiers => NULL()
    type(cpsi_desc) :: desc
    type(maillon_i), pointer :: listCodes, codeCourant
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getCorpsTheorieDef", &
            partie_variable="cps_utilisateur")
       return
    end if 

    allocate(listCodes)
    nullify(listCodes%suivant)
    codeCourant => listCodes

    nbCorps = 0

    !!trouve = cps_getFichierCateg(CPS_CATEG_CSTES_CORPS_THEORIE, nom_theorie, fichier)
    call cps_getListFichiersCateg(CPS_CATEG_CSTES_CORPS_THEORIE, list_fichiers)
    do i=1, cpsi_size_ptr(list_fichiers)
       fichier = trim(list_fichiers(i))
       ! description du fichier
       trouve = cpsi_getDescFichier(fichier, desc)
       if (trouve.eq.CPS_OK) then
          ! on regarde si l'id du fichier contient le nom de la theorie
          ind = index(trim(desc%id), trim(nom_theorie))
          if (ind.lt.1) then
             ! il ne s'agit pas de la bonne theorie
             ! on passe au fichier suivant
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
             exit
          end if
          call cpsi_getAccesMadona(fichier, acces)
          if (acces.LT.0) then
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                  routine="cpsi_getCorpsTheorieDef", &
                  partie_variable=trim(fichier))
             deallocate(listCodes, stat=iostat)
             return
          end if
          ret = acc_scan_reset(acces)
          do
             ret = acc_scan(acces, libelle, nature)
             if (nature.eq.0) then
                if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
                exit
             end if
             ret = acc_select(acces, libelle, ACC_STRUCT)
             ! on regarde si les attributs du corps courant sont definis
             correspond = cpsi_isCorpsDef(acces, tab_att, desc)
             ret = acc_geti(acces, "code", code)
             existe = cpsi_codeExisteDeja(code, listCodes)
             if ( correspond .and. ( .not. existe ) ) then
                ! ajouter le corps courant
                codeCourant%val = code
                allocate(codeCourant%suivant)
                codeCourant => codeCourant%suivant
                nullify(codeCourant%suivant)
                codeCourant%val=undef_vali
                nbCorps = nbCorps + 1
             end if
             ret = acc_select_end(acces)
          end do
       end if
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do

    allocate(listeCorps(nbCorps))
    
    codeCourant => listCodes
    do i=1, nbCorps
       listCodes => listCodes%suivant
       listeCorps(i) = codeCourant%val
       ! liberation memoire
       deallocate(codeCourant, stat=iostat)
       codeCourant => listCodes
    end do

    ! liberation memoire
    deallocate(listCodes, stat=iostat)
    if (associated(list_fichiers)) then
       deallocate(list_fichiers, stat=iostat)
    end if

  end subroutine cps_getCorpsTheorieDef
  
  
  function cpsi_isCorpsDef(acces, tab_att, desc) result (correspond)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_isCorpsDef
!
!$Resume
!
!$Description
! Fonction interne : determine si les attributs du corps specifies par tab_att sont definis ou non
! acces : structure Madona d'un corps (un acc_select(acces, <corps>, ACC_STRUCT) ayant ete fait avant appel)
! si le type de l'attribut est CPS_ENTIER : il est defini si sa valeur n'est pas celle par defaut
! si le type de l'attribut est CPS_REEL : il est defini si son unite n'est pas UNDEF
! si le type de l'attribut est CPS_STRING : il est defini si sa valeur n'est pas celle par defaut
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  correspond = cpsi_isCorpsDef(acces, tab_att, desc)
!.    integer :: acces
!.    character(LEN=*), dimension(:) :: tab_att
!.    type(cpsi_desc) :: desc
!.    logical :: correspond
!
!$Arguments
!>E     acces       :<integer>         
!>E     tab_att     :<LEN=*,DIM=(:)>   
!>E     desc        :<cpsi_desc>       
!>S     correspond  :<logical>         
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
    character(LEN=*), dimension(:), intent(in) :: tab_att
    type(cpsi_desc), intent(in) :: desc
    
    ! resultat
    logical :: correspond
    
    ! variables locales
    integer :: nb_att, i, j
    integer :: type_att_courant = 0
    integer :: vd_i_att_courant, v_i
    real(kind=PM_REEL) :: v_d
    character(LEN=CPS_MAXLG) :: vd_s_att_courant, v_s
    character(LEN=CPS_MAXLG) ::  unite

    ! Initialisation
    correspond = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_isCorpsDef", &
            partie_variable="cps_utilisateur")
       return
    end if 

    correspond = .true.
    nb_att = size(tab_att)
    ! determination de la definition de chaque attribut dans le corps courant
    do i=1, nb_att
       ret = acc_scan_reset(acces)
       ! determination du type de l'attribut courant
       do j=1, desc%nbChamps
          if (desc%infosChamps(j)%nom.eq.tab_att(i)) then
             type_att_courant = desc%infosChamps(j)%type
             exit
          end if
       end do      
       ! determination de la valeur par defaut de l'attribut courant
       select case (type_att_courant)
       case (CPS_ENTIER)
          vd_i_att_courant = desc%infosChamps(j)%vd_i
          ret = acc_geti(acces, tab_att(i), v_i)
          if ((ret.ne.0).OR.(v_i.eq.vd_i_att_courant)) then
             correspond = .false.
             exit
          end if
       case (CPS_REEL)
          unite = desc%infosChamps(j)%unite
          ret = acc_getd(acces, tab_att(i), v_d, unite)
          if (ret.ne.0) then
             correspond = .false.
             exit
          end if
       case (CPS_STRING)
          vd_s_att_courant = desc%infosChamps(j)%vd_s
          ret = acc_gets(acces, tab_att(i), v_s)
          if ((ret.ne.0).OR.(v_s.eq.vd_s_att_courant)) then
             correspond = .false.
             exit
          end if
       end select
    end do
    
  end function cpsi_isCorpsDef


  
  subroutine cps_getCorpsTheorieCouranteDef(tab_att, listeCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorpsTheorieCouranteDef
!
!$Resume
!
!$Description
!  Obtenir la liste des corps pour lesquels  des attributs specifies
!  sont definis dans la theorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorpsTheorieCouranteDef(tab_att, listeCorps)
!.    character(LEN=*), dimension(:) :: tab_att
!.    integer, dimension(:), pointer :: listeCorps
!
!$Arguments
!>E     tab_att     :<LEN=*,DIM=(:)>             nom des attributs
!>E/S   listeCorps  :<integer,DIM=(:),pointer>   liste des codes des corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getCorpsTheorieDef
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
    character(LEN=*), dimension(:), intent(in) :: tab_att
    integer, dimension(:), pointer :: listeCorps

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getCorpsTheorieCouranteDef", &
            partie_variable="cps_utilisateur")
       return
    end if 

    call cps_getCorpsTheorieDef(tab_att, theorie_courante, listeCorps)

  end subroutine cps_getCorpsTheorieCouranteDef


  
  function cps_existeAtt(code, nom_att) result (existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_existeAtt
!
!$Resume
!
!$Description
! Determiner si un attribut est defini pour un corps particulier
! si le type de l'attribut est CPS_ENTIER : il est defini si sa valeur n'est pas celle par defaut
! si le type de l'attribut est CPS_REEL : il est defini si son unite n'est pas UNDEF
! si le type de l'attribut est CPS_STRING : il est defini si sa valeur n'est pas celle par defaut
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cps_existeAtt(code, nom_att)
!.    integer :: code
!.    character(LEN=*) :: nom_att
!.    logical :: existe
!
!$Arguments
!>E     code     :<integer>   code du corps
!>E     nom_att  :<LEN=*>     nom de l'attribut
!>S     existe   :<logical>   .true. si l'attribut existe, .false. sinon
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
    integer, intent(in) :: code
    character(LEN=*), intent(in) :: nom_att

    ! resultat
    logical :: existe

    ! variables locales
    logical ::att_present
    integer :: i, type_att, index_att
    integer :: vd_i, v_i
    real(kind=PM_REEL) :: vd_d, v_d
    character(LEN=CPS_MAXLG) :: vd_s, v_s
    character(LEN=CPS_MAXLG) :: fichier, unite_att
    type(cpsi_desc), dimension(:), pointer :: listeFichiers => NULL()

    ! Initialisations
    existe = .false.
    att_present = .false.
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_existeAtt", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cpsi_getListeDescFichiers(listeFichiers)
    
    do i=1, cpsi_size_ptr(listeFichiers)
       ! recherche si l'attribut est present dans le fichier courant
       att_present = cpsi_existeAttDesc(trim(nom_att), listeFichiers(i), index_att)
       if (att_present) then
          ! un fichier est trouve
          ! determiner la valeur dans ce fichier
          fichier = listeFichiers(i)%fichier
          type_att = listeFichiers(i)%infosChamps(index_att)%type
          select case (type_att)
          case (CPS_ENTIER)
             vd_i = listeFichiers(i)%infosChamps(index_att)%vd_i
             existe = cpsi_getValCorpsFichier_i(code, fichier, nom_att, v_i, vd_i)
          case (CPS_REEL)
             vd_d = listeFichiers(i)%infosChamps(index_att)%vd_d
             unite_att = listeFichiers(i)%infosChamps(index_att)%unite
             existe = cpsi_getValCorpsFichier_d(code, fichier, nom_att, v_d, unite_att, vd_d)
          case (CPS_STRING)
             vd_s = listeFichiers(i)%infosChamps(index_att)%vd_s
             existe = cpsi_getValCorpsFichier_s(code, fichier, nom_att, v_s, vd_s)
          end select
       end if
       if (existe) then
          exit
       end if
    end do

    call cpsi_deallocateDescFichiers(listeFichiers)

  end function cps_existeAtt
     
     

  function cpsi_getValCorpsFichier_i(code, fichier, nom_att, v_i, vd_i) result(existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getValCorpsFichier_i
!
!$Resume
!
!$Description
!  Fonction interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_getValCorpsFichier_i(code, fichier, nom_att, v_i, [vd_i])
!.    character(LEN=*) :: fichier, nom_att
!.    integer :: code
!.    integer :: v_i
!.    integer :: vd_i
!.    logical :: existe
!
!$Arguments
!>E     code     :<integer>   code du corps
!>E     fichier  :<LEN=*>     nom du fichier dans lequel s'effectue la recherche
!>E     nom_att  :<LEN=*>     nom de l'attribut
!>S     v_i      :<integer>   valeur de l'attribut
!>[E]   vd_i     :<integer>   valeur par défaut de l'attribut
!>S     existe   :<logical>   .true. si l'attribut est défini, .false. sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_getAccesMadona
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

    character(LEN=*), intent(in) :: fichier, nom_att
    integer, intent(in) :: code
    integer, intent(out) :: v_i
    integer, optional, intent(in) :: vd_i
    character(LEN=CPS_MAXLG) :: cle
    integer :: acces, v_tmp
    logical :: existe

    ! Initialisation
    existe = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getValCorpsFichier_i", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cpsi_getAccesMadona(fichier, acces)
    if (acces.LT.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
            routine="cpsi_getValCorpsFichier_i", &
            partie_variable=trim(fichier))
       return
    end if
    cle = cpsi_intToChar(code)
    cle = "corps"//trim(cle)//"."//trim(nom_att)
    
    ret = acc_exist(acces, cle)
    if (ret.eq.1) then
       if (present(vd_i)) then
          ret = acc_geti(acces, cle, v_tmp)
          if ((ret.eq.0).and.&
               (v_tmp.ne.vd_i)) then
             existe = .true.
          end if
       else
          ret = acc_geti(acces, cle, v_i)
          if (ret.eq.0) then
             existe = .true.
          end if
       end if
    end if
  end function cpsi_getValCorpsFichier_i


  function cpsi_getValCorpsFichier_d(code, fichier, nom_att, v_d, unite, vd_d) result(existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getValCorpsFichier_d
!
!$Resume
!
!$Description
!  Fonction interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_getValCorpsFichier_d(code, fichier, nom_att, v_d, unite, [vd_d])
!.    character(LEN=*) :: fichier, nom_att, unite
!.    integer :: code
!.    real(kind=PM_REEL) :: v_d
!.    real(kind=PM_REEL) :: vd_d
!.    logical :: existe
!
!$Arguments
!>E     code     :<integer>   code du corps
!>E     fichier  :<LEN=*>     fichier dans lequel s'effectue la recherche
!>E     nom_att  :<LEN=*>     nom de l'attribut
!>S     v_d      :<PM_REEL>   valeur de l'attribut
!>E     unite    :<LEN=*>     unité de l'attribut
!>[E]   vd_d     :<PM_REEL>   valeur par défaut de l'attribut
!>S     existe   :<logical>   .true. si l'attribut est défini, .false .sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: fichier, nom_att, unite
    integer, intent(in) :: code
    real(kind=PM_REEL), intent(out) :: v_d
    real(kind=PM_REEL), optional, intent(in) :: vd_d

    ! resultat
    logical :: existe

    ! variables locales
    character(LEN=CPS_MAXLG) :: cle
    integer :: acces, res
    real(kind=PM_REEL) :: v_tmp

    ! Initialisation
    existe = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getValCorpsFichier_d", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cpsi_getAccesMadona(fichier, acces)
    if (acces.LT.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
            routine="cpsi_getValCorpsFichier_d", &
            partie_variable=trim(fichier))
       return
    end if
    cle = cpsi_intToChar(code)
    cle = "corps"//trim(cle)//"."//trim(nom_att)
    
    ret = acc_exist(acces, cle)
    if (ret.eq.1) then
       if (present(vd_d)) then
          ret = acc_getd(acces, cle, v_tmp, unite)
          res = cpsi_compareReels(v_tmp, vd_d)
          if ((ret.eq.0).and.&
               (res.ne.0)) then
             existe = .true.
          end if
       else
          ret = acc_getd(acces, cle, v_d, unite)
          if (ret.eq.0) then
             existe = .true.
          end if
       end if
    end if
  end function cpsi_getValCorpsFichier_d


  function cpsi_getValCorpsFichier_s(code, fichier, nom_att, v_s, vd_s) result(existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getValCorpsFichier_s
!
!$Resume
!
!$Description
!  Fonction interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_getValCorpsFichier_s(code, fichier, nom_att, v_s, [vd_s])
!.    character(LEN=*) :: fichier, nom_att
!.    integer :: code
!.    character(LEN=*) :: v_s
!.    character(LEN=*) :: vd_s
!.    logical :: existe
!
!$Arguments
!>E     code     :<integer>   code du corps
!>E     fichier  :<LEN=*>     fichier dans lequel s'effectue la recherche
!>E     nom_att  :<LEN=*>     nom de l'attribut
!>S     v_s      :<LEN=*>     valeur de l'attribut
!>[E]   vd_s     :<LEN=*>     valeur par défaut de l'attribut
!>S     existe   :<logical>   .true. si l'attribut est défini, .false. sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: fichier, nom_att
    integer, intent(in) :: code
    character(LEN=*), intent(out) :: v_s
    character(LEN=*), optional, intent(in) :: vd_s
    
    ! resultat
    logical :: existe

    ! variables locales
    character(LEN=CPS_MAXLG) :: cle, v_tmp
    integer :: acces

    ! Initialisation
    existe = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getValCorpsFichier_s", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cpsi_getAccesMadona(fichier, acces)
    if (acces.LT.0) then
       call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
            routine="cpsi_getValCorpsFichier_s", &
            partie_variable=trim(fichier))
       return
    end if
    cle = cpsi_intToChar(code)
    cle = "corps"//trim(cle)//"."//trim(nom_att)
    
    ret = acc_exist(acces, cle)
    if (ret.eq.1) then
       if (present(vd_s)) then
          ret = acc_gets(acces, cle, v_tmp)
          if ((ret.eq.0).and.&
               (v_tmp.ne.vd_s)) then
             existe = .true.
          end if
       else
          ret = acc_gets(acces, cle, v_s)
          if (ret.eq.0) then
             existe = .true.
          end if
       end if
    end if
  end function cpsi_getValCorpsFichier_s


  
  function cps_getFichiersPotentielCorps(corps, listeFichiersPot, rep, type_base) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichiersPotentielCorps
!
!$Resume
!
!$Description
!  Obtenir la liste des fichiers de potentiels pour un corps.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichiersPotentielCorps(corps, listeFichiersPot, [rep], [type_base])
!.    integer :: corps
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeFichiersPot
!.    logical :: rep
!.    integer :: type_base
!.    integer :: trouve
!
!$Arguments
!>E     corps             :<integer>                         code du corps
!>E/S   listeFichiersPot  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers de potentiel
!>[E]   rep               :<logical>                         booléen indiquant si on souhaite le chemin complet des fichiers
!>[E]   type_base         :<integer>                         
!>S     trouve            :<integer>                         CPS_OK s'il existe au moins un fichier de potentiel, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: corps
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeFichiersPot
    logical, intent(in), optional :: rep
    integer, intent(in), optional :: type_base

    ! resultat
    integer :: trouve

    ! variables locales
    integer :: i, k, acces, nature, nbFichiers, code
    character(LEN=CPS_MAXLG) :: libelle, f_tmp, rep_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersPot => NULL()
    integer :: base

    ! Initialisations
    nbFichiers = 0
    trouve = CPS_ERR_DEF

    if (present(type_base)) then
       base = type_base
    else
       ! par défaut : recherche dans la base de référence et dans la base locale
       base = 0
    end if

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichiersPotentielCorps", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_POTENTIEL, fichiersPot, type_base=base)

    ! 1ere passe : determination du nombre de fichiers
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichiersPotentielCorps", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          !elseif (nature.eq.ACC_STRUCT) then
          end if
          ret = acc_geti(acces, trim(libelle)//".code", code)
          if ((ret.eq.0).and.&
               (code.eq.corps)) then
             nbFichiers = nbFichiers+1
          end if
          !end if
       end do
    end do

    ! allocation memoire
    allocate(listeFichiersPot(nbFichiers))

    ! lecture des fichiers
    k = 1
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichiersPotentielCorps", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          end if
          !elseif (nature.eq.ACC_STRUCT) then
          ret = acc_geti(acces, trim(libelle)//".code", code)
          if ((ret.eq.0).and.&
               (code.eq.corps)) then
             ret = acc_gets(acces, trim(libelle)//".fichier_potentiel", f_tmp)
             if (present(rep).and.rep) then
                rep_tmp = ""
                ret = acc_gets(acces, trim(libelle)//".repertoire_potentiel", rep_tmp)
                ! on rajoute '/' si besoin
                rep_tmp = cpsi_getRep(rep_tmp)
                f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)
             end if
             listeFichiersPot(k) = trim(rep_base_ref)//"/"//trim(f_tmp)
             k = k+1
          end if
          !end if
       end do
    end do

    if (nbFichiers.gt.0) then
       trouve = CPS_OK
    end if

    ! liberation memoire
    if (associated(fichiersPot)) then
       deallocate(fichiersPot, stat=iostat)
    end if

  end function cps_getFichiersPotentielCorps

  
  function cps_getModelesPotentielCorps(corps, listeModelesPot, type_base) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getModelesPotentielCorps
!
!$Resume
!
!$Description
!  Obtenir la liste des modeles de potentiels pour un corps.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getModelesPotentielCorps(corps, listeModelesPot, [type_base])
!.    integer :: corps
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModelesPot
!.    integer :: type_base
!.    integer :: trouve
!
!$Arguments
!>E     corps            :<integer>                         code du corps
!>E/S   listeModelesPot  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des modèles de potentiel
!>[E]   type_base        :<integer>                         
!>S     trouve           :<integer>                         CPS_OK s'il existe au moins un modèle, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: corps
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModelesPot
    integer, optional, intent(in) :: type_base

    ! resultat
    integer :: trouve

    ! variables locales
    integer :: i, k, acces, nature, nbModeles, code
    character(LEN=CPS_MAXLG) :: libelle
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersPot => NULL()
    integer :: base

    ! Initialisations
    nbModeles = 0
    trouve = CPS_ERR_DEF

    if (present(type_base)) then
       base = type_base
    else
       ! par défaut : recherche dans la base de référence et dans la base locale
       base = 0
    end if

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getModelesPotentielCorps", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_POTENTIEL, fichiersPot, type_base=base)

    ! 1ere passe : determination du nombre de modeles
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getModelesPotentielCorps", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_geti(acces, trim(libelle)//".code", code)
             if ((ret.eq.0).and.&
                  (code.eq.corps)) then
                nbModeles = nbModeles+1
             end if
          end if
       end do
    end do

    ! allocation memoire
    allocate(listeModelesPot(nbModeles))

    ! 2eme passe : lecture du nom des modeles
    k = 1
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getModelesPotentielCorps", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_geti(acces, trim(libelle)//".code", code)
             if ((ret.eq.0).and.&
                  (code.eq.corps)) then
                ret = acc_gets(acces, trim(libelle)//".modele_potentiel", &
                     listeModelesPot(k))
                k = k+1
             end if
          end if
       end do
    end do

    if (nbModeles.gt.0) then
       trouve = CPS_OK
    end if

    ! liberation memoire
    if (associated(fichiersPot)) then
       deallocate(fichiersPot, stat=iostat)
    end if

  end function cps_getModelesPotentielCorps




  function cps_getListeFichiersPotentiel(listeFichiersPot, rep, type_base) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListeFichiersPotentiel
!
!$Resume
!
!$Description
!  Obtenir la liste de tous les fichiers de potentiels disponibles.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getListeFichiersPotentiel(listeFichiersPot, [rep], [type_base])
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeFichiersPot
!.    logical :: rep 
!.    integer :: type_base
!.    integer :: trouve
!
!$Arguments
!>E/S   listeFichiersPot  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichier de potentiel
!>[E]   rep               :<logical>                         booléen indiquant si on souhaite avoir le chemin complet des fichiers
!>[E]   type_base         :<integer>                         
!>S     trouve            :<integer>                         CPS_OK s'il existe au moins un fichier
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeFichiersPot
    logical, intent(in), optional :: rep    ! indique si on indique le reperoire
    integer, optional, intent(in) :: type_base
    
    ! resultat
    integer :: trouve
    integer :: base
    
    ! variables locales
    integer :: i, k, acces, nature, nbFichiers
    character(LEN=CPS_MAXLG) :: libelle, f_tmp, rep_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersPot => NULL()

    ! Initialisations
    nbFichiers = 0
    trouve = CPS_ERR_DEF

    if (present(type_base)) then
       base = type_base
    else
       ! par défaut : recherche dans la base de référence et dans la base locale
       base = 0
    end if

     if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListFichiersPotentiel", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_POTENTIEL, fichiersPot, type_base=base)

    ! 1ere passe : determination du nombre de fichiers
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getListFichiersPotentiel", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_gets(acces, trim(libelle)//".fichier_potentiel", f_tmp)
             if (ret.eq.0) then
                nbFichiers = nbFichiers+1
             end if
          end if
       end do
    end do

    ! allocation memoire
    allocate(listeFichiersPot(nbFichiers))

    ! 2eme passe : lecture du nom des modeles
    k = 1
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getListFichiersPotentiel", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_gets(acces, trim(libelle)//".fichier_potentiel", f_tmp)
             if (present(rep).and.rep) then
                rep_tmp = ""
                ret = acc_gets(acces, trim(libelle)//".repertoire_potentiel", rep_tmp)
                ! on rajoute '/' si besoin
                rep_tmp = cpsi_getRep(rep_tmp)
                f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)
             end if
             listeFichiersPot(k) = trim(rep_base_ref)//"/"//trim(f_tmp)
             k = k+1
          end if
       end do
    end do

    if (nbFichiers.gt.0) then
       trouve = CPS_OK
    end if

    ! liberation memoire
    if (associated(fichiersPot)) then
       deallocate(fichiersPot, stat=iostat)
    end if
    
  end function cps_getListeFichiersPotentiel



  function cps_getListeModelesPotentiel(listeModelesPot) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListeModelesPotentiel
!
!$Resume
!
!$Description
!  Obtenir la liste de tous les modeles de potentiels.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getListeModelesPotentiel(listeModelesPot)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModelesPot
!.    integer :: trouve
!
!$Arguments
!>E/S   listeModelesPot  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des modèles de potentiel
!>S     trouve           :<integer>                         CPS_OK s'il existe au moins un modèle
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModelesPot
    
    ! resultat
    integer :: trouve

    ! variables locales
    integer :: i, k, acces, nature, nbModeles
    character(LEN=CPS_MAXLG) :: libelle, f_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersPot => NULL()

    ! Initialisations
    nbModeles = 0
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListModelesPotentiel", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_POTENTIEL, fichiersPot)

    ! 1ere passe : determination du nombre de fichiers
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getListModelesPotentiel", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_gets(acces, trim(libelle)//".modele_potentiel", f_tmp)
             if (ret.eq.0) then
                nbModeles = nbModeles+1
             end if
          end if
       end do
    end do

    ! allocation memoire
    allocate(listeModelesPot(nbModeles))

    ! 2eme passe : lecture du nom des modeles
    k = 1
    do i=1, cpsi_size_ptr(fichiersPot)
       call cpsi_getAccesMadona(fichiersPot(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getListModelesPotentiel", &
               partie_variable=trim(fichiersPot(i)))
          return
       end if
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.CPS_FIN_SEQ) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             ret = acc_gets(acces, trim(libelle)//".modele_potentiel", f_tmp)
             if (ret.eq.0) then
                listeModelesPot(k) = trim(f_tmp)
                k = k+1
             end if
          end if
       end do
    end do

    if (nbModeles.gt.0) then
       trouve = CPS_OK
    end if

    ! liberation memoire
    if (associated(fichiersPot)) then
       deallocate(fichiersPot, stat=iostat)
    end if

  end function cps_getListeModelesPotentiel



  function cps_getFichierPotentiel(modelePot, fichierPot, rep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierPotentiel
!
!$Resume
!
!$Description
!  Obtenir le fichier correspondant a un modele de potentiel particulier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierPotentiel(modelePot, fichierPot, [rep])
!.    character(LEN=*) :: modelePot
!.    character(LEN=*) :: fichierPot
!.    logical :: rep
!.    integer :: trouve
!
!$Arguments
!>E     modelePot   :<LEN=*>     nom du modèle de potentiel
!>S     fichierPot  :<LEN=*>     fichier correspondant au modèle
!>[E]   rep         :<logical>   booléen indiquant si on souhaite avoir le chemin complet du fichier
!>S     trouve      :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: modelePot
    character(LEN=*), intent(out) :: fichierPot
    logical, optional, intent(in) :: rep

    ! resultat
    integer :: trouve
    
    ! variables locales
    integer :: i, acces
    character(LEN=CPS_MAXLG) :: rep_tmp, f_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers => NULL()
    logical :: exist

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichierPotentiel", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_POTENTIEL, listFichiers)
    do i=1, cpsi_size_ptr(listFichiers)
       call cpsi_getAccesMadona(listFichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichierPotentiel", &
               partie_variable=trim(listFichiers(i)))
          return
       end if
       f_tmp=""
       trouve = cps_getCritere(acces, "modele_potentiel", modelePot, &
            "fichier_potentiel", f_tmp)
       rep_tmp = ""
       trouve = cps_getCritere(acces, "fichier_potentiel", f_tmp, &
            "repertoire_potentiel", rep_tmp)
       f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)
       fichierPot = trim(f_tmp)
       if (present(rep).and.rep.and.(trouve.eq.CPS_OK)) then
          ! on regarde d'abord si le fichier est dans la base locale
          fichierPot = trim(rep_base_local)//"/"//trim(f_tmp)
          inquire(file=trim(fichierPot), exist=exist)
          if (.not.exist) then
             ! si le fichier n'est pas dans la base locale, il est
             ! dans la base de reference
             fichierPot = trim(rep_base_ref)//"/"//trim(f_tmp)
          end if
       end if
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do
             
    ! liberation memoire
    if (associated(listFichiers)) then
       deallocate(listFichiers, stat=iostat)
    end if

  end function cps_getFichierPotentiel

  
  subroutine cps_getListAttCateg(cle, categorie, nb, noms_att, types_att, &
       vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListAttCateg
!
!$Resume
!
!$Description
! V2.0
! Lecture des attributs d'un element identifié par 'cle' qui correspond
! au nom de la structure MADONA à lire dans la categorie 'categorie'
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListAttCateg(cle, categorie, nb, noms_att, types_att, &
!.           vals_i, vals_d, vals_s, unites, att_def)
!.    character(LEN=*) :: cle, categorie
!.    integer :: nb
!.    character(LEN=CPS_MAXLG), dimension(:) :: noms_att
!.    integer, dimension(:) :: types_att
!.    integer, dimension(:) :: vals_i
!.    real(KIND=PM_REEL), dimension(:) :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(:) :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(:) :: unites
!.    logical, dimension(:) :: att_def
!
!$Arguments
!>E     cle        :<LEN=*>                   nom de la structure MADONA à lire  
!>E     categorie  :<LEN=*>                   nom de la catégorie dans laquelle chercher
!>S     nb         :<integer>                 nombre d'attributs
!>S     noms_att   :<LEN=CPS_MAXLG,DIM=(:)>   noms des attributs
!>S     types_att  :<integer,DIM=(:)>         types des attributs
!>S     vals_i     :<integer,DIM=(:)>         valeurs des attributs de type entier
!>S     vals_d     :<PM_REEL,DIM=(:)>         valeurs des attributs de type réels
!>S     vals_s     :<LEN=CPS_MAXLG,DIM=(:)>   valeurs des attributs de type chaînes de caractères
!>S     unites     :<LEN=CPS_MAXLG,DIM=(:)>   unités des attributs de type réels
!>S     att_def    :<logical,DIM=(:)>         booléens indiquant si les attributs sont définis ou non
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getTheorie
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cpsi_readStructure
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
    character(LEN=*), intent(in) :: cle, categorie
    integer, intent(out) :: nb
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: noms_att
    integer, dimension(:),  intent(out) :: types_att
    integer, dimension(:), intent(out) :: vals_i
    real(KIND=PM_REEL), dimension(:), intent(out) :: vals_d
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: vals_s
    character(LEN=CPS_MAXLG), dimension(:), intent(out) :: unites
    logical, dimension(:), intent(out) :: att_def

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers => NULL()
    character(LEN=CPS_MAXLG) :: th, th_courante
    integer :: acces, ret_th, i, trouve
    logical :: fic_ok, th_ok
    type(cpsi_desc) :: desc

    !
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListAttCateg", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getTheorie(th_courante)

    !
    acces = 0
    th_ok = .false.
    nb = 0
    call cps_getListFichiersCateg(trim(categorie), listFichiers)
    fic_ok = .false.
    do i=1, cpsi_size_ptr(listFichiers)
       call cpsi_getAccesMadona(listFichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN",            &
               routine="cps_getListAttCateg",                          &
               partie_variable=trim(listFichiers(i)))
          return
       end if
       ret = acc_exist(acces, trim(cle))
       if (ret.eq.1) then
          ! le fichier contient la structure recherchee
          ! regarder si theorie = theorie_courante sauf
          ! dans le cas des ephemerides pour cause de 
          ! compatibilite ascendante
          ret_th = acc_exist(acces, trim(cle)//".theorie")
          if (ret_th.eq.1) then
             ret = acc_gets(acces, trim(cle)//".theorie", th)
             if ((trim(th).eq.trim(th_courante)).or.                   &
                  (trim(categorie).eq."ephemerides")) then
                th_ok = .true.
             end if
          else
             th_ok = .true.
          end if
          if (th_ok) then
             ! on recupere la description du fichier
             trouve = cpsi_getDescFichier(trim(listFichiers(i)), desc)
             if (trouve.eq.CPS_OK) then
                fic_ok = .true.
                exit
             end if
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
          end if
       end if
    end do
    
    if (fic_ok) then
       nb = desc%nbChamps
       ret = acc_select(acces, trim(cle), ACC_STRUCT)
       call cpsi_readStructure(acces,                &
            desc, noms_att, types_att,               &
            vals_i, vals_d, vals_s, unites, att_def)
       ret = acc_select_end(acces)
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end if
    
    ! liberation memoire
    if (associated(listFichiers)) then
       deallocate(listFichiers, stat=iostat)
    end if

  end subroutine cps_getListAttCateg

  
  subroutine cps_getInfosCorps(code, noms_att, &
       types_att, vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getInfosCorps
!
!$Resume
!
!$Description
! V2.0
! Routine qui fournie tous les attributs disponibles pour un corps a partir
! des fichiers associes aux categories CPS_CATEG_CORPS, CPS_CATEG_CSTES_CORPS_THEORIE 
! et CPS_CATEG_KEPLER_CORPS_THEORIE pour la theorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getInfosCorps(code, noms_att, &
!.           types_att, vals_i, vals_d, vals_s, unites, att_def)
!.    integer :: code
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att
!.    integer, dimension(:), pointer :: types_att
!.    integer, dimension(:), pointer :: vals_i
!.    real(KIND=PM_REEL), dimension(:), pointer :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites
!.    logical, dimension(:), pointer :: att_def
!
!$Arguments
!>E     code       :<integer>                         code du corps
!>E/S   noms_att   :<LEN=CPS_MAXLG,DIM=(:),pointer>   noms des attributs
!>E/S   types_att  :<integer,DIM=(:),pointer>         types des attributs
!>E/S   vals_i     :<integer,DIM=(:),pointer>         valeurs des attributs de type entier
!>E/S   vals_d     :<PM_REEL,DIM=(:),pointer>         valeurs des attributs de type réels
!>E/S   vals_s     :<LEN=CPS_MAXLG,DIM=(:),pointer>   valeurs des attributs de type chaînes de caractères
!>E/S   unites     :<LEN=CPS_MAXLG,DIM=(:),pointer>   unités des attributs de type réels
!>E/S   att_def    :<logical,DIM=(:),pointer>         booléens indiquant si les attributs sont définis ou non
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getTheorie
!- cpsi_getListeDescFichiers
!- cps_getListAttCateg
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
    integer, intent(in) :: code
    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att
    integer, dimension(:), pointer :: types_att
    integer, dimension(:), pointer :: vals_i
    real(KIND=PM_REEL), dimension(:), pointer :: vals_d
    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s
    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites
    logical, dimension(:), pointer :: att_def
    
    ! variables locales
    type(cpsi_desc), dimension(:), pointer :: listFichiers => NULL()
    character(LEN=CPS_MAXLG), dimension(3) :: listCategories
    integer :: nb_att
    integer :: i, nb, ind_att
    character(LEN=CPS_MAXLG) :: cle, th_courante
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getInfosCorps", &
            partie_variable="cps_utilisateur")
       return
    end if

    ! construction de la cle
    cle = cpsi_intToChar(code)
    cle = "corps"//trim(cle)

    ! theorie courante
    call cps_getTheorie(th_courante)

    ! determination du nombre d'attributs
    nb_att = 0
    ! recuperer toutes les categories concernant les corps
    listCategories(1) = CPS_CATEG_CORPS
    listCategories(2) = CPS_CATEG_CSTES_CORPS_THEORIE
    listCategories(3) = CPS_CATEG_KEPLER_CORPS_THEORIE
    ! pour chaque categorie concernant les corps, 
    ! recuperer le nombre d'attributs
    do i=1, 3
       nb = cpsi_getNbAttCateg(listCategories(i))
       nb_att = nb_att+nb
    end do
    ! allocation memoire
    allocate(noms_att(nb_att))
    allocate(types_att(nb_att))
    allocate(vals_i(nb_att))
    allocate(vals_d(nb_att))
    allocate(vals_s(nb_att))
    allocate(unites(nb_att))
    allocate(att_def(nb_att))
    ! initialisation
    noms_att(:) = ""
    types_att(:) = -1
    vals_i(:) = 0
    vals_d(:) = 0.
    vals_s(:) = ""
    unites(:) = ""
    ! 
    call cpsi_getListeDescFichiers(listFichiers)
    ind_att = 1
    do i=1, 3
       call cps_getListAttCateg(cle,       &
            trim(listCategories(i)), nb,   &
            noms_att(ind_att:),            &
            types_att(ind_att:),           &
            vals_i(ind_att:),              &
            vals_d(ind_att:),              &
            vals_s(ind_att:),              &
            unites(ind_att:),              &
            att_def(ind_att:))
       ind_att = ind_att+nb
    end do
    
    ! liberation memoire
    call cpsi_deallocateDescFichiers(listFichiers)
    
  end subroutine cps_getInfosCorps

  
  subroutine cps_getInfosCorpsSansDoublon(code, noms_att, &
       types_att, vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getInfosCorpsSansDoublon
!
!$Resume
!
!$Description
! V2.0
! Routine qui fournie tous les attributs disponibles sans redondance pour un corps a partir
! des fichiers associes aux categories CPS_CATEG_CORPS, CPS_CATEG_CSTES_CORPS_THEORIE 
! et CPS_CATEG_KEPLER_CORPS_THEORIE pour la theorie courante.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getInfosCorpsSansDoublon(code, noms_att, &
!.           types_att, vals_i, vals_d, vals_s, unites, att_def)
!.    integer :: code
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att
!.    integer, dimension(:), pointer :: types_att
!.    integer, dimension(:), pointer :: vals_i
!.    real(KIND=PM_REEL), dimension(:), pointer :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites
!.    logical, dimension(:), pointer :: att_def
!
!$Arguments
!>E     code       :<integer>                         code du corps
!>E/S   noms_att   :<LEN=CPS_MAXLG,DIM=(:),pointer>   noms des attributs
!>E/S   types_att  :<integer,DIM=(:),pointer>         types des attributs
!>E/S   vals_i     :<integer,DIM=(:),pointer>         valeurs des attributs de type entier
!>E/S   vals_d     :<PM_REEL,DIM=(:),pointer>         valeurs des attributs de type réels
!>E/S   vals_s     :<LEN=CPS_MAXLG,DIM=(:),pointer>   valeurs des attributs de type chaînes de caratères
!>E/S   unites     :<LEN=CPS_MAXLG,DIM=(:),pointer>   unités des attributs de type réels
!>E/S   att_def    :<logical,DIM=(:),pointer>         booléens indiquant si les attributs sont définis ou non
!
!$Common
!
!$Routines
!- cps_getInfosCorps
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
    integer, intent(in) :: code
    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att
    integer, dimension(:), pointer :: types_att
    integer, dimension(:), pointer :: vals_i
    real(KIND=PM_REEL), dimension(:), pointer :: vals_d
    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s
    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites
    logical, dimension(:), pointer :: att_def

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att_tmp => NULL()
    integer, dimension(:), pointer :: types_att_tmp => NULL()
    integer, dimension(:), pointer :: vals_i_tmp => NULL()
    real(KIND=PM_REEL), dimension(:), pointer :: vals_d_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites_tmp => NULL()
    logical, dimension(:), pointer :: att_def_tmp => NULL()
    integer :: i, nb, ind_att
    logical:: present

    ! appel à cps_getInfosCorps
    call cps_getInfosCorps(code, noms_att_tmp, types_att_tmp, &
         vals_i_tmp, vals_d_tmp, vals_s_tmp, unites_tmp, att_def_tmp)

    nb = 0

    do i=1, cpsi_size_ptr(noms_att_tmp)
       present = cpsi_stringInTab(trim(noms_att_tmp(i)), &
            noms_att_tmp(1:i-1))
       if (.not.present) then
          ! l'attribut n'est pas deja present
          nb = nb+1
       end if
    end do

    allocate(noms_att(nb))
    allocate(types_att(nb))
    allocate(vals_i(nb))
    allocate(vals_d(nb))
    allocate(vals_s(nb))
    allocate(unites(nb))
    allocate(att_def(nb))
    
    noms_att(:) = ""
    types_att(:) = -1
    vals_i(:) = 0
    vals_d(:) = 0.
    vals_s(:) = ""
    unites(:) = ""

    ! recopie des valeurs
    ind_att = 0
    do i=1, cpsi_size_ptr(noms_att_tmp)
       present = cpsi_stringInTab(trim(noms_att_tmp(i)), &
            noms_att_tmp(1:i-1))
       if (.not.present) then
          ! l'attribut n'est pas deja present
          ! on le recopie
          ind_att = ind_att+1
          noms_att(ind_att) = trim(noms_att_tmp(i))
          types_att(ind_att) = types_att_tmp(i)
          vals_i(ind_att) = vals_i_tmp(i)
          vals_d(ind_att) = vals_d_tmp(i)
          vals_s(ind_att) = trim(vals_s_tmp(i))
          unites(ind_att) = trim(unites_tmp(i))
          att_def(ind_att) = att_def_tmp(i)
       end if
    end do
    
    ! liberation memoire
    if (associated(noms_att_tmp)) then
       deallocate(noms_att_tmp, stat=iostat)
    end if
    if (associated(types_att_tmp)) then
       deallocate(types_att_tmp, stat=iostat)
    end if
    if (associated(vals_i_tmp)) then
       deallocate(vals_i_tmp, stat=iostat)
    end if
    if (associated(vals_d_tmp)) then
       deallocate(vals_d_tmp, stat=iostat)
    end if
    if (associated(vals_s_tmp)) then
       deallocate(vals_s_tmp, stat=iostat)
    end if
    if (associated(unites_tmp)) then
       deallocate(unites_tmp, stat=iostat)
    end if
    if (associated(att_def_tmp)) then
       deallocate(att_def_tmp, stat=iostat)
    end if
    
  end subroutine cps_getInfosCorpsSansDoublon

  
  subroutine cps_getNoms(listCodes, lang, listNoms)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getNoms
!
!$Resume
!
!$Description
! V2.0
! Obtention des noms d'une liste de corps dans une langue ('fr' ou 'en')
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getNoms(listCodes, lang, listNoms)
!.    integer, dimension(:) :: listCodes
!.    character(LEN=*) :: lang
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listNoms
!
!$Arguments
!>E     listCodes  :<integer,DIM=(:)>                 liste des codes des corps
!>E     lang       :<LEN=*>                           langue des noms
!>E/S   listNoms   :<LEN=CPS_MAXLG,DIM=(:),pointer>   listes des noms des corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, dimension(:), intent(in) :: listCodes
    character(LEN=*), intent(in) :: lang
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listNoms
    
    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersCorps => NULL()
    logical, dimension(:), pointer :: corps_deja_traites => NULL()
    integer :: nb_corps, nb_fic, ind_fic, ind_corps, num_corps
    character(LEN=CPS_MAXLG) :: cle, nom
    integer :: acces, stat

    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getNoms", &
            partie_variable="cps_utilisateur")
       return
    end if

    !
    nom = "nom_"//trim(lang)

    nb_corps = size(listCodes)

    if (associated(listNoms)) deallocate (listNoms, stat=stat)
    allocate(listNoms(nb_corps))

    allocate(corps_deja_traites(nb_corps))
    listNoms(:) = ""
    corps_deja_traites(:) = .false.

    call cps_getListFichiersCateg(CPS_CATEG_CORPS, listFichiersCorps)
    nb_fic = cpsi_size_ptr(listFichiersCorps)

    num_corps = 0

    ! parcours sur le fichier
    do ind_fic=1, nb_fic
       call cpsi_getAccesMadona(listFichiersCorps(ind_fic), acces)
       if (acces.lt.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getNoms", &
               partie_variable=trim(listFichiersCorps(ind_fic)))

          if (associated(listFichiersCorps)) &
               deallocate(listfichiersCorps, stat=iostat)
          if (associated(corps_deja_traites)) &
               deallocate(corps_deja_traites, stat=iostat)
          return
       end if
       do ind_corps=1, nb_corps
          if (.not.corps_deja_traites(ind_corps)) then
             ! corps non traite
             cle = cpsi_intToChar(listCodes(ind_corps))
             cle = "corps"//trim(cle)
             ret = acc_exist(acces, cle)
             if (ret.eq.1) then
                ret = acc_select(acces, cle, ACC_STRUCT)
                ret = acc_gets(acces, trim(nom), listNoms(ind_corps))
                num_corps = num_corps+1
                corps_deja_traites(ind_corps) = .true.
                ret = acc_select_end(acces)
             end if
          end if
          if (num_corps.ge.nb_corps) then
             exit
          end if
       end do

       if (num_corps.ge.nb_corps) then
          exit
       end if
    end do
    
    ! liberation memoire
    if (associated(listFichiersCorps)) then
       deallocate(listfichiersCorps, stat=iostat)
    end if
    if (associated(corps_deja_traites)) then
       deallocate(corps_deja_traites, stat=iostat)
    end if

  end subroutine cps_getNoms

  
  subroutine cps_intersecterCodes(list_codes, nb_list, codes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_intersecterCodes
!
!$Resume
!
!$Description
! V2.0
! Routine qui calcule l'intersection de plusieurs listes de codes
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_intersecterCodes(list_codes, nb_list, codes)
!.    integer, dimension(:,:) :: list_codes
!.    integer :: nb_list
!.    integer, dimension(:), pointer :: codes
!
!$Arguments
!>E     list_codes  :<integer,DIM=(:,:)>         liste des listes de codes à intersecter
!>E     nb_list     :<integer>                   nombre de listes à intersecter
!>E/S   codes       :<integer,DIM=(:),pointer>   liste correspondant à l'intersection des liste de codes
!
!$Common
!
!$Routines
!- cpsi_intersecter
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
    integer, dimension(:,:), intent(in) :: list_codes
    integer, intent(in) :: nb_list
    integer, dimension(:), pointer :: codes
    
    ! variables locales
    integer, dimension(:), pointer :: codes_tmp => NULL()
    integer, dimension(:), pointer :: codes_tmp_2 => NULL()
    integer :: nb_codes, nb_max_corps
    integer :: i
    
    ! initialisation
    nb_max_corps = size(list_codes(1,:))
    nb_codes = nb_max_corps
    allocate(codes_tmp(nb_max_corps))

    codes_tmp(:) = list_codes(1,:)
    
    do i=2, nb_list
       call cpsi_intersecter(codes_tmp(1:nb_codes), list_codes(i, :), codes_tmp_2)
       nb_codes = cpsi_size_ptr(codes_tmp_2)
       codes_tmp(1:nb_codes) = codes_tmp_2(1:nb_codes)
       if (nb_codes.eq.0) then
          exit
       end if
    end do
    
    !
     allocate(codes(nb_codes))
     codes(1:nb_codes) = codes_tmp(1:nb_codes)

    ! liberation memoire
    if (associated(codes_tmp)) deallocate(codes_tmp, stat=iostat)
    if (associated(codes_tmp_2)) deallocate(codes_tmp_2, stat=iostat)
    
  end subroutine cps_intersecterCodes

  
  subroutine cpsi_intersecter(codes_1, codes_2, codes_res)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_intersecter
!
!$Resume
!
!$Description
! V2.0
! Routine qui calcule l'intersection de 2 listes de codes
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_intersecter(codes_1, codes_2, codes_res)
!.    integer, dimension(:) :: codes_1, codes_2
!.    integer, dimension(:), pointer :: codes_res
!
!$Arguments
!>E     codes_1    :<integer,DIM=(:)>           première liste à intersecter
!>E     codes_2    :<integer,DIM=(:)>           seconde liste à intersecter
!>E/S   codes_res  :<integer,DIM=(:),pointer>   intersection des deux listes
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
    integer, dimension(:), intent(in) :: codes_1, codes_2
    integer, dimension(:), pointer :: codes_res
    
    ! variables locales
    integer :: nb, i, j, ind
    integer, dimension(:), pointer :: codes_tmp => NULL()
    logical :: present

    ! initialisation
    nb = size(codes_1)
    if (size(codes_2).lt.nb) then
       nb = size(codes_2)
    end if
    allocate(codes_tmp(nb))
    ind = 0

    if (associated(codes_res)) then
       deallocate(codes_res, stat=iostat)
    end if
    
    !
    do i=1, size(codes_1)
       do j=1, size(codes_2)
          if (codes_2(j).eq.codes_1(i)) then
             present = cpsi_intInTab(codes_1(i), codes_tmp(1:ind))
             if ((ind.eq.0).or.(.not.present)) then
                ind = ind+1
                codes_tmp(ind) = codes_1(i)
             end if
             exit
          end if
       end do
    end do

    ! recopie du resultats
    allocate(codes_res(ind))
    codes_res(1:ind) = codes_tmp(1:ind)
    
    ! liberation memoire
    if (associated(codes_tmp)) then
       deallocate(codes_tmp, stat=iostat)
    end if
    
  end subroutine cpsi_intersecter

  
  function cpsi_intInTab(val, tab) result (present)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_intInTab
!
!$Resume
!
!$Description
! V2.0
! Fonction interne
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  present = cpsi_intInTab(val, tab)
!.    integer :: val
!.    integer, dimension(:) :: tab
!.    logical :: present
!
!$Arguments
!>E     val      :<integer>           valeur à rechercher
!>E     tab      :<integer,DIM=(:)>   tableau dans lequel effectuer la recherche
!>S     present  :<logical>           .true. si la valeur est présente dans le tableau, .false. sinon
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
    integer, intent(in) :: val
    integer, dimension(:), intent(in) :: tab
    
    ! variable de retour
    logical :: present
    
    ! variables locales
    integer :: i
    
    present = .false.
    do i=1, size(tab)
       if (tab(i).eq.val) then
          present = .true.
          exit
       end if
    end do

  end function cpsi_intInTab


  
  subroutine cps_getListTheories(listeTheories)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListTheories
!
!$Resume
!
!$Description
! V2.0
! Routine qui renvoie la liste des théories planétaires disponibles.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListTheories(listeTheories)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeTheories
!
!$Arguments
!>E/S   listeTheories  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des théories disponibles
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeTheories
    
    ! variables locales
    character(LEN=CPS_MAXLG) :: libelle, nom
    integer :: nature, acces, nb_fic, i, nbTh
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiersTh => NULL()
    type(maillon_s), pointer :: listTh, thCourante
    logical :: existe

    !
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListTheories", &
            partie_variable="cps_utilisateur")
       return
    end if
    
    call cps_getListFichiersCateg(CPS_CATEG_THEORIE, listFichiersTh)
    nb_fic = cpsi_size_ptr(listFichiersTh)
    
    allocate(listTh)
    nullify(listTh%suivant)

    thCourante => listTh

    nbTh = 0

    do i=1, nb_fic
       ! fichier i
       call cpsi_getAccesMadona(listFichiersTh(i), acces)
       if (acces.LT.0) then
         call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getListTheories", &
               partie_variable=trim(listFichiersTh(i)))
         ! liberation memoire
         if (associated(listFichiersTh)) deallocate(listFichiersTh)
         return 
       else
          ret = acc_scan_reset(acces)
          do
             ret = acc_scan(acces, libelle, nature)
             if (nature.eq.0) then
                exit
             end if
             ret = acc_select(acces, libelle, ACC_STRUCT)
             ret = acc_gets(acces, "nom", nom)
             existe = cpsi_nomExisteDeja(nom, listTh)
             if (.not.existe) then
                thCourante%val = nom
                allocate(thCourante%suivant)
                thCourante => thCourante%suivant
                nullify(thCourante%suivant)
                nbth = nbth + 1
             end if
             ret = acc_select_end(acces)
          end do
       end if
    end do

    allocate(listeTheories(nbTh))

    ThCourante => listTh
    do i=1, nbTh
       listTh => listTh%suivant
       listeTheories(i) = trim(thCourante%val)
       ! liberation memoire
       deallocate(thCourante, stat=iostat)
       thCourante => listTh
    end do

    ! liberation memoire
    deallocate(listFichiersTh, stat=iostat)
    deallocate(listTh, stat=iostat)

  end subroutine cps_getListTheories


  
  function cpsi_nomExistedeja(nom, listNoms) result (existe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_nomExistedeja
!
!$Resume
!
!$Description
! V2.0
! Fonction interne.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  existe = cpsi_nomExistedeja(nom, listNoms)
!.    character(LEN=*) :: nom
!.    type(maillon_s), pointer :: listNoms
!.    logical :: existe
!
!$Arguments
!>E     nom       :<LEN=*>               nom à rechercher
!>E/S   listNoms  :<maillon_s,pointer>   liste des maillons à parcourir pendant la recherche
!>S     existe    :<logical>             .true. si le nom est présent dans la liste, .false. sinon
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
    character(LEN=*), intent(in) :: nom
    type(maillon_s), pointer :: listNoms
    logical :: existe
    ! variables locales
    type(maillon_s), pointer :: nomCourant

    ! Initialisation
    existe = .false.

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_nomExisteDeja", &
            partie_variable="cps_utilisateur")
       return
    end if 

    nomCourant => listNoms
    
    do
       if (.not.associated(nomCourant%suivant)) then
          exit
       else
          if (trim(nom).eq.trim(nomCourant%val)) then
             existe = .true.
          end if
          nomCourant => nomCourant%suivant
       end if
    end do

  end function cpsi_nomExistedeja
  
  
  subroutine cps_getListFichiersLocal(listFichiers)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListFichiersLocal
!
!$Resume
!
!$Description
! V2.0
! Renvoie la liste des fichiers de la base locale.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListFichiersLocal(listFichiers)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers
!
!$Arguments
!>E/S   listFichiers  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers de la base locale
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
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers

    ! variables locales
    integer :: i, nb, lg_rep_local


    ! Code
    lg_rep_local = len(trim(rep_base_local))+2


    if (associated(listFichiersLocal)) then
       nb = cpsi_size_ptr(listFichiersLocal)

       if (associated(listFichiers)) deallocate(listFichiers, stat=iostat)
       allocate(listFichiers(nb))
       do i=1, nb
          if (len(listFichiersLocal(i)%fichier).ge.lg_rep_local) then
             listFichiers(i) = listFichiersLocal(i)%fichier(lg_rep_local:)
          else
             listFichiers(i) = ""
          end if
       end do
    else
       ! S'il n'y a pas de base locale, on rend un pointeur nul
       nb = 0
       if (associated(listFichiers)) deallocate(listFichiers, stat=iostat)
       if (iostat < 0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
            routine="cps_getListFichiersLocal")
          return
       end if
       listFichiers => NULL()
    end if
 
    
  end subroutine cps_getListFichiersLocal

  
  subroutine cps_getListFichiersRef(listFichiers)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListFichiersRef
!
!$Resume
!
!$Description
! V2.0
! Renvoie la liste des fichiers de la base de reference.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListFichiersRef(listFichiers)
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers
!
!$Arguments
!>E/S   listFichiers  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers de la base de référence
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
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers

    ! variables locales
    integer :: i, nb, lg_rep_ref

    lg_rep_ref = len(trim(rep_base_ref))+2
    nb = cpsi_size_ptr(listFichiersRef)
    if (associated(listFichiers)) deallocate(listFichiers, stat=iostat)
    allocate(listFichiers(nb))
    do i=1, nb
       if (len(listFichiersRef(i)%fichier).ge.lg_rep_ref) then
          listFichiers(i) = listFichiersRef(i)%fichier(lg_rep_ref:)
       else
          listFichiers(i) = ""
       end if
    end do
    
  end subroutine cps_getListFichiersRef

 
  subroutine cps_setBaseLocale(repBaseLocale)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_setBaseLocale
!
!$Resume
!
!$Description
! V2.0
! Utilisation d'une base locale dont le repertoire racine est
! repBaseLocale.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_setBaseLocale(repBaseLocale)
!.    character(LEN=*) :: repBaseLocale
!
!$Arguments
!>E     repBaseLocale  :<LEN=*>   répertoire principal de la base locale
!
!$Common
!
!$Routines
!- cps_close_utilisateur
!- cps_init_utilisateur
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
    character(LEN=*), intent(in) :: repBaseLocale

    ! fermeture de COMPAS en vue d'une reinitialisation
    call cps_close_utilisateur()

    ! on positionne le repertoire de la base locale
    rep_base_local = trim(repBaseLocale)
    ! on indique au module cps_desc qu'il ne faut pas lire le repertoire
    ! de la base locale indique dans le fichier compas.rc lors de son
    ! initialisation
    if (trim(repBaseLocale).ne."") then
       cps_base_locale_chargee = .true.
    end if
    
    ! on reinitialise COMPAS avec la nouvelle base locale
    call cps_init_utilisateur()
    if ( MSP_gen_messages("cps_setBaseLocale") ) return

  end subroutine cps_setBaseLocale

  
  function cps_getListModelesCorps(corps, mod, liste_mod, type_base) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListModelesCorps
!
!$Resume
!
!$Description
! V2.0
! Renvoie la liste des modeles de potentiel ou d'atmosphere pour un
! corps.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getListModelesCorps(corps, mod, liste_mod, [type_base])
!.    integer :: corps
!.    character(LEN=*) :: mod
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_mod
!.    integer :: type_base
!.    integer :: trouve
!
!$Arguments
!>E     corps      :<integer>                         code du corps
!>E     mod        :<LEN=*>                           type du modele ('atmosphere' ou 'potentiel')
!>E/S   liste_mod  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des modèles
!>[E]   type_base  :<integer>                         
!>S     trouve     :<integer>                         CPS_OK s'il existe au moins un modèle, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    integer, intent(in) :: corps
    character(LEN=*), intent(in) :: mod
    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_mod
    integer, optional, intent(in) :: type_base

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=256) :: categ, libelle
    integer :: nb_fic, nb_mod, acces, i, nature, code
    character(LEN=256), dimension(:), pointer :: fichiers
    type(maillon_s), pointer :: listMod, modCourant
    logical :: existe
    integer :: base

    ! Initialisation
    trouve = CPS_ERR_DEF
    
    if (present(type_base)) then
       base = type_base
    else
       ! par defaut : recherche dans la base de référence et dans la base locale
       base = 0
    end if

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListModelesCorps", &
            partie_variable="cps_utilisateur")
       return
    end if

    
    ! initialisation
    categ = "modeles_"//mod
    nb_mod = 0
    allocate(listMod)
    nullify(listMod%suivant)
    listMod%val=""
    modCourant => listMod
    
    ! Initialisation des pointeurs locaux
    nullify(fichiers)

    ! fichiers de la categorie
    call cps_getListFichiersCateg(trim(categ), fichiers, type_base=base)
    nb_fic = cpsi_size_ptr(fichiers)
    
    do i=1, nb_fic
       ! ouverture du fichier courant
       call cpsi_getAccesMadona(trim(fichiers(i)), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getListModelesCorps", &
               partie_variable=trim(fichiers(i)))
          ! liberation memoire
          if (associated(fichiers)) deallocate(fichiers)
          return 
       end if
       
       ! parcourt des structures
       ret = acc_scan_reset(acces)
       do 
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          end if
          ! selection de la structure
          ret = acc_select(acces, trim(libelle), ACC_STRUCT)
          ! recuperation du code
          ret = acc_geti(acces, "code", code)
          if (code.eq.corps) then
             ! le code correspond au corps
             ! le nom du modele est le nom de la structure, ie. libelle
             existe = cpsi_nomExisteDeja(libelle, listMod)
             if (.not.existe) then
                modCourant%val = trim(libelle)
                allocate(modCourant%suivant)
                modCourant => modCourant%suivant
                nullify(modCourant%suivant)
                modCourant%val=""
                nb_mod = nb_mod+1
             end if
          end if
          ! fin de la selection de la structure
          ret = acc_select_end(acces)
          !end if
       end do
       
    end do
    
    ! resultat
    if (nb_mod.gt.0) then
       trouve = CPS_OK
    end if

    ! variable de sortie
    if (associated(liste_mod)) deallocate(liste_mod, stat=iostat)
    allocate(liste_mod(nb_mod))
    modCourant => listMod
    do i=1, nb_mod
       listMod => listMod%suivant
       liste_mod(i) = trim(modCourant%val)
       ! liberation memoire
       deallocate(modCourant, stat=iostat)
       modCourant => listMod
    end do
    
    ! liberation memoire
    if (associated(listmod)) deallocate(listmod, stat=iostat)
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end function cps_getListModelesCorps

  
  function cps_getFichierModele(mod, nom_mod, fic, rep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierModele
!
!$Resume
!
!$Description
!  V2.0
!  Retourne le nom du fichier associé à un modèle de potentiel
!  ou à un modèle d'atmosphère.
!  En option, le repertoire relatif par rapport au repertoire de la base est
!  ajoute au nom du fichier.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierModele(mod, nom_mod, fic, [rep])
!.    character(LEN=*) :: mod, nom_mod
!.    character(LEN=*) :: fic
!.    logical :: rep
!.    integer :: trouve
!
!$Arguments
!>E     mod      :<LEN=*>     type de modèle ('atmosphere' ou 'potentiel')
!>E     nom_mod  :<LEN=*>     nom du modèle
!>S     fic      :<LEN=*>     fichier associé au modèle
!>[E]   rep      :<logical>   booléen indiquant si on souhaite avoir le chemin complet du fichier
!>S     trouve   :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    !arguments
    character(LEN=*), intent(in) :: mod, nom_mod
    character(LEN=*), intent(out) :: fic
    logical, optional, intent(in) :: rep
    
    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG) :: categ, buffer, fic_tmp, rep_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    integer :: i, acces
    logical :: exist, replocal

    ! initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichierModele", &
            partie_variable="cps_utilisateur")
       return
    end if

    categ = "modeles_"//trim(mod)
    fic = "" 
    replocal = .false.
    if (present(rep)) replocal = rep
  
    ! liste des fichiers de la categorie
    call cps_getListFichiersCateg(trim(categ), fichiers)
    
    do i=1, cpsi_size_ptr(fichiers)
       ! ouverture du fichier courant
       call cpsi_getAccesMadona(trim(fichiers(i)), acces)
       if (acces.lt.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getListModelesCorps", &
               partie_variable=trim(fichiers(i)))
          ! libeartion memoire
          if (associated(fichiers)) deallocate(fichiers)
          return 
       end if
       ! le nom du modele correspond au nom de la structure
       ret = acc_exist(acces, trim(nom_mod))
       ! si basent, on passe au fichier suivant
       if (ret.eq.0) cycle

       ! le modele existe
       ! selection
       ret = acc_select(acces, trim(nom_mod), ACC_STRUCT)
       ! lecture du repertoire
       ret = acc_gets(acces, "repertoire_"//trim(mod), rep_tmp)
       ! lecture du fichier
       ret = acc_gets(acces, "fichier_"//trim(mod), buffer)
       fic_tmp = trim(rep_tmp)//"/"//trim(buffer)
       fic = trim(fic_tmp)

       ! Cas ou l'on veut le chemin complet
       if (replocal) then
          ! on regarde si le fichier est dans la base locale
          fic = trim(rep_base_local)//"/"//trim(fic_tmp)
          inquire(file=trim(fic), exist=exist)
          if (.not.exist) then
             ! si le fichier n'est pas dans la base locale,
             ! alors il est dans la base de reference
             fic = trim(rep_base_ref)//"/"//trim(fic_tmp)
          end if
       endif
       ! fin de la selection
       ret = acc_select_end(acces)
       ! fin
       trouve = CPS_OK
       exit
    end do


    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end function cps_getFichierModele

  
  function cps_getListEltsCateg(categ, liste_elts) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListEltsCateg
!
!$Resume
!
!$Description
! V2.0
! Renvoie la liste des noms des structures MADONA disponibles dans
! une categorie.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getListEltsCateg(categ, liste_elts)
!.    character(LEN=*) :: categ
!.    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_elts
!.    integer :: trouve
!
!$Arguments
!>E     categ       :<LEN=*>                           catégorie concernée
!>E/S   liste_elts  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des noms des structures MADONA contenues dans les fichiers associés à la catégorie
!>S     trouve      :<integer>                         CPS_OK s'il existe au moins une structure, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: categ
    character(LEN=CPS_MAXLG), dimension(:), pointer :: liste_elts
    
    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    character(LEN=CPS_MAXLG) :: libelle
    integer :: nb_fic, acces, i, nature, nb_elts
    logical :: existe
    type(maillon_s), pointer :: listElts, eltCourant

    ! initialisation
    trouve = CPS_ERR_DEF
    
    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getListModelesCorps", &
            partie_variable="cps_utilisateur")
       return
    end if
    
    nb_elts = 0
    allocate(listElts)
    nullify(listElts%suivant)
    listElts%val=""
    eltCourant => listElts
    

    ! liste des fichiers associes a la categorie
    call cps_getListFichiersCateg(trim(categ), fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    do i=1, nb_fic
       ! ouverture du fichier courant
       call cpsi_getAccesMadona(trim(fichiers(i)), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_getListEltsCateg", &
               partie_variable=trim(fichiers(i)))
          ! liberation memoire
          if (associated(fichiers)) deallocate(fichiers)
          return
       end if
       
       ! parcourt des structures
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          elseif(nature.eq.ACC_STRUCT) then
             ! on prend le nom de la structure, ie. libelle
             existe = cpsi_nomExisteDeja(libelle, listElts)
             if (.not.existe) then
                eltCourant%val = trim(libelle)
                allocate(eltCourant%suivant)
                eltCourant => eltCourant%suivant
                nullify(eltCourant%suivant)
                eltCourant%val=""
                nb_elts = nb_elts+1
             end if
          end if
       end do
       
    end do
    

    ! resultat
    if (nb_elts.gt.0) then
       trouve = CPS_OK
    end if

    allocate(liste_elts(nb_elts))
    eltCourant => listElts
    do i=1, nb_elts
       listElts => listElts%suivant
       liste_elts(i) = trim(eltCourant%val)
       ! liberation memoire
       deallocate(eltCourant, stat=iostat)
       eltCourant => listElts
    end do
    ! liberation memoire du dernier element de la chaine (nul mais alloué)
    deallocate(eltCourant)
    

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    
  end function cps_getListEltsCateg

  
  function cps_getFichierBullASure(date_du_jour, acces, fichier) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierBullASure
!
!$Resume
!
!$Description
! V2.0
! Obtention de l'acces MADONA sur le fichier contenant les donnees
! fixes issues du bulletin A correspondant a la date specifiee.
! Si la date n'est pas connue, le fichier est celui ou les donnees
! ajoutees.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierBullASure(date_du_jour, acces, fichier)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    integer :: acces
!.    character(LEN=*) :: fichier
!.    integer :: trouve
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>   date des données
!>S     acces         :<integer>   accès MADONA sur le fichier de données
!>S     fichier       :<LEN=*>     nom du fichier de données
!>S     trouve        :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    integer, intent(out) :: acces
    character(LEN=*), intent(out) :: fichier
    
    ! variables locales
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: date1950, sec
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    integer :: i, nb_fic, acces_liste_fichiers
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: trouve
    character(LEN=256) :: cle
    logical :: exist
    
    date1950 = date_du_jour-33282.00
    call md_jourfrac_joursec(date1950, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour) 
    
    write(cle, 10) aaaa
    cle = "annee_"//trim(cle)

    trouve = CPS_ERR_DEF

    call cps_getListFichiersCateg("bulletinA_sure", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    do i=1, nb_fic
       call cpsi_getAccesMadona(trim(fichiers(i)), acces_liste_fichiers)
       if (acces_liste_fichiers.lt.0) then
          exit
       end if
       ret = acc_exist(acces_liste_fichiers, trim(cle)//".fichier")
       if (ret.eq.1) then
          ret = acc_gets(acces_liste_fichiers, trim(cle)//".fichier", fichier)
          exist = .false.
          ! on regarde si le fichier existe d'abord dans la base locale
          inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=exist)
          if (exist) then
             ! le fichier existe dans la base locale : on forme le chemin complet
             fichier = trim(rep_base_local)//"/"//trim(fichier)
          else
             ! le fichier est dans la base de reference : on forme le chemin complet
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
          end if
          ! ouverture de l'acces MADONA
          call cpsi_getAccesMadona(trim(fichier), acces)
          trouve = CPS_OK
          exit
       end if
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    
10  format (i4)

  end function cps_getFichierBullASure


  
  function cps_getFichierBullAPred(date_du_jour, acces, fichier) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierBullAPred
!
!$Resume
!
!$Description
! V2.0
! Obtention de l'acces MADONA sur le fichier contenant les donnees
! predites issues du bulletin A correspondant a la date specifiee.
! Si la date n'est pas connue, le fichier est celui ou les donnees
! ajoutees.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierBullAPred(date_du_jour, acces, fichier)
!.    real(KIND=PM_REEL) :: date_du_jour
!.    integer :: acces
!.    character(LEN=*) :: fichier
!.    integer :: trouve
!
!$Arguments
!>E     date_du_jour  :<PM_REEL>   date des données
!>S     acces         :<integer>   accès MADONA ouvert sur le fichier de données
!>S     fichier       :<LEN=*>     nom du fichier
!>S     trouve        :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cpsi_chercherBullAPredAntePost
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
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    integer, intent(out) :: acces
    character(LEN=*), intent(out) :: fichier

    ! resultat
    integer :: trouve
    

    ! variables locales
    real(KIND=PM_REEL) :: date1950, sec
    integer :: jj, mm, aaaa, h, min
    character(LEN=256) :: buff_jj, buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()

    logical :: trouve_ante, trouve_post, exist
    integer :: acces_fic, i, nb_fic
    character(LEN=256) :: cle_fic
    character(LEN=256) :: dernier_libelle
    
    !
    trouve = CPS_ERR_DEF
    
    
    !
    date1950 = date_du_jour-33282.00
    call md_jourfrac_joursec(date1950, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    write(buff_jj,10) jj
    if (buff_jj(1:1).eq.' ') then
       buff_jj(1:1) = '0'
    end if
    write(buff_mm,10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20) aaaa

    cle_fic = "jour_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
    
    ! liste des fichiers d'indexage des fichiers de prédictions
    call cps_getListFichiersCateg("bulletinA_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)

    do i=1, nb_fic
       call cpsi_getAccesMadona(trim(fichiers(i)), acces_fic)
       if (acces_fic.LT.0) then
          ! liberation memoire
          if (associated(fichiers)) deallocate(fichiers)
          return
       end if
       
       dernier_libelle = "jour"

       trouve_post = .false.
       trouve_ante = .false.

       ! on regarde s'il existe un fichier de prédictions 
       ! créé à la date 'date_du_jour'
       ret = acc_exist(acces_fic, trim(cle_fic)//".fichier")
       if (ret.eq.1) then
          dernier_libelle = trim(cle_fic)
          ret = acc_gets(acces_fic, trim(dernier_libelle)//".fichier", fichier)
          exist = .false.
          ! on regarde d'abord si le fichier est present dans la base locale
          inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=exist)
          if (exist) then
             ! le fichier existe dans la base locale : on forme le chemin complet
             fichier = trim(rep_base_local)//"/"//trim(fichier)
          else
             ! le fichier est dans la base de reference : on forme le chemin complet
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
          end if
          ! ouverture de l'acces MADONA
          !fichier = trim(rep_base_ref)//"/"//trim(fichier)
          call cpsi_getAccesMadona(trim(fichier), acces)
          trouve = CPS_OK
          exit
       end if

       ! sinon, on cherche la prédiction la plus proche
       ret = acc_scan_reset(acces_fic)
       call cpsi_chercherBullAPredAntePost(acces_fic, date_du_jour, &
            trouve_ante, trouve_post, dernier_libelle)

       if (trouve_ante.and.trouve_post) then
          ret = acc_gets(acces_fic, trim(dernier_libelle)//".fichier", fichier)
          exist = .false.
          ! on regarde d'abord si le fichier est present dans la base locale
          inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=exist)
          if (exist) then
             ! le fichier existe dans la base locale : on forme le chemin complet
             fichier = trim(rep_base_local)//"/"//trim(fichier)
          else
             ! le fichier est dans la base de reference : on forme le chemin complet
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
          end if
          ! ouverture de l'acces MADONA
          call cpsi_getAccesMadona(trim(fichier), acces)
          trouve = CPS_OK
          exit
       end if
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

10  format(i2)
20  format(i4)
    
  end function cps_getFichierBullAPred

  
  ! Routine interne pour cps_getFichierBullAPred()
  subroutine cpsi_chercherBullAPredAntePost(acces_fic, date_du_jour, &
       trouve_ante, trouve_post, dernier_libelle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_chercherBullAPredAntePost
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
!  call cpsi_chercherBullAPredAntePost(acces_fic, date_du_jour, &
!.           trouve_ante, trouve_post, dernier_libelle)
!.    integer :: acces_fic
!.    real(KIND=PM_REEL) :: date_du_jour
!.    logical :: trouve_ante, trouve_post
!.    character(LEN=*) :: dernier_libelle
!
!$Arguments
!>E     acces_fic        :<integer>   
!>E     date_du_jour     :<PM_REEL>   
!>S     trouve_ante      :<logical>   
!>S     trouve_post      :<logical>   
!>S     dernier_libelle  :<LEN=*>     
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
    integer, intent(in) :: acces_fic
    real(KIND=PM_REEL), intent(in) :: date_du_jour
    logical, intent(out) :: trouve_ante, trouve_post
    character(LEN=*), intent(out) :: dernier_libelle
    
    ! variables locales
    character(LEN=256) :: libelle
    integer :: nature, res
    real(KIND=PM_REEL) :: date

    ! Initialisations
    trouve_ante = .false.
    trouve_post = .false.
    dernier_libelle=""

    ! Recherche de la date la plus proche
    do
       ret = acc_scan(acces_fic, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       elseif (nature.eq.ACC_STRUCT) then
          ret = acc_getd(acces_fic, trim(libelle)//".date", date, "")
          res = cpsi_compareReels(date, date_du_jour)
          if (res.eq.1) then
             trouve_post = .true.
             exit
          else
             dernier_libelle = trim(libelle)
             trouve_ante = .true.
          end if
       end if
    end do
    
  end subroutine cpsi_chercherBullAPredAntePost


  function cps_getBullASure(date, data_sure) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullASure
!
!$Resume
!
!$Description
! V2.0
! Retourne les donnees sures du bulletin A pour une date donnee
! data_sure(1) = date MJD
! data_sure(2) = UTC-UT1
! data_sure(3) = erreur UTC-UT1
! data_sure(4) = PM_x
! data_sure(5) = PM_y
! data_sure(6) = erreur PM_x
! data_sure(7) = erreur PM_y
! data_sure(8) = dpsi
! data_sure(9) = deps
! data_sure(10) = erreur dpsi
! data_sure(11) = erreur deps
! data_sure(12) = dX
! data_sure(13) = dY
! data_sure(14) = erreur dX
! data_sure(15) = erreur dY
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_getBullASure(date, data_sure)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(15) :: data_sure
!.    integer :: ok
!
!$Arguments
!>E     date       :<PM_REEL>            date des données
!>S     data_sure  :<PM_REEL,DIM=(15)>   tableau des données sûres
!>S     ok         :<integer>            CPS_OK si les données sont trouvées
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
    real(KIND=PM_REEL), intent(in) :: date
    real(KIND=PM_REEL), dimension(15), intent(out) :: data_sure
    
    ! resultat
    integer :: ok

    ! variables locales
    integer :: trouve, acces
    character(LEN=256) :: fichier, cle

    ! initialisation
    ok = CPS_ERR_DEF

    ! recuperer le fichier
    trouve = cps_getFichierBullASure(date, acces, fichier)
    if (trouve.ne.CPS_OK) then
       ! le fichier n'existe pas
       return
    end if
    
    cle = cpsi_getCleBullA(date)
    ret = acc_exist(acces, trim(cle))
    if (ret.eq.1) then
       ! la donnee existe
       ret = acc_select(acces, trim(cle), ACC_STRUCT)
       
       ret = acc_getd(acces, "date", data_sure(1), "")
       ret = acc_getd(acces, "UTC_UT1", data_sure(2), "s")
       ret = acc_getd(acces, "UTC_UT1_err", data_sure(3), "s")
       ret = acc_getd(acces, "PM_x", data_sure(4), "")
       ret = acc_getd(acces, "PM_y", data_sure(5), "")
       ret = acc_getd(acces, "PM_x_err", data_sure(6), "")
       ret = acc_getd(acces, "PM_y_err", data_sure(7), "")
       ret = acc_getd(acces, "dpsi", data_sure(8), "")
       ret = acc_getd(acces, "deps", data_sure(9), "")
       ret = acc_getd(acces, "dpsi_err", data_sure(10), "")
       ret = acc_getd(acces, "deps_err", data_sure(11), "")
       ret = acc_getd(acces, "dX", data_sure(12), "")
       ret = acc_getd(acces, "dY", data_sure(13), "")
       ret = acc_getd(acces, "dX_err", data_sure(14), "")
       ret = acc_getd(acces, "dY_err", data_sure(15), "")
       
       ret = acc_select_end(acces)
       ok = CPS_OK
    end if

  end function cps_getBullASure


  
  function cps_getBullASureCreneau(date_deb, date_fin, tab_data, nb_data) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullASureCreneau
!
!$Resume
!
!$Description
! V2.0
! Obtenir les donnees sures du bulletin A sur un creneau de dates.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_getBullASureCreneau(date_deb, date_fin, tab_data, nb_data)
!.    real(KIND=PM_REEL) :: date_deb, date_fin
!.    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE,15) :: tab_data
!.    integer :: nb_data
!.    integer :: ok
!
!$Arguments
!>E     date_deb  :<PM_REEL>                               date de début de créneau
!>E     date_fin  :<PM_REEL>                               date de fin du créneau
!>S     tab_data  :<PM_REEL,DIM=(CPS_MAX_BULLA_SURE,15)>   tableau des données
!>S     nb_data   :<integer>                               nombre d'éléments du tableau (date_deb-date_fin+1)
!>S     ok        :<integer>                               CPS_OK si toutes les donnnées sont trouvées, CPS_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date_deb, date_fin
    real(KIND=PM_REEL), dimension(CPS_MAX_BULLA_SURE,15), intent(out) :: tab_data
    integer, intent(out) :: nb_data

    ! resultat
    integer :: ok
    
    ! variables locales
    real(KIND=PM_REEL) :: date
    integer :: trouve, nb, i
    real(KIND=PM_REEL), dimension(15) :: tab_tmp
    
    ! initialisation
    ok = CPS_OK
    nb_data = 0
    nb = int(date_fin)-int(date_deb)+1
    date = date_deb

    if (nb.lt.1) then
       ! erreur : 'date_deb' doit etre inferieure a 'date_fin'
       ok = CPS_ERR_DEF
       return
    end if

    ! boucle sur les dates
    do i=1, nb
       trouve = cps_getBullASure(date, tab_tmp)
       if (trouve.eq.CPS_OK) then
          ! les donnees ont ete trouvees pour la date courante
          nb_data = nb_data+1
          tab_data(nb_data, 1:15) = tab_tmp(1:15)
       end if
       date = date+1._pm_reel
    end do
    
  end function cps_getBullASureCreneau

  
  subroutine cps_getBullADernieresPred(date, data_pred, nb_pred)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullADernieresPred
!
!$Resume
!
!$Description
! V2.0
! Obtenir les dernieres predictions.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getBullADernieresPred(date, data_pred, nb_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(120, 15) :: data_pred
!.    integer :: nb_pred
!
!$Arguments
!>S     date       :<PM_REEL>                 date à laquelle ont été effectuée les dernièeres prédictions
!>S     data_pred  :<PM_REEL,DIM=(120, 15)>   tableau des dernières prédictions
!>S     nb_pred    :<integer>                 taille du tableau
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(out) :: date
    real(KIND=PM_REEL), dimension(120, 15), intent(out) :: data_pred
    integer, intent(out) :: nb_pred
    

    ! variables locales
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: acces_fic, nb_fic, nature, acces
    character(LEN=256) :: libelle, fichier, dernier_libelle
    
    ! initialisation
    nb_pred = 0
    date = 0.0
    dernier_libelle = ""
    libelle = ""
    

    call cps_getListFichiersCateg("bulletinA_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    ! on prend le dernier fichier
    call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
    ! on prend la derniere structure
    ret = acc_scan_reset(acces_fic)
    do
       ret = acc_scan(acces_fic, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else
          dernier_libelle = trim(libelle)
       end if
    end do
    
    if (dernier_libelle.ne."") then
       ret = acc_getd(acces_fic, trim(dernier_libelle)//".date", date, "")
       ret = acc_gets(acces_fic, trim(dernier_libelle)//".fichier", fichier)
       fichier = trim(rep_base_ref)//"/"//trim(fichier)
       ! ouverture du fichier
       call cpsi_getAccesMadona(trim(fichier), acces)
       ! lecture des donnees
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             nb_pred = nb_pred+1
             ret = acc_select(acces, trim(libelle), ACC_STRUCT)
             
             ret = acc_getd(acces, "date", data_pred(nb_pred, 1), "")
             ret = acc_getd(acces, "UTC_UT1", data_pred(nb_pred, 2), "s")
             ret = acc_getd(acces, "UTC_UT1_err", data_pred(nb_pred, 3), "s")
             ret = acc_getd(acces, "PM_x", data_pred(nb_pred, 4), "")
             ret = acc_getd(acces, "PM_y", data_pred(nb_pred, 5), "")
             ret = acc_getd(acces, "PM_x_err", data_pred(nb_pred, 6), "")
             ret = acc_getd(acces, "PM_y_err", data_pred(nb_pred, 7), "")
             ret = acc_getd(acces, "dpsi", data_pred(nb_pred, 8), "")
             ret = acc_getd(acces, "deps", data_pred(nb_pred, 9), "")
             ret = acc_getd(acces, "dpsi_err", data_pred(nb_pred, 10), "")
             ret = acc_getd(acces, "deps_err", data_pred(nb_pred, 11), "")
             ret = acc_getd(acces, "dX", data_pred(nb_pred, 12), "")
             ret = acc_getd(acces, "dY", data_pred(nb_pred, 13), "")
             ret = acc_getd(acces, "dX_err", data_pred(nb_pred, 14), "")
             ret = acc_getd(acces, "dY_err", data_pred(nb_pred, 15), "")

             ret = acc_select_end(acces)
          end if
       end do
    end if


    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end subroutine cps_getBullADernieresPred

  
  function cps_getBullAPred(date, data_pred, nb_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullAPred
!
!$Resume
!
!$Description
! V2.0
! Obtenir les predictions effectuees a une date donnee.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getBullAPred(date, data_pred, nb_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(120,15) :: data_pred
!.    integer :: nb_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>                date à laquelle ont été effectuées les prédictions
!>S     data_pred  :<PM_REEL,DIM=(120,15)>   tableau des prédictions
!>S     nb_pred    :<integer>                taille du tableau
!>S     trouve     :<integer>                CPS_OK si les prédictions sont trouvées, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(in) :: date
    real(KIND=PM_REEL), dimension(120,15), intent(out) :: data_pred
    integer, intent(out) :: nb_pred
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    character(LEN=256) :: fichier, libelle
    integer :: nature, acces

    ! initialisation
    trouve = CPS_ERR_DEF
    nb_pred = 0
    
    ! obtention d'un acces sur le fichier contenant les données de la date
    trouve = cps_getFichierBullAPred(date, acces, fichier)

    ! lecture des predictions
    if (trouve.eq.CPS_OK) then
       call cpsi_getAccesMadona(trim(fichier), acces)
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             exit
          elseif (nature.eq.ACC_STRUCT) then
             nb_pred = nb_pred+1
             ret = acc_select(acces, trim(libelle), ACC_STRUCT)
             
             ret = acc_getd(acces, "date", data_pred(nb_pred, 1), "")
             ret = acc_getd(acces, "UTC_UT1", data_pred(nb_pred, 2), "s")
             ret = acc_getd(acces, "UTC_UT1_err", data_pred(nb_pred, 3), "s")
             ret = acc_getd(acces, "PM_x", data_pred(nb_pred, 4), "")
             ret = acc_getd(acces, "PM_y", data_pred(nb_pred, 5), "")
             ret = acc_getd(acces, "PM_x_err", data_pred(nb_pred, 6), "")
             ret = acc_getd(acces, "PM_y_err", data_pred(nb_pred, 7), "")
             ret = acc_getd(acces, "dpsi", data_pred(nb_pred, 8), "")
             ret = acc_getd(acces, "deps", data_pred(nb_pred, 9), "")
             ret = acc_getd(acces, "dpsi_err", data_pred(nb_pred, 10), "")
             ret = acc_getd(acces, "deps_err", data_pred(nb_pred, 11), "")
             ret = acc_getd(acces, "dX", data_pred(nb_pred, 12), "")
             ret = acc_getd(acces, "dY", data_pred(nb_pred, 13), "")
             ret = acc_getd(acces, "dX_err", data_pred(nb_pred, 14), "")
             ret = acc_getd(acces, "dY_err", data_pred(nb_pred, 15), "")

             ret = acc_select_end(acces)
          end if
       end do
    end if
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    
  end function cps_getBullAPred

  
  function cps_getBullADatePred(date, date_pred, data_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullADatePred
!
!$Resume
!
!$Description
! V2.0
! Obtenir la prediction pour la date 'date' effectuee a la date
! 'date_pred'.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getBullADatePred(date, date_pred, data_pred)
!.    real(KIND=PM_REEL) :: date, date_pred
!.    real(KIND=PM_REEL), dimension(15) :: data_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            date des données correspondant à la prédiction
!>E     date_pred  :<PM_REEL>            date à laquelle a été faite la prédiction
!>S     data_pred  :<PM_REEL,DIM=(15)>   données prédites
!>S     trouve     :<integer>            CPS_OK si les prédictions ont été trouvées, CPS_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date, date_pred
    real(KIND=PM_REEL), dimension(15), intent(out) :: data_pred
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    real(KIND=PM_REEL), dimension(120, 15) :: data_all
    integer :: nb_pred, i, res
    
    nb_pred = 0
    trouve = cps_getBullAPred(date_pred, data_all, nb_pred)
    
    if (trouve.eq.CPS_OK) then
       trouve = CPS_ERR_DEF
       do i=1, nb_pred
          res = cpsi_compareReels(date, data_all(i,1))
          if (res.eq.0) then
             trouve = CPS_OK
             data_pred(:) = data_all(i,:)
             exit
          end if
       end do
    end if
    
    
  end function cps_getBullADatePred

  
  function cps_getBullADernierePred(date, date_pred, data_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getBullADernierePred
!
!$Resume
!
!$Description
! V2.0
! Obtenir la derniere prediction pour une date.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getBullADernierePred(date, date_pred, data_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(15) :: data_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            date des données recherchées
!>S     date_pred  :<PM_REEL>            date à laquelle a été faite la prédiction
!>S     data_pred  :<PM_REEL,DIM=(15)>   données recherchées
!>S     trouve     :<integer>            CPS_OK si les données sont prédites, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- md_jourfrac_joursec
!- md_julien_calend
!- cpsi_lireBullAPred
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
    real(KIND=PM_REEL), intent(in) :: date
    real(KIND=PM_REEL), intent(out) :: date_pred
    real(KIND=PM_REEL), dimension(15), intent(out) :: data_pred
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: nb_fic, i, k, acces_fic, acces
    character(LEN=256) :: libelle, fichier
    integer :: nature, res_min, res_max
    real(KIND=PM_REEL) :: pred_min, pred_max, date1950, sec, date_entree
    integer :: jj, mm, aaaa, h, min
    character(LEN=265) :: buff_aaaa, buff_mm, buff_jj, cle
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour

    ! initialisation
    trouve = CPS_ERR_DEF
    

    call cps_getListFichiersCateg("bulletinA_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)

    do i=1, nb_fic
       k = nb_fic-i+1
       call cpsi_getAccesMadona(trim(fichiers(k)), acces_fic)
       ret = acc_scan_reset(acces_fic)
       do
         ret = acc_scan(acces_fic, libelle, nature)
         if (nature.eq.0) then
            exit
         elseif (nature.eq.ACC_STRUCT) then
            ret = acc_getd(acces_fic, trim(libelle)//".pred_min", pred_min, "")
            ret = acc_getd(acces_fic, trim(libelle)//".pred_max", pred_max, "")
            res_min = cpsi_compareReels(date, pred_min)
            res_max = cpsi_compareReels(date, pred_max)
            if ((res_min.ge.0).and.(res_max.le.0)) then
               ! 'date' appartient a l'intervalle [pred_min ; pred_max]
               ret = acc_getd(acces_fic, trim(libelle)//".date", date_entree, "")
               ret = acc_gets(acces_fic, trim(libelle)//".fichier", fichier)
               fichier = trim(rep_base_ref)//"/"//trim(fichier)
               trouve = CPS_OK
            end if
         end if
       end do
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do

    if (trouve.eq.CPS_OK) then
       ! extraction des donnees
       call cpsi_getAccesMadona(trim(fichier), acces)
       date1950 = date_entree-33282.00
       call md_jourfrac_joursec(date1950, jour_sec, code_retour)
       call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
       write(buff_jj,10) jj
       if (buff_jj(1:1).eq.' ') then
          buff_jj(1:1) = '0'
       end if
       write(buff_mm,10) mm
       if (buff_mm(1:1).eq.' ') then
          buff_mm(1:1) = '0'
       end if
       write(buff_aaaa,20) aaaa
       cle = cpsi_getCleBullA(date)
       cle = trim(cle)//"_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
       ret = acc_select(acces, trim(cle), ACC_STRUCT)
       
       call cpsi_lireBullAPred(acces, data_pred, date_pred)
       
       ret = acc_select_end(acces)
    end if
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

10  format (i2)
20  format (i4)

  end function cps_getBullADernierePred


  ! routine interne pour cps_getBullADernierePred()
  ! cette routine a pour but de diminuer le nombre d'instructions
  ! de cps_getBullADernierePred() (cf. seuils des métriques)
  subroutine cpsi_lireBullAPred(acces, data_pred, date_pred)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lireBullAPred
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
!  call cpsi_lireBullAPred(acces, data_pred, date_pred)
!.    integer :: acces
!.    real(KIND=PM_REEL), dimension(15) :: data_pred
!.    real(KIND=PM_REEL) :: date_pred
!
!$Arguments
!>E     acces      :<integer>            
!>S     data_pred  :<PM_REEL,DIM=(15)>   
!>S     date_pred  :<PM_REEL>            
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
    real(KIND=PM_REEL), dimension(15), intent(out) :: data_pred
    real(KIND=PM_REEL), intent(out) :: date_pred
    
    ret = acc_getd(acces, "date", data_pred(1), "")
    ret = acc_getd(acces, "UTC_UT1", data_pred(2), "s")
    ret = acc_getd(acces, "UTC_UT1_err", data_pred(3), "s")
    ret = acc_getd(acces, "PM_x", data_pred(4), "")
    ret = acc_getd(acces, "PM_y", data_pred(5), "")
    ret = acc_getd(acces, "PM_x_err", data_pred(6), "")
    ret = acc_getd(acces, "PM_y_err", data_pred(7), "")
    ret = acc_getd(acces, "dpsi", data_pred(8), "")
    ret = acc_getd(acces, "deps", data_pred(9), "")
    ret = acc_getd(acces, "dpsi_err", data_pred(10), "")
    ret = acc_getd(acces, "deps_err", data_pred(11), "")
    ret = acc_getd(acces, "dX", data_pred(12), "")
    ret = acc_getd(acces, "dY", data_pred(13), "")
    ret = acc_getd(acces, "dX_err", data_pred(14), "")
    ret = acc_getd(acces, "dY_err", data_pred(15), "")
    
    ret = acc_getd(acces, "date_debut", date_pred, "")
    
  end subroutine cpsi_lireBullAPred

  
  function cps_getFichierAcsol2Sure(date, acces, fichier) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierAcsol2Sure
!
!$Resume
!
!$Description
! V2.0
! Obtention du fichier et d'un acces Madona sur un fichier de donnees
! observees pour une date issues du fichier ACSOL2.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierAcsol2Sure(date, acces, fichier)
!.    real(KIND=PM_REEL) :: date
!.    integer :: acces
!.    character(LEN=*) :: fichier
!.    integer :: trouve
!
!$Arguments
!>E     date     :<PM_REEL>   date de données
!>S     acces    :<integer>   accès MADONA sur le fichier de données
!>S     fichier  :<LEN=*>     nom du fichier de données
!>S     trouve   :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(in) :: date
    integer, intent(out) :: acces
    character(LEN=*), intent(out) :: fichier
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: nb_fic, acces_fic, i
    character(LEN=256) :: cle_fic
    
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: sec
    character(LEN=4) :: buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    logical :: exist

    ! initialisation
    trouve = CPS_ERR_DEF
    
    call md_jourfrac_joursec(date, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    write(buff_mm,10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20) aaaa
    
    cle_fic = "mois_"//trim(buff_mm)//"_annee_"//trim(buff_aaaa)

    ! Obtenir le fichier de donnees COMPAS correpondant au mois
    ! et a l'annee de 'date'
    ! Liste des fichiers 
    call cps_getListFichiersCateg("acsol2_sure", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    ! Recherche du fichier
    do i=1, nb_fic
       call cpsi_getAccesMadona(trim(fichiers(i)), acces_fic)
       ret = acc_exist(acces_fic, trim(cle_fic)//".fichier")
       if (ret.eq.1) then
          ! trouve
          ret = acc_gets(acces_fic, trim(cle_fic)//".fichier", fichier)
          exist = .false.
          ! on regarde d'abord si le fichier est present dans la base locale
          inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=exist)
          if (exist) then
             ! le fichier existe dans la base locale : on forme le chemin complet
             fichier = trim(rep_base_local)//"/"//trim(fichier)
          else
             ! le fichier est present dans la base de reference : on forme le chemin complet
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
          end if
          ! acces Madona
          call cpsi_getAccesMadona(trim(fichier), acces)
          trouve = CPS_OK
          exit
       end if
    end do
    
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if


10  format(i2)
20  format(i4)

  end function cps_getFichierAcsol2Sure

  subroutine cps_extraireAcsol2Sure(tab_date,      &
       tab_data_flux, tab_data_ia, nb_data, date_deb, date_fin) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_extraireAcsol2Sure
!
!$Resume
!  Extraction des données d'activité solaire réelles observées contenues
!  dans la base
!
!$Description
!  Extraction des données d'activité solaire réelles observées contenues
!  dans la base
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_extraireAcsol2Sure(tab_date,      &
!.           tab_data_flux, tab_data_ia, nb_data, [date_deb], [date_fin]) 
!.    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
!.    integer, dimension(:,:), pointer :: tab_data_ia
!.    integer :: nb_data
!.    real(KIND=PM_REEL) :: date_deb
!.    real(KIND=PM_REEL) :: date_fin
!
!$Arguments
!>E/S   tab_date       :<PM_REEL,DIM=(:),pointer>     tableau des dates (jj1950 frac.)
!>E/S   tab_data_flux  :<PM_REEL,DIM=(:,:),pointer>   tableau des flux
!>E/S   tab_data_ia    :<integer,DIM=(:,:),pointer>   tableau des indices tri-horaires 
!>S     nb_data        :<integer>                     nombre de lignes des tableaux
!>[E]   date_deb       :<PM_REEL>                     éventuelle date de début (jj1950 frac.)
!>[E]   date_fin       :<PM_REEL>                     éventuelle date de fin (jj1950 frac.)
!
!$Common
!
!$Routines
!- cpsi_extraireDonneesAcsol2
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
!- cps_desc
!#
!
!$Remarques
!   Dans le cas où ni la date de début ni la date de fin est renseignée, l'ensemble 
!   des données sont extraites. 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona
   use cps_desc, only : cpsi_size_ptr

   implicit none

   ! arguments
   real(KIND=PM_REEL), dimension(:), pointer :: tab_date
   real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
   integer, dimension(:,:), pointer :: tab_data_ia

   integer, intent(out) :: nb_data
   real(KIND=PM_REEL), intent(in), optional :: date_deb
   real(KIND=PM_REEL), intent(in), optional :: date_fin

   ! Constantes
   real(KIND=PM_REEL), parameter :: DATE_DEB_DEFAUT = -1._pm_reel
   real(KIND=PM_REEL), parameter :: DATE_FIN_DEFAUT = 1e8_pm_reel

   ! Variables locales
   real(KIND=PM_REEL) :: date_deb_tmp,  date_fin_tmp

   ! Init des variables de parcours
   ret = ACC_OK
   if ( present(date_deb) ) then
      date_deb_tmp = date_deb
   else
      ! Attribut d'une date de début suffisamment petite
      ! pour que l'ensemble des données soient extraites.
      date_deb_tmp = DATE_DEB_DEFAUT
   endif

   if ( present(date_fin) ) then
      date_fin_tmp = date_fin
   else
      ! Attribut d'une date de fin suffisamment grande
      ! pour que l'ensemble des données soient extraites.
      date_fin_tmp = DATE_FIN_DEFAUT
   endif

   ! Appel à la routine de lecture
   call cpsi_extraireDonneesAcsol2("acsol2_sure", tab_date,      &
       tab_data_flux, tab_data_ia, nb_data, date_deb_tmp, date_fin_tmp)
   if ( MSP_gen_messages("cps_extraireAcsol2Sure") ) return

  end subroutine cps_extraireAcsol2Sure

  subroutine cps_extraireAcsol2Pred(tab_date,      &
       tab_data_flux, tab_data_ia, nb_data, date_deb, date_fin) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_extraireAcsol2Pred
!
!$Resume
!  Extraction des données d'activité solaire prédites contenues
!  dans la base
!
!$Description
!  Extraction des données d'activité solaire prédites contenues
!  dans la base
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_extraireAcsol2Pred(tab_date,      &
!.           tab_data_flux, tab_data_ia, nb_data, [date_deb], [date_fin]) 
!.    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
!.    integer, dimension(:,:), pointer :: tab_data_ia
!.    integer :: nb_data
!.    real(KIND=PM_REEL) :: date_deb
!.    real(KIND=PM_REEL) :: date_fin
!
!$Arguments
!>E/S   tab_date       :<PM_REEL,DIM=(:),pointer>     tableau des dates (jj1950 frac.)
!>E/S   tab_data_flux  :<PM_REEL,DIM=(:,:),pointer>   tableau des flux
!>E/S   tab_data_ia    :<integer,DIM=(:,:),pointer>   tableau des indices tri-horaires 
!>S     nb_data        :<integer>                     nombre de lignes des tableaux
!>[E]   date_deb       :<PM_REEL>                     éventuelle date de début (jj1950 frac.)
!>[E]   date_fin       :<PM_REEL>                     éventuelle date de fin (jj1950 frac.)       
!
!$Common
!
!$Routines
!- cpsi_extraireDonneesAcsol2
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
!- cps_desc
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona
   use cps_desc, only : cpsi_size_ptr

   implicit none

   ! arguments
   real(KIND=PM_REEL), dimension(:), pointer :: tab_date
   real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
   integer, dimension(:,:), pointer :: tab_data_ia

   integer, intent(out) :: nb_data
   real(KIND=PM_REEL), intent(in), optional :: date_deb
   real(KIND=PM_REEL), intent(in), optional :: date_fin

   ! Constantes
   real(KIND=PM_REEL), parameter :: DATE_DEB_DEFAUT = -1._pm_reel
   real(KIND=PM_REEL), parameter :: DATE_FIN_DEFAUT = 1e8_pm_reel

   ! Variables locales
   real(KIND=PM_REEL) :: date_deb_tmp,  date_fin_tmp

   ! Init des variables de parcours
   ret = ACC_OK
   if ( present(date_deb) ) then
      date_deb_tmp = date_deb
   else
      date_deb_tmp = DATE_DEB_DEFAUT
   endif

   if ( present(date_fin) ) then
      date_fin_tmp = date_fin
   else
      date_fin_tmp = DATE_FIN_DEFAUT
   endif

   call cpsi_extraireDonneesAcsol2("acsol2_pred", tab_date,      &
       tab_data_flux, tab_data_ia, nb_data, date_deb_tmp, date_fin_tmp)
   if ( MSP_gen_messages("cps_extraireAcsol2Sure") ) return

  end subroutine cps_extraireAcsol2Pred

  subroutine cpsi_extraireDonneesAcsol2(categ, tab_date,      &
       tab_data_flux, tab_data_ia, nb_data, date_deb, date_fin) 
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_extraireDonneesAcsol2
!
!$Resume
!   Lecture de la première table de la base regrouppant l'ensemble des
!   fichiers MADONA puis lecture de chacun de ces fichiers
!
!$Description
!   Lecture de la première table de la base regrouppant l'ensemble des
!   fichiers MADONA puis lecture de chacun de ces fichiers
!
!$Auteur
!   Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call cpsi_extraireDonneesAcsol2(categ, tab_date,      &
!.           tab_data_flux, tab_data_ia, nb_data, date_deb, date_fin) 
!.    character (len=*) :: categ
!.    real(KIND=PM_REEL), dimension(:), pointer :: tab_date
!.    real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
!.    integer, dimension(:,:), pointer :: tab_data_ia
!.    integer :: nb_data
!.    real(KIND=PM_REEL) :: date_deb
!.    real(KIND=PM_REEL) :: date_fin
!
!$Arguments
!>E/S   categ          :<LEN=*>                       catégorie parmi "acsol2_sure" et "acsol2_pred"
!>E/S   tab_date       :<PM_REEL,DIM=(:),pointer>     tableau des dates
!>E/S   tab_data_flux  :<PM_REEL,DIM=(:,:),pointer>   tableau des données de flux
!>E/S   tab_data_ia    :<integer,DIM=(:,:),pointer>   tableau des données liées aux indices
!>S     nb_data        :<integer>                     nombre déléments des tableaux
!>E     date_deb       :<PM_REEL>                     date de début du créneau (jj1950 frac.) 
!>E     date_fin       :<PM_REEL>                     date de fin du créneau (jj1950 frac.) 
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cpsi_cpter_lignes_Acsol2_MADONA
!- cpsi_lire_unficAcsol2_MADONA
!
!$Include
!
!$Module
!#V
!- cps_accesMadona
!- cps_desc
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_accesMadona
   use cps_desc, only : cpsi_size_ptr

   implicit none

   ! arguments
   character (len=*) :: categ
   real(KIND=PM_REEL), dimension(:), pointer :: tab_date
   real(KIND=PM_REEL), dimension(:,:), pointer :: tab_data_flux
   integer, dimension(:,:), pointer :: tab_data_ia

   integer, intent(out) :: nb_data
   real(KIND=PM_REEL), intent(in) :: date_deb
   real(KIND=PM_REEL), intent(in) :: date_fin

   ! Varaibles locales
   integer :: ret, ret_scan
   character(LEN=CPS_MAXLG) :: libelle
   integer :: nature
   character(LEN=256), dimension(:), pointer :: fichiers => NULL()
   integer :: nb_fic, acces_fic, ii, compteur, nb_donnees
   character(LEN=256) :: fichier
   logical :: existe

   ! Init des données en sorties
   nb_data = 0
   if ( associated(tab_date) ) deallocate(tab_date)
   if ( associated(tab_data_flux) ) deallocate(tab_data_flux)
   if ( associated(tab_data_ia) ) deallocate(tab_data_ia)

   ! Obtenir l'ensemble des fichiers des données sûres
   call cps_getListFichiersCateg(categ, fichiers)
   if ( MSP_gen_messages("cpsi_extraireDonneesAcsol2") ) return 

   nb_fic = cpsi_size_ptr(fichiers)
   nb_donnees = 0
   ! -----------------------------------------------------------
   ! Premier parcours pour compter le nombre de données disponibles
   ! par date. Ce premier passage permet de dimensionner les tableaux.
   ! -----------------------------------------------------------
   do ii=1, nb_fic
     call cpsi_getAccesMadona(trim(fichiers(ii)), acces_fic)
     ret_scan = acc_scan_reset(acces_fic)

     boucle_scan : do while (ret_scan /= ACC_EOF) 
       
       ! Lecture du contenu 
       ret_scan = acc_scan(acces_fic, libelle, nature)
       ! On ne veut des strcutures
       if (ret_scan /=0 .or. nature /= ACC_STRUCT ) cycle boucle_scan

       ! la structure existe : selection de la structure et extraction du fichier
       ret = acc_select(acces_fic, libelle, ACC_STRUCT)
       ret = acc_gets(acces_fic, "fichier", fichier)
       ret = acc_select_end(acces_fic)
       existe = .false.
       ! on regarde d'abord si le fichier est present dans la base locale
       inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=existe)
       if (existe) then
          ! le fichier existe dans la base locale : on forme le chemin complet
          fichier = trim(rep_base_local)//"/"//trim(fichier)
       else
          ! le fichier est present dans la base de reference : on forme le chemin complet
          fichier = trim(rep_base_ref)//"/"//trim(fichier)
       end if

       ! Appel à la routine qui compte les lignes correspondant au critère
       ! et met à jour le compteur
       call cpsi_cpter_lignes_Acsol2_MADONA(trim(fichier), nb_donnees, &
           date_deb, date_fin) 

     enddo boucle_scan
   enddo


   ! Allocation
   allocate (tab_date(nb_donnees))
   allocate (tab_data_flux(nb_donnees, 2))
   allocate (tab_data_ia(nb_donnees, 10))

   ! Lecture successive des fichiers
   compteur = 1
   ! -----------------------------------------------------------
   ! Second parcours qui, ctte fois-ci, stocke les données lues
   ! -----------------------------------------------------------
   do ii=1, nb_fic
     call cpsi_getAccesMadona(trim(fichiers(ii)), acces_fic)
     ret_scan = acc_scan_reset(acces_fic)

     boucle_scan2 : do while (ret_scan /= ACC_EOF) 
       
       ! Lecture du contenu 
       ret_scan = acc_scan(acces_fic, libelle, nature)
       ! On ne veut des strcutures
       if (ret_scan /=0 .or. nature /= ACC_STRUCT ) cycle boucle_scan2

       ! la structure existe : selection de la structure et extraction du fichier
       ret = acc_select(acces_fic, libelle, ACC_STRUCT)
       ret = acc_gets(acces_fic, "fichier", fichier)
       ret = acc_select_end(acces_fic)
       existe = .false.
       ! on regarde d'abord si le fichier est present dans la base locale
       inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=existe)
       if (existe) then
          ! le fichier existe dans la base locale : on forme le chemin complet
          fichier = trim(rep_base_local)//"/"//trim(fichier)
       else
          ! le fichier est present dans la base de reference : on forme le chemin complet
          fichier = trim(rep_base_ref)//"/"//trim(fichier)
       end if

       ! acces Madona au fichier
       call cpsi_lire_unficAcsol2_MADONA(trim(fichier), categ, compteur,nb_donnees, &
           tab_date, tab_data_flux, tab_data_ia, date_deb, date_fin) 

     enddo boucle_scan2
   enddo

   ! Copie du nombre de données
   nb_data = nb_donnees

end subroutine cpsi_extraireDonneesAcsol2

  function cps_getFichierAcsol2Pred(date, acces, fichier) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierAcsol2Pred
!
!$Resume
!
!$Description
! V2.0
! Obtention du fichier et d'un acces Madona sur un fichier de donnees
! predites pour une date issues du fichier ACSOL2.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierAcsol2Pred(date, acces, fichier)
!.    real(KIND=PM_REEL) :: date
!.    integer :: acces
!.    character(LEN=*) :: fichier
!.    integer :: trouve
!
!$Arguments
!>E     date     :<PM_REEL>   date des données recherchées
!>S     acces    :<integer>   accès MADONA ouvert sur le fichier de données
!>S     fichier  :<LEN=*>     nom du fichier de données
!>S     trouve   :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(in) :: date
    integer, intent(out) :: acces
    character(LEN=*), intent(out) :: fichier
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    integer :: nb_fic, acces_fic, i
    character(LEN=256) :: cle_fic
    
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_rEEL) :: sec
    character(LEN=4) :: buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    logical :: exist

    ! initialisation
    trouve = CPS_ERR_DEF
    
    call md_jourfrac_joursec(date, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    write(buff_mm,10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20) aaaa
    
    cle_fic = "mois_"//trim(buff_mm)//"_annee_"//trim(buff_aaaa)

    ! Obtenir le fichier de donnees COMPAS correpondant au mois
    ! et a l'annee de 'date'
    ! Liste des fichiers 
    call cps_getListFichiersCateg("acsol2_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    ! Recherche du fichier
    do i=1, nb_fic
       call cpsi_getAccesMadona(trim(fichiers(i)), acces_fic)
       ret = acc_exist(acces_fic, trim(cle_fic))
       if (ret.eq.1) then
          ! trouve
          ret = acc_gets(acces_fic, trim(cle_fic)//".fichier", fichier)
          exist = .false.
          ! on regarde d'abord si le fichier est present dans la base locale
          inquire(file=trim(rep_base_local)//"/"//trim(fichier), exist=exist)
          if (exist) then
             ! le fichier existe dans la base locale : on forme le chemin complet
             fichier = trim(rep_base_local)//"/"//trim(fichier)
          else
             ! le fichier est present dans la base de reference : on forme le chemin complet
             fichier = trim(rep_base_ref)//"/"//trim(fichier)
          end if
          ! acces Madona
          call cpsi_getAccesMadona(trim(fichier), acces)
          trouve = CPS_OK
          exit
       end if
    end do
    
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if


10  format(i2)
20  format(i4)

  end function cps_getFichierAcsol2Pred

  
  function cps_getAcsol2Sure(date, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2Sure
!
!$Resume
!
!$Description
! V2.0
! Obtenir les donnees observees issues du fichier ACSOL2 concernant
! une date donnee.
! data_flux(1) : flux observe
! data_flux(2) : flux moyen observe
! data_ia(1)   : indice AA moyen journalier
! data_ia(2:9) : indice AA trihoraire
! data_ia(10)  : indice AP moyen journalier
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAcsol2Sure(date, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date
!.    real(kind=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(10) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>            date des données recherchées
!>S     data_flux  :<PM_REEL,DIM=(2)>    données liées aux flux
!>S     data_ia    :<integer,DIM=(10)>   données liés aux indices
!>S     trouve     :<integer>            CPS_OK si les données sont trouvées, CPS_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date
    real(kind=PM_REEL), dimension(2), intent(out) :: data_flux
    integer, dimension(10), intent(out) :: data_ia

    ! resultat
    integer :: trouve
    
    ! variables locales
    integer :: acces
    character(LEN=256) :: fichier, cle

    ! initialisation
    trouve = CPS_ERR_DEF
    cle = cpsi_getCleAcsol2(date)

    ! obtenir le fichier
    trouve = cps_getFichierAcsol2Sure(date, acces, fichier)
    
    if (trouve.eq.CPS_OK) then
       ! le fichier est trouve
       trouve = CPS_ERR_DEF
       ret = acc_exist(acces, trim(cle))
       if (ret.eq.1) then
          ! la structure existe
          ! selection de la structure
          ret = acc_select(acces, trim(cle), ACC_STRUCT)
          
          ret = acc_getd(acces, "flux", data_flux(1), "sfu")
          ret = acc_getd(acces, "fluxm", data_flux(2), "sfu")

          ret = acc_geti(acces, "iaamoy", data_ia(1))
          ret = acc_geti(acces, "iaa_1", data_ia(2))
          ret = acc_geti(acces, "iaa_2", data_ia(3))
          ret = acc_geti(acces, "iaa_3", data_ia(4))
          ret = acc_geti(acces, "iaa_4", data_ia(5))
          ret = acc_geti(acces, "iaa_5", data_ia(6))
          ret = acc_geti(acces, "iaa_6", data_ia(7))
          ret = acc_geti(acces, "iaa_7", data_ia(8))
          ret = acc_geti(acces, "iaa_8", data_ia(9))
          ret = acc_geti(acces, "iapmoy", data_ia(10))

          ! fin de la selection
          ret = acc_select_end(acces)

          trouve = CPS_OK
       end if
    end if
    
  end function cps_getAcsol2Sure
  


  function cps_getAcsol2SureCreneau(date_deb, date_fin, tab_date,      &
       tab_data_flux, tab_data_ia, nb_data) result(ok)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2SureCreneau
!
!$Resume
!
!$Description
! V2.0
! Obtenir les donnees sures du fichier ACSOL2 sur un creneau de
! dates.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = cps_getAcsol2SureCreneau(date_deb, date_fin, tab_date,      &
!.           tab_data_flux, tab_data_ia, nb_data)
!.    real(KIND=PM_REEL) :: date_deb, date_fin
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE) :: tab_date
!.    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE,2) :: tab_data_flux
!.    integer, dimension(CPS_MAX_ACSOL2_SURE,10) :: tab_data_ia
!.    integer :: nb_data
!.    integer :: ok
!
!$Arguments
!>E     date_deb       :<PM_REEL>                                date de début du créneau
!>E     date_fin       :<PM_REEL>                                date de fin du créneau
!>S     tab_date       :<PM_REEL,DIM=(CPS_MAX_ACSOL2_SURE)>      tableau des dates
!>S     tab_data_flux  :<PM_REEL,DIM=(CPS_MAX_ACSOL2_SURE,2)>    tableau des données de flux
!>S     tab_data_ia    :<integer,DIM=(CPS_MAX_ACSOL2_SURE,10)>   tableau des données liées aux indices
!>S     nb_data        :<integer>                                nombre déléments des tableaux
!>S     ok             :<integer>                                CPS_OK si les données sont trouvées, CPS_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date_deb, date_fin
    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE), intent(out) :: tab_date
    real(KIND=PM_REEL), dimension(CPS_MAX_ACSOL2_SURE,2), intent(out) :: tab_data_flux
    integer, dimension(CPS_MAX_ACSOL2_SURE,10), intent(out) :: tab_data_ia
    integer, intent(out) :: nb_data

    ! resultat
    integer :: ok
    
    ! variables locales
    real(KIND=PM_REEL) :: date
    integer :: trouve, nb, i
    real(KIND=PM_REEL), dimension(2) :: tmp_flux
    integer, dimension(10) :: tmp_ia
    
    ! initialisation
    ok = CPS_OK
    nb_data = 0
    nb = int(date_fin)-int(date_deb)+1
    date = date_deb

    if (nb.lt.1) then
       ! erreur : 'date_deb' doit etre inferieure a 'date_fin'
       ok = CPS_ERR_DEF
       return
    end if

    ! boucle sur les dates
    do i=1, nb
       trouve = cps_getAcsol2Sure(date, tmp_flux, tmp_ia)
       if (trouve.eq.CPS_OK) then
          ! les donnees ont ete trouvees pour la date courante
          nb_data = nb_data+1
          tab_date(nb_data) = date
          tab_data_flux(nb_data,1:2) = tmp_flux(1:2)
          tab_data_ia(nb_data, 1:10) = tmp_ia(1:10)
       end if
       date = date+1._pm_reel
    end do
    
  end function cps_getAcsol2SureCreneau


  subroutine cps_getAcsol2DernieresPred(date_pred, dates, data_flux, data_ia, nb_pred)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2DernieresPred
!
!$Resume
!
!$Description
! V2.0
! Obtenir les dernieres predictions issues du fichier ACSOL2.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getAcsol2DernieresPred(date_pred, dates, data_flux, data_ia, nb_pred)
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(50) :: dates
!.    real(KIND=PM_REEL), dimension(50,2) :: data_flux
!.    integer, dimension(50,9) :: data_ia
!.    integer :: nb_pred
!
!$Arguments
!>S     date_pred  :<PM_REEL>              date à laquelle ont été faites les dernières prédictions
!>S     dates      :<PM_REEL,DIM=(50)>     tableau des dates
!>S     data_flux  :<PM_REEL,DIM=(50,2)>   tableau des données de flux
!>S     data_ia    :<integer,DIM=(50,9)>   tableau des données liées aux indices
!>S     nb_pred    :<integer>              nombre d'éléments des tableaux
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cpsi_lireAcsol2Pred
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
    real(KIND=PM_REEL), intent(out) :: date_pred
    real(KIND=PM_REEL), dimension(50), intent(out) :: dates
    real(KIND=PM_REEL), dimension(50,2), intent(out) :: data_flux
    integer, dimension(50,9),  intent(out) :: data_ia
    integer, intent(out) :: nb_pred

    ! variables locales
    integer :: nb_fic, i, acces_fic, acces, nature, res, ind_debut
    character(LEN=256) :: libelle
    character(LEN=256), dimension(2) :: fichiers_pred
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    logical :: trouve
    real(KIND=PM_REEL) :: date_tmp

    ! initialisations
    trouve = .false.
    nb_pred = 0
    date_pred = 0.0
    date_tmp = 0.0
    fichiers_pred(1) = ""
    fichiers_pred(2) = ""

    ! obtenir les deux derniers fichiers de predictionz
    call cps_getListFichiersCateg("acsol2_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    call cpsi_getAccesMadona(trim(fichiers(nb_fic)), acces_fic)
    ret = acc_scan_reset(acces_fic)
    do
       ret = acc_scan(acces_fic, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          fichiers_pred(1) = fichiers_pred(2)
          ret = acc_gets(acces_fic, trim(libelle)//".fichier", fichiers_pred(2))
          fichiers_pred(2) = trim(rep_base_ref)//"/"//trim(fichiers_pred(2))
          trouve = .true.
       end if
    end do
    
    ! recherche de la date la plus recente a laquelle des predictions
    ! ont ete faites
    if (trouve) then
       call cpsi_getAccesMadona(trim(fichiers_pred(2)), acces)
       ret = acc_scan_reset(acces)
       do
          ret = acc_scan(acces, libelle, nature)
          if (nature.eq.0) then
             ! fin du fichier
             exit
          else if (nature.eq.ACC_STRUCT) then
             ret = acc_getd(acces, trim(libelle)//".date_pred", date_tmp, "")
             res = cpsi_compareReels(date_tmp, date_pred)
             if (res.eq.1) then
                date_pred = date_tmp
             end if
          end if
       end do
    end if

    ! on recherche les predictions effectuees a la date 'derniere_date'
    ! dans les deux derniers fichiers de predictions
    if (fichiers_pred(1).ne."") then
       ind_debut = 1
    else
       ind_debut = 2
    end if
    
    do i=ind_debut,2
       call cpsi_getAccesMadona(trim(fichiers_pred(i)), acces)
       ! Extraction de toutes les predictions effectuees a la date 'derniere_date'
       ret = acc_scan_reset(acces)
       call cpsi_lireAcsol2Pred(acces, date_pred, nb_pred, dates, &
            data_flux, data_ia)
    end do
    


    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    
  end subroutine cps_getAcsol2DernieresPred


  ! routine interne pour cps_getAcsol2DernieresPred()
  ! cette routine a pour but de diminuer le nombre d'instructions de
  ! la routine cps_getAcsol2DernieresPred() (cf. seuil des métriques)
  subroutine cpsi_lireAcsol2Pred(acces, date_pred, nb_pred, dates, &
       data_flux, data_ia)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lireAcsol2Pred
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
!  call cpsi_lireAcsol2Pred(acces, date_pred, nb_pred, dates, &
!.           data_flux, data_ia)
!.    integer :: acces
!.    real(KIND=PM_REEL) :: date_pred
!.    integer :: nb_pred
!.    real(KIND=PM_REEL), dimension(50) :: dates
!.    real(KIND=PM_REEL), dimension(50,2) :: data_flux
!.    integer, dimension(50,9) :: data_ia
!
!$Arguments
!>E     acces      :<integer>              
!>E     date_pred  :<PM_REEL>              
!>E/S   nb_pred    :<integer>              
!>S     dates      :<PM_REEL,DIM=(50)>     
!>S     data_flux  :<PM_REEL,DIM=(50,2)>   
!>S     data_ia    :<integer,DIM=(50,9)>   
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
    real(KIND=PM_REEL), intent(in) :: date_pred
    integer, intent(inout) :: nb_pred
    real(KIND=PM_REEL), dimension(50), intent(out) :: dates
    real(KIND=PM_REEL), dimension(50,2), intent(out) :: data_flux
    integer, dimension(50,9),  intent(out) :: data_ia

    ! variables locales
    character(LEN=256) :: libelle
    integer :: nature, res
    real(KIND=PM_REEL) :: date_tmp

    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          ret = acc_getd(acces, trim(libelle)//".date_pred", date_tmp, "")
          res = cpsi_compareReels(date_tmp, date_pred)
          if (res.eq.0) then
             ! la prediction a ete effectuee a la date 'derniere_date'
             ! on l'extrait
             nb_pred = nb_pred+1
                
             ! selection de la structure
             ret = acc_select(acces, trim(libelle), ACC_STRUCT)
                
             ret = acc_getd(acces, "date", dates(nb_pred), "")
                
             ret = acc_getd(acces, "fluxpr", data_flux(nb_pred,1), "sfu")
             ret = acc_getd(acces, "fluxmp", data_flux(nb_pred,2), "sfu")
                
             ret = acc_geti(acces, "iaampr", data_ia(nb_pred,1))
             ret = acc_geti(acces, "iaapr_1", data_ia(nb_pred,2))
             ret = acc_geti(acces, "iaapr_2", data_ia(nb_pred,3))
             ret = acc_geti(acces, "iaapr_3", data_ia(nb_pred,4))
             ret = acc_geti(acces, "iaapr_4", data_ia(nb_pred,5))
             ret = acc_geti(acces, "iaapr_5", data_ia(nb_pred,6))
             ret = acc_geti(acces, "iaapr_6", data_ia(nb_pred,7))
             ret = acc_geti(acces, "iaapr_7", data_ia(nb_pred,8))
             ret = acc_geti(acces, "iaapr_8", data_ia(nb_pred,9))
                
             ! fin de la selection
             ret = acc_select_end(acces)
          end if
       end if
    end do
    
  end subroutine cpsi_lireAcsol2Pred



  function cps_getAcsol2DatePred(date, date_pred, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2DatePred
!
!$Resume
!
!$Description
! V2.0
! Obtenir les predictions issues du fichier ACSOL2 pour une date
! effectuees a une date donnee.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAcsol2DatePred(date, date_pred, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date, date_pred
!.    real(KIND=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(9) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>           date des donénes recherchées
!>E     date_pred  :<PM_REEL>           date à laquelle a été faite la prédiction
!>S     data_flux  :<PM_REEL,DIM=(2)>   données des flux
!>S     data_ia    :<integer,DIM=(9)>   données liées aux indices
!>S     trouve     :<integer>           CPS_OK si les donées sont trouvées, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
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
    real(KIND=PM_REEL), intent(in) :: date, date_pred
    real(KIND=PM_REEL), dimension(2), intent(out) :: data_flux
    integer, dimension(9), intent(out) :: data_ia

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=256) :: cle, fichier, libelle
    integer :: acces, res, ind, nature
    logical :: trouve_post, trouve_ante
    real(KIND=PM_REEL) :: date_tmp
    
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: sec
    character(LEN=4) :: buff_jj, buff_mm, buff_aaaa
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour

    
    ! initialisation
    trouve = CPS_ERR_DEF
    cle = cpsi_getCleAcsol2(date)
    trouve_ante = .false.
    trouve_post = .false.
    
    call md_jourfrac_joursec(date_pred, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    write(buff_jj,10) jj
    if (buff_jj(1:1).eq.' ') then
       buff_jj(1:1) = '0'
    end if
    write(buff_mm,10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa,20) aaaa

    ! 
    trouve = cps_getFichierAcsol2Pred(date, acces, fichier)

    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          ind = index(trim(libelle), trim(cle))
          if (ind.gt.0) then
             ret = acc_getd(acces, trim(libelle)//".date_pred", date_tmp, "")
             res = cpsi_compareReels(date, date_tmp)
             if (res.eq.0) then
                trouve = CPS_OK
                exit
             elseif(res.eq.1) then
                trouve_ante = .true.
             else
                trouve_post = .true.
             end if
          end if
       end if
    end do
    
    if (trouve_ante.and.trouve_post) then
       trouve = CPS_OK
    end if
    
    if (trouve.eq.CPS_OK) then
       cle = trim(cle)//"_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
       
       ! selection de la structure
       ret = acc_select(acces, trim(cle), ACC_STRUCT)
       
       ret = acc_getd(acces, "fluxpr", data_flux(1), "sfu")
       ret = acc_getd(acces, "fluxmp", data_flux(2), "sfu")
       
       ret = acc_geti(acces, "iaampr", data_ia(1))
       ret = acc_geti(acces, "iaapr_1", data_ia(2))
       ret = acc_geti(acces, "iaapr_2", data_ia(3))
       ret = acc_geti(acces, "iaapr_3", data_ia(4))
       ret = acc_geti(acces, "iaapr_4", data_ia(5))
       ret = acc_geti(acces, "iaapr_5", data_ia(6))
       ret = acc_geti(acces, "iaapr_6", data_ia(7))
       ret = acc_geti(acces, "iaapr_7", data_ia(8))
       ret = acc_geti(acces, "iaapr_8", data_ia(9))
       
       ! fin de la selection
       ret = acc_select_end(acces)
       
    end if
    
10  format (i2)
20  format (i4)
    
  end function cps_getAcsol2DatePred


  function cps_getAcsol2Pred(date, dates, data_flux, data_ia, nb_pred) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2Pred
!
!$Resume
!
!$Description
! V2.0
! Obtention de toutes les predictions effectuees a une date donnee.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAcsol2Pred(date, dates, data_flux, data_ia, nb_pred)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL), dimension(50) :: dates
!.    real(KIND=PM_REEL), dimension(50,2) :: data_flux
!.    integer, dimension(50,9) :: data_ia
!.    integer :: nb_pred
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>              date à laquelle ont été faites les prédictions
!>S     dates      :<PM_REEL,DIM=(50)>     tableau des dates
!>S     data_flux  :<PM_REEL,DIM=(50,2)>   tableau des données de flux
!>S     data_ia    :<integer,DIM=(50,9)>   tableau des données liées aux indices
!>S     nb_pred    :<integer>              nombre d'éléments des tableaux
!>S     trouve     :<integer>              CPS_OK si les données sont trouvées, CPS_ER_DEF sinon
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cpsi_getAcsol2PredFichier
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
    real(KIND=PM_REEL), intent(in) :: date
    real(KIND=PM_REEL), dimension(50), intent(out) :: dates
    real(KIND=PM_REEL), dimension(50,2), intent(out) :: data_flux
    integer, dimension(50,9), intent(out) :: data_ia
    integer, intent(out) :: nb_pred
    
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    integer :: nb_fic, i, acces_fic, nature, res
    real(KIND=PM_REEL) :: date_tmp, zero
    character(LEN=256) :: libelle, fichier
    character(LEN=256), dimension(:), pointer :: fichiers => NULL()
    
    ! initialisation
    trouve = CPS_ERR_DEF
    nb_pred = 0
    date_tmp = date
    zero = 0._pm_reel

    
    ! on ouvre chaque fichier de prediction et on lit chaque prediction
    call cps_getListFichiersCateg("acsol2_pred", fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    ! on decremente la date a chaque boucle
    do
       do i=1, nb_fic
          call cpsi_getAccesMadona(trim(fichiers(i)), acces_fic)
          ret = acc_scan_reset(acces_fic)
          do
             ret = acc_scan(acces_fic, libelle, nature)
             if (nature.eq.0) then
                ! fin du fichier
                exit
             else if (nature.eq.ACC_STRUCT) then
                ret = acc_gets(acces_fic, trim(libelle)//".fichier", fichier)
                fichier = trim(rep_base_ref)//"/"//trim(fichier)
                call cpsi_getAcsol2PredFichier(date_tmp, fichier, dates, data_flux, data_ia, nb_pred)
             end if
          end do
       end do
       if (nb_pred.gt.0) then
          exit
       else
          date_tmp = date_tmp-dble(1)
          res = cpsi_compareReels(date_tmp,zero)
          if (res.eq.-1) then
             exit
          end if
       end if
    end do
    
    if (nb_pred.gt.0) then
       trouve = CPS_OK
    end if
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    
  end function cps_getAcsol2Pred

  
  subroutine cpsi_getAcsol2PredFichier(date, fichier, dates, data_flux, data_ia, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getAcsol2PredFichier
!
!$Resume
!
!$Description
! V2.0
! Routine interne : extraction dans un fichier des predictions
! effectuees a une date particuliere.
! On commence a remplir 'data_flux' et 'data_ia' a l'indice nb+1.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getAcsol2PredFichier(date, fichier, dates, data_flux, data_ia, nb)
!.    real(KIND=PM_REEL) :: date
!.    character(LEN=*) :: fichier
!.    real(KIND=PM_REEL), dimension(50) :: dates
!.    real(KIND=PM_REEL), dimension(50,2) :: data_flux
!.    integer, dimension(50,9) :: data_ia
!.    integer :: nb
!
!$Arguments
!>E     date       :<PM_REEL>              date des données
!>E     fichier    :<LEN=*>                nom du fichier de données dans lequel effectuer la recherche
!>S     dates      :<PM_REEL,DIM=(50)>     tableau des dates
!>S     data_flux  :<PM_REEL,DIM=(50,2)>   tableau des données liées aux flux
!>S     data_ia    :<integer,DIM=(50,9)>   tableau des données liées aux indices
!>S     nb         :<integer>              nombre d'éléments des tableaux
!
!$Common
!
!$Routines
!- cpsi_getAccesMadona
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
    real(KIND=PM_REEL), intent(in) :: date
    character(LEN=*), intent(in) :: fichier
    real(KIND=PM_REEL), dimension(50), intent(out) :: dates
    real(KIND=PM_REEL), dimension(50,2), intent(out) :: data_flux
    integer, dimension(50,9), intent(out) :: data_ia
    integer, intent(out) :: nb
    
    ! variables locales
    integer :: nature, res, acces
    character(LEN=256) :: libelle
    real(KIND=PM_REEL) :: date_tmp
    
    ! acces MADONA sur les fichiers
    call cpsi_getAccesMadona(trim(fichier), acces)
    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          ret = acc_getd(acces, trim(libelle)//".date_pred", date_tmp, "")
          res = cpsi_compareReels(date, date_tmp)
          if (res.eq.0) then
             ! la prediction courante a ete effectuee a la date demandee
             ! on extrait les donnees
             nb = nb+1
             
             ! selection de la structure
             ret = acc_select(acces, trim(libelle), ACC_STRUCT)
             
             ret = acc_getd(acces, "date", dates(nb), "")
             
             ret = acc_getd(acces, "fluxpr", data_flux(nb,1), "sfu")
             ret = acc_getd(acces, "fluxmp", data_flux(nb,2), "sfu")
             
             ret = acc_geti(acces, "iaampr", data_ia(nb,1))
             ret = acc_geti(acces, "iaapr_1", data_ia(nb,2))
             ret = acc_geti(acces, "iaapr_2", data_ia(nb,3))
             ret = acc_geti(acces, "iaapr_3", data_ia(nb,4))
             ret = acc_geti(acces, "iaapr_4", data_ia(nb,5))
             ret = acc_geti(acces, "iaapr_5", data_ia(nb,6))
             ret = acc_geti(acces, "iaapr_6", data_ia(nb,7))
             ret = acc_geti(acces, "iaapr_7", data_ia(nb,8))
             ret = acc_geti(acces, "iaapr_8", data_ia(nb,9))
             
             ! fin de la selection
             ret = acc_select_end(acces)
          end if
       end if
    end do
    
  end subroutine cpsi_getAcsol2PredFichier

  
  function cps_getAcsol2DernierePred(date, date_pred, data_flux, data_ia) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAcsol2DernierePred
!
!$Resume
!
!$Description
! V2.0
! Obtenir la derniere prediction effectuee pour une date.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAcsol2DernierePred(date, date_pred, data_flux, data_ia)
!.    real(KIND=PM_REEL) :: date
!.    real(KIND=PM_REEL) :: date_pred
!.    real(KIND=PM_REEL), dimension(2) :: data_flux
!.    integer, dimension(9) :: data_ia
!.    integer :: trouve
!
!$Arguments
!>E     date       :<PM_REEL>           date des données
!>S     date_pred  :<PM_REEL>           date à laquelle a été faite la prédiction
!>S     data_flux  :<PM_REEL,DIM=(2)>   données des flux
!>S     data_ia    :<integer,DIM=(9)>   données liées aux indices
!>S     trouve     :<integer>           CPS_OK si les données ont été trouvées, CPS_ERR_DEF sinon
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
    real(KIND=PM_REEL), intent(in) :: date
    real(KIND=PM_REEL), intent(out) :: date_pred
    real(KIND=PM_REEL), dimension(2), intent(out) :: data_flux
    integer, dimension(9), intent(out) :: data_ia
    
    ! resultat
    integer :: trouve, acces, res, nature, ind
    character(LEN=256) :: cle, fichier, libelle, dernier_libelle
    real(KIND=PM_REEL) :: date_tmp
    
    
    ! variables locales
    
    
    ! initialisation
    trouve = CPS_ERR_DEF
    date_pred = 0.0
    
    cle = cpsi_getCleAcsol2(date)
    
    trouve = cps_getFichierAcsol2Pred(date, acces, fichier)
    if (trouve.ne.CPS_OK) then
       ! le fichier n'est pas trouve
       return
    end if

    trouve = CPS_ERR_DEF
    ret = acc_scan_reset(acces)
    do
       ret = acc_scan(acces, libelle, nature)
       if (nature.eq.0) then
          ! fin du fichier
          exit
       else if (nature.eq.ACC_STRUCT) then
          ind = index(trim(libelle), trim(cle))
          if (ind.gt.0) then
             ! la structure correspond a une prediction pour
             ! la date demandee
             ret = acc_getd(acces, trim(libelle)//".date_pred", date_tmp, "")
             res = cpsi_compareReels(date_tmp, date_pred)
             if (res.eq.1) then
                ! la prediction est plus recente
                date_pred = date_tmp
                dernier_libelle = trim(libelle)
                trouve = CPS_OK
             end if
          end if
       end if
    end do
    
    if (trouve.eq.CPS_OK) then
       ! on extrait les donnees dans la structure 'dernier_libelle'
       ! selection de la structure
       ret = acc_select(acces, trim(dernier_libelle), ACC_STRUCT)
       
       ret = acc_getd(acces, "date_pred", date_pred, "")
       
       ret = acc_getd(acces, "fluxpr", data_flux(1), "sfu")
       ret = acc_getd(acces, "fluxmp", data_flux(2), "sfu")
       
       ret = acc_geti(acces, "iaampr", data_ia(1))
       ret = acc_geti(acces, "iaapr_1", data_ia(2))
       ret = acc_geti(acces, "iaapr_2", data_ia(3))
       ret = acc_geti(acces, "iaapr_3", data_ia(4))
       ret = acc_geti(acces, "iaapr_4", data_ia(5))
       ret = acc_geti(acces, "iaapr_5", data_ia(6))
       ret = acc_geti(acces, "iaapr_6", data_ia(7))
       ret = acc_geti(acces, "iaapr_7", data_ia(8))
       ret = acc_geti(acces, "iaapr_8", data_ia(9))

       ! fin de la selection
       ret = acc_select_end(acces)
    end if
    
  end function cps_getAcsol2DernierePred



  subroutine cps_getListeFichiersAtmTab(listeFichiers, code_corps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListeFichiersAtmTab
!
!$Resume
!  Obtention la liste de tous les fichiers d'atmosphère tabulée d'un corps
!  donné.
!
!$Description
!  Obtention la liste de tous les fichiers d'atmosphère tabulée d'un corps
!  donné.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListeFichiersAtmTab(listeFichiers, code_corps)
!.    character(len=CPS_MAXLG), dimension(:), pointer :: listeFichiers
!.    integer :: code_corps
!
!$Arguments
!>E/S   listeFichiers  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers
!                                                         d'atmosphère tabulée
!>E     code_corps     :<integer>                         code du corps      
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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

! Arguments
      character(len=CPS_MAXLG), dimension(:), pointer :: listeFichiers
      integer, intent(in) :: code_corps
    
      ! variables locales
      integer :: nb_ficact, cpt, code_cour
      integer :: ii, acces, nature, ret
      character(LEN=CPS_MAXLG) :: libelle, methode, modele
      character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()

      ! Initialisations
      nb_ficact = 0
      cpt = 0
      if ( associated(listeFichiers) ) deallocate(listeFichiers)

      ! Extraction de la liste des fichiers d'atmosphère
      call cps_getListFichiersCateg(CPS_CATEG_ATMOSPHERE, fichiers)

     ! Passe 1 : Extraction du nombre de fichiers disponibles
      do ii=1, cpsi_size_ptr(fichiers)
         ! Init
         nature = ACC_STRUCT
         ! Extraction d'un accès
         call cpsi_getAccesMadona(fichiers(ii), acces)
         ret = acc_scan_reset(acces)
         boucle1 :do while ( nature /= CPS_FIN_SEQ )
            ! Lecture de l'element courant
            ret = acc_scan(acces, libelle, nature)
            ! S'il ne s'agit pas d'une structure on passe au suivant
            if (nature /= ACC_STRUCT) cycle boucle1
            
            ! Extraction du code du corps courant
            ret = acc_geti(acces, trim(libelle)//".code", code_cour)
            if ( ret /= 0 .or. code_cour /= code_corps ) cycle boucle1

            ! Extraction de la methode du modèle
            ret = acc_gets(acces, trim(libelle)//".methode", methode)
            if ( ret /= 0 .or. trim(methode) /= "FICHIER_TABULE") cycle boucle1

            ! Extraction du nom du modèle
            ret = acc_gets(acces, trim(libelle)//".modele_atmosphere", modele)
            if ( ret /= 0 ) cycle boucle1
          
            ! Le fichier est valide, on le stocke
            nb_ficact = nb_ficact+1
         end do boucle1
      end do

      ! Renvoie de la liste vite
      if ( nb_ficact == 0 ) return

      ! Allocation au nombre d'éléments détectés
      allocate(listeFichiers(nb_ficact))

      ! Passe 2 : Lecture du nom des modeles dans chacun de ces fichiers
      nb_ficact = 1
      do ii=1, cpsi_size_ptr(fichiers)
         ! Init
         nature = ACC_STRUCT
         ! Extraction d'un accès
         call cpsi_getAccesMadona(fichiers(ii), acces)
         ret = acc_scan_reset(acces)
         boucle2 :do while ( nature /= CPS_FIN_SEQ )
            ! Lecture de l'element courant
            ret = acc_scan(acces, libelle, nature)
            ! S'il ne s'agit pas d'une structure on passe au suivant
            if (nature /= ACC_STRUCT) cycle boucle2
            
            ! Extraction du code du corps courant
            ret = acc_geti(acces, trim(libelle)//".code", code_cour)
            if ( ret /= 0 .or. code_cour /= code_corps ) cycle boucle2

            ! Extraction de la methode du modèle
            ret = acc_gets(acces, trim(libelle)//".methode", methode)
            if ( ret /= 0 .or. trim(methode) /= "FICHIER_TABULE") cycle boucle2

            ! Extraction du nom du modèle
            ret = acc_gets(acces, trim(libelle)//".modele_atmosphere", modele)
            if ( ret /= 0 ) cycle boucle2
          
            ! Le fichier est valide, on le stocke 
            listeFichiers(nb_ficact) = modele
            nb_ficact = nb_ficact+1

         end do boucle2
      end do

      ! liberation memoire
      if (associated(fichiers)) deallocate(fichiers)
  
  end subroutine cps_getListeFichiersAtmTab



  function cps_getFichierAtmTab(identifiant, fichier, rep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierAtmTab
!
!$Resume
!  Obtenir le fichier d'atmosphère tabulée correspondant à 
!  un identifiant particulier.
!
!$Description
!  Obtenir le fichier d'atmosphère tabulée correspondant à 
!  un identifiant particulier.
!
!$Auteur
!  Cédric Martel (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierAtmTab(identifiant, fichier, [rep])
!.    character(LEN=*) :: identifiant
!.    character(LEN=*) :: fichier
!.    logical :: rep
!.    integer :: trouve
!
!$Arguments
!>E     identifiant  :<LEN=*>     identifiant associé au fichier (modele)
!>S     fichier      :<LEN=*>     fichier correspondant
!>[E]   rep          :<logical>   booléen indiquant si on souhaite avoir le chemin complet du fichier
!>S     trouve       :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: identifiant
    character(LEN=*), intent(out) :: fichier
    logical, optional, intent(in) :: rep

    ! resultat
    integer :: trouve
    
    ! variables locales
    integer :: i, acces
    character(LEN=CPS_MAXLG) :: rep_tmp, f_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers => NULL()
    logical :: exist

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichierAtmTab", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_ATMOSPHERE, listFichiers)

    do i=1, cpsi_size_ptr(listFichiers)
       ! Lecture du iième fichier de cette catégorie
       call cpsi_getAccesMadona(listFichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichierAtmTab", &
               partie_variable=trim(listFichiers(i)))
          return
       end if

       ! Extraction du fichier et du répertoire
       f_tmp=""
       trouve = cps_getCritere(acces, "modele_atmosphere", trim(identifiant), &
            "fichier_atmosphere", f_tmp)
       rep_tmp = ""
       trouve = cps_getCritere(acces, "modele_atmosphere", trim(identifiant), &
            "repertoire_atmosphere", rep_tmp)
       f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)

       ! Création du nom de fichier relatif
       fichier = trim(f_tmp)
       if (present(rep).and.rep.and.(trouve.eq.CPS_OK)) then
          ! on regarde d'abord si le fichier est dans la base locale
          fichier = trim(rep_base_local)//"/"//trim(f_tmp)
          inquire(file=trim(fichier), exist=exist)
          if (.not.exist) then
             ! si le fichier n'est pas dans la base locale, il est
             ! dans la base de reference
             fichier = trim(rep_base_ref)//"/"//trim(f_tmp)
          end if
       end if
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do
             
    ! liberation memoire
    if (associated(listFichiers)) then
       deallocate(listFichiers, stat=iostat)
    end if

  end function cps_getFichierAtmTab


  subroutine cps_getListeFichiersAcsol2(listeFichiers)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListeFichiersAcsol2
!
!$Resume
!  Obtention la liste de tous les fichiers d'activité solaire ACSOL2 disponibles.
!
!$Description
!  Obtention la liste de tous les fichiers d'activité solaire ACSOL2 disponibles.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListeFichiersAcsol2(listeFichiers)
!.    character(len=CPS_MAXLG), dimension(:), pointer :: listeFichiers
!
!$Arguments
!>E/S   listeFichiers  :<LEN=CPS_MAXLG,DIM=(:),pointer>   liste des fichiers ACSOL2 en base
!
!$Common
!
!$Routines
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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

! Arguments
      character(len=CPS_MAXLG), dimension(:), pointer :: listeFichiers
    
      ! variables locales
      integer :: nb_ficact, cpt
      integer :: ii, acces, nature, ret
      character(LEN=CPS_MAXLG) :: libelle, identifiant
      character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()

      ! Initialisations
      nb_ficact = 0
      cpt = 0
      if ( associated(listeFichiers) ) deallocate(listeFichiers)

      ! Extraction de la liste des fichiers de cette catégorie
      call cps_getListFichiersCateg(CPS_CATEG_ACSOL2_FICHIER, fichiers)

     ! Passe 1 : Extractipon du nombre de fichiers disponibles
      do ii=1, cpsi_size_ptr(fichiers)
         ! Init
         nature = ACC_STRUCT
         ! Extraction d'un accès
         call cpsi_getAccesMadona(fichiers(ii), acces)
         ret = acc_scan_reset(acces)
         boucle1 :do while ( nature /= CPS_FIN_SEQ )
            ! Lecture de l'element courant
            ret = acc_scan(acces, libelle, nature)
            ! S'il ne s'agit pas d'une structure on passe au suivant
            if (nature /= ACC_STRUCT) cycle boucle1
            
            ! Extraction du nom du modèle
            ret = acc_gets(acces, trim(libelle)//".identifiant", identifiant)
            if ( ret /= 0 ) cycle boucle1
          
            ! Le fichier est valide, on le stocke
            nb_ficact = nb_ficact+1
         end do boucle1
      end do

      ! Renvoie de la liste vite
      if ( nb_ficact == 0 ) return

      ! Allocation au nombre d'éléments détectés
      allocate(listeFichiers(nb_ficact))

      ! Passe 2 : Lecture du nom des modeles dans chacun de ces fichiers
      nb_ficact = 1
      do ii=1, cpsi_size_ptr(fichiers)
         ! Init
         nature = ACC_STRUCT
         ! Extraction d'un accès
         call cpsi_getAccesMadona(fichiers(ii), acces)
         ret = acc_scan_reset(acces)
         boucle2 :do while ( nature /= CPS_FIN_SEQ )
            ! Lecture de l'element courant
            ret = acc_scan(acces, libelle, nature)
            ! S'il ne s'agit pas d'une structure on passe au suivant
            if (nature /= ACC_STRUCT) cycle boucle2
            
            ! Extraction du nom du modèle
            ret = acc_gets(acces, trim(libelle)//".identifiant", identifiant)
            if ( ret /= 0 ) cycle boucle2
          
            ! Le fichier est valide, on le stocke
            listeFichiers(nb_ficact) = identifiant
            nb_ficact = nb_ficact+1
         end do boucle2
      end do

      ! liberation memoire
      if (associated(fichiers)) deallocate(fichiers)
  
  end subroutine cps_getListeFichiersAcsol2



  function cps_getFichierAcsol2(identifiant, fichier, rep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getFichierAcsol2
!
!$Resume
!  Obtenir le fichier d'activité solaire ACSOL2 correspondant à 
!  un identifiant particulier.
!
!$Description
!  Obtenir le fichier d'activité solaire ACSOL2 correspondant à 
!  un identifiant particulier.
!
!$Auteur
!  Cédric Martel (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getFichierAcsol2(identifiant, fichier, [rep])
!.    character(LEN=*) :: identifiant
!.    character(LEN=*) :: fichier
!.    logical :: rep
!.    integer :: trouve
!
!$Arguments
!>E     identifiant  :<LEN=*>     identifiant associé au fichier
!>S     fichier      :<LEN=*>     fichier correspondant
!>[E]   rep          :<logical>   booléen indiquant si on souhaite avoir le chemin complet du fichier
!>S     trouve       :<integer>   CPS_OK si le fichier est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: identifiant
    character(LEN=*), intent(out) :: fichier
    logical, optional, intent(in) :: rep

    ! resultat
    integer :: trouve
    
    ! variables locales
    integer :: i, acces
    character(LEN=CPS_MAXLG) :: rep_tmp, f_tmp
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listFichiers => NULL()
    logical :: exist

    ! Initialisation
    trouve = CPS_ERR_DEF

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cps_getFichierAcsol2", &
            partie_variable="cps_utilisateur")
       return
    end if

    call cps_getListFichiersCateg(CPS_CATEG_ACSOL2_FICHIER, listFichiers)

    do i=1, cpsi_size_ptr(listFichiers)
       ! Lecture du iième fichier de cette catégorie
       call cpsi_getAccesMadona(listFichiers(i), acces)
       if (acces.LT.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cps_getFichierAcsol2", &
               partie_variable=trim(listFichiers(i)))
          return
       end if

       ! Extraction du fichier et du répertoire
       f_tmp=""
       trouve = cps_getCritere(acces, "identifiant", trim(identifiant), &
            "fichier_acsol2", f_tmp)
       rep_tmp = ""
       trouve = cps_getCritere(acces, "identifiant", trim(identifiant), &
            "repertoire", rep_tmp)
       f_tmp = trim(rep_tmp)//"/"//trim(f_tmp)

       ! Création du nom de fichier relatif
       fichier = trim(f_tmp)
       if (present(rep).and.rep.and.(trouve.eq.CPS_OK)) then
          ! on regarde d'abord si le fichier est dans la base locale
          fichier = trim(rep_base_local)//"/"//trim(f_tmp)
          inquire(file=trim(fichier), exist=exist)
          if (.not.exist) then
             ! si le fichier n'est pas dans la base locale, il est
             ! dans la base de reference
             fichier = trim(rep_base_ref)//"/"//trim(f_tmp)
          end if
       end if
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do
             
    ! liberation memoire
    if (associated(listFichiers)) then
       deallocate(listFichiers, stat=iostat)
    end if

  end function cps_getFichierAcsol2


  function cpsi_requeteInt(categorie, cle, id, nom, val) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_requeteInt
!
!$Resume
!
!$Description
!  V2.0
!  Obtention d'une valeur entiere dans une structure MADONA.
!  'cle' correspond au nom de la structure MADONA.
!  Dans le cas où l'id n'a pas d'importance, mettre 'id' a 
!  CPS_ALL_ID. Dans le cas où l'attribut a rechercher depend
!  d'une theorie planétaire, affecter a 'id' le nom de la theorie
!  (en majuscules!)
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_requeteInt(categorie, cle, id, nom, val)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    integer :: val
!.    integer :: trouve
!
!$Arguments
!>E     categorie  :<LEN=*>     catégorie dans laquelle chercher
!>E     cle        :<LEN=*>     nom de la structure MADONA à lire
!>E     id         :<LEN=*>     id du fichier dans lequel chercher
!>E     nom        :<LEN=*>     nom de l'attribut
!>S     val        :<integer>   valeur de l'attribut recherché
!>S     trouve     :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: categorie, cle, id, nom
    integer, intent(out) :: val
    
    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(cpsi_desc) :: desc
    integer :: i, nb_fic, ind, acces
    logical :: att_present, id_ok
    
    ! initialisation
    trouve = CPS_ERR_DEF
    val = 0

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_requeteInt", &
            partie_variable="cps_utilisateur")
       return
    end if

    ! fichiers associes a la categorie
    call cps_getListFichiersCateg(trim(categorie),fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    if (nb_fic.eq.0) then
       ! pas de fichiers
       call MSP_signaler_message(cle_mes="CPS_NO_CATEG", &
            routine="cpsi_requeteInt",                   &
            partie_variable=trim(categorie))
       ! liberation memoire
       if (associated(fichiers)) deallocate(fichiers)
       return
    end if

    ! parcours des fichiers pour trouver la valeur recherchee
    do i=1, nb_fic
       ! description du fichier courant
       ret = cpsi_getDescFichier(trim(fichiers(i)), desc)
       if (ret.ne.CPS_OK) then
          ! description non trouvee
          call MSP_signaler_message(cle_mes="CPS_ERR_DESC",&
               routine="cpsi_requeteInt",                  &
               partie_variable=trim(fichiers(i)))
          ! liberation memoire
          if (associated(fichiers)) deallocate(fichiers)
          return
       end if
       att_present = .false.
       id_ok = .false.
       ! on regarde si l'attribut recherche est present
       att_present = cpsi_existeAttDesc(trim(nom),desc)
       ! on regarde si l'id demande est present dans celui du fichier
       if (trim(id).eq.CPS_ALL_ID) then
          ! n'importe quel id convient
          id_ok = .true.
       else
          ind = index(trim(desc%id),trim(id))
          if (ind.gt.0) then
             id_ok = .true.
          end if
       end if
          
       if (att_present.and.id_ok) then
          ! ouverture du fichier
          call cpsi_getAccesMadona(trim(fichiers(i)), acces)
          if (acces.lt.0) then
             ! erreur d'ouverture du fichier
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_requeteInt", &
               partie_variable=trim(fichiers(i)))
             ! liberation memoire
             if (associated(fichiers)) deallocate(fichiers)
             return
          end if
          ! on cherche dans le fichier si la structure designee par 'cle' existe
          ret = acc_exist(acces, trim(cle))
          if (ret.eq.1) then
             ! la structure est presente
             ! on extrait la valeur
             ret = acc_geti(acces, trim(cle)//"."//trim(nom), val)
             ! fin de la recherche
             trouve = CPS_OK
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
             exit
          end if
       end if
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)

  end function cpsi_requeteInt


  function cpsi_requeteReel(categorie, cle, id, nom, val, unite) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_requeteReel
!
!$Resume
!
!$Description
!  V2.0
!  Obtention d'une valeur reelle dans une structure MADONA.
!  'cle' correspond au nom de la structure MADONA.
!  Dans le cas où l'id n'a pas d'importance, mettre 'id' a 
!  CPS_ALL_ID. Dans le cas où l'attribut a rechercher depend
!  d'une theorie planétaire, affecter a 'id' le nom de la theorie
!  (en majuscules!)
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_requeteReel(categorie, cle, id, nom, val, unite)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     categorie  :<LEN=*>     catégorie dans laquelle chercher
!>E     cle        :<LEN=*>     nom de la structure MADONA à lire
!>E     id         :<LEN=*>     id du fichier dans lequel chercher
!>E     nom        :<LEN=*>     nom de l'attribut
!>S     val        :<PM_REEL>   valeur de l'attribut recherché
!>S     unite      :<LEN=*>     unité de l'attribut recherché
!>S     trouve     :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: categorie, cle, id, nom
    real(KIND=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite

    ! resultat
    integer :: trouve

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(cpsi_desc) :: desc
    integer :: i, nb_fic, ind, acces, k
    logical :: att_present, id_ok
    type(cpsi_infoChamp) :: info_champ
    
    ! initialisation
    trouve = CPS_ERR_DEF
    val = 0._pm_reel

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_requeteReel",                         &
            partie_variable="cps_utilisateur")
       return
    end if

    ! fichiers associes a la categorie
    call cps_getListFichiersCateg(trim(categorie),fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    if (nb_fic.eq.0) then
       ! pas de fichiers
       call MSP_signaler_message(cle_mes="CPS_ERR_CATEG", &
            routine="cpsi_requeteReel",                   &
            partie_variable=trim(categorie))
       ! liberation memoire
       if (associated(fichiers)) deallocate(fichiers)
       return
    end if

    ! parcours des fichiers pour trouver la valeur recherchee
    do i=1, nb_fic
       ! description du fichier courant
       ret = cpsi_getDescFichier(trim(fichiers(i)), desc)
       if (ret.ne.CPS_OK) then
          ! description non trouvee
          call MSP_signaler_message(cle_mes="CPS_ERR_DESC",&
               routine="cpsi_requeteReel",                 &
               partie_variable=trim(fichiers(i)))
          ! liberation memoire
           if (associated(fichiers)) deallocate(fichiers)
          return
       end if
       att_present = .false.
       id_ok = .false.
       ! on regarde si l'attribut recherche est present
       att_present = cpsi_existeAttDesc(trim(nom),desc, indice=k)
       ! on regarde si l'id demande est present dans celui du fichier
       if (trim(id).eq.CPS_ALL_ID) then
          ! n'importe quel id convient
          id_ok = .true.
       else
          ind = index(trim(desc%id),trim(id))
          if (ind.gt.0) then
             id_ok = .true.
          end if
       end if
          
       if (att_present.and.id_ok) then
          ! ouverture du fichier
          call cpsi_getAccesMadona(trim(fichiers(i)), acces)
          if (acces.lt.0) then
             ! erreur d'ouverture du fichier
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_requeteReel",                     &
               partie_variable=trim(fichiers(i)))
             ! liberation memoire
             if (associated(fichiers)) deallocate(fichiers)
             return
          end if
          ! on cherche dans le fichier si la structure designee par 'cle' existe
          ret = acc_exist(acces, trim(cle))
          if (ret.eq.1) then
             ! la structure est presente
             ! recherche de l'unite
             info_champ = desc%infosChamps(k)
             unite = trim(info_champ%unite)
             ! on extrait la valeur
             ret = acc_getd(acces, trim(cle)//"."//trim(nom), val, trim(unite))
             ! fin de la recherche
             trouve = CPS_OK
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
             exit
          end if
       end if
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end function cpsi_requeteReel


  function cpsi_requeteString(categorie, cle, id, nom, val) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_requeteString
!
!$Resume
!
!$Description
!  V2.0
!  Obtention d'une valeur chaine de caracteres dans une structure MADONA.
!  'cle' correspond au nom de la structure MADONA.
!  Dans le cas où l'id n'a pas d'importance, mettre 'id' a 
!  CPS_ALL_ID. Dans le cas où l'attribut a rechercher depend
!  d'une theorie planétaire, affecter a 'id' le nom de la theorie
!  (en majuscules!)
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  trouve = cpsi_requeteString(categorie, cle, id, nom, val)
!.    character(LEN=*) :: categorie, cle, id, nom
!.    character(LEN=*) :: val
!.    integer :: trouve
!
!$Arguments
!>E     categorie  :<LEN=*>     catégorie dans laquelle chercher
!>E     cle        :<LEN=*>     nom de la structure MADONA à lire
!>E     id         :<LEN=*>     id du fichier dans lequel chercher
!>E     nom        :<LEN=*>     nom de l'attribut
!>S     val        :<LEN=*>     valeur de l'attribut recherché
!>S     trouve     :<integer>   CPS_OK si l'attribut est trouvé, CPS_ERR_DEF sinon
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: categorie, cle, id, nom
    character(LEN=*), intent(out) :: val
    
    ! resultat
    integer :: trouve, ind, acces
    logical :: att_present, id_ok

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    type(cpsi_desc) :: desc
    integer :: i, nb_fic

    ! initialisation
    trouve = CPS_ERR_DEF
    val = ""

    if (.not.cps_utilisateur_init) then
       ! erreur : module non initialise
       call MSP_signaler_message(cle_mes="CPS_ERR_INIT_MODULE", &
            routine="cpsi_requeteString", &
            partie_variable="cps_utilisateur")
       return
    end if

    ! fichiers associes a la categorie
    call cps_getListFichiersCateg(trim(categorie),fichiers)
    nb_fic = cpsi_size_ptr(fichiers)
    
    if (nb_fic.eq.0) then
       ! pas de fichiers
       call MSP_signaler_message(cle_mes="CPS_ERR_CATEG", &
            routine="cpsi_requeteString",                 &
            partie_variable=trim(categorie))
       ! liberation memoire
       if (associated(fichiers)) deallocate(fichiers)
       return
    end if

    ! parcours des fichiers pour trouver la valeur recherchee
    do i=1, nb_fic
       ! description du fichier courant
       ret = cpsi_getDescFichier(trim(fichiers(i)), desc)
       if (ret.ne.CPS_OK) then
          ! description non trouvee
          call MSP_signaler_message(cle_mes="CPS_ERR_DESC",&
               routine="cpsi_requeteString",               &
               partie_variable=trim(fichiers(i)))
          ! liberation memoire
          if (associated(fichiers)) deallocate(fichiers)
          return
       end if
       att_present = .false.
       id_ok = .false.
       ! on regarde si l'attribut recherche est present
       att_present = cpsi_existeAttDesc(trim(nom),desc)
       ! on regarde si l'id demande est present dans celui du fichier
       if (trim(id).eq.CPS_ALL_ID) then
          ! n'importe quel id convient
          id_ok = .true.
       else
          ind = index(trim(desc%id),trim(id))
          if (ind.gt.0) then
             id_ok = .true.
          end if
       end if
          
       if (att_present.and.id_ok) then
          ! ouverture du fichier
          call cpsi_getAccesMadona(trim(fichiers(i)), acces)
          if (acces.lt.0) then
             ! erreur d'ouverture du fichier
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
               routine="cpsi_requeteString",                   &
               partie_variable=trim(fichiers(i)))
             ! liberation memoire
             if (associated(fichiers)) deallocate(fichiers)
             return
          end if
          ! on cherche dans le fichier si la structure designee par 'cle' existe
          ret = acc_exist(acces, trim(cle))
          if (ret.eq.1) then
             ! la structure est presente
             ! on extrait la valeur
             ret = acc_gets(acces, trim(cle)//"."//trim(nom), val)
             ! fin de la recherche
             trouve = CPS_OK
             if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
             exit
          end if
       end if
       if (associated(desc%infosChamps)) deallocate(desc%infosChamps, stat=iostat)
    end do
    
    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end function cpsi_requeteString


  subroutine cps_close_utilisateur()    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_close_utilisateur
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
!  call cps_close_utilisateur()    
!
!$Arguments
!
!$Common
!
!$Routines
!- cps_close_base
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

    ! desallocation memoire des modules cps_base, cps_desc et cps_accesMadona
    call cps_close_base()
    cps_utilisateur_init = .false.
  end subroutine cps_close_utilisateur

end module cps_utilisateur
