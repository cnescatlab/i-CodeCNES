module cps_acces

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_acces
!
!$Resume
!  Compatibilité COMPAS V1-5
!
!$Description
!  les routines ci-dessous sont conservée pour compatibilité ascendante
!  uniquement
!
!$Auteur
!
!$Version
!  $Id: cps_acces.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_acces.F90,v $
!  Revision 1.26  2010/10/21 13:46:19  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.25  2008/10/28 16:40:26  cml
!  FA-ID 1061 : Mise a jour de la propagation d une evenetuelle erreur durant l init. de la base
!
!  Revision 1.24  2008/10/28 12:43:47  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.23  2008/10/06 07:49:43  cml
!  FA-ID 1059 : Suppression de cps_charger et arguments obsoletes de cps_init
!
!  Revision 1.22  2008/10/03 12:37:34  cml
!  FA-ID 1024 : Modification de la signature de cpsi_existeAttDesc
!
!  Revision 1.21  2008/08/04 12:47:08  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration et ajout d'une gestion d'erreur dans la fonction retournant le
!  nom d'un corps en fonction de son code.
!
!  Revision 1.20  2008/07/04 12:15:06  huec
!  FA-ID 1002 : Modification du cartouche
!
!  Revision 1.19  2008/07/04 11:39:40  huec
!  DM-ID 1002 : Ajout du code pour le paremetre kep dans la fonction cps_existe
!
!  Revision 1.18  2007/06/20 08:51:17  vivaresf
!  FA-ID 746 : désallocation des variables de type cpsi_desc
!
!  Revision 1.17  2007/06/18 08:51:55  vivaresf
!  FA-ID 746 : deallocate des variables locale pointeur
!  suppression des codes commentés
!
!  Revision 1.16  2007/06/15 14:53:34  vivaresf
!  FA-ID 746 : désallocation de listeFichiers pour éviter une fuite mémoire
!
!  Revision 1.15  2007/05/21 06:56:17  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.14  2006/11/09 14:40:02  vivaresf
!  INitialisation de pointeurs
!  Revision 1.13  2006/10/24 15:38:10  vpg
!  correction d'un bug dans cps_path : acces a un pointeur non initialise
!  Revision 1.12  2006/10/18 09:52:19  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.11.2.1  2006/09/26 12:13:16  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.11  2006/07/04 12:05:28  okd
!  Correctino d'un message FORESYS
!  Revision 1.10  2006/05/30 15:25:48  vivaresf
!  Metriques : supression d'un niveau d'imbrication
!  Revision 1.9  2006/05/30 12:29:00  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.8  2006/05/30 09:11:18  vivaresf
!  Commentaires
!  Revision 1.7  2006/05/30 08:27:44  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.6  2006/03/10 12:45:49  vpg
!  Mise a jour suite a la modification sur cpsi_getAtt_d() ou l'unite est obligatoire
!  Revision 1.5  2006/01/23 14:05:12  vpg
!  rajout des traitements MAGE, rajout de fonctions liees aux ihm
!  Revision 1.4  2005/12/08 18:20:23  vivaresf
!  Cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_acces
!
!$Structure
!
!$Global
!
!>  cps_var_init   : <logical,public>   indicateur d'initialisation
!#V
!>  ret            : <integer,private>  
!>  iostat         : <integer,private>  
!#
!$Common
!
!$Routines
!- cps_constante
!- cps_init
!- cps_liste
!- cps_info
!- cps_codenom
!- cps_close
!- cps_get
!- cps_constante_bd
!- cps_constante_tab
!- cps_path
!- cps_constante_code
!
!$Fonctions
!- cps_existe
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Interface
!> cps_constante :  cps_constante_bd, cps_constante_tab, cps_path, 
!                   cps_constante_code
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cps_existe cps_constante cps_init cps_init cps_liste cps_info cps_codenom cps_close cps_get
!.  cps_constante_bd cps_constante_tab cps_path cps_constante_code
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_utilisateur

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_acces.F90 69 2012-09-11 08:33:34Z ffsm $'


  ! Variable de robustesse (status de desallocation), interne
  integer, private :: iostat

  interface cps_constante

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante
!
!$Resume
!  Extraction de constantes pour un corps (ou liste) dans la theorie courante (obsolète)
!
!$Description
!   Extraction d'attributs pour un corps (ou liste) dans la theorie courante
!   Le corps est defini soit comme un code, soit comme un nom
!   les attributs sont soit des constanets physiques, soit des modeles
!   soit des parametrages informatiques.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante(corps, [mu], [requa], [apla], [j2], [vrot], &
!.           potentiel, atmosphere, geometrie, dga, exc, inc, pom, gom, anm, dateref, &
!.           corpsc, typec, code, g, vlum, ua, coef_prs, theorie)
!.    character(LEN=*) :: corps
!.    real(kind=PM_REEL) :: mu, requa, apla, J2, vrot
!.    real(kind=PM_REEL) :: dga, exc, inc, pom, gom, anm, dateref
!.    character(LEN=*), dimension(:) :: potentiel
!.    character(LEN=*), dimension(:) :: atmosphere
!.    character(LEN=*), dimension(:) :: geometrie
!.    integer :: code
!.    character(LEN=*) :: corpsc, typec, theorie
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!  call cps_constante(corps, nb, [mu], [requa], [apla], [j2], [vrot], &
!.                 dga, exc, inc,pom, gom, anm, dateref, &
!.                 corpsc, typec, code, g, vlum, ua, coef_prs)
!.    character(LEN=*), dimension(nb) :: corps
!.    integer :: nb
!.    real(kind=PM_REEL), dimension(nb) :: mu, requa, apla, vrot
!.    real(kind=PM_REEL), dimension(nb) :: J2
!.    real(kind=PM_REEL), dimension(nb) :: dga, exc, inc, pom, gom, anm, dateref
!.    integer, dimension(nb) :: code
!.    character(LEN=*), dimension(nb) :: corpsc, typec
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!  call cps_constante(corps, path, [fichier], [modele])
!.    character(LEN=*) :: corps
!.    character(LEN=*) :: path
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: modele
!
!  call cps_constante(corps, [mu], [requa], [apla], [j2], [vrot], &
!.           potentiel, atmosphere, geometrie, dga, exc, inc, pom, gom, anm, dateref, &
!.           corpsc, typec, code, g, vlum, ua, coef_prs, theorie)
!.    integer :: corps
!.    real(kind=PM_REEL) :: mu, requa, apla, J2, vrot
!.    real(kind=PM_REEL) :: dga, exc, inc, pom, gom, anm, dateref
!.    character(LEN=*), dimension(:) :: potentiel
!.    character(LEN=*), dimension(:) :: atmosphere
!.    character(LEN=*), dimension(:) :: geometrie
!.    integer :: code
!.    character(LEN=*) :: corpsc, typec, theorie
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!$Procedures
!- cps_constante_bd
!- cps_constante_tab
!- cps_path
!- cps_constante_code
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cps_constante_bd, cps_constante_tab, cps_path, cps_constante_code
  end interface

  logical, public, save :: cps_var_init = .false.

contains

  ! initialisation
  subroutine cps_init(fichier) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_init
!
!$Resume
!  Initialisation de la partie utilisateur et de la théorie
!
!$Description
!  Initialisation de la partie utilisateur et de la théorie. Si aucune
!  théorie n'est fournie par fichier il s'agira de l'UAI1994.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_init([fichier])
!.    character (LEN=*) :: fichier
!
!$Arguments
!>[E]   fichier  :<LEN=*> Nom de la théorie à charger
!
!$Common
!
!$Routines
!- cps_init_utilisateur
!- cps_setTheorie
!
!$Include
!
!$Module
!
!$Remarques
!  Génération d'une exception si une théorie fournie est inexistante.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    character (LEN=*), optional, intent(in) :: fichier
    integer :: ret

    ! Init utilisateur
    call cps_init_utilisateur()
    if ( MSP_gen_messages("cps_init") ) return

    !Mise a jour du flag
    cps_var_init = .true.

    if ( .not. present(fichier)) then
       ! On utilise la méthode UAI1994 par défaut
       call cps_setTheorie("UAI1994")
    else
       ret = index(fichier, "uai1994")
       if (ret.gt.0) then
          call cps_setTheorie("UAI1994")
          return
       end if
       ret = index(fichier, "de403")
       if (ret.gt.0) then
          call cps_setTheorie("DE403")
          return
       end if
       ret = index(fichier, "de405")
       if (ret.gt.0) then
          call cps_setTheorie("DE405")
          return
       end if
       ret = index(fichier, "uai76")
       if (ret.gt.0) then
          call cps_setTheorie("UAI76")
          return
       end if
       ret = index(fichier, "iers92")
       if (ret.gt.0) then
          call cps_setTheorie("IERS92")
          return
       end if

       ! Si la theorie en entrée n'est pas déjà listée on renvoie une erreur
       cps_var_init = .false.
       call MSP_signaler_message(cle_mes="CPS_ERR_THEORIE", &
             routine="cps_init", &
             partie_variable=trim(fichier) )
    end if

  end subroutine cps_init


  subroutine cps_liste(liste, nbliste, typec, corpsc, codes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_liste
!
!$Resume
!  obtenir la liste des corps suivant un critere (obsolète)
!f
!$Description
!  Compatibilité COMPAS V1-5
! obtenir la liste des corps
! - soit tous les corps de la base
! - soit tous les corps selon le type
! - soit tous les corps selon le corps central
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_liste(liste, nbliste, [typec], [corpsc], [codes])
!.    character(LEN=*), dimension(MAXCORPS) :: liste
!.    character(LEN=*) :: typec, corpsc
!.    integer, dimension(MAXCORPS) :: codes
!.    integer :: nbliste
!
!$Arguments
!>S     liste    :<LEN=*,DIM=(MAXCORPS)>     
!>S     nbliste  :<integer>                  
!>[E]   typec    :<LEN=*>                    
!>[E]   corpsc   :<LEN=*>                    
!>[S]   codes    :<integer,DIM=(MAXCORPS)>   
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListCorps
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- cps_getCorps
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
    character(LEN=*), dimension(MAXCORPS), intent(out) :: liste
    character(LEN=*), optional, intent(in) :: typec, corpsc
    integer, dimension(MAXCORPS),optional, intent(out) :: codes
    integer, intent(out) :: nbliste

    integer :: codeCorpsc
    integer, dimension(:), pointer :: listeCodeCorps => NULL()
    integer :: trouve, ii, acces
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    character(LEN=CPS_MAXLG) :: nom_corps, mess

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    ! tous les corps de la base
    if ((.not.present(corpsc)).and.&
         (.not.present(typec))) then
       call cps_getListCorps(listeCodeCorps)
       nbliste = cpsi_size_ptr(listeCodeCorps)
       if (nbliste.gt.MAXCORPS) nbliste = MAXCORPS

       do ii=1, nbliste
          trouve = cps_getAtt(listeCodeCorps(ii), "nom_id", nom_corps)
          liste(ii) = trim(nom_corps)
       end do

       if (present(codes)) then
          codes(1:nbliste) = listeCodeCorps(1:nbliste)
       end if
       
       ! liberation memoire
       if (associated(listeCodeCorps)) then
          deallocate(listeCodeCorps, stat=iostat)
       end if

    end if

    if(nbliste.eq.0) mess=" "

    if (present(corpsc)) then
       ! trouver le code du corps dont le nom vaut corpsc
       mess="pour le corps central "//trim(corpsc)
       call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiers)
       trouve = CPS_ERR_DEF
       do ii=1, cpsi_size_ptr(fichiers)
          call cpsi_getAccesMadona(fichiers(ii), acces)
          ! recherche avec corpc = nom_fr
          trouve = cps_getCritere(acces, "nom_id", corpsc, "code", codeCorpsc)
          if (trouve.eq.CPS_OK) then
             exit
          end if
       end do

       if (trouve.eq.CPS_OK) then
          call cps_getCorps("corpsc", codeCorpsc, listeCodeCorps)
          nbliste = cpsi_size_ptr(listeCodeCorps)
          if (nbliste.gt.MAXCORPS) then
             nbliste = MAXCORPS
          end if
          do ii=1, nbliste
             trouve = cps_getAtt(listeCodeCorps(ii), "nom_id", nom_corps)
             liste(ii) = trim(nom_corps)
          end do

          if (present(codes)) then
             codes(1:nbliste) = listeCodeCorps(1:nbliste)
          end if
       end if
    end if

    if (present(typec).and.&
         (.not.present(corpsc))) then
       call cps_getCorps("type", typec, listeCodeCorps)
       nbliste = cpsi_size_ptr(listeCodeCorps)
       if (nbliste.gt.MAXCORPS) then
          nbliste = MAXCORPS
       end if
       do ii=1, nbliste
          trouve = cps_getAtt(listeCodeCorps(ii), "nom_id", nom_corps)
          liste(ii) = trim(nom_corps)
       end do
       if(.not.present(corpsc)) then
          mess = "pour le type "//trim(typec)
       else
          mess = trim(mess)//" et le type "//trim(typec)
       endif
       
       if (present(codes)) then
          codes(1:nbliste) = listeCodeCorps(1:nbliste)
       end if
       
    end if
    
    if (nbliste.eq.0) then
       call MSP_signaler_message(cle_mes="CPS_LISTEVIDE", & 
            routine="cps_liste", &
            partie_variable=trim(mess))
    endif

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if
    if (associated(listeCodeCorps)) then
       deallocate(listeCodeCorps, stat=iostat)
    end if
    
    
  end subroutine cps_liste

  

  subroutine cps_info(defbase, base, path)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_info
!
!$Resume
!   Informations generales sur la base utilisée (obsolète)
!
!$Description
!  Compatibilité COMPAS V1-5
!  Fonction inutile, conservée pour compatibilité
!  retourne le repertoire de la base courante
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_info([defbase], [base], [path])
!.    character(LEN=*) :: path, defbase, base
!
!$Arguments
!>[S]   defbase  :<LEN=*>   
!>[S]   base     :<LEN=*>   
!>[S]   path     :<LEN=*>   
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
    character(LEN=*), optional , intent(out) :: path, defbase, base
    
    if(present(path))    path    = trim(rep_base_ref)
    if(present(base))    base    = trim(rep_base_ref)
    if(present(defbase)) defbase = trim(rep_base_ref)
    
  end subroutine cps_info


  ! teste l'existence d'une donnee
  logical function cps_existe(corps, mu, requa, apla, J2, vrot, &
          potentiel, atmosphere, geometrie, dga, exc, inc, pom, gom, anm, &
          dateref, kep, corpsc, typec, G, vlum, ua, coef_prs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_existe
!
!$Resume
!  teste l'existence d'une donnee (obsolète)
!
!$Description
!  teste l'existence d'une constante pour un corps données
!  dans la théorie courante
!  Compatibilité COMPAS V1-5
!
!$Auteur
!  COMPAS V1-5
!
!$Acces
!  PUBLIC
!
!$Usage
!.      logical function cps_existe(corps, mu, requa, apla, J2, vrot, &
!.              [potentiel], [atmosphere], [geometrie], [dga], [exc], [inc], [pom], [gom], [anm], &
!.              [dateref], [kep], [corpsc], [typec], [g], [vlum], [ua], [coef_prs])
!.    character(LEN=*) :: corps
!.    logical :: mu, requa, apla, J2, vrot
!.    logical :: potentiel, atmosphere, geometrie
!.    logical :: dga, exc, inc, pom, gom, anm, dateref, kep
!.    logical :: corpsc, typec, G, vlum, ua, coef_prs
!
!$Arguments
!>E     corps       :<LEN=*>     nom du corps d'interet (nom_id)
!>[S]   mu          :<logical>   |
!>[S]   requa       :<logical>   |
!>[S]   apla        :<logical>   |
!>[S]   J2          :<logical>   |
!>[S]   vrot        :<logical>   |
!>[S]   potentiel   :<logical>   |
!>[S]   atmosphere  :<logical>   |
!>[S]   geometrie   :<logical>   |
!>[S]   dga         :<logical>   |
!>[S]   exc         :<logical>   |- la constantes mentionnée existe pour "corps"
!>[S]   inc         :<logical>   |
!>[S]   pom         :<logical>   |
!>[S]   gom         :<logical>   |
!>[S]   anm         :<logical>   |
!>[S]   dateref     :<logical>   |
!>[S]   kep         :<logical>   |
!>[S]   corpsc      :<logical>   |
!>[S]   typec       :<logical>   |
!>[S]   G           :<logical>   |
!>[S]   vlum        :<logical>   |
!>[S]   ua          :<logical>   |
!>[S]   coef_prs    :<logical>   |
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: corps
    logical, optional, intent(out) :: mu, requa, apla, J2, vrot
    logical, optional, intent(out) :: potentiel, atmosphere, geometrie
    logical, optional, intent(out) :: dga, exc, inc, pom, gom, anm, dateref, kep
    logical, optional, intent(out) :: corpsc, typec, G, vlum, ua, coef_prs

    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModeles => NULL()
    integer :: trouve, acces, code_corps, i
    real(kind=PM_REEL) :: v_tmp
    character(LEN=256) :: unite
    integer :: ret
    
    cps_existe = .false.

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if
    
    ! trouver le code du corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
    trouve = CPS_ERR_DEF
    do i=1, cpsi_size_ptr(fichiersCorps)
       call cpsi_getAccesMadona(fichiersCorps(i), acces)
       ! recherche avec corps = nom_id
       trouve = cps_getCritere(acces, "nom_id", corps, "code", code_corps)
       if (trouve.eq.CPS_OK) then
          cps_existe = .true.
          exit
       end if
    end do
    
    if (trouve.ne.CPS_OK) then
       ! le corps n'est pas dans la base
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSINC", & 
            routine="cps_existe", partie_variable=trim(corps), &
            type=MSP_ENUM_WARNING)
       ! liberation memoire
       if (associated(fichiersCorps)) deallocate(fichiersCorps, stat=iostat)
       return
    end if

    if (present(kep).and.(trouve.eq.CPS_OK)) then
       kep = .false.
       kep = cps_existeAtt(code_corps, "kep")
       cps_existe = cps_existe.and.kep
    end if

    if (present(mu).and.(trouve.eq.CPS_OK)) then
       mu = .false.
       mu = cps_existeAtt(code_corps, "mu")
       cps_existe = cps_existe.and.mu
    end if
       
    if (present(requa).and.(trouve.eq.CPS_OK)) then
       requa = .false.
       requa = cps_existeAtt(code_corps, "requa")
       cps_existe = cps_existe.and.requa
    end if

    if (present(apla).and.(trouve.eq.CPS_OK)) then
       apla = .false.
       apla = cps_existeAtt(code_corps, "apla")
       cps_existe = cps_existe.and.apla
    end if
    
    if (present(J2).and.(trouve.eq.CPS_OK)) then
       J2 = .false.
       J2 = cps_existeAtt(code_corps, "J2")
       cps_existe = cps_existe.and.J2
    end if

    if (present(vrot).and.(trouve.eq.CPS_OK)) then
       vrot = .false.
       vrot = cps_existeAtt(code_corps, "vrot")
       cps_existe = cps_existe.and.vrot
    end if

    if (present(potentiel).and.(trouve.eq.CPS_OK)) then
       potentiel = .false.
       ret = cps_getModelesPotentielCorps(code_corps, listeModeles)
       if (ret.eq.CPS_OK) then
          potentiel = .true.
       end if
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       cps_existe = cps_existe.and.potentiel
    end if

    if (present(atmosphere).and.(trouve.eq.CPS_OK)) then
       atmosphere = .false.
       ret = cps_getListEltsCorps(code_corps, "modele_atmosphere", listeModeles)
       if (ret.eq.CPS_OK) then
          atmosphere = .true.
       end if
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       !atmosphere = cps_existeAtt(code_corps, "modele_atmosphere")
       cps_existe = cps_existe.and.atmosphere
    end if

    if (present(geometrie).and.(trouve.eq.CPS_OK)) then
       geometrie = .false.
       ret = cps_getListEltsCorps(code_corps, "modele_geometrie", listeModeles)
       if (ret.eq.CPS_OK) then
          geometrie = .true.
       end if
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       !geometrie = cps_existeAtt(code_corps, "modele_geometrie")
       cps_existe = cps_existe.and.geometrie
    end if
    
    if (present(dga).and.(trouve.eq.CPS_OK)) then
       dga = .false.
       dga = cps_existeAtt(code_corps, "dga")
       cps_existe = cps_existe.and.dga
    end if

    if (present(exc).and.(trouve.eq.CPS_OK)) then
       exc = .false.
       exc = cps_existeAtt(code_corps, "exc")
       cps_existe = cps_existe.and.exc
    end if

    if (present(inc).and.(trouve.eq.CPS_OK)) then
       inc = .false.
       inc = cps_existeAtt(code_corps, "inc")
       cps_existe = cps_existe.and.inc
    end if
    
    if (present(pom).and.(trouve.eq.CPS_OK)) then
       pom = .false.
       pom = cps_existeAtt(code_corps, "pom")
       cps_existe = cps_existe.and.pom
    end if

    if (present(gom).and.(trouve.eq.CPS_OK)) then
       gom = .false.
       gom = cps_existeAtt(code_corps, "gom")
       cps_existe = cps_existe.and.gom
    end if

    if (present(anm).and.(trouve.eq.CPS_OK)) then
       anm = .false.
       anm = cps_existeAtt(code_corps, "anm")
       cps_existe = cps_existe.and.anm
    end if

    if (present(dateref).and.(trouve.eq.CPS_OK)) then
       dateref = .false.
       dateref = cps_existeAtt(code_corps, "dateref")
       cps_existe = cps_existe.and.dateref
    end if

    if (present(corpsc).and.(trouve.eq.CPS_OK)) then
       corpsc = .false.
       corpsc = cps_existeAtt(code_corps, "corpsc")
       cps_existe = cps_existe.and.corpsc
    end if

    if (present(typec).and.(trouve.eq.CPS_OK)) then
       typec = .false.
       typec = cps_existeAtt(code_corps, "type")
       cps_existe = cps_existe.and.typec
    end if

    ! on cherche parmi les constantes generales de la theorie courante
    if (present(G)) then
       G = .false.
       trouve = cps_getCsteGenThCourante("G", v_tmp, unite)
       if (trouve.eq.CPS_OK) then
          G = .true.
       end if
       cps_existe = cps_existe.and.G
    end if

    ! on cherche parmi les constantes generales de la theorie courante
    if (present(vlum)) then
       vlum = .false.
       trouve = cps_getCsteGenThCourante("vlum", v_tmp, unite)
       if (trouve.eq.CPS_OK) then
          vlum = .true.
       end if
       cps_existe = cps_existe.and.vlum
    end if

    ! on cherche parmi les constantes generales de la theorie courante
    if (present(ua)) then
       ua = .false.
       trouve = cps_getCsteGenThCourante("ua", v_tmp, unite)
       if (trouve.eq.CPS_OK) then
          ua = .true.
       end if
       cps_existe = cps_existe.and.ua
    end if

    ! a modifier : ne depend pas du corps!
    if (present(coef_prs)) then
       coef_prs = .false.
       trouve = cps_getCsteGenThCourante("coef_prs", v_tmp, unite)
       if (trouve.eq.CPS_OK) then
          coef_prs = .true.
       end if
       cps_existe = cps_existe.and.coef_prs
    end if

    ! liberation memoire
    if (associated(fichiersCorps)) deallocate(fichiersCorps, stat=iostat)
    if (associated(listeModeles)) deallocate(listeModeles, stat=iostat)

         
  end function cps_existe


  subroutine cps_codenom(code, nom, lang)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_codenom
!
!$Resume
!  obtenir le nom d'un corps selon son code et la langue courante
!
!$Description
!  Cette fonction retourne le nom d'un corps en fonction de la langue,
!  c'est à dire la champ "nom_<lang>"
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_codenom(code, nom, [lang])
!.    integer :: code
!.    character(LEN=*) :: nom
!.    character(LEN=*) :: lang
!
!$Arguments
!>E     code  :<integer>   code NAIF
!>S     nom   :<LEN=*>     champs nom_en ou nom_fr ou nom_id
!>[E]   lang  :<LEN=*>     langue (type $LANG ou $MADONA_LANG) id (défaut), fr, 
!                          en ou autres champs en fonction de la base utilisée
!
!$Common
!
!$Routines
!- cps_init
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
    integer, intent(in) :: code
    character(LEN=*), intent(out) :: nom
    character(LEN=*), intent(in), optional :: lang

    integer :: trouve
    character(LEN=5) :: codes

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if
    nom=""

    if (present(lang)) then
       if (lang.eq.'fr') then
          ! nom_fr
          trouve = cps_getAtt(code, "nom_fr", nom)
       elseif (lang.eq.'en') then
          ! nom_en
          trouve = cps_getAtt(code, "nom_en", nom)
       else
          ! erreur
          trouve = CPS_ERR_DEF
          nom = ""
       end if
    else
       ! nom_id
       trouve = cps_getAtt(code, "nom_id", nom)
    end if

    ! Gestion d'erreur
    if (trouve == CPS_ERR_DEF) then
       write(codes,'(i5)') code
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSINC", &
            routine="cps_codenom",partie_variable=trim(codes))
    endif

  end subroutine cps_codenom


  subroutine cps_close()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_close
!
!$Resume
!  Fermeture COMPAS (liberation memoire), obsolète
!
!$Description
!  Compatibilité COMPAS V1-5
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_close()
!
!$Arguments
!
!$Common
!
!$Routines
!- cps_close_utilisateur
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
    if (cps_var_init) then
       call cps_close_utilisateur()
       cps_var_init = .false.
    end if
  end subroutine cps_close






  ! obtenir une donnee en fonction de son nom et du corps
  subroutine cps_get(corps, libelle, vals, vald, vali, unit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_get
!
!$Resume
!  obtenir une donnee en fonction de son nom et du corps (obsolète)
!
!$Description
!  Compatibilité COMPAS V1-5
!  Fonction generique pour recuperer une constante pour un corps donné
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_get(corps, [libelle], [vals], [vald], [vali], [unit])
!.    character(LEN=*) :: corps
!.    character(LEN=*) :: libelle, unit
!.    character(LEN=*) :: vals
!.    real(KIND=PM_REEL) :: vald
!.    integer :: vali
!
!$Arguments
!>E     corps    :<LEN=*>     
!>[E]   libelle  :<LEN=*>     
!>[S]   vals     :<LEN=*>     
!>[S]   vald     :<PM_REEL>   
!>[S]   vali     :<integer>   
!>[E]   unit     :<LEN=*>     
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: corps
    character(LEN=*), intent(in), optional :: libelle, unit
    character(LEN=*), intent(out), optional :: vals
    real(KIND=PM_REEL), intent(out), optional :: vald
    integer, intent(out), optional :: vali
    
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
    integer :: code_corps, acces, trouve, i, type_att, index_att
    character(LEN=CPS_MAXLG) :: unite_att, fichier
    logical :: att_present, existe
    type(cpsi_desc), dimension(:), pointer :: listeFichiers => NULL()

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    ! trouver le code du corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
    trouve = CPS_ERR_DEF
    do i=1, cpsi_size_ptr(fichiersCorps)
       call cpsi_getAccesMadona(fichiersCorps(i), acces)
       ! recherche avec corps = nom_id
       trouve = cps_getCritere(acces, "nom_id", corps, "code", code_corps)
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do
    
    existe = .false.
    att_present = .false.

    call cpsi_getListeDescFichiers(listeFichiers)

    do i=1, cpsi_size_ptr(listeFichiers)
       ! recherche si l'attribut est present dans le fichier courant
       att_present = cpsi_existeAttDesc(trim(libelle), listeFichiers(i), indice=index_att)
       if (att_present) then
          ! un fichier est trouve
          ! determiner la valeur dans ce fichier
          fichier = listeFichiers(i)%fichier
          type_att = listeFichiers(i)%infosChamps(index_att)%type
          select case (type_att)
          case (CPS_ENTIER)
             existe = cpsi_getValCorpsFichier_i(code_corps, fichier, libelle, vali)
          case (CPS_REEL)
             unite_att = listeFichiers(i)%infosChamps(index_att)%unite
             if (present(unit)) then
                existe = cpsi_getValCorpsFichier_d(code_corps, fichier, libelle, vald, unit)
             else
                existe = cpsi_getValCorpsFichier_d(code_corps, fichier, libelle, vald, unite_att)
             end if
          case (CPS_STRING)
             existe = cpsi_getValCorpsFichier_s(code_corps, fichier, libelle, vals)
          end select
       end if
       if (existe) then
          exit
       end if
    end do

    ! liberation memoire
    if (associated(fichiersCorps)) deallocate(fichiersCorps, stat=iostat)
    call cpsi_deallocateDescFichiers(listeFichiers)
    
  end subroutine cps_get


  subroutine cps_constante_bd(corps, mu, requa, apla, J2, vrot, &
       potentiel, atmosphere, geometrie, dga, exc, inc, pom, gom, anm, dateref, &
       corpsc, typec, code, g, vlum, ua, coef_prs, theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante_bd
!
!$Resume
!  acces aux constantes corps par corps (obsolète)
!
!$Description
!  Compatibilité COMPAS V1-5
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante_bd(corps, [mu], [requa], [apla], [j2], [vrot], &
!.           [potentiel], [atmosphere], [geometrie], [dga], [exc], [inc], [pom], [gom], [anm], [dateref], &
!.           [corpsc], [typec], [code], [g], [vlum], [ua], [coef_prs], [theorie])
!.    character(LEN=*) :: corps
!.    real(kind=PM_REEL) :: mu, requa, apla, J2, vrot
!.    real(kind=PM_REEL) :: dga, exc, inc, pom, gom, anm, dateref
!.    character(LEN=*), dimension(:) :: potentiel
!.    character(LEN=*), dimension(:) :: atmosphere
!.    character(LEN=*), dimension(:) :: geometrie
!.    integer :: code
!.    character(LEN=*) :: corpsc, typec, theorie
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!$Arguments
!>E     corps       :<LEN=*>           
!>[S]   mu          :<PM_REEL>         
!>[S]   requa       :<PM_REEL>         
!>[S]   apla        :<PM_REEL>         
!>[S]   J2          :<PM_REEL>         
!>[S]   vrot        :<PM_REEL>         
!>[S]   potentiel   :<LEN=*,DIM=(:)>   
!>[S]   atmosphere  :<LEN=*,DIM=(:)>   
!>[S]   geometrie   :<LEN=*,DIM=(:)>   
!>[S]   dga         :<PM_REEL>         
!>[S]   exc         :<PM_REEL>         
!>[S]   inc         :<PM_REEL>         
!>[S]   pom         :<PM_REEL>         
!>[S]   gom         :<PM_REEL>         
!>[S]   anm         :<PM_REEL>         
!>[S]   dateref     :<PM_REEL>         
!>[S]   corpsc      :<LEN=*>           
!>[S]   typec       :<LEN=*>           
!>[S]   code        :<integer>         
!>[S]   g           :<PM_REEL>         
!>[S]   vlum        :<PM_REEL>         
!>[S]   ua          :<PM_REEL>         
!>[S]   coef_prs    :<PM_REEL>         
!>[S]   theorie     :<LEN=*>           
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
!- MSP_signaler_message
!- cps_getTheorie
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
    character(LEN=*), intent(in) :: corps
    real(kind=PM_REEL), optional, intent(out) :: mu, requa, apla, J2, vrot
    real(kind=PM_REEL), optional, intent(out) :: dga, exc, inc, pom, gom, anm, dateref
    character(LEN=*), optional, dimension(:), intent(out) :: potentiel
    character(LEN=*), optional, dimension(:), intent(out) :: atmosphere
    character(LEN=*), optional, dimension(:), intent(out) :: geometrie
    integer, optional, intent(out) :: code
    character(LEN=*), optional, intent(out) :: corpsc, typec, theorie
    real(kind=PM_REEL), optional, intent(out) :: G, vlum, ua, coef_prs

    integer :: code_corps, trouve, acces, i, code_corpsc, nb
    logical :: chercher_param_kep
    real(kind=PM_REEL) :: date_ref
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModeles => NULL()
    type(tm_orb_kep) :: param_kep
    character(LEN=CPS_MAXLG), dimension(2) :: mess
    character(LEN=256) :: unite
    integer :: ret

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    ! trouver le code du corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
    trouve = CPS_ERR_DEF
    do i=1, cpsi_size_ptr(fichiersCorps)
       call cpsi_getAccesMadona(fichiersCorps(i), acces)
       ! recherche avec corps = nom_id
       trouve = cps_getCritere(acces, "nom_id", corps, "code", code_corps)
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do

    chercher_param_kep = .true.

    if (trouve.eq.CPS_OK) then
       
       if (present(mu)) then
          trouve = cps_getCsteThCourante(code_corps, "mu", mu, unite)
          if (trouve.ne.CPS_OK) then
             mess(1) = "mu"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_bd", &
                  partie_variable=mess(1))
          end if
       end if

       if (present(requa)) then
          trouve = cps_getCsteThCourante(code_corps, "requa", requa, unite)
          if (trouve.ne.CPS_OK) then
             mess(1) = "requa"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_bd", &
                  partie_variable=mess(1))
          end if
       end if

       if (present(apla)) then
          trouve = cps_getCsteThCourante(code_corps, "apla", apla, unite)
          if (trouve.ne.CPS_OK) then
             mess(1) = "apla"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_bd", &
                  partie_variable=mess(1))
          end if
       end if
       
       if (present(J2)) then
          trouve = cps_getCsteThCourante(code_corps, "J2", J2, unite)
          if (trouve.ne.CPS_OK) then
             mess(1) = "J2"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_bd", &
                  partie_variable=mess(1))
          end if
       end if 

       if (present(vrot)) then
          trouve = cps_getCsteThCourante(code_corps, "vrot", vrot, unite)
          if (trouve.ne.CPS_OK) then
             mess(1) = "vrot"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_bd", &
                  partie_variable=mess(1))
          end if
       end if

       if (present(potentiel)) then
          ret = cps_getModelesPotentielCorps(code_corps, listeModeles)
          if (ret.eq.CPS_OK) then
             nb = cpsi_size_ptr(listeModeles)
             if (nb.gt.MAXCORPS) nb = MAXCORPS
             potentiel(1:nb) = listeModeles(1:nb)
          end if
          if (associated(listeModeles)) then
             deallocate(listeModeles, stat=iostat)
          end if
       end if

       if (present(atmosphere)) then
          ret = cps_getListEltsCorps(code_corps, "modele_atmosphere", &
               listeModeles)
          if (ret.eq.CPS_OK) then
             nb = cpsi_size_ptr(listeModeles)
             if (nb.gt.MAXCORPS) nb = MAXCORPS
             atmosphere(1:nb) = listeModeles(1:nb)
          end if
          if (associated(listeModeles)) then
             deallocate(listeModeles, stat=iostat)
          end if
       end if

       if (present(geometrie)) then
          ret = cps_getListEltsCorps(code_corps, "modele_geometrie", &
               listeModeles)
          if (ret.eq.CPS_OK) then
             nb = cpsi_size_ptr(listeModeles)
             if (nb.gt.MAXCORPS) nb = MAXCORPS
             geometrie(1:nb) = listeModeles(1:nb)
          end if
          if (associated(listeModeles)) then
             deallocate(listeModeles, stat=iostat)
          end if
       end if

       if (present(dga)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "dga"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          dga = param_kep%a
       end if

       if (present(exc)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "exc"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          exc = param_kep%e
       end if
       
       if (present(inc)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "inc"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          inc = param_kep%i
       end if
       
       if (present(pom)) then
          if (chercher_param_kep) then
             if (trouve.ne.CPS_OK) then
                mess(1) = "pom"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
          end if
          pom = param_kep%pom
       end if
       
       if (present(gom)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "gom"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          gom = param_kep%gom
       end if

       if (present(anm)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "anm"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          anm = param_kep%M
       end if

       if (present(dateref)) then
          if (chercher_param_kep) then
             trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
             if (trouve.ne.CPS_OK) then
                mess(1) = "dateref"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_bd", &
                     partie_variable=mess(1))
             end if
          end if
          dateref = date_ref
       end if

       if (present(corpsc)) then
         trouve = cps_getAtt(code_corps, "corpsc", code_corpsc) 
         if (trouve.ne.CPS_OK) then
            mess(1) = "corpsc"
            call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                 routine="cps_constante_bd", &
                 partie_variable=mess(1))
         end if
         trouve = cps_getAtt(code_corpsc, "nom_id", corpsc)
         if (trouve.ne.CPS_OK) then
            mess(1) = "corpsc"
            call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                 routine="cps_constante_bd", &
                 partie_variable=mess(1))
         end if
      end if

       if (present(typec)) then
          trouve = cps_getAtt(code_corps, "type", typec) 
          if (trouve.ne.CPS_OK) then
            mess(1) = "typec"
            call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                 routine="cps_constante_bd", &
                 partie_variable=mess(1))
         end if
       end if

       if (present(code)) then
          code = code_corps
       end if

    end if

    if (present(g)) then
       trouve = cps_getCsteGenThCourante("G", g, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "G"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_bd", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(vlum)) then
       trouve = cps_getCsteGenThCourante("vlum", vlum, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "vlum"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_bd", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(ua)) then
       trouve = cps_getCsteGenThCourante("ua", ua, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "ua"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_bd", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(coef_prs)) then
       trouve = cps_getCsteGenThCourante("coef_prs", coef_prs, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "coef_prs"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_bd", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(theorie)) then
       call cps_getTheorie(theorie)
    end if

    ! liberation memoire
    if(associated(fichiersCorps)) deallocate(fichiersCorps, stat=iostat)
    if(associated(listeModeles)) deallocate(listeModeles, stat=iostat)

  end subroutine cps_constante_bd


  subroutine cps_constante_tab(corps, nb, mu, requa, apla, J2, vrot, &
             dga, exc, inc,pom, gom, anm, dateref, &
             corpsc, typec, code, g, vlum, ua, coef_prs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante_tab
!
!$Resume
!  acces aux constantes pour un tableau de corps (theorie courante) obsolète
!
!$Description
!  Compatibilité COMPAS V1-5
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante_tab(corps, nb, [mu], [requa], [apla], [j2], [vrot], &
!.                 [dga], [exc], [inc],[pom], [gom], [anm], [dateref], &
!.                 [corpsc], [typec], [code], [g], [vlum], [ua], [coef_prs])
!.    character(LEN=*), dimension(nb) :: corps
!.    integer :: nb
!.    real(kind=PM_REEL), dimension(nb) :: mu, requa, apla, vrot
!.    real(kind=PM_REEL), dimension(nb) :: J2
!.    real(kind=PM_REEL), dimension(nb) :: dga, exc, inc, pom, gom, anm, dateref
!.    integer, dimension(nb) :: code
!.    character(LEN=*), dimension(nb) :: corpsc, typec
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!$Arguments
!>E     corps     :<LEN=*,DIM=(nb)>     
!>E     nb        :<integer>            
!>[S]   mu        :<PM_REEL,DIM=(nb)>   
!>[S]   requa     :<PM_REEL,DIM=(nb)>   
!>[S]   apla      :<PM_REEL,DIM=(nb)>   
!>[S]   J2        :<PM_REEL,DIM=(nb)>   
!>[S]   vrot      :<PM_REEL,DIM=(nb)>   
!>[S]   dga       :<PM_REEL,DIM=(nb)>   
!>[S]   exc       :<PM_REEL,DIM=(nb)>   
!>[S]   inc       :<PM_REEL,DIM=(nb)>   
!>[S]   pom       :<PM_REEL,DIM=(nb)>   
!>[S]   gom       :<PM_REEL,DIM=(nb)>   
!>[S]   anm       :<PM_REEL,DIM=(nb)>   
!>[S]   dateref   :<PM_REEL,DIM=(nb)>   
!>[S]   corpsc    :<LEN=*,DIM=(nb)>     
!>[S]   typec     :<LEN=*,DIM=(nb)>     
!>[S]   code      :<integer,DIM=(nb)>   
!>[S]   g         :<PM_REEL>            
!>[S]   vlum      :<PM_REEL>            
!>[S]   ua        :<PM_REEL>            
!>[S]   coef_prs  :<PM_REEL>            
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), dimension(nb), intent(in) :: corps
    integer, intent(in) :: nb
    real(kind=PM_REEL), dimension(nb), optional, intent(out) :: mu, requa, apla, vrot
    real(kind=PM_REEL), dimension(nb), optional, intent(out) :: J2
    real(kind=PM_REEL), dimension(nb), optional, intent(out) :: dga, exc, inc, pom, gom, anm, dateref
    integer, optional, dimension(nb), intent(out) :: code
    character(LEN=*), dimension(nb), optional, intent(out) :: corpsc, typec
    real(kind=PM_REEL), optional, intent(out) :: G, vlum, ua, coef_prs

    integer :: code_corps, trouve, acces, i, k, code_corpsc
    logical :: chercher_param_kep
    real(kind=PM_REEL) :: date_ref
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
    type(tm_orb_kep) :: param_kep
    character(LEN=CPS_MAXLG), dimension(2) :: mess
    character(LEN=256) :: unite

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)

    do k=1, nb
  
       ! trouver le code du corps
       trouve = CPS_ERR_DEF
       do i=1, cpsi_size_ptr(fichiersCorps)
          call cpsi_getAccesMadona(fichiersCorps(i), acces)
          ! recherche avec corps = nom_id
          trouve = cps_getCritere(acces, "nom_id", corps(k), "code", code_corps)
          if (trouve.eq.CPS_OK) then
             exit
          end if
       end do
       
       chercher_param_kep = .true.
       
       if (trouve.eq.CPS_OK) then
          
          if (present(mu)) then
             trouve = cps_getCsteThCourante(code_corps, "mu", mu(k), unite)
             if (trouve.ne.CPS_OK) then
                mess(1) = "mu"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(requa)) then
             trouve = cps_getCsteThCourante(code_corps, "requa", requa(k), unite)
             if (trouve.ne.CPS_OK) then
                mess(1) = "requa"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(apla)) then
             trouve = cps_getCsteThCourante(code_corps, "apla", apla(k), unite)
             if (trouve.ne.CPS_OK) then
                mess(1) = "apla"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(J2)) then
             trouve = cps_getCsteThCourante(code_corps, "J2", J2(k), unite)
             if (trouve.ne.CPS_OK) then
                mess(1) = "J2"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(vrot)) then
             trouve = cps_getCsteThCourante(code_corps, "vrot", vrot(k), unite)
             if (trouve.ne.CPS_OK) then
                mess(1) = "vrot"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(dga)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "dga"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             dga(k) = param_kep%a
          end if
          
          if (present(exc)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "exc"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             exc(k) = param_kep%e
          end if
          
          if (present(inc)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "inc"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             inc(k) = param_kep%i
          end if
          
          if (present(pom)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "pom"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                     partie_variable=mess(1))
                end if
             end if
             pom(k) = param_kep%pom
          end if
          
          if (present(gom)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "gom"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             gom(k) = param_kep%gom
          end if
          
          if (present(anm)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "anm"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             anm(k) = param_kep%M
          end if
          
          if (present(dateref)) then
             if (chercher_param_kep) then
                trouve = cps_getKeplerThCourante(code_corps, param_kep, date_ref)
                if (trouve.ne.CPS_OK) then
                   mess(1) = "dateref"
                   call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                        routine="cps_constante_tab", &
                        partie_variable=mess(1))
                end if
             end if
             dateref(k) = date_ref
          end if
          
          if (present(corpsc)) then
             trouve = cps_getAtt(code_corps, "corpsc", code_corpsc) 
             if (trouve.ne.CPS_OK) then
                mess(1) = "corpsc"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
             trouve = cps_getAtt(code_corpsc, "cnom_id", corpsc(k)) 
             if (trouve.ne.CPS_OK) then
                mess(1) = "corpsc"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(typec)) then
             trouve = cps_getAtt(code_corps, "type", typec(k)) 
             if (trouve.ne.CPS_OK) then
                mess(1) = "type"
                call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                     routine="cps_constante_tab", &
                     partie_variable=mess(1))
             end if
          end if
          
          if (present(code)) then
             code(k) = code_corps
          end if
          
       end if

    end do

    if (present(g)) then
       trouve = cps_getCsteGenThCourante("G", g, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "G"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(vlum)) then
       trouve = cps_getCsteGenThCourante("vlum", vlum, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "vlum"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(ua)) then
       trouve = cps_getCsteGenThCourante("ua", ua, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "ua"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(coef_prs)) then
       trouve = cps_getCsteGenThCourante("coef_prs", coef_prs, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "coef_prs"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if

    ! liberation memoire
    if(associated(fichiersCorps)) deallocate(fichiersCorps, stat=iostat)

  end subroutine cps_constante_tab


  ! 
  subroutine cps_path(corps, path, fichier, modele)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_path
!
!$Resume
!  Emplacement d'un modele (obsolète)
!
!$Description
!  Compatibilité COMPAS V1-5
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_path(corps, path, [fichier], [modele])
!.    character(LEN=*) :: corps
!.    character(LEN=*) :: path
!.    character(LEN=*) :: fichier
!.    character(LEN=*) :: modele
!
!$Arguments
!>E     corps    :<LEN=*>   
!>S     path     :<LEN=*>   
!>[E]   fichier  :<LEN=*>   
!>[E]   modele   :<LEN=*>   
!
!$Common
!
!$Routines
!- cps_init
!- cps_getListFichiersCateg
!- cpsi_getAccesMadona
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
    character(LEN=*), intent(in) :: corps
    character(LEN=*), intent(out) :: path
    character(LEN=*), intent(in), optional :: fichier
    character(LEN=*), intent(in), optional :: modele

    ! Variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
    integer :: trouve, acces, i, code_corps, num_nom
    logical :: existe
    character(LEN=CPS_MAXLG) :: mod_tmp, rep_tmp, mod
    character(LEN=CPS_MAXLG), dimension(3) :: nom
    integer :: ret
    type(cpsi_desc), dimension(:), pointer :: listeFichiers => NULL()

    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    ! trouver le code du corps
    call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
    trouve = CPS_ERR_DEF
    do i=1, cpsi_size_ptr(fichiersCorps)
       call cpsi_getAccesMadona(fichiersCorps(i), acces)
       ! recherche avec corps = nom_id
       trouve = cps_getCritere(acces, "nom_id", corps, "code", code_corps)
       if (trouve.eq.CPS_OK) then
          exit
       end if
    end do

    if (trouve.ne.CPS_OK) then
       ! corps non present dans la base
       !call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
       !     routine="cps_path", &
       !     "corps absent")
    end if

    call cpsi_getListeDescFichiers(listeFichiers)

    if (present(modele).and.(trouve.eq.CPS_OK)) then
       
       ! recherche de la premiere occurence de 'modele_<modele>'
       ! dans une structure ou code = code_corps
       
       ! modele peut valoir : atmosphere, potentiel, geometrie
       ! on met le modele en minuscules
       ! en effet, certains programme font des appels avec Atmosphere
       mod = trim(cpsi_getMinuscules(modele))
       trouve = CPS_ERR_DEF
       
       do i=1, cpsi_size_ptr(listeFichiers)
          if (trouve.eq.CPS_OK) exit

          !trouve = cpsi_getDescFichier(listeFichiers(i)%fichier, desc)
          existe = cpsi_existeAttDesc("modele_"//trim(mod), listeFichiers(i))
          if (existe) then
             trouve = CPS_ERR_DEF
             call cpsi_getAccesMadona(listeFichiers(i)%fichier, acces)
             ret = cps_getCritere(acces, "code", code_corps, &
                  "modele_"//trim(mod), mod_tmp)
             if (ret.eq.CPS_OK) then
                if (present(fichier)) then
                   trouve = cps_getCritere(acces, &
                        "fichier_"//trim(mod), fichier, &
                        "repertoire_"//trim(mod), rep_tmp)
                   if (trouve.eq.CPS_OK) then
                      rep_tmp = trim(cpsi_getRep(rep_tmp))
                      path = trim(rep_tmp)//"/"//trim(fichier)
                      path = trim(rep_base_ref)//"/"//trim(path)
                   end if
                else
                   trouve = CPS_OK
                   ret = cps_getCritere(acces, "code", code_corps, &
                        "repertoire_"//trim(mod), rep_tmp)
                   path = trim(cpsi_getRep(rep_tmp))
                   path = trim(rep_base_ref)//"/"//trim(path)
                end if
             end if
          else
             trouve = CPS_ERR_DEF
          end if
       end do
       
    end if

    if (present(fichier).and.&
         (.not.present(modele)).and.&
         (trouve.eq.CPS_OK)) then

       nom(1) = "potentiel"
       nom(2) = "atmosphere"
       nom(3) = "geometrie"

       do num_nom=1, 3
          trouve = CPS_ERR_DEF
          do i=1, cpsi_size_ptr(listeFichiers)
             if (trouve.eq.CPS_OK) exit
             
             existe = cpsi_existeAttDesc("modele_"//trim(nom(num_nom)), listeFichiers(i))
             if (existe) then
                trouve = CPS_ERR_DEF
                call cpsi_getAccesMadona(listeFichiers(i)%fichier, acces)
                ret = cps_getCritere(acces, "code", code_corps, &
                     "modele_"//trim(nom(num_nom)), mod_tmp)
                if (ret.eq.CPS_OK) then
                   trouve = cps_getCritere(acces, &
                        "fichier_"//trim(nom(num_nom)), fichier, &
                        "repertoire_"//trim(nom(num_nom)), rep_tmp)
                   if (trouve.eq.CPS_OK) then
                      rep_tmp = trim(cpsi_getRep(rep_tmp))
                      path = trim(rep_tmp)//"/"//trim(fichier)
                      path = trim(rep_base_ref)//"/"//trim(path)
                   end if
                end if
             else
                trouve = CPS_ERR_DEF
             end if
          end do
          
          if (trouve.eq.CPS_OK) then
             exit
          end if
          
       end do
       
    end if

    ! liberation memoire
    if (associated(fichiersCorps)) then
       deallocate(fichiersCorps, stat=iostat)
    end if

    call cpsi_deallocateDescFichiers(listeFichiers)

  end subroutine cps_path

  ! Obtenir des attributs d'un corps en fonction de son code
  subroutine cps_constante_code(corps, mu, requa, apla, J2, vrot, &
       potentiel, atmosphere, geometrie, dga, exc, inc, pom, gom, anm, dateref, &
       corpsc, typec, code, g, vlum, ua, coef_prs, theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante_code
!
!$Resume
! Obtenir des attributs d'un corps en fonction de son code (obsolète)
!
!$Description
!  Compatibilité COMPAS V1-5
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante_code(corps, [mu], [requa], [apla], [j2], [vrot], &
!.           [potentiel], [atmosphere], [geometrie], [dga], [exc], [inc], [pom], [gom], [anm], [dateref], &
!.           [corpsc], [typec], [code], [g], [vlum], [ua], [coef_prs], [theorie])
!.    integer :: corps
!.    real(kind=PM_REEL) :: mu, requa, apla, J2, vrot
!.    real(kind=PM_REEL) :: dga, exc, inc, pom, gom, anm, dateref
!.    character(LEN=*), dimension(:) :: potentiel
!.    character(LEN=*), dimension(:) :: atmosphere
!.    character(LEN=*), dimension(:) :: geometrie
!.    integer :: code
!.    character(LEN=*) :: corpsc, typec, theorie
!.    real(kind=PM_REEL) :: G, vlum, ua, coef_prs
!
!$Arguments
!>E     corps       :<integer>         
!>[S]   mu          :<PM_REEL>         
!>[S]   requa       :<PM_REEL>         
!>[S]   apla        :<PM_REEL>         
!>[S]   J2          :<PM_REEL>         
!>[S]   vrot        :<PM_REEL>         
!>[S]   potentiel   :<LEN=*,DIM=(:)>   
!>[S]   atmosphere  :<LEN=*,DIM=(:)>   
!>[S]   geometrie   :<LEN=*,DIM=(:)>   
!>[S]   dga         :<PM_REEL>         
!>[S]   exc         :<PM_REEL>         
!>[S]   inc         :<PM_REEL>         
!>[S]   pom         :<PM_REEL>         
!>[S]   gom         :<PM_REEL>         
!>[S]   anm         :<PM_REEL>         
!>[S]   dateref     :<PM_REEL>         
!>[S]   corpsc      :<LEN=*>           
!>[S]   typec       :<LEN=*>           
!>[S]   code        :<integer>         
!>[S]   g           :<PM_REEL>         
!>[S]   vlum        :<PM_REEL>         
!>[S]   ua          :<PM_REEL>         
!>[S]   coef_prs    :<PM_REEL>         
!>[S]   theorie     :<LEN=*>           
!
!$Common
!
!$Routines
!- cps_init
!- MSP_signaler_message
!- cps_getTheorie
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
    integer, intent(in) :: corps
    real(kind=PM_REEL), optional, intent(out) :: mu, requa, apla, J2, vrot
    real(kind=PM_REEL), optional, intent(out) :: dga, exc, inc, pom, gom, anm, dateref
    character(LEN=*), optional, dimension(:), intent(out) :: potentiel
    character(LEN=*), optional, dimension(:), intent(out) :: atmosphere
    character(LEN=*), optional, dimension(:), intent(out) :: geometrie
    integer, optional, intent(out) :: code
    character(LEN=*), optional, intent(out) :: corpsc, typec, theorie
    real(kind=PM_REEL), optional, intent(out) :: G, vlum, ua, coef_prs

    integer :: trouve, code_corpsc, nb
    logical :: chercher_param_kep
    real(kind=PM_REEL) :: date_ref
    type(tm_orb_kep) :: param_kep
    character(LEN=CPS_MAXLG), dimension(2) :: mess
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeModeles => NULL()
    character(LEN=256) :: unite
    integer :: ret              ! Code de retour

    chercher_param_kep = .true.
       
    ! initialisation
    if (.not.cps_var_init) then
       call cps_init()
    end if

    if (present(mu)) then
       trouve = cps_getCsteThCourante(corps, "mu", mu, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "mu"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(requa)) then
       trouve = cps_getCsteThCourante(corps, "requa", requa, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "requa"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(apla)) then
       trouve = cps_getCsteThCourante(corps, "apla", apla, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "apla"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(J2)) then
       trouve = cps_getCsteThCourante(corps, "J2", J2, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "J2"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(vrot)) then
       trouve = cps_getCsteThCourante(corps, "vrot", vrot, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "vrot"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(potentiel)) then
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       ret = cps_getModelesPotentielCorps(corps, listeModeles)
       if (ret.eq.CPS_OK) then
          nb = cpsi_size_ptr(listeModeles)
          if (nb.gt.size(potentiel)) nb = size(potentiel)
          potentiel(1:nb) = listeModeles(1:nb)
       end if
    end if
    
    if (present(atmosphere)) then
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       ret = cps_getListEltsCorps(corps, "modele_atmosphere", &
            listeModeles)
       if (ret.eq.CPS_OK) then
          nb = cpsi_size_ptr(listeModeles)
          if (nb.gt.size(atmosphere)) nb = size(atmosphere)
          atmosphere(1:nb) = listeModeles(1:nb)
       end if
    end if
    
    if (present(geometrie)) then
       if (associated(listeModeles)) then
          deallocate(listeModeles, stat=iostat)
       end if
       ret = cps_getListEltsCorps(corps, "modele_geometrie", &
            listeModeles)
       if (ret.eq.CPS_OK) then
          nb = cpsi_size_ptr(listeModeles)
          if (nb.gt.size(geometrie)) nb = size(geometrie)
          geometrie(1:nb) = listeModeles(1:nb)
       end if
    end if
    
    if (present(dga)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "dga"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       dga = param_kep%a
    end if
    
    if (present(exc)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "exc"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       exc = param_kep%e
    end if
    
    if (present(inc)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "inc"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       inc = param_kep%i
    end if
    
    if (present(pom)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "pom"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       pom = param_kep%pom
    end if
    
    if (present(gom)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "gom"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       gom = param_kep%gom
    end if
    
    if (present(anm)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "anm"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       anm = param_kep%M
    end if
    
    if (present(dateref)) then
       if (chercher_param_kep) then
          trouve = cps_getKeplerThCourante(corps, param_kep, date_ref)
          if (trouve.ne.CPS_OK) then
             mess(1) = "dateref"
             call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
                  routine="cps_constante_code", &
                  partie_variable=mess(1))
          end if
       end if
       dateref = date_ref
    end if
    
    if (present(corpsc)) then
       trouve = cps_getAtt(corps, "corpsc", code_corpsc) 
       if (trouve.ne.CPS_OK) then
          mess(1) = "corpsc"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
       trouve = cps_getAtt(code_corpsc, "nom_id", corpsc) 
       if (trouve.ne.CPS_OK) then
          mess(1) = "corpsc"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(typec)) then
       trouve = cps_getAtt(corps, "type", typec) 
       if (trouve.ne.CPS_OK) then
          mess(1) = "typec"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(code)) then
       code = corps
    end if
     
    if (present(g)) then
       trouve = cps_getCsteGenThCourante("G", g, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "G"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(vlum)) then
       trouve = cps_getCsteGenThCourante("vlum", vlum, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "vlum"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(ua)) then
       trouve = cps_getCsteGenThCourante("ua", ua, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "ua"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(coef_prs)) then
       trouve = cps_getCsteGenThCourante("coef_prs", coef_prs, unite)
       if (trouve.ne.CPS_OK) then
          mess(1) = "coef_prs"
          call MSP_signaler_message(cle_mes="CPS_ERR_INFO", & 
               routine="cps_constante_code", &
               partie_variable=mess(1))
       end if
    end if
    
    if (present(theorie)) then
       call cps_getTheorie(theorie)
    end if
    
    if (associated(listeModeles)) deallocate(listeModeles, stat=iostat)


  end subroutine cps_constante_code




end module cps_acces
