module eph_info

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_info
!
!$Resume
!  Module d'accès aux informations
!
!$Description
!  Les sous-programmes de ce module permettent d'extraire
!  différents types d'information du fichier de configuration
!  ephem.conf.
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema)   
!
!$Version
!  $Id: eph_info.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_info.F90,v $
!  Revision 1.32  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.31  2009/08/26 15:16:49  cml
!  AQ : Correction du numéro de FA dans l'historique
!
!  Revision 1.30  2009/08/26 15:04:59  cml
!  FA-ID 1315 : Construction du path complet dans le cas de la base locale
!  Revision 1.29  2008/10/28 12:44:37  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!  Revision 1.28  2008/10/15 15:01:50  tanguyy
!  DM-ID 1058 : Le fichier ephemerides de la base locale n'est lu que s'il est trouvé..
!  Revision 1.27  2008/10/08 15:46:16  cml
!  FA-ID 1062 : Ajout de compteurs afin d interrompre une boucle potentiellement infinie
!  Revision 1.26  2008/10/07 14:14:12  cml
!  FA-ID 1067 : Remplacement des appels au champs fic_taituc obsolete
!  Revision 1.25  2008/10/03 13:35:40  cml
!  FA-ID 1024 : Correction de nommage des variables (min, max, index, et mod)
!  Revision 1.24  2008/10/01 17:40:04  tanguyy
!  FA-ID 1007 : suppression de code mort, et retour (return) lorsqu'un message d'erreur est émis
!  Revision 1.23  2008/08/04 13:27:02  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration. Ajout des gestions d'erreur des fonctions d'accès aux fichiers
!  madona. Initialisation à 0 du code retour dans la routine eph_infoclose.
!  Revision 1.22  2007/10/26 15:10:42  huec
!  FA-ID 766 : Lecture des fichiers de TCHEBYCHEV dans la base locale pour GS_SYSTEME_REF
!  Revision 1.21  2007/05/21 07:02:51  vivaresf
!  Version 2.2 : révision des cartouches
!  Revision 1.20  2006/10/25 15:07:01  vpg
!  DM-ID 462 : correction de eph_lire_ephem_corps
!  Revision 1.19  2006/10/24 15:08:17  mle
!  MAJ : suppression de commentaires inutiles
!  Revision 1.18  2006/10/24 14:57:13  mle
!  DM-ID 462 : amelioration de l'acces a la description du contenu de la base COMPAS
!  Revision 1.17  2006/10/18 09:52:13  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.15.2.1  2006/09/26 12:12:41  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.15  2006/07/04 16:14:22  vivaresf
!  Passage understabnd
!  Revision 1.14  2006/07/03 13:51:13  vpg
!  Correction qualite : reduction du nombre d'imbrications
!  Revision 1.13  2006/07/03 08:53:54  vpg
!  Prise en compte des fichiers Tchebychev dans une base locale
!  Revision 1.12  2006/06/28 16:09:13  vpg
!  Les fichiers Tchebychev pour les calculs d'ephemerides sont recherches dans la base locale
!  Revision 1.11  2006/05/30 12:29:06  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.10  2006/05/30 08:23:47  vivaresf
!  Decoupage en COMPAS BASE et COMPAS UI
!  Revision 1.9  2006/04/18 09:52:11  vivaresf
!  Calculs synthetique
!  Revision 1.8  2006/04/06 16:02:17  vpg
!  Mise a jour de la lecture du code et de la date de reference du repere
!  Revision 1.7  2006/03/22 13:27:27  vpg
!  Le fichier contenant la liste des codes est extrait grace a cps_getListFichiersCateg()
!  Revision 1.6  2006/01/23 13:43:47  bouillaj
!  DM-ID 387 : Suppression de la metheode EPROC
!  Revision 1.5  2005/12/08 18:39:03  vivaresf
!  Cartouches et vérification des déclarations
!  Revision 1.4  2005/12/08 11:50:38  vivaresf
!  Initialisations et coherence d'algo
!  Revision 1.3  2005/12/08 08:59:43  vpg
!  Suppression des commentaires inutiles
!  Revision 1.2  2005/12/07 10:17:06  vpg
!  mise a jour variable de sortie pour eph_infocode() et eph_lire_ephem_corps()
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.8  2005/11/07 15:56:17  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.7  2005/10/13 10:38:00  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6.2.1  2005/10/13 10:29:31  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6  2005/05/09 14:17:32  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.5  2005/03/09 09:12:03  vivaresf
!  Correction des cartouches
!  Revision 1.4  2005/01/12 12:44:43  vivaresf
!  FA-ID 327 : commande plus rigoureuse
!  Revision 1.3  2004/12/17 14:41:52  vivaresf
!  FA-ID 327 : initialisation de la LIBEPHEM et cloture des ficheirs ouverts
!  Revision 1.2  2004/05/25 09:55:25  vivaresf
!  Rajout du prefixe ephv_ devant les constantes ephempath et fichierconf
!  Gestion des unités
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.22  2004/01/13 09:36:09  bremard
!  Suppression de commentaires obsolètes
!  Revision 1.19  2004/01/12 16:51:25  bremard
!  Remplacement variable rephome par reptmp
!  Revision 1.17  2004/01/09 16:20:01  bremard
!  Mise à jour des cartouches
!  Revision 1.16  2004/01/09 16:05:53  bremard
!  module msp_gestion_erreur remplace MECASPA
!  Revision 1.15  2004/01/08 11:10:19  bremard
!  Ajout arguments optionnels pour ephui_corps_in_fic
!  Revision 1.14  2004/01/07 16:22:04  bremard
!  maj cartouche + ajout ephui_corps_in_fic
!  Revision 1.13  2003/12/30 16:15:13  bremard
!  Ajout de fonctions d'informations + algo sans GOTO
!  Revision 1.12  2003/06/06 14:52:12  bremard
!  Particularisation des parametres
!  Revision 1.11  2002/09/26 15:29:33  vivaresf
!  bug en cas de depassement de la boucle
!  Revision 1.10  2001/12/18 16:15:01  vivaresf
!  Maj documentation
!  Revision 1.9  2001/12/07 16:46:25  vivaresf
!  Presentation fm des cartouches
!  Revision 1.8  2001/12/06 09:37:50  vivaresf
!  Maj doc
!  Revision 1.7  2001/12/05 13:49:20  bremard
!  PhB - Mise à jour des cartouches
!  Revision 1.5  2001/11/29 16:15:23  vivaresf
!  nom ddu fichier de ressources
!  Revision 1.4  2001/11/29 09:24:57  vivaresf
!  syntaxe
!  Revision 1.3  2001/11/28 13:33:17  bremard
!  PhB - Passage des constantes dans eph_varglob
!  Revision 1.2  2001/11/28 11:31:47  vivaresf
!  Gestion d'erreur sur la lecture du fichier de conf
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Usage
!  use eph_info
!
!$Global
!
!>  ephv_acces           : <integer>                pointer de la zone d'accès aux informations
!>  ephv_fichierconf     : <LEN=ephv_lgmax,public>  fichier de configuration
!>  ephv_sousrep_ephem   : <LEN=ephv_lgmax,public>  sous répertoire où se trouvent les éphémérides
!>  ephv_ephempath       : <LEN=ephv_lgmax,public>  répertoire ou se trouve ephv_fichierconf
!>  ephv_acces_open      : <logical,public>         T: ephv_fichierconf lu dans acces
!$Remarques
!
!$Routines
!- eph_infoinit
!- eph_infoget
!- eph_infogetLocal
!- eph_infocodes
!- eph_infoclose
!- eph_lire_ephem_corps
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_util
!- eph_varglob
!- cps_acces
!#
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_infoinit eph_infoget eph_infogetLocal eph_infocodes eph_infoclose eph_lire_ephem_corps
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur
  use eph_util
  use eph_varglob
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_info.F90 69 2012-09-11 08:33:34Z ffsm $'


! variables permanentes pour la zone d'acces MADONA 
      integer :: ephv_acces
      character(len=ephv_lgmax), public :: ephv_fichierconf, ephv_sousrep_ephem, ephv_ephempath
      logical, public :: ephv_acces_open=.false.

contains

  subroutine eph_infoinit()

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_infoinit
!
!$Resume
!  Ouverture du fichier de configuration éphémérides
!
!$Description
!   Lit le nom et l'emplacement du fichier de conf. dans le 
!   fichier de ressources et l'ouvre.
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!  Madona, ressources
!
!$Usage
!  call eph_infoinit()
!
!$Acces
!  PUBLIC
!
!$Remarques
!  Routine cachée, appelée au premier appel de eph_infoget 
!
!$Voir-Aussi
!  eph_infoget, eph_infoclose, eph_infocodes
!
!$<>
!***********************************************************************

    implicit none
    ! parametres
    
    ! variables locales
    integer ::  iret, acces_ref, iostat, ii
    character(len=ephv_lgmax) :: ficconf    
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    integer :: trouve  ! Flag indiquant si le fichier de tuc a été trouvé
    character (len=CPS_MAXLG), dimension(2) :: msp_mess

    if (.not.cps_var_init) call cps_init()

    ! lecture du fichier de ressources
    ! récuperer le fichier décrivant la configuration Ephémérides
    call cpsi_getAccesMadona(fichier_conf_ref, acces_ref)
    if(acces_ref.le.0) then
       call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
            partie_variable="de la base de reference", &
            routine="eph_infoinit" )
       return
    end if
    iret = acc_gets(acces_ref, "ephem_path", ephv_sousrep_ephem)
    if(iret.lt.0) then
       write(msp_mess(1),*) iret
       write(msp_mess(2),*) "ephem_path"
       call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
               routine="eph_infoinit", &
               partie_variable=msp_mess)
       return
    end if
    ! Suppression d'un slash final en trop
    ii=len_trim(ephv_sousrep_ephem)
    if ( index(ephv_sousrep_ephem,"/",back=.true.) == ii ) &
         ephv_sousrep_ephem = ephv_sousrep_ephem(1:ii-1)
    ! Constrcution du chemin complet vers les Ephemerides de la base de référence
    ephv_ephempath = trim(rep_base_ref)//"/"//trim(ephv_sousrep_ephem)
    if(iret.lt.0) then
       call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
            partie_variable="de configuration éphémérides", &
            routine="eph_infoinit" )
       return
    end if
    ficconf = ""
    do ii=1,cpsi_size_ptr(listFichiersRef)
       if (listFichiersRef(ii)%categorie=="ephemerides") then
          ficconf = trim(listFichiersRef(ii)%fichier)
          exit
       end if
    end do
    if (ficconf=="") then
       call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
            partie_variable="de configuration éphémérides", &
            routine="eph_infoinit" )
       return
    end if

    ! fichier de sauts de TUC
    call cps_getFichierTaituc(fictuc, trouve)
    if ( MSP_ERREUR .or. trouve /= CPS_OK ) then
       call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
            partie_variable="de sauts de TUC", &
            routine="eph_infoinit" )
       return
    else
       initficsauttuc=.true.
    end if
    
    ! ouverture du fichier de configuration
    ! fichier de configuration éphémérides
    ! ouvrir le fichier 
    ephv_acces = acc_load(ficconf)
    if(ephv_acces.le.0) then
       call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
            partie_variable="de configuration éphémérides", &
            routine="eph_infoinit" )
    end if
    
    if (ephv_acces.gt.0) ephv_acces_open = .true.

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers, stat=iostat)
    end if

  end subroutine eph_infoinit

  subroutine eph_infoget(code, repertoire, fichierd, methode, theorie, trouve)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_infoget
!
!$Resume
!     Informations concernant un code méthode/théorie
!
!$Description
!     Pour un code méthode/théorie donné, retourne le répertoire associé,
!     le fichier par défaut, le nom de la méthode, la théorie associée. 
!
!$Auteur
!     Florence VIVARES (SchlumbergerSema)!$Version
!
!$Mots-cles
!     Ephémérides
!
!$Usage
!  call eph_infoget(code, [repertoire], [fichierd], [methode], [theorie], [trouve])
!.    integer :: code
!.    character(len=*) :: fichierd, repertoire, methode, theorie
!.    logical :: trouve
!
!$Arguments
!>E     code        :<integer>   code méthode/théorie 
!>[E/S] repertoire  :<LEN=*>     répertoire où trouver les fichiers
!>[E/S] fichierd    :<LEN=*>     fichier par défaut
!>[E/S] methode     :<LEN=*>     nom de la méthode (BDL, NAIF, TCHE, TCHEmad,
!                                EXTRAPOLATION)
!>[E/S] theorie     :<LEN=*>     nom de la théorie (VSOP82, VSOP87, DE200,
!                                DE403, DE405, ...)
!>[S]   trouve      :<logical>   permet de tester si le code a ete trouve dans
!                                la base de reference
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!      eph_infoget, eph_infoclose
!      libui (ui_epheminfo)
!
!$<>
!***********************************************************************

    use eph_varglob, only : EPHV_NB_MAX_STRUCT_FICHIER
    implicit none
! parametres
    integer,intent(in) :: code
    character(len=*),intent(inout), optional :: fichierd, repertoire, methode, theorie
    logical, intent(out), optional :: trouve

! variables locales
    integer :: iret, icode
    character(len=4) :: codes
    integer :: nature
    character(LEN=CPS_MAXLG) :: libelle
    logical :: code_existe
    character (len=CPS_MAXLG), dimension(3) :: msp_mess
    logical :: fin_fichier   ! Flag indiquant la fin du fichier
    integer :: nb_champs_lus ! Nombre de champs lus dans le fichier

! ouverture du fichier de conf si besoin
    code_existe = .false.
    if(.not.ephv_acces_open) then
       call eph_infoinit()
       if ( MSP_gen_messages("eph_infoget") ) return
    endif

    ! Attention : eph_infoinit peut modifier ephv_acces_open
    if (ephv_acces_open) then

       iret = acc_scan_reset(ephv_acces)
       if (iret<0) then
          write(msp_mess(1),*) iret
          write(msp_mess(2),*) "acc_scan_reset"
          call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
               routine="eph_infoget",                      &
               partie_variable=msp_mess)
       endif

       ! Parcours jusqu'à atteindre la fin du fichier
       ! ou la limite algorithmique fixé à EPHV_NB_MAX_STRUCT_FICHIER
       nb_champs_lus = 0
       fin_fichier = .false.
       do while ( .not. code_existe &
            .and. .not.fin_fichier &
            .and. nb_champs_lus <= EPHV_NB_MAX_STRUCT_FICHIER)

          iret = acc_scan(ephv_acces, libelle, nature)
          if (iret<0) then
             write(msp_mess(1),*) iret
             write(msp_mess(2),*) "acc_scan"
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                  routine="eph_infoget",                      &
                  partie_variable=msp_mess)
          endif

          if (nature.eq.CPS_FIN_SEQ) then
             fin_fichier = .true.
          elseif (nature.eq.ACC_STRUCT) then
             iret = acc_select(ephv_acces, libelle, ACC_STRUCT)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infoget",                      &
                     partie_variable=msp_mess)
             endif
             ! Extraction du code de la structure courrante
             iret = acc_geti(ephv_acces, "code", icode)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "code"
                write(msp_mess(3),*) trim(fichier_conf_ref)
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                     routine="eph_infoget",                          &
                     partie_variable=msp_mess)
             endif
             ! Si le code correspond, extraction du chemin d'accès complet
             if(icode.eq.code) then
                code_existe = .true.
                if(present(repertoire)) then
                   iret=acc_gets(ephv_acces, "repertoire", repertoire)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "repertoire"
                      write(msp_mess(3),*) trim(fichier_conf_ref)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infoget",                          &
                           partie_variable=msp_mess)
                   endif
                   ! on ajoute le répertoire de la base locale
                   repertoire = trim(ephv_ephempath)//"/"//trim(repertoire)
                endif
                if(present(fichierd)) then
                   iret=acc_gets(ephv_acces, "fichier", fichierd)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "fichier"
                      write(msp_mess(3),*) trim(fichier_conf_ref)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infoget",                          &
                           partie_variable=msp_mess)
                   endif
                endif
                if(present(methode)) then
                   iret=acc_gets(ephv_acces, "methode", methode)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "methode"
                      write(msp_mess(3),*) trim(fichier_conf_ref)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infoget",                          &
                           partie_variable=msp_mess)
                   endif
                endif
                if(present(theorie)) then
                   iret=acc_gets(ephv_acces, "theorie", theorie)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "theorie"
                      write(msp_mess(3),*) trim(fichier_conf_ref)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infoget",                          &
                           partie_variable=msp_mess)
                   endif
                endif
             end if ! Fin si le code correspond
             iret = acc_select_end(ephv_acces)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select_end"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infoget",                      &
                     partie_variable=msp_mess)
             endif

             ! Mise a jour du compteur de champs
             nb_champs_lus = nb_champs_lus +1

          end if !Fin si structure
       end do ! Fin du parcours du fichier des ephemerides

       ! Traitement du cas où l'on est sorti de la boucle trop tôt
       if (nb_champs_lus > EPHV_NB_MAX_STRUCT_FICHIER) then
          call MSP_signaler_message(cle_mes="EPH_ERR_LECT_CHAMPS_FICHIER", &
             partie_variable="courant", &
             routine="eph_infoget")
          return
       endif

       if(.not.code_existe) then 
          ! Erreur methode inconnue
          write(codes, *) code
          call MSP_signaler_message(cle_mes="EPH_ERR_CODEMETHODE", &
                                    partie_variable=codes, &
                                    routine="eph_info")
          if (present(trouve)) then
             trouve = .false.
          endif
       else if (present(trouve)) then
          trouve = .true.
       endif

    endif

  end subroutine eph_infoget


subroutine eph_infogetLocal(code, code_existe, repertoire, fichierd, methode, theorie)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_infogetLocal
!
!$Resume
!     Informations concernant un code méthode/théorie dans la base locale.
!     Cette routine ne doit être appelée que pour des fichiers Tchebychev.
!
!$Description
!     Pour un code méthode/théorie donné, retourne le répertoire associé,
!     le fichier par défaut, le nom de la méthode, la théorie associée.
!     A la difference de eph_infoget(), les informations sont recherchees dans la base
!     locale.
!     Cette routine ne doit être appelée que pour des fichiers Tchebychev.
!
!$Auteur
!     vpg
!
!$Mots-cles
!     Ephémérides
!
!$Usage
!  call eph_infogetLocal(code, code_existe, [repertoire], [fichierd], [methode], [theorie])
!.    integer :: code
!.    logical :: code_existe
!.    character(len=*) :: fichierd, repertoire, methode, theorie
!
!$Arguments
!>E     code         :<integer>   code méthode/théorie 
!>S     code_existe  :<logical>   .true. si le code est présent dans la base locale, .false. sinon
!>[E/S] repertoire   :<LEN=*>     répertoire où trouver les fichiers
!>[E/S] fichierd     :<LEN=*>     fichier par défaut
!>[E/S] methode      :<LEN=*>     nom de la méthode (BDL, NAIF, TCHE, TCHEmad,
!                                EXTRAPOLATION)
!>[E/S] theorie      :<LEN=*>     nom de la théorie (VSOP82, VSOP87, DE200,
!                                DE403, DE405, ...)
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!      eph_infoget, eph_infoclose
!      libui (ui_epheminfo)
!
!$<>
!***********************************************************************
    use eph_varglob, only : EPHV_NB_MAX_STRUCT_FICHIER
    implicit none
    
    ! arguments
    integer,intent(in) :: code
    logical, intent(out) ::  code_existe
    character(len=*),intent(inout), optional :: fichierd, repertoire, methode, theorie

    ! variables locales
    integer :: eph_acces_local, ii
    integer :: iret, icode
    integer :: nature
    character(LEN=CPS_MAXLG) :: libelle, eph_fic_local
    character (len=CPS_MAXLG), dimension(3) :: msp_mess
    logical :: fin_fichier   ! Flag indiquant la fin du fichier
    integer :: nb_champs_lus ! Nombre de champs lus dans le fichier

    ! initialisation
    code_existe = .false.
    if(.not.ephv_acces_open) then
       call eph_infoinit()
       if ( MSP_gen_messages("eph_infogetLocal") ) return
    endif

    ! eph_acces_local est initialisé à -1 
    ! cette variable prendra une valeur > 0 une fois que le fichier
    ! descripteur des ephemerides aura été trouvé
    eph_acces_local = -1

    ! Ouverture du fichier de la table 'ephemerides' de la base locale
    if (.not.associated(listFichiersLocal)) then
       ! pas de base locale
       return
    end if
    do ii=1,cpsi_size_ptr(listFichiersLocal)
       !desc = listFichiersLocal(ii)
       if (listFichiersLocal(ii)%categorie=="ephemerides") then          
          eph_fic_local = trim(listFichiersLocal(ii)%fichier)
          call cpsi_getAccesMadona(trim(eph_fic_local),eph_acces_local)
          if (eph_acces_local<0) then
             ! echec de l'ouverture du fichier
             call MSP_signaler_message(cle_mes="CPS_ERR_OPEN", &
                  routine="eph_infogetLocal",                  &
                  partie_variable=trim(eph_fic_local))
             return
          end if
          ! sortie de boucle car le fichier descripteur est trouvé
          exit
       end if
    end do

    ! Si eph_acces_local /= -1, c'est à dire si un fichier descripteur est trouvé
    ! on fait la lecture des ephemerides de la base locale
    if (eph_acces_local /= -1) then

       iret = acc_scan_reset(eph_acces_local)
       if (iret<0) then
          write(msp_mess(1),*) iret
          write(msp_mess(2),*) "acc_scan_reset"
          call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
               routine="eph_infogetLocal",                 &
               partie_variable=msp_mess)
       endif

       ! Parcours jusqu'à atteindre la condition d'arret
       ! ou jusqu'à la fin du fichier
       ! ou la limite algorithmique fixé à EPHV_NB_MAX_STRUCT_FICHIER
       nb_champs_lus = 0
       fin_fichier = .false.
       do while ( .not. code_existe &
            .and. .not. fin_fichier &
            .and. nb_champs_lus <= EPHV_NB_MAX_STRUCT_FICHIER)

          iret = acc_scan(eph_acces_local, libelle, nature)
          if (iret<0) then
             write(msp_mess(1),*) iret
             write(msp_mess(2),*) "acc_scan"
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                  routine="eph_infogetLocal",                 &
                  partie_variable=msp_mess)
          endif

          if (nature.eq.CPS_FIN_SEQ) then
             fin_fichier = .true.
          elseif (nature.eq.ACC_STRUCT) then   
             iret = acc_select(eph_acces_local, libelle, ACC_STRUCT)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infogetLocal",                 &
                     partie_variable=msp_mess)
             endif
             iret = acc_geti(eph_acces_local, "code", icode)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "code"
                write(msp_mess(3),*) trim(listFichiersLocal(ii)%fichier)
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                     routine="eph_infogetLocal",                     &
                     partie_variable=msp_mess)
             endif
             if(icode.eq.code) then
                code_existe = .true.
                if(present(repertoire)) then
                   iret=acc_gets(eph_acces_local, "repertoire", repertoire)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "repertoire"
                      write(msp_mess(3),*) trim(listFichiersLocal(ii)%fichier)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infogetLocal",                     &
                           partie_variable=msp_mess)
                   endif                   
                   ! on ajoute le répertoire de la base locale
                   repertoire = trim(rep_base_local)//"/"//&
                        trim(ephv_sousrep_ephem)//"/"//trim(repertoire)
                end if
                if(present(fichierd)) then
                   iret=acc_gets(eph_acces_local, "fichier", fichierd)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "fichier"
                      write(msp_mess(3),*) trim(listFichiersLocal(ii)%fichier)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infogetLocal",                     &
                           partie_variable=msp_mess)
                   endif
                endif
                if(present(methode)) then
                   iret=acc_gets(eph_acces_local, "methode", methode)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "methode"
                      write(msp_mess(3),*) trim(listFichiersLocal(ii)%fichier)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infogetLocal",                     &
                           partie_variable=msp_mess)
                   endif
                endif
                if(present(theorie)) then
                   iret=acc_gets(eph_acces_local, "theorie", theorie)
                   if (iret<0) then
                      write(msp_mess(1),*) iret
                      write(msp_mess(2),*) "theorie"
                      write(msp_mess(3),*) trim(listFichiersLocal(ii)%fichier)
                      call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                           routine="eph_infogetLocal",                     &
                           partie_variable=msp_mess)
                   endif
                endif
             end if ! Fin si le code correspond
             iret = acc_select_end(eph_acces_local)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select_end"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infogetLocal",                 &
                     partie_variable=msp_mess)
             endif

             ! Mise a jour du compteur de champs
             nb_champs_lus = nb_champs_lus +1
          end if ! Fin si c'est une structure
       end do

       ! Traitement du cas où l'on est sorti de la boucle trop tôt
       if (nb_champs_lus > EPHV_NB_MAX_STRUCT_FICHIER) then
          call MSP_signaler_message(cle_mes="EPH_ERR_LECT_CHAMPS_FICHIER", &
               partie_variable=trim(eph_fic_local), &
               routine="eph_infogetLocal")
          return
       endif

    end if

  end subroutine eph_infogetLocal
  
  
  subroutine eph_infocodes(codes, ncodes)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_infocodes
!
!$Resume
!  Liste des codes méthodes/théories disponibles dans le fichier de configuration
!
!$Description
!   Extrait tous les codes méthodes disponibles dans le fichier 
!   de configuration
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!   Ephémérides
!
!$Usage
!  call eph_infocodes(codes, ncodes)
!.    integer, dimension(*) :: codes
!.    integer :: ncodes
!
!$Arguments
!>S     codes   :<integer,DIM=(*)>   liste des codes méthodes/théories
!>S     ncodes  :<integer>           nombre de codes trouvés
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!      eph_infoget, eph_infoclose, eph_infoget
!      libui (ui_epheminfo)
!
!$<>
!***********************************************************************
    use eph_varglob, only : EPHV_NB_MAX_STRUCT_FICHIER
    implicit none

! parametres
    integer,intent(out), dimension(*) :: codes
    integer,intent(out) :: ncodes

! local
    integer :: iret, icode, iloop
    integer :: nature
    character(LEN=CPS_MAXLG) :: libelle
    integer :: ii, acces, jj, deja_mis
    character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiers => NULL()
    character (len=CPS_MAXLG), dimension(3) :: msp_mess
    logical :: fin_fichier   ! Flag indiquant la fin du fichier

    ! Initialisations
    if(.not.ephv_acces_open) then
       call eph_infoinit()
       if ( MSP_gen_messages("eph_infocodes") ) return          
    endif

    iloop = 0

    ! Attention : eph_infoinit peut modifier ephv_acces_open
    !if(ephv_acces_open) then
    
    ! Fichiers de la table 'ephemerides'
    call cps_getListFichiersCateg('ephemerides', fichiers)

    do ii=1,cpsi_size_ptr(fichiers)
       call cpsi_getAccesMadona(fichiers(ii), acces)

! selectionner les chemins
       iret = acc_scan_reset(acces)
       if (iret<0) then
          write(msp_mess(1),*) iret
          write(msp_mess(2),*) "acc_scan_reset"
          call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
               routine="eph_infocodes",                    &
               partie_variable=msp_mess)
       endif

       ! Parcours jusqu'à atteindre la fin du fichier
       ! ou la limite algorithmique fixée à MAXCORPS
       fin_fichier = .false.
       do while ( .not.fin_fichier &
            .and. iloop <= MAXCORPS)
          
          iret = acc_scan(acces, libelle, nature)
          if (iret<0) then
             write(msp_mess(1),*) iret
             write(msp_mess(2),*) "acc_scan"
             call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                  routine="eph_infocodes",                    &
                  partie_variable=msp_mess)
          endif

          if (nature.eq.CPS_FIN_SEQ) then
             fin_fichier = .true.
          elseif (nature.eq.ACC_STRUCT) then
             iret = acc_select(acces, libelle, ACC_STRUCT)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infocodes",                    &
                     partie_variable=msp_mess)
             endif
             iret = acc_geti(acces, "code", icode)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "code"
                write(msp_mess(3),*) trim(fichiers(ii))
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                     routine="eph_infocodes",                          &
                     partie_variable=msp_mess)
             endif
             ! on verifie que le code n'est pas enregistre
             ! (s'il etait dans la base locale et dans la base ref)
             deja_mis = 0
             do jj=1,iloop
                if (codes(jj) .eq. icode) then
                   deja_mis = 1
                   exit
                end if
             end do
             if (deja_mis .eq. 0) then
                iloop = iloop + 1
                codes(iloop) = icode
             end if
             iret = acc_select_end(acces)
             if (iret<0) then
                write(msp_mess(1),*) iret
                write(msp_mess(2),*) "acc_select_end"
                call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                     routine="eph_infocodes",                    &
                     partie_variable=msp_mess)
             endif
          end if
       end do ! Fin du parcours

    end do ! Fin de la liste des fichiers
    ncodes = iloop

    ! liberation memoire
    if (associated(fichiers)) then
       deallocate(fichiers)
    end if

  end subroutine eph_infocodes

  subroutine eph_infoclose()

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_infoclose
!
!$Resume
!   Fermeture du fichier de configuration
!
!$Description
!   Fermeture de la zone d'accès MADONA associée
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!
!$Usage
!  call eph_infoclose()
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!      eph_infoget, eph_infoclose
!      libui (ui_epheminfo)
!
!$<>
!***********************************************************************
    implicit none

! parametres
    
! variables locales
    integer :: iret = 0

    ! fermer la zone d'accès
    if (ephv_acces_open) iret=acc_close(ephv_acces)

    if (iret.ge.0) ephv_acces_open = .false.

  end subroutine eph_infoclose

  subroutine eph_lire_ephem_corps(nomsfr, nomsen, nomconst, codes, ncorps, nmin)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_lire_ephem_corps
!
!$Resume
!   Lecture du fichier de configuration des éphémérides
!
!$Description
!   Lecture du fichier de configuration des éphémérides (on
!   lit ncorps corps, si on trouve moins de corps que demandé, alors
!   ncorps est mis à jour)
!   Si nmin est donné, on saute les nmin premiers éléments.
!   Pour ui_compasinfo
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!
!$Usage
!  call eph_lire_ephem_corps(nomsfr, nomsen, nomconst, codes, ncorps, [nmin])
!.    character(LEN=*), dimension(*) :: nomsfr
!.    character(LEN=*), dimension(*) :: nomsen
!.    character(LEN=*), dimension(*) :: nomconst
!.    integer, dimension(*) :: codes
!.    integer :: ncorps
!.    integer :: nmin
!
!$Arguments
!>S     nomsfr    :<LEN=*,DIM=(*)>     liste des noms des corps francais
!>S     nomsen    :<LEN=*,DIM=(*)>     liste des noms des corps anglais
!>S     nomconst  :<LEN=*,DIM=(*)>     liste des libellés des constantes
!>S     codes     :<integer,DIM=(*)>   liste des codes corps
!>E/S   ncorps    :<integer>           nombre d'éléments demandés / trouvés
!>[E]   nmin      :<integer>           indice de début du remplissage
!
!$Acces
!  PUBLIC
!
!$Remarques
!  Attention : 
!.    monsfr, nomsen, nomconst, codes doivent avoir la meme dimension
!.    dimension minimale : MAXCORPS  (cf. cps_constantes)
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
      use eph_varglob, only : EPHV_NB_MAX_STRUCT_FICHIER
      implicit none 

! Arguments
      character(LEN=*), dimension(*),intent(out) :: nomsfr
      character(LEN=*), dimension(*),intent(out) :: nomsen
      character(LEN=*), dimension(*),intent(out) :: nomconst
      integer, dimension(*),intent(out) :: codes
      integer,intent(inout) :: ncorps
      integer, intent(in), optional :: nmin

! Variables locales
      integer :: ii, code, ier, acces, ind_min
      character(LEN=ephv_lgmax) :: nom
      character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
      integer :: k, nature, iostat
      logical :: fin            ! Flag indiquant que la recherche est fructueuse
      character(LEN=CPS_MAXLG) :: libelle
      logical :: fin_fichier    ! Flag indiquant que la fin du fichier est atteinte
      integer :: nb_champs_lus  ! Nombre de champs lus dans le fichier courant

      ! Initialisations
      fin = .false.
      nomsfr(1:MAXCORPS) = ""
      nomsen(1:MAXCORPS) = ""
      nomconst(1:MAXCORPS) = ""
      codes(1:MAXCORPS) = 0
      if(.not. present(nmin)) then
         ind_min=1
      else
         ind_min=nmin
      end if

! Code 
! Langue (si definie)

      if(.not.ephv_acces_open) then
         call eph_infoinit()
         if ( MSP_gen_messages("eph_lire_ephem_corps") ) return
      endif 

      if (.not.cps_var_init) then
         call cps_init()
         if ( MSP_gen_messages("eph_lire_ephem_corps") ) return
      endif

      call cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
      if ( MSP_gen_messages("eph_lire_ephem_corps") ) return

! Lecture de la variable Corps_ip

      ii = 0
      do k=1, cpsi_size_ptr(fichiersCorps)
         call cpsi_getAccesMadona(fichiersCorps(k), acces)
         ier = acc_scan_reset(acces)
         
         nb_champs_lus = 0
         fin_fichier = .false.
         ! Parcours jusqu'à ce que l'on ait le nombre de corps ncorps
         ! ou jusqu'à la fin du fichier  
         ! ou jusqu'à ce que la limite algorithmique soit dépassée
         do while (.not. fin &
              .and. .not. fin_fichier &
              .and. nb_champs_lus <= EPHV_NB_MAX_STRUCT_FICHIER)

            ier = acc_scan(acces, libelle, nature)
            if (nature.eq.CPS_FIN_SEQ) then
               fin_fichier = .true.
            elseif (nature.eq.ACC_STRUCT) then
               ier = acc_select(acces, libelle, ACC_STRUCT)
               ier = acc_exist(acces, "eph_constante")
               if (ier.eq.1) then
                  ii = ii + 1
                  ! lecture
                  ier = acc_geti(acces, "code", code)
                  if(ii>=ind_min)  codes(ii-ind_min+1) = code
                  ier = acc_gets(acces, "nom_fr", nom)
                  if(ii>=ind_min) nomsfr(ii-ind_min+1) = trim(nom)
                  ier = acc_gets(acces, "nom_en", nom)
                  if(ii>=ind_min) nomsen(ii-ind_min+1) = trim(nom)
                  ier = acc_gets(acces, "eph_constante", nom)
                  if(ii>=ind_min) nomconst(ii-ind_min+1) = trim(nom)
               end if
               ier = acc_select_end(acces)
               if (ii>=ncorps+ind_min-1) then
                  fin = .true.
               end if
               
               ! Mise a jour du compteur
               nb_champs_lus = nb_champs_lus+1
            end if ! Fin si structure
         end do    ! Fin du parcours

         ! Fin de la lecture de l'ensemble des fichiers si la recherche est fructueuse
         if ( fin ) exit

         ! Traitement du cas où l'on est sorti de la boucle trop tôt
         if (nb_champs_lus > EPHV_NB_MAX_STRUCT_FICHIER) then
            call MSP_signaler_message(cle_mes="EPH_ERR_LECT_CHAMPS_FICHIER", &
               partie_variable=trim(fichiersCorps(k)), &
               routine="eph_lire_ephem_corps")
            return
         endif
      end do ! Fin de la liste des fichiers

      ! Recopie
      ncorps = ii

      if (associated(fichiersCorps)) then
         deallocate(fichiersCorps, stat=iostat)
      end if

      return

    end subroutine eph_lire_ephem_corps

end module eph_info
