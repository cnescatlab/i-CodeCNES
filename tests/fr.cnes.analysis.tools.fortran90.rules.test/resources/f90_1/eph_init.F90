module eph_init
!***********************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_init
!
!$Resume
!  Routines d'initialisation et de fermeture 
!
!$Description
!  Routines d'initialisation des différentes méthodes de calcul
!  d'éphémérides et de fermeture des fichiers associés à ces
!  méthodes. 
!
!$Version
!  $Id: eph_init.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: eph_init.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.22  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.21  2009/08/26 15:16:49  cml
!  AQ : Correction du numéro de FA dans l'historique
!
!  Revision 1.20  2009/08/26 15:06:18  cml
!  FA-ID 1315 : La construction du path se fait lors de l'appel à eph_infoget
!  Revision 1.19  2009/02/26 15:02:32  cml
!  AQ : Suppression d'un code en dur inutile
!  Revision 1.18  2009/02/20 10:26:38  cml
!  AQ : Suppression de use inutiles
!  Revision 1.17  2009/02/20 09:16:38  cml
!  DM-ID 960 : Ajout de la phase d'initialisation pour VSOP87
!  Revision 1.16  2009/02/12 15:34:50  cml
!  DM-ID 960 : Ajout de la methode JPL aux methodes d'ephemerides disponibles
!  Revision 1.15  2008/10/20 12:50:50  cml
!  AQ : Ajout de tests de coherences entre les parametres optionnels
!  Revision 1.14  2008/10/14 07:56:35  cml
!  DM-ID 1058 : Suppression d une variable inutile et renommage d une var. mal nommee
!  Revision 1.13  2008/10/07 14:14:13  cml
!  FA-ID 1067 : Remplacement des appels au champs fic_taituc obsolete
!  Revision 1.12  2008/10/02 13:33:49  tanguyy
!  DM-ID 1058/AQ : refonte de l'algorithme de eph_initposcor
!  Revision 1.11  2008/08/04 13:28:50  gss
!  DM-ID 1058 : (portage g95) ajout de la gestion d'erreur lors de l'accès au
!  fichier tai-tuc.
!  Revision 1.10  2006/06/28 16:09:13  vpg
!  Les fichiers Tchebychev pour les calculs d'ephemerides sont recherches dans la base locale
!  Revision 1.9  2006/05/30 15:24:53  vivaresf
!  Suppression des variables inutilisées
!  Revision 1.8  2006/05/30 12:29:07  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.7  2006/05/30 08:23:16  vivaresf
!  DM-ID 387 : variables inutiles
!  Revision 1.6  2006/04/18 09:53:30  vivaresf
!  FA 500 : traces da la DM 391
!  Revision 1.5  2006/01/23 13:43:46  bouillaj
!  Suppression de la metheode EPROC
!  Revision 1.4  2005/12/09 08:38:10  vivaresf
!  suppression de codes commentés
!  Revision 1.3  2005/12/08 18:39:04  vivaresf
!  Cartouches et vérification des déclarations
!  Revision 1.2  2005/12/08 08:59:44  vpg
!  Suppression des commentaires inutiles
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.9  2005/11/07 15:56:22  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.8  2005/10/13 15:30:20  bouillaj
!  FA-ID 367 : Cloture du FT ( Bug dans eph_initposcor de eph_init.F90)
!  Revision 1.7.2.1  2005/10/13 15:26:49  bouillaj
!  FA-ID 367 : Version initiale du FT ( Bug dans eph_initposcor de eph_init.F90)
!  Revision 1.7  2005/10/13 10:38:06  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6.2.1  2005/10/13 10:29:39  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.6  2005/10/12 13:03:30  bouillaj
!  DM-ID 147. Ajout extrapolation keplerienne
!  Revision 1.5  2005/05/09 14:17:32  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.4  2005/03/09 09:12:03  vivaresf
!  Correction des cartouches
!  Revision 1.3  2004/12/17 14:58:10  vivaresf
!  Documentation
!  Revision 1.2  2004/05/25 09:54:02  vivaresf
!  Rajout du prefixe ephv_ devant les noms de constantes
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.28  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.25  2004/01/07 16:17:16  bremard
!  Mise à jour cartouche
!  Revision 1.24  2003/12/31 15:56:38  bremard
!  Lecture fichier saut TUC + Init fichier saut TUC
!  Revision 1.23  2003/12/30 16:07:37  bremard
!  Suppression du cas Extrapolation Keplerienne (61)
!  Revision 1.22  2003/06/06 14:52:12  bremard
!  Particularisation des parametres
!  Revision 1.20  2002/10/16 13:30:37  vivaresf
!  PhB - AM_util_ficunit90 changé en eph_util_ficunit
!  Revision 1.19  2002/09/26 15:45:05  vivaresf
!  nfichiers en parametre optionnel, ouverture des fichiers en 90
!  Revision 1.17  2001/12/18 16:13:45  vivaresf
!  Maj documentation
!  Revision 1.16  2001/12/17 17:41:18  vivaresf
!  Controle sur les fichiers BdL
!  Revision 1.15  2001/12/13 17:31:24  vivaresf
!  Reinitialisations eproc
!  Revision 1.14  2001/12/12 10:19:45  vivaresf
!  close systematique des lfn avant ouverture
!  Revision 1.13  2001/12/07 16:46:25  vivaresf
!  Presentation fm des cartouches
!  Revision 1.12  2001/12/07 11:41:26  vivaresf
!  Compilation et librairies
!  Revision 1.11  2001/12/05 14:02:26  bremard
!  PhB - Mise à jour des cartouches
!  Revision 1.10  2001/12/04 16:59:32  vivaresf
!  lfneproc pour theories VSOP82 et VSOP87 methode EPROC
!  Revision 1.9  2001/12/04 14:49:34  vivaresf
!  Gestion d'erreur NAIF
!  Revision 1.8  2001/12/04 09:22:30  vivaresf
!  Appel a eph_inittchemad
!  Revision 1.7  2001/12/04 09:16:51  bremard
!  PhB - Ajout init pour methode tchebychev
!  Revision 1.6  2001/12/03 13:20:41  vivaresf
!  Correction appel a AMv_rc_get
!
!$FinHistorique
!
!$Usage
!  use eph_init
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_initposcor eph_closeposcor
!
!$Structure
!
!$Fonctions
!
!$Routines
!- eph_initposcor
!- eph_closeposcor
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_varglob
!- eph_info
!- eph_tcheb
!- eph_tchebmad
!- eph_kepler
!- eph_jpl_ephemeris
!- eph_vsop87
!- cps_acces
!#
!
!$Interface
!#V
!#
!
!$<>
!***********************************************************************

      use msp_gestion_erreur
      use eph_varglob
      use eph_info
      use eph_tcheb
      use eph_tchebmad
      use eph_kepler
      use eph_jpl_ephemeris, only : eph_initjpl, eph_closejpl
      use eph_vsop87, only : eph_initvsop87, eph_closevsop87
      use cps_acces
     implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_init.F90 379 2013-02-22 16:59:49Z ffsm $'


contains

      subroutine eph_initposcor(code, nfichiers, fichiers, nbbull, tabull, codecorps)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_initposcor
!
!$Resume
!       Initialisation des méthodes de calculs d'éphémérides
!       en préalable à l'utilisation de eph_poscor
!
!$Description
!       Ouverture et lecture des fichiers d'éphémérides (s'il y a lieu) et
!       initialisations des différents paramètres.
!       Une méthode d'éphéméride (NAIF, Tchebychev, Analytique,..) est un type
!       d'algorithme de calcul d'éphémérides.
!       Une théorie est relative aux valeurs descriptives des corps du Système Solaire
!       (DE200, DE405, etc.)
!       Le code "méthode/théorie" utilisé par COMPAS est un entier sur 2 chiffres, qui 
!       définit la méthode (le 1er digit) et la théorie (le 2e digit).
!       Certaines théories nécessitent des fichiers (tableau "fichiers" de dimension "nfichiers") 
!       ou des bulletins (tableau "tabull").
!
!$Auteur
!       Y.TANGUY (ATOS) d'après l'ancienne routine
!       Philippe BREMARD        (SchlumbergerSema)
!       Florence VIVARES        (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Routines
!- cps_init
!- cpsi_getAccesMadona
!- MSP_ajouter_fichier_message
!- cps_getFichierTaituc
!- MSP_signaler_message
!- eph_infogetLocal
!- erract
!- spkuef
!- spklef
!- eph_util_ficunit90
!- eph_inittche
!- eph_inittchemad
!- eph_initkep
!- eph_initjpl
!- eph_initvsop87
!
!$Module
!
!$Usage
!  call eph_initposcor(code, [nfichiers], [fichiers], [nbbull], [tabull], [codecorps])
!.    integer :: code
!.    integer :: nfichiers
!.    character (LEN=*), dimension(:) :: fichiers
!.    type(EPH_BULLETIN), dimension(:) :: tabull
!.    integer, dimension(:) :: codecorps
!.    integer :: nbbull
!
!$Arguments
!>E     code       :<integer>                code méthode/théorie lu dans ephem.conf   
!>[E]   nfichiers  :<integer>                nombre de fichiers d'entrée
!>[E]   fichiers   :<LEN=*,DIM=(:)>          fichiers d'entrée (dimension "nfichiers")
!>[E]   nbbull     :<integer>                nombre de bulletin entrant  (si code 11)
!>[E]   tabull     :<EPH_BULLETIN,DIM=(:)>   tableau regroupant les bulletins entrant
!>[E]   codecorps  :<integer,DIM=(:)>        tableau regroupant les codes des corps des bulletins
!
!$Remarques
!.       - On ne peut ouvrir qu'une théorie à la fois par méthode :
!       si on rappelle eph_initposcor sur une théorie alors qu'une
!       autre a été ouverte avec la même méthode, l'ancienne théorie est
!       refermée (ses variables et fichiers associes) avant d'ouvrir 
!       la nouvelle.
!
!$Mots-cles
!       lecture
!
!$Voir-Aussi
!       eph_closeposcor
!
!$<>
!***********************************************************************        
        ! Arguments
        !==========
        integer, intent(in)                        :: code
        integer, intent(in), optional              :: nfichiers
        character (LEN=*), dimension(:), optional, intent(in) :: fichiers
        type(EPH_BULLETIN), dimension(:), optional, intent(in) :: tabull
        integer, dimension(:), optional, intent(IN):: codecorps
        integer, optional, intent(IN)              :: nbbull


        ! Variables locales
        !==================
        integer :: ii, stat_alloc
        integer :: ifile,ntape,nerr, ier_appel
        character (LEN=ephv_lgmax), dimension(:), pointer :: fic => NULL()
        character (LEN=ephv_lgmax) :: fichierd,ficbdl, rep, methode, nomfic, repbdl
        character (LEN=ephv_lgmax) :: fichier_erreurs
        logical :: failed, loc
        integer :: nfichiers_loc
        integer :: acces_ref
        integer :: trouve     ! Code indiquant si le fichier de taituc a été trouve    


        ! INITIALISATION
        loc = .false.
        stat_alloc = 0

        ! Initialisations générales (COMPAS)
        !
        ! -> permet de ne pas passer par un appel
        ! à cps_init, si l'utilisateur ne veut utiliser
        ! que les éphémérides
        !
        !===================================
        if (.not.cps_var_init) call cps_init()
        ! ouvrir le fichier config de la base de reference
        call cpsi_getAccesMadona(fichier_conf_ref, acces_ref)

        ! recuperation du chemin et chargement du fichier d'erreurs
        if (.not.ficerrmessage) then 
           fichier_erreurs = trim(rep_cps_fcf)//"/EPH_erreur_fr"
           call MSP_ajouter_fichier_message (fichier_erreurs)
           ficerrmessage=.true.
        endif

        ! recuperation du fichier de saut du TUC
        if (.not.initficsauttuc) then 
           ! lire 'fichier_tai-tuc'
           call cps_getFichierTaituc(fictuc, trouve)
           if ( MSP_ERREUR .or. trouve /= CPS_OK ) then
              call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
                   partie_variable="tai-tuc", &
                   routine="eph_initposcor" )
              return
           endif
           initficsauttuc=.true.
        endif


        ! Initialisations lors de tout appel
        !===================================
        ! recuperation des informations éphémérides dans le fichier de configuration
        call eph_infogetLocal(code, loc, repertoire=rep, fichierd=fichierd, &
             methode=methode)
        if (.not.loc) then
           call eph_infoget(code, repertoire=rep, fichierd=fichierd, &
                methode=methode)
        end if
           
        ! Test de coherence des arguments optionnels
        if ((present(fichiers) .and. (.not. present(nfichiers))) .or. &
             (present(nfichiers) .and. (.not. present(fichiers)))) then
           
           ! Gestion d'erreur
           call MSP_signaler_message(cle_mes="EPH_ERR_ARGS_INIT_POSCOR")
           return
           
        end if

        ! Allocation du tableau de fichiers
        if (PRESENT(nfichiers)) then
           nfichiers_loc = nfichiers
        else
           nfichiers_loc = 0
        endif
        
        if (nfichiers_loc > 0) then

           if (associated(fic)) deallocate(fic, stat=stat_alloc) 
           if (stat_alloc < 0) then
              call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                   routine="eph_initposcor")
              return
           end if
           allocate(fic(nfichiers_loc),stat=stat_alloc)
           if (stat_alloc < 0) then
              call MSP_signaler_message(cle_mes="CPS_ERR_ALLOC",&
                   routine="eph_initposcor")
              return
           end if

           do ii=1, nfichiers_loc
              fic(ii)=fichiers(ii)
           end do
           
        end if

        ! Initialisations spécifiques
        !============================

        !-----------------------------------------------------------------------
        ! Cas NAIF
        !-----------------------------------------------------------------------
        
        if(trim(methode).eq."NAIF") then 
           ! eviter le abort du controle d'erreur de la spicelib
           call erract (  'SET',   'RETURN'  )
           ! raz ou non du noyau de la spicelib
           if(initnaif) then 
              do ii=1, nfichiers_loc 
                 if(ephv_lfnnaif(ii).ne.0) then
                    call spkuef(ephv_lfnnaif(ii))
                    ephv_lfnnaif(ii) = 0
                    ficnaif(ii) = ""
                 endif
              enddo
           endif

           ! chargement des fichiers 
           do ifile=1,nfichiers_loc
              if (len_trim(fic(ifile)).gt.0) then 
                 nomfic = trim(rep) // trim(fic(ifile))
                 call spklef(nomfic,ntape)
                 ephv_lfnnaif(ifile) = ntape
                 ficnaif(ifile) = nomfic
              endif
              initnaif=.true.
           enddo
           if (nfichiers_loc.le.0.or.failed()) then
              initnaif=.false.
              call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
                   partie_variable=" erreur dans l'initialisation NAIF ", &
                   routine="eph_initposcor" )
              return
           endif
           
           ! memoriser le code naif du fichier chargé  
           ! (pour eviter les mélanges de théories)
           codenaif = code
        endif
        
        !-----------------------------------------------------------------------
        ! Cas BDL : au premier appel uniquement
        !-----------------------------------------------------------------------
        

        if(trim(methode).eq."BDL") then 

           ! Méthode BDL : un fichier principal obligatoire (ex : COEFTCHE.DAT)
           ! + 'nfichiers_loc' fichiers annexes (pour les comètes)
           
           if(.not.isinit_bdl) then 
              call eph_infoget(code, repertoire=repbdl, fichierd=ficbdl)
              call eph_util_ficunit90(ephv_lfnbdl,ier_appel)
              if (ier_appel.lt.0) then
                 call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
                   partie_variable="reservation d'un lfn impossible", &
                   routine="eph_initposcor" )
                 return
              endif
              
              nomfic= trim(repbdl) // ficbdl
              !        Ouverture du fichier en F90 car la lecture est en F90 (eph_syst_sol)
#ifdef __GFORTRAN__
              open(ephv_lfnbdl,file=nomfic,status='old',access='direct',recl=5104, &
                   form='unformatted', convert='big_endian', iostat=ier_appel)
#else
              open(ephv_lfnbdl,file=nomfic,status='old',access='direct',recl=5104, &
                   form='unformatted', iostat=ier_appel)
#endif
              if (ier_appel.ne.0) then
                 call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
                      partie_variable=trim(nomfic), &
                      routine="eph_initposcor" )
                 return
              endif

              isinit_bdl = .true.

           endif


           ! fichier cometes si besoin
           do ifile=1,nfichiers_loc
              nomfic=trim(fic(ifile))
              if ((trim(nomfic)/="").and.(len_trim(nomfic)>0).and.&
                   (len_trim(nomfic)/=200)) then
                 call eph_util_ficunit90(ntape,ier_appel)
                 if (ier_appel.lt.0) then
                    call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
                         partie_variable="reservation d'un lfn impossible", &
                         routine="eph_initposcor" )
                    return
                 endif

                 nomfic= trim(rep)  // fic(ifile)
                 open (ntape,file=nomfic,access='direct',recl=80,status='old', &
                      iostat=nerr)
                 if (nerr.ne.0) then
                    call MSP_signaler_message (cle_mes="EPH_ERR_FICABS", &
                         partie_variable=trim(nomfic), &
                         routine="eph_initposcor" )
                    return
                 endif
                 ephv_lfncomet(ifile) = ntape
              endif
           enddo
        endif

        
        !-----------------------------------------------------------------------
        ! Cas polynômes de Tchebychev CNES
        !-----------------------------------------------------------------------

        if(trim(methode).eq."TCHE") then

           if (nfichiers_loc /= 1) then
              call MSP_signaler_message (cle_mes="EPH_ERR_FIC_TCHE", &
                   routine="eph_initposcor" )
              return
           endif

           ! recherche des ressources dans la base de référence
           nomfic= trim(rep) // fic(1)
           call eph_inittche(nomfic)
           if ( MSP_gen_messages("eph_initposcor") ) return
        endif

        !-----------------------------------------------------------------------
        ! Polynome de Tchebytchev au format MADONA : un seul fichier à la fois
        !-----------------------------------------------------------------------
        
        if(trim(methode).eq."TCHEmad") then 

           if (nfichiers_loc /= 1) then
              call MSP_signaler_message (cle_mes="EPH_ERR_FIC_TCHE_MAD", &
                   routine="eph_initposcor" )
              return
           endif

           nomfic= trim(rep) 
           call eph_inittchemad(nomfic, fic(1))
           if ( MSP_gen_messages("eph_initposcor") ) return
        endif

        !-----------------------------------------------------------------------
        ! Cas VSOP82 analytique
        !-----------------------------------------------------------------------
        if(trim(methode).eq."BDL-ANALYT") then 
           ! pas d'initialisation nécessaire
           continue
        endif

        !-----------------------------------------------------------------------
        !  Cas Extrapolation képlérienne
        !-----------------------------------------------------------------------
        if(trim(methode).eq."EXTRAPOLATION") then  
           if (nbbull == 1) then 
              call eph_initkep(tabull(1), codecorps(1))
           else if (nbbull > 1) then
              call eph_initkep(tabull, nbbull, codecorps)
           else
              call MSP_signaler_message (cle_mes="EPH_ERR_BUL_KEP", &
                   routine="eph_initposcor" )
              return

           end if
        endif

        !-----------------------------------------------------------------------
        ! Cas ephemerides JPL (DE405 ou INPOP06)
        !-----------------------------------------------------------------------
        if(trim(methode).eq."JPL") then 
           ! Recherche d'une unité logique disponible
           call eph_util_ficunit90(ntape,ier_appel)
           if (ier_appel.lt.0) then
               call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
                    partie_variable="reservation d'un lfn impossible", &
                    routine="eph_initposcor" )
               return
           endif

           ! Recherche du fichier en base locale puis en base de référence
           call eph_infogetLocal(code, loc, repertoire=rep, fichierd=fichierd)
           if (.not.loc) then
               call eph_infoget(code, repertoire=rep, fichierd=fichierd)
           end if
           if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="EPH_ERR_FIC_JPL", &
                   routine="eph_initposcor" )
               return
           endif

           ! initialisation de la méthode
           nomfic= trim(rep) // fichierd
           CALL eph_initjpl(ntape, nomfic) 
        endif


        !-----------------------------------------------------------------------
        ! Cas ephemerides IMCEE pour la théorie VSOP87
        !-----------------------------------------------------------------------
        if(trim(methode).eq."IMCEE") then 
           ! Recherche d'une unité logique disponible
           call eph_util_ficunit90(ntape,ier_appel)
           if (ier_appel.lt.0) then
               call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
                    partie_variable="reservation d'un lfn impossible", &
                    routine="eph_initposcor" )
               return
           endif

           ! Recherche du fichier en base locale puis en base de référence
           call eph_infogetLocal(code, loc, repertoire=rep)
           if (.not.loc) then
               call eph_infoget(code, repertoire=rep)
           end if
           if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="EPH_ERR_FIC_IMCEE", &
                   routine="eph_initposcor" )
               return
           endif

           ! initialisation de la méthode par le nom du repertoire
           nomfic= trim(rep)
           CALL eph_initvsop87(ntape, nomfic) 
        endif


        !------------------------------------------------------------------------
        !  Fin de eph_initposcor : 
        !  - sauvegarde de la méthode courante
        !  - désallocation des pointeurs
        !------------------------------------------------------------------------
        
        ! code_courant contient le 1er chiffre du code méthode/théorie
        ! qui correspond à NAIF, TCHEBYCHEV, BDL, etc.
        !
        ! il sera comparé au code donné par l'utilisateur lors de l'appel à 
        ! eph_poscor
        !---------------------------------------------
        code_courant = code/10
        ! code par défaut
        if (code_courant == 0) code_courant = eph_methode_analytique
        

        if (associated(fic)) then
           deallocate(fic, stat=stat_alloc)
           if (stat_alloc < 0) then
              call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                   routine="eph_initposcor")
              return
           end if
        end if
        

      end subroutine eph_initposcor

    subroutine eph_closeposcor()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closeposcor
!
!$Resume
!       Fermeture des fichiers Ephémérides
!
!$Description
!.       Fermeture des fichiers, BDL, comètes et Tchebytchev
!.       Remise à zéro des lfn et nom de fichiers (NAIF, BDL, comètes, Tchebytchev)
!.       Libération des variables pointées (Tchebytchev_madona)
!
!$Auteur
!       Philippe BREMARD        (SchlumbergerSema)
!       Florence VIVARES        (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Routines
!- spkuef
!- eph_closetche
!- eph_closetchemad
!- eph_closekep
!- eph_closejpl
!- eph_closevsop87
!- cps_close
!
!$Usage
!  call eph_closeposcor()
!
!$Arguments
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!      eph_initposcor
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      implicit none

      ! variables locales
      integer :: ii

      ! cas BDL
      if(isinit_bdl) then
         close(ephv_lfnbdl)
         ephv_lfnbdl=0
         isinit_bdl=.false.
      end if

      do ii=1, ephv_nbficmax 
         if(ephv_lfncomet(ii).gt.0) close(ephv_lfncomet(ii))
         ephv_lfncomet(ii)=0
      enddo

      !  cas NAIF
      if(initnaif) then 
         do ii=1, ephv_nbficmax 
            if(ephv_lfnnaif(ii).ne.0) then
               call spkuef(ephv_lfnnaif(ii))
               ephv_lfnnaif(ii) = 0
               ficnaif(ii) = ""
            endif
         enddo
         codenaif=0
         initnaif=.false.
      endif

      ! cas Tchebytchev
      call eph_closetche()
      codetche=0
      fictche=""

      ! cas Tchebytchev Madona
      codetchemad=0
      call eph_closetchemad()

      !cas EXTRAPOLATION
      call eph_closekep()
      
      !cas JPL
      call eph_closejpl()

      !cas IMCEEE / VSOP87
      call eph_closevsop87()

      initficsauttuc=.false.

      if (cps_var_init) call cps_close()
      
    end subroutine eph_closeposcor

end module eph_init

