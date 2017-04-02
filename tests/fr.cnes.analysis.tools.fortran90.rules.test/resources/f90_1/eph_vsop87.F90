module eph_vsop87

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_vsop87
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Version
!  $Id: eph_vsop87.F90 360 2013-02-15 11:38:21Z aadt $
!
!$Historique
!  $Log: eph_vsop87.F90,v $
!  Revision 360  2013/02/14 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 158 2012/11/27 Fran Simarro/FFSM - GMV
!  Fermeture de code source pour la livraison BIBMS 1.13 (dec 2012)
!
!  Revision 150 2012/11/23 ffsm
!  FA-ID 1524 : Modifié pour prendre en compte l'échelle temps de la date d'entrée
!               à l'heure d'ajouter un saut TaiTuc
!
!  Revision 1.14  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.13  2010/10/01 14:45:53  ogarat
!  VERSION::FA-ID:1430:01/10/2010: Correction de l'historique V2.9
!
!  Revision 1.12  2010/05/05 12:26:23  jlrobin
!  V2.9::FA-ID:1327:05/05/2010: Ajout de l'attribut "parameter" a une variable globale
!
!  Revision 1.11  2009/10/05 13:37:17  cmartel
!  FA-ID 1279 : Correction du message d'erreur sur les corps non traites
!
!  Revision 1.10  2009/10/02 14:48:07  cmartel
!  FA-ID 1279 : Modification de la méthode pour ne pas limiter le corps central au soleil
!
!  Revision 1.9  2009/10/01 14:20:17  cmartel
!  FA-ID 1277 : Correction mineure
!  Revision 1.7  2009/03/09 15:16:00  cml
!  DM-ID 960 : Correction de variables inutilisees
!  Revision 1.6  2009/03/02 16:48:22  cml
!  DM-ID 960 : Ajout d'un argument optionnel a la methode de calcul
!  Revision 1.5  2009/02/26 15:21:11  cml
!  DM-ID 960 : Ajout du changement de repere ecliptique 2000 -> EME 2000
!  Revision 1.4  2009/02/20 13:39:00  cml
!  DM-ID 960 : Correction dans les initialisations et fermetures de fichier
!  Revision 1.3  2009/02/20 11:06:18  cml
!  DM-ID 960 : Correction d'un use incorrect
!  Revision 1.2  2009/02/20 09:18:41  cml
!  DM-ID 960 : Ajout du traitement des erreurs et mise a jour des cartouches
!  Revision 1.1  2009/02/13 15:47:55  cml
!  DM-ID 960 : Ajout de la methode de calcul d'ephemerides de la theorie VSOP87
!
!$FinHistorique
!
!$Usage
!  use eph_vsop87
!
!$Structure
!
!$Global
!
!>  ephi_init_vsop87             : <logical>                                               Flag indiquant l'init. de la methode
!>  ephi_unit_lecture            : <integer>                                               Unité logique utilisée pour la lecture 
!>  ephi_rep_ephem               : <LEN=ephv_lgmax>                                        Répertoire contenant les fichiers éphémérides
!>  ephi_fichier_ephem           : <LEN=ephv_lgmax>                                        Dernier fichier éphéméride lu
!#V
!>  EPHI_VSOP87_NB_CORPS         : <integer,parameter,private>                             Nombre de corps traités par la méthode        
!>  EPHI_VSOP87_CORPS            : <integer,DIM=(EPHI_VSOP87_NB_CORPS),parameter,private>  Liste des corps traités par la méthode
!>  EPHI_VSOP87_CODES_INTERNE    : <integer,DIM=(EPHI_VSOP87_NB_CORPS),parameter,private>  Codes internes de ces corps
!>  EPHI_VSOP87_SUFFIXES         : <LEN=4,DIM=(EPHI_VSOP87_NB_CORPS),private>              Suffixes associés à chacun de ces corps
!#
!$Common
!
!$Routines
!- eph_initvsop87
!- eph_closevsop87
!- eph_pvvsop87
!- ephi_vsop87_pv_corps
!- ephi_verifier_entrees
!- ephi_VSOP87
!- eph_vsop87_infos
!- ephi_convertir_codecorps
!- ephi_extraire_ext
!- ephi_analyser_ext
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- eph_varglob
!- eph_constantes
!- mslib
!- msp_gestion_erreur
!- eph_util
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_initvsop87 eph_closevsop87 eph_pvvsop87 ephi_vsop87_pv_corps ephi_verifier_entrees
!.  ephi_VSOP87 eph_vsop87_infos ephi_convertir_codecorps ephi_extraire_ext ephi_analyser_ext
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use eph_varglob, only : ephv_lgmax
      use eph_constantes, only : eph_mercure, eph_venus, eph_terre, eph_mars, &
         eph_jupiter, eph_saturne, eph_uranus, eph_neptune, eph_bary_terre, eph_soleil
      use mslib

      ! Variables internes au module
      logical :: ephi_init_vsop87 = .false.
      integer :: ephi_unit_lecture = -999
      character(len=ephv_lgmax) :: ephi_rep_ephem 
      character(len=ephv_lgmax) :: ephi_fichier_ephem 
      
      ! Liste des corps disponibles par la méthode 
      ! Auquel il faut ajouter le soleil (corps central de la méthode originale)
      integer, parameter, private :: EPHI_VSOP87_NB_CORPS = 9
      integer, dimension(EPHI_VSOP87_NB_CORPS), parameter, private :: EPHI_VSOP87_CORPS &
           = (/ eph_mercure, eph_venus, eph_terre, eph_mars, eph_jupiter, &
             eph_saturne, eph_uranus, eph_neptune, eph_bary_terre /)
      ! Code interne des corps
      integer, dimension(EPHI_VSOP87_NB_CORPS), parameter, private :: EPHI_VSOP87_CODES_INTERNE &
           = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9/)
      ! Suffixes des fichiers correspondants
      character(len=4), dimension(EPHI_VSOP87_NB_CORPS), parameter, private :: EPHI_VSOP87_SUFFIXES&
           = (/ '.mer','.ven','.ear','.mar','.jup', &
           '.sat','.ura','.nep','.emb'/)


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_vsop87.F90 360 2013-02-15 11:38:21Z aadt $'

contains

      subroutine eph_initvsop87(num_unit, rep_ephem) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_initvsop87
!
!$Resume
!  Routine d'initialisation de la méthode
!
!$Description
!  Routine d'initialisation de la méthode
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_initvsop87(num_unit, rep_ephem) 
!.    integer :: num_unit
!.    character(len=ephv_lgmax) :: rep_ephem 
!
!$Arguments
!>E     num_unit   :<integer>          Numéro d'unité logique pour la lecture
!>E     rep_ephem  :<LEN=ephv_lgmax>   Répertoire contenant les fichiers
!                                      d'éphémérides
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
        
        integer, intent(in) :: num_unit
        character(len=ephv_lgmax), intent(in) :: rep_ephem 

        ! Fermeture du fichier en cours
        if ( ephi_init_vsop87 ) close(ephi_unit_lecture) 

        ! Recopie des valeurs          
        ephi_unit_lecture = num_unit
        ephi_rep_ephem = trim(rep_ephem)
        ephi_init_vsop87 = .true.
        ephi_fichier_ephem = ""

      end subroutine eph_initvsop87


      subroutine eph_closevsop87() 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closevsop87
!
!$Resume
!  Routine de finalisation de la méthode
!
!$Description
!  Routine de finalisation de la méthode
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_closevsop87() 
!
!$Arguments
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

        ! remise a zero du dernier fichier ouvert
        if ( ephi_init_vsop87 )  close(ephi_unit_lecture)
        ! Remise a zero du flag
        ephi_init_vsop87 = .false.
        ! Remise a zero des valeurs
        ephi_unit_lecture = -999
        ephi_rep_ephem = "" 
        ephi_fichier_ephem = ""

      end subroutine eph_closevsop87


      subroutine eph_pvvsop87(numpla, ncorpsc, t1950, xpla, prec)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_pvvsop87
!
!$Resume
!  Calcul d'éphémérides pour la théorie VSOP87
!
!$Description
!  Calcul d'éphémérides pour la théorie VSOP87.
!  Si l'argument "prec" n'est pas imposé, la solution complète de la méthode
!  est calculée. Si "prec" est spécifié non nul, alors la méthode calcule une
!  solution tronquée à cette précision relative près.
!  Il est nécessaire d'initialiser la méthode avant de lancer le calcul.
!  Le vectuer position vitesse calculé est exprimé dans l'EME2000 avec ncorpsc
!  comme corps central.
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_pvvsop87(numpla, ncorpsc, t1950, xpla, [prec])
!.    integer :: numpla
!.    integer :: ncorpsc
!.    real(kind = pm_reel) :: t1950
!.    real(kind = pm_reel), dimension(6) :: xpla
!.    integer :: prec
!
!$Arguments
!>E     numpla   :<integer>           Code du corps d'intérêt
!>E     ncorpsc  :<integer>           Code du corps central
!>E     t1950    :<pm_reel>           Date en jour juliens 1950 fractionnaires
!>S     xpla     :<pm_reel,DIM=(6)>   Vecteur position vitesse calculé dans l'EME2000
!>[E]   prec     :<integer>           Précision relative à utiliser pour le calcul 
!                                     d'une solution tronquée
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- ephi_verifier_entrees
!- eph_jjfrac1950_jjuliens
!- ephi_vsop87_pv_corps
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_util
!- eph_constantes
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

        use msp_gestion_erreur
        use eph_util, only : eph_jjfrac1950_jjuliens
        use eph_constantes, only : eph_uakm_uai1976, eph_uajkms_uai1976

        implicit none

        ! Variables en entrée / sortie
        integer, intent(in) :: numpla
        integer, intent(in) :: ncorpsc
        real(kind = pm_reel), intent(in) :: t1950
        real(kind = pm_reel), dimension(6), intent(out) :: xpla
        ! Paramètre optionnel
        integer, optional, intent(in) :: prec

        ! Constante
        real(kind=pm_reel), parameter :: precision_vsop87 = 0.0_pm_reel

        ! Variables internes
        real(kind = pm_reel) :: jjuliens, prec_calcul
        integer :: ii

        ! Vecteur temporaire stockant les pos-vit dans l'ecliptique 2000
        real(kind = pm_reel), dimension(6) :: xpla_corpsc_soleil
        real(kind = pm_reel), dimension(6) :: xpla_pla_soleil

        ! Verification de l'initialisation
        if ( .not. ephi_init_vsop87 ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_INIT", &
                routine="eph_pvvsop87")
           return
        endif

        ! Verification des codes en entree
        call ephi_verifier_entrees(ncorpsc, numpla)
        if ( MSP_ERREUR ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_ENTREES", &
                routine="eph_pvvsop87")
           return
        endif        

        ! Conversion (pas de gestion d'erreur particulière)
        call eph_jjfrac1950_jjuliens(t1950, jjuliens)
        
        ! Vérification de la date >= 1.0
        if ( jjuliens < 1.0_pm_reel) then
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_DATE", &
                routine="eph_pvvsop87")
           return
        endif   

        ! Extraction du parametre precision si spécifié
        ! Par défaut, on calcule la solution complète
        prec_calcul = precision_vsop87
        if ( present(prec) ) then
            prec_calcul = prec
        endif

        ! Lancement du calcul de la pv pour chacun des corps
        ! par rapport au soleil
        call ephi_vsop87_pv_corps (jjuliens, prec_calcul, &
             numpla, xpla_pla_soleil)
        if ( MSP_gen_messages("eph_pvvsop87") ) return 


        call ephi_vsop87_pv_corps (jjuliens, prec_calcul, &
             ncorpsc, xpla_corpsc_soleil)
        if ( MSP_gen_messages("eph_pvvsop87") ) return 


        ! La position et la vitesse du corps "pla" 
        ! par rapport au corps central "corpsc"
        ! est égale à la différence des deux pv exprimée par rapport au soleil
        do ii = 1, 6
           xpla(ii) = xpla_pla_soleil(ii) - xpla_corpsc_soleil(ii)
        enddo

      end subroutine eph_pvvsop87



      subroutine ephi_vsop87_pv_corps(jjuliens, prec_calcul, code_pla, &
           xpla_pla_soleil)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_vsop87_pv_corps
!
!$Resume
!  Calcul de la position d'un corps dans le repère EME2000 avec le soleil
!  comme corps central
!
!$Description
!  Calcul de la position d'un corps dans le repère EME2000 avec le soleil
!  comme corps central
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ephi_vsop87_pv_corps(jjuliens, prec_calcul, code_pla, &
!.               xpla_pla_soleil)
!.    integer :: code_pla
!.    real(kind = pm_reel) :: jjuliens
!.    real(kind = pm_reel) :: prec_calcul
!.    real(kind = pm_reel), dimension(6) :: xpla_pla_soleil
!
!$Arguments
!>E     jjuliens         :<pm_reel>           Date en entrée
!>E     prec_calcul      :<pm_reel>           Précision du calcul (si 0.0 précision max.)
!>E     code_pla         :<integer>           Code NAIF du corps
!>S     xpla_pla_soleil  :<pm_reel,DIM=(6)>   Position et vitesse du corps par rapport a soleil
!                                             exprimé dans l'EME2000.
!
!$Common
!
!$Routines
!- ephi_convertir_codecorps
!- MSP_signaler_message
!- ephi_extraire_ext
!- ephi_VSOP87
!- mr_ecliJ2000_J2000
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_util
!- eph_constantes
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


        use msp_gestion_erreur
        use eph_util, only : eph_jjfrac1950_jjuliens
        use eph_constantes, only : eph_uakm_uai1976, eph_uajkms_uai1976

        implicit none

        ! Variables en entrée / sortie
        integer, intent(in) :: code_pla
        real(kind = pm_reel), intent(in) :: jjuliens
        real(kind = pm_reel), intent(in) :: prec_calcul
        real(kind = pm_reel), dimension(6), intent(out) :: xpla_pla_soleil

        ! Constantes
        ! Code associé aux éphémérides héliocentriques rectangulaires
        integer , parameter :: version_vsop87 = 1
        character(len=7), parameter :: base_fichier_vsop87 = 'VSOP87A'

        ! Variables internes
        integer :: code_pla_imcee
        integer :: ierr, i
        character(len=ephv_lgmax) :: fichier_ephem
        character (len=10) :: suffixe
        character(len=10) :: buff_message
        type(tm_code_retour) :: code_retour

        ! Vecteur temporaire stockant les pos-vit dans l'ecliptique 2000
        real(kind = pm_reel), dimension(6) :: xpla_ecli

        ! Traitement particulier : la méthode calcule les coordonnées 
        ! par rapport au soleil
        if ( code_pla == eph_soleil ) then
           ! Position et vitesse nulle
           xpla_pla_soleil(1:6) = 0.0_pm_reel
           return
        endif
           
        ! Conversion des codes corps en entrée
        call ephi_convertir_codecorps(code_pla, code_pla_imcee)
        if ( MSP_ERREUR ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_ENTREES", &
                routine="ephi_vsop87_pv_corps")
           return
        endif

        ! Recherche du suffixe de la planète
        call ephi_extraire_ext(code_pla, suffixe)
        if ( MSP_gen_messages("ephi_vsop87_pv_corps") ) return 

        ! Construction du nom du fichier d'ephemerides de la planete    
        fichier_ephem = trim(ephi_rep_ephem) // '/' &
             // base_fichier_vsop87 // trim(suffixe)

        ! Vérification si le dernier fichier charger est bien le même
        if (trim(fichier_ephem) .ne. trim(ephi_fichier_ephem) ) then
           ! Ouverture du fichier sur la même unté logique
           ierr = 0
           open (ephi_unit_lecture,file=fichier_ephem,status='old',iostat=ierr)
           ! Renvoie en erreur
           if (ierr.ne.0) then
              call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_LECT_FIC", &
                routine="ephi_vsop87_pv_corps", &
                partie_variable=trim(fichier_ephem))
              return
           endif   

           ! Recopie
           ephi_fichier_ephem = trim(fichier_ephem)
        endif

        ! Lancement du calcul    
        ierr = 0
        call ephi_VSOP87 (jjuliens, &
             version_vsop87, &
             code_pla_imcee, &
             prec_calcul, &
             ephi_unit_lecture, &
             xpla_ecli, &
             ierr)

        ! Conversion du code d'erreur 
        if ( ierr .ne. 0 .or. MSP_ERREUR ) then
           select case ( ierr ) 
           case (1)
               call MSP_signaler_message(cle_mes='EPH_ERR_VSOP87_ENTREES', &
                   routine="ephi_vsop87_pv_corps")
           case (2)
               write (buff_message, *) code_pla
               call MSP_signaler_message(cle_mes='EPH_ERR_VSOP87_CODEPLA', &
                   routine="ephi_vsop87_pv_corps", &
                   partie_variable=buff_message)
           case (3)
               write (buff_message, *) 
               call MSP_signaler_message(cle_mes='EPH_ERR_VSOP87_PRECISION', &
                   routine="ephi_vsop87_pv_corps", &
                   partie_variable=ephi_fichier_ephem)
           case (4)
               call MSP_signaler_message(cle_mes='EPH_ERR_VSOP87_LECT_FIC', &
                   routine="ephi_vsop87_pv_corps", &
                   partie_variable=ephi_fichier_ephem)
           end select
           return 
        endif
       
        ! Conversion des unites astronomiques en km et km/s
        ! Note : La théorie VSOP87 s'appuie sur l'UAI 1976
        do i=1,3
           xpla_ecli(i) = xpla_ecli(i) *  eph_uakm_uai1976
        enddo
        do i=4,6
           xpla_ecli(i) = xpla_ecli(i) *  eph_uajkms_uai1976
        enddo
           
        ! Changement de repere de Ecliptique de 2000
        ! en equatorial moyen
        ! (origine AMLIB probable) MSP_obliquite = 0.40909261307192995_pm_reel
        call mr_ecliJ2000_J2000(xpla_ecli(1:3), xpla_pla_soleil(1:3), code_retour, &
            vit_ecliJ2000=xpla_ecli(4:6), vit_J2000=xpla_pla_soleil(4:6),&
            obliquite = 0.40909261307192995_PM_REEL)

        if (code_retour%valeur < 0) then
           call MSP_signaler_message(ier_mslib=code_retour, &
               routine="ephi_vsop87_pv_corps - appel a mr_ecliJ2000_J2000")
           return
        endif

      end subroutine ephi_vsop87_pv_corps



      subroutine ephi_verifier_entrees(code_corpsc, code_pla)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_verifier_entrees
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
!  call ephi_verifier_entrees(code_corpsc, code_pla)
!.    integer :: code_pla
!.    integer :: code_corpsc
!
!$Arguments
!>E     code_corpsc  :<integer>   
!>E     code_pla     :<integer>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
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

        use msp_gestion_erreur

        implicit none
        
        ! Entrées
        integer, intent(in) :: code_pla
        integer, intent(in) :: code_corpsc
        
        ! Varibales locales
        integer :: i
        logical :: code_corpsc_ok, code_pla_ok
        character(len=10) :: buff_messsage 

        ! Init
        code_corpsc_ok = .false.
        code_pla_ok = .false.

        ! Le soleil est accepté comme corps central ou corps d'intérêt
        ! Pas besoin de faire de recherche
        if ( code_pla == eph_soleil ) code_pla_ok = .true.
        if ( code_corpsc == eph_soleil ) code_corpsc_ok = .true.

        ! Recherche du corps central parmi les codes acceptés
        i = 1
        do while ( .not. code_corpsc_ok .and. i .le. EPHI_VSOP87_NB_CORPS ) 
           if ( code_corpsc ==  EPHI_VSOP87_CORPS(i) ) code_corpsc_ok = .true.
           i = i+1
        enddo
        ! Un des deux corps n'est pas OK => message d'erreur
        if ( .not. code_corpsc_ok ) then
           write(buff_messsage,'(I0)') code_corpsc
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_NUMPLA", &
                routine="ephi_verifier_entrees", &
                partie_variable=buff_messsage)
           return
        endif

        ! Recherche du corps d'intérêt parmi les codes acceptés
        ! la recherche ne se fait pas si echec lors du précédent
        i = 1
        do while ( .not. code_pla_ok .and. i .le. EPHI_VSOP87_NB_CORPS ) 
           if ( code_pla ==  EPHI_VSOP87_CORPS(i) ) code_pla_ok = .true.
           i = i+1
        enddo
        ! Un des deux corps n'est pas OK => message d'erreur
        if ( .not. code_pla_ok ) then
           write(buff_messsage,'(I0)' ) code_pla
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_NUMPLA", &
                routine="ephi_verifier_entrees", &
                partie_variable=buff_messsage)
           return
        endif
           
      end subroutine ephi_verifier_entrees


      subroutine ephi_VSOP87 (tdj,ivers,ibody,prec,lu,r,ierr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_VSOP87
!
!$Resume
!  Routine de lecture et de calcul des éphémérides héliocentriques d'un corps
!  pour la théorie VSOP87
!
!$Description
!  Routine de lecture et de calcul des éphémérides pour la théorie VSOP87.
!  Cette routine est adapté du code source fourni par l'IMCEE avec les
!  données de la théories.
!
!$Auteur
!  IMCEE
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_VSOP87 (tdj,ivers,ibody,prec,lu,r,ierr)
!.    real(kind=pm_reel) :: tdj
!.    real(kind=pm_reel) :: prec
!.    integer :: ivers, ibody, lu
!.    real(kind=pm_reel), dimension(6) :: r
!.    integer :: ierr
!
!$Arguments
!>E     tdj    :<pm_reel>           Date en jour juliens (depuis 4713 av JC)
!>E     ivers  :<integer>           Version de la théorie VSOP87
!>E     ibody  :<integer>           Code du corps d'intérêt 
!>E     prec   :<pm_reel>           Précision voulu (0.0 pour solution complète)
!>E     lu     :<integer>           Unite logique utilisée pour la lecture
!>S     r      :<pm_reel,DIM=(6)>   Vecteur position vitesse en (ua, et ua/j)
!>S     ierr   :<integer>           Code retour
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

!-----------------------------------------------------------------------
!
!     Reference : Bureau des Longitudes - PBGF9502
!
!     Object :
!
!     Substitution of time in VSOP87 solution written on a file.
!     The file corresponds to a version of VSOP87 theory and to a body.
!
!     Input :
!
!     tdj      julian date (real real(kind=pm_reel)).
!              time scale : dynamical time TDB.
!
!     ivers    version index (integer).
!              0: VSOP87 (initial solution).
!                 elliptic coordinates
!                 dynamical equinox and ecliptic J2000.
!              1: VSOP87A.
!                 rectangular coordinates
!                 heliocentric positions and velocities
!                 dynamical equinox and ecliptic J2000.
!              2: VSOP87A.
!                 spherical coordinates
!                 heliocentric positions and velocities
!                 dynamical equinox and ecliptic J2000.
!              3: VSOP87C.
!                 rectangular coordinates
!                 heliocentric positions and velocities
!                 dynamical equinox and ecliptic of the date.
!              4: VSOP87D.
!                 spherical coordinates
!                 heliocentric positions and velocities
!                 dynamical equinox and ecliptic of the date.
!              5: VSOP87E.
!                 rectangular coordinates
!                 barycentric positions and velocities
!                 dynamical equinox and ecliptic J2000.
!
!     ibody    body index (integer).
!              0: Sun
!              1: Mercury
!              2: Venus
!              3: Earth
!              4: Mars
!              5: Jupiter
!              6: Saturn
!              7: Uranus
!              8: Neptune
!              9: Earth-Moon barycenter
!
!     prec     relative precision (real real(kind=pm_reel)).
!
!              if prec is equal to 0 then the precision is the precision
!                 p0 of the complete solution VSOP87.
!                 Mercury    p0 =  0.6 10**-8
!                 Venus      p0 =  2.5 10**-8
!                 Earth      p0 =  2.5 10**-8
!                 Mars       p0 = 10.0 10**-8
!                 Jupiter    p0 = 35.0 10**-8
!                 Saturn     p0 = 70.0 10**-8
!                 Uranus     p0 =  8.0 10**-8
!                 Neptune    p0 = 42.0 10**-8
!
!              if prec is not equal to 0, let us say in between p0 and
!              10**-2, the precision is :
!                 for the positions :
!                 - prec*a0 au for the distances.
!                 - prec rd for the other variables.
!                 for the velocities :
!                 - prec*a0 au/day for the distances.
!                 - prec rd/day for the other variables.
!                   a0 is semi-major axis of the body.
!                 Mercury    a0 =  0.3871 ua
!                 Venus      a0 =  0.7233 ua
!                 Earth      a0 =  1.0000 ua
!                 Mars       a0 =  1.5237 ua
!                 Jupiter    a0 =  5.2026 ua
!                 Saturn     a0 =  9.5547 ua
!                 Uranus     a0 = 19.2181 ua
!                 Neptune    a0 = 30.1096 ua
!
!     lu       logical unit index of the file (integer).
!              The file corresponds to a version of VSOP87 theory and
!              a body, and it must be defined and opened before the
!              first call to subroutine VSOP87.
!
!     Output :
!
!     r(6)     array of the results (real real(kind=pm_reel)).
!
!              for elliptic coordinates :
!                  1: semi-major axis (au)
!                  2: mean longitude (rd)
!                  3: k = e*cos(pi) (rd)
!                  4: h = e*sin(pi) (rd)
!                  5: q = sin(i/2)*cos(omega) (rd)
!                  6: p = sin(i/2)*sin(omega) (rd)
!                     e:     eccentricity
!                     pi:    perihelion longitude
!                     i:     inclination
!                     omega: ascending node longitude
!
!              for rectangular coordinates :
!                  1: position x (au)
!                  2: position y (au)
!                  3: position z (au)
!                  4: velocity x (au/day)
!                  5: velocity y (au/day)
!                  6: velocity z (au/day)
!
!              for spherical coordinates :
!                  1: longitude (rd)
!                  2: latitude (rd)
!                  3: radius (au)
!                  4: longitude velocity (rd/day)
!                  5: latitude velocity (rd/day)
!                  6: radius velocity (au/day)
!
!     ierr     error index (integer).
!                  0: no error.
!                  1: file error (check up ivers index).
!                  2: file error (check up ibody index).
!                  3: precision error (check up prec parameter).
!                  4: reading file error.
!
!-----------------------------------------------------------------------
!
!     --------------------------------
!     Declarations and initializations
!     --------------------------------
!

      implicit none

      ! Entrées / sorties
      real(kind=pm_reel), intent(in) :: tdj
      real(kind=pm_reel), intent(in) :: prec
      integer, intent(in) :: ivers, ibody, lu

      real(kind=pm_reel), dimension(6), intent(out) :: r
      integer, intent(out) :: ierr

      ! Variable
      integer :: k, ideb, i, iv, ic, it, in, n, nn
      character(len=7) ::  bo,body(0:9)
      real(kind=pm_reel) :: dpi, t2000, a1000,p,a,b,c,u,cu,su,q

      real(kind=pm_reel) :: t(-1:5),a0(0:9)

      ! Données internes
      data body/'SUN','MERCURY','VENUS','EARTH','MARS','JUPITER', &
               'SATURN','URANUS','NEPTUNE','EMB'/
      data a0/0.01d0,0.3871d0,0.7233d0,1.d0,1.5237d0,5.2026d0, &
             9.5547d0,19.2181d0,30.1096d0,1.d0/
      data dpi/6.283185307179586d0/
      data t/0.d0,1.d0,5*0.d0/
      data t2000/2451545.d0/
      data a1000/365250.d0/
      k=0
      ideb=0
!
      rewind (lu,err=500)
      do i=1,6
         r(i)=0.d0
      enddo
!
      t(1)=(tdj-t2000)/a1000
      do i=2,5
         t(i)=t(1)*t(i-1)
      enddo
!
      ierr=3
      if (prec.lt.0.d0.or.prec.gt.1.d-2) return
      q=dmax1(3.d0,-dlog10(prec+1.d-50))
!
!     -------------------------------------
!     File reading and substitution of time
!     -------------------------------------
!
100   continue
      read (lu,1001,end=400,err=500) iv,bo,ic,it,in
!
      if (ideb.eq.0) then
         ideb=1
         ierr=1
         if (iv.ne.ivers) return
         ierr=2
         if (bo.ne.body(ibody)) return
         ierr=0
         if (iv.eq.0) k=2
         if (iv.eq.2.or.iv.eq.4) k=1
      endif
!
      if (in.eq.0) goto 100
!
      p=prec/10.d0/(q-2)/(dabs(t(it))+it*dabs(t(it-1))*1.d-4+1.d-50)
      if (k.eq.0.or.(k.ne.0.and.ic.eq.5-2*k)) p=p*a0(ibody)
!
      do 200 n=1,in
         nn=n
         read (lu,1002,err=500) a,b,c
         if (dabs(a).lt.p) goto 300
         u=b+c*t(1)
         cu=dcos(u)
         r(ic)=r(ic)+a*cu*t(it)
         if (iv.eq.0) goto 200
         su=dsin(u)
         r(ic+3)=r(ic+3)+t(it-1)*it*a*cu-t(it)*a*c*su
200   continue
!
      goto 100
300   continue
!
      if (nn.eq.in) goto 100
!
      do n=nn+1,in
         read (lu,1002,err=500)
      enddo
      goto 100
!
400   continue
      if (iv.ne.0) then
         do i=4,6
            r(i)=r(i)/a1000
         enddo
      endif
!
      if (k.eq.0) return
!
      r(k)=dmod(r(k),dpi)
      if (r(k).lt.0.d0) r(k)=r(k)+dpi
      return
!
500   continue
      ierr=4
      return
!
!     -------
!     Formats
!     -------
!
1001  format (17x,i1,4x,a7,12x,i1,17x,i1,i7)
1002  format (79x,f18.11,f14.11,f20.11)
!
      end subroutine ephi_vsop87


      subroutine eph_vsop87_infos(nom_fichier, code_corps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_vsop87_infos
!
!$Resume
!  Routine d'extraction des informations de la méthode
!
!$Description
!  Cette routine renvoie la liste des planètes traitées. Cette méthode
!  n'a pas de limite de validité temporelle. 
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_vsop87_infos(nom_fichier, code_corps)
!.    character(len=*) :: nom_fichier
!.    integer :: code_corps
!
!$Arguments
!>E     nom_fichier  :<LEN=*>     Nom du fichier de la méthode
!>S     code_corps   :<integer>   Code du corps correspondant
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- ephi_analyser_ext
!
!$Include
!
!$Module
!#V
!- eph_util
!#
!
!$Remarques
!    Cette méthode ne nécessite pas d'ouvrir les fichiers. Leur format et les
!    corps correspondants sont fixés.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        
        use eph_util

        implicit none
        
        ! Entrée / sortie
        character(len=*), intent(in) :: nom_fichier
        integer, intent(out) :: code_corps

        ! Variables internes
        integer :: indice

        ! Extraction du suffixe du fichier, son extension
        indice = index(nom_fichier,".")

        ! Cas d'erreur : le ficheir n'a pas de suffixe
        if ( indice == 0 ) then           
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_SUFFIXE", &
                  routine="eph_vsop87_infos", &
                  partie_variable=trim(nom_fichier))
           return
        endif

        ! Analyse du suffixe du fichier
        call ephi_analyser_ext(nom_fichier(indice:len_trim(nom_fichier)), code_corps)
        if ( MSP_gen_messages("eph_vsop87_infos")) return

      end subroutine eph_vsop87_infos

      subroutine ephi_convertir_codecorps(code_corps_naif, code_corps_vsop87)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_convertir_codecorps
!
!$Resume
!  Routine de conversion d'un code corps NAIF en code corps interne
!  à la méthode
!
!$Description
!  Routine de conversion d'un code corps NAIF en code corps interne
!  à la méthode
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_convertir_codecorps(code_corps_naif, code_corps_vsop87)
!.    integer :: code_corps_naif
!.    integer :: code_corps_vsop87
!
!$Arguments
!>E     code_corps_naif    :<integer>   Code corps NAIF en entrée
!>S     code_corps_vsop87  :<integer>   Code interne correspondant
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use msp_gestion_erreur

        implicit none

        ! Entrée / sortie
        integer, intent(in)  :: code_corps_naif
        integer, intent(out) :: code_corps_vsop87

        ! Variables locales
        character(len=10) :: buff_message
        integer :: ii
        logical :: trouve

        ! Init 
        code_corps_vsop87 = 0
        ii = 1
        trouve = .false.

        ! Parcours des codes disponibles
        do while ( ii <= EPHI_VSOP87_NB_CORPS .and. .not. trouve )
           if ( code_corps_naif == EPHI_VSOP87_CORPS(ii) ) then
              trouve = .true.
              code_corps_vsop87 = EPHI_VSOP87_CODES_INTERNE(ii)
           else
              ii = ii +1
           endif
        enddo

        ! Cas d'échec
        if ( .not. trouve ) then
           write (buff_message, *) code_corps_naif
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_CODEPLA", &
                  routine="ephi_convertir_codecorps", &
                  partie_variable=buff_message)              
        endif


      end subroutine ephi_convertir_codecorps


      subroutine ephi_extraire_ext(code_corps_naif, suffixe)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_extraire_ext
!
!$Resume
!
!$Description
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_extraire_ext(code_corps_naif, suffixe)
!.    integer :: code_corps_naif
!.    character(len=*) :: suffixe
!
!$Arguments
!>E     code_corps_naif  :<integer>   Code corps NAIF en entrée
!>S     suffixe          :<LEN=*>     Suffixe attendu pour le fichier 
!                                     éphémérides du corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
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

        use msp_gestion_erreur

        implicit none

        ! Entrée / sortie
        integer, intent(in)  :: code_corps_naif
        character(len=*), intent(out) :: suffixe

        ! Wariables locales
        character(len=10) :: buff_message
        integer :: ii
        logical :: trouve
       
        ! Initialisation
        suffixe="undefini"
        trouve = .false.
        ii = 1

        ! Parcours des codes disponibles
        do while ( ii <= EPHI_VSOP87_NB_CORPS .and. .not. trouve )
           if ( code_corps_naif == EPHI_VSOP87_CORPS(ii) ) then
              trouve = .true.
              suffixe = trim(EPHI_VSOP87_SUFFIXES(ii))
           else
              ii = ii +1
           endif
        enddo

        ! En cas d'échec
        if ( .not. trouve ) then
           write (buff_message, *) code_corps_naif
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_CODEPLA", &
                  routine="ephi_extraire_ext", &
                  partie_variable=buff_message)              
        endif

      end subroutine ephi_extraire_ext


      subroutine ephi_analyser_ext(suffixe, code_corps_naif)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_analyser_ext
!
!$Resume
!  Extraction du code NAIF du corps pour un suffixe de fichier
!
!$Description
!  Extraction du code NAIF du corps pour un suffixe de fichier
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_analyser_ext(suffixe, code_corps_naif)
!.    character(len=*) :: suffixe
!.    integer :: code_corps_naif
!
!$Arguments
!>E     suffixe          :<LEN=*>     Suffixe du fichier (ex : .emb)
!>S     code_corps_naif  :<integer>   Code NAIF du corps
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
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

        use msp_gestion_erreur

        implicit none

        ! Entrée / sortie
        character(len=*), intent(in) :: suffixe
        integer, intent(out)  :: code_corps_naif

        ! Wariables locales
        integer :: ii
        logical :: trouve
        
        ! Initialisation
        code_corps_naif = 0
        ii = 1
        trouve = .false.

        ! Parcours des suffixes disponibles
        do while ( ii <= EPHI_VSOP87_NB_CORPS .and. .not. trouve )
           if ( trim(suffixe) == EPHI_VSOP87_SUFFIXES(ii) ) then
              trouve = .true.
              code_corps_naif = EPHI_VSOP87_CORPS(ii)
           else
              ii = ii +1
           endif
        enddo

        ! En cas d'échec
        if ( .not. trouve ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_VSOP87_SUFFIXE", &
                  routine="ephi_analyser_ext", &
                  partie_variable=trim(suffixe))           
        endif

      end subroutine ephi_analyser_ext


end module eph_vsop87
