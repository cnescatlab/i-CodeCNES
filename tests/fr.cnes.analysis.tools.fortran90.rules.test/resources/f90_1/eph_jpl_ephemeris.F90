module eph_jpl_ephemeris

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_jpl_ephemeris
!
!$Resume
!  Methode de calcul des éphémérides du JPL.
!
!$Description
!  Methode de calcul avec utilisation de fichier JPL au format DE405.
!  Cette méthode est utilisée pour la théorie DE405 et les fichiers INPOP06
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Version
!  $Id: eph_jpl_ephemeris.F90 379 2013-02-22 16:59:49Z ffsm $
!
!$Historique
!  $Log: eph_jpl_ephemeris.F90,v $
!  Revision 379  2013/02/22 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.10  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2010/10/01 14:46:55  ogarat
!  VERSION::FA-ID:1430:01/10/2010: Variable inutilisee a retirer
!
!  Revision 1.8  2009/10/02 10:11:28  cmartel
!  FA-ID 1277 : Correction mineure du cartouche d'une routine
!
!  Revision 1.7  2009/10/01 13:48:26  cmartel
!  FA-ID 1277 : Ajout d'une routine d'extraction des infos de la methode
!
!  Revision 1.6  2009/09/09 09:17:03  cmartel
!  FA-ID 1264 : Correction des tests d'égalité entre réels
!  Revision 1.5  2009/03/09 15:15:53  cml
!  DM-ID 960 : Correction de variables inutilisees
!  Revision 1.4  2009/02/26 15:28:30  cml
!  DM-ID 960 : Modif. des flags FIRST pour reutilisation pour un second fichier
!  Revision 1.3  2009/02/17 14:09:51  cml
!  DM-ID 960 : Amelioration du traitement des erreurs
!  Revision 1.2  2009/02/12 15:40:49  cml
!  DM-ID 960 : La taille des blocs est calculee par fsizer2 et non plus fixee
!  Revision 1.1  2009/02/11 10:11:42  cml
!  DM-ID 960 : Ajout de la methode de calcul JPL - DE405 des ephemerides
!
!$FinHistorique
!
!$Usage
!  use eph_jpl_ephemeris
!
!$Structure
!
!: ephi_struct_CHRHDR : 
!>     CNAM    : <LEN=6,DIM=(400)>     
!>     TTL     : <LEN=6,DIM=(14,3)>    
!
!: ephi_struct_STCOMX : Structure regroupant les options de calcul par défaut
!>     KM      : <logical>             Flag qui définit les unités physiques en sortie
!.                                    KM = .true.    en km et km/sec
!.                                    KM = .false.   en ua et ua/jour
!.                                    (valeur par defaut .false.)       
!>     BARY    : <logical>             Flag de définition du corps central en sortie
!.                                    BARY = .true.  le centre est le barycentre du 
!                                     système solaire
!.                                    BARY = .false. le centre est le soleil
!.                                    (valeur par defaut .false.)       
!>     PVSUN   : <PM_REEL,DIM=(6)>     Tableau de 6 réels contenant la position
!                                     barycentrique et la vitesse du soleil 
!
!: ephi_struct_EPHHDR : 
!>     CVAL    : <PM_REEL,DIM=(400)>   
!>     SS      : <PM_REEL,DIM=(3)>     
!>     AU      : <PM_REEL>             
!>     EMRAT   : <PM_REEL>             
!>     NUMDE   : <integer>             
!>     NCON    : <integer>             
!>     IPT     : <integer,DIM=(3,13)>  
!
!$Global
!
!>  ephi_CHRHDR               : <ephi_struct_CHRHDR>  Structure globale  
!>  ephi_EPHHDR               : <ephi_struct_EPHHDR>  Structure globale 
!>  ephi_STCOMX               : <ephi_struct_STCOMX>  Structure regroupant les options
!>  ephi_fichier              : <LEN=256>             Path d'accès au fichier d'éphéméride            
!>  ephi_num_unit             : <integer>             Numéro d'unité logique utilisée pour la lecture           
!>  ephi_init_jpl_ephemeris   : <logical>             Flag définissant si la méhtoide a été initialisée           
!>  ephi_KSIZE                : <integer>             Taille en bit nécessaire à la lecture
!>  ephi_NRECL                : <integer>             Type d'enregistrement
!>  ephi_FIRST_PLEPH          : <logical>             Flag indiquant si la routine a été appelée
!>  ephi_FIRST_STATE          : <logical>             Flag indiquant si la routine a été appelée
!>  ephi_FIRST_CONST          : <logical>             Flag indiquant si la routine a été appelée
!$Common
!
!$Routines
!- eph_pvjpl
!- eph_initjpl
!- eph_closejpl
!- eph_jpl_infos
!- ephi_convertir_codecorps
!- ephi_verifier_entrees
!- ephi_PLEPH
!- ephi_INTERP
!- EPHI_SPLIT
!- ephi_STATE
!- ephi_CONST
!- ephi_FSIZER3
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- mslib
!- msp_gestion_erreur
!- eph_util
!- eph_constantes
!#
!
!$Interface
!#V
!#
!
!$Remarques
!   Attention, la méthode est écrite pour les fichiers DE405 seulement !
!   Les fichiers DE400, DE403 etc... utilise une variable KSIZE différente.
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_pvjpl eph_initjpl eph_closejpl eph_jpl_infos ephi_convertir_codecorps ephi_verifier_entrees
!.  ephi_PLEPH
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use mslib 
  use msp_gestion_erreur

  ! 
  ! Definition des types internes
  !
  type ephi_struct_EPHHDR
    REAL(KIND=PM_REEL), dimension(400) :: CVAL
    REAL(KIND=PM_REEL), dimension(3) :: SS
    REAL(KIND=PM_REEL) :: AU
    REAL(KIND=PM_REEL) :: EMRAT
    integer :: NUMDE
    integer :: NCON
    integer, dimension(3,13) :: IPT
  end type ephi_struct_EPHHDR

  type ephi_struct_STCOMX
     logical :: KM
     logical :: BARY
     REAL(KIND=PM_REEL), dimension(6) :: PVSUN
  end type ephi_struct_STCOMX

  type ephi_struct_CHRHDR
     CHARACTER(LEN=6) :: CNAM(400)
     CHARACTER(LEN=6) :: TTL(14,3)
  end type ephi_struct_CHRHDR

  !
  ! Definition des variables globales
  !

  type(ephi_struct_CHRHDR) :: ephi_CHRHDR
  type(ephi_struct_EPHHDR) :: ephi_EPHHDR
  type(ephi_struct_STCOMX) :: ephi_STCOMX

  CHARACTER(LEN=256) :: ephi_fichier
  INTEGER :: ephi_num_unit
  LOGICAL :: ephi_init_jpl_ephemeris = .false.
  INTEGER :: ephi_KSIZE, ephi_NRECL
  LOGICAL :: ephi_FIRST_PLEPH = .true.
  LOGICAL :: ephi_FIRST_STATE = .true.
  LOGICAL :: ephi_FIRST_CONST = .true.
  



! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_jpl_ephemeris.F90 379 2013-02-22 16:59:49Z ffsm $'

contains

      subroutine eph_pvjpl(numpla, ncorpsc, t1950, xpla)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_pvjpl
!
!$Resume
!   Routine de calcul des éphémérides au format JPL
!
!$Description
!   Routine principale de calcul. Nécessite un appel à la routine 
!   d'intialisation
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_pvjpl(numpla, ncorpsc, t1950, xpla)
!.    integer :: numpla
!.    integer :: ncorpsc
!.    real(kind = pm_reel) :: t1950
!.    real(kind = pm_reel), dimension(6) :: xpla
!
!$Arguments
!>E     numpla   :<integer>           Code NAIF du corps d'intérêt
!>E     ncorpsc  :<integer>           Code NAIF du corps central
!>E     t1950    :<pm_reel>           Date en jour juliens 1950 fractionnaires
!>S     xpla     :<pm_reel,DIM=(6)>   Vecteur position vitesse calculé
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- ephi_convertir_codecorps
!- eph_jjfrac1950_jjuliens
!- ephi_verifier_entrees
!- ephi_PLEPH
!
!$Include
!
!$Module
!#V
!- eph_util
!#
!
!$Remarques
!   Une exception est levée si un des codes corps fournit n'est pas traité
!   ou si la date est hors de l'intervalle du fichier d'éphémérides
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use eph_util, only : eph_jjfrac1950_jjuliens

        implicit none

        integer, intent(in) :: numpla
        integer, intent(in) :: ncorpsc
        real(kind = pm_reel), intent(in) :: t1950
        real(kind = pm_reel), dimension(6), intent(out) :: xpla

        ! Variables internes
        integer :: code_pla_jpl
        integer :: code_ncorpsc_jpl
!   JPL
        real(kind = pm_reel) :: jjuliens

        ! Verification de l'initialisation
        if ( .not. ephi_init_jpl_ephemeris ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_INIT", &
                routine="eph_pvjpl")
           return
        endif

        ! Conversion des codes corps en entrée
        call ephi_convertir_codecorps(numpla, code_pla_jpl)
        if ( MSP_ERREUR ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_ENTREES", &
                routine="eph_pvjpl")
           return
        endif
        call ephi_convertir_codecorps(ncorpsc, code_ncorpsc_jpl)
        if ( MSP_ERREUR ) then
          call MSP_signaler_message(cle_mes="EPH_ERR_JPL_ENTREES", &
                routine="eph_pvjpl")
           return
        endif
        

        ! Conversion (pas de gestion d'erreur particulière)
        call eph_jjfrac1950_jjuliens(t1950, jjuliens)
        
        ! Vérification des données
!   JPL
        call ephi_verifier_entrees(code_pla_jpl, code_ncorpsc_jpl)
        if ( MSP_ERREUR ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_ENTREES", &
                routine="eph_pvjpl")
           return
        endif

        ! Appel au calcul d'éphémérides
        call ephi_PLEPH ( jjuliens, code_pla_jpl, code_ncorpsc_jpl, xpla)
        if ( MSP_ERREUR ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_CALCUL", &
                routine="eph_pvjpl")
           return
        endif

      end  subroutine eph_pvjpl

      subroutine eph_initjpl(num_unit, fichier) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_initjpl
!
!$Resume
!  Routine d'initialisation de la méthode
!
!$Description
!  Routine d'initialisation de la méthode
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_initjpl(num_unit, fichier) 
!.    integer :: num_unit
!.    character(LEN=*) :: fichier
!
!$Arguments
!>E     num_unit  :<integer>   Unité logique disponible pour la lecture
!>E     fichier   :<LEN=*>     Fichier d'éphéméride utilisé
!
!$Common
!
!$Routines
!- ephi_FSIZER2
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
        character(LEN=*), intent(in) :: fichier

        ! Fermetude d'un eventuel fichier deja ouvert
        if ( ephi_init_jpl_ephemeris ) close(ephi_num_unit)

        ! Recopies des valeurs en interne
        ephi_num_unit = num_unit
        ephi_fichier = trim(fichier)
        ephi_init_jpl_ephemeris = .true.
        ephi_FIRST_PLEPH = .true.
        ephi_FIRST_CONST = .true.
        ephi_FIRST_STATE = .true.

        ! On fixe des options internes
        ephi_STCOMX%KM = .true.
        ephi_STCOMX%BARY = .false.
        
        ! Appel a la methode de definition des taille
        CALL ephi_FSIZER2(ephi_NRECL,ephi_KSIZE)

      end subroutine eph_initjpl


  
      subroutine eph_closejpl() 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closejpl
!
!$Resume
!  Routine de finalisation de la méthode
!
!$Description
!  Routine de finalisation de la méthode
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_closejpl() 
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
        integer :: io_stat

        ! Fermetude d'un eventuel fichier ouvert
        if ( ephi_init_jpl_ephemeris ) close(ephi_num_unit, iostat=io_stat)

        ! Remise a zero du flag
        ephi_init_jpl_ephemeris = .false.
        ephi_FIRST_PLEPH = .true.
        ephi_FIRST_CONST = .true.
        ephi_FIRST_STATE = .true.


      end subroutine eph_closejpl


      subroutine eph_jpl_infos(nb_corps, liste_corps, date_debut, date_fin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_jpl_infos
!
!$Resume
!  Routine d'extraction des infos du fichiers d'éphémérides
!
!$Description
!  Extraction de l'intervalle de validité du fichier et des corps traités par la méthode.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_jpl_infos(nb_corps, liste_corps, date_debut, date_fin)
!.    integer :: nb_corps
!.    integer, dimension(:) :: liste_corps
!.    real(kind = pm_reel) :: date_debut
!.    real(kind = pm_reel) :: date_fin
!
!$Arguments
!>S     nb_corps     :<integer>           nombre de corps acceptés par la méthode
!>S     liste_corps  :<integer,DIM=(:)>   liste des codes NAIF des corps 
!                                         acceptés en entrée
!>S     date_debut   :<pm_reel>           date de début en jour julien 1950 fractionnaire
!>S     date_fin     :<pm_reel>           date de fin en jour julien 1950 fractionnaire
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- eph_jjuliens_jjfrac1950
!- eph_initjpl
!
!$Include
!
!$Module
!#V
!- eph_constantes
!- eph_util
!#
!
!$Remarques
!   Cette routine nécessite une initialisation au préalable de toute utilisation.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        
        use eph_constantes
        use eph_util

        implicit none
        
        ! Sorties
        integer, intent(out) :: nb_corps
        integer, dimension(:), intent(out) :: liste_corps
        real(kind = pm_reel), intent(out) :: date_debut
        real(kind = pm_reel), intent(out) :: date_fin
             
        ! Constantes
        ! Nombre de planètes de la méthode
        integer, parameter :: NB_PLA_JPL = 13
        ! Liste fixe des planètes accessibles
        integer, dimension(NB_PLA_JPL), parameter :: liste_planetes = (/ &
             eph_mercure, eph_venus, eph_terre, eph_mars, eph_jupiter, &
             eph_saturne, eph_uranus, eph_neptune, eph_pluton, eph_lune, &
             eph_soleil, eph_bary_soleil, eph_bary_terre /)

        ! Variables locales
        integer :: ii, jj        ! Indices de parcours
        integer :: irecsz        ! Ensemble de variables lues sur la première ligne
        integer :: NCOEFFS
        CHARACTER(LEN=6) :: CNAM(400)
        CHARACTER(LEN=252) :: TTL
        REAL(KIND=PM_REEL), dimension(3) :: SS
        REAL(KIND=PM_REEL) :: AU
        REAL(KIND=PM_REEL) :: EMRAT
        integer :: NUMDE
        integer :: NCON
        integer, dimension(3,13) :: IPT

        ! Initialisations
        nb_corps = 0
        liste_corps(:) = 0
        date_debut = 0.0_pm_reel
        date_fin = 0.0_pm_reel

        ! Verif de l'initialisation de la methode avec le fichier
        if ( .not. ephi_init_jpl_ephemeris ) then
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_INIT", &
                routine="eph_pvjpl")
           return
        endif

        ! Extrait du code qui lit la ligne d'entete  
        IRECSZ=ephi_NRECL*ephi_KSIZE
        NCOEFFS=ephi_KSIZE/2

        OPEN(ephi_num_unit, & 
            FILE=ephi_fichier, &
            ACCESS='DIRECT', &
            FORM='UNFORMATTED', &
            RECL=IRECSZ, &
#ifdef __GFORTRAN__
            CONVERT='big_endian', &
#endif
            STATUS='OLD')

        READ(ephi_num_unit,REC=1) TTL, CNAM, SS, NCON, AU, EMRAT, &
            ((IPT(ii,jj),ii=1,3),jj=1,12), NUMDE, &
            (IPT(ii,13),ii=1,3)
        
        ! Conversion de jours juliens astronomiques en jour juliens 1950
        call eph_jjuliens_jjfrac1950(SS(1), date_debut)        
        call eph_jjuliens_jjfrac1950(SS(2), date_fin)

        ! Recopie de la liste
        liste_corps(1:NB_PLA_JPL) = liste_planetes
        nb_corps = NB_PLA_JPL

        ! Fermetude d'un eventuel fichier deja ouvert
        ! Réouverture pour future utilisation
        call eph_initjpl(ephi_num_unit, ephi_fichier) 

      end subroutine eph_jpl_infos


      subroutine ephi_convertir_codecorps(code_corps_naif, code_corps_jpl)

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
!  call ephi_convertir_codecorps(code_corps_naif, code_corps_jpl)
!.    integer :: code_corps_naif
!.    integer :: code_corps_jpl
!
!$Arguments
!>E     code_corps_naif  :<integer>   Code corps NAIF en entrée
!>S     code_corps_jpl   :<integer>   Code interne correspondant
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

        use eph_constantes

        implicit none

        integer, intent(in)  :: code_corps_naif
        integer, intent(out) :: code_corps_jpl

        character(len=5) :: buff_message

        ! Selection du code
        select case (code_corps_naif)
        case (eph_mercure) 
           code_corps_jpl = 1
        case (eph_venus)
           code_corps_jpl = 2
        case (eph_terre) 
           code_corps_jpl = 3
        case (eph_mars) 
           code_corps_jpl = 4
        case (eph_jupiter) 
           code_corps_jpl = 5
        case (eph_saturne) 
           code_corps_jpl = 6
        case (eph_uranus) 
           code_corps_jpl = 7
        case (eph_neptune) 
           code_corps_jpl = 8
        case (eph_pluton) 
           code_corps_jpl = 9

        case (eph_lune) 
           code_corps_jpl = 10

        case (eph_soleil) 
           code_corps_jpl = 11
        case (eph_bary_soleil) 
           code_corps_jpl = 12

        case (eph_bary_terre) 
           code_corps_jpl = 13
        case default 
           write(buff_message, '(I0)') code_corps_naif
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_CODEPLA", &
                  routine="ephi_convertir_codecorps", &
                  partie_variable=buff_message)              
        end select

      end subroutine ephi_convertir_codecorps


      subroutine ephi_verifier_entrees(code_pla_jpl, code_ncorpsc_jpl)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_verifier_entrees
!
!$Resume
!  Méthode de vérification de la validité des entrées de la méthode
!
!$Description
!  Méthode de vérification de la validité des entrées de la méthode
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_verifier_entrees(code_pla_jpl, code_ncorpsc_jpl)
!.    integer :: code_pla_jpl
!.    integer :: code_ncorpsc_jpl
!
!$Arguments
!>E     code_pla_jpl      :<integer>   Code corps interne du corps d'intérêt
!>E     code_ncorpsc_jpl  :<integer>   Code corps interne du corps central
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

        integer, intent(in) :: code_pla_jpl
        integer, intent(in) :: code_ncorpsc_jpl

        character(len=10) :: buff_message

        ! Création d'une erreur MAGE si le corps n'est pas traité
        if ( code_pla_jpl < 1 &
             .or. code_pla_jpl > 13 &
             .or. code_ncorpsc_jpl < 1 &
             .or. code_ncorpsc_jpl > 13 ) then
           ! Création du message d'erreur
           write (buff_message, *) code_ncorpsc_jpl
           call MSP_signaler_message(cle_mes="EPH_ERR_JPL_CODEPLA", &
                  routine="ephi_verifier_entrees", &
                  partie_variable=buff_message)
           return
        endif

      end subroutine ephi_verifier_entrees


      SUBROUTINE ephi_PLEPH ( ET, NTARG, NCENT, RRD )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_PLEPH
!
!$Resume
!  Méthode de calcul des éphémérides au format JPL - DE405
!
!$Description
!  Méthode de calcul de la position
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_PLEPH ( ET, NTARG, NCENT, RRD )
!.    REAL(KIND=PM_REEL) :: ET
!.    integer :: NTARG
!.    integer :: NCENT
!.    REAL(KIND=PM_REEL) :: RRD(6)
!
!$Arguments
!>E     ET     :<PM_REEL>           Date en jours juliens (depuis 4713 av. J.C.)           
!>E     NTARG  :<integer>           Code interne du corps d'intérêt
!>E     NCENT  :<integer>           Code interne du corps central
!>S     RRD    :<PM_REEL,DIM=(6)>   Vecteur position vitesse calculé
!
!$Common
!
!$Routines
!- ephi_STATE
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!  Cette méthode permet de calculer la nutation (NTARG = 14) et la précession
!  (NTARG = 15). Toutefois l'interface publique ne le permet pas volontairement.
!  La théorie INPOP6 ne marche pas notamment  
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!++++++++++++++++++++++++++
!  NOTE : Over the years, different versions of PLEPH have had a fifth argument:
!  sometimes, an error return statement number; sometimes, a logical denoting
!  whether or not the requested date is covered by the ephemeris.  We apologize
!  for this inconsistency; in this present version, we use only the four necessary 
!  arguments and do the testing outside of the subroutine.
!
!
!
!     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS
!     AND GIVES THE POSITION AND VELOCITY OF THE POINT 'NTARG'
!     WITH RESPECT TO 'NCENT'.
!
!     CALLING SEQUENCE PARAMETERS:
!
!       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION
!            IS WANTED.
!
!       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
!          THE REASON FOR THIS OPTION IS DISCUSSED IN THE 
!          SUBROUTINE STATE
!
!     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
!
!     NCENT = INTEGER NUMBER OF CENTER POINT.
!
!            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
!
!                1 = MERCURY           8 = NEPTUNE
!                2 = VENUS             9 = PLUTO
!                3 = EARTH            10 = MOON
!                4 = MARS             11 = SUN
!                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
!                6 = SATURN           13 = EARTH-MOON BARYCENTER
!                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
!                            15 = LIBRATIONS, IF ON EPH FILE
!
!             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
!              SET NTARG = 15. SET NCENT=0.)
!
!      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
!            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
!            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
!            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
!            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
!            RADIANS AND RADIANS/DAY.
!
!            The option is available to have the units in km and km/sec.
!            For this, set km=.true. in the STCOMX common block.
!

      implicit none

      !
      ! Variables d'entrée / sortie
      !
      REAL(KIND=PM_REEL), intent(in) :: ET
      integer, intent(in) :: NTARG
      integer, intent(in) :: NCENT
      REAL(KIND=PM_REEL), intent(out) ::RRD(6)

      !
      ! Variables locales
      !
      REAL(KIND=PM_REEL) ::ET2(2),PV(6,13)
      REAL(KIND=PM_REEL) ::zips(2)
      REAL(KIND=PM_REEL) ::PNUT(4)
      data zips/2*0.0_pm_reel/

      LOGICAL BSAVE

      INTEGER LIST(12)
      integer :: i,k

!     INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
!
      ET2(1)=ET
      ET2(2)=0.0_pm_reel
      
      IF(ephi_FIRST_PLEPH) CALL ephi_STATE(zips,list,pv(:,1:12),pnut)
      ! Propagation d'erreurs
      if (MSP_gen_messages("ephi_PLEPH")) return
      ephi_FIRST_PLEPH=.FALSE.

      IF(NTARG .EQ. NCENT) RETURN

      DO I=1,12
      LIST(I)=0
      ENDDO

!     CHECK FOR NUTATION CALL

      IF(NTARG.NE.14) GO TO 97
        IF(ephi_EPHHDR%IPT(3, 9).GT.0) THEN
          LIST(11)=2
          CALL ephi_STATE(ET2,LIST,PV(:,1:12),RRD(1:4))
          ! Propagation d'erreurs
          if ( MSP_gen_messages("ephi_PLEPH") ) return
          RETURN
        ELSE
          do i=1,4
          rrd(i)=0.0_pm_reel
          enddo
          ! Erreur MAGE au lieu d'un affichage sur la sortie standard
          call MSP_signaler_message(cle_mes="EPH_ERR_JPL_NUTATION", &
                  routine="ephi_PLEPH")
          return
        ENDIF

!     CHECK FOR LIBRATIONS

  97  do i=1,6
      rrd(i)=0.0_pm_reel
      enddo

      IF(NTARG.NE.15) GO TO 98
        IF(ephi_EPHHDR%IPT(3,12).GT.0) THEN
          LIST(12)=2
          CALL ephi_STATE(ET2,LIST,PV(:,1:12),RRD(1:4))
          ! Propagation d'erreurs
          if ( MSP_gen_messages("ephi_PLEPH") ) return
          DO I=1,6
          RRD(I)=PV(I,11)
          ENDDO
          RETURN
        ELSE
          ! Erreur MAGE au lieu d'un affichage sur la sortie standard
          call MSP_signaler_message(cle_mes="EPH_ERR_JPL_PRECESSION", &
                  routine="ephi_PLEPH")
          return
        ENDIF

!       FORCE BARYCENTRIC OUTPUT BY 'ephi_STATE'

  98  BSAVE=ephi_STCOMX%BARY
      ephi_STCOMX%BARY=.TRUE.

!       SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR ephi_STATE CALL

      DO I=1,2
      K=NTARG
      IF(I .EQ. 2) K=NCENT
      IF(K .LE. 10) LIST(K)=2
      IF(K .EQ. 10) LIST(3)=2
      IF(K .EQ. 3) LIST(10)=2
      IF(K .EQ. 13) LIST(3)=2
      ENDDO

!       MAKE CALL TO ephi_STATE

      CALL ephi_STATE(ET2,LIST,PV(:,1:12),RRD(1:4))
      ! Propagation d'erreurs
      if ( MSP_gen_messages("ephi_PLEPH") ) return

      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
      PV(I,11)=ephi_STCOMX%PVSUN(I)
      ENDDO
      ENDIF

      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
      DO I=1,6
      PV(I,12)=0.0_pm_reel
      ENDDO
      ENDIF

      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,13)=PV(I,3)
      ENDDO
      ENDIF

      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
      DO I=1,6
      PV(I,3)=0.0_pm_reel
      ENDDO
      GO TO 99
      ENDIF

      IF(LIST(3) .EQ. 2) THEN
      DO I=1,6
      PV(I,3)=PV(I,3)-PV(I,10)/(1.0_pm_reel+ephi_EPHHDR%EMRAT)
      ENDDO
      ENDIF

      IF(LIST(10) .EQ. 2) THEN
      DO I=1,6
      PV(I,10)=PV(I,3)+PV(I,10)
      ENDDO
      ENDIF

  99  DO I=1,6
      RRD(I)=PV(I,NTARG)-PV(I,NCENT)
      ENDDO

      ephi_STCOMX%BARY=BSAVE

      RETURN
      END subroutine ephi_PLEPH


      SUBROUTINE ephi_INTERP(BUF,T,NCF,NCM,NA,IFL,PV)

!++++++++++++++++++++++++++++++++
!
!     THIS SUBROUTINE DIFFERENTIATES AND INTERPOLATES A
!     SET OF CHEBYSHEV COEFFICIENTS TO GIVE POSITION AND VELOCITY
!
!     CALLING SEQUENCE PARAMETERS:
!
!       INPUT:
!
!         BUF   1ST LOCATION OF ARRAY OF D.P. CHEBYSHEV COEFFICIENTS OF POSITION
!
!           T   T(1) IS DP FRACTIONAL TIME IN INTERVAL COVERED BY
!               COEFFICIENTS AT WHICH INTERPOLATION IS WANTED
!               (0 .LE. T(1) .LE. 1).  T(2) IS DP LENGTH OF WHOLE
!               INTERVAL IN INPUT TIME UNITS.
!
!         NCF   # OF COEFFICIENTS PER COMPONENT
!
!         NCM   # OF COMPONENTS PER SET OF COEFFICIENTS
!
!          NA   # OF SETS OF COEFFICIENTS IN FULL ARRAY
!               (I.E., # OF SUB-INTERVALS IN FULL INTERVAL)
!
!          IFL  INTEGER FLAG: =1 FOR POSITIONS ONLY
!                             =2 FOR POS AND VEL
!
!
!       OUTPUT:
!
!         PV   INTERPOLATED QUANTITIES REQUESTED.  DIMENSION
!               EXPECTED IS PV(NCM,IFL), DP.
!
!

      implicit none

      SAVE

      !
      ! Variables d'entrée / sortie
      !
      REAL(KIND=PM_REEL), dimension(NCF,NCM,*), intent(in) :: BUF
      REAL(KIND=PM_REEL), intent(in) :: T(2)
      integer, intent(in) :: NCF,NCM,NA,IFL
      REAL(KIND=PM_REEL), intent(out) :: PV(NCM,*)

      !
      ! Variables locales
      !
      REAL(KIND=PM_REEL) :: PC(18)
      REAL(KIND=PM_REEL) :: VC(18)
      REAL(KIND=PM_REEL) :: DNA, DT1, TEMP, TC, VFAC
      integer :: I,J,L

!
      integer :: NP, NV
      REAL(KIND=PM_REEL) :: TWOT
      DATA NP/2/
      DATA NV/3/
      DATA TWOT/0.0_pm_reel/
      DATA PC(1),PC(2)/1.0_pm_reel,0.0_pm_reel/
      DATA VC(2)/1.0_pm_reel/
!
!       ENTRY POINT. GET CORRECT SUB-INTERVAL NUMBER FOR THIS SET
!       OF COEFFICIENTS AND THEN GET NORMALIZED CHEBYSHEV TIME
!       WITHIN THAT SUBINTERVAL.
!
      DNA=DBLE(NA)
      DT1=DINT(T(1))
      TEMP=DNA*T(1)
      L=IDINT(TEMP-DT1)+1

!         TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)

      TC=2.0_pm_reel*(DMOD(TEMP,1.0_pm_reel)+DT1)-1.0_pm_reel

!       CHECK TO SEE WHETHER CHEBYSHEV TIME HAS CHANGED,
!       AND COMPUTE NEW POLYNOMIAL VALUES IF IT HAS.
!       (THE ELEMENT PC(2) IS THE VALUE OF T1(TC) AND HENCE
!       CONTAINS THE VALUE OF TC ON THE PREVIOUS CALL.)

! Correction ATOS : Comparaison de réels (<=> TC /= PC(2)
      IF(ABS(TC - PC(2)) > EPSILON(1.0_pm_reel) ) THEN
        NP=2
        NV=3
        PC(2)=TC
        TWOT=TC+TC
      ENDIF
!
!       BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
!       AND ARE STORED IN THE ARRAY 'PC'.
!
      IF(NP.LT.NCF) THEN
        DO 1 I=NP+1,NCF
        PC(I)=TWOT*PC(I-1)-PC(I-2)
    1   CONTINUE
        NP=NCF
      ENDIF
!
!       INTERPOLATE TO GET POSITION FOR EACH COMPONENT
!
      DO 2 I=1,NCM
      PV(I,1)=0.0_pm_reel
      DO 3 J=NCF,1,-1
      PV(I,1)=PV(I,1)+PC(J)*BUF(J,I,L)
    3 CONTINUE
    2 CONTINUE
      IF(IFL.LE.1) RETURN
!
!       IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
!       DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
!
      VFAC=(DNA+DNA)/T(2)
      VC(3)=TWOT+TWOT
      IF(NV.LT.NCF) THEN
        DO 4 I=NV+1,NCF
        VC(I)=TWOT*VC(I-1)+PC(I-1)+PC(I-1)-VC(I-2)
    4   CONTINUE
        NV=NCF
      ENDIF
!
!       INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
!
      DO 5 I=1,NCM
      PV(I,2)=0.0_pm_reel
      DO 6 J=NCF,2,-1
      PV(I,2)=PV(I,2)+VC(J)*BUF(J,I,L)
    6 CONTINUE
      PV(I,2)=PV(I,2)*VFAC
    5 CONTINUE
!
      RETURN
!
      END subroutine ephi_INTERP



      SUBROUTINE EPHI_SPLIT(TT,FR)

!++++++++++++++++++++++++++++++++
!
!     THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
!     AND A D.P. FRACTIONAL PART.
!
!     CALLING SEQUENCE PARAMETERS:
!
!       TT = D.P. INPUT NUMBER
!
!       FR = D.P. 2-WORD OUTPUT ARRAY.
!            FR(1) CONTAINS INTEGER PART
!            FR(2) CONTAINS FRACTIONAL PART
!
!            FOR NEGATIVE INPUT NUMBERS, FR(1) CONTAINS THE NEXT
!            MORE NEGATIVE INTEGER; FR(2) CONTAINS A POSITIVE FRACTION.
!
!       CALLING SEQUENCE DECLARATIONS
!
      implicit none

      !
      ! Variables d'entrée / sortie
      !
      REAL(KIND=PM_REEL), intent(in) :: TT
      REAL(KIND=PM_REEL), intent(out) :: FR(2)

!       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS

      FR(1)=DINT(TT)
      FR(2)=TT-FR(1)

      IF(TT.GE.0.0_pm_reel .OR. FR(2).EQ.0.0_pm_reel) RETURN

!       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER

      FR(1)=FR(1)-1.0_pm_reel
      FR(2)=FR(2)+1.0_pm_reel

      RETURN

      END subroutine EPHI_SPLIT


      SUBROUTINE ephi_STATE(ET2,LIST,PV,PNUT)

!++++++++++++++++++++++++++++++++
!
! THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
!
!     CALLING SEQUENCE PARAMETERS:
!
!     INPUT:
!
!         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
!               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
!               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
!
!                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
!                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
!
!                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
!                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
!                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
!                   ELAPSED BETWEEN ET2(1) AND EPOCH.
!
!                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
!                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
!                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
!
!        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
!               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
!
!                         LIST(I)=0, NO INTERPOLATION FOR BODY I
!                                =1, POSITION ONLY
!                                =2, POSITION AND VELOCITY
!
!               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
!
!                         I = 1: MERCURY
!                           = 2: VENUS
!                           = 3: EARTH-MOON BARYCENTER
!                           = 4: MARS
!                           = 5: JUPITER
!                           = 6: SATURN
!                           = 7: URANUS
!                           = 8: NEPTUNE
!                           = 9: PLUTO
!                           =10: GEOCENTRIC MOON
!                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
!                           =12: LUNAR LIBRATIONS (IF ON FILE)
!
!
!     OUTPUT:
!
!          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
!               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
!               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
!               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
!               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
!               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
!               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
!               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
!
!               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
!               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
!               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
!
!               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
!               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
!               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
!
!               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
!               LIST(12) IS 1 OR 2.
!
!         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
!               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
!               QUANTITIES IN NUT IS:
!
!                        D PSI  (NUTATION IN LONGITUDE)
!                        D EPSILON (NUTATION IN OBLIQUITY)
!                        D PSI DOT
!                        D EPSILON DOT
!
!           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
!               RANGE OR I/O ERRORS.
!
!
!
!

      implicit none
      
      SAVE

      !
      ! Variables d'entrée / sortie
      !
      REAL(KIND=PM_REEL), intent(in) :: ET2(2)
      integer, intent(in) :: LIST(12)
      REAL(KIND=PM_REEL), intent(out) :: PV(6,12)
      REAL(KIND=PM_REEL), intent(out) :: PNUT(4)

      !
      ! Variables locales
      !
      REAL(KIND=PM_REEL) :: T(2)
      REAL(KIND=PM_REEL) :: PJD(4)
      REAL(KIND=PM_REEL) :: BUF(1500)

      integer :: IRECSZ, NRL, NCOEFFS, NR
      integer :: I,J,K
      REAL(KIND=PM_REEL) :: tmp1, tmp2
      REAL(KIND=PM_REEL) :: S, AUFAC

      ! Traitement des erreurs
      character (len=256), dimension(3) :: msp_mess
!
!       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
!
      IF(ephi_FIRST_STATE) THEN
        ephi_FIRST_STATE=.FALSE.

      
! ************************************************************************
! ************************************************************************

        ! L'appel a FSizer est faite pendant l'initialisation

! ************************************************************************
! ************************************************************************

      IRECSZ=ephi_NRECL*ephi_KSIZE
      NCOEFFS=ephi_KSIZE/2

      OPEN(ephi_num_unit, &
            FILE=ephi_fichier, &
            ACCESS='DIRECT', &
            FORM='UNFORMATTED', &
            RECL=IRECSZ, &
#ifdef __GFORTRAN__
            CONVERT='big_endian', &
#endif
            STATUS='OLD')

      READ(ephi_num_unit,REC=1) &
          ephi_CHRHDR%TTL, &
          ephi_CHRHDR%CNAM, &
          ephi_EPHHDR%SS,  &
          ephi_EPHHDR%NCON,  &
          ephi_EPHHDR%AU, &
          ephi_EPHHDR%EMRAT, &
          ((ephi_EPHHDR%IPT(I,J),I=1,3),J=1,12), &
          ephi_EPHHDR%NUMDE, &
          (ephi_EPHHDR%IPT(I,13),I=1,3)

      READ(ephi_num_unit,REC=2) ephi_EPHHDR%CVAL

      NRL=0

      ENDIF


!       ********** MAIN ENTRY POINT **********


      IF(ET2(1) .EQ. 0.0_pm_reel) RETURN

      S=ET2(1)-.50_pm_reel
      CALL EPHI_SPLIT(S,PJD(1))
      CALL EPHI_SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.50_pm_reel
      PJD(2)=PJD(2)+PJD(4)
      CALL EPHI_SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)

!       ERROR RETURN FOR EPOCH OUT OF RANGE

      IF(PJD(1)+PJD(4).LT.ephi_EPHHDR%SS(1) .OR. PJD(1)+PJD(4).GT.ephi_EPHHDR%SS(2)) GO TO 98

!       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL

      NR=IDINT((PJD(1)-ephi_EPHHDR%SS(1))/ephi_EPHHDR%SS(3))+3

! Correction ATOS : Comparaison de réels (<=> PJD(1) == ephi_EPHHDR%SS(2) )
      IF(ABS(PJD(1)-ephi_EPHHDR%SS(2)) < EPSILON(1.0_pm_reel) ) NR=NR-1

        tmp1 = DBLE(NR-3)*ephi_EPHHDR%SS(3) + ephi_EPHHDR%SS(1)
        tmp2 = PJD(1) - tmp1
        T(1) = (tmp2 + PJD(4))/ephi_EPHHDR%SS(3)

!       READ CORRECT RECORD IF NOT IN CORE

      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(ephi_num_unit,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
      ENDIF

      IF(ephi_STCOMX%KM) THEN
      T(2)=ephi_EPHHDR%SS(3)*86400.0_pm_reel
      AUFAC=1.0_pm_reel
      ELSE
      T(2)=ephi_EPHHDR%SS(3)
      AUFAC=1.0_pm_reel/ ephi_EPHHDR%AU
      ENDIF

!   INTERPOLATE SSBARY SUN

      CALL ephi_INTERP(BUF(ephi_EPHHDR%IPT(1,11)),T,ephi_EPHHDR%IPT(2,11),3, &
          ephi_EPHHDR%IPT(3,11),2, &
          ephi_STCOMX%PVSUN)

      DO I=1,6
      ephi_STCOMX%PVSUN(I)=ephi_STCOMX%PVSUN(I)*AUFAC
      ENDDO

!   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED

      DO 4 I=1,10
      IF(LIST(I).EQ.0) GO TO 4

      CALL ephi_INTERP(BUF(ephi_EPHHDR%IPT(1,I)),T,ephi_EPHHDR%IPT(2,I),3,ephi_EPHHDR%IPT(3,I), &
      LIST(I),PV(1,I))

      DO J=1,6
       IF(I.LE.9 .AND. .NOT.ephi_STCOMX%BARY) THEN
       PV(J,I)=PV(J,I)*AUFAC-ephi_STCOMX%PVSUN(J)
       ELSE
       PV(J,I)=PV(J,I)*AUFAC
       ENDIF
      ENDDO

   4  CONTINUE

!       DO NUTATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(11).GT.0 .AND. ephi_EPHHDR%IPT(2,12).GT.0) &
      CALL ephi_INTERP(BUF(ephi_EPHHDR%IPT(1,12)),T, &
         ephi_EPHHDR%IPT(2,12),2,ephi_EPHHDR%IPT(3,12), &
         LIST(11),PNUT)

!       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(12).GT.0 .AND. ephi_EPHHDR%IPT(2,13).GT.0) &
      CALL ephi_INTERP(BUF(ephi_EPHHDR%IPT(1,13)),T,&
         ephi_EPHHDR%IPT(2,13),3,ephi_EPHHDR%IPT(3,13), &
         LIST(12),PV(1,11))

      RETURN

      ! Traitement des erreurs

      ! Erreur MAGE au lieu d'un affichage sur la sortie standard
   98 write (msp_mess(1), *) ( ET2(1)+ET2(2) )
      write (msp_mess(2), *) ephi_EPHHDR%SS(1)
      write (msp_mess(3), *) ephi_EPHHDR%SS(2)
      call MSP_signaler_message(cle_mes="EPH_ERR_JPL_DOMAINE_VALIDITE", &
                  routine="ephi_STATE", &
                  partie_variable=msp_mess)
      return

      ! Erreur MAGE au lieu d'un affichage sur la sortie standard
   99 call MSP_signaler_message(cle_mes="EPH_ERR_JPL_LECTURE", &
                  routine="ephi_STATE")
      return

      END subroutine ephi_state


      SUBROUTINE ephi_CONST(NAM,VAL,SSS,N)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephi_CONST
!
!$Resume
!  Cette méthode extrait les constantes du fichier d'éphémérides
!
!$Description
!  Cette méthode extrait les constantes du fichier d'éphémérides
!
!$Auteur
!  JPL
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ephi_CONST(NAM,VAL,SSS,N)
!.    CHARACTER(LEN=6) :: NAM(*)
!.    REAL(KIND=PM_REEL) :: VAL(*)
!.    REAL(KIND=PM_REEL) :: SSS(3)
!.    INTEGER :: N
!
!$Arguments
!>S     NAM  :<LEN=6,DIM=(*)>   Tableau du nom des constantes  
!>S     VAL  :<PM_REEL,DIM=(*)> Tableau de la valeur des constantes
!>S     SSS  :<PM_REEL,DIM=(3)> Date de début, de fin et pas des éphémérides
!>S     N    :<integer>         Taille des tableaux
!
!$Common
!
!$Routines
!- ephi_STATE
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

      SAVE

      !
      ! Variables d'entrée / sortie
      !
      CHARACTER(LEN=6), intent(out) :: NAM(*)
      REAL(KIND=PM_REEL), intent(out) :: VAL(*)
      REAL(KIND=PM_REEL), intent(out) :: SSS(3)
      INTEGER, intent(out) :: N

      !
      ! Variables locales
      !
      REAL(KIND=PM_REEL) :: zips(2)
      data zips/2*0.0_pm_reel/

      INTEGER :: LIST(12)
      REAL(KIND=PM_REEL) :: PV(6,12)
      REAL(KIND=PM_REEL) :: PNUT(4)
      INTEGER :: I

      DO I=1,12
         LIST(I)=0
      ENDDO

!  CALL STATE TO INITIALIZE THE EPHEMERIS AND READ IN THE CONSTANTS

      IF(ephi_FIRST_const) CALL ephi_STATE(zips,list,PV,PNUT)
      ! Propagation d'erreurs
      if ( MSP_gen_messages("ephi_CONST") ) return
      ephi_FIRST_const=.false.

      N=ephi_EPHHDR%NCON

      DO I=1,3
      SSS(I)=ephi_EPHHDR%SS(I)
      ENDDO

      DO I=1,N
      NAM(I)=ephi_CHRHDR%CNAM(I)
      VAL(I)=ephi_EPHHDR%CVAL(I)
      ENDDO

      RETURN

      END subroutine ephi_CONST

!++++++++++++++++++++++++
!
      SUBROUTINE ephi_FSIZER2(NRECL,KSIZE)
!
!++++++++++++++++++++++++
!  THIS SUBROUTINE OPENS THE FILE, 'NAMFIL', WITH A PHONY RECORD LENGTH, READS 
!  THE FIRST RECORD, AND USES THE INFO TO COMPUTE KSIZE, THE NUMBER OF SINGLE 
!  PRECISION WORDS IN A RECORD.  
!
!  THE SUBROUTINE ALSO SETS THE VALUES OF  NRECL, NRFILE, AND NAMFIL.

      IMPLICIT NONE

      INTEGER, intent(out) :: NRECL
      INTEGER, intent(out) :: KSIZE

      SAVE

      INTEGER :: KMX, KHI, I, J, MRECL, ND

!  *****************************************************************
!  *****************************************************************
!
!  THE PARAMETERS NRECL, NRFILE, AND NAMFIL ARE TO BE SET BY THE USER
!
!  *****************************************************************

!  NRECL=1 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN S.P. WORDS
!  NRECL=4 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN BYTES
!  (for UNIX, it is probably 4)
!
      NRECL=4

!  *****************************************************************
!  *****************************************************************

!  **  OPEN THE DIRECT-ACCESS FILE AND GET THE POINTERS IN ORDER TO 
!  **  DETERMINE THE SIZE OF THE EPHEMERIS RECORD

      MRECL=NRECL*1000

      OPEN(ephi_num_unit, &
            FILE=ephi_fichier, &
             ACCESS='DIRECT', &
             FORM='UNFORMATTED', &
             RECL=MRECL, &
#ifdef __GFORTRAN__
             CONVERT='big_endian', &
#endif
             STATUS='OLD')

      READ(ephi_num_unit,REC=1) &
          ephi_CHRHDR%TTL, &
          ephi_CHRHDR%CNAM, &
          ephi_EPHHDR%SS,  &
          ephi_EPHHDR%NCON,  &
          ephi_EPHHDR%AU, &
          ephi_EPHHDR%EMRAT, &
          ((ephi_EPHHDR%IPT(I,J),I=1,3),J=1,12), &
          ephi_EPHHDR%NUMDE, &
          (ephi_EPHHDR%IPT(I,13),I=1,3)

      CLOSE(ephi_num_unit)



!  FIND THE NUMBER OF EPHEMERIS COEFFICIENTS FROM THE POINTERS

      KMX = 0
      KHI = 0

      DO I = 1,13
         IF (ephi_EPHHDR%IPT(1,I) .GT. KMX) THEN
            KMX = ephi_EPHHDR%IPT(1,I)
            KHI = I
         ENDIF
      ENDDO

      ND = 3
      IF (KHI .EQ. 12) ND=2

      KSIZE = 2*(ephi_EPHHDR%IPT(1,KHI)+ND* &
           ephi_EPHHDR%IPT(2,KHI)*ephi_EPHHDR%IPT(3,KHI)-1)

      RETURN

      END subroutine ephi_FSIZER2


end module eph_jpl_ephemeris

