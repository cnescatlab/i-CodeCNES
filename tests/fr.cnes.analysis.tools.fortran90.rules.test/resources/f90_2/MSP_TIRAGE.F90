MODULE MSP_TIRAGE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_TIRAGE
!
!$Resume
!  Module contenant des outils de génération de bulletins mesurés
!
!$Description
!  Module contenant des outils de génération de bulletins mesurés
!
!$Auteur
!  J.-J. WASBAUER
!
!$Version
!  $Id: MSP_TIRAGE.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_TIRAGE.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.19  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.18  2010/02/18 15:27:01  vivaresf
!  V4.10::DM-ID:1350:reproductibilité des résultats sous Linux
!
!  Revision 1.17  2010/02/16 16:02:11  vivaresf
!  VERSION::DM-ID:1350: portage Linux, pour la reproductivité des résultats il faut initialiser val dans MSP_init_tirage
!
!  Revision 1.16  2008/11/21 10:43:50  cml
!  AQ : Remise en place d une init supprimee par erreur
!
!  Revision 1.15  2008/11/20 12:46:02  cml
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.14  2008/08/08 15:04:58  gss
!  DM-ID 1058 : (portage g95) suppression de variables non utilisées.
!
!  Revision 1.13  2008/07/04 14:56:32  huec
!  DM-ID 1058 : Passage de la variable val a la dimension 4 pour compatibilite G95
!
!  Revision 1.12  2008/03/26 10:16:18  huec
!  AQ : ajout de PM_REEL pour des reels
!
!  Revision 1.11  2007/02/02 10:34:44  vivaresf
!  Version 4.4a1 : variables inutilisées
!
!  Revision 1.10  2006/11/06 14:13:59  vpg
!  DM-ID 425 : passage PSIMU sous Linux : initialisations complémentaires
!
!  Revision 1.9  2006/06/02 11:21:55  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!
!  Revision 1.8  2004/05/28 14:25:20  vivaresf
!  DM_105
!
!  Revision 1.7.2.1  2004/05/28 14:20:55  vivaresf
!  DM 105 : rajout d'une fonction externe en parametre optionel de
!  MSP_nombre_aleatoire
!  ce parametre optionnel est disponible aussi pour toutes les fonctions appelant MSP_nombre_aleatoire
!  transfert de flag_opt partout ou il n'etait pas transfere
!
!  Revision 1.7  2003/03/20 17:45:24  adm_ipsi
!  Ajout du paramètre ini_seed sur plusieurs fonctions
!  Revision 1.6  2003/03/18 14:20:20  adm_ipsi
!  MSP_random_seed, ajout du paramètre ini_seed pour pouvoir re-générer des fichiers comparables
!  Revision 1.5  2003/01/08 15:12:39  adm_ipsi
!   Explicitation des conversions de type implicites
!  Revision 1.4  2002/12/06 18:01:17  adm_ipsi
!  Utilisation des  fonctions .different.  et .egal. pour les comparaisons entre reel
!  Revision 1.3  2002/12/03 18:24:08  adm_ipsi
!  Remplacement de 2 goto par des boucles do / end do
!  Revision 1.2  2002/12/03 17:21:05  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.3  2000/06/15 08:46:00  util_am
!  - Correction d'une erreur de codage dans MSP_tirage_direction (angles en rad et non en deg)
!  - Ajout des interfaces anglaises
!  - Mise à jour des cartouches (section Voir-Aussi et Mots-cles)
!  Revision 1.2  1999/09/22 15:25:00  util_am
!  Mise a jour des cartouches
!  Revision 1.1  1999/09/03 16:42:41  util_am
!  MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!  MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!  MECASPA.F90 : Ajout des use aux nouveaux modules
!
!$FinHistorique
!
!$Usage
!  use MSP_TIRAGE
!
!$Structure
!
!$Global
!
!>  MSP_ENUM_ACCEPTATION_REJET      : <integer,parameter>  Méthode de tirage par acceptation-rejet
!>  MSP_ENUM_INVERSION_ERF          : <integer,parameter>  Méthode de tirage par inversion de la fonction erf
!>  MSP_ENUM_TIRAGE_ALEATOIRE       : <integer,parameter>  Méthode de tirage par simple tirage aléatoire
!>  MSP_ENUM_RANDOM                 : <integer,parameter>  Générateur aléatoire intrinsèque au FORTRAN90
!>  MSP_ENUM_GENERATEUR_ALEATOIRE   : <integer,parameter>  Générateur aléatoire de MECASPA
!$Common
!
!$Routines
!- MSP_set_random_seed
!- MSP_orbit_markov_process
!- MSP_markov_process_error
!- MSP_orbit_gauss_white_noise
!- MSP_3Dspherical_gauss_sample
!- MSP_TNW_gauss_sample
!- MSP_1D_gauss_sample_erf
!- MSP_1D_gauss_sample_ar
!- MSP_direction_sample
!- MSP_random_number
!- MSP_random_seed
!- MSP_markov
!- MSP_erreur_markov
!- MSP_bruit_blanc_gauss
!- MSP_tirage_gaussien_3D
!- MSP_tirage_gaussien_3D_TNW
!- MSP_tirage_gaussien_1D_erf
!- MSP_tirage_ar
!- MSP_tirage_direction
!
!$Fonctions
!- MSP_nombre_aleatoire
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_ALEATOIRE
!- MSP_ERF
!- MSP_MATH
!#
!
!$Interface
!> msp_random_number :             MSP_nombre_aleatoire
!> msp_direction_sample :          MSP_tirage_direction
!> msp_1d_gauss_sample_ar :        MSP_tirage_gaussien_1D_erf
!> msp_1d_gauss_sample_erf :       MSP_tirage_gaussien_1D_erf
!> msp_set_random_seed :           MSP_random_seed
!> msp_tnw_gauss_sample :          MSP_tirage_gaussien_3D_TNW
!> msp_3dspherical_gauss_sample :  MSP_tirage_gaussien_3D
!> msp_orbit_markov_process :      MSP_markov
!> msp_markov_process_error :      MSP_erreur_markov
!> msp_orbit_gauss_white_noise :   MSP_bruit_blanc_gauss
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  MSP_nombre_aleatoire MSP_set_random_seed MSP_orbit_markov_process MSP_markov_process_error
!.  MSP_orbit_gauss_white_noise MSP_3Dspherical_gauss_sample MSP_TNW_gauss_sample MSP_1D_gauss_sample_erf
!.  MSP_1D_gauss_sample_ar MSP_direction_sample MSP_random_number MSP_random_seed MSP_markov
!.  MSP_erreur_markov MSP_bruit_blanc_gauss MSP_tirage_gaussien_3D MSP_tirage_gaussien_3D_TNW
!.  MSP_tirage_gaussien_1D_erf MSP_tirage_ar MSP_tirage_direction
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  USE MSLIB, only : PM_REEL, PM_DEUX_PI, PM_PI_SUR2
  USE MSP_ALEATOIRE
  USE MSP_ERF
  USE MSP_MATH

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_TIRAGE.F90 365 2013-02-18 12:36:19Z aadt $'


  ! Constantes
  integer, parameter :: MSP_ENUM_ACCEPTATION_REJET = 1
  integer, parameter :: MSP_ENUM_INVERSION_ERF     = 2
  integer, parameter :: MSP_ENUM_TIRAGE_ALEATOIRE  = 3

  integer, parameter :: MSP_ENUM_RANDOM               = 1
  integer, parameter :: MSP_ENUM_GENERATEUR_ALEATOIRE = 2


  INTERFACE MSP_set_random_seed

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_random_seed
!
!$Resume
!  Gives an initial value to the random seed
!
!$Description
!  Gives an initial value to the random seed
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_random_seed([ini_seed])
!.    integer :: ini_seed
!
!$Procedures
!- MSP_random_seed
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_random_seed
  end INTERFACE

  INTERFACE MSP_orbit_markov_process

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_orbit_markov_process
!
!$Resume
!  Generates a Markov noise on the position-velocity vector
!
!$Description
!  Generates a Markov noise on the position-velocity vector
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_orbit_markov_process(sigmapos, sigmavit, tau, step, erreur_pos, erreur_vit, pvit,&
!.           [flag_opt],[sub_random])
!.    real(KIND=PM_REEL), dimension(:) :: pvit
!.    real(KIND=PM_REEL) :: sigmapos, sigmavit, tau, step
!.    real(KIND=PM_REEL), dimension(:) :: erreur_pos, erreur_vit
!.    integer :: flag_opt
!.    external sub_random
!
!$Procedures
!- MSP_markov
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_markov
  end INTERFACE

  INTERFACE MSP_markov_process_error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_markov_process_error
!
!$Resume
!  Generates an error based upon a 1st order Markov process
!
!$Description
!  Generates an error based upon a 1st order Markov process
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_markov_process_error(rho, sigma, erreur, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: erreur
!.    real(KIND=PM_REEL) :: rho, sigma
!.    external sub_random
!
!$Procedures
!- MSP_erreur_markov
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_erreur_markov
  end INTERFACE

  INTERFACE MSP_orbit_gauss_white_noise

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_orbit_gauss_white_noise
!
!$Resume
!  Generates a Gaussian white noise on a position-velocity vectore
!
!$Description
!  Generates a Gaussian white noise on a position-velocity vectore
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_orbit_gauss_white_noise(sigmapos, sigmavit, pvit, erreur_vit, erreur_pos,&
!.           [flag_opt],[sub_random])
!.    real(KIND=PM_REEL), dimension(:) :: pvit
!.    real(KIND=PM_REEL) :: sigmapos, sigmavit
!.    real(KIND=PM_REEL), dimension(:) :: erreur_pos, erreur_vit
!.    integer :: flag_opt
!.    external sub_random
!
!$Procedures
!- MSP_bruit_blanc_gauss
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_bruit_blanc_gauss
  end INTERFACE

  INTERFACE MSP_3Dspherical_gauss_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_3Dspherical_gauss_sample
!
!$Resume
!  Generates a 3D Gaussian white noise 
!
!$Description
!  Generates a 3D Gaussian white noise 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_3Dspherical_gauss_sample(sigma, delta, [flag_opt], [flag_tech],[ini_seed], &
!.           [sub_random])
!.    integer :: flag_opt, flag_tech
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL), dimension(:) :: delta
!.    integer :: ini_seed
!.    external sub_random
!
!$Procedures
!- MSP_tirage_gaussien_3D
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_tirage_gaussien_3D
  end INTERFACE

  INTERFACE MSP_TNW_gauss_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_TNW_gauss_sample
!
!$Resume
!  Generates a Gaussian white noise with three sigmas in the T, N and W directions
!
!$Description
!  Generates a Gaussian white noise with three sigmas in the T, N and W directions
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_TNW_gauss_sample(sigmaT, sigmaN, sigmaW, delta, [flag_opt], &
!.           [flag_tech],[ini_seed], [sub_random] )
!.    integer :: flag_opt, flag_tech
!.    real(KIND=PM_REEL) :: sigmaT, sigmaN, sigmaW
!.    real(KIND=PM_REEL), dimension(:) :: delta
!.    integer :: ini_seed
!.    external sub_random
!
!$Procedures
!- MSP_tirage_gaussien_3D_TNW
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_tirage_gaussien_3D_TNW
  end INTERFACE

  INTERFACE MSP_1D_gauss_sample_erf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_1D_gauss_sample_erf
!
!$Resume
!  Generates a 1D Gaussian white noise sampling with the erf inversion method
!
!$Description
!  Generates a 1D Gaussian white noise sampling with the erf inversion method
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_1D_gauss_sample_erf(sigma, delta, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL) :: delta
!.    external sub_random
!
!$Procedures
!- MSP_tirage_gaussien_1D_erf
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_tirage_gaussien_1D_erf
  end INTERFACE

  INTERFACE MSP_1D_gauss_sample_ar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_1D_gauss_sample_ar
!
!$Resume
!  Generates a 1D Gaussian white noise sampling with the acceptation-rejection method
!
!$Description
!  Generates a 1D Gaussian white noise sampling with the acceptation-rejection method
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_1D_gauss_sample_ar(sigma, delta, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL) :: delta
!.    external sub_random
!
!$Procedures
!- MSP_tirage_gaussien_1D_erf
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_tirage_gaussien_1D_erf
  end INTERFACE
   
  INTERFACE MSP_direction_sample

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_direction_sample
!
!$Resume
!  Generates a direction sampling around a mean direction
!
!$Description
!  Generates a direction sampling around a mean direction
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_direction_sample(wmoy, wpmoy, sigma_dir, w, wp, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: w, wp
!.    real(KIND=PM_REEL) :: wmoy, wpmoy, sigma_dir
!.    external sub_random
!
!$Procedures
!- MSP_tirage_direction
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_tirage_direction
  end INTERFACE

  INTERFACE MSP_random_number

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_random_number
!
!$Resume
!  Random number generator
!
!$Description
!  Random number generator
!
!$Acces
!  PUBLIC
!
!$Usage
!  u = MSP_random_number([flag_opt], [ini_seed], [sub_random])
!.    integer :: flag_opt
!.    integer :: ini_seed
!.    external sub_random
!
!$Procedures
!- MSP_nombre_aleatoire
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    module procedure MSP_nombre_aleatoire
  end INTERFACE

CONTAINS



  SUBROUTINE MSP_random_seed(ini_seed)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_random_seed
!
!$Resume
!  Cette routine positionne la graine aléatoire en fonction de la date du calculateur
!     ou bien à l'aide du paramètre optionnel ini_seed
!
!$Description
!  Cette routine positionne la graine aléatoire en fonction de la date du calculateur
!     ou bien à l'aide du paramètre optionnel ini_seed
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_random_seed([ini_seed])
!.    integer :: ini_seed
!
!$Arguments
!>[E]   ini_seed  :<integer>   graine à utiliser 
!
!$Common
!
!$Routines
!- DATE_AND_TIME
!- RANDOM_SEED
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

    ! Parametres
    integer, optional, intent(IN) :: ini_seed

    ! Variables locales
    integer, dimension(8) :: date
    integer, dimension(:), allocatable :: val
    integer  :: iSize

    ! Constantes
    integer, parameter :: n1 = 1
    
    ! Corps de la routine

    ! Taille minimale du tableau utilisé avec l'argument PUT
    call RANDOM_SEED (SIZE = iSize)
    allocate (val(iSize))

    ! Initialisations (pour compilateurs autres que FORTE)
    val(:) = 0 

    ! graine
    if ( present (ini_seed) ) then
       val(n1) = ini_seed
    else
       call DATE_AND_TIME(values=date(:))
       val(n1) = date(8)*max(1, date(7))*max(1, date(6))
    endif

    ! Initialisation de random_number
    call RANDOM_SEED(PUT=val)

  end SUBROUTINE MSP_random_seed





  SUBROUTINE MSP_markov(sigmapos, sigmavit, tau, step, erreur_pos, erreur_vit, pvit,&
       flag_opt,sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_markov
!
!$Resume
!  Cette routine génère un bulletin mesuré par un processus de Markov du premier ordre
!
!$Description
!  Cette routine génère un bulletin mesuré par un processus de Markov du premier ordre
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_markov(sigmapos, sigmavit, tau, step, erreur_pos, erreur_vit, pvit,&
!.           [flag_opt],[sub_random])
!.    real(KIND=PM_REEL), dimension(:) :: pvit
!.    real(KIND=PM_REEL) :: sigmapos, sigmavit, tau, step
!.    real(KIND=PM_REEL), dimension(:) :: erreur_pos, erreur_vit
!.    integer :: flag_opt
!.    external sub_random
!
!$Arguments
!>E     sigmapos    :<PM_REEL>           Ecart type en position
!>E     sigmavit    :<PM_REEL>           Ecart type en vitesse
!>E     tau         :<PM_REEL>           Constante de corrélation
!>E     step        :<PM_REEL>           Pas d'incrémentation par rapport à l'état précédent
!>E/S   erreur_pos  :<PM_REEL,DIM=(:)>   Erreur en position générée par le Markov 
!                                        (entrée=état précédent, sortie=nouvel état)
!>E/S   erreur_vit  :<PM_REEL,DIM=(:)>   Erreur en vitesse générée par le Markov
!                                        (entrée=état précédent, sortie=nouvel état)
!>E/S   pvit        :<PM_REEL,DIM=(:)>   Bulletin cartésien position-vitesse
!                                        d'entrée modifié en sortie 
!>[E]   flag_opt    :<integer>           Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>          Routine de generation de nombre aleatoire
!
!$Common
!
!$Routines
!- MSP_erreur_markov
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

    ! Parametres
    real(KIND=PM_REEL), intent(inout), dimension(:) :: pvit
    real(KIND=PM_REEL), intent(in) :: sigmapos, sigmavit, tau, step
    real(KIND=PM_REEL), intent(inout), dimension(:) :: erreur_pos, erreur_vit
    integer, intent(in), optional :: flag_opt
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: rho

    ! Corps de la routine
    rho = exp(-step/tau)

    ! =========================================
    ! =      Perturbation de la position      =
    ! =========================================

    call MSP_erreur_markov(rho, sigmapos, erreur_pos(1), flag_opt=flag_opt, &
         sub_random=sub_random)
    call MSP_erreur_markov(rho, sigmapos, erreur_pos(2), flag_opt=flag_opt, &
        sub_random=sub_random)
    call MSP_erreur_markov(rho, sigmapos, erreur_pos(3), flag_opt=flag_opt, &
        sub_random=sub_random)
    erreur_pos(3) = 0._PM_REEL
    pvit(1:3) = pvit(1:3) + erreur_pos(:)
    if (MSP_gen_messages ("MSP_markov")) return


    ! =========================================
    ! =      Perturbation de la vitesse       =
    ! =========================================

    call MSP_erreur_markov(rho, sigmavit, erreur_vit(1), flag_opt=flag_opt, &
        sub_random=sub_random)
    call MSP_erreur_markov(rho, sigmavit, erreur_vit(2), flag_opt=flag_opt, &
        sub_random=sub_random)
    call MSP_erreur_markov(rho, sigmavit, erreur_vit(3), flag_opt=flag_opt, &
        sub_random=sub_random)
    if (MSP_gen_messages ("MSP_markov")) return
    erreur_vit(3) = 0._PM_REEL
    pvit(4:6) = pvit(4:6) + erreur_vit(:)


  end SUBROUTINE MSP_markov
  



  SUBROUTINE MSP_erreur_markov(rho, sigma, erreur, flag_opt, sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_erreur_markov
!
!$Resume
!  Génération d'une erreur de Markov à partir de son état précédent
!
!$Description
!  Génération d'une erreur de Markov à partir de son état précédent
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_erreur_markov(rho, sigma, erreur, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: erreur
!.    real(KIND=PM_REEL) :: rho, sigma
!.    external sub_random
!
!$Arguments
!>E     rho         :<PM_REEL>    Pondération de Markov
!>E     sigma       :<PM_REEL>    Ecart type du processus Gaussien sous-jacent
!>E/S   erreur      :<PM_REEL>    Erreur de Markov
!>[E]   flag_opt    :<integer>    Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>   Routine de generation de nombre aleatoire
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

    ! Parametres
    integer, intent(in), optional :: flag_opt
    real(KIND=PM_REEL), intent(inout) :: erreur
    real(KIND=PM_REEL), intent(in) :: rho, sigma
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: u, rho2, err_nc, err_c

    ! Corps de la routine
    rho2 = sqrt(1._PM_REEL - rho*rho)
    u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
    err_nc = rho2*sigma*sqrt(2._PM_REEL)*MSP_inverf(erf_bornemax - 2._PM_REEL*u*erf_bornemax)
    err_c = rho*erreur

    erreur = err_nc + err_c
    if (MSP_gen_messages ("MSP_erreur_markov")) return

  end SUBROUTINE MSP_erreur_markov
  





  SUBROUTINE MSP_bruit_blanc_gauss(sigmapos, sigmavit, pvit, erreur_vit, erreur_pos,&
       flag_opt,sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_bruit_blanc_gauss
!
!$Resume
!  Génère un bulletin mesuré par un processus Gaussien 
!
!$Description
!  Génère un bulletin mesuré par un processus Gaussien 
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_bruit_blanc_gauss(sigmapos, sigmavit, pvit, erreur_vit, erreur_pos,&
!.           [flag_opt],[sub_random])
!.    real(KIND=PM_REEL), dimension(:) :: pvit
!.    real(KIND=PM_REEL) :: sigmapos, sigmavit
!.    real(KIND=PM_REEL), dimension(:) :: erreur_pos, erreur_vit
!.    integer :: flag_opt
!.    external sub_random
!
!$Arguments
!>E     sigmapos    :<PM_REEL>           Ecart type de position
!>E     sigmavit    :<PM_REEL>           Ecart type de vitesse
!>E/S   pvit        :<PM_REEL,DIM=(:)>   Bulletin mesuré (en entrée = bulletin parfait)
!>S     erreur_vit  :<PM_REEL,DIM=(:)>   Erreur gaussienne en vitesse
!>S     erreur_pos  :<PM_REEL,DIM=(:)>   Erreur gaussienne en position
!>[E]   flag_opt    :<integer>           Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>          Routine de generation de nombre aleatoire
!
!$Common
!
!$Routines
!- MSP_tirage_gaussien_3D
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

    ! Parametres
    real(KIND=PM_REEL), intent(inout), dimension(:) :: pvit
    real(KIND=PM_REEL), intent(in) :: sigmapos, sigmavit
    real(KIND=PM_REEL), intent(out), dimension(:) :: erreur_pos, erreur_vit
    integer, intent(in), optional :: flag_opt
    external sub_random
    optional :: sub_random

    ! =========================================
    ! =      Perturbation de la position      =
    ! =========================================

    call MSP_tirage_gaussien_3D(sigmapos, erreur_pos, flag_opt=flag_opt,&
         sub_random=sub_random)
    pvit(1:3) = pvit(1:3) + erreur_pos(:)
    if (MSP_gen_messages ("MSP_bruit_blanc_gauss")) return

    ! =========================================
    ! =      Perturbation de la vitesse       =
    ! =========================================

    call MSP_tirage_gaussien_3D(sigmavit, erreur_vit, flag_opt=flag_opt,&
         sub_random=sub_random)
    pvit(4:6) = pvit(4:6) + erreur_vit(:)
    if (MSP_gen_messages ("MSP_bruit_blanc_gauss")) return

  end SUBROUTINE MSP_bruit_blanc_gauss
  





  SUBROUTINE MSP_tirage_gaussien_3D(sigma, delta, flag_opt, flag_tech,ini_seed, &
       sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_tirage_gaussien_3D
!
!$Resume
!  Cette routine réalise un tirage gaussien 3D d'un vecteur
!
!$Description
!  Cette routine réalise un tirage gaussien 3D d'un vecteur
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_tirage_gaussien_3D(sigma, delta, [flag_opt], [flag_tech],[ini_seed], &
!.           [sub_random])
!.    integer :: flag_opt, flag_tech
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL), dimension(:) :: delta
!.    integer :: ini_seed
!.    external sub_random
!
!$Arguments
!>E     sigma       :<PM_REEL>           Ecart type du processus Gaussien 3D
!>S     delta       :<PM_REEL,DIM=(:)>   Erreur gaussienne centrée en 0
!>[E]   flag_opt    :<integer>           Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   flag_tech   :<integer>           Technique de tirage du nombre aléatoire
!                                       (Acceptation-rejet, Inversion de la fonction erf, tirage Gaussien 1D aléatoire)
!>[E]   ini_seed    :<integer>           graine à utiliser - permet de reproduire les résultats
!>[E]   sub_random  :<external>          Routine de generation de nombre aleatoire
!
!$Common
!
!$Routines
!- MSP_tirage_ar
!- MSP_tirage_gaussien_1D_erf
!- MSP_var_08
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

    ! Parametres
    integer, intent(in), optional :: flag_opt, flag_tech
    real(KIND=PM_REEL), intent(in) :: sigma
    real(KIND=PM_REEL), intent(out), dimension(:) :: delta
    integer, optional, intent(IN) :: ini_seed
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: dx1, dx2, theta, u

    ! Corps de la routine
    ! Choix de la composante parallèle 
    u = MSP_nombre_aleatoire(flag_opt=flag_opt, ini_seed = ini_seed,&
         sub_random=sub_random)
    
    if (PRESENT(flag_tech)) then 

       SELECT CASE(flag_tech)

       CASE (MSP_ENUM_ACCEPTATION_REJET)  ! Par acceptation rejet
          call MSP_tirage_ar(sigma, dx1, flag_opt=flag_opt,sub_random=sub_random)
          
       CASE(MSP_ENUM_INVERSION_ERF)   ! Par inversion de la fonction erf
          call MSP_tirage_gaussien_1D_erf(sigma, dx1, flag_opt=flag_opt,sub_random=sub_random)
          
       CASE(MSP_ENUM_TIRAGE_ALEATOIRE)   ! Par une routine ne s'appuyant pas sur random
          call MSP_var_08(0._PM_REEL,sigma,dx1,ini_seed=ini_seed)
          
       END SELECT

    else
       call MSP_tirage_gaussien_1D_erf(sigma, dx1, flag_opt=flag_opt,&
            sub_random=sub_random)
    end if
    if (MSP_gen_messages ("MSP_tirage_gaussien_3D")) return

    ! Choix de la composante perpendiculaire 
    u = MSP_nombre_aleatoire(flag_opt=flag_opt, ini_seed = ini_seed, &
         sub_random=sub_random)
    dx2 = sigma*sqrt(2._PM_REEL)*sqrt(-log(exp_bornemax + u*(1._PM_REEL - exp_bornemax)))
    if (MSP_gen_messages ("MSP_tirage_gaussien_3D")) return

    ! Choix de l'azimut de la composante perpendiculaire
    u = MSP_nombre_aleatoire(flag_opt=flag_opt, ini_seed = ini_seed,sub_random=sub_random)
    theta = pm_deux_pi*u
    if (MSP_gen_messages ("MSP_tirage_gaussien_3D")) return

    ! Coordonnées de l'écart à la moyenne
    delta(1) = dx1
    delta(2) = dx2*cos(theta)
    delta(3) = dx2*sin(theta)

  end SUBROUTINE MSP_tirage_gaussien_3D




  SUBROUTINE MSP_tirage_gaussien_3D_TNW(sigmaT, sigmaN, sigmaW, delta, flag_opt, &
       flag_tech,ini_seed, sub_random )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_tirage_gaussien_3D_TNW
!
!$Resume
!  Cette routine réalise un tirage Gaussien 3D avec des écarts 
!  types différents suivant les trois directions du repère TNW
!
!$Description
!  Cette routine réalise un tirage Gaussien 3D avec des écarts 
!  types différents suivant les trois directions du repère TNW
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_tirage_gaussien_3D_TNW(sigmaT, sigmaN, sigmaW, delta, [flag_opt], &
!.           [flag_tech],[ini_seed], [sub_random] )
!.    integer :: flag_opt, flag_tech
!.    real(KIND=PM_REEL) :: sigmaT, sigmaN, sigmaW
!.    real(KIND=PM_REEL), dimension(:) :: delta
!.    integer :: ini_seed
!.    external sub_random
!
!$Arguments
!>E     sigmaT      :<PM_REEL>           Sigma le long de T
!>E     sigmaN      :<PM_REEL>           Sigma le long de N
!>E     sigmaW      :<PM_REEL>           Sigma le long de W
!>S     delta       :<PM_REEL,DIM=(:)>   Erreur Gaussienne 3D
!>[E]   flag_opt    :<integer>           Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   flag_tech   :<integer>           Technique de tirage du nombre aléatoire
!                                       (Acceptation-rejet, Inversion de la fonction erf, tirage Gaussien 1D aléatoire)
!>[E]   ini_seed    :<integer>           graine à utiliser - permet de reproduire les résultats
!>[E]   sub_random  :<external>          Routine de generation de nombre aleatoire
!
!$Common
!
!$Routines
!- MSP_tirage_ar
!- MSP_tirage_gaussien_1D_erf
!- MSP_var_08
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

    ! Parametres
    integer, intent(in), optional :: flag_opt, flag_tech
    real(KIND=PM_REEL), intent(in) :: sigmaT, sigmaN, sigmaW
    real(KIND=PM_REEL), intent(out), dimension(:) :: delta
    integer, optional, intent(IN) :: ini_seed
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: dxT, dxN, dxW, u

    ! Corps de la routine
    ! Choix de la composante parallèle 
    ! ATTENTION : Cet appel cache une initialisation, NE PAS SUPPRIMER
    u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
    if (MSP_gen_messages ("MSP_tirage_gaussien_3D_TNW")) return

    ! Corps de la routine   
    if (PRESENT(flag_tech)) then 

       SELECT CASE(flag_tech)

       CASE (MSP_ENUM_ACCEPTATION_REJET)  ! Par acceptation rejet
          call MSP_tirage_ar(sigmaT, dxT, flag_opt=flag_opt,sub_random=sub_random)
          call MSP_tirage_ar(sigmaN, dxN, flag_opt=flag_opt,sub_random=sub_random)
          call MSP_tirage_ar(sigmaW, dxW, flag_opt=flag_opt,sub_random=sub_random)
          
       CASE(MSP_ENUM_INVERSION_ERF)   ! Par inversion de la fonction erf
          call MSP_tirage_gaussien_1D_erf(sigmaT, dxT, flag_opt=flag_opt,&
               sub_random=sub_random)
          call MSP_tirage_gaussien_1D_erf(sigmaN, dxN, flag_opt=flag_opt,&
               sub_random=sub_random)
          call MSP_tirage_gaussien_1D_erf(sigmaW, dxW, flag_opt=flag_opt,&
               sub_random=sub_random)
          
       CASE(MSP_ENUM_TIRAGE_ALEATOIRE)   ! Par une routine ne s'appuyant pas sur random
          call MSP_var_08(0._PM_REEL, sigmaT, dxT, ini_seed=ini_seed)
          call MSP_var_08(0._PM_REEL, sigmaN, dxN, ini_seed=ini_seed)
          call MSP_var_08(0._PM_REEL, sigmaW, dxW, ini_seed=ini_seed)
          
       END SELECT

    else
          call MSP_tirage_gaussien_1D_erf(sigmaT, dxT, flag_opt=flag_opt,&
               sub_random=sub_random)
          call MSP_tirage_gaussien_1D_erf(sigmaN, dxN, flag_opt=flag_opt,&
               sub_random=sub_random)
          call MSP_tirage_gaussien_1D_erf(sigmaW, dxW, flag_opt=flag_opt,&
               sub_random=sub_random)
    end if
    if (MSP_gen_messages ("MSP_tirage_gaussien_3D_TNW")) return

    ! Coordonnées de l'écart à la moyenne
    delta(1) = dxT
    delta(2) = dxN
    delta(3) = dxW

  end SUBROUTINE MSP_tirage_gaussien_3D_TNW




  SUBROUTINE MSP_tirage_gaussien_1D_erf(sigma, delta, flag_opt, sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_tirage_gaussien_1D_erf
!
!$Resume
!  Cette routine réalise un tirage Gaussien en 1D en inversant la fonction erf
!
!$Description
!  Cette routine réalise un tirage Gaussien en 1D en inversant la fonction erf
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_tirage_gaussien_1D_erf(sigma, delta, [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL) :: delta
!.    external sub_random
!
!$Arguments
!>E     sigma       :<PM_REEL>    Valeur de l'écart type
!>S     delta       :<PM_REEL>    Erreur 1D gaussienne
!>[E]   flag_opt    :<integer>    Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>   Routine de generation de nombre aleatoire
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

    ! Parametres
    integer, intent(in), optional :: flag_opt
    real(KIND=PM_REEL), intent(in) :: sigma
    real(KIND=PM_REEL), intent(out) :: delta
    external sub_random
    optional :: sub_random

    ! variables locales
    real(KIND=PM_REEL) :: u

    ! Corps de la routine
    ! Choix de la composante parallèle 
    u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
    delta = sigma*sqrt(2._PM_REEL)*MSP_inverf(erf_bornemax - 2._PM_REEL*u*erf_bornemax)
    if (MSP_gen_messages ("MSP_tirage_gaussien_1D_erf")) return

  end SUBROUTINE MSP_tirage_gaussien_1D_erf



  SUBROUTINE MSP_tirage_ar(sigma, delta, fmax, flag_opt, sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_tirage_ar
!
!$Resume
!  Cette routine réalise un tirage aléatoire par la méthode d'acceptation-rejet
!
!$Description
!  Cette routine réalise un tirage aléatoire par la méthode d'acceptation-rejet
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_tirage_ar(sigma, delta, [fmax], [flag_opt], [sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: sigma
!.    real(KIND=PM_REEL) :: delta
!.    real(KIND=PM_REEL) :: fmax
!.    external sub_random
!
!$Arguments
!>E     sigma       :<PM_REEL>    Ecart-type
!>S     delta       :<PM_REEL>    Erreur 1D obtenue par la méthode d'acceptation-rejet
!>[E]   fmax        :<PM_REEL>    Valeur Maximum prise par func sur l'intervalle de tirage
!>[E]   flag_opt    :<integer>    Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>   Routine de generation de nombre aleatoire
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

    ! Parametres
    integer, intent(in), optional :: flag_opt
    real(KIND=PM_REEL), intent(in) :: sigma
    real(KIND=PM_REEL), intent(out) :: delta
    real(KIND=PM_REEL), intent(in), optional :: fmax
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: u, y, x

    ! Corps de la routine
    ! Choix de l'ordonnée 
    do  
       y = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
       if (PRESENT(fmax)) y = fmax*y
    
       ! Choix de l'abscisse
       u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
       x = 6._PM_REEL*u - 3._PM_REEL

       if ( y <= exp(-x*x) ) exit
    end do
         
    delta = sigma*sqrt(2._PM_REEL)*x
    if (MSP_gen_messages ("MSP_tirage_ar")) return
    
  end SUBROUTINE MSP_tirage_ar



  SUBROUTINE MSP_tirage_direction(wmoy, wpmoy, sigma_dir, w, wp, flag_opt, sub_random)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_tirage_direction
!
!$Resume
!  Cette routine réalise le tirage d'une direction dans l'espace
!
!$Description
!  Cette routine réalise le tirage d'une direction dans l'espace
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_tirage_direction(wmoy, wpmoy, sigma_dir, w, wp, [flag_opt],[ sub_random])
!.    integer :: flag_opt
!.    real(KIND=PM_REEL) :: w, wp
!.    real(KIND=PM_REEL) :: wmoy, wpmoy, sigma_dir
!.    external sub_random
!
!$Arguments
!>E     wmoy        :<PM_REEL>    Valeur moyenne de l'angle w (en rad)
!>E     wpmoy       :<PM_REEL>    Valeur moyenne de l'angle wp (en rad)
!>E     sigma_dir   :<PM_REEL>    Ecart type angulaire représentant l'erreur angulaire commise
!>S     w           :<PM_REEL>    Premier angle tiré (w dans le plan, wp hors-plan) (en rad)
!>S     wp          :<PM_REEL>    Deuxième angle tiré (en rad)
!>[E]   flag_opt    :<integer>    Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   sub_random  :<external>   Routine de generation de nombre aleatoire
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

    ! Parametres
    integer, intent(in), optional :: flag_opt
    real(KIND=PM_REEL), intent(out) :: w, wp
    real(KIND=PM_REEL), intent(in) :: wmoy, wpmoy, sigma_dir
    external sub_random
    optional :: sub_random

    ! Variables locales
    real(KIND=PM_REEL) :: alpha_max, sigma, alpha, theta, y, u
    real(KIND=PM_REEL) :: cw, cwp, sw, swp, fx
    real(KIND=PM_REEL), dimension(3) :: uu, vv, ww, dir

    ! Corps de la routine
    if (sigma_dir .egal. 0._PM_REEL) then 
       w = wmoy
       wp = wpmoy
       return
    end if

    alpha_max = 5._PM_REEL*sigma_dir
    sigma = sigma_dir
    if (alpha_max > pm_pi_sur2) then 
       call MSP_signaler_message (cle_mes="MSP_tirage_direction")
       return
    end if
    
    ! Choix de l'angle du cône
    y = 1._PM_REEL
    fx = 0._PM_REEL
    do while ( y > fx )
       u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
       alpha = acos(1._PM_REEL - u*(1._PM_REEL - cos(alpha_max)))

       ! Acceptation - Rejet sur la gaussienne
       y = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)

       fx = exp(-alpha*alpha/(2._PM_REEL*sigma*sigma))
    end do
    if (MSP_gen_messages ("MSP_tirage_direction")) return
    
    ! Choix de l'angle azimut
    u = MSP_nombre_aleatoire(flag_opt=flag_opt,sub_random=sub_random)
    theta = pm_deux_pi*u
    
    cw = cos(wmoy)
    cwp = cos(wpmoy)
    sw = sin(wmoy)
    swp = sin(wpmoy)

    uu(1) = cwp*cw
    uu(2) = swp
    uu(3) = cwp*sw

    vv(1) = swp*cw
    vv(2) = -cwp
    vv(3) = swp*sw
    
    ww(1) = -sw
    ww(2) = 0._PM_REEL
    ww(3) = cw

    dir(:) = cos(alpha)*uu(:) + sin(alpha)*(cos(theta)*vv(:) + sin(theta)*ww(:))

    wp = asin(dir(2))
    w = acos(dir(1)/cos(wp))*sign(1._PM_REEL, dir(3))
    w = MODULO(w, pm_deux_pi)    


  end SUBROUTINE MSP_tirage_direction




  real(KIND=PM_REEL) function MSP_nombre_aleatoire(flag_opt, ini_seed, sub_random) result(u)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_nombre_aleatoire
!
!$Resume
!  Cette routine génère un nombre aléatoire à partir de plusieurs technique de tirage
!  uniforme
!
!$Description
!  Cette routine génère un nombre aléatoire à partir de plusieurs technique de tirage
!  uniforme
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  u = MSP_nombre_aleatoire([flag_opt], [ini_seed], [sub_random])
!.    integer :: flag_opt
!.    integer :: ini_seed
!.    external sub_random
!
!$Arguments
!>[E]   flag_opt    :<integer>    Flag indiquant le choix du générateur de nombre aléatoire
!>[E]   ini_seed    :<integer>    graine à utiliser - permet de reproduire les résultats
!>[E]   sub_random  :<external>   Routine de generation de nombre aleatoire
!>S     u           :<PM_REEL>    Nombre aléatoire entre 0 et 1
!
!$Common
!
!$Routines
!- sub_random
!- random_number
!- MSP_varuni
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

    ! Parametres
    integer, intent(in), optional :: flag_opt
    integer, optional, intent(IN) :: ini_seed
    external sub_random
    optional :: sub_random

    ! Corps de la fonction

    if (PRESENT(sub_random)) then 
       call sub_random(u)

    elseif (PRESENT(flag_opt)) then 
       
       SELECT CASE(flag_opt)
          
       CASE (MSP_ENUM_RANDOM)  ! Generation par la fonction random

          call random_number(u)
          
       CASE (MSP_ENUM_GENERATEUR_ALEATOIRE)  ! Generation par la routine VAR_08

          call MSP_varuni (u,ini_seed=ini_seed)

       END SELECT

    else

       call random_number(u)

    end if

  end FUNCTION MSP_nombre_aleatoire

     
END MODULE MSP_TIRAGE
