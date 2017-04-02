module MSP_PROPULSION_CONT_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PROPULSION_CONT_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux poussées continues.
!
!$Description
!  Module contenant les informations de base aux modules liés aux poussées continues.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_PROPULSION_CONT_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_PROPULSION_CONT_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.18  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.17  2008/11/19 13:33:45  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.16  2008/04/24 13:59:06  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.15  2007/10/23 15:00:38  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.14  2005/03/08 07:32:37  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.13  2005/01/20 13:56:44  pauh
!  FA_332
!  Revision 1.12.2.1  2005/01/20 11:10:11  pauh
!  Appels DEALLOCATE avec l argument stat=MSP_iostat
!  Revision 1.12  2005/01/17 15:19:28  fabrec
!  DM-ID 175 : rajout des debits
!  Revision 1.11  2005/01/06 09:35:29  vivaresf
!  Presentation
!  Revision 1.10  2005/01/06 09:30:13  vivaresf
!  FA-ID 320 : desallocation memoire des variables locales de dates (dd)
!  Revision 1.9.2.1  2005/01/05 12:38:02  vivaresf
!  FA_320, version initiale
!  Revision 1.9  2004/11/05 16:27:04  vivaresf
!  coquilles
!  Revision 1.8  2004/10/25 10:15:02  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.7  2004/05/03 15:24:08  vivaresf
!  DM_83
!  Revision 1.6.2.1  2004/05/03 14:58:44  vivaresf
!  DM-ID 83,dates en jour / secondes avec origine MJD1950 ou MJD2000
!  type en date ABSOREL
!  Revision 1.6  2003/02/10 17:59:31  adm_ipsi
!  MSP_egaler_poussee_continue, ajout de tests sur la validité des pointeurs
!  Revision 1.5  2002/12/06 11:15:59  adm_ipsi
!  MSP_modiifer_poussee_continue, Correction du bug relevé par Isoscope sur 
!  le test associated(omegap) au lieu de associated(poussee)
!  Revision 1.4  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.3  2002/11/13 11:13:39  adm_ipsi
!  MSP_consulter_impulsion, les ordres allocate doivent s'éxécuter sur les 
!  paramètres de la fonction et pas sur les champs de la structure consultée
!  Revision 1.2  2002/11/07 18:37:26  adm_ipsi
!  Ajout de l'usage du module MSP_MECASPA_DEF
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.6  2002/09/16 11:12:37  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.5  2002/05/03 07:52:47  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!  Revision 1.4  2000/07/04 13:59:10  util_am
!  - Ajout de la ligne permettant de récupérer la variable "pas" dans MSP_consulter_poussee_continue
!  - Gestion plus complète de l'allocation des tableaux dynamiques dans MSP_consulter_poussee_continue
!  Revision 1.3  2000/06/15 07:14:35  util_am
!  - Ajout du champ flag_func dans la structure MSP_POUSSEE_CONTINUE pour la gestion des fuites mémoires
!  - Privatisation du contenu de la structure MSP_POUSSEE_CONTINUE
!  - Ajout de la MSP_afficher_poussee_continue
!  - Transfert des routines MSP_consulter_loi_cont, MSP_modifier_loi_cont de MSP_PROPULSION_CONT.F90
!    en les renommant en MSP_consulter_poussee_continue MSP_modifier_poussee_continue
!  - Ajout d'interface anglaise
!  - Mise à jour des cartouches
!  Revision 1.2  2000/02/24 12:51:39  util_am
!  Meilleure prise en compte des DEALLOCATE dans une égalité de loi
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_PROPULSION_CONT_DEF
!
!$Structure
!
!: MSP_POUSSEE_CONTINUE : définition d'une loi de poussée  continue
!#V
!>     flag_func   : <logical,private>                      Flag indiquant si la structure a été créée par une fonction
!>     ntab        : <integer,private>                      nombre de points de tabulation.
!>     typdat      : <integer,private>                      type de date:
!.                                            1 => date [Jours Juliens CNES]
!.                                            2 => durée [s]
!.                                            3 => date pour la premiere, durée pour les autres 
!>     dates       : <tm_jour_sec,DIM=(:),pointer,private>  dates tabulées en ordre croissant [JJ CNES] (typdat == 1 ou 3)
!>     durees      : <pm_reel,DIM=(:),pointer,private>      durees en ordred croissant [ s ] (typdat == 2 ou 3)
!>     poussee     : <pm_reel,DIM=(:),pointer,private>      poussées tabulées [N]
!>     isp         : <pm_reel,private>                      impulsion spécifique [s]
!>     dirref      : <integer,private>                      repère de référence dans lequel est définie la direction de poussée:
!>                                                      -1: par rapport au repère véhicule
!>                                                      >0: voir les repères définis pour l'attitude
!>     origdat     : <integer,private>                      origine des dates 0 : J1950, 1 : J2000
!>     omega       : <pm_reel,DIM=(:),pointer,private>      angles dans le plan de symétrie du véhicule tabulés [rad]
!>     omegap      : <pm_reel,DIM=(:),pointer,private>      angles hors du plan de symétrie du véhicule tabulés [rad]
!>     merg        : <pm_reel,private>                      masse d'ergols disponible [kg]
!>     pas         : <pm_reel,private>                      pas d'intégration pendant la poussée [s]
!>     debit       : <pm_reel,DIM=(:),pointer,private>      
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_spread_thrust
!- MSP_get_spread_thrust_data
!- MSP_set_spread_thrust_data
!- MSP_clear_spread_thrust
!- MSP_display_spread_thrust
!- MSP_effacer_poussee_continue
!- MSP_consulter_poussee_continue
!- MSP_modifier_poussee_continue
!- MSP_afficher_poussee_continue
!#V
!- MSP_egaler_poussee_continue
!#
!
!$Fonctions
!- MSP_creer_poussee_continue
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!- MSLIB
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> msp_display_spread_thrust :   MSP_afficher_poussee_continue
!> assignment :                  MSP_egaler_poussee_continue
!> msp_clear_spread_thrust :     MSP_effacer_poussee_continue
!> msp_get_spread_thrust_data :  MSP_consulter_poussee_continue
!> msp_set_spread_thrust_data :  MSP_modifier_poussee_continue
!> msp_create_spread_thrust :    MSP_creer_poussee_continue
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  PROPULSION CONTINUE
!
!$Voir-Aussi
!#V
!.  MSP_egaler_poussee_continue
!#
!.  MSP_creer_poussee_continue MSP_create_spread_thrust MSP_get_spread_thrust_data MSP_set_spread_thrust_data
!.  MSP_clear_spread_thrust MSP_display_spread_thrust MSP_effacer_poussee_continue MSP_consulter_poussee_continue
!.  MSP_modifier_poussee_continue MSP_afficher_poussee_continue
!
!$<>
!******************************************************************************

   use MSP_MECASPA_DEF
   use MSLIB, only : pm_reel, tm_jour_sec, tm_code_retour, &
        md_jourfrac_joursec, md_joursec_jourfrac
   use MSP_GESTION_ERREUR

   ! DEFINITIONS DE TYPES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PROPULSION_CONT_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


   type MSP_POUSSEE_CONTINUE
      private
      logical                     :: flag_func
      integer                     :: ntab
      integer                     :: typdat
      type(tm_jour_sec) , pointer, dimension(:) :: dates => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: durees => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: poussee => NULL()
      real(KIND=pm_reel)          :: isp
      integer                     :: dirref
      integer                     :: origdat
      real(KIND=pm_reel), pointer, dimension(:) :: omega => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: omegap => NULL()
      real(KIND=pm_reel)          :: merg
      real(KIND=pm_reel)          :: pas
      real(KIND=pm_reel), pointer, dimension(:) :: debit => NULL()
   end type MSP_POUSSEE_CONTINUE

   ! INTERFACES:

   interface ASSIGNMENT (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT
!
!$Resume
!  Assignment of spread thurst law to another
!
!$Description
!  Assignment of spread thurst law to another
!
!$Acces
!  PUBLIC
!
!$Usage
!   str2=str1
!.    type(MSP_POUSSEE_CONTINUE) :: str2
!.    type(MSP_POUSSEE_CONTINUE) :: str1
!
!$Procedures
!#V
!- MSP_egaler_poussee_continue
!#
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_egaler_poussee_continue
   end interface

   interface MSP_create_spread_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_spread_thrust
!
!$Resume
!  Creation of spread thrust law
!
!$Description
!  Creation of spread thurst law
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_spread_thrust (ntab,typdat,dates,poussee,isp,[dirref],[omega],[omegap],&
!.            merg,pas,debit,dates_js, origdat)
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel) :: dates(:)
!.    real(KIND=pm_reel) :: poussee(:)
!.    real(KIND=pm_reel) :: isp
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega(:)
!.    real(KIND=pm_reel) :: omegap(:)
!.    real(KIND=pm_reel) :: merg
!.    real(KIND=pm_reel) :: pas
!.    real(KIND=pm_reel) :: debit(:)
!.    integer :: origdat
!.    type(tm_jour_sec),dimension(:) :: dates_js
!.    type(MSP_POUSSEE_CONTINUE) :: loi
!
!$Procedures
!- MSP_creer_poussee_continue
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_poussee_continue
   end interface

   interface MSP_get_spread_thrust_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_spread_thrust_data
!
!$Resume
!  Get characteristics of a spread thrust law
!
!$Description
!  Get characteristics of a spread thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_spread_thrust_data(loi_cont, [ntab], [typdat], [dates], [poussee], [isp], &
!.           dirref, omega, omegap, merg, pas, debit, datedeb, datefin, dates_js, origdat, datedeb_js, datefin_js)
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: poussee
!.    real(KIND=PM_REEL) :: isp
!.    integer :: dirref
!.    real(KIND=PM_REEL), dimension(:), pointer :: omega
!.    real(KIND=PM_REEL), dimension(:), pointer :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: pas
!.    real(KIND=PM_REEL), dimension(:), pointer :: debit
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!
!$Procedures
!- MSP_consulter_poussee_continue
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_poussee_continue
   end interface

   interface MSP_set_spread_thrust_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_spread_thrust_data
!
!$Resume
!  Modify characteristics of a spread thrust law
!
!$Description
!  Modify characteristics of a spread thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_spread_thrust_data(loi_cont, [typdat], [dates], [poussee], [isp], &
!.           dirref, omega, omegap, merg, pas, debit, dates_js, origdat)
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: typdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: poussee
!.    real(KIND=PM_REEL) :: isp
!.    integer :: dirref
!.    real(KIND=PM_REEL), dimension(:), pointer :: omega
!.    real(KIND=PM_REEL), dimension(:), pointer :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: pas
!.    real(KIND=PM_REEL), dimension(:), pointer :: debit
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!
!$Procedures
!- MSP_modifier_poussee_continue
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_poussee_continue
   end interface

   interface MSP_clear_spread_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_spread_thrust
!
!$Resume
!  Clear characteristics of a spread thrust law
!
!$Description
!  Clear characteristics of a spread thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_spread_thrust (loi_cont,[nul])
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_poussee_continue
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_poussee_continue
   end interface

   interface MSP_display_spread_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_spread_thrust
!
!$Resume
!  Display characteristics of a spread thrust law
!
!$Description
!  Display characteristics of a spread thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_spread_thrust(loi_cont, num)
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: num
!
!$Procedures
!- MSP_afficher_poussee_continue
!
!$Remarques
!
!$Mots-cles
! PROPULSION CONTINUE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_poussee_continue
   end interface

   

   private MSP_egaler_poussee_continue

   ! SOUS-PROGRAMMES ET FONCTIONS

   contains

   subroutine MSP_egaler_poussee_continue (str2,str1)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_poussee_continue
!
!$Resume
!  Sous-programme servant à surcharger "=" pour des types MSP_POUSSEE_CONTINUE.
!
!$Description
!  Sous-programme servant à surcharger "=" pour des types MSP_POUSSEE_CONTINUE.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_poussee_continue (str2,str1)
!.    type(MSP_POUSSEE_CONTINUE) :: str2
!.    type(MSP_POUSSEE_CONTINUE) :: str1
!
!$Arguments
!>E/S   str2  :<MSP_POUSSEE_CONTINUE>   variable à gauche du signe "=".
!>E     str1  :<MSP_POUSSEE_CONTINUE>   variable à droite du signe "=".
!
!$Common
!
!$Routines
!- MSP_effacer_poussee_continue
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION CONTINUE EGALER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_POUSSEE_CONTINUE), intent(INOUT) :: str2
      type(MSP_POUSSEE_CONTINUE), intent(IN)  :: str1

      ! Variables locales
      integer :: MSP_iostat
      
      MSP_iostat = 0
      
      str2%flag_func = .false.

      str2%ntab       = str1%ntab
      str2%typdat     = str1%typdat
      if (ASSOCIATED (str2%dates) ) DEALLOCATE(str2%dates,stat=MSP_iostat)
      if (ASSOCIATED (str1%dates) ) then
         ALLOCATE(str2%dates(size(str1%dates)))
         str2%dates(:)   = str1%dates(:)
      endif

      if (ASSOCIATED (str2%durees) ) DEALLOCATE(str2%durees,stat=MSP_iostat)
      if (ASSOCIATED (str1%durees) ) then
         ALLOCATE(str2%durees(size(str1%durees)))
         str2%durees(:)   = str1%durees(:)
      endif

      if (ASSOCIATED (str2%poussee) ) DEALLOCATE(str2%poussee,stat=MSP_iostat)
      if (ASSOCIATED (str1%poussee) ) then
         ALLOCATE(str2%poussee(size(str1%poussee)))
         str2%poussee(:) = str1%poussee(:)
      endif

      str2%isp        = str1%isp
      str2%dirref     = str1%dirref

      if (ASSOCIATED (str2%omega) ) DEALLOCATE(str2%omega,stat=MSP_iostat)
      if (ASSOCIATED (str1%omega) ) then
         ALLOCATE(str2%omega(size(str1%omega)))
         str2%omega(:)   = str1%omega(:)
      endif

      if (ASSOCIATED (str2%omegap) ) DEALLOCATE(str2%omegap,stat=MSP_iostat)
      if (ASSOCIATED (str1%omegap) ) then
         ALLOCATE(str2%omegap(size(str1%omegap)))
         str2%omegap(:)  = str1%omegap(:)
      endif

      str2%pas        = str1%pas
      if (ASSOCIATED (str2%debit) ) DEALLOCATE(str2%debit,stat=MSP_iostat)
      if (ASSOCIATED (str1%debit) ) then
         ALLOCATE(str2%debit(size(str1%debit)))
         str2%debit(:)   = str1%debit(:)
      endif

      str2%merg       = str1%merg
      str2%origdat    = str1%origdat

    end subroutine MSP_egaler_poussee_continue


   subroutine MSP_effacer_poussee_continue (loi_cont,nul)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_poussee_continue
!
!$Resume
!  Récupération de la mémoire occupée par une structure MSP_POUSSEE_CONTINUE
!
!$Description
!  Récupération de la mémoire occupée par une structure MSP_POUSSEE_CONTINUE
!  desallocation et remise à zéro des tableaux alloués.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_poussee_continue (loi_cont,[nul])
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    logical :: nul
!
!$Arguments
!>E/S   loi_cont  :<MSP_POUSSEE_CONTINUE>   loi de poussée continue à effacer
!>[E/S] nul       :<logical>                si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
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
!  PROPULSION CONTINUE EFFACER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      logical, optional :: nul

      ! Variables locales
      logical :: nul_tmp
      integer :: MSP_iostat
      
      MSP_iostat = 0

      if ( present (nul) ) then
         nul_tmp = nul
      else
         nul_tmp = .false.
      endif

     if ( nul_tmp ) then
     ! On se contente d'enlever les liens sans désallouer
        nullify(loi_cont%dates)
        nullify(loi_cont%durees)
        nullify(loi_cont%poussee)
        nullify(loi_cont%omega)
        nullify(loi_cont%omegap)
        nullify(loi_cont%debit)
     else
        if ( ASSOCIATED (loi_cont%dates))   DEALLOCATE(loi_cont%dates,stat=MSP_iostat)
        if ( ASSOCIATED (loi_cont%durees))  DEALLOCATE(loi_cont%durees,stat=MSP_iostat)
        if ( ASSOCIATED (loi_cont%poussee)) DEALLOCATE(loi_cont%poussee,stat=MSP_iostat)
        if ( ASSOCIATED (loi_cont%omega))   DEALLOCATE(loi_cont%omega,stat=MSP_iostat)
        if ( ASSOCIATED (loi_cont%omegap))  DEALLOCATE(loi_cont%omegap,stat=MSP_iostat)
        if ( ASSOCIATED (loi_cont%debit))  DEALLOCATE(loi_cont%debit,stat=MSP_iostat)
     end if

     

    end subroutine MSP_effacer_poussee_continue


   function MSP_creer_poussee_continue (ntab,typdat,dates,poussee,isp,dirref,omega,omegap,&
        merg,pas,debit,dates_js, origdat) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_poussee_continue
!
!$Resume
!  Fonction servant à créer une loi de type MSP_POUSSEE_CONTINUE.
!
!$Description
!  Fonction servant à créer une loi de type MSP_POUSSEE_CONTINUE.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_poussee_continue (ntab,typdat,dates,poussee,isp,[dirref],[omega],[omegap],&
!.            [merg],[pas],[debit],[dates_js], [origdat])
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel) :: dates(:)
!.    real(KIND=pm_reel) :: poussee(:)
!.    real(KIND=pm_reel) :: isp
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega(:)
!.    real(KIND=pm_reel) :: omegap(:)
!.    real(KIND=pm_reel) :: merg
!.    real(KIND=pm_reel) :: pas
!.    real(KIND=pm_reel) :: debit(:)
!.    integer :: origdat
!.    type(tm_jour_sec),dimension(:) :: dates_js
!.    type(MSP_POUSSEE_CONTINUE) :: loi
!
!$Arguments
!>E/S   ntab      :<integer>                nombre de points de tabulation.
!>E     typdat    :<integer>                type de date:
!.                                            1 => date [Jours Juliens CNES]
!.                                            2 => durée [s]
!.                                            3 => date pour la premiere, durée pour les autres 
!>E     dates     :<pm_reel,DIM=(:)>        dates tabulées en ordre croissant [JJ CNES ou s]
!>E     poussee   :<pm_reel,DIM=(:)>        poussées tabulées [N]
!>E     isp       :<pm_reel>                impulsion spécifique [s]
!>[E]   dirref    :<integer>                repère de référence dans lequel est définie la direction de poussée
!>[E]   omega     :<pm_reel,DIM=(:)>        angles dans le plan de symétrie du véhicule tabulés [rad] [par défaut 0.]
!>[E]   omegap    :<pm_reel,DIM=(:)>        angles hors du plan de symétrie du véhicule tabulés [rad] [par défaut 0.]
!>[E]   merg      :<pm_reel>                masse d'ergols disponible [kg] [par défaut 0.]
!>[E]   pas       :<pm_reel>                pas d'intégration pendant la poussée [s] [par défaut 1.]
!>[E]   debit     :<pm_reel,DIM=(:)>        debits
!>[E]   dates_js  :<tm_jour_sec,DIM=(:)>    dates tabulées en ordre croissant [JJ CNES] (typdat == 1 ou 3)
!>[E]   origdat   :<integer>                origine des dates 0 : J1950, 1 : J2000
!>S     loi       :<MSP_POUSSEE_CONTINUE>   structure contenant les données de la poussée continue
!
!$Common
!
!$Routines
!- MSP_effacer_poussee_continue
!- MSP_signaler_message
!- md_jourfrac_joursec
!
!$Include
!
!$Module
!
!$Remarques
! si typdat == 2 dates_js n'est pas pris en compte
! si typdat == 1 dates_js est prioritaire sur dates
! si typdat == 3 dates_js(1) est prioritaire sur dates(1) et dates(2:*) est utilisé
!    pour la suite
!
!$Mots-cles
!  PROPULSION CONTINUE CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      ! Arguments obligatoires:
      integer                      :: ntab
      integer, intent(IN)          :: typdat
      real(KIND=pm_reel), intent(IN)           :: dates(:)
      real(KIND=pm_reel), intent(IN)           :: poussee(:)
      real(KIND=pm_reel), intent(IN)           :: isp

      ! Arguments optionnels:
      integer, intent(IN), optional            :: dirref
      real(KIND=pm_reel), intent(IN), optional :: omega(:)
      real(KIND=pm_reel), intent(IN), optional :: omegap(:)
      real(KIND=pm_reel), intent(IN), optional :: merg
      real(KIND=pm_reel), intent(IN), optional :: pas
      real(KIND=pm_reel), intent(IN), optional :: debit(:)
      integer, intent(IN), optional            :: origdat
      type(tm_jour_sec), intent(IN), optional,dimension(:) :: dates_js

      ! Variable de sortie de la fonction:
      type(MSP_POUSSEE_CONTINUE) :: loi

      ! Variables locales
      integer :: ii
      type(tm_code_retour) :: code_retour

      ! Initialisations
      call MSP_effacer_poussee_continue(loi, nul=.false.)
      call MSP_effacer_poussee_continue(loi, nul=.true.)

      loi%flag_func = .true.

      loi%ntab      = ntab
      loi%typdat    = typdat
      loi%isp       = isp
      loi%origdat   = 0
      loi%pas       = 1._pm_reel
      loi%merg      = 0._pm_reel
      loi%dirref    = -1
      ALLOCATE(loi%dates(loi%ntab))
      ALLOCATE(loi%durees(loi%ntab))
      ALLOCATE(loi%poussee(loi%ntab))
      ALLOCATE(loi%omega(loi%ntab))
      ALLOCATE(loi%omegap(loi%ntab))
      ALLOCATE(loi%debit(loi%ntab))
      loi%dates(:)%jour = 0
      loi%dates(:)%sec  = 0._PM_REEL
      loi%durees(:)     = 0._PM_REEL
      loi%poussee(:)    = 0._PM_REEL
      loi%omega(:)      = 0._PM_REEL
      loi%omegap(:)     = 0._PM_REEL
      loi%debit(:)      = 0._PM_REEL

      ! dates
      ! -----

      ! dates absolues : dates_js prioritaire s'il est présent
      if (typdat == 1) then
         if (present(dates_js)) then
            if (size(dates_js) >= ntab ) then
               loi%dates(1:loi%ntab)  = dates_js(1:loi%ntab)
            else
               call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                    partie_variable='"de dates_js"')
               return
            endif
         else if ( size(dates) >= ntab ) then
            do ii=1, loi%ntab
               call md_jourfrac_joursec(dates(ii), loi%dates(ii), code_retour)
            enddo
         else
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"de dates"')
            return
         endif
      ! dates absolues / relatives : dates_js prioritaire si present
      ! sur la premiere, durees pour les autres dates (2:n)
      else if(typdat == 3) then
         ! date initiale, dates_js(1) a defaut dates(1)
         if(present(dates_js)) then
            if  (size(dates_js) >= 1 ) then
               loi%dates(1)  = dates_js(1)
            else
               call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                    partie_variable='"des dates_js"')
               return
            endif
         else
            if (size(dates) >= 1 ) then
               call md_jourfrac_joursec(dates(1), loi%dates(1), code_retour) 
            endif 
         endif
         ! dates suivantes viennent obligatoirement de dates
         if  (size(dates) >= ntab ) then
            loi%durees(2:ntab) = dates(2:ntab)
         else
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"des dates_js"')
            return
         endif
      ! dates relatives : seul date est utilise
      else 
         if ( size(dates) >= ntab ) then
            loi%durees(1:ntab) = dates(1:ntab)
         else 
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"de dates"')
            return
         endif
      endif

      ! poussee
      if ( size(poussee) < ntab ) then
         call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
              partie_variable='"de poussées"')
         return
      else
         loi%poussee(1:ntab) = poussee(1:ntab)
      endif

      ! omega
      if ( PRESENT(omega) ) then
         if ( size(omega) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"des omega"')
            return
         else
            loi%omega(1:ntab)  = omega(1:ntab)
         endif
      endif

      if ( PRESENT(omegap) ) then
         if ( size(omegap) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"des omegap"')
            return
         else
            loi%omegap(1:ntab)  = omegap(1:ntab)
         endif
      endif

      if ( PRESENT(dirref) ) loi%dirref   = dirref
      if ( PRESENT(merg) )   loi%merg     = merg
      if ( PRESENT(pas) )    loi%pas      = pas
      ! debit
      if ( PRESENT(debit) ) then
         if ( size(debit) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_004",&
                 partie_variable='"des debits"')
            return
         else
            loi%debit(:)  = debit(:)
         endif
      endif

      if ( PRESENT(origdat)) loi%origdat  = origdat

   end function MSP_creer_poussee_continue
      
  SUBROUTINE MSP_consulter_poussee_continue(loi_cont, ntab, typdat, dates, poussee, isp, &
       dirref, omega, omegap, merg, pas, debit, datedeb, datefin, dates_js, origdat, datedeb_js, datefin_js)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_poussee_continue
!
!$Resume
!  Routine de consultation d'une loi continue
!
!$Description
!  Routine de consultation d'une loi continue
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_poussee_continue(loi_cont, [ntab], [typdat], [dates], [poussee], [isp], &
!.           [dirref], [omega], [omegap], [merg], [pas], [debit], [datedeb], [datefin], [dates_js], [origdat], [datedeb_js], [datefin_js])
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: poussee
!.    real(KIND=PM_REEL) :: isp
!.    integer :: dirref
!.    real(KIND=PM_REEL), dimension(:), pointer :: omega
!.    real(KIND=PM_REEL), dimension(:), pointer :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: pas
!.    real(KIND=PM_REEL), dimension(:), pointer :: debit
!.    real(KIND=PM_REEL) :: datedeb
!.    real(KIND=PM_REEL) :: datefin
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!.    type(tm_jour_sec) :: datedeb_js, datefin_js
!
!$Arguments
!>E     loi_cont    :<MSP_POUSSEE_CONTINUE>          Loi continue a consulter
!>[S]   ntab        :<integer>                       nombre de points dans la tabulation de la loi continue
!>[S]   typdat      :<integer>                       type de date:
!.                                             1 => date [Jours Juliens CNES]
!.                                             2 => durée [s]
!.                                             3 => date pour la premiere, durée pour les autres 
!>[E/S] dates       :<PM_REEL,DIM=(:),pointer>       tableau des dates de la tabulation  en ordre croissant [JJ CNES ou s]
!>[E/S] poussee     :<PM_REEL,DIM=(:),pointer>       tableau des poussées tabulées [N]
!>[S]   isp         :<PM_REEL>                       impulsion spécifique [s]
!>[S]   dirref      :<integer>                       repère de référence dans lequel est définie la direction de poussée
!>[E/S] omega       :<PM_REEL,DIM=(:),pointer>       tableau de l'angle dans le plan de symétrie du véhicule [rad]
!>[E/S] omegap      :<PM_REEL,DIM=(:),pointer>       tableau de l'angle hors du plan de symétrie du véhicule [rad]
!>[S]   merg        :<PM_REEL>                       masse d'ergols disponible [kg] 
!>[S]   pas         :<PM_REEL>                       pas d'intégration pendant la poussée [s] 
!>[E/S] debit       :<PM_REEL,DIM=(:),pointer>       tableau des debits
!>[S]   datedeb     :<PM_REEL>                       date de début de poussée
!>[S]   datefin     :<PM_REEL>                       date de fin de poussée
!>[E/S] dates_js    :<tm_jour_sec,DIM=(:),pointer>   dates tabulées en ordre croissant [JJ CNES] (typdat == 1 ou 3)
!>[S]   origdat     :<integer>                       origine des dates 0 : J1950, 1 : J2000
!>[E/S] datedeb_js  :<tm_jour_sec>                   
!>[E/S] datefin_js  :<tm_jour_sec>                   
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!
!$Include
!
!$Module
!
!$Remarques
! si typdat == 2 dates_js n'a pas de sens 
! si typdat == 1 dates_js est plus précis que dates
! si typdat == 3 dates_js(1) est plus précis que dates(1)
!                et dates_js(2:*) n'a pas de sens
!
!$Mots-cles
!  PROPULSION CONTINUE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_POUSSEE_CONTINUE), intent(IN) :: loi_cont

    integer, intent(OUT), optional :: ntab
    integer, intent(OUT), optional :: typdat
    real(KIND=PM_REEL), dimension(:), pointer, optional :: dates
    real(KIND=PM_REEL), dimension(:), pointer, optional :: poussee
    real(KIND=PM_REEL), intent(OUT), optional :: isp
    integer, intent(OUT), optional :: dirref
    real(KIND=PM_REEL), dimension(:), pointer, optional :: omega
    real(KIND=PM_REEL), dimension(:), pointer, optional :: omegap
    real(KIND=PM_REEL), intent(OUT), optional :: merg
    real(KIND=PM_REEL), intent(OUT), optional :: pas
    real(KIND=PM_REEL), dimension(:), pointer, optional :: debit
    real(KIND=PM_REEL), intent(OUT), optional :: datedeb
    real(KIND=PM_REEL), intent(OUT), optional :: datefin
    integer, intent(OUT), optional :: origdat
    type(tm_jour_sec), dimension(:), pointer, optional :: dates_js
    type(tm_jour_sec), optional :: datedeb_js, datefin_js

    ! Variables locales
    real(KIND=PM_REEL), dimension(:), pointer :: dd => NULL ()
    type(tm_code_retour) :: code_retour
    integer :: ii, MSP_iostat
    
    MSP_iostat = 0

    ! Initialisations des sorties et sorties temporaires
    if (loi_cont%ntab == 0) return

    if (associated(dd)) deallocate(dd,stat=MSP_iostat)
    allocate(dd(loi_cont%ntab))


    if (PRESENT(dates)) then 
       if (.not.ASSOCIATED(dates)) then 
          ALLOCATE(dates(loi_cont%ntab))
       else
          if (loi_cont%ntab /= size(dates)) then
             DEALLOCATE(dates,stat=MSP_iostat)
             nullify(dates)
             ALLOCATE(dates(loi_cont%ntab))
             dates(:) = 0._PM_REEL
          end if
       end if
    end if

    if (PRESENT(dates_js)) then 
       if (.not.ASSOCIATED(dates_js)) then 
          ALLOCATE(dates_js(loi_cont%ntab))
       else
          if (loi_cont%ntab /= size(dates_js)) then
             DEALLOCATE(dates_js,stat=MSP_iostat)
             nullify(dates_js)
             ALLOCATE(dates_js(loi_cont%ntab))
             dates_js%jour = 0
             dates_js%sec = 0._PM_REEL
          end if
       end if
    end if


    if (PRESENT(poussee)) then 
       if (.not.ASSOCIATED(poussee)) then 
          ALLOCATE(poussee(loi_cont%ntab))
       else
          if (size(loi_cont%poussee) /= size(poussee)) then
             DEALLOCATE(poussee,stat=MSP_iostat)
             nullify(poussee)
             ALLOCATE(poussee(loi_cont%ntab))
             poussee(:) = 0._PM_REEL
          end if
       end if
    end if

    if (PRESENT(dirref))  dirref  = loi_cont%dirref

    if (PRESENT(omega))   then 
       if (.not.ASSOCIATED(omega)) then
          ALLOCATE(omega(loi_cont%ntab))
       else
          if (size(loi_cont%omega) /= size(omega)) then
             DEALLOCATE(omega,stat=MSP_iostat)
             nullify(omega)
             ALLOCATE(omega(loi_cont%ntab))
             omega(:) = 0._PM_REEL
          end if
       end if
    end if

    if (PRESENT(omegap))  then 
       if (.not.ASSOCIATED(omegap)) then
          ALLOCATE(omegap(loi_cont%ntab))
       else
          if (size(loi_cont%omegap) /= size(omegap)) then
             DEALLOCATE(omegap,stat=MSP_iostat)
             nullify(omegap)
             ALLOCATE(omegap(loi_cont%ntab))
             omegap(:) = 0._PM_REEL
         end if
       end if
    end if

    if (PRESENT(debit))   then 
       if (.not.ASSOCIATED(debit)) then
          ALLOCATE(debit(loi_cont%ntab))
       else
          if (size(loi_cont%debit) /= size(debit)) then
             DEALLOCATE(debit,stat=MSP_iostat)
             nullify(debit)
             ALLOCATE(debit(loi_cont%ntab))
             debit(:) = 0._PM_REEL
          end if
       end if
    end if

    ! Loi relative
    dd   = loi_cont%durees(1:loi_cont%ntab)

    ! loi aboslue ou absolue relative (date(1) : absolue
    if (loi_cont%typdat == 1.or.loi_cont%typdat == 3 ) then
       call md_joursec_jourfrac(loi_cont%dates(1), dd(1), code_retour)
    endif

    ! date absolue
    if (loi_cont%typdat == 1) then
       do ii = 2, loi_cont%ntab
          call md_joursec_jourfrac(loi_cont%dates(ii), dd(ii), code_retour)
       enddo
    endif

    if (PRESENT(datedeb))  datedeb      = dd(1) 
    if (PRESENT(datefin))  datefin      = dd(loi_cont%ntab) 
    if (PRESENT(typdat))   typdat       = loi_cont%typdat
    if (PRESENT(ntab))     ntab         = loi_cont%ntab
    if (PRESENT(origdat))  origdat      = loi_cont%origdat
    if (PRESENT(dates))    dates(:)     = dd
    if (PRESENT(dates_js)) dates_js(:)  = loi_cont%dates(:)
    if (PRESENT(datedeb_js)) datedeb_js = loi_cont%dates(1)
    if (PRESENT(datefin_js)) datefin_js = loi_cont%dates(loi_cont%ntab)
    if (PRESENT(poussee))  poussee(:)   = loi_cont%poussee(:)
    if (PRESENT(isp))      isp          = loi_cont%isp
    if (PRESENT(omega))    omega(:)     = loi_cont%omega(:)
    if (PRESENT(omegap))   omegap(:)    = loi_cont%omegap(:)
    if (PRESENT(merg))     merg         = loi_cont%merg
    if (PRESENT(pas))      pas          = loi_cont%pas
    if (PRESENT(debit))    debit(:)     = loi_cont%debit(:)

    deallocate(dd,stat=MSP_iostat)

  end SUBROUTINE MSP_consulter_poussee_continue
    

  SUBROUTINE MSP_modifier_poussee_continue(loi_cont, typdat, dates, poussee, isp, &
       dirref, omega, omegap, merg, pas, debit, dates_js, origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_poussee_continue
!
!$Resume
!  Routine de modification d'une loi continue
!
!$Description
!  Routine de modification d'une loi continue
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_poussee_continue(loi_cont, [typdat], [dates], [poussee], [isp], &
!.           [dirref], [omega], [omegap], [merg], [pas], [debit], [dates_js], [origdat])
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: typdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: poussee
!.    real(KIND=PM_REEL) :: isp
!.    integer :: dirref
!.    real(KIND=PM_REEL), dimension(:), pointer :: omega
!.    real(KIND=PM_REEL), dimension(:), pointer :: omegap
!.    real(KIND=PM_REEL) :: merg
!.    real(KIND=PM_REEL) :: pas
!.    real(KIND=PM_REEL), dimension(:), pointer :: debit
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!
!$Arguments
!>E/S   loi_cont  :<MSP_POUSSEE_CONTINUE>          Loi continue à modifier
!>[E]   typdat    :<integer>                       type de date:
!.                                             1 => date [Jours Juliens CNES]
!.                                             2 => durée [s]
!.                                             3 => date pour la premiere, durée pour les autres 
!>[E/S] dates     :<PM_REEL,DIM=(:),pointer>       dates tabulées en ordre croissant [JJ CNES ou s]
!>[E/S] poussee   :<PM_REEL,DIM=(:),pointer>       poussées tabulées [N]
!>[E]   isp       :<PM_REEL>                       impulsion spécifique [s]
!>[E]   dirref    :<integer>                       repère de référence dans lequel est définie la direction de poussée
!>[E/S] omega     :<PM_REEL,DIM=(:),pointer>       tableau de l'angle dans le plan de symétrie du véhicule [rad]
!>[E/S] omegap    :<PM_REEL,DIM=(:),pointer>       tableau de l'angle hors du plan de symétrie du véhicule [rad]
!>[E]   merg      :<PM_REEL>                       masse d'ergols disponible [kg]
!>[E]   pas       :<PM_REEL>                       pas d'intégration pendant la poussée [s]
!>[E/S] debit     :<PM_REEL,DIM=(:),pointer>       tableau des debits
!>[E/S] dates_js  :<tm_jour_sec,DIM=(:),pointer>   dates tabulées en ordre croissant [JJ CNES] (typdat == 1 ou 3)
!>[E]   origdat   :<integer>                       origine des dates 0 : J1950, 1 : J2000
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_jourfrac_joursec
!
!$Include
!
!$Module
!
!$Remarques
! si typdat == 2 dates_js n'est pas pris en compte
! si typdat == 1 dates_js est prioritaire sur dates
! si typdat == 3 dates_js(1) est prioritaire sur dates(1) et dates(2:*) est utilisé
!    pour la suite
! Attention, si on change le type de dates, il faut les redonner (elles ne sont pas
! stockées dans les mêmes champs) 
!
!$Mots-cles
!  PROPULSION CONTINUE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    ! Arguements 
    type(MSP_POUSSEE_CONTINUE), intent(INOUT) :: loi_cont

    ! Arguements optionels
    integer, intent(IN), optional :: typdat
    real(KIND=PM_REEL), dimension(:), pointer, optional :: dates
    real(KIND=PM_REEL), dimension(:), pointer, optional :: poussee
    real(KIND=PM_REEL), intent(IN), optional :: isp
    integer, intent(IN), optional :: dirref
    real(KIND=PM_REEL), dimension(:), pointer, optional :: omega
    real(KIND=PM_REEL), dimension(:), pointer, optional :: omegap
    real(KIND=PM_REEL), intent(IN), optional :: merg
    real(KIND=PM_REEL), intent(IN), optional :: pas
    real(KIND=PM_REEL), dimension(:), pointer, optional :: debit
    integer, intent(IN), optional :: origdat
    type(tm_jour_sec), dimension(:), pointer, optional :: dates_js

    ! variables locales
    integer :: ntab, ntab_in, ii, MSP_iostat
    logical :: test, testdate
    type(tm_code_retour) :: code_retour
    
    MSP_iostat = 0

    ! Initialisation (test de coherence)
    test = ((size(loi_cont%dates) == loi_cont%ntab).and. &
         (size(loi_cont%poussee) == loi_cont%ntab).and. &
         (size(loi_cont%omega)   == loi_cont%ntab).and. &
         (size(loi_cont%omegap)  == loi_cont%ntab).and. &
         (size(loi_cont%debit)  == loi_cont%ntab).and. &
         (size(loi_cont%durees)  == loi_cont%ntab))
    if (test) then 
       ntab_in = loi_cont%ntab
    else
       call MSP_signaler_message (cle_mes="MSP_poussee_cont_005")
       return
    end if

    ! Variables scalaires
    if (PRESENT(typdat))  loi_cont%typdat     = typdat
    if (PRESENT(isp))     loi_cont%isp        = isp
    if (PRESENT(dirref))  loi_cont%dirref     = dirref
    if (PRESENT(merg))    loi_cont%merg       = merg
    if (PRESENT(pas))     loi_cont%pas        = pas
    if (PRESENT(origdat)) loi_cont%origdat    = origdat

    ! dates
    testdate=.false.
    ntab = 0
    if (present(dates_js)) then
       if (associated(dates_js).and.loi_cont%typdat == 1) then
          if (size(dates_js) > 0) then
             ntab = size(dates_js)
             testdate=.true.
          endif
       endif
       if (associated(dates_js).and.loi_cont%typdat == 3.and.present(dates)) then
          if (size(dates_js) > 0.and.associated(dates)) then
             ntab = size(dates)
             testdate=.true.
          endif
       endif
    else if (present(dates)) then
       if (associated(dates)) then
          if (size(dates) > 0) then
             ntab = size(dates)
             testdate=.true.
          endif
       endif
    endif


    ! Si tous les tableaux sont présents
    if (testdate.and.PRESENT(poussee).and.PRESENT(omega).and.PRESENT(omegap).and.PRESENT(debit)) then
       if (ASSOCIATED(poussee).and. ASSOCIATED(omega).and.ASSOCIATED(omegap).and.ASSOCIATED(debit)) then
          test = (ntab == size(omegap)).and. &
                 (size(omegap) == size(omega)).and. &
                 (size(omega) == size(debit)).and. &
                 (size(omega) == size(poussee))
          if (test) then
             DEALLOCATE(loi_cont%dates, loi_cont%durees, loi_cont%poussee, loi_cont%omega,  &
                  loi_cont%omegap,loi_cont%debit,stat=MSP_iostat)
             ALLOCATE(loi_cont%dates(ntab),loi_cont%durees(ntab), &
                  loi_cont%omega(ntab), loi_cont%omegap(ntab),loi_cont%poussee(ntab),loi_cont%debit(ntab))
             loi_cont%durees(:)  = 0._PM_REEL
             loi_cont%dates(:)%sec  = 0._PM_REEL
             loi_cont%dates(:)%jour = 0

             if (loi_cont%typdat == 1.and.present(dates_js)) then
                loi_cont%dates(:)   = dates_js(:)
             else if (loi_cont%typdat == 1) then
                do ii=1, loi_cont%ntab
                   call md_jourfrac_joursec(dates(ii), loi_cont%dates(ii), code_retour)
                enddo
             else if (loi_cont%typdat == 2) then
                loi_cont%durees(:)  = dates(:)
             else if (loi_cont%typdat == 3) then
                loi_cont%durees(:)  = dates(:)
                loi_cont%durees(1)  = 0._PM_REEL
                call md_jourfrac_joursec(dates(1), loi_cont%dates(1), code_retour)
                if(present(dates_js)) loi_cont%dates(1)   = dates_js(1)
             endif
             loi_cont%poussee(:) = poussee(:)
             loi_cont%omega(:)   = omega(:)
             loi_cont%omegap(:)  = omegap(:)
             loi_cont%ntab       = ntab
             loi_cont%debit(:)  = debit(:)
          else
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_006")
             return
          end if
       else
          if (.not.ASSOCIATED(dates_js)) then 
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                   partie_variable="dates_js")
             return
          else if (.not.ASSOCIATED(poussee)) then 
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                   partie_variable="poussee")
             return
          else if (.not.ASSOCIATED(omega)) then 
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                   partie_variable="omega")
             return
          else if (.not.ASSOCIATED(omegap)) then 
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                   partie_variable="omegap")
             return
          else if (.not.ASSOCIATED(debit)) then 
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_007", &
                   partie_variable="debit")
             return
          end if
             
       end if

    ! Si les tableaux ne sont que partiellement présents
    else
       if (PRESENT(dates_js)) then 
          ! dates absolues
          if (loi_cont%typdat == 1.and.size(dates_js) == ntab_in) then
             loi_cont%dates(1:ntab_in) = dates_js(1:ntab_in)
          ! dates absolues /relatives
          else if (loi_cont%typdat == 3.and.size(dates_js) > 0) then
             loi_cont%dates(2:ntab_in)%jour = 0
             loi_cont%dates(2:ntab_in)%sec  = 0._PM_REEL
             loi_cont%dates(1) = dates_js(1)
          else if (loi_cont%typdat /=2 ) then
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                   partie_variable="dates_js")
             return
          end if
       end if

       if (PRESENT(dates)) then 
          ! dates absolues et dates_js absent
          if (loi_cont%typdat == 1.and.size(dates) == ntab_in.and.&
               .not.PRESENT(dates_js)) then
             do ii=1,loi_cont%ntab
                call md_jourfrac_joursec(dates(ii), loi_cont%dates(ii), code_retour)
             enddo
          ! dates absolues/relatives et dates_js absent
          else if (loi_cont%typdat == 3.and.size(dates) > 0.and.&
               .not.PRESENT(dates_js)) then
             loi_cont%dates(2:ntab_in)%jour = 0
             loi_cont%dates(2:ntab_in)%sec  = 0._PM_REEL
             call md_jourfrac_joursec(dates(1), loi_cont%dates(1), code_retour)
             loi_cont%durees(2:ntab_in) = dates(2:ntab_in)
          ! dates absolues/relatives et dates_js present
          else if (loi_cont%typdat == 3.and.size(dates) >= ntab_in) then
             loi_cont%durees(2:ntab_in) = dates(2:ntab_in)
          ! dates relatives
          else if (loi_cont%typdat == 2.and.size(dates) >= ntab_in) then
             loi_cont%durees(1:ntab_in) = dates(1:ntab_in)
          else if (loi_cont%typdat /= 1.or..not.PRESENT(dates_js)) then
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                   partie_variable="dates")
             return
          end if
       end if

       if (PRESENT(poussee)) then 
          if (size(poussee) == ntab_in) then
             loi_cont%poussee(:) = poussee(:)
          else
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                   partie_variable="poussée")
             return
          end if
       end if

       if (PRESENT(omega)) then 
          if (size(omega) == ntab_in) then
             loi_cont%omega(:) = omega(:)
          else
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                   partie_variable="omega")
             return
          end if
       end if

       if (PRESENT(omegap)) then 
          if (size(omegap) == ntab_in) then
             loi_cont%omegap(:) = omegap(:)
          else
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                  partie_variable="omegap")
             return
          end if
       end if

       if (PRESENT(debit)) then 
          if (size(debit) == ntab_in) then
             loi_cont%debit(:) = debit(:)
          else
             call MSP_signaler_message (cle_mes="MSP_poussee_cont_008", &
                   partie_variable="debit")
             return
          end if
       end if
    end if

  end SUBROUTINE MSP_modifier_poussee_continue
    

  subroutine MSP_afficher_poussee_continue(loi_cont, num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_poussee_continue
!
!$Resume
!  Affichage des caractéristiques d'une loi de poussée continue
!
!$Description
!  Affichage des caractéristiques d'une loi de poussée continue
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_poussee_continue(loi_cont, num)
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: num
!
!$Arguments
!>E     loi_cont  :<MSP_POUSSEE_CONTINUE>   Loi de poussée continue à afficher
!>E     num       :<integer>                Unité logique d'affichage
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  PROPULSION CONTINUE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguement
    type(MSP_POUSSEE_CONTINUE), intent(IN) :: loi_cont
    integer, intent(IN) :: num

    ! Variables locales
    type(tm_code_retour) :: code_retour
    real(KIND=PM_REEL), dimension(:), pointer :: dd => NULL ()
    integer :: ii, MSP_iostat
    character(len=8) :: oo

    ! Corps de la fonction
    
    MSP_iostat = 0

    ! origine des dates
    oo="MJD1950"
    if (loi_cont%origdat == 1) oo="MJD2000"

    ! le tableau des dates
    if (associated(dd)) deallocate(dd,stat=MSP_iostat)
    allocate(dd(loi_cont%ntab))

    ! Loi relative
    dd   = loi_cont%durees(1:loi_cont%ntab)

    ! loi absolue ou absolue relative (date(1) : absolue
    if (loi_cont%typdat == 1.or.loi_cont%typdat == 3 ) then
       call md_joursec_jourfrac(loi_cont%dates(1), dd(1), code_retour)
    endif

    ! date absolue
    if (loi_cont%typdat == 1) then
       do ii = 2, loi_cont%ntab
          call md_joursec_jourfrac(loi_cont%dates(ii), dd(ii), code_retour)
       enddo
    endif

    ! ecriture
    write(num,'(a,i9)') "TYPDAT:  ",loi_cont%typdat

    ! dates absolues
    if (loi_cont%typdat == 1) then
       write(num,'(a,a)') "DATES:   ",oo
       do ii=1,loi_cont%ntab
          write(num,'(i9,g21.12)') loi_cont%dates(ii)
       end do
    ! dates absolue / relatives
    else if (loi_cont%typdat == 3) then
       write(num,'(a,a,i9,a,g21.12,a)') "DATES:   ", oo, loi_cont%dates(1)%jour, "-", &
            loi_cont%dates(1)%sec, ":"
       do ii=2,loi_cont%ntab
          write(num,'(g21.12)') loi_cont%durees(ii)
       end do
    ! dates relatives
    else if (loi_cont%typdat == 2) then
       write(num,'(a)') "DATES:   "
       do ii=1,loi_cont%ntab
          write(num,'(g21.12)') loi_cont%durees(ii)
       end do
    endif

    write(num,'(a)') "POUSSEE: "
    do ii=1,loi_cont%ntab
       write(num,'(g21.12)') loi_cont%poussee(ii)
    end do
    write(num,'(a,g21.12)') "ISP:     ",loi_cont%isp
    write(num,'(a,i9)') "DIRREF:  ",loi_cont%dirref
    write(num,'(a)') "OMEGA:   "
    do ii=1,loi_cont%ntab
       write(num,'(g21.12)') loi_cont%omega(ii)
    end do
    write(num,'(a)') "OMEGAP:  "
    do ii=1,loi_cont%ntab
       write(num,'(g21.12)') loi_cont%omegap(ii)
    end do
    write(num,'(a,g21.12)') "MERG:    ",loi_cont%merg
    write(num,'(a,g21.12)') "PAS:     ",loi_cont%pas
    write(num,'(a)') "DEBIT:   "
    do ii=1,loi_cont%ntab
       write(num,'(g21.12)') loi_cont%debit(ii)
    end do

    deallocate(dd,stat=MSP_iostat)
  end subroutine MSP_afficher_poussee_continue
   
end module  MSP_PROPULSION_CONT_DEF
