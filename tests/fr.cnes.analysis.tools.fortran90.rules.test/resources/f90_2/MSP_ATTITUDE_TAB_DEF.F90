module MSP_ATTITUDE_TAB_DEF

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATTITUDE_TAB_DEF
!
!$Resume
!  Module contenant les informations de base aux modules liés aux lois d'attitude tabulées.
!
!$Description
!  Module contenant les informations de base aux modules liés aux lois d'attitude tabulées.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_ATTITUDE_TAB_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_ATTITUDE_TAB_DEF.F90,v $
!  Revision 1.21  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.20  2009/10/28 17:18:25  mercadig
!  DM-ID 1018: Mise a jour cartouche
!
!  Revision 1.19  2008/11/19 13:31:18  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.18  2008/07/04 15:01:22  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.17  2008/04/25 11:57:45  huec
!  AQ : Correction d erreurs dans les formats d ecriture
!  Revision 1.16  2008/04/24 13:59:03  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.15  2007/02/02 10:49:27  vivaresf
!  Version 4.4a1 : variables inutilisées
!  Revision 1.14  2007/02/02 10:04:22  vivaresf
!  Version 4.4a1 : validation (gestion de scas d'erreur sur attitude
!  tabulée : 0 en cas d'erreur)
!  Revision 1.13  2006/11/15 10:09:37  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.12  2006/11/15 09:58:09  tanguyy
!  DM-ID 552 : rajout d'un champ pour distinguer les types d'angles (Cardan, Euler..)
!  Revision 1.11  2005/03/08 07:32:34  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.10  2005/01/20 13:56:14  pauh
!  FA_332
!  Revision 1.9.2.1  2005/01/19 09:58:16  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.9  2004/11/05 16:27:03  vivaresf
!  coquilles
!  Revision 1.8  2004/10/25 10:15:01  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.7  2004/05/03 15:24:16  vivaresf
!  DM_83
!  Revision 1.6.2.1  2004/05/03 15:00:22  vivaresf
!  DM-ID 83,dates en jour / secondes avec origine MJD1950 ou MJD2000 type de date ABSOREL
!  Revision 1.6  2003/09/11 10:32:37  adm_ipsi
!  Ajout de la mention FA-ID 56
!  Revision 1.5  2003/09/03 15:31:22  adm_ipsi
!  FA-ID 56 MSP_consulter_attitude_tabulee, bug sur le calcul de la date de fin
!  Revision 1.4  2002/12/03 17:21:01  adm_ipsi
!   Ajout de implicit none
!  Revision 1.3  2002/11/07 18:37:06  adm_ipsi
!  Ajout de l'usage du module MSP_MECASPA_DEF
!  Revision 1.2  2002/11/07 18:20:30  adm_ipsi
!  MSP_modifier_attitude : deallocate généralisée sur tous les champs. MSP_effacer_attitude : ajout du paramètre nul, appelé au début de MSP_créer_attitude
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.7  2002/05/03 07:49:06  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!  Revision 1.6  2000/07/04 13:44:34  util_am
!  Ajout de la déclaration des variables : MSP_ENUM_ATTI_LVLH et MSP_ENUM_ATTI_YAW_STEERING
!  Mise à jour des cartouches
!  Revision 1.5  2000/06/22 11:40:38  util_am
!  Ajout d'un test de comparaison sur la taille des tableaux passés en argument
!  par rapport à la taille des tableaux présents dans la structure
!  Revision 1.4  2000/06/13 11:40:53  util_am
!  Gestion des fuites mémoire :
!      - Ajout du champ flag_func dans la structure MSP_attitude_tabulee
!      - Désallocation du second membre dans MSP_egal_attitude_tabulee si
!        la structure a été créée à partir d'une fonctin
!  Privatisation du contenu de la structure MSP_attitude_tabulee
!  Ajout des routines MSP_consulter_attitude_tabulee
!                     MSP_modifier_attitude_tabulee,
!                     MSP_afficher_attitude_tabulee
!  Mise à jour des cartouches : sections Voir-Aussi et Mots-cles
!  Revision 1.3  2000/02/24 12:52:10  util_am
!  Meilleure prise en compte des DEALLOCATE dans une égalité de loi
!  Revision 1.2  1999/10/25 15:27:35  util_am
!  Modification du type de repère inertiel (Gamma 50 CNES => inertiel)
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_ATTITUDE_TAB_DEF
!
!$Structure
!
!: MSP_ATTITUDE_TABULEE : définition d'une loi d'attitude tabulée
!#V
!>     flag_func   : <logical,private>                      Flag indiquant si la structure est créée par une fonction
!>     ntab        : <integer,private>                      nombre de points de tabulation.
!>     typdat      : <integer,private>                      type de date:
!.                                           1 => date [Jours Juliens CNES]
!.                                           2 => durée [s]
!.                                           3 => premiere date absolue, les autres en durée
!>     typangle    : <integer,private>                      Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
!>     origdat     : <integer,private>                      Origine des dates (0=J1950, 1=J2000)   
!>     dates       : <tm_jour_sec,DIM=(:),pointer,private>  dates tabulées en ordre croissant (typdat=1 ou 3) [j]
!>     durees      : <pm_reel,DIM=(:),pointer,private>      dates relatives (typdat=2 ou 3) [s]
!>     typrep      : <integer,private>                      type de repère:
!.                                           MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                           MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                           MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                           MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                           MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                           MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                           MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                           MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!.                                           MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                           MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>     psi         : <pm_reel,DIM=(:),pointer,private>      lacet [rad]
!>     teta        : <pm_reel,DIM=(:),pointer,private>      tangage [rad]
!>     phi         : <pm_reel,DIM=(:),pointer,private>      roulis [rad]
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_tabled_attitude
!- MSP_get_tabled_attitude_data
!- MSP_set_tabled_attitude_data
!- MSP_clear_tabled_attitude
!- MSP_display_tabled_attitude
!- MSP_effacer_attitude_tabulee
!- MSP_consulter_attitude_tabulee
!- MSP_modifier_attitude_tabulee
!- MSP_afficher_attitude_tabulee
!#V
!- MSP_egaler_attitude_tabulee
!#
!
!$Fonctions
!- MSP_creer_attitude_tabulee
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_MECASPA_DEF
!#
!
!$Interface
!> msp_display_tabled_attitude :   MSP_afficher_attitude_tabulee
!> assignment :                    MSP_egaler_attitude_tabulee
!> msp_set_tabled_attitude_data :  MSP_modifier_attitude_tabulee
!> msp_get_tabled_attitude_data :  MSP_consulter_attitude_tabulee
!> msp_create_tabled_attitude :    MSP_creer_attitude_tabulee
!> msp_clear_tabled_attitude :     MSP_effacer_attitude_tabulee
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ATTITUDE TABULEE
!
!$Voir-Aussi
!#V
!.  MSP_egaler_attitude_tabulee
!#
!.  MSP_creer_attitude_tabulee MSP_create_tabled_attitude MSP_get_tabled_attitude_data MSP_set_tabled_attitude_data
!.  MSP_clear_tabled_attitude MSP_display_tabled_attitude MSP_effacer_attitude_tabulee MSP_consulter_attitude_tabulee
!.  MSP_modifier_attitude_tabulee MSP_afficher_attitude_tabulee
!
!$<>
!******************************************************************************

   use MSLIB, only : pm_reel,tm_jour_sec
   use MSP_GESTION_ERREUR
   use MSP_MECASPA_DEF

   ! DEFINITIONS DE TYPES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATTITUDE_TAB_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   type MSP_ATTITUDE_TABULEE
      private
      logical                     :: flag_func
      integer                     :: ntab
      integer                     :: typdat
      integer                     :: typangle
      integer                     :: origdat
      type(tm_jour_sec),  pointer, dimension(:) :: dates => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: durees => NULL()
      integer                     :: typrep
      real(KIND=pm_reel), pointer, dimension(:) :: psi => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: teta => NULL()
      real(KIND=pm_reel), pointer, dimension(:) :: phi => NULL()
   end type MSP_ATTITUDE_TABULEE


   ! INTERFACES:

   interface ASSIGNMENT (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT
!
!$Resume
!  Opérateur permettant d'égaler deux lois d'attitude tabulées
!
!$Description
!  Opérateur permettant d'égaler deux lois d'attitude tabulées
!
!$Acces
!  PUBLIC
!
!$Usage
!   str2=str1
!.    type(MSP_ATTITUDE_TABULEE) :: str2
!.    type(MSP_ATTITUDE_TABULEE) :: str1
!
!$Procedures
!#V
!- MSP_egaler_attitude_tabulee
!#
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_egaler_attitude_tabulee
   end interface

   interface MSP_create_tabled_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_tabled_attitude
!
!$Resume
!  Creation of a tabled attitude
!
!$Description
!  Creation of a tabled attitude
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_create_tabled_attitude (ntab,typdat,dates,typrep,[psi],[teta],[phi],&
!.            origdat,typangle,dates_js)
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel), dimension(:) :: dates
!.    integer :: typrep
!.    real(KIND=pm_reel), dimension(:) :: psi,teta,phi
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:) :: dates_js
!.    integer :: typangle
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!
!$Procedures
!- MSP_creer_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_attitude_tabulee
   end interface

   interface MSP_get_tabled_attitude_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_tabled_attitude_data
!
!$Resume
!  Get attributes of a tabled attitude
!
!$Description
!  Get attributes of a tabled attitude
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_tabled_attitude_data(loi_tab, [ntab], [typdat], [typrep], [dates], &
!.           psi, teta, phi, datedeb, datefin, origdat, dates_js, datedeb_js, datefin_js, typangle)
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: ntab
!.    integer :: typdat
!.    integer :: typrep
!.    integer :: origdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: psi, teta, phi
!.    real(KIND=PM_REEL) :: datedeb,datefin
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!.    type(tm_jour_sec) :: datedeb_js
!.    type(tm_jour_sec) :: datefin_js
!.    integer :: typangle
!
!$Procedures
!- MSP_consulter_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_attitude_tabulee
   end interface
   
   interface MSP_set_tabled_attitude_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_tabled_attitude_data
!
!$Resume
!  Modify attributes of a tabled attitude
!
!$Description
!  Modify attributes of a tabled attitude
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_tabled_attitude_data(loi_tab, [typdat], [typrep], [dates], &
!.           psi, teta, phi, origdat, dates_js, typangle)
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: typdat
!.    integer :: typrep
!.    integer :: origdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    type(tm_jour_sec) , dimension(:), pointer :: dates_js
!.    real(KIND=PM_REEL), dimension(:), pointer :: psi, teta, phi
!.    integer :: typangle
!
!$Procedures
!- MSP_modifier_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_attitude_tabulee
   end interface

   interface MSP_clear_tabled_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_tabled_attitude
!
!$Resume
!  Clear the content of a tabled attitude
!
!$Description
!  Clear the content of a tabled attitude
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_tabled_attitude(loi_tab,[nul])
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_attitude_tabulee
   end interface

   interface MSP_display_tabled_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_tabled_attitude
!
!$Resume
!  Display the content of a tabled attitude
!
!$Description
!  Display the content of a tabled attitude
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_tabled_attitude(loi_tab, num)
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: num
!
!$Procedures
!- MSP_afficher_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! ATTITUDE TABULEE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_attitude_tabulee
   end interface

   ! SOUS-PROGRAMMES ET FONCTIONS
   private MSP_egaler_attitude_tabulee

   contains

   subroutine MSP_egaler_attitude_tabulee (str2,str1)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_attitude_tabulee
!
!$Resume
!  Sous-programme servant à surcharger "=" pour des types MSP_ATTITUDE_TABULEE.
!
!$Description
!  Sous-programme servant à surcharger "=" pour des types MSP_ATTITUDE_TABULEE.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_attitude_tabulee (str2,str1)
!.    type(MSP_ATTITUDE_TABULEE) :: str2
!.    type(MSP_ATTITUDE_TABULEE) :: str1
!
!$Arguments
!>E/S   str2  :<MSP_ATTITUDE_TABULEE>   variable à gauche du signe "=".
!>E     str1  :<MSP_ATTITUDE_TABULEE>   variable à droite du signe "=".
!
!$Common
!
!$Routines
!- MSP_effacer_attitude_tabulee
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ATTITUDE TABULEE EGALER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_ATTITUDE_TABULEE), intent(INOUT) :: str2
      type(MSP_ATTITUDE_TABULEE), intent(IN)  :: str1
      
      integer :: MSP_iostat
      
      MSP_iostat = 0


      str2%flag_func  = .false.

      str2%ntab       = str1%ntab
      str2%typdat     = str1%typdat
      str2%typangle   = str1%typangle
      str2%origdat    = str1%origdat

      if ( ASSOCIATED (str2%dates) ) DEALLOCATE(str2%dates,stat=MSP_iostat)
      ALLOCATE(str2%dates(str2%ntab))
      str2%dates(1:str2%ntab)   = str1%dates(1:str2%ntab)

      if ( ASSOCIATED (str2%durees) ) DEALLOCATE(str2%durees,stat=MSP_iostat)
      ALLOCATE(str2%durees(str2%ntab))
      str2%durees(1:str2%ntab)  = str1%durees(1:str2%ntab)

      str2%typrep     = str1%typrep

      if ( ASSOCIATED (str2%psi) ) DEALLOCATE(str2%psi,stat=MSP_iostat)
      ALLOCATE(str2%psi(str2%ntab))
      str2%psi(1:str2%ntab) = str1%psi(1:str2%ntab)
      if ( ASSOCIATED (str2%teta) ) DEALLOCATE(str2%teta,stat=MSP_iostat)
      ALLOCATE(str2%teta(str2%ntab))
      str2%teta(1:str2%ntab) = str1%teta(1:str2%ntab)
      if ( ASSOCIATED (str2%phi) ) DEALLOCATE(str2%phi,stat=MSP_iostat)
      ALLOCATE(str2%phi(str2%ntab))
      str2%phi(1:str2%ntab) = str1%phi(1:str2%ntab)

      if (str1%flag_func) call MSP_effacer_attitude_tabulee(str1)

    end subroutine MSP_egaler_attitude_tabulee



   SUBROUTINE MSP_effacer_attitude_tabulee(loi_tab,nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_attitude_tabulee
!
!$Resume
!  Cette routine efface le contenu d'une attitude tabulée
!
!$Description
!  Cette routine efface le contenu d'une attitude tabulée
!
!$Auteur
!  J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_attitude_tabulee(loi_tab,[nul])
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    logical :: nul
!
!$Arguments
!>E/S   loi_tab  :<MSP_ATTITUDE_TABULEE>   
!>[E/S] nul      :<logical>                si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
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
!  ATTITUDE TABULEE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     implicit none

     type(MSP_ATTITUDE_TABULEE) :: loi_tab
     logical, optional :: nul

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
        nullify(loi_tab%durees)
        nullify(loi_tab%dates)
        nullify(loi_tab%psi)
        nullify(loi_tab%teta)
        nullify(loi_tab%phi)
     else

        if ( ASSOCIATED (loi_tab%dates) )  DEALLOCATE(loi_tab%dates,stat=MSP_iostat)
        if ( ASSOCIATED (loi_tab%durees) ) DEALLOCATE(loi_tab%durees,stat=MSP_iostat)
        if ( ASSOCIATED (loi_tab%psi) )    DEALLOCATE(loi_tab%psi,stat=MSP_iostat)
        if ( ASSOCIATED (loi_tab%teta) )   DEALLOCATE(loi_tab%teta,stat=MSP_iostat)
        if ( ASSOCIATED (loi_tab%phi) )    DEALLOCATE(loi_tab%phi,stat=MSP_iostat)
     end if

     ! Remise à 0 des champs
     loi_tab%ntab       = 0              ! les tableaux sont de dim 0
     loi_tab%typdat     = 0              ! type de date non affacté
     loi_tab%typrep     = 0              ! type de repère non affecté
     loi_tab%origdat    = 0              ! origine des dates par défaut (1950) 
     loi_tab%typangle   = 0              ! type d'angle non affecté (sera créé
                                         ! par la fonction creer)

   end SUBROUTINE MSP_effacer_attitude_tabulee


   function MSP_creer_attitude_tabulee (ntab,typdat,dates,typrep,psi,teta,phi,&
        origdat,typangle,dates_js) result(loi)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_attitude_tabulee
!
!$Resume
!  Fonction servant à créer une loi de type MSP_ATTITUDE_TABULEE.
!
!$Description
!  Fonction servant à créer une loi de type MSP_ATTITUDE_TABULEE.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  loi = MSP_creer_attitude_tabulee (ntab,typdat,dates,typrep,[psi],[teta],[phi],&
!.            [origdat],[typangle],[dates_js])
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel), dimension(:) :: dates
!.    integer :: typrep
!.    real(KIND=pm_reel), dimension(:) :: psi,teta,phi
!.    integer :: origdat
!.    type(tm_jour_sec), dimension(:) :: dates_js
!.    integer :: typangle
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!
!$Arguments
!>E     ntab      :<integer>                nombre de points de tabulation.
!>E     typdat    :<integer>                type de date:
!.                                           1 => date [Jours Juliens CNES]
!.                                           2 => durée [s]
!.                                           3 => premiere date absolue, les autres en durée
!>E     dates     :<pm_reel,DIM=(:)>        dates tabulées en ordre croissant [JJ CNES ou s]
!>E     typrep    :<integer>                type de repère:
!.                                           MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                           MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                           MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                           MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                           MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                           MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                           MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                           MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!.                                           MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                           MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[E]   psi       :<pm_reel,DIM=(:)>        lacet ou incidence [rad] [par défaut 0.]
!>[E]   teta      :<pm_reel,DIM=(:)>        tangage ou dérapage [rad] [par défaut 0.]
!>[E]   phi       :<pm_reel,DIM=(:)>        roulis ou gite [rad] [par défaut 0.]
!>[E]   origdat   :<integer>                Origine des dates (0=J1950, 1=J2000)
!>[E]   typangle  :<integer>                Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
!>[E]   dates_js  :<tm_jour_sec,DIM=(:)>    dates tabulées en ordre croissant (typdat=1 ou 3) [j]
!>S     loi       :<MSP_ATTITUDE_TABULEE>   structure contenant les données de la poussée continue
!
!$Common
!
!$Routines
!- MSP_effacer_attitude_tabulee
!- MSP_signaler_message
!- md_jourfrac_joursec
!
!$Include
!
!$Module
!#V
!- MSLIB
!#
!
!$Remarques
!
!$Mots-cles
!  ATTITUDE TABULEE CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************
     use MSLIB

      implicit none

      ! Arguments obligatoires:
      integer, intent(IN)             :: ntab
      integer, intent(IN)             :: typdat
      real(KIND=pm_reel), intent(IN), dimension(:)  :: dates
      integer, intent(IN)             :: typrep

      ! Arguments optionnels:
      real(KIND=pm_reel), intent(IN), optional, dimension(:) :: psi,teta,phi
      integer, intent(IN), optional   :: origdat
      type(tm_jour_sec), optional,  intent(IN), dimension(:) :: dates_js
      integer, optional, intent(in) :: typangle

      ! Variable de sortie de la fonction:
      type(MSP_ATTITUDE_TABULEE) :: loi

      ! variables locales
      type(tm_jour_sec)     :: datejs
      integer               :: ii
      type(tm_code_retour)  :: code_retour

      ! corps de la fonction
      call MSP_effacer_attitude_tabulee(loi, nul=.true.)
      loi%flag_func = .true.
      
      ! variables fixes
      loi%ntab   = ntab
      loi%typdat = typdat
      loi%typrep = typrep

      ! Initialisations de la structure
      ALLOCATE(loi%dates(loi%ntab))
      do ii = 1, loi%ntab
         loi%dates(ii)%jour=0
         loi%dates(ii)%sec=0._PM_REEL
      enddo
      ALLOCATE(loi%durees(loi%ntab))
      loi%durees(1:loi%ntab)=0._PM_REEL

      ALLOCATE(loi%psi(loi%ntab))
      loi%psi(:)  = 0._pm_reel
      ALLOCATE(loi%teta(loi%ntab))
      loi%teta(:)  = 0._pm_reel
      ALLOCATE(loi%phi(ntab))
      loi%phi(:)  = 0._pm_reel
      loi%origdat=0


      ! Traitement des dates
      if (loi%typdat == 1) then
         if (present(dates_js)) then
            if ( size(dates_js) < ntab ) then
               call MSP_signaler_message (cle_mes="MSP_attitude_tab_004", &
                    partie_variable='"de dates (jour/sec)"')
               return 
            else
               loi%dates(1:loi%ntab)   = dates_js(1:loi%ntab)
            endif
         else 
            if ( size(dates) < ntab ) then
               call MSP_signaler_message (cle_mes="MSP_attitude_tab_004",&
                    partie_variable='"de dates"')
               return 
            else
               do ii = 1, loi%ntab
                  call md_jourfrac_joursec(dates(ii), datejs, code_retour)
                  loi%dates(ii) = datejs
               enddo
            endif
         endif
      else if(loi%typdat == 3) then
         if ( size(dates) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_004", &
                 partie_variable='"de durees"')
            return 
         else
            loi%durees(1) = 0._PM_REEL
            loi%durees(2:loi%ntab)   = dates(2:loi%ntab)
            call md_jourfrac_joursec(dates(1), datejs, code_retour)
            loi%dates(1) = datejs
         endif
         if (present(dates_js)) then
            if ( size(dates_js) < 1 ) then
               call MSP_signaler_message (cle_mes="MSP_attitude_tab_004", &
                    partie_variable='"de dates (jour/sec)"')
               return 
            else
               loi%dates(1) = dates_js(1)
             endif
         endif
      else
         if ( size(dates) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_004", &
                 partie_variable='"de durees"')
            return 
         else
            loi%durees(1:loi%ntab) = dates(1:loi%ntab)
         endif
      endif
      
      ! variables optionnelles
      !-----------------------

      if ( PRESENT(origdat) ) loi%origdat = origdat

      if ( PRESENT(psi) ) then
         if ( size(psi) < ntab ) then
         call MSP_signaler_message (cle_mes="MSP_attitude_tab_004",&
              partie_variable="'des angles de lacet'")
            return
         else
            loi%psi(1:loi%ntab) = psi(1:loi%ntab)
         endif
      endif

      if ( PRESENT(teta) ) then
         if ( size(teta) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_004",&
                 partie_variable="'des angles de tangage'")
            return
         else
            loi%teta(1:loi%ntab) = teta(1:loi%ntab)
         endif
      endif

      if ( PRESENT(phi) ) then
         if ( size(phi) < ntab ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_004",&
                 partie_variable="'des angles de roulis'")
            return
         else
            loi%phi(1:loi%ntab) = phi(1:loi%ntab)
         endif
      endif

      if ( PRESENT(typangle) ) then
         loi%typangle = typangle
      else
         ! par défaut, la convention utilisée est " Cardan, Z Y X" (angles de mécanique de vol)
         loi%typangle = pm_1z_2y_3x
      end if

   end function MSP_creer_attitude_tabulee
      
  SUBROUTINE MSP_consulter_attitude_tabulee(loi_tab, ntab, typdat, typrep, dates, &
       psi, teta, phi, datedeb, datefin, origdat, dates_js, datedeb_js, datefin_js, typangle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_attitude_tabulee
!
!$Resume
!  Routine de consultation du contenu d'une loi d'attitude tabulee
!
!$Description
!  Routine de consultation du contenu d'une loi d'attitude tabulee
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_attitude_tabulee(loi_tab, [ntab], [typdat], [typrep], [dates], &
!.           [psi], [teta], [phi], [datedeb], [datefin], [origdat], [dates_js], [datedeb_js], [datefin_js], [typangle])
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: ntab
!.    integer :: typdat
!.    integer :: typrep
!.    integer :: origdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    real(KIND=PM_REEL), dimension(:), pointer :: psi, teta, phi
!.    real(KIND=PM_REEL) :: datedeb,datefin
!.    type(tm_jour_sec), dimension(:), pointer :: dates_js
!.    type(tm_jour_sec) :: datedeb_js
!.    type(tm_jour_sec) :: datefin_js
!.    integer :: typangle
!
!$Arguments
!>E     loi_tab     :<MSP_ATTITUDE_TABULEE>          Loi d'attitude tabulee a consulter
!>[S]   ntab        :<integer>                       nombre de points dans la tabulation de la loi continue
!>[S]   typdat      :<integer>                       type de date utilisé (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVEou MSP_ENUM_LOI_ABSOREL)
!>[S]   typrep      :<integer>                       type du repère dans lequel sont exprimés les angles:
!.                                             MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                             MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                             MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                             MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                             MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                             MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                             MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                             MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!.                                             MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                             MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[E/S] dates       :<PM_REEL,DIM=(:),pointer>       tableau des dates de la tabulation (JJ CNES ou s)
!>[E/S] psi         :<PM_REEL,DIM=(:),pointer>       lacet ou incidence [rad]
!>[E/S] teta        :<PM_REEL,DIM=(:),pointer>       tangage ou dérapage [rad]
!>[E/S] phi         :<PM_REEL,DIM=(:),pointer>       roulis ou gite [rad]
!>[E/S] datedeb     :<PM_REEL>                       date de début de la loi
!>[E/S] datefin     :<PM_REEL>                       date de fin de la loi
!>[S]   origdat     :<integer>                       Origine des dates (0=J1950, 1=J2000) 
!>[E/S] dates_js    :<tm_jour_sec,DIM=(:),pointer>   dates tabulées en ordre croissant (typdat=1 ou 3) [j]
!>[E/S] datedeb_js  :<tm_jour_sec>                   
!>[E/S] datefin_js  :<tm_jour_sec>                   
!>[S]   typangle    :<integer>                       Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
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
!  ATTITUDE TABULEE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_ATTITUDE_TABULEE), intent(IN) :: loi_tab

    integer, intent(OUT), optional :: ntab
    integer, intent(OUT), optional :: typdat
    integer, intent(OUT), optional :: typrep
    integer, intent(OUT), optional :: origdat
    real(KIND=PM_REEL), dimension(:), pointer, optional :: dates
    real(KIND=PM_REEL), dimension(:), pointer, optional :: psi, teta, phi
    real(KIND=PM_REEL), optional :: datedeb,datefin
    type(tm_jour_sec),  dimension(:), pointer, optional :: dates_js
    type(tm_jour_sec),  optional :: datedeb_js
    type(tm_jour_sec),  optional :: datefin_js
    integer, optional, intent(out) :: typangle

    ! variables locales
    type(tm_code_retour) :: code_retour
    integer              :: ii
    integer		 :: MSP_iostat
    
    MSP_iostat = 0

    ! Corps de la fonction

    if (loi_tab%ntab == 0) return

    if (PRESENT(dates)) then 
       if (.not.ASSOCIATED(dates)) then 
          ALLOCATE(dates(loi_tab%ntab))
       else
          if (size(dates) /= size (loi_tab%dates)) then
             DEALLOCATE(dates,stat=MSP_iostat)
             ALLOCATE(dates(loi_tab%ntab))
          end if
       end if
    end if

    if (PRESENT(dates_js)) then 
       if (.not.ASSOCIATED(dates_js)) then 
         ALLOCATE(dates_js(loi_tab%ntab))
       else
          if (size(dates_js) /= size (loi_tab%dates)) then
             DEALLOCATE(dates_js,stat=MSP_iostat)
             ALLOCATE(dates_js(loi_tab%ntab))
          end if
       end if
    end if

    if (PRESENT(phi)) then 
       if (.not.ASSOCIATED(phi)) then
          ALLOCATE(phi(loi_tab%ntab))
       else
          if (size(phi) /= size (loi_tab%phi)) then
             DEALLOCATE(phi,stat=MSP_iostat)
             ALLOCATE(phi(loi_tab%ntab))
          end if
       end if
    end if

    if (PRESENT(teta)) then 
       if (.not.ASSOCIATED(teta)) then 
          ALLOCATE(teta(loi_tab%ntab))
       else
          if (size(teta) /= size (loi_tab%teta)) then
             DEALLOCATE(teta,stat=MSP_iostat)
             ALLOCATE(teta(loi_tab%ntab))
          end if
       end if
    end if

    if (PRESENT(psi)) then 
       if (.not.ASSOCIATED(psi)) then
          ALLOCATE(psi(loi_tab%ntab))
       else
          if (size(psi) /= size (loi_tab%psi)) then
             DEALLOCATE(psi,stat=MSP_iostat)
             ALLOCATE(psi(loi_tab%ntab))
          end if
       end if
    end if

    if (PRESENT(typdat))   typdat   = loi_tab%typdat
    if (PRESENT(typrep))   typrep   = loi_tab%typrep
    if (PRESENT(origdat))  origdat  = loi_tab%origdat
    if (PRESENT(ntab))     ntab     = loi_tab%ntab

    if (PRESENT(dates_js)) dates_js(1:loi_tab%ntab) = loi_tab%dates(1:loi_tab%ntab)
    if (PRESENT(phi))      phi(1:loi_tab%ntab)   = loi_tab%phi(1:loi_tab%ntab)
    if (PRESENT(teta))     teta(1:loi_tab%ntab)  = loi_tab%teta(1:loi_tab%ntab)
    if (PRESENT(psi))      psi(1:loi_tab%ntab)   = loi_tab%psi(1:loi_tab%ntab)

    if (PRESENT(dates)) then
       if (loi_tab%typdat == 1) then
          do ii=1, ntab
             call md_joursec_jourfrac(loi_tab%dates(ii), dates(ii), code_retour)
          enddo
       else if (loi_tab%typdat == 3) then
          dates(1:loi_tab%ntab) = loi_tab%durees(1:loi_tab%ntab)
          call md_joursec_jourfrac(loi_tab%dates(1), dates(1), code_retour)
       else
          dates(1:loi_tab%ntab) = loi_tab%durees(1:loi_tab%ntab)
       endif
    endif

    if (PRESENT(datedeb)) then
       if (loi_tab%typdat == 1.or.loi_tab%typdat == 3) then
          call md_joursec_jourfrac(loi_tab%dates(1), datedeb, code_retour)
       else
          datedeb = loi_tab%durees(1)
       endif
    endif

    if (PRESENT(datedeb_js)) datedeb_js = loi_tab%dates(1)
    if (PRESENT(datefin_js)) datefin_js = loi_tab%dates(loi_tab%ntab)

    if (PRESENT(datefin)) then
       if (loi_tab%typdat == 1) then
          call md_joursec_jourfrac(loi_tab%dates(loi_tab%ntab), datefin, code_retour)
       else
          datefin = loi_tab%durees(loi_tab%ntab)
       endif
    endif

    if(PRESENT(typangle)) typangle = loi_tab%typangle

  end SUBROUTINE MSP_consulter_attitude_tabulee
    
  SUBROUTINE MSP_modifier_attitude_tabulee(loi_tab, typdat, typrep, dates, &
       psi, teta, phi, origdat, dates_js, typangle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_attitude_tabulee
!
!$Resume
!  Routine de modification d'une loi d'attitude tabulee
!
!$Description
!  Routine de modification d'une loi d'attitude tabulee
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_attitude_tabulee(loi_tab, [typdat], [typrep], [dates], &
!.           [psi], [teta], [phi], [origdat], [dates_js], [typangle])
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: typdat
!.    integer :: typrep
!.    integer :: origdat
!.    real(KIND=PM_REEL), dimension(:), pointer :: dates
!.    type(tm_jour_sec) , dimension(:), pointer :: dates_js
!.    real(KIND=PM_REEL), dimension(:), pointer :: psi, teta, phi
!.    integer :: typangle
!
!$Arguments
!>E/S   loi_tab   :<MSP_ATTITUDE_TABULEE>          Loi d'attitude tabulée à modifier
!>[E]   typdat    :<integer>                       type de date utilisé (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVE ou MSP_ENUM_ABSOREL)
!>[E]   typrep    :<integer>                       type du repère dans lequel sont exprimés les angles. Valeur retournee :
!.                                             MSP_ENUM_ATTI_INERTIEL_G50 => Repère inertiel Gamma50 CNES
!.                                             MSP_ENUM_ATTI_INERTIEL_J2000 => Repère inertiel J2000
!.                                             MSP_ENUM_ATTI_INERTIEL_Gvrai => Repère inertiel Gamma vrai de la date
!.                                             MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                             MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                             MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                             MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                             MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!.                                             MSP_ENUM_ATTI_LVLH => Orbital local (LVLH)
!.                                             MSP_ENUM_ATTI_YAW_STEERING => mode yaw steering
!>[E/S] dates     :<PM_REEL,DIM=(:),pointer>       tableau des dates de la tabulation (JJ CNES ou s)
!>[E/S] psi       :<PM_REEL,DIM=(:),pointer>       lacet ou incidence [rad]
!>[E/S] teta      :<PM_REEL,DIM=(:),pointer>       tangage ou dérapage [rad]
!>[E/S] phi       :<PM_REEL,DIM=(:),pointer>       roulis ou gite [rad]
!>[E]   origdat   :<integer>                       Origine des dates (0=J50, 1=J2000)
!>[E/S] dates_js  :<tm_jour_sec,DIM=(:),pointer>   dates tabulées en ordre croissant (typdat=1 ou 3) [j]
!>[E/S] typangle  :<integer>                       Convention d'angles (code MSLIB du theme U) pour les lois d'attitude
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
!
!$Mots-cles
!  ATTITUDE TABULEE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_ATTITUDE_TABULEE), intent(INOUT) :: loi_tab

    integer, intent(IN), optional :: typdat
    integer, intent(IN), optional :: typrep
    integer, intent(IN), optional :: origdat
    real(KIND=PM_REEL), dimension(:), pointer, optional :: dates
    type(tm_jour_sec) , dimension(:), pointer, optional :: dates_js
    real(KIND=PM_REEL), dimension(:), pointer, optional :: psi, teta, phi
    integer, optional :: typangle

    ! Variables locales
    integer :: ntab_in, ii, MSP_iostat
    logical :: test, testdate
    type(tm_jour_sec), dimension(:), pointer :: pdates_js => NULL()
    type(tm_code_retour) :: code_retour

    MSP_iostat = 0
    
    ! test la cohérence de la taille des tableaux dans la structure d'entrée
    test = (size(loi_tab%dates) == size(loi_tab%phi)).and. &
         (size(loi_tab%phi) == size(loi_tab%teta)).and. &
         (size(loi_tab%durees) == size(loi_tab%durees)).and. &
         (size(loi_tab%teta) == size(loi_tab%psi))
    if (test) then 
        ntab_in=loi_tab%ntab
     else
       call MSP_signaler_message (cle_mes="MSP_attitude_tab_005")
       return
    end if


    if (PRESENT(typdat)) loi_tab%typdat = typdat
    if (PRESENT(typrep)) loi_tab%typrep = typrep
    if (PRESENT(origdat)) loi_tab%origdat = origdat


    ! Conversion préliminaire des dates
    testdate=.false.
    ntab_in = 0
    if (present(dates_js)) then
       if(associated(dates_js).and.loi_tab%typdat /= 2) then
          ntab_in = size(dates_js)
          testdate=.true.
          if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
          allocate(pdates_js(ntab_in))
          pdates_js(1:ntab_in) = dates_js(1:ntab_in)
       endif
       if (.not.associated(dates_js)) then
          call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
               partie_variable="dates_js")
          if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
          return
       endif
    else if (present(dates)) then
       if(associated(dates)) then
          ntab_in = size(dates)
          testdate=.true.
          if(loi_tab%typdat /= 2) then
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             allocate(pdates_js(ntab_in))
             do ii=1, ntab_in
                call md_jourfrac_joursec(dates(ii), pdates_js(ii), code_retour)
             enddo
          endif
       else
          call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
               partie_variable="dates")
          if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
          return
       endif
    endif

    ! Si tous les tableaux sont présents
    if (testdate.and.PRESENT(psi).and.PRESENT(teta).and.PRESENT(phi)) then
       if (ASSOCIATED(psi).and.ASSOCIATED(teta).and.ASSOCIATED(phi)) then
          test = (ntab_in == size(phi)).and.(ntab_in == size(teta)).and. &
                 (ntab_in == size(psi))
          if (test) then
             DEALLOCATE(loi_tab%dates, loi_tab%durees,stat=MSP_iostat)
             deallocate(loi_tab%psi, loi_tab%teta, loi_tab%phi,stat=MSP_iostat)
             ALLOCATE(loi_tab%dates(ntab_in), loi_tab%durees(ntab_in))
             allocate(loi_tab%psi(ntab_in), loi_tab%teta(ntab_in), loi_tab%phi(ntab_in))
             loi_tab%durees(:)=0._PM_REEL
             if(loi_tab%typdat/=2) loi_tab%dates(:) = pdates_js(:)
             if(loi_tab%typdat/=1.and.size(dates) >= ntab_in) &
                  loi_tab%durees(1:ntab_in) = dates(1:ntab_in)
             loi_tab%psi(:)   = psi(:)
             loi_tab%teta(:)  = teta(:)
             loi_tab%phi(:)   = phi(:)
             loi_tab%ntab     = ntab_in
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_006")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       else
          if (.not.ASSOCIATED(dates_js)) then 
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
                  partie_variable="date_jss")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          else if (.not.ASSOCIATED(dates)) then 
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
                  partie_variable="dates")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          else if (.not.ASSOCIATED(psi)) then 
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
                  partie_variable="psi")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          else if (.not.ASSOCIATED(teta)) then 
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
                  partie_variable="teta")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          else if (.not.ASSOCIATED(phi)) then 
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_007", &
                  partie_variable="phi")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
             
       end if

    ! Si les tableaux ne sont que partiellement présents
    else

       if (PRESENT(dates)) then 
          if (size(dates) == loi_tab%ntab) then
             if(loi_tab%typdat /= 2) &
                  loi_tab%dates(1:loi_tab%ntab) = pdates_js(1:loi_tab%ntab)
             if(loi_tab%typdat /= 1) &
                  loi_tab%durees(1:loi_tab%ntab) = dates(1:loi_tab%ntab)
             if(loi_tab%typdat == 3) loi_tab%durees(1)=0._PM_REEL
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_008", &
                  partie_variable="dates")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       end if
       
       if (PRESENT(dates_js)) then 
          if (size(dates_js) == loi_tab%ntab) then
             loi_tab%dates(1:loi_tab%ntab) = dates_js(1:loi_tab%ntab)
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_008", &
                  partie_variable="dates_js")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       end if

       if (PRESENT(psi)) then 
         if (size(psi) == loi_tab%ntab) then
             loi_tab%psi(:) = psi(:)
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_008", &
                  partie_variable="psi")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       end if

       if (PRESENT(teta)) then 
          if (size(teta) == loi_tab%ntab) then
             loi_tab%teta(:) = teta(:)
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_008", &
                  partie_variable="teta")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       end if

       if (PRESENT(phi)) then 
          if (size(phi) == loi_tab%ntab) then
             loi_tab%phi(:) = phi(:)
          else
             call MSP_signaler_message (cle_mes="MSP_attitude_tab_008", &
                  partie_variable="phi")
             if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)
             return
          end if
       end if
    end if

    if( PRESENT(typangle)) loi_tab%typangle = typangle

    if(associated(pdates_js)) deallocate(pdates_js,stat=MSP_iostat)

  end SUBROUTINE MSP_modifier_attitude_tabulee


  subroutine MSP_afficher_attitude_tabulee(loi_tab, num)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_attitude_tabulee
!
!$Resume
!  Affichage du contenu d'une loi d'attitude tabulée
!
!$Description
!  Affichage du contenu d'une loi d'attitude tabulée
!
!$Auteur
!  J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_attitude_tabulee(loi_tab, num)
!.    type(MSP_ATTITUDE_TABULEE) :: loi_tab
!.    integer :: num
!
!$Arguments
!>E     loi_tab  :<MSP_ATTITUDE_TABULEE>   
!>E     num      :<integer>                
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
!  ATTITUDE TABULEE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_ATTITUDE_TABULEE), intent(IN) :: loi_tab
    integer, intent(IN) :: num

    ! Variables locales
!    type(tm_code_retour) :: code_retour
!    real(kind=PM_REEL), dimension(:),pointer :: dates
!    integer :: ii
    character(len=8) :: oo
    integer :: ii

    ! Corps de la fonction

    write(num,'(a,i9)') "TYPDAT:  ",loi_tab%typdat

    oo="MJD1950"
    if (loi_tab%origdat == 1) oo="MJD2000"

    ! dates absolues
    if (loi_tab%typdat == 1) then
       write(num,'(a,a)') "DATES:   ", oo
       do ii=1,loi_tab%ntab
          write(num,'(g21.12)') loi_tab%dates(ii)
       end do

    ! dates absolues / relatives (dates(1) absolues, les autres en relatif)
    else if (loi_tab%typdat == 3) then
       write(num,'(a,a,i9,a,g21.12,a)') "DATES:   ", oo, loi_tab%dates(1)%jour, "-", &
            loi_tab%dates(1)%sec, ":"
       do ii=2,loi_tab%ntab
          write(num,'(g21.12)') loi_tab%durees(ii)
       end do

    ! dates relatives
    else 
       write(num,'(a)') "DATES:   "
       do ii=1,loi_tab%ntab
          write(num,'(g21.12)') loi_tab%durees(ii)
       end do
    endif

    write(num,'(a,i9)') "TYPREP:  ",loi_tab%typrep
    write(num,'(a,i9)') "Convention d'angles (code MSLIB) : ",loi_tab%typangle
    write(num,'(a)') "PSI:     "
    do ii=1,loi_tab%ntab
       write(num,'(g21.12)') loi_tab%psi(ii)
    end do
    write(num,'(a)') "TETA:    "
    do ii=1,loi_tab%ntab
       write(num,'(g21.12)') loi_tab%teta(ii)
    end do
    write(num,'(a)') "PHI:     "
    do ii=1,loi_tab%ntab
       write(num,'(g21.12)') loi_tab%phi(ii)
    end do

  end subroutine MSP_afficher_attitude_tabulee

end module  MSP_ATTITUDE_TAB_DEF
