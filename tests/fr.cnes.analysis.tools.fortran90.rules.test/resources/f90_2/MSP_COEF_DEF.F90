module MSP_COEF_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_COEF_DEF
!
!$Resume
!	Module permettant de manipuler une structure coefficient (type coef aero)
!
!$Description
!	Module permettant de manipuler une struture coefficient (type coef aero)
!	Ce module inclut la defnition de la struture,et les routines de mamipulation
!
!$Auteur
!	S. ROUSSEAU
!
!$Version
!  $Id: MSP_COEF_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_COEF_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.26  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.25  2008/11/19 13:31:45  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.24  2008/08/08 14:20:18  gss
!  DM-ID 1058 : (portage g95) suppression de l'attribut target à la variable
!  tab_val de la routine coherence_coef.
!  Revision 1.23  2008/07/04 15:00:51  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.22  2008/02/22 13:55:06  huec
!  FA-ID 968 : Ajout du IMPLICIT NONE manquant
!  Revision 1.21  2007/11/27 14:55:19  huec
!  DM-ID 699 : Amelioration des moyens de tests MECASPA
!  Revision 1.20  2007/10/31 16:43:24  tanguyy
!  FA-ID 818 : initialisation correcte de la structure coef, avec mise a null des pointeurs
!  Revision 1.19  2007/09/24 15:43:20  tanguyy
!  Correction FA-ID 823 (mise à 0 d'un pointeur non initialisé)
!  Revision 1.18  2007/06/18 10:44:16  vivaresf
!  FA-ID 748 : suppression des dimensions en clair, écriture des chaines avec trim
!  Revision 1.17  2007/06/15 13:51:26  vivaresf
!  FA-ID 746 : MSP_lire_str_coef, mise en inout de la structure coef pour permettre la désallocation
!  Revision 1.16  2007/02/02 10:36:11  vivaresf
!  Version 4.4a1 : variables inutilisées
!  Revision 1.15  2006/06/02 11:21:53  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.14  2005/03/08 07:32:35  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.13  2005/01/20 13:56:40  pauh
!  FA_332
!  Revision 1.12.2.1  2005/01/19 14:19:59  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.12  2005/01/05 12:32:01  vivaresf
!  DM_115
!  Revision 1.11.2.1  2005/01/05 12:30:18  vivaresf
!  DM-ID 115 : routine d'interpolation
!  Revision 1.11  2004/11/05 16:27:03  vivaresf
!  coquilles
!  Revision 1.10  2004/10/25 10:15:01  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.9  2004/07/07 10:30:28  vivaresf
!  Mise a jour des cartouches
!  Revision 1.8  2004/05/27 16:10:18  vivaresf
!  FA-ID 114, messages d'erreur pour MSP_COEF_DEF
!  Revision 1.7.2.1  2004/05/27 16:05:06  vivaresf
!  FA-ID 114, initialisation des pointeurs a NULL()
!  Revision 1.7  2003/01/07 15:24:56  adm_ipsi
!  egaler_coef, suppression du bug sur l'affectation de par3_coef. MSP_modifier_coef, remplacement des and par des or
!  Revision 1.6  2002/12/11 10:18:09  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.5  2002/12/04 18:08:24  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.4  2002/12/03 17:21:01  adm_ipsi
!   Ajout de implicit none
!  Revision 1.3  2002/11/13 17:02:46  adm_ipsi
!  MSP_créer_coef, le allocate de com_par_coef ne s'effectuait pas systématiquement. Les tests if associated sur les champs de la structure sont inutiles
!  Revision 1.2  2002/10/04 16:14:05  adm_ipsi
!  MSP_COEF_DEF.F90
!      . Modification concernant l'effacement avant création : nullify sur premier effacement
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!
!$FinHistorique
!
!$Usage
!  use MSP_COEF_DEF
!
!$Structure
!
!: MSP_COEF : 
!#V
!>     nom_coef       : <LEN=MSP_LONG_CHAINE,private>                  nom de la structure coefficient 
!>     flag_func      : <logical,private>                              booleen permettant de de deallouer proporement la structure
!>     dim_coef       : <integer,private>                              dimension du tableau de coefficient (1,2, ou 3)
!>     com_par_coef   : <LEN=MSP_LONG_CHAINE,DIM=(:),pointer,private>  tableau contenant la description des differents parametres du tableau
!>     nom_par_coef   : <LEN=MSP_LONG_CHAINE,DIM=(:),pointer,private>  nom du tableau associe au differents paramteres
!>     coef_1d        : <pm_reel,DIM=(:),pointer,private>              tableau a 1 dimension du tableau de coef
!>     coef_2d        : <pm_reel,DIM=(:,:),pointer,private>            tableau a 2 dimensions du tableau de coef
!>     coef_3d        : <pm_reel,DIM=(:,:,:),pointer,private>          tableau a 3 dimensions du tableau de coef
!>     par1_coef      : <pm_reel,DIM=(:),pointer,private>              parametre 1 du tableau de coef
!>     par2_coef      : <pm_reel,DIM=(:),pointer,private>              parametre 2 du tableau de coef
!>     par3_coef      : <pm_reel,DIM=(:),pointer,private>              parametre 3 du tableau de coef
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_coef
!- MSP_clear_coef
!- MSP_get_coef
!- MSP_set_coef
!- MSP_display_coef
!- MSP_effacer_coef
!- MSP_lire_str_coef
!- MSP_consulter_coef
!- MSP_modifier_coef
!- MSP_afficher_coef
!#V
!- egaler_coef
!- coherence_coef
!#
!
!$Fonctions
!- MSP_creer_coef
!- MSP_interp_coef
!
!$Include
!
!$Module
!#V
!- MSPRO
!- MSP_GESTION_ERREUR
!- MSP_MECASPA_DEF
!- MSP_ACCES
!#
!
!$Interface
!> msp_display_coef :  MSP_afficher_coef
!> msp_clear_coef :    MSP_effacer_coef
!> assignment :        egaler_coef
!> msp_set_coef :      MSP_modifier_coef
!> msp_create_coef :   MSP_creer_coef
!> msp_get_coef :      MSP_consulter_coef
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  egaler_coef coherence_coef
!#
!.  MSP_creer_coef MSP_interp_coef MSP_create_coef MSP_clear_coef MSP_get_coef MSP_set_coef
!.  MSP_display_coef MSP_effacer_coef MSP_lire_str_coef MSP_consulter_coef MSP_modifier_coef
!.  MSP_afficher_coef
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   use MSPRO
   use MSP_GESTION_ERREUR
   use MSP_MECASPA_DEF

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_COEF_DEF.F90 365 2013-02-18 12:36:19Z aadt $'



   ! DEFINITIONS DE TYPES:

! Type coefficient
   type MSP_COEF

      private
      character(len=MSP_LONG_CHAINE)::nom_coef
      logical :: flag_func
      integer :: dim_coef

      character(LEN=MSP_LONG_CHAINE), pointer, dimension(:) :: com_par_coef => NULL()
      character(LEN=MSP_LONG_CHAINE), pointer, dimension(:) :: nom_par_coef => NULL()

      real (KIND=pm_reel), pointer, dimension(:)     :: coef_1d   => NULL()
      real (KIND=pm_reel), pointer, dimension(:,:)   :: coef_2d   => NULL()

      real (KIND=pm_reel), pointer, dimension(:,:,:) :: coef_3d   => NULL()
      real (KIND=pm_reel), pointer, dimension(:)     :: par1_coef => NULL()
      real (KIND=pm_reel), pointer, dimension(:)     :: par2_coef => NULL()
      real (KIND=pm_reel), pointer, dimension(:)     :: par3_coef => NULL()
   end type MSP_COEF



   ! SOUS-PROGRAMMES ET FONCTIONS

   private :: egaler_coef
   private :: coherence_coef

   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  coefa=coefb
!.    type(MSP_COEF) :: coefa
!.    type(MSP_COEF) :: coefb
!
!$Procedures
!#V
!- egaler_coef
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

      module procedure egaler_coef
   end interface


   interface MSP_create_coef

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_coef
!
!$Resume
!	creates a structure coef
!
!$Description
!	creates a structure coef
!
!$Acces
!  PUBLIC
!
!$Usage
!  coef = MSP_create_coef  ([dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],[par2_coef],&
!.             par3_coef,com_par_coef,nom_coef)
!.    integer :: dim_coef
!.    character(LEN=*) :: nom_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!.    type(MSP_COEF) :: coef
!
!$Procedures
!- MSP_creer_coef
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_coef
   end interface


   interface MSP_clear_coef

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_coef
!
!$Resume
!  Clears the content of a coefficient structure
!
!$Description
!  Clears the content of a coefficient structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_coef(coef, [nul])
!.    type(MSP_COEF) :: coef
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_coef
!
!$Remarques
!
!$Mots-cles
! VEHICULE COEFDYNAMIQUE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      module procedure MSP_effacer_coef
   end interface



   interface MSP_get_coef

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_coef
!
!$Resume
!  Gets information on coefficient characteristics
!
!$Description
!  Gets information on coefficient characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_coef (coef,[dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],&
!.            par2_coef,par3_coef,com_par_coef,nom_coef)
!.    type(MSP_COEF) :: coef
!.    integer :: dim_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    character(len=*) :: nom_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!
!$Procedures
!- MSP_consulter_coef
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module procedure MSP_consulter_coef
   end interface

   interface MSP_set_coef

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_coef
!
!$Resume
!  Modifies information on coefficient characteristics
!
!$Description
!  Modifies information on coefficient characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_coef (coef, [dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],&
!.            par2_coef,par3_coef,com_par_coef,nom_coef)
!.    type(MSP_COEF) :: coef
!.    integer :: dim_coef
!.    character(LEN=*) :: nom_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!
!$Procedures
!- MSP_modifier_coef
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      module procedure MSP_modifier_coef
   end interface

   interface MSP_display_coef

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_coef
!
!$Resume
!  Displays information on coefficient characteristics
!
!$Description
!  Displays information on coefficient characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_coef (coef,[ilog])
!.    type(MSP_COEF) :: coef
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_coef
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      module procedure MSP_afficher_coef
   end interface


   contains


    subroutine egaler_coef(coefa,coefb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_coef
!
!$Resume
!	Routine definissant l'affectation de 2 structures coefficient
!
!$Description
!	Routine definissant l'affectation de 2 structures coefficient
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_coef(coefa,coefb)
!.    type(MSP_COEF) :: coefa
!.    type(MSP_COEF) :: coefb
!
!$Arguments
!>E/S   coefa  :<MSP_COEF>   structure COEF à gauche de =
!>E     coefb  :<MSP_COEF>   structure COEF à droite de =
!
!$Common
!
!$Routines
!- MSP_effacer_coef
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
      type(MSP_COEF),intent(inout)::coefa
      type(MSP_COEF),intent(in)::coefb


      ! Corps de la fonction
      call MSP_effacer_coef(coefa)

      coefa%flag_func = .false.

      coefa%nom_coef = coefb%nom_coef

      coefa%dim_coef = coefb%dim_coef


      if ( associated(coefb%com_par_coef)) then
         allocate(coefa%com_par_coef(size(coefb%com_par_coef)))
         coefa%com_par_coef(:) = coefb%com_par_coef(:)
      end if
      if ( associated(coefb%nom_par_coef)) then
         allocate(coefa%nom_par_coef(size(coefb%nom_par_coef)))
         coefa%nom_par_coef(:) = coefb%nom_par_coef(:)
      end if


      if ( associated(coefb%coef_1d)) then
         allocate(coefa%coef_1d(size(coefb%coef_1d)))
         coefa%coef_1d(:) = coefb%coef_1d(:)
      end if


      if ( associated(coefb%coef_2d)) then
         allocate(coefa%coef_2d(size(coefb%coef_2d,dim=1),&
              size(coefb%coef_2d,dim=2)))
         coefa%coef_2d(:,:) = coefb%coef_2d(:,:)
      end if

      if ( associated(coefb%coef_3d)) then
         allocate(coefa%coef_3d(size(coefb%coef_3d,dim=1),&
              size(coefb%coef_3d,dim=2),size(coefb%coef_3d,dim=3)))
         coefa%coef_3d(:,:,:) = coefb%coef_3d(:,:,:)
      end if


      if ( associated(coefb%par1_coef)) then
         allocate(coefa%par1_coef(size(coefb%par1_coef)))
         coefa%par1_coef(:) = coefb%par1_coef(:)
      end if
      if ( associated(coefb%par2_coef)) then
         allocate(coefa%par2_coef(size(coefb%par2_coef)))
         coefa%par2_coef(:) = coefb%par2_coef(:)
      end if

      if ( associated(coefb%par3_coef)) then
         allocate(coefa%par3_coef(size(coefb%par3_coef)))
         coefa%par3_coef(:) = coefb%par3_coef(:)
      end if

    end subroutine egaler_coef

    subroutine MSP_effacer_coef(coef, nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_coef
!
!$Resume
!	Routine permettant de désallouer proprement une structure coef
!
!$Description
!	Routine permettant de désallouer proprement une structure coef
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_coef(coef, [nul])
!.    type(MSP_COEF) :: coef
!.    logical :: nul
!
!$Arguments
!>E/S   coef  :<MSP_COEF>   Structure COEF à effacer
!>[E]   nul   :<logical>    =.true., on se contente des instructions NULLIFY (par défaut .false.)
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
!  VEHICULE COEFDYNAMIQUE EFFACER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      ! Parametres
      type(MSP_COEF) :: coef
      logical, intent(in), optional :: nul
       
      ! Variables locales
      logical :: nul_tmp
      integer :: MSP_iostat

      ! Corps de la fonction
      MSP_iostat = 0
      
      if ( present (nul) ) then
         nul_tmp = nul
      else
         nul_tmp = .false.
      endif

    if ( nul_tmp ) then

       ! On se contente d'enlever les liens sans désallouer
       nullify(coef%com_par_coef)
       nullify(coef%nom_par_coef)
       nullify(coef%coef_1d)
       nullify(coef%coef_2d)
       nullify(coef%coef_3d)
       nullify(coef%par1_coef)
       nullify(coef%par2_coef)
       nullify(coef%par3_coef)

   else

      coef%dim_coef = 0

      coef%nom_coef = ""

      if ( associated(coef%com_par_coef)) then
         deallocate(coef%com_par_coef,stat=MSP_iostat)
      end if
      if ( associated(coef%nom_par_coef)) then
         deallocate(coef%nom_par_coef,stat=MSP_iostat)
      end if


      if ( associated(coef%coef_1d)) then
         deallocate(coef%coef_1d,stat=MSP_iostat)
      end if

      if ( associated(coef%coef_2d)) then
         deallocate(coef%coef_2d,stat=MSP_iostat)
      end if

      if ( associated(coef%coef_3d)) then
         deallocate(coef%coef_3d,stat=MSP_iostat)
      end if

      if ( associated(coef%par1_coef)) then
         deallocate(coef%par1_coef,stat=MSP_iostat)
      end if
      if ( associated(coef%par2_coef)) then
         deallocate(coef%par2_coef,stat=MSP_iostat)
      end if

      if ( associated(coef%par3_coef)) then
         deallocate(coef%par3_coef,stat=MSP_iostat)
      end if

   end if

 end subroutine MSP_effacer_coef

 
   subroutine MSP_lire_str_coef (acc,nom_struc,coef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_str_coef
!
!$Resume
!	Routine permettant de lire une struture Coef dans un fichier madona
!
!$Description
!	Routine permettant de lire une struture Coef dans un fichier madona
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_str_coef (acc,nom_struc,coef)
!.    integer :: acc
!.    character(LEN=*) :: nom_struc
!.    type (MSP_COEF) :: coef
!
!$Arguments
!>E     acc        :<integer>    zone d'acces Madona
!>E     nom_struc  :<LEN=*>      Nom de la structure madona
!>S     coef       :<MSP_COEF>   structure coeeficient
!
!$Common
!
!$Routines
!- MSP_effacer_coef
!- MSP_signaler_message
!- MSP_acc_get_tab
!#V
!- coherence_coef
!#
!
!$Include
!
!$Module
!#V
!- MSP_ACCES
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


      use MSP_ACCES

      implicit none

      ! Parametres
      integer, intent(IN)                           :: acc
      character(LEN=*), intent(IN)                  :: nom_struc
      type (MSP_COEF),intent(OUT)                   :: coef

      ! Variables locales
      integer :: ier,dim_coeff_1,dim_coeff_2,i,iexist
      character(len=50)::unite

      real(kind=PM_REEL),dimension(:),pointer :: par1_c => NULL()
      real(kind=PM_REEL),dimension(:),pointer :: c_1d   => NULL()

      integer :: dim_c


      ! Corps de la fonction
      call MSP_effacer_coef(coef,nul=.true.)


      coef%flag_func = .false.

      
      ! Affectation a une valeur par defaut
      allocate(c_1d(1),par1_c(1))
      c_1d(1) = 0._pm_reel
      par1_c(1) = 0._PM_REEL
      dim_c = 1

      coef%nom_coef=nom_struc

      ! Lecture de la structure coef
      iexist = acc_exist(acc,trim(nom_struc))
      if ( iexist <= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",&
              partie_variable=nom_struc, &
              routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
         if (associated(c_1d)) deallocate(c_1d)
         if (associated(par1_c)) deallocate(par1_c)
         return
      else  if ( iexist == 1 ) then

         ! Accès à la structure du coefficient:
         ier = acc_select (acc,nom_struc,ACC_STRUCT)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='de la structure '//trim(nom_struc), &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif
         

         ! Lecture du nom des abcisses, ordonnées ...:
         call MSP_acc_get_tab (acc,"com_par_"//trim(nom_struc),&
              coef%com_par_coef,dim1=dim_coeff_1)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='de com_par_ '//trim(nom_struc), &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif
      
         call MSP_acc_get_tab (acc,"nom_par_"//trim(nom_struc),&
              coef%nom_par_coef,dim1=dim_coeff_2)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='de nom_par_'//trim(nom_struc), &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif

         ! Cohérence des dimensions des tableaux:
         if ( dim_coeff_1 /= dim_coeff_2 ) then
            call MSP_signaler_message (cle_mes="MSP_lire_str_coef_001",&
                 partie_variable=trim(nom_struc))
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         else
            coef%dim_coef = dim_coeff_2
         endif

         ! Lecture des valeurs du coefficient:
         if ( dim_coeff_2 == 1 ) then
            call MSP_acc_get_tab (acc,trim(nom_struc),coef%coef_1d,unit="")
         else if ( dim_coeff_2 == 2 ) then
            call MSP_acc_get_tab (acc,trim(nom_struc),coef%coef_2d,unit="")
         else
            call MSP_acc_get_tab (acc,trim(nom_struc),coef%coef_3d,unit="")
         endif
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='de '//trim(nom_struc), &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif

         ! Sortie de la structure du coefficient:
         ier = acc_select_end (acc)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='de la structure '//trim(nom_struc), &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif

      
         ! Lecture des differents parametres du tableau de coef
         
         do i = 1 , coef%dim_coef
            
            select case (coef%com_par_coef(i)(:len_trim(coef%com_par_coef(i))))
            case ("INCIDENCE")
               unite = "rad"
            case ("ALTITUDE")
               unite = "m"
            case ("MACH")
               unite = ""
            case ("REYNOLDS")
               unite = ""
            case ("DERAPAGE")
               unite = "rad"
            case default
               unite = ""
            end select

            select case (i)
            case (1)
               call MSP_acc_get_tab (acc,trim(coef%nom_par_coef(i)),&
                    coef%par1_coef,unit=unite)
            case (2)
               call MSP_acc_get_tab (acc,trim(coef%nom_par_coef(i)),&
                    coef%par2_coef,unit=unite)
            case (3)
               call MSP_acc_get_tab (acc,trim(coef%nom_par_coef(i)),&
                    coef%par3_coef,unit=unite)
               
            end select

         enddo
         ! Verification de la coherence des tableaux
         call coherence_coef(coef)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", &
                 partie_variable='controle coherence', &
                 routine="MSP_lire_str_coef",type=MSP_ENUM_ERREUR)
            if (associated(c_1d)) deallocate(c_1d)
            if (associated(par1_c)) deallocate(par1_c)
            return
         endif
      else
         coef=MSP_creer_coef(dim_coef=dim_c,par1_coef=par1_c,coef_1d=c_1d,&
              nom_coef=nom_struc)
      end if

      if (associated(c_1d)) deallocate(c_1d)
      if (associated(par1_c)) deallocate(par1_c)

   return
 end subroutine MSP_lire_str_coef



   subroutine coherence_coef(coef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  coherence_coef
!
!$Resume
!	Routine permettant de verifier la coherence des tableaux de la structure coef
!
!$Description
!	Routine permettant de verifier la coherence des tableaux de la structure coef
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PRIVE
!
!$Usage
!  call coherence_coef(coef)
!.    type (MSP_COEF) :: coef
!
!$Arguments
!>E     coef  :<MSP_COEF>   struture coef a analyser
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
    type (MSP_COEF),intent(in)::coef

     ! Variables locales
     character(len=250),dimension(2)::tab_val

      ! Corps de la fonction
     tab_val(1) = trim(coef%nom_coef)
     tab_val(2) = ""
     select case (coef%dim_coef)
     case (1)
        if (size(coef%par1_coef) /= size(coef%coef_1d))then
           tab_val(2) = "dim 1 (1D)"
           if (associated(coef%nom_par_coef)) tab_val(2) = trim(coef%nom_par_coef(1))
           call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                routine="coherence_coef",partie_variable=tab_val,&
                type=MSP_ENUM_ERREUR)   
        end if
     case (2)
        if (size(coef%par1_coef) /= size(coef%coef_2d,dim=1))then
           tab_val(2) = "dim 1 (2D)"
           if (associated(coef%nom_par_coef)) tab_val(2) = trim(coef%nom_par_coef(1))
           call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                routine="coherence_coef",partie_variable=tab_val,&
                type=MSP_ENUM_ERREUR)   
        end if
        if (size(coef%par2_coef) /= size(coef%coef_2d,dim=2))then
           tab_val(2) = "dim 2 (2D)"
           if (associated(coef%nom_par_coef)) tab_val(2) = trim(coef%nom_par_coef(2))
           call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                routine="coherence_coef",partie_variable=tab_val,&
                type=MSP_ENUM_ERREUR)   
        end if
     case (3)
         if (size(coef%par1_coef) /= size(coef%coef_3d,dim=1))then
            tab_val(2) = "dim 1 (3D)"
            if (associated(coef%nom_par_coef)) &
                 tab_val(2) = trim(coef%nom_par_coef(1))
            call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                 routine="coherence_coef",partie_variable=tab_val,&
                 type=MSP_ENUM_ERREUR)   
         end if
         if (size(coef%par2_coef) /= size(coef%coef_3d,dim=2))then
            tab_val(2) = "dim 2 (3D)"
            if (associated(coef%nom_par_coef)) &
                 tab_val(2) = trim(coef%nom_par_coef(2))
            call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                 routine="coherence_coef",partie_variable=tab_val,&
                 type=MSP_ENUM_ERREUR)   
         end if
         if (size(coef%par3_coef) /= size(coef%coef_3d,dim=3))then
            tab_val(2) = "dim 3 (3D)"
            if (associated(coef%nom_par_coef)) &
                 tab_val(2) = trim(coef%nom_par_coef(3))
            call MSP_signaler_message (cle_mes="MSP_creer_coef_002",&
                 routine="coherence_coef",partie_variable=tab_val,&
                 type=MSP_ENUM_ERREUR)   
         end if
      end select
    end subroutine coherence_coef



   SUBROUTINE MSP_consulter_coef (coef,dim_coef,coef_1d,coef_2d,coef_3d,par1_coef,&
        par2_coef,par3_coef,com_par_coef,nom_coef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_coef
!
!$Resume
!	Routine permettant de recuperer des information de la struture coef
!
!$Description
!	Routine permettant de recuperer des information de la struture coef
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_coef (coef,[dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],&
!.            [par2_coef],[par3_coef],[com_par_coef],[nom_coef])
!.    type(MSP_COEF) :: coef
!.    integer :: dim_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    character(len=*) :: nom_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!
!$Arguments
!>E     coef          :<MSP_COEF>                      Structure coef
!>[S]   dim_coef      :<integer>                       dimension du tableau de coef
!>[E/S] coef_1d       :<pm_reel,DIM=(:),pointer>       cas d'un tableau a 1 dimension
!>[E/S] coef_2d       :<pm_reel,DIM=(:,:),pointer>     cas d'un tableau a 2 dimensions
!>[E/S] coef_3d       :<pm_reel,DIM=(:,:,:),pointer>   cas d'un tableau a 3 dimensions
!>[E/S] par1_coef     :<pm_reel,DIM=(:),pointer>       parametre 1 definissant le taleau de coef
!>[E/S] par2_coef     :<pm_reel,DIM=(:),pointer>       parametre 2 definissant le taleau de coef
!>[E/S] par3_coef     :<pm_reel,DIM=(:),pointer>       parametre 3 definissant le taleau de coef
!>[E/S] com_par_coef  :<LEN=*,DIM=(:),pointer>         tableau contant la definition des paramtres du tableau de coef
!>[S]   nom_coef      :<LEN=*>                         nom du coef
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
      type(MSP_COEF), intent(IN)                                :: coef
      integer, intent(OUT), optional                            :: dim_coef
      character(LEN=*), pointer , optional, dimension(:)        :: com_par_coef
      character(len=*),intent(out),optional                     :: nom_coef
      real (KIND=pm_reel), pointer , optional, dimension(:)     :: coef_1d
      real (KIND=pm_reel), pointer , optional, dimension(:,:)   :: coef_2d
      real (KIND=pm_reel), pointer , optional, dimension(:,:,:) :: coef_3d
      real (KIND=pm_reel), pointer , optional, dimension(:)     :: par1_coef,par2_coef,par3_coef

      ! Variables locales
      integer :: MSP_iostat
      
      ! Corps de la fonction
      
      MSP_iostat = 0

      if ( present(nom_coef)) nom_coef = coef%nom_coef

      if ( present(dim_coef) ) dim_coef = coef%dim_coef
 
      if ( present(coef_1d) ) then
         if (ASSOCIATED(coef%coef_1d)) then
            if (ASSOCIATED(coef_1d)) then 
               if ( size(coef_1d) /= size(coef%coef_1d) ) then 
                  DEALLOCATE(coef_1d,stat=MSP_iostat)
                  ALLOCATE(coef_1d(size(coef%coef_1d)))
               end if
            else
               ALLOCATE(coef_1d(size(coef%coef_1d)))
            end if
            coef_1d = coef%coef_1d
         endif
      endif


      ! Tableaux de coefficients de dimensions 2:

      if ( present(coef_2d) ) then
         if (ASSOCIATED(coef%coef_2d)) then
            if (ASSOCIATED(coef_2d)) then
               if ( ( size(coef_2d,dim=1) /= size(coef%coef_2d,dim=1) ).and. &
                    ( size(coef_2d,dim=2) /= size(coef%coef_2d,dim=2) ) ) then 
                  DEALLOCATE(coef_2d,stat=MSP_iostat)
                  ALLOCATE(coef_2d(size(coef%coef_2d,dim=1),size(coef%coef_2d,dim=2)))
               end if
            else
               ALLOCATE(coef_2d(size(coef%coef_2d,dim=1),size(coef%coef_2d,dim=2)))
            end if
            coef_2d = coef%coef_2d
         endif
      end if
      
      ! Tableaux de coefficients de dimensions 3:
      
      if ( present(coef_3d) ) then
         if (ASSOCIATED(coef%coef_3d)) then
            if (ASSOCIATED(coef_3d)) then
               if ( ( size(coef_3d,dim=1) /= size(coef%coef_3d,dim=1) ).and. &
                    ( size(coef_3d,dim=2) /= size(coef%coef_3d,dim=2) ).and. &
                    ( size(coef_3d,dim=3) /= size(coef%coef_3d,dim=3) ) ) then 
                  DEALLOCATE(coef_3d,stat=MSP_iostat)
                  ALLOCATE(coef_3d(size(coef%coef_3d,dim=1),size(coef%coef_3d,dim=2),size(coef%coef_3d,dim=3)))
               end if
            else
               ALLOCATE(coef_3d(size(coef%coef_3d,dim=1),size(coef%coef_3d,dim=2),size(coef%coef_3d,dim=3)))
            end if
            coef_3d = coef%coef_3d   
         endif
      end if
      
       ! Abcisses de la première dimension:
      
      if ( present(par1_coef) ) then
         if (ASSOCIATED(coef%par1_coef)) then
            if (ASSOCIATED(par1_coef)) then
               if ( size(par1_coef) /= size(coef%par1_coef) ) then 
                  DEALLOCATE(par1_coef,stat=MSP_iostat)
                  ALLOCATE(par1_coef(size(coef%par1_coef)))
               end if
            else
               ALLOCATE(par1_coef(size(coef%par1_coef)))
            endif
            par1_coef = coef%par1_coef
         end if
      endif
      
     
      ! Abcisses de la seconde dimension:
      
      if ( present(par2_coef) ) then
         if (ASSOCIATED(coef%par2_coef)) then
            if (ASSOCIATED(par2_coef)) then
               if ( size(par2_coef) /= size(coef%par2_coef) ) then 
                  DEALLOCATE(par2_coef,stat=MSP_iostat)
                  ALLOCATE(par2_coef(size(coef%par2_coef)))
               end if
            else
               ALLOCATE(par2_coef(size(coef%par2_coef)))
            end if
            par2_coef = coef%par2_coef
         endif
      endif
     
      ! Abcisses de la troisième dimension:
      
      if ( present(par3_coef) ) then
         if (ASSOCIATED(coef%par3_coef)) then
            if (ASSOCIATED(par3_coef)) then
               if ( size(par3_coef) /= size(coef%par3_coef) ) then 
                  DEALLOCATE(par3_coef,stat=MSP_iostat)
                  ALLOCATE(par3_coef(size(coef%par3_coef)))
               end if
            else
               ALLOCATE(par3_coef(size(coef%par3_coef)))
            end if
            par3_coef = coef%par3_coef
         endif
      endif
      
 
      ! Tableaux donnant la signification physique des abcisses:
      
      if ( present(com_par_coef) ) then
         if (ASSOCIATED(coef%com_par_coef)) then
            if (ASSOCIATED(com_par_coef)) then
               if ( size(com_par_coef) /= size(coef%com_par_coef) ) then 
                  DEALLOCATE(com_par_coef,stat=MSP_iostat)
                  ALLOCATE(com_par_coef(size(coef%com_par_coef)))
               end if
            else
               ALLOCATE(com_par_coef(size(coef%com_par_coef)))
            endif
            com_par_coef = coef%com_par_coef
         end if
      endif

    end SUBROUTINE MSP_consulter_coef
    
    
   SUBROUTINE MSP_modifier_coef (coef, dim_coef,coef_1d,coef_2d,coef_3d,par1_coef,&
        par2_coef,par3_coef,com_par_coef,nom_coef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_coef
!
!$Resume
!	Routine permettant de modifier la structure coef
!
!$Description
!	Routine permettant de modifier la structure coef
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_coef (coef, [dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],&
!.            [par2_coef],[par3_coef],[com_par_coef],[nom_coef])
!.    type(MSP_COEF) :: coef
!.    integer :: dim_coef
!.    character(LEN=*) :: nom_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!
!$Arguments
!>E/S   coef          :<MSP_COEF>                      Structure coef
!>[E]   dim_coef      :<integer>                       dimension du tableau de coef
!>[E/S] coef_1d       :<pm_reel,DIM=(:),pointer>       cas d'un tableau a 1 dimension
!>[E/S] coef_2d       :<pm_reel,DIM=(:,:),pointer>     cas d'un tableau a 2 dimensions
!>[E/S] coef_3d       :<pm_reel,DIM=(:,:,:),pointer>   cas d'un tableau a 3 dimensions
!>[E/S] par1_coef     :<pm_reel,DIM=(:),pointer>       parametre 1 definissant le taleau de coef
!>[E/S] par2_coef     :<pm_reel,DIM=(:),pointer>       parametre 2 definissant le taleau de coef
!>[E/S] par3_coef     :<pm_reel,DIM=(:),pointer>       parametre 3 definissant le taleau de coef
!>[E/S] com_par_coef  :<LEN=*,DIM=(:),pointer>         tableau contant la definition des paramtres du tableau de coef
!>[E]   nom_coef      :<LEN=*>                         nom du coef
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- MSP_ACCES
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

     use MSP_ACCES
      implicit none

      ! Parametres
      type(MSP_COEF), intent(INOUT)  :: coef

      integer, intent(IN), optional :: dim_coef
      character(LEN=*), intent(IN), optional :: nom_coef
      character(LEN=*), pointer , optional, dimension(:)       :: com_par_coef

      real (KIND=pm_reel), pointer , optional, dimension(:)     :: coef_1d
      real (KIND=pm_reel), pointer , optional, dimension(:,:)   :: coef_2d
      real (KIND=pm_reel), pointer , optional, dimension(:,:,:) :: coef_3d
      real (KIND=pm_reel), pointer , optional, dimension(:)     :: par1_coef,par2_coef,par3_coef

      ! Variables locales
      integer :: MSP_iostat
      
      ! Corps de la fonction
      
      MSP_iostat = 0

      if ( present(nom_coef) ) coef%nom_coef = nom_coef

      if ( present(dim_coef) ) coef%dim_coef = dim_coef
 
      if ( present(coef_1d) ) then
         if (ASSOCIATED(coef_1d)) then
            if (ASSOCIATED(coef%coef_1d)) then
               if ( size(coef_1d) /= size(coef%coef_1d) ) then 
                  DEALLOCATE(coef%coef_1d,stat=MSP_iostat)
                  ALLOCATE(coef%coef_1d(size(coef_1d)))
               end if
            else
               ALLOCATE(coef%coef_1d(size(coef_1d)))
            end if
            coef%coef_1d = coef_1d
            coef%dim_coef = 1
         endif
      endif

     !  Tableaux de coefficients de dimensions 2:

      if ( present(coef_2d) ) then
         if (ASSOCIATED(coef_2d)) then
            if (ASSOCIATED(coef%coef_2d)) then
               if ( ( size(coef_2d,dim=1) /= size(coef%coef_2d,dim=1) ).or. &
                    ( size(coef_2d,dim=2) /= size(coef%coef_2d,dim=2) ) ) then 
                  DEALLOCATE(coef%coef_2d,stat=MSP_iostat)
                  ALLOCATE(coef%coef_2d(size(coef_2d,dim=1),size(coef_2d,dim=2)))
               end if
            else
               ALLOCATE(coef%coef_2d(size(coef_2d,dim=1),size(coef_2d,dim=2)))
            end if
            coef%coef_2d = coef_2d
            coef%dim_coef = 2
         endif
       end if



     !   Tableaux de coefficients de dimensions 3:
      
      if ( present(coef_3d) ) then
         if (ASSOCIATED(coef_3d)) then
            if (ASSOCIATED(coef%coef_3d)) then
               if ( ( size(coef_3d,dim=1) /= size(coef%coef_3d,dim=1) ).or. &
                    ( size(coef_3d,dim=2) /= size(coef%coef_3d,dim=2) ).or. &
                    ( size(coef_3d,dim=3) /= size(coef%coef_3d,dim=3) ) ) then 
                  DEALLOCATE(coef%coef_3d,stat=MSP_iostat)
                  ALLOCATE(coef%coef_3d(size(coef_3d,dim=1),size(coef_3d,dim=2),size(coef_3d,dim=3)))
               end if
            else
               ALLOCATE(coef%coef_3d(size(coef_3d,dim=1),size(coef_3d,dim=2),size(coef_3d,dim=3)))
            end if
            coef%coef_3d = coef_3d
            coef%dim_coef = 3
         endif
      end if

      ! Abcisses de la première dimension:
      
      if ( present(par1_coef) ) then
         if (ASSOCIATED(par1_coef)) then
            if (ASSOCIATED(coef%par1_coef)) then
               if ( size(par1_coef) /= size(coef%par1_coef) ) then 
                  DEALLOCATE(coef%par1_coef,stat=MSP_iostat)
                  ALLOCATE(coef%par1_coef(size(par1_coef)))
               end if
            else
               ALLOCATE(coef%par1_coef(size(par1_coef)))
            end if
            coef%par1_coef = par1_coef
         end if
      endif
      
      
      ! Abcisses de la seconde dimension:
      
      if ( present(par2_coef) ) then
         if (ASSOCIATED(par2_coef)) then
            if (ASSOCIATED(coef%par2_coef)) then
               if ( size(par2_coef) /= size(coef%par2_coef) ) then 
                  DEALLOCATE(coef%par2_coef,stat=MSP_iostat)
                  ALLOCATE(coef%par2_coef(size(par2_coef)))
               end if
            else
               ALLOCATE(coef%par2_coef(size(par2_coef)))
            end if
            coef%par2_coef = par2_coef
         end if
      endif
      
      ! Abcisses de la troisième dimension:
      
      if ( present(par3_coef) ) then
         if (ASSOCIATED(par3_coef)) then
            if (ASSOCIATED(coef%par3_coef)) then
               if ( size(par3_coef) /= size(coef%par3_coef) ) then 
                  DEALLOCATE(coef%par3_coef,stat=MSP_iostat)
                  ALLOCATE(coef%par3_coef(size(par3_coef)))
               end if
            else
               ALLOCATE(coef%par3_coef(size(par3_coef)))
            end if
            coef%par3_coef = par3_coef
         end if
      endif

      ! Tableaux donnant la signification physique des abcisses:
      
      if ( present(com_par_coef) ) then
         if (ASSOCIATED(com_par_coef)) then
            if (ASSOCIATED(coef%com_par_coef)) then
               if ( size(com_par_coef) /= size(coef%com_par_coef) ) then 
                  DEALLOCATE(coef%com_par_coef,stat=MSP_iostat)
                  ALLOCATE(coef%com_par_coef(size(com_par_coef)))
               end if
            else
               ALLOCATE(coef%com_par_coef(size(com_par_coef)))
            end if
            coef%com_par_coef = com_par_coef
         end if
      endif
      

    end SUBROUTINE MSP_modifier_coef


    function MSP_creer_coef  (dim_coef,coef_1d,coef_2d,coef_3d,par1_coef,par2_coef,&
         par3_coef,com_par_coef,nom_coef) result (coef)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_coef
!
!$Resume
!	Routine permettant de definir une structure COEF
!
!$Description
!	Routine permettant de definir une structure COEF
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  coef = MSP_creer_coef  ([dim_coef],[coef_1d],[coef_2d],[coef_3d],[par1_coef],[par2_coef],&
!.             [par3_coef],[com_par_coef],[nom_coef])
!.    integer :: dim_coef
!.    character(LEN=*) :: nom_coef
!.    character(LEN=*), pointer , dimension(:) :: com_par_coef
!.    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
!.    real (KIND=pm_reel), pointer , dimension(:,:) :: coef_2d
!.    real (KIND=pm_reel), pointer , dimension(:,:,:) :: coef_3d
!.    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef,par2_coef,par3_coef
!.    type(MSP_COEF) :: coef
!
!$Arguments
!>[E]   dim_coef      :<integer>                       dimension du tableau de coef
!>[E/S] coef_1d       :<pm_reel,DIM=(:),pointer>       cas d'un tableau a 1 dimension
!>[E/S] coef_2d       :<pm_reel,DIM=(:,:),pointer>     cas d'un tableau a 2 dimensions
!>[E/S] coef_3d       :<pm_reel,DIM=(:,:,:),pointer>   cas d'un tableau a 3 dimensions
!>[E/S] par1_coef     :<pm_reel,DIM=(:),pointer>       parametre 1 definissant le taleau de coef
!>[E/S] par2_coef     :<pm_reel,DIM=(:),pointer>       parametre 2 definissant le taleau de coef
!>[E/S] par3_coef     :<pm_reel,DIM=(:),pointer>       parametre 3 definissant le taleau de coef
!>[E/S] com_par_coef  :<LEN=*,DIM=(:),pointer>         tableau contant la definition des paramtres du tableau de coef
!>[E]   nom_coef      :<LEN=*>                         nom du coef 
!>S     coef          :<MSP_COEF>                      structure COEF
!
!$Common
!
!$Routines
!- MSP_effacer_coef
!- MSP_signaler_message
!#V
!- coherence_coef
!#
!
!$Include
!
!$Module
!#V
!- MSP_ACCES
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

     use MSP_ACCES
      implicit none

      ! Parametres
      !===========
      integer, intent(IN), optional :: dim_coef
      character(LEN=*), intent(IN), optional :: nom_coef
      character(LEN=*), pointer , optional, dimension(:)       :: com_par_coef

      real (KIND=pm_reel), pointer , optional, dimension(:)     :: coef_1d
      real (KIND=pm_reel), pointer , optional, dimension(:,:)   :: coef_2d
      real (KIND=pm_reel), pointer , optional, dimension(:,:,:) :: coef_3d
      real (KIND=pm_reel), pointer , optional, dimension(:)     :: par1_coef,par2_coef,par3_coef

      ! Resultats
      !==========
      type(MSP_COEF)  :: coef
      
      ! Variables locales
      logical :: exist_coef

      exist_coef = .false.

      ! Corps de la fonction
      !=====================

      ! On efface en mettant les pointeurs à NUL
      ! -> ceci permet une initialisation propre de la structure
      ! (cf FA-ID 818 et FA-ID 823)
      call MSP_effacer_coef(coef, nul=.true.)

      ! creation a partir d'une fonction
      coef%flag_func = .true.

      if ( present(dim_coef) ) coef%dim_coef = dim_coef
 
      if ( present(nom_coef)) then
         coef%nom_coef = nom_coef
      else
         coef%nom_coef = ""
      end if

      if ( present(coef_1d) ) then
         if (ASSOCIATED(coef_1d)) then
            ALLOCATE(coef%coef_1d(size(coef_1d)))
            coef%coef_1d = coef_1d
            coef%dim_coef = 1
            exist_coef = .true.
         endif
      endif

      ! Tableaux de coefficients de dimensions 2:

      if ( present(coef_2d) ) then
         if (ASSOCIATED(coef_2d)) then
            ALLOCATE(coef%coef_2d(size(coef_2d,dim=1),size(coef_2d,dim=2)))
            coef%coef_2d = coef_2d
            coef%dim_coef = 2
            exist_coef = .true.
         endif
      end if

        ! Tableaux de coefficients de dimensions 3:
      
      if ( present(coef_3d) ) then
         if (ASSOCIATED(coef_3d)) then
            ALLOCATE(coef%coef_3d(size(coef_3d,dim=1),size(coef_3d,dim=2),size(coef_3d,dim=3)))
            coef%coef_3d = coef_3d
            coef%dim_coef = 3
            exist_coef = .true.
         endif
      end if
      
      ! Abcisses de la première dimension:
      
      if ( present(par1_coef) ) then
         if (ASSOCIATED(par1_coef)) then
            ALLOCATE(coef%par1_coef(size(par1_coef)))
            coef%par1_coef = par1_coef
         endif
      end if
      
      ! Abcisses de la seconde dimension:
      
      if ( present(par2_coef) ) then
         if (ASSOCIATED(par2_coef)) then
            ALLOCATE(coef%par2_coef(size(par2_coef)))
            coef%par2_coef = par2_coef
         endif
      end if

      ! Abcisses de la troisième dimension:
      
      if ( present(par3_coef) ) then
         if (ASSOCIATED(par3_coef)) then
            ALLOCATE(coef%par3_coef(size(par3_coef)))
            coef%par3_coef = par3_coef
         endif
      end if

      ! Tableaux donnant la signification physique des abcisses:
      
      if ( present(com_par_coef) ) then
         if (ASSOCIATED(com_par_coef)) then
            ALLOCATE(coef%com_par_coef(size(com_par_coef)))
            coef%com_par_coef = com_par_coef
         endif
      end if



      if (exist_coef) then

         ! Verification de la coherence

         call coherence_coef(coef)
         if (MSP_PROBLEME) then
            call MSP_signaler_message (cle_mes="MSP_creer_coef_001", &
                 type=MSP_ENUM_ERREUR)   
            return
         end if
      else
         ! Creation d'un coef par defaut
         allocate(coef%par1_coef(1),coef%coef_1d(1))
         coef%par1_coef(1) = 0._PM_REEL
         coef%coef_1d(1) = 0._PM_REEL
         coef%dim_coef = 1
         exist_coef = .true.
      end if



    end function MSP_creer_coef
    
    

     subroutine MSP_afficher_coef (coef,ilog)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_coef
!
!$Resume
!	Routine permettant d'afficher le contenu de la structure coef
!
!$Description
!	Routine permettant d'afficher le contenu de la structure coef
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_coef (coef,[ilog])
!.    type(MSP_COEF) :: coef
!.    integer :: ilog
!
!$Arguments
!>E     coef  :<MSP_COEF>   Structure coef
!>[E]   ilog  :<integer>    numero d'unite logique (0, ou 6) ecran
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
      type(MSP_COEF), intent(IN) :: coef
      integer, intent(IN), optional :: ilog

      ! Variables locales
      integer :: num, m, ii, jj

      ! Corps de la fonction
      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif

      write(num,'(a,a)')   "NOM COEF :    ", trim(coef%nom_coef)
      select case (coef%dim_coef)
      case (1)
         write(num,'(a,i9)') "DIM X   :    ",size(coef%coef_1d)
         m = min(4,size(coef%coef_1d))
         write(num,'(a,4(g21.12))') "C(X)    :      ",coef%coef_1d(1:m)
         if (size(coef%coef_1d) .gt. 4) then
            write(num,'(4(g21.12))') coef%coef_1d(5:size(coef%coef_1d))
         endif
         write(num,'(a,4(g21.12))') "X       :    ",coef%par1_coef(1:m)
         if (size(coef%coef_1d) .gt. 4) then
            write(num,'(4(g21.12))') coef%par1_coef(5:size(coef%coef_1d))
         endif

      case (2)
         write(num,'(a,i9)') "DIM X     :    ",size(coef%coef_2d,dim=1)
         write(num,'(a,i9)') "DIM Y     :    ",size(coef%coef_2d,dim=2)
         m = min(4,size(coef%coef_2d,dim=2))
         write(num,'(a)') "C(X,Y)    :"
         do ii=1,size(coef%coef_2d,dim=1)
            write(num,'(4(g21.12))') coef%coef_2d(ii,1:m)
            if (size(coef%coef_2d,dim=2).gt.4) then
               write(num,'(4(g21.12))') coef%coef_2d(ii,5:size(coef%coef_2d,dim=2))
            endif
         end do
         m = min(4,size(coef%coef_2d,dim=1))
         write(num,'(a,4(g21.12))') "X         :    ",coef%par1_coef(1:m)
         if (size(coef%coef_2d,dim=1) .gt. 4) then
            write(num,'(4(g21.12))') coef%par1_coef(5:size(coef%coef_2d,dim=1))
         endif
         m = min(4,size(coef%coef_2d,dim=2))
         write(num,'(a,4(g21.12))') "Y         :    ",coef%par2_coef(1:m)
         if (size(coef%coef_2d,dim=2) .gt. 4) then
            write(num,'(4(g21.12))') coef%par2_coef(5:size(coef%coef_2d,dim=2))
         endif

      case (3)
         write(num,'(a,i9)') "DIM X       :    ",size(coef%coef_3d,dim=1)
         write(num,'(a,i9)') "DIM Y       :    ",size(coef%coef_3d,dim=2)
         write(num,'(a,i9)') "DIM Z       :    ",size(coef%coef_3d,dim=3)
         m = min(4,size(coef%coef_3d,dim=3))
         write(num,'(a)') "C(X,Y)    :"
         do ii=1,size(coef%coef_3d,dim=1)
            do jj=1,size(coef%coef_3d,dim=2)
               write(num,'(4(g21.12))') coef%coef_3d(ii,jj,1:m)
               if (size(coef%coef_3d,dim=3).gt.4) then
                  write(num,'(4(g21.12))') coef%coef_3d(ii,jj,5:size(coef%coef_3d,dim=3))
               endif
            end do
         end do
         m = min(4,size(coef%coef_3d,dim=1))
         write(num,'(a,4(g21.12))') "X           :    ",coef%par1_coef(1:m)
         if (size(coef%coef_3d,dim=1) .gt. 4) then
            write(num,'(4(g21.12))') coef%par1_coef(5:size(coef%coef_3d,dim=1))
         endif
         m = min(4,size(coef%coef_3d,dim=2))
         write(num,'(a,4(g21.12))') "Y           :    ",coef%par2_coef(1:m)
         if (size(coef%coef_3d,dim=1) .gt. 4) then
            write(num,'(4(g21.12))') coef%par2_coef(5:size(coef%coef_3d,dim=2))
         endif
         m = min(4,size(coef%coef_3d,dim=3))
         write(num,'(a,4(g21.12))') "Z           :    ",coef%par3_coef(1:m)
         if (size(coef%coef_3d,dim=3) .gt. 4) then
            write(num,'(4(g21.12))') coef%par3_coef(5:size(coef%coef_3d,dim=3))
         endif
         
      end select
      
      write(num,'(a)') ''

    end subroutine MSP_afficher_coef


  function MSP_interp_coef(coef,tab_valcoef,indcoef_i, indcoef_j,indcoef_k) result (coeff)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_coef
!
!$Resume
!	Routine permettant d'interpoler lineairement dans la structure coef
!
!$Description
!	Routine permettant d'interpoler lineairement dans la structure coef
!       Cette routine utilise l'interpolation lineaire de la MSPRO laquelle
!       utilise la fonction de recherche d'indice mu_inter_ind qui recherche 
!       l'indice de la valeur du tableau des indices le plus proche 
!       precedent la valeur  souhaitee.
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  coeff = MSP_interp_coef(coef,tab_valcoef,[indcoef_i], [indcoef_j],[indcoef_k])
!.    type(MSP_COEF) :: COEF
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcoef
!.    real(kind=PM_REEL) :: coeff
!.    integer :: indcoef_i,indcoef_j,indcoef_k
!
!$Arguments
!>E     coef             :<MSP_COEF>                  struture coef
!>E/S   tab_valcoef      :<PM_REEL,DIM=(:),pointer>   valeur a interpoler (tableau de 1, 2 ou 3 valeurs)
!>[E/S] indcoef_i        :<integer>                   indice initial du premier parametre
!>[E/S] indcoef_j        :<integer>                   indice initial du second parametre
!>[E/S] indcoef_k        :<integer>                   indice initial du troisieme parametre
!>S     coeff            :<PM_REEL>                   valeur interpolee
!
!$Common
!
!$Routines
!- mu_inter_dim1_lin
!- mu_inter_dim2_deg2
!- mu_inter_dim3_deg3
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
! Les valeurs de indcoef_i, indcoef_j, indcoef_k peuvent etre recuperees et reinjectes pour les
! appels suivants.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_COEF),intent(in)                 :: COEF
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcoef
    real(kind=PM_REEL)                        :: coeff
    integer,optional,intent(inout)            :: indcoef_i,indcoef_j,indcoef_k

    ! Variables locales
    integer  :: sz1, sz2, sz3
    integer, save  :: indc_i = 1
    integer, save  :: indc_j = 1
    integer, save  :: indc_k = 1
    type(tm_code_retour) :: code_retour

    ! Début de la routine
    if (present(indcoef_i)) then
       indc_i = indcoef_i
    endif
    if (present(indcoef_j)) then 
       indc_j = indcoef_j
    endif
    if (present(indcoef_k)) then
       indc_k = indcoef_k
    endif
    sz1=size(coef%par1_coef)
    sz2=size(coef%par2_coef)
    sz3=size(coef%par3_coef)


    ! Interpolation différentes en fonction  
    select case (coef%dim_coef)

    case (1)
       ! interpolation du cx avion ou aéro
!       coeff=interpol_lin1(coef%coef_1d,coef%par1_coef,tab_valcoef(1),indcoef_i,irecherchecoefi)
       call mu_inter_dim1_lin(sz1, coef%par1_coef, coef%coef_1d, tab_valcoef(1), &
            indc_i,coeff, code_retour)
       if (present(indcoef_i)) indcoef_i = indc_i

     case (2)

       ! interpolation cx avion ou coef
!       coeff = interpol_lin2(coef%coef_2d,coef%par1_coef,coef%par2_coef,&
!            tab_valcoef(1),tab_valcoef(2),indcoef_i,irecherchecoefi,indcoef_j,irecherchecoefj)
       call mu_inter_dim2_deg2(sz1, coef%par1_coef, sz2, coef%par2_coef, coef%coef_2d,&
            tab_valcoef(1), tab_valcoef(2), indc_i, indc_j, coeff, code_retour)

       if (present(indcoef_i)) indcoef_i = indc_i
       if (present(indcoef_j)) indcoef_j = indc_j
    case(3)
       
      ! interpolation cx avion ou coef
!      coeff = interpol_lin3(coef%coef_3d,coef%par1_coef,coef%par2_coef,coef%par3_coef,&
!            tab_valcoef(1),tab_valcoef(2),tab_valcoef(3),indcoef_i,irecherchecoefi,&
!            indcoef_j,irecherchecoefj,indcoef_k,irecherchecoefk)

       call mu_inter_dim3_deg3(sz1, coef%par1_coef, sz2, coef%par2_coef, sz3, coef%par3_coef, & 
            coef%coef_3d, tab_valcoef(1), tab_valcoef(2), tab_valcoef(3), indc_i, indc_j, indc_k, &
            coeff, code_retour)

       if (present(indcoef_i)) indcoef_i = indc_i
       if (present(indcoef_j)) indcoef_j = indc_j
       if (present(indcoef_k)) indcoef_k = indc_k
    end select

    if (code_retour%valeur < 0) call MSP_signaler_message (ier_mslib=code_retour)
    if (MSP_gen_messages("MSP_interp_coef")) return

  end function MSP_interp_coef

  end module MSP_COEF_DEF
