module MSP_MCI_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_MCI_DEF
!
!$Resume
!  Module gérant les caractéristiques MCI des véhicules.
!
!$Description
!  Module gérant les caractéristiques MCI des véhicules.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_MCI_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_MCI_DEF.F90,v $
!  Revision 1.17  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.16  2009/09/04 12:58:23  tanguyy
!  DM-ID 1113 : Fermeture des accÃ©s MADONA une fois le fichier ouvert
!
!  Revision 1.15  2008/11/12 10:26:22  mercadig
!  DM-ID 733: Implementation du calcul de la surface d application utile pour le calcul de force
!
!  Revision 1.14  2008/08/08 14:50:01  gss
!  DM-ID 1058 : (portage g95) ajout de la gestion d'erreur à la fonction
!  acc_charger_donnees.
!  Revision 1.13  2008/04/24 13:59:03  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.12  2006/06/02 11:21:54  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.11  2005/03/08 07:32:35  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.10  2004/11/05 16:27:04  vivaresf
!  coquilles
!  Revision 1.9  2004/10/25 10:15:02  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.8  2004/10/12 13:50:31  vivaresf
!  Version 1-4
!  Revision 1.7  2003/07/10 14:01:38  adm_ipsi
!  FA-ID 14 : Utilisation de la fonction MSP_acc_get_val
!  Revision 1.1  2003/03/27 16:44:08  util_am
!  SLB - Version industrialisée
!  Revision 1.6  2003/02/25 11:09:07  adm_ipsi
!   MSP_afficher_mic, ajout des coef Sp.
!  Revision 1.5  2002/12/11 10:18:10  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.4  2002/12/04 18:08:24  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:02  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/13 15:17:17  adm_ipsi
!  Création de l'interface MSP_display_mci
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.7  2000/07/04 14:02:22  util_am
!  Pour chaque recopie de tableau dans MSP_modifier_aero, MSP_consulter_aero,
!  un test est mis en place sur l'allocation des tableaux et sur la taille des
!  deux tableaux à égaliser.
!  Revision 1.6  2000/06/19 13:28:52  util_am
!  Ajout des routines MSP_consulter_aero et MSP_modifier_aero
!  Revision 1.5  2000/06/15 09:00:40  util_am
!  - Ajout du champ flag_func dans la structure MSP_AERO pour la gestion des fuites mémoires
!  - Privatisation du contenu des structures MSP_MCI, MSP_AERO, MSP_PRAD, MSP_VEHICULE
!  - Ajout des routines MSP_afficher_vehicule,
!          MSP_consulter_prad, MSP_modifier_prad,
!  - Renommage des routines MSP_creer_cf_aero en MSP_creer_aero
!  - Ajout d'interfaces anglaises aux routines et fonctions publiques
!  - Mise à jour des cartouches
!  Revision 1.4  2000/02/10 16:27:34  rousseau
!  definition des operateurs = des differentes srtuctures
!  definition des routine affecter ( = + supprimer B)
!  Revision 1.3  1999/10/25 15:28:36  util_am
!  Ajout des surfaces de panneaux solaires
!  Revision 1.2  1999/09/22 15:26:18  util_am
!  Ajout des routines : MSP_consulter_vehicule, MSP_modifier_vehicule, MSP_consulter_mci, MSP_modifier_mci
!  Mise a jour des cartouches
!  Revision 1.1.1.1  1999/07/13 08:37:57  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_MCI_DEF
!
!$Structure
!
!: MSP_MCI : 
!#V
!>     forme    : <integer,private>              forme du véhicule                   
!>     sx       : <pm_reel,private>              surface perpendiculaire à l'axe X du véhicule [m^2]                  
!>     sy       : <pm_reel,private>              surface perpendiculaire à l'axe Y du véhicule [m^2]                  
!>     sz       : <pm_reel,private>              surface perpendiculaire à l'axe Z du véhicule [m^2]                   
!>     st       : <pm_reel,private>              surface transverse de révolution du véhicule [m^2]                   
!>     spx      : <pm_reel,private>              surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2] 
!>     spy      : <pm_reel,private>              surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2] 
!>     spz      : <pm_reel,private>              surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2] 
!>     centrx   : <pm_reel,private>              centrage en X (%lref)                   
!>     centry   : <pm_reel,private>              centrage en Y (%lref)                   
!>     centrz   : <pm_reel,private>              centrage en Z (%lref)                   
!>     ixx      : <pm_reel,private>              inertie principale en X [kg.m^2]                   
!>     iyy      : <pm_reel,private>              inertie principale en Y [kg.m^2]                   
!>     izz      : <pm_reel,private>              inertie principale en Z [kg.m^2]                   
!>     ixy      : <pm_reel,private>              inertie en XY [kg.m^2]                   
!>     ixz      : <pm_reel,private>              inertie en XZ [kg.m^2]                   
!>     iyz      : <pm_reel,private>              inertie en YZ [kg.m^2]                   
!>     mstr     : <pm_reel,private>              masse sèche [kg]                   
!>     merg     : <pm_reel,private>              masse d'ergols [kg]                   
!>     mtot     : <pm_reel,private>              masse totale [kg]                   
!>     ficmci   : <LEN=MSP_LONG_CHAINE,private>  nom du fichier                   
!#
!
!$Common
!
!$Routines
!- MSP_create_mci
!- MSP_clear_mci
!- MSP_display_mci
!- MSP_get_mci_data
!- MSP_set_mci_data
!- MSP_effacer_mci
!- MSP_lire_str_mci
!- MSP_consulter_mci
!- MSP_modifier_mci
!- MSP_afficher_mci
!- MSP_calcul_surf_app
!#V
!- egaler_mci
!#
!
!$Fonctions
!- MSP_creer_mci
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_MECASPA_DEF
!- MSP_GESTION_ERREUR
!- MSP_ACCES
!#
!
!$Interface
!> msp_create_mci :    MSP_creer_mci
!> msp_display_mci :   MSP_afficher_mci
!> msp_clear_mci :     MSP_effacer_mci
!> assignment :        egaler_mci
!> msp_set_mci_data :  MSP_modifier_mci
!> msp_get_mci_data :  MSP_consulter_mci
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  MASSE, CENTRAGE, INERTIE
!
!$Voir-Aussi
!#V
!.  egaler_mci
!#
!.  MSP_creer_mci MSP_create_mci MSP_clear_mci MSP_display_mci MSP_get_mci_data MSP_set_mci_data
!.  MSP_effacer_mci MSP_lire_str_mci MSP_consulter_mci MSP_modifier_mci MSP_afficher_mci MSP_calcul_surf_app
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB, only : pm_reel
   use MSP_MECASPA_DEF
   use MSP_GESTION_ERREUR
   use MSP_ACCES


   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_MCI_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'



   ! DEFINITIONS DE TYPES:

   type MSP_MCI

      private

      integer :: forme

      real (KIND=pm_reel) :: sx
      real (KIND=pm_reel) :: sy
      real (KIND=pm_reel) :: sz
      real (KIND=pm_reel) :: st
      real (KIND=pm_reel) :: spx
      real (KIND=pm_reel) :: spy
      real (KIND=pm_reel) :: spz

      real (KIND=pm_reel) :: centrx
      real (KIND=pm_reel) :: centry
      real (KIND=pm_reel) :: centrz

      real (KIND=pm_reel) :: ixx
      real (KIND=pm_reel) :: iyy
      real (KIND=pm_reel) :: izz
      real (KIND=pm_reel) :: ixy
      real (KIND=pm_reel) :: ixz
      real (KIND=pm_reel) :: iyz

      real (KIND=pm_reel) :: mstr
      real (KIND=pm_reel) :: merg
      real (KIND=pm_reel) :: mtot

      character(LEN=MSP_LONG_CHAINE) :: ficmci

   end type MSP_MCI

   integer, parameter :: MSP_ENUM_SPHERE        = 1
   integer, parameter :: MSP_ENUM_PLAQUE        = 2
   integer, parameter :: MSP_ENUM_CYLINDRE      = 3
   integer, parameter :: MSP_ENUM_PARALLEPIPEDE = 4


   ! SOUS-PROGRAMMES ET FONCTIONS

   private ::egaler_mci


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
!  mcia=mcib
!.    type(MSP_MCI) :: mcia
!.    type(MSP_MCI) :: mcib
!
!$Procedures
!#V
!- egaler_mci
!#
!
!$Remarques
!
!$Mots-cles
! MCI EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_mci
   end interface


   interface MSP_create_mci

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_mci
!
!$Resume
!  Creates an MCI(Mass, centrage, Inertie) structure
!
!$Description
!  Creates an MCI(Mass, centrage, Inertie) structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  mci = MSP_create_mci ([ficmci],[forme],[sx],[sy],[sz],[st],[spx],[spy],[spz], &
!.                               centrx,centry,centrz,ixx,iyy,izz,ixy,ixz,iyz,mstr,merg)
!.    character(LEN=*) :: ficmci
!.    integer :: forme
!.    real (KIND=pm_reel) :: sx
!.    real (KIND=pm_reel) :: sy
!.    real (KIND=pm_reel) :: sz
!.    real (KIND=pm_reel) :: st
!.    real (KIND=pm_reel) :: spx
!.    real (KIND=pm_reel) :: spy
!.    real (KIND=pm_reel) :: spz
!.    real (KIND=pm_reel) :: centrx
!.    real (KIND=pm_reel) :: centry
!.    real (KIND=pm_reel) :: centrz
!.    real (KIND=pm_reel) :: ixx
!.    real (KIND=pm_reel) :: iyy
!.    real (KIND=pm_reel) :: izz
!.    real (KIND=pm_reel) :: ixy
!.    real (KIND=pm_reel) :: ixz
!.    real (KIND=pm_reel) :: iyz
!.    real (KIND=pm_reel) :: mstr
!.    real (KIND=pm_reel) :: merg
!.    type(MSP_MCI) :: mci
!
!$Procedures
!- MSP_creer_mci
!
!$Remarques
!
!$Mots-cles
! MCI CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_mci
   end interface


   interface MSP_clear_mci

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_mci
!
!$Resume
!  Clears the content of a MCI structure
!
!$Description
!  Clears the content of a MCI structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_mci(mci)
!.    type(MSP_MCI) :: mci
!
!$Procedures
!- MSP_effacer_mci
!
!$Remarques
!
!$Mots-cles
! MCI EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_mci
   end interface



   interface MSP_display_mci

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_mci
!
!$Resume
!  Displays information on a MCI structure
!
!$Description
!  Displays information on a MCI structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_mci (mci,[ilog])
!.    type(MSP_MCI) :: mci
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_mci
!
!$Remarques
!
!$Mots-cles
! MCI AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_mci
   end interface


   interface MSP_get_mci_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_mci_data
!
!$Resume
!  Gets information on MCI characteristics
!
!$Description
!  Gets information on MCI characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_mci_data(mci, [forme], [sx], [sy], [sz], [st], [spx], [spy], [spz],  &
!.            centrx, centry, centrz, ixx, iyy, izz, ixy, ixz, iyz, mstr, merg, mtot)
!.    type(MSP_MCI) :: mci
!.    integer :: forme
!.    real (KIND=PM_REEL) :: sx, sy, sz, st, spx, spy, spz
!.    real (KIND=PM_REEL) :: centrx, centry, centrz
!.    real (KIND=PM_REEL) :: ixx, iyy, izz
!.    real (KIND=PM_REEL) :: ixy, ixz, iyz
!.    real (KIND=PM_REEL) :: mstr, merg, mtot
!
!$Procedures
!- MSP_consulter_mci
!
!$Remarques
!
!$Mots-cles
! MCI CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_mci
   end interface

 
   interface MSP_set_mci_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_mci_data
!
!$Resume
!  Modifies information on MCI characteristics
!
!$Description
!  Modifies information on MCI characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_mci_data(mci, [forme], [sx], [sy], [sz], [st], [spx], [spy], [spz],   &
!.            centrx, centry, centrz, ixx, iyy, izz, ixy, ixz, iyz, mstr, merg)
!.    type(MSP_MCI) :: mci
!.    integer :: forme
!.    real (KIND=PM_REEL) :: sx, sy, sz, st, spx, spy, spz
!.    real (KIND=PM_REEL) :: centrx, centry, centrz
!.    real (KIND=PM_REEL) :: ixx, iyy, izz
!.    real (KIND=PM_REEL) :: ixy, ixz, iyz
!.    real (KIND=PM_REEL) :: mstr, merg
!
!$Procedures
!- MSP_modifier_mci
!
!$Remarques
!
!$Mots-cles
! MCI MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_mci
   end interface

   contains

     subroutine egaler_mci(mcia,mcib)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_mci
!
!$Resume
!	Definition de l'affectation de 2 structures MCI
!
!$Description
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_mci(mcia,mcib)
!.    type(MSP_MCI) :: mcia
!.    type(MSP_MCI) :: mcib
!
!$Arguments
!>E/S   mcia  :<MSP_MCI>   structure à gauche de '"'
!>E     mcib  :<MSP_MCI>   structure à droite de '"'
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
!  MCI EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none

       type(MSP_MCI),intent(inout) :: mcia
       type(MSP_MCI),intent(in) :: mcib

      mcia%forme = mcib%forme

      mcia%sx = mcib%sx
      mcia%sy = mcib%sy
      mcia%sz = mcib%sz
      mcia%st = mcib%st
      mcia%spx = mcib%spx
      mcia%spy = mcib%spy
      mcia%spz = mcib%spz

      mcia%centrx = mcib%centrx
      mcia%centry = mcib%centry
      mcia%centrz = mcib%centrz

      mcia%ixx = mcib%ixx
      mcia%iyy = mcib%iyy
      mcia%izz = mcib%izz
      mcia%ixy = mcib%ixy
      mcia%ixz = mcib%ixz
      mcia%iyz = mcib%iyz

      mcia%mstr = mcib%mstr
      mcia%merg = mcib%merg
      mcia%mtot = mcib%mtot

      mcia%ficmci = mcib%ficmci
    end subroutine egaler_mci

     subroutine MSP_effacer_mci(mci)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_mci
!
!$Resume
!	Routine permettant de desallouer proprement une struture mci
!
!$Description
!	Routine permettant de desallouer proprement une struture mci
!
!$Auteur
!       S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_mci(mci)
!.    type(MSP_MCI) :: mci
!
!$Arguments
!>E/S   mci  :<MSP_MCI>   Structure MCI à effacer
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
!   MCI EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none

       type(MSP_MCI) :: mci


      mci%forme = 0 

      mci%sx = 0._PM_REEL
      mci%sy = 0._PM_REEL
      mci%sz = 0._PM_REEL
      mci%st = 0._PM_REEL
      mci%spx = 0._PM_REEL
      mci%spy = 0._PM_REEL
      mci%spz = 0._PM_REEL

      mci%centrx = 0._PM_REEL
      mci%centry = 0._PM_REEL
      mci%centrz = 0._PM_REEL

      mci%ixx = 0._PM_REEL
      mci%iyy = 0._PM_REEL
      mci%izz = 0._PM_REEL
      mci%ixy = 0._PM_REEL
      mci%ixz = 0._PM_REEL
      mci%iyz = 0._PM_REEL

      mci%mstr = 0._PM_REEL
      mci%merg = 0._PM_REEL
      mci%mtot = 0._PM_REEL

      mci%ficmci = ""
    end subroutine MSP_effacer_mci


   function MSP_creer_mci (ficmci,forme,sx,sy,sz,st,spx,spy,spz, &
                           centrx,centry,centrz,ixx,iyy,izz,ixy,ixz,iyz,mstr,merg) result (mci)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_mci
!
!$Resume
!  Création d'une structure contenant les caractéristiques masse/centrage/inertie d'un véhicule.
!
!$Description
!  Création d'une structure contenant les caractéristiques masse/centrage/inertie d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  mci = MSP_creer_mci ([ficmci],[forme],[sx],[sy],[sz],[st],[spx],[spy],[spz], &
!.                               [centrx],[centry],[centrz],[ixx],[iyy],[izz],[ixy],[ixz],[iyz],[mstr],[merg])
!.    character(LEN=*) :: ficmci
!.    integer :: forme
!.    real (KIND=pm_reel) :: sx
!.    real (KIND=pm_reel) :: sy
!.    real (KIND=pm_reel) :: sz
!.    real (KIND=pm_reel) :: st
!.    real (KIND=pm_reel) :: spx
!.    real (KIND=pm_reel) :: spy
!.    real (KIND=pm_reel) :: spz
!.    real (KIND=pm_reel) :: centrx
!.    real (KIND=pm_reel) :: centry
!.    real (KIND=pm_reel) :: centrz
!.    real (KIND=pm_reel) :: ixx
!.    real (KIND=pm_reel) :: iyy
!.    real (KIND=pm_reel) :: izz
!.    real (KIND=pm_reel) :: ixy
!.    real (KIND=pm_reel) :: ixz
!.    real (KIND=pm_reel) :: iyz
!.    real (KIND=pm_reel) :: mstr
!.    real (KIND=pm_reel) :: merg
!.    type(MSP_MCI) :: mci
!
!$Arguments
!>[E]   ficmci  :<LEN=*>     nom du fichier
!>[E]   forme   :<integer>   forme du véhicule [par défaut MSP_ENUM_SPHERE]
!>[E]   sx      :<pm_reel>   surface perpendiculaire à l'axe X du véhicule [m^2] [par défaut 0.]
!>[E]   sy      :<pm_reel>   surface perpendiculaire à l'axe Y du véhicule [m^2] [par défaut 0.]
!>[E]   sz      :<pm_reel>   surface perpendiculaire à l'axe Z du véhicule [m^2] [par défaut 0.]
!>[E]   st      :<pm_reel>   surface transverse de révolution du véhicule [m^2] [par défaut 0.]
!>[E]   spx     :<pm_reel>   surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2] [par défaut 0.]
!>[E]   spy     :<pm_reel>   surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2] [par défaut 0.]
!>[E]   spz     :<pm_reel>   surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2] [par défaut 0.]
!>[E]   centrx  :<pm_reel>   centrage en X (%lref) [par défaut 0.]
!>[E]   centry  :<pm_reel>   centrage en Y (%lref) [par défaut 0.]
!>[E]   centrz  :<pm_reel>   centrage en Z (%lref) [par défaut 0.]
!>[E]   ixx     :<pm_reel>   inertie principale en X [kg.m^2] [par défaut 0.]
!>[E]   iyy     :<pm_reel>   inertie principale en Y [kg.m^2] [par défaut 0.]
!>[E]   izz     :<pm_reel>   inertie principale en Z [kg.m^2] [par défaut 0.]
!>[E]   ixy     :<pm_reel>   inertie en XY [kg.m^2] [par défaut 0.]
!>[E]   ixz     :<pm_reel>   inertie en XZ [kg.m^2] [par défaut 0.]
!>[E]   iyz     :<pm_reel>   inertie en YZ [kg.m^2] [par défaut 0.]
!>[E]   mstr    :<pm_reel>   masse sèche [kg] [par défaut 0.]
!>[E]   merg    :<pm_reel>   masse d'ergols [kg] [par défaut 0.]
!>S     mci     :<MSP_MCI>   structure contenant les caractéristiques 
!                            masse/centrage/inertie d'un véhicule
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_lire_str_mci
!
!$Include
!
!$Module
!#V
!- MSP_ACCES
!#
!
!$Remarques
!  Si un nom de fichier est présent, les appels aux autres paramètres ne sont pas pris en compte.
!
!$Mots-cles
!   MCI CREER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     use MSP_ACCES 
     implicit none

      character(LEN=*), intent(IN), optional    :: ficmci

      integer, intent(IN), optional             :: forme
      real (KIND=pm_reel), intent(IN), optional :: sx
      real (KIND=pm_reel), intent(IN), optional :: sy
      real (KIND=pm_reel), intent(IN), optional :: sz
      real (KIND=pm_reel), intent(IN), optional :: st
      real (KIND=pm_reel), intent(IN), optional :: spx
      real (KIND=pm_reel), intent(IN), optional :: spy
      real (KIND=pm_reel), intent(IN), optional :: spz

      real (KIND=pm_reel), intent(IN), optional :: centrx
      real (KIND=pm_reel), intent(IN), optional :: centry
      real (KIND=pm_reel), intent(IN), optional :: centrz

      real (KIND=pm_reel), intent(IN), optional :: ixx
      real (KIND=pm_reel), intent(IN), optional :: iyy
      real (KIND=pm_reel), intent(IN), optional :: izz
      real (KIND=pm_reel), intent(IN), optional :: ixy
      real (KIND=pm_reel), intent(IN), optional :: ixz
      real (KIND=pm_reel), intent(IN), optional :: iyz

      real (KIND=pm_reel), intent(IN), optional :: mstr
      real (KIND=pm_reel), intent(IN), optional :: merg

      type(MSP_MCI) :: mci

      integer ::ier,acc


      if ( present(ficmci) ) then
         mci%ficmci = ficmci
         ier = MSP_acc_charger_donnees (ficmci,acc)
         ! DM1058 - Gestion d'erreur
         if (ier < 0) then
            call MSP_signaler_message (cle_mes="MSP_chargement_donnees", &
                 routine="MSP_creer_mci",partie_variable=trim(ficmci))
         endif
         call  MSP_lire_str_mci(acc,mci)
         
         ! fermeture de l'accès MADONA
         call MSP_fermeture_MADONA(acc)

         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_creer_mci",type=MSP_ENUM_ERREUR)
         endif
         return
      endif

      if ( present(forme) ) then
         mci%forme = forme
      else
         mci%forme = MSP_ENUM_SPHERE
      endif

      if ( present(sx) ) then
         mci%sx = sx
      else
         mci%sx = 0._pm_reel
      endif

      if ( present(sy) ) then
         mci%sy = sy
      else
         mci%sy = 0._pm_reel
      endif

      if ( present(sz) ) then
         mci%sz = sz
      else
         mci%sz = 0._pm_reel
      endif

      if ( present(st) ) then
         mci%st = st
      else
         mci%st = 0._pm_reel
      endif

      if ( present(spx) ) then
         mci%spx = spx
      else
         mci%spx = 0._pm_reel
      endif

      if ( present(spy) ) then
         mci%spy = spy
      else
         mci%spy = 0._pm_reel
      endif

      if ( present(spz) ) then
         mci%spz = spz
      else
         mci%spz = 0._pm_reel
      endif

      ! Initialisations des données relatives au centrage:

      if ( present(centrx) ) then
         mci%centrx = centrx
      else
         mci%centrx = 0._pm_reel
      endif

      if ( present(centry) ) then
         mci%centry = centry
      else
         mci%centry = 0._pm_reel
      endif

      if ( present(centrz) ) then
         mci%centrz = centrz
      else
         mci%centrz = 0._pm_reel
      endif

      ! Initialisations des données relatives aux inerties:

      if ( present(ixx) ) then
         mci%ixx = ixx
      else
         mci%ixx = 0._pm_reel
      endif

      if ( present(iyy) ) then
         mci%iyy = iyy
      else
         mci%iyy = 0._pm_reel
      endif

      if ( present(izz) ) then
         mci%izz = izz
      else
         mci%izz = 0._pm_reel
      endif

      if ( present(ixy) ) then
         mci%ixy = ixy
      else
         mci%ixy = 0._pm_reel
      endif

      if ( present(ixz) ) then
         mci%ixz = ixz
      else
         mci%ixz = 0._pm_reel
      endif

      if ( present(iyz) ) then
         mci%iyz = iyz
      else
         mci%iyz = 0._pm_reel
      endif

      ! Initialisations des données relatives aux masses:

      if ( present(mstr) ) then
         mci%mstr = mstr
      else
         mci%mstr = 0._pm_reel
      endif

      if ( present(merg) ) then
         mci%merg = merg
      else
         mci%merg = 0._pm_reel
      endif

      mci%mtot = mci%mstr + mci%merg

   end function MSP_creer_mci


   subroutine MSP_lire_str_mci (acc,mci)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_str_mci
!
!$Resume
!  Lecture des données relatives aux masses/centrage/inertie d'un véhicule.
!
!$Description
!  Lecture dans une zone de moyen d'accès MADONA des données relatives
!  aux masses/centrage/inertie d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_str_mci (acc,mci)
!.    integer :: acc
!.    type(MSP_MCI) :: mci
!
!$Arguments
!>E     acc  :<integer>   numéro identificateur de la zone de moyen d'accès MADONA
!>S     mci  :<MSP_MCI>   structure contenant les données relatives 
!                         aux masses/centrage/inertie d'un véhicule
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_acc_get_val
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!   MCI LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      type(MSP_MCI), intent(OUT) :: mci

      integer :: ier,iexist
      character(LEN=MSP_LONG_CHAINE) :: label_type_forme

      character(LEN=80),dimension(2)       :: tmessage_var

      ier = acc_select (acc,"MCI",ACC_STRUCT)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure MCI', &
                                    routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture des données de forme:

      call MSP_acc_get_val(acc,"forme",label_type_forme)
      if (MSP_gen_messages("MSP_lire_str_mci")) return

      select case (label_type_forme)
         
      case ("sphère")
         
         mci%forme = MSP_ENUM_SPHERE
         
      case ("plaque")
         
         mci%forme = MSP_ENUM_PLAQUE
         
      case ("cylindre")
         
         mci%forme = MSP_ENUM_CYLINDRE
         
      case ("parallépipède")
         
         mci%forme = MSP_ENUM_PARALLEPIPEDE
         
      case default
         
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du type de forme du véhicule (inconnu)', &
              routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
         
      end select
         
      ! Lecture des différentes surfaces en fonction de la forme:
         
      if ( (mci%forme==MSP_ENUM_PLAQUE) .or. (mci%forme==MSP_ENUM_CYLINDRE) .or. (mci%forme==MSP_ENUM_PARALLEPIPEDE) ) then

         call MSP_acc_get_val(acc,"sx",mci%sx,unit="m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

      endif
         
         
      if ( (mci%forme==MSP_ENUM_CYLINDRE) .or. (mci%forme==MSP_ENUM_PARALLEPIPEDE) ) then

         call MSP_acc_get_val(acc,"sy",mci%sy,unit="m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"sz",mci%sz ,unit="m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

      endif
         
      if ( (mci%forme==MSP_ENUM_SPHERE) .or. (mci%forme==MSP_ENUM_CYLINDRE) ) then

         call MSP_acc_get_val(acc,"st",mci%st ,unit="m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

      endif
         

      ! Lectures des surfaces de panneaux solaires:
      call MSP_acc_get_val(acc,"spx",mci%spx,unit="m^2",valdef=0._pm_reel)
      if (MSP_gen_messages("MSP_lire_str_mci")) return

      call MSP_acc_get_val(acc,"spy",mci%spy ,unit="m^2",valdef=0._pm_reel)
      if (MSP_gen_messages("MSP_lire_str_mci")) return

      call MSP_acc_get_val(acc,"spz",mci%spz ,unit="m^2",valdef=0._pm_reel)
      if (MSP_gen_messages("MSP_lire_str_mci")) return

      ! Lecture des centrages:

      iexist = acc_exist (acc,"centrx")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='des données de centrage', &
                                    routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
      else  if ( iexist == 1 ) then

         call MSP_acc_get_val(acc,"centrx",mci%centrx ,unit="")
         if (MSP_gen_messages("MSP_lire_str_mci")) return
         
         call MSP_acc_get_val(acc,"centry",mci%centry ,unit="")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"centrz",mci%centrz ,unit="")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

      else
         !- Code retour inconnu
         tmessage_var(1) = 'des données de centrage'
         write(tmessage_var(2),'(I4)')  iexist
         
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE_002",partie_variable= tmessage_var, &
                                       routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
      endif

      ! Lecture des masses:

      iexist = acc_exist(acc,"mstr")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='des masses', &
                                    routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
      else  if ( iexist == 1 ) then

         call MSP_acc_get_val(acc,"mstr",mci%mstr ,unit="kg")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"merg",mci%merg ,unit="kg")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         mci%mtot = mci%mstr + mci%merg

      else
         !- Code retour inconnu
         tmessage_var(1) = 'des masses'
         write(tmessage_var(2),'(I4)')  iexist
         
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE_002",partie_variable= tmessage_var, &
                                       routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)


      endif

      ! Lecture des inerties:

      iexist = acc_exist(acc,"ixx")
      if ( iexist < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='des inerties', &
                                    routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
      else  if ( iexist == 1 ) then

         call MSP_acc_get_val(acc,"ixx",mci%ixx ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"iyy",mci%iyy ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"izz",mci%izz ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return

         call MSP_acc_get_val(acc,"ixy",mci%ixy ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return
         
         call MSP_acc_get_val(acc,"ixz",mci%ixz ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return         

         call MSP_acc_get_val(acc,"iyz",mci%iyz ,unit="kg*m^2")
         if (MSP_gen_messages("MSP_lire_str_mci")) return


      else
         !- Code retour inconnu
         tmessage_var(1) = 'des inerties'
         write(tmessage_var(2),'(I4)')  iexist
         
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE_002",partie_variable= tmessage_var, &
                                       routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)


      endif

      ! Fin de la lecture des MCI !

      ier = acc_select_end (acc)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure MCI', &
                                    routine="MSP_lire_str_mci",type=MSP_ENUM_ERREUR)
         return
      endif

   end subroutine MSP_lire_str_mci



   SUBROUTINE MSP_consulter_mci(mci, forme, sx, sy, sz, st, spx, spy, spz,  &
        centrx, centry, centrz, ixx, iyy, izz, ixy, ixz, iyz, mstr, merg, mtot)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_mci
!
!$Resume
!  Consultation de la structure MCI
!
!$Description
!  Consultation de la structure MCI
!
!$Auteur
!   Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_mci(mci, [forme], [sx], [sy], [sz], [st], [spx], [spy], [spz],  &
!.            [centrx], [centry], [centrz], [ixx], [iyy], [izz], [ixy], [ixz], [iyz], [mstr], [merg], [mtot])
!.    type(MSP_MCI) :: mci
!.    integer :: forme
!.    real (KIND=PM_REEL) :: sx, sy, sz, st, spx, spy, spz
!.    real (KIND=PM_REEL) :: centrx, centry, centrz
!.    real (KIND=PM_REEL) :: ixx, iyy, izz
!.    real (KIND=PM_REEL) :: ixy, ixz, iyz
!.    real (KIND=PM_REEL) :: mstr, merg, mtot
!
!$Arguments
!>E     mci     :<MSP_MCI>   Structure MCI à consulter
!>[S]   forme   :<integer>   forme du véhicule
!>[S]   sx      :<PM_REEL>   surface perpendiculaire à l'axe X du véhicule [m^2]
!>[S]   sy      :<PM_REEL>   surface perpendiculaire à l'axe Y du véhicule [m^2]
!>[S]   sz      :<PM_REEL>   surface perpendiculaire à l'axe Z du véhicule [m^2]
!>[S]   st      :<PM_REEL>   surface transverse de révolution du véhicule [m^2]  
!>[S]   spx     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>[S]   spy     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>[S]   spz     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>[S]   centrx  :<PM_REEL>   centrage en X (%lref)         
!>[S]   centry  :<PM_REEL>   centrage en Y (%lref)         
!>[S]   centrz  :<PM_REEL>   centrage en Z (%lref)         
!>[S]   ixx     :<PM_REEL>   inertie principale en X [kg.m^2]
!>[S]   iyy     :<PM_REEL>   inertie principale en Y [kg.m^2]
!>[S]   izz     :<PM_REEL>   inertie principale en Z [kg.m^2]
!>[S]   ixy     :<PM_REEL>   inertie principale en XY [kg.m^2]
!>[S]   ixz     :<PM_REEL>   inertie principale en XZ [kg.m^2]
!>[S]   iyz     :<PM_REEL>   inertie principale en YZ [kg.m^2]
!>[S]   mstr    :<PM_REEL>   masse sèche [kg]
!>[S]   merg    :<PM_REEL>   masse d'ergols [kg]
!>[S]   mtot    :<PM_REEL>   masse totale [kg]
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
!   MCI CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     type(MSP_MCI), intent(IN) :: mci
     integer, intent(OUT), optional             :: forme
     real (KIND=PM_REEL), intent(OUT), optional :: sx, sy, sz, st, spx, spy, spz

     real (KIND=PM_REEL), intent(OUT), optional :: centrx, centry, centrz

     real (KIND=PM_REEL), intent(OUT), optional :: ixx, iyy, izz
     real (KIND=PM_REEL), intent(OUT), optional :: ixy, ixz, iyz

     real (KIND=PM_REEL), intent(OUT), optional :: mstr, merg, mtot

     
     if (PRESENT(forme))  forme = mci%forme
     if (PRESENT(sx))     sx = mci%sx
     if (PRESENT(sy))     sy = mci%sy
     if (PRESENT(sz))     sz = mci%sz
     if (PRESENT(st))     st = mci%st
     if (PRESENT(spx))   spx = mci%spx
     if (PRESENT(spy))   spy = mci%spy
     if (PRESENT(spz))   spz = mci%spz

     if (PRESENT(centrx)) centrx = mci%centrx
     if (PRESENT(centry)) centry = mci%centry
     if (PRESENT(centrz)) centrz = mci%centrz

     if (PRESENT(ixx)) ixx = mci%ixx
     if (PRESENT(iyy)) iyy = mci%iyy
     if (PRESENT(izz)) izz = mci%izz
     if (PRESENT(ixy)) ixy = mci%ixy
     if (PRESENT(ixz)) ixz = mci%ixz
     if (PRESENT(iyz)) iyz = mci%iyz

     if (PRESENT(mstr)) mstr = mci%mstr
     if (PRESENT(merg)) merg = mci%merg
     if (PRESENT(mtot)) mtot = mci%mtot

 
   end SUBROUTINE MSP_consulter_mci

   
   SUBROUTINE MSP_modifier_mci(mci, forme, sx, sy, sz, st, spx, spy, spz,   &
        centrx, centry, centrz, ixx, iyy, izz, ixy, ixz, iyz, mstr, merg)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_mci
!
!$Resume
!  Modification des caractéristiques de la structure MCI
!
!$Description
!  Modification des caractéristiques de la structure MCI
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_mci(mci, [forme], [sx], [sy], [sz], [st], [spx], [spy], [spz],   &
!.            [centrx], [centry], [centrz], [ixx], [iyy], [izz], [ixy], [ixz], [iyz], [mstr], [merg])
!.    type(MSP_MCI) :: mci
!.    integer :: forme
!.    real (KIND=PM_REEL) :: sx, sy, sz, st, spx, spy, spz
!.    real (KIND=PM_REEL) :: centrx, centry, centrz
!.    real (KIND=PM_REEL) :: ixx, iyy, izz
!.    real (KIND=PM_REEL) :: ixy, ixz, iyz
!.    real (KIND=PM_REEL) :: mstr, merg
!
!$Arguments
!>E/S   mci     :<MSP_MCI>   Structure MCI à modifier
!>[E]   forme   :<integer>   forme du véhicule
!>[E]   sx      :<PM_REEL>   surface perpendiculaire à l'axe X du véhicule [m^2]
!>[E]   sy      :<PM_REEL>   surface perpendiculaire à l'axe Y du véhicule [m^2]
!>[E]   sz      :<PM_REEL>   surface perpendiculaire à l'axe Z du véhicule [m^2]
!>[E]   st      :<PM_REEL>   surface transverse de révolution du véhicule [m^2]  
!>[E]   spx     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>[E]   spy     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>[E]   spz     :<PM_REEL>   surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>[E]   centrx  :<PM_REEL>   centrage en X (%lref)
!>[E]   centry  :<PM_REEL>   centrage en Y (%lref)
!>[E]   centrz  :<PM_REEL>   centrage en Z (%lref)
!>[E]   ixx     :<PM_REEL>   inertie principale en X [kg.m^2]
!>[E]   iyy     :<PM_REEL>   inertie principale en Y [kg.m^2]
!>[E]   izz     :<PM_REEL>   inertie principale en Z [kg.m^2]
!>[E]   ixy     :<PM_REEL>   inertie principale en XY [kg.m^2]
!>[E]   ixz     :<PM_REEL>   inertie principale en XZ [kg.m^2]
!>[E]   iyz     :<PM_REEL>   inertie principale en YZ [kg.m^2]
!>[E]   mstr    :<PM_REEL>   masse sèche [kg]
!>[E]   merg    :<PM_REEL>   masse d'ergols [kg]
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
!   MCI MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     type(MSP_MCI), intent(INOUT) :: mci
     integer, intent(IN), optional             :: forme
     real (KIND=PM_REEL), intent(IN), optional :: sx, sy, sz, st, spx, spy, spz

     real (KIND=PM_REEL), intent(IN), optional :: centrx, centry, centrz

     real (KIND=PM_REEL), intent(IN), optional :: ixx, iyy, izz
     real (KIND=PM_REEL), intent(IN), optional :: ixy, ixz, iyz

     real (KIND=PM_REEL), intent(IN), optional :: mstr, merg

     
     if (PRESENT(forme))  mci%forme = forme
     if (PRESENT(sx))     mci%sx = sx
     if (PRESENT(sy))     mci%sy = sy
     if (PRESENT(sz))     mci%sz = sz
     if (PRESENT(st))     mci%st = st
     if (PRESENT(spx))    mci%spx = spx
     if (PRESENT(spy))    mci%spy = spy
     if (PRESENT(spz))    mci%spz = spz

     if (PRESENT(centrx)) mci%centrx = centrx
     if (PRESENT(centry)) mci%centry = centry
     if (PRESENT(centrz)) mci%centrz = centrz

     if (PRESENT(ixx))    mci%ixx = ixx
     if (PRESENT(iyy))    mci%iyy = iyy
     if (PRESENT(izz))    mci%izz = izz
     if (PRESENT(ixy))    mci%ixy = ixy
     if (PRESENT(ixz))    mci%ixz = ixz
     if (PRESENT(iyz))    mci%iyz = iyz

     if (PRESENT(mstr))   mci%mstr = mstr
     if (PRESENT(merg))   mci%merg = merg
     if (PRESENT(mstr).or.PRESENT(merg)) mci%mtot = mci%mstr + mci%merg

 
   end SUBROUTINE MSP_modifier_mci



    subroutine MSP_afficher_mci (mci,ilog)
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_mci
!
!$Resume
!  Affichage des caractéristiques du véhicule 
!
!$Description
!  Affichage des caractéristiques du véhicule 
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_mci (mci,[ilog])
!.    type(MSP_MCI) :: mci
!.    integer :: ilog
!
!$Arguments
!>E     mci   :<MSP_MCI>   
!>[E/S] ilog  :<integer>   Numéro de l'unité logique d'affichage
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
!  MCI AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_MCI), intent(IN) :: mci
      integer, optional :: ilog

      integer :: num

      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif


      write(num,'(a,i9)') "FORME:   ",mci%forme
      if ( (mci%forme==MSP_ENUM_PLAQUE) .or. (mci%forme==MSP_ENUM_CYLINDRE) .or. (mci%forme==MSP_ENUM_PARALLEPIPEDE) ) then
         write(num,'(a,g21.12)') "SX:      ",mci%sx
      endif
      if ( (mci%forme==MSP_ENUM_CYLINDRE) .or. (mci%forme==MSP_ENUM_PARALLEPIPEDE) ) then
         write(num,'(a,g21.12)') "SY:      ",mci%sy
         write(num,'(a,g21.12)') "SZ:      ",mci%sz
      endif
      if ( (mci%forme==MSP_ENUM_SPHERE) .or. (mci%forme==MSP_ENUM_CYLINDRE) ) then
         write(num,'(a,g21.12)') "ST:      ",mci%st         
      endif

      write(num,'(a,g21.12)') "IXX:     ",mci%ixx
      write(num,'(a,g21.12)') "IYY:     ",mci%iyy
      write(num,'(a,g21.12)') "IZZ:     ",mci%izz
      write(num,'(a,g21.12)')
      write(num,'(a,g21.12)') "IXY:     ",mci%ixy
      write(num,'(a,g21.12)') "IXZ:     ",mci%ixz
      write(num,'(a,g21.12)') "IYZ:     ",mci%iyz
      write(num,'(a)') ""
      write(num,'(a,g21.12)') "Spx:     ",mci%spx
      write(num,'(a,g21.12)') "Spy:     ",mci%spy
      write(num,'(a,g21.12)') "Spz:     ",mci%spz
      write(num,'(a)')
      write(num,'(a,g21.12)') "MSECHE:  ",mci%mstr
      write(num,'(a,g21.12)') "MERGOLS: ",mci%merg
      write(num,'(a,g21.12)') "MTOTALE: ",mci%mtot
      write(num,'(a)')

    end subroutine MSP_afficher_mci


 subroutine MSP_calcul_surf_app (forme,sx,sy,sz,st,modatt,matt,vdir,surfapp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calcul_surf_app
!
!$Resume
!       Routine de calcul de la surface d'application du véhicule (utilisée pour le calcul de force)
!
!$Description
!       Routine de calcul de la surface d'application du véhicule (utilisée pour le calcul de force)
!      
!$Auteur
!       G. Mercadier (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calcul_surf_app (forme,sx,sy,sz,st,modatt,matt,vdir,surfapp)
!.    integer :: forme
!.    real (kind=pm_reel) :: sx
!.    real (kind=pm_reel) :: sy
!.    real (kind=pm_reel) :: sz
!.    real (kind=pm_reel) :: st
!.    integer :: modatt
!.    real (kind=pm_reel),dimension(3,3) :: matt
!.    real (kind=pm_reel),dimension(3) :: vdir
!.    real (kind=pm_reel) :: surfapp
!
!$Arguments
!>E     forme    :<integer>           
!>E     modatt   :<integer>             
!>E     matt     :<pm_reel,DIM=(3,3)>   
!>E     vdir     :<pm_reel,DIM=(3)>     
!>S     surfapp  :<pm_reel>             
!
!$Common
!
!$Routines
!- mu_norme
!- MSP_signaler_message
!- MSP_consulter_mci
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
    
    ! Déclaration des arguments
    integer, intent(IN)                            :: forme
    real (kind=pm_reel), intent(IN)                :: sx, sy, sz, st
    integer, intent(IN)                            :: modatt
    real (kind=pm_reel), intent(IN),dimension(3,3) :: matt
    real (kind=pm_reel), intent(IN),dimension(3)   :: vdir
    real (kind=pm_reel), intent(OUT)               :: surfapp
    
    
    ! Autres déclarations
    type(tm_code_retour)             :: code_retour
    real(kind=pm_reel)               :: norme, cosx, cosy, cosz, sinx, sin2
    real (kind=pm_reel),dimension(3) :: vdirn
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Norme du vecteur directeur !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    call mu_norme (vdir, norme, code_retour, vdirn)
    if (code_retour%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_retour)
       if (MSP_gen_messages("MSP_calcul_surf_app" )) return
    end if

      
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul de la surface de référence ou surface apparente ! 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! Cas où il n'y a pas de loi d'attitude
    if ( modatt == 0 ) then

       select case (forme)

          case (MSP_ENUM_SPHERE)
             surfapp = st
          case default
             surfapp = sx
       end select
    
    ! Cas avec loi d'attitude
    else
       select case (forme)

          case (MSP_ENUM_SPHERE)
             surfapp = st
	     
          case (MSP_ENUM_PLAQUE)
             cosx = (vdirn(1)*matt(1,1)+vdirn(2)*matt(1,2)+vdirn(3)*matt(1,3))
             surfapp = sx * abs(cosx)

          case (MSP_ENUM_CYLINDRE)
             cosx = (vdirn(1)*matt(1,1)+vdirn(2)*matt(1,2)+vdirn(3)*matt(1,3))
             cosy = (vdirn(1)*matt(2,1)+vdirn(2)*matt(2,2)+vdirn(3)*matt(2,3))
             cosz = (vdirn(1)*matt(3,1)+vdirn(2)*matt(3,2)+vdirn(3)*matt(3,3))
	     
             sin2 = 1._pm_reel - cosx**2 
             if ( sin2 < MSP_EPSILON ) sin2 = 0._pm_reel
             if ( (sin2 - 1._pm_reel) > MSP_EPSILON ) sin2 = 1._pm_reel                 
             sinx = sqrt(sin2)

             surfapp = sx * abs(cosx) + sy * abs(cosy) + sz * abs(cosz) + st * sinx

          case (MSP_ENUM_PARALLEPIPEDE)
             cosx = (vdirn(1)*matt(1,1)+vdirn(2)*matt(1,2)+vdirn(3)*matt(1,3))
             cosy = (vdirn(1)*matt(2,1)+vdirn(2)*matt(2,2)+vdirn(3)*matt(2,3))
             cosz = (vdirn(1)*matt(3,1)+vdirn(2)*matt(3,2)+vdirn(3)*matt(3,3))
             
	     surfapp = sx * abs(cosx) + sy * abs(cosy) + sz * abs(cosz)

       end select

    endif

 end subroutine MSP_calcul_surf_app



  end module MSP_MCI_DEF
