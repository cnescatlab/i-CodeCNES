module MSP_AERO_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_AERO_DEF
!
!$Resume
!  Module gérant les caractéristiques aerodynamiques des véhicules.
!
!$Description
!  Module gérant les caractéristiques aerodynamiques des véhicules.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_AERO_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_AERO_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.37  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.36  2009/09/04 12:57:54  tanguyy
!  DM-ID 1113 : Fermeture des accÃ©s MADONA une fois le fichier ouvert
!
!  Revision 1.35  2009/02/16 14:57:22  huec
!  FA-ID 1182 : La variable inutilisee alt_max a ete supprimee
!
!  Revision 1.34  2008/11/20 14:14:54  tanguyy
!  DM-ID 733 : affichages inutiles dans msp_calculer_coef_tabules
!
!  Revision 1.33  2008/11/19 13:29:43  tanguyy
!  DM-ID 733 : consultation des coefs constants ; les valeurs se trouvent dans coef_1d
!
!  Revision 1.32  2008/11/19 13:28:50  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.31  2008/11/10 09:28:39  tanguyy
!  DM-ID 733 : fonctions msp_calculer_coefs_tabules et msp_calculer_coefs avec gestion des types de coefs utilisés par PSIMU
!
!  Revision 1.30  2008/10/24 08:01:34  huec
!  AQ : Indentation du code
!  Revision 1.29  2008/08/08 13:52:42  gss
!  DM-ID 1058 : (portage g95) suppression de variables non utilisées. Ajout de la
!  gestion d'erreur pour la fonction acc_charger_donnees. Initialisation à NULL
!  des pointeurs lors de leur déclaration.
!  Revision 1.28  2008/07/04 15:03:00  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.27  2007/11/27 14:55:16  huec
!  DM-ID 699 : Amelioration des moyens de tests MECASPA
!  Revision 1.26  2007/11/05 16:03:53  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.25  2007/06/18 10:14:22  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.24  2007/06/15 13:47:45  vivaresf
!  FA-ID 746 : mise en inout de la structure AERO pour éviter fuite mémoire dans :
!  - MSP_creer_aero
!  - MSP_lire_str_aero
!  - MSP_consulter_aero (structure COEF)
!  Revision 1.23  2007/03/19 09:55:14  tanguyy
!  AQ : mise a jour des cartouches
!  Revision 1.22  2007/02/02 10:36:11  vivaresf
!  Version 4.4a1 : variables inutilisées
!  Revision 1.21  2007/02/02 08:28:26  tanguyy
!  DM-ID 659 : rajout d'un type de variation MSP_ENUM_COEFF_NULS pour gerer les init par defaut de structure AERO
!  Revision 1.20  2007/01/25 16:16:55  tanguyy
!  DM-ID 659 : Cloture du FT (Coefficients aerodynamiques dependants de l altitude)
!  Revision 1.19.2.1  2007/01/25 16:16:44  tanguyy
!  DM-ID 659 : Rajout d'un code pour le type de variation des coefficients
!  Revision 1.19  2006/11/15 10:09:35  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.18  2006/11/09 09:52:33  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.17  2006/11/06 14:13:59  vpg
!  DM-ID 425 : passage PSIMU sous Linux : initialisations complémentaires
!  Revision 1.16  2006/06/02 11:21:51  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.15  2005/03/08 07:32:32  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.14  2005/01/20 13:56:06  pauh
!  FA_332
!  Revision 1.13.2.1  2005/01/19 09:26:03  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.13  2005/01/13 13:41:37  pauh
!  DM_113
!  Revision 1.12.2.1  2005/01/10 15:40:33  pauh
!  ajout des fonctions :
!  - MSP_interp_coef_vehicule
!  - MSP_interp_coef_aero
!  - MSP_interp_cm0
!  - MSP_interp_cmq
!  - MSP_interp_cnp
!  - MSP_interp_cnr
!  - MSP_interp_clp
!  - MSP_interp_clr
!  Revision 1.12  2004/12/10 16:24:54  fabrec
!  DM-ID 175 : correction d'un bug dans msp_creer_aero
!  Revision 1.11  2004/11/05 16:27:03  vivaresf
!  coquilles
!  Revision 1.10  2004/10/25 10:15:01  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.9  2003/07/10 13:59:47  adm_ipsi
!  FA-ID 14 : Utilisation de la fonction MSP_acc_get_val
!  Revision 1.1  2003/03/27 16:44:07  util_am
!  SLB - Version industrialisée
!  Revision 1.8  2003/01/31 15:37:20  adm_ipsi
!  Ajout de commentaires
!  Revision 1.6  2002/12/04 18:08:23  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.5  2002/12/03 17:21:00  adm_ipsi
!   Ajout de implicit none
!  Revision 1.4  2002/11/28 15:36:37  adm_ipsi
!  MSP_effacer_aero : remise à zero des champs non pointeur dans tous les cas
!  Revision 1.3  2002/11/26 11:45:56  adm_ipsi
!  Conservation du nom du fichier dans la structure AERO lors du chargement par lire_str_aero
!  Revision 1.2  2002/10/04 16:10:28  adm_ipsi
!  MSP_AERO_DEF Modification concernant l'effacement avant création : nullify sur premier effacement
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
!  use MSP_AERO_DEF
!
!$Structure
!
!: MSP_AERO : 
!>     flag_func        : <logical>              Booléen permettant de tracer l'origine de la création de la structure AERO
!>                                                     pour éliminer les fuites mémoire
!>     type_coef        : <integer>              type de coefficient :
!>                                               MSP_ENUM_COEFF_AERO_VITESSE (exprimé dans repère aérodynamique Cx/Cy/Cz)
!>                                               MSP_ENUM_COEFF_AERO_VEHICULE (exprimé dans repère véhicule : Ca/Cy/Cn)
!>                                               et MSP_ENUM_CF_TABULE_ALT_MOY : Cx/Cz en fonction de l'altitude)
!>     type_variation   : <integer>              type de variation des coefficients (nuls, constants, 
!>                                               en fonction de l'altitude, en fonction de l'incidence et du mach, etc.)
!>     cmf              : <pm_reel>              coefficient multiplicatif sur la force 
!>     sref             : <pm_reel>              surface de référence [m^2]
!>     lref             : <pm_reel>              longueur de référence [m]
!>     xref             : <pm_reel>              centrage en X de référence (%lref)
!>     yref             : <pm_reel>              centrage en Y de référence (%lref)
!>     zref             : <pm_reel>              centrage en Z de référence (%lref)
!>     cxa              : <MSP_COEF,pointer>     structure coefficient de trainee
!>     cy               : <MSP_COEF,pointer>     struturce coefficient cy
!>     czn              : <MSP_COEF,pointer>     structure coefficient de portance
!>     cl0              : <MSP_COEF,pointer>     structure coefficient de roulis
!>     cm0              : <MSP_COEF,pointer>     structure coefficient de tangage
!>     cn0              : <MSP_COEF,pointer>     structure coefficient de lacet
!>     cmq              : <MSP_COEF,pointer>     structure coefficient dynamique de tangage
!>     cnp              : <MSP_COEF,pointer>     structure coefficient dynamique de lacet
!>     cnr              : <MSP_COEF,pointer>     structure coefficient dynamique de lacet
!>     clp              : <MSP_COEF,pointer>     structure coefficient dynamique de roulis
!>     clr              : <MSP_COEF,pointer>     structure coefficient dynamique de roulis
!>     ficaero          : <LEN=MSP_LONG_NOMFIC>  nom du fichier
!
!$Global
!
!>  MSP_ENUM_COEFF_AERO_VITESSE    : <integer,parameter>  coefficients de trainée/portance sous la forme CX/CZ
!>  MSP_ENUM_COEFF_AERO_VEHICULE   : <integer,parameter>  coefficients de trainée/portance sous la forme CA/CN
!>  MSP_ENUM_CF_TABULE_ALT_MOY     : <integer,parameter>  coefficients Cx/Cz dépendant d'un profil donné en altitude
!>  MSP_ENUM_CONSTANT              : <integer,parameter>  coefficients de trainée/portance constants
!>  MSP_ENUM_ALTITUDE              : <integer,parameter>  coefficients de trainée/portance dépendants de l'altitude
!>  MSP_ENUM_INCIDENCE_MACH        : <integer,parameter>  coefficients de trainée/portance dépendants de l'incidence et du mach
!>  MSP_ENUM_AUTRE_VARIATION       : <integer,parameter>  coefficients de trainée/portance dépendants 
!>  MSP_ENUM_COEFF_NULS            : <integer,parameter>  coefficients de trainée/portance nuls/non initialisés
!>  MSP_NB_POINTS_TABULES          : <integer,parameter>  
!$Common
!
!$Routines
!- MSP_create_aerodynamics
!- MSP_clear_aerodynamics
!- MSP_get_aerodynamics
!- MSP_set_aerodynamics
!- MSP_display_aero
!- MSP_effacer_aero
!- MSP_lire_str_aero
!- MSP_consulter_aero
!- MSP_modifier_aero
!- MSP_afficher_aero
!- MSP_calculer_coefs_tabules
!- MSP_calculer_coefs_aero
!#V
!- egaler_aero
!#
!
!$Fonctions
!- MSP_creer_aero
!- MSP_interp_coef_vehicule
!- MSP_interp_coef_aero
!- MSP_interp_cm0
!- MSP_interp_cmq
!- MSP_interp_cnp
!- MSP_interp_cnr
!- MSP_interp_clp
!- MSP_interp_clr
!
!$Include
!
!$Module
!#V
!- MSP_COEF_DEF
!- MSP_ACCES
!#
!
!$Interface
!> msp_create_aerodynamics :  MSP_creer_aero
!> msp_set_aerodynamics :     MSP_modifier_aero
!> assignment :               egaler_aero
!> msp_display_aero :         MSP_afficher_aero
!> msp_get_aerodynamics :     MSP_consulter_aero
!> msp_clear_aerodynamics :   MSP_effacer_aero
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE
!
!$Voir-Aussi
!#V
!.  egaler_aero
!#
!.  MSP_creer_aero MSP_interp_coef_vehicule MSP_interp_coef_aero MSP_interp_cm0 MSP_interp_cmq
!.  MSP_interp_cnp MSP_interp_cnr MSP_interp_clp MSP_interp_clr MSP_create_aerodynamics MSP_clear_aerodynamics
!.  MSP_get_aerodynamics MSP_set_aerodynamics MSP_display_aero MSP_effacer_aero MSP_lire_str_aero
!.  MSP_consulter_aero MSP_modifier_aero MSP_afficher_aero MSP_calculer_coefs_tabules MSP_calculer_coefs_aero
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSP_COEF_DEF
   use MSP_ACCES
   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_AERO_DEF.F90 365 2013-02-18 12:36:19Z aadt $'

   ! DEFINITIONS DE TYPES:


   type MSP_AERO

      !private

      logical :: flag_func
      integer :: type_coef
      integer :: type_variation

      real (KIND=pm_reel) :: cmf

      real (KIND=pm_reel) :: sref
      real (KIND=pm_reel) :: lref

      real (KIND=pm_reel) :: xref
      real (KIND=pm_reel) :: yref
      real (KIND=pm_reel) :: zref

      type(MSP_COEF),pointer::cxa => NULL()
      type(MSP_COEF),pointer::cy  => NULL()
      type(MSP_COEF),pointer::czn => NULL()
      type(MSP_COEF),pointer::cl0 => NULL()
      type(MSP_COEF),pointer::cm0 => NULL()
      type(MSP_COEF),pointer::cn0 => NULL()
      type(MSP_COEF),pointer::cmq => NULL()
      type(MSP_COEF),pointer::cnp => NULL()
      type(MSP_COEF),pointer::cnr => NULL()
      type(MSP_COEF),pointer::clp => NULL()
      type(MSP_COEF),pointer::clr => NULL()
 
      character(LEN=MSP_LONG_NOMFIC) :: ficaero

   end type MSP_AERO


   ! VARIABLES GLOBALES:

   ! NOTION DE TYPE DE COEFFICIENT
   !=============================================
   ! Type de repère d'expression des coefs : repère vitesse (ou aérodynamique) : Cx/Cz
   ! repère véhicule : Ca/Cn
   integer, parameter :: MSP_ENUM_COEFF_AERO_VITESSE  = 1
   integer, parameter :: MSP_ENUM_COEFF_AERO_VEHICULE = 2
   
   ! types de coefficients : tabulés en altitude (cf PSIMU)
   integer, parameter :: MSP_ENUM_CF_TABULE_ALT_MOY  = 99

   ! NOTION DE TYPE DE VARIATION DES COEFFICIENTS
   !======================================================
   ! Type de variation des coefficients : constants, en fonction de l'altitude, en fonction
   ! de l'incidence et du mach
   ! Le type MSP_ENUM_AUTRE_VARIATION correspond aux autres possibilités, gérées par la MECASPA
   ! mais pas par PSIMU pour le moment (coefs en fonction du dérapage, du nb de reynolds, ou une combinaison de tous..)
   ! Le type MSP_ENUM_COEFF_NULS correspond à des coefs non initialisés (cxa et czn non alloués)
   ! -> sera traité par PSIMU comme des coefs constants et nuls (valeurs à 0)
   integer, parameter :: MSP_ENUM_CONSTANT = 0
   integer, parameter :: MSP_ENUM_ALTITUDE = 1
   integer, parameter :: MSP_ENUM_INCIDENCE_MACH = 2
   integer, parameter :: MSP_ENUM_AUTRE_VARIATION = 3
   ! Coefs nuls, c'est à dire non initialisés.
   integer, parameter :: MSP_ENUM_COEFF_NULS = -1

integer, parameter :: MSP_NB_POINTS_TABULES = 45
   
   ! SOUS-PROGRAMMES ET FONCTIONS
   !=============================
   private :: egaler_aero

   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
! surcharge de l affectation
!
!$Description
! surcharge de l affectation pour desallouer correctement les pointeurs
!
!$Acces
!  PUBLIC
!
!$Usage
!  aeroa=aerob
!.    type(MSP_AERO) :: aeroa
!.    type(MSP_AERO) :: aerob
!
!$Procedures
!#V
!- egaler_aero
!#
!
!$Remarques
!
!$Mots-cles
! VEHICULE AERODYNAMIQUE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_aero
   end interface


   interface MSP_create_aerodynamics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_aerodynamics
!
!$Resume
!  Creates an aerodynamics structure
!
!$Description
!  Creates an aerodynamics structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  aero = MSP_create_aerodynamics (type_coef,type_variation,[ficaero],[cmf],[sref],[lref],[xref],[yref],[zref],[cxa],[cy],[czn],&
!.                                   cm0,cn0,cl0,cmq,cnp,cnr,clp,clr)
!.    integer :: type_coef
!.    integer :: type_variation
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type(MSP_COEF) :: cxa
!.    type(MSP_COEF) :: cy
!.    type(MSP_COEF) :: czn
!.    type(MSP_COEF) :: cm0
!.    type(MSP_COEF) :: cn0
!.    type(MSP_COEF) :: cl0
!.    type(MSP_COEF) :: cmq
!.    type(MSP_COEF) :: cnp
!.    type(MSP_COEF) :: cnr
!.    type(MSP_COEF) :: clp
!.    type(MSP_COEF) :: clr
!.    type(MSP_AERO) :: aero
!
!$Procedures
!- MSP_creer_aero
!
!$Remarques
!
!$Mots-cles
! VEHICULE AERODYNAMIQUE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_aero
   end interface


   interface MSP_clear_aerodynamics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_aerodynamics
!
!$Resume
!  Clears the content of an aerodynamics structure
!
!$Description
!  Clears the content of an aerodynamics structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_aerodynamics(aero,[nul])
!.    type(MSP_AERO) :: aero
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_aero
!
!$Remarques
!
!$Mots-cles
! VEHICULE AERODYNAMIQUE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_aero
   end interface



   interface MSP_get_aerodynamics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_aerodynamics
!
!$Resume
!  To get information on satellite aerodynamics characteristics
!
!$Description
!  To get information on satellite aerodynamics characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_aerodynamics (aero, [ficaero], [cmf], [type_coef], [type_variation], [sref], [lref], [xref], [yref], [zref], &
!.                                   cxa,cy,czn,cm0,cn0,cl0,cmq,cnp,cnr,clp,clr)
!.    type(MSP_AERO) :: aero
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    integer :: type_coef
!.    integer :: type_variation
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type (MSP_COEF) :: cxa
!.    type (MSP_COEF) :: cy
!.    type (MSP_COEF) :: czn
!.    type (MSP_COEF) :: cm0
!.    type (MSP_COEF) :: cn0
!.    type (MSP_COEF) :: cl0
!.    type (MSP_COEF) :: cnp
!.    type (MSP_COEF) :: cnr
!.    type (MSP_COEF) :: cmq
!.    type (MSP_COEF) :: clp
!.    type (MSP_COEF) :: clr
!
!$Procedures
!- MSP_consulter_aero
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_aero
   end interface

   interface MSP_set_aerodynamics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_aerodynamics
!
!$Resume
!  To modifiy information on satellite aerodynamics characteristics
!
!$Description
!  To modifiy information on satellite aerodynamics characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_aerodynamics (aero, [ficaero], [cmf], [type_coef], [type_variation], [sref], [lref], [xref], [yref], [zref], &
!.                                   cxa,cy,czn,cm0,cn0,cl0,cmq,cnp,cnr,clp,clr)
!.    type(MSP_AERO) :: aero
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    integer :: type_coef
!.    integer :: type_variation
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type (MSP_COEF) :: cxa
!.    type (MSP_COEF) :: cy
!.    type (MSP_COEF) :: czn
!.    type (MSP_COEF) :: cm0
!.    type (MSP_COEF) :: cn0
!.    type (MSP_COEF) :: cl0
!.    type (MSP_COEF) :: cnp
!.    type (MSP_COEF) :: cnr
!.    type (MSP_COEF) :: cmq
!.    type (MSP_COEF) :: clp
!.    type (MSP_COEF) :: clr
!
!$Procedures
!- MSP_modifier_aero
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_aero
   end interface

   interface MSP_display_aero

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_aero
!
!$Resume
!  Displays information on satellite characteristics
!
!$Description
!  Displays information on satellite characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_aero (aero,[ilog])
!.    type(MSP_AERO) :: aero
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_aero
!
!$Remarques
!
!$Mots-cles
! VEHICULE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_aero
   end interface


   contains


    subroutine egaler_aero(aeroa,aerob)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_aero
!
!$Resume
!	Routine definissant l'affectation de 2 structures aero
!
!$Description
!	Routine definissant l'affectation de 2 structures aero
!
!$Auteur
!       S. Rousseau
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_aero(aeroa,aerob)
!.    type(MSP_AERO) :: aeroa
!.    type(MSP_AERO) :: aerob
!
!$Arguments
!>E/S   aeroa  :<MSP_AERO>   structure aero à gauche de =
!>E     aerob  :<MSP_AERO>   structure aero à droite de =
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- MSP_effacer_coef
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE AERODYNAMIQUE EGALER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_AERO),intent(inout)::aeroa
      type(MSP_AERO),intent(in)::aerob


      call MSP_effacer_aero(aeroa)
      aeroa%flag_func = .false.

      aeroa%type_coef = aerob%type_coef

      aeroa%type_variation = aerob%type_variation

      aeroa%cmf = aerob%cmf

      aeroa%sref = aerob%sref
      aeroa%lref = aerob%lref

      aeroa%xref = aerob%xref
      aeroa%yref = aerob%yref
      aeroa%zref = aerob%zref

      ! -- Coefficient cxa
      if (associated(aerob%cxa)) then
         allocate(aeroa%cxa)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cxa,nul=.true.)
         aeroa%cxa = aerob%cxa
      end if

      ! -- Coefficient cy
      if (associated(aerob%cy)) then
         allocate(aeroa%cy)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cy,nul=.true.)
         aeroa%cy = aerob%cy
      end if

      ! -- Coefficient czn
      if (associated(aerob%czn)) then
         allocate(aeroa%czn)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%czn,nul=.true.)
         aeroa%czn = aerob%czn
      end if

      ! -- Coefficient cm0
      if (associated(aerob%cm0)) then
         allocate(aeroa%cm0)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cm0,nul=.true.)
         aeroa%cm0 = aerob%cm0
      end if

      ! -- Coefficient cn0
      if (associated(aerob%cn0)) then
         allocate(aeroa%cn0)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cn0,nul=.true.)
         aeroa%cn0 = aerob%cn0
      end if

      ! -- Coefficient cl0
      if (associated(aerob%cl0)) then
         allocate(aeroa%cl0)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cl0,nul=.true.)
         aeroa%cl0 = aerob%cl0
      end if

      ! -- Coefficient cmq
      if (associated(aerob%cmq)) then
         allocate(aeroa%cmq)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cmq,nul=.true.)
         aeroa%cmq = aerob%cmq
      end if

      ! -- Coefficient cnp
       if (associated(aerob%cnp)) then
         allocate(aeroa%cnp)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cnp,nul=.true.)
         aeroa%cnp = aerob%cnp
      end if

      ! -- Coefficient cnr
       if (associated(aerob%cnr)) then
         allocate(aeroa%cnr)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%cnr,nul=.true.)
         aeroa%cnr = aerob%cnr
      end if

      ! -- Coefficient clp
       if (associated(aerob%clp)) then
         allocate(aeroa%clp)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%clp,nul=.true.)
         aeroa%clp = aerob%clp
      end if

      ! -- Coefficient clr
       if (associated(aerob%clr)) then
         allocate(aeroa%clr)
         ! On nullifie la structure coef
         call MSP_effacer_coef(aeroa%clr,nul=.true.)
         aeroa%clr = aerob%clr
      end if

 
      aeroa%ficaero=aerob%ficaero

    end subroutine egaler_aero

    subroutine MSP_effacer_aero(aero,nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_aero
!
!$Resume
!	Routine permettant de désallouer proprement une structure aero
!
!$Description
!	Routine permettant de désallouer proprement une structure aero
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_aero(aero,[nul])
!.    type(MSP_AERO) :: aero
!.    logical :: nul
!
!$Arguments
!>E/S   aero  :<MSP_AERO>   Structure AERO à effacer
!>[E]   nul   :<logical>    =.true., on se contente des instructions NULLIFY (par défaut .false.)
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
!  VEHICULE AERODYNAMIQUE EFFACER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_AERO), intent(inout) :: aero
      logical, intent(in), optional :: nul

      logical :: nul_tmp
      
      integer :: MSP_iostat
      
      MSP_iostat = 0

      if ( present (nul) ) then
         nul_tmp = nul
      else
         nul_tmp = .false.
      endif
        
      aero%ficaero=""

      aero%type_coef = 0 
      aero%type_variation = 0
      
      aero%cmf = 0._PM_REEL
      
      aero%sref = 0._PM_REEL
      aero%lref = 0._PM_REEL
      
      aero%xref = 0._PM_REEL
      aero%yref = 0._PM_REEL
      aero%zref = 0._PM_REEL


      if ( nul_tmp ) then

        nullify(aero%cxa)
        nullify(aero%cy)
        nullify(aero%czn)
        nullify(aero%cm0)
        nullify(aero%cn0)
        nullify(aero%cl0)
        nullify(aero%cmq)
        nullify(aero%cnr)
        nullify(aero%cnp)
        nullify(aero%clr)
        nullify(aero%clp)
        
     else

        ! -- Coefficient cxa
        if ( associated(aero%cxa)) then
           call MSP_effacer_coef(aero%cxa)
           deallocate(aero%cxa,stat=MSP_iostat)
        end if

        ! -- Coefficient cy
        if ( associated(aero%cy)) then
           call MSP_effacer_coef(aero%cy)
           deallocate(aero%cy,stat=MSP_iostat)
        end if

        ! -- Coefficient czn
        if ( associated(aero%czn)) then
           call MSP_effacer_coef(aero%czn)
           deallocate(aero%czn,stat=MSP_iostat)
        end if

        ! -- Coefficient cm0
        if ( associated(aero%cm0)) then
           call MSP_effacer_coef(aero%cm0)
           deallocate(aero%cm0,stat=MSP_iostat)
        end if

        ! -- Coefficient cn0
        if ( associated(aero%cn0)) then
           call MSP_effacer_coef(aero%cn0)
           deallocate(aero%cn0,stat=MSP_iostat)
        end if

        ! -- Coefficient cl0
        if ( associated(aero%cl0)) then
           call MSP_effacer_coef(aero%cl0)
           deallocate(aero%cl0,stat=MSP_iostat)
        end if

        ! -- Coefficient cmq
        if ( associated(aero%cmq)) then
           call MSP_effacer_coef(aero%cmq)
           deallocate(aero%cmq,stat=MSP_iostat)
        end if

        ! -- Coefficient cnr
        if ( associated(aero%cnr)) then
           call MSP_effacer_coef(aero%cnr)
           deallocate(aero%cnr,stat=MSP_iostat)
        end if

        ! -- Coefficient cnp
        if ( associated(aero%cnp)) then
           call MSP_effacer_coef(aero%cnp)
           deallocate(aero%cnp,stat=MSP_iostat)
        end if

        ! -- Coefficient clr
        if ( associated(aero%clr)) then
           call MSP_effacer_coef(aero%clr)
           deallocate(aero%clr,stat=MSP_iostat)
        end if

        ! -- Coefficient clp
        if ( associated(aero%clp)) then
           call MSP_effacer_coef(aero%clp)
           deallocate(aero%clp,stat=MSP_iostat)
        end if

      endif

    end subroutine MSP_effacer_aero


   function MSP_creer_aero (type_coef,type_variation,ficaero,cmf,sref,lref,xref,yref,zref,cxa,cy,czn,&
                               cm0,cn0,cl0,cmq,cnp,cnr,clp,clr) result (aero)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_aero
!
!$Resume
!  Création d'une structure contenant les caractéristiques aérodynamiques d'un véhicule.
!
!$Description
!  Création d'une structure contenant les caractéristiques aérodynamiques d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  aero = MSP_creer_aero (type_coef,type_variation,[ficaero],[cmf],[sref],[lref],[xref],[yref],[zref],[cxa],[cy],[czn],&
!.                                   [cm0],[cn0],[cl0],[cmq],[cnp],[cnr],[clp],[clr])
!.    integer :: type_coef
!.    integer :: type_variation
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type(MSP_COEF) :: cxa
!.    type(MSP_COEF) :: cy
!.    type(MSP_COEF) :: czn
!.    type(MSP_COEF) :: cm0
!.    type(MSP_COEF) :: cn0
!.    type(MSP_COEF) :: cl0
!.    type(MSP_COEF) :: cmq
!.    type(MSP_COEF) :: cnp
!.    type(MSP_COEF) :: cnr
!.    type(MSP_COEF) :: clp
!.    type(MSP_COEF) :: clr
!.    type(MSP_AERO) :: aero
!
!$Arguments
!>E     type_coef       :<integer>    type de coefficient (MSP_ENUM_COEFF_AERO_VITESSE ou MSP_ENUM_COEFF_AERO_VEHICULE)
!                                             [par défaut MSP_ENUM_COEFF_AERO_VEHICULE] ; /
!>E     type_variation  :<integer>    type de variation des coefficients /!\ paramètre OBLIGATOIRE
!>[E]   ficaero         :<LEN=*>      nom du fichier
!>[E]   cmf             :<pm_reel>    coefficient multiplicatif sur la force [par défaut 1.]
!>[E]   sref            :<pm_reel>    surface de référence [m^2] [par défaut 1.]
!>[E]   lref            :<pm_reel>    longueur de référence [m] [par défaut 1.]
!>[E]   xref            :<pm_reel>    centrage en X de référence (%lref) [par défaut 0.]
!>[E]   yref            :<pm_reel>    centrage en Y de référence (%lref) [par défaut 0.]
!>[E]   zref            :<pm_reel>    centrage en Z de référence (%lref) [par défaut 0.]
!>[E]   cxa             :<MSP_COEF>   structure coefficient de trainee
!>[E]   cy              :<MSP_COEF>   structure coefficient cy
!>[E]   czn             :<MSP_COEF>   structure coefficient de portance
!>[E]   cm0             :<MSP_COEF>   structure coefficient de tangage
!>[E]   cn0             :<MSP_COEF>   structure coefficient de lacet
!>[E]   cl0             :<MSP_COEF>   structure coefficient de roulis
!>[E]   cmq             :<MSP_COEF>   structure coefficient dynamique de tangage
!>[E]   cnp             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E]   cnr             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E]   clp             :<MSP_COEF>   structure coefficient dynamique de roulis
!>[E]   clr             :<MSP_COEF>   structure coefficient dynamique de roulis
!>S     aero            :<MSP_AERO>   structure contenant les caractéristiques 
!                                             aérodynamiques d'un véhicule
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- MSP_signaler_message
!- MSP_lire_str_aero
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
!  VEHICULE AERODYNAMIQUE CREER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use MSP_ACCES
      implicit none

      !-- Arguments
      
      !-- Arguments obligatoires, car on veut forcer l'utilisation
      ! de ces deux champs, qui permettent de décrire comment sont utilisés
      ! les coefs aero (repère aérodynamique, ou repère véhicule) et 
      ! en fonction de quel(s) paramètre(s) ils varient

      integer, intent(IN)             :: type_coef
      integer, intent(IN)             :: type_variation
      
      !-- Arguments optionnels : l'utilisation du ficaero prime sur les infos type coef et type de variation
      !  --> les valeurs seront donc prises dans le fichier AERO
      
      character(LEN=*), intent(IN), optional    :: ficaero

      real (KIND=pm_reel), intent(IN), optional :: cmf


      real (KIND=pm_reel), intent(IN), optional :: sref,lref
      real (KIND=pm_reel), intent(IN), optional :: xref,yref,zref

      type(MSP_COEF), intent(IN) , optional      :: cxa
      type(MSP_COEF), intent(IN) , optional      :: cy
      type(MSP_COEF), intent(IN) , optional      :: czn
      type(MSP_COEF), intent(IN) , optional      :: cm0
      type(MSP_COEF), intent(IN) , optional      :: cn0
      type(MSP_COEF), intent(IN) , optional      :: cl0
      type(MSP_COEF), intent(IN) , optional      :: cmq
      type(MSP_COEF), intent(IN) , optional      :: cnp
      type(MSP_COEF), intent(IN) , optional      :: cnr
      type(MSP_COEF), intent(IN) , optional      :: clp
      type(MSP_COEF), intent(IN) , optional      :: clr


      type(MSP_AERO) :: aero

      !-- Variables locales
      ! zone d'acces madona
      integer :: acc,ier

      ! valeurs des coefs tabules
      real(kind=PM_REEL),dimension(:),pointer::c_1d => NULL(), par1_c => NULL()
      character(len=MSP_LONG_CHAINE),dimension(:),pointer::com_par_c => NULL()
      integer :: dim_c

      ! La structure aero est créée par une fonction
      ! Pour une création, on demande le nullify sur les pointeurs

      call MSP_effacer_aero(aero,nul=.true.)


      aero%flag_func = .true.

      ! Affectation du type de coefficients 
      ! -> coefs aéro/vitesse, coefs véhicule
      ! OU coefs tabulés en altitude (cas particulier)
      aero%type_coef = type_coef
      
      aero%type_variation = type_variation
      
      if ( present(cmf) ) then
         aero%cmf = cmf
      else
         aero%cmf = 1._pm_reel
      endif

      if ( present(ficaero) ) then
         aero%ficaero = ficaero
         ier = MSP_acc_charger_donnees (ficaero,acc)
         ! DM1058 - Gestion d'erreur
         if (ier < 0) then
            call MSP_signaler_message (cle_mes="MSP_chargement_donnees", &
                 routine="MSP_creer_aero",partie_variable=trim(ficaero))
         endif
         call MSP_lire_str_aero (acc,aero)

         ! Le fichier a été lu, son accès peut être libéré (AQ / DM-ID 1113)
         call MSP_fermeture_MADONA(acc)

         ! MSP_lire_str_aero met le flag_func à true et on ne veut pas :
         aero%flag_func = .true.
         aero%ficaero = ficaero
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_creer_aero",type=MSP_ENUM_ERREUR)
         endif
         if ( present(cmf) ) then
            aero%cmf = cmf
         else
            aero%cmf = 1._pm_reel
         endif
         return
      endif

      if ( present(sref) ) then
         aero%sref = sref
      else
         aero%sref = 1._pm_reel
      endif

      if ( present(lref) ) then
         aero%lref = lref
      else
         aero%lref = 1._pm_reel
      endif

      if ( present(xref) ) then
         aero%xref = xref
      else
         aero%xref = 0._pm_reel
      endif

      if ( present(yref) ) then
         aero%yref = yref
      else
         aero%yref = 0._pm_reel
      endif

      if ( present(zref) ) then
         aero%zref = zref
      else
         aero%zref = 0._pm_reel
      endif

      ! Cas particulier d'une tabulation en altitude façon PSIMU:

      if ( aero%type_coef == MSP_ENUM_CF_TABULE_ALT_MOY ) then
         
         ! Valeurs constantes utilisées dans PSIMU
         dim_c = 1
         ALLOCATE(c_1d(MSP_NB_POINTS_TABULES))
         ALLOCATE(par1_c(MSP_NB_POINTS_TABULES))
         ALLOCATE(com_par_c(1))
         c_1d(:) = (/ 2.14_pm_reel , 2.14_pm_reel , 2.14_pm_reel , 2.14_pm_reel , 2.14_pm_reel , &
              2.15_pm_reel , 2.15_pm_reel , 2.16_pm_reel , 2.16_pm_reel , 2.16_pm_reel , &
              2.17_pm_reel , 2.18_pm_reel , 2.18_pm_reel , 2.19_pm_reel , 2.20_pm_reel , &
              2.21_pm_reel , 2.22_pm_reel , 2.23_pm_reel , 2.24_pm_reel , 2.26_pm_reel , &
              2.28_pm_reel , 2.33_pm_reel , 2.40_pm_reel , 2.43_pm_reel , 2.50_pm_reel , &
              2.52_pm_reel , 2.60_pm_reel , 2.60_pm_reel , 2.61_pm_reel , 2.63_pm_reel , &
              2.65_pm_reel , 2.66_pm_reel , 2.68_pm_reel , 2.69_pm_reel , 2.70_pm_reel , &
              2.71_pm_reel , 2.72_pm_reel , 2.72_pm_reel , 2.73_pm_reel , 2.73_pm_reel , &
              2.74_pm_reel , 2.74_pm_reel , 2.75_pm_reel , 2.75_pm_reel , 2.75_pm_reel /)
         par1_c(:) = (/ 120000._pm_reel , 140000._pm_reel , 160000._pm_reel , 180000._pm_reel , 200000._pm_reel , &
              220000._pm_reel , 240000._pm_reel , 260000._pm_reel , 280000._pm_reel , 300000._pm_reel , &
              320000._pm_reel , 340000._pm_reel , 360000._pm_reel , 380000._pm_reel , 400000._pm_reel , &
              420000._pm_reel , 440000._pm_reel , 460000._pm_reel , 480000._pm_reel , 500000._pm_reel , &
              520000._pm_reel , 540000._pm_reel , 560000._pm_reel , 580000._pm_reel , 600000._pm_reel , &
              620000._pm_reel , 640000._pm_reel , 660000._pm_reel , 680000._pm_reel , 700000._pm_reel , &
              720000._pm_reel , 740000._pm_reel , 760000._pm_reel , 780000._pm_reel , 800000._pm_reel , &
              820000._pm_reel , 840000._pm_reel , 860000._pm_reel , 880000._pm_reel , 900000._pm_reel , &
              920000._pm_reel , 940000._pm_reel , 960000._pm_reel , 980000._pm_reel , 99.e+99_pm_reel /)
         com_par_c(1) = "ALTITUDE"
         allocate(aero%cxa)
         aero%cxa=MSP_creer_coef(dim_coef=dim_c,com_par_coef=com_par_c,&
              par1_coef=par1_c,coef_1d=c_1d,nom_coef="Cd")

         deallocate(c_1d)
         deallocate(par1_c)
         deallocate(com_par_c)

      else

         ! Tableaux de coefficients :

         ! -- Coefficient cxa
         if ( present(cxa) ) then
            allocate(aero%cxa)
            aero%cxa=cxa
         endif

         ! -- Coefficient cy
         if ( present(cy) ) then
            allocate(aero%cy)
            aero%cy=cy
         endif

         ! -- Coefficient czn
         if ( present(czn) ) then
            allocate(aero%czn)
            aero%czn=czn
         endif

         ! -- Coefficient cm0
         if ( present(cm0) ) then
            allocate(aero%cm0)
            aero%cm0=cm0
         endif

         ! -- Coefficient cn0
         if ( present(cn0) ) then
            allocate(aero%cn0)
            aero%cn0=cn0
         endif

         ! -- Coefficient cl0
         if ( present(cl0) ) then
            allocate(aero%cl0)
            aero%cl0=cl0
         endif

         ! -- Coefficient cmq
         if ( present(cmq) ) then
            allocate(aero%cmq)
            aero%cmq=cmq
         endif

         ! -- Coefficient cnp
         if ( present(cnp) ) then
            allocate(aero%cnp)
            aero%cnp=cnp
         endif

         ! -- Coefficient cnr
         if ( present(cnr) ) then
            allocate(aero%cnr)
            aero%cnr=cnr
         endif

         ! -- Coefficient clp
         if ( present(clp) ) then
            allocate(aero%clp)
            aero%clp=clp
         endif

         ! -- Coefficient clr
         if ( present(clr) ) then
            allocate(aero%clr)
            aero%clr=clr
         endif
      end if


   end function MSP_creer_aero


   subroutine MSP_lire_str_aero (acc,aero)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_str_aero
!
!$Resume
!  Lecture des données relatives à l'aérodynamique d'un véhicule.
!
!$Description
!  Lecture dans une zone de moyen d'accès MADONA des données relatives
!  à l'aérodynamique d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_str_aero (acc,aero)
!.    integer :: acc
!.    type(MSP_AERO) :: aero
!
!$Arguments
!>E     acc   :<integer>    numéro identificateur de la zone de moyen d'accès MADONA
!>S     aero  :<MSP_AERO>   structure contenant les données relatives à l'aérodynamique d'un véhicule
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- MSP_signaler_message
!- MSP_acc_get_val
!- MSP_lire_str_coef
!- MSP_consulter_coef
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE AERODYNAMIQUE LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      type(MSP_AERO), intent(OUT) :: aero

! Variables locales
      integer :: ier,iexist
      character(LEN=MSP_LONG_CHAINE) :: label_type_coef
      character(LEN=MSP_LONG_CHAINE), pointer, dimension(:) :: com_par_coef => NULL()
      call MSP_effacer_aero(aero,nul=.true.)


      aero%flag_func = .false.


      ier = acc_select (acc,"AERO",ACC_STRUCT)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure AERO', &
                                    routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture de la surface de référence:

      call MSP_acc_get_val(acc,"sref",aero%sref,unit="m^2")
      if (MSP_gen_messages("MSP_lire_str_aero")) return

      ! Lecture de la longueur de référence:

      call MSP_acc_get_val(acc,"lref",aero%lref,unit="m")
      if (MSP_gen_messages("MSP_lire_str_aero")) return

      ! Lecture des centrages de référence:

      call MSP_acc_get_val(acc,"xref",aero%xref,unit="")
      if (MSP_gen_messages("MSP_lire_str_aero")) return

      call MSP_acc_get_val(acc,"yref",aero%yref,unit="")
      if (MSP_gen_messages("MSP_lire_str_aero")) return

      call MSP_acc_get_val(acc,"zref",aero%zref,unit="")
      if (MSP_gen_messages("MSP_lire_str_aero")) return      

      ! Types de coefficients (axes véhicule ou axes vitesse):

      call MSP_acc_get_val(acc,"type_coef",label_type_coef)
      if (MSP_gen_messages("MSP_lire_str_aero")) return  

      ! Lecture des CXA / CZN:

      select case (label_type_coef)

         case ("vitesse")

            aero%type_coef = MSP_ENUM_COEFF_AERO_VITESSE
            allocate(aero%cxa)
            call MSP_lire_str_coef (acc,"cx",aero%cxa)
            if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
            endif
            allocate(aero%czn)
            call MSP_lire_str_coef (acc,"cz",aero%czn)
            if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
            endif

         case ("vehicule")

            aero%type_coef = MSP_ENUM_COEFF_AERO_VEHICULE
            allocate(aero%cxa)
            call MSP_lire_str_coef (acc,"ca",aero%cxa)
            if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
            endif
            allocate(aero%czn)
            call MSP_lire_str_coef (acc,"cn",aero%czn)
            if ( MSP_ERREUR ) then
               call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
            endif

         case default

            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du type de coefficients aéros (inconnu)', &
                                       routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
            return

      end select

      ! DM-ID 659 : on gère deux types de variation pour le moment (altitude, ou incidence/mach)
      ! Le type MSP_ENUM_AUTRE_VARIATION recouvre les coefs variant en fonction des autres paramètres
      ! gérés par la MECASPA
      call MSP_consulter_coef(aero%cxa, com_par_coef = com_par_coef)
      select case (com_par_coef(1))
         case ("INCIDENCE")
            aero%type_variation = MSP_ENUM_INCIDENCE_MACH
         case ("ALTITUDE")
            aero%type_variation = MSP_ENUM_ALTITUDE
         case default
            aero%type_variation = MSP_ENUM_AUTRE_VARIATION
      end select
      
      if (associated(com_par_coef)) deallocate (com_par_coef)

      ! Lecture des CY:
      iexist = acc_exist (acc,"cy")
      if (iexist == 1) then
         allocate(aero%cy)
         call MSP_lire_str_coef (acc,"cy",aero%cy)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CM0:
      iexist = acc_exist (acc,"cm0")
      if (iexist == 1) then
         allocate(aero%cm0)
         call MSP_lire_str_coef (acc,"cm0",aero%cm0)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CL0:
      iexist = acc_exist (acc,"cl0")
      if (iexist == 1) then
         allocate(aero%cl0)
         call MSP_lire_str_coef (acc,"cl0",aero%cl0)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CN0:
      iexist = acc_exist (acc,"cn0")
      if (iexist == 1) then
         allocate(aero%cn0)
         call MSP_lire_str_coef (acc,"cn0",aero%cn0)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CMQ:
      iexist = acc_exist (acc,"cmq")
      if (iexist == 1) then
         allocate(aero%cmq)
         call MSP_lire_str_coef (acc,"cmq",aero%cmq)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CNP:
      iexist = acc_exist (acc,"cnp")
      if (iexist == 1) then
         allocate(aero%cnp)
         call MSP_lire_str_coef (acc,"cnp",aero%cnp)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CNR:
      iexist = acc_exist (acc,"cnr")
      if (iexist == 1) then
         allocate(aero%cnr)
         call MSP_lire_str_coef (acc,"cnr",aero%cnr)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CLP:
      iexist = acc_exist (acc,"clp")
      if (iexist == 1) then
         allocate(aero%clp)
         call MSP_lire_str_coef (acc,"clp",aero%clp)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if
      ! Lecture des CLR:
      iexist = acc_exist (acc,"clr")
      if (iexist == 1) then
         allocate(aero%clr)
         call MSP_lire_str_coef (acc,"clr",aero%clr)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         endif
      end if

      ! Fin de la lecture des coefficients aérodynamiques !

      ier = acc_select_end (acc)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure AERO', &
                                    routine="MSP_lire_str_aero",type=MSP_ENUM_ERREUR)
         return
      endif

   end subroutine MSP_lire_str_aero

   SUBROUTINE MSP_consulter_aero (aero, ficaero, cmf, type_coef, type_variation, sref, lref, xref, yref, zref, &
                               cxa,cy,czn,cm0,cn0,cl0,cmq,cnp,cnr,clp,clr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_aero
!
!$Resume
!	Routine permettant de consulter une structure aero
!
!$Description
!	Routine permettant de consulter une structure aero
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_aero (aero, [ficaero], [cmf], [type_coef], [type_variation], [sref], [lref], [xref], [yref], [zref], &
!.                                   [cxa],[cy],[czn],[cm0],[cn0],[cl0],[cmq],[cnp],[cnr],[clp],[clr])
!.    type(MSP_AERO) :: aero
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    integer :: type_coef
!.    integer :: type_variation
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type (MSP_COEF) :: cxa
!.    type (MSP_COEF) :: cy
!.    type (MSP_COEF) :: czn
!.    type (MSP_COEF) :: cm0
!.    type (MSP_COEF) :: cn0
!.    type (MSP_COEF) :: cl0
!.    type (MSP_COEF) :: cnp
!.    type (MSP_COEF) :: cnr
!.    type (MSP_COEF) :: cmq
!.    type (MSP_COEF) :: clp
!.    type (MSP_COEF) :: clr
!
!$Arguments
!>E     aero            :<MSP_AERO>   structure a consulter
!>[S]   ficaero         :<LEN=*>      nom du fichier madona associe a la struture
!>[S]   cmf             :<pm_reel>    coefficient multiplicatif sur la force
!>[S]   type_coef       :<integer>    type de coefficient aero (MSP_ENUM_COEFF_AERO_VITESSE/MSP_ENUM_COEFF_AERO_VEHICULE)
!>[S]   type_variation  :<integer>    type de variation des coefs : MSP_ENUM_ALTITUDE, MSP_ENUM_CONSTANT, .. 
!>[S]   sref            :<pm_reel>    surface de reference [m^2]
!>[S]   lref            :<pm_reel>    longueur de référence [m]
!>[S]   xref            :<pm_reel>    centrage en X de référence (%lref)
!>[S]   yref            :<pm_reel>    centrage en Y de référence (%lref)
!>[S]   zref            :<pm_reel>    centrage en Z de référence (%lref)
!>[E/S] cxa             :<MSP_COEF>   structure coefficient de trainee
!>[E/S] cy              :<MSP_COEF>   struturce coefficient cy
!>[E/S] czn             :<MSP_COEF>   structure coefficient de portance
!>[E/S] cm0             :<MSP_COEF>   structure coefficient de tangage
!>[E/S] cn0             :<MSP_COEF>   structure coefficient de lacet
!>[E/S] cl0             :<MSP_COEF>   structure coefficient de roulis
!>[E/S] cmq             :<MSP_COEF>   structure coefficient dynamique de tangage
!>[E/S] cnp             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E/S] cnr             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E/S] clp             :<MSP_COEF>   structure coefficient dynamique de roulis
!>[E/S] clr             :<MSP_COEF>   structure coefficient dynamique de roulis
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

      type(MSP_AERO), intent(IN)  :: aero

      character(LEN=*), intent(OUT), optional    :: ficaero

      real (KIND=pm_reel), intent(OUT), optional :: cmf
      integer, intent(OUT), optional             :: type_coef
      integer, intent(OUT), optional             :: type_variation

      real (KIND=pm_reel), intent(OUT), optional :: sref,lref
      real (KIND=pm_reel), intent(OUT), optional :: xref,yref,zref

      type (MSP_COEF),optional,intent(INOUT):: cxa
      type (MSP_COEF),optional,intent(INOUT):: cy
      type (MSP_COEF),optional,intent(INOUT):: czn
      type (MSP_COEF),optional,intent(INOUT):: cm0
      type (MSP_COEF),optional,intent(INOUT):: cn0
      type (MSP_COEF),optional,intent(INOUT):: cl0
      type (MSP_COEF),optional,intent(INOUT):: cnp
      type (MSP_COEF),optional,intent(INOUT):: cnr
      type (MSP_COEF),optional,intent(INOUT):: cmq
      type (MSP_COEF),optional,intent(INOUT):: clp
      type (MSP_COEF),optional,intent(INOUT):: clr

      if ( present(cmf) )       cmf = aero%cmf
      if ( present(ficaero) )   ficaero = aero%ficaero
      if ( present(type_coef) ) type_coef = aero%type_coef
      if ( present(type_variation) ) type_variation = aero%type_variation
      if ( present(sref) )      sref = aero%sref
      if ( present(lref) )      lref = aero%lref
      if ( present(xref) )      xref = aero%xref
      if ( present(yref) )      yref = aero%yref
      if ( present(zref) )      zref = aero%zref


      ! -- Coefficient cxa
      if ( present(cxa) ) then
         if ( associated(aero%cxa)) then
            cxa = aero%cxa
         else
            cxa=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cy
      if ( present(cy) ) then
         if ( associated(aero%cy)) then
            cy = aero%cy
         else
            cy=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient czn
      if ( present(czn) ) then
         if ( associated(aero%czn)) then
            czn = aero%czn
         else
            czn=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cm0
      if ( present(cm0) ) then
         if ( associated(aero%cm0)) then
            cm0 = aero%cm0
         else
            cm0=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cl0
      if ( present(cl0) ) then
         if ( associated(aero%cl0)) then
            cl0 = aero%cl0
         else
            cl0=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cn0
      if ( present(cn0) ) then
         if ( associated(aero%cn0)) then
            cn0 = aero%cn0
         else
            cn0=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cmq
      if ( present(cmq) ) then
         if ( associated(aero%cmq)) then
            cmq = aero%cmq
         else
            cmq=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cnp
      if ( present(cnp) ) then
         if ( associated(aero%cnp)) then
            cnp = aero%cnp
         else
            cnp=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient cnr
      if ( present(cnr) ) then
         if ( associated(aero%cnr)) then
            cnr = aero%cnr
         else
            cnr=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient clp
      if ( present(clp) ) then
         if ( associated(aero%clp)) then
            clp = aero%clp
         else
            clp=MSP_Creer_coef()
         end if
      end if

      ! -- Coefficient clr
      if ( present(clr) ) then
         if ( associated(aero%clr)) then
            clr = aero%clr
         else
            clr=MSP_Creer_coef()
         end if
      end if

      
    end SUBROUTINE MSP_consulter_aero
    
    
   SUBROUTINE MSP_modifier_aero (aero, ficaero, cmf, type_coef, type_variation, sref, lref, xref, yref, zref, &
                               cxa,cy,czn,cm0,cn0,cl0,cmq,cnp,cnr,clp,clr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_aero
!
!$Resume
!	Routine permetant de modofier une structure aero
!
!$Description
!	Routine permetant de modofier une structure aero
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_aero (aero, [ficaero], [cmf], [type_coef], [type_variation], [sref], [lref], [xref], [yref], [zref], &
!.                                   [cxa],[cy],[czn],[cm0],[cn0],[cl0],[cmq],[cnp],[cnr],[clp],[clr])
!.    type(MSP_AERO) :: aero
!.    character(LEN=*) :: ficaero
!.    real (KIND=pm_reel) :: cmf
!.    integer :: type_coef
!.    integer :: type_variation
!.    real (KIND=pm_reel) :: sref,lref
!.    real (KIND=pm_reel) :: xref,yref,zref
!.    type (MSP_COEF) :: cxa
!.    type (MSP_COEF) :: cy
!.    type (MSP_COEF) :: czn
!.    type (MSP_COEF) :: cm0
!.    type (MSP_COEF) :: cn0
!.    type (MSP_COEF) :: cl0
!.    type (MSP_COEF) :: cnp
!.    type (MSP_COEF) :: cnr
!.    type (MSP_COEF) :: cmq
!.    type (MSP_COEF) :: clp
!.    type (MSP_COEF) :: clr
!
!$Arguments
!>E/S   aero            :<MSP_AERO>   structure a modifier
!>[E]   ficaero         :<LEN=*>      nom du fichier decrivant la structure aero ( dans ce cas la structure sera mise a jour a partir des info du fichier)
!>[E]   cmf             :<pm_reel>    coefficient multiplicatif sur la force 
!>[E]   type_coef       :<integer>    type de coefficient (MSP_ENUM_COEFF_AERO_VITESSE/MSP_ENUM_COEFF_AERO_VEHICULE)
!>[E]   type_variation  :<integer>    type de variation des coefficients : MSP_ENUM_ALTITUDE, MSP_ENUM_CONSTANT, MSP_ENUM_INCIDENCE_MACH ..
!>[E]   sref            :<pm_reel>    surface de référence [m^2]
!>[E]   lref            :<pm_reel>    longueur de référence [m]
!>[E]   xref            :<pm_reel>    centrage en X de référence (%lref)
!>[E]   yref            :<pm_reel>    centrage en Y de référence (%lref)
!>[E]   zref            :<pm_reel>    centrage en Z de référence (%lref)
!>[E]   cxa             :<MSP_COEF>   structure coefficient de trainee
!>[E]   cy              :<MSP_COEF>   struturce coefficient cy
!>[E]   czn             :<MSP_COEF>   structure coefficient de portance
!>[E]   cm0             :<MSP_COEF>   structure coefficient de tangage
!>[E]   cn0             :<MSP_COEF>   structure coefficient de lacet
!>[E]   cl0             :<MSP_COEF>   structure coefficient de roulis
!>[E]   cmq             :<MSP_COEF>   structure coefficient dynamique de tangage
!>[E]   cnp             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E]   cnr             :<MSP_COEF>   structure coefficient dynamique de lacet
!>[E]   clp             :<MSP_COEF>   structure coefficient dynamique de roulis
!>[E]   clr             :<MSP_COEF>   structure coefficient dynamique de roulis
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- MSP_signaler_message
!- MSP_lire_str_aero
!- MSP_effacer_coef
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

      type(MSP_AERO), intent(INOUT)  :: aero

      character(LEN=*), intent(IN), optional    :: ficaero

      real (KIND=pm_reel), intent(IN), optional :: cmf
      integer, intent(IN), optional             :: type_coef
      integer, intent(IN), optional             :: type_variation
      real (KIND=pm_reel), intent(IN), optional :: sref,lref
      real (KIND=pm_reel), intent(IN), optional :: xref,yref,zref

      type (MSP_COEF),optional,intent(IN):: cxa
      type (MSP_COEF),optional,intent(IN):: cy
      type (MSP_COEF),optional,intent(IN):: czn
      type (MSP_COEF),optional,intent(IN):: cm0
      type (MSP_COEF),optional,intent(IN):: cn0
      type (MSP_COEF),optional,intent(IN):: cl0
      type (MSP_COEF),optional,intent(IN):: cnp
      type (MSP_COEF),optional,intent(IN):: cnr
      type (MSP_COEF),optional,intent(IN):: cmq
      type (MSP_COEF),optional,intent(IN):: clp
      type (MSP_COEF),optional,intent(IN):: clr


      integer::acc,ier

      if ( present(ficaero) ) then 
         aero%ficaero = ficaero
         call MSP_effacer_aero(aero)
         ier = MSP_acc_charger_donnees (ficaero,acc)
         ! DM1058 - Gestion d'erreur
         if (ier < 0) then
            call MSP_signaler_message (cle_mes="MSP_chargement_donnees", &
                 routine="MSP_modifier_aero",partie_variable=trim(ficaero))
         endif
     
         call MSP_lire_str_aero (acc,aero)

         call MSP_fermeture_MADONA(acc)

         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_modifier_aero",type=MSP_ENUM_ERREUR)
         endif
         return
      end if
 
      if ( present(cmf) )       aero%cmf = cmf
      if ( present(type_coef) ) aero%type_coef = type_coef
      if ( present(type_variation) ) aero%type_variation = type_variation
      if ( present(sref) )      aero%sref = sref
      if ( present(lref) )      aero%lref = lref
      if ( present(xref) )      aero%xref = xref
      if ( present(yref) )      aero%yref = yref
      if ( present(zref) )      aero%zref = zref

      ! -- Coeffcient cxa
      if ( present(cxa) ) then
         if ( .not.associated(aero%cxa)) then
            allocate(aero%cxa)
         else
            call MSP_effacer_coef(aero%cxa)
         end if
         aero%cxa = cxa
      end if

      ! -- Coeffcient cy
      if ( present(cy) ) then
         if ( .not.associated(aero%cy)) then
            allocate(aero%cy)
         else
            call MSP_effacer_coef(aero%cy)
         end if
         aero%cy = cy
      end if

      ! -- Coeffcient czn
      if ( present(czn) ) then
         if ( .not.associated(aero%czn)) then
            allocate(aero%czn)
         else
            call MSP_effacer_coef(aero%czn)
         end if
         aero%czn = czn
      end if

      ! -- Coeffcient cm0
      if ( present(cm0) ) then
         if ( .not.associated(aero%cm0)) then
            allocate(aero%cm0)
         else
            call MSP_effacer_coef(aero%cm0)
         end if
         aero%cm0 = cm0
      end if

      ! -- Coeffcient cn0
      if ( present(cn0) ) then
         if ( .not.associated(aero%cn0)) then
            allocate(aero%cn0)
         else
            call MSP_effacer_coef(aero%cn0)
         end if
         aero%cn0 = cn0
      end if

      ! -- Coeffcient cl0
      if ( present(cl0) ) then
         if ( .not.associated(aero%cl0)) then
            allocate(aero%cl0)
         else
            call MSP_effacer_coef(aero%cl0)
         end if
         aero%cl0 = cl0
      end if

      ! -- Coeffcient cmq
      if ( present(cmq) ) then
         if ( .not.associated(aero%cmq)) then
            allocate(aero%cmq)
         else
            call MSP_effacer_coef(aero%cmq)
         end if
         aero%cmq = cmq
      end if

      ! -- Coeffcient cnp
      if ( present(cnp) ) then
         if ( .not.associated(aero%cnp)) then
            allocate(aero%cnp)
         end if
         aero%cnp = cnp
      end if

      ! -- Coeffcient cnr
      if ( present(cnr) ) then
         if ( .not.associated(aero%cnr)) then
            allocate(aero%cnr)
         else
            call MSP_effacer_coef(aero%cnr)
         end if
         aero%cnr = cnr
      end if

      ! -- Coeffcient clp
      if ( present(clp) ) then
         if ( .not.associated(aero%clp)) then
            allocate(aero%clp)
         else
            call MSP_effacer_coef(aero%clp)
         end if
         aero%clp = clp
      end if

      ! -- Coeffcient clr
      if ( present(clr) ) then
         if ( .not.associated(aero%clr)) then
            allocate(aero%clr)
         else
            call MSP_effacer_coef(aero%clr)
         end if
         aero%clr = clr
      end if
           
    end SUBROUTINE MSP_modifier_aero
    
    

     subroutine MSP_afficher_aero (aero,ilog)
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_aero
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
!  call MSP_afficher_aero (aero,[ilog])
!.    type(MSP_AERO) :: aero
!.    integer :: ilog
!
!$Arguments
!>E     aero  :<MSP_AERO>   
!>[E/S] ilog  :<integer>    Numéro de l'unité logique d'affichage
!
!$Common
!
!$Routines
!- MSP_afficher_coef
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_AERO), intent(IN) :: aero
      integer, optional :: ilog

      integer :: num

      if ( present(ilog) ) then
         num = ilog
      else
         num = 6
      endif

      write(num,'(a,g21.12)') "SREF:    ",aero%sref
      write(num,'(a,g21.12)') "LREF:    ",aero%lref
      write(num,'(a,g21.12)') "XREF:    ",aero%xref
      write(num,'(a,g21.12)') "YREF:    ",aero%yref
      write(num,'(a,g21.12)') "ZREF:    ",aero%zref

      write(num,'(a)') ''
      write(num,'(a,g21.12)') "CMF:     ",aero%cmf
      write(num,'(a,i9)') "TYPCF:   ",aero%type_coef
      write(num,'(a,i9)') "TYP VARIATION", aero%type_variation

      ! -- Coefficient cxa
      if ( associated(aero%cxa)) then
         call MSP_afficher_coef(aero%cxa,ilog=ilog)
      end if

      ! -- Coefficient cy
      if ( associated(aero%cy)) then
         call MSP_afficher_coef(aero%cy,ilog=ilog)
      end if

      ! -- Coefficient czn
      if ( associated(aero%czn)) then
         call MSP_afficher_coef(aero%czn,ilog=ilog)
      end if

      ! -- Coefficient cm0
      if ( associated(aero%cm0)) then
         call MSP_afficher_coef(aero%cm0,ilog=ilog)
      end if

      ! -- Coefficient cn0
      if ( associated(aero%cn0)) then
         call MSP_afficher_coef(aero%cn0,ilog=ilog)
      end if

      ! -- Coefficient cl0
      if ( associated(aero%cl0)) then
         call MSP_afficher_coef(aero%cl0,ilog=ilog)
      end if

      ! -- Coefficient cmq
      if ( associated(aero%cmq)) then
         call MSP_afficher_coef(aero%cmq,ilog=ilog)
      end if

      ! -- Coefficient cnp
      if ( associated(aero%cnp)) then
         call MSP_afficher_coef(aero%cnp,ilog=ilog)
      end if

      ! -- Coefficient cnr
      if ( associated(aero%cnr)) then
         call MSP_afficher_coef(aero%cnr,ilog=ilog)
      end if

      ! -- Coefficient clp
      if ( associated(aero%clp)) then
         call MSP_afficher_coef(aero%clp,ilog=ilog)
      end if

      ! -- Coefficient clr
      if ( associated(aero%clr)) then
         call MSP_afficher_coef(aero%clr,ilog=ilog)
      end if

      write(num,'(a)') ''

    end subroutine MSP_afficher_aero
    
    
    
  function MSP_interp_coef_vehicule(aero,tab_valcxa,tab_valcy,tab_valczn,alpha,beta,&
       indcxa_i,indcxa_j,indcxa_k,indcy_i,indcy_j,indcy_k,&
       indczn_i,indczn_j,indczn_k) result (cx_a)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_coef_vehicule
!
!$Resume
!	Routine permettant de calculer les valeur des coefficients aero (en axe vehicule)
!
!$Description
!	Routine permettant de calculer les valeur des coefficients aero, en axe vehicule (!).
!	Tous les coefs passes en axe vitesse seront donc convertis en axe vehicule (!!!). 
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  cx_a = MSP_interp_coef_vehicule(aero,tab_valcxa,tab_valcy,tab_valczn,alpha,beta,&
!.           [indcxa_i],[indcxa_j],[indcxa_k],&
!.           [indcy_i],[indcy_j],[indcy_k],&
!.           [indczn_i],[indczn_j],[indczn_k]&
!.          )
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcxa
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcy
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valczn
!.    real(kind=PM_REEL) :: alpha
!.    real(kind=PM_REEL) :: beta
!.    real(kind=PM_REEL),dimension(3) :: cx_a
!.    integer :: indcxa_i,indcxa_j,indcxa_k
!.    integer :: indcy_i,indcy_j,indcy_k
!.    integer :: indczn_i,indczn_j,indczn_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valcxa      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cx
!>E/S   tab_valcy       :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cy
!>E/S   tab_valczn      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cz
!>E     alpha           :<PM_REEL>                   incidence [rad]
!>E     beta            :<PM_REEL>                   derapage  [rad]
!>[E/S] indcxa_i        :<integer>                   indice du premier parametre pour cx
!>[E/S] indcxa_j        :<integer>                   indice du second parametre pour cx
!>[E/S] indcxa_k        :<integer>                   indice du troisieme parametre pour cx
!>[E/S] indcy_i         :<integer>                   indice du premier parametre pour cy
!>[E/S] indcy_j         :<integer>                   indice du second parametre pour cy
!>[E/S] indcy_k         :<integer>                   indice du troisieme parametre pour cy
!>[E/S] indczn_i        :<integer>                   indice du premier parametre pour cz
!>[E/S] indczn_j        :<integer>                   indice du second parametre pour cz
!>[E/S] indczn_k        :<integer>                   indice du troisieme parametre pour cz
!>S     cx_a            :<PM_REEL,DIM=(3)>           valeur de [cx,cy,cz] interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcxa
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcy
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valczn
    real(kind=PM_REEL),intent(in)             :: alpha
    real(kind=PM_REEL),intent(in)             :: beta
    real(kind=PM_REEL),dimension(3)           :: cx_a
    integer,optional,intent(inout)            :: indcxa_i,indcxa_j,indcxa_k
    integer,optional,intent(inout)            :: indcy_i,indcy_j,indcy_k
    integer,optional,intent(inout)            :: indczn_i,indczn_j,indczn_k

    ! Variables locales
 
    real(kind=PM_REEL) :: calpha,salpha,cbeta,sbeta
    real(kind=PM_REEL) :: cx,cy,cz

    
    cx = 0._pm_reel
    cy = 0._pm_reel
    cz = 0._pm_reel

    ! interpolation cx avion ou aero
    if ( associated(aero%cxa)) then
       cx = MSP_interp_coef(aero%cxa,tab_valcxa,indcoef_i=indcxa_i,&
            indcoef_j=indcxa_j,indcoef_k=indcxa_k)
       if (MSP_gen_messages("MSP_interp_coef_vehicule")) return
    else
       cx = 2._PM_REEL
    end if
    ! interpolation cy avion ou aero
    if ( associated(aero%cy)) then
       cy = MSP_interp_coef(aero%cy,tab_valcy,indcoef_i=indcy_i,&
            indcoef_j=indcy_j,indcoef_k=indcy_k)
       if (MSP_gen_messages("MSP_interp_coef_vehicule")) return
    else
       cy = 0._PM_REEL
    end if
    ! interpolation cz avion ou aero
    if ( associated(aero%czn)) then
       cz = MSP_interp_coef(aero%czn,tab_valczn,indcoef_i=indczn_i,&
            indcoef_j=indczn_j,indcoef_k=indczn_k)
       if (MSP_gen_messages("MSP_interp_coef_vehicule")) return
    else
       cz = 0._PM_REEL
    end if

    ! Calcul en fonction du type de coef
    if ( aero%type_coef == MSP_enum_coeff_aero_vitesse) then

       calpha = cos(alpha)
       salpha = sin(alpha)
       cbeta  = cos(beta)
       sbeta  = sin(beta)


       ! Calcul du cx
       cx_a(1) = calpha*cbeta*cx -calpha*sbeta*cy -salpha*cz

       ! Calcul du cy
       cx_a(2) = cbeta*cy + sbeta*cx

       ! Calcul du cz
       cx_a(3) = salpha*cbeta*cx -salpha*sbeta*cy +calpha*cz

    else
       cx_a(1) = cx

       ! interpolation du cy avion   
       
       cx_a(2) = cy

       ! Affectation du cz
       cx_a(3) = cz


       
    end if
  end function MSP_interp_coef_vehicule



  function MSP_interp_coef_aero(aero,tab_valcxa,tab_valcy,tab_valczn,alpha,beta,&
       indcxa_i,indcxa_j,indcxa_k,indcy_i,indcy_j,indcy_k,&
       indczn_i,indczn_j,indczn_k) result (cx_a)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_coef_aero
!
!$Resume
!	Routine permettant de calculer les valeur des coefficients aero (en axe aero)
!
!$Description
!	Routine permettant de calculer les valeur des coefficients aero, en axe aero(!).
!	Les coefs passe en axe vehicule seront convertis en axe vitesse.
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  cx_a = MSP_interp_coef_aero(aero,tab_valcxa,tab_valcy,tab_valczn,alpha,beta,&
!.           [indcxa_i],[indcxa_j],[indcxa_k],&
!.           [indcy_i],[indcy_j],[indcy_k],&
!.           [indczn_i],[indczn_j],[indczn_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcxa
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcy
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valczn
!.    real(kind=PM_REEL) :: alpha
!.    real(kind=PM_REEL) :: beta
!.    real(kind=PM_REEL),dimension(3) :: cx_a
!.    integer :: indcxa_i,indcxa_j,indcxa_k
!.    integer :: indcy_i,indcy_j,indcy_k
!.    integer :: indczn_i,indczn_j,indczn_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valcxa      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cd
!>E/S   tab_valcy       :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cy
!>E/S   tab_valczn      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient cl
!>E     alpha           :<PM_REEL>                   incidence [rad]
!>E     beta            :<PM_REEL>                   derapage  [rad]
!>[E/S] indcxa_i        :<integer>                   indice du premier parametre pour cd
!>[E/S] indcxa_j        :<integer>                   indice du second parametre pour cd
!>[E/S] indcxa_k        :<integer>                   indice du troisieme parametre pour cd
!>[E/S] indcy_i         :<integer>                   indice du premier parametre pour cy
!>[E/S] indcy_j         :<integer>                   indice du second parametre pour cy
!>[E/S] indcy_k         :<integer>                   indice du troisieme parametre pour cy
!>[E/S] indczn_i        :<integer>                   indice du premier parametre pour cl
!>[E/S] indczn_j        :<integer>                   indice du second parametre pour cl
!>[E/S] indczn_k        :<integer>                   indice du troisieme parametre pour cl
!>S     cx_a            :<PM_REEL,DIM=(3)>           valeur de [cd,cy,cl] interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcxa
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcy
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valczn
    real(kind=PM_REEL),intent(in)             :: alpha
    real(kind=PM_REEL),intent(in)             :: beta
    real(kind=PM_REEL),dimension(3)           :: cx_a
    integer,optional,intent(inout)            :: indcxa_i,indcxa_j,indcxa_k
    integer,optional,intent(inout)            :: indcy_i,indcy_j,indcy_k
    integer,optional,intent(inout)            :: indczn_i,indczn_j,indczn_k

    ! Variables locales
    real(kind=PM_REEL) :: calpha,salpha,cbeta,sbeta
    real(kind=PM_REEL) :: cx,cy,cz



    cx = 0._pm_reel
    cy = 0._pm_reel
    cz = 0._pm_reel

    ! Début de la routine
    ! interpolation cx avion ou aero
    if ( associated(aero%cxa)) then
       cx = MSP_interp_coef(aero%cxa,tab_valcxa,indcoef_i=indcxa_i,&
            indcoef_j=indcxa_j,indcoef_k=indcxa_k)
       if (MSP_gen_messages("MSP_interp_coef_aero")) return
    else
       cx = 2._PM_REEL
    end if
    ! interpolation cy avion ou aero
    if ( associated(aero%cy)) then
       cy = MSP_interp_coef(aero%cy,tab_valcy,indcoef_i=indcy_i,&
            indcoef_j=indcy_j,indcoef_k=indcy_k)
       if (MSP_gen_messages("MSP_interp_coef_aero")) return
    else
       cy = 0._PM_REEL
    end if
    ! interpolation cz avion ou aero
    if ( associated(aero%czn)) then
       cz = MSP_interp_coef(aero%czn,tab_valczn,indcoef_i=indczn_i,&
            indcoef_j=indczn_j,indcoef_k=indczn_k)
       if (MSP_gen_messages("MSP_interp_coef_aero")) return
    else
       cz = 0._PM_REEL
    end if

    
    ! Calcul en fonction du type de coef
    if ( aero%type_coef == MSP_enum_coeff_aero_vehicule) then



       ! interpolation du cy avion   
       

       calpha = cos(alpha)
       salpha = sin(alpha)
       cbeta  = cos(beta)
       sbeta  = sin(beta)


       ! Calcul du cx
       cx_a(1) = calpha*cbeta*cx + sbeta*cy +salpha*cbeta*cz

       ! Calcul du cy
       cx_a(2) = -calpha*sbeta*cx +cbeta*cy - sbeta*salpha*cz

       ! Calcul du cz
       cx_a(3) =  -salpha*cx + calpha*cz


    else
       cx_a(1) = cx

       cx_a(2) = cy

       ! Affectation du cz
       cx_a(3) = cz

    end if


  end function MSP_interp_coef_aero


  function MSP_interp_cm0(aero,tab_valcl0,tab_valcm0,tab_valcn0,&
       indcm0_i,indcm0_j,indcm0_k,&
       indcn0_i,indcn0_j,indcn0_k,&
       indcl0_i,indcl0_j,indcl0_k) result (cm0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_cm0
!
!$Resume
!	Routine permettant de calculer les valeurs des coefficients de roulis, tangage et lacet.
!
!$Description
!	Routine permettant de calculer les valeurs des coefficients de roulis, tangage et lacet.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  cm0 = MSP_interp_cm0(aero,tab_valcl0,tab_valcm0,tab_valcn0,&
!.           [indcm0_i],[indcm0_j],[indcm0_k],&
!.           [indcn0_i],[indcn0_j],[indcn0_k],&
!.           [indcl0_i],[indcl0_j],[indcl0_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcl0
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcm0
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcn0
!.    real(kind=PM_REEL),dimension(3) :: cm0
!.    integer :: indcm0_i,indcm0_j,indcm0_k
!.    integer :: indcn0_i,indcn0_j,indcn0_k
!.    integer :: indcl0_i,indcl0_j,indcl0_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valcl0      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient de roulis
!>E/S   tab_valcm0      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient de tangage
!>E/S   tab_valcn0      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient de lacet
!>[E/S] indcm0_i        :<integer>                   indice du premier parametre pour le tangage
!>[E/S] indcm0_j        :<integer>                   indice du second parametre pour le tangage
!>[E/S] indcm0_k        :<integer>                   indice du troisieme parametre pour le tangage
!>[E/S] indcn0_i        :<integer>                   indice du premier parametre pour de lacet
!>[E/S] indcn0_j        :<integer>                   indice du second parametre pour de lacet
!>[E/S] indcn0_k        :<integer>                   indice du troisieme parametre pour de lacet
!>[E/S] indcl0_i        :<integer>                   indice du premier parametre pour de roulis
!>[E/S] indcl0_j        :<integer>                   indice du second parametre pour de roulis
!>[E/S] indcl0_k        :<integer>                   indice du troisieme parametre pour de roulis
!>S     cm0             :<PM_REEL,DIM=(3)>           valeur de [cl0,cm0,cn0] interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcl0
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcm0
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcn0
    real(kind=PM_REEL),dimension(3)           :: cm0
    integer,optional,intent(inout)            :: indcm0_i,indcm0_j,indcm0_k
    integer,optional,intent(inout)            :: indcn0_i,indcn0_j,indcn0_k
    integer,optional,intent(inout)            :: indcl0_i,indcl0_j,indcl0_k

    

    ! Début de la routine

    ! interpolation cz avion ou aero
    if ( associated(aero%cl0)) then
       cm0(1) = MSP_interp_coef(aero%cl0,tab_valcl0,indcoef_i=indcl0_i,&
            indcoef_j=indcl0_j,indcoef_k=indcl0_k)
       if (MSP_gen_messages("MSP_interp_cm0")) return
    else
       cm0(1) = 0._PM_REEL
    end if
    ! interpolation cx avion ou aero
    if ( associated(aero%cm0)) then
       cm0(2) = MSP_interp_coef(aero%cm0,tab_valcm0,indcoef_i=indcm0_i,&
            indcoef_j=indcm0_j,indcoef_k=indcm0_k)
       if (MSP_gen_messages("MSP_interp_cm0")) return
    else
       cm0(2) = 0._PM_REEL
    end if
    
    ! interpolation cx avion ou aero
    if ( associated(aero%cn0)) then
       cm0(3) = MSP_interp_coef(aero%cn0,tab_valcn0,indcoef_i=indcn0_i,&
            indcoef_j=indcn0_j,indcoef_k=indcn0_k)
       if (MSP_gen_messages("MSP_interp_cm0")) return
    else
       cm0(3) = 0._PM_REEL
    end if

  end function MSP_interp_cm0


  function MSP_interp_cmq(aero,tab_valcmq,indcmq_i,indcmq_j,indcmq_k) result (cmq)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_cmq
!
!$Resume
!	routine d'interpolation du coefficient dynamique de tangage
!
!$Description
!	routine d'interpolation du coefficient dynamique de tangage
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  cmq = MSP_interp_cmq(aero,tab_valcmq,[irecherchecmqi],&
!.           [indcmq_j],[indcmq_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcmq
!.    real(kind=PM_REEL) :: cmq
!.    integer :: indcmq_i,indcmq_j,indcmq_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero           
!>E/S   tab_valcmq      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient dynamique de tangage
!>[E/S] indcmq_i        :<integer>                   indice du premier parametre pour le coefficient dynamique de tangage
!>[E/S] indcmq_j        :<integer>                   indice du second parametre pour le coefficient dynamique de tangage
!>[E/S] indcmq_k        :<integer>                   indice du troisieme parametre pour le coefficient dynamique de tangage
!>S     cmq             :<PM_REEL>                   valeur cmq interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcmq
    real(kind=PM_REEL)                        :: cmq
    integer,optional,intent(inout)            :: indcmq_i,indcmq_j,indcmq_k

  
    ! Début de la routine
    ! interpolation cmq avion ou aero
    if ( associated(aero%cmq)) then
       cmq = MSP_interp_coef(aero%cmq,tab_valcmq,indcoef_i=indcmq_i,&
            indcoef_j=indcmq_j,indcoef_k=indcmq_k)
       if (MSP_gen_messages("MSP_interp_cmq")) return
    else
       cmq = 0._PM_REEL
    end if

  end function MSP_interp_cmq


  function MSP_interp_cnp(aero,tab_valcnp,indcnp_i,indcnp_j,indcnp_k) result (cnp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_cnp
!
!$Resume
!	Routine d'interpolation du coefficient dynamique de lacet
!
!$Description
!	Routine d'interpolation du coefficient dynamique de lacet
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  cnp = MSP_interp_cnp(aero,tab_valcnp,[indcnp_i],[indcnp_j],[indcnp_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcnp
!.    real(kind=PM_REEL) :: cnp
!.    integer :: indcnp_i,indcnp_j,indcnp_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valcnp      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient dynamique de lacet
!>[E/S] indcnp_i        :<integer>                   indice du premier parametre pour
!>[E/S] indcnp_j        :<integer>                   indice du second parametre pour
!>[E/S] indcnp_k        :<integer>                   indice du troisieme parametre pour
!>S     cnp             :<PM_REEL>                   valeur cnp interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcnp
    real(kind=PM_REEL)                        :: cnp
    integer,optional,intent(inout)            :: indcnp_i,indcnp_j,indcnp_k

        ! interpolation cnp avion ou aero
    if ( associated(aero%cnp)) then
       cnp = MSP_interp_coef(aero%cnp,tab_valcnp,indcoef_i=indcnp_i,&
            indcoef_j=indcnp_j,indcoef_k=indcnp_k)
       if (MSP_gen_messages("MSP_interp_cnp")) return
    else
       cnp = 0._PM_REEL
    end if
  end function MSP_interp_cnp
  function MSP_interp_cnr(aero,tab_valcnr,indcnr_i,indcnr_j,indcnr_k) result (cnr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_cnr
!
!$Resume
!	routine d'interpolation du coefficient dynamique de lacet
!
!$Description
!	routine d'interpolation du coefficient dynamique de lacet
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  cnr = MSP_interp_cnr(aero,tab_valcnr,[indcnr_i],[indcnr_j],[indcnr_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valcnr
!.    real(kind=PM_REEL) :: cnr
!.    integer :: indcnr_i,indcnr_j,indcnr_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valcnr      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient dynamique de lacet
!>[E/S] indcnr_i        :<integer>                   indice du premier parametre pour le coefficient dynamique de lacet
!>[E/S] indcnr_j        :<integer>                   indice du second parametre pour le coefficient dynamique de lacet
!>[E/S] indcnr_k        :<integer>                   indice du troisieme parametre pour le coefficient dynamique de lacet
!>S     cnr             :<PM_REEL>                   valeur cnr interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valcnr
    real(kind=PM_REEL)                        :: cnr
    integer,optional,intent(inout)            :: indcnr_i,indcnr_j,indcnr_k

    ! Début de la routine
       ! interpolation cnr avion ou aero
    if ( associated(aero%cnr)) then
       cnr = MSP_interp_coef(aero%cnr,tab_valcnr,indcoef_i=indcnr_i,&
            indcoef_j=indcnr_j,indcoef_k=indcnr_k)
       if (MSP_gen_messages("MSP_interp_cnr")) return
    else
       cnr = 0._PM_REEL
    end if
  end function MSP_interp_cnr
  function MSP_interp_clp(aero,tab_valclp,indclp_i,indclp_j,indclp_k) result (clp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_clp
!
!$Resume
!	Routine d'interpolation du coefficient dynamique de roulis
!
!$Description
!	Routine d'interpolation du coefficient dynamique de roulis
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  clp = MSP_interp_clp(aero,tab_valclp,[indclp_i],[indclp_j],[indclp_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valclp
!.    real(kind=PM_REEL) :: clp
!.    integer :: indclp_i,indclp_j,indclp_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valclp      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient dynamique de roulis
!>[E/S] indclp_i        :<integer>                   indice du premier parametre pour le coefficient dynamique de roulis
!>[E/S] indclp_j        :<integer>                   indice du second parametre pour le coefficient dynamique de roulis
!>[E/S] indclp_k        :<integer>                   indice du troisieme parametre pour le coefficient dynamique de roulis
!>S     clp             :<PM_REEL>                   valeur clp interpolee
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valclp
    real(kind=PM_REEL)                        :: clp
    integer,optional,intent(inout)            :: indclp_i,indclp_j,indclp_k

       
       ! interpolation clp avion ou aero
    if ( associated(aero%clp)) then
       clp = MSP_interp_coef(aero%clp,tab_valclp,indcoef_i=indclp_i,&
            indcoef_j=indclp_j,indcoef_k=indclp_k)
       if (MSP_gen_messages("MSP_interp_clp")) return
    else
       clp = 0._PM_REEL
    end if
  end function MSP_interp_clp
  function MSP_interp_clr(aero,tab_valclr,indclr_i,indclr_j,indclr_k) result (clr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_interp_clr
!
!$Resume
!	Routine d'interpolation du coefficient dynamique de roulis
!
!$Description
!	Routine d'interpolation du coefficient dynamique de roulis
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PUBLIC
!
!$Usage
!  clr = MSP_interp_clr(aero,tab_valclr,[indclr_i],[indclr_j],[indclr_k])
!.    type(MSP_AERO) :: AERO
!.    real(kind=PM_REEL),dimension(:),pointer :: tab_valclr
!.    real(kind=PM_REEL) :: clr
!.    integer :: indclr_i,indclr_j,indclr_k
!
!$Arguments
!>E     aero            :<MSP_AERO>                  structure aero
!>E/S   tab_valclr      :<PM_REEL,DIM=(:),pointer>   valeur des parametres (incidence, mach, ...) pour interpoler le coefficient dynamique de roulis
!>[E/S] indclr_i        :<integer>                   indice du premier parametre pour le coefficient dynamique de roulis
!>[E/S] indclr_j        :<integer>                   indice du second parametre pour le coefficient dynamique de roulis
!>[E/S] indclr_k        :<integer>                   indice du troisieme parametre pour le coefficient dynamique de roulis
!>S     clr             :<PM_REEL>                   valeur clr interpolee le coefficient dynamique de roulis
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
!	Tous les parametres irecherche* sont obsoletes mais concerve afin de garder la compatibilite avec 
!	d eventuelles fonctions ou routines qui les utiliseraient encore.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type(MSP_AERO),intent(in)                 :: AERO
    real(kind=PM_REEL),dimension(:),pointer   :: tab_valclr
    real(kind=PM_REEL)                        :: clr
    integer,optional,intent(inout)            :: indclr_i,indclr_j,indclr_k

  

    ! Début de la routine
      
       ! interpolation clr avion ou aero
    if ( associated(aero%clr)) then
        clr = MSP_interp_coef(aero%clr,tab_valclr,indcoef_i=indclr_i,&
            indcoef_j=indclr_j,indcoef_k=indclr_k)
       if (MSP_gen_messages("MSP_interp_clr")) return
    else
       clr = 0._PM_REEL
    end if
  end function MSP_interp_clr

  subroutine MSP_calculer_coefs_tabules(cxa,alsat,xcd,xcl)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_coefs_tabules
!
!$Resume
!  Calcul de coefs aéro tabulés selon le modèle "historique" 
!$Description
!  Cette routine calcule le coefficient xcd tabulé en fonction de l'altitude 'alsat'.
!  Ce coef sert à calculer la trainée. Le coef xcl (servant à calculer la portance) 
!  est nul.
!
!$Auteur
!  Y.TANGUY (ATOS), d'après les modèles PSIMU/MECASPA.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_coefs_tabules(cxa,alsat,xcd,xcl)
!.    type(MSP_COEF) :: cxa
!.    real(kind=pm_reel) :: alsat
!.    real(kind=pm_reel) :: xcd
!.    real(kind=pm_reel) :: xcl
!
!$Arguments
!>E     cxa    :<MSP_COEF>   Structure du coefficient Ca
!>E     alsat  :<pm_reel>    Altitude du satellite
!>S     xcd    :<pm_reel>    Coef xcd (pour la trainée)
!>S     xcl    :<pm_reel>    Coef xcl (pour la portance)
!
!$Common
!
!$Routines
!- MSP_consulter_coef
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

    ! Arguments
    !==========
    type(MSP_COEF),     intent(in) :: cxa
    real(kind=pm_reel), intent(in) :: alsat
    real(kind=pm_reel), intent(out) :: xcd
    real(kind=pm_reel), intent(out) :: xcl

    ! Variables locales
    !==================
    integer :: ialt
    real(kind=pm_reel), dimension(:), pointer :: cd
    
    ! Seuils d'altitude du modèle -> le modèle est valable
    ! entre 120 km et 1000 km.
    real(kind=pm_reel), parameter :: alt_min = 120000._pm_reel
    real(kind=pm_reel), parameter :: seuil_alt = 20000._pm_reel

    ! Début du code
    !==============
    nullify(cd)
    call MSP_consulter_coef(cxa,coef_1d=cd)
    if (MSP_gen_messages("MSP_calculer_coefs_tabules")) return
    

    ! ialt : n° de la "tranche" d'altitudes
    ialt = int((alsat-alt_min)/seuil_alt)+1
    if ( ialt < MSP_NB_POINTS_TABULES .and. ialt >= 1 ) then
       xcd = cd(ialt)
    else
       if ( ialt < 1 ) then
          ! cas ou alsat < alt_min
          ! xcd est borné au coef min
          xcd = cd(1)
       else
          ! cas ou alsat > alt_max
          ! xcd est borné au coef max
          xcd = cd(MSP_NB_POINTS_TABULES) 
       endif
    endif
    
    ! portance toujours nulle
    xcl = 0._pm_reel
    
    ! Désallocations
    if (associated(cd)) deallocate(cd)

  end subroutine MSP_calculer_coefs_tabules

  subroutine MSP_calculer_coefs_aero(aero,xcd,xcl,alsat,incidence,mach)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_coefs_aero
!
!$Resume
!  Routine de calcul des coefficients aérodynamiques
!$Description
!  Cette routine calcule les coefs exprimés dans un repère aérodynamique et 
!  gère différents types de variation (coefs nuls, constants, dépendants de l'incidence
!  et du mach, dépendants de l'altitude) et les coefs tabulés (modèle historique) qui
!  dépendent de l'altitude.
!
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_coefs_aero(aero,xcd,xcl,[alsat],[incidence],[mach])
!.    type(MSP_AERO) :: AERO
!.    real(kind=pm_reel) :: xcd
!.    real(kind=pm_reel) :: xcl
!.    real(kind=pm_reel) :: alsat
!.    real(kind=pm_reel) :: incidence
!.    real(kind=pm_reel) :: mach
!
!$Arguments
!>E     aero       :<MSP_AERO>   Structure AERO
!>S     xcd        :<pm_reel>    Coef xcd, utilisé par exemple pour le calcul d'un coef de trainée
!>S     xcl        :<pm_reel>    Coef xcl, utilisé par exemple pour le calcul d'un coef de portance
!>[E]   alsat      :<pm_reel>    Altitude du véhicule, pour les coefs variant selon l'altitude
!>[E]   incidence  :<pm_reel>    Incidence du véhicule, pour les coefs variant selon l'incidence et le mach
!>[E]   mach       :<pm_reel>    Nb de mach, pour les coefs variant selon l'incidence et le mach
!
!$Common
!
!$Routines
!- MSP_effacer_coef
!- MSP_consulter_aero
!- MSP_signaler_message
!- MSP_calculer_coefs_tabules
!- MSP_consulter_coef
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
! PSIMU
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    !==========
    type(MSP_AERO),intent(in)      :: AERO
    real(kind=pm_reel), intent(out) :: xcd
    real(kind=pm_reel), intent(out) :: xcl

    real(kind=pm_reel), optional, intent(in) :: alsat
    real(kind=pm_reel), optional, intent(in) :: incidence
    real(kind=pm_reel), optional, intent(in) :: mach


    ! Variables locales
    !==================

    ! tableaux dynamiques : mis à NULL et alloués si besoin
    real(kind=pm_reel), dimension(:), pointer :: tab_xcd, tab_xcl,incidence_mach,tab_alt
    real(kind=pm_reel), dimension(3) :: tab_val
    ! coefficients aero
    type(MSP_COEF) :: cxa, czn
    integer :: type_coef,type_variation
    character(len=MSP_long_chaine) :: partie_variable


    ! Début du code
    !==============

    call MSP_effacer_coef(cxa,nul=.true.)
    call MSP_effacer_coef(czn,nul=.true.)
    
    ! Consultation des tableaux de coefs et des types de coef et de variation des coefs
    call MSP_consulter_aero(aero,cxa=cxa,czn=czn, &
         type_coef=type_coef, type_variation=type_variation)
    if (MSP_gen_messages("MSP_calculer_coefs_aero") ) return

    
    if (type_coef == MSP_ENUM_CF_TABULE_ALT_MOY) then
       ! Coefficients tabulés en fonction de l'altitude.
       ! -> mode "historique" de PSIMU / MECASPA
       if(.not. present(alsat)) then
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
               partie_variable="L'altitude (alt) est manquante" , &
               routine="MSP_calculer_coefs_aero",type=MSP_ENUM_ERREUR)
          return
       end if
          
       call MSP_calculer_coefs_tabules(cxa,alsat,xcd,xcl)
       if (MSP_gen_messages("MSP_calculer_coefs_aero")) return

    else if (type_coef == MSP_ENUM_COEFF_AERO_VITESSE) then
       ! Type de coef : aérodynamiques (ca/cn)

       if(type_variation == MSP_ENUM_CONSTANT) then
          ! Coefs constants
          nullify(tab_xcd)
          nullify(tab_xcl)

          call MSP_consulter_coef(cxa,coef_1d=tab_xcd)
          if (MSP_gen_messages("MSP_calculer_coefs_aero")) return

          call MSP_consulter_coef(czn,coef_1d=tab_xcl)
          if (MSP_gen_messages("MSP_calculer_coefs_aero")) return

          xcd = tab_xcd(1)
          xcl = tab_xcl(1)

          if (associated(tab_xcd)) deallocate(tab_xcd)
          if (associated(tab_xcl)) deallocate(tab_xcl)

       else if (type_variation == MSP_ENUM_ALTITUDE) then
          
          if(.not. present(alsat)) then
             call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                  partie_variable="L'altitude (alt) est manquante" , &
                  routine="MSP_calculer_coefs_aero",type=MSP_ENUM_ERREUR)
             return
          end if

          ! Coefs variant en fonction de l'altitude, lecture dans un fichier
          
          nullify(tab_alt)
          allocate(tab_alt(1))

          tab_alt(1) = alsat

          ! appel à la MECASPA, avec incidence et gîte 0. (on exprime les coefs dans un repère aéro)
          tab_val = MSP_interp_coef_aero(aero,tab_alt,tab_alt,tab_alt,0._pm_reel,0._pm_reel)
          if(MSP_gen_messages("MSP_calculer_coefs_aero")) return

          ! assignation des valeurs à partir de coef aero
          xcd = tab_val(1)
          xcl = tab_val(3)

          if (associated(tab_alt)) deallocate(tab_alt)
          
       else if (type_variation == MSP_ENUM_INCIDENCE_MACH) then
          if((.not. present(incidence)) .or. (.not. present(mach))) then
             call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                  partie_variable="L'incidence ou le mach manque" , &
                  routine="MSP_calculer_coefs_aero",type=MSP_ENUM_ERREUR)
             return
          end if

          !  Coefs variant en fonction de l'incidence et du mach, lecture dans un fichier

          nullify(incidence_mach)
          allocate (incidence_mach(2))

          incidence_mach(1) = incidence
          incidence_mach(2) = mach
          tab_val = Msp_interp_coef_aero(aero,incidence_mach,incidence_mach,incidence_mach,0._pm_reel,0._pm_reel)
          if(MSP_gen_messages("ps_coef_incidence_mach")) return

          xcd = tab_val(1)
          xcl = tab_val(3)

          if (associated(incidence_mach)) deallocate(incidence_mach)

       else if (type_variation == MSP_ENUM_COEFF_NULS) then
          ! Coefs nuls, c'est à dire non initialisé ou non utilisé
          xcd = 0._pm_reel
          xcl = 0._pm_reel
       else

          write (partie_variable,'(i2)') type_variation

          call MSP_signaler_message (cle_mes="MSP_AERO_ERR_TYPE_VARIATION",&
               routine="MSP_calculer_coefs_aero",partie_variable=partie_variable)
          return
       end if
    else
       write (partie_variable,'(i2)') type_coef

       call MSP_signaler_message (cle_mes="MSP_AERO_ERR_TYPE_COEF", &
            routine="MSP_calculer_coefs_aero",partie_variable=partie_variable)
       return
    end if

    ! désallocations mémoire
    !=======================
    call MSP_effacer_coef(cxa)
    call MSP_effacer_coef(czn)
    if (associated(tab_xcd))        deallocate(tab_xcd)
    if (associated(tab_xcl))        deallocate(tab_xcl)
    if (associated(incidence_mach)) deallocate(incidence_mach)
    if (associated(tab_alt))        deallocate(tab_alt)


  end subroutine MSP_calculer_coefs_aero

  end module MSP_AERO_DEF
