module MSP_POTENTIEL_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_POTENTIEL_DEF
!
!$Resume
!  Module gérant les modèles de potentiel.
!
!$Description
!  Module gérant les modèles de potentiel.
!  calculs jusqu'à ordre/degré 120 
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_POTENTIEL_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_POTENTIEL_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.39  2011/04/26 17:46:03  vivaresf
!  VERSION::FA-ID:1470:26/04/2011:contrôle des division par 0 et contournement pour le cas des pôles
!
!  Revision 1.38  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.37  2010/06/23 14:12:20  mercadig
!  VERSION::AQ::23/06/2010: Suppression des variables inutilisees val1, val2 et val4
!
!  Revision 1.36  2010/03/29 13:06:58  vivaresf
!  Version::FA-ID:1377:29/03/2010: précision sur les limites du calcul de potentiel
!  remplacement de la dénormalisation valide jusqu'à degré 70 par la dénromalisation COMPAS (valide jusqu'à 12)
!  remplacement d'une variable locale p  par pp pour la lisibilité des algo
!
!  Revision 1.35  2009/11/05 09:08:55  cmartel
!  DM-ID 1047 : Correction de l'algorithme afin de denormaliser a des degres eleves
!
!  Revision 1.34  2009/11/04 08:44:58  cmartel
!  DM-ID 842 : Ajout d'un constructeur avec le mu seulement
!
!  Revision 1.33  2008/11/20 12:46:03  cml
!  AQ : Suppression de variables inutilisees
!  Revision 1.32  2008/11/19 13:38:14  mercadig
!  DM-ID 733 : Mise a jour cartouche
!  Revision 1.31  2008/11/05 08:49:28  tanguyy
!  DM-ID 733 : les tableaux des coefs de Legendre sont stockes dans la structure et alloues a la creation du potentiel ; AQ : maj des cartouches et des commentaires dans le code
!  Revision 1.30  2008/10/06 07:51:27  huec
!  AQ : Meilleur gestion de pointeurs
!  Revision 1.29  2008/08/08 14:54:34  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 de variables locales. Initialisation
!  à NULL des pointeurs lors de leur déclaration.
!  Revision 1.28  2008/07/04 14:57:27  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.27  2008/04/24 13:59:04  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.26  2008/02/26 08:04:31  huec
!  FA-ID 887 : Ajout de tests d erreur suite a appel COMPAS
!  Revision 1.25  2008/02/22 13:53:09  huec
!  FA-ID 968 : Utilisation de parentheses pour des calculs non triviaux et initialisation de pointeurs
!  Revision 1.24  2007/10/23 15:00:54  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.23  2007/10/03 07:56:25  tanguyy
!  DM-ID 733 : nouvelle option (tsid) pour le calcul du potentiel (msp_calculer_potentiel)
!  Revision 1.22  2007/06/18 10:44:15  vivaresf
!  FA-ID 748 : suppression des dimensions en clair, écriture des chaines avec trim
!  Revision 1.21  2007/06/18 10:14:24  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.20  2007/06/15 13:53:36  vivaresf
!  FA-ID 746 : mise en inout des structures suceptibles
!  d'être écrasées par une allocation pour permettre la désallocation propre
!  Revision 1.19  2007/06/14 14:19:55  vivaresf
!  FA-ID 679 : suppression de la multiplication par 1e-6 suite à la maj des potentiels COMPAS
!  Revision 1.18  2006/11/07 09:03:40  mle
!  DM-ID 560 : integration des modeles de potentiel de COMPAS dans PSIMU
!  Revision 1.17  2006/10/27 09:52:50  mle
!  DM-ID 560 : integration des modeles de potentiel de COMPAS dans PSIMU
!  Revision 1.16  2006/10/25 15:32:51  mle
!  DM-ID 560 : integration des modeles de potentiel de COMPAS dans PSIMU
!  Revision 1.15  2005/03/08 07:32:36  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.14  2005/01/20 13:56:30  pauh
!  FA_332
!  Revision 1.13.2.1  2005/01/19 10:41:10  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.13  2005/01/18 11:13:25  pauh
!  DM_126
!  Revision 1.12.2.1  2005/01/18 10:47:48  pauh
!  ajout de la routine MSP_calculer_potentiel
!  ajout de la routine privee MSP_poly_legendre_derive utilisee par la precedente
!  Revision 1.12  2004/11/05 16:27:04  vivaresf
!  coquilles
!  Revision 1.11  2004/10/25 10:15:02  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.10  2003/03/12 14:30:45  adm_ipsi
!   Déplacement du close (ifich)
!  Revision 1.9  2003/02/06 10:27:52  adm_ipsi
!  MSP_e_denpot, remplacement de la routine mufact par MSP_factorielle
!  Revision 1.8  2003/01/08 15:12:39  adm_ipsi
!   Explicitation des conversions de type implicites
!  Revision 1.7  2003/01/07 18:11:40  adm_ipsi
!   suppression des variables non utilisées
!  Revision 1.6  2002/12/11 10:18:10  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.5  2002/12/09 16:45:37  adm_ipsi
!  Utilisation du parametre MSP_ENUM_ECRAN pour les sorties ecran
!  Revision 1.4  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:03  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/07 17:48:28  adm_ipsi
!  MSP_modifier_potentiel, les paramètres nom_pot, nomfic et dirpot deviennent de longueur variable
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.4  2002/05/03 07:52:26  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL() et argument nul dans effacer_)
!  Revision 1.3  2000/06/14 16:21:26  util_am
!  - Ajout du champ flag_func dans la structure MSP_POTENTIEL pour la gestion des fuites mémoires
!  - Privatisation du contenu de la structure MSP_POTENTIEL
!  - Ajout des routines MSP_consulter_potentiel, MSP_modifier_potentiel
!  - Ajout des interfaces anglaises
!  - Mise à jour des cartouches
!  Revision 1.2  2000/02/10 16:20:27  rousseau
!  ajout de la fonction MSP_affecter_potentiel
!  Revision 1.1.1.1  1999/07/13 08:37:57  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_POTENTIEL_DEF
!
!$Structure
!
!: MSP_POTENTIEL : 
!#V
!>     flag_func   : <logical,private>                    
!>     nom_pot     : <LEN=MSP_LONG_CHAINE,private>        nom du potentiel
!>     nomfic      : <LEN=MSP_LONG_NOMFIC,private>        nom du fichier
!>     nzo         : <integer,private>                    degré des zonaux
!>     nte         : <integer,private>                    degré des tesseraux
!>     mu          : <pm_reel,private>                    constante gravitationelle [m^3/s^2]
!>     requa       : <pm_reel,private>                    rayon équatorial [m]
!>     apla        : <pm_reel,private>                    inverse de l'aplatissement
!>     vrot        : <pm_reel,private>                    vitesse de rotation du corps
!>     degmax      : <integer,private>                    degré maximum
!>     zb          : <pm_reel,DIM=(:),pointer,private>    tableau des zonaux normalisés
!>     clmb        : <pm_reel,DIM=(:,:),pointer,private>  tableau des tesseraux "CLM" normalisés
!>     slmb        : <pm_reel,DIM=(:,:),pointer,private>  tableau des tesseraux "SLM" normalisés
!>     zd          : <pm_reel,DIM=(:),pointer,private>    tableau des zonaux dénormalisés
!>     clmd        : <pm_reel,DIM=(:,:),pointer,private>  tableau des tesseraux "CLM" dénormalisés
!>     slmd        : <pm_reel,DIM=(:,:),pointer,private>  tableau des tesseraux "SLM" dénormalisés
!>     p           : <PM_REEL,DIM=(:),pointer,private>    tableau des coefs des polynomes de Legendre
!>     dp          : <PM_REEL,DIM=(:),pointer,private>    tableau des coefs des polynomes de Legendre
!>     t           : <PM_REEL,DIM=(:),pointer,private>    tableau des coefs des polynomes de Legendre
!>     w           : <PM_REEL,DIM=(:),pointer,private>    tableau des coefs des polynomes de Legendre
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_clear_potential
!- MSP_assign_potential
!- MSP_create_potential
!- MSP_read_MSDON_potential
!- MSP_unnormalize_potential
!- MSP_get_potential_data
!- MSP_set_potential_data
!- MSP_display_potential
!- MSP_effacer_potentiel
!- MSP_e_lecpot
!- MSP_e_denpot
!- MSP_affecter_potentiel
!- MSP_consulter_potentiel
!- MSP_modifier_potentiel
!- MSP_afficher_potentiel
!- MSP_calculer_potentiel
!#V
!- MSP_egaler_potentiel
!- MSP_poly_legendre_derive
!#
!
!$Fonctions
!- MSP_creer_potentiel
!- MSP_creer_potentiel_mu
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_ACCES
!- MSP_MECASPA_DEF
!- MSP_MATH
!- cps_potentiel
!- MSP_BULLETIN_DEF
!#
!
!$Interface
!> msp_get_potential_data :     MSP_consulter_potentiel
!> assignment :                 MSP_egaler_potentiel
!> msp_set_potential_data :     MSP_modifier_potentiel
!> msp_display_potential :      MSP_afficher_potentiel
!> msp_create_potential :       msp_creer_potentiel
!> msp_unnormalize_potential :  MSP_e_denpot
!> msp_assign_potential :       MSP_affecter_potentiel
!> msp_clear_potential :        MSP_effacer_potentiel
!> msp_read_msdon_potential :   MSP_e_lecpot
!#V
!#
!
!$Remarques
!
!$Mots-cles
! POTENTIEL
! POTENTIEL CREER
! POTENTIEL
! POTENTIEL CREER
!
!$Voir-Aussi
!#V
!.  MSP_egaler_potentiel MSP_poly_legendre_derive
!#
!.  MSP_creer_potentiel MSP_creer_potentiel_mu MSP_clear_potential MSP_assign_potential MSP_create_potential
!.  MSP_read_MSDON_potential MSP_unnormalize_potential MSP_get_potential_data MSP_set_potential_data
!.  MSP_display_potential MSP_effacer_potentiel MSP_e_lecpot MSP_e_denpot MSP_affecter_potentiel
!.  MSP_consulter_potentiel MSP_modifier_potentiel MSP_afficher_potentiel MSP_calculer_potentiel
!
!$Nom
!  MSP_POTENTIEL_DEF
!
!$Resume
!  Module gérant les modèles de potentiel.
!  Module gérant les modèles de potentiel.
!
!$Description
!  Module gérant les modèles de potentiel.
!  Module gérant les modèles de potentiel.
!
!$Usage
!  use MSP_POTENTIEL_DEF
!
!$Procedures
!
!$Remarques
!
!$Mots-cles
! POTENTIEL
! POTENTIEL CREER
! POTENTIEL
! POTENTIEL CREER
!
!$Voir-Aussi
!#V
!.  MSP_egaler_potentiel MSP_poly_legendre_derive
!#
!.  MSP_creer_potentiel MSP_creer_potentiel_mu MSP_clear_potential MSP_assign_potential MSP_create_potential
!.  MSP_read_MSDON_potential MSP_unnormalize_potential MSP_get_potential_data MSP_set_potential_data
!.  MSP_display_potential MSP_effacer_potentiel MSP_e_lecpot MSP_e_denpot MSP_affecter_potentiel
!.  MSP_consulter_potentiel MSP_modifier_potentiel MSP_afficher_potentiel MSP_calculer_potentiel
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB, only : pm_reel
  use MSP_GESTION_ERREUR
  use MSP_BULLETIN_DEF, only : MSP_epsilon_apla
  use MSP_ACCES
  use MSP_MECASPA_DEF
  use MSP_MATH
  use cps_potentiel

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_POTENTIEL_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


  type MSP_POTENTIEL

     private
     logical :: flag_func

     character(LEN=MSP_LONG_CHAINE)  :: nom_pot

     character(LEN=MSP_LONG_NOMFIC)  :: nomfic

     integer :: nzo
     integer :: nte
      
     real (KIND=pm_reel) :: mu
     real (KIND=pm_reel) :: requa
     real (KIND=pm_reel) :: apla
     real (KIND=pm_reel) :: vrot
     
     integer :: degmax
     
     real (KIND=pm_reel), pointer, dimension(:)   :: zb   => NULL()
     real (KIND=pm_reel), pointer, dimension(:,:) :: clmb => NULL()
     real (KIND=pm_reel), pointer, dimension(:,:) :: slmb => NULL()
     real (KIND=pm_reel), pointer, dimension(:)   :: zd   => NULL()
     real (KIND=pm_reel), pointer, dimension(:,:) :: clmd => NULL()
     real (KIND=pm_reel), pointer, dimension(:,:) :: slmd => NULL()
     
     ! tableaux du potentiel, utilisés lors du calcul de potentiel
     real(kind=PM_REEL),dimension(:),pointer :: p  => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: dp => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: t  => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: w  => NULL()
     
  end type MSP_POTENTIEL

  interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!	Fonction = du type MSP_POTENTIEL
!
!$Description
!	Fonction = du type MSP_POTENTIEL
!
!$Acces
!  PUBLIC
!
!$Usage
!  pota=potb
!.    type(MSP_POTENTIEL) :: pota
!.    type(MSP_POTENTIEL) :: potb
!
!$Procedures
!#V
!- MSP_egaler_potentiel
!#
!
!$Remarques
!
!$Mots-cles
! POTENTIEL EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module procedure MSP_egaler_potentiel

   end interface


   interface MSP_clear_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_potential
!
!$Resume
!  Clear a gravity potential structure
!
!$Description
!  Clear a gravity potential structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_potential (pot, [nul])
!.    type(MSP_POTENTIEL) :: pot
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_potentiel
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_potentiel
   end interface

   interface MSP_assign_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_assign_potential
!
!$Resume
!  Assign a gravity potential structure to another
!
!$Description
!  Assign a gravity potential structure to another
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_assign_potential(pota,potb)
!.    type(MSP_POTENTIEL) :: pota
!.    type(MSP_POTENTIEL) :: potb
!
!$Procedures
!- MSP_affecter_potentiel
!
!$Remarques
!
!$Mots-cles
! POTENTIEL AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_affecter_potentiel
   end interface

   interface MSP_create_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_potential
!
!$Resume
!  Create a potential gravity structure 
!
!$Description
!  Create a potential gravity structure 
!
!$Acces
!  PUBLIC
!
!$Usage
!
!$Procedures
!- msp_creer_potentiel
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       module procedure msp_creer_potentiel

   end interface
    

   interface MSP_read_MSDON_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_read_MSDON_potential
!
!$Resume
!  Création d'un modèle de potentiel.
!
!$Description
!  Création d'un modèle de potentiel.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_read_MSDON_potential (ifich,mu,requa,apla,zb,clmb,slmb,degmax,nz,nt)
!.    integer :: ifich
!.    real (KIND=pm_reel) :: mu,requa,apla
!.    real (KIND=pm_reel), pointer :: zb(:),clmb(:,:),slmb(:,:)
!.    integer :: degmax
!.    integer :: nz,nt
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
! POTENTIEL LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_e_lecpot
   end interface

   interface MSP_unnormalize_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_unnormalize_potential
!
!$Resume
!  Unnormalize potential gravity coefficients
!
!$Description
!  Unnormalize potential gravity coefficients
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_unnormalize_potential (nz,nt,zb,clmb,slmb,zd,clmd,slmd)
!.    integer :: nz,nt
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!
!$Procedures
!- MSP_e_denpot
!
!$Remarques
!
!$Mots-cles
! POTENTIEL DENORMALISER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_e_denpot
   end interface
 
   interface MSP_get_potential_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_potential_data
!
!$Resume
!  Get the potential gravity characteristics
!
!$Description
!  Get the potential gravity characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_potential_data(potentiel, [nom_pot], [nomfic], [nzo], [nte], &
!.           mu, requa, apla, degmax, zb, clmb, slmb, zd,  clmd, slmd)
!.    type(MSP_POTENTIEL) :: potentiel
!.    character(LEN=MSP_LONG_CHAINE) :: nom_pot
!.    character(LEN=MSP_LONG_NOMFIC) :: nomfic
!.    integer :: nzo
!.    integer :: nte
!.    real (KIND=pm_reel) :: mu
!.    real (KIND=pm_reel) :: requa
!.    real (KIND=pm_reel) :: apla
!.    integer :: degmax
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!
!$Procedures
!- MSP_consulter_potentiel
!
!$Remarques
!
!$Mots-cles
! POTENTIEL CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_potentiel
   end interface
 
   interface MSP_set_potential_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_potential_data
!
!$Resume
!  Modify the potential gravity characteristics
!
!$Description
!  Modify the potential gravity characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_potential_data(potentiel, [nom_pot], [nomfic], [nzo], [nte], &
!.           mu, requa, apla, degmax, zb, clmb, slmb, zd,  clmd, slmd)
!.    type(MSP_POTENTIEL) :: potentiel
!.    character(LEN=*) :: nom_pot
!.    character(LEN=*) :: nomfic
!.    integer :: nzo
!.    integer :: nte
!.    real (KIND=pm_reel) :: mu
!.    real (KIND=pm_reel) :: requa
!.    real (KIND=pm_reel) :: apla
!.    integer :: degmax
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!
!$Procedures
!- MSP_modifier_potentiel
!
!$Remarques
!
!$Mots-cles
! POTENTIEL MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_potentiel
   end interface
 
   interface MSP_display_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_potential
!
!$Resume
!  Display the potential gravity characteristics
!
!$Description
!  Display the potential gravity characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_potential (potentiel,nz,nt,[ilog])
!.    type(MSP_POTENTIEL) :: potentiel
!.    integer :: nz,nt
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_potentiel
!
!$Remarques
!
!$Mots-cles
! POTENTIEL AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_potentiel
   end interface
 
   private :: MSP_egaler_potentiel
   private :: MSP_poly_legendre_derive

 contains


   subroutine MSP_effacer_potentiel (pot, nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_potentiel
!
!$Resume
!  Efface le contenu d'une structure potentiel
!
!$Description
!  Efface le contenu d'une structure potentiel
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_potentiel (pot, [nul])
!.    type(MSP_POTENTIEL) :: pot
!.    logical :: nul
!
!$Arguments
!>E/S   pot  :<MSP_POTENTIEL>   Structure potentiel à effacer
!>[E/S] nul  :<logical>         si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
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

       type(MSP_POTENTIEL) ::pot
       logical, optional :: nul
       
       logical :: nul_tmp
       integer :: MSP_iostat
       
       MSP_iostat = 0

       if ( present (nul) ) then
          nul_tmp = nul
       else
          nul_tmp = .false.
       endif

       pot%nom_pot=""
       pot%nomfic=""
       pot%mu=0._PM_REEL
       pot%requa=0._PM_REEL
       pot%apla=0._PM_REEL
       pot%degmax=0

       if ( nul_tmp ) then

         ! On se contente d'enlever les liens sans désallouer
          nullify(pot%zb)
          nullify(pot%clmb)
          nullify(pot%slmb)
          nullify(pot%zd)
          nullify(pot%clmd)
          nullify(pot%slmd)

          nullify(pot%p)
          nullify(pot%dp)
          nullify(pot%t)
          nullify(pot%w)
       else

          if(associated(pot%zb)) then
             deallocate(pot%zb,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%clmb)) then
             deallocate(pot%clmb,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%slmb)) then
             deallocate(pot%slmb,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%zd)) then
             deallocate(pot%zd,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%clmd)) then
             deallocate(pot%clmd,stat=MSP_iostat) 
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%slmd)) then
             deallocate(pot%slmd,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if


          if ( associated(pot%p)) then
             deallocate(pot%p,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%t)) then
             deallocate(pot%t,stat=MSP_iostat) 
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%dp)) then
             deallocate(pot%dp,stat=MSP_iostat) 
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

          if ( associated(pot%w)) then
             deallocate(pot%w,stat=MSP_iostat)
             if (MSP_iostat /= 0) then
                call MSP_signaler_message(cle_mes="MSP_ERR_DESALLOC")
                return
             end if
          end if

        endif

     end subroutine MSP_effacer_potentiel
       


   function MSP_creer_potentiel (nomfic,nzo,nte,nom_pot) result (potentiel)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_potentiel
!
!$Resume
!  Création d'un modèle de potentiel.
!
!$Description
!  Création d'un modèle de potentiel.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  potentiel = MSP_creer_potentiel (nomfic,[nzo],[nte],[nom_pot])
!.    type(MSP_POTENTIEL) :: potentiel
!.    character(LEN=*) :: nomfic
!.    integer :: nzo,nte
!.    character(LEN=*) :: nom_pot
!
!$Arguments
!>E     nomfic     :<LEN=*>           nom du fichier
!>[E/S] nzo        :<integer>         degré des zonaux
!>[E/S] nte        :<integer>         degré des tesseraux
!>[E]   nom_pot    :<LEN=*>           nom du potentiel
!>S     potentiel  :<MSP_POTENTIEL>   
!
!$Common
!
!$Routines
!- MSP_effacer_potentiel
!- MSP_signaler_message
!- cps_lirePotentielGRGS
!- MSP_e_denpot
!
!$Include
!
!$Module
!#V
!- MSP_BULLETIN_DEF
!- cps_potentiel
!#
!
!$Remarques
!
!$Mots-cles
! POTENTIEL CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use cps_potentiel
     implicit none

     ! Arguments
     !==========
     type(MSP_POTENTIEL) :: potentiel
     
     character(LEN=*), intent(IN) :: nomfic
     integer, intent(INOUT), optional :: nzo,nte
     character(LEN=*), intent(IN), optional :: nom_pot
     

     ! Variables locales
     !==================
     integer :: lnom, ii,n,j
     integer :: nzo_tmp,nte_tmp
     
     character(LEN=80),dimension(2)       :: tmessage_var
     
     real(kind=PM_REEL) :: jdeb,q
     logical :: denorm = .false.
     integer :: ordremax, czmax, indm, indl, stat, ndim, MSP_iostat
     real(KIND=PM_REEL), dimension(:,:), pointer :: C, S

     ! Initialisations
     !================
     nullify(C)
     nullify(S)

     MSP_iostat = 0
     czmax = 0
     jdeb = 0._pm_reel

     call MSP_effacer_potentiel(potentiel, nul=.true.)
     potentiel%flag_func = .true.

     if ( .not. present (nzo) ) then
        nzo_tmp = -1
     else
        nzo_tmp = nzo
     endif

     if ( .not. present (nte) ) then
        nte_tmp = -1
     else
        nte_tmp = nte
     endif

     if ( present(nom_pot) ) then
        lnom = LEN_TRIM(nom_pot)
        if ( lnom <= MSP_LONG_CHAINE ) then 
           potentiel%nom_pot  = nom_pot
        else
           potentiel%nom_pot  = nom_pot(1:MSP_LONG_CHAINE)
           tmessage_var(1) = 'Le nom du potentiel'
           write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

           call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
                routine="MSP_creer_potentiel",type=MSP_ENUM_WARNING, &
                partie_variable=tmessage_var)
        endif
     else
        potentiel%nom_pot = ""
     endif

     lnom = LEN_TRIM(nomfic)
     if ( lnom <= MSP_LONG_NOMFIC ) then 
        potentiel%nomfic  = nomfic
     else
        potentiel%nomfic  = nomfic(1:MSP_LONG_NOMFIC)
        tmessage_var(1) = 'Le nom du fichier potentiel'
        write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC

        call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
             routine="MSP_creer_potentiel",type=MSP_ENUM_WARNING, &
             partie_variable=tmessage_var)
     endif


     ! Lecture du fichier afin d'obtenir les coefficients normalisés:

     !le fichier est au format GRGS, la routine de lecture de COMPAS est appelee
     call cps_lirePotentielGRGS(nomfic, jdeb, denorm, C, S, potentiel%requa, &
          potentiel%apla, &
          potentiel%mu, potentiel%vrot, potentiel%degmax, ordremax, czmax)
     if (MSP_gen_messages("MSP_creer_potentiel")) then
        !libération de mémoire
        if (associated(C)) then
           deallocate(C, stat=stat)
        end if
        if (associated(S)) then
           deallocate(S, stat=stat)
        end if
        return
     end if
     if(abs(potentiel%apla) > MSP_EPSILON_APLA) then
        potentiel%apla = 1._pm_reel/potentiel%apla
     end if
     if(potentiel%degmax>0) then
        !conversion des donnees obtenues au type potentiel
        if(nzo_tmp == -1) nzo_tmp =  potentiel%degmax 
        if(nte_tmp == -1) nte_tmp =  potentiel%degmax 
        ! on se base sur le principe de la MECASPA : les tableaux de coefficients sont de taille
        ! degre max x degremax ou degmax mais seuls les nte x nte ou nzo premiers termes sont remplis
        if (associated( potentiel%clmb)) deallocate ( potentiel%clmb)
        if (associated( potentiel%slmb)) deallocate ( potentiel%slmb)
        if (associated( potentiel%zb)) deallocate ( potentiel%zb)
        allocate(potentiel%clmb( potentiel%degmax, potentiel%degmax))
        allocate(potentiel%slmb( potentiel%degmax, potentiel%degmax))
        allocate(potentiel%zb(potentiel%degmax))
        potentiel%clmb(:,:) = 0._pm_reel
        potentiel%slmb(:,:) = 0._pm_reel
        potentiel%zb(:) = 0._pm_reel
        indm = 0
        do indl	= 1, nzo_tmp
           potentiel%zb(indl) =  C(indl,indm)
        enddo
        do indm = 1, nte_tmp
           do indl	= 1, nte_tmp
              potentiel%clmb(indl, indm) = C(indl,indm)
              potentiel%slmb(indl, indm) = S(indl,indm)
           enddo
        enddo
     else
        !ERREUR : le degre maximal vaut zéro
        call MSP_signaler_message (cle_mes="MSP_DEG_MAX", &
             routine="MSP_creer_potentiel")
        !libération de mémoire
        if (associated(C)) then
           deallocate(C, stat=stat)
        end if
        if (associated(S)) then
           deallocate(S, stat=stat)
        end if
        return
     end if

     if  (MSP_gen_messages("MSP_creer_potentiel")) then
        !libération de mémoire
        if (associated(C)) then
           deallocate(C, stat=stat)
        end if
        if (associated(S)) then
           deallocate(S, stat=stat)
        end if
        return
     end if

     potentiel%nzo = nzo_tmp
     potentiel%nte = nte_tmp

     ! Dénormalisation des coefficients:

     call MSP_e_denpot (nzo_tmp,nte_tmp,potentiel%zb,potentiel%clmb, &
          potentiel%slmb, &
          potentiel%zd,potentiel%clmd,potentiel%slmd)

     if  (MSP_gen_messages("MSP_creer_potentiel")) then
        !libération de mémoire
        if (associated(C)) then
           deallocate(C, stat=stat)
        end if
        if (associated(S)) then
           deallocate(S, stat=stat)
        end if
        return
     end if


     !  Affectation des éléments du potentiel 
     if (associated(potentiel%zb)) then
        ndim=size(potentiel%zb,dim=1)
     else
        call MSP_signaler_message(cle_mes="MSP_potentiel_002",partie_variable="zb")
        return
     endif

     ! Allocation des 4 tableaux p,dp,w,t : ils ont été mis à NULL
     ! par le MSP_effacer_potentiel

     ! Allocation vecteur p
     allocate(potentiel%p(ndim),stat=MSP_iostat)
     if (MSP_iostat /= 0) then
        call MSP_signaler_message(cle_mes="MSP_ERR_ALLOC")
        return
     end if

     ! Allocation vecteur dp

     allocate(potentiel%dp(ndim),stat=MSP_iostat)
     if (MSP_iostat /= 0) then
        call MSP_signaler_message(cle_mes="MSP_ERR_ALLOC")
        return
     end if

     ! Allocation vecteur t
     allocate(potentiel%t(ndim),stat=MSP_iostat)
     if (MSP_iostat /= 0) then
        call MSP_signaler_message(cle_mes="MSP_ERR_ALLOC")
        return
     end if

     ! Allocation vecteur w
     allocate(potentiel%w(ndim),stat=MSP_iostat)
     if (MSP_iostat /= 0) then
        call MSP_signaler_message(cle_mes="MSP_ERR_ALLOC")
        return
     end if

     ! les 4 tableaux sont utilisés pour les coefficients des polynomes de LEGENDRE
     ! -> ils sont alloués une fois dans MSP_creer_potentiel, calculés lors des appels
     ! à MSP_calculer_potentiel, et libérés dans MSP_effacer_potentiel
     do ii=1,ndim
        potentiel%p(ii) = 0._PM_REEL
        potentiel%dp(ii) = 0._PM_REEL
        potentiel%t(ii) = 0._PM_REEL
        potentiel%w(ii) = 0._PM_REEL
     end do

     ! Initialisation DES COEFS DES FONCTIONS DE LEGENDRE
     ! note : les tableaux
     potentiel%t(1)=1._pm_reel
     if ( nte_tmp >= 2 ) then
        do n = 2,nte_tmp
           j=n-1
           q=real(n+n-1,kind=pm_reel)
           potentiel%t(n)=q*potentiel%t(j)
        enddo
     endif

     !libération de mémoire
     if (associated(C)) then
        deallocate(C, stat=stat)
     end if
     if (associated(S)) then
        deallocate(S, stat=stat)
     end if

   !end function MSP_creer_potentiel_fichier

   end function MSP_creer_potentiel

   function MSP_creer_potentiel_mu (mu) result (potentiel)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_potentiel_mu
!
!$Resume
!  Création d'un modèle de potentiel à partir d'un mu.
!
!$Description
!  Création d'un modèle de potentiel à partir d'un mu.
!
!$Auteur
!  J.Jaussely
!
!$Acces
!  PUBLIC
!
!$Usage
!  potentiel = MSP_creer_potentiel_mu (mu)
!.    type(MSP_POTENTIEL) :: potentiel
!.    real(KIND=PM_REEL) :: mu
!
!$Arguments
!>E     mu         :<PM_REEL>         constante gravitationelle [m^3/s^2]
!>S     potentiel  :<MSP_POTENTIEL>   
!
!$Common
!
!$Routines
!- MSP_effacer_potentiel
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! POTENTIEL CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 
     implicit none

     ! Arguments
     !==========
     type(MSP_POTENTIEL) :: potentiel
     real(KIND=PM_REEL), intent(in) :: mu
        
     
     ! Initialisations
     !================

     call MSP_effacer_potentiel(potentiel, nul=.true.)
     potentiel%flag_func = .true.
  
     potentiel%mu = mu
     
   end function MSP_creer_potentiel_mu

   subroutine MSP_e_lecpot (ifich,mu,requa,apla,zb,clmb,slmb,degmax,nz,nt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_e_lecpot
!
!$Resume
!  Lecture d'un fichier potentiel au format MSDON.
!
!$Description
!  Lecture d'un fichier potentiel au format MSDON.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_e_lecpot (ifich,mu,requa,apla,zb,clmb,slmb,degmax,nz,nt)
!.    integer :: ifich
!.    real (KIND=pm_reel) :: mu,requa,apla
!.    real (KIND=pm_reel), pointer :: zb(:),clmb(:,:),slmb(:,:)
!.    integer :: degmax
!.    integer :: nz,nt
!
!$Arguments
!>E     ifich   :<integer>                     numéro logique du fichier potentiel
!>S     mu      :<pm_reel>                     constante gravitationelle [m^3/s^2]
!>S     requa   :<pm_reel>                     rayon équatorial [m]
!>S     apla    :<pm_reel>                     inverse de l'aplatissement
!>E/S   zb      :<pm_reel,DIM=(:),pointer>     tableau des zonaux normalisés
!>E/S   clmb    :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" normalisés
!>E/S   slmb    :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" normalisés
!>S     degmax  :<integer>                     degré maximum
!>E/S   nz      :<integer>                     degré des zonaux
!>E/S   nt      :<integer,DIM=(INOUT)>         degré des tesseraux
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
! POTENTIEL LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: ifich
      real (KIND=pm_reel), intent(OUT) :: mu,requa,apla
      real (KIND=pm_reel), pointer :: zb(:),clmb(:,:),slmb(:,:)
      integer, intent(OUT) :: degmax

      integer, intent(INOUT) :: nz,nt

      real (KIND=pm_reel) :: vallue(5)
      integer :: i1(5),i2(5)
      integer :: ier,j, MSP_iostat

      ier = 0
      MSP_iostat = 0

      ! LECTURE DU FICHIER:

      read (ifich,*,end=1000,err=2000)
      read (ifich,*,end=1000,err=2000)

      ! MU, RAYON EQUATORIAL, APLATISSEMENT:

      read (ifich,'(4d16.9)',iostat=ier) mu,requa,apla
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",partie_variable='"lors de la lecture des constantes planéto"', &
                                    routine="MSP_e_lecpot")
         return
      else if ( ier > 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='"des constantes planéto"', &
                                    routine="MSP_e_lecpot")
         return
      endif
      read (ifich,*,end=1000,err=2000) 

      ! DEGRE MAXIMAL

      read (ifich,*,iostat=ier) degmax
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",partie_variable='"lors de la lecture du degré max"', &
                                    routine="MSP_e_lecpot")
         return
      else if ( ier > 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='"du degré max"', &
                                    routine="MSP_e_lecpot")
         return
      endif
      if (associated(zb)) then
         deallocate(zb,stat=MSP_iostat)
      end if
      ALLOCATE(zb(degmax))

      if ( associated(clmb)) then
         deallocate(clmb,stat=MSP_iostat)
      end if
      ALLOCATE(clmb(degmax,degmax))

      if ( associated(slmb)) then
         deallocate(slmb,stat=MSP_iostat)
      end if

      ALLOCATE(slmb(degmax,degmax))

      ! MISE A ZERO DES TABLES:

      zb(:) = 0._pm_reel
      clmb(:,:) = 0._pm_reel
      slmb(:,:) = 0._pm_reel
      i1(:) = 0
      i2(:) = 0

!     POSITIONNEMENT SI DEGRE MAX DEMANDE > DEGRE MAX LU

      if ( nt < 0 ) then
         nt = degmax
      else
        if ( nt > degmax ) then
           call MSP_signaler_message (cle_mes="MSP_potentiel_001",partie_variable='"des tesseraux"')
           nt = degmax
        endif
      endif
      if ( nz < 0 ) then
         nz = degmax
      else
        if ( nz > degmax ) then
           call MSP_signaler_message (cle_mes="MSP_potentiel_001",partie_variable='"des zonaux"')
           nz = degmax
        endif
      endif

      ! COEFFICIENTS ZB

      do while ( (i1(5).ne.1) .or. (i2(5).ne.1) )
         read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",partie_variable='"lors de la lecture des termes zonaux"', &
                                       routine="MSP_e_lecpot")
            return
         else if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='"des termes zonaux"', &
                                       routine="MSP_e_lecpot")
            return
         endif
         do j = 1,5
            if ( (i1(j).le.nz) .and. (i1(j).gt.1) .and. (i2(j).le.nz) ) then
               zb(i1(j)) = vallue (j)
            endif
         enddo
      enddo

      ! COEFFICIENTS CLM BARRE (CLMB)

      i1(5) = 0
      i2(5) = 0
      do while ( (i1(5).ne.1) .or. (i2(5).ne.1) )
         read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",partie_variable='"lors de la lecture des termes tesseraux clmb"', &
                                       routine="MSP_e_lecpot")
            return
         else if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='"des termes tesseraux clmb"', &
                                       routine="MSP_e_lecpot")
            return
         endif
         do j = 1,5
            if ( (i1(j).le.nt) .and. (i1(j).gt.1) .and. (i2(j).le.nt+1) ) then
               clmb (i1(j),i2(j)-1) = vallue (j)
            endif
         enddo
      enddo

      ! COEFFICIENTS SLM BARRE  (SLMB)

         loop_slmb : do while ( .true. )
         read (ifich,9050,iostat=ier) (i1(j),i2(j),vallue(j),j=1,5)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER", &
                 partie_variable='"lors de la lecture des termes tesseraux slmb"', &
                 routine="MSP_e_lecpot")
            return
         else if ( ier > 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE", partie_variable='"des termes termes tesseraux slmb"', &
                 routine="MSP_e_lecpot")
            return
         endif
         do j = 1,5
            if ( (i1(j).le.nt) .and. (i1(j).gt.1) .and. (i2(j).le.nt+1) ) then
               slmb (i1(j),i2(j)-1) = vallue (j)
            endif
            if ( ( (i1(j)==1) .and. (i2(j)==1) ) .or. ( i2(j)==nt+1 ) ) then
               exit loop_slmb
            endif
         enddo
      enddo loop_slmb


      return

 9050 format (5(2i3,f10.5))

 1000 continue
      call MSP_signaler_message (cle_mes="MSP_FIN_FICHIER",routine="MSP_e_lecpot",type=MSP_ENUM_ERREUR)

      return

 2000 continue
      call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_e_lecpot",type=MSP_ENUM_ERREUR)

      return

   end subroutine MSP_e_lecpot

   subroutine MSP_e_denpot (nz,nt,zb,clmb,slmb,zd,clmd,slmd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_e_denpot
!
!$Resume
!  Dénormalisation des coefficients du potentiel terrestre.
!
!$Description
!  Dénormalisation des coefficients du potentiel terrestre.
!  Pour les coefficients tesséraux, le coef de dénormalisation est calculé
!  par le routine interne COMPAS qui permet d'aller jusqu'à ordre/degré 120
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_e_denpot (nz,nt,zb,clmb,slmb,zd,clmd,slmd)
!.    integer :: nz,nt
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!
!$Arguments
!>E     nz    :<integer>                     degré des zonaux
!>E     nt    :<integer,DIM=(IN)>            degré des tesseraux
!>E/S   zb    :<pm_reel,DIM=(:),pointer>     tableau des zonaux normalisés
!>E/S   clmb  :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" normalisés
!>E/S   slmb  :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" normalisés
!>E/S   zd    :<pm_reel,DIM=(:),pointer>     tableau des zonaux dénormalisés
!>E/S   clmd  :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" dénormalisés
!>E/S   slmd  :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" dénormalisés
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
! POTENTIEL DENORMALISER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: nz,nt
      real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
      real (KIND=pm_reel), pointer :: zb(:)
      real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
      real (KIND=pm_reel), pointer :: zd(:)

      ! Variables locales
      real (KIND=pm_reel) :: coef,coeff
      integer :: k,k1,k2, MSP_iostat

      ! Initialisations

      MSP_iostat = 0
      
      ! MISE A ZERO DES TABLES:
      if ( associated(zd)) then
         DEALLOCATE(zd,stat=MSP_iostat)
      end if
      ALLOCATE(zd(size(zb)))

      if ( associated(clmd)) then
         DEALLOCATE(clmd,stat=MSP_iostat)
      end if
      ALLOCATE(clmd(size(clmb,dim=1),size(clmb,dim=2)))

      if ( associated(slmd)) then
         DEALLOCATE(slmd,stat=MSP_iostat)
      end if
      ALLOCATE(slmd(size(slmb,dim=1),size(slmb,dim=2)))

      ! Init des tableaux contenant les coeff dénormalisés
      zd(:) = 0._pm_reel
      clmd(:,:) = 0._pm_reel
      slmd(:,:) = 0._pm_reel

      ! DENORMALISATION DES  ZB
      do k = 1 , nz
         coef = sqrt(2._pm_reel*real(k, kind=pm_reel) + 1._pm_reel)
         zd(k) = coef*zb(k)
      enddo

      ! BOUCLE SUR TOUS LES COEFFICIENTS K1 ET K2

bcl1: do k1 = 1 , nt

bcl2:    do k2 = 1 , k1
         ! L=k1, M=k2
         ! CALCUL DE RACINE[(L-M)!(2L+1)2/(L+M)!] 
         ! par la dénormalisation COMPAS
         coeff = cpsi_denorm(k1,k2)
         ! APPLICATION DU COEFFICIENT DE DENORMALISATION AUX TABLES
         ! CLMB ET SLMB POUR DONNER CLMD ET SLM
         slmd (k1,k2) = coeff * slmb(k1,k2)
         clmd (k1,k2) = coeff * clmb(k1,k2)

         enddo bcl2

      enddo bcl1

      return

   end subroutine MSP_e_denpot

  subroutine MSP_egaler_potentiel(pota,potb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_potentiel
!
!$Resume
!	Fonction = du type MSP_POTENTIEL
!
!$Description
!	Fonction = du type MSP_POTENTIEL
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_potentiel(pota,potb)
!.    type(MSP_POTENTIEL) :: pota
!.    type(MSP_POTENTIEL) :: potb
!
!$Arguments
!>E/S   pota  :<MSP_POTENTIEL>   structure potentiel à gauche du signe égal
!>E     potb  :<MSP_POTENTIEL>   structure potentiel à droite du signe égal
!
!$Common
!
!$Routines
!- MSP_effacer_potentiel
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! POTENTIEL EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    ! Arguments
    !==========
    type(MSP_POTENTIEL), intent(INOUT) :: pota
    type(MSP_POTENTIEL), intent(IN) :: potb

    ! Variables locales
    !==================
    integer :: ndim

    call MSP_effacer_potentiel(pota)
    ! flag_func mis par défaut à false.
    ! flag_func ne doit être mis à true que lorsque c'est une
    ! fonction qui initialise une structure : 
    ! la structure "temporaire" rendue par la fonction sera alors effacée
    pota%flag_func = .false.

    pota%nom_pot = potb%nom_pot
    pota%nomfic = potb%nomfic

    ! Degrés zonaux et tesseraux
    pota%nzo = potb%nzo
    pota%nte = potb%nte

    ! Caractéristiques physiques
    pota%mu = potb%mu
    pota%requa = potb%requa
    pota%apla = potb%apla

    pota%degmax = potb%degmax
    
    if ( associated(potb%zb)) then
       allocate(pota%zb(potb%degmax))
       pota%zb(:) = potb%zb(:)
    end if

    if (associated(potb%clmb)) then
       allocate(pota%clmb(potb%degmax,potb%degmax))
       pota%clmb(:,:) = potb%clmb(:,:)
    end if

    if ( associated(potb%slmb)) then
       allocate(pota%slmb(potb%degmax,potb%degmax))
       pota%slmb(:,:) = potb%slmb(:,:)
    end if

    ! Si le zonaux dénormalisés  de B existent
    if (associated(potb%zd)) then
       allocate(pota%zd(potb%degmax))
       pota%zd(:) = potb%zd(:)
    end if

    ! Affectation si les clmd de B existent
    if ( associated(potb%clmd)) then
       allocate(pota%clmd(potb%degmax,potb%degmax))
       pota%clmd(:,:) = potb%clmd(:,:)
    end if

    ! Affectation si les Slmd de B existent
    if ( associated(potb%slmd)) then
       allocate(pota%slmd(potb%degmax,potb%degmax))
       pota%slmd(:,:) = potb%slmd(:,:)
    end if

    !  Affectation des tableaux des polynomes de Legendre
    
    if (potb%degmax /= 0 .and. associated(potb%zb)) then
       ndim=size(potb%zb,dim=1)

       if (associated(potb%p)) then
          allocate(pota%p(ndim))
          pota%p(:) = potb%p(:)
       end if

       if (associated(potb%dp)) then
          allocate(pota%dp(ndim))
          pota%dp(:) = potb%dp(:)
       end if

       if (associated(potb%w)) then
          allocate(pota%w(ndim))
          pota%w(:) = potb%w(:)
       end if


       if (associated(potb%t)) then
          allocate(pota%t(ndim))
          pota%t(:) = potb%t(:)
       end if

    endif

    if (potb%flag_func) call MSP_effacer_potentiel(potb)

  end subroutine MSP_egaler_potentiel


  subroutine MSP_affecter_potentiel(pota,potb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_affecter_potentiel
!
!$Resume
!	Routine realisant les operations suivantes :
!	- pota = potb
!	- potb est supprimé si il a été créé par une fonction (ex : msp_creer_potentiel)
!               et inchangé sinon
!
!$Description
!
!$Auteur
!   Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_affecter_potentiel(pota,potb)
!.    type(MSP_POTENTIEL) :: pota
!.    type(MSP_POTENTIEL) :: potb
!
!$Arguments
!>E/S   pota  :<MSP_POTENTIEL>   potentiel à affecter
!>E/S   potb  :<MSP_POTENTIEL>   potentiel 
!
!$Common
!
!$Routines
!#V
!- MSP_egaler_potentiel
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! POTENTIEL AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
    implicit none

    type(MSP_POTENTIEL),intent(inout) :: pota
    type(MSP_POTENTIEL) :: potb

    ! Si potb est créé par une fonction, flag_func sera mis à true
    ! et potb sera effacé par MSP_egaler_potentiel

    ! Affectation de pota
    call MSP_egaler_potentiel(pota,potb)

  end subroutine MSP_affecter_potentiel

  SUBROUTINE MSP_consulter_potentiel(potentiel, nom_pot, nomfic, nzo, nte, &
       mu, requa, apla, degmax, zb, clmb, slmb, zd,  clmd, slmd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_potentiel
!
!$Resume
!  Consultation du contenu de la structure potentiel
!
!$Description
!  Consultation du contenu de la structure potentiel
!
!$Auteur
!   Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_potentiel(potentiel, [nom_pot], [nomfic], [nzo], [nte], &
!.           [mu], [requa], [apla], [degmax], [zb], [clmb], [slmb], [zd],  [clmd], [slmd])
!.    type(MSP_POTENTIEL) :: potentiel
!.    character(LEN=MSP_LONG_CHAINE) :: nom_pot
!.    character(LEN=MSP_LONG_NOMFIC) :: nomfic
!.    integer :: nzo
!.    integer :: nte
!.    real (KIND=pm_reel) :: mu
!.    real (KIND=pm_reel) :: requa
!.    real (KIND=pm_reel) :: apla
!.    integer :: degmax
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!
!$Arguments
!>E     potentiel  :<MSP_POTENTIEL>               Structure potentiel à consulter
!>[S]   nom_pot    :<LEN=MSP_LONG_CHAINE>         Nom du modèle de potentiel
!>[S]   nomfic     :<LEN=MSP_LONG_NOMFIC>         Nom du fichier potentiel
!>[S]   nzo        :<integer>                     degré des zonaux
!>[S]   nte        :<integer>                     degré des tesseraux
!>[S]   mu         :<pm_reel>                     Constante de gravitation
!>[S]   requa      :<pm_reel>                     Rayon équatorial
!>[S]   apla       :<pm_reel>                     Aplatissement
!>[S]   degmax     :<integer>                     Ordre maximum du développement du potentiel
!>[E/S] zb         :<pm_reel,DIM=(:),pointer>     tableau des zonaux normalisés
!>[E/S] clmb       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" normalisés
!>[E/S] slmb       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" normalisés
!>[E/S] zd         :<pm_reel,DIM=(:),pointer>     tableau des zonaux dénormalisés
!>[E/S] clmd       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" dénormalisés
!>[E/S] slmd       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" dénormalisés
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
! POTENTIEL CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_POTENTIEL), intent(IN) :: potentiel

    character(LEN=MSP_LONG_CHAINE), intent(OUT), optional  :: nom_pot
    character(LEN=MSP_LONG_NOMFIC), intent(OUT), optional  :: nomfic
    
    integer, intent(OUT), optional :: nzo
    integer, intent(OUT), optional :: nte

    real (KIND=pm_reel), intent(OUT), optional :: mu
    real (KIND=pm_reel), intent(OUT), optional :: requa
    real (KIND=pm_reel), intent(OUT), optional :: apla

    integer, intent(OUT), optional :: degmax

    real (KIND=pm_reel), pointer, optional :: zb(:)
    real (KIND=pm_reel), pointer, optional :: clmb(:,:),slmb(:,:)
    real (KIND=pm_reel), pointer, optional :: zd(:)
    real (KIND=pm_reel), pointer, optional :: clmd(:,:),slmd(:,:)

    integer :: MSP_iostat
    
    MSP_iostat = 0

    if ( present(nom_pot) ) nom_pot = potentiel%nom_pot
    if ( present(nomfic) )  nomfic  = potentiel%nomfic
    if ( present(nzo) )     nzo     = potentiel%nzo
    if ( present(nte) )     nte     = potentiel%nte
    if ( present(mu) )      mu      = potentiel%mu
    if ( present(requa) )   requa   = potentiel%requa
    if ( present(apla) )    apla    = potentiel%apla
    if ( present(degmax) )  degmax  = potentiel%degmax
  
    if ( present(zb) ) then
       if (ASSOCIATED(zb)) DEALLOCATE(zb,stat=MSP_iostat)
       ALLOCATE(zb(size(potentiel%zb)))
       zb(:) = potentiel%zb(:)
    end if
    if ( present(clmb) ) then
       if (ASSOCIATED(clmb)) DEALLOCATE(clmb,stat=MSP_iostat)
       ALLOCATE(clmb(size(potentiel%clmb, dim=1),size(potentiel%clmb, dim=2)))
       clmb(:, :) = potentiel%clmb(:, :)
    end if
    if ( present(slmb) ) then
       if (ASSOCIATED(slmb)) DEALLOCATE(slmb,stat=MSP_iostat)
       ALLOCATE(slmb(size(potentiel%slmb, dim=1), size(potentiel%slmb, dim=2)))
       slmb(:, :) = potentiel%slmb(:, :)
    end if
    if ( present(zd) ) then
       if (ASSOCIATED(zd)) DEALLOCATE(zd,stat=MSP_iostat)
       ALLOCATE(zd(size(potentiel%zd)))
       zd(:) = potentiel%zd(:)
    end if
    if ( present(clmd) ) then
       if (ASSOCIATED(clmd)) DEALLOCATE(clmd,stat=MSP_iostat)
       ALLOCATE(clmd(size(potentiel%clmd, dim=1), size(potentiel%clmd, dim=2)))
       clmd(:, :) = potentiel%clmd(:, :)
    end if
    if ( present(slmd) ) then
       if (ASSOCIATED(slmd)) DEALLOCATE(slmd,stat=MSP_iostat)
       ALLOCATE(slmd(size(potentiel%slmd, dim=1), size(potentiel%clmd, dim=2)))
       slmd(:, :) = potentiel%slmd(:, :)
    end if

  end SUBROUTINE MSP_consulter_potentiel


  SUBROUTINE MSP_modifier_potentiel(potentiel, nom_pot, nomfic, nzo, nte, &
       mu, requa, apla, degmax, zb, clmb, slmb, zd,  clmd, slmd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_potentiel
!
!$Resume
!  Modification  du contenu de la structure potentiel
!
!$Description
!  Modification  du contenu de la structure potentiel
!
!$Auteur
!   Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_potentiel(potentiel, [nom_pot], [nomfic], [nzo], [nte], &
!.           [mu], [requa], [apla], [degmax], [zb], [clmb], [slmb], [zd],  [clmd], [slmd])
!.    type(MSP_POTENTIEL) :: potentiel
!.    character(LEN=*) :: nom_pot
!.    character(LEN=*) :: nomfic
!.    integer :: nzo
!.    integer :: nte
!.    real (KIND=pm_reel) :: mu
!.    real (KIND=pm_reel) :: requa
!.    real (KIND=pm_reel) :: apla
!.    integer :: degmax
!.    real (KIND=pm_reel), pointer :: zb(:)
!.    real (KIND=pm_reel), pointer :: clmb(:,:),slmb(:,:)
!.    real (KIND=pm_reel), pointer :: zd(:)
!.    real (KIND=pm_reel), pointer :: clmd(:,:),slmd(:,:)
!
!$Arguments
!>E/S   potentiel  :<MSP_POTENTIEL>               Structure potentiel à modifier
!>[E]   nom_pot    :<LEN=*>                       Nom du modèle de potentiel
!>[E]   nomfic     :<LEN=*>                       Nom du fichier potentiel
!>[E]   nzo        :<integer>                     degré des zonaux
!>[E]   nte        :<integer>                     degré des tesseraux
!>[E]   mu         :<pm_reel>                     Constante de gravitation
!>[E]   requa      :<pm_reel>                     Rayon équatorial
!>[E]   apla       :<pm_reel>                     Aplatissement
!>[E]   degmax     :<integer>                     Ordre maximum du développement du potentiel
!>[E/S] zb         :<pm_reel,DIM=(:),pointer>     tableau des zonaux normalisés
!>[E/S] clmb       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" normalisés
!>[E/S] slmb       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" normalisés
!>[E/S] zd         :<pm_reel,DIM=(:),pointer>     tableau des zonaux dénormalisés
!>[E/S] clmd       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "CLM" dénormalisés
!>[E/S] slmd       :<pm_reel,DIM=(:,:),pointer>   tableau des tesseraux "SLM" dénormalisés
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
! POTENTIEL MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    !==========
    
    type(MSP_POTENTIEL), intent(INOUT) :: potentiel

    character(LEN=*), intent(IN), optional  :: nom_pot
    character(LEN=*), intent(IN), optional  :: nomfic

    integer, intent(IN), optional :: nzo
    integer, intent(IN), optional :: nte

    real (KIND=pm_reel), intent(IN), optional :: mu
    real (KIND=pm_reel), intent(IN), optional :: requa
    real (KIND=pm_reel), intent(IN), optional :: apla

    integer, intent(IN), optional :: degmax

    real (KIND=pm_reel), pointer, optional :: zb(:)
    real (KIND=pm_reel), pointer, optional :: clmb(:,:),slmb(:,:)
    real (KIND=pm_reel), pointer, optional :: zd(:)
    real (KIND=pm_reel), pointer, optional :: clmd(:,:),slmd(:,:)

    ! Variables locales
    !==================
    integer :: nzo_tmp, nte_tmp, MSP_iostat

    ! Initialisations
    MSP_iostat = 0
    
    if ( present(nom_pot) ) potentiel%nom_pot = nom_pot
     
    if ( .not. present (nzo) ) then
       nzo_tmp = -1
    else
       nzo_tmp = nzo
    endif
    
    if ( .not. present (nte) ) then
       nte_tmp = -1
    else
       nte_tmp = nte
    endif


    if ( present(nomfic) ) then

       ! Appel à MSP_creer_potentiel
       potentiel = MSP_creer_potentiel(nomfic,nzo_tmp,nte_tmp,nom_pot)
       if (MSP_gen_messages("MSP_modifier_potentiel")) return

   else

      if ( present(nzo) )     potentiel%nzo    = nzo
      if ( present(nte) )     potentiel%nte    = nte
      if ( present(mu) )      potentiel%mu     = mu
      if ( present(requa) )   potentiel%requa  = requa
      if ( present(apla) )    potentiel%apla   = apla
      if ( present(degmax) )  potentiel%degmax = degmax
  
      if ( present(zb) ) then
         if (ASSOCIATED(potentiel%zb)) DEALLOCATE(potentiel%zb,stat=MSP_iostat)
         ALLOCATE(potentiel%zb(size(zb)))
         potentiel%zb(:) = zb(:)
      end if
      if ( present(clmb) ) then
         if (ASSOCIATED(potentiel%clmb)) DEALLOCATE(potentiel%clmb,stat=MSP_iostat)
         ALLOCATE(potentiel%clmb(size(clmb, dim=1),size(clmb, dim=2)))
         potentiel%clmb(:, :) = clmb(:, :)
      end if
      if ( present(slmb) ) then
         if (ASSOCIATED(potentiel%slmb)) DEALLOCATE(potentiel%slmb,stat=MSP_iostat)
         ALLOCATE(potentiel%slmb(size(slmb, dim=1), size(slmb, dim=2)))
         potentiel%slmb(:, :) = slmb(:, :)
      end if
      if ( present(zd) ) then
         if (ASSOCIATED(potentiel%zd)) DEALLOCATE(potentiel%zd,stat=MSP_iostat)
         ALLOCATE(potentiel%zd(size(zd)))
         potentiel%zd(:) = zd(:)
      end if
      if ( present(clmd) ) then
         if (ASSOCIATED(potentiel%clmd)) DEALLOCATE(potentiel%clmd,stat=MSP_iostat)
         ALLOCATE(potentiel%clmd(size(clmd, dim=1), size(clmd, dim=2)))
         potentiel%clmd(:, :) = clmd(:, :)
      end if
      if ( present(slmd) ) then
         if (ASSOCIATED(potentiel%slmd)) DEALLOCATE(potentiel%slmd,stat=MSP_iostat)
         ALLOCATE(potentiel%slmd(size(slmd, dim=1), size(clmd, dim=2)))
         potentiel%slmd(:, :) = slmd(:, :)
      end if

   end if

  end SUBROUTINE MSP_modifier_potentiel

   subroutine MSP_afficher_potentiel (potentiel,nz,nt,ilog)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_potentiel
!
!$Resume
!  Affichage  du contenu de la structure potentiel
!
!$Description
!  Affichage  du contenu de la structure potentiel
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_potentiel (potentiel,nz,nt,[ilog])
!.    type(MSP_POTENTIEL) :: potentiel
!.    integer :: nz,nt
!.    integer :: ilog
!
!$Arguments
!>E     potentiel  :<MSP_POTENTIEL>      Structure potentiel à afficher
!>E     nz         :<integer>            nombre de termes zonaux à afficher
!>E     nt         :<integer,DIM=(IN)>   nombre de termes tesseraux à afficher
!>[E/S] ilog       :<integer>            unité logique d'affichage
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
! POTENTIEL AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_POTENTIEL), intent(IN) :: potentiel
      integer, intent(IN) :: nz,nt
      integer, optional :: ilog

      integer :: num, ii, jj
      character(LEN=40) :: fzo,fte

      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif

      write(fzo,'(a,i3,a)') '(a,/,/,',nz,'(1x,e16.10))'
      write(fte,'(a,i3,a,i3,a)') '(a,/,/,',nt,'(',potentiel%degmax,'(1x,e14.8),/))'

      write(num,'(a)') fzo
      write(num,'(a)') fte

      write(num,'(a,a)') "NOM DU POTENTIEL: ",potentiel%nom_pot(1:LEN_TRIM(potentiel%nom_pot))
      write(num,'(a)') ""

      write(num,'(a,i9)') "DEGMAX:  ",potentiel%degmax
      write(num,'(a,g21.12)') "MU:      ",potentiel%mu
      write(num,'(a,g21.12)') "REQUA:   ",potentiel%requa
      write(num,'(a,g21.12)') "APLA:    ",potentiel%apla
      write(num,'(a)') ""
      write(num,fzo) "ZB:    ",potentiel%zb(1:nz)
      write(num,'(a)')
      write(num,fte) "CLMB:  ",potentiel%clmb(1:nt,1:nt)
      write(num,'(a)') ""
      write(num,'(a)') "SLMB:  "
      do ii=1,nt
         do jj=1,nt
            write(num,'(g21.12)') potentiel%slmb(ii,jj)
         end do
      end do
      write(num,'(a)') ""
      write(num,'(a)') "ZD:    "
      do ii=1,nz
         write(num,'(g21.12)') potentiel%zd(ii)
      end do
      write(num,'(a)') ""
      write(num,'(a)') "CLMD:  "
      do ii=1,nt
         do jj=1,nt
            write(num,'(g21.12)') potentiel%clmb(ii,jj)
         end do
      end do
      write(num,'(a)')
      write(num,'(a)') "SLMD:  "
      do ii=1,nt
         do jj=1,nt
            write(num,'(g21.12)') potentiel%slmb(ii,jj)
         end do
      end do
      write(num,'(a)') ""

    end subroutine MSP_afficher_potentiel

  subroutine MSP_calculer_potentiel(pot,y,accpot)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_potentiel
!
!$Resume
!  Fonction permettant de calculer un potentiel d'après un modèle au format GRGS
!
!$Description
!  Cette fonction calcule l acceleration dans un repere planetocentrique a partir
!  d un potentiel et d une position dans le repere planetocentrique.
!  La lecture du modèle de potentiel doit avoir été effectuée par MSP_creer_potentiel.
!  Les modèles au format GRGS sont distribués par COMPAS.
!
!$Auteur
!  Y.TANGUY (ATOS Origin) d'après la routine existante et pspoter (PSIMU)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_potentiel(pot,y,accpot)
!.    type (MSP_POTENTIEL) :: pot 
!.    real (KIND=pm_reel), dimension(3) :: y 
!.    real (KIND=pm_reel), dimension(3) :: accpot 
!
!$Arguments
!>E/S   pot     :<MSP_POTENTIEL>     Structure Potentiel
!>E     y       :<pm_reel,DIM=(3)>   Position dans le repere planetocentrique (à date courante, ou à t0 = t - tsid si tsid présent)
!>S     accpot  :<pm_reel,DIM=(3)>   Acceleration dans le repere planetocentrique (à date courante, ou à t0 = t - tsid si tsid présent)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!#V
!- MSP_poly_legendre_derive
!#
!
!$Include
!
!$Module
!#V
!- MSP_BULLETIN_DEF
!#
!
!$Remarques
!  Le parametre optionnel d optimisation ne doit etre utilisé que si un calcul de
!  potentiel a ete precedemment effectue (dans la meme routine) avec le meme nombre
!  de dimensions
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Argument d'appel
    !=================
    type (MSP_POTENTIEL), intent(INOUT)            :: pot    ! Structure potentiel
    real (KIND=pm_reel), dimension(3), intent(IN)  :: y      ! Position dans le repère planétocentrique
    real (KIND=pm_reel), dimension(3), intent(OUT) :: accpot ! Accéleration dans le repère planétocentrique

    ! Variables locales
    !==================

    ! variables définissant le potentiel
    integer :: nzo
    integer ::nte
    real(kind=PM_REEL)::rxmu
    real(kind=PM_REEL)::requa
    real(kind=PM_REEL),dimension(:),pointer::vj => NULL()
    real(kind=PM_REEL),dimension(:,:),pointer::c => NULL()
    real(kind=PM_REEL),dimension(:,:),pointer::s => NULL()

    integer :: ii,j,l,m
    real (KIND=pm_reel) :: ro2,r2,r,qa,qb,qc,sphi,rtsr,da,db,dc,rl,yy
    real (KIND=pm_reel) :: ro,col,sil,colm,silm,clm,slm,flm,rlm,colma
    real (KIND=pm_reel),dimension(3) :: yc,yz,yt

    nzo = pot%nzo
    nte = pot%nte
    rxmu = pot%mu
    requa = pot%requa
    
    if ( pot%degmax /= 0 ) then 
       if (associated(pot%zd)) then
          vj => pot%zd(1:pot%degmax )
       else
          call MSP_signaler_message(cle_mes="MSP_potentiel_002",partie_variable="zd")
          return
       endif
       if (associated(pot%clmd)) then
          c => pot%clmd(1:pot%degmax ,1:pot%degmax )
       else
          call MSP_signaler_message(cle_mes="MSP_potentiel_002",partie_variable="clmd")
          return
       endif
       if (associated(pot%slmd)) then
          s => pot%slmd(1:pot%degmax ,1:pot%degmax )
       else
          call MSP_signaler_message(cle_mes="MSP_potentiel_002",partie_variable="slmd")
          return
       end if
    endif

    !        TERMES ZONAUX
    !        =============

    !        CONTRIBUTION DU TERME CENTRAL A L'ACCELERATION
    ro2=y(1)*y(1)+y(2)*y(2)
    r2=ro2+y(3)*y(3)

    if(abs(r2) < MSP_EPSILON_APLA) then
       call MSP_signaler_message(cle_mes="MSP_potentiel_003") 
       return
    end if
    
    ! FA 1470 : le contrôle sur la division par 0 est fait ci-dessus
    r=sqrt(r2)
    qa=rxmu/(r2*r)
    qb=qa/r
    sphi=y(3)/r
    rtsr=requa/r
    yc(:)=-qa*y(:)
    if (pot%degmax /=0) then

       !        CALCUL DES POLYNOMES DE LEGENDRE ET DES POLYNOMES DERIVEES
       call MSP_poly_legendre_derive (0,nzo,sphi,pot%w,pot%t,pot%p,pot%dp)
       !        DA=DERIVEE PARTIELLE DU POTENTIEL PAR RAPPORT A R
       !        DB=DERIVEE PARTIELLE DU POTENTIEL PAR RAPPORT A PHI
       da=0._pm_reel
       db=0._pm_reel
       rl=rtsr
       if ( nzo >= 2 ) then
          do l = 2,nzo
             rl=rl*rtsr
             da=da+real(l+1,kind=pm_reel)*rl*pot%p(l)*vj(l)
             db=db+rl*pot%dp(l)*vj(l)
          enddo
       endif
       da=-qa*da
       db=qb*db

       
       !        CONTRIBUTION DES TERMES ZONAUX A L ACCELERATION
       yz(1)=da*y(1)-db*y(1)*y(3)
       yz(2)=da*y(2)-db*y(2)*y(3)
       yz(3)=da*y(3)+db*ro2

       !        TERMES TESSERAUX
       !        ================
       
       !        CALCULS PRELIMINAIRES
       ! FA 1470 : contrôle sur les divisions
       if (ro2>0) then
          qc=rxmu/(r*ro2)
       else
          ! qc sera multiplié par y(1) ou y(2) donc son impact
          ! sera toujours 0 si ro2 vaut 0 
          qc=0_PM_REEL
       endif
       
       !        CALCUL DES PUISSANCES DE DSQRT(1._PM_REEL-X*X)
       yy=sqrt(1._pm_reel-sphi*sphi)
       pot%w(1)=yy
       if ( nte >= 2 ) then
          do ii = 2,nte
             j=ii-1
             pot%w(ii)=yy*pot%w(j)
          enddo
       endif

       !        LONGITUDE DU SATELLITE
       ro=sqrt(ro2)
       ! FA 1470 : contrôle sur les divisions
       ! ro = norme de y(1:2) donc si 0, col et sil restent à 0
       if (ro>0) then
          col=y(1)/ro
          sil=y(2)/ro
       else
          col=0_PM_REEL
          sil=0_PM_REEL
       endif

       !        CALCUL DES QUANTITES DA,DB,DC
       da=0._pm_reel
       db=0._pm_reel
       dc=0._pm_reel
       
       !        BOUCLE SUR L'ORDRE
       colm=1._pm_reel
       silm=0._pm_reel
       rlm=1._pm_reel
       if ( nte >= 2 ) then

          !           CALCUL DE COS(M*LONGITUDE) ET SIN(M*LONGITUDE)
          do m = 1,nte
             colma=colm*col-silm*sil
             silm=silm*col+colm*sil
             colm=colma
             rlm=rlm*rtsr
             
             !              FONCTIONS DE LEGENDRE D'ORDRE M
             call MSP_poly_legendre_derive (m,nte,sphi,pot%w,pot%t,pot%p,pot%dp)
             
             !              BOUCLE SUR LE DEGRE
             rl=rlm
             do l = m,nte
                clm=c(l,m)
                slm=s(l,m)
                flm=clm*colm+slm*silm
                da=da+real(l+1,kind=pm_reel)*rl*pot%p(l)*flm
                db=db+rl*pot%dp(l)*flm
                dc=dc+real(m,kind=pm_reel)*rl*pot%p(l)*(slm*colm-clm*silm)
                rl=rl*rtsr
             enddo

          enddo
       endif



       da=-qa*da
       db=qb*db
       dc=qc*dc

       !        CONTRIBUTION DES TERMES TESSERAUX A L'ACCELERATION
       yt(1)=da*y(1)-db*y(1)*y(3)-dc*y(2)
       yt(2)=da*y(2)-db*y(2)*y(3)+dc*y(1)
       yt(3)=da*y(3)+db*ro2
    else
       yz(:) = 0._pm_reel
       yt(:) = 0._pm_reel
    end if
    !        ACCELERATION TOTALE DUE AU POTENTIEL TERRESTRE
    
    accpot(:)=yc(:)+yz(:)+yt(:)
    
  end subroutine MSP_calculer_potentiel
    

    subroutine MSP_poly_legendre_derive (m,nmax,x,ww,tt,pp,dpp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_poly_legendre_derive
!
!$Resume
!  Calcul des polynômes de Legendre d'ordre m et des polynômes dérivés.
!
!$Description
!  Calcul des polynômes de Legendre d'ordre m et des polynômes dérivés.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  Version 6.0 du 11/05/1999
!
!$Historique
!  Version 6.0 du 11/05/1999
!
!$FinHistorique
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_poly_legendre_derive (m,nmax,x,ww,tt,p,dp)
!.    integer :: m,nmax
!.    real (KIND=pm_reel) :: x
!.    real (KIND=pm_reel),dimension(:),pointer :: ww,tt
!.    real (KIND=pm_reel),dimension(:),pointer :: pp,dpp
!
!$Arguments
!>E     m      :<integer>                   ordre des polynomes
!>E     nmax   :<integer>                   degré maximum: le calcul est fait du degré nmin au degré nmax=max0(1,m)  
!>E     x      :<pm_reel>                   valeur de l'argument
!>E/S   ww     :<pm_reel,DIM=(:),pointer>   tableau des puissances de y=dsqrt(1._pm_reel-x*x)
!>E/S   tt     :<pm_reel,DIM=(:),pointer>   tableau des coefficients des fonctions d'ordre et de degrés égaux
!>E/S   pp     :<pm_reel,DIM=(:),pointer>   polynôme de degré i
!>E/S   dpp    :<pm_reel,DIM=(:),pointer>   dérivée du polynôme de degré i
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
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN)  :: m,nmax
      real (KIND=pm_reel), intent(IN)  :: x
      real (KIND=pm_reel),dimension(:),pointer::ww,tt
      real (KIND=pm_reel),dimension(:),pointer:: pp,dpp

      integer :: nmin,i,j,n,mp
      real (KIND=pm_reel) :: q,r,s
!      real (KIND=pm_reel) :: un

!     TEST POUR PB DE SINGULARITE
! FA 1470 test supprimé : le calcul aux pôle fonctionne
!      un=1._pm_reel-0.1e-12_pm_reel
!      if ( x > un ) then
!          call MSP_signaler_message (cle_mes="MSP_potentiel_002", &
!               routine="MSP_poly_legendre_derive")
!      endif

      nmin=max0(1,m)
      if ( nmax >= nmin ) then
         if ( m == 0 ) then

!           CALCUL DES POLYNOMES DANS LE CAS M=0
            pp(1)=x
            dpp(1)=1._pm_reel
            if (nmax /= 1) then
               pp(2)=1.5_pm_reel*x*x-0.5_pm_reel
               dpp(2)=3._pm_reel*x
               if (nmax /= 2) then
                  do n = 3,nmax
                     i=n-1
                     j=n-2
                     q=real(n,kind=pm_reel)
                     r=real(n+n-1,kind=pm_reel)
                     s=real(i,kind=pm_reel)
                     pp(n)=(r*x*pp(i)-s*pp(j))/q
                     dpp(n)=r*pp(i)+dpp(j)
                  enddo
               endif
            endif
         else

!           CALCUL DES FONCTIONS D'ORDRE M NON NUL
            pp(m)=tt(m)*ww(m)
!## FA 1470 : division par 0
            if (abs(pp(m)) < MSP_EPSILON_APLA) then
               dpp(m)=0_PM_REEL
            else
               dpp(m)=-real(m,kind=pm_reel)*pp(m)*x/ww(2)
            endif

            if (nmax /= m) then
               mp=m+1
               r=real(mp+m,kind=pm_reel)
               pp(mp)=r*x*pp(m)
               dpp(mp)=r*(pp(m)+x*dpp(m))
               if (nmax /= mp) then
                  mp=mp+1
                  do n = mp,nmax
                     i=n-1
                     j=n-2
                     q=real(n-m,kind=pm_reel)
                     r=real(n+n-1,kind=pm_reel)/q
                     s=real(n+m-1,kind=pm_reel)/q
                     pp(n)=r*x*pp(i)-s*pp(j)
                     dpp(n)=r*(pp(i)+x*dpp(i))-s*dpp(j)
                  enddo
               endif
            endif
         endif
      endif

    end subroutine MSP_poly_legendre_derive

end module MSP_POTENTIEL_DEF
