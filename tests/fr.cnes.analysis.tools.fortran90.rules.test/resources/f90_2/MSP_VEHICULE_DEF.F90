module MSP_VEHICULE_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_VEHICULE_DEF
!
!$Resume
!  Module gérant les caractéristiques des véhicules.
!
!$Description
!  Module gérant les caractéristiques des véhicules.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_VEHICULE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_VEHICULE_DEF.F90,v $
!  Revision 1.17  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.16  2009/10/13 10:04:07  kvernelo
!  VERSION::FA-ID:1169:13/10/2009:Traitement d'une erreur de fermeture MADONA
!
!  Revision 1.15  2008/11/24 15:14:39  huec
!  DM-ID 1058 : Correction d\'une erreur
!
!  Revision 1.14  2008/07/04 14:54:57  huec
!  DM-ID 1058 : Initialisation de variable
!
!  Revision 1.13  2008/04/24 13:59:08  huec
!  DM-ID 553 : On impose les formats d ecriture
!
!  Revision 1.12  2007/06/15 13:42:02  vivaresf
!  FA-ID 746 : FA-ID 746 : mise en inout des structures contenant des pointeurs suceptibles d'être écrasés :
!          - egaler_véhicule
!          - affecter_vehicule
!          - lire_vehicule
!          - MSP_consulter_vehicule
!          correction de l'utilisation de trim
!
!  Revision 1.11  2007/02/02 08:28:27  tanguyy
!  DM-ID 659 : rajout d'un type de variation MSP_ENUM_COEFF_NULS pour gerer les init par defaut de structure AERO
!
!  Revision 1.10  2006/06/02 11:21:56  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!
!  Revision 1.9  2005/03/08 07:32:38  fabrec
!  DM-ID 111 : mise à jour des cartouches
!
!  Revision 1.8  2005/01/10 12:48:01  vivaresf
!  FA_321
!  Revision 1.7.2.1  2005/01/07 10:36:20  vivaresf
!  FA_321, version initiale
!  Revision 1.7  2004/11/05 16:27:05  vivaresf
!  coquilles
!  Revision 1.6  2004/10/25 10:15:03  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.5  2002/12/11 10:18:11  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.4  2002/12/04 18:08:26  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:05  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/10/07 09:14:26  adm_ipsi
!  Ajout du paramètre [nul] dans MSP_effacer_vehicule, et appel avec ce paramètre dans MSP_créer_vehicule
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
!  use MSP_VEHICULE_DEF
!
!$Structure
!
!: MSP_VEHICULE : 
!#V
!>     flag_func      : <logical,private>              
!>     nom_vehicule   : <LEN=MSP_LONG_CHAINE,private>  nom du véhicule
!>     mci            : <MSP_MCI,private>              sous-structure contenant les données relatives aux masse/centrage/inertie                   
!>     aero           : <MSP_AERO,private>             sous-structure contenant les données relatives à l'aérodynamique                  
!>     prad           : <MSP_PRAD,private>             sous-structure contenant les données relatives à la pression de radiation solaire                  
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_satellite
!- MSP_assign_satellite
!- MSP_clear_satellite
!- MSP_read_satellite
!- MSP_get_satellite_data
!- MSP_set_satellite_data
!- MSP_display_satellite
!- MSP_effacer_vehicule
!- MSP_affecter_vehicule
!- MSP_lire_vehicule
!- MSP_consulter_vehicule
!- MSP_modifier_vehicule
!- MSP_afficher_vehicule
!#V
!- egaler_vehicule
!#
!
!$Fonctions
!- MSP_creer_vehicule
!- MSP_pt_vehicule_aero
!- MSP_pt_vehicule_mci
!- MSP_pt_vehicule_prad
!
!$Include
!
!$Module
!#V
!- MSP_MCI_DEF
!- MSP_AERO_DEF
!- MSP_PRAD_DEF
!- MSP_ACCES
!#
!
!$Interface
!> msp_read_satellite :      MSP_lire_vehicule
!> assignment :              egaler_vehicule
!> msp_create_satellite :    MSP_creer_vehicule
!> msp_assign_satellite :    MSP_affecter_vehicule
!> msp_set_satellite_data :  MSP_modifier_vehicule
!> msp_display_satellite :   MSP_afficher_vehicule
!> msp_clear_satellite :     MSP_effacer_vehicule
!> msp_get_satellite_data :  MSP_consulter_vehicule
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
!.  egaler_vehicule
!#
!.  MSP_creer_vehicule MSP_pt_vehicule_aero MSP_pt_vehicule_mci MSP_pt_vehicule_prad MSP_create_satellite
!.  MSP_assign_satellite MSP_clear_satellite MSP_read_satellite MSP_get_satellite_data MSP_set_satellite_data
!.  MSP_display_satellite MSP_effacer_vehicule MSP_affecter_vehicule MSP_lire_vehicule MSP_consulter_vehicule
!.  MSP_modifier_vehicule MSP_afficher_vehicule
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSP_MCI_DEF
   use MSP_AERO_DEF
   use MSP_PRAD_DEF
   ! DEFINITIONS DE TYPES:
   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_VEHICULE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'

   type MSP_VEHICULE

      private
      logical :: flag_func
      character(LEN=MSP_LONG_CHAINE) :: nom_vehicule

      type(MSP_MCI)  :: mci
      type(MSP_AERO) :: aero
      type(MSP_PRAD) :: prad

   end type MSP_VEHICULE

   ! SOUS-PROGRAMMES ET FONCTIONS

   private :: egaler_vehicule


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
!  vehiculea=vehiculeb
!.    type(MSP_VEHICULE) :: vehiculea
!.    type(MSP_VEHICULE) :: vehiculeb
!
!$Procedures
!#V
!- egaler_vehicule
!#
!
!$Remarques
!
!$Mots-cles
! VEHICULE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_vehicule
   end interface

   interface MSP_create_satellite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_satellite
!
!$Resume
!  Creates a satellite structure
!
!$Description
!  Creates a satellite structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  vehicule = MSP_create_satellite ([ficveh],[mci],[aero],[prad],[nom_vehicule])
!.    character(LEN=*) :: ficveh
!.    type(MSP_MCI) :: mci
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    character(LEN=*) :: nom_vehicule
!.    type(MSP_VEHICULE) :: vehicule
!
!$Procedures
!- MSP_creer_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_vehicule
   end interface

   interface MSP_assign_satellite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_assign_satellite
!
!$Resume
!  Assign a satellite structure to another
!
!$Description
!  Assign a satellite structure to another
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_assign_satellite(vehiculea,vehiculeb)
!.    type(MSP_VEHICULE) :: vehiculea
!.    type(MSP_VEHICULE) :: vehiculeb
!
!$Procedures
!- MSP_affecter_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_affecter_vehicule
   end interface

   interface MSP_clear_satellite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_satellite
!
!$Resume
!  Clears the content of a satellite structure
!
!$Description
!  Clears the content of a satellite structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_satellite(vehicule,[nul])
!.    type(MSP_VEHICULE) :: vehicule
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_vehicule
   end interface


   interface MSP_read_satellite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_read_satellite
!
!$Resume
!  reads the satellite characteristics from a MADONA file
!
!$Description
!  reads the satellite characteristics from a MADONA file
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_read_satellite (fichier,[mci],[aero],[prad])
!.    character(LEN=*) :: fichier
!.    type (MSP_MCI) :: mci
!.    type (MSP_AERO) :: aero
!.    type (MSP_PRAD) :: prad
!
!$Procedures
!- MSP_lire_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_lire_vehicule
   end interface

   interface MSP_get_satellite_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_satellite_data
!
!$Resume
!  Gets information on satellite characteristics
!
!$Description
!  Gets information on satellite characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_satellite_data(vehicule, [mci], [aero], [prad])
!.    type(MSP_VEHICULE) :: vehicule
!.    type(MSP_PRAD) :: prad
!.    type(MSP_AERO) :: aero
!.    type(MSP_MCI) :: mci
!
!$Procedures
!- MSP_consulter_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_vehicule
   end interface

   interface MSP_set_satellite_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_satellite_data
!
!$Resume
!  Modifies information on satellite characteristics
!
!$Description
!  Modifies information on satellite characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_satellite_data(vehicule, [mci], [aero], [prad])
!.    type(MSP_VEHICULE) :: vehicule
!.    type(MSP_PRAD) :: prad
!.    type(MSP_AERO) :: aero
!.    type(MSP_MCI) :: mci
!
!$Procedures
!- MSP_modifier_vehicule
!
!$Remarques
!
!$Mots-cles
! VEHICULE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_vehicule
   end interface

   interface MSP_display_satellite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_satellite
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
!  call MSP_display_satellite (vehicule,[ilog])
!.    type(MSP_VEHICULE) :: vehicule
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_vehicule
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
      module procedure MSP_afficher_vehicule
   end interface


   contains


    subroutine MSP_effacer_vehicule(vehicule,nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_vehicule
!
!$Resume
!	Routine permettant de désallouer proprement une structure véhicule
!
!$Description
!	Routine permettant de désallouer proprement une structure véhicule
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_vehicule(vehicule,[nul])
!.    type(MSP_VEHICULE) :: vehicule
!.    logical :: nul
!
!$Arguments
!>E/S   vehicule  :<MSP_VEHICULE>   Structure VEHICULE à effacer
!>[E]   nul       :<logical>        =.true., on se contente des instructions NULLIFY (par défaut .false.)
!
!$Common
!
!$Routines
!- MSP_effacer_mci
!- MSP_effacer_aero
!- MSP_effacer_prad
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE EFFACER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_VEHICULE)::vehicule
      logical, intent(in), optional :: nul

     logical :: nul_tmp

     if ( present (nul) ) then
       nul_tmp = nul
     else
       nul_tmp = .false.
     endif

      call MSP_effacer_mci(vehicule%mci)
      call MSP_effacer_aero(vehicule%aero,nul=nul_tmp)
      call MSP_effacer_prad(vehicule%prad)
      vehicule%nom_vehicule=""


    end subroutine MSP_effacer_vehicule


    subroutine egaler_vehicule(vehiculea,vehiculeb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_vehicule
!
!$Resume
!	Routine definissant l'affectation de 2 structures vehicules
!
!$Description
!	Routine definissant l'affectation de 2 structures vehicules
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_vehicule(vehiculea,vehiculeb)
!.    type(MSP_VEHICULE) :: vehiculea
!.    type(MSP_VEHICULE) :: vehiculeb
!
!$Arguments
!>E/S   vehiculea  :<MSP_VEHICULE>   structure VEHICULE à gauche de =
!>E     vehiculeb  :<MSP_VEHICULE>   structure VEHICULE à droite de =
!
!$Common
!
!$Routines
!- MSP_effacer_vehicule
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE EGALER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_VEHICULE), intent(inout) ::vehiculea
      type(MSP_VEHICULE), intent(in) ::vehiculeb

      call MSP_effacer_vehicule(vehiculea)
      vehiculea%flag_func=.false.
      vehiculea%nom_vehicule = vehiculeb%nom_vehicule
      vehiculea%mci=vehiculeb%mci
      vehiculea%aero=vehiculeb%aero
      vehiculea%prad=vehiculeb%prad

      if ( vehiculeb%flag_func) then
         call MSP_effacer_vehicule(vehiculeb)
      end if
    end subroutine egaler_vehicule

    subroutine MSP_affecter_vehicule(vehiculea,vehiculeb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_affecter_vehicule
!
!$Resume
!	Routine realisant les operation ssuivantes:
!	- vehiculea = vehiculeb
!	- supprimer vehiculeb
!
!$Description
!	Routine realisant les operations suivantes:
!	- vehiculea = vehiculeb
!	- supprimer vehiculeb
!
!$Auteur
!   J.J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_affecter_vehicule(vehiculea,vehiculeb)
!.    type(MSP_VEHICULE) :: vehiculea
!.    type(MSP_VEHICULE) :: vehiculeb
!
!$Arguments
!>S     vehiculea  :<MSP_VEHICULE>   structure VEHICULE à gauche de =
!>E/S   vehiculeb  :<MSP_VEHICULE>   structure VEHICULE à droite de =
!
!$Common
!
!$Routines
!- MSP_effacer_vehicule
!#V
!- egaler_vehicule
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE AFFECTER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      implicit none

      type(MSP_VEHICULE),intent(inout)::vehiculea
      type(MSP_VEHICULE)::vehiculeb

      call egaler_vehicule(vehiculea,vehiculeb)
      call MSP_effacer_vehicule(vehiculeb)
    end subroutine MSP_affecter_vehicule

   function MSP_creer_vehicule (ficveh,mci,aero,prad,nom_vehicule) result (vehicule)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_vehicule
!
!$Resume
!  Création d'une structure contenant les caractéristiques d'un véhicule.
!
!$Description
!  Création d'une structure contenant les caractéristiques d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vehicule = MSP_creer_vehicule ([ficveh],[mci],[aero],[prad],[nom_vehicule])
!.    character(LEN=*) :: ficveh
!.    type(MSP_MCI) :: mci
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!.    character(LEN=*) :: nom_vehicule
!.    type(MSP_VEHICULE) :: vehicule
!
!$Arguments
!>[E]   ficveh        :<LEN=*>          nom du fichier véhicule
!>[E]   mci           :<MSP_MCI>        sous-structure contenant les données masse/centrage/inertie
!>[E]   aero          :<MSP_AERO>       sous-structure contenant les données aérodynamiques
!>[E]   prad          :<MSP_PRAD>       sous-structure contenant les données relatives à la pression de radiation solaire
!>[E/S] nom_vehicule  :<LEN=*>          nom du véhicule
!>S     vehicule      :<MSP_VEHICULE>   structure contenant les données relatives à un véhicule
!
!$Common
!
!$Routines
!- MSP_effacer_vehicule
!- MSP_signaler_message
!- MSP_lire_vehicule
!
!$Include
!
!$Module
!
!$Remarques
!  Si un nom de fichier est présent, les appels aux autres paramètres ne sont pas pris en compte.
!
!$Mots-cles
!  VEHICULE CREER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      character(LEN=*), intent(IN), optional    :: ficveh

      type(MSP_MCI), intent(IN), optional  :: mci
      type(MSP_AERO), intent(IN), optional :: aero
      type(MSP_PRAD), intent(IN), optional :: prad

      character(LEN=*), optional :: nom_vehicule

      type(MSP_VEHICULE) :: vehicule

      integer :: lnom
      character(LEN=80),dimension(2)       :: tmessage_var


      call MSP_effacer_vehicule(vehicule,nul=.true.)

      vehicule%flag_func = .true.

      if ( present(nom_vehicule) ) then
         lnom = LEN_TRIM(nom_vehicule)
         if ( lnom <= MSP_LONG_CHAINE ) then 
            vehicule%nom_vehicule  = nom_vehicule
         else
            vehicule%nom_vehicule  = nom_vehicule(1:MSP_LONG_CHAINE)
            tmessage_var(1) = 'Le nom du véhicule'
            write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_vehicule",type=MSP_ENUM_WARNING,partie_variable=tmessage_var)
         endif
      else
         vehicule%nom_vehicule = ""
      endif

      ! Initialisations de toutes les données véhicule:

      if ( present(ficveh) ) then
         call MSP_lire_vehicule (ficveh,mci=vehicule%mci,aero=vehicule%aero,prad=vehicule%prad)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_creer_vehicule",type=MSP_ENUM_ERREUR)
         endif
         return
      endif

      ! Initialisations des données de type MCI:

      if ( present(mci) ) then
         vehicule%mci = mci
      else
         vehicule%mci = MSP_creer_mci ()
      endif

      ! Initialisations des données relatives au frottement atmosphérique:

      if ( present(aero) ) then
         vehicule%aero = aero
      else
         vehicule%aero = MSP_creer_aero (type_coef=MSP_ENUM_COEFF_AERO_VITESSE,type_variation=MSP_ENUM_COEFF_NULS)
      endif

      ! Initialisations des données relatives à la pression de radiation solaire:

      if ( present(prad) ) then
         vehicule%prad = prad
      else
         vehicule%prad = MSP_creer_prad ()
      endif

   end function MSP_creer_vehicule


  subroutine MSP_lire_vehicule (fichier,mci,aero,prad)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_vehicule
!
!$Resume
!  Lecture dans un fichier des caractéristiques d'un véhicule.
!
!$Description
!  Lecture dans un fichier des caractéristiques d'un véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_vehicule (fichier,[mci],[aero],[prad])
!.    character(LEN=*) :: fichier
!.    type (MSP_MCI) :: mci
!.    type (MSP_AERO) :: aero
!.    type (MSP_PRAD) :: prad
!
!$Arguments
!>E     fichier  :<LEN=*>      nom du fichier
!>[S]   mci      :<MSP_MCI>    structure contenant les données relatives 
!                              aux masses/centrage/inertie d'un véhicule
!>[S]   aero     :<MSP_AERO>   structure contenant les données relatives
!                              à l'aérodynamique d'un véhicule
!>[S]   prad     :<MSP_PRAD>   structure contenant les caractéristiques d'un véhicule
!                              pour la pression de radiation solaire
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_lire_str_mci
!- MSP_lire_str_aero
!- MSP_lire_str_prad
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
!  VEHICULE LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_ACCES

      implicit none

      character(LEN=*), intent(IN) :: fichier
      type (MSP_MCI), intent(OUT), optional  :: mci
      type (MSP_AERO), intent(INOUT), optional :: aero
      type (MSP_PRAD), intent(OUT), optional :: prad

      integer :: ier,acc

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Ouverture et chargement des données !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ier = MSP_acc_charger_donnees (fichier,acc)
      if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_OUVERTURE_FICHIER",partie_variable=fichier, &
                                    routine="MSP_lire_vehicule",type=MSP_ENUM_ERREUR)
         return
      endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Lecture des données liées aux Masse/centrage/inertie !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ( present (mci) ) then
         call MSP_lire_str_mci (acc,mci)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_vehicule",type=MSP_ENUM_ERREUR)
         endif
      endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Lecture des données liées aux coefficients aérodynamiques !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ( present (aero) ) then
         call MSP_lire_str_aero (acc,aero)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_vehicule",type=MSP_ENUM_ERREUR)
         endif
      endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Lecture des données liées aux coefficients de pression de radiation solaire !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ( present (prad) ) then
         call MSP_lire_str_prad (acc,prad)
         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_ERREUR",routine="MSP_lire_vehicule",type=MSP_ENUM_ERREUR)
         endif
      endif

      !!!!!!!!!!!!!!!!!!!!!!!!
      ! Fermeture du fichier !
      !!!!!!!!!!!!!!!!!!!!!!!!

      ier = acc_close(acc)
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_probleme_close", &
              routine="MSP_lire_vehicule",type=MSP_ENUM_ERREUR)
         return
      endif
      
   end subroutine MSP_lire_vehicule

 
   SUBROUTINE MSP_consulter_vehicule(vehicule, mci, aero, prad)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_vehicule
!
!$Resume
!  Consulter les caractéristiques d'un véhicule
!
!$Description
!  Consulter les caractéristiques d'un véhicule
!
!$Auteur
!  Jean-Jacques Wasbuer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_vehicule(vehicule, [mci], [aero], [prad])
!.    type(MSP_VEHICULE) :: vehicule
!.    type(MSP_PRAD) :: prad
!.    type(MSP_AERO) :: aero
!.    type(MSP_MCI) :: mci
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE>   Structure véhicule à consulter
!>[S]   mci       :<MSP_MCI>        copie de la structure MCI contenue dans véhicule
!>[S]   aero      :<MSP_AERO>       copie de la structure AERO contenue dans véhicule
!>[S]   prad      :<MSP_PRAD>       copie de la structure PRAD contenue dans véhicule
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
!  VEHICULE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     type(MSP_VEHICULE), intent(IN) :: vehicule
     type(MSP_PRAD), intent(OUT), optional :: prad
     type(MSP_AERO), intent(INOUT), optional :: aero
     type(MSP_MCI),  intent(OUT), optional :: mci

     if (PRESENT(mci))  mci  = vehicule%mci
     if (PRESENT(prad)) prad = vehicule%prad
     if (PRESENT(aero)) aero = vehicule%aero

   end SUBROUTINE MSP_consulter_vehicule


   function MSP_pt_vehicule_aero(vehicule) result (aero)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_vehicule_aero
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
!  aero = MSP_pt_vehicule_aero(vehicule)
!.    type(MSP_VEHICULE),target :: vehicule
!.    type(MSP_AERO), pointer :: aero
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE,target>   
!>S     aero      :<MSP_AERO,pointer>      
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

     type(MSP_VEHICULE), intent(IN),target :: vehicule
     type(MSP_AERO), pointer :: aero

     aero => vehicule%aero
   end function MSP_pt_vehicule_aero

   function MSP_pt_vehicule_mci(vehicule) result (mci)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_vehicule_mci
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
!  mci = MSP_pt_vehicule_mci(vehicule)
!.    type(MSP_VEHICULE),target :: vehicule
!.    type(MSP_MCI), pointer :: mci
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE,target>   
!>S     mci       :<MSP_MCI,pointer>       
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

     type(MSP_VEHICULE), intent(IN),target :: vehicule
     type(MSP_MCI), pointer :: mci

     mci => vehicule%mci
   end function MSP_pt_vehicule_mci

   function MSP_pt_vehicule_prad(vehicule) result (prad)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_vehicule_prad
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
!  prad = MSP_pt_vehicule_prad(vehicule)
!.    type(MSP_VEHICULE),target :: vehicule
!.    type(MSP_PRAD), pointer :: prad
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE,target>   
!>S     prad      :<MSP_PRAD,pointer>      
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

     type(MSP_VEHICULE), intent(IN),target :: vehicule
     type(MSP_PRAD), pointer :: prad

     prad => vehicule%prad
   end function MSP_pt_vehicule_prad


   SUBROUTINE MSP_modifier_vehicule(vehicule, mci, aero, prad)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_vehicule
!
!$Resume
!  Modification des caractéristiques d'une structure véhicule
!
!$Description
!  Modification des caractéristiques d'une structure véhicule
!
!$Auteur
!   Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_vehicule(vehicule, [mci], [aero], [prad])
!.    type(MSP_VEHICULE) :: vehicule
!.    type(MSP_PRAD) :: prad
!.    type(MSP_AERO) :: aero
!.    type(MSP_MCI) :: mci
!
!$Arguments
!>E/S   vehicule  :<MSP_VEHICULE>   Structure véhicule à modifier
!>[E]   mci       :<MSP_MCI>        Structure MCI à mettre dans la structure véhicule
!>[E]   aero      :<MSP_AERO>       Structure AERO à mettre dans la structure véhicule
!>[E]   prad      :<MSP_PRAD>       Structure PRAD à mettre dans la structure véhicule
!
!$Common
!
!$Routines
!- MSP_effacer_mci
!- MSP_effacer_prad
!- MSP_effacer_aero
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VEHICULE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     type(MSP_VEHICULE), intent(INOUT) :: vehicule
     type(MSP_PRAD), intent(IN), optional :: prad
     type(MSP_AERO), intent(IN), optional :: aero
     type(MSP_MCI),  intent(IN), optional :: mci

     if (PRESENT(mci)) then
        call MSP_effacer_mci(vehicule%mci)
        vehicule%mci  = mci
     endif
     if (PRESENT(prad)) then
        call MSP_effacer_prad(vehicule%prad)
        vehicule%prad = prad
     endif
     if (PRESENT(aero)) then
        call MSP_effacer_aero(vehicule%aero)
        vehicule%aero = aero
     endif

   end SUBROUTINE MSP_modifier_vehicule




    subroutine MSP_afficher_vehicule (vehicule,ilog)
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_vehicule
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
!  call MSP_afficher_vehicule (vehicule,[ilog])
!.    type(MSP_VEHICULE) :: vehicule
!.    integer :: ilog
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE>   Structure vehicule à afficher
!>[E/S] ilog      :<integer>        Numéro de l'unité logique d'affichage
!
!$Common
!
!$Routines
!- MSP_afficher_MCI
!- MSP_afficher_aero
!- MSP_afficher_prad
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

      type(MSP_VEHICULE), intent(IN) :: vehicule
      integer, optional :: ilog

      integer :: num

      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif

      write(num,'(a,a)') "NOM DU VEHICULE: ",trim(vehicule%nom_vehicule)
      write(num,'(a)') ""
      
      call MSP_afficher_MCI(vehicule%mci,ilog=ilog)
      call MSP_afficher_aero(vehicule%aero,ilog=ilog)
      call MSP_afficher_prad(vehicule%prad,ilog=ilog)
      
    end subroutine MSP_afficher_vehicule

end module MSP_VEHICULE_DEF
