module MSP_PRAD_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PRAD_DEF
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
!  $Id: MSP_PRAD_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_PRAD_DEF.F90,v $
!  Revision 1.15  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.14  2009/09/04 12:58:39  tanguyy
!  DM-ID 1113 : Fermeture des accÃ©s MADONA une fois le fichier ouvert
!
!  Revision 1.13  2008/11/19 13:38:19  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.12  2008/11/03 17:10:35  huec
!  DM-ID 733 : Changement de module pour les routines de calcul de pression de radiation solaire
!  Revision 1.11  2008/11/03 16:53:49  huec
!  DM-ID 733 : Changement de module pour les routines de calcul de pression de radiation solaire
!  Revision 1.10  2008/11/03 16:02:04  huec
!  DM-ID 733 : Mise a jour de l\'entete
!  Revision 1.9  2008/11/03 15:05:19  huec
!  DM-ID 733 : Creation des routines pour le calcul de l acceleration de pression de radiation solaire
!  Revision 1.8  2008/08/08 14:55:32  gss
!  DM-ID 1058 : (portage g95) ajout du traitement d'erreur en sortie de la fonction
!  acc_charger_donnees.
!  Revision 1.7  2008/04/24 13:59:05  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.6  2007/06/18 10:14:25  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.5  2005/03/08 07:32:36  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.4  2002/12/11 10:18:10  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.3  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.2  2002/12/03 17:21:03  adm_ipsi
!   Ajout de implicit none
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
!  use MSP_PRAD_DEF
!
!$Structure
!
!: MSP_PRAD : 
!#V
!>     cmp       : <pm_reel,private>              coefficient multiplicatif sur la force                   
!>     ka        : <pm_reel,private>              coefficient d'absorbtion                   
!>     ks        : <pm_reel,private>              coefficient de spécularité                   
!>     kd        : <pm_reel,private>              coefficient de diffusion                   
!>     ficprad   : <LEN=MSP_LONG_NOMFIC,private>  nom du fichier                   
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_radiative_coeff
!- MSP_clear_radiative_coeff
!- MSP_get_prad_data
!- MSP_set_prad_data
!- MSP_display_prad
!- MSP_effacer_prad
!- MSP_lire_str_prad
!- MSP_consulter_prad
!- MSP_modifier_prad
!- MSP_afficher_prad
!#V
!- egaler_prad
!#
!
!$Fonctions
!- MSP_creer_prad
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
!> assignment :                  egaler_prad
!> msp_create_radiative_coeff :  MSP_creer_prad
!> msp_display_prad :            MSP_afficher_prad
!> msp_get_prad_data :           MSP_consulter_prad
!> msp_clear_radiative_coeff :   MSP_effacer_prad
!> msp_set_prad_data :           MSP_modifier_prad
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
!.  egaler_prad
!#
!.  MSP_creer_prad MSP_create_radiative_coeff MSP_clear_radiative_coeff MSP_get_prad_data
!.  MSP_set_prad_data MSP_display_prad MSP_effacer_prad MSP_lire_str_prad MSP_consulter_prad
!.  MSP_modifier_prad MSP_afficher_prad
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB, only : pm_reel
   use MSP_MECASPA_DEF
   use MSP_GESTION_ERREUR

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PRAD_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   ! DEFINITIONS DE TYPES:


   type MSP_PRAD

      private

      real (KIND=pm_reel) :: cmp
      real (KIND=pm_reel) :: ka,ks,kd      

      character(LEN=MSP_LONG_NOMFIC) :: ficprad

   end type MSP_PRAD

 
   ! SOUS-PROGRAMMES ET FONCTIONS

   private :: egaler_prad


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
!  prada=pradb
!.    type(MSP_prad) :: prada
!.    type(MSP_prad) :: pradb
!
!$Procedures
!#V
!- egaler_prad
!#
!
!$Remarques
!
!$Mots-cles
! VEHICULE PRESSION_RADIATION EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_prad
   end interface


   interface MSP_create_radiative_coeff

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_radiative_coeff
!
!$Resume
!  Creates an radiative coefficients structure
!
!$Description
!  Creates an radiative coefficients structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  prad = MSP_create_radiative_coeff ([ficprad],[cmp],[ka],[ks],[kd])
!.    character(LEN=*) :: ficprad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!.    type(MSP_PRAD) :: prad
!
!$Procedures
!- MSP_creer_prad
!
!$Remarques
!
!$Mots-cles
! VEHICULE PRESSION_RADIATION CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_prad
   end interface


   interface MSP_clear_radiative_coeff

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_radiative_coeff
!
!$Resume
!  Clears the content of a radiative coefficients structure
!
!$Description
!  Clears the content of a radiative coefficients structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_radiative_coeff(prad)
!.    type(MSP_prad) :: prad
!
!$Procedures
!- MSP_effacer_prad
!
!$Remarques
!
!$Mots-cles
! VEHICULE PRESSION_RADIATION EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_prad
   end interface

   interface MSP_get_prad_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_prad_data
!
!$Resume
!  Gets information on radiative coefficients characteristics
!
!$Description
!  Gets information on radiative coefficients characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_prad_data (prad,[ficprad],[cmp],[ka],[ks],[kd])
!.    type(MSP_PRAD) :: prad
!.    character(LEN=*) :: ficprad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!
!$Procedures
!- MSP_consulter_prad
!
!$Remarques
!
!$Mots-cles
! VEHICULE PRESSION_RADIATION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_prad
   end interface
   interface MSP_set_prad_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_prad_data
!
!$Resume
!  Modifies information on radiative coefficients characteristics
!
!$Description
!  Modifies information on radiative coefficients characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_prad_data (prad,[cmp],[ka],[ks],[kd])
!.    type(MSP_PRAD) :: prad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!
!$Procedures
!- MSP_modifier_prad
!
!$Remarques
!
!$Mots-cles
! VEHICULE PRESSION_RADIATION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_prad
   end interface

   interface MSP_display_prad

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_prad
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
!  call MSP_display_prad (prad,[ilog])
!.    type(MSP_PRAD) :: prad
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_prad
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
      module procedure MSP_afficher_prad
   end interface


   contains

    subroutine egaler_prad(prada,pradb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_prad
!
!$Resume
!	Routine definissant l'affectation de 2 structures pression de radiation solaire
!
!$Description
!	Routine definissant l'affectation de 2 structures pression de radiation solaire
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_prad(prada,pradb)
!.    type(MSP_prad) :: prada
!.    type(MSP_prad) :: pradb
!
!$Arguments
!>S     prada  :<MSP_prad>   structure PRAD à gauche de =
!>E     pradb  :<MSP_prad>   structure PRAD à droite de =
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
!  VEHICULE PRESSION_RADIATION EGALER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_prad),intent(out) :: prada
      type(MSP_prad),intent(in) :: pradb

      prada%cmp =  pradb%cmp
      prada%ka  = pradb%ka 
      prada%ks  = pradb%ks
      prada%kd  =   pradb%kd     
      
      prada%ficprad= pradb%ficprad
      
    end subroutine egaler_prad

    subroutine MSP_effacer_prad(prad)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_prad
!
!$Resume
!	Routine permettant de désallouer proprement une structure pression de radiation solaire
!
!$Description
!	Routine permettant de désallouer proprement une structure pression de radiation solaire
!
!$Auteur
!       J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_prad(prad)
!.    type(MSP_prad) :: prad
!
!$Arguments
!>E/S   prad  :<MSP_prad>   structure PRAD à effacer
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
!  VEHICULE PRESSION_RADIATION EFFACER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_prad) :: prad

      prad%cmp = 0._PM_REEL
      prad%ka  = 0._PM_REEL
      prad%ks  = 0._PM_REEL
      prad%kd  = 0._PM_REEL       
      
      prad%ficprad=""
      
    end subroutine MSP_effacer_prad

   function MSP_creer_prad (ficprad,cmp,ka,ks,kd) result (prad)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_prad
!
!$Resume
!  Création d'une structure contenant les caractéristiques d'un véhicule pour la pression de radiation solaire.
!
!$Description
!  Création d'une structure contenant les caractéristiques d'un véhicule pour la pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  prad = MSP_creer_prad ([ficprad],[cmp],[ka],[ks],[kd])
!.    character(LEN=*) :: ficprad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!.    type(MSP_PRAD) :: prad
!
!$Arguments
!>[E]   ficprad  :<LEN=*>      nom du fichier
!>[E]   cmp      :<pm_reel>    coefficient multiplicatif sur la force [par défaut 1.]
!>[E]   ka       :<pm_reel>    coefficient d'absorbtion [par défaut 1.]
!>[E]   ks       :<pm_reel>    coefficient de spécularité [par défaut 0.]
!>[E]   kd       :<pm_reel>    coefficient de diffusion [par défaut 0.]
!>S     prad     :<MSP_PRAD>   structure contenant les caractéristiques
!                              d'un véhicule pour la pression de radiation solaire
!
!$Common
!
!$Routines
!- MSP_signaler_message
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
!  Si un nom de fichier est présent, les appels aux autres paramètres ne sont pas pris en compte.
!  Normalement, ka + ks + kd = 1
!
!$Mots-cles
!  VEHICULE PRESSION_RADIATION CREER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use MSP_ACCES
      implicit none

      character(LEN=*), intent(IN), optional   :: ficprad
      real(kind=pm_reel), intent(IN), optional ::cmp,ka,ks,kd

      type(MSP_PRAD) :: prad

      integer::ier,acc

      if ( present(cmp) ) then
         prad%cmp = cmp
      else
         prad%cmp = 1._pm_reel
      endif

      if ( present(ficprad) ) then
         prad%ficprad = ficprad
         ier = MSP_acc_charger_donnees (ficprad,acc)
         ! DM1058 - Gestion d'erreur
         if (ier < 0) then
            call MSP_signaler_message (cle_mes="MSP_chargement_donnees", &
                 routine="MSP_creer_prad",partie_variable=trim(ficprad))
         endif
         call MSP_lire_str_prad(acc,prad)

         ! fermeture de l'accès MADONA
         call MSP_fermeture_MADONA(acc)

         if ( MSP_ERREUR ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",routine="MSP_creer_prad",type=MSP_ENUM_ERREUR)
         endif
         return
      endif

      if ( present(ka) ) then
         prad%ka = ka
      else
         prad%ka = 1._pm_reel
      endif

      if ( present(ks) ) then
         prad%ks = ks
      else
         prad%ks = 0._pm_reel
      endif

      if ( present(kd) ) then
         prad%kd = kd
      else
         prad%kd = 0._pm_reel
      endif

   end function MSP_creer_prad

   subroutine MSP_lire_str_prad (acc,prad)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_str_prad
!
!$Resume
!  Lecture des caractéristiques d'un véhicule pour la pression de radiation solaire.
!
!$Description
!  Lecture dans une zone de moyen d'accès MADONA des caractéristiques
!  d'un véhicule pour la pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_lire_str_prad (acc,prad)
!.    integer :: acc
!.    type(MSP_PRAD) :: prad
!
!$Arguments
!>E     acc   :<integer>    numéro identificateur de la zone de moyen d'accès MADONA
!>S     prad  :<MSP_PRAD>   structure contenant les caractéristiques d'un véhicule
!                           pour la pression de radiation solaire
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
!  VEHICULE PRESSION_RADIATION LIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(IN) :: acc
      type(MSP_PRAD), intent(OUT) :: prad

      integer :: ier

      ier = acc_select (acc,"PRAD",ACC_STRUCT)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure PRAD', &
                                    routine="MSP_lire_str_prad",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture du coefficient de spécularité:

      ier = acc_getd (acc,"ka",prad%ka,"")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable="du coefficient d'absorbtion", &
                                    routine="MSP_lire_str_prad",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture du coefficient de spécularité:

      ier = acc_getd (acc,"ks",prad%ks,"")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du coefficient de spécularité', &
                                    routine="MSP_lire_str_prad",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture du coefficient de diffusion:

      ier = acc_getd (acc,"kd",prad%kd,"")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du coefficient de diffusion', &
                                    routine="MSP_lire_str_prad",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Fin de la lecture

      ier = acc_select_end (acc)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la structure PRAD', &
                                    routine="MSP_lire_str_prad",type=MSP_ENUM_ERREUR)
         return
      endif

   end subroutine MSP_lire_str_prad

   
   subroutine  MSP_consulter_prad (prad,ficprad,cmp,ka,ks,kd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_prad
!
!$Resume
!  Consultation des caractéristiques de la pression de radiation solaire
!
!$Description
!  Consultation des caractéristiques de la pression de radiation solaire
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_prad (prad,[ficprad],[cmp],[ka],[ks],[kd])
!.    type(MSP_PRAD) :: prad
!.    character(LEN=*) :: ficprad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!
!$Arguments
!>E     prad     :<MSP_PRAD>   Structure PRAD à consulter
!>[S]   ficprad  :<LEN=*>      Nom du fichier contenant les infos de pression de radiation
!>[S]   cmp      :<pm_reel>    coefficient multiplicatif sur la force 
!>[S]   ka       :<pm_reel>    coefficient d'absorbtion 
!>[S]   ks       :<pm_reel>    coefficient de spécularité
!>[S]   kd       :<pm_reel>    coefficient de diffusion
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
!  VEHICULE PRESSION_RADIATION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_PRAD), intent(IN) :: prad
      character(LEN=*), intent(OUT), optional   :: ficprad
      real(kind=pm_reel), intent(OUT), optional ::cmp,ka,ks,kd


      if ( present(ficprad) ) ficprad = prad%ficprad
      if ( present(cmp) )     cmp     = prad%cmp
      if ( present(ka) )      ka      = prad%ka
      if ( present(ks) )      ks      = prad%ks
      if ( present(kd) )      kd      = prad%kd

    end subroutine MSP_consulter_prad

   subroutine  MSP_modifier_prad (prad,cmp,ka,ks,kd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_prad
!
!$Resume
!  Modification des caractéristiques de la pression de radiation solaire
!
!$Description
!  Modification des caractéristiques de la pression de radiation solaire
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_prad (prad,[cmp],[ka],[ks],[kd])
!.    type(MSP_PRAD) :: prad
!.    real(kind=pm_reel) :: cmp,ka,ks,kd
!
!$Arguments
!>E/S   prad  :<MSP_PRAD>   structure contenant les caractéristiques à modifier
!>[E]   cmp   :<pm_reel>    coefficient multiplicatif sur la force
!>[E]   ka    :<pm_reel>    coefficient d'absorbtion
!>[E]   ks    :<pm_reel>    coefficient de spécularité
!>[E]   kd    :<pm_reel>    coefficient de diffusion
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
!  VEHICULE PRESSION_RADIATION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_PRAD), intent(INOUT) :: prad
      real(kind=pm_reel), intent(IN), optional ::cmp,ka,ks,kd

      if ( present(cmp) ) prad%cmp = cmp
      if ( present(ka) )  prad%ka = ka
      if ( present(ks) )  prad%ks = ks
      if ( present(kd) )  prad%kd = kd

    end subroutine MSP_modifier_prad

    subroutine MSP_afficher_prad (prad,ilog)
         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_prad
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
!  call MSP_afficher_prad (prad,[ilog])
!.    type(MSP_PRAD) :: prad
!.    integer :: ilog
!
!$Arguments
!>E     prad  :<MSP_PRAD>   
!>[E/S] ilog  :<integer>    Numéro de l'unité logique d'affichage
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
!  VEHICULE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_PRAD), intent(IN) :: prad
      integer, optional :: ilog

      integer :: num

      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif

      write(num,'(a,g21.12)') "CMP:     ",prad%cmp
      write(num,'(a,g21.12)') "KA:      ",prad%ka
      write(num,'(a,g21.12)') "KS:      ",prad%ks
      write(num,'(a,g21.12)') "KD:      ",prad%kd
      write(num,'(a)')

    end subroutine MSP_afficher_prad

  end module MSP_PRAD_DEF
