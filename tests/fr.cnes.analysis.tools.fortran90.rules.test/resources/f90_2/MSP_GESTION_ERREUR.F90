module MSP_gestion_erreur

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_gestion_erreur
!
!$Resume
!	Module de gestion des erreurs
!
!$Description
!	Ce Module permet de gérer les erreurs et les warnings sans transmettre de code erreur (ier) 
!-	Le schéma de fonctionnement est le suivant :
!.	- Lorsque l'on veut signaler un problème dans une routine routine1 il suffit de faire appel à MSP_signaler_message
!.	(call MSP_signaler_message(....))
!.	- Pour savoir s'il y a eu un problème à la suite de l'appel à la routine "routine1", il suffit de consulter les erreurs et warnings
!-	ex:
!.	call routine1(...)
!.	if (MSP_erreur) then
!.	   ! action si une erreur est détectée
!.	....
!.	if (MSP_warning) then
!.	   ! action si un warning est détecté
!.	....
!
!$Auteur
!	S. ROUSSEAU CS SI
!
!$Version
!	$Id: MSP_GESTION_ERREUR.F90 363 2013-02-18 10:06:08Z bbjc $
!
!$Historique
!	$Log: MSP_GESTION_ERREUR.F90,v $
! Revision 363 2013/02/18 bbjc
! DM-ID 1513: Suppression des warnings de compilation

!	Revision 1.29  2008/10/17 14:09:17  huec
!	AQ : Correction de cartouche
!	
!	Revision 1.28  2008/10/15 14:19:08  tanguyy
!	AQ : suppression d'une variable inutilisee
!	
!	Revision 1.27  2008/09/29 15:56:41  huec
!	AQ : Correction
!	
!	Revision 1.26  2008/08/04 09:36:37  gss
!	DM-ID 1058 : portage g95. Initialisation à NULL des pointeurs. Suppression
!	de variables non utilisées. Ajout d'un warning lorsque le fichier de messages
!	recherché n'existe pas et ajout d'une erreur lorsque le code message
!	demandé n'existe dans aucun fichier message.
!	Modification de la recherche des fichiers message par chemin absolu ou relatif
!	et par MADONA_RC_PATH.
!	
!	Revision 1.25  2008/03/17 14:52:25  huec
!	DM-ID 553 : Portage Solaris 10
!	
!	Revision 1.24  2007/06/28 14:34:05  tanguyy
!	Correction du message d'erreur 'flight dynamics team'
!	
!	Revision 1.23  2007/06/18 07:17:45  vivaresf
!	Version 1.8 : action qualité, rajout des stat= aux deallocate
!	
!	Revision 1.22  2007/05/29 11:06:58  vivaresf
!	Version 1.8 : mise à jour des cartouches
!	
!	Revision 1.21  2007/02/22 17:19:05  vivaresf
!	DM-ID 655 : correction du message par défaut et modification du test de sur le chargement d'un fichier en relatif
!	on s'arrete lorsque le fichier est chargé
!	
!	Revision 1.20  2006/11/07 17:18:26  vivaresf
!	Version 1.7 : validation
!
!	Revision 1.19  2006/10/12 15:27:40  vivaresf
!	DM-ID 559 : suppression info sur extraire_mot_cle_message
!	
!	Revision 1.18  2006/10/12 14:58:35  vivaresf
!	DM-ID 425 : utilisation de trim/len_trim, lisibilité du code
!	
!       DM-ID 559 : routine expand_message, vérification que les `"' detectes
!            par scan sont bien entre 2 et len_trim des chaines de caracteres
!            testees (sinon on met ces  `"' dans les message et provoque des
!            core dump aleatoires).
!       AQ version 1-7 : renommage variable locale i en ii (recup_message)
!       AQ version 1-7 : rajout des implicit none manquant (4)
!       AQ version 1-7 : suppression des routines inutilisées de la liste
!          des routines explicitement recuperees dans les modules MSLIB et MSPRO
!       AQ version 1-7 : suppression des variables inutilisées (message understand)
!       AQ version 1-7 : suppression de la routine extraire_mot_cle_message
!
!	Revision 1.17  2006/06/02 11:22:32  vpg
!	DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!	
!	Revision 1.16  2005/03/02 11:14:19  vivaresf
!	DM 111 : sauvegarde des variables a propager
!	
!	Revision 1.15  2003/12/02 15:45:16  adm_ipsi
!	FA-ID 86 : message%non_vide à false si pas de message à afficher
!	
!	Revision 1.14  2003/07/15 14:52:17  adm_ipsi
!	FA-ID 6 : Libération de la mémoire dans recuperer_message_chaine et recuperer_message_type
!	
!	Revision 1.13  2003/07/11 09:51:13  adm_ipsi
!	FA-ID 37 : Re-initialisation du nombre de message maximum. Corrections de fuites mémoire
!	
!	Revision 1.12  2003/03/14 14:56:14  adm_ipsi
!	signaler_mesage, suppression de l'argument ier_mspro et utilisation de la fonction mzpro_code_retour avec ier_mslib
!	
!	Revision 1.11  2003/03/14 13:39:05  adm_ipsi
!	Modification du commentaire sur MSP_ajouter_fichier_message
!	
!	Revision 1.10  2003/03/04 14:46:18  adm_ipsi
!	 Correction d'un bug sur l'initilaisation du tableau  liste_routine, et utilisation du paramètre enum_len_fic (S. Rousseau)
!	
!	Revision 1.9  2003/02/19 10:32:55  adm_ipsi
!	signaler_message, ajout du parametre ier_mspro pour la gestion des erreurs mspro
!	
!	Revision 1.8  2003/02/12 14:38:22  adm_ipsi
!	Nouvelle version livrée par S. Rousseau
!	
!	Revision 1.3  2002/12/09 16:42:11  adm_ipsi
!	Utilisation du parametre MAGE_ENUM_ECRAN pour l'affichage à l'ecran
!	Revision 1.2  2002/12/05 14:36:48  adm_ipsi
!	Ajout des implcit none
!	Revision 1.1.1.1  2002/11/29 15:25:46  vivaresf
!	Corrections mineures par rapport a la MECASPA V2-1
!	- variable nb_mes_plus a 200 au lieu de 100
!	- MSP_fic_default rebatise en MAGE_fic_default
!	Revision 1.12  2002/05/03 07:49:52  util_am
!	Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!	Revision 1.11  2001/09/14 12:07:58  util_am
!	correction d'un bug :
!	        L'ajout d'un fichier de description des erreurs peut se produire avant ou apres l'appel a signaler message
!	        Le chargement du fichier associe est limite au premier trouve (contrairement a la version precedente ou tous les fichiers rencontres ete ouverts)
!	Revision 1.10  2001/08/28 09:32:28  util_am
!	changements dans des recopies de tableaux pour éviter des écrasements mémoire (bug vu lors du passage au compilo Forte 6.1)
!	Revision 1.9  2000/06/14 16:08:13  util_am
!	- Correction d'un bug concernant l'ajout d'un fichier de message par son chemin absolu
!	- Ajout des interface anglaises
!	- Mise à jour des cartouches
!	Revision 1.8  2000/02/11 12:45:16  rousseau
!	ajout de la fonction MSP_gen_messages
!	Revision 1.7  1999/10/14 14:20:22  rousseau
!	modification de la subroutine MSP_effacer_message pour pouvoir effacer n messages
!	Revision 1.6  1999/08/04 07:32:51  util_am
!	Création de la routine MSP_annuler_probleme.
!	Ajout du type propagation.
!	Correction d'un bug pour la gestion des messages MSLIB.
!	Possibilité d'appeler la routine MSP_afficher_message même s'il n'y a pas de messages.
!	Revision 1.5  1999/08/02 14:23:07  util_am
!	Idem précédente modif
!	Revision 1.4  1999/08/02 09:31:00  util_am
!	Idem modif précédente
!	Revision 1.3  1999/08/02 07:45:13  util_am
!	Idem modif précédente
!	Revision 1.2  1999/08/02 07:39:17  util_am
!	Correction d'erreur dans signaler_message pour des messages provenant de la MSLIB90
!	Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!	Version 1.0 de MECASPA mise sous CVS
!
!$Usage
!  use MSP_gestion_erreur
!
!$Structure
!
!: MSP_message : Structure contenant l'historique des messages
!#V
!>     routine       : <LEN=PM_len_routine,DIM=(:),pointer,private>  tableau contenant la liste des routines 
!>     erreur        : <LEN=PM_len_code_pb,DIM=(:),pointer,private>  tableau contenant la liste des codes erreurs
!>     message       : <LEN=PM_len_message,DIM=(:),pointer,private>  tableau contenant la liste des libellés des messages
!>     warning       : <logical,DIM=(:),pointer,private>             tableau indiquant si les divers messages sont des erreurs ou des warnings
!>     non_vide      : <logical,private>                             
!#
!
!: MSP_type_erreur : Structure contenant toute l'information relative à une erreur
!#V
!>     routine       : <LEN=PM_len_routine,private>                  nom de la routine
!>     erreur        : <LEN=PM_len_code_pb,private>                  code PB
!>     message       : <LEN=PM_len_message,private>                  libellé du message
!>     warning       : <logical,private>                             flag warning (TRUE) ou erreur (FALSE)
!>     developpeur   : <logical,private>                             flag developpeur (TRUE) ou utilisateur (FALSE)
!#
!
!$Global
!
!>  PM_len_code_pb         : <integer,parameter>                        Longueur des chaînes de caractères contenant le nom de l'erreur (ou du warning)
!-	Si on veut utiliser une variable libelle_erreur il faut la déclarer de la façon suivante :
!-	(ex :character(len=PM_len_code_pb) :: libelle_erreur)
!-	il est important de respecter les formats des chaînes de caractères sous peine de fonctionnement "bizarre"
!>  PM_len_routine         : <integer,parameter>                        Longueur des chaînes de caractères contenant le nom de la routine       
!>  PM_len_message         : <integer,parameter>                        Longueur des chaînes de caractères contenant le libellé du message      
!>  MSP_enum_warning       : <integer,parameter>                        Flag indiqunt qu'il s'agit d'un warning
!>  MSP_enum_erreur        : <integer,parameter>                        Flag indiquant qu'il s'agit d'une erreur
!>  MSP_enum_propagation   : <integer,parameter>                        Flag indiquant que le type de l'erreur ou du warning et le meme qu'avant
!>  MSP_enum_developpeur   : <logical,parameter>                        Flag indiquant que le message est de niveau développeur
!>  MSP_enum_utilisateur   : <logical,parameter>                        Flag indiquant que le message est de niveau utilisateur
!>  MSP_dernier_message    : <integer,parameter>                        Flag permettant de récupérer le dernier message
!>  MSP_tous_messages      : <integer,parameter>                        Flag permettant de récupérer tous les messages
!>  MAGE_fic_default       : <LEN=250>                                  Nom du fichier descripteur des messages par défauts
!>  MAGE_ENUM_ECRAN        : <integer,parameter>                        Sortie standard (écran) = 6
!>  MSP_erreur             : <logical>                                  Booléen indiquant si une erreur s'est produite            
!>  MSP_warning            : <logical>                                  Booléen indiquant si un warning s'est produit            
!>  MSP_probleme           : <logical>                                  Booléen indiquant si un warning ou une erreur s'est produit
!#V
!>  mode_developpeur       : <logical,private>                          Booléen indiquant si on est en mode développeur ou utilisateur
!>  stop_erreur            : <logical,private>                          Booléen indiquant si on doit arrêter en cas de PB
!>  nb_mes_plus            : <integer,parameter,private>                Nombre de messages définis à priori
!>  n_mes_max              : <integer,private>                          Nombre max courant de messages autorisés
!>  tab_message            : <MSP_type_erreur,DIM=(:),pointer,private>  Tableau contenant l'historique de tous les messages
!>  nb_message             : <integer,private>                          entier indiquant le nombre de messages générés
!>  code_courant           : <LEN=PM_len_code_pb,private>               Valeur courante du code erreur (remarque : lorsqu'il n'y a pas d'erreur
!								 cette chaîne est vide)
!>  routine_courant        : <LEN=PM_len_routine,private>               Valeur courante du nom de la routine (remarque : lorsqu'il n'y a pas d'erreur
!								 cette chaîne est vide)
!>  message_courant        : <LEN=PM_len_message,private>               Valeur courante du message (remarque : lorsqu'il n'y a pas d'erreur
!								 cette chaîne est vide)
!>  nb_max_fic             : <integer,parameter,private>                entier indiquant le nombre max de fichiers descripteurs autorisés (10)
!>  tab_acc                : <integer,DIM=(nb_max_fic),private>         tableau contenant l'ensemble des fichiers descripteurs chargés
!>  acc_cour               : <integer,private>                          entier indiquant le nombre de fichiers ouverts
!>  nfic_cour              : <integer,private>                          entier inquant le nombre de nom de fichiers descripteurs
!>  non_init               : <logical,private>                          booléen indiquant si les fichiers descripteurs ont été chargés ou non
!>  message_nul            : <MSP_type_erreur,parameter,private>        Message Nul
!>  message_default        : <MSP_type_erreur,parameter,private>        Message par défaut
!-	Routine     : UNKNOWN
!-	Message     : PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!-	Code Erreur : FATAL
!#
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_recuperer_message
!- MSP_afficher_message
!- MSP_send_message
!- MSP_get_message
!- MSP_display_message
!- MSP_clear_message
!- MSP_expand_message
!- MSP_cancel_problem
!- MSP_set_user_mode
!- MSP_set_developer_mode
!- MSP_stop_at_problem_occurence
!- MSP_add_message_file
!- expand_message_tab
!- MSP_effacer_message
!- MSP_annuler_probleme
!- MSP_mode_utilisateur
!- MSP_mode_developpeur
!- MSP_stopper_en_cas_de_pb
!- MSP_ajouter_fichier_message
!#V
!- augmente_taille_message
!- affecte_chaine_tab
!- affecte_tab_chaine
!- insere_chaine
!- egaler_MSP_message
!- expand_message
!- signaler_message
!- signaler_message_tab
!- recuperer_message_chaine
!- recuperer_message_type
!- afficher_message_chaine
!- afficher_message_type
!#
!
!$Fonctions
!- MSP_comparer_erreur
!- MSP_gen_messages
!#V
!- charge_conf
!- recup_message
!- comparer_erreur_code_erreur
!#
!
!$Include
!#V
!- acces_F.h
!- AMenv_F.h
!- domtrad_F.h
!#
!
!$Module
!#V
!- MSLIB
!#
!
!$Interface
!> assignment :                     egaler_MSP_message
!> msp_send_message :               signaler_message, signaler_message_tab
!> msp_add_message_file :           MSP_ajouter_fichier_message
!> msp_clear_message :              MSP_effacer_message
!> msp_afficher_message :           afficher_message_chaine, 
!                                   afficher_message_type
!> msp_signaler_message :           signaler_message, signaler_message_tab
!> operator(.equal.) :              comparer_erreur_code_erreur
!> msp_expand_message :             expand_message_tab
!> msp_get_message :                recuperer_message_chaine, 
!                                   recuperer_message_type
!> msp_set_user_mode :              MSP_mode_utilisateur
!> msp_cancel_problem :             MSP_annuler_probleme
!> msp_recuperer_message :          recuperer_message_chaine, 
!                                   recuperer_message_type
!> msp_set_developer_mode :         MSP_mode_developpeur
!> msp_display_message :            afficher_message_chaine, 
!                                   afficher_message_type
!> msp_stop_at_problem_occurence :  MSP_stopper_en_cas_de_pb
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ERREUR
!
!$Voir-Aussi
!#V
!.  charge_conf recup_message comparer_erreur_code_erreur augmente_taille_message affecte_chaine_tab
!.  affecte_tab_chaine insere_chaine egaler_MSP_message expand_message 
!.  signaler_message signaler_message_tab recuperer_message_chaine recuperer_message_type
!.  afficher_message_chaine afficher_message_type
!#
!.  MSP_comparer_erreur MSP_gen_messages MSP_signaler_message MSP_recuperer_message MSP_afficher_message
!.  MSP_send_message MSP_get_message MSP_display_message MSP_clear_message MSP_expand_message
!.  MSP_cancel_problem MSP_set_user_mode MSP_set_developer_mode MSP_stop_at_problem_occurence
!.  MSP_add_message_file expand_message_tab MSP_effacer_message MSP_annuler_probleme MSP_mode_utilisateur
!.  MSP_mode_developpeur MSP_stopper_en_cas_de_pb MSP_ajouter_fichier_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSLIB, only : tm_code_retour,PM_nom_routine,PM_signification_code_retour,PM_identification_routine
  use MSPRO , only : mzpro_code_retour, pm_nom_biblio

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_GESTION_ERREUR.F90 363 2013-02-18 10:06:08Z bbjc $'


#include "acces_F.h"
#include "AMenv_F.h"
#include "domtrad_F.h"

  !  Définition de la longueur du nom de l'erreur
  integer,parameter :: PM_len_code_pb = 100 
  integer,parameter :: PM_len_routine = PM_nom_routine+PM_identification_routine+3
  integer,parameter :: PM_len_message = PM_len_code_pb + PM_len_routine + PM_signification_code_retour+100
  integer,parameter :: MSP_enum_warning = 1
  integer,parameter :: MSP_enum_erreur = 2
  integer,parameter :: MSP_enum_propagation = 3
  logical,parameter :: MSP_enum_developpeur = .true.
  logical,parameter :: MSP_enum_utilisateur = .false.
  integer,parameter :: MSP_dernier_message = 1
  integer,parameter :: MSP_tous_messages = -12
  character(len=250):: MAGE_fic_default = "MAG_MESSAGES"

  ! Sortie ecran standard

  integer,parameter :: MAGE_ENUM_ECRAN = 6
  

  !  Définition du type erreur
  type MSP_type_erreur
     private
     character(len=PM_len_routine) :: routine
     character(len=PM_len_code_pb) :: erreur
     character(len=PM_len_message) :: message
     logical:: warning
     logical:: developpeur
  end type MSP_type_erreur

  type MSP_message
     private
     character(len=PM_len_routine),pointer,dimension(:) :: routine => NULL()
     character(len=PM_len_code_pb),pointer,dimension(:) :: erreur => NULL()
     character(len=PM_len_message),pointer,dimension(:) :: message => NULL()
     logical,pointer,dimension(:) :: warning => NULL()
     logical :: non_vide = .false.
  end type MSP_message

  ! Définition des variables globales publiques
  
  ! booleen indiquant le signalement d'une erreur
  logical,save :: MSP_erreur = .false.
  ! booleen indiquant le signalement d'un warning
  logical,save :: MSP_warning = .false.
  ! booleen indiquant le signalement d'un warning ou d'une erreur
  logical,save :: MSP_probleme = .false.
  
  ! booleen indiquant le mode de fonctionnment des messages "developpeur" ou "utilisateur"
  logical,private,save :: mode_developpeur = .false.
  
  ! booleen autorisant l'arret du programme en cas d'erreur
  logical,private :: stop_erreur = .false.
  
  ! Définition des variables globales privées
  integer,parameter,private:: nb_mes_plus = 200
  !  Dimension initiale des erreurs
  integer,private,save::n_mes_max = nb_mes_plus
  !  Gestion de l'historique des erreurs
  type (MSP_type_erreur),dimension(:),pointer,private,save :: tab_message => NULL()
  integer,private,save :: nb_message = 0

  
  ! Variable privée contenant le code du message courant
  character(len=PM_len_code_pb) ,private,save:: code_courant = ""
  character(len=PM_len_routine) ,private,save:: routine_courant = ""
  character(len=PM_len_message) ,private,save:: message_courant = ""
  
  ! Gestion des différents fichiers messages erreurs
  integer,parameter,private:: nb_max_fic = 10
  integer,dimension(nb_max_fic),private,save :: tab_acc
  integer,private,save::acc_cour
  integer,private,save :: nfic_cour = 0
  logical,private,save ::non_init = .true.
  type(MSP_type_erreur) ,private,parameter :: message_nul = MSP_type_erreur("","","",.false.,.false.)
  type(MSP_type_erreur) ,private,parameter :: message_default = MSP_type_erreur("ROUTINE : UNKNOWN",&
       "FATAL","PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM",.false.,.false.)
  
  
  private :: charge_conf
  private :: recup_message
  private :: expand_message
  private :: comparer_erreur_code_erreur
  private :: recuperer_message_chaine
  private :: recuperer_message_type
  private :: egaler_MSP_message
  private :: afficher_message_chaine
  private :: afficher_message_type
  private :: signaler_message
  private :: signaler_message_tab
  private :: augmente_taille_message
  private :: insere_chaine
  private :: affecte_tab_chaine
  private :: affecte_chaine_tab


  interface operator (.equal.)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.equal.)
!
!$Resume
!	Fonction logique permettant de comparer deux chaînes des longueurs quelconques
!
!$Description
!	Fonction logique permettant de comparer deux chaînes des longueurs quelconques
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = code_erreur1.equal.code_erreur2
!.    character(len=*) :: code_erreur1,code_erreur2
!
!$Procedures
!#V
!- comparer_erreur_code_erreur
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR COMPARER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure comparer_erreur_code_erreur
  end interface
  interface MSP_signaler_message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_signaler_message
!
!$Resume
!	Routine permettant de signaler un erreur ou un warning
!
!$Description
!	Cette routine permet de générer une erreur ou un warning.
!-	Il y a diverse niveau de fonctionnement :
!.	 1 : A partir d'un mot clé
!.		Le libellé du message ainsi que le libellé de l'erreur associé sont décrit dans un fichier descripteur
!.	 2 : En renseignant directement le message , l'erreur,...
!.	 3 : A partir d'une erreur de type MSLIB
!-	De plus il y a 2 niveaux de message :
!.	 1 : niveau "utilisateur" (mode fonctionnement par défaut)(  
!	il est possible de repsser dans ce mode avec la routine MSP_mode_utilisateur)
!.	 2 : niveau "developpeur" (pour activer ce mode de fonctionnement il faut faire appel à la routine MSP_mode_developpeur)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_signaler_message([cle_mes],[message],[partie_variable],&
!.           init,routine,&
!.           code,type,developpeur,ier_mslib)
!.    character(len=*) :: cle_mes
!.    character(len=*) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    type(tm_code_retour) :: ier_mslib
!.    logical :: developpeur
!.    logical :: init
!
!  call MSP_signaler_message([cle_mes],[message],partie_variable,&
!.           init,routine,&
!.           code,type,developpeur)
!.    character(len=*) :: cle_mes
!.    character(len=*) ,dimension(:) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    logical :: developpeur
!.    logical :: init
!
!$Procedures
!#V
!- signaler_message
!- signaler_message_tab
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR SIGNALER MESSAGE TABLEAU
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure signaler_message,signaler_message_tab
  end interface
  interface MSP_recuperer_message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_recuperer_message
!
!$Resume
!	Routine permettant de récupérer l'historique des messages
!
!$Description
!	Cette routine posséde 2 modes de fonctionnement :
!-	1) Routine permettant de récupérer un certain nombre de paramètres liés au message	
!.	. Le dernier message
!.	. La dernière routine qui a générée un message
!.	. Le libellé de l'erreur ou du warning
!.	. La liste des messages (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. La liste des routines (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. La liste des libellés erreur (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. les liste des flags warning ou erreur (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!-	2) Routine permettant de récupérer les messages dans une structure MSP_message
!-	Il existe trois mode de fonctionnements:
!.		1) Récupérer le dernier message, il s'agit du mode par défaut (nb_mes absent ou nb_mes = MSP_dernier_message)
!.		2) Récupérer tous les messages (nb_mes = MSP_tous_messages)
!.		3) Récupérer le n derniers messages (nb_mes = n) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_recuperer_message([message],[routine],[code_pb],[liste_message],&
!.            liste_routine,liste_code_pb,liste_type_pb)
!.    character(len=*) :: message
!.    character(len=* ) :: routine
!.    character(len=*) :: code_pb
!.    character(len=*),dimension(:),pointer :: liste_message
!.    character(len=* ),dimension(:),pointer :: liste_routine
!.    character(len=*),dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!  call MSP_recuperer_message(message,[nb_mes])
!.    type(MSP_message) :: message
!.    integer :: nb_mes
!
!$Procedures
!#V
!- recuperer_message_chaine
!- recuperer_message_type
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR RECUPERER MESSAGE CHAINE TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure recuperer_message_chaine,recuperer_message_type
  end interface

  interface MSP_afficher_message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_message
!
!$Resume
!	Routine permettant d'afficher un message à l'écran
!
!$Description
!	Routine permettant d'afficher un message à l'écran
!-	Cette routine permet d'afficher des erreurs et des warnings
!-	Tous les élément du message sont optionels
!-	Exemples de message:
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code erreur : FATAL)
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	 (code warning : Warning)
!.	*** WARNING:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!-	Il existe des modes de fonctionnement :
!.	1) En passant les éléments que l'on veut afficher sous forme de chaînes de
!.		de caractéres.
!.	2) En passant la structure Message
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_message([unit],[message],[routine],[code_pb],[type_pb],&
!.           liste_message,liste_routine,liste_code_pb,liste_type_pb)
!.    integer :: unit
!.    character(len=*) :: message
!.    character(len=*) :: routine
!.    character(len=*) :: code_pb
!.    logical :: type_pb
!.    character(len=PM_len_message) ,dimension(:),pointer :: liste_message
!.    character(len=PM_len_routine) ,dimension(:),pointer :: liste_routine
!.    character(len=PM_len_code_pb) ,dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!  call MSP_afficher_message(message,[unit],[flag_routine],[flag_message],[flag_code_pb])
!.    type (MSP_message) :: message
!.    integer :: unit
!.    logical :: flag_routine
!.    logical :: flag_message
!.    logical :: flag_code_pb
!
!$Procedures
!#V
!- afficher_message_chaine
!- afficher_message_type
!#
!
!$Remarques
!	Cette routine posséde deux modes de fonctionnement suivant les arguments que l'on utilise
!.	1) On peut afficher un message à partir des différents éléments définis par des chaînes de caractères
!.	2) On peut afficher un message à partir de la structure MSP_message
!
!$Mots-cles
! ERREUR AFFICHER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     module procedure afficher_message_chaine,afficher_message_type
  end interface

  interface assignment (=)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!	Routine permettant d'égaler deux types messages
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  message1=message2
!.    type(MSP_message) :: message1
!.    type(MSP_message) :: message2
!
!$Procedures
!#V
!- egaler_MSP_message
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR MESSAGE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_MSP_message
  end interface

  interface MSP_send_message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_send_message
!
!$Resume
!	This routine sends a warning or an error to the error management 
!
!$Description
!	This routine allow to send a warning or an error
!-	Several type of use are available:
!.	 1 : From a key word
!.		The wording of the message and of the error is in a file
!.	 2 : Giving directly the message, the error ...
!.	 3 : From an mslib error
!-	Moreover there are two messages levels :
!.	 1 : "user" level (default) (it is possible to go back into this mode with the MSP_set_user_mode)
!.	 2 : "developper" level (to switch to this mode, the MSP_set_developper_mode function is used)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_send_message([cle_mes],[message],[partie_variable],&
!.           init,routine,&
!.           code,type,developpeur,ier_mslib)
!.    character(len=*) :: cle_mes
!.    character(len=*) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    type(tm_code_retour) :: ier_mslib
!.    logical :: developpeur
!.    logical :: init
!
!  call MSP_send_message_tab([cle_mes],[message],[partie_variable],&
!.           init,routine,&
!.           code,type,developpeur)
!.    character(len=*) :: cle_mes
!.    character(len=*) ,dimension(:) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    logical :: developpeur
!.    logical :: init
!
!$Procedures
!#V
!- signaler_message
!- signaler_message_tab
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR SIGNALER MESSAGE TABLEAU
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure signaler_message,signaler_message_tab
  end interface
  
  interface MSP_get_message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_message
!
!$Resume
!	Getting the message history 
!
!$Description
!	This routine has two modes :
!-	1) This routine allow to get some attributes of the error message history
!.	. The last message
!.	. The last routine that has sent the message
!.	. The wording of the error or the warning
!.	. The message list (user level or developper level depends on the mode)
!.	. The routine list (user level or developper level depends on the mode)
!.	. The error wording list (user level or developper level depends on the mode)
!.	. The warnings or errors flag list (user level or developper level depends on the mode)
!-	2) This routine allow to get the messages in a MSP_message structure
!-	Three modes are available:
!.		1) Get the last message, this is the default mode (nb_mes absent or nb_mes = MSP_dernier_message)
!.		2) Get the whole set of messages (nb_mes = MSP_tous_messages)
!.		3) Get the last n messages (nb_mes = n) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_message([message],[routine],[code_pb],[liste_message],&
!.            liste_routine,liste_code_pb,liste_type_pb)
!.    character(len=*) :: message
!.    character(len=* ) :: routine
!.    character(len=*) :: code_pb
!.    character(len=*),dimension(:),pointer :: liste_message
!.    character(len=* ),dimension(:),pointer :: liste_routine
!.    character(len=*),dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!  call MSP_get_message(message,[nb_mes])
!.    type(MSP_message) :: message
!.    integer :: nb_mes
!
!$Procedures
!#V
!- recuperer_message_chaine
!- recuperer_message_type
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR RECUPERER MESSAGE CHAINE TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure recuperer_message_chaine,recuperer_message_type
  end interface
  
  interface MSP_display_message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_message
!
!$Resume
!	Display an error/warning message in a logical unit
!
!$Description
!	Display an error/warning message in a logical unit
!-	All the message features are optional
!-	Examples :
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code erreur : FATAL)
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	 (code warning : Warning)
!.	*** WARNING:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!-	Two mdes are available :
!.	1) Passing the elements that are to be displayed in a character string form.
!.	2) Passing an MSP_message structure.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_message([unit],[message],[routine],[code_pb],[type_pb],&
!.           liste_message,liste_routine,liste_code_pb,liste_type_pb)
!.    integer :: unit
!.    character(len=*) :: message
!.    character(len=*) :: routine
!.    character(len=*) :: code_pb
!.    logical :: type_pb
!.    character(len=PM_len_message) ,dimension(:),pointer :: liste_message
!.    character(len=PM_len_routine) ,dimension(:),pointer :: liste_routine
!.    character(len=PM_len_code_pb) ,dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!  call MSP_display_message(message,[unit],[flag_routine],[flag_message],[flag_code_pb])
!.    type (MSP_message) :: message
!.    integer :: unit
!.    logical :: flag_routine
!.    logical :: flag_message
!.    logical :: flag_code_pb
!
!$Procedures
!#V
!- afficher_message_chaine
!- afficher_message_type
!#
!
!$Remarques
!
!$Mots-cles
! ERREUR AFFICHER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure afficher_message_chaine,afficher_message_type
  end interface
  
  
  interface MSP_clear_message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_message
!
!$Resume
!	Reset the message stack
!
!$Description
!	This routine cancel any error or warning and reset the message stack
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_message([nb_mes])
!.    integer :: nb_mes
!
!$Procedures
!- MSP_effacer_message
!
!$Remarques
!
!$Mots-cles
! ERREUR EFFACER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_effacer_message
  end interface
  
  interface MSP_expand_message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_expand_message
!
!$Resume
!	Substitue the # of a message with the word of the variable part of the message
!
!$Description
!	This routine allows to position the varaible part of the message
!-	Two rules applies to this routine
!.	1) The varaible part of the message are identified by the # character
!.	2) The variable part is a character string table
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_expand_message(mes,expension)
!.    type(MSP_type_erreur) :: mes
!.    character(len=*) ,dimension(:) :: expension
!
!$Procedures
!- expand_message_tab
!
!$Remarques
!
!$Mots-cles
! ERREUR SUBSTITUER MESSAGE TABLEAU
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure expand_message_tab
  end interface
  
  interface MSP_cancel_problem

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_cancel_problem
!
!$Resume
!  This routine cancel the last problem, keeping the message history
!
!$Description
!  This routine cancel the last problem, keeping the message history
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_cancel_problem()
!
!$Procedures
!- MSP_annuler_probleme
!
!$Remarques
!
!$Mots-cles
! ERREUR ANNULER PROBLEME
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_annuler_probleme
  end interface
  
  interface MSP_set_user_mode

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_user_mode
!
!$Resume
!  This routine switch on the user mode 
!
!$Description
!  This routine switch on the user mode 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_user_mode()
!
!$Procedures
!- MSP_mode_utilisateur
!
!$Remarques
!
!$Mots-cles
! ERREUR MODE UTILISATEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_mode_utilisateur
  end interface
  
  interface MSP_set_developer_mode

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_developer_mode
!
!$Resume
!  This routine switch on the developper mode 
!
!$Description
!  This routine switch on the developper mode 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_developer_mode()
!
!$Procedures
!- MSP_mode_developpeur
!
!$Remarques
!
!$Mots-cles
! ERREUR MODE DEVELOPPEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_mode_developpeur
  end interface
  
  interface MSP_stop_at_problem_occurence

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_stop_at_problem_occurence
!
!$Resume
!  This routine switch on a mode where the execution is stopped at the first warning/error occurence
!
!$Description
!  This routine switch on a mode where the execution is stopped at the first warning/error occurence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_stop_at_problem_occurence()
!
!$Procedures
!- MSP_stopper_en_cas_de_pb
!
!$Remarques
!
!$Mots-cles
! ERREUR STOPPER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_stopper_en_cas_de_pb
  end interface
  
  interface MSP_add_message_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_message_file
!
!$Resume
!	This routines add a file message to the list of available file message
!
!$Description
!	This routine increase the list of file that contains error message description
!.	This list contains by default the MSP_fic_defaut file.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_message_file(fichier)
!.    character(len=*) :: fichier
!
!$Procedures
!- MSP_ajouter_fichier_message
!
!$Remarques
!
!$Mots-cles
! ERREUR MESSAGE AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_ajouter_fichier_message
  end interface



CONTAINS


  subroutine augmente_taille_message(tmessage,nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  augmente_taille_message
!
!$Resume
!	routine permettant d'augmenter la taille d'un tableau en conservant les données
!
!$Description
!	routine permettant d'augmenter la taille d'un tableau en conservant les données
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PRIVE
!
!$Usage
!  call augmente_taille_message(tmessage,nb)
!.    type (MSP_type_erreur),dimension(:),pointer :: tmessage
!.    integer :: nb
!
!$Arguments
!>E/S   tmessage  :<MSP_type_erreur,DIM=(:),pointer>   tableau de message
!>E     nb        :<integer>                           nombre d'élémént à ajouter 
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

    type (MSP_type_erreur),dimension(:),pointer :: tmessage
    integer,intent(in)::nb

    ! tableau local des messages
    type (MSP_type_erreur),dimension(:),pointer :: tab_message_loc => NULL()
    integer::ntaille_cour, stat

    ntaille_cour=size(tmessage)
    ! création d'un pointeur locale sur le tableau actuel
    !allocate(tab_message_loc(ntaille_cour))
    !tab_message_loc(:) = tmessage(:)
    tab_message_loc => tmessage

    ! annulation du lien entre le pointeur tmessage et les donnnées
    !deallocate(tmessage)
    nullify(tmessage)

    ! augmentation de la taille des messages
    allocate(tmessage(ntaille_cour+nb))

    
    ! Affectation des premiers messages
    tmessage(:ntaille_cour)=tab_message_loc(:)
    ! initialisation des nouveaux messages
    tmessage(ntaille_cour+1:)=message_nul

    ! Destruction du tableau temporaire
    deallocate(tab_message_loc, stat=stat)

  end subroutine augmente_taille_message

  subroutine affecte_chaine_tab(chaine,pos1,pos2,tab)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  affecte_chaine_tab
!
!$Resume
!	Routine permettant d'affecter une chaine de caractères dans un tableau de caractères
!$Description
!	tab(pos1:pos2)=chaine(1:pos2-pos1+1)
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call affecte_chaine_tab(chaine,pos1,pos2,tab)
!.    character(len=*) :: chaine
!.    character(len=1),dimension(:),pointer :: tab
!.    integer :: pos1,pos2
!
!$Arguments
!>E     chaine  :<LEN=*>                   chaine à affecter
!>E     pos1    :<integer>                 position 1 du tableau
!>E     pos2    :<integer>                 position 2 du tableau
!>E/S   tab     :<LEN=1,DIM=(:),pointer>   tableau de caractère
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
    character(len=*),intent(in)::chaine
    character(len=1),dimension(:),pointer::tab
    integer,intent(in)::pos1,pos2

    integer::i

    do i=1,pos2-pos1+1
       tab(pos1+i-1)=chaine(i:i)
    end do
  end subroutine affecte_chaine_tab

  subroutine affecte_tab_chaine(tab,pos1,pos2,chaine)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  affecte_tab_chaine
!
!$Resume
!	Routine permettant d'affecter un tableau de caractères dans une chaine de caractères
!$Description
!	chaine(1:pos2-pos1+1)=tab(pos1:pos2)
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call affecte_tab_chaine(tab,pos1,pos2,chaine)
!.    character(len=1),dimension(:),pointer :: tab
!.    character(len=*) :: chaine
!.    integer :: pos1,pos2
!
!$Arguments
!>E/S   tab     :<LEN=1,DIM=(:),pointer>   tableau de caractère à affecter
!>E     pos1    :<integer>                 position 1 du tableau
!>E     pos2    :<integer>                 position 2 du tableau
!>S     chaine  :<LEN=*>                   chaine résultat
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
    character(len=1),dimension(:),pointer::tab
    character(len=*),intent(out)::chaine
    integer,intent(in)::pos1,pos2

    integer::i

    do i=1,pos2-pos1+1
       chaine(i:i)=tab(pos1+i-1)
    end do
  end subroutine affecte_tab_chaine

  subroutine insere_chaine(chaine1,chaine2,pos,chaine) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  insere_chaine
!
!$Resume
!	routine permettant d'insérer une chaîne à la place d'un carctère dans une chaîne
!
!$Description
!	routine permettant d'insérer une chaîne à la place d'un carctère dans une chaîne
!
!$Auteur
!	S. Rousseau
!
!$Acces
!  PRIVE
!
!$Usage
!  call insere_chaine(chaine1,chaine2,pos,chaine) 
!.    character(len=*) :: chaine1,chaine2
!.    integer :: pos
!.    character(len=*) :: chaine
!
!$Arguments
!>E     chaine1  :<LEN=*>     chaîne en entrée 
!>E     chaine2  :<LEN=*>     chaîne à insérée
!>E     pos      :<integer>   position du caractère à substituer
!>S     chaine   :<LEN=*>     chaîne résultat
!
!$Common
!
!$Routines
!#V
!- affecte_chaine_tab
!- affecte_tab_chaine
!#
!
!$Include
!
!$Module
!
!$Remarques
!	Cette routine ne test pas les cas o· les longueurs de chaîne l1+l2 sont plus longues que la chaîne finale
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    character(len=*),intent(in):: chaine1,chaine2
    integer,intent(in)::pos
    character(len=*),intent(out)::chaine

    ! Variables locales
    integer::l1,l2,l, stat
    character(len=1),dimension(:),pointer::tab_loc=>null()

    ! Lecture des longueur des différentes chaines
    ! longueur utile de la chaine 1
    l1=len_trim(chaine1)
    ! longueur utile de la chaine 2
    l2=len_trim(chaine2)
    l=len(chaine)
    allocate(tab_loc(l))
    tab_loc(:)=""
    ! insertion de la première partie
    ! cas o· # est en premier on insère une chaine vide
    if ( pos-1<=l) then
       call affecte_chaine_tab(chaine1,1,pos-1,tab_loc)
    else
       ! le calcul est fini car on atteint la fin de la chaine
       ! a priori ce cas n'est pas atteignable (TBC)
       call affecte_chaine_tab(chaine1,1,l,tab_loc)
       call affecte_tab_chaine(tab_loc,1,l,chaine)
       deallocate(tab_loc, stat=stat)
       return
    end if

    ! insertion de la deuxième partie
    ! on teste si la longueur pos+l2<l sinon on coupe
    if ((pos+l2-1)<=l) then
       call affecte_chaine_tab(chaine2,pos,pos-1+l2,tab_loc)
    else
       ! on a forcémént pos<l
       call affecte_chaine_tab(chaine2,pos,l,tab_loc)
       call affecte_tab_chaine(tab_loc,1,l,chaine)
       deallocate(tab_loc, stat=stat)
       return
    endif

    ! insertion de la troisiéme partie
    ! on teste si la longueur l1+l2-1<=l sinon on coupe
    if ((l1+l2-1)<=l) then
       call affecte_chaine_tab(chaine1(pos+1:),pos+l2,l1+l2-1,tab_loc)
    else
       ! on a forcémént pos+l2-1<l
       call affecte_chaine_tab(chaine1(pos+1:),pos+l2,l,tab_loc)
    endif
    call affecte_tab_chaine(tab_loc,1,l,chaine)
    deallocate(tab_loc, stat=stat)
  end subroutine insere_chaine

  subroutine egaler_MSP_message(message1,message2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_MSP_message
!
!$Resume
!	Routine permettant d'égaler deux types messages
!
!$Description
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_MSP_message(message1,message2)
!.    type(MSP_message) :: message1
!.    type(MSP_message) :: message2
!
!$Arguments
!>S     message1  :<MSP_message>   Structure message 1
!>E     message2  :<MSP_message>   Structure message 2
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
!  ERREUR MESSAGE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_message),intent(inout) :: message1
    type(MSP_message),intent(in) :: message2

    ! Variables locales
    integer::taillemes2, i, stat

    if (associated(message1%routine)) then
       deallocate(message1%routine, stat=stat)
    end if

    if ( associated(message1%erreur)) then
       deallocate(message1%erreur, stat=stat)
    end if

    if (associated(message1%message)) then
       deallocate(message1%message, stat=stat)
    end if

    if (associated(message1%warning)) then
       deallocate(message1%warning, stat=stat)
    end if


    taillemes2 = size(message2%message)

    ! Allocation du message 1
    allocate(message1%routine(taillemes2))
    allocate(message1%message(taillemes2))
    allocate(message1%erreur(taillemes2))
    allocate(message1%warning(taillemes2))


    ! Affectation de chaque tableaux
    do i = 1 , taillemes2
       message1%routine(i) = message2%routine(i)
       message1%message(i) = message2%message(i)
       message1%erreur(i)  = message2%erreur(i)
       message1%warning(i) = message2%warning(i)
    enddo

  end subroutine egaler_MSP_message


  integer function charge_conf(fichier) result (ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  charge_conf
!
!$Resume
!	Fonction permettant de charger les différents fichiers descripteurs
!
!$Description
!	Cette fonction charge les différents fichiers descripteurs
!-	Ces fichiers sont au format MADONA.
!-	La recherche de ces fichiers s'effectue en parcourant le PATH "MADONA_RC_PATH",
!        sauf si le fichier commence par le caractère /
!
!$Auteur
!	S. ROUSSEAU CS SI
!
!$Acces
!  PRIVE
!
!$Usage
!  ier = charge_conf(fichier)
!.    character(len=*) :: fichier
!
!$Arguments
!>E     fichier  :<LEN=*>     
!>S     ier      :<integer>   code erreur
!>	-1   : pas de fichier descripteur
!
!$Common
!
!$Routines
!- GETENV
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR CHARGER DESCRIPTEUR MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments d'entrée
    character(len=*) ,intent(in)::fichier

    ! Variables locales
    character(len=500) :: liste_chemin,chem_fic
    integer ::lgliste,lfinfic,ldebfic
    logical :: fexist

    ! Initialisation du code de sortie
    ier = -1

    ! Récupération du path MADONA_RC_PATH
    call GETENV("MADONA_RC_PATH",liste_chemin)

    ! Calcul de la longueur de la liste des chemins
    lgliste = LEN_TRIM(STRING=liste_chemin)

    ! Par défaut le PATH contient point
    if ( lgliste == 0) then
       liste_chemin = "."
       lgliste = 1
    end if

    
    ! On teste si le fichier existe : le fichier peut etre specifie par 
    ! un chemin absolu ( /rep/rep2/fic) ou un chemin relatif (ex : ../rep/fic)
    
    inquire(file=fichier,exist=fexist)
    ! Le fichier est spécifié par son chemin absolu
    if ( fexist) then
       acc_cour = acc_cour + 1
       tab_acc(acc_cour) = acc_open()
       ier = acc_connect(tab_acc(acc_cour),fichier,'r')
       ier = acc_read(tab_acc(acc_cour),ACC_ALL)
       ier = acc_deconnect(tab_acc(acc_cour),"r")


       ! Le fichier est recherché à partir du MADONA_RC_PATH
    else
          
       lfinfic = 0
       ldebfic = 1
       fexist=.false.
       do while (( lfinfic /= lgliste).and.(.not.fexist))
          lfinfic = ldebfic+scan(string=liste_chemin(ldebfic:lgliste),set=":") - 2 
          if (lfinfic == (ldebfic-2) ) then
             lfinfic = lgliste
          end if
             
          if (liste_chemin(lfinfic:lfinfic) == "/") then
             chem_fic = liste_chemin(ldebfic:lfinfic)//fichier
          else
             chem_fic = liste_chemin(ldebfic:lfinfic)//"/"//fichier
          end if
          inquire(file=chem_fic,exist=fexist)
          if ( fexist) then
             acc_cour = acc_cour + 1
             tab_acc(acc_cour) = acc_open()
             ier = acc_connect(tab_acc(acc_cour),chem_fic,'r')
             ier = acc_read(tab_acc(acc_cour),ACC_ALL)
             ier = acc_deconnect(tab_acc(acc_cour),"r")
          end if
          ldebfic = lfinfic + 2
       end do
       
    end if


    ! Affectation du code ier
    if ( acc_cour == 0) then
       ier = -1
    end if

  end function charge_conf

  integer  function recup_message(cle_mes,mes) result (ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  recup_message
!
!$Resume
!	Fonction permettant de récupérer la structure Message à partir d'un fichier descripteur de type MADONA
!
!$Description
!	Fonction permettant de récuperer une structure Message dans un fichier descripteur
!-	La structure du message dans le fichier madona se présente de la façon suivante :
!-	Message_1 = {
!.		Routine      = "routine quie a générée le message"
!.		Message      = "Arrêt du calcul à l'itération #"
!.		Code  = "Divergence"
!.		Type  = "warning"
!.		Niveau       = "developpeur"
!.		}
!-	Aucun des éléments de la structure ne sont obligatoires.
!-	Code_erreur et Code_warning sont incompatibles. Si les deux sont présents la valeur Code_erreur sera la valeur lue.
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  ier = recup_message(cle_mes,mes)
!.    character(len=*) :: cle_mes
!.    type(MSP_type_erreur) :: mes
!
!$Arguments
!>E     cle_mes  :<LEN=*>             Mot clé correspond à la structure MADONA du message
!>S     mes      :<MSP_type_erreur>   Message correspondant à la structure MADONA
!>S     ier      :<integer>           Code erreur
!>	-1       :                   Il n'y a pas de structure MADONA correspondant à cle_mes
!
!$Common
!
!$Routines
!- MSP_ajouter_fichier_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR RECUPERER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    character(len=*),intent(in) :: cle_mes
    type(MSP_type_erreur),intent(out) :: mes


    integer :: iexist_mes,iexistRoutine,iexistMessage,iexistCode,iexistNiveau
    character(len=50) :: niv_mes,type_mes
    integer :: l_niv_mes, ii

    ! Chargement des différents fichiers d'erreur
    if (non_init) then
       call MSP_ajouter_fichier_message(trim(MAGE_fic_default))
       non_init = .false.
    end if

    ! Recherche de cle_mes dans les différents fichiers
    recherche : do ii = 1,acc_cour
       iexist_mes = acc_exist(tab_acc(ii),cle_mes)
       if (iexist_mes == 1) then
          ier = acc_select(tab_acc(ii),cle_mes,ACC_STRUCT)
          
          ! Chargement de la structure erreur
          iexistRoutine = acc_exist(tab_acc(ii),"Routine")
          if ( iexistRoutine == 1) then
             ier = acc_gets(tab_acc(ii),"Routine",mes%routine)
          end if

          iexistMessage = acc_exist(tab_acc(ii),"Message")
          if ( iexistMessage == 1) then
             ier = acc_gets(tab_acc(ii),"Message",mes%Message)
          end if

          iexistCode = acc_exist(tab_acc(ii),"Code")
          if ( iexistCode == 1) then
             ier = acc_gets(tab_acc(ii),"Code",mes%erreur)
          end if

          ! Affection du type de message ( Erreur ou Warning)
          iexistCode = acc_exist(tab_acc(ii),"Type")
          if ( iexistCode == 1) then
             type_mes = "erreur"
             ier = acc_gets(tab_acc(ii),"Type",type_mes)
             select case (trim(string=type_mes))
             case ("erreur")
                mes%warning = .false.
             case ("warning")
                mes%warning = .true.
             case ("propagation")
                mes%warning = MSP_warning
             end select
          end if
   

          ! Affectationm du niveau du message (developpeur utilisateur)
          iexistNiveau = acc_exist(tab_acc(ii),"Niveau")
          if ( iexistNiveau == 1) then
             ier = acc_gets(tab_acc(ii),"Niveau",niv_mes)
             l_niv_mes = len_trim(string=niv_mes)
             select case (niv_mes(1:l_niv_mes))

             case ("developpeur")
                mes%developpeur = .true.
             case default
                mes%developpeur = .false.
             end select
          end if

          ier = acc_select_end(tab_acc(ii))
          ier = 0
          return
       end if
    end do recherche      

    ier = -1
    
  end function recup_message


  recursive subroutine expand_message(mes,expension)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  expand_message
!
!$Resume
!	Routine permettant de substituer les # du message par les différents mots de la partie variable
!
!$Description
!	Cette routine permet de positionner les parties variables dans le message
!-	Cette routine suit 2 règles
!.	1) les parties variables du message sont signalées par un #
!.	2) la partie variable est un chaîne de caractère contenant tous les mots à substituer, elle se présente sous la forme suivante :
!	'"mot1""mot2".."motn"' ou s'il n'y a qu'un mot 'mot'
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call expand_message(mes,expension)
!.    type(MSP_type_erreur) :: mes
!.    character(len=*) :: expension
!
!$Arguments
!>E/S   mes        :<MSP_type_erreur>   Structure message
!>E     expension  :<LEN=*>             Partie variable du message
!
!$Common
!
!$Routines
!#V
!- insere_chaine
!- expand_message
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR SUBSTITUER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    type(MSP_type_erreur),intent(inout) :: mes
    character(len=*) ,intent(in):: expension

    integer :: pos_diese_mes
    integer :: pos_diese_var,lfin_var,deb_var
    character(len=250) :: partie_variable
    character(len=PM_len_message) :: message

    partie_variable=""
    ! Test pour savoir si la partie variable est en plusieurs morceaux
    if ( expension(1:1) == '"') then
       deb_var = 2
    else
       deb_var = 1
    end if
    ! Recherche d'un diese dans le message
    pos_diese_mes = scan(string=mes%message,set="#")
    if (pos_diese_mes /= 0) then
       ! il y a un diese
       ! découpage de la partie variable
       ! la partie variable se présente sous la forme
       ! '"mot1""mot2""mot3".."motn"' ou 'mot'
       ! Recherche d'un " dans la partie variable
       pos_diese_var = scan(string=expension(deb_var:len_trim(expension)),set='"')
       if (pos_diese_var > 0.and.pos_diese_var<=len_trim(expension)) then
          lfin_var = pos_diese_var
       else
          lfin_var = len_trim(string=expension)
       end if

       ! concatenation du message et de sa partie variable
       call insere_chaine(mes%message,expension(deb_var:lfin_var),&
            pos_diese_mes,message)
            
       mes%message = trim(message)
            
       if ( pos_diese_var /= 0.and.pos_diese_var<=len_trim(expension)) then
          partie_variable = expension(pos_diese_var+2 :)
       else
          partie_variable = trim(string=expension)
       end if
       call expand_message(mes,partie_variable)
    end if
  end subroutine expand_message

  subroutine expand_message_tab(mes,expension)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  expand_message_tab
!
!$Resume
!	Routine permettant de substituer les # du message par les différents mots de la partie variable
!
!$Description
!	Cette routine permet de positionner les parties variables dans le message
!-	Cette routine suit 2 règles
!.	1) les parties variables du message sont signalées par un #
!.	2) la partie variable est un tableau de chaine de character 
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call expand_message_tab(mes,expension)
!.    type(MSP_type_erreur) :: mes
!.    character(len=*) ,dimension(:) :: expension
!
!$Arguments
!>E/S   mes        :<MSP_type_erreur>   Structure message
!>E/S   expension  :<LEN=*,DIM=(:)>     Partie variable du message
!
!$Common
!
!$Routines
!#V
!- insere_chaine
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR SUBSTITUER MESSAGE TABLEAU
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    type(MSP_type_erreur),intent(inout) :: mes
    character(len=*) ,dimension(:):: expension
    character(len=PM_len_message) :: message
    
    integer :: pos_diese_mes
    integer :: nb_part_var,i
    

    ! Test pour savoir si la partie variable est en plusieurs morceaux
    nb_part_var = size(expension,dim=1)

    ! affectation de l'indice du premier indice de la partie variable
    i = 1


    ! Boucle sur le message à expander
    boucle_mes: do
       

       ! Recherche d'un diese dans le message
       pos_diese_mes = scan(string=mes%message,set="#")

       if (pos_diese_mes /= 0) then
          ! il y a un diese
          
          ! concatenation du message et de sa partie variable
          call insere_chaine(mes%message,expension(i),pos_diese_mes,message)
          mes%message = trim(message)

          ! passage a la partie variable suivante sinon on repète la dernière
          if (i < nb_part_var) then
             i = i + 1
          end if
       else
          exit boucle_mes
       end if
    end do boucle_mes
  end subroutine expand_message_tab


  subroutine MSP_effacer_message(nb_mes)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_message
!
!$Resume
!	Routine permettant de réinitialiser les messages 
!
!$Description
!	Cette routine annule les PB (erreur ou warning)
!	et réinitialise l'historique des messages
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_message([nb_mes])
!.    integer :: nb_mes
!
!$Arguments
!>[E]   nb_mes  :<integer>   nombre de message a effacer (MSP_dernier_message,MSP_tous_messages, n les n derniers messages) 
!			     [defaut MSP_tous_messages]
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
!  ERREUR EFFACER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    !Declaration des arguments
    integer,intent(in),optional::nb_mes

    ! Variables locales
    integer :: nb_mes_loc, stat


    ! Lecture de l'argument
    if ( present(nb_mes)) then
       nb_mes_loc = nb_mes
    else
       nb_mes_loc = MSP_tous_messages
    end if


    ! Verification de la compatibilite de nb_mes_loc avec le nombre de message generes
    if ( nb_mes_loc > nb_message) then
       nb_mes_loc = nb_message
    end if

    if ( nb_mes_loc == MSP_tous_messages ) then
       ! Initialisation des messages à zéro
       nb_message = 0    
       if ( associated(tab_message)) then
          deallocate(tab_message, stat=stat)
       end if

       ! Re-initialisation du nombre de message maximum
       n_mes_max = nb_mes_plus

       allocate(tab_message(nb_mes_plus))
       tab_message(:) = message_nul
       MSP_erreur = .false.
       MSP_warning = .false.
       MSP_probleme = .false.
       code_courant = ""
       message_courant =""
       routine_courant =""
    else
       ! suppression des nb_mes_loc derniers messages
       nb_message = nb_message - nb_mes_loc

       ! mise a jour des messages effaces
       tab_message(nb_message+1 : nb_message + nb_mes_loc) = message_nul
       if ( nb_message > 1) then
          ! remise a jour des informations courantes
          MSP_warning      = tab_message(nb_message)%warning
          MSP_erreur       = .not.tab_message(nb_message)%warning
          MSP_probleme     = .true.
          code_courant     = tab_message(nb_message)%erreur
          message_courant  = tab_message(nb_message)%message
          routine_courant  = tab_message(nb_message)%routine
       else
          MSP_erreur = .false.
          MSP_warning = .false.
          MSP_probleme = .false.
          code_courant = ""
          message_courant =""
          routine_courant =""
       end if
    end if
        
         

  end subroutine MSP_effacer_message



  subroutine MSP_annuler_probleme()


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_annuler_probleme
!
!$Resume
!	Routine permettant d'annuler un problème tout en gardant l'historique des messages
!
!$Description
!	Routine permettant d'annuler un problème tout en gardant l'historique des messages
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_annuler_probleme()
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
!  ERREUR ANNULER PROBLEME
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    MSP_probleme = .false.
    MSP_erreur = .false.
    MSP_warning = .false.
  end subroutine MSP_annuler_probleme

  subroutine MSP_mode_utilisateur()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mode_utilisateur
!
!$Resume
!	Routine permettant de passer en mode utilisateur
!
!$Description
!	Routine permettant de passer en mode utilisateur, c'est-à-dire que seuls les messages
!	de niveau utilisateur sont transmis
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_mode_utilisateur()
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
!  ERREUR MODE UTILISATEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    mode_developpeur = .false.
  end subroutine MSP_mode_utilisateur

  subroutine MSP_mode_developpeur()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mode_developpeur
!
!$Resume
!	Routine permettant de passer en mode développeur
!
!$Description
!	Mode de fonctionnement utilisateur : Tous les messages sont transmis
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_mode_developpeur()
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
!  ERREUR MODE DEVELOPPEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    mode_developpeur = .true.
  end subroutine MSP_mode_developpeur

  subroutine MSP_stopper_en_cas_de_pb()


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_stopper_en_cas_de_pb
!
!$Resume
!	Routine permettant de stopper un programme après un signeler message
!
!$Description
!	Routine permettant de stopper un programme après un signeler message
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_stopper_en_cas_de_pb()
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
!  ERREUR STOPPER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    stop_erreur = .true.
  end subroutine MSP_stopper_en_cas_de_pb
  subroutine MSP_ajouter_fichier_message(fichier)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_fichier_message
!
!$Resume
!	Routine permettant de définir un fichier descripteur des messages
!
!$Description
!	Cette routine permet d'enrichier la liste des fichiers contenants les message d'erreur
!.	Cette liste contient par défaut le fichier MAGE_fic_default
!
!$Auteur
!	S. ROUSSEAU CS SI
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_fichier_message(fichier)
!.    character(len=*) :: fichier
!
!$Arguments
!>E     fichier  :<LEN=*>   Nom du fichier
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
!	Eles fichiers peuvent être défini en chemin absolu (commençant par /) ou relatif.
!       En cas de chemin relatif, la recherche s'effetue à partir du MADONA_DATA_PATH
!
!$Mots-cles
!  ERREUR MESSAGE AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments d'appel
    character(len=*),intent(in) :: fichier

    ! Variable locale
    integer::ier

    nfic_cour = nfic_cour + 1
    ier = charge_conf(fichier)
    if (ier < 0) then
       if (trim(fichier)/=trim(MAGE_fic_default)) then
          call MSP_signaler_message (cle_mes="MSP_CONNECTION_FICHIER", &
               routine="MSP_ajouter_fichier_message", partie_variable=trim(fichier), &
               type=MSP_enum_warning)
       else
          call signaler_message(message="Warning : fichier de messages MAGE par défaut absent",&
               routine="MSP_ajouter_fichier_message",&
               type=MSP_enum_warning,code="MINEUR")
       endif
    else
       non_init = .false.
    endif
    
  end subroutine MSP_ajouter_fichier_message



  subroutine signaler_message(cle_mes,message,partie_variable,&
       init,routine,&
       code,type,developpeur,ier_mslib)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  signaler_message
!
!$Resume
!	Routine permettant de signaler un erreur ou un warning
!
!$Description
!	Cette routine permet de générer une erreur ou un warning.
!-	Il y a diverse niveau de fonctionnement :
!.	 1 : A partir d'un mot clé
!.		Le libellé du message ainsi que le libellé de l'erreur associé sont décrit dans un fichier descripteur
!.	 2 : En renseignant directement le message , l'erreur,...
!.	 3 : A partir d'une erreur de type MSLIB
!-	De plus il y a 2 niveaux de message :
!.	 1 : niveau "utilisateur" (mode fonctionnement par défaut)(  
!	il est possible de repsser dans ce mode avec la routine MSP_mode_utilisateur)
!.	 2 : niveau "developpeur" (pour activer ce mode de fonctionnement il faut faire appel à la routine MSP_mode_developpeur)
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call signaler_message([cle_mes],[message],[partie_variable],&
!.           [init],[routine],&
!.           [code],[type],[developpeur],[ier_mslib])
!.    character(len=*) :: cle_mes
!.    character(len=*) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    type(tm_code_retour) :: ier_mslib
!.    logical :: developpeur
!.    logical :: init
!
!$Arguments
!>[E]   cle_mes          :<LEN=*>            Mot clé correspondant à une structure Message MADONA
!>[E]   message          :<LEN=*>            Libellé du message
!>[E]   partie_variable  :<LEN=*>            Partie variable du message 
!>[E]   init             :<logical>          booléen permettant de réinitialiser l'historique des messages
!>[E]   routine          :<LEN=*>            Nom de la routine 
!>[E]   code             :<LEN=*>            Libellé du code du message 
!>[E]   type             :<integer>          Type de message (MSP_enum_warning ou MSP_enum_erreur) [défaut MSP_enum_erreur] 
!>[E]   developpeur      :<logical>          Niveau du message (MSP_enum_developpeur ou MSP_enum_utilisateur) [ défaut MSP_enum_utilisateur]  
!>[E]   ier_mslib        :<tm_code_retour>   Erreur de type MSLIB 90 ou MSPRO
!
!$Common
!
!$Routines
!- MSP_effacer_message
!#V
!- augmente_taille_message
!- expand_message
!- recuperer_message_type
!- afficher_message_type
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR SIGNALER MESSAGE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments d'appels
    character(len=*) ,intent(in),optional::cle_mes
    character(len=*) ,intent(in),optional:: partie_variable
    character(len=*) ,intent(in),optional:: routine
    character(len=*) ,intent(in),optional:: message
    character(len=*) ,intent(in),optional:: code
    integer,intent(in),optional :: type
    type(tm_code_retour),intent(in),optional :: ier_mslib
    logical,optional,intent(in)::developpeur
    logical ,intent(in),optional:: init
 
    ! Variables locales
    integer :: ier
    character(len=PM_nom_biblio ):: nom_biblio
    character(len=PM_nom_routine):: nom_routine
    character(len=PM_identification_routine) :: id_routine
    character(len=PM_signification_code_retour) :: lib_erreur
    type(tm_code_retour) ::ier_loc


    ! Historique local de messages
    type(MSP_message)::historique

    ! Test sur la validité du tableau des messages
    ! si nb_message = 0 if faut initialiser tab_message
    ! si nb_message > n_mes_max il faut augmenter tab_message
    if ( .not.(associated(tab_message)) ) then
       allocate(tab_message(nb_mes_plus))
       ! Initialisation du tableau
       tab_message(:) = message_nul
    end if

    ! Initialisation des messages

    if ( nb_message >= n_mes_max) then
       call augmente_taille_message(tab_message,nb_mes_plus)
       ! Mise à jour de n_mes_max
       n_mes_max = n_mes_max + nb_mes_plus
    end if

    ! initialisation de l'historique des erreurs
    if ( present(init)) then
       call MSP_effacer_message()
    end if

    ! Génération d'un message le cas échéant
    if ( present(cle_mes).or.present(routine).or.present(message).or.present(code) ) then

       nb_message = nb_message + 1

       ! Affectation des valeurs par défaut
       tab_message(nb_message) = message_default

       ! Affectation du nom de la routine si l'argument est présent
       if ( present(routine)) then
          tab_message(nb_message)%routine = trim(string=routine)
       end if

       ! Affectation du message s'il est présent
       if (present(message)) then
          tab_message(nb_message)%message = trim(string=message)
       end if

       ! Affectation du code  s'il est présent
       if ( present(code)) then
          tab_message(nb_message)%erreur = trim(string=code)
       end if

       ! Affection du type
       if (present(type)) then
          select case (type)
          case ( MSP_enum_erreur)
             tab_message(nb_message)%warning = .false.
          case (MSP_enum_warning)
             tab_message(nb_message)%warning = .true.
          case (MSP_enum_propagation)
             tab_message(nb_message)%warning = MSP_warning
          end select
       end if

    else if ( present(ier_mslib) ) then

       ! Cas d'un message MSLIB
       ! ces messages sont considérés comme de niveau développeur

       if (ier_mslib%valeur /= 0) then

          ! cas d'une erreur ou d'un warning
          ! Génération du message

          nb_message = nb_message + 1

          nom_routine=" "
          id_routine= " "
          call mzpro_code_retour(ier_mslib,nom_biblio,nom_routine,id_routine,&
               lib_erreur,ier_loc)
          tab_message(nb_message)%routine = trim(string=nom_routine)//" : "//trim(string=id_routine)
          tab_message(nb_message)%message = trim(string=lib_erreur)
          write(tab_message(nb_message)%erreur,'(i6)') ier_mslib%valeur

          if ( ier_mslib%valeur > 0) then
            ! il s'agit d'un warning
             tab_message(nb_message)%warning = .true.
          end if

          tab_message(nb_message)%developpeur = .true.

       else

          ! Sortie sans générer de messages:
          return

       end if


    end if


    if ( present(developpeur)) then
       tab_message(nb_message)%developpeur = developpeur
    end if


    ! décodage du mot clé
    if ( present(cle_mes) ) then
       ier = recup_message(cle_mes,tab_message(nb_message))
       if (ier < 0) then
          write(0,'(a)') "Code d'erreur inconnu : "//trim(cle_mes)
          write(0,'(a)') "Unknown error code : "//trim(cle_mes)
       endif
    end if

    ! Ajout de la partie variable
    if ( present(partie_variable) ) then
       call expand_message(tab_message(nb_message),partie_variable)
    end if


    ! Mise à jour des variables globales
    if (tab_message(nb_message)%warning) then
       MSP_erreur = .false.
       MSP_warning = .true.
       MSP_probleme = .true.
    else
       MSP_erreur = .true.
       MSP_warning = .false.
       MSP_probleme = .true.
   end if

    ! Affectation du message courant
    code_courant = trim(string=tab_message(nb_message)%erreur)
    message_courant = trim(string=tab_message(nb_message)%message)
    routine_courant = trim(string=tab_message(nb_message)%routine)

    ! Traitement en cas d'arret du programme
    if ( stop_erreur) then

       ! Récuperation de tous les messages
       call recuperer_message_type(historique,nb_mes = MSP_tous_messages)
       call afficher_message_type(historique,unit=0)

       ! Arret
       STOP
    end if

  end subroutine Signaler_message


  subroutine signaler_message_tab(cle_mes,message,partie_variable,&
       init,routine, code,type,developpeur)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  signaler_message_tab
!
!$Resume
!	Routine permettant de signaler un erreur ou un warning
!
!$Description
!	Cette routine permet de générer une erreur ou un warning.
!-	Il y a diverse niveau de fonctionnement :
!.	 1 : A partir d'un mot clé
!.		Le libellé du message ainsi que le libellé de l'erreur associé sont décrit dans un fichier descripteur
!.	 2 : En renseignant directement le message , l'erreur,...
!.	 3 : A partir d'une erreur de type MSLIB
!-	De plus il y a 2 niveaux de message :
!.	 1 : niveau "utilisateur" (mode fonctionnement par défaut)(  
!	il est possible de repsser dans ce mode avec la routine MSP_mode_utilisateur)
!.	 2 : niveau "developpeur" (pour activer ce mode de fonctionnement il faut faire appel à la routine MSP_mode_developpeur)
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call signaler_message_tab([cle_mes],[message],partie_variable,&
!.           [init],[routine],&
!.           [code],[type],[developpeur])
!.    character(len=*) :: cle_mes
!.    character(len=*) ,dimension(:) :: partie_variable
!.    character(len=*) :: routine
!.    character(len=*) :: message
!.    character(len=*) :: code
!.    integer :: type
!.    logical :: developpeur
!.    logical :: init
!
!$Arguments
!>[E]   cle_mes          :<LEN=*>           Mot clé correspondant à une structure Message MADONA
!>[E]   message          :<LEN=*>           Libellé du message
!>E/S   partie_variable  :<LEN=*,DIM=(:)>   Partie variable du message sous forme de tableaux de chaine de character
!>[E]   init             :<logical>         booléen permettant de réinitialiser l'historique des messages
!>[E]   routine          :<LEN=*>           Nom de la routine 
!>[E]   code             :<LEN=*>           Libellé du code du message 
!>[E]   type             :<integer>         Type de message (MSP_enum_warning ou MSP_enum_erreur) [défaut MSP_enum_erreur] 
!>[E]   developpeur      :<logical>         Niveau du message (MSP_enum_developpeur ou MSP_enum_utilisateur) [ défaut MSP_enum_utilisateur]  
!
!$Common
!
!$Routines
!- MSP_effacer_message
!- expand_message_tab
!#V
!- augmente_taille_message
!- recuperer_message_type
!- afficher_message_type
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR SIGNALER MESSAGE TABLEAU
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments d'appels
    character(len=*) ,intent(in),optional::cle_mes
    character(len=*) ,dimension(:):: partie_variable
    character(len=*) ,intent(in),optional:: routine
    character(len=*) ,intent(in),optional:: message
    character(len=*) ,intent(in),optional:: code
    integer,intent(in),optional :: type
    logical,optional,intent(in)::developpeur
    logical ,intent(in),optional:: init
 
    ! Variables locales
    integer :: ier

    ! Historique local de messages
    type(MSP_message)::historique

    ! Test sur la validité du tableau des messages
    ! si nb_message = 0 if faut initialiser tab_message
    ! si nb_message > n_mes_max il faut augmenter tab_message
    if ( .not.(associated(tab_message))) then
       allocate(tab_message(nb_mes_plus))
       ! Initialisation du tableau
       tab_message(:) = message_nul
    end if

    if ( nb_message >= n_mes_max) then
       call augmente_taille_message(tab_message,nb_mes_plus)
       ! Mise à jour de n_mes_max
       n_mes_max = n_mes_max + nb_mes_plus
    end if

    ! initialisation de l'historique des erreurs
    if ( present(init)) then
       call MSP_effacer_message()
    end if

    ! Génération d'un message le cas échéant
    if ( present(cle_mes).or.present(routine).or.present(message).or.present(code) ) then

       nb_message = nb_message + 1

       ! Affectation des valeurs par défaut
       tab_message(nb_message) = message_default

       ! Affectation du nom de la routine si l'argument est présent
       if ( present(routine)) then
          tab_message(nb_message)%routine = trim(string=routine)
       end if

       ! Affectation du message s'il est présent
       if (present(message)) then
          tab_message(nb_message)%message = trim(string=message)
       end if

       ! Affectation du code  s'il est présent
       if ( present(code)) then
          tab_message(nb_message)%erreur = trim(string=code)
       end if

       ! Affection du type
       if (present(type)) then
          select case (type)
          case ( MSP_enum_erreur)
             tab_message(nb_message)%warning = .false.
          case (MSP_enum_warning)
             tab_message(nb_message)%warning = .true.
          case (MSP_enum_propagation)
             tab_message(nb_message)%warning = MSP_warning
          end select
       end if


    end if

    if ( present(developpeur)) then
       tab_message(nb_message)%developpeur = developpeur
    end if

    ! décodage du mot clé
    if ( present(cle_mes)) then
       ier = recup_message(cle_mes,tab_message(nb_message))
       if (ier < 0) then
          write(0,'(a)') "Code d'erreur inconnu : "//trim(cle_mes)
          write(0,'(a)') "Unknown error code : "//trim(cle_mes)
       endif
    end if

    ! Ajout de la partie variable
    call expand_message_tab(tab_message(nb_message),partie_variable)

    if (tab_message(nb_message)%warning) then
       MSP_erreur = .false.
       MSP_warning = .true.
       MSP_probleme = .true.
    else
       MSP_erreur = .true.
       MSP_warning = .false.
       MSP_probleme = .true.
   end if

    ! Affectation du message courant
    code_courant = trim(string=tab_message(nb_message)%erreur)
    message_courant = trim(string=tab_message(nb_message)%message)
    routine_courant = trim(string=tab_message(nb_message)%routine)

    ! Traitement en cas d'arret du programme
    if ( stop_erreur) then

       ! Récuperation de tous les messages
       call recuperer_message_type(historique,nb_mes = MSP_tous_messages)
       call afficher_message_type(historique,unit=0)

       ! Arret
       STOP
    end if

  end subroutine Signaler_message_tab


   subroutine recuperer_message_chaine(message,routine,code_pb,liste_message,&
        liste_routine,liste_code_pb,liste_type_pb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  recuperer_message_chaine
!
!$Resume
!	Routine permettant de récupérer un certain nombre de paramètres liés au message	
!
!$Description
!	Routine permettant de récupérer un certain nombre de paramètres liés au message	
!.	. Le dernier message
!.	. La dernière routine qui a générée un message
!.	. Le libellé de l'erreur ou du warning
!.	. La liste des messages (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. La liste des routines (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. La liste des libellés erreur (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!.	. les liste des flags warning ou erreur (niveau utilisateur ou développeur suivant le mode de fonctionnement)
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call recuperer_message_chaine([message],[routine],[code_pb],[liste_message],&
!.            [liste_routine],[liste_code_pb],[liste_type_pb])
!.    character(len=*) :: message
!.    character(len=* ) :: routine
!.    character(len=*) :: code_pb
!.    character(len=*),dimension(:),pointer :: liste_message
!.    character(len=* ),dimension(:),pointer :: liste_routine
!.    character(len=*),dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!$Arguments
!>[S]   message        :<LEN=*>                     Libellé du message 
!>[S]   routine        :<LEN=*>                     Nom de la routine 
!>[S]   code_pb        :<LEN=*>                     valeur du code erreur (ou warning) 
!>[E/S] liste_message  :<LEN=*,DIM=(:),pointer>     liste des messages 
!>[E/S] liste_routine  :<LEN=*,DIM=(:),pointer>     liste des routines 
!>[E/S] liste_code_pb  :<LEN=*,DIM=(:),pointer>     liste des valeurs des codes erreur (ou warning) 
!>[E/S] liste_type_pb  :<logical,DIM=(:),pointer>   liste des flags indiquant s'il s'agit d'un warning (.true.) ou d'une erreur (.false.) 
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
!  ERREUR RECUPERER MESSAGE CHAINE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

     character(len=*),intent(out),optional :: message
     character(len=* ),intent(out),optional :: routine
     character(len=*),intent(out),optional :: code_pb
     character(len=*),dimension(:),pointer,optional :: liste_message
     character(len=* ),dimension(:),pointer,optional :: liste_routine
     character(len=*),dimension(:),pointer,optional :: liste_code_pb
     logical,dimension(:),pointer,optional::liste_type_pb

     ! Variables locales
     integer::i,mes_utilisateur, stat
     character(len=PM_len_message),dimension(:),pointer :: liste_message_loc => NULL()
     character(len=PM_len_routine ),dimension(:),pointer :: liste_routine_loc => NULL()
     character(len=PM_len_code_pb),dimension(:),pointer :: liste_code_pb_loc => NULL()
     logical,dimension(:),pointer::liste_type_pb_loc => NULL()
    

     ! Récupération du dernier message
     if ( present(message) ) then
        message = trim(string=message_courant)
     end if

     ! Récupération du nom de la routine qui a généré le dernier message 
     if ( present(routine)) then
        routine = trim(string=routine_courant)
     end if

     ! Récupération du dernier code erreur
     if ( present(code_pb)) then
        code_pb = trim(code_courant)
     end if


     ! Récupération de la liste des messages générés
     if (present(liste_message)) then
      
        ! Test si le tableau est déjà alloué
        if ( associated(liste_message)) then
           deallocate(liste_message, stat=stat)
        end if
        
        ! Dans le cas d'un mode de fonctionnement developpeur on récupère tous les messages
        if ( mode_developpeur) then
  
           ! cas o· il n'y a pas de message
           if (nb_message == 0) then
              ! on retourne les chaînes courantes
              ! Allocation du tableau message
              allocate(liste_message(1))
              liste_message(1) = ""
           else

              ! Allocation du tableau message
              allocate(liste_message(nb_message))


              ! Récupération de la liste
              do i = 1,nb_message
                 liste_message(i) = trim(string=tab_message(nb_message-i+1)%message)
              end do

           end if
        else
           ! Dans le cas d'un fonctionnement utilisateur on ne récupère que les message de niveau utilisateur
           if ( associated(liste_message_loc) ) then
              deallocate(liste_message_loc, stat=stat)
           end if

           ! Allocation du tableau de message local
           if (nb_message /= 0) then
              allocate(liste_message_loc(nb_message)) 
           end if
           ! Récupération des message uniquement utilisateur
           mes_utilisateur = 0
           recup_utilisateur : do i = nb_message,1,-1
              if ( .not.tab_message(i)%developpeur) then
                 mes_utilisateur = mes_utilisateur + 1
                 liste_message_loc(mes_utilisateur) = trim(string=tab_message(i)%message)
              end if
           end do recup_utilisateur

           ! Test si il existe des messages utilisateurs
           if ( mes_utilisateur == 0) then
              ! on renvoie une chaîne vide
              ! Allocation du tableau message
              allocate(liste_message(1))
              liste_message(1) = ""
           else

              ! Allocation du tableau message
              allocate(liste_message(mes_utilisateur))

              ! Récupération de la liste
              do i = 1,mes_utilisateur
                 liste_message(i) = trim(string=liste_message_loc(i))
              end do
           end if
        end if
     end if


     ! Récupération de la liste des routines qui ont générées les messages
     if ( present(liste_routine)) then
        ! Test si le tableau est déjà alloué
        if ( associated(liste_routine)) then
           deallocate(liste_routine, stat=stat)
        end if


        ! cas du mode developpeur
        if ( mode_developpeur) then

           ! Test s'il existe des messages
            if (nb_message == 0) then
              ! on retourne les chaînes courantes
              ! Allocation du tableau message
              allocate(liste_routine(1))
              liste_routine(1) = ""
           else
          
              ! Allocation du tableau message
              allocate(liste_routine(nb_message))

              ! Récupération de la liste
              do i =1,nb_message
                 liste_routine(i) = trim(string=tab_message(nb_message+1-i)%routine)
              end do
           end if
        else

           ! cas du mode utilisateur
            if ( associated(liste_routine_loc) ) then
              deallocate(liste_routine_loc, stat=stat)
           end if

           ! Allocation du tableau de message local
           if (nb_message /= 0) then
              allocate(liste_routine_loc(nb_message)) 
           end if
           ! Récupération des message uniquement utilisateur
           mes_utilisateur = 0
           recup_routine_utilisateur : do i = nb_message,1,-1
              if ( .not.tab_message(i)%developpeur) then
                 mes_utilisateur = mes_utilisateur + 1
                 liste_routine_loc(mes_utilisateur) = trim(string=tab_message(i)%routine)
              end if
           end do recup_routine_utilisateur


           ! test s'il existe des messages utilisateurs
           if (mes_utilisateur == 0 ) then
              allocate(liste_routine(1))
              liste_routine(1) = ""
           else

              ! Allocation du tableau message
              allocate(liste_routine(mes_utilisateur))

              ! Récupération de la liste
              do i =1,mes_utilisateur
                 liste_routine(i) = trim(string=liste_routine_loc(i))          
              end do
           end if
        end if
     end if      


     ! Récupération de la liste des codes erreurs
     if ( present(liste_code_pb)) then

        ! Test si le tableau est déjà alloué
        if ( associated(liste_code_pb)) then
           deallocate(liste_code_pb, stat=stat)
        end if
        

        if ( mode_developpeur) then
           ! mode développeur

           ! test s'il y a des messages
           if (nb_message == 0) then
              allocate(liste_code_pb(1))
              liste_code_pb(1) =""
           else
              ! Allocation du tableau message
              allocate(liste_code_pb(nb_message))
              ! Récupération de la liste
              do i =1,nb_message
                 liste_code_pb(i) = trim(string=tab_message(nb_message+1-i)%erreur)
              end do
           end if
        else
           ! cas du mode utilisateur
            if ( associated(liste_code_pb_loc) ) then
              deallocate(liste_code_pb_loc, stat=stat)
           end if

           ! Allocation du tableau de message local
           if (nb_message /= 0) then
              allocate(liste_code_pb_loc(nb_message)) 
           end if
           ! Récupération des message uniquement utilisateur
           mes_utilisateur = 0
           recup_code_pb_utilisateur : do i = nb_message,1,-1
              if ( .not.tab_message(i)%developpeur) then
                 mes_utilisateur = mes_utilisateur + 1
                 liste_code_pb_loc(mes_utilisateur) = trim(string=tab_message(i)%erreur)
              end if
           end do recup_code_pb_utilisateur


           ! Test sur la présence d'un message
           if ( mes_utilisateur == 0) then
              allocate(liste_code_pb(1))
              liste_code_pb(1) =""
           else

              ! Allocation du tableau message
              allocate(liste_code_pb(mes_utilisateur))
              
              ! Récupération de la liste
              do i =1,mes_utilisateur
                 liste_code_pb(i) = trim(string=liste_code_pb_loc(i))
              end do
           end if
        end if
     end if

       
     ! Récupération de la liste des flags erreur-warning
     if ( present(liste_type_pb)) then
        ! Test si le tableau est déjà alloué
        if ( associated(liste_type_pb)) then
           deallocate(liste_type_pb, stat=stat)
        end if

        if ( mode_developpeur) then
           ! mode developpeur
           
           ! test s'il existe un message
           if (nb_message == 0) then
              allocate(liste_type_pb(1))
              liste_type_pb(1) = .false.
           else
              ! Allocation du tableau message
              allocate(liste_type_pb(nb_message))
                
              ! Récupération de la liste
              liste_type_pb(1:nb_message) = tab_message(nb_message:1:-1)%warning
           end if
        else
           ! mode utilisateur
           if ( associated(liste_type_pb_loc) ) then
              deallocate(liste_type_pb_loc, stat=stat)
           end if
           if (nb_message /= 0) then
              ! Allocation du tableau de message local
              allocate(liste_type_pb_loc(nb_message)) 
           end if
           ! Récupération des message uniquement utilisateur
           mes_utilisateur = 0
           recup_warning_utilisateur : do i = nb_message,1,-1
              if ( .not.tab_message(i)%developpeur) then
                 mes_utilisateur = mes_utilisateur + 1
                 liste_type_pb_loc(mes_utilisateur) = tab_message(i)%warning
              end if
           end do recup_warning_utilisateur

           ! Test sur la présence de code utilisateur
           if (mes_utilisateur == 0) then
              allocate(liste_type_pb(1))
              liste_type_pb(1) = .false.
           else
              ! Allocation du tableau message
              allocate(liste_type_pb(mes_utilisateur))
              ! Récupération de la liste
              liste_type_pb(1:mes_utilisateur) = liste_type_pb_loc(1:mes_utilisateur)    
           end if
        end if
     end if


     ! Désallocation mémoire
     if (associated(liste_routine_loc )) deallocate (liste_routine_loc, stat=stat)
     if (associated(liste_code_pb_loc )) deallocate (liste_code_pb_loc, stat=stat)
     if (associated(liste_message_loc )) deallocate (liste_message_loc, stat=stat)
     if (associated(liste_type_pb_loc )) deallocate (liste_type_pb_loc, stat=stat)


        
   end subroutine recuperer_message_chaine


   subroutine recuperer_message_type(message,nb_mes)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  recuperer_message_type
!
!$Resume
!	Routine permettant de récupérer l'historique des messages
!
!$Description
!	Routine permettant de récupérer les messages dans une structure MSP_message
!-	Il existe trois mode de fonctionnements:
!.		1) Récupérer le dernier message, il s'agit du mode par défaut (nb_mes absent ou nb_mes = MSP_dernier_message)
!.		2) Récupérer tous les messages (nb_mes = MSP_tous_messages)
!.		3) Récupérer le n derniers messages (nb_mes = n) 
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call recuperer_message_type(message,[nb_mes])
!.    type(MSP_message) :: message
!.    integer :: nb_mes
!
!$Arguments
!>S     message  :<MSP_message>   Structure contenant le(s) message(s) 
!>[E]   nb_mes   :<integer>       Argument permettant de récupérer le dernier messsage (MSP_dernier_message)[defaut],
!-				   tous les messages (MSP_tous_messages), 
!-				   les n derniers messages (n) 
!
!$Common
!
!$Routines
!#V
!- recuperer_message_chaine
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR RECUPERER MESSAGE TYPE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

     ! Arguments
     type(MSP_message),intent(out) ::message
     integer,intent(in),optional :: nb_mes



     ! Variables locales
     integer :: nb_mes_loc, i, stat
     integer :: nb_message_traite
     integer :: nb_mes_filtre

     character(len=PM_len_routine),pointer,dimension(:) :: liste_routine => NULL()
     character(len=PM_len_code_pb),pointer,dimension(:)  :: liste_code_pb => NULL()
     character(len=PM_len_message),pointer,dimension(:) :: liste_message => NULL()
     logical,pointer,dimension(:):: liste_type_pb => NULL()
   


     ! Début du programme
     if ( nb_message == 0) then
        message%non_vide = .false.
     else
        message%non_vide = .true.
     end if

     ! initialisation de la structure message
     if (associated(message%routine)) then
        deallocate(message%routine, stat=stat)
     end if
     if ( associated(message%erreur)) then
        deallocate(message%erreur, stat=stat)
     end if
     if (associated(message%message)) then
        deallocate(message%message, stat=stat)
     end if
     if (associated(message%warning)) then
        deallocate(message%warning, stat=stat)
     end if


     ! Test sur la présence du nombre de message
     if ( present(nb_mes)) then
        nb_mes_loc = nb_mes
     else
        nb_mes_loc = MSP_dernier_message
     end if


     ! Test sur la validité du nombre de message
     if (nb_mes_loc > nb_message) then
        nb_mes_loc = nb_message
     end if


     ! Récupération de tous les messages sous forme de chaines
     call recuperer_message_chaine(liste_message=liste_message,&
          liste_routine=liste_routine,liste_code_pb=liste_code_pb,&
          liste_type_pb=liste_type_pb)

     ! Traitement en fonction des cas
     select case (nb_mes_loc) 


        ! Cas du dernier message
     case (MSP_dernier_message)
        ! allocation de la taille du message
        nb_mes_filtre   = 1
     case (MSP_tous_messages)
        ! Récupération du nombre de messages
        nb_mes_filtre = size(liste_message)
     case default
        nb_message_traite = size(liste_message)
        nb_mes_filtre = min(nb_message_traite,nb_mes_loc)
     end select


     
     ! Allocation de la structure message
     allocate(message%routine(nb_mes_filtre))
     allocate(message%erreur(nb_mes_filtre))
     allocate(message%message(nb_mes_filtre))
     allocate(message%warning(nb_mes_filtre))

     ! Affectation des valeurs
     do i = 1 , nb_mes_filtre
        message%routine(i) = liste_routine(i)
        message%erreur(i)  = liste_code_pb(i)
        message%message(i) = liste_message(i)
        message%warning(i) = liste_type_pb(i)
     enddo


     ! Cas où seul un message vide était présent
     if (nb_mes_filtre == 1) then
        i = len_trim(liste_routine(1)) + len_trim(liste_code_pb(1))
        i = i + len_trim(liste_message(1)) 
        if (i==0) message%non_vide=.false.
     end if

     



     ! Désallocation mémoire
     if (associated(liste_routine )) deallocate (liste_routine, stat=stat)
     if (associated(liste_code_pb )) deallocate (liste_code_pb, stat=stat)
     if (associated(liste_message )) deallocate (liste_message, stat=stat)
     if (associated(liste_type_pb )) deallocate (liste_type_pb, stat=stat)

   end subroutine recuperer_message_type

  logical function  comparer_erreur_code_erreur(code_erreur1,code_erreur2) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  comparer_erreur_code_erreur
!
!$Resume
!	Fonction logique permettant de comparer deux chaînes des longueurs quelconques
!
!$Description
!	Fonction logique permettant de comparer deux chaînes des longueurs quelconques
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  ok = comparer_erreur_code_erreur(code_erreur1,code_erreur2)
!.    character(len=*) :: code_erreur1,code_erreur2
!
!$Arguments
!>E     code_erreur1  :<LEN=*>     chaîne 1 
!>E     code_erreur2  :<LEN=*>     chaîne 2 
!>S     ok            :<logical>   booléen 
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
!  ERREUR COMPARER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    character(len=*),intent(in) :: code_erreur1,code_erreur2

    ok = ( trim(string=code_erreur1) == trim(string=code_erreur2))

  end function comparer_erreur_code_erreur

    

  logical function MSP_comparer_erreur(erreur) result (ok) 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_comparer_erreur
!
!$Resume
!	Fonction logique permettant de savoir si la dernière erreur 
!	est égale à "erreur"
!
!$Description
!	Fonction logique permettant de savoir si la dernière erreur 
!	est égale à "erreur"
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_comparer_erreur(erreur)
!.    character(len=*) :: erreur
!
!$Arguments
!>E     erreur  :<LEN=*>     Code erreur (warning)
!>S     ok      :<logical>   Booléen (.true. si l'erreur courant est identique à erreur, .false. sinon)
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
!  ERREUR COMPARER 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none


    character(len=*),intent(in) :: erreur


    ok = ( code_courant .equal. erreur)

  end function MSP_comparer_erreur

  recursive subroutine afficher_message_chaine(unit,message,routine,code_pb,type_pb,&
       liste_message,liste_routine,liste_code_pb,liste_type_pb)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficher_message_chaine
!
!$Resume
!	Routine permettant d'afficher un message à l'écran
!
!$Description
!	Routine permettant d'afficher un message à l'écran
!-	Cette routine permet d'afficher des erreurs et des warnings
!-	Tous les élément du message sont optionels
!-	Exemples de message:
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code erreur : FATAL)
!.	*** ERREUR:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!.	*** ERREUR:
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	 (code warning : Warning)
!.	*** WARNING:
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM (code warning : Warning)
!.	*** WARNING:   (UNKNOWN)
!.	    -> PLEASE CONTACT CNES FLIGHT DYNAMICS TEAM
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call afficher_message_chaine([unit],[message],[routine],[code_pb],[type_pb],&
!.           [liste_message],[liste_routine],[liste_code_pb],[liste_type_pb])
!.    integer :: unit
!.    character(len=*) :: message
!.    character(len=*) :: routine
!.    character(len=*) :: code_pb
!.    logical :: type_pb
!.    character(len=PM_len_message) ,dimension(:),pointer :: liste_message
!.    character(len=PM_len_routine) ,dimension(:),pointer :: liste_routine
!.    character(len=PM_len_code_pb) ,dimension(:),pointer :: liste_code_pb
!.    logical,dimension(:),pointer :: liste_type_pb
!
!$Arguments
!>[E]   unit           :<integer>                              Numéro d'unité logique (0 ou 6 pour la sortie standart) [défaut 6] 
!>[E]   message        :<LEN=*>                                Message 
!>[E]   routine        :<LEN=*>                                Nom de la routine 
!>[E]   code_pb        :<LEN=*>                                Code erreur (warning) 
!>[E]   type_pb        :<logical>                              Type indiquant s'il s'agit d'un warning (TRUE) ou d'une erreur (FALSE)
!>[E/S] liste_message  :<LEN=PM_len_message,DIM=(:),pointer>   Liste des messages 
!>[E/S] liste_routine  :<LEN=PM_len_routine,DIM=(:),pointer>   Liste de routines 
!>[E/S] liste_code_pb  :<LEN=PM_len_code_pb,DIM=(:),pointer>   Liste des code_pb
!>[E/S] liste_type_pb  :<logical,DIM=(:),pointer>              Liste des types warning (TRUE) erreur (FALSE)
!
!$Common
!
!$Routines
!#V
!- afficher_message_chaine
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR AFFICHER MESSAGE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    implicit none


    ! Arguments d'appel
    integer,intent(in),optional::unit
    character(len=*) ,intent(in),optional :: message
    character(len=*) ,intent(in),optional :: routine
    character(len=*) ,intent(in),optional :: code_pb
    logical,optional,intent(in) :: type_pb
    character(len=PM_len_message) ,optional,dimension(:),pointer :: liste_message
    character(len=PM_len_routine) ,optional,dimension(:),pointer :: liste_routine
    character(len=PM_len_code_pb) ,optional,dimension(:),pointer :: liste_code_pb
    logical,optional,dimension(:),pointer :: liste_type_pb



    ! Variables locales
    integer ::unit_loc 
    logical ::loc_warning
    character(len=1250) :: mon_format
    integer ::nb_mes,i
    character(len=PM_len_message) :: message_loc
    character(len=PM_len_routine)  :: routine_loc
    character(len=PM_len_code_pb)  :: code_pb_loc



    ! Affectation du numéro d'unité logique par défaut il s'agit de la sortie standard
    if (present(unit)) then
       unit_loc = unit
    else
       unit_loc = MAGE_ENUM_ECRAN
    end if


    if ((.not.present(liste_message)).or.(.not.present(liste_routine)).or.&
         (.not.present(liste_code_pb))) then

       ! Affichage simple d'un message

       ! Affectation de l'erreur locale
       if ( present(type_pb)) then
          loc_warning = type_pb
       else
          loc_warning = MSP_warning
       end if
       

       if ( loc_warning) then
          mon_format='("*** WARNING: '
       else
          mon_format='("*** ERREUR: '
       end if
       ! Test sur la présence du nom de la routine
       if ( present(routine)) then
          ! Vérification que la chaîne n'est pas vide
          if ( len_trim(string=routine) /= 0) then
             ! Ecriture du nom de la routine dans le message affiché
             mon_format = trim(string=mon_format)//"   ("//trim(string=routine)//')'
          end if
       end if

       ! Insertion d'un retour "chariot"
       mon_format = trim(string=mon_format)//'", / '

       if ( present(message)) then


          ! Vérification que la chaîne n'est pas vide
          if ( len_trim(string=message) /= 0) then
             ! Ecriture du message erreur
             mon_format=trim(string=mon_format)//',"    -> '//trim(string=message)//'"'
          end if
       end if
       if (present(code_pb)) then
          
          ! Vérification que la chaîne n'est pas vide
          if ( len_trim(string=code_pb) /= 0) then
             if ( loc_warning) then
                ! ecriture du code erreur
                mon_format=trim(string=mon_format)//'," (code warning : '//trim(string=code_pb)//')"'
             else
                mon_format=trim(string=mon_format)//'," (code erreur : '//trim(string=code_pb)//')"'
             end if
          end if
       end if
       ! fermeture du format
       mon_format=trim(string=mon_format)//")"

       write(unit_loc,trim(mon_format)) 
       
    else 
       ! Affichage d'une liste de message
       nb_mes = size(liste_message)

       boucle_mes : do i = 1,nb_mes

          write(unit_loc,'(" ",i3," :")',ADVANCE='NO') i
          
          if ( present(liste_message)) then
             message_loc = liste_message(i)
          else
             message_loc = ""
          end if
       
          if ( presenT(liste_routine)) then
             routine_loc = liste_routine(i)
          else
             routine_loc = ""
          end if

          if (present(liste_code_pb)) then
             code_pb_loc = liste_code_pb(i)
          else
             code_pb_loc =""
          end if

          if (present(liste_type_pb)) then
             call afficher_message_chaine(unit=unit_loc,message=message_loc,routine=routine_loc,code_pb=code_pb_loc,&
                  type_pb=liste_type_pb(i))
          else
             call afficher_message_chaine(unit=unit_loc,message=message_loc,routine=routine_loc,code_pb=code_pb_loc)
          end if
       end do boucle_mes
       
    end if
  end subroutine Afficher_message_chaine



  subroutine afficher_message_type(message,unit,flag_routine,flag_message,flag_code_pb)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  afficher_message_type
!
!$Resume
!	Routine permettant d'afficher une structure MSP_message
!
!$Description
!	Routine permettant d'afficher à l'écran une structure MSP_message
!-	Les différents éléments de la structure message (routine,message,code_pb) sont optionnels.
!-	Les flags permettent de les sélectionner (true on affiche, false on n'affiche pas) par défaut
!	ces flags sont positionnés à true
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call afficher_message_type(message,[unit],[flag_routine],[flag_message],[flag_code_pb])
!.    type (MSP_message) :: message
!.    integer :: unit
!.    logical :: flag_routine
!.    logical :: flag_message
!.    logical :: flag_code_pb
!
!$Arguments
!>E     message       :<MSP_message>   Structure contenant les infos relatives au message (message, routine, code_pb, ...) 
!>[E]   unit          :<integer>       Numéro d'unité logique (0 ou 6 pour la sortie standart) [défaut 6]
!>[E]   flag_routine  :<logical>       Flags permettant d'afficher ou non le nom de la routine [defaut affiché] 
!>[E]   flag_message  :<logical>       Flags permettant d'afficher ou non le message [defaut affiché] 
!>[E]   flag_code_pb  :<logical>       Flags permettant d'afficher ou non le code PB [defaut affiché] 
!
!$Common
!
!$Routines
!#V
!- afficher_message_chaine
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR AFFICHER MESSAGE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    type (MSP_message) ,intent(in) :: message
    integer,intent(in),optional :: unit
    logical,intent(in),optional ::flag_routine
    logical,intent(in),optional ::flag_message
    logical,intent(in),optional ::flag_code_pb

    ! Variables locales
    integer::unit_loc
    logical:: flag_routine_loc,flag_message_loc,flag_code_pb_loc
    type (MSP_message) :: message_loc

    !Debut du programme


    ! Affichage s'il y a quelque chose à afficher
    if ( message%non_vide) then
       ! Affectation du numéro d'unité logique par défaut il s'agit de la sortie standart
       if (present(unit)) then
          unit_loc = unit
       else
          unit_loc = MAGE_ENUM_ECRAN
       end if
    
       ! Affection du message local
       message_loc = message
   
       if (present(flag_routine) ) then
          flag_routine_loc = flag_routine
       else
          flag_routine_loc = .true.
       end if

       if ( present(flag_message)) then
          flag_message_loc = flag_message
       else
          flag_message_loc = .true.
       end if

       if (present(flag_code_pb)) then
          flag_code_pb_loc = flag_code_pb
       else
          flag_code_pb_loc = .true.
       end if


       ! traitement des différents flags
       
       if ( .not.flag_routine_loc) then
          message_loc%routine(:)=""
       end if
       
       if (.not.flag_message_loc) then
          message_loc%message(:) =""
       end if
       
       if (.not.flag_code_pb_loc) then
          message_loc%erreur(:)=""
       end if

       call afficher_message_chaine(unit=unit_loc,liste_type_pb=message_loc%warning,&
            liste_code_pb=message_loc%erreur,&
            liste_message=message_loc%message,liste_routine=message_loc%routine)
    end if
  end subroutine afficher_message_type


   function MSP_gen_messages (nom_sp_propag) result (retour) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!$<AM-V2.0>
!
!$Nom
!  MSP_gen_messages
!
!$Resume
!  Gestion des problèmes selon qu'ils sont des erreurs ou des warnings. 
!
!$Description
!  Gestion des problèmes selon qu'ils sont des erreurs ou des warnings. 
!
!$Auteur
!  J. F. GOESTER 
!
!$Acces
!  PUBLIC
!
!$Usage
!  retour = MSP_gen_messages ([nom_sp_propag])
!.    character(LEN=*) :: nom_sp_propag 
!.    logical :: retour 
!
!$Arguments
!>[E]   nom_sp_propag  :<LEN=*>     nom de la routine qui propage le problème 
!>S     retour         :<logical>   booléen à .true. si c'est une erreur et à .false. si c'est un warning 
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_annuler_probleme
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ERREUR GERER MESSAGE 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !

      implicit none 

      character(LEN=*), intent(IN), optional :: nom_sp_propag 

      logical :: retour 

      retour = .false. 

      if ( MSP_PROBLEME ) then 

         if ( present (nom_sp_propag) ) then 
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",&
                 routine=nom_sp_propag) 
         endif 

         if ( MSP_ERREUR ) then 
            retour = .true. 
            return 
         endif 

         call MSP_annuler_probleme () 

      endif 

    end function MSP_gen_messages
  

end module MSP_gestion_erreur

