module MSP_ATMOSPHERE_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_ATMOSPHERE_DEF
!
!$Resume
!  Module gérant les modèles d'atmosphère.
!
!$Description
!  Module gérant les modèles d'atmosphère.
!
!$Auteur
!  S. ROSTAN
!
!$Version
!  $Id: MSP_ATMOSPHERE_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_ATMOSPHERE_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.61  2010/10/20 09:35:42  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.60  2010/06/17 08:16:47  mercadig
!  VERSION::FA-ID:1411:17/06/2010: Mise a 0 des variables du modele si h > 1000 km, on ne retourne pas de warning pour les cas limites
!
!  Revision 1.59  2010/06/16 15:31:16  mercadig
!  VERSION::FA-ID:1411:16/06/2010: Meilleure gestion des cas limites
!
!  Revision 1.58  2010/06/16 08:19:07  mercadig
!  VERSION::FA-ID:1411:16/06/2010: Gestion des cas limites pour le modele US76 (retour de warning)
!
!  Revision 1.57  2009/11/09 14:13:49  mercadig
!  DM-ID 842: Gestion du mode d'activite solaire reelle via COMPAS (creation, consultation et affichage de la structure: ajout de la date de reference)
!
!  Revision 1.56  2009/10/13 13:37:32  kvernelo
!  VERSION::FA-ID:1304:13/10/2009:Utilisation de us76d dans COMPAS a la place de MSPRO
!  Revision 1.55  2009/08/27 12:29:57  cml
!  DM-ID 1120 : Correction des cartouches des routines
!  Revision 1.54  2009/08/27 12:26:03  cml
!  DM-ID 1120 : Ajout de valeurs par défaut pour la structure
!  Revision 1.53  2009/08/24 16:05:26  cml
!  DM-ID 1120 : Utilisation de la routine COMPAS_BASE pour le modèle exponentiel
!  Revision 1.52  2009/07/09 12:50:39  mercadig
!  FA-ID 1303: Correction du calcul de la direction de portance (si pas de loi d attitude)
!  Revision 1.51  2009/07/09 12:34:33  mercadig
!  AQ: Ajout de la description des variables tscale et beta dans les cartouches
!  Revision 1.50  2009/05/28 13:57:20  cml
!  FA-ID 1302 : Au dessous de 120 km, DTM78 renvoie la densite 120 km
!  Revision 1.49  2009/03/10 08:58:43  tanguyy
!  FA-ID 1177 : conversion m->km avant l'appel au modele ROAT77
!  Revision 1.48  2009/01/28 08:39:12  cml
!  FA-ID 1225 : Changement de la declaration de deltat pour le modele US76D
!  Revision 1.47  2009/01/14 08:16:15  cml
!  FA-ID 1179 : Correction du code pour la hauteur pour l EMCD 4.3
!  Revision 1.46  2009/01/13 14:02:21  cml
!  FA-ID 1179 : Ajout de lambda_gw et correction des appels aux modeles EMCD 42 et 43
!  Revision 1.45  2008/12/05 13:47:42  mercadig
!  FA-ID 1172: Desallocation de la structure MSP_MCI dans la routine MSP_calculer_frottement
!  Revision 1.44  2008/12/04 13:18:18  tanguyy
!  FA-ID 1172 : désallocation mémoire de la structure MSP_AERO dans MSP_calculer_frottement
!  Revision 1.43  2008/11/24 10:35:35  mercadig
!  DM-ID 733 : Mise a jour des cartouches
!  Revision 1.42  2008/11/24 10:21:54  tanguyy
!  DM-ID 733 : correction pour les panneaux solaires : l'attitude des panneaux solaires est prise en compte par msp_calculer_surf_app si 'modatt' est active (=1)
!  Revision 1.41  2008/11/20 12:46:01  cml
!  AQ : Suppression de variables inutilisees
!  Revision 1.40  2008/11/19 11:10:22  tanguyy
!  DM-ID 733 : Calcul de l'activité solaire directement par la fonction MSP_calculer_actsol
!  Revision 1.39  2008/11/18 19:04:28  mercadig
!  DM-ID 733 : Implementation du calcul atmosphere selon PSIMU
!  Revision 1.38  2008/11/12 10:11:48  mercadig
!  DM-ID 733: Implementation du calcul des forces de frottement atmospherique
!  Revision 1.37  2008/08/11 12:17:45  cml
!  DM-ID 1091 : Branchement du modele d'atmosphere EMCD 4.3
!  Revision 1.36  2008/08/08 13:56:02  gss
!  DM-ID 1058 : (portage g95) forcage du typage entre marsgram%marsgram_per (entier)
!  et marsgram_per local (reel). Initialisation à NULL des pointeurs lors de leur
!  déclaration.
!  Revision 1.35  2008/07/04 15:02:46  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.34  2008/04/16 13:12:21  tanguyy
!  AQ. Correction sur code mort de msp_calculer_frottement
!  Revision 1.33  2008/04/08 12:58:36  huec
!  FA-ID 1009 : Les memes codes que dans COMPAS sont utilises pour MECASPA
!  Revision 1.32  2008/02/22 13:56:37  huec
!  FA-ID 968 : Suppression de variables declarees en double
!  Revision 1.31  2008/02/15 08:50:23  huec
!  DM-ID 11 : Utilisation de la base COMPAS pour le saut du TUC
!  Revision 1.30  2007/11/27 14:55:17  huec
!  DM-ID 699 : Amelioration des moyens de tests MECASPA
!  Revision 1.29  2007/11/26 09:11:38  huec
!  FA-ID 776 : Variables locales non inutilisees
!  Revision 1.28  2007/11/23 13:56:26  jpi
!  DM-ID 551 : couverture de tests, modele venus
!  Revision 1.27  2007/11/22 15:46:30  jpi
!  DM-ID 551 : ajout modele EMCD42 dans MECASPA
!  Revision 1.26  2007/11/19 09:40:00  huec
!  DM-ID 820 : Scripts de generation automatique, utilisation de makemake V2.0
!  Revision 1.25  2007/11/09 09:04:43  tanguyy
!  Correction d'un test dans MSP_ATMOSPHERE
!  Revision 1.24  2007/11/05 16:03:54  tanguyy
!  DM-ID 733 : generalisation de l'utilisation des dates jj/sec dans les modeles d'atmosphere
!  Revision 1.23  2007/10/23 15:02:40  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.22  2007/10/16 12:54:53  jpi
!  DM-ID744:modele Venus petropoulos88
!  Revision 1.21  2007/06/18 10:14:23  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.20  2007/06/15 13:49:29  vivaresf
!  FA-ID 746 : mise en inout des structure actsol et vent contenant des pointeurs dans la fonction de consultation pour
!  permettre la désallocation des pointeurs éventuellement déjà affectés
!  Revision 1.19  2006/11/15 10:09:35  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.18  2006/11/09 09:13:54  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.17  2006/06/02 11:21:52  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.16  2005/03/09 09:06:13  pauh
!  DM 111 : ajout de modules pour les divers modeles d atmospheres
!  Revision 1.15  2005/03/08 07:32:34  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.14  2005/03/03 15:05:19  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.13  2005/03/03 09:14:49  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.12  2005/03/01 09:59:41  pauh
!  DM 111 : nouveaux modules pour les modeles d atmospheres martiennes
!  Revision 1.11  2005/01/21 17:21:22  rostan
!  ajout calculer_atmosphere et maj du reste
!  Revision 1.10  2005/01/20 13:56:10  pauh
!  FA_332
!  Revision 1.9.4.1  2005/01/19 09:43:14  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.9  2003/11/26 11:19:45  adm_ipsi
!  DM-ID 9, Ajout du champ ficactsol dans la structure MSP_ATMOSPHERE
!  Revision 1.8  2003/03/24 09:03:14  adm_ipsi
!  PhB - Ajout du sous-programme MSP_roat77
!  Revision 1.7  2003/02/13 19:00:58  adm_ipsi
!  MSP_creer_atmosphere, suppression des blocs non executables
!  Revision 1.6  2003/02/12 15:47:06  adm_ipsi
!  MSP_creer_atmosphere, Remplacement de IO_e_us76 par mp_atm_us76d
!  Revision 1.5  2002/12/11 10:18:09  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.4  2002/12/04 18:08:24  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:00  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/05 17:49:14  adm_ipsi
!  Ajout de l'interface MSP_display_atmosphere, et dans MSP_creer_atmosphere, traitement simplifié des types de vent
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.6  2002/05/03 07:48:17  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL() etargument nul dans effacer_)
!  Revision 1.5  2000/06/13 11:24:27  util_am
!  Privatisation du contenu de la structure MSP_ATMOSPHERE
!  Suppression de la routine MSP_initialiser_atmosphère (doublon de MSP_effacer_atmosphere)
!  Ajout des routines : MSP_consulter_atmosphere, MSP_modifier_atmosphere
!  Ajout des interface en anglais des routines publiques
!  Mise à jour des cartouches : ajout des sections Mots-cles et Voir-Aussi
!  Revision 1.4  2000/03/27 07:56:26  rousseau
!  ajout du modele d'atmosphere MARSGRAM
!  Revision 1.3  2000/02/10 16:09:44  rousseau
!  creation d'une subroutine permettant de desallouer proprement la structure atmosphere
!  creation d'une subroutine permettant d'initialiser proprement une structure atmosphere
!  Revision 1.2  1999/10/14 14:19:19  rousseau
!  ajout d'un modele de vent
!  Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_ATMOSPHERE_DEF
!
!$Structure
!
!: MSP_ATMOSPHERE : 
!#V
!>     flag_func        : <logical,private>            Flag indiquant si la structure est créée par une fonction
!>     nom_atmosphere   : <LEN=80,private>             nom du modèle
!>     modele           : <LEN=80,private>             type de modèle (MSP_MODATM_NUL,MSP_MODATM_CIRA88_MSIS86,MSP_MODATM_US76,
!                                                       MSP_MODATM_MARS_RUSSE,MSP_MODATM_MARS_EMCD,MSP_MODATM_EXP)
!>     id_modele        : <integer,private>            
!>     actsol           : <MSP_ACTSOL,private>         activité solaire (MSP_ENUM_ACTSOL_REELLE, MSP_ENUM_ACTSOL_STD ou MSP_ENUM_ACTSOL_COMPAS)
!>     us76             : <MSP_ATM_US76,private>       
!>     marsrusse        : <MSP_ATM_MARSRUSSE,private>  
!>     emcd             : <MSP_ATM_EMCD,private>       
!>     marsgram         : <MSP_ATM_MARSGRAM,private>   
!>     exp              : <MSP_ATM_EXP,private>        
!>     venus            : <MSP_ATM_VENUS,private>      
!>     vent             : <MSP_VENT,private>           
!#
!
!$Global
!
!>  MSP_MODATM_NUL              : <LEN=MSP_LONG_CHAINE,parameter>  pas d'atmosphère
!pas d'atmosphère
!>  MSP_MODATM_CIRA88_MSIS86    : <LEN=MSP_LONG_CHAINE,parameter>  modèle CIRA88 et MSIS86 (Terre)
!modèle CIRA88 et MSIS86 (Terre)
!>  MSP_MODATM_US76             : <LEN=MSP_LONG_CHAINE,parameter>  modèle US76 (Terre)
!modèle US76 (Terre)
!>  MSP_MODATM_MARS_RUSSE       : <LEN=MSP_LONG_CHAINE,parameter>  modèle Russe (Mars)
!modèle Russe (Mars)
!>  MSP_MODATM_MARS_EMCD        : <LEN=MSP_LONG_CHAINE,parameter>  modèle Européen (Mars)
!modèle Européen (Mars)
!>  MSP_MODATM_EXP              : <LEN=MSP_LONG_CHAINE,parameter>  modele exponentiel
!modele exponentiel
!>  MSP_MODATM_MARSGRAM         : <LEN=MSP_LONG_CHAINE,parameter>  modèle americain gram (Mars)
!modèle americain gram (Mars)
!>  MSP_MODATM_AT77             : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_DTM              : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_MARS_EMCD_23A    : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_MARS_EMCD_31     : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_MARS_EMCD_42     : <LEN=MSP_LONG_CHAINE,parameter>  modèle européen EMCD 4.2 (Mars) 
!>  MSP_MODATM_MARS_EMCD_43     : <LEN=MSP_LONG_CHAINE,parameter>  modèle européen EMCD 4.3 (Mars)
!>  MSP_MODATM_MARSGRAM_2001    : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_VENUS            : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_MSIS_90          : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_NRL_MSISE_2000   : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_MODATM_MET_88           : <LEN=MSP_LONG_CHAINE,parameter>  
!>  MSP_IDATM_CIRA88_MSIS86     : <integer,parameter>              
!>  MSP_IDATM_US76              : <integer,parameter>              
!>  MSP_IDATM_DTM               : <integer,parameter>              
!>  MSP_IDATM_AT77              : <integer,parameter>              
!>  MSP_IDATM_MARS_RUSSE        : <integer,parameter>              
!>  MSP_IDATM_MARS_EMCD         : <integer,parameter>              
!>  MSP_IDATM_MARS_EMCD_23A     : <integer,parameter>              
!>  MSP_IDATM_MARS_EMCD_31      : <integer,parameter>              
!>  MSP_IDATM_MARS_EMCD_42      : <integer,parameter>              
!>  MSP_IDATM_MARS_EMCD_43      : <integer,parameter>              
!>  MSP_IDATM_MARSGRAM          : <integer,parameter>              
!>  MSP_IDATM_MARSGRAM_2001     : <integer,parameter>              
!>  MSP_IDATM_EXP               : <integer,parameter>              
!>  MSP_IDATM_MSIS_90           : <integer,parameter>              
!>  MSP_IDATM_NRL_MSISE_2000    : <integer,parameter>              
!>  MSP_IDATM_MET_88            : <integer,parameter>              
!>  MSP_IDATM_VENUS             : <integer,parameter>              
!>  MSP_VENUS_CSTE_RHO0         : <pm_reel,parameter>              
!>  MSP_VENUS_CSTE_BETA         : <pm_reel,parameter>              
!>  MSP_REQ_TERRE               : <pm_reel,parameter>              
!>  MSP_APL_TERRE               : <pm_reel,parameter>              
!>  MSP_REQ_MARS                : <pm_reel,parameter>              
!>  MSP_APL_MARS                : <pm_reel,parameter>              
!$Common
!
!$Routines
!- MSP_clear_atmosphere
!- MSP_create_atmosphere
!- MSP_get_atmosphere_data
!- MSP_display_atmosphere
!- MSP_set_atmosphere_data
!- MSP_calculate_drag
!- MSP_calculate_atmosphere
!- MSP_effacer_atmosphere
!- MSP_consulter_atmosphere
!- MSP_modifier_atmosphere
!- MSP_afficher_atmosphere
!- MSP_vgram
!- MSP_calculer_atmosphere_old
!- MSP_calculer_atmosphere
!- MSP_calculer_atmosphere_terre
!- MSP_calculer_atmosphere_mars
!- MSP_calculer_atmosphere_venus
!- MSP_calculer_frottement
!- MSP_acc_frot
!#V
!- MSP_egaler_atmosphere
!#
!
!$Fonctions
!- MSP_creer_atmosphere
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- cps_atmosphere
!- cps_modele_emcd31
!- cps_modele_emcd42
!- cps_modele_emcd43
!- MSP_ACTSOL_DEF
!- MSP_ATM_US76_DEF
!- MSP_ATM_EXP_DEF
!- MSP_ATM_EMCD_DEF
!- MSP_ATM_MARSRUSSE_DEF
!- MSP_ATM_MARSGRAM_DEF
!- MSP_VENT_DEF
!- MSP_MODVENT_DEF
!- MSP_ATM_FUNC
!- MSP_MECASPA_DEF
!- MSP_BULLETIN_DEF
!- MSP_ATM_VENUS_DEF
!- eph_constantes
!- MSP_VEHICULE_DEF
!- MSP_MCI_DEF
!- MSP_AERO_DEF
!#
!
!$Interface
!> msp_get_atmosphere_data :   MSP_consulter_atmosphere
!> msp_set_atmosphere_data :   MSP_modifier_atmosphere
!> assignment(=) :             MSP_egaler_atmosphere
!> msp_create_atmosphere :     MSP_creer_atmosphere
!> msp_display_atmosphere :    MSP_afficher_atmosphere
!> msp_clear_atmosphere :      MSP_effacer_atmosphere
!> msp_calculate_drag :        MSP_calculer_frottement
!> msp_calculate_atmosphere :  MSP_calculer_atmosphere
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE 
!
!$Voir-Aussi
!#V
!.  MSP_egaler_atmosphere
!#
!.  MSP_creer_atmosphere MSP_clear_atmosphere MSP_create_atmosphere MSP_get_atmosphere_data
!.  MSP_display_atmosphere MSP_set_atmosphere_data MSP_calculate_drag MSP_calculate_atmosphere
!.  MSP_effacer_atmosphere MSP_consulter_atmosphere MSP_modifier_atmosphere MSP_afficher_atmosphere
!.  MSP_vgram MSP_calculer_atmosphere_old MSP_calculer_atmosphere MSP_calculer_atmosphere_terre
!.  MSP_calculer_atmosphere_mars MSP_calculer_atmosphere_venus MSP_calculer_frottement MSP_acc_frot
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB, only : pm_reel, PM_PI, PM_PI_SUR2
   use MSP_GESTION_ERREUR
   use cps_atmosphere
   use cps_modele_emcd31
   use cps_modele_emcd42
   use cps_modele_emcd43
   use MSP_ACTSOL_DEF
   use MSP_ATM_US76_DEF
   use MSP_ATM_EXP_DEF
   use MSP_ATM_EMCD_DEF
   use MSP_ATM_MARSRUSSE_DEF
   use MSP_ATM_MARSGRAM_DEF
   use MSP_VENT_DEF
   use MSP_MODVENT_DEF
   use MSP_ATM_FUNC
   use MSP_MECASPA_DEF
   use MSP_BULLETIN_DEF, only : MSP_date_etutc,PM_TUC,PM_TE   
   use MSP_ATM_VENUS_DEF
   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_ATMOSPHERE_DEF.F90 365 2013-02-18 12:36:19Z aadt $'



   type MSP_ATMOSPHERE

      private

      logical :: flag_func
      character(LEN=80)  :: nom_atmosphere
      character(LEN=80)  :: modele
      integer :: id_modele ! numero d'identifiant de modele
      ! Données nécessaires au modèles d'atmosphère terrestres
      ! structure activite solaire reelle
      type(MSP_ACTSOL):: actsol
      ! Donnees necessaires au modele us76
      type(MSP_ATM_US76) :: us76
      ! Données nécessaires au modèles d'atmosphère martiens:
      ! - Modele Mars90
      type(MSP_ATM_MARSRUSSE)::marsrusse
      ! - Modeles EMCD 2.3.A, 3.1, 4.2 et 4.3
      type(MSP_ATM_EMCD)  :: emcd
      ! - Modele Mars GRAM 2001
      type(MSP_ATM_MARSGRAM) ::marsgram
      ! Donnees necessaires au modele exponentiel
      type(MSP_ATM_EXP)::exp
      ! Modele Venus PETROPOULOS88
      type(MSP_ATM_VENUS)::venus
      ! Prise en compte de differents types de vents
      ! Definition du vent (selon SIMBAD)
      type(MSP_VENT)::vent

   end type MSP_ATMOSPHERE

   interface ASSIGNMENT(=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT(=)
!
!$Resume
!  Cette routine permet d'égaler deux structures atmosphère
!
!$Description
!  Cette routine permet d'égaler deux structures atmosphère
!
!$Acces
!  PUBLIC
!
!$Usage
!  =(atmosa= atmosb)
!.    type(MSP_ATMOSPHERE) :: atmosb
!.    type(MSP_ATMOSPHERE) :: atmosa
!
!$Procedures
!#V
!- MSP_egaler_atmosphere
!#
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_egaler_atmosphere
   end interface

   !Interfaces anglaises pour les routines ATMOSPHERE
   interface MSP_clear_atmosphere

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_atmosphere
!
!$Resume
!  This routine deallocates properly an atmosphere derived type
!
!$Description
!  This routine deallocates properly an atmosphere derived type
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_atmosphere (atmos, [nul])
!.    type(MSP_ATMOSPHERE) :: atmos
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_atmosphere
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_atmosphere
   end interface

   interface MSP_create_atmosphere

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_atmosphere
!
!$Resume
!  Create an atmosphere model
!
!$Description
!  Create an atmosphere model
!
!$Acces
!  PUBLIC
!
!$Usage
!  atmosphere = MSP_create_atmosphere ([nom_atmosphere],[modele],[act_sol], [us76],[exp],&
!.                                      emcd,marsrusse,marsgram,venus,vent,&
!.                                      actsol,ficactsol,dateref_actsol,flux,ap,ap7,us76_deltat, &
!.                                      mars_russe_mod,mars_emcd_sce,mars_emcd_per,&
!.                                      marsgram_sce,marsgram_per,vent_nord,vent_est,vent_vert,&
!.                                      vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
!.                                      vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
!.                                      par1_vent,par2_vent,type_vent,type_emcd,&
!.                                      venus_rho0,venus_beta)
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: nom_atmosphere
!.    character(LEN=*) :: modele
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    integer :: dateref_actsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    integer :: marsgram_sce
!.    real (KIND=pm_reel),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!.    integer :: type_emcd
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!
!$Procedures
!- MSP_creer_atmosphere
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_atmosphere
   end interface

   interface MSP_get_atmosphere_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_atmosphere_data
!
!$Resume
!  Get atmosphere model related data
!
!$Description
!  Get atmosphere model related data
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_atmosphere_data(atmosphere,[nom_atmosphere],[modele],[act_sol],[us76],&
!.            exp,emcd, marsrusse,marsgram,venus,vent,&
!.                                           actsol,ficactsol,dateref_actsol,flux,ap,ap7,us76_deltat, &
!.                                           mars_russe_mod,mars_emcd_sce, &
!.                                           mars_emcd_per,mars_emcd_lambda_gw,&
!.                                           marsgram_per,&
!.                                           venus_rho0, venus_beta,&
!.                                           vent_nord,vent_est,vent_vert,&
!.                                           vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
!.                                           vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
!.                                           par1_vent,par2_vent,type_vent) 
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: modele
!.    character(LEN=*) :: nom_atmosphere
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP 
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    integer :: dateref_actsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    real (KIND=PM_REEL),dimension(2) :: mars_emcd_per
!.    real (KIND=PM_REEL) :: mars_emcd_lambda_gw
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),pointer,dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!
!$Procedures
!- MSP_consulter_atmosphere
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_consulter_atmosphere
   end interface

   interface MSP_display_atmosphere
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_atmosphere
!
!$Resume
!  display the content of an atmosphere structure
!
!$Description
!  display the content of an atmosphere structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_atmosphere (atmosphere,[ilog])
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    integer :: ilog
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_atmosphere
   end interface

   interface MSP_set_atmosphere_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_atmosphere_data
!
!$Resume
!  Modifies attributes of an atmosphere model
!
!$Description
!  Modifies attributes of an atmosphere model
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_atmosphere_data(atmosphere,[nom_atmosphere],[modele],[act_sol],[us76],&
!.            exp, emcd,marsrusse,marsgram,vent,venus,actsol,ficactsol,flux,ap,ap7,us76_deltat, &
!.                                      mars_russe_mod,mars_emcd_sce,mars_emcd_per,&
!.                                      marsgram_per,&
!.                                      venus_rho0, venus_beta,&
!.                                      vent_nord,vent_est,vent_vert,&
!.                                      vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
!.                                      vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
!.                                      par1_vent,par2_vent,type_vent) 
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: modele
!.    character(LEN=*) :: nom_atmosphere
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    real (KIND=pm_reel),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!
!$Procedures
!- MSP_modifier_atmosphere
!
!$Remarques
!
!$Mots-cles
! ATMOSPHERE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_atmosphere
   end interface

   interface MSP_calculate_drag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculate_drag
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculate_drag(atm,vehicule,date,corgeod,hsol,vitr,mat_att_veh,incidence,&
!.                 acc_frot,ro,mach,xcd,xcl,vit_vent,modatt,panneaux,mat_att_panneaux,vit_atm)
!.    type (MSP_ATMOSPHERE) :: atm
!.    type (MSP_VEHICULE) :: vehicule
!.    type(tm_jour_sec) :: date
!.    type (tm_geodesique) :: corgeod 
!.    real(kind=pm_reel) :: hsol 
!.    real(kind=pm_reel) :: incidence
!.    real(kind=pm_reel), dimension(3) :: vitr 
!.    real(kind=pm_reel), dimension(3,3) :: mat_att_veh 
!.    real(kind=pm_reel), dimension(3) :: acc_frot
!.    real(kind=pm_reel) :: ro
!.    real(kind=pm_reel) :: mach
!.    real(kind=pm_reel) :: xcd
!.    real(kind=pm_reel) :: xcl
!.    integer :: modatt
!.    integer :: panneaux
!.    real(kind=pm_reel), dimension(3,3) :: mat_att_panneaux
!.    real(kind=pm_reel), dimension(3) :: vit_vent
!.    real(kind=pm_reel), dimension(3) :: vit_atm
!
!$Procedures
!- MSP_calculer_frottement
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_calculer_frottement
   end interface

   interface MSP_calculate_atmosphere

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculate_atmosphere
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculate_atmosphere(atm, date_js_te, corgeod, hsol, ro, vson)
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_jour_sec) :: date_js_te 
!.    type (tm_geodesique) :: corgeod 
!.    real(KIND=PM_REEL) :: hsol 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!
!$Procedures
!- MSP_calculer_atmosphere
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_calculer_atmosphere
   end interface

   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_NUL           = ""
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_CIRA88_MSIS86 = cps_modatm_cira_msis86
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_US76          = cps_modatm_us76d
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_RUSSE    = cps_modatm_mars90
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_EMCD     = "MARS_EMCD"
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_EXP           = cps_modatm_exp
   
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARSGRAM      = "MARSGRAM"

   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_AT77          = cps_modatm_roat77
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_DTM           = cps_modatm_dtm78
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_EMCD_23A = cps_modatm_emcd23
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_EMCD_31  = cps_modatm_emcd31
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_EMCD_42  = cps_modatm_emcd42
   character(LEN=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARS_EMCD_43  = cps_modatm_emcd43
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MARSGRAM_2001 = cps_modatm_gram2001
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_VENUS         = cps_modatm_petropoulos88
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MSIS_90       = cps_modatm_msis90
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_NRL_MSISE_2000= cps_modatm_msis2000
   character(len=MSP_LONG_CHAINE), parameter :: MSP_MODATM_MET_88        = cps_modatm_met88
   
   ! Identifiants de modèle
   integer, parameter :: MSP_IDATM_CIRA88_MSIS86  = 1
   integer, parameter :: MSP_IDATM_US76           = 2
   integer, parameter :: MSP_IDATM_DTM            = 3
   integer, parameter :: MSP_IDATM_AT77           = 4
   integer, parameter :: MSP_IDATM_MARS_RUSSE     = 5
   integer, parameter :: MSP_IDATM_MARS_EMCD      = 6
   integer, parameter :: MSP_IDATM_MARS_EMCD_23A  = 7
   integer, parameter :: MSP_IDATM_MARS_EMCD_31   = 8
   integer, parameter :: MSP_IDATM_MARS_EMCD_42   = 9
   integer, parameter :: MSP_IDATM_MARS_EMCD_43   = 10
   integer, parameter :: MSP_IDATM_MARSGRAM       = 11
   integer, parameter :: MSP_IDATM_MARSGRAM_2001  = 12
   integer, parameter :: MSP_IDATM_EXP            = 13
   integer, parameter :: MSP_IDATM_MSIS_90        = 14
   integer, parameter :: MSP_IDATM_NRL_MSISE_2000 = 15
   integer, parameter :: MSP_IDATM_MET_88         = 16
   integer, parameter :: MSP_IDATM_VENUS          = 17
   
   real(kind=pm_reel), parameter :: MSP_VENUS_CSTE_RHO0    = 530.0_pm_reel
   real(kind=pm_reel), parameter :: MSP_VENUS_CSTE_BETA    = 1.58e-04_pm_reel

   ! Constantes planètes : données issues de PSIMU V9.2.1
   real(kind=pm_reel), parameter :: MSP_REQ_TERRE = 6378135._pm_reel
   real(kind=pm_reel), parameter :: MSP_APL_TERRE = 1._pm_reel/298.26_pm_reel
   real(kind=pm_reel), parameter :: MSP_REQ_MARS = 3397000._pm_reel
   real(kind=pm_reel), parameter :: MSP_APL_MARS = 1._pm_reel/154.409091_pm_reel

   private :: MSP_egaler_atmosphere

   contains

     subroutine MSP_effacer_atmosphere (atmos, nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_atmosphere
!
!$Resume
!  Routine permettant de desallouer proprement la structure atmosphere
!
!$Description
!  Routine permettant de desallouer proprement la structure atmosphere
!
!$Auteur
!  J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_atmosphere (atmos, [nul])
!.    type(MSP_ATMOSPHERE) :: atmos
!.    logical :: nul
!
!$Arguments
!>E/S   atmos  :<MSP_ATMOSPHERE>   
!>[E/S] nul    :<logical>          si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
!
!$Common
!
!$Routines
!- MSP_effacer_actsol
!- MSP_effacer_atm_us76
!- MSP_effacer_atm_exp
!- MSP_effacer_atm_emcd
!- MSP_effacer_atm_marsrusse
!- MSP_effacer_atm_marsgram
!- MSP_effacer_atm_venus
!- MSP_effacer_vent
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       implicit none

       type(MSP_ATMOSPHERE) :: atmos
       logical, optional :: nul

       logical :: nul_tmp
       
       if ( present (nul) ) then
          nul_tmp = nul
       else
          nul_tmp = .false.
       endif

       ! -- Mise à zero des champs scalaires
       atmos%nom_atmosphere=""
       atmos%modele=""
       atmos%id_modele=0
       
       call MSP_effacer_actsol(atmos%actsol,nul_tmp)
       call MSP_effacer_atm_us76(atmos%us76)
       call MSP_effacer_atm_exp(atmos%exp)
       call MSP_effacer_atm_emcd(atmos%emcd)
       call MSP_effacer_atm_marsrusse(atmos%marsrusse)
       call MSP_effacer_atm_marsgram(atmos%marsgram)
       call MSP_effacer_atm_venus(atmos%venus)
       call MSP_effacer_vent(atmos%vent,nul_tmp)


       atmos%flag_func = .false.



   end subroutine MSP_effacer_atmosphere


   function MSP_creer_atmosphere (nom_atmosphere,modele,act_sol, us76,exp,&
                                  emcd,marsrusse,marsgram,venus,vent,&
                                  actsol,ficactsol,dateref_actsol,flux,ap,ap7,us76_deltat, &
                                  mars_russe_mod,mars_emcd_sce,mars_emcd_per,&
                                  marsgram_sce,marsgram_per,vent_nord,vent_est,vent_vert,&
                                  vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
                                  vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
                                  par1_vent,par2_vent,type_vent,type_emcd,&
                                  venus_rho0,venus_beta) result (atmosphere)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_atmosphere
!
!$Resume
!  Création d'un modèle d'atmosphère.
!
!$Description
!  Création d'un modèle d'atmosphère.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  atmosphere = MSP_creer_atmosphere ([nom_atmosphere],[modele],[act_sol], [us76],[exp],&
!.                                      [emcd],[marsrusse],[marsgram],[venus],[vent],&
!.                                      [actsol],[ficactsol],[dateref_actsol],[flux],[ap],[ap7],[us76_deltat], &
!.                                      [mars_russe_mod],[mars_emcd_sce],[mars_emcd_per],&
!.                                      [marsgram_sce],[marsgram_per],[vent_nord],[vent_est],[vent_vert],&
!.                                      [vent_dim1_nord],[vent_dim1_est],[vent_dim1_vert],&
!.                                      [vent_dim2_nord],[vent_dim2_est],[vent_dim2_vert],&
!.                                      [par1_vent],[par2_vent],[type_vent],[type_emcd],&
!.                                      [venus_rho0],[venus_beta])
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: nom_atmosphere
!.    character(LEN=*) :: modele
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    integer :: dateref_actsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    integer :: marsgram_sce
!.    real (KIND=pm_reel),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!.    integer :: type_emcd
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!
!$Arguments
!>[E]   nom_atmosphere  :<LEN=*>               nom du modèle
!>[E]   modele          :<LEN=*>               type de modèle (MSP_MODATM_NUL,MSP_MODATM_CIRA88_MSIS86,MSP_MODATM_US76,
!                                              MSP_MODATM_MARS_RUSSE,MSP_MODATM_MARS_EMCD) [par défaut MSP_MODATM_NUL]
!>[E]   act_sol         :<MSP_ACTSOL>          structure activité solaire
!>[E]   us76            :<MSP_ATM_US76>        structure atmosphère us76
!>[E]   exp             :<MSP_ATM_EXP>         structure atmosphère exponentielle
!>[E]   emcd            :<MSP_ATM_EMCD>        structure atmosphère emcd
!>[E]   marsrusse       :<MSP_ATM_MARSRUSSE>   structure atmosphère marsrusse
!>[E]   marsgram        :<MSP_ATM_MARSGRAM>    structure atmosphère marsgram
!>[E]   venus           :<MSP_ATM_VENUS>       
!>[E]   vent            :<MSP_VENT>            structure vent
!>[E]   actsol          :<integer>             activité solaire (MSP_ENUM_ACTSOL_REELLE, MSP_ENUM_ACTSOL_STD ou MSP_ENUM_ACTSOL_COMPAS) [par défaut MSP_ENUM_ACTSOL_STD]
!>[E]   ficactsol       :<LEN=*>               nom du fichier d'activité solaire
!>[E]   dateref_actsol  :<integer>             date de référence données réelles/données prédites de COMPAS
!>[E]   flux            :<pm_reel>             flux standard [par défaut 75.]
!>[E]   ap              :<pm_reel>             indice geomagnetique (Ap) (par défaut 12.]
!>[E]   ap7             :<pm_reel,DIM=(7)>     
!>[E]   us76_deltat     :<pm_reel,DIM=(8)>     tableau d'écarts en température pour le modèle US76 [K] [par défaut 0.]
!>[E]   mars_russe_mod  :<integer>             type de modèle pour le modèle d'atmosphère martien Russe [par défaut 1]
!.                                              => 1 : modele nominal
!.                                              => 2 : modele maximal
!.                                              => 3 : modele minimal
!>[E]   mars_emcd_sce   :<integer>             type de scénario pour le modèle d'atmosphère martien européen (1=poussiereux, 2=clair) [par défaut 2]
!>[E]   mars_emcd_per   :<pm_reel,DIM=(2)>     perturbations pour le modèle d'atmosphère martien européen [par défaut 1,0]
!>                                             aucune : mars_emcd_per(1)=1. mars_emcd_per(2)=0.
!>                                             n sigma: mars_emcd_per(1)=5. mars_emcd_per(2)=n
!>[E]   marsgram_sce    :<integer>             
!>[E]   marsgram_per    :<PM_REEL,DIM=(2)>     perturbations pour le modèle d'atmosphère martien gram
!>                                                      aucune : marsgram_per(1)=1. marsgram_per(2)=0.
!>                                                      dispersees: marsgram_per(1)=2. marsgram_per(2)=graine ( reel entre 0 et 30000)
!>[E]   vent_nord       :<PM_REEL>             |
!>[E]   vent_est        :<PM_REEL>             Vent constant
!>[E]   vent_vert       :<PM_REEL>             |
!>[E]   vent_dim1_nord  :<PM_REEL,DIM=(:)>     |
!>[E]   vent_dim1_est   :<PM_REEL,DIM=(:)>     Vent tabule a 1 dimension
!>[E]   vent_dim1_vert  :<PM_REEL,DIM=(:)>     |
!>[E]   vent_dim2_nord  :<PM_REEL,DIM=(:,:)>   |
!>[E]   vent_dim2_est   :<PM_REEL,DIM=(:,:)>   Vent tabule a 2 dimensions
!>[E]   vent_dim2_vert  :<PM_REEL,DIM=(:,:)>   |
!>[E]   par1_vent       :<PM_REEL,DIM=(:)>     grille du parametre 1 du vent
!>[E]   par2_vent       :<PM_REEL,DIM=(:)>     grille du parametre 2 du vent
!>[E]   type_vent       :<integer>             type du vent (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2,
!.                                      MSP_ENUM_VENT_MODATM_CONST ,
!.                                      MSP_ENUM_VENT_MODATM_TAB_1 ,
!.                                      MSP_ENUM_VENT_MODATM_TAB_2  
!>[E]   type_emcd       :<integer>             
!>[E]   venus_rho0      :<PM_REEL>             |         
!>[E]   venus_beta      :<PM_REEL>             Modele Venus Petropoulos88  rho(z) = rho0 * exp(- beta * z )
!>S     atmosphere      :<MSP_ATMOSPHERE>      type dérivé contenant les informations du modèle d'atmosphère
!
!$Common
!
!$Routines
!- MSP_effacer_atmosphere
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!#
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_MECASPA_DEF

      implicit none

      type(MSP_ATMOSPHERE) :: atmosphere

      character(LEN=*),intent(IN), optional    :: nom_atmosphere
      character(LEN=*),intent(IN), optional    :: modele

      integer,intent(IN), optional :: actsol
      character(LEN=*),intent(IN), optional    :: ficactsol
      integer,intent(IN), optional :: dateref_actsol
      real (KIND=pm_reel),intent(IN), optional :: flux
      real (KIND=pm_reel),intent(IN), optional :: ap
      real (KIND=pm_reel),dimension(7),intent(IN), optional :: ap7

      real (KIND=pm_reel),intent(IN), optional :: us76_deltat(8)

      integer,intent(IN), optional             :: mars_russe_mod
      integer,intent(IN), optional             :: mars_emcd_sce
      integer,intent(IN), optional             :: marsgram_sce
      real (KIND=pm_reel),intent(IN), optional,dimension(2) :: mars_emcd_per
      real(kind=PM_REEL),intent(IN),optional,dimension(2)   :: marsgram_per
      real(kind=PM_REEL),intent(IN),optional   :: venus_rho0, venus_beta

      real(kind=PM_REEL),intent(IN),optional ::vent_nord,vent_est,vent_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:) ::vent_dim1_nord,vent_dim1_est,vent_dim1_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:,:) ::vent_dim2_nord,vent_dim2_est,vent_dim2_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:) ::par1_vent,par2_vent
      integer,intent(IN),optional ::type_vent
      integer,intent(IN),optional ::type_emcd


      type(MSP_ACTSOL),intent(IN), optional    :: act_sol
      type(MSP_ATM_US76),intent(IN), optional  :: us76
      type(MSP_ATM_EXP),intent(IN),optional    :: EXP
      type(MSP_ATM_EMCD),intent(IN),optional   :: emcd
      type(MSP_ATM_MARSRUSSE),intent(IN),optional::marsrusse
      type(MSP_ATM_MARSGRAM),intent(IN),optional::marsgram
      type(MSP_ATM_VENUS),intent(IN),optional::venus
      type(MSP_VENT),intent(IN),optional :: vent

      

      integer::ndim1_grille,ndim2_grille
      integer::ndim1_v_nord,ndim1_v_est,ndim1_v_vert
      integer::ndim2_v_nord,ndim2_v_est,ndim2_v_vert



      ! Variables locales

      integer :: lnom
      character(LEN=80),dimension(2)       :: tmessage_var


      ! -- Initialisation de la structure
      call MSP_effacer_atmosphere(atmosphere, nul=.true.)
      atmosphere%flag_func = .true.

      ! -- Nom de l'atmosphere
      if ( present(nom_atmosphere) ) then
         lnom = LEN_TRIM(nom_atmosphere)
         if ( lnom <= MSP_LONG_CHAINE ) then 
            atmosphere%nom_atmosphere  = trim(nom_atmosphere)
         else
            atmosphere%nom_atmosphere  = trim(nom_atmosphere(1:MSP_LONG_CHAINE))
            tmessage_var(1) = 'Le nom du modèle d''atmosphère'
            write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE
!            write(*,'(I8)')   MSP_LONG_CHAINE
            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_atmosphere",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         atmosphere%nom_atmosphere = ""
      endif
      
      ! -- Modele d'atmosphere (par défaut MSP_MODATM_NUL)
      if ( present(modele) ) then
         atmosphere%modele = trim(modele)
	 select case (atmosphere%modele)
	 ! Affectation de l'identifiant de modele
	 case (MSP_MODATM_CIRA88_MSIS86)
	    atmosphere%id_modele = MSP_IDATM_CIRA88_MSIS86
	 case (MSP_MODATM_US76)
	    atmosphere%id_modele = MSP_IDATM_US76
	 case (MSP_MODATM_DTM)
	    atmosphere%id_modele = MSP_IDATM_DTM
	 case (MSP_MODATM_AT77)
	    atmosphere%id_modele = MSP_IDATM_AT77
	 case (MSP_MODATM_MARS_RUSSE)
	    atmosphere%id_modele = MSP_IDATM_MARS_RUSSE	    
	 case (MSP_MODATM_MARS_EMCD)
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD
	 case (MSP_MODATM_MARS_EMCD_23A)
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_23A
	 case (MSP_MODATM_MARS_EMCD_31)
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_31
	 case (MSP_MODATM_MARS_EMCD_42)
            atmosphere%id_modele = MSP_IDATM_MARS_EMCD_42
	 case (MSP_MODATM_MARS_EMCD_43) 
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_43
	 case (MSP_MODATM_MARSGRAM)  
	    atmosphere%id_modele = MSP_IDATM_MARSGRAM
	 case (MSP_MODATM_MARSGRAM_2001)
	    atmosphere%id_modele = MSP_IDATM_MARSGRAM_2001
	 case(MSP_MODATM_EXP) 
	    atmosphere%id_modele = MSP_IDATM_EXP
	 case (MSP_MODATM_MSIS_90)
	    atmosphere%id_modele = MSP_IDATM_MSIS_90
	 case (MSP_MODATM_NRL_MSISE_2000)
	    atmosphere%id_modele = MSP_IDATM_NRL_MSISE_2000
	 case (MSP_MODATM_MET_88)
	    atmosphere%id_modele = MSP_IDATM_MET_88
	 case (MSP_MODATM_VENUS)
	    atmosphere%id_modele = MSP_IDATM_VENUS
	 end select
      else
         atmosphere%modele = MSP_MODATM_NUL
	 atmosphere%id_modele = 0
      endif
      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA  SIMBAD              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! -- type d emcd
      if (present(type_emcd)) then
         atmosphere%emcd%type_emcd=type_emcd
      endif


      ! -- Type d'activité solaire (par défaut MSP_ENUM_ACTSOL_STD)
      if ( present(actsol) ) then
         atmosphere%actsol%type_actsol = actsol
      else
         atmosphere%actsol%type_actsol = MSP_ENUM_ACTSOL_STD
      endif
      if (atmosphere%actsol%type_actsol == MSP_ENUM_ACTSOL_REELLE) then
         ! -- Fichier d'activité solaire
         if ( present(ficactsol) ) then
            lnom = LEN_TRIM(ficactsol)
            if ( lnom <= MSP_LONG_NOMFIC ) then 
               atmosphere%actsol%ficactsol  = trim(ficactsol)
            else
               atmosphere%actsol%ficactsol  = trim(ficactsol(1:MSP_LONG_NOMFIC))
               tmessage_var(1) = 'Le nom du fichier d''activite solaire'
               write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC
!               write(*,'(I8)')   MSP_LONG_NOMFIC
               call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
                    routine="MSP_creer_atmosphere",type=MSP_ENUM_WARNING, &
                    partie_variable=tmessage_var)
            endif
         else
            ! -- Le nom du fichier d'activité solaire est absent
            atmosphere%actsol%ficactsol = ""
            call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_002")
         endif
      endif
      
      if (atmosphere%actsol%type_actsol == MSP_ENUM_ACTSOL_COMPAS) then
         ! Date de référence données observées/données prédites de COMPAS
	 if ( present(dateref_actsol) ) then
	    atmosphere%actsol%date_ref = dateref_actsol  
	 else
	    atmosphere%actsol%date_ref = -1
	 endif
      endif
      
      ! -- Flux solaire (par défaut 75.0)
      if ( present(flux) ) then
         atmosphere%actsol%flux = flux
      else
         atmosphere%actsol%flux = MSP_ENUM_VAL_FLUX
      endif
      
      ! -- Indice géomagnétique (par défaut 12.0)
      if ( present(ap7) ) then
         atmosphere%actsol%app = ap7
      elseif (present(ap)) then
         atmosphere%actsol%app(:) = ap
      else
         atmosphere%actsol%app = MSP_ENUM_VAL_AP
      endif
      
      ! --  Tableau d'écarts en température pour le modèle US76 [K] (par défaut 0.)
      if ( present(us76_deltat) ) then
         atmosphere%us76%deltat(:) = us76_deltat(:)
      else
         atmosphere%us76%deltat(:) = 0._pm_reel
      endif

      
      ! -- Type de modèle pour le modèle d'atmosphère martien Russe (par défaut 1)
      if ( present(mars_russe_mod) ) then
         atmosphere%marsrusse%mars_russe_mod = mars_russe_mod
      else
         atmosphere%marsrusse%mars_russe_mod = 1
      endif
      
      ! -- Type de scénario pour le modèle d'atmosphère martien européen (1=poussiereux, 2=clair) 
      !    (par défaut 2)
      if ( present(mars_emcd_sce) ) then
         atmosphere%emcd%mars_emcd_sce = mars_emcd_sce
      else
         atmosphere%emcd%mars_emcd_sce = 2
      endif
      if ( present(marsgram_sce) ) then
         atmosphere%marsgram%marsgram_sce = marsgram_sce
      else
         atmosphere%marsgram%marsgram_sce = 2
      endif
      
      
      ! -- Perturbations pour le modèle d'atmosphère martien européen 
      !    (par defaut le modele est initialise a aucune perturbations)
      if ( present(mars_emcd_per) ) then
         atmosphere%emcd%mars_emcd_per(:) = mars_emcd_per(:)
      else
      ! par defaut le modele est initialise a aucune perturbations
         atmosphere%emcd%mars_emcd_per(:) = (/ 1._pm_reel , 0._pm_reel /)
      endif
      
      ! -- Perturbations pour le modèle d'atmosphère martien gram
      !   (par défaut aucune)
      if ( present(marsgram_per)) then
         atmosphere%marsgram%marsgram_per = nint(marsgram_per)
      else
         atmosphere%marsgram%marsgram_per(1) = 1
         atmosphere%marsgram%marsgram_per(2) = 0 
      end if
      
      ! -- Modele exponentiel ro=ro0*exp((h-h0/hscale)
      ! Les données sont à renseigner dans la structure ATM_EXP
      
      ! -- Modele venus PETROPOULOS88 rho(z) = rho0 * exp(- beta * z )
      ! -- rho0
      if ( present(venus_rho0) ) then
         atmosphere%venus%rho0 = venus_rho0
      else
         atmosphere%venus%rho0 = MSP_VENUS_CSTE_RHO0
      endif
      ! -- beta
      if ( present(venus_beta) ) then
         atmosphere%venus%beta = venus_beta
      else
         atmosphere%venus%beta = MSP_VENUS_CSTE_BETA
      endif
      
      ! -- Cas des vents
      
      ! Lecture du type de vent
      if ( present(type_vent)) then
         
         select case (type_vent) 
            
            ! -- Cas du vent constant
         case (MSP_ENUM_VENT_CONST, MSP_ENUM_VENT_MODATM_CONST,MSP_ENUM_VENT_MODATM ) 
            
          atmosphere%vent%type_vent = type_vent
          
          if ( present(vent_nord) ) then 
             atmosphere%vent%vent_nord = vent_nord 
          else 
             atmosphere%vent%vent_nord = 0._PM_REEL 
          end if
          
          if ( present(vent_est) ) then 
             atmosphere%vent%vent_est = vent_est 
          else 
             atmosphere%vent%vent_est = 0._PM_REEL 
          end if
          
          if ( present(vent_vert) ) then 
             atmosphere%vent%vent_vert = vent_vert 
          else 
             atmosphere%vent%vent_vert = 0._PM_REEL 
          end if
          
          ! -- Cas du vent à 1 dimension
       case (MSP_ENUM_VENT_TAB_1, MSP_ENUM_VENT_MODATM_TAB_1) 
          
          atmosphere%vent%type_vent = type_vent
          
            ! Test sur la presence de la grille dim1_vent 
          if ( present(par1_vent) ) then 
             
             ! Allocation dynamique de la grille du modele d'atmosphere 
             ndim1_grille = size(par1_vent) 

             allocate(atmosphere%vent%par1_vent(ndim1_grille)) 
             atmosphere%vent%par1_vent(:) = par1_vent(:) 
             
          else 
             
             call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_003") 
             return 
             
          end if
          
          ! -- Vent Nord à une dimension
          if ( present(vent_dim1_nord) ) then 
             
             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_nord = size(vent_dim1_nord) 
             
             if ( ndim1_v_nord == ndim1_grille ) then 
                allocate(atmosphere%vent%vent_dim1_nord(ndim1_v_nord)) 
                atmosphere%vent%vent_dim1_nord(:) = vent_dim1_nord(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004") 
                return 
             end if
             
          else 

             ! Initialisation a 0 de la valeur du vent Nord
             allocate(atmosphere%vent%vent_dim1_nord(ndim1_grille)) 
             atmosphere%vent%vent_dim1_nord(:) = 0._PM_REEL 
             
          end if
          
          ! -- Vent Est à une dimension
          if ( present(vent_dim1_est) ) then 
             
             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_est = size(vent_dim1_est) 
             
             if ( ndim1_v_est == ndim1_grille ) then 
                allocate(atmosphere%vent%vent_dim1_est(ndim1_v_est)) 
                atmosphere%vent%vent_dim1_est(:) = vent_dim1_est(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004") 
                return 
             end if
             
          else 
             
             ! Initialisation a 0 de la valeur du vent Est
             allocate(atmosphere%vent%vent_dim1_est(ndim1_grille)) 
             atmosphere%vent%vent_dim1_est(:) = 0._PM_REEL 
             
          end if
          
          ! -- Vent Vertical à une dimension
          if ( present(vent_dim1_vert) ) then 
             
             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_vert = size(vent_dim1_vert) 
             
             if ( ndim1_v_vert == ndim1_grille ) then 
                allocate(atmosphere%vent%vent_dim1_vert(ndim1_v_vert)) 
                atmosphere%vent%vent_dim1_vert(:) = vent_dim1_vert(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004") 
                return 
             end if
             
          else 
             
             ! Initialisation a 0 de la valeur du vent Vertical 
             allocate(atmosphere%vent%vent_dim1_vert(ndim1_grille)) 
             atmosphere%vent%vent_dim1_vert(:) = 0._PM_REEL 
             
          end if
          
          ! -- Cas du vent à 2 dimensions
       case (MSP_ENUM_VENT_TAB_2, MSP_ENUM_VENT_MODATM_TAB_2) 
          
          atmosphere%vent%type_vent = type_vent
          
          ! Test sur la presence de la grille dim1_vent
            if ( present(par1_vent).and.present(par2_vent)) then
               
               ! Allocation dynamique de la grille 1 du modele d'atmosphere
               ndim1_grille=size(par1_vent)
               
               allocate(atmosphere%vent%par1_vent(ndim1_grille))
               atmosphere%vent%par1_vent(:) = par1_vent(:)
               
               ! Allocation dynamique de la grille 2 du modele d'atmosphere
               ndim2_grille=size(par2_vent)
               
               allocate(atmosphere%vent%par2_vent(ndim2_grille))
               atmosphere%vent%par2_vent(:) = par2_vent(:)
               
            else
               call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_003")
               return
            end if
            
            ! -- Vent Nord à deux dimensions
            if (present(vent_dim2_nord)) then
               
               ! test sur la coherence avec la grille par1_vent
               ndim1_v_nord=size(vent_dim2_nord,dim=1)
               ndim2_v_nord=size(vent_dim2_nord,dim=2)
               
               if (( ndim1_v_nord == ndim1_grille).and.( ndim2_v_nord == ndim2_grille)) then

                  allocate(atmosphere%vent%vent_dim2_nord(ndim1_v_nord,ndim2_v_nord))
                  atmosphere%vent%vent_dim2_nord(:,:) = vent_dim2_nord(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004")
                  return
               end if
            else
               
               ! Initialisation a 0 de la valeur du vent Nord
               
               allocate(atmosphere%vent%vent_dim2_nord(ndim1_grille,ndim2_grille))
               atmosphere%vent%vent_dim2_nord(:,:) = 0._PM_REEL
               
            end if
            
            ! -- Vent Est à deux dimensions
            if (present(vent_dim2_est)) then

               ! test sur la coherence avec la grille par1_vent
               ndim1_v_est=size(vent_dim2_est,dim=1)
               ndim2_v_est=size(vent_dim2_est,dim=2)

               if (( ndim1_v_est == ndim1_grille).and.( ndim2_v_est == ndim2_grille)) then

                  allocate(atmosphere%vent%vent_dim2_est(ndim1_v_est,ndim2_v_est))
                  atmosphere%vent%vent_dim2_est(:,:) = vent_dim2_est(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004")
                  return
               end if
            else

               ! Initialisation a 0 de la valeur du vent Est
               
               allocate(atmosphere%vent%vent_dim2_est(ndim1_grille,ndim2_grille))
               atmosphere%vent%vent_dim2_est(:,:) = 0._PM_REEL

            end if
                  
            ! -- Vent Vertical à deux dimensions
            if (present(vent_dim2_vert)) then

               ! test sur la coherence avec la grille par1_vent
               ndim1_v_vert=size(vent_dim2_vert,dim=1)
               ndim2_v_vert=size(vent_dim2_vert,dim=2)

               if (( ndim1_v_vert == ndim1_grille).and.( ndim2_v_vert == ndim2_grille)) then

                  allocate(atmosphere%vent%vent_dim2_vert(ndim1_v_vert,ndim2_v_vert))
                  atmosphere%vent%vent_dim2_vert(:,:) = vent_dim2_vert(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_004")
                  return
               end if
            else

               ! Initialisation a 0 de la valeur du vent Vertical
               
               allocate(atmosphere%vent%vent_dim2_vert(ndim1_grille,ndim2_grille))
               atmosphere%vent%vent_dim2_vert(:,:) = 0._PM_REEL

            end if

       ! -- Type de vent inconnu
       case default
          call MSP_signaler_message (cle_mes="MSP_creer_atmosphere_001")
          return
       end select

     else
        ! Initialisation par defaut au modele d'atmosphere
        atmosphere%vent%type_vent=MSP_ENUM_VENT_MODATM
     end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      if ( present(act_sol) ) then
         atmosphere%actsol = act_sol
      endif
      
      
      if (present(us76)) then
         atmosphere%us76=us76
      end if
      
      if (present(exp)) then
         atmosphere%exp = exp
      end if
      
      
      if (present(emcd)) then
         atmosphere%emcd = emcd
      end if
      
      if (present(marsrusse)) then
         atmosphere%marsrusse = marsrusse
      end if
      
      if (present(marsgram)) then
         atmosphere%marsgram=marsgram
      end if

      if (present(venus)) then
         atmosphere%venus=venus
      end if

      ! Cas des vents
      if (present(vent)) then
         atmosphere%vent = vent

      endif
       
    end function MSP_creer_atmosphere


    SUBROUTINE MSP_egaler_atmosphere(atmosa, atmosb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_atmosphere
!
!$Resume
!  Routine permettant d'égaler deux modèles d'atmosphère
!
!$Description
!  Routine permettant d'égaler deux modèles d'atmosphère
!
!$Auteur
!  S. ROSTAN
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_atmosphere(atmosa, atmosb)
!.    type(MSP_ATMOSPHERE) :: atmosb
!.    type(MSP_ATMOSPHERE) :: atmosa
!
!$Arguments
!>E/S   atmosa  :<MSP_ATMOSPHERE>   modèle d'atmosphère à modifier
!>E     atmosb  :<MSP_ATMOSPHERE>   modèle d'atmosphère copié
!
!$Common
!
!$Routines
!- MSP_effacer_atmosphere
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      implicit none
      type(MSP_ATMOSPHERE), intent(IN) :: atmosb
      type(MSP_ATMOSPHERE), intent(INOUT) :: atmosa

      atmosa%flag_func = .false.
      
      call MSP_effacer_atmosphere(atmosa, nul=.true.)

      atmosa%nom_atmosphere = atmosb%nom_atmosphere
      atmosa%modele = atmosb%modele
      atmosa%id_modele = atmosb%id_modele

      atmosa%actsol = atmosb%actsol
      atmosa%us76 = atmosb%us76
      atmosa%exp = atmosb%exp

      atmosa%emcd = atmosb%emcd

      atmosa%marsrusse = atmosb%marsrusse
     
      atmosa%marsgram = atmosb%marsgram

      atmosa%vent = atmosb%vent

      atmosa%venus = atmosb%venus

      if (atmosb%flag_func) call MSP_effacer_atmosphere(atmosb)

    end SUBROUTINE MSP_egaler_atmosphere


   SUBROUTINE MSP_consulter_atmosphere(atmosphere,nom_atmosphere,modele,act_sol,us76,&
        exp,emcd, marsrusse,marsgram,venus,vent,&
                                       actsol,ficactsol,dateref_actsol,flux,ap,ap7,us76_deltat, &
                                       mars_russe_mod,mars_emcd_sce, &
                                       mars_emcd_per,mars_emcd_lambda_gw,&
                                       marsgram_per,&
                                       venus_rho0, venus_beta,&
                                       vent_nord,vent_est,vent_vert,&
                                       vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
                                       vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
                                       par1_vent,par2_vent,type_vent) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_atmosphere
!
!$Resume
!  Routine permettant de consulter les attributs d'un modèle d'atmosphère
!
!$Description
!  Routine permettant de consulter les attributs d'un modèle d'atmosphère
!
!$Auteur
!  S.ROSTAN
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_atmosphere(atmosphere,[nom_atmosphere],[modele],[act_sol],[us76],&
!.            [exp],[emcd], [marsrusse],[marsgram],[venus],[vent],&
!.                                           [actsol],[ficactsol],[dateref_actsol],[flux],[ap],[ap7],[us76_deltat], &
!.                                           [mars_russe_mod],[mars_emcd_sce], &
!.                                           [mars_emcd_per],[mars_emcd_lambda_gw],&
!.                                           [marsgram_per],&
!.                                           [venus_rho0], [venus_beta],&
!.                                           [vent_nord],[vent_est],[vent_vert],&
!.                                           [vent_dim1_nord],[vent_dim1_est],[vent_dim1_vert],&
!.                                           [vent_dim2_nord],[vent_dim2_est],[vent_dim2_vert],&
!.                                           [par1_vent],[par2_vent],[type_vent]) 
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: modele
!.    character(LEN=*) :: nom_atmosphere
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP 
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    integer :: dateref_actsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    real (KIND=PM_REEL),dimension(2) :: mars_emcd_per
!.    real (KIND=PM_REEL) :: mars_emcd_lambda_gw
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),pointer,dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!
!$Arguments
!>E     atmosphere           :<MSP_ATMOSPHERE>              modèle d'atmosphère à consulter
!>[S]   nom_atmosphere       :<LEN=*>                       nom du modèle
!>[S]   modele               :<LEN=*>                       type de modèle (MSP_MODATM_NUL,MSP_MODATM_CIRA88_MSIS86,MSP_MODATM_US76,
!                                              MSP_MODATM_MARS_RUSSE,MSP_MODATM_MARS_EMCD) [par défaut MSP_MODATM_NUL]
!>[E/S] act_sol              :<MSP_ACTSOL>                  structure activité solaire (utile pour les modèles terretres)
!>[S]   us76                 :<MSP_ATM_US76>                structure modèle US 76
!>[S]   exp                  :<MSP_ATM_EXP>                 structure modèle exponentiel 
!>[S]   emcd                 :<MSP_ATM_EMCD>                structure modèle emcd 
!>[S]   marsrusse            :<MSP_ATM_MARSRUSSE>           structure modèle marsrusse 
!>[S]   marsgram             :<MSP_ATM_MARSGRAM>            structure modèle marsgram
!>[S]   venus                :<MSP_ATM_VENUS>               
!>[E/S] vent                 :<MSP_VENT>                    structure modèle vent
!structure modèle vent
!>[S]   actsol               :<integer>                     activité solaire (MSP_ENUM_ACTSOL_REELLE ou MSP_ENUM_ACTSOL_STD) [par défaut MSP_ENUM_ACTSOL_STD]
!>[S]   ficactsol            :<LEN=*>                       nom du fichier d'activité solaire
!>[S]   dateref_actsol       :<integer>                     date de référence données réelles/données prédites de COMPAS
!>[S]   flux                 :<pm_reel>                     flux standard
!>[S]   ap                   :<pm_reel>                     indice geomagnetique (Ap)
!>[S]   ap7                  :<pm_reel,DIM=(7)>             
!>[S]   us76_deltat          :<pm_reel,DIM=(8)>             tableau d'écarts en température pour le modèle US76 [K]
!>[S]   mars_russe_mod       :<integer>                     type de modèle pour le modèle d'atmosphère martien Russe
!.                                              => 1 : modele nominal
!.                                              => 2 : modele maximal
!.                                              => 3 : modele minimal
!>[S]   mars_emcd_sce        :<integer>                     type de scénario pour le modèle d'atmosphère martien européen (1=poussiereux, 2=clair)
!>[S]   mars_emcd_per        :<PM_REEL,DIM=(2)>             perturbations pour le modèle d'atmosphère martien européen
!>                                             aucune : mars_emcd_per(1)=1. mars_emcd_per(2)=0.
!>                                             n sigma: mars_emcd_per(1)=5. mars_emcd_per(2)=n
!>[S]   mars_emcd_lambda_gw  :<PM_REEL>                     
!>[S]   marsgram_per         :<PM_REEL,DIM=(2)>             perturbations pour le modèle d'atmosphère martien gram
!>                                                      aucune : marsgram_per(1)=1. marsgram_per(2)=0.
!>                                                      dispersees: marsgram_per(1)=2. marsgram_per(2)=graine ( reel entre 0 et 30000)
!>[S]   venus_rho0           :<PM_REEL>                     |
!>[S]   venus_beta           :<PM_REEL>                     Modele venus PETROPOULOS88 rho(z) = rho0 * exp(- beta * z )
!>[S]   vent_nord            :<PM_REEL>                     |
!>[S]   vent_est             :<PM_REEL>                     Vent constant
!>[S]   vent_vert            :<PM_REEL>                     |
!>[E/S] vent_dim1_nord       :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim1_est        :<PM_REEL,DIM=(:),pointer>     Vent tabule a 1 dimension
!>[E/S] vent_dim1_vert       :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim2_nord       :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] vent_dim2_est        :<PM_REEL,DIM=(:,:),pointer>   Vent tabule a 2 dimensions
!>[E/S] vent_dim2_vert       :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] par1_vent            :<PM_REEL,DIM=(:),pointer>     grille du parametre 1 du vent
!>[E/S] par2_vent            :<PM_REEL,DIM=(:),pointer>     grille du parametre 2 du vent
!>[S]   type_vent            :<integer>                     type du vent (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2)
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
!  ATMOSPHERE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      type(MSP_ATMOSPHERE), intent(IN) :: atmosphere

      character(LEN=*),intent(OUT), optional    :: modele
      character(LEN=*),intent(OUT), optional    :: nom_atmosphere

      type(MSP_ACTSOL),intent(INOUT),optional   :: act_sol
      type(MSP_ATM_US76),intent(OUT),optional   ::us76
      type(MSP_ATM_EXP),intent(OUT),optional    ::EXP      
      type(MSP_ATM_EMCD),intent(OUT),optional   ::emcd
      type(MSP_ATM_MARSRUSSE),intent(OUT),optional ::marsrusse
      type(MSP_ATM_MARSGRAM),intent(OUT), optional :: marsgram
      type(MSP_ATM_VENUS),intent(OUT),optional  ::venus

      type(MSP_VENT),intent(INOUT),optional::vent

      integer,intent(OUT), optional :: actsol
      character(LEN=*),intent(OUT), optional    :: ficactsol
      integer,intent(OUT), optional :: dateref_actsol
      real (KIND=pm_reel),intent(OUT), optional :: flux
      real (KIND=pm_reel),intent(OUT), optional :: ap
      real (KIND=pm_reel),dimension(7),intent(OUT), optional :: ap7

      real (KIND=pm_reel),intent(OUT), optional :: us76_deltat(8)

      integer,intent(OUT), optional             :: mars_russe_mod
      integer,intent(OUT), optional             :: mars_emcd_sce
      real (KIND=PM_REEL),intent(OUT), optional,dimension(2) :: mars_emcd_per
      real (KIND=PM_REEL),optional, intent(OUT) ::  mars_emcd_lambda_gw
      real(kind=PM_REEL),intent(OUT),optional,dimension(2)   :: marsgram_per
      real(kind=PM_REEL),intent(OUT),optional   :: venus_rho0, venus_beta

      real(kind=PM_REEL),intent(OUT),optional ::vent_nord,vent_est,vent_vert
      real(kind=PM_REEL),optional,pointer,dimension(:) ::vent_dim1_nord,vent_dim1_est,vent_dim1_vert
      real(kind=PM_REEL),optional,pointer,dimension(:,:) ::vent_dim2_nord,vent_dim2_est,vent_dim2_vert
      real(kind=PM_REEL),optional,pointer,dimension(:) ::par1_vent,par2_vent
      integer,intent(OUT),optional ::type_vent

      
      integer :: MSP_iostat
      
      MSP_iostat = 0

    
      if ( present(modele) )          modele = atmosphere%modele
      if ( present(nom_atmosphere) )  nom_atmosphere = atmosphere%nom_atmosphere


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ( present(act_sol) )         act_sol = atmosphere%actsol
      if ( present(us76) )            us76 = atmosphere%us76
      if ( present(exp) )             exp  = atmosphere%exp
      if ( present(emcd) )            emcd = atmosphere%emcd
      if ( present(marsrusse) )       marsrusse = atmosphere%marsrusse
      if ( present(marsgram) )        marsgram = atmosphere%marsgram
      if ( present(venus) )           venus = atmosphere%venus
      if ( present(vent))             vent = atmosphere%vent


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA  SIMBAD              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if ( present(actsol) )          actsol = atmosphere%actsol%type_actsol
      if ( present(ficactsol) )       ficactsol = atmosphere%actsol%ficactsol
      if ( present(dateref_actsol) )  dateref_actsol = atmosphere%actsol%date_ref
      if ( present(flux) )            flux = atmosphere%actsol%flux
      if ( present(ap) )              ap = atmosphere%actsol%app(2)
      if ( present(ap7) )             ap7 = atmosphere%actsol%app
      if ( present(us76_deltat) )     us76_deltat(:) = atmosphere%us76%deltat(:)
      if ( present(mars_russe_mod) )  mars_russe_mod = atmosphere%marsrusse%mars_russe_mod
      if ( present(mars_emcd_sce) )   mars_emcd_sce = atmosphere%emcd%mars_emcd_sce
      if ( present(mars_emcd_per) )   mars_emcd_per(:) = atmosphere%emcd%mars_emcd_per(:)
      if ( present(mars_emcd_lambda_gw) ) mars_emcd_lambda_gw = atmosphere%emcd%lambda_gw
      if ( present(marsgram_per))     marsgram_per = atmosphere%marsgram%marsgram_per

      if ( present(venus_rho0))       venus_rho0 = atmosphere%venus%rho0
      if ( present(venus_beta))       venus_beta = atmosphere%venus%beta

      if ( present(type_vent))        type_vent = atmosphere%vent%type_vent
      if ( present(vent_nord))        vent_nord = atmosphere%vent%vent_nord
      if ( present(vent_est))         vent_est = atmosphere%vent%vent_est
      if ( present(vent_vert))        vent_vert = atmosphere%vent%vent_vert
      if ( present(type_vent) )       type_vent = atmosphere%vent%type_vent

      if ( present(vent_dim1_nord)) then 
         if (ASSOCIATED(vent_dim1_nord)) DEALLOCATE(vent_dim1_nord,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim1_nord)) then
            ALLOCATE(vent_dim1_nord(size(atmosphere%vent%vent_dim1_nord)))
            vent_dim1_nord(:) = atmosphere%vent%vent_dim1_nord(:)
         end if
      end if

      if ( present(vent_dim1_est)) then 
         if (ASSOCIATED(vent_dim1_est)) DEALLOCATE(vent_dim1_est,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim1_est)) then
            ALLOCATE(vent_dim1_est(size(atmosphere%vent%vent_dim1_est)))
            vent_dim1_est(:) = atmosphere%vent%vent_dim1_est(:)
         end if
      end if

      if ( present(vent_dim1_vert)) then
         if (ASSOCIATED(vent_dim1_vert)) DEALLOCATE(vent_dim1_vert,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim1_vert)) then
            ALLOCATE(vent_dim1_vert(size(atmosphere%vent%vent_dim1_vert)))
            vent_dim1_vert(:) = atmosphere%vent%vent_dim1_vert(:)
         end if
      end if

      if ( present(vent_dim2_nord)) then 
         if (ASSOCIATED(vent_dim2_nord)) DEALLOCATE(vent_dim2_nord,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim2_nord)) then
            ALLOCATE(vent_dim2_nord(size(atmosphere%vent%vent_dim2_nord, DIM=1), size(atmosphere%vent%vent_dim2_nord, DIM=2)))
            vent_dim2_nord(:, :) = atmosphere%vent%vent_dim2_nord(:, :)
         end if
      end if
      if ( present(vent_dim2_est)) then 
         if (ASSOCIATED(vent_dim2_est)) DEALLOCATE(vent_dim2_est,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim2_est)) then
            ALLOCATE(vent_dim2_est(size(atmosphere%vent%vent_dim2_est, DIM=1), size(atmosphere%vent%vent_dim2_est, DIM=2)))
            vent_dim2_est(:, :) = atmosphere%vent%vent_dim2_est(:, :)
         end if
      end if
      if ( present(vent_dim2_vert)) then 
         if (ASSOCIATED(vent_dim2_vert)) DEALLOCATE(vent_dim2_vert,stat=MSP_iostat)
         if (associated(atmosphere%vent%vent_dim2_vert)) then
            ALLOCATE(vent_dim2_vert(size(atmosphere%vent%vent_dim2_vert, DIM=1), size(atmosphere%vent%vent_dim2_vert, DIM=2)))
            vent_dim2_vert(:, :) = atmosphere%vent%vent_dim2_vert(:, :)
         end if
      end if

      if ( present(par1_vent)) then 
         if (ASSOCIATED(par1_vent)) DEALLOCATE(par1_vent,stat=MSP_iostat)
         if (associated(atmosphere%vent%par1_vent)) then
            ALLOCATE(par1_vent(size(atmosphere%vent%par1_vent)))
            par1_vent(:) = atmosphere%vent%par1_vent(:)
         end if

      end if
      if ( present(par2_vent)) then 
         if (ASSOCIATED(par2_vent)) DEALLOCATE(par2_vent,stat=MSP_iostat)
         if (associated(atmosphere%vent%par2_vent)) then
            ALLOCATE(par2_vent(size(atmosphere%vent%par2_vent)))
            par2_vent(:) = atmosphere%vent%par2_vent(:)
         end if

      end if



    end SUBROUTINE MSP_consulter_atmosphere



   SUBROUTINE MSP_modifier_atmosphere(atmosphere,nom_atmosphere,modele,act_sol,us76,&
        exp, emcd,marsrusse,marsgram,vent,venus,actsol,ficactsol,flux,ap,ap7,us76_deltat, &
                                  mars_russe_mod,mars_emcd_sce,mars_emcd_per,&
                                  marsgram_per,&
                                  venus_rho0, venus_beta,&
                                  vent_nord,vent_est,vent_vert,&
                                  vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
                                  vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
                                  par1_vent,par2_vent,type_vent) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_atmosphere
!
!$Resume
!  Cette routine permet de modifier les caractéristiques du modèle d'atmosphère
!
!$Description
!  Cette routine permet de modifier les caractéristiques du modèle d'atmosphère
!
!$Auteur
!  J. J. Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_atmosphere(atmosphere,[nom_atmosphere],[modele],[act_sol],[us76],&
!.            [exp], [emcd],[marsrusse],[marsgram],[vent],[venus],[actsol],[ficactsol],[flux],[ap],[ap7],[us76_deltat], &
!.                                      [mars_russe_mod],[mars_emcd_sce],[mars_emcd_per],&
!.                                      [marsgram_per],&
!.                                      [venus_rho0], [venus_beta],&
!.                                      [vent_nord],[vent_est],[vent_vert],&
!.                                      [vent_dim1_nord],[vent_dim1_est],[vent_dim1_vert],&
!.                                      [vent_dim2_nord],[vent_dim2_est],[vent_dim2_vert],&
!.                                      [par1_vent],[par2_vent],[type_vent]) 
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    character(LEN=*) :: modele
!.    character(LEN=*) :: nom_atmosphere
!.    type(MSP_ACTSOL) :: act_sol
!.    type(MSP_ATM_US76) :: us76
!.    type(MSP_ATM_EXP) :: EXP
!.    type(MSP_ATM_EMCD) :: emcd
!.    type(MSP_ATM_MARSRUSSE) :: marsrusse
!.    type(MSP_ATM_MARSGRAM) :: marsgram
!.    type(MSP_ATM_VENUS) :: venus
!.    type(MSP_VENT) :: vent
!.    integer :: actsol
!.    character(LEN=*) :: ficactsol
!.    real (KIND=pm_reel) :: flux
!.    real (KIND=pm_reel) :: ap
!.    real (KIND=pm_reel),dimension(7) :: ap7
!.    real (KIND=pm_reel) :: us76_deltat(8)
!.    integer :: mars_russe_mod
!.    integer :: mars_emcd_sce
!.    real (KIND=pm_reel),dimension(2) :: mars_emcd_per
!.    real(kind=PM_REEL),dimension(2) :: marsgram_per
!.    real(kind=PM_REEL) :: venus_rho0, venus_beta
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!
!$Arguments
!>E/S   atmosphere      :<MSP_ATMOSPHERE>      Structure contenant le modele d'atmosphere à modifier
!>[E]   nom_atmosphere  :<LEN=*>               nom du modèle
!nom du modèle
!>[E]   modele          :<LEN=*>               type de modèle (MSP_MODATM_NUL,MSP_MODATM_CIRA88_MSIS86,MSP_MODATM_US76,
!                                              MSP_MODATM_MARS_RUSSE,MSP_MODATM_MARS_EMCD) [par défaut MSP_MODATM_NUL]
!>[E]   act_sol         :<MSP_ACTSOL>          structure activité solaire (utile pour les modèles terretres)
!>[E]   us76            :<MSP_ATM_US76>        structure modèle US 76
!>[E]   exp             :<MSP_ATM_EXP>         structure modèle exponentiel 
!>[E]   emcd            :<MSP_ATM_EMCD>        structure modèle emcd 
!>[E]   marsrusse       :<MSP_ATM_MARSRUSSE>   structure modèle marsrusse
!>[E]   marsgram        :<MSP_ATM_MARSGRAM>    structure modèle marsgram
!>[E]   vent            :<MSP_VENT>            structure modèle vent
!>[E]   venus           :<MSP_ATM_VENUS>       
!>[E]   actsol          :<integer>             activité solaire (MSP_ENUM_ACTSOL_REELLE ou MSP_ENUM_ACTSOL_STD) [par défaut MSP_ENUM_ACTSOL_STD]
!>[E]   ficactsol       :<LEN=*>               nom du fichier d'activité solaire 
!>[E]   flux            :<pm_reel>             flux standard
!>[E]   ap              :<pm_reel>             indice geomagnetique (Ap)
!>[E]   ap7             :<pm_reel,DIM=(7)>     
!>[E]   us76_deltat     :<pm_reel,DIM=(8)>     tableau d'écarts en température pour le modèle US76 [K] 
!>[E]   mars_russe_mod  :<integer>             type de modèle pour le modèle d'atmosphère martien Russe [par défaut 1]
!.                                              => 1 : modele nominal
!.                                              => 2 : modele maximal
!.                                              => 3 : modele minimal
!>[E]   mars_emcd_sce   :<integer>             type de scénario pour le modèle d'atmosphère martien européen (1=poussiereux, 2=clair)
!>[E]   mars_emcd_per   :<pm_reel,DIM=(2)>     perturbations pour le modèle d'atmosphère martien européen [par défaut 1,0]
!>                                             aucune : mars_emcd_per(1)=1. mars_emcd_per(2)=0.
!>                                             n sigma: mars_emcd_per(1)=5. mars_emcd_per(2)=n
!>[E]   marsgram_per    :<PM_REEL,DIM=(2)>     perturbations pour le modèle d'atmosphère martien gram
!>                                                      aucune : marsgram_per(1)=1. marsgram_per(2)=0.
!>                                                      dispersees: marsgram_per(1)=2. marsgram_per(2)=graine ( reel entre 0 et 30000)
!>[E]   venus_rho0      :<PM_REEL>             
!>[E]   venus_beta      :<PM_REEL>             
!>[E]   vent_nord       :<PM_REEL>             |
!>[E]   vent_est        :<PM_REEL>             Vent constant
!>[E]   vent_vert       :<PM_REEL>             |
!>[E]   vent_dim1_nord  :<PM_REEL,DIM=(:)>     |
!>[E]   vent_dim1_est   :<PM_REEL,DIM=(:)>     Vent tabule a 1 dimension
!>[E]   vent_dim1_vert  :<PM_REEL,DIM=(:)>     |
!>[E]   vent_dim2_nord  :<PM_REEL,DIM=(:,:)>   |
!>[E]   vent_dim2_est   :<PM_REEL,DIM=(:,:)>   Vent tabule a 2 dimensions
!>[E]   vent_dim2_vert  :<PM_REEL,DIM=(:,:)>   |
!>[E]   par1_vent       :<PM_REEL,DIM=(:)>     grille du parametre 1 du vent
!>[E]   par2_vent       :<PM_REEL,DIM=(:)>     grille du parametre 2 du vent
!>[E]   type_vent       :<integer>             type du vent (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2)
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
!  ATMOSPHERE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      type(MSP_ATMOSPHERE), intent(INOUT)      :: atmosphere

      character(LEN=*),intent(IN), optional    :: modele
      character(LEN=*),intent(IN), optional    :: nom_atmosphere

      type(MSP_ACTSOL),intent(IN), optional    :: act_sol
      type(MSP_ATM_US76),intent(IN),optional   :: us76
      type(MSP_ATM_EXP) ,intent(in),optional   ::EXP
      type(MSP_ATM_EMCD),intent(in),optional   ::emcd
      type(MSP_ATM_MARSRUSSE),intent(in),optional ::marsrusse
      type(MSP_ATM_MARSGRAM),intent(IN), optional :: marsgram
      type(MSP_ATM_VENUS),intent(IN), optional :: venus

      type(MSP_VENT),intent(IN),optional::vent


      integer,intent(IN), optional :: actsol
      character(LEN=*),intent(IN), optional    :: ficactsol
      real (KIND=pm_reel),intent(IN), optional :: flux
      real (KIND=pm_reel),intent(IN), optional :: ap
      real (KIND=pm_reel),dimension(7),intent(IN), optional :: ap7

      real (KIND=pm_reel),intent(IN), optional :: us76_deltat(8)

      integer,intent(IN), optional             :: mars_russe_mod
      integer,intent(IN), optional             :: mars_emcd_sce
      real (KIND=pm_reel),intent(IN), optional,dimension(2) :: mars_emcd_per
      real(kind=PM_REEL),intent(IN),optional,dimension(2)   :: marsgram_per
      real(kind=PM_REEL),intent(IN),optional   :: venus_rho0, venus_beta

      real(kind=PM_REEL),intent(IN),optional ::vent_nord,vent_est,vent_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:) ::vent_dim1_nord,vent_dim1_est,vent_dim1_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:,:) ::vent_dim2_nord,vent_dim2_est,vent_dim2_vert
      real(kind=PM_REEL),intent(IN),optional,dimension(:) ::par1_vent,par2_vent
      integer,intent(IN),optional ::type_vent

      
      integer :: MSP_iostat
      
      MSP_iostat = 0

      if ( present(nom_atmosphere) )  atmosphere%nom_atmosphere = nom_atmosphere
      if ( present(modele) ) then
         atmosphere%modele = modele
         ! Modification de l'identifiant de modele
         select case (modele)
	 case (MSP_MODATM_CIRA88_MSIS86)
	    atmosphere%id_modele = MSP_IDATM_CIRA88_MSIS86
	 case (MSP_MODATM_US76)
	    atmosphere%id_modele = MSP_IDATM_US76
	 case (MSP_MODATM_DTM)
	    atmosphere%id_modele = MSP_IDATM_DTM
	 case (MSP_MODATM_AT77)
	    atmosphere%id_modele = MSP_IDATM_AT77
	 case (MSP_MODATM_MARS_RUSSE)
	    atmosphere%id_modele = MSP_IDATM_MARS_RUSSE
	 case (MSP_MODATM_MARS_EMCD)
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD
	 case (MSP_MODATM_MARS_EMCD_23A)
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_23A	 
	 case (MSP_MODATM_MARS_EMCD_31)  
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_31
	 case (MSP_MODATM_MARS_EMCD_42)    
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_42
	 case (MSP_MODATM_MARS_EMCD_43) 
	    atmosphere%id_modele = MSP_IDATM_MARS_EMCD_43	    
	 case (MSP_MODATM_MARSGRAM)  
	    atmosphere%id_modele = MSP_IDATM_MARSGRAM
	  case (MSP_MODATM_MARSGRAM_2001)  
	    atmosphere%id_modele = MSP_IDATM_MARSGRAM_2001  
	 case(MSP_MODATM_EXP) 
	    atmosphere%id_modele = MSP_IDATM_EXP
	 case (MSP_MODATM_MSIS_90)
	    atmosphere%id_modele = MSP_IDATM_MSIS_90
	 case (MSP_MODATM_NRL_MSISE_2000)
	    atmosphere%id_modele = MSP_IDATM_NRL_MSISE_2000
	 case (MSP_MODATM_MET_88)
	    atmosphere%id_modele = MSP_IDATM_MET_88
	 case (MSP_MODATM_VENUS)
	    atmosphere%id_modele = MSP_IDATM_VENUS
	 end select
      end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      if ( present(act_sol) )         atmosphere%actsol = act_sol
      if ( present(us76) )            atmosphere%us76 = us76
      if ( present(exp) )             atmosphere%exp = exp
      if ( present(emcd) )            atmosphere%emcd = emcd     
      if ( present(marsrusse) )       atmosphere%marsrusse = marsrusse
      if ( present(marsgram) )        atmosphere%marsgram = marsgram
      if ( present(venus) )           atmosphere%venus = venus
      if ( present(vent))             atmosphere%vent = vent


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                 Utilisation des structures MECASPA  SIMBAD              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      if ( present(actsol) )          atmosphere%actsol%type_actsol = actsol
      if ( present(ficactsol) )       atmosphere%actsol%ficactsol = ficactsol
      if ( present(flux) )            atmosphere%actsol%flux = flux 
      if ( present(ap) )              atmosphere%actsol%app(:) = ap
      if ( present(ap7) )             atmosphere%actsol%app = ap7
      if ( present(us76_deltat) )     atmosphere%us76%deltat(:) = us76_deltat(:)
      if ( present(mars_russe_mod) )  atmosphere%marsrusse%mars_russe_mod = mars_russe_mod
      if ( present(mars_emcd_sce) )   atmosphere%emcd%mars_emcd_sce = mars_emcd_sce
      if ( present(mars_emcd_per) )   atmosphere%emcd%mars_emcd_per(:) = mars_emcd_per(:)
      if ( present(marsgram_per))     atmosphere%marsgram%marsgram_per = nint(marsgram_per)
      
      if ( present(venus_rho0))       atmosphere%venus%rho0 = venus_rho0
      if ( present(venus_beta))       atmosphere%venus%beta = venus_beta

      if ( present(type_vent))        atmosphere%vent%type_vent = type_vent
      if ( present(vent_nord))        atmosphere%vent%vent_nord = vent_nord
      if ( present(vent_est))         atmosphere%vent%vent_est = vent_est
      if ( present(vent_vert))        atmosphere%vent%vent_vert = vent_vert
      if ( present(type_vent) )       atmosphere%vent%type_vent = type_vent

      if ( present(vent_dim1_nord)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim1_nord)) DEALLOCATE(atmosphere%vent%vent_dim1_nord,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim1_nord(size(vent_dim1_nord)))
         atmosphere%vent%vent_dim1_nord(:) = vent_dim1_nord(:)
      end if
      if ( present(vent_dim1_est)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim1_est)) DEALLOCATE(atmosphere%vent%vent_dim1_est,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim1_est(size(vent_dim1_est)))
         atmosphere%vent%vent_dim1_est(:) = vent_dim1_est(:)
      end if
      if ( present(vent_dim1_vert)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim1_vert)) DEALLOCATE(atmosphere%vent%vent_dim1_vert,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim1_vert(size(vent_dim1_vert)))
         atmosphere%vent%vent_dim1_vert(:) = vent_dim1_vert(:)
      end if

      if ( present(vent_dim2_nord)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim2_nord)) DEALLOCATE(atmosphere%vent%vent_dim2_nord,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim2_nord(size(vent_dim2_nord, DIM=1), size(vent_dim2_nord, DIM=2)))
         atmosphere%vent%vent_dim2_nord(:, :) = vent_dim2_nord(:, :)
      end if
      if ( present(vent_dim2_est)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim2_est)) DEALLOCATE(atmosphere%vent%vent_dim2_est,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim2_est(size(vent_dim2_est, DIM=1), size(vent_dim2_est, DIM=2)))
         atmosphere%vent%vent_dim2_est(:, :) = vent_dim2_est(:, :)
      end if
      if ( present(vent_dim2_vert)) then 
         if (ASSOCIATED(atmosphere%vent%vent_dim2_vert)) DEALLOCATE(atmosphere%vent%vent_dim2_vert,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%vent_dim2_vert(size(vent_dim2_vert, DIM=1), size(vent_dim2_vert, DIM=2)))
         atmosphere%vent%vent_dim2_vert(:, :) = vent_dim2_vert(:, :)
      end if

      if ( present(par1_vent)) then 
         if (ASSOCIATED(atmosphere%vent%par1_vent)) DEALLOCATE(atmosphere%vent%par1_vent,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%par1_vent(size(par1_vent)))
         atmosphere%vent%par1_vent(:) = par1_vent(:)
      end if
      if ( present(par2_vent)) then 
         if (ASSOCIATED(atmosphere%vent%par2_vent)) DEALLOCATE(atmosphere%vent%par2_vent,stat=MSP_iostat)
         ALLOCATE(atmosphere%vent%par2_vent(size(par2_vent)))
         atmosphere%vent%par2_vent(:) = par2_vent(:)
      end if



    end SUBROUTINE MSP_modifier_atmosphere


    subroutine MSP_afficher_atmosphere (atmosphere,ilog)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_atmosphere
!
!$Resume
!  Affichage  du contenu de la structure atmosphere
!
!$Description
!  Affichage  du contenu de la structure atmosphere
!
!$Auteur
!  Jean-François Goester
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_atmosphere (atmosphere,[ilog])
!.    type(MSP_ATMOSPHERE) :: atmosphere
!.    integer :: ilog
!
!$Arguments
!>E     atmosphere  :<MSP_ATMOSPHERE>   Structure atmosphere à afficher
!>[E/S] ilog        :<integer>          unité logique d'affichage
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
! ATMOSPHERE AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_ATMOSPHERE), intent(IN) :: atmosphere
      integer, optional :: ilog

      integer :: num

      if ( present(ilog) ) then
         num = ilog
      else
         num = MSP_ENUM_ECRAN
      endif
      write(num,'(a,a)') "NOM DE L'ATMOSPHERE: ",TRIM(atmosphere%nom_atmosphere)
      write(num,'(a,a)') "MODELE D'ATMOSPHERE: ",TRIM(atmosphere%modele)
      write(num,'(a)')''


      if ( atmosphere%actsol%type_actsol == MSP_ENUM_ACTSOL_REELLE ) then
         write(num,'(a,a)') "Fichier d'activité solaire:    ",atmosphere%actsol%ficactsol
      endif
      if ( atmosphere%actsol%type_actsol == MSP_ENUM_ACTSOL_COMPAS ) then
         write(num,'(a,i9)') "Date de référence:    ",atmosphere%actsol%date_ref
      endif
      if ( atmosphere%modele == MSP_MODATM_US76 ) then
         write(num,'(a,8(g21.12))') "DELTAT:  ",atmosphere%us76%deltat(1:8)
      else if (atmosphere%modele  == MSP_MODATM_CIRA88_MSIS86 ) then
         write(num,'(a,i9)') "ACTSOL:    ",atmosphere%actsol%type_actsol
         if ( atmosphere%actsol%type_actsol == MSP_ENUM_ACTSOL_STD ) then
            write(num,'(a,g21.12)') "FLUX:    ",atmosphere%actsol%flux
            write(num,'(a,7(g21.12))') "AP:    ",atmosphere%actsol%app
         endif
      else if ( atmosphere%modele == MSP_MODATM_EXP ) then
         write(num,'(a,g21.12)') "RO0:     ",atmosphere%exp%ro0
         write(num,'(a,g21.12)') "H0:      ",atmosphere%exp%h0
         write(num,'(a,g21.12)') "HSCALE:  ",atmosphere%exp%hscale
	     write(num,'(a,g21.12)') "BETA:    ",atmosphere%exp%beta
	     write(num,'(a,i9)')     "TSCALE:  ",atmosphere%exp%tscale
      else if ( atmosphere%modele == MSP_MODATM_MARS_RUSSE ) then
         select case (atmosphere%marsrusse%mars_russe_mod)
         case (1)
            write(num,'(a)') "MODELE NOMINAL"
         case (2)
            write(num,'(a)') "MODELE MAXIMAL"
         case (3)
            write(num,'(a)') "MODELE MINIMAL"
         end select
      else if ( atmosphere%modele == MSP_MODATM_MARS_EMCD ) then
         select case (atmosphere%emcd%mars_emcd_sce)
         case (1)
            write(num,'(a)') "MODELE POUSSIEREUX"
         case (2)
            write(num,'(a)') "MODELE CLAIR"
         end select
         if ( atmosphere%emcd%mars_emcd_per(1) == 1._pm_reel ) then
            write(num,'(a)') "Pas de perturbations"
         else
            write(num,'(a,g21.12,a)') "Perturbations à ",atmosphere%emcd%mars_emcd_per(2)," sigma"
         endif
      else if ( atmosphere%modele == MSP_MODATM_MARSGRAM ) then
         if ( atmosphere%marsgram%marsgram_per(1) == 1._pm_reel ) then
            write(num,'(a)') "Pas de perturbations"
         else
            write(num,'(a,i9)') "Perturbations avec une graine de ",atmosphere%marsgram%marsgram_per(2)
         endif
      else if ( atmosphere%modele == MSP_MODATM_VENUS ) then
         write(num,'(a)') "MODELE VENUS PETROPOULOS88"
         write(num,'(a,g21.12)') "RO0:     ",atmosphere%venus%rho0
         write(num,'(a,g21.12)') "BETA:    ",atmosphere%venus%beta
      endif
      write(num,'(a)') ''

      select case (atmosphere%vent%type_vent)
      case (MSP_ENUM_SANS_VENT)
      case (MSP_ENUM_VENT_MODATM)
      case (MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_MODATM_CONST )
         write(num,'(g21.12)') atmosphere%vent%vent_nord
         write(num,'(g21.12)') atmosphere%vent%vent_est
         write(num,'(g21.12)') atmosphere%vent%vent_vert
      case (MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_MODATM_TAB_1 )
         write(num,'(4(g21.12))') atmosphere%vent%par1_vent(:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim1_nord(:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim1_est(:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim1_vert(:)
      case (MSP_ENUM_VENT_TAB_2,MSP_ENUM_VENT_MODATM_TAB_2 )
         write(num,'(4(g21.12))') atmosphere%vent%par1_vent(:)
         write(num,'(4(g21.12))') atmosphere%vent%par2_vent(:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim2_nord(:,:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim2_est(:,:)
         write(num,'(4(g21.12))') atmosphere%vent%vent_dim2_vert(:,:)
      end select
      write(num,'(a)')


    end subroutine MSP_afficher_atmosphere


      subroutine MSP_vgram (z,xlat,xlon,imois,pi,pis2,wind,aziw,ier)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_vgram
!
!$Resume
!     Calcul de la vitesse du vent et de son azimut d après le modèle GRAM tabulé
!
!$Description
!     Calcul de la vitesse du vent et de son azimut d après le modèle GRAM tabulé
!
!$Auteur
!     J.F. GOESTER,R.CLEDASSOU
!
!$Version
!     23/05/89
!
!$Usage
!  call MSP_vgram (z,xlat,xlon,imois,pi,pis2,wind,aziw,ier)
!.    real(KIND=PM_REEL) :: z,xlat,xlon,pi,pis2
!.    integer :: imois
!.    integer :: ier
!.    real(KIND=PM_REEL) :: wind,aziw
!
!$Remarques
!     Ancien nom: vgram
!
!$Arguments
!>E     z      :<PM_REEL>   altitude géodésique [m]
!>E     xlat   :<PM_REEL>   latitude [rad]
!>E     xlon   :<PM_REEL>   longitude [rad]
!>E     imois  :<integer>   numéro du mois
!>E     pi     :<PM_REEL>   nombre pi
!>E     pis2   :<PM_REEL>   nombre pi/2
!>S     wind   :<PM_REEL>   vitesse du vent [m/s]
!>S     aziw   :<PM_REEL>   azimut du vent [rad]
!>S     ier    :<integer>   test d erreur (OK si 0)
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
        implicit none
!
        real(KIND=PM_REEL), intent(in)   ::  z,xlat,xlon,pi,pis2
        integer, intent(in)  ::  imois

        integer, intent(out) :: ier
        real(KIND=PM_REEL), intent(out) ::   wind,aziw


        real(KIND=PM_REEL) ::  a1(9),b1(9),a2(9),b2(9),z1,z2,rlon,rcolat,u1,v1,u2,v2,u,v
        integer :: igramu(12,14,9),igramv(12,14,9),jmois,i,j,k
!
!     {coefficients harmoniques spheriques pour le calcul des vents du
!      modele GRAM entre 25 et 90 kilometres}
!
      data ((igramu(1,j,k),k=1,9),j=1,14) /  &
        -711,  840,  245,  675, 1188,  -20,  245,   86,   62, &
        -881, 1246,  272,  770, 1010,   87,  347,  140,    3, &
       -1051, 1566,  160, 1006, 1014,  351,  393,   46, -221, &
       -1302, 1834,   85, 1243, 1499,  416,  288,  -12, -475, &
       -1367, 2771,  -88, 1384, 2240,  768, -131, -166, -642, &
       -1242, 3707, -117, 1218, 2056,  850, -298, -202, -783, &
        -591, 3511, -638, 2072, -255, 1210, -447,  -98, -920, &
         221, 3594, -379, 2791,-2082, 1333, -683,  -44,-1075, &
       -1197, 4623,  -66, 3334,-4594, -355, 2211,  574, -442, &
         559, 3331, -237, 2488,    0,    0,    0,    0,    0, &
         641, 2705,   16, 2047,    0,    0,    0,    0,    0, &
         607, 2290,   29,  507,    0,    0,    0,    0,    0, &
        1206,  933, -595,  139,    0,    0,    0,    0,    0, &
         986, -566,-3359, -250,    0,    0,    0,    0,    0/
      data ((igramu(2,j,k),k=1,9),j=1,14) / &
        -576,  761,    0,  619, 1379, -144,  159,   23,   72, &
        -625, 1125,  215,  724, 1673, -177,  296,   10,   33, &
        -896, 1332,  139, 1073, 1976,  159,  510,   29, -218, &
       -1096, 1658,  201, 1197, 2355,  375,  626,   -4, -451, &
        -844, 2425,  -38, 1476, 2520,  739,  310, -118, -581, &
        -250, 2920, -136, 1515, 1211,  270,  433,  -21, -375, &
          61, 2993,  155, 1962,-1220,  -63,  884,  243, -370, &
         501, 2825,  346, 2292,-2378,  504,  903,  320, -384, &
        -761, 2642,  197, 2929,-4014, -181, 3096,  556, -313, &
        1532, 3063, -587, 1289,    0,    0,    0,    0,    0, &
         690, 2086, -273,  816,    0,    0,    0,    0,    0, &
          37, 1593,-1908,  385,    0,    0,    0,    0,    0, & 
         -16, 1431,-1589, -888,    0,    0,    0,    0,    0, &
         630,  -60,-3774,-1464,    0,    0,    0,    0,    0/
      data ((igramu(3,j,k),k=1,9),j=1,14) /  &
        -327,  312,   23,  274, 1498,  194, -167,  -32,   28, &
        -522,  274,  415,  676, 1906,  320,  285,  114,  -69, &
        -580,  211,  535,  887, 2146,  459,  612,   15, -215, &
        -256,  173,  108, 1070, 2035,  538,  472,  -83, -433, &
         420,  375, -291, 1092, 1076,  232,  286, -135, -210, &
        1029,  852, -115,  964,  515,  500,  208,  -82, -217, &
        1670,  865, -401,  952, -112,  260,   80,   39,  -82, &
        1550,  544,  218, 1108, -693, -154,  682,  240, -141, &
          96,  271,  769, 1527, -865, -490, 2872,  852,  -46, &
        1418, 2048,  -33, -300,    0,    0,    0,    0,    0, &
         269,  467,  276, -540,    0,    0,    0,    0,    0, &
         -71, 1299,  474, -409,    0,    0,    0,    0,    0, &
         209,  467,  276, -540,    0,    0,    0,    0,    0, &
        1088, -155,-4063, -761,    0,    0,    0,    0,    0/
      data ((igramu(4,j,k),k=1,9),j=1,14) /  &
        -459, -592,  112,  322, 1273,   10,  234,   30,  117, &
        -319, -819,  472,  499, 1488,  121,  533,   95,  -50, &
          14,-1099,  449,  473, 1576,  177,  551,  -42,  -17, &
         599,-1216,  250,   98, 1190,  249,  397, -110,  -17, &
        1314,-1392,  -96, -213,  820,  115,  138, -192,  136, &
        1637,-1969,  -73, -198,  -92,  -94,  334, -108,  170, &
        2596,-1601, -364,-1082,  331, -343, -438, -121,  345, &
        2774,-2098,  122, -764,  343,  307, -336,   59,  236, &
        2621,-3036,  495, -284,  760,  -92,  -30,  221,  204, &
        1634,-2286,  -31,  140,    0,    0,    0,    0,    0, &
          90,-1584,  355,  638,    0,    0,    0,    0,    0, &
        -169,-2090,  -40,  756,    0,    0,    0,    0,    0, &
         334,   52, -982,-1153,    0,    0,    0,    0,    0, &
         900,  438,-3888,-1663,    0,    0,    0,    0,    0/
      data ((igramu(5,j,k),k=1,9),j=1,14) /  &
        -461, -827, -193,   21, 1236,   46,  115,  -87,  120, &
        -405,-1138,  201,  -89, 1062,  167,  435,  -31,  -43, &
         -83,-1761,  207, -266, 1067,  -47,  548,  -81,  134, &
         505,-2029,  246, -892,  211, -135,  569,  -47,  270, &
         820,-2363,   -1,-1575,  -64, -223,  440, -147,  428, &
        1257,-3345,  153,-1889,  135, -538,  459,  -87,  439, &
        2076,-2684, -104,-3002,  369, -366, -829, -409,  788, &
        2429,-4467, -650,-1679,  446,  404,-1180, -326,  402, &
        3059,-5203,  478,-1232, 1932,  420,-1539,  -42,  317, &
         941,-3887,  249, -847,    0,    0,    0,    0,    0, &
         421,-4788,   50,  316,    0,    0,    0,    0,    0, &
         849,-4199, -179, 1288,    0,    0,    0,    0,    0, &
         719, -676, -992, -420,    0,    0,    0,    0,    0, &
         901, -278,-2890, 1279,    0,    0,    0,    0,    0/
      data ((igramu(6,j,k),k=1,9),j=1,14) / &
        -433,-1575,   84,   15, 1535,  116,  119,  -22,   -8, &
        -198,-2625,  221,  134, 1958,  137,  217,   61, -261, &
          66,-3324,  271, -292, 1993, -209,  316,   38,   36, &
         384,-3602,  441,-1109, 1744, -342,  312,   74,  322, &
         294,-4194,   89,-1700, 1736, -749,  298,  -26,  784, &
         117,-4549,  -56,-2116, 1806, -891,  201,  -97,  952, &
         723,-4788, -194,-2134,  879, -704, -313,   -1,  808, &
        1306,-3750, -699,-2945, -512,  -10,-1143, -301,  892, &
        2527,-6108,  334,-2552,  887,  469,-1842, -110,  442, &
        1425,-4104,  595,-2907,    0,    0,    0,    0,    0, &
        1331,-5958,  328, -733,    0,    0,    0,    0,    0, &
         965,-4838,   21, 1085,    0,    0,    0,    0,    0, &
         939,-1013,-1067,  124,    0,    0,    0,    0,    0, &
         672,  204,-3358, 1253,    0,    0,    0,    0,    0 /
      data ((igramu(7,j,k),k=1,9),j=1,14) /  &
        -505,-1126,   67, -458, 1185, -176, -182,  -58,  -20, &
        -470,-1820,  187, -476, 1450, -282, -214,  -13,  -35, &
        -467,-2415,  194, -502, 1719, -637, -352, -119,  259, &
        -653,-2654,   95, -868, 2154, -453, -431, -160,  371, &
       -1023,-3505, -561,-1244, 2744, -831, -599, -370,  755, &
        -769,-4327, -227,-1226, 2755, -630, -660, -281,  832, &
         358,-4608, -523,-1668, 1009,-1045,-1133, -159,  837, &
        1613,-3206, -525,-3314, -440,-1038,-2134, -342, 1225, &
        2712,-6106, -124,-2458, 1206, 1016,-1677,   37,  217, &
        1877,-6369, -331,-1201,    0,    0,    0,    0,    0, &
        1200,-7179,   21, -123,    0,    0,    0,    0,    0, &
         662,-5700,  233, 1133,    0,    0,    0,    0,    0, &
        1206, -933, -595, -139,    0,    0,    0,    0,    0, &
         986,  566,-3359,  250,    0,    0,    0,    0,    0/
      data ((igramu(8,j,k),k=1,9),j=1,14) / &
        -503, -907,  -61, -516, 1300,   64,    3,  -32,  -69, &
        -505,-1429,   98, -575, 1529,   11,   17,   -4,  -82, &
        -764,-1731,  108, -833, 2151, -403,  158, -107,  242, &
        -818,-2281,  267, -918, 2610, -565,  214, -121,  359, &
        -748,-3038, -391,-1373, 2830,-1013,  -10, -333,  721, &
         155,-3256, -185,-1596, 1214, -397,  -74, -198,  525, &
        1177,-4372,  -84,-1196,  297,  121, -320,  -48,  320, &
        1988,-3365,  -63,-2127, -521, -276, -728,  -32,  480, &
        3314,-4647, -191,-1687,  746,  663,-1323, -164,  154, &
        1986,-4586, -244, -725,    0,    0,    0,    0,    0, &
        1094,-7017, -537, 1277,    0,    0,    0,    0,    0, &
         445,-6044,-1835, 2684,    0,    0,    0,    0,    0, &
         -16,-1431,-1589,  888,    0,    0,    0,    0,    0, &
         630,   60,-3774, 1464,    0,    0,    0,    0,    0 / 
      data ((igramu(9,j,k),k=1,9),j=1,14) /  &
        -269, -356,   27, -243, 1470, -207, -255,  -54,  -35, &
        -405, -511,  409, -570, 1975, -468,   88,   42,   58, &
        -559, -561,  546, -677, 2290, -734,  434,  -76,  321, &
        -119, -499,  228, -926, 2151, -755,  241, -155,  443, &
         538, -453, -556,-1153, 1473, -529, -146, -367,  479, &
        1386,-1205,  -76, -785,  932, -711, -240, -215,  412, &
        2083,-1673, -427, -471, -212, -318, -355,  -94,   89, &
        2545,-1784, -167, -455,   30,  188, -566,  -86,   70, &
        2982,-3117,  508,  145, 2456, -226, -273,  354,  136, &
        1774,-3800, -109, 1306,    0,    0,    0,    0,    0, &
         295,-4635,  715, 2044,    0,    0,    0,    0,    0, &
         -14,-4175,  481, 2512,    0,    0,    0,    0,    0, &
         209, -467,  276,  540,    0,    0,    0,    0,    0, &
        1088,  155,-4063,  761,    0,    0,    0,    0,    0/
      data ((igramu(10,j,k),k=1,9),j=1,14) /  &
        -421,  413,   75, -150, 1225, -149,  143,    2,  -53, &
        -381,  690,  360, -380, 1334, -390,  485,   17,  134, &
        -189,  836,  352, -244, 1362, -571,  665,  -77,  262, &
         448, 1119,  240,   43,  727, -511,  558,  -93,  146, &
        1179, 1153,  -73,  290,   62, -438,  365, -118,   37, &
        1433, 1438,  -56,  492, -669, -290,  636,  -10,  -64, &
        1898, 1051, -332, 1338, -880,  126,  463,   68, -302, &
        1841,  396,  209, 1752,-1305,  -36,  606,  215, -502, &
        1295, 1441,  423, 1174, -226, -742, 1498,  481, -110, &
         712, -130, -424, 1418,    0,    0,    0,    0,    0, &
        -782, 1499,  296,   89,    0,    0,    0,    0,    0, &
        -991,  198,   35, 1168,    0,    0,    0,    0,    0, &
         334,  -52, -982, 1153,    0,    0,    0,    0,    0, & 
         900, -438,-3888, 1663,    0,    0,    0,    0,    0/
      data ((igramu(11,j,k),k=1,9),j=1,14) /  &
        -481,  648, -167,  131, 1300, -212,  186,  -51,  -32, &
        -408, 1089,  158,  137, 1413, -370,  464,  -20,  114, &
        -109, 1610,  282,  374, 1268, -244,  629,   -4,   95, &
         519, 2112,  268,  853,  508,  -84,  530,    6,  -56, &
         800, 2554,   33, 1340,  256,   96,  461,  -55, -155, &
        1000, 3016,  273, 1903,  -60,  250,  765,   71, -186, &
         973, 2377, -120, 2930, -871,  316,  554,  -64, -583, &
         661, 2208, -111, 2959,-1655,  280,  502,  -56, -728, &
        -539, 2535,  783, 2808,-1888, -286, 2335,  559, -363, &
         321, 2900,   87, 1592,    0,    0,    0,    0,    0, &
        -486, 2952,  676, 1088,    0,    0,    0,    0,    0, &
        -302, 1562,  179, 1170,    0,    0,    0,    0,    0, &
         719,  676, -992,  420,    0,    0,    0,    0,    0, &
         901,  278,-2890,-1279,    0,    0,    0,    0,    0/
      data ((igramu(12,j,k),k=1,9),j=1,14) / &
        -477, 1433,  108,  116, 1639, -239,  211,   11,   56, &
        -391, 2297,  148,  44, 1631,  -321,  440,  105,  282, &
        -190, 2904,  231,  538, 1481,  -34,  648,  126,   54, &
         -67, 3051,  442, 1389, 1093,  156,  899,  222, -307, &
        -143, 3594,  125, 1885, 1107,  590,  818,  113, -642, &
        -723, 3746,   -8, 2408,  403,  851, 1182,   94, -815, &
        -201, 3969,  295, 2394,   26,  835,  490,  -47, -763, &
        -492, 2533,    7, 3708,-1961,  477,  835,   97, -993, &
        -582, 5098, -124, 3359,-3499,  682, 1094,  203, -640, &
         -28, 3114, -282, 3238,    0,    0,    0,    0,    0, &
         -84, 3674,  -23, 1902,    0,    0,    0,    0,    0, &
         -85, 2077, -160, 1227,    0,    0,    0,    0,    0, &
         939, 1013,-1067, -124,    0,    0,    0,    0,    0, &
         672, -204,-3358,-1253,    0,    0,    0,    0,    0 /
!
      data ((igramv(1,j,k),k=1,9),j=1,14) /  &
          10, -240,  -76,  -67, -212,  160, -223,  -42,  -16, &
         -25, -355,  -61,  -52, -346,  233, -242,  -47,  -29, &
         -92, -541,  -37,   38, -480,  304, -226,  -66,  -37, &
          -4, -698,   46,  290, -713,  350, -312, -110,  -78, &
         137, -260, -117,  150, -543,  686, -271, -157, -132, &
         125, -490, -231,  316, -743,  649,   18, -159, -264, &
         142, -781, -220,  414, -368,  222,  235,  -84, -167, &
         106,-1023, -101,  728, -226, -145,  334,  -12,  -59, &
        -317,-1126,  531,  250,   24, -658,  924,  274, -140, &
        -350, -512,  582, -375,    0,    0,    0,    0,    0, &
           9,  324,  350, -237,    0,    0,    0,    0,    0, &
         109, 1667,  603, -290,    0,    0,    0,    0,    0, &
         -50,-1062, -237, 1865,    0,    0,    0,    0,    0, &
        -157,  735,-2659,  704,    0,    0,    0,    0,    0/
      data ((igramv(2,j,k),k=1,9),j=1,14) / &
         101,  -41, -138, -184, -170,  149, -326,  -52,  -25, &
         176,  222, -175, -366, -189,  403, -530,  -89,  -28, &
         235,   70, -160, -324, -113,  350, -669, -114,  -11, &
         336,  135, -135, -339, -282,  489, -805, -171,  -48, &
         357,  -75,  -31,   47, -304,  555, -629, -155, -153, &
         432, -416, -181,  209, -343,  348, -537, -223,  -72, &
         194, -486, -338,  478, -662,  580,  -44, -165, -175, &
         386, -762,   83,  642, -665,  799,  -83, -164, -277, &
         -72, -321,  337,  -42, -515,   51,  123,   52, -114, &
        -471, -453,  575,    2,    0,    0,    0,    0,    0, &
         113,  156,  597,  -55,    0,    0,    0,    0,    0, &
         293, 1331, -320,  -76,    0,    0,    0,    0,    0, &
         576, -841,-1548, 1420,    0,    0,    0,    0,    0, &
        -446, 1008,-2070,-1156,    0,    0,    0,    0,    0 /
      data ((igramv(3,j,k),k=1,9),j=1,14) /  &
          90,  -56,  -94, -100,  -81,   27, -189,  -20,   -1, &
          56, -130, -126,  -78, -188,   87, -190,  -33,  -32, &
         129, -238,  -62,  -92, -264,   -7, -243,  -37,   -2, &
         128, -197,  -73,  -23, -320,  -38, -257,  -53,  -33, &
         127, -278,  -16,  138, -495,  112,  -59,  -39, -156, &
         169, -208, -175,   63, -121,  -94,   93,  -55,  -62, &
         257,  -79, -399,   85, -182,  297,   55, -119, -182, &
         216,  -49,  -34,  259, -292,  -58,  381,   37,  -17, &
         126,  422,  220, -359, -705,  371,  -49,  -67, -105, &
        -288,   80,  -22, -187,    0,    0,    0,    0,    0, &
        -238,  530, -135,  408,    0,    0,    0,    0,    0, &
         162, 1723, -518, -777,    0,    0,    0,    0,    0, &
        1130, -427,-1869,  431,    0,    0,    0,    0,    0, &
        -304,  430,-2055,  200,    0,    0,    0,    0,    0/
      data ((igramv(4,j,k),k=1,9),j=1,14) /  &
         -10,  -73,  -55,  -22, -181, -182,   57,   14,   10, &
         -44,  213,  -31, -116, -206, -151,   40,   -2,   73, &
          41,   78,  124, -120, -231, -309,  -34,   17,  114, &
           8,  296,  127, -231, -304, -270,  -53,   11,  130, &
          15,   27,  301,   10, -516, -580,  252,   77,  117, &
         358,  336,  203, -369, -187, -687, -103,  -45,  254, &
         475,  388,  -23, -307, -157, -287, -165, -107,  171, &
         140,   70,    0, -148, -194, -396,  356,   -6,  142, &
         387,  503,  447, -536,  272,  150,  -23,  -56,  -14, &
          16,  704,  181, -504,    0,    0,    0,    0,    0, &
         -88,  679,  269, -775,    0,    0,    0,    0,    0, &
         139, 1645, -278,-1611,    0,    0,    0,    0,    0, &
        1084,  -32,-1414,  -66,    0,    0,    0,    0,    0, &
         234,  149,-2049, -472,    0,    0,    0,    0,    0/
      data ((igramv(5,j,k),k=1,9),j=1,14) / &
         -35,  185, -111,   31, -265,  -31,    4,  -18,   16, &
         -46,  301,  -74,  -95, -232, -117,   21,    3,   42, &
          70,  178,   55,  -50, -230, -234, -108,  -19,   38, &
          98,  261,  197,  -78, -137, -317, -168,   -8,   62, &
         220,  212,  206, -170, -429, -538,  -53,  -22,   80, &
         269,  354, -189, -206, -550, -447,  -90, -139,  184, &
         168,  831, -170, -581, -596, -344,  170, -102,  187, &
         213,  962, -103, -587, -737, -331,  108,  -57,  209, &
         289, 1171,  290, -673, -573,  194, -203, -172,    7, &
          37,  522,  308, -681,    0,    0,    0,    0,    0, &
        -176,  498,  693,-1183,    0,    0,    0,    0,    0, &
        -280, 1163,  -25,-1525,    0,    0,    0,    0,    0, &
         231,  439, -813, -487,    0,    0,    0,    0,    0, &
         411, -673,-1671,  960,    0,    0,    0,    0,    0 /
      data ((igramv(6,j,k),k=1,9),j=1,14) / &
         -57,  412, -111,  -94, -364,  -26,  -18,  -34,   40, &
        -192,  836,   35, -246, -656, -201,   15,   -5,   66, &
        -157, 1006,  155, -462, -879, -436,   -9,  -47,  136, &
        -318, 1645,  209, -946,-1241, -614,  135,  -63,  298, &
         246,  864,  148, -699, -914, -791, -206, -143,  278, &
         384,  337,  -14, -370, -561, -785, -116, -143,  250, &
         333,  388, -103, -316, -249, -480,  107,  -46,  261, &
         202,  780, -290, -888, -500, -499,  326,  -32,  246, &
         331, 1407,   63,-1012, -259,   -1,  178,  -65,  177, &
        -382,  851,  367, -166,    0,    0,    0,    0,    0, &
        -779,  494,  336, -520,    0,    0,    0,    0,    0, &
        -825,  748,  514,-1044,    0,    0,    0,    0,    0, &
        -101, 1061, -624,-1611,    0,    0,    0,    0,    0, &
        -475,-1188,-2589, 1459,    0,    0,    0,    0,    0 /
      data ((igramv(7,j,k),k=1,9),j=1,14) /  &
        -100,  522,  -44, -130, -392,  -98,  -54,  -11,   44, &
        -109,  625,    0, -199, -454, -157,  -41,   -3,   68, &
         -37,  502,    8,  -68, -476, -336, -230,  -64,   80, &
         -57,  889,   64, -446, -809, -313, -165,  -81,   99, &
         -46,  929,  -17, -522, -841, -568,  120,  -68,  156, &
           4,  712, -125, -432, -608, -566,  299,  -81,  189, &
         -95, 1052, -110, -616, -516, -237,  568,  -13,  135, &
         -77, 1186,  -51, -831, -843,  149,  556,   10,   46, &
        -121,-1178,  912,  966, 1939,  301, 1394,  547,  -73, &
        -433,  379,  357,  310,    0,    0,    0,    0,    0, &
        -365,  -67,  251, -101,    0,    0,    0,    0,    0, &
        -382, -158,  818, -819,    0,    0,    0,    0,    0, &
         -50, 1062, -237,-1865,    0,    0,    0,    0,    0, &
        -157, -735,-2659, -704,    0,    0,    0,    0,    0/
      data ((igramv(8,j,k),k=1,9),j=1,14) /  &
          83,  127, -103,  116, -187,  -85, -277,  -32,   11, &
         196, -164,  -67,  205,   74, -318, -364,  -35,   40, &
         264, -160,   -2,  258,  127, -281, -525,  -51,   -7, &
         352,  -65,   33,  209,   42, -370, -600,  -88,   21, &
         358,  247,   35, -205,    5, -428, -473, -102,   93, &
         371,  553, -148, -322, -461, -208, -382, -191,  -20, &
          74,  722, -201, -616, -506, -412,  201,  -74,  131, &
         202,  722,  229, -622, -636, -614,  212,  -73,  267, &
          -9,  946,  476, -356, -307,   83,  132,   92,   91, &
        -581,  768,  486, -311,    0,    0,    0,    0,    0, &
        -386,  340,  375, -702,    0,    0,    0,    0,    0, &
        -150, -277,  -35, -867,    0,    0,    0,    0,    0, &
         576,  841,-1548,-1420,    0,    0,    0,    0,    0, &
        -446,-1008,-2070, 1156,    0,    0,    0,    0,    0/
      data ((igramv(9,j,k),k=1,9),j=1,14) / &
          80,  101,  -85,   58, -101,    2, -168,  -12,   -6, &
          83,  131, -122,    5,  -51,  -66, -174,  -31,   21, &
         164,  199,  -27,   30,  -69,   31, -238,  -29,  -20, &
         164,  180,  -55,  -44,  -82,   83, -260,  -53,  -19, &
         177,   96,  -57,  -47, -143, -148,  -70,  -41,   76, &
          73,  387, -251, -183, -158,  116,  182,  -66,   57, &
         123,  260, -246, -224, -258, -281,  324,  -37,  167, &
         217, -290,   44, -104,  186,  226,  467,   87,  -82, &
           9,  731,  268, -314, -684,  152,  171,    5,    8, &
        -363,  590,  -31, -302,    0,    0,    0,    0,    0, &
        -458,  427,  -51,-1171,    0,    0,    0,    0,    0, &
          42, -666, -593, -147,    0,    0,    0,    0,    0, &
        1130,  427,-1869, -431,    0,    0,    0,    0,    0, &
        -304, -430,-2055, -200,    0,    0,    0,    0,    0 /
      data ((igramv(10,j,k),k=1,9),j=1,14) / &
           2,   -6,  -39,   60,  -84,  148,   54,   14,   -8, &
          12,  -66,  -15,   30,    8,  175,  -34,  -20,  -58, &
          18,   58,   96,   34,  -57,  337,  -30,   10,  -94, &
          59,  -89,   97,   82, -163,  351, -145,  -14, -107, &
         179,  140,  312,  -52, -356,  629,    5,   24, -122, &
         450,   -9,  276,  178, -176,  849, -218,  -59, -276, &
         679, -380,   14,  321,   40,  425, -357, -131, -270, &
         323, -351,   -5,  301,   49,  361,  157,  -55,  -99, &
          85,  109,  510,  172, -322,  -17,  297,   -9,    2, &
         -68,  -36,  300,  154,    0,    0,    0,    0,    0, &
         -35,  589,  151,  117,    0,    0,    0,    0,    0, &
         589, -459, -403,  931,    0,    0,    0,    0,    0, &
        1084,   32,-1414,   66,    0,    0,    0,    0,    0, &
         234, -149,-2049,  472,    0,    0,    0,    0,    0 /
      data ((igramv(11,j,k),k=1,9),j=1,14) / &
           2, -102,  -83,  -57, -146,   61,  -24,  -16,   -5, &
          21, -179,  -65,   13, -197,  146,  -85,   -9,  -36, &
         116,  -66,   32,  -76, -287,  296, -249,  -48,  -48, &
         182, -144,  174, -117, -272,  413, -386,  -56,  -59, &
         275,    5,  160,  -50, -644,  643, -273,  -73,  -84, &
         315, -218, -223,   61, -524,  523, -281, -186, -198, &
         210, -655, -112,  436, -579,  481,   61, -124, -180, &
         301, -781,   88,  500, -658,  494,   76,  -35,  -88, &
        -178, -161,  461,   81,-1029,  231,  316,  -68,  -75, &
        -119, -240,  422,  374,    0,    0,    0,    0,    0, &
          45,  580,  261,  374,    0,    0,    0,    0,    0, &
         247,  193, -378,  741,    0,    0,    0,    0,    0, &
         231, -439, -813,  487,    0,    0,    0,    0,    0, &
         411,  673,-1671, -960,    0,    0,    0,    0,    0 /
      data ((igramv(12,j,k),k=1,9),j=1,14) /  &
           8, -250, -112,  -23, -196,   46, -105,  -42,  -23, &
         -74, -412,   19,  -37, -348,  239, -182,  -42,    4, &
         -70, -665,  134,  220, -607,  454, -167,  -73,  -56, &
        -112, -873,  139,  432, -796,  710, -204, -135, -168, &
         315, -546,  -12,  473, -925,  928, -451, -241, -278, &
         538, -184, -124,  211, -468,  911, -484, -259, -299, &
         450, -404,  -18,  284,  -33,  524, -118, -113, -313, &
         345, -749, -328,  881, -265,  535,  142,  -74, -283, &
         -50,-1017,  -31,  812, -191,  540,  529,  -27, -407, &
        -194, -186,  563, -300,    0,    0,    0,    0,    0, &
        -260,  645,  351, -250,    0,    0,    0,    0,    0, &
        -345, 1191,  409,   12,    0,    0,    0,    0,    0, &
        -101,-1061, -624, 1611,    0,    0,    0,    0,    0, &
        -475, 1188,-2589,-1459,    0,    0,    0,    0,    0/
!
      ier=0
!
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   ! Encadrement de r parmi les valeurs tabules du modele gram        !
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      if ((z.gt.90000.d0).or.(z.lt.25000.d0)) then
         wind=0.d0
         aziw=0.d0
         goto 999
      end if
!
      do 300 i=1,14
         z1=25000.d0+(i-1)*5000.d0
         z2=25000.d0+(i)*5000.d0
         if((z.ge.z1).and.(z.lt.z2)) go to 400
 300  continue
!
 400  continue
!
      rlon=xlon
!
!     Si le vehicule se trouve dans l hemisphere sud, on doit inverser
!     les saisons ....
!
      if (xlat.lt.0.d0) then
         rcolat=pis2+xlat
         rlon=-rlon
         jmois=imois+6
         if (jmois.gt.12) jmois=jmois-12
      else
         rcolat=pis2-xlat
         jmois=imois
      end if
      do 450 j=1,9
         a1(j)=dfloat(igramu(jmois,i,j))
         b1(j)=dfloat(igramv(jmois,i,j))
         a2(j)=dfloat(igramu(jmois,i+1,j))
         b2(j)=dfloat(igramv(jmois,i+1,j))
 450  continue
!
      u1=a1(1)+a1(2)*dcos(rcolat)+a1(3)*dcos(rlon)*dsin(rcolat) &
         +a1(4)*dsin(rlon)*dsin(rcolat) &
         +0.5d0*a1(5)*(3.d0*(dcos(rcolat))**2-1.d0) &
         +1.5d0*a1(6)*dcos(rlon)*dsin(2.d0*rcolat) &
         +1.5d0*a1(7)*dsin(rlon)*dsin(2.d0*rcolat) &
         +3.d0*(dsin(rcolat))**2 &
         *(a1(8)*(2.d0*(dcos(rlon))**2-1.d0)+a1(9)*dsin(2.d0*rlon)) 
      v1=b1(1)+b1(2)*dcos(rcolat)+b1(3)*dcos(rlon)*dsin(rcolat) &
         +b1(4)*dsin(rlon)*dsin(rcolat) &
         +0.5d0*b1(5)*(3.d0*(dcos(rcolat))**2-1.d0) &
         +1.5d0*b1(6)*dcos(rlon)*dsin(2.d0*rcolat) &
         +1.5d0*b1(7)*dsin(rlon)*dsin(2.d0*rcolat) &
         +3.d0*(dsin(rcolat))**2 &
         *(b1(8)*(2.d0*(dcos(rlon))**2-1.d0)+b1(9)*dsin(2.d0*rlon))
      u2=a2(1)+a2(2)*dcos(rcolat)+a2(3)*dcos(rlon)*dsin(rcolat) &
         +a2(4)*dsin(rlon)*dsin(rcolat) &
         +0.5d0*a2(5)*(3.d0*(dcos(rcolat))**2-1.d0) &
         +1.5d0*a2(6)*dcos(rlon)*dsin(2.d0*rcolat) &
         +1.5d0*a2(7)*dsin(rlon)*dsin(2.d0*rcolat) &
         +3.d0*(dsin(rcolat))**2 &
         *(a2(8)*(2.d0*(dcos(rlon))**2-1.d0)+a2(9)*dsin(2.d0*rlon))
      v2=b2(1)+b2(2)*dcos(rcolat)+b2(3)*dcos(rlon)*dsin(rcolat) &
         +b2(4)*dsin(rlon)*dsin(rcolat) &
         +0.5d0*b2(5)*(3.d0*(dcos(rcolat))**2-1.d0) &
         +1.5d0*b2(6)*dcos(rlon)*dsin(2.d0*rcolat) &
         +1.5d0*b2(7)*dsin(rlon)*dsin(2.d0*rcolat) &
         +3.d0*(dsin(rcolat))**2 &
         *(b2(8)*(2.d0*(dcos(rlon))**2-1.d0)+b2(9)*dsin(2.d0*rlon))
!
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   ! Calcul des composantes u et v du vent par interpolation          !
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      u=((z2-z)*u1+(z-z1)*u2)/(z2-z1)
      v=((z2-z)*v1+(z-z1)*v2)/(z2-z1)
!
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   ! Calcul de wind (en m/s) et de aziw (en radians)                  !
!   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      wind=0.01d0*dsqrt(u**2+v**2)
      if (v.eq.0.d0) then
         if (u.eq.0.d0) then
            aziw=0.d0
         else if (u.gt.0.d0) then
            aziw=pis2
         else
            aziw=-pis2
         end if
         goto 999
      else
         aziw=datan(u/v)
      end if
      if ((u.eq.0.d0).and.(v.lt.0.d0)) aziw=pi
      if ((u.gt.0.d0).and.(v.lt.0.d0)) aziw=aziw+pi
      if ((u.lt.0.d0).and.(v.lt.0.d0)) aziw=aziw-pi
!
!
 999  return
      end  subroutine MSP_vgram


  subroutine MSP_calculer_atmosphere_old (atm,date_js,R,rlon,phisat,ro,vson,pression,temperature,wind,aziw,ech_date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atmosphere_old
!
!$Resume
!
!$Description
!    Routine de calcul de la pression, de la temperature, de la densite de l'atmosphere
!    et de la vitesse du son, ainsi que de la vitesse et de l'azimut du vent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atmosphere_old (atm,date_js,R,rlon,phisat,ro,vson,pression,temperature,wind,aziw,[ech_date])
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_jour_sec) :: date_js 
!.    real (KIND=PM_REEL) :: R 
!.    real (KIND=PM_REEL) :: rlon 
!.    real (KIND=PM_REEL) :: phisat 
!.    integer :: ech_date 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!.    real (KIND=PM_REEL) :: pression 
!.    real (KIND=PM_REEL) :: temperature 
!.    real (KIND=PM_REEL) :: wind 
!.    real (KIND=PM_REEL) :: aziw 
!
!$Arguments
!>E     atm          :<MSP_ATMOSPHERE>   
!>E     date_js      :<tm_jour_sec>      Date jj
!>E     R            :<PM_REEL>          Distance centre-planete / sonde (m)
!>E     rlon         :<PM_REEL>          Longitude (rad)
!>E     phisat       :<PM_REEL>          Latitude sphérique (rad)
!>S     ro           :<PM_REEL>          Densite atmospherique (kg/m^3)
!>S     vson         :<PM_REEL>          Vitesse du son (m/s)
!>S     pression     :<PM_REEL>          Pression atmospherique (Pa)
!>S     temperature  :<PM_REEL>          Temperature atmospherique (K)
!>S     wind         :<PM_REEL>          Vitesse du vent en m/s
!>S     aziw         :<PM_REEL>          Azimut du vent en rad
!>[E]   ech_date     :<integer>          Echelle de temps associé à la date (PM_TE, Pm_TUC) [défaut PM_TUC]
!
!$Common
!
!$Routines
!- MSP_date_etutc
!- MSP_calculer_atm_cira
!- MSP_vgram
!- MSP_signaler_message
!- MSP_cal_atm_us76
!- MSP_calculer_atm_dtm
!- MSP_calculer_atm_at77
!- MSP_calculer_atm_marsrusse
!- MSP_calculer_atm_emcd
!- MSP_calculer_atm_marsgram
!- MSP_cal_atm_exp
!- MSP_calculer_actsol
!- cps_calculer_msis90
!- cps_calculer_msis2000
!- cps_calculer_met88
!- MSP_cal_atm_venus
!- MSP_cal_vent
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

    !--------------------------------------------------------------------------------
    ! Arguments
    !--------------------------------------------------------------------------------

    ! Entrees
    type (MSP_ATMOSPHERE), intent(IN):: atm           ! structure atmosphère
    type (tm_jour_sec),  intent(IN)  :: date_js       ! Date JJ 1950 (en jours/secondes)
    real (KIND=PM_REEL), intent(IN)  ::R              ! Distance centre planete - sonde
    real (KIND=PM_REEL), intent(IN)  ::rlon           ! Longitude
    real (KIND=PM_REEL), intent(IN)  ::phisat         ! Latitude
    integer,intent(in),optional      :: ech_date      ! echelle de temps de la date en entrée (1 = TE, 3 = TUC)

    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro            ! Densite atmospherique
    real (KIND=PM_REEL), intent(OUT) :: vson          ! Vitesse du son
    real (KIND=PM_REEL), intent(OUT) :: pression      ! Pression atmospherique
    real (KIND=PM_REEL), intent(OUT) :: temperature   ! Temperature atmospherique
    real (KIND=PM_REEL), intent(OUT) :: wind          ! Vitesse du vent en m/s
    real (KIND=PM_REEL), intent(OUT) :: aziw          ! Azimut du vent en radians

    !-----------------------------------------------------------------------------------
    ! Variables locales
    !-----------------------------------------------------------------------------------

    ! Conversion de date
    integer::  imois
    
    ! Variables pour le flux solaire
    real(kind=pm_reel) :: fljop, flmoy ! flux journalier, flux moyen
    real(kind=pm_reel), dimension(7) :: ap ! indices géomagnétiques
    real (kind=pm_reel) :: ahsos ! heure solaire
        
    ! Variables liées au vent tabule
    real(kind=pm_reel)::windm,aziwm

    real(kind=PM_REEL) ::alsat
    real(kind=PM_REEL):: visco

    ! date TE, date TUC
    type(tm_jour_sec) :: date_tuc,date_te
    integer :: ier,ech_t

    ! rayon equatorial Venus
    integer :: trouve
    real(kind=PM_REEL):: venus_requa, venus_alt_sonde

    ! 
    character(LEN=256) :: unite
    !-------------------------------------------------------------------------
    ! Corps de la routine
    !-------------------------------------------------------------------------
    ! Calcul des dates TUC et TE.
    !
    ! Par convention les modèles d'atmosphère terrestres sont définis en date TUC
    ! Par convention les modèles d'atmosphère martiens sont définis en date TE
    if (present(ech_date)) then
       ech_t = ech_date
    else
       ech_t = PM_TUC
    end if

    ! convertion de la date si elle est définie en TE
    if (ech_t==PM_TE) then
       date_te=date_js
!
!
      ! -- Par défaut, date en ECHT_TE
      
      call MSP_date_etutc(date_js,1,date_tuc)
      if (MSP_gen_messages("MSP_calculer_atmosphere")) return

    else

       date_tuc=date_js
       
       call MSP_date_etutc(date_js,-1,date_te)
       if (MSP_gen_messages("MSP_calculer_atmosphere")) return

    end if

    select case (atm%id_modele)

       ! APPEL A UN MODELE D'ATMOSPHERE TERRESTRE 
       !-------------------------------------------

    case (MSP_IDATM_CIRA88_MSIS86)

       call MSP_calculer_atm_cira(date_tuc,atm%actsol,R,rlon,phisat,ro,vson,pression,temperature)
       if (MSP_gen_messages("MSP_calculer_atmosphere") ) return


       ! Utilisation de vent tabule gram si demandé
       if (MSP_mode_vent_modele(atm%vent)) then
          ! Calcul du vent tabule
          imois = MSP_calculer_mois(date_tuc)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
          alsat = MSP_calculer_alt_terre(R,rlon,phisat)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

!        ------------ Remplacementio_e_vgram (iolib) par la fonction MSP_vgram -----------------

          call MSP_vgram (alsat,phisat,rlon,imois,PM_PI,PM_PI_SUR2,windm,aziwm,ier)
          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_vgram",routine="MSP_calculer_atmosphere")
             return
          endif
       else
          windm = 0._PM_REEL
          aziwm = 0._PM_REEL
       end if

       !----------------------------------------------------------------------------

    case (MSP_IDATM_US76)

    ! Calcul de vson, de la pression, de ro, de la temperature
       call MSP_cal_atm_us76(atm%us76,R,rlon,phisat,vson,pression,ro,visco,temperature)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

       ! Utilisation de vent tabule gram si demandé
       if (MSP_mode_vent_modele(atm%vent)) then
          imois = MSP_calculer_mois(date_tuc)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
          alsat = MSP_calculer_alt_terre(R,rlon,phisat)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
          ! Calcul du vent tabule
!        ------------ Remplacementio_e_vgram (iolib) par la fonction MSP_vgram -----------------

          call MSP_vgram (alsat,phisat,rlon,imois,PM_PI,PM_PI_SUR2,windm,aziwm,ier)
          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_vgram",routine="MSP_calculer_atmosphere")
             return
          endif
       else
          windm = 0._PM_REEL
          aziwm = 0._PM_REEL
       end if

       !------------------------------------------------------------------------------

    case (MSP_IDATM_DTM)


       call MSP_calculer_atm_dtm(date_tuc,atm%actsol,R,rlon,phisat,ro)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
       
       vson = 0._pm_reel
       pression = 0._pm_reel
       temperature = 0._pm_reel

      ! Utilisation de vent tabule gram si demandé
      if (MSP_mode_vent_modele(atm%vent)) then
         ! Calcul du vent tabule
         imois=MSP_calculer_mois(date_tuc)
         if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
         alsat = MSP_calculer_alt_terre(R,rlon,phisat)
         if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

!        ------------ Remplacementio_e_vgram (iolib) par la fonction MSP_vgram -----------------

         call MSP_vgram (alsat,phisat,rlon,imois,PM_PI,PM_PI_SUR2,windm,aziwm,ier)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_vgram",routine="MSP_calculer_atmosphere")
            return
         endif
      else
         windm = 0._PM_REEL
         aziwm = 0._PM_REEL
      end if

       !------------------------------------------------------------------------------------

    case (MSP_IDATM_AT77)

       call MSP_calculer_atm_at77(date_tuc,atm%actsol,R,rlon,phisat,ro)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
       vson = 0._pm_reel
       pression = 0._pm_reel
       temperature = 0._pm_reel

       ! Utilisation de vent tabule gram si demandé
       if (MSP_mode_vent_modele(atm%vent)) then
          ! calcul du mois
          imois=MSP_calculer_mois(date_tuc)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
          alsat = MSP_calculer_alt_terre(R,rlon,phisat)
          if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

!        ------------ Remplacementio_e_vgram (iolib) par la fonction MSP_vgram -----------------

          call MSP_vgram (alsat,phisat,rlon,imois,PM_PI,PM_PI_SUR2,windm,aziwm,ier)
          if ( ier /= 0 ) then
             call MSP_signaler_message (cle_mes="MSP_vgram",routine="MSP_calculer_atmosphere")
             return
          endif
       else
          windm = 0._PM_REEL
          aziwm = 0._PM_REEL
       end if

       !-------------------------------------------------------------------------------------
       ! APPEL A UN MODELE D'ATMOSPHERE MARTIENNE
       !-----------------------------------------

    case (MSP_IDATM_MARS_RUSSE)

       call MSP_calculer_atm_marsrusse(atm%marsrusse,R,phisat,ro,vson,pression,temperature)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
      ! Definition du vent tabule
       windm = 0._PM_REEL
       aziwm = 0._PM_REEL

       !-------------------------------------------------------------------------------------

    case(MSP_IDATM_MARS_EMCD,MSP_IDATM_MARS_EMCD_23A,&
         MSP_IDATM_MARS_EMCD_31,MSP_IDATM_MARS_EMCD_42,MSP_IDATM_MARS_EMCD_43) 

       call MSP_calculer_atm_emcd(atm%emcd,date_te,R,rlon,phisat,ro,vson,pression,temperature,windm,aziwm)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

       !--------------------------------------------------------------------------------------------

    case (MSP_IDATM_MARSGRAM,MSP_IDATM_MARSGRAM_2001)

       call MSP_calculer_atm_marsgram(atm%marsgram,date_te,R,rlon,phisat,ro,vson,pression,&
            temperature,windm,aziwm)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

       !--------------------------------------------------------------------------------------------


       !--------------------------------------------------------------------------------------------
       ! APPEL  A UN MODELE D'ATMOSPHERE DE TYPE EXPONENTIEL TERRESTRE OU MARTIEN
       !-------------------------------------------------------------------------

    case(MSP_IDATM_EXP)

       ! Calcul de ro
       call MSP_cal_atm_exp(atm%exp,R,phisat,ro)
       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

       pression = 0._pm_reel
       temperature = 0._pm_reel
       vson = 0._PM_REEL
       windm = 0._PM_REEL
       aziwm = 0._PM_REEL

       
       !--------------------------------------------------------------------------------------------
       ! APPEL  AU MODELE TERRESTRE MSIS 90
       !-------------------------------------------------------------------------

    case (MSP_IDATM_MSIS_90)

       call MSP_calculer_actsol(atm%actsol,date_te, fljop, flmoy, ap)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) return

       ! Calcul de l'heure locale d'après la date en TUC
       ahsos = MSP_heure_locale(date_tuc,rlon)

       call cps_calculer_msis90 (date_te, fljop, flmoy, ap, phisat, rlon, &
            alsat, ahsos, ro, temp=temperature)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) then
          return 
       end if
       vson = 0._pm_reel

       
       !--------------------------------------------------------------------------------------------
       ! APPEL  AU MODELE TERRESTRE NRL-MSISE 2000
       !-------------------------------------------------------------------------

    case (MSP_IDATM_NRL_MSISE_2000)
       call MSP_calculer_actsol(atm%actsol,date_te, fljop, flmoy, ap)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) return

       ! Calcul de l'heure locale d'après la date en TUC
       ahsos = MSP_heure_locale(date_tuc,rlon)

       call cps_calculer_msis2000 (date_te, fljop, flmoy, ap, phisat, rlon, &
            alsat, ahsos, ro, temp=temperature)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) then
          return 
       end if
       vson = 0._pm_reel
       
       !--------------------------------------------------------------------------------------------
       ! APPEL  AU MODELE TERRESTRE MET88
       !-------------------------------------------------------------------------

    case (MSP_IDATM_MET_88)
       call MSP_calculer_actsol(atm%actsol,date_te, fljop, flmoy, ap)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) return
      
       if(alsat < 86000.0) then
          call MSP_signaler_message(cle_mes="MSP_ATM_DTM_01", partie_variable="Altitude < 86 km")
       end if
       
       call cps_calculer_met88 (date_te, alsat, phisat, rlon, fljop, flmoy, &
            ap, ro, temperature)
       if(MSP_gen_messages("MSP_calculer_atmosphere")) then
          return 
       end if
       vson = 0._pm_reel
       
       !--------------------------------------------------------------------------------------------
       ! APPEL  A UN MODELE D'ATMOSPHERE DE TYPE VENUS_PETROPOULOS88
       !--------------------------------------------------------------------------------------------

    case(MSP_IDATM_VENUS)

       ! Rayon equatorial de Venus
       ! trouve = cps_getCsteThCourante(299,'requa',venus_requa)
       trouve = cps_getCsteTh(299, "UAI1994", "requa", venus_requa, unite)
       if (trouve /= 0) then
          call MSP_signaler_message (cle_mes="MSP_calculer_atmosphere_001",routine="MSP_calculer_atmosphere")
          return
       end if

       ! Calcul de l'altitude :
       ! - R = Distance centre planete - sonde
       ! - alt = R - venus_requa       
       venus_alt_sonde = R - venus_requa

       ! Calcul de la densite ro
       call MSP_cal_atm_venus(ro,venus_alt_sonde,atm%venus)

       if (MSP_gen_messages("MSP_calculer_atmosphere" )) return

       !
       vson = 0._PM_REEL
       pression = 0._pm_reel
       temperature = 0._pm_reel
       windm = 0._PM_REEL
       aziwm = 0._PM_REEL

       !
       !---------------------------------------------------------------------------------------------

    case default

       pression = 0._PM_REEL
       ro = 0._PM_REEL
       temperature = 0._PM_REEL
       vson = 0._PM_REEL
    end select

    ! Calcul de la vitesse et de l'azimut du vent
    !--------------------------------------------
    call MSP_cal_vent(atm%vent,alsat,wind,aziw,windm=windm,aziwm=aziwm)
    if (MSP_gen_messages("MSP_calculer_atmosphere" )) return
    
  end subroutine MSP_calculer_atmosphere_old


  subroutine MSP_calculer_atmosphere(atm, date_js_te, corgeod, hsol, ro, vson)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atmosphere
!
!$Resume
!    Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!
!$Description
!    Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!
!$Auteur
!   G. Mercadier (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atmosphere(atm, date_js_te, corgeod, hsol, ro, vson)
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_jour_sec) :: date_js_te 
!.    type (tm_geodesique) :: corgeod 
!.    real(KIND=PM_REEL) :: hsol 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!
!$Arguments
!>E     atm         :<MSP_ATMOSPHERE>   Structure atmosphère
!>E     date_js_te  :<tm_jour_sec>      Date TE JJ 1950 (en jours/secondes)
!>E     corgeod     :<tm_geodesique>    Coordonnées géodésiques du véhicule
!>E     hsol        :<PM_REEL>          Heure solaire 
!>S     ro          :<PM_REEL>          Densité de l'atmosphère
!>S     vson        :<PM_REEL>          Vitesse du son
!
!$Common
!
!$Routines
!- MSP_date_etutc
!- MSP_calculer_atmosphere_terre
!- MSP_calculer_atmosphere_mars
!- MSP_calculer_atmosphere_venus
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

    !------------!
    ! Arguments  !
    !------------!

    ! Entrées
    type (MSP_ATMOSPHERE), intent(IN) :: atm           ! Structure atmosphère
    type (tm_jour_sec), intent(IN)    :: date_js_te    ! Date TE JJ 1950 (en jours/secondes)
    type (tm_geodesique), intent(IN)  :: corgeod       ! Coordonnées géodésiques du véhicule dans le repère planétographique    
    real(KIND=PM_REEL), intent(IN)    :: hsol          ! Heure solaire locale (rad)
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro             ! Densité atmosphérique
    real (KIND=PM_REEL), intent(OUT) :: vson           ! Vitesse du son
    
    !-------------------!
    ! Variables locales !
    !-------------------!
    type (tm_jour_sec) :: date_js_tuc

    !---------------------!
    ! Corps de la routine !
    !---------------------!
  
    ! Par convention:
    !  - les modèles d'atmosphère terrestres sont définis en date TUC
    !  - les modèles d'atmosphère martiens sont définis en date TE
  
    ! Initialisations
    ro = 0._pm_reel
    vson = 0._pm_reel   
  
    ! Conversion de la date TE en date TUC
    call MSP_date_etutc(date_js_te, 1, date_js_tuc)
    if (MSP_gen_messages("MSP_calculer_atmosphere")) return
        
    select case (atm%id_modele)
       !
       ! Appel aux modèles d'atmosphère terrestres
       !
       case (MSP_IDATM_CIRA88_MSIS86,MSP_IDATM_US76,MSP_IDATM_DTM,MSP_IDATM_AT77,&
             MSP_IDATM_MSIS_90,MSP_IDATM_NRL_MSISE_2000,MSP_IDATM_MET_88,MSP_IDATM_EXP)
          call MSP_calculer_atmosphere_terre(atm, date_js_tuc, corgeod, hsol, ro, vson)	   
       !
       ! Appel aux modèles d'atmosphère martiens
       !
       case (MSP_IDATM_MARS_RUSSE,MSP_IDATM_MARS_EMCD_31,&
             MSP_IDATM_MARS_EMCD_42,MSP_IDATM_MARS_EMCD_43)
          call MSP_calculer_atmosphere_mars(atm, date_js_te, corgeod, ro, vson)
       case (MSP_IDATM_VENUS)  
       !
       ! Appel aux modèles d'atmosphère vénusiens
       !
          call MSP_calculer_atmosphere_venus(atm, corgeod, ro, vson)	     
       case default
          ro = 0._pm_reel
	  vson = 0._pm_reel
          call MSP_signaler_message(cle_mes="MSP_calculer_atmosphere_003",routine="MSP_calculer_atmosphere")
          return
    end select
	   
  end subroutine MSP_calculer_atmosphere	   
	   
	   
  subroutine MSP_calculer_atmosphere_terre(atm, date_js_tuc, corgeod, hsol, ro, vson)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atmosphere_terre
!
!$Resume
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles terrestres
!
!$Description
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles terrestres
!
!$Auteur
!    G. Mercadier (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atmosphere_terre(atm, date_js_tuc, corgeod, hsol, ro, vson)
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_jour_sec) :: date_js_tuc 
!.    type (tm_geodesique) :: corgeod 
!.    real(KIND=PM_REEL) :: hsol 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!
!$Arguments
!>E     atm          :<MSP_ATMOSPHERE>   Structure atmosphère
!>E     date_js_tuc  :<tm_jour_sec>      Date TUC JJ 1950 (en jours/secondes)
!>E     corgeod      :<tm_geodesique>    Coordonnées géodésiques du véhicule
!>E     hsol         :<PM_REEL>          Heure solaire
!>S     ro           :<PM_REEL>          Densité de l'atmosphère
!>S     vson         :<PM_REEL>          Vitesse du son
!
!$Common
!
!$Routines
!- MSP_calculer_actsol
!- cps_atm_cira_msis86
!- MSP_signaler_message
!- MSP_consulter_atmosphere
!- cps_atm_us76d
!- cps_atm_dtm78
!- mt_geod_car
!- md_joursec_jourfrac
!- cps_roat77
!- cps_calculer_msis90
!- cps_calculer_msis2000
!- cps_calculer_met88
!- cps_atm_exponentiel
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

    !------------!
    ! Arguments  !
    !------------!

    ! Entrées
    type (MSP_ATMOSPHERE), intent(IN) :: atm           ! Structure atmosphère
    type (tm_jour_sec), intent(IN)    :: date_js_tuc   ! Date TUC JJ 1950 (en jours/secondes)
    type (tm_geodesique), intent(IN)  :: corgeod       ! Coordonnées géodésiques du véhicule dans le repère planétographique    
    real(KIND=PM_REEL), intent(IN)    :: hsol          ! Heure solaire locale (rad)
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro             ! Densité atmosphérique
    real (KIND=PM_REEL), intent(OUT) :: vson           ! Vitesse du son
    
    !-------------------!
    ! Variables locales !
    !-------------------!
    
    type (tm_code_retour) :: code_erreur
    real (KIND=PM_REEL) :: us76_deltat(8), date_frac
    type (MSP_ATM_EXP)  :: expo
    real (KIND=PM_REEL), dimension(7) :: ap ! indices géomagnétiques
    real (KIND=PM_REEL) :: fljop, flmoy ! flux journalier, flux moyen
    real (KIND=PM_REEL) :: rtemp
    real (KIND=PM_REEL), dimension(3) :: pos_car ! Coordonnées cartésiennes
    logical, save :: warning_us76_existe_0 = .false. ! flag de test si altg < 0 km
    ! Variables pour sauvegarder les valeurs de ro et de vson à 0 km
    real (KIND=PM_REEL), save :: ro_0 = 0._pm_reel
    real (KIND=PM_REEL), save :: vson_0 = 0._pm_reel


    !---------------------!
    ! Corps de la routine !
    !---------------------!

    ! Initialisation 
    ro = 0._pm_reel
    vson = 0._pm_reel   

    ! Calcul de l'activité solaire (standard ou réelle, selon le contenu
    ! de la structure actsol)
    ! Note : l'appel est fait directement à atm%actsol par souci d'efficacité
    ! -> permet de ne pas recopier (et réallouer..) les données d'activité solaire.
    call MSP_calculer_actsol(atm%actsol, date_js_tuc, fljop, flmoy, ap)
    if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
    	   
    select case (atm%id_modele)   
	   
       case (MSP_IDATM_CIRA88_MSIS86)   
	   
          ! Modèle CIRA+MSIS86
          call cps_atm_cira_msis86 (date_js_tuc, fljop, flmoy, ap, corgeod%lat, corgeod%long, &
                                    corgeod%haut, hsol, ro, code_erreur)
          if(code_erreur%valeur < 0) then
             call MSP_signaler_message(ier_mslib=code_erreur)
             if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
          end if
       
          ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel
      
       case (MSP_IDATM_US76)
       
         ! Modèle US76D
	 
	 ! Domaine de validité du modèle : 0 km =< altitude =< 1000 km
	 ! Note FA 1411:
	 ! Le modèle COMPAS ne retourne plus de warning si l'altitude sort du domaine
	 ! Si h < 0 km alors la densité est calculée à 0 km
	 ! Si h > 1000 km alors la densité est nulle 
	 
	 if (corgeod%haut > 1000000._pm_reel) then
	    ! Mise à 0 des variables du modèle, on ne retourne pas de warning (FA 1411)
            ro = 0._pm_reel
	    vson = 0._pm_reel
         else if (corgeod%haut < 0._pm_reel) then
            if (.not.warning_us76_existe_0) then
	       ! Récupération de deltat de la structure atm%us76
	       call MSP_consulter_atmosphere(atm, us76_deltat=us76_deltat)	    
	       ! Calcul du modèle US76D, on ne retourne pas de warning (FA 1411)
               call cps_atm_us76d (us76_deltat, corgeod%haut, ro, code_erreur, &
                                   delta_dens=0._pm_reel, vit_son=vson)
	       if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return

	       if(code_erreur%valeur < 0) then
                  call MSP_signaler_message (ier_mslib=code_erreur)
                  if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
               end if

	       ! Sauvegarde des variables du modèle pour les prochains appels
	       ro_0 = ro
	       vson_0 = vson
	       ! Flag à true => on ne repassera pas dans cette branche
               warning_us76_existe_0 = .true.
            else
	       ! On ne rappelle pas le modèle COMPAS pour éviter de dégrader les performances
	       ! Les variables du modèle prennent les valeurs déjà calculées la première fois 
	       ! où l'altitude est inférieure à 0 km
	       ro = ro_0
	       vson = vson_0
	    end if  
         else
	    ! Récupération de deltat de la structure atm%us76
	    call MSP_consulter_atmosphere(atm, us76_deltat=us76_deltat)
	    ! Calcul du modèle US76D
            call cps_atm_us76d (us76_deltat, corgeod%haut, ro, code_erreur, &
                                delta_dens=0._pm_reel, vit_son=vson)
	    if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return

	    if(code_erreur%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_erreur)
               if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
            end if
         end if
	
       case (MSP_IDATM_DTM)

	  ! Modèle DTM78	  
          call cps_atm_dtm78 (date_js_tuc, fljop, flmoy, ap(3), corgeod%lat, &
               corgeod%haut, hsol, ro)
          if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return 

          ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel
	  
       case (MSP_IDATM_AT77)
       
          ! Modèle AT77
	  
          ! Passage aux coordonnées cartésiennes		  
          call mt_geod_car(corgeod, MSP_REQ_TERRE, MSP_APL_TERRE, pos_car, code_erreur)
          if (code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
          end if

	  ! Date fractionnaire
          call md_joursec_jourfrac(date_js_tuc, date_frac, code_erreur)
          if (code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
          end if

          ! Conversion m -> km
          ! car le modèle attend des km.
          pos_car(1) = pos_car(1)/1000._pm_reel
          pos_car(2) = pos_car(2)/1000._pm_reel
          pos_car(3) = pos_car(3)/1000._pm_reel

	  ! Modèle AT77
          call cps_roat77 (date_frac, pos_car, flmoy, ap(1), ro)
          if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
	  
          ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel
       
       case (MSP_IDATM_MSIS_90)
       
          ! Modèle MSIS90
       
          call cps_calculer_msis90(date_js_tuc, fljop, flmoy, ap, corgeod%lat, corgeod%long,&
                                   corgeod%haut, hsol, ro)
          if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return 

	  ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel

       case (MSP_IDATM_NRL_MSISE_2000)
       
          ! Modèle MSIS2000
       
          call cps_calculer_msis2000 (date_js_tuc, fljop, flmoy, ap,corgeod%lat, corgeod%long,&
                                      corgeod%haut, hsol, ro)
          if(MSP_gen_messages("MSP_calculer_atmosphere_terre")) return 

          ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel

       case (MSP_IDATM_MET_88)

          ! Modèle MET88

          if(corgeod%haut < 86000.0_pm_reel) then
             call MSP_signaler_message(cle_mes="MSP_ATM_DTM_01", partie_variable="Altitude < 86 km")
	     return
          end if
           
          call cps_calculer_met88 (date_js_tuc, corgeod%haut, corgeod%lat, corgeod%long, fljop, flmoy,&
                                  ap, ro, rtemp)
          if (MSP_gen_messages("MSP_calculer_atmosphere_terre")) return
	  
	  ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel
	  
       case(MSP_IDATM_EXP)
       
          ! Modèle exponentiel (pour Terre ou Vénus)
       
          ! Récupération de la structure exp
          call MSP_consulter_atmosphere(atm, exp=expo)	     
          
          ! Selon que l'on travaille avec l'exponentiel ou son inverse
          if ( expo%tscale == 1 ) then
             call cps_atm_exponentiel (corgeod%haut,expo%ro0,expo%h0,expo%hscale,&
                  expo%altmin,expo%altmax,ro)     
          else
             call cps_atm_exponentiel (corgeod%haut,expo%ro0,expo%h0,1/expo%beta,&
                  expo%altmin,expo%altmax,ro) 
          endif
	  
	  ! Pas de calcul de la vitesse du son
          vson = 0._pm_reel

       case default
          ro = 0._pm_reel
	  vson = 0._pm_reel
          call MSP_signaler_message(cle_mes="MSP_calculer_atmosphere_003",routine="MSP_calculer_atmosphere_terre")
          return

       end select
  
  end subroutine MSP_calculer_atmosphere_terre

  subroutine MSP_calculer_atmosphere_mars(atm, date_js_te, corgeod, ro, vson)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atmosphere_mars
!
!$Resume
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles martiens
!
!$Description
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles martiens
!
!$Auteur
!     G. Mercadier (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atmosphere_mars(atm, date_js_te, corgeod, ro, vson)
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_jour_sec) :: date_js_te 
!.    type (tm_geodesique) :: corgeod 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!
!$Arguments
!>E     atm         :<MSP_ATMOSPHERE>   Structure atmosphère 
!>E     date_js_te  :<tm_jour_sec>      Date TE JJ 1950 (en jours/secondes)
!>E     corgeod     :<tm_geodesique>    Coordonnées géodésiques du véhicule   
!>S     ro          :<PM_REEL>          Densité de l'atmosphère
!>S     vson        :<PM_REEL>          Vitesse du son
!
!$Common
!
!$Routines
!- MSP_consulter_atmosphere
!- cps_atmars90
!- MSP_signaler_message
!- md_joursec_jourfrac
!- mt_geod_car
!- mt_car_geoc
!- cps_height_31
!- cps_atmemcd_31
!- cps_atmemcd_42
!- cps_atmemcd_43
!
!$Include
!
!$Module
!#V
!- eph_constantes
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
    
    use eph_constantes, only : eph_date_dj1950
    
    implicit none
    
    !------------!
    ! Arguments  !
    !------------!

    ! Entrées
    type (MSP_ATMOSPHERE), intent(IN) :: atm           ! Structure atmosphère
    type (tm_jour_sec), intent(IN)    :: date_js_te    ! Date TE JJ 1950 (en jours/secondes)
    type (tm_geodesique), intent(IN)  :: corgeod       ! Coordonnées géodésiques du véhicule dans le repère planétographique    
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro             ! Densité atmosphérique
    real (KIND=PM_REEL), intent(OUT) :: vson           ! Vitesse du son
    
    !-------------------!
    ! Variables locales !
    !-------------------!
    
    type (tm_code_retour) :: code_erreur
    real (KIND=PM_REEL) :: date_frac, tjul
    type (tm_geocentrique) :: pos_geoc
    type (MSP_ATM_EMCD) :: emcd
    integer :: mars_russe_mod
    real (KIND=PM_REEL) :: alsat, rtemp, rpres, visco,seedout,ventu,ventv
    real(kind=pm_reel), dimension(5) ::meanvar
    real(kind=pm_reel), dimension(25) ::extvar
    real (KIND=PM_REEL), dimension(3) :: pos_car
    integer :: ii, ier
    integer :: iconv=1
    integer :: ikey=0
    integer :: zkey=1
    integer :: hireskey=0
    integer :: datekey=0
    integer :: extvarkey=0
    real(kind=PM_REEL) :: zareoid,zsurface,z_centre
    real(kind=PM_REEL) :: localtime=0._pm_reel
    real(kind=PM_REEL) :: invar = 16._pm_reel
    character(len=256) :: chaine
    logical ::init_atm=.true.    
    real(kind=pm_reel), dimension(100) ::extvaremcd4x
    ! Jeu de variables pour utiliser le modèle EMCD 4.2
    real(kind=4) :: ro_4, rtemp_4, ventu_4, ventv_4, seedout_4
    real(kind=4), dimension(5) :: meanvar_4
    real(kind=4), dimension(100) :: extvaremcd4x_4
    

    
    !---------------------!
    ! Corps de la routine !
    !---------------------!
    
    
    ! Initialisations
    ro = 0._pm_reel
    vson = 0._pm_reel
    zareoid = 0._pm_reel
    zsurface = 0._pm_reel

    ! Récupération de la structure emcd
    call MSP_consulter_atmosphere(atm, mars_russe_mod=mars_russe_mod, emcd=emcd)	     
  
    select case (atm%id_modele)          
 
       case (MSP_IDATM_MARS_RUSSE)
         ! Modèle Mars russe: le 3ème argument doit etre inout d'où définition de alsat = corgeod%haut
	 ! corgeod%haut étant un paramètre in
	 alsat = corgeod%haut
	 
       	 call cps_atmars90(mars_russe_mod, 1._pm_reel, alsat, rtemp, rpres, ro, vson, visco, ier)

	 if ( ier /= 0 ) then
	    call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_006",routine="MSP_calculer_atmosphere_mars")
	    return
	 endif 
     
       case(MSP_IDATM_MARS_EMCD_31)
       
         ! Modèle Mars EMCD_31
	 
	 if (corgeod%haut <= 0.3e+6_pm_reel) then
            
	    ! Date fractionnaire
	    call md_joursec_jourfrac(date_js_te, date_frac, code_erreur)	    
	    tjul = date_frac + eph_date_dj1950

            ! Coordonnées cartésiennes
            call mt_geod_car(corgeod, MSP_REQ_MARS, MSP_APL_MARS, pos_car, code_erreur)
            if (code_erreur%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_erreur)
               if (MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
            end if

	    ! Coordonnées sphériques
	    call mt_car_geoc(pos_car,pos_geoc,code_erreur)
	    if (code_erreur%valeur < 0) then
	       call MSP_signaler_message(ier_mslib=code_erreur)
	       if(MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
	    end if

	    chaine = ""
               
            ! Calcul de l'altitude au dessus de l'ellipsoide de référence
	    call cps_height_31(pos_geoc%lat,pos_geoc%long,pos_geoc%dist,zareoid,zsurface,iconv,chaine,ier)
	    if ( ier /= 0 ) then
	       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_004",routine="MSP_calculer_atmosphere_mars")
	       return
	    endif
            
	    ! Modèle EMCD_31
	    call cps_atmemcd_31(zareoid,pos_geoc%lat,pos_geoc%long,tjul,&
	                   emcd%dir_emcd,emcd%mars_emcd_sce,emcd%mars_emcd_per,invar,init_atm,&
			   seedout,ikey,rpres,ro,rtemp,ventu,ventv,meanvar,extvar,ier)  	 
		 
	    if ( ier /= 0 ) then
	       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_003",routine="cps_atmemcd_31 - MSP_calculer_atmosphere_mars")
	       return
	    endif
	 else
	    vson = 0._pm_reel
	    ro   = 0._pm_reel
	 endif
              
       case(MSP_IDATM_MARS_EMCD_42)
       
         ! Modèle Mars EMCD_42
	 
       	 if (corgeod%haut <= 0.3e+6_pm_reel) then

            ! Date fractionnaire
	    call md_joursec_jourfrac(date_js_te, date_frac, code_erreur)
	    tjul = date_frac + eph_date_dj1950

            ! Coordonnées cartésiennes
            call mt_geod_car(corgeod, MSP_REQ_MARS, MSP_APL_MARS, pos_car, code_erreur)
            if (code_erreur%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_erreur)
               if (MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
            end if

	    ! Coordonnées sphériques
	    call mt_car_geoc(pos_car,pos_geoc,code_erreur)
	    if (code_erreur%valeur < 0) then
	       call MSP_signaler_message(ier_mslib=code_erreur)
	       if(MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
	    end if

	    z_centre = sqrt((pos_car(1))**2 + (pos_car(2))**2 + (pos_car(3))**2)

	    ! Initialisations des variables locales avant l'appel au modèle
	    extvaremcd4x_4(:) = 0. 
	    ro_4 = 0.
	    rtemp_4 = 0.
	    ventu_4 = 0. 
	    ventv_4 = 0.
	    seedout_4 = 0.
	    meanvar_4 = 0.
        extvarkey = 0   ! Désactivation du calcul des vars supplémentaires
        ier = 0
        ! Lancement du calcul et conversion de la longueur d'onde de km en m
	    call cps_atmemcd_42(zkey,real(z_centre),real(pos_geoc%long),real(pos_geoc%lat),hireskey, &
               datekey,tjul,real(localtime),emcd%dir_emcd,emcd%mars_emcd_sce, &
               int(emcd%mars_emcd_per(1)), real(emcd%mars_emcd_per(2)), &
               real(emcd%lambda_gw) * 1000.0, extvarkey, &
               real(rpres), ro_4, rtemp_4, ventu_4, ventv_4, &
               meanvar_4, extvaremcd4x_4,seedout_4,ier)	  

	    if ( ier /= 0 ) then
	       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_007",routine="cps_atmemcd_42 - MSP_calculer_atmosphere_mars")
	       return
	    endif
		 		 
	    ! Affectation des variables de sorties (réels 8 = pm_reel) avec les valeurs sorties par le modèle (réels 4)
	    ro       = ro_4
	    rtemp    = rtemp_4
	    ventu    = ventu_4
	    ventv    = ventv_4
	    meanvar  = meanvar_4
	    extvaremcd4x = extvaremcd4x_4
	    seedout  = seedout_4

            do ii=1,25
               extvar(ii)=extvaremcd4x(ii)
            end do
	             
	 else
	    vson = 0._pm_reel
	    ro   = 0._pm_reel     
         end if
       
       case(MSP_IDATM_MARS_EMCD_43)
       
         ! Modèle Mars EMCD_43
	 
         if (corgeod%haut <= 0.3e+6_pm_reel) then

            ! Date fractionnaire
	    call md_joursec_jourfrac(date_js_te, date_frac, code_erreur)
	    tjul = date_frac + eph_date_dj1950

            ! Coordonnées cartésiennes 
            call mt_geod_car(corgeod, MSP_REQ_MARS, MSP_APL_MARS, pos_car, code_erreur)
            if (code_erreur%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_erreur)
               if (MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
            end if

	    z_centre = sqrt((pos_car(1))**2 + (pos_car(2))**2 + (pos_car(3))**2)
	    
	    ! Coordonnées sphériques
	    call mt_car_geoc(pos_car,pos_geoc,code_erreur)
	    if (code_erreur%valeur < 0) then
	       call MSP_signaler_message(ier_mslib=code_erreur)
	       if(MSP_gen_messages("MSP_calculer_atmosphere_mars")) return
	    end if
	    
        hireskey=1           ! Flag de topographie fixé en haute résolution
        extvarkey=0          ! Flag indiquant si des valeurs suppl doivent etre calculées
	    extvaremcd4x(:) = 0._pm_reel  ! Init des vars supplémentaires
	    	    
        ! Contrairement au EMCD 4.2 pas besoin de passer par des variables
        ! de type real intermédiaire
        ! Note : La longueur d'onde lambda_gw extvarkeyest attendue en m et est stockée en km dans MSP_ATM_EMCD
        ! mars_emcd_per(1) 1: none
        !             2: large scale : 'seedin' is seed or signals reseting the
        !                EOF perturbations if changed between calls to CALL_MCD
        !             3: small scale : 'seedin' is seed or signals reseting the
        !                GW perturbations if changed between calls to CALL_MCD
        !             4: large and small scale : does both 2 and 3 above
        !             5: add 'seedin' times the standard deviation
        !                (seedin must not be greater than 4 or less than -4)

        call cps_atmemcd_43(zkey,z_centre,pos_geoc%long,pos_geoc%lat,hireskey, &
                 datekey,tjul,localtime,emcd%dir_emcd,emcd%mars_emcd_sce, &
		         int(emcd%mars_emcd_per(1)), emcd%mars_emcd_per(2), emcd%lambda_gw * 1000._pm_reel,extvarkey, &
		         rpres,ro,rtemp,ventu,ventv,&
		         meanvar,extvaremcd4x,seedout,ier)

	    if ( ier /= 0 ) then
	       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_008",routine="cps_atmemcd_43 - MSP_calculer_atmosphere_mars")
	       return
	    endif    
	 else
	    vson = 0._pm_reel
	    ro   = 0._pm_reel     
         end if

       
       case default
          ro = 0._pm_reel
	  vson = 0._pm_reel
          call MSP_signaler_message(cle_mes="MSP_calculer_atmosphere_003",routine="MSP_calculer_atmosphere_mars")
          return

       end select

  end subroutine MSP_calculer_atmosphere_mars          


  subroutine MSP_calculer_atmosphere_venus(atm, corgeod, ro, vson)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_atmosphere_venus
!
!$Resume
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles vénusiens
!
!$Description
!     Routine de calcul de la densité de l'atmosphère et de la vitesse du son
!     pour les modèles vénusiens
!
!$Auteur
!    G. Mercadier (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_atmosphere_venus(atm, corgeod, ro, vson)
!.    type (MSP_ATMOSPHERE) :: atm 
!.    type (tm_geodesique) :: corgeod 
!.    real (KIND=PM_REEL) :: ro 
!.    real (KIND=PM_REEL) :: vson 
!
!$Arguments
!>E     atm      :<MSP_ATMOSPHERE>   Structure atmosphère  
!>E     corgeod  :<tm_geodesique>    Coordonnées géodésiques du véhicule
!>S     ro       :<PM_REEL>          Densité de l'atmosphère
!>S     vson     :<PM_REEL>          Vitesse du son
!
!$Common
!
!$Routines
!- MSP_consulter_atmosphere
!- cps_atm_venus
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

    !------------!
    ! Arguments  !
    !------------!

    ! Entrées
    type (MSP_ATMOSPHERE), intent(IN) :: atm           ! Structure atmosphère
    type (tm_geodesique), intent(IN)  :: corgeod       ! Coordonnées géodésiques du véhicule dans le repère planétographique    
    ! Sorties
    real (KIND=PM_REEL), intent(OUT) :: ro             ! Densité atmosphérique
    real (KIND=PM_REEL), intent(OUT) :: vson           ! Vitesse du son
    
    !-------------------!
    ! Variables locales !
    !-------------------!
    real (KIND=PM_REEL) :: ro0, beta

    !---------------------!
    ! Corps de la routine !
    !---------------------!
    
    ! Initialisations
    ro = 0._pm_reel
    vson = 0._pm_reel   
      	     
       
    select case (atm%id_modele)    
       
       case(MSP_IDATM_VENUS)
       
          ! Modèle Vénus Petropoulos88

          if ( corgeod%haut >= 0._pm_reel ) then
	     ! Récupération de ro0 et beta dans la structure atm
	     call MSP_consulter_atmosphere(atm, venus_rho0=ro0, venus_beta=beta)
             call cps_atm_venus (corgeod%haut, ro, ro0, beta)
	     
	     ! Pas de calcul de la vitesse du son
	     vson = 0._pm_reel

             if (MSP_gen_messages("MSP_calculer_atmosphere_venus")) return
	  else
             call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_001", Routine="MSP_calculer_atmosphere_venus")
             return
          endif	  
	  
       case default
          ro = 0._pm_reel
	  vson = 0._pm_reel
          call MSP_signaler_message(cle_mes="MSP_calculer_atmosphere_003",routine="MSP_calculer_atmosphere_venus")
          return

       end select

  end subroutine MSP_calculer_atmosphere_venus          


  subroutine MSP_calculer_frottement(atm,vehicule,date,corgeod,hsol,vitr,mat_att_veh,incidence,&
             acc_frot,ro,mach,xcd,xcl,vit_vent,modatt,panneaux,mat_att_panneaux,vit_atm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_frottement
!
!$Resume
!  Calcul d'une force de frottement atmosphérique, tenant compte d'un modèle 
!  d'atmosphère, d'un modèle de vent, de l'attitude du véhicule et de sa forme.
!
!$Description
!  Calcul d'une force de frottement atmosphérique, tenant compte d'un modèle 
!  d'atmosphère, d'un modèle de vent, de l'attitude du véhicule et de sa forme.
!  Cette fonction n'effectue aucun changement de repère.
!  Le vecteur position (en géodésique) doit être exprimé dans un repère planétographique.
!  Le vecteur accélération sera exprimé dans le repère d'application de la force.
!  Les vecteurs vitesse du vent, vitesse relative et vitesse par rapport à l'atmosphère seront 
!  exprimés dans le repère d'application de la force.
!
!$Auteur
!  G. MERCADIER (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_frottement(atm,vehicule,date,corgeod,hsol,vitr,mat_att_veh,incidence,&
!.                 acc_frot,ro,mach,xcd,xcl,[vit_vent],[modatt],[panneaux],[mat_att_panneaux],[vit_atm])
!.    type (MSP_ATMOSPHERE) :: atm
!.    type (MSP_VEHICULE) :: vehicule
!.    type(tm_jour_sec) :: date
!.    type (tm_geodesique) :: corgeod 
!.    real(kind=pm_reel) :: hsol 
!.    real(kind=pm_reel) :: incidence
!.    real(kind=pm_reel), dimension(3) :: vitr 
!.    real(kind=pm_reel), dimension(3,3) :: mat_att_veh 
!.    real(kind=pm_reel), dimension(3) :: acc_frot
!.    real(kind=pm_reel) :: ro
!.    real(kind=pm_reel) :: mach
!.    real(kind=pm_reel) :: xcd
!.    real(kind=pm_reel) :: xcl
!.    integer :: modatt
!.    integer :: panneaux
!.    real(kind=pm_reel), dimension(3,3) :: mat_att_panneaux
!.    real(kind=pm_reel), dimension(3) :: vit_vent
!.    real(kind=pm_reel), dimension(3) :: vit_atm
!
!$Arguments
!>E     atm               :<MSP_ATMOSPHERE>      Modèle d'atmosphère (structure)
!>E     vehicule          :<MSP_VEHICULE>        Structure véhicule 
!>E     date              :<tm_jour_sec>         Date courante en j1950 jj/s (TE) 
!>E     corgeod           :<tm_geodesique>       Coordonnées géodésiques du véhicule dans le repère planétographique
!>E     hsol              :<pm_reel>             Heure solaire locale dans le repère planétographique     
!>E     vitr              :<pm_reel,DIM=(3)>     Vitesse relative du véhicule dans le repère d'application de la force
!>E     mat_att_veh       :<pm_reel,DIM=(3,3)>   Matrice d'attitude "véhicule" dans le repère d'application de la force 
!>E     incidence         :<pm_reel>             Incidence      
!>S     acc_frot          :<pm_reel,DIM=(3)>     Force de frottement atmosphérique
!>S     ro                :<pm_reel>             Densité atmosphérique
!>S     mach              :<pm_reel>             Nombre de Mach
!>S     xcd               :<pm_reel>             Coefficient de trainée
!>S     xcl               :<pm_reel>             Coefficient de portance
!>[E]   vit_vent          :<pm_reel,DIM=(3)>     Vitesse du vent dans le repère d'application de la force
!>[E]   modatt            :<integer>             Présence ou non d'une loi d'attitude "véhicule"
!>[E]   panneaux          :<integer>             Présence ou non d'une loi d'attitude "panneaux"
!>[E]   mat_att_panneaux  :<pm_reel,DIM=(3,3)>   Matrice d'attitude "panneaux" dans le repère d'application de la force
!>[S]   vit_atm           :<pm_reel,DIM=(3)>     Vitesse par rapport à l'atmosphère
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- MSP_signaler_message
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_consulter_aero
!- MSP_calculer_atmosphere
!- MSP_calculer_coefs_aero
!- MSP_calcul_surf_app
!- MSP_acc_frot
!- mu_norme
!- MSP_effacer_mci
!
!$Include
!
!$Module
!#V
!- MSP_VEHICULE_DEF
!- MSP_MCI_DEF
!- MSP_AERO_DEF
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

    use MSP_VEHICULE_DEF
    use MSP_MCI_DEF
    use MSP_AERO_DEF

 
    implicit none
    
    ! Déclaration des arguments
 
    type (MSP_ATMOSPHERE), intent(IN)                          :: atm
    type (MSP_VEHICULE), intent(IN)                            :: vehicule
    type(tm_jour_sec), intent(IN)                              :: date
    type (tm_geodesique), intent(IN)                           :: corgeod 
    real(kind=pm_reel), intent(IN)                             :: hsol   
    real(kind=pm_reel), intent(IN)                             :: incidence
    real(kind=pm_reel), dimension(3), intent(IN)               :: vitr         
    real(kind=pm_reel), dimension(3,3), intent(IN)             :: mat_att_veh             
    real(kind=pm_reel), intent(OUT), dimension(3)              :: acc_frot
    real(kind=pm_reel), intent(OUT)                            :: ro
    real(kind=pm_reel), intent(OUT)                            :: mach
    real(kind=pm_reel), intent(OUT)                            :: xcd
    real(kind=pm_reel), intent(OUT)                            :: xcl
    integer, intent(IN), optional                              :: modatt
    integer, intent(IN), optional                              :: panneaux
    real(kind=pm_reel), dimension(3,3), intent(IN), optional   :: mat_att_panneaux
    real(kind=pm_reel), dimension(3), intent(IN), optional     :: vit_vent
    real(kind=pm_reel), dimension(3), intent(OUT), optional    :: vit_atm
        
    
    ! Autres déclarations
    
    integer                          :: modatt_util, panneaux_util, i
 
    type (tm_code_retour)            :: code_retour
      
    integer                          :: forme
    real(kind=pm_reel)               :: sx, sy, sz, st, spx, spy, spz ,masse
    real(kind=pm_reel)               :: cmf, surf_app, surf_app_ps
    real(kind=pm_reel), dimension(3) :: vitr_mod, vrveh, axe_port, dir_port, dirn_port
    real(kind=pm_reel), dimension(3) :: acc_portance, acc_trainee, acc_trainee_ps
    real(kind=pm_reel)               :: vson, vpair, norme
    real(kind=pm_reel)               :: coef_portance, coef_trainee
    type(MSP_AERO)                   :: veh_aero
    type(MSP_MCI)                    :: veh_mci

    ! Initialisation de la structure AERO : mise à nul des pointeurs
    ! pour éviter les fuites mémoires
    call MSP_effacer_aero(veh_aero,nul=.true.)
    if (MSP_gen_messages("MSP_calculer_frottement")) return
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Controle de la cohérence des arguments !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    if (present(modatt)) then
       modatt_util = modatt
       if (.not.((modatt_util == 1).or.(modatt_util == 0))) then
          ! Erreur, code non accepté
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                                     partie_variable='Code inconnu pour modatt', &
                                     routine="MSP_calculer_frottement",type=MSP_ENUM_ERREUR)
          return
       end if
    else
       ! Par défaut, pas de loi d'attitude
       modatt_util = 0
    end if
    
    if (present(panneaux)) then
       panneaux_util = panneaux
       if (panneaux_util == 1) then
          ! On prend en compte les panneaux solaires
          ! On a besoin de la loi d'attitude des panneaux solaires
          if (.not.present(mat_att_panneaux)) then
             call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS",&
                                        partie_variable='Panneaux solaires demandés, mais manque argument', &
                                        routine="MSP_calculer_frottement",type=MSP_ENUM_ERREUR)
             return
          end if
       else if (panneaux_util == 0) then
          ! On ne prend pas en compte les panneaux solaires
          if (present(mat_att_panneaux)) then
             call MSP_signaler_message (cle_mes="MSP_WARN_ARGUMENTS", &
                                        partie_variable='Argument de panneaux solaires fourni mais ignoré', &
                                        routine="MSP_calculer_frottement",type=MSP_ENUM_WARNING)
          end if
       else
          ! Erreur, code non accepté
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                                     partie_variable='Code inconnu pour panneaux', &
                                     routine="MSP_calculer_frottement",type=MSP_ENUM_ERREUR)
          return
       end if
    else
       ! Par défaut , pas de panneaux solaires
       panneaux_util = 0
       if (present(mat_att_panneaux)) then
          call MSP_signaler_message (cle_mes="MSP_WARN_ARGUMENTS", &
                                     partie_variable='Argument de panneaux solaires fourni mais ignoré', &
                                     routine="MSP_calculer_frottement",type=MSP_ENUM_WARNING)
       end if
    end if
    
    
    !!!!!!!!!!!!!!!!!!!
    ! Initialisations !
    !!!!!!!!!!!!!!!!!!!
    
    do i=1,3
       acc_frot(i) = 0._pm_reel    
    end do
    
    ro   = 0._pm_reel
    mach = 0._pm_reel
        
    if (present(vit_atm)) then
       vit_atm(1) = 0._pm_reel
       vit_atm(2) = 0._pm_reel
       vit_atm(3) = 0._pm_reel
    end if
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Initialisation des paramètres véhicule !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! Structure véhicule (géométrie + coefficients aérodynamiques)
    call MSP_consulter_vehicule(vehicule,mci=veh_mci,aero=veh_aero)
    if ( MSP_gen_messages("MSP_calculer_frottement") ) return
    
    ! Géométrie du véhicule
    call MSP_consulter_mci(veh_mci, forme=forme, sx=sx, sy=sy, sz=sz, st=st, spx=spx, spy=spy, spz=spz, mtot=masse)
    if ( MSP_gen_messages("MSP_calculer_frottement") ) return
    
    ! Coefficients aérodynamiques
    call MSP_consulter_aero(veh_aero, cmf=cmf)			       
    if (MSP_gen_messages("MSP_calculer_frottement") ) return
    

    !!!!!!!!!!!!!!!!!!!!!!!!
    ! Modèle d'atmosphère  !
    !!!!!!!!!!!!!!!!!!!!!!!!
    
    call MSP_calculer_atmosphere(atm, date, corgeod, hsol, ro, vson)   
    if(MSP_gen_messages("MSP_calculer_frottement")) return
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Correction de la vitesse relative du véhicule !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

    ! Prise en compte de la vitesse du vent
    if (present(vit_vent)) then 
       vitr_mod(1) = vitr(1) - vit_vent(1)
       vitr_mod(2) = vitr(2) - vit_vent(2)
       vitr_mod(3) = vitr(3) - vit_vent(3)
    else
       vitr_mod(1) = vitr(1)
       vitr_mod(2) = vitr(2)
       vitr_mod(3) = vitr(3)
    end if
    
    ! Vitesse par rapport à l'air
    vpair = sqrt(vitr_mod(1)**2+vitr_mod(2)**2+vitr_mod(3)**2)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul du nombre de Mach : vpair / vson !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ( vson > MSP_EPSILON ) then
       mach = vpair/vson
    else
       mach = 50._pm_reel
    endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul de la vitesse par rapport à l'atmosphère !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (present(vit_atm)) then
       vit_atm(1) = vitr_mod(1)
       vit_atm(2) = vitr_mod(2)
       vit_atm(3) = vitr_mod(3)
    end if
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul des coefficients aérodynamiques du véhicule xcd et xcl !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call MSP_calculer_coefs_aero(veh_aero, xcd, xcl, corgeod%haut, incidence, mach)
    if ( MSP_gen_messages ("MSP_calculer_frottement") ) return  
        
	   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul  de la force de trainee !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    
    ! Calcul du coefficient de trainee
    ! Contrôle que la masse > epsilon*surface 
    !   Si la masse est très faible, il faut arrêter le calcul qui n'a plus de sens. 
    !   Si la masse devient très faible sans que l'on quitte, des calculs de forces sont faussés.
    if (masse > (MSP_EPSILON*st)) then
       coef_trainee = (0.5_pm_reel/masse)*ro*(vpair**2)*xcd*cmf
    else
       call MSP_signaler_message (cle_mes="MSP_MASSE_PRAD",routine="MSP_calculer_frottement")
       return
    endif  

    ! Calcul de la surface d'application pour le corps du véhicule  
    call MSP_calcul_surf_app(forme, sx, sy, sz, st, modatt_util, mat_att_veh, vitr_mod, surf_app)
    
    ! Calcul de la force de trainee pour le corps du véhicule
    call MSP_acc_frot(surf_app, vitr_mod, coef_trainee, acc_trainee)
    
    
    if ( MSP_gen_messages ("MSP_calculer_frottement") ) return 
    
    ! Traitement des panneaux solaires
    if ( panneaux_util == 1 ) then
       
       ! Calcul de la surface d'application pour les panneaux solaires          
       ! Remarque : on passe le flag 'modatt_util' à cette routine, afin
       ! de prendre en compte la surface transverse si il n'y a pas de loi
       ! d'attitude définie.
       call MSP_calcul_surf_app (MSP_ENUM_PARALLEPIPEDE, spx, spy, spz, st, modatt_util,&
                                 mat_att_panneaux, vitr_mod, surf_app_ps)
       
       ! Calcul de la force de trainee pour les panneaux solaires
       call MSP_acc_frot(surf_app_ps, vitr_mod, coef_trainee, acc_trainee_ps)
       if ( MSP_gen_messages ("MSP_calculer_frottement") ) return 
    
       acc_trainee(1) = acc_trainee(1) + acc_trainee_ps(1)
       acc_trainee(2) = acc_trainee(2) + acc_trainee_ps(2)
       acc_trainee(3) = acc_trainee(3) + acc_trainee_ps(3)

       surf_app = surf_app + surf_app_ps

    end if
       
        
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul de la force de portance !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
    ! Vitesse relative exprimée dans le repère véhicule
    if (modatt_util == 1) then
       vrveh(1) = mat_att_veh(1,1)*vitr_mod(1) + mat_att_veh(1,2)*vitr_mod(2) + mat_att_veh(1,3)*vitr_mod(3)
       vrveh(2) = mat_att_veh(2,1)*vitr_mod(1) + mat_att_veh(2,2)*vitr_mod(2) + mat_att_veh(2,3)*vitr_mod(3)
       vrveh(3) = mat_att_veh(3,1)*vitr_mod(1) + mat_att_veh(3,2)*vitr_mod(2) + mat_att_veh(3,3)*vitr_mod(3)
    else
       vrveh(1) = vitr_mod(1)
       vrveh(2) = vitr_mod(2)
       vrveh(3) = vitr_mod(3)  
    end if
    
    ! Axe de la portance exprimé dans le repère véhicule
    axe_port(1) = -vrveh(3)
    axe_port(2) = 0._pm_reel
    axe_port(3) = vrveh(1)

    ! Axe de la portance exprimé dans le repère d'application
    
    if (modatt_util == 1) then
       ! Direction de la portance si loi d'attitude
       dir_port(1) = mat_att_veh(1,1)*axe_port(1) + mat_att_veh(2,1)*axe_port(2) + mat_att_veh(3,1)*axe_port(3)
       dir_port(2) = mat_att_veh(1,2)*axe_port(1) + mat_att_veh(2,2)*axe_port(2) + mat_att_veh(3,2)*axe_port(3)
       dir_port(3) = mat_att_veh(1,3)*axe_port(1) + mat_att_veh(2,3)*axe_port(2) + mat_att_veh(3,3)*axe_port(3)
    else
       ! Direction de la portance si pas de loi d'attitude
       dir_port(1) = axe_port(1)
       dir_port(2) = axe_port(2)
       dir_port(3) = axe_port(3)
    end if

    ! Normalisation de l'axe de la portance
    if ( (dir_port(1)**2+dir_port(2)**2+dir_port(3)**2) <= MSP_EPSILON ) then
       norme = 0._pm_reel
       dirn_port(1) = 0._pm_reel
       dirn_port(2) = 0._pm_reel
       dirn_port(3) = 0._pm_reel
    else
       call mu_norme (dir_port, norme, code_retour, dirn_port)
       if (code_retour%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_retour)
          if (MSP_gen_messages("MSP_calculer_frottement" )) return
       end if
    endif

    ! Calcul du coefficient de portance
    
    ! Contrôle que la masse > epsilon*surface 
    !   Si la masse est très faible, il faut arrêter le calcul qui n'a plus de sens. 
    !   Si la masse devient très faible sans que l'on quitte, des calculs de forces sont faussés. 
    if (masse > (MSP_EPSILON*st)) then
       coef_portance = (0.5_pm_reel/masse)*ro*(vpair**2)*xcl*cmf*surf_app 
    else
       call MSP_signaler_message (cle_mes="MSP_MASSE_PRAD",routine="MSP_calculer_frottement")
       return
    endif

    ! Calcul de la force de portance
    do i=1,3
       acc_portance(i) = coef_portance*dirn_port(i)
    end do     
     
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Calcul de l'accélération totale !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    acc_frot(1) = acc_trainee(1) + acc_portance(1)
    acc_frot(2) = acc_trainee(2) + acc_portance(2)
    acc_frot(3) = acc_trainee(3) + acc_portance(3)
        
    ! Désallocation mémoire
    call MSP_effacer_aero(veh_aero)
    if (MSP_gen_messages("MSP_calculer_frottement")) return
    
    call MSP_effacer_mci(veh_mci)
    if (MSP_gen_messages("MSP_calculer_frottement")) return
    
  end subroutine MSP_calculer_frottement

  subroutine MSP_acc_frot(surf_app,dir_app,coef,acc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_acc_frot
!
!$Resume
!  Calcul d'une force de surface
!
!$Description
!  Calcul d'une force de surface
!
!$Auteur
!  G. MERCADIER (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_acc_frot(surf_app,dir_app,coef,acc)
!.    real(kind=pm_reel) :: surf_app
!.    real(kind=pm_reel), dimension(3) :: dir_app
!.    real(kind=pm_reel) :: coef
!.    real(kind=pm_reel), dimension(3) :: acc
!
!$Arguments
!>E     surf_app  :<pm_reel>           Surface d'application de la force (~ m²)
!>E     dir_app   :<pm_reel,DIM=(3)>   Direction d'application de la force
!>E     coef      :<pm_reel>           Coefficient de frottement ( 1/(m*s²))
!>S     acc       :<pm_reel,DIM=(3)>   Accélération (m/s²)
!
!$Common
!
!$Routines
!- mu_norme
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

    ! Déclaration des arguments
    real(kind=pm_reel),              intent(IN) :: surf_app
    real(kind=pm_reel), dimension(3),intent(IN) :: dir_app
    real(kind=pm_reel),              intent(IN) :: coef
    real(kind=pm_reel), dimension(3),intent(OUT):: acc


    ! Autres déclarations
    real(kind=pm_reel), dimension(3) :: dir_app_n
    real(kind=pm_reel)               :: norme
    type (tm_code_retour)            :: code_retour
    
    
    ! Initialisation
    acc(1) = 0._pm_reel
    acc(2) = 0._pm_reel
    acc(3) = 0._pm_reel
    
    ! Norme du vecteur directeur
    call mu_norme (dir_app, norme, code_retour, dir_app_n)
    if (code_retour%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_retour)
       if (MSP_gen_messages("MSP_acc_frot" )) return
    end if

    ! Force d'absorption : sens opposé de la direction d'application
    acc(1) = -coef*surf_app*dir_app_n(1)
    acc(2) = -coef*surf_app*dir_app_n(2)
    acc(3) = -coef*surf_app*dir_app_n(3)
    
  end subroutine MSP_acc_frot


  end module MSP_ATMOSPHERE_DEF

