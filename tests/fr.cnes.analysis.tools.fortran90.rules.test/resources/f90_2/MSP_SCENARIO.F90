module MSP_SCENARIO

!*******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_SCENARIO
!
!$Resume
!  Module gérant des scénarios.
!
!$Description
!  Module gérant des scénarios.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_SCENARIO.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_SCENARIO.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.49  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.48  2009/10/28 17:16:19  mercadig
!  DM-ID 1018: Mise a jour cartouche
!
!  Revision 1.47  2008/11/19 13:33:59  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.46  2008/10/24 08:01:55  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.45  2008/10/06 07:51:48  huec
!  AQ : Suppression de traces de travail
!  Revision 1.44  2008/08/08 15:03:13  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration. Suppression des variables non utlisées. Suppression de l'attribut
!  target des variables loi en entrée des fonctions ajouter impulsion et separation.
!  Revision 1.43  2008/07/04 14:56:58  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.42  2008/04/24 13:59:07  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.41  2007/11/15 17:14:15  huec
!  FA-ID 839 : Variables declarees deux fois dans des fonctions
!  Revision 1.38  2007/10/31 16:54:55  huec
!  *** empty log message ***
!  Revision 1.37  2007/10/31 14:16:36  tanguyy
!  FA-ID 823 : nouvelle routine pour effacer un scenario ou l initialiser proprement
!  Revision 1.36  2007/06/18 10:44:13  vivaresf
!  FA-ID 748 : suppression des dimensions en clair, écriture des chaines avec trim
!  Revision 1.35  2007/06/15 14:26:38  vivaresf
!  FA-ID 746 mise en INOUT des loi OUT pour permettre la désallocation correcte de celles-ci (cas lois tabulées et séparations)
!  Revision 1.34  2007/05/21 16:22:42  vivaresf
!  Version 4.4 : cartouches
!  Revision 1.33  2007/02/06 12:18:27  vivaresf
!  FA-ID 694 : positionnement de loi_courante si en tete de liste
!  Revision 1.32  2007/02/06 11:26:04  vivaresf
!  FA-ID 678 : nullify pour éviter les débordement dans l'affectation
!  Revision 1.31  2007/02/02 10:36:10  vivaresf
!  Version 4.4a1 : variables inutilisées
!  Revision 1.30  2007/02/02 08:25:23  vivaresf
!  FA-ID 694 : positionnement de la loi courante
!  Revision 1.29  2006/11/15 16:38:57  tanguyy
!  MECASPA V4-3
!  Revision 1.28  2006/11/15 09:58:08  tanguyy
!  DM-ID 552 : rajout d'un champ pour distinguer les types d'angles (Cardan, Euler..)
!  Revision 1.27  2006/10/11 08:05:37  tanguyy
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.26.2.2  2006/10/11 07:58:56  tanguyy
!  AQ : MAJ des commentaires
!  Revision 1.26.2.1  2006/09/25 15:13:38  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.26  2006/06/02 11:21:54  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!  Revision 1.25  2006/04/21 08:11:29  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.24.2.2  2006/04/21 08:10:58  tanguyy
!  DM-ID 400. Validation
!  Revision 1.24.2.1  2006/04/20 13:51:07  tanguyy
!  DM-ID 400 : routines permettant d'optimiser l'utilisation du pointeur loi courante
!  Revision 1.24  2005/03/08 07:32:37  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.23  2005/02/01 08:59:15  fabrec
!  DM-ID 235 : typcf
!  Revision 1.22  2005/01/20 13:56:48  pauh
!  FA_332
!  Revision 1.21.2.1  2005/01/20 13:50:16  pauh
!  FA 332 : Appels DEALLOCATE avec l argument stat=MSP_iostat
!  Revision 1.21  2005/01/17 15:16:54  fabrec
!  DM-ID 175 : rajout des debits
!  Revision 1.20  2005/01/10 12:47:58  vivaresf
!  FA_321
!  Revision 1.19.2.3  2005/01/07 14:21:00  vivaresf
!  Effacement
!  Revision 1.19.2.2  2005/01/07 10:50:33  vivaresf
!  FA-ID 321 : effacement propre des lois de propulsion continue, d'attitude tabulee ou de separation avant re_affectation
!  Revision 1.19.2.1  2005/01/07 10:36:18  vivaresf
!  FA_321, version initiale
!  Revision 1.19  2005/01/07 10:21:48  vivaresf
!  DM-ID 175 : routine MSP_loi_precedente pour circuler dans les scenarios
!  Revision 1.18.2.2  2005/01/07 10:21:31  vivaresf
!  Effacement des structures avec pointeur
!  Revision 1.18.2.1  2004/12/10 15:41:39  vivaresf
!  DM_175, version initiale
!  Revision 1.18  2004/11/05 16:22:21  vivaresf
!  Coquille dans le nom du message d'erreur
!  Revision 1.17  2004/06/22 09:32:25  vivaresf
!  DM_133
!  Revision 1.14.2.4  2004/06/22 09:31:47  vivaresf
!  DM-ID 133, MSP_ajouter_separation,
!  rajout du type de masse (tymasse), masse absolue ou delta
!  Revision 1.14.2.3  2004/06/17 11:04:46  vivaresf
!  DM-ID 133 : refonte des if/elseif pour na'voir qu'un cas d'erreur
!  (sinon résultats non reproductibles apres la DM 133)
!  Revision 1.14.2.2  2004/06/08 10:42:21  vivaresf
!  Validation DM 133
!  Revision 1.14.2.1  2004/06/08 10:27:47  vivaresf
!  DM-ID 133 : rajout de structures pard et aero dans la separation,
!  transfert de `origdat' si absent de la definition d'une loi
!  affichage de la date de reference
!  Revision 1.14  2004/05/06 09:44:18  vivaresf
!  DM-ID 83 Traitement des dates en jour/secondes
!  Origine des dates a MJD1950 ou MJD2000
!  Loi avec type de date absolues, relatives ou absolues/relatives
!  Revision 1.13.2.1  2004/05/03 14:28:37  vivaresf
!  DM_83, version initiale
!  Revision 1.13  2003/07/10 10:18:55  adm_ipsi
!  FA-ID 23 : Initialisation des ptr_loi%flag_func à .false. dans les routines MSP_ajouter_
!  Revision 1.11  2003/03/27 16:44:09  util_am
!  SLB - Version industrialisée
!  Revision 1.12  2003/02/25 15:23:06  adm_ipsi
!  MSP_type_loi, utilisation de MSP_gen_messages
!  Revision 1.11  2003/02/25 15:18:18  adm_ipsi
!  egaler_scenario, recopie de loi_courante
!  Revision 1.10  2003/02/10 17:31:18  adm_ipsi
!  Ajout d'un message d'erreur dans MSP_type_loi
!  Revision 1.9  2003/02/10 17:24:55  adm_ipsi
!  Utilisation de MSP_gen_messages() dans les traitements d'erreur
!  Revision 1.8  2003/01/07 18:11:40  adm_ipsi
!   suppression des variables non utilisées
!  Revision 1.7  2002/12/11 10:18:10  adm_ipsi
!  Ajout du traitement par défaut
!  Revision 1.6  2002/12/09 16:45:37  adm_ipsi
!  Utilisation du parametre MSP_ENUM_ECRAN pour les sorties ecran
!  Revision 1.5  2002/12/05 18:09:38  adm_ipsi
!  Changement de nom des champs type dans les structures MSP_LOI et MSP_SCENARIO_LOI
!  Revision 1.4  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/13 15:00:55  adm_ipsi
!  Gestion du pointeur loi_courante : positionné sur la loi insérée ou consultée.
!  En cas de suppresion de loi, positionnement sur la loi précédente
!  Revision 1.1.1.1  2002/09/30 14:09:36  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.10  2002/09/16 11:12:37  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.9  2002/05/03 07:53:25  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!  Revision 1.8  2000/08/31 15:13:33  util_am
!  - Ajout de la routine privée MSP_consulter_loi permettant de lire le contenu d'une structure MSP_LOI.
!  Elle est accessible via l'interface MSP_consulter_scenario
!  - Ajout de l'argument optionnel loi à MSP_premiere_loi, ce qui permet de récupérer dans un pointeur la première loi d'un scenario
!  - Transformation de la routine MSP_loi_suivante en une interface de deux routines privées MSP_loi_suivante_scenar et MSP_loi_suivante_loi.
!  La première permet d'incrémenter le pointeur courant d'un scenario tandis que la seconde permet de récupérer le pointeur courant.
!  Revision 1.7  2000/07/24 16:23:50  util_am
!  Ajout de la routine MSP_modifier_scenario_gen accessible via l'interface MSP_modifier_scenario.
!  Revision 1.6  2000/07/04 14:00:17  util_am
!  Ajout de test de gestion de l'erreur là où des routines de MECASPA sont utilisées.
!  Revision 1.5  2000/06/15 08:38:32  util_am
!  - Ajout du champ flag_func dans les structures MSP_LOI et MSP_SCENARIO_LOI pour la gestion des fuites mémoires
!  - Privatisation du contenu des structures MSP_LOI et MSP_SCENARIO_LOI
!  - Ajout de la MSP_afficher_scenario, MSP_egaler_scenario, MSP_egaler_loi
!  - Transfert des routines MSP_ajouter_* depuis :
!          MSP_ATTITUDE_SPIN.F90, MSP_ATTITUDE_TAB.F90, MSP_PROPULSION_BUL.F90,
!          MSP_PROPULSION_CONT.F90, MSP_PROPULSION_IMP.F90, MSP_SEPARATION_IMP.F90
!  - Transfert des routines du contenu de MSP_SCENARIO_DEF.F90 dans MSP_SCENARIO.F90
!  - Ajout d'interfaces anglaises aux routines et fonctions publiques
!  - Mise à jour des cartouches
!  Revision 1.4  2000/02/24 12:54:01  util_am
!  Meilleure prise en compte des égalités entre scénarios ou entre lois
!  Revision 1.3  2000/02/24 08:02:23  util_am
!  Ajout de la routine MSP_egale_scenario
!  Revision 1.2  1999/09/22 15:22:45  util_am
!  Ajout des routines privees : MSP_consulter_scenario_*, MSP_modifier_scenario_*
!  Ajout de la surcharge publique MSP_consulter_scenario et MSP_modifier_scenario
!  Ajout des routines : MSP_supprimer_liste_loi, MSP_consulter_scenario_loi,
!                       MSP_loi_suivante
!  Ajout des fonctions : MSP_test_loi_courante, MSP_type_loi
!  Mise a jour des cartouches
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_SCENARIO
!
!$Structure
!
!: MSP_SCENARIO_LOI : 
!#V
!>     flag_func       : <logical,private>                       Flag indiquant si le scenario a été créé à partir d'une fonction
!>     type_scenario   : <integer,private>                       type du scénario:
!>                                                       MSP_ENUM_PROPULSION => propulsion
!>                                                       MSP_ENUM_ATTITUDE   => attitude
!>                                                       MSP_ENUM_SEPARATION => séparation
!>     nloi            : <integer,private>                       nombre de lois du scénario
!>     nom             : <LEN=MSP_LONG_CHAINE,private>           nom du scénario
!>     date_ref        : <tm_jour_sec,private>                   date de référence du scénario (JJ CNES)
!>     origdat         : <integer,private>                       Origine des dates (0=MJD1950, 1=MJD2000)
!>     tete_liste      : <MSP_LOI,pointer,private>               pointeur vers la première loi du scénario
!>     loi_courante    : <MSP_LOI,pointer,private>               pointeur vers la loi courante
!#
!
!: MSP_LOI : définition d'une loi d'un scénario.
!#V
!>     flag_func       : <logical,private>                       Flag indiquant si la loi a été créée à partir d'une fonction
!>     id              : <integer,private>                       numéro d'ordre de la loi (dans le scénario)
!>     nom             : <LEN=MSP_LONG_CHAINE,private>           nom de la loi
!>     type_loi        : <integer,private>                       type de la loi:
!.                                                       MSP_ENUM_LOI_IMP  => impulsionelle
!.                                                       MSP_ENUM_LOI_CONT => poussée continue
!.                                                       MSP_ENUM_LOI_BUL  => poussée bulletin
!.                                                       MSP_ENUM_LOI_SEP  => séparation
!.                                                       MSP_ENUM_LOI_ATTIT_TAB   => séparation
!.                                                       MSP_ENUM_LOI_ATTIT_SPIN  => séparation
!>     loi_imp         : <MSP_IMPULSION,pointer,private>         pointeur vers une loi de type impulsionelle
!>     loi_cont        : <MSP_POUSSEE_CONTINUE,pointer,private>  pointeur vers une loi de type poussée continue
!>     loi_bul         : <MSP_POUSSEE_BULLETIN,pointer,private>  pointeur vers une loi de type poussée bulletin
!>     loi_sep         : <MSP_SEPARATION,pointer,private>        pointeur vers une loi de type séparation
!>     loi_atti_tab    : <MSP_ATTITUDE_TABULEE,pointer,private>  pointeur vers une loi d'attitude de type tabulée
!>     loi_atti_spin   : <MSP_ATTITUDE_SPINNEE,pointer,private>  pointeur vers une loi d'attitude de type spin
!>     precedent       : <MSP_LOI,pointer,private>               pointeur vers la loi précédente dans le scénario
!>     suivant         : <MSP_LOI,pointer,private>               pointeur vers la loi suivante dans le scénario
!#
!
!$Global
!
!>  MSP_ENUM_PROPULSION      : <integer,parameter>  type de scénario de propulsion
!>  MSP_ENUM_ATTITUDE        : <integer,parameter>  type de scénario d'attitude
!>  MSP_ENUM_SEPARATION      : <integer,parameter>  type de scénario de séparation
!>  MSP_ENUM_LOI_IMP         : <integer,parameter>  loi de poussée impulsionelle
!>  MSP_ENUM_LOI_CONT        : <integer,parameter>  loi de poussée continue
!>  MSP_ENUM_LOI_BUL         : <integer,parameter>  loi de poussée de type bulletin
!>  MSP_ENUM_LOI_SEP         : <integer,parameter>  loi de séparation
!>  MSP_ENUM_LOI_ATTI_TAB    : <integer,parameter>  loi d'attitude de type tabulée
!>  MSP_ENUM_LOI_ATTI_SPIN   : <integer,parameter>  loi d'attitude de type spin
!>  MSP_ENUM_LOI_ABSOLUE     : <integer,parameter>  loi définie avec des dates en JJ CNES
!>  MSP_ENUM_LOI_RELATIVE    : <integer,parameter>  loi définie avec des dates en secondes relatives à une date de référence
!>  MSP_ENUM_LOI_ABSOREL     : <integer,parameter>  loi définie avec des dates en secondes relatives à la première date aui est 
!                                                   donnée en absolue
!$Common
!
!$Routines
!- MSP_ajouter_loi
!- MSP_consulter_scenario
!- MSP_modifier_scenario
!- MSP_loi_suivante
!- MSP_loi_precedente
!- MSP_test_loi_atti_tab
!- MSP_test_loi_atti_spin
!- MSP_test_loi_sep
!- MSP_test_loi_cont
!- MSP_test_loi_imp
!- MSP_test_loi_bul
!- MSP_consulter_ptr_loi
!- MSP_positionner_loi_courante
!- MSP_add_scenario_law
!- MSP_get_scenario_data
!- MSP_set_scenario_data
!- MSP_get_scenario_law
!- MSP_insert_scenario_law
!- MSP_sort_scenario
!- MSP_get_last_scenario_law
!- MSP_delete_scenario_law
!- MSP_clear_scenario_law
!- MSP_clear_scenario
!- MSP_get_current_law
!- MSP_go_to_next_law
!- MSP_go_to_previous_law
!- MSP_go_to_first_law
!- MSP_add_spinned_attitude
!- MSP_add_tabled_attitude
!- MSP_add_orbit_thrust
!- MSP_add_spread_thrust
!- MSP_add_impulse
!- MSP_add_separation
!- MSP_display_scenario
!- MSP_test_tabled_attitude_law
!- MSP_test_spinned_attitude_law
!- MSP_test_separation_law
!- MSP_test_spread_thrust_law
!- MSP_test_impulse_law
!- MSP_test_orbit_thust_law
!- MSP_get_current_law_pointer
!- MSP_set_current_law
!- MSP_consulter_scenario_loi
!- MSP_supprimer_loi
!- MSP_supprimer_liste_loi
!- MSP_effacer_loi
!- MSP_effacer_scenario
!- MSP_loi_precedente_scenar
!- MSP_loi_precedente_loi
!- MSP_premiere_loi
!- MSP_rechercher_loi_dates
!- MSP_rechercher_loi
!- MSP_inserer_loi
!- MSP_ranger_scenario
!- MSP_dates_loi
!- MSP_ajouter_attitude_spinnee
!- MSP_ajouter_attitude_tabulee
!- MSP_ajouter_poussee_bulletin
!- MSP_ajouter_poussee_continue
!- MSP_ajouter_impulsion
!- MSP_ajouter_separation
!- MSP_afficher_scenario
!- MSP_scenario_posit_loi_courante
!- MSP_consulter_ptr_loi
!#V
!- MSP_ajouter_impulsion_s
!- MSP_ajouter_poussee_continue_s
!- MSP_ajouter_poussee_bulletin_s
!- MSP_ajouter_attitude_spinnee_s
!- MSP_ajouter_attitude_tabulee_s
!- MSP_ajouter_separation_s
!- MSP_consulter_scenario_gen
!- MSP_consulter_scenario_imp
!- MSP_consulter_scenario_cont
!- MSP_consulter_scenario_bul
!- MSP_consulter_scenario_spin
!- MSP_consulter_scenario_tab
!- MSP_consulter_scenario_sep
!- MSP_consulter_loi
!- MSP_modifier_scenario_gen
!- MSP_modifier_scenario_imp
!- MSP_modifier_scenario_cont
!- MSP_modifier_scenario_bul
!- MSP_modifier_scenario_spin
!- MSP_modifier_scenario_tab
!- MSP_modifier_scenario_sep
!- MSP_egaler_scenario
!- MSP_egaler_loi
!- MSP_loi_suivante_scenar
!- MSP_loi_suivante_loi
!#
!
!$Fonctions
!- MSP_creer_scenario
!- MSP_loi_courante
!- MSP_test_loi_courante
!- MSP_derniere_loi
!- MSP_type_loi
!- MSP_test_loi_atti_tab
!- MSP_test_loi_atti_spin
!- MSP_test_loi_sep
!- MSP_test_loi_imp
!- MSP_test_loi_cont
!- MSP_test_loi_bul
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSPRO
!- MSP_GESTION_ERREUR
!- MSP_PROPULSION_IMP_DEF
!- MSP_PROPULSION_CONT_DEF
!- MSP_PROPULSION_BUL_DEF
!- MSP_SEPARATION_IMP_DEF
!- MSP_ATTITUDE_TAB_DEF
!- MSP_ATTITUDE_SPIN_DEF
!- MSP_MATH
!#
!
!$Interface
!> msp_test_spinned_attitude_law :  MSP_test_loi_atti_spin
!> msp_positionner_loi_courante :   MSP_scenario_posit_loi_courante
!> msp_add_separation :             MSP_ajouter_separation
!> msp_delete_scenario_law :        MSP_supprimer_loi
!> msp_set_current_law :            MSP_scenario_posit_loi_courante
!> msp_modifier_scenario :          MSP_modifier_scenario_gen, 
!                                   MSP_modifier_scenario_imp, 
!                                   MSP_modifier_scenario_cont, 
!                                   MSP_modifier_scenario_bul, 
!                                   MSP_modifier_scenario_sep, 
!                                   MSP_modifier_scenario_tab, 
!                                   MSP_modifier_scenario_spin
!> msp_insert_scenario_law :        MSP_inserer_loi
!> msp_go_to_first_law :            MSP_premiere_loi
!> msp_test_loi_imp :               MSP_test_loi_imp
!> msp_test_loi_bul :               MSP_test_loi_bul
!> msp_display_scenario :           MSP_afficher_scenario
!> msp_add_spread_thrust :          MSP_ajouter_poussee_continue
!> msp_loi_precedente :             MSP_loi_precedente_loi, 
!                                   MSP_loi_precedente_scenar
!> msp_clear_scenario_law :         MSP_effacer_loi
!> msp_get_last_scenario_law :      MSP_derniere_loi
!> assignment :                     MSP_egaler_scenario, MSP_egaler_loi
!> msp_set_scenario_data :          MSP_modifier_scenario_gen, 
!                                   MSP_modifier_scenario_imp, 
!                                   MSP_modifier_scenario_cont, 
!                                   MSP_modifier_scenario_bul, 
!                                   MSP_modifier_scenario_sep, 
!                                   MSP_modifier_scenario_tab, 
!                                   MSP_modifier_scenario_spin
!> msp_get_current_law_pointer :    MSP_consulter_ptr_loi
!> msp_test_orbit_thust_law :       MSP_test_loi_bul
!> msp_consulter_ptr_loi :          MSP_consulter_ptr_loi
!> msp_go_to_previous_law :         MSP_loi_precedente_loi, 
!                                   MSP_loi_precedente_scenar
!> msp_add_tabled_attitude :        MSP_ajouter_attitude_tabulee
!> msp_add_spinned_attitude :       MSP_ajouter_attitude_spinnee
!> msp_consulter_scenario :         MSP_consulter_scenario_gen, 
!                                   MSP_consulter_scenario_imp, 
!                                   MSP_consulter_scenario_cont, 
!                                   MSP_consulter_scenario_bul, 
!                                   MSP_consulter_scenario_spin, 
!                                   MSP_consulter_scenario_tab, 
!                                   MSP_consulter_scenario_sep, MSP_consulter_loi
!> msp_get_scenario_data :          MSP_consulter_scenario_gen, 
!                                   MSP_consulter_scenario_imp, 
!                                   MSP_consulter_scenario_cont, 
!                                   MSP_consulter_scenario_bul, 
!                                   MSP_consulter_scenario_spin, 
!                                   MSP_consulter_scenario_tab, 
!                                   MSP_consulter_scenario_sep, MSP_consulter_loi
!> msp_test_loi_cont :              MSP_test_loi_cont
!> msp_clear_scenario :             MSP_supprimer_liste_loi
!> msp_sort_scenario :              MSP_ranger_scenario
!> msp_add_orbit_thrust :           MSP_ajouter_poussee_bulletin
!> msp_go_to_next_law :             MSP_loi_suivante_loi, 
!                                   MSP_loi_suivante_scenar
!> msp_add_impulse :                MSP_ajouter_impulsion
!> msp_test_loi_sep :               MSP_test_loi_sep
!> msp_loi_suivante :               MSP_loi_suivante_loi, 
!                                   MSP_loi_suivante_scenar
!> msp_test_loi_atti_spin :         MSP_test_loi_atti_spin
!> msp_test_tabled_attitude_law :   MSP_test_loi_atti_tab
!> msp_get_scenario_law :           MSP_rechercher_loi
!> msp_test_loi_atti_tab :          MSP_test_loi_atti_tab
!> msp_add_scenario_law :           MSP_ajouter_impulsion_s, 
!                                   MSP_ajouter_poussee_continue_s, 
!                                   MSP_ajouter_poussee_bulletin_s, 
!                                   MSP_ajouter_separation_s, 
!                                   MSP_ajouter_attitude_tabulee_s, 
!                                   MSP_ajouter_attitude_spinnee_s
!> msp_test_impulse_law :           MSP_test_loi_imp
!> msp_test_spread_thrust_law :     MSP_test_loi_cont
!> msp_get_current_law :            MSP_loi_courante
!> msp_test_separation_law :        MSP_test_loi_sep
!> msp_ajouter_loi :                MSP_ajouter_impulsion_s, 
!                                   MSP_ajouter_poussee_continue_s, 
!                                   MSP_ajouter_poussee_bulletin_s, 
!                                   MSP_ajouter_separation_s, 
!                                   MSP_ajouter_attitude_tabulee_s, 
!                                   MSP_ajouter_attitude_spinnee_s
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  SCENARIO
!
!$Voir-Aussi
!#V
!.  MSP_ajouter_impulsion_s MSP_ajouter_poussee_continue_s MSP_ajouter_poussee_bulletin_s
!.  MSP_ajouter_attitude_spinnee_s MSP_ajouter_attitude_tabulee_s MSP_ajouter_separation_s
!.  MSP_consulter_scenario_gen MSP_consulter_scenario_imp MSP_consulter_scenario_cont MSP_consulter_scenario_bul
!.  MSP_consulter_scenario_spin MSP_consulter_scenario_tab MSP_consulter_scenario_sep MSP_consulter_loi
!.  MSP_modifier_scenario_gen MSP_modifier_scenario_imp MSP_modifier_scenario_cont MSP_modifier_scenario_bul
!.  MSP_modifier_scenario_spin MSP_modifier_scenario_tab MSP_modifier_scenario_sep MSP_egaler_scenario
!.  MSP_egaler_loi MSP_loi_suivante_scenar MSP_loi_suivante_loi
!#
!.  MSP_creer_scenario MSP_loi_courante MSP_test_loi_courante MSP_derniere_loi MSP_type_loi
!.  MSP_test_loi_atti_tab MSP_test_loi_atti_spin MSP_test_loi_sep MSP_test_loi_imp MSP_test_loi_cont
!.  MSP_test_loi_bul MSP_ajouter_loi MSP_consulter_scenario MSP_modifier_scenario MSP_loi_suivante
!.  MSP_loi_precedente MSP_test_loi_atti_tab MSP_test_loi_atti_spin MSP_test_loi_sep MSP_test_loi_cont
!.  MSP_test_loi_imp MSP_test_loi_bul MSP_consulter_ptr_loi MSP_positionner_loi_courante
!.  MSP_add_scenario_law MSP_get_scenario_data MSP_set_scenario_data MSP_get_scenario_law
!.  MSP_insert_scenario_law MSP_sort_scenario MSP_get_last_scenario_law MSP_delete_scenario_law
!.  MSP_clear_scenario_law MSP_clear_scenario MSP_get_current_law MSP_go_to_next_law MSP_go_to_previous_law
!.  MSP_go_to_first_law MSP_add_spinned_attitude MSP_add_tabled_attitude MSP_add_orbit_thrust
!.  MSP_add_spread_thrust MSP_add_impulse MSP_add_separation MSP_display_scenario MSP_test_tabled_attitude_law
!.  MSP_test_spinned_attitude_law MSP_test_separation_law MSP_test_spread_thrust_law MSP_test_impulse_law
!.  MSP_test_orbit_thust_law MSP_get_current_law_pointer MSP_set_current_law MSP_consulter_scenario_loi
!.  MSP_supprimer_loi MSP_supprimer_liste_loi MSP_effacer_loi MSP_effacer_scenario MSP_loi_precedente_scenar
!.  MSP_loi_precedente_loi MSP_premiere_loi MSP_rechercher_loi_dates MSP_rechercher_loi MSP_inserer_loi
!.  MSP_ranger_scenario MSP_dates_loi MSP_ajouter_attitude_spinnee MSP_ajouter_attitude_tabulee
!.  MSP_ajouter_poussee_bulletin MSP_ajouter_poussee_continue MSP_ajouter_impulsion MSP_ajouter_separation
!.  MSP_afficher_scenario MSP_scenario_posit_loi_courante MSP_consulter_ptr_loi
!
!$<>
!******************************************************************************

   use MSLIB, only : pm_reel
   use MSPRO
   use MSP_GESTION_ERREUR
   use MSP_PROPULSION_IMP_DEF
   use MSP_PROPULSION_CONT_DEF
   use MSP_PROPULSION_BUL_DEF
   use MSP_SEPARATION_IMP_DEF
   use MSP_ATTITUDE_TAB_DEF
   use MSP_ATTITUDE_SPIN_DEF
   use MSP_MATH

   ! DEFINITIONS DE TYPES:

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_SCENARIO.F90 365 2013-02-18 12:36:19Z aadt $'



   type MSP_SCENARIO_LOI
      
      private
      logical                :: flag_func
      integer                :: type_scenario
      integer                :: nloi
      character(LEN= MSP_LONG_CHAINE)      :: nom
      type(tm_jour_sec)      :: date_ref
      integer                :: origdat
      type(MSP_LOI), pointer :: tete_liste => NULL()
      type(MSP_LOI), pointer :: loi_courante => NULL()

   end type MSP_SCENARIO_LOI

   type MSP_LOI
      private
      logical                             :: flag_func
      integer                             :: id
      character(LEN= MSP_LONG_CHAINE)     :: nom
      integer                             :: type_loi

      type(MSP_IMPULSION), pointer        :: loi_imp => NULL()
      type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont => NULL()
      type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul => NULL()

      type(MSP_SEPARATION), pointer       :: loi_sep => NULL()

      type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab => NULL()
      type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin => NULL()

      type(MSP_LOI), pointer   :: precedent => NULL()
      type(MSP_LOI), pointer   :: suivant => NULL()

   end type MSP_LOI

   ! VARIABLES GLOBALES:

   integer, parameter :: MSP_ENUM_PROPULSION  = 1
   integer, parameter :: MSP_ENUM_ATTITUDE    = 2
   integer, parameter :: MSP_ENUM_SEPARATION  = 3

   integer, parameter :: MSP_ENUM_LOI_IMP  = 1
   integer, parameter :: MSP_ENUM_LOI_CONT = 2
   integer, parameter :: MSP_ENUM_LOI_BUL  = 3

   integer, parameter :: MSP_ENUM_LOI_SEP  = 4

   integer, parameter :: MSP_ENUM_LOI_ATTI_TAB  = 5
   integer, parameter :: MSP_ENUM_LOI_ATTI_SPIN = 6

   integer, parameter :: MSP_ENUM_LOI_ABSOLUE   = 1
   integer, parameter :: MSP_ENUM_LOI_RELATIVE  = 2
   integer, parameter :: MSP_ENUM_LOI_ABSOREL   = 3

   interface ASSIGNMENT (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ASSIGNMENT
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!   scenario_out= scenario_in
!.    type(MSP_SCENARIO_LOI) :: scenario_in
!.    type(MSP_SCENARIO_LOI) :: scenario_out
!
!   loi_out= loi_in
!.    type(MSP_LOI) :: loi_in
!.    type(MSP_LOI) :: loi_out
!
!$Procedures
!#V
!- MSP_egaler_scenario
!- MSP_egaler_loi
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO EGALER LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_egaler_scenario
     module procedure MSP_egaler_loi
   end interface

   ! INTERFACES:

   interface MSP_ajouter_loi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_loi
!
!$Resume
!  Ajouter une loi à un scenario
!
!$Description
!  Ajouter une loi à un scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_loi (scenario,nom,loi_imp)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!  call MSP_ajouter_loi (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi
!
!  call MSP_ajouter_loi (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!
!  call MSP_ajouter_loi (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi
!
!  call MSP_ajouter_loi (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!
!  call MSP_ajouter_loi (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!
!$Procedures
!#V
!- MSP_ajouter_impulsion_s
!- MSP_ajouter_poussee_continue_s
!- MSP_ajouter_poussee_bulletin_s
!- MSP_ajouter_separation_s
!- MSP_ajouter_attitude_tabulee_s
!- MSP_ajouter_attitude_spinnee_s
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO IMPULSION AJOUTER POUSSEE CONTINUE BULLETIN SEPARATION ATTITUDE TABULEE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_impulsion_s,MSP_ajouter_poussee_continue_s,MSP_ajouter_poussee_bulletin_s
      module procedure MSP_ajouter_separation_s,MSP_ajouter_attitude_tabulee_s,MSP_ajouter_attitude_spinnee_s
   end interface

   interface MSP_consulter_scenario

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario
!
!$Resume
!  Consulter les caractéristiques d'un scenario 
!
!$Description
!  Cette routine permet de consulter les caractéristiques du scenario et de récupérer
!  une loi du scenario par son identifiant ou son nom
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_scenario(scenario, [type], [nloi], [date_ref],[nom],[date_js], &
!.           origdat)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec) :: date_js
!.    integer :: nloi
!.    integer :: type
!.    integer :: origdat
!
!  call MSP_consulter_scenario(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!  call MSP_consulter_scenario(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!
!  call MSP_consulter_scenario(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!
!  call MSP_consulter_scenario(scenario, loi_attit_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_attit_spin
!
!  call MSP_consulter_scenario(scenario, loi_attit_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi_attit_tab
!
!  call MSP_consulter_scenario(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi_sep
!
!  call MSP_consulter_scenario(loi, [id], [nom], [type], &
!.           loi_imp, loi_cont, loi_bul, loi_atti_tab, loi_atti_spin, loi_sep)
!.    type(MSP_LOI) :: loi
!.    integer :: id, type
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    type(MSP_SEPARATION) :: loi_sep
!
!$Procedures
!#V
!- MSP_consulter_scenario_gen
!- MSP_consulter_scenario_imp
!- MSP_consulter_scenario_cont
!- MSP_consulter_scenario_bul
!- MSP_consulter_scenario_spin
!- MSP_consulter_scenario_tab
!- MSP_consulter_scenario_sep
!- MSP_consulter_loi
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER IMPULSION POUSSEE CONTINUE BULLETIN ATTITUDE SPINNEE TABULEE SEPARATION LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_scenario_gen
      module procedure MSP_consulter_scenario_imp, MSP_consulter_scenario_cont, MSP_consulter_scenario_bul
      module procedure MSP_consulter_scenario_spin, MSP_consulter_scenario_tab, MSP_consulter_scenario_sep
      module procedure MSP_consulter_loi
   end interface

   interface MSP_modifier_scenario

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario
!
!$Resume
!  Modifier les caractéristiques d'un scenario 
!
!$Description
!  Cette routine permet de modifier les caractéristiques du scenario et de modifier
!  une loi du scenario par son identifiant ou son nom
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_scenario(scenario, [date_ref], [nom], [date_js], [origdat])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec ) :: date_js
!.    integer :: origdat
!
!  call MSP_modifier_scenario(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_modifier_scenario(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_modifier_scenario(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_modifier_scenario(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_modifier_scenario(scenario, loi_atti_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_modifier_scenario(scenario, loi_atti_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
!#V
!- MSP_modifier_scenario_gen
!- MSP_modifier_scenario_imp
!- MSP_modifier_scenario_cont
!- MSP_modifier_scenario_bul
!- MSP_modifier_scenario_sep
!- MSP_modifier_scenario_tab
!- MSP_modifier_scenario_spin
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO MODIFIER IMPULSION POUSSEE CONTINUE BULLETIN SEPARATION ATTITUDE TABULEE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_scenario_gen
      module procedure MSP_modifier_scenario_imp, MSP_modifier_scenario_cont, MSP_modifier_scenario_bul
      module procedure MSP_modifier_scenario_sep, MSP_modifier_scenario_tab, MSP_modifier_scenario_spin
   end interface


   interface MSP_loi_suivante

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_suivante
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_loi_suivante(loi)
!.    type(MSP_LOI), pointer :: loi
!
!  call MSP_loi_suivante(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!#V
!- MSP_loi_suivante_loi
!- MSP_loi_suivante_scenar
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI SUIVANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_loi_suivante_loi, MSP_loi_suivante_scenar
   end interface

   interface MSP_loi_precedente

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_precedente
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_loi_precedente(loi)
!.    type(MSP_LOI), pointer :: loi
!
!  call MSP_loi_precedente(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_loi_precedente_loi
!- MSP_loi_precedente_scenar
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI PRECEDENTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_loi_precedente_loi, MSP_loi_precedente_scenar
   end interface

   interface MSP_test_loi_atti_tab

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_atti_tab
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_tab(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen : .true. / .false.
!booleen : .true. / .false.
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
!$Nom
!  MSP_test_loi_atti_tab
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_tab(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen : .true. / .false.
!booleen : .true. / .false.
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
      module procedure MSP_test_loi_atti_tab
   end interface

   interface MSP_test_loi_atti_spin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_atti_spin
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_spin(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_atti_spin
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_spin(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
      module procedure MSP_test_loi_atti_spin
   end interface

   interface MSP_test_loi_sep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_sep
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_sep(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario 
!scenario 
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_sep
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_sep(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario 
!scenario 
!>S     test      :<logical>            booleen
!booleen
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
      module procedure MSP_test_loi_sep
   end interface

   interface MSP_test_loi_cont

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_cont
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_cont(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   
!
!>S     test      :<logical>            
!
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
!$Nom
!  MSP_test_loi_cont
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_cont(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   
!
!>S     test      :<logical>            
!
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
      module procedure MSP_test_loi_cont
   end interface
   
   interface MSP_test_loi_imp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_imp
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_imp(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_imp
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_imp(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
      module procedure MSP_test_loi_imp
   end interface

   interface MSP_test_loi_bul

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_bul
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_bul(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_bul
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_bul(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
      module procedure MSP_test_loi_bul
   end interface

   interface MSP_consulter_ptr_loi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_ptr_loi
!
!$Resume
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
!
!$Description
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_ptr_loi(scenario, &
!.              [loi_imp], [loi_cont], [loi_bul], [loi_atti_tab], [loi_atti_spin], [loi_sep])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION), pointer :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin
!.    type(MSP_SEPARATION), pointer :: loi_sep
!
!$Procedures
!- MSP_consulter_ptr_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER LOI
! SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$Nom
!  MSP_consulter_ptr_loi
!
!$Resume
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
!
!$Description
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_ptr_loi(scenario, &
!.              [loi_imp], [loi_cont], [loi_bul], [loi_atti_tab], [loi_atti_spin], [loi_sep])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION), pointer :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin
!.    type(MSP_SEPARATION), pointer :: loi_sep
!
!$Procedures
!- MSP_consulter_ptr_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER LOI
! SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_ptr_loi
   end interface

   interface MSP_positionner_loi_courante

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_positionner_loi_courante
!
!$Resume
!  Recherche la loi n°id ou de nom "NOM" et positionne le pointeur loi courante
!  sur cette loi.
!
!$Description
!  Recherche la loi n°id ou de nom "NOM" et positionne le pointeur loi courante
!  sur cette loi.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_positionner_loi_courante(scenario, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
!- MSP_scenario_posit_loi_courante
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      module procedure MSP_scenario_posit_loi_courante
   end interface

   ! Interfaces anglaises
   interface MSP_add_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_scenario_law
!
!$Resume
!  Add a law to a scenario
!
!$Description
!  Add a law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_scenario_law (scenario,nom,loi_imp)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!  call MSP_add_scenario_law (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi
!
!  call MSP_add_scenario_law (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!
!  call MSP_add_scenario_law (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi
!
!  call MSP_add_scenario_law (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!
!  call MSP_add_scenario_law (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!
!$Procedures
!#V
!- MSP_ajouter_impulsion_s
!- MSP_ajouter_poussee_continue_s
!- MSP_ajouter_poussee_bulletin_s
!- MSP_ajouter_separation_s
!- MSP_ajouter_attitude_tabulee_s
!- MSP_ajouter_attitude_spinnee_s
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO IMPULSION AJOUTER POUSSEE CONTINUE BULLETIN SEPARATION ATTITUDE TABULEE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_impulsion_s,MSP_ajouter_poussee_continue_s,MSP_ajouter_poussee_bulletin_s
      module procedure MSP_ajouter_separation_s,MSP_ajouter_attitude_tabulee_s,MSP_ajouter_attitude_spinnee_s
   end interface

   interface MSP_get_scenario_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_scenario_data
!
!$Resume
!  Get information on the characteristics of a scenario
!
!$Description
!  This routine allow the user to get information on the sceanrio characteristics but also to get 
!  a copy of a law of a scenario by its id or its name
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_scenario_data(scenario, [type], [nloi], [date_ref],[nom],[date_js], &
!.           origdat)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec) :: date_js
!.    integer :: nloi
!.    integer :: type
!.    integer :: origdat
!
!  call MSP_get_scenario_data(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!  call MSP_get_scenario_data(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!
!  call MSP_get_scenario_data(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!
!  call MSP_get_scenario_data(scenario, loi_attit_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_attit_spin
!
!  call MSP_get_scenario_data(scenario, loi_attit_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi_attit_tab
!
!  call MSP_get_scenario_data(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi_sep
!
!  call MSP_get_scenario_data(loi, [id], [nom], [type], &
!.           loi_imp, loi_cont, loi_bul, loi_atti_tab, loi_atti_spin, loi_sep)
!.    type(MSP_LOI) :: loi
!.    integer :: id, type
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    type(MSP_SEPARATION) :: loi_sep
!
!$Procedures
!#V
!- MSP_consulter_scenario_gen
!- MSP_consulter_scenario_imp
!- MSP_consulter_scenario_cont
!- MSP_consulter_scenario_bul
!- MSP_consulter_scenario_spin
!- MSP_consulter_scenario_tab
!- MSP_consulter_scenario_sep
!- MSP_consulter_loi
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER IMPULSION POUSSEE CONTINUE BULLETIN ATTITUDE SPINNEE TABULEE SEPARATION LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_scenario_gen
      module procedure MSP_consulter_scenario_imp, MSP_consulter_scenario_cont, MSP_consulter_scenario_bul
      module procedure MSP_consulter_scenario_spin, MSP_consulter_scenario_tab, MSP_consulter_scenario_sep
      module procedure MSP_consulter_loi
   end interface

   interface MSP_set_scenario_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_scenario_data
!
!$Resume
!  Modify information on the characteristics of a scenario
!
!$Description
!  This routine allow the user to modify the scenario characteristics but also to modify 
!  a law of a scenario by its id or its name
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_scenario_data(scenario, [date_ref], [nom], [date_js], [origdat])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec ) :: date_js
!.    integer :: origdat
!
!  call MSP_set_scenario_data(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_set_scenario_data(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_set_scenario_data(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_set_scenario_data(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_set_scenario_data(scenario, loi_atti_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    integer :: id
!.    character(LEN=*) :: nom
!
!  call MSP_set_scenario_data(scenario, loi_atti_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
!#V
!- MSP_modifier_scenario_gen
!- MSP_modifier_scenario_imp
!- MSP_modifier_scenario_cont
!- MSP_modifier_scenario_bul
!- MSP_modifier_scenario_sep
!- MSP_modifier_scenario_tab
!- MSP_modifier_scenario_spin
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO MODIFIER IMPULSION POUSSEE CONTINUE BULLETIN SEPARATION ATTITUDE TABULEE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_scenario_gen
      module procedure MSP_modifier_scenario_imp, MSP_modifier_scenario_cont, MSP_modifier_scenario_bul
      module procedure MSP_modifier_scenario_sep, MSP_modifier_scenario_tab, MSP_modifier_scenario_spin
   end interface

   interface MSP_get_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_scenario_law
!
!$Resume
!  Scans the scenario laws in order to find a specified law
!
!$Description
!  Scans the scenario laws in order to find a law specified by its name or its id
!  It returns a pointer towards the identified law.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_scenario_law(scenario, ptr, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_LOI), pointer :: ptr
!
!$Procedures
!- MSP_rechercher_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI RECHERCHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_rechercher_loi
   end interface

   interface MSP_insert_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_insert_scenario_law
!
!$Resume
!  Insets a law in the sequence of law defined in a scenario
!
!$Description
!  Insets a law in the sequence of law defined in a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_insert_scenario_law (scenario,ptr_loi,t1,t2)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: ptr_loi
!.    real(KIND=pm_reel) :: t1,t2
!
!$Procedures
!- MSP_inserer_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI INSERER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_inserer_loi
   end interface

   interface MSP_sort_scenario

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_sort_scenario
!
!$Resume
!  Arrange the scenario laws in chronological order
!
!$Description
!  Arrange the scenario laws in chronological order
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_sort_scenario(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_ranger_scenario
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI RANGER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ranger_scenario
   end interface

   interface MSP_get_last_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_last_scenario_law
!
!$Resume
!  Get the last scenario law in the scenario sequence
!
!$Description
!  Get the last scenario law in the scenario sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  ptr = MSP_get_last_scenario_law (tete)
!.    type(MSP_LOI), pointer :: tete
!.    type(MSP_LOI), pointer :: ptr
!
!$Procedures
!- MSP_derniere_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI DERNIERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_derniere_loi
   end interface

   interface MSP_delete_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_delete_scenario_law
!
!$Resume
!  Delete a law in the scenario sequence
!
!$Description
!  Delete a law in the scenario sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_delete_scenario_law (scenario,[nom],[id])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
!- MSP_supprimer_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI SUPPRIMER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_supprimer_loi
   end interface

   interface MSP_clear_scenario_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_scenario_law
!
!$Resume
!  Clear a law in the scenario sequence
!
!$Description
!  Clear a law in the scenario sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_scenario_law(loi)
!.    type (MSP_LOI) :: loi
!
!$Procedures
!- MSP_effacer_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_loi
   end interface

   interface MSP_clear_scenario

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_scenario
!
!$Resume
!  Clear the scenario contents
!
!$Description
!  Clear the scenario contents
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_scenario(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_supprimer_liste_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI SUPPRIMER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_supprimer_liste_loi
   end interface

   interface MSP_get_current_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_current_law
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  ptr = MSP_get_current_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: ptr
!
!$Procedures
!- MSP_loi_courante
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI COURANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_loi_courante
   end interface

   interface MSP_go_to_next_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_go_to_next_law
!
!$Resume
!  Points the current pointer towards the next law in the sequence
!
!$Description
!  Points the current pointer towards the next law in the sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_go_to_next_law(loi)
!.    type(MSP_LOI), pointer :: loi
!
!  call MSP_go_to_next_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!#V
!- MSP_loi_suivante_loi
!- MSP_loi_suivante_scenar
!#
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI SUIVANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_loi_suivante_loi, MSP_loi_suivante_scenar
   end interface

   interface MSP_go_to_previous_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_go_to_previous_law
!
!$Resume
!  Points the current pointer towards the previous law in the sequence
!
!$Description
!  Points the current pointer towards the next law in the sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_go_to_previous_law(loi)
!.    type(MSP_LOI), pointer :: loi
!
!  call MSP_go_to_previous_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_loi_precedente_loi
!- MSP_loi_precedente_scenar
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI PRECEDENTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_loi_precedente_loi, MSP_loi_precedente_scenar
   end interface

   interface MSP_go_to_first_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_go_to_first_law
!
!$Resume
!  Points the current pointer towards the first law of the scenario sequence
!
!$Description
!  Points the current pointer towards the first law of the scenario sequence
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_go_to_first_law(scenario, [loi])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: loi
!
!$Procedures
!- MSP_premiere_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO LOI PREMIERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_premiere_loi
   end interface

   interface MSP_add_spinned_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_spinned_attitude
!
!$Resume
!  Adds a spinned attitude law to a scenario
!
!$Description
!  Adds a spinned attitude law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_spinned_attitude (scenario,nom,[loi],[typdat],[datdeb],[datfin], &
!.            typrep,psi0,teta0,phi0,psip,tetap,phip,typangle)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb,datfin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0,teta0,phi0
!.    real(KIND=pm_reel) :: psip,tetap,phip
!.    integer :: typangle
!
!$Procedures
!- MSP_ajouter_attitude_spinnee
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER ATTITUDE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_attitude_spinnee
   end interface
   
   interface MSP_add_tabled_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_tabled_attitude
!
!$Resume
!  Adds a tabled attitude law to a scenario
!
!$Description
!  Adds a tabled attitude law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_tabled_attitude (scenario,nom,[loi],[ntab],[typdat],[dates],[typrep],[psi],[teta],[phi],[typangle])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel) :: dates(:)
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi(:),teta(:),phi(:)
!.    integer :: typangle
!
!$Procedures
!- MSP_ajouter_attitude_tabulee
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER ATTITUDE TABULEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_attitude_tabulee
   end interface
   
   interface MSP_add_orbit_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_orbit_thrust
!
!$Resume
!  Adds a tabled propulsion law defined by an end of thrust orbit to a scenario
!
!$Description
!  Adds a tabled propulsion law defined by an end of thrust orbit to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_orbit_thrust (scenario,nom,[loi],[typdat],[datdeb],[bul],[merg])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=pm_reel) :: merg
!
!$Procedures
!- MSP_ajouter_poussee_bulletin
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER POUSSEE BULLETIN
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_poussee_bulletin
   end interface  

   interface MSP_add_spread_thrust

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_spread_thrust
!
!$Resume
!  Adds a spread thrust propulsion law to a scenario
!
!$Description
!  Adds a spread thrust propulsion law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_spread_thrust (scenario,nom,[loi],[ntab],[typdat],[dates], &
!.         poussee,isp,dirref,omega,omegap,merg,pas,debit)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi
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
!
!$Procedures
!- MSP_ajouter_poussee_continue
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER POUSSEE CONTINUE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_poussee_continue
   end interface

   interface MSP_add_impulse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_impulse
!
!$Resume
!  Adds an impulse propulsion law to a scenario
!
!$Description
!  Adds an impulse propulsion law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_impulse (scenario,nom,[loi],[typdat],[date],[deltav],[dirref], &
!.            omega,omegap,merg)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega, omegap, merg
!
!$Procedures
!- MSP_ajouter_impulsion
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER IMPULSION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_impulsion
   end interface

   interface MSP_add_separation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_add_separation
!
!$Resume
!  Adds a separation law to a scenario
!
!$Description
!  Adds a separation law to a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_add_separation (scenario,nom,[loi],[typdat],[date],[deltav],[omega],[omegap],&
!.            forme,sx,sy,sz,st,spx,spy,spz,tymasse,typcf,merg,aero,prad, date_js)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    real(KIND=pm_reel) :: omega, omegap
!.    integer :: forme,tymasse,typcf
!.    real(KIND=pm_reel) :: sx, sy, sz, st, spx, spy, spz, merg
!.    type(tm_jour_sec) :: date_js
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!
!$Procedures
!- MSP_ajouter_separation
!
!$Remarques
!
!$Mots-cles
! SCENARIO AJOUTER SEPARATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_ajouter_separation
   end interface   

   interface MSP_display_scenario

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_display_scenario
!
!$Resume
!  Display the characteristics of a scenario
!
!$Description
!  Display the characteristics of a scenario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_display_scenario (scenario,[ilog])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: ilog
!
!$Procedures
!- MSP_afficher_scenario
!
!$Remarques
!
!$Mots-cles
! SCENARIO AFFICHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_afficher_scenario
   end interface   

   interface MSP_test_tabled_attitude_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_tabled_attitude_law
!
!$Resume
!  This function tests if the current law is a tabbled attitude law
!
!$Description
!  This function tests if the current law is a tabbled attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_tabled_attitude_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_atti_tab
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_atti_tab
   end interface

   interface MSP_test_spinned_attitude_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_spinned_attitude_law
!
!$Resume
! This function tests if the current law is a spinned attitude law
!
!$Description
! This function tests if the current law is a spinned attitude law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_spinned_attitude_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_atti_spin
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_atti_spin
   end interface

   interface MSP_test_separation_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_separation_law
!
!$Resume
! This function tests if the current law is a separation law
!
!$Description
! This function tests if the current law is a separation law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_separation_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_sep
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_sep
   end interface

   interface MSP_test_spread_thrust_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_spread_thrust_law
!
!$Resume
!   This function tests if the current law is a spread thrust law
!
!$Description
!   This function tests if the current law is a spread thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_spread_thrust_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_cont
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_cont
   end interface
   
   interface MSP_test_impulse_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_impulse_law
!
!$Resume
!   This function tests if the current law is an impulse law 
!
!$Description
!   This function tests if the current law is an impulse law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_impulse_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_imp
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_imp
   end interface

   interface MSP_test_orbit_thust_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_orbit_thust_law
!
!$Resume
!   This function tests if the current law is an orbit thrust law
!
!$Description
!   This function tests if the current law is an orbit thrust law
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_orbit_thust_law(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Procedures
!- MSP_test_loi_bul
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_test_loi_bul
   end interface

   interface MSP_get_current_law_pointer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_current_law_pointer
!
!$Resume
!  Returns a pointer on a structure (attitude law, propulsion law..) embedded 
!  in the current law of the scenario.
!
!$Description
!  Returns a pointer on a structure (attitude law, propulsion law..) embedded 
!  in the current law of the scenario.
!  This routine allows to increase performance, because data structures are not
!  implicitly copied.
!  The law returned is a pointer, and should not be freed after use, because it 
!  belongs to the scenario.
!  This subroutine is designed to be used when consulting the scenario data structure,
!  and should not be used to get and modify a data structure.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_current_law_pointer(scenario, &
!.              loi_imp, loi_cont, loi_bul, loi_atti_tab, loi_atti_spin, loi_sep)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION), pointer :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin
!.    type(MSP_SEPARATION), pointer :: loi_sep
!
!$Procedures
!- MSP_consulter_ptr_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_ptr_loi
   end interface

   interface MSP_set_current_law

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_current_law
!
!$Resume
! This function sets the current law to a particular law (Nr Id or named "nom")
!
!$Description
! This function sets the current law to a particular law (Nr Id or named "nom")
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_current_law(scenario, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
!- MSP_scenario_posit_loi_courante
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_scenario_posit_loi_courante
   end interface



   ! ROUTINES / FONCTIONS:

   private MSP_consulter_scenario_gen, MSP_consulter_scenario_imp
   private MSP_consulter_scenario_cont, MSP_consulter_scenario_bul
   private MSP_consulter_scenario_spin, MSP_consulter_scenario_tab, MSP_consulter_scenario_sep
   private MSP_consulter_loi

   private MSP_modifier_scenario_gen, MSP_modifier_scenario_imp
   private MSP_modifier_scenario_cont, MSP_modifier_scenario_bul
   private MSP_modifier_scenario_sep, MSP_modifier_scenario_tab, MSP_modifier_scenario_spin
   
   private MSP_ajouter_impulsion_s,MSP_ajouter_poussee_continue_s,MSP_ajouter_poussee_bulletin_s
   private MSP_ajouter_separation_s,MSP_ajouter_attitude_tabulee_s,MSP_ajouter_attitude_spinnee_s

   private MSP_loi_suivante_scenar, MSP_loi_suivante_loi

   private MSP_egaler_scenario, MSP_egaler_loi

    contains


   subroutine MSP_ajouter_impulsion_s (scenario,nom,loi_imp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_impulsion_s
!
!$Resume
!  Ajout d'une loi de poussée impulsionnelle dans un scénario avec appel simplifié (uniquement avec le type MSP_IMPULSION).
!
!$Description
!  Ajout d'une loi de poussée impulsionnelle dans un scénario avec appel simplifié (uniquement avec le type MSP_IMPULSION).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_impulsion_s (scenario,nom,loi_imp)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>              nom de la loi
!>E     loi_imp   :<MSP_IMPULSION>      type contenant les informations de la loi (généré par appel à MSP_creer_impulsion)
!
!$Common
!
!$Routines
!- MSP_ajouter_impulsion
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO IMPULSION AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN) :: nom
      type(MSP_IMPULSION), intent(IN)  :: loi_imp
      call MSP_ajouter_impulsion (scenario,nom,loi=loi_imp)

   end subroutine MSP_ajouter_impulsion_s


   subroutine MSP_ajouter_poussee_continue_s (scenario,nom,loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_poussee_continue_s
!
!$Resume
!  Ajout d'une loi de poussée continue dans un scénario avec appel simplifié (uniquement avec le type MSP_POUSSEE_CONTINUE).
!
!$Description
!  Ajout d'une loi de poussée continue dans un scénario avec appel simplifié (uniquement avec le type MSP_POUSSEE_CONTINUE).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_poussee_continue_s (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>E     loi       :<MSP_POUSSEE_CONTINUE>   type contenant les informations de la loi (généré par appel à MSP_creer_poussee_continue)
!
!$Common
!
!$Routines
!- MSP_ajouter_poussee_continue
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO POUSSEE CONTINUE AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN)        :: nom
      type(MSP_POUSSEE_CONTINUE), intent(IN)  :: loi
      call MSP_ajouter_poussee_continue (scenario,nom,loi=loi)
   end subroutine MSP_ajouter_poussee_continue_s


   subroutine MSP_ajouter_poussee_bulletin_s (scenario,nom,loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_poussee_bulletin_s
!
!$Resume
!  Ajout d'une loi de poussée de type bulletin dans un scénario avec appel simplifié (uniquement avec le type MSP_POUSSEE_BULLETIN).
!
!$Description
!  Ajout d'une loi de poussée de type bulletin dans un scénario avec appel simplifié (uniquement avec le type MSP_POUSSEE_BULLETIN).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_poussee_bulletin_s (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>E     loi       :<MSP_POUSSEE_BULLETIN>   type contenant les informations de la loi (généré par appel à MSP_creer_poussee_bulletin)
!
!$Common
!
!$Routines
!- MSP_ajouter_poussee_bulletin
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO POUSSEE BULLETIN AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN)        :: nom
      type(MSP_POUSSEE_BULLETIN), intent(IN)  :: loi
      call MSP_ajouter_poussee_bulletin (scenario,nom,loi=loi)

   end subroutine MSP_ajouter_poussee_bulletin_s


   subroutine MSP_ajouter_attitude_spinnee_s (scenario,nom,loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_attitude_spinnee_s
!
!$Resume
!  Ajout d'une loi d'attitude spinnée dans un scénario avec appel simplifié (uniquement avec le type MSP_ATTITUDE_SPINNEE).
!
!$Description
!  Ajout d'une loi d'attitude spinnée dans un scénario avec appel simplifié (uniquement avec le type MSP_ATTITUDE_SPINNEE).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_attitude_spinnee_s (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>E     loi       :<MSP_ATTITUDE_SPINNEE>   type contenant les informations de la loi (généré par appel à MSP_creer_attitude_spinnee)
!
!$Common
!
!$Routines
!- MSP_ajouter_attitude_spinnee
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO ATTITUDE SPINNEE AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN)        :: nom
      type(MSP_ATTITUDE_SPINNEE), intent(IN)  :: loi
      call MSP_ajouter_attitude_spinnee (scenario,nom,loi=loi)
   end subroutine MSP_ajouter_attitude_spinnee_s

   subroutine MSP_ajouter_attitude_tabulee_s (scenario,nom,loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_attitude_tabulee_s
!
!$Resume
!  Ajout d'une loi d'attitude tabulée dans un scénario avec appel simplifié (uniquement avec le type MSP_ATTITUDE_TABULEE).
!
!$Description
!  Ajout d'une loi d'attitude tabulée dans un scénario avec appel simplifié (uniquement avec le type MSP_ATTITUDE_TABULEE).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_attitude_tabulee_s (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>E     loi       :<MSP_ATTITUDE_TABULEE>   type contenant les informations de la loi (généré par appel à MSP_creer_attitude_tabulee)
!
!$Common
!
!$Routines
!- MSP_ajouter_attitude_tabulee
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO ATTITUDE TABULEE AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN)        :: nom
      type(MSP_ATTITUDE_TABULEE), intent(IN)  :: loi
      call MSP_ajouter_attitude_tabulee (scenario,nom,loi=loi)
   end subroutine MSP_ajouter_attitude_tabulee_s

   subroutine MSP_ajouter_separation_s (scenario,nom,loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_separation_s
!
!$Resume
!  Ajout d'une loi de séparation dans un scénario avec appel simplifié (uniquement avec le type MSP_SEPARATION).
!
!$Description
!  Ajout d'une loi de séparation dans un scénario avec appel simplifié (uniquement avec le type MSP_SEPARATION).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_ajouter_separation_s (scenario,nom,loi)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>              nom de la loi
!>E     loi       :<MSP_SEPARATION>     type contenant les informations de la loi (généré par appel à MSP_creer_separation)
!
!$Common
!
!$Routines
!- MSP_ajouter_separation
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO SEPARATION AJOUTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN) :: nom
      type(MSP_SEPARATION), intent(IN)  :: loi
      call MSP_ajouter_separation (scenario,nom,loi=loi)
   end subroutine MSP_ajouter_separation_s

  SUBROUTINE MSP_consulter_scenario_gen(scenario, type, nloi, date_ref,nom,date_js, &
       origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_gen
!
!$Resume
!  Routine de consulter les informations relatives au scenario
!
!$Description
!  Routine de consulter les informations relatives au scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_gen(scenario, [type], [nloi], [date_ref],[nom],[date_js], &
!.           [origdat])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec) :: date_js
!.    integer :: nloi
!.    integer :: type
!.    integer :: origdat
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario à lire
!>[S]   type      :<integer>            type de scenario
!>[S]   nloi      :<integer>            nombre de lois le constituant
!>[S]   date_ref  :<PM_REEL>            date de référence utilisée
!>[S]   nom       :<LEN=*>              nom du scenario
!>[S]   date_js   :<tm_jour_sec>        Date de reference en jour / secondes
!>[S]   origdat   :<integer>            Origine des dates (0=MJD1950, 1=MJD2000)
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
!  SCENARIO CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_SCENARIO_LOI), intent(IN) :: scenario
    character(LEN=*), intent(OUT), optional   :: nom
    real(KIND=PM_REEL), intent(OUT), optional :: date_ref
    type(tm_jour_sec) , intent(OUT), optional :: date_js
    integer, intent(OUT), optional            :: nloi
    integer, intent(OUT), optional            :: type
    integer, intent(OUT), optional            :: origdat

    ! variables locales
    real(KIND=PM_REEL)   :: date
    type(tm_code_retour) :: code_retour

    ! corps de la fonction
    call md_joursec_jourfrac(scenario%date_ref, date, code_retour)

    if (PRESENT(type))     type     = scenario%type_scenario
    if (PRESENT(nloi))     nloi     = scenario%nloi
    if (PRESENT(date_ref)) date_ref = date
    if (PRESENT(date_js))  date_js  = scenario%date_ref
    if (PRESENT(nom))      nom      = scenario%nom
    if (PRESENT(origdat))  origdat  = scenario%origdat

  end SUBROUTINE MSP_consulter_scenario_gen


  SUBROUTINE MSP_consulter_scenario_imp(scenario, loi_imp, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_imp
!
!$Resume
!  Routine permettant de consulter une loi impulsionnelle d'un scenario
!
!$Description
!  Routine permettant de consulter une loi impulsionnelle d'un scenario
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_imp(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario contenant la loi à consulter
!>S     loi_imp   :<MSP_IMPULSION>      loi impulsionnelle en retour
!>[E]   id        :<integer>            Numéro identifiant de la loi à consulter
!>[E]   nom       :<LEN=*>              Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER IMPULSION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_IMPULSION), intent(OUT) :: loi_imp
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_imp" )) return

    else if (associated(scenario%loi_courante)) then
       ptr => scenario%loi_courante
    else
       call MSP_signaler_message ( message="Pas de loi courante", &
                  routine="MSP_consulter_scenario_imp", type=MSP_ENUM_WARNING)
       return
    end if
    loi_imp = ptr%loi_imp

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_imp


  SUBROUTINE MSP_consulter_scenario_cont(scenario, loi_cont, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_cont
!
!$Resume
!  Routine permettant de consulter une loi continue d'un scenario
!
!$Description
!  Routine permettant de consulter une loi continue d'un scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_cont(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       Scenario contenant la loi à consulter
!>E/S   loi_cont  :<MSP_POUSSEE_CONTINUE>   loi continue en retour
!>[E]   id        :<integer>                Numéro identifiant de la loi à consulter
!>[E]   nom       :<LEN=*>                  Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER POUSSEE CONTINUE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_POUSSEE_CONTINUE), intent(INOUT) :: loi_cont
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_cont" )) return

    else
       ptr => scenario%loi_courante
    end if
    loi_cont = ptr%loi_cont

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_cont


  SUBROUTINE MSP_consulter_scenario_bul(scenario, loi_bul, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_bul
!
!$Resume
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Description
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_bul(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       Scenario contenant la loi à consulter
!>S     loi_bul   :<MSP_POUSSEE_BULLETIN>   loi bulletin en retour
!>[E]   id        :<integer>                Numéro identifiant de la loi à consulter
!>[E]   nom       :<LEN=*>                  Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER POUSSEE BULLETIN
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_POUSSEE_BULLETIN), intent(OUT) :: loi_bul
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_bul" )) return

    else
       ptr => scenario%loi_courante
    end if
    loi_bul = ptr%loi_bul

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_bul



  SUBROUTINE MSP_consulter_scenario_spin(scenario, loi_attit_spin, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_spin
!
!$Resume
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Description
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_spin(scenario, loi_attit_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_attit_spin
!
!$Arguments
!>E/S   scenario        :<MSP_SCENARIO_LOI>       Scenario contenant la loi à consulter
!>S     loi_attit_spin  :<MSP_ATTITUDE_SPINNEE>   loi d'attitude spinnee en retour
!>[E]   id              :<integer>                Numéro identifiant de la loi à consulter
!>[E]   nom             :<LEN=*>                  Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER ATTITUDE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_ATTITUDE_SPINNEE), intent(OUT) :: loi_attit_spin
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_spin" )) return

    else
       ptr => scenario%loi_courante
    end if
    loi_attit_spin = ptr%loi_atti_spin

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_spin



  SUBROUTINE MSP_consulter_scenario_tab(scenario, loi_attit_tab, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_tab
!
!$Resume
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Description
!  Routine permettant de consulter une loi bulletin d'un scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_tab(scenario, loi_attit_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi_attit_tab
!
!$Arguments
!>E/S   scenario       :<MSP_SCENARIO_LOI>       Scenario contenant la loi à consulter
!>E/S   loi_attit_tab  :<MSP_ATTITUDE_TABULEE>   Loi attitude tabulee en retour
!>[E]   id             :<integer>                Numéro identifiant de la loi à consulter
!>[E]   nom            :<LEN=*>                  Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER ATTITUDE TABULEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_ATTITUDE_TABULEE), intent(INOUT) :: loi_attit_tab
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_tab" )) return

    else
       ptr => scenario%loi_courante
    end if
    loi_attit_tab = ptr%loi_atti_tab

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_tab



  SUBROUTINE MSP_consulter_scenario_sep(scenario, loi_sep, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_sep
!
!$Resume
!  Routine permettant de consulter une loi separation d'un scenario
!
!$Description
!  Routine permettant de consulter une loi separation d'un scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_scenario_sep(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi_sep
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario contenant la loi à consulter
!>E/S   loi_sep   :<MSP_SEPARATION>     loi separation en retour
!>[E]   id        :<integer>            Numéro identifiant de la loi à consulter
!>[E]   nom       :<LEN=*>              Nom de la loi à consulter
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER SEPARATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_SEPARATION), intent(INOUT) :: loi_sep
    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_consulter_scenario_sep" )) return

    else
       ptr => scenario%loi_courante
    end if
    loi_sep = ptr%loi_sep

    ! -- Positionnement loi_courante sur la loi consultée
    scenario%loi_courante => ptr

  end SUBROUTINE MSP_consulter_scenario_sep


  SUBROUTINE MSP_consulter_loi(loi, id, nom, type, &
       loi_imp, loi_cont, loi_bul, loi_atti_tab, loi_atti_spin, loi_sep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_loi
!
!$Resume
!  Cette routine permet la consultation à partir de la structure MSP_LOI
!
!$Description
!  Cette routine permet la consultation à partir de la structure MSP_LOI
!
!$Auteur
!  26/07/2000 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_consulter_loi(loi, [id], [nom], [type], &
!.           [loi_imp], [loi_cont], [loi_bul], [loi_atti_tab], [loi_atti_spin], [loi_sep])
!.    type(MSP_LOI) :: loi
!.    integer :: id, type
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    type(MSP_SEPARATION) :: loi_sep
!
!$Arguments
!>E     loi            :<MSP_LOI>                Structure à consulter
!>[S]   id             :<integer>                Identificateur dans la liste des lois
!>[S]   nom            :<LEN=*>                  Nom de la loi
!>[S]   type           :<integer>                Type de la loi
!>[S]   loi_imp        :<MSP_IMPULSION>          Structure impulsion à récupérer
!>[E/S] loi_cont       :<MSP_POUSSEE_CONTINUE>   Structure poussée continue à récupérer
!>[S]   loi_bul        :<MSP_POUSSEE_BULLETIN>   Structure poussée bulletin à récupérer
!>[E/S] loi_atti_tab   :<MSP_ATTITUDE_TABULEE>   Structure attitude tabulée à récupérer
!>[S]   loi_atti_spin  :<MSP_ATTITUDE_SPINNEE>   Structure attitude spinnée à récupérer
!>[E/S] loi_sep        :<MSP_SEPARATION>         Structure séparation à récupérer
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
!  SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    implicit none

    type(MSP_LOI), intent(IN) :: loi
    integer, intent(OUT), optional :: id, type
    character(LEN=*), intent(OUT), optional :: nom
    type(MSP_IMPULSION), intent(OUT), optional :: loi_imp
    type(MSP_POUSSEE_CONTINUE), intent(INOUT), optional :: loi_cont
    type(MSP_POUSSEE_BULLETIN), intent(OUT), optional :: loi_bul
    type(MSP_ATTITUDE_TABULEE), intent(INOUT), optional :: loi_atti_tab
    type(MSP_ATTITUDE_SPINNEE), intent(OUT), optional :: loi_atti_spin
    type(MSP_SEPARATION), intent(INOUT), optional :: loi_sep

    if (PRESENT(id))   id   = loi%id
    if (PRESENT(type)) type = loi%type_loi
    if (PRESENT(nom))  nom  = loi%nom
    if (PRESENT(loi_imp))  loi_imp  = loi%loi_imp
    if (PRESENT(loi_cont)) loi_cont = loi%loi_cont
    if (PRESENT(loi_bul))  loi_bul  = loi%loi_bul
    if (PRESENT(loi_atti_tab))  loi_atti_tab  = loi%loi_atti_tab
    if (PRESENT(loi_atti_spin)) loi_atti_spin = loi%loi_atti_spin
    if (PRESENT(loi_sep))  loi_sep  = loi%loi_sep

  end SUBROUTINE MSP_consulter_loi


  SUBROUTINE MSP_consulter_scenario_loi(scenario, id, nom, &
       datedeb, datefin, dv, w, wp, erg, poussee, nom_loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_scenario_loi
!
!$Resume
!  Routine de lecture d'information sur une loi de propulsion quel que soit son type
!
!$Description
!  Routine de lecture d'information sur une loi de propulsion quel que soit son type
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_scenario_loi(scenario, [id], [nom], &
!.           [datedeb], [datefin], [dv], [w], [wp], [erg], [poussee], [nom_loi])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: datedeb, datefin
!.    real(KIND=PM_REEL) :: dv, w, wp, erg
!.    character(LEN=*) :: nom_loi
!.    real(KIND=PM_REEL), dimension(:), pointer :: poussee
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>          Scenario contenant la loi à lire
!>[E]   id        :<integer>                   Identifiant de la loi à lire
!>[E]   nom       :<LEN=*>                     nom de la loi à lire
!>[S]   datedeb   :<PM_REEL>                   date début de poussée [en JJ ou en s]
!>[S]   datefin   :<PM_REEL>                   date de fin de poussée [en JJ ou en s]
!>[S]   dv        :<PM_REEL>                   intensité de l'impulsion [m/s]
!>[S]   w         :<PM_REEL>                   angle de poussée dans le plan du véhicule [rad]
!>[S]   wp        :<PM_REEL>                   angle de poussée hors du plan du véhicule [rad]
!>[S]   erg       :<PM_REEL>                   masse d'ergols disponibles [kg]
!>[E/S] poussee   :<PM_REEL,DIM=(:),pointer>   Niveau de poussée au cours de l'impulsion [N]
!>[S]   nom_loi   :<LEN=*>                     Nom de la loi
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_consulter_impulsion
!- MSP_consulter_poussee_continue
!- MSP_consulter_poussee_bulletin
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguements
    type(MSP_SCENARIO_LOI), intent(IN) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    real(KIND=PM_REEL), intent(OUT), optional :: datedeb, datefin
    real(KIND=PM_REEL), intent(OUT), optional :: dv, w, wp, erg
    character(LEN=*), intent(OUT), optional :: nom_loi
    real(KIND=PM_REEL), dimension(:), pointer, optional :: poussee

    ! Variables loacles
    integer :: type_loi
    real(KIND=PM_REEL) :: isp
    type(MSP_LOI), pointer :: ptr => NULL()

    ! Corps de la fonction
    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_lire_loi" )) return

    else
       ptr => scenario%loi_courante
    end if

    type_loi = MSP_type_loi(loi=ptr)
    if (MSP_gen_messages("MSP_lire_loi" )) return

    SELECT CASE(type_loi)

    CASE(MSP_ENUM_LOI_IMP)

       call MSP_consulter_impulsion(ptr%loi_imp, datedeb=datedeb, datefin=datefin, &
            deltav=dv, omega=w, omegap=wp, merg=erg)

    CASE(MSP_ENUM_LOI_CONT)

       call MSP_consulter_poussee_continue(ptr%loi_cont, datedeb=datedeb, datefin=datefin, poussee=poussee, isp=isp)
       if (PRESENT(erg)) erg = MSP_consommation_continue(isp, datefin-datedeb, poussee(1))

    CASE(MSP_ENUM_LOI_BUL)

       call MSP_consulter_poussee_bulletin(ptr%loi_bul, datedeb=datedeb, datefin=datefin, merg=erg)

    end SELECT

    if (PRESENT(nom_loi)) nom_loi = ptr%nom

  end SUBROUTINE MSP_consulter_scenario_loi


  SUBROUTINE MSP_modifier_scenario_gen(scenario, date_ref, nom, date_js, origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_gen
!
!$Resume
!  Cette routine permet de modifier certains champs de la structure scenario
!
!$Description
!  Cette routine permet de modifier certains champs de la structure scenario
!
!$Auteur
!  24/07/2000 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_gen(scenario, [date_ref], [nom], [date_js], [origdat])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    real(KIND=PM_REEL) :: date_ref
!.    type(tm_jour_sec ) :: date_js
!.    integer :: origdat
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario à modifier
!>[E]   date_ref  :<PM_REEL>            Date de référence des lois du scenario
!>[E]   nom       :<LEN=*>              Nom du scenario
!>[E]   date_js   :<tm_jour_sec>        Date de reference en jour / secondes
!>[E]   origdat   :<integer>            Origine des dates (0=MJD1950, 1=MJD2000)
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_joursec_jourfrac
!- MSP_consulter_impulsion
!- MSP_modifier_impulsion
!- MSP_consulter_poussee_continue
!- MSP_modifier_poussee_continue
!- MSP_consulter_poussee_bulletin
!- MSP_modifier_poussee_bulletin
!- MSP_consulter_separation
!- MSP_modifier_separation
!- MSP_consulter_attitude_tabulee
!- MSP_modifier_attitude_tabulee
!- MSP_consulter_attitude_spinnee
!- MSP_modifier_attitude_spinnee
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

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    character(LEN=*), intent(IN), optional   :: nom
    real(KIND=PM_REEL), intent(IN), optional :: date_ref
    type(tm_jour_sec ), intent(IN), optional :: date_js
    integer, intent(IN), optional            :: origdat

    ! variables locales
!    real(KIND=PM_REEL)   :: date
    type(tm_code_retour) :: code_retour
    logical :: date_changee
    type(tm_jour_sec ) :: old_date
    real(kind=pm_reel) :: ecart_date,old_date_frac,new_date_frac,date_loi
    type(MSP_LOI), pointer :: ptr => NULL()
    integer :: typdate, nb_dates
    real(kind=pm_reel), dimension(:), pointer :: dates_loi=>NULL()
    

    date_changee = .false.
    old_date = scenario%date_ref
    ecart_date = 0._pm_reel

    ! corps de la fonction
    if (PRESENT(date_js)) then
       scenario%date_ref = date_js
       date_changee = .true.
    else if (PRESENT(date_ref)) then
       call md_jourfrac_joursec(date_ref, scenario%date_ref, code_retour)
       date_changee = .true.
    endif

    if (date_changee) then
       ! La date a été changée, il faut modifier les dates relatives des lois
       call md_joursec_jourfrac(scenario%date_ref,new_date_frac, code_retour)
       call md_joursec_jourfrac(old_date,old_date_frac, code_retour)
       ! les dates relatives sont en secondes
       ecart_date = (old_date_frac - new_date_frac)*86400
       ! Parcours de la listeParcours de la liste
       ptr => scenario%tete_liste
       do while (associated(ptr))
          SELECT CASE (ptr%type_loi)

          CASE(MSP_ENUM_LOI_IMP)
             call MSP_consulter_impulsion(ptr%loi_imp,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_impulsion(ptr%loi_imp,date=date_loi)
                date_loi = date_loi + ecart_date
                call MSP_modifier_impulsion(ptr%loi_imp,date=date_loi)
             end if

          CASE(MSP_ENUM_LOI_CONT)
             call MSP_consulter_poussee_continue(ptr%loi_cont,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_poussee_continue(ptr%loi_cont,ntab=nb_dates,dates=dates_loi)
                dates_loi = dates_loi + ecart_date
                call MSP_modifier_poussee_continue(ptr%loi_cont,dates=dates_loi)
                if (associated(dates_loi)) deallocate(dates_loi)
             end if
          
          CASE(MSP_ENUM_LOI_BUL)
             call MSP_consulter_poussee_bulletin(ptr%loi_bul,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_poussee_bulletin(ptr%loi_bul,datedeb=date_loi)
                date_loi = date_loi + ecart_date
                call MSP_modifier_poussee_bulletin(ptr%loi_bul,datdeb=date_loi)
             end if

          CASE(MSP_ENUM_LOI_SEP)
             call MSP_consulter_separation(ptr%loi_sep,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_separation(ptr%loi_sep,date=date_loi)
                date_loi = date_loi + ecart_date
                call MSP_modifier_separation(ptr%loi_sep,date=date_loi)
             end if
             
          CASE(MSP_ENUM_LOI_ATTI_TAB)
             call MSP_consulter_attitude_tabulee(ptr%loi_atti_tab,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_attitude_tabulee(ptr%loi_atti_tab,dates=dates_loi)
                dates_loi = dates_loi + ecart_date
                call MSP_modifier_attitude_tabulee(ptr%loi_atti_tab,dates=dates_loi)
                if (associated(dates_loi)) deallocate(dates_loi)
             end if
          
          CASE(MSP_ENUM_LOI_ATTI_SPIN)
             call MSP_consulter_attitude_spinnee(ptr%loi_atti_spin,typdat=typdate)
             if (typdate == MSP_ENUM_LOI_RELATIVE) then
                ! Loi relative, on doit changer la date
                call MSP_consulter_attitude_spinnee(ptr%loi_atti_spin,datedeb=date_loi)
                date_loi = date_loi + ecart_date
                call MSP_modifier_attitude_spinnee(ptr%loi_atti_spin,datedeb=date_loi)
                call MSP_consulter_attitude_spinnee(ptr%loi_atti_spin,datefin=date_loi)
                date_loi = date_loi + ecart_date
                call MSP_modifier_attitude_spinnee(ptr%loi_atti_spin,datefin=date_loi)
             end if
          
          end SELECT

          ptr => ptr%suivant
       end do !Parcours de la liste des lois
    end if ! Si date changee
       
    if (PRESENT(nom))      scenario%nom      = nom
    if (PRESENT(origdat))  scenario%origdat  = origdat

  end SUBROUTINE MSP_modifier_scenario_gen


  SUBROUTINE MSP_modifier_scenario_imp(scenario, loi_imp, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_imp
!
!$Resume
!  Modification d'une loi impulsionnelle d'un scenario par une autre loi impulsionnelle
!
!$Description
!  Modification d'une loi impulsionnelle d'un scenario par une autre loi impulsionnelle
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_imp(scenario, loi_imp, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION) :: loi_imp
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario contenant la loi à modifier
!>E     loi_imp   :<MSP_IMPULSION>      loi impulsionnelle à mettre dans le scenario
!>[E]   id        :<integer>            Numéro identifiant de la loi à substituer
!>[E]   nom       :<LEN=*>              Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO MODIFIER IMPULSION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_IMPULSION), intent(IN) :: loi_imp

    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr  => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_imp" )) return

    else
       ptr => scenario%loi_courante
    end if
    ptr%loi_imp = loi_imp

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
       if (MSP_gen_messages("MSP_modifier_scenario_imp" )) return

  end SUBROUTINE MSP_modifier_scenario_imp



  SUBROUTINE MSP_modifier_scenario_cont(scenario, loi_cont, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_cont
!
!$Resume
!  Modification d'une loi continue d'un scenario par une autre loi continue
!
!$Description
!  Modification d'une loi continue d'un scenario par une autre loi continue
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_cont(scenario, loi_cont, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_CONTINUE) :: loi_cont
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       Scenario contenant la loi à modifier
!>E     loi_cont  :<MSP_POUSSEE_CONTINUE>   loi continue à mettre dans le scenario
!>[E]   id        :<integer>                Numéro identifiant de la loi à substituer
!>[E]   nom       :<LEN=*>                  Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_effacer_poussee_continue
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO MODIFIER POUSSEE CONTINUE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_POUSSEE_CONTINUE), intent(IN) :: loi_cont
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_cont" )) return

    else
       ptr => scenario%loi_courante
    end if

    if ( associated (ptr%loi_cont) ) then
       call MSP_effacer_poussee_continue(ptr%loi_cont)
    endif
    ptr%loi_cont = loi_cont

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
    if (MSP_gen_messages("MSP_modifier_scenario_cont" )) return

  end SUBROUTINE MSP_modifier_scenario_cont


  SUBROUTINE MSP_modifier_scenario_bul(scenario, loi_bul, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_bul
!
!$Resume
!  Modification d'une loi bulletin d'un scenario par une autre loi bulletin
!
!$Description
!  Modification d'une loi bulletin d'un scenario par une autre loi bulletin
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_bul(scenario, loi_bul, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_POUSSEE_BULLETIN) :: loi_bul
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       Scenario contenant la loi à modifier
!>E     loi_bul   :<MSP_POUSSEE_BULLETIN>   loi bulletin à mettre dans le scenario
!>[E]   id        :<integer>                Numéro identifiant de la loi à substituer
!>[E]   nom       :<LEN=*>                  Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO MODIFIER POUSSEE BULLETIN
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_POUSSEE_BULLETIN), intent(IN) :: loi_bul
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_bul" )) return

    else
       ptr => scenario%loi_courante
    end if
    ptr%loi_bul = loi_bul

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
    if (MSP_gen_messages("MSP_modifier_scenario_bul" )) return

  end SUBROUTINE MSP_modifier_scenario_bul

  SUBROUTINE MSP_modifier_scenario_spin(scenario, loi_atti_spin, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_spin
!
!$Resume
!  Modification d'une loi d'attitude spinnée d'un scenario par une autre loi d'attitude spinnée
!
!$Description
!  Modification d'une loi d'attitude spinnée d'un scenario par une autre loi d'attitude spinnée
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_spin(scenario, loi_atti_spin, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario       :<MSP_SCENARIO_LOI>       Scenario contenant la loi à modifier
!>E     loi_atti_spin  :<MSP_ATTITUDE_SPINNEE>   loi d'attitude spinnée à mettre dans le scenario
!>[E]   id             :<integer>                Numéro identifiant de la loi à substituer
!>[E]   nom            :<LEN=*>                  Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO ATTITUDE SPINNEE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_ATTITUDE_SPINNEE), intent(IN) :: loi_atti_spin
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_spin" )) return

    else
       ptr => scenario%loi_courante
    end if
    ptr%loi_atti_spin = loi_atti_spin

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
    if (MSP_gen_messages("MSP_modifier_scenario_spin" )) return

  end SUBROUTINE MSP_modifier_scenario_spin

  SUBROUTINE MSP_modifier_scenario_tab(scenario, loi_atti_tab, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_tab
!
!$Resume
!  Modification d'une loi d'attitude tabulée d'un scenario par une autre loi d'attitude tabulée
!
!$Description
!  Modification d'une loi d'attitude tabulée d'un scenario par une autre loi d'attitude tabulée
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_tab(scenario, loi_atti_tab, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario      :<MSP_SCENARIO_LOI>       Scenario contenant la loi à modifier
!>E     loi_atti_tab  :<MSP_ATTITUDE_TABULEE>   loi d'attitude tabulée à mettre dans le scenario
!>[E]   id            :<integer>                Numéro identifiant de la loi à substituer
!>[E]   nom           :<LEN=*>                  Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_effacer_attitude_tabulee
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO ATTITUDE TABULEE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_ATTITUDE_TABULEE), intent(IN) :: loi_atti_tab
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_tab" )) return

    else
       ptr => scenario%loi_courante
    end if
    if ( associated (ptr%loi_atti_tab) )  then
       call MSP_effacer_attitude_tabulee(ptr%loi_atti_tab)
    endif
    ptr%loi_atti_tab = loi_atti_tab

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
    if (MSP_gen_messages("MSP_modifier_scenario_tab" )) return


  end SUBROUTINE MSP_modifier_scenario_tab

  SUBROUTINE MSP_modifier_scenario_sep(scenario, loi_sep, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_scenario_sep
!
!$Resume
!  Modification d'une loi séparation d'un scenario par une autre loi séparation
!
!$Description
!  Modification d'une loi séparation d'un scenario par une autre loi séparation
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_modifier_scenario_sep(scenario, loi_sep, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_SEPARATION) :: loi_sep
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario contenant la loi à modifier
!>E     loi_sep   :<MSP_SEPARATION>     loi séparation à mettre dans le scenario
!>[E]   id        :<integer>            Numéro identifiant de la loi à substituer
!>[E]   nom       :<LEN=*>              Nom de la loi à substituer
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_effacer_separation
!- MSP_ranger_scenario
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO SEPARATION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_SEPARATION), intent(IN) :: loi_sep
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom

    type(MSP_LOI), pointer :: ptr => NULL()

    if (PRESENT(id).or.PRESENT(nom)) then 
       call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
       if (MSP_gen_messages("MSP_modifier_scenario_sep" )) return

    else
       ptr => scenario%loi_courante
    end if
    if ( associated (ptr%loi_sep) ) then
       call MSP_effacer_separation(ptr%loi_sep)
    endif
    ptr%loi_sep = loi_sep

    ! Range le scenario dans l'ordre des dates croissante et verifie qu'il n'y a pas 
    ! de chevauchement
    call MSP_ranger_scenario(scenario)
    if (MSP_gen_messages("MSP_modifier_scenario_sep" )) return

  end SUBROUTINE MSP_modifier_scenario_sep
      
  function MSP_creer_scenario (type,date_ref,nom,date_js,origdat) result (scenario)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_scenario
!
!$Resume
!  Sous-programme permettant de créer un scénario.
!
!$Description
!  Sous-programme permettant de créer un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  scenario = MSP_creer_scenario (type,date_ref,nom,[date_js],[origdat])
!.    integer :: type
!.    real(kind=pm_reel) :: date_ref
!.    character(LEN=*) :: nom
!.    type(tm_jour_sec ) :: date_js
!.    integer :: origdat
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     type      :<integer>            type de scénario
!>E     date_ref  :<pm_reel>            date de référence [JJ]
!>E     nom       :<LEN=*>              nom du scénario
!>[E]   date_js   :<tm_jour_sec>        Date de reference en jour / secondes
!>[E]   origdat   :<integer>            Origine des dates (0=MJD1950, 1=MJD2000)
!>S     scenario  :<MSP_SCENARIO_LOI>   structure contenant le scenario
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO CREER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      integer, intent(IN)            :: type
      real(kind=pm_reel), intent(IN) :: date_ref
      character(LEN=*), intent(IN)   :: nom
      type(tm_jour_sec ), intent(IN), optional :: date_js
      integer, intent(IN), optional  :: origdat

      ! retour de la fonction
      type(MSP_SCENARIO_LOI)         :: scenario

      ! variables locales
!      real(KIND=PM_REEL)   :: date
      type(tm_code_retour) :: code_retour
      integer :: lnom
      character(LEN=80),dimension(2)       :: tmessage_var

      ! corps de la fonction
      scenario%flag_func = .true.
      scenario%type_scenario     = type

      if (PRESENT(date_js)) then 
         scenario%date_ref = date_js
      else
         call md_jourfrac_joursec(date_ref, scenario%date_ref, code_retour)
      endif

      lnom = LEN_TRIM(nom)
      if ( lnom <=  MSP_LONG_CHAINE ) then 
         scenario%nom  = nom
      else
         scenario%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom du scenario'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_creer_scenario",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif

      scenario%origdat = 0
      if (PRESENT(origdat)) scenario%origdat = origdat

      scenario%nloi = 0
      NULLIFY(scenario%tete_liste)
      NULLIFY(scenario%loi_courante)

   end function MSP_creer_scenario


   SUBROUTINE MSP_egaler_scenario (scenario_out, scenario_in)
    
!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_scenario
!
!$Resume
!  Sous-programme permettant de copier un scénario.
!
!$Description
!  Sous-programme permettant de copier un scénario.
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_scenario (scenario_out, scenario_in)
!.    type(MSP_SCENARIO_LOI) :: scenario_in
!.    type(MSP_SCENARIO_LOI) :: scenario_out
!
!$Arguments
!>E/S   scenario_out  :<MSP_SCENARIO_LOI>   structure contenant la copie du scenario
!>E     scenario_in   :<MSP_SCENARIO_LOI>   structure contenant le scenario initial
!
!$Common
!
!$Routines
!- MSP_supprimer_liste_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO EGALER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_SCENARIO_LOI), intent(in) :: scenario_in
      type(MSP_SCENARIO_LOI), intent(inout) :: scenario_out
    
      ! Variables locales
      type(MSP_LOI), pointer :: ptr_in => NULL()
      type(MSP_LOI), pointer :: ptr_out => NULL()
      type(MSP_LOI), pointer :: ptr_prec => NULL()
      logical :: tstloicour

      ! Initialisations
      NULLIFY (ptr_prec)
      tstloicour=.false.

      call MSP_supprimer_liste_loi(scenario_out)
      if ( MSP_gen_messages ("MSP_egaler_scenario") ) return

      ! Affectation des éléments hors pointeurs
      scenario_out%flag_func = .false.
      scenario_out%type_scenario      = scenario_in%type_scenario
      scenario_out%nloi      = scenario_in%nloi
      scenario_out%nom       = scenario_in%nom
      scenario_out%date_ref  = scenario_in%date_ref
      scenario_out%origdat   = scenario_in%origdat

      ! scenario_out est vide, on s'assure juste que les pointeurs sont
      ! correctement initialisés
      nullify(scenario_out%tete_liste)
      nullify(scenario_out%loi_courante)

      ! Gestion des pointeurs
      ! Création d'une nouvelle liste chaînée
      if ( ASSOCIATED(scenario_in%tete_liste) ) then

         ! Tête de litse
         ALLOCATE (scenario_out%tete_liste)
         scenario_out%tete_liste = scenario_in%tete_liste
         if ( MSP_gen_messages ("MSP_egaler_scenario") ) return

         NULLIFY (scenario_out%tete_liste%precedent)

         ! Pointeurs de gestion 
         ptr_in   => scenario_in%tete_liste
         ptr_out  => scenario_out%tete_liste
         ptr_prec => scenario_out%tete_liste

         ! Pour affecter le pointeur loi courante si besoin
         tstloicour = ASSOCIATED(scenario_in%loi_courante)

         if (tstloicour) then
            ! -- Positionnement de la loi_courante
            if(associated(ptr_in, scenario_in%loi_courante)) &
                 scenario_out%loi_courante=>ptr_out
         endif

         ! suite de la liste chaînée
         do while (ASSOCIATED(ptr_in%suivant))

            ALLOCATE (ptr_out%suivant)
            ptr_out%suivant = ptr_in%suivant
            ptr_out%suivant%precedent => ptr_prec

            ! si les champs identifiant sont égaux
            if (tstloicour) then
               ! -- Positionnement de la loi_courante
               if(associated(ptr_in, scenario_in%loi_courante)) &
                    scenario_out%loi_courante=>ptr_out
            endif
            if ( MSP_gen_messages ("MSP_egaler_scenario") ) return
            
            ptr_prec => ptr_out%suivant
            ptr_out  => ptr_out%suivant
            ptr_in   => ptr_in%suivant
         end do

      endif

   end SUBROUTINE MSP_egaler_scenario


   SUBROUTINE MSP_egaler_loi (loi_out, loi_in)
    
!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_egaler_loi
!
!$Resume
!  Sous-programme permettant de copier une loi.
!
!$Description
!  Sous-programme permettant de copier une loi.
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_egaler_loi (loi_out, loi_in)
!.    type(MSP_LOI) :: loi_in
!.    type(MSP_LOI) :: loi_out
!
!$Arguments
!>E/S   loi_out  :<MSP_LOI>   structure contenant la copie de la loi
!>E     loi_in   :<MSP_LOI>   structure contenant la loi initiale
!
!$Common
!
!$Routines
!- MSP_effacer_loi
!
!$Include
!
!$Module
!
!$Remarques
!   Il faur que les lois soient déjà allouées.
!
!$Mots-cles
!  SCENARIO LOI EGALER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_LOI), intent(IN) :: loi_in
      type(MSP_LOI), intent(INOUT) :: loi_out

      ! On effectue la copie:

      loi_out%flag_func = .false.
      loi_out%id = loi_in%id
      loi_out%nom = loi_in%nom
      loi_out%type_loi = loi_in%type_loi

      SELECT CASE (loi_in%type_loi)

         CASE(MSP_ENUM_LOI_IMP)
            ALLOCATE(loi_out%loi_imp)
            loi_out%loi_imp = loi_in%loi_imp
            if ( MSP_gen_messages ("MSP_egale_loi") ) return

         CASE(MSP_ENUM_LOI_CONT)
            ALLOCATE(loi_out%loi_cont)
            loi_out%loi_cont = loi_in%loi_cont
            if ( MSP_gen_messages ("MSP_egale_loi") ) return

         CASE(MSP_ENUM_LOI_BUL)
            ALLOCATE(loi_out%loi_bul)
            loi_out%loi_bul = loi_in%loi_bul
            if ( MSP_gen_messages ("MSP_egale_loi") ) return
          
         CASE(MSP_ENUM_LOI_SEP)
            ALLOCATE(loi_out%loi_sep)
            loi_out%loi_sep = loi_in%loi_sep
            if ( MSP_gen_messages ("MSP_egale_loi") ) return
          
         CASE(MSP_ENUM_LOI_ATTI_TAB)
            ALLOCATE(loi_out%loi_atti_tab)
            loi_out%loi_atti_tab = loi_in%loi_atti_tab
            if ( MSP_gen_messages ("MSP_egale_loi") ) return

         CASE(MSP_ENUM_LOI_ATTI_SPIN)
            ALLOCATE(loi_out%loi_atti_spin)
            loi_out%loi_atti_spin = loi_in%loi_atti_spin
            if ( MSP_gen_messages ("MSP_egale_loi") ) return

      end SELECT

   end SUBROUTINE MSP_egaler_loi

   SUBROUTINE MSP_supprimer_loi (scenario,nom,id)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_supprimer_loi
!
!$Resume
!  Sous-programme permettant de supprimer une loi dans un scénario.
!
!$Description
!  Sous-programme permettant de supprimer une loi dans un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_supprimer_loi (scenario,[nom],[id])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   structure contenant le scénario
!>[E]   nom       :<LEN=*>              nom de la loi
!>[E]   id        :<integer>            numéro de la loi
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_effacer_poussee_continue
!- MSP_effacer_separation
!- MSP_effacer_attitude_tabulee
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO LOI SUPPRIMER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT)   :: scenario
      integer, intent(IN), optional          :: id
      character(LEN=*), intent(IN), optional :: nom

      type(MSP_LOI), pointer :: ptr_tmp => NULL()
      type(MSP_LOI), pointer :: ptr_avant => NULL()
      type(MSP_LOI), pointer :: ptr_apres => NULL()
      logical :: loi_trouvee
      
      integer :: MSP_iostat
      
      MSP_iostat = 0

      loi_trouvee = .false.
      if ( present(nom) ) then
         if ( present(id) ) then
            call MSP_signaler_message (cle_mes="MSP_scenario_002")
         endif
         ! Recherche de la loi:
         ptr_tmp => scenario%tete_liste
         do while ( ASSOCIATED(ptr_tmp) )
            if ( ptr_tmp%nom == nom ) then
               loi_trouvee = .true.
               exit
            endif
            ptr_tmp => ptr_tmp%suivant
         enddo
      else
         if ( .not. present(id) ) then
            call MSP_signaler_message (cle_mes="MSP_scenario_003")
            return
         else
            ! Recherche du numéro de la loi:
            ptr_tmp => scenario%tete_liste
            do while ( ASSOCIATED(ptr_tmp) )
               if ( ptr_tmp%id == id ) then
                  loi_trouvee = .true.
                  exit
               endif
               ptr_tmp => ptr_tmp%suivant
            enddo
         endif
      endif

      if ( loi_trouvee ) then

         ! Nouvel enchainement:
         ptr_avant => ptr_tmp%precedent
         ptr_apres => ptr_tmp%suivant

         if ( ASSOCIATED(ptr_avant) ) then
            if ( ASSOCIATED (ptr_avant%suivant) ) NULLIFY (ptr_avant%suivant)
            if ( ASSOCIATED(ptr_apres) ) then
               ptr_avant%suivant   => ptr_apres
            endif

            ! -- Positionnement de loi_courante sur la loi précédente
            scenario%loi_courante => ptr_avant
         else
            ! -- Positionnement de loi_courante sur la premiere loi
            scenario%loi_courante => ptr_apres

         endif
         if ( ASSOCIATED(ptr_apres) ) then
            if ( ASSOCIATED(ptr_avant) ) then
               ptr_apres%precedent => ptr_avant
            else
               scenario%tete_liste => ptr_apres
               NULLIFY(scenario%tete_liste%precedent)
            endif
         endif

         ! Désallocation mémoire de la loi supprimée:
         select case (ptr_tmp%type_loi)
            case (MSP_ENUM_LOI_IMP)
               if ( ASSOCIATED (ptr_tmp%loi_imp) ) DEALLOCATE (ptr_tmp%loi_imp,stat=MSP_iostat)
            case (MSP_ENUM_LOI_CONT)
               if ( associated (ptr_tmp%loi_cont) ) then
                  call MSP_effacer_poussee_continue(ptr_tmp%loi_cont)
                  deallocate (ptr_tmp%loi_cont,stat=MSP_iostat)
               endif
            case (MSP_ENUM_LOI_BUL)
               if ( ASSOCIATED (ptr_tmp%loi_bul) ) DEALLOCATE (ptr_tmp%loi_bul,stat=MSP_iostat)
            case (MSP_ENUM_LOI_SEP)
               if ( associated (ptr_tmp%loi_sep) ) then
                  call MSP_effacer_separation(ptr_tmp%loi_sep)
                  deallocate (ptr_tmp%loi_sep,stat=MSP_iostat)
               endif
            case (MSP_ENUM_LOI_ATTI_TAB)
               if ( associated (ptr_tmp%loi_atti_tab) )  then
                  call MSP_effacer_attitude_tabulee(ptr_tmp%loi_atti_tab)
                  deallocate (ptr_tmp%loi_atti_tab)
               endif
            case (MSP_ENUM_LOI_ATTI_SPIN)
               if ( ASSOCIATED (ptr_tmp%loi_atti_spin) ) DEALLOCATE (ptr_tmp%loi_atti_spin,stat=MSP_iostat)
         end select
         if ( ASSOCIATED (ptr_tmp) ) DEALLOCATE (ptr_tmp,stat=MSP_iostat)

         ! Nouvelle numérotation des lois:
         ptr_tmp => ptr_apres
         do while ( ASSOCIATED(ptr_tmp) )
            ptr_tmp%id = ptr_tmp%id - 1
            ptr_tmp => ptr_tmp%suivant
         enddo

         scenario%nloi = scenario%nloi - 1

         if ( scenario%nloi == 0 ) then
            if ( ASSOCIATED (scenario%tete_liste) ) then
               NULLIFY (scenario%tete_liste)
            endif
            if ( ASSOCIATED (scenario%loi_courante) ) then
               NULLIFY (scenario%loi_courante)
            endif

         endif
         

      else

         call MSP_signaler_message (cle_mes="MSP_scenario_004")

      endif

   end subroutine MSP_supprimer_loi



  SUBROUTINE MSP_supprimer_liste_loi(scenario)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_supprimer_liste_loi
!
!$Resume
!  Destruction complète de la liste des lois du scenario
!
!$Description
!  Destruction complète de la liste des lois du scenario
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_supprimer_liste_loi(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scenario pour lequel on veut supprimer la liste des lois
!
!$Common
!
!$Routines
!- MSP_supprimer_loi
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO LOI SUPPRIMER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI) :: scenario
    type(MSP_LOI), pointer :: ptr => NULL()

    ! Si aucun scenario n'existe dans la liste
    if (.not.ASSOCIATED(scenario%tete_liste)) return

    ! Recherche de la derniere loi
    ptr => MSP_derniere_loi (scenario%tete_liste)

    do while (ASSOCIATED(ptr%precedent))
       ptr => ptr%precedent
       call MSP_supprimer_loi(scenario, id=ptr%suivant%id)
    end do

    ! Suppression de la premiere loi
    call MSP_supprimer_loi(scenario, id=scenario%tete_liste%id)
    

  end SUBROUTINE MSP_supprimer_liste_loi


  SUBROUTINE MSP_effacer_loi(loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_loi
!
!$Resume
!  Effacer le contenu d'une loi
!
!$Description
!  Effacer le contenu d'une loi
!
!$Auteur
!  Jean-jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_loi(loi)
!.    type (MSP_LOI) :: loi
!
!$Arguments
!>E/S   loi  :<MSP_LOI>   
!
!$Common
!
!$Routines
!- MSP_effacer_poussee_continue
!- MSP_effacer_separation
!- MSP_effacer_attitude_tabulee
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO LOI EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
    
    type (MSP_LOI) :: loi
    
    integer :: MSP_iostat
    
    MSP_iostat = 0

    
    SELECT CASE (loi%type_loi)

    CASE(MSP_ENUM_LOI_IMP)
       DEALLOCATE(loi%loi_imp,stat=MSP_iostat)

    CASE(MSP_ENUM_LOI_CONT)
       call MSP_effacer_poussee_continue(loi%loi_cont)
       DEALLOCATE(loi%loi_cont,stat=MSP_iostat)

    CASE(MSP_ENUM_LOI_BUL)
       DEALLOCATE(loi%loi_bul,stat=MSP_iostat)
          
    CASE(MSP_ENUM_LOI_SEP)
       call MSP_effacer_separation(loi%loi_sep)
       DEALLOCATE(loi%loi_sep,stat=MSP_iostat)
          
    CASE(MSP_ENUM_LOI_ATTI_TAB)
       call MSP_effacer_attitude_tabulee(loi%loi_atti_tab)
       DEALLOCATE(loi%loi_atti_tab,stat=MSP_iostat)

    CASE(MSP_ENUM_LOI_ATTI_SPIN)
       DEALLOCATE(loi%loi_atti_spin,stat=MSP_iostat)

    end SELECT

    loi%id = 0
    loi%nom = ""
    loi%type_loi = 0

  end SUBROUTINE MSP_effacer_loi

  subroutine MSP_effacer_scenario(scenario,nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_scenario
!
!$Resume
!  Routine d'effacement d'un scénario, permettant de tout désallouer, et
!  permettant aussi d'initialiser correctement les pointeurs du scénario.
!
!$Description
!  Routine d'effacement d'un scénario, permettant de tout désallouer, et
!  permettant aussi d'initialiser correctement les pointeurs du scénario.
!
!$Auteur
!  Y. TANGUY (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_scenario(scenario,[nul])
!.    type (MSP_SCENARIO_LOI) :: scenario
!.    logical :: nul
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scénario à effacer/initialiser
!>[E]   nul       :<logical>            vrai si l'on veut simplement forcer 
!                                       les pointeurs à NUL (cas d'une initialisation)
!
!$Common
!
!$Routines
!- msp_effacer_loi
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
    type (MSP_SCENARIO_LOI), intent(inout) :: scenario
    logical, optional, intent(in)          :: nul
    
    ! Variables locales
    logical :: nul_tmp
    type (MSP_loi), pointer :: ptr_loi, ptr_loi_suiv

    ! Routine utilisée pour mettre à 0 un scénario "local"
    ! qui va être affecté par un nouveau scénario
    ! --> ceci permet d'éviter que les pointeurs ne pointent nulle part..
    !=============================================
 
    nullify (ptr_loi)
    nullify (ptr_loi_suiv)
    
    if (present(nul)) then
       nul_tmp = nul
    else 
       nul_tmp = .false.
    end if

    if (nul_tmp) then

       ! On force les deux pointeurs à NUL
       ! -> ceci doit être fait lorsque une 
       ! variable locale de type MSP_SCENARIO vient d'être déclaré

       nullify(scenario%tete_liste)
       nullify(scenario%loi_courante)
    else
       
       ! Effacement total du scénario, en parcourant la liste 
       ! des lois.

       ptr_loi => scenario%tete_liste
       
       do while (associated(ptr_loi))
          ptr_loi_suiv => ptr_loi%suivant
          call msp_effacer_loi(ptr_loi)
          deallocate(ptr_loi)
          ptr_loi => ptr_loi_suiv
       end do

       nullify(scenario%tete_liste)

    end if
    

    scenario%nloi = 0
    scenario%date_ref%jour = 0
    scenario%date_ref%sec = 0._pm_reel

  end subroutine MSP_effacer_scenario



  FUNCTION MSP_loi_courante(scenario) result (ptr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_courante
!
!$Resume
!  Récupère le pointeur courant du scenario
!
!$Description
!  Récupère le pointeur courant du scenario
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  ptr = MSP_loi_courante(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: ptr
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   structure scenario
!>S     ptr       :<MSP_LOI,pointer>    pointeur vers la loi courante
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
!  SCENARIO LOI COURANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(IN) :: scenario
    type(MSP_LOI), pointer :: ptr

    ptr => scenario%loi_courante

  end FUNCTION MSP_loi_courante


  logical FUNCTION MSP_test_loi_courante(scenario) result (test)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_courante
!
!$Resume
!  Routine de test d'existence d'une loi suivante
!
!$Description
!  Routine de test d'existence d'une loi suivante
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_courante(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario sur lequel on désire apliquer le test
!>S     test      :<logical>            valeur logique du test
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
!  SCENARIO LOI COURANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(IN) :: scenario

    test = ASSOCIATED(scenario%loi_courante)

  end FUNCTION MSP_test_loi_courante


  SUBROUTINE MSP_loi_suivante_scenar(scenario)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_suivante_scenar
!
!$Resume
!  Routine permettant d'incrémenter le pointeur courant vers la loi suivante
!
!$Description
!  Routine permettant d'incrémenter le pointeur courant vers la loi suivante
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_loi_suivante_scenar(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scenario sur lequel on veut appliquer cette incrément
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
!  SCENARIO LOI SUIVANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

    scenario%loi_courante => scenario%loi_courante%suivant

  end SUBROUTINE MSP_loi_suivante_scenar


  SUBROUTINE MSP_loi_suivante_loi(loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_suivante_loi
!
!$Resume
!  Routine permettant d'incrémenter le pointeur loi vers la loi suivante
!
!$Description
!  Routine permettant d'incrémenter le pointeur loi vers la loi suivante
!
!$Auteur
!  26/07/2000 - Jean-Jacques Wasbauer
!
!$Acces
!  PRIVE
!
!$Usage
!  call MSP_loi_suivante_loi(loi)
!.    type(MSP_LOI), pointer :: loi
!
!$Arguments
!>E/S   loi  :<MSP_LOI,pointer>   Pointeur vers la loi suivante
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
!  SCENARIO LOI SUIVANTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_LOI), pointer :: loi

    loi => loi%suivant

  end SUBROUTINE MSP_loi_suivante_loi

  SUBROUTINE MSP_loi_precedente_scenar(scenario)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_precedente_scenar
!
!$Resume
!  Routine permettant d'incrémenter le pointeur courant vers la loi precedente
!
!$Description
!  Routine permettant d'incrémenter le pointeur courant vers la loi precedente
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_loi_precedente_scenar(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scenario sur lequel on veut appliquer cette incrément
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
!  SCENARIO LOI PRECEDENTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

    scenario%loi_courante => scenario%loi_courante%precedent

  end SUBROUTINE MSP_loi_precedente_scenar


  SUBROUTINE MSP_loi_precedente_loi(loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_loi_precedente_loi
!
!$Resume
!  Routine permettant d'incrémenter le pointeur loi vers la loi precedente
!
!$Description
!  Routine permettant d'incrémenter le pointeur loi vers la loi precedente
!
!$Auteur
!  26/07/2000 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_loi_precedente_loi(loi)
!.    type(MSP_LOI), pointer :: loi
!
!$Arguments
!>E/S   loi  :<MSP_LOI,pointer>   Pointeur vers la loi precedente
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
!  SCENARIO LOI PRECEDENTE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_LOI), pointer :: loi

    loi => loi%precedent

  end SUBROUTINE MSP_loi_precedente_loi


  SUBROUTINE MSP_premiere_loi(scenario, loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_premiere_loi
!
!$Resume
!  Routine permettant de réinitialiser le pointeur courant à la tête dela liste de lois
!
!$Description
!  Routine permettant de réinitialiser le pointeur courant à la tête dela liste de lois
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_premiere_loi(scenario, [loi])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: loi
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scenario sur lequel on veut appliquer cette réinitialisation
!>[E/S] loi       :<MSP_LOI,pointer>    Loi dans laquelle sera retournée le pointeur vers le début de la liste
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
!  SCENARIO LOI PREMIERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
    type(MSP_LOI), pointer, optional :: loi

    scenario%loi_courante => scenario%tete_liste

    if (PRESENT(loi)) then 
       loi => scenario%tete_liste
    end if

  end SUBROUTINE MSP_premiere_loi

   function MSP_derniere_loi (tete) result(ptr)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_derniere_loi
!
!$Resume
!  Fonction retournant la dernière loi d'un scénario.
!
!$Description
!  Fonction retournant la dernière loi d'un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  ptr = MSP_derniere_loi (tete)
!.    type(MSP_LOI), pointer :: tete
!.    type(MSP_LOI), pointer :: ptr
!
!$Arguments
!>E/S   tete  :<MSP_LOI,pointer>   pointeur sur la tête de liste du scénario.
!>S     ptr   :<MSP_LOI,pointer>   pointeur sur la dernière loi du scénario.
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
!  SCENARIO LOI DERNIERE
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      type(MSP_LOI), pointer :: tete
      type(MSP_LOI), pointer :: ptr

      ptr => tete
      do while (ASSOCIATED(ptr%suivant))
         ptr => ptr%suivant
      end do

   end function MSP_derniere_loi

   subroutine MSP_rechercher_loi_dates (scenario,tdebut,tfinal,ptr_avant,ptr_apres)

!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  MSP_rechercher_loi_dates
!
!$Resume
!  Sous-programme retournant les lois entre lesquelles insérer une nouvelle loi.
!
!$Description
!  Sous-programme retournant les lois entre lesquelles insérer une nouvelle loi.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rechercher_loi_dates (scenario,tdebut,tfinal,ptr_avant,ptr_apres)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    real(kind=pm_reel) :: tdebut,tfinal
!.    type(MSP_LOI), pointer :: ptr_avant,ptr_apres
!
!$Arguments
!>E     scenario   :<MSP_SCENARIO_LOI>   structure contenant le scenario
!>E     tdebut     :<pm_reel>            date de début de la nouvelle loi.
!>E     tfinal     :<pm_reel>            date de fin de la nouvelle loi.
!>E/S   ptr_avant  :<MSP_LOI,pointer>    pointeur sur la loi de propulsion précédant la nouvelle.
!>E/S   ptr_apres  :<MSP_LOI,pointer>    pointeur sur la loi de propulsion suivant la nouvelle.
!
!$Common
!
!$Routines
!- MSP_dates_loi
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO LOI RECHERCHER
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

      implicit none

      ! Arguements
      type(MSP_SCENARIO_LOI), intent(IN) :: scenario
      real(kind=pm_reel), intent(IN)     :: tdebut,tfinal
      type(MSP_LOI), pointer             :: ptr_avant,ptr_apres

      ! Variables locales
      type(MSP_LOI), pointer             :: ptr => NULL()
      real(kind=pm_reel) :: t1,t2,t2_avant
      logical :: t2_avant_init
      
      ! Corps de la fonction
      t2_avant=0._pm_reel
      t2_avant_init = .false.
      NULLIFY(ptr_avant)
      ptr => scenario%tete_liste

      do while ( ASSOCIATED(ptr) )
         call MSP_dates_loi(scenario, ptr, t1, t2)
         if ( tfinal <= t1 ) then
            ! Deux lois ne peuvent s'enchevetrer
            if ((tdebut < t2_avant).and.(t2_avant_init)) then
               call MSP_signaler_message (cle_mes="MSP_scenario_005")
               return
            end if
            ! Sinon c'est bon, on a trouvé
            exit
         else if ( tdebut >= t2 ) then
            ptr_avant => ptr
            ptr => ptr%suivant
            t2_avant = t2
            t2_avant_init = .true.
         else
            call MSP_signaler_message (cle_mes="MSP_scenario_001")
            return
         endif
      end do

      ptr_apres => ptr
      
   end subroutine MSP_rechercher_loi_dates



  SUBROUTINE MSP_rechercher_loi(scenario, ptr, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_rechercher_loi
!
!$Resume
!  Recherche d'une loi à partir de son nom ou de son numero identifiant
!
!$Description
!  Recherche d'une loi à partir de son nom ou de son numero identifiant
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rechercher_loi(scenario, ptr, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_LOI), pointer :: ptr
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   Scenario sur lequel s'applique la recherche
!>E/S   ptr       :<MSP_LOI,pointer>    Pointer vers la loi recherchée
!>[E]   id        :<integer>            Numéro identifiant de la loi recherchée
!>[E]   nom       :<LEN=*>              nom de la loi recherchée
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
!  SCENARIO LOI RECHERCHER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_SCENARIO_LOI), intent(IN) :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom
    type(MSP_LOI), pointer :: ptr
    
    logical :: test
    character(LEN= MSP_LONG_CHAINE) :: string
    
    if (PRESENT(id).and.PRESENT(nom)) then 
       call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_004", &
            routine="MSP_rechercher_loi", type=MSP_ENUM_ERREUR, &
            partie_variable="id et nom")
       return
    end if
    
    test = .false.

    ptr => scenario%tete_liste
    do while(ASSOCIATED(ptr))
       if (PRESENT(id))  test = ( ptr%id == id )
       if (PRESENT(nom)) test = ( ptr%nom(1:LEN_TRIM(ptr%nom)) == nom(1:LEN_TRIM(nom)) )
       if (test) exit
       ptr => ptr%suivant
    end do
    if (.not.ASSOCIATED(ptr)) then
       if (PRESENT(id)) write(string, '(''ID='',I2)')
       if (PRESENT(nom)) string="nom="//trim(nom)
       call MSP_signaler_message (cle_mes="MSP_rechercher_loi_001", partie_variable=string)
       return
    end if

  end SUBROUTINE MSP_rechercher_loi


   SUBROUTINE MSP_inserer_loi (scenario,ptr_loi,t1,t2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_inserer_loi
!
!$Resume
!  Insertion d'une loi dans un scénario.
!
!$Description
!  Insertion d'une loi dans un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_inserer_loi (scenario,ptr_loi,t1,t2)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI), pointer :: ptr_loi
!.    real(KIND=pm_reel) :: t1,t2
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario dans lequel la loi doit être insérée
!>E/S   ptr_loi   :<MSP_LOI,pointer>    pointeur vers la loi à insérer
!>E     t1        :<pm_reel>            date de début de la loi
!>E     t2        :<pm_reel>            date de fin de la loi
!
!$Common
!
!$Routines
!- MSP_rechercher_loi_dates
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO LOI INSERER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      type(MSP_LOI), pointer                :: ptr_loi
      real(KIND=pm_reel), intent(IN)        :: t1,t2
      
      type(MSP_LOI), pointer :: ptr_tmp => NULL()
      type(MSP_LOI), pointer :: ptr_avant => NULL()
      type(MSP_LOI), pointer :: ptr_apres => NULL()

      if ( ASSOCIATED(scenario%tete_liste) ) then
         call MSP_rechercher_loi_dates (scenario,t1,t2,ptr_avant,ptr_apres)

         if  (MSP_gen_messages("MSP_inserer_loi")) return
         ! Cas où on insère une loi:
         if ( ASSOCIATED(ptr_apres) ) then
            ptr_loi%suivant     => ptr_apres
            ptr_apres%precedent => ptr_loi
            ! Cas où on insère au début du scénario:
            if ( .not. ASSOCIATED(ptr_avant) ) then
               scenario%tete_liste => ptr_loi
            else
               ptr_loi%precedent   => ptr_avant
               ptr_avant%suivant   => ptr_loi
            endif
        ! Cas où on est en fin de chaine:
         else
            ptr_apres           => ptr_loi
            ptr_apres%precedent => ptr_avant
            ptr_avant%suivant   => ptr_apres
         endif
         ptr_tmp => ptr_loi
         do while ( ASSOCIATED(ptr_tmp) )
            if ( ASSOCIATED(ptr_tmp%precedent) ) then
               ptr_tmp%id = ptr_tmp%precedent%id + 1
            else
               ptr_tmp%id = 1
            endif
            ptr_tmp => ptr_tmp%suivant
         enddo
         scenario%nloi = scenario%nloi + 1
      else
         scenario%tete_liste => ptr_loi
         scenario%tete_liste%id = 1
         scenario%nloi = 1
         scenario%tete_liste%suivant => NULL()
      endif

      ! -- Positionnement du pointeur loi_courante
      scenario%loi_courante => ptr_loi

   end subroutine MSP_inserer_loi

   subroutine MSP_ranger_scenario(scenario)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ranger_scenario
!
!$Resume
!  Routine permettant de ranger les lois par ordre chronologique
!
!$Description
!  Routine permettant de ranger les lois par ordre chronologique
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ranger_scenario(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   Scenario à réorganiser
!
!$Common
!
!$Routines
!- MSP_dates_loi
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, les origine des dates doivent etre identiques
!
!$Mots-cles
!  SCENARIO LOI RANGER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     implicit none
     
     type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

     ! Variables locales
     type(MSP_LOI), pointer :: ptr_prec => NULL()
     type(MSP_LOI), pointer :: ptr_prec2 => NULL()
     type(MSP_LOI), pointer :: ptr_suiv => NULL()
     type(MSP_LOI), pointer :: ptr => NULL()
     real(KIND=PM_REEL) :: datedeb, datefin, t1, t2
     integer :: tmp_id

     ! Corps de la fonctions
     ptr => scenario%tete_liste
     call MSP_dates_loi(scenario, ptr, datedeb, datefin)
     ptr => ptr%suivant

     do while (ASSOCIATED(ptr))
        
        call MSP_dates_loi(scenario, ptr, t1, t2)
        if ( ( (t1 >= datedeb).and.(t1 < datefin ) ).or. &
             ( (t2 > datedeb) .and.(t2 <= datefin) ) ) then 
           call MSP_signaler_message (cle_mes="MSP_scenario_001")
           return
        end if

        ! Permutation des deux lois
        if ((t2 < datedeb).or.((t2 == datedeb).and.(datedeb < datefin))) then 
           ptr_prec  => ptr%precedent
           ptr_prec2 => ptr_prec%precedent
           ptr_suiv  => ptr%suivant
           tmp_id = ptr_prec%id
           
           if (ASSOCIATED(ptr_prec2)) then 
              ptr_prec2%suivant => ptr
              ptr%precedent => ptr_prec2
           else
              scenario%tete_liste => ptr
              NULLIFY(scenario%tete_liste%precedent)
           end if
  
           ptr_prec%precedent => ptr
           if (ASSOCIATED(ptr_suiv)) then 
              ptr_prec%suivant => ptr_suiv
              ptr_suiv%precedent => ptr_prec
           else
              NULLIFY(ptr_prec%suivant)
           end if
           ptr%suivant => ptr_prec
           

           ! Conservation de la numerotation croissante apres permutation
           ptr_prec%id = ptr%id
           ptr%id = tmp_id

           ! Redémarrage de la boucle en tete de liste
           ptr => scenario%tete_liste
           call MSP_dates_loi(scenario, ptr, datedeb, datefin)

        else
    
           datedeb = t1
           datefin = t2

        end if

        ptr => ptr%suivant

     end do

     NULLIFY(ptr_prec)
     NULLIFY(ptr_prec2)
     NULLIFY(ptr_suiv)

   end subroutine MSP_ranger_scenario


   subroutine MSP_dates_loi(scenario, loi, t1, t2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_dates_loi
!
!$Resume
!  Retourne les dates de debut et de fin d'une loi
!
!$Description
!  Retourne les dates de debut et de fin d'une loi relativement a la date
!  de reference du scenario
!
!$Auteur
!  J. J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_dates_loi(scenario, loi, t1, t2)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_LOI) :: loi
!.    real(KIND=PM_REEL) :: t1, t2
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   Scenario contenant la loi
!>E     loi       :<MSP_LOI>            Loi dont on veut connaitre les dates de debut et de fin
!>S     t1        :<PM_REEL>            Date de debut
!>S     t2        :<PM_REEL>            Date de fin
!
!$Common
!
!$Routines
!- MSP_consulter_impulsion
!- MSP_consulter_poussee_continue
!- MSP_consulter_poussee_bulletin
!- MSP_consulter_attitude_tabulee
!- MSP_consulter_attitude_spinnee
!- MSP_consulter_separation
!- md_joursec_jourfrac
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO LOI CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     implicit none

     type(MSP_SCENARIO_LOI), intent(IN) :: scenario
     type(MSP_LOI), intent(IN)  :: loi
     real(KIND=PM_REEL), intent(OUT) :: t1, t2

     ! variables locales
     integer :: typdat
     real(KIND=PM_REEL) :: ttmp
     type(tm_code_retour) :: code_retour

     ! Corps de la fonction
     SELECT CASE (scenario%type_scenario)
     CASE (MSP_ENUM_PROPULSION)
        SELECT CASE (loi%type_loi)
        CASE(MSP_ENUM_LOI_IMP)
           call MSP_consulter_impulsion(loi%loi_imp, typdat=typdat, datedeb=t1, &
                datefin=t2)
        CASE(MSP_ENUM_LOI_CONT)
           call MSP_consulter_poussee_continue(loi%loi_cont, typdat=typdat, &
                datedeb=t1, datefin=t2)
        CASE(MSP_ENUM_LOI_BUL)
           call MSP_consulter_poussee_bulletin(loi%loi_bul, typdat=typdat, datedeb=t1, &
                datefin=t2)
        end SELECT
     CASE (MSP_ENUM_ATTITUDE)
        select case (loi%type_loi)
        case(MSP_ENUM_LOI_ATTI_TAB)
           call MSP_consulter_attitude_tabulee(loi%loi_atti_tab, typdat=typdat, &
                datedeb=t1, datefin=t2)
        case(MSP_ENUM_LOI_ATTI_SPIN)
           call MSP_consulter_attitude_spinnee(loi%loi_atti_spin, typdat=typdat, &
                datedeb=t1, datefin=t2)
        end select
     CASE (MSP_ENUM_SEPARATION)
        call MSP_consulter_separation(loi%loi_sep, typdat=typdat, date=t1)
        t2 = t1
     END SELECT

     ! Dates relatives a la date de reference
     call md_joursec_jourfrac(scenario%date_ref, ttmp, code_retour)

     if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then
        t1 = (t1-ttmp)*86400._pm_reel
        t2 = (t2-ttmp)*86400._pm_reel
      else if (typdat == MSP_ENUM_LOI_ABSOREL) then
        t1 = (t1-ttmp)*86400._pm_reel
        t2 = t2 + t1
     else if ( (scenario%type_scenario == MSP_ENUM_PROPULSION) .and. &
               (loi%type_loi == MSP_ENUM_LOI_BUL) ) then
        t2 = (t2-ttmp)*86400._pm_reel
     endif

   end subroutine MSP_dates_loi



  integer function MSP_type_loi(scenario, loi, id, nom) result(type_loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_type_loi
!
!$Resume
!  Routine retournant le type de la loi
!
!$Description
!  Routine retournant le type de la loi
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  type_loi = MSP_type_loi([scenario], [loi], [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!.    type(MSP_LOI), pointer :: loi
!
!$Arguments
!>[E]   scenario  :<MSP_SCENARIO_LOI>   scenario contenant la loi à examiner
!>[E/S] loi       :<MSP_LOI,pointer>    Loi à examiner
!>[E]   id        :<integer>            Numéro identifiant de la loi à examiner
!>[E]   nom       :<LEN=*>              Nom de la loi à examiner
!>S     type_loi  :<integer>            Type de la loi recherchée
!
!$Common
!
!$Routines
!- MSP_rechercher_loi
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO LOI CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguements
    type(MSP_SCENARIO_LOI), intent(IN), optional :: scenario
    integer, intent(IN), optional :: id
    character(LEN=*), intent(IN), optional :: nom
    type(MSP_LOI), pointer, optional :: loi

    ! variables locales
    type(MSP_LOI), pointer :: ptr => NULL()

    ! Initialisation de la sortie
    type_loi = 0

    ! Corps de la fonction
    if (PRESENT(loi)) then 
         ptr => loi
    elseif (PRESENT(scenario)) then
       if (PRESENT(id).or.PRESENT(nom)) then
          call MSP_rechercher_loi(scenario, ptr, id=id, nom=nom)
          if (MSP_gen_messages("MSP_type_loi")) return
       else
          ptr => scenario%loi_courante
       endif
    else
       !ERREUR
       call MSP_signaler_message (cle_mes="MSP_type_loi_001")
       type_loi = 0
       return       
    endif

    type_loi = ptr%type_loi

  end function MSP_type_loi


   SUBROUTINE MSP_ajouter_attitude_spinnee (scenario,nom,loi,typdat,datdeb,datfin, &
        typrep,psi0,teta0,phi0,psip,tetap,phip,typangle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_attitude_spinnee
!
!$Resume
!  Ajout d'une loi d'attitude tabulée en temps.
!
!$Description
!  Ajout d'une loi d'attitude tabulée en temps.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_attitude_spinnee (scenario,nom,[loi],[typdat],[datdeb],[datfin], &
!.            [typrep],[psi0],[teta0],[phi0],[psip],[tetap],[phip],[typangle])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_SPINNEE) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb,datfin
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi0,teta0,phi0
!.    real(KIND=pm_reel) :: psip,tetap,phip
!.    integer :: typangle
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>[E]   loi       :<MSP_ATTITUDE_SPINNEE>   type contenant les informations de la loi (généré par appel à MSP_creer_attitude_spinnee)
!>[E]   typdat    :<integer>                type de date (MSP_ENUM_LOI_ABSOLUE, MSP_ENUM_LOI_ABSOREL ou MSP_ENUM_LOI_RELATIVE)
!>[E]   datdeb    :<pm_reel>                date de début (JJ CNES ou s)
!>[E]   datfin    :<pm_reel>                date de fin (JJ CNES ou s)
!>[E]   typrep    :<integer>                type du repère dans lequel sont exprimés les angles:
!.                                          MSP_ENUM_ATTI_INERTIEL_G50 => Repère Gamma 50 CNES
!.					    MSP_ENUM_ATTI_INERTIEL_J2000 => Repère J2000
!.					    MSP_ENUM_ATTI_INERTIEL_GVrai => Repère Gamma vrai de la date
!.                                          MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                          MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                          MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                          MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                          MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!>[E/S] psi0      :<pm_reel>                lacet ou incidence initial [rad] [par défaut 0.]
!>[E/S] teta0     :<pm_reel>                tangage ou dérapage initial [rad] [par défaut 0.]
!>[E/S] phi0      :<pm_reel>                roulis ou gite initial [rad] [par défaut 0.]
!>[E/S] psip      :<pm_reel>                vitesse en lacet ou incidence [rad/s] [par défaut 0.]
!>[E/S] tetap     :<pm_reel>                vitesse en tangage ou dérapage [rad/s] [par défaut 0.]
!>[E/S] phip      :<pm_reel>                vitesse en roulis ou gite [rad/s] [par défaut 0.]
!>[E]   typangle  :<integer>                
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_attitude_spinnee
!- MSP_inserer_loi
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas aue l'origine des dates est coherente
!
!$Mots-cles
!  SCENARIO AJOUTER ATTITUDE SPINNEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN) :: nom
      type(MSP_ATTITUDE_SPINNEE), intent(IN), optional   :: loi
      integer, intent(IN), optional  :: typdat
      real(KIND=pm_reel), intent(IN),  optional  :: datdeb,datfin
      integer, intent(IN), optional  :: typrep
      real(KIND=pm_reel),  optional  :: psi0,teta0,phi0
      real(KIND=pm_reel),  optional  :: psip,tetap,phip
      integer, optional, intent(in) :: typangle

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: t1,t2, date_ref
      character(LEN=80),dimension(2)       :: tmessage_var
      integer :: convention

      ! On teste la cohérence des arguments en entrée:
      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_attitude_spinnee") ) return

      if ( PRESENT(loi) ) then
         if ( PRESENT(typdat) .or. PRESENT(datdeb) .or. PRESENT(datfin) .or. PRESENT(typrep) .or.&
              PRESENT(psi0) .or. PRESENT(teta0) .or. PRESENT(phi0) .or. &
              PRESENT(psip) .or. PRESENT(tetap) .or. PRESENT(phip) ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_spin_001")
         endif
      else
         if ( .not.PRESENT(typdat) .or. .not.PRESENT(datdeb) .or. .not.PRESENT(datfin) .or. .not.PRESENT(typrep) ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_spin_002")
            return
         endif
      endif

      if ( type /= MSP_ENUM_ATTITUDE ) then
         call MSP_signaler_message (cle_mes="MSP_attitude_spin_003")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi d''attitude spinnée'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_attitude_spinnee",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_ATTI_SPIN
      ALLOCATE (ptr_loi%loi_atti_spin)

      if(present (typangle)) then
         convention = typangle
      else
         convention = pm_1z_2y_3x
      end if

      if ( PRESENT(loi) ) then
         ptr_loi%loi_atti_spin = loi
         call MSP_consulter_attitude_spinnee(ptr_loi%loi_atti_spin, typdat=type_date, &
              datedeb=t1, datefin=t2,typangle=convention)
      else
         ptr_loi%loi_atti_spin = MSP_creer_attitude_spinnee(typdat,datdeb,datfin,&
              typrep, psi0=psi0,teta0=teta0,phi0=phi0,psip=psip,tetap=tetap,&
              phip=phip, origdat=scenario%origdat, typangle=convention)
         type_date = typdat
         t1 = datdeb
         t2 = datfin
     endif

     ! rendre les dates comparables
     if ( type_date == MSP_ENUM_LOI_ABSOLUE ) then
        t1 = (t1 - date_ref)*86400._pm_reel
        t2 = (t2 - date_ref)*86400._pm_reel
     elseif ( type_date == MSP_ENUM_LOI_ABSOREL ) then
        t1 = (t1 - date_ref)*86400._pm_reel
        t2 = t2 + t1
     endif
 
      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,t1,t2)

      if  (MSP_gen_messages("MSP_ajouter_attitude_spinnee")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         ! Pas de pointeur dans une loi spin donc pas de fonction effacer
         if (associated(ptr_loi%loi_atti_spin)) deallocate(ptr_loi%loi_atti_spin)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if

   end SUBROUTINE MSP_ajouter_attitude_spinnee


   SUBROUTINE MSP_ajouter_attitude_tabulee (scenario,nom,loi,ntab,typdat,dates,typrep,psi,teta,phi,typangle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_attitude_tabulee
!
!$Resume
!  Ajout d'une loi d'attitude tabulée en temps.
!
!$Description
!  Ajout d'une loi d'attitude tabulée en temps.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_attitude_tabulee (scenario,nom,[loi],[ntab],[typdat],[dates],[typrep],[psi],[teta],[phi],[typangle])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_ATTITUDE_TABULEE) :: loi
!.    integer :: ntab
!.    integer :: typdat
!.    real(KIND=pm_reel) :: dates(:)
!.    integer :: typrep
!.    real(KIND=pm_reel) :: psi(:),teta(:),phi(:)
!.    integer :: typangle
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>[E]   loi       :<MSP_ATTITUDE_TABULEE>   type contenant les informations de la loi (généré par appel à MSP_creer_attitude_tabulee)
!>[E]   ntab      :<integer>                nombre de points de tabulation
!>[E]   typdat    :<integer>                type de date (MSP_ENUM_LOI_ABSOLUE, MSP_ENUM_LOI_ABSOREL ou MSP_ENUM_LOI_RELATIVE)
!>[E]   dates     :<pm_reel,DIM=(:)>        date (JJ CNES ou s)
!>[E]   typrep    :<integer>                type du repère dans lequel sont exprimés les angles:
!.                                             MSP_ENUM_ATTI_INERTIEL_G50 => Repère Gamma 50 CNES
!.					       MSP_ENUM_ATTI_INERTIEL_J2000 => Repère J2000
!.					       MSP_ENUM_ATTI_INERTIEL_GVrai => Repère Gamma vrai de la date
!.                                             MSP_ENUM_ATTI_QSW => Orbital local (QSW)
!.                                             MSP_ENUM_ATTI_TNW => Orbital local (TNW)
!.                                             MSP_ENUM_ATTI_POINTE_SOLAIRE => Pointé solaire
!.                                             MSP_ENUM_ATTI_TOPO_LOCAL => Topocentrique local
!.                                             MSP_ENUM_ATTI_AERODYNAMIQUE => incidence, dérapage, gite
!>[E]   psi       :<pm_reel,DIM=(:)>        lacet ou incidence [rad] [par défaut 0.]
!>[E]   teta      :<pm_reel,DIM=(:)>        tangage ou dérapage [rad] [par défaut 0.]
!>[E]   phi       :<pm_reel,DIM=(:)>        roulis ou gite [rad] [par défaut 0.]
!>[E]   typangle  :<integer>                
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_attitude_tabulee
!- MSP_inserer_loi
!- MSP_effacer_attitude_tabulee
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO AJOUTER ATTITUDE TABULEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN) :: nom
      type(MSP_ATTITUDE_TABULEE), intent(IN), optional   :: loi
      integer, intent(IN), optional             :: ntab
      integer, intent(IN), optional             :: typdat
      real(KIND=pm_reel), intent(IN), optional  :: dates(:)
      integer, intent(IN), optional             :: typrep
      real(KIND=pm_reel), intent(IN), optional  :: psi(:),teta(:),phi(:)
      integer, intent(in), optional             :: typangle

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: t1,t2,date_ref
      character(LEN=80),dimension(2)       :: tmessage_var
      integer :: convention
      logical :: loi_creee_ici = .false.


      ! On teste la cohérence des arguments en entrée:
      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_attitude_tabulee") ) return

      if ( PRESENT(loi) ) then
         if (PRESENT(ntab) .or.  PRESENT(typdat) .or. PRESENT(dates) .or. PRESENT(typrep) ) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_001")
         endif
      else
         if ( .not.PRESENT(ntab) .or. .not.PRESENT(typdat) .or. .not.PRESENT(dates) .or. .not.PRESENT(typrep)) then
            call MSP_signaler_message (cle_mes="MSP_attitude_tab_002")
            return
         endif
      endif

      if ( type /= MSP_ENUM_ATTITUDE ) then
         call MSP_signaler_message (cle_mes="MSP_attitude_tab_003")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi d''attitude tabulée'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_attitude_tabulee",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_ATTI_TAB
      ALLOCATE (ptr_loi%loi_atti_tab)

      if( present(typangle)) then
         convention = typangle
      else
         convention = pm_1z_2y_3x
      end if

      if ( PRESENT(loi) ) then
         ptr_loi%loi_atti_tab = loi
         call MSP_consulter_attitude_tabulee(ptr_loi%loi_atti_tab, typdat=type_date, &
              datedeb=t1, datefin=t2, typangle=convention)
      else
         ptr_loi%loi_atti_tab = MSP_creer_attitude_tabulee (ntab,typdat,dates,typrep,psi=psi, &
              teta=teta,phi=phi, origdat=scenario%origdat,typangle=convention)
         type_date=typdat
         t1 = dates(1)
         t2 = dates(ntab)
         loi_creee_ici = .true.
      endif
      
      ! rendre les dates comparables
      if ( type_date == MSP_ENUM_LOI_ABSOLUE ) then
         t1 = (t1 - date_ref)*86400._pm_reel
         t2 = (t2 - date_ref)*86400._pm_reel
      else if ( type_date == MSP_ENUM_LOI_ABSOREL ) then
         t1 = (t1 - date_ref)*86400._pm_reel
         t2 = t2 + t1
      endif

      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,t1,t2)

      if  (MSP_gen_messages("MSP_ajouter_attitude_tabulee")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         if (loi_creee_ici) call MSP_effacer_attitude_tabulee(ptr_loi%loi_atti_tab)
         if (associated(ptr_loi%loi_atti_tab)) deallocate(ptr_loi%loi_atti_tab)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if

   end SUBROUTINE MSP_ajouter_attitude_tabulee

   SUBROUTINE MSP_ajouter_poussee_bulletin (scenario,nom,loi,typdat,datdeb,bul,merg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_poussee_bulletin
!
!$Resume
!  Ajout d'une loi de poussée de type bulletin.
!
!$Description
!  Ajout d'une loi de poussée de type bulletin.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_poussee_bulletin (scenario,nom,[loi],[typdat],[datdeb],[bul],[merg])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_BULLETIN) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: datdeb
!.    type(MSP_BULLETIN) :: bul
!.    real(KIND=pm_reel) :: merg
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>[E]   loi       :<MSP_POUSSEE_BULLETIN>   type contenant les informations de la loi (généré par appel à MSP_creer_poussee_bulletin)
!>[E]   typdat    :<integer>                type de date (MSP_ENUM_LOI_ABSOLUE, MSP_ENUM_LOI_ABSOREL ou MSP_ENUM_LOI_RELATIVE)
!>[E]   datdeb    :<pm_reel>                date (JJ CNES ou s)
!>[E]   bul       :<MSP_BULLETIN>           bulletin obtenu à la fin de la poussée
!>[E]   merg      :<pm_reel>                masse d'ergols dépensée pendant la poussée [kg] [par défaut 0.]
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_poussee_bulletin
!- MSP_consulter_bulletin
!- MSP_inserer_loi
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO AJOUTER POUSSEE BULLETIN
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT)            :: scenario
      character(LEN=*), intent(IN)                     :: nom
      type(MSP_POUSSEE_BULLETIN), intent(IN), optional :: loi
      integer, intent(IN), optional                    :: typdat
      real(KIND=pm_reel), intent(in), optional         :: datdeb
      type(MSP_BULLETIN), intent(IN), optional         :: bul

      real(KIND=pm_reel), intent(in), optional         :: merg

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: t1, t2, datbul, date_ref
      character(LEN=80),dimension(2)       :: tmessage_var

      ! On teste la cohérence des arguments en entrée:
      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_poussee_bulletin") ) return

      if ( PRESENT(loi) ) then
         if ( PRESENT(typdat) .or. PRESENT(datdeb) .or. PRESENT(bul) .or. PRESENT(merg) ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_bul_001")
         endif
      else
         if ( .not.PRESENT(typdat) .or. .not.PRESENT(datdeb) .or. .not.PRESENT(bul) ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_bul_001")
            return
         endif
      endif

      if ( type /= MSP_ENUM_PROPULSION ) then
         call MSP_signaler_message (cle_mes="MSP_poussee_bul_001")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi de type bulletin'
         write(tmessage_var(2),'(I8)')  MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_poussee_bulletin",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_BUL
      ALLOCATE (ptr_loi%loi_bul)

      if ( PRESENT(loi) ) then
         ptr_loi%loi_bul = loi
         call MSP_consulter_poussee_bulletin(ptr_loi%loi_bul, typdat=type_date, datedeb=t1, datefin=t2)
         if ( type_date /= MSP_ENUM_LOI_RELATIVE ) then
            t1 = (t1 - date_ref)*86400._pm_reel
         endif
         if ( type_date == MSP_ENUM_LOI_ABSOLUE  ) then
            t2 = (t2 - date_ref)*86400._pm_reel
         endif
      else
         ptr_loi%loi_bul = MSP_creer_poussee_bulletin (typdat,datdeb=datdeb,bul=bul,&
              merg=merg, origdat=scenario%origdat)
         if ( typdat /= MSP_ENUM_LOI_RELATIVE ) then
            t1 = (datdeb - date_ref)*86400._pm_reel
         else 
            t1 = datdeb
         endif
         call MSP_consulter_bulletin(bul, datbul=datbul)
         t2 = (datbul - date_ref)*86400._pm_reel
      endif

      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,t1,t2)

      if  (MSP_gen_messages("MSP_ajouter_poussee_bulletin")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         ! pas de fonction effacer dans poussee_bulletin puisque pas de pointeur
         if (associated(ptr_loi%loi_bul)) deallocate(ptr_loi%loi_bul)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if

   end SUBROUTINE MSP_ajouter_poussee_bulletin


   SUBROUTINE MSP_ajouter_poussee_continue (scenario,nom,loi,ntab,typdat,dates, &
     poussee,isp,dirref,omega,omegap,merg,pas,debit)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_poussee_continue
!
!$Resume
!  Ajout d'une loi de type poussée continue.
!
!$Description
!  Ajout d'une loi de type poussée continue.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_poussee_continue (scenario,nom,[loi],[ntab],[typdat],[dates], &
!.         [poussee],[isp],[dirref],[omega],[omegap],[merg],[pas],[debit])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_POUSSEE_CONTINUE) :: loi
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
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>       scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>                  nom de la loi
!>[E]   loi       :<MSP_POUSSEE_CONTINUE>   type contenant les informations de la loi (généré par appel à MSP_creer_poussee_continue)
!>[E]   ntab      :<integer>                nombre de points de tabulation
!>[E]   typdat    :<integer>                type de date:
!.                                             1 => date [Jours Juliens CNES]
!.                                             2 => durée [s]
!>[E]   dates     :<pm_reel,DIM=(:)>        dates tabulées en ordre croissant [JJ CNES ou s]
!>[E]   poussee   :<pm_reel,DIM=(:)>        poussées tabulées [N]
!>[E]   isp       :<pm_reel>                impulsion spécifique [s]
!>[E]   dirref    :<integer>                repère de référence dans lequel est définie la direction de poussée
!>[E]   omega     :<pm_reel,DIM=(:)>        angle dans le plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   omegap    :<pm_reel,DIM=(:)>        angle hors du plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   merg      :<pm_reel>                masse d'ergols disponible [kg] [par défaut 0.]
!>[E]   pas       :<pm_reel>                pas d'intégration pendant la poussée [s] [par défaut 1.]
!>[E]   debit     :<pm_reel,DIM=(:)>        debits
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_poussee_continue
!- MSP_inserer_loi
!- MSP_effacer_poussee_continue
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO AJOUTER POUSSEE CONTINUE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
      character(LEN=*), intent(IN) :: nom
      type(MSP_POUSSEE_CONTINUE), intent(IN), optional   :: loi
      integer, intent(IN), optional :: ntab
      integer, intent(IN), optional :: typdat
      real(KIND=pm_reel), intent(IN), optional :: dates(:)
      real(KIND=pm_reel), intent(IN), optional :: poussee(:)
      real(KIND=pm_reel), intent(IN), optional :: isp
      integer, intent(IN), optional :: dirref
      real(KIND=pm_reel), intent(IN), optional :: omega(:)
      real(KIND=pm_reel), intent(IN), optional :: omegap(:)
      real(KIND=pm_reel), intent(IN), optional :: merg
      real(KIND=pm_reel), intent(IN), optional :: pas
      real(KIND=pm_reel), intent(IN), optional :: debit(:)

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: t1,t2,date_ref
      character(LEN=80),dimension(2)       :: tmessage_var
      logical :: loi_creee_ici = .false.

      ! On teste la cohérence des arguments en entrée:

      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_poussee_continue") ) return

      if ( PRESENT(loi) ) then
         if (PRESENT(ntab).or.PRESENT(typdat).or.PRESENT(dates).or.PRESENT(poussee).or. &
              PRESENT(isp).or.PRESENT(dirref).or.PRESENT(omega).or.PRESENT(omegap) .or. &
              PRESENT(merg).or.PRESENT(pas).or.PRESENT(debit) ) then
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_001")
         endif
      else
         if (.not.PRESENT(ntab).or..not.PRESENT(typdat).or..not.PRESENT(dates).or. &
             .not.PRESENT(poussee).or..not.PRESENT(isp)) then
            call MSP_signaler_message (cle_mes="MSP_poussee_cont_002")
            return
         endif
      endif

      if ( type /= MSP_ENUM_PROPULSION ) then
         call MSP_signaler_message (cle_mes="MSP_poussee_cont_002")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi de pousséee continue'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_poussee_continue",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_CONT
      ALLOCATE (ptr_loi%loi_cont)

      if ( PRESENT(loi) ) then
         ptr_loi%loi_cont = loi
         call MSP_consulter_poussee_continue(ptr_loi%loi_cont, typdat=type_date, &
              datedeb=t1, datefin=t2)
      else
         ptr_loi%loi_cont = MSP_creer_poussee_continue (ntab,typdat,dates,poussee,isp,&
              dirref=dirref,omega=omega,omegap=omegap,merg=merg,pas=pas, debit=debit,origdat=scenario%origdat)
         loi_creee_ici = .true.
         type_date = typdat
         t1 = dates(1)
         t2 = dates(ntab)
      endif

      if ( type_date == MSP_ENUM_LOI_ABSOLUE ) then
         t1 = (t1 - date_ref)*86400._pm_reel
         t2 = (t2 - date_ref)*86400._pm_reel
      else if ( type_date == MSP_ENUM_LOI_ABSOREL ) then
         t1 = (t1 - date_ref)*86400._pm_reel
         t2 = t2 + t1
      endif
      
      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,t1,t2)
      if  (MSP_gen_messages("MSP_ajouter_poussee_continue")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         if (loi_creee_ici) call MSP_effacer_poussee_continue(ptr_loi%loi_cont)
         if (associated(ptr_loi%loi_cont)) deallocate(ptr_loi%loi_cont)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if


   end SUBROUTINE MSP_ajouter_poussee_continue

   subroutine MSP_ajouter_impulsion (scenario,nom,loi,typdat,date,deltav,dirref, &
        omega,omegap,merg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_impulsion
!
!$Resume
!  Ajout d'une loi de poussée impulsionnelle dans un scénario.
!
!$Description
!  Ajout d'une loi de poussée impulsionnelle dans un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_impulsion (scenario,nom,[loi],[typdat],[date],[deltav],[dirref], &
!.            [omega],[omegap],[merg])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_IMPULSION) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    integer :: dirref
!.    real(KIND=pm_reel) :: omega, omegap, merg
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>              nom de la loi
!>[E]   loi       :<MSP_IMPULSION>      type contenant les informations de la loi (généré par appel à MSP_creer_impulsion)
!>[E]   typdat    :<integer>            type de date (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVE)
!>[E]   date      :<pm_reel>            date (JJ CNES ou s)
!>[E]   deltav    :<pm_reel>            incrément de vitesse [m/s]
!>[E]   dirref    :<integer>            repère de référence dans lequel est définie la direction de poussée
!>[E]   omega     :<pm_reel>            angle dans le plan de symétrie du véhicule (ou dans le repère dirref) [rad] [par défaut 0.]
!>[E]   omegap    :<pm_reel>            angle hors du plan de symétrie du véhicule (ou dans le repère dirref) [rad] [par défaut 0.]
!>[E]   merg      :<pm_reel>            masse d'ergols dépensée pendant la poussée [kg] [par défaut 0.]
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_impulsion
!- MSP_inserer_loi
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente
!
!$Mots-cles
!  SCENARIO AJOUTER IMPULSION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT)             :: scenario
      character(LEN=*), intent(IN)                      :: nom
      type(MSP_IMPULSION), intent(IN), optional         :: loi
      integer, intent(IN), optional                     :: typdat
      real(KIND=pm_reel), intent(in), optional          :: date, deltav
      integer, intent(IN), optional                     :: dirref
      real(KIND=pm_reel), intent(in), optional          :: omega, omegap, merg

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: ttt, date_ref
      character(LEN=80),dimension(2)       :: tmessage_var

      ! On teste la cohérence des arguments en entrée:
      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_impulsion") ) return

      if ( PRESENT(loi) ) then
         if ( PRESENT(typdat).or.PRESENT(date) .or.PRESENT(deltav).or.&
              PRESENT(dirref).or.PRESENT(omega).or.PRESENT(omegap).or.PRESENT(merg)) then
            call MSP_signaler_message (cle_mes="MSP_impulsion_001")
         endif
      else
         if (.not.PRESENT(typdat).or..not.PRESENT(date).or..not.PRESENT(deltav)) then
            call MSP_signaler_message (cle_mes="MSP_impulsion_002")
            return
         endif
      endif

      if ( type /= MSP_ENUM_PROPULSION ) then
         call MSP_signaler_message (cle_mes="MSP_impulsion_003")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi de poussée impulsionelle'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_impulsion",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_IMP
      ALLOCATE (ptr_loi%loi_imp)

      if ( PRESENT(loi) ) then
         ptr_loi%loi_imp = loi
         call MSP_consulter_impulsion(ptr_loi%loi_imp, date=ttt, typdat=type_date)
         if (type_date /= MSP_ENUM_LOI_RELATIVE) then
            ttt = (ttt - date_ref)*86400._pm_reel
         endif
      else
         ptr_loi%loi_imp = MSP_creer_impulsion (typdat,date=date,deltav=deltav,&
              dirref=dirref,omega=omega,omegap=omegap,merg=merg, &
              origdat=scenario%origdat)
         ttt = date
         if ( typdat  /= MSP_ENUM_LOI_RELATIVE) then
            ttt = (date - date_ref)*86400._pm_reel
         else
            ttt = date
         endif
      endif

      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,ttt,ttt)

      if  (MSP_gen_messages("MSP_ajouter_impulsion")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         ! pas de fonction effacer dans impulsion puisque pas de pointeur
         if (associated(ptr_loi%loi_imp)) deallocate(ptr_loi%loi_imp)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if

   end SUBROUTINE MSP_ajouter_impulsion

   subroutine MSP_ajouter_separation (scenario,nom,loi,typdat,date,deltav,omega,omegap,&
        forme,sx,sy,sz,st,spx,spy,spz,tymasse,typcf,merg,aero,prad, date_js)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_ajouter_separation
!
!$Resume
!  Ajout d'une loi de séparation dans un scénario.
!
!$Description
!  Ajout d'une loi de séparation dans un scénario.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_ajouter_separation (scenario,nom,[loi],[typdat],[date],[deltav],[omega],[omegap],&
!.            [forme],[sx],[sy],[sz],[st],[spx],[spy],[spz],[tymasse],[typcf],[merg],[aero],[prad], [date_js])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    character(LEN=*) :: nom
!.    type(MSP_SEPARATION) :: loi
!.    integer :: typdat
!.    real(KIND=pm_reel) :: date, deltav
!.    real(KIND=pm_reel) :: omega, omegap
!.    integer :: forme,tymasse,typcf
!.    real(KIND=pm_reel) :: sx, sy, sz, st, spx, spy, spz, merg
!.    type(tm_jour_sec) :: date_js
!.    type(MSP_AERO) :: aero
!.    type(MSP_PRAD) :: prad
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scénario dans lequel on veut ajouter la loi
!>E     nom       :<LEN=*>              nom de la loi
!>[E]   loi       :<MSP_SEPARATION>     type contenant les informations de la loi (généré par appel à MSP_creer_separation)
!>[E]   typdat    :<integer>            type de date (MSP_ENUM_LOI_ABSOLUE ou MSP_ENUM_LOI_RELATIVE)
!>[E]   date      :<pm_reel>            date (JJ CNES ou s)
!>[E]   deltav    :<pm_reel>            incrément de vitesse [m/s]
!>[E]   omega     :<pm_reel>            angle dans le plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   omegap    :<pm_reel>            angle hors du plan de symétrie du véhicule [rad] [par défaut 0.]
!>[E]   forme     :<integer>            forme du véhicule (MSP_ENUM_SPHERE, MSP_ENUM_PLAQUE,
!>                                           MSP_ENUM_CYLINDRE, MSP_ENUM_PARALLEPIPEDE) [par défaut MSP_ENUM_SPHERE]
!>[E]   sx        :<pm_reel>            surface perpendiculaire à l'axe X du véhicule [m^2] [par défaut 0.]
!>[E]   sy        :<pm_reel>            surface perpendiculaire à l'axe Y du véhicule [m^2] [par défaut 0.]
!>[E]   sz        :<pm_reel>            surface perpendiculaire à l'axe Z du véhicule [m^2] [par défaut 0.]
!>[E]   st        :<pm_reel>            surface transverse de révolution du véhicule [m^2] [par défaut 0.]
!>[E]   spx       :<pm_reel>            surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2] [par défaut 0.]
!>[E]   spy       :<pm_reel>            surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2] [par défaut 0.]
!>[E]   spz       :<pm_reel>            surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2] [par défaut 0.]
!>[E]   tymasse   :<integer>            Type de masse (1:delta, 2:masse absolue [defaut 1]
!>[E]   typcf     :<integer>            type de frottement aero apres separation
!>[E]   merg      :<pm_reel>            masse d'ergols dépensée pendant la séparation
!                                            ou masse après séparation [kg] [par défaut 0.]
!>[E]   aero      :<MSP_AERO>           sous-structure contenant les données aérodynamiques
!                                    après séparation (si absente, newaero vaut false)
!>[E]   prad      :<MSP_PRAD>           sous-structure contenant les données relatives à la pression de radiation
!                                    solaire après séparation (si absente, newprad vaut false)
!>[E]   date_js   :<tm_jour_sec>        date en jour/secondes (si present, remplace date)
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_separation
!- MSP_inserer_loi
!- MSP_effacer_separation
!
!$Include
!
!$Module
!
!$Remarques
!  Attention, on ne verifie pas que l'origine des dates reste coherente entre
!  Les lois
!
!$Mots-cles
!  SCENARIO AJOUTER SEPARATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_SCENARIO_LOI), intent(INOUT)              :: scenario
      character(LEN=*), intent(IN)                       :: nom
      type(MSP_SEPARATION), intent(IN), optional         :: loi
      integer, intent(IN), optional                      :: typdat
      real(KIND=pm_reel), intent(in), optional           :: date, deltav

      real(KIND=pm_reel), intent(in), optional           :: omega, omegap
      integer,  intent(in), optional                     :: forme,tymasse,typcf
      real(KIND=pm_reel), intent(in), optional           :: sx, sy, sz, st, spx, spy, spz, merg
      type(tm_jour_sec), intent(IN), optional  :: date_js
      type(MSP_AERO), intent(IN), optional :: aero
      type(MSP_PRAD), intent(IN), optional :: prad

      ! Variables internes:
      integer :: type_date, type
      type(MSP_LOI), pointer :: ptr_loi => NULL()
      real(KIND=pm_reel) :: ttt, date_ref
      character(LEN=80),dimension(2)       :: tmessage_var
      logical :: loi_creee_ici = .false.

      ! On teste la cohérence des arguments en entrée:
      call MSP_consulter_scenario(scenario, type=type, date_ref=date_ref)
      if ( MSP_gen_messages("MSP_ajouter_separation") ) return

      if ( PRESENT(loi) ) then
         if ( PRESENT(typdat) .or. PRESENT(date) .or. PRESENT(deltav) .or. &
              PRESENT(omega) .or. PRESENT(omegap) .or. PRESENT(merg) .or. &
              PRESENT(forme) .or. PRESENT(sx) .or. PRESENT(sy) .or. PRESENT(sz) .or. &
              PRESENT(spx) .or. PRESENT(spy) .or. PRESENT(spz).or. &
              present(date_js).or.present(prad).or.present(aero)) then
            call MSP_signaler_message (cle_mes="MSP_separation_001")
         endif
      else
         if ( .not.PRESENT(typdat) .or. .not.PRESENT(date) .or. .not.PRESENT(deltav) ) then
            call MSP_signaler_message (cle_mes="MSP_separation_002")
            return
         endif
      endif

      if ( type /= MSP_ENUM_SEPARATION ) then
         call MSP_signaler_message (cle_mes="MSP_separation_003")
         return
      endif

      ! On remplit les champs de données:

      ALLOCATE (ptr_loi)
      ptr_loi%flag_func = .false.
      if ( LEN_TRIM(nom) <=  MSP_LONG_CHAINE ) then 
         ptr_loi%nom  = nom
      else
         ptr_loi%nom  = nom(1: MSP_LONG_CHAINE)
         tmessage_var(1) = 'Le nom de la loi de séparation'
         write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

         call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
            routine="MSP_ajouter_separation",type=MSP_ENUM_WARNING, &
            partie_variable=tmessage_var)
      endif
      ptr_loi%type_loi = MSP_ENUM_LOI_SEP
      ALLOCATE (ptr_loi%loi_sep)

      if ( PRESENT(loi) ) then
         ptr_loi%loi_sep = loi
         call MSP_consulter_separation(ptr_loi%loi_sep, typdat=type_date, date=ttt)
         if (type_date /= MSP_ENUM_LOI_RELATIVE) then
            ttt = (ttt - date_ref)*86400._pm_reel
         endif
      else
         ptr_loi%loi_sep = MSP_creer_separation(typdat,date=date,deltav=deltav,&
              omega=omega,omegap=omegap,date_js=date_js, origdat=scenario%origdat, &
              forme=forme,sx=sx,sy=sy,sz=sz,st=st,spx=spx,spy=spy,spz=spz,&
              tymasse=tymasse,typcf=typcf,merg=merg,aero=aero,prad=prad)
         ttt = date
         if ( typdat /= MSP_ENUM_LOI_RELATIVE) then
            ttt = (date - date_ref)*86400._pm_reel
         else
            ttt = date
         endif
         loi_creee_ici = .true.
      endif

      ! On insère la loi:
      call MSP_inserer_loi (scenario,ptr_loi,ttt,ttt)

      if  (MSP_gen_messages("MSP_ajouter_separation")) then
         ! On doit desallouer la memoire de la loi si on l'a creee ici
         if (loi_creee_ici) call MSP_effacer_separation(ptr_loi%loi_sep)
         if (associated(ptr_loi%loi_sep)) deallocate(ptr_loi%loi_sep)
         if (associated(ptr_loi)) deallocate(ptr_loi)
         return
      end if

   end SUBROUTINE MSP_ajouter_separation

     subroutine MSP_afficher_scenario (scenario,ilog)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_scenario
!
!$Resume
!  Routine permettant d'afficher les caractéristiques d'un scenario
!
!$Description
!  Routine permettant d'afficher les caractéristiques d'un scenario
!
!$Auteur
!  Jean-jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_scenario (scenario,[ilog])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: ilog
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario à afficher
!>[E/S] ilog      :<integer>            Numéro logique d'affichage
!
!$Common
!
!$Routines
!- MSP_afficher_impulsion
!- MSP_afficher_poussee_continue
!- MSP_afficher_poussee_bulletin
!- MSP_afficher_separation
!- MSP_afficher_attitude_tabulee
!- MSP_afficher_attitude_spinnee
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  SCENARIO AFFICHER SCENARIO
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none

       type(MSP_SCENARIO_LOI), intent(IN) :: scenario
       integer, optional :: ilog

       type(MSP_LOI), pointer :: ptr_tmp => NULL()
       integer :: num

       ! variables locales
       character(len=8) :: oo

       ! Ecriture
       oo="MJD1950"
       if (scenario%origdat == 1) oo="MJD2000"

       if ( present(ilog) ) then
          num = ilog
       else
          num = MSP_ENUM_ECRAN
       endif

       write(num,'(a,a)') "NOM DU SCENARIO: ",trim(scenario%nom)
       write(num,'(a,a,i9,a,g21.12)') "Date de reference:  ",oo,scenario%date_ref%jour, "-", scenario%date_ref%sec
       write(num,'(a,i9)') "NOMBRE DE LOIS:  ",scenario%nloi
       write(num,'(a)') ""

       ptr_tmp => scenario%tete_liste
       do while ( ASSOCIATED(ptr_tmp) )
          write(num,'(a,i9)') "NUMERO: ",ptr_tmp%id
          write(num,'(a,a)') "NOM:    ",trim(ptr_tmp%nom)
          select case (ptr_tmp%type_loi)
          case(MSP_ENUM_LOI_IMP)
             call MSP_afficher_impulsion(ptr_tmp%loi_imp, num)
             write(num,'(a)') ""
          case(MSP_ENUM_LOI_CONT)
             call MSP_afficher_poussee_continue(ptr_tmp%loi_cont, num)
             write(num,'(a)') ""
          case(MSP_ENUM_LOI_BUL)
             call MSP_afficher_poussee_bulletin(ptr_tmp%loi_bul, num)
             write(num,'(a)') ""
          case(MSP_ENUM_LOI_SEP)
             call MSP_afficher_separation(ptr_tmp%loi_sep, num)
             write(num,'(a)') ""
          case(MSP_ENUM_LOI_ATTI_TAB)
             call MSP_afficher_attitude_tabulee(ptr_tmp%loi_atti_tab, num)
             write(num,'(a)') ""
          case(MSP_ENUM_LOI_ATTI_SPIN)
             call MSP_afficher_attitude_spinnee(ptr_tmp%loi_atti_spin, num)
             write(num,'(a)') ""
          end select
          ptr_tmp => ptr_tmp%suivant
       enddo

     end subroutine MSP_afficher_scenario
     

     logical function MSP_test_loi_atti_tab(scenario) result(test)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_atti_tab
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_tab(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen : .true. / .false.
!booleen : .true. / .false.
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
!$Nom
!  MSP_test_loi_atti_tab
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
! Teste si la loi courante d'un scenario est de type loi d'attitude tabulée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_tab(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen : .true. / .false.
!booleen : .true. / .false.
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
       
       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_ATTI_TAB)

     end function MSP_test_loi_atti_tab

     logical function MSP_test_loi_atti_spin(scenario) result(test)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_atti_spin
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_spin(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_atti_spin
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Description
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
! Teste si la loi courante d'un scenario est de type loi d'attitude spinnée
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_atti_spin(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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

       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_ATTI_SPIN)

     end function MSP_test_loi_atti_spin

     logical function MSP_test_loi_sep(scenario) result(test)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_sep
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_sep(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario 
!scenario 
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_sep
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de séparation
! Teste si la loi courante d'un scenario est de type loi de séparation
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_sep(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario 
!scenario 
!>S     test      :<logical>            booleen
!booleen
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

       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_SEP)

     end function MSP_test_loi_sep

     logical function MSP_test_loi_imp(scenario) result(test)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_imp
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_imp(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_imp
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
! Teste si la loi courante d'un scenario est de type loi de propulsion impulsionnelle
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_imp(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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

       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_IMP)

     end function MSP_test_loi_imp

     logical function MSP_test_loi_cont(scenario) result(test)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_cont
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_cont(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   
!
!>S     test      :<logical>            
!
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
!$Nom
!  MSP_test_loi_cont
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
! Teste si la loi courante d'un scenario est de type loi de propulsion continue
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_cont(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   
!
!>S     test      :<logical>            
!
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

       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_CONT)

     end function MSP_test_loi_cont

     logical function MSP_test_loi_bul(scenario) result(test)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_loi_bul
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_bul(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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
!$Nom
!  MSP_test_loi_bul
!
!$Resume
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Description
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
! Teste si la loi courante d'un scenario est de type loi de propulsion bulletin
!
!$Auteur
! Y. TANGUY (ATOS)
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  test = MSP_test_loi_bul(scenario)
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     scenario  :<MSP_SCENARIO_LOI>   scenario
!scenario
!>S     test      :<logical>            booleen
!booleen
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

       ! Arguments
       type(MSP_SCENARIO_LOI), intent(in) :: scenario

       ! Code
       test = (scenario%loi_courante%type_loi == MSP_ENUM_LOI_ATTI_TAB)

     end function MSP_test_loi_bul

     subroutine MSP_scenario_posit_loi_courante(scenario, id, nom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_scenario_posit_loi_courante
!
!$Resume
!  Recherche la loi n°id ou de nom "NOM" et positionne le pointeur loi courante
!  sur cette loi.
!
!$Description
!  Recherche la loi n°id ou de nom "NOM" et positionne le pointeur loi courante
!  sur cette loi.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_scenario_posit_loi_courante(scenario, [id], [nom])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: id
!.    character(LEN=*) :: nom
!
!$Procedures
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
       type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
       integer, intent(IN), optional :: id
       character(LEN=*), intent(IN), optional :: nom

       type(MSP_LOI), pointer :: ptr 

       ! Code
       if (PRESENT(id).and.PRESENT(nom)) then 
          call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS_004", &
               routine="MSP_positionner_loi_courante", type=MSP_ENUM_ERREUR, &
               partie_variable="id et nom")
          return
       end if
       
       ! Cas de la recherche d'après l'identifiant (n° loi)
       if (PRESENT(id)) then
          call MSP_rechercher_loi(scenario,ptr,id=id)
          if(MSP_gen_messages("MSP_scenario_posit_loi_courante")) then 
             return
          else
             scenario%loi_courante => ptr
          end if
       end if
       
       ! Cas de la recherche d'après le nom
       if (PRESENT(nom)) then
          call MSP_rechercher_loi(scenario,ptr,nom=nom)
          if(MSP_gen_messages("MSP_scenario_posit_loi_courante")) then 
             return
          else
             scenario%loi_courante => ptr
          end if
       end if


     end subroutine MSP_scenario_posit_loi_courante

     SUBROUTINE MSP_consulter_ptr_loi(scenario, &
          loi_imp, loi_cont, loi_bul, loi_atti_tab, loi_atti_spin, loi_sep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_ptr_loi
!
!$Resume
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
!
!$Description
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_ptr_loi(scenario, &
!.              [loi_imp], [loi_cont], [loi_bul], [loi_atti_tab], [loi_atti_spin], [loi_sep])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION), pointer :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin
!.    type(MSP_SEPARATION), pointer :: loi_sep
!
!$Procedures
!- MSP_consulter_ptr_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER LOI
! SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$Nom
!  MSP_consulter_ptr_loi
!
!$Resume
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
!
!$Description
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
! Renvoie un pointeur sur la structure encapsulée par la loi courante du 
! scenario passe en argument.
! Cette routine permet un gain en performances, car il n'y a pas de recopie
! implicite des structures de données complexes.
! Cependant, elle nécessite quelques précautions d'emplois :
! - à utiliser plutôt pour des consultations de structures, lorsqu'il n'y aura pas de remise à 
! lorsqu'il n'y aura pas de remise à de la structure dans le scénario
! - les données rendues sont des pointeurs, pointant sur les structures internes d'un scénario 
! -> il ne faut pas effacer les structures, sous peine de déteriorer le scénario
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_ptr_loi(scenario, &
!.              [loi_imp], [loi_cont], [loi_bul], [loi_atti_tab], [loi_atti_spin], [loi_sep])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    type(MSP_IMPULSION), pointer :: loi_imp
!.    type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
!.    type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
!.    type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
!.    type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin
!.    type(MSP_SEPARATION), pointer :: loi_sep
!
!$Procedures
!- MSP_consulter_ptr_loi
!
!$Remarques
!
!$Mots-cles
! SCENARIO CONSULTER LOI
! SCENARIO CONSULTER LOI
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    implicit none

    type(MSP_SCENARIO_LOI), intent(IN) :: scenario

    type(MSP_IMPULSION), pointer, optional :: loi_imp
    type(MSP_POUSSEE_CONTINUE), pointer, optional :: loi_cont
    type(MSP_POUSSEE_BULLETIN), pointer, optional :: loi_bul
    type(MSP_ATTITUDE_TABULEE), pointer, optional :: loi_atti_tab
    type(MSP_ATTITUDE_SPINNEE), pointer, optional :: loi_atti_spin
    type(MSP_SEPARATION), pointer, optional :: loi_sep

    if (PRESENT(loi_imp))  loi_imp  => scenario%loi_courante%loi_imp
    if (PRESENT(loi_cont)) loi_cont => scenario%loi_courante%loi_cont
    if (PRESENT(loi_bul))  loi_bul  => scenario%loi_courante%loi_bul
    if (PRESENT(loi_atti_tab))  loi_atti_tab  => scenario%loi_courante%loi_atti_tab
    if (PRESENT(loi_atti_spin)) loi_atti_spin => scenario%loi_courante%loi_atti_spin
    if (PRESENT(loi_sep))  loi_sep  => scenario%loi_courante%loi_sep

  end SUBROUTINE MSP_consulter_ptr_loi
 
end module MSP_SCENARIO
