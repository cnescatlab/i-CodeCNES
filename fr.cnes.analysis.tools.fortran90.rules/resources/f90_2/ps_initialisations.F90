module ps_initialisations

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_initialisations
!
!$Resume
!  Module contenant les sous-programmes d'initialisation de PSIMU.
!
!$Description
!  Module contenant les sous-programmes d'initialisation de PSIMU.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_initialisations.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_initialisations.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.136  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.135  2010/03/01 13:43:36  mercadig
!  VERSION:9.6:DM-ID:1350:01/03/2010: Reecriture d une comparaison de reels dans ps_convertir_bulletin_initial (utilisation de eps)
!
!  Revision 1.134  2009/12/03 13:15:23  mercadig
!  AQ: Suppression de variables inutilisees
!
!  Revision 1.133  2009/11/13 15:12:01  mercadig
!  DM-ID 842 : Prise en compte de l activite solaire reelle COMPAS et suppression de la ressource data_actsol
!
!  Revision 1.132  2009/10/28 16:10:38  mercadig
!  DM-ID 1018: Controles sur la presence des champs typrep et dirref pour sorties en erreur
!
!  Revision 1.131  2009/10/26 14:02:47  kvernelo
!  VERSION:9.5:DM-ID:1299:26/10/2009:Noms absolus des fichiers si c'est une arboresence
!
!  Revision 1.130  2009/09/15 14:36:48  mercadig
!  DM-ID 1218 : Gestion de l'absence du fichier d'ephemerides par signalement d'un warning
!
!  Revision 1.129  2009/09/04 15:04:00  mercadig
!  DM-ID 1218: Les modes Tchebytchev et analytique passent par des initialisations eph_initposcor, la lecture du fichier ephemerides ne passe plus par la MECASPA
!
!  Revision 1.128  2009/09/04 12:54:19  tanguyy
!  DM-ID 1113 : AmÃ©lioration de la gestion des fichiers AERO en mode sous-programme : ils ne sont plus obligÃ©s d'Ãªtre dans le rÃ©pertoire  dÃ©fini par la ressource "DATA_PSIMU"
!
!  Revision 1.127  2009/08/27 13:53:09  cmartel
!  DM-ID 1120 : Prise en compte de la modification de signature des modèles exp.
!
!  Revision 1.126  2009/06/16 07:55:43  tanguyy
!  FA-ID 1306 : report des corrections du patch V9-3-1 ( fuites memoires potentielles, notamment pour les structures msp_vehicule)
!
!  Revision 1.125  2009/05/07 15:30:37  tanguyy
!  FA-ID 1295 : suppression de 2 variables inutilisees
!
!  Revision 1.124  2009/04/14 16:15:44  tanguyy
!  DM-ID 1274 : nouvelle routine d'analyse d'un scenario d'attitude, ce qui permet de ne pas reinitialiser obligatoirement PSIMU lors du passage d'un nouveau scenario d'attitude
!
!  Revision 1.123  2009/04/10 15:11:23  tanguyy
!  FA-ID 1275 : ypas n'est remis a 0 que lorsque reinit_bulletin vaut 1
!  Revision 1.122  2009/03/18 16:53:59  mercadig
!  AQ: Suppression d une declaration double (variable ntab) dans la routine ps_test_arret_propulsion_CI
!  Revision 1.121  2009/03/17 16:02:20  tanguyy
!  DM-ID 1227 : 2nde phase / utilisation des flags reinit_bulletin et reinit_calcul
!  Revision 1.120  2009/03/13 07:57:54  tanguyy
!  FA-ID 1174 / FA-ID 1175 : gestion des manoeuvres de duree nulle ou de deltaV nul. Prototypage DM-ID 1227 : premiere version equivalente au patch, pour assurer les re-initialisations partielles
!  Revision 1.119  2009/02/03 10:06:19  cml
!  FA-ID 1225 : Correction de l utilisation de deltat pour US76
!  Revision 1.118  2009/01/13 14:09:42  cml
!  FA-ID 1179 : Ajout de rlambda_gw pour les modeles d'atmosphere EMCD 43
!  Revision 1.117  2008/12/08 08:37:02  tanguyy
!  AQ : amelioration des liberations memoires
!  Revision 1.116  2008/12/04 15:42:04  tanguyy
!  AQ : suppression de variables inutilisees
!  Revision 1.115  2008/12/03 12:52:55  tanguyy
!  DM-ID 733 : desallocation des structures MECASPA utilisees, et FA-ID 1138 : recherche de la vitesse de rotation du corps central dans COMPAS
!  Revision 1.114  2008/12/02 10:48:22  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.113  2008/12/02 09:56:45  mercadig
!  DM-ID 733: Ajout de la structure emcd pour creation de la structure atmosphere
!  Revision 1.112  2008/12/02 08:17:41  tanguyy
!  DM-ID 733 : correction mineures du code suite aux remarques de la relecture de code, mise en forme des cartouches
!  Revision 1.111  2008/11/26 08:49:54  tanguyy
!  DM-ID 733 : initialisations des structures MSP_ATMOSPHERE ; gestion des caracteristiques vehicule
!  Revision 1.110  2008/10/24 09:39:36  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.109  2008/10/17 14:25:15  tanguyy
!  DM-ID 1058 : controles memoire ; nouvelle routine ps_terminer_session_PSIMU
!  Revision 1.108  2008/10/17 10:06:06  mercadig
!  DM-ID 1058 Correction gestion ouverture fichier resultats
!  Revision 1.107  2008/10/15 13:10:29  tanguyy
!  DM-ID 1058 : controles lors des desallocations memoire
!  Revision 1.106  2008/10/15 12:53:13  tanguyy
!  DM-ID 1058 : - Initialisations des variables, y compris des structures MECASPA (msp_effacer_..(.., nul=.true.).
!  - ps_rech_date_premiere_poussee rend maintenant le nb de points de poussée (ntab) afin que ps_test_arret_propulsion_CI l'utilise
!  Revision 1.105  2008/09/22 09:59:25  mercadig
!  FA-ID 1105 Chargement du fichier potentiel si les degres des tesseraux ou des zonaux ont change
!  Revision 1.104  2008/09/11 16:03:02  tanguyy
!  DM-ID 1058 : controle des desallocations d'integrateurs pour les propulsions
!  Revision 1.103  2008/09/04 07:53:01  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.102  2008/05/02 14:37:19  tanguyy
!  FA-ID 865 : suppression de modprec (obsolete dans GSLIB et inutilise dans PSIMU)
!  Revision 1.101  2008/04/29 17:24:46  tanguyy
!  Correction dans ps_initialiser_modeles (lie a FA-ID 1009)
!  Revision 1.97  2008/04/11 15:06:08  tanguyy
!  FA-ID 798 : utilisation de ps_num_version pour obtenir le numero de version PSIMU
!  Revision 1.96  2008/04/03 16:08:44  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.94  2008/03/07 09:59:23  huec
!  DM-ID 859 : tests des code_erreur
!  Revision 1.93  2008/03/06 12:48:19  ttn
!  DM-ID 959 : Modification routine ps_calcul_altitude_pente + prise en compte dans ps_init_boucle_cowell et ps_init_boucle_rkutta
!  Revision 1.92  2008/02/15 16:37:10  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.91  2008/01/17 16:25:30  tanguyy
!  FA-ID 915 / DM-ID 551 : correction du mode SORTILEGE, et utilisation du nom du modele a la place d'un code
!  Revision 1.90  2007/12/06 16:00:39  huec
!  FA-ID 658 : Variables declarees en double
!  Revision 1.89  2007/12/06 15:26:47  huec
!  DM-ID 744 : Modele d atmosphere de Venus dans PSIMU
!  Revision 1.88  2007/12/05 16:00:30  tanguyy
!  Correction de l'init du repertoire atmosphere pour DM-ID 808
!  Revision 1.87  2007/12/04 08:16:16  tanguyy
!  Integration DM-ID 551 / DM-ID 808
!  Revision 1.86  2007/10/30 08:45:26  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.85  2007/09/24 15:06:16  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.84  2007/07/13 15:27:59  tanguyy
!  AQ : PSIMU V9.0 - maj de commentaires
!  Revision 1.83  2007/07/12 10:34:45  tanguyy
!  Correction d'une FA interne sur la gestion de iveh_save : rajout d'une variable pour distinguer la sauvegarde des parametres des ephemerides et des parametres du potentiel
!  Revision 1.82  2007/07/10 14:07:50  tanguyy
!  Correction d'une anomalie sur l'utilisation de iveh_save. Explications rajoutées sur son emploi dans les initialisations
!  Revision 1.81  2007/07/09 12:00:51  tanguyy
!  DM-ID 688 (mode Terre/Mars) + intégration des DM-ID 702, DM-ID 748 et DM-ID 692 sur les paramètres d'intégration
!  Revision 1.80  2007/06/21 13:16:01  vivaresf
!  FA-ID 746 : validation
!  Revision 1.79  2007/06/20 12:23:45  vivaresf
!  FA-ID 746 : Fuites mémoires, désallocation propres
!  Revision 1.78  2007/04/16 14:11:58  couturis
!  FA-ID 725 : Initialisation des mu Soleil et mu Lune
!  Revision 1.77  2007/03/19 14:56:44  mle
!  DM-ID 600 : permettre de traiter les ephemerides PSIMU par SORTILEGE
!  Revision 1.76  2007/02/09 13:39:01  tanguyy
!  Utilisation du modèle de vent : modif du test pour gagner en performances
!  Revision 1.75.2.5  2007/04/18 06:30:56  vivaresf
!  FA-ID 725 : ré-initialisation du COWELL (complète, i.e.
!  avec désallocation des intégrateurs) si on ré-initialise un des éléments de PSIMU
!  Revision 1.75.2.4  2007/04/17 09:07:53  vivaresf
!  FA-ID 725 : libération du COWELL dans les cas de réinitialisation
!  Revision 1.75.2.3  2007/04/16 09:51:07  couturis
!  FA-ID 725 : Initialisation des mu Soleil et Lune
!  Revision 1.75.2.2  2007/04/16 09:42:48  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.75.2.1  2007/03/12 14:55:27  tanguyy
!  Modification du test de la presence du modele de vent pour gagner en performances
!  Revision 1.75  2007/02/05 17:45:43  tanguyy
!  PSIMU V8.7a1
!  Revision 1.74  2007/02/02 13:32:18  tanguyy
!  DM-ID 659 / DM-ID 643 : finalisation
!  Revision 1.73  2007/01/29 16:30:44  mle
!  DM-ID 643 : modeles de vents dans PSIMU
!  Revision 1.72  2007/01/18 14:10:13  tanguyy
!  FA-ID 687 : Correction de l'initialisation pour gerer correctement les modes Terre/Mars
!  Revision 1.71  2007/01/16 11:39:02  mle
!  DM-ID 642 : Nombre de separations dans PSIMU
!  Revision 1.70  2006/12/01 13:39:11  tanguyy
!  PSIMU V8-6 / Integration finale
!  Revision 1.69  2006/11/30 15:20:33  vpg
!  DM-ID 425 : portage Linux. Remplacement des fonctions dfloat, dmod et dsqrt par real, mod et sqrt
!  Revision 1.68  2006/11/06 17:19:59  tanguyy
!  DM-ID 560 / DM-ID 552 : prise en comptes des evolutions GS_LIB (Lois d'attitude, et modeles de potentiels de COMPAS
!  Revision 1.67  2006/10/19 15:07:54  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.66.2.3  2006/10/19 13:44:43  tanguyy
!  Finalisation DM-ID 478
!  Revision 1.66.2.2  2006/10/17 09:54:23  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.66.2.1  2006/10/13 07:55:15  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.66  2006/10/02 13:10:21  aitiere
!  DM-ID 522 : suppression des variables inutilisees
!  Revision 1.65  2006/10/02 07:55:16  tanguyy
!  DM-ID 522 : decoupage psinit0, psinit2 termine
!  Revision 1.64  2006/09/26 15:45:49  aitiere
!  DM-ID 522 suite
!  Revision 1.63  2006/09/26 12:47:26  aitiere
!  DM-ID 522 : version incomplete
!  Revision 1.62  2006/09/25 10:22:08  aitiere
!  DM-ID 522 version intermediaire
!  Revision 1.61  2006/06/30 15:53:05  tanguyy
!  Integration PSIMU 8-5
!  Revision 1.60  2006/05/30 09:32:18  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.59  2006/04/10 14:45:36  tanguyy
!  FA-ID 524 : Cloture du FT (Integration a rebours et utilisation des sorties a pas fixes)
!  Revision 1.58.4.1  2006/04/10 14:45:11  tanguyy
!  FA-ID 524 : gestion de str_ecr(iveh)%nsor et des dates de sorties a pas fixes modifiee
!  Revision 1.58  2006/03/02 10:58:40  tanguyy
!  MAJ des cartouches pour PSIMU V8-4
!  Revision 1.57  2006/02/27 15:13:47  tanguyy
!  FA-ID 482 : allocation dynamique du tableau des dates de sortie
!  Revision 1.56  2005/12/14 14:16:21  vivaresf
!  DM-ID 397 : pas d'integration a rebours avec ce COWELL
!  Revision 1.55  2005/11/25 14:47:56  tanguyy
!  DM-ID 233, suppression calcul matrice
!  Revision 1.54  2005/11/10 18:37:06  vivaresf
!  Mise à jour des cartouches
!  Revision 1.53  2005/11/09 13:24:18  vivaresf
!  DM-ID 6 : initialisation variables de éphémérides
!  Revision 1.52.2.1  2005/11/22 15:13:26  tanguyy
!  DM-ID 233, Suppression du calcul de la matrice Mat_ri_g50
!  Revision 1.52  2005/03/11 15:59:14  fabrec
!  V8-2 : utilisation de COMPAS
!  Revision 1.51  2005/02/17 13:57:32  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.50  2005/01/28 10:42:11  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.49  2005/01/25 16:19:53  fabrec
!  DM-ID 175 : utilisation de datbul0 au lieu de datbul
!  Revision 1.48  2005/01/17 15:26:03  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.47  2004/12/10 17:11:04  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.46  2004/11/23 10:46:17  vivaresf
!  DM-ID 200 : fin integration
!  Revision 1.45  2004/11/22 15:40:52  vivaresf
!  Integration DM 200
!  Revision 1.44  2004/11/22 15:14:27  vivaresf
!  Integration DM 200/DM 175
!  Revision 1.43  2004/11/22 15:13:22  vivaresf
!  Integration DM 200/DM 175
!  Revision 1.40  2004/11/22 08:57:11  vivaresf
!   DM_ID 200 : utilisation des requa et apla du bulletin pour les conversions
!   de type du bulletin initial et des propulsions bulletin
!  Revision 1.42  2004/11/22 14:16:36  fabrec
!  DM_175! Choix du pas d'integration en fonction de l'altitude
!  Revision 1.39.2.1  2004/11/22 14:01:22  fabrec
!  Type d'attitudes : fichier
!  Revision 1.39  2004/10/12 09:56:42  vivaresf
!  DM-ID 81 : conversion Dates absolues en relatives
!  Revision 1.38  2004/10/11 16:27:41  vivaresf
!  Changement de repere dans le cas date de reference
!  codes dates absolues/relatives/absolues-relatives idem MECASPA
!  Revision 1.37  2004/10/11 08:10:46  jfb
!  DM-ID 81: JFB
!  Revision 1.36  2004/10/05 16:28:28  vivaresf
!  FA-ID 204 : reinitialisation des lois d'attitude
!  Revision 1.34  2004/09/20 13:25:46  vivaresf
!  version V7-3-1
!  Revision 1.35  2004/07/08 16:39:41  adm_ipsi
!  DM-ID 89 : Integration - Maj argument pole
!  Revision 1.34  2004/07/05 10:13:57  ole
!  DM_89
!  Revision 1.33.2.1  2004/07/02 09:53:42  ole
!  Modif repere d'integraion et reperes de sortie pour Mars
!  Revision 1.33  2004/06/24 08:56:04  adm_ipsi
!  DM-ID 88 : intégration
!  Revision 1.32  2004/06/22 10:58:27  vivaresf
!  DM-ID 108 : Initialisation du champ mu
!  Revision 1.31  2004/06/18 10:33:10  vivaresf
!  Mise a jour des entetes
!  Revision 1.30  2004/06/18 10:06:00  vivaresf
!  DM_133 : intégration
!  Revision 1.29.6.1  2004/06/23 15:57:23  adm_ipsi
!  DM-ID 88 : Utilisation du fichier de ressources associé à la version
!  Revision 1.29.4.1  2004/06/18 09:44:40  vivaresf
!  DM-ID 133, Redefinition possible du vehicule apres separation
!  - initialisation des coefs aero
!  - transfert des nouveaux coefs apres separation
!  - choix entre delta de masse ou nouvelle masse
!  Revision 1.29  2004/05/26 15:25:28  adm_ipsi
!  Mise à jour du cartouche de présentation
!  Revision 1.28  2004/03/31 14:28:45  adm_ipsi
!  FA-ID 117, Passage matrice 6,6 pour Mat_RI_G50 calculée par mx_rep et Mat_RI_ME2000,
!  Revision 1.27  2004/01/15 16:33:24  adm_ipsi
!  DM-ID 10, les calculs internes de PSIMU se font en date TE
!  Revision 1.26  2003/11/28 16:53:19  adm_ipsi
!  DM-ID 9, Ajout du Choix du fichier d'activite solaire
!  Revision 1.25  2003/11/25 17:49:53  adm_ipsi
!  DM-ID 80, filtrage des warnings sur mx_var et mu_inter_dim1_lin
!  Revision 1.24  2003/10/03 08:42:51  adm_ipsi
!   Passage d'une ligne longue sur deux lignes
!  Revision 1.23  2003/10/01 16:17:50  adm_ipsi
!   DM-ID 52, Utilisation de mx_var au lieu de MSP_conv_typbul,
!   La borne inférieure des types de bulletin devient MSP_ENUM_PERIGEE_APOGEE
!  Revision 1.22  2003/08/01 17:09:45  adm_ipsi
!  DM-ID 43 : Utilisation nouvelle mslib 4.0 et mspro 3.0, recodification des constantesiscreen
!  Revision 1.21  2003/07/15 13:18:34  adm_ipsi
!  FA-ID 11 : Liberation de la mémoire
!  Revision 1.20  2003/07/11 09:40:36  adm_ipsi
!  DM-ID 21 : Suppression des routines Initialiser_FichierXXX
!  Revision 1.29  2003/03/28 13:11:10  util_am
!  SLB - Décalage contour du cadre (*)
!  Revision 1.28  2003/03/28 09:56:46  util_am
!  SLB - Version industrialisée
!  Revision 1.18  2003/03/20 10:34:20  laurent
!  remplacement de io_e_mars_russe_init par mars_russe_init
!  Revision 1.17  2003/03/14 15:25:28  adm_ipsi
!  Utilisation de ps_nloi_att, ps_nloi_propu, ps_npts_att, ps_npts_propu, ps_nsepa
!  Revision 1.16  2003/02/21 11:38:06  rodier
!  PhB - Correction format d'affichage des messages d'erreur
!  Revision 1.14  2003/02/14 15:56:31  rodier
!  PhB - Initialisation - Controle des données en entrée - MSP_conv_typrep
!  Revision 1.13  2003/02/14 15:03:00  boschett
!  A Deramecourt : avertissement pour portage des init d'amotsphere martienne
!  Revision 1.12  2003/02/12 14:44:54  adm_ipsi
!  Remplacement de IO_p_cvbul par MSP_conv_typbul
!  Revision 1.11  2003/02/04 11:18:37  boschett
!   A Deramecourt : modifications des appels de fonctions de la IOLIB en MSPRO
!   pour les modeles d'atmosphere
!  Revision 1.10  2003/01/27 15:40:27  boschett
!  A Deramecourt : modif type fhier evenement
!  Revision 1.9  2003/01/24 11:51:00  adm_ipsi
!  Utilisation de la mspro pour les conversions de date TE/TUC
!  Revision 1.8  2003/01/16 17:52:07  boschett
!  A Deramecourt : mise a jour au format MADONA (partielle)
!  Revision 1.7  2002/12/20 16:38:32  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.6  2002/12/05 17:31:33  boschett
!   Utilisation de les operateurs MECASPA .egal. et .different. pour tester
!   l'égalité (inégalité) entre réels
!  Revision 1.5  2002/12/04 14:24:16  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/02 17:03:32  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.3  2002/11/26 16:10:02  boschett
!  Ajout de implicit none
!  Revision 1.2  2002/10/07 09:20:53  adm_ipsi
!  Utilisation de la structure MSP_COEF pour l'appel à MSP_consulter_coef
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.27  2002/09/16 11:06:02  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.26  2001/11/09 09:00:04  util_am
!  Gestion d'une date en entrée exprimée en TE
!  Revision 1.25  2001/11/07 13:11:20  util_am
!  Possibilité de choisir entre G50 CNES et J2000 pour le RIS en mode Terre
!  Revision 1.24  2001/10/29 12:46:55  util_am
!  Ajout d'un modèle d'atmosphère exponentiel
!  Revision 1.23  2001/08/24 12:25:50  util_am
!  Ajout d'un test dans le cas où la masse initiale est nulle
!  Revision 1.22  2000/09/05 12:52:04  util_am
!  Passage à 10 véhicules au lieu de 100
!  Revision 1.21  2000/09/01 14:50:59  util_am
!  Initialisation du champ diratm dans le type PS_STR_INT_ATMOSPHERE pour le modèle EMCD
!  Revision 1.20  2000/06/27 11:42:39  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.19  2000/06/21 15:26:50  util_am
!  Gestion des ressources (fichiers de donnees) + appels prives aux structures mecaspa
!  Revision 1.18  2000/05/29 09:15:44  util_am
!  Utilisation de Domtraduire
!  Revision 1.17  2000/05/04 09:04:36  util_am
!   Bug du au non chargement des ephémérides 3corps pour plusieurs véhicules si le fichier
!   est le même
!  Revision 1.16  2000/04/18 07:46:55  util_am
!  Correction d'un bug sur le calcul de Mat_RI_ME2000 sur Mars
!  Revision 1.15  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.14  2000/02/08 09:55:50  util_am
!   Modifications importantes (!) pour tenir compte d'un delta de date sur les lois lors
!   d'une réinitialisation de bulletin
!  Revision 1.13  2000/02/03 09:43:22  util_am
!   Message d'erreur si il manque les lois d'attitude alors qu'il y a une propulsion
!   ou une séparation
!  Revision 1.12  1999/12/02 17:23:16  util_am
!  Ajout d'un test pour savoir si on est sur une trajectoire hyperbolique ou elliptique
!  Revision 1.11  1999/11/18 12:12:08  util_am
!  Optimisation de la lecture du fichier ephemerides
!  Revision 1.10  1999/10/27 11:24:05  util_am
!  Prise en compte des panneaux solaires dans le calcul des forces
!  Revision 1.9  1999/10/26 16:00:02  util_am
!  Bug sur le calcul de l'altitude d'arrêt en mode sous-programme simplifié
!  Revision 1.8  1999/10/26 11:38:44  util_am
!  Modification de la date de la version
!  Revision 1.7  1999/10/26 10:58:27  util_am
!  Prise en compte du 3ème corps
!  Ajout des surfaces de panneaux solaire
!  Centralisation des matrices de passage RI/ROUT/EME2000/MME2000
!  Revision 1.6  1999/09/30 13:44:53  util_am
!  date de la version 6.0.3
!  Revision 1.5  1999/08/31 11:56:10  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.4  1999/08/04 11:28:12  util_am
!   Prise en compte de la gestion des erreurs de MECASPA
!   (20/05/1999) modification dans psinit0: lecture du fichier d'activité
!   solaire uniquement si planète = Terre
!
!$FinHistorique
!
!$Usage
!  use ps_initialisations
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psinit2
!- psinit0
!- ps_initialiser_simulation
!- ps_init_conditions_initiales
!- ps_analyse_scenario_attitude
!- ps_terminer_session_PSIMU
!#V
!- ps_initialiser_bulletin
!- ps_initialiser_carac_vehi
!- ps_initialiser_separation
!- ps_initialiser_propulsion
!- ps_initialiser_integration
!- ps_initialiser_modeles
!- ps_initialiser_attitude
!- ps_ecrire_param_initiaux
!- ps_rech_date_premier_evt
!- ps_rech_date_premiere_sepa
!- ps_test_arret_propulsion_CI
!- ps_rech_date_premiere_poussee
!- ps_convertir_bulletins_poussee
!- ps_convertir_bulletin_initial
!#
!
!$Fonctions
!
!$Include
!#V
!- parameter_mspro.h
!#
!
!$Module
!#V
!- MSP_GESTION_ERREUR
!- MECASPA
!- ps_generalites
!- ps_integration_don
!- ps_modeles
!- ps_attitude
!- ps_separations
!- ps_bulletin
!- ps_evenements
!- ps_propulsion
!- ps_ecriture
!- ps_variables
!- MSLIB
!- ps_interface_psimu_don
!- ps_caracteristiques
!- ps_troiscorps
!- mspro
!- mecaspa
!- ps_integration_cowell
!- ps_calcul_forces
!- ps_propulsion_don
!- ps_atmosphere
!- cps_utilisateur
!- ephem
!#
!
!$Interface
!> psinit2 :  ps_initialiser_simulation
!> psinit0 :  ps_init_conditions_initiales
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSP_GESTION_ERREUR
   use MECASPA

   use ps_generalites
   use ps_integration_don
   use ps_modeles
   use ps_attitude
   use ps_separations

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_initialisations.F90 368 2013-02-19 14:43:59Z aadt $'



   interface psinit2 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psinit2
!
!$Resume
!  Initialise les paramètres de la simulation. 
!
!$Description
!  Initialise les paramètres de la simulation. Interface de l'ancienne routine
!  psinit2, pour assurer une compatibilité ascendante.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psinit2 (reinit_bulletin,reinit_calcul,jflag,iter0,iter,iterw,inicow,itsgnold,ypas,datex,pvs)
!.    integer :: reinit_bulletin, reinit_calcul
!.    integer :: jflag,iter0,iter,iterw,inicow,itsgnold
!.    real (KIND=pm_reel) :: ypas
!.    type(tm_jour_sec) :: datex
!.    real (KIND=pm_reel), dimension(6) :: pvs
!
!$Procedures
!- ps_initialiser_simulation
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      module procedure ps_initialiser_simulation
   end interface
   
   interface psinit0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psinit0
!
!$Resume
!  Initialisation des conditions initiales, d'après les structures
!  chargées en mémoire.
!
!$Description
!  Initialisation des conditions initiales, d'après les structures
!  chargées en mémoire.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psinit0 (mode,delta_date,kkbul,kkcar,kkmod,kkint,&
!.               kkati,kksep,kkpro,kkecr,kkeve,reinit_bulletin,reinit_calcul)
!.    integer :: mode
!.    real(kind=pm_reel) :: delta_date
!.    integer :: kkbul,kkcar,kkmod,kkint,kkati,kksep,kkpro,kkecr,kkeve
!.    integer :: reinit_bulletin, reinit_calcul
!
!$Procedures
!- ps_init_conditions_initiales
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      module procedure ps_init_conditions_initiales
   end interface
     
   ! routines privées permettant de découper ps_init_conditions_initiales (ex : psinit0)
   private :: ps_initialiser_bulletin, ps_initialiser_carac_vehi, ps_initialiser_propulsion
   private :: ps_initialiser_integration, ps_initialiser_modeles 
   private :: ps_initialiser_separation, ps_initialiser_attitude

   ! routines privées permettant de découper ps_initialiser_simulation (ex psinit2)
   private :: ps_ecrire_param_initiaux, ps_rech_date_premier_evt, ps_rech_date_premiere_sepa
   private :: ps_test_arret_propulsion_CI, ps_rech_date_premiere_poussee
   private :: ps_convertir_bulletins_poussee, ps_convertir_bulletin_initial

   contains


      subroutine ps_initialiser_simulation (reinit_bulletin,reinit_calcul,jflag,iter0,iter,iterw,inicow,itsgnold,ypas,datex,pvs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_simulation
!
!$Resume
!  Initialisation de la simulation
!
!$Description
!  Initialisation de la simulation
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_initialiser_simulation (reinit_bulletin,reinit_calcul,jflag,iter0,iter,iterw,inicow,itsgnold,ypas,datex,pvs)
!.    integer :: reinit_bulletin, reinit_calcul
!.    integer :: jflag,iter0,iter,iterw,inicow,itsgnold
!.    real (KIND=pm_reel) :: ypas
!.    type(tm_jour_sec) :: datex
!.    real (KIND=pm_reel), dimension(6) :: pvs
!
!$Arguments
!>E/S   reinit_bulletin  :<integer>           flag de réinitialisation du bulletin, ce qui entraine 
!                                             la réinitialisation du calcul (=1 si réinitialisation, 0 sinon)
!>E/S   reinit_calcul    :<integer>           flag de réinitialisation du calcul, ce qui entraine 
!                                             la réinitialisation de l'intégrateur (=1 si réinitialisation, 0 sinon)
!>S     jflag            :<integer>           = tmax
!.                                     2 si h < hstop
!.                                     3 si h >  h2 ... utilisation de Cowell
!.                                     4 si h <= h2 ... utilisation de Gill
!>S     iter0            :<integer>           numéro de la première itération
!>S     iter             :<integer>           numéro de l'itération courante
!>S     iterw            :<integer>           compteur servant à l'écriture des résultats
!>S     inicow           :<integer>           indicateur de l'état de l'initialisation
!>S     itsgnold         :<integer>           sens de la simulation 
!>E/S   ypas             :<pm_reel>           temps écoulé / début de la simulation (sec)
!>S     datex            :<tm_jour_sec>       date (Jours Juliens CNES)
!>S     pvs              :<pm_reel,DIM=(6)>   position-vitesse courantes (m-m/s)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!#V
!- ps_convertir_bulletin_initial
!- ps_convertir_bulletins_poussee
!- ps_ecrire_param_initiaux
!- ps_rech_date_premier_evt
!- ps_rech_date_premiere_sepa
!- ps_rech_date_premiere_poussee
!- ps_test_arret_propulsion_CI
!#
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_evenements
!- ps_propulsion
!- ps_ecriture
!- ps_variables
!- MSLIB
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_bulletin
      use ps_evenements
      use ps_propulsion
      use ps_ecriture
      use ps_variables
      use MSLIB

      implicit none
      
      integer, intent(inout)             :: reinit_bulletin, reinit_calcul
      integer, intent(OUT)               :: jflag,iter0,iter,iterw,inicow,itsgnold
      real (KIND=pm_reel), intent(INOUT) :: ypas
      type(tm_jour_sec), intent(OUT)     :: datex
      real (KIND=pm_reel), dimension(6), intent(OUT) ::pvs

      ! Variables locales
      logical :: lboucl
      integer :: kdeb,kfin,nfin, stat

      ! Pour test non-régression

      integer :: ntab
      real (KIND=pm_reel), pointer :: timp(:)

      ! Initialisations
      nullify(timp)
      stat = 0

      !   **********************************************************************
      !   * Passage du bulletin initial en position/vitesse dans               *
      !   *       planeto                                                      *
      !   **********************************************************************

      call ps_convertir_bulletin_initial(reinit_bulletin,pvs)
      if(MSP_gen_messages("ps_initialiser_simulation")) return

      !   ********************************************************************
      !   * Passage des bulletins de type créneaux en position/vitesse       *
      !   * dans planeto                                                     *
      !   ********************************************************************

      call ps_convertir_bulletins_poussee(kdeb)
      if(MSP_gen_messages("ps_initialiser_simulation")) return

      !   ********************************************************************
      !   * Initialisations propres a la boucle                              *
      !   ********************************************************************

      iter0=1
      iter=0
      iterw=0

      if (reinit_calcul == 1) then
         ! Le Cowell n'est ré-initialisé que si les conditions de calcul ont changé
         ! ex : modèle, intégrateur, bulletin initiale, lois d'attitude, caractéristiques véhicules
         inicow=0
      end if

      iloi_scenar_sep(iveh)=0
      iloi_scenar_pro(iveh)=0
      str_eve(iveh)%neve=0
      itsgnold=str_int(iveh)%itsgn
      
      if (reinit_bulletin == 1) then
         ! Le compteur "ypas" stocke la date courante, relative au bulletin initial.
         ! Ce compteur est remis à 0 lorsque l'on reinitialise le bulletin.
         ypas = 0._pm_reel
      end if

      datex=str_bul(iveh)%datbul0_js + ypas


      !   ********************************************************************
      !   * Recherche de la 1ere date de sortie reguliere                    *
      !   ********************************************************************

      if (str_ecr(iveh)%nb_sorties > 0) then
         !/ FA-ID 524 : nsor est init à 1 quoiqu'il arrive : c'est plus simple à gérer
         str_ecr(iveh)%nsor = 1
         nfin = str_ecr(iveh)%nb_sorties + 1

         lboucl = .true.
         do while (lboucl)
            if (str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nb_sorties) >= str_int(iveh)%tsign*ypas) then
               lboucl = .false.
            else
               str_ecr(iveh)%nsor = str_ecr(iveh)%nsor + 1 ! str_int(iveh)%itsgn
               if (str_ecr(iveh)%nsor == nfin) lboucl = .false.
            endif
         enddo
      endif



      !   ********************************************************************
      !   * Ecriture des parametres initiaux                                 *
      !   ********************************************************************

      call ps_ecrire_param_initiaux (iter, ypas, pvs)
      if(MSP_gen_messages("ps_initialiser_simulation")) return

      !   ********************************************************************
      !   * Recherche de la 1ere date evenement                              *
      !   ********************************************************************

      call ps_rech_date_premier_evt (ypas)
      if(MSP_gen_messages("ps_initialiser_simulation")) return
      
      !   ********************************************************************
      !   * Recherche de la 1ere date de separation                          *
      !   ********************************************************************

      call ps_rech_date_premiere_sepa (ypas)
      if(MSP_gen_messages("ps_initialiser_simulation")) return

      !   ********************************************************************
      !   * Recherche de la 1ere date de poussee                             *
      !   ********************************************************************

      call ps_rech_date_premiere_poussee (ypas, kdeb, kfin, ntab, timp)
      if(MSP_gen_messages("ps_initialiser_simulation")) return

      !   ********************************************************************
      !   * Tests d'arret ou de propulsion pour les conditions initiales     *
      !   ********************************************************************

      call ps_test_arret_propulsion_CI (timp, ntab, jflag, iter, ypas, datex, pvs, kdeb, kfin, inicow)
      if (MSP_gen_messages("ps_initialiser_simulation")) return
      iter0 = iter + 1

      !     Deallocations memoire
      if (associated(timp)) then
         deallocate(timp, stat=stat)
         if (stat < 0) then
            call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                 partie_variable="ps_initialiser_simulation")
            return
         end if
      end if
      
    end subroutine ps_initialiser_simulation

      subroutine ps_init_conditions_initiales (mode,delta_date,kkbul,kkcar,kkmod,kkint,&
           kkati,kksep,kkpro,kkecr,kkeve,reinit_bulletin,reinit_calcul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_conditions_initiales
!
!$Resume
!  Initialisation des conditions initiales.
!
!$Description
!  Initialisation des conditions initiales via des common (version Fortran77)
!  ou des variables globales correspondantes (version Fortran90).
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_init_conditions_initiales (mode,delta_date,kkbul,kkcar,kkmod,kkint,&
!.               kkati,kksep,kkpro,kkecr,kkeve,reinit_bulletin,reinit_calcul)
!.    integer :: mode
!.    real(kind=pm_reel) :: delta_date
!.    integer :: kkbul,kkcar,kkmod,kkint,kkati,kksep,kkpro,kkecr,kkeve
!.    integer :: reinit_bulletin, reinit_calcul
!
!$Arguments
!>E     mode             :<integer>   mode programme
!>E     delta_date       :<pm_reel>   écart entre la date du bulletin et la date de 
!                                l'initialisation précedente [s]
!>E/S   kkbul            :<integer>   réinitialisation du bloc BULLETIN 
!>E/S   kkcar            :<integer>   réinitialisation du bloc CARACTERISTIQUES
!>E/S   kkmod            :<integer>   réinitialisation du bloc MODELES
!>E/S   kkint            :<integer>   réinitialisation du bloc INTEGRATION
!>E/S   kkati            :<integer>   réinitialisation du bloc ATTITUDE
!>E/S   kksep            :<integer>   réinitialisation du bloc SEPARATIONS 
!>E/S   kkpro            :<integer>   réinitialisation du bloc PROPULSION
!>E/S   kkecr            :<integer>   initialisation du bloc ECRITURE
!>E/S   kkeve            :<integer>   initialisation du bloc EVENEMENT
!>S     reinit_bulletin  :<integer>   flag de réinitialisation du bulletin, ce qui entraine                 
!                                     la réinitialisation du calcul (=1 si réinitialisation, 0 sinon)       
!>S     reinit_calcul    :<integer>   flag de réinitialisation du calcul, ce qui entraine                   
!                                     la réinitialisation de l'intégrateur (=1 si réinitialisation, 0 sinon)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- pslnvar
!- pslcons
!- MSP_consulter_scenario
!- MSP_ouverture_MADONA_col
!#V
!- ps_initialiser_bulletin
!- ps_initialiser_carac_vehi
!- ps_initialiser_separation
!- ps_initialiser_propulsion
!- ps_initialiser_integration
!- ps_initialiser_modeles
!- ps_initialiser_attitude
!#
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_bulletin
!- ps_caracteristiques
!- ps_propulsion
!- ps_troiscorps
!- ps_ecriture
!- ps_evenements
!- mspro
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_interface_psimu_don
      use ps_bulletin
      use ps_caracteristiques
      use ps_propulsion
      use ps_troiscorps
      use ps_ecriture
      use ps_evenements

      use mspro
      

      implicit none

      ! Arguments
      !==========

      integer, intent(IN)    :: mode
      real(kind=pm_reel), intent(IN) :: delta_date
      integer, intent(INOUT) :: kkbul,kkcar,kkmod,kkint,kkati,kksep,kkpro,kkecr,kkeve
      integer, intent(out) :: reinit_bulletin, reinit_calcul

      ! Variables Locales
      !==================
      integer :: i,ini0(PS_NVMAX),init,ier

      integer :: longueur
      logical :: fsep

      character(LEN=80) :: cver
      character(LEN=256) :: rctmp, cdat, cdep

      real (KIND=pm_reel), pointer :: timp(:)

      ! Fonction externe psnum_version
      character(len=12) :: psnum_version

      ! Initialisations
      !================
      data ini0 /PS_NVMAX* 0 /
      data init / 0 /

      save ini0,init

      nullify(timp)
      nimp(:) = 0
      fsep = .false.

      ! Début du code
      !==============
      
      ! on met à 0 ces flags : si une ré-init (partielle ou totale) est nécessaire,
      ! ils seront remis à 1
      reinit_bulletin = 0
      reinit_calcul = 0

!   ********************************************************************
!   * Ecriture à l'écran                                               *
!   ********************************************************************

      if ( init == 0 ) then

         if ( mode == 0 ) then
            ! Récupération des données incluses dans les fichiers de ressource
            
            ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
            ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
            if ( ier < 0 ) then
               call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",&
                    partie_variable="data_psimu",routine="ps_init_conditions_initiales")
               return
            else
               dirdat = rctmp(1:longueur)
            endif
            ier = DOMchargeDomaine(trim(dirfcf_psimu)//&
                                   "/PS_MESSAGES_"//clangue//".conf",nomdomaine)
         endif

         cdep = Domtraduire(nomdomaine,"PS_DEPARTEMENT")
         ! Le numéro de version est obtenu par psnum_version() qui est généré à la volée
         ! lors de la génération de PSIMU
         cver = psnum_version()
         cdat = Domtraduire(nomdomaine,"PS_VERSION_DATE")
         write(6,1000) trim(cdep),trim(cver),trim(cdat)
 1000    format(/,/, &
      T11,'*****************************************************',/, &
      T11,'*',                T63,'*',/, &
      T11,'*',T29,a,          T63,'*',/, &
      T11,'*',                T63,'*',/, &
      T11,'*',T33,'P.S.I.M.U',T63,'*',/, &
      T11,'*',                T63,'*',/, &
      T11,'*',T27,a,' - ',a,  T63,'*',/, &
      T11,'*',                T63,'*',/, &
      T11,'*****************************************************',/,/)


         ! Lecture du fichier VARIABLES:

         call pslnvar
         if ( MSP_PROBLEME ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",routine="ps_init_conditions_initiales")
            if ( MSP_ERREUR ) return
         endif

         if ( mode == 0 ) then
            call pslcons
            if ( MSP_PROBLEME ) then
               call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",routine="ps_init_conditions_initiales")
               if ( MSP_ERREUR ) return
            endif
            init = 1
         endif


      endif

      if ( ini0(iveh) == 0 ) then

         ! En mode sous-programme on désactive l'écran et les sorties :
         if ( mode == 0 ) then
            ilogeph = 0
            ilogeve = 0
            iscreen = -1
            str_ecr(iveh)%indw    = 999999999
            ini0(iveh) = 1
         else
            ! str_gen(iveh)%planet -> code NAIF du corps central
            ! -> correspond au mode PSIMU (Terre -> str_gen%planet = eph_terre = 399)
            str_gen(iveh)%planet = iplanet
         endif

      endif

      if ( mode == 1 ) then
         PS_MODOUT = ps_don_ecr%imodout
      endif
      

      !================================================================
      ! Initialisations des structures de données 
      ! - à chaque structure est associé un flag "kk<struct>", à 0 lorsque la structure
      ! n'est pas (ré)initialisé, et à une valeur /= 0 si la structure est modifiée
      ! - des sous-routines s'occupent du traitement des différentes structures
      ! - les flags servent aussi à déterminer 2 niveaux de ré-initialisations
      !   - ré-initialisation du bulletin : la simulation repart d'un nouveau bulletin initial
      !     ie : sinon, la simulation repart du vecteur d'état
      !   - ré-initialisation du calcul : les intégrateurs sont ré-initialisés, car 
      !     certaines données modifient les conditions du calcul (changement d'intégrateur,
      !     du bulletin, des caractéristirques véhicule, des modèles, des caractéristiques véhicules)
      !
      ! - les deux flags "globaux" reinit_bulletin et reinit_calcul sont donc initialisés
      !   à la suite de la conversion des différentes structures, et traités ensuite dans la routine
      !   ps_initialiser_simulation.
      ! - ensuite, les flags kk<struct> sont mis à 0.
      !
      !=================================================================




!   ********************************************************************
!   * Bulletin                                                         *
!   ********************************************************************

      call ps_initialiser_bulletin (kkbul)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return

!   ********************************************************************
!   * Caractéristiques du véhicule                                     *
!   ********************************************************************

      call ps_initialiser_carac_vehi (kkcar) 
      if (MSP_gen_messages("ps_init_conditions_initiales")) return
!   ********************************************************************
!   * Séparations                                                      *
!   ********************************************************************

      call ps_initialiser_separation(kksep, fsep, delta_date)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return

!   ********************************************************************
!   * Propulsion                                                       *
!   ********************************************************************

      call ps_initialiser_propulsion (delta_date, kkpro, timp)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return

!   ********************************************************************
!   * Integration                                                      *
!   ********************************************************************

      call ps_initialiser_integration (kkint)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return


!   ********************************************************************
!   * Modeles                                                          *
!   ********************************************************************

      call ps_initialiser_modeles (kkmod)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return

!   ********************************************************************
!   * Attitude                                                         *
!   ********************************************************************
      call ps_initialiser_attitude(delta_date, kkati)
      if (MSP_gen_messages("ps_init_conditions_initiales")) return
 
!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_ati(iveh), nloi=nloi_scenar_ati(iveh))
      if ( MSP_gen_messages("ps_init_conditions_initiales") ) return

      if ( nloi_scenar_ati(iveh) == 0 ) then
         
         ! Si il y a un scénario de séparations, mais pas de lois d'attitude
         ! on sort en erreur.
         if ( fsep ) then
            call MSP_signaler_message (cle_mes="PS_INIT_ATI_002")
            return
         endif
         
      endif


!   ********************************************************************
!   * Variables de sortie                                              *
!   ********************************************************************
      if ( kkecr == 1 ) then
         kkecr = 0
         !DM-ID 600 : gestion du type de sorties : personnalisé ou SORTILEGE
         if(ps_don_ecr%itypsort/=0) str_ecr(iveh)%type_de_sortie = ps_don_ecr%itypsort
	 
         ! ouverture du fichier éphémérides (format MADONA colonne)
	 if (index(cficout1,"/") /= 0 ) then
	    call  MSP_ouverture_MADONA_col (trim(cficout1), ilogeph)
         else
            call MSP_ouverture_MADONA_col (trim(dirres)//"/"//trim(cficout1), ilogeph)
         endif
         if ( MSP_gen_messages("ps_init_conditions_initiales") ) return

         ! ouverture du fichier évènement
         ! le format du fichier évènement change en focntion du type de sortie
         if(str_ecr(iveh)%type_de_sortie==1) then
            ! Type Personnalise => 2 fichiers au format MADONA colonne
            ! 
            !     Ouverture des fichiers de resultats.
            !     Si l'ouverture provoque une erreur, on remonte l'erreur.
            if (index(cficout2,"/") /= 0 ) then
               call MSP_ouverture_MADONA_col (trim(cficout2), ilogeve)
            else
               call MSP_ouverture_MADONA_col (trim(dirres)//"/"//trim(cficout2), ilogeve)
            endif
            if ( MSP_gen_messages("ps_init_conditions_initiales") ) return
            
         elseif(str_ecr(iveh)%type_de_sortie==2) then
            ! type SORTILEGE => fichier au format MADONA DB
            ilogeve = acc_open()
            if (ilogeve < 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA", &
                    routine ="ps_init_conditions_initiales", &
                    partie_variable='acc_open')
               return
            endif
            if (index(cficout2,"/") /= 0 ) then
               ier = acc_connect (ilogeve,trim(cficout2),ACC_W)
            else
               ier = acc_connect (ilogeve,trim(dirres)//"/"//trim(cficout2),ACC_W)
            endif
            if (ier /= 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",&
                    routine ="ps_init_conditions_initiales", &
                    partie_variable='acc_connect')
               return
            endif
            ier = acc_set_ftype(ilogeve,ACC_FIL_DB)
            if (ier /= 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",&
                    routine ="ps_init_conditions_initiales", &
                    partie_variable='acc_set_ftype')
               return
            endif
            ier = acc_set_mode (ilogeve,ACC_FORCE_SELECT,ACC_ON)
            if (ier /= 0) then
               call MSP_signaler_message (cle_mes ="PSIMU_ERR_FCT_MADONA",&
                    routine ="ps_init_conditions_initiales", &
                    partie_variable='acc_set_mode')
               return
            endif
         end if
         str_ecr(iveh)%nvarw    = ps_don_ecr%nvarw
         str_ecr(iveh)%iresw(:) = ps_don_ecr%iresw(:)
         str_ecr(iveh)%indw     = ps_don_ecr%indw
         str_ecr(iveh)%iforw    = ps_don_ecr%iforw
         str_ecr(iveh)%etatn(:) = ps_don_ecr%etatn(:)
         ! Dans le cas SORTIILEGE, mise à jour de str_ecr%etatn
         if(str_ecr(iveh)%type_de_sortie ==2) then
            ! Modifications des libellés pour la compatibilité avec
            ! SORTILEGE
            
            str_ecr(iveh)%etatn(106) = "DATE"
            str_ecr(iveh)%etatn(113) = "JJ50"
            str_ecr(iveh)%etatn(114) = "SEC"
            str_ecr(iveh)%etatn(41) = "MASSE"
            str_ecr(iveh)%etatn(102) = "Q1"
            str_ecr(iveh)%etatn(103) = "Q2"
            str_ecr(iveh)%etatn(104) = "Q3"
            str_ecr(iveh)%etatn(105) = "Q4"

            ! Variables utilisées en mode SORTILEGE
            str_ecr(iveh)%iresw(1) = 106
            str_ecr(iveh)%iresw(2) = 113
            str_ecr(iveh)%iresw(3) = 114
            str_ecr(iveh)%iresw(4) = 3
            str_ecr(iveh)%iresw(5) = 4
            str_ecr(iveh)%iresw(6) = 5
            str_ecr(iveh)%iresw(7) = 9
            str_ecr(iveh)%iresw(8) = 10
            str_ecr(iveh)%iresw(9) = 11
            str_ecr(iveh)%iresw(10) = 41
            str_ecr(iveh)%iresw(11) = 117
            str_ecr(iveh)%iresw(12) = 102
            str_ecr(iveh)%iresw(13) = 103
            str_ecr(iveh)%iresw(14) = 104
            str_ecr(iveh)%iresw(15) = 105
            
         end if
         str_ecr(iveh)%ipas = ps_don_ecr%ipas
         if(str_ecr(iveh)%ipas == 2) then
            !/ FA-ID 524 : le pas de sortie peut être négatif, si l'on intègre à rebours
            str_ecr(iveh)%pas_sortie = str_int(iveh)%itsgn*abs(ps_don_ecr%pas_sortie)
            !/ FA-ID 691 : calcul du nb de pas de sortie
            !/ 2 sorties minimum : pour t0=0 et pour la sortie finale
            i = 1
            do while(i*abs(str_ecr(iveh)%pas_sortie) < abs(str_int(iveh)%tmax))
               i = i + 1
            end do
            str_ecr(iveh)%nb_sorties = i + 1
            

            !/ FA-ID 482
            if(associated(str_ecr(iveh)%ypas_sorties)) then
               deallocate(str_ecr(iveh)%ypas_sorties)
            end if
            allocate(str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nb_sorties))

            str_ecr(iveh)%ypas_sorties(1) = 0._PM_REEL
            do i = 2, str_ecr(iveh)%nb_sorties - 1
               ! Les dates de sorties sont négatives lorsque l'on intègre à rebours
               str_ecr(iveh)%ypas_sorties(i) = str_ecr(iveh)%ypas_sorties(i-1) + str_ecr(iveh)%pas_sortie
            enddo
            str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nb_sorties) = str_int(iveh)%tmax
         endif
      endif

!   ********************************************************************
!   * Variables pour les événements                                    *
!   ********************************************************************

      if ( kkeve == 1 ) then
         kkeve    = 0
         str_eve(iveh)%ndeve   = ps_don_eve%ndeve
         str_eve(iveh)%ifore   = ps_don_eve%ifore
         str_eve(iveh)%neve    = ps_don_eve%neve
         str_eve(iveh)%deve(:) = ps_don_eve%deve(:)
         str_eve(iveh)%aeve(:) = ps_don_eve%aeve(:)
      endif


!   ********************************************************************
!   * Caractéristiques des intégrateurs                                *
!   ********************************************************************
      if(str_ecr(iveh)%ipas == 2) then
         str_int(iveh)%type_sortie = 2
      else
         ! soit multiple du pas d'integration, soit non précisé (ex :mode ss prg)
         str_int(iveh)%type_sortie = 1 
      end if

!============================================================================
! Gestion des reinitialisations : 
! -> but : analyser les structures passéees par l'utilisateur, et décider (au choix)
! - il n'y a pas de réinitialisation à faire (reinit_calcul=0 et reinit_bulletin=0)
! - il y a réinitialisation, mais seulement de l'intégrateur 
!   car les conditions du calcul ont changé : on reinitialise le Cowell (reinit_calcul = 1)
! - il y a réinitialisation du bulletin initial, et de l'intégrateur (reinit_calcul = 1 et reinit_bulletin = 1)
!
! /!\ Pour le scénario de lois d'attitude, il faut analyser le contenu du scénario pour vérifier si 
! les lois ont changé ou pas..
!============================================================================


      if (kkbul == 1) then
         
         ! Ré-initialisation du bulletin
         ! -> nécessite de recalculer les PV initiales dans le repère d'intégration à partir du bulletin
         ! -> nécessite également une ré-initialisation globale du calcul 
         ! =============================


         !*************************************************************************************
         ! La date initiale (datbul0) est affectée et utilise maintenant l'échelle de temps TE
         ! 
         !*************************************************************************************
         str_bul(iveh)%datbul0    = str_bul(iveh)%datbul
         str_bul(iveh)%datbul0_js = str_bul(iveh)%datbul_js 
         
         ! Le bulletin va être ré-initialisé (cad converti dans le repère d'intégration)
         ! Le calcul va être ré-initialisé : les intégrateurs seront ré-initialisés
         reinit_bulletin = 1
         reinit_calcul = 1

      end if

      if (kkmod /= 0 .or. kkint /= 0 .or. kkati == 1 .or. kkcar /= 0) then
         ! Modification d'une des structures parmi : 
         !   - modèles (-> modification des modèles de forces naturelles)
         !   - paramètres d'intégration 
         !   - lois d'attitude, si le nouveau scénario a un impact sur l'attitude "courante" du véhicule
         !   - caractéristiques véhicules
         ! -> nécessite une ré-initialisation globale du calcul
         !
         ! Note : seules les modifications de lois de propulsion ou de séparation 
         ! ne nécessitent pas de ré-initialisation du calcul, car les ré-initialisations seront 
         ! réalisées au moment du traitement des manoeuvres
         
         reinit_calcul = 1
         
      end if

      kkbul = 0
      kkcar = 0 
      kkmod = 0 
      kkint = 0 
      kkati = 0 
      kksep = 0 
      kkpro = 0 

!     Liberation de la memoire
      if (associated(timp)) deallocate(timp)

      end subroutine ps_init_conditions_initiales

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  la routine qui suit est appelée dans ps_spsimu
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine ps_analyse_scenario_attitude(pati, reinit_attitude) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_analyse_scenario_attitude
!
!$Resume
!  Routine d'analyse d'un scénario d'attitude qui détermine si le scénario
!  nécessite une réinitialisation de l'intégrateur ou pas.
!$Description
!  Routine d'analyse d'un scénario d'attitude qui détermine si le scénario
!  nécessite une réinitialisation de l'intégrateur ou pas. 
!  Si une loi d'attitude coïncide avec la date courante de l'intégrateur, et 
!  qu'elle est différente de l'éventuelle autre loi pré-existante, alors on reinitialise
!  le calcul de PSIMU (et donc l'intégrateur de Cowell)
!  Dans les cas suivants, ce n'est pas nécessaire : 
!  - si l'intégrateur courant est le gill
!  - si les dates des lois du nouveau scénario ne concernent pas la date courante de calcul 
!    (avec une marge de "ordre/2 * pas" secondes autour de la date courante)
!  - si la loi courante est identique à l'ancienne loi en cours. 
!
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_analyse_scenario_attitude(pati, reinit_attitude) 
!.    type(MSP_SCENARIO_LOI) :: pati
!.    integer :: reinit_attitude
!
!$Arguments
!>E/S   pati             :<MSP_SCENARIO_LOI>   Nouveau scenario d'attitude
!>S     reinit_attitude  :<integer>            =0 si aucune reintialisation n'est nécessaire, 1 sinon
!
!$Common
!
!$Routines
!- MSP_consulter_integrator
!- MSP_consulter_scenario
!- ps_recupere_date_loi_att
!- ps_compare_loi_att
!
!$Include
!
!$Module
!#V
!- ps_attitude
!- ps_interface_psimu_don
!- ps_generalites
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
        
        use ps_attitude
        use ps_interface_psimu_don
        use ps_generalites
        
        implicit none
        
        ! Arguments
        !==========
        type(MSP_SCENARIO_LOI), intent(INOUT) :: pati
        integer, intent(out)                  :: reinit_attitude

        ! Variables locales
        !==================
        real(kind=pm_reel) :: date_courante, date_deb_integ, date_fin_integ
        real(kind=pm_reel) :: date_deb_abs_ati, date_fin_abs_ati
        real(kind=pm_reel) :: date_deb_ati, date_fin_ati
        integer :: ordre, type, nloi_pati, nb_loi_scenar_ati
        integer :: ii, jj
        real(kind=pm_reel) :: pas
        logical :: lois_identiques


        ! Début du code
        !==============
        
        reinit_attitude = 0
        
        ! Est-ce qu'une loi impactant les dates courantes
        ! a changé ?
        ! Si oui, il faut réinitialiser.
        !
        ! Démarche : 
        ! 1) déterminer l'intervalle de dates couvert par l'intégrateur de Cowell
        ! 
        ! Pour chaque loi
        ! 2) déterminer si la loi se recoupe avec l'intervalle de l'intégrateur
        !    Ensuite, si c'est le cas, chercher si il n'y a pas une ancienne loi identique
        !    à la loi courante..
        !=================================================================================


        ! Détermination d'un intervalle de date : 
        ! [ date_courante - ordre/2 * pas ; date_courante + ordre/2 * pas ]
        date_courante = str_gen(iveh)%etat(1)

        call MSP_consulter_integrator(str_int(iveh)%pinteg_courant,&
             type_integrateur=type, pas=pas, ordre=ordre)

        if (type .eq. pm_gill) then

           ! intégrateur Gill : pas de pb pour les réinitialisations
           
           reinit_attitude = 0

        else if (type .eq. pm_cowell) then
           
           ! intégrateur de Cowell : 
           ! l'intégrateur va peut être devoir être réinitialisé
           !
           ! Note : pour le momebt reinit_attitude vaut 0

           date_deb_integ = date_courante - ((ordre/2) * pas)/86400._pm_reel
           date_fin_integ = date_courante + ((ordre/2) * pas)/86400._pm_reel

           !  Lecture de la structure MSP_SCENARIO_LOI
           call MSP_consulter_scenario (pati, nloi=nloi_pati)
           if ( MSP_gen_messages("ps_analyse_scenario_attitude") ) return



           do ii = 1, nloi_pati
              ! Boucle sur les lois du nouveau scenario, et vérification 
              ! des intervalles

              call ps_recupere_date_loi_att(pati,ii,date_deb_ati,date_fin_ati)
              if ( MSP_gen_messages("ps_analyse_scenario_attitude") ) return

              date_deb_abs_ati = str_bul(iveh)%datbul0 + (date_deb_ati/86400._pm_reel)
              date_fin_abs_ati = str_bul(iveh)%datbul0 + (date_fin_ati/86400._pm_reel)

              ! Est-ce que l'intervalle se recoupe avec les dates de l'intégrateur
              ! 4 cas possibles 
              ! 1) la date de début de loi est comprise dans l'intervalle d'intégration
              ! 2) la date de fin de loi est comprise dans l'intervalle d'intégration
              ! 3) l'intervalle de loi d'attitude est compris entièrement dans l'intervalle d'intégration
              ! 4) l'intervalle d'intégration est compris dans l'intervalle de la loi d'attitude
              if ((date_deb_integ < date_deb_abs_ati .and. date_deb_abs_ati < date_fin_integ) .or. &
                  (date_deb_integ < date_fin_abs_ati .and. date_fin_abs_ati < date_fin_integ) .or. &
                  (date_deb_integ < date_deb_abs_ati .and. date_fin_abs_ati < date_fin_integ)   .or. &
                  (date_deb_abs_ati < date_deb_integ .and. date_fin_integ < date_fin_abs_ati)) then

                 ! le flag reinit_attitude est mis à 1 : il sera remis à 0 si l'ancienne loi courante
                 ! et la nouvelle sont identiques
                 reinit_attitude = 1

                 ! Analyser si une loi existe déjà, et n'est pas similaire à la loi ii
                 ! Sinon, il faut réinitialiser

                 ! On consulte le scénario existant
                 call MSP_consulter_scenario (scenar_ati(iveh), nloi=nb_loi_scenar_ati)
                 if ( MSP_gen_messages("ps_analyse_scenario_attitude") ) return

                 do jj=1,nb_loi_scenar_ati
                    ! Boucle sur les lois de l'ancien scénario : 
                    ! si la loi courante de l'ancien coïncide exactement avec la nouvelle loi 
                    ! courante, et qu'elles sont identiques, alors on n'a pas besoin de réinitialiser
                   
                    call ps_compare_loi_att(pati,ii,scenar_ati(iveh),jj,lois_identiques)
                    
                    if (lois_identiques) then
                       ! La nouvelle loi courante (ii) est identique à la 
                       ! précédente loi courante (jj)
                       ! -> il n'est pas nécessaire de réintialiser
                       reinit_attitude = 0
                    end if
                    
                 end do

              end if

           end do

        else 
           ! type d'intégrateur non défini : 
           ! l'intégrateur courant n'est pas encore initialisé, donc il
           ! faut de toute façon initialiser le calcul
           reinit_attitude = 1

        end if

      end subroutine ps_analyse_scenario_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  les routines qui suivent sont appelées dans ps_init_conditions_initiales
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine ps_initialiser_bulletin(kkbul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_bulletin
!
!$Resume
!  initialise le bulletin
!
!$Description
!  initialise le bulletin
!
!$Auteur
!  E Aitier (ATOS)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_bulletin(kkbul)
!.    integer :: kkbul
!
!$Arguments
!>E     kkbul  :<integer>   flag indiquant si les données bulletin ont été 
!                           renseignées ou modifiées
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- md_joursec_jourfrac
!- MSP_date_etutc
!- md_jourfrac_joursec
!- MSP_conv_typrep
!- mr_tsid_veis
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_bulletin
!- mspro
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

    use ps_interface_psimu_don
    use ps_bulletin
    use mspro

    implicit none

    ! Arguments
    ! =========
    integer, intent(in)      :: kkbul

    ! Variables locales
    ! =================

    integer                     :: iirep, plae
    type(tm_jour_sec)           :: date_te, date_tuc
    real (kind=pm_reel)         :: apla_rep
    real (KIND=pm_reel)         :: rdxdp2000(6,6),rep2000(10),parbide(6),parbids(6)
    character (LEN=256)         :: ctmp
    character (LEN=256)         :: parvar


    type (tm_code_retour)       :: code_erreur
    type (tm_jour_sec)          :: jul1950, date_rep_in
    type(tm_pole_uv)            :: pole

    ! Début du code
    !==============
    
    if(kkbul == 1) then

       ! /!\ le flag kkbul est remis à 0 une fois les dates des manoeuvres calculées

       str_bul(iveh)%iorb = ps_don_bul%iorb
       if ( (str_bul(iveh)%iorb < MSP_ENUM_PERIGEE_APOGEE) .or. (str_bul(iveh)%iorb > MSP_ENUM_RENTREE_SPHERIQUE) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_TYPBUL")
          write(parvar,'(a,a,i4,a,a,i4,a)') trim(ctmp),'"', &
               MSP_ENUM_PERIGEE_APOGEE,'"','"', &
               MSP_ENUM_RENTREE_SPHERIQUE,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",partie_variable=parvar)
          return
       endif

       str_bul(iveh)%rep(:) = ps_don_bul%rep(:)
       iirep = int(str_bul(iveh)%rep(1))
       if ( (iirep < pm_mx_rep_min) .or. (iirep > pm_mx_rep_max) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_REPERE")
          write(parvar,'(a,a,i3,a,a,i3,a8)') trim(ctmp),'"',pm_mx_rep_min,'"','"',pm_mx_rep_max ,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",partie_variable=parvar)
          return
       endif

       str_bul(iveh)%ech_temps_bul = ps_don_bul%ech_temps_bul
       if ( (str_bul(iveh)%ech_temps_bul /= MSP_ENUM_ECHT_TE) .and. &
            (str_bul(iveh)%ech_temps_bul /= MSP_ENUM_ECHT_TUC) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_ECHT")
          write(parvar,'(a,a,i3,a,a,i3,a)') trim(ctmp),'"',&
               MSP_ENUM_ECHT_TE,'"','"',MSP_ENUM_ECHT_TUC,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_ENSEMBLE",partie_variable=parvar)
          return
       endif

       ! Passage en échelle TE :

       ! Affectation de la date en J/JS 
       ! 2 dates dans PSIMU (et deux variantes jj et jours/sec) 
       !   str_bul(iveh)%datbul[_js] et str_bul(iveh)%datbul0[_js]
       !   < => date courante TE  >     < => date initiale TE ou TUC >
       ! datbul est convertie en TE ici, et datbul0 est affectée à datbul 
       ! plus tard, dans ps_initialiser_simulation
       ! Ainsi, les deux dates sont en TE avant de commencer le calcul
       !=========================================================================
       str_bul(iveh)%datbul_js%jour= ps_don_bul%jjbul
       str_bul(iveh)%datbul_js%sec = ps_don_bul%secbul

       call md_joursec_jourfrac(str_bul(iveh)%datbul_js,str_bul(iveh)%datbul,code_erreur)

       str_bul(iveh)%datbul0=str_bul(iveh)%datbul !dm478 : doit disparaitre
       str_bul(iveh)%datbul0_js = str_bul(iveh)%datbul_js

       if ( str_bul(iveh)%ech_temps_bul == MSP_ENUM_ECHT_TUC ) then

          date_tuc = str_bul(iveh)%datbul_js
          ! Conversion de date TUC en date TE pour la date du bulletin
          call MSP_date_etutc(str_bul(iveh)%datbul_js,-1,date_te)
          if ( MSP_ERREUR ) then
             call MSP_signaler_message (cle_mes="MSP_MSPRO",&
                  partie_variable="MSP_date_etutc", &
                  routine="ps_initialiser_bulletin",type=MSP_ENUM_ERREUR)
             return
          else
             str_bul(iveh)%ech_temps_bul = MSP_ENUM_ECHT_TE
             ! date te
             str_bul(iveh)%datbul_js = date_te
      
             call md_joursec_jourfrac(date_te,str_bul(iveh)%datbul,code_erreur)
          endif
       else
          date_te = str_bul(iveh)%datbul_js
          call MSP_date_etutc(str_bul(iveh)%datbul_js,1,date_tuc)
       endif


       ! Mémorisation de l'écart TUC/TE à la date du bulletin
       ! Cet écart sera utilisé durant le calcul pour toutes les conversions TE -> TUC
       ! afin de garantir la continuité des résultats
       
       ! Ecart TE-TUC en secondes (soustraction de 2 dates jj/sec -> durée en secondes)
       str_bul(iveh)%ecart_te_tuc = (date_tuc - date_te)

       str_bul(iveh)%cle_date = ps_don_bul%cle_date
       if ( (str_bul(iveh)%cle_date < MSP_ENUM_ECHD_1950) .or. &
            (str_bul(iveh)%cle_date > MSP_ENUM_ECHD_DATE_REF) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_ECHD")
          write(parvar,'(a,a,i3,a,a,i3,a)') trim(ctmp),'"',&
               MSP_ENUM_ECHD_1950,'"','"',MSP_ENUM_ECHD_DATE_REF,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",partie_variable=parvar)
          return
       endif

       ! Conversion de date TUC en date TE pour la date du repère
       if ((iirep == MSP_ENUM_PLANETO_REF_INER) .or. &
            (iirep== MSP_ENUM_PLANETO_VRAI) ) then
          if ( str_bul(iveh)%cle_date == MSP_ENUM_ECHD_DATE_REF ) then
             if ( str_bul(iveh)%rep(2) == MSP_ENUM_ECHT_TUC ) then
                ! str_bul(iveh)%rep(4) : date de déf du repère 
                str_bul(iveh)%rep(4) = str_bul(iveh)%rep(4) - (str_bul(iveh)%ecart_te_tuc/86400._pm_reel)
                str_bul(iveh)%rep(2) = real(MSP_ENUM_ECHT_TE,kind=pm_reel)

             endif

          endif
       endif

       str_bul(iveh)%param(:) = ps_don_bul%param(:)

       str_bul(iveh)%num_pla = ps_don_bul%numpla
       if ( (str_bul(iveh)%num_pla /= MSP_ENUM_MERCURE) .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_VENUS)   .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_TERRE)   .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_MARS)    .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_JUPITER) .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_SATURNE) .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_NEPTUNE) .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_URANUS)  .and. &
            (str_bul(iveh)%num_pla /= MSP_ENUM_PLUTON) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_PLANETE")
          write(parvar,'(a,a,i3,a,a,i3,a)') trim(ctmp),'"',&
               MSP_ENUM_MERCURE,'"','"',MSP_ENUM_PLUTON,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_ENSEMBLE",partie_variable=parvar)
          return
       endif

       str_bul(iveh)%num_cen = ps_don_bul%numcen
       if ( (str_bul(iveh)%num_cen /= MSP_ENUM_MERCURE) .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_VENUS)   .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_TERRE)   .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_MARS)    .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_JUPITER) .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_SATURNE) .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_NEPTUNE) .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_URANUS)  .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_PLUTON)  .and. &
            (str_bul(iveh)%num_cen /= MSP_ENUM_SOLEIL) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_CORPS_CENTRAL")
          write(parvar,'(a,a,i3,a,a,i3,a)') trim(ctmp),'"',&
               MSP_ENUM_SOLEIL,'"','"',MSP_ENUM_PLUTON,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_ENSEMBLE",partie_variable=parvar)
          return
       endif

       str_bul(iveh)%vitrot  = ps_don_bul%vitrot
       str_bul(iveh)%obli    = ps_don_bul%obli
       str_bul(iveh)%mu      = ps_don_bul%rmu
       str_bul(iveh)%requa_r = ps_don_bul%requa_r
       str_bul(iveh)%apla_r  = ps_don_bul%apla_r
       str_bul(iveh)%requa   = ps_don_bul%requa
       str_bul(iveh)%apla    = ps_don_bul%apla

       ! Définition du repère d'intégration et des matrices de passage associées
       ! Dans le cas terre, ce repère est le planetocentrique intertiel.
       if ( str_gen(iveh)%planet == eph_terre) then
          str_int(iveh)%reps(1)    = real(MSP_ENUM_PLANETO_REF_INER,kind=pm_reel)
          str_int(iveh)%reps(2:3)  = 0._pm_reel
          str_int(iveh)%reps(4)    = str_bul(iveh)%datbul
          str_int(iveh)%reps(5:10) = 0._pm_reel
          str_int(iveh)%echds      = MSP_ENUM_ECHD_DATE_REF
          str_int(iveh)%num_plas   = str_bul(iveh)%num_cen
       else            
          ! Dans le cas mars, ce repère est l'equatorial planetocentrique UAI 
          ! de la date du bulletin initial.
          str_int(iveh)%reps(1)    = real(MSP_ENUM_PQ,kind=pm_reel)
          str_int(iveh)%reps(2:3)  = 0._pm_reel
          str_int(iveh)%reps(4)    = str_bul(iveh)%datbul
          str_int(iveh)%reps(5:10) = 0._pm_reel
          str_int(iveh)%echds      = MSP_ENUM_ECHD_DATE_BUL
          str_int(iveh)%num_plas   = str_bul(iveh)%num_cen
       end if

       ! Dans le cas terre, le Repère Intertiel de Sortie (RIS) est, selon le choix
       ! de l'utilisateur, soit le repère EME 2000 ou le GAMMA 50.
       ! On calcule alors les matrices de passage du repère d'intégration au RIS.
       ! Dans le cas mars, le RIS est le EME 2000 ou 
       ! l'Equatorial Planetocentrique UAI de la date courante (date de l'éphéméride) 
       ! Le matrice de passage depend de la date, on le calculera plus tard au fur et à mesure.

       ! Dans tous les cas il faut calculer la matrice de passage vers EME 2000, car on a
       ! les ephemerides
       ! des corps perturbateurs dans ce repere, que ce soit le cas terre ou le cas mars
       !rep2000(1)    = dfloat(MSP_ENUM_GAMMA_MOYEN)
       rep2000(1) = real(MSP_ENUM_GAMMA_MOYEN)
       rep2000(2:10) = 0._pm_reel

       parbide(:) = 1._pm_reel

       ! corps central utilisé par PSIMU (Terre ou Mars)
       plae = str_gen(iveh)%planet

       ! Changement de repere TIV --> EME2000


       ! Test de la valeur de apla_r avant passage de 1/apla_r
       if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
          apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
       else
          apla_rep = 0._PM_REEL
       endif

       ! Les dates sont repassées en date TUC

       ! Initialisation de la variable pole pour appel MSP_conv_typrep
       pole%u = str_bul(iveh)%polu
       pole%v = str_bul(iveh)%polv

       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

       ! Calcul de la matrice de passage RI -> EME2000
       !==============================================
       call MSP_conv_typrep (parbide,&
            int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
            date_rep_in , MSP_ENUM_ECHT_TUC, &
            str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            int(rep2000(1)), MSP_ENUM_ECHD_2000, MSP_ENUM_TERRE,&
            str_bul(iveh)%datbul_js + str_bul(iveh)%ecart_te_tuc  , MSP_ENUM_ECHT_TUC,&
            rep2000(3), rep2000(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            str_bul(iveh)%vitrot, &
            parbids, mat_jacob=rdxdp2000, obli=str_bul(iveh)%obli, pole_in=pole, pole_out=pole)

       if ( MSP_gen_messages("ps_initialiser_bulletin") ) return 

       ! Extrait de la Jacobienne Repère d'intégration --> EME2000:
       str_int(iveh)%Mat_RI_ME2000(:,:) = rdxdp2000(:,:)
       ! Pour gagner du temps de calcul, on stocke toute suite la matrice inverse EME2000->RI

       
       str_int(iveh)%Mat_ME2000_RI = transpose(str_int(iveh)%Mat_RI_ME2000)
       ! Calcul de la matrice de passage du repère d'intégration au Gamma 50 CNES
       ! (cas terre uniquement) :
       if( str_gen(iveh)%planet== eph_terre ) then 

          ! - La date est convertie en TUC
          call md_jourfrac_joursec (str_int(iveh)%reps(4) + (str_bul(iveh)%ecart_te_tuc/86400._pm_reel) , &
               jul1950, code_erreur)
          if (code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("ps_initialiser_bulletin")) return
          end if

	  call mr_tsid_veis (date_tuc, 0._pm_reel, str_int(iveh)%teta_RI_g50,code_erreur)
	  if (code_erreur%valeur < 0) then
	     call MSP_signaler_message (ier_mslib=code_erreur)
	     if ( MSP_ERREUR ) return
	  end if

          ! Calcul par appel à mx_rep
          ! pour éviter les écarts de précision
          ! quand simulation sur une durée nulle

          ! Initialisation de la variable pole pour appel MSP_conv_typrep
          pole%u = str_bul(iveh)%polu
          pole%v = str_bul(iveh)%polv
          
          call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
 
          call MSP_conv_typrep (parbide,&
               int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
               date_rep_in, MSP_ENUM_ECHT_TE, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               MSP_ENUM_VEIS, MSP_ENUM_ECHD_DATE_REF, plae,&
               str_bul(iveh)%datbul_js, MSP_ENUM_ECHT_TE,&
               0._pm_reel, rep2000(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               parbids, obli=str_bul(iveh)%obli, pole_in=pole, pole_out=pole)

          if ( MSP_gen_messages("ps_initialiser_bulletin") ) return 

       endif ! if (planete == terre)

    end if ! if kkbul == 1

  end subroutine ps_initialiser_bulletin


  subroutine ps_initialiser_carac_vehi (kkcar)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_carac_vehi
!
!$Resume
!  initialise les caracteristiques du vehicule
!
!$Description
!  initialise les caracteristiques du vehicule
!
!$Auteur
!  E Aitier (ATOS)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_carac_vehi (kkcar)
!.    integer :: kkcar
!
!$Arguments
!>E     kkcar  :<integer>   flag indicant si les caracteristiques vehicules 
!                           ont ete renseignees ou modifiees
!
!$Common
!
!$Routines
!- MSP_effacer_vehicule
!- MSP_effacer_mci
!- MSP_effacer_prad
!- MSP_effacer_aero
!- MSP_effacer_coef
!- MSP_consulter_aero
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_propulsion
!- mspro
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

    use ps_interface_psimu_don
    use ps_propulsion
    use mspro

    implicit none
    
    ! Arguments
    ! =========
    integer, intent (in)                      :: kkcar

    ! Variables locales
    ! ==================
    type(MSP_MCI) :: mci
    type(MSP_AERO) :: aero
    type(MSP_PRAD) :: prad
    type(MSP_COEF) :: cxa,czn
    real(kind=pm_reel) :: cmf
    real(kind=pm_reel), dimension(:), pointer :: coef_cd,  &
                                                 coef_cl,  &
                                                 par1_coef
    
    character(len=256) :: nom_fichier_aero
    integer :: acces, ier

    ! Début du code
    !==============

    nullify(coef_cd)
    nullify(coef_cl)
    nullify(par1_coef)

    if ( kkcar == 1 ) then
           
       if (str_car(iveh)%vehiculeinit) then
          call MSP_effacer_vehicule(str_car(iveh)%vehicule)
          str_car(iveh)%vehiculeinit = .false.
       else
          call MSP_effacer_vehicule(str_car(iveh)%vehicule, nul=.true.)
       endif

       call MSP_effacer_mci(mci)
       call MSP_effacer_prad(prad)
       call MSP_effacer_aero(aero,nul=.true.)

       ! Caracteristiques véhicule
       mci  = MSP_creer_mci (forme=ps_don_car%forme,sx=ps_don_car%sx,&
            sy=ps_don_car%sy,sz=ps_don_car%sz,st=ps_don_car%st,&
            spx=ps_don_car%spx,spy=ps_don_car%spy,spz=ps_don_car%spz,&
            mstr=ps_don_car%xm)
       if (MSP_gen_messages("ps_initialiser_carac_vehi")) return

       prad = MSP_creer_prad(cmp=ps_don_car%cp,ka=ps_don_car%ka,&
            ks=ps_don_car%ks,kd=ps_don_car%kd)

       cmf = ps_don_car%cf

       ! le champ typcf est gardé dans la structure, car cela évite
       ! de faire un MSP_consulter_MECASPA trop souvent
       str_car(iveh)%typcf = ps_don_car%typcf
   
       select case (str_car(iveh)%typcf)
       case (1) 
          if(ps_don_car%typcf_alt == 0) then
             ! DM659
             ! Modèle tabulé
             aero = Msp_creer_aero(type_coef=MSP_ENUM_CF_TABULE_ALT_MOY,&
                  type_variation=MSP_ENUM_ALTITUDE,cmf=cmf)
             if (MSP_gen_messages("ps_initialiser_carac_vehi")) return
          else
             
             nom_fichier_aero = ""

             acces = acc_open()
             ier = acc_connect (acces, ps_don_car%ficaero, ACC_R)
             if (ier < 0) then
                ier = acc_connect (acces, trim(dirdat)//"/"//ps_don_car%ficaero, ACC_R)
                
                nom_fichier_aero = trim(dirdat)//"/"//ps_don_car%ficaero

                if (ier < 0) then
                   call MSP_signaler_message(cle_mes="PS_LECT_FIC_CONF",&
                        partie_variable=nom_fichier_aero)
                end if

             else
                nom_fichier_aero = trim( ps_don_car%ficaero )
             end if

             ier = acc_close(acces)

             aero = Msp_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE, &
                  type_variation=MSP_ENUM_ALTITUDE,ficaero=nom_fichier_aero,&
                  cmf=cmf)
             if (MSP_gen_messages("ps_initialiser_carac_vehi")) return
          end if
       case (2) 
          
          
          allocate(coef_cd(1))
          allocate(coef_cl(1))
          allocate(par1_coef(1))
          
          ! Coefs trainee / portance
          coef_cd(1) = ps_don_car%cd(1,1)
          coef_cl(1) = ps_don_car%cl(1,1)
          ! abcisse : la valeur importe peu, car les coefs sont constants
          par1_coef(1) = 0._pm_reel

          call MSP_effacer_coef(cxa,nul=.true.)
          call MSP_effacer_coef(czn,nul=.true.)

          cxa  = MSP_creer_coef(coef_1d=coef_cd,par1_coef=par1_coef)
          czn = MSP_creer_coef(coef_1d=coef_cl,par1_coef=par1_coef)  
   

          aero=MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
               type_variation=MSP_ENUM_CONSTANT,cxa=cxa,czn=czn,cmf=cmf)
          
          deallocate(coef_cd)
          deallocate(coef_cl)
          deallocate(par1_coef)
          
          call MSP_effacer_coef(cxa)
          call MSP_effacer_coef(czn)
          
       case (3)
          aero = Msp_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
               type_variation=MSP_ENUM_INCIDENCE_MACH,ficaero=trim(dirdat)//"/"//ps_don_car%ficaero,&
               cmf=cmf)
          
          call MSP_consulter_aero (aero,sref=str_car(iveh)%sref)
          if ( MSP_gen_messages("ps_initialiser_carac_vehi") ) return

       end select

       ! Création du véhicule uniquement si kkcar = 1 (initialisation)
       str_car(iveh)%vehicule=MSP_creer_vehicule(aero=aero,mci=mci,prad=prad)
       
       ! le flag d'initialisation est mis à true : le véhicule sera désalloué 
       ! lors de la prochaine réinitialisation, et de toutes façons lors de la fin d'utilisation
       ! de PSIMU (ps_terminer_session_PSIMU)
       str_car(iveh)%vehiculeinit = .true.

       call MSP_effacer_aero(aero)
       call MSP_effacer_mci(mci)
       call MSP_effacer_prad(prad)
    
    end if

  end subroutine ps_initialiser_carac_vehi

  subroutine ps_initialiser_separation(kksep, fsep, delta_date)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_separation
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_separation(kksep, fsep, delta_date)
!.    integer :: kksep
!.    logical :: fsep
!.    real(kind=pm_reel) :: delta_date
!
!$Arguments
!>E     kksep       :<integer>   
!>E/S   fsep        :<logical>   
!>E     delta_date  :<pm_reel>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- psconv_pssep_sep
!- MSP_consulter_scenario
!- MSP_consulter_separation
!- MSP_modifier_separation
!- MSP_modifier_scenario
!- MSP_effacer_separation
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
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

    use ps_interface_psimu_don

    implicit none
    
    ! Arguments
    !==========
    integer, intent(in) :: kksep
    logical, intent(inout) :: fsep
    real(kind=pm_reel), intent(in) :: delta_date

    ! Variables locales
    !==================
    character (LEN=256)                       :: ctmp
    character (LEN=256)                       :: parvar
    integer                                   :: i, sc_type
    real(kind=pm_reel) :: tsep
    type(MSP_SEPARATION) :: loi_sep

    ! Début du code
    !==============

    call MSP_effacer_separation(loi_sep,nul=.true.)

    if ( kksep == 1 ) then

       ! cas d'erreur :
       ! on fait un test sur le nombre de séparations, il doit être 
       ! positif et inférieur à 10 (ps_nsepa) (DM 642)
       if ( (ps_don_sep%nsep > ps_nsepa) .or. ( ps_don_sep%nsep < 0) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_NBSEP")
          write(parvar,'(a,a,i1,a,a,i1,a)') trim(ctmp),'"',0,'"', &
               '"',10,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",&
               routine="ps_initialiser_separation",partie_variable=parvar)
          return
       end if

       ! si il y a des séparations
       if ( ps_don_sep%nsep /= 0 ) then
          fsep = .true.
          ! test du type de date, cas absolu
          if (ps_don_sep%typdat(1).eq.1) then 
             ps_don_sep%tsep(1) = ps_don_sep%secsep(1) +&
                  (ps_don_sep%jjsep(1)-str_bul(iveh)%datbul0)*86400._pm_reel
          endif

          if (ps_don_sep%typdat(2).eq.1) then 
             ps_don_sep%tsep(2) = ps_don_sep%secsep(2) + &
                  (ps_don_sep%jjsep(2)-str_bul(iveh)%datbul0)*86400._pm_reel
          endif

          ! Conversion de la structure ps_don_sep de type PS_STR_SEPARATIONS
          ! en une structure sep de type MSP_SCENARIO_LOI

          call psconv_pssep_sep (ps_don_sep,str_bul(iveh)%datbul0,scenar_sep(iveh))
          if ( MSP_gen_messages("ps_initialiser_separation") ) return         

       endif

    else if ( kksep == 2 ) then

       !  Lecture de la structure MSP_SCENARIO_LOI
       call MSP_consulter_scenario (scenar_sep(iveh), type=sc_type, nloi=nloi_scenar_sep(iveh))
       if ( MSP_gen_messages("ps_initialiser_separation") ) return

       !   Si le scenario est de type separation      
       if ( sc_type == MSP_ENUM_SEPARATION ) then
          if (nloi_scenar_sep(iveh) > 0) then
             do i = 1, nloi_scenar_sep(iveh)
                ! -- Positionnement de la loi_courante sur la loi iloi
                call MSP_consulter_scenario(scenar_sep(iveh), loi_sep=loi_sep, id=i)
                if ( MSP_gen_messages("ps_initialiser_separation") ) return

                call MSP_consulter_separation (loi_sep, date=tsep)
                if ( MSP_gen_messages("ps_initialiser_separation") ) return

                tsep = tsep + delta_date
                call MSP_modifier_separation(loi_sep, date=tsep)
                if ( MSP_gen_messages("ps_initialiser_separation") ) return

                ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                call MSP_modifier_scenario(scenar_sep(iveh), loi_sep=loi_sep, id=i)
                if ( MSP_gen_messages("ps_initialiser_separation") ) return
          
                call MSP_effacer_separation(loi_sep)
             enddo
          endif
       endif
       
    end if

  end subroutine ps_initialiser_separation

  subroutine ps_initialiser_propulsion (delta_date, kkpro, timp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_propulsion
!
!$Resume
!  Initialise la structure propulsion
!
!$Description
!  Initialise la structure propulsion
!
!$Auteur
!  E Aitier (ATOS)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_propulsion (delta_date, kkpro, timp)
!.    integer :: kkpro
!.    real (kind=pm_reel) :: delta_date
!.    real (KIND=pm_reel),dimension(:),pointer :: timp
!
!$Arguments
!>E     delta_date  :<pm_reel>                   écart entre la date du bulletin et la date de 
!                                                l'initialisation précedente [s]
!>E     kkpro       :<integer>                   flag indiquant si les caracteristiques de propulsion ont été 
!                                                renseignées / modifiées    
!>E/S   timp        :<pm_reel,DIM=(:),pointer>   dates relatices (sec) des propulsions
!
!$Common
!
!$Routines
!- MSP_effacer_poussee_continue
!- MSP_signaler_message
!- psconv_pspro_pro
!- psinipc
!- MSP_consulter_scenario
!- MSP_consulter_impulsion
!- MSP_modifier_impulsion
!- MSP_modifier_scenario
!- MSP_consulter_poussee_continue
!- MSP_modifier_poussee_continue
!- MSP_consulter_poussee_bulletin
!- MSP_modifier_poussee_bulletin
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_bulletin
!- mecaspa
!- ps_propulsion
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

    use ps_interface_psimu_don
    use ps_bulletin
    use mecaspa
    use ps_propulsion
    
    implicit none

    ! Arguments
    ! =========
    integer,intent(in)                        :: kkpro
    real (kind=pm_reel),intent(in)            :: delta_date
    real (KIND=pm_reel),dimension(:),pointer  :: timp

    ! Variables locales
    ! =================
    logical                                   :: scenario
    character (LEN=256)                       :: parvar
    character (LEN=265)                       :: ctmp
    integer                                   :: i, sc_type, stat
    integer                                   :: ntab, typloi
    real(KIND=PM_REEL), dimension(:), pointer :: tab_dates
    real (kind=pm_reel)                       :: datdeb, tbul, datprec_tmp, p_datbul
    real (kind=pm_reel)                       :: date, date_ref

    type (MSP_POUSSEE_BULLETIN)               :: loi_bul
    type (MSP_IMPULSION)                      :: loi_imp
    type(MSP_POUSSEE_CONTINUE)                :: loi_cont

    ! Début du code
    !==============

    datprec_tmp = 0.001_pm_reel
    nullify(tab_dates)

    do i=1,PS_NVMAX
       init_integ_propulsion(i) = 0
       pas_integ_propulsion(i) = 0._pm_reel
    end do

    call MSP_effacer_poussee_continue(loi_cont,nul=.true.)
    if ( MSP_gen_messages("ps_initialiser_propulsion") ) return         

    if (kkpro ==1 ) then

       if ( (ps_don_pro%npo > ps_nloi_propu) .or. (ps_don_pro%npo < 0) ) then

          ctmp = Domtraduire(nomdomaine,"PS_INI_NBPRO")
          write(parvar,'(a,a,i1,a,a,i2,a)') trim(ctmp),'"',0,'"','"',ps_nloi_propu,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",partie_variable=parvar)
          return
       end if

       if ( ps_don_pro%npo /= 0 ) then
         
          ! Test sur le champ dirref de l'onglet propulsion
	  ! Si dirref = inertiel alors sortie en erreur avec message
	  ! (car ancien fichier OPTIONS)	 
	  do i=1,ps_don_pro%npo
             if ( ps_don_pro%iprop(i) == -1 ) then
                call MSP_signaler_message (cle_mes="PS_INIT_PRO_004")
                return
	     end if
          end do

          ! Conversion de la structure ps_don_pro de type PS_STR_PROPULSION
          ! en une structure scenar_pro de type MSP_SCENARIO_LOI

          call psconv_pspro_pro (ps_don_pro,str_bul(iveh)%datbul0,scenar_pro(iveh))
          if ( MSP_gen_messages("ps_initialiser_propulsion") ) return         


          call psinipc
          if ( MSP_gen_messages("ps_initialiser_propulsion") ) return         

       endif

    else if ( kkpro == 2 ) then

       !  Lecture de la structure MSP_SCENARIO_LOI
       call MSP_consulter_scenario (scenar_pro(iveh), type=sc_type, nloi=nloi_scenar_pro(iveh))
       if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

       !   Si le scenario est de type propulsion
       scenario = ( sc_type == MSP_ENUM_PROPULSION )
       scenario = scenario .and. (nloi_scenar_pro(iveh) > 0)

       if (scenario) then

          do i = 1, nloi_scenar_pro(iveh)
             !/  Extraction du type de la loi
             typloi = MSP_type_loi (scenar_pro(iveh), id=i)
             if ( MSP_gen_messages("ps_initialiser_propulsion") ) return
             if (typloi == MSP_ENUM_LOI_IMP) then

                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                call MSP_consulter_impulsion(loi_imp,date=date)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                date = date + delta_date

                call MSP_modifier_impulsion(loi_imp,date=date)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                call MSP_modifier_scenario(scenar_pro(iveh), loi_imp=loi_imp, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

             else if (typloi == MSP_ENUM_LOI_CONT) then

                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                call MSP_consulter_poussee_continue(loi_cont, ntab=ntab,dates=tab_dates)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                tab_dates(1:ntab) = tab_dates(1:ntab)+ delta_date

                call MSP_modifier_poussee_continue(loi_cont,dates=tab_dates)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                call MSP_modifier_scenario(scenar_pro(iveh), loi_cont=loi_cont, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                ! Libération de la mémoire
                call MSP_effacer_poussee_continue (loi_cont)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return
                if(associated(tab_dates)) then
                   deallocate(tab_dates, stat=stat)
                   if (stat < 0) then
                      call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                           partie_variable="ps_initialiser_propulsion")
                      return
                   end if
                end if
             else

                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return
                call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                call MSP_consulter_poussee_bulletin(loi_bul,datedeb=datdeb,datbul=p_datbul)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                if (ASSOCIATED(timp)) deallocate(timp) 
                allocate(timp(2))

                timp(1) = datdeb
                tbul = (p_datbul - date_ref)*86400._pm_reel
                ! Arrondi à la milli-seconde:
                timp(2) = real(int(tbul),KIND=pm_reel) + &
                     real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp

                timp(1:2) = timp(1:2) + delta_date

                datdeb = timp(1)
                p_datbul = timp(2)/86400._pm_reel+date_ref

                call MSP_modifier_poussee_bulletin(loi_bul,datdeb=datdeb,datbul=p_datbul)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

                ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                call MSP_modifier_scenario(scenar_pro(iveh), loi_bul=loi_bul, id=i)
                if ( MSP_gen_messages("ps_initialiser_propulsion") ) return

             endif
          enddo
       endif
    endif

  end subroutine ps_initialiser_propulsion

  subroutine ps_initialiser_integration (kkint)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_integration
!
!$Resume
!   initialise les données d'intégration
!
!$Description
!   initialise les données d'intégration
!
!$Auteur
!  E Aitier (ATOS)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_integration (kkint)
!.    integer :: kkint
!
!$Arguments
!>E     kkint  :<integer>   
!
!$Common
!
!$Routines
!- MSP_consulter_integrator
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_integration_cowell
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

    use ps_interface_psimu_don
    use ps_integration_cowell, only : ps_libere_cowell

    implicit none

    ! Arguments
    ! =========
    integer, intent (in)                   :: kkint

    ! Variables locales
    !==================
    real(kind=pm_reel) :: xpash2,xpash3

    ! Début du code
    !==============    
    
    if(kkint == 1) then

       ! pas d'intégration courant remis à 0
       str_int(iveh)%h     = 0._pm_reel

       str_int(iveh)%h1     = ps_don_int%h1
       str_int(iveh)%h2     = ps_don_int%h2
       str_int(iveh)%hstop  = ps_don_int%hstop
       str_int(iveh)%h3     = ps_don_int%h3
       str_int(iveh)%pinteg0 = ps_don_int%pinteg0
       str_int(iveh)%pinteg1 = ps_don_int%pinteg1
       str_int(iveh)%pinteg2 = ps_don_int%pinteg2
       str_int(iveh)%pinteg3 = ps_don_int%pinteg3
       
       ! Emission d'un warning si pinteg3%pas > pinteg2%pas
       ! --> il est en effet conseillé d'utiliser un pas plus petit sur les altitudes "très" basses
       call MSP_consulter_integrator(str_int(iveh)%pinteg2,pas=xpash2)
       if(MSP_gen_messages("ps_initialiser_integration")) return

       call MSP_consulter_integrator(str_int(iveh)%pinteg3,pas=xpash3)
       if(MSP_gen_messages("ps_initialiser_integration")) return

       if(xpash3 > xpash2) then
          call MSP_signaler_message(cle_mes="PS_INIT_INTEG_01")
       end if

       ! Type d'altitude utilisée pour le critère d'arrêt (géodésique, ou géocentrique)
       str_int(iveh)%type_alt = ps_don_int%type_alt

       str_int(iveh)%tmax = ps_don_int%tmax

       ! détermination du sens de l'intégration
       if( str_int(iveh)%tmax < 0._pm_reel ) then
          str_int(iveh)%itsgn = -1
          str_int(iveh)%tsign = -1._pm_reel
       else
          str_int(iveh)%itsgn = 1
          str_int(iveh)%tsign = 1._pm_reel
       endif

       ! FA-ID 725 : date à la date du bulletin (de toutes facons on ré-initialisera au besoin)
       str_int(iveh)%date_init_cowell = str_bul(iveh)%datbul0_js

    end if

    
  end subroutine ps_initialiser_integration


  subroutine ps_initialiser_modeles (kkmod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_modeles
!
!$Resume
!   Initialisation des modèles de force
!
!$Description
!   Initialisation des modèles de force
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_modeles (kkmod)
!.    integer :: kkmod
!
!$Arguments
!>E     kkmod  :<integer>   
!
!$Common
!
!$Routines
!- Msp_signaler_message
!- MSP_consulter_potentiel
!- MSP_signaler_message
!- MSP_effacer_actsol
!- MSP_creer_actsol
!- MSP_effacer_atm_emcd
!- eph_initposcor
!
!$Include
!#V
!- parameter_mspro.h
!#
!
!$Module
!#V
!- ps_interface_psimu_don
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
    use ps_interface_psimu_don
    use ephem, only: eph_initposcor

    implicit none
    
    ! Arguments
    ! =========
    integer, intent (in)                      :: kkmod

    ! Variables locales
    ! =================
    integer                                   :: ier                      
    character(LEN=256)                        :: fichier_pot
    character(LEN=256), dimension(1)          :: fichier_eph 
    character(LEN=256)                        :: rep_eph
    character(LEN=256)                        :: rep_atm_emcd
    character(LEN=256)                        :: fic_mod_vent
    integer                                   :: trouve, stat
    integer, dimension(13)                    :: statb
    
    integer :: code_soleil,code_lune,code_phobos,code_deimos
    integer :: ii
    real(kind=pm_reel),dimension(7) :: tab_ap
    character(LEN=32) :: unite
    character(LEN=128) :: partie_variable
    integer :: atm_ephemerides_solaire

    logical :: charger_potentiel = .false. 
    logical :: loc = .false.

    ! Variables locales permanentes pour ne pas ré-initialiser les structures
    ! en mode sous-prorgamme si déjà fait
    
    ! iveh_save_xxx est affecté à la valeur iveh, lorsque modpot est différent de modpot_save
    ! -> c'est à dire, à l'initialisation, et à chaque fois que l'utilisateur veut un modèle différent
    ! modpot_save est alors également affecté à la valeur modpot
    character(LEN=256), save                  :: ficept_save
    character(LEN=256), save                  :: modpot_save 
    integer, save                             :: nzo_save, nte_save

    character(LEN=256) :: factsol
    integer :: dateref_actsol
    type(MSP_ACTSOL)   :: str_actsol
    type(MSP_ATM_EMCD) :: str_emcd
    type(MSP_ATM_EXP)  :: str_exp

    ! Include --> paramètre de la GSLIB pour désigner les modèles martiens et vénusiens
    !========
#include "parameter_mspro.h"    


    ! Début du code
    !==============


    ! Initialisation codes du soleil et de la lune
    code_soleil = eph_soleil
    code_lune = eph_lune
    code_phobos = eph_phobos 
    code_deimos = eph_deimos


    if(kkmod == 1) then

       ! 1) Modèle de potentiel et données physiques générales
       ! liées au corps d'intérêt : requa, rapla, mu, vit rot.
       !
       !=================================================================

       ! Affectation du fichier potentiel, d'après le nom du modèle COMPAS
       ! (si celui-ci a changé)
       if ( trim(ps_don_mod%modpot) /= trim(modpot_save) ) then
          charger_potentiel = .true.
       else if ( (ps_don_mod%nzo /= nzo_save) .or. (ps_don_mod%nte /= nte_save) ) then 
             ! Rechargement également si les degrés tesseraux ou zonaux ont changé
          charger_potentiel = .true.
       end if

       if (charger_potentiel) then 
          ! On doit (re)charger le potentiel

          trouve = cps_getFichierModele("potentiel", trim(ps_don_mod%modpot), &
               fichier_pot, .true.)
          modpot_save = trim(ps_don_mod%modpot)
	  nzo_save = ps_don_mod%nzo
	  nte_save = ps_don_mod%nte
          if(trouve /= 0) then
             call Msp_signaler_message(cle_mes="PS_INIT_POT_001",&
                  partie_variable=trim(ps_don_mod%modpot))
             return
          end if
          
          ! Sauvegarde des degrés zonaux et tesseraux dans ces variables de la structure str_mod
          ! -> ceci permet un accès rapide à ces valeurs, notamment lors de la permutation
          ! entre 2nd membre simplifié et 2nd membre complet.
          str_mod(iveh)%nzo       = ps_don_mod%nzo
          str_mod(iveh)%nte       = ps_don_mod%nte

          if (associated(str_mod(iveh)%vj)) deallocate(str_mod(iveh)%vj)
          nullify(str_mod(iveh)%vj)

          ! Chargement du modèle grâce à MECASPA / COMPAS : 
          str_mod(iveh)%pot = Msp_creer_potentiel(fichier_pot,&
               str_mod(iveh)%nzo,str_mod(iveh)%nte,nom_pot=trim(ps_don_mod%modpot))
          if ( MSP_gen_messages("ps_initialiser_modeles") ) return         
          
          call MSP_consulter_potentiel(str_mod(iveh)%pot,mu=str_mod(iveh)%gmu,&
               requa=str_mod(iveh)%requa,apla=str_mod(iveh)%apla,zd=str_mod(iveh)%vj)
          if ( MSP_gen_messages("ps_initialiser_modeles") ) return        

       end if

       ! Vitesse de rotation des différents corps (FA-ID 1138)
       ! -> interrogation COMPAS
       !=========================================
       if (str_gen(iveh)%planet /= eph_terre .and. &
            str_gen(iveh)%planet /= eph_mars .and. &
            str_gen(iveh)%planet /= eph_venus ) then
          ! On s'assure que le mode d'utilisation PSIMU est bien valide.

          write (partie_variable,'(i3)') str_gen(iveh)%planet
          call MSP_signaler_message(cle_mes="PS_ERR_MODE_INCONNU",partie_variable=partie_variable)
          return
       end if

       trouve = cps_getCsteThCourante(str_gen(iveh)%planet,"vrot", str_mod(iveh)%vitrotpla, unite)
       if (trouve /= CPS_OK) then       
          write (partie_variable,'(a)') "vrot"
          call MSP_signaler_message(cle_mes="PS_ERR_VALEUR_COMPAS_INCONNUE",partie_variable=partie_variable)
       end if
       
       ! 2) Initialisation des clés d'activation des différentes forces
       !
       !===============================================================
       str_mod(iveh)%ikle(1:4) = ps_don_mod%ikle(1:4)


       !/ Données pour le modèle d'atmosphère
       !-------------------------------------
       
       ! Effacement de la structure str_actsol
       call MSP_effacer_actsol(str_actsol,nul=.true.)
       
       if (ps_don_mod%ikle(3) == 3 .and. str_gen(iveh)%planet == eph_terre .and. &
            ps_don_mod%modatm /= CPS_MODATM_ROAT77) then
          ! Activité solaire réelle mode fichier: valeurs lues dans un fichier
          ! -> création de la structure en lisant le fichier
          !
          ! Exception : ROAT 77 prend toujours une activité solaire standard
          !=================================================
                    
	  factsol=trim(ps_don_mod%ficactsol)
          call MSP_creer_actsol(type_actsol=MSP_ENUM_ACTSOL_REELLE,&
               factsol=factsol,str_actsol=str_actsol)	  
	  if (MSP_gen_messages("ps_initialiser_modeles")) return
	  
       else if (ps_don_mod%ikle(3) == 1 .and. str_gen(iveh)%planet == eph_terre .and. &
            ps_don_mod%modatm /= CPS_MODATM_ROAT77) then
       	  ! Activité solaire réelle mode COMPAS: valeurs extraites de COMPAS
	  ! -> création de la structure en récupérant les valeurs de COMPAS
	  
          ! Exception : ROAT 77 prend toujours une activité solaire standard
          !=================================================
 
	  if (ps_don_mod%mode_avance_actsol == 1) then
	     ! Mode avancé : une date de référence est fournie
	     dateref_actsol=ps_don_mod%dateref_actsol	    
	     call MSP_creer_actsol(type_actsol=MSP_ENUM_ACTSOL_COMPAS,&
                  str_actsol=str_actsol,dateref_actsol=dateref_actsol)
             if (MSP_gen_messages("ps_initialiser_modeles")) return
	  else
	     ! Pas de date de référence  
             call MSP_creer_actsol(type_actsol=MSP_ENUM_ACTSOL_COMPAS,&
                  str_actsol=str_actsol)
             if (MSP_gen_messages("ps_initialiser_modeles")) return
	  endif

       else
          ! Activité solaire standard : les valeurs d'AP 
          ! sont les mêmes pour les 7 points
          ! le flux journalier et moyen sont identiques
          !
          ! Remarque :En mode Mars / Vénus, 
          !la structure MSP_ACTSOL ne sera pas utilisée
          !============================================
          do ii=1,7
             tab_ap(ii) = ps_don_mod%app
          end do
          call MSP_creer_actsol(ap=tab_ap,flux=ps_don_mod%flu,&
               fluxmoy=ps_don_mod%flu,type_actsol=MSP_ENUM_ACTSOL_STD,str_actsol=str_actsol)
          if (MSP_gen_messages("ps_initialiser_modeles")) return
       endif



       ! Création du modèle d'atmosphère : 
       !----------------------------------
       ! - type de modèle (champ modele) = ps_don_mod%modatm (nom COMPAS
       ! du modèle saisi par la GSLIB)
       ! - type d'activité solaire (ikle(3) = 1 (std) ou 2 (réelle)
       ! - act_sol : ficactsol ou flux/ap selon les cas
       ! - us76_deltat = écarts de températures pour le modèle US76
       ! - mars_russe_mod = type d'atmosphere pour le modèle Russe 'Mars 1990' (froid,moyen,chaud)
       ! - mars_emcd_sce = type de scénario pour l'EMCD
       ! - mars_emcd_per = type de perturbation pour l'EMCD
       ! - rep_atm_emcd = répertoire de données pour l'EMCD  
       ! - ro0, h0 : densité de référence et hauteur d'échelle
       ! - tscale : type de hauteur d'échelle : hauteur (hscale) ou inverse de la hauteur (beta)
       ! - venus_rho0 et venus_beta : densité de réf et inverse de la hauteur d'échelle, pour le modèle Pétropoulos
       !===================================================================================
       
       call MSP_effacer_atm_exp(str_exp)
       if ( ps_don_mod%modatm == MSP_MODATM_EXP ) then
          str_exp = MSP_creer_atm_exp(hscale = ps_don_mod%param_atm_exp(6), &
               h0 = ps_don_mod%param_atm_exp(2), &
               ro0 = ps_don_mod%param_atm_exp(1),&
               beta = ps_don_mod%param_atm_exp(7), &
               tscale = int(ps_don_mod%param_atm_exp(5)), &
               altmin = ps_don_mod%param_atm_exp(3), &
               altmax = ps_don_mod%param_atm_exp(4) )
       endif

       call MSP_effacer_atm_emcd(str_emcd)
       if(ps_don_mod%modatm == MSP_MODATM_MARS_EMCD_31 .or. &
            ps_don_mod%modatm == MSP_MODATM_MARS_EMCD_42 .or. &
            ps_don_mod%modatm == MSP_MODATM_MARS_EMCD_43) then
          !/ Recherche du répertoire de données EMCD 3.1, 4.2 ou 4.3(nommé "MCD" dans GS_LIB) dans COMPAS 
          !/ Le ".true." permet de récupérer le chemin absolu 
          !/ ie : répertoire des modèles + ssrép du modèle EMCD 3.1, 4.2 ou 4.3
	  
          trouve =  cps_getFichierModele("atmosphere",ps_don_mod%modatm,rep_atm_emcd,.true.)
          if (trouve /= CPS_OK) then 
             if(MSP_gen_messages("ps_initialiser_modeles")) return
          end if
	  
	      str_emcd = MSP_creer_atm_emcd(mars_emcd_sce=ps_don_mod%scena,&
                                        mars_emcd_per=ps_don_mod%typper,lambda_gw=ps_don_mod%lambda_gw,&
                                        dir_emcd=trim(rep_atm_emcd))
       end if



       str_mod(iveh)%atm = MSP_creer_atmosphere(modele=ps_don_mod%modatm,&
            act_sol=str_actsol,us76_deltat=ps_don_mod%deltat, &	    
            mars_russe_mod=ps_don_mod%scena,emcd=str_emcd,&
            exp=str_exp,venus_rho0=ps_don_mod%param_atm_exp(1),&
            venus_beta=ps_don_mod%param_atm_exp(7))
       if (MSP_gen_messages("ps_initialiser_modeles")) return
       
       str_mod(iveh)%calcul_vent = 0
       
       ! Si le modèle de vent n'est pas demandé (ikle_6 = 0, ou modèle non trouvé)
       ! alors on ne calculera pas la vitesse du vent
       if(ps_don_mod%ikle(6) /= 0) then
          ! (DM-ID 643) rajout du modele de vent avec le modele d'atmosphere

          if (trim(str_mod(iveh)%modvent).ne.trim(ps_don_mod%modvent)) then
             str_mod(iveh)%modvent = ps_don_mod%modvent

             ! (DM-ID 643) lecture du modele de vent, s'il existe : 
             ! on cherche d'abord si le fichier existe càd si le modèle existe
             trouve = cps_getFichierModele("vent", str_mod(iveh)%modvent, &
                  fic_mod_vent, .true.)
             if (trouve == CPS_OK)  then
                str_mod(iveh)%vent = MSP_lire_modvent(str_mod(iveh)%modvent)
                str_mod(iveh)%calcul_vent = 1
             else
                str_mod(iveh)%modvent = ""
             end if
          endif
       else
          str_mod(iveh)%modvent = ""
       end if


       !/ Gestion des éphémérides 3è corps
       ! Règles appliquées : 
       ! - si le type d'éphémérides n'est pas renseigné, on utilise des éphémérides analytiques
       ! - pour les modèles d'atmosphère, on consulte COMPAS pour savoir s'ils nécessitent
       ! des éphémérides solaires.
       !----------------------------------

       !/ FA-ID 420 : si ikle non init, on force les ephemerides à un modele analytique
       if (ps_don_mod%ikle(5) == 0) then
          str_3co(iveh)%typephem = 2
       else
          str_3co(iveh)%typephem = ps_don_mod%ikle(5)
       end if

       !/ Recherche dans COMPAS : selon le modèle d'atmosphère, on a besoin ou pas
       !/ d'éphémérides solaires
       ier = cps_requete("modeles_atmosphere", ps_don_mod%modatm, CPS_ALL_ID, &
            "ephemerides_solaire", atm_ephemerides_solaire)


       if ( (str_mod(iveh)%ikle(1)==1) .or. (str_mod(iveh)%ikle(2)==1) .or. &
            (atm_ephemerides_solaire==1) .or. (str_mod(iveh)%ikle(4)==1) ) then
          str_3co(iveh)%ficept  = ps_don_mod%ficept
       else

          ! DM733 : gestion de ficept à revoir

          if ( str_gen(iveh)%planet == eph_terre ) then
             str_3co(iveh)%ficept  = "EPT_PS_EME2000_LS_REF"
          else
             ! Il faudrait avoir les ephemerides dans de l'EME2000
             str_3co(iveh)%ficept  = "EPT_PS_MME2000_S_REF"
          endif
       endif

       ! Stockage du fichier dans un tableau pour appel à eph_initposcor
       fichier_eph(1) = str_3co(iveh)%ficept
       
       ! Initialisation des mu soleil et lune, uniquement si non fait
       !=============================================================
       if (str_mod(iveh)%ikle(1) /= 0) then
          ! Mu des lunes
          !=============
          
          ! Mode Terre
          if (str_gen(iveh)%planet == eph_terre) then
             if (.not.str_3co(iveh)%init_lune) then
                trouve = cps_getCsteThCourante(code_lune,"mu",str_3co(iveh)%mu_lune, unite)
                str_3co(iveh)%init_lune = .true.
             end if
          end if
          
          ! Mode Mars
          if (str_gen(iveh)%planet == eph_mars) then
             if (.not.str_3co(iveh)%init_phobos) then
                trouve = cps_getCsteThCourante(code_phobos,"mu",str_3co(iveh)%mu_phobos, unite)
                str_3co(iveh)%init_phobos = .true.
             end if
             if (.not.str_3co(iveh)%init_deimos) then
                trouve = cps_getCsteThCourante(code_deimos,"mu",str_3co(iveh)%mu_deimos, unite)
                str_3co(iveh)%init_deimos = .true.
             end if
          end if
       end if

       if (str_mod(iveh)%ikle(2) /= 0) then
          ! Mu du Soleil

          if (.not.str_3co(iveh)%init_soleil) then
             trouve = cps_getCsteThCourante(code_soleil,"mu",str_3co(iveh)%mu_soleil, unite)
             str_3co(iveh)%init_soleil = .true.
          end if
       end if

       
       if (str_3co(iveh)%typephem == 1) then

	  ! Initialisation de la méthode Tchebytchev uniquement si le fichier d'éphémérides a changé
	  if ( trim(str_3co(iveh)%ficept) /= trim(ficept_save) .and. &
               trim(str_3co(iveh)%ficept) /= "") then


	     ! Mode Tchebytchev: vérification de la présence du fichier utilisateur dans la base locale
             ! Si non disponible, warning émis et on bascule en mode analytique
		
             ! Récupération des informations "éphémérides" de la base locale
             call eph_infogetLocal(PSI_METHODE_EPH_TCHEMAD, loc, repertoire=rep_eph)
             if (.not.loc) then
                call eph_infoget(PSI_METHODE_EPH_TCHEMAD, repertoire=rep_eph)
             end if
             if ( MSP_ERREUR ) then
                call MSP_signaler_message (cle_mes="PS_ERR_FIC_EPH",routine="ps_initialiser_modeles")
                ! On sélectionne le mode analytique
		str_3co(iveh)%typephem = 2
             endif

             ! Test d'existence du fichier Tchebytchev
             ier=stat(trim(rep_eph)//trim(fichier_eph(1)), statb)
             if(ier.ne.0) then
                call MSP_signaler_message(cle_mes="PS_ERR_FIC_EPH",routine="ps_initialiser_modeles")
	        ! On sélectionne le mode analytique
                str_3co(iveh)%typephem = 2
             endif


             ! Le type d'éphémérides est resté en mode "Tchebytchev" 
	     ! Pas de basculement en mode analytique 
             if (str_3co(iveh)%typephem == 1) then
	        call eph_initposcor(PSI_METHODE_EPH_TCHEMAD, nfichiers=1, fichiers=fichier_eph)
	        ficept_save = str_3co(iveh)%ficept
	     end if
	     
          endif

       endif
       
       if (str_3co(iveh)%typephem == 2 .and. str_gen(iveh)%planet == eph_terre) then
          ! Initialisation de la méthode BDL, théorie VSOP82 avec fichier
	  ! utilisée pour la Terre avec le mode analytique
          call eph_initposcor(PSI_METHODE_EPH_ANALYTIQUE_FIC)
       end if

       call MSP_effacer_actsol(str_actsol)
       if (MSP_gen_messages("ps_initialiser_modeles")) return

    end if ! if kkmod = 1

  end subroutine ps_initialiser_modeles

  subroutine ps_initialiser_attitude(delta_date, kkati)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_initialiser_attitude
!
!$Resume
!   initialise les données d'attitude
!
!$Description
!   initialise les données d'attitude
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_initialiser_attitude(delta_date, kkati)
!.    integer :: kkati
!.    real(KIND=PM_REEL) :: delta_date
!
!$Arguments
!>E     delta_date  :<PM_REEL>   
!>E     kkati       :<integer>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- psconv_psati_ati
!- MSP_consulter_scenario
!- MSP_consulter_attitude_tabulee
!- MSP_modifier_attitude_tabulee
!- MSP_modifier_scenario
!- MSP_effacer_attitude_tabulee
!- MSP_consulter_attitude_spinnee
!- MSP_modifier_attitude_spinnee
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_attitude
!- mecaspa
!- MSLIB
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

    use ps_interface_psimu_don
    use ps_attitude 
    use mecaspa
    use MSLIB

    implicit none

    ! Arguments
    ! =========
    integer, intent (in)                   :: kkati
    real(KIND=PM_REEL), intent (in)        :: delta_date

    ! Variables locales
    ! =================
    character (LEN=256)                       :: parvar
    character (LEN=256)                       :: ctmp
    integer                                   :: i, sc_type, typloi,ntab, stat
    real(KIND=PM_REEL), dimension(:), pointer :: dates
    real(kind=pm_reel)                        :: datedeb, datefin

    type(MSP_ATTITUDE_TABULEE)                :: loi_atti_tab
    type(MSP_ATTITUDE_SPINNEE)                :: loi_atti_spin

    
    ! Début du code
    !==============

    nullify(dates)

    if ( kkati == 1 .or. kkati == 3) then
       ! kkati = 1 : on fournit un nouveau scenario d'attitude
       ! kkati = 3 : on fournit un nouveau scenario d'attitude
       !             mais celui-ci ne nécessitera pas de reinitialisation
       !             de l'intégrateur : pas d'impact sur la loi courante

       if ( (ps_don_ati%natt > ps_nloi_att) .or. (ps_don_ati%natt < 0) ) then
          ctmp = Domtraduire(nomdomaine,"PS_INI_NBATI")
          write(parvar,'(a,a,i1,a,a,i2,a)') trim(ctmp),'"',0,'"','"',ps_nloi_att,'"'
          call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",partie_variable=parvar)
          return
       end if

       if ( ps_don_ati%natt == 0 ) then

          modatt(iveh) = 0

       else
         
          ! Test sur le champ typrep de l'onglet attitude
	  ! Si typrep = inertiel alors sortie en erreur avec message
	  ! (car ancien fichier OPTIONS)	 
	  do i=1,ps_don_ati%natt
             if ( ps_don_ati%irepa(i) == -1 ) then
                call MSP_signaler_message (cle_mes="PS_INIT_ATI_003")
                return
	     end if
          end do

          modatt(iveh) = 1

          ! Conversion de la structure ps_don_ati de type PS_STR_ATTITUDE
          ! en une structure scenar_ati de type MSP_SCENARIO_LOI
          call psconv_psati_ati (ps_don_ati,str_bul(iveh)%datbul0,scenar_ati(iveh))
          if ( MSP_gen_messages("ps_initialiser_attitude") ) return         

       endif


    else if ( kkati == 2 ) then

       modatt(iveh) = 1

       !  Lecture de la structure MSP_SCENARIO_LOI
       call MSP_consulter_scenario (scenar_ati(iveh), type=sc_type, nloi=nloi_scenar_ati(iveh))
       if ( MSP_gen_messages("ps_initialiser_attitude") ) return

       !   Si le scenario est de type attitude
       if ( sc_type == MSP_ENUM_ATTITUDE ) then
          if (nloi_scenar_ati(iveh) > 0) then
             do i = 1, nloi_scenar_ati(iveh)
                !/  Extraction du type de la loi
                typloi = MSP_type_loi (scenar_ati(iveh), id=i)
                if ( MSP_gen_messages("ps_initialiser_attitude") ) return
                if (typloi == MSP_ENUM_LOI_ATTI_TAB) then

                   !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                   call MSP_consulter_scenario (scenar_ati(iveh), loi_attit_tab=loi_atti_tab, id=i)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   call MSP_consulter_attitude_tabulee(loi_atti_tab,ntab=ntab,dates=dates)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   dates(1:ntab) = dates(1:ntab) + delta_date

                   call MSP_modifier_attitude_tabulee(loi_atti_tab,dates=dates)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   ! Désallocation mémoire.
                   if(associated(dates)) then
                      deallocate(dates, stat=stat)
                      if (stat < 0) then
                         call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                              partie_variable="ps_initialiser_attitude")
                         return
                      end if
                   end if
                   
                   ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                   call MSP_modifier_scenario(scenar_ati(iveh), loi_atti_tab=loi_atti_tab, id=i)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   ! Libération de la mémoire
                   call MSP_effacer_attitude_tabulee (loi_atti_tab)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                else if (typloi == MSP_ENUM_LOI_ATTI_SPIN) then
                   !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                   call MSP_consulter_scenario (scenar_ati(iveh), loi_attit_spin=loi_atti_spin, id=i)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   call MSP_consulter_attitude_spinnee(loi_atti_spin,datedeb=datedeb,datefin=datefin)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   datedeb = datedeb + delta_date
                   datefin = datefin + delta_date

                   call MSP_modifier_attitude_spinnee(loi_atti_spin,datedeb=datedeb,datefin=datefin)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return

                   ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                   call MSP_modifier_scenario(scenar_ati(iveh), loi_atti_spin=loi_atti_spin, id=i)
                   if ( MSP_gen_messages("ps_initialiser_attitude") ) return
                endif
             enddo
          endif
       endif
    end if
    

  end subroutine ps_initialiser_attitude



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!   les routines suivantes servent dans ps_initialiser_simulation
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ps_ecrire_param_initiaux (iter, ypas, pvs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_ecrire_param_initiaux
!
!$Resume
!   appelle ps_maj_vecteur_etat (initialisation des variables) et ecrit les premieres lignes 
!   des fichiers de sortie
!
!$Description
!   appelle ps_maj_vecteur_etat (initialisation des variables) et ecrit les premieres lignes 
!   des fichiers de sortie
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_ecrire_param_initiaux (iter, ypas, pvs)
!.    integer :: iter
!.    real(kind=pm_reel) :: ypas
!.    real(kind=pm_reel), dimension(6) :: pvs
!
!$Arguments
!>E     iter  :<integer>           
!>E/S   ypas  :<pm_reel>           
!>E     pvs   :<pm_reel,DIM=(6)>   
!
!$Common
!
!$Routines
!- flush
!- ps_maj_vecteur_etat
!- MSP_signaler_message
!- pswresu
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
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

    use ps_interface_psimu_don

    implicit none

! Arguments
! =========
    integer, intent (in)                         :: iter
    real(kind=pm_reel), intent(inout)             :: ypas
    real(kind=pm_reel), dimension(6), intent(in) :: pvs

! Variables locales
! =================

    if (iscreen == 1) then
       write (6,*)
       call flush (6)
    endif
    
    call ps_maj_vecteur_etat (ypas,str_bul(iveh)%datbul_js,pvs)
    if ( MSP_PROBLEME ) then
       call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",&
            routine="ps_ecrire_param_initiaux")
	    
       if ( MSP_ERREUR ) return
    endif
    call pswresu (1,ilogeph,iscreen,1,iter,ypas,&
         str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
         str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
         str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
    if(MSP_gen_messages("ps_ecrire_param_initiaux")) return

    call pswresu (2,ilogeve,0,1,iter,ypas,str_gen(iveh)%etat,&
         str_ecr(iveh)%nvarw,&
         str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
         str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
    if(MSP_gen_messages("ps_ecrire_param_initiaux")) return

  end subroutine ps_ecrire_param_initiaux


  subroutine ps_rech_date_premier_evt (ypas)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rech_date_premier_evt
!
!$Resume
!   determine le temps écoulé depuis le début de la simulation
!   à la  date du premier evenement
!
!$Description
!   determine le temps écoulé depuis le début de la simulation
!   à la  date du premier evenement
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_rech_date_premier_evt (ypas)
!.    real (kind=pm_reel) :: ypas
!
!$Arguments
!>E/S   ypas  :<pm_reel>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
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

    use ps_interface_psimu_don

    implicit none

! Arguments 
! =========
    real (kind=pm_reel), intent (inout) :: ypas

! Variables locales
! =================
    integer :: nfin
    logical :: lboucle


    if (str_eve(iveh)%ndeve > 0) then
       if (str_int(iveh)%itsgn > 0) then
          str_eve(iveh)%neve = 1
          nfin = str_eve(iveh)%ndeve + 1
       else
          str_eve(iveh)%neve = str_eve(iveh)%ndeve
          nfin = 0
       endif
       lboucle = .true.
       do while (lboucle)
          if (str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve) >= str_int(iveh)%tsign*ypas) then
             lboucle = .false.
          else
             str_eve(iveh)%neve = str_eve(iveh)%neve + str_int(iveh)%itsgn
             if (str_eve(iveh)%neve == nfin) lboucle = .false.
          endif
       enddo
    endif

  end subroutine ps_rech_date_premier_evt


  subroutine ps_rech_date_premiere_sepa (ypas)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rech_date_premiere_sepa
!
!$Resume
!   determine le temps écoulé depuis le début de la simulation
!   à la  date de la première séparation
!
!$Description
!   determine le temps écoulé depuis le début de la simulation
!   à la  date de la première séparation
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_rech_date_premiere_sepa (ypas)
!.    real (kind = pm_reel) :: ypas
!
!$Arguments
!>E/S   ypas  :<pm_reel>   
!
!$Common
!
!$Routines
!- MSP_effacer_separation
!- MSP_consulter_scenario
!- MSP_consulter_separation
!
!$Include
!
!$Module
!#V
!- ps_propulsion
!- ps_interface_psimu_don
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

    use ps_propulsion
    use ps_interface_psimu_don

    implicit none

    ! Arguments
    ! =========
    real (kind = pm_reel), intent(inout) :: ypas

    ! Variables locales 
    ! =================
    integer                              :: iloifin, sc_type
    logical                              :: lboucl
    real(KIND=pm_reel)                   :: tsep
    type(MSP_SEPARATION)                 :: loi_sep


    
    ! Initialisations
    !================
    call MSP_effacer_separation(loi_sep,nul=.true.)
    if ( MSP_gen_messages("ps_rech_date_premiere_sepa") ) return

    !  Lecture de la structure MSP_SCENARIO_LOI
    call MSP_consulter_scenario (scenar_sep(iveh), type=sc_type, nloi=nloi_scenar_sep(iveh))
    if ( MSP_gen_messages("ps_rech_date_premiere_sepa") ) return

    !   Si le scenario est de type separation      
    if ( sc_type == MSP_ENUM_SEPARATION ) then
       if (nloi_scenar_sep(iveh) > 0) then

          if (str_int(iveh)%itsgn > 0) then
             iloi_scenar_sep(iveh) = 1
             iloifin = nloi_scenar_sep(iveh) + 1
          else
             iloi_scenar_sep(iveh) = nloi_scenar_sep(iveh)
             iloifin = 0
          endif

          lboucl = .true.
          do while (lboucl)

             ! Pour une loi de separation
             !  extraction de la structure MSP_SEPARATION dans la structure scenario (MSP_SCENARIO_LOI)
             call MSP_consulter_scenario (scenar_sep(iveh), loi_sep=loi_sep, id=iloi_scenar_sep(iveh))
             if ( MSP_gen_messages("ps_rech_date_premiere_sepa") ) return

             !  Lecture des donnees dans la structure loi de separation (MSP_SEPARATION)

             call MSP_consulter_separation (loi_sep, date=tsep)
             if ( MSP_gen_messages("ps_rech_date_premiere_sepa") ) return


             if (str_int(iveh)%tsign*tsep >= str_int(iveh)%tsign*ypas) then
                lboucl = .false.
             else
                iloi_scenar_sep(iveh) = iloi_scenar_sep(iveh) + str_int(iveh)%itsgn
                if (iloi_scenar_sep(iveh) == iloifin) lboucl = .false.
             endif
             call MSP_effacer_separation(loi_sep)
          enddo

       endif
    endif

    ! FA-IF 746 : désallocation
    call MSP_effacer_separation(loi_sep)
  end subroutine ps_rech_date_premiere_sepa

  subroutine ps_test_arret_propulsion_CI (timp, ntab, jflag, iter, ypas, datex, pvs, kdeb, kfin,inicow)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_test_arret_propulsion_CI
!
!$Resume
!   Tests d'arret ou de propulsion pour les conditions initiales
!
!$Description
!   Tests d'arret ou de propulsion pour les conditions initiales
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_test_arret_propulsion_CI (timp, ntab, jflag, iter, ypas, datex, pvs, kdeb, kfin,inicow)
!.    real (kind=pm_reel) :: ypas
!.    type(tm_jour_sec) :: datex
!.    real (kind=pm_reel), dimension(:), pointer :: timp
!.    real (kind=pm_reel), dimension(6) :: pvs
!.    integer :: ntab 
!.    integer :: jflag, iter
!.    integer :: kdeb, kfin, inicow
!
!$Arguments
!>E/S   timp    :<pm_reel,DIM=(:),pointer>   
!>E     ntab    :<integer>                   
!>E/S   jflag   :<integer>                   
!>E/S   iter    :<integer>                   
!>E/S   ypas    :<pm_reel>                   
!>E/S   datex   :<tm_jour_sec>               
!>E/S   pvs     :<pm_reel,DIM=(6)>           
!>E/S   kdeb    :<integer>                   
!>E/S   kfin    :<integer>                   
!>E/S   inicow  :<integer>                   
!
!$Common
!
!$Routines
!- ps_calcul_altitude_pente
!- MSP_effacer_poussee_continue
!- MSP_consulter_scenario
!- MSP_consulter_impulsion
!- ps_libere_cowell
!- pspimpu
!- MSP_signaler_message
!- pspcont
!- psnewbl
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_propulsion
!- ps_integration
!- ps_integration_cowell
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

    use ps_interface_psimu_don
    use ps_propulsion
    use ps_integration
    use ps_integration_cowell

    implicit none

    ! Arguments 
    ! =========
    real (kind=pm_reel),intent (inout)              :: ypas
    type(tm_jour_sec)  ,intent (inout)              :: datex
    real (kind=pm_reel), dimension(:), pointer      :: timp
    real (kind=pm_reel), dimension(6), intent(inout):: pvs
    integer, intent (in)                         :: ntab 
    integer, intent (inout)                         :: jflag, iter
    integer, intent (inout)                         :: kdeb, kfin, inicow
      
    ! Variables locales
    ! =================
    logical             :: loi_scenario, libere_cowell
    integer             :: kflag
    integer             :: typloi
    real (kind=pm_reel) :: ralt
    real (kind=pm_reel) :: pente, deltav, datetimp
    type(MSP_IMPULSION) :: loi_imp
    type(MSP_POUSSEE_CONTINUE) :: loi_cont

    ! Code
    !=====

    ralt = sqrt(pvs(1)**2+pvs(2)**2+pvs(3)**2) - str_mod(iveh)%requa
    
    ! DM 959 : Calcul de la pente de la vitesse du véhicule
    ! Ce calcul permet de savoir si la trajectoire du véhicule
    ! est montante ou descendante dans le cas de h<=hstop.
    ! Le critère d'arret doit prendre en compte la valeur de cette pente
    call ps_calcul_altitude_pente(pvs,ralt,pente)
    
    
    ! En dessous de hstop, le véhicule est en descente (pente de sa vitesse < 0) on arrete la simulation
    if ( ralt <= str_int(iveh)%hstop .and. pente < 0) then
       jflag=2
       
    ! Temps de calcul atteint, fin de la simulation   
    else if (str_int(iveh)%tmax == 0._pm_reel) then
       jflag=1
    else

       loi_scenario = (nloi_scenar_pro(iveh) > 0)
       loi_scenario = loi_scenario .and. &
            (iloi_scenar_pro(iveh) /= 0 .and. (iloi_scenar_pro(iveh) /= (nloi_scenar_pro(iveh) + 1)) )

       if (loi_scenario ) then

          if (str_int(iveh)%itsgn > 0) then
             kdeb = 1
             kfin = ntab
          else
             kdeb = ntab
             kfin = 1
          endif

          if ( (str_int(iveh)%tsign*ypas >= str_int(iveh)%tsign*timp(kdeb)) &
               .and. (str_int(iveh)%tsign*ypas <= str_int(iveh)%tsign*timp(kfin))) then
             !                 On commence par une poussee ...
             !                 ===========================
             !/  Extraction du type de la loi
             typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
             if ( MSP_gen_messages("ps_test_arret_propulsion_CI") ) return

             libere_cowell = .true.

             ! Analyse des poussées, pour éventuellemnent empêcher la réinitialisation automatique
             ! du Cowell
             if (typloi == MSP_ENUM_LOI_CONT) then
 
                ! Est-ce que la poussée à une durée nulle ?
                ! -> si c'est le cas, alors il ne faut pas ré-initialiser le Cowell 
                ! après la propulsion, car celle-ci est sans effet 
                ! et ne modifie pas les conditions initiales.
                if (abs(timp(kfin) - timp(kdeb)) < MSP_EPSILON_APLA) then
                   libere_cowell = .false.
                end if

                ! Libération de la mémoire
                call MSP_effacer_poussee_continue (loi_cont)
                if ( MSP_gen_messages("psbcowe") ) return

             elseif (typloi == MSP_ENUM_LOI_IMP) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbcowe") ) return

                call MSP_consulter_impulsion (loi_imp, date=datetimp, deltav=deltav)
                if ( MSP_gen_messages("psbcowe") ) return

                ! Est-ce que la poussée est nulle (delta V = 0 m/s) ?
                ! -> si c'est le cas, alors il ne faut pas ré-initialiser le Cowell 
                ! après la propulsion, car celle-ci est sans effet 
                ! et ne modifie pas les conditions initiales.
                if (deltav < MSP_EPSILON_APLA) then
                   libere_cowell = .false.
                end if
                
             end if

             if (libere_cowell) then
                  !/ On libere l'intégrateur de Cowell avant la manoeuvre
                  ! car les conditions du calcul après manoeuvres (P/V, date, etc.)
                  ! auront changé.
                  !
                  ! /!\ Seuls deux cas imposent de ne pas ré-initialiser l'intégrateur :
                  ! - poussée impulsionnelle nulle (le bulletin reste inchangé)
                  ! - poussée continue de durée nulle (le bulletin reste inchangé)
                
                inicow = 0
                call ps_libere_cowell()
             end if

             if (typloi == MSP_ENUM_LOI_IMP) then

                call pspimpu (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,pvs,iter)
                if ( MSP_PROBLEME ) then
                   call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",&
                        routine="ps_test_arret_propulsion_CI")
                   if ( MSP_ERREUR ) return
                endif

             else if (typloi == MSP_ENUM_LOI_CONT) then

                call pspcont (str_int(iveh)%itsgn,str_int(iveh)%tsign,&
                     str_int(iveh)%hstop,str_int(iveh)%tmax,ypas,datex,&
                     pvs,iter,kflag)
                if ( MSP_PROBLEME ) then
                   call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",&
                        routine="ps_test_arret_propulsion_CI")
                   if ( MSP_ERREUR ) return
                endif !MSP_probleme

                if (kflag /= 0) then

                   if (ypas .egal. timp(kfin)) then
                      iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
                   endif
                   jflag=kflag
                   return

                endif ! kflag /= 0

             else ! typloi == MSP_ENUM_LOI_XXX

                call psnewbl (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,pvs,iter)
                if ( MSP_PROBLEME ) then
                   call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",&
                        routine="ps_test_arret_propulsion_CI")
                   if ( MSP_ERREUR ) return
                endif

             end if ! typloi == MSP_ENUM_LOI_XXX

             iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn

          end if

       end if
       
       ! En dessus de h2, on revient au Cowell
       if (ralt > str_int(iveh)%h2) then
	 jflag=3
	  
       ! En dessous de h2, on revient au Gill
       ! En dessous de hstop, le véhicule est en montée (pente de sa vitesse >= 0) on continue la simulation
       else
          jflag=4
       endif

    endif !ralt >= str_int(iveh)%hstop .and. pente 

  end subroutine ps_test_arret_propulsion_CI

  subroutine ps_rech_date_premiere_poussee(ypas, kdeb, kfin, ntab, timp)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rech_date_premiere_poussee
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_rech_date_premiere_poussee(ypas, kdeb, kfin, ntab, timp)
!.    real(kind=pm_reel) :: ypas
!.    integer :: kdeb, kfin
!.    integer :: ntab
!.    real (KIND=pm_reel), pointer :: timp(:)
!
!$Arguments
!>E     ypas  :<pm_reel>                   
!>E/S   kdeb  :<integer>                   
!>E/S   kfin  :<integer>                   
!>S     ntab  :<integer>                   
!>E/S   timp  :<pm_reel,DIM=(:),pointer>   
!
!$Common
!
!$Routines
!- MSP_effacer_poussee_continue
!- MSP_consulter_scenario
!- MSP_consulter_poussee_continue
!- MSP_consulter_impulsion
!- MSP_consulter_poussee_bulletin
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_propulsion
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
    
    use ps_interface_psimu_don
    use ps_propulsion

    implicit none

    ! Arguments 
    !==========
    real(kind=pm_reel), intent(in) :: ypas
    integer, intent(inout)       :: kdeb, kfin
    integer, intent(out)         :: ntab
    real (KIND=pm_reel), pointer :: timp(:)



    ! Variables locales
    !==================
    integer :: sc_type, typloi
    logical :: lboucl
    integer :: iloifin, i
    real(kind=pm_reel) :: datedeb, datbul, date_ref, tbul, datetimp
    real(kind=pm_reel), parameter :: datprec_tmp = 0.001_pm_reel

    
    type(MSP_IMPULSION) :: loi_imp
    type(MSP_POUSSEE_CONTINUE) :: loi_cont
    type(MSP_POUSSEE_BULLETIN) :: loi_bul

    ! Début du code
    !==============

    ntab = 0 ! FA-ID 1209 : initialisation de ntab

    ! Initialisations
    call MSP_effacer_poussee_continue(loi_cont,nul=.true.)
    if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return
        
!  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), type=sc_type, nloi=nloi_scenar_pro(iveh))
      if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

!   Si le scenario est de type propulsion
      if ( sc_type == MSP_ENUM_PROPULSION .and. nloi_scenar_pro(iveh) > 0) then

            if (str_int(iveh)%itsgn > 0) then

               ! FA-ID 1228 : le compteur de loi devrait partir à 0, car il est incrémenté ensuite 
               ! pour chaque loi

               iloi_scenar_pro(iveh) = 1
               iloifin = nloi_scenar_pro(iveh) + 1
            else
               iloi_scenar_pro(iveh) = nloi_scenar_pro(iveh)
               iloifin = 0
            endif

            lboucl = .true.
            do while (lboucl)

!/  Extraction du type de la loi
               typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
               if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

               if (typloi == MSP_ENUM_LOI_CONT) then
                  !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                  call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=iloi_scenar_pro(iveh))
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

                  call MSP_consulter_poussee_continue (loi_cont, ntab=ntab,  dates=timp)
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

                  ! Libération de la mémoire
                  call MSP_effacer_poussee_continue (loi_cont)
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

               elseif (typloi == MSP_ENUM_LOI_IMP) then
                  !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                  call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=iloi_scenar_pro(iveh))
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

                  call MSP_consulter_impulsion (loi_imp, date=datetimp)
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return
                  ntab = 1
                  if (associated(timp)) deallocate(timp)
                  allocate(timp(1))
                  timp(1) = datetimp

               elseif (typloi == MSP_ENUM_LOI_BUL) then
                  !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                  call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return
                  call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=iloi_scenar_pro(iveh))
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return

                  call MSP_consulter_poussee_bulletin (loi_bul, datedeb=datedeb, datbul = datbul)
                  if ( MSP_gen_messages("ps_rech_date_premiere_poussee") ) return
                  
                  if (associated(timp)) deallocate(timp)
                  allocate(timp(2))

                  timp(1) = datedeb
                  tbul = (datbul - date_ref)*86400._pm_reel
                  ! Arrondi à la milli-seconde:
                  timp(2) = real(int(tbul),KIND=pm_reel) + &
                       real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp
                  ntab = 2

               endif


               if (typloi == MSP_ENUM_LOI_CONT) then
                  
                  if (str_int(iveh)%itsgn > 0) then
                     kdeb = 1
                     kfin = ntab
                  else
                     kdeb = ntab
                     kfin = 1
                  endif
                  
                  do i=kdeb,kfin,str_int(iveh)%itsgn
                     if (i == kfin .and. str_int(iveh)%tsign*timp(i) > str_int(iveh)%tsign*ypas) then
                        lboucl = .false.
                     else if (str_int(iveh)%tsign*timp(i) >= str_int(iveh)%tsign*ypas) then
                        lboucl = .false.
                     end if
                  end do
               else 
                  ! Loi de poussée impulsionnelle ou bulletin
                  if (str_int(iveh)%tsign*timp(1) >= str_int(iveh)%tsign*ypas) then
                     ! la date de la manoeuvre i dépasse la date courante
                     lboucl = .false.
                  end if 
               end if
               
               if ( lboucl ) then
                  iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
               end if
               
               if (iloi_scenar_pro(iveh) == iloifin) then
                  lboucl = .false.
               end if

            end do

         endif

  end subroutine ps_rech_date_premiere_poussee

  subroutine ps_convertir_bulletins_poussee(kdeb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_convertir_bulletins_poussee
!
!$Resume
!  Convertit les bulletins des manoeuvres bulletins pour les passer
!  dans le repère d'intégration
!
!$Description
!  Convertit les bulletins des manoeuvres bulletins pour les passer
!  dans le repère d'intégration
!
!$Auteur
!  Y TANGUY (ATOS)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_convertir_bulletins_poussee(kdeb)
!.    integer :: kdeb
!
!$Arguments
!>E/S   kdeb  :<integer>   
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_poussee_bulletin
!- MSP_consulter_bulletin
!- mx_var
!- MSP_annuler_probleme
!- md_jourfrac_joursec
!- MSP_conv_typrep
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_propulsion
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
    
    use ps_interface_psimu_don
    use ps_propulsion

    implicit none

    ! Arguments
    !==========
    integer, intent(inout)       :: kdeb


    ! Variables locales
    !==================
    type(MSP_POUSSEE_BULLETIN) :: loi_bul
    type(MSP_BULLETIN) :: bul
    type(tm_pole_uv)            :: pole
    type(tm_code_retour) :: code_erreur
    type(tm_ellipsoide)      :: ellips
    type(tm_jour_sec)        :: trf
    real(kind=pm_reel) :: date_ref, datdeb, tbul
    real (KIND=pm_reel), dimension(6)   :: parbid,parbidnew, bpvs
    real(KIND=PM_REEL), dimension(10) :: brep
    real(KIND=PM_REEL) :: bmu, brequa, bapla, brequa_r, bapla_r, bvitrot
    real(KIND=PM_REEL) :: bpolu, bpolv, apla_rep, p_datbul
    real (KIND=pm_reel), dimension(6,6) :: rdxdp 

    integer :: sc_type, sc_nloi, iloi
    integer :: typloi, typdat
    integer   :: bnumpla, bcle_date, biorb
        
    real(kind=pm_reel), parameter :: datprec_tmp = 0.001_pm_reel    
    
    ! Début du code
    !==============

       
    !/  Lecture de la structure MSP_SCENARIO_LOI
    call MSP_consulter_scenario (scenar_pro(iveh), type=sc_type, nloi=sc_nloi)
    if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return

    !   Si il y a des lois de propulsion
    if (nloi_scenar_pro(iveh) > 0) then

       if ( sc_type /= MSP_ENUM_PROPULSION ) then
          !/  Si la loi de  scenario n'est pas de type propulsion
          call MSP_signaler_message (cle_mes="PS_CONV_001")
          return
       endif

       !/  Boucle sur les lois du scénario:
       do iloi = 1 , nloi_scenar_pro(iveh)

          !/  Extraction du type de la loi
          typloi = MSP_type_loi (scenario=scenar_pro(iveh),id=iloi)
          if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return

          if (typloi == MSP_ENUM_LOI_BUL) then
             !/  Pour une loi de propulsion de type bulletin (MSP_POUSSEE_BULLETIN)

             !/  extraction de la structure loi MSP_POUSSEE_BULLETIN dans la structure scenario (MSP_SCENARIO_LOI)
             call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
             if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return
             call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=iloi)
             if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return

             !/  Lecture des donnees dans la structure MSP_POUSSEE_BULLETIN + extraction de la structure Bulletin
             call MSP_consulter_poussee_bulletin (loi_bul, typdat=typdat, &
                  datedeb=datdeb, bul=bul)
             if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return

             !/  Lecture des donnees dans la structure bulletin
             call MSP_consulter_bulletin (bul,datbul = p_datbul,&
                  param=parbid(:),&
                  rep=brep(:),&
                  iorb = biorb,&
                  cle_date=bcle_date,&
                  planete=bnumpla,&
                  mu=bmu, vitrot=bvitrot, &
                  apla=bapla, requa=brequa, &
                  apla_r=bapla_r, requa_r=brequa_r, &
                  pole_v=bpolv, pole_u=bpolu)
             if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return

             if (datdeb < 0._pm_reel) then
                kdeb = 1
             else
                kdeb = 2
             endif

             ! -- Conversion du type de paramètres

             ! Test de la valeur de apla_r avant passage de 1/apla_r
             ! DM 200 : str_pro(iveh)%bapla au lieu de str_mod(iveh)%rapla
             if (bapla .different. 0._PM_REEL) then
                apla_rep = 1._PM_REEL /   bapla
             else
                apla_rep = 0._PM_REEL
             endif

             ellips%r_equa = brequa
             ellips%apla   = apla_rep

             call mx_var(biorb, parbid, MSP_ENUM_CARTESIENS,parbidnew, &
                  code_erreur, mu = bmu, ellips = ellips)

             if ( (code_erreur%valeur /= pm_warn_para_opt_trop) .and. &
                  (code_erreur%valeur /= pm_OK) ) then
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (code_erreur%valeur > 0 ) call MSP_annuler_probleme()
             endif

             if (MSP_ERREUR) then
                call MSP_signaler_message (cle_mes="PS_CONV_BUL_004")
                return
             endif

             !/  Transformation de la date issus du bulletin de poussee en duree
             tbul = (p_datbul - date_ref)*86400._pm_reel
             ! Arrondi à la milli-seconde:
             tbul = real(int(tbul),KIND=pm_reel) + &
                  real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp

             if ((brep(1) == MSP_ENUM_PLANETO_REF_INER) .or. &
                  (brep(1) == MSP_ENUM_PLANETO_VRAI)) then
                if ( bcle_date == MSP_ENUM_ECHD_DATE_BUL ) then
                   trf = str_bul(iveh)%datbul_js + tbul
                else
                   call md_jourfrac_joursec(brep(4), trf, code_erreur)
                endif
             else
                trf = str_bul(iveh)%datbul_js + tbul
             endif

             ! Changement de repere "repere bulletin propulsion" --> TIV

             ! Test de la valeur de apla_r avant passage de 1/apla_r
             if (bapla_r .different. 0._PM_REEL) then
                apla_rep = 1._PM_REEL / bapla_r
             else
                apla_rep = 0._PM_REEL
             endif

             ! Initialisation de la variable pole pour appel MSP_conv_typrep
             pole%u = bpolu
             pole%v = bpolv

             

             call MSP_conv_typrep (parbidnew,&
                  int(brep(1)), bcle_date, &
                  bnumpla, trf, MSP_ENUM_ECHT_TE ,&
                  brep(3), brep(2:10), &
                  brequa_r, apla_rep, &
                  int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas,&
                  str_bul(iveh)%datbul_js, MSP_ENUM_ECHT_TE,&
                  str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
                  str_bul(iveh)%requa_r, apla_rep, &
                  bvitrot, &
                  bpvs, mat_jacob=rdxdp, obli=str_bul(iveh)%obli, vrot_out=str_bul(iveh)%vitrot, &
                  pole_in=pole, pole_out=pole)

             if ( MSP_gen_messages("ps_convertir_bulletins_poussee") ) return 

             parman_car(iloi,:,kdeb) = bpvs(:)
          endif

       enddo
    endif

  end subroutine ps_convertir_bulletins_poussee
  
  subroutine ps_convertir_bulletin_initial(reinit_bulletin,pvs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_convertir_bulletin_initial
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_convertir_bulletin_initial(reinit_bulletin,pvs)
!.    integer :: reinit_bulletin
!.    real(kind=pm_reel), dimension(6) :: pvs
!
!$Arguments
!>E     reinit_bulletin  :<integer>           
!>S     pvs              :<pm_reel,DIM=(6)>   
!
!$Common
!
!$Routines
!- mx_var
!- MSP_signaler_message
!- MSP_annuler_probleme
!- md_jourfrac_joursec
!- MSP_conv_typrep
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_calcul_forces
!- ps_integration_don
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
         
    use ps_bulletin
    use ps_calcul_forces
    use ps_integration_don

    implicit none 

    ! Arguments
    !==========
    integer, intent(in) :: reinit_bulletin
    real(kind=pm_reel), dimension(6), intent(out) :: pvs


    ! Variables locales
    !==================
    type(tm_code_retour) :: code_erreur
    type(tm_ellipsoide)      :: ellips
    type(tm_pole_uv)            :: pole
    real (KIND=pm_reel), dimension(6)   :: parnew, param_ROUT
    real (KIND=pm_reel), dimension(6,6) :: rdxdp, mat_rout_ri
    real(KIND=PM_REEL) :: apla_rep 
    real(kind=pm_reel) :: r2, v2, ray, unsura
    type(tm_jour_sec) :: trf
    
    ! Début du code
    !==============

    if (reinit_bulletin == 1) then
       ! Conversion à partir du bulletin initial
       !========================================


       !   * Passage en paramètres cartésiens pour éviter
       !     tous problèmes de conversion:

       ! Test de la valeur de apla_r avant passage de 1/apla_r
       ! DM 200 : str_bul(iveh)%apla_r au lieu de str_mod(iveh)%rapla
       if (str_bul(iveh)%apla .different. 0._PM_REEL) then
          apla_rep = 1._PM_REEL / str_bul(iveh)%apla
       else
          apla_rep = 0._PM_REEL
       endif

       ellips%r_equa = str_bul(iveh)%requa
       ellips%apla   = apla_rep

       call mx_var(str_bul(iveh)%iorb, str_bul(iveh)%param, MSP_ENUM_CARTESIENS, parnew, &
            code_erreur, mu=str_bul(iveh)%mu, ellips = ellips)

       if ( (code_erreur%valeur /= pm_warn_para_opt_trop) .and. (code_erreur%valeur /= pm_OK) ) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (code_erreur%valeur > 0 ) call MSP_annuler_probleme()
       endif


       if (MSP_ERREUR) then
          call MSP_signaler_message (cle_mes="PS_CONV_BUL_001")
          return
       endif

       !     Calcul de la Jacobienne
       if ((str_bul(iveh)%rep(1) == MSP_ENUM_PLANETO_REF_INER) .or. &
            (str_bul(iveh)%rep(1) == MSP_ENUM_PLANETO_VRAI) ) then
          if ( str_bul(iveh)%cle_date == MSP_ENUM_ECHD_DATE_BUL ) then
             trf = str_bul(iveh)%datbul_js
          else
             ! repere a une date de reference fixee
             call md_jourfrac_joursec(str_bul(iveh)%rep(4), trf, code_erreur)
             trf = trf + str_bul(iveh)%rep(5) 
          endif
       else
          trf = str_bul(iveh)%datbul_js !! datbul ou datbul0 ????
       endif

       str_bul(iveh)%rep(2) = real(MSP_ENUM_ECHT_TE,kind=pm_reel)
       str_int(iveh)%reps(2) = real(MSP_ENUM_ECHT_TE,kind=pm_reel)

       ! Passage du repère d'expression du bulletin au repère d'intégration 

       ! Test de la valeur de apla_r avant passage de 1/apla_r
       if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
          apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
       else
          apla_rep = 0._PM_REEL
       endif

       ! Initialisation de la variable pole pour appel MSP_conv_typrep
       pole%u = str_bul(iveh)%polu
       pole%v = str_bul(iveh)%polv

       call MSP_conv_typrep (parnew,&
            int(str_bul(iveh)%rep(1)), str_bul(iveh)%cle_date, str_bul(iveh)%num_pla,  &
            trf, str_bul(iveh)%ech_temps_bul,&
            str_bul(iveh)%rep(3), str_bul(iveh)%rep(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas,&
            str_bul(iveh)%datbul_js, MSP_ENUM_ECHT_TE,&
            str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            str_bul(iveh)%vitrot, &
            pvs, mat_jacob=rdxdp, obli=str_bul(iveh)%obli, pole_in=pole, pole_out=pole)


       if ( MSP_gen_messages("ps_convertir_bulletin_initial") ) return 

       ! On regarde si on se trouve sur une ellipse ou une hyperbole:
       r2  = pvs(1)**2+pvs(2)**2+pvs(3)**2
       v2  = pvs(4)**2+pvs(5)**2+pvs(6)**2
       ray = sqrt(r2)
       if (ray<eps) then
          call MSP_signaler_message(cle_mes="PS_RAY_001")
          return
       endif
       unsura = 2._pm_reel/ray - v2/str_mod(iveh)%gmu

       if (unsura >= 0) then
          str_gen(iveh)%hyp=0
       else
          str_gen(iveh)%hyp=1
       endif

    else
       ! Conversion à partir du vecteur d'état
       !======================================
       
       mat_Rout_RI = transpose(str_int(iveh)%Mat_RI_ROUT)

       param_ROUT(1:3) = str_gen(iveh)%etat(3:5) 
       param_ROUT(4:6) = str_gen(iveh)%etat(9:11)
 
       pvs = matmul(mat_Rout_RI,param_ROUT)
       
    end if


  end subroutine ps_convertir_bulletin_initial

  subroutine ps_terminer_session_PSIMU()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_terminer_session_PSIMU
!
!$Resume
!  Routine de fin de session PSIMU, qui réalise les désallocations mémoire
!$Description
!  Routine de fin de session PSIMU, qui réalise les désallocations mémoire
!  des différentes structures MECASPA, et quitte COMPAS
!$Auteur
!  Y.TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_terminer_session_PSIMU()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_effacer_vehicule
!- MSP_effacer_scenario
!- MSP_effacer_modvent
!- MSP_effacer_ephemerides
!- cps_close_utilisateur
!- eph_closeposcor
!- ps_libere_integrateurs
!
!$Include
!
!$Module
!#V
!- ps_integration
!- ps_propulsion_don
!- ps_attitude
!- ps_separations
!- ps_atmosphere
!- ps_troiscorps
!- cps_utilisateur
!- ephem
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

    use ps_integration
    use ps_propulsion_don
    use ps_attitude
    use ps_separations
    use ps_atmosphere
    use ps_troiscorps
    use cps_utilisateur
    use ephem

    implicit none

    ! Variables locales
    !==================
    integer :: ii, iostat, delta


    ! Libération mémoire dans COMPAS et PSIMU


    ! Appel à la routine COMPAS avec l'argument dernier_appel = .true.
    ! -> cela libère les tableaux utilisés pour stocker les sauts du TUC
    call cps_get_sautTAITUC(str_bul(iveh)%datbul0_js,delta,dernier_appel=.true.)

    ! -> fermeture des modules appelés
    ! COMPAS (partie commune) : systématique
    ! COMPAS (éphémérides)    : si les éphémérides analytiques ont été utilisés
    ! intégrateurs : systématique
    !===================

    call cps_close_utilisateur()
    call eph_closeposcor()
    
    call ps_libere_integrateurs()

    
    ! Libération mémoire des structures MECASPA
    !==========================================
    do ii=1,PS_NVMAX

       if (tab_veh_init(ii) == 1) then
          
          ! Structure véhicule
          call MSP_effacer_vehicule(str_car(ii)%vehicule)

          ! Scénarios de propulsion, attitude, séparations
          call MSP_effacer_scenario(scenar_pro(ii))
          call MSP_effacer_scenario(scenar_ati(ii))
          call MSP_effacer_scenario(scenar_sep(ii))

          ! Modèles :
          ! rmq : les éphémérides (MSP_ephemerides) ne peuvent pas être désalloués 
          ! correctement, car il n'existe pas de fonction pour mettre 
          ! à NULL les pointeurs. Et une désallocation sur pointeur non initialisé
          ! risque de provoquer un "seg fault"                                    
          call MSP_effacer_modvent(str_mod(ii)%vent)
          call MSP_effacer_atmosphere(str_mod(ii)%atm)
          call MSP_effacer_potentiel(str_mod(ii)%pot)
          if (associated(str_mod(ii)%vj)) then
             deallocate(str_mod(ii)%vj,stat=iostat)
             if (iostat < 0) then
                call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                     partie_variable="ps_terminer_session_PSIMU")
             end if
          end if

          

          if (associated(str_ecr(ii)%ypas_sorties)) then
             deallocate(str_ecr(ii)%ypas_sorties,stat=iostat)
             if (iostat < 0) then
                call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                     partie_variable="ps_terminer_session_PSIMU")
             end if
          end if

          tab_veh_init(ii) = 0

       end if

    end do
   
  end subroutine ps_terminer_session_PSIMU


end module ps_initialisations




