module ps_variables

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_variables
!
!$Resume
!   Module contenant le sous-programme psnetat mettant à jour les variables à la date courante.
!
!$Description
!   Module contenant le sous-programme psnetat mettant à jour les variables à la date courante.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_variables.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_variables.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.116  2010/11/09 13:05:44  mercadig
!  VERSION::AQ::09/11/2010:Suppression de variables inutilisees delta_tai et delta_tu1
!
!  Revision 1.115  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.114  2010/10/22 15:48:33  mercadig
!  VERSION::FA-ID:1404:22/10/2010:Modification du test sur la valeur du code retour apres appel a me_eck_hech_moy
!
!  Revision 1.113  2010/10/22 10:06:21  mercadig
!  VERSION::FA-ID:1426:22/10/2010:Correction de la conversion de l heure locale de radians en heures
!
!  Revision 1.112  2010/09/30 08:32:09  mercadig
!  VERSION::FA-ID:1453:30/09/2010:Correction de la longitude moyenne
!
!  Revision 1.111  2010/08/25 12:50:13  mercadig
!  VERSION::AQ::25/08/2010: Initialisation des variables locales fp, omega et omegap dans ps_poussee_moteur
!
!  Revision 1.110  2010/06/02 13:08:38  mercadig
!  VERSION::FA-ID:1388:02/06/2010: Mise a zero des acc de frott et de prs dans psi_contrib_ si acc non demandee
!
!  Revision 1.109  2010/03/01 13:35:51  mercadig
!  VERSION:9.6:FA-ID:1344:01/03/2010: Correction du calcul de l angle solaire (retour a l implementation de la 8.2)
!
!  Revision 1.108  2009/11/19 16:25:54  mercadig
!  DM-ID 1019: Mise a jour des cartouches
!
!  Revision 1.107  2009/11/19 16:02:06  mercadig
!  DM-ID 1019: Gestion du repere Gamma vrai de la date (calcul matrice RI-ROUT, mise a jour du temps sideral
!  Revision 1.106  2009/10/28 16:23:14  mercadig
!  DM-ID 1019: Gestion du RIS Gamma vrai de la date
!  Revision 1.105  2009/10/19 10:05:59  kvernelo
!  VERSION:V9.5:FA-ID:1314:19/10/2009:Gestion du cas particulier d une orbite equatoriale
!  Revision 1.104  2009/09/04 15:22:30  mercadig
!  DM-ID 1218: Mise a jour dans les appels a ps_propage, ps_contribution_force et ps_contrib_lune_sol
!  Revision 1.103  2009/08/27 13:44:07  cmartel
!  DM-ID 1164 : Correction du nommage des routines internes
!  Revision 1.102  2009/08/19 09:16:47  tanguyy
!  AQ : corrections de commentaires
!  Revision 1.101  2009/07/16 09:47:37  cml
!  DM-ID 1164 : Creation de la routine de calcul de la vitesse geodesique
!  Revision 1.100  2009/07/16 08:02:16  cml
!  DM-ID 1164 : Ajout de la recopie du temps sideral
!  Revision 1.99  2009/07/15 10:21:40  cml
!  DM-ID 1164 : Decoupage de la routine de mise a jour et ajout de flags conditionant les calculs
!  Revision 1.98  2009/06/16 08:28:17  tanguyy
!  Rappatriement des modifications du patch V9-3-1 (FA-ID 1306)
!  Revision 1.97.2.1  2009/06/11 14:12:18  tanguyy
!  FA-ID 1306 : correction d'une fuite memoire : l'intégrateur de Cowell et Runge-Kutta n'était désalloué que pour le véhicule courant
!  Revision 1.97  2008/12/08 08:39:08  tanguyy
!  AQ : nettoyage de commentaires obsoletes
!  Revision 1.96  2008/12/04 15:42:06  tanguyy
!  AQ : suppression de variables inutilisees
!  Revision 1.95  2008/12/04 13:56:44  tanguyy
!  DM-ID 733 : gestion mémoire dans ps_variables
!  Revision 1.94  2008/12/02 16:56:16  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!  Revision 1.93  2008/12/02 10:45:27  huec
!  DM-ID 1058 : Suppression de variables inutilisees, initialisations de variables et utilisation de la variable yampli
!  Revision 1.92  2008/12/02 08:17:43  tanguyy
!  DM-ID 733 : correction mineures du code suite aux remarques de la relecture de code, mise en forme des cartouches
!  Revision 1.91  2008/11/26 08:54:18  tanguyy
!  DM-ID 733 : utilisation des nouvelles routines de calcul de forces
!  Revision 1.90  2008/09/11 16:02:27  tanguyy
!  DM-ID 1058 : mise a NULL des pointeurs
!  Revision 1.89  2008/04/16 15:53:20  huec
!  DM-ID 859 : Utilisation de boucles explicites
!  Revision 1.88  2008/04/03 16:09:01  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.86  2008/03/31 15:28:11  ttn
!  correction dans pspoter de la place de l'argument du SMC
!  Revision 1.85  2008/03/19 15:21:02  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.84  2008/03/07 09:56:57  huec
!  DM-ID 859 : Utilisation de matmul3, mulvecttrans3, tests des code_erreur
!  Revision 1.83  2008/02/15 16:36:11  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.82  2007/12/06 15:14:21  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.81  2007/11/29 14:42:43  jpi
!  DM-ID 733 : annulation de la DM donc des modifs
!  Revision 1.80  2007/10/30 08:43:31  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.79  2007/10/03 12:27:36  huec
!  DM-ID 733 : Extraction du calcul de la force de pression de radiation solaire
!  Revision 1.78  2007/10/02 08:02:01  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.77  2007/07/13 17:07:55  tanguyy
!  FA-ID 730 : masse d'ergols en sortie de PSIMU
!  Revision 1.76  2007/07/12 10:35:54  tanguyy
!  Annulation systematique du message d'Eckstein-Echler, qui ne devrait etre qu'un warning (FA-ID 714)
!  Revision 1.75  2007/07/09 12:25:43  tanguyy
!  PSIMU V9.0 - intégration DM-ID 688
!  Revision 1.74  2007/06/20 12:33:34  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.73  2007/03/19 14:52:11  mle
!  DM-ID 600 : type de sortie SORTILEGE - calcul dans EME2000 pour le mode Mars
!  Revision 1.72  2007/02/15 13:26:40  mle
!  FA-ID 679 : incoherence des modeles de potentiel issus du format MSDON et des modeles GRGS
!  Revision 1.71.2.2  2007/04/17 08:22:59  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.71.2.1  2007/04/16 09:42:55  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.71  2007/02/02 11:52:06  mle
!  MAJ des commentaires
!  Revision 1.70  2007/02/02 08:23:12  mle
!  DM-ID 643 : modeles de vent dans PSIMU, expression des vitesses dans les bons reperes
!  Revision 1.69  2007/01/29 16:30:45  mle
!  DM-ID 643 : modeles de vents dans PSIMU
!  Revision 1.68  2006/12/05 15:09:47  fabrec
!  suppression de variables redondantes
!  Revision 1.67  2006/12/01 13:39:11  tanguyy
!  PSIMU V8-6 / Integration finale
!  Revision 1.66  2006/11/30 15:20:35  vpg
!  DM-ID 425 : portage Linux. Remplacement des fonctions dfloat, dmod et dsqrt par real, mod et sqrt
!  Revision 1.65  2006/10/19 15:08:25  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.64.2.3  2006/10/17 09:54:29  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.64.2.2  2006/10/13 07:55:18  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.64.2.1  2006/10/09 15:04:53  tanguyy
!  DM-ID 478 : Version initiale du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.64  2006/10/03 06:38:19  aitiere
!  AQ: renommage de la routine psnetat, creation d'une interface
!  Revision 1.63  2006/10/02 13:10:09  aitiere
!  DM-ID 522 : decoupage et suppression des variables inutilisees
!  Revision 1.62  2006/10/02 07:55:41  tanguyy
!  DM-ID 522 : avt decoupage final de psnetat
!  Revision 1.61  2006/09/27 08:29:46  aitiere
!  DM-ID 522 prochain episode (correction)
!  Revision 1.60  2006/09/26 15:45:48  aitiere
!  DM-ID 522 suite
!  Revision 1.59  2006/09/25 10:22:07  aitiere
!  DM-ID 522 version intermediaire
!  Revision 1.58  2006/09/22 08:45:21  aitiere
!  DM-522 version intermediaire
!  Revision 1.57.2.1  2006/09/21 11:58:02  aitiere
!  DM-ID 522 : Version initiale du FT (Amelioration qualite dans PSIMU)
!  Revision 1.57  2006/05/30 09:32:23  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.56  2006/04/21 08:12:24  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.55.2.1  2006/04/20 13:23:18  tanguyy
!  DM-ID 400 : utilisation de routines MSP rendant un pointeur sur la structure encapsulee dans les scenarios de lois
!  Revision 1.55  2006/03/15 13:25:22  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.54  2006/02/27 15:47:38  tanguyy
!  FA-ID 446 : module ps_cowell n est plus utilise
!  Revision 1.53  2005/11/25 14:49:54  tanguyy
!  DM-ID 233
!  Revision 1.52  2005/11/10 18:37:11  vivaresf
!  Mise à jour des cartouches
!  Revision 1.51  2005/11/09 13:23:09  vivaresf
!  DM-ID 6 : remplacement de MSP_posvit_3corps par ps_propage
!  Revision 1.50.2.1  2005/11/22 15:05:16  tanguyy
!  DM-ID 233 : mesures performances
!  Revision 1.50  2005/03/17 08:39:47  fabrec
!  MSLIB90 V6.2
!  Revision 1.48  2005/02/11 09:01:23  fabrec
!  DM-ID 175 : bug calcul de omegap
!  Revision 1.47  2005/02/01 09:01:02  fabrec
!  DM-ID 175 : gestion de la memoire
!  Revision 1.46  2005/01/28 10:42:27  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.45  2005/01/17 15:31:12  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.44  2004/11/22 08:51:53  vivaresf
!  DM-ID 200 : initialisation de apla_rep avant son utilisation (non significatif
!  Revision 1.43  2004/10/12 09:55:53  vivaresf
!  Erreur de frappe
!  Revision 1.42  2004/10/11 16:25:56  vivaresf
!  DM 201 : changement de repere RTS
!  FA 205 : changement de repere RTS bugge sur les attitudes
!  FA 204 : gestion des attitudes
!  FA 202 : erreur dans le calcul des sorties gamma50
!  Revision 1.41  2004/07/08 16:40:04  adm_ipsi
!  DM-ID 89 : Integration - Maj argument pole
!  Revision 1.40  2004/07/07 09:32:58  adm_ipsi
!  DM-ID 89 : reperes interplanetaires
!  Revision 1.38.2.1  2004/07/02 09:52:57  ole
!  Modif reperes de sortie martiens
!  Revision 1.38  2004/06/24 08:58:19  adm_ipsi
!  DM_88
!  Revision 1.37.2.1  2004/06/23 15:59:31  adm_ipsi
!  DM-ID 88 : Utilisation du fichier de ressources associé à la version
!  Revision 1.37  2004/03/31 14:33:56  adm_ipsi
!  FA-ID 117, Passage matrice 6,6 pour Mat_RI_G50, Mat_RI_ME2000 et Mat_RI_ROUT
!  Revision 1.36  2003/12/17 10:33:32  adm_ipsi
!  DM-ID 10, les calculs de PSIMU sont effectués en TE. Suppression des lignes en commentaires
!  Revision 1.35  2003/10/20 16:59:10  adm_ipsi
!  DM-ID 69, lecture unique du fichier des sauts de TUC
!  Revision 1.34  2003/08/08 14:17:08  adm_ipsi
!  suppression use ps_propulsion
!  Revision 1.33  2003/08/05 15:25:24  adm_ipsi
!  FA-ID 26, utilisation de mt_car_meca_vol pour le calcul de VA, PENA, AZIA
!  Revision 1.32  2003/07/10 10:15:08  adm_ipsi
!  FA-ID 12 : Calcul de l'accélération avec propulsion - etat(59) -
!  Revision 1.23  2003/03/28 09:56:47  util_am
!  SLB - Version industrialisée
!  Revision 1.31  2003/03/21 11:16:31  laurent
!  Remplacement de la routine de la amlib AM_coor_aneanv par la routine de la MECASPA MSP_excentrique_to_vraie
!  Revision 1.30  2003/03/20 16:54:53  laurent
!  remplacemant de IO_a_angatt par des routine de la mspro
!  Revision 1.29  2003/03/14 15:41:58  adm_ipsi
!  Utilisation de ier_mslib au lieu de ier_mspro suite à la modification de MSP_signaler_message
!  Revision 1.28  2003/03/10 18:12:00  adm_ipsi
!  Inhibition de l'avertissement sur l'aplatissement nul en sortie de mt_car_meca_vol
!  Revision 1.27  2003/02/20 11:20:26  boschett
!  A Deramecourt : modif appel  mu_3rot_quat
!  Revision 1.26  2003/02/19 10:52:47  rodier
!  PhB - fic_tuc dans MSP_posvit_3corps + correction argument ier_mspro dans MSP_signaler_message
!  Revision 1.25  2003/02/14 15:44:01  rodier
!  PhB - Renommage MSP_date_etutc_mspro en MSP_date_etutc
!  Revision 1.24  2003/02/13 17:48:09  boschett
!  A Deramecourt : portage partiel de AM_coor_anmanv
!  Revision 1.23  2003/02/12 17:08:00  boschett
!  A Deramecourt : retour en arriere suite coredump sur mu_mat_quat
!  Revision 1.22  2003/02/11 16:11:53  boschett
!  A Deramecourt : portage de l'appel de murota (mslib77) en mu_3rot_quat (ms)
!  Revision 1.21  2003/02/11 15:52:29  boschett
!  A Deramecourt : portage de mvrxkg (mslib77) en mv_kepler_gene (mslib90)
!  Revision 1.20  2003/02/11 15:36:18  boschett
!  A Deramecourt : portage de mvredo (mslib77) en mv_car_cir (mslib90)
!  Revision 1.19  2003/02/11 15:13:26  boschett
!  A Deramecourt : portage mvredc (mslib77) a mv_car_cir (mslib90)
!  Revision 1.18  2003/02/11 13:06:42  boschett
!  A Deramecourt : portage de AM_coor_anmane(amlib) a mv_kepler_bar (mslib90)
!  Revision 1.17  2003/02/11 11:17:02  boschett
!  A Deramecourt : portage de l'appel de IO_a_angatt (iolib) en appel de mu_mat_quat et de mu_quat_3rot (mspro)
!  Revision 1.16  2003/02/10 11:04:49  boschett
!  A Deramecourt : portage de IO_p_cafv (IOlib) en mt_car_meca_vol ou mt_car_geoc de la mspro
!  Revision 1.15  2003/02/07 10:17:23  boschett
!  A Deramecourt : portage muamod (mslib77) en dmod de fortran
!  Revision 1.14  2003/02/05 19:35:10  boschett
!  A Deramecourt : modification de l'appel a mrtsid (mslib77) pour mr_tsid_veis (mslib90)
!  Revision 1.13  2003/02/05 16:16:55  boschett
!  A Deramecourt : modification de l'appel meeh6m (mslib77) en appel de me_eck_hech_moy (mslib90)
!  Revision 1.12  2003/02/04 16:22:58  boschett
!  A Deramecourt : remplacement de IO_m_cosiang de la IOlib par atan2
!  Revision 1.11  2003/01/31 12:33:51  boschett
!  A Deramecourt : suppression appels mumat3, muvec3, muvectr3 de la amlib
!  Revision 1.10  2003/01/29 17:19:12  boschett
!  A Deramecourt : modif appel AM_math_mumat3 (amlib) en appel de mumatp (mslib 77)
!  Revision 1.9  2003/01/24 11:51:44  adm_ipsi
!  Utilisation de la mspro pour les conversions de date TE/TUC
!  Revision 1.8  2002/12/20 16:41:16  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.7  2002/12/09 12:17:42  boschett
!  Mise en conformité de la taille des arguments d'appel
!  Revision 1.6  2002/12/06 17:18:44  boschett
!  Suppression du code inutile de la routine psnetat
!  Revision 1.5  2002/12/05 17:40:35  boschett
!  Utilisation de les operateurs MECASPA .egal. et .different. pour tester l'égalité (inégalité) entre réels
!  Revision 1.4  2002/12/04 14:27:25  boschett
!  Suppression des instructions return inutiles en fin de routine ps_variables.F90
!  Revision 1.3  2002/12/02 17:06:08  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.2  2002/11/26 17:06:09  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.22  2001/11/20 16:50:19  util_am
!  Calcul des quaternions et de la date en TE
!  Revision 1.20  2001/11/07 13:13:03  util_am
!  Ajout des variables beta et pann + choix du RIS
!  Revision 1.19  2001/08/21 07:52:16  util_am
!  Bug dans le remplissage de etat(87:89) et etat(90:92)
!  Revision 1.18  2000/09/21 16:31:02  util_am
!  Bug dans la sortie de Psi/Greenwich (en radians au lieu de degrés)
!  Revision 1.17  2000/09/05 12:52:04  util_am
!  Passage à 10 véhicules au lieu de 100
!  Revision 1.16  2000/07/18 13:56:16  util_am
!  Calcul des paramètres orbitaux dans le cas d'une hyperbole avec pvout et non par
!  Revision 1.15  2000/06/27 11:42:40  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.14  2000/04/19 13:06:40  util_am
!  Bug dû à un copier/coller sur phobos et deimos
!  Revision 1.13  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.12  2000/02/08 09:49:48  util_am
!  Modification du calcul des coordonnées dans Greenwich en utilisant mrtsid et non wt*dt
!  Revision 1.11  2000/02/02 09:28:55  util_am
!  Ajout de flags pour d'autres warnings afin de ne les signaler qu'une fois
!  Revision 1.10  2000/01/14 12:17:13  util_am
!  Correction sur le choix de la loi d'attitude quand la date de fin de la loi i-1 est égale à la date de début de la loi i+1 (spécialement pour les poussées et les séparations)
!  Revision 1.9  1999/12/14 09:23:08  util_am
!  Affichage une seule fois du warning sur meeh6m
!  Revision 1.8  1999/12/09 10:21:05  util_am
!  Ajout d'un mode de calcul simplifié avec sorties dans Greenwich + calcul de PSIG, TEG et PHIG
!  Revision 1.7  1999/12/06 14:42:32  util_am
!  Gestion de messages de warning + calcul des paramètres orbitaux moyens même si nzo < 6
!  Revision 1.6  1999/11/18 12:11:39  util_am
!  Correction d'un bug sur le calcul de la direction du soleil si on considère uniquement la pression de radiation solaire
!  Revision 1.5  1999/10/26 10:56:48  util_am
!  Centralisation des matrices de passage RI/ROUT/EME2000/MME2000 et prise en compte du 3ème corps
!  Revision 1.4  1999/08/31 14:58:06  util_am
!  Meilleure prise en compte des sorties dans le cas hyperbolique
!  Revision 1.3  1999/08/31 11:56:13  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:20  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_variables
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psnetat
!- ps_maj_vecteur_etat
!- ps_calcul_vit_geo
!- ps_car_kep_equatorial
!- psi_contribution_forces
!- psi_contrib_frott_atm
!- psi_contrib_rad_sol
!- psi_contrib_lune_sol
!#V
!- ps_calculer_pvout
!- ps_date_te_tuc
!- ps_init_rep_integration_opt2
!- ps_rep_planeto_centrique
!- ps_parametres_orbitaux
!- ps_calcul_anglesRIS
!- ps_calcul_anglesRTS
!- ps_poussee_moteur
!- ps_parametres_EH
!- ps_angle_beta
!- ps_rotation_yawsteering
!#
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_bulletin
!- ps_caracteristiques
!- ps_integration_don
!- ps_modeles
!- ps_attitude
!- ps_propulsion_don
!- ps_ecriture
!- mecaspa
!#
!
!$Interface
!> psnetat :  ps_maj_vecteur_etat
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MECASPA
  use ps_generalites

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_variables.F90 368 2013-02-19 14:43:59Z aadt $'


  interface psnetat 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psnetat
!
!$Resume
!  mise à jour des variables à la date courante
!
!$Description
!  mise à jour des variables à la date courante
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psnetat (ypas,date,par,[choix_loi_atti])
!.    real (KIND=pm_reel) :: ypas
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel), dimension(6) :: par
!.    integer :: choix_loi_atti
!
!$Procedures
!- ps_maj_vecteur_etat
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     module procedure ps_maj_vecteur_etat
  end interface

  private ps_date_te_tuc, ps_init_rep_integration_opt2, ps_rep_planeto_centrique
  private ps_parametres_orbitaux, ps_calcul_anglesRIS, ps_calcul_anglesRTS
  private ps_poussee_moteur, ps_parametres_EH, ps_angle_beta, ps_rotation_yawsteering
  private ps_calculer_pvout

contains

  subroutine ps_maj_vecteur_etat (ypas,date,par,choix_loi_atti)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_maj_vecteur_etat
!
!$Resume
!   Sous-programme mettant à jour les variables à la date courante.
!
!$Description
!   Sous-programme mettant à jour les variables à la date courante.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_maj_vecteur_etat (ypas,date,par,[choix_loi_atti])
!.    real (KIND=pm_reel) :: ypas
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel), dimension(6) :: par
!.    integer :: choix_loi_atti
!
!$Arguments
!>E     ypas            :<pm_reel>           temps écoulé depuis le début de l'extrapolation (sec)
!>E     date            :<tm_jour_sec>       date courante (Jours Juliens)
!>E     par             :<pm_reel,DIM=(6)>   vecteur position/vitesse dans le repère d'intégration (m-m/s)
!>[E]   choix_loi_atti  :<integer>           principe de choix de la loi d'attitude quand deux lois i/i+1 se jouxtent:
!                                  si 1: on prend la loi i, si 2: on prend la loi i+1 (valeur par défaut = 1)
!
!$Common
!
!$Routines
!- MSP_effacer_aero
!- md_joursec_jourfrac
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_signaler_message
!- psattit
!- mu_transpose3
!- mu_matmul3
!- mu_mulvect3
!- ps_propage
!- mt_car_geoc
!- MSP_calcul_ecart_echt
!- mr_tsid_vrai
!- mr_tsid_veis
!- ps_calcul_vit_geo
!- psi_contribution_forces
!#V
!- ps_date_te_tuc
!- ps_calculer_pvout
!- ps_calcul_anglesRIS
!- ps_init_rep_integration_opt2
!- ps_calcul_anglesRTS
!- ps_rep_planeto_centrique
!- ps_parametres_orbitaux
!- ps_poussee_moteur
!- ps_parametres_EH
!- ps_angle_beta
!- ps_rotation_yawsteering
!#
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_caracteristiques
!- ps_integration_don
!- ps_modeles
!- ps_attitude
!- ps_propulsion_don
!- ps_ecriture
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ps_bulletin
    use ps_caracteristiques
    use ps_integration_don
    use ps_modeles
    use ps_attitude
    use ps_propulsion_don
    use ps_ecriture

    implicit none

    ! Arguments
    !==========
    
    real (KIND=pm_reel), intent(IN) :: ypas
    type(tm_jour_sec), intent(in)   :: date
    real (KIND=pm_reel), dimension(6), intent(IN) ::par
    integer, intent(IN), optional :: choix_loi_atti

    ! Variables locales
    !==================

    integer :: typa,irepa, ii
    
    real (kind=pm_reel) :: date_frac
    real (KIND=pm_reel) :: angle1,angle2,angle3,angri1,angri2,angri3
    real (KIND=pm_reel), dimension(3,3) :: pgamav
    real (KIND=pm_reel), dimension(6) :: parorb,parbid
    real (KIND=pm_reel), dimension(6) :: pvs
    type (tm_geocentrique) :: corgeoc
    real(kind=pm_reel)  :: ralt_pla_apla
    real (KIND=pm_reel) :: rlat,rlon,ralt,tsid
    real (KIND=pm_reel) :: sinlat,rayter
    real (KIND=pm_reel), dimension(6) :: pvout
    real (kind=pm_reel), dimension(3,3) :: Pplan_green
    real (KIND=pm_reel) :: teta_planeto_green
    real(kind=pm_reel), dimension(3) :: vrelgam

    real (kind=pm_reel), dimension(3,3) :: Mat_ROUT_ATT , Mat_RTS_ATT, mat_tmp
    real (kind=pm_reel) :: tsid_autre_pla
    real (kind=pm_reel) :: apla_rep
    real (kind=pm_reel) :: masse_vehicule
    real(kind=pm_reel), dimension(3) :: vrelpla, vrelgreen
    type (tm_vit_meca_vol) :: vit_meca_vol

    real (kind=pm_reel) :: mu_soleil
    real (kind=pm_reel), dimension(3) :: pos_soleil,dir_soleil

    real (kind=pm_reel) :: delta_tu1, delta_tai

    type (MSP_MCI) :: mci
    type (MSP_AERO) :: aero
    type (tm_code_retour) :: code_erreur, code_retour_local
    type (tm_jour_sec) :: date_tuc
    type (tm_pole_uv)  :: pole

    logical :: flag_w_parmoy(PS_NVMAX),flag_w_parorb(PS_NVMAX),flag_w_anome(PS_NVMAX)

    data flag_w_parmoy,flag_w_parorb,flag_w_anome &
         / PS_NVMAX* .false. ,PS_NVMAX* .false. ,PS_NVMAX* .false. /

    save flag_w_parmoy,flag_w_parorb,flag_w_anome

    ! Début du code
    !==============

    call MSP_effacer_aero(aero,nul=.true.)
    
    ! DM-ID 600 : rajout de la variable 117, NUM_EVENT
    ! initialisation de cette variable (MAJ dans pswresu de ps_ecriture.F90)
    do ii=1, 117
       str_gen(iveh)%etat(ii) = 0.0_pm_reel
    enddo


!**** Traitement du cas particulier où aucune variable n'est sélectionnée
    if ( .not. str_gen(iveh)%maj_vect_etat%calcul_car_ris) return

!   ***********************************************************************
!   * Paramètres "de base" : Date, masse et position vitesse dans le RIS  *
!   ***********************************************************************

!**** jour de la date en TE :
    str_gen(iveh)%etat(113) = date%jour
!**** secondes de la date en TE :
    str_gen(iveh)%etat(114) = date%sec

!**** date en TE (jj):
    call md_joursec_jourfrac(date,date_frac,code_erreur)
    str_gen(iveh)%etat(106) = date_frac

!**** date en TUC :

    call ps_date_te_tuc (date, date_tuc)
    if (MSP_gen_messages("ps_maj_vecteur_etat")) return

!**** jour de la date en TUC :
    str_gen(iveh)%etat(115) = date_tuc%jour
!**** secondes de la date en TUC :
    str_gen(iveh)%etat(116) = date_tuc%sec

!**** durée de la simulation (s):
    str_gen(iveh)%etat(2) = ypas

    ! Calcul des coordonnees de sortie
    call ps_calculer_pvout(par,date_tuc,date,pole,apla_rep,pvout,tsid_autre_pla)

! On place finalement les positions vitesses dans le RIS de la date dans le vecteur etat
    str_gen(iveh)%etat(3:5)  = pvout(1:3)
    str_gen(iveh)%etat(9:11) = pvout(4:6)

! - Contrôle que la masse > 0
    call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci,aero=aero)
    if(MSP_gen_messages("ps_maj_vecteur_etat")) return

    call MSP_consulter_mci(mci,mstr=masse_vehicule)
    if(MSP_gen_messages("ps_maj_vecteur_etat")) return
    
    if (masse_vehicule > 0 ) then
       !**** Masse du satellite (kg):
       str_gen(iveh)%etat(41) = masse_vehicule
    else
       call MSP_signaler_message (cle_mes="PS_MASSE_001",routine="ps_maj_vecteur_etat")
       return
    endif

!   *************************************************
!   * Attitude: Attitude dans le RIS
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_att_ris) then 
       call psattit (date,ypas,par(1:3),par(4:6),angle1,angle2,angle3,angri1,angri2,angri3,&
            pgamav,typa,irepa,choix_loi_atti)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
! Matrice de passage du repère de sortie au repère de l'attitude  :
       call mu_transpose3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3), mat_tmp,code_retour_local)
       call mu_matmul3(pgamav,mat_tmp,Mat_ROUT_ATT,code_retour_local)

! Calcul des angles dans le RIS
       call ps_calcul_anglesRIS(typa, Mat_ROUT_ATT, angle1, angle2, angle3)
       if(MSP_gen_messages("ps_maj_vecteur_etat")) return

    endif

!   *************************************************
!   * Positions et vitesses dans le RTS 
!   *************************************************

!**** position, vitesse par rapport au repère planétographique date courante ("Greenwich" pour la Terre)
    if ( str_gen(iveh)%maj_vect_etat%calcul_car_rts) then
       call ps_init_rep_integration_opt2(str_gen(iveh)%planet, par,             &
         date_tuc, apla_rep, teta_planeto_green,&
         tsid_autre_pla, Pplan_green, pvs, pole)
       if(MSP_gen_messages("ps_maj_vecteur_etat") ) return

!!! Essai nouvelle manière de sortie dans RTS

       str_gen(iveh)%etat(49)=pvs(1)
       str_gen(iveh)%etat(50)=pvs(2)
       str_gen(iveh)%etat(51)=pvs(3)
       str_gen(iveh)%etat(52)=pvs(4)
       str_gen(iveh)%etat(53)=pvs(5)
       str_gen(iveh)%etat(54)=pvs(6)

!*** vitesse relative en x,y,z  dans le repère planétographique date courante ("Greenwich" pour la Terre):

       vrelpla(1) = par(4) + str_mod(iveh)%vitrotpla*par(2)
       vrelpla(2) = par(5) - str_mod(iveh)%vitrotpla*par(1)
       vrelpla(3) = par(6)

       call mu_mulvect3(Pplan_green,vrelpla,vrelgreen,code_retour_local)
       str_gen(iveh)%etat(55) = vrelgreen(1)
       str_gen(iveh)%etat(56) = vrelgreen(2)
       str_gen(iveh)%etat(57) = vrelgreen(3)
    endif

!   *************************************************
!   * Attitude dans le RTS  
!   *************************************************

!!! FA205 : Essai nouvelle manière de sortie de l'attitude dans RTS
! Matrice de passage du RTS au repère de l'attitude  :

    if ( str_gen(iveh)%maj_vect_etat%calcul_att_rts ) then

       call mu_transpose3(Pplan_green,mat_tmp,code_retour_local)
       call mu_matmul3(pgamav,mat_tmp,Mat_RTS_ATT,code_retour_local)

       call ps_calcul_anglesRTS(typa, Mat_RTS_ATT)
       if(MSP_gen_messages("ps_maj_vecteur_etat") ) return

    endif


!   *************************************************
!   * Calculs intérmédiaires
!   *************************************************

    ! Dans les cas où les variables demandées concernent les forces
    ! ou les positions géodésiques, il faut calculer une fois pour toutes
    ! la position du soleil
    if ( str_gen(iveh)%maj_vect_etat%calcul_pos_geo &
         .or. str_gen(iveh)%maj_vect_etat%calcul_forces &
         .or. str_gen(iveh)%maj_vect_etat%calcul_autres &
         .or. str_gen(iveh)%maj_vect_etat%calcul_kep ) then
!**** Calcul de la position de la lune ou du soleil dans le repère ME2000:
       call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_SOLEIL,&
            pos_soleil,mu_soleil)
	  	    
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
    endif


!**** Calcul des altitudes : altitude Terre ronde et Terre applatie : 
!     approximations suffisantes permettant d'éviter les calculs de pos. géodésiques
    if ( str_gen(iveh)%maj_vect_etat%calcul_kep  &
       .or. str_gen(iveh)%maj_vect_etat%calcul_kep_moy ) then

       ! Distance au centre de la planète (m) calculé dans le RIS 
       call mt_car_geoc (pvout(1:3), corgeoc, code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_maj_vecteur_etat" )) return
       end if

       ! Rayon de la terre a cette latitude (km)
       sinlat = par(3)/corgeoc%dist
       rayter = str_mod(iveh)%requa*(1-sinlat**2/str_mod(iveh)%apla)
       ralt_pla_apla = (corgeoc%dist - rayter) / 1000._pm_reel

    endif

!   * Temps Sideral
    if ( str_gen(iveh)%maj_vect_etat%calcul_pos_geo  &
       .or. str_gen(iveh)%maj_vect_etat%calcul_kep ) then
       if ( str_gen(iveh)%planet == eph_terre ) then
     	  
	  if ( PS_MODOUT == 3 ) then
	     ! RIS = Gamma vrai de la date
	     call MSP_calcul_ecart_echt(date_tuc, pm_TUC, ecart_tu1=delta_tu1, &
                                        ecart_tai=delta_tai)
             if ( MSP_gen_messages ("ps_maj_vecteur_etat")) return
     
	     call mr_tsid_vrai (pm_lieske_wahr, date_tuc, delta_tu1, delta_tai, tsid, code_erreur)
	                  
	     if (code_erreur%valeur < 0) then
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (MSP_gen_messages("ps_maj_vecteur_etat" )) return
             endif
	  
	  else
	  
	     ! RIS != Gamma vrai de la date
	     
	     ! Le temps sidéral est ici calculé en tenant compte du saut du TUC 
	     call mr_tsid_veis (date_tuc, 0._pm_reel, tsid, code_erreur)
             if (code_erreur%valeur < 0) then
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (MSP_gen_messages("ps_maj_vecteur_etat" )) return
             end if    
	          
	  endif
	  
       else
          ! Cas Mars ou Venus
          tsid = tsid_autre_pla
       endif
    endif

!   ***************************************************
!   * Positions et éventuellement vitesses géodésiques 
!   ***************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_pos_geo ) then

!**** calcule du vecteur position dans le repère planétocentrique 
       call ps_rep_planeto_centrique(pvs, par, &
            ralt, rlat, rlon, vit_meca_vol)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return 

       ! Recopie du temps sidéral calculé
       str_gen(iveh)%etat(76) = tsid*raddeg

       if ( str_gen(iveh)%maj_vect_etat%calcul_vit_geo ) then
!**** calcule du vecteur vitesse dans le repère planétocentrique 
          call ps_calcul_vit_geo(pvout, vit_meca_vol, vrelgam)

          ! Recopie de la vitesse relative dans le RIS
          if ( str_gen(iveh)%maj_vect_etat%calcul_autres ) then
             str_gen(iveh)%etat(15:17) = vrelgam(1:3)
          endif
       endif  

    endif ! calcul_pos_geo


!   *************************************************
!   *  Parametres orbitaux (au dessus de 40 km d'altitude)
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_kep ) then
!     Attention les résultats sont données dans Gamma50 CNES (VEIS de la date) pour la Terre, 
!     sinon dans le repère d'integration:
       call ps_parametres_orbitaux(tsid, pos_soleil,  &
            pvout, parbid, parorb, ralt_pla_apla, &
            flag_w_parorb, flag_w_anome)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return
    endif

!   *************************************************
!   *  Accelerations du au second membre
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_forces ) then
       call psi_contribution_forces(date, irepa, par, mu_soleil, &
            pos_soleil, masse_vehicule, mci, aero, pgamav)
       if(MSP_gen_messages("ps_maj_vecteur_etat")) return
    endif

!   *************************************************
!   * Poussee, debit et direction de poussee des moteurs
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_force_prop ) then
       call ps_poussee_moteur(ypas,Mat_ROUT_ATT)
       if(MSP_gen_messages("ps_maj_vecteur_etat")) return
    endif


!   *************************************************
!   * parametres orbitaux moyens du modele d'ECKSTEIN-HECHLER:
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_kep_moy ) then
       call ps_parametres_EH(ralt_pla_apla, parorb, flag_w_parmoy)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return
    endif

!   *************************************************
!   * Variables supplémentaires
!   *************************************************
    if ( str_gen(iveh)%maj_vect_etat%calcul_autres ) then
!****  angle beta entre la direction du soleil et le plan de l'orbite:
       call ps_angle_beta(ralt, pos_soleil, dir_soleil, par)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return
!****  angle de rotation des panneaux solaires en mode Yaw-Steering:
       call ps_rotation_yawsteering(irepa, pgamav, dir_soleil)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return
    endif

    ! Effacement des structures MECASPA
    call MSP_effacer_aero(aero)

  end subroutine ps_maj_vecteur_etat



  subroutine ps_calculer_pvout(par,date_tuc,date,pole,apla_rep,pvout,tsid_autre_pla)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calculer_pvout
!
!$Resume
!  Calcule le vecteur position/vitesse du satellite dans le repere de sortie
!
!$Description
!  Calcule le vecteur position/vitesse du satellite dans le repere de sortie
!
!$Auteur
! Camille HUE (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_calculer_pvout(par,date_tuc,date,pole,apla_rep,pvout,tsid_autre_pla)
!.    real (KIND=pm_reel), dimension(6) :: par
!.    type (tm_jour_sec) :: date_tuc
!.    type (tm_jour_sec) :: date
!.    type (tm_pole_uv) :: pole
!.    real (kind=pm_reel) :: apla_rep
!.    real (KIND=pm_reel), dimension(6) :: pvout
!.    real (kind=pm_reel) :: tsid_autre_pla
!
!$Arguments
!>E     par             :<pm_reel,DIM=(6)>   Vecteur position/vitesse du satellite dans le RI
!Vecteur position/vitesse du satellite dans le RI
!>E     date_tuc        :<tm_jour_sec>       Date en TUC
!Date en TUC
!>E     date            :<tm_jour_sec>       Date
!Date
!>S     pole            :<tm_pole_uv>        Structure des poles u et v
!Structure des poles u et v
!>S     apla_rep        :<pm_reel>           Aplatissement de la planete
!Aplatissement de la planete
!>S     pvout           :<pm_reel,DIM=(6)>   Vecteur position/vitesse dans le repere de sortie
!Vecteur position/vitesse dans le repere de sortie
!>S     tsid_autre_pla  :<pm_reel>           heure solaire (cas Mars et Venus), 0 pour la Terre
!heure solaire (cas Mars et Venus), 0 pour la Terre
!
!$Arguments
!>E     par             :<pm_reel,DIM=(6)>   Vecteur position/vitesse du satellite dans le RI
!Vecteur position/vitesse du satellite dans le RI
!>E     date_tuc        :<tm_jour_sec>       Date en TUC
!Date en TUC
!>E     date            :<tm_jour_sec>       Date
!Date
!>S     pole            :<tm_pole_uv>        Structure des poles u et v
!Structure des poles u et v
!>S     apla_rep        :<pm_reel>           Aplatissement de la planete
!Aplatissement de la planete
!>S     pvout           :<pm_reel,DIM=(6)>   Vecteur position/vitesse dans le repere de sortie
!Vecteur position/vitesse dans le repere de sortie
!>S     tsid_autre_pla  :<pm_reel>           heure solaire (cas Mars et Venus), 0 pour la Terre
!heure solaire (cas Mars et Venus), 0 pour la Terre
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_conv_typrep
!- mu_angle2
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_caracteristiques
!- ps_integration_don
!- ps_modeles
!- ps_attitude
!- ps_propulsion_don
!- ps_ecriture
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
    use ps_caracteristiques
    use ps_integration_don
    use ps_modeles
    use ps_attitude
    use ps_propulsion_don
    use ps_ecriture

    implicit none

    ! Arguments
    !==========
    real (KIND=pm_reel), dimension(6), intent(IN)  :: par
    type (tm_jour_sec), intent(IN)                 :: date_tuc
    type (tm_jour_sec), intent(IN)                 :: date
    type (tm_pole_uv), intent(OUT)                 :: pole
    real (kind=pm_reel), intent(OUT)               :: apla_rep
    real (KIND=pm_reel), dimension(6), intent(OUT) :: pvout
    real (kind=pm_reel), intent(OUT)               :: tsid_autre_pla

    ! Variables locales
    !==================
    integer            :: ii,jj
    type (tm_jour_sec) :: date_rep_in
    type (tm_code_retour) :: code_erreur
    real (kind=pm_reel), dimension(6) :: parbide, parbids
    real (kind=pm_reel), dimension(10) :: repPQ, repPlanetoVrai
    real (kind=pm_reel), dimension(6,6) :: rdxdpPlaEquaUAI, Mat_RI_tmp
    real (KIND=pm_reel), dimension(10) :: rep_g50, rep_gVrai
    type (tm_jour_sec) :: date_J2000

    rep_g50(:) = 0._pm_reel
    rep_gVrai(:) = 0._pm_reel

!     Choix du type de repère de sortie (Terre uniquement entre Gamma 50 CNES et J2000)
!     Cas mars et Venus il faut calculer la matrice de passage du repère d'intégration vers
!     le repère de sortie qui est l'Equatorial Planetocentrique UAI à la date courante
!     ou l'EME2000

! Initialisation de la variable pole pour appel MSP_conv_typrep
    pole%u = str_bul(iveh)%polu
    pole%v = str_bul(iveh)%polv

! Test de la valeur de apla_r avant passage de 1/apla_r
    if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
       apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
    else
       apla_rep = 0._PM_REEL
    endif

    if ( str_gen(iveh)%planet == eph_terre ) then

       if ( PS_MODOUT == 1 ) then
       
          ! Repère Gamma 50 CNES
	  
          call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
          ! date du repère d'entrée
          date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

      ! FA 202 : une matrice constante ne permet d'avoir le VEIS de la date, mais le VEIS
      ! de la date initiale, il faut donc la recalculer a chaque pas
      ! 20% de degradation de perf.

          call MSP_conv_typrep (par,&
               int(str_int(iveh)%reps(1)),str_int(iveh)%echds,str_int(iveh)%num_plas, &
               date_rep_in, MSP_ENUM_ECHT_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               MSP_ENUM_VEIS, MSP_ENUM_ECHD_DATE_REF, eph_terre,&
               date_tuc, MSP_ENUM_ECHT_TUC, &
               0._pm_reel, rep_g50(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               pvout, mat_jacob=str_int(iveh)%Mat_RI_ROUT(1:6,1:6), obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)
	       
       else if ( PS_MODOUT == 2) then
      !**** Positions-vitesses: dans le cas de la Terre, on les exprime dans EME2000
      
          ! Repère J2000
	 
          do jj=1,6
             do ii=1,6
                str_int(iveh)%Mat_RI_ROUT(ii,jj) = str_int(iveh)%Mat_RI_ME2000(ii,jj)
             enddo
          enddo
          pvout(1:6) = matmul (str_int(iveh)%Mat_RI_ROUT,par(1:6))
       
       else 
       
       ! Repère Gamma vrai de la date
                     
          call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
          ! date du repère d'entrée
          date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc


          call MSP_conv_typrep (par,&
               int(str_int(iveh)%reps(1)),str_int(iveh)%echds,str_int(iveh)%num_plas, &
               date_rep_in, MSP_ENUM_ECHT_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               pm_equa_vrai, MSP_ENUM_ECHD_DATE_REF, eph_terre,&
               date_tuc, MSP_ENUM_ECHT_TUC, &
               0._pm_reel, rep_gVrai(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               pvout, mat_jacob=str_int(iveh)%Mat_RI_ROUT(1:6,1:6), obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)

       endif


    else if (str_gen(iveh)%planet == eph_mars) then
       ! Repere planétocentrique UAI
       ! On est dans le cas mars
       ! La matrice de passage doit donc être recalculée à chaque fois.
       parbide(1) = 1._pm_reel
       ! Le RIS est le equatorial planetocentrique de la date de sortie
       repPQ(1)=real(pm_equa_uai,kind=pm_reel)
       repPQ(2:10) = 0._pm_reel

       ! DM 233 : *** appel 'A' ***
       ! L'optimisation suivante a été testée :
       ! chgt rep de UAI(t0) -> j2000 puis j2000 -> UAI(t)
       ! et obtention de la matrice MAT_RI_OUT par composition
       ! des deux matrices. 
       ! => gain en temps non mesurable, et différences numériques (environ 10e-10)
       ! sur des scénarios "courts" (cas 28)
       ! => le code précédent a été remis en place

       ! Matrice de passage du repere d'intégration vers le repère 
       ! Equatorial planétaire UAI de la date courante (de l'ephemeride)
       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       ! date du repère d'entrée
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

       ! Type de sorties habituel
       ! RIS : repère équatorial planétaire UAI déf. à la date du bulletin
       if ((str_ecr(iveh)%type_de_sortie == 1).and.(PS_MODOUT == 1)) then
          call MSP_conv_typrep (parbide,&
               int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
               date_rep_in , MSP_ENUM_ECHT_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               int(repPQ(1)), pm_autre_date, eph_mars,&
               date + str_bul(iveh)%ecart_te_tuc, MSP_ENUM_ECHT_TUC,&
               repPQ(3), repPQ(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               parbids, mat_jacob=rdxdpPlaEquaUAI, obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)

          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          

          do jj=1,6
             do ii=1,6
                str_int(iveh)%Mat_RI_ROUT(ii,jj) = rdxdpPlaEquaUAI(ii,jj)
             enddo
          enddo

       else
          ! Type de sorties SORTILEGE
          ! RIS : EME2000
          date_J2000%jour = 18262
          date_J2000%sec = 43200
          call MSP_conv_typrep (parbide,&
               int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
               date_rep_in , pm_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               pm_equa_moy, pm_1janvier2000_12h00, eph_terre,&
               date_J2000, pm_TE,&
               0._pm_reel, repPQ(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               parbids, mat_jacob=str_int(iveh)%Mat_RI_ROUT(1:6,1:6), obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)

          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          

       end if

       repPlanetoVrai(1) = real(MSP_ENUM_PLANETO_REF_INER)
       repPlanetoVrai(2:10) = 0._pm_reel
       
       ! On a aussi besoin de connaitre le temps sideral à la date courante
       ! On calcule la matrice de passage entre le Equa Pla UAI et le Planetocentrique
       ! inertiel de la date courante
       parbide(:) = 0._pm_reel
       parbide(1) = 1._pm_reel

       ! Initialisation de la variable pole pour appel MSP_conv_typrep
       pole%u = str_bul(iveh)%polu
       pole%v = str_bul(iveh)%polv


       ! DM 233 : *** appel 'B' ***
       ! Calcul temps sidéral : pas encore disponible dans la MSLIB, on garde donc le 
       ! chgt de repere précédent, qui permet de le calculer...

       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       ! date du repère d'entrée
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

       call MSP_conv_typrep (parbide,&
            int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
            date_rep_in , MSP_ENUM_ECHT_TUC, &
            str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            int(repPlanetoVrai(1)), pm_autre_date, eph_mars,&
            date + str_bul(iveh)%ecart_te_tuc, MSP_ENUM_ECHT_TUC,&
            repPlanetoVrai(3), repPlanetoVrai(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            str_bul(iveh)%vitrot, &
            parbids, mat_jacob=Mat_RI_tmp(1:6,1:6), obli=str_bul(iveh)%obli, &
            pole_in=pole, pole_out=pole)


       call mu_angle2(parbids(1), parbids(2), tsid_autre_pla, code_erreur)
   ! "parbids" est le X du Equa Pla exprime dans le Planeto de reference
   ! Le temps sideral est donc égal à 2 PI - l'angle calculé

       tsid_autre_pla = pm_deux_pi - tsid_autre_pla

       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          
       end if

   !**** Positions-vitesses: dans le cas de Mars, on les exprime egalement dans le Planetocentrique
   !                         Equatorial de la date de l'ephemeride ou dans l'EME2000
       pvout(1:6) = matmul (str_int(iveh)%Mat_RI_ROUT,par(1:6))

   else if (str_gen(iveh)%planet == eph_venus) then
       ! Repere planétocentrique UAI
       ! On est dans le cas venus
       ! La matrice de passage doit donc être recalculée à chaque fois.
       parbide(1) = 1._pm_reel
       ! Le RIS est le equatorial planetocentrique de la date de sortie
       repPQ(1)=real(pm_equa_uai,kind=pm_reel)
       repPQ(2:10) = 0._pm_reel

       ! Matrice de passage du repere d'intégration vers le repère 
       ! Equatorial planétaire UAI de la date courante (de l'ephemeride)
       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       ! date du repère d'entrée
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

       ! Type de sorties habituel
       ! RIS : repère équatorial planétaire UAI déf. à la date du bulletin
       if ((str_ecr(iveh)%type_de_sortie == 1).and.(PS_MODOUT == 1)) then
          call MSP_conv_typrep (parbide,&
               int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
               date_rep_in , MSP_ENUM_ECHT_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               int(repPQ(1)), pm_autre_date, eph_venus,&
               date + str_bul(iveh)%ecart_te_tuc, MSP_ENUM_ECHT_TUC,&
               repPQ(3), repPQ(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               parbids, mat_jacob=rdxdpPlaEquaUAI, obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)

          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          

          str_int(iveh)%Mat_RI_ROUT(1:6,1:6) = rdxdpPlaEquaUAI(1:6,1:6)

       else
          ! Type de sorties SORTILEGE
          ! RIS : EME2000
          date_J2000%jour = 18262
          date_J2000%sec = 43200
          call MSP_conv_typrep (parbide,&
               int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
               date_rep_in , pm_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               pm_equa_moy, pm_1janvier2000_12h00, eph_terre,&
               date_J2000, pm_TE,&
               0._pm_reel, repPQ(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               parbids, mat_jacob=str_int(iveh)%Mat_RI_ROUT(1:6,1:6), obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)

          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          

       end if

       repPlanetoVrai(1) = real(MSP_ENUM_PLANETO_REF_INER)
       repPlanetoVrai(2:10) = 0._pm_reel
       
       ! On a aussi besoin de connaitre le temps sideral à la date courante
       ! On calcule la matrice de passage entre le Equa Pla UAI et le Planetocentrique
       ! inertiel de la date courante
       parbide(:) = 0._pm_reel
       parbide(1) = 1._pm_reel

       ! Initialisation de la variable pole pour appel MSP_conv_typrep
       pole%u = str_bul(iveh)%polu
       pole%v = str_bul(iveh)%polv


       ! DM 233 : *** appel 'B' ***
       ! Calcul temps sidéral : pas encore disponible dans la MSLIB, on garde donc le 
       ! chgt de repere précédent, qui permet de le calculer...

       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       ! date du repère d'entrée
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

       call MSP_conv_typrep (parbide,&
            int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
            date_rep_in , MSP_ENUM_ECHT_TUC, &
            str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            int(repPlanetoVrai(1)), pm_autre_date, eph_venus,&
            date + str_bul(iveh)%ecart_te_tuc, MSP_ENUM_ECHT_TUC,&
            repPlanetoVrai(3), repPlanetoVrai(2:10), &
            str_bul(iveh)%requa_r, apla_rep, &
            str_bul(iveh)%vitrot, &
            parbids, mat_jacob=Mat_RI_tmp(1:6,1:6), obli=str_bul(iveh)%obli, &
            pole_in=pole, pole_out=pole)


       call mu_angle2(parbids(1), parbids(2), tsid_autre_pla, code_erreur)
   ! "parbids" est le X du Equa Pla exprime dans le Planeto de reference
   ! Le temps sideral est donc égal à 2 PI - l'angle calculé

       tsid_autre_pla = pm_deux_pi - tsid_autre_pla

       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return          
       end if

   !**** Positions-vitesses: dans le cas de Venus, on les exprime egalement dans le Planetocentrique
   !                         Equatorial de la date de l'ephemeride ou dans le EME2000
       pvout(1:6) = matmul (str_int(iveh)%Mat_RI_ROUT,par(1:6))

    endif

  end subroutine ps_calculer_pvout


  subroutine ps_date_te_tuc(date, date_tuc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_date_te_tuc
!
!$Resume
!  permet de passer de la date TE "date" à la date TUC "date_tuc"
!
!$Description
!   permet de passer de la date TE "date" à la date TUC "date_tuc"
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_date_te_tuc(date, date_tuc)
!.    type(tm_jour_sec) :: date
!.    type(tm_jour_sec) :: date_tuc
!
!$Arguments
!>E     date      :<tm_jour_sec>   Date courante
!>S     date_tuc  :<tm_jour_sec>   Date TUC
!
!$Common
!
!$Routines
!- MSP_date_etutc
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
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

    use ps_generalites

    implicit none

! Arguments 
! =========
    type(tm_jour_sec), intent(in)           :: date
    type(tm_jour_sec), intent(out)          :: date_tuc

! Conversion de date TE en date TUC
    call MSP_date_etutc(date,1,date_tuc)
    if ( MSP_ERREUR) then
       call MSP_signaler_message (cle_mes="MSP_MSPRO",&
            partie_variable="MSP_date_etutc_mspro", &
            routine="ps_date_te_tuc",type=MSP_ENUM_WARNING)
       str_gen(iveh)%etat(1) = 0._pm_reel
    else
       str_gen(iveh)%etat(1) = (date_tuc%jour+date_tuc%sec/86400._pm_reel)
    endif

  end subroutine ps_date_te_tuc


  subroutine ps_init_rep_integration_opt2(planete, par,                                           &
       date_tuc, apla_rep, teta_green, tsid_autre_pla, &
       Pplan_green, pvs, pole)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_rep_integration_opt2
!
!$Resume
!! changement de repere vers le planeto_ref_iner
! on a le VEIS de la date courante dans str_gen(iveh)%etat(3:5) et str_gen(iveh)%etat(9:11)
! on veut le planeto_ref_iner de la date courante
!
!$Description
! changement de repere vers le planeto_ref_iner
! on a le VEIS de la date courante dans str_gen(iveh)%etat(3:5) et str_gen(iveh)%etat(9:11)
! on veut le planeto_ref_iner de la date courante
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_init_rep_integration_opt2(planete, par,                                           &
!.           date_tuc, apla_rep, teta_green, tsid_autre_pla, &
!.           Pplan_green, pvs, pole)
!.    integer :: planete
!.    real(kind=pm_reel), dimension(6) :: par
!.    type(tm_jour_sec) :: date_tuc
!.    real(kind=pm_reel) :: apla_rep, teta_green, tsid_autre_pla
!.    real(kind=pm_reel), dimension(3,3) :: Pplan_green
!.    real(kind=pm_reel), dimension(6) :: pvs
!.    type(tm_pole_uv) :: pole
!
!$Arguments
!>E     planete         :<integer>             
!>E     par             :<pm_reel,DIM=(6)>     vecteur position/vitesse dans le repère d'intégration (m-m/s) 
!>E     date_tuc        :<tm_jour_sec>         
!>E/S   apla_rep        :<pm_reel>             
!>E/S   teta_green      :<pm_reel>             
!>E/S   tsid_autre_pla  :<pm_reel>             
!>S     Pplan_green     :<pm_reel,DIM=(3,3)>   
!>S     pvs             :<pm_reel,DIM=(6)>     
!>E/S   pole            :<tm_pole_uv>          
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- md_jourfrac_joursec
!- MSP_conv_typrep
!- mu_mulvect3
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_integration_don
!- ps_bulletin
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

    use ps_generalites
    use ps_integration_don
    use ps_bulletin

    implicit none

    ! Arguments 
    ! =========
    integer, intent(in)                       :: planete
    real(kind=pm_reel), dimension(6), intent(in)    :: par
    type(tm_jour_sec), intent(in)                   :: date_tuc
    real(kind=pm_reel), intent(inout)               :: apla_rep, teta_green, tsid_autre_pla
    real(kind=pm_reel), dimension(3,3), intent(out) :: Pplan_green
    real(kind=pm_reel), dimension(6), intent(out)   :: pvs
    type(tm_pole_uv), intent(inout)                 :: pole
    
    ! Variables locales
    ! =================
    real(kind=pm_reel)                              :: cplaneto_green, splaneto_green
    real(kind=pm_reel), dimension(10)               :: rep_rts
    real(kind=pm_reel), dimension(6,6)              :: Pplan_green6
    real(kind=pm_reel)                              :: date_tuc_frac
    type(tm_code_retour)                            :: code_erreur, code_retour_local
    type(tm_jour_sec)                               :: date_rep_in
    
    ! Début du code
    !==============

    call md_joursec_jourfrac(date_tuc,date_tuc_frac,code_erreur)

!! changement de repere vers le planeto_ref_iner
! on a le VEIS de la date courante dans str_gen(iveh)%etat(3:5) et str_gen(iveh)%etat(9:11)
! on veut le planeto_ref_iner de la date courante
! - pas de raccourci evident dans la MSLIB90
! 
   

    if (planete == eph_terre) then
       rep_rts(1)    = real(MSP_ENUM_PLANETO_REF_INER,kind=pm_reel)
       rep_rts(2)  = 0._pm_reel
       rep_rts(3)  = 0._pm_reel
       rep_rts(4)  = date_tuc_frac
       rep_rts(5)  = 0._pm_reel
       rep_rts(6)  = 0._pm_reel
       rep_rts(7)  = 0._pm_reel
       rep_rts(8)  = 0._pm_reel
       rep_rts(9)  = 0._pm_reel
       rep_rts(10) = 0._pm_reel

       call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
       date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

   ! DM 233 : *** appel 'C' ***
   ! Pas d'équivalent immédiat pour effectuer le changement de repere avec la MSLIB,
   ! sans passer par mxrep
       call MSP_conv_typrep (par,                                                    &
            int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
            date_rep_in , MSP_ENUM_ECHT_TUC,   &
            str_int(iveh)%reps(3), str_int(iveh)%reps(2:10),                         &
            str_bul(iveh)%requa_r, apla_rep,                                         &
            MSP_ENUM_PLANETO_REF_INER, MSP_ENUM_ECHD_DATE_REF, eph_terre,       &
            date_tuc, MSP_ENUM_ECHT_TUC, rep_rts(3), rep_rts(2:10),                  &
            str_bul(iveh)%requa_r, apla_rep,                                         &
            str_bul(iveh)%vitrot,                            &
            pvs, mat_jacob=Pplan_green6, obli=str_bul(iveh)%obli, pole_in=pole,      &
            pole_out=pole)
       if ( MSP_gen_messages("ps_init_rep_integration_opt2") ) return 
       Pplan_green = Pplan_green6(1:3,1:3)

    else if ((planete == eph_mars).or.(planete == eph_venus)) then
   ! cas mars ou venus
       teta_green = tsid_autre_pla

       cplaneto_green = cos(teta_green)
       splaneto_green = sin(teta_green)

   ! Construction de la matrice de passage du planéto à Greenwich (Pplan_green)

       Pplan_green(1,1) = cplaneto_green
       Pplan_green(1,2) = splaneto_green
       Pplan_green(1,3) = 0._pm_reel
       Pplan_green(2,1) = -splaneto_green
       Pplan_green(2,2) = cplaneto_green
       Pplan_green(2,3) = 0._pm_reel
       Pplan_green(3,1) = 0._pm_reel
       Pplan_green(3,2) = 0._pm_reel
       Pplan_green(3,3) = 1._pm_reel

       call mu_mulvect3(Pplan_green,par(1:3),pvs(1:3),code_retour_local)
       call mu_mulvect3(Pplan_green,par(4:6),pvs(4:6),code_retour_local)

    endif
  end subroutine ps_init_rep_integration_opt2

  subroutine ps_rep_planeto_centrique (pvs, par, ralt, rlat, rlon, vit_meca_vol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rep_planeto_centrique
!
!$Resume
!  calcule les vecteurs position et vitesse dans le repère planétocentrique
!
!$Description
!  calcule les vecteurs position et vitesse dans le repère planétocentrique
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_rep_planeto_centrique (pvs, par, ralt, rlat, rlon, vit_meca_vol)
!.    real(kind=pm_reel), dimension(6) :: pvs, par
!.    real(kind=pm_reel) :: ralt, rlat, rlon 
!.    type (tm_vit_meca_vol) :: vit_meca_vol 
!
!$Arguments
!>E     pvs           :<pm_reel,DIM=(6)>     
!>E     par           :<pm_reel,DIM=(6)>     vecteur position/vitesse dans le repère d'intégration (m-m/s)
!>S     ralt          :<pm_reel>             
!>S     rlat          :<pm_reel>             
!>S     rlon          :<pm_reel>             
!>S     vit_meca_vol  :<tm_vit_meca_vol>     
!
!$Common
!
!$Routines
!- mt_car_geoc
!- MSP_signaler_message
!- mt_car_meca_vol
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_modeles
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

    use ps_integration_don
    use ps_modeles

    implicit none

! Arguments 
! =========
    real(kind=pm_reel), dimension(6), intent(in)   :: pvs, par
    real(kind=pm_reel), intent(out)                :: ralt, rlat, rlon 
    type (tm_vit_meca_vol), intent(out)            :: vit_meca_vol 

! Variables locales
! =================
    type (tm_geodesique)                           :: corgeod 
    type (tm_geocentrique)                         :: corgeoc
    type (tm_code_retour)                          :: code_erreur
    real (KIND=pm_reel)                            :: sinlat,rayter

    call mt_car_geoc (pvs(1:3), corgeoc, code_erreur)
    if (code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_rep_planeto_centrique" )) return
    end if

    str_gen(iveh)%etat(6)  = corgeoc%dist / 1000._pm_reel
    str_gen(iveh)%etat(69) = corgeoc%lat * raddeg

!**** latitude et longitude dans le repère planétographique date courante ("Greenwich" pour la Terre):
    ! on utilise mt_car_meca_vol pour obtenir 1/vit_meca_vol : la vitesse en coord. mécanique de vol
    !                                         2/corgeod : la position en coord. géodésique (lat,long,alt)
    call mt_car_meca_vol (str_mod(iveh)%requa,1._pm_reel/str_mod(iveh)%apla,pvs(1:3),&
         pvs(4:6),corgeod,vit_meca_vol,code_erreur)
    if (code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_rep_planeto_centrique" )) return
    end if

    ralt = corgeod%haut
    rlat = corgeod%lat
    rlon = corgeod%long

    rlon = mod (rlon, dpi)

    str_gen(iveh)%etat(7) = rlat*raddeg
    str_gen(iveh)%etat(8) = rlon*raddeg

!**** altitude geocentrique terre spherique (km):
    str_gen(iveh)%etat(27) = str_gen(iveh)%etat(6) - str_mod(iveh)%requa/1000._pm_reel

!**** altitude geocentrique terre aplatie (km):
    sinlat = par(3)/(str_gen(iveh)%etat(6)*1000._pm_reel)
    ! Rayon de la Terre à cette latitude (m)
    rayter = str_mod(iveh)%requa*(1-sinlat**2/str_mod(iveh)%apla)
    str_gen(iveh)%etat(28) = str_gen(iveh)%etat(6) - rayter/1000._pm_reel

!**** altitude geodesique (km):
    str_gen(iveh)%etat(29) = ralt/1000._pm_reel

  end subroutine ps_rep_planeto_centrique


  subroutine ps_calcul_vit_geo(pvout, vit_meca_vol, vrelgam)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_vit_geo
!
!$Resume
!  Calcul des composantes de la vitesse dans le repère géodésique
!
!$Description
!  Calcul des composantes de la vitesse dans le repère géodésique
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_vit_geo(pvout, vit_meca_vol, vrelgam)
!.    real(kind=pm_reel), dimension(6) :: pvout
!.    type (tm_vit_meca_vol) :: vit_meca_vol
!.    real(kind=pm_reel), dimension(3) :: vrelgam
!
!$Arguments
!>E     pvout         :<pm_reel,DIM=(6)>   Position vitesse dans le RIS (m, m/s)
!>E     vit_meca_vol  :<tm_vit_meca_vol>   Structures MSLIB de mecanique de vol
!>S     vrelgam       :<pm_reel,DIM=(3)>   Vitesse relative dans le RIS
!
!$Common
!
!$Routines
!- mt_car_meca_vol
!- MSP_signaler_message
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ps_integration_don
    use ps_modeles
  
    implicit none

    ! Entrée sorties
    real(kind=pm_reel), dimension(6), intent(in)  :: pvout
    type (tm_vit_meca_vol), intent(in)            :: vit_meca_vol
    real(kind=pm_reel), dimension(3), intent(out) :: vrelgam

    ! Variables locales
    real(kind=pm_reel), dimension(6) :: parrel
    type (tm_code_retour)            :: code_erreur
    type (tm_geodesique)             :: corgeod 
    type (tm_vit_meca_vol)           :: vit_meca_vol_apla_nul
    
!**** norme, pente et azimut de la vitesse absolue (m/s,deg,deg):
    str_gen(iveh)%etat(12) = vit_meca_vol%norme
    str_gen(iveh)%etat(13) = vit_meca_vol%pente * raddeg
    str_gen(iveh)%etat(14) = vit_meca_vol%azimut * raddeg

!**** vitesse relative en x,y,z dans le repère inertiel de sortie.
    vrelgam(1) = pvout(4) + str_mod(iveh)%vitrotpla*pvout(2)
    vrelgam(2) = pvout(5) - str_mod(iveh)%vitrotpla*pvout(1)
    vrelgam(3) = pvout(6)

!**** norme, pente et azimut de la vitesse relative (m/s,deg,deg):
    parrel(1) = pvout(1)
    parrel(2) = pvout(2)
    parrel(3) = pvout(3)
    parrel(4) = vrelgam(1)
    parrel(5) = vrelgam(2)
    parrel(6) = vrelgam(3)

!     on passe un aplatissement nul pour obtenir les memes resultats qu'avec 
!     IO_p_cafv
    call mt_car_meca_vol (str_mod(iveh)%requa, 0._pm_reel, &
         parrel(1:3), parrel(4:6), corgeod, vit_meca_vol_apla_nul, &
         code_erreur)
!     afin d'eviter de sortir un warning inutile sur l'applatissement nul
    if ( code_erreur%valeur /= pm_warn_apla_nul .and. code_erreur%valeur /= pm_OK ) then
       call MSP_signaler_message (ier_mslib=code_erreur)
    endif

    if (MSP_gen_messages("ps_rep_planeto_centrique" )) return

    ! Recopie et conversion
    str_gen(iveh)%etat(18) = vit_meca_vol_apla_nul%norme
    str_gen(iveh)%etat(19) = vit_meca_vol_apla_nul%pente * raddeg
    str_gen(iveh)%etat(20) = vit_meca_vol_apla_nul%azimut * raddeg


  end subroutine ps_calcul_vit_geo




  subroutine ps_parametres_orbitaux (tsid, pos_soleil, &
       pvout, parbid, parorb, ralt_pla_apla, &
       flag_w_parorb, flag_w_anome)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_parametres_orbitaux
!
!$Resume
!  calcul des paramètres orbitaux
!
!$Description
!  calcul des paramètres orbitaux
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_parametres_orbitaux (tsid, pos_soleil, &
!.           pvout, parbid, parorb, ralt_pla_apla, &
!.           flag_w_parorb, flag_w_anome)
!.    real(kind=pm_reel) :: tsid
!.    real(kind=pm_reel), dimension(3) :: pos_soleil
!.    logical, dimension(PS_NVMAX) :: flag_w_parorb, flag_w_anome
!.    real(kind=pm_reel), dimension(6) :: pvout
!.    real(kind=pm_reel) :: ralt_pla_apla
!.    real(kind=pm_reel), dimension(6) :: parbid, parorb
!
!$Arguments
!>E     tsid           :<pm_reel>                  
!>E     pos_soleil     :<pm_reel,DIM=(3)>          
!>E     pvout          :<pm_reel,DIM=(6)>          
!>S     parbid         :<pm_reel,DIM=(6)>          
!>S     parorb         :<pm_reel,DIM=(6)>          
!>E     ralt_pla_apla  :<pm_reel>                  Altitude calculé sur une terre applatie (km)
!>E/S   flag_w_parorb  :<logical,DIM=(PS_NVMAX)>   
!>E/S   flag_w_anome   :<logical,DIM=(PS_NVMAX)>   
!
!$Common
!
!$Routines
!- mv_car_cir
!- ps_car_kep_equatorial
!- MSP_annuler_probleme
!- MSP_signaler_message
!- mv_car_kep
!- mv_kepler_gene
!- mv_kepler_bar
!- md_jourfrac_joursec
!- MSP_calcul_ecart_echt
!- mr_tsid_vrai
!- mr_tsid_veis
!- mu_mulvecttrans3
!- mu_mulvect3
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_modeles
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

    use ps_integration_don
    use ps_modeles

    implicit none


! Arguments
! =========
    real(kind=pm_reel), intent(in)                  :: tsid
    real(kind=pm_reel), dimension(3), intent(in)    :: pos_soleil
    logical, dimension(PS_NVMAX), intent(inout)     :: flag_w_parorb, flag_w_anome
    real(kind=pm_reel), dimension(6), intent(in)    :: pvout
    real(kind=pm_reel), intent(in)                  :: ralt_pla_apla
    real(kind=pm_reel), dimension(6), intent(out)   :: parbid, parorb


! Variables locales
! =================
    real(kind=pm_reel)                              :: tsideral
    real(kind=pm_reel)                              :: ex,ey,xlm,xle,ane,cane,denom
    real(kind=pm_reel)                              :: xlonmoy,hlna,anv,cosv, sinv
    real(kind=pm_reel), dimension(3)                :: vecs_RI, vecs_ROUT
    type (tm_orb_cir)                               :: orb_cir
    type (tm_orb_kep)                               :: orb_kep
    type (tm_code_retour)                           :: code_erreur, code_retour_local
    type (tm_jour_sec)                              :: jul1950

    ! Initialisation de anv
    anv = 0._pm_reel
    xlonmoy = 0._pm_reel

    ! An dessous de 40 km on ne calcule pas les paramètres
    if ( ralt_pla_apla > 40._pm_reel ) then
       if ( str_gen(iveh)%hyp == 0) then

      !           on passe ici dans TEST 01

          call mv_car_cir (str_mod(iveh)%gmu, pvout(1:3), pvout(4:6), &
               orb_cir, code_erreur)

          ! On traitement différemment le cas ou l'orbite est équatoriale,
          ! car dans ce cas la conversion ci-dessus aura levée une erreur
          if (code_erreur%valeur == pm_err_i_equa) then
             
             call ps_car_kep_equatorial(pvout,parbid)
             if (MSP_gen_messages("ps_parametres_orbitaux" )) return
             
          elseif(code_erreur%valeur == pm_ok) then
             ! Cas standard ou l'orbite n'est pas equatoriale

             !           afin d'eviter de sortir un warning repetitif
             if ( code_erreur%valeur /= pm_OK ) then
                if ( .not. flag_w_parorb(iveh)) then
                   call MSP_annuler_probleme ()
                   flag_w_parorb (iveh) = .true.
                endif
             endif
             
             parbid(1) = orb_cir%a
             parbid(2) = orb_cir%ex
             parbid(3) = orb_cir%ey
             parbid(4) = orb_cir%i
             parbid(5) = orb_cir%gom
             parbid(6) = orb_cir%pso_M
          else
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("ps_parametres_orbitaux" )) return
          end if
       

       else
      !**** parametres orbitaux normaux (cas hyperbolique):
      !           on passe ici dans TEST 35

          call mv_car_kep (str_mod(iveh)%gmu, pvout(1:3), pvout(4:6), &
               orb_kep, code_erreur)
          if (code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("ps_parametres_orbitaux" )) return
          end if

      !           afin d'eviter de sortir un warning repetitif
          if ( code_erreur%valeur /= pm_OK ) then
             if ( .not. flag_w_parorb(iveh)) then
                call MSP_annuler_probleme ()
                flag_w_parorb (iveh) = .true.
             endif
          endif

          parbid(1) = orb_kep%a
          parbid(2) = orb_kep%e
          parbid(3) = orb_kep%i
          parbid(4) = orb_kep%pom
          parbid(5) = orb_kep%gom
          parbid(6) = orb_kep%M


       endif
    endif

    ! An dessous de 40 km on ne calcule pas les paramètres
    if ( (ralt_pla_apla > 40._pm_reel) ) then

       if ( str_gen(iveh)%hyp == 0 ) then

          parorb(1) = parbid(1)
          parorb(2) = sqrt(parbid(2)**2+parbid(3)**2)
          if ( parorb(2) < 1.e-12_pm_reel ) then
             parorb(4) = 0._pm_reel
          else
             parorb(4) = atan2 (parbid(3), parbid(2))
          endif

          parorb(3) = parbid(4)
          parorb(5) = parbid(5)
          parorb(6) = parbid(6) - parorb(4)

          if ( parorb(6) < 0._pm_reel ) then
             parorb(6) = parorb(6) + dpi
          else if ( parorb(6) > dpi ) then
             parorb(6) = parorb(6) - dpi
          endif

       else

          parorb(1) = parbid(1)
          parorb(2) = parbid(2)
          parorb(3) = parbid(3)
          parorb(4) = parbid(4)
          parorb(5) = parbid(5)
          parorb(6) = parbid(6)

       endif

       str_gen(iveh)%etat(21) = parorb(1)/1000._pm_reel
       str_gen(iveh)%etat(22) = parorb(2)
       str_gen(iveh)%etat(23) = parorb(3)*raddeg
       str_gen(iveh)%etat(24) = parorb(4)*raddeg
       str_gen(iveh)%etat(25) = parorb(5)*raddeg
       str_gen(iveh)%etat(26) = parorb(6)*raddeg

       if ( str_gen(iveh)%hyp == 0 ) then

          xlm = parorb(4) + parorb(6)
          ex  = parorb(2)*cos(parorb(4))
          ey  = parorb(2)*sin(parorb(4))

      !           on passe ici dans le TEST PLANETE

          call mv_kepler_gene (xlm, ex, ey, xle, code_erreur)
      !           afin d'eviter de sortir un warning repetitif
          if ( code_erreur%valeur /= pm_OK ) then
             if ( .not. flag_w_anome(iveh)) then
                flag_w_anome (iveh) = .true.
             else
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (MSP_gen_messages("ps_parametres_orbitaux" )) return
             endif
          else
             ane = mod((xle-parorb(4)),dpi)
             if ( ane < 0._pm_reel ) ane = ane + dpi
             cane = cos(ane)
             denom = 1._pm_reel - parorb(2)*cane
             cosv = (cane-parorb(2))/denom
             sinv = sin(ane)*sqrt(1._pm_reel-parorb(2)**2)/denom
             anv  = atan2(sinv,cosv)
             if ( anv < 0._pm_reel ) anv = anv + dpi
          endif

       else

      !           on passe ici dans le scenario de test 35

          call mv_kepler_bar (parorb(6), parorb(2), ane, code_erreur)

      !           afin d'eviter de sortir un warning repetitif
          if ( code_erreur%valeur /= pm_OK ) then
             if ( .not. flag_w_anome(iveh)) then
                flag_w_anome(iveh) = .true.
             else
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (MSP_gen_messages("ps_parametres_orbitaux" )) return
             endif
          endif

          anv = MSP_excentrique_to_vraie (ane, parorb(2))

       endif

       str_gen(iveh)%etat(67) = anv*raddeg
       str_gen(iveh)%etat(68) = ane*raddeg

   ! temps sideral a la date du repere
       if ( str_gen(iveh)%planet == eph_terre ) then
          call md_jourfrac_joursec (str_int(iveh)%reps(4), jul1950, code_erreur)
          if (code_erreur%valeur < 0) then
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages("ps_parametres_orbitaux" )) return
          end if

	  if ( PS_MODOUT == 3 ) then
	     ! RIS = Gamma vrai de la date
	     xlonmoy = parorb(4) + parorb(5) + parorb(6) - tsid
	  else	  
	     ! RIS != Gamma vrai de la date
	     call mr_tsid_veis (jul1950, 0._pm_reel, tsideral, code_erreur)
             if (code_erreur%valeur < 0) then
                call MSP_signaler_message (ier_mslib=code_erreur)
                if (MSP_gen_messages("ps_parametres_orbitaux" )) return
             end if    	     
	     xlonmoy = parorb(4) + parorb(5) + parorb(6) - tsid + tsideral	     
	  endif

          
       else if ((str_gen(iveh)%planet == eph_mars).or.(str_gen(iveh)%planet == eph_venus)) then
          xlonmoy = parorb(4) + parorb(5) + parorb(6) - tsid
       endif

       xlonmoy = mod (xlonmoy, dpi)

       if ( xlonmoy < 0._pm_reel ) xlonmoy = xlonmoy + dpi
       str_gen(iveh)%etat(77) = xlonmoy*raddeg

   !****    altitude du perigee et de l'apogee de l'orbite osculatrice:

       str_gen(iveh)%etat(43) = str_gen(iveh)%etat(21)*&
            (1._pm_reel-str_gen(iveh)%etat(22)) - str_mod(iveh)%requa/1000._pm_reel
       str_gen(iveh)%etat(44) = str_gen(iveh)%etat(21)*&
            (1._pm_reel+str_gen(iveh)%etat(22)) - str_mod(iveh)%requa/1000._pm_reel

   !****    Heure locale du noeud ascendant:

   ! Calcul de la position du Soleil dans le ROUT

       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),pos_soleil,vecs_RI,code_retour_local)
       call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),vecs_RI,vecs_ROUT,code_retour_local)

       hlna = parorb(5) - atan2 (vecs_ROUT(2), vecs_ROUT(1))

       if ( hlna > dpi ) then
          hlna = hlna - dpi
       else if ( hlna < 0._pm_reel ) then
          hlna = hlna + dpi
       endif
       str_gen(iveh)%etat(93) = 12._pm_reel + hlna*86400._pm_reel/(3600._pm_reel*dpi)
       if ( str_gen(iveh)%etat(93) > 24._pm_reel ) &
            str_gen(iveh)%etat(93) = str_gen(iveh)%etat(93) - 24._pm_reel

    else

       str_gen(iveh)%etat(21:26) = 0._pm_reel
       str_gen(iveh)%etat(43)    = 0._pm_reel
       str_gen(iveh)%etat(44)    = 0._pm_reel
       str_gen(iveh)%etat(67)    = 0._pm_reel
       str_gen(iveh)%etat(68)    = 0._pm_reel
       str_gen(iveh)%etat(77)    = 0._pm_reel
       str_gen(iveh)%etat(93)    = 0._pm_reel

    end if

  end subroutine ps_parametres_orbitaux

  subroutine ps_car_kep_equatorial(pvin, pvout)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_car_kep_equatorial
!
!$Resume
!  conversion des paramètre cartésiens en paramètre képlériens dans le cas
!  d'une orbite équatoriale
!
!$Description
!  Si l'orbite est equatorial, il faut passer par les paramètre
!  circulaire équatoraux car il y a une indétérmination sur l'ascension droite
!
!$Auteur
!  O Kverneland - Atos Origin
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_car_kep_equatorial(pvin, pvout)
!.    real(kind=pm_reel), dimension(6) :: pvin
!.    real(kind=pm_reel), dimension(6) :: pvout
!
!$Arguments
!>E     pvin   :<pm_reel,DIM=(6)>   : paramètres cartésiens
!>S     pvout  :<pm_reel,DIM=(6)>   : paramètres képlériens
!
!$Common
!
!$Routines
!- mv_car_cir_equa
!- MSP_signaler_message
!- mv_cir_equa_kep
!
!$Include
!
!$Module
!#V
!- ps_modeles
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
     
    use ps_modeles
    
    implicit none
    
    
    ! Arguments
    !==========
    real(kind=pm_reel), dimension(6), intent(in)    :: pvin
    real(kind=pm_reel), dimension(6), intent(out)   :: pvout
    
    ! Variables locales
    !==========
    type(tm_orb_kep)                                :: kep            
    type(tm_orb_cir_equa)                           :: cir_equa
    type (tm_code_retour)                           :: code_erreur
    

    ! Début du code
    !==============

    ! Conversion à avec la MSLIB en paramètres circulaires équatoriaux
    call mv_car_cir_equa(str_mod(iveh)%gmu, pvin(1:3), pvin(4:6), &
         cir_equa, code_erreur)

    ! On ignore les warnings MSLIB90, car la conversion ci-dessus provoquera systématiquement
    ! un warning comme quoi l'orbite est equatoriale (mais ça, si on est ici, on le sait déjà 
    ! car l'objectif de cette fonction est de traiter ce cas particulier) !
    if(code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_parametres_orbitaux" )) return
    endif

    ! On convertit en képlérien avec la MSLIB90.
    call mv_cir_equa_kep(cir_equa, kep, code_erreur)
    
    ! On ignore les warnings toujours pour la même raison
    if(code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_parametres_orbitaux" )) return
    endif
    
    
    pvout(1) = kep%a
    pvout(2) = kep%e
    pvout(3) = kep%i
    pvout(4) = kep%pom
    pvout(5) = kep%gom
    pvout(6) = kep%M


    
  end subroutine ps_car_kep_equatorial



  subroutine ps_calcul_anglesRIS(typa, mat_rout_att, angle1, angle2, angle3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_anglesRIS
!
!$Resume
!  calcul des angles d'attitude psi, theta et phi dans le RIS et le repère local
!
!$Description
!  calcul des angles d'attitude psi, theta et phi dans le RIS et le repère local
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_calcul_anglesRIS(typa, mat_rout_att, angle1, angle2, angle3)
!.    integer :: typa 
!.    real(kind=pm_reel), dimension(3,3) :: Mat_ROUT_ATT
!.    real(kind=pm_reel) :: angle1, angle2, angle3
!
!$Arguments
!>E     typa          :<integer>             
!>E/S   mat_rout_att  :<pm_reel,DIM=(3,3)>   
!>E     angle1        :<pm_reel>             
!>E     angle2        :<pm_reel>             
!>E     angle3        :<pm_reel>             
!
!$Common
!
!$Routines
!- mu_mat_quat
!- MSP_signaler_message
!- mu_quat_3rot
!- mu_3rot_quat
!
!$Include
!
!$Module
!#V
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

    use ps_integration_don

    implicit none

! Arguments
! =========
    integer, intent(in)                               :: typa 
    real(kind=pm_reel), dimension(3,3), intent(inout) :: Mat_ROUT_ATT
    real(kind=pm_reel), intent(in)                    :: angle1, angle2, angle3

! Variables locales
! =================
    real(kind=pm_reel)                                :: angw1, angw2, angw3
    type(tm_quat)                                     :: quaternion
    type(tm_code_retour)                              :: code_erreur


    if ( typa /= -1) then
   !/ si il y a au moins une loi d'attitude
       call mu_mat_quat (Mat_ROUT_ATT, quaternion, code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_calcul_anglesRIS" )) return
       end if

       call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
            code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_calcul_anglesRIS" )) return
       end if
    else
   !/ si il n'y a pas de loi d'attitude
       quaternion%q0 = 1._pm_reel
       quaternion%q123(1:3) = 0._pm_reel
       angw1 = 0._pm_reel
       angw2 = 0._pm_reel
       angw3 = 0._pm_reel
    endif

!     attention, les arguments retournes par mu_quat_3rot ne sont pas dans les 
!     memes intervalles que les valeurs rendues par IO_a_angatt
    angw1 = mod (angw1, dpi)
    angw2 = mod (angw2, dpi)
    angw3 = mod (angw3, dpi)


    str_gen(iveh)%etat(70) = angw1*raddeg
    str_gen(iveh)%etat(71) = angw2*raddeg
    str_gen(iveh)%etat(72) = angw3*raddeg
    str_gen(iveh)%etat(73) = angle1*raddeg
    str_gen(iveh)%etat(74) = angle2*raddeg
    str_gen(iveh)%etat(75) = angle3*raddeg

! Quaternions dans le RIS:

    call mu_3rot_quat (pm_1z_2y_3x, angw1, angw2, angw3, quaternion, code_erreur)
    if (code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_calcul_angleRIS" )) return
    end if

    str_gen(iveh)%etat(102) = quaternion%q0
    str_gen(iveh)%etat(103) = quaternion%q123(1)
    str_gen(iveh)%etat(104) = quaternion%q123(2)
    str_gen(iveh)%etat(105) = quaternion%q123(3)

  end subroutine ps_calcul_anglesRIS



  subroutine ps_calcul_anglesRTS(typa, mat_rts_att)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_anglesRTS
!
!$Resume
!  calcul des angles d'attitudes psi, theta et phi dans RTS
!
!$Description
!  calcul des angles d'attitudes psi, theta et phi dans RTS
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_calcul_anglesRTS(typa, mat_rts_att)
!.    integer :: typa 
!.    real(kind=pm_reel), dimension(3,3) :: Mat_rts_att
!
!$Arguments
!>E     typa         :<integer>             
!>E/S   mat_rts_att  :<pm_reel,DIM=(3,3)>   
!
!$Common
!
!$Routines
!- mu_mat_quat
!- MSP_signaler_message
!- mu_quat_3rot
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- mecaspa
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

    use ps_integration_don
    use mecaspa

    implicit none

! Arguments
! =========
    integer, intent(in)                               :: typa 
    real(kind=pm_reel), dimension(3,3), intent(inout) :: Mat_rts_att

! Variables locales
! =================
    real(kind=pm_reel)                                :: angw1, angw2, angw3
    type(tm_quat)                                     :: quaternion
    type(tm_code_retour)                              :: code_erreur


    if ( typa /= -1) then
   !/ si il y a au moins une loi d'attitude
       call mu_mat_quat (Mat_RTS_ATT, quaternion, code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_calcul_anglesRTS" )) return
       end if

       call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_calcul_anglesRTS" )) return
       end if

       str_gen(iveh)%etat(97) = mod (angw1, dpi)*raddeg
       str_gen(iveh)%etat(98) = mod (angw2, dpi)*raddeg
       str_gen(iveh)%etat(99) = mod (angw3, dpi)*raddeg
    else
   !/ si il n'y a pas de loi d'attitude
       str_gen(iveh)%etat(97) = 0._pm_reel
       str_gen(iveh)%etat(98) = 0._pm_reel
       str_gen(iveh)%etat(99) = 0._pm_reel
    endif

  end subroutine ps_calcul_anglesRTS


  subroutine ps_poussee_moteur(ypas, Mat_ROUT_ATT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_poussee_moteur
!
!$Resume
!  calcul de la poussee, du debit et de la direction de poussee des moteurs
!
!$Description
!  calcul de la poussee, du debit et de la direction de poussee des moteurs
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_poussee_moteur(ypas, Mat_ROUT_ATT)
!.    real (kind=pm_reel) :: ypas
!.    real(kind=pm_reel), dimension(3,3) :: Mat_ROUT_ATT
!
!$Arguments
!>E     ypas          :<pm_reel>             
!>E     Mat_ROUT_ATT  :<pm_reel,DIM=(3,3)>   
!
!$Common
!
!$Routines
!- MSP_positionner_loi_courante
!- MSP_consulter_ptr_loi
!- MSP_consulter_poussee_continue
!- MSP_consulter_impulsion
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- psapous
!
!$Include
!
!$Module
!#V
!- mecaspa
!- ps_propulsion_don
!- ps_caracteristiques
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

    use mecaspa 
    use ps_propulsion_don
    use ps_caracteristiques

    implicit none

! Arguments
! =========
    real (kind=pm_reel), intent(in)                :: ypas
    real(kind=pm_reel), dimension(3,3), intent(in) :: Mat_ROUT_ATT

! Variables locales
! =================
    integer                                        :: j
    integer                                        :: ntab, typloi, npt
    real (kind=pm_reel)                            :: fp,debit,omega,omegap,dt, datetimp
    real (kind=pm_reel)                            :: omtabimp, omptabimp
    real (kind=pm_reel), dimension(3)              :: acc_pro, acc_tot
    real (kind=pm_reel)                            :: xmerg, masse
    real (kind=pm_reel), dimension(:), pointer     :: timp
    real (kind=pm_reel), dimension(:), pointer     :: fptab
    real (kind=pm_reel), dimension(:), pointer     :: omtab
    real (kind=pm_reel), dimension(:), pointer     :: omptab
    real (kind=pm_reel), dimension(:), pointer     :: debtab

    type(msp_poussee_continue), pointer            :: loi_pro
    type(msp_impulsion), pointer                   :: loi_imp
    type(MSP_MCI) :: mci

! Début du code
!==============

    nullify(timp)
    nullify(fptab)
    nullify(omtab)
    nullify(omptab)
    nullify(debtab)
    nullify(loi_pro)
    nullify(loi_imp)
    xmerg = 0._pm_reel
    fp    = 0._pm_reel
    debit = 0._pm_reel
    omega = 0._pm_reel
    omegap= 0._pm_reel

!**** poussee,debit et direction de poussee des moteurs:
    npt = iloi_scenar_pro(iveh)

    if ( (npt >= 1) .and. (npt <= nloi_scenar_pro(iveh)) .and. (nimp(npt) == 1)) then

       call MSP_positionner_loi_courante(scenar_pro(iveh), id=npt)
       if ( MSP_gen_messages("ps_poussee_moteur") ) return

   !/  Extraction du type de la loi
       typloi = MSP_type_loi (scenar_pro(iveh))
       if ( MSP_gen_messages("ps_poussee_moteur") ) return
       if (typloi == MSP_ENUM_LOI_CONT .or. typloi == MSP_ENUM_LOI_IMP) then
          if (typloi == MSP_ENUM_LOI_CONT) then
             call MSP_consulter_ptr_loi (scenar_pro(iveh), loi_cont=loi_pro)
             if ( MSP_gen_messages("ps_poussee_moteur") ) return

             call MSP_consulter_poussee_continue (loi_pro,  ntab=ntab, &
                  dates=timp, &
                  poussee=fptab,  &
                  omega=omtab, omegap=omptab, debit=debtab, merg=xmerg)
             if ( MSP_gen_messages("ps_poussee_moteur") ) return

             lois_p : do j = ntab,1,-1
                if ( ypas .egal. (timp(j)) ) then
                   fp     = fptab(j)
                   debit  = debtab(j)
                   omega  = omtab(j)
                   omegap = omptab(j)
                   exit lois_p
                else if ( ypas > (timp(j))) then
                   dt     = (ypas-timp(j))/&
                        (timp(j+1)-timp(j))
                   fp     = fptab(j) + &
                        (fptab(j+1)-fptab(j))*dt
                   debit  = debtab(j) + &
                        (debtab(j+1)-debtab(j))*dt
                   omega  = omtab(j) + &
                        (omtab(j+1)-omtab(j))*dt
                   omegap = omptab(j) + &
                        (omptab(j+1)-omptab(j))*dt
                   exit lois_p
                end if
             enddo lois_p

          else if (typloi == MSP_ENUM_LOI_IMP) then
             call MSP_consulter_ptr_loi (scenar_pro(iveh), loi_imp=loi_imp)
             if ( MSP_gen_messages("ps_poussee_moteur") ) return

             call MSP_consulter_impulsion (loi_imp, &
                  date=datetimp, &
                  omega=omtabimp, omegap=omptabimp)
             if ( MSP_gen_messages("ps_poussee_moteur") ) return

             if ( ypas .egal. datetimp ) then
                fp     = 0._pm_reel
                debit  = 0._pm_reel
                omega  = omtabimp
                omegap = omptabimp
             else if ( ypas > (datetimp)) then
                dt     = (ypas-datetimp)/datetimp
                fp     = 0._pm_reel
                debit  = 0._pm_reel
                omega  = omtabimp + (-omtabimp)*dt
                omegap = omptabimp + (-omptabimp)*dt
             endif
          endif
       endif
    else
       fp     = 0._pm_reel
       debit  = 0._pm_reel
       omega  = 0._pm_reel
       omegap = 0._pm_reel
    end if

    str_gen(iveh)%etat(42) = xmerg ! FA-ID 730 : masse d'ergols en sortie de PSIMU
    str_gen(iveh)%etat(45) = fp
    str_gen(iveh)%etat(46) = debit
    str_gen(iveh)%etat(47) = omega*raddeg
    str_gen(iveh)%etat(48) = omegap*raddeg

!**** norme de l'accélération propulsive (m/s**2):
    call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)

    call MSP_consulter_mci(mci,mstr=masse)


    if (masse > MSP_EPSILON_APLA) then
       str_gen(iveh)%etat(58) = fp/masse
    end if

!**** norme de l'accélération totale avec propulsion (m/s**2):

! - Calcul du vecteur accélération de la propulsion
!  passage du repère véhicule au repère de sortie
    call psapous(Mat_ROUT_ATT,fp,omega,omegap,acc_pro)

! - Calcul du vecteur accélération totale avec propulsion
    acc_tot(1) = str_gen(iveh)%etat(30) + acc_pro(1)
    acc_tot(2) = str_gen(iveh)%etat(31) + acc_pro(2)
    acc_tot(3) = str_gen(iveh)%etat(32) + acc_pro(3)

    str_gen(iveh)%etat(59) = sqrt(acc_tot(1)**2+acc_tot(2)**2+acc_tot(3)**2)

    if (associated(timp)) deallocate (timp)
    if (associated(fptab)) deallocate (fptab)
    if (associated(omtab)) deallocate (omtab)
    if (associated(omptab)) deallocate (omptab)
    if (associated(debtab)) deallocate (debtab)


  end subroutine ps_poussee_moteur

  subroutine ps_parametres_EH(ralt_pla_apla, parorb, flag_w_parmoy)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_parametres_EH
!
!$Resume
!  calcule les paramètres orbitaux du modèle d'ECKSTEIN-HECHLER
!
!$Description
!  calcule les paramètres orbitaux du modèle d'ECKSTEIN-HECHLER
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_parametres_EH(ralt_pla_apla, parorb, flag_w_parmoy)
!.    real(kind=pm_reel), dimension(6) :: parorb
!.    logical, dimension(PS_NVMAX) :: flag_w_parmoy
!.    real (KIND=pm_reel) :: ralt_pla_apla
!
!$Common
!
!$Routines
!- me_eck_hech_moy
!
!$Include
!
!$Module
!#V
!- ps_modeles
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

    use ps_modeles

    implicit none

! Arguments
! =========
    real(kind=pm_reel), dimension(6), intent(in) :: parorb
    logical, dimension(PS_NVMAX), intent(inout)  :: flag_w_parmoy
    real (KIND=pm_reel), intent(in)              :: ralt_pla_apla

! Variables locales
! =================
    integer                                      :: nzo_tmp
    real (KIND=pm_reel), dimension(6)            :: xcn0
    real (KIND=pm_reel), dimension(6)            :: parmoy
    real (KIND=pm_reel), dimension(3)            :: xeps

    type (tm_orb_cir)                            :: moy
    type (tm_orb_cir)                            :: osc
    type (tm_orb_cir)                            :: ecart_admi
    type(tm_code_retour)                         :: code_erreur

!**** parametres orbitaux moyens du modele d'ECKSTEIN-HECHLER:
    ! An dessous de 40 km on ne calcule pas les paramètres
    if (ralt_pla_apla > 40._pm_reel) then
       if ( str_mod(iveh)%nzo >= 2 ) then

          xcn0(1:6) = 0._pm_reel
          nzo_tmp = min(str_mod(iveh)%nzo,6)
          xcn0(1:nzo_tmp) = str_mod(iveh)%vj(1:nzo_tmp)
          xeps(1) = 1.e-3_pm_reel
          xeps(2) = 1.e-6_pm_reel
          xeps(3) = 1.e-7_pm_reel

          ecart_admi%a     = xeps(1)
          ecart_admi%ex    = xeps(2)
          ecart_admi%ey    = xeps(2)
          ecart_admi%i     = xeps(3)
          ecart_admi%gom   = xeps(3)
          ecart_admi%pso_M = xeps(3)
          osc%a     = parorb(1)
          osc%ex    = parorb(2) * cos (parorb(4))
          osc%ey    = parorb(2) * sin (parorb(4))
          osc%i     = parorb(3)
          osc%gom   = parorb(5)
          osc%pso_M = parorb(4) + parorb(6)   ! pso_M = pom + M

          call me_eck_hech_moy (str_mod(iveh)%requa, xcn0(2:6), ecart_admi, &
               osc, moy, code_erreur)

          parmoy (1) = moy%a
          parmoy (2) = sqrt ( moy%ex * moy%ex + moy%ey * moy%ey )
          parmoy (3) = moy%i
          parmoy (4) = atan2 (moy%ey, moy%ex)
          parmoy (5) = moy%gom
          parmoy (6) = moy%pso_M - parmoy (4)

      !           sortie precedente recopiee
          if ( code_erreur%valeur < pm_OK ) then  
             if ( (abs(parorb(2)) <= 1.e-12_pm_reel) .or. &
                  (abs(parorb(3)) <= 1.e-12_pm_reel) ) then
                parmoy(1:6) = parorb(1:6)
             else
                if ( .not. flag_w_parmoy(iveh) ) then
                   flag_w_parmoy(iveh) = .true.
                   ! FA-ID 714 : on annule l'emission du message qui n'est pas justifié
                   !call MSP_signaler_message (ier_mslib=code_erreur)
                   !call MSP_annuler_probleme ()
                endif
                parmoy(1:6) = 0._pm_reel
             endif
          endif


       else
          parmoy(1:6) = parorb(1:6)
	  
       endif
       
       
       str_gen(iveh)%etat(61)    = parmoy(1)/1000._pm_reel
       str_gen(iveh)%etat(62)    = parmoy(2)
       str_gen(iveh)%etat(63) = parmoy(3)*raddeg
       str_gen(iveh)%etat(64) = parmoy(4)*raddeg
       str_gen(iveh)%etat(65) = parmoy(5)*raddeg
       str_gen(iveh)%etat(66) = parmoy(6)*raddeg
    end if
    
  end subroutine ps_parametres_EH

  subroutine ps_angle_beta(ralt, pos_soleil, dir_soleil, par)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_angle_beta
!
!$Resume
!  calcule l'angle beta entre la direction du soleil et le plan de l'orbite
!
!$Description
!  calcule l'angle beta entre la direction du soleil et le plan de l'orbite
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_angle_beta(ralt, pos_soleil, dir_soleil, par)
!.    real (KIND=pm_reel) :: ralt
!.    real (KIND=pm_reel), dimension(3) :: pos_soleil
!.    real (KIND=pm_reel), dimension(3) :: dir_soleil
!.    real (KIND=pm_reel), dimension(6) :: par
!
!$Arguments
!>E     ralt        :<pm_reel>           
!>E     pos_soleil  :<pm_reel,DIM=(3)>   
!>S     dir_soleil  :<pm_reel,DIM=(3)>   
!>E     par         :<pm_reel,DIM=(6)>   vecteur position/vitesse dans le repère d'intégration (m-m/s)
!
!$Common
!
!$Routines
!- mu_mulvecttrans3
!- mo_def_tnw
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_generalites
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

    use ps_generalites
    use ps_integration_don

    implicit none

! Arguments
! =========
    real (KIND=pm_reel), intent(in)                :: ralt
    real (KIND=pm_reel), dimension(3), intent(in)  :: pos_soleil
    real (KIND=pm_reel), dimension(3), intent(out) :: dir_soleil
    real (KIND=pm_reel), dimension(6), intent(in)  :: par

! Variables locales
! =================
    real (KIND=pm_reel)                            :: ndir_soleil
    real (KIND=pm_reel)                            :: betasol, cos_betasol, cobetasol
    real (KIND=pm_reel), dimension(3)              :: vecs_RI, vect, vecn, vecw

    type(tm_code_retour)                           :: code_erreur, code_retour_local

    if ( ralt > 40000._pm_reel ) then

   ! Calcul de la position du Soleil dans le RI

       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),pos_soleil,vecs_RI,code_retour_local)

   ! Direction du soleil dans RI:
       ndir_soleil = sqrt ( vecs_RI(1)**2 + vecs_RI(2)**2 + vecs_RI(3)**2  )
       dir_soleil(1) =  vecs_RI(1)/ndir_soleil
       dir_soleil(2) =  vecs_RI(2)/ndir_soleil
       dir_soleil(3) =  vecs_RI(3)/ndir_soleil


   ! Calcul de la direction perpendiculaire au plan de l'orbite - mo_def_tnw (mslib90)
       call mo_def_tnw (par(1:3), par(4:6), vect, vecn, vecw, code_erreur)
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages("ps_maj_vecteur_etat" )) return 
       end if
   ! Angle entre le plan de l'orbite et la direction du soleil:
       cos_betasol = vecw(1)*dir_soleil(1) + vecw(2)*dir_soleil(2) + vecw(3)*dir_soleil(3)
       cobetasol = acos(cos_betasol)

       betasol = pisd - cobetasol

       str_gen(iveh)%etat(100) = betasol*raddeg

    endif

  end subroutine ps_angle_beta


  subroutine ps_rotation_yawsteering(repa, pgamav, dir_soleil)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rotation_yawsteering
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
!  call ps_rotation_yawsteering(repa, pgamav, dir_soleil)
!.    integer :: repa
!.    real (kind=pm_reel), dimension(3,3) :: pgamav
!.    real (kind=pm_reel), dimension(3) :: dir_soleil
!
!$Arguments
!>E     repa        :<integer>             indicateur du type d'attitude 
!>E     pgamav      :<pm_reel,DIM=(3,3)>   matrice de passage du repère 
!                                          d'intégration au repère véhicule
!>E     dir_soleil  :<pm_reel,DIM=(3)>     direction du soleil dans le RI
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
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

    use ps_generalites

    implicit none

! Arguments
! =========
    integer, intent(in)                             :: repa
    real (kind=pm_reel), dimension(3,3), intent(in) :: pgamav
    real (kind=pm_reel), dimension(3), intent(in)   :: dir_soleil

! Variables locales
! =================
    real (kind=pm_reel)                             :: ndir_xveh, cos_rot_pan, rot_pan, cos_solz
    real (kind=pm_reel), dimension(3)               :: dir_xveh, dir_zveh

    if ( repa == MSP_ENUM_ATTI_YAW_STEERING ) then

   ! Direction de l'axe X du véhicule dans RI:
       ndir_xveh = sqrt (  pgamav(1,1)**2 + pgamav(2,1)**2 + pgamav(3,1)**2 )
       dir_xveh(1:3) = (/ pgamav(1,1)/ndir_xveh, pgamav(1,2)/ndir_xveh , pgamav(1,3)/ndir_xveh /)
       dir_zveh(1:3) = (/ pgamav(3,1)/ndir_xveh, pgamav(3,2)/ndir_xveh , pgamav(3,3)/ndir_xveh /)

   ! Angle entre l'axe X du véhicule et le soleil
       cos_rot_pan = dir_xveh(1)*dir_soleil(1) + dir_xveh(2)*dir_soleil(2) + &
            dir_xveh(3)*dir_soleil(3)
       rot_pan = acos(cos_rot_pan)

   ! Angle entre l'axe Z du véhicule et le soleil
       cos_solz = dir_zveh(1)*dir_soleil(1) + dir_zveh(2)*dir_soleil(2) + &
            dir_zveh(3)*dir_soleil(3)
       if ( cos_solz > 0._pm_reel ) then
          rot_pan = -rot_pan
       endif
       str_gen(iveh)%etat(101) = rot_pan*raddeg
    else
       str_gen(iveh)%etat(101) = 0._pm_reel
    endif

  end subroutine ps_rotation_yawsteering

  subroutine psi_contribution_forces(date, irepa, par, mu_soleil, pos_soleil, masse_vehicule, mci, aero, pgamav) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_contribution_forces
!
!$Resume
!  Routine de mise à jour de la partie du vecteur d'état concernant les
!  forces.
!
!$Description
!  Pour chacun des modèles inclus dans l'intégration, calcul des contributions
!  de chacune des forces. Cette routine met à jour les variables concernant
!  la force du potentiel du corps central, les forces du soleil et des éventuelles
!  lunes, la pression de radiation solaire, les composantes de l'atmosphère,
!  les propriétés ballistiques du véhicule, la force de frottement atmosphérique 
!  et la force de prpulsion.
!
!$Auteur
!  Cédric MARTEL
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_contribution_forces(date, irepa, par, mu_soleil, pos_soleil, masse_vehicule, mci, aero, pgamav) 
!.    type(tm_jour_sec) :: date
!.    integer :: irepa
!.    real (KIND=pm_reel), dimension(6) :: par
!.    real (kind=pm_reel) :: mu_soleil
!.    real (kind=pm_reel) :: masse_vehicule
!.    type (MSP_MCI) :: mci
!.    type (MSP_AERO) :: aero
!.    real (kind=pm_reel), dimension(3,3) :: pgamav
!.    real (kind=pm_reel), dimension(3) :: pos_soleil
!
!$Arguments
!>E     date            :<tm_jour_sec>         date courante (Jours Juliens)
!>E     irepa           :<integer>             indicateur du type d'attitude 
!>E     par             :<pm_reel,DIM=(6)>     vecteur position/vitesse dans le repère d'intégration (m-m/s)
!>E     mu_soleil       :<pm_reel>             mu du soleil
!>E/S   pos_soleil      :<pm_reel,DIM=(3)>     position du soleil dans l'EME2000
!>E     masse_vehicule  :<pm_reel>             masse du véhicule  
!>E     mci             :<MSP_MCI>             caractéristiques du véhicule
!>E     aero            :<MSP_AERO>            caractéristiques aérodynamiques du véhicule  
!>E     pgamav          :<pm_reel,DIM=(3,3)>   matrice de passage du repère 
!                                            d'intégration au repère véhicule 
!
!$Common
!
!$Routines
!- ps_calcul_tsid_RI_Rapp
!- ps_rotation_tsid
!- ps_calcul_potentiel
!- mu_mulvect3
!- psi_contrib_lune_sol
!- psi_contrib_rad_sol
!- psi_contrib_frott_atm
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_integration_don
!- ps_bulletin
!- ps_modeles
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

    use ps_generalites
    use ps_integration_don
    use ps_bulletin
    use ps_modeles


    implicit none
    
! Variables d'entrée et de sortie
    type(tm_jour_sec), intent(in)   :: date
    integer, intent(IN) :: irepa
    real (KIND=pm_reel), dimension(6), intent(IN) ::par
    real (kind=pm_reel), intent(IN) :: mu_soleil
    real (kind=pm_reel), intent(IN) :: masse_vehicule
    type (MSP_MCI), intent(IN) :: mci
    type (MSP_AERO), intent(IN) :: aero
    real (kind=pm_reel), dimension(3,3), intent(in) :: pgamav
    real (kind=pm_reel), dimension(3), intent(inout) :: pos_soleil

! Variables locales
    real (KIND=pm_reel) :: tsid
    ! Position et vitesses dans le repère d'application
    real (KIND=pm_reel), dimension(3) :: pos_rapp,vit_rapp, acc_pot_rapp
    ! Différentes accélerations
    real (KIND=pm_reel), dimension(3) :: accpot,accls,acctmp,accfro,accpra
    type (tm_code_retour) :: code_retour_local
    ! Position dans l'EME2000
    real (kind=pm_reel), dimension(3) :: pos2000



!   ********************************************************************
!   * Accélérations                                                    *
!   ********************************************************************

!**** Accélération due au potentiel (m/s**2):

    ! Expression de la position dans Rapp (repère d'application de la force)
    call ps_calcul_tsid_RI_Rapp(date,tsid)
    call ps_rotation_tsid(par(1:3),tsid,pos_rapp)
    call ps_rotation_tsid(par(4:6),tsid,vit_rapp)    
    
    ! calcul avec 2nd membre complet
    call ps_calcul_potentiel(pos_rapp,0,acc_Pot_rapp)
    if (MSP_gen_messages("ps_maj_vecteur_etat")) return

    ! Rotation inverse pour obtenir l'accélération dans RI.
    call ps_rotation_tsid(acc_Pot_rapp,-tsid,accpot)
    
    if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
    if ( str_gen(iveh)%planet == eph_terre ) then

       call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),accpot,acctmp,code_retour_local)
       accpot(1) = acctmp(1)
       accpot(2) = acctmp(2)
       accpot(3) = acctmp(3)

    endif
    str_gen(iveh)%etat(78) = accpot(1)
    str_gen(iveh)%etat(79) = accpot(2)
    str_gen(iveh)%etat(80) = accpot(3)
    str_gen(iveh)%etat(34) = sqrt(accpot(1)**2+accpot(2)**2+accpot(3)**2)

! Position du satellite dans l'EME 2000
    call mu_mulvect3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),par(1:3),pos2000,code_retour_local)

!**** Accélération due aux eventuels potentiels lunaire et solaire (m/s**2):
    call psi_contrib_lune_sol(date, pos2000, mu_soleil, pos_soleil,accls)

!**** Accélération due à l'éventuelle pression de radiation solaire (m/s**2):
    call psi_contrib_rad_sol(irepa, par, pos2000, mu_soleil, pos_soleil, &
         pgamav, accpra)

!**** Accélération due au frottement atmospherique (m/s**2):
    call psi_contrib_frott_atm(date, irepa, pos_rapp, vit_rapp, masse_vehicule, &
       mci, aero, pgamav, pos_soleil, accfro)

!**** Accélération totale à la date du bulletin (m/s**2):
!     (attention ... sans la propulsion)

    str_gen(iveh)%etat(30) = accpot(1) + accls(1) + accpra(1) + accfro(1)
    str_gen(iveh)%etat(31) = accpot(2) + accls(2) + accpra(2) + accfro(2)
    str_gen(iveh)%etat(32) = accpot(3) + accls(3) + accpra(3) + accfro(3)
    str_gen(iveh)%etat(33) = sqrt(str_gen(iveh)%etat(30)**2+ &
         str_gen(iveh)%etat(31)**2+str_gen(iveh)%etat(32)**2)

  end subroutine psi_contribution_forces

  subroutine psi_contrib_frott_atm(date, irepa, pos_rapp, vit_rapp, masse_vehicule, &
       mci, aero, pgamav, pos_soleil, accfro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_contrib_frott_atm
!
!$Resume
!  Routine de mise à jour de la partie du vecteur d'état concernant la force
!  de frottement atmosphérique
!
!$Description
!  Cette routine calcule la force de frottement atmosphérique et met à jour 
!  le vecteur d'état. Ce sont les variables VXAT, VYAT, VZAT, VVX/Y/Z, 
!  FROX/Y/Z, AFRO, RO, PDYN, MACH, CBAL, CPOR et FINE.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_contrib_frott_atm(date, irepa, pos_rapp, vit_rapp, masse_vehicule, &
!.           mci, aero, pgamav, pos_soleil, accfro)
!.    type(tm_jour_sec) :: date
!.    integer :: irepa
!.    real (KIND=pm_reel), dimension(3) :: pos_rapp,vit_rapp
!.    real (kind=pm_reel) :: masse_vehicule
!.    type (MSP_MCI) :: mci
!.    type (MSP_AERO) :: aero
!.    real (kind=pm_reel), dimension(3,3) :: pgamav
!.    real (kind=pm_reel), dimension(3) :: pos_soleil
!.    real (kind=pm_reel), dimension(3) :: accfro
!
!$Arguments
!>E     date            :<tm_jour_sec>         date courante (Jours Juliens)
!>E     irepa           :<integer>             indicateur du type d'attitude
!>E     pos_rapp        :<pm_reel,DIM=(3)>     position du véhicule dans le repère d'application
!>E     vit_rapp        :<pm_reel,DIM=(3)>     vitesse du véhicule
!>E     masse_vehicule  :<pm_reel>             masse du véhicule
!>E     mci             :<MSP_MCI>             caractéristiques du véhicule
!>E     aero            :<MSP_AERO>            caractéristiques aérodynamiques du véhicule
!>E     pgamav          :<pm_reel,DIM=(3,3)>   matrice de passage du repère 
!                                            d'intégration au repère véhicule
!>E/S   pos_soleil      :<pm_reel,DIM=(3)>     position du soleil dans l'EME2000
!>S     accfro          :<pm_reel,DIM=(3)>     composantes de l'accélération 
!                                            correspondant au frottement calculé
!
!$Common
!
!$Routines
!- ps_calcul_tsid_RI_Rapp
!- ps_mat_rotation_tsid
!- mu_matmul3
!- psi_rep_eme2000_veis
!- psfratm
!- ps_rotation_tsid
!- mu_mulvect3
!- MSP_consulter_mci
!- MSP_calcul_surf_app
!- MSP_consulter_aero
!
!$Include
!
!$Module
!#V
!- ps_modeles
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


    use ps_modeles

    implicit none

! Variables d'entrée et de sortie
    type(tm_jour_sec), intent(in)   :: date
    integer, intent(IN) :: irepa
    real (KIND=pm_reel), intent(IN), dimension(3) :: pos_rapp,vit_rapp
    real (kind=pm_reel), intent(IN) :: masse_vehicule
    type (MSP_MCI), intent(IN) :: mci
    type (MSP_AERO), intent(IN) :: aero
    real (kind=pm_reel), dimension(3,3), intent(in) :: pgamav
    real (kind=pm_reel), dimension(3), intent(inout) :: pos_soleil
    real (kind=pm_reel), dimension(3), intent(out) :: accfro

! Variables locales
    real(kind=pm_reel), dimension(3) :: vit_atm
    real(kind=pm_reel), dimension(3) :: vit_vent
    ! Elements de ballistique
    real (kind=pm_reel) :: sx, sy, sz, st,cmf,surf_app
    real (KIND=pm_reel) :: mach,xcd,xcl
    integer :: forme

    type (tm_code_retour) :: code_retour_local
    real (KIND=pm_reel) :: tsid, ro
    ! Accélération
    real (KIND=pm_reel), dimension(3) :: acctmp, acc_fro_rapp

    ! matrices de changement de repère
    real (kind=pm_reel), dimension(3,3) :: mat_RI_Rapp
    real (kind=pm_reel), dimension(3,3) :: mat_Rapp_Rveh,mat_Rapp_RI
    ! position du soleil dans le Veis
    real (kind=pm_reel), dimension(3) :: pos_soleil_veis

    ! Si le frottement est pris en compte
    if ( str_mod(iveh)%ikle(3) /= 0 ) then


       ! a) calcul du temps sidéral
       call ps_calcul_tsid_RI_Rapp(date,tsid)

       ! b) Conversion de la position dans le repère d'application de la force


       ! c) Calcul de la matrice de rotation de passage de Rapp à RI
       !    puis de la matrice Rapp -> Rveh 
       call ps_mat_rotation_tsid(tsid,mat_RI_Rapp)
       call ps_mat_rotation_tsid(-tsid,mat_Rapp_RI)
       call mu_matmul3(pgamav,mat_Rapp_RI,mat_Rapp_Rveh,code_retour_local) 

       ! d) calcul position du soleil dans le Veis
       call psi_rep_eme2000_veis(date,pos_soleil,pos_soleil_veis)

       call psfratm(date,pos_rapp,vit_rapp,mat_Rapp_Rveh,pos_soleil_veis,irepa,&
            acc_fro_rapp,ro,mach,vit_atm,vit_vent,xcd,xcl)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return

       ! Rotation inverse pour se replacer dans le repère d'intégration
       call ps_rotation_tsid(acc_fro_rapp,-tsid,accfro)

       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
       str_gen(iveh)%etat(107) = vit_atm(1)       ! vitesse par rapport à l'atmosphère dans  
       str_gen(iveh)%etat(108) = vit_atm(2)       ! le repère RTS
       str_gen(iveh)%etat(109) = vit_atm(3)       
       str_gen(iveh)%etat(110) = vit_vent(1)       ! vitesse du vent dans le repèreRTS 
       str_gen(iveh)%etat(111) = vit_vent(2)       
       str_gen(iveh)%etat(112) = vit_vent(3)      
       if ( str_gen(iveh)%planet == eph_terre ) then

          call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),accfro,acctmp,code_retour_local)
          accfro(1)   = acctmp(1)  
          accfro(2)   = acctmp(2)  
          accfro(3)   = acctmp(3) 
       endif
       str_gen(iveh)%etat(90) = accfro(1)
       str_gen(iveh)%etat(91) = accfro(2)
       str_gen(iveh)%etat(92) = accfro(3)
       str_gen(iveh)%etat(38)    = sqrt(accfro(1)**2+accfro(2)**2+accfro(3)**2)


!**** densite atmospherique (kg/m**3):
       str_gen(iveh)%etat(39) = ro

!**** pression dynamique (Pa):
       str_gen(iveh)%etat(60) = 0.5_pm_reel*ro*str_gen(iveh)%etat(18)**2

!**** nombre de Mach:
       str_gen(iveh)%etat(96) = mach

!**** coefficient balistique (m**2/kg):
       call MSP_consulter_mci(mci,forme=forme,sx=sx,sy=sy,sz=sz,st=st)

       ! appel à la routine MECASPA de calcul de la surface d'application de la force
       ! selon les différentes surfaces et la matrice de l'attitude du véhicule
       ! dans le repère d'intégration
       call MSP_calcul_surf_app(forme, sx, sy, sz, st, 1, pgamav, vit_atm, surf_app)
       if (MSP_gen_messages("ps_maj_vecteur_etat")) return

       call MSP_consulter_aero(aero,cmf=cmf)

       ! Mise à jour du vecteur d'état
       str_gen(iveh)%etat(40)=surf_app*xcd*cmf/masse_vehicule
       str_gen(iveh)%etat(94)=surf_app*xcl*cmf/masse_vehicule
    
       if ( abs(xcl) < MSP_EPSILON_APLA .or. &
            abs(xcd) < MSP_EPSILON_APLA ) then
          str_gen(iveh)%etat(95) = 0._pm_reel
       else
          str_gen(iveh)%etat(95) = xcl/xcd
       endif

    else
       ! mise à 0 de ces variables de sorties lorsque le frottement n'est pas utilisé
       str_gen(iveh)%etat(90) = 0._pm_reel
       str_gen(iveh)%etat(91) = 0._pm_reel
       str_gen(iveh)%etat(92) = 0._pm_reel
       str_gen(iveh)%etat(38) = 0._pm_reel
       
       str_gen(iveh)%etat(40) = 0._pm_reel
       str_gen(iveh)%etat(94) = 0._pm_reel
       str_gen(iveh)%etat(95) = 0._pm_reel
       
       accfro(1) = 0._pm_reel
       accfro(2) = 0._pm_reel
       accfro(3) = 0._pm_reel
      
    endif
    

  end subroutine psi_contrib_frott_atm


  subroutine psi_contrib_rad_sol(irepa, par, pos2000, mu_soleil, &
       pos_soleil, pgamav, accpra)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_contrib_rad_sol
!
!$Resume
!  Routine de mise à jour de la partie du vecteur d'état concernant les
!
!$Description
!  Cette routine calcule la contribution de la force de radiation solaire.
!  Le vecteur d'état est mis à jour, notamment les variables RADX/Y/Z et ARAD.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_contrib_rad_sol(irepa, par, pos2000, mu_soleil, &
!.           pos_soleil, pgamav, accpra)
!.    integer :: irepa
!.    real (KIND=pm_reel), dimension(6) :: par
!.    real (kind=pm_reel), dimension(3) :: pos2000
!.    real (kind=pm_reel) :: mu_soleil
!.    real (kind=pm_reel), dimension(3) :: pos_soleil
!.    real (kind=pm_reel), dimension(3) :: accpra
!.    real (kind=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     irepa       :<integer>             indicateur du type d'attitude
!>E     par         :<pm_reel,DIM=(6)>     vecteur position/vitesse dans le repère d'intégration (m-m/s)
!>E/S   pos2000     :<pm_reel,DIM=(3)>     position du véhicule dans l'EME2000
!>E     mu_soleil   :<pm_reel>             mu du soleil
!>E/S   pos_soleil  :<pm_reel,DIM=(3)>     position du soleil dans l'EME2000
!>E     pgamav      :<pm_reel,DIM=(3,3)>   matrice de passage du repère 
!                                          d'intégration au repère véhicule
!>S     accpra      :<pm_reel,DIM=(3)>     accélération due à la pression de radiation solaire
!
!$Common
!
!$Routines
!- MSP_acc_3corps
!- mu_mulvect3
!- mu_mulvecttrans3
!- mu_norme
!- psprsol
!
!$Include
!
!$Module
!#V
!- ps_modeles
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

    use ps_modeles

    implicit none

! Variables d'entrée et de sortie
    integer, intent(IN) :: irepa
    real (KIND=pm_reel), dimension(6), intent(IN) ::par
    real (kind=pm_reel), dimension(3) :: pos2000
    real (kind=pm_reel), intent(IN) :: mu_soleil
    real (kind=pm_reel), dimension(3), intent(inout) :: pos_soleil
    real (kind=pm_reel), dimension(3), intent(out) :: accpra

! Variables locales
    real (kind=pm_reel) :: dis_soleil
    real (kind=pm_reel), dimension(3) :: acc_soleil,dir_soleil
    real (kind=pm_reel), dimension(3) :: pos_soleil_RI
    real (KIND=pm_reel), dimension(3) :: dirs_norme, dirs
    real (KIND=pm_reel) :: norme
    real (KIND=pm_reel), dimension(3) :: acctmp
    type (tm_code_retour) :: code_retour_local
    real (kind=pm_reel), dimension(3,3), intent(in) :: pgamav

    ! Si la pression de radiation solaire doit être prise en compte
    if ( str_mod(iveh)%ikle(4) /= 0 ) then
       call MSP_acc_3corps (pos2000,mu_soleil,pos_soleil,dis_soleil,&
            dir_soleil,acc_soleil)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return

       ! Note : pas de retour en erreur possible
       ! Passage de la position du soleil de l'EME2000 au repère d'intégration
       call mu_mulvect3(str_int(iveh)%Mat_ME2000_RI(1:3,1:3),pos_soleil,pos_soleil_RI,code_retour_local)

       ! Passage de la direction Sonde->soleil dans l'EME2000
       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),dir_soleil,dirs,code_retour_local)

       call mu_norme (dirs,norme,code_retour_local,dirs_norme)

       ! Calcul de la pression de radiation solaire
       call psprsol (par(1:3),dirs_norme(1),dirs_norme(2),dirs_norme(3),pgamav,irepa,accpra)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return

       ! Passage de l'accélération du repèr d'intégration au repère de sortie
       call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),accpra,acctmp,code_retour_local)
       accpra(1)   = acctmp(1)
       accpra(2)   = acctmp(2)
       accpra(3)   = acctmp(3)
       ! Stockage
       str_gen(iveh)%etat(87) = accpra(1)
       str_gen(iveh)%etat(88) = accpra(2)
       str_gen(iveh)%etat(89) = accpra(3)
       str_gen(iveh)%etat(37)    = sqrt(accpra(1)**2+accpra(2)**2+accpra(3)**2)
    else
       str_gen(iveh)%etat(87:89) = 0._pm_reel
       str_gen(iveh)%etat(37)    = 0._pm_reel
       accpra(1:3) = 0._pm_reel
    endif


  end subroutine psi_contrib_rad_sol


  subroutine psi_contrib_lune_sol(date, pos2000, mu_soleil, pos_soleil, accls)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_contrib_lune_sol
!
!$Resume
!  Routine de mise à jour de la partie du vecteur d'état concernant la force
!  de potentiel des lunes du corps central. Les variables LUNX/Y/Z, ALUN, SOLX/Y/Z
!  et ASOL sont mises à jour dans le vecteur d'état.
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_contrib_lune_sol(date, pos2000, mu_soleil, pos_soleil, accls)
!.    type(tm_jour_sec) :: date
!.    real (kind=pm_reel), dimension(3) :: pos2000
!.    real (kind=pm_reel) :: mu_soleil
!.    real (kind=pm_reel), dimension(3) :: pos_soleil
!.    real (kind=pm_reel), dimension(3) :: accls
!
!$Arguments
!>E     date        :<tm_jour_sec>       date courante (Jours Juliens)
!>E/S   pos2000     :<pm_reel,DIM=(3)>   position du véhicule dans l'EME2000
!>E     mu_soleil   :<pm_reel>           mu du soleil
!>E/S   pos_soleil  :<pm_reel,DIM=(3)>   position du soleil dans le RI
!>S     accls       :<pm_reel,DIM=(3)>   accélération du au soleil et à d'éventuelles lunes
!
!$Common
!
!$Routines
!- ps_propage
!- MSP_acc_3corps
!- mu_mulvecttrans3
!- mu_mulvect3
!
!$Include
!
!$Module
!#V
!- ps_modeles
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

    use ps_modeles

    implicit none

! Variables d'entrée et de sortie
    type(tm_jour_sec), intent(in)   :: date
    real (kind=pm_reel), dimension(3) :: pos2000
    real (kind=pm_reel), intent(IN) :: mu_soleil
    real (kind=pm_reel), dimension(3), intent(inout) :: pos_soleil
    real (kind=pm_reel), dimension(3), intent(out) :: accls

! Variables locales
    ! Ephémérides des différents corps possibles
    real (kind=pm_reel), dimension(3) :: pos_lune,dir_lune
    real (kind=pm_reel), dimension(3) :: pos_phobos,dir_phobos
    real (kind=pm_reel), dimension(3) :: pos_deimos,dir_deimos
    real (kind=pm_reel) :: dis_soleil,dis_lune,dis_phobos,dis_deimos
    real (kind=pm_reel), dimension(3) :: dir_soleil
    real (kind=pm_reel) :: mu_lune,mu_phobos,mu_deimos
    ! Accélération des différents corps
    real (KIND=pm_reel), dimension(3) :: acc_soleil,acc_lune,acc_deimos,acc_phobos
    real (KIND=pm_reel), dimension(3) :: accl,accl1,accl2,accs
    type (tm_code_retour) :: code_retour_local

!**** Accélération due au potentiels lunaire (m/s**2) si sélectionné
if ( str_mod(iveh)%ikle(1) /= 0 ) then

    if ( str_gen(iveh)%planet == eph_terre) then 
       ! Dans le cas Terre, Lune seulement : effet de la Lune
       call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_LUNE,&
         pos_lune,mu_lune)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return

       call MSP_acc_3corps (pos2000,mu_lune,pos_lune,dis_lune,dir_lune,acc_lune)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
       ! calcul des accélérations dans le RI
       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),acc_lune,accl,code_retour_local)

    else if ( str_gen(iveh)%planet == eph_mars) then 

       ! Dans le cas Mars : Deimos et Phobos
       call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_PHOBOS,&
         pos_phobos,mu_phobos)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
       call MSP_acc_3corps (pos2000,mu_phobos,pos_phobos,dis_phobos,&
            dir_phobos,acc_phobos)
       ! calcul des accélérations dans le RI
       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),acc_phobos,accl1,code_retour_local)

       call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_DEIMOS,&
         pos_deimos,mu_deimos)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
       call MSP_acc_3corps (pos2000,mu_deimos,pos_deimos,dis_deimos,&
            dir_deimos,acc_deimos)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
       ! calcul des accélérations dans le RI
       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),acc_deimos,accl2,code_retour_local)

       ! Somme des accélérations
       accl(1) = accl1(1) + accl2(1)
       accl(2) = accl1(2) + accl2(2)
       accl(3) = accl1(3) + accl2(3)

    endif

! calcul de la somme des accélérations (accl) dans le repère de sortie
    call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),accl(1:3),str_gen(iveh)%etat(81:83),code_retour_local)
    ! Calcul de la norme
    str_gen(iveh)%etat(35) = sqrt(str_gen(iveh)%etat(81)**2+ &
         str_gen(iveh)%etat(82)**2+str_gen(iveh)%etat(83)**2)
else
    !La force n'est pas sélctionnée, on met les valeurs à zéro
    str_gen(iveh)%etat(81:83) = 0._pm_reel
    str_gen(iveh)%etat(35)    = 0._pm_reel
endif

!**** Accélération due au potentiel solaire (m/s**2)
    ! Si le soleil est sélectionné et pour toutes les planètes
    if ( str_mod(iveh)%ikle(2) /= 0 ) then
       call MSP_acc_3corps (pos2000,mu_soleil,pos_soleil,dis_soleil,&
            dir_soleil,acc_soleil)
       if ( MSP_gen_messages("ps_maj_vecteur_etat") ) return
   ! calcul des accélérations dans le RI puis le ROUT

       call mu_mulvecttrans3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),acc_soleil,accs,code_retour_local)
       call mu_mulvect3(str_int(iveh)%Mat_RI_ROUT(1:3,1:3),accs(1:3),str_gen(iveh)%etat(84:86), code_retour_local)

       str_gen(iveh)%etat(35)    = sqrt(str_gen(iveh)%etat(81)**2+&
            str_gen(iveh)%etat(82)**2+str_gen(iveh)%etat(83)**2)
       str_gen(iveh)%etat(36)    = sqrt(str_gen(iveh)%etat(84)**2+&
            str_gen(iveh)%etat(85)**2+str_gen(iveh)%etat(86)**2)
    else
       str_gen(iveh)%etat(84:86) = 0._pm_reel
       str_gen(iveh)%etat(36)    = 0._pm_reel
    endif

    ! Contribution complète
    accls(:) = str_gen(iveh)%etat(81:83) + str_gen(iveh)%etat(84:86)

  end subroutine psi_contrib_lune_sol

end module ps_variables
