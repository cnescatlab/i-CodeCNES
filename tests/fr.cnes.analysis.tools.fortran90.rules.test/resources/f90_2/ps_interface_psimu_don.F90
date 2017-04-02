module ps_interface_psimu_don

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_interface_psimu_don
!
!$Resume
!  Module gérant les données et les sous-programmes d'interface de PSIMU avec l'utilisateur.
!
!$Description
!  Module gérant les données et les sous-programmes d'interface de PSIMU avec l'utilisateur.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_interface_psimu_don.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_interface_psimu_don.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.90  2011/09/26 14:08:27  mercadig
!  VERSION::FA-ID:1493:26/09/2011:ajout test sur la valeur de psmod0kle(5) dans la routine de conversion psimu vers mecaspa
!
!  Revision 1.89  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.88  2009/12/03 13:20:23  mercadig
!  AQ: Suppression de variables inutilisees
!
!  Revision 1.87  2009/11/19 15:55:28  mercadig
!  DM-ID 1019: Mise a jour cartouche imodout
!
!  Revision 1.86  2009/11/13 15:09:26  mercadig
!  DM-ID 842 : Prise en compte de l activite solaire reelle COMPAS
!
!  Revision 1.85  2009/10/06 08:35:26  cmartel
!  DM-ID 1164 : Le coordonnees cartesiennes doivent toujours etre mises a jour (pb algo)
!
!  Revision 1.84  2009/09/04 15:15:22  mercadig
!  DM-ID 1218: Recherche des fichiers ephemerides dans la base locale COMPAS, suppression de la ressource data_3corps
!
!  Revision 1.83  2009/09/04 12:53:13  tanguyy
!  DM-ID 1113 : lorsqu'une structure MSP_AERO est passÃ©e Ã  psconv_vehicule_pscar, PSIMU la stocke tout de suite dans str_car(iveh)%vehicule
!
!  Revision 1.82  2009/08/27 13:53:20  cmartel
!  DM-ID 1120 : Prise en compte de la modification de signature des modèles exp.
!
!  Revision 1.81  2009/07/15 10:20:03  cml
!  DM-ID 1164 : Ajout d'une routine d'analyse des variables de sortie
!
!  Revision 1.80  2009/06/16 07:33:45  tanguyy
!  FA-ID 1306 : import de la routine ps_terminer_session_PSIMU
!  Revision 1.79  2009/05/05 09:56:20  tanguyy
!  FA-ID 1291 : correction de la prise en compte des coefs aero constants dans les structures 'separation'
!  Revision 1.78  2009/03/17 16:00:02  tanguyy
!  FA-ID 1214 : utilisation des nouvelles constantes definies dans MSP_integrator_def, pour donner les parametrages par defaut du Cowell
!  Revision 1.77  2009/03/17 15:56:20  tanguyy
!  FA-ID 1249 : prise en compte des structures MSP_MODVENT vides
!  Revision 1.76  2009/03/13 07:47:17  tanguyy
!  FA-ID 1233 : le champ mtot de la structure MCI correspond bien a la masse totale du vehicule (=structure + ergols)
!  Revision 1.75  2009/02/03 10:06:20  cml
!  FA-ID 1225 : Correction de l utilisation de deltat pour US76
!  Revision 1.74  2009/01/13 14:08:56  cml
!  FA-ID 1179 : Ajout de lambda_gw dans la structure du modele et dans les conversions
!  Revision 1.73  2008/12/08 08:38:29  tanguyy
!  AQ : amelioration des liberations memoires et FA-ID 1161 (gestion du cas Venus lors de la recherche du modele de potentiel)
!  Revision 1.72  2008/12/04 15:42:05  tanguyy
!  AQ : suppression de variables inutilisees
!  Revision 1.71  2008/12/02 13:26:48  mercadig
!  DM-ID 733: Ajout structure emcd pour creation structure atmosphere
!  Revision 1.70  2008/12/02 10:48:08  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.69  2008/11/26 08:34:41  tanguyy
!  DM-ID 733 : gestion des coefs aero -> tout est "confié" à la MECASPA
!  Revision 1.68  2008/10/24 09:45:10  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.67  2008/10/20 14:39:13  mercadig
!  FA-ID 1129 : Tableau typcf dimensionne a ps_nsepa dans la routine psconv_pssep_sep
!  Revision 1.66  2008/09/19 12:34:41  mercadig
!  FA-ID 1103 : Modif des valeurs par defaut de ps_creer_strinteg
!  Revision 1.65  2008/09/11 15:59:26  tanguyy
!  DM-ID 1058 : mise a NULL des pointeurs
!  Revision 1.64  2008/09/04 07:53:08  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.63  2008/05/02 14:37:20  tanguyy
!  FA-ID 865 : suppression de modprec (obsolete dans GSLIB et inutilise dans PSIMU)
!  Revision 1.62  2008/03/20 10:42:36  tanguyy
!  Correction du cartouche
!  Revision 1.61  2008/03/19 15:20:50  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.60  2008/01/17 16:46:32  tanguyy
!  FA-ID 905 : suppression du parametre iatmos dans la structure modeles
!  Revision 1.59  2008/01/17 14:01:00  huec
!  Compatibilite des noms de modeles d atmosphere PSIMU / MECASPA
!  Revision 1.58  2008/01/08 13:12:56  huec
!  FA-ID 841 : Suppression de variables declarees en double
!  Revision 1.57  2007/12/04 08:16:17  tanguyy
!  Integration DM-ID 551 / DM-ID 808
!  Revision 1.56  2007/10/30 08:44:44  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.55  2007/09/24 15:06:19  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.54  2007/07/09 12:35:08  tanguyy
!  DM-ID 702, DM-ID 692, DM-ID 748. Création de la routine ps_creer_strinteg
!  Revision 1.53  2007/06/20 12:22:48  vivaresf
!  FA-ID 746 : Fuites mémoires, scénarios de sortie en inout pour permettre leur désallocation
!  Revision 1.52  2007/03/19 14:56:09  mle
!  DM-ID 600 : modification de la structure d ecriture et etatn peut avoir 10 characteres
!  Revision 1.51  2007/02/05 17:44:26  tanguyy
!  DM-ID 643 : rajout de routines pour convertir MSP_MODVENT vers les champs ad hoc de PS_STR_MODELES
!  Revision 1.50  2007/02/02 13:31:35  tanguyy
!  DM-ID 659 : prise en compte des types de variation des coefs Aero
!  Revision 1.49  2007/01/26 14:26:17  vivaresf
!  DM-ID 643 : ikle de taille 6
!  les appels à deallocate sont fait avec un stat)
!  Revision 1.48  2007/01/26 08:03:20  vivaresf
!  DM-ID 659 : Cloture du FT (Coefficients aerodynamiques dependants de l altitude)
!  Revision 1.47.2.1  2007/01/25 08:44:22  tanguyy
!  Adaptation des structures de donnees PSIMU (DM-ID 643, DM-ID 659)
!  Revision 1.47  2007/01/16 12:48:47  mle
!  DM-ID 642 : Nombre de separations dans PSIMU, modifications dans les commentaires
!  Revision 1.46  2007/01/16 11:39:03  mle
!  DM-ID 642 : Nombre de separations dans PSIMU
!  Revision 1.45  2006/12/01 09:38:53  tanguyy
!  Integration DM-ID 560 et DM-ID 552
!  Revision 1.44  2006/11/07 13:09:59  tanguyy
!  Modif d'un appel MECASPA (DM-ID 560)
!  Revision 1.43  2006/11/06 17:20:00  tanguyy
!  DM-ID 560 / DM-ID 552 : prise en comptes des evolutions GS_LIB (Lois d'attitude, et modeles de potentiels de COMPAS
!  Revision 1.42  2006/10/17 09:54:25  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.41  2006/10/02 14:01:48  aitiere
!  AQ : maj des declarations
!  Revision 1.40  2006/09/25 10:27:02  tanguyy
!  Conversion de la structure modèle PSIMU -> MECASPA : meilleure utilisation de ikle(5)
!  Revision 1.39  2006/06/30 15:53:05  tanguyy
!  Integration PSIMU 8-5
!  Revision 1.38  2006/05/30 09:33:08  tanguyy
!  DM-ID 232 : Cloture du FT (Nommage des arguments lors de l appel d une routine ou d une fonction en Fortran 90 ou d un objet GENESIS)
!  Revision 1.37.2.1  2006/05/30 09:32:20  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.37  2006/04/21 08:12:33  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.36.2.1  2006/04/20 13:23:16  tanguyy
!  DM-ID 400 : utilisation de routines MSP rendant un pointeur sur la structure encapsulee dans les scenarios de lois
!  Revision 1.36  2006/03/16 13:13:16  tanguyy
!  FA-ID 497 Reorganisation des 'use'
!  Revision 1.35  2006/03/15 13:14:26  tanguyy
!  FA-ID 496 : Correction d'une anomalie dans psconv_sep_pssep
!  Revision 1.34  2005/11/10 18:37:08  vivaresf
!  Mise à jour des cartouches
!  Revision 1.33  2005/11/09 13:23:47  vivaresf
!  DM-ID 6 : valeur par défaut de type d'éphémérides
!  Revision 1.32  2005/02/17 13:57:33  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.31  2005/01/31 16:37:24  fabrec
!  DM-ID 235 : utilisation de typcf
!  Revision 1.30  2005/01/28 10:42:12  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.29  2005/01/18 07:58:05  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.28  2005/01/17 15:28:02  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.27  2004/12/10 16:59:01  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.26  2004/11/22 14:35:14  vivaresf
!  FA-ID 200 : gestion de la variable vitrot pour les propulsion bulletin
!  Revision 1.25  2004/11/22 14:09:34  fabrec
!  DM_175
!  Revision 1.24.2.1  2004/11/22 14:00:18  fabrec
!  fichiers de manouvres
!  Revision 1.24  2004/11/22 08:54:08  vivaresf
!  DM_ID 200 : lecture des requa et apla du bulletin pour les conversions de type du
!              bulletin initial et des propulsions bulletin
!  Revision 1.23  2004/10/11 16:28:38  vivaresf
!  DM 81 : precision sur les dates de manoeuvres
!  Revision 1.22  2004/07/07 08:55:31  adm_ipsi
!  DM_89
!  Revision 1.21.2.1  2004/07/02 07:41:40  ole
!  DM_89, version initiale
!  Revision 1.21  2004/06/24 08:58:01  adm_ipsi
!  DM-ID 88 : intégration
!  Revision 1.20  2004/06/22 10:50:20  vivaresf
!  DM-ID 108 : Utilisation du rmu de psbul
!  Revision 1.19  2004/06/18 10:33:10  vivaresf
!   Mise a jour des entetes
!  Revision 1.18  2004/06/18 10:05:56  vivaresf
!   DM-ID 133 : integration
!  Revision 1.17.6.1  2004/06/23 15:57:54  adm_ipsi
!   DM-ID 88 : Utilisation du fichier de ressources associé à la version
!  Revision 1.17.4.1  2004/06/18 09:42:29  vivaresf
!   DM-ID 133, psconv_pssep_sep et psconv_sep_pssep : 
!  Redefinition possible du vehicule apres separation
!  Revision 1.17  2003/11/28 16:55:03  adm_ipsi
!   DM-ID 9, Ajout du Choix du fichier d'activite solaire
!  Revision 1.16  2003/07/15 13:17:42  adm_ipsi
!   FA-ID 11 : Liberation de la mémoire
!  Revision 1.16  2003/03/28 09:56:46  util_am
!   SLB - Version industrialisée
!  Revision 1.15  2003/03/14 15:25:28  adm_ipsi
!   Utilisation de ps_nloi_att, ps_nloi_propu, ps_npts_att, ps_npts_propu, ps_nsepa
!  Revision 1.14  2003/02/21 16:00:13  rodier
!   PhB - Mise à jour appel MSP_creer_bulletin dans psconv_psbul_bul
!  Revision 1.13  2003/02/14 16:27:50  rodier
!   PhB - Ajout variables pour définition nouveaux repères
!  Revision 1.12  2002/12/20 16:39:39  boschett
!   Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.11  2002/12/12 15:05:24  boschett
!   Livraison Intermediaire 16/12/2002
!  Revision 1.10  2002/12/10 11:41:39  boschett
!   Ajout du traitement par défaut dans la structure if/elseif/else de la
!   subroutine psconv_vehicule_pscar
!  Revision 1.9  2002/12/05 15:47:02  boschett
!  Conversion explicite d'entier à réel dans les assignations melangeant les types
!  Revision 1.8  2002/12/04 10:03:26  boschett
!   Substitution du mot clé 'type' par 'typloi' dans les subroutines
!   psconv_pro_pspro et psconv_ati_psati
!  Revision 1.7  2002/12/02 17:06:41  boschett
!   Suppression des variables locales déclarées et non utilisées
!  Revision 1.6  2002/11/26 17:00:25  boschett
!  Ajout de implicit none  
!  Revision 1.5  2002/11/22 12:05:43  boschett
!  Passage de l'argument de scénario en INOUT pour l'appel à MSP_consulter_scenario
!  car cette fonction repositionne le pointeur loi_courante du scénario
!  Revision 1.4  2002/11/15 15:44:05  adm_ipsi
!  *** empty log message ***
!  Revision 1.3  2002/10/30 11:16:52  laurent
!  Ajout de commentaire fonctionnel
!  Revision 1.2  2002/10/07 09:21:56  adm_ipsi
!  Utilisation de la structure MSP_COEF pour l'appel à MSP_consulter_aero
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.15  2002/09/16 11:06:02  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.14  2001/11/07 13:12:23  util_am
!  Possibilité de choisir entre G50 CNES et J2000 pour le RIS en mode Terre
!  Revision 1.13  2001/01/09 12:42:30  util_am
!  Prise en compte de la précision numérique sur une date d'entrée
!  Revision 1.12  2000/06/21 15:29:54  util_am
!  Gestion des ressources (fichiers de donnees) + appels prives aux structures mecaspa
!  Revision 1.11  2000/04/19 11:51:05  util_am
!  Problèmes de fuite mémoire à la création de potentiels
!  Revision 1.10  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.9  2000/02/08 09:47:17  util_am
!  Modification pour tenir compte d'un delta de date sur les lois lors d'une réinitialisation de bulletin
!  (=> Ajout du paramètre datbul pour les utilitaires de conversions)
!  Revision 1.8  1999/12/02 16:54:47  util_am
!  Correction d'un bug lors de l'appel à MSP_creer_trois_corps (dans psconv_psmod_mod) à cause de l'inversion de lune/soleil
!  Revision 1.7  1999/11/18 12:10:46  util_am
!  Correction d'un bug sur la gestion du nom du fichier ephemerides
!  Revision 1.6  1999/10/26 10:57:28  util_am
!  Prise en compte du 3ème corps et ajout des surfaces de panneaux solaires
!  Revision 1.5  1999/08/31 11:56:11  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.4  1999/08/04 11:28:15  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_interface_psimu_don
!
!$Structure
!
!: PS_STR_ECRITURE : structure définissant ce qui sera écrit dans le fichier EPHEM
!>     SEQUENCE         : <>                                           
!>     pas_sortie       : <pm_reel>                                    
!>     nvarw            : <integer>                                    nombre de variables à écrire dans les fichiers EPHEM/EVENT  
!>     iresw            : <integer,DIM=(nvmax)>                        numéro d'ordre des variables à écrire dans les fichiers EPHEM/EVENT  
!>     indw             : <integer>                                    indice d'écriture (modulo indw)  
!>     iforw            : <integer>                                    format d'écriture des variables à écrire dans le fichier EPHEM  
!>     imodout          : <integer>                                    GAMMA 50, J2000 ou GAMMA VRAI de la date pour la Terre
!.                                               sur Mars et Venus, Planetocentrique Equatorial de la date ou EME2000
!>     ipas             : <integer>                                    
!>     ivarmax          : <integer>                                    nombre de variables maximum  
!>     etatn            : <LEN=10,DIM=(nvmax)>                         tableau des noms des variables  
!>     itypsort         : <integer>                                    "SORTILEGE"
!
!: PS_STR_INTEGRATION : structure définissant les pas d'intégration
!>     SEQUENCE         : <>                                           
!>     h1               : <pm_reel>                                    altitude au dessus de laquelle on est avec Cowell + xpash0  (m)                
!>     h2               : <pm_reel>                                    altitude en dessous de laquelle on passe à Runge Kutta (m)                
!>     hstop            : <pm_reel>                                    altitude d'arrêt (m)                
!>     h3               : <pm_reel>                                    
!>     pinteg0          : <MSP_INTEGRATOR>                             
!>     pinteg1          : <MSP_INTEGRATOR>                             
!>     pinteg2          : <MSP_INTEGRATOR>                             
!>     pinteg3          : <MSP_INTEGRATOR>                             
!>     rdatmax          : <pm_reel>                                    
!>     tmax             : <pm_reel>                                    date max (s)              
!>     rdatbul          : <pm_reel>                                    
!>     type_alt         : <integer>                                    
!>     idatmax          : <integer>                                    
!>     typdat           : <integer>                                    
!>     idatbul          : <integer>                                    
!
!: PS_STR_MODELES : structure définissant les modèles utilisés
!>     SEQUENCE         : <>                                           
!>     flu              : <pm_reel>                                    flux solaire standard      
!>     app              : <pm_reel>                                    indice géomagnétique AP     
!>     deltat           : <pm_reel,DIM=(8)>                            tableau d'écarts en température pour le modèle terrestre US76    
!>     param_atm_exp    : <pm_reel,DIM=(7)>                            tableau des paramètres nécessaires au modèle exponentiel
!.                                        param_atm_exp(1) : densité de référence (ro0) (kg/m^3)
!.                                        param_atm_exp(2) : hauteur de référence (h0) (m)
!.                                        param_atm_exp(3) : altitude minimun (m)
!.                                        param_atm_exp(4) : altitude maximum (m)
!.                                        param_atm_exp(5) : flag indiquant l'utiliation de hscale ou beta
!.                                        param_atm_exp(6) : hauteur d'échelle (m)
!.                                        param_atm_exp(7) : inverse de la hauteur d'échelle (m^-1)
!>     typper           : <pm_reel,DIM=(2)>                            perturbations pour le modèle d'atmosphère martien européen.
!.                                        typper(1) : code utilisé pour le type de perturbation
!.                                                    typper(1) == 1. : aucune perturbation
!.                                                    typper(1) == 2. : perturbation de grande échelle
!.                                                    typper(1) == 3. : perturbation de petite échelle
!.                                                    typper(1) == 4. : perturbations de petite et  grande échelle
!.                                                    typper(1) == 5. : pertubation comme n fois l'écart type sigma                         
!.                                        typper(2) : si typper == 2 ou 4 : graine pour le tirage aléatoire 
!.                                                    si typper == 5 : facteur n
!>     lambda_gw        : <pm_reel>                                    longueur d'onde (en km) utilisée pour les perturbations petite échelle
!                                                                      des modèles EMCD 42 et EMCD 43 dans les cas typper == 3 ou 4.
!>     ikle             : <integer,DIM=(6)>                            tableau d'indicateurs déterminant les type de modèles utilisés:
!.                                        ikle(1) = potentiel lunaire: 0 => non ; 1 => oui      
!.                                        ikle(2) = potentiel solaire: 0 => non ; 1 => oui      
!.                                        ikle(3) = frottement atmosphérique: 0 => non ; 1 => activité solaire réelle ; 2 => standard 
!.                                                                            3 => fichier                                                 
!.                                        ikle(4) = pression de radiation solaire: 0 => non ; 1 => oui   
!.                                        ikle(5) = Type d'éphémérides : 1 =>Tchebytchev, 2=> analytique
!>     nzo              : <integer>                                    degré des zonaux    
!>     nte              : <integer>                                    degré des tesseraux     
!>     typemod          : <integer>                                    type de modèle pour le modèle d'atmosphère martien Russe
!.                                         => 1 : modèle nominal
!.                                         => 2 : modèle maximal
!.                                         => 3 : modèle minimal     
!>     scena            : <integer>                                    type de scénario pour le modèle d'atmosphère martien européen (1=poussiereux, 2=clair)     
!>     dateref_actsol   : <integer>                                    date de référence données observées/prédites
!>     mode_avance_actsol: <integer>                                   flag mode COMPAS : 0=> pas de date de référence; 1=> date de référence
!>     modpot           : <LEN=80>                                     
!>     modatm           : <LEN=80>                                     nom du modèle d'atmosphère    
!>     ficept           : <LEN=80>                                     nom du fichier épéhémrides pour le troisième corps    
!>     ficactsol        : <LEN=80>                                     nom du fichier d'activite solaire
!>     modvent          : <LEN=80>                                     
!
!: PS_STR_CARACTERISTIQUES : structure définissant les caractéristiques du véhicule
!>     SEQUENCE         : <>                                           
!>     sx               : <pm_reel>                                    surface perpendiculaire à l'axe X du véhicule [m^2]
!>     sy               : <pm_reel>                                    surface perpendiculaire à l'axe Y du véhicule [m^2]
!>     sz               : <pm_reel>                                    surface perpendiculaire à l'axe Z du véhicule [m^2]
!>     st               : <pm_reel>                                    surface transverse de révolution du véhicule [m^2]
!>     spx              : <pm_reel>                                    surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>     spy              : <pm_reel>                                    surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>     spz              : <pm_reel>                                    surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>     xm               : <pm_reel>                                    masse du véhicule (kg)
!>     cp               : <pm_reel>                                    coefficient multiplicatif pour la pression de radiation solaire
!>     ka               : <pm_reel>                                    coefficient multiplicatif pour la pression de radiation solaire
!>     ks               : <pm_reel>                                    coefficient de spécularité pour la pression de radiation solaire
!>     kd               : <pm_reel>                                    coefficient de diffusion pour la pression de radiation solaire
!>     cf               : <pm_reel>                                    coefficient multiplicatif pour les forces aérodynamiques
!>     cd               : <pm_reel,DIM=(npcf,npcf)>                    tableau des CD
!>     cl               : <pm_reel,DIM=(npcf,npcf)>                    tableau des CL
!>     txcd             : <pm_reel,DIM=(npcf)>                         tableau des abscisses des CD
!>     tycd             : <pm_reel,DIM=(npcf)>                         tableau des ordonnées des CD
!>     txcl             : <pm_reel,DIM=(npcf)>                         tableau des abscisses des CL
!>     tycl             : <pm_reel,DIM=(npcf)>                         tableau des ordonnées des CL
!>     forme            : <integer>                                    forme du véhicule
!>     typcf            : <integer>                                    type de coefficients de frottement:
!.                                                      1 => tabulés en altitude
!.                                                      2 => constants
!.                                                      3 => lus dans un fichier AERO
!>     typcf_alt        : <integer>                                    
!>     nxcd             : <integer>                                    dimension du tableau des CD en abscisse
!>     nycd             : <integer>                                    dimension du tableau des CD en ordonnée
!>     nxcl             : <integer>                                    dimension du tableau des CL en abscisse
!>     nycl             : <integer>                                    dimension du tableau des CL en ordonnée
!>     ficaero          : <LEN=80>                                     nom du fichier AERO
!
!: PS_STR_BULLETIN : structure définissant le bulletin initial
!>     SEQUENCE         : <>                                           
!>     jjbul            
!>     secbul           : <pm_reel>                                    Nombre de secondes de la date exprimée en J J/sec
!>     param            : <pm_reel,DIM=(6)>                            paramètres de position/vitesse (dépend de iorb)
!>     rep              : <pm_reel,DIM=(10)>                           paramètres du repère
!>     vitrot           : <pm_reel>                                    vitesse de rotation de la planète (rad/s)
!>     obli             : <pm_reel>                                    obliquité
!>     polu             : <pm_reel>                                    angle U du pole vrai
!>     polv             : <pm_reel>                                    angle V du pole vrai
!>     rmu              : <pm_reel>                                    constante de gravition du corps
!>     requa_r          : <pm_reel>                                    rayon équatorial utilisé dans la définition du repère
!>     apla_r           : <pm_reel>                                    aplatissement utilisé dans la définition du repère
!>     requa            : <pm_reel>                                    
!>     apla             : <pm_reel>                                    
!>     jjbul            : <integer>                                    
!>     iorb             : <integer>                                    type de bulletin
!>     ech_temps_bul    : <integer>                                    échelle de temps dans laquelle est exprimée la date du bulletin
!>     cle_date         : <integer>                                    clé fixant la date de définition du repère à une date prédéfinie
!>     numpla           : <integer>                                    numéro de la planète
!>     numcen           : <integer>                                    numéro du corps central
!>     ficrepcorps      : <LEN=MSP_LONG_NOMFIC>                        
!>     listeficcorps    : <LEN=MSP_LONG_NOMFIC>                        
!
!: PS_STR_ATTITUDE : structure définissant les lois d'attitude
!>     SEQUENCE         : <>                                           
!>     secatt           : <pm_reel,DIM=(ps_nloi_att,ps_npts_att)>      nb de sec de la date exprimée en JJ/Sec              
!>     datt             : <pm_reel,DIM=(ps_nloi_att,ps_npts_att)>      tableau des dates relatives des lois d'attitude (s)
!>     ang1             : <pm_reel,DIM=(ps_nloi_att,ps_npts_att)>      tableau des angles de lacet ou incidence [rad]
!>     ang2             : <pm_reel,DIM=(ps_nloi_att,ps_npts_att)>      tableau des angles de tangage ou dérapage [rad]
!>     ang3             : <pm_reel,DIM=(ps_nloi_att,ps_npts_att)>      tableau des angles de roulis ou gite [rad]
!>     angp1            : <pm_reel,DIM=(ps_nloi_att)>                  tableau des vitesses angulaires en lacet ou incidence [rad/s]
!>     angp2            : <pm_reel,DIM=(ps_nloi_att)>                  tableau des vitesses angulaires en tangage ou dérapage [rad/s]
!>     angp3            : <pm_reel,DIM=(ps_nloi_att)>                  tableau des vitesses angulaires en roulis ou gite [rad/s]
!>     natt             : <integer>                                    nombre de lois d'attitude
!>     itypa            : <integer,DIM=(ps_nloi_att)>                  tableau des types de loi d'attitude
!>     iangle           : <integer,DIM=(ps_nloi_att)>                  
!>     irepa            : <integer,DIM=(ps_nloi_att)>                  tableau des types de repère d'attitude
!>     npatt            : <integer,DIM=(ps_nloi_att)>                  tableau des nombre de points de tabulation
!>     typdatatt        : <integer,DIM=(ps_nloi_att)>                  
!>     typdatvrot       : <integer,DIM=(ps_nloi_att)>                  
!>     jjatt            : <integer,DIM=(ps_nloi_att,ps_npts_att)>      JJ de la date exprimée en JJ/Sec
!>     ficatt           : <LEN=80,DIM=(ps_nloi_att)>                   nom du fichier d'attitude
!
!: PS_STR_PROPULSION : structure définissant les lois de propulsion
!>     SEQUENCE         : <>                                           
!>     sectimp          : <pm_reel,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des sec des dates de poussées exprimées en JJ/sec
!>     timp             : <pm_reel,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des dates de poussées (sec)
!>     deltav           : <pm_reel,DIM=(ps_nloi_propu)>                tableau des incréments de vitesse (poussées impulsionnelles)
!>     omtab            : <pm_reel,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des directions de poussée dans le plan véhicule (rad)
!>     omptab           : <pm_reel,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des directions de poussée hors du  plan véhicule (rad)
!>     fptab            : <pm_reel,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des niveaux de poussée (poussées continues)
!>     xisp             : <pm_reel,DIM=(ps_nloi_propu)>                tableau des impulsions spécifiques (poussées continues)
!>     paspro           : <pm_reel,DIM=(ps_nloi_propu)>                tableau des pas d'intégration lors de la poussée (poussées continues)
!>     xmerg            : <pm_reel,DIM=(ps_nloi_propu)>                tableau des masses d'ergols dépensées ou à dépenser
!>     parman           : <pm_reel,DIM=(ps_nloi_propu,6)>              tableau des nouvelles coordonnées (poussées "bulletin")
!>     brep             : <pm_reel,DIM=(ps_nloi_propu,10)>             tableau des nouveaux repères (poussées "bulletin")
!>     bobli            : <pm_reel,DIM=(ps_nloi_propu)>                tableau des obliquités (poussées "bulletin")
!>     bpolu            : <pm_reel,DIM=(ps_nloi_propu)>                tableau des angles U du pole vrai (poussées "bulletin")
!>     bpolv            : <pm_reel,DIM=(ps_nloi_propu)>                tableau des angles V du pole vrai (poussées "bulletin")
!>     brequa_r         : <pm_reel,DIM=(ps_nloi_propu)>                tableau des rayons équatoriaux (poussées "bulletin")
!>     bapla_r          : <pm_reel,DIM=(ps_nloi_propu)>                tableau des aplatissements (poussées "bulletin")
!>     brequa           : <pm_reel,DIM=(ps_nloi_propu)>                
!>     bapla            : <pm_reel,DIM=(ps_nloi_propu)>                
!>     bvitrot          : <pm_reel,DIM=(ps_nloi_propu)>                
!>     bmu              : <pm_reel,DIM=(ps_nloi_propu)>                
!>     bibul            : <integer,DIM=(ps_nloi_propu)>                tableau des types des nouveaux bulletins (poussées "bulletin")
!>     bnumpla          : <integer,DIM=(ps_nloi_propu)>                tableau des nouvelles planètes (poussées "bulletin")
!>     bech_temps_bul   : <integer,DIM=(ps_nloi_propu)>                tableau des nouvelles échelles de temps (poussées "bulletin")
!>     bcle_date        : <integer,DIM=(ps_nloi_propu)>                tableau des nouvelles clés de date (poussées "bulletin")
!>     npo              : <integer>                                    nombre de poussées
!>     iprop            : <integer,DIM=(ps_nloi_propu)>                tableau des types de poussées
!>     ntab             : <integer,DIM=(ps_nloi_propu)>                tableau des nombres de points de tabulation (poussées continues)
!>     typdatimp        : <integer,DIM=(ps_nloi_propu)>                
!>     typdatcont       : <integer,DIM=(ps_nloi_propu)>                
!>     jjtimp           : <integer,DIM=(ps_nloi_propu,ps_npts_propu)>  tableau des JJ des dates de poussées exprimées en JJ/sec
!>     ficpro           : <LEN=80,DIM=(ps_nloi_propu)>                 nom du fichier de propulsion
!
!: PS_STR_EVENEMENTS : structure définissant les événements
!>     SEQUENCE         : <>                                           
!>     deve             : <pm_reel,DIM=(nbeve)>                        tableau des dates des événements    
!>     ndeve            : <integer>                                    nombre d'évenements    
!>     ifore            : <integer>                                    format d'écriture des variables à écrire dans le fichier EVENT    
!>     neve             : <integer>                                    numéro de l'événement    
!>     aeve             : <LEN=80,DIM=(nbeve)>                         tableau des commentaires des événements    
!
!: PS_STR_SEPARATIONS : structure définissant les lois de séparations
!>     SEQUENCE         : <>                                           
!>     secsep           : <pm_reel,DIM=(ps_nsepa)>                     
!>     tsep             : <pm_reel,DIM=(ps_nsepa)>                     tableau des dates de séparation (s) de taille le nb de separations maxi (ps_netat) 
!>     sepx             : <pm_reel,DIM=(ps_nsepa)>                     tableau des nouvelles surfaces en x après séparation (m^2)       
!>     sepy             : <pm_reel,DIM=(ps_nsepa)>                     tableau des nouvelles surfaces en y après séparation (m^2)      
!>     sepz             : <pm_reel,DIM=(ps_nsepa)>                     tableau des nouvelles surfaces en z après séparation (m^2)       
!>     sept             : <pm_reel,DIM=(ps_nsepa)>                     tableau des nouvelles surfaces transverses après séparation (m^2)       
!>     seppx            : <pm_reel,DIM=(ps_nsepa)>                     nouvelle surface des panneaux solaires perpendiculaire à l'axe X du véhicule [m^2]
!>     seppy            : <pm_reel,DIM=(ps_nsepa)>                     nouvelle surface des panneaux solaires perpendiculaire à l'axe Y du véhicule [m^2]
!>     seppz            : <pm_reel,DIM=(ps_nsepa)>                     nouvelle surface des panneaux solaires perpendiculaire à l'axe Z du véhicule [m^2]
!>     xma              : <pm_reel,DIM=(ps_nsepa)>                     tableau des masses dépensées lors de la séparation (kg)       
!>     dvsep            : <pm_reel,DIM=(ps_nsepa)>                     tableau des DeltaV lors de la séparation (m/s)       
!>     omsep            : <pm_reel,DIM=(ps_nsepa)>                     tableau des directions de DeltaV dans le plan véhicule (rad)       
!>     ompsep           : <pm_reel,DIM=(ps_nsepa)>                     tableau des directions de DeltaV hors du plan véhicule (rad)       
!>     sepcp            : <pm_reel,DIM=(ps_nsepa)>                     tableau des coefficients multiplicatif pour la pression de radiation solaire
!>     sepka            : <pm_reel,DIM=(ps_nsepa)>                     tableau des coefficients d'absorbtion pour la pression de radiation solaire
!>     sepks            : <pm_reel,DIM=(ps_nsepa)>                     tableau des coefficients de spécularité pour la pression de radiation solaire
!>     sepkd            : <pm_reel,DIM=(ps_nsepa)>                     tableau des coefficients de diffusion pour la pression de radiation solaire
!>     sepcf            : <pm_reel,DIM=(ps_nsepa)>                     tableau des coefficients multiplicatif pour les forces aérodynamiques
!>     sepcd            : <pm_reel,DIM=(ps_nsepa)>                     tableau des tableaux des CD
!>     sepcl            : <pm_reel,DIM=(ps_nsepa)>                     tableau des tableaux des CL
!>     tymasse          : <integer,DIM=(ps_nsepa)>                     tableau des type de masse (delta ou masse apres separation)
!>     typcf            : <integer,DIM=(ps_nsepa)>                     tableau des type de frottement aero apres separation
!>     typcf_alt        : <integer,DIM=(ps_nsepa)>                     
!>     sepf             : <integer,DIM=(ps_nsepa)>                     tableau des nouvelles formes du véhicule après séparation       
!>     nsep             : <integer>                                    Nombre de separations
!>     typdat           : <integer,DIM=(ps_nsepa)>                     
!>     jjsep            : <integer,DIM=(ps_nsepa)>                     
!>     sepficaero       : <LEN=80,DIM=(ps_nsepa)>                      Fichier AERO apres separation (si typcf == 3)
!
!$Global
!
!>  ps_don_bul   : <PS_STR_BULLETIN>          structure contenant les données du bulletin initial
!>  ps_don_car   : <PS_STR_CARACTERISTIQUES>  structure contenant les données du véhicule
!>  ps_don_mod   : <PS_STR_MODELES>           structure contenant les données des modèles
!>  ps_don_int   : <PS_STR_INTEGRATION>       structure contenant les données d'intégration
!>  ps_don_sep   : <PS_STR_SEPARATIONS>       structure contenant les données des lois de séparation
!>  ps_don_ati   : <PS_STR_ATTITUDE>          structure contenant les données des lois d'attitude
!>  ps_don_pro   : <PS_STR_PROPULSION>        structure contenant les données des lois de propulsion
!>  iplanet      : <integer>                  
!>  ps_don_ecr   : <PS_STR_ECRITURE>          structure contenant les données pour écriture des résultats
!>  ps_don_eve   : <PS_STR_EVENEMENTS>        structure contenant les données gérant les événements
!>  kbul         : <integer>                  indice indiquant si on (ré)initialise le bulletin (si /= 0)
!>  kcar         : <integer>                  indice indiquant si on (ré)initialise le véhicule (si /= 0)
!>  kati         : <integer>                  indice indiquant si on (ré)initialise les lois d'attitude (si /= 0)
!>  ksep         : <integer>                  indice indiquant si on (ré)initialise les lois de séparation (si /= 0)
!>  kpro         : <integer>                  indice indiquant si on (ré)initialise les lois de propulsion (si /= 0)
!>  kint         : <integer>                  indice indiquant si on (ré)initialise les pas d'intégration (si /= 0)
!>  kmod         : <integer>                  indice indiquant si on (ré)initialise les modèles (si /= 0)
!$Common
!#V
!- common /pbul/
!- common /pcar/
!- common /pmod/
!- common /pint/
!- common /psep/
!- common /pati/
!- common /ppro/
!- common /ppla/
!- common /pecr/
!- common /peve/
!- common /pini/
!#
!
!$Routines
!- psconv_msp_vers_psimu
!- psconv_psimu_vers_msp
!- psconv_bul_psbul
!- psconv_psbul_bul
!- psconv_pro_pspro
!- psconv_pspro_pro
!- psconv_sep_pssep
!- psconv_pssep_sep
!- psconv_ati_psati
!- psconv_psati_ati
!- psconv_vehicule_pscar
!- psconv_pscar_vehicule
!- psconv_mod_psmod
!- psconv_psmod_mod
!- psconv_psmod_vent
!- psconv_vent_psmod
!- ps_analyser_maj_vetat
!- psi_analyse_maj_kep
!- psi_analyse_maj_posvit_geo
!- psi_analyse_maj_forces
!- psi_analyse_maj_ris_rts
!
!$Fonctions
!- ps_creer_strinteg
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- MECASPA
!- MSP_INTEGRATOR_DEF
!- ps_generalites
!- ps_propulsion
!- ps_caracteristiques
!- ps_attitude
!- EPHEM
!#
!
!$Interface
!> psconv_msp_vers_psimu :  psconv_bul_psbul, psconv_pro_pspro, 
!                           psconv_sep_pssep, psconv_ati_psati, psconv_mod_psmod, 
!                           psconv_vehicule_pscar, psconv_vent_psmod
!> psconv_psimu_vers_msp :  psconv_psbul_bul, psconv_pspro_pro, 
!                           psconv_pssep_sep, psconv_psati_ati, psconv_psmod_mod, 
!                           psconv_pscar_vehicule, psconv_psmod_vent
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use MSP_INTEGRATOR_DEF, only : msp_integrator, msp_creer_integrator, msp_consulter_integrator, msp_modifier_integrator
   use ps_generalites , only : ps_nloi_propu,ps_npts_propu,ps_nloi_att,ps_npts_att,ps_nsepa,npcf,nvmax,nbeve
   use ps_calcul_forces, only : PSI_METHODE_EPH_TCHEMAD

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_interface_psimu_don.F90 368 2013-02-19 14:43:59Z aadt $'


   type PS_STR_BULLETIN

      SEQUENCE

      real (KIND=pm_reel) :: secbul
      real (KIND=pm_reel) :: param(6)
      real (KIND=pm_reel) :: rep(10)
      real (KIND=pm_reel) :: vitrot
      real (KIND=pm_reel) :: obli
      real (KIND=pm_reel) :: polu
      real (KIND=pm_reel) :: polv
      real (KIND=pm_reel) :: rmu
      real (KIND=pm_reel) :: requa_r
      real (KIND=pm_reel) :: apla_r
      real (KIND=pm_reel) :: requa
      real (KIND=pm_reel) :: apla

      integer             :: jjbul
      integer             :: iorb
      integer             :: ech_temps_bul
      integer             :: cle_date
      integer             :: numpla
      integer             :: numcen

      character(len=MSP_LONG_NOMFIC) :: ficrepcorps
      character(len=MSP_LONG_NOMFIC) :: listeficcorps
      

   end type PS_STR_BULLETIN

   type PS_STR_CARACTERISTIQUES

      SEQUENCE

      real (KIND=pm_reel) :: sx,sy,sz,st,spx,spy,spz
      real (KIND=pm_reel) :: xm
      real (KIND=pm_reel) :: cp
      real (KIND=pm_reel) :: ka,ks,kd
      real (KIND=pm_reel) :: cf
      real (KIND=pm_reel), dimension(npcf,npcf) :: cd,cl
      real (KIND=pm_reel), dimension(npcf)      :: txcd,tycd
      real (KIND=pm_reel), dimension(npcf)      :: txcl,tycl

      integer :: forme
      integer :: typcf
      integer :: typcf_alt
      integer :: nxcd,nycd,nxcl,nycl

      character(LEN=80)   :: ficaero

   end type PS_STR_CARACTERISTIQUES

   type PS_STR_MODELES

      SEQUENCE

      real (KIND=pm_reel) :: flu
      real (KIND=pm_reel) :: app
      real (KIND=pm_reel), dimension(8) :: deltat
      real (KIND=pm_reel), dimension(7) :: param_atm_exp
      real (KIND=pm_reel), dimension(2) :: typper
      real (KIND=pm_reel) :: lambda_gw

      integer, dimension(6) :: ikle
      integer             :: nzo
      integer             :: nte
      integer             :: typemod
      integer             :: scena
      integer             :: dateref_actsol
      integer             :: mode_avance_actsol
			     
      character(LEN=80)   :: modpot 
      character(LEN=80)   :: modatm
      character(LEN=80)   :: ficept
      character(LEN=80)   :: ficactsol
      character(LEN=80)   :: modvent

   end type PS_STR_MODELES

   type PS_STR_INTEGRATION

      SEQUENCE
      real (KIND=pm_reel) :: h1
      real (KIND=pm_reel) :: h2
      real (KIND=pm_reel) :: hstop
      real (KIND=pm_reel) :: h3
      ! Structure MSP_INTEGRATOR définie (provisoirement) dans la GSLIB (DM-ID 702 / DM-ID 748)
      type(MSP_INTEGRATOR):: pinteg0
      type(MSP_INTEGRATOR):: pinteg1
      type(MSP_INTEGRATOR):: pinteg2
      type(MSP_INTEGRATOR):: pinteg3

      real (KIND=pm_reel) :: rdatmax
      real (KIND=pm_reel) :: tmax
      real (KIND=pm_reel) :: rdatbul
      
      integer             :: type_alt
      integer             :: idatmax
      integer             :: typdat
      integer             :: idatbul
   end type PS_STR_INTEGRATION

   type PS_STR_SEPARATIONS

      SEQUENCE

      !ps_nsepa est le nombre maximum de separations

      real (KIND=pm_reel), dimension(ps_nsepa) :: secsep
      real (KIND=pm_reel), dimension(ps_nsepa) :: tsep
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepx
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepy
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepz
      real (KIND=pm_reel), dimension(ps_nsepa) :: sept
      real (KIND=pm_reel), dimension(ps_nsepa) :: seppx
      real (KIND=pm_reel), dimension(ps_nsepa) :: seppy
      real (KIND=pm_reel), dimension(ps_nsepa) :: seppz
      real (KIND=pm_reel), dimension(ps_nsepa) :: xma
      real (KIND=pm_reel), dimension(ps_nsepa) :: dvsep
      real (KIND=pm_reel), dimension(ps_nsepa) :: omsep
      real (KIND=pm_reel), dimension(ps_nsepa) :: ompsep
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepcp
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepka
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepks
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepkd
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepcf
      real (KIND=pm_reel) , dimension(ps_nsepa):: sepcd
      real (KIND=pm_reel), dimension(ps_nsepa) :: sepcl

      integer, dimension(ps_nsepa) :: tymasse
      integer, dimension(ps_nsepa) :: typcf
      integer, dimension(ps_nsepa) :: typcf_alt
      integer, dimension(ps_nsepa) :: sepf
      integer :: nsep
      integer, dimension(ps_nsepa) :: typdat
      integer, dimension(ps_nsepa) :: jjsep

      character(LEN=80), dimension(ps_nsepa) :: sepficaero

   end type PS_STR_SEPARATIONS

   type PS_STR_ATTITUDE

      SEQUENCE

      real (KIND=pm_reel), dimension(ps_nloi_att,ps_npts_att) :: secatt
      real (KIND=pm_reel), dimension(ps_nloi_att,ps_npts_att) :: datt
      real (KIND=pm_reel), dimension(ps_nloi_att,ps_npts_att) :: ang1
      real (KIND=pm_reel), dimension(ps_nloi_att,ps_npts_att) :: ang2
      real (KIND=pm_reel), dimension(ps_nloi_att,ps_npts_att) :: ang3
      real (KIND=pm_reel), dimension(ps_nloi_att)             :: angp1
      real (KIND=pm_reel), dimension(ps_nloi_att)             :: angp2
      real (KIND=pm_reel), dimension(ps_nloi_att)             :: angp3

      integer                                                 :: natt
      integer, dimension(ps_nloi_att)                         :: itypa
      integer, dimension(ps_nloi_att)                         :: iangle
      integer, dimension(ps_nloi_att)                         :: irepa
      integer, dimension(ps_nloi_att)                         :: npatt
      integer, dimension(ps_nloi_att)                         :: typdatatt,typdatvrot
      integer, dimension(ps_nloi_att,ps_npts_att)             :: jjatt

      character(LEN=80), dimension(ps_nloi_att)               :: ficatt

   end type PS_STR_ATTITUDE

   type PS_STR_PROPULSION

      SEQUENCE

      real (KIND=pm_reel), dimension(ps_nloi_propu,ps_npts_propu) :: sectimp
      real (KIND=pm_reel), dimension(ps_nloi_propu,ps_npts_propu) :: timp
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: deltav
      real (KIND=pm_reel), dimension(ps_nloi_propu,ps_npts_propu) :: omtab
      real (KIND=pm_reel), dimension(ps_nloi_propu,ps_npts_propu) :: omptab
      real (KIND=pm_reel), dimension(ps_nloi_propu,ps_npts_propu) :: fptab
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: xisp
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: paspro
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: xmerg
      real (KIND=pm_reel), dimension(ps_nloi_propu,6)             :: parman
      real (KIND=pm_reel), dimension(ps_nloi_propu,10)            :: brep
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bobli
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bpolu
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bpolv
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: brequa_r
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bapla_r
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: brequa
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bapla
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bvitrot
      real (KIND=pm_reel), dimension(ps_nloi_propu)               :: bmu
              
      integer, dimension(ps_nloi_propu)                           :: bibul
      integer, dimension(ps_nloi_propu)                           :: bnumpla
      integer, dimension(ps_nloi_propu)                           :: bech_temps_bul
      integer, dimension(ps_nloi_propu)                           :: bcle_date
      integer                                                     :: npo
      integer, dimension(ps_nloi_propu)                           :: iprop
      integer, dimension(ps_nloi_propu)                           :: ntab
      integer, dimension(ps_nloi_propu)                           :: typdatimp,typdatcont
      integer, dimension(ps_nloi_propu,ps_npts_propu)             :: jjtimp

      character(LEN=80), dimension(ps_nloi_propu)                 :: ficpro

   end type PS_STR_PROPULSION

   type PS_STR_ECRITURE

      SEQUENCE

      real (KIND=pm_reel) :: pas_sortie

      integer             :: nvarw
      integer             :: iresw(nvmax)
      integer             :: indw
      integer             :: iforw
      integer             :: imodout
      integer             :: ipas
      integer             :: ivarmax

      character(LEN=10)   :: etatn(nvmax)

      integer             :: itypsort

   end type PS_STR_ECRITURE

   type PS_STR_EVENEMENTS

      SEQUENCE

      real (KIND=pm_reel) :: deve(nbeve)

      integer             :: ndeve
      integer             :: ifore
      integer             :: neve

      character(LEN=80)   :: aeve(nbeve)

   end type PS_STR_EVENEMENTS

   common /pbul/ ps_don_bul
   type (PS_STR_BULLETIN) :: ps_don_bul
   common /pcar/ ps_don_car
   type (PS_STR_CARACTERISTIQUES) :: ps_don_car
   common /pmod/ ps_don_mod
   type (PS_STR_MODELES) :: ps_don_mod
   common /pint/ ps_don_int
   type (PS_STR_INTEGRATION) :: ps_don_int
   common /psep/ ps_don_sep
   type (PS_STR_SEPARATIONS) :: ps_don_sep
   common /pati/ ps_don_ati
   type (PS_STR_ATTITUDE) :: ps_don_ati
   common /ppro/ ps_don_pro
   type (PS_STR_PROPULSION) :: ps_don_pro

   common /ppla/ iplanet
   integer :: iplanet
   common /pecr/ ps_don_ecr
   type (PS_STR_ECRITURE) :: ps_don_ecr
   common /peve/ ps_don_eve
   type (PS_STR_EVENEMENTS) :: ps_don_eve

   ! Common uniquement utilisé pour l'interface Fortran77
   common /pini/ kbul,kcar,kati,ksep,kpro,kint,kmod
   integer :: kbul,kcar,kati,ksep,kpro,kint,kmod

   interface psconv_msp_vers_psimu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_msp_vers_psimu
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_msp_vers_psimu (bul,psbul)
!.    type(MSP_BULLETIN) :: bul
!.    type(PS_STR_BULLETIN) :: psbul
!
!  call psconv_msp_vers_psimu (pro,datbul,pspro,[datprec])
!.    type(MSP_SCENARIO_LOI) :: pro
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_PROPULSION) :: pspro
!.    real (KIND=pm_reel) :: datprec
!
!  call psconv_msp_vers_psimu (sep,datbul,pssep)
!.    type(MSP_SCENARIO_LOI) :: sep
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_SEPARATIONS) :: pssep
!
!  call psconv_msp_vers_psimu (ati,datbul,psati)
!.    type(MSP_SCENARIO_LOI) :: ati
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_ATTITUDE) :: psati
!
!  call psconv_msp_vers_psimu (mod,psmod)
!.    type (MSP_MODELE) :: mod
!.    type (PS_STR_MODELES) :: psmod
!
!  call psconv_msp_vers_psimu (vehicule,pscar)
!.    type (MSP_VEHICULE) :: vehicule
!.    type (PS_STR_CARACTERISTIQUES) :: pscar
!
!  call psconv_msp_vers_psimu(modvent,psmod)
!.    type (MSP_MODVENT) :: modvent
!.    type (PS_STR_MODELES) :: psmod
!
!$Procedures
!- psconv_bul_psbul
!- psconv_pro_pspro
!- psconv_sep_pssep
!- psconv_ati_psati
!- psconv_mod_psmod
!- psconv_vehicule_pscar
!- psconv_vent_psmod
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure psconv_bul_psbul
      module procedure psconv_pro_pspro
      module procedure psconv_sep_pssep
      module procedure psconv_ati_psati
      module procedure psconv_mod_psmod
      module procedure psconv_vehicule_pscar
      module procedure psconv_vent_psmod
   end interface

   interface psconv_psimu_vers_msp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_psimu_vers_msp
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_psimu_vers_msp (psbul,bul)
!.    type(PS_STR_BULLETIN) :: psbul
!.    type(MSP_BULLETIN) :: bul
!
!  call psconv_psimu_vers_msp (pspro,datbul,pro)
!.    type(PS_STR_PROPULSION) :: pspro
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: pro
!
!  call psconv_psimu_vers_msp (pssep,datbul,sep)
!.    type(PS_STR_SEPARATIONS) :: pssep
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: sep
!
!  call psconv_psimu_vers_msp (psati,datbul,ati)
!.    type(PS_STR_ATTITUDE) :: psati
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: ati
!
!  call psconv_psimu_vers_msp (psmod,mod)
!.    type (PS_STR_MODELES) :: psmod
!.    type (MSP_MODELE) :: mod
!
!  call psconv_psimu_vers_msp (pscar,vehicule)
!.    type (PS_STR_CARACTERISTIQUES) :: pscar
!.    type (MSP_VEHICULE) :: vehicule
!
!  call psconv_psimu_vers_msp(psmod,modvent)
!.    type (PS_STR_MODELES) :: psmod
!.    type (MSP_MODVENT) :: modvent
!
!$Procedures
!- psconv_psbul_bul
!- psconv_pspro_pro
!- psconv_pssep_sep
!- psconv_psati_ati
!- psconv_psmod_mod
!- psconv_pscar_vehicule
!- psconv_psmod_vent
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure psconv_psbul_bul
      module procedure psconv_pspro_pro
      module procedure psconv_pssep_sep
      module procedure psconv_psati_ati
      module procedure psconv_psmod_mod
      module procedure psconv_pscar_vehicule
      module procedure psconv_psmod_vent
   end interface

   contains

   subroutine psconv_bul_psbul (bul,psbul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_bul_psbul
!
!$Resume
!  Conversion d'une structure de type MSP_BULLETIN à une structure de type PS_STR_BULLETIN
!
!$Description
!  Conversion d'une structure de type MSP_BULLETIN à une structure de type PS_STR_BULLETIN
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_bul_psbul (bul,psbul)
!.    type(MSP_BULLETIN) :: bul
!.    type(PS_STR_BULLETIN) :: psbul
!
!$Arguments
!>E     bul    :<MSP_BULLETIN>      bulletin de type MSP_BULLETIN
!>S     psbul  :<PS_STR_BULLETIN>   bulletin de type PS_STR_BULLETIN
!
!$Common
!
!$Routines
!- MSP_consulter_bulletin
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

      type(MSP_BULLETIN), intent(IN) :: bul
      type(PS_STR_BULLETIN), intent(OUT) :: psbul

      type(tm_jour_sec)                  :: datebul
 

      call MSP_consulter_bulletin (bul,&
           datbul_js=datebul,&
           ech_temps_bul=psbul%ech_temps_bul,&
           param=psbul%param(:),&
           rep=psbul%rep(:),&
           vitrot=psbul%vitrot,&
           mu=psbul%rmu,&
           requa_r=psbul%requa_r,&
           apla_r=psbul%apla_r,&
           requa=psbul%requa,&
           apla=psbul%apla,&
           iorb=psbul%iorb,&
           cle_date=psbul%cle_date,&
           planete=psbul%numpla,&
           corcen=psbul%numcen,&
           pole_u=psbul%polu,&
           pole_v=psbul%polv,&
           obli=psbul%obli)
     
      psbul%jjbul=datebul%jour
      psbul%secbul=datebul%sec

      if ( MSP_gen_messages("psconv_bul_psbul") ) return

   end subroutine psconv_bul_psbul

   subroutine psconv_psbul_bul (psbul,bul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_psbul_bul
!
!$Resume
!  Conversion d'une structure de type PS_STR_BULLETIN à une structure de type MSP_BULLETIN 
!
!$Description
!  Conversion d'une structure de type PS_STR_BULLETIN à une structure de type MSP_BULLETIN 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_psbul_bul (psbul,bul)
!.    type(PS_STR_BULLETIN) :: psbul
!.    type(MSP_BULLETIN) :: bul
!
!$Arguments
!>E     psbul  :<PS_STR_BULLETIN>   bulletin de type PS_STR_BULLETIN
!>S     bul    :<MSP_BULLETIN>      bulletin de type MSP_BULLETIN
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
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(PS_STR_BULLETIN), intent(IN) :: psbul
      type(MSP_BULLETIN), intent(OUT)   :: bul

      ! variables locales 
      type(tm_jour_sec)                 :: datbul_js
      type(tm_code_retour)              :: code_retour
      
      real(kind=pm_reel) :: datbul_init

      ! conversion 
      datbul_js%jour=psbul%jjbul
      datbul_js%sec=psbul%secbul

      call md_joursec_jourfrac(datbul_js,datbul_init,code_retour)

      bul = MSP_creer_bulletin (datbul=datbul_init,iorb=psbul%iorb,param=psbul%param(:),&
           typrep=int(psbul%rep(1)), rep=psbul%rep(:),ech_temps_bul=psbul%ech_temps_bul,&
           datbul_js=datbul_js,cle_date=psbul%cle_date, &
           planete=psbul%numpla,corcen=psbul%numcen,vrot=psbul%vitrot,&
           pole_u=psbul%polu,pole_v=psbul%polv,requa_r=psbul%requa_r,requa=psbul%requa, &
           apla_r=psbul%apla_r,apla=psbul%apla, mu=psbul%rmu,obli=psbul%obli)
      if ( MSP_gen_messages("psconv_psbul_bul") ) return

   end subroutine psconv_psbul_bul

   subroutine psconv_pro_pspro (pro,datbul,pspro,datprec)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_pro_pspro
!
!$Resume
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_PROPULSION.
!
!$Description
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_PROPULSION.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_pro_pspro (pro,datbul,pspro,[datprec])
!.    type(MSP_SCENARIO_LOI) :: pro
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_PROPULSION) :: pspro
!.    real (KIND=pm_reel) :: datprec
!
!$Arguments
!>E/S   pro      :<MSP_SCENARIO_LOI>    lois de propulsion de type MSP_SCENARIO_LOI
!>E     datbul   :<pm_reel>             date du bulletin en JJ CNES
!>S     pspro    :<PS_STR_PROPULSION>   lois de propulsion de type PS_STR_PROPULSION
!>[E]   datprec  :<pm_reel>             précision sur les dates (sec) (par défaut la milli-seconde à savoir 0.001)
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_premiere_loi
!- MSP_consulter_ptr_loi
!- MSP_consulter_impulsion
!- md_joursec_jourfrac
!- MSP_consulter_poussee_continue
!- MSP_consulter_poussee_bulletin
!- MSP_consulter_bulletin
!- MSP_loi_suivante
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

      ! Arguments
      !==========
      type(MSP_SCENARIO_LOI), intent(INOUT) :: pro
      real(kind=pm_reel), intent(IN)     :: datbul
      type(PS_STR_PROPULSION), intent(OUT) :: pspro
      real (KIND=pm_reel), intent(IN), OPTIONAL :: datprec

      ! Variables locales
      !==================

      type(MSP_IMPULSION), pointer :: loi_imp
      type(MSP_POUSSEE_CONTINUE), pointer :: loi_cont
      type(MSP_POUSSEE_BULLETIN), pointer :: loi_bul
      type(MSP_BULLETIN) :: bul
      character(LEN=8) :: pvar
      integer :: i, sc_type, sc_nloi, typloi, typdat, ntab, iloi, dirref_tmp
      real(kind=pm_reel) :: tbul,delta_date,sc_date_ref
      real(kind=pm_reel) :: date, p_datbul, datdeb, datprec_tmp,date_jjfrac
      real(kind=pm_reel), dimension(:), pointer :: tab_poussee
      real(kind=pm_reel), dimension(:), pointer :: tab_omega
      real(kind=pm_reel), dimension(:), pointer :: tab_omegap
      real(KIND=PM_REEL), dimension(:), pointer :: tab_dates
      type(tm_jour_sec),  dimension(:), pointer :: tab_dates_js
      type(tm_jour_sec)    :: datjs
      type(tm_code_retour) :: code_retour

      integer :: stat   ! status du deallocate

      nullify(loi_imp)
      nullify(loi_cont)
      nullify(loi_bul)
      nullify(tab_poussee)
      nullify(tab_omega)
      nullify(tab_omegap)
      nullify(tab_dates)
      nullify(tab_dates_js)

      if ( PRESENT(datprec) ) then
!/  Lecture de la precision sur les dates (0.001 par defaut)
        datprec_tmp = datprec
      else
        datprec_tmp = 0.001_pm_reel
      endif

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (pro, type=sc_type, nloi=sc_nloi, &
           date_ref=sc_date_ref)
      if ( MSP_gen_messages("psconv_pro_pspro") ) return

      if ( sc_type /= MSP_ENUM_PROPULSION ) then
!/  Si la loi de  scenario n'est pas de type propulsion
         call MSP_signaler_message (cle_mes="PS_CONV_001")
         return
      endif

      if ( sc_nloi > ps_nloi_propu ) then
!/  Test sur le nombre de lois du scenario 
         write(pvar,*) ps_nloi_propu
         call MSP_signaler_message (cle_mes="PS_CONV_002",partie_variable=pvar)
         return
      endif

      delta_date =(sc_date_ref - datbul)*86400._pm_reel 

!/  Boucle sur les lois du scénario:
      pspro%npo = 0

      !/ init de la boucle : positionnement du pointeur loi courante sur la 1ere loi
      call MSP_premiere_loi(pro)
      if ( MSP_gen_messages("psconv_pro_pspro") ) return

      do iloi = 1 , sc_nloi

         !/  Extraction du type de la loi courante
         typloi = MSP_type_loi (scenario=pro)
         if ( MSP_gen_messages("psconv_pro_pspro") ) return
         pspro%npo = pspro%npo + 1
         select case ( typloi )
            case (MSP_ENUM_LOI_IMP)
!/  Pour une loi de propulsion de type impulsionnelle (MSP_IMPULSION)

!/  extraction de la structure MSP_IMPULSION dans la structure loi de scenario (MSP_SCENARIO_LOI)
               call MSP_consulter_ptr_loi (pro, loi_imp=loi_imp)
               if ( MSP_gen_messages("psconv_pro_pspro") ) return

!/  Lecture des donnees dans la structure loi poussee impulsionnelle (MSP_IMPULSION)
               call MSP_consulter_impulsion (loi_imp, typdat=typdat, date=date, &
                    deltav=pspro%deltav(pspro%npo), dirref=dirref_tmp, &
                    omega=pspro%omtab(pspro%npo,1), omegap=pspro%omptab(pspro%npo,1), &
                    merg=pspro%xmerg(pspro%npo), date_js=datjs)
               if ( MSP_gen_messages("psconv_pro_pspro") ) return

               pspro%iprop(pspro%npo)  = 1 + (dirref_tmp+1)*100

               call md_joursec_jourfrac(datjs,date_jjfrac,code_retour)

               if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then
!/  Si la date de poussee est de type absolue (date en jour julien) --> transformation en duree (s)
                  pspro%timp(pspro%npo,1) = (date_jjfrac - datbul)*86400._pm_reel
               
               else if ( typdat == MSP_ENUM_LOI_ABSOREL ) then
!/  Si la date de poussee est de type absolue relative (date de saisie + 
!/  duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin
                  pspro%timp(pspro%npo,1) = (date_jjfrac - datbul)*86400._pm_reel + delta_date
               
               else
!/  Si la date de poussee est de type relative (duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin
                  pspro%timp(pspro%npo,1) = date + delta_date
               endif

            case (MSP_ENUM_LOI_CONT)
!/  Pour une loi de propulsion de type continue (MSP_POUSSEE_CONTINUE)

!/  extraction de la structure loi poussee continue dans la structure scenario (MSP_SCENARIO_LOI)             
               call MSP_consulter_ptr_loi (pro, loi_cont=loi_cont)
               if ( MSP_gen_messages("psconv_pro_pspro") ) return

!/  Lecture des donnees dans la structure MSP_POUSSEE_CONTINUE
               call MSP_consulter_poussee_continue (loi_cont=loi_cont, ntab=ntab, &
                    typdat=typdat, dates=tab_dates, dates_js=tab_dates_js,&
                    poussee=tab_poussee, isp=pspro%xisp(pspro%npo), &
                    dirref=dirref_tmp, omega=tab_omega, omegap=tab_omegap,&
                    merg=pspro%xmerg(pspro%npo), pas=pspro%paspro(pspro%npo))

               if ( MSP_gen_messages("psconv_pro_pspro") ) return
               ! note : pas de desallocation memoire de loi_cont, 
               ! car il s'agit d'un pointeur sur la structure du scenario "pro"


               pspro%iprop(pspro%npo)  = 2 + (dirref_tmp+1)*100

               if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then

!/  Si la date de poussee est de type absolue (date en jour julien) -->
!  transformation en duree depuis de debut de la mission (s)
                  
                  do i = 1 , ntab
                     pspro%timp(pspro%npo,i) = (tab_dates(i) - datbul)*86400._pm_reel
                  enddo

               else if ( typdat == MSP_ENUM_LOI_ABSOREL ) then
!/  Si les dates des lois d'attitude sont de types absolu relatif (duree (s) depuis une date de saisie) 
!/   --> Prise en compte de la date absolue de reference puis ajout des durées tabulées 
                  call md_joursec_jourfrac(tab_dates_js(1),tab_dates(1),code_retour)
                  pspro%timp(pspro%npo,1) = tab_dates(1)*86400._pm_reel
                  pspro%timp(pspro%npo,2:ntab) = (tab_dates(1)- datbul)*86400._pm_reel+tab_dates(2:ntab)  
               else 
!/  Si la date de poussee est de type relative (durees depuis la date bulletin)
!/  --> Prise en compte de la date de reference du scenario par rapport a la date du bulletin
                  pspro%timp(pspro%npo,1:ntab) = tab_dates(1:ntab) + delta_date
               endif

!/  sauvegarde des donnees dans la structure PS_STR_PROPULSION
               pspro%ntab(pspro%npo)          = ntab
               pspro%fptab(pspro%npo,1:ntab)  = tab_poussee(1:ntab)
               pspro%omtab(pspro%npo,1:ntab)  = tab_omega(1:ntab)
               pspro%omptab(pspro%npo,1:ntab) = tab_omegap(1:ntab)

!/  Deallocation des tableaux dynamiques
               DEALLOCATE (tab_dates_js, stat=stat)
               DEALLOCATE (tab_dates, stat=stat)
               DEALLOCATE (tab_poussee, stat=stat)
               DEALLOCATE (tab_omega, stat=stat)
               DEALLOCATE (tab_omegap, stat=stat)

            case (MSP_ENUM_LOI_BUL)
!/  Pour une loi de propulsion de type bulletin (MSP_POUSSEE_BULLETIN)

!/  extraction de la structure loi MSP_POUSSEE_BULLETIN dans la structure scenario (MSP_SCENARIO_LOI)
               call MSP_consulter_ptr_loi (pro, loi_bul=loi_bul)
               if ( MSP_gen_messages("psconv_pro_pspro") ) return

!/  Lecture des donnees dans la structure MSP_POUSSEE_BULLETIN + extraction de la structure Bulletin
               call MSP_consulter_poussee_bulletin (loi_bul, typdat=typdat, &
                    datedeb=datdeb, bul=bul, merg=pspro%xmerg(pspro%npo))
               if ( MSP_gen_messages("psconv_pro_pspro") ) return

!/  Lecture des donnees dans la structure bulletin
               call MSP_consulter_bulletin (bul,datbul=p_datbul,&
                    ech_temps_bul=pspro%bech_temps_bul(pspro%npo),&
                    param=pspro%parman(pspro%npo,:),&
                    rep=pspro%brep(pspro%npo,:),&
                    iorb=pspro%bibul(pspro%npo),&
                    cle_date=pspro%bcle_date(pspro%npo),&
                    planete=pspro%bnumpla(pspro%npo),&
                    mu=pspro%bmu(pspro%npo), vitrot=pspro%bvitrot(pspro%npo), &
                    apla=pspro%bapla(pspro%npo), requa=pspro%brequa(pspro%npo), &
                    apla_r=pspro%bapla_r(pspro%npo), requa_r=pspro%brequa_r(pspro%npo), &
                    obli=pspro%bobli(pspro%npo),&
                    pole_v=pspro%bpolv(pspro%npo), pole_u=pspro%bpolu(pspro%npo))
               if ( MSP_gen_messages("psconv_bul_psbul") ) return
               pspro%iprop(pspro%npo)  = 3
!/ Si la date de poussee est de type absolue (date en jour julien) --> 
!/  transformation de la date en duree (s)
               if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then

                  pspro%timp(pspro%npo,1) = (datdeb - datbul)*86400._pm_reel
               else
!/  Si la date de poussee est de type relative (duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin
                  pspro%timp(pspro%npo,1) = datdeb + delta_date
               endif

!/  Transformation de la date issus du bulletin de poussee en duree
               tbul = (p_datbul - datbul)*86400._pm_reel
               ! Arrondi à la milli-seconde:
               tbul = real(int(tbul),KIND=pm_reel) + &
                    real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp
               pspro%timp(pspro%npo,2)     = tbul
         end select

         call MSP_loi_suivante(pro)
         if ( MSP_gen_messages("psconv_pro_pspro") ) return

      enddo

   end subroutine psconv_pro_pspro

   subroutine psconv_pspro_pro (pspro,datbul,pro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_pspro_pro
!
!$Resume
!  Conversion d'une structure de type PS_STR_PROPULSION à une structure de type MSP_SCENARIO_LOI.
!
!$Description
!  Conversion d'une structure de type PS_STR_PROPULSION à une structure de type MSP_SCENARIO_LOI.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_pspro_pro (pspro,datbul,pro)
!.    type(PS_STR_PROPULSION) :: pspro
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: pro
!
!$Arguments
!>E     pspro   :<PS_STR_PROPULSION>   lois de propulsion de type PS_STR_PROPULSION 
!>E     datbul  :<pm_reel>             date du bulletin en JJ CNES
!>E/S   pro     :<MSP_SCENARIO_LOI>    lois de propulsion de type MSP_SCENARIO_LOI
!
!$Common
!
!$Routines
!- MSP_ajouter_impulsion
!- MSP_ajouter_poussee_continue
!- MSP_signaler_message
!- MSP_ajouter_poussee_bulletin
!- pslecpro
!
!$Include
!
!$Module
!#V
!- ps_propulsion
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     use ps_propulsion

      implicit none

      ! Arguments
      !==========
      type(PS_STR_PROPULSION), intent(IN) :: pspro
      real(kind=pm_reel), intent(IN)      :: datbul
      type(MSP_SCENARIO_LOI), intent(INOUT) :: pro
      
      ! Variables locales
      !==================
      
      integer :: numloi, dirref_tmp, iprop_tmp
      type(MSP_BULLETIN) :: bul

      character(LEN=80)  :: ficpro
      real (KIND=pm_reel) :: paspro
      real (KIND=pm_reel) :: g0
      real (KIND=pm_reel), dimension(ps_npts_propu) :: date
      real (KIND=pm_reel), dimension(ps_nloi_propu,10) :: brep
      real (KIND=pm_reel) :: datebul
      character (LEN=256) :: parvar

      ! Initialisations
      data g0 / 9.80665_pm_reel /

!/  Creation d'un scenario de type propulsion
      pro = MSP_creer_scenario (MSP_ENUM_PROPULSION,date_ref=datbul,nom="")
      if ( MSP_gen_messages("psconv_pspro_pro") ) return

!/ Initialisation du nombre de lois
      nloi_scenar_pro(iveh) = pspro%npo

!/  Boucle sur les lois du scénario:
      numloi = 1
      do while ( numloi <= pspro%npo )
         dirref_tmp = pspro%iprop(numloi)/100
         iprop_tmp  = pspro%iprop(numloi) - dirref_tmp*100
         dirref_tmp = dirref_tmp - 1
         select case ( iprop_tmp )

            case (1)
               ! test du type de date, cas absolu
               if (pspro%typdatimp(numloi).eq.1) then                   
                  date(1) = &
                       (pspro%jjtimp(numloi,1)-datbul)*86400._pm_reel &
                       +pspro%sectimp(numloi,1)
               else
                  date(1)=pspro%timp(numloi,1)
               end if

!/  Cas d'une loi de propulsion impulsionnelle
!/  Ajout d'une loi de propulsion impulsionnelle dans la structure MSP_SCENARIO_LOI 
               call MSP_ajouter_impulsion (pro,"",typdat=MSP_ENUM_LOI_RELATIVE, &
               date=date(1),deltav=pspro%deltav(numloi), dirref=dirref_tmp, &
               omega=pspro%omtab(numloi,1),omegap=pspro%omptab(numloi,1),merg=pspro%xmerg(numloi))
               if ( MSP_gen_messages("psconv_pspro_pro") ) return

            case (2)
                   ! test du type de date
                  if (pspro%typdatcont(numloi).eq.1) then  
                     date(1:pspro%ntab(numloi)) = &
                          (pspro%jjtimp(numloi,1:pspro%ntab(numloi))-datbul )*86400._pm_reel &
                          +pspro%sectimp(numloi,1:pspro%ntab(numloi))
                                     
                  else if (pspro%typdatcont(numloi).eq.3) then !absolu relatif
                     date(1) = &
                         (pspro%jjtimp(numloi,1)-datbul)*86400._pm_reel&
                         + pspro%sectimp(numloi,1)

                     date(2:pspro%ntab(numloi)) = date(1) &
                        + pspro%timp(numloi,2:pspro%ntab(numloi))

                  else !relatif
                     date(1:pspro%ntab(numloi)) = &
                          pspro%timp(numloi,1:pspro%ntab(numloi))          
                  end if

!/  Cas d'une loi de propulsion continue
!/  Ajout d'une loi de propulsion continue dans la structure MSP_SCENARIO_LOI
               call MSP_ajouter_poussee_continue (pro,"",ntab=pspro%ntab(numloi),&
                    typdat=MSP_ENUM_LOI_RELATIVE, &
                    dates=date,&
                    poussee=pspro%fptab(numloi,1:pspro%ntab(numloi)), &
                    isp=pspro%xisp(numloi),omega=pspro%omtab(numloi,1:pspro%ntab(numloi)), &
                    dirref=dirref_tmp, omegap=pspro%omptab(numloi,1:pspro%ntab(numloi)), &
                    merg=pspro%xmerg(numloi),pas=pspro%paspro(numloi),&
                    debit=pspro%fptab(numloi,1:pspro%ntab(numloi))/(g0*pspro%xisp(numloi)))
               if ( MSP_gen_messages("psconv_pspro_pro") ) return
      
            case (3)

               brep(numloi,:) = pspro%brep(numloi,:)
               if ( pspro%bcle_date(numloi) == MSP_ENUM_ECHD_DATE_REF ) then
                  
                  if ( pspro%brep(numloi,2) == MSP_ENUM_ECHT_TUC ) then
                     ! Passage en date TE
                     brep(numloi,4) = pspro%brep(numloi,4) - (str_bul(iveh)%ecart_te_tuc/86400._pm_reel)
                     brep(numloi,2) = real(MSP_ENUM_ECHT_TE,kind=pm_reel)
                  endif
               endif
               if ( ((pspro%timp(numloi,1) < 0._pm_reel) .and. &
                     (pspro%timp(numloi,2) > 0._pm_reel)) .or. &
                    ((pspro%timp(numloi,1) > 0._pm_reel) .and. &
                    (pspro%timp(numloi,2) < 0._pm_reel)) ) then
                  ! La loi de propulsion chevauche la date 0
                  write(parvar,'(i2)') numloi
                  call MSP_signaler_message (cle_mes="PS_INIT_PRO_001",partie_variable=parvar)
                  return
               endif
               datebul = pspro%timp(numloi,2)/86400._pm_reel+datbul

!/  Cas d'une loi de propulsion par bulletin
!/  Crestion du bulletin lie a la propulsion
               bul = MSP_creer_bulletin(datbul=datebul,&
                    iorb=pspro%bibul(numloi),param=pspro%parman(numloi,:), &
                    typrep=int(brep(numloi,1)), &
                    rep=brep(numloi,:), ech_temps_bul=pspro%bech_temps_bul(numloi), &
                    cle_date=pspro%bcle_date(numloi), planete=pspro%bnumpla(numloi), &
                    requa_r=pspro%brequa_r(numloi), apla_r=pspro%bapla_r(numloi),&
                    requa=pspro%brequa(numloi), apla=pspro%bapla(numloi),&
                    mu=pspro%bmu(numloi), vrot=pspro%bvitrot(numloi),obli=pspro%bobli(numloi),&
                    pole_v=pspro%bpolv(numloi), pole_u=pspro%bpolu(numloi))
               if ( MSP_gen_messages("psconv_pspro_pro") ) return
      
!/  Ajout d'une loi de propulsion par bulletin dans la structure MSP_SCENARIO_LOI
               call MSP_ajouter_poussee_bulletin (pro,"",typdat=MSP_ENUM_LOI_RELATIVE, &
               datdeb=pspro%timp(numloi,1),bul=bul,merg=pspro%xmerg(numloi))
               if ( MSP_gen_messages("psconv_pspro_pro") ) return

               nimp(:)  = 0

            case (4)
!/  Cas d'une loi de propulsion sur fichier
               ficpro = pspro%ficpro(numloi)
               paspro = pspro%paspro(numloi)
!/  Lecture du fichier 
               call pslecpro(ficpro,paspro,pro)
               if ( MSP_gen_messages("psconv_pspro_pro") ) return

         end select
         if ( MSP_gen_messages("psconv_pspro_pro") ) return
         numloi = numloi + 1
      end do

   end subroutine psconv_pspro_pro

   subroutine psconv_sep_pssep (sep,datbul,pssep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_sep_pssep
!
!$Resume
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_SEPARATIONS. 
!
!$Description
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_SEPARATIONS. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_sep_pssep (sep,datbul,pssep)
!.    type(MSP_SCENARIO_LOI) :: sep
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_SEPARATIONS) :: pssep
!
!$Arguments
!>E/S   sep     :<MSP_SCENARIO_LOI>     lois de séparation de type MSP_SCENARIO_LOI
!>E     datbul  :<pm_reel>              date du bulletin en JJ CNES
!>S     pssep   :<PS_STR_SEPARATIONS>   lois de séparation de type PS_STR_SEPARATIONS
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_effacer_aero
!- MSP_effacer_coef
!- MSP_effacer_separation
!- MSP_consulter_separation
!- MSP_consulter_prad
!- MSP_consulter_aero
!- MSP_consulter_coef
!- MSP_effacer_prad
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

      ! Parametres
      type(MSP_SCENARIO_LOI), intent(INOUT)    :: sep
      real(kind=pm_reel), intent(IN)      :: datbul
      type(PS_STR_SEPARATIONS), intent(OUT) :: pssep

      ! Variables locales

      type(MSP_SEPARATION) :: loi_sep

      type(MSP_PRAD) :: sep_prad
      type(MSP_AERO) :: sep_aero
      type(MSP_COEF) :: cxa,czn

      character(LEN=8) :: pvar
      real(kind=pm_reel) :: delta_date, sc_date_ref, date
      integer :: sc_type, sc_nloi, iloi, typdat
      integer :: dim_cxa
      real (KIND=pm_reel), pointer, dimension(:) :: cxa_1d
      real (KIND=pm_reel), pointer, dimension(:) :: czn_1d
      integer :: typcf
      integer :: type_coef,type_variation
      integer :: stat

      nullify(cxa_1d)
      nullify(czn_1d)

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (sep, type=sc_type, nloi=sc_nloi, date_ref=sc_date_ref)
      if ( MSP_gen_messages("psconv_sep_pssep") ) return
      
      if ( sc_type /= MSP_ENUM_SEPARATION ) then
!/  Si le scenario n'est pas de type separation
         call MSP_signaler_message (cle_mes="PS_CONV_003")
         return
      endif

      if ( sc_nloi > ps_nsepa ) then
!/  Test sur le nombre de lois du scenario 
         write(pvar,*) ps_nsepa
         call MSP_signaler_message (cle_mes="PS_CONV_003",partie_variable=pvar)
         return
      endif
      
      delta_date =(sc_date_ref - datbul)*86400._pm_reel 


      ! Initialisations à nul de tous les pointeurs contenus 
      ! dans les structures
      call MSP_effacer_aero(sep_aero,nul=.true.)
      call MSP_effacer_coef(cxa,nul=.true.)
      call MSP_effacer_coef(czn,nul=.true.)
      call MSP_effacer_separation(loi_sep,nul=.true.)
      
      ! Boucle sur les lois du scénario:
      pssep%nsep = 0
      do iloi = 1 , sc_nloi
         pssep%nsep = pssep%nsep + 1

!/  extraction de la structure loi de separation dans la structure MSP_SCENARIO_LOI
         call MSP_consulter_scenario (sep, loi_sep=loi_sep, id=iloi)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return

!/  Lecture des donnees dans la structure loi de separation (MSP_SEPARATION)
         call MSP_consulter_separation (loi_sep, typdat=typdat, date=date, &
              deltav=pssep%dvsep(pssep%nsep), omega=pssep%omsep(pssep%nsep), &
              omegap=pssep%ompsep(pssep%nsep), &
              forme=pssep%sepf(pssep%nsep), sx=pssep%sepx(pssep%nsep), &
              sy=pssep%sepy(pssep%nsep), sz=pssep%sepz(pssep%nsep), &
              st=pssep%sept(pssep%nsep), spx=pssep%seppx(pssep%nsep), &
              spy=pssep%seppy(pssep%nsep), spz=pssep%seppz(pssep%nsep), &
              tymasse=pssep%tymasse(pssep%nsep), typcf = typcf, &
              merg=pssep%xma(pssep%nsep), aero=sep_aero,prad=sep_prad)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return

         ! Nouveau coef de pression de radiation solaire
         call MSP_consulter_prad (sep_prad,cmp=pssep%sepcp(pssep%nsep), &
              ka=pssep%sepka(pssep%nsep),ks=pssep%sepks(pssep%nsep), &
              kd=pssep%sepkd(pssep%nsep))
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         
         ! Nouveaux coef AERO 
         call MSP_consulter_aero (sep_aero,cmf=pssep%sepcf(pssep%nsep), &
              cxa=cxa,czn=czn,ficaero=pssep%sepficaero(pssep%nsep),&
              type_coef=type_coef,type_variation=type_variation)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         
         ! Il faut les initialiser idem psconv_vehicule_pscar
         call MSP_consulter_coef(cxa,coef_1d=cxa_1d,dim_coef=dim_cxa)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         
         call MSP_consulter_coef(czn,coef_1d=czn_1d)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return

        
         ! La MECASPA traite des coefficients de frottements dans deux repères
         ! -> repère aérodynamique (typcf = 1 = MSP...AERO_VITESSE) : Cx / Cz
         ! -> repère véhicule (typcf = 2 = MSP_...AERO_VEHICULE) : Ca / Cn
         ! 
         ! Pour la MECASPA, les coefficients sont dans des tableaux à 
         ! 1 dimension (coefs constants ou ne dépendant que d'un paramètre)
         ! 2 dimensions : coefs dépendants de deux paramètres...
         ! ..
         ! un troisième type de coef existe, pour traiter des coefs tabulés
         ! -> MSP_ENUM_CF_TABULE_ALT_MOY
         !
         ! Un champ "type_variation" permet d'indiquer en fonction de quel(s) paramètre(s)
         ! varie(nt) les coefs
         !
         ! PSIMU traite 4 types de coefs, tous dans le repère aérodynamique :
         ! - tabulés en fonction de l'altitude (modèle interne à PSIMU),
         ! - interpolés en fonction de l'altitude, les Cx/Cz sont lus dans un fichier,
         ! - constants ; les valeurs de coefs de trainée et de portance sont saisis à l'ihm
         ! - interpolés en fonction de l'incidence et du mach, les Cx/Cz sont lus dans un fichier.

         if (type_coef == MSP_ENUM_CF_TABULE_ALT_MOY) then
            ! Coefficients tabulés en fonction de l'altitude.
            ! Modèle codé dans PSIMU et MECASPA, avec valeurs des coefs de trainée et de portance
            ! tabulés en fonction de l'altitude.
            !
            ! -> Correspond au choix '1' pour typcf et à 0 pour typcf_alt (n'est pas lu dans un fic)
            pssep%typcf(pssep%nsep)     = 1
            pssep%typcf_alt(pssep%nsep) = 0
         else if (type_coef == MSP_ENUM_COEFF_AERO_VITESSE) then
            if(type_variation == MSP_ENUM_CONSTANT) then
               ! Coefs constants
               ! -> Correspond au choix '2' pour typcf
               pssep%typcf(pssep%nsep)     = 2
               if (associated(cxa_1d)) pssep%sepcd(pssep%nsep)   = cxa_1d(1)
               if (associated(czn_1d)) pssep%sepcl(pssep%nsep)   = czn_1d(1)
               
            else if (type_variation == MSP_ENUM_ALTITUDE) then
               ! Coefs variant en fonction de l'altitude, lecture dans un fichier
               ! -> Correspond au choix '1' pour typcf, et 1 pour typcf_alt
               pssep%typcf(pssep%nsep)     = 1
               pssep%typcf_alt(pssep%nsep) = 1
               
               ! le fichier AERO est stocké dans pssep%sepficaero(pssep%nsep)
            else if (type_variation == MSP_ENUM_INCIDENCE_MACH) then
               !  Coefs variant en fonction de l'incidence et du mach, lecture dans un fichier
               !  -> Correspond au choix '3' pour typcf dans PSIMU
               pssep%typcf(pssep%nsep)     = 3
               ! le fichier AERO est stocké dans pssep%sepficaero(pssep%nsep)
            else if (type_variation ==  MSP_ENUM_COEFF_NULS) then
               ! Coefs nuls (non initialisés) : dans PSIMU, ce seront des coefs
               ! constants (typcf=2) avec comme valeur 0
               pssep%typcf(pssep%nsep)   = 2
               pssep%sepcd(pssep%nsep)   = 0._pm_reel
               pssep%sepcl(pssep%nsep)   = 0._pm_reel
               
            else
               call MSP_signaler_message (cle_mes="PS_INTERFACE_PSIMU_DON_003", &
                    routine="psconv_sep_pssep")
            end if
         else
            call MSP_signaler_message (cle_mes="PS_INTERFACE_PSIMU_DON_002", &
                 routine="psconv_sep_pssep")
         end if

         if ( typdat == MSP_ENUM_LOI_ABSOLUE) then
!/  Si la date de separation est de type absolue (date en jour julien) --> transformation en duree (s)
            pssep%tsep(pssep%nsep) = (date - datbul)*86400._pm_reel
         else
!/  Si la date de separation est de type relative (duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin
            pssep%tsep(pssep%nsep) = date + delta_date
         endif

         ! Libération de la mémoire
         ! -> effectuée dans la boucle, pour éviter les fuites mémoire
         call MSP_effacer_prad (sep_prad)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         call MSP_effacer_aero (sep_aero)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         call MSP_effacer_separation (loi_sep)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         call MSP_effacer_coef (cxa)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return
         call MSP_effacer_coef (czn)
         if ( MSP_gen_messages("psconv_sep_pssep") ) return

         if (associated(cxa_1d)) then
            deallocate (cxa_1d, stat=stat)
            if (stat < 0) then
               call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psconv_sep_pssep")
            end if
         end if
         if (associated(czn_1d)) then
            deallocate (czn_1d, stat=stat)
            if (stat < 0) then
               call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psconv_sep_pssep")
            end if
         end if
 
      enddo

       
    end subroutine psconv_sep_pssep

   subroutine psconv_pssep_sep (pssep,datbul,sep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_pssep_sep
!
!$Resume
!  Conversion d'une structure de type PS_STR_SEPARATIONS à une structure de type MSP_SCENARIO_LOI. 
!
!$Description
!  Conversion d'une structure de type PS_STR_SEPARATIONS à une structure de type MSP_SCENARIO_LOI. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_pssep_sep (pssep,datbul,sep)
!.    type(PS_STR_SEPARATIONS) :: pssep
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: sep
!
!$Arguments
!>E     pssep   :<PS_STR_SEPARATIONS>   lois de séparation de type PS_STR_SEPARATIONS
!>E     datbul  :<pm_reel>              date du bulletin en JJ CNES
!>E/S   sep     :<MSP_SCENARIO_LOI>     lois de séparation de type MSP_SCENARIO_LOI
!
!$Common
!
!$Routines
!- MSP_effacer_prad
!- MSP_effacer_aero
!- MSP_effacer_coef
!- MSP_ajouter_separation
!
!$Include
!
!$Module
!#V
!- ps_caracteristiques
!- ps_attitude
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use ps_caracteristiques
     use ps_attitude

      implicit none

      type(PS_STR_SEPARATIONS), intent(IN) :: pssep
      real(kind=pm_reel), intent(IN)     :: datbul
      type(MSP_SCENARIO_LOI), intent(INOUT)  :: sep

      ! variables locales
      integer :: numloi
      type(MSP_AERO) :: sep_aero
      type(MSP_prad) :: sep_prad
      type(MSP_COEF) :: cxa,czn
      real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
      real (KIND=pm_reel), pointer , dimension(:) :: par1_coef
      integer, dimension(ps_nsepa) :: typcf
      integer :: stat

      nullify(coef_1d)
      nullify(par1_coef)

      call MSP_effacer_prad(sep_prad)
      call MSP_effacer_aero(sep_aero,nul=.true.)
      call MSP_effacer_coef(cxa,nul=.true.)
      call MSP_effacer_coef(czn,nul=.true.)


!/  Creation d'une loi de  scenario de type separation
      sep = MSP_creer_scenario (MSP_ENUM_SEPARATION,date_ref=datbul,nom="")

!/  Boucle sur les lois du scénario:
      numloi = 1
      do while ( numloi <= pssep%nsep )

!/  Ajout d'une loi de separation dans la structure MSP_SCENARIO_LOI

         ! structure PRAD
         sep_prad = MSP_creer_prad(cmp=pssep%sepcp(numloi), &
              ka=pssep%sepka(numloi),ks=pssep%sepks(numloi), &
              kd=pssep%sepkd(numloi))
         if ( MSP_gen_messages("psconv_pssep_sep") ) return

               !/  Creation de la structure MSP_AERO (coef aerodynamique)  a partir des donnees de 
               !/  la structure PS_STR_CARACTERISTIQUES
         select case ( pssep%typcf(numloi) )
         case (1)
            !/  Cas de coefficients de frottement tabules en altitude
            !/  2 sous-cas : coefs constants
            !/               coefs lus dans un fichier
            if(pssep%typcf_alt(numloi) == 1) then
               
               !/ Coefs dépendants de l'altitude, avec interpolation sur fichier
               sep_aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE, &
                 type_variation=MSP_ENUM_ALTITUDE,ficaero=trim(dirdat)//"/"// &
                 pssep%sepficaero(numloi), cmf=pssep%sepcf(numloi))

               typcf(numloi) = MSP_ENUM_COEFF_AERO_VITESSE

            else
               !/ Coefs dépendants de l'altitude : utilisation du modèle tabulé 
               sep_aero = MSP_creer_aero(type_coef=MSP_ENUM_CF_TABULE_ALT_MOY,&
                    type_variation=MSP_ENUM_ALTITUDE,cmf=pssep%sepcf(numloi))
               if(MSP_gen_messages("psconv_pssep_sep")) return


               typcf(numloi) = MSP_ENUM_CF_TABULE_ALT_MOY

            end if
         case (2)
            !/  Cas de coefficients de frottement constants
            if (associated(coef_1d)) deallocate(coef_1d, stat=stat)
            allocate(coef_1d(1))
            if (associated(par1_coef)) deallocate(par1_coef, stat=stat)
            allocate(par1_coef(1))
            par1_coef(1)= 0._pm_reel
            coef_1d = pssep%sepcd(numloi)
            !/  Creation de la structure MSP_COEF pour les coefficient de frottement
            cxa = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)

            coef_1d = pssep%sepcl(numloi)
            !/  Creation de la structure MSP_COEF pour les coefficients de portance
            czn = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)  

            ! Libération de la mémoire
            if (associated(coef_1d)) deallocate(coef_1d, stat=stat)
            if (associated(par1_coef)) deallocate(par1_coef, stat=stat)

            !/  Creation de la structure MSP_AERO a partir des coefficients de portance, 
            !/  de frottement et de la structure PS_STR_CARACTERISTIQUES          
           sep_aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
                type_variation=MSP_ENUM_CONSTANT,cmf=pssep%sepcf(numloi),cxa=cxa,czn=czn)

            typcf(numloi) = MSP_ENUM_COEFF_AERO_VITESSE
            

         case (3)
            !/  Cas de coefficients de frottement lus dans un fichier AERO
            !/  Creation de la structure MSP_AERO a partir de la structure PS_STR_CARACTERISTIQUES 
            sep_aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
                 type_variation=MSP_ENUM_INCIDENCE_MACH,ficaero=trim(dirdat)//"/"//pssep%sepficaero(numloi), &
                 cmf=pssep%sepcf(numloi))
 
            typcf(numloi) = MSP_ENUM_COEFF_AERO_VITESSE
         end select

         if ( MSP_gen_messages("psconv_pssep_sep") ) return
         call MSP_ajouter_separation (sep,"",typdat=MSP_ENUM_LOI_RELATIVE,&
              date=pssep%tsep(numloi), &
              deltav=pssep%dvsep(numloi),omega=pssep%omsep(numloi),&
              omegap=pssep%ompsep(numloi), &
              forme=pssep%sepf(numloi),sx=pssep%sepx(numloi),sy=pssep%sepy(numloi),&
              sz=pssep%sepz(numloi),st=pssep%sept(numloi), typcf=typcf(numloi), &
              spx=pssep%seppx(numloi),spy=pssep%seppy(numloi),&
              spz=pssep%seppz(numloi),tymasse=pssep%tymasse(numloi), &
              merg=pssep%xma(numloi), aero=sep_aero,prad=sep_prad)
         if ( MSP_gen_messages("psconv_pssep_sep") ) return
         numloi = numloi + 1

         ! Libération de la mémoire
         call MSP_effacer_prad (sep_prad)
         if ( MSP_gen_messages("psconv_pssep_sep") ) return
         call MSP_effacer_aero (sep_aero)
         if ( MSP_gen_messages("psconv_pssep_sep") ) return
 
         call MSP_effacer_coef (cxa)
         if ( MSP_gen_messages("psconv_pssep_sep") ) return

         call MSP_effacer_coef (czn)
         if ( MSP_gen_messages("psconv_pssep_sep") ) return

      end do
   end subroutine psconv_pssep_sep

   subroutine psconv_ati_psati (ati,datbul,psati)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_ati_psati
!
!$Resume
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_ATTITUDE. 
!
!$Description
!  Conversion d'une structure de type MSP_SCENARIO_LOI à une structure de type PS_STR_ATTITUDE. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_ati_psati (ati,datbul,psati)
!.    type(MSP_SCENARIO_LOI) :: ati
!.    real(kind=pm_reel) :: datbul
!.    type(PS_STR_ATTITUDE) :: psati
!
!$Arguments
!>E/S   ati     :<MSP_SCENARIO_LOI>   lois d'attitude de type MSP_SCENARIO_LOI
!>E     datbul  :<pm_reel>            date du bulletin en JJ CNES
!>S     psati   :<PS_STR_ATTITUDE>    lois d'attitude de type PS_STR_ATTITUDE
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_premiere_loi
!- MSP_consulter_ptr_loi
!- MSP_consulter_attitude_tabulee
!- md_joursec_jourfrac
!- MSP_consulter_attitude_spinnee
!- MSP_loi_suivante
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

      character(LEN=8) :: com

      ! Arguments
      !==========
      type(MSP_SCENARIO_LOI), intent(INOUT) :: ati
      real(kind=pm_reel), intent(IN)     :: datbul
      type(PS_STR_ATTITUDE), intent(OUT) :: psati

      type(MSP_ATTITUDE_TABULEE), pointer :: loi_attit_tab
      type(MSP_ATTITUDE_SPINNEE), pointer :: loi_attit_spin

      ! Variables locales
      !==================

      integer :: i
      real(kind=pm_reel) :: delta_date, sc_date_ref, datdeb, datfin
      integer :: sc_type, sc_nloi, iloi, typdat, typloi, ntab
      real(KIND=PM_REEL), dimension(:), pointer :: tab_dates
      type(tm_jour_sec),  dimension(:), pointer :: tab_dates_js
      real(KIND=PM_REEL), dimension(:), pointer :: tab_psi
      real(KIND=PM_REEL), dimension(:), pointer :: tab_teta
      real(KIND=PM_REEL), dimension(:), pointer :: tab_phi
      integer :: convention_angles
      type(tm_code_retour) :: code_retour

      integer :: stat

      nullify(loi_attit_tab)
      nullify(loi_attit_spin)
      nullify(tab_dates)
      nullify(tab_dates_js)
      nullify(tab_psi)
      nullify(tab_teta)
      nullify(tab_phi)

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (ati, type=sc_type, nloi=sc_nloi, date_ref=sc_date_ref)
      if ( MSP_gen_messages("psconv_ati_psati") ) return

!/  Si la loi de scenario n'est pas de type attitude
      if ( sc_type /= MSP_ENUM_ATTITUDE ) then
         call MSP_signaler_message (cle_mes="PS_CONV_005")
         return
      endif

      if ( sc_nloi > ps_nloi_att ) then
!/  Test sur le nombre de lois du scenario 
         write (com,'(i2)') ps_nloi_att
         call MSP_signaler_message (cle_mes="PS_CONV_006",partie_variable=com)
         return
      endif

      delta_date =(sc_date_ref - datbul)*86400._pm_reel 

      ! Boucle sur les lois du scénario:
      psati%natt = 0

      ! Init de boucle : positionnement du pointeur loi courante sur la premiere loi
      call MSP_premiere_loi(ati)
      if ( MSP_gen_messages("psconv_ati_psati") ) return

      do iloi = 1 , sc_nloi

!/  Extraction du type de la loi courante
        typloi = MSP_type_loi (scenario=ati)
         if ( MSP_gen_messages("psconv_ati_psati") ) return
         psati%natt = psati%natt + 1

         select case ( typloi )
            case (MSP_ENUM_LOI_ATTI_TAB)
!/  Pour une loi d'attitude tabulee
               call MSP_consulter_ptr_loi (ati, loi_atti_tab=loi_attit_tab)
               if ( MSP_gen_messages("psconv_ati_psati") ) return

!/  Lecture des donnees dans la structure loi d'attitude tabulee (MSP_ATTITUDE_TABULEE)
               call MSP_consulter_attitude_tabulee (loi_attit_tab, ntab, typdat=typdat, typrep=psati%irepa(psati%natt),&
                                                    dates=tab_dates, dates_js=tab_dates_js,psi=tab_psi, teta=tab_teta, phi=tab_phi,&
                                                    typangle = convention_angles)
               if ( MSP_gen_messages("psconv_ati_psati") ) return

               psati%itypa(psati%natt)  = 1
               psati%npatt(psati%natt)  = ntab
               
               psati%iangle(psati%natt) = convention_angles

               if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then
!/  Si les dates des lois d'attitude sont de type absolu (date en jour julien) --> transformation en duree (s)
                  do i = 1 , ntab
                     psati%datt(psati%natt,i) = (tab_dates(i) - datbul)*86400._pm_reel
                  enddo
               else if (typdat == MSP_ENUM_LOI_ABSOREL) then
!/  Si les dates des lois d'attitude sont de types absolu relatif (duree (s) depuis une date de saisie) 
!/   --> Prise en compte de la date absolue de reference puis ajout des durées tabulées 
                  call md_joursec_jourfrac(tab_dates_js(1),tab_dates(1),code_retour)

                  psati%datt(psati%natt,1) = (tab_dates(1)- datbul)*86400._pm_reel
                  psati%datt(psati%natt,2:ntab) = psati%datt(psati%natt,1)+tab_dates(2:ntab) 
               else
!/  Si les dates des lois d'attitude sont de types relatif (duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin
                  psati%datt(psati%natt,1:ntab) = tab_dates(1:ntab) + delta_date
               endif

!/  sauvegarde des donnees dans la structure PS_STR_ATTITUDE
               psati%ang1(psati%natt,1:ntab)  = tab_psi(1:ntab)
               psati%ang2(psati%natt,1:ntab)  = tab_teta(1:ntab)
               psati%ang3(psati%natt,1:ntab)  = tab_phi(1:ntab)

!/  Deallocation des tableaux dynamiques
               DEALLOCATE(tab_dates_js, stat=stat)
               DEALLOCATE(tab_dates, stat=stat)
               DEALLOCATE(tab_psi, stat=stat)
               DEALLOCATE(tab_teta, stat=stat)
               DEALLOCATE(tab_phi, stat=stat)

            case (MSP_ENUM_LOI_ATTI_SPIN)
!/  Pour une loi d'attitude de type spinnee (MSP_ATTITUDE_SPINNEE)

!/  extraction de la structure MSP_ATTITUDE_SPINNEE dans la structure scenario (MSP_SCENARIO_LOI)
               call MSP_consulter_ptr_loi (ati, loi_atti_spin=loi_attit_spin)
               if ( MSP_gen_messages("psconv_ati_psati") ) return

!/  Lecture des donnees dans la structure loi d'attitude spinee (MSP_ATTITUDE_SPINNEE)
               call MSP_consulter_attitude_spinnee (loi_attit_spin, typdat=typdat, &
                    datedeb=datdeb, datefin=datfin, &
                    typrep=psati%irepa(psati%natt), &
                    psi0=psati%ang1(psati%natt,1), teta0=psati%ang2(psati%natt,1),&
                    phi0=psati%ang3(psati%natt,1), &
                    psip=psati%angp1(psati%natt), tetap=psati%angp2(psati%natt), &
                    phip=psati%angp3(psati%natt), typangle = convention_angles)
               if ( MSP_gen_messages("psconv_ati_psati") ) return
               psati%itypa(psati%natt)  = 2
               psati%npatt(psati%natt)  = 2

               psati%iangle(psati%natt)  = convention_angles 

               if ( typdat == MSP_ENUM_LOI_ABSOLUE ) then
!/  Si les dates des lois d'attitude sont de types absolus (date en jour julien) --> transformation en duree (s)
                  psati%datt(psati%natt,1) = (datdeb - datbul)*86400._pm_reel
                  psati%datt(psati%natt,2) = (datfin - datbul)*86400._pm_reel
               else if ( typdat == MSP_ENUM_LOI_ABSOREL ) then
!/  Si les dates des lois d'attitude sont de types absolue relatif (duree (s) depuis une date de saisie absolue) 
!/  --> Convertion en durée de la date de reference + ajout de la durée 
                  psati%datt(psati%natt,1) = (datdeb- datbul)*86400._pm_reel
                  psati%datt(psati%natt,2) = psati%datt(psati%natt,1) + datfin
               else
!/  Si les dates des lois d'attitude sont de types relatifs (duree (s) depuis le debut de la simulation) --> Prise 
!/   en compte de la date de reference du scenario par rapport a la date du bulletin      
                  psati%datt(psati%natt,1) = datdeb + delta_date
                  psati%datt(psati%natt,2) = datfin + delta_date
               endif
         end select

         call MSP_loi_suivante(ati)
         if ( MSP_gen_messages("psconv_ati_psati") ) return

      enddo

   end subroutine psconv_ati_psati

   subroutine psconv_psati_ati (psati,datbul,ati)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_psati_ati
!
!$Resume
!  Conversion d'une structure de type PS_STR_ATTITUDE à une structure de type MSP_SCENARIO_LOI. 
!
!$Description
!  Conversion d'une structure de type PS_STR_ATTITUDE à une structure de type MSP_SCENARIO_LOI. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_psati_ati (psati,datbul,ati)
!.    type(PS_STR_ATTITUDE) :: psati
!.    real(kind=pm_reel) :: datbul
!.    type(MSP_SCENARIO_LOI) :: ati
!
!$Arguments
!>E     psati   :<PS_STR_ATTITUDE>    lois d'attitude de type PS_STR_ATTITUDE
!>E     datbul  :<pm_reel>            date du bulletin en JJ CNES
!>E/S   ati     :<MSP_SCENARIO_LOI>   lois d'attitude de type MSP_SCENARIO_LOI
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_ajouter_attitude_tabulee
!- MSP_ajouter_attitude_spinnee
!- pslecatt
!
!$Include
!
!$Module
!#V
!- ps_attitude
!- ps_propulsion
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use ps_attitude
     use ps_propulsion
     
      implicit none
      
      ! Arguments
      !==========

      type(PS_STR_ATTITUDE), intent(IN) :: psati
      real(kind=pm_reel), intent(IN)     :: datbul
      type(MSP_SCENARIO_LOI), intent(INOUT) :: ati

      ! Variables locales
      !==================

      integer :: numloi
      real (KIND=pm_reel), dimension(ps_npts_att) :: dates
      real (KIND=pm_reel) :: datdeb
      real (KIND=pm_reel) :: datfin

      character(LEN=256) :: ctmp
      character (LEN=256) :: parvar
      integer :: lnblnk
      character(LEN=80)  :: ficati

!/  Creation d'une loi de  scenario de type attitude
      ati = MSP_creer_scenario (MSP_ENUM_ATTITUDE,date_ref=datbul,nom="")

!/ Boucle sur les lois du scénario:
      numloi = 1
      do while ( numloi <= psati%natt )

         select case ( psati%itypa(numloi) )
         case (1)
            if ( (psati%npatt(numloi) < 2) .or. &
                 (psati%npatt(numloi) > ps_npts_att) ) then
               ctmp = Domtraduire(nomdomaine,"PS_INI_NBATITAB")
               write(parvar,'(a,a,i1,a,a,i2,a)') ctmp(1:lnblnk(ctmp)),&
                    '"',2,'"','"',ps_npts_att,'"'
               call MSP_signaler_message (cle_mes="PS_INIT_INTERVALLE",&
                    partie_variable=parvar)
               return
            end if

            ! test du type de date, cas absolu
            if (psati%typdatatt(numloi).eq.1) then                   
               dates(1:psati%npatt(numloi)) = &
                    (psati%jjatt(numloi,1:psati%npatt(numloi))-str_bul(iveh)%datbul0 )*86400._pm_reel &
                    +psati%secatt(numloi,1:psati%npatt(numloi))
               
            else if (psati%typdatatt(numloi) == MSP_ENUM_LOI_ABSOREL) then
!/  Si les dates des lois d'attitude sont de types absolu relatif (duree (s) depuis une date de saisie) 
!/   --> Prise en compte de la date absolue de reference puis ajout des durées tabulées 

               dates(1)= &
                    (psati%jjatt(numloi,1)-str_bul(iveh)%datbul0)*86400._pm_reel&
                    + psati%secatt(numloi,1)
               dates(2:psati%npatt(numloi))= dates(1) &
                    + psati%datt(numloi,2:psati%npatt(numloi))
               
            else
               dates(1:psati%npatt(numloi)) = psati%datt(numloi,1:psati%npatt(numloi))
            endif

!/  Cas d'une loi d'attitude tabulee
!/  Ajout d'une loi d'attitude tabulee dans la structure MSP_SCENARIO_LOI
            call MSP_ajouter_attitude_tabulee (ati,"",typdat=MSP_ENUM_LOI_RELATIVE, &
                 ntab=psati%npatt(numloi),dates=dates, &
                 typrep=psati%irepa(numloi),psi=psati%ang1(numloi,1:psati%npatt(numloi)), &
                 teta=psati%ang2(numloi,1:psati%npatt(numloi)),phi=psati%ang3(numloi,1:psati%npatt(numloi)),&
                 typangle=psati%iangle(numloi))
            if (MSP_gen_messages("psconv_psati_ati")) return

         case (2)
            ! test du type de date, cas absolu
            if (psati%typdatvrot(numloi).eq.1) then                   
               datdeb = (psati%jjatt(numloi,1)-str_bul(iveh)%datbul0 )*86400._pm_reel &
                    +psati%secatt(numloi,1)
               datfin = (psati%jjatt(numloi,2)-str_bul(iveh)%datbul0 )*86400._pm_reel &
                    +psati%secatt(numloi,2)
            else if (psati%typdatvrot(numloi) == MSP_ENUM_LOI_ABSOREL) then
               datdeb = (psati%jjatt(numloi,1)-str_bul(iveh)%datbul0 )*86400._pm_reel &
                    +psati%secatt(numloi,1)
               datfin = datdeb + psati%datt(numloi,1)
            else
               datdeb = psati%datt(numloi,1)
               datfin = psati%datt(numloi,2)
            endif

!/  Cas d'une loi d'attitude spinnee
!/  Ajout d'une loi d'attitude spinnee dans la structure MSP_SCENARIO_LOI
            call MSP_ajouter_attitude_spinnee (ati,"",typdat=MSP_ENUM_LOI_RELATIVE, &
                 datdeb=datdeb,datfin=datfin, &
                 typrep=psati%irepa(numloi), &
                 psi0=psati%ang1(numloi,1),teta0=psati%ang2(numloi,1),phi0=psati%ang3(numloi,1), &
                 psip=psati%angp1(numloi),tetap=psati%angp2(numloi),phip=psati%angp3(numloi),&
                 typangle=psati%iangle(numloi))
            if (MSP_gen_messages("psconv_psati_ati")) return
            
         case (3)
!/  Cas d'une loi d'attitude sur fichier
            ficati = psati%ficatt(numloi)
!/  Lecture du fichier 
            call pslecatt(ficati,ati)
            if ( MSP_gen_messages("psconv_psati_ati") ) return

         end select
         if ( MSP_gen_messages("psconv_psati_ati") ) return       
         numloi = numloi + 1
      end do

   end subroutine psconv_psati_ati

   subroutine psconv_vehicule_pscar (vehicule,pscar)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_vehicule_pscar
!
!$Resume
!  Conversion d'une structure de type MSP_VEHICULE à une structure de type PS_STR_CARACTERISTIQUES. 
!
!$Description
!  Conversion d'une structure de type MSP_VEHICULE à une structure de type PS_STR_CARACTERISTIQUES. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_vehicule_pscar (vehicule,pscar)
!.    type (MSP_VEHICULE) :: vehicule
!.    type (PS_STR_CARACTERISTIQUES) :: pscar
!
!$Arguments
!>E     vehicule  :<MSP_VEHICULE>              caractéristiques véhicule de type MSP_VEHICULE
!>S     pscar     :<PS_STR_CARACTERISTIQUES>   caractéristiques véhicule de type PS_STR_CARACTERISTIQUES
!
!$Common
!
!$Routines
!- MSP_effacer_prad
!- MSP_effacer_aero
!- MSP_effacer_coef
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_consulter_prad
!- MSP_consulter_aero
!- MSP_consulter_coef
!- MSP_signaler_message
!- MSP_effacer_mci
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use ps_caracteristiques

      implicit none

      ! Arguments
      !==========
      type (MSP_VEHICULE), intent(IN)             :: vehicule
      type (PS_STR_CARACTERISTIQUES), intent(OUT) :: pscar

      ! Variables locales
      !==================

      type(MSP_PRAD) :: veh_prad
      type(MSP_AERO) :: veh_aero
      type(MSP_MCI)  :: veh_mci

      integer :: dim_cxa
      real (KIND=pm_reel), pointer, dimension(:) :: cxa_1d
      real (KIND=pm_reel), pointer, dimension(:) :: czn_1d
      character(LEN=80) :: ficaero
      integer :: stat
      integer :: type_coef, type_variation

      !--- ajout variables pour nouveaux modules Vehicules de la MECASPA -----
      type(MSP_COEF) :: cxa,czn

      ! Initialisations
      nullify(cxa_1d)
      nullify(czn_1d)
      call MSP_effacer_prad(veh_prad)
      call MSP_effacer_aero(veh_aero, nul=.true.)
      call MSP_effacer_coef(cxa, nul=.true.)
      call MSP_effacer_coef(czn, nul=.true.)

      ! DM-ID 1113 : cette fonction remplit le "common" (structure PS_STR_CARACTERISTIQUES)
      ! mais aussi la structure interne PSIMU (str_car(iveh)%vehicule) 

      pscar%ficaero = ""
      if (str_car(iveh)%vehiculeinit) then
         call MSP_effacer_vehicule(str_car(iveh)%vehicule)
         str_car(iveh)%vehiculeinit = .false.
      else
         call MSP_effacer_vehicule(str_car(iveh)%vehicule, nul=.true.)
      endif
      str_car(iveh)%vehicule = vehicule
      str_car(iveh)%vehiculeinit = .true.
      
      !-----------------------------------------------------------------------
!/  Extraction des strcture MSP_PRAD (coef thermique), MSP_AERO (coef aero) et MSP_MCI 
!/  (geometrie du vehicule) de la structure MSP_VEHICULE
      call MSP_consulter_vehicule (vehicule,mci=veh_mci,aero=veh_aero,prad=veh_prad)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return

!/  passage des donnees de la structure MSP_MCI (geometrie du vehicule) dans la structure PS_STR_CARACTERISTIQUES
      call MSP_consulter_mci (veh_mci,forme=pscar%forme, sx=pscar%sx, sy=pscar%sy, sz=pscar%sz, st=pscar%st, &
                              spx=pscar%spx, spy=pscar%spy, spz=pscar%spz, mtot=pscar%xm)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return

!/  passage des donnees de la structure MSP_PRAD (coef thermique) dans la structure PS_STR_CARACTERISTIQUES
      call MSP_consulter_prad (veh_prad,cmp=pscar%cp,ka=pscar%ka,ks=pscar%ks,kd=pscar%kd)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return

      !--- modif pour nouveaux modules Vehicules de la MECASPA -----

!/  passage des donnees de la structure MSP_AERO (coef aerodynamique) dans la structure PS_STR_CARACTERISTIQUES
!/  +  extraction des structure cxa et czn
      call MSP_consulter_aero (veh_aero,cmf=pscar%cf,cxa=cxa,czn=czn,ficaero=ficaero, &
           type_coef=type_coef, type_variation=type_variation)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return

!/  Lecture des coefficient de frottement dans la structure MSP_COEF (coef aerodynamique)
      call MSP_consulter_coef(cxa,coef_1d=cxa_1d,dim_coef=dim_cxa)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return

!/  Lecture des coefficient de portance dans la structure MSP_COEF (coef aerodynamique)
      call MSP_consulter_coef(czn,coef_1d=czn_1d)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
      !----------------------------------------------------------------

      if (type_coef == MSP_ENUM_CF_TABULE_ALT_MOY) then
         ! Coefficients tabulés en fonction de l'altitude.
         ! Modèle codé dans PSIMU et MECASPA, avec valeurs des coefs de trainée et de portance
         ! tabulés en fonction de l'altitude.
         !
         ! -> Correspond au choix '1' pour typcf et à 0 pour typcf_alt (n'est pas lu dans un fic)
         pscar%typcf     = 1 
         pscar%typcf_alt = 0
      else if (type_coef == MSP_ENUM_COEFF_AERO_VITESSE) then
         if(type_variation == MSP_ENUM_CONSTANT) then
            ! Coefs constants
            ! -> Correspond au choix '2' pour typcf
            pscar%typcf     = 2
            if(associated(cxa_1d)) pscar%cd(1,1)   = cxa_1d(1)
            if(associated(czn_1d)) pscar%cl(1,1)   = czn_1d(1)

         else if (type_variation == MSP_ENUM_ALTITUDE) then
            ! Coefs variant en fonction de l'altitude, lecture dans un fichier
            ! -> Correspond au choix '1' pour typcf, et 1 pour typcf_alt
            pscar%typcf     = 1
            pscar%typcf_alt = 1

            pscar%ficaero = ficaero

         else if (type_variation == MSP_ENUM_INCIDENCE_MACH) then
            !  Coefs variant en fonction de l'incidence et du mach, lecture dans un fichier
            !  -> Correspond au choix '3' pour typcf dans PSIMU
            pscar%typcf     = 3
            pscar%ficaero = ficaero
         else if (type_variation == MSP_ENUM_COEFF_NULS) then
            ! Coefs nuls, c'est à dire non initialisé : 
            ! correspond à l'init par défaut si à la création d'un véhicule, on ne mentionne
            ! pas de structure aero
            ! -> simulé au niveau de PSIMU sous la forme de 2 coefs constants nuls
            pscar%typcf     = 2
            pscar%cd(1,1) = 0._pm_reel
            pscar%cl(1,1) = 0._pm_reel
         else
            call MSP_signaler_message (cle_mes="PS_INTERFACE_PSIMU_DON_003", &
                 routine="psconv_vehicule_pscar")
         end if
      else
         call MSP_signaler_message (cle_mes="PS_INTERFACE_PSIMU_DON_002", &
              routine="psconv_vehicule_pscar")
      end if

      ! Libération de la mémoire
      if (associated(cxa_1d)) deallocate (cxa_1d, stat=stat)
      if (associated(czn_1d)) deallocate (czn_1d, stat=stat)

      call MSP_effacer_prad (veh_prad)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
      call MSP_effacer_aero (veh_aero)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
      call MSP_effacer_mci (veh_mci)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
      call MSP_effacer_coef (cxa)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
      call MSP_effacer_coef (czn)
      if ( MSP_gen_messages("psconv_vehicule_pscar") ) return
   end subroutine psconv_vehicule_pscar

   subroutine psconv_pscar_vehicule (pscar,vehicule)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_pscar_vehicule
!
!$Resume
!  Conversion d'une structure de type PS_STR_CARACTERISTIQUES à une structure de type MSP_VEHICULE. 
!
!$Description
!  Conversion d'une structure de type PS_STR_CARACTERISTIQUES à une structure de type MSP_VEHICULE. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_pscar_vehicule (pscar,vehicule)
!.    type (PS_STR_CARACTERISTIQUES) :: pscar
!.    type (MSP_VEHICULE) :: vehicule
!
!$Arguments
!>E     pscar     :<PS_STR_CARACTERISTIQUES>   caractéristiques véhicule de type PS_STR_CARACTERISTIQUES
!>E/S   vehicule  :<MSP_VEHICULE>              caractéristiques véhicule de type MSP_VEHICULE 
!
!$Common
!
!$Routines
!- MSP_effacer_mci
!- MSP_effacer_prad
!- MSP_effacer_aero
!- MSP_effacer_coef
!- MSP_effacer_vehicule
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

      ! Arguments
      !==========
      type (PS_STR_CARACTERISTIQUES), intent(IN) :: pscar
      type (MSP_VEHICULE), intent(INOUT)         :: vehicule

      ! Variables locales
      !==================
      type(MSP_MCI)  :: mci
      type(MSP_AERO) :: aero
      type(MSP_prad) :: prad
      type(MSP_COEF) :: cxa,czn
      real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
      real (KIND=pm_reel), pointer , dimension(:) :: par1_coef

      integer :: stat
      
      ! Initialisations
      nullify(coef_1d)
      nullify(par1_coef)
      call MSP_effacer_mci(mci)
      call MSP_effacer_prad(prad)
      call MSP_effacer_aero(aero, nul=.true.)
      call MSP_effacer_coef(cxa,nul=.true.)
      call MSP_effacer_coef(czn,nul=.true.)

!/  Creation de la structure MSP_MCI (geom. du vehicule) a partir des donnees de 
!/  la structure PS_STR_CARACTERISTIQUES
      mci  = MSP_creer_mci (forme=pscar%forme,sx=pscar%sx,sy=pscar%sy,sz=pscar%sz,st=pscar%st,&
                            spx=pscar%spx,spy=pscar%spy,spz=pscar%spz,mstr=pscar%xm)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return

!/  Creation de la structure MSP_prad (coef thermique) a partir des donnees de 
!/  la structure PS_STR_CARACTERISTIQUES
      prad = MSP_creer_prad(cmp=pscar%cp,ka=pscar%ka,ks=pscar%ks,kd=pscar%kd)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return


!/  Creation de la structure MSP_AERO (coef aerodynamique)  a partir des donnees de 
!/  la structure PS_STR_CARACTERISTIQUES
      select case ( pscar%typcf )
      case (1)
         !/  Cas de coefficients de frottement tabules en altitude
         ! Deux sous cas : soit les coefs sont décrits dans un fichier, 
         ! soit ce sont des coefs codés en interne de PSIMU 
         if(pscar%typcf_alt == 0) then
            aero = MSP_creer_aero(type_coef=MSP_ENUM_CF_TABULE_ALT_MOY,&
                 type_variation=MSP_ENUM_ALTITUDE,cmf=pscar%cf)
         else
            aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
                 type_variation=MSP_ENUM_ALTITUDE,ficaero = pscar%ficaero)
         end if
      case (2)
         !/  Cas de coefficients de frottement constants
         
         if (associated(coef_1d)) deallocate(coef_1d, stat=stat)
         allocate(coef_1d(1))
         if (associated(par1_coef)) deallocate(par1_coef, stat=stat)
         allocate(par1_coef(1))
         par1_coef(1)= 0._pm_reel
         coef_1d = pscar%cd(1,1)
         !/  Creation de la structure MSP_COEF pour les coefficient de frottement
         cxa = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)

         coef_1d = pscar%cl(1,1)
         !/  Creation de la structure MSP_COEF pour les coefficients de portance
         czn = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)  

         ! Libération de la mémoire
         if (associated(coef_1d)) deallocate(coef_1d, stat=stat)
         if (associated(par1_coef)) deallocate(par1_coef, stat=stat)
         
         !/  Creation de la structure MSP_AERO a partir des coefficients de portance, 
         !/  de frottement et de la structure PS_STR_CARACTERISTIQUES          
         aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
              type_variation = MSP_ENUM_CONSTANT,cmf=pscar%cf,cxa=cxa,czn=czn)

      case (3)
         !/  Cas de coefficients de frottement lus dans un fichier AERO
         !/  Creation de la structure MSP_AERO a partir de la structure PS_STR_CARACTERISTIQUES 
         aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
              type_variation = MSP_ENUM_INCIDENCE_MACH,ficaero=pscar%ficaero,cmf=pscar%cf)

      end select

      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return

!/  Creation de la structure MSP_VEHICULE
      call MSP_effacer_vehicule (vehicule)
      vehicule = MSP_creer_vehicule (mci=mci,aero=aero,prad=prad)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return

      ! Libération de la mémoire
      call MSP_effacer_prad (prad)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return
      call MSP_effacer_aero (aero)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return
      call MSP_effacer_mci (mci)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return
      call MSP_effacer_coef (cxa)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return
      call MSP_effacer_coef (czn)
      if ( MSP_gen_messages("psconv_pscar_vehicule") ) return
      
   end subroutine psconv_pscar_vehicule

   subroutine psconv_mod_psmod (mod,psmod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_mod_psmod
!
!$Resume
!  Conversion d'une structure de type MSP_MODELE à une structure de type PS_STR_MODELES. 
!
!$Description
!  Conversion d'une structure de type MSP_MODELE à une structure de type PS_STR_MODELES. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_mod_psmod (mod,psmod)
!.    type (MSP_MODELE) :: mod
!.    type (PS_STR_MODELES) :: psmod
!
!$Arguments
!>E     mod    :<MSP_MODELE>       modèles utilisés de type MSP_MODELE
!>S     psmod  :<PS_STR_MODELES>   modèles utilisés de type PS_STR_MODELES 
!
!$Common
!
!$Routines
!- MSP_effacer_potentiel
!- MSP_effacer_atmosphere
!- MSP_modifier_trois_corps
!- MSP_modifier_pre_solaire
!- MSP_consulter_modele
!- MSP_consulter_trois_corps
!- MSP_consulter_atmosphere
!- MSP_consulter_potentiel
!- MSP_signaler_message
!- MSP_consulter_pre_solaire
!
!$Include
!
!$Module
!#V
!- EPHEM
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     use EPHEM

      implicit none

      ! Arguments
      !==========
      type (MSP_MODELE), intent(IN)      :: mod
      type (PS_STR_MODELES), intent(OUT) :: psmod

      ! Variables locales
      !==================
      type (MSP_potentiel) :: mod_pot
      type(MSP_atmosphere) :: mod_atm
      type (MSP_PRE_SOLAIRE) :: mod_prad
      type (MSP_TROIS_CORPS) :: mod_tcps
      type(MSP_ATM_EXP) :: atm_exp

      character(LEN=80) :: atm_modele,act_nomfic
      character(LEN=80)  :: tcps_ficept,fic_ept_tmp
      integer :: atm_actsol,atm_russe_mod,atm_emcd_sce,tcps_soleil,tcps_lune,prad_type,pot_nzo,pot_nte
      real(kind=pm_reel) :: atm_flux,atm_ap,atm_lambda_gw,atm_dt(8),atm_emcd_per(2)

      integer :: trouve, base_potentiel, ind, i, atm_dateref
      integer, dimension(3) :: corps
      character(len=cps_maxlg), dimension(:), pointer :: modelesCorps
      character(LEN=256) :: fichier, pot_nomfic
      character(len=128) :: fichier_compas, fichier_msp
      character(LEN=32) :: modpot
      integer :: lg_chaine_compas, stat
      

      
      ! Début du code
      !==============
      nullify(modelesCorps)
      corps(1) = eph_terre
      corps(2) = eph_mars
      corps(3) = eph_venus
      base_potentiel = 1 ! base de référence 

      call MSP_effacer_potentiel(mod_pot,nul=.true.)
      call MSP_effacer_atmosphere(mod_atm,nul=.true.)
      call MSP_modifier_trois_corps (mod_tcps,soleil=0,lune=0)
      call MSP_modifier_pre_solaire(mod_prad,type=0)

!/  Extraction des structure MSP_potentiel (modele de potentiel de la planete), MSP_atmosphere (modele atmospherique de 
!/  la planete), MSP_PRE_SOLAIRE (pression de radiation solaire), MSP_TROIS_CORPS ( donnees liees au troisieme corps)
      call MSP_consulter_modele (mod,potentiel=mod_pot,atmosphere=mod_atm,pre_solaire=mod_prad,trois_corps=mod_tcps)
      if ( MSP_gen_messages("psconvsmod_psmod") ) return

!/  Lecture de la structure MSP_TROIS_CORPS
      call MSP_consulter_trois_corps (mod_tcps,soleil=tcps_soleil,lune=tcps_lune,ficept=tcps_ficept)
      if ( MSP_gen_messages("psconvsmod_psmod") ) return

!/  ecriture dans la structure PS_STR_MODELES (modele atmospherique de PSIMU)
      psmod%ikle(1) = tcps_lune
      psmod%ikle(2) = tcps_soleil

!/  Lecture de la structure MSP_atmosphere (modele atmospherique de la planete)
      call MSP_consulter_atmosphere (mod_atm,modele=atm_modele,actsol=atm_actsol,&
                                     exp=atm_exp,&
                                     ficactsol=act_nomfic,dateref_actsol=atm_dateref,flux=atm_flux,&
                                     ap=atm_ap,us76_deltat=atm_dt, &
                                     mars_russe_mod=atm_russe_mod,mars_emcd_sce=atm_emcd_sce, &
                                     mars_emcd_per=atm_emcd_per,mars_emcd_lambda_gw=atm_lambda_gw )
      if ( MSP_gen_messages("psconvsmod_psmod") ) return

!/  ecriture dans la structure de psimu PS_STR_MODELES
      psmod%modatm      = atm_modele
      if ( psmod%modatm == "" ) then
         psmod%ikle(3)     = 0
      else
         if (atm_actsol == MSP_ENUM_ACTSOL_COMPAS) then
            ! Activité solaire réelle COMPAS
            psmod%ikle(3) = 1
	 else if (atm_actsol == MSP_ENUM_ACTSOL_REELLE) then
	    ! Activité solaire réelle sur fichier
	    psmod%ikle(3) = 3
	 else if (atm_actsol == MSP_ENUM_ACTSOL_STD) then
	    ! Activité solaire standard
	    psmod%ikle(3) = 2
	 else
	    psmod%ikle(3) = 0
	 endif
	     
         !/ Modification des noms des modeles d atmosphere differents entre MECASPA et PSIMU
         if (trim(psmod%modatm).eq."CIRA88_MSIS86") then
            psmod%modatm = "CIRA_MSIS86"
         elseif (trim(psmod%modatm).eq."DTM") then
            psmod%modatm = "DTM78"
         elseif (trim(psmod%modatm).eq."US76") then
            psmod%modatm = "US76D"
         elseif (trim(psmod%modatm).eq."AT77") then
            psmod%modatm = "ROAT77"
         elseif (trim(psmod%modatm).eq."MSIS_90") then
            psmod%modatm = "MSIS90"
         elseif (trim(psmod%modatm).eq."NRL-MISE_2000") then
            psmod%modatm = "MSIS2000"
         elseif (trim(psmod%modatm).eq."MET-88") then
            psmod%modatm = "MET88"
         endif
      endif
      psmod%ficactsol   = act_nomfic
      psmod%dateref_actsol = atm_dateref
      if (psmod%dateref_actsol < 0) then
         psmod%mode_avance_actsol = 0
      else
         psmod%mode_avance_actsol = 1
      end if
      psmod%flu         = atm_flux
      psmod%app         = atm_ap
      psmod%deltat(:)   = atm_dt(:)    
      psmod%param_atm_exp(1) = atm_exp%ro0
      psmod%param_atm_exp(2) = atm_exp%h0
      psmod%param_atm_exp(3) = atm_exp%altmin
      psmod%param_atm_exp(4) = atm_exp%altmax
      psmod%param_atm_exp(5) = real(atm_exp%tscale)
      psmod%param_atm_exp(6) = atm_exp%hscale
      psmod%param_atm_exp(7) = atm_exp%beta
      psmod%typper(:)   = atm_emcd_per(:)
      psmod%lambda_gw   = atm_lambda_gw
      psmod%scena       = atm_emcd_sce
      psmod%typemod     = atm_russe_mod

!/  Lecture de la structure MSP_potentiel (modele de potentiel de la planete)
      call MSP_consulter_potentiel (mod_pot,nomfic=pot_nomfic,nzo=pot_nzo,nte=pot_nte)
      if ( MSP_gen_messages("psconvsmod_psmod") ) return

     

!/  ecriture dans la structure de psimu PS_STR_MODELES

      !   Traduction fichier potentiel --> modèle potentiel DM560
      do i=1,3
         trouve = cps_getListModelesCorps(corps(i), "potentiel", modelesCorps, base_potentiel)
         if(trouve == CPS_OK) then
            do ind=1,size(modelesCorps)
               ! fichier du modèle
               trouve = cps_getFichierModele("potentiel", trim(modelesCorps(ind)), fichier, .true.)
               if (trouve /= CPS_OK) then
                  call MSP_signaler_message (cle_mes="PS_INIT_POT_001",partie_variable=trim(psmod%modpot),&
                       routine="psconv_mod_psmod")
                  return
               end if

               fichier_compas = trim(fichier)
               lg_chaine_compas = len_trim(fichier_compas)

               fichier_msp = ""
               fichier_msp = pot_nomfic(1:lg_chaine_compas)

               if (trim(fichier_compas) .eq. trim(fichier_msp)) then
                  modpot = trim(modelesCorps(ind))
               end if
            end do
         end if
         ! FA-ID 746 : suppresison de modelescorps devenu inutile à ce stade
         if (associated(modelesCorps)) deallocate(modelesCorps, stat=stat)
      end do
      psmod%modpot      = modpot
      psmod%nzo         = pot_nzo
      psmod%nte         = pot_nte

!/  Lecture de la structure MSP_PRE_SOLAIRE (Pression de radiation solaire)
      call MSP_consulter_pre_solaire(mod_prad,type=prad_type,ficept=fic_ept_tmp)
      if ( MSP_gen_messages("psconvsmod_psmod") ) return

      if(trim(tcps_ficept) == "") then
         ! Fichier d'ephemerides 
         tcps_ficept = fic_ept_tmp
      end if
!/  ecriture dans la structure de psimu PS_STR_MODELES
      psmod%ikle(4)     = prad_type


      if ( (psmod%ikle(1)/=MSP_ENUM_NUL) .or.(psmod%ikle(2)/=MSP_ENUM_NUL) .or. &
           & (psmod%ikle(3) >= 1) .or. (psmod%ikle(4)/=MSP_ENUM_NUL) ) then
         if(trim(tcps_ficept) /= "") then
            !/  éphémérides de Tchebytchev
            psmod%ikle(5)     = 1
            psmod%ficept = tcps_ficept
         else
            !/  éphémérides analytique 
            psmod%ikle(5)     = 2
         end if
      endif 

      ! Libération de la mémoire
      call MSP_effacer_potentiel (mod_pot)
      if ( MSP_gen_messages("psconv_mod_psmod") ) return
      call MSP_effacer_atmosphere (mod_atm)
      if ( MSP_gen_messages("psconv_mod_psmod") ) return

      ! FA-ID 746 : pour être sur que modelescorps est desalloué
      if (associated(modelesCorps)) deallocate(modelesCorps, stat=stat)

   end subroutine psconv_mod_psmod

   subroutine psconv_psmod_mod (psmod,mod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_psmod_mod
!
!$Resume
!  Conversion d'une structure de type PS_STR_MODELES à une structure de type MSP_MODELE. 
!
!$Description
!  Conversion d'une structure de type PS_STR_MODELES à une structure de type MSP_MODELE. 
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_psmod_mod (psmod,mod)
!.    type (PS_STR_MODELES) :: psmod
!.    type (MSP_MODELE) :: mod
!
!$Arguments
!>E     psmod    :<PS_STR_MODELES>   modèles utilisés de type PS_STR_MODELES
!>S     mod      :<MSP_MODELE>       modèles utilisés de type MSP_MODELE
!
!$Common
!
!$Routines
!- MSP_effacer_potentiel
!- MSP_effacer_atmosphere
!- MSP_modifier_trois_corps
!- MSP_modifier_pre_solaire
!- MSP_signaler_message
!- MSP_affecter_potentiel
!- MSP_effacer_atm_emcd
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
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none
      
      ! Arguments
      !==========
      type (PS_STR_MODELES), intent(IN)  :: psmod
      type (MSP_MODELE), intent(OUT)     :: mod


      ! Variables locales
      !==================
      type (MSP_POTENTIEL)   :: pot_tmp
      type (MSP_ATMOSPHERE)  :: atm_tmp
      type (MSP_PRE_SOLAIRE) :: pre_tmp
      type (MSP_TROIS_CORPS) :: trc_tmp

      integer :: nzo_tmp,nte_tmp,ier
      integer :: longueur, trouve, actsol
      character(LEN=256) :: rctmp,dirept,ficept, fichier_pot
      type(MSP_ATM_EMCD) :: str_emcd
      type(MSP_ATM_EXP) :: atm_exp
      character(LEN=256) :: rep_atm_emcd
      logical :: loc = .false.

      nzo_tmp = psmod%nzo
      nte_tmp = psmod%nte

      ! Effacement des structures (initialisation)
      !===========================================
      call MSP_effacer_potentiel(pot_tmp,nul=.true.)
      call MSP_effacer_atmosphere(atm_tmp,nul=.true.)
      call MSP_modifier_trois_corps (trc_tmp,soleil=0,lune=0)
      call MSP_modifier_pre_solaire(pre_tmp,type=0)

!/  Récupération des données incluses dans les fichiers de ressource

      ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="data_psimu",routine="psconv_psmod_mod")
         return
      else
         dirdat = rctmp(1:longueur)
      endif

      ! Récupération des informations "éphémérides" de la base locale
      ! Recherche du répertoire où sont stockés les fichiers Tchebytchev
      call eph_infogetLocal(PSI_METHODE_EPH_TCHEMAD, loc, repertoire=dirept)
      if (.not.loc) then
      	call eph_infoget(PSI_METHODE_EPH_TCHEMAD, repertoire=dirept)
      end if
      if ( MSP_ERREUR ) then
 	call MSP_signaler_message (cle_mes="PS_ERR_FIC_EPH", routine="psconv_psmod_mod")
	return
      endif


!/  Creation d'un potentiel et le copier dans  pot_tmp (temporaire)

       ! Affectation du fichier potentiel, d'après le nom du modèle COMPAS
       trouve = cps_getFichierModele("potentiel", trim(psmod%modpot), fichier_pot, .true.)
       if (trouve /= CPS_OK) then
          call MSP_signaler_message (cle_mes="PS_INIT_POT_001",partie_variable=trim(psmod%modpot),routine="psconv_psmod_mod")
         return
       end if

       ! Le nom du fichier potentiel est traduit par COMPAS, à partir du nom 
       ! d'un modèle de potentiel, choisi dans COMPAS.
       ! La structure MECASPA est remplie par la MECASPA, qui appelle COMPAS
       ! pour la lecture des potentiels. Tous les potentiels utilisés par PSIMU
       ! sont au format GRGS.

       ! Note : on stocke le nom du potentiel dans la structure MECASPA
      call MSP_affecter_potentiel(pot_tmp, &
                                  MSP_creer_potentiel(nomfic=fichier_pot,&
                                  nzo=nzo_tmp,nte=nte_tmp, nom_pot= trim(psmod%modpot)) )
      if ( MSP_gen_messages("psconv_psmod_mod") ) return

!/  Creation d'un modele atmospherique a partir de la structure de psimu (PS_STR_MODELES) -->  atm_tmp
!/  est provisoire
      call MSP_effacer_atm_emcd(str_emcd)

      !/ Construction d'un éventuel objet MSP_ATM_EXP
      if(psmod%modatm == MSP_MODATM_EXP ) then
         atm_exp = MSP_creer_atm_exp(hscale = psmod%param_atm_exp(6), &
               h0 = psmod%param_atm_exp(2), &
               ro0 = psmod%param_atm_exp(1),&
               beta = psmod%param_atm_exp(7), &
               tscale = int(psmod%param_atm_exp(5)), &
               altmin = psmod%param_atm_exp(3), &
               altmax = psmod%param_atm_exp(4) )
      endif

      !/ Construction d'un éventuel objet MSP_ATM_EMCD
      if( psmod%modatm == MSP_MODATM_MARS_EMCD_31 .or. &
           psmod%modatm == MSP_MODATM_MARS_EMCD_42 .or. &
           psmod%modatm == MSP_MODATM_MARS_EMCD_43) then
         !/ Recherche du répertoire de données EMCD 3.1, 4.2 ou 4.3(nommé "MCD" dans GS_LIB) dans COMPAS 
         !/ Le ".true." permet de récupérer le chemin absolu 
         !/ ie : répertoire des modèles + ssrép du modèle EMCD 3.1, 4.2 ou 4.3
	  
         trouve =  cps_getFichierModele("atmosphere",psmod%modatm,rep_atm_emcd,.true.)
         if (trouve /= CPS_OK) then 
            if(MSP_gen_messages("psconv_psmod_mod")) return
         end if
	  
         str_emcd = MSP_creer_atm_emcd(mars_emcd_sce=psmod%scena,mars_emcd_per=psmod%typper,lambda_gw=psmod%lambda_gw,&
                                   dir_emcd=trim(rep_atm_emcd))
      end if

      ! Conversion du type d'activité solaire (pour compatibilité GSLIB/MECASPA: DM842)
      if (psmod%ikle(3) == 1) then
         ! ikle(3)=1 => activité réelle COMPAS
         actsol = MSP_ENUM_ACTSOL_COMPAS
      else if (psmod%ikle(3) == 3) then  
         ! ikle(3)=3 => activité réelle sur fichier
         actsol = MSP_ENUM_ACTSOL_REELLE
      else if (psmod%ikle(3) == 2) then
         actsol = MSP_ENUM_ACTSOL_STD
      else
         actsol = 0
      endif 

      if (psmod%mode_avance_actsol == 1) then
         ! Mode avancé : une date de référence est fournie
         atm_tmp = MSP_creer_atmosphere(modele=psmod%modatm,actsol=actsol,ficactsol=psmod%ficactsol,&
	                                dateref_actsol=psmod%dateref_actsol,&
                                        flux=psmod%flu,ap=psmod%app,us76_deltat=psmod%deltat, &
                                        mars_russe_mod=psmod%scena,exp=atm_exp,emcd=str_emcd)
         if ( MSP_gen_messages("psconv_psmod_mod") ) return
      else
         ! Pas de date de référence
         atm_tmp = MSP_creer_atmosphere(modele=psmod%modatm,actsol=actsol,ficactsol=psmod%ficactsol,&
                                        flux=psmod%flu,ap=psmod%app,us76_deltat=psmod%deltat, &
                                        mars_russe_mod=psmod%scena,exp=atm_exp,emcd=str_emcd)
         if ( MSP_gen_messages("psconv_psmod_mod") ) return
      end if
      				     
      if ( MSP_gen_messages("psconv_psmod_mod") ) return

      !/ Si on utilise les éphémérides analytiques, on force la chaine ficept à la chaîne vide
      if(psmod%ikle(5) == 2 .or. psmod%ikle(5) == 0) then
         ficept = ""
      else
         ficept = trim(psmod%ficept)
      end if


!/  Creation de la structure de la mecaspa (MSP_PRE_SOLAIRE) liee aux pressions de radiation solaire
!/    a partir de la structure de psimu (PS_STR_MODELES)
      pre_tmp = MSP_creer_pre_solaire(type=psmod%ikle(4),ficept=ficept,dirept=dirept)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return

!/  Creation de la structure de la mecaspa (MSP_TROIS_CORPS) liee au troisieme corps
!/    a partir de la structure de psimu (PS_STR_MODELES)
      trc_tmp = MSP_creer_trois_corps(lune=psmod%ikle(1),soleil=psmod%ikle(2),&
                                      ficept=ficept,dirept=dirept)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return

!/  Creation de la structure modele de la mecaspa (MSP_MODELE) a partir des structures MSP_PRE_SOLAIRE, 
!/  MSP_TROIS_CORPS et MSP_PRE_SOLAIRE
      mod = MSP_creer_modele (potentiel=pot_tmp,atmosphere=atm_tmp,pre_solaire=pre_tmp,trois_corps=trc_tmp)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return


!/  Destruction des Strucures de la mecaspa liees  au potentiel et au modele atmospherique
      call MSP_effacer_potentiel (pot_tmp)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return
      call MSP_effacer_atm_emcd(str_emcd)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return
      call MSP_effacer_atmosphere (atm_tmp)
      if ( MSP_gen_messages("psconv_psmod_mod") ) return
                                                  
   end subroutine psconv_psmod_mod

   subroutine psconv_psmod_vent(psmod,modvent)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_psmod_vent
!
!$Resume
!  Conversion d'une structure PSIMU vers une structure MECASPA MSP_MODVENT
!  -> seuls les champs concernant le vent serviront à créer la structure MECASPA
!
!$Description
!  Conversion d'une structure PSIMU vers une structure MECASPA MSP_MODVENT
!  -> seuls les champs concernant le vent serviront à créer la structure MECASPA
!
!$Auteur
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_psmod_vent(psmod,modvent)
!.    type (PS_STR_MODELES) :: psmod
!.    type (MSP_MODVENT) :: modvent
!
!$Arguments
!>E     psmod    :<PS_STR_MODELES>   structure PSIMU
!>S     modvent  :<MSP_MODVENT>      structure MECASPA
!
!$Common
!
!$Routines
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
      !==========
      type (PS_STR_MODELES), intent(IN)  :: psmod
      type (MSP_MODVENT), intent(OUT)     :: modvent
      
      ! Variables locales
      !==================
      character(len=80) :: modele_vent
      character(len=256) :: fic_mod_vent
      integer :: trouve

      ! Début du code
      !==============

      if(psmod%ikle(6) /= 0) then
         !/ Si un modèle de vent existe
         
         modele_vent = psmod%modvent
         
         ! (DM-ID 643) lecture du modele de vent, s'il existe : 
         ! on cherche d'abord si le fichier existe càd si le modèle existe
         trouve = cps_getFichierModele("vent", modele_vent, fic_mod_vent, &
              .true.)
         if (trouve == CPS_OK)  then
            modvent = MSP_lire_modvent(modele_vent)
         else
            call MSP_signaler_message(cle_mes="PS_ATM_INIT_VENT",partie_variable=trim(modele_vent))
            return
         end if
      end if

   end subroutine psconv_psmod_vent

   subroutine psconv_vent_psmod(modvent,psmod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psconv_vent_psmod
!
!$Resume
! Conversion d'une structure vent MSP_MODVENT vers les champs ad hoc
! de la structure ps_str_modeles.
!
!$Description
! Conversion d'une structure vent MSP_MODVENT vers les champs ad hoc
! de la structure ps_str_modeles.
!
!$Auteur
! Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psconv_vent_psmod(modvent,psmod)
!.    type (MSP_MODVENT) :: modvent
!.    type (PS_STR_MODELES) :: psmod
!
!$Arguments
!>E     modvent  :<MSP_MODVENT>      structure MECASPA
!>S     psmod    :<PS_STR_MODELES>   structure PSIMU (certains champs seront remplis)
!
!$Common
!
!$Routines
!- Msp_consulter_modvent
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
      !==========
      type (MSP_MODVENT), intent(IN)       :: modvent
      type (PS_STR_MODELES), intent(OUT)  :: psmod

      ! Variables locales
      !==================
      character(len=80) :: nom_modele
      integer :: type_vent
      integer :: trouve
      character(len=256) :: fic_mod_vent
      
      ! Début du code
      !==============
      nom_modele=""
      type_vent=0

      call Msp_consulter_modvent(modvent,nom=nom_modele,modelevent=type_vent)
      
      if (nom_modele == "" .or. type_vent == 0) then
         ! La structure MSP_MODVENT est vide : on annule la prise en compte du modèle de vent
         psmod%ikle(6) = 0
         psmod%modvent = ""

      else
         ! La structure MSP_MODVENT n'est pas vide ; on recherche le fichier de modèle de vent
         
         trouve = cps_getFichierModele("vent", nom_modele , fic_mod_vent, &
              .true.)
         if (trouve == CPS_OK ) then
            psmod%ikle(6) = type_vent
            psmod%modvent = nom_modele
         else
            call MSP_signaler_message(cle_mes="PS_ATM_INIT_VENT",partie_variable=trim(nom_modele))
            return
         end if

      end if

   end subroutine psconv_vent_psmod

  type(PS_STR_INTEGRATION) function ps_creer_strinteg (h1, h2, hstop, typdat, h3, xpash0, xpash1, xpash2, xpash3, &
       second_membre_simp0,second_membre_simp1,tmax, jj_datemax, sec_datemax, type_alt, pinteg0, pinteg1, pinteg2,&
       pinteg3) result(donnees_integ)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_creer_strinteg
!
!$Resume
!  Fonction de création d'une structure PS_STR_INTEGRATION
!
!$Description
!  Fonction de création d'une structure PS_STR_INTEGRATION
!  -> gère les structures MSP_INTEGRATOR, le type d'altitude, les critères d'arrêt
!
!$Acces
!  PUBLIC
!
!$Usage
!  donnees_integ = ps_creer_strinteg (h1, h2, hstop, typdat, [h3], [xpash0], [xpash1], [xpash2], [xpash3], &
!.           [second_membre_simp0],[second_membre_simp1],[tmax], [jj_datemax], [sec_datemax], [type_alt], [pinteg0], [pinteg1], [pinteg2],&
!.           [pinteg3])
!.    real(kind=pm_reel) :: h1
!.    real(kind=pm_reel) :: h2
!.    real(kind=pm_reel) :: hstop
!.    integer :: typdat
!.    real(kind=pm_reel) :: h3
!.    real(kind=pm_reel) :: xpash0
!.    real(kind=pm_reel) :: xpash1
!.    real(kind=pm_reel) :: xpash2
!.    real(kind=pm_reel) :: xpash3
!.    integer :: second_membre_simp0
!.    integer :: second_membre_simp1
!.    real(kind=pm_reel) :: tmax
!.    integer :: jj_datemax
!.    real(kind=pm_reel) :: sec_datemax
!.    integer :: type_alt
!.    type(MSP_INTEGRATOR) :: pinteg0
!.    type(MSP_INTEGRATOR) :: pinteg1
!.    type(MSP_INTEGRATOR) :: pinteg2
!.    type(MSP_INTEGRATOR) :: pinteg3
!.    type ( PS_STR_INTEGRATION) :: donnees_integ
!
!$Arguments
!>E     h1                   :<pm_reel>              Altitude seuil pour le passage au pas de base (Cowell)
!>E     h2                   :<pm_reel>              Altitude seuil pour le passage au pas fin du Cowell
!>E     hstop                :<pm_reel>              Altitude d'arrêt
!>E     typdat               :<integer>              Type de date pour l'arrêt de l'intégration (1=date, 2=durée)
!>[E]   h3                   :<pm_reel>              Altitude max pour l'utilisation du Gill en altitudes très basses
!>[E]   xpash0               :<pm_reel>              Pas d'intégration de base pour le Cowell
!>[E]   xpash1               :<pm_reel>              Pas d'intégration fin du Cowell
!>[E]   xpash2               :<pm_reel>              Pas d'intégration fin du Gill
!>[E]   xpash3               :<pm_reel>              Pas d'intégration très fin du Gill
!>[E]   second_membre_simp0  :<integer>              Utilisation du second membre simplifié(1) ou non(0) pour les altitudes hautes
!>[E]   second_membre_simp1  :<integer>              Utilisation du second membre simplifié(1) ou non(0) pour les altitudes moyennes
!>[E]   tmax                 :<pm_reel>              Durée max d'intégration
!>[E]   jj_datemax           :<integer>              Date de fin d'intégration (jours)
!>[E]   sec_datemax          :<pm_reel>              Date de fin d'intégration (nb de secondes)
!>[E]   type_alt             :<integer>              Type d'altitude (0 = géocentrique, 1 = géodésique) pour le critère d'arrêt
!>[E]   pinteg0              :<MSP_INTEGRATOR>       Paramètres d'intégration pour Cowell en altitudes hautes (prioritaire sur xpash0)
!>[E]   pinteg1              :<MSP_INTEGRATOR>       Paramètres d'intégration pour Cowell en altitudes moyennes (prioritaire sur xpash1)
!>[E]   pinteg2              :<MSP_INTEGRATOR>       Paramètres d'intégration pour Gill en altitudes basses (prioritaire sur xpash2)
!>[E]   pinteg3              :<MSP_INTEGRATOR>       Paramètres d'intégration pour Gill en altitudes très basses (prioritaire sur xpash3)
!>S     donnees_integ        :<PS_STR_INTEGRATION>   Structure PS_STR_INTEGRATION
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- MSP_INTEGRATOR_DEF
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

    use MSP_INTEGRATOR_DEF

    implicit none

    ! Arguments
    !==========
    real(kind=pm_reel), intent(in) :: h1
    real(kind=pm_reel), intent(in) :: h2
    real(kind=pm_reel), intent(in) :: hstop
    integer, intent(in)            :: typdat
    
    real(kind=pm_reel), intent(in), optional :: h3
    real(kind=pm_reel), intent(in), optional :: xpash0
    real(kind=pm_reel), intent(in), optional :: xpash1
    real(kind=pm_reel), intent(in), optional :: xpash2
    real(kind=pm_reel), intent(in), optional :: xpash3
    
    integer, intent(in), optional  :: second_membre_simp0
    integer, intent(in), optional  :: second_membre_simp1

    real(kind=pm_reel), intent(in), optional :: tmax
    integer,            intent(in), optional :: jj_datemax
    real(kind=pm_reel), intent(in), optional :: sec_datemax
    
    integer, intent(in), optional :: type_alt

    type(MSP_INTEGRATOR), intent(in), optional :: pinteg0
    type(MSP_INTEGRATOR), intent(in), optional :: pinteg1
    type(MSP_INTEGRATOR), intent(in), optional :: pinteg2
    type(MSP_INTEGRATOR), intent(in), optional :: pinteg3

    ! Include
    !========

#include "parameter_gslib.h"

    ! Variables locales
    !==================
    integer, dimension(6) :: indic_lib
    integer :: secd_mb_simp0
    integer :: secd_mb_simp1

    ! Début du code
    !==============
    donnees_integ%h1   = h1 
    donnees_integ%h2   = h2
    donnees_integ%hstop= hstop

    ! Indicateurs de liberation des paramètres : desactivés dans PSIMU
    indic_lib(:) = -1


    if (present(h3)) then
       donnees_integ%h3   = h3
    else
       donnees_integ%h3   = 20000._pm_reel
    end if

    if (present(type_alt)) then
       donnees_integ%type_alt = type_alt
    else
       donnees_integ%type_alt = GS_PS_ALT_GEOCENTRIQUE
    end if
    
    if (present(second_membre_simp0)) then
       secd_mb_simp0=second_membre_simp0
    else 
       secd_mb_simp0=1
    end if
    
    if (present(second_membre_simp1)) then      
       secd_mb_simp1=second_membre_simp1
    else
       secd_mb_simp1=1
    end if


    if (present(pinteg0)) then
       donnees_integ%pinteg0 = pinteg0
    else if (present(xpash0)) then
       donnees_integ%pinteg0 = MSP_creer_integrator(type_integrateur=pm_cowell,&
            pas=xpash0,ordre=MSP_ordre_cowell_defaut,circularisation=MSP_circularisation_defaut,&
            second_membre_simp=secd_mb_simp0,convergence=MSP_convergence_defaut,&
            epsilon_init=MSP_epsilon_init_defaut,epsilon_prog=MSP_epsilon_prog_defaut,indic_lib=indic_lib)
    else
       ! Erreur : il faut au moins remplir le pas xpash0
       call MSP_signaler_message(cle_mes="PS_INTEGRATION_ERR_STRUCTURE",&
            partie_variable="le pas xpash0 est manquant / xpash0 step is missing")
       return
    end if

    if (present(pinteg1)) then
       donnees_integ%pinteg1 = pinteg1
    else if (present(xpash1)) then
       donnees_integ%pinteg1 = MSP_creer_integrator(type_integrateur=pm_cowell,&
            pas=xpash1,ordre=MSP_ordre_cowell_defaut,circularisation=MSP_circularisation_defaut,&
            second_membre_simp=secd_mb_simp1,convergence=MSP_convergence_defaut,&
            epsilon_init=MSP_epsilon_init_defaut,epsilon_prog=MSP_epsilon_prog_defaut,indic_lib=indic_lib)
    else
       ! Erreur : il faut au moins remplir le pas xpash1
       call MSP_signaler_message(cle_mes="PS_INTEGRATION_ERR_STRUCTURE",&
            partie_variable="le pas xpash1 est manquant / xpash1 step is missing")
       return
    end if

    if (present(pinteg2)) then
       donnees_integ%pinteg2 = pinteg2
    else if (present(xpash2)) then
       donnees_integ%pinteg2 = MSP_creer_integrator(type_integrateur=pm_gill,pas=xpash2)
    else
       ! Erreur : il faut au moins remplir le pas xpash2
       call MSP_signaler_message(cle_mes="PS_INTEGRATION_ERR_STRUCTURE",&
            partie_variable="le pas xpash2 est manquant / xpash2 step is missing")
       return
    end if

    if (present(pinteg3)) then
       donnees_integ%pinteg3 = pinteg3
    else if (present(xpash3)) then
       donnees_integ%pinteg3 = MSP_creer_integrator(type_integrateur=pm_gill,pas=xpash3)
    else
       ! Init par défaut, comme c'était fait auparavant
       donnees_integ%pinteg3 = MSP_creer_integrator(type_integrateur=pm_gill,pas=1._pm_reel)
    end if
    
    donnees_integ%typdat = typdat

    if(donnees_integ%typdat == 1) then
       ! Date finale
       if(present(jj_datemax) .and. present(sec_datemax)) then
          donnees_integ%rdatmax = sec_datemax
          donnees_integ%idatmax = jj_datemax
       else
          call MSP_signaler_message(cle_mes="PS_INTEGRATION_ERR_STRUCTURE",&
               partie_variable="La variable jj_datemax ou sec_datemax est manquante / jj_datemax or sec_datemax is missing")
          return
       end if
    else
       ! Durée d'intégration
       if(present(tmax)) then
          donnees_integ%tmax = tmax
       else
          call MSP_signaler_message(cle_mes="PS_INTEGRATION_ERR_STRUCTURE",&
               partie_variable="La variable tmax est manquante / tmax is missing")
          return
       end if
    end if
    
  end function ps_creer_strinteg

   subroutine ps_analyser_maj_vetat(don_ecr, maj_vect_etat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_analyser_maj_vetat
!
!$Resume
!  Routine permettant de mettre à jour les flags liés à la mise à jour
!  du vecteur d'état
!
!$Description
!  L'analyse des variables sélectionnées permet de connaître quels sont
!  les éléments à mettre à jour dans le vecteur d'état à chaque pas de 
!  sorties.
!  Chaque bloc d'instructions est commandé par un booléen contenu dans
!  la structure  PS_STR_MAJ_VECT_ETAT.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_analyser_maj_vetat(don_ecr, maj_vect_etat)
!.    type (PS_STR_ECRITURE) :: don_ecr
!.    type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
!
!$Arguments
!>E     don_ecr        :<PS_STR_ECRITURE>        Structure contenant les variables
!                                                de sorties sélectionnées
!>E/S   maj_vect_etat  :<PS_STR_MAJ_VECT_ETAT>   Structure contenant les flags de
!                                                mise à jour du vecteur d'état
!
!$Common
!
!$Routines
!- psi_analyse_maj_kep
!- psi_analyse_maj_posvit_geo
!- psi_analyse_maj_forces
!- psi_analyse_maj_ris_rts
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
      type (PS_STR_ECRITURE), intent(in) :: don_ecr
      type (PS_STR_MAJ_VECT_ETAT), intent(inout) :: maj_vect_etat

      ! Definition des différents lots de variables
      ! integer, dimension(15), parameter :: vars_car_ris = &
      !     (/ 1, 2, 3, 4, 5, 9, 10, 11, 41, 106, 113, 114, 115, 116, 117/)
      integer, dimension(5), parameter :: vars_autres = &
           (/ 15, 16, 17, 100, 101 /)

      ! Correspondance des indices aux flags
      ! integer, parameter :: nb_vars_car_ris = 15
      integer, parameter :: nb_vars_autres  = 5

      ! Indice de parcours des lots predifinis
      integer :: ii
      ! Indice de paracours des variables selectionnées
      integer :: jj

      ! Mise a jour du flag minimum pour les positions cartésiennes dans le RIS
      ! Le cas échéant, cela pose des problèmes lorsque l'on ne spécifia aucune variable
      maj_vect_etat%calcul_car_ris    = .true.

      ! Inits des flags
      maj_vect_etat%calcul_att_ris = .false.
      maj_vect_etat%calcul_car_rts = .false.
      maj_vect_etat%calcul_att_rts = .false.
      maj_vect_etat%calcul_pos_geo = .false.
      maj_vect_etat%calcul_vit_geo = .false.
      maj_vect_etat%calcul_kep     = .false.
      maj_vect_etat%calcul_forces  = .false.
      maj_vect_etat%calcul_force_prop = .false.
      maj_vect_etat%calcul_kep_moy = .false.
      maj_vect_etat%calcul_autres  = .false.

      ! Traitement particulier du cas sortilege
      if ( don_ecr%itypsort == 2 ) then
         maj_vect_etat%calcul_car_ris    = .true.
         maj_vect_etat%calcul_att_ris    = .true.
         return
      endif 


      ! Recherche de la (jj)ième variable lue parmi les variables
      ! du lot correspondant au "calcul_autres"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_autres & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_autres & 
              .and. ii <= nb_vars_autres ) 
            if ( don_ecr%iresw(jj) == vars_autres(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_att_ris    = .true.
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_att_rts    = .true.
               maj_vect_etat%calcul_pos_geo    = .true.
               maj_vect_etat%calcul_vit_geo    = .true.
               maj_vect_etat%calcul_kep        = .true.
               maj_vect_etat%calcul_forces     = .true.
               maj_vect_etat%calcul_force_prop = .true.
               maj_vect_etat%calcul_kep_moy    = .true.
               maj_vect_etat%calcul_autres     = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour les lots correspondant au "calcul_kep" et "calcul_kep_moy"  
      call psi_analyse_maj_kep(don_ecr, maj_vect_etat)

      ! Idem pour les lots correspondant au "calcul_pos_geo" et  "calcul_vit_geo"
      call psi_analyse_maj_posvit_geo(don_ecr, maj_vect_etat)

      ! Idem pour les lots correspondant au "calcul_forces" et "calcul_force_prop"
      call psi_analyse_maj_forces(don_ecr, maj_vect_etat)

      ! Idem pour les lots correspondant au "calcul_att_ris", "calcul_att_rts"
      ! et "calcul_car_rts"
      call psi_analyse_maj_ris_rts(don_ecr, maj_vect_etat)

   end subroutine ps_analyser_maj_vetat


   subroutine psi_analyse_maj_kep(don_ecr, maj_vect_etat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_analyse_maj_kep
!
!$Resume
!  Analyse des variables liés aux paramètres orbitaux
!
!$Description
!  Analyse des variables liés aux paramètres orbitaux
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_analyse_maj_kep(don_ecr, maj_vect_etat)
!.    type (PS_STR_ECRITURE) :: don_ecr
!.    type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
!
!$Arguments
!>E     don_ecr        :<PS_STR_ECRITURE>        Structure contenant les variables
!                                                de sorties sélectionnées
!>E/S   maj_vect_etat  :<PS_STR_MAJ_VECT_ETAT>   Structure contenant les flags de
!                                                mise à jour du vecteur d'état
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none

      type (PS_STR_ECRITURE), intent(in) :: don_ecr
      type (PS_STR_MAJ_VECT_ETAT), intent(inout) :: maj_vect_etat

      ! Definition des différents lots de variables
      integer, dimension(12), parameter :: vars_kep = &
           (/ 21, 22, 23, 24, 25, 26, 43, 44, 67, 68, 77, 93/)
      integer, dimension(6), parameter :: vars_kep_moy = &
           (/ 61, 62, 63, 64, 65, 66 /)

      ! Correspondance des indices aux flags
      integer, parameter :: nb_vars_kep     = 12
      integer, parameter :: nb_vars_kep_moy = 6

      ! Indice de parcours des lots predifinis
      integer :: ii
      ! Indice de paracours des variables selectionnées
      integer :: jj

      ! Idem pour le lot correspondant au "calcul_kep_moy"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_kep_moy & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_kep_moy & 
              .and. ii <= nb_vars_kep_moy ) 
            if ( don_ecr%iresw(jj) == vars_kep_moy(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_kep        = .true.
               maj_vect_etat%calcul_kep_moy    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour le lot correspondant au "calcul_kep"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_kep & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_kep & 
              .and. ii <= nb_vars_kep ) 
            if ( don_ecr%iresw(jj) == vars_kep(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_kep        = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

   end subroutine psi_analyse_maj_kep


   subroutine psi_analyse_maj_posvit_geo(don_ecr, maj_vect_etat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_analyse_maj_posvit_geo
!
!$Resume
!  Analyse des variables liés aux positions vitesses géodésiques
!
!$Description
!  Analyse des variables liés aux positions vitesses géodésiques
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_analyse_maj_posvit_geo(don_ecr, maj_vect_etat)
!.    type (PS_STR_ECRITURE) :: don_ecr
!.    type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
!
!$Arguments
!>E     don_ecr        :<PS_STR_ECRITURE>        Structure contenant les variables
!                                                de sorties sélectionnées
!>E/S   maj_vect_etat  :<PS_STR_MAJ_VECT_ETAT>   Structure contenant les flags de
!                                                mise à jour du vecteur d'état
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none

      type (PS_STR_ECRITURE), intent(in) :: don_ecr
      type (PS_STR_MAJ_VECT_ETAT), intent(inout) :: maj_vect_etat

      ! Definition des différents lots de variables
      integer, dimension(8), parameter :: vars_pos_geo = &
           (/ 6, 7, 8, 27, 28, 29, 69, 76 /)
      integer, dimension(6), parameter :: vars_vit_geo = &
           (/ 12, 13, 14, 18, 19, 20 /)

      ! Correspondance des indices aux flags
      integer, parameter :: nb_vars_pos_geo = 8
      integer, parameter :: nb_vars_vit_geo = 6

      ! Indice de parcours des lots predifinis
      integer :: ii
      ! Indice de paracours des variables selectionnées
      integer :: jj

      ! Idem pour le lot correspondant au "calcul_vit_geo"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_vit_geo & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_vit_geo & 
              .and. ii <= nb_vars_vit_geo ) 
            if ( don_ecr%iresw(jj) == vars_vit_geo(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_pos_geo    = .true.
               maj_vect_etat%calcul_vit_geo    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour le lot correspondant au "calcul_pos_geo"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_pos_geo & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_pos_geo & 
              .and. ii <= nb_vars_pos_geo ) 
            if ( don_ecr%iresw(jj) == vars_pos_geo(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_pos_geo    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

   end subroutine psi_analyse_maj_posvit_geo

   subroutine psi_analyse_maj_forces(don_ecr, maj_vect_etat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_analyse_maj_forces
!
!$Resume
!  Analyse des variables liés aux forces dont la force de propulsion
!
!$Description
!  Analyse des variables liés aux forces dont la force de propulsion
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_analyse_maj_forces(don_ecr, maj_vect_etat)
!.    type (PS_STR_ECRITURE) :: don_ecr
!.    type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
!
!$Arguments
!>E     don_ecr        :<PS_STR_ECRITURE>        Structure contenant les variables
!                                                de sorties sélectionnées
!>E/S   maj_vect_etat  :<PS_STR_MAJ_VECT_ETAT>   Structure contenant les flags de
!                                                mise à jour du vecteur d'états
!
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none

      type (PS_STR_ECRITURE), intent(in) :: don_ecr
      type (PS_STR_MAJ_VECT_ETAT), intent(inout) :: maj_vect_etat

      ! Definition des différents lots de variables
      integer, dimension(35), parameter :: vars_forces = &
           (/ 30, 31, 32, 34, 35, 36, 37, 38, 39, 40,  &
              60, 78, 79, 80, 81, 82, 83, 84, 85, 86, &
              87, 88, 89, 90, 91, 92, 94, 95, 96, 107, &
              108, 109, 110, 111, 112 /)
      integer, dimension(8), parameter :: vars_force_prop = &
           (/ 33, 42, 45, 46, 47, 48, 58, 59 /) 
      ! Correspondance des indices aux flags
      integer, parameter :: nb_vars_forces  = 35
      integer, parameter :: nb_vars_force_prop = 8

      ! Indice de parcours des lots predifinis
      integer :: ii
      ! Indice de paracours des variables selectionnées
      integer :: jj

      ! Idem pour le lot correspondant au "calcul_force_prop"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_force_prop & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_force_prop & 
              .and. ii <= nb_vars_force_prop ) 
            if ( don_ecr%iresw(jj) == vars_force_prop(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_att_ris    = .true.
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_att_rts    = .true.
               maj_vect_etat%calcul_forces     = .true.
               maj_vect_etat%calcul_force_prop = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour le lot correspondant au "calcul_forces"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_forces & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_forces & 
              .and. ii <= nb_vars_forces ) 
            if ( don_ecr%iresw(jj) == vars_forces(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_att_ris    = .true.
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_att_rts    = .true.
               maj_vect_etat%calcul_forces     = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

   end subroutine psi_analyse_maj_forces


   subroutine psi_analyse_maj_ris_rts(don_ecr, maj_vect_etat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_analyse_maj_ris_rts
!
!$Resume
!  Analyse des variables liés aux positions vitesses et attitudes dans
!  le RIS et le RTS
!
!$Description
!  Analyse des variables liés aux positions vitesses et attitudes dans
!  le repère d'intégration de sortie et repère tournant de sortie.
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_analyse_maj_ris_rts(don_ecr, maj_vect_etat)
!.    type (PS_STR_ECRITURE) :: don_ecr
!.    type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
!
!$Arguments
!>E     don_ecr        :<PS_STR_ECRITURE>        Structure contenant les variables
!                                                de sorties sélectionnées
!>E/S   maj_vect_etat  :<PS_STR_MAJ_VECT_ETAT>   Structure contenant les flags de
!                                                mise à jour du vecteur d'état
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites

      implicit none

      type (PS_STR_ECRITURE), intent(in) :: don_ecr
      type (PS_STR_MAJ_VECT_ETAT), intent(inout) :: maj_vect_etat

      ! Correspondance des indices aux flags
      integer, parameter :: nb_vars_att_ris = 10
      integer, parameter :: nb_vars_car_rts = 9
      integer, parameter :: nb_vars_att_rts = 3 
      ! Definition des différents lots de variables
      integer, dimension(10), parameter :: vars_att_ris = &
           (/ 70, 71, 72, 73, 74, 75, 102, 103, 104, 105 /)
      integer, dimension(9), parameter :: vars_car_rts = &
           (/ 49, 50, 51, 52, 53, 54, 55, 56, 57 /)
      integer, dimension(3), parameter :: vars_att_rts = &
           (/ 97, 98, 99/)

      ! Indice de parcours des lots predifinis
      integer :: ii
      ! Indice de paracours des variables selectionnées
      integer :: jj

      ! Idem pour le lot correspondant au "calcul_att_rts"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_att_rts & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_att_rts & 
              .and. ii <= nb_vars_att_rts ) 
            if ( don_ecr%iresw(jj) == vars_att_rts(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_att_ris    = .true.
               maj_vect_etat%calcul_car_rts    = .true.
               maj_vect_etat%calcul_att_rts    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour le lot correspondant au "calcul_car_rts"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_car_rts & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_car_rts & 
              .and. ii <= nb_vars_car_rts ) 
            if ( don_ecr%iresw(jj) == vars_car_rts(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_car_rts    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

      ! Idem pour le lot correspondant au "calcul_att_ris"
      jj = 1
      do while ( .not. maj_vect_etat%calcul_att_ris & 
           .and. jj <= don_ecr%nvarw) 
         ii=1
         ! Parcours des variables du lot
         do while ( .not. maj_vect_etat%calcul_att_ris & 
              .and. ii <= nb_vars_att_ris ) 
            if ( don_ecr%iresw(jj) == vars_att_ris(ii)) then
               ! Mise a jour des dependances
               maj_vect_etat%calcul_att_ris    = .true.
            endif
            ii=ii+1
         enddo
         jj=jj+1
      enddo

   end subroutine psi_analyse_maj_ris_rts

end module ps_interface_psimu_don
