module ps_atmosphere

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_atmosphere
!
!$Resume
! Module calculant les forces de frottement atmosphériques.
!
!$Description
! Module calculant les forces de frottement atmosphériques.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_atmosphere.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_atmosphere.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.92  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.91  2009/08/19 09:16:47  tanguyy
!  AQ : corrections de commentaires
!
!  Revision 1.90  2009/04/14 16:05:14  tanguyy
!  AQ : variables inutilisees, et FA-ID 1260 : mecanisme pour sauver la valeur d incidence et la reutiliser quand ma_avion_vit sort en erreur et ne peut la calculer
!
!  Revision 1.89  2008/12/04 13:48:53  tanguyy
!  DM-ID 733 /AQ : remise à jour des cartouches et simplification + commentaires de ps_calcul_incidence
!
!  Revision 1.88  2008/12/02 10:49:36  huec
!  DM-ID 1058 : Ajout des sorties xcd et xcl a psfratm, suppression de variables inutilisees, initialisation de la varable sta
!  Revision 1.87  2008/12/02 08:16:58  tanguyy
!  DM-ID 733 : mise à jour des cartouches, correction du code suite à relecture, utilisation de la latitude géodésique, et non géocentrique, en entrée des modèles d'atmosphère
!  Revision 1.86  2008/11/26 09:05:04  tanguyy
!  DM-ID 733 : version initiale de la nouvelle routine psfratm, qui appelle MSP_calculer_frottements en réalisant tous les pré-traitements nécessaires
!  Revision 1.85  2008/10/17 15:48:54  tanguyy
!  DM-ID 1091 : debut de l'integration de l'emcd 4.3
!  Revision 1.84  2008/10/17 10:00:59  mercadig
!  DM-ID 1058 Initialisations de variables
!  Revision 1.83  2008/09/22 09:32:45  mercadig
!  FA-ID 1016 Suppression du parametre isol des routines psfrvenus et psfrmar
!  Revision 1.82  2008/09/04 07:52:54  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.81  2008/05/05 15:55:56  tanguyy
!  AQ : correction des commentaires
!  Revision 1.80  2008/04/23 17:37:10  tanguyy
!  FA-ID 1009 : utilisation des noms COMPAS pour les modeles d'atmosphere, et de constantes entieres specifiques a PSIMU (cf DM-ID 859)
!  Revision 1.79  2008/04/16 15:53:10  huec
!  DM-ID 859 : Utilisation de boucles explicites
!  Revision 1.78  2008/04/03 16:08:25  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.76  2008/03/27 16:18:19  ttn
!  FA-ID 973 : Perte de performances avec la prise en compte du modele de vent
!  Revision 1.75  2008/03/07 10:00:49  huec
!  DM-ID 859 : Utilisation de mu_matmul3, mu_transpose3, tests des code_erreur
!  Revision 1.74  2008/02/15 16:37:38  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.73  2008/01/22 08:57:44  huec
!  FA-ID 846 : Appel au modele d atmosphere CIRA_MSIS COMPAS prefere a MSPRO
!  Revision 1.72  2008/01/17 16:21:53  tanguyy
!  DM-ID 551 / FA-ID 905 / FA-ID 906 : correction des appels aux modèles martiens EMCD 4.2 et ATMMARS 90.
!  Revision 1.71  2007/12/06 16:00:33  huec
!  FA-ID 658 : Variables declarees en double
!  Revision 1.70  2007/12/06 15:28:29  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.69  2007/12/06 13:39:58  tanguyy
!  Appel au modele EMCD 4.2 avec une distance depuis au centre de la planete
!  Revision 1.68  2007/12/04 08:16:16  tanguyy
!  Integration DM-ID 551 / DM-ID 808
!  Revision 1.67  2007/12/03 13:22:31  tanguyy
!  DM-ID 551 : suppression de msp_roat77 au profit de cps_roat77
!  Revision 1.66  2007/11/29 14:42:41  jpi
!  DM-ID 733 : annulation de la DM donc des modifs
!  Revision 1.65  2007/10/30 08:46:48  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.64  2007/09/24 15:06:15  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.63  2007/07/13 15:27:57  tanguyy
!  AQ : PSIMU V9.0 - maj de commentaires
!  Revision 1.62  2007/07/10 14:07:01  tanguyy
!  DM-ID 724 : désactivation du modèle US76 au dessus de 1000km, et utilisation du modèle de la MSPRO car le comportement est différent (cas en mode prog n°11, 62, 63) avec la routine cps_atm_us76d
!  Revision 1.61  2007/07/09 11:57:50  tanguyy
!  DM-ID 724 et DM-ID 688 : mise à jour de l'appel au modèle US76 + mode Terre/Mars basé sur le code NAIF
!  Revision 1.60  2007/06/20 12:21:34  vivaresf
!  FA-ID 746 : Fuites mémoires, scénarios de sortie en inout pour permettre leur désallocation
!  Revision 1.59  2007/02/09 13:39:00  tanguyy
!  Utilisation du modèle de vent : modif du test pour gagner en performances
!  Revision 1.58.2.5  2007/04/18 06:33:21  vivaresf
!  FA-ID 725 : validation
!  Revision 1.58.2.4  2007/04/17 08:23:29  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.58.2.3  2007/04/16 09:42:46  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.58.2.2  2007/03/12 14:55:26  tanguyy
!  Modification du test de la presence du modele de vent pour gagner en performances
!  Revision 1.58.2.1  2007/03/09 16:48:14  couturis
!  FA-ID 713 : seuil de masse fixé à epsilon*surface dans les calculs de trainee/portance
!  Revision 1.58  2007/02/02 16:23:07  vivaresf
!  DM-ID 643 : commentaires à jour
!  Revision 1.57  2007/02/02 15:15:40  mle
!  DM-ID 643 modeles de vent dans PSIMU
!  Revision 1.56  2007/02/02 13:30:59  tanguyy
!  DM-ID 659 / DM-ID 643 : integration
!  Revision 1.55  2007/02/02 08:21:55  mle
!  DM-ID 643 : modeles de vent dans PSIMU, expression des vitesses dans les bons reperes
!  Revision 1.54  2007/01/29 16:30:43  mle
!  DM-ID 643 : modeles de vents dans PSIMU
!  Revision 1.53  2007/01/18 15:14:23  tanguyy
!  FA-ID 687 : separation du calcul des coefficients de frottement
!  Revision 1.52  2006/11/29 11:00:14  tanguyy
!  FA-ID 627 : correction de la remontee d'erreur pour le modele CIRA
!  Revision 1.51  2006/10/20 10:33:52  tanguyy
!  PSIMU V8-6a1 : version d'integration
!  Revision 1.50  2006/10/19 15:08:41  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.49.2.3  2006/10/19 15:06:45  tanguyy
!  AQ : simplification d'une multiplication de matrices
!  Revision 1.49.2.2  2006/10/13 07:55:19  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.49.2.1  2006/10/10 07:42:22  tanguyy
!  DM-ID 478 : Version initiale du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.49  2006/10/02 14:01:47  aitiere
!  AQ : maj des declarations
!  Revision 1.48  2006/06/30 15:53:04  tanguyy
!  Integration PSIMU 8-5
!  Revision 1.47  2006/03/15 13:25:16  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.46  2006/02/27 15:46:17  tanguyy
!  FA-ID 430 : recadrage de l incidence entre -pi et pi
!  Revision 1.45  2005/12/14 19:08:04  tanguyy
!  PSIMU V8-3
!  Revision 1.44  2005/12/14 13:22:36  vivaresf
!  DM-ID 397 : accepter les altitudes negatives si pas trop
!  Revision 1.43  2005/11/16 11:23:56  vivaresf
!  DM-ID 5 : Cloture du FT (Calcul de l heure locale dans le module atmosphere)
!  Revision 1.42.2.1  2005/11/16 11:23:04  vivaresf
!  DM-ID 5 : ephemerides analytique pour le calcul de l'heure locale
!  Revision 1.42  2005/11/10 18:37:03  vivaresf
!  Mise à jour des cartouches
!  Revision 1.41  2005/11/09 13:23:11  vivaresf
!  DM-ID 6 : remplacement de MSP_posvit_3corps par ps_propage
!  Revision 1.40  2005/03/17 08:27:27  vivaresf
!  MSLIB90 V6.2
!  Revision 1.39  2005/03/11 16:04:13  vivaresf
!  DM-ID 245 : warning lorsque la vitesse du vehicule est perpendiculaire a son axe X theorique
!  Revision 1.38  2005/03/11 15:58:59  fabrec
!  V8-2 : utilisation de COMPAS
!  Revision 1.37  2005/01/28 10:37:24  fabrec
!  maj cartouches
!  Revision 1.36  2005/01/17 15:22:12  fabrec
!  DM-ID 175 : modatt
!  Revision 1.35  2004/11/22 08:58:48  vivaresf
!  DM_ID 200 : utilisation des requa et apla du potentiel dans le courant des calculs
!  Revision 1.34  2004/07/08 16:39:17  adm_ipsi
!  DM-ID 89 : Integration - Maj argument pole
!  Revision 1.33  2004/06/18 09:45:42  vivaresf
!  Rajout des noms de routines pour aider au debug
!  Revision 1.32  2004/03/31 10:41:21  adm_ipsi
!   Suppression de code en commentaire
!  Revision 1.31  2004/01/15 16:33:23  adm_ipsi
!  DM-ID 10, les calculs internes de PSIMU se font en date TE
!  Revision 1.30  2003/11/25 17:50:18  adm_ipsi
!  DM-ID 80, filtrage des warnings sur mx_var et mu_inter_dim1_lin
!  Revision 1.29  2003/10/01 16:17:47  adm_ipsi
!  DM-ID 52, Utilisation de mx_var au lieu de MSP_conv_typbul
!  Revision 1.28  2003/07/10 10:09:38  adm_ipsi
!  FA-ID 13 : Controle que la masse >0
!  Revision 1.16  2003/03/28 09:56:45  util_am
!  SLB - Version industrialisée
!  Revision 1.27  2003/03/20 16:56:08  laurent
!  remplacement de  IO_a_angatt par des routine de la mspro
!  Revision 1.26  2003/03/18 17:17:52  adm_ipsi
!  Utilisation de MSP_roat77 en attendant mp_atm_roat77
!  Revision 1.25  2003/03/14 15:41:57  adm_ipsi
!  Utilisation de ier_mslib au lieu de ier_mspro suite à la modification de MSP_signaler_message
!  Revision 1.24  2003/02/19 11:06:19  boschett
!  A Deramecourt : modif ier_mslib en ier_mspro pour les appels MSPRO
!  Revision 1.23  2003/02/19 10:42:20  rodier
!  PhB - Modification indices pente, azimut + fic_tuc dans MSP_posvit_3corps
!  Revision 1.22  2003/02/18 09:01:29  rodier
!  PhB - Correction appel à MSP_conv_typrep et MSP_conv_typbul
!  Revision 1.21  2003/02/14 16:36:05  rodier
!  PhB - Suppression IO_a_atiter
!  Revision 1.20  2003/02/14 16:01:30  rodier
!  PhB - MSP_conv_typbul - MSP_conv_typrep
!  Revision 1.19  2003/02/14 15:04:50  boschett
!  A Deramecourt : portage de IO_e_atmars90/IO_e_atmemcd (iolib) a atmars90/atmemcd_31 (libMars)
!  Revision 1.18  2003/02/13 15:01:29  boschett
!  A Deramecourt : portage de IO_a_atiter (IOLIB) en methodes de la mslib/mspro et methodes fortran
!  Revision 1.17  2003/02/12 17:09:47  boschett
!  A Deramecourt : retour en arriere suite coredump sur mu_mat_quat
!  Revision 1.16  2003/02/12 12:50:55  boschett
!  A Deramecourt : remplacement de l'appel de IO_m_rotvec3 (iolib) par mu_3rot_quat + mu_quat_mat + matmul
!  Revision 1.15  2003/02/12 10:25:06  adm_ipsi
!  Remplacement de IO_p_cvbul par MSP_conv_typbul
!  Revision 1.14  2003/02/11 11:38:37  boschett
!  A Deramecourt : portage de l'appel de IO_a_angatt (iolib) en appel de mu_mat_quat et de mu_quat_3rot (mspro)
!  Revision 1.13  2003/02/05 18:48:23  boschett
!  A Deramecourt : modification appel mujjul (mslib77) en md_calend_julien (mslib90)
!  Revision 1.12  2003/02/05 18:20:37  boschett
!  A Deramecourt : portage de IO_m_dlin2 (iolib) en mu_inter_dim2_deg2 de la MSRPO
!  Revision 1.11  2003/02/04 14:51:55  boschett
!  A Deramecourt : suppression trace d'execution
!  Revision 1.10  2003/02/04 11:18:26  boschett
!  A Deramecourt : modifications des appels de fonctions de la IOLIB en MSPRO pour les modeles d'atmosphere
!  Revision 1.9  2003/01/31 17:42:03  boschett
!  A Deramecourt : modif appel de IO_a_anglab en mspro ma_avion_vit
!  Revision 1.8  2003/01/31 12:30:57  boschett
!  A Deramecourt : suppression appels mumat3, muvec3, muvectr3 de la amlib
!  Revision 1.7  2002/12/20 16:37:43  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.6  2002/12/06 09:48:53  boschett
!  Suppression des assignations inutiles
!  Revision 1.5  2002/12/04 14:22:32  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/11/27 17:30:53  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.3  2002/11/27 15:54:42  boschett
!  Utilisation de la variable globale P_ECART_JULIEN dans la subroutine psfrmar
!  Revision 1.2  2002/11/26 15:45:13  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.15  2002/06/24 13:29:28  util_am
!  Meilleure optimisation de la recherche 3ème corps engardant la mémoire de l'indice
!  Revision 1.14  2001/11/09 08:58:14  util_am
!  Appel de la routine MSP_posvit_3corps avec l'argument echt pour donner une date en TE
!  Revision 1.13  2001/10/29 12:46:55  util_am
!  Ajout d'un modèle d'atmosphère exponentiel
!  Revision 1.12  2001/09/12 11:15:47  util_am
!  Bug dans l'appel à mujjul (considéré comme retournant un réel)
!  Revision 1.11  2000/09/18 14:08:31  util_am
!  Passage des Ap aux Kp pour le modèle DTM
!  Revision 1.10  2000/09/01 14:50:35  util_am
!  Modification de l'appel du modèle EMCD + ajout du champ diratm dans le type PS_STR_INT_ATMOSPHERE
!  Revision 1.9  2000/06/27 11:42:39  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.8  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.7  2000/02/08 09:39:36  util_am
!  Modification du calcul des coordonnées dans Greenwich en utilisant mrtsid et non wt*dt
!  Revision 1.6  2000/02/03 16:57:45  util_am
!  renseignement d'une cle de message manquante
!  Revision 1.5  1999/10/27 11:23:37  util_am
!  Prise en compte des panneaux solaires dans le calcul des forces
!  Revision 1.4  1999/10/26 11:00:11  util_am
!  Mise à jour des cartouches
!  Revision 1.3  1999/08/31 11:56:09  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:09  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_atmosphere
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psfratm
!- ps_calcul_vent
!- ps_calcul_incidence
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_integration_don
!- mspro
!- ps_bulletin
!- ps_attitude
!- ps_caracteristiques
!- ps_troiscorps
!- cps_utilisateur
!- ps_calcul_forces
!- mslib
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_generalites
   use ps_integration_don

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_atmosphere.F90 368 2013-02-19 14:43:59Z aadt $'


   contains


     subroutine psfratm (date,pos,vit,mat_Rapp_Rveh,pos_soleil_veis,irepa,accfro,ro,mach,vitatm,vit_vent_gw,xcd,xcl)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psfratm
!
!$Resume
!   Routine de calcul de l'accélération de frottement atmosphérique.
!
!$Description
!   Routine de calcul de l'accélération de frottement atmosphérique tenant compte des formes variées de véhicule
!   de l'attitude du véhicule et des différents modèles d'atmosphère terrestre, martiens ou vénusiens.
!
!$Auteur
!   Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psfratm (date,pos,vit,mat_Rapp_Rveh,pos_soleil_veis,irepa,accfro,ro,mach,vitatm,vit_vent_gw,xcd,xcl)
!.    type(tm_jour_sec) :: date
!.    real (kind=pm_reel), dimension(3) :: pos,vit
!.    real (kind=pm_reel), dimension(3,3) :: mat_Rapp_Rveh
!.    real(kind=pm_reel), dimension(3) :: pos_soleil_veis
!.    integer :: irepa
!.    real (kind=pm_reel) :: ro,mach
!.    real (kind=pm_reel), dimension(3) :: accfro
!.    real(KIND=pm_reel), dimension(3) :: vitatm
!.    real(KIND=pm_reel), dimension(3) :: vit_vent_gw
!.    real(kind=pm_reel) :: xcd
!.    real(kind=pm_reel) :: xcl
!
!$Arguments
!>E     date             :<tm_jour_sec>         Date jj 1950 TE en jours/secondes
!>E     pos              :<pm_reel,DIM=(3)>     Position dans le repère planétocentrique inertiel (Rapp) à date courante (m)
!>E     vit              :<pm_reel,DIM=(3)>     Vitesse absolue dans Rapp à date courante (m/s)
!>E     mat_Rapp_Rveh    :<pm_reel,DIM=(3,3)>   Matrice de passage du repère Rapp au repère véhicule
!>E/S   pos_soleil_veis  :<pm_reel,DIM=(3)>     Position du soleil dans le Veis (mode Terre uniquement) (m)
!>E     irepa            :<integer>             Indicateur du type d'attitude ; permet de distinguer
!                                               le cas Yaw Steering (pointé soleil / MSP_ENUM_ATTI_YAW_STEERING) des autres
!>S     accfro           :<pm_reel,DIM=(3)>     Accélération lié au frottement atmosphérique, dans Rapp (m/s²)
!>S     ro               :<pm_reel>             Densité atmosphérique (kg/m3)
!>S     mach             :<pm_reel>             Nombre de mach
!>S     vitatm           :<pm_reel,DIM=(3)>     Vitesse par rapport à l'atmosphere : vitesse relative du véhicule dans Rapp en m/s
!>S     vit_vent_gw      :<pm_reel,DIM=(3)>     Vitesse du vent dans le repère de Greenwich (= vitesse relative dans Rapp) en ms/s
!>S     xcd              :<pm_reel>             Coefficient aérodynamique CD pour le calcul du coefficient de trainée
!>S     xcl              :<pm_reel>             Coefficient aérodynamique CL pour le calcul du coefficient de portance
!
!$Common
!
!$Routines
!- ps_calcul_tsid_RI_Rapp
!- mt_car_geod
!- MSP_signaler_message
!- psi_rep_veis_ri
!- ps_rotation_tsid
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_modifier_mci
!- MSP_modifier_vehicule
!- ps_calcul_vent
!- ps_mat_rotation_tsid
!- mu_matmul3
!- ps_calcul_incidence
!- mu_angle2
!- MSP_calculer_frottement
!
!$Include
!
!$Module
!#V
!- mspro
!- ps_bulletin
!- ps_attitude
!- ps_caracteristiques
!- ps_integration_don
!- ps_troiscorps
!- cps_utilisateur
!- ps_calcul_forces
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!  MSP_calculer_frottement
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       use mspro
       use ps_bulletin
       use ps_attitude
       use ps_caracteristiques
       use ps_integration_don
       use ps_troiscorps
       use cps_utilisateur
       use ps_calcul_forces


       implicit none

       ! Arguments
       !==========
       type(tm_jour_sec),   intent(in) :: date
       ! Positions vitesses dans le repère d'application du frottement ( planéto à date courante)
       real (kind=pm_reel), dimension(3), intent(in) :: pos,vit
       ! Matrice de passage du repère d'application de la force au repère véhicule
       real (kind=pm_reel), dimension(3,3), intent(in) :: mat_Rapp_Rveh
       real(kind=pm_reel), dimension(3) :: pos_soleil_veis

       integer, intent(in) :: irepa
       real (kind=pm_reel), intent(out) :: ro,mach
       real (kind=pm_reel), dimension(3), intent(out)  ::  accfro
       real(KIND=pm_reel), dimension(3), intent(out)   :: vitatm
       real(KIND=pm_reel), dimension(3), intent(out) :: vit_vent_gw
       real(kind=pm_reel), intent(out) :: xcd
       real(kind=pm_reel), intent(out) :: xcl


       ! Variables locales
       !==================
       real(kind=pm_reel), dimension(3,3) :: mat_Rapp_Panneaux,mat_RI_Rapp,pgamav
       real(kind=pm_reel) :: norme_pos_soleil, us_Rapp, vs_Rapp, ws_Rapp
       real(kind=pm_reel) :: incidence,apla,tsid,tsid_date_courante
       real(kind=pm_reel), dimension(3) :: pos_soleil_Rapp, pos_soleil_RI
       real(kind=pm_reel) :: heure_solaire, adroit_veis
       real(kind=pm_reel), dimension(3) :: pos_RI, vit_RI, vit_rel_Rapp
       real(kind=pm_reel) :: surface_transverse
       integer :: forme_vehicule, panneaux
       

       integer :: ii,jj
       type(tm_geodesique) :: pos_geod
       type(tm_code_retour) :: code_erreur
       type(MSP_MCI) :: mci

       ! Début du code
       !==============

       ! Calculs préliminaires à l'appel de la routine de calcul des frottements
       ! - calcul des coordonnées géodésiques dans un repère planétographique
       ! - calcul de la matrice d'attitude des panneaux (si applicable)
       ! - calcul de la vitesse du vent (si applicable)
       ! - calcul de l'incidence, si on utilise des coefs aéro dépendants de l'incidence
       ! et du mach
       ! - calcul de l'heure solaire
       ! - calcul de la vitesse relative du véhicule
       !========================================================================


       ! calcul coordonnées géodésiques dans le repère planétographique
       !===============================================================

       ! calcul du temps sidéral entre le repère d'intégration
       ! et le repère d'application des forces (repère planéto à date courante)
       call ps_calcul_tsid_RI_Rapp(date,tsid)

       ! Vérification sur la valeur de l'aplatissement
       if (abs(str_mod(iveh)%apla) > MSP_EPSILON_APLA) then
          apla = 1/str_mod(iveh)%apla
       else
          apla = 0._pm_reel
       end if
       
       call mt_car_geod(pos,str_mod(iveh)%requa,apla,pos_geod,code_erreur) 
       if (code_erreur%valeur < 0) then
          call MSP_signaler_message(ier_mslib=code_erreur)
          return
       end if
    
       ! calcul de la matrice d'attitude des panneaux (mat_Rapp_Panneaux + 'flag' panneaux)
       !===================================================================================
       if (str_car(iveh)%typcf /= PS_INCIDENCE_MACH) then
          ! Pour les modes autres que incidence/mach, on tient compte des panneaux

          if (irepa == MSP_ENUM_ATTI_YAW_STEERING ) then
             ! Si l'attitude est en Yaw Steering, on calcule une matrice d'attitude particulière, 
             ! sinon, la matrice d'attitude des panneaux est celle du véhicule

             call psi_rep_veis_ri(date,pos_soleil_veis,pos_soleil_ri)
             call ps_rotation_tsid(pos_soleil_RI,tsid,pos_soleil_Rapp)

             ! a) normalisation du vecteur position du soleil
             norme_pos_soleil = sqrt ( pos_soleil_Rapp(1)**2 + pos_soleil_Rapp(2)**2 + pos_soleil_Rapp(3)**2  )
             us_Rapp = pos_soleil_Rapp(1)/norme_pos_soleil
             vs_Rapp = pos_soleil_Rapp(2)/norme_pos_soleil
             ws_Rapp = pos_soleil_Rapp(3)/norme_pos_soleil

             ! b) remplissage de la matrice
             mat_Rapp_Panneaux(1,1) = us_Rapp
             mat_Rapp_Panneaux(1,2) = vs_Rapp
             mat_Rapp_Panneaux(1,3) = ws_Rapp
             mat_Rapp_Panneaux(2,1) = us_Rapp
             mat_Rapp_Panneaux(2,2) = vs_Rapp
             mat_Rapp_Panneaux(2,3) = ws_Rapp
             mat_Rapp_Panneaux(3,1) = us_Rapp
             mat_Rapp_Panneaux(3,2) = vs_Rapp
             mat_Rapp_Panneaux(3,3) = ws_Rapp
          else
             ! b) remplissage de la matrice
             do ii=1,3
                do jj=1,3
                   mat_Rapp_Panneaux(ii,jj) = mat_Rapp_Rveh(ii,jj)
                end do
             end do
          end if
          panneaux = 1
       else
          ! En incidence/mach, on ne tient pas compte des panneaux
          ! et on utilise un véhicule particulier
          ! -> sphère avec une surface sref
          !================================
          do ii=1,3
             do jj=1,3
                mat_Rapp_Panneaux(ii,jj) = 0._pm_reel
             end do
          end do
          ! on ne prend pas en compte les panneaux
          panneaux = 0

          call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)

          call MSP_consulter_mci(mci,forme=forme_vehicule,st=surface_transverse)

          call MSP_modifier_mci(mci,forme=MSP_ENUM_SPHERE,st=str_car(iveh)%sref)

          call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)

       end if

       ! Appel au calcul de vent
       ! -> la vitesse du vent sera exprimée 
       ! dans le repère d'application de la force
       !========================================= 
       if (str_mod(iveh)%calcul_vent == 1) then
          call ps_calcul_vent(pos_geod, vit_vent_gw)
          if (MSP_gen_messages("psfratm")) return
       else
          do ii = 1,3
             vit_vent_gw(ii) = 0._pm_reel
          end do
       end if
       
       ! Calcul de l'incidence
       !======================
       if (str_car(iveh)%typcf == PS_INCIDENCE_MACH) then

          ! Pour calculer l'incidence, on recupère les pos/vit dans le repère à date initiale (RI)
          ! et la matrice pgamav de passage RI -> Rveh
          
          
          call ps_rotation_tsid(pos,-tsid,pos_RI)
          call ps_rotation_tsid(vit,-tsid,vit_RI)

          call ps_mat_rotation_tsid(tsid,mat_RI_Rapp)
          
          
          call mu_matmul3(mat_Rapp_Rveh,mat_RI_Rapp,pgamav,code_erreur)

          call ps_calcul_incidence(date,pos_RI,vit_RI,tsid,pos_geod%lat,pos_geod%long,pgamav,incidence) 

          if(MSP_gen_messages("psfratm")) return

       else
          incidence = 0._pm_reel
       end if

       ! Calcul de l'heure solaire
       !==========================
       if ( str_gen(iveh)%planet == eph_terre ) then
          ! Calcul ascension droite du soleil
          ! -> position du soleil dans le repère de Veis
          call mu_angle2 (pos_soleil_veis(1),pos_soleil_veis(2),adroit_veis,code_erreur)
          if ( MSP_gen_messages ("psfratm") ) return
          
          call ps_calcul_tsid(date,tsid_date_courante)

          heure_solaire = pos_geod%long - adroit_veis + pi + tsid_date_courante

       else
          heure_solaire = 0._pm_reel
       end if




       ! Calcul de la vitesse relative
       ! à partir des vitesses absolues, des positions X et Y
       ! dans le repère inertiel, et de la vitesse de rotation
       ! de la planète.
       !======================================================
       vit_rel_rapp(1) = vit(1) + str_mod(iveh)%vitrotpla * pos(2)
       vit_rel_rapp(2) = vit(2) - str_mod(iveh)%vitrotpla * pos(1)
       vit_rel_rapp(3) = vit(3)
     
 
       ! Appel à la routine de calcul 
       ! de frottement atmosphérique 
       !==================================
       if (panneaux == 1) then
          ! Utilisation de panneaux solaires
          call MSP_calculer_frottement(str_mod(iveh)%atm,str_car(iveh)%vehicule, &
               date,pos_geod,&
               heure_solaire,vit_rel_rapp,mat_Rapp_Rveh,incidence,&
               accfro,ro,mach,xcd,xcl,vit_vent=vit_vent_gw,modatt=modatt(iveh), &
               panneaux=panneaux,&
               mat_att_panneaux=mat_Rapp_Panneaux,vit_atm=vitatm)
          if (MSP_gen_messages("psfratm")) return
       else
          ! Sans panneaux solaires.
          call MSP_calculer_frottement(str_mod(iveh)%atm,str_car(iveh)%vehicule,date,pos_geod,&
               heure_solaire,vit_rel_rapp,mat_Rapp_Rveh,incidence,&
               accfro,ro,mach,xcd,xcl,vit_vent=vit_vent_gw,modatt=modatt(iveh),&
               vit_atm=vitatm)
          if (MSP_gen_messages("psfratm")) return
       end if

       if (str_car(iveh)%typcf == PS_INCIDENCE_MACH) then

          call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
          call MSP_modifier_mci(mci,forme=forme_vehicule,st=surface_transverse)
          call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)
          
       end if

     end subroutine psfratm


    subroutine ps_calcul_vent (don_geod, vit_vent_gw)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_vent
!
!$Resume
!  Calcul du vent à partir de la position géodésique du véhicule dans le repère planéto à date courante
!
!$Description
!  Calcul du vent à partir de la position géodésique du véhicule dans le repère planéto à date courante
!
!$Auteur
!  Marie Larroque (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_vent (don_geod, vit_vent_gw)
!.    type(tm_geodesique) :: don_geod
!.    real(kind = pm_reel), dimension(3) :: vit_vent_gw
!
!$Arguments
!>E/S   don_geod     :<tm_geodesique>     position du vehicule dans le repere 
!>S     vit_vent_gw  :<pm_reel,DIM=(3)>   vitesse du vent dans le Greenwich (vitesse relative dans le planéto à
!                                         date courante)
!
!$Common
!
!$Routines
!- MSP_calculer_modvent
!- mt_topo_N_ref
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- mslib
!- mspro
!- ps_generalites
!- ps_bulletin
!- ps_calcul_forces
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use mslib
      use mspro
      use ps_generalites
      use ps_bulletin
      use ps_calcul_forces

      implicit none

      ! arguments
      !----------
      type(tm_geodesique) :: don_geod
      real(kind = pm_reel), dimension(3), intent(out) :: vit_vent_gw
      
      !variables locales
      !-----------------
      real(kind=pm_reel) :: vent_EO, vent_SN, vent_vert
      real(kind=pm_reel), dimension(3) :: vit_topo, position_topo
      real(kind=pm_reel), dimension(3) :: position_gw
      type(tm_code_retour) :: code_erreur


      ! début du code
      !--------------
      
      ! Conversion cartésien - géodésique
      !==================================
      
      position_topo(1) = 0._pm_reel
      position_topo(2) = 0._pm_reel
      position_topo(3) = 0._pm_reel


      ! on calcule le vent à la position où est le vehicule
      !----------------------------------------------------
      call MSP_calculer_modvent(str_mod(iveh)%vent,don_geod%haut,&
           vent_EO,vent_SN, ventvert = vent_vert)
      if(MSP_gen_messages("ps_calcul_vent")) return

      ! position/vitesse du vent dans le repere topocentrique Nord
      !-----------------------------------------------------------
      vit_topo(1) = vent_SN
      vit_topo(2) = vent_EO
      vit_topo(3) = vent_vert
      
      ! on calcule la vitesse du vent dans le repere de Greenwich (planéto de référence)
      !----------------------------------------------------------
      call mt_topo_N_ref(don_geod,str_mod(iveh)%requa,1._pm_reel/str_mod(iveh)%apla,&
           position_topo,position_gw, code_erreur, vit_topo, vit_vent_gw )
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message(cle_mes="PS_ATM_VENT", &
              partie_variable="1")
         return
      end if

    end subroutine ps_calcul_vent


    subroutine ps_calcul_incidence(date,pos,vit,tsid,latgeo,longeo,pgamav,incidence) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_incidence
!
!$Resume
!  Calcul de l'incidence du véhicule dans un le repère aérodynamique
!
!$Description
!  Calcul de l'incidence du véhicule dans un le repère aérodynamique
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_incidence(date,pos,vit,tsid,latgeo,longeo,pgamav,incidence) 
!.    type(tm_jour_sec) :: date
!.    real (kind=pm_reel), dimension(3) :: pos,vit
!.    real(kind=pm_reel) :: tsid
!.    real(kind=pm_reel) :: latgeo, longeo
!.    real(kind=pm_reel), dimension(3,3) :: pgamav
!.    real(kind=pm_reel) :: incidence
!
!$Arguments
!>E     date       :<tm_jour_sec>         Date jj 1950 TE en jours/secondes
!>E     pos        :<pm_reel,DIM=(3)>     Position dans le repère planéto à date initiale (RI) (m)
!>E     vit        :<pm_reel,DIM=(3)>     Vitesse absolue dans le repère planéto à date initiale (RI) (ms/)
!>E     tsid       :<pm_reel>             Temps sidéral entre le repère planéto à date initiale et le repère à date courante
!>E     latgeo     :<pm_reel>             Latitude géodésique dans le repère à date courante (Rapp)
!>E     longeo     :<pm_reel>             Longitude géodésique dans le repère à date courante (Rapp)
!>E     pgamav     :<pm_reel,DIM=(3,3)>   Matrice de passage du repère RI au repère véhicule
!>S     incidence  :<pm_reel>             Incidence du véhicule dans le repère topocentrique local
!
!$Common
!
!$Routines
!- mt_topo_N_ref
!- ps_mat_rotation_tsid
!- mu_transpose3
!- mu_matmul3
!- mu_mat_quat
!- MSP_signaler_message
!- mu_quat_3rot
!- MSP_conv_typrep
!- mx_var
!- ma_avion_vit
!
!$Include
!
!$Module
!#V
!- ps_calcul_forces
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

      use ps_calcul_forces
      use ps_bulletin
      use ps_attitude 

        implicit none
        
        ! Arguments
        !==========
        type(tm_jour_sec), intent(in) :: date
        real (kind=pm_reel), dimension(3), intent(in) :: pos,vit
        real(kind=pm_reel), intent(in) :: tsid
        real(kind=pm_reel), intent(in) :: latgeo, longeo
        real(kind=pm_reel), dimension(3,3), intent(in) :: pgamav
        real(kind=pm_reel), intent(out) :: incidence

        ! Variables locales
        !==================
        integer :: k,j
        real(kind=pm_reel) :: apla_rep
        real(kind=pm_reel) :: psis,tets,phis
        real(kind=pm_reel) :: pente, azimut
        real(kind=pm_reel) :: derapage, gite_vit
        real (kind=pm_reel), dimension(9) :: sta

        type(tm_quat) :: quaternion
        type(tm_code_retour) :: code_erreur, code_retour_local
        type (tm_geodesique)     :: orig_topo
        real (KIND=pm_reel), dimension(3,3)  :: pgamavbis, pgamgr, pgater, ptergr
        real (KIND=pm_reel), dimension(3,3)  :: pos_ref, pterav, pterga, pgrter

        type(tm_pole_uv)         :: pole
        type(tm_ellipsoide)      :: ellips
        real (kind=pm_reel), dimension(6,6) :: rdxdp, jacob
        real (kind=pm_reel), dimension(6) :: pvr, parren
        
        ! tableau de valeurs d'incidence (voir explication lors de l'appel à ma_avion_vit)
        real(kind=pm_reel), dimension(PS_NVMAX), save :: tab_incidence=0._pm_reel
        real(kind=pm_reel) :: valeur_incidence
        
        ! Début du code
        !==============

        ! Initialisation de sta
        do k=1,9
           sta(k) = 0._pm_reel
        end do

        if (modatt(iveh) == 0) then
           ! Matrice non initialisée cas pas de loi d'attitude
           ! -> on initialise à la matrice identité
           do k=1,3
              do j=1,3
                 pgamavbis(k,j) = 0._pm_reel
              end do
              pgamavbis(k,k) = 1._pm_reel
           end do
        else
           ! Attitude définie
           ! -> on utilise la matrice d'attitude
           do k=1,3
              do j=1,3
                 pgamavbis(k,j) = pgamav(k,j)
              end do
           end do
        end if

        ! A propose des différents repères utilisés :
        ! RI : Repère Intégration (gamma) : planétocentrique inertiel à date initiale
        ! Rapp : Repère d'application de la force (Greenwich) : planétocentrique inertiel 
        !                                                       à date courante
        ! Topo : Terrestre Topocentrique Nord : repère local (station) pointé nord
        ! 
        ! ptergr : passage de Topo à Rapp 
        ! pgamgr : passage de RI à Rapp
        ! pgamav(bis) : passage de RI au repère véhicule
        ! pterga : passage du Topo au RI
        ! -> pterav : passage du Topo au Repère véhicule (ou repère avion)
        !
        ! A partir de pterav, on calcule les angles de rotation (Cardan standard)
        ! qui servent à positionner le véhicule dans le repère topocentrique local (lié au véhicule)
        ! et à calculer l'incidence, le dérapage et la gîte avec ma_avion_vit

        orig_topo%lat = -latgeo
        orig_topo%long = longeo + pi
        !       valeur arbitraire, non utilisee pour les resultats que l'on cherche
        orig_topo%haut = str_mod(iveh)%requa  

        call mt_topo_N_ref (orig_topo, str_mod(iveh)%requa, 0._pm_reel, &
             (/0._pm_reel, 0._pm_reel, 0._pm_reel/),  pos_ref, &
             code_erreur, jacob=jacob)

        ptergr(1:3, 1:3) = jacob (1:3, 1:3)
        

        ! Matrice de rotation pour le passage entre 
        !  Repère d'intégration et Greenwich (repère planétocentrique à date courante)
        !
        ! pas de retour en erreur possible
        call ps_mat_rotation_tsid(tsid,pgamgr)
        
        ! produits
        ! décomposition en matrices élémentaires pour éviter les fuites mémoire

        call mu_transpose3(ptergr,pgrter,code_retour_local)
        call mu_matmul3(pgrter,pgamgr,pgater,code_retour_local)
        
        ! pas de test sur le code retour de ces deux fonctions car pas d erreur possible

        call mu_transpose3(pgater,pterga,code_retour_local)
        call mu_matmul3(pgamavbis,pterga,pterav,code_retour_local)
        ! pas de test sur le code retour de ces deux fonctions car pas d erreur possible

        !       calcul des angles d'attitude

        call mu_mat_quat (pterav, quaternion, code_erreur)
        if (code_erreur%valeur < 0) then
           call MSP_signaler_message (ier_mslib=code_erreur)
           if (MSP_gen_messages("ps_calcul_incidence" )) return
        end if

        call mu_quat_3rot (pm_1z_2y_3x, quaternion, psis, tets, phis, &
             code_erreur)
        if (code_erreur%valeur < 0) then
           call MSP_signaler_message (ier_mslib=code_erreur)
           if (MSP_gen_messages("ps_calcul_incidence" )) return
        end if




        ! initialisation variable pole pour appel MSP_conv_typrep         
        pole%u = str_bul(iveh)%polu
        pole%v = str_bul(iveh)%polv         

        ! Passage Planeto absolu en Planeto relatif

        ! Test de la valeur de apla_r avant passage de 1/apla_r
        if (str_mod(iveh)%apla .different. 0._PM_REEL) then
           apla_rep = 1._PM_REEL / str_mod(iveh)%apla
        else
           apla_rep = 0._PM_REEL
        endif

        call MSP_conv_typrep ((/pos,vit/),&
             MSP_ENUM_PLANETO_REF_INER, MSP_ENUM_ECHD_DATE_REF, str_bul(iveh)%num_pla, &
             str_bul(iveh)%datbul_js, MSP_ENUM_ECHT_TE, 0._pm_reel, sta, &
             str_mod(iveh)%requa, apla_rep, &
             MSP_ENUM_PLANETO_REF, MSP_ENUM_ECHD_DATE_BUL, str_bul(iveh)%num_pla,&
             date, MSP_ENUM_ECHT_TE, 0._pm_reel, sta, &
             str_mod(iveh)%requa, apla_rep, str_mod(iveh)%vitrotpla, &
             pvr, rdxdp, obli = str_bul(iveh)%obli, pole_in = pole, pole_out = pole)

        if ( MSP_gen_messages("ps_calcul_incidence") ) return 

        ! -- Conversion du type de bulletin, en utilisant la mspro

        ellips%r_equa = str_mod(iveh)%requa
        ellips%apla   = apla_rep

        call mx_var(MSP_ENUM_CARTESIENS, pvr, MSP_ENUM_RENTREE_ELLIPSOIDIQUE, parren, &
             code_erreur, mu = str_mod(iveh)%gmu, ellips = ellips)

        if ( code_erreur%valeur < 0 ) then
           call MSP_signaler_message (ier_mslib=code_erreur)
           if (MSP_gen_messages("ps_calcul_incidence" )) return
        endif

        if (MSP_ERREUR) then
           call MSP_signaler_message(cle_mes="PS_CONV_BUL_005",routine="ps_calcul_incidence") 
           return
        endif

        !       Attention : pente et azimut sont donnés par parren(4) et parren(5) dans la MSPRO.
        pente  = parren(4)
        azimut = parren(5)


        ! Calcul de l'incidence du véhicule, dans le repère aérodynamique
        !================================================================
        ! Explications à propos de la gestion d'erreur
        ! - ma_avion_vit ne peut calculer ses sorties (incidence, derapage, gite) si la vitesse du véhicule
        ! est orthonormale au plan de symétrie du véhicule (X,Z) :
        !  -> dans ce cas précis, selon les plateformes, la valeur rendue peut être nulle, ou indéfinie
        ! 
        ! PSIMU doit traiter ce cas d'erreur : le calcul peut continuer, même si cette valeur, à cette date donnée
        ! n'a pas pu être calculée
        ! Comme la valeur d'incidence sert à interpoler des valeurs de coefficients incidence/mach
        ! il est important que la valeur soit une valeur correcte, c'est à dire dans l'intervalle de validité de l'interpolation
        !
        ! -> le contournement de l'erreur est donc le suivant : 
        ! Si la routine sort en erreur avec le code lié à la vitesse orthonormale au plan de symétrie, 
        ! alors la valeur d'incidence rendue est la précédente valeur
        ! Sinon (cas "normal"), on sauvegarde la valeur rendue dans tab_incidence(iveh), et on rend cette valeur.
        
        call ma_avion_vit (pente, azimut, psis, tets, phis, valeur_incidence, derapage, gite_vit, code_erreur)
        if(code_erreur%valeur == PM_ERR_VIT_PLAN_SYM_AVION_ORTHO) then
           call MSP_signaler_message (cle_mes="PS_ATM_ma_avion_vit",routine="ps_calcul_incidence")
           
           incidence = tab_incidence(iveh)
           derapage  = 0._pm_reel
           gite_vit  = 0._pm_reel
           
        elseif (code_erreur%valeur < 0) then
           call MSP_signaler_message (ier_mslib=code_erreur)
           if (MSP_gen_messages("ps_calcul_incidence" )) return
        else
           
           incidence = valeur_incidence
           tab_incidence(iveh) = valeur_incidence
        end if
        
      end subroutine ps_calcul_incidence


end module ps_atmosphere
