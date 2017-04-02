module ps_attitude

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_attitude
!
!$Resume
!  Module gérant les lois d'attitude dans PSIMU.
!
!$Description
!  Module gérant les lois d'attitude dans PSIMU.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_attitude.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_attitude.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.80  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.79  2009/12/15 08:55:44  mercadig
!  VERSION::FA-ID:1288:15/12/2009:Initialisation des angles d attitude dans le repere local
!
!  Revision 1.78  2009/12/03 13:13:06  mercadig
!  AQ: Suppression de variables inutilisees
!
!  Revision 1.77  2009/11/30 17:37:39  mercadig
!  AQ: Correction des tests sur reels (ps_compare_loi_att) avec utilisation des epsilon MSP_EPSILON_DATE et MSP_EPSILON_APLA
!
!  Revision 1.76  2009/11/19 15:50:55  mercadig
!  DM-ID 1019: Ajout commentaire ris
!
!  Revision 1.75  2009/11/18 13:20:42  kvernelo
!  AQ:libérations mémoire
!
!  Revision 1.74  2009/11/17 09:46:47  mercadig
!  DM-ID 1018: Correction dans la routine psi_Mat_RI_Ratt_terre
!
!  Revision 1.73  2009/10/28 16:04:54  mercadig
!  DM-ID 1018: Gestion des differents reperes inertiels, calcul des matrices de passage et conversion des angles d attitude dans le RI
!
!  Revision 1.72  2009/09/04 15:18:28  mercadig
!  DM-ID 1218: Mise a jour dans les appels a ps_propage
!  Revision 1.71  2009/04/14 16:08:15  tanguyy
!  DM-ID 1274 : routines internes pour analyser des lois d'attitude
!  Revision 1.70  2009/02/13 14:54:03  tanguyy
!  FA-ID 1183 : suppression de la routine psaccsurf (code mort)
!  Revision 1.69  2008/12/02 16:34:16  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!  Revision 1.68  2008/12/02 10:48:33  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.67  2008/11/18 13:35:33  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_calcul_forces
!  Revision 1.66  2008/10/20 14:35:51  mercadig
!  FA-ID 1129 : Correction sur le test du terme sin2
!  Revision 1.65  2008/10/17 10:01:58  mercadig
!  DM-ID 1058 Initialisations de variables et calcul de la normale pour le cas sphere
!  Revision 1.64  2008/09/22 09:44:28  mercadig
!  FA-ID 1017 Correction appel de acc_close
!  Revision 1.63  2008/09/04 07:52:56  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.62  2008/04/04 13:38:05  ttn
!  FA-ID 940 : Comparaison de deux entites physiques reelles
!  Revision 1.60  2008/04/03 16:08:35  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.58  2008/03/07 10:00:16  huec
!  DM-ID 859 : Utilisation de mu_matmul3, tests des code_erreur
!  Revision 1.57  2008/02/15 16:37:30  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.56  2007/11/29 14:42:42  jpi
!  DM-ID 733 : annulation de la DM donc des modifs
!  Revision 1.55  2007/10/30 08:46:17  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.54  2007/07/13 15:27:58  tanguyy
!  AQ : PSIMU V9.0 - maj de commentaires
!  Revision 1.53  2007/07/09 11:58:21  tanguyy
!  DM-ID 688 : mode Terre/Mars basé sur le code NAIF
!  Revision 1.52  2007/06/20 12:33:27  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.50.2.2  2007/04/17 08:23:28  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.50.2.1  2007/04/16 09:42:47  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.51  2007/06/20 12:21:35  vivaresf
!  FA-ID 746 : Fuites mémoires, scénarios de sortie en inout pour permettre leur désallocation
!  Revision 1.50.2.2  2007/04/17 08:23:28  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.50.2.1  2007/04/16 09:42:47  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.50  2007/01/19 15:11:17  vivaresf
!  FA-ID 678 : Cloture du FT (Core Dump PSIMU)
!  Revision 1.49.2.1  2006/12/22 10:41:07  vivaresf
!  FA-ID 678 : Version initiale du FT (Core Dump PSIMU)
!  Revision 1.49  2006/12/05 15:09:50  tanguyy
!  AQ : maj des commentaires de ps_convertir_angles
!  Revision 1.48  2006/11/30 15:20:32  vpg
!  DM-ID 425 : portage Linux. Remplacement des fonctions dfloat, dmod et dsqrt par real, mod et sqrt
!  Revision 1.47  2006/11/16 10:10:39  tanguyy
!  DM-ID 552 : conversion des angles vers des angles Cardan ZYX au moment du calcul
!  Revision 1.46  2006/10/19 15:08:58  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.45.2.2  2006/10/17 09:54:20  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.45.2.1  2006/10/13 07:55:20  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.45  2006/10/02 14:01:48  aitiere
!  AQ : maj des declarations
!  Revision 1.44  2006/09/25 10:21:27  tanguyy
!  AQ : Suppression de variables inutilisées
!  Revision 1.43  2006/04/20 13:23:16  tanguyy
!  DM-ID 400 : utilisation de routines MSP rendant un pointeur sur la structure encapsulee dans les scenarios de lois
!  Revision 1.42.2.1  2006/04/18 09:30:21  tanguyy
!  DM 400 : essai 1 avec utilisation MSP_consulter_ptr_loi
!  Revision 1.42  2006/03/15 13:25:16  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.41  2006/02/27 15:45:53  tanguyy
!  FA-ID 430 : phi ne doit pas etre inverse avant appel a psattop
!  Revision 1.40  2005/12/14 11:14:01  tanguyy
!  Correction d un test pour la recherche d une loi d attitude
!  Revision 1.39  2005/11/25 14:50:54  tanguyy
!  DM-ID 233
!  Revision 1.38  2005/11/10 18:37:04  vivaresf
!  Mise à jour des cartouches
!  Revision 1.37  2005/11/09 13:23:10  vivaresf
!  DM-ID 6 : remplacement de MSP_posvit_3corps par ps_propage
!  Revision 1.36.2.1  2005/11/22 15:16:54  tanguyy
!  DM-ID 233, Utilisation du pointeur loi_courante pour ameliorer les acces aux scenarios d attitude
!  Revision 1.36  2005/03/17 08:27:28  vivaresf
!  MSLIB90 V6.2
!  Revision 1.35  2005/01/28 10:42:11  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.34  2005/01/25 16:19:16  fabrec
!  DM-ID 175 : warning pm_warm_extrapol_borne_double
!  Revision 1.33  2005/01/21 16:03:53  fabrec
!  DM-ID 175 : dates
!  Revision 1.32  2005/01/17 15:23:39  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa d'attitude
!  Revision 1.31  2004/12/10 16:51:51  fabrec
!  DM-ID 175 : nouveau type de loi fichier
!  Revision 1.30  2004/11/22 14:09:38  fabrec
!  DM_175
!  Revision 1.29.2.1  2004/11/22 14:01:33  fabrec
!  Type d'attitudes : fichier
!  Revision 1.29.6.1  2007/04/26 15:59:24  tanguyy
!  Patch PSIMU V8-1p3 : FA-ID 678 et FA-ID 730
!  Revision 1.29.4.1  2007/04/26 15:16:10  tanguyy
!  Patch PSIMU V8-1p3 : correction de la FA 678 et de la FA 730
!  Revision 1.29  2004/10/05 16:29:20  vivaresf
!  FA-ID 204 : derniere loi d'attitude conservee meme apres sa fin
!  Revision 1.18  2004/09/20 13:25:46  vivaresf
!  version V7-3-1
!  Revision 1.28  2004/03/31 14:25:41  adm_ipsi
!  FA-ID 117, Passage matrice 6,6 pour Mat_RI_ME_2000, et utilisation de la sous-matrice 3,3 ici
!  Revision 1.27  2004/01/15 16:33:23  adm_ipsi
!  DM-ID 10, les calculs internes de PSIMU se font en date TE
!  Revision 1.26  2003/11/25 17:50:05  adm_ipsi
!  DM-ID 80, filtrage des warnings sur mx_var et mu_inter_dim1_lin
!  Revision 1.25  2003/10/01 16:17:49  adm_ipsi
!   Suppression de lignes en commentaire
!  Revision 1.24  2003/08/05 15:23:50  adm_ipsi
!  DM-ID 30 , Suppression de l'appel à MSP_conv_typrep et MSP_conv_typbul pour optimiser lmps de calcul
!  Revision 1.23  2003/03/20 16:58:24  laurent
!  remplacement de IO_a_angatt par des routines de la mspro
!  Revision 1.22  2003/03/14 15:41:57  adm_ipsi
!  Utilisation de ier_mslib au lieu de ier_mspro suite à la modification de MSP_signaler_message
!  Revision 1.21  2003/03/14 15:25:27  adm_ipsi
!  Utilisation de ps_nloi_att, ps_nloi_propu, ps_npts_att, ps_npts_propu, ps_nsepa
!  Revision 1.20  2003/03/12 14:20:22  adm_ipsi
!   Utilisation de MSP_gen_messages
!  Revision 1.19  2003/02/19 11:00:07  boschett
!  A Deramecourt : modif ier_mslib en ier_mspro pour les appels MSPRO
!  Revision 1.18  2003/02/19 10:42:56  rodier
!  PhB - Modification indices pente, azimut + fic_tuc dans MSP_posvit_3corps
!  Revision 1.17  2003/02/18 09:01:38  rodier
!  PhB - Correction appel à MSP_conv_typrep et MSP_conv_typbul
!  Revision 1.16  2003/02/14 16:03:48  rodier
!  PhB - MSP_conv_typbul - MSP_conv_typrep - mu_quat_3rot
!  Revision 1.15  2003/02/12 17:06:21  boschett
!  A Deramecourt : retour en arriere suite coredump sur mu_mat_quat
!  Revision 1.14  2003/02/12 16:49:54  boschett
!  A Deramecourt : portage de IO_r_pategr (iolib) en mt_topo_N_ref (mslib90)
!  Revision 1.13  2003/02/12 10:25:42  adm_ipsi
!  Remplacement de IO_p_cvbul par MSP_conv_typbul
!  Revision 1.12  2003/02/11 12:11:02  boschett
!  A Deramecourt : portage de l'appel de IO_a_angatt (iolib) en appel de mu_mat_quat et de mu_quat_3rot (mspro)
!  Revision 1.11  2003/02/11 10:12:43  boschett
!  A Deramecourt : portage de l'appel de IO_r_cremat (iolib) en appel de mu_3rot_quat + mu_quat_mat (mspro)
!  Revision 1.10  2003/02/05 12:13:56  boschett
!  A Deramecourt : portage de IO_m_dlin1 (iolib) en mu_inter_dim1_lin de la MSRPO
!  Revision 1.9  2003/02/04 16:22:50  boschett
!  A Deramecourt : remplacement de IO_m_cosiang de la IOlib par atan2
!  Revision 1.8  2003/01/31 17:42:27  boschett
!  A Deramecourt : modif appel de IO_a_indeat en mspro ma_avion_vit
!  Revision 1.7  2003/01/31 12:32:28  boschett
!  A Deramecourt : suppression appels mumat3, muvec3, muvectr3 de la amlib
!  Revision 1.6  2003/01/29 17:06:58  boschett
!  A Deramecourt : modif AM_math_mumat3 en mumatp de la mslib77
!  Revision 1.5  2002/12/20 16:38:10  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.4  2002/12/04 14:22:51  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.3  2002/12/04 09:43:28  boschett
!  Substitution du mot clé 'type' par 'typloi' dans la subroutine psattit et par 'typforce' dans psaccsurf
!  Revision 1.2  2002/11/26 15:51:00  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.12  2002/09/16 11:06:02  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.11  2002/06/24 13:29:28  util_am
!  Meilleure optimisation de la recherche 3ème corps engardant la mémoire de l'indice
!  Revision 1.10  2001/11/09 08:58:14  util_am
!  Appel de la routine MSP_posvit_3corps avec l'argument echt pour donner une date en TE
!  Revision 1.9  2000/09/05 12:52:03  util_am
!  Passage à 10 véhicules au lieu de 100
!  Revision 1.8  2000/06/27 11:42:39  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.7  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.6  2000/01/14 12:16:31  util_am
!  Correction sur le choix de la loi d'attitude quand la date de fin de la loi i-1 est égale à la date de début de la loi i+1 (spécialement pour les poussées et les séparations)
!  Revision 1.5  2000/01/07 10:22:52  util_am
!  Bug dans le cas où on n'a pas de loi d'attitude après en avoir eu par une précédente initialisation (mode sous-programme)
!  Revision 1.4  1999/10/26 10:59:03  util_am
!  Modification du type de repère inertiel (Gamma 50 CNES => inertiel)
!  Revision 1.3  1999/08/31 11:56:09  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:10  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_attitude
!
!$Structure
!
!$Global
!
!>  scenar_ati        : <MSP_SCENARIO_LOI,DIM=(PS_NVMAX)>  scenario d'attitude
!>  nloi_scenar_ati   : <integer,DIM=(PS_NVMAX)>           nombre de lois d'attitude du scenario
!>  modatt            : <integer,DIM=(PS_NVMAX)>           modele d'attitude
!$Common
!
!$Routines
!- psattit
!- psatqsw
!- psatlvlh
!- psatsol
!- psatys
!- psattnw
!- psattop
!- psatine
!- pslecatt
!- ps_recupere_date_loi_att
!- ps_compare_loi_att
!#V
!- ps_convertir_angles
!- psi_Mat_RI_Ratt_terre
!- psi_Mat_RI_Ratt_mars_venus
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
!- ps_integration_don
!- ps_troiscorps
!- ps_calcul_forces
!- ps_bulletin
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

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_attitude.F90 368 2013-02-19 14:43:59Z aadt $'


   type(MSP_SCENARIO_LOI), dimension(PS_NVMAX), save   :: scenar_ati
   integer, dimension(PS_NVMAX),save :: nloi_scenar_ati

   integer, dimension(PS_NVMAX),save :: modatt = 0

   private :: ps_convertir_angles, psi_Mat_RI_Ratt_terre, psi_Mat_RI_Ratt_mars_venus

   contains

      subroutine psattit (date,t,x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav,typloi,repere,choix_loi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psattit
!
!$Resume
!  Sous-programme donnant les angles d'attitude dans PSIMU à la date courante.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans PSIMU à la date courante.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psattit (date,t,x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav,typloi,repere,[choix_loi])
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel) :: t
!.    real (KIND=pm_reel), dimension(3) :: x,xp
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!.    integer :: typloi, repere
!.    integer :: choix_loi
!
!$Arguments
!>E     date       :<tm_jour_sec>         date courante (Jours Juliens CNES)
!>E     t          :<pm_reel,DIM=(IN)>    date courante (s)
!>E     x          :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m)
!>E     xp         :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>S     angle1     :<pm_reel>             angle de lacet ou incidence dans le repère "local" [rad]
!>S     angle2     :<pm_reel>             angle de tangage ou dérapage dans le repère "local" [rad]
!>S     angle3     :<pm_reel>             angle de roulis ou gite dans le repère "local" [rad]
!>S     angw1      :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad]
!>S     angw2      :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3      :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav     :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!>S     typloi     :<integer>             type de loi d'attitude
!>S     repere     :<integer>             type de repère "local" pour la définition de l'attitude
!>[E]   choix_loi  :<integer>             principe de choix de la loi d'attitude quand deux lois i/i+1 se jouxtent:
!                                         si 1: on prend la loi i, si 2: on prend la loi i+1 (valeur par défaut = 1)
!                                         si <= 0, on force un type de loi => le type est alors égal à -choix_loi
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_premiere_loi
!- MSP_signaler_message
!- MSP_consulter_ptr_loi
!- MSP_consulter_attitude_tabulee
!- MSP_consulter_attitude_spinnee
!- MSP_loi_suivante
!- MSP_positionner_loi_courante
!- Msp_consulter_ptr_loi
!- mu_inter_dim1_lin
!- psatine
!- psatqsw
!- psattnw
!- psatsol
!- psattop
!- mr_tsid_veis
!- mu_mulvect3
!- mt_car_meca_vol
!- ma_avion_sol
!- psatlvlh
!- psatys
!#V
!- ps_convertir_angles
!#
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_troiscorps
!- ps_calcul_forces
!- ps_bulletin
!- ps_generalites
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_integration_don
      use ps_troiscorps
      use ps_calcul_forces
      use ps_bulletin
      use ps_generalites

      implicit none

      ! Arguments
      !==========
      
      type(tm_jour_sec),   intent(IN)                  :: date
      real (KIND=pm_reel), intent(IN)                  :: t
      real (KIND=pm_reel), intent(IN), dimension(3)    :: x,xp

      real (KIND=pm_reel), intent(OUT)                 :: angle1,angle2,angle3
      real (KIND=pm_reel), intent(OUT)                 :: angw1,angw2,angw3
      real (KIND=pm_reel), intent(OUT), dimension(3,3) :: pgamav

      integer, intent(OUT) :: typloi, repere

      integer, intent(IN), optional :: choix_loi
      
      ! Variables locales
      !==================

      integer :: i,im1(PS_NVMAX),numloi, ch_loi
      real (KIND=pm_reel) :: psit,tett,phit,pvr(6),deltat
      real(kind=pm_reel), dimension(3,3) :: mat_Pla_Green
      real (KIND=pm_reel) :: apla_rep

      real (KIND=pm_reel) :: tsid1, tsid2, rot_Z, crot_Z, srot_Z

      type(tm_geodesique)   :: pos_geod
      type(tm_vit_meca_vol) :: vit_meca_vol
      type (tm_jour_sec)    :: jul1950
      type (tm_code_retour) :: code_erreur, code_retour_local

      type(MSP_LOI), pointer :: loi_courante
      type(MSP_ATTITUDE_TABULEE), pointer :: loi_atti_tab
      type(MSP_ATTITUDE_SPINNEE), pointer :: loi_atti_spin

      integer :: ntab, allocstat
      real(KIND=PM_REEL) :: t_deb, t_fin
      real(KIND=PM_REEL), dimension(:), pointer :: dates_relatives
      real(KIND=pm_reel), dimension(:), pointer :: psi
      real(KIND=pm_reel), dimension(:), pointer :: teta
      real(KIND=pm_reel), dimension(:), pointer :: phi
      real(KIND=PM_REEL) :: psi0, teta0, phi0, psip, tetap, phip
      integer :: convention ! convention d'angles (Cardan, Euler : cf MSLIB, theme U)
      
      
      data im1 / PS_NVMAX* 1 /

      save im1

      ! Début du code
      !==============
      nullify(loi_courante)
      nullify(loi_atti_tab)
      nullify(loi_atti_spin)
      nullify(dates_relatives)
      nullify(psi)
      nullify(teta)
      nullify(phi)
      
      if ( present(choix_loi) ) then
         ch_loi = choix_loi
      else
         ch_loi = 1
      endif

      ! Cas ou il n'y a pas de loi d'attitude:
      if ( (modatt(iveh) == 0) .and. (ch_loi > 0) ) then

            angw1 = 0._pm_reel
            angw2 = 0._pm_reel
            angw3 = 0._pm_reel
            pgamav(:,:) = 0._pm_reel
            typloi = -1
            repere = -1
            angle1 = 0._pm_reel
            angle2 = 0._pm_reel 
            angle3 = 0._pm_reel

      ! Cas où il y a au moins une loi d'attitude:

         else

         if ( ch_loi <= 0 ) then

            angle1 = 0._pm_reel
            angle2 = 0._pm_reel
            angle3 = 0._pm_reel
            typloi = -1
            repere = -ch_loi

         else
	 
            ! Initialisation des angles d'attitude (dans le repère local)
            angle1 = 0._pm_reel
            angle2 = 0._pm_reel 
            angle3 = 0._pm_reel

            ! Test pour savoir quelle loi utiliser:

            numloi = 0

!/  Lecture de la structure MSP_SCENARIO_LOI
            call MSP_consulter_scenario (scenar_ati(iveh), nloi=nloi_scenar_ati(iveh))
            if ( MSP_gen_messages("psattit") ) return

!/ DM-ID 233 : on réalise la boucle en utilisant le pointeur loi courante             
            call MSP_premiere_loi(scenar_ati(iveh), loi_courante)
            if ( MSP_gen_messages("psattit") ) return

            nu_loi: do i = 1 , nloi_scenar_ati(iveh)

               !/ Test de la validité de la loi courante
               if( .not. MSP_test_loi_courante(scenar_ati(iveh))) then
                  ! traitement d'erreur si le pointeur n'est pas associé..
                  ! /!\ Normalement, on ne passe pas ici, car 
                  ! la boucle est réalisée sur le nb de lois
                  call MSP_signaler_message (cle_mes="PS_LOI_COURANTE", &
                       routine="psattit",type=MSP_ENUM_ERREUR)
                  exit nu_loi
               end if
               !/  Extraction du type de la loi
               typloi = MSP_type_loi (scenar_ati(iveh))
               if ( MSP_gen_messages("psattit") ) then
                  return
               end if

               if(typloi == 0) then

                  call MSP_signaler_message (cle_mes="PS_TYPE_LOI_COURANTE", &
                       routine="psattit",type=MSP_ENUM_ERREUR)

               elseif (typloi == MSP_ENUM_LOI_ATTI_TAB) then
                  
                  call MSP_consulter_ptr_loi(scenar_ati(iveh), loi_atti_tab = loi_atti_tab)
                  if ( MSP_gen_messages("psattit") ) return


                  call MSP_consulter_attitude_tabulee (loi_atti_tab,&
                       ntab=ntab, dates=dates_relatives)
                  if ( MSP_gen_messages("psattit") ) then
                     return
                  end if

               elseif (typloi == MSP_ENUM_LOI_ATTI_SPIN) then
                  !/  extraction de la structure loi d'attitude dans la structure MSP_SCENARIO_LOI
                  call MSP_consulter_ptr_loi(scenar_ati(iveh), loi_atti_spin = loi_atti_spin)
                  if ( MSP_gen_messages("psattit") ) return

                  call MSP_consulter_attitude_spinnee (&
                       loi_atti_spin,&
                       datedeb=t_deb, datefin=t_fin)

                  if ( MSP_gen_messages("psattit") ) return
                  ntab = 2
                  if (associated(dates_relatives)) &
                       deallocate(dates_relatives, stat=allocstat)
                  allocate(dates_relatives(2))
                  dates_relatives(1) = t_deb
                  dates_relatives(2) = t_fin
               endif

               if ( t <=  dates_relatives(ntab) ) then
                  numloi = i
                  if ( abs(t-dates_relatives(ntab)) < PS_EPSILON  .and. (ch_loi == 2) ) then
                     numloi = numloi + 1
                     if ( numloi > nloi_scenar_ati(iveh) ) numloi = nloi_scenar_ati(iveh)
                  endif
                  exit nu_loi
	       endif

               !/ Incrementation du pointeur, afin de passer à la loi suivante :
               call MSP_loi_suivante(scenar_ati(iveh))
               if ( MSP_gen_messages("psattit") ) return

            enddo nu_loi
               
            if ( i > nloi_scenar_ati(iveh) ) numloi = nloi_scenar_ati(iveh)
            if (associated(dates_relatives)) deallocate(dates_relatives)

            !/ Positionnement de la loi courante sur la loi n°numloi ;
            !/ les fonctions d'acces msp_recuperer_loi iront chercher dans la loi courante.
            if (numloi /= 0) then
               call MSP_positionner_loi_courante(scenar_ati(iveh), id=numloi)
               if ( MSP_gen_messages("psattit") ) return
            endif

            
            if ( numloi /= 0 ) then

               !/  Extraction du type de la loi
               typloi = MSP_type_loi (scenar_ati(iveh))
               if ( MSP_gen_messages("psattit") ) then
                  return
               end if
               if(typloi == 0) then
                  call MSP_signaler_message (cle_mes="PS_TYPE_LOI_COURANTE", &
                       routine="psattit",type=MSP_ENUM_ERREUR)
               end if

               if ( MSP_gen_messages("psattit") ) return
               if (typloi == MSP_ENUM_LOI_ATTI_TAB) then
                  ! Cas d'une loi de type MSP_ENUM_LOI_ATTI_TAB
                  !============================================
                  
                  !/  extraction de la structure loi d'attitude dans la structure MSP_SCENARIO_LOI
                  call Msp_consulter_ptr_loi(scenar_ati(iveh),loi_atti_tab=loi_atti_tab)
                  if ( MSP_gen_messages("psattit") ) return


                  call MSP_consulter_attitude_tabulee (loi_atti_tab,&
                       ntab=ntab, typrep=repere, dates=dates_relatives, &
                       psi=psi, teta=teta, phi=phi, typangle=convention)
                  if ( MSP_gen_messages("psattit") ) return


                  ! Interpolation successive des trois angles
                  call mu_inter_dim1_lin (ntab, dates_relatives(1:ntab), &
                                          psi(1:ntab), t, im1(iveh), &
                                          angle1, code_erreur, x_int_bis=.FALSE.)

                  if ( (code_erreur%valeur /= pm_warn_extrapol) .and. &
                     (code_erreur%valeur /= pm_warn_extrapol_borne_double) .and. (code_erreur%valeur /= pm_OK) ) then
                     call MSP_signaler_message (ier_mslib=code_erreur)
                     if (MSP_gen_messages("psattit") ) return
                  endif


                  call mu_inter_dim1_lin (ntab, dates_relatives(1:ntab), &
                                          teta(1:ntab), t, im1(iveh), &
                                          angle2, code_erreur, x_int_bis=.TRUE.)

                  if ( (code_erreur%valeur /= pm_warn_extrapol) .and.  &
                     (code_erreur%valeur /= pm_warn_extrapol_borne_double) .and. (code_erreur%valeur /= pm_OK) ) then
                     call MSP_signaler_message (ier_mslib=code_erreur)
                     if (MSP_gen_messages("psattit") ) return
                  endif

                  call mu_inter_dim1_lin (ntab, dates_relatives, &
                                          phi, t, im1(iveh), &
                                          angle3, code_erreur, x_int_bis=.TRUE.)

                  if ( (code_erreur%valeur /= pm_warn_extrapol) .and.  &
                     (code_erreur%valeur /= pm_warn_extrapol_borne_double) .and. (code_erreur%valeur /= pm_OK) ) then
                    call MSP_signaler_message (ier_mslib=code_erreur)
                     if (MSP_gen_messages("psattit") ) return
                  endif
                   		  
                  ! Conversion du triplet d'angles en angles de Cardan Z Y X (convention "mécanique de vol")
                  ! Attention : la saisie d'angles d'attitude dans le repère aérodynamique
                  ! ne suit pas les conventions classiques "Cardan / Euler"
                  ! --> on ne les convertit donc pas
                  if (repere /= MSP_ENUM_ATTI_AERODYNAMIQUE) then
                     call ps_convertir_angles(convention,angle1,angle2,angle3) 
                     if(MSP_gen_messages("psattit")) return
                  end if
               else
                  ! Cas d'une loi de type MSP_ENUM_LOI_ATTI_SPIN:
                  !==============================================

                  !/  extraction de la structure loi d'attitude dans la structure MSP_SCENARIO_LOI
                  call Msp_consulter_ptr_loi(scenar_ati(iveh),loi_atti_spin=loi_atti_spin)
                  if ( MSP_gen_messages("psattit") ) return
                  


                  call MSP_consulter_attitude_spinnee (loi_atti_spin,&
                       datedeb=t_deb, datefin=t_fin, typrep=repere, &
                       psi0=psi0, teta0=teta0, phi0=phi0, &
                       psip=psip, tetap=tetap, phip=phip, typangle=convention)
                  if ( MSP_gen_messages("psattit") ) return

                  deltat = t - t_deb
                  angle1 = mod(psi0+psip*deltat,dpi)
                  angle2 = mod(teta0+tetap*deltat,dpi)
                  angle3 = mod(phi0+phip*deltat,dpi)
                  
                  ! Conversion du triplet d'angles en angles de Cardan Z Y X (convention "mécanique de vol")
                  ! Attention : la saisie d'angles d'attitude dans le repère aérodynamique
                  ! ne suit pas les conventions classiques "Cardan / Euler"
                  ! --> on ne les convertit donc pas
                  if (repere /= MSP_ENUM_ATTI_AERODYNAMIQUE) then
                     call ps_convertir_angles(convention,angle1,angle2,angle3)
                     if(MSP_gen_messages("psattit")) return
                  end if
                  
               endif

            else

               typloi = -1
               repere = -1

            endif

         endif

         ! Calcul des angles d'attitude dans le repère d'intégration:         

         select case ( repere )

            case (MSP_ENUM_ATTI_INERTIEL_G50,MSP_ENUM_ATTI_INERTIEL_J2000,MSP_ENUM_ATTI_INERTIEL_GVrai)

               call psatine (date,x,xp,repere,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_QSW)

               call psatqsw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_TNW)

               call psattnw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_POINTE_SOLAIRE)

               call psatsol (date,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_TOPO_LOCAL)

               call psattop (x,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_AERODYNAMIQUE)
               
               ! Passage Planeto absolu en planeto relatif

               ! Test de la valeur de apla_r avant passage de 1/apla_r
               if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
                  apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
               else
                  apla_rep = 0._PM_REEL
               endif
               
               ! Passage Planeto absolu en Planeto relatif

               if ( str_gen(iveh)%planet == eph_terre ) then

                  ! Calcul du temps sidéral à datbul, convertie en TUC
                  jul1950 = str_bul(iveh)%datbul_js + str_bul(iveh)%ecart_te_tuc

     		  call mr_tsid_veis (jul1950, 0._pm_reel , tsid1, code_erreur)
		  if (code_erreur%valeur < 0) then
		     call MSP_signaler_message (ier_mslib=code_erreur)
		     if (MSP_gen_messages("psattit")) return
		  end if
		  
                  ! Calcul du temps sidéral à la date courante (en TUC)
                  jul1950 = date + str_bul(iveh)%ecart_te_tuc

		  call mr_tsid_veis (jul1950, 0._pm_reel, tsid2, code_erreur)
		  if (code_erreur%valeur < 0) then
		     call MSP_signaler_message (ier_mslib=code_erreur)
		     if (MSP_gen_messages("psattit")) return
		  end if
		  
                  ! Calcul de la rotation autour de z 
               
                  rot_Z = tsid2 - tsid1

               else
                  ! /!\ soustraction de deux dates jj/sec --> quantité en secondes (cf thème D MSLIB)
                  rot_Z = str_mod(iveh)%vitrotpla*(date - str_bul(iveh)%datbul_js)
               endif

               crot_Z = cos(rot_Z)
               srot_Z = sin(rot_Z)
            
               ! Construction de la matrice de passage du planéto à Greenwich

               mat_Pla_Green(1,1) = crot_Z
               mat_Pla_Green(1,2) = srot_Z
               mat_Pla_Green(1,3) = 0._pm_reel
               mat_Pla_Green(2,1) = -srot_Z
               mat_Pla_Green(2,2) = crot_Z
               mat_Pla_Green(2,3) = 0._pm_reel
               mat_Pla_Green(3,1) = 0._pm_reel
               mat_Pla_Green(3,2) = 0._pm_reel
               mat_Pla_Green(3,3) = 1._pm_reel


               ! Prise en compte de la rotation du corps
               call mu_mulvect3(mat_Pla_Green,x,pvr(1:3),code_retour_local)
               call mu_mulvect3(mat_Pla_Green,xp,pvr(4:6),code_retour_local)

               ! Calcul de la vitesse relative
               pvr(4) = pvr(4) + str_mod(iveh)%vitrotpla*pvr(2)
               pvr(5) = pvr(5) - str_mod(iveh)%vitrotpla*pvr(1)
                                
               ! -- Conversion du type de bulletin en utilisant la mspro

               ! Test de la valeur de apla_r avant passage de 1/apla_r
               if (str_mod(iveh)%apla .different. 0._PM_REEL) then
                  apla_rep = 1._PM_REEL / str_mod(iveh)%apla
               else
                  apla_rep = 0._PM_REEL
               endif


               ! Passage des coordonnées cartésiennes aux coordonnées mécanique de vol
               ! sans utiliser mx_var pour optimisation du temps de calcul

               call mt_car_meca_vol(str_mod(iveh)%requa,apla_rep,pvr(1:3),pvr(4:6), &
                                    pos_geod, vit_meca_vol,code_erreur)
               if (code_erreur%valeur < 0) then
                  call MSP_signaler_message (ier_mslib=code_erreur)
                  if (MSP_gen_messages("psattit" )) return
               end if

               call ma_avion_sol (angle1, angle2,vit_meca_vol%pente ,vit_meca_vol%azimut , angle3,  &
                                  psit, tett, phit, code_erreur)
               if (code_erreur%valeur < 0) then
                  call MSP_signaler_message (ier_mslib=code_erreur)
                  if (MSP_gen_messages("psattit" )) return
               end if

               !/ Note : psit,tett,phit : azimut, assiette et gîte de l'avion
               !/        par rapport au triedre normal au sol et porté par l'avion
               call psattop (x,psit,tett,phit,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_LVLH)

               call psatlvlh (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case (MSP_ENUM_ATTI_YAW_STEERING)

               call psatys (date,x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
               if ( MSP_gen_messages ("psattit") ) return

            case default

               modatt(iveh) = 0
               angw1 = 0._pm_reel
               angw2 = 0._pm_reel
               angw3 = 0._pm_reel
               pgamav(:,:) = 0._pm_reel

        end select

      endif 

      ! Libération de la mémoire
      if (associated(dates_relatives)) deallocate(dates_relatives)
      if (associated(psi)) deallocate(psi)
      if (associated(teta)) deallocate(teta)
      if (associated(phi)) deallocate(phi)

      end subroutine psattit

      subroutine psatqsw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psatqsw
!
!$Resume
!  Sous-programme donnant les angles d'attitude "QSW" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère local QSW.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psatqsw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel), dimension(3) :: x, xp
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     x       :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m) 
!>E     xp      :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère QSW [rad] 
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère QSW [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère QSW [rad]  
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
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

      real (KIND=pm_reel), intent(IN)  :: angle1,angle2,angle3
      real (KIND=pm_reel), dimension(3), intent(IN)  :: x, xp
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      integer :: i,j
      real (KIND=pm_reel) :: r,w
      real (KIND=pm_reel), dimension(3,3) :: qsw, pterav
      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur

!   * Calcul de la matrice de passage de Planeto a QSW :

      r = sqrt ( x(1)**2 + x(2)**2 + x(3)**2 )
      w = sqrt ( (x(2)*xp(3)-x(3)*xp(2))**2 + (x(3)*xp(1)-x(1)*xp(3))**2 + (x(1)*xp(2)-x(2)*xp(1))**2 )

      qsw(1,1) = x(1)/r
      qsw(1,2) = x(2)/r
      qsw(1,3) = x(3)/r
      qsw(2,1) = (xp(1)*(x(2)**2+x(3)**2)-x(1)*(x(2)*xp(2)+x(3)*xp(3))) / (w*r)
      qsw(2,2) = (xp(2)*(x(1)**2+x(3)**2)-x(2)*(x(1)*xp(1)+x(3)*xp(3))) / (w*r)
      qsw(2,3) = (xp(3)*(x(2)**2+x(1)**2)-x(3)*(x(2)*xp(2)+x(1)*xp(1))) / (w*r)
      qsw(3,1) = (x(2)*xp(3)-x(3)*xp(2)) / w
      qsw(3,2) = (x(3)*xp(1)-x(1)*xp(3)) / w
      qsw(3,3) = (x(1)*xp(2)-x(2)*xp(1)) / w

!   * Calcul de la matrice de passage de QSW au repere avion :

      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatqsw" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatqsw" )) return
      end if


!   * Calcul de la matrice de passage de Planeto au repere avion :

      do i = 1,3
         do j = 1,3
            pgamav(i,j) = pterav(i,1)*qsw(1,j) + pterav(i,2)*qsw(2,j) + pterav(i,3)*qsw(3,j)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :

     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatqsw" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatqsw" )) return
     end if


      end subroutine psatqsw

      subroutine psatlvlh (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psatlvlh
!
!$Resume
!  Sous-programme donnant les angles d'attitude "LVLH" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère local LVLH.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psatlvlh (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    real (kind=pm_reel) :: angle1,angle2,angle3
!.    real (kind=pm_reel), dimension(3) :: x, xp
!.    real (kind=pm_reel) :: angw1,angw2,angw3
!.    real (kind=pm_reel), dimension(3,3) :: pgamav(3,3)
!
!$Arguments
!>E     x       :<pm_reel,DIM=(3)>               position dans le repère d'intégration (m) 
!>E     xp      :<pm_reel,DIM=(3)>               vitesse dans le repère d'intégration (m/s)
!>E     angle1  :<pm_reel>                       angle de lacet (psi) dans le repère LVLH [rad] 
!>E     angle2  :<pm_reel>                       angle de tangage (teta) dans le repère LVLH [rad] 
!>E     angle3  :<pm_reel>                       angle de roulis (phi) dans le repère LVLH [rad]  
!>S     angw1   :<pm_reel>                       angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>                       angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>                       angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3),DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
!
!$Include
!
!$Module
!
!$Remarques
!   LVLH correspond en fait à QSW mais avec Z=-Q, Y=-W et X=S
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real (kind=pm_reel), intent(in)  :: angle1,angle2,angle3
      real (kind=pm_reel), dimension(3), intent(in)  :: x, xp
      real (kind=pm_reel), intent(out) :: angw1,angw2,angw3
      real (kind=pm_reel), dimension(3,3), intent(out) :: pgamav(3,3)

      integer :: i,j
      real (kind=pm_reel) :: r, w
      real (kind=pm_reel), dimension(3,3) ::lvlh,pterav

      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur

!   * Calcul de la matrice de passage de Planeto a LVLH :

      r = sqrt ( x(1)**2 + x(2)**2 + x(3)**2 )
      w = sqrt ( (x(2)*xp(3)-x(3)*xp(2))**2 + (x(3)*xp(1)-x(1)*xp(3))**2 + (x(1)*xp(2)-x(2)*xp(1))**2 )

      lvlh(1,1) = (xp(1)*(x(2)**2+x(3)**2)-x(1)*(x(2)*xp(2)+x(3)*xp(3))) / (w*r)
      lvlh(1,2) = (xp(2)*(x(1)**2+x(3)**2)-x(2)*(x(1)*xp(1)+x(3)*xp(3))) / (w*r)
      lvlh(1,3) = (xp(3)*(x(2)**2+x(1)**2)-x(3)*(x(2)*xp(2)+x(1)*xp(1))) / (w*r)
      lvlh(2,1) = -(x(2)*xp(3)-x(3)*xp(2)) / w
      lvlh(2,2) = -(x(3)*xp(1)-x(1)*xp(3)) / w
      lvlh(2,3) = -(x(1)*xp(2)-x(2)*xp(1)) / w
      lvlh(3,1) = -x(1)/r
      lvlh(3,2) = -x(2)/r
      lvlh(3,3) = -x(3)/r

!   * Calcul de la matrice de passage de LVLH au repere avion :

      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatlvlh" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatlvlh" )) return
      end if


!   * Calcul de la matrice de passage de Planeto au repere avion :

      do i = 1,3
         do j = 1,3
            pgamav(i,j) = pterav(i,1)*lvlh(1,j) + pterav(i,2)*lvlh(2,j) + pterav(i,3)*lvlh(3,j)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :

     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatlvlh" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatlvlh" )) return
     end if

      call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatlvlh" )) return
      end if


      end subroutine psatlvlh

      subroutine psatsol (date,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psatsol
!
!$Resume
!  Sous-programme donnant les angles d'attitude "pointé solaire" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère "pointé solaire".
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psatsol (date,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     date    :<tm_jour_sec>         date courante (Jours Juliens CNES)
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère "pointé solaire" [rad] 
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère "pointé solaire" [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère "pointé solaire" [rad]  
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- ps_propage
!- flush
!- mt_topo_N_ref
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
!
!$Include
!
!$Module
!#V
!- ps_troiscorps
!- ps_integration_don
!- ps_calcul_forces
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_troiscorps
      use ps_integration_don
      use ps_calcul_forces

      implicit none
      
      type(tm_jour_sec),   intent(IN)  :: date
      real (KIND=pm_reel), intent(IN)  :: angle1,angle2,angle3
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      integer :: i,j
      real(kind=pm_reel), dimension(3) :: pos_soleil,vecs_RI
      real(kind=pm_reel)  :: mu_soleil,nus,us,vs,ws
      real (KIND=pm_reel) :: sinlat,xlat,xlon
      real (KIND=pm_reel), dimension(3,3) :: rps,pterav,pos_ref
      real (KIND=pm_reel), dimension(6,6) :: jacob

      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur
      type (tm_geodesique) :: orig_topo

!   * Calcul de la position du soleil:
      call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_SOLEIL,&
           pos_soleil,mu_soleil,repinteg=1)
      if( MSP_gen_messages("psatsol") ) return

! FA 725 (optimisation ps_propage)
      vecs_RI = pos_soleil

      call flush(6)

      nus = sqrt ( vecs_RI(1)**2 + vecs_RI(2)**2 + vecs_RI(3)**2  )
      us = vecs_RI(1)/nus
      vs = vecs_RI(2)/nus
      ws = vecs_RI(3)/nus

      sinlat = ws
      xlat = asin(sinlat)

      xlon = atan2 (vs, us)

!   * Calcul de la matrice de passage du repère RPS à Planeto:

      orig_topo%lat = xlat
      orig_topo%long = xlon
!     valeur arbitraire, non utilisee pour les resultats que l'on cherche
      orig_topo%haut = str_mod(iveh)%requa  

      call mt_topo_N_ref (orig_topo, str_mod(iveh)%requa, 0._pm_reel, &
		         (/0._pm_reel, 0._pm_reel, 0._pm_reel/),  pos_ref, &
			 code_erreur, jacob=jacob)

      rps(1:3, 1:3) = jacob (1:3, 1:3)
      
!   * Calcul de la matrice de passage du RPS au repère avion :

      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatsol" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatsol" )) return
      end if


!   * Calcul de la matrice de passage de Planeto au repere avion :

      do i=1,3
         do j=1,3
            pgamav(i,j) = pterav(i,1)*rps(j,1) + pterav(i,2)*rps(j,2) + pterav(i,3)*rps(j,3)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :


     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatsol" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatsol" )) return
     end if

      end subroutine psatsol

      subroutine psatys (date,x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psatys
!
!$Resume
!  Sous-programme donnant les angles d'attitude "yaw steering" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère "yaw steering".
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psatys (date,x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel), dimension(3) :: x,xp
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     date    :<tm_jour_sec>         date courante (Jours Juliens CNES)
!>E     x       :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m) 
!>E     xp      :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère "yaw steering" [rad] 
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère "yaw steering" [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère "yaw steering" [rad]  
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- ps_propage
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
!
!$Include
!
!$Module
!#V
!- ps_troiscorps
!- ps_integration_don
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_troiscorps
      use ps_integration_don

      implicit none

      type(tm_jour_sec),   intent(IN)  :: date
      real (KIND=pm_reel), intent(IN)  :: angle1,angle2,angle3
      real (KIND=pm_reel), dimension(3), intent(IN)  :: x,xp
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      integer :: i,j
      real(kind=pm_reel), dimension(3) :: pos_soleil,vecs_RI

      real(kind=pm_reel)  :: mu_soleil,nus,us,vs,ws,usb,vsb,n_uv,r,w
      real (KIND=pm_reel), dimension(3,3) :: lvlh,ys,pterav

      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur

!   * Calcul de la position du soleil:

      call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_SOLEIL,&
           pos_soleil,mu_soleil, repinteg=1)
      if ( MSP_gen_messages ("psatys") ) return

! FA 725 : optimisation
      vecs_RI = pos_soleil
      
      nus = sqrt ( vecs_RI(1)**2 + vecs_RI(2)**2 + vecs_RI(3)**2  )
      us = vecs_RI(1)/nus
      vs = vecs_RI(2)/nus
      ws = vecs_RI(3)/nus

!   * Calcul de la matrice de passage de Planeto a LVLH :

      r = sqrt ( x(1)**2 + x(2)**2 + x(3)**2 )
      w = sqrt ( (x(2)*xp(3)-x(3)*xp(2))**2 + (x(3)*xp(1)-x(1)*xp(3))**2 + (x(1)*xp(2)-x(2)*xp(1))**2 )

      lvlh(1,1) = (xp(1)*(x(2)**2+x(3)**2)-x(1)*(x(2)*xp(2)+x(3)*xp(3))) / (w*r)
      lvlh(1,2) = (xp(2)*(x(1)**2+x(3)**2)-x(2)*(x(1)*xp(1)+x(3)*xp(3))) / (w*r)
      lvlh(1,3) = (xp(3)*(x(2)**2+x(1)**2)-x(3)*(x(2)*xp(2)+x(1)*xp(1))) / (w*r)
      lvlh(2,1) = -(x(2)*xp(3)-x(3)*xp(2)) / w
      lvlh(2,2) = -(x(3)*xp(1)-x(1)*xp(3)) / w
      lvlh(2,3) = -(x(1)*xp(2)-x(2)*xp(1)) / w
      lvlh(3,1) = -x(1)/r
      lvlh(3,2) = -x(2)/r
      lvlh(3,3) = -x(3)/r

      ! Composante de la direction du soleil dans le plan tangent:
      usb = lvlh(1,1)*us + lvlh(1,2)*vs + lvlh(1,3)*ws
      vsb = lvlh(2,1)*us + lvlh(2,2)*vs + lvlh(2,3)*ws
      n_uv = sqrt(usb*usb + vsb*vsb)
      usb = usb/n_uv
      vsb = vsb/n_uv

      ys(1,1) =  lvlh(1,1)*usb + lvlh(2,1)*vsb 
      ys(1,2) =  lvlh(1,2)*usb + lvlh(2,2)*vsb
      ys(1,3) =  lvlh(1,3)*usb + lvlh(2,3)*vsb
      ys(2,1) = -lvlh(1,1)*vsb + lvlh(2,1)*usb 
      ys(2,2) = -lvlh(1,2)*vsb + lvlh(2,2)*usb
      ys(2,3) = -lvlh(1,3)*vsb + lvlh(2,3)*usb
      ys(3,1:3) = lvlh(3,1:3)

!   * Calcul de la matrice de passage du RPS au repère avion :

      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatys" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatys" )) return
      end if


!   * Calcul de la matrice de passage de Planeto au repere avion :

      do i=1,3
         do j=1,3
            pgamav(i,j) = pterav(i,1)*ys(1,j) + pterav(i,2)*ys(2,j) + pterav(i,3)*ys(3,j)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :

     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatys" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psatys" )) return
     end if

      call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatys" )) return
      end if

      end subroutine psatys

      subroutine psattnw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psattnw
!
!$Resume
!  Sous-programme donnant les angles d'attitude "TNW" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère local TNW.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psattnw (x,xp,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    real (KIND=pm_reel), dimension(3) :: x,xp
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     x       :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m) 
!>E     xp      :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère TNW [rad] 
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère TNW [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère TNW [rad]  
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
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

      real (KIND=pm_reel), dimension(3), intent(IN)  :: x,xp
      real (KIND=pm_reel), intent(IN)  :: angle1,angle2,angle3
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      integer :: i,j
      real (KIND=pm_reel) :: w,v
      real (KIND=pm_reel), dimension(3,3) :: tnw,pterav

      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur

!   * Calcul de la matrice de passage de Planeto a TNW :

      w = sqrt ( (x(2)*xp(3)-x(3)*xp(2))**2 + (x(3)*xp(1)-x(1)*xp(3))**2 + (x(1)*xp(2)-x(2)*xp(1))**2 )
      v = sqrt ( xp(1)**2+xp(2)**2+xp(3)**2 )

      tnw(1,1)=xp(1)/v
      tnw(1,2)=xp(2)/v
      tnw(1,3)=xp(3)/v
      tnw(2,1)=(-x(1)*(xp(2)**2+xp(3)**2)+xp(1)*(x(2)*xp(2)+x(3)*xp(3))) / (w*v)
      tnw(2,2)=(-x(2)*(xp(1)**2+xp(3)**2)+xp(2)*(x(1)*xp(1)+x(3)*xp(3))) / (w*v)
      tnw(2,3)=(-x(3)*(xp(2)**2+xp(1)**2)+xp(3)*(x(2)*xp(2)+x(1)*xp(1))) / (w*v)
      tnw(3,1)= (x(2)*xp(3)-x(3)*xp(2)) / w
      tnw(3,2)=-(x(1)*xp(3)-x(3)*xp(1)) / w
      tnw(3,3)= (x(1)*xp(2)-x(2)*xp(1)) / w

!   * Calcul de la matrice de passage de TNW au repere avion :


      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psattnw" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psattnw" )) return
      end if


!   * Calcul de la matrice de passage de Planeto au repere avion :

      do i=1,3
         do j=1,3
            pgamav(i,j) = pterav(i,1)*tnw(1,j) + pterav(i,2)*tnw(2,j) + pterav(i,3)*tnw(3,j)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :

     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psattnw" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psattnw" )) return
     end if

      end subroutine psattnw

      subroutine psattop (x,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psattop
!
!$Resume
!  Sous-programme donnant les angles d'attitude "topocentrique local" dans le repère d'intégration.
!
!$Description
!  Sous-programme donnant les angles d'attitude dans le repère d'intégration
!  alors qu'ils sont définis dans le repère local "topocentrique local".
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psattop (x,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel), dimension(3) :: x
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     x       :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m) 
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère "topocentrique local" [rad] 
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère "topocentrique local" [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère "topocentrique local" [rad]  
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad] 
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   
!matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- mt_topo_N_ref
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_mat_quat
!- mu_quat_3rot
!
!$Include
!
!$Module
!#V
!- ps_calcul_forces
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_calcul_forces

      implicit none

      real (KIND=pm_reel), intent(IN) ::angle1,angle2,angle3
      real (KIND=pm_reel), dimension(3), intent(IN) :: x
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      integer :: i,j
      real (KIND=pm_reel) :: rayv,sinlat,xlat,xlon
      real (KIND=pm_reel), dimension(3,3) :: top, pos_ref, pterav
      real (KIND=pm_reel), dimension(6,6) :: jacob

      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur
      type (tm_geodesique) :: orig_topo

!   * Calcul de la latitude/longitude / Planeto:

      rayv = sqrt( x(1)**2 + x(2)**2 + x(3)**2 )

      sinlat = x(3) / rayv
      xlat = asin(sinlat)

      xlon = atan2 (x(2), x(1))

!   * Calcul de la matrice de passage du repère TOPO à Planeto:

      orig_topo%lat = -xlat
      orig_topo%long = xlon + pi
!     valeur arbitraire, non utilisee pour les resultats que l'on cherche

      orig_topo%haut = str_mod(iveh)%requa  


      call mt_topo_N_ref (orig_topo, str_mod(iveh)%requa, 0._pm_reel, &
		         (/0._pm_reel, 0._pm_reel, 0._pm_reel/),  pos_ref, &
			 code_erreur, jacob=jacob)

      top(1:3, 1:3) = jacob (1:3, 1:3)
 

!   * Calcul de la matrice de passage du TOPO au repère avion :


      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
                         quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psattop" )) return
      end if

      call mu_quat_mat (quaternion, pterav, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psattop" )) return
      end if

!   * Calcul de la matrice de passage de Planeto au repere avion :
!     Note (YTY, 10/02/06) : code equivalent à pgamav = matmul(pterav,transpose(top))

      do i=1,3
         do j=1,3
            pgamav(i,j) = pterav(i,1)*top(j,1) + pterav(i,2)*top(j,2) + pterav(i,3)*top(j,3)
         enddo
      enddo

!   * Calcul de l'attitude dans Planeto :

     call mu_mat_quat (pgamav, quaternion, code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psattop" )) return
     end if

     call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
	                 code_erreur)
     if (code_erreur%valeur < 0) then
        call MSP_signaler_message (ier_mslib=code_erreur)
        if (MSP_gen_messages("psattop" )) return
     end if


      end subroutine psattop



      subroutine psatine (date,x,xp,repere,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psatine
!
!$Resume
!   Sous-programme qui exprime dans le repère d'intégration les angles
!   d'attitude définis dans un repère inertiel.
!
!$Description
!   Sous-programme qui exprime dans le repère d'intégration les angles
!   d'attitude définis dans un repère inertiel (G50, GVrai, J2000).
!
!$Auteur
!  Atos Origin
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psatine (date,x,xp,repere,angle1,angle2,angle3,angw1,angw2,angw3,pgamav)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel), dimension(3) :: x,xp
!.    integer :: repere
!.    real (KIND=pm_reel) :: angle1,angle2,angle3
!.    real (KIND=pm_reel) :: angw1,angw2,angw3
!.    real (KIND=pm_reel), dimension(3,3) :: pgamav
!
!$Arguments
!>E     date    :<tm_jour_sec>         date courante (Jours Juliens CNES)
!>E     x       :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m)
!>E     xp      :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>E     repere  :<integer>             type de repère inertiel de définition de l'attitude
!>E     angle1  :<pm_reel>             angle de lacet (psi) dans le repère inertiel [rad]
!>E     angle2  :<pm_reel>             angle de tangage (teta) dans le repère inertiel [rad] 
!>E     angle3  :<pm_reel>             angle de roulis (phi) dans le repère inertiel [rad]
!>S     angw1   :<pm_reel>             angle de lacet (psi) dans le repère d'intégration [rad]
!>S     angw2   :<pm_reel>             angle de tangage (teta) dans le repère d'intégration [rad]
!>S     angw3   :<pm_reel>             angle de roulis (phi) dans le repère d'intégration [rad]
!>S     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!
!$Common
!
!$Routines
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_mat
!- mu_matmul3
!- mu_mat_quat
!- mu_quat_3rot
!#V
!- psi_Mat_RI_Ratt_terre
!- psi_Mat_RI_Ratt_mars_venus
!#
!
!$Include
!
!$Module
!#V
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

      use ps_integration_don
      use ps_bulletin

      implicit none

      type(tm_jour_sec),   intent(IN)  :: date
      real (KIND=pm_reel), dimension(3), intent(IN)  :: x,xp
      integer, intent(IN) :: repere
      real (KIND=pm_reel), intent(IN)  :: angle1,angle2,angle3
      real (KIND=pm_reel), intent(OUT) :: angw1,angw2,angw3
      real (KIND=pm_reel), dimension(3,3), intent(OUT) :: pgamav

      real (KIND=pm_reel), dimension(3,3) :: Mat_RI_RVeh, Mat_Ratt_RVeh
      real (KIND=pm_reel), dimension(6,6) :: Mat_RI_Ratt
      type (tm_quat) :: quaternion
      type (tm_code_retour) :: code_erreur
      type (tm_code_retour) :: code_retour_local


      !!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calcul de Mat_RI_Ratt !
      !!!!!!!!!!!!!!!!!!!!!!!!!
      
      ! Calcul de la matrice de passage Mat_RI_Ratt (Ratt = repère de définition de l'attitude)
      if (str_gen(iveh)%planet == eph_terre) then
	 ! Calcul dans le cas Terre
         call psi_Mat_RI_Ratt_terre(date, x, xp, repere, Mat_RI_Ratt) 
	 if ( MSP_gen_messages ("psatine") ) return
      else
         ! Calcul dans le cas Mars ou Vénus
         call psi_Mat_RI_Ratt_mars_venus(date, repere, Mat_RI_Ratt)
	 if ( MSP_gen_messages ("psatine") ) return
      end if
     

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Calcul de l'attitude dans RI !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      ! Conversion des angles de rotation -> quaternion
      call mu_3rot_quat (pm_1z_2y_3x, angle1, angle2, angle3, &
        		quaternion, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatine" )) return
      end if		    

      ! Conversion quaternion -> matrice de rotation
      ! La matrice Mat_Ratt_RVeh permet de passer du repère inertiel de définition de la loi d'attitude
      ! (Ratt) au repère Véhicule (Rveh)
      call mu_quat_mat (quaternion, Mat_Ratt_RVeh, code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatine" )) return
      end if

      ! Multiplication par la matrice RI->Repère inertiel de définition de l'attitude
      ! pour obtenir la matrice de passage du repère RI vers le repère Véhicule
      call mu_matmul3(Mat_Ratt_RVeh,Mat_RI_Ratt(1:3,1:3),Mat_RI_RVeh,code_retour_local)       
    
      call mu_mat_quat (Mat_RI_RVeh, quaternion, code_erreur)
	
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatine" )) return
      end if

      call mu_quat_3rot (pm_1z_2y_3x, quaternion, angw1, angw2, angw3, &
        		 code_erreur)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if (MSP_gen_messages("psatine" )) return
      end if

      ! Initialisation de la sortie pgamav
      pgamav(:,:) = Mat_RI_RVeh
    
      end subroutine psatine



      subroutine pslecatt(ficati,ati)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pslecatt
!
!$Resume
!  Sous-programme de lecture du fichier d'attitude et de mise à jour
!  du scenario MECASPA avec la loi d'attitude tabulee lue
!
!$Description
!  Sous-programme de lecture du fichier d'attitude et de mise à jour
!  du scenario MECASPA avec la loi d'attitude tabulee lue
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pslecatt(ficati,ati)
!.    character(LEN=80) :: ficati
!.    type(MSP_SCENARIO_LOI) :: ati
!
!$Arguments
!>E     ficati  :<LEN=80>             
!>E/S   ati     :<MSP_SCENARIO_LOI>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_consulter_scenario
!- MSP_ajouter_attitude_tabulee
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_bulletin
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites
      use ps_bulletin

      implicit none

      character(LEN=80), intent(IN)  :: ficati
      type(MSP_SCENARIO_LOI), intent(INOUT) :: ati

      real (KIND=pm_reel), dimension(:), pointer :: datt
      real (KIND=pm_reel), dimension(:), pointer :: ang1
      real (KIND=pm_reel), dimension(:), pointer :: ang2
      real (KIND=pm_reel), dimension(:), pointer :: ang3
      integer, dimension(:), pointer :: jjdatt
      real (KIND=pm_reel), dimension(:), pointer :: secdatt
      integer :: irepa
      integer :: jjdattori
      real (KIND=pm_reel) :: secdattori, datt1

      integer :: ier,longueur,acces
      integer :: ipts_att=0, i=0
      integer :: npts_att

      character(LEN=256) :: rctmp
      integer :: itypdat
      real (KIND=pm_reel) :: date_ref

      ! Récupération des données incluses dans les fichiers de ressource
            
      ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",&
              partie_variable="data_psimu",routine="pslecatt")
         return
      else
         dirdat = rctmp(1:longueur)
      endif

      ! Ouverture du fichier d'attitude
      acces = acc_open()
      ier = acc_connect (acces, dirdat(1:LEN_TRIM(dirdat))//"/"//ficati(1:LEN_TRIM(ficati)), ACC_R)

      ier = acc_read(acces, ACC_HEADER)

      ! Lecture du type de repere
         ier = acc_geti (acces,"typrep",irepa)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du type de repere', &
                 routine="pslecatt",type=MSP_ENUM_ERREUR)
            return
         endif

       ! Lecture du type de date
         ier = acc_geti (acces,"typdatatt",itypdat)
         if ( ier /= 0 ) then
            call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du type de date', &
                 routine="pslecatt",type=MSP_ENUM_ERREUR)
            return
         endif

      ! Lecture de JJ date dorigine
      ier = acc_geti (acces,"jjdattori",jjdattori)
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de date origine', &
              routine="pslecatt",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture de sec date origine
      ier = acc_getd (acces,"secdattori",secdattori,"s")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de date origine', &
              routine="pslecatt",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture du fichier pour connaitre le nombre de lignes
       do while(acc_read(acces,ACC_NEXT) .ne. ACC_EOF)
         ipts_att = ipts_att+ 1
      enddo

      npts_att = ipts_att

      ! allocation des tableaux de donnees
      allocate(datt(npts_att))
      allocate(ang1(npts_att))
      allocate(ang2(npts_att))
      allocate(ang3(npts_att))
      allocate(jjdatt(npts_att))
      allocate(secdatt(npts_att))

      ! Retour en debut de fichier
      ier = acc_deconnect(acces, ACC_R)
      ier = acc_close(acces)

      acces = acc_open()
      ier = acc_connect (acces, dirdat(1:LEN_TRIM(dirdat))//"/"//ficati(1:LEN_TRIM(ficati)), ACC_R)

      ier = acc_read(acces, ACC_HEADER)

     ! Lecture des colonnes jjdatt/secdatt/ang1/ang2/ang3 pour typdat=1
     ! Lecture des colonnes datt/ang1/ang2/ang3 pour typdat=2 ou 3

      do while(acc_read(acces,ACC_NEXT) .ne. ACC_EOF)
         i = i+ 1
         if (itypdat .eq. 1) then !date absolue
            ier = acc_geti(acces,"jjdatt",jjdatt(i))
            ier = acc_getd(acces,"secdatt",secdatt(i) , "s")
         else
            ier = acc_getd(acces,"datt",datt(i) , "s")
         endif
         ier = acc_getd(acces,"ang1",ang1(i) , "rad")
         ier = acc_getd(acces,"ang2",ang2(i) , "rad")
         ier = acc_getd(acces,"ang3",ang3(i) , "rad")
      enddo

      ! recuperation de la date de reference du scenario
      call MSP_consulter_scenario (ati, date_ref=date_ref)
      if ( MSP_gen_messages("pslecpro") ) return

      ! test du type de date
      if (itypdat.eq.1) then  !absolu
         datt(1:npts_att) = (jjdatt(1:npts_att)-date_ref)*86400._pm_reel &
              +secdatt(1:npts_att)
         
      else if (itypdat.eq.3) then !absolu relatif
         datt1 = (jjdattori-date_ref)*86400._pm_reel&
              + secdattori
         
         datt(2:npts_att) = datt1 + datt(2:(npts_att-1))     
         datt(1) = datt1

      end if
 
!/  Ajout d'une loi de propulsion sur fichier dans la structure MSP_SCENARIO_LOI
      call MSP_ajouter_attitude_tabulee(ati,"",ntab=npts_att,&
           typdat=MSP_ENUM_LOI_RELATIVE, &
           dates=datt(1:npts_att), typrep=irepa, & 
           psi=ang1(1:npts_att),teta=ang2(1:npts_att),phi=ang3(1:npts_att) )
      if ( MSP_gen_messages("pslecatt") ) return

      deallocate(datt)
      deallocate(ang1)
      deallocate(ang2)
      deallocate(ang3)
      deallocate(jjdatt)
      deallocate(secdatt)

      ier = acc_deconnect(acces, ACC_R)
      ier = acc_close(acces)

      end subroutine pslecatt

      subroutine ps_convertir_angles(convention,psi,theta,phi)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_convertir_angles
!
!$Resume
!  Conversion d'angles d'une convention d'angles donnée vers les angles de 
!  Cardan Z Y X
!
!$Description
!  Conversion d'angles d'une convention d'angles donnée vers les angles de 
!  Cardan Z Y X
!
!$Auteur
!  Y. TANGUY
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_convertir_angles(convention,psi,theta,phi)
!.    real(kind=pm_reel) :: psi, theta, phi
!.    integer :: convention
!
!$Arguments
!>E     convention  :<integer>   convention d'angles de la MSLIB90 (cf thème U)
!>E/S   psi         :<pm_reel>   
!>E/S   theta       :<pm_reel>   
!>E/S   phi         :<pm_reel>   
!
!$Common
!
!$Routines
!- mu_3rot_quat
!- MSP_signaler_message
!- mu_quat_3rot
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
        real(kind=pm_reel), intent(inout) :: psi, theta, phi
        integer, intent(in) :: convention

        ! Variables locales
        !==================
        type(tm_code_retour) :: code_retour
        type(tm_quat) :: quat
        
        ! Début du code
        !==============
        
        if (convention /= pm_1z_2y_3x) then
           ! On n'effectue pas de conversion si les angles sont déjà 
           ! exprimés comme des angles de Cardan selon Z, Y, X

           ! Conversion triplet d'angles vers quaternion, en utilisant la convention 
           ! saisie par l'utilisateur
           call mu_3rot_quat(convention,psi,theta,phi,quat,code_retour)
           if (code_retour%valeur < 0) then
              call MSP_signaler_message (cle_mes="PS_INIT_TYPE_ANGLE")
              return
           endif

           ! Conversion quaternion vers angles de cardan "Z Y X" (mécanique de vol)
           call mu_quat_3rot(pm_1z_2y_3x,quat,psi,theta,phi,code_retour)
           if (code_retour%valeur < 0) then
              call MSP_signaler_message (cle_mes="PS_INIT_TYPE_ANGLE")
              return
           endif

        end if

      end subroutine ps_convertir_angles


      subroutine ps_recupere_date_loi_att(scenario,ii,date_deb,date_fin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_recupere_date_loi_att
!
!$Resume
!  Routine simplifiée de récupération des dates de début et fin d'une loi d'attitude
!  n°ii dans un scénario de lois d'attitude
!
!$Description
!  Routine simplifiée de récupération des dates de début et fin d'une loi d'attitude
!  n°ii dans un scénario de lois d'attitude
!
!$Auteur
!  Y.TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_recupere_date_loi_att(scenario,ii,date_deb,date_fin)
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    integer :: ii
!.    real(kind=pm_reel) :: date_deb
!.    real(kind=pm_reel) :: date_fin
!
!$Arguments
!>E/S   scenario  :<MSP_SCENARIO_LOI>   scenario de lois d'attitude
!>E     ii        :<integer>            indice de la loi
!>S     date_deb  :<pm_reel>            date de début
!>S     date_fin  :<pm_reel>            date de fin
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_consulter_attitude_tabulee
!- MSP_signaler_message
!- MSP_consulter_attitude_spinnee
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
        type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
        integer, intent(in)                   :: ii
        real(kind=pm_reel),intent(out)        :: date_deb
        real(kind=pm_reel),intent(out)        :: date_fin

        ! Variables locales
        !==================
        integer :: typloi,ntab,stat
        type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin
        type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab
        real(kind=pm_reel), dimension(:), pointer :: dates


        ! Début du code
        !==============

        nullify(dates)

        ! Type de loi
        typloi = MSP_type_loi (scenario, id=ii)
        
        ! Selon le type de loin, on extrait les dates différemment
        if (typloi == MSP_ENUM_LOI_ATTI_TAB) then
           !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
           call MSP_consulter_scenario (scenario, loi_attit_tab=loi_atti_tab, id=ii)
           if ( MSP_gen_messages("ps_recupere_date_loi_att") ) return

           call MSP_consulter_attitude_tabulee(loi_atti_tab,ntab=ntab,dates=dates)
           if ( MSP_gen_messages("ps_recupere_date_loi_att") ) return

           date_deb=dates(1)
           date_fin=dates(ntab)

           ! Désallocation mémoire.
           if(associated(dates)) then
              deallocate(dates, stat=stat)
              if (stat < 0) then
                 call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                      partie_variable="ps_recupere_date_loi_att")
                 return
              end if
           end if

        else if (typloi == MSP_ENUM_LOI_ATTI_SPIN) then

           !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
           call MSP_consulter_scenario (scenario, loi_attit_spin=loi_atti_spin, id=ii)
           if ( MSP_gen_messages("ps_initialiser_attitude") ) return

           call MSP_consulter_attitude_spinnee(loi_atti_spin,datedeb=date_deb,datefin=date_fin)
           if ( MSP_gen_messages("ps_initialiser_attitude") ) return

        end if
	call MSP_effacer_attitude_tabulee(loi_atti_tab)
      end subroutine ps_recupere_date_loi_att

      subroutine ps_compare_loi_att(scenario_a,indice_loi_a,scenario_b,indice_loi_b,lois_identiques)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_compare_loi_att
!
!$Resume
!  Routine de comparaison de deux lois n°incide_loi_a et indice_loi_b dans deux scenarios scenario_a et
!  scenario_b. Rend une valeur "lois_identiques" à .true. si les lois sont identiques, à .false. sinon
!
!$Description
!  Routine de comparaison de deux lois n°incide_loi_a et indice_loi_b dans deux scenarios scenario_a et
!  scenario_b. Rend une valeur "lois_identiques" à .true. si les lois sont identiques, à .false. sinon
!
!$Auteur
!  Y.TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_compare_loi_att(scenario_a,indice_loi_a,scenario_b,indice_loi_b,lois_identiques)
!.    type(MSP_SCENARIO_LOI) :: scenario_a
!.    integer :: indice_loi_a
!.    type(MSP_SCENARIO_LOI) :: scenario_b
!.    integer :: indice_loi_b
!.    logical :: lois_identiques
!
!$Arguments
!>E/S   scenario_a       :<MSP_SCENARIO_LOI>   Scénario A
!>E     indice_loi_a     :<integer>            Indice A de la loi dans le scénario A
!>E/S   scenario_b       :<MSP_SCENARIO_LOI>   Scénario B
!>E     indice_loi_b     :<integer>            Indice B de la loi dans le scénario B
!>S     lois_identiques  :<logical>            vrai si les lois sont identiques
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_consulter_attitude_spinnee
!- MSP_consulter_attitude_tabulee
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
        type(MSP_SCENARIO_LOI),intent(inout) :: scenario_a
        integer, intent(in)           :: indice_loi_a
        type(MSP_SCENARIO_LOI),intent(inout) :: scenario_b
        integer, intent(in)           :: indice_loi_b
        logical, intent(out)          :: lois_identiques

        ! Variables locales
        !==================
        type(MSP_ATTITUDE_TABULEE) :: loi_atti_tab_a, loi_atti_tab_b
        type(MSP_ATTITUDE_SPINNEE) :: loi_atti_spin_a, loi_atti_spin_b
        integer :: typloi_a,typloi_b, stat, kk
        
        real(kind=pm_reel) :: t_deb_a, t_fin_a, psi0_a, teta0_a, phi0_a
        real(kind=pm_reel) :: psip_a, tetap_a, phip_a
        integer :: convention_a, ntab_a, repere_a
        real(kind=pm_reel), dimension(:), pointer :: dates_relatives_a
        real(kind=pm_reel), dimension(:), pointer :: psi_a
        real(kind=pm_reel), dimension(:), pointer :: teta_a
        real(kind=pm_reel), dimension(:), pointer :: phi_a

        real(kind=pm_reel) :: t_deb_b, t_fin_b, psi0_b, teta0_b, phi0_b
        real(kind=pm_reel) :: psip_b, tetap_b, phip_b
        integer :: convention_b, ntab_b, repere_b
        real(kind=pm_reel), dimension(:), pointer :: dates_relatives_b
        real(kind=pm_reel), dimension(:), pointer :: psi_b
        real(kind=pm_reel), dimension(:), pointer :: teta_b
        real(kind=pm_reel), dimension(:), pointer :: phi_b

        ! Début du code
        !==============

        nullify(dates_relatives_a)
        nullify(psi_a)
        nullify(teta_a)
        nullify(phi_a)
        nullify(dates_relatives_b)
        nullify(psi_b)
        nullify(teta_b)
        nullify(phi_b)
        lois_identiques = .false.

        ! Récupération des types de lois
        typloi_a = MSP_type_loi (scenario_a, id=indice_loi_a)
        typloi_b = MSP_type_loi (scenario_b, id=indice_loi_b)

        if (typloi_a == typloi_b) then
           ! Si les lois sont de types différents, elles seront de toutes façons différentes

           if (typloi_a == MSP_ENUM_LOI_ATTI_SPIN) then
              ! Cas de lois spinnées

              call MSP_consulter_scenario (scenario_a, loi_attit_spin=loi_atti_spin_a, id=indice_loi_a)
              if (MSP_gen_messages("ps_compare_loi_att")) return
              
              call MSP_consulter_attitude_spinnee (loi_atti_spin_a,&
                   datedeb=t_deb_a, datefin=t_fin_a, typrep=repere_a, &
                   psi0=psi0_a, teta0=teta0_a, phi0=phi0_a, &
                   psip=psip_a, tetap=tetap_a, phip=phip_a, typangle=convention_a)

              call MSP_consulter_scenario (scenario_b, loi_attit_spin=loi_atti_spin_b, id=indice_loi_b)
              if (MSP_gen_messages("ps_compare_loi_att")) return

              call MSP_consulter_attitude_spinnee (loi_atti_spin_b,&
                   datedeb=t_deb_b, datefin=t_fin_b, typrep=repere_b, &
                   psi0=psi0_b, teta0=teta0_b, phi0=phi0_b, &
                   psip=psip_b, tetap=tetap_b, phip=phip_b, typangle=convention_b)
              
              ! Comparaison de tous les champs
	      if ( (abs(t_deb_a - t_deb_b) < MSP_EPSILON_DATE) .and. (abs(t_fin_a - t_fin_b) < MSP_EPSILON_DATE) .and. &
	           (repere_a == repere_b) .and. (abs(psi0_a - psi0_b) < MSP_EPSILON_APLA) .and. &
	           (abs(teta0_a - teta0_b) < MSP_EPSILON_APLA) .and. (abs(phi0_a - phi0_b) < MSP_EPSILON_APLA) .and. &
		   (abs(psip_a - psip_b) < MSP_EPSILON_APLA) .and. (abs(tetap_a - tetap_b) < MSP_EPSILON_APLA) .and. &
		   (abs(phip_a - phip_b) < MSP_EPSILON_APLA) .and. (convention_a == convention_b) ) then
                 lois_identiques = .true.
              end if
              
           else 
              ! Cas de lois tabulées

              call MSP_consulter_scenario (scenario_a, loi_attit_tab=loi_atti_tab_a, id=indice_loi_a)
              if (MSP_gen_messages("ps_compare_loi_att")) return

              call MSP_consulter_attitude_tabulee (loi_atti_tab_a,&
                   ntab=ntab_a, typrep=repere_a, dates=dates_relatives_a, &
                   psi=psi_a, teta=teta_a, phi=phi_a, typangle=convention_a)
                if (MSP_gen_messages("ps_compare_loi_att")) return

              call MSP_consulter_scenario (scenario_b, loi_attit_tab=loi_atti_tab_b, id=indice_loi_b)
              if (MSP_gen_messages("ps_compare_loi_att")) return

              call MSP_consulter_attitude_tabulee (loi_atti_tab_b,&
                   ntab=ntab_b, typrep=repere_b, dates=dates_relatives_b, &
                   psi=psi_b, teta=teta_b, phi=phi_b, typangle=convention_b)
              if (MSP_gen_messages("ps_compare_loi_att")) return

              if (ntab_a == ntab_b .and. repere_a == repere_b .and. &
                   convention_a == convention_b) then
                 lois_identiques = .true.

                 ! boucle sur les dates de la tabulations
                 do kk=1,ntab_a
                    if (.not. (abs(dates_relatives_a(kk) - dates_relatives_b(kk)) < MSP_EPSILON_DATE .and. &
                         abs(psi_a(kk) - psi_b(kk)) < MSP_EPSILON_APLA .and. abs(teta_a(kk) - teta_b(kk)) < MSP_EPSILON_APLA .and. &
                         abs(phi_a(kk) - phi_b(kk)) < MSP_EPSILON_APLA ) ) then
                       lois_identiques = .false.
                    end if
                 end do
              end if

              ! Désallocation de tous les pointeurs
              if (associated(dates_relatives_a)) then
                 deallocate(dates_relatives_a,stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(dates_relatives_b)) then
                 deallocate(dates_relatives_b, stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(psi_a)) then
                 deallocate(psi_a,stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(psi_b)) then
                 deallocate(psi_b, stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(phi_a)) then
                 deallocate(phi_a,stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(phi_b)) then
                 deallocate(phi_b, stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(teta_a)) then
                 deallocate(teta_a,stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if
              if (associated(teta_b)) then
                 deallocate(teta_b, stat=stat)
                 if (stat < 0) then
                    call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",&
                         partie_variable="ps_compare_loi_att")
                    return
                 end if
              end if

           end if
           
        end if
	call MSP_effacer_attitude_tabulee(loi_atti_tab_a)
	call MSP_effacer_attitude_tabulee(loi_atti_tab_b)

      end subroutine ps_compare_loi_att

      
      subroutine psi_Mat_RI_Ratt_terre(date,x,xp,repere,Mat_RI_Ratt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_Mat_RI_Ratt_terre
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
!  call psi_Mat_RI_Ratt_terre(date,x,xp,repere,Mat_RI_Ratt)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel), dimension(3) :: x,xp
!.    integer :: repere
!.    real (KIND=pm_reel), dimension(6,6) :: Mat_RI_Ratt
!
!$Arguments
!>E     date         :<tm_jour_sec>         
!>E     x            :<pm_reel,DIM=(3)>     
!>E     xp           :<pm_reel,DIM=(3)>     
!>E     repere       :<integer>             
!>S     Mat_RI_Ratt  :<pm_reel,DIM=(6,6)>   
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_date_etutc
!- MSP_signaler_message
!- MSP_conv_typrep
!
!$Include
!
!$Module
!#V
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


      use ps_integration_don
      use ps_bulletin

      implicit none

      type(tm_jour_sec),   intent(IN)  :: date
      real (KIND=pm_reel), dimension(3), intent(IN)  :: x,xp
      integer, intent(IN) :: repere
      real (KIND=pm_reel), dimension(6,6), intent(OUT) :: Mat_RI_Ratt

      type(tm_jour_sec) :: date_tuc, date_rep_in
      real (KIND=pm_reel), dimension(6) :: par, pvout
      real (kind=pm_reel) :: apla_rep
      real (KIND=pm_reel), dimension(10) :: rep_g50, rep_gVrai
      type (tm_pole_uv) :: pole
      type (tm_code_retour) :: code_erreur
      integer :: ris
      integer :: i,j



      ! Repère inertiel de sortie
      ! ris = 0 si Gamma 50 CNES
      ! ris = 1 si J2000
      ! ris = 2 si Gamma vrai de la date
      ris = PS_MODOUT - 1

      ! Position et vitesse dans le RI
      par(1:3) = x(1:3)
      par(4:6) = xp(1:3)

      ! Date du repère d'entrée
      call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
      date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc
      
      ! Test de la valeur de apla_r avant passage de 1/apla_r
      if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
         apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
      else
         apla_rep = 0._PM_REEL
      endif

      ! Conversion de la date TE en date TUC
      call MSP_date_etutc(date,1,date_tuc)
      if ( MSP_ERREUR) then
         call MSP_signaler_message (cle_mes="MSP_MSPRO",&
            partie_variable="MSP_date_etutc_mspro", &
            routine="psatine",type=MSP_ENUM_WARNING)
      end if

      ! Coordonnées de la station
      rep_gVrai(:) = 0._pm_reel
      rep_g50(:) = 0._pm_reel
      
      ! Initialisation de la variable pole
      pole%u = str_bul(iveh)%polu
      pole%v = str_bul(iveh)%polv


      
      ! Calcul de la matrice de passage Mat_RI_Ratt (Ratt = repère de définition de l'attitude)      
      if (repere == ris) then
      ! Les matrices de passage à la date courante sont déjà calculées par ps_calculer_pvout
      ! et stockées dans str_int(iveh)%Mat_RI_ROUT

         do j=1,6
            do i=1,6
               Mat_RI_Ratt(i,j) = str_int(iveh)%Mat_RI_ROUT(i,j)
            enddo
         enddo

      else if (repere == MSP_ENUM_ATTI_INERTIEL_G50) then
         ! Calcul de la matrice de passage du RI au Gamma 50 CNES
         
         call MSP_conv_typrep (par,&
               int(str_int(iveh)%reps(1)),str_int(iveh)%echds,str_int(iveh)%num_plas, &
               date_rep_in, MSP_ENUM_ECHT_TUC, &
               str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               pm_veis, MSP_ENUM_ECHD_DATE_REF, eph_terre,&
               date_tuc, MSP_ENUM_ECHT_TUC, &
               0._pm_reel, rep_g50(2:10), &
               str_bul(iveh)%requa_r, apla_rep, &
               str_bul(iveh)%vitrot, &
               pvout, mat_jacob=Mat_RI_Ratt, obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)
      
      else if (repere == MSP_ENUM_ATTI_INERTIEL_GVrai) then
         ! Calcul de la matrice de passage du RI au Gamma vrai de la date

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
               pvout, mat_jacob=Mat_RI_Ratt, obli=str_bul(iveh)%obli, &
               pole_in=pole, pole_out=pole)
	       
      else if (repere == MSP_ENUM_ATTI_INERTIEL_J2000) then
         ! Calcul de la matrice de passage du RI au EME2000
	 ! La matrice de passage est constante, elle est calculée à l'initialisation
	 ! et stockée dans str_int(iveh)%Mat_RI_ME2000
         do j=1,6
            do i=1,6
               Mat_RI_Ratt(i,j) = str_int(iveh)%Mat_RI_ME2000(i,j)
            enddo
         enddo

      end if

      
      
      end subroutine psi_Mat_RI_Ratt_terre


      subroutine psi_Mat_RI_Ratt_mars_venus(date,repere,Mat_RI_Ratt)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_Mat_RI_Ratt_mars_venus
!
!$Resume
!    Sous-programme qui calcule la matrice de passage du RI au repère
!    de définition de l'attitude (cas repère inertiel) pour le mode
!    Mars ou Vénus
!
!$Description
!    Sous-programme qui calcule la matrice de passage du RI au repère
!    de définition de l'attitude (cas repère inertiel) pour le mode
!    Mars ou Vénus
!
!$Auteur
!  Atos Origin
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_Mat_RI_Ratt_mars_venus(date,repere,Mat_RI_Ratt)
!.    type(tm_jour_sec) :: date
!.    integer :: repere
!.    real (KIND=pm_reel), dimension(6,6) :: Mat_RI_Ratt
!
!$Arguments
!>E     date         :<tm_jour_sec>         date courante (Jours Juliens CNES)      
!>E     repere       :<integer>             type de repère inertiel de définition de l'attitude 
!>S     Mat_RI_Ratt  :<pm_reel,DIM=(6,6)>   matrice de passage du repère d'intégration
!                                           au repère de définition de l'attitude
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!- MSP_conv_typrep
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!- ps_bulletin
!#
!
!$Remarques
!
!  Dans le cas où repère = MSP_ENUM_ATTI_INERTIEL_GVrai, on considère
!    qu'il s'agit du repère équatorial planétaire UAI
!  Dans le cas où repère = MSP_ENUM_ATTI_INERTIEL_J2000, on considère
!    qu'il s'agit du repère équatorial moyen 2000
!  Dans le cas où repère = MSP_ENUM_ATTI_INERTIEL_G50, on sort en erreur
!    car ce repère n'est pas compatible avec les modes Mars ou Vénus
!
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      use ps_integration_don
      use ps_bulletin

      implicit none

      type(tm_jour_sec),   intent(IN)  :: date
      integer, intent(IN) :: repere
      real (KIND=pm_reel), dimension(6,6), intent(OUT) :: Mat_RI_Ratt

      type(tm_jour_sec) :: date_rep_in
      type (tm_jour_sec) :: date_J2000
      real (KIND=pm_reel), dimension(6) :: par, pvout
      real (kind=pm_reel) :: apla_rep
      real (KIND=pm_reel), dimension(10) :: repPQ
      type (tm_code_retour) :: code_erreur
      type (tm_pole_uv) :: pole
      integer :: ris
      integer :: i,j

      ris = 0
      ! Repère inertiel de sortie
      if (PS_MODOUT == 1) then
         ! Equatorial planétaire UAI
         ris = 2	 
      else if (PS_MODOUT == 2) then
         ! Equatorial moyen 2000
         ris = 1	 	 
      end if

      ! Position et vitesse dans le RI
      par(:) = 0._pm_reel
      par(1) = 1._pm_reel

      ! Date du repère d'entrée
      call md_jourfrac_joursec(str_int(iveh)%reps(4), date_rep_in, code_erreur)
      date_rep_in = date_rep_in + str_bul(iveh)%ecart_te_tuc

      ! Initialisation de la variable pole
      pole%u = str_bul(iveh)%polu
      pole%v = str_bul(iveh)%polv
      
      ! Test de la valeur de apla_r avant passage de 1/apla_r
      if (str_bul(iveh)%apla_r .different. 0._PM_REEL) then
         apla_rep = 1._PM_REEL / str_bul(iveh)%apla_r
      else
         apla_rep = 0._PM_REEL
      endif

      ! Coordonnées de la station
      repPQ(1:10) = 0._pm_reel
      

      if (repere == 0) then
         ! Si le repère de définition de l'attitude = Gamma 50 CNES
	 ! On sort en erreur car ce repère ne s'applique qu'à la Terre
         call MSP_signaler_message (cle_mes="PS_INIT_ATI_004")
         return
      else
      
         if (repere == ris) then
	     
            ! Les matrices de passage à la date courante sont déjà calculées par ps_calculer_pvout
            ! et stockées dans str_int(iveh)%Mat_RI_ROUT

            do j=1,6
               do i=1,6
                  Mat_RI_Ratt(i,j) = str_int(iveh)%Mat_RI_ROUT(i,j)
               enddo
            enddo
	 else if (repere == MSP_ENUM_ATTI_INERTIEL_GVrai) then
            ! Dans le cas Mars ou Vénus, si repère = MSP_ENUM_ATTI_INERTIEL_GVrai
	    ! c'est-à-dire Equatorial vrai de la date, alors on considère qu'il s'agit du repère
	    ! Equatorial planétaire UAI
	    ! Calcul de la matrice de passage du RI à l'équatorial planétaire UAI

            call MSP_conv_typrep (par,&
                 int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
                 date_rep_in , MSP_ENUM_ECHT_TUC, &
                 str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
                 str_bul(iveh)%requa_r, apla_rep, &
                 pm_equa_uai, pm_autre_date, str_gen(iveh)%planet,&
                 date + str_bul(iveh)%ecart_te_tuc, MSP_ENUM_ECHT_TUC,&
                 repPQ(3), repPQ(2:10), &
                 str_bul(iveh)%requa_r, apla_rep, &
                 str_bul(iveh)%vitrot, &
                 pvout, mat_jacob=Mat_RI_Ratt, obli=str_bul(iveh)%obli, &
                 pole_in=pole, pole_out=pole)

	 
	 else if (repere == MSP_ENUM_ATTI_INERTIEL_J2000) then
	 
	    ! Calcul de la matrice de passage du RI à l'équatorial moyen 2000
	    date_J2000%jour = 18262
            date_J2000%sec = 43200

            call MSP_conv_typrep (par,&
                 int(str_int(iveh)%reps(1)), str_int(iveh)%echds, str_int(iveh)%num_plas, &
                 date_rep_in , pm_TUC, &
                 str_int(iveh)%reps(3), str_int(iveh)%reps(2:10), &
                 str_bul(iveh)%requa_r, apla_rep, &
                 pm_equa_moy, pm_1janvier2000_12h00, eph_terre,&
                 date_J2000, pm_TE,&
                 0._pm_reel, repPQ(2:10), &
                 str_bul(iveh)%requa_r, apla_rep, &
                 str_bul(iveh)%vitrot, &
                 pvout, mat_jacob=Mat_RI_Ratt, obli=str_bul(iveh)%obli, &
                 pole_in=pole, pole_out=pole)

	 end if

      end if


      end subroutine psi_Mat_RI_Ratt_mars_venus



end module ps_attitude
