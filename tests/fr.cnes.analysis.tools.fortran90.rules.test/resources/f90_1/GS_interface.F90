! ---------------------------------------------
! Interface MSPRO pour la GS_LIB
!
!  Version :
!   $Id: GS_interface.F90 379 2013-02-22 16:59:49Z ffsm $
!  Historique :
!   $Log: GS_interface.F90,v $
!   Revision 379  2013/02/22 ffsm
!   DM-ID 1513: Montee de niveau Gfortran
!
!   Revision 1.87  2011/06/01 15:53:04  vivaresf
!   VERSION::FA-ID:1471:01/06/2011: validation avec ANGE
!
!   Revision 1.86  2011/05/30 15:01:12  vivaresf
!   VERSION::FA-ID:1471:30/05/2011:raz des valeurs de covariances trop proches de 0
!
!   Revision 1.85  2009/11/02 10:11:49  cmartel
!   AQ : Suppression de traces dans le code
!
!   Revision 1.84  2009/10/16 16:29:27  kvernelo
!   VERSION:6.12:FA-ID:1324:16/10/2009:Gestion de la lanuge au niveau du read/write pour pouvoir lire/ecrire dans une langue differente de la langue d'affichage
!
!   Revision 1.83  2008/10/29 16:56:18  tanguyy
!   AQ : correction de warnings Foresys
!
!   Revision 1.82  2008/10/28 15:45:37  tanguyy
!   AQ : suppression de code mort & FA-ID 883 : renommage de la routine gsi_get_liste_astre_rep
!
!   Revision 1.81  2008/10/22 16:23:20  tanguyy
!   FA-ID 883 : sauvegarde de la theorie des poles dans le mode historique
!
!   Revision 1.80  2008/10/20 15:24:43  tanguyy
!   AQ : maj des commentaires
!
!   Revision 1.79  2008/10/14 13:05:02  cml
!   DM-ID 1111 : Utilisation du nom du modele au lieu du path du fichier
!
!   Revision 1.78  2008/10/10 08:31:48  cml
!   DM-ID 1111 : Correction d un message d erreur
!
!   Revision 1.77  2008/10/10 07:52:24  tanguyy
!   AQ : correction du cartouche
!
!   Revision 1.76  2008/10/10 07:51:30  tanguyy
!   FA-ID 1029 : suppression des conversions reel<->double dans gs_get_donnees_astre + verif AUTOQUAL
!
!   Revision 1.75  2008/10/10 07:17:55  tanguyy
!   FA-ID 1029 : suppression des conversions reel<->double dans gscovfor_ip
!
!   Revision 1.74  2008/10/10 06:51:15  tanguyy
!   FA-ID 883 : sauvegarde de la theorie des poles dans th_uai via la routine gs_init_bulletin, et simplification des initialisations
!
!   Revision 1.73  2008/10/08 17:06:49  tanguyy
!   FA-ID 883 : la routine principale d'init s'appelle gs_init_bulletin. Les messages ne sont plus forcement affiches. La compatibilite avec les codes de theories de poles MSPRO est assuree
!
!   Revision 1.72  2008/10/07 16:02:51  cml
!   DM-ID 1111 : Correction du nom de la structure COMPAS
!
!   Revision 1.71  2008/10/07 11:58:07  tanguyy
!   FA-ID 883 : nouvelles routines gs_init_bulletin et gs_init_bulletin_fichier
!
!   Revision 1.70  2008/09/30 07:30:04  cml
!   DM-ID 1111 : Ajout d un code d erreur dans la routine d interpolation
!
!   Revision 1.69  2008/09/23 13:17:14  cml
!   AQ : Suppression des traces superflues
!
!   Revision 1.68  2008/09/18 15:33:50  cml
!   DM-ID 1111 : Deplacement de routines dans GS_interface
!
!   Revision 1.67  2008/09/16 09:14:23  cml
!   DM-ID 1111 : Utilisation du modele de pole sur fichier
!
!   Revision 1.66  2008/09/02 12:09:22  tanguyy
!   DM-ID 1058 : suppression des warnings levés par g95
!
!   Revision 1.65  2008/04/23 08:10:34  tanguyy
!   FA-ID 862 : verif et correction des variables inutilises
!
!   Revision 1.64  2008/04/01 06:43:36  huec
!   FA-ID 865 : Suppression de la variable obsolete modprec
!
!   Revision 1.63  2008/03/31 12:08:13  ttn
!   correction coquille dans gs_get_obliquite
!
!   Revision 1.62  2008/03/28 15:20:16  tanguyy
!   FA-ID 866 : nouvelle routine gs_get_obliquite
!
!   Revision 1.61  2008/03/19 14:28:40  tanguyy
!   FA-ID 861 : correction d'une déclaration de chaine de caractères
!
!   Revision 1.60  2008/02/15 16:27:04  huec
!   DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors COMPAS
!
!   Revision 1.59  2008/02/14 08:58:32  huec
!   DM-ID 11 : Appel a cps_get_sautTAITUC pour recuperer le saut du TUC dans la base COMPAS
!
!   Revision 1.58  2008/01/31 08:38:42  huec
!   FA-ID 862 : Suppression des variables inutilisees
!
!   Revision 1.57  2008/01/29 09:21:16  tanguyy
!   FA-ID 862 : suppression de variables inutilisées dans GS_interface
!
!   Revision 1.56  2008/01/14 10:32:34  huec
!   FA-ID 921 : mise a jour du tableau des noms de repere
!
!   Revision 1.55  2008/01/08 11:01:46  huec
!   FA-ID 921 : Reinitialisation de la liste des noms de corps dans le cas d un changement de langue
!
!   Revision 1.54  2008/01/08 08:23:03  huec
!   FA-ID 921 : Reinitialisation de la liste des corps si la langue a change dans gs_init_bulletin
!
!   Revision 1.53  2008/01/03 16:34:22  huec
!   AQ : Suppression de code duplique
!
!   Revision 1.52  2007/11/22 16:48:41  tanguyy
!   GSLIB V6.8 : utilisation de eph_.. au lieu des constantes gs_.. de l'ancien parametres_corps.h
!
!   Revision 1.51  2007/11/21 12:43:49  vivaresf
!   DM-ID 539 : validation
!
!   Revision 1.50  2007/11/14 17:07:40  sbd
!   FA-ID 835 suppression du doublon des variables premier_pas et ier declarees 2 fois
!
!   Revision 1.49  2007/10/09 12:32:39  huec
!   DM-ID 539 : Modification de l objet GS_CORPS pour exploiter la liste des corps COMPAS
!
!   Revision 1.48  2007/09/18 12:09:38  huec
!   DM-ID 539 Modification de l objet GS_CORPS pour exploiter la liste des corps COMPAS
!
!   Revision 1.47  2007/08/20 12:53:19  tanguyy
!   Simplification de l'algorithme de gscovreploc
!
!   Revision 1.46  2007/08/06 08:29:09  vivaresf
!   FA-ID 801 : correction de la conversion vers QSW/TNW
!   correction des cartouches
!
!   Revision 1.45  2007/06/07 07:17:25  bibms
!   DM-ID 538 : validation des chgt repres
!
!   Revision 1.44  2007/04/25 08:03:08  vivaresf
!   DM-ID 538 : validation des données (ré-initialisations
!   uniquement sur demande du mu dans GS_TYPBUL,
!   validation des clear, read et write
!   Changements de repères avec th_poles
!
!   Revision 1.43  2007/01/29 15:02:57  vivaresf
!   Version 6.7a1 : noms des corps en minuscule avant appel à COMPAS
!
!   Revision 1.42  2007/01/29 13:18:57  vivaresf
!   DM-ID 643 : on ne retourne pas les fichiers mais les noms de modèles
!   filtrage des fichiers en direct
!
!   Revision 1.41  2007/01/25 10:20:09  vivaresf
!   DM-ID 643 : 2 types de modèles ATM et ARPEGE
!
!   Revision 1.40  2007/01/23 15:33:31  vivaresf
!   DM-ID 643 : appel COMPAS pour les modèles de vent
!
!   Revision 1.39  2007/01/22 08:31:51  mle
!   DM-ID 643 : modeles de vent dans PSIMU
!
!   Revision 1.38  2006/11/17 07:31:45  vivaresf
!   Version 6.6 : validation
!
!   Revision 1.37  2006/11/06 17:08:37  vpg
!   DM-ID 425 : passage PSIMU sous Linux
!
!   Revision 1.36  2006/11/06 13:30:18  mle
!   DM-ID 560 : integration des modeles de potentiel de COMPAS dans PSIMU
!
!   Revision 1.35  2006/10/16 13:18:14  vivaresf
!   Commentaires
!
!   Revision 1.34  2006/06/27 13:23:12  tanguyy
!   Suppression d'un doublon sur les balises de log CVS
!
!   Revision 1.33  2006/06/20 15:46:05  tanguyy
!   Utilisation d'angles en radians a la place des angles en degres
!
!   Revision 1.32  2006/05/22 10:14:42  tanguyy
!   Test sur la valeur de l'aplatissement
!
!   Revision 1.31  2006/03/01 10:27:49  bouillaj
!   Amelioration des volumes de donnees stockees
!
!   Revision 1.30  2006/01/23 09:11:06  bouillaj
!   Suppression des fichiers sur les Mouvements de Poles pour passage sous COMPAS
!
!   Revision 1.29  2005/12/05 15:25:17  vivaresf
!   DM-ID 396 : validation
!   - messages d'erreur
!   - cartouches
!   - fonctions BULCOV
!
!   Revision 1.28  2005/12/05 07:57:12  valletn
!   insertion GS_changer_repere
!
!   Revision 1.27  2005/11/24 16:29:27  bouillaj
!   DM-ID 144 Validation
!
!   Revision 1.26  2005/11/23 13:41:00  vivaresf
!   DM-ID 144 : mise au point
!
!   Revision 1.25  2005/11/23 08:12:14  valletn
!   rapatriement gscovfor_ip depuis bulcov
!
!   Revision 1.24  2005/11/22 15:58:21  vivaresf
!   DM-ID 144 : repères liés à la Lune et aux satellites de Mars
!
!   Revision 1.23  2005/11/22 15:08:56  valletn
!   DM396 rapatriement de fonctions de F77 en F90
!
!   Revision 1.22  2005/11/08 11:40:52  vivaresf
!   DM-ID 144 : appels a COMPAS
!
!   Revision 1.21  2005/11/02 10:14:33  bouillaj
!   *** empty log message ***
!
!   Revision 1.20  2005/10/27 14:40:58  vivaresf
!   DM-ID 396 : mise au point des changements de repères dans le cas COMPAS
!
!   Revision 1.19  2005/10/26 12:50:31  vivaresf
!   DM-ID 144 : Changements de repères interplanétaires
!
!   Revision 1.18  2005/10/25 13:13:14  bouillaj
!   DM-ID 144 : option COMPAS pour les changements de repere et reperes
!   lunes
!
!   Revision 1.17  2005/03/14 09:00:02  vivaresf
!   DM-ID 226 : marquage de GS_interface
!
!
! ----------------------------------------------
 
subroutine GS_changer_repere_cov(param1,&
     rep1,ech_date1,numpla1,tframe1,tframe1sec,tfrech1,longref1, &
     pole_u1,pole_v1,obli1, &
     rep2,ech_date2,numpla2,tframe2,tframe2sec,tfrech2,longref2, &
     pole_u2,pole_v2,obli2, &
     vitrot1,vitrot2,requa,apla,optcompas, &
     param2,ier,jacob, th_poles, &
     fichier_poletsid_1, fichier_poletsid_2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_changer_repere_cov
!
!$Resume
!   Changement de repere
!
!$Description
!   Changement de repere pour l'objet GS_BULCOV_IP
!   Appel du sous-programme mx_rep de changement de repère de la MSPRO 
!
!$Auteur
!  Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_changer_repere_cov(param1,&
!.         rep1,ech_date1,numpla1,tframe1,tframe1sec,tfrech1,longref1, &
!.         pole_u1,pole_v1,obli1, &
!.         rep2,ech_date2,numpla2,tframe2,tframe2sec,tfrech2,longref2, &
!.         pole_u2,pole_v2,obli2, &
!.         vitrot1,vitrot2,requa,apla,optcompas, &
!.         param2,ier,jacob, th_poles, &
!.         [fichier_poletsid_1], [fichier_poletsid_2])
!.    real(KIND=PM_REEL),dimension(6) :: param1
!.    real(KIND=PM_REEL),dimension(10) :: rep1
!.    integer :: ech_date1
!.    integer :: numpla1
!.    real(KIND=PM_REEL) :: tframe1, tframe1sec
!.    integer :: tfrech1
!.    real(KIND=PM_REEL) :: longref1
!.    real(KIND=PM_REEL) :: pole_u1
!.    real(KIND=PM_REEL) :: pole_v1
!.    real(KIND=PM_REEL) :: obli1
!.    real(KIND=PM_REEL),dimension(10) :: rep2
!.    integer :: ech_date2
!.    integer :: numpla2
!.    real(KIND=PM_REEL) :: tframe2, tframe2sec
!.    integer :: tfrech2
!.    real(KIND=PM_REEL) :: longref2
!.    real(KIND=PM_REEL) :: pole_u2
!.    real(KIND=PM_REEL) :: pole_v2
!.    real(KIND=PM_REEL) :: vitrot1
!.    real(KIND=PM_REEL) :: vitrot2
!.    real(KIND=PM_REEL) :: obli2
!.    real(KIND=PM_REEL) :: requa
!.    real(KIND=PM_REEL) :: apla
!.    integer :: optcompas
!.    real(KIND=PM_REEL),dimension(6) :: param2
!.    integer :: ier
!.    real(KIND=PM_REEL),dimension(6,6) :: jacob
!.    integer :: th_poles
!.    character(len=*) :: fichier_poletsid_1
!.    character(len=*) :: fichier_poletsid_2
!
!$Arguments
!>E     param1              :<PM_REEL,DIM=(6)>     Vecteur position/vitesse en entrée
!>E     rep1                :<PM_REEL,DIM=(10)>    Tableau contenant les informations liées au repère de départ
!>E     ech_date1           :<integer>             Echelle de date du repère de départ
!>E     numpla1             :<integer>             Planète caractérisant le repère de départ
!>E     tframe1             :<PM_REEL>             Date de définition du repère de départ (jours)
!>E     tframe1sec          :<PM_REEL>             Date de définition du repère de départ (secondes)
!>E     tfrech1             :<integer>             Echelle de temps de la date de définition du repère de départ
!>E     longref1            :<PM_REEL>             Longitude de référence du repère de départ dans le cas des repères planéto 
!>E     pole_u1             :<PM_REEL>             Valeur de l'angle u caractérisant le repère de départ
!>E     pole_v1             :<PM_REEL>             Valeur de l'angle v caractérisant le repère de départ 
!>E     obli1               :<PM_REEL>             Valeur de l'angle d'obliquité du repère de départ 
!>E     rep2                :<PM_REEL,DIM=(10)>    Tableau contenant les informations liées au repère d'arrivée        
!>E     ech_date2           :<integer>             Echelle de date du repère d'arrivée 
!>E     numpla2             :<integer>             Planète caractérisant le repère d'arrivée 
!>E     tframe2             :<PM_REEL>             Date de définition du repère d'arrivée (jours)
!>E     tframe2sec          :<PM_REEL>             Date de définition du repère d'arrivée (secondes)
!>E     tfrech2             :<integer>             Echelle de temps de la date de définition du repère d'arrivée 
!>E     longref2            :<PM_REEL>             Longitude de référence du repère d'arrivée dans le cas des repères planéto 
!>E     pole_u2             :<PM_REEL>             Valeur de l'angle u caractérisant le repère d'arrivée 
!>E     pole_v2             :<PM_REEL>             Valeur de l'angle v caractérisant le repère d'arrivée 
!>E     obli2               :<PM_REEL>             Valeur de l'angle d'obliquité du repère d'arrivée 
!>E     vitrot1             :<PM_REEL>             Vitesse de rotation de la planete de depart 
!>E     vitrot2             :<PM_REEL>             Vitesse de rotation de la planete d'arrivee 
!>E     requa               :<PM_REEL>             Rayon équatorial du corps central
!>E     apla                :<PM_REEL>             aplatissement du corps central
!>E     optcompas           :<integer>             Option compas active ou non
!>S     param2              :<PM_REEL,DIM=(6)>     Vecteur position/vitesse en sortie
!>S     ier                 :<integer>             code retour
!.             ier = -1 Si erreur lors de la conversion des paramètres du premier repère 
!.             ier = -2 Si erreur lors de la conversion des paramètres du second repère 
!.             ier = -3 Si erreur lors du changement de repère par la MSPRO 
!>S     jacob               :<PM_REEL,DIM=(6,6)>   Jacobienne de la transformation
!>E     th_poles            :<integer>             Code de la théorie des pôles (UAI1991, MSPRO ou UAI2000)
!>[E]   fichier_poletsid_1  :<LEN=*>               Modèle de pôle sur fichier pour le corps du premier repère
!>[E]   fichier_poletsid_2  :<LEN=*>               Modèle de pôle sur fichier pour le corps du second repère
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MSPRO
  use parametre_mspro
  
  implicit none
#include "parameter_gslib.h"

  
  ! Arguments
  
  real(KIND=PM_REEL),dimension(6),   intent(IN)   :: param1
  real(KIND=PM_REEL),dimension(10),  intent(IN)   :: rep1
  integer,                           intent(IN)   :: ech_date1
  integer,                           intent(IN)   :: numpla1
  real(KIND=PM_REEL),                intent(IN)   :: tframe1, tframe1sec
  integer,                           intent(IN)   :: tfrech1
  real(KIND=PM_REEL),                intent(IN)   :: longref1
  real(KIND=PM_REEL),                intent(IN)   :: pole_u1
  real(KIND=PM_REEL),                intent(IN)   :: pole_v1
  real(KIND=PM_REEL),                intent(IN)   :: obli1
  real(KIND=PM_REEL),dimension(10),  intent(IN)   :: rep2
  integer,                           intent(IN)   :: ech_date2
  integer,                           intent(IN)   :: numpla2
  real(KIND=PM_REEL),                intent(IN)   :: tframe2, tframe2sec
  integer,                           intent(IN)   :: tfrech2
  real(KIND=PM_REEL),                intent(IN)   :: longref2
  real(KIND=PM_REEL),                intent(IN)   :: pole_u2
  real(KIND=PM_REEL),                intent(IN)   :: pole_v2
  real(KIND=PM_REEL),                intent(IN)   :: vitrot1
  real(KIND=PM_REEL),                intent(IN)   :: vitrot2
  real(KIND=PM_REEL),                intent(IN)   :: obli2
  real(KIND=PM_REEL),                intent(IN)   :: requa
  real(KIND=PM_REEL),                intent(IN)   :: apla
  integer,                           intent(IN)   :: optcompas
  real(KIND=PM_REEL),dimension(6),   intent(OUT)  :: param2
  integer,                           intent(OUT)  :: ier
  real(KIND=PM_REEL),dimension(6,6), intent(OUT)  :: jacob
  integer,                           intent(IN)   :: th_poles
  character(len=*), OPTIONAL,        intent(IN)   :: fichier_poletsid_1
  character(len=*), OPTIONAL,        intent(IN)   :: fichier_poletsid_2

  ! Variables locales

  integer :: prec_nuta_tsid_in, prec_nuta_tsid_out
  integer :: pm_numpla_in, pm_numpla_out
  integer :: mech_date1, mech_date2

  real(KIND=PM_REEL) :: delta_ech_tu1_in, delta_ech_tai_in
  real(KIND=PM_REEL) :: delta_ech_tu1_out, delta_ech_tai_out

  type(tm_code_retour)  :: code_retour

  ! Pour l'analyse des erreurs de changement de repere
  type(tm_code_retour) :: code_retour_tmp
  character(len=pm_signification_code_retour) :: signification ! Signification du code de retour
  logical :: poletsid_nonnul_1, poletsid_nonnul_2

  type(tm_pole_uv)  :: pole_in, pole_out
  type(tm_jour_sec) :: pm_date_in, pm_date_out
  type(tm_def_topo) :: topo_in, topo_out
  type(tm_pole_tsid_planete) :: pole_tsid_planete_in, pole_tsid_planete_out

  ! Interface 
  interface 
     subroutine GS_param_msp_vers_mspro ( repere, numpla, &
             lat_sta,lon_sta, alt_sta, requa, apla, direction, &
             pole_u, pole_v, ech_tref, optcompas, &
             pm_numpla, prec_nuta_tsid, pole, topo, &
             delta_ech_tu1,delta_ech_tai,dateref_pm, &
             pole_tsid_planete,ier, th_poles, &
             fichier_poletsid)
!$<AM-V2.0>
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     use MSPRO
      
     ! Arguments
     integer,                           intent(IN)   :: repere
     integer,                           intent(IN)   :: numpla
     real(KIND=PM_REEL),                intent(IN)   :: lat_sta
     real(KIND=PM_REEL),                intent(IN)   :: lon_sta
     real(KIND=PM_REEL),                intent(IN)   :: alt_sta
     real(KIND=PM_REEL),                intent(IN)   :: requa
     real(KIND=PM_REEL),                intent(IN)   :: apla
     integer,                           intent(IN)   :: direction
     real(KIND=PM_REEL),                intent(IN)   :: pole_u
     real(KIND=PM_REEL),                intent(IN)   :: pole_v
     integer,                           intent(in)   :: ech_tref
     integer,                           intent(IN)   :: optcompas
     integer,                           intent(OUT)  :: pm_numpla
     integer,                           intent(OUT)  :: prec_nuta_tsid
     type(tm_pole_uv),                  intent(OUT)  :: pole
     type(tm_def_topo),                 intent(OUT)  :: topo
     real(kind=PM_REEL),                intent(OUT)  :: delta_ech_tu1
     real(kind=PM_REEL),                intent(OUT)  :: delta_ech_tai
     type(tm_jour_sec),                 intent(IN)   :: dateref_pm
     type(tm_pole_tsid_planete),        intent(OUT)  :: pole_tsid_planete
     integer,                           intent(OUT)  :: ier
     integer,                           intent(IN)   :: th_poles
     character(len=*), OPTIONAL,        intent(IN)   :: fichier_poletsid

   end subroutine GS_param_msp_vers_mspro
  end interface 

! Début code

  ! Initialisations
  ier = 0

  ! Analyse de la présence ou non des options
  poletsid_nonnul_1 = .false.
  if ( present(fichier_poletsid_1) ) then
     if ( len_trim(fichier_poletsid_1) > 0 ) then
        poletsid_nonnul_1 = .true.
     endif
  endif
        
  poletsid_nonnul_2 = .false.
  if ( present(fichier_poletsid_2) ) then
     if ( len_trim(fichier_poletsid_2) > 0 ) then
        poletsid_nonnul_2 = .true.
     endif
  endif
  ! Transformation des paramètres vers MSPRO

  ! -- Calcul des paramètres MSPRO du repère de départ
  pm_date_in%jour = int(tframe1)
  pm_date_in%sec  = tframe1sec

  if ( poletsid_nonnul_1 ) then
     call GS_param_msp_vers_mspro(  int(rep1(1)), numpla1, &
       rep1(4), rep1(5), rep1(6), requa, apla, int(rep1(7)), &
       pole_u1, pole_v1, tfrech1, optcompas, &
       pm_numpla_in, prec_nuta_tsid_in, pole_in, topo_in, &
       delta_ech_tu1_in, delta_ech_tai_in, pm_date_in, &
       pole_tsid_planete_in, ier, th_poles=th_poles, fichier_poletsid=fichier_poletsid_1 )
  else
     call GS_param_msp_vers_mspro( int(rep1(1)), numpla1, &
       rep1(4), rep1(5), rep1(6), requa, apla, int(rep1(7)), &
       pole_u1, pole_v1, tfrech1, optcompas, &
       pm_numpla_in, prec_nuta_tsid_in, pole_in, topo_in, &
       delta_ech_tu1_in, delta_ech_tai_in, pm_date_in, &
          pole_tsid_planete_in, ier, th_poles=th_poles )
  endif
  if (ier < 0) then
     write(0,*) 'Sous-programme : GS_changer_repere_cov'
     write(0,*) 'Erreur ',ier,' dans le sous-programme GS_param_msp_vers_mspro'
     ier = -1
     return
  endif

  ! ech_date date_ref devient autre_date
  mech_date1 = ech_date1
  if (mech_date1 == pm_autre_date + 1) mech_date1 = pm_autre_date


  ! -- Calcul des paramètres MSPRO du repère d'arrivée
  pm_date_out%jour = int(tframe2)
  pm_date_out%sec  = tframe2sec

  if ( poletsid_nonnul_2 ) then
     call GS_param_msp_vers_mspro( int(rep2(1)), numpla2, &
       rep2(4), rep2(5), rep2(6), requa, apla, int(rep2(7)), &
       pole_u2, pole_v2, tfrech2, optcompas, &
       pm_numpla_out, prec_nuta_tsid_out, pole_out, topo_out, &
       delta_ech_tu1_out, delta_ech_tai_out, pm_date_out, &
       pole_tsid_planete_out, ier, th_poles=th_poles , fichier_poletsid=fichier_poletsid_2)
  else
     call GS_param_msp_vers_mspro( int(rep2(1)), numpla2, &
       rep2(4), rep2(5), rep2(6), requa, apla, int(rep2(7)), &
       pole_u2, pole_v2, tfrech2, optcompas, &
       pm_numpla_out, prec_nuta_tsid_out, pole_out, topo_out, &
       delta_ech_tu1_out, delta_ech_tai_out, pm_date_out, &
       pole_tsid_planete_out, ier, th_poles=th_poles )
  endif

  if (ier < 0) then
     write(0,*) 'Sous-programme : GS_changer_repere_cov'
     write(0,*) 'Erreur ',ier,' dans le sous-programme GS_param_msp_vers_mspro'
     ier = -2
     return
  endif

  ! ech_date date_ref devient autre_date
  mech_date2 = ech_date2
  if (mech_date2 == pm_autre_date + 1) mech_date2 = pm_autre_date


  !Changement de repere MSPRO 
  call mx_rep(int(rep1(1)),pm_numpla_in,mech_date1,prec_nuta_tsid_in, &
      param1(1:3),param1(4:6), &
      int(rep2(1)),pm_numpla_out,mech_date2,prec_nuta_tsid_out, &
      param2(1:3),param2(4:6), &
      code_retour, &
      vit_rot_in=vitrot1, vit_rot_out=vitrot2, &
      obliquite_in=obli1, obliquite_out=obli2,& 
      pole_in=pole_in, pole_out=pole_out, &
      long_ref_in=longref1, long_ref_out=longref2, &
      val_date_in=pm_date_in,delta_tu1_in=delta_ech_tu1_in,delta_tai_in=delta_ech_tai_in, &
      val_date_out=pm_date_out,delta_tu1_out=delta_ech_tu1_out,delta_tai_out=delta_ech_tai_out, &
      eps_date=1.e-6_PM_REEL, def_topo_in=topo_in, def_topo_out=topo_out, &
      pole_tsid_planete_in=pole_tsid_planete_in, pole_tsid_planete_out=pole_tsid_planete_out,jacob=jacob )
  if (code_retour%valeur < 0) then
      call mzipro_val_retour (code_retour%valeur, signification, code_retour_tmp)
      write(0,*) 'Sous-programme : GS_changer_repere_cov'
      write(0,*) 'Erreur ',code_retour%valeur,' (', trim(signification), &
           ') dans le sous-programme mx_rep'
      ier = -3
   endif


end subroutine GS_changer_repere_cov

subroutine GS_changer_repere(param1,&
     rep1,ech_date1,numpla1,tframe1,tframe1sec,tfrech1,longref1, &
     pole_u1,pole_v1,obli1, &
     rep2,ech_date2,numpla2,tframe2,tframe2sec,tfrech2,longref2, &
     pole_u2,pole_v2,obli2, &
     vitrot1,vitrot2,requa,apla,optcompas, &
     param2,ier, th_poles, &
     fichier_poletsid_1, fichier_poletsid_2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_changer_repere
!
!$Resume
!   Changement de repere 
!
!$Description
!   Changement de repere pour l'objet GS_BULLETIN_IP
!   Appel du sous-programme mx_rep de changement de repère de la MSPRO 
!
!$Auteur
!  Philippe Brémard (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_changer_repere(param1,&
!.         rep1,ech_date1,numpla1,tframe1,tframe1sec,tfrech1,longref1, &
!.         pole_u1,pole_v1,obli1, &
!.         rep2,ech_date2,numpla2,tframe2,tframe2sec,tfrech2,longref2, &
!.         pole_u2,pole_v2,obli2, &
!.         vitrot1,vitrot2,requa,apla,optcompas, &
!.         param2,ier, th_poles, &
!.         [fichier_poletsid_1], [fichier_poletsid_2])
!.    real(KIND=PM_REEL),dimension(6) :: param1
!.    real(KIND=PM_REEL),dimension(10) :: rep1
!.    integer :: ech_date1
!.    integer :: numpla1
!.    real(KIND=PM_REEL) :: tframe1, tframe1sec
!.    integer :: tfrech1
!.    real(KIND=PM_REEL) :: longref1
!.    real(KIND=PM_REEL) :: pole_u1
!.    real(KIND=PM_REEL) :: pole_v1
!.    real(KIND=PM_REEL) :: obli1
!.    real(KIND=PM_REEL),dimension(10) :: rep2
!.    integer :: ech_date2
!.    integer :: numpla2
!.    real(KIND=PM_REEL) :: tframe2, tframe2sec
!.    integer :: tfrech2
!.    real(KIND=PM_REEL) :: longref2
!.    real(KIND=PM_REEL) :: pole_u2
!.    real(KIND=PM_REEL) :: pole_v2
!.    real(KIND=PM_REEL) :: vitrot1
!.    real(KIND=PM_REEL) :: vitrot2
!.    real(KIND=PM_REEL) :: obli2
!.    real(KIND=PM_REEL) :: requa
!.    real(KIND=PM_REEL) :: apla
!.    integer :: optcompas
!.    real(KIND=PM_REEL),dimension(6) :: param2
!.    integer :: ier
!.    integer :: th_poles
!.    character(len=*) :: fichier_poletsid_1
!.    character(len=*) :: fichier_poletsid_2
!
!$Arguments
!>E     param1              :<PM_REEL,DIM=(6)>    Vecteur position/vitesse en entrée
!>E     rep1                :<PM_REEL,DIM=(10)>   Tableau contenant les informations liées au repère de départ
!>E     ech_date1           :<integer>            Echelle de date du repère de départ
!>E     numpla1             :<integer>            Planète caractérisant le repère de départ
!>E     tframe1             :<PM_REEL>            Date de définition du repère de départ (jours)
!>E     tframe1sec          :<PM_REEL>            Date de définition du repère de départ (secondes)
!>E     tfrech1             :<integer>            Echelle de temps de la date de définition du repère de départ
!>E     longref1            :<PM_REEL>            Longitude de référence du repère de départ dans le cas des repères planéto 
!>E     pole_u1             :<PM_REEL>            Valeur de l'angle u caractérisant le repère de départ
!>E     pole_v1             :<PM_REEL>            Valeur de l'angle v caractérisant le repère de départ 
!>E     obli1               :<PM_REEL>            Valeur de l'angle d'obliquité du repère de départ 
!>E     rep2                :<PM_REEL,DIM=(10)>   Tableau contenant les informations liées au repère d'arrivée        
!>E     ech_date2           :<integer>            Echelle de date du repère d'arrivée 
!>E     numpla2             :<integer>            Planète caractérisant le repère d'arrivée 
!>E     tframe2             :<PM_REEL>            Date de définition du repère d'arrivée (jours)
!>E     tframe2sec          :<PM_REEL>            Date de définition du repère d'arrivée (secondes)
!>E     tfrech2             :<integer>            Echelle de temps de la date de définition du repère d'arrivée 
!>E     longref2            :<PM_REEL>            Longitude de référence du repère d'arrivée dans le cas des repères planéto 
!>E     pole_u2             :<PM_REEL>            Valeur de l'angle u caractérisant le repère d'arrivée 
!>E     pole_v2             :<PM_REEL>            Valeur de l'angle v caractérisant le repère d'arrivée 
!>E     obli2               :<PM_REEL>            Valeur de l'angle d'obliquité du repère d'arrivée 
!>E     vitrot1             :<PM_REEL>            Vitesse de rotation de la planete de depart 
!>E     vitrot2             :<PM_REEL>            Vitesse de rotation de la planete d'arrivee 
!>E     requa               :<PM_REEL>            Rayon équatorial du corps central
!>E     apla                :<PM_REEL>            aplatissement du corps central
!>E     optcompas           :<integer>            Option compas active ou non
!>S     param2              :<PM_REEL,DIM=(6)>    Vecteur position/vitesse en sortie
!>S     ier                 :<integer>            code retour
!.             ier = -1 Si erreur lors de la conversion des paramètres du premier repère 
!.             ier = -2 Si erreur lors de la conversion des paramètres du second repère 
!.             ier = -3 Si erreur lors du changement de repère par la MSPRO 
!>E     th_poles            :<integer>             Code de la théorie des pôles (UAI1991, MSPRO ou UAI2000)
!>[E]   fichier_poletsid_1  :<LEN=*>               Modèle de pôle sur fichier pour le corps du premier repère
!>[E]   fichier_poletsid_2  :<LEN=*>               Modèle de pôle sur fichier pour le corps du second repère
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MSPRO
  
  implicit none

#include "parameter_gslib.h"
  
  ! Arguments
  
  real(KIND=PM_REEL),dimension(6),   intent(IN)   :: param1
  real(KIND=PM_REEL),dimension(10),  intent(IN)   :: rep1
  integer,                           intent(IN)   :: ech_date1
  integer,                           intent(IN)   :: numpla1
  real(KIND=PM_REEL),                intent(IN)   :: tframe1, tframe1sec
  integer,                           intent(IN)   :: tfrech1
  real(KIND=PM_REEL),                intent(IN)   :: longref1
  real(KIND=PM_REEL),                intent(IN)   :: pole_u1
  real(KIND=PM_REEL),                intent(IN)   :: pole_v1
  real(KIND=PM_REEL),                intent(IN)   :: obli1
  real(KIND=PM_REEL),dimension(10),  intent(IN)   :: rep2
  integer,                           intent(IN)   :: ech_date2
  integer,                           intent(IN)   :: numpla2
  real(KIND=PM_REEL),                intent(IN)   :: tframe2, tframe2sec
  integer,                           intent(IN)   :: tfrech2
  real(KIND=PM_REEL),                intent(IN)   :: longref2
  real(KIND=PM_REEL),                intent(IN)   :: pole_u2
  real(KIND=PM_REEL),                intent(IN)   :: pole_v2
  real(KIND=PM_REEL),                intent(IN)   :: vitrot1
  real(KIND=PM_REEL),                intent(IN)   :: vitrot2
  real(KIND=PM_REEL),                intent(IN)   :: obli2
  real(KIND=PM_REEL),                intent(IN)   :: requa
  real(KIND=PM_REEL),                intent(IN)   :: apla
  integer,                           intent(IN)   :: optcompas
  real(KIND=PM_REEL),dimension(6),   intent(OUT)  :: param2
  integer,                           intent(OUT)  :: ier
  integer,                           intent(IN)   :: th_poles
  character(len=*), OPTIONAL,        intent(IN)   :: fichier_poletsid_1
  character(len=*), OPTIONAL,        intent(IN)   :: fichier_poletsid_2
  

  ! Variables locales

  integer :: prec_nuta_tsid_in, prec_nuta_tsid_out
  integer :: pm_numpla_in, pm_numpla_out
  integer :: mech_date1, mech_date2

  ! Pour l'analyse des erreurs de changement de repere
  type(tm_code_retour) :: code_retour_tmp
  character(len=pm_signification_code_retour) :: signification ! Signification du code de retour
  logical :: poletsid_nonnul_1, poletsid_nonnul_2

  real(KIND=PM_REEL) :: delta_ech_tu1_in, delta_ech_tai_in
  real(KIND=PM_REEL) :: delta_ech_tu1_out, delta_ech_tai_out

  type(tm_pole_uv)  :: pole_in, pole_out
  type(tm_jour_sec) :: pm_date_in, pm_date_out
  type(tm_def_topo) :: topo_in, topo_out
  type(tm_pole_tsid_planete) :: pole_tsid_planete_in, pole_tsid_planete_out

  ! Variables locales
  type(tm_code_retour)  :: code_retour

  ! Interface 
  interface 
     subroutine GS_param_msp_vers_mspro ( repere, numpla, &
             lat_sta,lon_sta, alt_sta, requa, apla, direction, &
             pole_u, pole_v, ech_tref, optcompas, &
             pm_numpla, prec_nuta_tsid, pole, topo, &
             delta_ech_tu1,delta_ech_tai,dateref_pm, &
             pole_tsid_planete,ier, th_poles, &
             fichier_poletsid)
!$<AM-V2.0>
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     use MSPRO
     ! Arguments
     integer,                    intent(IN)   :: repere
     integer,                    intent(IN)   :: numpla
     real(KIND=PM_REEL),         intent(IN)   :: lat_sta
     real(KIND=PM_REEL),         intent(IN)   :: lon_sta
     real(KIND=PM_REEL),         intent(IN)   :: alt_sta
     real(KIND=PM_REEL),         intent(IN)   :: requa
     real(KIND=PM_REEL),         intent(IN)   :: apla
     integer,                    intent(IN)   :: direction
     real(KIND=PM_REEL),         intent(IN)   :: pole_u
     real(KIND=PM_REEL),         intent(IN)   :: pole_v
     integer,                    intent(in)   :: ech_tref
     integer,                    intent(IN)   :: optcompas
     integer,                    intent(OUT)  :: pm_numpla
     integer,                    intent(OUT)  :: prec_nuta_tsid
     type(tm_pole_uv),           intent(OUT)  :: pole
     type(tm_def_topo),          intent(OUT)  :: topo
     real(kind=PM_REEL),         intent(OUT)  :: delta_ech_tu1
     real(kind=PM_REEL),         intent(OUT)  :: delta_ech_tai
     type(tm_jour_sec),          intent(IN)   :: dateref_pm
     type(tm_pole_tsid_planete), intent(OUT)  :: pole_tsid_planete
     integer,                    intent(OUT)  :: ier
     integer,                    intent(IN)   :: th_poles
     character(len=*), OPTIONAL, intent(IN)   :: fichier_poletsid

    end subroutine GS_param_msp_vers_mspro
  end interface 

! Début code

  ! Initialisations
  ier = 0

  ! Transformation des paramètres vers MSPRO

  ! Analyse de la présence ou non des options
  poletsid_nonnul_1 = .false.
  if ( present(fichier_poletsid_1) ) then
     if ( len_trim(fichier_poletsid_1) > 0 ) then
        poletsid_nonnul_1 = .true.
     endif
  endif
        
  poletsid_nonnul_2 = .false.
  if ( present(fichier_poletsid_2) ) then
     if ( len_trim(fichier_poletsid_2) > 0 ) then
        poletsid_nonnul_2 = .true.
     endif
  endif
  
  ! -- Calcul des paramètres MSPRO du repère de départ
  pm_date_in%jour = int(tframe1)
  pm_date_in%sec  = tframe1sec

  if ( poletsid_nonnul_1 ) then
     call GS_param_msp_vers_mspro( int(rep1(1)), numpla1, &
       rep1(4), rep1(5), rep1(6), requa, apla, int(rep1(7)), &
       pole_u1, pole_v1, tfrech1, optcompas, &
       pm_numpla_in, prec_nuta_tsid_in, pole_in, topo_in, &
       delta_ech_tu1_in, delta_ech_tai_in, pm_date_in, &
       pole_tsid_planete_in, ier, th_poles=th_poles, fichier_poletsid=fichier_poletsid_1)
  else
     call GS_param_msp_vers_mspro( int(rep1(1)), numpla1, &
       rep1(4), rep1(5), rep1(6), requa, apla, int(rep1(7)), &
       pole_u1, pole_v1, tfrech1, optcompas, &
       pm_numpla_in, prec_nuta_tsid_in, pole_in, topo_in, &
       delta_ech_tu1_in, delta_ech_tai_in, pm_date_in, &
       pole_tsid_planete_in, ier, th_poles=th_poles)
  endif
  if (ier < 0) then
     write(0,*) 'Sous-programme : GS_changer_repere'
     write(0,*) 'Erreur ',ier,' dans le sous-programme GS_param_msp_vers_mspro'
     ier = -1
     return
  endif

  ! ech_date date_ref devient autre_date
  mech_date1 = ech_date1
  if (mech_date1 == pm_autre_date + 1) mech_date1 = pm_autre_date


  ! -- Calcul des paramètres MSPRO du repère d'arrivée
  pm_date_out%jour = int(tframe2)
  pm_date_out%sec  = tframe2sec

  if ( poletsid_nonnul_2 ) then
     call GS_param_msp_vers_mspro( int(rep2(1)), numpla2, &
       rep2(4), rep2(5), rep2(6), requa, apla, int(rep2(7)), &
       pole_u2, pole_v2, tfrech2, optcompas, &
       pm_numpla_out, prec_nuta_tsid_out, pole_out, topo_out, &
       delta_ech_tu1_out, delta_ech_tai_out, pm_date_out, &
       pole_tsid_planete_out, ier, th_poles=th_poles, fichier_poletsid=fichier_poletsid_2)
  else
     call GS_param_msp_vers_mspro( int(rep2(1)), numpla2, &
       rep2(4), rep2(5), rep2(6), requa, apla, int(rep2(7)), &
       pole_u2, pole_v2, tfrech2, optcompas, &
       pm_numpla_out, prec_nuta_tsid_out, pole_out, topo_out, &
       delta_ech_tu1_out, delta_ech_tai_out, pm_date_out, &
       pole_tsid_planete_out, ier, th_poles=th_poles)
  endif

  if (ier < 0) then
     write(0,*) 'Sous-programme : GS_changer_repere'
     write(0,*) 'Erreur ',ier,' dans le sous-programme GS_param_msp_vers_mspro'
     ier = -2
     return
  endif

  ! ech_date date_ref devient autre_date
  mech_date2 = ech_date2
  if (mech_date2 == pm_autre_date + 1) mech_date2 = pm_autre_date

  !Changement de repere MSPRO 
  call mx_rep(int(rep1(1)),pm_numpla_in,mech_date1,prec_nuta_tsid_in, &
       param1(1:3),param1(4:6), &
       int(rep2(1)),pm_numpla_out,mech_date2,prec_nuta_tsid_out, &
       param2(1:3),param2(4:6), &
       code_retour, &
       vit_rot_in=vitrot1, vit_rot_out=vitrot2, &
       obliquite_in=obli1, obliquite_out=obli2,& 
       pole_in=pole_in, pole_out=pole_out, &
       long_ref_in=longref1, long_ref_out=longref2, &
       val_date_in=pm_date_in,delta_tu1_in=delta_ech_tu1_in,delta_tai_in=delta_ech_tai_in, &
       val_date_out=pm_date_out,delta_tu1_out=delta_ech_tu1_out,delta_tai_out=delta_ech_tai_out, &
       eps_date=1.e-6_PM_REEL, def_topo_in=topo_in, def_topo_out=topo_out, &
       pole_tsid_planete_in=pole_tsid_planete_in, pole_tsid_planete_out=pole_tsid_planete_out)

   
  if (code_retour%valeur < 0) then
      call mzipro_val_retour (code_retour%valeur, signification, code_retour_tmp)
      write(0,*) 'Sous-programme : GS_changer_repere'
      write(0,*) 'Erreur ',code_retour%valeur,' (', &
           trim(signification), ') dans le sous-programme mx_rep'
      ier = -3
   endif


 end subroutine GS_changer_repere


 subroutine GS_param_msp_vers_mspro ( repere,  numpla, &
      lat_sta,lon_sta, alt_sta, requa, apla, direction, &
      pole_u, pole_v, ech_tref, optcompas, &
      pm_numpla, prec_nuta_tsid, pole, topo, &
      delta_ech_tu1,delta_ech_tai,dateref_pm, &
      pole_tsid_planete,ier, th_poles, &
      fichier_poletsid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_param_msp_vers_mspro
!
!$Resume
!  Préparation des paramètres spécifiques MSPRO
!
!$Description
!  Préparation des paramètres spécifiques MSPRO
!
!$Auteur
!  23/01/2003 - Philippe Bremard / Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_param_msp_vers_mspro ( repere, numpla, &
!.          lat_sta,lon_sta, alt_sta, requa, apla, direction, &
!.          pole_u, pole_v, ech_tref, optcompas, &
!.          pm_numpla, prec_nuta_tsid, pole, topo, &
!.          delta_ech_tu1,delta_ech_tai,dateref_pm, &
!.          pole_tsid_planete,ier, th_poles)
!.    integer :: repere
!.    integer :: numpla
!.    real(KIND=PM_REEL) :: lat_sta
!.    real(KIND=PM_REEL) :: lon_sta
!.    real(KIND=PM_REEL) :: alt_sta
!.    real(KIND=PM_REEL) :: requa
!.    real(KIND=PM_REEL) :: apla
!.    integer :: direction
!.    real(KIND=PM_REEL) :: pole_u
!.    real(KIND=PM_REEL) :: pole_v
!.    integer :: ech_tref
!.    integer :: optcompas
!.    integer :: pm_numpla
!.    integer :: prec_nuta_tsid
!.    type(tm_pole_uv) :: pole
!.    type(tm_def_topo) :: topo
!.    real(kind=PM_REEL) :: delta_ech_tu1
!.    real(kind=PM_REEL) :: delta_ech_tai
!.    type(tm_jour_sec) :: dateref_pm
!.    type(tm_pole_tsid_planete) :: pole_tsid_planete
!.    integer :: ier
!.    integer :: th_poles
!
!$Arguments
!>E     repere             :<integer>                Code du repère
!>E     numpla             :<integer>                Numéro de la planète (même numérotation LIBEPHEM que corcen)
!>E     lat_sta            :<PM_REEL>                Latitude station (rad) pour repère topocentrique
!>E     lon_sta            :<PM_REEL>                Longitude station (rad) pour repère topocentrique
!>E     alt_sta            :<PM_REEL>                Altitude station (rad) pour repère topocentrique
!>E     requa              :<PM_REEL>                Rayon équatorial du corps central (m)
!>E     apla               :<PM_REEL>                Aplatissement ( a / (a-b))
!>E     direction          :<integer>                Direction du repère topocentrique (1:Nord, 2:sud, 3:Est, 4:Ouest)
!>E     pole_u             :<PM_REEL>                Valeur de l'angle u caractérisant le repère
!>E     pole_v             :<PM_REEL>                Valeur de l'angle v caractérisant le repère
!>E     ech_tref           :<integer>                Echelle de temps de la date de définition du repère
!>E     optcompas          :<integer>                Option COMPAS active ou non
!>S     pm_numpla          :<integer>                Numéro de la planète (numérotation MSPRO)
!>S     prec_nuta_tsid     :<integer>                Code du modèle de précession
!>S     pole               :<tm_pole_uv>             Structure pole de la MSPRO
!>S     topo               :<tm_def_topo>            Structure topo de la MSPRO
!>S     delta_ech_tu1      :<PM_REEL>                Ecart entre l'échelle TU1 et l'échelle ech_tref 
!>S     delta_ech_tai      :<PM_REEL>                Ecart entre l'échelle TAI et l'échelle ech_tref
!>E     dateref_pm         :<tm_jour_sec>            Date de définition du repère au format MSPRO
!>S     pole_tsid_planete  :<tm_pole_tsid_planete>   Parametres d'orientation du Pole et de temps sidéral
!>S     ier                :<integer>                Code retour
!.             ier = -1 Si le numéro de la planète n'est pas compatible avec les données d'entrée
!.             ier = -5 Si la valeur de direction station est non valide
!.             ier = -6 Si erreur lors du calcul des écarts entre echelles de temps
!.             ier = -7 Si erreur lors de la lecture et l'interpolation d'un fichier pole et temps sidérals
!>E     th_poles           :<integer>                Code de la théorie des pôles (GS_TH_UAI1991, GS_TH_MSPRO ou GS_TH_UAI2000)
!>[E]   fichier_poletsid   :<LEN=*>                  Modèle de pôle sur fichier pour le corps du repère ("" si aucun)
!
!$Remarques
!
!$Mots-cles
!  CONVERSION PARAMETRES MSP MSPRO
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   use MSPRO
   use cps_poles
   use ephem
   use cps_poletsid_fichier, only : &
        cps_struct_poletsid_fichier, cps_lireFichierPoletsid, cps_interpolPoletsid

   implicit none

#include "parameter_gslib.h"

! Arguments

   integer,                    intent(IN)   :: repere
   integer,                    intent(IN)   :: numpla
   real(KIND=PM_REEL),         intent(IN)   :: lat_sta
   real(KIND=PM_REEL),         intent(IN)   :: lon_sta
   real(KIND=PM_REEL),         intent(IN)   :: alt_sta
   real(KIND=PM_REEL),         intent(IN)   :: requa
   real(KIND=PM_REEL),         intent(IN)   :: apla
   integer,                    intent(IN)   :: direction
   real(KIND=PM_REEL),         intent(IN)   :: pole_u
   real(KIND=PM_REEL),         intent(IN)   :: pole_v
   integer,                    intent(in)   :: ech_tref
   integer,                    intent(IN)   :: optcompas
   integer,                    intent(OUT)  :: pm_numpla
   integer,                    intent(OUT)  :: prec_nuta_tsid
   type(tm_pole_uv),           intent(OUT)  :: pole
   type(tm_def_topo),          intent(OUT)  :: topo
   real(kind=PM_REEL),         intent(OUT)  :: delta_ech_tu1
   real(kind=PM_REEL),         intent(OUT)  :: delta_ech_tai
   type(tm_jour_sec),          intent(IN)   :: dateref_pm
   type(tm_pole_tsid_planete), intent(OUT)  :: pole_tsid_planete
   integer,                    intent(OUT)  :: ier
   integer,                    intent(IN)   :: th_poles
   character(len=*), OPTIONAL, intent(IN)   :: fichier_poletsid

! Variables locales

   ! Variables utilisées lors du calcul des mouvement des pôles (compas et poletsid)
   real (kind=PM_REEL) :: alpha0, delta0, W, dW, date_jjfrac
   integer :: polescompas


! Début du code

   ! initialisations
   ier = 0

   ! -- Numéro de la planète en numérotation MSPRO
   select case(numpla)
   case(eph_mercure)
      pm_numpla = pm_pla_mercure
   case(eph_venus)
      pm_numpla = pm_pla_venus
   case(eph_terre)
      pm_numpla = pm_pla_terre
   case(eph_mars)
      pm_numpla = pm_pla_mars
   case(eph_jupiter)
      pm_numpla = pm_pla_jupiter
   case(eph_saturne)
      pm_numpla = pm_pla_saturne
   case(eph_neptune)
      pm_numpla = pm_pla_neptune
   case(eph_uranus)
      pm_numpla = pm_pla_uranus
   case(eph_pluton)
      pm_numpla = pm_pla_pluton
   case default
      ! Cas COMPAS (DM 144) : on donne des code planetes pour les satellites
      if (optcompas == 1.and.numpla < 1000 ) then
         ! corps attracteur des satellite
         pm_numpla = int(numpla/100)*100+99
      ! Attention, si modèle sur fichier, on testera cela plus loin
      else if (optcompas == 1.and.present(fichier_poletsid) ) then
         pm_numpla = 299 ! code par défaut pour un modèle externe
      else
         write(0,*) 'Sous-programme : GS_param_msp_vers_mspro'
         write(0,*) 'Le numéro de la planète n''est pas dans le domaine de validité'
         ier = -2
         return
      endif
   end select

   ! On teste si on a les repères standard ou non
   ! DM 1111 : Ajout des cas UAI1991+fichier et  UAI2000+fichier
   if (th_poles == GS_TH_UAI1991 .or. th_poles == GS_TH_UAI2000) then
      polescompas=th_poles
   else
      polescompas=0
   endif

   ! -- Affectation du modele de precession
   ! Ce modèle est fixé par la planete de reference du repere
   if (polescompas==0.and.pm_numpla==pm_pla_terre) then
      ! Le code Terre va dépendre de la liste des corps proposée
      ! pour les corps centraux
      prec_nuta_tsid = pm_lieske_wahr_aoki
   else if (polescompas>0.and.(repere==pm_planeto_ref.or. &
           repere==pm_planeto_ref_iner.or.repere==pm_equa_uai)) then
      prec_nuta_tsid = pm_UAI_autre_modele
   else
      prec_nuta_tsid = pm_uai1994
   endif

   ! Cas COMPAS, modele UAI : 
   ! ------------------------
   ! les repères UAI, planetocentriques et planetocentriques
   ! inertiels, non défini pour la Terre dans la MSPRO : on donne alors un
   ! code fictif (Venus) et on utilise les valeurs calculees par COMPAS
   if (prec_nuta_tsid/=pm_lieske_wahr_aoki.and.pm_numpla==399) then
      if (repere==pm_planeto_ref.or.repere==pm_planeto_ref_iner.or.&
           repere==pm_equa_uai) then
         pm_numpla=pm_pla_venus
      else
         prec_nuta_tsid = pm_lieske_wahr_aoki
      endif
   endif


   ! -- Affectation des coordonnées du pole vrai 
   pole%u = pole_u
   pole%v = pole_v

   ! -- Affectation des caractéristiques du repère topocentrique
   if (repere == pm_topo) then
      topo%geod%lat  = lat_sta      
      topo%geod%long = lon_sta   
      topo%geod%haut = alt_sta
      topo%ellips%r_equa = requa
      if (abs(apla) > SPACING(apla)) then
         ! apla /= 0._PM_REEL
         topo%ellips%apla =   1._pm_reel/apla
      else
         ! Applatissement = 0 (cercle) 
         topo%ellips%apla = 0._pm_reel
      endif
      select case(direction)
         case(1)  ! Nord 
            topo%axe_x = 0._pm_reel
         case(2)  ! Sud
            topo%axe_x = pm_pi
         case(3)  ! est
            topo%axe_x = pm_pi_sur2
         case(4)  ! Ouest 
            topo%axe_x = - pm_pi_sur2
         case default 
            write(0,*) 'Sous-programme : GS_param_msp_vers_mspro'
            write(0,*) 'Valeur de direction station non valide'
            ier = -5
            return
      end select
   else
      topo%geod%lat      =  0._pm_reel    
      topo%geod%long     =  0._pm_reel   
      topo%geod%haut     =  0._pm_reel     
      topo%ellips%r_equa =  0._pm_reel  
      topo%ellips%apla   =  0._pm_reel  
      topo%axe_x         =  0._pm_reel   
   endif

   ! Dans le cas COMPAS pour un corps autre que la Terre
   ! remplissage de la structure des paramètres d'orientation
   ! du Pole et temps sidéral.
   if ( .not. present(fichier_poletsid) ) then
      if (prec_nuta_tsid == pm_UAI_autre_modele) then
         !/ Par défaut choix de la théorie UAI2000

         if (polescompas == GS_TH_UAI1991) then
            call cps_getMouvementsPolesCorps(numpla,"UAI1991",dateref_pm,alpha0,delta0, W, dW)
         else ! cas ou polescompas
            call cps_getMouvementsPolesCorps(numpla,"UAI2000",dateref_pm,alpha0,delta0, W, dW)
         endif
         pole_tsid_planete%delta0=delta0
         pole_tsid_planete%alpha0=alpha0
         pole_tsid_planete%W=W
         pole_tsid_planete%dW=dW

      endif
   else

      ! Conversion de la struture de la date du jour en jours juliens fractionnaires
      date_jjfrac = real(dateref_pm%jour, kind=PM_REEL) &
           + dateref_pm%sec / 86400.0_pm_reel

      call gs_lire_poletsid(date_jjfrac, fichier_poletsid, numpla,&
           pole_tsid_planete%alpha0, pole_tsid_planete%delta0, &
           pole_tsid_planete%W, pole_tsid_planete%dW, ier)
      if ( ier < 0 ) then
         write(0,*) 'Sous-programme : GS_param_msp_vers_mspro'
         write(0,*) 'Erreur ',ier,' dans le sous-programme GS_lire_poletsid'
         ier = -7
         return
      endif

   endif

   ! -- Calcul des écarts entre echelles de temps
   call GS_calcul_ecart_echt(dateref_pm, ech_tref, &
                             delta_ech_tu1, delta_ech_tai, ier)
   if (ier < 0) then
      write(0,*) 'Sous-programme : GS_param_msp_vers_mspro'
      write(0,*) 'Erreur ',ier,' dans le sous-programme GS_calcul_ecart_echt'
      ier = -6
      return
   endif


 end subroutine GS_param_msp_vers_mspro


 subroutine GS_calcul_ecart_echt(date_pm,ech_temps,ecart_tu1,ecart_tai,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_calcul_ecart_echt
!
!$Resume
! Calcul des écart de date entre l'echelle ech_temps (MSP_ENUM_ECHT_TE ou MSP_ENUM_ECHT_TUC) 
! et les  echelles de temps TU1 et TAI à la date 'date'
!
!$Description
! Calcul des écart de date entre l'echelle ech_temps (MSP_ENUM_ECHT_TE ou MSP_ENUM_ECHT_TUC) 
! et les  echelles de temps TU1 et TAI à la date 'date'
!
!$Auteur
!  17/01/2003 - Philippe Bremard / Didier Semeux (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_calcul_ecart_echt(date_pm,ech_temps,ecart_tu1,ecart_tai,ier)
!.    type(tm_jour_sec) :: date_pm
!.    integer :: ech_temps
!.    real(kind=pm_reel) :: ecart_tu1,ecart_tai
!.    integer :: ier
!
!$Arguments
!>E     date_pm    :<tm_jour_sec>   Date à laquelle on souhaite calculer les écarts
!>E     ech_temps  :<integer>       Echelle de temps de la date (MSP_ENUM_ECHT_TUC, MSP_ENUM_ECHT_TE)
!>S     ecart_tu1  :<pm_reel>       Ecart entre l'echelle de temps TU1 et l'echelle 'ech-temps'
!>S     ecart_tai  :<pm_reel>       Ecart entre l'echelle de temps TAI et l'echelle 'ech-temps'
!>S     ier        :<integer>       Code retour
!
!$Remarques
!
!$Mots-cles
!  ECART TAI TUC TU1
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSPRO
   use cps_utilisateur

   IMPLICIT NONE

! Arguments

   type(tm_jour_sec),  intent(IN)   :: date_pm
   integer,            intent(IN)   :: ech_temps
   real(kind=pm_reel), intent(OUT)  :: ecart_tu1,ecart_tai
   integer ,           intent(OUT)  :: ier

! Variables locales

   type(tm_jour_sec)   :: date_tmp
   real(KIND=PM_REEL)  :: delta_te_tai
   integer             :: delta_tai_tuc

   type(tm_code_retour) :: code_retour

! Début code

   ! -- Initialisations 
   ier = 0
      
   ! -- Calcul de l'ecart TAI-TUC : delta_tai_tuc
   call cps_get_sautTAITUC(date_pm,delta_tai_tuc)
   if (MSP_gen_messages("GS_calcul_ecart_echt")) return
      

   ! -- Calcul de l'ecart TE-TAI : delta_te_tai
   call md_ech_temps(pm_TAI,date_pm,pm_TE,delta_te_tai,date_tmp,&
                     code_retour=code_retour)

   if (code_retour%valeur < 0) then
      write(0,*) 'Sous-programme : GS_calcul_ecart_echt'
      write(0,*) 'Erreur ',code_retour%valeur,' dans le sous-programme md_ech_temps2'
      ier = -4
      return
   endif

!            écart TAI-TU1       écart TE-TAI
!     ---+------------------+-------------------+----------->
!       TUC                TAI                  TE
!       TU1

   if (ech_temps == pm_TE)  then
   ! -- Ecarts avec le TE
      ecart_tu1 = - (delta_te_tai + delta_tai_tuc)    ! écart TU1 - TE
      ecart_tai = -delta_te_tai                       ! écart TAI - TE
   else 
   ! -- Ecarts avec le TUC
      ecart_tu1 = 0._PM_REEL                          ! écart TU1 - TUC
      ecart_tai = delta_tai_tuc                       ! écart TAI - TUC
   endif

  
 end subroutine GS_calcul_ecart_echt

 subroutine GS_Changer_typbul(typcoord1,param1,typcoord2,gmu,requa,&
                              apla,param2,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_Changer_typbul
!
!$Resume
!   Changement de type de coordonnées
!
!$Description
!   Changement de type de coordonnées pour l'objet GS_BULLETIN_IP
!   Appel du sous-programme mx_var de changement de type de coordonnées de la MSPRO 
!
!$Auteur
!   Florence VIVARES (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_Changer_typbul(typcoord1,param1,typcoord2,gmu,requa,&
!.                                  apla,param2,ier)
!.    integer :: typcoord1
!.    real(KIND=PM_REEL),dimension(6) :: param1
!.    integer :: typcoord2
!.    real(KIND=PM_REEL) :: gmu
!.    real(KIND=PM_REEL) :: requa
!.    real(KIND=PM_REEL) :: apla
!.    real(KIND=PM_REEL),dimension(6) :: param2
!.    integer :: ier
!
!$Arguments
!>E     typcoord1  :<integer>           Type de coordonnées en entrées (voir MSPRO)
!>E     param1     :<PM_REEL,DIM=(6)>   Coordonnées d'entrées
!>E     typcoord2  :<integer>           Type de coordonnées en sortie (voir MSPRO)
!>E     gmu        :<PM_REEL>           Mu du corps central
!>E     requa      :<PM_REEL>           Rayon a l'equateur du corps central
!>E     apla       :<PM_REEL>           Applatissement du corps central
!>E/S   param2     :<PM_REEL,DIM=(6)>   Coordonnées de sortie
!>S     ier        :<integer>           Code retour
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   use MSPRO

   implicit none


! Arguments 
   integer,                           intent(IN)    :: typcoord1
   real(KIND=PM_REEL),dimension(6),   intent(IN)    :: param1
   integer,                           intent(IN)    :: typcoord2
   real(KIND=PM_REEL),                intent(IN)    :: gmu
   real(KIND=PM_REEL),                intent(IN)    :: requa
   real(KIND=PM_REEL),                intent(IN)    :: apla
   real(KIND=PM_REEL),dimension(6),   intent(INOUT) :: param2
   integer,                           intent(OUT)   :: ier

! Variables locales

   real(KIND=PM_REEL)   :: mu, unsurapla
   type(tm_ellipsoide)  :: ellips
   type(tm_code_retour) :: code_retour,ier_loc

    character(len=PM_nom_biblio ):: nom_biblio
    character(len=PM_nom_routine):: nom_routine
    character(len=PM_identification_routine) :: id_routine
    character(len=PM_signification_code_retour) :: lib_erreur

   
! Paramètres
   !Pas d'include de parameter_mspro car on fait un use MSPRO


! Début code
   
   ! -- Initialisations
   ier = 0

   if(abs(apla) < 10e-10) then
      unsurapla = 0._pm_reel 
   else
      unsurapla = 1._PM_REEL / apla
   end if
   mu = gmu                       ! m3/s2

   ellips%r_equa = requa          ! m
   ellips%apla   = unsurapla
   
   !// -- Changement de type de bulletin MSPRO

    

     call mx_var(typcoord1,param1,typcoord2,param2,code_retour,mu=mu,ellips=ellips)

     if (code_retour%valeur < pm_OK) then
        write(0,*) 'Sous-programme : GS_changer_typbul'
        write(0,*) 'Erreur ',code_retour%valeur,' dans le sous-programme mx_var'
        call mzpro_code_retour(code_retour,nom_biblio,nom_routine,&
             id_routine,lib_erreur,ier_loc)
        write(0,*) '   -  ',lib_erreur
        ier = -1
        return
     endif

 end subroutine GS_Changer_typbul

 subroutine GS_Changer_typbulcov(typcoord1,param1,typcoord2,gmu,requa,&
                              apla,param2,ier,jacob)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_Changer_typbulcov
!
!$Resume
!   Changement de type de coordonnées
!
!$Description
!   Changement de type de coordonnées pour l'objet GS_BULCOV_IP
!   Appel du sous-programme mx_var de changement de type de coordonnées de la MSPRO 
!
!$Auteur
!   Nicolas VALLET (ATos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_Changer_typbulcov(typcoord1,param1,typcoord2,gmu,requa,&
!.                                  apla,param2,ier,jacob)
!.    integer :: typcoord1
!.    real(KIND=PM_REEL),dimension(6) :: param1
!.    integer :: typcoord2
!.    real(KIND=PM_REEL) :: gmu
!.    real(KIND=PM_REEL) :: requa
!.    real(KIND=PM_REEL) :: apla
!.    real(KIND=PM_REEL),dimension(6) :: param2
!.    integer :: ier
!.    real(KIND=PM_REEL),dimension(6,6) :: jacob
!
!$Arguments
!>E     typcoord1  :<integer>             Type de coordonnées en entrées (voir MSPRO) 
!>E     param1     :<PM_REEL,DIM=(6)>     Coordonnées d'entrées 
!>E     typcoord2  :<integer>             Type de coordonnées en sortie (voir MSPRO) 
!>E     gmu        :<PM_REEL>             Mu du corps central 
!>E     requa      :<PM_REEL>             Rayon a l'equateur du corps central 
!>E     apla       :<PM_REEL>             Applatissement du corps central 
!>E/S   param2     :<PM_REEL,DIM=(6)>     Coordonnées de sortie 
!>S     ier        :<integer>             Code retour 
!>S     jacob      :<PM_REEL,DIM=(6,6)>   Jacobienne de la transformation 
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   use MSPRO

   implicit none


! Arguments 
   integer,                           intent(IN)    :: typcoord1
   real(KIND=PM_REEL),dimension(6),   intent(IN)    :: param1
   integer,                           intent(IN)    :: typcoord2
   real(KIND=PM_REEL),                intent(IN)    :: gmu
   real(KIND=PM_REEL),                intent(IN)    :: requa
   real(KIND=PM_REEL),                intent(IN)    :: apla
   real(KIND=PM_REEL),dimension(6),   intent(INOUT) :: param2
   integer,                           intent(OUT)   :: ier
   real(KIND=PM_REEL),dimension(6,6), intent(OUT)   :: jacob
   

! Variables locales

   real(KIND=PM_REEL)   :: mu, unsurapla
   type(tm_ellipsoide)  :: ellips
   type(tm_code_retour) :: code_retour,ier_loc

    character(len=PM_nom_biblio ):: nom_biblio
    character(len=PM_nom_routine):: nom_routine
    character(len=PM_identification_routine) :: id_routine
    character(len=PM_signification_code_retour) :: lib_erreur

   
! Paramètres
   !Pas d'include de parameter_mspro car on fait un use MSPRO


! Début code
   
   ! -- Initialisations
   ier = 0
   
   if(abs(apla) < 10e-10) then
      unsurapla = 0._pm_reel
   else
      unsurapla = 1._PM_REEL / apla
   end if
   
   mu = gmu                       ! m3/s2

   ellips%r_equa = requa          ! m
   ellips%apla   = unsurapla
   
   !// -- Changement de type de bulletin MSPRO

     
     
     call mx_var(typcoord1,param1,typcoord2,param2,code_retour,mu=mu,ellips=ellips,jacob=jacob)

     if (code_retour%valeur < pm_OK) then
        write(0,*) 'Sous-programme : GS_changer_typbulcov'
        write(0,*) 'Erreur ',code_retour%valeur,' dans le sous-programme mx_var'
        call mzpro_code_retour(code_retour,nom_biblio,nom_routine,id_routine,lib_erreur,ier_loc)
        write(0,*) '   -  ',lib_erreur
        ier = -1
        return
     endif

 end subroutine GS_Changer_typbulcov



 subroutine GS_date_etutc_jjsec(jj_in,sec_in,sens,jj_out,sec_out,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_date_etutc_jjsec
!
!$Resume
!  Cette routine réalise les conversions entre date TUC et TE en utilisant la mspro
!
!$Description
!  Cette routine réalise les conversions entre date TUC et TE en utilisant la mspro
!
!$Acces
!  PUBLIC
!
!$Auteur
!  22/01/2003 - Philippe Bremard / Didier Semeux (SchlumbergerSema)
!  23/09/2004 - Reprise et adaptation de la routine developpee sous MECASPA (JF Becquaert)
!
!$Usage
!  call GS_date_etutc_jjsec(jj_in,sec_in,sens,jj_out,sec_out,ier)
!.    integer :: jj_in
!.    real*8 :: sec_in
!.    integer :: jj_out
!.    real*8 :: sec_out
!.    integer :: sens
!.    integer :: ier
!
!$Arguments
!>E     jj_in    :<integer>   Jour julien de la Date à convertir, en jour jour/secondes
!>E     sec_in   :<real*8>    Secondes dans le jour de la Date à convertir, en jour jour/secondes
!>E     sens     :<integer>   Sens de conversion :
!                                  1  : passage de date TE à la date TUC
!                                  -1 : passage de date TUC à la date TE
!>S     jj_out   :<integer>   Jour julien de la Date convertie, en jour jour/secondes
!>S     sec_out  :<real*8>    Secondes dans le jour de la Date convertie, en jour jour/secondes
!>S     ier      :<integer>   code d'erreur de la convertion
!
!$Remarques
!
!$Mots-cles
!  BULLETIN CONVERTIR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSPRO
   use cps_utilisateur
   IMPLICIT NONE

   !/ Arguments
   integer, intent(IN)                      :: jj_in
   real*8,  intent(IN)                      :: sec_in
   integer, intent(OUT)                     :: jj_out
   real*8,  intent(OUT)                     :: sec_out
   integer, intent(IN)                      :: sens
   integer, intent(OUT)                     :: ier

   !/ Variables locales
   type(tm_jour_sec)    :: date_in
   type(tm_jour_sec)    :: date_out
   type(tm_jour_sec)    :: date_inter
   real(KIND=PM_REEL)   :: reste
   
   real(pm_reel), parameter :: delta_sec_TE_TAI = 32.184_pm_reel ! ecart en secondes entre les echelles TE et TAI
   integer :: delta_TAITUC, k
   
   !-- initialisation de la structure
   date_in%jour = jj_in
   date_in%sec = sec_in
   
   date_out%jour = 0
   date_out%sec = 0._pm_reel
   
   ! -- Calcul du saut du TUC
   call cps_get_sautTAITUC(date_in,delta_TAITUC)
   if (MSP_gen_messages("GS_date_etutc_jjsec")) return
   
   if (sens == -1) then
      ! -- Conversion de la date TUC vers TE
      ! passage TUC-> TAI
      ! Calcul de la date TAI
      date_inter%jour = date_in%jour
      date_inter%sec  = date_in%sec + real(delta_TAITUC, kind=pm_reel)
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

      ! Passage TAI -> TE
      date_inter%jour = date_out%jour
      date_inter%sec  = date_out%sec + delta_sec_TE_TAI
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel
      
   else if (sens == 1) then
      ! -- Conversion de la date  TE vers TUC
      ! passage TE-> TAI
      ! Calcul de la date TAI
      date_inter%jour = date_in%jour
      date_inter%sec  = date_in%sec - delta_sec_TE_TAI
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

      ! Passage TAI -> TUC
      date_inter%jour = date_out%jour
      date_inter%sec  = date_out%sec - real(delta_TAITUC,kind=pm_reel)
      ! calcul du nombre de jours lies aux secondes
      reste = date_inter%sec / 86400._pm_reel
      k     = floor(reste)
      ! calcul de la quantite normalisee, k vaut 0 ou 1
      date_out%jour = date_inter%jour + k
      date_out%sec  = date_inter%sec - real(k, kind=pm_reel) * 86400._pm_reel

   end if

   !-- affectation de la structure de sortie
   jj_out=date_out%jour 
   sec_out=date_out%sec 
   
   ier = 0

 end  subroutine GS_date_etutc_jjsec


 subroutine GS_conv_anomalie(ex,iane,ane,ians,ans,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  GS_conv_anomalie
!
!$Resume
!  Sous-programme de conversion d'anomalie
!
!$Description
!  Sous-programme de conversion entre les 3 types d'anomalies
!  (vraie, moyenne, excentrique) 
!
!$Auteur
!  Philippe Brémard (SchlumbergerSema) (à partir de l'AMLIB) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call GS_conv_anomalie(ex,iane,ane,ians,ans,ier)
!.    real(kind=pm_reel) :: ex
!.    integer :: iane
!.    real(kind=pm_reel) :: ane
!.    integer :: ians
!.    real(kind=pm_reel) :: ans
!.    integer :: ier
!
!$Arguments
!>E     ex    :<pm_reel>   Valeur de l'excentricité
!>E     iane  :<integer>   Type d'anomalie en entrée (1:vraie, 2:moyenne, 3:excentrique)
!>E     ane   :<pm_reel>   Valeur de l'anomalie en entrée
!>E     ians  :<integer>   Type d'anomalie en sortie (1:vraie, 2:moyenne, 3:excentrique)
!>S     ans   :<pm_reel>   Valeur de l'anomalie en sortie
!>S     ier   :<integer>   Code retour
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   

   use MSPRO
   
   implicit none
   
   ! Arguments
   
   real(kind=pm_reel), intent(in)  :: ex
   integer,            intent(in)  :: iane
   real(kind=pm_reel), intent(in)  :: ane
   integer,            intent(in)  :: ians
   real(kind=pm_reel), intent(out) :: ans
   integer,            intent(out) :: ier
   
   ! Variables locales
   
   real(kind=pm_reel) :: ano_moy,ano_vrai,ano_exc
   real(kind=pm_reel) ::beta,w,arg
   real(kind=pm_reel),parameter :: epsilon = 1.e-08_PM_REEL
   type(TM_CODE_RETOUR)::code_retour
   
   ! Début du code
   
   ier = 0
   ans = -99999._PM_REEL  ! Valeur de retour en erreur

   ano_moy  = 0._pm_reel
   ano_vrai = 0._pm_reel
   ano_exc  = 0._pm_reel
   
   !// Controle des types d'anomalies en entrée et en sortie
   !// 1 : anomalie moyenne
   !// 2 : anomalie excentrique
   !// 3 : anomalie vrai
   
   
   if ( iane<1 .or. iane>3 ) then
      write(0,*) 'Sous-programme : GS_conv_anomalie'
      write(0,*) 'Type d''anomalie en entrée incorrect'
      ier = -1
      return
   endif
   
   if ( ians<1.or. ians>3 ) then
      write(0,*) 'Sous-programme : GS_conv_anomalie'
      write(0,*) 'Type d''anomalie en sortie incorrect'
      ier = -2
      return
   endif
   
   !//       Anomalie vraie en entrée
   
   if ( iane==1 ) then
      
      ano_vrai=ane
      
      ! Controle de l'excentricité
      
      if(ex>=(1._PM_REEL-epsilon).and.ex<=(1.d0+epsilon))then
         write(0,*) 'Sous-programme : GS_conv_anomalie'
         write(0,*) 'Excentricité=',ex,'cas limite de la parabole'
         ier = -6
         return
      endif
      
      
      !/ Calcul de l'anomalie excentrique
      
      if(ex.lt.(1.d0-epsilon))then
         
         ! Cas de l'ellipse
         
         beta = ex/(1._PM_REEL+sqrt(1._PM_REEL-ex*ex))
         ano_exc = ano_vrai -2._PM_REEL*atan(beta*sin(ano_vrai)/(1._PM_REEL+beta*cos(ano_vrai)))
         
      else
         
         ! Cas de l'hyperbole:
         ! on teste que l'anomalie l'anomalie vraie soit 
         ! inférieure à l'anomalie à l'infini.
         
         if(cos(ano_vrai) < (-1._PM_REEL/ex))then
            write(0,*) 'Sous-programme : GS_conv_anomalie'
            write(0,*) 'Cas hyperbole :anomalie vraie >= anomalie vraie à l''infini'
            ier = -7
            return

            arg = sqrt((ex-1._PM_REEL)/(ex+1._PM_REEL))*tan(ano_vrai/2._PM_REEL)
            
            ! Remarque:
            ! le cas arg=1 ne peut se produire car 
            ! il correspond au cas de la parabole   
            ano_exc = log((arg+1._PM_REEL)/(1._PM_REEL-arg))
            

         endif

         
      endif
      
      
      !/ Calcul de l'anomalie moyenne
      
      
      if (ex < (1._PM_REEL-epsilon)) then
         
         ! Cas de l'ellipse
         
         ano_moy = ano_exc - ex*sin(ano_exc)
         
      else
         
         ! Cas de l'hyperbole:
         
         ano_moy = ex*sinh(ano_exc) - ano_exc
         
      endif
      
      
   endif
   
   !//     Anomalie moyenne en entrée
   
   if ( iane==2 ) then
      
      ano_moy=ane
      
      ! Calcul de l'anomalie excentrique (cas général: elliptique et hyperbolique)
      
      call mv_kepler_bar(ano_moy,ex,ano_exc,code_retour)
      if(code_retour%valeur<0)then
         write(0,*) 'Sous-programme : GS_conv_anomalie'
         write(0,*) 'Erreur ',code_retour%valeur,' dans le sous-programme mv_kepler_bar'
         ier = -3
         return
      endif
      
      ! Calcul de l'anomalie vraie
      
      if(ex>=(1._PM_REEL-epsilon).and.ex<=(1.d0+epsilon))then
         write(0,*) 'Sous-programme : GS_conv_anomalie'
         write(0,*) 'Excentricité=',ex,'cas limite de la parabole'
         ier = -4
         return
      endif
      
      if (ex < (1._PM_REEL-epsilon)) then
         
         ! Cas de l'ellipse
         
         beta = ex/(1._PM_REEL+sqrt(1._PM_REEL-ex*ex))
         ano_vrai = ano_exc +2._PM_REEL*atan(beta*sin(ano_exc)/(1._PM_REEL-beta*cos(ano_exc)))
         
      else
         
         ! Cas de l'hyperbole
         
         w=sqrt((ex+1._PM_REEL)/(ex-1._PM_REEL))*tanh(ano_exc/2._PM_REEL)
         ano_vrai = 2._PM_REEL*atan(w)
         
      endif
      
   endif
   
   !//       Anomalie excentrique en entrée
   
   if ( iane==3 ) then
      
      ano_exc=ane
      
      ! Controle de l'excentricité
      
      if(ex>=(1._PM_REEL-epsilon).and.ex<=(1.d0+epsilon))then
         write(0,*) 'Sous-programme : GS_conv_anomalie'
         write(0,*) 'Excentricité=',ex,'cas limite de la parabole'
         ier = -5
         return
      endif
      
      !/ Calcul de l'anomalie vraie
      
      if (ex < (1._PM_REEL-epsilon)) then
         
         ! Cas de l'ellipse
         
         beta = ex/(1._PM_REEL+sqrt(1._PM_REEL-ex*ex))
         ano_vrai = ano_exc +2._PM_REEL*atan(beta*sin(ano_exc)/(1._PM_REEL-beta*cos(ano_exc)))
         
      else
         
         ! Cas de l'hyperbole
         
         w=sqrt((ex+1._PM_REEL)/(ex-1._PM_REEL))*tanh(ano_exc/2._PM_REEL)
         ano_vrai = 2._PM_REEL*atan(w)
         
      endif
      
      !/ Calcul de l'anomalie moyenne
      
      if (ex < (1._PM_REEL-epsilon)) then
         ! Cas de l'ellipse
         ano_moy = ano_exc - ex*sin(ano_exc)
      else
         ! Cas de l'hyperbole
         ano_moy = ex*sinh(ano_exc) - ano_exc
      endif
   endif
   
   
   !// Affectation de l'anomalie en sortie
   
   select case ( ians )
   case (1)
      ans = ano_vrai
   case (2)
      ans = ano_moy
   case (3)
      ans = ano_exc
   end select
   
 end subroutine GS_conv_anomalie
 
subroutine gscovaff_ip(lig,forme,uni,tabuni,cov,covd)
!*******************************************************************************
!$<AM-V2.0>
!
!$Nom
!  gscovaff_ip
!
!$Resume
!      conversion d'unités à l'affichage
!
!$Description
!      convertit la matrice en fonction de l'unité demandée pour chaque ligne
!
!$Usage
!  call gscovaff_ip(lig,forme,uni,tabuni,cov,covd)
!.    integer :: lig
!.    integer :: forme
!.    integer, dimension(6) :: uni
!.    character(len=*), dimension(3,6) :: tabuni
!.    real(kind=PM_REEL), dimension(6,6) :: cov
!.    real(kind=PM_REEL), dimension(6,6) :: covd
!
!$Remarques
!     Après un changement d'unités ou à l'écriture
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSPRO

  implicit none

  !-- Parametres

  integer, intent(in) :: lig
  integer, intent(in) :: forme
  integer, dimension(6), intent(in) :: uni
  character(len=*), dimension(3,6), intent(in) :: tabuni
  real(kind=PM_REEL), dimension(6,6), intent(in) :: cov
  real(kind=PM_REEL), dimension(6,6), intent(out) :: covd


  !-- Variables locales

  integer :: i
  integer :: ier

  integer :: AMv_unit_convert

  ! Conversion d'unites 
  if(forme.eq.1)then
     do i=1,6
        if(tabuni(1,i).ne."") then
           ier=AMv_unit_convert(tabuni(2,i),cov(i,lig),&
                tabuni(uni(i),i),covd(i,lig))
           if (ier < 0) then
              write(0,*) "Sous-programme : gscovaff_ip"
              write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
              
              return
           end if
        else
           covd(i,lig)=cov(i,lig)
        endif
        if(tabuni(1,lig).ne."") then
           ier=AMv_unit_convert(tabuni(2,lig),covd(i,lig),&
                tabuni(uni(lig),lig),covd(i,lig))
           if (ier < 0) then
              write(0,*) "Sous-programme : gscovaff_ip"
              write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
           
              return
           end if

        endif
     enddo
  else
     do i=1,6
        covd(i,lig)=cov(i,lig)
     enddo
  endif

  return
end subroutine gscovaff_ip

subroutine gssigaff_ip(uni,tabuni,sig,sigd)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gssigaff_ip
!
!$Resume
!      conversion d'unités à l'affichage
!
!$Description
!      convertit le vecteur sig (écart-types) en fonction de l'unité
!      demandée pour chaque ligne
!
!$Usage
!  call gssigaff_ip(uni,tabuni,sig,sigd)
!.    integer,dimension(6) :: uni
!.    character(len=*), dimension(3,6) :: tabuni
!.    real(kind=PM_REEL), dimension(6) :: sig
!.    real(kind=PM_REEL), dimension(6) :: sigd
!
!$Remarques
!     Après un changement d'unités ou à l'écriture
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MSPRO

  implicit none

  !-- Parametres
  integer,dimension(6), intent(in) :: uni
  character(len=*), dimension(3,6), intent(in) :: tabuni
  real(kind=PM_REEL), dimension(6), intent(in) :: sig
  real(kind=PM_REEL), dimension(6), intent(out) :: sigd

  integer :: i
  integer :: ier, AMv_unit_convert

  ! Conversion d'unites !
  do i=1,6
     if(tabuni(1,i).ne."") then
        ier=AMv_unit_convert(tabuni(2,i),sig(i),tabuni(uni(i),i),sigd(i))
        if (ier < 0) then
           write(0,*) "Sous-programme : gssigaff_ip"
           write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
        
           return
        end if
     else
        sigd(i)=sig(i)
     endif
  enddo

  return
end subroutine gssigaff_ip


  subroutine gscovsai_ip(lig,forme,uni,tabuni,cov,covd)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gscovsai_ip
!
!$Resume
!      Conversion d'unites a la saisie et symetrisation
!
!$Description
!      Convertit la matrice en mémoire suivant l'unité affichée et la valeur
!      saisie pour chaque ligne et la symétrise
!
!$Usage
!  call gscovsai_ip(lig,forme,uni,tabuni,cov,covd)
!.    integer :: lig
!.    integer :: forme
!.    integer, dimension(6) :: uni
!.    character(len=*), dimension(3,6) :: tabuni
!.    real(kind=PM_REEL), dimension(6,6) :: cov
!.    real(kind=PM_REEL), dimension(6,6) :: covd
!
!$Arguments
!>E     lig     :<integer>             indice de la ligne concernée       
!>E     forme   :<integer>             forme de la matrice de covariance        
!                                    (Corrélations/Covariance).
!>E     uni     :<integer,DIM=(6)>     tableau d'indices des unités d'affichage    
!>E     tabuni  :<LEN=*,DIM=(3,6)>     tableau des tableaux d'unités possibles    
!>E/S   cov     :<PM_REEL,DIM=(6,6)>   matrice de covariance en mémoire  
!>E/S   covd    :<PM_REEL,DIM=(6,6)>   matrice de covariance à afficher  
!
!$Remarques
!     Après une modification ou une lecture
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use MSPRO
    
      implicit none
      
      !-- Parametres

      integer, intent(in) :: lig
      integer, intent(in) :: forme
      integer, dimension(6), intent(in) :: uni
      character(len=*), dimension(3,6), intent(in) :: tabuni
      real(kind=PM_REEL), dimension(6,6), intent(inout) :: cov
      real(kind=PM_REEL), dimension(6,6), intent(inout) :: covd
      

      !-- Variables locales
      integer :: i
      integer :: ier, AMv_unit_convert

      ! Boucle sur toute les colonnes !
      do i=1,6
      
         ! Test coherance !
         if(lig.eq.i.and.covd(i,lig).lt.0.) then
            call gsmes("Une variance ne peut pas etre negative",2)
            covd(i,lig)=0.
         endif
         if (lig.ne.i.and.forme.eq.2.and. &
            (covd(i,lig).lt.-1..or.covd(i,lig).gt.1.))then
            call gsmes("Une correlation est toujours comprise entre -1 et 1",2)
            covd(i,lig)=0.
         endif
      
         ! Conversion d'unites !
         if(forme.eq.1)then
            if(tabuni(1,i).ne."") then
               ier=AMv_unit_convert(tabuni(uni(i),i),covd(i,lig),&
               tabuni(2,i),cov(i,lig))
               if (ier < 0) then
                  write(0,*) "Sous-programme : gscovsai_ip"
                  write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
                  
                  return
               end if
               
            else
               cov(i,lig)=covd(i,lig)
            endif
            if(tabuni(1,lig).ne."") then
               ier=AMv_unit_convert(tabuni(uni(lig),lig),cov(i,lig),&
               tabuni(2,lig),cov(i,lig))
               
               if (ier < 0) then
                  write(0,*) "Sous-programme : gscovsai_ip"
                  write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
                  
                  return
               end if

            endif
         else
            cov(i,lig)=covd(i,lig)
         endif
      
         ! Symetrisation !
         cov(lig,i)=cov(i,lig)
      enddo

      return
    end subroutine gscovsai_ip

    subroutine gssigsai_ip(uni,tabuni,sig,sigd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gssigsai_ip
!
!$Resume
!      Conversion d'unites a la saisie 
!
!$Description
!      Convertit le vecteur sigma en mémoire suivant l'unité affichée et la valeur
!      saisie.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gssigsai_ip(uni,tabuni,sig,sigd)
!.    integer, dimension(6) :: uni
!.    character(len=*), dimension(3,6) :: tabuni
!.    real(kind=PM_REEL), dimension(6) :: sig
!.    real(kind=PM_REEL), dimension(6) :: sigd
!
!$Remarques
!     Après une modification ou une lecture
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!/
      
      use MSPRO

      implicit none

      !-- Parametres
      
      integer, dimension(6), intent(in) :: uni
      character(len=*), dimension(3,6), intent(in) :: tabuni
      real(kind=PM_REEL), dimension(6), intent(inout) :: sig
      real(kind=PM_REEL), dimension(6), intent(inout) :: sigd
      

      !-- Variables locales
      
      integer :: i,ier, AMv_unit_convert
      
      ! Boucle sur toute les colonnes !
      do i=1,6
         
         ! Test coherance !
         if(sigd(i).lt.0.) then
            call gsmes("Un ecart type ne peut pas etre negatif",2)
            sigd(i)=0.
         endif
         
         ! Conversion d'unites !
         if(tabuni(1,i).ne."") then
            ier=AMv_unit_convert(tabuni(uni(i),i),sigd(i),tabuni(2,i),sig(i))
            if (ier < 0) then
               write(0,*) "Sous-programme : gssigsai_ip"
               write(0,*) "Erreur interne lors de l'appel à AMv_unit_convert"
               
               return
            end if

         else
            sig(i)=sigd(i)
         endif
      enddo

      return
    end subroutine gssigsai_ip


  subroutine gscovreploc(typrep,typrep_old,forme,param,cov,sig)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gscovreploc
!
!$Resume
!      Changement vers les reperes orbitaux locaux
!
!$Description
!      Changement de ou vers les reperes orbitaux locaux
!
!$Usage
!  call gscovreploc(typrep,typrep_old,forme,param,cov,sig)
!.    integer :: typrep
!.    integer :: typrep_old
!.    integer :: forme
!.    real(kind=PM_REEL), dimension(6) :: param
!.    real(kind=PM_REEL), dimension(6,6) :: cov
!.    real(kind=PM_REEL), dimension(6) :: sig
!
!$Arguments
!>E     typrep      :<integer>             (0,1,2) Type de repère (repère bulletin, QSW,TNW)
!>E     typrep_old  :<integer>             
!>E     forme       :<integer>             (0,1) Forme (corrélation / covariance)
!>E     param       :<PM_REEL,DIM=(6)>     Paramètres cartésiens 
!>S     cov         :<PM_REEL,DIM=(6,6)>   Covariance (ou corrélation) 
!>S     sig         :<PM_REEL,DIM=(6)>     Sigma  
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!/

    use MSPRO

    implicit none
      
    !-- DECLARATION DES ARGUMENTS
    integer, intent(in) :: typrep
    integer, intent(in) :: typrep_old
    integer, intent(in) :: forme
    real(kind=PM_REEL), dimension(6), intent(in) :: param
    
    real(kind=PM_REEL), dimension(6,6), intent(out) :: cov
    real(kind=PM_REEL), dimension(6), intent(out) :: sig
    
    !-- DECLARATION DES VARIABLES LOCALES
    real(kind=PM_REEL), dimension(6,6) :: Pmat
    real(kind=PM_REEL), dimension(6) :: XX
    real(kind=PM_REEL), dimension(6,6) :: cov_local
    integer :: ier
    
    ! POur éviter les maj intempestives
    XX = param
    if (typrep.ne.typrep_old) then
       call gscovfor_ip(forme,1,cov,sig)
       if ( typrep_old.eq. 0 .AND. (typrep.eq.1 .OR. typrep.eq.2) ) then
          call cov_reploc_car(cov,XX,cov_local,typrep,-1,Pmat,ier)
          cov = cov_local
       endif
       if ( (typrep_old.eq.1 .OR. typrep_old.eq.2) .AND. typrep.eq.0 ) then
          call cov_reploc_car(cov,XX,cov_local,typrep_old,1,Pmat,ier)
          cov = cov_local
       endif
       if ( (typrep_old.eq.1 .OR. typrep_old.eq.2) .AND. (typrep.eq.1 .OR. typrep.eq.2) ) then
          call cov_reploc_car(cov,XX,cov_local,typrep_old,1,Pmat,ier)
          cov = cov_local
          call cov_reploc_car(cov,XX,cov_local,typrep,-1,Pmat,ier)
          cov = cov_local
       endif
       call gscovfor_ip(1,forme,cov,sig)
    endif

    return
  end subroutine gscovreploc

subroutine cov_reploc_car(covi,XX,covf,typreploc,isens,Pmat,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cov_reploc_car
!
!$Resume
!   Conversion d'une covariance de qsw ou tnw vers un repère quelconque
!   et inversement
!
!$Description
!            Convertit une matrice de covariance exprimée en cartésien
!            dans un repère local ( qsw ou tnw ) vers un repère de base
!            et inversement
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cov_reploc_car(covi,XX,covf,typreploc,isens,Pmat,ier)
!.    real(KIND=PM_REEL),DIMENSION(6,6) :: covi
!.    real(KIND=PM_REEL),DIMENSION(6) :: XX
!.    real(KIND=PM_REEL),DIMENSION(6,6) :: covf, Pmat
!.    integer :: typreploc
!.    integer :: isens
!.    integer :: ier
!
!$Arguments
!>E     covi       :<PM_REEL,DIM=(6,6)>   matrice de covariance cartésienne 
!                                    initiale dans le repère local
!>E     XX         :<PM_REEL,DIM=(6)>     Point ou l'on fait la transformation
!                                    (en coordonnées cartésiennes)
!>S     covf       :<PM_REEL,DIM=(6,6)>   matrice de covariance cartésienne dans
!                                    le repère finalcov_reploc_car
!>E     typreploc  :<integer>             type de repere orbital local =1 QSW, =2 TNW
!>E     isens      :<integer>             orbital local
!>S     Pmat       :<PM_REEL,DIM=(6,6)>   Matrice de passage associée
!>S     ier        :<integer>             Code erreur  
!
!$Include
!
!$Auteur
!       J Rizzi (Juillet 2003)
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSPRO
!  use amlib

  implicit none

  !------------------------------
  !	ARGUMENTS
  !------------------------------


  real(KIND=PM_REEL),DIMENSION(6,6),intent(in)  :: covi
  real(KIND=PM_REEL),DIMENSION(6),  intent(in)  :: XX
  real(KIND=PM_REEL),DIMENSION(6,6),intent(out) :: covf, Pmat
  integer, intent(in)  :: typreploc
  integer, intent(in)  :: isens
  integer, intent(out) :: ier

  !-------------------------------
  !	VARIABLES LOCALES
  !-------------------------------

  real(KIND=PM_REEL),DIMENSION (6,6) :: co,TPmat
  real(KIND=PM_REEL),DIMENSION (3,3) :: P
  real(KIND=PM_REEL),DIMENSION (3)   :: xsat,vsat,v1,v2,v3
  TYPE(tm_code_retour) :: code_retour
  integer :: i

  !-------------------------------
  !	INITIALISATION
  !-------------------------------

  xsat(1:3)=XX(1:3)
  vsat(1:3)=XX(4:6)
  Pmat(:,:)=0._PM_REEL

  !------------------------------------
  ! Calcul de la matrice de passage
  !------------------------------------

  !   Passage au repere orbital local
  if (typreploc==1) then  ! QSW
     call mo_def_qsw(xsat,vsat,v1,v2,v3,code_retour)
  elseif (typreploc==2) then  ! TNW
     call mo_def_tnw(xsat,vsat,v1,v2,v3,code_retour)
  endif

  if (code_retour%valeur /= 0 ) then
     ier = -1
     return
  endif
  !   Calcul de la matrice de passage pour la covariance
  do i=1,3
     P(1,i) = v1(i)
     P(2,i) = v2(i)
     P(3,i) = v3(i)
  enddo

  Pmat(:,:) = 0.0_PM_REEL
  Pmat(1:3,1:3)=P(1:3,1:3)
  Pmat(4:6,4:6)=P(1:3,1:3)

  !--------------------------------------------------   
  ! Calcul de la transposee de la matrice de passage
  !--------------------------------------------------

  TPmat = TRANSPOSE(Pmat)

  !----------------------------------
  ! calcul de la covariance
  !----------------------------------

  if (isens==1) then
     ! de orbital local vers cartesien
     ! on fait le produit tP.cov.P car l'inverse de la matrice est la transposee
     Co=matmul(Covi,Pmat)
     Covf=matmul(TPmat,co)

  elseif (isens==-1) then
     ! de cartesien vers orbital local
     ! on fait le produit P.cov.tP
     Co=matmul(Covi,TPmat)
     Covf=matmul(Pmat,co)
  endif


  return

end subroutine cov_reploc_car


subroutine gscovfor_ip(formei,formeo,cov,sig)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gscovfor_ip
!
!$Resume
!  Changement de forme de la covariance
!
!$Description
!  Changement de forme de la covariance
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gscovfor_ip(formei,formeo,cov,sig)
!.    integer :: formei
!.    integer :: formeo
!.    real(KIND=PM_REEL),DIMENSION(6,6) :: cov 
!.    real(KIND=PM_REEL), dimension(6) :: sig
!
!$Arguments
!>E     formei  :<integer>             forme d'origine (0=corrélation, 1=covariance)
!>E     formeo  :<integer>             nouvelle forme
!>S     cov     :<PM_REEL,DIM=(6,6)>   matrice de covariance ou corrélation
!>S     sig     :<PM_REEL,DIM=(6)>     sigma
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSPRO
  implicit none
   
   
  !-- DECLARATION DES ARGUMENTS
  integer, intent(in) :: formei
  integer, intent(in) :: formeo
  real(KIND=PM_REEL),DIMENSION(6,6),intent(out) :: cov       
  real(KIND=PM_REEL), dimension(6), intent(out) :: sig
      
  !-- DECLARATION DES VARIABLES LOCALES
  real(kind=PM_REEL):: eps, uneps, un
  integer :: message
  integer :: ii
  integer :: jj
  
  !! declaration des constantes locales
  eps=1.e-15_pm_reel
  un = 1_pm_reel


  ! Test si les formes sont differentes
  if(formei.ne.formeo)then
     if(formeo.eq.1)then
        ! passage en matrice de covariance
        do ii=1,6
           do jj=ii,6
              ! symmétrisation des covariances 
              cov(ii,jj) = (cov(ii,jj) + cov(jj,ii))/2_PM_reel
              ! raz des "presque 0"
              if (abs(cov(ii,jj)) < eps) cov(ii,jj) = 0_PM_REEL
              cov(ii,jj)=cov(ii,jj)*sig(ii)*sig(jj)
              cov(jj,ii)=cov(ii,jj)
           enddo
           !---------
        enddo
     else
        ! on ne prévient qu'une fois des corrélation > 1
        message=0
        eps=1.e-10_pm_reel
        uneps = un + eps
        
        ! passage en matrice de correlation normalisee
        do ii=1,6
           sig(ii)=sqrt(abs(cov(ii,ii)))
           cov(ii,ii)=1._pm_reel
           if(abs(sig(ii)).lt.eps) sig(ii)=0._pm_reel
        enddo
        do ii=1,6
           do jj=1,6
              if(ii.ne.jj)then                 
                 ! raz des "presque 0"
                 if (( abs(sig(ii)) < eps).or.( abs(sig(jj)) < eps).or.&
                    ( abs( cov(ii,jj)) < eps)) then
                    cov(ii,jj)=0._pm_reel
                 else
                    cov(ii,jj)=cov(ii,jj)/(sig(ii)*sig(jj))
                 endif
                 ! raz à 1 des presque 1 / -1 dépassant 1 / -1
                 if ((cov(ii,jj)> un).and.(cov(ii,jj) <  uneps) ) cov(ii,jj)=1_pm_reel
                 if ((cov(ii,jj)<-un).and.(cov(ii,jj) > -uneps) ) cov(ii,jj)=-1_pm_reel
                 !sinon
                 if((abs(cov(ii,jj)) >= 1._pm_reel).and.(message.eq.0)) then
                    call gsmes("Attention correlation <-1 ou >1",2)
                    message=1
                 endif
              endif
           enddo
        enddo
     endif
  endif
  
end subroutine gscovfor_ip


subroutine gs_getListModPotentiel(corps, base_potentiel, modeles, degres, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_getListModPotentiel
!
!$Resume
!
!$Description
! donne la liste des modeles de potentiel a partir du code NAIF du corps
! d'interet et du format de potentiel
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_getListModPotentiel(corps, base_potentiel, modeles, degres, nb)
!.    integer :: corps, base_potentiel
!.    character(LEN=80),DIMENSION(100) :: modeles
!.    integer, DIMENSION(100) :: degres
!.    integer :: nb
!
!$Arguments
!>E     corps           :<integer>             
!>E     base_potentiel  :<integer>             
!>S     modeles         :<LEN=80,DIM=(100)>    
!>S     degres          :<integer,DIM=(100)>   
!>S     nb              :<integer>             
!
!$Common
!
!$Routines
!- cps_lirePotentielGRGS
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!- cps_potentiel
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

  use cps_utilisateur
  use cps_potentiel

  implicit none

  !-- DECLARATION DES ARGUMENTS
  integer, intent(in) :: corps, base_potentiel
  character(LEN=80),DIMENSION(100),intent(out) :: modeles
  integer, DIMENSION(100), intent(out) :: degres
  integer, intent(out) ::nb

  !-- DECLARATION DES VARIABLES LOCALES
  character(len=cps_maxlg), dimension(:), pointer :: modelesCorps
  integer :: val, ind, trouve, ordremax
  character(len = 256) :: tmp, fichier
  real(kind = pm_reel) :: jdeb, requa, apla, mu, vrot
  real(KIND=PM_REEL), dimension(:,:), pointer :: C, S
  logical :: denorm 

  !-- INITIALISATION
  denorm = .false.
  jdeb = 0._pm_reel
  trouve = CPS_ERR_DEF
  do ind =1, size(modeles)
     modeles(ind)=""
     degres=0
  end do
  ! Initialisation des pointeurs
  nullify(C)
  nullify(S)
  nullify(modelesCorps)

  !-- RECUPERATION DE LA LISTE DES MODELES DE POTENTIEL

  nb = 0

  !-- recuperation de tous les fichiers de modeles de potentiel
  trouve = cps_getListModelesCorps(corps, "potentiel", modelesCorps, base_potentiel)
  if (trouve == CPS_OK) then
     do ind = 1, size(modelesCorps)           
        !-- on recupere le modele et le chemin du fichier ou il est
        trouve = cps_requete("modeles_potentiel", trim(modelesCorps(ind)), "*",&
             "modele_potentiel", tmp)
        if (trouve == CPS_OK) then
           modeles(ind) = trim(tmp)
        else
           return
        end if
        ! fichier du modèle
        ! .true. indique que l'on veut le chemin absolu.
        trouve = cps_getFichierModele("potentiel", trim(modelesCorps(ind)), fichier, .true.)
        ! format du fichier
        val =0
        trouve = cps_requete("modeles_potentiel", trim(modelesCorps(ind)), "*",&
             "format", val)

        if (val==1) then
           !-- le fichier est au format GRGS
           !-- on recupere le degre maximale correspondant au fichier
           call cps_lirePotentielGRGS(fichier, jdeb, denorm, C, S, requa, apla, &
                mu, vrot, degres(ind), ordremax)
        else 
           degres(ind) = 0
        end if

        nb = nb+1
     end do
     
     ! liberation memoire
     if (associated(modelesCorps)) then
        deallocate(modelesCorps)
     endif
     if (associated(C)) then
        deallocate(C)
     endif
     if (associated(S)) then
        deallocate(S)
     endif
     
  end if

end  subroutine gs_getListModPotentiel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine gs_getListModVent(corps, typevent,base_vent, modeles, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_getListModVent
!
!$Resume
!
!$Description
! donne la liste des modeles de vent a partir du code NAIF du corps
! d'interet
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_getListModVent(corps, typevent,base_vent, modeles, nb)
!.    integer :: corps, typevent
!.    integer :: base_vent
!.    character(LEN=80),DIMENSION(100) :: modeles
!.    integer :: nb
!
!$Arguments
!>E     corps      :<integer>            code corps COMPAS  
!>E     typevent   :<integer>            type de vent  
!>E     base_vent  :<integer>            indique si on se limite à la base locale,
!                                      ou de référence (défaut 0 : les 2)
!>S     modeles    :<LEN=80,DIM=(100)>   liste des modèles
!>S     nb         :<integer>            nombre de modèles dans "modeles"
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
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

  use cps_utilisateur

  implicit none

  !-- DECLARATION DES ARGUMENTS
  integer, intent(in) :: corps, typevent
  integer, intent(in) :: base_vent
  character(LEN=80),DIMENSION(100),intent(out) :: modeles
  integer, intent(out) ::nb


  !-- DECLARATION DES VARIABLES LOCALES
  character(len=cps_maxlg), dimension(:), pointer :: modelesCorps => NULL()
  integer :: ind, trouve, ii, nb0, stat
  character(len = 256) :: tmp

  !-- INITIALISATION
  if (associated(modelesCorps)) deallocate(modelesCorps, stat=stat)
  trouve = CPS_ERR_DEF
  do ind =1, 100
     modeles(ind)=""
  end do

  !-- RECUPERATION DE LA LISTE DES MODELES DE VENT

  nb = 0 !nombre de modeles

  !-- recuperation de tous les fichiers de modeles de potentiel
  trouve = cps_getListModelesCorps(corps, "vent", modelesCorps, base_vent) 
  if (MSP_gen_messages("gs_getListModVent")) return

  ! On réduit à la taille des tableau disponibles
  nb0=size(modelesCorps)  
  if (nb0>100) nb0=100

  ii = 0
  if (trouve == CPS_OK) then
     do ind = 1, nb0
        ! On sélectionne les modèles du bon type (ATM ou ARPEGE)
        tmp = ""
        trouve = cps_requete("modeles_vent", trim(modelesCorps(ind)), "*",&
             "typemodele", tmp)
        if (typevent==1.and. tmp.eq."ATM") then
           ii = ii+1
           modeles(ii)=trim(modelesCorps(ind))
        endif
        if (typevent==2.and. tmp.eq."ARPEGE") then
           ii = ii+1
           modeles(ii)=trim(modelesCorps(ind))
        endif
        nb =nb+1
     end do
  end if
  nb = ii

  ! liberation memoire
  if (associated(modelesCorps)) deallocate(modelesCorps, stat=stat)
     
end  subroutine gs_getListModVent

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine gs_ps_codeCorps(nom, code)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_ps_codeCorps
!
!$Resume
!
!$Description
! retrouve le code du corps d'intéret à partir de son nom
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_ps_codeCorps(nom, code)
!.    character(len=*) :: nom
!.    integer :: code
!
!$Arguments
!>E     nom   :<LEN=*>     nom du corps
!>S     code  :<integer>   code COMPAS
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_utilisateur
  use eph_util

  implicit none  

  !-- DECLARATION DES ARGUMENTS
  character(len=*), intent(in) :: nom
  integer, intent(out) ::code

  !-- DECLARATION DES VARIABLES LOCALES
  integer, dimension(:), pointer :: codes
  character(len=80) :: nomminuscule

  !-- CONVERSION DU NOM EN CODE
  code = -1
  ! Init du pointeur 
  nullify(codes)

  nomminuscule=nom
  call eph_conv_minusc(nomminuscule)
  call cps_getCorps("nom_fr",trim(nomminuscule),codes)
  if (size(codes)>0) then
     ! corps trouvé
     code = codes(1)
  else
     ! corps pas trouvé
     call cps_getCorps("nom_en",trim(nomminuscule),codes)
     if (size(codes)>0) then
        ! corps trouvé
        code = codes(1)
     else
        ! corps pas trouvé
        call cps_getCorps("nom_id",trim(nomminuscule),codes)
        if (size(codes)>0) then
           ! corps trouvé
           code = codes(1)
        endif
     end if
  end if
  
  !libération de la memoire
  if( associated(codes)) then
     deallocate(codes)
  end if

end  subroutine gs_ps_codeCorps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine gs_sauvertableau(nomfichier,listecodes,nbcorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_sauvertableau
!
!$Resume
!
!$Description
! ecrit dans nomfichier la liste des codes des corps centraux
!
!$Auteur
! Camille Hue
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_sauvertableau(nomfichier,listecodes,nbcorps)
!.    character(len=*) :: nomfichier
!.    integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT) :: listecodes
!.    integer :: nbcorps
!
!$Arguments
!>E     nomfichier  :<LEN=*>                                      Nom du fichier                               
!>E     listecodes  :<integer,DIM=(GS_CPS_NB_MAX_CORPS_SELECT)>   tableau à sauver 
!>E     nbcorps     :<integer>                                    Nombre d'éléments dans le tableau
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

#include "acces_F.h"
#include "parameter_gslib.h"

  !-- DECLARATION DES ARGUMENTS
  character(len=*), intent(in) :: nomfichier
  integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT), intent(in) ::listecodes
  integer, intent(in) :: nbcorps

  !-- Variables locales --!
  integer :: ii, acces, ier

  ! Remarques : on gère les erreurs sur les fonctions d'accès fichier
  acces = acc_open()
  if (acces < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_sauver_tableau"
  end if
  ! Ecriture du nombre de corps 
  ier = acc_puti(acces,"Nb_corps",nbcorps)
  ! Creation d'une zone de memoire
  ier = acc_create(acces,"Tableau_codes",ACC_TABL,"")
  ! Acces au tableau cree
  ier = acc_select(acces,"Tableau_codes",ACC_TABL)

  ! On boucle pour ecrire toute la liste
  do ii=1,nbcorps
     ! Positionnement de l'index du tableau
     ier = acc_set_index(acces,ii)
     ! Ecriture du code 
     ier = acc_puti(acces,ACC_INDEX,listecodes(ii))
  end do

  ! Fermeture du tableau
  ier = acc_select_end(acces)

  ! On ecrit dans le fichier 
  ier = acc_connect(acces,nomfichier,ACC_W)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_sauver_tableau"
  end if

  ier = acc_write(acces,ACC_ALL)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_sauver_tableau"
  end if

  ier = acc_close(acces)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_sauver_tableau"
  end if

end  subroutine gs_sauvertableau

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine gs_liretableau(nomfichier,listecodes,nbcorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_liretableau
!
!$Resume
!
!$Description
! lit dans nomfichier la liste des codes des corps centraux
!
!$Auteur
! che
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_liretableau(nomfichier,listecodes,nbcorps)
!.    character(len=*) :: nomfichier
!.    integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT) :: listecodes
!.    integer :: nbcorps
!
!$Arguments
!>E     nomfichier  :<LEN=*>                                      Nom du fichier
!>S     listecodes  :<integer,DIM=(GS_CPS_NB_MAX_CORPS_SELECT)>   tableau lu
!>S     nbcorps     :<integer>                                    Nombre d'éléments dans le tableau 
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

#include "acces_F.h"
#include "parameter_gslib.h"

  !-- DECLARATION DES ARGUMENTS
  character(len=*), intent(in) :: nomfichier
  integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT), intent(out) ::listecodes
  integer, intent(out) :: nbcorps

  !-- Variables locales --!
  integer :: ii, acces, ier

  acces = acc_open()
  if (acces < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_lire_tableau"
  end if

  ! On lit dans le fichier
  ier = acc_connect(acces,nomfichier,ACC_R)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_lire_tableau"
  end if

  ier = acc_read(acces,ACC_ALL)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_lire_tableau"
  end if

  ! Ecriture du nombre de corps
  ier = acc_geti(acces,"Nb_corps",nbcorps)
  ! Acces au tableau
  ier = acc_select(acces,"Tableau_codes",ACC_TABL)

  ! On boucle pour ecrire toute la liste
  do ii=1,nbcorps
     ! Positionnement de l'index du tableau
     ier = acc_set_index(acces,ii)
     ! Ecriture du code
     ier = acc_geti(acces,ACC_INDEX,listecodes(ii))
  end do

  ! Fermeture du tableau
  ier = acc_select_end(acces)
  ier = acc_close(acces)
  if (ier < 0) then
     write (0,*) "INTERNAL ERROR : Erreur d'accès dans gs_lire_tableau"
  end if


end  subroutine gs_liretableau

subroutine gs_get_nom_astre(code,nom,langue)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_get_nom_astre
!
!$Resume
!  Trouve le nom d'un corps associe a un code
!
!$Description
!    Trouve le nom d'un corps associe a un code 
!     DM-539: Recherche du nom dans la base compas a partir d'un code!
!
!$Auteur
!  Camille HUE (Atos origin)  
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_get_nom_astre(code,nom,langue)
!.    	integer :: code 
!.    character(len=*) :: nom, langue
!
!$Arguments
!>E/S   code    :<integer>   code COMPAS
!>E/S   nom     :<LEN=*>     Nom COMPAS (fonction de langue)
!>E/S   langue  :<LEN=*>     langue (fr ou en)
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use cps_acces

	implicit none


!	ARGUMENTS
	integer :: code        
        character(len=*) :: nom, langue

!	VARIABLES 

!     Appel de la fonction compas qui renvoit le nom du corps
      call cps_codenom(code,nom,langue)

!     Si le corps n'a pas ete trouve on renvoit corpsXXX ou XXX est le code
      if (trim(nom).eq."") then
         write(nom,'(a,i3)') "corps",code
      endif

    end subroutine gs_get_nom_astre
    
    subroutine gs_get_nom_rep(code,nom,nomdom)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_get_nom_rep
!
!$Resume
!  Trouve le nom d'un repere associe a un code de corps
!
!$Description
!    Trouve le nom d'un repere associe a un code de corps
!     DM-539: Recherche du code parmi les planetes du systeme solaire
!             et retourne le nom du repere dans la langue, sinon
!             appelle COMPAS
!
!$Auteur
!  Camille HUE (Atos origin)  
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_get_nom_rep(code,nom,nomdom)
!.    	integer :: code 
!.    character(len=*) :: nom, nomdom
!
!$Arguments
!>E/S   code    :<integer>   code COMPAS
!>E/S   nom     :<LEN=*>     Nom COMPAS (fonction de langue)
!>E/S   nomdom  :<LEN=*>     Nom de domaine de traduction
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use cps_acces
        use MSPRO
        use ephem

	implicit none


!	ARGUMENTS
	integer :: code        
        character(len=*) :: nom, nomdom

!	VARIABLES 
        integer :: lenn
        character(len=2) :: langue

!     Recherche du code parmi les planetes
      if (code.eq.eph_mercure) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_MERCURE")
      elseif (code.eq.eph_venus) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_VENUS")
      elseif (code.eq.eph_terre) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_TERRE")
      elseif (code.eq.eph_mars) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_MARS")
      elseif (code.eq.eph_jupiter) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_JUPITER")
      elseif (code.eq.eph_saturne) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_SATURNE")
      elseif (code.eq.eph_uranus) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_URANUS")
      elseif (code.eq.eph_neptune) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_NEPTUNE")
      elseif (code.eq.eph_pluton) then
         nom  = DOMtraduire(nomdom,"GS_REP_PLA_PLUTON")
      else
         ! Le corps n est pas une planete donc on cherche dans la base COMPAS
         ! Langue (à partir du nom du domaine)
         lenn=len_trim(nomdom)
         if(lenn>2) langue=nomdom(lenn-1:lenn)
         if (langue.ne."fr".and.langue.ne."en") langue="fr"
         call cps_codenom(code,nom,langue)

         !     Si le corps n'a pas ete trouve on renvoit corpsXXX ou XXX est le code
         if (trim(nom).eq."") then
            write(nom,'(a,i3)') "corps",code
         endif
      endif

    end subroutine gs_get_nom_rep

    subroutine gs_get_donnees_astre(nomdom,donnees_astre,code_astre,n_astre,type_requete,mode_bavard)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_get_donnees_astre
!
!$Resume
!  Initialisation du tableau donnees_astre des constantes de corps
!
!$Description
!   Permet le tableau des donnees nécessaire pour les bulletins
!      d apres la base de donnees COMPAS en fonction de la théorie
!   La théorie est la théorie courante qui doit avoir été fixée avant
!
!$Auteur
!  Julien Rizzi (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_get_donnees_astre(donnees_astre,code_astre,n_astre,type_requete,mode_bavard)
!.    	real(kind=pm_reel), dimension(275, 7) :: donnees_astre
!.    	integer :: n_astre
!.    	integer, dimension(275) :: code_astre
!.      integer :: type_requete
!.      integer :: mode_bavard
!
!$Arguments
!>E     nomdom         : character(*)         Nom de domaine de traduction
!>S     donnees_astre  :<real,DIM=(275, 7)>   donnees des corps
!>E     code_astre     :<integer,DIM=(275)>   codes des corps
!>E     n_astre        :<integer>             nombre de corps dans la liste         
!>E     type_requete   :<integer>             type de requete : GS_REQUETE_CORCEN ou GS_REQUETE_ASTRE_REP
!>E     mode_bavard    :<integer>             affichage ou non des messages
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use cps_utilisateur

	implicit none

#include "parameter_gslib.h"

        ! ARGUMENTS
        !===============
        character(len=*), intent(in) :: nomdom
	real(kind=pm_reel), dimension(275, 7), intent(out) :: donnees_astre
	integer, intent(in) :: n_astre
	integer, dimension(275), intent(in) :: code_astre
        integer, intent(in) :: type_requete
        integer, intent(in) :: mode_bavard


        ! VARIABLES LOCALES
        !==================
	character(LEN=32) :: unite
	character(LEN=128) :: avertissement
	real(kind=pm_reel) :: mu,requa,apla,vrot

        integer :: ii
        integer :: trouve
        integer :: valeur_non_trouvee,valeur_trouvee,nb_total_requete
       
        
        ! Initialisations
        !================
        ! -> on suppose d'abord que la base est complète (=0 -> toutes les valeurs 
        ! sont trouvées. > 0 : x valeurs n'ont pas été trouvées) 
        donnees_astre = 0._pm_reel
        valeur_non_trouvee = 0
        valeur_trouvee = 0
        nb_total_requete = 0

        if (mode_bavard == 1) then
           call gsmes(DOMtraduire(nomdom,"GS_CHARGEMENT_BASE_COMPAS"),2)
        end if
        
        ! restrictions aux fichiers dont l'id contient 'systeme_solaire'
        call cps_restreindre("systeme_solaire")

        !     Pour tout astre, pour la théorie choisie, on 
        !     récupère les différentes variables voulues (mu, requa, apla, vrot)
        !     DM-538: remplissage du tableau 2D donnees_astre.
        ii= acc_err_setmode(ACC_ERR_MEMO)

        do ii=1,n_astre
           if (code_astre(ii).ge.0) then
              mu = 0._pm_reel
              requa = 0._pm_reel
              apla = 0._pm_reel
              vrot = 0._pm_reel
              if (type_requete == GS_REQUETE_CORCEN .or. &
                   type_requete == 0) then
              trouve = cps_getCsteThCourante(code_astre(ii), "mu", mu, unite)
              if(trouve /= CPS_OK) then 
                 valeur_non_trouvee = valeur_non_trouvee + 1
              else
                 valeur_trouvee = valeur_trouvee + 1
              end if
           end if
              trouve = cps_getCsteThCourante(code_astre(ii), "requa", requa, unite)
              if(trouve /= CPS_OK) then 
                 valeur_non_trouvee = valeur_non_trouvee + 1
              else
                 valeur_trouvee = valeur_trouvee + 1
              end if
              
              trouve = cps_getCsteThCourante(code_astre(ii), "apla", apla, unite)
              if(trouve /= CPS_OK) then 
                 valeur_non_trouvee = valeur_non_trouvee + 1
              else
                 valeur_trouvee = valeur_trouvee + 1
              end if
              if (type_requete == GS_REQUETE_ASTRE_REP .or. &
                   type_requete == 0) then
              trouve = cps_getCsteThCourante(code_astre(ii), "vrot", vrot, unite)
              if(trouve /= CPS_OK) then 
                 valeur_non_trouvee = valeur_non_trouvee + 1
              else
                 valeur_trouvee = valeur_trouvee + 1
              end if
           end if
              donnees_astre(ii,1) = mu
              donnees_astre(ii,2) = requa
              donnees_astre(ii,3) = apla
              if (apla > 0._pm_reel) donnees_astre(ii,3) = 1._pm_reel/apla
              donnees_astre(ii,4) = vrot
              donnees_astre(ii,5) = 0._pm_reel
           endif
        enddo

        ! Décompte des valeurs non trouvées
        if (mode_bavard == 1) then
           if (valeur_non_trouvee > 0) then
              ! Certaines valeurs n'ont pas été trouvées dans COMPAS
              ! Cela n'a pas forcément d'incidence pour l'utilisateur

              nb_total_requete = valeur_non_trouvee + valeur_trouvee
              
              write(avertissement,'(i4,a1,i4,a80)') valeur_trouvee,"/",&
                   nb_total_requete,DOMtraduire(nomdom,"GS_BASE_COMPAS_INCOMPLETE")
              call gsmes(avertissement,2)
           else
              ! -> toutes les valeurs ont été trouvées
              call gsmes(DOMtraduire(nomdom,"GS_BASE_COMPAS_COMPLETE"),2)

           end if
        end if

        ! reset des erreurs MADONA (cas d'erreur d'unité par exemple)
        if (acc_error() < 0 ) ii = acc_err_reset()
        ii= acc_err_setmode(ACC_ERR_ECHO)

        if (ii < 0) then
           write (avertissement,'(a80)') DOMtraduire(nomdom,"GS_ERR_INTERNE_GSLIB_COMPAS")
           call gsmes(avertissement,2)
        end if


      end subroutine gs_get_donnees_astre


      subroutine gs_init_bulletin(nomdom,theorie_poles,fichier_corps,init_gs_bulletin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_init_bulletin
!
!$Resume
!  Initialise les données du bulletin (corps de repère et corps centraux) en lisant 
!  les corps centraux dans un fichier.
!  La théorie de pôles détermine la liste de corps de repère.
!  Cette routine est une surcouche à gsi_init_bulletin
!
!$Description
!  Initialise les données du bulletin (corps de repère et corps centraux) en lisant 
!  les corps centraux dans un fichier.
!  La théorie de pôles détermine la liste de corps de repère.
!  Cette routine est une surcouche à gsi_init_bulletin
!
!$Auteur
! Y. TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_init_bulletin(nomdom,theorie_poles,fichier_corps,init_gs_bulletin)
!.    character(len=*) :: nomdom
!.    integer :: theorie_poles
!.    character(len=*) :: fichier_corps
!.    integer :: init_gs_bulletin
!
!$Arguments
!>E     nomdom            :<LEN=*>     Nom de domaine pour la traduction (ex : gslib_fr)
!>E     theorie_poles     :<integer>   Théorie de pôles (ex : GS_TH_UAI1991, GS_TH_MSPRO)
!>E     fichier_corps     :<LEN=*>     Fichier de corps
!>E/S   init_gs_bulletin   :<integer>  Variable indiquant si on veut (ré)initialiser les données (=0)
!                                      puis si l'initialisation a eu lieu (=1)
!
!$Common
!
!$Routines
!- gs_liretableau
!- gsmes
!- gs_init_bulletin
!
!$Include
!#V
!- gsastre.h
!- parameter_planete.h
!#
!
!$Module
!#V
!- EPHEM
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
! gs_init_bulletin
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use EPHEM

        implicit none
        
        ! Arguments
        !===========
        character(len=*), intent(in) :: nomdom
        integer,          intent(in) :: theorie_poles
        character(len=*), intent(in) :: fichier_corps
        integer,          intent(inout) :: init_gs_bulletin


        ! Includes
        !===========

#include "gsastre.h"
#include "parameter_planete.h"


        ! Variables locales
        !===================
        integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT) :: codes_corcen
        integer :: nb_lus
        integer :: ii
        
        if (init_gs_bulletin==0) then
        if (trim(fichier_corps) /= "") then
           ! Si un fichier de corps est donné, 
           ! on le lit et on initialise le tableau de corps centraux
           !========================================================

           do ii = 1,GS_CPS_NB_MAX_CORPS_SELECT
              ! valeur "invalide"
              codes_corcen = -1
           end do
           ! On lit les corps indiqués et on compare avec code_astre/n_astre
           call gs_liretableau(fichier_corps,codes_corcen,nb_lus)           

           if (nb_lus<1) then
              call gsmes(DOMtraduire(nomdom,"GS_FICCORPS_INC"),2)
           end if
           
        else 
           ! Si aucun fichier n'est proposé, on initialise la liste par défaut
           ! -> les 9 planètes du Système Solaire + le soleil
           !=================================================
           
           do ii = 1,GS_CPS_NB_MAX_CORPS_SELECT
              ! valeur "invalide"
              codes_corcen = -1
           end do

           do ii = 1,9
              codes_corcen(ii) = ii*100+99 ! Ex : corps 3 = 399 (Terre)
           enddo
           codes_corcen(10) = eph_soleil
           
        end if
        endif

        ! Appel à la fonction d'initialisation gs_init_bulletin
        call gsi_init_bulletin(nomdom,theorie_poles,codes_corcen,init_gs_bulletin)

      end subroutine gs_init_bulletin

      subroutine gs_traduire_planetes(nomdom)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_traduire_planetes
!
!$Resume
!
!$Description
!   Traduit les nom des planètes "historiques" (ie corps non compas) en fonction
!    du domaine de traduction
!
!$Auteur
!  O Kverneland
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_get_liste_astre_rep(nomdom)
!.    character(len=*) :: nomdom
!
!$Arguments
!>E     nomdom       :<character(LEN=*)>           Nom du domaine de traduction
!
!$Common
!
!$Routines
!- Domtrad
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- cps_utilisateur
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
        
        use cps_utilisateur
        
        implicit none
        character(len=*), intent(in) :: nomdom

        integer :: ii
        
#include "gsastre.h"
#include "parameter_planete.h"
#include "parameter_gslib.h"
        ! Initialisations des listes de corps pour les repères
        tab_astre_rep(1)  = DOMtraduire(nomdom,"GS_REP_PLA_MERCURE")
        tab_astre_rep(2)  = DOMtraduire(nomdom,"GS_REP_PLA_VENUS")
        tab_astre_rep(3)  = DOMtraduire(nomdom,"GS_REP_PLA_TERRE")
        tab_astre_rep(4)  = DOMtraduire(nomdom,"GS_REP_PLA_MARS")
        tab_astre_rep(5)  = DOMtraduire(nomdom,"GS_REP_PLA_JUPITER")
        tab_astre_rep(6)  = DOMtraduire(nomdom,"GS_REP_PLA_SATURNE")
        tab_astre_rep(7)  = DOMtraduire(nomdom,"GS_REP_PLA_URANUS")
        tab_astre_rep(8)  = DOMtraduire(nomdom,"GS_REP_PLA_NEPTUNE")
        tab_astre_rep(9)  = DOMtraduire(nomdom,"GS_REP_PLA_PLUTON")
        
        ! Tableaux pour l'écriture/lecture des fichiers
        do ii=1,n_astre_rep
           tab_astre_rep_mad(ii)=tab_astre_rep(ii)
        enddo
      end subroutine gs_traduire_planetes

      subroutine gsi_init_bulletin(nomdom,theorie_poles,liste_code_corcen,init_gs_bulletin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_init_bulletin
!
!$Resume
!  Initialise les données du bulletin (corps de repère et corps centraux) 
!  grâce à une liste de code NAIF de corps centraux.
!  La théorie de pôles détermine la liste de corps de repère.
!  Si l'utilisateur veut initialiser les corps centraux via une liste sur fichier,
!  alors la routine gs_init_bulletin est recommandée.
!
!$Description
!  Initialise les données du bulletin (corps de repère et corps centraux) 
!  grâce à une liste de code NAIF de corps centraux.
!  La théorie de pôles détermine la liste de corps de repère.
!  alors la routine gs_init_bulletin est recommandée.
!
!$Auteur
!  Y. TANGUY (ATOS)
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_init_bulletin(nomdom,theorie_poles,liste_code_corcen,init_gs_bulletin)
!.    character(len=*) :: nomdom
!.    integer :: theorie_poles
!.    integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT) :: liste_code_corcen
!.    integer :: init_gs_bulletin
!
!$Arguments
!>E     nomdom             :<LEN=*>              Nom de domaine pour la traduction (ex : gslib_fr)
!>E     theorie_poles      :<integer>            Théorie de pôles (ex : GS_TH_UAI1991, GS_TH_MSPRO)        
!>E     liste_code_corcen  :<integer,DIM=(275)>  Tableau de corps centraux
!>E/S   init_gs_bulletin   :<integer>            Variable indiquant si on veut (ré)initialiser les données (=0)
!                                                puis si l'initialisation a eu lieu (=1)
!
!$Common
!
!$Routines
!- gs_get_nom_astre
!- gs_init_astre_rep
!- gsmes
!- gs_get_donnees_astre
!
!$Include
!#V
!- gsastre.h
!- parameter_planete.h
!#
!
!$Module
!#V
!- EPHEM
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
! gs_init_bulletin
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use EPHEM

        implicit none
        
        ! Arguments
        !===========
        character(len=*), intent(in) :: nomdom
        integer,          intent(in) :: theorie_poles
        integer, dimension(GS_CPS_NB_MAX_CORPS_SELECT), intent(in) :: liste_code_corcen
        integer,          intent(inout) :: init_gs_bulletin


        ! Includes
        !===========

#include "gsastre.h"
#include "parameter_planete.h"
#include "parameter_gslib.h"

        ! Variables locales
        !===================
        integer :: nb_lus
        integer :: ii
        character(len=2) :: langue
        integer :: lenn

        logical :: init_corcen, init_rep
        
        ! Initialisations des listes de corps utilisées par GSLIB et des valeurs 
        ! associées à ces corps.
        !
        ! Cette routine initialise les corps centraux disponibles, et les corps de 
        ! définition des repères.
        !
        ! Les paramètres sont une théorie de pôles
        ! et éventuellement un fichier de liste de corps
        !============================================================================

        ! Init variables locales
        nb_lus = 0 ! nb de corps lus dans le fichier de corps
        
        init_corcen = .false.
        init_rep = .false.

        affichage_message_COMPAS = 0

        ! Langue (à partir du nom du domaine)
        lenn=len_trim(nomdom)
        if(lenn>2) langue=nomdom(lenn-1:lenn)
        if (langue.ne."fr".and.langue.ne."en") langue="fr"

        !==============================================
        ! A) Constitution de la liste de corps centraux
        ! -> n_astre : nb de corps centraux
        ! -> tab_astre : tableau des noms
        ! -> code_astre: tableau des codes NAIF
        !==============================================
        
        ! Contrôle que la liste passée en paramètre est différente 
        ! de la liste code_astre
        !
        ! -> si les listes sont différentes, on ré-initialise : init_corcen = Vrai
        ! -> sinon, on ne ré-initialise pas (gain de temps) : init_corcen = Faux
        !
        ! Conditions sur la boucle : taille max des tab (GS_CPS_NB_MAX_CORPS_SELECT)
        ! ET init_corcen==Faux ET liste_code_corcen contient des codes NAIF valides (>=0)

        ii = 1
        
        do while (ii <= GS_CPS_NB_MAX_CORPS_SELECT .and. &
             (.not.init_corcen) .and. &
             liste_code_corcen(ii)>=0 ) 
           
           if (code_astre(ii) /= liste_code_corcen(ii) &
                .or. init_gs_bulletin == 0) then
              ! Si un des corps est différent, il faut réinitialiser la liste
              ! ou si l'init du bulletin est demandée
              init_corcen = .true.
           end if
           
           ii = ii +1
           
        end do

        if (init_corcen) then

           n_astre = 1
           
           ! La liste des codes fournis par l'utilisateur est recopiée dans
           ! code_astre.
           do while (n_astre <= GS_CPS_NB_MAX_CORPS_SELECT .and. &
                liste_code_corcen(n_astre)>=0 ) 
              
              code_astre(n_astre) = liste_code_corcen(n_astre)
              n_astre = n_astre + 1
           end do
           n_astre = n_astre - 1

           ! Pour chaque corps, on récupère le nom dans tab_astre (liste écran)
           ! et on les recopie dans tab_astre_mad (liste pour les read/write de MADONA)
           do ii=1,n_astre
              call gs_get_nom_astre(code_astre(ii),tab_astre(ii),langue)
              tab_astre_mad(ii) = tab_astre(ii)
              
           end do
           
        end if

        !==============================================
        ! B) Constitution de la liste de planètes de 
        ! définition des repères
        ! -> n_astre_rep   : nb de planètes de définition de repères
        ! -> tab_astre_rep : tableau des noms
        ! -> code_astre_rep: tableau des codes NAIF
        !==============================================
        
        if (theorie_poles == 0) then

           if (n_astre_rep /= (nb_cen - 1) .or. init_gs_bulletin == 0) then

              init_rep = .true.
              
              ! Sauvegarde de la théorie de pôles dans la variable globale th_uai
              th_uai = theorie_poles

              ! Cas B1) : 10 corps par défaut pour définir les reperes
              ! -> theorie_poles mise à 0 pour "forcer" une liste restreinte, avec 
              ! les noms utilisés par PSIMU (Terrien, Martien, etc.)

              ! Il y a toujours 9 corps de définition des repères (les 9 planètes)
              n_astre_rep = nb_cen - 1

              do ii = 1, n_astre_rep
                 code_astre_rep(ii) = ii*100+99
              enddo
              
              ! Traduction des planetes dans ce mode historique ou les noms des corps
              ! sont dans le fichier de traduction (et non pas dans le fichier compas
              ! comme c'est le cas du mode COMPAS)
              call gs_traduire_planetes(nomdom)
              
           end if

        else
           
           ! Théorie de pôles non nulle -> théorie COMPAS 
           !=============================================
           
           if (init_gs_bulletin == 0 .or. theorie_poles /= th_uai) then
              ! On effectue la ré-init si init_gs_bulletin vaut 0
              ! où si la théorie a changé
              init_rep = .true.

              ! Sauvegarde de la théorie de pôles dans la variable globale th_uai
              th_uai = theorie_poles

              call gsi_get_liste_astre_rep(tab_astre_rep,&
                   tab_astre_rep_mad, code_astre_rep,&
                   n_astre_rep, th_uai)

           end if

        end if

        !==============================================
        ! C) Récupération des valeurs associées aux 
        ! listes de corps centraux et de corps de repère
        !==============================================
        if (init_corcen) then
           ! Récupération des données pour les corps centraux
           call gs_get_donnees_astre(nomdom,donnees_astre,code_astre,&
                n_astre,GS_REQUETE_CORCEN,affichage_message_COMPAS)
        end if

        if (init_rep) then
           ! Récupération des données pour les corps des repères
           call gs_get_donnees_astre(nomdom,donnees_astre_rep,code_astre_rep,&
                n_astre_rep,GS_REQUETE_ASTRE_REP,affichage_message_COMPAS)
        end if

        if (init_rep .and. init_corcen) then
           ! Le bulletin a été (ré)initialisé
           init_gs_bulletin = 1
        end if

      end subroutine gsi_init_bulletin


      subroutine gsi_get_liste_astre_rep(tab_astre,tab_astre_mad, code_astre, &
         nb, th_poles)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_get_liste_astre_rep
!
!$Resume
!
!$Description
!   Permet de construire la liste des astres avec leur code        
!      d'apres la base de données COMPAS, restreinte aux corps        
!      définissant un repère (planètes et satellites)
!
!$Auteur
!  Florence VIVARES (ATOS Origin)    
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_get_liste_astre_rep(tab_astre,tab_astre_mad, code_astre, &
!.             nb, th_poles)
!.    integer :: th_poles 
!.    character(len=256), dimension(*) :: tab_astre
!.    character(len=256), dimension(*) :: tab_astre_mad
!.    integer, dimension(*) :: code_astre
!.    integer :: nb
!
!$Arguments
!>S     tab_astre      :<LEN=256,DIM=(*)>   liste des astres (squelette panel) 
!>S     tab_astre_mad  :<LEN=256,DIM=(*)>   liste des astres pour read/write   
!>S     code_astre     :<integer,DIM=(*)>   code_astre(*) = codes des astres   
!>S     nb             :<integer>           nombre d'astres dans la liste  
!>E     th_poles       :<integer>           Théorie de mouvement de pôles  
!
!$Common
!
!$Routines
!- getenv
!- cps_getNoms77
!- cps_getListCorps77Int
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Remarques
! YTY 19/05/06. Explications sur le contenu des tableaux :            
! Les tableaux servent aux menus du squelette panel (variable tab_astre)
! et pour écrire/lire les noms (read/write)
! code_astre contient les codes NAIF.
!.     tab_astre   tab_astre_mad  code_astre
!.     terre        ""             -1     (valeur volontairement invalide)
!.     @terre       terre         399
!.     @lune        lune          301
!.     mars         ""             -2 
!.     @mars        mars          499
!.     @phobos      phobos        401
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use cps_utilisateur

use MSPRO
use MSLIB

      implicit none

! Pour les codes des théories
#include "parameter_gslib.h"
      
      ! Paramètres
      !===========

      ! Entrée obligatoire
      integer, intent(IN)                           :: th_poles 
      
      ! Sorties
      character(len=256), dimension(*), intent(OUT) ::  tab_astre
      character(len=256), dimension(*), intent(OUT) ::  tab_astre_mad
      integer, dimension(*), intent(OUT)            :: code_astre
      integer, intent(OUT)                          :: nb
              
      ! Variables locales
      !==================

      ! Code de la langue pour la traduction
      character(len=2)                       :: langue      

      character(len=32)                      :: nomcorps
      integer, dimension(MAXCORPS) 	     :: code_pla
      integer, dimension(MAXCORPS)           :: code_sat
      integer		                     :: nb_planet
      integer		                     :: nb_sat
      character(len=256), dimension(10000)   :: tab_astre_tmp
      integer, dimension(10000)              :: code_astre_tmp
      
      integer		:: ier, ii, jj, kk, codetmp
      
      ! Compteur de corps centraux
      integer           :: nb_corps_centraux 
      ! Théorie de poles (locale)
      integer           :: th_poles_loc

      ! Initialisations
      !=================
      kk = 1 

      call getenv("MADONA_LANG", langue)
      if (langue.ne."en") langue = "fr"
      
      
      ! Liste des planètes du système solaire
      !======================================
      nb_planet=9
      code_pla(1) = 199
      code_pla(2) = 299
      code_pla(3) = 399
      code_pla(4) = 499
      code_pla(5) = 599
      code_pla(6) = 699
      code_pla(7) = 799
      code_pla(8) = 899
      code_pla(9) = 999
      
      ! Compatibilité avec les codes MSPRO
      ! -> la gestion de la compatibilité avec les codes
      ! MSPRO est réalisée ici.
      !===================================
      if (th_poles == pm_UAI1994 .or. &
           th_poles == pm_lieske_wahr_aoki) then
         ! GS_TH_MSPRO couvre les théories LWA (pour la Terre) et 
         ! UAI 1994 (pour les autres planètes du Système Solaire)
         th_poles_loc = GS_TH_MSPRO
      else if (th_poles == pm_UAI2000) then
         ! Traduction vers le code GSLIB
         th_poles_loc = GS_TH_UAI2000
      else
         ! Sinon, on utilise le code existant
         th_poles_loc = th_poles
      end if

      
      ! Si théorie MSPRO : on reste sur les planètes
      !=============================================
      if(th_poles_loc == GS_TH_MSPRO) then
         nb = nb_planet
         code_astre(1:nb_planet)= code_pla(1:nb_planet)
         call cps_getNoms77(code_astre,nb,langue,tab_astre_tmp)
         tab_astre(1:nb_planet) = tab_astre_tmp(1:nb_planet)
         tab_astre_mad(1:nb_planet) = tab_astre_tmp(1:nb_planet)
      
         
      ! Si théorie UAI 1991 ou 2000 : systèmes planétaires (planètes + satellites)
      !===========================================================================
      else if ( th_poles_loc == GS_TH_UAI1991 .or. th_poles_loc == GS_TH_UAI2000 ) then
      ! Si théorie UAI1991 ou UAI2000
         do ii=1,nb_planet
            ! Ajout du titre du menu (-code)
            code_astre(kk) = code_pla(ii)*(-1)
            kk = kk+1
            ! Et du corps lui même
            code_astre(kk) = code_pla(ii)
            kk = kk+1
         
            ! Création de la liste des satellites des planètes
            !=================================================
         
            if (ii.ge.1.and.ii.le.9) then
               call cps_getListCorps77Int("corpsc",code_pla(ii), &
               code_sat, nb_sat)
            else
               nb_sat = 0
            endif
            do jj = 1,nb_sat
               ! Vérifier qu'il s'agit d'un corps UAI
               write(nomcorps,'(a5,i3)') "corps", code_sat(jj)

               ! tester si corps défini dans la théorie UAI
               ier = cps_requete("mvts_poles_UAI", nomcorps, "*", &
               "code", codetmp)
               ! sinon
               if (ier==cps_ok) then
                  code_astre(kk) = code_sat(jj)
                  kk = kk+1
               endif
           
            enddo
         enddo
         nb = kk-1
        
         ! code_astre_tmp ne contient que des codes NAIF "valides"
         ! pour permettre un appel a cps_getNoms77
         do ii=1,nb
            if (code_astre(ii).lt.0) then
               code_astre_tmp(ii)=-code_astre(ii)
            else
               code_astre_tmp(ii)=code_astre(ii)
            endif
         enddo
         call cps_getNoms77(code_astre_tmp,nb,langue,tab_astre_tmp)
      
         ! Nouveau parcours :
         ! Dès qu'un corps est présent deux fois, cela signifie que le premier
         ! sera le titre (voir "@") du sous menu ou apparaîtra la seconde occurence
         !=========================================================================
         nb_corps_centraux = 1
         do ii=1,nb-1
            if (tab_astre_tmp(ii).eq.tab_astre_tmp(ii+1)) then
               tab_astre(ii) = trim(tab_astre_tmp(ii))
               ! On enleve le @ dans les tableaux tab_astre mad : ils ne servent
               ! a rien tab_astre_mad(ii) ne doit pas contenir le nom de la planete
               !===================================================================
               tab_astre_mad(ii) = ""
               code_astre(ii) = -nb_corps_centraux
               nb_corps_centraux = nb_corps_centraux+1
            else
               tab_astre(ii) = "@"//trim(tab_astre_tmp(ii))
               tab_astre_mad(ii) = trim(tab_astre_tmp(ii))
            endif
         enddo
         tab_astre(nb) = "@"//trim(tab_astre_tmp(nb))
         tab_astre_mad(nb) = trim(tab_astre_tmp(nb))

      endif ! FIN cas 1991, 2000

      return
   end subroutine gsi_get_liste_astre_rep


      
     subroutine gs_testcompas(ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_testcompas
!
!$Resume
!  Teste la présence de COMPAS
!
!$Description
!  
!$Auteur
!  Florence Vivares
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_testcompas(ier)
!.    integer :: ier
!
!$Arguments
!>S     ier  :<integer>   code retour 1=OK, 
!                            -1= compas.rc absent
!                            -2= base infiquée absente
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       use cps_utilisateur

       implicit none

       ! Argument
       integer, intent(out) :: ier

       ! Variables locales
       integer :: iertmp, lch
       character(len=132) :: path, nom, fic
       integer :: acces, ii

       ! initialisations
       ier = 1
       
       ! Présence d'un compas.rc complet
       iertmp = AMv_rc_get('compas2_fcf','compas','','',path,lch)
       if (iertmp < 0) then
           write (0,*) "INTERNAL ERROR : Erreur d'accès au fichier de ressource compas.rc"
           write (0,*) "INTERNAL ERROR : An error occured while accessing through compas.rc file"
        end if
       if (lch == 0) ier = -1

       iertmp = AMv_rc_get('DB_COMPAS_REF','compas','','',path,lch)
       if (iertmp < 0) then
           write (0,*) "INTERNAL ERROR : Erreur d'accès au fichier de ressource compas.rc"
           write (0,*) "INTERNAL ERROR : An error occured while accessing through compas.rc file"
       end if
       if (lch == 0) ier = -1

       iertmp = AMv_rc_get('DB_COMPAS_CONFIG','compas','','',nom,lch)
       if (iertmp < 0) then
          write (0,*) "INTERNAL ERROR : Erreur d'accès au fichier de ressource compas.rc"
          write (0,*) "INTERNAL ERROR : An error occured while accessing through compas.rc file"
       end if

       if (lch == 0) ier = -1
       if (acc_error() < 0 ) iertmp = acc_err_reset()       
       if (ier < 0) return

       ! vérification COMPAS OK
       ! accès MADONA au fichier de config.
       fic=trim(path)//"/"//nom
       acces=acc_open()
       lch=acc_connect(acces, fic, "r")
       iertmp=acc_close(acces)
       ! Accès NOK
       if (lch<0) then
          ier= -2
          write(0,*) "Attention : base COMPAS NOK"
          call MSP_effacer_message(nb_mes=MSP_dernier_message)
          if (acc_error() < 0 ) then 
             ii = acc_err_reset()
             if (ii < 0) then 
                write (0,*) "INTERNAL ERROR"
                return
             end if
          end if
       end if
       
       ! Base locale
       iertmp = AMv_rc_get('DB_COMPAS_LOCAL','compas','','',path,lch)

       ! Pas de base locale : tout est OK
       if (lch == 0) return

       ! Si base locale précisée elle doit être présente et le fichier config
       ! doit être OK
       fic=trim(path)//"/"//nom
       acces=acc_open()
       if (acces < 0) then
          write (0,*) "INTERNAL ERROR : Erreur d'accès au fichier de ressource compas.rc"
          write (0,*) "INTERNAL ERROR : An error occured while accessing through compas.rc file"
       end if

       lch=acc_connect(acces, fic, "r")
       iertmp=acc_close(acces)
       if (iertmp < 0) then
          write (0,*) "INTERNAL ERROR : Erreur d'accès au fichier de ressource compas.rc"
          write (0,*) "INTERNAL ERROR : An error occured while accessing through compas.rc file"
       end if

       if (lch<0) then
          ! Accès NOK
          ier= -2
          write(0,*) "Attention : base locale NOK"
          call MSP_effacer_message(nb_mes=MSP_dernier_message)
          if (acc_error() < 0 ) ii = acc_err_reset()
          return
       endif

     end subroutine gs_testcompas


     subroutine gs_get_obliquite(obliquite)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_get_obliquite
!
!$Resume
!  Récupération de la valeur d'obliquité dans COMPAS
!
!$Description
!  Récupération de la valeur d'obliquité dans COMPAS
!
!$Auteur
!  Y.TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gs_get_obliquite(obliquite)
!.    real(kind=pm_reel) :: obliquite
!
!$Arguments
!>S     obliquite  :<pm_reel>   Valeur de l'obliquité en radians
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
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

       use cps_utilisateur

       implicit none
       
       ! Argument
       !=========
       real(kind=pm_reel), intent(out) :: obliquite

       ! Variables locales
       !==================
       integer :: trouve
       character(len=32) :: unite

       ! Début du code
       !==============
       ! Recherche de la valeur d'obliquité dans la théorie courante
       trouve = cps_getCsteGenThCourante("obliq", obliquite, unite)

       if (trouve /= CPS_OK) then
          ! Recherche de la valeur d'obliquité dans la théorie par défaut
          ! (sera disponible : la théorie par défaut UAI1994 est complète)
          trouve = cps_getCsteGenTh("UAI1994","obliq", obliquite, unite)
         
       end if
       
       ! conversion de la valeur en rad (la valeur sortie par COMPAS est en arc seconde)
       ! 1 arcs = 1/60 arc min = 1/3600 deg
       if (trim(unite) == "arcs") then
          obliquite = obliquite*pm_pi/(3600*180)
       else if (trim(unite) == "deg") then
          obliquite = obliquite*pm_pi/180
       end if

     end subroutine gs_get_obliquite


     subroutine gs_lire_poletsid(date_jjfrac, fichier_poletsid, numpla,&
          alpha, delta, tsid, dtsid, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gs_lire_poletsid
!
!$Resume
!  Calcul de la position du pôle, du temps sidéral et de sa dérivée
!  à partir d'un fichier poletsid donnée
!
!$Description
!  Calcul de la position du pôle, du temps sidéral et de sa dérivée
!  à partir d'un fichier poletsid donnée. Le code de retour ier
!  est négatif en cas d'erreur.
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Arguments
!>E     date_jjfrac       :<pm_reel>         Date en jour julein 1950 fractionnaires
!>E     fichier_poletsid  :<LEN=CPS_MAXLG>   Fichier décrivant le mouvement du pôle et temps sidéral
!>E     numpla            :<integer>         Numéro NAIF du corps tel qu'il doit être lu
!>S     alpha             :<pm_reel>         Angle alpha de positionnement du pôle (rad)
!>S     delta             :<pm_reel>         Angle delta de positionnement du pôle (rad)
!>S     tsid              :<pm_reel>         Temps sidéral (rad)
!>S     dtsid             :<pm_reel>         Dérivée du temps sidéral (rad/s)
!>S     ier               :<integer>         Code retour
!.            ier =  0 : Succès de l'opération
!.            ier = -1 : Erreur lors de la recherche du path du fichier dans la base de données
!.            ier = -2 : Erreur lors de la lecture du fichier
!.            ier = -3 : Le code NAIF du corps dans le fichier ne correspond pas à celui en entrée
!.            ier = -4 : Erreur lors de l'interpolation
!
!$Common
!
!$Routines
!- MSP_effacer_message
!- cps_lireFichierPoletsid
!- MSP_recuperer_message
!- MSP_afficher_message
!- cps_interpolPoletsid
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!- cps_poletsid_fichier
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

use cps_utilisateur
use cps_poletsid_fichier

     implicit none
   
!  Paramètre d'entrée
!----------------------
     character(LEN=CPS_MAXLG), intent(in) :: fichier_poletsid
     real(KIND=pm_reel), intent(in) :: date_jjfrac
     integer, intent(in)            :: numpla

!  Paramètres de sortie
!----------------------
     real(KIND=pm_reel), intent(out) :: alpha
     real(KIND=pm_reel), intent(out) :: delta
     real(KIND=pm_reel), intent(out) :: tsid
     real(KIND=pm_reel), intent(out) :: dtsid
     integer, intent(out)            :: ier

!  Variables locales
!----------------------

     character(LEN=CPS_MAXLG) :: path_fichier_poletsid
     ! Nessaire pour l'interpolation (cas fichier poletsid)
     type(cps_struct_poletsid_fichier) :: tab_poletsid 
     ! Pas de temps autour de la date de ref permettant le'interpolation (cas poletsid) 
     ! Si le pas de temps dans le fichier est supèrieur à deltaT, on prend les valeurs autour 
     real (kind=PM_REEL), parameter  :: deltaT = 0.01_pm_reel    ! 0.01jour ~ 14 min  
     
     ! Traitement d'une erreur lors de la lecture ou interpolation de poletsid
     type(MSP_MESSAGE) :: cps_mess
     ! Corps present dans le fichier poletsid
     integer :: corps_lu
     integer :: trouve_base   !Code de retour de la recherche en base

!  Debut du code
!----------------------
     ier = 0
     path_fichier_poletsid = ""
     trouve_base = CPS_OK
     if (MSP_PROBLEME) then
         call MSP_effacer_message()
     endif

     ! Extraction du path du fichier pour le modele fournit en entrée
     ! Recherche du chemin d'acces au fichier du modèle en entrée
     trouve_base = cps_getFichierModele("poletsid_fichier", trim(fichier_poletsid), &
               path_fichier_poletsid, rep=.true.)
     ! Erreur interne, cela ne devrait pas arriver
     if ( trouve_base /= CPS_OK ) then
         write(0,*) 'Sous-programme : GS_lire_poletsid'
         write(0,*) "ERREUR : Impossible de retrouver le fichier du modèle ", &
                    trim(fichier_poletsid), " dans la base de donnees."
         ier = -1
         return
     endif

     ! Extraction d'une partie des données du fichier centrée sur la date en entrée
     call cps_lireFichierPoletsid(path_fichier_poletsid, tab_poletsid, corps_lu, &
           datemin=date_jjfrac-deltaT, datemax=date_jjfrac+deltaT)
     if (MSP_PROBLEME) then
        write(0,*) 'Sous-programme : GS_lire_poletsid'
        call MSP_recuperer_message(message=cps_mess)
        call MSP_afficher_message(message=cps_mess, unit=0)
        call MSP_effacer_message()
        ier = -2
        return
     endif
  
     ! Vérification de la cohérence entre le corps voulu et celui lu
     if ( numpla /= corps_lu) then
        write(0,*) 'Sous-programme : GS_lire_poletsid'
        write(0,*) 'Erreur : le corps (', corps_lu, &
             ') du fichier lu ne correspond pas au corps voulu (',numpla,')'
        ier = -3
        return
     endif
          

     ! Interpolation pour obtenir les angles alpha, delta, le temps sidéral et dériviée
     call cps_interpolPoletsid(tab_poletsid, date_jjfrac, alpha, delta, tsid, dtsid)
     if (MSP_PROBLEME) then
        write(0,*) 'Sous-programme : GS_lire_poletsid'
        call MSP_recuperer_message(message=cps_mess)
        call MSP_afficher_message(message=cps_mess, unit=0)
        call MSP_effacer_message()
        ier = -4
        return
     endif

     ! Destruction de la structure intermédiaire
     if ( associated(tab_poletsid%dates ) )  deallocate(tab_poletsid%dates)   
     if ( associated(tab_poletsid%alphas ) ) deallocate(tab_poletsid%alphas) 
     if ( associated(tab_poletsid%deltas ) ) deallocate(tab_poletsid%deltas) 
     if ( associated(tab_poletsid%tsid ) )   deallocate(tab_poletsid%tsid) 
     if ( associated(tab_poletsid%dtsid ) )  deallocate(tab_poletsid%dtsid)

end subroutine gs_lire_poletsid

   subroutine gsi_ajout_corps_fic_poletsid(nb, tab_astre, &
      tab_astre_mad, code_astre)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_ajout_corps_fic_poletsid
!
!$Resume
!  Routine qui a une liste de corps ajoute les corps disposant
!  d'un modèle de pôles sur fichier
!
!$Description
!  Routine qui a une liste de corps ajoute les corps disposant
!  d'un modèle de pôles sur fichier
!
!$Auteur
!  Cédric MARTEL (ATOS Origin)    
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_ajout_corps_fic_poletsid(nb, tab_astre, &
!.          tab_astre_mad, code_astre)
!.    character(len=256), dimension(*) :: tab_astre
!.    character(len=256), dimension(*) :: tab_astre_mad
!.    integer, dimension(*) :: code_astre
!.    integer :: nb
!
!$Arguments
!>S     nb             :<integer>           Nombre d'éléments dans la liste des corps
!>S     tab_astre      :<LEN=256,DIM=(*)>   Tableau des noms des astres adapté au menu
!>S     tab_astre_mad  :<LEN=256,DIM=(*)>   Tableau des noms des astres
!>S     code_astre     :<integer,DIM=(*)>   Tableau des codes des astres
!
!$Common
!
!$Routines
!- getenv
!- cps_getListeCorpsPoletsid
!- cps_getNoms
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!- cps_poletsid_fichier
!#
!
!$Remarques
!  Disposant déjà d'une liste de corps formatée créée par "get_liste_astre_rep", 
!  on ajoute un sous-menu "autres" contenant les corps diposant d'un modèle de 
!  pôle sur fichier.
!  On peut obtenir la structure de donnée suivante :
!.     tab_astre           tab_astre_mad    code_astre
!.     terre                ""               -1  (valeur volontairement invalide)
!.     @terre               terre           399
!.     @lune                lune            301
!.     mars                 ""               -2
!.     @mars                mars            499
!.     @phobos              phobos          401
!.     autre                ""               -3
!.     @mars+fichier        mars           -499  (le code sera interprété)
!.     @churyumov+fichier   churyumov  -1000012
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use cps_utilisateur
use cps_poletsid_fichier, only : cps_getListeCorpsPoletsid

      implicit none
      
! Paramètres      
      ! Sorties
      character(len=256), dimension(*), intent(OUT) :: tab_astre
      character(len=256), dimension(*), intent(OUT) :: tab_astre_mad
      integer, dimension(*), intent(OUT)            :: code_astre
      integer, intent(OUT)                          :: nb
              
! Variables locales
      
      character(len=2) :: langue  ! Code de la langue de traduction
      integer     :: ii      ! Indice de boucle
      
      ! Liste des corps disposant d'un fichier poletsid
      integer, dimension(:), pointer :: liste_code_corps => NULL()  
      ! Nombre de corps possédant au moins un fichier poletsid
      integer           :: nb_corps        
      !  Liste des noms des corps
      character(LEN=CPS_MAXLG), dimension(:), pointer :: list_noms => NULL()
    
! Initialisations
      call getenv("MADONA_LANG", langue)
      if (langue.ne."en") langue = "fr"

! Ajout de la case "autres" si l'analyse des fichiers poletsid est présent
      ! Extraction de la liste des corps disposant d'un fichier poletsid
      call cps_getListeCorpsPoletsid(nb_corps, liste_code_corps)
           
      if ( nb_corps >= 1 ) then
           nb = nb+1
           ! Ajout du sous-menu "Autres"
           tab_astre(nb) = "autres"
           tab_astre_mad(nb) = ""
           code_astre(nb) = -999999 ! Code inexploité et inaccessible dans le menu 

           ! Conversion des noms
           call cps_getNoms(liste_code_corps,langue, &
              list_noms)
              
           ! Recopie dans la liste
           do ii = 1, nb_corps
              nb = nb+1 
              tab_astre(nb) = "@"//trim(list_noms(ii))//"+fichier"
              tab_astre_mad(nb) = trim(list_noms(ii))//"+fichier"
              code_astre(nb) = (-1) * liste_code_corps(ii)
           end do

           if ( associated(list_noms) ) deallocate(list_noms)
      end if ! Fin SI plus d'un corps

      return
   end subroutine gsi_ajout_corps_fic_poletsid
 
   subroutine gsi_creer_liste_mod_poletsid(corps, &
      nb_modeles, codes_poletsid, liste_modeles)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_creer_liste_mod_poletsid
!
!$Resume
!  Création de la liste des modèles poletsid disponibles pour un corps donné
!
!$Description
!  Création simultannée de 3 liste, une liste de codes arbitraire par modèle,
!  la liste des noms des modèles, la liste qui a un modèle diponible donne
!  le fichier associé
!
!$Auteur
!  Cédric Martel
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_creer_liste_mod_poletsid(corps, &
!.          nb_modeles, codes_poletsid, liste_modeles)
!.    integer :: corps
!.    integer :: nb_modeles
!.    character(LEN=CPS_MAXLG), dimension(GS_MAX_POLETSID) :: liste_modeles
!.    integer, dimension(GS_MAX_POLETSID) :: codes_poletsid
!
!$Arguments
!>E     corps           :<integer>                               Code NAIF du corps
!>S     nb_modeles      :<integer>                               Nombre de modèles disponible pour ce corps
!>S     codes_poletsid  :<integer,DIM=(GS_MAX_POLETSID)>         Codes associés aux fichiers
!>S     liste_modeles   :<LEN=CPS_MAXLG,DIM=(GS_MAX_POLETSID)>   Liste des modèles diponibles
!
!$Common
!
!$Routines
!- cps_getModelesPoletsidCorps
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- cps_utilisateur
!- cps_poletsid_fichier
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

use cps_utilisateur
use cps_poletsid_fichier, only : cps_getModelesPoletsidCorps

      implicit none
#include "parameter_gslib.h" 
      
      integer, intent(in) :: corps
      integer, intent(out) :: nb_modeles
      character(LEN=CPS_MAXLG), dimension(GS_MAX_POLETSID), intent(out) :: liste_modeles
      integer, dimension(GS_MAX_POLETSID), intent(out) :: codes_poletsid
      
      ! Inidices de boucle
      integer :: ii 
      ! Vecteur temporaire
      character(LEN=CPS_MAXLG), dimension(:), pointer :: ptr_liste_modeles => NULL()

      ! Extraction de la liste des modèles poletsid disponibles 
      call cps_getModelesPoletsidCorps(corps, nb_modeles, ptr_liste_modeles)     
      
      if ( nb_modeles == 0 ) then 
         write (0,*) "Erreur interne : On ne devrait pas appeler gsi_creer_liste_mod_poletsid"
         write (0,*) " si la liste des modèles est vide"
         if ( associated(ptr_liste_modeles) ) deallocate(ptr_liste_modeles)
         return
       endif
 
      ! TODO 
      do ii=1, nb_modeles
         liste_modeles(ii) = ptr_liste_modeles(ii) 
      enddo
 
      ! Creation d'un liste de codes arbitraires
      do ii = 1, nb_modeles 
         codes_poletsid(ii) = ii
      enddo 

      if ( associated(ptr_liste_modeles) ) deallocate(ptr_liste_modeles)

   end subroutine gsi_creer_liste_mod_poletsid

   subroutine gsi_select_fic_poletsid (nb_modeles, &
      liste_modeles, fichierpoletsid, indice_select)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_select_fic_poletsid
!
!$Resume
!  Recherche de l'indice du modèle dans une liste construite
!
!$Description
!  Recherche du modele dans la liste. En cas d'échec, ou si le modèle n'est pas
!  spécifié en entrée, on choisit le premier modèle de la liste.
!
!$Auteur
!  Cédric Martel
!
!$Acces
!  PUBLIC
!
!$Usage
!  call gsi_select_fic_poletsid (nb_modeles, &
!.          liste_modeles, fichierpoletsid, indice_select)
!.    integer :: nb_modeles
!.    character(LEN=CPS_MAXLG), dimension(GS_MAX_POLETSID) :: liste_modeles
!.    character(LEN=CPS_MAXLG) :: fichierpoletsid
!.    integer :: indice_select
!
!$Arguments
!>E     nb_modeles          :<integer>                             Nombre de modèles disponibles  
!>E     liste_modeles       :<LEN=CPS_MAXLG,DIM=(GS_MAX_POLETSID)> Listes des nom des modèles de pôles sur fichier
!>E/S   fichierpoletsid     :<LEN=CPS_MAXLG>                       Nom du modèle à rechercher
!>S     indice_select       :<integer>                             Indice correspondant
!
!$Common
!
!$Routines
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- cps_utilisateur
!#
!
!$Remarques
!  Cette routine est utilisée dans le squelette panel et après une 
!  modification extérieure des paramètres. A partir du corps (numpla)
!  on sait retrouver la liste des noms des modèles disponibles.
!  On recherche alors le modèles "fichierpoletsid" parmi ces modèles.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
use cps_utilisateur, only : cps_getFichierModele, CPS_OK, CPS_MAXLG

      implicit none
#include "parameter_gslib.h" 

      integer, intent(in) :: nb_modeles
      character(LEN=CPS_MAXLG), dimension(GS_MAX_POLETSID), intent(in) :: liste_modeles
      character(LEN=CPS_MAXLG), intent(inout) :: fichierpoletsid
      integer, intent(out) :: indice_select 
      
! Variables locales
      logical :: trouve        ! Flag indiquant la réussite de la recherche
      integer :: ii            ! Indice de boucle

      ! Erreur interne, cela ne devrait pas arriver
      if (nb_modeles == 0 ) then 
          write(0,*) "ERREUR : Le nombre de modèle est nul, la fonction gsi_select_fic_poletsid n'aurait pas du être appelée "
          return
      endif
         
      ! Si le nom du fichier est nul, on prend l'indice par defaut 1
      if ( len_trim(fichierpoletsid) == 0 ) then
         indice_select = 1
         fichierpoletsid =  trim(liste_modeles(indice_select))
         return
      endif

      trouve = .false.
      ii = 1
      ! Recherche du modèle "fichierpoletsid" dans la liste
      do while ( .not. trouve .and. ( ii <= nb_modeles ) )
          if ( trim(liste_modeles(ii)) == trim(fichierpoletsid) ) then             
             trouve = .true.                   
          else
             ii = ii+1
          endif
      enddo

      ! Traitement du succès où de l'échec 
      if ( trouve) then
         indice_select = ii
      else
         ! Affichage d'un message d'erreur dans le cas où la base à changer
         ! et que le modèle est donc introuvable
         write(0,*) "ERREUR : Impossible de retrouver le modèle ",&
                    trim(fichierpoletsid), " dans la base de données."
         ! Selection du premier modele de la liste
         indice_select = 1
         fichierpoletsid = trim(liste_modeles(indice_select))
      endif


   end subroutine gsi_select_fic_poletsid



   subroutine gsi_lire_vitrot_poletsid(ech_date, jj, sec, &
      jj_ref, sec_ref, fichierpoletsid, numpla, &
      dtsid, retour_interpol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  gsi_lire_vitrot_poletsid
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
!  call gsi_lire_vitrot_poletsid(ech_date, jj, sec, fichierpoletsid, numpla, &
!.          alpha, delta, dtsid, retour_interpol)
!.    integer :: ech_date
!.    integer :: jj
!.    real(KIND=pm_reel) :: sec
!.    character(LEN=CPS_MAXLG) :: fichierpoletsid
!.    integer :: numpla
!.    real(KIND=pm_reel) :: dtsid
!.    integer :: retour_interpol
!
!$Arguments
!>E     ech_date         :<integer>       Echelle de date du repère 
!.          1950 : pm_1janvier1950_00h00
!.          2000 : pm_1janvier2000_12h00
!.          date courante (date bulletin) : pm_autre_date
!.          date de référence : pm_date_ref     
!>E     jj               :<integer>       Jour julien 1950
!>E     sec              :<pm_reel>       Secondes dans le jour (sec)
!>E/S   fichierpoletsid  :<LEN=CPS_MAXLG> Nom du modèle pôle et temsp sidéral
!>E     numpla           :<integer>       Code NAIF du corps
!>S     dtsid            :<pm_reel>       Dérivée du temps sidéral
!>S     retour_interpol  :<integer>       Code de retour de la méthode
!.          retour_interpol =  0 : Succés
!.          retour_interpol =  1 : Attention, le code de la planète du fichier 
!                                  n'est pas celui attendu
!.          retour_interpol = -1 : Erreur lors de la lecture du fichier
!.          retour_interpol = -2 : Erreur lors de l'interpolation finale
!
!$Common
!
!$Routines
!- gs_lire_poletsid
!
!$Include
!#V
!- parameter_mspro.h
!#
!
!$Module
!#V
!- cps_utilisateur
!- MSPRO
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

use cps_utilisateur
use MSPRO
implicit none

#include "parameter_mspro.h" 

!  Paramètres en entrée
      integer, intent(in) :: ech_date
      integer, intent(in) :: jj
      real(KIND=pm_reel), intent(in) :: sec
      integer, intent(in) :: jj_ref
      real(KIND=pm_reel), intent(in) :: sec_ref
      character(LEN=CPS_MAXLG) :: fichierpoletsid
      integer, intent(in) :: numpla

!  Paramètres en sortie
      real(KIND=pm_reel), intent(out) :: dtsid
      integer, intent(out)            :: retour_interpol

!  Variables locales
      real(KIND=pm_reel) :: date_jjfrac  ! Date en jours julien 1950 fractionnaires
      real(KIND=pm_reel) :: tsid         ! Temps sidéral, inexploité
      real(KIND=pm_reel) :: alpha        ! Angle alpha du pole, inexploité
      real(KIND=pm_reel) :: delta        ! Angle delta du pole, inexploité
      
!  Début du code 
      ! Initialisation
      retour_interpol = 0
      alpha = 0._pm_reel
      delta = 0._pm_reel
      tsid = 0._pm_reel
      dtsid = 0._pm_reel

      ! Conversion de la date en jj_frac selon l'échelle retenue :
      if ( ech_date == pm_1janvier1950_00h00 ) then 
         ! Jour julien 1950
         date_jjfrac = 0._pm_reel
      else
         if ( ech_date == pm_1janvier2000_12h00 ) then
            ! Jour julien 2000
            date_jjfrac = 18262.5_pm_reel
         else
            if ( ech_date == pm_date_ref ) then
               ! Conversion de la date en jj fractionnaires
               date_jjfrac = real(jj_ref, kind=pm_reel) &
                             + sec_ref / 86400._pm_reel               
            else
               ! Conversion de la date en jj fractionnaires
               date_jjfrac = real(jj, kind=pm_reel) + sec / 86400._pm_reel
            endif
         endif
      endif

      ! Appel à la méthode de lecture et d'interpolation
      call gs_lire_poletsid(date_jjfrac, fichierpoletsid, numpla, &
         alpha, delta, tsid, dtsid, retour_interpol)

      ! Traitement d'une erreur, affichage de valeurs farfelues
      if ( retour_interpol < 0 ) then
         write(0, *) "Erreur lors de la lecture du fichier du modele (", &
              trim(fichierpoletsid), ")" 
         dtsid = 0.0_pm_reel
      endif

   end subroutine gsi_lire_vitrot_poletsid

