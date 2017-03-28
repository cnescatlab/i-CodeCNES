module ps_generalites

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_generalites
!
!$Resume
!  Module décrivant les données générales à PSIMU.
!
!$Description
!  Module décrivant les données générales à PSIMU.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_generalites.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_generalites.F90,v $
!  Revision 1.43  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.42  2009/11/19 15:53:54  mercadig
!  DM-ID 1019: Mise a jour cartouche PS_MODOUT
!
!  Revision 1.41  2009/07/10 13:44:34  cml
!  DM-ID 1164 : Suppression de icalsim et ajout de maj_vect_etat
!
!  Revision 1.40  2009/06/16 07:22:24  tanguyy
!  FA-ID 1306 : fusion dans le tronc commun
!  Revision 1.39.2.1  2009/06/11 14:07:59  tanguyy
!  FA-ID 1306 : rajout d'un tableau de flags indiquant l'initialisation ou non d'un véhicule (argument numveh de psimu())
!  Revision 1.39  2008/12/04 15:40:05  tanguyy
!  AQ : suppression de variables inutilisees
!  Revision 1.38  2008/12/02 16:47:23  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!  Revision 1.37  2008/11/26 08:06:40  tanguyy
!  DM-ID 733 : nouveau paramètre pour distinguer les coefs incidence/mach et les autres types de coefs
!  Revision 1.36  2008/11/18 13:34:53  tanguyy
!  DM-ID 733 : rajout d'un type enumere pour les coefs incidence/mach
!  Revision 1.35  2008/10/24 09:43:32  huec
!  AQ : Simplification de la gestion des modules
!  Revision 1.34  2008/10/20 14:36:29  mercadig
!  FA-ID 1129 : Ajout d un epsilon
!  Revision 1.33  2008/10/15 12:49:18  tanguyy
!  DM-ID 1058 : déclaration des deux fichiers résultats en variables globales dans ps_generalites
!  Revision 1.32  2008/04/09 15:13:06  ttn
!  FA-ID 940 : definition de la valeur PS_EPSILON pour comparaison de deux entites physiques
!  Revision 1.29  2008/03/07 09:59:29  huec
!  DM-ID 859 : tests des code_erreur
!  Revision 1.28  2008/02/15 16:37:19  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.27  2007/07/20 08:36:10  tanguyy
!  FA-ID 779 : gestion correcte des écritures de fichiers lorsque l'on utilise plusieurs véhicules
!  Revision 1.26  2007/07/09 11:59:08  tanguyy
!  DM-ID 688 : mode Terre/Mars basé sur le code NAIF
!  Revision 1.25  2007/06/20 12:33:29  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.24  2007/03/19 14:53:27  mle
!  DM-ID 600 : rajout de 2 variables globales (cficin,cficout2)
!  Revision 1.23  2007/01/16 11:39:01  mle
!  DM-ID 642 : Nombre de separations dans PSIMU
!  Revision 1.22  2006/10/17 09:54:22  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.21  2006/03/15 13:25:18  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.20  2006/02/09 14:48:14  tanguyy
!  FA-ID 446 : suppression de code mort
!  Revision 1.19  2005/12/15 07:13:44  vivaresf
!  Code inutile
!  Revision 1.18  2005/12/14 11:31:41  tanguyy
!  DM-ID 397 : Cloture du FT (Utiliser les intégrateurs MSPRO dans PSIMU)
!  Revision 1.17.2.1  2005/12/08 09:02:51  tanguyy
!  DM-ID 397 : version intermediaire
!  Revision 1.17  2005/11/10 18:37:06  vivaresf
!  Mise à jour des cartouches
!  Revision 1.16  2005/01/28 10:38:37  fabrec
!  maj cartouches
!  Revision 1.15  2005/01/17 15:25:11  fabrec
!  DM-ID 175 : maj des cartouches
!  Revision 1.14  2004/07/06 09:07:11  ole
!  DM_89
!  Revision 1.12.2.2  2004/07/02 09:25:36  ole
!  Mise a jour commentaire reperes MARS
!  Revision 1.12.2.1  2004/07/02 07:41:37  ole
!  DM_89, version initiale
!  Revision 1.12  2004/05/26 15:23:26  adm_ipsi
!  FA-ID 130 : indentation sur test erreur - corrigée par livraison MSLIB90_5.0.1/MSPRO_4.0.1
!  Revision 1.11  2003/08/08 14:15:52  adm_ipsi
!  PM_ID 36: passage ps_nloi_att=50, ps_nloi_propu=50
!  Revision 1.10  2003/03/19 15:25:28  laurent
!   Ajout de la function psgil4 (copy de mugil4 de la mslib77)
!  Revision 1.9  2003/03/14 15:22:51  adm_ipsi
!  nloi est remplacé par ps_nloi_att et ps_nloi_propu, npts est remplacé par ps_npts_att et ps_npts_propu, nsepa est remplacé par ps_nsepa
!  Revision 1.8  2003/02/14 16:11:38  rodier
!  PhB - ajout variable fic_tuc
!  Revision 1.7  2003/02/05 12:28:34  boschett
!  A Deramecourt : remplacement mconst en mc_math
!  Revision 1.6  2002/12/12 15:05:24  boschett
!  Livraison Intermediaire 16/12/2002
!  Revision 1.5  2002/12/04 14:23:49  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/03 17:18:32  boschett
!  Ajout de la constante symbolique PS_ECRAN
!  Revision 1.3  2002/11/27 15:49:45  boschett
!  Ajout de la constante symbolique PS_ECART_JULIEN
!  Revision 1.2  2002/11/26 15:54:15  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.14  2001/11/07 13:11:45  util_am
!  Passage à 120 variables max
!  Revision 1.13  2001/01/09 12:42:08  util_am
!  Prise en compte de la précision numérique sur une date d'entrée + passage à 25 satellites
!  Revision 1.12  2000/09/05 12:52:04  util_am
!  Passage à 10 véhicules au lieu de 100
!  Revision 1.11  2000/06/21 15:30:32  util_am
!  Gestion des ressources (fichiers de donnees)
!  Revision 1.10  2000/05/29 09:12:38  util_am
!  Utilisation de Domtraduire
!  Revision 1.9  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Conversion des variables en type structure indice
!  Revision 1.8  2000/03/01 11:53:32  util_am
!  Le nombre de points d'une loi passe de 20 à 100
!  Revision 1.7  1999/12/09 10:19:26  util_am
!  Ajout d'un mode de calcul simplifié avec sorties dans Greenwich
!  Revision 1.6  1999/12/02 16:53:28  util_am
!  Redirection des messages d'erreurs vers le standard erreur (unit=0)
!  Revision 1.5  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.4  1999/10/15 09:30:23  util_am
!  Erreur car on effacait les messages avant de tester si on était en mode ERREUR
!  Revision 1.3  1999/08/31 14:58:05  util_am
!  Meilleure prise en compte des sorties dans le cas hyperbolique
!  Revision 1.2  1999/08/04 11:28:12  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_generalites
!
!$Structure
!
!: PS_STR_INT_GENERALITES : 
!>     etat                : <pm_reel,DIM=(nvmax)>  tableau des variables courantes
!>     planet              : <integer>              Nom de la planète 'corps central'
!>     hyp                 : <integer>              traitement du cas hyperbolique
!
!: PS_STR_MAJ_VECT_ETAT :  Structure regroupant les flags commandant la màj du vecteur d'état
!>     calcul_car_ris      : <logical>              Temps, masse, position et vitesse dans le RIS
!>     calcul_att_ris      : <logical>              Attitude dans le RIS
!>     calcul_car_rts      : <logical>              Position et vitesse dans le RTS
!>     calcul_att_rts      : <logical>              Attitude dans le RTS
!>     calcul_pos_geo      : <logical>              Position géodésique
!>     calcul_vit_geo      : <logical>              Vitesse géodésique correspondant aux variables
!                                                      de la mécanique de vol
!>     calcul_kep          : <logical>              Paramètres képlériens dans le RIS
!>     calcul_forces       : <logical>              Variables intervenant dans le calcul des forces
!                                                      et accélérations résultantes
!>     calcul_force_prop   : <logical>              Varibales dont accélérations pour la propulsion
!>     calcul_kep_moy      : <logical>              Paramètres képlériens moyens dans le RIS
!>     calcul_autres       : <logical>              Variables "annexes"
!
!$Global
!
!>  PS_NVMAX            : <integer,parameter>                      nombre maximum de vehicules
!>  PS_ECRAN            : <integer,parameter>                      numero logique de la sortie écran  
!>  ps_nloi_att         : <integer,parameter>                      nombre maximum de lois d'attitude
!>  ps_npts_att         : <integer,parameter>                      nombre maximum de points pour des lois d'attitude tabulées
!>  ps_nloi_propu       : <integer,parameter>                      nombre maximum de lois de propulsion
!>  ps_npts_propu       : <integer,parameter>                      nombre maximum de points pour des lois de propulsion tabulées 
!>  ps_nsepa            : <integer,parameter>                      nombre maximum de lois de séparations                      
!>  npcf                : <integer,parameter>                      Nombre de points maximum pour les coefficients de frottement
!>  npas                : <integer,parameter>                      nombre maximum d'itérations
!>  nvmax               : <integer,parameter>                      nombre maximum de variables
!>  nbeve               : <integer,parameter>                      nombre maximum d'événements
!>  PS_INCIDENCE_MACH   : <integer,parameter>                      
!>  PS_COEFS_AUTRES     : <integer,parameter>                      
!>  dirdat              : <LEN=256>                                nom du répertoire de données de PSIMU
!>  dirres              : <LEN=256>                                nom du répertoire de résultats de PSIMU
!>  dirfcf_psimu        : <LEN=256>                                nom du répertoire de fichiers de configuration PSIMU
!>  dirfcf_gslib        : <LEN=256>                                nom du répertoire de fichiers de configuration GSLIB
!>  dirfcf_mecaspa      : <LEN=256>                                nom du répertoire de fichiers de configuration MECASPA
!>  iscreen             : <integer>                                affichage à l'écran:
!.                                                        0 => pas d'affichage écran
!.                                                        1 => affichage écran
!>  ilogeph             : <integer>                                numéro logique du fichier EPHEM
!>  ilogeve             : <integer>                                numéro logique du fichier EVENT
!>  iveh                : <integer>                                numero du vehicule
!>  flagw               : <logical>                                Flag d'affichage des warnings
!>  PS_EPSILON          : <pm_reel>                                
!>  pi                  : <pm_reel>                                nombre PI
!>  dpi                 : <pm_reel>                                2*PI
!>  pisd                : <pm_reel>                                PI/2
!>  tpisd               : <pm_reel>                                3*PI/2
!>  raddeg              : <pm_reel>                                coefficient pour passer de radians en degrés
!>  degrad              : <pm_reel>                                coefficient pour passer de degrés en radians
!>  eps                 : <pm_reel>                                coefficient de précision
!>  eps_trigo           : <pm_reel>                                coefficient de précision (1.e-16)
!>  PS_PREC_DATE        : <pm_reel>                                précision sur les dates (sec) (par défaut la milli-seconde à savoir 0.001)
!>  PS_MODOUT           : <integer>                                J2000, GAMMA 50 ou GAMMA VRAI de la date
!.                                                        sur Mars, toujours Planeto Equatorial UAI de la date
!>  clangue             : <LEN=2>                                  langue employée pour les messages (fr=français; en=anglais)
!>  nomdomaine          : <LEN=51>                                 nom du fichier où lire les messages MADONA
!>  cficin              : <LEN=80>                                 nom du fichier d'option
!>  cficout1            : <LEN=80>                                 
!>  cficout2            : <LEN=80>                                 nom du fichier evenement
!>  tab_fic_lu          : <integer>                                
!>  tab_veh_init        : <integer>                                
!>  str_gen             : <PS_STR_INT_GENERALITES,DIM=(PS_NVMAX)>  type structure generalite
!$Common
!
!$Routines
!- pslcons
!- psmess
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
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

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_generalites.F90 69 2012-09-11 08:33:34Z ffsm $'


   integer,parameter :: PS_NVMAX = 25
   integer,parameter :: PS_ECRAN = 6
   integer,parameter :: ps_nloi_att   = 50
   integer,parameter :: ps_npts_att   = 100
   integer,parameter :: ps_nloi_propu = 50
   integer,parameter :: ps_npts_propu = 100
   integer,parameter :: ps_nsepa = 10
   integer,parameter :: npcf  = 50
   integer,parameter :: npas  = 1000000000
   integer,parameter :: nvmax = 120
   integer,parameter :: nbeve = 1000

   ! type de coefs dépendant de l'incidence et du mach
   integer, parameter :: PS_INCIDENCE_MACH = 3
   integer, parameter :: PS_COEFS_AUTRES = 0

   character(LEN=256) :: dirdat
   character(LEN=256) :: dirres
   character(LEN=256) :: dirfcf_psimu
   character(LEN=256) :: dirfcf_gslib
   character(LEN=256) :: dirfcf_mecaspa

   integer :: iscreen
   integer :: ilogeph
   integer :: ilogeve
   integer :: iveh

   logical :: flagw
   
   ! FA-ID 940 : EPSILON defini pour comparaison de deux entités physiques réelles
   ! PS_EPSILON est initialisé dans pslcons()
   real (KIND=pm_reel) :: PS_EPSILON

   real (KIND=pm_reel) :: pi,dpi,pisd,tpisd,raddeg,degrad,eps,eps_trigo
   real (KIND=pm_reel) :: PS_PREC_DATE
   integer :: PS_MODOUT

   character(LEN=2)  :: clangue
   character(LEN=51) :: nomdomaine

   character(len=80) :: cficin          ! fichier d'option
   character(len=80) :: cficout1        ! fichier résultat (éphémérides)
   character(len=80) :: cficout2        ! fichier résultat (événements)

   !-- Tableau permettant de connaître l'initialisation
   ! des fichiers en mode sous-programme (FA-ID 779)
   !
   ! tab_fic_lu(1,1) = n° UL du fichier 1 (1er fichier a être écrit)
   ! tab_fic_lu(1,1) = 0 <=> le fichier d'UL  tab_fic_lu(1,1) n'a pas été initialisé (pas d'entêtes)
   ! tab_fic_lu(1,1) = 1 <=> le fichier d'UL  tab_fic_lu(1,1) a déjà ses entêtes
   !
   ! /!\ dimension 2*pnvmax : pour pouvoir écrire un fichier EPHEM et un fichier EVENT par véhicule
   integer, dimension (2,2*ps_nvmax) :: tab_fic_lu =0

   integer, dimension (PS_NVMAX) :: tab_veh_init = 0

   type PS_STR_MAJ_VECT_ETAT
      logical :: calcul_car_ris
      logical :: calcul_att_ris
      logical :: calcul_car_rts
      logical :: calcul_att_rts
      logical :: calcul_pos_geo
      logical :: calcul_vit_geo
      logical :: calcul_kep
      logical :: calcul_forces
      logical :: calcul_force_prop
      logical :: calcul_kep_moy
      logical :: calcul_autres
   end type PS_STR_MAJ_VECT_ETAT


   type PS_STR_INT_GENERALITES
      type (PS_STR_MAJ_VECT_ETAT) :: maj_vect_etat
      real(kind=pm_reel) , dimension(nvmax) :: etat
      integer :: planet
      integer :: hyp
   end type PS_STR_INT_GENERALITES

   type (PS_STR_INT_GENERALITES) :: str_gen(PS_NVMAX)

   contains

      subroutine pslcons ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pslcons
!
!$Resume
!  Initialisations de constantes
!
!$Description
!  Initialisations de constantes
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pslcons ()
!
!$Arguments
!
!$Common
!
!$Routines
!- mc_math
!- MSP_signaler_message
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
      
      type (tm_code_retour) :: code_erreur

!     CALCUL DES CONSTANTES
      call mc_math (code_erreur, pi=pi, deux_pi=dpi, pi_sur2=pisd, deg_rad=degrad, rad_deg=raddeg)
      if (code_erreur%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_erreur)
         if ( MSP_ERREUR ) return
      end if

      tpisd  = 3._pm_reel*pisd

      eps    = 1e-10_pm_reel
      
      eps_trigo = 1e-16_pm_reel

      ! FA-ID 940 : EPSILON defini pour comparaison de deux entités physiques réelles
      ! PS_EPSILON ~ 1E-308
      PS_EPSILON = tiny(1._pm_reel)

      end subroutine pslcons

      subroutine psmess (affichage_warning,nom_sp_propag)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psmess
!
!$Resume
!  Affichage de messages d'erreur ou de warning.
!
!$Description
!  Affichage de messages d'erreur ou de warning.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psmess ([affichage_warning],[nom_sp_propag])
!.    logical :: affichage_warning
!.    character(LEN=*) :: nom_sp_propag
!
!$Arguments
!>[E]   affichage_warning  :<logical>   indique si on affiche les warnings
!>[E]   nom_sp_propag      :<LEN=*>     nom de la routine propageant le problème
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_recuperer_message
!- MSP_afficher_message
!- MSP_effacer_message
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

      logical, intent(IN), optional          :: affichage_warning
      character(LEN=*), intent(IN), optional :: nom_sp_propag

      logical :: localw

      ! Z! Variable permanante pour s'assurer qu'elle est effacée à fois
      ! chaque qu'on l'affecte avec MSP_recuperer_message
      type(MSP_MESSAGE), save :: messages

      if ( present (affichage_warning) ) then
         localw = affichage_warning
      else
         localw = .false.
      endif

      ! Cas où on affiche les messages de warning:
      if ( localw .or. MSP_PROBLEME ) then

         if ( MSP_PROBLEME .and. present(nom_sp_propag) ) then
            call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME",routine=nom_sp_propag)
         endif
         call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
         call MSP_afficher_message  (message=messages,unit=0)
         if ( MSP_ERREUR ) stop
         call MSP_effacer_message  ()      

      endif

    end subroutine psmess




end module ps_generalites


