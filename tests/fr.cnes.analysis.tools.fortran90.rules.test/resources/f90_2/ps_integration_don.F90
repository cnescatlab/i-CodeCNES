module ps_integration_don


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_integration_don
!
!$Resume
!  Module décrivant les données nécessaires à l'intégration.
!
!$Description
!  Module décrivant les données nécessaires à l'intégration.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_integration_don.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_integration_don.F90,v $
!  Revision 1.21  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.20  2008/12/04 15:42:05  tanguyy
!  AQ : suppression de variables inutilisees
!
!  Revision 1.19  2008/12/02 16:51:55  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.18  2008/11/26 08:24:24  tanguyy
!  DM-ID 733 : les matrices RI-EME2000 et EME2000-RI sont bien de dimension (6,6) mais on utilise seulement la partie mat(1:3,1:3) pour les multiplications de vecteur de dim 3
!  Revision 1.17  2008/11/18 13:38:08  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.16  2008/04/29 17:22:10  tanguyy
!  DM-ID 964 et  FA-ID 886 : le test se fait desormais sur l'ecart entre la date precedente et la date courante : s'il est inferieur a 10-6 sec, l'integrateur n'est pas appele
!  Revision 1.15  2008/04/02 13:47:30  tanguyy
!  DM-ID 983 : utilisation d'un epsilon 10+4 fois plus grand pour la variable d'integration
!  Revision 1.14  2008/03/19 15:21:19  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.13  2008/01/29 16:18:52  tanguyy
!  Annulation des tests pour la FA 855 -> en attente de realisation de DM 964 pour obtenir une solution viable au probleme de la reinitialisation de l'integrateur lors d'un aller-retour du Cowell
!  Revision 1.11  2007/09/24 15:06:18  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.10  2007/07/09 12:21:26  tanguyy
!  DM-ID 702, 748, 692 : utilisation des structures MSP_INTEGRATOR
!  Revision 1.9  2006/10/19 15:09:43  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.8.2.1  2006/10/13 07:55:24  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.8  2006/03/15 13:25:19  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.7  2006/02/27 15:55:44  tanguyy
!  DM-ID 397 : adaptation de la structure str_int pour manipuler les integrateurs MSPRO
!  Revision 1.6  2005/11/10 18:37:07  vivaresf
!  Mise à jour des cartouches
!  Revision 1.5  2005/01/28 10:42:12  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.4  2005/01/17 15:26:44  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.3  2004/03/31 14:30:17  adm_ipsi
!  FA-ID 117, Passage matrice 6,6 pour Mat_RI_G50, Mat_RI_ME2000 et Mat_RI_ROUT
!  Revision 1.2  2002/11/26 16:59:40  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.6  2001/11/07 13:11:20  util_am
!  Possibilité de choisir entre G50 CNES et J2000 pour le RIS en mode Terre
!  Revision 1.5  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  1999/10/26 10:55:32  util_am
!  Centralisation des matrices de passage RI/ROUT/EME2000/MME2000
!  Revision 1.3  1999/08/31 11:56:11  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:14  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_integration_don
!
!$Structure
!
!: PS_STR_INT_INTEGRATION : 
!>     h                         : <pm_reel>            pas d'intégration courant (s)
!>     h1                        : <pm_reel>            altitude au dessus de laquelle on est avec Cowell + xpash0  (m)
!>     h2                        : <pm_reel>            altitude en dessous de laquelle on passe à Runge Kutta (m)
!>     hstop                     : <pm_reel>            altitude d'arrêt (m)
!>     h3                        : <pm_reel>            
!>     second_membre_simplifie   : <integer>            utilisation ou non du second membre simplifié
!>     pinteg_courant            : <MSP_INTEGRATOR>     structure intégrateur courante
!>     pinteg0                   : <MSP_INTEGRATOR>     structure intégrateur utilisée au dessus de h1
!>     pinteg1                   : <MSP_INTEGRATOR>     " entre h2 et h1
!>     pinteg2                   : <MSP_INTEGRATOR>     " entre h3 (ou hstop) et h2
!>     pinteg3                   : <MSP_INTEGRATOR>     " entre hstop et h3
!>     type_alt                  : <integer>            
!>     tmax                      : <pm_reel>            date max (s)
!>     tsign                     : <pm_reel>            sens de l'intégration:
!.					        	 1 => posigrade
!.					        	-1 => rétrograde
!>     itsgn                     : <integer>            sens de l'intégration:
!.					        	 1 => posigrade
!.					        	-1 => rétrograde
!>     reps                      : <pm_reel,DIM=(10)>   définition du repère d'intégration
!>     echds                     : <integer>            échelle de date du repère d'intégration
!>     num_plas                  : <integer>            numéro de la planète du repère d'intégration
!>     Mat_RI_ME2000             : <pm_reel,DIM=(6,6)>  Matrice de passage du Repère d'Intégration au ME2000
!>     Mat_ME2000_RI             : <pm_reel,DIM=(6,6)>  Matrice de passage du Repère ME 2000 au repère d'intégration
!>     Mat_RI_ROUT               : <pm_reel,DIM=(6,6)>  Matrice de passage du Repère d'Intégration au repère de sortie
!>     teta_RI_g50               : <pm_reel>            Rotation entre de le repère d'intégration (Planeto. inertiel) et le Veis de la date
!>     type_sortie               : <integer>            pas (cf str_ecr%ipas)      
!>     integrateur_cowell        : <tm_integrateur>     structure MSPRO                                     
!>     init_cowell               : <integer>            flag indiquant l'initialisation de l'intégrateur    
!>     pas_cowell                : <PM_REEL>            pas utilisé pour l'intégration                                        
!>     date_init_cowell          : <tm_jour_sec>        date initiale de l'intégration avec COWELL          
!>     date_prec_cowell          : <tm_jour_sec>        date précédente d'appel au COWELL
!>     integrateur_rkutta        : <tm_integrateur>     structure MSPRO     
!>     init_rkutta               : <integer>            flag indiquant l'initialisation de l'intégrateur                                     
!>     pas_rkutta                : <PM_REEL>            pas d'intégration pour Runge-Kutta (Gill)    
!
!$Global
!
!>  str_int          : <PS_STR_INT_INTEGRATION,DIM=(PS_NVMAX)>  type structure integration
!>  PS_COEFF_MULTI   : <pm_reel,parameter>                      
!$Common
!
!$Routines
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- MSP_INTEGRATOR_DEF
!- ps_generalites
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
   use MSP_INTEGRATOR_DEF, only : msp_integrator, msp_creer_integrator, msp_consulter_integrator, msp_modifier_integrator
   use ps_generalites

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_integration_don.F90 69 2012-09-11 08:33:34Z ffsm $'


   type PS_STR_INT_INTEGRATION
      real (KIND=pm_reel) :: h
      
      real (KIND=pm_reel) :: h1
      real (KIND=pm_reel) :: h2
      real (KIND=pm_reel) :: hstop
      real (KIND=pm_reel) :: h3
      
      ! DM-ID 983 : second membre simplifié
      integer :: second_membre_simplifie

      ! On recopie dans pinteg_courant les paramètres courants
      ! de l'intégrateur (pas, ordre, epsilons, etc.)
      type(MSP_INTEGRATOR) :: pinteg_courant
      
      type(MSP_INTEGRATOR):: pinteg0
      type(MSP_INTEGRATOR):: pinteg1
      type(MSP_INTEGRATOR):: pinteg2
      type(MSP_INTEGRATOR):: pinteg3

      integer             :: type_alt
      real (KIND=pm_reel) :: tmax
      real (KIND=pm_reel) :: tsign
      integer :: itsgn
      real (KIND=pm_reel), dimension(10) :: reps
      integer :: echds
      integer :: num_plas
      ! Matrices de passage RI <-> EME 2000
      real (KIND=pm_reel), dimension(6,6) :: Mat_RI_ME2000
      real (KIND=pm_reel), dimension(6,6) :: Mat_ME2000_RI
      ! Matrice de passage RI <-> Repère de sortie
      real (KIND=pm_reel), dimension(6,6) :: Mat_RI_ROUT
      real (KIND=pm_reel) :: teta_RI_g50
      !/ Variables spécifiques aux intégrateurs
      integer :: type_sortie                         ! Intégration sur un pas ou sur une durée > pas 
      type(tm_integrateur) :: integrateur_cowell     ! structure MSPRO 
      integer :: init_cowell                         ! flag indiquant l'initialisation de l'intégrateur
      real(kind=PM_REEL) :: pas_cowell               ! pas utilisé
      type(tm_jour_sec)  :: date_init_cowell         ! date initiale de l'intégration avec COWELL (jj/sec)
      type(tm_jour_sec)  :: date_prec_cowell         ! date d'appel précédente (jj/sec) -> sert à savoir si on doit appeler l'intégrateur
      ! ou simplement rendre les mêmes paramètres
      type(tm_integrateur) :: integrateur_rkutta     ! structure MSPRO
      integer :: init_rkutta                         ! flag indiquant l'initialisation de l'intégrateur
      real(kind=PM_REEL) :: pas_rkutta                          ! pas d'intégration pour Runge-Kutta (Gill)
   end type PS_STR_INT_INTEGRATION

   type (PS_STR_INT_INTEGRATION) :: str_int(PS_NVMAX)
   
   ! Coefficient multiplicatif des epsilons du Cowell
   ! Ex : si l'epsilon de progression est de 10-10, alors les epsilons utilisés seront : 
   !  10e-10 pour les variables intégrées (équation du mouvement)
   !  10e-06 pour la variable d'intégration (anomalie)
   real(kind=pm_reel), parameter :: PS_COEFF_MULTI = 1e+04_pm_reel 

end module ps_integration_don
