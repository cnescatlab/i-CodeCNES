module ps_propulsion_don

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_propulsion_don
!
!$Resume
!  Module écrivant les données nécessaires aux poussées.
!
!$Description
!  Module écrivant les données nécessaires aux poussées.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_propulsion_don.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_propulsion_don.F90,v $
!  Revision 1.21  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.20  2008/12/04 15:41:48  tanguyy
!  FA-ID 1100 : maj des tests pour la creation / suppression de l'integrateur pour la propulsion
!
!  Revision 1.19  2008/12/02 16:53:05  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.18  2008/12/02 10:47:38  huec
!  DM-ID 1058 : Initialisation de fpsxm dans le cas d\'une masse nulle
!  Revision 1.17  2008/11/26 08:36:51  tanguyy
!  DM-ID 733 : caracteristiques véhicules "lues" dans la structure MSP_VEHICULE + maj des commentaires
!  Revision 1.16  2008/09/11 16:03:04  tanguyy
!  DM-ID 1058 : controle des desallocations d'integrateurs pour les propulsions
!  Revision 1.15  2008/09/04 07:53:15  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.14  2006/10/17 09:54:28  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.13  2006/03/15 13:25:21  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.12  2005/11/10 18:37:09  vivaresf
!  Mise à jour des cartouches
!  Revision 1.11  2005/01/28 10:42:12  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.10  2005/01/25 16:20:55  fabrec
!  DM-ID 175
!  Revision 1.9  2005/01/17 15:28:46  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.8  2004/11/22 14:34:19  vivaresf
!  FA-ID 200 : gestion de la variable vitrot pour
!  les propulsion buleltin
!  Revision 1.7  2004/11/22 08:52:49  vivaresf
!  DM_ID 200 : lecture des requa et apla des utilises a l'IHM pour les conversions de type
!  Revision 1.6  2003/08/08 14:17:52  adm_ipsi
!  Ajout de la routine psapous
!  Revision 1.5  2003/03/14 15:25:27  adm_ipsi
!  Utilisation de ps_nloi_att, ps_nloi_propu, ps_npts_att, ps_npts_propu, ps_nsepa
!  Revision 1.4  2003/02/18 17:00:27  rodier
!  PhB - Suppression du tableau ibref (referentiel) non utilisé
!  Revision 1.3  2003/02/14 16:18:03  rodier
!  PhB - ajout variables pour définition des nouveaux repères
!  Revision 1.2  2002/11/26 17:04:21  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.7  2002/09/16 11:06:02  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.6  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.5  2000/02/08 09:51:05  util_am
!  Ajout du tableau parman_car pour tenir compte d'un delta de date lors d'une réinitialisation de bulletin
!  Revision 1.4  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.3  1999/08/31 11:56:13  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:18  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_propulsion_don
!
!$Structure
!
!$Global
!
!>  scenar_pro              : <MSP_SCENARIO_LOI,DIM=(PS_NVMAX)>  scenario de propulsion
!>  nloi_scenar_pro         : <integer,DIM=(PS_NVMAX)>           nombre de lois du scenario de propulsion
!>  iloi_scenar_pro         : <integer,DIM=(PS_NVMAX)>           indice de la loi de propulsion courante
!>  parman_car              : <pm_reel,DIM=(ps_nloi_propu,6,2)>  tableau ajoute pour tenir compte 
! d'un delta de date lors d'une réinitialisation de bulletin
!>  nimp                    : <integer,DIM=(ps_nloi_propu)>      tableau indiquant si la poussée a été réalisée (0) ou non (1)
!>  xmerg_21                : <pm_reel>                          
!>  integ_propulsion        : <tm_integrateur,DIM=(PS_NVMAX)>    
!>  init_integ_propulsion   : <integer,DIM=(PS_NVMAX)>           
!$Common
!
!$Routines
!- psapous
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_caracteristiques
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
   use ps_caracteristiques

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_propulsion_don.F90 69 2012-09-11 08:33:34Z ffsm $'


   type(MSP_SCENARIO_LOI), dimension(PS_NVMAX), save :: scenar_pro
   integer, dimension(PS_NVMAX) :: nloi_scenar_pro
   integer, dimension(PS_NVMAX) :: iloi_scenar_pro
   real (KIND=pm_reel), dimension(ps_nloi_propu,6,2) :: parman_car
!  nimp : tableau indiquant si la poussée a été réalisée (0) ou non (1)
   integer, dimension(ps_nloi_propu)   :: nimp
   real (KIND=pm_reel)      :: xmerg_21

   type(tm_integrateur), dimension(PS_NVMAX) :: integ_propulsion
   integer, dimension(PS_NVMAX) :: init_integ_propulsion
   real(KIND=PM_REEL), dimension(PS_NVMAX) :: pas_integ_propulsion       ! Le pas est tjs > 0.0

  contains

      subroutine psapous (pgamav,fp,omega,omegap,accv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psapous
!
!$Resume
! Calcul de l'accélération produite par une poussée.
!
!$Description
! Calcul de l'accélération produite par une poussée.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psapous (pgamav,fp,omega,omegap,accv)
!.    real (KIND=pm_reel) :: pgamav(3,3),fp,omega,omegap
!.    real (KIND=pm_reel) :: accv(3)
!
!$Arguments
!>E     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule 
!>E     fp      :<pm_reel>             poussée des moteurs (N) 
!>E     omega   :<pm_reel>             direction de poussée dans le plan x,z véhicule
!>E     omegap  :<pm_reel>             direction de poussée perpendiculaire au plan x,z véhicule  
!>S     accv    :<pm_reel,DIM=(3)>     accélération exprimée dans le repère d'intégration (m/s^2)
!
!$Common
!
!$Routines
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!
!$Include
!
!$Module
!#V
!- ps_caracteristiques
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use ps_caracteristiques

      implicit none

      ! Arguments
      !=============
      real (KIND=pm_reel), intent(IN)  :: pgamav(3,3),fp,omega,omegap
      real (KIND=pm_reel), intent(OUT) :: accv(3)
      
      ! Variables locales
      !====================
      type (MSP_MCI) :: mci
      real (KIND=pm_reel) :: acc(3),comp,fpsxm,masse


!     Composantes 3 axes de l'acceleration:

      comp  = cos(omegap)
      
      call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
      call MSP_consulter_mci(mci,mstr=masse)

      if (masse > MSP_EPSILON_APLA) then
         fpsxm = fp/masse
      else
         fpsxm = 0._pm_reel
      end if

      acc(1)=fpsxm*comp*cos(omega)
      acc(2)=fpsxm*sin(omegap)
      acc(3)=fpsxm*comp*sin(omega)

      ! Acceleration dans le repere Planeto :

      accv(1)=pgamav(1,1)*acc(1)+pgamav(2,1)*acc(2)+pgamav(3,1)*acc(3)
      accv(2)=pgamav(1,2)*acc(1)+pgamav(2,2)*acc(2)+pgamav(3,2)*acc(3)
      accv(3)=pgamav(1,3)*acc(1)+pgamav(2,3)*acc(2)+pgamav(3,3)*acc(3)

      end subroutine psapous


end module ps_propulsion_don
