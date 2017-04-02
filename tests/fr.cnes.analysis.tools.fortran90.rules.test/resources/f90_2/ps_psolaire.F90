module ps_psolaire

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_psolaire
!
!$Resume
!  Module regroupant les sous-programmes permettant de calculer la pression de radiation solaire.
!
!$Description
!  Module regroupant les sous-programmes permettant de calculer la pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_psolaire.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_psolaire.F90,v $
!  Revision 1.13  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.12  2009/02/13 14:51:20  tanguyy
!  FA-ID 1142 (AQ) : utilisation de variables constantes pour definir les coefs
!
!  Revision 1.11  2009/02/13 10:34:08  tanguyy
!  FA-ID 1142 : rajout d'explications sur le calcul du coef
!
!  Revision 1.10  2009/02/13 10:30:42  tanguyy
!  FA-ID 1142 : rajout de coefs multiplicatifs pour moduler la force de PRS sur Mars et Venus
!
!  Revision 1.9  2008/12/02 16:53:01  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.8  2008/12/02 10:47:01  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.7  2008/11/26 08:50:57  tanguyy
!  DM-ID 733 : extraction du calcul de la pression solaire dans MECASPA ; utilisation des structures mci et prad du véhicule
!  Revision 1.6  2008/11/07 09:52:34  huec
!  DM-ID 733 : Gestion d\'erreur
!  Revision 1.5  2008/11/03 17:17:31  huec
!  DM-ID 733 : Changement de module pour les routines de calcul de pression de radiation solaire
!  Revision 1.4  2008/11/03 15:22:46  huec
!  DM-ID 733 : Calcul de l\'acceleration de radiation solaire par la MECASPA
!  Revision 1.3  2008/10/17 10:15:23  mercadig
!  DM-ID 1112 & 1058 : Passage des bons parametres a psaccsurf et initialisations de variables
!  Revision 1.2  2008/04/03 14:30:41  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.1  2007/12/06 19:25:31  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.11  2006/10/17 09:54:28  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.10  2006/03/15 13:22:29  tanguyy
!  FA-ID 494 : Cloture du FT (Bug dans le calcul de la pression de radiation solaire)
!  Revision 1.9.2.2  2006/03/15 13:22:18  tanguyy
!  Exposant borne au min (-600) et au max (+600)
!  Revision 1.9.2.1  2006/03/06 08:58:38  tanguyy
!  FA-ID 494 : Version initiale du FT (Bug dans le calcul de la pression de radiation solaire)
!  Revision 1.9  2005/11/10 18:37:10  vivaresf
!  Mise à jour des cartouches
!  Revision 1.8  2005/01/28 10:39:42  fabrec
!  maj cartouches
!  Revision 1.7  2005/01/17 15:29:10  fabrec
!  DM-ID 175 : modatt
!  Revision 1.6  2003/07/10 10:10:09  adm_ipsi
!  FA-ID 13 : Controle que la masse >0
!  Revision 1.7  2003/03/28 09:56:47  util_am
!  SLB - Version industrialisée
!  Revision 1.5  2002/12/06 11:13:18  boschett
!  Ajout des parentheses dans l'expression complexe de la routine psprsol
!  Revision 1.4  2002/12/04 14:26:38  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.3  2002/12/02 17:05:34  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.2  2002/11/26 17:04:42  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.6  2000/06/27 11:42:40  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.5  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  1999/10/27 11:24:18  util_am
!  Prise en compte des panneaux solaires dans le calcul des forces
!  Revision 1.3  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:18  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_psolaire
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psprsol
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
!- ps_calcul_forces
!- ps_attitude
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
  character(len=256), private :: SVN_VER =  '$Id: ps_psolaire.F90 69 2012-09-11 08:33:34Z ffsm $'


   contains

      subroutine psprsol (y,us,vs,ws,pgamav,irepa,accpra)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psprsol
!
!$Resume
!  Calcul de l'accélération produite par la pression de radiation solaire.
!
!$Description
!  Calcul de l'accélération produite par la pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psprsol (y,us,vs,ws,pgamav,irepa,accpra)
!.    real (KIND=pm_reel) :: y(3),us,vs,ws,pgamav(3,3)
!.    integer :: irepa
!.    real (KIND=pm_reel) :: accpra(3)
!
!$Arguments
!>E     y       :<pm_reel,DIM=(3)>     position du véhicule dans le repère d'intégration (m)
!>E     us      :<pm_reel>             Soleil
!>E     vs      :<pm_reel>             Soleil
!>E     ws      :<pm_reel>             Soleil
!>E     pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!>E     irepa   :<integer>             type de repère "local" pour la définition de l'attitude
!>S     accpra  :<pm_reel,DIM=(3)>     accélération dans le repère d'intégration (m/s^2)
!
!$Common
!
!$Routines
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_consulter_prad
!- MSP_flux_solaire
!- MSP_calculer_pre_solaire
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_caracteristiques
!- ps_calcul_forces
!- ps_attitude
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_bulletin
      use ps_caracteristiques
      use ps_calcul_forces
      use ps_attitude

      implicit none

      real (KIND=pm_reel), intent(IN)  :: y(3),us,vs,ws,pgamav(3,3)
      integer, intent(IN)              :: irepa
      real (KIND=pm_reel), intent(OUT) :: accpra(3)

      real (KIND=pm_reel) :: cp, pkreg,gkreg,consol,masse_totale,masse_struct,coef_consol
      real (KIND=pm_reel) :: accel,dirsol(3)
      real (KIND=pm_reel) :: mat_att(3,3)

      real (kind=pm_reel), parameter :: psi_coef_venus = 1.913_pm_reel
      real (kind=pm_reel), parameter :: psi_coef_mars  = 0.433_pm_reel

      type(MSP_PRAD) :: prad
      type(MSP_MCI) :: mci

      data pkreg/90._pm_reel/,gkreg/20._pm_reel/,consol/.465e-5_pm_reel/

      dirsol(:) = (/ us , vs , ws /)
      
      ! "Fausse" matrice de passage pour les panneaux solaires dans le cas du YAW_STEERING:
      if ( irepa == MSP_ENUM_ATTI_YAW_STEERING ) then
         mat_att(1,:) = dirsol(:)
         mat_att(2,:) = dirsol(:)
         mat_att(3,:) = dirsol(:)
      else
         mat_att(:,:) = pgamav(:,:)
      endif


      call MSP_consulter_vehicule(str_car(iveh)%vehicule,prad=prad,mci=mci)

      call MSP_consulter_mci(mci,mstr=masse_struct)

      masse_totale = masse_struct 

      call MSP_consulter_prad (prad,cmp=cp)

      ! Calcul de l'accélération due à la pression de radiation solaire
      ! Les valeurs de coefficients (pkreg, gkreg, consol) sont historiques, et sont a priori
      ! valables pour la Terre.
      ! Pour les autres planètes (Mars et Vénus), on applique un coefficient multiplicatif sur "consol" 
      ! qui est fonction du rapport des distances (Soleil-Planète) et (Soleil-Terre) au carré.
      ! Coef multiplicatif = [ (distance Planete-Soleil) / (distance Terre-Soleil) ]**2
      ! Les valeurs fournies par le CNES sont 1.913 pour Vénus et 0.433 pour Mars
      !================================================================================

      if (str_gen(iveh)%planet == eph_mars) then
         ! cas Mars : Mars est plus éloignée du Soleil que la Terre, le coef multiplicatif est < 1
         coef_consol = psi_coef_mars * consol
      else if (str_gen(iveh)%planet == eph_venus) then
         ! cas Vénus : Vénus est proche du soleil, 
         ! on applique un coef multiplicatif (rapport des distances Vénus-Soleil / Terre-Soleil )
         ! pour augmenter la force de PRS
         coef_consol = psi_coef_venus * consol
      else
         ! cas Terre
         coef_consol = consol
      end if

      call MSP_flux_solaire(y, dirsol, cp, masse_totale, str_mod(iveh)%requa, &
           pkreg, gkreg, coef_consol, accel)
      if (MSP_gen_messages("psprsol" )) return

      call MSP_calculer_pre_solaire(prad, accel, mci, dirsol, pgamav, accpra, modatt=modatt(iveh), &
           panneaux=1, mat_att_panneaux=mat_att)
      if (MSP_gen_messages("psprsol" )) return

      end subroutine psprsol

end module ps_psolaire
