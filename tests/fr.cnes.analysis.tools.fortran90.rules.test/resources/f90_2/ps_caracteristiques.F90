module ps_caracteristiques

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_caracteristiques
!
!$Resume
!  Module décrivant les données du véhicule.
!
!$Description
!  Module décrivant les données du véhicule.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_caracteristiques.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_caracteristiques.F90,v $
!  Revision 1.13  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.12  2008/12/02 16:46:13  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.11  2008/11/26 08:05:58  tanguyy
!  DM-ID 733 : simplification de la structure PS_STR_INT_CARACTERISTIQUES : elle ne contient qu'une structure MSP_VEHICULE et 3 variables
!  Revision 1.10  2008/10/24 09:38:25  huec
!  DM-ID 1058 : Initialisations
!  Revision 1.9  2008/09/04 07:52:58  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.8  2007/06/21 13:16:00  vivaresf
!  FA-ID 746 : validation
!  Revision 1.7  2007/02/02 13:32:18  tanguyy
!  DM-ID 659 / DM-ID 643 : finalisation
!  Revision 1.6  2006/10/17 09:54:22  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.5  2006/03/15 13:25:17  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.4  2005/11/10 18:37:04  vivaresf
!  Mise à jour des cartouches
!  Revision 1.3  2005/01/28 10:37:46  fabrec
!  maj cartouches
!  Revision 1.2  2002/11/26 15:51:32  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.5  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  1999/10/27 11:23:53  util_am
!  Prise en compte des panneaux solaires dans le calcul des forces
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:10  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_caracteristiques
!
!$Structure
!
!: PS_STR_INT_CARACTERISTIQUES : 
!>     vehicule       : <MSP_VEHICULE>  
!>     vehiculeinit   : <logical>       
!>     typcf          : <integer>       type de coefficients de frottement:
!.					    1 => tabulés en altitude
!.					    2 => constants
!.					    3 => lus dans un fichier AERO
!>     sref           : <pm_reel>       surface de référence [m^2]
!
!$Global
!
!>  str_car   : <PS_STR_INT_CARACTERISTIQUES,DIM=(PS_NVMAX)>  type structure caracteristique
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
   use ps_generalites

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_caracteristiques.F90 69 2012-09-11 08:33:34Z ffsm $'


   type PS_STR_INT_CARACTERISTIQUES
      ! structure véhicule 
      type (MSP_VEHICULE) :: vehicule 

      ! pour savoir si le véhicule a déjà été affecté
      logical :: vehiculeinit = .false. 

      ! type de coef aéro pour distinguer le cas coef "INCIDENCE/MACH" et les autres cas
      ! -> ceci permet de ne pas dépiler les structures msp_vehicule et msp_aero à chaque 
      ! appel au calcul d'atmosphere
      integer :: typcf                  

      ! surface de référence utilisée dans le cas
      ! de frottement en incidence/mach
      real (KIND=pm_reel) :: sref  
      
   end type PS_STR_INT_CARACTERISTIQUES

   type (PS_STR_INT_CARACTERISTIQUES), dimension(PS_NVMAX), save :: str_car

end module ps_caracteristiques
