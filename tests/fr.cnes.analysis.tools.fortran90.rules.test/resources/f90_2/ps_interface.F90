module INTERFACE_PSIMU

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  INTERFACE_PSIMU
!
!$Resume
!  Module d'interface pour l'appel de PSIMU en mode sous-programme Fortran90.
!
!$Description
!  Module d'interface pour l'appel de PSIMU en mode sous-programme Fortran90.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_interface.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_interface.F90,v $
!  Revision 1.10  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.9  2009/03/17 16:02:22  tanguyy
!  DM-ID 1227 : 2nde phase / utilisation des flags reinit_bulletin et reinit_calcul
!
!  Revision 1.8  2008/12/02 16:51:54  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.7  2006/10/17 09:54:25  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.6  2006/03/15 13:25:19  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.5  2005/11/10 18:37:08  vivaresf
!  Mise à jour des cartouches
!  Revision 1.4  2005/01/28 10:42:46  fabrec
!  maj cartouches
!  Revision 1.3  2005/01/17 15:27:37  fabrec
!  DM-ID 175 : maj des cartouches
!  Revision 1.2  2002/11/26 17:00:05  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.5  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  2000/01/13 13:28:05  util_am
!  Ajout de pswresu dans l'interface
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:15  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use INTERFACE_PSIMU
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psimu
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_interface_psimu_don
!- ps_spsimu
!- ps_ecriture
!#
!
!$Interface
!> psimu :  psimu90
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_interface_psimu_don
   use ps_initialisations, only : ps_terminer_session_PSIMU
   use ps_spsimu, only : psimu90
   use ps_ecriture, only : pswresu

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_interface.F90 69 2012-09-11 08:33:34Z ffsm $'


   interface psimu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psimu
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!
!$Procedures
!- psimu90
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure psimu90
   end interface

end module INTERFACE_PSIMU
