module MECASPA

!******************************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MECASPA
!
!$Resume
!  Module "chapeau" de la librairie MECASPA.
!
!$Description
!  Module "chapeau" de la librairie MECASPA.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MECASPA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MECASPA.F90,v $
!  Revision 1.13  2010/10/20 09:39:54  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.12  2008/11/19 13:28:14  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.11  2008/02/22 13:58:35  huec
!  FA-ID 968 : Ajout du IMPLICIT NONE manquant
!  Revision 1.10  2007/10/23 15:03:11  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.9  2007/10/16 12:54:52  jpi
!  DM-ID744:modele Venus petropoulos88
!  Revision 1.8  2007/06/08 13:08:09  couturis
!  Correction problme: manque le module MSP_POTENTIEL_DEF
!  Revision 1.7  2007/03/19 09:39:12  tanguyy
!  AQ : mise a jour des cartouches
!  Revision 1.6  2007/01/25 14:35:17  vivaresf
!  DM-ID 643 : prise en compte du modèle ARPEGE
!  Revision 1.5  2006/11/15 10:09:34  tanguyy
!  AQ : mise a jour des commentaires dans les cartouches
!  Revision 1.4  2006/11/09 09:13:49  mle
!  DM-ID 487 : noms des parameter dans MECASPA
!  Revision 1.3  2005/03/08 07:32:32  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.2  2004/05/03 14:59:11  vivaresf
!  DM-ID 83,dates en jour / secondes avec origine MJD1950 ou MJD2000
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.4  2000/06/15 09:02:18  util_am
!  Mise à jour des cartouches
!  Revision 1.3  1999/09/22 13:31:26  util_am
!  mise a jour du cartouche module
!  Revision 1.2  1999/09/03 16:42:36  util_am
!  MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!  MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!  MECASPA.F90 : Ajout des use aux nouveaux modules
!  Revision 1.1.1.1  1999/07/13 08:37:56  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MECASPA
!
!$Structure
!
!$Global
!
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
!- MSPRO
!- MSP_MECASPA_DEF
!- MSP_GESTION_ERREUR
!- MSP_ACCES
!- MSP_MATH
!- MSP_TIRAGE
!- MSP_BULLETIN_DEF
!- MSP_VEHICULE_DEF
!- MSP_MODELE_DEF
!- MSP_SCENARIO
!- MSP_MODVENT_DEF
!- MSP_POTENTIEL_DEF
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!******************************************************************************

   use MSPRO

   use MSP_MECASPA_DEF

   use MSP_GESTION_ERREUR
   use MSP_ACCES

   use MSP_MATH
   use MSP_TIRAGE

   use MSP_BULLETIN_DEF
   use MSP_VEHICULE_DEF
   use MSP_MODELE_DEF
   use MSP_SCENARIO
   use MSP_MODVENT_DEF
   use MSP_POTENTIEL_DEF

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MECASPA.F90 69 2012-09-11 08:33:34Z ffsm $'


 end module MECASPA
