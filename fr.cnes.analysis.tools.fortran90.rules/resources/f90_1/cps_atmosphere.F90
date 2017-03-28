module   CPS_ATMOSPHERE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  CPS_ATMOSPHERE
!
!$Resume
!  Modèles d'Atmosphère (martiens, terrestres et venusiens)
!
!$Description
!  Modèles d'Atmosphère (martiens, terrestres et venusiens)
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_atmosphere.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atmosphere.F90,v $
!  Revision 1.18  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.17  2009/08/21 09:40:36  cml
!  DM-ID 1120 : Ajout du modele exponentiel multi-planetes
!
!  Revision 1.16  2007/11/05 12:34:54  jpi
!  DM-ID551 : split cps_atmosphere en cps_atmosphere_[venus|terre|mars]
!
!  Revision 1.15  2007/10/23 16:37:42  jpi
!  DM-ID744 : modele atmosphere Venus : cartouches
!  Revision 1.14  2007/10/18 12:51:16  jpi
!  DM-ID744 : modele atmosphere Venus, nom de variables trop long
!  Revision 1.13  2007/10/16 12:19:40  jpi
!  DM-ID744:modele Venus petropoulos88
!  Revision 1.12  2007/01/31 11:16:05  vivaresf
!  FA-ID 663 : gamma et m en sortie si Mars
!  Revision 1.11  2006/11/17 07:01:52  vivaresf
!  DM-ID 425 : code plus portage (float en real, alog en log, dsqrt en sqrt)
!  Revision 1.10  2006/11/10 10:48:19  vivaresf
!  Version 2.1 : validation
!  Revision 1.9  2006/11/08 13:08:30  vivaresf
!  FA-ID 629 : utilisation de variables locales et de variables optionnelle
!  calcul de g par mu/(requa+h0)/(requa+h0)
!  gestion d'erreur MAGE et non par stop
!  Revision 1.8  2006/10/18 09:54:30  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!  Revision 1.7.2.1  2006/09/26 12:20:11  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.7  2006/09/12 14:01:59  tanguyy
!  DM-ID 428 : Ajout de routines chapeaux pour les modeles MSIS90 / MSIS2000 et MET88
!  Revision 1.6  2006/05/30 15:19:10  vivaresf
!  regle de codage : suppression de *(*) obsolete
!  Revision 1.5  2006/05/12 12:05:57  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.4  2006/04/11 09:17:32  bouillaj
!  *** empty log message ***
!  Revision 1.3  2006/03/10 12:47:07  vpg
!  Mise a jour suite a la modification de cps_getCsteTh() ou l'unite est obligatoire
!  Revision 1.2  2006/02/17 13:08:41  bouillaj
!  Mise en place du modele exponetiel multi planete
!  Revision 1.1.1.1  2005/12/07 07:23:07  vivaresf
!  Refonte de COMPAS
!  Revision 1.7  2005/03/07 08:15:49  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!  Revision 1.6  2005/03/04 16:39:51  vivaresf
!  DM-ID 318 : traduction des entêtes
!  Revision 1.5  2005/01/27 12:57:15  vivaresf
!  DM-ID 318 : entetes des fichiers et des routines utilisateurs
!  nommage des fonctions publique, privatisation des fonctions internes
!  declaration des variables
!
!$FinHistorique
!
!$Usage
!  use CPS_ATMOSPHERE
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
!- CPS_ATMOSPHERE_TERRE
!- CPS_ATMOSPHERE_MARS
!- CPS_ATMOSPHERE_VENUS
!- CPS_ATM_EXP
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE TERRE MARS VENUS EXPONENTIEL
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  use cps_acces
  use CPS_ATMOSPHERE_TERRE
  use CPS_ATMOSPHERE_MARS
  use CPS_ATMOSPHERE_VENUS
  use CPS_ATM_EXP

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atmosphere.F90 69 2012-09-11 08:33:34Z ffsm $'


!   contains

end module CPS_ATMOSPHERE
