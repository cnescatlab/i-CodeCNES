module MSP_MECASPA_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_MECASPA_DEF
!
!$Resume
!  Données de base de la librairie MECASPA.
!
!$Description
!  Données de base de la librairie MECASPA.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_MECASPA_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_MECASPA_DEF.F90,v $
!  Revision 1.10  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.9  2009/10/28 17:17:10  mercadig
!  DM-ID 1018: Gestion des types de reperes inertiels et mise a jour cartouche
!
!  Revision 1.8  2008/11/12 10:27:18  mercadig
!  DM-ID 733: Definition de MSP_EPSILON
!
!  Revision 1.7  2008/02/22 13:55:01  huec
!  FA-ID 968 : Ajout du IMPLICIT NONE manquant
!  Revision 1.6  2007/06/18 10:14:24  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.5  2005/03/08 07:32:35  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.4  2002/12/09 16:45:37  adm_ipsi
!  Utilisation du parametre MSP_ENUM_ECRAN pour les sorties ecran
!  Revision 1.3  2002/12/04 18:08:24  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.2  2002/11/07 18:39:04  adm_ipsi
!  Ajout des constantes définissant les repères d'attitude
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.2  2000/06/14 16:13:47  util_am
!  Mise à jour des cartouches
!  Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_MECASPA_DEF
!
!$Structure
!
!$Global
!
!>  MSP_ENUM_NUL                   : <integer,parameter>  paramètre égal à 0
!>  MSP_ENUM_NON                   : <integer,parameter>  paramètre égal à 0
!>  MSP_ENUM_OUI                   : <integer,parameter>  paramètre égal à 1
!>  MSP_RECMAX                     : <integer,parameter>  longueur maximum de lecture/écriture (10000 caractères)
!>  MSP_ENUM_ABSENT                : <integer,parameter>  paramètre égal à -10
!>  MSP_ENUM_ATTI_INERTIEL_G50     : <integer,parameter>
!>  MSP_ENUM_ATTI_INERTIEL_J2000   : <integer,parameter>
!>  MSP_ENUM_ATTI_INERTIEL_Gvrai   : <integer,parameter>
!>  MSP_ENUM_ATTI_QSW              : <integer,parameter>  
!>  MSP_ENUM_ATTI_TNW              : <integer,parameter>  
!>  MSP_ENUM_ATTI_POINTE_SOLAIRE   : <integer,parameter>  
!>  MSP_ENUM_ATTI_TOPO_LOCAL       : <integer,parameter>  
!>  MSP_ENUM_ATTI_AERODYNAMIQUE    : <integer,parameter>  
!>  MSP_ENUM_ATTI_LVLH             : <integer,parameter>  
!>  MSP_ENUM_ATTI_YAW_STEERING     : <integer,parameter>  
!>  MSP_LONG_CHAINE                : <integer,parameter>  longueur maximale autorisée pour les chaines de caractères
!>  MSP_LONG_NOMFIC                : <integer,parameter>  
!>  MSP_ENUM_ECRAN                 : <integer,parameter>  numéro logique de sortie écran (6)
!>  MSP_EPSILON                    : <pm_reel,parameter>  
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
!- MSLIB
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
! MECASPA 
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MSLIB, only : pm_reel
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_MECASPA_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


   integer, parameter :: MSP_ENUM_NUL = 0
   integer, parameter :: MSP_ENUM_NON = 0
   integer, parameter :: MSP_ENUM_OUI = 1

   integer, parameter :: MSP_RECMAX = 10000

   ! Valeur retournée quand on essaye de consulter un champ non alloué d'une structure :

   integer, parameter :: MSP_ENUM_ABSENT = -10

   ! Type de repères d'attitude :
   ! MSP_ENUM_ATTI_INERTIEL est obsolète (à partir de V4.9)
   ! Dans le cas où l'utilisateur continue à se servir de MSP_ENUM_ATTI_INERTIEL, on  
   ! remontera une erreur
   integer, parameter :: MSP_ENUM_ATTI_INERTIEL       = -1 
   integer, parameter :: MSP_ENUM_ATTI_INERTIEL_G50   = 0  
   integer, parameter :: MSP_ENUM_ATTI_INERTIEL_J2000 = 1  
   integer, parameter :: MSP_ENUM_ATTI_INERTIEL_GVrai = 2  
   integer, parameter :: MSP_ENUM_ATTI_QSW            = 3  
   integer, parameter :: MSP_ENUM_ATTI_TNW            = 4  
   integer, parameter :: MSP_ENUM_ATTI_POINTE_SOLAIRE = 5  
   integer, parameter :: MSP_ENUM_ATTI_TOPO_LOCAL     = 6  
   integer, parameter :: MSP_ENUM_ATTI_AERODYNAMIQUE  = 7  
   integer, parameter :: MSP_ENUM_ATTI_LVLH           = 8  
   integer, parameter :: MSP_ENUM_ATTI_YAW_STEERING   = 9

   ! Longueur des chaines

   integer, parameter :: MSP_LONG_CHAINE   = 80

   ! Longueur des noms de fichiers (doit remplacer MSP_LONG_CHAINE pour les noms de fichiers)
   integer, parameter :: MSP_LONG_NOMFIC   = 256

   ! Sortie ecran

   integer, parameter :: MSP_ENUM_ECRAN   = 6
   
   ! Définition de epsilon
   real(kind=pm_reel), parameter :: MSP_EPSILON = 1.e-15_pm_reel

end module MSP_MECASPA_DEF
