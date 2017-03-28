module eph_varglob

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_varglob
!
!$Resume
!  Module contenant les variables globales utilisées par la LIBEPHEM
!
!$Description
!. Module contenant les variables globales utilisées par
!  la LIBEPHEM. 
!. Les variables globales permettent le passage d'informations entre les 
!  sous-programmes d'initialisation et les sous-programmes 
!  de calcul.
!
!$Auteur
!  Ph. Bremard / Florence Vivares (SchlumbergerSema)
!
!$Version
!  $Id: eph_varglob.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_varglob.F90,v $
!  Revision 1.7  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2008/10/08 15:45:17  cml
!  FA-ID 1062 : Ajout de la constante decrivant le nombre max. de struct. d'un fichier
!
!  Revision 1.5  2008/10/02 13:31:44  tanguyy
!  AQ : refonte de l'initialisation des ephemerides / rajout de constantes pour les methodes d'ephemerides
!
!  Revision 1.4  2006/05/30 08:22:28  vivaresf
!  DM-ID 387 : variables inutile
!
!  Revision 1.3  2006/01/23 13:43:46  bouillaj
!  Suppression de la metheode EPROC
!
!  Revision 1.2  2005/12/08 18:39:06  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.6  2005/05/09 14:17:33  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.5  2005/03/09 09:12:05  vivaresf
!  Correction des cartouches
!  Revision 1.4  2004/12/17 14:58:11  vivaresf
!  Documentation
!  Revision 1.3  2004/05/24 16:32:51  vivaresf
!  Gestion des unites MADONA
!  Revision 1.2  2004/05/24 16:26:50  vivaresf
!  Gestion du fichier d'unites MADONA
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.15  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.14  2004/01/09 16:20:01  bremard
!  Mise à jour des cartouches
!  Revision 1.13  2004/01/07 16:12:03  bremard
!  Mise à jour cartouche
!  Revision 1.12  2003/12/31 15:55:58  bremard
!  Ajout variables fichier saut TUC + Init fichier saut TUC
!  Revision 1.11  2003/06/06 14:52:41  bremard
!  Particularisation des parametres
!  Revision 1.10  2002/10/09 07:48:39  vivaresf
!  Assurer la presence du rest_tcheb
!  Revision 1.8  2001/12/18 16:11:38  vivaresf
!  Maj documentation
!  Revision 1.7  2001/12/13 14:28:48  vivaresf
!  initialisation des fichiers
!  Revision 1.6  2001/12/11 15:19:09  vivaresf
!  Presentation de la doc
!  Revision 1.5  2001/12/07 16:44:44  vivaresf
!  Presentation fm des cartouches
!  Revision 1.4  2001/12/05 15:58:51  bremard
!  PhB - Mise à jour du cartouche
!  Revision 1.3  2001/12/04 17:31:07  bremard
!  PhB - MAJ cartouche
!  Revision 1.2  2001/12/04 16:59:32  vivaresf
!  lfneproc pour theories VSOP82 et VSOP87 methode EPROC
!  Revision 1.1  2001/11/28 13:30:06  bremard
!  PhB - Version initiale
!
!$FinHistorique
!
!$Routines
!
!$Fonctions
!
!$Module
!
!$Interface
!#V
!#
!
!$Usage
!  use eph_varglob
!
!$Structure
!
!$Global
!
!>  ephv_nbficmax      : <integer,parameter>                   ( 10) nombre max. de fichiers initialisables 
!>  ephv_lgmax         : <integer,parameter>                   (200) taille max. d'un nom de fichier
!>  ephv_maxcodes      : <integer,parameter>                   ( 50) nombre maximum de codes méthodes lus dans ephem.conf
!>  ephv_lfnnaif       : <integer,DIM=(ephv_nbficmax)>         logical file number (lfn) du fichier NAIF
!>  ephv_lfncomet      : <integer,DIM=(ephv_nbficmax)>         logical file number (lfn) du fichier comètes
!>  ephv_lfnbdl        : <integer>                             logical file number (lfn) du fichier BDL
!>  ephv_lfntche       : <integer>                             logical file number (lfn) du fichier Tchebychev
!>  codenaif           : <integer>                             code méthode NAIF en cours
!>  codetche           : <integer>                             code méthode Tchebychev en cours
!>  codetchemad        : <integer>                             code méthode Tchebychev MADONA en cours
!>  fictche            : <LEN=ephv_lgmax>                      fichier Tchebychev utilisé
!>  ficnaif            : <LEN=ephv_lgmax,DIM=(ephv_nbficmax)>  tableau des fichiers NAIF en cours
!>  fictuc             : <LEN=ephv_lgmax>                      fichier des sauts du TUC
!>  isinit             : <logical>                             variable d'initialisation générale
!>  initnaif           : <logical>                             variable d'initialisation NAIF
!>  ficerrmessage      : <logical>                             =.true. : fichier de messages d'erreur initialisé
!>  initficsauttuc     : <logical>                             =.true. : fichier de saut du TUC initialisé
!>  ephv_madona_unit   : <logical>                             =.true. : fichier MADONA "unites" chargé
!>  reset_tcheb        : <integer>                             =.true. : réinitialisation du pointeur en début de fichier Tchebychev demandée
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_varglob.F90 69 2012-09-11 08:33:34Z ffsm $'


! Constantes
      ! nombre max de fichiers initialisables (methode NAIF)
      integer, parameter :: ephv_nbficmax = 10

      ! taille max d'un nom de fichier
      integer, parameter :: ephv_lgmax = 200

      ! nombre maximum de codes méthodes acceptés
      integer, parameter :: ephv_maxcodes=50

      ! nombre maximum de champs tolérés dans un fichier de la base
      integer, parameter :: ephv_nb_max_struct_fichier = 500
                            
! Variables globales pour passage d'information entre
!   le sous-programme d'init (eph_init) et
!   le sous-programme de calcul (eph_poscor)

      ! logical file number (lfn) des fichiers utilisés
      integer, dimension(ephv_nbficmax) :: ephv_lfnnaif, ephv_lfncomet
      integer :: ephv_lfnbdl, ephv_lfntche

      ! code methode
      integer :: codenaif
      integer :: codetche, codetchemad

      ! Code de la méthode courante
      ! ce code sera contrôlé avec le code donné lors
      ! d'un appel à eph_poscor
      integer :: code_courant
 
      character(len=ephv_lgmax) :: fictche=""
      character(len=ephv_lgmax),dimension(ephv_nbficmax) :: ficnaif=""
      character(len=ephv_lgmax) :: fictuc=""

! variables remanantes (initialisation globale faite ou non)
      logical :: isinit_bdl=.false.
      logical :: initnaif=.false.
      logical :: ficerrmessage=.false.
      logical :: initficsauttuc=.false.

!      logical :: ephv_madona_unit=.false. ! fichier MADONA unite charge 

      integer, save :: reset_tcheb 

end module eph_varglob
