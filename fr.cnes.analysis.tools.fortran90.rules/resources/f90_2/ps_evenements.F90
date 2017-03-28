module ps_evenements

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_evenements
!
!$Resume
!  Module décrivant les données nécessaires à l'écriture des événements.
!
!$Description
!  Module décrivant les données nécessaires à l'écriture des événements.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_evenements.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_evenements.F90,v $
!  Revision 1.8  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.7  2008/12/02 16:46:54  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.6  2006/10/17 09:54:22  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.5  2006/03/15 13:25:18  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.4  2005/11/10 18:37:05  vivaresf
!  Mise à jour des cartouches
!  Revision 1.3  2005/01/28 10:38:24  fabrec
!  maj cartouches
!  Revision 1.2  2002/11/26 15:53:47  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.4  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Conversion des variables en type structure indice
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:12  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_evenements
!
!$Structure
!
!: PS_STR_INT_EVENEMENTS : 
!>     ndeve   : <integer>              nombre d'évenements
!>     ifore   : <integer>              format d'écriture des variables à écrire dans le fichier EVENT
!>     neve    : <integer>              numéro de l'événement
!>     deve    : <pm_reel,DIM=(nbeve)>  tableau des dates des événements
!>     aeve    : <LEN=80,DIM=(nbeve)>   tableau des commentaires des événements
!
!$Global
!
!>  str_eve   : <PS_STR_INT_EVENEMENTS,DIM=(PS_NVMAX)>  type structure evenement
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
  character(len=256), private :: SVN_VER =  '$Id: ps_evenements.F90 69 2012-09-11 08:33:34Z ffsm $'


   type PS_STR_INT_EVENEMENTS
      integer :: ndeve
      integer :: ifore
      integer :: neve
      real (KIND=pm_reel), dimension(nbeve) :: deve
      character(LEN=80),dimension(nbeve)  :: aeve
   end type PS_STR_INT_EVENEMENTS

   type (PS_STR_INT_EVENEMENTS) :: str_eve(PS_NVMAX)

end module ps_evenements
