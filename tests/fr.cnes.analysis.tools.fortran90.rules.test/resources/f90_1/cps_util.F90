module cps_util

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_util
!
!$Resume
!  Fonctions utilitaires pour la gestion des lfn
!
!$Description
!  Fonctions utilitaires pour la gestion des lfn
!
!$Auteur
!  H.M. Pau
!
!$Version
!  $Id: cps_util.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_util.F90,v $
!  Revision 1.3  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.2  2005/12/08 18:39:31  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:14  vivaresf
!  Refonte de COMPAS
!  Revision 1.2  2005/03/08 14:02:25  vivaresf
!  Marqueur Log et Id
!
!$FinHistorique
!
!$Usage
!  use cps_util
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cps_file_unit
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
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
!.  cps_file_unit
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
    implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_util.F90 69 2012-09-11 08:33:34Z ffsm $'


   contains
   
   subroutine cps_file_unit(unit_number,ierr)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_file_unit
!
!$Resume
!  Fournit une unité logique de fichier (lfn) libre entre 50 et 99
!
!$Description
!  Fournit une unité logique de fichier (lfn) libre entre 50 et 99
!  Le test pour savoir d'unité est libre est fait avec "inquire" 
!  qui teste si un fichier a été ouvert avec cette unité.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_file_unit(unit_number,ierr)
!.    integer :: unit_number
!.    integer :: ierr
!
!$Arguments
!>E/S   unit_number  :<integer>   Unité libre
!>S     ierr         :<integer>   code retour (0=OK)
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- msp_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      use msp_gestion_erreur
      
      implicit none
      
      ! Arguments
      integer,intent(inout) :: unit_number
      integer,intent(out)   :: ierr
      
      ! Variables locales
      logical :: ouvert, cherche_encore
      integer :: lu
      integer, parameter :: maxlu = 99
      
      
      ierr = 0
      lu = 50
      cherche_encore = .true.
      
      do while ((lu <= maxlu) .and. cherche_encore)
         inquire (unit=lu,opened=ouvert)
	 if (ouvert) then
	    lu = lu + 1
	 else
	    cherche_encore = .false.
	 endif
      enddo
      
      if (lu > maxlu) then
         ierr = 3
	 call MSP_signaler_message(cle_mes="EPH_ERR_LFNSUP", &
!                   routine="eph_util_ficunit90", &
                   routine="cps_file_unit", &
                   partie_variable="99")     
	 return
      endif
      
      unit_number = lu
      
      return
      
   end subroutine cps_file_unit
   
end module cps_util
