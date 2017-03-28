!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!	Interface FORTRAN 77 a la LIBEPHEM
!
!$Version
!  $Id: eph_poscor77.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_poscor77.F90,v $
!  Revision 1.5  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.4  2008/08/04 13:31:41  gss
!  DM-ID 1058 : (portage g95) suppression de la variable tau_loc non utilisée.
!
!  Revision 1.3  2006/05/30 08:23:47  vivaresf
!  Decoupage en COMPAS BASE et COMPAS UI
!
!  Revision 1.2  2006/03/09 12:04:41  vivaresf
!  FA-ID 500 : trace de la DM 391
!
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.5  2005/11/07 15:56:24  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!
!  Revision 1.4  2005/03/09 09:12:04  vivaresf
!  Correction des cartouches
!
!  Revision 1.3  2005/03/01 12:56:00  vivaresf
!  Version 2-1
!
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine eph_poscor77(code, date, nc, ni, coord, ier)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_poscor77
!
!$Resume
!  Interface FORTRAN 77 au sous_programme eph_poscor
!
!$Description
!  Sous-programme chapeau de calcul d'éphémérides couvrant
!  les différentes méthodes proposées
!
!$Auteur
!  Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_poscor77(code, date, nc, ni, coord, [ier])
!.    real(KIND=PM_REEL) :: date
!.    integer :: code, nc, ni
!.    real(KIND=PM_REEL), dimension(6) :: coord
!.    integer :: ier
!
!$Arguments
!>E     code   :<integer>           code méthode/théorie
!>E     date   :<PM_REEL>           date en JJ50 
!>E     nc     :<integer>           numéro du corps central 
!>E     ni     :<integer>           numéro du corps d'intérêt
!>S     coord  :<PM_REEL,DIM=(6)>   position/vitesse de ci par rapport à cc
!                                   dans le repère d'expression : 
!.                                   - repère dans lequel sont définies les 
!                                     éphémérides tchebychevisées, 
!.                                   - EME2000 pour les autres méthodes
!>[S]   ier    :<integer>           Code retour (0=OK)
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$Routines
!- eph_poscor
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ephem
      use msp_gestion_erreur

      implicit none

! Arguments
      real(KIND=PM_REEL), intent(in) :: date
      integer, intent(in) :: code, nc, ni
      real(KIND=PM_REEL), dimension(6), intent(out) :: coord
      integer, optional, intent(out) :: ier
      
! Variable locales
      integer :: iret

      iret = 0
      
      call eph_poscor(code, date, nc, ni, coord)

      ! Traitement des erreurs 

      if (MSP_ERREUR) iret=-1

      if (PRESENT(ier)) ier = iret

      end subroutine eph_poscor77

subroutine eph_cnes2old77(n1,n2,sens,ier)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_cnes2old77
!
!$Resume
!  Interface FORTRAN 77 au sous_programme eph_cnes2old
!
!$Description
!  Sous-programme chapeau au sous-programme eph_cnes2old pour
!  appel en F77. Conversion de numéro de corps entre 
!  numérotation AMLIB et numérotation LIBEPHEM.
!
!$Auteur
!  Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_cnes2old77(n1,n2,sens,[ier])
!.    integer :: n1
!.    integer :: n2
!.    integer :: sens
!.    integer :: ier
!
!$Arguments
!>E     n1    :<integer>   numéro CNES si isens=1 - numéro AMLIB si isens=-1
!>E/S   n2    :<integer>   numéro AMLIB si isens=1 - numéro CNES si isens=-1
!>E     sens  :<integer>   sens de conversion
!>[S]   ier   :<integer>   code retour
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$Routines
!- eph_cnes2old
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ephem
      use msp_gestion_erreur

      implicit none

! Arguments
      integer, intent(in)    :: n1
      integer, intent(inout)   :: n2
      integer, intent(in)    :: sens
      integer, intent(out),optional   :: ier
      
! Variable locales
      integer :: iret

      iret = 0
      
      call eph_cnes2old(n1, n2, sens, iret)

      ! Traitement des erreurs 

      if (MSP_ERREUR) iret=-1

      if (PRESENT(ier)) ier = iret

      end subroutine eph_cnes2old77
