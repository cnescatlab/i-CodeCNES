subroutine mui_integ_butcher ( integrateur, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Recuperer les parametres du tableau de Butcher pour un integrateur donne.
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro
use parametre_interne_mspro
use parametre_mspro

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur), intent(inout)    ::  integrateur  ! integrateur a completer
integer, intent(out)                   ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: i,j,k ! compteurs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mui_integ_butcher.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! selon le type d'integrateur

select case (integrateur%type)

case (pm_Gill)

   integrateur%a(:,:) = 0._pm_reel
   k = 0
   do i=1,integrateur%nb_etapes-1
      do j=1,i
         k = k + 1
         integrateur%a(i,j) = pm_i_Gill_a(k)
      end do
   end do
   integrateur%b(:) = pm_i_Gill_b(:)
   integrateur%c(:) = pm_i_Gill_c(:)
   integrateur%report_fin_deb = pm_i_Gill_report_fin_deb

case (pm_DOP853) 

   integrateur%a(:,:) = 0._pm_reel
   k = 0
   do i=1,integrateur%nb_etapes-1
      do j=1,i
         k = k + 1
         integrateur%a(i,j) = pm_i_DOP853_a(k)
      end do
   end do
   integrateur%b(:) = pm_i_DOP853_b(:)
   integrateur%c(:) = pm_i_DOP853_c(:)
   integrateur%report_fin_deb = pm_i_DOP853_report_fin_deb

end select ! case default inutile

end subroutine mui_integ_butcher
