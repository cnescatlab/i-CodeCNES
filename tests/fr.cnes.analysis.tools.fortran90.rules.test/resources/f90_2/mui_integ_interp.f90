subroutine mui_integ_interp ( integrateur, t, y, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Interpolation de y(t) utilisant les calculs de l'integrateur
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

use int_util_internes_mspro, only : mui_interp_Gill
use int_util_internes_mspro, only : mui_interp_DOP853

use type_mspro
use parametre_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur),                  intent(in)     ::  integrateur   ! integrateur utilise
real(pm_reel),                         intent(in)     ::  t             ! abscisse
real(pm_reel),dimension(integrateur%n),intent(out)    ::  y             ! vecteur d'etat interpole
integer,                               intent(out)    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_interp.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK
retour_local = pm_OK

! Orientation selon l'integrateur choisi

select case (integrateur%type)

case (pm_Gill)
   
   call mui_interp_Gill (integrateur, t, y, retour_local)

case (pm_DOP853) 

   call mui_interp_DOP853 (integrateur, t, y, retour_local)

end select

if (retour_local /= pm_OK) then
   retour = retour_local
end if
   

end subroutine mui_integ_interp
