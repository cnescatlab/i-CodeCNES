subroutine mui_interp_DOP853 ( integrateur, t, y, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Interpolateur lie a l'integrateur DOrmandPrince853. 
! ===
!
! Note d'utilisation:  
! ==================  
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
!             routine definie a partir de la classe DormandPrince853StepInterpolator
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
!   + Version 5.4 : FA-ID 475 : Division par zero
!                         (Date: 02/02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   FA-ID 624 : variables non initialisees
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur),                  intent(in)    ::  integrateur   ! integrateur utilise
real(pm_reel),                         intent(in)    ::  t             ! abscisse
real(pm_reel),dimension(integrateur%n),intent(out)   ::  y             ! vecteur d'etat interpole
integer,                               intent(out)   ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: theta    ! abscisse d'interpolation normalisee
real(pm_reel) :: h        ! taille du pas
real(pm_reel) :: eta      ! valeur intermediaire
real(pm_reel) :: eps100   ! epsilon de comparaison pour les reels

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_interp_DOP853.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK
eps100 = 100._pm_reel * epsilon(1._pm_reel)  !epsilon de test pour les reels
theta = 0.0_pm_reel
eta = 0.0_pm_reel

h = integrateur%t_fin - integrateur%t_deb
if (abs(h)< eps100) then
   retour = pm_err_integ_dates
   go to 6000
else
   theta = (t - integrateur%t_deb)/h
   eta = (integrateur%t_fin-t)/h
endif

y(:) = integrateur%y_fin(:) - eta * (integrateur%v(1,:) - theta * (integrateur%v(2,:) &
      + theta * (integrateur%v(3,:) + eta * (integrateur%v(4,:) + theta * (integrateur%v(5,:) &
      + eta * (integrateur%v(6,:) + theta * integrateur%v(7,:)))))))

6000 continue

end subroutine mui_interp_DOP853
