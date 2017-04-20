subroutine mui_interp_Gill ( integrateur, t, y, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Interpolateur lie a l'integrateur de Gill. 
! ===
!
! Note d'utilisation:  
! ================== 
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
!             routine definie a partir de la classe GillInterpolator
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

real(pm_reel) :: h        ! taille du pas
real(pm_reel) :: theta    ! abscisse d'interpolation normalisee
real(pm_reel) :: s, soMt, c23, coeff1,coeff2,coeff3,coeff4         ! valeurs intermediaires

intrinsic sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_interp_Gill.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

h = integrateur%t_fin - integrateur%t_deb
theta = (t - integrateur%t_deb)/h
s = (integrateur%t_fin - t)/6._pm_reel
soMt = s * (1._pm_reel - theta)
c23 = soMt * (1._pm_reel + 2._pm_reel*theta)
coeff1 = soMt * (1._pm_reel - 4._pm_reel*theta)
coeff2 = c23 * (2._pm_reel - sqrt(2._pm_reel))
coeff3 = c23 * (2._pm_reel + sqrt(2._pm_reel))
coeff4 = s * (1._pm_reel + theta*(1._pm_reel + 4._pm_reel*theta))

y(:) = integrateur%y_fin(:) - coeff1 * integrateur%yDotK(1,:) - coeff2 * integrateur%yDotK(2,:) &
      - coeff3 * integrateur%yDotK(3,:) - coeff4 * integrateur%yDotK(4,:)

end subroutine mui_interp_Gill
