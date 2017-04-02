subroutine mui_integ_erreur ( integrateur,y_deb,y_fin,h,erreur,retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Estimation de l'erreur d'integration sur un pas donne
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
use parametre_mspro
use parametre_interne_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur),                   intent(in)   ::  integrateur  ! integrateur utilise
real(pm_reel),dimension(integrateur%n), intent(in)   ::  y_deb        ! etat en debut de pas
real(pm_reel),dimension(integrateur%n), intent(in)   ::  y_fin        ! etat en fin de pas
real(pm_reel),                          intent(in)   ::  h            ! pas
real(pm_reel),                          intent(out)  ::  erreur       ! erreur estimee
integer,                                intent(out)  ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

! pour DOP853
real(pm_reel) :: erreur1,erreur2,somme1,somme2,ratio1,ratio2,den   ! valeurs intermediaires
real(pm_reel) :: y_echelle   ! echelle max de y
real(pm_reel) :: tolerance   ! tolerance sur un element

integer :: i,j   ! compteurs

intrinsic abs,max,sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_erreur.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

select case (integrateur%type)

! le seul cas d'integrateur a pas variable implemente a ce jour est le DOP853
case (pm_DOP853)
   erreur1 = 0._pm_reel
   erreur2 = 0._pm_reel

   do j=1,integrateur%n
      somme1 = pm_i_DOP853_e1(1) * integrateur%yDotK(1,j)
      somme2 = pm_i_DOP853_e2(1) * integrateur%yDotK(1,j)
      do i=6,integrateur%nb_etapes-1
         somme1 = somme1 + pm_i_DOP853_e1(i) * integrateur%yDotK(i,j)
         somme2 = somme2 + pm_i_DOP853_e2(i) * integrateur%yDotK(i,j)
      end do
      y_echelle = max (abs(y_deb(j)),abs(y_fin(j)))
      tolerance = abs(integrateur%tol_abs(j))+(abs(integrateur%tol_rel(j))*y_echelle)
      ratio1 = somme1/tolerance
      erreur1 = erreur1 + ratio1*ratio1
      ratio2 = somme2/tolerance
      erreur2 = erreur2 + ratio2*ratio2
   end do
   
   den = erreur1 + 0.01_pm_reel * erreur2
   if (den <= 0._pm_reel) den = 1._pm_reel

   erreur = abs(h)*erreur1/sqrt(real(integrateur%n,pm_reel)*den)

end select ! case default inutile

end subroutine mui_integ_erreur
