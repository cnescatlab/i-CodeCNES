subroutine mui_integ_init_pas ( fsub, integrateur, t0, y0, en_avant, h, retour_fsub, pb_fsub, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Initialise le pas variable d'integration
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
!   + Version 5.2 : creation a
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : FA-ID 385 : Anomalie mu_integrer 
!                         (Date: 09/2005 - Realisation: Claire Fabre)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
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
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
logical,                               intent(in)                     ::  en_avant     ! sens d'integration
real(pm_reel),                         intent(out)                    ::  h            ! pas initialise
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: ratio, y_echelle, ydot_echelle, h_tmp, yddot_echelle   ! valeurs temporaires
real(pm_reel) :: maxinv2, h1 ! valeurs temporaires
real(pm_reel),dimension(integrateur%n) :: y1
real(pm_reel), dimension(integrateur%n)    :: yDotK            ! vecteur d'etat temporaire

integer :: i ! compteur

intrinsic sqrt,abs,min,max,epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_integ_init_pas.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! 1ere approximation: h = 0.01 * ||y/tolerance|| / ||y'/tolerance||
y_echelle = 0._pm_reel
ydot_echelle = 0._pm_reel

do i=1,integrateur%n
   ratio = y0(i)/abs(integrateur%tol_abs(i))
   y_echelle = y_echelle + ratio*ratio
   ratio = integrateur%yDotK(1,i)/abs(integrateur%tol_abs(i))
   ydot_echelle = ydot_echelle + ratio*ratio
end do

if ((y_echelle < 1.e-10_pm_reel).or.(ydot_echelle < 1.e-10_pm_reel)) then
   h_tmp = 1.e-6_pm_reel
else
   h_tmp = 0.01_pm_reel * sqrt(y_echelle/ydot_echelle)
end if
if (.not.en_avant) h_tmp = -h_tmp

! on procede a un pas d'integration de type Euler a partir de la 1ere approximation
do i=1,integrateur%n
   y1(i) = y0(i) + h_tmp * integrateur%yDotK(1,i)
end do
do i=1, integrateur%n
   yDotK(i) = integrateur%yDotK(2,i)
enddo
call fsub (t0+h_tmp, y1, yDotK(:), retour_fsub)  ! appel a l'equation differentielle
if (retour_fsub /= pm_OK) then
   pb_fsub = t0+h_tmp
   if (retour_fsub > 0) then ! Avertissement
      retour = pm_warn_sub
   else                      ! Erreur
      retour = pm_err_sub
      go to 6000
   end if
end if
do i=1, integrateur%n
   integrateur%yDotK(2,i) = yDotK(i)
enddo

! estimation de la derivee seconde
yddot_echelle = 0._pm_reel
do i=1,integrateur%n
   ratio = (integrateur%yDotK(2,i)-integrateur%yDotK(1,i))/abs(integrateur%tol_abs(i))
   yddot_echelle = yddot_echelle + ratio*ratio
end do

! on calcule h tel que : h^ordre * max(||y'/tolerance||,||y''/tolerance||) = 0.01
maxinv2 = max(sqrt(ydot_echelle),sqrt(yddot_echelle)/h_tmp)
if (maxinv2<epsilon(1._pm_reel)) then
   h1 = max(1.e-6_pm_reel,0.001_pm_reel*abs(h_tmp))
else
   h1 = (0.01_pm_reel/maxinv2)**(1._pm_reel/integrateur%ordre)
end if
h_tmp = min(100._pm_reel*abs(h_tmp),h1)
h_tmp = max(h_tmp,1.e-12_pm_reel*abs(t0))

if (en_avant) then
   h = h_tmp
else
   h = - h_tmp
end if

6000 continue

end subroutine mui_integ_init_pas
