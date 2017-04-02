subroutine mui_interp_init ( fsub, integrateur, retour_fsub, pb_fsub, retour )

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Initialiser les vecteurs necessaires a l'interpolation pour integrateurs a pas variable
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
use parametre_mspro
use parametre_interne_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none
real(pm_reel),dimension(4,12)::pm_i_dop853_d

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
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: s ! valeurs intermediaires
real(pm_reel) :: h ! pas
real(pm_reel),dimension(pm_i_DOP853_l_ydotK_reste,integrateur%n) :: ydotK_reste ! derivees restant a calculer
real(pm_reel),dimension(integrateur%n)     :: y_tmp       ! vecteur d'etat intermediaire
real(pm_reel), dimension(integrateur%n)    :: yDotK            ! vecteur d'etat temporaire

integer :: i,j ! compteurs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mui_interp_init.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
 pm_i_DOP853_d = reshape(pm_i_DOP853_tmp_d,(/4,12/),order=(/2,1/))
retour = pm_OK

if (.not.integrateur%v_initialise) then 

   h = integrateur%t_fin - integrateur%t_deb

   if (integrateur%type == pm_DOP853) then   ! cas de l'integrateur DOP853
      ! k14
      do j=1,integrateur%n
         s = pm_i_DOP853_k14(1) * integrateur%yDotK(1,j)
         do i=6,13
            s = s + pm_i_DOP853_k14(i) * integrateur%yDotK(i,j)
         end do
         y_tmp(j) = integrateur%y_fin(j)+ h*s
      end do
      do i=1, integrateur%n
         yDotK(i) = yDotK_reste(1,i)
      enddo
      call fsub (integrateur%t_deb+(h*pm_i_DOP853_c14), y_tmp(:), yDotK(:), &
           retour_fsub)  ! appel a l'equation differentielle
      if (retour_fsub /= pm_OK) then
         pb_fsub = integrateur%t_deb+(h*pm_i_DOP853_c14)
         if (retour_fsub > 0) then ! Avertissement
            retour = pm_warn_sub
         else                      ! Erreur
            retour = pm_err_sub
            go to 6000
         end if
      end if
      do i=1, integrateur%n
         yDotK_reste(1,i) = yDotK(i)
      enddo
      
      ! k15
      do j=1,integrateur%n
         s = pm_i_DOP853_k15(1) * integrateur%yDotK(1,j)
         do i=6,13
            s = s + pm_i_DOP853_k15(i) * integrateur%yDotK(i,j)
         end do
         s = s + pm_i_DOP853_k15(14) * ydotK_reste(1,j)
         y_tmp(j) = integrateur%y_fin(j)+ h*s
      end do
      do i=1, integrateur%n
         yDotK(i) = yDotK_reste(2,i)
      enddo
      call fsub (integrateur%t_deb+(h*pm_i_DOP853_c15), y_tmp(:), yDotK(:), &
           retour_fsub)  ! appel a l'equation differentielle
      if (retour_fsub /= pm_OK) then
         pb_fsub = integrateur%t_deb+(h*pm_i_DOP853_c15)
         if (retour_fsub > 0) then ! Avertissement
            retour = pm_warn_sub
         else                      ! Erreur
            retour = pm_err_sub
            go to 6000
         end if
      end if
      do i=1, integrateur%n
         yDotK_reste(2,i) = yDotK(i)
      enddo
         
      ! k16
      do j=1,integrateur%n
         s = pm_i_DOP853_k16(1) * integrateur%yDotK(1,j)
         do i=6,13
            s = s + pm_i_DOP853_k16(i) * integrateur%yDotK(i,j)
         end do
         s = s + pm_i_DOP853_k16(14) * ydotK_reste(1,j)
         s = s + pm_i_DOP853_k16(15) * ydotK_reste(2,j)
         y_tmp(j) = integrateur%y_fin(j)+ h*s
      end do
      do i=1, integrateur%n
         yDotK(i) = yDotK_reste(3,i)
      enddo
      call fsub (integrateur%t_deb+(h*pm_i_DOP853_c16), y_tmp(:), yDotK(:), &
           retour_fsub)  ! appel a l'equation differentielle
      if (retour_fsub /= pm_OK) then
         pb_fsub = integrateur%t_deb+(h*pm_i_DOP853_c16)
         if (retour_fsub > 0) then ! Avertissement
            retour = pm_warn_sub
         else                      ! Erreur
            retour = pm_err_sub
            go to 6000
         end if
      end if
      do i=1, integrateur%n
         yDotK_reste(3,i) = yDotK(i)
      enddo

      ! calcul du vecteur d'interpolation
      integrateur%v(1,:) = pm_i_DOP853_b(1) * integrateur%yDotK(1,:)
      do i=6,12
         integrateur%v(1,:) = integrateur%v(1,:) + pm_i_DOP853_b(i) * integrateur%yDotK(i,:)
      end do
      integrateur%v(1,:) = h * integrateur%v(1,:)
      integrateur%v(2,:) = h * integrateur%yDotK(1,:) - integrateur%v(1,:)
      integrateur%v(3,:) = integrateur%v(1,:) - integrateur%v(2,:) - h * integrateur%yDotK(13,:)
      do j=1,size(pm_i_DOP853_d,1)
         integrateur%v(3+j,:) = pm_i_DOP853_d(j,1) * integrateur%yDotK(1,:)
         do i=6,13
            integrateur%v(3+j,:) = integrateur%v(3+j,:) + pm_i_DOP853_d(j,i-4) * integrateur%yDotK(i,:)
         end do
         do i=1,3
            integrateur%v(3+j,:) = integrateur%v(3+j,:) + pm_i_DOP853_d(j,i+9) * yDotK_reste(i,:)
         end do
         integrateur%v(3+j,:) = h * integrateur%v(3+j,:)
      end do

   end if
   integrateur%v_initialise = pm_i_oui

end if

6000 continue

end subroutine mui_interp_init
