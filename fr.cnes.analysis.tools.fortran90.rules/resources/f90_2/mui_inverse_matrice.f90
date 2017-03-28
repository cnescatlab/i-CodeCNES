subroutine mui_inverse_matrice (mat_A, mat_B, retour)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Calcul de l'inverse d'une matrice 3*3 quelconque
! ===   
!
! Note d'utilisation: Sans objet 
! ==================
!
!$Historique
! ==========
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   creation a partir de la routine MUMINV de la MSLIB f77
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

   use precision_mslib
   use type_mslib
   use valeur_code_retour_mslib

! Declarations
! ============
   implicit none   

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3,3) , intent(in)  ::  mat_A   ! matrice 3*3 initiale
real(pm_reel), dimension(3,3) , intent(out) ::  mat_B   ! matrice 3*3 inversee
integer , intent(out)           :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

   real(pm_reel)   ::  det     ! determinant   
   integer         ::  i, j

   character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
        '@(#) fichier mslib mui_inverse_matrice.f90: derniere modification V6.13 >'

! ne pas toucher a la ligne suivante
   character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mui_inverse_matrice.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

   retour = pm_ok

! calcul du determinant avec les cofacteurs de la 1ere ligne

   mat_b(1,1)=mat_a(2,2)*mat_a(3,3)-mat_a(3,2)*mat_a(2,3)
   mat_b(2,1)=-mat_a(2,1)*mat_a(3,3)+mat_a(3,1)*mat_a(2,3)
   mat_b(3,1)=mat_a(2,1)*mat_a(3,2)-mat_a(3,1)*mat_a(2,2)

   det=mat_a(1,1)*mat_b(1,1)+mat_a(1,2)*mat_b(2,1)+mat_a(1,3)*mat_b(3,1)

   if (abs(det).lt.1.d-50) then
      retour = pm_err_det_nul
      go to 6000
   endif

! calcul des autres cofacteurs transposes

   mat_b(1,2)=-mat_a(1,2)*mat_a(3,3)+mat_a(3,2)*mat_a(1,3)
   mat_b(2,2)=mat_a(1,1)*mat_a(3,3)-mat_a(3,1)*mat_a(1,3)
   mat_b(3,2)=-mat_a(1,1)*mat_a(3,2)+mat_a(3,1)*mat_a(1,2)
   mat_b(1,3)=mat_a(1,2)*mat_a(2,3)-mat_a(2,2)*mat_a(1,3)
   mat_b(2,3)=-mat_a(1,1)*mat_a(2,3)+mat_a(2,1)*mat_a(1,3)
   mat_b(3,3)=mat_a(1,1)*mat_a(2,2)-mat_a(2,1)*mat_a(1,2)

! division de la matrice b par le determinant d

   do i=1,3
      do j=1,3
         mat_b(i,j)=mat_b(i,j)/det
      enddo
   enddo

6000 continue

end subroutine mui_inverse_matrice
