subroutine mri_R1_iers2003(phi,R1,code_retour)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Calcul de la matrice R1 (IERS 2003)
! ===
!
!$Historique
! ==========
!   + Version 6.9 : DM-ID 1092 Création
!                   (Date: 07/2008 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======

use type_mslib
use parametre_mslib
use valeur_code_retour_mslib
use numero_routine_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                      intent(in)            :: phi ! Angle de rotation
real(pm_reel), dimension(3,3),      intent(out)           :: R1  ! Matrice de rotation
type(tm_code_retour),               intent(out)           :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!           [1      0         0   ]
! R1(phi) = [0  cos(phi)  sin(phi)]
!           [0 -sin(phi)  cos(phi)]

R1(1,1) = 1._pm_reel
R1(1,2) = 0._pm_reel
R1(1,3) = 0._pm_reel
R1(2,1) = 0._pm_reel
R1(2,2) = cos(phi)
R1(2,3) = sin(phi)
R1(3,1) = 0._pm_reel
R1(3,2) = -sin(phi)
R1(3,3) = cos(phi)

code_retour%valeur = pm_OK
code_retour%routine = pm_num_mri_R1_iers2003
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mri_R1_iers2003
