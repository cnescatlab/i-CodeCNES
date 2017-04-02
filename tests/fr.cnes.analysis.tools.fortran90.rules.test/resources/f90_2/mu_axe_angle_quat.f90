subroutine mu_axe_angle_quat (axe, angle, quat, code_retour)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But: Conversion d'une rotation definie par son AXE et son ANGLE en QUATernion.
! ===
!
! Note d'utilisation:  La transformation inverse peut etre effectuee par la routine mu_quat_axe_angle
! ==================
!
!$Historique
! ==========
!   + Version 3.0 (SP 421 ed01 rev00): creation a partir de la routine MUCVECQUA de la MSLIB f77
!                         (Date: 09/2000 - Realisation: auteur: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
!                   (Date: 03/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_norme

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

  use precision_mslib
  use type_mslib
  use valeur_code_retour_mslib
  use numero_routine_mslib
  use longueur_chaine_mslib

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  axe         ! axe de rotation
real(pm_reel), intent(in)                            ::  angle       ! angle de rotation
type(tm_quat), intent(out)                           ::  quat        ! quaternion norme              
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================
  real(pm_reel), dimension(3)   :: axe_norme           ! vecteur axe norme
  real(pm_reel)                 :: anglemod, norme_axe ! angle modulo 2*pi, norme de l'axe
  integer                       :: ii                  ! indice de boucle

  intrinsic modulo, cos, sin

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mu_axe_angle_quat.f90: derniere modification V6.13 >'

  ! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_axe_angle_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  !
  call mu_norme(axe, norme_axe, code_retour, vect_norme = axe_norme) !calcul de la norme et normalisation du vecteur axe
  if (code_retour%valeur /= pm_OK) then
     if (code_retour%valeur == pm_err_vect_nul) then
        code_retour%valeur = pm_err_axe_rot_nul      ! affectation specifique du code retour dans le cas d'un axe nul
     else
        code_retour%valeur = pm_err_valid            ! probleme dans la MSLIB90
     end if
     go to 6000
  end if
  !
  anglemod = modulo (angle, pm_deux_pi)                               !conversion de l'angle sur [0,2pi[
  !
  quat%q0      = cos (anglemod / 2._pm_reel)                          !calcul du quaternion equivalent
  do ii = 1,3
     quat%q123(ii) = sin (anglemod / 2._pm_reel) * axe_norme(ii)
  end do
  !

6000 continue

  code_retour%routine = pm_num_mu_axe_angle_quat
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_axe_angle_quat
