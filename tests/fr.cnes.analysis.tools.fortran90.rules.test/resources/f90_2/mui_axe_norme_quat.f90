function mui_axe_norme_quat (axe_norme, angle )

! (C) Copyright CNES - MSLIB - 2003

!************************************************************************
!
! But:  Conversion d'une rotation definie par son AXE norme et son ANGLE en QUATernion.
! ===
!
! Note d'utilisation:  ATTENTION: 
! ==================
!                            * fonction dediee aux axes de norme 1, tels les axes canoniques X, Y ou Z
!                            * pas de test effectue sur la norme de l'axe
!                            * equivalent a la routine mu_axe_angle_quat sans les tests pour 
!                              des raisons de gain en temps calcul
!$Historique
! ==========
! 
!   + Version 5.0 (SP 608 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3) , intent(in) ::  axe_norme            ! axe de rotation (norme)
real(pm_reel)               , intent(in) ::  angle                ! angle de rotation
type(tm_quat)                            ::  mui_axe_norme_quat   ! quaternion resultat

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
type(tm_quat)                          ::  quat        ! quaternion norme              
real(pm_reel)                          :: anglemod
intrinsic modulo, cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mui_axe_norme_quat.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mui_axe_norme_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

anglemod = modulo (angle, pm_deux_pi)                               !conversion de l'angle sur [0,2pi[
quat%q0      = cos (anglemod / 2._pm_reel)                          !calcul du quaternion equivalent
quat%q123(:) = sin (anglemod / 2._pm_reel) * axe_norme

mui_axe_norme_quat%q0 = quat%q0
mui_axe_norme_quat%q123(:)= quat%q123(:)

end function mui_axe_norme_quat
