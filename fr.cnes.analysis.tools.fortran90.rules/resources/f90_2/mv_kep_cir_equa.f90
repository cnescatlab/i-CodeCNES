subroutine mv_kep_cir_equa (kep, cir_equa, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Passage des parametres kepleriens aux parametres orbitaux dits 
! ===   adaptes aux orbites circulaires equatoriales
! 
!
! Note d'utilisation:  La transformation inverse peut se faire par la routine
! ==================   mv_cir_equa_kep
!
!$Historique
! ==========
!   
!  + Version 6.0 (SP 621 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (date: 02/2004 - realisation: veronique lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib (supprimé) par 
!     une sélection de int_constantes, suppression phys_mslib (inutilisé)
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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

use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_kep), intent(in)        :: kep      ! parametres kepleriens
type(tm_orb_cir_equa), intent(out)  :: cir_equa ! parametres adaptes aux orbites circulaires equatoriales
type(tm_code_retour), intent(out)   ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob  ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: pom_plus_gom  ! argument du perigee + longitude du noeud ascendant
real(pm_reel) :: sinus_i_sur_2 ! sin(i/2)
real(pm_reel) :: ex,ey,ix,iy   ! calculs a reporter pour la jacobienne

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mv_kep_cir_equa.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kep_cir_equa.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

cir_equa%a = kep%a

pom_plus_gom = kep%pom+kep%gom
ex = kep%e*cos(pom_plus_gom)
cir_equa%ex = ex
ey = kep%e*sin(pom_plus_gom)
cir_equa%ey = ey

sinus_i_sur_2 = sin(kep%i/2._pm_reel)
ix = 2._pm_reel*sinus_i_sur_2*cos(kep%gom)
cir_equa%ix = ix
iy = 2._pm_reel*sinus_i_sur_2*sin(kep%gom)
cir_equa%iy = iy

cir_equa%pso_M = pom_plus_gom + kep%M

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cos(pom_plus_gom)
   jacob(2,4) = - ey
   jacob(2,5) = - ey
   jacob(3,2) = sin(pom_plus_gom)
   jacob(3,4) = ex
   jacob(3,5) = ex
   jacob(4,3) = cos(kep%i/2._pm_reel)*cos(kep%gom)
   jacob(4,5) = - iy
   jacob(5,3) = cos(kep%i/2._pm_reel)*sin(kep%gom)
   jacob(5,5) = ix
   jacob(6,4) = 1._pm_reel
   jacob(6,5) = 1._pm_reel
   jacob(6,6) = 1._pm_reel
end if

code_retour%routine = pm_num_mv_kep_cir_equa
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kep_cir_equa
