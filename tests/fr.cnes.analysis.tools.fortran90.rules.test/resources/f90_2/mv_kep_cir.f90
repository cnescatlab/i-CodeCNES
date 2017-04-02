subroutine mv_kep_cir (kep, cir, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Passage des parametres kepleriens aux parametres orbitaux dits 
! ===   adaptes aux orbites circulaires non equatoriales
!
! Note d'utilisation:  La transformation inverse peut se faire par la routine
! ==================   mv_cir_kep
!
!$Historique
! ==========
!   + Version 6.0 (SP 624 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_orb_kep), intent(in)                         ::  kep ! parametres kepleriens
type(tm_orb_cir), intent(out)                        ::  cir ! parametres adaptes aux orbites circulaires
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: ex, ey   ! calculs a reporter
intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_kep_cir.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kep_cir.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations

cir%a     = kep%a
ex = kep%e*cos(kep%pom)
cir%ex    = ex
ey = kep%e*sin(kep%pom)
cir%ey    = ey
cir%i     = kep%i
cir%gom   = kep%gom
cir%pso_M = kep%pom + kep%M

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cos(kep%pom)
   jacob(2,4) = - ey
   jacob(3,2) = sin(kep%pom)
   jacob(3,4) = ex
   jacob(4,3) = 1._pm_reel
   jacob(5,5) = 1._pm_reel
   jacob(6,4) = 1._pm_reel
   jacob(6,6) = 1._pm_reel
end if

code_retour%routine = pm_num_mv_kep_cir
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kep_cir
