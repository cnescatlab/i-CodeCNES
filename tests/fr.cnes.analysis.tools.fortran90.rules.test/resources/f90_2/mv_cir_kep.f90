subroutine mv_cir_kep (cir, kep, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But: Passage des parametres orbitaux dits adaptes aux orbites 
! ===  circulaires non equatoriales aux parametres kepleriens 
! 
!
! Note d'utilisation:  La transformation inverse peut se faire par la routine
! ==================   mv_kep_cir.
!
!$Historique
! ==========
!   + Version 6.0 (SP 623 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
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
use int_utilitaires, only : mu_angle2

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

type(tm_orb_cir), intent(in)                         ::  cir ! parametres adaptes aux orbites circulaires
type(tm_orb_kep), intent(out)                        ::  kep ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel)        :: pom               ! petit omega
real(pm_reel)        :: excentricite      ! e
real(pm_reel)        :: un_sur_e          ! inverse de e
real(pm_reel)        :: eps_cir           ! pour test orbite circulaire
type(tm_code_retour) :: code_retour_local ! code retour local

intrinsic sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_cir_kep.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_cir_kep.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations

! valeur de test
eps_cir = pm_eps_cir

! Calculs
! =======
kep%a = cir%a
kep%i = cir%i
kep%gom = cir%gom

excentricite = sqrt(cir%ex*cir%ex + cir%ey*cir%ey)
kep%e = excentricite

if (excentricite < eps_cir) then ! orbite circulaire: petit omega indetermine
   pom = 0._pm_reel   ! choix arbitraire: mise a 0
   code_retour%valeur = pm_warn_e_circul
else
   call mu_angle2(cir%ex, cir%ey, pom, code_retour_local) ! pas de code retour a tester
end if

kep%pom = pom
kep%M = cir%pso_M - pom

if (present(jacob)) then
   if (excentricite < eps_cir) then
      code_retour%valeur = pm_err_jac_non_calc_e_circul
      go to 6000
   end if
   un_sur_e = 1._pm_reel / excentricite
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = cir%ex * un_sur_e
   jacob(2,3) = cir%ey * un_sur_e
   jacob(3,4) = 1._pm_reel
   jacob(4,2) = - cir%ey * un_sur_e * un_sur_e
   jacob(4,3) = cir%ex * un_sur_e * un_sur_e
   jacob(5,5) = 1._pm_reel
   jacob(6,2) = cir%ey * un_sur_e * un_sur_e
   jacob(6,3) = - cir%ex * un_sur_e * un_sur_e
   jacob(6,6) = 1._pm_reel
end if

6000 continue

code_retour%routine = pm_num_mv_cir_kep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_cir_kep
