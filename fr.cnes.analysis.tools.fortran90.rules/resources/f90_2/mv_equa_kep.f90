subroutine mv_equa_kep (equa, kep, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites 
! ===   equatoriales non circulaires aux parametres kepleriens
!
! Note d'utilisation: La transformation inverse peut se faire par la routine
! ==================  mv_kep_equa
!
!$Historique
! ==========
!   + Version 6.0 (SP 626 ed01 rev00): creation par transfert de la routine de meme nom de la MSPRO
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

type(tm_orb_equa), intent(in)                        ::  equa  ! parametres adaptes aux orbites equatoriales
type(tm_orb_kep), intent(out)                        ::  kep   ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)        :: gom ! longitude du noeud ascendant
real(pm_reel)        :: norme_carree   ! norme au carree du vecteur inclinaison
real(pm_reel)        :: un_sur_nc      ! inverse de norme carree
real(pm_reel)        :: sinus_i_sur_2  ! sin(i/2)
real(pm_reel)        :: inclinaison    ! i
real(pm_reel)        :: eps_equa       ! pour test orbite equatoriale
type(tm_code_retour) :: code_retour_local

intrinsic asin, sin, sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_equa_kep.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_equa_kep.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
! valeur de test
eps_equa = pm_eps_equa

! Calculs
! =======
kep%a = equa%a
kep%e = equa%e
kep%M = equa%M

norme_carree = equa%ix*equa%ix + equa%iy*equa%iy
if (norme_carree > 4._pm_reel) then  !  norme du vecteur inclinaison trop grande
   code_retour%valeur = pm_err_ix_iy_sup2
   go to 6000
end if

sinus_i_sur_2 = sqrt(norme_carree)/2._pm_reel
inclinaison = 2._pm_reel * asin(sinus_i_sur_2) ! resultat dans [0,+pi] car sin(i/2) > 0
kep%i = inclinaison

if (sin(inclinaison) < eps_equa) then ! orbite equatoriale: grand omega indetermine
   gom = 0._pm_reel   ! choix arbitraire: mise a 0
   code_retour%valeur = pm_warn_i_equa  
else
   call mu_angle2(equa%ix, equa%iy, gom, code_retour_local) ! pas de code retour a tester
end if

kep%gom = gom
kep%pom = equa%pgom - gom

if (present(jacob)) then
   if (sin(inclinaison) < eps_equa) then
      code_retour%valeur = pm_err_jac_non_calc_i_equa
      go to 6000
   end if
   un_sur_nc = 1._pm_reel / norme_carree
   jacob(:,:) = 0._pm_reel
   jacob(1,1) = 1._pm_reel
   jacob(2,2) = 1._pm_reel
   jacob(3,4) = equa%ix / sin(inclinaison)
   jacob(3,5) = equa%iy / sin(inclinaison)
   jacob(4,3) = 1._pm_reel
   jacob(4,4) = equa%iy * un_sur_nc
   jacob(4,5) = - equa%ix * un_sur_nc
   jacob(5,4) = - equa%iy * un_sur_nc
   jacob(5,5) = equa%ix * un_sur_nc
   jacob(6,6) = 1._pm_reel
end if

6000 continue

code_retour%routine = pm_num_mv_equa_kep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_equa_kep
