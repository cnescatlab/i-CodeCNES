subroutine mvi_kepler_hyperb (anom_M, e, anom_E, retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Resolution de l'equation de KEPLER dans le cas HYPERBolique (e>1)
! ===
!
! Note d'utilisation:  routine interne
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 224 ed01 rev00): creation a partir du code correspondant au cas hyperbolique
!                                      dans la routine MVRYKN de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
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
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(in)                 :: e        ! excentricite (e)
real(pm_reel), intent(out)                :: anom_E   ! anomalie excentrique (E) 
integer, intent(out)                      :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: eps100                                     ! variable 100 * epsilon machine
real(pm_reel) :: e0,ei,eip1                                 ! valeurs initiale ,courante et suivante pour resolution iterative
real(pm_reel) :: c                                          ! variable intermediaire
real(pm_reel) :: err1, err2                                 ! criteres de convergence

integer       :: iter                                       ! nombre d'iterations

real(pm_reel), parameter :: anom_Mmin= 1._pm_reel           ! valeur de test pour l'initialisation de l'anomalie moyenne
real(pm_reel), parameter :: emin=1.05_pm_reel               ! valeur de test pour l'initialisation de l'excentricite
integer, parameter       :: nb_max_iter=10                  !nombre maximum d'iterations

intrinsic abs, log, sqrt, sinh, max, tanh

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_kepler_hyperb.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_kepler_hyperb.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = 0

eps100  =  100._pm_reel  * epsilon(1._pm_reel)! constante eps pour les reels

if ((abs(anom_M)  <= anom_Mmin) .and. (e  <= emin)) then
   e0 = (6._pm_reel*abs(anom_M))**(1._pm_reel/3._pm_reel)
   if (anom_M < 0._pm_reel) e0 = -e0
else
   e0 = log(anom_M/e+sqrt(anom_M*anom_M/e/e+1._pm_reel))
end if

! resolution iterative par methode de newton
!    f(x) = 0
!    xi+1 = xi - (f(xi) / f'(xi))
! -------------------------------------------

ei = e0
!  initialisation des criteres de convergence et du nombre d'iterations
err1 = 1._pm_reel
err2 = 1._pm_reel
iter = 0
do while ((err1  > 10._pm_reel *eps100) .and. (err2  > 10._pm_reel *eps100) .and. (iter /= nb_max_iter))

   iter = iter + 1
   c=tanh(ei/2._pm_reel)
   eip1=ei-((anom_M+ei)*(1._pm_reel-c*c)-e*2._pm_reel*c)/(1._pm_reel-c*c-e*(1._pm_reel+c*c))

   err1 = abs(eip1-ei)/max(abs(eip1),1._pm_reel)    !  calcul des criteres de convergence
   err2 = e*sinh(eip1) - eip1 - anom_M
   err2 = abs(err2)/max(abs(anom_M),1._pm_reel)
   ei = eip1

end do

if ((err1  > 10._pm_reel *eps100) .and. (err2  > 10._pm_reel *eps100)) then    !    pas de convergence au bout de 10 iterations ->  arret
   retour = pm_err_conv_kepler_hyperb
   go to 6000
end if

anom_E = ei

6000 continue

end subroutine mvi_kepler_hyperb
