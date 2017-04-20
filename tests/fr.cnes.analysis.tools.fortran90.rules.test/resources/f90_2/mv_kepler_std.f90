subroutine mv_kepler_std (anom_M, e, anom_E, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Resolution de l'equation de Kepler, orbite elliptique.
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 208 ed01 rev00): creation a partir de la routine MVRXKN de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use test_mslib
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

real(pm_reel), intent(in)                :: anom_M      ! anomalie moyenne (M)
real(pm_reel), intent(in)                :: e           ! excentricite (e)

real(pm_reel), intent(out)               :: anom_E      ! anomalie excentrique (E)
type(tm_code_retour), intent(out)        :: code_retour !code retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: eps100             !     nom constante eps    
real(pm_reel) :: pi                 !     pi
real(pm_reel) :: eps_cir, eps_parab !     epsilons de test pour orbites circulaire ou parabolique
real(pm_reel) :: e0,ei,eip1         !     valeurs initiale ,courante et suivante pour resolution iterative
real(pm_reel) :: az                 !     variable intermediaire az pour initialisation de encke
real(pm_reel) :: cosM,sinM          !     variables intermediaires de calcul de cosinus et sinus de M
real(pm_reel) :: rapport, az2       !     variables intermediaires de calcul
real(pm_reel) :: anom_M0            !     anomalie moyenne proche de 0
real(pm_reel) :: err1, err2         !     criteres de convergence

integer       :: iter               !     indice d'iteration
integer       :: kpi                !     multiple de pi

integer, parameter :: max_nb_iter = 20 !  nombre maximum d'iterations

intrinsic epsilon, sin, cos, abs, sqrt, real, mod, nint, max

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_kepler_std.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kepler_std.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

eps100  =  100._pm_reel  * epsilon(1._pm_reel)                 ! constante eps pour les reels

pi = pm_pi  ! recuperation de la valeur de pi
! recuperation des epsilons de test sur l'excentricite
eps_cir = pm_eps_cir
eps_parab = pm_eps_parab

! controle de l'excentricite ( e dans [0,1[)
! ===========================================
if (e < 0._pm_reel) then                   ! excentricite negative
   code_retour%valeur = pm_err_e_negatif
   go to 6000
end if

if (e > (1._pm_reel-eps_parab)) then       ! orbite parabolique ou hyperbolique
   code_retour%valeur = pm_err_e_non_ellip
   go to 6000
end if

! traitement des cas de fonctionnement non optimal:     exc < eps_cir (orbite circulaire),    sin(M) < 100*eps
! ==========================================================================================================

if (e < eps_cir)  then !     orbite circulaire : pas de code retour necessaire
   anom_E = anom_M
   go to 6000
end if

sinM = sin(anom_M)     !     calcul du sinus de l'anomalie moyenne
cosM = cos(anom_M)     !     calcul du cosinus de l'anomalie moyenne

if (abs(sinM) < eps100) then    !     M est "presque" un multiple de pi: M = M0 + kpi.pi
                                !                                        avec M0 nul ou proche de 0
   kpi = nint(anom_M/pi)
   anom_M0 = anom_M - real(kpi, kind=pm_reel)*pi

   if (mod(kpi,2) == 0) then   ! determination de la parite de kpi
      anom_E = real(kpi, kind=pm_reel)*pi + anom_M0/(1._pm_reel - e)
   else
      anom_E = real(kpi, kind=pm_reel)*pi + anom_M0/(1._pm_reel + e)
   end if

   go to 6000
end if

!initialisation de encke
!=======================
!
rapport = (e - cosM) / sinM
az = e / sqrt (1._pm_reel+(rapport*rapport))
!
if (sinM < 0._pm_reel)  az = -az
!
az2 = az*az
e0 = anom_M + az - ( az2*az2*cosM*(1._pm_reel-e*e)/(6._pm_reel*sinM) )

!resolution iterative par methode de newton
!    f(x) = 0
!    xi+1 = xi - (f(xi) / f'(xi))
!==========================================

eip1 = e0         !
iter = 0          !  Initialisations avant boucle do while
err1 = 1._pm_reel !
err2 = 1._pm_reel !

do while ((err1 > eps100) .and. (err2 > eps100).and.(iter /= max_nb_iter))
   ei = eip1
   iter = iter + 1
   
   eip1 = ei - ((anom_M + e*sin(ei) - ei) / (e*cos(ei) - 1.0_pm_reel))   !        calcul pour l'iteration courante
   err1 = abs(eip1-ei)/max(abs(eip1),1.0_pm_reel)   !        calcul du residu
   err2 = abs(anom_M + e*sin(eip1) - eip1)/max(abs(anom_M),1.0_pm_reel)

end do

if ((err1 > eps100) .and. (err2 > eps100)) then !     pas de convergence au bout du nombre maximum d'iterations
   code_retour%valeur = pm_err_conv_kepler_ellip
   go to 6000
end if

! affectation du resultat

anom_E = eip1

6000 continue

code_retour%routine = pm_num_mv_kepler_std
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kepler_std
