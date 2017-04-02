subroutine mv_kepler_gene (pso_M, ex, ey, pso_E, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Resolution de l'equation de KEPLER generalisee.
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 222 ed01 rev00): creation a partir de la routine MVRXKG de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
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

use int_utilitaires, only : mu_angle2

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                            :: pso_M       !  argument du perigee + anomalie moyenne (+ longitude du noeud ascendant)
real(pm_reel), intent(in)                            :: ex          !  composante x du vecteur excentricite
real(pm_reel), intent(in)                            :: ey          !  composante y du vecteur excentricite

real(pm_reel), intent(out)                           :: pso_E       ! argument du perigee + anomalie excentrique (+ longitude du noeud ascendant)
type(tm_code_retour), intent(out)                    :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel)         :: excen, e2               ! excentricite et carre de l'excentricite
real(pm_reel)         :: cos_pso_M, sin_pso_M    ! cosinus et sinus de la position sur orbite en entree
real(pm_reel)         :: eps100                  ! epsilon de test pour les reels
real(pm_reel)         :: anomalie_moyenne, anomalie_moyenne0 ! anomalies moyennes
real(pm_reel)         :: rapport, az, az2, x, y  ! variables intermediaires de calcul
real(pm_reel)         :: f0, fi, fip1            ! initialisation de la fonction, fonction et derivee premiere de la fonction
real(pm_reel)         :: err1, err2              ! criteres de convergence
real(pm_reel)         :: alpha                   ! phase du vecteur excentricite

integer                             :: kpi                     ! multiple de pi
integer                             :: nb_iterations           ! compteur d'iterations
integer, parameter                  :: nb_max_iterations = 20  ! nombre maximum d'iterations

intrinsic epsilon, sqrt, cos, sin, abs, nint, modulo, max, real

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_kepler_gene.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kepler_gene.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

eps100 = 100._pm_reel * epsilon(1._pm_reel)                          ! initialisation constante de test sur les reels

! test sur l'excentricite
! =======================

                                                
e2 = (ex**2) + (ey**2)  ! carre de l'excentricite
excen = sqrt(e2)        ! excentricite

if (excen > (1._pm_reel - pm_eps_parab)) then       ! orbite parabolique ou hyperbolique
   code_retour%valeur = pm_err_e_non_ellip
   go to 6000
end if

if (excen < pm_eps_cir) then       ! orbite circulaire : pas de code retour necessaire
   pso_E = pso_M
   go to 6000
end if

!initialisation de encke
!=======================

cos_pso_M = cos (pso_M)
sin_pso_M = sin (pso_M)

x = (ex * cos_pso_M) + (ey * sin_pso_M)    
y = (ex * sin_pso_M) - (ey * cos_pso_M)     

if (abs(y)/excen <= eps100) then            !       y/e < ou = eps100 -> traitement special
   call mu_angle2(ex,ey,alpha,code_retour)  !       pas de code retour a tester en retour de mu_angle2 car ex et ey
                                            !       ne peuvent etre nuls en meme temps car a ce stade des calculs
                                            !       l'excentricite est non nulle
 
   anomalie_moyenne = pso_M - alpha         

   kpi = nint(anomalie_moyenne/pm_pi)          
   anomalie_moyenne0 = anomalie_moyenne - real(kpi, kind=pm_reel)*pm_pi  ! M est "presque" un multiple de pi: M = M0 + kpi.pi
                                                                         !   avec M0 nul ou proche de 0

   if (modulo(kpi,2) == 0) then   ! determination de la parite de kpi
      pso_E = alpha + real(kpi, kind=pm_reel)*pm_pi + anomalie_moyenne0/(1._pm_reel - excen)
   else
      pso_E = alpha + real(kpi, kind=pm_reel)*pm_pi + anomalie_moyenne0/(1._pm_reel + excen)
   end if

   go to 6000   !     pas de code retour positif necessaire
end if

rapport = (e2 - x)/y
az = excen/sqrt(1.0_pm_reel + rapport*rapport)
if (y < 0._pm_reel) az = - az

az2 = az*az

f0 = pso_M + az - (az2*az2*x*(1._pm_reel-e2))/(6._pm_reel*y)

! resolution iterative par methode de newton
!    f(x) = 0
!    xi+1 = xi - (f(xi) / f'(xi))
!==============================================

fi = f0          !
err1= 1._pm_reel ! Initialisations avant iterations
err2= 1._pm_reel !
nb_iterations=0  !

do while ((err1 > eps100) .and. (err2 > eps100) .and. (nb_iterations /= nb_max_iterations))

   fip1 = fi - ((fi - (ex*sin(fi)) + (ey*cos(fi)) - pso_M) /(1._pm_reel - (ex*cos(fi)) - (ey*sin(fi)))) 
        
   err1 = abs(fip1-fi)/max(abs(fip1),1._pm_reel)     ! calcul des criteres de convergence
   err2 = pso_M - fip1 + ex*sin(fip1) - ey*cos(fip1) ! err1 et
   err2 = abs(err2)/max(abs(pso_M),1._pm_reel)       ! err2

   fi = fip1

   nb_iterations = nb_iterations + 1 ! incrementation du compteur d'iterations

end do    
if ((err1 > eps100) .and. (err2 > eps100)) then   ! pas de convergence apres le nombre maximum d'iterations
    code_retour%valeur= pm_err_conv_kepler_gene
    go to 6000
end if

   pso_E = fi

6000 continue

code_retour%routine = pm_num_mv_kepler_gene
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kepler_gene
