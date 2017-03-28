subroutine mri_equa_moy_t1_t2 (model, jul1950_t1, jul1950_t2, mat_pass, retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul de la matrice de passage du repere EQUAtorial MOYen a T1 a ce meme repere a T2.
! ===
!
! Note d'utilisation: - Routine interne
! ================== 
! 
!$Historique
! ==========
!   + Version 1.0 (SP 264 ed01 rev00): creation a partir de la routine MRQQMO de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 362 ed01 rev00): suppression des commentaires sur la limitation sur les dates et des codes retour
!                                      utilisation de md_joursec_jourfrac, revision clef de modele
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!       remplacement du module math_mslib (supprimé) par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.8 : DM-ID 859 : utilisation de matmul3
!                   (Date: 03/2008 - Realisation: Atos origin)
!
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
use int_rep_internes, only : mri_eclip_M_equa_M
use int_dates, only : md_joursec_jourfrac

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use type_mslib
use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)       :: t1               !     date d'origine en jours juliens cnes fract.
real(pm_reel)       :: t                !     ecart entre date d'origine et epoque de base du modele

real(pm_reel)       :: eps              !     obliquite moyenne au temps de destination
!     obliquite moyenne a l'epoque de base selon Lieske 
real(pm_reel)     :: e0               
real(pm_reel), dimension(3), parameter   :: el0 = (/23._pm_reel,26._pm_reel,21.448_pm_reel/)
!     coefficients de l'expression de l'obliquite selon Lieske
real(pm_reel), dimension(3), parameter   :: epsl = (/-468.150_pm_reel,-0.059_pm_reel,1.813_pm_reel/)
real(pm_reel), parameter                 :: delta_te_tai = 32.184_pm_reel  !     difference entre le temps des ephemerides et le tai
!  initialisation de la constante de passage des secondes aux degres
real(pm_reel), parameter        ::    sec_rad = pm_deg_rad/3600._pm_reel    
! initialisation de l'epoque de base du modele Lieske exprimee en jours juliens cnes (origine lieske = j2000.0)
real(pm_reel), parameter        :: tl0 = 18262.5_pm_reel

!     variables auxiliaires pour le calcul
real(pm_reel), dimension(3,3)   :: mat_pass1,mat_pass2
real(pm_reel)                   :: ta2,ta3,ceps,seps

integer   ::  modele_precession   ! indicateur du modele de precession correspondant a "model"

type(tm_jour_sec)              ::  jul1950_temp               ! variable temporaire pour appel md_joursec_jourfrac
type(tm_code_retour)           ::  code_retour_local

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_equa_moy_t1_t2.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_equa_moy_t1_t2.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

if (model == pm_lieske) then! calcul de la matrice de passage du repere equatorial moyen a t1 au repere equatorial moyen a t2
                                 ! avec le modele de precession de Lieske (epoque de base : J2000)

   modele_precession = pm_lieske

   !     ---------------
   !     initialisations
   !     ---------------

   !calcul de la date en jours juliens cnes fractionnaires
   jul1950_temp%jour = jul1950_t1%jour
   jul1950_temp%sec  = jul1950_t1%sec + delta_te_tai
   call md_joursec_jourfrac ( jul1950_temp, t1, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste

  

   ! ---------------------------------
   ! calcul de l'obliquite au temps t1
   ! ---------------------------------

   !      les coefficients epsl sont en secondes d'arc
   !      el0(1) en degres
   !      el0(2) en minutes
   !      el0(3) en secondes
   !      l'obliquite eps est exprimee en radians

   t=(t1-tl0)/365250._pm_reel   !        t est exprimee en milliers d'annees juliennes de 365250 jours
   ta2=t*t
   ta3=ta2*t

   e0 = el0(1)*pm_deg_rad + el0(2)*pm_deg_rad/60._pm_reel +el0(3)*sec_rad
   eps = e0 + (epsl(1)*t+epsl(2)*ta2+epsl(3)*ta3)*sec_rad

   ! ----------------------------------------------------------------------------------------------
   ! calcul de la matrice de passage du repere equatorial moyen a t1 au repere ecliptique moyen a t1
   ! (rotation autour de ox d'angle egal a l'obliquite
   ! ----------------------------------------------------------------------------------------------

   mat_pass1(:,:) = 0._pm_reel

   ceps=cos(eps)
   seps=sin(eps)
   mat_pass1(1,1)=1._pm_reel
   mat_pass1(2,2)= ceps
   mat_pass1(3,2)=-seps
   mat_pass1(2,3)= seps
   mat_pass1(3,3)= ceps

   ! ------------------------------------------------------------------------------------
   ! calcul de la matrice de passage de l'ecliptique moyen a t1 a l'equatorial moyen a t2
   ! ------------------------------------------------------------------------------------

   call mri_eclip_M_equa_M(modele_precession, jul1950_t1, jul1950_t2 ,mat_pass2, retour)
   ! pas de test de retour car il ne peut etre que nul (controles deja effectues)

   ! ------------------------------------------------------------------------------------
   ! calcul de la matrice de passage de l'equatorial moyen a t1 a l'equatorial moyen a t2
   ! ------------------------------------------------------------------------------------

   call mu_matmul3(mat_pass2,mat_pass1,mat_pass,code_retour_local)
   ! pas d'erreur possible donc pas de test dur code retour

else   ! erreur de code de  modele

   retour = pm_err_ind_model

end if

end subroutine mri_equa_moy_t1_t2
