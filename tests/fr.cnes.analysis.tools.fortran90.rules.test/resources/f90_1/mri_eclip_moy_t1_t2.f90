subroutine mri_eclip_moy_t1_t2 (model, jul1950_t1, jul1950_t2, mat_pass, retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul de la matrice de passage du repere ECLIPtique MOYen a T1 au repere ECLIPtique MOYen a T2.
! ===
!
! Note d'utilisation: - Routine interne
! ==================  
!
!$Historique
! ==========
!   + Version 1.0 (SP 274 ed01 rev00): creation a partir de la routine MRCCMO de la MSLIB f77
!                         (Date: 09/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 362 ed01 rev00): suppression des commentaires sur la limitation sur les dates et des codes retour
!                                      utilisation de md_joursec_jourfrac, revision clef de modele
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!       remplacement du module math_mslib (supprimé) par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg
use type_mslib
use parametre_mslib

use int_dates, only : md_joursec_jourfrac

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model      ! indicateur du modele --> pm_lieske: LIESKE 
type(tm_jour_sec), intent(in)       :: jul1950_t1 ! date du repere initial
type(tm_jour_sec), intent(in)       :: jul1950_t2 ! date du repere final

real(pm_reel), dimension(3,3), intent(out) :: mat_pass ! matrice de passage
integer, intent(out)                ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)    ::    t1,t2                                      ! date d'origine,date de destination en jours juliens fract.
real(pm_reel)    ::    c10,c21                                    ! ecarts sur les dates pour Lieske
real(pm_reel)    ::    pa,teta                                    ! precession,angle entre les 2 ecliptiques
real(pm_reel)    ::    psi,phi                                    ! arc gamma1.n, arc -gamma2.n
real(pm_reel)    ::    sec_rad                                    ! constantes de passage des secondes aux rad
real(pm_reel)    ::    a,a2,b,b2,b3,c1,c2,c3,s1,s2,s3             ! variables auxiliaires pour le calcul
real(pm_reel), parameter       ::  delta_te_tai = 32.184_pm_reel  ! difference entre le temps des ephemerides et le tai
type(tm_jour_sec)              ::  jul1950_temp                   ! variable temporaire pour appel md_joursec_jourfrac
type(tm_code_retour)           ::  code_retour_local

!   coefficients de la precession selon Lieske
real(pm_reel), dimension(6), parameter ::   &
       pl  = (/50290.966_pm_reel,222.226_pm_reel,-.042_pm_reel,111.113_pm_reel,-0.042_pm_reel,-0.006_pm_reel/)    
!   coeff. de l'expression de l'angle entre les 2 ecliptiques 
real(pm_reel), dimension(6), parameter :: &
       tl = (/470.029_pm_reel,-6.603_pm_reel,0.598_pm_reel,-3.302_pm_reel,0.598_pm_reel,0.060_pm_reel/) 
!   coef. de gamma1 selon Lieske
real(pm_reel), dimension(6), parameter ::  &
       psl= (/34.982_pm_reel,32894.789_pm_reel,60.622_pm_reel,-8698.089_pm_reel,-50.491_pm_reel,3.536_pm_reel/)
!     coeff. de l'expression de gamma1.n selon Lieske
real(pm_reel), parameter    ::    psl1 = 174._pm_reel ,psl2 = 52._pm_reel

!     . . . initialisation de l'epoque de base du modele exprimees en jours juliens cnes
real(pm_reel), parameter  ::   tl0 =  18262.5_pm_reel     !   origine lieske = j2000
intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mri_eclip_moy_t1_t2.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mri_eclip_moy_t1_t2.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

if (model == pm_lieske ) then ! calcul de la matrice de passage du repere ecliptique moyen a t1 au repere ecliptique moyen a t2
                                  ! avec le modele de precession de Lieske (epoque de base : J2000)

   sec_rad = pm_deg_rad/3600._pm_reel     !  initialisation de sec_rad

   !     . . . calcul des dates en jours juliens cnes fractionnaires
   jul1950_temp%jour = jul1950_t1%jour
   jul1950_temp%sec  = jul1950_t1%sec + delta_te_tai
   call md_joursec_jourfrac ( jul1950_temp, t1, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste
   jul1950_temp%jour = jul1950_t2%jour
   jul1950_temp%sec  = jul1950_t2%sec + delta_te_tai
   call md_joursec_jourfrac ( jul1950_temp, t2, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste

   ! ----------------------------------------------------------------
   ! calcul constantes de la precession (modele de Lieske)
   ! -----------------------------------------------------------------

   !     les coefficients pl,tl,psl sont en secondes d'arc
   !     psl1 est en deg, psl2 en minutes
   !     pa,teta,psi ei phi sont calcules en radians

   !        c20 et c12 sont exprimees en milliers d'annees juliennes de 365250 jours
   c10 = (t1-tl0)/365250._pm_reel
   c21 = (t2-t1)/ 365250._pm_reel

   a=c10
   a2=a*a
   b=c21
   b2=b*b
   b3=b2*b

   !        calcul de la precession
   pa = (pl(1)+pl(2)*a+pl(3)*a2)*b + (pl(4)+pl(5)*a)*b2 +pl(6)*b3
   pa = pa*sec_rad

   !        calcul de l'angle entre les 2 ecliptiques
   teta = (tl(1)+tl(2)*a+tl(3)*a2)*b + (tl(4)+tl(5)*a)*b2 + tl(6)*b3
   teta = teta*sec_rad

   !        calcul de l'arc gamma1.n
   psi = psl(1)+psl(2)*a+psl(3)*a2 + (psl(4)+psl(5)*a)*b+psl(6)*b2
   psi = psi*sec_rad +psl1*pm_deg_rad + psl2*pm_deg_rad/60._pm_reel

   !        calcul de l'arc gamma2.n = -gamma1.n - precession
   phi = -psi-pa

   ! -------------------------------------------------------------
   ! calcul de la matrice de passage du repere ecliptique moyen
   ! a t1 au repere ecliptique moyen a t2
   ! le passage de l'ancien repere au nouveau se fait par 3 rotations
   !  - rotation d'angle psi autour de oz (l'axe ox vient en oxu)
   !  - rotation d'angle teta autour de oxu (oz vient en ozn)
   !  - rotation d'angle phi autour de ozn (l'axe oxu vient en oxn)
   !------------------------------------------------------------------

   !     . . . calcul cosinus et sinus des angles de rotations
   c1=cos(psi)
   s1=sin(psi)
   c2=cos(teta)
   s2=sin(teta)
   c3=cos(phi)
   s3=sin(phi)

   !     . . . calcul de la matrice de passage
   mat_pass(1,1)=c1*c3-s1*c2*s3
   mat_pass(2,1)=-c1*s3-s1*c2*c3
   mat_pass(3,1)=s1*s2

   mat_pass(1,2)=s1*c3+c1*c2*s3
   mat_pass(2,2)=-s1*s3+c1*c2*c3
   mat_pass(3,2)=-c1*s2

   mat_pass(1,3)=s2*s3
   mat_pass(2,3)=s2*c3
   mat_pass(3,3)=c2

else   ! modele inconnu

   retour = pm_err_ind_model

end if

end subroutine mri_eclip_moy_t1_t2
