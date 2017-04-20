subroutine mi_pro_j2(mu, r_equa_pla, coeff_j2, duree, osc_t0, osc_t1, code_retour, mat_trans)

!***********************************************************************
!
! But:  extrapolation des parametres d'orbite en fonction du j2 \
! ===	avec possibilite de calcul de la jacobienne. \
!	
!
!$Historique
! ==========
!   + version 6.3 : DM-ID 162 : introduction d'une routine de propagation d'orbite prenant en compte le 
!                               terme central du potentiel gravitationnel et les effets seculaires du J2
!                   creation a partir de la routine AM_pro_exorj2 de l' AMLIB
!                   (date: 11/2005 - realisation: Julien Bouillant - Atos Origin)
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
!$Remarques
!     Les unites sont choisies ainsi :
!                        m  ,  m3.s-2  et s
!***********************************************************************

! modules
! =======

use int_utilitaires, only : mu_norme

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! declarations
! ===========

implicit none

intrinsic present

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(kind=pm_reel), intent(in)                             :: mu         ! constante d'attraction de la planète
real(kind=pm_reel), intent(in)                             :: r_equa_pla ! rayon équatorial de la planète
real(kind=pm_reel), intent(in)                             :: coeff_j2   ! coefficient J2 du potentiel de la planète
real(kind=pm_reel), intent(in)                             :: duree      ! durée de transfert
type(tm_orb_kep), intent(in)                               :: osc_t0     ! paramètres orbitaux à la date initiale
type(tm_orb_kep), intent(out)                              :: osc_t1     ! paramètres orbitaux après la durée du transfert
type(tm_code_retour), intent(out)                         :: code_retour
real(kind=pm_reel), dimension(6,6), intent(out), optional :: mat_trans  ! matrice de transition

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-------------------------------
!	variables locales
!-------------------------------
real (kind=pm_reel), dimension(3) :: dgomp,dpomp,damp
real(kind=pm_reel) :: pomp,gomp,amp
integer :: i, j

! variable intermediaire de calcul
real(kind=pm_reel) :: coef, coef2
! termes en excentricite
real(kind=pm_reel) :: ecarre, unmoinse2, esurfe
! sinus de l'inclinaison et sinus de deux fois l'inclinaison
real(kind=pm_reel) :: sini, sin2i
!cosinus et cosinus carre de l'inclinaison
real(kind=pm_reel) :: cosi, cosi2
! pulsation moyenne et pulsation moyenne au carre
real(kind=pm_reel) :: xn, xn2

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) fichier mslib mi_pro_j2.f90: derniere modification V6.13 >'
! ne pas toucher à la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: &
     rcs_id =' $Id: mi_pro_j2.f90 362 2013-02-15 18:01:28Z bbjc $ '

!-------------------------------
!	initialisation
!-------------------------------
! initialisation de la valeur du code retour

code_retour%valeur = pm_ok

!-------------------------------
! corps du programme 
!-------------------------------
!-------------------------------------------------------------------------------
!     tests sur les parametres d'entree
!-------------------------------------------------------------------------------

if (osc_t0%a <=  0._pm_reel) then
   code_retour%valeur = pm_err_a_negatif
   go to 6000
endif

if (osc_t0%e >= 1._pm_reel) then
   code_retour%valeur = pm_err_e_non_ellip
   go to 6000
endif

if (osc_t0%e < 0._pm_reel) then
   code_retour%valeur = pm_err_e_negatif
   go to 6000
endif

if (osc_t0%i < 0._pm_reel) then
   code_retour%valeur = pm_err_i_negatif
   go to 6000
endif

if (mu < 0._pm_reel) then
   code_retour%valeur = pm_err_mu_negatif
   go to 6000
endif

if (duree < 0._pm_reel) then
   code_retour%valeur = pm_err_duree_negatif
   go to 6000
endif

if (r_equa_pla < 0._pm_reel) then
   code_retour%valeur = pm_err_r_equa_negatif
   go to 6000
endif

if (coeff_j2 < 0._pm_reel) then
   code_retour%valeur = pm_err_j2_negatif
   go to 6000
endif

!-------------------------------------------------------------------------------
!	calcul des variables locales et intermediaires
!-------------------------------------------------------------------------------

xn2 = mu / (osc_t0%a * osc_t0%a * osc_t0%a)
xn = sqrt(xn2)
ecarre = osc_t0%e * osc_t0%e
unmoinse2 = (1._pm_reel - ecarre)
esurfe = osc_t0%e / unmoinse2
coef = -0.75_pm_reel * xn * coeff_j2 * r_equa_pla * r_equa_pla / &
     (unmoinse2 * unmoinse2 * osc_t0%a * osc_t0%a)
sini = sin(osc_t0%i)
sin2i = 2._pm_reel * cos(osc_t0%i) * sini
cosi = cos(osc_t0%i)
cosi2 = cosi * cosi
coef2 = (1.5_pm_reel * xn * coeff_j2 * r_equa_pla * r_equa_pla) / &
     (osc_t0%a * osc_t0%a * unmoinse2 * unmoinse2)

!-------------------------------------------------------------------------------
!	calcul des parametres gomp, pomp, amp.
!-------------------------------------------------------------------------------

pomp = 0.5_pm_reel * coef2 * (5._pm_reel*cosi2 - 1._pm_reel)
gomp = -coef2 * cosi
amp  = xn + &
     0.5_pm_reel * coef2 * (3._pm_reel*cosi2 - 1._pm_reel) * sqrt(unmoinse2)

!-------------------------------------------------------------------------------
!	calcul des derivees partielle par rapport a a, e, et i
!	et de gomp, pomp, amp
!	 (1. par rapport a a
!-------------------------------------------------------------------------------

dgomp(1) = -3.5_pm_reel * gomp / osc_t0%a
dpomp(1) = -3.5_pm_reel * pomp / osc_t0%a
damp(1) = (2._pm_reel * xn - 3.5_pm_reel * amp) / osc_t0%a

!-------------------------------------------------------------------------------
!     	(2. par rapport a e
!-------------------------------------------------------------------------------

dgomp(2) = 4._pm_reel * esurfe * gomp
dpomp(2) = 4._pm_reel * esurfe * pomp
damp(2) = 3._pm_reel * esurfe * (amp - xn)

!-------------------------------------------------------------------------------
!    	(3. par rapport a i
!-------------------------------------------------------------------------------

dgomp(3) = -2._pm_reel * coef * sini
dpomp(3) = 5._pm_reel * coef * sin2i
damp(3) = 3._pm_reel * coef * sqrt(unmoinse2) * sin2i

!-----------------------------------------------------------------------
!     extrapolation des parametres
!-----------------------------------------------------------------------
osc_t1%a = osc_t0%a
osc_t1%e = osc_t0%e
osc_t1%i = osc_t0%i
osc_t1%pom = osc_t0%pom + duree*pomp
osc_t1%gom = osc_t0%gom + duree*gomp
osc_t1%M = osc_t0%m + duree*amp

!-----------------------------------------------------------------------
!     passage modulo 2*pi
!-----------------------------------------------------------------------
osc_t1%pom = mod(osc_t1%pom,pm_deux_pi)
if (osc_t1%pom < 0._pm_reel) osc_t1%pom = osc_t1%pom + pm_deux_pi

osc_t1%gom = mod(osc_t1%gom,pm_deux_pi)
if (osc_t1%gom < 0._pm_reel) osc_t1%gom = osc_t1%gom + pm_deux_pi

osc_t1%M = mod(osc_t1%M,pm_deux_pi)
if (osc_t1%M < 0._pm_reel) osc_t1%M = osc_t1%M + pm_deux_pi

!-----------------------------------------------------------------------
!     calcul des dérivees 
!-----------------------------------------------------------------------
if(present(mat_trans)) then
   !-----------------------------------------------------------------------
   !        initialisation à l'identite de la matrice
   !-----------------------------------------------------------------------
   do i=1,6
      do j=1,6
         mat_trans(i,j) = 0._pm_reel
      enddo
      mat_trans(i,i) = 1._pm_reel
   enddo
   
   !-----------------------------------------------------------------------
   !        on remplit maintenant les termes non diagonaux
   !-----------------------------------------------------------------------
   do i=1,3
      mat_trans(4,i) = duree*dpomp(i)
      mat_trans(5,i) = duree*dgomp(i)
      mat_trans(6,i) = duree*damp(i)
   enddo
   
endif
!-------------------------------
!	gestion des erreurs
!-------------------------------

6000 continue

code_retour%routine = pm_num_mi_pro_j2
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_ok) code_retour%message = ' '

end subroutine mi_pro_j2
