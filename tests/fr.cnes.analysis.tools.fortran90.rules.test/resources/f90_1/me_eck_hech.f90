subroutine me_eck_hech ( mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2 )

! (C) Copyright CNES - MSLIB - 1999-2004

!************************************************************************
!
! But:  Modele analytique d'extrapolation d'orbite de ECKSTEIN-HECHLER.
! ===
!
! Note d'utilisation:  L'excentricite  doit etre superieure >= 0 et < 0.1.
! ==================   Le domaine d'utilisation du modele est .. [ 0E+0 , 5E-3 [.
!                      Le domaine de precision degradee est .... [ 5E-3 , 1E-1 [.
!                      Le domaine d'erreur est ................. [ 1E-1 , +Oo  [.
!                      L'inclinaison  doit etre > 0. et < pi et non proches des inclinaisons critiques
!                      ( pm_i_critique_non_retro, pm_i_critique_retro ) a pm_eps_i_critique pres.
!
!$Historique
! ==========
!   + Version 2.0 (SP 341 ed01 rev00): creation a partir de la routine MEEHJ6 de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.0 (DE globale 618 ed01 rev00): ajout de tests sur mu et les coefficients CN0
!                         (Date: 02/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules math_mslib et phys_mslib par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : FA-ID 1108 Ajout de parenthèses dans expressions ambigues
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
use int_utilitaires, only : mu_angle2
use int_dates, only : md_joursec_jourfrac

use precision_mslib
use type_mslib

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use int_constantes, only : pm_i_critique_non_retro,pm_i_critique_retro

use test_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                                  intent(in)  :: mu          ! constante gravitationnelle terrestre
real(pm_reel),                                  intent(in)  :: r_equa      ! rayon equatorial terrestre
real(pm_reel),        dimension(2:6),           intent(in)  :: cn0         ! coeff. zonaux (c20 a c60) du developpement
type(tm_jour_sec),                              intent(in)  :: t1          ! date initiale t1 des parametres moyens (jours, sec)
type(tm_orb_cir),                               intent(in)  :: moy_t1      ! parametres moyens a la date t1
type(tm_jour_sec),                              intent(in)  :: t2          ! date finale t2 des parametres osculateurs (jours, sec)
type(tm_orb_cir),                               intent(out) :: moy_t2      ! parametres moyens a la date t2
type(tm_code_retour),                           intent(out) :: code_retour
type(tm_orb_cir),                     optional, intent(out) :: osc_t2      ! parametres osculateurs extrapoles a t2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
real(pm_reel), dimension(6) :: g, b, cl, sl                               ! variables intermediaires
real(pm_reel)               :: a0, i0, om0, ex0, ey0, l0                  ! variables intermediaires
real(pm_reel)               :: e,  jour1, jour2                           ! variables intermediaires
real(pm_reel)               :: q, ql, c, xnot, am, delta_pom              ! variables pour l'evolution seculaire
real(pm_reel)               :: delta_pomp, x, cx, sx, eps1,eps2           ! variables pour l'evolution seculaire
real(pm_reel)               :: exm, eym, im, omm, delta_l, lm             ! variables pour l'evolution seculaire
real(pm_reel)               :: qq, qh, f, delta_a, delta_ex, delta_ey     ! termes periodiques
real(pm_reel)               :: delta_gom, delta_i, ddelta_l               ! termes periodiques
real(pm_reel)               :: ex, ey, om, xi, xl                         ! termes periodiques
real(pm_reel),parameter     :: e_borne_inf_err  = 1.e-1_pm_reel           ! borne inferieure domaine d'erreur
real(pm_reel),parameter     :: e_borne_inf_degr = 5.e-3_pm_reel           ! borne inferieure domaine de precision degradee
real(pm_reel)               :: eps100                                     ! variable  epsilon machine * 100
integer                     :: i, j                                       ! indices de boucle
integer                     :: code_warning                               ! code pour gestion des warnings

type(tm_code_retour)        :: code_retour_local

intrinsic abs, epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB me_eck_hech.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: me_eck_hech.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK
code_warning       = pm_OK

! initialisation de la valeur epsilon * 100
! =========================================

eps100  =  100._pm_reel * epsilon(1._pm_reel)

! controle des donnees
! ====================

! constante de la gravitation

if (mu <= eps100) then
   if (mu < 0._pm_reel) then              ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul  ! constante de la gravitation nulle
   end if
   go to 6000
end if

! zonaux
do i = 2,6
   if (abs(cn0(i)) <= eps100) then
      code_retour%valeur = pm_err_cn0_nul  ! coefficient zonal nul
      go to 6000
   end if
end do

! excentricite
! ............
e = sqrt((moy_t1%ex)**2+(moy_t1%ey)**2)

if (e < 0._pm_reel) then
   code_retour%valeur= pm_err_e_negatif          ! excentricite negative
   go to 6000
end if
if ((e >= e_borne_inf_degr)                                                                                    &
   .and. (e <e_borne_inf_err)) then
   code_warning = pm_warn_e_grand_eck_hech       ! excentricite dans le domaine de precision degradee
end if
if (e >= e_borne_inf_err) then   
   code_retour%valeur = pm_err_e_grand_eck_hech  ! excentricite dans le domaine d'erreur
   go to 6000
end if

! inclinaison
! ...........
if (moy_t1%i < 0._pm_reel) then                  ! inclinaison negative
   code_retour%valeur= pm_err_i_negatif
   go to 6000 
else if (moy_t1%i > pm_pi) then                  ! inclinaison superieure a pi
   code_retour%valeur= pm_err_i_sup_pi
   go to 6000
else if (sin(moy_t1%i) <= pm_eps_equa) then      ! orbite equatoriale
   code_retour%valeur= pm_err_i_equa
   go to 6000
end if

if ((moy_t1%i > (pm_i_critique_non_retro-pm_eps_i_critique)).and. &      ! inclinaison proche de pm_i_critique_non_retro
     (moy_t1%i < (pm_i_critique_non_retro+pm_eps_i_critique))) then      ! ( a pm_eps_i_critique radians pres )
   code_retour%valeur = pm_err_i_critique
   go to 6000
else if ((moy_t1%i > (pm_i_critique_retro-pm_eps_i_critique)).and. &     ! inclinaison proche de pm_i_critique_retro
          (moy_t1%i < (pm_i_critique_retro+pm_eps_i_critique))) then     ! ( a pm_eps_i_critique radians pres )
   code_retour%valeur = pm_err_i_critique                   
   go to 6000
end if

! recuperation valeurs parametres moyens en entree
! ................................................
a0   = moy_t1%a
i0   = modulo(moy_t1%i,pm_deux_pi)
om0  = modulo(moy_t1%gom,pm_deux_pi)
ex0  = moy_t1%ex                          ! e cos(w)
ey0  = moy_t1%ey                          ! e sin(w)
l0   = modulo(moy_t1%pso_M,pm_deux_pi)    ! w + M

! calculs preliminaires
! .....................
! calcul de : g(l) = cn0(l) * ( r_equa / a0 )**l
q  = r_equa / a0
ql = q
do i = 2, 6
   ql   = ql * q
   g(i) = cn0(i) * ql
end do

! c    = cos(i0)
! b(i) = sin(i0)**i
c    = cos(i0)
b(1) = sin(i0)
do i = 2, 6
   j    = i-1
   b(i) = b(j) * b(1)
end do

! evolution seculaire
! ...................
call md_joursec_jourfrac(t1, jour1, code_retour_local)
if (code_retour_local%valeur < 0) then
   code_retour%valeur = code_retour_local%valeur
   go to 6000
else if (code_retour_local%valeur > 0) then
   code_retour%valeur = code_retour_local%valeur
end if

call md_joursec_jourfrac(t2, jour2, code_retour_local)
if (code_retour_local%valeur < 0) then
   code_retour%valeur = code_retour_local%valeur
   go to 6000
else if (code_retour_local%valeur > 0) then
   code_retour%valeur = code_retour_local%valeur
end if

xnot  = sqrt(mu/a0) / a0 * (jour2 - jour1) * 86400._pm_reel

! demi grand axe
am = a0

! vecteur excentricite      
delta_pom  = -0.75_pm_reel * g(2) * ( 4._pm_reel - 5._pm_reel * b(2) )
delta_pomp = 7.5_pm_reel * g(4) * ( 1._pm_reel - 31._pm_reel / 8._pm_reel * b(2) + 49._pm_reel / 16._pm_reel * b(4) )
delta_pomp = delta_pomp - 13.125_pm_reel * g(6) * ( 1._pm_reel - 8._pm_reel * b(2) + 129._pm_reel / 8._pm_reel * b(4) -          &
             297._pm_reel / 32._pm_reel * b(6) )
x          = ( delta_pom + delta_pomp ) * xnot
x          = x - aint(x/pm_deux_pi, kind=pm_reel) * pm_deux_pi
cx         = cos(x)
sx         = sin(x)
q          = (3._pm_reel / 32._pm_reel) / delta_pom
eps1       = q * g(4) * b(2) * ( 30._pm_reel - 35._pm_reel * b(2) ) - 175._pm_reel * q * g(6) * b(2)                             &
             * ( 1._pm_reel - 3._pm_reel * b(2) + 33._pm_reel / 16._pm_reel * b(4) )
q          = 3._pm_reel / 8._pm_reel * b(1) / delta_pom
eps2       = q * g(3) * ( 4._pm_reel - 5._pm_reel * b(2) ) - q * g(5) * ( 10._pm_reel - 35._pm_reel * b(2) + 26.25_pm_reel * b(4) )
exm        = ex0 * cx - ( 1._pm_reel - eps1 ) * ey0 * sx + eps2 * sx
eym        = ( 1._pm_reel + eps1 ) * ex0 * sx + ( ey0 - eps2 ) * cx + eps2

! inclinaison
im = i0

! ascension droite du noeud
q   = 1.5_pm_reel * g(2) - 2.25_pm_reel * g(2) * g(2) * ( 2.5_pm_reel - (19._pm_reel / 6._pm_reel) * b(2) )
q   = q + 15._pm_reel / 16._pm_reel * g(4) * ( 7._pm_reel * b(2) - 4._pm_reel ) + 105._pm_reel / 32._pm_reel * g(6)              &
      * ( 2._pm_reel - 9._pm_reel * b(2) + 33._pm_reel / 4._pm_reel * b(4) )
omm = om0 + q * c * xnot
omm = omm - aint(omm / pm_deux_pi, kind=pm_reel) * pm_deux_pi

! argument de latitude
delta_l = 1._pm_reel - 1.5_pm_reel * g(2) * ( 3._pm_reel - 4._pm_reel * b(2) )
q   = delta_l + 2.25_pm_reel * g(2) * g(2) * ( 9._pm_reel - 263._pm_reel / 12._pm_reel * b(2) + 341._pm_reel / 24._pm_reel * b(4) )
q   = q + 15._pm_reel / 16._pm_reel * g(4) * ( 8._pm_reel - 31._pm_reel * b(2) + 24.5_pm_reel * b(4) )
q   = q + 105._pm_reel / 32._pm_reel * g(6) * ( -10._pm_reel / 3._pm_reel + 25._pm_reel * b(2) - 48.75_pm_reel                   &
      * b(4) + 27.5_pm_reel * b(6) )
lm  = l0 + q * xnot
lm  = lm - aint(lm/pm_deux_pi, kind=pm_reel) * pm_deux_pi

! termes periodiques
! ..................    

! calcul des cl(i)=cos(i*lm)  sl(i)=sin(i*lm)
cl(1) = cos(lm)
sl(1) = sin(lm)
do i = 2, 6
   j = i-1
   cl(i) = cl(j) * cl(1) - sl(j) * sl(1)
   sl(i) = cl(j) * sl(1) + sl(j) * cl(1)
end do

! autres quantites
qq = -1.5_pm_reel * g(2) / delta_l
qh =  3._pm_reel / 8._pm_reel * ( eym - eps2 ) / delta_pom
ql =  ((3._pm_reel / 8._pm_reel) * (exm / b(1))) / delta_pom

! sortie des elements moyens a la date t2
! .......................................
moy_t2%a     = am
moy_t2%ex    = exm
moy_t2%ey    = eym
moy_t2%i     = modulo(im,pm_deux_pi)
moy_t2%gom   = modulo(omm,pm_deux_pi)
moy_t2%pso_M = modulo(lm,pm_deux_pi)

! termes periodiques sur a
! ........................
! contribution de j2
f       = ( 2._pm_reel - 3.5_pm_reel * b(2) ) * exm * cl(1) + ( 2._pm_reel - 2.5_pm_reel * b(2) ) * eym * sl(1)
f       = f + b(2) * cl(2) + 3.5_pm_reel * b(2) * ( exm * cl(3) + eym * sl(3) )
delta_a = qq * f

! contribution j2 carre
q       = 0.75_pm_reel * g(2) * g(2) * b(2)
f       = 7._pm_reel * ( 2._pm_reel - 3._pm_reel * b(2) ) * cl(2) + b(2) * cl(4)
delta_a = delta_a + q * f

! contribution de j3
q       = -0.75_pm_reel * g(3) * b(1)
f       = ( 4._pm_reel - 5._pm_reel * b(2) ) * sl(1) + 5._pm_reel / 3._pm_reel * b(2) * sl(3)
delta_a = delta_a + q * f

! contribution de j4
q       = 0.25_pm_reel * g(4) * b(2)
f       = ( 15._pm_reel - 17.5_pm_reel * b(2) ) * cl(2) + 4.375_pm_reel * b(2) * cl(4)
delta_a = delta_a + q * f

! contribution de j5
q       = 3.75_pm_reel * g(5) * b(1)
f       = ( 2.625_pm_reel * b(4) - 3.5_pm_reel * b(2) + 1._pm_reel ) * sl(1)
f       = f + (7._pm_reel / 6._pm_reel) * b(2) * ( 1._pm_reel - 1.125_pm_reel * b(2) ) * sl(3) + (21._pm_reel &
          / 80._pm_reel) * b(4) * sl(5)
delta_a = delta_a + q * f

! contribution de j6
q       = 105._pm_reel / 16._pm_reel * g(6) * b(2)
f       = ( 3._pm_reel * b(2) - 1._pm_reel - 33._pm_reel / 16._pm_reel * b(4) ) * cl(2)
f       = f + 0.75_pm_reel * ( 1.1_pm_reel * b(4) - b(2) ) * cl(4) - 11._pm_reel / 80._pm_reel * b(4) * cl(6)
delta_a = delta_a + q * f

! termes periodiques sur ex
! .........................
! contribution de j2
f        = ( 1._pm_reel - 1.25_pm_reel * b(2) ) * cl(1) + 0.5_pm_reel * ( 3._pm_reel - 5._pm_reel * b(2) ) * exm * cl(2)
f        = f + ( 2._pm_reel - 1.5_pm_reel * b(2) ) * eym * sl(2) + 7._pm_reel / 12._pm_reel * b(2) * cl(3)
f        = f + 17._pm_reel / 8._pm_reel * b(2) * ( exm * cl(4) + eym * sl(4) )
delta_ex = qq * f

! termes periodiques sur ey
! .........................
! contribution j2
f        = ( 1._pm_reel - 1.75_pm_reel * b(2) ) * sl(1) + ( 1._pm_reel - 3._pm_reel * b(2) ) * exm * sl(2)
f        = f + ( 2._pm_reel * b(2) - 1.5_pm_reel ) * eym * cl(2) + 7._pm_reel / 12._pm_reel * b(2) * sl(3)
f        = f + 17._pm_reel / 8._pm_reel * b(2) * ( exm * sl(4) - eym * cl(4) )
delta_ey = qq * f

! termes periodiques sur om
! .........................
! contribution de j2
q         = -qq * c
f         = 3.5_pm_reel * exm * sl(1) - 2.5_pm_reel * eym * cl(1) - 0.5_pm_reel * sl(2)
f         = f + 7._pm_reel / 6._pm_reel * ( eym * cl(3) - exm * sl(3) )
delta_gom = q * f

! contribution de j3
f         = g(3) * c * ( 4._pm_reel - 15._pm_reel * b(2) )
delta_gom = delta_gom + ql * f

! contribution de j5
f         = 2.5_pm_reel * g(5) * c * ( 4._pm_reel - 42._pm_reel * b(2) + 52.5_pm_reel * b(4) )
delta_gom = delta_gom - ql * f

! termes periodiques sur xi
! .........................
! contribution de j2
q       = 0.5_pm_reel * qq * b(1) * c
f       = eym * sl(1) - exm * cl(1) + cl(2)
f       = f + 7._pm_reel / 3._pm_reel * ( exm * cl(3) + eym * sl(3) )
delta_i = q * f

! contribution de j3
f       = g(3) * c * ( 4._pm_reel - 5._pm_reel * b(2) )
delta_i = delta_i - qh * f

! contribution de j5
f       = 2.5_pm_reel * g(5) * c * ( 4._pm_reel - 14._pm_reel * b(2) + 10.5_pm_reel * b(4) )
delta_i = delta_i + qh * f

! termes periodiques sur xl
! .........................
! contribution de j2
f        = ( 7._pm_reel - 77._pm_reel / 8._pm_reel * b(2) ) * exm * sl(1) +                                                      &
           ( 55._pm_reel / 8._pm_reel * b(2) - 7.5_pm_reel ) * eym * cl(1)
f        = f + ( 1.25_pm_reel * b(2) - 0.5_pm_reel ) * sl(2)
f        = f + ( 77._pm_reel / 24._pm_reel * b(2) - 7._pm_reel / 6._pm_reel ) * ( exm * sl(3) - eym * cl(3) )
ddelta_l = qq * f

! contribution de j3
f        = g(3) * ( 53._pm_reel * b(2) - 4._pm_reel - 57.5_pm_reel * b(4) )
ddelta_l = ddelta_l + ql * f

! contribution de j5
f        = 2.5_pm_reel * g(5) * ( 4._pm_reel - 96._pm_reel * b(2) + 269.5_pm_reel * b(4) - 183.75_pm_reel * b(6) )
ddelta_l = ddelta_l + ql * f

! parametres osculateurs
! ......................
ex = exm + delta_ex
ey = eym + delta_ey
om = omm + delta_gom
xi = im + delta_i
xl = lm + ddelta_l

! sortie optionnelle des parametres osculateurs a la date t2
! ..........................................................
if (present(osc_t2)) then

   osc_t2%a     = am * ( 1._pm_reel  + delta_a )
   osc_t2%ex    = ex
   osc_t2%ey    = ey
   osc_t2%i     = modulo(xi,pm_deux_pi)
   osc_t2%gom   = modulo(om,pm_deux_pi)
   osc_t2%pso_M = modulo(xl,pm_deux_pi)

end if

code_retour%valeur = code_warning ! en cas de mode degrade (warning), l'utilisateur est prevenu par cette affectation

6000 continue

code_retour%routine = pm_num_me_eck_hech
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine me_eck_hech
