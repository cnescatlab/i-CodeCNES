subroutine me_brouwer (mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2)

! (C) Copyright CNES - MSLIB - 1998-2004

!************************************************************************
!
! But:  Modele analytique d'extrapolation d'orbite de BROUWER
! ===   
!
! Note d'utilisation:  L'excentricite  doit etre superieure >= 1E-4 et < 0.9.
! ==================   Le domaine d'erreur est ................. [ 0E+0 , 1E-4 [ et >= 9E-1.
!                      Le domaine de precision degradee est .... [ 1E-4 , 1E-2 [.
!                      Le domaine d'utilisation du modele est .. [ 1E-2 , 9E-1 [.
!
!                      L'inclinaison  doit etre > 0. et < pi et non proches des inclinaisons critiques
!                      ( pm_i_critique_non_retro, pm_i_critique_retro ) a pm_eps_i_critique pres.
!                      De plus si 0 < i < 0.018 radian ( environ 1 degre ) alors on est egalement en mode degradee.
!                      
!
!$Historique
! ==========
!   + Version 1.0 (SP 207 ed01 rev00): creation a partir de la routine MEBRJ5 de la MSLIB f77
!                         (Date: 06/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 351 ed01 rev00): revision du test sur l'inclinaison critique - ajout de commentaire
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.0 (DE globale 618 ed01 rev00): ajout de tests sur les coefficients CN0
!                         (Date: 02/2004 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules math_mslib et phys_mslib par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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
use int_constantes, only : pm_i_critique_non_retro,pm_i_critique_retro

use test_mslib
use int_chgmnt_variables, only : mv_kepler_std
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

real(pm_reel), intent(in)                :: mu          !mu                                               
real(pm_reel), intent(in)                :: r_equa      !rayon equatorial
real(pm_reel), intent(in), dimension(2:5):: cn0         !coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_jour_sec), intent(in)            :: t1          !date t1
type(tm_orb_kep), intent(in)             :: moy_t1      !parametres moyens a la date t1
type(tm_jour_sec), intent(in)            :: t2          !date t2

type(tm_orb_kep), intent(out)            :: moy_t2      !parametres moyens a la date t2   
type(tm_code_retour), intent(out)        :: code_retour !code retour

type(tm_orb_kep), intent(out), optional  :: osc_t2      ! parametres osculateurs a la date t2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: ak2,ak3,ak4,ak5!     constantes liees aux termes zonaux du potentiel terrestre

!constantes liees au 1/2 grand axe et aux termes zonaux
real(pm_reel) :: g2,g3,g4,g5,tt1,tt2,t3,t4,t5,h2,h3,h4,h5,an,ant,c1,c2,c3,c4,c5,c6,rdc,s1 

real(pm_reel) :: ex1,h21,h22,h41,am1,po1,go1,c1p,c2p,c3p,s1p,s2p,s3p !     termes seculaires

!     termes a longue periode
real(pm_reel) :: e1,e2,e3,p1,p2,p3,p4,q0,q1,q2,q3,a,b,c,rdex,rdai,ex2,ai2,am2,po2,go2,r1,r2,r3,r4
!     termes a courte periode en j2
real(pm_reel) :: e,x,y,v,sv,a1,a2,a3,rda,rde1,rde2,rdm1,rdm2,rdp,c2p1v,s2p1v,c2p2v,s2p2v,c2p3v,s2p3v

real(pm_reel) :: eps100!     variable  epsilon machine * 100

real(pm_reel),parameter :: e_borne_sup_err  = 1.e-4_pm_reel ! bornes superieure domaine d'erreur
real(pm_reel),parameter :: e_borne_sup_degr = 1.e-2_pm_reel ! bornes superieure domaine de precision degradee
real(pm_reel),parameter :: e_borne_sup_util = 0.9_pm_reel   ! bornes superieure domaine d'utilisation du modele
real(pm_reel),parameter :: deg1=0.018_pm_reel               ! 1*degre = 0.018 rad

real(pm_reel) :: M,pom,gom  ! valeurs intermediaires

real(pm_reel) :: jour1, jour2 

integer       :: i          ! indice de boucle de controle

type(tm_code_retour)  :: code_retour_bis  ! code retour intermediaire des routines appelees (pour ne pas ecraser
! un eventuel code retour positif anterieur) 

intrinsic epsilon, sqrt, cos, sin, modulo, real, abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB me_brouwer.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: me_brouwer.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

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
do i = 2,5
   if (abs(cn0(i)) <= eps100) then
      code_retour%valeur = pm_err_cn0_nul  ! coefficient zonal nul
      go to 6000
   end if
end do

! demi grand axe

if (moy_t1%a <= eps100) then
   if (moy_t1%a < 0._pm_reel) then     ! demi-grand axe < 0.
      code_retour%valeur= pm_err_a_negatif
      go to 6000
   else                        ! demi-grand axe = 0.
      code_retour%valeur= pm_err_a_nul
      go to 6000
   end if
end if

! excentricite

if (moy_t1%e < 0._pm_reel) then
   code_retour%valeur= pm_err_e_negatif            ! excentricite negative
   go to 6000
end if

if (moy_t1%e < e_borne_sup_err) then
   code_retour%valeur = pm_err_e_faible_brouwer    ! excentricite dans le domaine d'erreur 
   go to 6000
end if
if ((moy_t1%e >= e_borne_sup_err)                                                                              &
     .and. (moy_t1%e < e_borne_sup_degr)) then
   code_retour%valeur = pm_warn_e_faible_brouwer   ! excentricite dans le domaine de precision degradee 
end if
if (moy_t1%e > e_borne_sup_util) then
   code_retour%valeur = pm_err_e_grand_brouwer     ! excentricite trop forte > e_borne_sup_util 
   go to 6000
end if

! inclinaison

if (moy_t1%i < 0._pm_reel) then             !     inclinaison negative
   code_retour%valeur= pm_err_i_negatif
   go to 6000 
else if (moy_t1%i > pm_pi) then             !     inclinaison > pi
   code_retour%valeur= pm_err_i_sup_pi
   go to 6000
else if (sin(moy_t1%i) <= pm_eps_equa) then !     orbite equatoriale
   code_retour%valeur= pm_err_i_equa
   go to 6000
end if

if (moy_t1%i < deg1) code_retour%valeur = pm_warn_i_faible_brouwer   ! si inclinaison < 1 degre = 0.018 rad  
! zone mode degrade

if ((moy_t1%i > (pm_i_critique_non_retro-pm_eps_i_critique)).and. &  ! inclinaison proche de pm_i_critique_non_retro
     (moy_t1%i < (pm_i_critique_non_retro+pm_eps_i_critique))) then  ! ( a pm_eps_i_critique radians pres )
   code_retour%valeur = pm_err_i_critique
   go to 6000
else if ((moy_t1%i > (pm_i_critique_retro-pm_eps_i_critique)).and. & ! inclinaison proche de pm_i_critique_retro
     (moy_t1%i < (pm_i_critique_retro+pm_eps_i_critique))) then ! ( a pm_eps_i_critique radians pres )
   code_retour%valeur = pm_err_i_critique
   go to 6000
end if

ak2=-cn0(2)*(r_equa**2)/2._pm_reel
ak3=cn0(3)*(r_equa**3)
ak4=cn0(4)*(r_equa**4)*(3._pm_reel/8._pm_reel)
ak5=cn0(5)*(r_equa**5)

! constantes liees a 1/2 grand-axe,excentricite et inclinaison
! ============================================================

g2=ak2/(moy_t1%a**2)
g3=ak3/(moy_t1%a**3)
g4=ak4/(moy_t1%a**4)
g5=ak5/(moy_t1%a**5)

tt1=sqrt(1._pm_reel-moy_t1%e*moy_t1%e)  ! calcul de tt1 = sqrt(1.- e2) avec e < 0.9
tt2=tt1*tt1
t3=tt2*tt1
t4=t3*tt1
t5=t4*tt1

h2=g2/tt2**2
h3=g3/t3**2
h4=g4/t4**2
h5=g5/t5**2

an=86400._pm_reel*sqrt(mu/moy_t1%a**3)
jour1= real(t1%jour, pm_reel) + t1%sec/86400._pm_reel
jour2= real(t2%jour, pm_reel) + t2%sec/86400._pm_reel
ant=(jour2-jour1)*an

c1=cos(moy_t1%i)   ! calcul de cos(inclinaison)
c2=c1*c1
c3=c2*c1
c4=c3*c1
c5=c4*c1
c6=c5*c1
rdc=1._pm_reel-5._pm_reel*c2   ! rdc = 1 - 5 cos(i)**2 = -4 + 5 sin(i)**2 (rdc = 0 definit l'inclinaison critique)

s1=sin(moy_t1%i)               ! calcul de sin(inclinaison)

! calcul des termes seculaires en j2,j4
! =====================================

ex1=moy_t1%e
h21=-rdc
h22=-35._pm_reel+24._pm_reel*tt1+25._pm_reel*tt2
h22=h22+(90._pm_reel-192._pm_reel*tt1-126._pm_reel*tt2)*c2
h22=h22+(385._pm_reel+360._pm_reel*tt1+45._pm_reel*tt2)*c4
h41=21._pm_reel-9._pm_reel*tt2
h41=h41+(-270._pm_reel+126._pm_reel*tt2)*c2
h41=h41+(385._pm_reel-189._pm_reel*tt2)*c4
po1=moy_t1%pom+ant*((3._pm_reel/2._pm_reel)*h2*h21+(3._pm_reel/32._pm_reel)*(h2*h2)*h22+(5._pm_reel/16._pm_reel)*h4*h41)

h21=-c1
h22=(-5._pm_reel+12._pm_reel*tt1+9._pm_reel*tt2)*c1+(-35._pm_reel-36._pm_reel*tt1-5._pm_reel*tt2)*c3
h41=(5._pm_reel-3._pm_reel*tt2)*c1*(3._pm_reel-7._pm_reel*c2)
go1=moy_t1%gom+ant*(3._pm_reel*h2*h21+(3._pm_reel/8._pm_reel)*(h2*h2)*h22+(5._pm_reel/4._pm_reel)*h4*h41)

h21=-1._pm_reel+3._pm_reel*c2
h22=-15._pm_reel+16._pm_reel*tt1+25._pm_reel*tt2
h22=h22+(30._pm_reel-96._pm_reel*tt1-90._pm_reel*tt2)*c2
h22=h22+(105._pm_reel+144._pm_reel*tt1+25._pm_reel*tt2)*c4
h41=(3._pm_reel-30._pm_reel*c2+35._pm_reel*c4)
am1=moy_t1%M+ant*(1._pm_reel+(3._pm_reel/2._pm_reel)*h2*tt1*h21+(3._pm_reel/32._pm_reel)*(h2*h2)*tt1*h22 &
     +(15._pm_reel/16._pm_reel)*h4*tt1*(moy_t1%e*moy_t1%e)*h41)
am1=modulo(am1,pm_deux_pi)

c1p=cos(po1)
c2p=cos(2._pm_reel*po1)
c3p=cos(3._pm_reel*po1)

s1p=sin(po1)
s2p=sin(2._pm_reel*po1)
s3p=sin(3._pm_reel*po1)

e1=moy_t1%e
e2=e1*e1
e3=e2*e1

! calcul des termes a longues periodes en j2,j3,j4,j5
! ===================================================

p1=1._pm_reel-11._pm_reel*c2-40._pm_reel*c4/rdc
p2=1._pm_reel-03._pm_reel*c2-08._pm_reel*c4/rdc
p3=1._pm_reel-09._pm_reel*c2-24._pm_reel*c4/rdc
p4=1._pm_reel-05._pm_reel*c2-16._pm_reel*c4/rdc

q0=16._pm_reel*(c2/rdc)+40._pm_reel*(c4/rdc**2)
q1=03._pm_reel+1._pm_reel*q0
q2=05._pm_reel+2._pm_reel*q0
q3=11._pm_reel+5._pm_reel*q0

a=(1._pm_reel/8._pm_reel)*h2*e1*tt2*p1-(5._pm_reel/12._pm_reel)*(h4/h2)*e1*tt2*p2
b=(1._pm_reel/4._pm_reel)*(h3/h2)*tt2*s1+(5._pm_reel/64._pm_reel)*(h5/h2)*(s1/tt2)*(4._pm_reel+3._pm_reel*e2)*p3
c=(35._pm_reel/384._pm_reel)*(h5/h2)*e2*tt2*s1*p4
rdex=a*c2p+b*s1p-c*s3p
rdai=-(rdex*e1*c1)/(tt2*s1)

ex2=ex1+rdex
ai2=moy_t1%i+rdai
a=a*(tt1/e1)
c=c*(tt1/e1)
b=(t3/e1)*s1*((1._pm_reel/4._pm_reel)*(h3/h2)+(5._pm_reel/64._pm_reel)*(h5/h2)*(4._pm_reel+9._pm_reel*e2)*p3)
am2=am1+a*s2p-b*c1p+c*c3p
am2=modulo(am2,pm_deux_pi)

a=-(1._pm_reel/8._pm_reel)*h2*e2*c1*q3+(5._pm_reel/12._pm_reel)*(h4/h2)*e2*c1*q1
b=e1*(c1/h2)*((1._pm_reel/4._pm_reel)*(h3/s1)+h5*(4._pm_reel+3._pm_reel*e2)*((5._pm_reel/64._pm_reel)*(p3/s1)  &
     +(15._pm_reel/32._pm_reel)*q1*s1))
c=(35._pm_reel/576._pm_reel)*(h5/h2)*e3*c1*(0.5_pm_reel*(p4/s1)+q2*s1)
go2=go1+a*s2p+b*c1p-c*c3p

r1=(2._pm_reel+1._pm_reel*e2)
r2=(2._pm_reel+3._pm_reel*e2)*c2
r3=(2._pm_reel+5._pm_reel*e2)*c4/rdc
r4=e2*c6/rdc**2
a=-(1._pm_reel/16._pm_reel)*h2*(r1-11._pm_reel*r2-40._pm_reel*r3-400._pm_reel*r4)
a=a+(5._pm_reel/24._pm_reel)*h4*(r1-03._pm_reel*r2-08._pm_reel*r3-80._pm_reel*r4)/h2
r1=(s1/e1)-c2*(e1/s1)
r2=(s1/e1)*tt2-(e1/s1)*c2
r2=(r2*(4._pm_reel+3._pm_reel*e2)+e1*s1*(26._pm_reel+9._pm_reel*e2))*p3
r3=e1*c2*s1*(4._pm_reel+3._pm_reel*e2)*q1
b=((1._pm_reel/4._pm_reel)*h3*r1+(5._pm_reel/64._pm_reel)*h5*r2-(15._pm_reel/32._pm_reel)* h5*r3)/h2
r1=(e1*s1*(3._pm_reel+2._pm_reel*e2)-c2*(e3/s1))*p4
r2=e3*c2*s1*q2
c=(35._pm_reel/576._pm_reel)*(h5/h2)*(-0.5_pm_reel*r1+r2)
po2=po1+a*s2p+b*c1p+c*c3p

! calcul des termes a courtes periodes en j2
! ==========================================

call mv_kepler_std(am2,ex1,E,code_retour_bis)
if (code_retour_bis%valeur < 0) then   ! seul pm_err_conv_kepler_ellip possible ici
   code_retour%valeur = code_retour_bis%valeur
   go to 6000
end if

x=cos(E)-ex1   ! (cos(E) - e) et (tt1 sin(E)) ne peuvent pas etre nuls en meme temps :
!  cos(E)-e = 0 => cos(E) = e
y=tt1*sin(E)    !  sqrt(1-e2)* sin(E) = 0 => sqrt(1-cos(E)**2) * sin(E) = |sin(E)|*sin(E) = 0
! |sin(E)|*sin(E) = 0 => |cos(E)| = 1 => e = 1  or par definition  0<e<0.9

call mu_angle2(x,y,v,code_retour_bis)
if (code_retour_bis%valeur == pm_err_vect_nul) then
   code_retour%valeur = pm_err_cni
   go to 6000
end if

sv=sin(v)
a1=1._pm_reel/(1._pm_reel-ex1*cos(E))
a2=a1*a1

a3=a2*a1
c2p1v=cos(2._pm_reel*po2+1._pm_reel*v)
s2p1v=sin(2._pm_reel*po2+1._pm_reel*v)
c2p2v=cos(2._pm_reel*po2+2._pm_reel*v)
s2p2v=sin(2._pm_reel*po2+2._pm_reel*v)
c2p3v=cos(2._pm_reel*po2+3._pm_reel*v)
s2p3v=sin(2._pm_reel*po2+3._pm_reel*v)

rda=(-1._pm_reel+3._pm_reel*c2)*(a3-1._pm_reel/t3)+3._pm_reel*(1._pm_reel-c2)*a3*c2p2v

! Sortie des elements moyens a la date t2
! ===============================================
moy_t2%a = moy_t1%a
moy_t2%e = moy_t1%e
moy_t2%i = moy_t1%i
!seuls pom, gom et M sont modifies
moy_t2%pom = po1
moy_t2%gom = go1
moy_t2%M   = am1

! sortie optionnelle des parametres osculateurs a la date t2
! ==================================================================

if (present(osc_t2)) then

   osc_t2%a=moy_t1%a+moy_t1%a*g2*rda

   rde1=rda-(3._pm_reel/t4)*(1._pm_reel-c2)*c2p2v
   rde2=3._pm_reel*ex1*c2p1v+ex1*c2p3v
   osc_t2%e=ex2+(tt2/(2._pm_reel*ex1))*(g2*rde1-h2*(1._pm_reel-c2)*rde2)

   osc_t2%i=ai2+(h2*c1*s1/2._pm_reel)*(3._pm_reel*c2p2v+rde2)

   a=a2*tt2+a1+1._pm_reel
   b=-a+2._pm_reel
   c=a-2._pm_reel/3._pm_reel
   rdm1=2._pm_reel*(-1._pm_reel+3._pm_reel*c2)*a*sv+3._pm_reel*(1._pm_reel-c2)*(b*s2p1v+c*s2p3v)
   rdm2=(t3*h2/(4._pm_reel*ex1))*rdm1
   M=am2-rdm2
   osc_t2%M=modulo(M,pm_deux_pi)

   x=v-am2+ex1*sv
   y=3._pm_reel*s2p2v+3._pm_reel*ex1*s2p1v+ex1*s2p3v
   rdp=(tt2*h2/(4._pm_reel*ex1))*rdm1+(h2/4._pm_reel)*(6._pm_reel*(-1._pm_reel+5._pm_reel*c2)*x+(3._pm_reel-5._pm_reel*c2)*y)
   pom=po2+rdp
   gom=go2-(h2*c1/2._pm_reel)*(6._pm_reel*x-y)
   osc_t2%pom=modulo(pom,pm_deux_pi)
   osc_t2%gom=modulo(gom,pm_deux_pi)

end if

6000 continue

code_retour%routine = pm_num_me_brouwer
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine me_brouwer
