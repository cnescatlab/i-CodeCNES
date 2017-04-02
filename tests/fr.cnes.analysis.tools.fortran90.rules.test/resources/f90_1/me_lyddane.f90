subroutine me_lyddane (mu, r_equa, cn0, t1, moy_t1, t2, moy_t2, code_retour, osc_t2)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  modele analytique d'extrapolation d'orbite de Lyddane
! ===
!
! Note d'utilisation: les unites de distance sont en metres OBLIGATOIREMENT 
! ==================  (pour mu, r_equa, et a)
!
!                     l'excentricite doit appartenir a [ 0. , 0.9 [
!                     le domaine d'erreur est          < 0. et >= 0.9
!
!                     l'inclinaison  doit appartenir a [ 0. , pi ]
!                     et non proches des inclinaisons critiques definies par
!                     (pm_i_critique_non_retro, pm_i_critique_retro) a pm_eps_i_critique pres.
!
!$Historique
! ==========
!   + Version 6.0 (SP 619 ed01 rev00): creation a partir de la routine LYDDAN du CNES utilisee en geostationnaire
!                         (Date: 02/2004 - Realisation: Veronique Lepine et Bruno Revelin)
!   + Version 6.3 (DM-ID 394) : Qualite : augmentation du taux de commentaires
!                 (Date: 11/2005 - Realisation: Atos Origin)   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 remplacement des modules math_mslib et phys_mslib par 
!     une sélection de int_constantes
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use int_constantes, only : pm_i_critique_non_retro,pm_i_critique_retro


use test_mslib
use int_chgmnt_variables, only : mv_kepler_std
use int_utilitaires, only : mu_angle2
use int_chgmnt_variables, only : mv_kep_cir_equa
use int_chgmnt_variables, only : mv_cir_equa_kep

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                :: mu          ! constante de la gravitation
real(pm_reel), intent(in)                :: r_equa      ! rayon equatorial terrestre
real(pm_reel), dimension(2:5), intent(in):: cn0         ! coefficients harmoniques zonaux C20 a C50 denormalises
type(tm_jour_sec), intent(in)            :: t1          ! date t1
type(tm_orb_cir_equa), intent(in)        :: moy_t1      ! parametres moyens a la date t1
type(tm_jour_sec), intent(in)            :: t2          ! date t2
type(tm_orb_cir_equa), intent(out)       :: moy_t2      ! parametres moyens a la date t2
type(tm_code_retour), intent(out)        :: code_retour

type(tm_orb_cir_equa), intent(out), optional  :: osc_t2 ! parametres osculateurs a la date t2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)    :: tt, tb, rte, xmu                                                      ! dates et var ramenees en km
real(pm_reel)    :: ak2, ak3, ak4, ak5, g2, g3, g4, g5, e2, tt1, tt2, t3, t4, t5, t6      ! coef interm de calcul 
real(pm_reel)    :: h2, h3, h4, h5, an, ant, c1, c2, c3, c4, c5, c6, dc, s1, s2           ! coef interm de calcul
real(pm_reel)    :: es1, ec1, cc1, r1, r2, r4, h21, h22, h41, po1, go1, am1, cam1, sam1   ! coef interm de calcul
real(pm_reel)    :: cdai, sdai, cgom1, sgom1, c1p, c2p, c3p, s1p, s2p, s3p, cc            ! coef interm de calcul
real(pm_reel)    :: xp1, xp2, xp4, p1, p2, p3, p4, q0, q1, q2, q3, a, b, c                ! coef interm de calcul
real(pm_reel)    :: ac, bs, cs, dai2, dex2, edm2, sidg2, cp, som1, som2, som3, som4, som5 ! coef interm de calcul
real(pm_reel)    :: pgm2, am2, ecc2, po2, ede2, x, y, aux, auxi, gom2                     ! coef interm de calcul
real(pm_reel)    :: e, v, sv, cv, cv2, xc, xa, xb, pc2, pc21, pc22, pc23, c2p1v, s2p1v    ! coef interm de calcul
real(pm_reel)    :: c2p2v, s2p2v, c2p3v, s2p3v, da, db, dax3, de1, de2, dex3, dai3 ! coef interm de calcul
real(pm_reel)    :: a1, a2, d1, d21, d22, d2, dpgm3, edm3, sidg3                          ! coef interm de calcul
real(pm_reel)    :: ax3, dex, dai, edm, sidg, pgm, ede, qex, qey         ! coef interm de calcul
real(pm_reel)    :: ex3, am3, sidi, qix, qiy, si3, ai3,go3, po3                           ! coef interm de calcul

type(tm_orb_kep)       :: moy_kep_t1, moy_kep_t2, osc_kep_t2 ! parametres kepleriens intermediaires
type(tm_orb_cir_equa)  :: osc_t2_temp                        ! parametres osculateurs temporaires a la date t2

real(pm_reel), parameter :: e_borne_sup_util = 0.9_pm_reel   ! bornes superieure du domaine d'utilisation du modele

real(pm_reel), parameter :: excel = 0.05_pm_reel               ! constante de test pour l'excentricite

real(pm_reel) :: eps100 ! epsilon de calcul
integer       :: i      ! indice de boucle
type(tm_code_retour)          :: code_retour_local

intrinsic sqrt, sin, cos, asin, modulo, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB me_lyddane.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: me_lyddane.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! initialisation de la valeur epsilon * 100
eps100 = 100._pm_reel * epsilon(1._pm_reel)

! autres initialisations : passage en kepleriens a cause du modele
! ----------------------------------------------------------------
call mv_cir_equa_kep(moy_t1, moy_kep_t1, code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

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
if (moy_kep_t1%a <= eps100) then
   if (moy_kep_t1%a < 0._pm_reel) then     ! demi-grand axe < 0.
      code_retour%valeur= pm_err_a_negatif
      go to 6000
   else                                    ! demi-grand axe = 0.
      code_retour%valeur= pm_err_a_nul
      go to 6000
   end if
end if

! excentricite
if (moy_kep_t1%e < 0._pm_reel) then
   code_retour%valeur= pm_err_e_negatif        ! excentricite negative
   go to 6000                                 
end if                                        

if (moy_kep_t1%e > e_borne_sup_util) then     
   code_retour%valeur = pm_err_e_grand_brouwer ! excentricite trop forte > e_borne_sup_util 
   go to 6000
end if

! inclinaison
if (moy_kep_t1%i < 0._pm_reel) then             !     inclinaison negative
   code_retour%valeur= pm_err_i_negatif
   go to 6000 
else if (moy_kep_t1%i > pm_pi) then             !     inclinaison > pi
   code_retour%valeur= pm_err_i_sup_pi
   go to 6000
end if

! inclinaison proche de pm_i_critique_non_retro ou pm_i_critique_retro
if ((abs(moy_kep_t1%i - pm_i_critique_non_retro) < pm_eps_i_critique).or.&
     (abs(moy_kep_t1%i - pm_i_critique_retro) < pm_eps_i_critique)) then
   code_retour%valeur = pm_err_i_critique
   go to 6000
end if

! Passage aux unites utilisees par l'algorithme : km
! --------------------------------------------------
rte = r_equa / 1000._pm_reel
moy_kep_t1%a = moy_kep_t1%a / 1000._pm_reel
xmu = mu * 1.e-9_pm_reel

! DEBUT DU MODELE
! ===============

ak2=-cn0(2)*(rte**2)/2._pm_reel
ak3=cn0(3)*(rte**3)
ak4=cn0(4)*(rte**4)*(3._pm_reel/8._pm_reel)
ak5=cn0(5)*(rte**5)

! constantes liees a 1/2 grand-axe, excentricite et inclinaison
! ============================================================

g2=ak2/(moy_kep_t1%a**2)
g3=ak3/(moy_kep_t1%a**3)
g4=ak4/(moy_kep_t1%a**4)
g5=ak5/(moy_kep_t1%a**5)

e2=moy_kep_t1%e*moy_kep_t1%e
tt1=sqrt(1._pm_reel-e2)
tt2=tt1*tt1
t3=tt2*tt1
t4=t3*tt1
t5=t4*tt1
t6=t5*tt1

h2=g2/tt2**2
h3=g3/t3**2
h4=g4/t4**2
h5=g5/t5**2

an=86400._pm_reel*sqrt(xmu/moy_kep_t1%a)/moy_kep_t1%a
tt=real(t2%jour, kind=pm_reel)+t2%sec/86400._pm_reel
tb=real(t1%jour, kind=pm_reel)+t1%sec/86400._pm_reel
ant=(tt-tb)*an

c1=cos(moy_kep_t1%i)
c2=c1*c1
c3=c2*c1
c4=c3*c1
c5=c4*c1
c6=c5*c1
dc=1._pm_reel-5._pm_reel*c2

s1=sin(moy_kep_t1%i)
s2=s1*s1

es1=moy_kep_t1%e*s1
ec1=moy_kep_t1%e*c1
cc1=c1*(1._pm_reel-c1)

r1=4._pm_reel+3._pm_reel*e2
r2=4._pm_reel+9._pm_reel*e2
r4=e2*c6/dc**2

! calcul des termes seculaires en J2,J2**2,J4
! ===========================================

h21=-dc
h22=-35._pm_reel+24._pm_reel*tt1+25._pm_reel*tt2
h22=h22+(90._pm_reel-192._pm_reel*tt1-126._pm_reel*tt2)*c2
h22=h22+(385._pm_reel+360._pm_reel*tt1+45._pm_reel*tt2)*c4
h41=21._pm_reel-9._pm_reel*tt2
h41=h41+(-270._pm_reel+126._pm_reel*tt2)*c2
h41=h41+(385._pm_reel-189._pm_reel*tt2)*c4
po1= 1.5_pm_reel*h2*( h21 + h2*h22/16._pm_reel )
po1= moy_kep_t1%pom + ant*( po1 + (5._pm_reel/16._pm_reel)*h4*h41 ) ! determination argument du perigee po1

h21=-c1
h22=(-5._pm_reel+12._pm_reel*tt1+9._pm_reel*tt2)*c1+(-35._pm_reel-36._pm_reel*tt1-5._pm_reel*tt2)*c3
h41=(5._pm_reel-3._pm_reel*tt2)*c1*(3._pm_reel-7._pm_reel*c2)
go1= 3._pm_reel*h2*(h21 + h2*h22/8._pm_reel )
go1= moy_kep_t1%gom + ant*( go1 + 5._pm_reel*h4*h41/4._pm_reel )    ! determination ascension droite du noeud go1

h21=-1._pm_reel+3._pm_reel*c2
h22=-15._pm_reel+16._pm_reel*tt1+25._pm_reel*tt2
h22=h22+(30._pm_reel-96._pm_reel*tt1-90._pm_reel*tt2)*c2
h22=h22+(105._pm_reel+144._pm_reel*tt1+25._pm_reel*tt2)*c4
h41=(3._pm_reel-30._pm_reel*c2+35._pm_reel*c4)
am1= 1.5_pm_reel*h2*( tt1*h21 + h2*tt1*h22/16._pm_reel )
am1= am1 + 15._pm_reel*h4*tt1*e2*h41/16._pm_reel
am1= moy_kep_t1%M + ant*( 1._pm_reel + am1 )
am1=modulo(am1,pm_deux_pi)                                           ! determination anomalie moyenne am1

! FIN DU MODELE POUR LES MOYENS
!==============================

! Sortie des elements moyens a la date t2
! =======================================

moy_kep_t2%a = moy_kep_t1%a * 1000._pm_reel   ! passage en metres
moy_kep_t2%e = moy_kep_t1%e
moy_kep_t2%i = moy_kep_t1%i
! seuls pom, gom et M sont modifies
moy_kep_t2%pom = po1
moy_kep_t2%gom = go1
moy_kep_t2%M   = am1

! Passage en parametres circulaires equatoriaux

call mv_kep_cir_equa (moy_kep_t2, moy_t2, code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! MODELE POUR LES OSCULATEURS
!============================

if (present(osc_t2)) then

   cam1=cos(am1)
   sam1=sin(am1)
   cdai=cos(0.5_pm_reel*moy_kep_t1%i)
   sdai=sin(0.5_pm_reel*moy_kep_t1%i)
   cgom1=cos(go1)
   sgom1=sin(go1)

   !  calcul des termes a longues periodes en J2,J3,J4,J5
   !  ===================================================
   c1p=cos(po1)
   c2p=cos(2._pm_reel*po1)
   c3p=cos(3._pm_reel*po1)

   s1p=sin(po1)
   s2p=sin(2._pm_reel*po1)
   s3p=sin(3._pm_reel*po1)

   cc=2._pm_reel*c2/dc
   xp1=(1._pm_reel- 5._pm_reel*cc)*s1
   xp2=(1._pm_reel- cc)*s1
   xp4=(1._pm_reel- 2._pm_reel*cc)*s1

   p1=xp1*s1
   p2=xp2*s1
   p4=xp4*s1
   p3=3._pm_reel*p2 - 2._pm_reel

   q0=16._pm_reel*(c2/dc)+40._pm_reel*(c4/dc**2)
   q1=03._pm_reel+q0
   q2=05._pm_reel+2._pm_reel*q0
   q3=11._pm_reel+5._pm_reel*q0

   a=0.5_pm_reel*xp1*h2 - 5._pm_reel*xp2*h4/(3._pm_reel*h2)
   b=(5._pm_reel/16._pm_reel)*r1*p3/tt2
   b=( tt2*h3 + b*h5 )/h2
   c=(35._pm_reel/32._pm_reel)*h5*p4/(3._pm_reel*h2)

   ac=a*c2p
   bs=b*s1p
   cs=c*s3p

   dai2=moy_kep_t1%e*( -ac + moy_kep_t1%e*cs )
   dex2=0.25_pm_reel*( -tt2*dai2 + bs )*s1
   dai2=0.25_pm_reel*( dai2 - bs/tt2 )*ec1

   edm2= - h3 - (5._pm_reel/16._pm_reel)*r2*h5*p3
   edm2= edm2*c1p/h2 + moy_kep_t1%e*a*s2p
   edm2= edm2 + e2*c*c3p
   edm2= 0.25_pm_reel*edm2*t3*s1

   a= -0.5_pm_reel*q3*h2 + 5._pm_reel*q1*h4/(3._pm_reel*h2)
   a= es1*a*s2p
   b= (5._pm_reel/8._pm_reel)*r1*( 0.5_pm_reel*p3 + 3._pm_reel*s2*q1 )
   b= ( h3 + b*h5 )*c1p/h2
   c= (35._pm_reel/16._pm_reel)*( 0.5_pm_reel*p4 + s2*q2 )*h5/(9._pm_reel*h2)
   c= e2*c*c3p

   sidg2= 0.25_pm_reel*ec1*(a+b-c)

   a= c1/(1._pm_reel+c1)
   b= tt1/(1._pm_reel+tt1)
   c= tt1 + 1._pm_reel/(1._pm_reel+tt1)

   cp=c+2.5_pm_reel
   som1= -cp*p1 - c1*q3 - 11._pm_reel*c2
   som1= (som1 + 2._pm_reel)*e2 + 200._pm_reel*r4
   som2= cp*p2 + c1*q1 + 3._pm_reel*c2
   som2= (som2 -2._pm_reel)*e2 - 40._pm_reel*r4
   som3= (a+c)*es1
   som4= (a + tt1*b)*r1
   som4= som4 + e2*(9._pm_reel + 6._pm_reel*c) +20._pm_reel
   som4= som4*p3 + 6._pm_reel*r1*cc1*q1
   som4= es1*som4
   som5= -0.5_pm_reel*p4*(a + 3._pm_reel*c + 2._pm_reel)
   som5= (som5 - cc1*q2)*es1*e2

   pgm2= 0.125_pm_reel*( h2*som1 + 10._pm_reel*h4*som2/(3._pm_reel*h2) )*s2p
   pgm2= pgm2 + ( 0.25_pm_reel*h3*som3 + (5._pm_reel/64._pm_reel)*h5*som4 )*c1p/h2
   pgm2= pgm2 + (35._pm_reel/64._pm_reel)*h5*som5*c3p/(9._pm_reel*h2)
   pgm2= pgm2 + (po1+go1+am1)

   ! calcul des termes a courtes periodes en J2
   ! ==========================================

   am2=am1
   ecc2=moy_kep_t1%e
   po2=po1

   if(moy_kep_t1%e > excel) then

      ede2=moy_kep_t1%e + dex2
      x=ede2*cam1 - edm2*sam1
      y=ede2*sam1 + edm2*cam1

      call mu_angle2(x,y,am2,code_retour_local)
      if (code_retour_local%valeur == pm_err_vect_nul) then
         code_retour%valeur = pm_err_cni
         go to 6000
      end if

      ecc2=sqrt(x*x + y*y)
      aux=sdai + 0.5_pm_reel*dai2*cdai
      auxi=0.5_pm_reel*sidg2/cdai
      x=aux*cgom1 - auxi*sgom1
      y=aux*sgom1 + auxi*cgom1

      call mu_angle2(x,y,gom2,code_retour_local)
      if (code_retour_local%valeur == pm_err_vect_nul) then
         code_retour%valeur = pm_err_cni
         go to 6000
      end if

      po2=pgm2 - gom2 - am2

   end if

! Resolution de l'equation de Kepler, orbite elliptique

   call mv_kepler_std(am2,ecc2,e,code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then   ! seul pm_err_conv_kepler_ellip possible ici
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

   x=cos(e) - ecc2
   y=sqrt(1._pm_reel - ecc2*ecc2)*sin(e)

   call mu_angle2(x,y,v,code_retour_local)
   if (code_retour_local%valeur == pm_err_vect_nul) then
      code_retour%valeur = pm_err_cni
      go to 6000
   end if

   sv=sin(v)
   cv=cos(v)
   cv2=cv*cv
   xc=( 3._pm_reel*(1._pm_reel+moy_kep_t1%e*cv) + e2*cv2 )*cv
   xa=xc+moy_kep_t1%e*c
   xb=xc+moy_kep_t1%e

   pc2=2._pm_reel*po2
   pc21=pc2+v
   pc22=pc2+2._pm_reel*v
   pc23=pc2+3._pm_reel*v

   c2p1v=cos(pc21)
   s2p1v=sin(pc21)
   c2p2v=cos(pc22)
   s2p2v=sin(pc22)
   c2p3v=cos(pc23)
   s2p3v=sin(pc23)

   da=(-1._pm_reel+3._pm_reel*c2)*xa
   db=3._pm_reel*s2*c2p2v

   dax3= moy_kep_t1%e*da + (1._pm_reel + moy_kep_t1%e*xc)*db
   dax3= moy_kep_t1%a*dax3*g2/t6

   de1= (da + db*xb)/t6
   de2= c2p3v + 3._pm_reel*c2p1v

   dex3= 0.5_pm_reel*tt2*(de1*g2 - de2*s2*h2)

   dai3= (moy_kep_t1%e*de2 + 3._pm_reel*c2p2v)/2._pm_reel
   dai3= c1*s1*dai3*h2

   a1=1._pm_reel+moy_kep_t1%e*cv
   a2=a1*(1._pm_reel+a1)/tt2
   d1=2._pm_reel*(3._pm_reel*c2-1._pm_reel)*(1._pm_reel+a2)*sv
   d1=d1+s2*( 3._pm_reel*(1._pm_reel-a2)*s2p1v + (3._pm_reel*a2+1._pm_reel)*s2p3v )

   d21=6._pm_reel*(v + moy_kep_t1%e*sv - am2)
   d22=3._pm_reel*s2p2v + moy_kep_t1%e*(3._pm_reel*s2p1v + s2p3v)

   a1=5._pm_reel*c2 - 2._pm_reel*c1 -1._pm_reel
   d2=moy_kep_t1%e*b*tt1*d1

   dpgm3= d2 + a1*d21 + (2._pm_reel-a1)*d22
   dpgm3= 0.25_pm_reel*dpgm3*h2

   edm3= - 0.25_pm_reel*t3*d1*h2
   sidg3= 0.5_pm_reel*c1*s1*(d22-d21)*h2

   ax3= moy_kep_t1%a + dax3
   dex= dex2 + dex3
   dai= dai2 + dai3
   edm= edm2 + edm3
   sidg= sidg2 + sidg3
   pgm= pgm2 + dpgm3
   ! qlm=modulo(pgm,pm_deux_pi)
   ! qclm=cos(qlm)
   ! qslm=sin(qlm)

   ede= moy_kep_t1%e + dex
   qex= ede*cam1 -edm*sam1
   qey= ede*sam1 + edm*cam1

   ex3= sqrt( ede*ede + edm*edm )

   call mu_angle2(qex,qey,am3,code_retour_local)
   if (code_retour_local%valeur == pm_err_vect_nul) then
      code_retour%valeur = pm_err_cni
      go to 6000
   end if

   sidi=sdai + 0.5_pm_reel*dai*cdai
   sidg=0.5_pm_reel*sidg/cdai
   qix= sidi*cgom1 - sidg*sgom1
   qiy= sidi*sgom1 + sidg*cgom1
   si3= sqrt( sidi*sidi + sidg*sidg )

   ai3= 2._pm_reel * asin(si3)

   call mu_angle2(qix,qiy,go3,code_retour_local)
   if (code_retour_local%valeur == pm_err_vect_nul) then
      code_retour%valeur = pm_err_cni
      go to 6000
   end if

   po3= pgm-go3-am3

! recadrage dans [0, 2pi]
   am3=modulo(am3,pm_deux_pi)
   po3=modulo(po3,pm_deux_pi)
   go3=modulo(go3,pm_deux_pi)

   ! FIN DU MODELE POUR LES OSCULATEURS
   !===================================

   osc_kep_t2%a=ax3 * 1000._pm_reel ! passage en metres
   osc_kep_t2%e=ex3
   osc_kep_t2%i=ai3
   osc_kep_t2%pom=po3
   osc_kep_t2%gom=go3
   osc_kep_t2%M=am3

   ! Passage en parametres circulaires equatoriaux

   call mv_kep_cir_equa (osc_kep_t2, osc_t2_temp, code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

   osc_t2%a  = osc_t2_temp%a
   osc_t2%ex = osc_t2_temp%ex
   osc_t2%ey = osc_t2_temp%ey
   osc_t2%ix = osc_t2_temp%ix
   osc_t2%iy = osc_t2_temp%iy

   osc_t2%pso_M = modulo(osc_t2_temp%pso_M, pm_deux_pi) ! recadrage dans [0, 2pi]

end if

6000 continue

code_retour%routine = pm_num_me_lyddane
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine me_lyddane
