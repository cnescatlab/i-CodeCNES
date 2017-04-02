subroutine mvi_kep_car_hyperb (kep,mu,pos_car,vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres KEPleriens aux parametres CARtesiens dans le cas HYPERBolique
! ===
!
! Note d'utilisation:  -  Routine interne 
! ==================   -  Uniquement appliquable au cas HYPERBolique
!                      -  L'appelant doit verifier que:
!                              1) l'excentricite est > 1
!                              2) le demi grand axe est > 0
!                              3) la constante de la gravitation est > 0
!                              
!
!$Historique
! ==========
!   + Version 1.0 (SP 232 ed01 rev00): creation a partir de la routine MVHYOC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!!************************************************************************

! Modules
! =======
use int_var_internes, only : mvi_kepler_hyperb

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                              
type(tm_orb_kep), intent(in)                         :: kep     ! parametres kepleriens
real(pm_reel), intent(in)                            :: mu      ! constante de la gravitation

real(pm_reel), dimension(3), intent(out)             :: pos_car ! position en coordonnees cartesiennes
real(pm_reel), dimension(3), intent(out)             :: vit_car ! vitesse ne coordonnees cartesiennes
integer ,intent(out)                                 ::  retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob   ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)               :: a,exc,ai,apom,agom,am,ah         ! parametres kepleriens (a,e,angles i,petit omega,grand omega,M,H)
real(pm_reel), dimension(3) :: p,q,xdpai,xdqai,xdpgo,xdqgo      ! vecteurs unitaires p et q, derivees par rapport a i et gomega
real(pm_reel)               :: cai,sai,cgo,sgo,cpo,spo,chah,shah! variables cos et sin des angles i,grand omega,petit omega,H
real(pm_reel)               :: cgocpo,sgocpo,sgospo,cgospo,v1,v2! autres variables intermediaires
real(pm_reel)               :: rld0,rmu0,rnu0,rki0,sqmusa       ! variables ld0,mu0,nu0,ki0,sqrt(mu/a),mu
real(pm_reel)               :: xdldex,xdmuex,xdldah,xdmuah      ! et leurs derivees par rapport a e et H
real(pm_reel)               :: xdnuex,xdkiex,xdnuah,xdkiah
real(pm_reel)               :: xdaham,xdahex                    ! derivees de H % m et de H % e

intrinsic sqrt, cos, sin, cosh, sinh, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_kep_car_hyperb.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_kep_car_hyperb.f90 362 2013-02-15 18:01:28Z bbjc $ '

!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

a    = kep%a
exc  = kep%e
ai   = kep%i
apom = kep%pom
agom = kep%gom
am   = kep%M

call mvi_kepler_hyperb(am, exc, ah, retour) ! calcul anomalie excentrique H par equation de kepler   e * sinh(H) - H = M
if (retour /= 0) go to 6000 ! seul le code retour 0 ou pm_err_conv_kepler_hyperb issu de mvi_kepler_hyperb est possible ici
     

!*************************************************************
! 1- calcul vecteur position r = a (ld0*p + mu0*q)
!        et vecteur vitesse rv = sqrt(mu/a) (nu0*p + ki0*q)
!*************************************************************

! ------------------------------------
! calcul de variables intermediaires
! -------------------------------------

cai = cos(ai)       !
sai = sin(ai)       !
cgo = cos(agom)     !
sgo = sin(agom)     !    cosinus et sinus des angles i,gomega,pomega
cpo = cos(apom)     !
spo = sin(apom)     !
cgocpo = cgo * cpo
sgocpo = sgo * cpo
sgospo = sgo * spo
cgospo = cgo * spo

chah = cosh(ah)!     cosinus et 
shah = sinh(ah)!     sinus hyperboliques de H

v1 = sqrt((exc*exc)- 1._pm_reel)!     calcul des variables sqrt(e**2-1) et 
v2 = (exc*chah) - 1._pm_reel    !     e*cosh(H)-1

p(1) = cgocpo - (cai * sgospo)   !
p(2) = sgocpo + (cai * cgospo)   !
p(3) = sai * spo                 ! calcul des composantes des vecteurs p et q en fonction de                                
q(1) = - cgospo - (cai * sgocpo) ! angles i,pomega,gomega
q(2) = - sgospo + (cai * cgocpo) !
q(3) = sai * cpo                 !

rld0 =exc - chah! calcul de ld0 =e-coshH et de
rmu0 = shah * v1! mu0 = sinhH * sqrt(e**2-1)

sqmusa = sqrt(mu/a)     ! calcul de sqmusa = sqrt(mu/a),
rnu0 = -shah / v2       !  de nu0 = -sinhH/(e*coshH-1)
rki0 =  (chah * v1) / v2! et de ki0 = coshH*sqrt(e**2-1) / (e*coshH-1)

pos_car(:) = a * ((rld0 * p(:)) + (rmu0 * q(:)))     ! calcul des composantes des vecteurs position 
vit_car(:) = sqmusa * ((rnu0 * p(:)) + (rki0 * q(:)))!et vitesse

!********************************************
! 2- calcul du jacobien de la transformation
!********************************************

if (present(jacob))  then

   !-----------------------------------
   ! calcul de variables intermediaires
   !-----------------------------------

   xdpai(1) = sai  * sgospo
   xdpai(2) = -sai * cgospo
   xdpai(3) = cai  * spo       ! derivees des composantes de p et q par rapport a i
   xdqai(1) = sai  * sgocpo
   xdqai(2) = -sai * cgocpo
   xdqai(3) = cai  * cpo

   xdpgo(1) = - sgocpo - (cai * cgospo)
   xdpgo(2) = cgocpo - (cai * sgospo)
   xdpgo(3) = 0._pm_reel             ! derivees des composantes de p et q par rapport a gomega
   xdqgo(1) = sgospo - (cai * cgocpo)
   xdqgo(2) = - cgospo - (cai * sgocpo)
   xdqgo(3) = 0._pm_reel

   xdldex =  1._pm_reel
   xdmuex = shah * (exc/v1)    ! calcul des derivees de ld0 et mu0 par rapport a e et H
   xdldah = -shah
   xdmuah = chah * v1

   ! calcul derivees de H par rapport a M et e
   xdaham = 1._pm_reel / v2          ! dH/dam = 1 / (exc*coshH-1) et 
   xdahex = - shah * xdaham          ! dH/dex =-sinH / (-1+exc*coshH)

   ! calcul des derivees de nu0 et ki0 par rapport a e et H
   ! avec la variable : xdaham = 1._pm_reel / v2
   xdnuex = chah * shah * xdaham * xdaham                  ! calcul de: xdnuex = (chah*shah) / (v2**2)
   xdkiex = xdaham * chah * ((exc/v1) - (chah*v1*xdaham))  ! calcul de: xdkiex=((exc*chah)/(v1*v2)) - (((chah**2)*v1)/(v2**2))
   xdnuah = xdaham * (-chah + (exc*shah*shah*xdaham))      ! calcul de: xdnuah = (-chah/v2) + ((exc*(shah**2)) / (v2**2))
   ! calcul de: xdkiah = v1 * ((shah/v2) - ((exc*chah*shah) / (v2**2)))
   xdkiah = v1 * xdaham * shah * (1._pm_reel - (exc*chah*xdaham))

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport a a,e,i,pomega,gomega,m
   !===========================================================

   !        ---par rapport a a
   jacob(1:3,1) = (rld0 * p(:)) + (rmu0 * q(:))
   jacob(4:6,1) = (-0.5_pm_reel*sqmusa/a) *((rnu0 * p(:)) + (rki0 * q(:)))

   !        ---par rapport a e ( = derivee/e + derivee/H * derivee H/e)
   jacob(1:3,2) = a * (((xdldex*p(:)) + (xdmuex*q(:))) +((xdldah*p(:)) + (xdmuah*q(:))) * xdahex)
   jacob(4:6,2) = sqmusa * (((xdnuex*p(:)) + (xdkiex*q(:))) +((xdnuah*p(:)) + (xdkiah*q(:))) * xdahex)

   !        ---par rapport a i
   jacob(1:3,3) = a * ((rld0 * xdpai(:)) + (rmu0 * xdqai(:)))
   jacob(4:6,3) = sqmusa * ((rnu0 * xdpai(:)) + (rki0 * xdqai(:)))

   !        ---par rapport a pomega (q = derivee de p /pomega)
   jacob(1:3,4) = a * ((rld0 * q(:)) - (rmu0 * p(:)))
   jacob(4:6,4) = sqmusa * ((rnu0 * q(:)) - (rki0 * p(:)))

   !        ---par rapport a gomega
   jacob(1:3,5) = a * ((rld0 * xdpgo(:)) + (rmu0 * xdqgo(:)))
   jacob(4:6,5) = sqmusa * ((rnu0 * xdpgo(:)) + (rki0 * xdqgo(:)))

   !        ---par rapport a M ( = derivee/H * derivee de H/M)
   jacob(1:3,6) = a * ((xdldah * p(:)) + (xdmuah * q(:))) * xdaham
   jacob(4:6,6) = sqmusa * ((xdnuah * p(:)) + (xdkiah * q(:))) *xdaham

end if

6000 continue

end subroutine mvi_kep_car_hyperb
