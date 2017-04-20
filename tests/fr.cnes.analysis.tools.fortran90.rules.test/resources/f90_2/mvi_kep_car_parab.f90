subroutine mvi_kep_car_parab (kep,mu,pos_car,vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres KEPleriens aux parametres CARtesiens dans le cas PARABolique
! ===
!
! Note d'utilisation:  -  Routine interne 
! ==================   -  Uniquement appliquable au cas PARABolique
!                      -  L'appelant doit verifier que :
!                             1) l'excentricite est proche de 1.
!                             2) le parametre p est > 0.
!                             3) la constante de la gravitation est > 0
!$Historique
! ==========
!   + Version 1.0 (SP 233 ed01 rev00): creation a partir de la routine MVPAOC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use int_var_internes, only : mvi_barker

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

!-------------------------------
!declaration des donnees locales
!-------------------------------

real(pm_reel)     :: rp,ai,apom,agom,am                         ! parametres kepleriens :
                                                                ! parametre p, i, petit omega, grand omega, M
real(pm_reel)     :: d
real(pm_reel)     :: cai,sai,cgo,sgo,cpo,spo                    ! variables cos et sin des angles i,gomega,pomega
real(pm_reel)     :: cgocpo,sgocpo,sgospo,cgospo                ! autres variables intermediaires
real(pm_reel)     :: rld0,rmu0,rnu0,smusrp,coef                 ! variables ld0,mu0,nu0,sqrt(mu/rp),coef,mu
real(pm_reel)     :: xdldd,xdmud                                ! et leurs derivees par rapport a d ou parametre p
real(pm_reel)     :: xdnud,dcoefd,dcoefp
real(pm_reel)     :: xddam                                      ! derivee de d % M
real(pm_reel), dimension(3) :: p ,q ,xdpai ,xdqai ,xdpgo ,xdqgo ! vecteurs unitaires P et Q, derivees par rapport a i et gomega

intrinsic sin, cos, sqrt, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mvi_kep_car_parab.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_kep_car_parab.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

rp   = kep%a
ai   = kep%i
apom = kep%pom
agom = kep%gom
am   = kep%M

call mvi_barker (am, d, retour)! resolution de l'equation de Barker :6 * M = 3 * d + d**3
                               ! avec d  =  tan(theta/2) et  theta = anomalie vraie
if (retour == pm_OK) retour = pm_warn_e_parab

!******************************************************************
! 1- calcul vecteur position r = rp (ld0*u + mu0*q)
!    et vecteur vitesse rv = 2*sqrt(mu/rp)(nu0*p+q)/(1.+d**2)
!******************************************************************

!     -------------------------------------
!     calcul de variables intermediaires
!     -------------------------------------

cai = cos(ai)
sai = sin(ai)
cgo = cos(agom)
sgo = sin(agom)
cpo = cos(apom)            !     cosinus et sinus des angles i,grand omega,petit omega
spo = sin(apom)
cgocpo = cgo * cpo
sgocpo = sgo * cpo
sgospo = sgo * spo
cgospo = cgo * spo

p(1) = cgocpo - (cai * sgospo)
p(2) = sgocpo + (cai * cgospo)
p(3) = sai * spo                ! calcul des composantes des vecteurs P et Q en fonction des
q(1) = - cgospo - (cai * sgocpo)! angles i,petit omega grand omega
q(2) = - sgospo + (cai * cgocpo)
q(3) = sai * cpo

rld0 = (1._pm_reel - d*d)/2._pm_reel        ! calcul de ld0 = (1. - d**2)/2 et de 
rmu0 = d                                    ! mu0 = d

smusrp = sqrt(mu/rp)                         ! calcul de smusrp = sqrt(mu/rp) , de 
rnu0 = - d                                   ! nu0 = -d et
coef = 2._pm_reel * smusrp / (1._pm_reel+d*d)! de coef = 2.*smusrp / (1.+d**2)

pos_car(:) = rp * ((rld0 * p(:)) + (rmu0 * q(:)))! calcul des composantes des vecteurs position et 
vit_car(:) = coef * ((rnu0 * p(:)) + q(:))       ! vitesse

!********************************************
! 2- calcul du jacobien de la transformation
!********************************************

if (present(jacob))  then

   !-----------------------------------
   !calcul de variables intermediaires
   !-----------------------------------

   xdpai(1) = sai  * sgospo
   xdpai(2) = -sai * cgospo
   xdpai(3) = cai  * spo        ! derivees des composantes de P et Q par rapport a i
   xdqai(1) = sai  * sgocpo
   xdqai(2) = -sai * cgocpo
   xdqai(3) = cai  * cpo

   xdpgo(1) = - sgocpo - (cai * cgospo)
   xdpgo(2) = cgocpo - (cai * sgospo)
   xdpgo(3) = 0._pm_reel              !  derivees des composantes de P et Q par rapport a grand omega
   xdqgo(1) = sgospo - (cai * cgocpo)
   xdqgo(2) = - cgospo - (cai * sgocpo)
   xdqgo(3) = 0._pm_reel

   dcoefp  = - smusrp/(rp*(1._pm_reel+d*d))   ! calcul derivee de coeff par rapport au parametre p

   xdldd = -d         ! calcul des derivees de ld0 et de  
   xdmud = 1._pm_reel ! mu0 par rapport a d

   xddam = 2._pm_reel / (1._pm_reel + d*d)   ! calcul derivee de d par rapport a M
                                             !    dd/dam = 2._pm_reel / (1 + d**2)

   xdnud  = -1._pm_reel                   ! calcul de la derivee de nu0 par rapport a d
   dcoefd = -smusrp * d * xddam * xddam   ! calcul de la derivee de coef par rapport a d = -4. * smusrp * d /((1.+d*d)**2)

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport au parametre p,e,i,pomega,gomega,m
   !===========================================================

   !        ---par rapport au parametre p
   jacob(1:3,1) = (rld0 * p(:)) + (rmu0 * q(:))
   jacob(4:6,1) =  dcoefp * ((rnu0 * p(:)) + q(:))

   !        ---par rapport a e ( toujours nulle)
   jacob(1:3,2) = 0._pm_reel
   jacob(4:6,2) = 0._pm_reel

   !        ---par rapport a i
   jacob(1:3,3) = rp * ((rld0 * xdpai(:)) + (rmu0 * xdqai(:)))
   jacob(4:6,3) = coef  * ((rnu0 * xdpai(:)) + xdqai(:))

   !        ---par rapport a pomega (q = derivee de p /pomega)
   jacob(1:3,4) = rp * ((rld0 * q(:)) - (rmu0 * p(:)))
   jacob(4:6,4) = coef * ((rnu0 * q(:)) - p(:))

   !        ---par rapport a gomega
   jacob(1:3,5) = rp * ((rld0 * xdpgo(:)) + (rmu0 * xdqgo(:)))
   jacob(4:6,5) = coef * ((rnu0 * xdpgo(:)) + xdqgo(:))

   !        ---par rapport a M ( = derivee/d * derivee de d/m)
   jacob(1:3,6) = rp * ((xdldd * p(:)) + (xdmud * q(:))) * xddam
   jacob(4:6,6) = ((dcoefd*rnu0+coef*xdnud) * p(:) +(dcoefd * q(:))) * xddam

end if

end subroutine mvi_kep_car_parab
