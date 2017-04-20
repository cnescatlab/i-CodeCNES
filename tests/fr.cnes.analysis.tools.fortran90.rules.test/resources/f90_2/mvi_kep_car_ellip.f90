subroutine mvi_kep_car_ellip (kep,mu,pos_car,vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres KEPleriens aux parametres CARtesiens dans le cas Elliptique
! ===
!
! Note d'utilisation:  - Routine interne
! ==================   - Uniquement appliquable au cas ELLIPtique 
!                      - L'appelant doit tester
!                            1)  que l'excentricite est dans [0.,1.[ 
!                            2)  que le demi grand axe est > 0.
!                            3)  que la constante gravitationnelle est > 0.
!
!$Historique
! ==========
!   + Version 1.0 (SP 231 ed01 rev00): creation a partir de la routine MVELOC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use int_chgmnt_variables, only : mv_kepler_std

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

real(pm_reel)                :: a,exc,ai,apom,agom,am,ae          ! parametres kepleriens:
                                                                  ! (a,e,i,petit omega,grand omega,M,E)
real(pm_reel), dimension(3)  :: p,q,xdpai,xdqai,xdpgo,xdqgo       ! vecteurs unitaires p et q, derivees par rapport a i et gomega
real(pm_reel)                :: cai,sai,cgo,sgo,cpo,spo,cae,sae   ! variables cos et sin des angles i,grand omega,petit omega,E
real(pm_reel)                :: cgocpo,sgocpo,sgospo,cgospo,v1,v2 ! autres variables intermediaires
real(pm_reel)                :: rld0,rmu0,rnu0,rki0,sqmusa        ! variables ld0,mu0,nu0,ki0,sqrt(mu/a),mu
real(pm_reel)                :: xdldex,xdmuex,xdldae,xdmuae       ! et leurs derivees par rapport a e et E
real(pm_reel)                :: xdnuex,xdkiex,xdnuae,xdkiae
real(pm_reel)                :: xdaeam,xdaeex                     ! derivees de E % M et de E % e

type(tm_code_retour)         :: code_retour

intrinsic sqrt, cos, sin, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_kep_car_ellip.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_kep_car_ellip.f90 362 2013-02-15 18:01:28Z bbjc $ '

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

call mv_kepler_std( am, exc, ae, code_retour)! calcul anomalie excentrique E par equation de kepler
if (code_retour%valeur /= pm_OK) then            ! seul retour en erreur possible : pm_err_conv_kepler_ellip
    retour = code_retour%valeur
    go to 6000      
end if

! 1- calcul vecteur position r = a (ld0*p + mu0*q)
!    et vecteur vitesse rv = sqrt(mu/a) (nu0*p + ki0*q)
!******************************************************

! ------------------------------------
! calcul de variables intermediaires
! -------------------------------------

cai = cos(ai)       !
sai = sin(ai)       !
cgo = cos(agom)     !
sgo = sin(agom)     !    cosinus et sinus des angles i,gomega,pomega,E
cpo = cos(apom)     !
spo = sin(apom)     !
cae = cos(ae)       !
sae = sin(ae)       !

cgocpo = cgo * cpo
sgocpo = sgo * cpo
sgospo = sgo * spo
cgospo = cgo * spo

v1 = sqrt(1._pm_reel-(exc*exc))!     calcul des variables sqrt(1-e**2)
v2 = 1._pm_reel - (exc*cae)    !     et 1-e*cosE

p(1) = cgocpo - (cai * sgospo)   !
p(2) = sgocpo + (cai * cgospo)   !
p(3) = sai * spo                 ! calcul des composantes des vecteurs p et q en fonction de                                
q(1) = - cgospo - (cai * sgocpo) ! angles i,pomega,gomega
q(2) = - sgospo + (cai * cgocpo) !
q(3) = sai * cpo                 !

rld0 = cae - exc    ! calcul de ld0 = cosE - e et de
rmu0 = sae * v1     ! mu0 = sinE * sqrt(1-e**2)

sqmusa = sqrt(mu/a)    ! calcul de sqmusa = sqrt(mu/a)
rnu0 = -sae / v2       !        de nu0 = -sinE/(1-e*cosE)
rki0 = (cae * v1) / v2 !     et de ki0 = cosE*sqrt(1-e**2) / 1-e*cosE

pos_car(:) = a * ((rld0 * p(:)) + (rmu0 * q(:)))       ! calcul des composantes des vecteurs position 
vit_car(:) = sqmusa * ((rnu0 * p(:)) + (rki0 * q(:)))  ! et vitesse

! 2- calcul du jacobien de la transformation
! ********************************************

if (present(jacob))  then

   xdpai(1) = sai  * sgospo  !
   xdpai(2) = -sai * cgospo  !
   xdpai(3) = cai  * spo     ! derivees des composantes de p et q par rapport a i
   xdqai(1) = sai  * sgocpo  !
   xdqai(2) = -sai * cgocpo  !
   xdqai(3) = cai  * cpo     !

   xdpgo(1) = - sgocpo - (cai * cgospo) !
   xdpgo(2) = cgocpo - (cai * sgospo)   !
   xdpgo(3) = 0._pm_reel                ! derivees des composantes de p et q par rapport a gomega
   xdqgo(1) = sgospo - (cai * cgocpo)   !
   xdqgo(2) = - cgospo - (cai * sgocpo) !
   xdqgo(3) = 0._pm_reel                !

   xdldex = -1._pm_reel     !
   xdmuex = sae * (-exc/v1) !  calcul des derivees de ld0 et mu0 par rapport a e et E
   xdldae = -sae            !
   xdmuae = cae * v1        !

   xdaeam = 1._pm_reel / v2  ! calcul derivees de E par rapport a M et e
   xdaeex = sae * xdaeam     ! dE/dam = 1 / 1-exc*cosE et dE/dex = sinE / 1-exc*cosE

   ! calcul des derivees de nu0 et ki0 par rapport a e et E
   ! avec les variables : xdaeam = 1._pm_reel/v2  et xdaeex = sae/v2
   xdnuex = -cae * xdaeam * xdaeex                        !     calcul de xdnuex = (-cae*sae) / (v2**2)
   xdkiex = xdaeam * cae * ((-exc/v1) + cae*v1*xdaeam)    !     calcul de xdkiex=((-exc*cae)/(v1*v2)) + (((cae**2)*v1)/(v2**2))
   xdnuae = xdaeam * (-cae + (exc*sae*xdaeex))            !     calcul de xdnuae = (-cae/v2) + ((exc*(sae**2)) / (v2**2))
   xdkiae = v1 * xdaeex * (-1._pm_reel - (exc*cae*xdaeam))!     calcul de xdkiae = v1 * ((-sae/v2) - ((exc*cae*sae) / (v2**2)))

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport a a,e,i,pomega,gomega,M
   !===========================================================

   jacob(1:3,1) = (rld0 * p(:)) + (rmu0 * q(:))      ! ---par rapport a a
   jacob(4:6,1) = (-0.5_pm_reel*sqmusa/a) *((rnu0 * p(:)) + (rki0 * q(:)))

   jacob(1:3,2) = a * (((xdldex*p(:)) + (xdmuex*q(:))) + ((xdldae*p(:)) + (xdmuae*q(:))) * xdaeex)     ! ---par rapport a e 
   jacob(4:6,2) = sqmusa * (((xdnuex*p(:)) + (xdkiex*q(:))) +((xdnuae*p(:)) + (xdkiae*q(:))) * xdaeex) 
   !( = derivee/e + derivee/E * derivee E/e)

   jacob(1:3,3) = a * ((rld0 * xdpai(:)) + (rmu0 * xdqai(:)))      ! ---par rapport a i
   jacob(4:6,3) = sqmusa * ((rnu0 * xdpai(:)) + (rki0 * xdqai(:)))

   jacob(1:3,4) = a * ((rld0 * q(:)) - (rmu0 * p(:)))             ! ---par rapport a pomega (q = derivee de p /pomega)
   jacob(4:6,4) = sqmusa * ((rnu0 * q(:)) - (rki0 * p(:)))

   jacob(1:3,5) = a * ((rld0 * xdpgo(:)) + (rmu0 * xdqgo(:)))     ! ---par rapport a gomega
   jacob(4:6,5) = sqmusa * ((rnu0 * xdpgo(:)) + (rki0 * xdqgo(:)))

   jacob(1:3,6) = a * ((xdldae * p(:)) + (xdmuae * q(:))) * xdaeam! ---par rapport a M ( = derivee/E * derivee de E/M)
   jacob(4:6,6) = sqmusa * ((xdnuae * p(:)) + (xdkiae * q(:))) *xdaeam

end if

6000 continue

end subroutine mvi_kep_car_ellip
