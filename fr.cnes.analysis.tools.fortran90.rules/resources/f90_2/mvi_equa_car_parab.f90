subroutine mvi_equa_car_parab(equa,mu,pos_car, vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires
! ===   aux parametres CARtesiens dans le cas PARABolique.
!
! Note d'utilisation: - Routine interne.
! ==================  - L'appelant doit tester que :
!                           1) l'excentricite est proche de 1
!                           2) le parametre p est > 0
!                           3) la constante de la gravitation est > 0
!                           4) la norme du vecteur inclinaison est < 2
!
!$Historique
! ==========
!   + Version 1.0 (SP 243 ed01 rev00): creation a partir de la routine MVPAQC de la MSLIB f77
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
                                                       
type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), intent(in)                :: mu       ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
integer, intent(out)                     :: retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)   ::   rp,om,hx,hy,am,d                 !     parametres osculateurs adaptes
real(pm_reel), dimension(3)   ::   u,w                !     vecteurs unitaires u et w, et leurs
real(pm_reel), dimension(3)   ::   duhx,duhy,dwhx,dwhy!     derivees par rapport a hx et hy
real(pm_reel)   ::   com,som                          !     variables cos et sin de om
real(pm_reel)   ::   smusrp                           !     variable sqrt(mu/rp)
real(pm_reel)   ::   x1,y1,xp1,x3,y3,xp3,yp3          !     variables x1,y1,xp1,x3,y3,xp3,yp3
real(pm_reel)   ::   dx1d,dy1d,dx3d,dy3d,dx3om,dy3om  !     et leurs derivees par rapport a d 
real(pm_reel)   ::   dxp1d,dxp3d,dyp3d,dxp3om,dyp3om  !     et om
real(pm_reel)   ::   ddam                             !     derivee de d % M
!     variables intermediaires
real(pm_reel)   ::   coef,cofh,dcoefd,dcoefp,dcof
real(pm_reel)   ::   h2,hx2,hy2,hxhys2

intrinsic cos, sin, sqrt, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_equa_car_parab.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_equa_car_parab.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

rp  = equa%a
om  = equa%pgom
hx  = equa%ix
hy  = equa%iy
am  = equa%M

!==================================================
! resolution de l'equation de barker :
!        6 * M = 3 * D + D**3
!     avec D    =  tan(theta/2)
!          theta = anomalie vraie
!==================================================
call mvi_barker(am, d, retour)

! ******************************************************************
! 1- calcul vecteur position r = rp (x3*u + y3*w)
!    et vecteur vitesse rv = 2*sqrt(mu/rp) (xp3*u + w)/(1._pm_reel+d**2)
! ******************************************************************

!     -------------------------------------
!     calcul de variables intermediaires
!     -------------------------------------

com = cos(om)!     cosinus de om=(petit omega + grand omega)
som = sin(om)!     sinus   de om=(petit omega + grand omega)

hx2    = hx*hx
hy2    = hy*hy
hxhys2 = hx*hy / 2._pm_reel
h2     = hx2 + hy2
cofh   = sqrt(1._pm_reel-h2/4._pm_reel)

u(1) = 1._pm_reel - hy2 / 2._pm_reel
u(2) =  hxhys2                          ! composantes du vecteur u 
u(3) = -hy * cofh
w(1) =  hxhys2
w(2) = 1._pm_reel - hx2 / 2._pm_reel    ! composantes du vecteur w
w(3) =  hx * cofh

x1 = (1._pm_reel - d**2)/2._pm_reel   
y1 = d                               

smusrp = sqrt(mu/rp)       
xp1 = - d                
coef = 2._pm_reel * smusrp / (1._pm_reel+d*d)  

x3 = x1 * com - y1 * som
y3 = x1 * som + y1 * com

xp3 = xp1 * com - som
yp3 = xp1 * som + com

! =======================================================
! calcul des composantes des vecteurs position et vitesse
! =======================================================

pos_car(:) = rp * ((x3 * u(:)) + (y3 * w(:)))
vit_car(:) = coef * ((xp3 * u(:)) + (yp3 * w(:)))

! ********************************************
! 2- calcul du jacobien de la transformation
! ********************************************

if (present(jacob))  then

   !     -----------------------------------
   !     calcul de variables intermediaires
   !     -----------------------------------

   !     derivees des composantes de u et w par rapport a hx et hy

   dcof     = -4._pm_reel * cofh

   duhx(1) = 0._pm_reel
   duhx(2) = hy / 2._pm_reel
   duhx(3) = -hy * hx / dcof
   dwhx(1) = hy / 2._pm_reel
   dwhx(2) = -hx
   dwhx(3) = cofh + hx2 / dcof

   duhy(1) = -hy
   duhy(2) =  hx / 2._pm_reel
   duhy(3) =  - cofh - hy2 / dcof
   dwhy(1) =  hx / 2._pm_reel
   dwhy(2) = 0._pm_reel
   dwhy(3) =  hx * hy / dcof

   !     derivees des composantes x3 et y3 par rapport a om
   dx3om = - x1 * som - y1 * com
   dy3om =   x1 * com - y1 * som

   !     derivees des composantes xp3 et yp3 par rapport a om
   dxp3om = - xp1 * som - com
   dyp3om =   xp1 * com - som

   ! calcul des derivees de x1 et y1 par rapport a d
   dx1d = -d
   dy1d = 1._pm_reel

   ! calcul des derivees de xp1 et coef par rapport a d
   dxp1d  = -1._pm_reel
   dcoefd = -4._pm_reel * smusrp * d /((1._pm_reel+d*d)**2)

   !     derivees des composantes x3 et y3 par rapport a d
   dx3d = dx1d * com - dy1d * som
   dy3d = dx1d * som + dy1d * com

   ! calcul des derivees de xp3 et yp3 par rapport a d
   dxp3d = dxp1d * com
   dyp3d = dxp1d * som

   ! calcul derivee de d par rapport a m
   !    dd/dam = 2._pm_reel / (1 + d**2)
   ddam = 2._pm_reel / (1._pm_reel + d*d)

   ! calcul derivee de coeff par rapport a rp
   dcoefp  = - smusrp/(rp*(1._pm_reel+d*d))

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport aux parametres adaptes
   !===========================================================

   !        ---par rapport a rp
   jacob(1:3,1) = (x3 * u(:)) + (y3 * w(:))
   jacob(4:6,1) =  dcoefp * (xp3 * u(:) + yp3 * w(:))

   !        ---par rapport a e ( toujours nulle)
   jacob(1:3,2) = 0._pm_reel
   jacob(4:6,2) = 0._pm_reel

   !        ---par rapport a om
   jacob(1:3,3) = rp * (dx3om*u(:) + dy3om*w(:))
   jacob(4:6,3) = coef * ((dxp3om*u(:)) + (dyp3om*w(:)))

   !        ---par rapport a hx
   jacob(1:3,4) = rp * (x3 * duhx(:) + y3 *  dwhx(:))
   jacob(4:6,4) = coef  * (xp3 * duhx(:) + yp3 * dwhx(:))

   !        ---par rapport a hy
   jacob(1:3,5) = rp * (x3 * duhy(:) + y3 * dwhy(:))
   jacob(4:6,5) = coef  * (xp3 * duhy(:) + yp3 * dwhy(:))

   !        ---par rapport a m ( = derivee/d * derivee de d/m)
   jacob(1:3,6) = rp  * ((dx3d * u(:)) + (dy3d * w(:))) * ddam
   jacob(4:6,6) = (( dcoefd*xp3+coef*dxp3d) * u(:) +(dcoefd*yp3+coef*dyp3d) * w(:)) * ddam

end if

end subroutine mvi_equa_car_parab
