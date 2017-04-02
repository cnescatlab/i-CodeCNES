subroutine mvi_equa_car_hyperb (equa,mu,pos_car, vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires
! ===   aux parametres CARtesiens dans le cas HYPERBolique.
!
! Note d'utilisation:  - Routine interne.
! ==================   - L'appelant doit tester que :
!                            1) l'excentricite est  > 1.
!                            2) le demi grand axe est  > 0.
!                            1) la constante de la gravitation est  > 0.
!                            1) la norme du vecteur inclinaison est  < 2.
!
!$Historique
! ==========
!   + Version 1.0 (SP 242 ed01 rev00): creation a partir de la routine MVHYQC de la MSLIB f77
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
use int_var_internes, only : mvi_kepler_hyperb

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
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)   ::  a,exc,om,hx,hy,am,ah                  !     parametres osculateurs adaptes
real(pm_reel),dimension(3)   ::  u,w,duhx,duhy, dwhx,dwhy !     vecteurs unitaires u et w et derivees par rapport a hx et hy
real(pm_reel)   ::  com,som,chah,shah                     !     variables cos et sin des angles om et H
!     autres variables intermediaires
real(pm_reel)   ::  v1,v2,cofh,dcof
real(pm_reel)   ::  h2,hx2,hy2,hxhys2
real(pm_reel)   ::  x1,y1,xp1,yp1             ! variables x1,y1,xp1,yp1
real(pm_reel)   ::  x3,y3,xp3,yp3,sqmusa      ! x3,y3,xp3,yp3,sqrt(mu/a)
!     et leurs derivees par rapport a e, om et H
real(pm_reel)   ::  dx1ah,dx1ex,dx3ah,dx3ex
real(pm_reel)   ::  dxp1ah,dxp1ex,dxp3ah,dxp3ex
real(pm_reel)   ::  dy1ah,dy1ex,dy3ah,dy3ex
real(pm_reel)   ::  dyp1ah,dyp1ex,dyp3ah,dyp3ex
real(pm_reel)   ::  dx3om,dy3om,dxp3om,dyp3om
real(pm_reel)   ::  daham,dahex               !     derivees de H / M et de H / e

intrinsic cos, sin, cosh, sinh, sqrt, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_equa_car_hyperb.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_equa_car_hyperb.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

a   = equa%a
exc = equa%e
om  = equa%pgom
hx  = equa%ix
hy  = equa%iy
am  = equa%M

! ======================================================
! calcul anomalie hyperbolique H par equation de kepler
!            e * sinh(H) - H = M
! ======================================================
call mvi_kepler_hyperb(am, exc, ah, retour)
if (retour /= 0) go to 6000!     seul retour = 0 ou pm_err_conv_kepler_hyperb issu de mvi_kepler_hyperb possible ici

! *************************************************************
! 1- calcul vecteur position r = a (x3*u + y3*w)
!        et vecteur vitesse rv = sqrt(mu/a) (xp3*u + yp3*w)
! *************************************************************

!     -------------------------------------
!     calcul de variables intermediaires
!     -------------------------------------

com = cos(om) 
som = sin(om)  
chah = cosh(ah)
shah = sinh(ah)

v1 = sqrt((exc*exc)- 1._pm_reel)
v2 = (exc*chah) - 1._pm_reel    

hx2    = hx*hx
hy2    = hy*hy
hxhys2 = hx*hy / 2._pm_reel
h2     = hx2 + hy2
cofh   = sqrt(1._pm_reel-h2/4._pm_reel)

u(1) = 1._pm_reel - hy2 / 2._pm_reel
u(2) =  hxhys2                         !  composantes du vecteur u 
u(3) = -hy * cofh
w(1) =  hxhys2
w(2) = 1._pm_reel - hx2 / 2._pm_reel   !  composantes du vecteur w
w(3) =  hx * cofh

x1 = exc - chah
y1 = shah * v1
x3 = x1 * com - y1 * som       
y3 = x1 * som + y1 * com       

sqmusa = sqrt(mu/a)            
xp1  = -shah / v2
yp1  =  (chah * v1) / v2
xp3 = xp1 * com - yp1 * som    
yp3 = xp1 * som + yp1 * com    

! =======================================================
! calcul des composantes des vecteurs position et vitesse
! =======================================================

pos_car(:) = a * ((x3 * u(:)) + (y3 * w(:)))
vit_car(:) = sqmusa * ((xp3 * u(:)) + (yp3 * w(:)))

!********************************************
! 2- calcul du jacobien de la transformation
!********************************************

if  (present(jacob))  then

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
   dxp3om = - xp1 * som - yp1 * com
   dyp3om =   xp1 * com - yp1 * som

   ! calcul des derivees de x1 et y1 par rapport a e et H
   dx1ex = 1._pm_reel
   dy1ex = shah * (exc/v1)
   dx1ah = -shah
   dy1ah = chah * v1

   !     derivees des composantes x3 et y3 par rapport a e et H
   dx3ex = dx1ex * com - dy1ex * som
   dy3ex = dx1ex * som + dy1ex * com
   dx3ah = dx1ah * com - dy1ah * som
   dy3ah = dx1ah * som + dy1ah * com

   ! calcul des derivees de xp1 et yp1 par rapport a e et H
   dxp1ex = (chah*shah) / (v2**2)
   dyp1ex = ((exc*chah) / (v1*v2)) - (((chah**2)*v1) / (v2**2))
   dxp1ah = (-chah/v2) + ((exc*(shah**2)) / (v2**2))
   dyp1ah = v1 * ((shah/v2) - ((exc*chah*shah) / (v2**2)))

   ! calcul des derivees de xp3 et yp3 par rapport a e et H
   dxp3ex = dxp1ex * com - dyp1ex * som
   dyp3ex = dxp1ex * som + dyp1ex * com
   dxp3ah = dxp1ah * com - dyp1ah * som
   dyp3ah = dxp1ah * som + dyp1ah * com

   ! calcul derivees de H par rapport a M et e
   ! dH/dM = 1 / (exc*coshH-1) et  dH/dex =-sinH / (-1+exc*cosh H)
   daham = 1._pm_reel / v2
   dahex = - shah / v2

   ! ===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport aux parametres adaptes
   ! ===========================================================

   !        ---par rapport a a
   jacob(1:3,1) = (x3 * u(:)) + (y3 * w(:))
   jacob(4:6,1) = (-0.5_pm_reel*sqmusa/a) * ((xp3 * u(:)) + (yp3 * w(:)))

   !        ---par rapport a e ( = derivee/e + derivee/ H * derivee H/e)
   jacob(1:3,2) = a * (((dx3ex*u(:)) + (dy3ex*w(:))) +((dx3ah*u(:)) + (dy3ah*w(:))) * dahex)
   jacob(4:6,2) = sqmusa * (((dxp3ex*u(:)) + (dyp3ex*w(:))) +((dxp3ah*u(:)) + (dyp3ah*w(:))) * dahex)

   !        ---par rapport a om
   jacob(1:3,3) = a * (dx3om*u(:) + dy3om*w(:))
   jacob(4:6,3) = sqmusa * ((dxp3om*u(:)) + (dyp3om*w(:)))

   !        ---par rapport a hx
   jacob(1:3,4) = a * ((x3 * duhx(:)) + (y3 * dwhx(:)))
   jacob(4:6,4) = sqmusa * ((xp3 * duhx(:)) + (yp3 * dwhx(:)))

   !        ---par rapport a hy
   jacob(1:3,5) = a * ((x3 * duhy(:)) + (y3 * dwhy(:)))
   jacob(4:6,5) = sqmusa * ((xp3 * duhy(:)) + (yp3 * dwhy(:)))

   !        ---par rapport a M ( = derivee/ H * derivee de H/m)
   jacob(1:3,6) = a * ((dx3ah * u(:)) + (dy3ah * w(:))) * daham
   jacob(4:6,6) = sqmusa * ((dxp3ah * u(:)) + (dyp3ah * w(:))) *daham

end if

6000 continue

end subroutine mvi_equa_car_hyperb
