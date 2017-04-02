subroutine mvi_car_equa_parab (pos_car, vit_car,rrxv,norme_vect_pos,norme_moment_cinetique,&
                               excentricite, parametre,rscal,mu,equa,retour,jacob)
! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des paremetres CARtesiens aux parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires
! ===   applicable uniqu ment au cas PARABolique.
!
! Note d'utilisation:  -  Routine interne 
! ==================   -  Uniquement appliquable au cas PARABolique
!                      -  L'appelant doit verifier que :
!                             1) l'excentricite est proche de 1.
!                             2) le parametre p est > 0.
!                             3) la norme du vecteur position est non nulle
!                             4) la norme du moment cinetique est non nulle

!
!$Historique
! ==========
!   + Version 1.0 (SP 239 ed01 rev00): creation a partir de la routine MVPACQ de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
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
!************************************************************************

! Modules
! =======
use int_utilitaires, only : mu_angle2
use int_util_internes, only : mui_dot_product3

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel),dimension(3), intent(in)               ::  pos_car ! position en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  vit_car ! vitesse en coordonnees cartesiennes
real(pm_reel),dimension(3), intent(in)               ::  rrxv    ! produit vectoriel position x vitesse
real(pm_reel),intent(in)                             ::  norme_vect_pos   !  norme du vecteur position
real(pm_reel),intent(in)                             ::  norme_moment_cinetique   !  norme du produit vectoriel de ces derniers
real(pm_reel),intent(in)                             ::  excentricite     !  excentricite e
real(pm_reel),intent(in)                             ::  parametre        !  parametre p
real(pm_reel),intent(in)                             ::  rscal   ! produit scalaire position.vitesse
real(pm_reel),intent(in)                             ::  mu      ! constante de la gravitation

type(tm_orb_equa), intent(out)                       ::  equa    ! parametres orbitaux de l'orbite equatoriale
integer ,intent(out)                                 ::  retour
real(pm_reel),dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

!     tableaux derivees intermediaires
real(pm_reel), dimension(6)    :: drscal
real(pm_reel), dimension(6)    :: drxv1,drxv2,drxv3
real(pm_reel), dimension(6)    :: drxv,dpdx,dedx
real(pm_reel), dimension(6)    :: dcoef,dddx,dpogo
real(pm_reel), dimension(6)    :: dx1,dy1,dx3,dy3
real(pm_reel), dimension(6)    :: dhx,dhy,dxmdx
real(pm_reel), dimension(6)    :: duu1,duu2,duu3
real(pm_reel), dimension(6)    :: dvv1,dvv2,dvv3

real(pm_reel)  :: r                 !     norme vecteur position 
real(pm_reel)  :: rxv1,rxv2,rxv3,rxv!     coordonnees produit vectoriel positionxvitesse, norme et norme**2
real(pm_reel)  :: hx,hy             !     coordonnees (hx,hy)
real(pm_reel)  :: p                 !     parametre p=(rxv*rxv)/mu

!     termes intermediaires
real(pm_reel)  :: spogo,cpogo
real(pm_reel)  :: x1,x3,y1,y3,rxvc,d
real(pm_reel)  :: cof1,cof2,cof3,cof4
real(pm_reel)  :: coef,coef2
real(pm_reel), dimension(3)  :: uu,vv

type(tm_code_retour) :: code_retour
real(pm_reel)        :: mu_retour

intrinsic present, sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_car_equa_parab.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_car_equa_parab.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

r=norme_vect_pos     !     norme vecteur position

rxv1 = rrxv(1)
rxv2 = rrxv(2)                !     produit vectoriel (position x vitesse) 
rxv3 = rrxv(3)
rxv  = norme_moment_cinetique !     et sa norme

!----------------------------------------------------------------------------------
p      = parametre 
equa%a = p            !     p  =  rxv**2/mu = 2 * distance au perigee

!----------------------------------------------------------------------------------

equa%e = excentricite !     excentricite equa%e = 1._pm_reel

!----------------------------------------------------------------------------------

d     = rscal/rxv                          ! anomalie moyenne
equa%M= (3._pm_reel* d + d**3) / 6._pm_reel! equation de barker : 6 * M = 3*d + d**3 avec d = tan(anomalie vraie /2)

coef2 = ( 1._pm_reel + rxv3/rxv ) / 2._pm_reel
coef  =  sqrt(coef2)
rxvc  =  rxv * coef

hx     = -rxv2 / rxvc
hy     =  rxv1 / rxvc

equa%ix = hx! calcul de hx 
equa%iy = hy! et de hy

!-----------------------------------------------------------------------------------

uu(1) = 1._pm_reel - hy*hy/2._pm_reel
uu(2) = hx*hy/2._pm_reel
uu(3) = - hy * coef

vv(1) = hx*hy/2._pm_reel
vv(2) = 1._pm_reel - hx*hx/2._pm_reel
vv(3) = hx * coef

x1    = (1._pm_reel-d*d)/2._pm_reel
y1    =  d

call mui_dot_product3 ( pos_car , uu , x3 , mu_retour )

call mui_dot_product3 ( pos_car , vv , y3 , mu_retour )

cpogo = x1 * x3 + y1 * y3
spogo = x1 * y3 - y1 * x3

call mu_angle2(cpogo,spogo,equa%pgom,code_retour)! angle (petit omega + grand omega)
if (code_retour%valeur /= pm_OK) then                ! seul code_retour = 0 ou pm_err_vect_nul possible
   retour = pm_err_cni
   go to 6000
end if

if( present(jacob))  then

   !     calcul derivees
   !     ***************

   ! calcul derivees intermediaires
   ! ------------------------------

   drxv1(1) = 0._pm_reel
   drxv1(2) =  vit_car(3)
   drxv1(3) = -vit_car(2)
   drxv1(4) = 0._pm_reel
   drxv1(5) = -pos_car(3)
   drxv1(6) =  pos_car(2)

   drxv2(1) = -vit_car(3)
   drxv2(2) = 0._pm_reel
   drxv2(3) =  vit_car(1)
   drxv2(4) =  pos_car(3)
   drxv2(5) = 0._pm_reel
   drxv2(6) = -pos_car(1)

   drxv3(1) =  vit_car(2)
   drxv3(2) = -vit_car(1)
   drxv3(3) = 0._pm_reel
   drxv3(4) = -pos_car(2)
   drxv3(5) =  pos_car(1)
   drxv3(6) = 0._pm_reel

   drxv(:) = (drxv1(:)*rxv1+drxv2(:)*rxv2+drxv3(:)*rxv3)/rxv

   drscal(1:3)= vit_car(:)   !     derivees produit scalaire r.v
   drscal(4:6)= pos_car(:)

   cof1 = 4._pm_reel*rxv*rxvc
   dcoef(:) = ( rxv*drxv3(:) - rxv3*drxv(:) ) / cof1   !     derivees du coefficient coef=sqrt((1._pm_reel+rxv3/rxv)/2._pm_reel)

   dpdx(:) = 2._pm_reel*rxv*drxv(:)/mu   !  derivees du parametre p

   dedx(:)= 0._pm_reel   ! derivees de l'excentricite e

   cof2 = rxvc * rxvc
   dhx(:) = (-rxv*coef*drxv2(:)+rxv2*rxv*dcoef(:)+rxv2*coef*drxv(:))/ cof2   ! calcul derivees de hx 
   dhy(:) = ( rxv*coef*drxv1(:)-rxv1*rxv*dcoef(:)-rxv1*coef*drxv(:))/ cof2   ! et hy

   !     --------------------------------

   dddx(:) = (drscal(:) - d*drxv(:)) / rxv   !     derivees de d = rscal/rxv

   dx1(:) = - d * dddx(:)   !     derivees de x1 
   dy1(:) = dddx(:)         !     et y1

   duu1(:) = -hy*dhy(:)
   duu2(:) = (hy*dhx(:) + hx*dhy(:))/2._pm_reel   !     derivees de uu 
   duu3(:) = -hy*dcoef(:) - coef*dhy(:)
   dvv1(:) = (hy*dhx(:) + hx*dhy(:))/2._pm_reel
   dvv2(:) = -hx*dhx(:)                           !     et vv
   dvv3(:) =  hx*dcoef(:) + coef*dhx(:)

   dx3(:)= pos_car(1)*duu1(:)+pos_car(2)*duu2(:)+pos_car(3)*duu3(:)   !     derivees de x3 et y3
   dy3(:)= pos_car(1)*dvv1(:)+pos_car(2)*dvv2(:)+pos_car(3)*dvv3(:)

   dx3(1:3) = dx3(1:3) + uu(:)
   dy3(1:3) = dy3(1:3) + vv(:)

   !  derivees de pogo = equa%pgom
   cof4 = (r**4)/(p*p)
   dpogo(:)=(-spogo*(x3*dx1(:)+x1*dx3(:)+y3*dy1(:)+y1*dy3(:))+cpogo*(-y1*dx3(:)-x3*dy1(:)+x1*dy3(:)+y3*dx1(:)))/cof4

   cof3 = (1._pm_reel+ d*d)/2._pm_reel
   dxmdx(:)= cof3 * dddx(:)   ! derivees anomalie moyenne

   ! affectation de la jacobienne de la transformation
   ! -------------------------------------------------

   jacob(1,:)=dpdx(:)

   jacob(2,:)=dedx(:)

   jacob(3,:)=dpogo(:)

   jacob(4,:)=dhx(:)

   jacob(5,:)=dhy(:)

   jacob(6,:)=dxmdx(:) 

end if

6000 continue

end subroutine mvi_car_equa_parab
