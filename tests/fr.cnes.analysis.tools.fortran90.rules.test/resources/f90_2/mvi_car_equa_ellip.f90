subroutine mvi_car_equa_ellip (pos_car, vit_car,rrxv,norme_vect_pos, norme_vect_vit, norme_moment_cinetique,un_sur_a,&
                               excentricite,rscal,mu,equa,retour,jacob)
! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres CARtesiens aux parametres orbitaux adaptes a l'orbite EQUAtoriale
! ===   (a ou p, e, pom+gom, 2sin(i/2)cos(gom), 2sin(i/2)sin(gom), M ) dans le cas ELLIPtique
!
! Note d'utilisation:  -  Routine interne 
! ==================   -  Uniquement appliquable au cas ELLIPtique NON circulaire
!                      -  L'appelant doit verifier que :
!                             1) l'excentricite est dans ]0,1[.
!                             2) 1/a est > 0 (strictement)
!                             3) la constante de la gravitation est > 0
!                             4) la norme du produit vectoriel est non nulle
!
!$Historique
! ==========
!   + Version 1.0 (SP 237 ed01 rev00): creation a partir de la routine MVELCQ de la MSLIB f77
!                         (Date:08/1998 - Realisation: Veronique Lepine)
!   + Version 1.0.1 (FA 293 ed01 rev 00): erreur de dimension a la declaration de "drxv"
!                         (Date: 01/1999 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin) 
!   + Version 6.8 : DM-ID 859 : Suppression de la declaration intrinsic
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : FA-ID 1108 : Ajout de parenthèses dans expression ambigue
!                   (Date: 09/2008 - Realisation: Atos origin)
!!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
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
real(pm_reel),intent(in)                             ::  norme_vect_vit   !  norme du vecteur vitesse 
real(pm_reel),intent(in)                             ::  norme_moment_cinetique   !  norme du produit vectoriel de ces derniers
real(pm_reel),intent(in)                             ::  un_sur_a         ! 1/a
real(pm_reel),intent(in)                             ::  excentricite     !  excentricite
real(pm_reel),intent(in)                             ::  rscal   ! produit scalaire position.vitesse
real(pm_reel),intent(in)                             ::  mu      ! constante de la gravitation

type(tm_orb_equa), intent(out)                       ::  equa    ! parametres orbitaux de l'orbite equatoriale
integer ,intent(out)                                 ::  retour
real(pm_reel),dimension(6,6), intent(out), optional  :: jacob    ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(6)    :: dadx,dedx,danedx,dal1,dal2,dsf,dsg,dmdx,domdx,dhxdx,dhydx,dan,dch,f1,drscal,f2,hdh
real(pm_reel), dimension(6,6)  :: dv
real(pm_reel), dimension(3,6)  :: drxv
real(pm_reel), dimension(3)    :: vv

real(pm_reel)  :: a,anome,cof1,cof2,cose
real(pm_reel)  :: e,ecose,esine,om,r
real(pm_reel)  :: r2,rmua,sine,unme2
real(pm_reel)  :: v,v2,xmoy

real(pm_reel)  :: al1,al2,cofh,drace,dcdv3
real(pm_reel), dimension(6)   :: f, g
real(pm_reel)  :: hx,hy,hx2,hy2
real(pm_reel)  :: r2coso,r2sino,race,rach,rach2
real(pm_reel)  :: rxv,rxv2,scalf,scalg

type(tm_code_retour)  :: code_retour
real(pm_reel)         :: mu_retour

integer        :: i,j ! indices de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_car_equa_ellip.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_car_equa_ellip.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! calculs des variables intermediaires
! ------------------------------------
r    = norme_vect_pos
r2   = r*r
v    = norme_vect_vit
v2   = v*v
rxv  = norme_moment_cinetique
rxv2 = rxv*rxv

a=1._pm_reel/un_sur_a !  demi-grand axe

e = excentricite      ! excentricite

rmua=sqrt(mu*a)
ecose=r*v2/mu-1._pm_reel
esine=rscal/rmua

call mu_angle2(ecose, esine, anome, code_retour)  !   anomalie excentrique
if (code_retour%valeur /= pm_OK) then                 !   le code retour ne peut pas etre non nul
   retour = pm_err_cni
   go to 6000
end if

cose=ecose/e
sine=esine/e

! calcul de hx et hy
! ------------------

unme2=1._pm_reel-e**2
race=sqrt(unme2)

vv(:)=rrxv(:)/rxv

rach2=0.5_pm_reel*(1._pm_reel+vv(3))
rach=sqrt(rach2)
cofh=1._pm_reel/rach

hx=-cofh*vv(2)
hy=cofh*vv(1)

hx2=hx*hx
hy2=hy*hy

f(1) = 1._pm_reel-0.5_pm_reel*hy2
f(2) = 0.5_pm_reel*hx*hy
f(3) = -rach*hy
f(4:6) = 0._pm_reel

g(1) = f(2)
g(2) = 1._pm_reel-0.5_pm_reel*hx2
g(3) = hx*rach
g(4:6) = 0._pm_reel

! calcul de om = (petit omega + grand omega)
!------------------------------------------

call mui_dot_product3 ( pos_car , f(1:3) , scalf , mu_retour )

call mui_dot_product3 ( pos_car , g(1:3) , scalg , mu_retour )

al1=a*(cose-e)
al2=a*race*sine
r2coso=al1*scalf+al2*scalg
r2sino=al1*scalg-al2*scalf

call mu_angle2(r2coso,r2sino,om,code_retour)
if (code_retour%valeur /= pm_OK) then                 !   le code retour ne peut pas etre non nul
   retour = pm_err_cni
   go to 6000
end if

xmoy=anome-esine! anomalie moyenne

! affectation des parametres osculateurs
! --------------------------------------

equa%a    =a
equa%e    =e
equa%pgom =om
equa%ix   =hx
equa%iy   =hy
equa%M    =xmoy

if (present(jacob))  then

   !***************************************
   ! calcul du jacobien de la transformation
   !***************************************

   drace=-e/race

   drxv(3,1)=vit_car(2)
   drxv(3,2)=-vit_car(1)
   drxv(3,3)=0._pm_reel
   drxv(3,4)=-pos_car(2)
   drxv(3,5)=pos_car(1)
   drxv(3,6)=0._pm_reel

   drxv(1,1)=0._pm_reel
   drxv(1,2)=vit_car(3)
   drxv(1,3)=-vit_car(2)
   drxv(1,4)=0._pm_reel
   drxv(1,6)=pos_car(2)
   drxv(1,5)=-pos_car(3)

   drxv(2,1)=-vit_car(3)
   drxv(2,2)=0._pm_reel
   drxv(2,3)=vit_car(1)
   drxv(2,4)=pos_car(3)
   drxv(2,5)=0._pm_reel
   drxv(2,6)=-pos_car(1)

   dan(:) = matmul (rrxv(:),drxv(:,:))/rxv2

   do i=1,3
      do j=1,6
         dv(i,j)=drxv(i,j)/rxv-vv(i)*dan(j)
      end do
   end do

   dcdv3=-0.25_pm_reel*cofh/rach2

   dch(:)=dcdv3*dv(3,:)

   !     derivees du demi-grand axe a

   cof1=(2._pm_reel*a**2)/(r**3)
   cof2=(2._pm_reel*a**2)/mu
   dadx(1:3)=cof1*pos_car(:)
   dadx(4:6)=cof2*vit_car(:)

   cof1=v2/(mu*r)
   cof2=2._pm_reel*r/mu
   f1(1:3)=cof1*pos_car(:)
   f1(4:6)=cof2*vit_car(:)

   drscal(1:3)=vit_car(:)
   drscal(4:6)=pos_car(:)

   f2(:)=(drscal(:)-0.5_pm_reel*rscal*dadx(:)/a)/rmua

   !     derivees de l'excentricite et de l'anomalie excentrique

   dedx(:)=cose*f1(:)+sine*f2(:)
   danedx(:)=(-sine*f1(:)+cose*f2(:))/e

   !     derivees de hx et hy

   dhxdx(:)=-dch(:)*vv(2)-cofh*dv(2,:)
   dhydx(:)=dch(:)*vv(1)+cofh*dv(1,:)

   !     derivees de om

   dal1(:)=dadx(:)*(cose-e)+a*(-sine*danedx(:)-dedx(:))
   dal2(:)=dadx(:)*race*sine+a*(drace*dedx(:)*sine+race*cose*danedx(:))

   hdh(:)=(hx*dhxdx(:)+hy*dhydx(:))/4._pm_reel

   dsf(:)=pos_car(1)*(-hy*dhydx(:))+pos_car(2)*0.5_pm_reel*(hx*dhydx(:)+hy*dhxdx(:)) &
         -pos_car(3)*(rach*dhydx(:)-hy*hdh(:)/rach)+f(:)
   dsg(:)=pos_car(1)*0.5_pm_reel*(hx*dhydx(:)+hy*dhxdx(:))+pos_car(2)*(-hx*dhxdx(:)) &
         +pos_car(3)*(rach*dhxdx(:)-hx*hdh(:)/rach)+g(:)

   domdx(:)=(scalf*dsg(:)-scalg*dsf(:)+al2*dal1(:)-al1*dal2(:))/r2

   !     derivees de l'anomalie moyenne

   dmdx(:)=danedx(:)*(1._pm_reel-ecose)-sine*dedx(:)

   ! affectation de la jacobienne de la transformation
   ! -------------------------------------------------

   jacob(1,:)=dadx(:)
   jacob(2,:)=dedx(:)
   jacob(3,:)=domdx(:)
   jacob(4,:)=dhxdx(:)
   jacob(5,:)=dhydx(:)
   jacob(6,:)=dmdx(:)

end if

6000 continue

end subroutine mvi_car_equa_ellip
