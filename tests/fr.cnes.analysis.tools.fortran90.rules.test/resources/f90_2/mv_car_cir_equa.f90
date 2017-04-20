subroutine mv_car_cir_equa (mu, pos_car, vit_car, cir_equa, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres CARtesiens aux parametres orbitaux dits adaptes aux orbites CIRculaires EQUAtoriales.
! ===
!
! Note d'utilisation: 1) Applicable aux orbites circulaires ou elliptiques, equatoriales ou non
! ==================  2) Dans le cas d'orbites paraboliques ou hyperboliques  voir la routine:  mv_car_equa
!                     3) Les unites en entree doivent etre coherentes entre elles. Ex.: pos_car en metres, vit_car en m/s,
!                        mu en m**3/s**2, et les parametres orbitaux dits adaptes aux orbites CIRculaires EQUAtoriales
!                        seront en metres et radians.
!                     4) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre adaptes aux orbites
!                        CIRculaires EQUAtoriales numero i par rapport a la derivee partielle du parametre cartesien numero j.
!                     5) La transformation inverse se fait par mv_cir_equa_car.
!$Historique
! ==========
!   + Version 1.0 (SP 244 ed01 rev00): creation a partir de la routine MVRECE de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 358 ed01 rev00): ajout du test sur i**2 > 4*(1 - 100*epsilon(1_pm_reel))
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin) 
!   + Version 6.8 : DM-ID 859 : Suppression de la declaration intrinsic
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.9 : FA-ID 1108 : Ajout de parenthèses dans expression ambigue
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_norme
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_angle2
use int_util_internes, only : mui_dot_product3

use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)              :: mu                 ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car            ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car            ! vecteur vitesse du satellite
type(tm_orb_cir_equa),intent(out)      :: cir_equa           ! parametres orbitaux de l'orbite circulaire equatoriale
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)   ::    eps100  ! epsilon de test

real(pm_reel), dimension(6)     ::    dadx,dexdx,deydx,dhxdx,dhydx,dmdx,drdx,decose,desine,dch,dsf,dsg,dan,dce,hdh
real(pm_reel), dimension(6)     ::    de2,drace,dal1,dal2,dbeta,dl2,dm2,dt2b,dcle,dsle,dxle, f, g
real(pm_reel), dimension(3)     ::    vect,v
real(pm_reel), dimension(3,6)   ::    dvect,dv

real(pm_reel)   ::    a,al1,al2,anorm,anorm2,beta,cle,cofh
real(pm_reel)   ::    e,e2,ecose,esine,ex,exsc,ey,dcdv3
real(pm_reel)   ::    eysc,hx,hx2,hy,hy2,r,r2,race
real(pm_reel)   ::    rach,rach2,rmua,rsa,scal,scalf,scalg,sle,un_sur_a,v2
real(pm_reel)   ::    xle,xlm,vitesse
real(pm_reel)   ::    retour

integer         ::    i, j   ! indices de boucle d'affectation de tableaux

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_car_cir_equa.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_car_cir_equa.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

if (mu <= eps100) then
   if (mu < 0._pm_reel) then              ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul  ! constante de la gravitation nulle
   end if
   go to 6000
end if

call mu_norme(pos_car, r, code_retour)
r2 = r*r
if (r < eps100)  then   ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

! calcul du demi-grand axe
! ------------------------

call mu_norme (vit_car, vitesse, code_retour)
v2 = vitesse*vitesse 

un_sur_a=2._pm_reel/r-v2/mu
! 1/a = 2/r - v**2/mu   ==>  test si 1/a est negatif ou nul
! pour savoir si l'orbite est hyperbolique (v2>2*mu/r) ou parabolique (v2= 2*mu/r)

if (un_sur_a < eps100) then 
   if (un_sur_a < 0._pm_reel) then ! 1/a negatif -> a negatif (cas hyperbolique)
      code_retour%valeur = pm_err_a_negatif
   else                            ! 1/a proche de 0 -> a infini (cas parabolique)
      code_retour%valeur = pm_err_a_infini
   end if
   go to 6000
end if

a=1._pm_reel/un_sur_a
! a ne peut pas etre negatif car 1/a a deja ete teste en ce sens
! a ne peut pas etre nul non plus, car cela signifierait que r est nul, et r a deja ete teste en ce sens

! calcul de l'excentricite
! ------------------------

call mui_dot_product3 ( pos_car , vit_car , scal , retour )

rmua=sqrt(mu*a)
esine=scal/rmua

rsa=r*un_sur_a
ecose=1._pm_reel-rsa

e2=ecose*ecose+esine*esine   ! par calcul e2 est positif ou nul

! test aux limites sur l'excentricite (que l'on sait deja inferieure a 1)
e = sqrt(e2)
if (e > 1._pm_reel - pm_eps_parab) then  ! orbite trop excentrique, quasi parabolique
   code_retour%valeur = pm_err_e_non_ellip
end if

! calcul des parametres hx et hy
! ------------------------------
call mu_prod_vect(pos_car, vit_car, vect, code_retour)     !     moment cinetique
call mu_norme (vect, anorm, code_retour, vect_norme = v)   !     norme et direction du moment cinetique

if (anorm < eps100)  then                                  !     la norme du moment cinetique est nulle
   code_retour%valeur = pm_err_pos_vit_colineaire
   go to 6000
end if

rach2=0.5_pm_reel*(1._pm_reel+v(3))
rach=sqrt(rach2)
cofh=1._pm_reel/rach

hx=-cofh*v(2)
hy=cofh*v(1)

if ((hx**2+hy**2) > (4._pm_reel*(1._pm_reel-eps100))) then ! la definition du vecteur inclinaison (hx,hy) impose que l'inclinaison i soit
   code_retour%valeur = pm_err_i_equa_retro                ! inferieure a (pi - epsilon_retro) -> test si h**2 <= 4*(cos(epsilon_retro/2)**2)
   go to 6000                                              ! soit h**2 <= 4*(1-100*epsilon) si epsilon_retro = 20*sqrt(epsilon)
end if

! calcul des parametres ex et ey
! ------------------------------

hx2=hx*hx
hy2=hy*hy

f(1)=1._pm_reel-0.5_pm_reel*hy2
f(2)=0.5_pm_reel*hx*hy
f(3)=-rach*hy
f(4:6) = 0._pm_reel

g(1)=f(2)
g(2)=1._pm_reel-0.5_pm_reel*hx2
g(3)=    hx*rach
g(4:6) = 0._pm_reel

call mui_dot_product3 ( pos_car , f(1:3) , scalf , retour )

call mui_dot_product3 ( pos_car , g(1:3) , scalg , retour )

race=sqrt(1._pm_reel-e2)               !     race est different de zero car e2 < 1
al1=ecose-e2
al2=race*esine

exsc=    al1*scalf+al2*scalg
eysc=    al1*scalg-al2*scalf

ex=exsc*a/r2
ey=eysc*a/r2

! calcul du parametre xlm
! -----------------------

beta=1._pm_reel/(1._pm_reel+race)!     race est > 0

cle=scalf/a+ex-esine*beta*ey
sle=scalg/a+ey+esine*beta*ex

!     calcul de (petit omega + grand omega + e)
call mu_angle2(cle, sle, xle, code_retour)
if (code_retour%valeur  <  pm_OK) then
   code_retour%valeur = pm_err_cni
   go to 6000
end if

!     calcul de la longitude moyenne (petit omega + grand omega + m)
xlm=xle-esine

! ------------------------------------
! affectation des parametres osculateurs
! --------------------------------------
cir_equa%a = a
cir_equa%ex = ex
cir_equa%ey = ey
cir_equa%ix = hx
cir_equa%iy = hy
cir_equa%pso_M = xlm

if (present(jacob)) then    ! calcul de la jacobienne

   dadx(1:3)=2._pm_reel*a*a*pos_car(:)/r**3
   dadx(4:6)=2._pm_reel*a*a*vit_car(:)/mu

   drdx(1:3)=pos_car(:)/r
   drdx (4:6)=0._pm_reel

   decose(:)=un_sur_a*(-drdx(:)+dadx(:)*rsa)
   desine(1:3)=-0.5_pm_reel*esine*un_sur_a*dadx(1:3)+vit_car(:)/rmua
   desine(4:6)=-0.5_pm_reel*esine*un_sur_a*dadx(4:6)+pos_car(:)/rmua

   dce(:)=(dadx(:)/r2)-(((2._pm_reel*a)/r2)/r)*drdx(:)
   
   dvect(3,1)=vit_car(2)
   dvect(3,2)=-vit_car(1)
   dvect(3,3)=0._pm_reel
   dvect(3,4)=-pos_car(2)
   dvect(3,5)=pos_car(1)
   dvect(3,6)=0._pm_reel

   dvect(1,1)=0._pm_reel
   dvect(1,2)=vit_car(3)
   dvect(1,3)=-vit_car(2)
   dvect(1,4)=0._pm_reel
   dvect(1,6)=pos_car(2)
   dvect(1,5)=-pos_car(3)

   dvect(2,1)=-vit_car(3)
   dvect(2,2)=0._pm_reel
   dvect(2,3)=vit_car(1)
   dvect(2,4)=pos_car(3)
   dvect(2,5)=0._pm_reel
   dvect(2,6)=-pos_car(1)

   anorm2= anorm * anorm 
   dan(:)= matmul(vect, dvect)/anorm2 

   do  i=1,3
      do  j=1,6
         dv(i,j)=dvect(i,j)/anorm-v(i)*dan(j)
      end do
   end do

   dcdv3=-0.25_pm_reel*cofh/rach2
   dch(:)=dcdv3*dv(3,:)
   dhxdx(:)=-dch(:)*v(2)-cofh*dv(2,:)
   dhydx(:)=dch(:)*v(1)+cofh*dv(1,:)

   hdh(:)=(hx*dhxdx(:)+hy*dhydx(:))*0.25_pm_reel
   dsf(:)=pos_car(1)*(-hy*dhydx(:))+pos_car(2)*0.5_pm_reel*(hx*dhydx(:)+hy*dhxdx(:))-pos_car(3)*(rach*dhydx(:)-hy*hdh(:)/rach)+f(:)
   dsg(:)=pos_car(1)*0.5_pm_reel*(hx*dhydx(:)+hy*dhxdx(:))+pos_car(2)*(-hx*dhxdx(:))+pos_car(3)*(rach*dhxdx(:)-hx*hdh(:)/rach)+g(:)

   de2(:)=2._pm_reel*(decose(:)*ecose+desine(:)*esine)
   drace(:)=-0.5_pm_reel*de2(:)/race

   dal1(:)=decose(:)-de2(:)
   dal2(:)=drace(:)*esine+race*desine(:)

   dexdx(:) = dce(:)*exsc+a/r2*(dal1(:)*scalf+al1*dsf(:)+dal2(:)*scalg+al2*dsg(:))
   deydx(:) = dce(:)*eysc+a/r2*(dal1(:)*scalg+al1*dsg(:)-dal2(:)*scalf-al2*dsf(:))

   dbeta(:)=-drace(:)*beta*beta

   dl2(:)=dsf(:)/a-scalf*dadx(:)/(a*a)
   dm2(:)=dsg(:)/a-scalg*dadx(:)/(a*a)

   dt2b(:)=desine(:)*beta+esine*dbeta(:)

   dcle(:)=dl2(:)+dexdx(:)-(dt2b(:)*ey+esine*beta*deydx(:))
   dsle(:)=dm2(:)+deydx(:)+(dt2b(:)*ex+esine*beta*dexdx(:))

   dxle(:)=cle*dsle(:)-sle*dcle(:)

   dmdx(:)=dxle(:)-desine(:)

   jacob(1,:)=dadx(:)
   jacob(2,:)=dexdx(:)
   jacob(3,:)=deydx(:)
   jacob(4,:)=dhxdx(:)
   jacob(5,:)=dhydx(:)
   jacob(6,:)=dmdx(:)

end if

6000 continue

code_retour%routine = pm_num_mv_car_cir_equa
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_car_cir_equa
