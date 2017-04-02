subroutine mv_car_cir (mu, pos_car, vit_car, cir, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres CARtesiens aux parametres orbitaux dits adaptes aux orbites circulaires
! ===   non equatoriales. (x,y,z,x',y',z') --> (a, e cos (pom), e sin(pom), i, gom, pom+M)
!
! Note d'utilisation: 0) Applicable aux orbites circulaires et elliptiques, NON equatoriales.
! ==================  1) Pour les orbites hyperboliques ou paraboliques, voir la routine mv_car_kep.
!                     2) Pour les orbites circulaires et/ou elliptiques ET equatoriales, voir mv_car_cir_equa.
!                     3) Les unites en entree doivent etre coherentes entre elles. Ex.: pos_car en metres, vit_car en m/s,
!                        mu en m**3/s**2, et les parametres orbitaux dits adaptes aux orbites circulaires seront en metres
!                        et radians.
!                     4) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre adapte aux orbites
!                        circulaires numero i par rapport a la derivee partielle du parametre cartesien numero j.
!                     5) La transformation inverse se fait par mv_cir_car.
!
!$Historique
! ==========
!   + Version 1.0 (SP 234 ed01 rev00): creation a partir de la routine MVREDC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 3.0 (FA 432 ed01 rev00) : initialisation a 0 de tableau de travail
!                         (Date 10/2000 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : suppression de la declaration intrinsic
!                   (Date: 03/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_norme
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
                                                       
real(pm_reel), intent(in)              :: mu  ! constante de la gravitation universelle
real(pm_reel), dimension(3), intent(in):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(in):: vit_car  ! vecteur vitesse du satellite
type(tm_orb_cir),intent(out)           :: cir      ! parametres orbitaux de l'orbite circulaire
type(tm_code_retour), intent(out)      ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(6)     :: drsx,dvsx,dt1sx,dt2sx,dsrrp,drhx,drlx,dc1x,dc2x,dan,dadx,dex,dey
real(pm_reel), dimension(6)     :: dcle,dsle,dle,dusi,dla,dmu,dbeta,dt2b
real(pm_reel), dimension(3)     :: rrp, w
real(pm_reel), dimension(3,6)   :: dwdx,drrp

real(pm_reel)   :: a,anorm,asr2,c1,c2,ci,cle,cof1,cof2,beta
real(pm_reel)   :: crl,runmt1
real(pm_reel)   :: cotinc,ex,ey,omega,r,r2,r3,rdunsa,rh,rl
real(pm_reel)   :: si,sle,srrp,t1,t2,tau2,un,unmt1,unsan,unsm,eps100
real(pm_reel)   :: unsr,unsra,unsv,usi,v,v2,xi,crh
real(pm_reel)   :: deuxt1,deuxt2,si2,beta2,t2beta,c1sr2,ac1,xle,xlm
real(pm_reel)   :: tc2sr2,at2c2,ac2sr2,coef,m2sr3,deuxrv,usi2
real(pm_reel)   :: retour

integer :: i ! indices de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mv_car_cir.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_car_cir.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

call mu_norme(pos_car, r, code_retour) ! calcul du rayon vecteur r
if (r < eps100) then                   ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if
r2=r*r                                 ! carre du rayon vecteur r2

call mu_norme(vit_car, v, code_retour) ! norme de la vitesse v
if (v < eps100) then                   ! vecteur vitesse nul
   code_retour%valeur = pm_err_vit_nul
   go to 6000
end if
v2=v*v                                 ! carre de la norme de la vitesse v2

if (mu <= eps100) then
   if (mu < 0._pm_reel) then ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else                      ! constante de la gravitation nulle
      code_retour%valeur = pm_err_mu_nul
   end if
   go to 6000
end if
      
unsm=1._pm_reel/mu
t1=r*v2*unsm-1._pm_reel! calcul de t1=r*v2/mu - 1  avec t1 < 1 pour une orbite circulaire ou elliptique
                       !                           t1 = excentricite* cos(anom. excentrique)

if (t1 > (1._pm_reel-pm_eps_parab)) then       !        condition non suffisante pour verifier la non ellipticite 
   code_retour%valeur = pm_err_e_non_ellip  !        mais assure un demi-grand axe > 0 et la definition de unmt1
   go to 6000
end if

unmt1=1._pm_reel/(1._pm_reel-t1)
a=r*unmt1! calcul du demi-grand axe

!======
cir%a=a
!======

call mu_prod_vect(pos_car,vit_car,rrp, code_retour)   ! rrp(3) = produit vectoriel position * vitesse
call mu_norme(rrp, anorm, code_retour,vect_norme = w) ! calcul de w (produit vectoriel norme position-vitesse)

if (anorm < eps100) then      ! le vecteur vitesse et le vecteur position sont colineaires
   code_retour%valeur = pm_err_pos_vit_colineaire
   go to 6000
end if

unsan=1._pm_reel/anorm

! calcul de l'inclinaison
!     -----------------------

si2=w(1)*w(1)+w(2)*w(2)             !     >0 par calcul
si=sqrt(si2)
ci=w(3)
call mu_angle2(ci,si,xi,code_retour)!     code retour = 0 car (w(1)**2+w(2)**2) et w(3) differents de 0 
                                    !     en meme temps car anorm different de 0

if (sin(xi) < pm_eps_equa) then          !     l'orbite est equatoriale 
   code_retour%valeur = pm_err_i_equa
   go to 6000
end if

!======
cir%i=xi
!======

call mu_angle2(-w(2),w(1),omega,code_retour)   ! calcul de l'ascension droite du noeud ascendant
                                               ! code_retour%valeur = 0 sinon w(1)=w(2)=0 => sin(i)=0 => orb equatoriale interdite

!============
cir%gom=omega
!============

! calcul du vecteur excentricite
! ------------------------------

usi=1._pm_reel/si
crh=-pos_car(1)*w(2)+pos_car(2)*w(1)
crl=-pos_car(1)*w(1)-pos_car(2)*w(2)
rh=usi*crh
rl=w(3)*usi*crl+pos_car(3)*si

!     produit scalaire position.vitesse
call mui_dot_product3 ( pos_car , vit_car , srrp , retour )

unsra=1._pm_reel/sqrt(mu*a)
t2=unsra*srrp  ! calcul de t2 = produit scalaire (position * vitesse)/sqrt(rmu*a) = excentricite * sin(anom. excentrique)
               !     on ne teste pas t2: voir test sur tau2 ci-dessous

tau2=t1*t1+t2*t2    !     excentricite**2 = t1**2 + t2**2
if (sqrt(tau2) > (1._pm_reel - pm_eps_parab)) then   !        orbite parabolique ou hyperbolique 
   code_retour%valeur = pm_err_e_non_ellip
   go to 6000
end if

c1=t1-tau2
c2=sqrt(1._pm_reel-tau2)!     c2 non nul car tau2 < 1
asr2=a/r2
ac2sr2=asr2*c2
cof1=c1*asr2
cof2=ac2sr2*t2

ex=cof1*rh+cof2*rl
ey=cof1*rl-cof2*rh

!========
cir%ex=ex             
!========
!     vecteur excentricite
!========
cir%ey=ey
!========

! calcul de la longitude moyenne
!     ------------------------------

beta=1._pm_reel/(1._pm_reel+c2)
t2beta=t2*beta
cle=rh/a+ex-t2beta*ey
sle=rl/a+ey+t2beta*ex

call mu_angle2(cle,sle,xle,code_retour)   !  calcul de (petit omega + e)
if (code_retour%valeur < pm_OK)  then
   code_retour%valeur = pm_err_cni
   go to 6000
end if

xlm=xle-t2!     calcul de la longitude moyenne (petit omega + m)

!============
cir%pso_M=xlm
!============

!********************************************
! calcul du jacobien de la transformation
!********************************************

if (present(jacob))  then

   unsr=1._pm_reel/r
   drsx(1:3)=unsr*pos_car(:)   ! calcul des derivees de r
   drsx(4:6)= 0._pm_reel       ! mise a zero de la partie non utile

   unsv=1._pm_reel/v
   dvsx(4:6)=unsv*vit_car(:)   ! calcul des derivees de v
   dvsx(1:3)= 0._pm_reel       ! mise a zero de la partie non utile

   deuxrv = 2._pm_reel*r*v
   dt1sx(:)=unsm*(drsx(:)*v2+deuxrv*dvsx(:))         ! calcul des derivees de t1=r*v2/mu

   runmt1=r*unmt1*unmt1
   dadx(:)   =unmt1*drsx(:)+dt1sx(:)*runmt1 
   jacob(1,:)=dadx(:)           ! calcul des derivees du demi-grand axe

   drrp(:,:)=0._pm_reel

   drrp(1,2)= vit_car(3)
   drrp(1,3)=-vit_car(2)
   drrp(1,5)=-pos_car(3)
   drrp(1,6)= pos_car(2)

   drrp(2,1)=-vit_car(3)    ! calcul des derivees de w= produit vectoriel norme position*vitesse
   drrp(2,3)= vit_car(1)    !     rrp(3) = produit vectoriel position * vitesse
   drrp(2,4)= pos_car(3)    !     drrp(3,6) = derivees du produit vectoriel position * vitesse
   drrp(2,6)=-pos_car(1)

   drrp(3,1)= vit_car(2)
   drrp(3,2)=-vit_car(1)
   drrp(3,4)=-pos_car(2)
   drrp(3,5)= pos_car(1)

   
   dan(:) = matmul(rrp(:), drrp(:,:))

   do  i=1,6
      dwdx(:,i)=unsan*(drrp(:,i)-unsan*unsan*dan(i)*rrp(:))
   end do

   cotinc = ci/si         !     sinus de l'inclinaison non nulle car orbite non equatoriale
   !     remarque: cosinus de l'inclinaison = 0 si orbite polaire

   jacob(4,:) = cotinc*(w(1)*dwdx(1,:)+w(2)*dwdx(2,:)) - si*dwdx(3,:)         ! calcul des derivees de l'inclinaison
   jacob(5,:) = (w(1)*dwdx(2,:) - w(2)*dwdx(1,:))/si2            ! calcul des derivees de l'ascension droite du noeud ascendant

   usi2 = usi*usi

   dusi(:) = -(w(1)*dwdx(1,:)+w(2)*dwdx(2,:))*usi2
   drhx(:) = usi*(dusi(:)*crh - pos_car(1)*dwdx(2,:) + pos_car(2)*dwdx(1,:))               
   drlx(:) = usi*((dwdx(3,:) + w(3)*dusi(:))*crl - w(3)*(pos_car(1)*dwdx(1,:) + pos_car(2)*dwdx(2,:)) + &
             pos_car(3)*(w(1)*dwdx(1,:)+w(2)*dwdx(2,:)))

   drhx(1)=drhx(1)-usi*w(2)
   drhx(2)=drhx(2)+usi*w(1)
   drlx(1)=drlx(1)-w(3)*usi*w(1)
   drlx(2)=drlx(2)-w(3)*usi*w(2)
   drlx(3)=drlx(3)+si

   dsrrp(1:3)= vit_car(:)                  !     derivees du produit scalaire position.vitesse
   dsrrp(4:6)= pos_car(:)

   rdunsa=-0.5_pm_reel*t2/a
   dt2sx(:)=unsra*dsrrp(:)+rdunsa*dadx(:)   !     derivees de t2

   deuxt1=2._pm_reel*t1
   deuxt2=2._pm_reel*t2
   r3=r2*r
   m2sr3=-2._pm_reel/r3
   c1sr2=c1/r2
   ac1=a*c1
   tc2sr2=t2*c2/r2
   at2c2=a*t2*c2
   coef=-0.5_pm_reel*asr2*t2/c2

   dc1x(:) = dadx(:)*c1sr2 + ac1*m2sr3*drsx(:) + asr2*(dt1sx(:)-deuxt1*dt1sx(:) - deuxt2*dt2sx(:))
   dc2x(:) = dadx(:)*tc2sr2 + at2c2*m2sr3*drsx(:)+ ac2sr2*dt2sx(:) + coef*(deuxt1*dt1sx(:) + deuxt2*dt2sx(:))

   dex(:)=dc1x(:)*rh +cof1*drhx(:) +dc2x(:)*rl +cof2*drlx(:)! calcul des derivees du vecteur excentricite
   dey(:)=dc1x(:)*rl +cof1*drlx(:) -dc2x(:)*rh -cof2*drhx(:)
   jacob(2,:) = dex(:)
   jacob(3,:) = dey(:)

   beta2=beta*beta

   dla(:) =(drhx(:)-rh*dadx(:)/a)/a
   dmu(:) =(drlx(:)-rl*dadx(:)/a)/a
   dbeta(:) = (t1*dt1sx(:)+dt2sx(:)*t2)*beta2/c2
   dt2b(:) = dt2sx(:)*beta+t2*dbeta(:)
   dcle(:)= dla(:)+dex(:)-t2beta*dey(:)- dt2b(:)*ey
   dsle(:)= dmu(:)+dey(:)+t2beta*dex(:)+ dt2b(:)*ex
   un=cle*cle+sle*sle
   dle(:)=(cle*dsle(:)-sle*dcle(:))/un

   jacob(6,:)=dle(:)-dt2sx(:)                              ! calcul des derivees de la longitude moyenne

end if

6000 continue

code_retour%routine = pm_num_mv_car_cir
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_car_cir
