subroutine mvi_car_kep_hyperb (pos_car,vit_car,moment_cinetique,norme_vect_pos,norme_vect_vit,norme_moment_cinetique,&
                               un_sur_a,excentricite, parametre,pos_fois_vit,mu,kep,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But: Passage des coordonnees CARtesiennes aux coordonnees KEPleriennes dans le cas HYPERBolique, avec calcul 
! ===  optionnel de la jacobienne.
!
! Note d'utilisation: - Routine interne. 
! ==================  - L'appelant doit tester que :
!                           1) l'orbite est hyperbolique : e > 1 (strictement)
!                           2) la constante de la gravitation est > 0 (strictement)
!                           3) la norme du moment cinetique est non nulle
!                           4) 1/a est > 0 (strictement)
!
!$Historique
! ==========
!   + Version 1.0 (SP 228 ed01 rev00): creation a partir de la routine MVHYCO de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
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
use test_mslib
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use int_utilitaires, only : mu_angle2

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3),intent(in)     :: pos_car, vit_car ! positions vitesses en coordonnees cartesiennes
real(pm_reel), dimension(3),intent(in)     :: moment_cinetique ! moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_vit   ! norme de la vitesse
real(pm_reel),intent(in)                   :: norme_moment_cinetique   ! norme du moment cinetique du satellite
real(pm_reel),intent(in)                   :: norme_vect_pos   ! rayon vecteur
real(pm_reel),intent(in)                   :: un_sur_a         ! inverse du demi grand axe
real(pm_reel),intent(in)                   :: parametre        ! parametre de l'hyperbole
real(pm_reel),intent(in)                   :: excentricite     ! excentricite
real(pm_reel),intent(in)                   :: pos_fois_vit     ! produit scalaire position x vitesse
real(pm_reel),intent(in)                   :: mu               ! constante de la gravitation

type(tm_orb_kep),intent(out)               :: kep              ! parametres osculateurs
integer ,intent(out)                       ::  retour
real(pm_reel), dimension(6,6), intent(out), optional  :: jacob ! jacobienne                  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real(pm_reel)  :: eps_equa   ! epsilon de test pour l'orbite equatoriale
real(pm_reel)  :: deuxpi     ! 2*pi

!     tableaux derivees intermediaires
real(pm_reel), dimension(6)  :: drdx ,dvdx ,dpos_fois_vit 
real(pm_reel), dimension(6)  :: drxv1 ,drxv2 ,drxv3 
real(pm_reel), dimension(6)  :: drxv ,dpdx ,dpovdx ,davdx 
real(pm_reel), dimension(6)  :: daux ,dbux ,dcux ,dacpov ,dresav 
real(pm_reel), dimension(6)  :: dadx ,dedx ,dxidx 
real(pm_reel), dimension(6)  :: dpodx ,dgodx ,dxmdx 

real(pm_reel)  :: r,v       !     norme vecteurs position et vitesse
!     coordonnees produit vectoriel position x vitesse, norme, norme**2
real(pm_reel)  :: rxv1,rxv2,rxv3,rxv,rxvn2,nrxv1,nrxv2,nrxv3
real(pm_reel)  :: p         !     parametre p=(rxv*rxv)/mu 
real(pm_reel)  :: po,pov,av !     variables angles intermediaires

!     variables calculs sinus-cosinus d'angles
real(pm_reel)  :: rxvsxi,cxi,sxi,rsicgo,rsisgo
real(pm_reel)  :: aspov,acpov,recav,resav

!     termes intermediaires
real(pm_reel)  :: rusamu,bux,cux,dux,cof,cof1,cof2,rxvsmu,scal2a
real(pm_reel)  :: rxvsi2,rsxi2,re2,psura,sur2ea,demimu
real(pm_reel)  :: scasbu,unsbux,somme

type(tm_code_retour) :: code_retour

intrinsic sqrt, modulo, tiny, log, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB mvi_car_kep_hyperb.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_car_kep_hyperb.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

deuxpi = pm_deux_pi    ! recuperation de la valeur de 2*pi
r=norme_vect_pos       ! norme vecteur position 
v=norme_vect_vit       ! norme vecteur vitesse 

rxv1 = moment_cinetique(1)!
rxv2 = moment_cinetique(2)!     produit vectoriel (position x vitesse)
rxv3 = moment_cinetique(3)!     

rxv  = norme_moment_cinetique!     norme du vecteur produit vectoriel (position x vitesse)

rxvn2= rxv*rxv

nrxv1 = rxv1 / rxv
nrxv2 = rxv2 / rxv
nrxv3 = rxv3 / rxv

p    = parametre    !     parametre p=(rxv*rxv)/mu=parametre ( donc > 0 )

! demi-grand axe
! ==============

kep%a = 1._pm_reel / un_sur_a

! excentricite
! ============

kep%e = excentricite

! inclinaison sur le plan equatorial
! ==================================

rxvsxi = sqrt(rxv1*rxv1+rxv2*rxv2)!    avec rxvsxi non nul si inclinaison non nulle
sxi = rxvsxi/rxv                  !    sinus(inclinaison)

eps_equa=pm_eps_equa
if (sxi < eps_equa) then          !    orbite equatoriale
   retour = pm_err_i_equa
   go to 6000
end if

cxi = nrxv3                       !    cosinus(inclinaison)
call mu_angle2(cxi,sxi,kep%i,code_retour)!    calcul de l'inclinaison par appel a mu_angle2
                                         !    (resultat dans [0,pi] car sin(inclinaison) > 0)
if (code_retour%valeur == pm_err_vect_nul)  then
   retour = pm_err_cni   
   go to 6000
end if

! longitude du noeud ascendant
! ============================

rsicgo = - rxv2
rsisgo =   rxv1

call mu_angle2(rsicgo,rsisgo,kep%gom,code_retour)
if (code_retour%valeur == pm_err_vect_nul)  then
   retour = pm_err_cni   
   go to 6000
end if

! argument du perigee
! ===================

!     perigee + anomalie vraie
aspov = pos_car(3)
acpov = nrxv1*pos_car(2)-nrxv2*pos_car(1)
call mu_angle2(acpov, aspov, pov, code_retour)
if (code_retour%valeur == pm_err_vect_nul)  then
   retour = pm_err_cni   
   go to 6000
end if

recav  = p - r

if (p < 0._pm_reel) then  ! parametre negatif 
      retour = pm_err_cni
      go to 6000
end if

resav  = pos_fois_vit*sqrt(p/mu)

call mu_angle2(recav, resav, av, code_retour)!     anomalie vraie

if (code_retour%valeur == pm_err_vect_nul)  then
   retour = pm_err_cni   
   go to 6000
end if

po     = pov - av
kep%pom = modulo(po,deuxpi)!     d'ou l'argument du perigee

! anomalie moyenne
!=================
!   calculs intermediaires

rusamu = sqrt(un_sur_a/mu)

bux = sqrt(rxvn2 + mu/un_sur_a)!     rxvn2 est non nul et (mu et un_sur_a) sont > 0 donc bux non nul
unsbux = 1._pm_reel/bux

cux = pos_fois_vit*unsbux!   sinus hyperbolique de l'anomalie excentrique hyperbolique: sinh(h)
                         !     remarque: pos_fois_vit peut etre <= 0 

dux = (1._pm_reel + r*un_sur_a) / excentricite!   cosinus hyperbolique de l'anomalie excentrique hyperbolique: cosh(h)
!     remarque: e > 1 et a non nul donc division possible
!     de plus cosh(h) > 0 compte tenu des parametres de calcul

somme = cux + dux
if (somme <= tiny(somme)) then     !   verifications numeriques
   retour = pm_err_cni             !     sinh(h) + cosh(h) = sinh(h) + sqrt(1+sinh(h)**2) 
   go to 6000                      !     est donc > 0 strictement (mathematiquement).
end if                             !     pour eviter des problemes numeriques: test si cette somme est <=0

kep%M= pos_fois_vit * rusamu - log(somme)!   calcul de l'anomalie moyenne

! Jacobienne
!===========
if (present(jacob))  then  ! requete de calcul de la jacobienne

   !     calcul derivees
   !     ***************

   ! calcul derivees intermediaires
   ! ------------------------------

   ! derivees normes vecteurs position, vitesse, et r x v
   drdx (1:3) = pos_car(:)/r
   dvdx (1:3) = 0._pm_reel
   drdx (4:6) = 0._pm_reel
   dvdx (4:6) = vit_car(:)/v

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

   drxv(:) = drxv1(:)*nrxv1+drxv2(:)*nrxv2+drxv3(:)*nrxv3

   ! derivees produit rscalaire
   dpos_fois_vit(1:3)= vit_car(:)
   dpos_fois_vit(4:6)= pos_car(:)

   ! derivees du parametre p
   rxvsmu = 2._pm_reel * rxv / mu
   dpdx(:) = rxvsmu*drxv(:)

   ! calcul derivees demi-grand axe a
   ! --------------------------------
   cof = -2._pm_reel/(un_sur_a*un_sur_a)
   cof1 = cof/(r*r)
   cof2 = cof*v/mu
   dadx(1:3) = cof1*drdx(1:3)
   dadx(4:6) = cof2*dvdx(4:6)

   ! calcul derivees de l'excentricite e
   ! -----------------------------------

   sur2ea = 0.5_pm_reel*un_sur_a/excentricite
   !     avec excentricite > 1.
   psura = p*un_sur_a
   dedx(:)= (dpdx(:) - psura*dadx(:))*sur2ea

   ! calcul derivees inclinaison
   ! ---------------------------

   dxidx(:) = (cxi*drxv(:) - drxv3(:))/rxvsxi

   ! calcul derivees noeud ascendant
   ! --------------------------------

   rxvsi2 = rxvsxi*rxvsxi
   dgodx(:) = (rxv1*drxv2(:) - rxv2*drxv1(:))/rxvsi2

   ! calcul derivees perigee
   ! -----------------------
   !     po = pov - av --> dpodx(i)=dpovdx(i)-davdx(i)

   !     r*sxi * spov = aspov = pos_car(3)
   !     r*sxi * cpov = acpov

   rsxi2 = (r*sxi)**2
   !     avec rsxi2 non nul si inclinaison non nulle et r non nul
   dacpov(:) = (drxv1(:)*pos_car(2) - drxv2(:)*pos_car(1) - acpov * drxv(:) )/ rxv             
   dacpov(1)  = dacpov(1) - nrxv2
   dacpov(2)  = dacpov(2) + nrxv1

   dpovdx(:) = -pos_car(3)*dacpov(:)/rsxi2
   dpovdx(3) = dpovdx(3) + acpov/rsxi2

   !     r*excentricite*cav = recav = p - r
   !     r*excentricite*sav = resav = pos_fois_vit*sqrt(p/mu) = pos_fois_vit*rxv/mu

   re2   = (r*excentricite)**2
   !     avec excentricite > 1 et r non nul
   dresav(:)  = (dpos_fois_vit(:)*rxv + pos_fois_vit*drxv(:))/mu
   davdx(:)   = (-resav*(dpdx(:)-drdx(:)) + recav*dresav(:))/re2

   dpodx(:)   = dpovdx(:) - davdx(:)

   ! calcul derivees anomalie moyenne
   ! --------------------------------
   !     derivees de termes intermediaires utilises dans le calcul de xm
   scal2a = 0.5_pm_reel*pos_fois_vit*un_sur_a
   demimu = 0.5_pm_reel*mu
   scasbu = pos_fois_vit*unsbux
   daux(:) = rusamu*(dpos_fois_vit(:) - scal2a*dadx(:))
   dbux(:) = (demimu*dadx(:)+ rxv*drxv(:)) * unsbux
   dcux(:) = (dpos_fois_vit(:) - scasbu*dbux(:)) * unsbux

   dxmdx(:)= daux(:) - dcux(:)/dux

   ! affectation de la jacobienne de la transformation
   ! -------------------------------------------------

   jacob(1,:)=dadx(:)
   jacob(2,:)=dedx(:)
   jacob(3,:)=dxidx(:)
   jacob(4,:)=dpodx(:)
   jacob(5,:)=dgodx(:)
   jacob(6,:)=dxmdx(:)

end if

6000 continue

end subroutine mvi_car_kep_hyperb
